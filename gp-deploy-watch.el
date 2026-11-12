;;; gp-deploy-watch.el --- Arm a manual pipeline step to fire when it is reached -*- lexical-binding: t; -*-

;;; Commentary:

;; "Deploy to dev when the build gets there."
;;
;; A manual pipeline step cannot be started before the steps ahead of it
;; have finished -- firing early either errors or, worse, deploys a build
;; whose earlier stages later fail.  So the usual way to run one is to sit
;; and watch the pipeline until the gate opens, then press `T'.  This
;; module does the sitting: `A' on a manual step ARMS it, and a background
;; watcher fires it the moment it is genuinely runnable.
;;
;; The watcher is deliberately a *client-side* wait: it only presses the
;; button, at the moment a human would, and never models the dependency
;; graph -- it acts only once the backend reports the step waiting (see
;; `gp-pipeline-step-runnable-manual-p').
;;
;; Watchers live in a global registry rather than in the PR's detail
;; buffer, so closing the buffer does not cancel a deploy you are waiting
;; on.  Each keeps an in-memory event log (`gp-deploy-watch-show-log') and
;; can be cancelled (`gp-deploy-watch-cancel-watcher').  Nothing is
;; persisted across Emacs sessions.
;;
;; Keys, on a manual step in the detail view:
;;
;;   A     arm / disarm a watcher for the step at point
;;   C-c A list every armed watcher (RET a row for its log, `k' cancels)
;;
;; The state a watcher moves through:
;;
;;   waiting  -- polling; the step is not runnable yet
;;   firing   -- the gate opened; the trigger is running
;;   done     -- fired successfully
;;   failed   -- the trigger errored, or the run finished without ever
;;               opening the gate
;;   cancelled -- cancelled by hand

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'git-platform)
(require 'gp-log)
(require 'gp-pipeline)

(declare-function gp-detail-refresh "gp-ui")
(declare-function magit-current-section "magit-section")
(declare-function magit-insert-heading "magit-section")
(declare-function magit-insert-section "magit-section")
(defvar gp--pr)

;;;; Options -------------------------------------------------------------------

(defcustom gp-deploy-watch-interval 15
  "Seconds between polls while a deploy watcher waits for its step.
Deliberately slower than the detail buffer's poll: a watcher may run
unattended for a long time."
  :type 'integer :group 'bitbucket)

(defcustom gp-deploy-watch-timeout 7200
  "Seconds a watcher waits before giving up, or nil for no limit.
A build that never reaches the gate would otherwise leave a timer
polling until Emacs exits."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'bitbucket)

(defcustom gp-deploy-watch-log-max 200
  "How many events one watcher keeps in memory; older ones are dropped."
  :type 'integer :group 'bitbucket)

(defcustom gp-deploy-watch-confirm t
  "When non-nil, arming a watcher asks first.
Arming schedules a deployment that fires without a further keypress."
  :type 'boolean :group 'bitbucket)

;;;; Faces ---------------------------------------------------------------------

(defface gp-deploy-watch-armed-face '((t :inherit gp-pipeline-paused-face))
  "Face for the armed marker on a step line." :group 'bitbucket-faces)
(defface gp-deploy-watch-failed-face '((t :inherit error))
  "Face for a failed watcher." :group 'bitbucket-faces)
(defface gp-deploy-watch-done-face '((t :inherit success))
  "Face for a watcher that fired." :group 'bitbucket-faces)

;;;; The watcher object ---------------------------------------------------------

(cl-defstruct (gp-deploy-watch (:constructor gp-deploy-watch--make)
                               (:copier nil))
  "One armed manual step, waiting for its gate to open."
  key            ; string, identity in the registry
  full-name      ; "acme/web"
  branch         ; source branch the pipeline runs on
  commit         ; head commit the run must belong to, or nil for any
  step-name      ; the manual step's name -- matched across refetches
  pipeline-id    ; uuid of the run this watcher is currently following, or
                 ; nil before its first sighting -- see `gp-deploy-watch--steps-of'
  pr             ; the PR alist, for the deploy script's environment
  state          ; waiting | firing | done | failed | cancelled
  detail         ; short human string about the current state
  timer          ; the poll timer, or nil
  started        ; float-time when armed
  fired-gates    ; names of earlier gates already pressed, so none is pressed twice
  log)           ; newest-first list of (TIMESTAMP . STRING)

(defvar gp-deploy-watch--registry (make-hash-table :test 'equal)
  "KEY -> `gp-deploy-watch', every watcher currently armed or finished.
Finished watchers stay until dismissed so their log can still be
read; `gp-deploy-watch-clear-finished' reaps them.")

(defun gp-deploy-watch--key (full-name branch step-name)
  "Return the registry key for STEP-NAME on BRANCH of FULL-NAME.
Keyed by step name rather than id, since ids change when a new run starts."
  (format "%s@%s#%s" (or full-name "?") (or branch "?") (or step-name "?")))

(defun gp-deploy-watch-get (full-name branch step-name)
  "Return the watcher for STEP-NAME on BRANCH of FULL-NAME, or nil."
  (gethash (gp-deploy-watch--key full-name branch step-name)
           gp-deploy-watch--registry))

(defun gp-deploy-watch-list ()
  "Return every watcher, newest first."
  (let (all)
    (maphash (lambda (_k w) (push w all)) gp-deploy-watch--registry)
    (sort all (lambda (a b) (> (gp-deploy-watch-started a)
                               (gp-deploy-watch-started b))))))

(defun gp-deploy-watch-schedulable-p (step)
  "Return non-nil when STEP is one a watcher could actually fire.
Only a triggerable step qualifies -- today a manual gate
\(`gp-pipeline-step-manual-p').  Asked through the backend rather than
branching on the forge, so a backend gaining triggerable steps needs
no change here."
  (and step (gp-pipeline-step-manual-p step)))

(defun gp-deploy-watch-backend-supports-p ()
  "Return non-nil when this backend has triggerable steps at all.
Distinguishes \"this step is not a gate\" from \"this forge has no
gates\", so arming can say which."
  (ignore-errors
    (gp-deploy-watch-schedulable-p
     ;; a synthetic gated step, asked of the live backend
     '((name . "probe") (trigger (type . "pipeline_step_trigger_manual"))))))

(defun gp-deploy-watch-active-p (w)
  "Return non-nil when watcher W is still working."
  (memq (gp-deploy-watch-state w) '(waiting firing)))

;;;; Event log ------------------------------------------------------------------

(defun gp-deploy-watch--log (w fmt &rest args)
  "Record an event on watcher W, built from FMT and ARGS.
Kept on the watcher itself and mirrored into the diagnostic log.
Returns the formatted line."
  (let ((line (apply #'format fmt args)))
    (push (cons (current-time) line) (gp-deploy-watch-log w))
    (let ((log (gp-deploy-watch-log w)))
      (when (> (length log) gp-deploy-watch-log-max)
        (setf (gp-deploy-watch-log w) (seq-take log gp-deploy-watch-log-max))))
    (gp-log 'info "deploy-watch [%s]: %s" (gp-deploy-watch-key w) line)
    (gp-deploy-watch--refresh-buffers w)
    line))

(defun gp-deploy-watch--set-state (w state fmt &rest args)
  "Move watcher W to STATE, logging why (FMT and ARGS).
Reaching `done' or `failed' also notifies, since a watcher runs
unattended; `cancelled' does not, as the user just did that."
  (setf (gp-deploy-watch-state w) state)
  (setf (gp-deploy-watch-detail w) (apply #'format fmt args))
  (apply #'gp-deploy-watch--log w fmt args)
  (when (memq state '(done failed))
    (gp-deploy-watch--notify w)))

;;;; Polling --------------------------------------------------------------------

(defun gp-deploy-watch--cancel-timer (w)
  "Cancel watcher W's poll timer, if any."
  (when (timerp (gp-deploy-watch-timer w))
    (cancel-timer (gp-deploy-watch-timer w)))
  (setf (gp-deploy-watch-timer w) nil))

(defun gp-deploy-watch--rearm (w)
  "Schedule watcher W's next poll."
  (gp-deploy-watch--cancel-timer w)
  (when (gp-deploy-watch-active-p w)
    (setf (gp-deploy-watch-timer w)
          (run-with-timer gp-deploy-watch-interval nil
                          #'gp-deploy-watch--tick w))))

(defun gp-deploy-watch--timed-out-p (w)
  "Return non-nil when watcher W has waited past `gp-deploy-watch-timeout'."
  (and gp-deploy-watch-timeout
       (> (- (float-time) (gp-deploy-watch-started w))
          gp-deploy-watch-timeout)))

(defun gp-deploy-watch--steps-of (w data)
  "Return (PIPELINE . STEPS) for the run watcher W is following in DATA.

Once W has followed a run (`gp-deploy-watch-pipeline-id' is set), only
that exact run counts, so an unrelated later run on the same branch and
commit is never fired against.  Before the first sighting, any run with
a name-matching step is adopted and its id recorded;
`gp-deploy-watch--fire-blocking' clears it when pressing an earlier gate
starts a new run."
  (catch 'hit
    (pcase-dolist (`(,pipeline . ,steps) (plist-get data :current))
      (when (and (or (null (gp-deploy-watch-pipeline-id w))
                     (equal (gp-pipeline-id pipeline)
                            (gp-deploy-watch-pipeline-id w)))
                 (cl-find (gp-deploy-watch-step-name w) steps
                          :key (lambda (s) (alist-get 'name s)) :test #'equal))
        (unless (gp-deploy-watch-pipeline-id w)
          (setf (gp-deploy-watch-pipeline-id w) (gp-pipeline-id pipeline)))
        (throw 'hit (cons pipeline steps))))
    nil))

(defun gp-deploy-watch--steps-before (w steps)
  "Return the STEPS that come before watcher W's target, in run order.
Relies on the backend returning steps in pipeline order."
  (let ((seen nil) (before nil))
    (dolist (s steps (nreverse before))
      (cond (seen)
            ((equal (alist-get 'name s) (gp-deploy-watch-step-name w))
             (setq seen t))
            (t (push s before))))))

(defun gp-deploy-watch--step-failed-p (step)
  "Return non-nil when STEP failed or was stopped."
  (member (gp-pipeline-step-result step)
          '("FAILED" "ERROR" "STOPPED" "HALTED")))

(defun gp-deploy-watch--blocking-gate (w steps)
  "Return the first open gate ahead of watcher W's target in STEPS, or nil.
E.g. a dev gate in front of a live target.  Only triggerable steps
qualify (`gp-deploy-watch-schedulable-p')."
  (cl-find-if (lambda (s)
                (and (gp-deploy-watch-schedulable-p s)
                     (gp-pipeline-step-runnable-manual-p s)))
              (gp-deploy-watch--steps-before w steps)))

(defun gp-deploy-watch--failed-step-before (w steps)
  "Return the first failed step ahead of watcher W's target in STEPS, or nil."
  (cl-find-if #'gp-deploy-watch--step-failed-p
              (gp-deploy-watch--steps-before w steps)))

(defun gp-deploy-watch--tick (w)
  "Poll once for watcher W: fire the step if it is now runnable."
  (cond
   ((not (gp-deploy-watch-active-p w)) (gp-deploy-watch--cancel-timer w))
   ((gp-deploy-watch--timed-out-p w)
    (gp-deploy-watch--cancel-timer w)
    (gp-deploy-watch--set-state
     w 'failed "gave up after %ds without the gate opening"
     (round (- (float-time) (gp-deploy-watch-started w)))))
   (t
    (condition-case e
        (gp-pipeline-fetch-for-branch-async
         (gp-deploy-watch-full-name w)
         (gp-deploy-watch-branch w)
         (gp-deploy-watch-commit w)
         (lambda (data) (gp-deploy-watch--consider w data)))
      (error
       (gp-deploy-watch--log w "poll failed: %s" (error-message-string e))
       (gp-deploy-watch--rearm w))))))

(defun gp-deploy-watch--consider (w data)
  "Decide what watcher W should do given freshly fetched DATA.
The clause order is the behaviour.  Each poll asks, in order: has
anything ahead of the target failed (give up), is the target itself
open with every earlier step completed (fire it), is an earlier gate
open (press it and keep waiting), has the build finished without
opening it (give up).  Otherwise keep waiting."
  (when (gp-deploy-watch-active-p w)
    (let* ((run (and data (gp-deploy-watch--steps-of w data)))
           (pipeline (car run))
           (steps (cdr run))
           (step (cl-find (gp-deploy-watch-step-name w) steps
                          :key (lambda (s) (alist-get 'name s)) :test #'equal))
           ;; An untouched manual step and one not reached yet both report
           ;; NOT_RUN, so only an earlier step that has COMPLETED counts as
           ;; clear -- an unpressed earlier gate blocks as much as a running
           ;; step does.
           (nothing-blocking-before-target
            (and step
                 (cl-every (lambda (s) (equal (gp-pipeline-step-state s) "COMPLETED"))
                           (gp-deploy-watch--steps-before w steps))))
           (bad (gp-deploy-watch--failed-step-before w steps))
           (gate (gp-deploy-watch--blocking-gate w steps)))
      (cond
       ;; A failed fetch reports nil exactly as a pipeline-less branch does,
       ;; so it cannot be read as "the run vanished" -- keep waiting.
       ((null data)
        (gp-deploy-watch--log w "no pipeline data (fetch failed or no runs yet)")
        (gp-deploy-watch--rearm w))
       ((null step)
        (gp-deploy-watch--log w "step %S not in the current run yet"
                              (gp-deploy-watch-step-name w))
        (gp-deploy-watch--rearm w))
       ;; Something ahead of the target failed: the gate will never open.
       (bad
        (gp-deploy-watch--cancel-timer w)
        (gp-deploy-watch--set-state
         w 'failed "step %S %s -- %S is unreachable"
         (or (alist-get 'name bad) "?")
         (downcase (or (gp-pipeline-step-result bad) "failed"))
         (gp-deploy-watch-step-name w)))
       ;; Checked before the blocking-gate clause so a ready target is
       ;; never deferred.
       ((and nothing-blocking-before-target (gp-pipeline-step-runnable-manual-p step))
        (gp-deploy-watch--cancel-timer w)
        (gp-deploy-watch--set-state w 'firing "gate open on build %s; triggering"
                                    (or (gp-pipeline-number pipeline) "?"))
        (gp-deploy-watch--fire w pipeline step))
       ;; An earlier gate is in the way: press it, then keep waiting.
       (gate
        (gp-deploy-watch--fire-blocking w pipeline gate))
       ;; A later run would be a different build than the one armed.
       ((gp-pipeline-finished-p pipeline)
        (gp-deploy-watch--cancel-timer w)
        (gp-deploy-watch--set-state
         w 'failed "build %s finished (%s) without opening the gate"
         (or (gp-pipeline-number pipeline) "?")
         (or (gp-pipeline-result pipeline) (gp-pipeline-state pipeline) "?")))
       (t
        (gp-deploy-watch--log w "waiting -- step %S is %s"
                              (gp-deploy-watch-step-name w)
                              (or (gp-pipeline-step-state step) "?"))
        (gp-deploy-watch--rearm w))))))

(defun gp-deploy-watch--fire-blocking (w pipeline gate)
  "Press GATE of PIPELINE, an open gate standing between W and its target.
Does not finish the watcher.  Each gate is pressed once (recorded on
W), since a gate still reads as open for a moment after triggering.
Re-triggering starts a NEW run with its own id, so W's followed run is
cleared for `gp-deploy-watch--steps-of' to adopt the new one."
  (let ((name (or (alist-get 'name gate) "?")))
    (if (member name (gp-deploy-watch-fired-gates w))
        (progn
          (gp-deploy-watch--log w "waiting -- already pressed %S" name)
          (gp-deploy-watch--rearm w))
      (push name (gp-deploy-watch-fired-gates w))
      (gp-deploy-watch--log w "earlier gate %S is open; pressing it first" name)
      (condition-case e
          (progn
            (if gp-pipeline-deploy-script
                (gp-pipeline--deploy-run (gp-deploy-watch-full-name w)
                                         (gp-deploy-watch-branch w)
                                         pipeline gate (gp-deploy-watch-pr w))
              (gp-pipeline-run-manual-step (gp-deploy-watch-full-name w)
                                           (gp-deploy-watch-branch w)
                                           pipeline gate))
            (setf (gp-deploy-watch-pipeline-id w) nil)
            (gp-deploy-watch--log w "pressed %S; still waiting for %S"
                                  name (gp-deploy-watch-step-name w))
            (gp-deploy-watch--rearm w))
        (error
         (gp-deploy-watch--cancel-timer w)
         (gp-deploy-watch--set-state
          w 'failed "could not press the earlier gate %S: %s"
          name (error-message-string e)))))))

;;;; Firing ---------------------------------------------------------------------

(defun gp-deploy-watch--fire (w pipeline step)
  "Run watcher W's STEP of PIPELINE, by whichever route is available.

1. `gp-pipeline-deploy-script', when configured: advances THIS build's
   gate in place.  Asynchronous, so W stays `firing' until the script's
   exit status arrives via `gp-pipeline--deploy-run's ON-EXIT.
2. Otherwise `gp-pipeline-run-manual-step', which on Bitbucket
   re-triggers the pipeline (BCLOUD-20050) and so re-runs everything
   before the gate -- the watcher's state says so."
  (let ((full-name (gp-deploy-watch-full-name w))
        (branch (gp-deploy-watch-branch w)))
    (condition-case e
        (if gp-pipeline-deploy-script
            (progn
              (gp-deploy-watch--log w "running `gp-pipeline-deploy-script'")
              (gp-pipeline--deploy-run
               full-name branch pipeline step (gp-deploy-watch-pr w)
               (lambda (ok)
                 (if ok
                     (gp-deploy-watch--set-state
                      w 'done "deploy triggered for %S; Bitbucket is running it"
                      (gp-deploy-watch-step-name w))
                   (gp-deploy-watch--set-state
                    w 'failed "deploy script failed for %S; see %s"
                    (gp-deploy-watch-step-name w) gp-pipeline-deploy-buffer)))))
          (gp-deploy-watch--log
           w "no deploy script; using the backend's manual-step API")
          (gp-pipeline-run-manual-step full-name branch pipeline step)
          (gp-deploy-watch--set-state
           w 'done "triggered via the API (re-runs the steps before the gate)"))
      (error
       (gp-deploy-watch--set-state w 'failed "trigger failed: %s"
                                   (error-message-string e))))))

(defun gp-deploy-watch--notify (w)
  "Report watcher W's outcome, and refresh any detail view showing it.
Raises an OS notification as well as an echo-area message."
  (let ((failed (eq (gp-deploy-watch-state w) 'failed)))
    ;; The emoji carries the status (notifiers offer no colour control).
    ;; "Triggered", not "Deployed": nothing here observes the deploy itself.
    (gp-notify (format "%s %s %s"
                       (if failed "🔴" "🟢")
                       (if failed "Deploy blocked:" "Deploy triggered:")
                       (gp-deploy-watch-step-name w))
               (format "%s\n%s on %s"
                       (gp-deploy-watch-detail w)
                       (gp-deploy-watch-full-name w)
                       (gp-deploy-watch-branch w))
               failed))
  (message "gp: deploy watcher %s -- %s"
           (gp-deploy-watch-step-name w) (gp-deploy-watch-detail w))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'gp-detail-mode)
                 (boundp 'gp--pr) gp--pr
                 (equal (ignore-errors (gp-pr-full-name gp--pr))
                        (gp-deploy-watch-full-name w))
                 (equal (ignore-errors (gp-pr-source-branch gp--pr))
                        (gp-deploy-watch-branch w))
                 (fboundp 'gp-detail-refresh))
        (ignore-errors (gp-detail-refresh))))))

;;;; Arming / cancelling ---------------------------------------------------------

(defun gp-deploy-watch-arm (full-name branch commit step-name pr)
  "Arm a watcher for STEP-NAME on BRANCH of FULL-NAME and return it.
COMMIT scopes which runs count as current; PR carries the context the
deploy script is given.  Re-arming an already-active step is refused,
as two watchers on one gate would both fire."
  (let* ((key (gp-deploy-watch--key full-name branch step-name))
         (existing (gethash key gp-deploy-watch--registry)))
    (when (and existing (gp-deploy-watch-active-p existing))
      (user-error "Already watching %S (%s)"
                  step-name (gp-deploy-watch-state existing)))
    (let ((w (gp-deploy-watch--make
              :key key :full-name full-name :branch branch :commit commit
              :step-name step-name :pr pr :state 'waiting
              :detail "armed" :started (float-time) :log nil)))
      (puthash key w gp-deploy-watch--registry)
      (gp-deploy-watch--log w "armed -- waiting for %S to become runnable"
                            step-name)
      ;; Poll once straight away: the gate may already be open.
      (gp-deploy-watch--tick w)
      w)))

(defun gp-deploy-watch-cancel-watcher (w)
  "Cancel watcher W."
  (gp-deploy-watch--cancel-timer w)
  (if (gp-deploy-watch-active-p w)
      (gp-deploy-watch--set-state w 'cancelled "cancelled")
    (gp-deploy-watch--log w "already %s; nothing to cancel"
                          (gp-deploy-watch-state w)))
  w)

(defun gp-deploy-watch-clear-finished ()
  "Forget every watcher that is no longer active, dropping its log."
  (interactive)
  (let ((n 0))
    (maphash (lambda (k w)
               (unless (gp-deploy-watch-active-p w)
                 (remhash k gp-deploy-watch--registry)
                 (setq n (1+ n))))
             gp-deploy-watch--registry)
    (gp-deploy-watch--refresh-list-buffer)
    (message "gp: cleared %d finished watcher%s" n (if (= n 1) "" "s"))))

;;;; Presentation ----------------------------------------------------------------

(defun gp-deploy-watch--state-face (state)
  "Return the face for a watcher in STATE."
  (pcase state
    ('done 'gp-deploy-watch-done-face)
    ((or 'failed 'cancelled) 'gp-deploy-watch-failed-face)
    (_ 'gp-deploy-watch-armed-face)))

(defun gp-deploy-watch--state-glyph (state)
  "Return the status glyph for a watcher in STATE.
Matches the glyphs of the OS notification."
  (pcase state
    ('waiting "⏳")
    ('firing "🚀")
    ('done "🟢")
    ('failed "🔴")
    ('cancelled "⚪")
    (_ "•")))

(defun gp-deploy-watch-step-marker (step full-name branch)
  "Return a propertized marker for STEP's arm-on-reach state, or nil.
STEP is matched by name against the watchers on BRANCH of FULL-NAME.
A step with a watcher shows its state; a schedulable step without one
gets a plain `[▸ A]' hint, like the other pipeline key hints."
  (when-let* ((name (alist-get 'name step)))
    (if-let* ((w (gp-deploy-watch-get full-name branch name)))
        (propertize (format "  [%s %s ▸ A]"
                            (gp-deploy-watch--state-glyph (gp-deploy-watch-state w))
                            (gp-deploy-watch-state w))
                    'face (gp-deploy-watch--state-face (gp-deploy-watch-state w))
                    'help-echo (gp-deploy-watch-detail w))
      (when (gp-deploy-watch-schedulable-p step)
        (propertize "  [▸ A]" 'face 'shadow
                    'help-echo "Arm this gate to run automatically once the build reaches it")))))

(defun gp-deploy-watch--elapsed (w)
  "Return W's age as a short human string."
  (gp-pipeline--format-secs
   (max 0 (round (- (float-time) (gp-deploy-watch-started w))))))

;;;; Log buffer -------------------------------------------------------------------

(defvar-local gp-deploy-watch--buffer-watcher nil
  "The watcher a log buffer is showing.")

(defun gp-deploy-watch--log-buffer-name (w)
  "Return the log buffer name for watcher W."
  (gp--buffer-name (format "watch %s" (gp-deploy-watch-key w))))

(defvar-keymap gp-deploy-watch-log-mode-map
  "g" #'gp-deploy-watch-log-refresh
  "k" #'gp-deploy-watch-log-cancel
  "q" #'quit-window)

(define-derived-mode gp-deploy-watch-log-mode special-mode "gp-watch"
  "Major mode for one deploy watcher's in-memory log.")

(defun gp-deploy-watch--render-log (w)
  "Render watcher W's log into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (format "%s\n" (gp-deploy-watch-step-name w))
                        'face 'bold))
    (insert (format "%s on %s\n"
                    (gp-deploy-watch-full-name w) (gp-deploy-watch-branch w)))
    (insert (propertize (format "%s %s -- %s"
                                (gp-deploy-watch--state-glyph
                                 (gp-deploy-watch-state w))
                                (gp-deploy-watch-state w)
                                (gp-deploy-watch-detail w))
                        'face (gp-deploy-watch--state-face
                               (gp-deploy-watch-state w))))
    (insert (propertize (format "   (armed %s ago)\n\n"
                                (gp-deploy-watch--elapsed w))
                        'face 'shadow))
    (if (null (gp-deploy-watch-log w))
        (insert "  (no events yet)\n")
      ;; oldest first: a log reads forwards
      (dolist (entry (reverse (gp-deploy-watch-log w)))
        (insert (propertize (format-time-string "  %H:%M:%S " (car entry))
                            'face 'shadow)
                (cdr entry) "\n")))
    (goto-char (point-max))))

(defun gp-deploy-watch-show-log (w)
  "Pop to watcher W's in-memory log buffer."
  (interactive (list (gp-deploy-watch--read "Show log of watcher: ")))
  (let ((buf (get-buffer-create (gp-deploy-watch--log-buffer-name w))))
    (with-current-buffer buf
      (gp-deploy-watch-log-mode)
      (setq gp-deploy-watch--buffer-watcher w)
      (gp-deploy-watch--render-log w))
    (pop-to-buffer buf)))

(defun gp-deploy-watch-log-refresh ()
  "Redraw the current watcher log buffer."
  (interactive)
  (if-let* ((w gp-deploy-watch--buffer-watcher))
      (gp-deploy-watch--render-log w)
    (user-error "Not a watcher log buffer")))

(defun gp-deploy-watch-log-cancel ()
  "Cancel the watcher this log buffer shows."
  (interactive)
  (if-let* ((w gp-deploy-watch--buffer-watcher))
      (progn (gp-deploy-watch-cancel-watcher w)
             (gp-deploy-watch--render-log w))
    (user-error "Not a watcher log buffer")))

(defun gp-deploy-watch--refresh-buffers (w)
  "Redraw W's log buffer, the watcher list and the mode-line segment."
  (ignore-errors
    (when-let* ((buf (get-buffer (gp-deploy-watch--log-buffer-name w))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (gp-deploy-watch--render-log w))))
    (gp-deploy-watch--refresh-list-buffer)
    (force-mode-line-update t)))

;;;; The watcher list --------------------------------------------------------------

(defconst gp-deploy-watch-list-buffer-name (gp--buffer-name "watchers")
  "Name of the buffer listing every armed watcher.")

(defvar-keymap gp-deploy-watch-list-mode-map
  "g"   #'gp-deploy-watch-list-refresh
  "k"   #'gp-deploy-watch-list-cancel
  "RET" #'gp-deploy-watch-list-show-log
  "C"   #'gp-deploy-watch-clear-finished
  "q"   #'quit-window)

(define-derived-mode gp-deploy-watch-list-mode special-mode "gp-watchers"
  "Major mode listing armed deploy watchers.")

(defun gp-deploy-watch--render-list ()
  "Render every watcher into the current buffer."
  (let ((inhibit-read-only t)
        (watchers (gp-deploy-watch-list)))
    (erase-buffer)
    (insert (propertize "Deploy watchers" 'face 'bold)
            (propertize "   RET log · k cancel · C clear finished · g refresh\n\n"
                        'face 'shadow))
    (if (null watchers)
        (insert "  (none armed)\n")
      (dolist (w watchers)
        ;; `symbol-name' first: %-10s does not pad a symbol.
        (insert (propertize (format "  %s %-10s"
                                    (gp-deploy-watch--state-glyph
                                     (gp-deploy-watch-state w))
                                    (symbol-name (gp-deploy-watch-state w)))
                            'face (gp-deploy-watch--state-face
                                   (gp-deploy-watch-state w)))
                (propertize (format "%-24s" (gp-deploy-watch-step-name w))
                            'face 'default)
                (propertize (format " %s @ %s"
                                    (gp-deploy-watch-full-name w)
                                    (gp-deploy-watch-branch w))
                            'face 'shadow)
                (propertize (format "   %s\n" (gp-deploy-watch-detail w))
                            'face 'shadow))
        ;; the watcher itself rides the line, so the row commands need no lookup
        (put-text-property (line-beginning-position 0) (point)
                           'gp-deploy-watch w)))
    (goto-char (point-min))))

;;;###autoload
(defun gp-deploy-watch-list-show ()
  "Pop to the list of armed deploy watchers."
  (interactive)
  (let ((buf (get-buffer-create gp-deploy-watch-list-buffer-name)))
    (with-current-buffer buf
      (gp-deploy-watch-list-mode)
      (gp-deploy-watch--render-list))
    (pop-to-buffer buf)))

(defun gp-deploy-watch--refresh-list-buffer ()
  "Redraw the watcher list buffer if it exists."
  (when-let* ((buf (get-buffer gp-deploy-watch-list-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf (gp-deploy-watch--render-list)))))

(defun gp-deploy-watch-list-refresh ()
  "Redraw the watcher list."
  (interactive)
  (gp-deploy-watch--render-list))

(defun gp-deploy-watch--at-point ()
  "Return the watcher on the current list line, or nil."
  (get-text-property (point) 'gp-deploy-watch))

(defun gp-deploy-watch-list-show-log ()
  "Show the log of the watcher at point."
  (interactive)
  (if-let* ((w (gp-deploy-watch--at-point)))
      (gp-deploy-watch-show-log w)
    (user-error "No watcher on this line")))

(defun gp-deploy-watch-list-cancel ()
  "Cancel the watcher at point."
  (interactive)
  (if-let* ((w (gp-deploy-watch--at-point)))
      (progn (gp-deploy-watch-cancel-watcher w)
             (gp-deploy-watch--render-list))
    (user-error "No watcher on this line")))

(defun gp-deploy-watch--read (prompt)
  "Read a watcher with PROMPT, completing on the armed ones."
  (let ((watchers (gp-deploy-watch-list)))
    (unless watchers (user-error "No deploy watchers"))
    (let* ((rows (mapcar (lambda (w)
                           (cons (format "%s [%s] %s"
                                         (gp-deploy-watch-step-name w)
                                         (gp-deploy-watch-state w)
                                         (gp-deploy-watch-full-name w))
                                 w))
                         watchers))
           (pick (completing-read prompt rows nil t)))
      (cdr (assoc pick rows)))))

;;;; Detail-view commands -----------------------------------------------------------

(defun gp-deploy-watch-toggle-at-point ()
  "Arm a watcher for the manual step at point, or cancel the armed one.
The step need not be runnable yet, but must be triggerable
\(`gp-deploy-watch-schedulable-p').  Earlier gates in the way are
approved automatically.  For a merged PR the watcher follows the
destination branch and merge commit -- the run that deploys."
  (interactive)
  (unless (boundp 'gp--pr) (user-error "Not in a pull-request buffer"))
  (let* ((step (gp-pipeline--step-at-point))
         (name (or (alist-get 'name step)
                   (user-error "No pipeline step at point")))
         (pr gp--pr)
         (merged (gp-pr-merged-p pr))
         (full-name (gp-pr-full-name pr))
         (branch (if merged (gp-pr-destination-branch pr) (gp-pr-source-branch pr)))
         (commit (if merged (gp-pr-merge-commit pr) (gp-pr-source-commit pr)))
         (existing (gp-deploy-watch-get full-name branch name)))
    (cond
     ((and existing (gp-deploy-watch-active-p existing))
      (gp-deploy-watch-cancel-watcher existing)
      (message "gp: cancelled the watcher on %S" name)
      (when (fboundp 'gp-detail-refresh) (gp-detail-refresh)))
     ;; Say whether the step or the whole backend lacks gates.
     ((not (gp-deploy-watch-schedulable-p step))
      (user-error
       "Step %S cannot be scheduled: %s" name
       (if (gp-deploy-watch-backend-supports-p)
           "it runs automatically, so there is nothing to trigger"
         "this backend has no triggerable steps")))
     (t
      (when (or (not gp-deploy-watch-confirm)
                (yes-or-no-p
                 (format "Run %S as soon as the build reaches it (approving any gates in the way)? "
                         name)))
        (gp-deploy-watch-arm full-name branch commit name pr)
        (message "gp: watching %S%s; earlier gates will be approved on the way"
                 name (if merged (format " on %s (merged)" branch) ""))
        (when (fboundp 'gp-detail-refresh) (gp-detail-refresh)))))))

;;;; Global mode-line segment -----------------------------------------------------

(defun gp-deploy-watch--active-watchers ()
  "Return every active watcher, newest first."
  (seq-filter #'gp-deploy-watch-active-p (gp-deploy-watch-list)))

(defvar gp-deploy-watch--modeline-map
  (let ((m (make-sparse-keymap)))
    (define-key m [mode-line mouse-1] #'gp-deploy-watch-list-show)
    (define-key m [mode-line mouse-3] #'gp-deploy-watch-mode-line-cancel)
    m)
  "Keymap for the mode-line deploy-watch segment.")

(defun gp-deploy-watch--mode-line ()
  "Return the global mode-line segment for active deploy watchers, or \"\"."
  (let ((active (gp-deploy-watch--active-watchers)))
    (if (null active) ""
      (propertize
       (format " %s%s"
               (gp-deploy-watch--state-glyph (gp-deploy-watch-state (car active)))
               (if (cdr active)
                   (format " %d deploys" (length active))
                 (format " %s" (gp-deploy-watch-step-name (car active)))))
       'face 'gp-deploy-watch-armed-face
       'help-echo "Deploy watcher(s) — click to list, right-click to cancel"
       'mouse-face 'mode-line-highlight
       'local-map gp-deploy-watch--modeline-map))))

(defun gp-deploy-watch-mode-line-cancel ()
  "Cancel the sole active watcher, or open the list if there are several."
  (interactive)
  (let ((active (gp-deploy-watch--active-watchers)))
    (cond
     ((null active) (user-error "No active deploy watchers"))
     ((null (cdr active))
      (gp-deploy-watch-cancel-watcher (car active))
      (message "gp: cancelled the watcher on %S"
               (gp-deploy-watch-step-name (car active))))
     (t (gp-deploy-watch-list-show)))))

(unless (member '(:eval (gp-deploy-watch--mode-line)) mode-line-misc-info)
  (setq mode-line-misc-info
        (append mode-line-misc-info '((:eval (gp-deploy-watch--mode-line))))))

(provide 'gp-deploy-watch)
;;; gp-deploy-watch.el ends here
