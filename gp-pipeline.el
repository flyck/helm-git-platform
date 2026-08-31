;;; gp-pipeline.el --- CI pipelines in the PR detail view -*- lexical-binding: t; -*-

;;; Commentary:

;; Renders a PR's CI pipelines as a magit-section in the detail buffer and
;; provides the per-pipeline actions.  Everything here goes through the
;; backend-free `gp-' protocol (see git-platform.el), so it knows nothing
;; about any particular forge.
;;
;; Layout: one collapsible section per pipeline, sorted with the pipeline
;; that has the most steps on top (ties: newest first).  A finished
;; pipeline's steps start collapsed; a running one stays expanded.  Each
;; step shows its status and duration.
;;
;; Actions on the pipeline/step at point:
;;   s   stop the running pipeline       (pipeline-level; the platform has
;;   T   trigger / re-run the pipeline    no per-step stop or trigger)
;;   m   run a waiting *manual* step
;;   l   open the step's log in a buffer (polled live while it runs,
;;       historical once finished)
;;
;; These are wired into `gp-detail-mode-map' from gp-ui.el.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ansi-color)
(require 'magit-section)
(require 'git-platform)
(require 'gp-local)
(require 'gp-log)

(declare-function gp-pr-full-name "git-platform")
(declare-function gp-pr-source-branch "git-platform")
;; `gp-defop' defines these at load time, so the compiler cannot see them
(declare-function gp-pipelines-for-branch-async "git-platform")
(declare-function gp-pipeline-steps-async "git-platform")
(declare-function gp-commit-message-async "git-platform")
(declare-function gp-pipeline-step-log-classify-line "git-platform")

(defvar gp--pr)                         ;; the detail buffer's PR (gp-ui.el)
(declare-function gp-detail-refresh "gp-ui")
(declare-function gp-deploy-watch-step-marker "gp-deploy-watch")
(declare-function gp-deploy-watch-toggle-at-point "gp-deploy-watch")
(declare-function gp-helm--deploy-cache-bust-repo "gp-helm")

;;;; Faces ---------------------------------------------------------------------

(defface gp-pipeline-running-face '((t :inherit warning))
  "Face for a running pipeline/step." :group 'bitbucket-faces)
(defface gp-pipeline-spinner-face '((t :inherit font-lock-keyword-face))
  "Face for the animated in-progress spinner glyph.
Inherits a theme-provided face rather than naming a colour, so the
spinner picks up whatever blue/accent hue the active theme uses."
  :group 'bitbucket-faces)
(defface gp-pipeline-success-face '((t :inherit success))
  "Face for a successful pipeline/step." :group 'bitbucket-faces)
(defface gp-pipeline-failed-face '((t :inherit error))
  "Face for a failed pipeline/step." :group 'bitbucket-faces)
(defface gp-pipeline-stopped-face '((t :inherit shadow))
  "Face for a stopped/halted pipeline/step." :group 'bitbucket-faces)
(defface gp-pipeline-paused-face '((t :inherit warning :weight bold))
  "Face for a pipeline paused at an open manual gate." :group 'bitbucket-faces)
(defface gp-pipeline-log-command-face '((t :inherit font-lock-comment-face :weight bold))
  "Face for a step-log line that echoes the command being run
\(e.g. Bitbucket's leading \"+ \"\), so it stands out from the
command's own output." :group 'bitbucket-faces)
(defface gp-pipeline-log-group-face '((t :inherit magit-section-heading))
  "Face for a step-log line that marks a named section
\(e.g. GitHub's \"##[group]\"/\"##[endgroup]\")." :group 'bitbucket-faces)

(defcustom gp-pipeline-max 20
  "How many recent branch pipelines to fetch before partitioning by commit."
  :type 'integer :group 'bitbucket)

(defcustom gp-pipeline-recent-max 5
  "How many prior-commit pipelines to show in the recent-runs summary."
  :type 'integer :group 'bitbucket)

(defcustom gp-pipeline-log-poll-interval 3
  "Seconds between polls when tailing a running step's log."
  :type 'number :group 'bitbucket)

;;;; Section classes -----------------------------------------------------------

(defclass gp-pipeline-section (magit-section) ())
(defclass gp-pipeline-step-section (magit-section) ())

;;;; Spinner -------------------------------------------------------------------

;; An in-progress pipeline/step renders as a one-character animated spinner
;; instead of a static glyph.  The frames orbit a Braille dot around the cell:
;; a step is RUNNING, not loading toward a known finish, so the animation has
;; to read as indeterminate motion.  A bar that fills and empties (the earlier
;; default) implied measurable progress and, worse, appeared to reset to zero
;; on every cycle.  All frames are single-width characters from the same
;; Braille block, so the animation never reflows the line: the timer only
;; swaps the character in place, it never inserts or deletes.

(defcustom gp-pipeline-spinner-frames
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Frames of the in-progress spinner.
The default orbits a Braille dot, the conventional indeterminate
spinner: a running job has no measurable percentage, so the motion
must not suggest one.  Every frame must be a single character of the
same display width, or the animation will shift the rest of the line."
  :type '(vector string) :group 'bitbucket)

(defcustom gp-pipeline-spinner-interval 0.08
  "Seconds between in-progress spinner frames.
With the 10-frame default this is one rotation every 0.8s -- the
usual pace for an indeterminate spinner.  The previous 0.12s was
tuned for a 12-frame bar and reads sluggishly as a rotation."
  :type 'number :group 'bitbucket)

(defvar gp-pipeline--spinner-index 0
  "Current frame index into `gp-pipeline-spinner-frames'.")

(defvar gp-pipeline--spinner-timer nil
  "Repeating timer animating the in-progress spinners, or nil.")

(defun gp-pipeline--spinner-frame ()
  "Return the spinner's current frame string."
  (aref gp-pipeline-spinner-frames
        (mod gp-pipeline--spinner-index (length gp-pipeline-spinner-frames))))

(defun gp-pipeline--spinner-glyph ()
  "Return a propertized spinner character for an in-progress entry.
The `gp-pipeline-spinner' text property marks it for repainting by
`gp-pipeline--spinner-tick'."
  (propertize (gp-pipeline--spinner-frame)
              'face 'gp-pipeline-spinner-face
              'gp-pipeline-spinner t))

(defun gp-pipeline--spinner-repaint-buffer (frame)
  "Replace every spinner character in the current buffer with FRAME.
Rewrites the single marked character in place, so no text is inserted
or removed and point, marks and window scroll all stay put.  Returns
non-nil when the buffer contained at least one spinner."
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        (found nil)
        (pos (point-min)))
    (save-excursion
      (while (setq pos (text-property-not-all pos (point-max)
                                              'gp-pipeline-spinner nil))
        (let ((end (or (next-single-property-change
                        pos 'gp-pipeline-spinner nil (point-max))
                       (point-max))))
          (setq found t)
          (unless (string= (buffer-substring-no-properties pos end) frame)
            (let ((props (text-properties-at pos)))
              (goto-char pos)
              (delete-region pos end)
              (insert (apply #'propertize frame props))))
          (setq pos (1+ pos)))))
    (set-buffer-modified-p nil)
    found))

(defconst gp-pipeline--spinner-grace-ticks 25
  "Empty ticks tolerated before the spinner timer stops itself.

A detail buffer is erased and rebuilt wholesale on every rerender, so
a tick landing inside that window sees no spinner even though one is
about to reappear.  Stopping on the FIRST empty tick made the spinner
freeze: only `gp-pipeline--status-glyph' restarts the timer, and the
pipeline poll deliberately skips the rerender while fetched data is
unchanged -- exactly the steady state of a long-running pipeline.  So
nothing repainted the glyph until the next unrelated redraw.

At the default 0.08s interval this grace spans ~2s of genuine absence
before the timer retires, which idle buffers reach in well under a
second of wall-clock cost.")

(defvar gp-pipeline--spinner-misses 0
  "Consecutive `gp-pipeline--spinner-tick' runs that found no spinner.")

(defvar gp-pipeline--elapsed-last-second nil
  "Wall-clock second at which elapsed times were last reticked.
Throttles the elapsed repaint to once a second: the spinner ticks
about twelve times as often, and a seconds display cannot change
faster than the clock.")

(defun gp-pipeline--elapsed-repaint-buffer ()
  "Retick every running step's elapsed-time text in the current buffer.
The duration is derived from `started_on', so it only advances when
something redraws it -- and the pipeline poll skips the redraw while a
running step's JSON is unchanged, which is the whole time it runs.
Rewrites just the tagged text, so point, marks and folding stay put.
Returns non-nil when at least one duration was found."
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        (found nil)
        (pos (point-min)))
    (save-excursion
      (while (setq pos (text-property-not-all pos (point-max)
                                              'gp-pipeline-elapsed nil))
        (let* ((started (get-text-property pos 'gp-pipeline-elapsed))
               (end (or (next-single-property-change
                         pos 'gp-pipeline-elapsed nil (point-max))
                        (point-max)))
               (new (ignore-errors
                      (format "  %s"
                              (gp-pipeline--format-secs
                               (max 0 (floor (- (float-time)
                                                (float-time
                                                 (date-to-time started))))))))))
          (setq found t)
          (when (and new (not (string= (buffer-substring-no-properties pos end)
                                       new)))
            (let ((props (text-properties-at pos)))
              (goto-char pos)
              (delete-region pos end)
              (insert (apply #'propertize new props))
              (setq end (+ pos (length new)))))
          (setq pos (max end (1+ pos))))))
    (set-buffer-modified-p nil)
    found))

(defun gp-pipeline--spinner-tick ()
  "Advance the spinner and repaint it in every buffer showing one.
Stops the timer once no live buffer has contained a spinner for
`gp-pipeline--spinner-grace-ticks' consecutive ticks."
  (setq gp-pipeline--spinner-index (1+ gp-pipeline--spinner-index))
  (let* ((frame (gp-pipeline--spinner-frame))
         (now (floor (float-time)))
         (tick-elapsed (not (equal now gp-pipeline--elapsed-last-second)))
         (any nil))
    (when tick-elapsed (setq gp-pipeline--elapsed-last-second now))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (derived-mode-p 'magit-section-mode)
            ;; Retick elapsed times on the same clock as the spinner: a step
            ;; that is running is exactly a step whose duration must advance.
            ;; Only once a second though -- the seconds display cannot change
            ;; faster than that, and the spinner ticks ~12x more often.
            (when (and tick-elapsed (gp-pipeline--elapsed-repaint-buffer))
              (setq any t))
            (when (gp-pipeline--spinner-repaint-buffer frame)
              (setq any t))))))
    (if any
        (setq gp-pipeline--spinner-misses 0)
      (cl-incf gp-pipeline--spinner-misses)
      (when (>= gp-pipeline--spinner-misses gp-pipeline--spinner-grace-ticks)
        (gp-pipeline--spinner-stop)))))

(defun gp-pipeline--spinner-stop ()
  "Cancel the spinner timer."
  (when (timerp gp-pipeline--spinner-timer)
    (cancel-timer gp-pipeline--spinner-timer))
  (setq gp-pipeline--spinner-timer nil
        gp-pipeline--spinner-misses 0))

(defun gp-pipeline--spinner-ensure ()
  "Start the spinner timer unless it is already running.
Also clears the miss counter, so a buffer that draws a spinner again
resets the grace window rather than inheriting a nearly-expired one."
  (setq gp-pipeline--spinner-misses 0)
  (unless (timerp gp-pipeline--spinner-timer)
    (setq gp-pipeline--spinner-timer
          (run-with-timer gp-pipeline-spinner-interval
                          gp-pipeline-spinner-interval
                          #'gp-pipeline--spinner-tick))))

;;;; Status formatting (pure) --------------------------------------------------

(defun gp-pipeline--status-glyph (state result &optional step)
  "Return (GLYPH . FACE) for a pipeline/step STATE and RESULT string.
A pipeline paused at an open manual gate (stage PAUSED) gets its own
⏸ glyph instead of the running one.  An in-progress entry gets the
animated spinner glyph (see `gp-pipeline--spinner-glyph'), which
already carries its own face -- FACE is `gp-pipeline-spinner-face' in
that case.  STEP is accepted for backward compatibility and no longer
changes the glyph."
  (ignore step)
  (cond
   ((equal result "PAUSED") (cons "⏸" 'gp-pipeline-paused-face))
   ((equal state "IN_PROGRESS")
    (gp-pipeline--spinner-ensure)
    (cons (gp-pipeline--spinner-glyph) 'gp-pipeline-spinner-face))
   ((member state '("PENDING" "READY" "BUILDING" nil))
    (cons "…" 'gp-pipeline-running-face))
   ((equal result "SUCCESSFUL") (cons "✔" 'gp-pipeline-success-face))
   ((equal result "FAILED")     (cons "✘" 'gp-pipeline-failed-face))
   ((equal result "ERROR")      (cons "✘" 'gp-pipeline-failed-face))
   ((member result '("STOPPED" "HALTED" "SKIPPED"))
    (cons "■" 'gp-pipeline-stopped-face))
   (t (cons "•" 'shadow))))

(defun gp-pipeline--format-secs (secs)
  "Format SECS (a nonnegative integer) as a short human duration string."
  (cond
   ((< secs 60) (format "%ds" secs))
   ((< secs 3600) (format "%dm%02ds" (/ secs 60) (% secs 60)))
   (t (format "%dh%02dm" (/ secs 3600) (/ (% secs 3600) 60)))))

(defun gp-pipeline--format-duration (step)
  "Return a short human duration string for STEP, or \"\".
Bitbucket only fills `duration_in_seconds' once a step completes, so
for a step still running the elapsed time is computed from its
`started_on' timestamp instead."
  (let ((secs (alist-get 'duration_in_seconds step))
        (started (alist-get 'started_on step)))
    (cond
     ((and started (gp-pipeline-step-running-p step))
      (gp-pipeline--format-secs
       (max 0 (floor (- (float-time) (float-time (date-to-time started)))))))
     (secs (gp-pipeline--format-secs secs))
     (t ""))))

(defun gp-pipeline--manual-gate-open-p (pipeline steps)
  "Non-nil when PIPELINE is only waiting on an open manual gate.
Bitbucket keeps reporting stage RUNNING for such pipelines, so the
gate has to be read off STEPS: nothing is executing, but a manual
step is startable."
  (and (not (gp-pipeline-finished-p pipeline))
       (not (cl-some #'gp-pipeline-step-running-p steps))
       (cl-some #'gp-pipeline-step-runnable-manual-p steps)))

(defun gp-pipeline--label (pipeline &optional steps)
  "Return a one-line label string for PIPELINE (number + status).
When STEPS reveal an open manual gate, show ⏸ and say so instead of
the indistinguishable running state."
  (let* ((state (gp-pipeline-state pipeline))
         (result (gp-pipeline-result pipeline))
         (gated (gp-pipeline--manual-gate-open-p pipeline steps))
         (g (if gated
                (cons "⏸" 'gp-pipeline-paused-face)
              (gp-pipeline--status-glyph state result)))
         (num (gp-pipeline-number pipeline)))
    (concat (propertize (car g) 'face (cdr g))
            (format " Pipeline #%s" (or num "?"))
            (propertize (format "  %s" (if gated "manual gate open"
                                         (or result state "")))
                        'face 'shadow))))

;;;; Rendering -----------------------------------------------------------------

(defun gp-pipeline--insert-step (step)
  "Insert one STEP line as a `gp-pipeline-step-section'."
  (let* ((state (gp-pipeline-step-state step))
         (result (gp-pipeline-step-result step))
         (g (gp-pipeline--status-glyph state result 'step))
         (name (or (alist-get 'name step) "step"))
         (dur (gp-pipeline--format-duration step))
         (runnable (gp-pipeline-step-runnable-manual-p step))
         (manual (gp-pipeline-step-manual-p step))
         (rerunnable (gp-pipeline-step-rerunnable-p step)))
    (magit-insert-section (gp-pipeline-step-section step)
      (magit-insert-heading
        (concat "    "
                (propertize (car g) 'face (cdr g))
                " "
                (propertize name 'face 'default)
                 (cond
                  (runnable (propertize "  [manual ▸ T]" 'face 'gp-pipeline-running-face))
                  (manual (propertize "  [manual]" 'face 'shadow)))
                (when rerunnable
                  (propertize "  [rerun ▸ P]" 'face 'gp-pipeline-running-face))
                ;; an armed deploy watcher, so a waiting gate is visible from
                ;; the PR itself rather than only in the watcher list
                (and manual (fboundp 'gp-deploy-watch-step-marker)
                     (boundp 'gp--pr) gp--pr
                     (ignore-errors
                       (gp-deploy-watch-step-marker
                        step (gp-pr-full-name gp--pr)
                        (gp-pr-source-branch gp--pr))))
                (unless (string-empty-p dur)
                  ;; A running step's duration is computed from `started_on'
                  ;; at RENDER time, so it is a snapshot, not a clock.  The
                  ;; poll deliberately skips the rerender while fetched data
                  ;; is unchanged, which is exactly what a still-running step
                  ;; returns -- so tag the text with its start timestamp and
                  ;; let `gp-pipeline--spinner-tick' retick it in place.
                  (apply #'propertize (format "  %s" dur) 'face 'shadow
                         (when (gp-pipeline-step-running-p step)
                           (list 'gp-pipeline-elapsed
                                 (alist-get 'started_on step)))))
                (propertize "   l:log" 'face 'shadow))))))

(defun gp--insert-pipelines (data)
  "Insert the Pipelines section from DATA (plist :current :recent).
:current is a sorted alist of (PIPELINE . STEPS) for the PR's head
commit, rendered in full (finished ones collapsed).  :recent (prior
commits' pipelines on the same branch) is not rendered here at all --
see `gp--insert-commits', which shows each one beside the commit it
belongs to instead: a dedicated \"Recent runs\" block here used to
repeat the same commit summary a second time with a status beside it,
and nothing there was actionable."
  (let* ((current (plist-get data :current))
         (recent (plist-get data :recent)))
    ;; still shown (with the "no pipeline" fallback) when only :recent has
    ;; anything, so a PR whose head commit has no run of its own doesn't
    ;; silently lose this section -- :recent itself renders in the Commits
    ;; section instead (see `gp--insert-commits'), not here.
    (when (or current recent)
      (magit-insert-section (magit-section 'pipelines)
        (magit-insert-heading
          (format "Pipelines (%d for this commit)" (length current)))
        (if current
            (pcase-dolist (`(,pipeline . ,steps) current)
              (let ((collapsed (gp-pipeline-finished-p pipeline)))
                (magit-insert-section
                    (gp-pipeline-section (cons pipeline steps) collapsed)
                  (magit-insert-heading
                    (concat "  " (gp-pipeline--label pipeline steps)
                            (propertize "   s:stop  T:trigger"
                                        'face 'shadow)))
                  (if steps
                      (dolist (s steps) (gp-pipeline--insert-step s))
                    (insert "    (no steps)\n")))))
          (insert "  (no pipeline for the current commit)\n"))
        (insert "\n")))))

;;;; Fetching (network, via the protocol) --------------------------------------

(defun gp-pipeline--partition (all commit)
  "Split ALL pipelines into (CURRENT . RECENT) around head COMMIT.
Paged fetches can duplicate a run when a new pipeline starts
mid-pagination (the pages shift) -- dedupe by uuid first."
  (let* ((all (cl-remove-duplicates all
                                    :key (lambda (p) (gp-pipeline-id p))
                                    :test #'equal
                                    :from-end t))
         (current (gp-pipelines-match-commit all commit))
         (current-uuids (mapcar (lambda (p) (gp-pipeline-id p)) current)))
    (cons current
          (cl-remove-if (lambda (p) (member (gp-pipeline-id p) current-uuids))
                        all))))

(defun gp-pipeline--assemble (current steps-of recent summaries)
  "Build the (:current … :recent …) plist from fetched parts.
STEPS-OF maps a pipeline uuid to its steps; SUMMARIES maps a commit
hash to its commit message."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (p current)
      (puthash (gp-pipeline-id p)
               (length (gethash (gp-pipeline-id p) steps-of))
               counts))
    (list :current
          (mapcar (lambda (p)
                    (cons p (gethash (gp-pipeline-id p) steps-of)))
                  (gp-pipelines-sort current counts))
          :recent
          (mapcar (lambda (p)
                    (cons p (gp-commit-summary
                             (gethash (gp-pipeline-commit p) summaries))))
                  recent))))

(defun gp-pipeline-fetch-for-pr (pr)
  "Return pipeline data for PR as a plist (:current ALIST :recent LIST).

:current is a sorted alist of (PIPELINE . STEPS) for the PR's
current head commit -- the runs that matter -- most steps first,
with each pipeline's steps fetched.

:recent is a short list of (PIPELINE . COMMIT-SUMMARY) conses for
the branch's other recent commits (newest first, no steps fetched)
-- a lightweight status summary.  Both may be nil.  Returns nil on
any error (the detail view degrades gracefully; the error is logged)."
  (condition-case e
    (let* ((full-name (gp-pr-full-name pr))
           (branch (gp-pr-source-branch pr))
           (commit (gp-pr-source-commit pr))
           ;; one branch fetch; partition into current-commit vs the rest
           (split (gp-pipeline--partition
                   (gp-pipelines-for-branch full-name branch gp-pipeline-max)
                   commit))
           (current (car split))
           (recent (seq-take (cdr split) gp-pipeline-recent-max))
           (steps-of (make-hash-table :test 'equal))
           (summaries (make-hash-table :test 'equal)))
      (dolist (p current)
        (let ((uuid (gp-pipeline-id p)))
          (puthash uuid (gp-pipeline-steps full-name uuid) steps-of)))
      ;; attach each run's commit summary (one extra lookup per run,
      ;; cached) so the renderer needs no network access
      (dolist (p recent)
        (let ((hash (gp-pipeline-commit p)))
          (puthash hash (gp-commit-message full-name hash) summaries)))
      (gp-pipeline--assemble current steps-of recent summaries))
    (error
     (gp-log-error "pipeline fetch failed: %s" (error-message-string e))
     nil)))

(defun gp-pipeline-fetch-for-pr-async (pr callback)
  "Fetch PR's pipeline data asynchronously; CALLBACK gets the plist.
Non-blocking twin of `gp-pipeline-fetch-for-pr', producing the exact
same (:current … :recent …) shape.  This is what the detail view
polls: the synchronous version blocks Emacs's main thread for the
whole branch fetch + one step fetch per current-commit run, which
froze the UI once per poll interval.

The fan-outs (steps per current run, commit message per recent run)
run concurrently and a shared counter fires CALLBACK once the last
one lands.  CALLBACK gets nil if the branch fetch itself failed --
the caller treats that like the synchronous nil (keeps stale data)."
  (gp-pipeline-fetch-for-branch-async
   (gp-pr-full-name pr) (gp-pr-source-branch pr) (gp-pr-source-commit pr) callback))

(defun gp-pipeline-fetch-for-branch-async (full-name branch commit callback)
  "Fetch BRANCH's pipeline data in FULL-NAME; CALLBACK gets the plist.
COMMIT is the commit whose runs count as `:current'.  Split out of
`gp-pipeline-fetch-for-pr-async' so a merged PR can ask the same
question about its DESTINATION branch and merge commit -- the run that
actually deploys."
  (let ((full-name full-name)
        (branch branch)
        (commit commit))
    (gp-pipelines-for-branch-async
     full-name branch gp-pipeline-max nil
     (lambda (all)
       (condition-case e
           (if (null all)
               ;; nil means the fetch failed OR the branch genuinely has no
               ;; runs; both are reported as an empty result, exactly as the
               ;; synchronous path did.
               (funcall callback nil)
             (let* ((split (gp-pipeline--partition all commit))
                    (current (car split))
                    (recent (seq-take (cdr split) gp-pipeline-recent-max))
                    (steps-of (make-hash-table :test 'equal))
                    (summaries (make-hash-table :test 'equal))
                    ;; one tick per step fetch and per commit-message lookup
                    (pending (+ (length current) (length recent)))
                    (done nil))
               (cl-flet ((settle ()
                           (setq pending (1- pending))
                           (when (and (<= pending 0) (not done))
                             (setq done t)
                             (funcall callback
                                      (gp-pipeline--assemble
                                       current steps-of recent summaries)))))
                 (if (zerop pending)
                     ;; runs exist but none current and none recent
                     (funcall callback (gp-pipeline--assemble
                                        current steps-of recent summaries))
                   (dolist (p current)
                     (let ((uuid (gp-pipeline-id p)))
                       (gp-pipeline-steps-async
                        full-name uuid
                        (lambda (steps)
                          (puthash uuid steps steps-of)
                          (settle)))))
                   (dolist (p recent)
                     (let ((hash (gp-pipeline-commit p)))
                       (gp-commit-message-async
                        full-name hash
                        (lambda (msg)
                          (puthash hash msg summaries)
                          (settle)))))))))
         (error
          (gp-log-error "pipeline fetch failed: %s" (error-message-string e))
          (funcall callback nil)))))))

(defun gp-pipelines-match-commit (pipelines commit)
  "Return PIPELINES whose target commit matches COMMIT (delegates to backend)."
  (gp--pipelines-match-commit (git-platform-backend) pipelines commit))

;;;; Manual-step trigger hook (external script) --------------------------------

;; Bitbucket Cloud has no public REST endpoint that advances an individual
;; halted manual step (BCLOUD-20050, open since 2020); only the web UI can.
;; `gp-pipeline-run-manual-step' therefore re-triggers the whole pipeline,
;; which re-runs everything up to the gate.  Users who have a working
;; out-of-band way to click that gate -- typically a browser-automation
;; script driving a logged-in session -- can plug it in here instead.
;;
;; The hook is deliberately backend-agnostic and lives outside the protocol
;; layer: it is a user-supplied escape hatch, not a backend capability, so a
;; backend gaining a real API later does not have to model it.

(defcustom gp-pipeline-deploy-script nil
  "External command run to trigger a manual deploy step, or nil.

When set, `gp-detail-pipeline-run-manual' runs this command instead
of prompting: it is the only route that advances THIS build's gate,
where the browser and new-run choices either need a human click or
re-execute everything before the gate.  Clear it to get that prompt
back.  Intended for backends whose API cannot advance a gated step in
place \(Bitbucket Cloud), where the only working route is out-of-band.

The value is a list of strings: the program followed by its fixed
arguments, e.g. `(\"~/bin/bb-deploy\")'.  No shell is involved, so
arguments are passed verbatim -- no quoting or word-splitting.  The
program is looked up on `exec-path'; a leading \"~\" is expanded.

Context is passed in the ENVIRONMENT, not as arguments, so one script
serves every repo and step without the caller knowing its flags:

  GP_WORKSPACE      workspace / owner (\"acme\" of \"acme/web\")
  GP_REPO           repository slug (\"web\" of \"acme/web\")
  GP_FULL_NAME      \"acme/web\"
  GP_BRANCH         the PR's source branch
  GP_PIPELINE_ID    pipeline build number (\"1234\")
  GP_PIPELINE_UUID  pipeline uuid, braces included
  GP_STEP_NAME      step name, e.g. \"Deploy to DEV\"
  GP_STEP_UUID      step uuid, braces included
  GP_STEP_STATE     step state, e.g. \"HALTED\"
  GP_PR_ID          pull request id
  GP_WEB_URL        web-UI URL, deep-linked to the step

Step uuids change when a step is re-run, so a script should prefer
resolving by GP_STEP_NAME against the live build.

The command runs asynchronously with output streamed to
`gp-pipeline-deploy-buffer'; Emacs is never blocked and the buffer is
not popped up -- the result arrives as an OS notification (see
`gp-notify') and an echo-area message."
  :type '(choice (const :tag "No script (re-trigger the pipeline)" nil)
                 (repeat :tag "Program and arguments" string))
  :group 'bitbucket)

(defcustom gp-pipeline-deploy-buffer "*gp-deploy*"
  "Buffer name for `gp-pipeline-deploy-script' output."
  :type 'string :group 'bitbucket)

(defcustom gp-pipeline-deploy-notify t
  "Whether a finished deploy script raises an OS notification.
A browser-driven deploy runs long enough that the user has usually
switched away from Emacs, so the echo-area message alone is missed.

Narrows the package-wide `gp-notify': the notification is sent only
when both are non-nil.  Set this to nil to keep just deploy results
in the echo area and `gp-pipeline-deploy-buffer'."
  :type 'boolean :group 'bitbucket)

(defun gp-pipeline--deploy-env (full-name branch pipeline step pr)
  "Return an `process-environment' with GP_* context for the deploy script.
FULL-NAME is \"workspace/slug\"; PIPELINE and STEP are the objects at
point; PR supplies the pull request id.  Values that cannot be
resolved are omitted rather than exported empty, so a script can tell
\"absent\" from \"empty\" with a plain -z test."
  (let* ((parts (split-string (or full-name "") "/"))
         (workspace (car parts))
         (slug (cadr parts))
         (pairs
          (list (cons "GP_WORKSPACE" workspace)
                (cons "GP_REPO" slug)
                (cons "GP_FULL_NAME" full-name)
                (cons "GP_BRANCH" branch)
                (cons "GP_PIPELINE_ID"
                      (let ((n (gp-pipeline-number pipeline)))
                        (and n (format "%s" n))))
                (cons "GP_PIPELINE_UUID" (gp-pipeline-id pipeline))
                (cons "GP_STEP_NAME" (alist-get 'name step))
                (cons "GP_STEP_UUID" (gp-pipeline-step-id step))
                (cons "GP_STEP_STATE" (gp-pipeline-step-state step))
                (cons "GP_PR_ID"
                      (let ((id (alist-get 'id pr)))
                        (and id (format "%s" id))))
                (cons "GP_WEB_URL"
                      (ignore-errors
                        (gp-pipeline-web-url full-name pipeline step)))))
         (env process-environment))
    (dolist (p pairs env)
      (when (and (cdr p) (not (equal (cdr p) "")))
        (push (format "%s=%s" (car p) (cdr p)) env)))))

(defun gp-pipeline--deploy-run (full-name branch pipeline step pr)
  "Run `gp-pipeline-deploy-script' for STEP, streaming output to a buffer.
Asynchronous: browser automation takes tens of seconds, and blocking
Emacs on it is exactly what the async work in this package removed.
Refreshes the detail view when the script exits successfully."
  (unless gp-pipeline-deploy-script
    (user-error "No `gp-pipeline-deploy-script' configured"))
  (let* ((cmd (copy-sequence gp-pipeline-deploy-script))
         (program (expand-file-name (car cmd)))
         (name (or (alist-get 'name step) "?"))
         (buf (get-buffer-create gp-pipeline-deploy-buffer))
         ;; Captured HERE, not read in the sentinel: a process sentinel runs
         ;; in whatever buffer is current when the process exits -- usually
         ;; the process buffer -- where `gp--pr' is nil and
         ;; `gp-detail-refresh' would refresh nothing.
         (detail-buf (current-buffer))
         (process-environment
          (gp-pipeline--deploy-env full-name branch pipeline step pr)))
    (unless (or (file-executable-p program) (executable-find (car cmd)))
      (user-error "Deploy script not found or not executable: %s" (car cmd)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\n=== %s | %s | build %s ===\n"
                        full-name name
                        (or (gp-pipeline-number pipeline) "?")))))
    ;; Deliberately NOT `display-buffer': the run is long and unattended,
    ;; and stealing a window mid-review is worse than the notification and
    ;; echo-area message that already report the result.  The buffer is
    ;; there when wanted.
    (message "Deploy script started for %S; output in %s"
             name gp-pipeline-deploy-buffer)
    (gp-log 'info "deploy script: %S for %s/%s" cmd full-name name)
    (make-process
     :name "gp-deploy"
     :buffer buf
     :command (cons (if (file-executable-p program) program (car cmd))
                    (cdr cmd))
     :noquery t
     :sentinel
     (lambda (_proc event)
       (let ((ok (string-prefix-p "finished" event)))
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert (format "=== %s ===\n" (string-trim event)))))
         (when gp-pipeline-deploy-notify
           (gp-notify (if ok "Deploy succeeded" "Deploy failed")
                      (format "%s — %s" name full-name)
                      (not ok)))
         (if ok
             (progn
               (message "Deploy script finished for %S" name)
               ;; A deploy to a shared environment can supersede what an
               ;; EARLIER pr's successful deploy step left behind; that pr's
               ;; own commit never changes, so only a repo-wide bust (not
               ;; a re-scan of this one commit) can catch it -- see
               ;; `gp-helm--deploy-cache-bust-repo'.
               (when (fboundp 'gp-helm--deploy-cache-bust-repo)
                 (gp-helm--deploy-cache-bust-repo full-name))
               ;; Refresh the detail buffer the run was started from, so the
               ;; step's new state shows up without a manual `g'.  Guarded on
               ;; it still being a live detail buffer: the user may have
               ;; killed or navigated it during the (long) run.
               (when (and (buffer-live-p detail-buf)
                          (fboundp 'gp-detail-refresh))
                 (with-current-buffer detail-buf
                   (when (bound-and-true-p gp--pr)
                     (gp-detail-refresh)))))
           (message "Deploy script failed for %S (%s); see %s"
                    name (string-trim event) gp-pipeline-deploy-buffer)))))))

;;;; Actions -------------------------------------------------------------------

(defun gp-pipeline--at-point ()
  "Return the pipeline (and its steps) for the section at point.
Looks at the step or pipeline section under point.  Signals if
point is not within a pipeline."
  (let ((sec (magit-current-section)))
    (while (and sec (not (object-of-class-p sec 'gp-pipeline-section)))
      (setq sec (and (slot-boundp sec 'parent) (oref sec parent))))
    (if (and sec (object-of-class-p sec 'gp-pipeline-section))
        (oref sec value)                ;; (PIPELINE . STEPS)
      (user-error "Point is not on a pipeline"))))

(defun gp-pipeline--step-at-point ()
  "Return the step object at point, or signal."
  (let ((sec (magit-current-section)))
    (if (and sec (object-of-class-p sec 'gp-pipeline-step-section))
        (oref sec value)
      (user-error "Point is not on a pipeline step"))))

(defun gp-detail-pipeline-stop ()
  "Stop the running pipeline at point (pipeline-level)."
  (interactive)
  (let* ((pp (gp-pipeline--at-point))
         (pipeline (car pp))
         (full-name (gp-pr-full-name gp--pr))
         (uuid (gp-pipeline-id pipeline)))
    (when (yes-or-no-p (format "Stop pipeline #%s? "
                               (or (gp-pipeline-number pipeline) "?")))
      (condition-case e
          (progn
            (gp-pipeline-stop full-name uuid)
            (message "Stop signalled for pipeline #%s"
                     (or (gp-pipeline-number pipeline) "?"))
            (when (fboundp 'gp-detail-refresh) (gp-detail-refresh)))
        (error (message "Could not stop pipeline: %s" (error-message-string e)))))))

(defun gp-detail-pipeline-trigger ()
  "Trigger / re-run the PR's branch pipeline (pipeline-level)."
  (interactive)
  (let* ((full-name (gp-pr-full-name gp--pr))
         (branch (gp-pr-source-branch gp--pr)))
    (when (yes-or-no-p (format "Trigger a new pipeline on %s? " branch))
      (condition-case e
          (let ((new (gp-pipeline-trigger full-name branch)))
            (message "Triggered pipeline #%s"
                     (or (gp-pipeline-number new) "?"))
            (when (fboundp 'gp-detail-refresh) (gp-detail-refresh)))
         (error (message "Could not trigger pipeline: %s"
                         (error-message-string e)))))))

(defun gp-detail-pipeline-trigger-or-run-manual ()
  "Trigger the pipeline, or run a waiting manual step when point is on one."
  (interactive)
  (let ((sec (magit-current-section)))
    (if (and sec (object-of-class-p sec 'gp-pipeline-step-section)
             (gp-pipeline-step-runnable-manual-p (oref sec value)))
        (gp-detail-pipeline-run-manual)
      (gp-detail-pipeline-trigger))))

(defun gp-detail-pipeline-run-manual ()
  "Run the waiting manual step at point.
Bitbucket's public API cannot resume a step in place (BCLOUD-20050,
open since 2020) -- only the web UI can.

With `gp-pipeline-deploy-script' set, that script runs directly: it
is the only route that advances THIS build's gate.  Without one, the
choice is between opening the step's pipeline page in the browser
(one click runs it in place) and spawning a NEW pipeline run, which
re-executes everything up to the gate.

Only offered on a manual step that is still waiting."
  (interactive)
  (let* ((step (gp-pipeline--step-at-point))
         (pp (gp-pipeline--at-point))
         (pipeline (car pp))
         (full-name (gp-pr-full-name gp--pr))
         (branch (gp-pr-source-branch gp--pr))
         (name (or (alist-get 'name step) "?")))
    (unless (gp-pipeline-step-manual-p step)
      (user-error "Step %S is not a manual step" name))
    (unless (gp-pipeline-step-runnable-manual-p step)
      (user-error "Manual step %S is not waiting (state: %s)"
                  name (or (gp-pipeline-step-state step) "?")))
    (pcase (if gp-pipeline-deploy-script
               ;; A configured script is the only route that advances THIS
               ;; build's gate rather than starting the pipeline over, so it
               ;; is simply the answer -- no point asking.  Clear it to get
               ;; the browser / new-run choice back.
               ?s
             (car (read-multiple-choice
                   (format "Run manual step %S (API can't resume it in place):"
                           name)
                   '((?b "browser"
                         "open the pipeline in the web UI and run the step there, in place")
                     (?n "new run"
                         "trigger a NEW pipeline run of this definition (re-runs earlier steps)")
                     (?q "quit" "do nothing")))))
      (?s (gp-pipeline--deploy-run full-name branch pipeline step gp--pr))
      (?b (browse-url (gp-pipeline-web-url full-name pipeline step)))
      (?n (condition-case e
              (progn
                (gp-pipeline-run-manual-step full-name branch pipeline step)
                (message "Triggered a new pipeline run for %S" name)
                (when (fboundp 'gp-detail-refresh) (gp-detail-refresh)))
            (error (message "Could not run manual step: %s"
                            (error-message-string e))))))))

(defun gp-detail-pipeline-arm-deploy ()
  "Arm the manual step at point to run as soon as the build reaches it.
Pressing `A' again on an armed step cancels it.  The waiting itself
lives in `gp-deploy-watch'; this is just the detail view's door into
it, kept beside the other pipeline commands so the keymap has one
place to point at."
  (interactive)
  (require 'gp-deploy-watch)
  (gp-deploy-watch-toggle-at-point))

(defun gp-detail-pipeline-rerun-step ()
  "Re-run the finished step at point in place.
Distinct from `gp-detail-pipeline-run-manual': this restarts an
already-finished (typically failed) step via the backend's own
per-step rerun capability (GitHub Actions supports this; Bitbucket
Pipelines does not, so `gp-pipeline-step-rerunnable-p' is always nil
there).  Only offered when the backend reports the step rerunnable."
  (interactive)
  (let* ((step (gp-pipeline--step-at-point))
         (pp (gp-pipeline--at-point))
         (pipeline (car pp))
         (full-name (gp-pr-full-name gp--pr))
         (name (or (alist-get 'name step) "?")))
    (unless (gp-pipeline-step-rerunnable-p step)
      (user-error "Step %S cannot be re-run individually on this backend" name))
    (condition-case e
        (progn
          (gp-pipeline-step-rerun full-name (gp-pipeline-id pipeline) step)
          (message "Re-running step %S" name)
          (when (fboundp 'gp-detail-refresh) (gp-detail-refresh)))
      (error (message "Could not re-run step: %s" (error-message-string e))))))

;;;; Step log buffer (live-polled while running) -------------------------------

(defvar-local gp-pipeline-log--ctx nil
  "Plist (:full-name :pipeline-uuid :step-uuid :step) for the log buffer.")
(defvar-local gp-pipeline-log--timer nil
  "Poll timer for a running step's log, or nil.")

(define-derived-mode gp-pipeline-log-mode special-mode "PR-Pipeline-Log"
  "Major mode for a pipeline step's log."
  (setq-local truncate-lines nil)
  (add-hook 'kill-buffer-hook #'gp-pipeline-log--cancel-timer nil t))

(defun gp-pipeline-log--cancel-timer ()
  "Cancel the poll timer for the current log buffer, if any."
  (when (timerp gp-pipeline-log--timer)
    (cancel-timer gp-pipeline-log--timer)
    (setq gp-pipeline-log--timer nil)))

(defun gp-pipeline-log--insert-classified (text)
  "Insert TEXT into the current buffer, one line at a time, applying
each line's platform classification (see
`gp-pipeline-step-log-classify-line') as a face on top of whatever
`ansi-color-apply-on-region' later derives from any SGR codes in it.
Splitting per line -- rather than classifying the raw TEXT wholesale
-- is what lets a `group' marker line be replaced by its own (often
empty) label while ordinary output lines around it pass through
untouched."
  (dolist (line (split-string (or text "") "\n"))
    (pcase-let ((`(,kind . ,shown) (gp-pipeline-step-log-classify-line line)))
      (insert (pcase kind
                ('command (propertize shown 'face 'gp-pipeline-log-command-face))
                ('group   (propertize shown 'face 'gp-pipeline-log-group-face))
                ('error   (propertize shown 'face 'gp-pipeline-failed-face))
                ('warning (propertize shown 'face 'gp-pipeline-running-face))
                (_ shown)))
      (insert "\n")))
  ;; the loop always adds a trailing newline; undo it so callers appending
  ;; after this (the "running" banner) don't get a blank line first
  (when (> (point) (point-min))
    (delete-char -1)))

(defun gp-pipeline-log--render (text running)
  "Replace the log buffer's contents with TEXT; note RUNNING state.
TEXT is the platform's raw captured log: real ANSI SGR escapes (colour,
bold, dim, …) are decoded into faces via `ansi-color-apply-on-region'
rather than shown as literal \"[90m\"-style codes, and each line gets
its platform-specific classification (see
`gp-pipeline-step-log-classify-line') -- e.g. Bitbucket's leading
\"+ \" command echo, or GitHub's \"##[group]\"/timestamp convention --
applied as a distinct face."
  (let ((inhibit-read-only t)
        (at-end (eobp)))
    (erase-buffer)
    (gp-pipeline-log--insert-classified text)
    (ansi-color-apply-on-region (point-min) (point-max))
    (when running
      (insert (propertize "\n⏳ running — tailing…\n" 'face 'shadow)))
    (when at-end (goto-char (point-max)))))

(defun gp-pipeline-log--poll (buf)
  "Refetch the log into BUF; stop polling once the step is no longer running."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((ctx gp-pipeline-log--ctx)
             (running (gp-pipeline-step-running-p (plist-get ctx :step)))
             (text (ignore-errors
                     (gp-pipeline-step-log (plist-get ctx :full-name)
                                           (plist-get ctx :pipeline-uuid)
                                           (plist-get ctx :step-uuid)))))
        (gp-pipeline-log--render text running)
        ;; the cached step won't flip to finished on its own; re-fetch the
        ;; step's state cheaply by asking only while we believe it runs.
        (unless running
          (gp-pipeline-log--cancel-timer))))))

(defun gp-detail-pipeline-step-log ()
  "Open the log of the step at point in a buffer.
If the step is still running, poll and append; otherwise show the
captured historical log."
  (interactive)
  (let* ((step (gp-pipeline--step-at-point))
         (pp (gp-pipeline--at-point))
         (pipeline (car pp))
         ;; captured before `with-current-buffer' -- `gp--pr' is
         ;; buffer-local to the detail buffer we are leaving
         (pr gp--pr)
         (full-name (gp-pr-full-name pr))
         (pipeline-uuid (gp-pipeline-id pipeline))
         (step-uuid (gp-pipeline-step-id step))
         (running (gp-pipeline-step-running-p step))
         (buf (get-buffer-create
               (gp--buffer-name
                (format "pipeline #%s log: %s"
                        (or (gp-pipeline-number pipeline) "?")
                        (or (alist-get 'name step) "step"))))))
    (with-current-buffer buf
      (gp-pipeline-log-mode)
      (setq gp-pipeline-log--ctx
            (list :full-name full-name :pipeline-uuid pipeline-uuid
                  :step-uuid step-uuid :step step))
      (gp-local-anchor-to-checkout pr)
      (gp-pipeline-log--cancel-timer)
      (let ((text (ignore-errors
                    (gp-pipeline-step-log full-name pipeline-uuid step-uuid))))
        (gp-pipeline-log--render text running))
      (when running
        (setq gp-pipeline-log--timer
              (run-with-timer gp-pipeline-log-poll-interval
                              gp-pipeline-log-poll-interval
                              #'gp-pipeline-log--poll buf))))
    (pop-to-buffer buf)))

(provide 'gp-pipeline)
;;; gp-pipeline.el ends here
