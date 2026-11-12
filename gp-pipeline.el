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
;;   s   stop the running pipeline
;;   T   trigger the pipeline, or run/redeploy the manual step at point
;;   P   re-run the finished step at point (where the backend allows it)
;;   A   arm/disarm a deploy watcher on the manual step at point
;;   l   open the step's log in a buffer (polled live while it runs)
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
(declare-function gp-pipelines-match-commit "git-platform")

(defvar gp--pr)                         ;; the detail buffer's PR (gp-ui.el)
(defvar gp-pipeline-deploy-script)      ;; defcustom, defined below
(declare-function gp-detail-refresh "gp-ui")
(declare-function gp--detail-rerender "gp-ui")
(declare-function gp-deploy-watch-step-marker "gp-deploy-watch")
(declare-function gp-deploy-watch-toggle-at-point "gp-deploy-watch")
(declare-function gp-helm--deploy-cache-bust-repo "gp-helm")

;;;; Faces ---------------------------------------------------------------------

(defface gp-pipeline-running-face '((t :inherit warning))
  "Face for a running pipeline/step." :group 'bitbucket-faces)
(defface gp-pipeline-spinner-face
  '((((class color) (background dark)) :foreground "#61afef")
    (((class color) (background light)) :foreground "#1f6feb")
    (t :inherit font-lock-keyword-face))
  "Face for the animated spinner of a running or queued pipeline/step.
Blue in both light and dark themes; keyword faces are often red, which
reads as an error."
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
  "Face for a step-log line echoing the command being run.
E.g. Bitbucket's leading \"+ \"." :group 'bitbucket-faces)
(defface gp-pipeline-log-group-face '((t :inherit magit-section-heading))
  "Face for a step-log line marking a named section.
E.g. GitHub's \"##[group]\"/\"##[endgroup]\"." :group 'bitbucket-faces)

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

;; An in-progress pipeline/step renders as a one-character animated spinner.
;; All frames are single-width, so the timer swaps the character in place
;; and never reflows the line.

(defcustom gp-pipeline-spinner-frames
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Frames of the in-progress spinner.
Every frame must be a single character of the same display width, or
the animation will shift the rest of the line."
  :type '(vector string) :group 'bitbucket)

(defcustom gp-pipeline-spinner-interval 0.08
  "Seconds between in-progress spinner frames."
  :type 'number :group 'bitbucket)

(defvar gp-pipeline--spinner-index 0
  "Current frame index into `gp-pipeline-spinner-frames'.")

(defvar gp-pipeline--spinner-timer nil
  "Repeating timer animating the in-progress spinners, or nil.")

(defvar gp-pipeline--deploy-running (make-hash-table :test 'equal)
  "Step id -> start time (`float-time') for an in-flight deploy script.
The forge keeps reporting the step's pre-run state while the local
`gp-pipeline--deploy-run' process works, so this registry is the only
source of truth for it; `gp-pipeline--insert-step' checks it first.")

(defun gp-pipeline--deploy-running-p (step)
  "Non-nil when a deploy script is currently running for STEP."
  (and (gethash (gp-pipeline-step-id step) gp-pipeline--deploy-running) t))

(defun gp-pipeline--spinner-frame ()
  "Return the spinner's current frame string."
  (aref gp-pipeline-spinner-frames
        (mod gp-pipeline--spinner-index (length gp-pipeline-spinner-frames))))

(defun gp-pipeline--spinner-glyph ()
  "Return a propertized spinner character for an in-progress entry.
Tagged `gp-pipeline-spinner' for `gp-pipeline--spinner-tick'."
  (propertize (gp-pipeline--spinner-frame)
              'face 'gp-pipeline-spinner-face
              'gp-pipeline-spinner t))

(defun gp-pipeline--spinner-repaint-buffer (frame)
  "Replace every spinner character in the current buffer with FRAME.
Rewrites in place, so point, marks and scroll stay put.  Returns
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
A tick landing mid-rerender sees no spinner, and only a rerender
restarts the timer (which the poll skips while data is unchanged), so
stopping on the first empty tick would freeze the spinner.")

(defvar gp-pipeline--spinner-misses 0
  "Consecutive `gp-pipeline--spinner-tick' runs that found no spinner.")

(defvar gp-pipeline--elapsed-last-second nil
  "Wall-clock second at which elapsed times were last reticked.
Throttles the elapsed repaint to once a second.")

(defun gp-pipeline--elapsed-repaint-buffer ()
  "Retick every running step's elapsed-time text in the current buffer.
Needed because the poll skips the redraw while a running step's JSON
is unchanged.  Rewrites just the tagged text, so point, marks and
folding stay put.  Returns non-nil when at least one duration was found."
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
Also resets the miss counter, restarting the grace window."
  (setq gp-pipeline--spinner-misses 0)
  (unless (timerp gp-pipeline--spinner-timer)
    (setq gp-pipeline--spinner-timer
          (run-with-timer gp-pipeline-spinner-interval
                          gp-pipeline-spinner-interval
                          #'gp-pipeline--spinner-tick))))

;;;; Status formatting (pure) --------------------------------------------------

(defun gp-pipeline--queued-p (state &optional step)
  "Non-nil when STATE means waiting for a runner rather than not started.
Bitbucket reports a queued step as READY, and a pipeline that has not
started yet as PENDING; GitHub's queued runs and jobs map to QUEUED.
A PENDING step, by contrast, is one whose turn has not come (a later
or manual step), so it is only queued when STEP is nil."
  (or (member state '("READY" "QUEUED"))
      (and (not step) (equal state "PENDING"))))

(defun gp-pipeline--status-glyph (state result &optional step)
  "Return (GLYPH . FACE) for a pipeline/step STATE and RESULT string.
A pipeline paused at an open manual gate (result PAUSED) gets ⏸.  An
in-progress or queued entry gets the animated spinner glyph (see
`gp-pipeline--spinner-glyph').  STEP non-nil means STATE is a step's,
as `gp-pipeline--queued-p' reads it."
  (cond
   ((equal result "PAUSED") (cons "⏸" 'gp-pipeline-paused-face))
   ((or (equal state "IN_PROGRESS") (gp-pipeline--queued-p state step))
    (gp-pipeline--spinner-ensure)
    (cons (gp-pipeline--spinner-glyph) 'gp-pipeline-spinner-face))
   ((member state '("PENDING" "READY" "BUILDING" nil))
    (cons "…" 'gp-pipeline-running-face))
   ((equal result "SUCCESSFUL") (cons "✔" 'gp-pipeline-success-face))
   ((member result '("FAILED" "ERROR")) (cons "✘" 'gp-pipeline-failed-face))
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

(defun gp-pipeline-step-redeployable-p (step)
  "Non-nil when STEP already succeeded and can be redeployed on demand.
Distinct from `gp-pipeline-step-runnable-manual-p' (a manual step still
waiting for its first run).  Requires `gp-pipeline-deploy-script': it
is the only route that can re-run the step (see
`gp-detail-pipeline-run-manual')."
  (and gp-pipeline-deploy-script
       (gp-pipeline-step-manual-p step)
       (equal (gp-pipeline-step-result step) "SUCCESSFUL")))

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
            (propertize (format "  %s" (cond (gated "manual gate open")
                                             ((and (not result) (gp-pipeline--queued-p state))
                                              "queued")
                                             (t (or result state ""))))
                        'face 'shadow))))

;;;; Rendering -----------------------------------------------------------------

(defun gp-pipeline--insert-step (step)
  "Insert one STEP line as a `gp-pipeline-step-section'."
  (let* ((deploy-started (gethash (gp-pipeline-step-id step) gp-pipeline--deploy-running))
         (state (gp-pipeline-step-state step))
         (result (gp-pipeline-step-result step))
         ;; A local deploy script overrides the forge-reported glyph,
         ;; which does not change while it runs.
         (g (if deploy-started
                (progn (gp-pipeline--spinner-ensure)
                       (cons (gp-pipeline--spinner-glyph) 'gp-pipeline-spinner-face))
              (gp-pipeline--status-glyph state result 'step)))
         (name (or (alist-get 'name step) "step"))
         (dur (gp-pipeline--format-duration step))
         (runnable (gp-pipeline-step-runnable-manual-p step))
         (redeployable (gp-pipeline-step-redeployable-p step))
         (manual (gp-pipeline-step-manual-p step))
         (rerunnable (gp-pipeline-step-rerunnable-p step)))
    (magit-insert-section (gp-pipeline-step-section step)
      (magit-insert-heading
        (concat "    "
                (propertize (car g) 'face (cdr g))
                " "
                (propertize name 'face 'default)
                 (cond
                  (deploy-started (propertize "  [deploying…]" 'face 'gp-pipeline-running-face))
                  (runnable (propertize "  [manual ▸ T]" 'face 'gp-pipeline-running-face))
                  (redeployable (propertize "  [redeploy ▸ T]" 'face 'gp-pipeline-running-face))
                  (manual (propertize "  [manual]" 'face 'shadow))
                  ((gp-pipeline--queued-p state 'step)
                   (propertize "  queued" 'face 'shadow)))
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
                (cond
                 ;; Tagged so `gp-pipeline--spinner-tick' reticks it in place.
                 (deploy-started
                  (propertize
                   (format "  %s" (gp-pipeline--format-secs
                                   (max 0 (floor (- (float-time) deploy-started)))))
                   'face 'shadow
                   'gp-pipeline-elapsed
                   (format-time-string "%Y-%m-%dT%H:%M:%S+00:00" deploy-started t)))
                 ((not (string-empty-p dur))
                  ;; A render-time snapshot; tag a running step's text with
                  ;; its start so `gp-pipeline--spinner-tick' reticks it.
                  (apply #'propertize (format "  %s" dur) 'face 'shadow
                         (when (gp-pipeline-step-running-p step)
                           (list 'gp-pipeline-elapsed
                                 (alist-get 'started_on step))))))
                (propertize "   l:log" 'face 'shadow))))))

(defun gp--insert-pipelines (data)
  "Insert the Pipelines section from DATA (plist :current :recent).
:current is a sorted alist of (PIPELINE . STEPS) for the PR's head
commit, rendered in full (finished ones collapsed).  :recent is not
rendered here but beside each commit by `gp--insert-commits'; it only
keeps this section (with its \"no pipeline\" fallback) visible."
  (let* ((current (plist-get data :current))
         (recent (plist-get data :recent)))
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
                            (propertize "   s:stop  T:trigger  w:web"
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
mid-pagination (the pages shift) -- dedupe by id first."
  (let* ((all (cl-remove-duplicates all
                                    :key #'gp-pipeline-id
                                    :test #'equal
                                    :from-end t))
         (current (gp-pipelines-match-commit all commit))
         (current-uuids (mapcar #'gp-pipeline-id current)))
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

:current is a sorted alist of (PIPELINE . STEPS) for the PR's head
commit, most steps first.  :recent is a short list of
\(PIPELINE . COMMIT-SUMMARY) for the branch's other recent commits,
newest first, without steps.  Both may be nil.  Returns nil (and logs)
on any error."
  (condition-case e
    (let* ((full-name (gp-pr-full-name pr))
           (branch (gp-pr-source-branch pr))
           (commit (gp-pr-source-commit pr))
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
      ;; so the renderer needs no network access
      (dolist (p recent)
        (let ((hash (gp-pipeline-commit p)))
          (puthash hash (gp-commit-message full-name hash) summaries)))
      (gp-pipeline--assemble current steps-of recent summaries))
    (error
     (gp-log-error "pipeline fetch failed: %s" (error-message-string e))
     nil)))

(defun gp-pipeline-fetch-for-pr-async (pr callback)
  "Fetch PR's pipeline data asynchronously; CALLBACK gets the plist.
Non-blocking twin of `gp-pipeline-fetch-for-pr' with the same shape;
this is what the detail view polls.  The per-run fan-outs run
concurrently and CALLBACK fires once the last one lands.  CALLBACK gets
nil if the branch fetch failed or found no runs."
  (gp-pipeline-fetch-for-branch-async
   (gp-pr-full-name pr) (gp-pr-source-branch pr) (gp-pr-source-commit pr) callback))

(defun gp-pipeline-fetch-for-branch-async (full-name branch commit callback)
  "Fetch BRANCH's pipeline data in FULL-NAME; CALLBACK gets the plist.
COMMIT is the commit whose runs count as `:current'.  Lets a merged PR
ask about its destination branch and merge commit -- the run that
deploys."
  (gp-pipelines-for-branch-async
   full-name branch gp-pipeline-max nil
   (lambda (all)
     (condition-case e
         (if (null all)
             ;; fetch failed OR the branch has no runs
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
        (funcall callback nil))))))

;;;; Manual-step trigger hook (external script) --------------------------------

;; Bitbucket Cloud has no REST endpoint that advances a halted manual step
;; (BCLOUD-20050); `gp-pipeline-run-manual-step' re-triggers the whole
;; pipeline instead.  A user script (e.g. browser automation) can click the
;; gate out-of-band.  It is a user escape hatch, deliberately kept outside
;; the protocol layer.

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
Reports that the step was TRIGGERED, not that the deployment finished;
use `gp-deploy-watch' for the actual outcome.  Narrows the package-wide
`gp-notify': the notification is sent only when both are non-nil."
  :type 'boolean :group 'bitbucket)

(defun gp-pipeline--deploy-env (full-name branch pipeline step pr)
  "Return a `process-environment' with GP_* context for the deploy script.
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

(defun gp-pipeline--deploy-run (full-name branch pipeline step pr &optional on-exit)
  "Run `gp-pipeline-deploy-script' for STEP, streaming output to a buffer.
Asynchronous.  Refreshes the detail view when the script exits.

ON-EXIT, if given, is called with OK once the process exits: non-nil
for a clean exit, nil otherwise.  Use it to learn the real outcome;
the process is still running when this function returns.

While it runs, STEP's id is registered in `gp-pipeline--deploy-running'
so the step line shows a spinner and elapsed time."
  (unless gp-pipeline-deploy-script
    (user-error "No `gp-pipeline-deploy-script' configured"))
  (let* ((cmd (copy-sequence gp-pipeline-deploy-script))
         (program (expand-file-name (car cmd)))
         (name (or (alist-get 'name step) "?"))
         (step-id (gp-pipeline-step-id step))
         (buf (get-buffer-create gp-pipeline-deploy-buffer))
         ;; Captured here: the sentinel runs in an arbitrary buffer where
         ;; `gp--pr' is nil.
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
    ;; Deliberately not displayed: the notification reports the result.
    (message "Deploy script started for %S; output in %s"
             name gp-pipeline-deploy-buffer)
    (gp-log 'info "deploy script: %S for %s/%s" cmd full-name name)
    (when step-id
      (puthash step-id (float-time) gp-pipeline--deploy-running)
      ;; Paint the spinner immediately.
      (when (and (buffer-live-p detail-buf) (fboundp 'gp--detail-rerender))
        (with-current-buffer detail-buf
          (when (bound-and-true-p gp--pr) (gp--detail-rerender detail-buf)))))
    (make-process
     :name "gp-deploy"
     :buffer buf
     :command (cons (if (file-executable-p program) program (car cmd))
                    (cdr cmd))
     :noquery t
     :sentinel
     (lambda (_proc event)
       (when step-id (remhash step-id gp-pipeline--deploy-running))
       (let ((ok (string-prefix-p "finished" event)))
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert (format "=== %s ===\n" (string-trim event)))))
         ;; OK means the step was triggered, NOT that the deployment
         ;; succeeded; only `gp-deploy-watch' sees that verdict.
         (when gp-pipeline-deploy-notify
           (gp-notify (if ok "Deploy triggered" "Deploy trigger failed")
                      (format "%s — %s" name full-name)
                      (not ok)))
         (when ok
           (message "Deploy triggered for %S; Bitbucket is now running it" name)
           ;; A deploy can supersede an earlier PR's, whose commit never
           ;; changes, so bust repo-wide.
           (when (fboundp 'gp-helm--deploy-cache-bust-repo)
             (gp-helm--deploy-cache-bust-repo full-name)))
         (unless ok
           (message "Deploy script failed for %S (%s); see %s"
                    name (string-trim event) gp-pipeline-deploy-buffer))
         ;; Either way the spinner must clear; the buffer may be gone.
         (if ok
             (when (and (buffer-live-p detail-buf) (fboundp 'gp-detail-refresh))
               (with-current-buffer detail-buf
                 (when (bound-and-true-p gp--pr)
                   (gp-detail-refresh))))
           (when (and (buffer-live-p detail-buf) (fboundp 'gp--detail-rerender))
             (with-current-buffer detail-buf
               (when (bound-and-true-p gp--pr)
                 (gp--detail-rerender detail-buf)))))
         (when on-exit (funcall on-exit ok)))))))

;;;; Actions -------------------------------------------------------------------

(defun gp-pipeline--at-point ()
  "Return (PIPELINE . STEPS) for the section at point, or signal."
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
  "Trigger the pipeline, run a waiting manual step, or redeploy at point.
A waiting manual step runs via `gp-detail-pipeline-run-manual'; a
redeployable one (see `gp-pipeline-step-redeployable-p') via
`gp-detail-pipeline-redeploy'; anything else triggers a new run."
  (interactive)
  (let* ((sec (magit-current-section))
         (step (and sec (object-of-class-p sec 'gp-pipeline-step-section)
                    (oref sec value))))
    (cond
     ((and step (gp-pipeline-step-runnable-manual-p step))
      (gp-detail-pipeline-run-manual))
     ((and step (gp-pipeline-step-redeployable-p step))
      (gp-detail-pipeline-redeploy))
     (t (gp-detail-pipeline-trigger)))))

(defun gp-detail-pipeline-run-manual ()
  "Run the waiting manual step at point.
Bitbucket's API cannot resume a step in place (BCLOUD-20050).  With
`gp-pipeline-deploy-script' set, that script runs directly.  Otherwise
choose between the browser (runs it in place) and a NEW pipeline run,
which re-executes everything up to the gate."
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
               ;; the script advances THIS build's gate, so don't ask
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

(defun gp-detail-pipeline-redeploy ()
  "Redeploy the already-successful manual step at point.
Re-runs `gp-pipeline-deploy-script' for a step whose result is already
SUCCESSFUL, e.g. after a rollback.  Requires
`gp-pipeline-step-redeployable-p'.  Asks for confirmation first."
  (interactive)
  (let* ((step (gp-pipeline--step-at-point))
         (pp (gp-pipeline--at-point))
         (pipeline (car pp))
         (full-name (gp-pr-full-name gp--pr))
         (branch (gp-pr-source-branch gp--pr))
         (name (or (alist-get 'name step) "?")))
    (unless (gp-pipeline-step-redeployable-p step)
      (user-error "Step %S is not redeployable (needs `gp-pipeline-deploy-script' \
and a SUCCESSFUL result; state: %s, result: %s)"
                  name (or (gp-pipeline-step-state step) "?")
                  (or (gp-pipeline-step-result step) "?")))
    (when (yes-or-no-p (format "Redeploy already-successful step %S? " name))
      (gp-pipeline--deploy-run full-name branch pipeline step gp--pr))))

(defun gp-detail-pipeline-arm-deploy ()
  "Arm the manual step at point to run as soon as the build reaches it.
Pressing `A' again on an armed step cancels it.  The waiting itself
lives in `gp-deploy-watch'."
  (interactive)
  (require 'gp-deploy-watch)
  (gp-deploy-watch-toggle-at-point))

(defun gp-detail-pipeline-rerun-step ()
  "Re-run the finished step at point in place.
Distinct from `gp-detail-pipeline-run-manual': this restarts a
finished (typically failed) step.  Requires
`gp-pipeline-step-rerunnable-p' (never true on Bitbucket)."
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
  "Insert TEXT line by line, faced by `gp-pipeline-step-log-classify-line'.
Leaves no trailing newline."
  (dolist (line (split-string (or text "") "\n"))
    (pcase-let ((`(,kind . ,shown) (gp-pipeline-step-log-classify-line line)))
      (insert (pcase kind
                ('command (propertize shown 'face 'gp-pipeline-log-command-face))
                ('group   (propertize shown 'face 'gp-pipeline-log-group-face))
                ('error   (propertize shown 'face 'gp-pipeline-failed-face))
                ('warning (propertize shown 'face 'gp-pipeline-running-face))
                (_ shown)))
      (insert "\n")))
  (when (> (point) (point-min))
    (delete-char -1)))

(defun gp-pipeline-log--render (text running)
  "Replace the log buffer's contents with TEXT; note RUNNING state.
TEXT is the raw log; ANSI SGR escapes are decoded into faces and each
line is classified (see `gp-pipeline-log--insert-classified')."
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
        ;; NOTE: :step is the snapshot taken when the log was opened and is
        ;; never re-fetched, so RUNNING does not change between polls.
        (unless running
          (gp-pipeline-log--cancel-timer))))))

(defun gp-detail-pipeline-step-log ()
  "Open the log of the step at point in a buffer.
A running step's log is re-polled every `gp-pipeline-log-poll-interval'
seconds."
  (interactive)
  (let* ((step (gp-pipeline--step-at-point))
         (pp (gp-pipeline--at-point))
         (pipeline (car pp))
         ;; `gp--pr' is local to the detail buffer we are leaving
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
