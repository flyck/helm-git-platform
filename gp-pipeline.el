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
(require 'magit-section)
(require 'git-platform)
(require 'gp-log)

(declare-function gp-pr-full-name "git-platform")
(declare-function gp-pr-source-branch "git-platform")

(defvar gp--pr)                         ;; the detail buffer's PR (gp-ui.el)
(declare-function gp-detail-refresh "gp-ui")

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
(defclass gp-pipeline-recent-section (magit-section) ())

;;;; Spinner -------------------------------------------------------------------

;; An in-progress pipeline/step renders as a one-character animated spinner
;; instead of a static glyph.  The frames sweep a Braille dot pair left to
;; right across the cell, so the motion reads horizontally -- like progress
;; along the pipeline -- rather than as a vertical orbit.  All frames are
;; single-width characters from the same Braille block, so the animation never
;; reflows the line: the timer only swaps the character in place, it never
;; inserts or deletes.

(defcustom gp-pipeline-spinner-frames
  ["▏" "▎" "▍" "▌" "▋" "▊" "▉" "▊" "▋" "▌" "▍" "▎"]
  "Frames of the in-progress spinner.
The default is a bar that grows and shrinks horizontally, so the
motion reads left-to-right rather than as a vertical orbit.
Every frame must be a single character of the same display width, or
the animation will shift the rest of the line."
  :type '(vector string) :group 'bitbucket)

(defcustom gp-pipeline-spinner-interval 0.12
  "Seconds between in-progress spinner frames."
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

(defun gp-pipeline--spinner-tick ()
  "Advance the spinner and repaint it in every buffer showing one.
Stops the timer once no live buffer contains a spinner."
  (setq gp-pipeline--spinner-index (1+ gp-pipeline--spinner-index))
  (let ((frame (gp-pipeline--spinner-frame))
        (any nil))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (derived-mode-p 'magit-section-mode)
            (when (gp-pipeline--spinner-repaint-buffer frame)
              (setq any t))))))
    (unless any (gp-pipeline--spinner-stop))))

(defun gp-pipeline--spinner-stop ()
  "Cancel the spinner timer."
  (when (timerp gp-pipeline--spinner-timer)
    (cancel-timer gp-pipeline--spinner-timer))
  (setq gp-pipeline--spinner-timer nil))

(defun gp-pipeline--spinner-ensure ()
  "Start the spinner timer unless it is already running."
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
                (unless (string-empty-p dur)
                  (propertize (format "  %s" dur) 'face 'shadow))
                (propertize "   l:log" 'face 'shadow))))))

(defun gp-pipeline--short-hash (hash)
  "Return a 8-char short form of commit HASH, or \"\"."
  (if (and hash (>= (length hash) 8)) (substring hash 0 8) (or hash "")))

(defun gp--insert-pipelines (data)
  "Insert the Pipelines section from DATA (plist :current :recent).
:current is a sorted alist of (PIPELINE . STEPS) for the PR's head
commit (rendered in full, finished ones collapsed); :recent is a
list of prior-commit pipelines shown as a one-line status summary."
  (let ((current (plist-get data :current))
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
                            (propertize "   s:stop  T:trigger"
                                        'face 'shadow)))
                  (if steps
                      (dolist (s steps) (gp-pipeline--insert-step s))
                    (insert "    (no steps)\n")))))
          (insert "  (no pipeline for the current commit)\n"))
        ;; status summary of runs on the branch's other recent commits;
        ;; each run is its own (foldable, navigable) section.
        (when recent
          (magit-insert-section (gp-pipeline-recent-section nil t)
            (magit-insert-heading
              (format "  Recent runs on this branch (%d)" (length recent)))
            (dolist (entry recent)
              (gp-pipeline--insert-recent entry))))
        (insert "\n")))))

(defun gp-pipeline--insert-recent (entry)
  "Insert one recent-run ENTRY (a cons (PIPELINE . SUMMARY)) as a section."
  (let* ((p (car entry))
         (summary (cdr entry))
         (state (gp-pipeline-state p))
         (result (gp-pipeline-result p))
         (g (gp-pipeline--status-glyph state result)))
    (magit-insert-section (gp-pipeline-recent-section p)
      (magit-insert-heading
        (concat "    "
                (propertize (car g) 'face (cdr g))
                (format " #%s " (or (gp-pipeline-number p) "?"))
                (propertize (gp-pipeline--short-hash (gp-pipeline-commit p))
                            'face 'magit-hash)
                (when (and summary (not (string-empty-p summary)))
                  (propertize (format "  %s" summary) 'face 'default))
                (propertize (format "   %s" (or result state ""))
                            'face 'shadow))))))

;;;; Fetching (network, via the protocol) --------------------------------------

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
           ;; one branch fetch; partition into current-commit vs the rest.
           ;; Paged fetches can duplicate a run when a new pipeline starts
           ;; mid-pagination (the pages shift) -- dedupe by uuid.
           (all (cl-remove-duplicates
                 (gp-pipelines-for-branch full-name branch gp-pipeline-max)
                 :key (lambda (p) (alist-get 'uuid p))
                 :test #'equal
                 :from-end t))
           (current (gp-pipelines-match-commit all commit))
           (current-uuids (mapcar (lambda (p) (alist-get 'uuid p)) current))
           (recent (cl-remove-if
                    (lambda (p) (member (alist-get 'uuid p) current-uuids))
                    all))
           (steps-of (make-hash-table :test 'equal))
           (counts (make-hash-table :test 'equal)))
      (dolist (p current)
        (let* ((uuid (alist-get 'uuid p))
               (steps (gp-pipeline-steps full-name uuid)))
          (puthash uuid steps steps-of)
          (puthash uuid (length steps) counts)))
      (list :current
            (mapcar (lambda (p)
                      (cons p (gethash (alist-get 'uuid p) steps-of)))
                    (gp-pipelines-sort current counts))
            :recent
            ;; attach each run's commit summary (one extra lookup per run,
            ;; cached) so the renderer needs no network access
            (mapcar (lambda (p)
                      (cons p (gp-commit-summary
                               (gp-commit-message
                                full-name (gp-pipeline-commit p)))))
                    (seq-take recent gp-pipeline-recent-max))))
    (error
     (gp-log-error "pipeline fetch failed: %s" (error-message-string e))
     nil)))

(defun gp-pipelines-match-commit (pipelines commit)
  "Return PIPELINES whose target commit matches COMMIT (delegates to backend)."
  (gp--pipelines-match-commit (git-platform-backend) pipelines commit))

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
         (uuid (alist-get 'uuid pipeline)))
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
open since 2020) -- only the web UI can.  So the default action opens
the step's pipeline page in the browser, where one click runs it
in place.  Spawning a NEW pipeline run (which re-executes everything
up to the gate) stays available as an explicit choice.  Only offered
on a manual step that is still waiting."
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
    (pcase (car (read-multiple-choice
                 (format "Run manual step %S (API can't resume it in place):"
                         name)
                 '((?b "browser"
                       "open the pipeline in the web UI and run the step there, in place")
                   (?n "new run"
                       "trigger a NEW pipeline run of this definition (re-runs earlier steps)")
                   (?q "quit" "do nothing"))))
      (?b (browse-url (gp-pipeline-web-url full-name pipeline step)))
      (?n (condition-case e
              (progn
                (gp-pipeline-run-manual-step full-name branch pipeline step)
                (message "Triggered a new pipeline run for %S" name)
                (when (fboundp 'gp-detail-refresh) (gp-detail-refresh)))
            (error (message "Could not run manual step: %s"
                            (error-message-string e))))))))

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
          (gp-pipeline-step-rerun full-name (alist-get 'uuid pipeline) step)
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

(defun gp-pipeline-log--render (text running)
  "Replace the log buffer's contents with TEXT; note RUNNING state."
  (let ((inhibit-read-only t)
        (at-end (eobp)))
    (erase-buffer)
    (insert (or text ""))
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
         (full-name (gp-pr-full-name gp--pr))
         (pipeline-uuid (alist-get 'uuid pipeline))
         (step-uuid (alist-get 'uuid step))
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
