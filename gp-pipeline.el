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

;;;; Status formatting (pure) --------------------------------------------------

(defun gp-pipeline--status-glyph (state result &optional step)
  "Return (GLYPH . FACE) for a pipeline/step STATE and RESULT string.
A pipeline paused at an open manual gate (stage PAUSED) gets its own
⏸ glyph instead of the running one.  With STEP non-nil an in-progress
entry renders as ⟳, so a step actually executing is distinguishable
from its enclosing running pipeline's ▶."
  (cond
   ((equal result "PAUSED") (cons "⏸" 'gp-pipeline-paused-face))
   ((equal state "IN_PROGRESS")
    (cons (if step "⟳" "▶") 'gp-pipeline-running-face))
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
         (manual (gp-pipeline-step-manual-p step)))
    (magit-insert-section (gp-pipeline-step-section step)
      (magit-insert-heading
        (concat "    "
                (propertize (car g) 'face (cdr g))
                " "
                (propertize name 'face 'default)
                 (cond
                  (runnable (propertize "  [manual ▸ T]" 'face 'gp-pipeline-running-face))
                  (manual (propertize "  [manual]" 'face 'shadow)))
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
  "Start the waiting manual step at point.
Bitbucket has no public \"advance this step\" API, so this triggers a
fresh pipeline run targeting the step's pipeline definition (with a
custom selector when the pipeline has one).  Only offered on a
manual step that is still waiting."
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
    (when (yes-or-no-p
           (format "Bitbucket can't resume a step in place; trigger a NEW pipeline run for manual step %S? " name))
      (condition-case e
          (progn
            (gp-pipeline-run-manual-step full-name branch pipeline step)
            (message "Triggered a new pipeline run for %S" name)
            (when (fboundp 'gp-detail-refresh) (gp-detail-refresh)))
        (error (message "Could not run manual step: %s"
                        (error-message-string e)))))))

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
               (format "*PR pipeline #%s log: %s*"
                       (or (gp-pipeline-number pipeline) "?")
                       (or (alist-get 'name step) "step")))))
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
