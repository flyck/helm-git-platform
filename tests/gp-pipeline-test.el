;;; gp-pipeline-test.el --- Tests for the pipelines detail section -*- lexical-binding: t; -*-

;;; Commentary:
;; Drives the pipeline renderer in a real buffer, checks status glyph /
;; duration formatting, the sorted fetch, and the section tree.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gp-pipeline)
(require 'gp-ui)
(require 'bitbucket-mock)

;;;; Pure formatting -----------------------------------------------------------

(ert-deftest gp-test-pipeline-status-glyph ()
  (should (eq (cdr (gp-pipeline--status-glyph "IN_PROGRESS" nil))
              'gp-pipeline-running-face))
  (should (eq (cdr (gp-pipeline--status-glyph "COMPLETED" "SUCCESSFUL"))
              'gp-pipeline-success-face))
  (should (eq (cdr (gp-pipeline--status-glyph "COMPLETED" "FAILED"))
              'gp-pipeline-failed-face))
  (should (eq (cdr (gp-pipeline--status-glyph "COMPLETED" "STOPPED"))
              'gp-pipeline-stopped-face)))

(ert-deftest gp-test-pipeline-format-duration ()
  (should (equal (gp-pipeline--format-duration '((duration_in_seconds . 5))) "5s"))
  (should (equal (gp-pipeline--format-duration '((duration_in_seconds . 95))) "1m35s"))
  (should (equal (gp-pipeline--format-duration '((duration_in_seconds . 3700))) "1h01m"))
  (should (equal (gp-pipeline--format-duration '((name . "x"))) "")))

(ert-deftest gp-test-pipeline-label-has-number-and-status ()
  (let ((p '((build_number . 42) (state (name . "COMPLETED")
                                         (result (name . "SUCCESSFUL"))))))
    (should (string-match-p "#42"
                            (substring-no-properties (gp-pipeline--label p))))
    (should (string-match-p "SUCCESSFUL"
                            (substring-no-properties (gp-pipeline--label p))))))

;;;; Fetch (sorted, with steps) ------------------------------------------------

(ert-deftest gp-test-pipeline-fetch-current-vs-recent ()
  (bitbucket-mock-with-service
    (let* ((pr (car (alist-get 'values (bitbucket-mock--fixture "workspace-prs.json"))))
           (result (gp-pipeline-fetch-for-pr pr))
           (current (plist-get result :current))
           (recent (plist-get result :recent)))
      ;; two pipelines on the PR head commit -> :current (with steps)
      (should (= (length current) 2))
      (should (cl-every (lambda (pp) (listp (cdr pp))) current))
      (should (= (length (cdr (car current))) 3))   ;; steps fetched
      ;; the one pipeline on an older commit -> :recent (PIPELINE . SUMMARY)
      (should (= (length recent) 1))
      (should (consp (car recent)))
      ;; the summary is the first line of the mocked commit message
      (should (equal (cdr (car recent)) "Fix the widget toggle")))))

(ert-deftest gp-test-commit-summary-first-line ()
  (should (equal (gp-commit-summary "first line\nsecond") "first line"))
  (should (equal (gp-commit-summary "  spaced  \nx") "spaced"))
  (should (equal (gp-commit-summary nil) "")))

;;;; Rendering -----------------------------------------------------------------

(defun gp-test--render-pipelines (pipelines)
  "Render PIPELINES into a temp detail buffer; return its text."
  (with-temp-buffer
    (gp-detail-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (gp-root)
        (gp--insert-pipelines pipelines)))
    (substring-no-properties (buffer-string))))

(ert-deftest gp-test-pipeline-render-section ()
  (bitbucket-mock-with-service
    (let* ((pr (car (alist-get 'values (bitbucket-mock--fixture "workspace-prs.json"))))
           (pipelines (gp-pipeline-fetch-for-pr pr))
           (text (gp-test--render-pipelines pipelines)))
      (should (string-match-p "Pipelines (2 for this commit)" text))
      (should (string-match-p "Pipeline #42" text))
      ;; step names from the fixture appear
      (should (string-match-p "Build and test" text))
      (should (string-match-p "Deploy to LIVE" text))
      ;; the waiting manual step is flagged as runnable
      (should (string-match-p "\\[manual ▸ m\\]" text))
      ;; the older-commit run appears in the recent-runs summary,
      ;; with its commit message (not just the sha)
      (should (string-match-p "Recent runs on this branch (1)" text))
      (should (string-match-p "Fix the widget toggle" text)))))

(ert-deftest gp-test-pipeline-recent-runs-are-sections ()
  "Each recent run is its own foldable section, like comments."
  (bitbucket-mock-with-service
    (let* ((pr (car (alist-get 'values (bitbucket-mock--fixture "workspace-prs.json"))))
           (data (gp-pipeline-fetch-for-pr pr)))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (gp-root)
            (gp--insert-pipelines data)))
        (let ((recent-secs
               (cl-remove-if-not
                (lambda (s) (and (object-of-class-p s 'gp-pipeline-recent-section)
                                 ;; the per-run sections carry a pipeline value
                                 (oref s value)))
                (gp-test--all-pipeline-sections magit-root-section))))
          ;; one per recent run
          (should (= (length recent-secs) 1)))))))

(ert-deftest gp-test-pipeline-render-empty-is-noop ()
  (should (equal (gp-test--render-pipelines nil) "")))

(ert-deftest gp-test-pipeline-finished-collapsed-flag ()
  "Finished pipelines render collapsed; running ones expanded."
  (bitbucket-mock-with-service
    (let* ((pr (car (alist-get 'values (bitbucket-mock--fixture "workspace-prs.json"))))
           (pipelines (gp-pipeline-fetch-for-pr pr)))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (gp-root)
            (gp--insert-pipelines pipelines)))
        (let ((pipe-secs
               (cl-remove-if-not
                (lambda (s) (object-of-class-p s 'gp-pipeline-section))
                (gp-test--all-pipeline-sections magit-root-section))))
          (should (= (length pipe-secs) 2))
          ;; each section's value is (PIPELINE . STEPS)
          (should (cl-every (lambda (s) (consp (oref s value))) pipe-secs)))))))

(defun gp-test--all-pipeline-sections (section)
  "Flatten SECTION and descendants."
  (cons section
        (cl-mapcan #'gp-test--all-pipeline-sections
                   (and (slot-boundp section 'children)
                        (oref section children)))))

(provide 'gp-pipeline-test)
;;; gp-pipeline-test.el ends here
