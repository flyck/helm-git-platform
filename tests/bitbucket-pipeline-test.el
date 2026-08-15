;;; bitbucket-pipeline-test.el --- Tests for the Pipelines API layer -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers the pipeline endpoint functions (against the mock) and the pure
;; shape/sort helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'bitbucket-api)
(require 'bitbucket-mock)

;;;; Pure helpers --------------------------------------------------------------

(ert-deftest bitbucket-test-pipeline-state-and-result ()
  (let ((running '((state (name . "IN_PROGRESS") (stage (name . "RUNNING")))))
        (done '((state (name . "COMPLETED") (result (name . "SUCCESSFUL"))))))
    (should (equal (bitbucket-pipeline-state running) "IN_PROGRESS"))
    (should (equal (bitbucket-pipeline-result running) "RUNNING")) ;; stage fallback
    (should (equal (bitbucket-pipeline-state done) "COMPLETED"))
    (should (equal (bitbucket-pipeline-result done) "SUCCESSFUL"))
    (should-not (bitbucket-pipeline-finished-p running))
    (should (bitbucket-pipeline-finished-p done))))

(ert-deftest bitbucket-test-pipeline-step-helpers ()
  (let ((running '((state (name . "IN_PROGRESS"))))
        (done '((state (name . "COMPLETED") (result (name . "FAILED")))))
        ;; the REAL API trigger types, plus a waiting manual gate
        (manual-waiting '((state (name . "PENDING") (stage (name . "PAUSED")))
                          (trigger (type . "pipeline_step_trigger_manual"))))
        (manual-done '((state (name . "COMPLETED") (result (name . "SUCCESSFUL")))
                       (trigger (type . "pipeline_step_trigger_manual"))))
        (auto '((state (name . "PENDING"))
                (trigger (type . "pipeline_step_trigger_automatic")))))
    (should (bitbucket-pipeline-step-running-p running))
    (should-not (bitbucket-pipeline-step-running-p done))
    (should (equal (bitbucket-pipeline-step-result done) "FAILED"))
    ;; the real "pipeline_step_trigger_manual" type is detected
    (should (bitbucket-pipeline-step-manual-p manual-waiting))
    (should (bitbucket-pipeline-step-manual-p manual-done))
    (should-not (bitbucket-pipeline-step-manual-p auto))
    ;; plain "manual" still works (older shape)
    (should (bitbucket-pipeline-step-manual-p '((trigger (type . "manual")))))
    ;; "runnable" = manual AND waiting; a done manual step is not runnable
    (should (bitbucket-pipeline-step-runnable-manual-p manual-waiting))
    (should-not (bitbucket-pipeline-step-runnable-manual-p manual-done))
    (should-not (bitbucket-pipeline-step-runnable-manual-p auto))))

(ert-deftest bitbucket-test-pipelines-sort-most-steps-first ()
  "Sort by step count desc, ties broken by newest created_on."
  (let* ((p1 '((uuid . "a") (created_on . "2026-06-10T00:00:00Z")))
         (p2 '((uuid . "b") (created_on . "2026-06-12T00:00:00Z")))
         (p3 '((uuid . "c") (created_on . "2026-06-11T00:00:00Z")))
         (counts (make-hash-table :test 'equal)))
    (puthash "a" 5 counts)              ;; most steps -> top
    (puthash "b" 2 counts)              ;; tie with c on count
    (puthash "c" 2 counts)              ;; tie; newer than... b is newer
    (let ((sorted (bitbucket-pipelines-sort (list p2 p3 p1) counts)))
      (should (equal (mapcar (lambda (p) (alist-get 'uuid p)) sorted)
                     ;; a (5) first; then the two 2-step ones newest-first: b,c
                     '("a" "b" "c"))))))

(ert-deftest bitbucket-test-pipelines-sort-unknown-count-zero ()
  (let* ((p1 '((uuid . "a") (created_on . "2026-01-01T00:00:00Z")))
         (p2 '((uuid . "b") (created_on . "2026-01-02T00:00:00Z")))
         (counts (make-hash-table :test 'equal)))
    (puthash "a" 1 counts)              ;; b unknown -> 0, so a wins
    (should (equal (mapcar (lambda (p) (alist-get 'uuid p))
                           (bitbucket-pipelines-sort (list p2 p1) counts))
                   '("a" "b")))))

(ert-deftest bitbucket-test-pipeline-trigger-builds-target ()
  "Default trigger sends a branch ref target with no selector."
  (bitbucket-mock-with-service
    (bitbucket-pipeline-trigger "acme/x" "feature/foo")
    (let* ((call (cl-find "POST" bitbucket-mock-calls :key #'car :test #'equal))
           (data (nth 3 call)))
      (should (string-suffix-p "/pipelines" (nth 1 call)))
      (should (equal (let-alist data .target.type) "pipeline_ref_target"))
      (should (equal (let-alist data .target.ref_name) "feature/foo"))
      (should (null (let-alist data .target.selector))))))

(ert-deftest bitbucket-test-pipeline-trigger-with-selector ()
  (bitbucket-mock-with-service
    (bitbucket-pipeline-trigger "acme/x" "main" '("custom" . "deploy"))
    (let* ((call (cl-find "POST" bitbucket-mock-calls :key #'car :test #'equal))
           (data (nth 3 call)))
      (should (equal (let-alist data .target.selector.type) "custom"))
      (should (equal (let-alist data .target.selector.pattern) "deploy")))))

(ert-deftest bitbucket-test-pipeline-stop-posts-to-endpoint ()
  (bitbucket-mock-with-service
    (bitbucket-pipeline-stop "acme/x" "{pipe}")
    (let ((call (cl-find "POST" bitbucket-mock-calls :key #'car :test #'equal)))
      (should call)
      (should (string-suffix-p "/pipelines/{pipe}/stopPipeline" (nth 1 call))))))

(ert-deftest bitbucket-test-pipeline-run-manual-rejects-non-manual ()
  (should-error
   (bitbucket-pipeline-run-manual-step
    "acme/x" "main" '((target)) '((state (name . "PENDING")) (trigger (type . "automatic"))))
   :type 'user-error))

(ert-deftest bitbucket-test-pipeline-run-manual-triggers-with-selector ()
  "Bitbucket has no per-step run endpoint, so a waiting manual step is
advanced by re-triggering its pipeline with the custom selector."
  (bitbucket-mock-with-service
    (bitbucket-pipeline-run-manual-step
     "acme/x" "main"
     '((uuid . "{pipe}") (target (selector (pattern . "deploy"))))
     '((uuid . "{step-3}")
       (state (name . "PENDING") (stage (name . "PAUSED")))
       (trigger (type . "pipeline_step_trigger_manual"))))
    (let* ((call (cl-find "POST" bitbucket-mock-calls :key #'car :test #'equal))
           (data (nth 3 call)))
      (should call)
      (should (string-suffix-p "/pipelines" (nth 1 call)))
      (should (equal (let-alist data .target.selector.type) "custom"))
      (should (equal (let-alist data .target.selector.pattern) "deploy")))))

(ert-deftest bitbucket-test-pipelines-for-branch-and-steps ()
  (bitbucket-mock-with-service
    ;; no commit filter -> all three fixture pipelines
    (let ((pipes (bitbucket-pipelines-for-branch "acme/x" "feature/widget-toggle")))
      (should (= (length pipes) 3))
      (let ((steps (bitbucket-pipeline-steps "acme/x" (alist-get 'uuid (car pipes)))))
        (should (= (length steps) 3))
        (should (cl-some #'bitbucket-pipeline-step-manual-p steps))))))

(ert-deftest bitbucket-test-pipelines-filtered-to-commit ()
  "Only pipelines for the given head commit are returned."
  (bitbucket-mock-with-service
    (let ((pipes (bitbucket-pipelines-for-branch
                  "acme/x" "feature/widget-toggle" 20 "deadbeefcafe0001")))
      ;; two of the three fixture pipelines are on this commit
      (should (= (length pipes) 2))
      (should (cl-every (lambda (p)
                          (equal (bitbucket-pipeline-commit p) "deadbeefcafe0001"))
                        pipes)))))

(ert-deftest bitbucket-test-pipelines-match-commit-prefix ()
  "Commit matching works in either direction (short vs full hash)."
  (let ((pipes '(((target (commit (hash . "abcdef1234567890"))))
                 ((target (commit (hash . "0000000000000000")))))))
    ;; full query hash, short hash on the pipeline
    (should (= 1 (length (bitbucket-pipelines-match-commit
                          '(((target (commit (hash . "abcdef12")))))
                          "abcdef1234567890"))))
    ;; nil commit -> unchanged
    (should (= 2 (length (bitbucket-pipelines-match-commit pipes nil))))))

;;;; PR commits (normalisation) ------------------------------------------------

(ert-deftest bitbucket-test-commit-entry-normalises-fields ()
  "A PR commit is flattened to (:hash :summary :author :date)."
  (let ((entry (bitbucket--commit-entry
                '((hash . "87c8054110c84d42edc3a4e89184ffd1a15d3a8d")
                  (date . "2026-07-15T17:28:26+00:00")
                  (author (raw . "Felix Brilej <f@example.com>")
                          (user (display_name . "Felix Brilej")))
                  (message . "fix the thing\n\nlonger body ignored")))))
    (should (equal (plist-get entry :hash)
                   "87c8054110c84d42edc3a4e89184ffd1a15d3a8d"))
    ;; only the first line of the message
    (should (equal (plist-get entry :summary) "fix the thing"))
    (should (equal (plist-get entry :author) "Felix Brilej"))
    (should (equal (plist-get entry :date) "2026-07-15T17:28:26+00:00"))))

(ert-deftest bitbucket-test-commit-entry-falls-back-to-raw-author ()
  "Commits from non-Bitbucket accounts have no `author.user' to read.
Without the raw fallback those rows would render with no author."
  (let ((entry (bitbucket--commit-entry
                '((hash . "abc123")
                  (author (raw . "Outside Contributor <o@example.com>"))
                  (message . "drive-by fix")))))
    (should (equal (plist-get entry :author) "Outside Contributor"))))

(ert-deftest bitbucket-test-commit-entry-survives-missing-author ()
  "A commit with no author at all still yields a usable plist."
  (let ((entry (bitbucket--commit-entry '((hash . "abc123") (message . "x")))))
    (should (equal (plist-get entry :hash) "abc123"))
    (should (null (plist-get entry :author)))))

(provide 'bitbucket-pipeline-test)
;;; bitbucket-pipeline-test.el ends here
