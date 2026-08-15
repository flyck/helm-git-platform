;;; git-platform-mock-test.el --- Tests for the SQLite demo backend -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercises the demo backend end-to-end against a throwaway SQLite
;; database: seeding, list/detail reads, the comment/review/draft write
;; paths the video demos, and the simulated pipelines.  No network.

;;; Code:

(require 'ert)
(require 'git-platform-mock)
(require 'gp-pipeline)

(defmacro gp-mock-test--with (&rest body)
  "Run BODY against a fresh, seeded, temporary mock database."
  (declare (indent 0))
  `(let* ((gp-mock-test--dir (make-temp-file "gp-mock-test" t))
          (git-platform-mock-db-file
           (expand-file-name "demo.sqlite" gp-mock-test--dir))
          (git-platform-current-backend (git-platform-mock))
          (gp-mock--db nil)
          (gp-mock--epoch (float-time))
          (gp-mock--p101-stopped nil)
          (gp-mock--prod-run-at nil))
     (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
     (unwind-protect
         (progn ,@body)
       (when gp-mock--db (ignore-errors (sqlite-close gp-mock--db)))
       (delete-directory gp-mock-test--dir t))))

(ert-deftest gp-mock-test-seed-and-lists ()
  "The seed produces my PRs, my draft, and two PRs to review."
  (gp-mock-test--with
    (let* ((prs (gp-workspace-pull-requests nil "OPEN"))
           (ids (sort (mapcar (lambda (p) (alist-get 'id p)) prs) #'<)))
      (should (equal ids '(101 102 103 104)))
      (let ((cat (gp-categorize-pull-requests prs (gp-user-uuid))))
        (should (equal (mapcar (lambda (p) (alist-get 'id p))
                               (plist-get cat :mine))
                       '(101)))
        (should (equal (mapcar (lambda (p) (alist-get 'id p))
                               (plist-get cat :drafts))
                       '(102)))))
    ;; reviewing = others' PRs where I'm a reviewer, newest activity first
    (should (equal (mapcar (lambda (p) (alist-get 'id p))
                           (gp-reviewing-pull-requests nil nil '("OPEN")))
                   '(104 103)))
    ;; shape accessors (inherited from the bitbucket backend) work as-is
    (let ((pr (gp-pull-request "acme/webshop" 101)))
      (should (equal (gp-pr-full-name pr) "acme/webshop"))
      (should (equal (gp-pr-source-branch pr) "feature/gift-cards"))
      (should-not (gp-pr-draft-p pr))
      (should (equal (gp-pr-review-tally pr)
                     '(:approved 1 :changes 0 :pending 1))))
    (should (gp-pr-draft-p (gp-pull-request "acme/webshop" 102)))))

(ert-deftest gp-mock-test-comment-write-paths ()
  "Create/edit/resolve/reopen/delete all persist to the database."
  (gp-mock-test--with
    (let* ((count (length (gp-pull-request-comments "acme/webshop" 101)))
           (created (gp-create-comment "acme/webshop" 101
                                       "Will do — assertion added." nil 1002))
           (cid (alist-get 'id created)))
      ;; reply landed, threaded under its parent, authored by me
      (should (equal (let-alist created .parent.id) 1002))
      (should (gp-comment-own-p created (gp-user-uuid)))
      (let ((all (gp-pull-request-comments "acme/webshop" 101)))
        (should (= (length all) (1+ count)))
        (should (member cid (mapcar (lambda (c) (alist-get 'id c)) all))))
      ;; the PR's comment_count follows
      (should (= (alist-get 'comment_count (gp-pull-request "acme/webshop" 101))
                 (1+ count)))
      ;; edit
      (let ((edited (gp-edit-comment "acme/webshop" 101 cid "Amended.")))
        (should (equal (let-alist edited .content.raw) "Amended."))
        (should (let-alist edited .updated_on)))
      ;; resolve / reopen
      (gp-resolve-comment "acme/webshop" 101 1005)
      (should (gp-comment-resolved-p
               (car (cl-member 1005 (gp-pull-request-comments "acme/webshop" 101)
                               :key (lambda (c) (alist-get 'id c))))))
      (gp-reopen-comment "acme/webshop" 101 1005)
      (should-not (gp-comment-resolved-p
                   (car (cl-member 1005 (gp-pull-request-comments "acme/webshop" 101)
                                   :key (lambda (c) (alist-get 'id c))))))
      ;; delete hides the comment from the listing
      (gp-delete-comment "acme/webshop" 101 cid)
      (should-not (member cid (mapcar (lambda (c) (alist-get 'id c))
                                      (gp-pull-request-comments "acme/webshop" 101)))))))

(ert-deftest gp-mock-test-review-and-draft ()
  "Approve / request-changes / draft-toggle round-trip through SQLite."
  (gp-mock-test--with
    (let ((me (gp-user-uuid)))
      (should-not (gp-pr-my-review-state (gp-pull-request "acme/billing" 103) me))
      (gp-approve-pr "acme/billing" 103)
      (let ((pr (gp-pull-request "acme/billing" 103)))
        (should (eq (gp-pr-my-review-state pr me) 'approved))
        (should (= (plist-get (gp-pr-review-tally pr) :approved) 2)))
      (gp-approve-pr "acme/billing" 103 'unapprove)
      (should-not (gp-pr-my-review-state (gp-pull-request "acme/billing" 103) me))
      (gp-request-changes-pr "acme/billing" 103)
      (should (eq (gp-pr-my-review-state (gp-pull-request "acme/billing" 103) me)
                  'changes)))
    (should-not (gp-pr-draft-p (gp-set-pull-request-draft "acme/webshop" 102 nil)))
    (should (gp-pr-draft-p (gp-set-pull-request-draft "acme/webshop" 102 t)))))

(ert-deftest gp-mock-test-diff-stats-and-outdated ()
  "Stats derive from the stored diff; the seeded outdated comment is flagged."
  (gp-mock-test--with
    (let ((stats (gp-pull-request-stats "acme/webshop" 101)))
      (should (= (plist-get stats :files) 3))
      (should (= (plist-get stats :added) 45))
      (should (= (plist-get stats :removed) 3))
      (should (= (plist-get stats :commits) 4))
      (should (equal (plist-get (car (plist-get stats :file-list)) :status)
                     "modified")))
    (clrhash gp--comment-outdated-cache)
    (let* ((diff-by-file (gp-split-diff-by-file
                          (gp-pull-request-diff "acme/billing" 103)))
           (comments (gp-pull-request-comments "acme/billing" 103))
           (c1007 (car (cl-member 1007 comments
                                  :key (lambda (c) (alist-get 'id c))))))
      ;; anchored to line 99, which the current diff no longer touches
      (should (gp-comment-outdated-p c1007 diff-by-file)))
    (clrhash gp--comment-outdated-cache)
    (let* ((diff-by-file (gp-split-diff-by-file
                          (gp-pull-request-diff "acme/webshop" 101)))
           (comments (gp-pull-request-comments "acme/webshop" 101))
           (c1002 (car (cl-member 1002 comments
                                  :key (lambda (c) (alist-get 'id c))))))
      ;; anchored to a line the diff still introduces
      (should-not (gp-comment-outdated-p c1002 diff-by-file)))))

(ert-deftest gp-mock-test-pipeline-simulation ()
  "#101's pipeline advances with the clock; stop halts it."
  (gp-mock-test--with
    ;; t=0: build phase, pipeline running, first step executing
    (let ((steps (gp-pipeline-steps "acme/webshop" "{mock-pipe-101}")))
      (should (gp-pipeline-step-running-p (car steps))))
    ;; t=50: unit tests run with a live started_on for the elapsed display
    (setq gp-mock--epoch (- (float-time) 50))
    (let ((steps (gp-pipeline-steps "acme/webshop" "{mock-pipe-101}")))
      (should (gp-pipeline-step-running-p (nth 1 steps)))
      (should (alist-get 'started_on (nth 1 steps))))
    ;; t=120: everything finished successfully
    (setq gp-mock--epoch (- (float-time) 120))
    (let ((pipes (gp-pipelines-for-branch "acme/webshop" "feature/gift-cards")))
      (should (= (length pipes) 2))
      (should (gp-pipeline-finished-p (car pipes)))
      (should (equal (gp-pipeline-result (car pipes)) "SUCCESSFUL")))
    ;; the PR-level fetch partitions into current run + prior-commit run
    (let ((data (gp-pipeline-fetch-for-pr (gp-pull-request "acme/webshop" 101))))
      (should (= (length (plist-get data :current)) 1))
      (should (equal (cdar (plist-get data :recent))
                     "Add gift card model and migrations")))
    ;; stopping is remembered
    (setq gp-mock--epoch (- (float-time) 50))
    (gp-pipeline-stop "acme/webshop" "{mock-pipe-101}")
    (should (equal (gp-pipeline-result
                    (car (gp-pipelines-for-branch "acme/webshop"
                                                  "feature/gift-cards")))
                   "STOPPED"))))

(ert-deftest gp-mock-test-manual-gate ()
  "#104 pauses at an open manual gate until the step is run."
  (gp-mock-test--with
    (let* ((pipe (car (gp-pipelines-for-branch "acme/infra" "infra/eks-1-30")))
           (steps (gp-pipeline-steps "acme/infra" "{mock-pipe-104}"))
           (prod (nth 2 steps)))
      (should-not (gp-pipeline-finished-p pipe))
      (should (gp-pipeline-step-runnable-manual-p prod))
      (should (gp-pipeline--manual-gate-open-p pipe steps))
      ;; running the manual step closes the gate and starts the apply
      (gp-pipeline-run-manual-step "acme/infra" "infra/eks-1-30" pipe prod)
      (let ((steps (gp-pipeline-steps "acme/infra" "{mock-pipe-104}")))
        (should (gp-pipeline-step-running-p (nth 2 steps)))
        (should-not (gp-pipeline--manual-gate-open-p
                     (car (gp-pipelines-for-branch "acme/infra"
                                                   "infra/eks-1-30"))
                     steps)))
      ;; ...and 20s later the run is green
      (setq gp-mock--prod-run-at (- (float-time) 25))
      (should (equal (gp-pipeline-result
                      (car (gp-pipelines-for-branch "acme/infra"
                                                    "infra/eks-1-30")))
                     "SUCCESSFUL")))))

(ert-deftest gp-mock-test-commit-statuses-and-create-pr ()
  "Helm's status bubbles and PR creation read/write the mock."
  (gp-mock-test--with
    (should (equal (gp-mock--commit-statuses "c1d2e3f4a5b6") '("FAILED")))
    (should (equal (gp-commit-build-states "acme/infra" "0ab1c2d3e4f5")
                   '("INPROGRESS")))
    (let ((pr (gp-create-pull-request "acme/webshop" "feature/wishlist" "main"
                                      "Add wishlists" "First cut." nil nil
                                      '("{mock-alice}"))))
      (should (equal (alist-get 'id pr) 106))
      (should (equal (gp-pr-source-branch pr) "feature/wishlist"))
      (should (= (plist-get (gp-pr-review-tally pr) :pending) 1))
      (should (equal (alist-get 'id (gp-open-pr-for-branch
                                     "acme/webshop" "feature/wishlist"))
                     106)))))

(ert-deftest gp-mock-test-pull-request-commits ()
  "The mock serves a PR's commits, newest first, headed by its real head commit."
  (gp-mock-test--with
    (let ((commits nil))
      (gp-pull-request-commits-async
       "acme/webshop" 101 (lambda (cs) (setq commits cs)))
      (should commits)
      ;; the newest entry is the PR's actual head commit, so the commits and
      ;; pipelines sections agree on which commit is current
      (let ((head (gp-pr-source-commit (gp-pull-request "acme/webshop" 101))))
        (should (equal (plist-get (car commits) :hash) head)))
      ;; every entry carries the full normalised shape
      (dolist (c commits)
        (should (plist-get c :hash))
        (should (plist-get c :summary))
        (should (plist-get c :author)))
      ;; the cap keeps the newest
      (let ((capped nil))
        (gp-pull-request-commits-async
         "acme/webshop" 101 (lambda (cs) (setq capped cs)) 1)
        (should (= (length capped) 1))
        (should (equal (plist-get (car capped) :hash)
                       (plist-get (car commits) :hash)))))))

(provide 'git-platform-mock-test)
;;; git-platform-mock-test.el ends here
