;;; gp-checkout-test.el --- Tests for the checkout service -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercises the pure command-plan builders and the executor with git
;; faked, so no real repository is touched.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gp-checkout)

(ert-deftest gp-test-plan-clean-tree ()
  "A clean tree: fetch, checkout, pull -- no stash."
  (let ((gp-checkout-remote "origin"))
    (should (equal (gp-checkout--plan "feature" nil "main")
                   '(("fetch" "origin" "feature")
                     ("checkout" "feature")
                     ("pull" "--ff-only" "origin" "feature"))))))

(ert-deftest gp-test-plan-fetches-base ()
  "With a BASE branch, its remote ref is fetched too (for accurate diffs)."
  (let ((gp-checkout-remote "origin"))
    (should (equal (gp-checkout--plan "feature" nil "main" "develop")
                   '(("fetch" "origin" "feature")
                     ("fetch" "origin" "develop")
                     ("checkout" "feature")
                     ("pull" "--ff-only" "origin" "feature"))))
    ;; base == branch: no redundant fetch
    (should (= (length (gp-checkout--plan "main" nil "x" "main")) 3))))

(ert-deftest gp-test-plan-dirty-tree-stashes-first ()
  "A dirty tree prepends a named stash referencing the current branch."
  (let ((gp-checkout-remote "origin")
        (gp-checkout-stash-prefix "gp-auto"))
    (let ((plan (gp-checkout--plan "feature" t "main")))
      (should (equal (car plan)
                     '("stash" "push" "--include-untracked"
                       "-m" "gp-auto: WIP on main")))
      (should (= (length plan) 4)))))

(ert-deftest gp-test-dirty-count-counts-porcelain-lines ()
  "One line per path, whatever the status codes on it -- a staged AND
modified file is still one file to the user."
  (cl-letf (((symbol-function 'gp-checkout--git)
             (lambda (_dir &rest _args)
               (cons 0 " M lib/a.el\nMM lib/b.el\n?? scratch.txt\n"))))
    (should (= (gp-checkout-dirty-count "/repo") 3))))

(ert-deftest gp-test-dirty-count-clean-tree-is-zero ()
  "A clean tree reports zero, not nil -- callers do arithmetic on it."
  (cl-letf (((symbol-function 'gp-checkout--git)
             (lambda (_dir &rest _args) (cons 0 ""))))
    (should (= (gp-checkout-dirty-count "/repo") 0))))

(ert-deftest gp-test-dirty-count-unreadable-repo-is-zero ()
  "Git refusing to answer is not evidence of unsaved work: a warning
built on a failed command would fire on every non-repo directory."
  (cl-letf (((symbol-function 'gp-checkout--git)
             (lambda (_dir &rest _args)
               (cons 128 "fatal: not a git repository"))))
    (should (= (gp-checkout-dirty-count "/nope") 0))))

(ert-deftest gp-test-clone-command-requires-base ()
  (let ((gp-checkout-clone-base nil))
    (should-error (gp-checkout--clone-command "ws/slug" "/tmp/slug")
                  :type 'user-error))
  (let ((gp-checkout-clone-base "git@bitbucket.org:"))
    (should (equal (gp-checkout--clone-command "ws/slug" "/tmp/slug")
                   '("clone" "git@bitbucket.org:ws/slug.git" "/tmp/slug")))))

(ert-deftest gp-test-worktree-for-branch-finds-sibling ()
  "A branch held by another worktree is reported with its path."
  (cl-letf (((symbol-function 'gp-checkout--git)
             (lambda (_dir &rest _args)
               (cons 0 (concat
                        "worktree /repo\n"
                        "HEAD abc\n"
                        "branch refs/heads/main\n\n"
                        "worktree /repo/.wt/labels\n"
                        "HEAD def\n"
                        "branch refs/heads/feat/pr-labels\n")))))
    (should (equal (gp-checkout-worktree-for-branch "/repo" "feat/pr-labels")
                   "/repo/.wt/labels"))
    ;; a branch nobody has checked out
    (should-not (gp-checkout-worktree-for-branch "/repo" "other"))
    ;; the branch DIR itself holds is not a "different" worktree
    (should-not (gp-checkout-worktree-for-branch "/repo" "main"))))

(ert-deftest gp-test-worktree-detached-head-ignored ()
  "A detached worktree has no branch line and must not match."
  (cl-letf (((symbol-function 'gp-checkout--git)
             (lambda (_dir &rest _args)
               (cons 0 "worktree /repo\nHEAD abc\ndetached\n"))))
    (should-not (gp-checkout-worktree-for-branch "/repo" "main"))))

(defmacro gp-test-with-fake-git (script &rest body)
  "Run BODY with `gp-checkout--git' replaced by SCRIPT.
SCRIPT is a function (dir &rest args) returning (CODE . OUTPUT);
all invocations are recorded in the dynamically-bound list
`git-calls' (newest last)."
  (declare (indent 1) (debug t))
  `(let ((git-calls '()))
     (cl-letf (((symbol-function 'gp-checkout--git)
                (lambda (dir &rest args)
                  (setq git-calls (append git-calls (list (cons dir args))))
                  (funcall ,script dir args))))
       ,@body)))

(ert-deftest gp-test-run-clean-executes-three-steps ()
  (gp-test-with-fake-git
      (lambda (_dir args)
        (cond ((equal (car args) "status") '(0 . ""))         ;; clean
              ((equal (car args) "rev-parse") '(0 . "main"))
              (t '(0 . "ok"))))
    (let ((res (gp-checkout-run "/repo" "feature")))
      (should (plist-get res :ok))
      (should-not (plist-get res :stashed))
      ;; worktree-list + status + rev-parse(current branch) + fetch + checkout
      ;; + [divergence probe: rev-parse x2 + one merge-base --is-ancestor,
      ;;    short-circuiting before the second since the first already
      ;;    disproves divergence] + pull
      (should (= (length git-calls) 9))
      (should-not (cl-find "stash" git-calls
                           :key (lambda (c) (cadr c)) :test #'equal)))))

(ert-deftest gp-test-run-dirty-stashes ()
  (gp-test-with-fake-git
      (lambda (_dir args)
        (cond ((equal (car args) "status") '(0 . " M file.txt")) ;; dirty
              ((equal (car args) "rev-parse") '(0 . "main"))
              (t '(0 . "ok"))))
    (let ((res (gp-checkout-run "/repo" "feature")))
      (should (plist-get res :ok))
      (should (plist-get res :stashed))
      (should (cl-find "stash" git-calls
                       :key (lambda (c) (cadr c)) :test #'equal)))))

(ert-deftest gp-test-run-stops-on-failure ()
  "If checkout fails, pull is not attempted and :ok is nil."
  (gp-test-with-fake-git
      (lambda (_dir args)
        (cond ((equal (car args) "status") '(0 . ""))
              ((equal (car args) "rev-parse") '(0 . "main"))
              ((equal (car args) "checkout") '(1 . "error: conflict"))
              (t '(0 . "ok"))))
    (let ((res (gp-checkout-run "/repo" "feature")))
      (should-not (plist-get res :ok))
      (should (string-match-p "conflict" (plist-get res :log)))
      ;; pull must never have run
      (should-not (cl-find "pull" git-calls
                           :key (lambda (c) (cadr c)) :test #'equal)))))

(defun gp-test--diverged-fake-git (_dir args)
  "Fake git for a BRANCH that has diverged from origin/BRANCH.
`rev-parse --verify' succeeds for both refs (both exist); every
`merge-base --is-ancestor' fails (neither is an ancestor of the
other, i.e. genuinely diverged, not just behind/ahead)."
  (cond ((equal (car args) "status") '(0 . ""))
        ((equal args '("rev-parse" "--abbrev-ref" "HEAD")) '(0 . "feature"))
        ((equal (car args) "worktree") '(0 . ""))
        ((and (equal (car args) "rev-parse") (equal (nth 1 args) "--verify"))
         '(0 . "deadbeef"))
        ((equal (car args) "merge-base") '(1 . ""))
        (t '(0 . "ok"))))

(ert-deftest gp-test-branch-diverged-p-true-when-neither-is-ancestor ()
  (cl-letf (((symbol-function 'gp-checkout--git)
             (lambda (dir &rest args) (gp-test--diverged-fake-git dir args))))
    (should (gp-checkout-branch-diverged-p "/repo" "feature" "origin"))))

(ert-deftest gp-test-branch-diverged-p-false-when-remote-ref-missing ()
  "Not diverged (just not yet fetched) when the remote ref doesn't exist."
  (cl-letf (((symbol-function 'gp-checkout--git)
             (lambda (_dir &rest args)
               (cond ((equal (nth 1 args) "--verify")
                      (if (equal (nth 2 args) "feature") '(0 . "x") '(1 . "")))
                     (t '(0 . "ok"))))))
    (should-not (gp-checkout-branch-diverged-p "/repo" "feature" "origin"))))

(ert-deftest gp-test-branch-diverged-p-false-when-just-behind ()
  "Not diverged when the local branch IS an ancestor of the remote one
(the ordinary \"just behind\" case `pull --ff-only' handles fine)."
  (cl-letf (((symbol-function 'gp-checkout--git)
             (lambda (_dir &rest args)
               (cond ((equal (nth 1 args) "--verify") '(0 . "x"))
                     ((equal (car args) "merge-base")
                      ;; local is-ancestor-of remote: succeeds (0)
                      (if (equal (nth 3 args) "feature") '(0 . "") '(1 . "")))
                     (t '(0 . "ok"))))))
    (should-not (gp-checkout-branch-diverged-p "/repo" "feature" "origin"))))

(ert-deftest gp-test-run-recovers-diverged-branch-on-confirmation ()
  "Confirming the prompt backs up the local branch and recreates it
fresh from origin, then still finishes with :ok."
  (gp-test-with-fake-git #'gp-test--diverged-fake-git
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'format-time-string) (lambda (&rest _) "20260101000000")))
      (let ((res (gp-checkout-run "/repo" "feature")))
        (should (plist-get res :ok))
        (should (cl-find '("branch" "gp-auto-backup/feature/20260101000000" "feature")
                         git-calls :key #'cdr :test #'equal))
        (should (cl-find '("checkout" "-B" "feature" "origin/feature")
                         git-calls :key #'cdr :test #'equal))
        ;; the plain ff-only pull must never run once recovery took over
        (should-not (cl-find '("pull" "--ff-only" "origin" "feature")
                             git-calls :key #'cdr :test #'equal))))))

(ert-deftest gp-test-run-declines-diverged-branch-recovery ()
  "Declining the prompt fails cleanly instead of running the doomed pull."
  (gp-test-with-fake-git #'gp-test--diverged-fake-git
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (let ((res (gp-checkout-run "/repo" "feature")))
        (should-not (plist-get res :ok))
        (should (string-match-p "diverged" (plist-get res :log)))
        (should-not (cl-find "branch" git-calls :key #'cadr :test #'equal))
        (should-not (cl-find '("pull" "--ff-only" "origin" "feature")
                             git-calls :key #'cdr :test #'equal))))))

(ert-deftest gp-test-run-skips-divergence-check-when-disabled ()
  "With the customvar off, a diverged branch hits the plain (failing)
`pull --ff-only' the same way it always did."
  (let ((gp-checkout-recover-diverged-branch nil))
    (gp-test-with-fake-git
        (lambda (_dir args)
          (cond ((equal (car args) "pull") '(1 . "fatal: not possible to fast-forward"))
                (t (gp-test--diverged-fake-git nil args))))
      (let ((res (gp-checkout-run "/repo" "feature")))
        (should-not (plist-get res :ok))
        (should (string-match-p "fast-forward" (plist-get res :log)))
        (should (cl-find '("pull" "--ff-only" "origin" "feature")
                         git-calls :key #'cdr :test #'equal))))))

(ert-deftest gp-test-pop-stash-guards-foreign-stash ()
  "Popping refuses when the top stash is not one of ours."
  (let ((gp-checkout-stash-prefix "gp-auto"))
    (gp-test-with-fake-git
        (lambda (_dir _args) '(0 . "On main: some manual stash"))
      (should-error (gp-checkout-pop-stash "/repo") :type 'user-error))))

(ert-deftest gp-test-pop-stash-accepts-ours ()
  (let ((gp-checkout-stash-prefix "gp-auto"))
    (gp-test-with-fake-git
        (lambda (_dir args)
          (if (equal args '("stash" "list" "--max-count=1" "--format=%gs"))
              '(0 . "On main: gp-auto: WIP on main")
            '(0 . "Dropped stash")))
      (should (equal (gp-checkout-pop-stash "/repo") "Dropped stash")))))

(provide 'gp-checkout-test)
(ert-deftest gp-test-run-redirects-to-worktree-without-stashing ()
  "When a sibling worktree holds BRANCH, use it and never stash.
This is the regression guard: the old code ran the plan anyway, so
`git checkout' failed with \"already used by worktree\" AFTER the
auto-stash step had already run -- stranding the user's work."
  (let ((calls '()))
    (cl-letf (((symbol-function 'gp-checkout--git)
               (lambda (_dir &rest args)
                 (setq calls (append calls (list args)))
                 (if (equal (car args) "worktree")
                     (cons 0 (concat "worktree /repo\nHEAD a\n"
                                     "branch refs/heads/main\n\n"
                                     "worktree /repo/.wt/x\nHEAD b\n"
                                     "branch refs/heads/feature\n"))
                   (cons 0 "")))))
      (let ((res (gp-checkout-run "/repo" "feature" "main")))
        (should (plist-get res :ok))
        (should (equal (plist-get res :dir) "/repo/.wt/x"))
        (should-not (plist-get res :stashed))
        ;; only the worktree query ran -- no stash, no checkout
        (should (equal calls '(("worktree" "list" "--porcelain"))))))))

(ert-deftest gp-test-run-normal-path-still-switches ()
  "With no competing worktree the ordinary plan still runs."
  (let ((calls '()))
    (cl-letf (((symbol-function 'gp-checkout--git)
               (lambda (_dir &rest args)
                 (setq calls (append calls (list args)))
                 (cons 0 (if (equal (car args) "worktree")
                             "worktree /repo\nHEAD a\nbranch refs/heads/main\n"
                           "")))))
      (let ((res (gp-checkout-run "/repo" "feature")))
        (should (plist-get res :ok))
        (should (equal (plist-get res :dir) "/repo"))
        (should (member '("checkout" "feature") calls))))))

;;; gp-checkout-test.el ends here