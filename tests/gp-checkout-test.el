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
      ;; worktree-list + status + rev-parse + fetch + checkout + pull
      (should (= (length git-calls) 6))
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