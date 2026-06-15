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
      ;; status + rev-parse + fetch + checkout + pull
      (should (= (length git-calls) 5))
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
;;; gp-checkout-test.el ends here
