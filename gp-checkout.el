;;; gp-checkout.el --- Branch checkout service for pull requests -*- lexical-binding: t; -*-

;;; Commentary:

;; A self-contained service for getting a PR's branch checked out locally,
;; safely:
;;
;;   * if the working tree is dirty, stash the work under a recognisable
;;     name first (so nothing is lost when we switch);
;;   * fetch the branch from origin and check it out;
;;   * pull to fast-forward to the latest;
;;   * if there is no local clone at all, optionally clone it first.
;;
;; The git plumbing is expressed as pure command-list builders
;; (`gp-checkout--plan' and friends) that are unit-tested without
;; running git.  `gp-checkout-run' is the thin impure executor.
;;
;; Restoring a stash created here is a separate, explicit action
;; (`gp-checkout-pop-stash') -- we never auto-apply it onto a
;; different branch.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup gp-checkout nil
  "Branch checkout service for pull requests."
  :group 'bitbucket)

(defcustom gp-checkout-clone-base nil
  "Base SSH/HTTPS URL used to clone a missing repo, e.g.
\"git@bitbucket.org:\".  The repo \"workspace/slug\" plus \".git\"
is appended.  When nil, cloning is disabled and a missing checkout
is an error."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'gp-checkout)

(defcustom gp-checkout-stash-prefix "gp-auto"
  "Prefix for stashes created automatically before switching branches."
  :type 'string
  :group 'gp-checkout)

(defcustom gp-checkout-remote "origin"
  "Name of the git remote PR branches are fetched from."
  :type 'string
  :group 'gp-checkout)

;;;; Git helpers (impure, but tiny) ------------------------------------------

(defun gp-checkout--git (dir &rest args)
  "Run git with ARGS in DIR, returning (CODE . OUTPUT)."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (let ((code (apply #'call-process "git" nil t nil args)))
        (cons code (string-trim (buffer-string)))))))

(defun gp-checkout-dirty-p (dir)
  "Return non-nil if the working tree in DIR has uncommitted changes."
  (let ((res (gp-checkout--git dir "status" "--porcelain")))
    (and (= (car res) 0)
         (not (string-empty-p (cdr res))))))

(defun gp-checkout-current-branch (dir)
  "Return the current branch name in DIR, or nil."
  (let ((res (gp-checkout--git dir "rev-parse" "--abbrev-ref" "HEAD")))
    (when (= (car res) 0) (cdr res))))

(defun gp-checkout-branch-on-remote-p (dir branch &optional remote)
  "Return non-nil if BRANCH exists on REMOTE (default origin) for DIR."
  (let ((res (gp-checkout--git dir "ls-remote" "--heads"
                              (or remote gp-checkout-remote) branch)))
    (and (= (car res) 0) (not (string-empty-p (cdr res))))))

(defun gp-checkout-commit-summaries (dir base &optional branch)
  "Return the commit summary lines on BRANCH (default HEAD) not on BASE in DIR.
Newest first, as `git log BASE..BRANCH --format=%s' produces.  BASE
may be a local or `origin/'-qualified ref; if the plain BASE is
unknown we retry against `origin/BASE'.  Returns nil on failure."
  (let* ((range (lambda (b) (format "%s..%s" b (or branch "HEAD"))))
         (run (lambda (b)
                (gp-checkout--git dir "log" (funcall range b) "--format=%s")))
         (res (funcall run base)))
    (when (/= (car res) 0)
      (setq res (funcall run (concat gp-checkout-remote "/" base))))
    (when (= (car res) 0)
      (split-string (cdr res) "\n" t))))

(defun gp-checkout-push-branch (dir branch &optional remote)
  "Push BRANCH from DIR to REMOTE (default origin), setting upstream.
Refuses to push a branch named \"main\" or \"master\".  Returns a
plist (:ok BOOL :log STRING)."
  (when (member branch '("main" "master"))
    (user-error "Refusing to push protected branch %s" branch))
  (let ((res (gp-checkout--git dir "push" "--set-upstream"
                              (or remote gp-checkout-remote) branch)))
    (list :ok (= (car res) 0) :log (cdr res))))

;;;; Pure planning -----------------------------------------------------------

(defun gp-checkout--stash-name (branch)
  "Return the auto-stash message used when leaving BRANCH."
  (format "%s: WIP on %s" gp-checkout-stash-prefix (or branch "?")))

(defun gp-checkout--plan (branch &optional dirty current-branch base)
  "Return an ordered list of git argument-lists to switch to BRANCH.
When DIRTY is non-nil a stash step is prepended, labelled with
CURRENT-BRANCH.  When BASE (the PR's destination branch) is given,
its remote ref is fetched too so a diff against `origin/BASE'
matches what the server computed even if local BASE is stale.
Pure: only builds the commands; the caller runs them."
  (let ((remote gp-checkout-remote)
        (steps '()))
    (when dirty
      (push (list "stash" "push" "--include-untracked"
                  "-m" (gp-checkout--stash-name current-branch))
            steps))
    (push (list "fetch" remote branch) steps)
    (when (and base (not (equal base branch)))
      (push (list "fetch" remote base) steps))
    (push (list "checkout" branch) steps)
    (push (list "pull" "--ff-only" remote branch) steps)
    (nreverse steps)))

(defun gp-checkout--clone-command (full-name dest)
  "Return the git args to clone FULL-NAME (\"ws/slug\") into DEST.
Signals if `gp-checkout-clone-base' is nil."
  (unless gp-checkout-clone-base
    (user-error "Cloning disabled: set gp-checkout-clone-base"))
  (list "clone"
        (concat gp-checkout-clone-base full-name ".git")
        dest))

;;;; Execution ---------------------------------------------------------------

(defun gp-checkout-run (dir branch &optional base)
  "Switch DIR to BRANCH, auto-stashing dirty work first.
When BASE (the PR's destination branch) is given, its remote ref
is fetched too so diffs against `origin/BASE' are accurate.
Returns a plist (:ok BOOL :stashed BOOL :log STRING).  Stops at
the first failing git step and reports it."
  (let* ((dirty (gp-checkout-dirty-p dir))
         (current (gp-checkout-current-branch dir))
         (plan (gp-checkout--plan branch dirty current base))
         (log '())
         (ok t))
    (cl-block run
      (dolist (args plan)
        (let ((res (apply #'gp-checkout--git dir args)))
          (push (format "$ git %s\n%s" (string-join args " ") (cdr res)) log)
          (unless (= (car res) 0)
            (setq ok nil)
            (cl-return-from run)))))
    (list :ok ok :stashed dirty :log (string-join (nreverse log) "\n"))))

(defun gp-checkout-ensure-clone (full-name dest)
  "Ensure a clone of FULL-NAME exists at DEST, cloning if absent.
Returns DEST.  Errors if cloning is needed but disabled or fails."
  (if (file-directory-p (expand-file-name ".git" dest))
      dest
    (let* ((parent (file-name-directory (directory-file-name dest)))
           (default-directory (file-name-as-directory parent))
           (args (gp-checkout--clone-command full-name dest)))
      (make-directory parent t)
      (let ((res (apply #'gp-checkout--git parent args)))
        (unless (= (car res) 0)
          (error "git clone failed: %s" (cdr res)))
        dest))))

(defun gp-checkout-pop-stash (dir)
  "Pop the most recent auto-stash in DIR if it is one of ours.
Returns the git output, or signals if the top stash was not
created by this service."
  (let* ((top (gp-checkout--git
               dir "stash" "list" "--max-count=1" "--format=%gs")))
    (unless (and (= (car top) 0)
                 (string-prefix-p gp-checkout-stash-prefix
                                  ;; drop the "On <branch>: " git prefix
                                  (replace-regexp-in-string
                                   "\\`On [^:]*: " "" (cdr top))))
      (user-error "Top stash is not a %s stash; pop it manually"
                  gp-checkout-stash-prefix))
    (cdr (gp-checkout--git dir "stash" "pop"))))

(provide 'gp-checkout)
;;; gp-checkout.el ends here
