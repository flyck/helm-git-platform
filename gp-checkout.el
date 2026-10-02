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

(defcustom gp-checkout-recover-diverged-branch t
  "When non-nil, offer to recover a branch that has diverged from origin.
This happens when a branch is merged and deleted, then its name is
reused for an unrelated new branch: the local copy and the new
remote one share no common history, so `pull --ff-only' can never
reconcile them.  With this on, `gp-checkout-run' detects that case
and prompts to back the local branch up under a timestamped name
\(see `gp-checkout--backup-branch-name') and recreate it fresh from
the remote.  With this off, that case surfaces as today's plain
`pull --ff-only' failure instead."
  :type 'boolean
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

(defun gp-checkout-dirty-count (dir)
  "Return how many paths in DIR's working tree have uncommitted changes.
Zero when the tree is clean AND when git could not answer at all -- a
caller showing a warning wants \"nothing to say\" in both cases, and a
repo git refuses to read is not evidence of unsaved work.  Counts
`git status --porcelain' lines, so an untracked path counts once and a
staged-plus-modified path also counts once, matching what the user
would see in `magit-status'."
  (let ((res (gp-checkout--git dir "status" "--porcelain")))
    (if (and (= (car res) 0) (not (string-empty-p (cdr res))))
        (length (split-string (cdr res) "\n" t))
      0)))

(defun gp-checkout-current-branch (dir)
  "Return the current branch name in DIR, or nil."
  (let ((res (gp-checkout--git dir "rev-parse" "--abbrev-ref" "HEAD")))
    (when (= (car res) 0) (cdr res))))

(defun gp-checkout-branch-diverged-p (dir branch &optional remote)
  "Return non-nil if local BRANCH in DIR has diverged from REMOTE/BRANCH.
Diverged means neither is an ancestor of the other -- the case a
`pull --ff-only' can never resolve, typically because BRANCH was
merged and its name reused for an unrelated new branch on the
remote.  Nil (not diverged) when either ref is missing, so a genuinely
new local branch or a not-yet-fetched remote ref never false-positives
here; the ordinary `pull --ff-only' failure path still catches those."
  (let* ((remote (or remote gp-checkout-remote))
         (remote-ref (format "%s/%s" remote branch))
         (local-ok (= (car (gp-checkout--git dir "rev-parse" "--verify" branch)) 0))
         (remote-ok (= (car (gp-checkout--git dir "rev-parse" "--verify" remote-ref)) 0)))
    (and local-ok remote-ok
         (/= 0 (car (gp-checkout--git dir "merge-base" "--is-ancestor" branch remote-ref)))
         (/= 0 (car (gp-checkout--git dir "merge-base" "--is-ancestor" remote-ref branch))))))

(defun gp-checkout-branch-on-remote-p (dir branch &optional remote)
  "Return non-nil if BRANCH exists on REMOTE (default origin) for DIR."
  (let ((res (gp-checkout--git dir "ls-remote" "--heads"
                              (or remote gp-checkout-remote) branch)))
    (and (= (car res) 0) (not (string-empty-p (cdr res))))))

(defun gp-checkout-worktree-for-branch (dir branch)
  "Return the worktree directory in DIR's repo that has BRANCH checked out.
Nil when no worktree holds BRANCH, or when it is held by DIR itself
-- callers only care about a DIFFERENT worktree, since that is the
case git refuses to check out again.  Parses `git worktree list
--porcelain', whose records are blank-line separated with a
`worktree PATH' line and, for an attached head, a
`branch refs/heads/NAME' line."
  (let ((res (gp-checkout--git dir "worktree" "list" "--porcelain")))
    (when (= (car res) 0)
      (let ((self (file-truename (file-name-as-directory dir)))
            (path nil) (hit nil))
        (dolist (line (split-string (cdr res) "\n"))
          (cond
           ((string-prefix-p "worktree " line)
            (setq path (substring line 9)))
           ((string-prefix-p "branch refs/heads/" line)
            (when (and path (equal (substring line 18) branch))
              (let ((wt (file-truename (file-name-as-directory path))))
                (unless (equal wt self) (setq hit path)))))))
        hit))))

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

(defun gp-checkout--backup-branch-name (branch)
  "Return the backup-branch name used before deleting a diverged BRANCH.
Timestamped so recreating the same diverged branch twice never
collides with (or silently overwrites) an earlier backup."
  (format "%s-backup/%s/%s" gp-checkout-stash-prefix branch
          (format-time-string "%Y%m%d%H%M%S")))

(defun gp-checkout--recover-diverged-branch (dir branch remote log)
  "Handle BRANCH in DIR having diverged from REMOTE/BRANCH, appending to LOG.
Prompts to back up the local branch under a timestamped name, delete
it, and check out REMOTE/BRANCH fresh under BRANCH's name again.
Returns (OK . NEW-LOG); OK nil (declining, or any step failing) means
the caller should stop and report LOG as a normal failure -- the
`pull --ff-only' this replaces would have failed anyway, so declining
here is no worse than today's behaviour, just clearer about why."
  (if (not (yes-or-no-p
            (format "Local branch %s has diverged from %s/%s (likely reused after the old branch was merged). Back it up and check out %s/%s fresh? "
                    branch remote branch remote branch)))
      (cons nil (cons (format "$ (declined) local %s has diverged from %s/%s"
                              branch remote branch)
                      log))
    (let* ((backup (gp-checkout--backup-branch-name branch))
           (steps (list (list "branch" backup branch)
                        (list "checkout" "-B" branch (format "%s/%s" remote branch))))
           (ok t))
      (cl-block recover
        (dolist (args steps)
          (let ((res (apply #'gp-checkout--git dir args)))
            (push (format "$ git %s\n%s" (string-join args " ") (cdr res)) log)
            (unless (= (car res) 0)
              (setq ok nil)
              (cl-return-from recover)))))
      (cons ok log))))

(defun gp-checkout-run (dir branch &optional base)
  "Switch DIR to BRANCH, auto-stashing dirty work first.
When BASE (the PR's destination branch) is given, its remote ref
is fetched too so diffs against `origin/BASE' are accurate.
Returns a plist (:ok BOOL :stashed BOOL :log STRING :dir DIR).
Stops at the first failing git step and reports it.

When BRANCH is already checked out in ANOTHER worktree of the same
repo, that worktree is returned as :dir and no branch switch is
attempted: git refuses to check out one branch in two worktrees, so
the switch could only ever fail -- and failing AFTER the auto-stash
step would strand the user's work in a stash nobody pops.  The
worktree already holds the branch, which is what the caller wanted.

When `gp-checkout-recover-diverged-branch' is non-nil (the default),
checks just before the final `pull --ff-only' whether BRANCH has
diverged from the remote's own BRANCH (see
`gp-checkout-branch-diverged-p') -- typically because the name was
reused for an unrelated branch after the old one merged, which
`pull --ff-only' can never resolve.  When it has, prompts to back the
local branch up under a timestamped name and recreate it fresh from
the remote (`gp-checkout--recover-diverged-branch') instead of
letting `pull' fail with a raw, confusing git error.  With the
customvar off, that case surfaces as the plain `pull --ff-only'
failure it always used to."
  (let ((wt (gp-checkout-worktree-for-branch dir branch)))
    (if wt
        (list :ok t :stashed nil :dir wt
              :log (format "branch %s is checked out in worktree %s; using it"
                           branch wt))
      (let* ((dirty (gp-checkout-dirty-p dir))
             (current (gp-checkout-current-branch dir))
             (plan (gp-checkout--plan branch dirty current base))
             (remote gp-checkout-remote)
             (pull-step (list "pull" "--ff-only" remote branch))
             (log '())
             (ok t))
        (cl-block run
          (dolist (args plan)
            (if (and (equal args pull-step)
                     gp-checkout-recover-diverged-branch
                     (gp-checkout-branch-diverged-p dir branch remote))
                (pcase-let ((`(,rok . ,rlog)
                             (gp-checkout--recover-diverged-branch dir branch remote log)))
                  (setq log rlog)
                  (unless rok
                    (setq ok nil)
                    (cl-return-from run)))
              (let ((res (apply #'gp-checkout--git dir args)))
                (push (format "$ git %s\n%s" (string-join args " ") (cdr res)) log)
                (unless (= (car res) 0)
                  (setq ok nil)
                  (cl-return-from run))))))
        (list :ok ok :stashed dirty :dir dir
              :log (string-join (nreverse log) "\n"))))))

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
