;;; gp-local.el --- Link pull requests to local checkouts -*- lexical-binding: t; -*-

;;; Commentary:

;; Bridges a pull request to a local working copy under
;; `gp-local-git-root' (default ~/git).
;;
;; A PR carries its repository as "owner/slug" (full_name).  We find
;; the local directory whose git "origin" remote points at that same repo
;; -- matching on the parsed remote, not just the folder name, so a
;; checkout living under a differently named directory still resolves.
;; Bitbucket and GitHub remotes are both recognised (see
;; `gp-local--forge-hosts').
;;
;; Once resolved we can open the project and check out the PR's branch.

;;; Code:

(require 'cl-lib)
(require 'git-platform)

(declare-function gp-checkout-run "gp-checkout")
(declare-function gp-checkout-ensure-clone "gp-checkout")
(declare-function gp-checkout-current-branch "gp-checkout")
(defvar gp-checkout-clone-base)

(defcustom gp-local-git-root "~/git"
  "Directory containing local clones of forge repositories."
  :type 'directory
  :group 'bitbucket)

(defcustom gp-open-function #'dired
  "Function called with a repo directory to \"open\" a PR locally.
Receives the absolute path of the working copy.  Replace with
e.g. `magit-status' or a projectile switch to taste."
  :type 'function
  :group 'bitbucket)

(defvar gp-local--remote-cache (make-hash-table :test 'equal)
  "Cache mapping a local directory to its parsed origin \"ws/slug\".")

;;;; Remote parsing ----------------------------------------------------------

(defconst gp-local--forge-hosts '("bitbucket\\.org" "github\\.com")
  "Regexps matching the remote hosts recognised as a code-review forge.")

(defun gp-local--forge-host-p (url)
  "Return non-nil if URL's host matches one of `gp-local--forge-hosts'."
  (and url (cl-some (lambda (re) (string-match-p re url)) gp-local--forge-hosts)))

(defconst gp-local--backend-host-alist
  '((git-platform-bitbucket . "bitbucket\\.org")
    (git-platform-github . "github\\.com"))
  "Backend class -> the remote host regexp it actually talks to.
Only one backend is active at a time, so a repo on the OTHER forge
cannot be queried; `gp-local--active-backend-host-p' uses this to
show nothing instead of letting a doomed API call 404.")

(defun gp-local--active-backend-host-p (url)
  "Return non-nil if URL's host matches the currently active backend.
Nil for another forge's URL or an unrecognised one: both read as
\"no repo here\".  A backend with no entry in
`gp-local--backend-host-alist' (e.g. `git-platform-mock') matches
nothing."
  (when url
    (when-let* ((re (alist-get (eieio-object-class (git-platform-backend))
                                gp-local--backend-host-alist)))
      (string-match-p re url))))

(defun gp-local--parse-remote (url)
  "Return \"owner/slug\" parsed from a git remote URL, or nil.
Handles SSH (git@HOST:owner/slug.git) and HTTPS
(https://x@HOST/owner/slug.git) forms, but only when HOST matches
the active backend (see `gp-local--active-backend-host-p')."
  (when (gp-local--active-backend-host-p url)
    (when (string-match "\\(?:bitbucket\\.org\\|github\\.com\\)[:/]\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?/?\\'" url)
      (concat (match-string 1 url) "/" (match-string 2 url)))))

(defun gp-local--git-output (&rest args)
  "Run git with ARGS in `default-directory'; return trimmed stdout or nil.
Nil on any non-zero exit or empty output."
  (with-temp-buffer
    (let ((status (apply #'process-file "git" nil t nil args)))
      (and (eq status 0)
           (let ((out (string-trim (buffer-string))))
             (unless (string-empty-p out) out))))))

(defun gp-local--forge-remote-url (dir)
  "Return a recognised forge remote URL for DIR, preferring `origin', or nil.
Asks git whether DIR is inside a work tree, so worktrees and
submodules (where `.git' is a FILE) work too.  If `origin' is not a
recognised forge remote, falls back to the first remote that is."
  (let ((default-directory (file-name-as-directory
                            (directory-file-name (expand-file-name dir)))))
    (when (equal "true" (gp-local--git-output
                         "rev-parse" "--is-inside-work-tree"))
      (let ((origin (gp-local--git-output "remote" "get-url" "origin")))
        (if (gp-local--forge-host-p origin)
            origin
          (let ((remotes (gp-local--git-output "remote")))
            (catch 'hit
              (dolist (r (and remotes (split-string remotes "\n" t)))
                (let ((url (gp-local--git-output "remote" "get-url" r)))
                  (when (gp-local--forge-host-p url)
                    (throw 'hit url))))
              ;; Nothing recognised: let the parse reject origin.
              origin)))))))

(defun gp-local--dir-remote (dir)
  "Return the parsed \"owner/slug\" of DIR's forge remote, cached.
Prefers `origin'; falls back to any other recognised forge remote.
Works in plain clones, worktrees and submodules."
  (let* ((key (directory-file-name (expand-file-name dir)))
         (cached (gethash key gp-local--remote-cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (let ((parsed (gp-local--parse-remote
                     (gp-local--forge-remote-url key))))
        ;; Cache only successes: nil may just mean the (lazily resolved)
        ;; active backend is the other forge, and could change later.
        (when parsed
          (puthash key parsed gp-local--remote-cache))
        parsed))))

(defun gp-local-clear-cache ()
  "Clear the cached directory->remote mapping."
  (clrhash gp-local--remote-cache))

;;;; Resolution --------------------------------------------------------------

(defun gp-local-find-checkout (full-name)
  "Return the local directory for the repo FULL-NAME (\"ws/slug\"), or nil.
Matches by git origin remote.  As a fast path, a same-named folder
is checked first, then any sibling whose remote matches."
  (let* ((root (expand-file-name gp-local-git-root))
         (slug (file-name-nondirectory full-name))
         (fast (expand-file-name slug root)))
    (or
     (when (and (file-directory-p fast)
                (equal (gp-local--dir-remote fast) full-name))
       fast)
     (cl-loop for dir in (and (file-directory-p root)
                              (directory-files root t "\\`[^.]" t))
              when (and (file-directory-p dir)
                        (equal (gp-local--dir-remote dir) full-name))
              return dir))))

(defun gp-local-anchor-to-checkout (pr)
  "Point the current buffer's `default-directory' at PR's local checkout.
PR-scoped buffers otherwise inherit an unrelated directory, so
`find-file', `compile' etc. would default to the wrong repo.

Local resolution only (`gp-local-find-checkout'): painting a buffer
must never clone or switch branches.  With no clone the existing
value is left untouched.  Returns the directory, or nil."
  (when-let* ((full-name (ignore-errors (gp-pr-full-name pr)))
              (dir (ignore-errors (gp-local-find-checkout full-name))))
    (setq default-directory (file-name-as-directory dir))))

;;;; Actions -----------------------------------------------------------------

(defun gp-local-open (pr)
  "Open the local checkout for PR using `gp-open-function'.
Returns the directory, or signals if no local clone is found."
  (let* ((full-name (gp-pr-full-name pr))
         (dir (gp-local-find-checkout full-name)))
    (unless dir
      (user-error "No local checkout of %s under %s"
                  full-name gp-local-git-root))
    (funcall gp-open-function dir)
    dir))

(defun gp-local-clone-dest (full-name)
  "Return the directory a clone of FULL-NAME would live in under the git root."
  (expand-file-name (file-name-nondirectory full-name)
                    (expand-file-name gp-local-git-root)))

(defun gp-local-resolve-dir (full-name &optional clone)
  "Return the local directory for repo FULL-NAME.
If there is no checkout and CLONE is non-nil, clone it (requires
`gp-checkout-clone-base').  Signals a clear error when no
checkout exists and cloning is off."
  (require 'gp-checkout)
  (or (gp-local-find-checkout full-name)
      (when clone
        (gp-checkout-ensure-clone
         full-name (gp-local-clone-dest full-name)))
      (user-error
       "No local checkout of %s under %s%s"
       full-name gp-local-git-root
       (if gp-checkout-clone-base ""
         " (set gp-checkout-clone-base to auto-clone)"))))

(defun gp-local-ensure-checkout (pr)
  "Ensure PR's repo exists locally and is on its source branch; return the dir.
When already checked out on that branch, return at once without any
git fetch/checkout; otherwise clone/switch (a network operation)."
  (require 'gp-checkout)
  (let* ((full-name (gp-pr-full-name pr))
         (branch (gp-pr-source-branch pr))
         (dir (gp-local-find-checkout full-name)))
    (if (and dir branch
             (equal (gp-checkout-current-branch dir) branch))
        dir
      (let ((res (gp-local-checkout-branch pr dir)))
        (unless (plist-get res :ok)
          (user-error "Could not prepare %s: %s"
                      full-name (plist-get res :log)))
        (plist-get res :dir)))))

(defun gp-local-checkout-branch (pr &optional dir)
  "Check out PR's source branch in its local clone via the checkout service.
DIR overrides the auto-resolved directory; if there is no local
clone, one is cloned when `gp-checkout-clone-base' is set.
Returns the plist from `gp-checkout-run'; its :dir (possibly a
sibling worktree already holding the branch) wins over DIR."
  (require 'gp-checkout)
  (let* ((full-name (gp-pr-full-name pr))
         (branch (gp-pr-source-branch pr))
         (base (gp-pr-destination-branch pr))
         (dir (or dir (gp-local-resolve-dir full-name t))))
    (unless branch (user-error "PR has no source branch"))
    (let ((res (gp-checkout-run dir branch base)))
      (if (plist-member res :dir) res (append (list :dir dir) res)))))

(provide 'gp-local)
;;; gp-local.el ends here
