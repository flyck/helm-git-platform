;;; gp-local.el --- Link pull requests to local checkouts -*- lexical-binding: t; -*-

;;; Commentary:

;; Bridges a pull request to a local working copy under
;; `gp-local-git-root' (default ~/git).
;;
;; A PR carries its repository as "workspace/slug" (full_name).  We find
;; the local directory whose git "origin" remote points at that same repo
;; -- matching on the parsed remote, not just the folder name, so a
;; checkout living under a differently named directory still resolves.
;;
;; Once resolved we can open the project, fetch, and check out the PR's
;; source branch, all without leaving Emacs.

;;; Code:

(require 'cl-lib)
(require 'bitbucket-api)
(require 'git-platform)

(declare-function gp-checkout-run "gp-checkout")
(declare-function gp-checkout-ensure-clone "gp-checkout")
(declare-function gp-checkout-current-branch "gp-checkout")
(defvar gp-checkout-clone-base)   ;; defined in gp-checkout.el

(defcustom gp-local-git-root "~/git"
  "Directory containing local clones of Bitbucket repositories."
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

(defun gp-local--parse-remote (url)
  "Return \"workspace/slug\" parsed from a git remote URL, or nil.
Handles SSH (git@bitbucket.org:ws/slug.git) and HTTPS
(https://x@bitbucket.org/ws/slug.git) forms."
  (when (and url (string-match-p "bitbucket\\.org" url))
    (when (string-match "bitbucket\\.org[:/]\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?/?\\'" url)
      (concat (match-string 1 url) "/" (match-string 2 url)))))

(defun gp-local--dir-remote (dir)
  "Return the parsed \"ws/slug\" of DIR's origin remote, cached."
  (let* ((key (directory-file-name (expand-file-name dir)))
         (cached (gethash key gp-local--remote-cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (let* ((default-directory (file-name-as-directory key))
             (url (when (file-directory-p (expand-file-name ".git" key))
                    (string-trim
                     (shell-command-to-string
                      "git remote get-url origin 2>/dev/null"))))
             (parsed (gp-local--parse-remote url)))
        (puthash key parsed gp-local--remote-cache)
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
     ;; fast path: folder named like the slug, with a matching remote
     (when (and (file-directory-p fast)
                (equal (gp-local--dir-remote fast) full-name))
       fast)
     ;; full scan: any immediate subdirectory whose remote matches
     (cl-loop for dir in (and (file-directory-p root)
                              (directory-files root t "\\`[^.]" t))
              when (and (file-directory-p dir)
                        (equal (gp-local--dir-remote dir) full-name))
              return dir))))

;; PR field accessors are the protocol functions `gp-pr-full-name',
;; `gp-pr-source-branch', `gp-pr-destination-branch' (see git-platform.el).

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

Fast path: if the repo is already checked out and already on the
PR's source branch, return immediately -- no git fetch/checkout.
Only when the repo is missing or on a different branch does it
clone/switch (a network operation).  This keeps file-visiting and
comment navigation snappy on repeated use."
  (require 'gp-checkout)
  (let* ((full-name (gp-pr-full-name pr))
         (branch (gp-pr-source-branch pr))
         (dir (gp-local-find-checkout full-name)))
    (if (and dir branch
             (equal (gp-checkout-current-branch dir) branch))
        dir                             ;; already here -- nothing to do
      (let ((res (gp-local-checkout-branch pr dir)))
        (unless (plist-get res :ok)
          (user-error "Could not prepare %s: %s"
                      full-name (plist-get res :log)))
        (plist-get res :dir)))))

(defun gp-local-checkout-branch (pr &optional dir)
  "Check out PR's source branch in its local clone via the checkout service.
DIR overrides the auto-resolved directory; if there is no local
clone, one is cloned when `gp-checkout-clone-base' is set.
Returns the plist from `gp-checkout-run' with :dir added."
  (require 'gp-checkout)
  (let* ((full-name (gp-pr-full-name pr))
         (branch (gp-pr-source-branch pr))
         (base (gp-pr-destination-branch pr))
         (dir (or dir (gp-local-resolve-dir full-name t))))
    (unless branch (user-error "PR has no source branch"))
    (let ((res (gp-checkout-run dir branch base)))
      (append (list :dir dir) res))))

(provide 'gp-local)
;;; gp-local.el ends here
