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
;; Once resolved we can open the project, fetch, and check out the PR's
;; source branch, all without leaving Emacs.

;;; Code:

(require 'cl-lib)
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

(defconst gp-local--forge-hosts '("bitbucket\\.org" "github\\.com")
  "Regexps matching the remote hosts recognised as a code-review forge.")

(defun gp-local--forge-host-p (url)
  "Return non-nil if URL's host matches one of `gp-local--forge-hosts'."
  (and url (cl-some (lambda (re) (string-match-p re url)) gp-local--forge-hosts)))

(defconst gp-local--backend-host-alist
  '((git-platform-bitbucket . "bitbucket\\.org")
    (git-platform-github . "github\\.com"))
  "Backend class -> the remote host regexp it actually talks to.
Only one `git-platform' backend is active at a time (see
`git-platform-backend'), so a repo whose remote is on the OTHER
forge cannot be queried through it -- there is no Bitbucket-shaped
call that succeeds against api.github.com or vice versa.  Used by
`gp-local--active-backend-host-p' to recognise that mismatch and
show nothing instead of letting a doomed API call 404.")

(defun gp-local--active-backend-host-p (url)
  "Return non-nil if URL's host matches the currently active backend.
Nil for a URL on a different (but still recognised) forge, and nil
for an unrecognised URL -- both cases should read as \"no repo here\"
to callers, not surface a network error for a repo the active
backend can never reach.  A backend class with no entry in
`gp-local--backend-host-alist' (e.g. `git-platform-mock', which
isn't tied to any real remote at all) matches nothing here; mock
PRs are never resolved via a local git remote in the first place."
  (when url
    (when-let* ((re (alist-get (eieio-object-class (git-platform-backend))
                                gp-local--backend-host-alist)))
      (string-match-p re url))))

(defun gp-local--parse-remote (url)
  "Return \"owner/slug\" parsed from a git remote URL, or nil.
Handles SSH (git@HOST:owner/slug.git) and HTTPS
(https://x@HOST/owner/slug.git) forms, but only when HOST matches
the CURRENTLY ACTIVE backend (see `gp-local--active-backend-host-p')
-- a Bitbucket remote resolves to nil while the GitHub backend is
active, and vice versa, rather than resolving to a full-name no
network call against the active backend could ever reach."
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
Asks git itself whether DIR is inside a work tree -- so this also
works in worktrees and submodules, where `.git' is a FILE (a
gitdir pointer) rather than a directory.  If `origin' is not a
recognised forge remote (Bitbucket or GitHub), falls back to the
first remote whose URL is."
  (let ((default-directory (file-name-as-directory
                            (directory-file-name (expand-file-name dir)))))
    (when (equal "true" (gp-local--git-output
                         "rev-parse" "--is-inside-work-tree"))
      (let ((origin (gp-local--git-output "remote" "get-url" "origin")))
        (if (gp-local--forge-host-p origin)
            origin
          ;; origin missing or not a recognised forge -> scan every remote's URL
          (let ((remotes (gp-local--git-output "remote")))
            (catch 'hit
              (dolist (r (and remotes (split-string remotes "\n" t)))
                (let ((url (gp-local--git-output "remote" "get-url" r)))
                  (when (gp-local--forge-host-p url)
                    (throw 'hit url))))
              ;; nothing recognised; still hand back origin so the parse
              ;; (which rejects unrecognised URLs) decides, keeping old
              ;; behaviour for other remotes.
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
