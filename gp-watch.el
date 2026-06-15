;;; gp-watch.el --- Auto-overlay PR comments on visited files -*- lexical-binding: t; -*-

;;; Commentary:

;; A global minor mode that, whenever you open a file, quietly works out
;; whether you are sitting on a Bitbucket pull-request branch and, if that
;; PR has inline comments on the file, draws them as overlays -- without
;; you asking.  It also shows a per-buffer count of the repo's open PRs in
;; the mode line.
;;
;; The pipeline on `find-file', all early-exit and cached:
;;
;;   1. Is the file under a git repo with a Bitbucket remote?
;;      -> `gp-local--dir-remote', memoised here for ONE DAY since
;;         a checkout's origin essentially never changes.
;;   2. What branch is checked out?  (cheap git call)
;;   3. Is there an OPEN PR whose source branch is that branch?
;;      -> short-cached per (repo,branch).
;;   4. Does that PR have inline comments for THIS file?
;;      -> short-cached per PR; only then do we draw overlays and turn on
;;         `gp-overlay-mode'.
;;
;; If any step says no, the mode stays dormant for that buffer -- no
;; overlays, no mode-line noise.  Step 1's repo membership and the open-PR
;; count are also surfaced in the mode line.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bitbucket-api)
(require 'git-platform)
(require 'gp-local)
(require 'gp-overlay)

(declare-function gp-checkout-current-branch "gp-checkout")
(declare-function gp-helm-repo "gp-helm")
;; Forward declaration: the minor-mode variable is defined below but
;; referenced by the activation helper above its definition.
(defvar gp-watch-mode)

(defcustom gp-watch-repo-cache-ttl 86400
  "Seconds to cache whether a directory is a Bitbucket repo (default 1 day)."
  :type 'integer
  :group 'bitbucket)

(defcustom gp-watch-pr-cache-ttl 300
  "Seconds to cache the open-PR / comment lookups for a repo+branch."
  :type 'integer
  :group 'bitbucket)

(defcustom gp-watch-now-function #'float-time
  "Function returning the current time as a float; overridable in tests."
  :type 'function
  :group 'bitbucket)

;; Each cache maps KEY -> (EXPIRY . VALUE).  EXPIRY is a float-time.
(defvar gp-watch--repo-cache (make-hash-table :test 'equal)
  "dir -> (EXPIRY . FULL-NAME-or-`none').")
(defvar gp-watch--pr-cache (make-hash-table :test 'equal)
  "(full-name . branch) -> (EXPIRY . PR-or-`none').")
(defvar gp-watch--count-cache (make-hash-table :test 'equal)
  "full-name -> (EXPIRY . COUNT).")

(defvar-local gp-watch--repo nil
  "The Bitbucket \"ws/slug\" this buffer belongs to, or nil.")
(defvar-local gp-watch--pr-count nil
  "Open-PR count for this buffer's repo, or nil.")
(defvar-local gp-watch--branch-pr nil
  "The open PR for this buffer's current branch, or nil.")
(defvar-local gp-watch-mode-line ""
  "Mode-line string for this buffer.")
(put 'gp-watch-mode-line 'risky-local-variable t)

;;;; Generic TTL cache -------------------------------------------------------

(defun gp-watch--now ()
  "Current time as a float."
  (funcall gp-watch-now-function))

(defun gp-watch--cache-get (table key)
  "Return the cached value for KEY in TABLE if unexpired, else `miss'.
A stored `none' represents a cached negative; it is returned as nil."
  (let ((entry (gethash key table)))
    (if (and entry (< (gp-watch--now) (car entry)))
        (if (eq (cdr entry) 'none) nil (cdr entry))
      'miss)))

(defun gp-watch--cache-put (table key value ttl)
  "Cache VALUE (nil stored as `none') for KEY in TABLE for TTL seconds."
  (puthash key (cons (+ (gp-watch--now) ttl) (or value 'none)) table)
  value)

(defun gp-watch-clear-cache ()
  "Forget all watch caches (repo membership, PRs, counts)."
  (interactive)
  (clrhash gp-watch--repo-cache)
  (clrhash gp-watch--pr-cache)
  (clrhash gp-watch--count-cache))

;;;; Resolution steps (each cached) ------------------------------------------

(defun gp-watch--repo-for-file (file)
  "Return the Bitbucket \"ws/slug\" for FILE, or nil. Cached for a day."
  (when file
    (let* ((dir (file-name-directory (expand-file-name file)))
           (root (or (locate-dominating-file dir ".git") dir))
           (key (directory-file-name (expand-file-name root)))
           (cached (gp-watch--cache-get gp-watch--repo-cache key)))
      (if (not (eq cached 'miss))
          cached
        (gp-watch--cache-put
         gp-watch--repo-cache key
         (gp-local--dir-remote key)
         gp-watch-repo-cache-ttl)))))

(defun gp-watch--current-branch (file)
  "Return the git branch checked out for FILE's repo, or nil."
  (require 'gp-checkout)
  (let ((root (locate-dominating-file
               (file-name-directory (expand-file-name file)) ".git")))
    (when root (gp-checkout-current-branch root))))

(defun gp-watch--pr-for (full-name branch)
  "Return the open PR for FULL-NAME on BRANCH, or nil. Short-cached."
  (when (and full-name branch)
    (let* ((key (cons full-name branch))
           (cached (gp-watch--cache-get gp-watch--pr-cache key)))
      (if (not (eq cached 'miss))
          cached
        (gp-watch--cache-put
         gp-watch--pr-cache key
         (gp-open-pr-for-branch full-name branch)
         gp-watch-pr-cache-ttl)))))

(defun gp-watch--open-count (full-name)
  "Return the open-PR count for FULL-NAME, short-cached."
  (when full-name
    (let ((cached (gp-watch--cache-get gp-watch--count-cache full-name)))
      (if (not (eq cached 'miss))
          cached
        (gp-watch--cache-put
         gp-watch--count-cache full-name
         (gp-repo-open-pr-count full-name)
         gp-watch-pr-cache-ttl)))))

;;;; Mode-line ---------------------------------------------------------------

(defvar gp-watch--modeline-comment-map
  (let ((m (make-sparse-keymap)))
    (define-key m [mode-line mouse-1] #'gp-watch-visit-branch-pr)
    m)
  "Keymap making the mode-line comment counter clickable.")

(defvar gp-watch--modeline-count-map
  (let ((m (make-sparse-keymap)))
    (define-key m [mode-line mouse-1] #'gp-watch-list-repo-prs)
    m)
  "Keymap making the mode-line repo PR count clickable.")

(defun gp-watch-list-repo-prs ()
  "List the open PRs for this buffer's repo (Helm side window)."
  (interactive "@")
  (if (and gp-watch--repo (fboundp 'gp-helm-repo))
      (gp-helm-repo gp-watch--repo)
    (user-error "No Bitbucket repository for this buffer")))

(defun gp-watch--format-mode-line (count pr)
  "Return the per-buffer mode-line string.
COUNT is the repo's open-PR count; PR, when non-nil, is the open
PR for the current branch -- shown as a clickable comment counter."
  (if (and (null count) (null pr)) ""
    (concat
     " "
     (when count
       (concat (propertize "BB:" 'face 'shadow)
               (propertize (number-to-string count)
                           'face (if (> count 0) 'warning 'shadow)
                           'help-echo "Open PRs in this repo — click to list"
                           'mouse-face 'mode-line-highlight
                           'local-map gp-watch--modeline-count-map)))
     ;; clickable comment counter for the current-branch PR
     (when pr
       (let ((n (or (alist-get 'comment_count pr) 0)))
         (concat
          " "
          (propertize
           (format "💬%d" n)
           'face (if (> n 0) 'warning 'success)
           'help-echo (format "PR #%s on this branch: %d comment%s — click to open"
                              (alist-get 'id pr) n (if (= n 1) "" "s"))
           'mouse-face 'mode-line-highlight
           'local-map gp-watch--modeline-comment-map)))))))

(defun gp-watch--update-mode-line (count pr)
  "Set this buffer's watch mode-line from COUNT and the branch PR."
  (setq gp-watch--pr-count count
        gp-watch--branch-pr pr
        gp-watch-mode-line
        (gp-watch--format-mode-line count pr))
  (force-mode-line-update))

(declare-function gp-show-pr "gp-ui")

(defun gp-watch-visit-branch-pr ()
  "Open the detail page for the current buffer's branch PR.
Bound to the mode-line comment counter.  The \"@\" in the
`interactive' spec selects the window whose mode line was clicked,
so the correct buffer's branch PR is used."
  (interactive "@")
  (let ((pr gp-watch--branch-pr))
    (if pr
        (progn (require 'gp-ui) (gp-show-pr pr))
      (user-error "No pull request for the current branch"))))

;;;; Per-buffer activation ---------------------------------------------------

(defun gp-watch--maybe-activate (&optional buffer)
  "Run the resolution pipeline for BUFFER (default current) and act.
Draws overlays only when on a PR branch whose PR has comments for
this file.  Errors are swallowed so opening files never breaks."
  (with-current-buffer (or buffer (current-buffer))
    (when (and buffer-file-name gp-watch-mode)
      (condition-case err
          (let ((full-name (gp-watch--repo-for-file buffer-file-name)))
            (setq gp-watch--repo full-name)
            (when full-name
              (let* ((count (gp-watch--open-count full-name))
                     (branch (gp-watch--current-branch buffer-file-name))
                     (pr (gp-watch--pr-for full-name branch)))
                (gp-watch--update-mode-line count pr)
                ;; only draw when on a PR branch *and* comments exist here
                (when pr
                  (gp-watch--overlay-if-comments pr full-name)))))
        (error
         (message "gp-watch: %s" (error-message-string err)))))))

(defun gp-watch--overlay-if-comments (pr full-name)
  "Draw overlays for this buffer if PR has inline comments for the file.
Returns the number of overlays drawn."
  (let* ((id (alist-get 'id pr))
         (dir (gp-local-find-checkout full-name))
         (rel (and dir (file-relative-name buffer-file-name dir)))
         (comments (gp-pull-request-comments full-name id))
         (by-file (gp-overlay-comments-by-file comments))
         (lines (and rel (cdr (assoc rel by-file)))))
    (when lines
      (setq gp-overlay--pr pr)
      (gp-overlay-mode 1)
      (gp-overlay-apply-to-buffer (current-buffer) lines))))

;;;; Commenting from a watched file ------------------------------------------

(defun gp-watch-add-comment ()
  "Add an inline PR comment on the current file line.
Works in any file `gp-watch-mode' recognises as belonging to an
open PR for the checked-out branch -- even when no comment
overlays are drawn yet."
  (interactive)
  (let ((pr gp-watch--branch-pr))
    (unless pr
      (user-error "No open pull request for this file's branch"))
    ;; gp-overlay-new-comment composes at point; it reads gp-overlay--pr
    (setq gp-overlay--pr pr)
    (gp-overlay-new-comment)))

(defvar-keymap gp-watch-command-map
  :doc "Keymap for `gp-watch-mode' file actions (under `C-c b')."
  "n" #'gp-watch-add-comment
  "p" #'gp-watch-visit-branch-pr)

;;;; Global mode -------------------------------------------------------------

(defun gp-watch--find-file-hook ()
  "Entry from `find-file-hook'."
  (gp-watch--maybe-activate))

(defvar-keymap gp-watch-mode-map
  :doc "Global keymap active while `gp-watch-mode' is on."
  "C-c b" gp-watch-command-map)

;;;###autoload
(define-minor-mode gp-watch-mode
  "Global mode: show open-PR counts and auto-overlay PR comments on files.

When you visit a file in a git repo with a Bitbucket remote, the
mode line shows that repo's open-PR count; and if you are on a
pull-request branch whose PR has inline comments on the file, they
are drawn as overlays automatically.

While active, \\[gp-watch-add-comment] adds an inline comment on the
file line at point (when the file belongs to an open PR), and
\\[gp-watch-visit-branch-pr] opens that PR's detail buffer."
  :global t
  :group 'bitbucket
  :keymap gp-watch-mode-map
  (if gp-watch-mode
      (progn
        (unless (member '(:eval gp-watch-mode-line)
                        mode-line-misc-info)
          (setq mode-line-misc-info
                (append mode-line-misc-info
                        '((:eval gp-watch-mode-line)))))
        (add-hook 'find-file-hook #'gp-watch--find-file-hook)
        ;; activate for already-open file buffers too
        (dolist (buf (buffer-list))
          (gp-watch--maybe-activate buf)))
    (remove-hook 'find-file-hook #'gp-watch--find-file-hook)
    (setq mode-line-misc-info
          (delete '(:eval gp-watch-mode-line) mode-line-misc-info))))

(provide 'gp-watch)
;;; gp-watch.el ends here
