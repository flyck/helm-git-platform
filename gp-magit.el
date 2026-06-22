;;; gp-magit.el --- PR comments inside magit-diff buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; When you view a PR branch's diff in Magit (`d' from the detail buffer, or
;; any `magit-diff'), this shows the PR's existing inline comments as
;; overlays on the diff's added lines, and lets you add a new inline comment
;; on the file:line at point.
;;
;; It is strictly scoped: it only acts in `magit-diff-mode' buffers, only
;; when `gp-watch-mode' is on, and only when the repo's checked-out branch
;; has an open PR.  Anywhere else -- rebase, log, status, a non-PR diff -- it
;; is a no-op, so it never interferes with ordinary Magit use.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'git-platform)
(require 'gp-local)
(require 'gp-overlay)

(declare-function magit-current-file "magit-git")
(declare-function magit-current-section "magit-section")
(declare-function magit-diff-hunk-line "magit-diff")
(declare-function gp-watch--repo-for-file "gp-watch")
(declare-function gp-watch--current-branch "gp-watch")
(declare-function gp-watch--pr-for "gp-watch")
(declare-function gp-compose "gp-compose")
(defvar gp-watch-mode)
(defvar magit-post-refresh-hook)
(defvar magit-diff-mode-hook)

;;;; Context resolution -------------------------------------------------------

(defun gp-magit--pr ()
  "Return the open PR for this magit-diff buffer's repo+branch, or nil.
Only when `gp-watch-mode' is on and we are in a `magit-diff-mode'
buffer; reuses the watch-mode resolvers (and their caches)."
  (when (and (bound-and-true-p gp-watch-mode)
             (derived-mode-p 'magit-diff-mode)
             default-directory)
    (require 'gp-watch)
    (let* ((probe (expand-file-name "x" default-directory))
           (full-name (gp-watch--repo-for-file probe))
           (branch (and full-name (gp-watch--current-branch probe))))
      (and full-name branch (gp-watch--pr-for full-name branch)))))

(defun gp-magit--file-line-at-point ()
  "Return (PATH . LINE) for the new-side diff location at point, or nil."
  (let ((file (ignore-errors (magit-current-file)))
        (sec (ignore-errors (magit-current-section))))
    (when (and file sec)
      (let ((line (ignore-errors (magit-diff-hunk-line sec nil))))
        (when line (cons file line))))))

;;;; Drawing existing comments ------------------------------------------------

(defun gp-magit--comment-line-positions (path)
  "Return an alist (NEW-LINE . BUFFER-POS) for PATH's added lines in the diff.
Walks the diff buffer hunk by hunk, tracking the new-side line
number so comments can be anchored to their `+' lines."
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (let ((in-file nil) (newline-no nil))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (cond
             ;; entering a file section: match "diff --git .../PATH" or +++ b/PATH
             ((string-match "^\\(?:modified\\|new file\\|deleted\\|renamed\\).*" line)
              (setq in-file nil))
             ((string-match (concat "^\\+\\+\\+ b/" (regexp-quote path) "$") line)
              (setq in-file t))
             ((and in-file (string-match "^\\+\\+\\+ b/" line))
              (setq in-file nil))             ;; a different file's +++ line
             ((and in-file (string-match "^@@ -[0-9,]+ \\+\\([0-9]+\\)" line))
              (setq newline-no (string-to-number (match-string 1 line))))
             ((and in-file newline-no)
              (let ((c (and (> (length line) 0) (aref line 0))))
                (cond
                 ((eq c ?+)                      ;; added line: record + advance
                  (push (cons newline-no (line-beginning-position)) result)
                  (setq newline-no (1+ newline-no)))
                 ((eq c ?-) nil)                 ;; removed: new side unchanged
                 ((eq c ?\s) (setq newline-no (1+ newline-no)))  ;; context
                 (t (setq newline-no nil)))))))  ;; left the hunk body
          (forward-line 1))))
    (nreverse result)))

(defun gp-magit-draw-comments ()
  "Draw the PR's inline comments as overlays in this magit-diff buffer.
Returns the number drawn, or nil when not applicable."
  (let ((pr (and gp-overlay-enabled (gp-magit--pr))))
    (when pr
      (let* ((full-name (gp-pr-full-name pr))
             (id (alist-get 'id pr))
             (comments (gp-pull-request-comments full-name id))
             (by-file (gp-overlay-comments-by-file comments))
             (total 0))
        (gp-overlay-clear (current-buffer))
        (setq gp-overlay--pr pr)
        (pcase-dolist (`(,path . ,lines) by-file)
          (let ((positions (gp-magit--comment-line-positions path)))
            (pcase-dolist (`(,line . ,cmts) lines)
              (when-let* ((pos (cdr (assq line positions))))
                (save-excursion
                  (goto-char pos)
                  (let* ((eol (line-end-position))
                         (ov (make-overlay eol eol)))
                    (overlay-put ov 'gp t)
                    (overlay-put ov 'gp-comments cmts)
                    (overlay-put ov 'after-string
                                 (concat "\n" (gp-overlay--format cmts)))
                    (push ov gp-overlay--list)
                    (cl-incf total)))))))
        total))))

;;;; Commands -----------------------------------------------------------------

(defun gp-magit-add-comment ()
  "Add an inline PR comment on the file:line at point in the magit diff."
  (interactive)
  (let ((pr (gp-magit--pr))
        (loc (gp-magit--file-line-at-point)))
    (unless pr (user-error "No open PR for this diff's branch"))
    (unless loc (user-error "Point is not on a diff line"))
    (require 'gp-compose)
    (gp-compose
     (list :full-name (gp-pr-full-name pr)
           :id (alist-get 'id pr)
           :inline (cons (car loc) (cdr loc))
           :on-success (lambda (_c)
                         (when (derived-mode-p 'magit-diff-mode)
                           (gp-magit-draw-comments)))))))

(defun gp-magit-refresh-comments ()
  "Refetch and redraw PR comment overlays in this magit-diff buffer."
  (interactive)
  (let ((n (gp-magit-draw-comments)))
    (message (if n (format "Drew %d PR comment(s)" n)
               "No PR for this diff"))))

;;;; Activation ---------------------------------------------------------------

(defun gp-magit--maybe-activate ()
  "Draw PR comments after a magit-diff refresh, when applicable.
Added to `magit-diff-mode-hook' / refresh; a no-op outside a PR diff."
  (when (gp-magit--pr)
    (ignore-errors (gp-magit-draw-comments))))

;;;###autoload
(define-minor-mode gp-magit-mode
  "Show and add PR inline comments in magit-diff buffers.
Enable globally; it only acts in `magit-diff-mode' buffers whose
branch has an open PR (and `gp-watch-mode' is on)."
  :global t
  :group 'bitbucket
  (if gp-magit-mode
      (progn
        (add-hook 'magit-diff-mode-hook #'gp-magit--activate-keys)
        (add-hook 'magit-post-refresh-hook #'gp-magit--maybe-activate))
    (remove-hook 'magit-diff-mode-hook #'gp-magit--activate-keys)
    (remove-hook 'magit-post-refresh-hook #'gp-magit--maybe-activate)))

(defvar-keymap gp-magit-command-map
  :doc "Keymap for PR-comment actions in magit-diff buffers (under `C-c B')."
  "n" #'gp-magit-add-comment
  "g" #'gp-magit-refresh-comments)

(defun gp-magit--activate-keys ()
  "Bind the PR-comment keys locally in this magit-diff buffer."
  (local-set-key (kbd "C-c B") gp-magit-command-map))

(provide 'gp-magit)
;;; gp-magit.el ends here
