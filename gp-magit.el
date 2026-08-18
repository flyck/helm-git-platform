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
(require 'eieio)
;; Compile-time only: gives the byte-compiler `magit-section' slot names.
;; Not a hard runtime dependency -- this file stays loadable without magit,
;; and every entry point is gated on `magit-diff-mode'.
(eval-when-compile
  (require 'magit-section nil t)
  (require 'magit-base nil t))   ;; defines the hunk section's `to-range' slot

(declare-function magit-current-file "magit-git")
(declare-function magit-current-section "magit-section")
(declare-function magit-diff-hunk-line "magit-diff")
(declare-function magit-section-ident "magit-section")
(defvar magit-root-section)
(declare-function gp-watch--repo-for-file "gp-watch")
(declare-function gp-watch--current-branch "gp-watch")
(declare-function gp-watch--pr-for "gp-watch")
(declare-function gp-compose "gp-compose")
(defvar gp-watch-mode)
(defvar magit-refresh-buffer-hook)
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
Walks the buffer's magit section tree, so it works on the washed
diff magit actually displays (which has no `+++ b/PATH' header --
that text is kept in the file section's `header' slot instead).
Falls back to scanning raw diff text when there is no section
tree, which is what the unit tests exercise."
  (if (gp-magit--section-tree-p)
      (gp-magit--section-line-positions path)
    (gp-magit--raw-line-positions path)))

(defun gp-magit--section-tree-p ()
  "Non-nil when this buffer has a usable magit section tree."
  (and (bound-and-true-p magit-root-section)
       (fboundp 'magit-section-ident)
       t))

(defun gp-magit--file-sections (path)
  "Return the file sections of this diff buffer whose file is PATH."
  (let (found)
    (dolist (file (oref magit-root-section children))
      (when (and (eq (oref file type) 'file)
                 (equal (oref file value) path))
        (push file found)))
    (nreverse found)))

(defconst gp-magit--to-range-slot 'to-range
  "The `magit-hunk-section' slot holding a hunk's new-side line range.
Held in a variable so the slot is resolved at RUNTIME.  A literal
`oref'/`eieio-oref' resolves it at compile time, which requires
`magit-hunk-section' to be defined then -- and the magit requires in
this file are noerror, so a checkout where magit fails to load (a
transient version conflict in CI is enough) breaks the build with
\"Unknown slot\" rather than degrading gracefully.")

(defun gp-magit--section-line-positions (path)
  "Return (NEW-LINE . BUFFER-POS) for PATH using magit's section tree.
Each hunk's own `to-range' gives the new-side start line, so the
line numbers agree with what `magit-diff-hunk-line' reports for
the same position -- the numbers PR comments are anchored to."
  (let (result)
    (dolist (file (gp-magit--file-sections path))
      (dolist (hunk (oref file children))
        (when (eq (oref hunk type) 'hunk)
          ;; `eieio-oref', not `oref': the latter resolves the slot at
          ;; COMPILE time, which needs `magit-hunk-section' to be defined
          ;; then.  The requires below are noerror, and in a clean CI
          ;; checkout magit can fail to load outright (a transient version
          ;; conflict is enough), leaving the class undefined and failing
          ;; the build with "Unknown slot `to-range'".  Reading the slot at
          ;; runtime behaves identically where it matters -- by the time
          ;; this runs we are inside a live magit-diff buffer.
          (let ((line (car (eieio-oref hunk gp-magit--to-range-slot)))
                (end (oref hunk end)))
            (when line
              (save-excursion
                (goto-char (oref hunk content))
                (while (< (point) end)
                  (pcase (char-after (line-beginning-position))
                    (?+ (push (cons line (line-beginning-position)) result)
                        (setq line (1+ line)))
                    (?- nil)                 ;; old side only
                    (_  (setq line (1+ line))))
                  (forward-line 1))))))))
    (nreverse result)))

(defun gp-magit--raw-line-positions (path)
  "Return (NEW-LINE . BUFFER-POS) for PATH by scanning raw diff text.
Used when there is no magit section tree (plain diff text in a
buffer).  Recognises the `+++ b/PATH' header form."
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (let ((in-file nil) (newline-no nil))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (cond
             ((string-match (concat "^\\+\\+\\+ b/" (regexp-quote path) "$") line)
              (setq in-file t newline-no nil))
             ((string-match "^\\+\\+\\+ b/" line)
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

(defvar gp-magit--comments-cache (make-hash-table :test 'equal)
  "(full-name . id) -> (EXPIRY . COMMENTS), for the auto-redraw path.
`gp-magit-draw-comments' runs on every magit refresh, so the
comment fetch is cached briefly to keep redraws off the network.
`gp-magit-refresh-comments' bypasses this to force a refetch.")

(defcustom gp-magit-comments-cache-ttl 60
  "Seconds to cache a PR's comments for magit-diff redraws.
Set to 0 to always refetch."
  :type 'integer
  :group 'bitbucket)

(defun gp-magit--comments (full-name id &optional force)
  "Return PR FULL-NAME/ID's comments, cached for a short while.
With FORCE, refetch and refresh the cache entry."
  (let* ((key (cons full-name id))
         (entry (gethash key gp-magit--comments-cache)))
    (if (and (not force) entry (< (float-time) (car entry)))
        (cdr entry)
      (let ((comments (gp-pull-request-comments full-name id)))
        (puthash key (cons (+ (float-time) gp-magit-comments-cache-ttl) comments)
                 gp-magit--comments-cache)
        comments))))

(defun gp-magit-draw-comments (&optional force)
  "Draw the PR's inline comments as overlays in this magit-diff buffer.
With FORCE, refetch the comments instead of using the short cache.
Returns the number drawn, or nil when not applicable."
  (let ((pr (and gp-overlay-enabled (gp-magit--pr))))
    (when pr
      (let* ((full-name (gp-pr-full-name pr))
             (id (alist-get 'id pr))
             (comments (gp-magit--comments full-name id force))
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
                           (gp-magit-draw-comments t)))))))

(defun gp-magit-refresh-comments ()
  "Refetch and redraw PR comment overlays in this magit-diff buffer."
  (interactive)
  (let ((n (gp-magit-draw-comments t)))
    (message (if n (format "Drew %d PR comment(s)" n)
               "No PR for this diff"))))

;;;; Activation ---------------------------------------------------------------

(defun gp-magit--maybe-activate ()
  "Bind the keys and draw PR comments after a magit-diff refresh.
Runs from `magit-refresh-buffer-hook', which fires with the buffer
current on every refresh *including the first render* -- unlike
`magit-post-refresh-hook', which only runs for an explicit `g'.
A no-op outside a PR diff."
  (when (derived-mode-p 'magit-diff-mode)
    (gp-magit--activate-keys)
    (when (gp-magit--pr)
      (ignore-errors (gp-magit-draw-comments)))))

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
        (add-hook 'magit-refresh-buffer-hook #'gp-magit--maybe-activate)
        ;; catch diff buffers that were already open when the mode came on
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (derived-mode-p 'magit-diff-mode)
              (gp-magit--activate-keys)))))
    (remove-hook 'magit-diff-mode-hook #'gp-magit--activate-keys)
    (remove-hook 'magit-refresh-buffer-hook #'gp-magit--maybe-activate)))

(defvar-keymap gp-magit-command-map
  :doc "Keymap for PR-comment actions in magit-diff buffers (under `C-c B')."
  "n" #'gp-magit-add-comment
  "g" #'gp-magit-refresh-comments)

(defvar-keymap gp-magit-local-mode-map
  :doc "Buffer-local keymap for PR-comment actions in a magit-diff buffer."
  "C-c B" gp-magit-command-map)

(define-minor-mode gp-magit-local-mode
  "Buffer-local mode giving magit-diff buffers the PR-comment keys.
Its map is registered in `minor-mode-overriding-map-alist' so that
`C-c B n' reaches `gp-magit-add-comment' here, rather than being
shadowed by the identical prefix in the global `gp-watch-mode-map'
\(minor-mode maps outrank the buffer-local map, so `local-set-key'
is not enough)."
  :lighter nil
  :keymap gp-magit-local-mode-map
  (if gp-magit-local-mode
      (setf (alist-get 'gp-watch-mode minor-mode-overriding-map-alist)
            ;; Override only the keys we define; inherit the rest of
            ;; `gp-watch-mode-map' (e.g. `C-c B p') so overriding this
            ;; one minor mode does not make its other keys dead here.
            (make-composed-keymap gp-magit-local-mode-map
                                  (and (boundp 'gp-watch-mode-map)
                                       gp-watch-mode-map)))
    (setq minor-mode-overriding-map-alist
          (assq-delete-all 'gp-watch-mode minor-mode-overriding-map-alist))))

(defun gp-magit--activate-keys ()
  "Enable the PR-comment keys in this magit-diff buffer."
  (when (derived-mode-p 'magit-diff-mode)
    (gp-magit-local-mode 1)))

(provide 'gp-magit)
;;; gp-magit.el ends here
