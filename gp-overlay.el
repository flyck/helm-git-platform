;;; gp-overlay.el --- Inline PR comment overlays -*- lexical-binding: t; -*-

;;; Commentary:

;; Renders a PR's inline comments as overlays in the file buffers of the
;; corresponding local checkout, so review remarks appear next to the code
;; they refer to -- and lets you act on them in place: reply, resolve /
;; reopen, minimise an individual comment, or start a brand-new comment on
;; the line at point.
;;
;; Rendering is split into pure data helpers and an impure draw step:
;;   * `gp-overlay-comments-by-file' groups a flat comment list by
;;     file and line (tested directly);
;;   * `gp-overlay--comment-string' renders one comment, honouring
;;     resolved state and the per-comment collapse set;
;;   * `gp-overlay-apply-to-buffer' lays the overlays down.
;;
;; Each comment carries clickable [reply] [resolve] [-] [+ new] buttons and
;; equivalent keybindings; resolve/reopen call the PR API so the state
;; round-trips to Bitbucket.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bitbucket-api)
(require 'git-platform)

(declare-function gp-local-find-checkout "gp-local")
(declare-function gp-pr-full-name "gp-local")
(declare-function gp-compose "gp-compose")

;; Forward declaration: `gp-overlay-enabled' is a defcustom defined further
;; down in this same file (after the toggle command that reads it), so the
;; byte-compiler needs this to avoid a free-variable warning/error.
(defvar gp-overlay-enabled)

(defface gp-overlay-face
  '((t :inherit font-lock-comment-face :extend t))
  "Face for inline PR comment overlays."
  :group 'bitbucket-faces)

(defface gp-overlay-resolved-face
  '((t :inherit (shadow font-lock-comment-face) :strike-through t :extend t))
  "Face for resolved inline comments."
  :group 'bitbucket-faces)

(defface gp-overlay-button-face
  '((t :inherit (button gp-overlay-face)))
  "Face for the action buttons in an inline comment overlay."
  :group 'bitbucket-faces)

(defcustom gp-overlay-show-avatars t
  "When non-nil and on a graphical display, show commenter avatars."
  :type 'boolean
  :group 'bitbucket)

(defvar-local gp-overlay--list nil
  "Overlays created in this buffer, for later removal.")

(defvar-local gp-overlay--pr nil
  "The PR alist these overlays belong to (for actions).")

(defvar-local gp-overlay--lines nil
  "Last-drawn LINE-ALIST for this buffer, re-applied after a revert.")

(defvar-local gp-overlay--collapsed nil
  "Hash set of comment ids currently minimised in this buffer.")

(defun gp-overlay--collapsed-table ()
  "Return the buffer-local collapsed-id set, creating it if needed."
  (or gp-overlay--collapsed
      (setq gp-overlay--collapsed (make-hash-table :test 'eql))))

;;;; Grouping (pure) ---------------------------------------------------------

(defun gp-overlay--inline-line (comment)
  "Return the 1-based line a COMMENT is anchored to, or nil if not inline.
Bitbucket reports the line on the new side as `to' and on the old
side as `from'; prefer `to'."
  (let-alist comment
    (when .inline.path
      (or .inline.to .inline.from))))

(defcustom gp-overlay-show-resolved nil
  "When non-nil, resolved comments are also drawn as overlays.
Off by default: resolved threads are hidden to keep the code view
focused on what still needs attention."
  :type 'boolean :group 'bitbucket)

(defcustom gp-overlay-show-outdated nil
  "When non-nil, outdated comments are also drawn as overlays.
Off by default: an outdated comment is anchored to a line no
longer present in the PR's current diff, so its overlay would land
on unrelated code.  Outdated-ness is computed from the diff (see
`gp-comment-outdated-p'); with no diff available nothing is treated
as outdated.  Hidden to keep the view accurate."
  :type 'boolean :group 'bitbucket)

(defun gp-overlay--comment-thread-resolved-p (comment by-id)
  "Return non-nil if COMMENT or its thread root is resolved.
BY-ID is a hash table of comment id -> comment, used to walk up
`parent.id' links -- a reply's own `resolution' is often unset even
when the thread it belongs to has been resolved, since Bitbucket
only sets `resolution' on the comment the resolve action targeted
\(usually the root)."
  (let ((c comment) (seen (make-hash-table :test 'eql)))
    (catch 'done
      (while c
        (when (gp-comment-resolved-p c) (throw 'done t))
        (let ((id (alist-get 'id c)))
          (when (gethash id seen) (throw 'done nil))
          (puthash id t seen))
        (setq c (let-alist c (and .parent.id (gethash .parent.id by-id)))))
      nil)))

(defun gp-overlay-comments-by-file (comments &optional diff-by-file)
  "Group inline COMMENTS into an alist keyed by file path.
Returns ((PATH . ((LINE . (COMMENT...)) ...)) ...).  Non-inline
comments and those without a line are skipped; resolved comments
-- including replies whose thread root is resolved -- are skipped
unless `gp-overlay-show-resolved' is set, and -- when DIFF-BY-FILE
(from `gp-split-diff-by-file') is supplied -- outdated comments
unless `gp-overlay-show-outdated' is set.  Lines and files are kept
in ascending / insertion order."
  (let ((by-file '())
        (by-id (make-hash-table :test 'eql)))
    (dolist (c comments) (puthash (alist-get 'id c) c by-id))
    (dolist (c comments)
      (let ((path (let-alist c .inline.path))
            (line (gp-overlay--inline-line c)))
        (when (and path line
                   (or gp-overlay-show-resolved
                       (not (gp-overlay--comment-thread-resolved-p c by-id)))
                   (or gp-overlay-show-outdated
                       (not (gp-comment-outdated-p c diff-by-file))))
          (let* ((file-entry (or (assoc path by-file)
                                 (car (push (cons path '()) by-file))))
                 (line-entry (assq line (cdr file-entry))))
            (if line-entry
                (setcdr line-entry (append (cdr line-entry) (list c)))
              (setcdr file-entry
                      (cons (cons line (list c)) (cdr file-entry))))))))
    (dolist (fe by-file)
      (setcdr fe (cl-sort (cdr fe) #'< :key #'car)))
    (nreverse by-file)))

;;;; Avatars (GUI) -----------------------------------------------------------

(defcustom gp-overlay-avatar-ttl 86400
  "Seconds to cache a fetched avatar image (default 1 day)."
  :type 'integer :group 'bitbucket)

(defvar gp-overlay--avatar-cache (make-hash-table :test 'equal)
  "Cache of avatar URL -> (EXPIRY . IMAGE-or-`none').")

(defun gp-overlay--avatar-image (url)
  "Return a small image for avatar URL, or nil.
Cached for `gp-overlay-avatar-ttl' seconds; never errors."
  (when (and url gp-overlay-show-avatars (display-graphic-p))
    (let* ((entry (gethash url gp-overlay--avatar-cache))
           ;; entry is (EXPIRY . IMAGE-or-`none'); guard against any
           ;; stale/foreign shape so a bad cache never throws.
           (fresh (and (consp entry) (numberp (car entry))
                       (< (float-time) (car entry)))))
      (if fresh
          (unless (eq (cdr entry) 'none) (cdr entry))
        (let ((img (ignore-errors
                     (let* ((buf (url-retrieve-synchronously url t t 5)))
                       (when buf
                         (unwind-protect
                             (with-current-buffer buf
                               (goto-char (point-min))
                               (when (re-search-forward "\n\n" nil t)
                                 (create-image
                                  (buffer-substring-no-properties (point) (point-max))
                                  nil t :height (frame-char-height) :ascent 'center)))
                           (kill-buffer buf)))))))
          (puthash url (cons (+ (float-time) gp-overlay-avatar-ttl)
                             (or img 'none))
                   gp-overlay--avatar-cache)
          img)))))

;;;; Buttons -----------------------------------------------------------------

(defun gp-overlay--button (label help action comment)
  "Return a clickable LABEL string invoking ACTION on COMMENT.
ACTION is a symbol naming a function of one argument (the comment)."
  (propertize
   label
   'face 'gp-overlay-button-face
   'mouse-face 'highlight
   'help-echo help
   'gp-comment comment
   'gp-action action
   'keymap (let ((m (make-sparse-keymap)))
             (define-key m [mouse-1]
                         (lambda () (interactive) (funcall action comment)))
             (define-key m (kbd "RET")
                         (lambda () (interactive) (funcall action comment)))
             m)))

;;;; Per-comment rendering (pure-ish: no network) ----------------------------

(defun gp-overlay--collapsed-p (comment)
  "Return non-nil if COMMENT should render collapsed.
An explicit per-comment choice (`collapsed'/`expanded', set by
`gp-overlay-toggle-collapse') wins; otherwise resolved comments
default to collapsed and the rest to expanded."
  (let ((explicit (and gp-overlay--collapsed
                       (gethash (alist-get 'id comment) gp-overlay--collapsed))))
    (cond ((eq explicit 'collapsed) t)
          ((eq explicit 'expanded) nil)
          (t (gp-comment-resolved-p comment)))))

(defun gp-overlay--comment-string (comment)
  "Render a single COMMENT to a propertized string (with trailing newline).
Shows a one-line summary when collapsed or resolved, full text
otherwise, followed by the action buttons."
  (let* ((resolved (gp-comment-resolved-p comment))
         (collapsed (gp-overlay--collapsed-p comment))
         (face (if resolved 'gp-overlay-resolved-face 'gp-overlay-face))
         (author (let-alist comment (or .user.display_name "?")))
         (raw (string-trim (gp-resolve-mentions
                            (gp-resolve-emojis
                             (let-alist comment (or .content.raw ""))))))
         (first-line (car (split-string raw "\n")))
         (avatar (gp-overlay--avatar-image
                  (let-alist comment .user.links.avatar.href)))
         (head (concat
                "    "
                (if avatar (propertize " " 'display avatar) "💬")
                " "
                (propertize author 'face '(bold gp-overlay-face))
                (cond (resolved "  ✓ resolved")
                      (collapsed "  …"))
                ": "))
         (body (gp-overlay--face-body
                (gp-linkify-string (if collapsed first-line raw)) face))
         (buttons (gp-overlay--comment-buttons comment resolved collapsed
                                               (gp-comment-resolvable-p comment))))
    (concat (gp-overlay--face-body head face) body
            "\n" buttons "\n")))

(defun gp-overlay--face-body (str face)
  "Return STR with FACE applied only where no face is already set.
Keeps embedded link faces visible instead of overriding them."
  (let ((s (copy-sequence str)) (i 0) (n (length str)))
    (while (< i n)
      (let ((next (or (next-single-property-change i 'face s) n)))
        (unless (get-text-property i 'face s)
          (put-text-property i next 'face face s))
        (setq i next)))
    s))

(defun gp-overlay--comment-buttons (comment resolved collapsed resolvable)
  "Return the action-button line for COMMENT given RESOLVED/COLLAPSED state.
RESOLVABLE, when nil, hides the resolve/reopen action entirely (the
backend has no resolve concept for this comment, e.g. a GitHub
general/issue comment)."
  (string-join
   (delq nil
         (list "      "
               (gp-overlay--button "[reply]" "Reply to this comment"
                                          'gp-overlay-reply comment)
               (when resolvable
                 (if resolved
                     (gp-overlay--button "[reopen]" "Reopen on the PR"
                                                'gp-overlay-reopen comment)
                   (gp-overlay--button "[resolve]" "Resolve on the PR"
                                              'gp-overlay-resolve comment)))
               (gp-overlay--button (if collapsed "[+]" "[−]")
                                          "Minimise / expand this comment"
                                          'gp-overlay-toggle-collapse comment)))
   " "))

(defun gp-overlay--format (comments)
  "Render COMMENTS as the overlay after-string text."
  (mapconcat #'gp-overlay--comment-string comments ""))

;;;; Drawing -----------------------------------------------------------------

(defun gp-overlay--line-pos (line)
  "Return the buffer position at the start of 1-based LINE, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (zerop (forward-line (1- line)))
      (line-beginning-position))))

(defun gp-overlay-clear (&optional buffer)
  "Remove all bitbucket overlays from BUFFER (default current)."
  (with-current-buffer (or buffer (current-buffer))
    (mapc #'delete-overlay gp-overlay--list)
    (setq gp-overlay--list nil)))

(defun gp-overlay-apply-to-buffer (buffer line-alist)
  "Draw overlays in BUFFER from LINE-ALIST ((LINE . (COMMENT...))...).
Returns the number of overlays created.  Stores each line's
comments on the overlay so per-line actions can find them."
  (with-current-buffer buffer
    (gp-overlay-clear buffer)
    ;; remember the lines and re-apply them after a revert, which wipes
    ;; buffer text (and thus the overlays)
    (setq gp-overlay--lines line-alist)
    (add-hook 'after-revert-hook #'gp-overlay--reapply-after-revert nil t)
    (let ((n 0))
      ;; honour the global toggle: keep the lines (for re-enable) but draw nothing
      (when gp-overlay-enabled
      (pcase-dolist (`(,line . ,comments) line-alist)
        (when-let* ((pos (gp-overlay--line-pos line)))
          ;; Anchor at end of the commented line and render the comment
          ;; block on the following line(s): the leading newline pushes the
          ;; text below the code instead of trailing it on the same line.
          (let* ((eol (save-excursion (goto-char pos) (line-end-position)))
                 (ov (make-overlay eol eol)))
            (overlay-put ov 'bitbucket t)
            (overlay-put ov 'gp-line line)
            (overlay-put ov 'gp-comments comments)
            (overlay-put ov 'after-string
                         (concat "\n" (gp-overlay--format comments)))
            (push ov gp-overlay--list)
            (cl-incf n)))))
      n)))

(defun gp-overlay--redraw ()
  "Re-render all overlays in the current buffer from their stored comments.
Used after a state change (collapse/resolve) without refetching."
  (dolist (ov gp-overlay--list)
    (when (overlay-buffer ov)
      (overlay-put ov 'after-string
                   (concat "\n" (gp-overlay--format
                                 (overlay-get ov 'gp-comments)))))))

;;;; Locating the comment at point -------------------------------------------

(defun gp-overlay-comment-at-point ()
  "Return the comment described by the button/overlay at point, or nil.
Prefers a button under point; otherwise the first comment of the
overlay on the current line."
  (or (get-text-property (point) 'gp-comment)
      ;; the overlay is anchored at end-of-line, so match any bitbucket
      ;; overlay whose anchor sits on the current screen line
      (let ((bol (line-beginning-position))
            (eol (line-end-position)))
        (cl-loop for ov in gp-overlay--list
                 when (and (overlay-buffer ov)
                           (overlay-get ov 'gp-comments)
                           (<= bol (overlay-start ov))
                           (<= (overlay-start ov) eol))
                 return (car (overlay-get ov 'gp-comments))))))

;;;; Actions ------------------------------------------------------------------

(defun gp-overlay--target (&optional inline parent)
  "Build a compose TARGET plist for the buffer's PR.
INLINE is a (PATH . LINE) cons; PARENT a comment id."
  (let ((pr gp-overlay--pr))
    (unless pr (user-error "No PR associated with this buffer"))
    (list :full-name (gp-pr-full-name pr)
          :id (alist-get 'id pr)
          :inline inline
          :parent parent
          :on-success (lambda (_c) (gp-overlay-refresh)))))

(defun gp-overlay-reply (&optional comment)
  "Reply to COMMENT (or the comment at point)."
  (interactive)
  (require 'gp-compose)
  (let* ((c (or comment (gp-overlay-comment-at-point)
                (user-error "No comment here")))
         (inline (let-alist c
                   (when .inline.path (cons .inline.path
                                            (or .inline.to .inline.from))))))
    (gp-compose (gp-overlay--target inline (alist-get 'id c)))))

(defun gp-overlay-new-comment ()
  "Start a brand-new inline comment on the file line at point."
  (interactive)
  (require 'gp-compose)
  (let ((pr gp-overlay--pr))
    (unless pr (user-error "No PR associated with this buffer"))
    (gp-compose
     (gp-overlay--target
      (cons (file-relative-name
             buffer-file-name
             (gp-local-find-checkout (gp-pr-full-name pr)))
            (line-number-at-pos))))))

(defun gp-overlay-resolve (&optional comment)
  "Resolve COMMENT (or the one at point) on the PR and locally."
  (interactive)
  (gp-overlay--set-resolution comment t))

(defun gp-overlay-reopen (&optional comment)
  "Reopen COMMENT (or the one at point) on the PR and locally."
  (interactive)
  (gp-overlay--set-resolution comment nil))

(defun gp-overlay--set-resolution (comment resolve)
  "Resolve (RESOLVE non-nil) or reopen COMMENT on the PR, then redraw."
  (let* ((pr gp-overlay--pr)
         (c (or comment (gp-overlay-comment-at-point)
                (user-error "No comment here")))
         (full-name (gp-pr-full-name pr))
         (id (alist-get 'id pr))
         (cid (alist-get 'id c)))
    (unless (gp-comment-resolvable-p c)
      (user-error "This comment cannot be resolved/reopened"))
    (if resolve
        (gp-resolve-comment full-name id cid)
      (gp-reopen-comment full-name id cid))
    ;; reflect locally without a full refetch.  Mutate the existing cons
    ;; cell destructively so the comment object held by the overlay sees
    ;; the change (setf alist-get on a missing key would only rebind a
    ;; local).
    (let ((cell (assq 'resolution c)))
      (if cell
          (setcdr cell (and resolve '((user (display_name . "you")))))
        (when resolve
          (setcdr c (cons (cons 'resolution '((user (display_name . "you"))))
                          (cdr c))))))
    (gp-overlay--redraw)
    (message "Comment %s" (if resolve "resolved" "reopened"))))

(defun gp-overlay-toggle-collapse (&optional comment)
  "Toggle the minimised state of COMMENT (or the one at point)."
  (interactive)
  (let* ((c (or comment (gp-overlay-comment-at-point)
                (user-error "No comment here")))
         (id (alist-get 'id c))
         (tbl (gp-overlay--collapsed-table))
         ;; flip the *current effective* state to its opposite, explicitly
         (now-collapsed (gp-overlay--collapsed-p c)))
    (puthash id (if now-collapsed 'expanded 'collapsed) tbl)
    (gp-overlay--redraw)))

(defun gp-overlay--reapply-after-revert ()
  "Redraw the stored overlays in this buffer (e.g. after `revert-buffer').
Reuses the last-drawn lines, so no network fetch is needed."
  (when gp-overlay--lines
    (gp-overlay-apply-to-buffer (current-buffer) gp-overlay--lines)))

(defun gp-overlay-refresh ()
  "Refetch the PR's comments and redraw overlays in this buffer."
  (interactive)
  (when gp-overlay--pr
    (gp-overlay-pr gp-overlay--pr)))

;;;; Entry point + minor mode ------------------------------------------------

(defun gp-overlay--sorted-positions ()
  "Return this buffer's comment-overlay anchor positions, ascending."
  (sort (cl-loop for ov in gp-overlay--list
                 when (overlay-buffer ov) collect (overlay-start ov))
        #'<))

(defun gp-overlay-next-comment (&optional n)
  "Move point to the next commented line (Nth, default 1).
With a negative N, move to the previous one."
  (interactive "p")
  (let* ((positions (gp-overlay--sorted-positions))
         (positions (if (and n (< n 0)) (reverse positions) positions))
         (here (point))
         (cmp (if (and n (< n 0)) #'< #'>))
         (target (cl-find-if (lambda (p) (funcall cmp p here)) positions)))
    (unless positions (user-error "No PR comments in this buffer"))
    (goto-char (or target (car positions)))   ;; wrap around
    (beginning-of-line)
    (when-let* ((c (gp-overlay-comment-at-point)))
      (message "%s: %s"
               (let-alist c (or .user.display_name "?"))
               (car (split-string (string-trim (let-alist c (or .content.raw ""))) "\n"))))))

(defun gp-overlay-previous-comment (&optional n)
  "Move point to the previous commented line (Nth, default 1)."
  (interactive "p")
  (gp-overlay-next-comment (- (or n 1))))

(defvar-keymap gp-overlay-mode-map
  "C-c B r" #'gp-overlay-reply
  "C-c B R" #'gp-overlay-resolve
  "C-c B k" #'gp-overlay-reopen
  "C-c B n" #'gp-overlay-new-comment
  "C-c B TAB" #'gp-overlay-toggle-collapse
  "C-c B g" #'gp-overlay-refresh
  "C-c B ]" #'gp-overlay-next-comment
  "C-c B [" #'gp-overlay-previous-comment)

(define-minor-mode gp-overlay-mode
  "Minor mode adding keybindings for acting on inline PR comments."
  :lighter " BB-Inline"
  :keymap gp-overlay-mode-map)

(defcustom gp-overlay-enabled t
  "When non-nil, inline PR comment overlays are drawn.
Toggle with `gp-overlay-toggle-globally'; when off, no overlays are
drawn anywhere and existing ones are cleared."
  :type 'boolean :group 'bitbucket)

;;;###autoload
(defun gp-overlay-toggle-globally (&optional arg)
  "Globally turn inline PR comment overlays off or on.
With no ARG, toggle; positive ARG turns on, non-positive off.
When turning off, clears overlays in every buffer.  When turning
on, redraws in buffers that know their PR (and in any live
magit-diff buffer)."
  (interactive "P")
  (setq gp-overlay-enabled
        (cond ((null arg) (not gp-overlay-enabled))
              ((> (prefix-numeric-value arg) 0) t)
              (t nil)))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (cond
       ((not gp-overlay-enabled)
        (when gp-overlay--list (gp-overlay-clear buf)))
       ;; turning on: redraw where we can
       ((and gp-overlay--pr gp-overlay--lines)
        (gp-overlay-apply-to-buffer buf gp-overlay--lines))
       ((and (derived-mode-p 'magit-diff-mode) (fboundp 'gp-magit-draw-comments))
        (ignore-errors (gp-magit-draw-comments))))))
  (message "PR comment overlays %s" (if gp-overlay-enabled "on" "off"))
  gp-overlay-enabled)

;;;###autoload
(defun gp-overlay-pr (pr)
  "Fetch PR's inline comments and overlay them onto its local files.
Visits each referenced file in the local checkout and draws the
comments.  Returns the total number of overlays drawn."
  (require 'gp-local)
  (let* ((full-name (gp-pr-full-name pr))
         (id (alist-get 'id pr))
         (dir (gp-local-resolve-dir full-name t))
         (comments (gp-pull-request-comments full-name id))
         ;; fetch the diff so outdated comments (anchored to lines no
         ;; longer in the diff) can be filtered out; nil on any error
         ;; just disables the outdated filter, never breaks overlays.
         (diff-by-file (unless gp-overlay-show-outdated
                         (ignore-errors
                           (gp-split-diff-by-file
                            (gp-pull-request-diff
                             full-name id (gp-pr-source-commit pr))))))
         (by-file (gp-overlay-comments-by-file comments diff-by-file))
         (total 0))
    (pcase-dolist (`(,path . ,lines) by-file)
      (let ((file (expand-file-name path dir)))
        (when (file-exists-p file)
          (with-current-buffer (find-file-noselect file)
            (setq gp-overlay--pr pr)
            (gp-overlay-mode 1)
            (cl-incf total (gp-overlay-apply-to-buffer
                            (current-buffer) lines))))))
    (message "Drew %d inline comment overlay(s)" total)
    total))

(provide 'gp-overlay)
;;; gp-overlay.el ends here
