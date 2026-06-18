;;; gp-compose.el --- Compose buffer for PR comments -*- lexical-binding: t; -*-

;;; Commentary:

;; A small compose buffer for writing a PR comment in Markdown.
;; It derives from `gfm-mode' (markdown-mode's GitHub-flavoured variant)
;; when available so you get live fontification and a familiar preview;
;; otherwise it falls back to plain `text-mode'.
;;
;;   C-c C-c   submit the comment to the PR
;;   C-c C-k   cancel
;;   C-c C-p   preview the rendered Markdown (local, instant)
;;
;; The submit target (repo, PR id, optional inline anchor, optional parent
;; for a reply) is captured in buffer-local `gp-compose--target'
;; when the buffer is created, so the same buffer serves new comments,
;; inline comments and replies.  The actual POST is delegated to a
;; submit-function so callers (and tests) can intercept it.

;;; Code:

(require 'cl-lib)
(require 'bitbucket-api)
(require 'git-platform)

(defvar markdown-mode-map)
(declare-function gfm-mode "markdown-mode")
(declare-function markdown-standalone "markdown-mode")

(defcustom gp-compose-preview-buffer "*PR Comment Preview*"
  "Name of the buffer used to render a Markdown preview."
  :type 'string
  :group 'bitbucket)

(defvar-local gp-compose--target nil
  "Plist describing where the composed comment goes:
\(:full-name S :id N :inline (PATH . LINE) :parent ID
 :submit-function FN :on-success FN).
SUBMIT-FUNCTION defaults to `bitbucket-create-comment'.")

(defvar-local gp-compose--return-window nil
  "Window configuration to restore after the compose buffer closes.")

;;;; Markdown editing mode ---------------------------------------------------

(defun gp-compose--base-mode ()
  "Enter the best available Markdown editing mode for the compose buffer."
  (cond ((require 'markdown-mode nil t) (gfm-mode))
        (t (text-mode))))

;;;; Emoji shortcode completion ----------------------------------------------

(declare-function emojify-emojis-each "emojify")
(declare-function emojify-create-emojify-emojis "emojify")
(declare-function ht-get "ht")

(defvar gp-compose--emoji-candidates nil
  "Cached list of (\":shortcode:\" . EMOJI) for completion.")

(defun gp-compose--emoji-cands ()
  "Return (and cache) the emoji shortcode completion candidates."
  (or gp-compose--emoji-candidates
      (setq gp-compose--emoji-candidates
            (cond
             ((require 'emojify nil t)
              (ignore-errors (emojify-create-emojify-emojis))
              (let (acc)
                (ignore-errors
                  (emojify-emojis-each
                   (lambda (key data)
                     (when (and (stringp key) (string-prefix-p ":" key))
                       (push (cons key (ignore-errors (ht-get data "unicode")))
                             acc)))))
                (or acc (copy-sequence gp--emoji-fallback))))
             (t (copy-sequence gp--emoji-fallback))))))

(defun gp-compose-emoji-capf ()
  "`completion-at-point-functions' entry completing :emoji: shortcodes.
Triggers after a colon, e.g. typing \":think\" offers \":thinking:\"."
  (when (looking-back ":\\([a-zA-Z0-9_+-]*\\)" (line-beginning-position))
    (let* ((start (match-beginning 0))
           (end (point))
           (cands (mapcar #'car (gp-compose--emoji-cands))))
      (list start end cands
            :exclusive 'no
            :annotation-function
            (lambda (cand)
              (let ((e (cdr (assoc cand (gp-compose--emoji-cands)))))
                (when e (concat " " e))))
            :exit-function
            (lambda (cand status)
              ;; on a full accept, replace the shortcode with the emoji glyph
              (when (eq status 'finished)
                (let ((e (cdr (assoc cand (gp-compose--emoji-cands)))))
                  (when e
                    (delete-region (- (point) (length cand)) (point))
                    (insert e)))))))))

(defvar-keymap gp-compose-mode-map
  "C-c C-c" #'gp-compose-submit
  "C-c C-k" #'gp-compose-cancel
  "C-c C-p" #'gp-compose-preview)

(define-minor-mode gp-compose-mode
  "Minor mode active in a PR comment compose buffer.
Layers submit/cancel/preview bindings on top of the Markdown mode,
and adds :emoji: shortcode completion via `completion-at-point'."
  :lighter " BB-Compose"
  :keymap gp-compose-mode-map
  (if gp-compose-mode
      (add-hook 'completion-at-point-functions
                #'gp-compose-emoji-capf nil t)
    (remove-hook 'completion-at-point-functions
                 #'gp-compose-emoji-capf t)))

;;;; Preview (local) ---------------------------------------------------------

(defun gp-compose-render-markdown (text)
  "Render TEXT as Markdown into a fontified buffer, returning that buffer.
Uses `markdown-mode' fontification when available, else inserts
TEXT verbatim.  Pure enough to test: it touches only its own
temporary-ish buffer."
  (let ((buf (get-buffer-create gp-compose-preview-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or text ""))
        (when (require 'markdown-mode nil t)
          (delay-mode-hooks (gfm-mode))
          (font-lock-ensure))
        (goto-char (point-min))
        (view-mode 1)))
    buf))

(defun gp-compose-preview ()
  "Render the current draft as Markdown in a side window."
  (interactive)
  (let ((text (buffer-string)))
    (display-buffer (gp-compose-render-markdown text))))

;;;; Submit / cancel ---------------------------------------------------------

(defun gp-compose--do-submit (target text)
  "Send TEXT to the PR described by TARGET, returning the created comment.
Honours TARGET's :submit-function, defaulting to
`gp-create-comment' on the active backend."
  (let ((fn (or (plist-get target :submit-function)
                #'gp-create-comment)))
    (funcall fn
             (plist-get target :full-name)
             (plist-get target :id)
             text
             (plist-get target :inline)
             (plist-get target :parent))))

(defcustom gp-compose-hard-line-breaks t
  "When non-nil, preserve single newlines as hard breaks in posted comments.

Bitbucket renders comments as Markdown, where a single newline is
treated as a space (so your line breaks collapse).  With this on,
single newlines get a trailing \"  \" (Markdown hard break) so the
text renders the way you typed it.  Blank lines (paragraph breaks)
and fenced code blocks are left untouched."
  :type 'boolean :group 'bitbucket)

(defun gp-compose--apply-hard-breaks (text)
  "Append Markdown hard-break spaces to single newlines in TEXT.
Lines that are blank, already end in two spaces, or sit inside a
``` fenced code block are left alone."
  (let* ((lines (split-string text "\n"))
         (vec (vconcat lines))
         (n (length vec))
         (in-fence nil) out)
    (dotimes (i n)
      (let* ((line (aref vec i))
             (fence-line (string-match-p "^[ \t]*```" line))
             (next (and (< (1+ i) n) (aref vec (1+ i))))
             (next-blank (or (null next)
                             (string-empty-p (string-trim-right next)))))
        (when fence-line (setq in-fence (not in-fence)))
        ;; a hard break is only useful when the NEXT line is more prose
        (push (if (or in-fence fence-line next-blank
                      (string-empty-p (string-trim-right line))
                      (string-suffix-p "  " line))
                  line
                (concat line "  "))
              out)))
    (string-join (nreverse out) "\n")))

(defun gp-compose-submit ()
  "Submit the composed comment to its PR and close the buffer."
  (interactive)
  (let ((text (string-trim (buffer-string)))
        (target gp-compose--target)
        (winconf gp-compose--return-window))
    (when (string-empty-p text)
      (user-error "Comment is empty"))
    (when gp-compose-hard-line-breaks
      (setq text (gp-compose--apply-hard-breaks text)))
    (let ((created (gp-compose--do-submit target text)))
      (let ((on-success (plist-get target :on-success)))
        (when on-success (funcall on-success created)))
      (kill-buffer (current-buffer))
      (when winconf (set-window-configuration winconf))
      (message "Comment posted")
      created)))

(defun gp-compose-cancel ()
  "Discard the composed comment and close the buffer."
  (interactive)
  (let ((winconf gp-compose--return-window))
    (kill-buffer (current-buffer))
    (when winconf (set-window-configuration winconf))
    (message "Comment discarded")))

;;;; Entry point -------------------------------------------------------------

(defun gp-compose--describe-target (target)
  "Return a short human description of TARGET for the buffer header."
  (let ((inline (plist-get target :inline))
        (parent (plist-get target :parent)))
    (cond (parent (format "Reply on PR #%s" (plist-get target :id)))
          (inline (format "Inline comment on %s:%s"
                          (car inline) (cdr inline)))
          (t (format "Comment on PR #%s" (plist-get target :id))))))

;;;###autoload
(defun gp-compose (target)
  "Open a compose buffer for a comment described by TARGET (a plist).
TARGET keys: :full-name :id [:inline (PATH . LINE)] [:parent ID]
[:submit-function FN] [:on-success FN].  See
`gp-compose--target'."
  (let ((buf (generate-new-buffer "*PR Comment*"))
        (winconf (current-window-configuration)))
    (with-current-buffer buf
      (gp-compose--base-mode)
      (gp-compose-mode 1)
      (when-let* ((init (plist-get target :initial-text)))
        (insert init))
      (setq gp-compose--target target
            gp-compose--return-window winconf
            header-line-format
            (concat (gp-compose--describe-target target)
                    "   C-c C-c submit · C-c C-p preview · C-c C-k cancel")))
    (pop-to-buffer buf)
    buf))

(provide 'gp-compose)
;;; gp-compose.el ends here
