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

(defcustom gp-compose-preview-buffer (gp--buffer-name "comment preview")
  "Name of the buffer used to render a Markdown preview.
Defaults to the shared `gp-buffer-name-prefix' tag."
  :type 'string
  :group 'bitbucket)

(defcustom gp-review-batch-default t
  "When non-nil, a new top-level inline comment defaults into the review batch.
Both Bitbucket and GitHub have a native \"pending review\" mechanism:
the comment is created for real but stays invisible to everyone else
until the batch is submitted (see `gp-submit-review-batch').  Toggle
a single comment's fate with `gp-compose-toggle-batch' (\\<gp-compose-mode-map>\\[gp-compose-toggle-batch]) while composing it.
Only applies where the compose target says the comment is eligible
\(a brand-new inline comment; see `gp-overlay-new-comment') -- replies
and general comments always post immediately regardless of this."
  :type 'boolean
  :group 'bitbucket)

(defvar-local gp-compose--target nil
  "Plist describing where the composed comment goes:
\(:full-name S :id N :inline (PATH . LINE) :parent ID
 :submit-function FN :on-success FN :batchable BOOL).
SUBMIT-FUNCTION defaults to `gp-create-comment' on the active backend,
or to `gp-add-review-batch-comment' while :batchable is set and the
buffer-local `gp-compose--batch' toggle (see `gp-compose-toggle-batch')
is on.")

(defvar-local gp-compose--batch nil
  "Whether this buffer's comment goes into the review batch instead of
posting immediately.  Seeded from `gp-review-batch-default' when
TARGET's :batchable is set; meaningless otherwise.  See
`gp-compose-toggle-batch'.")

(defvar-local gp-compose--return-window nil
  "Window configuration to restore after the compose buffer closes.")

;;;; Markdown editing mode ---------------------------------------------------

(defun gp-compose--base-mode ()
  "Enter the best available Markdown editing mode for the compose buffer.

Wraps visually rather than by inserting newlines.  `gfm-mode' derives
from `text-mode', so a `text-mode-hook' that calls `turn-on-auto-fill'
\(a common default) hard-wraps the comment as it is typed -- and with
`gp-compose-hard-line-breaks' on, those accidental newlines are posted
as Markdown hard breaks, freezing the editor's wrap points into the
published comment.  Soft wrap keeps a paragraph one logical line, so
only breaks the user actually typed survive."
  (cond ((require 'markdown-mode nil t) (gfm-mode))
        (t (text-mode)))
  (auto-fill-mode -1)
  (setq-local truncate-lines nil)
  (visual-line-mode 1))

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
  "C-c C-p" #'gp-compose-preview
  "C-c C-b" #'gp-compose-toggle-batch)

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
Uses `markdown-mode' with `markdown-hide-markup' turned on when
available (so `**bold**'/`# heading' actually LOOK bold/large, markup
characters hidden, not just syntax-colored) else inserts TEXT
verbatim.  Pure enough to test: it touches only its own
temporary-ish buffer.

Turns `view-mode' off before erasing: a stale preview buffer left in
`view-mode' from a previous call can leave `buffer-read-only' set in
a way `inhibit-read-only' around just the erase/insert doesn't
reliably unwind before `view-mode' is re-enabled at the end, so the
mode is explicitly toggled off first rather than relied upon to stay
inert."
  (let ((buf (get-buffer-create gp-compose-preview-buffer)))
    (with-current-buffer buf
      (when view-mode (view-mode -1))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or text ""))
        (if (require 'markdown-mode nil t)
            (progn
              ;; `gfm-mode' runs `kill-all-local-variables' on entry, so
              ;; `markdown-hide-markup' must be set AFTER the mode is
              ;; active, not before -- setting it first is silently wiped.
              (delay-mode-hooks (gfm-mode))
              (setq-local markdown-hide-markup t)
              (font-lock-ensure))
          (text-mode))
        (goto-char (point-min))
        (view-mode 1)))
    buf))

(defun gp-compose-preview ()
  "Render the current draft as Markdown in a side window."
  (interactive)
  (let ((text (buffer-string)))
    (display-buffer (gp-compose-render-markdown text))))

(defun gp-compose--kill-preview-buffer ()
  "Kill the Markdown preview buffer, if one exists.
Called on submit/cancel so a stray preview from this session doesn't
linger (and doesn't carry stale content into the next compose)."
  (when-let* ((buf (get-buffer gp-compose-preview-buffer)))
    (kill-buffer buf)))

;;;; Submit / cancel ---------------------------------------------------------

(defun gp-compose--noun (target)
  "Return what TARGET composes -- \"comment\" unless it says otherwise.
Lets the buffer's prompts and messages read correctly when the same
editor is reused for something that is not a comment (a PR
description, say) via TARGET's `:what'."
  (or (plist-get target :what) "comment"))

(defun gp-compose--do-submit (target text)
  "Send TEXT to the PR described by TARGET, returning the created comment.
Honours TARGET's :submit-function; otherwise posts immediately via
`gp-create-comment', unless TARGET is :batchable and the buffer's
`gp-compose--batch' toggle is on, in which case it goes to the
review batch via `gp-add-review-batch-comment' instead."
  (let ((fn (or (plist-get target :submit-function)
                (if (and (plist-get target :batchable) gp-compose--batch)
                    #'gp-add-review-batch-comment
                  #'gp-create-comment))))
    (funcall fn
             (plist-get target :full-name)
             (plist-get target :id)
             text
             (plist-get target :inline)
             (plist-get target :parent))))

(defun gp-compose-toggle-batch ()
  "Toggle whether this buffer's comment joins the review batch.
Only meaningful when the compose target is :batchable (a brand-new
inline comment); a no-op with a clear message otherwise."
  (interactive)
  (unless (plist-get gp-compose--target :batchable)
    (user-error "This comment cannot be batched into a review"))
  (setq gp-compose--batch (not gp-compose--batch))
  (gp-compose--update-header-line)
  (message "%s" (if gp-compose--batch
                    "Will be added to the review batch (not visible until submitted)"
                  "Will post immediately")))

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
      ;; Clearing a description is a legitimate edit; an empty comment is
      ;; always a slip.  Refuse the latter, confirm the former.
      (if (plist-get target :allow-empty)
          (unless (yes-or-no-p
                   (format "%s is empty -- clear it? "
                           (capitalize (gp-compose--noun target))))
            (user-error "Aborted"))
        (user-error "%s is empty" (capitalize (gp-compose--noun target)))))
    ;; A target can opt out: hard-break munging suits chat-like comments,
    ;; but rewriting the newlines of a long-lived document (a PR
    ;; description) on every save slowly mangles tables and lists.
    (when (and gp-compose-hard-line-breaks
               (not (plist-get target :no-hard-breaks)))
      (setq text (gp-compose--apply-hard-breaks text)))
    ;; For a NEW inline comment, check the target is one the platform will
    ;; accept before posting.  GitHub answers an out-of-diff path/line with a
    ;; bare 422 only after the request goes out, which loses nothing but tells
    ;; the user nothing either; failing here keeps this buffer -- and the text
    ;; just written -- intact.  A reply carries a parent, which needs no such
    ;; check.
    (when-let* ((inline (and (not (plist-get target :parent))
                             (plist-get target :inline))))
      (when-let* ((problem (ignore-errors
                             (gp-inline-target-problem
                              (plist-get target :full-name)
                              (plist-get target :id)
                              (car inline) (cdr inline)))))
        (user-error "%s" problem)))
    (let* ((batched (and (plist-get target :batchable) gp-compose--batch))
           (created (gp-compose--do-submit target text)))
      (let ((on-success (plist-get target :on-success)))
        (when on-success (funcall on-success created)))
      (kill-buffer (current-buffer))
      (gp-compose--kill-preview-buffer)
      (when winconf (set-window-configuration winconf))
      ;; a target with an :on-success of its own has already reported
      (unless (plist-get target :on-success)
        (message "%s %s" (capitalize (gp-compose--noun target))
                 (if batched "added to the review batch" "posted")))
      created)))

(defun gp-compose-cancel ()
  "Discard the composed comment and close the buffer."
  (interactive)
  (let ((winconf gp-compose--return-window)
        (noun (gp-compose--noun gp-compose--target)))
    (kill-buffer (current-buffer))
    (gp-compose--kill-preview-buffer)
    (when winconf (set-window-configuration winconf))
    (message "%s discarded" (capitalize noun))))

;;;; Entry point -------------------------------------------------------------

(defun gp-compose--describe-target (target)
  "Return a short human description of TARGET for the buffer header."
  (let ((inline (plist-get target :inline))
        (parent (plist-get target :parent))
        (what (plist-get target :what)))
    (cond (what (format "%s of PR #%s"
                        (capitalize what) (plist-get target :id)))
          (parent (format "Reply on PR #%s" (plist-get target :id)))
          ;; the filename alone, not the full relative path: a deep path
          ;; combined with the batch state and every shortcut would overflow
          ;; the header line's single (non-wrapping) display line
          (inline (format "Inline comment on %s:%s"
                          (file-name-nondirectory (car inline)) (cdr inline)))
          (t (format "Comment on PR #%s" (plist-get target :id))))))

(defun gp-compose--update-header-line ()
  "Refresh the compose buffer's header line to reflect `gp-compose--batch'."
  (setq header-line-format
        (concat (gp-compose--describe-target gp-compose--target)
                (if (plist-get gp-compose--target :batchable)
                    (if gp-compose--batch " [batched]" " [immediate]")
                  "")
                "   C-c C-c submit · C-c C-p preview · C-c C-k cancel"
                (if (plist-get gp-compose--target :batchable)
                    " · C-c C-b batch"
                  ""))))

;;;###autoload
(defun gp-compose (target)
  "Open a compose buffer for a comment described by TARGET (a plist).
TARGET keys: :full-name :id [:inline (PATH . LINE)] [:parent ID]
[:submit-function FN] [:on-success FN] [:initial-text TEXT]
[:what NOUN] [:no-hard-breaks BOOL] [:allow-empty BOOL] [:batchable BOOL].
See `gp-compose--target'.

`:batchable' marks a new inline comment as eligible for the review
batch (see `gp-review-batch-default'/`gp-compose-toggle-batch'); the
other four let the same editor serve something that is not a
comment: `:what' names it in the header, prompts and messages;
`:no-hard-breaks' keeps newlines verbatim; `:allow-empty' makes
submitting nothing a confirmable clear rather than an error."
  (let ((buf (generate-new-buffer (gp--buffer-name (gp-compose--noun target))))
        (winconf (current-window-configuration)))
    (with-current-buffer buf
      (gp-compose--base-mode)
      (gp-compose-mode 1)
      (when-let* ((init (plist-get target :initial-text)))
        (insert init))
      (setq gp-compose--target target
            gp-compose--return-window winconf
            gp-compose--batch (and (plist-get target :batchable)
                                   gp-review-batch-default))
      (gp-compose--update-header-line))
    (pop-to-buffer buf)
    buf))

(provide 'gp-compose)
;;; gp-compose.el ends here
