;;; gp-ui.el --- magit-section UI for pull requests -*- lexical-binding: t; -*-

;;; Commentary:

;; The interactive surface: a `magit-section'-based list buffer showing
;; "Needs my review" and "My pull requests", and a per-PR detail buffer
;; rendering the comment thread.  A `transient' menu binds the actions.
;;
;; Rendering is factored into pure-ish helpers (they only need a live
;; buffer, no network) so the test-suite can drive them against mock data
;; and assert on the produced text and section tree.

;;; Code:

(require 'magit-section)
(require 'transient)
(require 'subr-x)
(require 'iso8601)
(require 'bitbucket-api)
(require 'git-platform)
(require 'gp-local)
(require 'gp-log)
(require 'gp-pipeline)

(declare-function gp-helm "gp-helm")
(declare-function gp-helm-terminal-send-comment "gp-helm-terminal")
(declare-function gp-helm-terminal-send-comments "gp-helm-terminal")
(declare-function gp-compose "gp-compose")
(declare-function gp-reviewers-edit "gp-reviewers")
(declare-function gp-overlay-pr "gp-overlay")
(declare-function gfm-mode "markdown-mode")
(declare-function magit-section-toggle "magit-section")
(declare-function magit-section-hide "magit-section")
(defvar magit-root-section)
(defvar magit-section-highlight-current)
(defvar gp-checkout-remote)
(declare-function magit-diff-range "magit-diff")
(declare-function magit-show-commit "magit-diff")
(declare-function magit-rev-verify "magit-git")
(declare-function magit-status "magit-status")
(declare-function magit-refresh "magit-mode")
(declare-function gp-overlay--avatar-image "gp-overlay")
(defvar gp-helm--last-visited-pr-id)

(defcustom gp-detail-show-avatars t
  "When non-nil and graphical, show author avatars in the detail buffer."
  :type 'boolean :group 'bitbucket)

(defun gp--avatar-string (url &optional fallback)
  "Return an avatar image string for URL, or FALLBACK (default \"👤\")."
  (require 'gp-overlay)
  (let ((img (and gp-detail-show-avatars
                  (ignore-errors
                    (let ((gp-overlay-show-avatars t))
                      (gp-overlay--avatar-image url))))))
    (if img (propertize " " 'display img) (or fallback "👤"))))

(defun gp--format-date (iso)
  "Format an ISO-8601 timestamp ISO as local \"YYYY-MM-DD HH:MM\", or ISO as-is."
  (or (ignore-errors
        (format-time-string "%Y-%m-%d %H:%M" (encode-time (iso8601-parse iso))))
      iso))

(defun gp--relative-time (iso)
  "Return a human \"3 hours ago\"-style string for ISO-8601 timestamp ISO."
  (or (ignore-errors
        (let* ((then (float-time (encode-time (iso8601-parse iso))))
               (secs (max 0 (- (float-time) then))))
          (cond
           ((< secs 60) "just now")
           ((< secs 3600) (format "%d minute%s ago" (/ secs 60)
                                  (if (< secs 120) "" "s")))
           ((< secs 86400) (let ((h (floor secs 3600)))
                             (format "%d hour%s ago" h (if (= h 1) "" "s"))))
           ((< secs 2592000) (let ((d (floor secs 86400)))
                               (format "%d day%s ago" d (if (= d 1) "" "s"))))
           ((< secs 31536000) (let ((m (floor secs 2592000)))
                                (format "%d month%s ago" m (if (= m 1) "" "s"))))
           (t (let ((y (floor secs 31536000)))
                (format "%d year%s ago" y (if (= y 1) "" "s")))))))
      ""))

(defgroup gp-faces nil "Faces for gp-ui." :group 'bitbucket)

(defface gp-pr-id-face '((t :inherit magit-hash))
  "Face for PR ids." :group 'bitbucket-faces)
(defface gp-pr-title-face '((t :inherit default))
  "Face for PR titles." :group 'bitbucket-faces)
(defface gp-author-face '((t :inherit magit-log-author))
  "Face for PR authors." :group 'bitbucket-faces)
(defface gp-branch-face '((t :inherit magit-branch-local))
  "Face for branch names." :group 'bitbucket-faces)
(defface gp-comment-author-face '((t :inherit bold))
  "Face for comment authors." :group 'bitbucket-faces)
(defface gp-detail-title-face
  '((t :inherit gp-pr-title-face :weight bold :height 1.4))
  "Face for the large PR title in the detail buffer." :group 'bitbucket-faces)
(defface gp-link-face '((t :inherit link))
  "Face for the clickable PR link." :group 'bitbucket-faces)
(defface gp-comment-marked-face '((t :inherit highlight :extend t))
  "Face for comments marked for batch terminal handoff."
  :group 'bitbucket-faces)

(defface gp-label-face '((t :inherit magit-tag))
  "Fallback face for a PR label with no usable colour.
Also the base every colour-derived label face inherits from, so a
theme can restyle all labels (box, weight, height) in one place while
each keeps its own platform colour."
  :group 'bitbucket-faces)

(defcustom gp-label-colors t
  "When non-nil, render each PR label in the colour the platform gives it.
GitHub assigns every label a hex colour; honouring it makes the list
scan the same way the web UI does.  Set to nil to render every label
in `gp-label-face' instead -- useful on a terminal whose palette
fights the repo's colours."
  :type 'boolean :group 'bitbucket)

(defvar gp--label-face-cache (make-hash-table :test 'equal)
  "HEX -> face symbol, for faces derived from platform label colours.
Faces are interned, so they are built once per distinct colour rather
than per render (a list view repeats the same handful of labels across
many rows).")

(defun gp--color-luminance (hex)
  "Return the relative luminance (0..1) of \"RRGGBB\" string HEX, or nil.
Used only to decide whether black or white text reads on top of it, so
this is the cheap sRGB approximation, not the gamma-corrected form."
  (when (and (stringp hex) (string-match-p "\\`[0-9A-Fa-f]\\{6\\}\\'" hex))
    (let ((r (/ (string-to-number (substring hex 0 2) 16) 255.0))
          (g (/ (string-to-number (substring hex 2 4) 16) 255.0))
          (b (/ (string-to-number (substring hex 4 6) 16) 255.0)))
      (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)))))

(defun gp--label-face (hex)
  "Return a face rendering a label whose platform colour is HEX.
Falls back to `gp-label-face' when HEX is missing or malformed, when
`gp-label-colors' is nil, or when the display has too few colours to
place an arbitrary background (a 16-colour TTY would snap every label
to the same approximate shade, losing the distinction the colour was
carrying)."
  (let ((lum (and gp-label-colors (gp--color-luminance hex))))
    (if (or (null lum) (< (display-color-cells) 256))
        'gp-label-face
      (or (gethash hex gp--label-face-cache)
          (let ((face (intern (format "gp-label-color-%s" (downcase hex)))))
            (unless (facep face)
              (make-face face)
              (set-face-attribute face nil
                                  :inherit 'gp-label-face
                                  :background (concat "#" hex)
                                  ;; dark labels need light text and vice versa
                                  :foreground (if (> lum 0.6) "black" "white")))
            (puthash hex face gp--label-face-cache)
            face)))))

(defun gp--format-labels (labels)
  "Return LABELS as one propertized string, or \"\" when there are none.
LABELS is `gp-pr-labels' shape.  Returns the empty string rather than
nil so callers can `concat' it unconditionally."
  (if (null labels)
      ""
    (mapconcat
     (lambda (l)
       (propertize (format " %s " (plist-get l :name))
                   'face (gp--label-face (plist-get l :color))
                   'help-echo (format "label: %s" (plist-get l :name))))
     labels " ")))

;;;; Section types -----------------------------------------------------------

;; Both section types stash their backing object in the standard `value'
;; slot magit provides, so no extra slots are needed.
(defclass gp-pr-section (magit-section) ())
(defclass gp-comment-section (magit-section) ())
(defclass gp-file-section (magit-section) ())
(defclass gp-commit-section (magit-section) ())

;;;; Buffer-local state ------------------------------------------------------

(defvar-local gp--prs nil
  "PR list backing the current list buffer.")
(defvar-local gp--pr nil
  "PR alist backing the current detail buffer.")
(defvar-local gp--detail-stats nil
  "Plist of (:commits :files :added :removed) for the detail buffer, or nil.")
(defvar-local gp--detail-diff nil
  "Alist of (PATH . DIFF-CHUNK) for the detail buffer, or nil.")
(defvar-local gp--detail-pipelines nil
  "Pipeline data plist (:current :recent) for the detail buffer, or nil.")
(defvar-local gp--detail-pipeline-timer nil
  "Poll timer re-fetching pipelines while a current run is unfinished, or nil.")
(defvar-local gp--detail-commits nil
  "List of plists (:hash :summary :author :date) for the detail buffer, or nil.
Newest first.  Fetched asynchronously via `gp-pull-request-commits-async'
-- see `gp--detail-load-commits'.")
(defvar-local gp--detail-comments nil
  "Cached comment list for the detail buffer (so it can redraw without refetch).")
(defvar-local gp--detail-reviewers nil
  "List of plists (:name :avatar :state) for the detail buffer, or nil.
Fetched asynchronously via `gp-pr-reviewers-async' -- see
`gp--detail-load-reviewers'.")
(defvar-local gp--detail-pending-action nil
  "Tag of the action button currently in flight, or nil.
Set by `gp--detail-run-action' just before a blocking mutation (draft
toggle, approve, resolve, …) and cleared once it returns.  The
render functions check this so that ONE button swaps to a spinner in
its own slot on the immediate redraw, instead of the mutation
blocking Emacs with no feedback until the full post-mutation refresh
lands (see `gp--insert-action-button/spinner').")
(defvar-local gp--detail-marked-comment-ids nil
  "Comment ids marked for batch terminal handoff in the detail buffer.")
(defvar-local gp--detail-refresh-token 0
  "Monotonic token used to ignore stale async detail refresh callbacks.")
;;;; Formatting helpers ------------------------------------------------------

(defun gp--pr-heading (pr)
  "Return a one-line propertized heading string for PR."
  (let-alist pr
    (let ((count (gp-pr-comment-count pr))
          ;; Labels sit with the title -- they say what the PR *is*.  Platforms
          ;; without them (Bitbucket) contribute nothing here, so no empty slot
          ;; is left behind and the line reads exactly as it did before.
          (labels (gp--format-labels (gp-pr-labels pr))))
      (concat
       (propertize (format "#%s" .id) 'face 'gp-pr-id-face)
       " "
       (propertize (or .title "(no title)") 'face 'gp-pr-title-face)
       (if (string-empty-p labels) "" (concat "  " labels))
       "  "
       (propertize (format "[%s]" (or (gp-pr-repo-slug pr) "?"))
                   'face 'gp-branch-face)
       " "
       (propertize (or (gp-pr-author-name pr) "?") 'face 'gp-author-face)
       (if (and count (> count 0))
           (format "  💬%d" count) "")))))

(defun gp--insert-pr (pr)
  "Insert a collapsible section for PR into the current buffer."
  (magit-insert-section (gp-pr-section pr)
    (magit-insert-heading (gp--pr-heading pr))
    (insert (format "  %s → %s\n"
                    (propertize (or (gp-pr-source-branch pr) "?")
                                'face 'gp-branch-face)
                    (propertize (or (gp-pr-destination-branch pr) "?")
                                'face 'gp-branch-face)))))

(defun gp--insert-group (title prs)
  "Insert a magit section grouping PRS under TITLE."
  (magit-insert-section (gp-group)
    (magit-insert-heading
      (format "%s (%d)" title (length prs)))
    (if prs
        (dolist (pr prs) (gp--insert-pr pr))
      (insert "  (none)\n"))
    (insert "\n")))

(defun gp--render-list (prs uuid)
  "Render PRS into the current buffer, partitioned by UUID authorship.
Assumes the buffer is in `gp-list-mode' and writable."
  (let* ((split (gp-partition-pull-requests prs uuid))
         (mine (car split))
         (reviewing (cdr split)))
    (magit-insert-section (gp-root)
      (gp--insert-group "Needs my review" reviewing)
      (gp--insert-group "My pull requests" mine))))

;;;; Comment rendering -------------------------------------------------------

(defun gp--comment-location (comment)
  "Return a human label for COMMENT's location (inline path:line or \"general\")."
  (let-alist comment
    (if .inline.path
        (format "%s:%s" .inline.path (or .inline.to .inline.from "?"))
      "general")))

(defun gp--linkify (start)
  "Add `link' face + clickability to markdown/bare links from START in buffer.
Handles \[label](url) (showing the label) and bare http(s) URLs."
  (save-excursion
    ;; [label](url) -> clickable label
    (goto-char start)
    (while (re-search-forward "\\[\\([^]]+\\)\\](\\(https?://[^)]+\\))" nil t)
      (let ((label (match-string 1)) (url (match-string 2)))
        (replace-match
         (propertize label 'face 'link 'mouse-face 'highlight
                     'help-echo url 'follow-link t
                     'keymap (let ((m (make-sparse-keymap)))
                               (define-key m [mouse-1]
                                           (lambda () (interactive) (browse-url url)))
                               (define-key m (kbd "RET")
                                           (lambda () (interactive) (browse-url url)))
                               m))
         t t)))
    ;; bare URLs not already faced
    (goto-char start)
    (while (re-search-forward "\\(?:^\\|[ \t]\\)\\(https?://[^ \t\n]+\\)" nil t)
      (unless (eq (get-text-property (match-beginning 1) 'face) 'link)
        (let ((url (match-string 1)))
          (add-text-properties
           (match-beginning 1) (match-end 1)
           (list 'face 'link 'mouse-face 'highlight 'help-echo url
                 'follow-link t
                 'keymap (let ((m (make-sparse-keymap)))
                           (define-key m [mouse-1]
                                       (lambda () (interactive) (browse-url url)))
                           (define-key m (kbd "RET")
                                       (lambda () (interactive) (browse-url url)))
                           m))))))))

(defun gp--render-markdown (text)
  "Return TEXT fontified as GitHub-flavoured Markdown, with colored links.
Bitbucket :shortcode: emojis and @{account_id} mentions are resolved first."
  (setq text (bitbucket-resolve-mentions (gp-resolve-emojis text)))
  (if (and text (require 'markdown-mode nil t))
      (with-temp-buffer
        (insert text)
        (delay-mode-hooks (gfm-mode))
        (font-lock-ensure)
        (gp--linkify (point-min))
        (buffer-string))
    (or text "")))

(defun gp--comment-threads (comments)
  "Order COMMENTS into a depth-tagged list ((COMMENT . DEPTH) ...).
Replies (those with a parent) are placed directly after their
parent and one level deeper, recursively.  Top-level comments are
ordered newest first; replies within a thread keep their
original (chronological) order."
  (let ((children (make-hash-table :test 'eql))   ;; parent-id -> (child...)
        (ids (make-hash-table :test 'eql))
        (roots '())
        (result '()))
    (dolist (c comments) (puthash (alist-get 'id c) t ids))
    (dolist (c comments)
      (let ((parent (let-alist c .parent.id)))
        ;; treat a reply whose parent is absent as a root (orphan)
        (if (and parent (gethash parent ids))
            (push c (gethash parent children))
          (push c roots))))
    (maphash (lambda (k v) (puthash k (nreverse v) children)) children)
    (cl-labels ((walk (c depth)
                  (push (cons c depth) result)
                  (dolist (kid (gethash (alist-get 'id c) children))
                    (walk kid (1+ depth)))))
      ;; newest root first; ISO-8601 strings compare chronologically
      (dolist (root (sort (nreverse roots)
                          (lambda (a b)
                            (string> (or (alist-get 'created_on a) "")
                                     (or (alist-get 'created_on b) "")))))
        (walk root 0)))
    (nreverse result)))

(defun gp--comment-thread-resolved-p (comment by-id)
  "Return non-nil if COMMENT or any ancestor of it is resolved.
BY-ID maps comment id -> comment, for walking `parent.id' links.

A reply's own `resolution' is usually unset even when its thread has
been resolved: Bitbucket only sets it on the comment the resolve
action targeted, normally the thread root.  Testing a reply on its own
therefore leaves it expanded under a collapsed root, which is exactly
the noise collapsing resolved threads is meant to remove.  Mirrors
`gp-overlay--comment-thread-resolved-p', which solves the same problem
for the overlay layer.  The `seen' guard keeps a cyclic parent chain
from looping forever."
  (let ((c comment) (seen (make-hash-table :test 'eql)))
    (catch 'done
      (while c
        (when (gp-comment-resolved-p c) (throw 'done t))
        (let ((id (alist-get 'id c)))
          (when (gethash id seen) (throw 'done nil))
          (puthash id t seen))
        (setq c (let-alist c (and .parent.id (gethash .parent.id by-id)))))
      nil)))

(defun gp--comments-by-id (comments)
  "Return a hash table of comment id -> comment for COMMENTS."
  (let ((h (make-hash-table :test 'eql)))
    (dolist (c comments h) (puthash (alist-get 'id c) c h))))

(defun gp--detail-comment-marked-p (comment)
  "Return non-nil when COMMENT is marked for batch terminal handoff."
  (memq (alist-get 'id comment) gp--detail-marked-comment-ids))

(defun gp--detail-marked-comments ()
  "Return marked comments from the detail buffer in display order."
  (let ((ids gp--detail-marked-comment-ids))
    (cl-remove-if-not (lambda (comment) (memq (alist-get 'id comment) ids))
                      gp--detail-comments)))

(defun gp--insert-comment (comment &optional pr depth by-id)
  "Insert a COMMENT section, with markdown body and action buttons.
PR is the enclosing pull request, needed for the reply/resolve
actions.  DEPTH (default 0) indents the whole comment to visualise
reply threads.  BY-ID, when given, maps comment id -> comment so a
reply can be collapsed along with the thread it belongs to (see
`gp--comment-thread-resolved-p').

Resolved-ness and collapsing are deliberately separate: only the
comment the resolve action targeted is actually resolved -- Bitbucket
cannot resolve or unresolve a reply -- so a reply keeps its own
\(unresolved) status and glyph while still folding away with its
thread."
  (let* ((depth (or depth 0))
         (ind (make-string (* depth 4) ?\s))
         (resolved (gp-comment-resolved-p comment))
         (collapse (if by-id
                       (gp--comment-thread-resolved-p comment by-id)
                     resolved))
         (outdated (gp-comment-outdated-p comment gp--detail-diff))
         (marked (and pr (gp--detail-comment-marked-p comment)))
         ;; prefix every line of STR with the thread indent
         (pad (lambda (str) (replace-regexp-in-string "^" ind str))))
    ;; resolved threads start collapsed (HIDE arg); TAB expands them
    (magit-insert-section (gp-comment-section comment collapse)
      (let ((start (point)))
        (let-alist comment
          (progn
          (magit-insert-heading
            (concat
             ind
             (if (> depth 0) (propertize "↳ " 'face 'shadow) "")
             (gp--avatar-string .user.links.avatar.href
                                       (if resolved "✅" "💬"))
             " "
             (propertize (or .user.display_name "?")
                         'face 'gp-comment-author-face)
             "  "
             (propertize (gp--comment-location comment)
                         'face 'gp-branch-face)
             (let ((ts (or .created_on .updated_on)))
               (if ts (propertize (concat "  " (gp--relative-time ts))
                                  'face 'shadow)
                 ""))
             (if resolved (propertize "  ✓ resolved" 'face 'success) "")
             (if outdated (propertize "  ⊘ outdated" 'face 'shadow) "")))
          (when (and pr .inline.path)
            (insert ind "  ")
            (gp--insert-action-button
             (format "📂 %s:%s" .inline.path (or .inline.to .inline.from "?"))
             "Open this file at the commented line in the local checkout"
             (lambda () (gp-ui-goto-comment-file pr comment)))
            (insert "\n"))
          (when-let* ((url (let-alist comment .links.html.href)))
            (insert ind "  ")
            (gp--insert-link url "↗ view in browser [w]")
            (insert "\n"))
          (let ((body (string-trim-right
                       (gp--render-markdown (or .content.raw "")))))
            (insert (funcall pad (replace-regexp-in-string "^" "  " body)) "\n"))
          (when pr
            (insert ind "  ")
            (gp--insert-action-button
             "reply [R]" "Reply to this comment"
             (lambda () (gp-ui-reply-comment pr comment)))
            (insert " ")
            (gp--insert-action-button
             (if marked "unmark [m]" "mark [m]")
             "Toggle whether this comment participates in batch terminal handoff"
             (lambda () (gp-detail-toggle-mark)))
            (insert " ")
            (gp--insert-action-button
             "send to terminal [t]"
             "Send this comment to the matching AI terminal session"
             (lambda () (gp-ui-send-comment-to-terminal pr comment)))
            (when (gp-comment-resolvable-p comment)
              (insert " ")
              (let ((tag (cons 'resolution (alist-get 'id comment)))
                    (buf (current-buffer)))
                (if resolved
                    (gp--insert-action-button/spinner
                     tag "reopen [X]" "Reopen this comment on the PR"
                     (lambda () (gp--detail-run-action
                                 buf tag (lambda () (gp-ui-set-resolution pr comment nil)))))
                  (gp--insert-action-button/spinner
                   tag "resolve [X]" "Resolve this comment on the PR"
                   (lambda () (gp--detail-run-action
                               buf tag (lambda () (gp-ui-set-resolution pr comment t))))))))
            ;; editing is always own-only (no API lets you rewrite someone
            ;; else's words); deleting can be granted more widely -- see
            ;; `gp-comment-delete-others'.
            (let ((uuid (gp-user-uuid)))
              (when (gp-comment-own-p comment uuid)
                (insert " ")
                (gp--insert-action-button
                 "edit [e]" "Edit this comment"
                 (lambda () (gp-ui-edit-comment pr comment))))
              (when (gp-comment-deletable-p comment uuid)
                (insert " ")
                (gp--insert-action-button
                 "delete [K]" "Delete this comment"
                 (lambda () (gp-ui-delete-comment pr comment)))))
            (insert "\n"))
          (insert "\n"))
          (when marked
            (add-face-text-property start (point) 'gp-comment-marked-face t)))))))

(defun gp--insert-link (url &optional label)
  "Insert URL as a clickable button (showing LABEL, default URL)."
  (when (and url (not (string-empty-p url)))
    (insert-button (or label url)
                   'face 'gp-link-face
                   'follow-link t
                   'help-echo (format "Open %s in browser" url)
                   'action (lambda (_b) (browse-url url)))))

(defun gp--insert-action-button (label help fn)
  "Insert a clickable button LABEL with HELP that calls FN (no args)."
  (insert-button label
                 'face 'gp-link-face
                 'follow-link t
                 'help-echo help
                 'action (lambda (_b) (funcall fn))))

(defun gp--insert-action-button/spinner (tag label help fn)
  "Insert a button, or a spinner in its place while TAG is pending.
Same slot, same width class as a plain `gp--insert-action-button' --
just enough to avoid the layout shifting when the mutation this
button fires (via `gp--detail-run-action') is in flight.  TAG
identifies this button -- a symbol for a PR-level action (`draft',
`approve', …), or a compound value like (resolution . COMMENT-ID)
for a per-comment action, since several comments render in the same
buffer and each needs its own independent pending state.  Compared
with `equal' against `gp--detail-pending-action', not `eq': a
compound tag is a fresh cons on every render, so `eq' would never
match across the render that sets the flag and the one reading it."
  (if (equal gp--detail-pending-action tag)
      (insert (propertize "⏳" 'face 'shadow))
    (gp--insert-action-button label help fn)))

(defun gp--detail-run-action (buf tag thunk)
  "Run THUNK with BUF's button TAG showing a spinner meanwhile.
Redraws BUF once immediately so the spinner appears before the
\(blocking\) THUNK runs, then clears the pending flag once THUNK
returns \(or signals\) -- the caller is expected to trigger the real
post-mutation redraw itself right after \(e.g. via `gp-detail-refresh'
or `gp-invalidate-pr-caches' + a manual rerender\), same as today."
  (with-current-buffer buf
    (setq gp--detail-pending-action tag)
    (gp--detail-rerender buf))
  (unwind-protect
      (funcall thunk)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq gp--detail-pending-action nil)))))

(defcustom gp-detail-files-collapsed nil
  "When non-nil, the changed-files section starts collapsed."
  :type 'boolean :group 'bitbucket)

(defcustom gp-detail-show-file-diffs t
  "When non-nil, each changed file is expandable to show its diff inline."
  :type 'boolean :group 'bitbucket)

(defun gp--fontify-diff (text)
  "Return diff TEXT fontified as in `diff-mode'."
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (diff-mode))
    (font-lock-ensure)
    (buffer-string)))

(defcustom gp-detail-description-collapsed nil
  "When non-nil, the PR description section starts collapsed."
  :type 'boolean :group 'bitbucket)

(defun gp--insert-description (pr)
  "Insert a collapsable description section for PR, if it has one.
Rendered through `gp--render-markdown' so it gets the same emoji,
mention and link handling as comment bodies.  Backends disagree on
the field name (Bitbucket `description', GitHub `body'), so the value
comes from `gp-pr-description' rather than the alist directly."
  (let ((desc (ignore-errors (gp-pr-description pr))))
    (when desc
      (magit-insert-section (gp-description nil gp-detail-description-collapsed)
        (magit-insert-heading "Description")
        (let ((start (point)))
          (insert (gp--render-markdown desc))
          (unless (bolp) (insert "\n"))
          ;; indent the body so it reads as belonging to the heading
          (indent-rigidly start (point) 2))
        (insert "\n")))))

(defun gp--insert-changed-files ()
  "Insert a collapsable changed-files section for the detail buffer's PR.
Each file is a single line inside the section so `p' and `n' can skip the whole
block at once.  The file name remains clickable and opens the checkout."
  (let ((files (plist-get gp--detail-stats :file-list)))
    (when files
      (magit-insert-section (gp-files nil gp-detail-files-collapsed)
        (magit-insert-heading (format "Changed files (%d)" (length files)))
        (dolist (f files)
          (let ((path (plist-get f :path)))
            (insert "  ")
            (insert-text-button path
                                'face 'gp-link-face
                                'follow-link t
                                'help-echo "RET/mouse-1: open in checkout"
                                'gp-file-path path
                                'action (lambda (_button)
                                          (gp-ui-open-file gp--pr path)))
            (insert "  ")
            (insert (propertize (format "+%d" (plist-get f :added)) 'face 'diff-added))
            (insert " ")
            (insert (propertize (format "-%d" (plist-get f :removed)) 'face 'diff-removed))
            (let ((st (plist-get f :status)))
              (when (member st '("added" "removed" "renamed"))
                (insert (propertize (format "  (%s)" st) 'face 'shadow))))
            (insert "\n")))
        (insert "\n")))))

(defcustom gp-detail-commits-collapsed nil
  "When non-nil, the commits section starts collapsed."
  :type 'boolean :group 'bitbucket)

(defcustom gp-detail-max-commits 50
  "Maximum number of PR commits to fetch for the detail view.
A long-running branch can carry hundreds; the section is a
navigation aid, not a full history (that is what `l' in the magit
checkout is for).  Set to nil for no cap."
  :type '(choice (integer :tag "At most this many")
                 (const :tag "No limit" nil))
  :group 'bitbucket)

(defun gp--insert-commits ()
  "Insert a collapsable commits section for the detail buffer's PR.
Each commit is one line -- short hash, summary, author, relative date
-- carrying its plist as the section value so `RET' can open just that
commit in Magit (see `gp-detail-show-commit')."
  (let ((commits gp--detail-commits))
    (when commits
      (magit-insert-section (gp-commits nil gp-detail-commits-collapsed)
        (magit-insert-heading (format "Commits (%d)" (length commits)))
        (dolist (c commits)
          (magit-insert-section (gp-commit-section c)
            (let ((hash (plist-get c :hash))
                  (author (plist-get c :author))
                  (date (plist-get c :date)))
              (insert "  ")
              (insert (propertize (gp--short-hash hash) 'face 'magit-hash))
              (insert "  ")
              (insert (or (plist-get c :summary) ""))
              (when author
                (insert (propertize (format "  — %s" author) 'face 'gp-author-face)))
              (when date
                (insert (propertize (format "  %s" (gp--relative-time date))
                                    'face 'shadow)))
              (insert "\n"))))
        (insert "\n")))))

(defun gp--short-hash (hash)
  "Return the leading 8 characters of HASH (or all of it when shorter)."
  (if (and hash (> (length hash) 8)) (substring hash 0 8) (or hash "")))

(defun gp-detail-show-commit ()
  "Show the commit at point in Magit, in the PR's local checkout.
Opens a single-commit revision buffer -- the commit's own diff and
message -- rather than the whole-PR diff `d' gives.  Needs the branch
checked out locally, and the commit to exist there: a freshly-pushed
commit that the local clone has not fetched yet is reported rather
than silently showing nothing."
  (interactive)
  (let* ((sec (magit-current-section))
         (commit (and sec (object-of-class-p sec 'gp-commit-section)
                      (oref sec value)))
         (hash (and commit (plist-get commit :hash))))
    (unless hash (user-error "Point is not on a commit"))
    (unless (require 'magit nil t) (user-error "Magit is not available"))
    (let* ((dir (gp-local-ensure-checkout gp--pr))
           (default-directory (file-name-as-directory dir)))
      (if (magit-rev-verify hash)
          (magit-show-commit hash)
        ;; not fetched yet -- fetch just this branch, then retry once
        (message "Commit %s not in the local clone; fetching…" (gp--short-hash hash))
        (if (and (zerop (call-process "git" nil nil nil
                                      "fetch" gp-checkout-remote
                                      (gp-pr-source-branch gp--pr)))
                 (magit-rev-verify hash))
            (magit-show-commit hash)
          (user-error "Commit %s is not in the local clone (try `b' to check out the branch)"
                      (gp--short-hash hash)))))))

(defun gp-ui-open-file (pr path)
  "Open PATH from PR's checked-out branch (cloning/switching if needed)."
  (let ((dir (gp-local-ensure-checkout pr)))
    (find-file (expand-file-name path dir))
    (when (fboundp 'gp-overlay-pr)
      (ignore-errors (gp-overlay-pr pr)))))

(defun gp-detail-visit-file ()
  "Open the changed file at point (detail buffer)."
  (interactive)
  (if-let* ((button (button-at (point)))
            (path (button-get button 'gp-file-path)))
      (gp-ui-open-file gp--pr path)
    (user-error "Point is not on a changed file")))

(defun gp--reviewer-state-badge (state)
  "Return a propertized one-glyph badge for a reviewer plist's STATE.
STATE is `approved', `changes', or `pending' (see `gp-pr-reviewers-async')."
  (pcase state
    ('approved (propertize "✅" 'face 'success))
    ('changes (propertize "❌" 'face 'error))
    (_ (propertize "⏳" 'face 'shadow))))

(defun gp--insert-reviewers-line (reviewers &optional pr)
  "Insert a line listing REVIEWERS and their approval state.
REVIEWERS is the list of plists from `gp-pr-reviewers-async'
\(cached in `gp--detail-reviewers').  With PR and an open PR, an
edit button follows -- shown even when REVIEWERS is empty, since
that is exactly when you need a way to add the first one."
  (let ((editable (and pr (gp-pr-open-p pr))))
    (when (or reviewers editable)
      (insert "👥 ")
      (if reviewers
          (insert (mapconcat
                   (lambda (r)
                     (concat (gp--reviewer-state-badge (plist-get r :state))
                             " "
                             (propertize (or (plist-get r :name) "?")
                                         'face 'gp-author-face)))
                   reviewers "   "))
        (insert (propertize "no reviewers" 'face 'shadow)))
      (when editable
        (insert "   ")
        (gp--insert-action-button
         "✎ edit [V]" "Add or remove reviewers on this PR"
         (lambda () (gp-ui-edit-reviewers pr))))
      (insert "\n"))))

(defun gp--insert-labels-line (pr)
  "Insert PR's labels as a line in the detail buffer's top section.
Nothing at all is inserted on a platform without labels: Bitbucket
users get no empty \"no labels\" slot that could never fill, which is
why this asks `gp-labels-supported-p' rather than just checking
whether PR happens to carry any.  Where they are supported an edit
button follows on open PRs -- shown even with no labels yet, since
that is when you need a way to add the first one."
  (when (gp-labels-supported-p)
    (let ((labels (gp-pr-labels pr))
          (editable (gp-pr-open-p pr)))
      (when (or labels editable)
        (insert "🏷 ")
        (if labels
            (insert (gp--format-labels labels))
          (insert (propertize "no labels" 'face 'shadow)))
        (when editable
          (insert "   ")
          (gp--insert-action-button
           "✎ edit [L]" "Add or remove labels on this PR"
           (lambda () (gp-ui-edit-labels pr))))
        (insert "\n")))))

(defun gp--render-detail (pr comments)
  "Render PR and its COMMENTS into the current detail buffer."
  (require 'button)
  (magit-insert-section (gp-root)
    (let-alist pr
      (insert (propertize
               (concat (cond ((gp-pr-draft-p pr) "📝 ")
                             ((gp-pr-merged-p pr) "🟣 ")
                             ((not (gp-pr-open-p pr)) "🔴 ")
                             (t "🟢 "))
                       (format "#%s" .id))
               'face 'gp-pr-id-face))
      (insert "  ")
      (insert (propertize (or .title "(no title)")
                          'face 'gp-detail-title-face))
      (insert "\n")
      (insert "🔀 "
              (propertize (format "%s → %s"
                                  (or (gp-pr-source-branch pr) "?")
                                  (or (gp-pr-destination-branch pr) "?"))
                          'face 'gp-branch-face)
              "    "
              (gp--avatar-string (gp-pr-author-avatar pr) "👤")
              " "
              (propertize (or (gp-pr-author-name pr) "?")
                          'face 'gp-author-face))
      (insert "\n")
      (when (or .created_on .updated_on)
        (insert "🕓 "
                (propertize
                 (concat
                  (when .created_on
                    (format "created %s" (gp--format-date .created_on)))
                  (when (and .created_on .updated_on) "  ·  ")
                  (when .updated_on
                    (format "updated %s (%s)"
                            (gp--format-date .updated_on)
                            (gp--relative-time .updated_on))))
                 'face 'shadow)
                "\n"))
      (when gp--detail-stats
        (let ((s gp--detail-stats))
          (insert "📊 "
                  (propertize (format "%d commit%s" (plist-get s :commits)
                                      (if (= 1 (plist-get s :commits)) "" "s"))
                              'face 'shadow)
                  "  "
                  (propertize (format "%d file%s" (plist-get s :files)
                                      (if (= 1 (plist-get s :files)) "" "s"))
                              'face 'shadow)
                  "  "
                  (propertize (format "+%d" (plist-get s :added)) 'face 'diff-added)
                  " "
                  (propertize (format "-%d" (plist-get s :removed)) 'face 'diff-removed)
                  "\n")))
      (gp--insert-reviewers-line gp--detail-reviewers pr)
      (gp--insert-labels-line pr)
      (insert "\n")
      (gp--insert-action-button
       "← Back [b]" "Return to the pull-request list"
       (lambda () (gp-ui-back-to-list)))
      (insert "   ")
      (gp--insert-action-button
       "⎇ Open local repo [o]"
       "Open the current repo in Magit without changing branches"
       (lambda () (gp-detail-open-local)))
      (insert "   ")
      (gp--insert-action-button
       "📦 Autostash & checkout [O]"
       "Stash any local changes and check out the PR branch, then open the repo (unlike 'd', which only shows the diff)"
       (lambda () (gp-ui-open-in-ide pr)))
      (insert "   ")
      (gp--insert-action-button
       "🔍 Show diff in Magit [d]" "Checkout the branch and show its diff in Magit"
       (lambda () (gp-ui-show-diff-in-magit pr)))
      (insert "   ")
      (gp--insert-link .links.html.href "🔗 View in browser [w]")
      (when gp--detail-marked-comment-ids
        (insert "   ")
        (gp--insert-action-button
         (format "📤 Send %d marked [t]" (length gp--detail-marked-comment-ids))
         "Send all marked comments to the matching AI terminal session"
         (lambda () (gp-detail-send-to-terminal))))
      ;; draft toggle, only on the user's own open PRs
      (when (and (gp-pr-authored-by-p pr (gp-user-uuid))
                 (gp-pr-open-p pr))
        (insert "   ")
        (let ((buf (current-buffer)))
          (if (gp-pr-draft-p pr)
              (gp--insert-action-button/spinner
               'draft "✅ Mark ready [D]" "Mark this draft PR as ready for review"
               (lambda () (gp--detail-run-action
                           buf 'draft (lambda () (gp-ui-set-draft pr nil)))))
            (gp--insert-action-button/spinner
             'draft "📝 Convert to draft [D]" "Convert this PR back to a draft"
             (lambda () (gp--detail-run-action
                         buf 'draft (lambda () (gp-ui-set-draft pr t))))))))
      ;; review actions, only on others' open PRs you can review
      (when (and (gp-pr-open-p pr)
                 (not (gp-pr-authored-by-p pr (gp-user-uuid))))
        (let* ((mine (gp-pr-my-review-state pr (gp-user-uuid)))
               (dismiss (eq (gp-review-retraction-kind) 'dismiss))
               (buf (current-buffer)))
          (insert "\n   ")
          (if (eq mine 'approved)
              (gp--insert-action-button/spinner
               'approve
               (if dismiss "↩ Dismiss approval [a]" "↩ Unapprove [a]")
               (if dismiss
                   "Dismiss your approval (stays visible on the PR timeline with a reason)"
                 "Retract your approval of this PR")
               (lambda () (gp--detail-run-action
                           buf 'approve (lambda () (gp-ui-set-review pr 'approved t)))))
            (gp--insert-action-button/spinner
             'approve "✅ Approve [a]" "Approve this pull request"
             (lambda () (gp--detail-run-action
                         buf 'approve (lambda () (gp-ui-set-review pr 'approved nil))))))
          (insert "   ")
          (if (eq mine 'changes)
              (gp--insert-action-button/spinner
               'changes
               (if dismiss "↩ Dismiss request [c]" "↩ Clear request [c]")
               (if dismiss
                   "Dismiss your changes-requested review (stays visible on the PR timeline with a reason)"
                 "Retract your request for changes")
               (lambda () (gp--detail-run-action
                           buf 'changes (lambda () (gp-ui-set-review pr 'changes t)))))
            (gp--insert-action-button/spinner
             'changes "🚫 Request changes [c]" "Request changes on this pull request"
             (lambda () (gp--detail-run-action
                         buf 'changes (lambda () (gp-ui-set-review pr 'changes nil))))))))
      (insert "\n\n"))
    (gp--insert-description pr)
    (gp--insert-changed-files)
    (gp--insert-commits)
    (gp--insert-pipelines gp--detail-pipelines)
    (magit-insert-section (gp-comments)
      (magit-insert-heading
        (format "Comments (%d)" (length comments)))
      (if comments
          (let ((by-id (gp--comments-by-id comments)))
            (pcase-dolist (`(,c . ,depth) (gp--comment-threads comments))
              (gp--insert-comment c pr depth by-id)))
        (insert "  (no comments)\n")))))

;;;; Modes -------------------------------------------------------------------

(defvar-keymap gp-list-mode-map
  :parent magit-section-mode-map
  "g"   #'gp-refresh
  "G"   #'gp-refresh-full
  "RET" #'gp-visit-pr
  "o"   #'gp-open-local
  "b"   #'gp-checkout-branch
  "w"   #'gp-browse-pr
  "?"   #'gp-dispatch)

(define-derived-mode gp-list-mode magit-section-mode "PRs"
  "Major mode for the pull-request list.")

(defvar-keymap gp-detail-mode-map
  :parent magit-section-mode-map
  "g"   #'gp-detail-refresh
  "b"   #'gp-ui-back-to-list
  "o"   #'gp-detail-open-local
  "O"   #'gp-detail-open-in-ide
  ;; Comment actions that write to the PR are capitalised, so a stray
  ;; lowercase keypress while reading can't mutate anything.
  "R"   #'gp-detail-reply
  "t"   #'gp-detail-send-to-terminal
  "X"   #'gp-detail-resolve
  "e"   #'gp-detail-edit
  "f"   #'gp-detail-goto-comment
  "d"   #'gp-detail-show-diff
  "K"   #'gp-detail-delete        ;; delete a comment (see `gp-comment-delete-others')
  "D"   #'gp-detail-toggle-draft
  "V"   #'gp-detail-edit-reviewers  ;; add / remove reviewers (open PRs)
  "L"   #'gp-detail-edit-labels     ;; add / remove labels (open PRs, GitHub)
  "a"   #'gp-detail-approve         ;; approve / unapprove (others' open PRs)
  "c"   #'gp-detail-request-changes ;; request changes / clear (others' open PRs)
  "RET" #'gp-detail-ret
  "v"   #'gp-detail-show-commit     ;; read-only, so lowercase (see `R'/`X'/`K')
  "w"   #'gp-detail-browse
  ;; pipelines (pipeline-level stop/trigger; per-step log + manual run)
  "s"   #'gp-detail-pipeline-stop
  "T"   #'gp-detail-pipeline-trigger-or-run-manual
  "P"   #'gp-detail-pipeline-rerun-step
  "m"   #'gp-detail-toggle-mark
  "l"   #'gp-detail-pipeline-step-log)

(defun gp-detail-edit-reviewers ()
  "Add or remove reviewers on the PR shown in this buffer."
  (interactive)
  (gp-ui-edit-reviewers gp--pr))

(defun gp-detail-show-diff ()
  "Show the current PR's branch diff in Magit."
  (interactive)
  (gp-ui-show-diff-in-magit gp--pr))

(defun gp-detail-delete ()
  "Delete the comment at point.
Your own comments always; anyone's when `gp-comment-delete-others'
grants it for the active backend."
  (interactive)
  (let ((c (gp-detail--comment-at-point)))
    (unless (gp-comment-deletable-p c (gp-user-uuid))
      (user-error
       "You can only delete your own comments (see `gp-comment-delete-others')"))
    (gp-ui-delete-comment gp--pr c)))

(defun gp-detail--comment-at-point ()
  "Return the comment at point in the detail buffer, or signal."
  (let ((sec (magit-current-section)))
    (if (and sec (object-of-class-p sec 'gp-comment-section))
        (oref sec value)
      (user-error "Point is not on a comment"))))

(defun gp-detail-reply ()
  "Reply to the comment at point."
  (interactive)
  (gp-ui-reply-comment gp--pr (gp-detail--comment-at-point)))

(defun gp-detail-send-to-terminal ()
  "Send marked comments, or the comment at point, to the terminal session."
  (interactive)
  (if-let* ((comments (gp--detail-marked-comments)))
      (progn
        (gp-ui-send-comments-to-terminal gp--pr comments)
        (setq gp--detail-marked-comment-ids nil)
        (gp--detail-rerender (current-buffer)))
    (gp-ui-send-comment-to-terminal gp--pr (gp-detail--comment-at-point))))

(defun gp-detail-toggle-mark ()
  "Toggle whether the comment at point is marked for batch handoff."
  (interactive)
  (let* ((comment (gp-detail--comment-at-point))
         (id (alist-get 'id comment)))
    (if (memq id gp--detail-marked-comment-ids)
        (setq gp--detail-marked-comment-ids (delq id gp--detail-marked-comment-ids))
      (push id gp--detail-marked-comment-ids))
    (gp--detail-rerender (current-buffer))
    (message "%s comment #%s"
             (if (memq id gp--detail-marked-comment-ids) "Marked" "Unmarked") id)))

(defun gp-detail-resolve ()
  "Toggle resolve/reopen on the comment at point."
  (interactive)
  (let ((c (gp-detail--comment-at-point)))
    (gp-ui-set-resolution
     gp--pr c (not (gp-comment-resolved-p c)))))

(defun gp-detail-edit ()
  "Edit the comment at point (must be your own)."
  (interactive)
  (let ((c (gp-detail--comment-at-point)))
    (unless (gp-comment-own-p c (gp-user-uuid))
      (user-error "You can only edit your own comments"))
    (gp-ui-edit-comment gp--pr c)))

(defun gp-detail-browse ()
  "Browse the comment at point, or the PR when point is elsewhere."
  (interactive)
  (let ((sec (magit-current-section)))
    (if (and sec (object-of-class-p sec 'gp-comment-section))
        (let ((url (let-alist (oref sec value) .links.html.href)))
          (if url
              (browse-url url)
            (user-error "No URL for this comment")))
      (gp-browse-pr))))

(defun gp-detail-ret ()
  "Context action: open a changed file or commit, jump to a comment, else fold."
  (interactive)
  (let ((sec (magit-current-section)))
    (cond
     ((and (button-at (point))
           (button-get (button-at (point)) 'gp-file-path))
      (gp-detail-visit-file))
     ((and sec (object-of-class-p sec 'gp-commit-section))
      (gp-detail-show-commit))
     ((and sec (object-of-class-p sec 'gp-comment-section)
           (let-alist (oref sec value) .inline.path))
      (gp-detail-goto-comment))
     (t (call-interactively #'magit-section-toggle)))))

(define-derived-mode gp-detail-mode magit-section-mode "PR"
  "Major mode for a single Bitbucket pull request."
  ;; We apply all faces ourselves (links, markdown, etc.).  Any font-lock
  ;; activation clears those manual `face' text properties on redisplay, and
  ;; magit's current-section highlight overrides them -- disable both.
  (setq-local magit-section-highlight-current nil)
  (setq-local font-lock-defaults nil)
  (font-lock-mode -1)
  (when (fboundp 'jit-lock-mode) (jit-lock-mode nil))
  ;; magit-section-mode turns on `truncate-lines'; soft-wrap instead so
  ;; long comment bodies stay fully readable without horizontal scroll.
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (add-hook 'kill-buffer-hook #'gp--detail-cancel-pipeline-timer nil t)
  ;; Polling stops when the buffer leaves the screen, so resume on return.
  (add-hook 'window-selection-change-functions
            #'gp--detail-maybe-resume-pipelines nil t))

(defun gp-detail-open-in-ide ()
  "Open the current detail buffer's PR in the IDE."
  (interactive)
  (gp-ui-open-in-ide gp--pr))

(defun gp-detail-open-local ()
  "Open the current detail buffer's repo in Magit without switching branches."
  (interactive)
  (unless (require 'magit nil t)
    (user-error "Magit is not available"))
  (let* ((full-name (gp-pr-full-name gp--pr))
         ;; `default-directory' is already anchored to the checkout by
         ;; `gp-show-pr', but re-resolve: it falls back to an inherited
         ;; directory when no clone existed at paint time, and one may
         ;; have appeared since.
         (dir (gp-local-find-checkout full-name)))
    (unless dir
      (user-error "No local checkout of %s under %s"
                  full-name gp-local-git-root))
    (magit-status dir)))

;;;; Section accessors -------------------------------------------------------

(defun gp-current-pr ()
  "Return the PR at point (from a PR section or the detail buffer)."
  (or (let ((sec (magit-current-section)))
        (when (and sec (object-of-class-p sec 'gp-pr-section))
          (oref sec value)))
      gp--pr
      (user-error "No pull request at point")))

;;;; Detail-view actions ------------------------------------------------------

(defun gp-ui-reply-comment (pr comment)
  "Open a compose buffer replying to COMMENT on PR, refreshing on success."
  (require 'gp-compose)
  (let-alist comment
    (gp-compose
     (list :full-name (gp-pr-full-name pr)
           :id (alist-get 'id pr)
           :parent (alist-get 'id comment)
           :inline (when .inline.path
                     (cons .inline.path (or .inline.to .inline.from)))
           :on-success (lambda (_c)
                         (gp-invalidate-pr-caches pr)
                         (when (buffer-live-p (get-buffer (gp--detail-buffer-name pr)))
                           (with-current-buffer (gp--detail-buffer-name pr)
                             (gp-detail-refresh))))))))

(defun gp-ui-send-comment-to-terminal (pr comment)
  "Send COMMENT on PR to the configured AI terminal session."
  (require 'gp-helm-terminal)
  (gp-helm-terminal-send-comment pr comment))

(defun gp-ui-send-comments-to-terminal (pr comments)
  "Send COMMENTS on PR to the configured AI terminal session as one batch."
  (require 'gp-helm-terminal)
  (gp-helm-terminal-send-comments pr comments))

(defun gp-ui-edit-reviewers (pr)
  "Open the reviewer-editing form for PR.
Passes the reviewers this buffer already loaded, so the form opens
without a second fetch."
  (require 'gp-reviewers)
  (unless (gp-pr-open-p pr)
    (user-error "Only open pull requests can have their reviewers changed"))
  (gp-reviewers-edit pr gp--detail-reviewers))

(defun gp-ui-edit-labels (pr)
  "Set PR's labels from the minibuffer, then refresh the detail buffer.
Prompts with the repo's whole label pool for completion and the PR's
current labels pre-filled, so the edit reads as \"adjust this set\":
leaving the default untouched is a no-op, deleting a name drops that
label, and typing one adds it.  Only names in the pool are accepted --
GitHub would silently *create* an unknown label on the repo, which is
never what a typo means.

`completing-read-multiple' rather than the checkbox form
`gp-ui-edit-reviewers' opens, because labels are short plain strings
with no per-item state to display or lock."
  (unless (gp-labels-supported-p)
    (user-error "%s pull requests do not support labels" (gp-backend-name)))
  (unless (gp-pr-open-p pr)
    (user-error "Only open pull requests can have their labels changed"))
  (let* ((full-name (gp-pr-full-name pr))
         (id (alist-get 'id pr))
         (pool (mapcar (lambda (l) (plist-get l :name)) (gp-repo-labels full-name)))
         (current (mapcar (lambda (l) (plist-get l :name)) (gp-pr-labels pr)))
         ;; the pool can legitimately be empty (a repo with no labels defined);
         ;; still offer the current ones so an unwanted label can be removed
         (candidates (delete-dups (append pool current)))
         (chosen (completing-read-multiple
                  (format "Labels for #%s (comma-separated): " id)
                  candidates nil t
                  (when current (concat (mapconcat #'identity current ",") ","))))
         (unknown (cl-remove-if (lambda (n) (member n candidates)) chosen)))
    (when unknown
      (user-error "No such label in %s: %s" full-name
                  (mapconcat #'identity unknown ", ")))
    (if (equal (sort (copy-sequence chosen) #'string<)
               (sort (copy-sequence current) #'string<))
        (message "Labels unchanged")
      (gp-set-pull-request-labels full-name id chosen)
      (message "Labels on #%s: %s" id
               (if chosen (mapconcat #'identity chosen ", ") "(none)"))
      (gp-invalidate-pr-caches pr)
      (gp-detail-refresh))))

(defun gp-detail-edit-labels ()
  "Add or remove labels on the PR shown in this buffer."
  (interactive)
  (gp-ui-edit-labels gp--pr))

(defun gp-ui-set-resolution (pr comment resolve)
  "Resolve (RESOLVE non-nil) or reopen COMMENT on PR, then refresh the buffer."
  (unless (gp-comment-resolvable-p comment)
    (user-error "This comment cannot be resolved/reopened"))
  (let ((full-name (gp-pr-full-name pr))
        (pid (alist-get 'id pr))
        (cid (alist-get 'id comment)))
    (if resolve
        (gp-resolve-comment full-name pid cid)
      (gp-reopen-comment full-name pid cid))
    (message "Comment %s" (if resolve "resolved" "reopened"))
    (gp-invalidate-pr-caches pr)
    (gp-detail-refresh)))

(defun gp-ui-edit-comment (pr comment)
  "Edit COMMENT on PR: compose prefilled with its text, PUT on submit."
  (require 'gp-compose)
  (let ((full-name (gp-pr-full-name pr))
        (pid (alist-get 'id pr))
        (cid (alist-get 'id comment))
        (buf (gp--detail-buffer-name pr)))
    (gp-compose
     (list :full-name full-name :id pid
           :initial-text (let-alist comment (or .content.raw ""))
           :submit-function
           (lambda (fn _id text _inline _parent)
             (gp-edit-comment fn pid cid text))
           :on-success
           (lambda (_c)
             (when (buffer-live-p (get-buffer buf))
               (with-current-buffer buf (gp-detail-refresh))))))))

(defun gp-ui-delete-comment (pr comment)
  "Delete COMMENT on PR after confirmation, then refresh.
Someone else's comment names its author in the prompt -- deleting
those is a privilege, and a misfire is not undoable."
  (when (yes-or-no-p
         (if (gp-comment-own-p comment (gp-user-uuid))
             "Delete this comment? "
           (format "Delete %s's comment? "
                   (let-alist comment (or .user.display_name "another user")))))
    (gp-delete-comment (gp-pr-full-name pr)
                              (alist-get 'id pr)
                              (alist-get 'id comment))
    (message "Comment deleted")
    (gp-invalidate-pr-caches pr)
    (gp-detail-refresh)))

(defun gp-ui-goto-comment-file (pr comment)
  "Open the local file for COMMENT's inline location in PR, at its line."
  (let-alist comment
    (unless .inline.path
      (user-error "This comment is not anchored to a file line"))
    (let* ((dir (gp-local-ensure-checkout pr))
           (file (expand-file-name .inline.path dir)))
      (unless (file-exists-p file)
        (user-error "File %s not found in the checkout (different branch?)"
                    .inline.path))
      (find-file file)
      (when-let* ((line (or .inline.to .inline.from)))
        (goto-char (point-min))
        (forward-line (1- line)))
      (when (fboundp 'gp-overlay-pr)
        (ignore-errors (gp-overlay-pr pr))))))

(defun gp-detail-goto-comment ()
  "From point on a comment in the detail buffer, jump to its file:line."
  (interactive)
  (let ((sec (magit-current-section)))
    (if (and sec (object-of-class-p sec 'gp-comment-section))
        (gp-ui-goto-comment-file gp--pr (oref sec value))
      (user-error "Point is not on a comment"))))

(defun gp-ui-set-draft (pr draft)
  "Set PR's draft flag to DRAFT, then refresh the detail buffer."
  (let-alist pr
    (gp-set-pull-request-draft
     (gp-pr-full-name pr) .id draft .title))
  (message "PR #%s %s" (alist-get 'id pr)
           (if draft "converted to draft" "marked ready for review"))
  (gp-invalidate-pr-caches pr)
  (gp-detail-refresh))

(defun gp-ui-set-review (pr kind retract)
  "Set your review on PR.
KIND is `approved' or `changes'; RETRACT non-nil withdraws it.
When withdrawing and `gp-review-retraction-kind' reports `dismiss'
\(GitHub: the withdrawal is a visible timeline event, not a silent
retraction), prompts for a dismissal reason first.  Refreshes the
detail buffer afterwards."
  (let* ((full-name (gp-pr-full-name pr))
         (id (alist-get 'id pr))
         (dismiss (and retract (eq (gp-review-retraction-kind) 'dismiss)))
         (reason (when dismiss
                   (read-string "Dismissal reason (shown on the PR): "))))
    (pcase kind
      ('approved (gp-approve-pr full-name id retract reason))
      ('changes  (gp-request-changes-pr full-name id retract reason)))
    (message "PR #%s %s" id
             (pcase (list kind retract dismiss)
               (`(approved nil ,_) "approved")
               (`(approved t t)    "approval dismissed")
               (`(approved t ,_)   "approval retracted")
               (`(changes nil ,_)  "changes requested")
               (`(changes t t)     "changes-request dismissed")
               (`(changes t ,_)    "changes-request cleared")))
    (gp-invalidate-pr-caches pr)
    (gp-detail-refresh)))

(defun gp-detail-approve ()
  "Approve the current PR, or withdraw if you already approved it.
Withdrawal reads as \"unapprove\" or \"dismiss\" depending on what
the active backend actually supports -- see `gp-review-retraction-kind'."
  (interactive)
  (let ((pr gp--pr))
    (when (gp-pr-authored-by-p pr (gp-user-uuid))
      (user-error "You cannot approve your own PR"))
    (gp-ui-set-review pr 'approved
                      (eq (gp-pr-my-review-state pr (gp-user-uuid)) 'approved))))

(defun gp-detail-request-changes ()
  "Request changes on the current PR, or withdraw if you already did."
  (interactive)
  (let ((pr gp--pr))
    (when (gp-pr-authored-by-p pr (gp-user-uuid))
      (user-error "You cannot request changes on your own PR"))
    (gp-ui-set-review pr 'changes
                      (eq (gp-pr-my-review-state pr (gp-user-uuid)) 'changes))))

(defun gp-detail-toggle-draft ()
  "Toggle draft/ready on the current PR (must be your own)."
  (interactive)
  (let ((pr gp--pr))
    (unless (gp-pr-authored-by-p pr (gp-user-uuid))
      (user-error "You can only change the draft status of your own PRs"))
    (gp-ui-set-draft pr (not (gp-pr-draft-p pr)))))

(defun gp-ui-open-in-ide (pr)
  "Ensure PR's branch is checked out, then open the repo (magit)."
  (let* ((res (gp-local-checkout-branch pr))
         (dir (plist-get res :dir)))
    (if (and (plist-get res :ok) dir)
        (progn
          (message "On branch in %s%s" dir
                   (if (plist-get res :stashed) " (stashed work)" ""))
          (funcall gp-open-function dir))
      (user-error "Could not check out PR branch: %s" (plist-get res :log)))))

(defun gp-ui-show-diff-in-magit (pr)
  "Show PR's diff vs the base in Magit, opening immediately.

Shows the diff against the current local refs right away (no
network wait), then fetches source+base in the background and
refreshes the diff buffer so it converges on the server's view."
  (unless (require 'magit nil t)
    (user-error "Magit is not available"))
  ;; fast: ensure we're on the branch (no fetch if already there)
  (let* ((dir (gp-local-ensure-checkout pr))
         (dest (gp-pr-destination-branch pr))
         (src (gp-pr-source-branch pr))
         (default-directory (file-name-as-directory dir)))
    (gp-ui--magit-pr-diff dest)
    ;; background: refresh the remote refs, then redraw the diff
    (let ((buf (current-buffer))
          (remote gp-checkout-remote))
      (make-process
       :name "gp-diff-fetch"
       :command (list "git" "fetch" remote src dest)
       :sentinel
       (lambda (_proc event)
         (when (and (string-prefix-p "finished" event)
                    (buffer-live-p buf))
           (with-current-buffer buf
             (when (derived-mode-p 'magit-diff-mode)
               (ignore-errors (magit-refresh))))))))))

(defun gp-ui--magit-pr-diff (dest)
  "Run the magit diff for the PR base branch DEST in `default-directory'.
Diffs against the remote base via merge-base, so it matches the
server's diff regardless of local base staleness."
  (magit-diff-range (format "%s/%s...HEAD" gp-checkout-remote dest)))

(defun gp-ui-back-to-list ()
  "Return to the PR overview (Helm if available, else the list)."
  (interactive)
  (if (fboundp 'gp-helm)
      (gp-helm)
    (gp-list)))

;;;; Commands ----------------------------------------------------------------

(defconst gp-list-buffer-name (gp--buffer-name "PRs"))

(defcustom gp-detail-buffer-title-width 40
  "Max characters of the PR title shown in the detail buffer name."
  :type 'integer :group 'bitbucket)

(defun gp--detail-buffer-name (pr)
  "Return the detail buffer name for PR.
Includes the title and repo so buffers are easy to tell apart, e.g.
`*gp: PR #239 add widget toggle (web-frontend)*'."
  (let* ((id (alist-get 'id pr))
         (title (or (alist-get 'title pr) ""))
         (full-name (or (ignore-errors (gp-pr-full-name pr)) ""))
         (repo (if (string-match "/\\([^/]+\\)\\'" full-name)
                   (match-string 1 full-name)
                 full-name))
         (title (if (> (length title) gp-detail-buffer-title-width)
                    (concat (substring title 0 (1- gp-detail-buffer-title-width)) "…")
                  title)))
    (gp--buffer-name
     (format "PR #%s%s%s"
             id
             (if (string-empty-p title) "" (concat " " title))
             (if (string-empty-p repo) "" (format " (%s)" repo))))))

;;;###autoload
(defun gp-list ()
  "Open (or refresh) the pull-request list."
  (interactive)
  (let ((buf (get-buffer-create gp-list-buffer-name)))
    (with-current-buffer buf
      (gp-list-mode)
      (gp-refresh))
    (pop-to-buffer buf)))

(defun gp-refresh ()
  "Fetch and redraw the PR list, restoring point to the last-visited PR."
  (interactive)
  (let* ((uuid (gp-user-uuid))
         (prs (gp-workspace-pull-requests))
         (last-id (and (boundp 'gp-helm--last-visited-pr-id)
                       gp-helm--last-visited-pr-id)))
    (setq gp--prs prs)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (gp--render-list prs uuid))
    (goto-char (or (and last-id (gp--list-find-pr-point last-id))
                   (point-min)))))

(defun gp-reset-caches ()
  "Clear every cache across the API, watch, helm, overlay and local layers.
Use this when a repo or PR is missing from the list because a stale
cache (e.g. the 24h repo-list cache used for \"needs my review\"
scans) predates it.  Callers that are not loaded are skipped via
`fboundp'/`boundp' checks, so this is safe to call regardless of
which optional layers (gp-watch, gp-helm) are in use."
  (interactive)
  (when (fboundp 'bitbucket-cache-clear)
    (bitbucket-cache-clear))
  (when (fboundp 'bitbucket-clear-cache)
    (bitbucket-clear-cache))
  (when (fboundp 'github-clear-cache)
    (github-clear-cache))
  (when (fboundp 'gp-watch-clear-cache)
    (gp-watch-clear-cache))
  (when (fboundp 'gp-local-clear-cache)
    (gp-local-clear-cache))
  (when (boundp 'gp-overlay--avatar-cache)
    (clrhash gp-overlay--avatar-cache))
  (when (boundp 'gp--diff-lines-cache)
    (clrhash gp--diff-lines-cache))
  (when (boundp 'gp--comment-outdated-cache)
    (clrhash gp--comment-outdated-cache))
  (when (boundp 'gp-helm--pipeline-cache)
    (clrhash gp-helm--pipeline-cache))
  (when (boundp 'gp-helm--reviewing-cache)
    (setq gp-helm--reviewing-cache nil))
  (when (boundp 'gp-helm--others-cache)
    (setq gp-helm--others-cache nil))
  (message "gp: all caches cleared"))

(defun gp-refresh-full ()
  "Clear all caches, then refresh the PR list from scratch."
  (interactive)
  (gp-reset-caches)
  (gp-refresh))

(defun gp--list-find-pr-point (id)
  "Return the start of the PR section for ID in the current list buffer, or nil."
  (when (slot-boundp magit-root-section 'children)
    (catch 'found
      (dolist (group (oref magit-root-section children))
        (when (slot-boundp group 'children)
          (dolist (sec (oref group children))
            (when (and (object-of-class-p sec 'gp-pr-section)
                       (equal (alist-get 'id (oref sec value)) id))
              (throw 'found (oref sec start))))))
      nil)))

(defun gp-visit-pr ()
  "Open the detail buffer for the PR at point."
  (interactive)
  (gp-show-pr (gp-current-pr)))

(defcustom gp-detail-show-stats t
  "When non-nil, fetch and show commit/file/line stats in the detail buffer.
Costs two extra API calls (diffstat + commits) per PR."
  :type 'boolean :group 'bitbucket)

(defcustom gp-detail-show-pipelines t
  "When non-nil, fetch and show the PR branch's CI pipelines in the detail buffer.
Costs one API call to list pipelines plus one per pipeline for its
steps; set to nil to skip it."
  :type 'boolean :group 'bitbucket)

(defcustom gp-detail-pipeline-poll-interval 6
  "Seconds between pipeline re-fetches while a current run is unfinished.
The detail buffer auto-polls so a freshly triggered deployment's
state updates without a manual refresh; polling stops once every
current-commit pipeline has finished.  Set to 0 to disable."
  :type 'integer :group 'bitbucket)

(defcustom gp-detail-pipeline-watch-interval 1
  "Seconds between pipeline re-fetches while the head commit has NO run yet.
After pushing a commit there is a window where the PR's new head has
no pipeline; a visible detail buffer keeps watching (re-fetching the
PR head and its pipelines) until the fresh run appears, then hands
over to `gp-detail-pipeline-poll-interval'.  Only branches that
already have pipeline history are watched.  Set to 0 to disable."
  :type 'integer :group 'bitbucket)

(defun gp--detail-pipelines-running-p (data)
  "Non-nil when any current-commit pipeline in DATA is not finished."
  (cl-some (lambda (entry)
             (not (gp-pipeline-finished-p (car entry))))
           (plist-get data :current)))

(defun gp--detail-pipeline-poll-mode (data visible)
  "Decide what to schedule after a pipelines load of DATA.
Both modes require a VISIBLE buffer: polling an off-screen buffer
burns an N+1 fetch (and blocks Emacs for it) for output nobody is
looking at.  Return `poll' while a current-commit run is unfinished,
`watch' when the head commit has no run yet (but the branch has
pipeline history), else nil."
  (cond
   ((and (> gp-detail-pipeline-poll-interval 0)
         visible
         (gp--detail-pipelines-running-p data))
    'poll)
   ((and (> gp-detail-pipeline-watch-interval 0)
         visible
         (null (plist-get data :current))
         (plist-get data :recent))
    'watch)))

(defun gp--detail-pipelines-empty-p (data)
  "Non-nil when DATA carries no pipelines at all (no :current, no :recent)."
  (not (or (plist-get data :current) (plist-get data :recent))))

(defun gp--detail-cancel-pipeline-timer ()
  "Cancel the current detail buffer's pipeline poll timer, if any."
  (when (timerp gp--detail-pipeline-timer)
    (cancel-timer gp--detail-pipeline-timer)
    (setq gp--detail-pipeline-timer nil)))

(defun gp--detail-maybe-resume-pipelines (&optional _frame-or-window)
  "Resume pipeline polling when a detail buffer becomes selected again.
Polling only runs while the buffer is displayed, so re-entering it has
to pick the tracking back up."
  (when (and (derived-mode-p 'gp-detail-mode)
             gp-detail-show-pipelines
             (not gp--detail-pipeline-timer)
             gp--pr
             (eq (current-buffer) (window-buffer (selected-window)))
             (gp--detail-pipeline-poll-mode gp--detail-pipelines t))
    (gp--detail-load-pipelines (current-buffer) gp--pr)))

(defun gp--render-detail-into (buf pr comments stats &optional diff pipelines)
  "Render PR/COMMENTS/STATS (and per-file DIFF, PIPELINES) into BUF."
  (with-current-buffer buf
    (setq gp--pr pr gp--detail-stats stats gp--detail-comments comments
          gp--detail-diff diff gp--detail-pipelines pipelines)
    (let ((inhibit-read-only t)
          (pos (point)))
      (erase-buffer)
      (gp--render-detail pr comments)
      ;; magit caches section visibility across redraws, which would re-expand
      ;; resolved comments on refresh -- force them collapsed again
      (gp--collapse-resolved-sections)
      (goto-char (min pos (point-max))))))

(defun gp--detail-rerender (buf)
  "Redraw BUF's detail view from its cached buffer-locals (no refetch).
Used to fold in pipeline data that arrives after the first render."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when gp--pr
        (gp--render-detail-into buf gp--pr gp--detail-comments
                                gp--detail-stats gp--detail-diff
                                gp--detail-pipelines)))))

(defun gp--collapse-resolved-sections ()
  "Hide every comment in a resolved thread, in the current detail buffer.
Replies fold away with their thread even though they are not
themselves resolved: Bitbucket sets `resolution' only on the comment
the resolve action targeted and cannot resolve a reply at all, so a
reply tested on its own would stay expanded under a collapsed root."
  (when (slot-boundp magit-root-section 'children)
    (let ((by-id (gp--comments-by-id gp--detail-comments)))
      (dolist (sec (oref magit-root-section children))
        (gp--collapse-resolved-walk sec by-id)))))

(defun gp--collapse-resolved-walk (section &optional by-id)
  "Collapse SECTION if its comment THREAD is resolved; recurse into children.
Collapsing only -- the comment's own resolved status is untouched.
BY-ID maps comment id -> comment for the thread walk; without it the
comment is tested on its own."
  (when (and (object-of-class-p section 'gp-comment-section)
             (let ((c (oref section value)))
               (if by-id
                   (gp--comment-thread-resolved-p c by-id)
                 (gp-comment-resolved-p c))))
    (magit-section-hide section))
  (when (slot-boundp section 'children)
    (dolist (child (oref section children))
      (gp--collapse-resolved-walk child by-id))))

(defvar-local gp--detail-loading nil
  "Overlay showing a loading spinner near the title, or nil.")

(defun gp--detail-show-loading ()
  "Mark the current detail buffer as loading with a ⏳ near the title.
Leaves existing content intact (only fresh buffers are blank)."
  (when gp--detail-loading
    (delete-overlay gp--detail-loading))
  (save-excursion
    (goto-char (point-min))
    (if (= (point-min) (point-max))
        ;; fresh buffer: a single minimal line, not a full-screen banner
        (let ((inhibit-read-only t))
          (insert (propertize
                   (format "⏳ #%s %s\n" (alist-get 'id gp--pr)
                           (or (alist-get 'title gp--pr) ""))
                   'face 'gp-detail-title-face)))
      ;; existing content: hang a ⏳ at end of the first line
      (end-of-line)
      (let ((ov (make-overlay (point) (point))))
        (overlay-put ov 'after-string (propertize "  ⏳ refreshing…" 'face 'shadow))
        (setq gp--detail-loading ov)))))

(defun gp--detail-clear-loading ()
  "Remove the loading spinner overlay, if any."
  (when gp--detail-loading
    (delete-overlay gp--detail-loading)
    (setq gp--detail-loading nil)))

(defun gp--detail-buffer-shows-p (buf pr)
  "Return non-nil if BUF is still showing PR (same id).
Used by deferred loaders to decide whether their fetched data is
still relevant, regardless of how many refreshes have happened
since -- the PR id, not a refresh token, is the real invariant."
  (and (buffer-live-p buf)
       (let ((cur (buffer-local-value 'gp--pr buf)))
         (and cur (equal (alist-get 'id cur) (alist-get 'id pr))))))

(defun gp-show-pr (pr)
  "Display PR's detail buffer; load it asynchronously, never freezing.

A skeleton (title, branches, author, action buttons) is painted
IMMEDIATELY from the overview PR object, so navigating in never
shows a blank or frozen page.  The full PR object and comments are
then fetched in the background (cached where it makes sense), and
the heavier stats/diff and pipelines fold in afterwards.  A ⏳
spinner near the title marks the in-flight load."
  (when (boundp 'gp-helm--last-visited-pr-id)
    (setq gp-helm--last-visited-pr-id (alist-get 'id pr)))
  (let ((buf (get-buffer-create (gp--detail-buffer-name pr))))
    (with-current-buffer buf
      ;; Only (re)initialise the mode on a fresh buffer.  `gp-detail-mode' is a
      ;; `define-derived-mode', so calling it runs `kill-all-local-variables'
      ;; and wipes the cached `gp--detail-pipelines' (etc.) -- re-running it on
      ;; an already-open buffer is what made pipelines vanish on reopen.
      (unless (derived-mode-p 'gp-detail-mode)
        (gp-detail-mode))
      (setq gp--pr pr)
      ;; Anchor the buffer to the PR's repo so `find-file' and friends
      ;; default there.  After the mode call: `define-derived-mode' runs
      ;; `kill-all-local-variables', and while `default-directory' survives
      ;; that as a permanent-local, setting it afterwards keeps the
      ;; ordering obvious rather than relying on the exemption.
      (gp-local-anchor-to-checkout pr)
      ;; Paint a skeleton from the overview PR right away, then hang the
      ;; spinner on it.  This is the instant first paint -- no network yet.
      ;; Reuse any cached comments so a revisit shows them without a flash.
      (gp--render-detail-into buf pr gp--detail-comments
                              gp--detail-stats gp--detail-diff
                              gp--detail-pipelines)
      (gp--detail-show-loading))
    ;; reuse the current window so a full-frame helm leads to a full-frame
    ;; detail view instead of splitting
    (pop-to-buffer-same-window buf)
    (gp--detail-load-async buf pr)
    buf))

(defun gp--detail-load-async (buf pr)
  "Load BUF's PR detail off the main thread, folding results in as they arrive.
Fetches the PR object and comments asynchronously, then defers the
heavier stats/diff and pipelines so the visible render is never
blocked.  A monotonic token (`gp--detail-refresh-token') guards
against stale callbacks from a previously-shown PR rendering into
this buffer.  Cached fetches make a revisit cheap; callers wanting
fresh data bind `gp-cache-ttl' to 0 around the call."
  (with-current-buffer buf
    (cl-incf gp--detail-refresh-token)
    (let ((token gp--detail-refresh-token)
          (full-name (gp-pr-full-name pr))
          (id (alist-get 'id pr))
          (old-pr gp--pr)
          (old-comments gp--detail-comments)
          (new-pr nil)
          (new-comments nil)
          (pending 2)
          (failed nil)
          ;; freeze the cache policy in effect at call time so the deferred
          ;; stats/diff fetch honours a `g'-refresh's TTL=0 binding too
          (ttl gp-cache-ttl))
      (cl-labels ((current-p ()
                    (and (buffer-live-p buf)
                         (= token (buffer-local-value 'gp--detail-refresh-token buf))))
                  (finish-one (ok value kind)
                    (when (current-p)
                      (unless ok (setq failed t))
                      (pcase kind
                        ('pr (setq new-pr value))
                        ('comments (setq new-comments value)))
                      (setq pending (1- pending))
                      (when (zerop pending)
                        (with-current-buffer buf
                          (gp--detail-clear-loading)
                          (let ((pr (or new-pr old-pr))
                                (comments (or new-comments old-comments)))
                            (when failed
                              (gp-log-error "detail load: a fetch failed; showing best-effort content"))
                            (gp--render-detail-into
                             buf pr comments
                             gp--detail-stats gp--detail-diff gp--detail-pipelines)
                            ;; heavier data, deferred so it never blocks the paint
                            (gp--detail-load-stats-diff buf pr ttl)
                            (gp--detail-load-reviewers buf pr)
                            (gp--detail-load-commits buf pr)
                            (if gp-detail-show-pipelines
                                (gp--detail-load-pipelines buf pr)
                              (gp-log 'info "pipelines skipped: gp-detail-show-pipelines is nil"))))))))
        (gp-pull-request-async
         full-name id
         (lambda (ok value) (finish-one ok value 'pr)))
        (gp-pull-request-comments-async
         full-name id
         (lambda (ok value) (finish-one ok value 'comments)))))))

(defun gp--detail-load-stats-diff (buf pr ttl)
  "Fetch PR's stats and diff asynchronously and fold them into BUF.
Kept out of the visible-render path: stats/diff are cached by source
commit, so a revisit is instant, while a cold fetch is two round-trips.
Both are fetched with the async ops, so a cold fetch never blocks
Emacs -- the synchronous twins froze the UI for the whole round-trip
(~1.2s) shortly after every `g'.

Relevance is checked by PR id (see `gp--detail-buffer-shows-p'), not a
refresh token: a rapid series of refreshes starts several fetches and
only the last-started token would match, so a strict token check could
drop the only result that ever arrives and leave stats/diff empty
forever.  Keying on the PR id is the real invariant -- same PR, so the
fetched stats/diff are valid whichever refresh asked for them.

TTL carries the caller's cache policy (0 forces fresh on `g')."
  (when (or gp-detail-show-stats gp-detail-show-file-diffs)
    (let ((commit (gp-pr-source-commit pr))
          (full-name (gp-pr-full-name pr))
          (id (alist-get 'id pr)))
      (cl-labels
          ((still-relevant-p ()
             (and (buffer-live-p buf) (gp--detail-buffer-shows-p buf pr)))
           (fold-in (setter value)
             ;; Each result lands on its own; render what we have so far.
             ;; A nil value means the fetch failed -- keep whatever is already
             ;; displayed rather than blanking the section, since the two
             ;; fetches land independently and a failure should not throw away
             ;; a good previous result.
             (when (and value (still-relevant-p))
               (with-current-buffer buf
                 (funcall setter value)
                 (gp--detail-rerender buf)))))
        (when gp-detail-show-stats
          (let ((gp-cache-ttl ttl))
            (gp-pull-request-stats-async
             full-name id pr
             (lambda (stats)
               (fold-in (lambda (v) (setq gp--detail-stats v)) stats)))))
        (when gp-detail-show-file-diffs
          (let ((gp-cache-ttl ttl))
            (gp-pull-request-diff-async
             full-name id commit pr
             (lambda (diff)
               (fold-in (lambda (v) (setq gp--detail-diff (gp-split-diff-by-file v)))
                        diff)))))))))

(defun gp--detail-load-commits (buf pr)
  "Fetch PR's commits asynchronously and fold them into BUF.
Deferred like stats/diff and reviewers so the visible render never
waits on it.  A wall-clock timer, NOT `run-with-idle-timer' -- see
`gp--detail-load-stats-diff'."
  (run-at-time
   0.1 nil
   (lambda ()
     (when (and (buffer-live-p buf) (gp--detail-buffer-shows-p buf pr))
       (condition-case e
           (gp-pull-request-commits-async
            (gp-pr-full-name pr) (alist-get 'id pr)
            (lambda (commits)
              (when (and (buffer-live-p buf) (gp--detail-buffer-shows-p buf pr))
                (with-current-buffer buf
                  ;; nil means the fetch failed; keep whatever we already show
                  ;; rather than blanking the section (same rule as pipelines)
                  (when commits
                    (setq gp--detail-commits commits)
                    (gp--detail-rerender buf)))))
            gp-detail-max-commits)
         (error
          (gp-log-error "commits load failed: %s" (error-message-string e))))))))

(defun gp--detail-load-reviewers (buf pr)
  "Fetch PR's individual reviewers asynchronously and fold them into BUF.
Bitbucket answers this for free (embedded in PR already); GitHub
needs a real fetch (`gp-pr-reviewers-async'), so this stays off the
visible-render path the same way stats/diff do.  A wall-clock timer,
NOT `run-with-idle-timer' -- see `gp--detail-load-stats-diff'."
  (run-at-time
   0.1 nil
   (lambda ()
     (when (and (buffer-live-p buf) (gp--detail-buffer-shows-p buf pr))
       (condition-case e
           (gp-pr-reviewers-async
            pr
            (lambda (reviewers)
              (when (and (buffer-live-p buf) (gp--detail-buffer-shows-p buf pr))
                (with-current-buffer buf
                  (setq gp--detail-reviewers reviewers)
                  (gp--detail-rerender buf)))))
         (error
          (gp-log-error "reviewers load failed: %s" (error-message-string e))))))))

(defun gp--detail-load-pipelines (buf pr)
  "Fetch PR's pipelines on a separate idle timer and fold them into BUF.
Kept separate from the main load so the N+1 pipeline calls never
block or delay the comments/diff view.  While any current-commit
pipeline is still running, schedules a poll so the buffer tracks a
live deployment without a manual refresh."
  (gp-log 'info "pipelines: load scheduled for %s" (buffer-name buf))
  ;; Drop any pending load first: entry points other than the reschedule path
  ;; used to stack, and each pending timer re-arms itself.
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (gp--detail-cancel-pipeline-timer)))
  ;; A wall-clock timer, NOT `run-with-idle-timer': an idle timer scheduled
  ;; from a url sentinel while Emacs is already idle can silently never fire
  ;; (observed live; the sibling stats loader got lucky with ordering).
  (let ((timer
         (run-at-time
          0.2 nil
          (lambda ()
            (if (not (buffer-live-p buf))
                (gp-log 'info "pipelines: buffer gone before load timer fired")
              (condition-case e
                  ;; Async: a synchronous fetch here blocks Emacs's main thread
                  ;; for the branch fetch plus one step fetch per current run --
                  ;; once every `gp-detail-pipeline-poll-interval' seconds, which
                  ;; is exactly the periodic freeze this replaced.
                  (gp-pipeline-fetch-for-pr-async
                   pr
                   (lambda (data)
                     (when (buffer-live-p buf)
                       (unless (equal data (buffer-local-value 'gp--detail-pipelines buf))
                         (gp-log 'info "pipelines: fetched %d current / %d recent"
                                 (length (plist-get data :current))
                                 (length (plist-get data :recent))))
                       (with-current-buffer buf
                         ;; The fetch reports nil on ANY error (it can't tell a
                         ;; transient API hiccup from a genuinely pipeline-less
                         ;; PR).  So an empty result must NOT clobber pipelines we
                         ;; already have -- otherwise a flaky refetch/poll blanks
                         ;; the section until the next successful fetch.  Keep the
                         ;; old data instead; only adopt an empty result on a
                         ;; first-ever load.
                         (let ((keep (and (gp--detail-pipelines-empty-p data)
                                          (not (gp--detail-pipelines-empty-p
                                                gp--detail-pipelines))))
                               ;; a 1s watch tick usually returns identical data --
                               ;; skip the rerender then so point/folding stay put
                               (changed (not (equal data gp--detail-pipelines))))
                           (unless keep
                             (setq gp--detail-pipelines data)
                             (when changed (gp--detail-rerender buf)))
                           ;; Re-arm the animation from STATE, not from the
                           ;; render.  `gp-pipeline--spinner-ensure' otherwise
                           ;; only runs while drawing a glyph, and the
                           ;; `changed' guard above deliberately skips the
                           ;; redraw whenever a poll returns identical data --
                           ;; the steady state of a long-running pipeline.  A
                           ;; spinner timer that retired in the meantime would
                           ;; then never come back and the glyph would sit
                           ;; frozen while the run was still going.
                           (when (gp--detail-pipelines-running-p
                                  gp--detail-pipelines)
                             (gp-pipeline--spinner-ensure))
                           (gp--detail-cancel-pipeline-timer)
                           ;; keep polling against whatever we're actually showing
                           ;; (`visible' -> any frame, not just the selected one)
                           (pcase (gp--detail-pipeline-poll-mode
                                   gp--detail-pipelines (get-buffer-window buf 'visible))
                             ('poll
                              (setq gp--detail-pipeline-timer
                                    (run-with-timer
                                     gp-detail-pipeline-poll-interval nil
                                     #'gp--detail-load-pipelines buf pr)))
                             ('watch
                              (setq gp--detail-pipeline-timer
                                    (run-with-timer
                                     gp-detail-pipeline-watch-interval nil
                                     #'gp--detail-pipeline-watch-tick buf)))))))))
                (error
                 (gp-log-error "pipeline load failed: %s"
                               (error-message-string e)))))))))
    ;; Store in BUF's slot, not the caller's current buffer.
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq gp--detail-pipeline-timer timer)))))

(defun gp--detail-pipeline-watch-tick (buf)
  "One watch cycle: re-fetch BUF's PR head, then reload its pipelines.
Re-fetching the PR first means a just-pushed commit's fresh run shows
up as the CURRENT run (the head hash moved), not as a recent one.
Stops silently when BUF is gone or no longer displayed; the next
manual refresh restarts the watch."
  (when (and (buffer-live-p buf) (get-buffer-window buf))
    (let ((pr (buffer-local-value 'gp--pr buf)))
      (gp-pull-request-async
       (gp-pr-full-name pr) (alist-get 'id pr)
       (lambda (ok new-pr)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (when (and ok new-pr
                        (equal (alist-get 'id new-pr) (alist-get 'id gp--pr)))
               (setq gp--pr new-pr)))
           (gp--detail-load-pipelines
            buf (buffer-local-value 'gp--pr buf))))))))

(defun gp--detail-refresh-async (buf pr)
  "Refresh BUF's PR detail asynchronously, bypassing the result cache.
Existing content stays visible while a fresh PR object, comments,
stats, diff and pipelines are fetched.  Binding `gp-cache-ttl'
to 0 makes the shared loader (`gp--detail-load-async') and its
deferred stats/diff fetch ignore the cache, so `g' always re-fetches."
  (let ((gp-cache-ttl 0))
    (gp--detail-load-async buf pr)))

(defun gp-detail-refresh ()
  "Re-fetch and redraw the current detail buffer (non-blocking, force-fresh)."
  (interactive)
  (if gp--pr
      (progn
        (gp--detail-show-loading)
        (gp--detail-refresh-async (current-buffer) gp--pr))
    (gp-show-pr gp--pr)))

(defun gp-browse-pr ()
  "Open the PR at point in a web browser."
  (interactive)
  (let ((url (let-alist (gp-current-pr) .links.html.href)))
    (if url (browse-url url) (user-error "No URL for this PR"))))

(defun gp-open-local ()
  "Open the local checkout of the PR at point."
  (interactive)
  (gp-local-open (gp-current-pr)))

(defun gp-checkout-branch ()
  "Fetch and check out the PR's source branch locally, then open the repo."
  (interactive)
  (let* ((res (gp-local-checkout-branch (gp-current-pr)))
         (dir (plist-get res :dir)))
    (message "%s %s%s"
             (if (plist-get res :ok) "Checked out in" "Checkout FAILED in")
             dir
             (if (plist-get res :stashed) " (stashed local work)" ""))
    (when (and (plist-get res :ok) dir)
      (funcall gp-open-function dir))))

;;;; Transient ---------------------------------------------------------------

;;;###autoload (autoload 'gp-dispatch "gp-ui" nil t)
(transient-define-prefix gp-dispatch ()
  "Bitbucket pull-request actions."
  ["Pull request"
   ("RET" "Visit (comments)" gp-visit-pr)
   ("w"   "Browse on web"     gp-browse-pr)]
  ["Local"
   ("o"   "Open checkout"     gp-open-local)
   ("b"   "Checkout branch"   gp-checkout-branch)]
  ["List"
   ("g"   "Refresh"           gp-refresh)])

(provide 'gp-ui)
;;; gp-ui.el ends here
