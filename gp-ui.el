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

(declare-function gp-deploy-watch-list-show "gp-deploy-watch")
(declare-function gp-checkout--git "gp-checkout")
(declare-function gp-checkout-current-branch "gp-checkout")
(declare-function gp-checkout-dirty-count "gp-checkout")
(declare-function gp-helm "gp-helm")
(declare-function gp-helm-terminal-send-comment "gp-helm-terminal")
(declare-function gp-helm-terminal-send-comments "gp-helm-terminal")
(declare-function gp-helm-terminal-send-conflict "gp-helm-terminal")
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
(defface gp-repo-face '((t :inherit magit-branch-remote))
  "Face for a repository name in the detail buffer.
Distinct from `gp-branch-face' so the repo line does not read as
another branch: they sit one above the other and would otherwise be
one indistinguishable block." :group 'bitbucket-faces)
(defface gp-dirty-tree-face '((t :inherit warning))
  "Face for the uncommitted-local-changes notice in the detail buffer.
`warning', not `error': uncommitted work is a caution about what a
checkout would move, not something broken -- the conflict warning
right below it owns `error', and two lines shouting equally would
flatten the difference." :group 'bitbucket-faces)
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
(defvar-local gp--list-loading nil
  "Overlay showing a loading spinner near the top of the list buffer, or nil.")
(defvar-local gp--list-refresh-token 0
  "Monotonic token used to ignore stale async list refresh callbacks.")
(defvar-local gp--pr nil
  "PR alist backing the current detail buffer.")
(defvar-local gp--detail-stats nil
  "Plist of (:commits :files :added :removed) for the detail buffer, or nil.")
(defvar-local gp--detail-diff nil
  "Alist of (PATH . DIFF-CHUNK) for the detail buffer, or nil.")
(defvar-local gp--detail-mergeability nil
  "Cons (MERGEABLE . STATE) for the detail buffer's PR, or nil.
Fetched by the async load, never during a render: asking the forge costs
a request, and a fetch mid-redisplay re-enters the renderer.")
(defvar-local gp--detail-divergence nil
  "Cons (AHEAD . BEHIND) commit counts for the detail buffer's PR, or nil.
Fetched with the mergeability, never during a render.  Nil where the
backend cannot say (Bitbucket), in which case nothing is shown -- a zero
would claim the branch is up to date when the truth is unknown.")
(defvar-local gp--detail-merge-pipelines nil
  "Pipeline data plist for a merged PR's merge commit, or nil.
The build that matters after a merge runs on the destination branch, not
on the PR branch -- that is the one carrying the deployment.")
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
(defvar-local gp--detail-local-dirty nil
  "Local working-tree state of the PR's checkout, or nil.
Plist (:dir DIR :branch BRANCH :count N), set only when the tree has
uncommitted changes.  Filled by `gp--detail-load-local-dirty', never
during a render: it shells out to git, and the no-fetch-in-render rule
covers a subprocess just as much as a request.  Nil also means \"no
clone\" and \"not asked yet\", all three of which draw nothing -- a
notice about work that may not exist is worse than none.")
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
          (when-let* ((url (gp-comment-web-url comment)))
            (insert ind "  ")
            (gp--insert-link url "↗ view in browser [w]")
            (insert "\n"))
          (let ((body (string-trim-right
                       (gp--render-markdown (or .content.raw "")))))
            (insert (funcall pad (replace-regexp-in-string "^" "  " body)) "\n"))
          (when pr
            ;; the reactions line indents itself: emitting the indent here
            ;; left stray whitespace on the action row when a comment had no
            ;; reactions to draw
            (gp--insert-reactions pr comment (concat ind "  "))
            (insert ind "  ")
            (gp--insert-action-button
             "reply [R]" "Reply to this comment"
             (lambda () (gp-ui-reply-comment pr comment)))
            (when (gp-reactions-supported-p)
              (insert " ")
              (let ((liked (and (member "+1" (alist-get 'reaction-mine comment)) t)))
                (gp--insert-action-button
                 ;; The label names the KEY, which is `+' in both directions
                 ;; (`-' is `negative-argument' globally, so binding it here
                 ;; would fight Emacs).  `+' toggles; the help text is what
                 ;; says which way it will go.
                 "👍 [+]"
                 (if liked "Remove your 👍 from this comment"
                   "Add your 👍 to this comment")
                 (lambda () (gp-ui-toggle-reaction pr comment "+1"))))
              (insert " ")
              (gp--insert-action-button
               "react [!]" "Pick a reaction for this comment"
               (lambda () (gp-ui-react-to-comment pr comment))))
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

(defconst gp-reaction-emoji-alist
  '(("+1" . "👍") ("-1" . "👎") ("laugh" . "😄") ("confused" . "😕")
    ("heart" . "❤️") ("hooray" . "🎉") ("rocket" . "🚀") ("eyes" . "👀"))
  "Display emoji for each platform reaction token.
The tokens are the platform's own (GitHub sends \"+1\", not
\"thumbs_up\"); anything unmapped falls back to showing the token, so a
platform adding a ninth reaction degrades to readable text rather than
breaking.")

(defun gp-reaction-emoji (content)
  "Return the display emoji for reaction CONTENT, or CONTENT itself."
  (or (cdr (assoc content gp-reaction-emoji-alist)) content))

;; help-at-pt.el is loaded on demand in `gp-detail-mode'; declare its vars so
;; a cold byte-compile doesn't treat them as free.
(defvar help-at-pt-display-when-idle)
(defvar help-at-pt-timer-delay)

(defcustom gp-detail-help-at-point t
  "When non-nil, echo a button's `help-echo' when point lands on it.
Reaction buttons name their reactors this way, so the information is
reachable without a mouse.  This arms Emacs' global `help-at-pt' timer
\(`help-at-pt-display-when-idle'), which affects every buffer, not only
this package's -- set nil to leave that global alone."
  :type 'boolean :group 'bitbucket)

(defcustom gp-reaction-names-max 5
  "How many reactor names to name in a reaction's tooltip.
Beyond this the rest are summarised as \"+N more\", so a widely-liked
comment does not produce an echo-area line too long to read."
  :type 'integer :group 'bitbucket)

(defun gp--reaction-summary (reactions uuid)
  "Summarise REACTIONS as ((CONTENT COUNT MINE-P NAMES) …), platform order kept.
UUID identifies the current user so its own reactions can be marked, and
NAMES lists who reacted (in arrival order) for the tooltip.  Grouping
happens here rather than in each backend: the APIs return one row per
(user, reaction), which is what makes the count, the \"did I react\" test
and the name list all possible from a single fetch."
  (let (out)
    (dolist (r reactions)
      (let* ((content (alist-get 'content r))
             (mine (equal (let-alist r .user.uuid) uuid))
             (who (or (let-alist r .user.display_name)
                      (let-alist r .user.uuid)
                      "someone"))
             (cell (assoc content out)))
        (if cell
            (setcdr cell (list (1+ (nth 1 cell))
                               (or (nth 2 cell) mine)
                               (append (nth 3 cell) (list who))))
          (push (list content 1 mine (list who)) out))))
    (nreverse out)))

(defun gp--reaction-tooltip (content count names mine)
  "Describe a reaction for the echo area: who reacted, and what a click does.
NAMES is abbreviated at `gp-reaction-names-max' so a popular comment
does not produce an unreadable line."
  (let* ((shown (seq-take names gp-reaction-names-max))
         (extra (- (length names) (length shown))))
    (format "%s %s by %s%s -- click to %s yours"
            count content
            (mapconcat #'identity shown ", ")
            (if (> extra 0) (format " +%d more" extra) "")
            (if mine "remove" "add"))))

(defun gp--reaction-names (pr comment content)
  "Return the display names of whoever reacted CONTENT to COMMENT of PR.
Fetched on demand -- when the tooltip is actually shown -- because the
counts come free with the comment while the names cost a request per
comment.  Doing that eagerly during a render meant a network call per
comment mid-redisplay, which re-entered the renderer and drew the
comment several times over."
  (let ((uuid (gp-user-uuid)))
    (mapcar (lambda (cell) (car (nth 3 cell)))
            (seq-filter (lambda (cell) (equal (car cell) content))
                        (gp--reaction-summary
                         (seq-filter
                          (lambda (r) (equal (alist-get 'content r) content))
                          (ignore-errors
                            (gp-comment-reactions (gp-pr-full-name pr) comment)))
                         uuid)))))

(defun gp--reaction-help-echo (pr comment content count mine)
  "Build a lazy `help-echo' for a reaction button.
A function rather than a string, so the reactor names are fetched only
when the tooltip is actually displayed."
  (lambda (&rest _)
    (let* ((rows (ignore-errors
                   (seq-filter (lambda (r) (equal (alist-get 'content r) content))
                               (gp-comment-reactions (gp-pr-full-name pr) comment))))
           (uuid (ignore-errors (gp-user-uuid)))
           (names (mapcar (lambda (r) (or (let-alist r .user.display_name)
                                          (let-alist r .user.uuid)))
                          rows))
           ;; whether it is mine is only knowable from these rows -- the
           ;; comment payload carries counts, not who reacted -- so decide it
           ;; here rather than trusting the caller's guess
           (mine (or mine
                     (and uuid (seq-some (lambda (r) (equal (let-alist r .user.uuid) uuid))
                                         rows)
                          t))))
      (if names
          (gp--reaction-tooltip content count names mine)
        (format "%d %s -- click to %s yours"
                count content (if mine "remove" "add"))))))

(defun gp--insert-reactions (pr comment &optional indent)
  "Insert COMMENT's reaction summary for PR, if the platform has any.
INDENT is prefixed only when something is actually drawn, so a comment
with no reactions leaves no stray whitespace behind.
Each reaction is a button that toggles the current user's own -- so a
click adds yours, and a second click takes it away.  Nothing is inserted
on a platform without reactions, or on a comment nobody has reacted to
yet (`+' / `!' are how you add the first one).

Counts come from the comment itself (`reaction-counts', filled in by the
backend), never from a fetch: rendering must not touch the network."
  (when (gp-reactions-supported-p)
    (let ((counts (alist-get 'reaction-counts comment))
          (mine-set (alist-get 'reaction-mine comment)))
      (when counts
        (when indent (insert indent))
        (dolist (cell counts)
          (let* ((content (car cell))
                 (count (cdr cell))
                 ;; REST ships counts but never says whether *you* reacted;
                 ;; the backend fills `reaction-mine' from one GraphQL
                 ;; prefetch (`viewerHasReacted') so this stays fetch-free.
                 (mine (and (member content mine-set) t)))
            (gp--insert-action-button
             ;; The count always shows, in brackets, so a reaction never
             ;; looks like a single anonymous mark.  Whether it is yours is
             ;; deliberately NOT marked on the pill: a glyph here reads as
             ;; "resolved"/"done" rather than "me".  The quick action's
             ;; [+]/[-] label carries that, and the tooltip names the
             ;; reactors.
             (format "%s (%d)" (gp-reaction-emoji content) count)
             (gp--reaction-help-echo pr comment content count mine)
             (lambda () (gp-ui-toggle-reaction pr comment content)))
            (insert " ")))
        (insert "\n")))))

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
comes from `gp-pr-description' rather than the alist directly.

On an open PR the heading carries an edit button, the same way the
reviewers and labels lines do, and the section appears even with no
description yet -- that empty state is exactly when you need a way to
write the first one, and it is where `E' would otherwise be invisible.
A closed PR with no description still renders nothing at all: there is
nothing to read and, since the forges only allow mutating an open PR,
nothing to do either."
  (let* ((desc (ignore-errors (gp-pr-description pr)))
         (editable (gp-pr-open-p pr)))
    (when (or desc editable)
      (magit-insert-section (gp-description nil gp-detail-description-collapsed)
        ;; The button has to be inserted into THIS buffer: `insert-button'
        ;; puts its properties in an overlay, and an overlay does not
        ;; survive `buffer-string', so building the heading as a string
        ;; first would silently yield plain text that merely looks like a
        ;; button.  Insert the pieces, then close the heading with a bare
        ;; `magit-insert-heading' (which just marks where the body starts).
        (insert (propertize "Description" 'face 'magit-section-heading))
        (when editable
          (insert "   ")
          (gp--insert-action-button
           "✎ edit [E]" "Edit this PR's description"
           (lambda () (gp-ui-edit-description pr))))
        (insert "\n")
        (magit-insert-heading)
        (let ((start (point)))
          (insert (if desc
                      (gp--render-markdown desc)
                    (propertize "(no description yet)\n" 'face 'shadow)))
          (unless (bolp) (insert "\n"))
          ;; indent the body so it reads as belonging to the heading
          (indent-rigidly start (point) 2))
        (insert "\n")))))

(defface gp-outcome-merged-face
  '((t :inherit success :weight bold :height 1.2))
  "Face for the MERGED banner on a closed pull request."
  :group 'bitbucket)

(defface gp-outcome-declined-face
  '((t :inherit error :weight bold :height 1.2))
  "Face for the DECLINED banner on a closed pull request."
  :group 'bitbucket)

(defun gp--insert-outcome-banner (pr)
  "Insert a prominent outcome banner for a PR that is no longer open.
An open PR gets nothing: its state is the whole detail view.  A closed
one gets a single loud line, since that outcome is the first thing worth
knowing -- and for a decline, the reason where the backend has one
\(Bitbucket keeps a free-text `reason'; GitHub does not)."
  (unless (ignore-errors (gp-pr-open-p pr))
    (let ((merged (ignore-errors (gp-pr-merged-p pr))))
      (insert (propertize (if merged "  MERGED  " "  DECLINED  ")
                          'face (if merged
                                    'gp-outcome-merged-face
                                  'gp-outcome-declined-face)))
      (when-let* ((at (ignore-errors (gp-pr-merged-at pr))))
        (insert (propertize (format "  %s" (gp--relative-time at)) 'face 'shadow)))
      (insert "\n")
      (unless merged
        (when-let* ((reason (ignore-errors (gp-pr-closed-reason pr))))
          (insert (propertize (format "  %s\n" (string-trim reason)) 'face 'shadow))))
      (insert "\n"))))

(defun gp--insert-local-dirty-warning ()
  "Insert a notice when the PR's local checkout has uncommitted changes.
Reads `gp--detail-local-dirty' rather than shelling out to git, so this
costs nothing during a render (same rule as the conflict warning).

Sits near the top because the buttons a few lines above it -- checkout,
show-diff -- are exactly the ones that move the working tree, and
knowing there is unsaved work HERE is what stops you pressing them
blind.  It says the branch the work is sitting on: dirty work on the
PR's own source branch is a different situation from dirty work on some
unrelated branch, and only the second is a surprise."
  (when-let* ((state gp--detail-local-dirty)
              (count (plist-get state :count)))
    (insert (propertize "⚠ Uncommitted local changes" 'face 'gp-dirty-tree-face))
    (insert (propertize
             (format "  (%d file%s%s)\n"
                     count (if (= count 1) "" "s")
                     (if-let* ((branch (plist-get state :branch)))
                         (format " on %s" branch)
                       ""))
             'face 'shadow))
    (insert (propertize
             "  Checkout [O] stashes them first; [o] opens the repo as it is.\n"
             'face 'shadow))
    (insert "\n")))

(defun gp--insert-merge-conflict-warning (pr)
  "Insert a conflict warning for PR when the forge reports one.
Reads `gp--detail-mergeability' rather than asking the forge, so this
costs nothing during a render.  Nothing is drawn when the answer is
`unknown' (GitHub has not judged yet) or nil (Bitbucket cannot say) --
only a stated conflict is worth a warning, since a false one would send
you looking for a problem that is not there."
  (when (and gp--detail-mergeability
             (null (car gp--detail-mergeability)))
    (let ((dest (or (ignore-errors (gp-pr-destination-branch pr)) "the target branch"))
          (start (point)))
      (insert (propertize
               (format "⚠ Cannot merge: conflicts with %s" dest)
               'face 'gp-pipeline-failed-face))
      (insert (propertize
               (format "  (%s%s)\n" (or (cdr gp--detail-mergeability) "conflicting")
                       ;; how far behind is the actionable part: it says how
                       ;; much target-branch history the rebase has to cross
                       (if (and gp--detail-divergence
                                (> (cdr gp--detail-divergence) 0))
                           (format ", %d commit%s behind" (cdr gp--detail-divergence)
                                   (if (= (cdr gp--detail-divergence) 1) "" "s"))
                         ""))
               'face 'shadow))
      (insert "  ")
      (gp--insert-action-button
       "resolve in terminal [t]"
       "Ask the AI terminal session to rebase onto the target and resolve"
       (lambda () (gp-ui-send-conflict-to-terminal pr)))
      (insert (propertize "   or merge the target in locally, resolve, and push.\n"
                          'face 'shadow))
      (insert "\n")
      ;; tag the whole block so `t' knows it is on the conflict warning
      ;; rather than a comment (see `gp-detail-send-to-terminal')
      (put-text-property start (point) 'gp-conflict-warning t))))

(defun gp--insert-merge-pipelines (pr)
  "Insert the destination-branch pipeline of a merged PR, if any.
After a merge the interesting run is the one on the merge commit -- that
is what deploys -- so it is shown here rather than making you leave the
editor to find it.  Reuses `gp--insert-pipelines', so the same step
actions apply (logs on `l', manual gates on `T')."
  (when (and gp--detail-merge-pipelines (gp-pr-merged-p pr))
    (let ((dest (or (ignore-errors (gp-pr-destination-branch pr)) "destination")))
      (insert (propertize (format "After merge (on %s)\n" dest)
                          'face 'magit-section-heading))
      ;; Only the merge commit's own run belongs here.  `:recent' is the
      ;; destination branch's other commits -- everything else that landed on
      ;; main -- which has nothing to do with this PR.
      (gp--insert-pipelines
       (list :current (plist-get gp--detail-merge-pipelines :current)
             :recent nil))
      (insert "\n"))))

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

(defun gp--file-path-at-line ()
  "Return the changed-file path for the line point is on, or nil.
A file line is \"  NAME  +N -M\" with only NAME buttonised, so requiring
point to sit on the button itself makes RET miss from the indentation,
the stat columns or end of line.  Scanning the whole line instead means
anywhere on it works; only lines carrying a `gp-file-path' button match,
so other sections are unaffected."
  (let* ((bol (line-beginning-position))
         (eol (line-end-position))
         (button (or (button-at bol) (next-button bol)))
         path)
    (while (and button (not path) (< (button-start button) eol))
      (setq path (button-get button 'gp-file-path))
      (unless path
        (setq button (next-button (button-end button)))))
    path))

(defun gp-detail-visit-file ()
  "Open the changed file at point (detail buffer).
Point may be anywhere on the file's line, not just on the name."
  (interactive)
  (if-let* ((path (gp--file-path-at-line)))
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
      (when (gp-pr-open-p pr)
        (insert "   ")
        (gp--insert-action-button
         "✎ [N]" "Edit this pull request's title"
         (lambda () (gp-ui-edit-title pr))))
      (insert "\n")
      ;; The repo, on its own line above the branches.  A detail buffer is
      ;; often reached from a workspace-wide list where consecutive PRs come
      ;; from different repos, and "#42 fix the toggle" says nothing about
      ;; which one -- the branch line below answers "from where to where",
      ;; not "in what".  Full name rather than the bare slug, since the
      ;; owner is what tells two same-named repos apart.
      (when-let* ((repo (ignore-errors (gp-pr-full-name pr))))
        ;; 📁 not 📦: the checkout button below already uses 📦, and two
        ;; different things wearing one glyph in the same header defeats the
        ;; scanning the emoji is there for.
        (insert "📁 " (propertize repo 'face 'gp-repo-face) "\n"))
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
      (gp--insert-link (gp-pr-web-url pr) "🔗 View in browser [w]")
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
      ;; merge, on any open PR that can actually merge.  A conflict simply
      ;; removes the action: the warning under the description already says
      ;; why, and repeating it here was noise.  Mergeability comes from
      ;; `gp--detail-mergeability' (filled by the async load) -- this must
      ;; never fetch during a render.
      (when (and (gp-pr-open-p pr)
                 (not (and gp--detail-mergeability
                           (null (car gp--detail-mergeability)))))
        (insert "   ")
        (let ((buf (current-buffer)))
          (gp--insert-action-button/spinner
           'merge "🔀 Merge [M]"
           "Merge this pull request (C-u for the merge strategy)"
           (lambda () (gp--detail-run-action
                       buf 'merge (lambda () (gp-ui-merge-pr pr)))))))
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
    (gp--insert-outcome-banner pr)
    (gp--insert-local-dirty-warning)
    (gp--insert-description pr)
    (gp--insert-merge-conflict-warning pr)
    (gp--insert-merge-pipelines pr)
    (gp--insert-changed-files)
    (gp--insert-commits)
    (gp--insert-pipelines gp--detail-pipelines)
    (magit-insert-section (gp-comments)
      (insert (propertize (format "Comments (%d)" (length comments))
                          'face 'magit-section-heading))
      (when (gp-pr-open-p pr)
        (insert "   ")
        (gp--insert-action-button
         "✎ comment [C]" "Add a general comment on this pull request"
         (lambda () (gp-ui-add-general-comment pr))))
      (insert "\n")
      (magit-insert-heading)
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
  "E"   #'gp-detail-edit-description  ;; edit the PR's own description
  "N"   #'gp-detail-edit-title        ;; rename: edit the PR's title
  "C"   #'gp-detail-add-general-comment ;; new general (non-inline) comment
  "M"   #'gp-detail-merge               ;; merge (C-u picks the strategy)
  "+"   #'gp-detail-like-comment        ;; quick 👍 toggle on the comment at point
  "!"   #'gp-detail-react-to-comment    ;; pick any reaction (where supported)
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
  "A"   #'gp-detail-pipeline-arm-deploy   ;; arm/disarm a deploy watcher
  "C-c A" #'gp-deploy-watch-list-show     ;; every armed watcher
  "m"   #'gp-detail-toggle-mark
  "l"   #'gp-detail-pipeline-step-log)

(defun gp-detail-edit-reviewers ()
  "Add or remove reviewers on the PR shown in this buffer."
  (interactive)
  (gp-ui-edit-reviewers gp--pr))

(defun gp-detail-edit-title ()
  "Edit the title of the PR shown in this buffer."
  (interactive)
  (gp-ui-edit-title gp--pr))

(defun gp-detail-edit-description ()
  "Edit the description of the PR shown in this buffer."
  (interactive)
  (gp-ui-edit-description gp--pr))

(defun gp-detail-merge (&optional arg)
  "Merge the PR shown in this buffer.
With \\[universal-argument] ARG, choose the merge strategy."
  (interactive "P")
  (gp-ui-merge-pr gp--pr arg))

(defun gp-detail-add-general-comment ()
  "Add a general (non-inline) comment on the PR shown in this buffer."
  (interactive)
  (gp-ui-add-general-comment gp--pr))

(defun gp-detail-like-comment ()
  "Toggle your 👍 on the comment at point."
  (interactive)
  (unless (gp-reactions-supported-p)
    (user-error "This platform has no reactions on comments"))
  (gp-ui-toggle-reaction gp--pr (gp-detail--comment-at-point) "+1"))

(defun gp-detail-react-to-comment ()
  "Pick a reaction for the comment at point."
  (interactive)
  (unless (gp-reactions-supported-p)
    (user-error "This platform has no reactions on comments"))
  (gp-ui-react-to-comment gp--pr (gp-detail--comment-at-point)))

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
  "Hand the thing at point to the terminal session.
On the merge-conflict warning that is the conflict; otherwise the marked
comments, or the comment at point.  Marked comments still win over a
bare position, since marking them is the explicit act."
  (interactive)
  (cond
   ((get-text-property (point) 'gp-conflict-warning)
    (gp-ui-send-conflict-to-terminal gp--pr))
   ((gp--detail-marked-comments)
    (gp-ui-send-comments-to-terminal gp--pr (gp--detail-marked-comments))
    (setq gp--detail-marked-comment-ids nil)
    (gp--detail-rerender (current-buffer)))
   (t (gp-ui-send-comment-to-terminal gp--pr (gp-detail--comment-at-point)))))

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
        (let ((url (gp-pr-web-url (oref sec value))))
          (if url
              (browse-url url)
            (user-error "No URL for this comment")))
      (gp-browse-pr))))

(defun gp-detail-ret ()
  "Context action: open a changed file or commit, jump to a comment, else fold."
  (interactive)
  (let ((sec (magit-current-section)))
    (cond
     ((gp--file-path-at-line)
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
  ;; `help-echo' fires for the mouse only; `help-at-pt' echoes it when POINT
  ;; lands on a button too, which is how a reaction's reactor names become
  ;; visible without reaching for the mouse.  Both are global and only take
  ;; effect once the timer is (re)armed, so `setq-local' would silently do
  ;; nothing -- see `gp-detail-help-at-point'.
  (when gp-detail-help-at-point
    (require 'help-at-pt)
    (setq help-at-pt-display-when-idle '(help-echo)
          help-at-pt-timer-delay 0.3)
    (when (fboundp 'help-at-pt-set-timer) (help-at-pt-set-timer)))
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

(defun gp-ui-add-general-comment (pr)
  "Open a compose buffer for a new general (non-inline) comment on PR.
No path or line is involved, so this is the one comment kind that
cannot fail the way an inline one can when the target line is outside
the PR's diff (see `gp-github-check-inline-target')."
  (require 'gp-compose)
  (let ((buf (gp--detail-buffer-name pr)))
    (gp-compose
     (list :full-name (gp-pr-full-name pr)
           :id (alist-get 'id pr)
           :what "general comment"
           :on-success
           (lambda (_c)
             (gp-invalidate-pr-caches pr)
             (when (buffer-live-p (get-buffer buf))
               (with-current-buffer buf (gp-detail-refresh)))
             (message "Comment posted on PR #%s" (alist-get 'id pr)))))))

(defun gp-ui-send-conflict-to-terminal (pr)
  "Ask PR's terminal session to resolve its merge conflicts."
  (require 'gp-helm-terminal)
  (gp-helm-terminal-send-conflict pr))

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

(defun gp-ui-toggle-reaction (pr comment content)
  "Toggle the current user's CONTENT reaction on COMMENT of PR.
Reads the comment's reactions first to decide the direction, so the
same entry point serves both adding and removing -- which is what lets
a rendered reaction button be a toggle."
  (let* ((full-name (gp-pr-full-name pr))
         (uuid (gp-user-uuid))
         (mine (seq-find (lambda (r)
                           (and (equal (alist-get 'content r) content)
                                (equal (let-alist r .user.uuid) uuid)))
                         (ignore-errors (gp-comment-reactions full-name comment))))
         (buf (current-buffer)))
    (gp-set-comment-reaction full-name comment content (not mine))
    (message "%s %s" (gp-reaction-emoji content) (if mine "removed" "added"))
    (gp-invalidate-pr-caches pr)
    (when (buffer-live-p buf)
      (with-current-buffer buf (gp-detail-refresh)))))

(defun gp-ui-react-to-comment (pr comment)
  "Pick a reaction for COMMENT of PR and toggle it.
Offers the platform's own set (`gp-reaction-choices'), each shown as
emoji plus its token so the completing-read is searchable by either."
  (let* ((choices (gp-reaction-choices))
         (table (mapcar (lambda (c)
                          (cons (format "%s  %s" (gp-reaction-emoji c) c) c))
                        choices)))
    (unless choices
      (user-error "This platform has no reactions on comments"))
    (let ((pick (completing-read "React with: " table nil t)))
      (gp-ui-toggle-reaction pr comment (or (cdr (assoc pick table)) pick)))))

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

(defun gp-ui-edit-title (pr)
  "Edit PR's title in the minibuffer, saving it back on RET.
A title is one line, so this is a `read-string' rather than a compose
buffer -- the editor `E' opens would be the wrong shape for it.  The
current title is pre-filled and editable in place; leaving it unchanged,
or blank, does nothing."
  (let* ((full-name (gp-pr-full-name pr))
         (id (alist-get 'id pr))
         (current (or (alist-get 'title pr) ""))
         (new (string-trim (read-string (format "Title for PR #%s: " id) current))))
    (cond
     ((string-empty-p new) (user-error "A pull request title cannot be empty"))
     ((equal new (string-trim current)) (message "Title unchanged"))
     (t
      (gp-set-pull-request-title full-name id new)
      (gp-invalidate-pr-caches pr)
      (gp-detail-refresh)
      (message "PR #%s retitled" id)))))

(defun gp-ui-edit-description (pr)
  "Edit PR's description in a compose buffer, saving it back on submit.
Reuses `gp-compose' (Markdown, emoji completion, C-c C-p preview) with
the current description as the starting text, so this is the same
editor comments get.  Hard-break munging is disabled: a description is
a document rather than a chat message, and rewriting its newlines on
every save would slowly mangle tables and lists."
  (let* ((full-name (gp-pr-full-name pr))
         (id (alist-get 'id pr))
         (title (alist-get 'title pr))
         (current (or (ignore-errors (gp-pr-description pr)) ""))
         ;; `:on-success' runs in the *compose* buffer, where `gp--pr' is
         ;; nil and `gp-detail-refresh' would no-op.  Capture the detail
         ;; buffer by name now and redraw inside it, as `gp-ui-edit-comment'
         ;; does.
         (buf (gp--detail-buffer-name pr)))
    (gp-compose
     (list :full-name full-name
           :id id
           :initial-text current
           :what "description"
           ;; `gp-compose-submit' applies hard breaks before handing the
           ;; text over, so the opt-out has to travel with the target.
           :no-hard-breaks t
           :allow-empty t          ;; clearing a description is a real edit
           :submit-function
           (lambda (fn pid text &rest _)
             (gp-set-pull-request-description fn pid text title))
           :on-success
           (lambda (_updated)
             (gp-invalidate-pr-caches pr)
             (when (buffer-live-p (get-buffer buf))
               (with-current-buffer buf (gp-detail-refresh)))
             (message "PR #%s description updated" id))))))

(defcustom gp-merge-delete-local-branch t
  "When non-nil, delete the merged source branch from the local checkout.
Only ever the LOCAL branch: the forge owns the remote one (it deletes it
itself when asked, and racing that would either fail or delete a branch
the merge still needs), so this never pushes a deletion."
  :type 'boolean :group 'bitbucket)

(defcustom gp-merge-close-source-branch t
  "When non-nil, ask the forge to delete the source branch on merge.
Bitbucket takes this per merge.  GitHub has no per-merge control -- it
follows the repository's own `delete_branch_on_merge' -- so there the
value has no effect."
  :type 'boolean :group 'bitbucket)

(defun gp--merge-strategy-choice (full-name id arg)
  "Return (STRATEGY . SOURCE) to merge PR FULL-NAME/ID with.
With ARG non-nil, prompt among the strategies the forge permits;
otherwise take its default.  SOURCE is a word naming where the strategy
came from, so the confirmation can say whether it was chosen or
inherited.  A backend that cannot answer yields (nil . \"the forge's
default\"), which merges without naming a strategy at all."
  (let* ((info (ignore-errors (gp-pull-request-merge-strategies full-name id)))
         (strategies (car info))
         (default (cdr info)))
    (cond
     ((null strategies) (cons nil "the forge's default"))
     (arg (cons (completing-read
                 (format "Merge strategy (default %s): " (or default "?"))
                 strategies nil t nil nil default)
                "your choice"))
     (default (cons default "the repository default"))
     (t (cons (car strategies) "the only permitted strategy")))))

(defun gp--merge-delete-local-branch (pr branch)
  "Delete BRANCH from PR's local checkout, if there is one.
Never touches the remote: the forge deletes that itself.  Switches off
BRANCH first when it is checked out, since git refuses to delete the
current branch.  Every failure is reported, not signalled -- the merge
has already happened by this point, so aborting here would misreport a
completed merge as a failure."
  (let ((dir (ignore-errors (gp-local-find-checkout (gp-pr-full-name pr)))))
    (cond
     ((not (and dir branch)) nil)
     ((not (file-directory-p dir)) nil)
     (t
      (when (equal (gp-checkout-current-branch dir) branch)
        ;; park on the destination branch so the delete is allowed
        (let ((dest (or (ignore-errors (gp-pr-destination-branch pr)) "main")))
          (gp-checkout--git dir "checkout" dest)))
      (let ((res (gp-checkout--git dir "branch" "-D" branch)))
        (if (= (car res) 0)
            (format "local branch %s deleted" branch)
          ;; already gone, or still checked out somewhere -- say so, don't fail
          (format "local branch %s kept (%s)" branch
                  (car (split-string (cdr res) "\n")))))))))

(defun gp-ui-merge-pr (pr &optional arg)
  "Merge PR after confirmation, then tidy up the local branch.
With ARG (\\[universal-argument]), pick the merge strategy from the ones
the forge permits; otherwise its default is used and named in the
prompt.  A PR whose latest build failed needs a second confirmation.

Only the LOCAL source branch is deleted afterwards (subject to
`gp-merge-delete-local-branch'); the remote one belongs to the forge,
which removes it itself when `gp-merge-close-source-branch' asked it to."
  (let* ((full-name (gp-pr-full-name pr))
         (id (alist-get 'id pr))
         (branch (ignore-errors (gp-pr-source-branch pr)))
         (choice (gp--merge-strategy-choice full-name id arg))
         (strategy (car choice))
         (source (cdr choice))
         (states (ignore-errors (gp-commit-build-states
                                 full-name (ignore-errors (gp-pr-source-commit pr))))))
    (unless (gp-pr-open-p pr)
      (user-error "PR #%s is not open" id))
    ;; Conflicts are a hard stop: the forge would reject the merge anyway,
    ;; and nothing local can resolve them.  `unknown' (GitHub computes
    ;; mergeability lazily and answers null until it has) and nil (Bitbucket
    ;; has no such field) must NOT block -- refusing on those would make the
    ;; action unusable on a PR the forge simply had not judged yet.
    (let ((m (ignore-errors (gp-pull-request-mergeability full-name id))))
      (when (and m (null (car m)))
        (user-error "PR #%s has conflicts with %s (%s) -- resolve them first"
                    id (or (ignore-errors (gp-pr-destination-branch pr)) "the target")
                    (or (cdr m) "conflicting"))))
    ;; a red PR is mergeable, but not by accident
    (when (member "FAILED" states)
      (unless (yes-or-no-p
               (format "PR #%s's last build FAILED -- merge anyway? " id))
        (user-error "Aborted")))
    (unless (yes-or-no-p
             (format "Merge PR #%s with %s (%s)? " id
                     (or strategy "the forge's default") source))
      (user-error "Aborted"))
    (message "Merging PR #%s…" id)
    (gp-merge-pull-request full-name id strategy nil gp-merge-close-source-branch)
    (let ((tidied (when gp-merge-delete-local-branch
                    (gp--merge-delete-local-branch pr branch))))
      (gp-invalidate-pr-caches pr)
      (gp-detail-refresh)
      (message "Merged PR #%s with %s (%s)%s" id
               (or strategy "the forge's default") source
               (if tidied (format "; %s" tidied) "")))))

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
  "Open (or refresh) the pull-request list.
The buffer is displayed BEFORE the refresh is kicked off, so the
⏳ spinner and any previously fetched PRs are on screen while the
new list is fetched in the background (see `gp-refresh')."
  (interactive)
  (let ((buf (get-buffer-create gp-list-buffer-name)))
    (with-current-buffer buf
      ;; Only (re)initialise the mode on a fresh buffer: `gp-list-mode' is a
      ;; `define-derived-mode', so re-running it on an open buffer would wipe
      ;; the buffer-local `gp--prs'/refresh token and lose the list that is
      ;; keeping the view useful during the refresh.
      (unless (derived-mode-p 'gp-list-mode)
        (gp-list-mode)))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (gp-refresh))
    buf))

(defun gp--list-show-loading ()
  "Mark the current list buffer as loading with a ⏳ near the top.
Leaves existing content intact so a refresh keeps showing the old PRs
while the new ones are in flight; only a fresh buffer is blank, and
gets a single minimal line rather than a full-screen banner.
Mirrors `gp--detail-show-loading'."
  (when gp--list-loading
    (delete-overlay gp--list-loading))
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (if (= (point-min) (point-max))
          (insert (propertize "⏳ loading pull requests…\n" 'face 'shadow))
        (end-of-line)
        (let ((ov (make-overlay (point) (point))))
          (overlay-put ov 'after-string
                       (propertize "  ⏳ refreshing…" 'face 'shadow))
          (setq gp--list-loading ov))))))

(defun gp--list-clear-loading ()
  "Remove the list buffer's loading spinner overlay, if any."
  (when gp--list-loading
    (delete-overlay gp--list-loading)
    (setq gp--list-loading nil)))

(defun gp--list-render (prs uuid)
  "Redraw the current list buffer from PRS and UUID, restoring point.
Point returns to the last-visited PR when it is still in the list."
  (let ((last-id (and (boundp 'gp-helm--last-visited-pr-id)
                      gp-helm--last-visited-pr-id)))
    (setq gp--prs prs)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (gp--render-list prs uuid))
    (goto-char (or (and last-id (gp--list-find-pr-point last-id))
                   (point-min)))))

(defun gp-refresh ()
  "Fetch and redraw the PR list asynchronously, never freezing the buffer.

A ⏳ spinner is painted FIRST -- before any network call -- and the PR
list is then fetched in the background, so the buffer stays responsive
and any previously shown PRs remain readable and navigable while the
fetch is in flight.  A monotonic token (`gp--list-refresh-token')
guards against a slow earlier refresh redrawing over a newer one.

On failure the buffer keeps whatever it was already showing rather
than blanking, and the error is logged."
  (interactive)
  (let ((buf (current-buffer)))
    (cl-incf gp--list-refresh-token)
    (let ((token gp--list-refresh-token))
      (gp--list-show-loading)
      ;; Force the spinner onto the screen before anything can block.  The
      ;; identity lookup below is cached after the first call, so `g' on an
      ;; open buffer never blocks -- but a cold buffer (or a `G' that just
      ;; cleared the cache) does one identity request, and without this the
      ;; freeze would happen before the paint the user is meant to see.
      (redisplay)
      ;; Resolve the identity before handing off: passing it in keeps the
      ;; async fetch from blocking on an identity request of its own (see
      ;; `bitbucket-workspace-pull-requests-async').
      (let ((uuid (gp-user-uuid)))
        (gp-workspace-pull-requests-async
         (lambda (ok prs)
           (when (and (buffer-live-p buf)
                      (= token (buffer-local-value 'gp--list-refresh-token buf)))
             (with-current-buffer buf
               (gp--list-clear-loading)
               (if ok
                   (gp--list-render prs uuid)
                 (gp-log-error "PR list refresh failed; keeping the previous list")))))
         uuid)))))

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
  (when (boundp 'gp-helm--deploy-cache)
    (clrhash gp-helm--deploy-cache))
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
                            (gp--detail-load-local-dirty buf pr)
                            (gp--detail-load-mergeability buf pr)
                            (gp--detail-load-merge-pipelines buf pr)
                            (if gp-detail-show-pipelines
                                (gp--detail-load-pipelines buf pr)
                              (gp-log 'info "pipelines skipped: gp-detail-show-pipelines is nil"))))))))
        (gp-pull-request-async
         full-name id
         (lambda (ok value) (finish-one ok value 'pr)))
        (gp-pull-request-comments-async
         full-name id
         (lambda (ok value) (finish-one ok value 'comments)))))))

(defun gp--detail-load-merge-pipelines (buf pr)
  "Fetch the destination-branch pipeline of a merged PR into BUF.
Only for a merged PR, and only when the backend can name the merge
commit: that run is the one that deploys, and it lives on the
destination branch, so the PR-branch fetch never sees it."
  (when (and (ignore-errors (gp-pr-merged-p pr))
             gp-detail-show-pipelines)
    (let ((full-name (gp-pr-full-name pr))
          (dest (ignore-errors (gp-pr-destination-branch pr)))
          (merge-commit (ignore-errors (gp-pr-merge-commit pr))))
      (when (and dest merge-commit)
        (run-at-time
         0.3 nil
         (lambda ()
           (when (buffer-live-p buf)
             (condition-case e
                 (gp-pipeline-fetch-for-branch-async
                  full-name dest merge-commit
                  (lambda (data)
                    (when (and data (buffer-live-p buf))
                      (with-current-buffer buf
                        (when (gp--detail-buffer-shows-p buf pr)
                          (setq gp--detail-merge-pipelines data)
                          (gp--detail-rerender buf))))))
               (error (gp-log-error "merge pipelines: %s" (error-message-string e)))))))))))

(defun gp--detail-load-mergeability (buf pr)
  "Fetch PR's mergeability and fold it into BUF, redrawing if it changed.
Deferred like the other heavy data: the answer costs a request, so it
must not be asked for during a render.  A backend that cannot answer
\(Bitbucket) leaves the value nil, which the renderer treats as \"no
warning\" rather than \"conflicted\"."
  (let ((full-name (gp-pr-full-name pr))
        (id (alist-get 'id pr)))
    (run-with-idle-timer
     0 nil
     (lambda ()
       (let ((m (ignore-errors (gp-pull-request-mergeability full-name id)))
             (d (ignore-errors
                  (gp-pull-request-divergence
                   full-name
                   (ignore-errors (gp-pr-destination-branch pr))
                   (ignore-errors (gp-pr-source-branch pr))))))
         (when (and (or m d) (buffer-live-p buf))
           (with-current-buffer buf
             (when (and (gp--detail-buffer-shows-p buf pr)
                        (not (and (equal m gp--detail-mergeability)
                                  (equal d gp--detail-divergence))))
               (setq gp--detail-mergeability m
                     gp--detail-divergence d)
               (gp--detail-rerender buf)))))))))

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

(defun gp--detail-load-local-dirty (buf pr)
  "Check PR's local checkout for uncommitted work and fold it into BUF.
Deferred like the other heavy data.  Git here is a subprocess rather
than a request, but `call-process' blocks Emacs just as flatly as a
socket read does, so it stays off the visible-render path for the same
reason -- and a `git status' on a large tree is not reliably fast.

Local resolution only (`gp-local-find-checkout'), never
`gp-local-ensure-checkout': drawing a warning must not clone a repo or
switch a branch as a side effect.  No clone means nothing to warn
about, and the value stays nil.

Re-read on every refresh rather than cached: the whole point is to
reflect the tree as it is right now, and it goes stale the moment you
save a file outside Emacs."
  (run-at-time
   0.1 nil
   (lambda ()
     (when (and (buffer-live-p buf) (gp--detail-buffer-shows-p buf pr))
       (condition-case e
           (let* ((_ (require 'gp-checkout))
                  (full-name (ignore-errors (gp-pr-full-name pr)))
                  (dir (and full-name (gp-local-find-checkout full-name)))
                  (count (and dir (gp-checkout-dirty-count dir)))
                  (state (when (and count (> count 0))
                           (list :dir dir
                                 :branch (gp-checkout-current-branch dir)
                                 :count count))))
             (when (and (buffer-live-p buf) (gp--detail-buffer-shows-p buf pr))
               (with-current-buffer buf
                 ;; redraw only on a real change: a clean tree is the common
                 ;; case, and re-rendering the whole buffer to draw nothing
                 ;; would throw away point and section visibility for free
                 (unless (equal state gp--detail-local-dirty)
                   (setq gp--detail-local-dirty state)
                   (gp--detail-rerender buf)))))
         (error
          (gp-log-error "local dirty check failed: %s" (error-message-string e))))))))

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
  "Re-fetch and redraw the current detail buffer (non-blocking, force-fresh).
Also busts the overview's cached deploy verdict for this PR's commit
(see `gp-helm--deploy-cache'): every pipeline-mutating command here
(trigger, stop, run/rerun a step) calls this on completion, which is
exactly when a deploy step could have newly succeeded or a previously
successful run could have been superseded by a new one."
  (interactive)
  (when (and gp--pr (fboundp 'gp-helm--deploy-cache-bust))
    (gp-helm--deploy-cache-bust (gp-pr-source-commit gp--pr)))
  (if gp--pr
      (progn
        (gp--detail-show-loading)
        (gp--detail-refresh-async (current-buffer) gp--pr))
    (gp-show-pr gp--pr)))

(defun gp-browse-pr ()
  "Open the PR at point in a web browser."
  (interactive)
  (let ((url (gp-pr-web-url (gp-current-pr))))
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
