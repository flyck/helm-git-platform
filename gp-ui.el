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
(declare-function gp-overlay-pr "gp-overlay")
(declare-function magit-section-toggle "magit-section")
(declare-function magit-section-hide "magit-section")
(defvar magit-root-section)
(defvar magit-section-highlight-current)
(defvar gp-checkout-remote)
(declare-function magit-diff-range "magit-diff")
(declare-function magit-status "magit-status")
(declare-function magit-refresh "magit-mode")
(declare-function gp-overlay--avatar-image "gp-overlay")

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

;;;; Section types -----------------------------------------------------------

;; Both section types stash their backing object in the standard `value'
;; slot magit provides, so no extra slots are needed.
(defclass gp-pr-section (magit-section) ())
(defclass gp-comment-section (magit-section) ())
(defclass gp-file-section (magit-section) ())

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
(defvar-local gp--detail-comments nil
  "Cached comment list for the detail buffer (so it can redraw without refetch).")
(defvar-local gp--detail-marked-comment-ids nil
  "Comment ids marked for batch terminal handoff in the detail buffer.")
(defvar-local gp--detail-refresh-token 0
  "Monotonic token used to ignore stale async detail refresh callbacks.")
;;;; Formatting helpers ------------------------------------------------------

(defun gp--pr-heading (pr)
  "Return a one-line propertized heading string for PR."
  (let-alist pr
    (concat
     (propertize (format "#%s" .id) 'face 'gp-pr-id-face)
     " "
     (propertize (or .title "(no title)") 'face 'gp-pr-title-face)
     "  "
     (propertize (format "[%s]" .destination.repository.slug)
                 'face 'gp-branch-face)
     " "
     (propertize (or .author.display_name "?") 'face 'gp-author-face)
     (if (and .comment_count (> .comment_count 0))
         (format "  💬%d" .comment_count) ""))))

(defun gp--insert-pr (pr)
  "Insert a collapsible section for PR into the current buffer."
  (magit-insert-section (gp-pr-section pr)
    (magit-insert-heading (gp--pr-heading pr))
    (let-alist pr
      (insert (format "  %s → %s\n"
                      (propertize (or .source.branch.name "?")
                                  'face 'gp-branch-face)
                      (propertize (or .destination.branch.name "?")
                                  'face 'gp-branch-face))))))

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
Bitbucket :shortcode: emojis are resolved first."
  (setq text (gp-resolve-emojis text))
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
parent and one level deeper, recursively.  Orphans/top-level
comments keep their original order."
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
      (dolist (root (nreverse roots)) (walk root 0)))
    (nreverse result)))

(defun gp--detail-comment-marked-p (comment)
  "Return non-nil when COMMENT is marked for batch terminal handoff."
  (memq (alist-get 'id comment) gp--detail-marked-comment-ids))

(defun gp--detail-marked-comments ()
  "Return marked comments from the detail buffer in display order."
  (let ((ids gp--detail-marked-comment-ids))
    (cl-remove-if-not (lambda (comment) (memq (alist-get 'id comment) ids))
                      gp--detail-comments)))

(defun gp--insert-comment (comment &optional pr depth)
  "Insert a COMMENT section, with markdown body and action buttons.
PR is the enclosing pull request, needed for the reply/resolve
actions.  DEPTH (default 0) indents the whole comment to visualise
reply threads."
  (let* ((depth (or depth 0))
         (ind (make-string (* depth 4) ?\s))
         (resolved (gp-comment-resolved-p comment))
         (marked (and pr (gp--detail-comment-marked-p comment)))
         ;; prefix every line of STR with the thread indent
         (pad (lambda (str) (replace-regexp-in-string "^" ind str))))
    ;; resolved comments start collapsed (HIDE arg); TAB expands them
    (magit-insert-section (gp-comment-section comment resolved)
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
             (if resolved (propertize "  ✓ resolved" 'face 'success) "")))
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
             "reply [r]" "Reply to this comment"
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
            (insert " ")
            (if resolved
                (gp--insert-action-button
                 "reopen [x]" "Reopen this comment on the PR"
                 (lambda () (gp-ui-set-resolution pr comment nil)))
              (gp--insert-action-button
               "resolve [x]" "Resolve this comment on the PR"
               (lambda () (gp-ui-set-resolution pr comment t))))
            (when (gp-comment-own-p comment (gp-user-uuid))
              (insert " ")
              (gp--insert-action-button
               "edit [e]" "Edit this comment"
               (lambda () (gp-ui-edit-comment pr comment)))
              (insert " ")
              (gp--insert-action-button
               "delete" "Delete this comment"
               (lambda () (gp-ui-delete-comment pr comment))))
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

(defun gp--render-detail (pr comments)
  "Render PR and its COMMENTS into the current detail buffer."
  (require 'button)
  (magit-insert-section (gp-root)
    (let-alist pr
      (insert (propertize
               (concat (cond ((gp-pr-draft-p pr) "📝 ")
                             ((equal .state "MERGED") "🟣 ")
                             ((equal .state "DECLINED") "🔴 ")
                             (t "🟢 "))
                       (format "#%s" .id))
               'face 'gp-pr-id-face))
      (insert "  ")
      (insert (propertize (or .title "(no title)")
                          'face 'gp-detail-title-face))
      (insert "\n")
      (insert "🔀 "
              (propertize (format "%s → %s"
                                  (or .source.branch.name "?")
                                  (or .destination.branch.name "?"))
                          'face 'gp-branch-face)
              "    "
              (gp--avatar-string .author.links.avatar.href "👤")
              " "
              (propertize (or .author.display_name "?")
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
      (insert "\n")
      (gp--insert-action-button
       "← Back [b]" "Return to the pull-request list"
       (lambda () (gp-ui-back-to-list)))
      (insert "   ")
      (gp--insert-action-button
       "🖥  Open local repo [o]"
       "Open the current repo in Magit without changing branches"
       (lambda () (gp-detail-open-local)))
      (insert "   ")
      (gp--insert-action-button
       "🖥  Autostash & checkout [O]"
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
                 (member .state '("OPEN" nil)))
        (insert "   ")
        (if (gp-pr-draft-p pr)
            (gp--insert-action-button
             "✅ Mark ready [D]" "Mark this draft PR as ready for review"
             (lambda () (gp-ui-set-draft pr nil)))
          (gp--insert-action-button
           "📝 Convert to draft [D]" "Convert this PR back to a draft"
           (lambda () (gp-ui-set-draft pr t)))))
      ;; review actions, only on others' open PRs you can review
      (when (and (member .state '("OPEN" nil))
                 (not (gp-pr-authored-by-p pr (gp-user-uuid))))
        (let ((mine (gp-pr-my-review-state pr (gp-user-uuid))))
          (insert "\n   ")
          (if (eq mine 'approved)
              (gp--insert-action-button
               "↩ Unapprove [a]" "Retract your approval of this PR"
               (lambda () (gp-ui-set-review pr 'approved t)))
            (gp--insert-action-button
             "✅ Approve [a]" "Approve this pull request"
             (lambda () (gp-ui-set-review pr 'approved nil))))
          (insert "   ")
          (if (eq mine 'changes)
              (gp--insert-action-button
               "↩ Clear request [c]" "Retract your request for changes"
               (lambda () (gp-ui-set-review pr 'changes t)))
            (gp--insert-action-button
             "🚫 Request changes [c]" "Request changes on this pull request"
             (lambda () (gp-ui-set-review pr 'changes nil))))))
      (insert "\n\n"))
    (gp--insert-changed-files)
    (gp--insert-pipelines gp--detail-pipelines)
    (magit-insert-section (gp-comments)
      (magit-insert-heading
        (format "Comments (%d)" (length comments)))
      (if comments
          (pcase-dolist (`(,c . ,depth) (gp--comment-threads comments))
            (gp--insert-comment c pr depth))
        (insert "  (no comments)\n")))))

;;;; Modes -------------------------------------------------------------------

(defvar-keymap gp-list-mode-map
  :parent magit-section-mode-map
  "g"   #'gp-refresh
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
  "r"   #'gp-detail-reply
  "t"   #'gp-detail-send-to-terminal
  "x"   #'gp-detail-resolve
  "e"   #'gp-detail-edit
  "f"   #'gp-detail-goto-comment
  "d"   #'gp-detail-show-diff
  "X"   #'gp-detail-delete        ;; delete your own comment at point
  "D"   #'gp-detail-toggle-draft
  "a"   #'gp-detail-approve         ;; approve / unapprove (others' open PRs)
  "c"   #'gp-detail-request-changes ;; request changes / clear (others' open PRs)
  "RET" #'gp-detail-ret
  "w"   #'gp-detail-browse
  ;; pipelines (pipeline-level stop/trigger; per-step log + manual run)
  "s"   #'gp-detail-pipeline-stop
  "T"   #'gp-detail-pipeline-trigger-or-run-manual
  "m"   #'gp-detail-toggle-mark
  "l"   #'gp-detail-pipeline-step-log)

(defun gp-detail-show-diff ()
  "Show the current PR's branch diff in Magit."
  (interactive)
  (gp-ui-show-diff-in-magit gp--pr))

(defun gp-detail-delete ()
  "Delete the comment at point (your own only)."
  (interactive)
  (let ((c (gp-detail--comment-at-point)))
    (unless (gp-comment-own-p c (gp-user-uuid))
      (user-error "You can only delete your own comments"))
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
  "Context action: open a changed file, jump to an inline comment, else fold."
  (interactive)
  (let ((sec (magit-current-section)))
    (cond
     ((and (button-at (point))
           (button-get (button-at (point)) 'gp-file-path))
      (gp-detail-visit-file))
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
  (add-hook 'kill-buffer-hook #'gp--detail-cancel-pipeline-timer nil t))

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

(defun gp-ui-set-resolution (pr comment resolve)
  "Resolve (RESOLVE non-nil) or reopen COMMENT on PR, then refresh the buffer."
  (let ((full-name (gp-pr-full-name pr))
        (pid (alist-get 'id pr))
        (cid (alist-get 'id comment)))
    (if resolve
        (gp-resolve-comment full-name pid cid)
      (gp-reopen-comment full-name pid cid))
    (message "Comment %s" (if resolve "resolved" "reopened"))
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
  "Delete COMMENT on PR after confirmation, then refresh."
  (when (yes-or-no-p "Delete this comment? ")
    (gp-delete-comment (gp-pr-full-name pr)
                              (alist-get 'id pr)
                              (alist-get 'id comment))
    (message "Comment deleted")
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
  (gp-detail-refresh))

(defun gp-ui-set-review (pr kind retract)
  "Set your review on PR.
KIND is `approved' or `changes'; RETRACT non-nil withdraws it.
Refreshes the detail buffer afterwards."
  (let ((full-name (gp-pr-full-name pr))
        (id (alist-get 'id pr)))
    (pcase kind
      ('approved (gp-approve-pr full-name id retract))
      ('changes  (gp-request-changes-pr full-name id retract)))
    (message "PR #%s %s" id
             (pcase (cons kind retract)
               ('(approved . nil) "approved")
               ('(approved . t)   "approval retracted")
               ('(changes . nil)  "changes requested")
               ('(changes . t)    "changes-request cleared")))
    (gp-detail-refresh)))

(defun gp-detail-approve ()
  "Approve the current PR, or retract if you already approved it."
  (interactive)
  (let ((pr gp--pr))
    (when (gp-pr-authored-by-p pr (gp-user-uuid))
      (user-error "You cannot approve your own PR"))
    (gp-ui-set-review pr 'approved
                      (eq (gp-pr-my-review-state pr (gp-user-uuid)) 'approved))))

(defun gp-detail-request-changes ()
  "Request changes on the current PR, or retract if you already did."
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

(defconst gp-list-buffer-name "*PRs*")

(defcustom gp-detail-buffer-title-width 40
  "Max characters of the PR title shown in the detail buffer name."
  :type 'integer :group 'bitbucket)

(defun gp--detail-buffer-name (pr)
  "Return the detail buffer name for PR.
Includes the title and repo so buffers are easy to tell apart, e.g.
`*PR #239 add widget toggle (web-frontend)*'."
  (let* ((id (alist-get 'id pr))
         (title (or (alist-get 'title pr) ""))
         (full-name (or (ignore-errors (gp-pr-full-name pr)) ""))
         (repo (if (string-match "/\\([^/]+\\)\\'" full-name)
                   (match-string 1 full-name)
                 full-name))
         (title (if (> (length title) gp-detail-buffer-title-width)
                    (concat (substring title 0 (1- gp-detail-buffer-title-width)) "…")
                  title)))
    (format "*PR #%s%s%s*"
            id
            (if (string-empty-p title) "" (concat " " title))
            (if (string-empty-p repo) "" (format " (%s)" repo)))))

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
  "Fetch and redraw the PR list."
  (interactive)
  (let* ((uuid (gp-user-uuid))
         (prs (gp-workspace-pull-requests)))
    (setq gp--prs prs)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (gp--render-list prs uuid))
    (goto-char (point-min))))

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

(defun gp--detail-pipelines-running-p (data)
  "Non-nil when any current-commit pipeline in DATA is not finished."
  (cl-some (lambda (entry)
             (not (gp-pipeline-finished-p (car entry))))
           (plist-get data :current)))

(defun gp--detail-cancel-pipeline-timer ()
  "Cancel the current detail buffer's pipeline poll timer, if any."
  (when (timerp gp--detail-pipeline-timer)
    (cancel-timer gp--detail-pipeline-timer)
    (setq gp--detail-pipeline-timer nil)))

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
  "Hide every resolved-comment section in the current detail buffer."
  (when (slot-boundp magit-root-section 'children)
    (dolist (sec (oref magit-root-section children))
      (gp--collapse-resolved-walk sec))))

(defun gp--collapse-resolved-walk (section)
  "Collapse SECTION if it is a resolved comment; recurse into children."
  (when (and (object-of-class-p section 'gp-comment-section)
             (gp-comment-resolved-p (oref section value)))
    (magit-section-hide section))
  (when (slot-boundp section 'children)
    (dolist (child (oref section children))
      (gp--collapse-resolved-walk child))))

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

(defun gp-show-pr (pr)
  "Display PR's detail buffer; load comments/stats without blocking.

The buffer shows a small ⏳ spinner near the title while the
comments and stats are fetched on an idle timer; existing content
stays visible during a refresh -- so it never freezes or blanks."
  (let ((buf (get-buffer-create (gp--detail-buffer-name pr))))
    (with-current-buffer buf
      (gp-detail-mode)
      (setq gp--pr pr)
      (gp--detail-show-loading))
    ;; reuse the current window so a full-frame helm leads to a full-frame
    ;; detail view instead of splitting
    (pop-to-buffer-same-window buf)
    (run-with-idle-timer
     0.05 nil
     (lambda ()
       (when (buffer-live-p buf)
         (condition-case e
             (let* ((full-name (gp-pr-full-name pr))
                    (id (alist-get 'id pr))
                    (comments (gp-pull-request-comments full-name id))
                    (stats (when gp-detail-show-stats
                             (ignore-errors
                               (gp-pull-request-stats full-name id))))
                    (diff (when gp-detail-show-file-diffs
                            (ignore-errors
                              (gp-split-diff-by-file
                               (gp-pull-request-diff full-name id))))))
               ;; render the main content first; pipelines (N+1 calls) are
               ;; fetched in a SEPARATE deferred step so they never delay or
               ;; block the comments/diff view.
               (gp--render-detail-into buf pr comments stats diff
                                       (with-current-buffer buf
                                         gp--detail-pipelines))
               (with-current-buffer buf (gp--detail-clear-loading))
               (when gp-detail-show-pipelines
                 (gp--detail-load-pipelines buf pr)))
           (error
            (with-current-buffer buf
              (gp--detail-clear-loading)
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert (propertize
                         (format "\n  error: %s\n" (error-message-string e))
                         'face 'error))))
            (gp-log-error "detail load failed: %s"
                                 (error-message-string e)))))))
    buf))

(defun gp--detail-load-pipelines (buf pr)
  "Fetch PR's pipelines on a separate idle timer and fold them into BUF.
Kept separate from the main load so the N+1 pipeline calls never
block or delay the comments/diff view.  While any current-commit
pipeline is still running, schedules a poll so the buffer tracks a
live deployment without a manual refresh."
  (run-with-idle-timer
   0.1 nil
   (lambda ()
     (when (buffer-live-p buf)
       (condition-case e
           (let ((data (gp-pipeline-fetch-for-pr pr)))
             (with-current-buffer buf
               (setq gp--detail-pipelines data)
               (gp--detail-rerender buf)
               (gp--detail-cancel-pipeline-timer)
               (when (and (> gp-detail-pipeline-poll-interval 0)
                          (gp--detail-pipelines-running-p data))
                 (setq gp--detail-pipeline-timer
                       (run-with-timer
                        gp-detail-pipeline-poll-interval nil
                        #'gp--detail-load-pipelines buf pr)))))
         (error
           (gp-log-error "pipeline load failed: %s"
                         (error-message-string e))))))))

(defun gp--detail-refresh-async (buf pr)
  "Refresh BUF's PR detail asynchronously, preserving existing content.
This refreshes the PR object and comments in the background, then rerenders
using the buffer's already-cached stats, diff and pipelines."
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
          (failed nil))
      (cl-labels ((finish-one (ok value kind)
                    (when (and (buffer-live-p buf)
                               (= token (buffer-local-value 'gp--detail-refresh-token buf)))
                      (unless ok (setq failed t))
                      (pcase kind
                        ('pr (setq new-pr value))
                        ('comments (setq new-comments value)))
                      (setq pending (1- pending))
                      (when (zerop pending)
                        (with-current-buffer buf
                          (gp--detail-clear-loading)
                          (if failed
                              (let ((inhibit-read-only t))
                                (goto-char (point-max))
                                (insert (propertize "\n  refresh failed; showing cached content\n"
                                                    'face 'error)))
                            (gp--render-detail-into
                             buf (or new-pr old-pr) (or new-comments old-comments)
                             gp--detail-stats gp--detail-diff gp--detail-pipelines)
                            (when gp-detail-show-pipelines
                              (gp--detail-load-pipelines buf (or new-pr old-pr)))))))))
        (bitbucket-pull-request-async
         full-name id
         (lambda (ok value) (finish-one ok value 'pr)))
        (bitbucket-pull-request-comments-async
         full-name id
         (lambda (ok value) (finish-one ok value 'comments)))))))

(defun gp-detail-refresh ()
  "Re-fetch and redraw the current detail buffer (non-blocking)."
  (interactive)
  (if (and gp--pr gp--detail-comments)
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
