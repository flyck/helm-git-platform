;;; gp-helm.el --- Helm front-end for pull requests -*- lexical-binding: t; -*-

;;; Commentary:

;; The primary entry point: `gp-helm' opens a Helm session listing
;; the workspace pull requests (grouped into "needs my review" and
;; "mine").  From a selected PR you can:
;;
;;   * checkout its branch (via the checkout service);
;;   * open the local working copy;
;;   * browse it on the web;
;;   * drill into a second Helm session of its changed files, or its
;;     comments -- choosing a file/comment opens the magit-section detail
;;     buffer and draws the inline-comment overlays.
;;
;; Candidate construction is factored into pure functions
;; (`gp-helm--pr-candidates', `--file-candidates',
;; `--comment-candidates') that the tests drive directly; only the
;; `helm' invocations and the actions touch global state.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'helm)
(require 'helm-source)
(require 'bitbucket-api)
(require 'git-platform)
(require 'gp-local)
(require 'gp-overlay)
(require 'gp-ui)

(declare-function helm "helm")
(declare-function helm-build-sync-source "helm-source")
(declare-function helm-run-after-exit "helm-core")
(declare-function helm-get-selection "helm-core")
(declare-function helm-update "helm-core")
(declare-function gp-compose "gp-compose")
(declare-function gp-create-pr "gp-create")
(declare-function gp-watch--repo-for-path "gp-watch")
(declare-function gp-watch--current-branch "gp-watch")
(declare-function gp-watch--pr-for "gp-watch")
(declare-function gp-local-find-checkout "gp-local")
(defvar helm-map)
(defvar helm-alive-p)

(defvar gp-helm--last-visited-pr-id nil
  "Id of the last PR opened via `gp-show-pr', so the list can preselect it.
Set from `gp-ui.el' (which has no Helm dependency) so leaving a
detail buffer back to the overview lands the cursor back on the
PR you came from.")

;;;; Faces --------------------------------------------------------------------

(defface gp-helm-id-face '((t :inherit helm-grep-lineno))
  "Face for the PR id column." :group 'bitbucket-faces)
(defface gp-helm-title-face '((t :inherit default))
  "Face for the PR title column." :group 'bitbucket-faces)
(defface gp-helm-repo-face '((t :inherit helm-grep-file))
  "Face for the repository column." :group 'bitbucket-faces)
(defface gp-helm-author-face '((t :inherit font-lock-variable-name-face))
  "Face for the author column." :group 'bitbucket-faces)
(defface gp-helm-draft-face '((t :inherit shadow :slant italic))
  "Face for draft PR rows." :group 'bitbucket-faces)
(defface gp-helm-comments-face '((t :inherit warning))
  "Face for the comment-count badge." :group 'bitbucket-faces)

;;;; Helm buffer names --------------------------------------------------------

;; All tagged via `gp--buffer-name' so every buffer this package opens shares
;; one searchable prefix.  `gp-helm-buffer' is looked up by name in
;; `gp-helm--title-width', so it lives in a constant rather than being
;; spelled out at each call site.

(defconst gp-helm-buffer (gp--buffer-name "helm")
  "Name of the main Helm PR-list buffer.")

(defun gp-helm--buffer (suffix)
  "Return the tagged Helm buffer name for SUFFIX, e.g. \"files\"."
  (gp--buffer-name (format "helm %s" suffix)))

;;;; Pure candidate builders -------------------------------------------------

(defcustom gp-helm-title-width 52
  "Fallback column width for the PR title when the window width is unknown.
Normally the title column auto-grows to fill the available width
\(see `gp-helm--title-width')."
  :type 'integer :group 'bitbucket)

(defcustom gp-helm-title-min-width 24
  "Minimum width the auto-growing title column will shrink to."
  :type 'integer :group 'bitbucket)

(defcustom gp-helm-repo-width 22
  "Column width for the repository slug in the Helm list."
  :type 'integer :group 'bitbucket)

(defcustom gp-helm-title-reserve 28
  "Extra columns held back from the title for the trailing badges.
Covers the reviewer tally, comment count and pipeline bubble,
whose emoji are double-width and easy to under-count; raise it if
rows still overflow on the right."
  :type 'integer :group 'bitbucket)

(defun gp-helm--title-width ()
  "Compute the title column width, growing to fill the window.
The title takes the window width minus the fixed columns (bubble,
avatar, id, repo, author, separators) and `gp-helm-title-reserve'
for the trailing badges, never below `gp-helm-title-min-width'."
  (let ((win (and (window-live-p (get-buffer-window (get-buffer gp-helm-buffer)))
                  (window-body-width (get-buffer-window (get-buffer gp-helm-buffer))))))
    (if (not win)
        gp-helm-title-width
      ;; bubble 3 + avatar 3 + id 6 + repo + author 16 + separators 8
      (max gp-helm-title-min-width
           (- win 3 3 6 gp-helm-repo-width 16 8
              gp-helm-title-reserve)))))

(defun gp-helm--pad (s width &optional face)
  "Return S padded/truncated to WIDTH columns, propertized with FACE."
  (let ((s (truncate-string-to-width (or s "") width nil ?\s "…")))
    (if face (propertize s 'face face) s)))

(defcustom gp-helm-full-frame t
  "When non-nil, `gp-helm' uses the whole frame.
This gives the list maximum vertical space and the widest
possible title column."
  :type 'boolean :group 'bitbucket)

(defcustom gp-helm-show-avatars nil
  "When non-nil and on a graphical display, show author avatars in the list.

Off by default: avatars are fetched over HTTP and, with many PRs,
that can make the list slow to first paint.  The avatar cache is
shared with the inline-comment overlays, so once those are warm
turning this on is cheap."
  :type 'boolean :group 'bitbucket)

(defun gp-helm--avatar (pr)
  "Return a leading avatar image string for PR's author, or \"\" (text only)."
  (let ((img (and gp-helm-show-avatars
                  (gp-overlay--avatar-image (gp-pr-author-avatar pr)))))
    (if img (concat (propertize " " 'display img) " ") "")))

(defvar gp-helm--pipeline-cache (make-hash-table :test 'equal)
  "commit-hash -> pipeline state symbol (see `gp-build-states-summary').")

(defun gp-helm--pipeline-bubble (pr)
  "Return a colored pipeline-status bubble for PR's latest commit.
Reads the async `gp-helm--pipeline-cache'; a neutral bubble
shows until the status arrives."
  (let* ((hash (let-alist pr .source.commit.hash))
         (state (and hash (gethash hash gp-helm--pipeline-cache 'unknown))))
    (pcase state
      ('failed     (propertize "🔴" 'help-echo "Pipeline failed"))
      ('running    (propertize "🔵" 'help-echo "Pipeline running"))
      ('stopped    (propertize "⚪" 'help-echo "Pipeline cancelled"))
      ('successful (propertize "🟢" 'help-echo "Pipeline succeeded"))
      ('nil        "  ")              ;; resolved: no pipeline ran
      (_           (propertize "⚫" 'help-echo "Pipeline status loading…")))))

(defun gp-helm--pr-display (pr &optional draft)
  "Return an aligned, multi-column, propertized Helm line for PR.
DRAFT non-nil dims the whole row.  A leading author avatar is
shown on graphical displays."
  (let-alist pr
    (let* ((bubble (gp-helm--pipeline-bubble pr))
           (avatar (gp-helm--avatar pr))
           (id (gp-helm--pad (format "#%s" .id) 6 'gp-helm-id-face))
           (title (gp-helm--pad .title (gp-helm--title-width)
                                       'gp-helm-title-face))
           (repo (gp-helm--pad (gp-pr-repo-slug pr)
                                      gp-helm-repo-width
                                      'gp-helm-repo-face))
           (author (gp-helm--pad (gp-pr-author-name pr) 16
                                        'gp-helm-author-face))
           (count (gp-pr-comment-count pr))
           (badge (if (and count (> count 0))
                      (propertize (format " 💬%d" count)
                                  'face 'gp-helm-comments-face)
                    ""))
           (reviews (gp-helm--review-badge pr))
           (line (format "%s %s%s  %s  %s  %s%s%s"
                         bubble avatar id title repo author reviews badge)))
      (if draft (propertize line 'face 'gp-helm-draft-face) line))))

(defcustom gp-helm-review-style 'tally
  "How reviewer state is shown in the PR list.
`tally' shows counts (✅2 ✗1 ⏳1); `dots' shows one emoji per
reviewer (✅✅⏳); nil hides it."
  :type '(choice (const :tag "Counts" tally)
                 (const :tag "One per reviewer" dots)
                 (const :tag "Hidden" nil))
  :group 'bitbucket)

(defvar gp-helm--review-tally-cache (make-hash-table :test 'eql)
  "PR id -> review tally plist (see `gp-pr-review-tally').
Bitbucket's tally is embedded in the PR object already (free, no
network); GitHub's needs a fetch per PR (see `gp-pr-review-tally-async'),
which would block the list one row at a time if called directly from
`gp-helm--review-badge' -- so it's populated asynchronously here,
exactly like `gp-helm--pipeline-cache' already does for build status.")

(defun gp-helm--format-review-tally (tally)
  "Return the reviewer badge string for TALLY, honouring `gp-helm-review-style'."
  (let ((a (plist-get tally :approved))
        (c (plist-get tally :changes))
        (p (plist-get tally :pending)))
    (cond
     ((null gp-helm-review-style) "")
     ((zerop (+ a c p)) "")
     ((eq gp-helm-review-style 'dots)
      (concat "  "
              (apply #'concat (make-list a "✅"))
              (apply #'concat (make-list c "❌"))
              (apply #'concat (make-list p "⏳"))))
     (t                                 ;; tally
      (concat "  "
              (if (> a 0) (format "✅%d " a) "")
              (if (> c 0) (format "❌%d " c) "")
              (if (> p 0) (format "⏳%d" p) ""))))))

(defun gp-helm--review-badge (pr)
  "Return a reviewer approval/changes/pending badge string for PR.
Reads the async `gp-helm--review-tally-cache'; a blank badge shows
until the status arrives (see `gp-helm--scan-review-tallies-async')."
  (let* ((id (alist-get 'id pr))
         (cached (gethash id gp-helm--review-tally-cache)))
    (if cached
        (gp-helm--format-review-tally cached)
      "")))

(defun gp-helm--scan-review-tallies-async (prs)
  "Fetch each PR's review tally in parallel, caching by id and refreshing live.
Mirrors `gp-helm--scan-pipelines-async''s pattern.  Bitbucket's
`gp-pr-review-tally-async' calls back immediately with its already-free
result, so this costs nothing extra there; GitHub's does a real
per-PR fetch."
  (dolist (pr prs)
    (let ((id (alist-get 'id pr)))
      (unless (gethash id gp-helm--review-tally-cache)
        (gp-pr-review-tally-async
         pr
         (lambda (tally)
           (puthash id tally gp-helm--review-tally-cache)
           (gp-helm--refresh-if-alive)))))))

(defvar gp-helm--reviewers-cache (make-hash-table :test 'eql)
  "PR id -> per-reviewer plists (see `gp-pr-reviewers-async').
The aggregate tally in `gp-helm--review-tally-cache' cannot say WHO
voted, which is what `gp-helm--covered-by-others-p' needs to exclude
the user's own vote -- hence a second cache over the same data.
Bitbucket answers from the already-embedded participants (no network);
GitHub does a real per-PR fetch, exactly as the tally scan does.")

(defun gp-helm--scan-reviewers-async (prs)
  "Fetch each PR's per-reviewer breakdown in parallel, caching by id.
Mirrors `gp-helm--scan-review-tallies-async'; feeds
`gp-helm--covered-by-others-p'."
  (dolist (pr prs)
    (let ((id (alist-get 'id pr)))
      (unless (gethash id gp-helm--reviewers-cache)
        (gp-pr-reviewers-async
         pr
         (lambda (reviewers)
           ;; cache the empty list as an empty list, not nil: nil reads as
           ;; "not fetched yet" and would refetch on every redraw
           (puthash id (or reviewers 'none) gp-helm--reviewers-cache)
           (gp-helm--refresh-if-alive)))))))

(defun gp-helm--pr-search-tail (pr)
  "Return an invisible, searchable suffix of PR's full untruncated fields.
The visible row truncates the repo slug, title and author to fixed
column widths (see `gp-helm--pad'), so Helm -- which matches the
display string -- cannot match text past the cut.  Appending the
full fields as an invisible tail makes the whole slug/title/author
matchable regardless of truncation."
  (let-alist pr
    (propertize
     (concat " " (mapconcat #'identity
                            (delq nil (list (format "#%s" .id)
                                            .destination.repository.slug
                                            .title
                                            .author.display_name))
                            " "))
     'invisible t)))

(defun gp-helm--pr-candidates (prs &optional draft)
  "Return helm candidates (DISPLAY . PR) for PRS.
DRAFT non-nil styles the rows as drafts."
  (mapcar (lambda (pr)
            (cons (concat (gp-helm--pr-display pr draft)
                          (gp-helm--pr-search-tail pr))
                  pr))
          prs))

(defun gp-helm--header (label prs)
  "Return a section header string LABEL with the count of PRS."
  (format "%s (%d)" label (length prs)))

(defun gp-helm--diff-files (diff)
  "Parse a unified DIFF string into a list of changed file paths.
Reads the \"+++ b/PATH\" lines, dropping the /dev/null deletions."
  (let ((files '()))
    (dolist (line (split-string (or diff "") "\n"))
      (when (string-match "\\`\\+\\+\\+ b/\\(.+\\)\\'" line)
        (let ((f (match-string 1 line)))
          (unless (string= f "/dev/null")
            (push f files)))))
    (nreverse files)))

(defun gp-helm--file-candidates (diff)
  "Return helm candidates (PATH . PATH) for the files changed in DIFF."
  (mapcar (lambda (f) (cons f f)) (gp-helm--diff-files diff)))

(defcustom gp-helm-comment-hint " · RET open · C-c r reply"
  "Trailing hint appended to each comment line, or nil for none."
  :type '(choice (const :tag "No hint" nil) string) :group 'bitbucket)

(defun gp-helm--comment-display (comment)
  "Return a one-line helm display string for COMMENT, with an action hint."
  (let-alist comment
    (concat
     (format "%-22s %s  %s"
             (truncate-string-to-width
              (gp--comment-location comment) 22 nil ?\s "…")
             (or .user.display_name "?")
             (car (split-string (string-trim (or .content.raw "")) "\n")))
     (if gp-helm-comment-hint
         (propertize gp-helm-comment-hint 'face 'shadow)
       ""))))

(defun gp-helm--comment-candidates (comments)
  "Return helm candidates (DISPLAY . COMMENT) for COMMENTS."
  (mapcar (lambda (c) (cons (gp-helm--comment-display c) c)) comments))

;;;; Drill-down sessions -----------------------------------------------------

(defun gp-helm-files (pr)
  "Helm session over the files changed in PR.
Selecting a file checks the branch out (if possible) and visits
the file, then overlays its inline comments."
  (let* ((diff (gp-pull-request-diff (gp-pr-full-name pr)
                                            (alist-get 'id pr)))
         (cands (gp-helm--file-candidates diff)))
    (unless cands
      (user-error "No changed files found for PR #%s (empty diff?)"
                  (alist-get 'id pr)))
    (helm :sources
          (helm-build-sync-source
              (format "Changed files · PR #%s   (C-c C-b back · C-c g reload)"
                      (alist-get 'id pr))
            :candidates cands
            :action
            (list (cons "Visit file in checkout"
                        (lambda (path) (gp-helm--visit-file pr path)))
                  (cons "← Back to PR list"
                        (lambda (_p) (gp-helm))))
            :keymap (gp-helm--drilldown-keymap
                     (lambda () (gp-helm-files pr))))
          :buffer (gp-helm--buffer "files"))))

(defun gp-helm--visit-file (pr path)
  "Open PATH from PR's local checkout (cloning/checkout if needed)."
  (let ((dir (gp-local-ensure-checkout pr)))
    (find-file (expand-file-name path dir))
    (gp-overlay-pr pr)))

(defun gp-helm-comments (pr)
  "Helm session over PR's comments.
Selecting an inline comment jumps to it in the checkout (with
overlays); a general comment opens the detail buffer."
  (let* ((comments (gp-pull-request-comments
                    (gp-pr-full-name pr) (alist-get 'id pr)))
         (cands (gp-helm--comment-candidates comments)))
    (unless cands
      (user-error "PR #%s has no comments yet" (alist-get 'id pr)))
    (helm :sources
          (helm-build-sync-source
              (format "Comments · PR #%s   (C-c C-b back · C-c g reload)"
                      (alist-get 'id pr))
            :candidates cands
            :action
            (list (cons "Open comment in browser (focused)"
                        (lambda (c) (gp-helm--browse-comment pr c)))
                  (cons "Reply to comment              (C-c r)"
                        (lambda (c) (gp-helm--reply-comment pr c)))
                  (cons "Go to comment in checkout     (C-c f)"
                        (lambda (c) (gp-helm--goto-comment pr c)))
                  (cons "Open PR detail buffer"
                        (lambda (_c) (gp-show-pr pr)))
                  (cons "← Back to PR list             (C-c C-b)"
                        (lambda (_c) (gp-helm))))
            :keymap (gp-helm--comments-keymap pr))
          :buffer (gp-helm--buffer "comments"))))

(defun gp-helm--comment-url (pr comment)
  "Return the web URL focusing COMMENT on PR, with a fallback anchor."
  (or (let-alist comment .links.html.href)
      (let ((pr-url (let-alist pr .links.html.href))
            (cid (alist-get 'id comment)))
        (when (and pr-url cid)
          (format "%s#comment-%s" pr-url cid)))))

(defun gp-helm--browse-comment (pr comment)
  "Open COMMENT on PR in the browser, focused via its anchor."
  (let ((url (gp-helm--comment-url pr comment)))
    (if url (browse-url url)
      (user-error "No web link for this comment"))))

(defun gp-helm--comments-keymap (pr)
  "Keymap for the comments drill-down: reply/goto/back/reload bindings."
  (let ((map (gp-helm--drilldown-keymap
              (lambda () (gp-helm-comments pr)))))
    (define-key map (kbd "C-c r")
                (lambda () (interactive)
                  (let ((c (helm-get-selection)))
                    (helm-run-after-exit
                     (lambda () (gp-helm--reply-comment pr c))))))
    (define-key map (kbd "C-c f")
                (lambda () (interactive)
                  (let ((c (helm-get-selection)))
                    (helm-run-after-exit
                     (lambda () (gp-helm--goto-comment pr c))))))
    map))

(defun gp-helm--reply-comment (pr comment)
  "Open a compose buffer replying to COMMENT on PR."
  (require 'gp-compose)
  (let-alist comment
    (gp-compose
     (list :full-name (gp-pr-full-name pr)
           :id (alist-get 'id pr)
           :parent (alist-get 'id comment)
           :inline (when .inline.path
                     (cons .inline.path (or .inline.to .inline.from)))))))

(defun gp-helm--drilldown-keymap (&optional refresh)
  "Return a keymap for drill-down Helm sessions.
Binds C-c C-b to go back to the PR list and, when REFRESH (a
thunk) is given, C-c g to reload the current drill-down.

We use C-c g rather than a bare `g' because in a Helm session
ordinary keys are the search pattern -- binding `g' alone would
stop you from typing it."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c C-b")
                (lambda () (interactive)
                  (helm-run-after-exit #'gp-helm)))
    (when refresh
      (define-key map (kbd "C-c g")
                  (lambda () (interactive) (helm-run-after-exit refresh))))
    map))

(defun gp-helm--goto-comment (pr comment)
  "Jump to COMMENT's location for PR, drawing overlays, or show detail."
  (let-alist comment
    (if .inline.path
        (progn
          (gp-helm--visit-file pr .inline.path)
          (when-let* ((line (or .inline.to .inline.from)))
            (goto-char (point-min))
            (forward-line (1- line))))
      (gp-show-pr pr))))

;;;; Actions on a PR ---------------------------------------------------------

(defun gp-helm--pr-actions ()
  "Return the helm action alist for a selected PR."
  ;; first entry is the default action (RET / mouse click)
  (list
   (cons "Open detail buffer"      #'gp-show-pr)
   (cons "Browse files (helm)"     #'gp-helm-files)
   (cons "Browse comments (helm)"  #'gp-helm-comments)
   (cons "Checkout branch & open"
         (lambda (pr)
           (let* ((res (gp-local-checkout-branch pr))
                  (dir (plist-get res :dir)))
             (message "%s %s%s"
                      (if (plist-get res :ok) "Checked out in" "FAILED in")
                      dir
                      (if (plist-get res :stashed) " (stashed work)" ""))
             (when (and (plist-get res :ok) dir)
               (funcall gp-open-function dir)))))
   (cons "Open local checkout"     #'gp-local-open)
   (cons "Overlay inline comments" #'gp-overlay-pr)
   (cons "Comment on PR (general)"
         (lambda (pr)
           (require 'gp-compose)
           (gp-compose
            (list :full-name (gp-pr-full-name pr)
                  :id (alist-get 'id pr)))))
   (cons "Browse on web"
         (lambda (pr) (browse-url (let-alist pr .links.html.href))))))

;;;; Entry point -------------------------------------------------------------

(defun gp-helm--source (label prs actions &optional draft keymap)
  "Build a Helm source named LABEL over PRS using ACTIONS.
The header shows the live count; DRAFT styles the rows as drafts;
KEYMAP overrides the source keymap.  Returns nil when PRS is empty
so empty sections are omitted."
  (when prs
    (helm-build-sync-source label
      ;; volatile + a candidates function so the title column is recomputed
      ;; once the helm window exists (auto-grow to full width)
      :candidates (lambda () (gp-helm--pr-candidates prs draft))
      :volatile t
      :action actions
      :nomark t
      :keymap (or keymap (gp-helm--list-keymap nil))
      :header-name (lambda (name)
                     (concat (gp-helm--header name prs)
                             "   (C-c g reload · C-c G refresh · C-c m merged)")))))

(defun gp-helm--prs-for-branch (prs branch)
  "Return the PRs from PRS whose source branch is BRANCH."
  (cl-remove-if-not (lambda (pr)
                      (equal (gp-pr-source-branch pr) branch))
                    prs))

(defun gp-helm--list-keymap (include-merged)
  "Keymap for the main PR list.
C-c g reloads (clearing the PR-list cache only); C-c G does a full
refresh (clearing every cache across all layers, see
`gp-reset-caches') -- use this when a repo or PR is missing
because a stale cache (e.g. the 24h repo-list cache) predates it.
C-c m toggles whether MERGED/DECLINED PRs are shown \(currently
INCLUDE-MERGED)."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c g")
                (lambda () (interactive)
                  (helm-run-after-exit
                   (lambda () (gp-cache-clear) (gp-helm--list include-merged)))))
    (define-key map (kbd "C-c G")
                (lambda () (interactive)
                  (helm-run-after-exit
                   (lambda () (gp-reset-caches) (gp-helm--list include-merged)))))
    (define-key map (kbd "C-c m")
                (lambda () (interactive)
                  (helm-run-after-exit
                   (lambda () (gp-helm--list (not include-merged))))))
    map))

(defvar gp-helm--reviewing-cache nil
  "Async-filled list of reviewer PRs for the current `gp-helm' run.
Symbol `loading' while the background scan runs.")

(defvar gp-helm--reviewing-uuid nil
  "The uuid the current reviewing scan belongs to.
Needed by the candidate functions to tell the user's own vote apart
from everyone else's.")

(defun gp-helm--reviewing-candidates ()
  "Helm `:candidates' for the reviewing source: reads the async cache.
Only PRs still needing the user's attention; ones already approved
are served by `gp-helm--voted-candidates'."
  (cond
   ((eq gp-helm--reviewing-cache 'loading)
    (list (cons (propertize "  ⏳ scanning repositories for review requests…"
                            'face 'shadow)
                nil)))
   (gp-helm--reviewing-cache
    (gp-helm--pr-candidates
     (car (gp-helm--partition-reviewing gp-helm--reviewing-cache
                                        gp-helm--reviewing-uuid))))
   (t nil)))

(defun gp-helm--covered-candidates ()
  "Helm `:candidates' for PRs other reviewers have already settled."
  (when (listp gp-helm--reviewing-cache)
    (gp-helm--pr-candidates
     (nth 2 (gp-helm--partition-reviewing gp-helm--reviewing-cache
                                          gp-helm--reviewing-uuid)))))

(defun gp-helm--voted-candidates ()
  "Helm `:candidates' for PRs the user has already approved."
  (when (listp gp-helm--reviewing-cache)
    (gp-helm--pr-candidates
     (nth 1 (gp-helm--partition-reviewing gp-helm--reviewing-cache
                                          gp-helm--reviewing-uuid)))))

(defcustom gp-helm-create-from-magit t
  "When non-nil, `gp-helm' from a magit buffer on a branch with no
open PR opens the PR-creation mask instead of the workspace list.
Set to nil to always get the PR list."
  :type 'boolean :group 'bitbucket)

(defun gp-helm--magit-create-context ()
  "Return (DIR FULL-NAME BRANCH) to offer PR creation, or nil.
Non-nil only in a magit buffer whose repo+branch resolve, whose
branch is not the repo's default branch, AND that has no open PR.

The \"has no open PR\" check uses the SAME source as
`gp-helm--magit-branch-prs' (the actual repo PR list filtered by
source branch), not a separate lookup -- so the create-vs-open
decision can never disagree with itself and drop you into the
create mask for a branch that already has a PR."
  (when (and gp-helm-create-from-magit
             (derived-mode-p 'magit-mode)
             default-directory)
    (require 'gp-watch)
    (let* ((dir (expand-file-name default-directory))
           (full-name (gp-watch--repo-for-path dir))
           (branch (and full-name (gp-watch--current-branch dir))))
      (when (and full-name branch
                 (not (member branch '("main" "master")))
                 (not (equal branch (gp-repo-default-branch full-name)))
                 (null (gp-helm--magit-branch-prs)))
        (let ((root (or (and (fboundp 'gp-local-find-checkout)
                             (gp-local-find-checkout full-name))
                        (locate-dominating-file dir ".git")
                        dir)))
          (list (directory-file-name root) full-name branch))))))

;;;###autoload
(defun gp-helm (&optional include-merged)
  "List workspace pull requests with Helm, without freezing.

When called from a magit buffer on a branch that has no open pull
request (and is not the repo's default branch), open the
PR-creation mask instead -- see `gp-helm-create-from-magit'.

Your authored PRs and drafts appear immediately; the \"Needs my
review\" section shows a loading row and fills in once a
background scan of recent repositories finishes (it has no single
fast endpoint -- see `bitbucket-reviewing-pull-requests').

Sections are colour-coded with aligned id / title / repo / author
columns.  RET (or click) opens the PR detail buffer; more actions
are on the action menu (\\<helm-map>\\[helm-select-action]).

By default only OPEN PRs are shown; with a prefix argument, or
\\`C-c m' in the list, INCLUDE-MERGED also shows MERGED/DECLINED."
  (interactive "P")
  (require 'helm)
  (let (branch-prs ctx)
    (cond
     ;; magit buffer, branch with exactly one open PR -> open it directly.
     ;; Checked BEFORE create so an existing PR always wins.
     ((and (not include-merged)
           (setq branch-prs (gp-helm--magit-branch-prs))
           (= (length branch-prs) 1))
      (require 'gp-ui)
      (gp-show-pr (car branch-prs)))
     ;; magit buffer, branch with NO open PR -> offer to create one.
     ;; (`gp-helm--magit-create-context' re-checks branch-prs is empty.)
     ((and (not include-merged)
           (null branch-prs)
           (setq ctx (gp-helm--magit-create-context)))
      (require 'gp-create)
      (gp-create-pr (nth 0 ctx) (nth 1 ctx) (nth 2 ctx)))
     ;; several PRs on the branch, default branch, or non-magit -> full list
     (t (gp-helm--list include-merged)))))

(defun gp-helm--magit-branch-prs ()
  "Return the open PRs for the current magit buffer's repo+branch, or nil.
Nil outside a magit buffer, or when the repo/branch cannot be
resolved.  Used so `gp-helm' can jump straight to a lone branch PR."
  (when (and (derived-mode-p 'magit-mode) default-directory)
    (require 'gp-watch)
    (let* ((dir (expand-file-name default-directory))
           (full-name (gp-watch--repo-for-path dir))
           (branch (and full-name (gp-watch--current-branch dir))))
      (when (and full-name branch (fboundp 'gp-repo-pull-requests))
        (cl-remove-if-not
         (lambda (pr) (equal (gp-pr-source-branch pr) branch))
         (gp-repo-pull-requests full-name))))))

(defun gp-helm--list (include-merged)
  "Show the Helm workspace PR list.  See `gp-helm' for INCLUDE-MERGED."
  (let* ((uuid (gp-user-uuid))
         (states (if include-merged '("OPEN" "MERGED" "DECLINED") '("OPEN")))
         (mine-prs (gp-cache-with-cache
                    (list 'mine uuid include-merged)
                    (lambda ()
                      (gp-workspace-pull-requests
                       uuid (if include-merged nil "OPEN")))))
         (cat (gp-categorize-pull-requests mine-prs uuid))
         (actions (gp-helm--pr-actions))
         (km (gp-helm--list-keymap include-merged)))
    (setq gp-helm--reviewing-cache 'loading
          gp-helm--reviewing-uuid uuid)
    (let ((reviewing-source
           (helm-build-sync-source "Needs my review"
             :candidates #'gp-helm--reviewing-candidates
             ;; volatile so each helm-update re-runs :candidates and the
             ;; async results replace the loading row as they arrive
             :volatile t
             :action actions :nomark t :keymap km
             :header-name
             (lambda (name)
               (concat name
                       (if (listp gp-helm--reviewing-cache)
                           (format " (%d)"
                                   (length (car (gp-helm--partition-reviewing
                                                 gp-helm--reviewing-cache
                                                 gp-helm--reviewing-uuid))))
                         " (…)")
                       "   (C-c g reload · C-c G refresh · C-c m merged)"))))
          ;; PRs where the user's vote is already in: still worth seeing
          ;; (a follow-up push may need another look), but they are not
          ;; pending work, so they sit in their own section rather than
          ;; padding "Needs my review".
          (voted-source
           (helm-build-sync-source "Approved by me"
             :candidates #'gp-helm--voted-candidates
             :volatile t
             :action actions :nomark t :keymap km
             :header-name
             (lambda (name)
               (concat name
                       (if (listp gp-helm--reviewing-cache)
                           (format " (%d)"
                                   (length (nth 1 (gp-helm--partition-reviewing
                                                   gp-helm--reviewing-cache
                                                   gp-helm--reviewing-uuid))))
                         " (…)")))))
          ;; Enough other reviewers have approved (or asked for changes)
          ;; that the user's vote would not change the outcome.  Still
          ;; listed -- they may want to weigh in regardless -- but not
          ;; counted as pending work.
          (covered-source
           (helm-build-sync-source "Covered by others"
             :candidates #'gp-helm--covered-candidates
             :volatile t
             :action actions :nomark t :keymap km
             :header-name
             (lambda (name)
               (concat name
                       (if (listp gp-helm--reviewing-cache)
                           (format " (%d)"
                                   (length (nth 2 (gp-helm--partition-reviewing
                                                   gp-helm--reviewing-cache
                                                   gp-helm--reviewing-uuid))))
                         " (…)")))))
          (sources nil))
      (setq sources
            (cons reviewing-source
                  (delq nil
                        (list
                         (gp-helm--source "My pull requests"
                                                 (plist-get cat :mine) actions nil km)
                         voted-source
                         covered-source
                         (gp-helm--source "My drafts"
                                                 (plist-get cat :drafts) actions t km)))))
      ;; reviewing PRs have no fast endpoint: serve from cache, else scan
      ;; repos in parallel (non-blocking) and fill the section as batches land
      (let ((hit (gp-cache-get (list 'reviewing uuid states))))
        (if (car hit)
            (progn (setq gp-helm--reviewing-cache (cdr hit))
                   ;; Served from cache, so `gp-helm--scan-reviewing-async'
                   ;; (and the tally/pipeline fetches hanging off its on-done)
                   ;; never runs -- without this the badges on every cached
                   ;; reviewing row stay blank until the cache expires.
                   (let ((cached (cdr hit)))
                     (run-with-idle-timer
                      0.1 nil
                      (lambda ()
                        (gp-helm--scan-pipelines-async cached)
                        (gp-helm--scan-review-tallies-async cached))))
                   (run-with-idle-timer 0.1 nil #'gp-helm--refresh-if-alive))
          (run-with-idle-timer
           0.1 nil
           (lambda () (gp-helm--scan-reviewing-async uuid states)))))
      ;; fetch pipeline statuses and review tallies for the immediately-known
      ;; PRs in the background
      (run-with-idle-timer
       0.1 nil
       (lambda ()
         (let ((known (append (plist-get cat :mine) (plist-get cat :drafts))))
           (gp-helm--scan-pipelines-async known)
           (gp-helm--scan-review-tallies-async known))))
      (helm :sources sources
            :truncate-lines t
            :buffer gp-helm-buffer
            :full-frame gp-helm-full-frame
            :preselect (when gp-helm--last-visited-pr-id
                         (format "#%s " gp-helm--last-visited-pr-id))))))

(defun gp-helm--scan-pipelines-async (prs)
  "Fetch each PR's latest-commit pipeline state in parallel, refreshing live.
Results land in `gp-helm--pipeline-cache' keyed by commit hash."
  (dolist (pr prs)
    (let ((hash (gp-pr-source-commit pr))
          (full-name (gp-pr-full-name pr)))
      (when (and hash full-name (eq (gethash hash gp-helm--pipeline-cache 'miss) 'miss))
        (gp-commit-build-states-async
         full-name hash
         (lambda (states)
           (puthash hash (gp-build-states-summary states)
                    gp-helm--pipeline-cache)
           (gp-helm--refresh-if-alive)))))))

(defun gp-helm--refresh-if-alive ()
  "Redraw the live helm session, if any."
  (when (and (bound-and-true-p helm-alive-p) (fboundp 'helm-update))
    (ignore-errors (helm-update))))

(defun gp-helm--my-vote (pr uuid)
  "Return UUID's own review state on PR, or nil.
Wrapped because not every backend implements `gp-pr-my-review-state'
\(the mock does not); a backend without it simply reports no vote,
which degrades to the previous behaviour rather than erroring."
  (and uuid
       (ignore-errors (gp-pr-my-review-state pr uuid))))

(defun gp-helm--voted-p (pr uuid)
  "Non-nil when UUID has already APPROVED PR.
Such a PR no longer needs the user's action, so it moves out of
`Needs my review' into the `Approved by me' section.

Deliberately approval-only: a PR you sent back with changes requested
is still yours to re-review once the author pushes, so it belongs in
the pending list, not filed away as done."
  (eq (gp-helm--my-vote pr uuid) 'approved))

(defcustom gp-helm-min-approvals 2
  "Approvals by OTHER reviewers after which a PR no longer needs yours.

A PR that already has this many approvals from people other than you
moves out of \"Needs my review\" into \"Covered by others\" -- still
listed, so you can weigh in anyway, but not counted as pending work.

Your own vote is excluded from the count deliberately: the question is
whether other people have already covered the review, not how many
approvals exist in total.  Set to 0 to disable this grouping."
  :type 'integer :group 'bitbucket)

(defcustom gp-helm-min-rejections 2
  "Changes-requested by OTHER reviewers after which a PR no longer needs yours.
The `gp-helm-min-approvals' rule for the other direction: once this
many other reviewers have asked for changes, the point is made and the
PR moves to \"Covered by others\".  Set to 0 to disable."
  :type 'integer :group 'bitbucket)

(defun gp-helm--others-votes (pr uuid)
  "Return (APPROVED . CHANGES) counted over reviewers other than UUID.
nil when the per-reviewer breakdown is not available, which callers
must treat as unknown, NOT as quorum-met -- the data arrives
asynchronously, and assuming quorum would make rows vanish from the
pending list and then reappear.

Uses the cached reviewer list rather than `gp-pr-review-tally', whose
aggregate counts cannot tell the user's own vote from anyone else's."
  (let ((reviewers (gethash (alist-get 'id pr) gp-helm--reviewers-cache)))
    (when (eq reviewers 'none) (setq reviewers nil))
    (when (gethash (alist-get 'id pr) gp-helm--reviewers-cache)
      (let ((a 0) (c 0))
        (dolist (r reviewers)
          (unless (and uuid (equal (plist-get r :id) uuid))
            (pcase (plist-get r :state)
              ('approved (setq a (1+ a)))
              ('changes (setq c (1+ c))))))
        (cons a c)))))

(defun gp-helm--covered-by-others-p (pr uuid)
  "Non-nil when other reviewers have already settled PR without UUID.
True once approvals reach `gp-helm-min-approvals' or changes-requested
reach `gp-helm-min-rejections', counting only other people's votes.
Unknown vote data (still loading) is never treated as covered."
  (let ((votes (gp-helm--others-votes pr uuid)))
    (and votes
         (or (and (> gp-helm-min-approvals 0)
                  (>= (car votes) gp-helm-min-approvals))
             (and (> gp-helm-min-rejections 0)
                  (>= (cdr votes) gp-helm-min-rejections))))))

(defun gp-helm--partition-reviewing (prs uuid)
  "Split PRS into (NEEDS-ACTION APPROVED COVERED) for UUID.

NEEDS-ACTION still wants the user's review; APPROVED are ones they
already approved; COVERED are ones other reviewers have settled (see
`gp-helm--covered-by-others-p').  The user's own vote wins over
quorum: a PR you approved belongs under \"Approved by me\" even if
others have since piled on."
  (let (todo done covered)
    (dolist (pr prs)
      (cond
       ((gp-helm--voted-p pr uuid) (push pr done))
       ((gp-helm--covered-by-others-p pr uuid) (push pr covered))
       (t (push pr todo))))
    (list (nreverse todo) (nreverse done) (nreverse covered))))

(defun gp-helm--scan-reviewing-async (uuid states)
  "Scan reviewer PRs for UUID/STATES in parallel, updating helm live."
  (let ((seen (make-hash-table :test 'eql))
        (acc '()))
    (setq gp-helm--reviewing-cache nil)
    (gp-reviewing-pull-requests-async
     uuid states
     ;; on-batch: merge new PRs (dedup by id), refresh the section
     (lambda (prs)
       (dolist (pr prs)
         (let ((id (alist-get 'id pr)))
           (unless (gethash id seen)
             (puthash id t seen)
             (push pr acc))))
       (setq gp-helm--reviewing-cache (reverse acc))
       (gp-helm--refresh-if-alive))
     ;; on-done: cache the final result for 5 min, fetch pipelines/reviews, refresh
     (lambda ()
       (let ((final (reverse acc)))
         (setq gp-helm--reviewing-cache final)
         (gp-cache-put (list 'reviewing uuid states) final)
         (gp-helm--scan-pipelines-async final)
         (gp-helm--scan-review-tallies-async final)
         ;; per-reviewer data too: the quorum split needs to know WHO
         ;; voted, which the aggregate tally cannot say
         (gp-helm--scan-reviewers-async final))
       (gp-helm--refresh-if-alive)))))

;;;; Others' open PRs ---------------------------------------------------------

(defvar gp-helm--others-cache nil
  "Async-filled list of others' open PRs for `gp-helm-open-prs'.")

(defun gp-helm--others-candidates ()
  "Helm `:candidates' reading the async others'-PRs cache."
  (if (listp gp-helm--others-cache)
      (if gp-helm--others-cache
          (gp-helm--pr-candidates gp-helm--others-cache)
        (list (cons (propertize "  ⏳ scanning repositories for open PRs…"
                                'face 'shadow) nil)))
    nil))

;;;###autoload
(defun gp-helm-open-prs ()
  "List open pull requests across the workspace that are NOT yours.
A broad parallel scan of recent repositories; fills in
asynchronously.  Excludes PRs you authored (use `gp-helm'
for those)."
  (interactive)
  (require 'helm)
  (let* ((uuid (gp-user-uuid))
         (hit (gp-cache-get (list 'others uuid))))
    (setq gp-helm--others-cache (if (car hit) (cdr hit) nil))
    (unless (car hit)
      (run-with-idle-timer
       0.1 nil
       (lambda ()
         (let ((seen (make-hash-table :test 'eql)) (acc '()))
           (gp-open-pull-requests-async
            '("OPEN")
            (lambda (prs)
              (dolist (pr prs)
                (let ((id (alist-get 'id pr)))
                  (unless (or (gethash id seen)
                              (gp-pr-authored-by-p pr uuid))
                    (puthash id t seen)
                    (push pr acc))))
              (setq gp-helm--others-cache (reverse acc))
              (gp-helm--refresh-if-alive))
            (lambda ()
              (let ((final (reverse acc)))
                (setq gp-helm--others-cache final)
                (gp-cache-put (list 'others uuid) final)
                (gp-helm--scan-review-tallies-async final))
              (gp-helm--refresh-if-alive)))))))
    (helm :sources
          (helm-build-sync-source "Open PRs (others)"
            :candidates #'gp-helm--others-candidates
            :volatile t
            :action (gp-helm--pr-actions)
            :nomark t
            :keymap (gp-helm--list-keymap nil)
            :header-name
            (lambda (name)
              (concat name
                      (if (listp gp-helm--others-cache)
                          (format " (%d)" (length gp-helm--others-cache))
                        " (…)"))))
          :truncate-lines t
          :full-frame gp-helm-full-frame
          :buffer (gp-helm--buffer "open"))))

;;;###autoload
(defun gp-helm-repo (full-name)
  "List the open pull requests in repo FULL-NAME (\"owner/slug\") with Helm.
One quick per-repo fetch (cached 5 min), shown in a side window
rather than full-frame -- handy from the per-repo mode-line count."
  (interactive
   (list (or (and (boundp 'gp-watch--repo) gp-watch--repo)
             (read-string "Repository (owner/slug): "))))
  (require 'helm)
  (let* ((prs (gp-cache-with-cache
               (list 'repo-prs full-name)
               (lambda () (gp-repo-pull-requests full-name))))
         (gp-helm-full-frame nil))     ;; side window, not whole frame
    (unless prs
      (user-error "No open pull requests in %s" full-name))
    (helm :sources
          (helm-build-sync-source (format "Open PRs · %s" full-name)
            :candidates (gp-helm--pr-candidates prs)
            :action (gp-helm--pr-actions)
            :nomark t
            :keymap (gp-helm--list-keymap nil))
           :truncate-lines t
           :buffer (gp-helm--buffer "repo"))))

;;;###autoload
(defun gp-helm-repo-branch (full-name branch)
  "List open pull requests in FULL-NAME whose source branch is BRANCH.
Uses the cached per-repo open-PR list and filters it client-side."
  (interactive
   (list (or (and (boundp 'gp-watch--repo) gp-watch--repo)
             (read-string "Repository (owner/slug): "))
         (read-string "Branch: ")))
  (require 'helm)
  (let* ((prs (gp-cache-with-cache
               (list 'repo-prs full-name)
               (lambda () (gp-repo-pull-requests full-name))))
         (matches (gp-helm--prs-for-branch prs branch))
         (gp-helm-full-frame nil))
    (unless matches
      (user-error "No open pull requests in %s on branch %s" full-name branch))
    (helm :sources
          (helm-build-sync-source (format "Open PRs · %s · %s" full-name branch)
            :candidates (gp-helm--pr-candidates matches)
            :action (gp-helm--pr-actions)
            :nomark t
            :keymap (gp-helm--list-keymap nil))
          :truncate-lines t
          :buffer (gp-helm--buffer "repo-branch"))))

(provide 'gp-helm)
;;; gp-helm.el ends here
