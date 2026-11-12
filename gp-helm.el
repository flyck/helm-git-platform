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
(require 'gp-pipeline)
(require 'gp-ui)

(declare-function helm "helm")
(declare-function helm-build-sync-source "helm-source")
(declare-function helm-run-after-exit "helm-core")
(declare-function helm-get-selection "helm-core")
(declare-function helm-update "helm-core")
(declare-function helm-set-pattern "helm-core")
(declare-function gp-compose "gp-compose")
(declare-function gp-create-pr "gp-create")
(declare-function gp-watch--repo-for-path "gp-watch")
(declare-function gp-watch--current-branch "gp-watch")
(declare-function gp-watch--pr-for "gp-watch")
(declare-function gp-local-find-checkout "gp-local")
(defvar helm-map)
(defvar helm-alive-p)
(defvar helm-pattern)

(defvar gp-helm--last-visited-pr-id nil
  "Id of the last PR opened via `gp-show-pr', so the list can preselect it.
Set from `gp-ui.el', which has no Helm dependency.")

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

;;;; Helm buffer names --------------------------------------------------------

;; All tagged via `gp--buffer-name' so every buffer this package opens shares
;; one searchable prefix.

(defconst gp-helm-buffer (gp--buffer-name "helm")
  "Name of the main Helm PR-list buffer.")

(defun gp-helm--buffer (suffix)
  "Return the tagged Helm buffer name for SUFFIX, e.g. \"files\"."
  (gp--buffer-name (format "helm %s" suffix)))

(defcustom gp-helm-kill-buffer-on-exit t
  "Non-nil kills a gp Helm buffer once its session has ended.
Helm only buries its buffer on exit, leaving a dead buffer whose keys
all fail with \"Running helm command outside of context\".  The cost
is that `helm-resume' cannot reopen an ended gp session."
  :type 'boolean :group 'bitbucket)

(defun gp-helm--own-buffer-p (name)
  "Non-nil if NAME is a Helm buffer created by `gp-helm--buffer'."
  (and (stringp name)
       (string-match-p
        (concat "\\`" (regexp-quote (concat "*" gp-buffer-name-prefix "helm"))
                "[ *]")
        name)))

(defun gp-helm--kill-dead-buffer ()
  "Kill the gp Helm buffer whose session is ending.
Runs from `helm-cleanup-hook'.  The kill is deferred until the
current command has finished, so the session's actions can still use
the buffer.  A buffer that a live (nested or resumed) session is
using again by then is left alone."
  (let ((name helm-buffer))
    (when (and gp-helm-kill-buffer-on-exit (gp-helm--own-buffer-p name))
      (run-at-time
       0 nil
       (lambda ()
         (let ((buf (get-buffer name)))
           (when (and buf
                      (not (and (bound-and-true-p helm-alive-p)
                                (equal helm-buffer name))))
             (kill-buffer buf))))))))

(add-hook 'helm-cleanup-hook #'gp-helm--kill-dead-buffer)

;;;; Pure candidate builders -------------------------------------------------

(defcustom gp-helm-title-width 52
  "Fallback column width for the PR title when the window width is unknown.
Normally the title column auto-grows to fill the available width
\(see `gp-helm--title-width')."
  :type 'integer :group 'bitbucket)

(defcustom gp-helm-title-min-width 24
  "Minimum width the auto-growing title column will shrink to."
  :type 'integer :group 'bitbucket)

(defcustom gp-helm-repo-width 38
  "Column width for the repository slug in the Helm list.
Sized for long prefixed slugs.  The auto-growing title column
\(`gp-helm--title-width') shrinks by the same amount, down to
`gp-helm-title-min-width'; lower this to give the title more room."
  :type 'integer :group 'bitbucket)

(defcustom gp-helm-labels-width 18
  "Column width for PR labels in the Helm list.
Labels are variable-length while this list is column-aligned, so they
get a fixed column truncated with an ellipsis like the title and repo
ones; the full names stay matchable through the invisible search tail
\(see `gp-helm--pr-search-tail').  The column is omitted on a platform
without labels; set to 0 to hide it everywhere."
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
  (let* ((window (get-buffer-window (get-buffer gp-helm-buffer)))
         (win (and (window-live-p window) (window-body-width window))))
    (if (not win)
        gp-helm-title-width
      ;; bubble 3 + avatar 3 + id 6 + labels + repo + author 16 + separators 8
      (max gp-helm-title-min-width
           (- win 3 3 6 (gp-helm--labels-column-width) gp-helm-repo-width 16 8
              gp-helm-title-reserve)))))

(defun gp-helm--labels-column-width ()
  "Return the columns the label column occupies, 0 when it is not shown.
It is not shown on a backend without labels."
  (if (and (> gp-helm-labels-width 0) (gp-labels-supported-p))
      gp-helm-labels-width
    0))

(defun gp-helm--labels-cell (pr)
  "Return PR's labels as one fixed-width, colour-faced cell.
Empty string when the column is not shown.  Each label keeps its own
face, which is why this does not use `gp-helm--pad' (one face for all)."
  (let ((width (gp-helm--labels-column-width)))
    (if (zerop width)
        ""
      (let* ((labels (gp-pr-labels pr))
             (cell (if labels
                       (gp--format-labels labels)
                     (propertize "···" 'face 'shadow)))
             (cell (truncate-string-to-width cell width nil nil "…")))
        (concat cell
                (make-string (max 0 (- width (string-width cell))) ?\s))))))

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
Off by default: fetching avatars over HTTP can slow the first paint.
The avatar cache is shared with the inline-comment overlays."
  :type 'boolean :group 'bitbucket)

(defun gp-helm--avatar (pr)
  "Return a leading avatar image string for PR's author, or \"\" (text only)."
  (let ((img (and gp-helm-show-avatars
                  (gp-overlay--avatar-image (gp-pr-author-avatar pr)))))
    (if img (concat (propertize " " 'display img) " ") "")))

(defcustom gp-helm-pipeline-settling-ttl 60
  "Seconds to cache a build state that has not settled yet.
Terminal states -- failed, successful, stopped -- are cached
indefinitely; running or no-result states expire after this.  0
re-fetches non-terminal states on every scan."
  :type 'integer :group 'bitbucket)

(defvar gp-helm--pipeline-cache (make-hash-table :test 'equal)
  "commit-hash -> (EXPIRY . STATE) for the pipeline bubble.
STATE is a symbol from `gp-build-states-summary'.  EXPIRY is nil for
a terminal state (cached forever) or a `float-time' deadline for one
still settling -- see `gp-helm-pipeline-settling-ttl'.")

(defun gp-helm--pipeline-terminal-p (state)
  "Return non-nil when STATE is a final verdict for a commit.
nil (\"no build reported\") is not final: CI may not have registered yet."
  (memq state '(failed successful stopped)))

(defun gp-helm--pipeline-cache-put (hash state)
  "Cache STATE for commit HASH, expiring it unless it is terminal."
  (puthash hash
           (cons (unless (gp-helm--pipeline-terminal-p state)
                   (+ (float-time) gp-helm-pipeline-settling-ttl))
                 state)
           gp-helm--pipeline-cache)
  state)

(defun gp-helm--pipeline-cache-get (hash)
  "Return (FOUND . STATE) for commit HASH, treating an expired entry as a miss."
  (let ((entry (gethash hash gp-helm--pipeline-cache)))
    (cond ((null entry) (cons nil nil))
          ((or (null (car entry)) (< (float-time) (car entry)))
           (cons t (cdr entry)))
          (t (cons nil nil)))))

(defun gp-helm--pipeline-bubble (pr)
  "Return a colored pipeline-status bubble for PR's latest commit.
Reads the async `gp-helm--pipeline-cache'; a neutral bubble
shows until the status arrives.  Must key on `gp-pr-source-commit',
like `gp-helm--scan-pipelines-async', since the raw path differs
per backend."
  (let* ((hash (ignore-errors (gp-pr-source-commit pr)))
         (hit (and hash (gp-helm--pipeline-cache-get hash)))
         (state (if (car hit) (cdr hit) 'unknown)))
    (pcase state
      ('failed     (propertize "🔴" 'help-echo "Pipeline failed"))
      ('running    (propertize "🔵" 'help-echo "Pipeline running"))
      ('stopped    (propertize "⚪" 'help-echo "Pipeline cancelled"))
      ('successful (propertize "🟢" 'help-echo "Pipeline succeeded"))
      ('nil        "  ")              ;; resolved: no pipeline ran
      (_           (propertize "⚫" 'help-echo "Pipeline status loading…")))))

(defvar gp-helm--deploy-cache (make-hash-table :test 'equal)
  "(FULL-NAME . commit-hash) -> t/nil: whether a deploy step succeeded.
Covers that commit's current pipeline(s); absent means not yet scanned
or the last fetch failed.
Keyed by repo too so a deploy can be busted per-repo.

Resolved entries never expire (walking every pipeline's steps is too
expensive per refresh), so a still-running pipeline stays cached nil
until busted: per commit by `gp-helm--deploy-cache-bust' (from
`gp-detail-refresh'), or per repo by `gp-helm--deploy-cache-bust-repo'
\(from `gp-pipeline-deploy-script', since a deploy to a shared
environment can supersede an earlier PR's deploy).")

(defvar gp-helm--live-deploy-cache (make-hash-table :test 'equal)
  "(FULL-NAME . merge-commit-hash) -> t/nil: whether a merged PR went live.
True when the destination branch's pipeline for the MERGE commit (what
production runs against) has a successful step matching both
`gp-helm-deploy-keywords' and `gp-helm-live-keywords'.  Absent means
not yet scanned.  Cached and busted like `gp-helm--deploy-cache'.")

(defun gp-helm--deploy-cache-key (pr)
  "Return PR's `gp-helm--deploy-cache' key, or nil if its commit is unknown."
  (let ((hash (gp-pr-source-commit pr)))
    (and hash (cons (gp-pr-full-name pr) hash))))

(defun gp-helm--deploy-cache-bust (pr)
  "Forget any cached deploy verdict for PR's current commit, if any."
  (let ((key (gp-helm--deploy-cache-key pr)))
    (when key (remhash key gp-helm--deploy-cache))))

(defun gp-helm--cache-bust-repo-in (table full-name)
  "Remove every entry in TABLE whose (FULL-NAME . commit) key is FULL-NAME's."
  (let (stale)
    (maphash (lambda (k _v) (when (equal (car k) full-name) (push k stale))) table)
    (dolist (k stale) (remhash k table))))

(defun gp-helm--deploy-cache-bust-repo (full-name)
  "Forget every cached deploy verdict for FULL-NAME's repo.
Clears both `gp-helm--deploy-cache' and `gp-helm--live-deploy-cache'."
  (when full-name
    (gp-helm--cache-bust-repo-in gp-helm--deploy-cache full-name)
    (gp-helm--cache-bust-repo-in gp-helm--live-deploy-cache full-name)))

(defcustom gp-helm-deploy-keywords '("deploy")
  "Words that mark a pipeline step as a deploy, for the 🚀/🚢 badges.
Matched case-insensitively as a substring of the step name; a step
matches if ANY word in the list is found (see
`gp-helm--step-name-matches-p').  `gp-helm-live-keywords' narrows this
further, for the merged-PR \"deployed live\" badge specifically."
  :type '(repeat string) :group 'bitbucket)

(defcustom gp-helm-live-keywords '("live" "prod")
  "Words that mark a deploy step as targeting production, for the 🚢 badge.
Matched case-insensitively as a substring of the step name, same as
`gp-helm-deploy-keywords'; a merged PR's step must match a word from
BOTH lists to count as \"deployed live\" -- e.g. \"Deploy to LIVE\"
matches, \"Deploy to STAGING\" does not."
  :type '(repeat string) :group 'bitbucket)

(defun gp-helm--step-name-matches-p (name words)
  "Return non-nil if NAME contains any of WORDS, case-insensitively."
  (let ((name (downcase (or name ""))))
    (cl-some (lambda (w) (string-match-p (regexp-quote (downcase w)) name)) words)))

(defun gp-helm--pipeline-has-successful-deploy-p (steps)
  "Return non-nil if STEPS has a successful `gp-helm-deploy-keywords' step."
  (cl-some (lambda (step)
             (and (gp-helm--step-name-matches-p (alist-get 'name step) gp-helm-deploy-keywords)
                  (equal (gp-pipeline-step-result step) "SUCCESSFUL")))
           steps))

(defun gp-helm--pipeline-has-successful-live-deploy-p (steps)
  "Return non-nil if STEPS has a successful live deploy step.
The step name must match BOTH `gp-helm-deploy-keywords' and
`gp-helm-live-keywords'."
  (cl-some (lambda (step)
             (let ((name (alist-get 'name step)))
               (and (gp-helm--step-name-matches-p name gp-helm-deploy-keywords)
                    (gp-helm--step-name-matches-p name gp-helm-live-keywords)
                    (equal (gp-pipeline-step-result step) "SUCCESSFUL"))))
           steps))

(defun gp-helm--scan-deploys-async (prs)
  "Fetch each PR's current-commit pipeline steps, caching a deploy flag.
Like `gp-helm--scan-pipelines-async', but needs step names, so it uses
the heavier `gp-pipeline-fetch-for-pr-async'.  Only fetches on a cache
miss (see `gp-helm--deploy-cache')."
  (dolist (pr prs)
    (let ((key (gp-helm--deploy-cache-key pr)))
      (when (and key (eq (gethash key gp-helm--deploy-cache 'miss) 'miss))
        (gp-pipeline-fetch-for-pr-async
         pr
         (lambda (data)
           ;; nil means the fetch failed (or no runs yet): stay unscanned
           ;; so the next scan retries rather than pinning "not deployed".
           (when data
             (let ((deployed (cl-some (lambda (pp) (gp-helm--pipeline-has-successful-deploy-p (cdr pp)))
                                       (plist-get data :current))))
               (puthash key (and deployed t) gp-helm--deploy-cache)
               (gp-helm--refresh-if-alive)))))))))

(defun gp-helm--deploy-badge (pr)
  "Return a 🚀 badge if PR's current pipeline deployed successfully, else \"\".
Reads the async `gp-helm--deploy-cache'; blank until the scan resolves."
  (let* ((key (gp-helm--deploy-cache-key pr))
         (deployed (and key (gethash key gp-helm--deploy-cache))))
    (if deployed (propertize " 🚀" 'help-echo "A deploy step succeeded") "")))

(defun gp-helm--live-deploy-cache-key (pr)
  "Return PR's `gp-helm--live-deploy-cache' key, or nil if unresolvable.
Only merged PRs have a merge commit to key on."
  (let ((hash (and (gp-pr-merged-p pr) (gp-pr-merge-commit pr))))
    (and hash (cons (gp-pr-full-name pr) hash))))

(defun gp-helm--scan-live-deploys-async (prs)
  "Cache whether each merged PR in PRS was deployed live at its merge commit.
Fetches the destination branch's pipeline steps; non-merged PRs are
skipped."
  (dolist (pr prs)
    (let ((key (gp-helm--live-deploy-cache-key pr)))
      (when (and key (eq (gethash key gp-helm--live-deploy-cache 'miss) 'miss))
        (gp-pipeline-fetch-for-branch-async
         (gp-pr-full-name pr) (gp-pr-destination-branch pr) (gp-pr-merge-commit pr)
         (lambda (data)
           ;; As in `gp-helm--scan-deploys-async': nil retries next scan.
           (when data
             (let ((deployed (cl-some (lambda (pp) (gp-helm--pipeline-has-successful-live-deploy-p (cdr pp)))
                                       (plist-get data :current))))
               (puthash key (and deployed t) gp-helm--live-deploy-cache)
               (gp-helm--refresh-if-alive)))))))))

(defun gp-helm--live-deploy-badge (pr)
  "Return a 🚢 badge if merged PR was deployed live, else \"\".
Reads the async `gp-helm--live-deploy-cache'; blank until the scan
resolves."
  (let* ((key (gp-helm--live-deploy-cache-key pr))
         (deployed (and key (gethash key gp-helm--live-deploy-cache))))
    (if deployed (propertize " 🚢" 'help-echo "Deployed live") "")))

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
           (badge (gp-helm--comment-badge pr))
           (reviews (gp-helm--review-badge pr))
           (deploy (concat (gp-helm--deploy-badge pr) (gp-helm--live-deploy-badge pr)))
           ;; "" when unsupported, so it carries its own separator.
           (labels (let ((cell (gp-helm--labels-cell pr)))
                     (if (string-empty-p cell) "" (concat cell "  "))))
           (line (format "%s %s%s  %s  %s%s  %s%s%s%s"
                         bubble avatar id title labels repo author reviews badge deploy)))
      ;; Drafts are dimmed as a whole row on purpose, label colours included.
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
Populated asynchronously because both backends do a per-PR fetch (see
`gp-pr-review-tally-async'): a cached PR-list entry's votes can be
stale, since other reviewers' votes do not invalidate it.")

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
Always refetches, even when the id is cached: approvals can change
\(someone votes in the web UI) well before `gp-cache-ttl' forces a
relist.  Runs on each list open/refresh, so no dedicated poll is needed."
  (dolist (pr prs)
    (let ((id (alist-get 'id pr)))
      (gp-pr-review-tally-async
       pr
       (lambda (tally)
         (puthash id tally gp-helm--review-tally-cache)
         (gp-helm--refresh-if-alive))))))

(defvar gp-helm--reviewers-cache (make-hash-table :test 'eql)
  "PR id -> per-reviewer plists (see `gp-pr-reviewers-async').
Separate from `gp-helm--review-tally-cache' because the tally cannot say
WHO voted, which `gp-helm--covered-by-others-p' needs.  Holds `none'
for a fetched empty list.")

(defun gp-helm--scan-reviewers-async (prs)
  "Fetch each PR's per-reviewer breakdown in parallel, caching by id.
Always refetches, like `gp-helm--scan-review-tallies-async'."
  (dolist (pr prs)
    (let ((id (alist-get 'id pr)))
      (gp-pr-reviewers-async
       pr
       (lambda (reviewers)
         ;; nil would read as "not fetched yet".
         (puthash id (or reviewers 'none) gp-helm--reviewers-cache)
         (gp-helm--refresh-if-alive))))))

(defvar gp-helm--comment-resolution-cache (make-hash-table :test 'eql)
  "PR id -> (UNRESOLVED . RESOLVED) counts of resolvable comments.
Resolution status only exists on the full comment thread, so this
needs a per-PR fetch.  Non-resolvable comments (see
`gp-comment-resolvable-p') count toward neither; a reply counts under
its thread root's resolution (`gp--comment-thread-resolved-p').")

(defun gp-helm--format-comment-resolution (counts)
  "Return the comment badge string for COUNTS, a (UNRESOLVED . RESOLVED) cons.
One 💬, then \"unresolved(total)\" (e.g. \"💬1(10)\"), in plain text:
Helm's selection overlay would paint over a face anyway."
  (let* ((u (car counts)) (r (cdr counts)) (total (+ u r)))
    (if (zerop total)
        ""
      (format " 💬%d(%d)" u total))))

(defun gp-helm--comment-badge (pr)
  "Return the comment-resolution badge string for PR.
Reads the async `gp-helm--comment-resolution-cache'; a blank badge
shows until it arrives (see `gp-helm--scan-comment-resolution-async')."
  (let* ((id (alist-get 'id pr))
         (cached (gethash id gp-helm--comment-resolution-cache)))
    (if (consp cached)
        (gp-helm--format-comment-resolution cached)
      "")))

(defun gp-helm--comment-resolution-counts (comments)
  "Return (UNRESOLVED . RESOLVED) over resolvable comments in COMMENTS."
  (let ((by-id (gp--comments-by-id comments))
        (unresolved 0) (resolved 0))
    (dolist (c comments)
      (when (gp-comment-resolvable-p c)
        (if (gp--comment-thread-resolved-p c by-id)
            (setq resolved (1+ resolved))
          (setq unresolved (1+ unresolved)))))
    (cons unresolved resolved)))

(defun gp-helm--scan-comment-resolution-async (prs)
  "Fetch each PR's comments in parallel and cache resolution counts by id.
Always refetches, like `gp-helm--scan-review-tallies-async', so a
resolution made elsewhere shows before `gp-cache-ttl' expires."
  (dolist (pr prs)
    (let ((id (alist-get 'id pr))
          (full-name (gp-pr-full-name pr)))
      (gp-pull-request-comments-async
       full-name id
       (lambda (ok comments)
         (when ok
           (puthash id (gp-helm--comment-resolution-counts comments)
                    gp-helm--comment-resolution-cache)
           (gp-helm--refresh-if-alive)))))))

(defun gp-helm--search-token (field value)
  "Return \"FIELD:VALUE\" with whitespace in VALUE folded to `-', or nil.
Nil when VALUE is empty, so callers can `delq' it away.  Spaces are
folded because Helm splits a pattern on whitespace into separate
patterns."
  (when (and value (not (string-empty-p (format "%s" value))))
    (format "%s:%s" field
            (replace-regexp-in-string "[ \t]+" "-" (format "%s" value)))))

(defun gp-helm--pr-search-tokens (pr)
  "Return PR's field-scoped search tokens as a list of strings.
Helm matches the display string, so each field is emitted again
prefixed with its name and `label:bug repo:portal' then matches
natively (Helm ANDs whitespace-separated patterns).  Matching stays
substring-loose: `label:bug' also hits `bugfix'."
  ;; Use the normalised `gp-pr-*' accessors: raw keys differ per backend.
  (delq nil
        (append
         (list (gp-helm--search-token "repo" (gp-pr-repo-slug pr))
               (gp-helm--search-token "author" (gp-pr-author-name pr))
               (gp-helm--search-token "branch" (gp-pr-source-branch pr)))
         (mapcar (lambda (l) (gp-helm--search-token "label" (plist-get l :name)))
                 (gp-pr-labels pr)))))

(defun gp-helm--pr-search-tail (pr)
  "Return an invisible, searchable suffix of PR's full untruncated fields.
Helm matches the display string, whose columns are truncated, so the
full fields are appended invisibly: bare first, then in `field:value'
form (`gp-helm--pr-search-tokens')."
  (let-alist pr
    (propertize
     (concat " " (mapconcat #'identity
                            (delq nil (append
                                       (list (format "#%s" .id)
                                             .destination.repository.slug
                                             .title
                                             .author.display_name)
                                       (mapcar (lambda (l) (plist-get l :name))
                                               (gp-pr-labels pr))
                                       (gp-helm--pr-search-tokens pr)))
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
The default action opens the comment in the browser; others reply,
jump to it in the checkout, or open the detail buffer."
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
  (or (gp-comment-web-url comment)
      (let ((pr-url (gp-pr-web-url pr))
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
    (pcase-dolist (`(,key . ,fn) `(("C-c r" . ,#'gp-helm--reply-comment)
                                   ("C-c f" . ,#'gp-helm--goto-comment)))
      (define-key map (kbd key)
                  (lambda () (interactive)
                    (let ((c (helm-get-selection)))
                      (helm-run-after-exit (lambda () (funcall fn pr c)))))))
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
thunk) is given, C-c g to reload the current drill-down (a bare `g'
would be swallowed by the search pattern)."
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
  ;; First entry is the default action.
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
         (lambda (pr) (browse-url (gp-pr-web-url pr))))))

;;;; Entry point -------------------------------------------------------------

(defvar gp-helm--mine-cache nil
  "Async-filled list of the user's own PRs for the current `gp-helm' run.
Symbol `loading' while the background fetch runs, so the section can
render a ⏳ row instead of blocking Helm from opening.")

(defvar gp-helm--mine-uuid nil
  "The uuid the current \"mine\" fetch belongs to.")

(defun gp-helm--source (label prs actions &optional draft keymap)
  "Build a Helm source named LABEL over PRS using ACTIONS.
The header shows the live count; DRAFT styles the rows as drafts;
KEYMAP overrides the source keymap.  Returns nil when PRS is empty
so empty sections are omitted."
  (when prs
    (helm-build-sync-source label
      ;; Volatile so the title width is recomputed once the window exists.
      :candidates (lambda () (gp-helm--pr-candidates prs draft))
      :volatile t
      :action actions
      :nomark t
      :keymap (or keymap (gp-helm--list-keymap nil))
      :header-name (lambda (name)
                     (concat (gp-helm--header name prs)
                             "   (C-c g reload · C-c G refresh · C-c m merged"
                             (if (gp-labels-supported-p) " · C-c l label" "")
                             ")")))))

(defcustom gp-helm-show-merged-recent t
  "When non-nil, `gp-helm' shows a section of recently merged PRs.
Nil removes it entirely -- and skips the extra request it needs, since
merged PRs are not otherwise fetched for the open-only list."
  :type 'boolean :group 'bitbucket)

(defcustom gp-helm-merged-recent-hours 24
  "How many hours back the \"Merged\" section reaches.
A rolling window, not a calendar-day cutoff.  The section is omitted
when nothing falls inside it."
  :type 'integer :group 'bitbucket)

(defun gp-helm--merged-recently (prs &optional hours)
  "Return the PRs from PRS merged within the last HOURS.
HOURS defaults to `gp-helm-merged-recent-hours'.  A PR whose merge time
cannot be read is left out."
  (let* ((hours (or hours gp-helm-merged-recent-hours))
         (cutoff (time-subtract (current-time) (seconds-to-time (* 3600 (max 1 hours))))))
    (cl-remove-if-not
     (lambda (pr)
       (and (ignore-errors (gp-pr-merged-p pr))
            (when-let* ((at (ignore-errors (gp-pr-merged-at pr)))
                        (parsed (ignore-errors (date-to-time at))))
              (time-less-p cutoff parsed))))
     prs)))

(defun gp-helm--merged-section-label ()
  "Return the heading for the recently-merged section."
  (let ((h gp-helm-merged-recent-hours))
    (cond ((= h 24) "Merged in the last 24 hours")
          ((and (> h 24) (zerop (% h 24)))
           (format "Merged in the last %d days" (/ h 24)))
          ((= h 1) "Merged in the last hour")
          (t (format "Merged in the last %d hours" h)))))

(defvar gp-helm--merged-recent-cache nil
  "Async-filled list of recently-merged PRs for the current `gp-helm' run.
Symbol `loading' while the background fetch runs; nil once loaded means
\"none in the window\".")

(defun gp-helm--merged-recent-candidates ()
  "Helm `:candidates' for the recently-merged section.
Shows a ⏳ row while the fetch is still in flight."
  (cond
   ((eq gp-helm--merged-recent-cache 'loading)
    (list (cons (propertize "  ⏳ fetching recently merged…" 'face 'shadow) nil)))
   ((listp gp-helm--merged-recent-cache)
    (gp-helm--pr-candidates gp-helm--merged-recent-cache t))))

(defun gp-helm--merged-recent-source (actions keymap)
  "Build the Helm source for recently-merged PRs.
Always created, even for an empty list, so it can carry the ⏳ row
while the fetch is in flight."
  (helm-build-sync-source (gp-helm--merged-section-label)
    :candidates #'gp-helm--merged-recent-candidates
    :volatile t
    :action actions
    :nomark t
    :keymap (or keymap (gp-helm--list-keymap nil))
    :header-name
    (lambda (name)
      (if (listp gp-helm--merged-recent-cache)
          (gp-helm--header name gp-helm--merged-recent-cache)
        (concat name " (…)")))))

(defun gp-helm--fetch-merged-recent-async (uuid)
  "Fetch UUID's recently-merged PRs asynchronously into `gp-helm' live.
Capped at 20 so it never becomes a full history fetch, and cached
separately from the open-only \"mine\" list."
  (gp-workspace-pull-requests-async
   (lambda (ok prs)
     (setq gp-helm--merged-recent-cache
           (if ok (gp-helm--merged-recently prs) nil))
     (when ok
       (gp-cache-put (list 'merged-recent uuid) prs)
       (gp-helm--scan-live-deploys-async gp-helm--merged-recent-cache))
     (gp-helm--refresh-if-alive))
   uuid "MERGED" 20))

(defun gp-helm--mine-source (label key draft actions keymap)
  "Build the Helm source named LABEL over the user's own PRs under KEY.
KEY is `:mine' or `:drafts'; DRAFT styles the rows as drafts.  Unlike
`gp-helm--source' this is always created, so it can carry the ⏳ row
while the fetch is in flight; Helm hides it once it loads empty."
  (helm-build-sync-source label
    :candidates (lambda () (gp-helm--mine-candidates key draft))
    :volatile t
    :action actions
    :nomark t
    :keymap (or keymap (gp-helm--list-keymap nil))
    :header-name
    (lambda (name)
      (concat (if (listp gp-helm--mine-cache)
                  (gp-helm--header name (plist-get (gp-helm--mine-categorized) key))
                (concat name " (…)"))
              "   (C-c g reload · C-c G refresh · C-c m merged)"))))

(defun gp-helm--scan-mine-badges (prs)
  "Fetch the pipeline, review, comment and deploy badges for PRS."
  (gp-helm--scan-pipelines-async prs)
  (gp-helm--scan-review-tallies-async prs)
  (gp-helm--scan-comment-resolution-async prs)
  (gp-helm--scan-deploys-async prs))

(defun gp-helm--fetch-mine-async (uuid include-merged)
  "Fetch UUID's own PRs asynchronously, filling the live helm session.
INCLUDE-MERGED widens the state filter.  Results are cached under the
key `gp-helm' checks on open, so a reopen is instant."
  (gp-workspace-pull-requests-async
   (lambda (ok prs)
     (if (not ok)
         (progn
           (gp-log-error "gp-helm: fetching your pull requests failed")
           ;; nil, not `loading', so the ⏳ row does not spin forever.
           (setq gp-helm--mine-cache nil)
           (gp-helm--refresh-if-alive))
       (gp-cache-put (list 'mine uuid include-merged) prs)
       (setq gp-helm--mine-cache prs)
       (gp-helm--refresh-if-alive)
       (let ((cat (gp-categorize-pull-requests prs uuid)))
         (gp-helm--scan-mine-badges
          (append (plist-get cat :mine) (plist-get cat :drafts))))))
   uuid
   (if include-merged nil "OPEN")))

(defun gp-helm--prs-for-branch (prs branch)
  "Return the PRs from PRS whose source branch is BRANCH."
  (cl-remove-if-not (lambda (pr)
                      (equal (gp-pr-source-branch pr) branch))
                    prs))

(defun gp-helm--append-to-pattern (token)
  "Append TOKEN to the live Helm pattern, narrowing the current session.
Appends so scoped tokens stack and a typed pattern survives; a token
already present is not added twice."
  (let* ((pattern (or helm-pattern ""))
         (parts (split-string pattern "[ \t]+" t)))
    (unless (member token parts)
      (helm-set-pattern
       (if (string-empty-p pattern) token (concat pattern " " token))))))

(defun gp-helm--filter-by-label ()
  "Narrow the list to the label of the PR at point.
Prompts when the PR carries several labels.  Inserts the scoped
`label:NAME' token (see `gp-helm--pr-search-tokens') into the visible
search pattern, so it composes with typing and is undone by editing."
  (interactive)
  (let* ((pr (helm-get-selection))
         (names (and (consp pr)
                     (delq nil (mapcar (lambda (l) (plist-get l :name))
                                       (gp-pr-labels pr))))))
    (cond
     ((null names)
      (message "gp: no labels on this pull request"))
     (t (gp-helm--append-to-pattern
         (gp-helm--search-token
          "label" (if (cdr names)
                      (completing-read "Filter by label: " names nil t)
                    (car names))))))))

(defun gp-helm-list-by-label (name)
  "Open the Helm PR list narrowed to label NAME.
Used by `gp-filter-by-label', whose buffers have no Helm dependency."
  (require 'helm)
  (gp-helm--list nil (concat (gp-helm--search-token "label" name) " ")))

(defun gp-helm--list-keymap (include-merged)
  "Keymap for the main PR list.
C-c g reloads, clearing the PR-list cache and the otherwise
never-expiring deploy caches.  C-c G clears every cache (see
`gp-reset-caches'), e.g. when a repo is missing from the 24h repo
list.  C-c m toggles whether MERGED/DECLINED PRs are shown (currently
INCLUDE-MERGED).  C-c l narrows to the label of the PR at point."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c l") #'gp-helm--filter-by-label)
    (define-key map (kbd "C-c g")
                (lambda () (interactive)
                  (helm-run-after-exit
                   (lambda ()
                     (gp-cache-clear)
                     (clrhash gp-helm--deploy-cache)
                     (clrhash gp-helm--live-deploy-cache)
                     (gp-helm--list include-merged)))))
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
Lets the candidate functions tell the user's own vote apart.")

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

(defun gp-helm--mine-categorized ()
  "Return the categorisation plist of the user's own PRs, or nil while loading."
  (when (listp gp-helm--mine-cache)
    (gp-categorize-pull-requests gp-helm--mine-cache gp-helm--mine-uuid)))

(defun gp-helm--mine-candidates (key draft)
  "Helm `:candidates' for KEY (`:mine' or `:drafts') of the user's own PRs.
DRAFT styles the rows as drafts.  Shows a ⏳ row while the fetch is
still in flight, so helm opens instantly instead of waiting on it."
  (if (eq gp-helm--mine-cache 'loading)
      ;; Each loading section needs its OWN row, or Helm drops the empty
      ;; section and it blinks out mid-load.
      (list (cons (propertize (if draft
                                  "  ⏳ fetching your drafts…"
                                "  ⏳ fetching your pull requests…")
                              'face 'shadow)
                  nil))
    (gp-helm--pr-candidates (plist-get (gp-helm--mine-categorized) key) draft)))

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
  "Non-nil makes `gp-helm' offer PR creation from a magit branch without a PR.
That is, it opens the PR-creation mask instead of the workspace list.
Set to nil to always get the PR list."
  :type 'boolean :group 'bitbucket)

(defun gp-helm--magit-create-context ()
  "Return (DIR FULL-NAME BRANCH) to offer PR creation, or nil.
Non-nil only in a magit buffer whose repo+branch resolve, whose
branch is not the repo's default branch, AND that has no open PR.
The no-PR check must use `gp-helm--magit-branch-prs', the same source
as the open-PR path, so the two decisions can never disagree."
  (when gp-helm-create-from-magit
    (pcase-let ((`(,dir ,full-name ,branch) (gp-helm--magit-context)))
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
background scan finishes (see `gp-reviewing-pull-requests-async').

Sections are colour-coded with aligned id / title / repo / author
columns.  RET (or click) opens the PR detail buffer; more actions
are on the action menu (\\<helm-map>\\[helm-select-action]).

By default only OPEN PRs are shown; with a prefix argument, or
\\`C-c m' in the list, INCLUDE-MERGED also shows MERGED/DECLINED."
  (interactive "P")
  (require 'helm)
  (let (branch-prs ctx)
    (cond
     ;; Checked BEFORE create so an existing PR always wins.
     ((and (not include-merged)
           (setq branch-prs (gp-helm--magit-branch-prs))
           (= (length branch-prs) 1))
      (require 'gp-ui)
      (gp-show-pr (car branch-prs)))
     ((and (not include-merged)
           (null branch-prs)
           (setq ctx (gp-helm--magit-create-context)))
      (require 'gp-create)
      (gp-create-pr (nth 0 ctx) (nth 1 ctx) (nth 2 ctx)))
     (t (gp-helm--magit-report-fallthrough include-merged)
        (gp-helm--list include-merged)))))

(defun gp-helm--magit-report-fallthrough (include-merged)
  "Say why `gp-helm' fell back to the full list from a magit buffer.
Only speaks up in a magit buffer where the branch-PR shortcut was
supposed to apply, so a repo or branch that fails to resolve is not
mistaken for a branch without a PR."
  (when-let* (((not include-merged))
              (ctx (gp-helm--magit-context)))
    (pcase-let ((`(,dir ,full-name ,branch) ctx))
      (cond
       ((not full-name)
        (message "gp-helm: no forge repo for %s (active backend: %s) -- showing the full list"
                 (abbreviate-file-name dir)
                 (ignore-errors (eieio-object-class-name (git-platform-backend)))))
       ((not branch)
        (message "gp-helm: could not read the branch in %s -- showing the full list"
                 (abbreviate-file-name dir)))
       (t
        (let ((n (length (gp-helm--prs-for-branch
                          (ignore-errors (gp-repo-pull-requests full-name))
                          branch))))
          (when (> n 1)
            (message "gp-helm: %d open PRs on %s -- showing the full list"
                     n branch))))))))

(defun gp-helm--magit-context ()
  "Return (DIR FULL-NAME BRANCH) for the current magit buffer, or nil.
Nil outside a magit buffer; FULL-NAME and BRANCH are nil when they
cannot be resolved."
  (when (and (derived-mode-p 'magit-mode) default-directory)
    (require 'gp-watch)
    (let* ((dir (expand-file-name default-directory))
           (full-name (gp-watch--repo-for-path dir))
           (branch (and full-name (gp-watch--current-branch dir))))
      (list dir full-name branch))))

(defun gp-helm--magit-branch-prs ()
  "Return the open PRs for the current magit buffer's repo+branch, or nil.
Nil outside a magit buffer, or when the repo/branch cannot be
resolved.  Used so `gp-helm' can jump straight to a lone branch PR."
  (pcase-let ((`(,_dir ,full-name ,branch) (gp-helm--magit-context)))
    (when (and full-name branch (fboundp 'gp-repo-pull-requests))
      (gp-helm--prs-for-branch (gp-repo-pull-requests full-name) branch))))

(defun gp-helm--reviewing-source (label candidates index actions keymap
                                         &optional hint)
  "Build a Helm source LABEL over one partition of the reviewing PRs.
CANDIDATES is the `:candidates' function; INDEX selects the partition
of `gp-helm--partition-reviewing' counted in the header, followed by
HINT.  ACTIONS and KEYMAP are the source's.  Volatile, so async
results replace the loading row as they land."
  (helm-build-sync-source label
    :candidates candidates
    :volatile t
    :action actions :nomark t :keymap keymap
    :header-name
    (lambda (name)
      (concat name
              (if (listp gp-helm--reviewing-cache)
                  (format " (%d)"
                          (length (nth index (gp-helm--partition-reviewing
                                              gp-helm--reviewing-cache
                                              gp-helm--reviewing-uuid))))
                " (…)")
              hint))))

(defun gp-helm--list (include-merged &optional input)
  "Show the Helm workspace PR list.  See `gp-helm' for INCLUDE-MERGED.
INPUT pre-fills the search pattern, e.g. the `label:NAME' token
`gp-filter-by-label' passes; editing it widens the list again."
  (let* ((uuid (gp-user-uuid))
         (states (if include-merged '("OPEN" "MERGED" "DECLINED") '("OPEN")))
         (mine-hit (gp-cache-get (list 'mine uuid include-merged)))
         ;; Only on the open-only list: with include-merged, "mine"
         ;; already shows the user's merged PRs.
         (merged-hit (unless include-merged (gp-cache-get (list 'merged-recent uuid))))
         (actions (gp-helm--pr-actions))
         (km (gp-helm--list-keymap include-merged)))
    ;; Never fetch "mine" synchronously here: it would freeze Emacs before
    ;; Helm has a window.  Serve from cache or fill in asynchronously.
    (setq gp-helm--mine-uuid uuid
          gp-helm--mine-cache (if (car mine-hit) (cdr mine-hit) 'loading))
    (setq gp-helm--reviewing-cache 'loading
          gp-helm--reviewing-uuid uuid)
    (if (car mine-hit)
        ;; Cached: still fetch the badges the async path would have.
        (let ((cached (cdr mine-hit)))
          (run-with-idle-timer
           0.1 nil
           (lambda ()
             (let ((cat (gp-categorize-pull-requests cached uuid)))
               (gp-helm--scan-mine-badges
                (append (plist-get cat :mine) (plist-get cat :drafts)))))))
      (gp-helm--fetch-mine-async uuid include-merged))
    (setq gp-helm--merged-recent-cache
          (cond
           ((or (not gp-helm-show-merged-recent) include-merged) nil)
           ((car merged-hit) (gp-helm--merged-recently (cdr merged-hit)))
           (t 'loading)))
    (if (and gp-helm-show-merged-recent (not include-merged) (not (car merged-hit)))
        (gp-helm--fetch-merged-recent-async uuid)
      ;; Served from cache, so the fetch's live-deploy scan never runs;
      ;; without this the 🚢 badges stay blank until the cache expires.
      (when (listp gp-helm--merged-recent-cache)
        (run-with-idle-timer
         0.1 nil
         (lambda () (gp-helm--scan-live-deploys-async gp-helm--merged-recent-cache)))))
    (let ((sources
           (delq nil
                 (list
                  (gp-helm--reviewing-source
                   "Needs my review" #'gp-helm--reviewing-candidates 0
                   actions km
                   "   (C-c g reload · C-c G refresh · C-c m merged)")
                  (gp-helm--mine-source "My pull requests" :mine nil actions km)
                  ;; Already voted: not pending work, but a follow-up push
                  ;; may need another look.
                  (gp-helm--reviewing-source
                   "Approved by me" #'gp-helm--voted-candidates 1 actions km)
                  ;; Other reviewers' votes already settle the outcome.
                  (gp-helm--reviewing-source
                   "Covered by others" #'gp-helm--covered-candidates 2
                   actions km)
                  (gp-helm--mine-source "My drafts" :drafts t actions km)
                  ;; Omitted when include-merged already covers it via "mine".
                  (when (and gp-helm-show-merged-recent (not include-merged))
                    (gp-helm--merged-recent-source actions km))))))
      ;; Reviewing PRs have no fast endpoint: serve from cache, else scan
      ;; repos in parallel and fill the section as batches land.
      (let ((hit (gp-cache-get (list 'reviewing uuid states))))
        (if (car hit)
            (progn (setq gp-helm--reviewing-cache (cdr hit))
                   ;; Served from cache, so the scan's badge fetches never
                   ;; run; without this the badges stay blank until expiry.
                   (run-with-idle-timer 0.1 nil #'gp-helm--scan-reviewing-badges
                                        (cdr hit))
                   (run-with-idle-timer 0.1 nil #'gp-helm--refresh-if-alive))
          (run-with-idle-timer
           0.1 nil
           (lambda () (gp-helm--scan-reviewing-async uuid states)))))
      (helm :sources sources
            :truncate-lines t
            :buffer gp-helm-buffer
            :full-frame gp-helm-full-frame
            :input (or input "")
            ;; Preselecting on top of a pre-filled pattern would fight it.
            :preselect (unless input
                         (when gp-helm--last-visited-pr-id
                           (format "#%s " gp-helm--last-visited-pr-id)))))))

(defun gp-helm--scan-pipelines-async (prs)
  "Fetch each PR's latest-commit pipeline state in parallel, refreshing live.
Results land in `gp-helm--pipeline-cache' keyed by commit hash."
  (dolist (pr prs)
    (let ((hash (gp-pr-source-commit pr))
          (full-name (gp-pr-full-name pr)))
      (when (and hash full-name
                 (not (car (gp-helm--pipeline-cache-get hash))))
        (gp-commit-build-states-async
         full-name hash
         (lambda (states)
           (gp-helm--pipeline-cache-put hash (gp-build-states-summary states))
           (gp-helm--refresh-if-alive)))))))

(defun gp-helm--refresh-if-alive ()
  "Redraw the live helm session, if any."
  (when (and (bound-and-true-p helm-alive-p) (fboundp 'helm-update))
    (ignore-errors (helm-update))))

(defun gp-helm--my-vote (pr uuid)
  "Return UUID's own review state on PR, or nil.
A backend without `gp-pr-my-review-state' (e.g. the mock) reports no
vote rather than erroring."
  (and uuid
       (ignore-errors (gp-pr-my-review-state pr uuid))))

(defun gp-helm--voted-p (pr uuid)
  "Non-nil when UUID has already APPROVED PR.
Such a PR moves from `Needs my review' to `Approved by me'.
Approval-only on purpose: a PR you sent back with changes requested
is still yours to re-review once the author pushes."
  (eq (gp-helm--my-vote pr uuid) 'approved))

(defcustom gp-helm-min-approvals 2
  "Approvals by OTHER reviewers after which a PR no longer needs yours.
Such a PR moves from \"Needs my review\" to \"Covered by others\".
Your own vote is not counted.  Set to 0 to disable this grouping."
  :type 'integer :group 'bitbucket)

(defcustom gp-helm-min-rejections 2
  "Changes-requested by OTHER reviewers after which a PR no longer needs yours.
The `gp-helm-min-approvals' rule for the other direction: once this
many other reviewers have asked for changes, the point is made and the
PR moves to \"Covered by others\".  Set to 0 to disable."
  :type 'integer :group 'bitbucket)

(defun gp-helm--others-votes (pr uuid)
  "Return (APPROVED . CHANGES) counted over reviewers other than UUID.
nil when the per-reviewer breakdown has not arrived yet, which callers
must treat as unknown, NOT as quorum-met, or rows would vanish from
the pending list and then reappear."
  (let ((reviewers (gethash (alist-get 'id pr) gp-helm--reviewers-cache)))
    (when reviewers
      (let ((a 0) (c 0))
        (dolist (r (unless (eq reviewers 'none) reviewers))
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
     ;; on-batch: merge new PRs, deduplicated by id.
     (lambda (prs)
       (dolist (pr prs)
         (let ((id (alist-get 'id pr)))
           (unless (gethash id seen)
             (puthash id t seen)
             (push pr acc))))
       (setq gp-helm--reviewing-cache (reverse acc))
       (gp-helm--refresh-if-alive))
     (lambda ()
       (let ((final (reverse acc)))
         (setq gp-helm--reviewing-cache final)
         (gp-cache-put (list 'reviewing uuid states) final)
         (gp-helm--scan-reviewing-badges final))
       (gp-helm--refresh-if-alive)))))

(defun gp-helm--scan-reviewing-badges (prs)
  "Fetch the badges and per-reviewer votes for reviewing PRS."
  (gp-helm--scan-pipelines-async prs)
  (gp-helm--scan-review-tallies-async prs)
  ;; The quorum split needs WHO voted, which the tally cannot say.
  (gp-helm--scan-reviewers-async prs)
  (gp-helm--scan-comment-resolution-async prs)
  (gp-helm--scan-deploys-async prs))

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
One quick per-repo fetch (cached `gp-cache-ttl'), shown in a side
window rather than full-frame -- handy from the mode-line count."
  (interactive
   (list (or (and (boundp 'gp-watch--repo) gp-watch--repo)
             (read-string "Repository (owner/slug): "))))
  (require 'helm)
  (let* ((prs (gp-cache-with-cache
               (list 'repo-prs full-name)
               (lambda () (gp-repo-pull-requests full-name))))
         ;; Side window: C-c g/G/m reopen `gp-helm--list' under this binding.
         (gp-helm-full-frame nil))
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
