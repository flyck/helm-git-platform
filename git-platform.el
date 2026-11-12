;;; git-platform.el --- Backend-agnostic PR protocol -*- lexical-binding: t; -*-

;;; Commentary:

;; A small abstraction over a code-review platform (Bitbucket, GitHub) so
;; the UI/overlay/helm layers talk to one protocol rather than to a
;; platform directly.
;;
;; Consumers call the backend-free `gp-' functions (e.g. `gp-pull-request-
;; comments').  Each is a thin wrapper that injects the active backend and
;; dispatches to a `cl-defgeneric' method (`gp--...') implemented per
;; platform -- so callers never pass a backend around.  The active backend
;; is configured once (`git-platform-default-backend') and built lazily.
;;
;; Both network operations and field accessors are generic, because each
;; platform's JSON shape differs.  Pure, shape-free helpers (categorize,
;; partition) live here and go through the accessors.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'gp-log)

(defclass git-platform () ()
  :abstract t
  :documentation "Abstract base for a code-review platform backend.")

(defcustom git-platform-default-backend 'bitbucket
  "Which backend `git-platform-backend' builds by default."
  :type '(choice (const :tag "Bitbucket Cloud" bitbucket)
                 (const :tag "GitHub" github))
  :group 'bitbucket)

(defvar git-platform-current-backend nil
  "The active `git-platform' backend instance, or nil (built lazily).")

(declare-function git-platform-bitbucket "git-platform-bitbucket")
(declare-function git-platform-github "git-platform-github")

(defun git-platform-backend ()
  "Return the active backend, constructing the default lazily.
Set `git-platform-current-backend' (or `git-platform-default-backend')
once to choose the platform; callers then use the backend-free
`gp-' functions and never pass a backend explicitly."
  (or git-platform-current-backend
      (setq git-platform-current-backend
            (pcase git-platform-default-backend
              ('bitbucket (require 'git-platform-bitbucket)
                          (git-platform-bitbucket))
              ('github (require 'git-platform-github)
                       (git-platform-github))
              (other (error "Unknown git-platform backend: %s" other))))))

;;;; Protocol definition helper ----------------------------------------------

(defmacro gp-defop (name arglist &optional doc)
  "Define protocol operation NAME with ARGLIST (excluding the backend).
Creates the generic `gp--NAME' (backend is its first argument) and
the public `gp-NAME' wrapper that supplies `(git-platform-backend)'.
ARGLIST may contain &optional; &rest is not supported here."
  (let* ((generic (intern (format "gp--%s" name)))
         (public  (intern (format "gp-%s" name)))
         (call-args (cl-remove '&optional arglist)))
    `(progn
       (cl-defgeneric ,generic (backend ,@arglist) ,(or doc ""))
       (defun ,public ,arglist
         ,(or doc "")
         (,generic (git-platform-backend) ,@call-args)))))

;;;; Network operations -------------------------------------------------------

(gp-defop user-uuid ()
  "Return the authenticated user's id.")
(gp-defop workspace-pull-requests (&optional uuid state max-items)
  "Return PRs authored by UUID.")
(gp-defop workspace-pull-requests-async (callback &optional uuid state max-items)
  "Async twin of `gp-workspace-pull-requests'; CALLBACK gets (OK PRS).
CALLBACK is always called exactly once; on failure OK and PRS are nil
and the caller should keep whatever it was already showing.")
(gp-defop reviewing-pull-requests (&optional uuid limit states)
  "Return PRs where UUID is a reviewer (synchronous).")
(gp-defop reviewing-pull-requests-async (uuid states on-batch on-done &optional limit)
  "Scan reviewer PRs for UUID, calling ON-BATCH/ON-DONE.")
(gp-defop open-pull-requests-async (states on-batch on-done &optional limit)
  "Scan all open PRs, calling ON-BATCH/ON-DONE.")
(gp-defop pull-request (full-name id)
  "Return the full PR object for FULL-NAME/ID.")
(gp-defop pull-request-async (full-name id callback)
  "Fetch PR FULL-NAME/ID asynchronously; CALLBACK gets (OK PR).")
(gp-defop pull-request-comments (full-name id &optional max-items)
  "Return comments for PR FULL-NAME/ID.")
(gp-defop pull-request-comments-async (full-name id callback &optional max-items)
  "Fetch comments for PR FULL-NAME/ID asynchronously; CALLBACK gets (OK COMMENTS).")
(gp-defop pull-request-diff (full-name id &optional commit)
  "Return the unified diff text for PR FULL-NAME/ID.
COMMIT, the source commit hash, lets the backend cache the diff.")
(gp-defop pull-request-stats (full-name id &optional pr)
  "Return a stats plist for PR FULL-NAME/ID.")
(gp-defop pull-request-diff-async (full-name id commit pr callback)
  "Fetch the unified diff for PR FULL-NAME/ID asynchronously.
CALLBACK gets the diff text, or nil on error.  COMMIT (the source
commit hash) lets the backend cache the result; PR supplies the
pre-signed diff link Bitbucket requires.")
(gp-defop pull-request-stats-async (full-name id pr callback)
  "Fetch the stats plist for PR FULL-NAME/ID asynchronously.
CALLBACK gets the plist, or nil on error.  PR is the already-fetched
PR object, which carries the diffstat link.")
(gp-defop pull-request-commits-async (full-name id callback &optional max-items)
  "Fetch the commits on PR FULL-NAME/ID asynchronously.
CALLBACK gets a list of plists (:hash :summary :author :date), newest
first, or nil on error.  Normalised because the platforms disagree on
nearly every field name, so the detail view renders one shape.  Async
only: a synchronous twin would block the detail view.")
(gp-defop create-comment (full-name id text &optional inline parent-id)
  "Create a comment on PR FULL-NAME/ID.")
(gp-defop add-review-batch-comment (full-name id text inline &optional parent-id)
  "Add an inline comment on PR FULL-NAME/ID to the caller's review batch.
Unlike `gp-create-comment', the comment stays invisible to others until
`gp-submit-review-batch' (Bitbucket's `pending' comment flag; GitHub's
PENDING review, built one `addPullRequestReviewThread' at a time).")
(gp-defop review-batch-comments (full-name id)
  "Return the caller's own not-yet-submitted review-batch comments on PR.
Shaped like `gp-pull-request-comments' entries (so the same renderer
draws them, and `gp-comment-pending-p' answers non-nil).")
(gp-defop review-batch-comments-async (full-name id callback)
  "Async twin of `gp-review-batch-comments'; CALLBACK gets the list, or nil.")
(gp-defop submit-review-batch (full-name id &optional event body)
  "Submit every pending review-batch comment on PR FULL-NAME/ID.
EVENT is `approve', `changes', `comment', or nil (post the comments
with no PR-level verdict, where the platform allows it).  BODY is an
optional summary comment posted alongside the verdict.")
(gp-defop discard-review-batch (full-name id)
  "Delete every pending review-batch comment on PR FULL-NAME/ID.")
(gp-defop resolve-comment (full-name id comment-id)
  "Resolve COMMENT-ID on PR FULL-NAME/ID.")
(gp-defop reopen-comment (full-name id comment-id)
  "Reopen COMMENT-ID on PR FULL-NAME/ID.")
(gp-defop edit-comment (full-name id comment-id text)
  "Replace COMMENT-ID's body with TEXT on PR FULL-NAME/ID.")
(gp-defop delete-comment (full-name id comment-id)
  "Delete COMMENT-ID on PR FULL-NAME/ID.")
(gp-defop set-pull-request-draft (full-name id draft &optional title)
  "Set PR FULL-NAME/ID draft flag to DRAFT.")
(gp-defop pull-request-merge-strategies (full-name id)
  "Return (STRATEGIES . DEFAULT) permitted for PR FULL-NAME/ID, or nil.
STRATEGIES is a list of opaque backend tokens; DEFAULT is the one the
forge would apply on its own.  Both come from the forge (Bitbucket:
per destination branch; GitHub: per repository).  Nil means unknown:
callers should merge without naming a strategy.")

(gp-defop pull-request-mergeability (full-name id)
  "Return (MERGEABLE . STATE) for PR FULL-NAME/ID, or nil.
MERGEABLE is t when the forge says it will merge cleanly, nil when it
reports conflicts, and `unknown' when it has not decided yet.  STATE is
a backend word for display.  Nil overall means the backend cannot answer
-- Bitbucket Cloud's PR payload has no mergeability field -- in which
case callers must not block the merge on it.")

(gp-defop pull-request-divergence (full-name base head)
  "Return (AHEAD . BEHIND) commit counts of HEAD against BASE, or nil.
BEHIND is how far behind BASE the branch is.  Nil means the backend
cannot say -- Bitbucket Cloud exposes diffs but no ahead/behind counts --
so callers must simply omit the information rather than show a zero.")

(gp-defop merge-pull-request (full-name id &optional strategy message close-source-branch)
  "Merge PR FULL-NAME/ID and return the backend's result.
STRATEGY is one of `gp-pull-request-merge-strategies' (nil lets the forge
apply its default).  MESSAGE overrides the merge commit message.
CLOSE-SOURCE-BRANCH asks the forge to delete the source branch; where a
backend has no per-merge control (GitHub decides by repository setting)
it is ignored.  Either way the forge owns the remote branch -- callers
must never delete it themselves.")

(gp-defop set-pull-request-title (full-name id title)
  "Set PR FULL-NAME/ID's title to TITLE, returning the updated PR.
Backends differ in what else they must resend: Bitbucket's update is a
whole-object PUT (so the description rides along or is lost), GitHub's a
partial PATCH.  An empty title is rejected rather than sent.")

(gp-defop set-pull-request-description (full-name id description &optional title)
  "Set PR FULL-NAME/ID's description/body to DESCRIPTION.
TITLE is an optimisation for backends whose update is a whole-object
PUT (Bitbucket), which must resend the title to avoid blanking it;
pass it when the caller already has it to save a fetch; GitHub ignores
it.")
(gp-defop approve-pr (full-name id &optional unapprove reason)
  "Approve PR FULL-NAME/ID (UNAPPROVE non-nil retracts it).
REASON is a message shown alongside the retraction, used only when
`gp-review-retraction-kind' reports `dismiss' for this backend
(ignored otherwise).")
(gp-defop request-changes-pr (full-name id &optional unrequest reason)
  "Request changes on PR FULL-NAME/ID (UNREQUEST non-nil retracts it).
REASON is as in `gp-approve-pr'.")
(gp-defop review-retraction-kind ()
  "Return how withdrawing your own review reads to other viewers.
`retract' means it disappears without a trace (Bitbucket).  `dismiss'
means it stays visible in the timeline as a dismissal, usually with a
reason (GitHub -- see `github--dismiss-own-review').  Callers label the
action accordingly and prompt for REASON only for `dismiss'.")
(gp-defop open-pr-for-branch (full-name branch)
  "Return the open PR in FULL-NAME whose source branch is BRANCH.")
(gp-defop repo-default-branch (full-name)
  "Return repo FULL-NAME's default (main) branch name, or nil.")
(gp-defop repo-branches (full-name)
  "Return every branch name in FULL-NAME, for the create-PR destination picker.")
(gp-defop repo-default-reviewers (full-name)
  "Return repo FULL-NAME's default reviewers (list of user alists).
Pre-selected in the create-PR form, since the platform auto-adds them
to every new PR.  Contrast `gp-repo-suggested-reviewers'.")
(gp-defop set-pull-request-reviewers (full-name id reviewer-ids &optional current-ids)
  "Set PR ID in FULL-NAME to have exactly REVIEWER-IDS as reviewers.
REVIEWER-IDS is the complete desired list (Bitbucket uuids, GitHub
logins), not a delta.  CURRENT-IDS, when given, is the PR's present
reviewer list, so GitHub can compute its add/remove delta without
re-fetching.

Only reviewers who have not yet submitted a review can be removed:
GitHub keeps a submitted review regardless, so callers must not offer
to withdraw one (see `gp-ui-edit-reviewers').")
(gp-defop repo-suggested-reviewers (full-name)
  "Return reviewers the platform suggests (not auto-adds) for FULL-NAME.
A list of user alists shaped like `gp-repo-default-reviewers', left
unchecked in the create-PR form.  Bitbucket always returns nil.")
(gp-defop create-pull-request (full-name source dest title &optional description draft close-source-branch reviewer-uuids)
  "Open a PR in FULL-NAME from SOURCE into DEST with TITLE.
DESCRIPTION/DRAFT/CLOSE-SOURCE-BRANCH/REVIEWER-UUIDS are optional.")
(gp-defop repo-open-pr-count (full-name)
  "Return the open-PR count for repo FULL-NAME.")
(gp-defop repo-pull-requests (full-name &optional state)
  "Return the open PRs in repo FULL-NAME.")
(gp-defop commit-build-states (full-name hash)
  "Return the build state strings for commit HASH in FULL-NAME.")
(gp-defop commit-build-states-async (full-name hash callback)
  "Fetch the build state strings for commit HASH in FULL-NAME asynchronously.
CALLBACK receives the list of state strings (possibly empty), or nil
on error.")
(gp-defop resolve-mentions (text)
  "Return TEXT with any platform-specific mention tokens resolved to names.
Bitbucket encodes mentions as opaque \"@{account_id}\" tokens needing
a lookup; GitHub's are already literal \"@username\" text, so its
implementation is the identity function.")

;;;; CI pipelines --------------------------------------------------------------
(gp-defop pipelines-for-branch (full-name branch &optional max-items commit)
  "Return CI pipelines in FULL-NAME for BRANCH (filtered to COMMIT if given).")
(gp-defop pipelines-for-branch-async (full-name branch max-items commit callback)
  "Fetch CI pipelines in FULL-NAME for BRANCH asynchronously.
CALLBACK gets the list of pipelines (filtered to COMMIT if given),
or nil on error.  Non-blocking twin of `gp-pipelines-for-branch',
which the detail view polls.")
(gp-defop pipeline-steps (full-name pipeline-uuid)
  "Return the steps of PIPELINE-UUID in FULL-NAME, in order.")
(gp-defop pipeline-steps-async (full-name pipeline-uuid callback)
  "Fetch the steps of PIPELINE-UUID in FULL-NAME asynchronously.
CALLBACK gets the list of steps, or nil on error.  Non-blocking twin
of `gp-pipeline-steps'.")
(gp-defop commit-message-async (full-name hash callback)
  "Fetch the commit message for HASH in FULL-NAME asynchronously.
CALLBACK gets the message string, or nil.  Non-blocking twin of
`gp-commit-message'; results are cached since messages are immutable.")
(gp-defop pipeline-stop (full-name pipeline-uuid)
  "Stop running PIPELINE-UUID in FULL-NAME (pipeline-level).")
(gp-defop pipeline-trigger (full-name branch &optional selector variables)
  "Trigger a pipeline in FULL-NAME for BRANCH (pipeline-level).")
(gp-defop pipeline-run-manual-step (full-name branch pipeline step)
  "Run a waiting manual STEP of PIPELINE in FULL-NAME on BRANCH.")
(gp-defop pipeline-step-rerun (full-name pipeline-uuid step)
  "Re-run just STEP of PIPELINE-UUID in FULL-NAME, in place.
Distinct from `gp-pipeline-run-manual-step': this restarts a finished
\(typically failed) step, not a gated step awaiting its first run.
Call only when `gp-pipeline-step-rerunnable-p' is non-nil for STEP; a
backend without the capability signals a `user-error'.")
(gp-defop pipeline-web-url (full-name pipeline &optional step)
  "Return the web-UI URL for PIPELINE in FULL-NAME (deep-linked to STEP).")
(gp-defop pipeline-step-log (full-name pipeline-uuid step-uuid)
  "Return the captured log text for STEP-UUID of PIPELINE-UUID.")
(gp-defop pipeline-step-log-classify-line (line)
  "Classify one raw LINE of a step log, for display in the log buffer.
Returns a cons (KIND . TEXT): KIND is `command' (the platform is
echoing a shell command it is about to run), `group' (a named section
header/footer), `error', `warning', or nil for an ordinary output
line; TEXT is LINE with that platform's own marker/prefix stripped
\(unchanged for nil).  Pure text classifier: Bitbucket echoes commands
as \"+ \", GitHub uses a timestamp prefix and \"##[group]\"-style
markers.")

;;;; Field accessors (JSON shape differs per platform) ------------------------

(gp-defop pr-full-name (pr)
  "Return the repository \"owner/slug\" for PR.")
(gp-defop pr-source-branch (pr)
  "Return PR's source branch name.")
(gp-defop pr-source-commit (pr)
  "Return PR's source (head) commit hash, or nil.")
(gp-defop pr-destination-branch (pr)
  "Return PR's destination (base) branch name.")
(gp-defop pr-web-url (pr)
  "Return the browser URL for PR, or nil.
Bitbucket nests it at `links.html.href'; GitHub has a flat `html_url',
so never read either field directly.")

(gp-defop comment-web-url (comment)
  "Return the browser URL for COMMENT, or nil.
Same per-forge split as `gp-pr-web-url'.")

(gp-defop pr-draft-p (pr)
  "Return non-nil if PR is a draft.")
(gp-defop pr-authored-by-p (pr uuid)
  "Return non-nil if PR was authored by UUID.")
(gp-defop pr-author-name (pr)
  "Return PR's author display name, or nil.")
(gp-defop pr-author-avatar (pr)
  "Return PR's author avatar image URL, or nil.")
(gp-defop pr-open-p (pr)
  "Return non-nil if PR is open.
Bitbucket's `state' is \"OPEN\", GitHub's \"open\", so callers must
never compare `state' against a literal.")
(gp-defop pr-closed-reason (pr)
  "Return the reason a PR was declined/closed, or nil.
Bitbucket carries a free-text `reason' on a declined PR; GitHub has
none, so its backend returns nil.")

(gp-defop pr-merged-at (pr)
  "Return when PR was merged, as an ISO-8601 string, or nil.
GitHub has `merged_at'.  Bitbucket falls back to `updated_on' (last
activity, possibly a later comment): good enough to group by day.")

(gp-defop pr-merge-commit (pr)
  "Return the commit a merged PR landed as on its destination branch, or nil.
The build worth watching after a merge runs on this commit, not on the
PR branch's last one -- that is the run that deploys.")

(gp-defop pr-merged-p (pr)
  "Return non-nil if PR was merged.
Bitbucket reports `state' = \"MERGED\"; GitHub a separate boolean
`merged' alongside `state' = \"closed\".")
(gp-defop pr-repo-slug (pr)
  "Return PR's bare repository slug (no owner/workspace prefix), or nil.
Contrast `gp-pr-full-name', which includes the owner/workspace.")
(gp-defop pr-review-tally (pr)
  "Return a plist (:approved :changes :pending) over PR's reviewers.
Bitbucket answers from PR's embedded `participants' (no network);
GitHub fetches synchronously, so callers rendering many PRs should use
`gp-pr-review-tally-async' instead.")
(gp-defop pr-review-tally-async (pr callback)
  "Fetch PR's review tally asynchronously; CALLBACK gets the plist.
Non-blocking twin of `gp-pr-review-tally'.  Bitbucket re-fetches PR
bypassing the cache, since a cached PR's votes go stale.")
(gp-defop pr-reviewers-async (pr callback)
  "Fetch PR's individual reviewers asynchronously.
CALLBACK gets a list of plists (:name :avatar :state), STATE one of
`approved'/`changes'/`pending' -- the per-person breakdown behind
`gp-pr-review-tally'.  Both backends fetch (Bitbucket re-fetches PR
for fresh participants).")
(gp-defop pr-my-review-state (pr uuid)
  "Return UUID's own review state on PR: `approved', `changes', or nil.")
(gp-defop pr-comment-count (pr)
  "Return PR's total comment count (all kinds), or nil if unknown.
Bitbucket's PR object carries this pre-summed as `comment_count'.
GitHub has no single field for it -- general and inline (review)
comments are counted separately as `comments'/`review_comments' --
so this sums both.")
(gp-defop pr-labels (pr)
  "Return PR's labels as a list of plists (:name :color), or nil.
COLOR is a 6-digit hex string without the leading \"#\", or nil.
GitHub embeds labels in the PR payload (no extra request).  Bitbucket
has no PR labels and returns nil, so \"no labels\" and \"unsupported\"
are different questions (see `gp-labels-supported-p').")

(gp-defop pr-description (pr)
  "Return PR's description/body as a Markdown string, or nil when empty.
Bitbucket calls this `description', GitHub `body'.  Returns nil rather
than \"\" when empty, so callers can test the value directly.")
(gp-defop comment-resolved-p (comment)
  "Return non-nil if COMMENT is resolved.")
(gp-defop comment-resolvable-p (comment)
  "Return non-nil if COMMENT supports resolve/reopen at all.
Bitbucket comments always are.  On GitHub only inline review comments
belong to a resolvable thread; callers hide the action when nil.")
(gp-defop comment-own-p (comment uuid)
  "Return non-nil if COMMENT was written by UUID.")
(gp-defop comment-pending-p (comment)
  "Return non-nil if COMMENT is part of an unsubmitted review batch.
Only ever true for the caller's own comments -- both platforms hide
someone else's pending review entirely (see `gp-review-batch-comments').")

(gp-defop inline-target-problem (full-name id path line)
  "Return a human explanation if PATH:LINE cannot take an inline comment.
Nil means go ahead.  GitHub accepts inline comments only on lines in
the PR's diff (else a bare 422 after the text is written); Bitbucket
accepts any line.  Checking up front keeps the user's text.")

(gp-defop reactions-supported-p ()
  "Return non-nil if this platform has reactions on comments at all.
The UI hides every reaction affordance when nil.

Nil for Bitbucket Cloud: the web UI's Like has no v2.0 REST route
\(`/comments/{id}/likes' 404s) and emoji reactions are only a feature
request (BCLOUD-21346).  True for GitHub.")

(gp-defop reaction-choices ()
  "Return the reaction contents this platform accepts, most-liked first.
A list of opaque token strings, e.g. GitHub's eight (\"+1\", \"heart\",
…).")

(gp-defop comment-reactions (full-name comment)
  "Return COMMENT's reactions in FULL-NAME as a list of alists.
Each entry carries at least `content' and a `user' whose id matches
`gp-user-uuid', so callers can both count reactions and tell which are
their own.  Platforms without reactions return nil.")

(gp-defop set-comment-reaction (full-name comment content on)
  "Add (ON non-nil) or remove COMMENT's CONTENT reaction in FULL-NAME.
CONTENT is one of `gp-reaction-choices'.  Adding one the user already
has is a no-op rather than an error, so callers can toggle blindly.
Returns non-nil when the reaction set changed.")

(gp-defop labels-supported-p ()
  "Return non-nil if this platform has PR labels at all.
Distinct from a PR merely having none: the UI hides every label
affordance when nil.  True for GitHub, nil for Bitbucket.")

(gp-defop repo-labels (full-name)
  "Return the labels defined in repo FULL-NAME, as `gp-pr-labels' plists.
The whole set offered when editing a PR's labels.  Backends without
labels return nil.")

(gp-defop set-pull-request-labels (full-name id labels)
  "Set PR ID in FULL-NAME to carry exactly LABELS (a list of name strings).
Callers pass the complete desired set, not a delta, matching
`gp-set-pull-request-reviewers'.  Backends without labels signal a
`user-error' rather than silently doing nothing.")

(gp-defop backend-name ()
  "Return the symbol naming this backend (`bitbucket', `github', …).
Lets configuration key off the active platform.")

(defcustom gp-comment-delete-others nil
  "Backends on which you may delete comments written by other users.
Deleting someone else's comment needs elevated permissions the APIs
do not advertise, so declare it here only where you hold them.
Elsewhere the delete action stays hidden on other people's comments.

Value is nil (never), t (every backend), or a list of backend
symbols, e.g. `(bitbucket)'."
  :type '(choice (const :tag "Never -- only my own comments" nil)
                 (const :tag "All backends" t)
                 (repeat :tag "Only these backends" symbol))
  :group 'bitbucket)

(defun gp-comment-delete-others-allowed-p ()
  "Return non-nil if `gp-comment-delete-others' covers the active backend."
  (cond ((eq gp-comment-delete-others t) t)
        ((consp gp-comment-delete-others)
         (and (memq (gp-backend-name) gp-comment-delete-others) t))
        (t nil)))

(defun gp-comment-deletable-p (comment uuid)
  "Return non-nil if COMMENT may be deleted by the user identified by UUID.
True for your own comments always, and for anyone's when
`gp-comment-delete-others' enables it for the active backend."
  (or (gp-comment-own-p comment uuid)
      (gp-comment-delete-others-allowed-p)))

;; Pipeline / step shape accessors (kept backend-free for the UI).
(gp-defop pipeline-state (pipeline)
  "Return PIPELINE's coarse state string, or nil.")
(gp-defop pipeline-result (pipeline)
  "Return PIPELINE's result/stage string, or nil.")
(gp-defop pipeline-finished-p (pipeline)
  "Return non-nil if PIPELINE has finished.")
(gp-defop pipeline-id (pipeline)
  "Return the identifier PIPELINE's own endpoints accept, or nil.
Bitbucket keys a pipeline by `uuid'; GitHub Actions a run by `id'.
Never read `uuid' directly: on GitHub it is nil and steps never load.")
(gp-defop pipeline-step-id (step)
  "Return the identifier STEP's own endpoints accept, or nil.
As `gp-pipeline-id', for a step/job: Bitbucket `uuid', Actions `id'.")
(gp-defop pipeline-number (pipeline)
  "Return PIPELINE's build number, or nil.")
(gp-defop pipeline-commit (pipeline)
  "Return PIPELINE's target commit hash, or nil.")
(gp-defop commit-message (full-name hash)
  "Return the commit message for HASH in FULL-NAME, or nil.")
(gp-defop commit-summary (message)
  "Return the first line of a commit MESSAGE, trimmed.")
(gp-defop pipeline-step-state (step)
  "Return STEP's coarse state string.")
(gp-defop pipeline-step-result (step)
  "Return STEP's result string, or nil.")
(gp-defop pipeline-step-running-p (step)
  "Return non-nil if STEP is currently running.")
(gp-defop pipeline-step-manual-p (step)
  "Return non-nil if STEP is a manual (on-demand) step.")
(gp-defop pipeline-step-runnable-manual-p (step)
  "Return non-nil if STEP is a manual step waiting to be started.")
(gp-defop pipeline-step-rerunnable-p (step)
  "Return non-nil if STEP can be individually re-run in place.
Always nil on Bitbucket (no per-step re-run).  GitHub Actions can
re-run a single job once it has finished.  Callers hide the action
when nil.")
(gp-defop pipelines-sort (pipelines step-counts)
  "Sort PIPELINES most-steps-first (STEP-COUNTS maps uuid->count).")
(gp-defop pipelines-match-commit (pipelines commit)
  "Return the PIPELINES whose target commit matches COMMIT.")

;;;; Buffer naming --------------------------------------------------------------

(defcustom gp-buffer-name-prefix "gp: "
  "Prefix tagging every buffer this package creates.
Buffer names are built as \"*PREFIX SUFFIX*\" -- e.g. \"*gp: PRs*\".
Read at buffer-creation time, so existing buffers keep their names
until recreated."
  :type 'string
  :group 'bitbucket)

(defun gp--buffer-name (suffix)
  "Return the package buffer name for SUFFIX, e.g. \"*gp: PRs*\"."
  (format "*%s%s*" gp-buffer-name-prefix suffix))

;;;; TTL result cache -----------------------------------------------------------

;; Shared by both backends and the UI layers.

(defcustom gp-cache-ttl 300
  "Seconds to cache PR-list results (default 5 minutes).
Set to 0 to disable caching."
  :type 'integer
  :group 'bitbucket)

(defvar gp--result-cache (make-hash-table :test 'equal)
  "KEY -> (EXPIRY . VALUE) cache for PR-list fetches.")

(defun gp-cache-clear ()
  "Clear cached PR-list results (forces a fresh fetch)."
  (interactive)
  (clrhash gp--result-cache))

(defun gp-cache-remove (key)
  "Remove KEY from the result cache, if present."
  (remhash key gp--result-cache))

(defun gp-cache-remove-matching (predicate)
  "Remove every cache entry whose KEY satisfies PREDICATE.
Cache keys are lists tagged by their first element (e.g. `pull-request',
`mine', `reviewing'); PREDICATE receives the raw key list."
  (let (doomed)
    (maphash (lambda (k _v) (when (funcall predicate k) (push k doomed)))
              gp--result-cache)
    (dolist (k doomed) (remhash k gp--result-cache))))

(defun gp-invalidate-pr-caches (pr)
  "Invalidate every cache entry that could now show stale data for PR.
Call this after any action that mutates PR on the server, alongside
refreshing the buffer that showed it; otherwise the list caches and
this PR's `pull-request'/`pr-stats'/`pr-diff' entries stay stale until
they expire.  List-level caches are cleared unconditionally: they are
cheap to recompute."
  (let* ((full-name (ignore-errors (gp-pr-full-name pr)))
         (id (alist-get 'id pr))
         (commit (ignore-errors (gp-pr-source-commit pr))))
    (when (and full-name id)
      (gp-cache-remove (list 'pull-request full-name id))
      (when commit
        (gp-cache-remove (list 'pr-stats full-name id commit))
        (gp-cache-remove (list 'pr-diff full-name id commit))))
    (gp-cache-remove-matching
     (lambda (k) (memq (car-safe k) '(mine reviewing others))))))

(defun gp-cache-get (key)
  "Return (FOUND . VALUE) for KEY from the result cache.
FOUND is nil on a miss/expiry.  Honours `gp-cache-ttl' = 0 (always a miss)."
  (if (<= gp-cache-ttl 0)
      (cons nil nil)
    (let ((entry (gethash key gp--result-cache)))
      (if (and entry (< (float-time) (car entry)))
          (progn (gp-log 'cache "hit %S" key) (cons t (cdr entry)))
        (cons nil nil)))))

(defun gp-cache-put (key value &optional ttl)
  "Cache VALUE under KEY for TTL seconds, defaulting to `gp-cache-ttl'.
A short TTL suits a still-settling value (e.g. an unfinished build).
A TTL of 0 or less skips caching.  Returns VALUE."
  (let ((ttl (or ttl gp-cache-ttl)))
    (when (> ttl 0)
      (puthash key (cons (+ (float-time) ttl) value) gp--result-cache)))
  value)

(defun gp-cache-with-cache (key thunk &optional ttl)
  "Return cached value for KEY, or call THUNK, caching the result.
TTL is passed to `gp-cache-put' (see there); it defaults to
`gp-cache-ttl'."
  (let ((hit (gp-cache-get key)))
    (if (car hit)
        (cdr hit)
      (gp-cache-put key (funcall thunk) ttl))))

;;;; Platform-agnostic helpers ------------------------------------------------

(defun gp-partition-pull-requests (prs uuid)
  "Split PRS into a cons (MINE . REVIEWING) by UUID authorship."
  (let (mine reviewing)
    (dolist (pr prs)
      (if (gp-pr-authored-by-p pr uuid)
          (push pr mine)
        (push pr reviewing)))
    (cons (nreverse mine) (nreverse reviewing))))

(defun gp-categorize-pull-requests (prs uuid)
  "Categorise PRS for UUID into a plist (:reviewing :mine :drafts).
A draft authored by the user goes to :drafts; non-draft authored
PRs to :mine; everything else to :reviewing."
  (let (mine reviewing drafts)
    (dolist (pr prs)
      (cond
       ((and (gp-pr-authored-by-p pr uuid) (gp-pr-draft-p pr))
        (push pr drafts))
       ((gp-pr-authored-by-p pr uuid)
        (push pr mine))
       (t (push pr reviewing))))
    (list :reviewing (nreverse reviewing)
          :mine (nreverse mine)
          :drafts (nreverse drafts))))

(defun gp-build-states-summary (states)
  "Reduce build STATES to one symbol, or nil when there are none.
One of `failed', `running', `stopped' or `successful', in that
order of precedence."
  (cond ((null states) nil)
        ((member "FAILED" states) 'failed)
        ((member "INPROGRESS" states) 'running)
        ((member "STOPPED" states) 'stopped)
        (t 'successful)))

(defun gp-split-diff-by-file (diff)
  "Split unified DIFF text into an alist of (PATH . CHUNK).
PATH is the new-side path (\"+++ b/PATH\"), falling back to the
old side for deletions.  CHUNK is that file's full diff text."
  (when diff
    (let ((case-fold-search nil) starts result)
      (with-temp-buffer
        (insert diff)
        (goto-char (point-min))
        (while (re-search-forward "^diff --git " nil t)
          (push (match-beginning 0) starts))
        (setq starts (nreverse starts))
        (cl-loop for (beg . rest) on starts
                 for end = (or (car rest) (point-max))
                 do (let* ((full (buffer-substring-no-properties beg end))
                           (path (cond
                                  ((string-match "^\\+\\+\\+ b/\\(.+\\)$" full)
                                   (match-string 1 full))
                                  ((string-match "^--- a/\\(.+\\)$" full)
                                   (match-string 1 full)))))
                      (when path
                        (push (cons (string-trim-right path) full) result)))))
      (nreverse result))))

(defun gp-diff-chunk-new-lines (chunk)
  "Return a hash-set of the new-side line numbers present in diff CHUNK.
Walks the `@@ -a,b +c,d @@' hunk headers and counts context and
added lines (the lines that exist on the new side), so a comment
anchored to any of them is still on current code.  Deleted lines
are not counted -- they no longer exist on the new side."
  (let ((present (make-hash-table :test 'eq))
        (case-fold-search nil))
    (with-temp-buffer
      (insert (or chunk ""))
      (goto-char (point-min))
      (let ((lineno nil))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (cond
             ((string-match "^@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)" line)
              (setq lineno (string-to-number (match-string 1 line))))
             ((null lineno) nil)                       ;; preamble before any hunk
             ((string-prefix-p "-" line) nil)          ;; old side only
             ((string-prefix-p "\\" line) nil)         ;; "\ No newline at end"
             (t                                        ;; context (" ") or added ("+")
              (puthash lineno t present)
              (setq lineno (1+ lineno)))))
          (forward-line 1))))
    present))

(defvar gp--diff-lines-cache (make-hash-table :test 'eq :weakness 'key)
  "Per-render memo: diff CHUNK string -> its new-line hash-set.
Keyed by identity and weak, so entries vanish with the render's
DIFF-BY-FILE alist.")

(defun gp-diff-chunk-new-lines--cached (chunk)
  "Return `gp-diff-chunk-new-lines' for CHUNK, memoised by identity."
  (or (gethash chunk gp--diff-lines-cache)
      (puthash chunk (gp-diff-chunk-new-lines chunk) gp--diff-lines-cache)))

(defvar gp--comment-outdated-cache (make-hash-table :test 'eql)
  "Comment id -> t once it has been proven outdated.
Outdatedness is monotonic, so a true verdict is cached for the session;
a false one never is, since the next commit can still make it stale.")

(defun gp-comment-outdated-p (comment diff-by-file)
  "Return non-nil when inline COMMENT is anchored to a stale line.
Trusts the forge's own `inline.outdated' flag when COMMENT carries one.
Bitbucket's list-comments endpoint omits that flag (only the
single-comment endpoint returns it, an N+1 we avoid), so in practice
the fallback heuristic decides: DIFF-BY-FILE is the alist from
`gp-split-diff-by-file', and a comment is outdated when its file is in
the diff but its anchored new-side line is in no hunk.  The heuristic
can under-flag when the anchor coincides with an unrelated added line.

Returns nil when COMMENT is not inline, the diff is nil, or the file is
absent from the diff.  A true verdict is memoised by comment id (see
`gp--comment-outdated-cache')."
  (let ((id (alist-get 'id comment)))
    (or (and id (gethash id gp--comment-outdated-cache))
        (let* ((flagged (let-alist comment .inline.outdated))
               (path (let-alist comment .inline.path))
               (line (let-alist comment (or .inline.to .inline.from)))
               (chunk (and path diff-by-file (cdr (assoc path diff-by-file))))
               (outdated (or flagged
                             (and path line chunk
                                  (not (gethash line (gp-diff-chunk-new-lines--cached chunk)))))))
          (when (and outdated id)
            (puthash id t gp--comment-outdated-cache))
          outdated))))

;;;; OS notifications -----------------------------------------------------------

(defcustom gp-notify t
  "Whether this package may raise OS (desktop) notifications at all.
Nil silences every notification.  Per-feature options (e.g.
`gp-pipeline-deploy-notify') narrow this further: a notification is
sent only when BOTH this and the feature switch are non-nil."
  :type 'boolean :group 'bitbucket)

(defcustom gp-notify-function nil
  "Function used to raise a notification, or nil for the built-in one.
Called with (TITLE BODY URGENT).  Set this to route notifications
through `alert', a pager, or anything else; the default tries D-Bus
`notifications-notify' and macOS `osascript' before falling back to
the echo area."
  :type '(choice (const :tag "Built-in" nil) function)
  :group 'bitbucket)

(defun gp-notify (title body &optional urgent)
  "Raise an OS notification with TITLE and BODY; URGENT marks a failure.
A no-op unless `gp-notify' is non-nil.  Uses `gp-notify-function' when
set, else D-Bus, `osascript' (macOS), or the echo area.  Notifier
errors are swallowed so they never break the reporting operation.
Returns non-nil when something was dispatched."
  (when gp-notify
    (condition-case nil
        (cond
         (gp-notify-function
          (funcall gp-notify-function title body urgent)
          t)
         ((and (fboundp 'notifications-notify)
               (not (eq system-type 'darwin)))
          (notifications-notify :title title :body body
                                :urgency (if urgent 'critical 'normal))
          t)
         ((eq system-type 'darwin)
          ;; `display notification' takes these as AppleScript string
          ;; literals, so quotes and backslashes must be escaped.
          (let ((esc (lambda (s)
                       (replace-regexp-in-string
                        "[\"\\\\]" "\\\\\\&" (or s "")))))
            (call-process "osascript" nil 0 nil
                          "-e" (format "display notification \"%s\" with title \"%s\"%s"
                                       (funcall esc body)
                                       (funcall esc title)
                                       (if urgent " sound name \"Basso\"" ""))))
          t)
         (t (message "%s: %s" title body) t))
      (error nil))))

;;;; Markdown / emoji rendering helpers ---------------------------------------

(defcustom gp-resolve-emoji-shortcodes t
  "When non-nil, turn :shortcode: tokens in comments into emoji.
Uses the `emojify' package's database when available; otherwise a
small built-in fallback covers the common ones."
  :type 'boolean :group 'bitbucket)

(defconst gp--emoji-fallback
  '((":thinking:" . "🤔") (":smile:" . "😄") (":+1:" . "👍") (":-1:" . "👎")
    (":tada:" . "🎉") (":rocket:" . "🚀") (":fire:" . "🔥") (":eyes:" . "👀")
    (":warning:" . "⚠️") (":bug:" . "🐛") (":sparkles:" . "✨")
    (":heavy_check_mark:" . "✔️") (":x:" . "❌") (":wave:" . "👋")
    (":pray:" . "🙏") (":raised_hands:" . "🙌") (":100:" . "💯")
    (":heart:" . "❤️") (":laughing:" . "😆") (":thumbsup:" . "👍")
    (":thumbsdown:" . "👎") (":ok_hand:" . "👌") (":clap:" . "👏"))
  "Fallback shortcode->emoji map used when `emojify' is unavailable.")

(declare-function emojify-get-emoji "emojify")
(declare-function emojify-create-emojify-emojis "emojify")
(declare-function ht-get "ht")

(defun gp--emoji-for (shortcode)
  "Return the unicode emoji for SHORTCODE (e.g. \":thinking:\"), or nil."
  (or (when (require 'emojify nil t)
        (ignore-errors
          (emojify-create-emojify-emojis)
          (let ((e (emojify-get-emoji shortcode)))
            (and e (ht-get e "unicode")))))
      (cdr (assoc shortcode gp--emoji-fallback))))

(defconst gp--markup-regexp
  (concat
   ;; 1: inline code, left untouched
   "\\(`[^`\n]*`\\)"
   ;; 2 alt, 3 url: a Markdown image, with optional {: ...} attributes
   "\\|!\\[\\([^]\n]*\\)\\](\\([^)\n]+\\))\\(?:{:[^}\n]*}\\)?"
   ;; 4 shortcode, 5 attributes: Bitbucket's {: ...} attribute list
   "\\|\\(:[a-z0-9_+-]+:\\)?{:\\([^}\n]*\\)}")
  "Regexp for `gp-clean-markup'.")

(defun gp-clean-markup (text)
  "Rewrite markup in TEXT that Emacs cannot show into readable text.
Bitbucket appends kramdown attribute lists, \"{: ... }\", to emojis,
pasted images and link cards; they show up as raw text.  An emoji
becomes the character named in its data-emoji-text attribute (or its
:shortcode: when there is none); any other attribute list is dropped.
An image becomes a link labelled \"🖼 ALT\" (\"🖼 Image\" without alt
text), because its file is only served to a browser session.
Inline code is left untouched."
  (if (null text)
      ""
    (replace-regexp-in-string
     gp--markup-regexp
     (lambda (m)
       (cond
        ((match-string 1 m) m)
        ((match-string 3 m)
         (let ((alt (string-trim (match-string 2 m))))
           (format "[🖼 %s](%s)" (if (string-empty-p alt) "Image" alt)
                   (match-string 3 m))))
        (t
         (let ((shortcode (match-string 4 m))
               (attrs (match-string 5 m)))
           ;; `replace-regexp-in-string' still needs its own match data
           (save-match-data
             (if (string-match "data-emoji-text=['\"]\\([^'\"]+\\)['\"]" attrs)
                 (match-string 1 attrs)
               (or shortcode "")))))))
     text t t)))

(defun gp-resolve-emojis (text)
  "Replace :shortcode: tokens in TEXT with their emoji, when enabled.
Tokens inside inline/fenced code are left untouched."
  (if (or (not gp-resolve-emoji-shortcodes) (null text))
      (or text "")
    (let ((case-fold-search t))
      (replace-regexp-in-string
       "\\(`[^`]*`\\)\\|:\\([a-z0-9_+-]+\\):"
       (lambda (m)
         (if (match-string 1 m)
             m
           (or (gp--emoji-for (downcase m)) m)))
       text t t))))

(defcustom gp-ticket-key-pattern "\\b[A-Za-z]\\{2,3\\}-[0-9]\\{1,5\\}\\b"
  "Regexp matching an external ticket key, e.g. \"WP-1231\".
Default is a 2-3 letter project prefix, a dash, then 1-5 digits; it
deliberately never matches a bare GitHub \"#123\" reference.  Applied
to PR descriptions and commit summaries (see `gp-ticket-linkify-string',
`gp--linkify')."
  :type 'regexp :group 'bitbucket)

(defcustom gp-ticket-url-format nil
  "URL template for turning a ticket key into a link, or nil.
A format string with one `%s' for the ticket key, e.g.
\"https://mycompany.atlassian.net/browse/%s\".  When nil, ticket keys
are still highlighted, and clicking one reports that this needs setting."
  :type '(choice (const :tag "Not configured" nil) string)
  :group 'bitbucket)

(defun gp-ticket-url-for (key)
  "Return the configured ticket URL for KEY, or nil if unconfigured."
  (and gp-ticket-url-format (format gp-ticket-url-format key)))

(defun gp-ticket--open (key)
  "Open KEY's ticket URL, or report that `gp-ticket-url-format' needs setting."
  (let ((url (gp-ticket-url-for key)))
    (if url
        (browse-url url)
      (message "Set `gp-ticket-url-format' to link ticket keys like %S" key))))

(defun gp-ticket--keymap (key)
  "Return a keymap opening KEY's ticket link on RET/mouse-1/mouse-2.
`mouse-2' must be bound too: `mouse-1-click-follows-link' turns a
mouse-1 click on a `follow-link' region into mouse-2 before lookup."
  (let ((m (make-sparse-keymap))
        (cmd (lambda () (interactive) (gp-ticket--open key))))
    (define-key m [mouse-1] cmd)
    (define-key m [mouse-2] cmd)
    (define-key m (kbd "RET") cmd)
    m))

(defun gp-ticket--props (key)
  "Return the text properties for a ticket-key match on KEY."
  (list 'face 'link 'mouse-face 'highlight
        'help-echo (or (gp-ticket-url-for key)
                       (format "Set `gp-ticket-url-format' to link %s" key))
        'follow-link t 'keymap (gp-ticket--keymap key)))

(defun gp-ticket-linkify-string (text)
  "Return TEXT with ticket keys (`gp-ticket-key-pattern') turned into links.
Pure -- returns a fresh string.  Skips a match already faced `link'
\(from `gp-linkify-string' running first, say), so a key that happens
to sit inside an already-linkified URL is not re-propertized."
  (if (not text)
      text
    (let ((s (copy-sequence text)) (i 0))
      (while (string-match gp-ticket-key-pattern s i)
        (let ((b (match-beginning 0)) (e (match-end 0)))
          (unless (eq (get-text-property b 'face s) 'link)
            (add-text-properties b e (gp-ticket--props (match-string 0 s)) s))
          (setq i e)))
      s)))

(defun gp-linkify-string (text)
  "Return TEXT with markdown [label](url) and bare URLs turned into links.
\[label](url) is shown as LABEL; both forms get the `link' face and
a keymap opening the URL on RET/mouse-1/mouse-2 (see
`gp-ticket--keymap').  Pure -- returns a fresh string."
  (when text
    (let* ((open (lambda (url)
                   (let ((m (make-sparse-keymap))
                         (cmd (lambda () (interactive) (browse-url url))))
                     (define-key m [mouse-1] cmd)
                     (define-key m [mouse-2] cmd)
                     (define-key m (kbd "RET") cmd)
                     m)))
           (link-props (lambda (url)
                         (list 'face 'link 'mouse-face 'highlight
                               'help-echo url 'follow-link t
                               'keymap (funcall open url))))
           (s (replace-regexp-in-string
               "\\[\\([^]]+\\)\\](\\(https?://[^)]+\\))"
               (lambda (m)
                 (apply #'propertize (match-string 1 m)
                        (funcall link-props (match-string 2 m))))
               text t t)))
      (let ((i 0))
        (while (string-match "\\(https?://[^ \t\n)]+\\)" s i)
          (let ((b (match-beginning 1)) (e (match-end 1)))
            (unless (eq (get-text-property b 'face s) 'link)
              (add-text-properties b e (funcall link-props (match-string 1 s)) s))
            (setq i e))))
      s)))

(provide 'git-platform)
;;; git-platform.el ends here
