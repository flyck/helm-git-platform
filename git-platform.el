;;; git-platform.el --- Backend-agnostic PR protocol -*- lexical-binding: t; -*-

;;; Commentary:

;; A small abstraction over a code-review platform (Bitbucket today, GitHub
;; later) so the UI/overlay/helm layers talk to one protocol rather than to
;; Bitbucket directly.
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

;; Each operation is declared as a generic (`gp--NAME', dispatching on the
;; backend) plus a backend-free public wrapper (`gp-NAME') that injects the
;; active backend.  `gp-defop' generates both so the two never drift.

(defmacro gp-defop (name arglist &optional doc)
  "Define protocol operation NAME with ARGLIST (excluding the backend).
Creates the generic `gp--NAME' (backend is its first argument) and
the public `gp-NAME' wrapper that supplies `(git-platform-backend)'.
ARGLIST may contain &optional; &rest is not supported here."
  (let* ((generic (intern (format "gp--%s" name)))
         (public  (intern (format "gp-%s" name)))
         ;; strip &optional markers to build the call argument list
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
pre-signed diff link Bitbucket requires.  Async because a
synchronous diff fetch blocks Emacs for the whole round-trip.")
(gp-defop pull-request-stats-async (full-name id pr callback)
  "Fetch the stats plist for PR FULL-NAME/ID asynchronously.
CALLBACK gets the plist, or nil on error.  PR is the already-fetched
PR object, which carries the diffstat link.")
(gp-defop pull-request-commits-async (full-name id callback &optional max-items)
  "Fetch the commits on PR FULL-NAME/ID asynchronously.
CALLBACK gets a list of plists (:hash :summary :author :date), newest
first, or nil on error.  Normalised here rather than passed through
raw because the two platforms disagree on nearly every field name
\(Bitbucket `hash'/`message'/`date' under `author.user'; GitHub
`sha'/`commit.message'/`commit.author') -- the detail view renders
one shape and never branches on the backend.  Async only: this is
deferred detail-view data, and a synchronous twin would just invite
the blocking-poll bug `gp-pipeline-fetch-for-pr-async' exists to
avoid.")
(gp-defop create-comment (full-name id text &optional inline parent-id)
  "Create a comment on PR FULL-NAME/ID.")
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
forge would apply on its own.  Both come from the forge rather than a
local guess: Bitbucket permits a per-destination-branch subset of six
strategies (with its own default), GitHub a per-repository subset of
three.  Nil means the question could not be answered, in which case
callers should merge without naming a strategy and let the forge decide.")

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
pass it when the caller already has it to save a fetch.  Backends
where the update is a partial PATCH (GitHub) ignore it.")
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
means it stays visible in the PR's timeline as an explicit dismissal
event, generally with a reason attached (GitHub, which has no true
retraction API -- see `github--dismiss-own-review').  Callers should
label the retract action accordingly (e.g. \"Unapprove\" vs. \"Dismiss
approval\") and prompt for REASON only when this is `dismiss'.")
(gp-defop open-pr-for-branch (full-name branch)
  "Return the open PR in FULL-NAME whose source branch is BRANCH.")
(gp-defop repo-default-branch (full-name)
  "Return repo FULL-NAME's default (main) branch name, or nil.")
(gp-defop repo-default-reviewers (full-name)
  "Return repo FULL-NAME's default reviewers (list of user alists).
These are pre-selected in the create-PR form: the platform itself
auto-adds them as reviewers on every new PR, so opting out is the
unusual case.  Contrast `gp-repo-suggested-reviewers'.")
(gp-defop set-pull-request-reviewers (full-name id reviewer-ids &optional current-ids)
  "Set PR ID in FULL-NAME to have exactly REVIEWER-IDS as reviewers.
REVIEWER-IDS is the complete desired list (Bitbucket uuids, GitHub
logins) -- callers pass the end state, not a delta, and each backend
reaches it whichever way its API allows: Bitbucket PUTs the whole
reviewer list, GitHub POSTs the additions and DELETEs the removals
against `requested_reviewers'.  CURRENT-IDS, when given, is the PR's
present reviewer list, so the GitHub side can compute that delta
without re-fetching.

Only reviewers who have not yet submitted a review can be removed:
GitHub keeps a submitted review on the PR regardless of whether the
person is still a requested reviewer, so callers must not offer to
withdraw one (see `gp-ui-edit-reviewers').")
(gp-defop repo-suggested-reviewers (full-name)
  "Return candidate reviewers for FULL-NAME the platform merely
suggests rather than auto-selects (list of user alists, same shape
as `gp-repo-default-reviewers').  Left unchecked in the create-PR
form -- picking one is an explicit opt-in, not an opt-out.  Bitbucket
has no separate \"suggested\" concept beyond its real defaults, so
its backend always returns nil here.")
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
or nil on error.  Non-blocking twin of `gp-pipelines-for-branch':
the detail view polls this every few seconds, and a synchronous
fetch there freezes Emacs for the whole round-trip.")
(gp-defop pipeline-steps (full-name pipeline-uuid)
  "Return the steps of PIPELINE-UUID in FULL-NAME, in order.")
(gp-defop pipeline-steps-async (full-name pipeline-uuid callback)
  "Fetch the steps of PIPELINE-UUID in FULL-NAME asynchronously.
CALLBACK gets the list of steps, or nil on error.  Non-blocking twin
of `gp-pipeline-steps'; the polling detail view fans one of these out
per current-commit pipeline, so they must not block.")
(gp-defop commit-message-async (full-name hash callback)
  "Fetch the commit message for HASH in FULL-NAME asynchronously.
CALLBACK gets the message string, or nil.  Non-blocking twin of
`gp-commit-message'.  Results are cached (commit messages are
immutable), so a warm cache answers without touching the network --
but the first poll must not block on it either.")
(gp-defop pipeline-stop (full-name pipeline-uuid)
  "Stop running PIPELINE-UUID in FULL-NAME (pipeline-level).")
(gp-defop pipeline-trigger (full-name branch &optional selector variables)
  "Trigger a pipeline in FULL-NAME for BRANCH (pipeline-level).")
(gp-defop pipeline-run-manual-step (full-name branch pipeline step)
  "Run a waiting manual STEP of PIPELINE in FULL-NAME on BRANCH.")
(gp-defop pipeline-step-rerun (full-name pipeline-uuid step)
  "Re-run just STEP of PIPELINE-UUID in FULL-NAME, in place.
Distinct from `pipeline-run-manual-step': this restarts a single
already-finished (typically failed) step, not a gated step waiting
for its first run.  Only call this when `gp-pipeline-step-rerunnable-p'
is non-nil for STEP; a backend with no such capability signals a
clear `user-error' instead of silently no-oping.")
(gp-defop pipeline-web-url (full-name pipeline &optional step)
  "Return the web-UI URL for PIPELINE in FULL-NAME (deep-linked to STEP).")
(gp-defop pipeline-step-log (full-name pipeline-uuid step-uuid)
  "Return the captured log text for STEP-UUID of PIPELINE-UUID.")

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
Bitbucket nests it at `links.html.href'; GitHub has a flat `html_url'.
Reading either directly works on one forge only -- on the other it is
nil and \"open in browser\" fails with nothing to show.")

(gp-defop comment-web-url (comment)
  "Return the browser URL for COMMENT, or nil.
Same per-forge split as `pr-web-url'.")

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
Bitbucket's PR `state' is uppercase (\"OPEN\"); GitHub's is lowercase
(\"open\") -- this exists so callers never compare `state' as a raw
string against a platform-specific literal.")
(gp-defop pr-closed-reason (pr)
  "Return the reason a PR was declined/closed, or nil.
Bitbucket carries a free-text `reason' on a declined PR.  GitHub has no
such field -- a closed PR's explanation lives in a comment -- so its
backend returns nil and callers simply show no reason.")

(gp-defop pr-merged-at (pr)
  "Return when PR was merged, as an ISO-8601 string, or nil.
GitHub states it outright (`merged_at').  Bitbucket has no such field,
so its backend falls back to `updated_on' -- the last activity, which
for a merged PR is normally the merge itself but can be later if someone
comments afterwards.  Good enough to group by day, not to trust to the
second.")

(gp-defop pr-merge-commit (pr)
  "Return the commit a merged PR landed as on its destination branch, or nil.
The build worth watching after a merge runs on this commit, not on the
PR branch's last one -- that is the run that deploys.")

(gp-defop pr-merged-p (pr)
  "Return non-nil if PR was merged.
Bitbucket reports this as `state' = \"MERGED\"; GitHub as a separate
boolean `merged' field alongside `state' = \"closed\" -- so \"merged\"
and \"closed-but-not-merged\" cannot both be read off one `state'
string across backends.")
(gp-defop pr-repo-slug (pr)
  "Return PR's bare repository slug (no owner/workspace prefix), or nil.
Used for a compact list-view label; contrast `gp-pr-full-name', which
includes the owner/workspace.")
(gp-defop pr-review-tally (pr)
  "Return a plist (:approved :changes :pending) over PR's reviewers.
Bitbucket answers this from data already embedded in PR (`participants'),
so it never touches the network.  GitHub has no such embedded data --
reviews are a separate resource -- so its implementation DOES fetch,
synchronously; callers rendering many PRs at once (e.g. a list view)
should use `gp-pr-review-tally-async' instead to avoid blocking on
one HTTP round-trip per row.")
(gp-defop pr-review-tally-async (pr callback)
  "Fetch PR's review tally asynchronously; CALLBACK gets the plist.
Non-blocking twin of `gp-pr-review-tally', for callers (list views)
that can't afford a synchronous fetch per PR.  Bitbucket's
implementation has nothing to fetch, so it calls CALLBACK
immediately with the same (free) result `gp-pr-review-tally' gives.")
(gp-defop pr-reviewers-async (pr callback)
  "Fetch PR's individual reviewers asynchronously.
CALLBACK gets a list of plists (:name :avatar :state), STATE one of
`approved'/`changes'/`pending' -- the per-person breakdown behind
`gp-pr-review-tally'/-async's aggregate counts.  Bitbucket answers
from `participants' already embedded in PR (no network); GitHub
fetches the same review data `gp-pr-review-tally-async' does.")
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
COLOR is a 6-digit hex string without the leading \"#\" (GitHub's own
encoding), or nil when the platform gives no colour.  GitHub embeds
labels in the PR payload itself, so this costs no request and a
plain PR re-fetch already refreshes them.  Bitbucket Cloud has no
label concept for pull requests at all -- its implementation returns
nil, which is why callers must treat \"no labels\" and \"unsupported\"
as different questions (see `gp-labels-supported-p').")

(gp-defop pr-description (pr)
  "Return PR's description/body as a Markdown string, or nil when empty.
Bitbucket calls this `description', GitHub calls it `body'.  Returns
nil rather than \"\" for an empty description, so callers can simply
test the value to decide whether to render a section at all.")
(gp-defop comment-resolved-p (comment)
  "Return non-nil if COMMENT is resolved.")
(gp-defop comment-resolvable-p (comment)
  "Return non-nil if COMMENT supports resolve/reopen at all.
Bitbucket comments are always resolvable.  GitHub has no \"resolve\"
concept for plain issue (general discussion) comments -- only inline
review comments belong to a resolvable review thread -- so callers
should hide the resolve/reopen action entirely when this is nil
rather than let it fail on click.")
(gp-defop comment-own-p (comment uuid)
  "Return non-nil if COMMENT was written by UUID.")

(gp-defop inline-target-problem (full-name id path line)
  "Return a human explanation if PATH:LINE cannot take an inline comment.
Nil means go ahead.  Platforms differ sharply here: GitHub accepts an
inline comment only on a line inside the PR's diff and otherwise
answers a bare 422 once the comment is already written, whereas
Bitbucket takes a comment on any line of any file.  Checking up front
lets the caller refuse with something actionable and keep the user's
text.")

(gp-defop reactions-supported-p ()
  "Return non-nil if this platform has reactions on comments at all.
Like `labels-supported-p', this is about the platform's concept, not
whether a given comment happens to carry any -- the UI hides every
reaction affordance when nil rather than offering an action that can
only fail.

Nil for Bitbucket Cloud: its web UI has a single binary Like, but the
public v2.0 REST API exposes no route for it (the spec mentions no
reaction, and `/comments/{id}/likes' 404s while `/resolve' 405s), and
emoji reactions are still only a feature request (BCLOUD-21346).  True
for GitHub.")

(gp-defop reaction-choices ()
  "Return the reaction contents this platform accepts, most-liked first.
A list of strings.  GitHub returns its eight (\"+1\", \"heart\", …); a
platform with a single binary Like would return just one element, which
is why the read/write ops below speak in these opaque tokens rather than
assuming a fixed emoji set.")

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
affordance (list column, detail line, the edit key) when the
platform itself has no such concept, rather than showing an empty
slot that can never fill.  True for GitHub, nil for Bitbucket
Cloud, whose PRs carry no labels.")

(gp-defop repo-labels (full-name)
  "Return the labels defined in repo FULL-NAME, as `gp-pr-labels' plists.
The pool offered when editing a PR's labels -- the repo's whole
label set, not just the ones already applied.  Backends without
labels return nil.")

(gp-defop set-pull-request-labels (full-name id labels)
  "Set PR ID in FULL-NAME to carry exactly LABELS (a list of name strings).
Callers pass the complete desired set, not a delta, matching
`gp-set-pull-request-reviewers'.  Backends without labels signal a
`user-error' rather than silently doing nothing.")

(gp-defop backend-name ()
  "Return the symbol naming this backend (`bitbucket', `github', …).
Lets configuration key off the active platform without comparing
against the backend *instance*, which callers never construct
themselves.")

(defcustom gp-comment-delete-others nil
  "Backends on which you may delete comments written by other users.
Deleting someone else's comment needs elevated repository
permissions, which the APIs do not advertise -- so this is a
declaration, not a discovery: set it only for platforms where you
actually hold that power.  Where it does not apply, the delete
action stays hidden on other people's comments (your own are
always deletable).

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
Bitbucket keys a pipeline by `uuid'; GitHub Actions keys a workflow run
by `id'.  Everything that fetches steps, stops a run or reads a log
takes this value, so reading `uuid' directly works only against
Bitbucket -- on GitHub it is nil and the steps silently never load.")
(gp-defop pipeline-step-id (step)
  "Return the identifier STEP's own endpoints accept, or nil.
As `pipeline-id', for a step/job: Bitbucket `uuid', Actions `id'.")
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
Bitbucket has no per-step re-run (only whole-pipeline re-trigger),
so its backend always returns nil here.  GitHub Actions can re-run a
single finished job via its rerun endpoint, but only once the job
has actually finished (queued/in-progress jobs can't be rerun) --
callers should hide the \"rerun this step\" action entirely when this
is nil rather than let it fail on click.")
(gp-defop pipelines-sort (pipelines step-counts)
  "Sort PIPELINES most-steps-first (STEP-COUNTS maps uuid->count).")
(gp-defop pipelines-match-commit (pipelines commit)
  "Return the PIPELINES whose target commit matches COMMIT.")

;;;; Buffer naming --------------------------------------------------------------

;; Every buffer this package creates carries one prefix, so they can be
;; found with a single filter in `switch-to-buffer'/ibuffer and matched by
;; one `display-buffer-alist' rule instead of a pattern per buffer kind.

(defcustom gp-buffer-name-prefix "gp: "
  "Prefix tagging every buffer this package creates.
Buffer names are built as \"*PREFIX SUFFIX*\" -- e.g. \"*gp: PRs*\".
Change this to retag every buffer at once; it is read at buffer-creation
time, so existing buffers keep their old names until recreated."
  :type 'string
  :group 'bitbucket)

(defun gp--buffer-name (suffix)
  "Return the package buffer name for SUFFIX, tagged with the shared prefix.
E.g. \"PRs\" -> \"*gp: PRs*\"."
  (format "*%s%s*" gp-buffer-name-prefix suffix))

;;;; TTL result cache -----------------------------------------------------------

;; Provider-agnostic so both backends (and the UI layers that fetch
;; around the `gp-' protocol) share one cache instead of each provider
;; needing its own.  Originally lived in bitbucket-api.el as
;; `bitbucket-cache-*'; moved here when the GitHub backend was added.

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
`mine', `reviewing'); PREDICATE receives the raw key list.  Used to
invalidate a family of entries at once, e.g. every list-level cache
after a PR mutation that could change which list it belongs in."
  (let (doomed)
    (maphash (lambda (k _v) (when (funcall predicate k) (push k doomed)))
              gp--result-cache)
    (dolist (k doomed) (remhash k gp--result-cache))))

(defun gp-invalidate-pr-caches (pr)
  "Invalidate every cache entry that could now show stale data for PR.
Call this after any action that mutates PR's state on the server
\(set-draft, approve/request-changes, resolve/reopen/edit/delete a
comment, create a comment, …), alongside refreshing whichever buffer
showed the action (detail view, inline overlay, …).  Without this, a
per-buffer refresh fixes only that buffer -- the list view's
`mine'/`reviewing'/`others' caches (up to `gp-cache-ttl' seconds
stale) and this PR's own `pull-request'/`pr-stats'/`pr-diff' cache
entries keep serving pre-mutation data until they expire on their own.

Clears the list-level caches unconditionally rather than trying to
compute which uuid/state combination a mutated PR now belongs to --
those are just PR-list snapshots, cheap to recompute, and correctness
here matters more than saving one re-fetch."
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

(defun gp-cache-put (key value)
  "Cache VALUE under KEY for `gp-cache-ttl' seconds (no-op if 0)."
  (when (> gp-cache-ttl 0)
    (puthash key (cons (+ (float-time) gp-cache-ttl) value) gp--result-cache))
  value)

(defun gp-cache-with-cache (key thunk)
  "Return cached value for KEY, or call THUNK, caching for `gp-cache-ttl'."
  (let ((hit (gp-cache-get key)))
    (if (car hit)
        (cdr hit)
      (gp-cache-put key (funcall thunk)))))

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
  "Reduce build STATES to one symbol: `failed', `running', `stopped',
`successful', or nil (no builds).  Failure dominates, then running."
  (cond ((null states) nil)
        ((member "FAILED" states) 'failed)
        ((member "INPROGRESS" states) 'running)
        ((member "STOPPED" states) 'stopped)
        ((seq-every-p (lambda (s) (equal s "SUCCESSFUL")) states) 'successful)
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
             ;; hunk header: reset the new-side counter to its start line
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
Keyed by object identity (`eq') and weak on the key, so the entries
vanish once a render's DIFF-BY-FILE alist is dropped.  Avoids
re-parsing a file's hunk once per comment on that file.")

(defun gp-diff-chunk-new-lines--cached (chunk)
  "Return `gp-diff-chunk-new-lines' for CHUNK, memoised by identity."
  (or (gethash chunk gp--diff-lines-cache)
      (puthash chunk (gp-diff-chunk-new-lines chunk) gp--diff-lines-cache)))

(defvar gp--comment-outdated-cache (make-hash-table :test 'eql)
  "Comment id -> t once it has been proven outdated.
Outdatedness is monotonic: once a comment's anchor line has dropped
out of the diff, later commits can only move it further away, never
restore that exact anchor.  So a true verdict is cached permanently
(for the session); a false verdict is never cached, since the next
commit can still make the comment stale.")

(defun gp-comment-outdated-p (comment diff-by-file)
  "Return non-nil when inline COMMENT is anchored to a stale line.
DIFF-BY-FILE is the alist from `gp-split-diff-by-file'.  A comment
is outdated when its file is in the diff but its anchored new-side
line is no longer present in any hunk (it was changed away under
the comment).  Returns nil when COMMENT is not inline, when the
diff is unknown (nil), or when the comment's file is absent from
the diff -- we only flag outdated when we can prove it.

A true verdict is memoised by comment id (see
`gp--comment-outdated-cache') and short-circuits future checks even
across diff refreshes, since outdatedness never reverses."
  (let ((id (alist-get 'id comment)))
    (or (and id (gethash id gp--comment-outdated-cache))
        (let* ((path (let-alist comment .inline.path))
               (line (let-alist comment (or .inline.to .inline.from)))
               (chunk (and path diff-by-file (cdr (assoc path diff-by-file))))
               (outdated (and path line chunk
                              (not (gethash line (gp-diff-chunk-new-lines--cached chunk))))))
          (when (and outdated id)
            (puthash id t gp--comment-outdated-cache))
          outdated))))

;;;; OS notifications -----------------------------------------------------------

(defcustom gp-notify t
  "Whether this package may raise OS (desktop) notifications at all.

The master switch: nil silences every notification the package would
otherwise send, regardless of any per-feature setting.  Long-running
work -- a deploy script, a pipeline finishing -- usually outlasts the
user's attention on Emacs, so the echo area alone goes unread; but a
notification is an interruption, and some users want none.

Per-feature options (e.g. `gp-pipeline-deploy-notify') narrow this
further: a notification is sent only when BOTH this and the relevant
feature switch are non-nil."
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

A no-op unless `gp-notify' is non-nil.  Honours `gp-notify-function'
when set, otherwise falls through the facilities actually present:
D-Bus `notifications-notify' (Linux), `osascript' (macOS), and finally
the echo area -- so this is safe to call on any platform.

Notification failures are swallowed: a missing or misconfigured
notifier must never break the operation that reported through it.
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

(defun gp-linkify-string (text)
  "Return TEXT with markdown [label](url) and bare URLs turned into links.
\[label](url) is shown as LABEL; both forms get the `link' face and
a keymap opening the URL on RET/mouse-1.  Pure -- returns a fresh string."
  (when text
    (let* ((open (lambda (url)
                   (let ((m (make-sparse-keymap)))
                     (define-key m [mouse-1] (lambda () (interactive) (browse-url url)))
                     (define-key m (kbd "RET") (lambda () (interactive) (browse-url url)))
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
            (if (eq (get-text-property b 'face s) 'link)
                (setq i e)
              (add-text-properties b e (funcall link-props (match-string 1 s)) s)
              (setq i e)))))
      s)))

(provide 'git-platform)
;;; git-platform.el ends here
