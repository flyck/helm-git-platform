# helm-git-platform

[![CI](https://github.com/flyck/helm-git-platform/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/flyck/helm-git-platform/actions/workflows/ci.yml)
[![Emacs](https://img.shields.io/badge/Emacs-28.1%2B-7F5AB6?logo=gnuemacs&logoColor=white)](https://www.gnu.org/software/emacs/)

A magit-flavoured **git-platform client** for Emacs. Browse pull requests across your whole
workspace, drill into changed files and comments with Helm, jump to the matching local checkout
and switch branches safely, see inline review comments as overlays on the code, and watch live PR
counts in the mode line.

## Why I built this

As AI adoption grows, models improve and code throughput increases, code reviews stay an important
part of the work. Due to these factors, I chose to build a great reviewing experience right inside
my IDE.

While performance of git platforms like github have been disappointing, and different companies
use different solutions, having one such local interface across them all should help with staying
efficient.

Even in its early stages, this local tool can already provide consistent search capabilities and
functionality across providers, which sometimes even exceeds the official UI flows.

## Install

Install straight from GitHub — no manual clone needed.

**Emacs 30+** with `use-package`'s built-in `:vc`:

```elisp
(use-package helm-git-platform
  :vc (:url "https://github.com/flyck/helm-git-platform" :rev :newest)
  :after (magit emojify)
  :commands (gp-helm gp-list gp-watch-mode)
  :bind ("C-c b" . gp-helm)
  :custom
  (gp-local-git-root "~/git")              ; where your local clones live
  (gp-open-function #'magit-status)        ; how to open a checkout
  (gp-checkout-clone-base "git@bitbucket.org:") ; auto-clone missing repos
  :config
  (gp-watch-mode 1)                        ; live PR counts + auto overlays
  (gp-magit-mode 1))                       ; PR comments in magit diffs
```

**Emacs 29 or earlier** — same form, but with [straight.el](https://github.com/radian-software/straight.el):
swap `:vc (...)` for `:straight (helm-git-platform :host github :repo "flyck/helm-git-platform")`.

**Updating** — with `:vc`, run `M-x package-vc-upgrade RET helm-git-platform RET`
to pull the latest pushed commit and rebuild it.

**Manual clone** — clone anywhere (e.g. `~/.emacs.d/lisp/helm-git-platform`)
and replace the recipe line with `:load-path "~/.emacs.d/lisp/helm-git-platform"`.

A fuller, annotated example is in [`examples/use-package.el`](examples/use-package.el).

### Dependencies

Installing the package pulls in everything it needs:

- `helm` — the `gp-helm` PR browser, the main entry point
- `magit` — PR diffs, checkouts, and comments in magit diffs (`gp-magit-mode`)
- `markdown-mode` — the Markdown compose buffer and preview
- `emojify` — `:emoji:` shortcodes and completion

### Credentials

**Bitbucket** (the default backend): set three environment variables (an API token, not your password):

```sh
export BITBUCKET_WORKSPACE="your-workspace"
export BITBUCKET_USER_EMAIL="you@example.com"
export BITBUCKET_API_TOKEN="…"   # https://id.atlassian.com/manage-profile/security/api-tokens
```

They can also be set via the `bitbucket-workspace` / `bitbucket-user-email` /
`bitbucket-api-token` customs, and the token falls back to `auth-source`.

Grant the token these scopes:

| Scope | Needed for |
|---|---|
| **Account: Read** | resolve your own identity (split mine vs needs-my-review); list workspace members as reviewer candidates |
| **Repositories: Read** | list repos, read diffs and commit messages |
| **Pull requests: Read** | list PRs and read details/comments |
| **Pipelines: Read** | show the PR's CI pipelines and step logs |
| **Pull requests: Write** | *(optional)* post / reply / resolve comments from Emacs |
| **Pipelines: Write** | *(optional)* stop / trigger / run-manual on pipelines |

Everything except the two **Write** scopes works read-only — omit them for a strictly read-only
setup (the write actions simply 403).

**GitHub**: set `git-platform-default-backend` to `'github` and provide a token via `GITHUB_TOKEN`,
`github-api-token`, or `auth-source` (host `api.github.com`). Without a token, public repos are
read-only. For a fine-grained PAT, grant:

| Permission | Access | Needed for |
|---|---|---|
| Contents | Read and write | files and diffs; pushing branches when creating a PR |
| Pull requests | Read and write | PRs, reviews, comments (resolution via GraphQL needs nothing extra) |
| Issues | Read and write | general PR comments and labels |
| Actions | Read and write | workflow runs and logs, re-running and dispatching |
| Commit statuses | Read | build-state badges |
| Metadata | Read | always required |

With a classic PAT, use the `repo` and `workflow` scopes.

> **macOS GUI Emacs** doesn't source your shell rc, so exports in `~/.zshrc`
> are invisible. The optional `bitbucket-env` helper reads them out without
> running the shell — it is **not loaded by default**; opt in with
> `(require 'bitbucket-env)` then `(bitbucket-env-load)`. See
> [`examples/use-package.el`](examples/use-package.el).

## Optional features (default on, easy to turn off)

| Feature | Turn on | Turn off |
|---|---|---|
| **Inline comment overlays** — review comments drawn on the code | on by default | `(setq gp-overlay-enabled nil)` or `M-x gp-overlay-toggle-globally` |
| **Wrapping of long comment text** — overlay text is hard-wrapped to the window | on by default | `(setq gp-overlay-wrap-width nil)`, or an integer for a fixed column |
| **Auto-overlay + mode-line counts** — per-repo PR count and comments while you visit files | `(gp-watch-mode 1)` | omit it, or `(gp-watch-mode -1)` |
| **Comments in magit diffs** | `(gp-magit-mode 1)` (also needs `gp-watch-mode`) | omit it |
| **CI pipelines in the detail view** — the PR branch's pipelines, tabbable, with stop / trigger / manual-run / logs | on by default | `(setq gp-detail-show-pipelines nil)` |
| **External deploy hook** — run your own script to advance a gated manual step | `(setq gp-pipeline-deploy-script '("~/bin/gp-deploy"))` | omit it (default) |
| **OS notifications** — desktop alert when long-running work finishes | on by default | `(setq gp-notify nil)` |
| **Review quorum** — PRs other reviewers already settled move out of the pending list | 2 approvals / 2 rejections | `(setq gp-helm-min-approvals 0)` / `gp-helm-min-rejections` |
| **New PRs start as drafts** — the create mask's "Create as draft" box starts ticked | on by default | `(setq gp-create-draft nil)`, or untick the box per PR |
| **Comment reactions** — 👍 and the rest on PR comments (`+` / `!`) | GitHub only, on by default | nothing to turn off; hidden entirely on Bitbucket, whose API has none |
| **Shell-rc env import** (macOS convenience) | `(require 'bitbucket-env)` + `(bitbucket-env-load)` | omit it (default) |
| **Send a PR comment to an AI terminal session** (iTerm2 or Ghostty) | `(setq gp-helm-terminal-backend 'iterm2)` or `'ghostty` | omit it (default) |
| **New inline comments join a review batch** — invisible to others until you submit the review | on by default | `(setq gp-review-batch-default nil)`, or `C-c C-b` per comment while composing it |
| **Recover a checkout whose branch name was reused** — offers to back up and recreate a local branch that has diverged from a same-named remote branch (typically: the old one merged, a colleague pushed a new one under the same name) instead of a raw `pull --ff-only` failure | on by default | `(setq gp-checkout-recover-diverged-branch nil)` |

The core browsing (`gp-helm`, `gp-list`, checkout) works with none of these on.

## Commands

| Command | What it does |
|---|---|
| `gp-helm` | List PRs across the workspace (needs-my-review / mine / drafts), drill into files or comments, check out, open, browse |
| `gp-list` | Same list as a magit-section detail buffer |
| `gp-helm-repo` | List open PRs in one repository |
| `gp-watch-mode` | Global: live per-repo PR count + auto comment overlays |
| `gp-magit-mode` | PR comments inside magit-diff buffers |

### Comments in a magit diff

With `(gp-magit-mode 1)` *and* `(gp-watch-mode 1)` on, any magit diff of a branch that has an open
PR shows that PR's inline comments as overlays, drawn as soon as the diff renders. Inside such a
buffer:

| Key | What it does |
|---|---|
| `C-c B n` | Add an inline PR comment on the file:line at point |
| `C-c B g` | Refetch and redraw the comments |

Both keys are no-ops outside a PR-branch diff, so ordinary magit use (status, log, rebase) is
untouched. Comments are cached for `gp-magit-comments-cache-ttl` seconds (60) so redraws on every
magit refresh stay off the network; `C-c B g` always refetches.

### Batching a review

New inline comments (`C-c B n` in a magit diff, or on the line at point in an overlay buffer) join a
pending review by default (`gp-review-batch-default`), so they stay invisible to others until you
submit. `C-c C-b` while composing flips it for that one comment. Pending comments are marked
"pending review" and can only be edited or removed. In the detail buffer, `S` submits the batch
(with an optional approve / request-changes verdict) and `C-c C-k` discards it.

GitHub submits a real PENDING review in one call. Bitbucket Cloud's public API cannot un-pend a
comment, so on submit each pending comment is reposted as a normal comment and the original
deleted. The reposted comment gets a new id and timestamp; nothing else can reference it while it's
pending, so nothing is lost.

### Running a gated deploy step

Bitbucket Cloud can't advance a single halted manual step
([BCLOUD-20050](https://jira.atlassian.com/browse/BCLOUD-20050)): `T` on a waiting gate can only
open the web UI or start a new run, which re-executes every earlier step. To press the gate in
place, point `gp-pipeline-deploy-script` at a script that can, typically browser automation on a
logged-in session:

```elisp
(setq gp-pipeline-deploy-script '("~/bin/gp-deploy"))
```

`T` then runs it asynchronously, with output in `*gp-deploy*` and the result as a notification.
The script gets its context from `GP_*` environment variables (`GP_FULL_NAME`, `GP_BRANCH`,
`GP_PIPELINE_ID`, `GP_STEP_NAME`, `GP_PR_ID`, …); unresolved values are left unset. Prefer
`GP_STEP_NAME` over `GP_STEP_UUID`, since uuids change on every re-run. See
[`docs/deploy-hook-example.sh`](docs/deploy-hook-example.sh).

**Deploy watcher.** `A` on a manual step arms a watcher that polls in the background and fires the
step once its gate is open, pressing any earlier gates on the way (arm `deploy-live` and it presses
`deploy-dev` first). It stops and names the step if one ahead of the target fails, and notifies on
every outcome (🟢 triggered, 🔴 blocked). Watchers are global and in-memory only: `A` again disarms,
`C-c A` lists them (`RET` log, `k` cancel, `C` clear finished). Without a deploy script, firing falls
back to re-triggering the whole pipeline. Tunables: `gp-deploy-watch-interval`,
`gp-deploy-watch-timeout`, `gp-deploy-watch-confirm`, `gp-deploy-watch-log-max`.

Notifications go through `gp-notify` (nil silences everything; `gp-pipeline-deploy-notify` narrows
deploy results), routed by `gp-notify-function`.

### The PR detail buffer

Actions show their key in `[brackets]` and buttons are clickable. Comments are Markdown, posted with
`C-c C-c`. Actions that write to the PR sit on **capital** letters (`R` reply, `X` resolve, `K`
delete, `V` reviewers, `L` labels), so a stray lowercase key can't mutate anything.

- **Reviewers** (`V`): checkboxes from workspace members (Bitbucket) or collaborators (GitHub).
  Anyone who has already reviewed is locked, since removing them can't withdraw the review.
- **Labels** (`L`, GitHub only): shown in GitHub's colours in the picker, list and header. Tunables:
  `gp-helm-labels-width` (0 hides the column), `gp-label-colors`.
- **CI pipelines**: finished ones start collapsed (`TAB` expands). `s` stops, `T` triggers or starts
  a manual step, `P` re-runs one step where supported, `l` opens a step's log (polled while
  running). Stop/trigger are whole-pipeline only.
- **Commits**: `RET` or `v` opens one in Magit. Tunables: `gp-detail-max-commits` (50),
  `gp-detail-commits-collapsed`.

In the helm picker the title column takes all spare width; lower `gp-helm-repo-width` (38) to give
it more. Every buffer is named `*gp: …*` (`gp-buffer-name-prefix`) so one filter finds them all.

## Extensibility

It talks to a forge through a backend protocol (`git-platform`).  **Bitbucket Cloud and GitHub are
both implemented**; the UI, overlays, checkout service and Helm front-end are all
platform-agnostic — adding another forge (GitLab, …) is a matter of writing one backend.

> Nothing is hardcoded to a workspace or host — every value is a `defcustom`, and credentials come
> from the environment or `auth-source`.

The one thing GitHub's API genuinely cannot do, at all, regardless of implementation: a queryable
repo-level "default reviewers" list (closest is CODEOWNERS, which isn't one).

Everything else — comment resolution, withdrawing a review, converting a PR back to draft,
re-running a CI step — works, just routed through whatever GitHub API actually supports it
(REST where it can, GraphQL where REST has no equivalent — see `github-api.el`'s Commentary for
specifics), with the UI adapting itself to what's available rather than guessing.

## Limitations

- **Bitbucket Cloud and GitHub are supported today**; the code sits behind a backend protocol
  (`git-platform`) so another forge (GitLab, …) could be added the same way. GitHub has a handful
  of documented gaps relative to Bitbucket (see [Extensibility](#extensibility) above) stemming
  from real product/API differences, not missing implementation effort.
- **Bitbucket's merge-conflict warning is derived, not authoritative.** Bitbucket Cloud's PR payload
  has no mergeability field, and its dedicated conflicts endpoint does not accept the Atlassian
  API-token auth this package uses (see `todo.md` for the confirmed details). The warning is instead
  inferred from the per-file `status` in the PR's diffstat (a `"merge conflict"` entry), which is
  reachable with this package's credentials but not independently verified against a live conflicted
  PR -- treat it as a best-effort hint rather than as reliable as GitHub's own "Cannot merge" state.

## Tests

```sh
./scripts/run-tests.sh   # ERT suite, fully offline (Bitbucket is mocked)
```

## More

- [`docs/DEVELOPMENT.md`](docs/DEVELOPMENT.md) — dev loop, testing approach, architecture, the
  API-spec drift check, key bindings reference, and known limitations.
