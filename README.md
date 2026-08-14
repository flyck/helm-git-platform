# helm-git-platform

A magit-flavoured **git-platform client** for Emacs. Browse pull requests across your whole
workspace, drill into changed files and comments with Helm, jump to the matching local checkout
and switch branches safely, see inline review comments as overlays on the code, and watch live PR
counts in the mode line.

## Why I build this

- Managing pull requests (review and feedback) right in my IDE allows me to work on them
  faster. Less context switches and less clicking.
- The helm interface makes for great search, sometimes exceeding the original search capability of
  the official UIs (bitbucket, github, etc.)
- The underlying git-platform becomes exchangeable. My workflow needs to be great independent
  while the underlying system stays replacable. Especially true when the employer picks bitbucket.

## Extensibility

It talks to a forge through a backend protocol (`git-platform`).  **Bitbucket Cloud and GitHub are
both implemented**; the UI, overlays, checkout service and Helm front-end are all
platform-agnostic — adding another forge (GitLab, …) is a matter of writing one backend.

> Nothing is hardcoded to a workspace or host — every value is a `defcustom`, and credentials come
> from the environment or `auth-source`.

Set `git-platform-default-backend` to `'github` to talk to GitHub instead of Bitbucket, and
configure a token via `github-api-token`, the `GITHUB_TOKEN` environment variable, or an
`auth-source` entry for host `api.github.com`.  A token is optional for read-only access to public
repos (GitHub's unauthenticated rate limit applies); write operations require one.

**Token permissions.** For a fine-grained PAT scoped to the repo(s) you want to use:

| Permission | Access | Why |
|---|---|---|
| Contents | Read and write | Reading files/diffs; pushing branches when creating a PR |
| Pull requests | Read and write | List/view/create PRs, reviews, approve/request-changes, draft toggle |
| Issues | Read and write | General (non-inline) PR comments go through the Issues API |
| Actions | Read and write | Workflow runs/jobs/logs, re-running a job, dispatching a workflow |
| Commit statuses | Read | Combined commit status for build-state badges |
| Metadata | Read | Mandatory default, always required |

Comment resolution goes through GraphQL rather than REST, but needs no separate scope — GraphQL
mutations are gated by the same underlying permission (Pull requests: write).

With a classic PAT, the closest equivalent is the `repo` scope plus `workflow` (the latter needed
specifically for dispatching/re-running Actions runs).

The one thing GitHub's API genuinely cannot do, at all, regardless of implementation: a queryable
repo-level "default reviewers" list (closest is CODEOWNERS, which isn't one).

Everything else — comment resolution, withdrawing a review, converting a PR back to draft,
re-running a CI step — works, just routed through whatever GitHub API actually supports it
(REST where it can, GraphQL where REST has no equivalent — see `github-api.el`'s Commentary for
specifics), with the UI adapting itself to what's available rather than guessing.

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

| Package | Needed for | Pulled automatically? |
|---|---|---|
| `magit-section`, `transient` | core rendering | yes (declared in `Package-Requires`) |
| `helm` | the `gp-helm` browser — the main entry point | **no, install it yourself** |
| `magit` (full) | the `d` diff, "open in IDE", `gp-magit-mode` | no |
| `markdown-mode` | the Markdown compose buffer + preview | no |
| `emojify` | `:emoji:` shortcodes and completion | no |

Only `magit-section` and `transient` are pulled in by `:vc`/`:straight`. The rest are soft
dependencies — install the ones you want with `M-x package-install` (most setups already have
`magit` and `helm`).

> The example config uses `:after (magit emojify)`, which means use-package
> won't load the package until **both** are present. Drop names you don't
> install from that list, or remove `:after` entirely, otherwise the package
> silently never loads. Without `helm`, `gp-helm` (and the `C-c b` binding)
> won't work — use `gp-list` instead, or install helm.

### Credentials

Set three environment variables (an API token, not your password):

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

> **macOS GUI Emacs** doesn't source your shell rc, so exports in `~/.zshrc`
> are invisible. The optional `bitbucket-env` helper reads them out without
> running the shell — it is **not loaded by default**; opt in with
> `(require 'bitbucket-env)` then `(bitbucket-env-load)`. See
> [`examples/use-package.el`](examples/use-package.el).

## Optional features (default on, easy to turn off)

| Feature | Turn on | Turn off |
|---|---|---|
| **Inline comment overlays** — review comments drawn on the code | on by default | `(setq gp-overlay-enabled nil)` or `M-x gp-overlay-toggle-globally` |
| **Auto-overlay + mode-line counts** — per-repo PR count and comments while you visit files | `(gp-watch-mode 1)` | omit it, or `(gp-watch-mode -1)` |
| **Comments in magit diffs** | `(gp-magit-mode 1)` | omit it |
| **CI pipelines in the detail view** — the PR branch's pipelines, tabbable, with stop / trigger / manual-run / logs | on by default | `(setq gp-detail-show-pipelines nil)` |
| **Shell-rc env import** (macOS convenience) | `(require 'bitbucket-env)` + `(bitbucket-env-load)` | omit it (default) |
| **Send a PR comment to an AI terminal session** (iTerm2 or Ghostty) | `(setq gp-helm-terminal-backend 'iterm2)` or `'ghostty` | omit it (default) |

The core browsing (`gp-helm`, `gp-list`, checkout) works with none of these on.

## Commands

| Command | What it does |
|---|---|
| `gp-helm` | List PRs across the workspace (needs-my-review / mine / drafts), drill into files or comments, check out, open, browse |
| `gp-list` | Same list as a magit-section detail buffer |
| `gp-helm-repo` | List open PRs in one repository |
| `gp-watch-mode` | Global: live per-repo PR count + auto comment overlays |
| `gp-magit-mode` | PR comments inside magit-diff buffers |

In the detail buffer and on overlays, most actions show their key in `[brackets]` and the buttons
are clickable (reply, resolve, new comment, open, diff). Comments are written in Markdown with
`C-c C-c` to post. Actions that write to the PR sit on **capital** letters (`R` reply, `X` resolve,
`K` delete, `V` edit reviewers), so a stray lowercase keypress while reading can't mutate anything.

**Reviewers** can be picked as checkboxes both when creating a PR and afterwards (`V` on an open
PR). Candidates come from the workspace members on Bitbucket and the repo collaborators on GitHub;
anyone who has already submitted a review is shown locked, since dropping them from the list cannot
withdraw a review that is already on the record.

Every buffer the package opens is tagged `*gp: …*` (`*gp: PRs*`, `*gp: PR #101 …*`,
`*gp: reviewers #101*`, `*gp: log*`, …) so one filter finds them all; retag with
`gp-buffer-name-prefix`.

The detail buffer also shows the PR branch's **CI pipelines** (the one with the most steps on top;
finished pipelines start collapsed, `TAB` expands). On a pipeline or step: `s` stops the running
pipeline, `T` triggers/re-runs it (and starts a waiting *manual* step), `P` re-runs a single
finished step where the platform supports it, and `l` opens a step's log in a buffer (tailed live
while it runs, historical once finished).

> The platform allows stop and trigger only at the **whole-pipeline** level —
> there is no per-step stop/trigger API — and step logs are fetched, not
> streamed (so "live" means polled). Requires a token with **Pipelines: Read**,
> plus **Pipelines: Write** for stop/trigger/manual-run.

## Limitations

- **Bitbucket Cloud and GitHub are supported today**; the code sits behind a backend protocol
  (`git-platform`) so another forge (GitLab, …) could be added the same way. GitHub has a handful
  of documented gaps relative to Bitbucket (see [Extensibility](#extensibility) above) stemming
  from real product/API differences, not missing implementation effort.
- **No side-by-side diff view.** Diffs render unified (both the inline changed-file diffs and
  Magit's `d`). See [`docs/DEVELOPMENT.md`](docs/DEVELOPMENT.md) for why.

## Tests

```sh
./run-tests.sh        # ERT suite, fully offline (Bitbucket is mocked)
```

## More

- [`docs/DEVELOPMENT.md`](docs/DEVELOPMENT.md) — dev loop, testing approach, architecture, the
  API-spec drift check, key bindings reference, and known limitations.
