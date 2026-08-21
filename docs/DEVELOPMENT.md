# Development guide

Notes for hacking on `helm-git-platform` (and for AI agents — see
[`../AGENTS.md`](../AGENTS.md), which points here).

## Dev loop: edit → test → reload (no Emacs restart)

With a running Emacs server (`(server-start)` in your init), changes can be
tried live without a restart:

```sh
./scripts/run-tests.sh   # full ERT suite, fully offline (mocked Bitbucket)
./scripts/reload.sh      # hot-reload all *.el into the live Emacs
./scripts/install.sh     # promote pushed HEAD into ~/.emacs.d (survives restart)
```

`scripts/reload.sh` talks to the live Emacs via `emacsclient`:

| Command | Effect |
|---|---|
| `./scripts/reload.sh` | force-load every `*.el`, re-`require` the umbrella, re-import env vars |
| `./scripts/reload.sh helm` | reload, then open `M-x gp-helm` in the live session |
| `./scripts/reload.sh watch` | reload, then toggle `gp-watch-mode` |
| `./scripts/reload.sh eval 'ELISP'` | eval arbitrary elisp in the live session (for probing) |

`scripts/install.sh` is the *promotion* step that comes after that loop. A reload only
patches the running session, so the change dies with it; `./scripts/install.sh` moves the
long-lived install at `~/.emacs.d/elpa/helm-git-platform` forward via
`package-vc-upgrade`, then reloads so both copies agree. It gates on a clean tree,
an in-sync upstream (`package-vc-upgrade` only sees **pushed** commits) and a green
suite, then verifies the installed revision really matches local `HEAD` rather than
assuming it. `--skip-tests` bypasses the ERT gate for a quick re-install.

One gotcha it handles: `defvar`/`defvar-keymap` do **not** reassign an
already-bound variable, so a naive reload keeps stale keymaps and defcustom
defaults. `scripts/reload.sh` unbinds the package's `*-map` vars before loading so
keymap and default edits actually take effect — don't bypass it.

If `emacsclient` can't connect, fall back to `emacs --batch -Q` with
`package-initialize` and `-L .` (see `scripts/run-tests.sh`).

## Debugging via the log buffer

`gp-log.el` writes to `*gp-log*`: every API request (method, path,
status, timing), errors with response bodies, and key actions. Read the log
before speculating:

```sh
./scripts/reload.sh eval '(with-current-buffer "*gp-log*"
  (buffer-substring (max (point-min) (- (point-max) 4000)) (point-max)))'
```

Keep new network/IO paths logging through `gp-log` so this stays true.

## Testing approach

- **Every new behaviour ships with an ERT test in the same change.**
- **The centralized mock makes this cheap.** All network goes through
  `bitbucket-api-request`, which `tests/bitbucket-mock.el` replaces with
  fixtures from `tests/fixtures/` (captured from the real API, then scrubbed of
  any private data). End-to-end-ish flows run offline and deterministically.
- **Test the pure core directly.** Parsing, formatting, partitioning,
  threading, emoji/markdown resolution, cache TTLs, command-plan builders — all
  pure functions with focused tests.
- **Byte-compile clean.** Resolve warnings (use `declare-function` / `defvar`
  for genuine forward references) before declaring done.

The suite is fast (~1s of real work) and fully offline.

## Conventions

- **One network choke-point.** All HTTP goes through `bitbucket-api-request`
  in `bitbucket-api.el`. Never add a second path to the network.
- **Pure core, impure shell.** Keep parsing/formatting/decision logic in pure
  functions; keep `helm`, overlays, buffers and git calls in thin wrappers.
- **No hardcoded workspace/host.** Everything is a `defcustom` with a
  Bitbucket-Cloud default; credentials resolve from customs → env vars →
  `auth-source`. Don't bake in a workspace name or `api.bitbucket.org`.
- **Read vs write scopes.** Browsing/overlays/checkout/pipeline-viewing need
  only read scopes (incl. Pipelines:Read); posting/resolving comments needs
  Pull-requests:Write, and pipeline stop/trigger/manual-run needs
  Pipelines:Write. Keep that split honest in code and docs.

## Architecture

Three layers:

1. **Umbrella** — `helm-git-platform.el`, the `use-package` entry point. It
   `require`s the components and wires up the cross-cutting bits. It also
   `(provide 'bitbucket)` for backward compatibility with the package's old
   name.
2. **Generic core** — the `git-platform` protocol (`git-platform.el`) and the
   backend-free `gp-*` UI/overlay/helm/checkout layers. Consumers call
   `gp-*` functions, which dispatch to the active backend (set via
   `git-platform-default-backend`); a different forge is just another backend.
   The TTL result cache (`gp-cache-*`) also lives here, shared by every backend.
3. **Bitbucket backend** — `bitbucket-api.el` and `git-platform-bitbucket.el`,
   plus the `bitbucket-*` customs. These are deliberately Bitbucket-specific.
4. **GitHub backend** — `github-api.el` and `git-platform-github.el`, plus the
   `github-*` customs (`github-api-token`/`GITHUB_TOKEN`). GitHub's REST v3
   API alone can't do everything Bitbucket's can (no native comment-thread
   resolution, no review retraction, no draft-state toggle in either
   direction, no per-job manual pipeline step) but GraphQL covers comment
   resolution and both draft-toggle directions; only review retraction, a
   true default-reviewers list, and per-job manual-step triggering remain
   real gaps — see the Commentary block at the top of `github-api.el` for
   the full list and the reasoning behind each workaround (a clear
   `user-error` or a documented nil where nothing can substitute).
   CI maps to GitHub Actions workflow runs (`/actions/runs`), with a run's
   jobs standing in for Bitbucket's pipeline "steps".

`bitbucket-env.el` (importing `BITBUCKET_*` from a shell rc file) is an opt-in
convenience and is **not** required by the umbrella.

Each file's header comment states its responsibility, with a matching
`tests/<name>-test.el`.

## Buffer names

Every buffer the package creates is tagged with one prefix, so a single filter
in `switch-to-buffer`/ibuffer finds them all and one `display-buffer-alist`
rule can match them:

```
*gp: PRs*                        the PR list
*gp: PR #101 add gift cards (webshop)*   a PR detail buffer
*gp: create PR*                  the create form
*gp: reviewers #101*             the reviewer editor
*gp: comment*  *gp: comment preview*
*gp: pipeline #42 log: Deploy*
*gp: helm*  *gp: helm files*  *gp: helm comments*  …
*gp: log*                        the diagnostic log
```

Names are built by `gp--buffer-name`, whose prefix is the `gp-buffer-name-prefix`
defcustom (default `"gp: "`) — set it to retag everything at once. Existing
buffers keep their old names until recreated. `gp-log.el` spells the tag
literally instead of calling the helper: it is a leaf that `git-platform`
requires, so using the helper there would create a load cycle.

## API spec drift check

`tests/gp-api-drift-test.el` is the one test that needs the network (it
**skips** when offline). It fetches Bitbucket's OpenAPI spec
(`https://api.bitbucket.org/swagger.json`, cached in `/tmp` for a day) and
asserts that every endpoint + method the package calls still exists. Run it
deliberately or on a schedule to learn when Bitbucket changes an endpoint:

```sh
emacs --batch -Q -L . -L tests -l tests/gp-api-drift-test.el \
  --eval '(ert-run-tests-batch-and-exit "drift")'
```

## Key bindings

In the PR **detail** buffer (read-only actions lowercase; state-changing
actions uppercase; buttons are clickable):

| Key | Action |
|---|---|
| `RET` | Open the changed file / show the commit / jump to comment line / fold |
| `TAB` | Fold or unfold the section at point |
| `b` | Back to the PR list |
| `o` | Autostash & checkout the PR branch, then open the repo |
| `d` | Show the branch diff in Magit (no checkout) |
| `v` | Show just the commit at point in Magit (single-commit revision buffer) |
| `w` | View the PR in the browser |
| `i` | Overlay this PR's inline comments onto its local files |
| `e` | Edit your own comment at point |
| `g` | Refresh (non-blocking) |
| `D` | Convert to draft / mark ready (your own PRs) |
| `s` `T` `m` `l` | Pipeline: stop · trigger/run-manual · toggle mark · step log |
| `P` | Re-run the finished pipeline step at point (GitHub Actions only) |

Comment actions that **write** to the PR sit on capital letters, so a stray
lowercase keypress while reading can't mutate anything:

| Key | Action |
|---|---|
| `R` | Reply to the comment at point |
| `X` | Resolve / reopen the comment at point |
| `K` | Delete the comment at point (see below) |
| `V` | Add / remove reviewers on this PR |
| `E` | Edit this PR's description (opens a compose buffer; empty clears it) |

`K` offers itself on your own comments always. Deleting *other* people's
comments needs elevated repository permissions that the APIs don't advertise,
so it is opt-in via `gp-comment-delete-others` — nil (default), `t`, or a list
of backend symbols:

```elisp
(setq gp-comment-delete-others '(bitbucket))
```

Where it doesn't apply, the delete action stays hidden rather than failing on
click. Editing is always own-only regardless: no API lets you rewrite someone
else's text.

On an inline comment **overlay** (in a checked-out file), under the `C-c B`
prefix so they don't collide with the file's own bindings:

| Key | Action |
|---|---|
| `C-c B R` | Reply to the comment at point |
| `C-c B X` | Resolve it (round-trips to the PR) |
| `C-c B k` | Reopen a resolved comment |
| `C-c B K` | Delete the comment at point |
| `C-c B n` | New inline comment on the line at point |
| `C-c B TAB` | Minimise / expand this comment |
| `C-c B g` | Refetch and redraw overlays |
| `C-c B ]` `C-c B [` | Next / previous commented line |

In the Markdown compose buffer: `C-c C-c` post · `C-c C-p` preview ·
`C-c C-k` cancel.

## Picking reviewers

### On a new PR

The create form (`gp-create-pr`) lists reviewer candidates as checkboxes, in
two groups — `SPC` toggles one, `C-c C-c` (or `C`) submits:

- **Default reviewers**, pre-checked, because the platform would add them
  anyway; unchecking is the unusual case. Bitbucket's repo-level
  default-reviewers list; GitHub has no queryable equivalent, so this group is
  empty there.
- **Suggested**, unchecked, an explicit opt-in. Bitbucket lists the
  **workspace members**; GitHub lists the **repo collaborators**. Yourself and
  anyone already in the defaults are filtered out, so no name appears twice.

Both backends therefore offer a real candidate pool. The selected ids reach
`gp-create-pull-request`'s `reviewer-uuids` argument, and each backend sends
them the way its API wants: Bitbucket inline in the create POST
(`reviewers: [{uuid}]`), GitHub as a follow-up
`POST .../requested_reviewers` with plain logins.

Candidate lookups are synchronous at form-build time and cached, so opening the
form is one extra round-trip per workspace, not per PR.

### On an existing PR

`V` in the detail buffer (or the `✎ edit [V]` button on the 👥 reviewers line)
opens `gp-reviewers-edit` — the same checkbox idiom, from the same candidate
pool, in three groups: **Current** (pre-checked), the repo's **Default
reviewers** not yet on the PR, and **Suggested**. The line and its button show
even when the PR has no reviewers yet, since that is when you need a way in;
they are hidden on a closed or merged PR, which no platform lets you mutate.

Anyone who has already **submitted a review** is shown with their badge, ticked
and *locked*:

```
Current
[X] Alice Meyer   ✓ approved  (locked)
[X] Bob Tanaka   ⏳ pending
```

A submitted review stays attached to the PR whether or not the person is still
a requested reviewer, so dropping them from the list cannot withdraw it —
attempting to untick says so. The lock is enforced in
`gp-reviewers--selected-ids`, not just in the keymap, because a checkbox can
also be toggled by mouse, `RET`, or `widget-value-set`.

Saving hands the **complete desired list** to `gp-set-pull-request-reviewers`;
each backend reaches that end state the way its API allows:

| | How reviewers are set |
|---|---|
| Bitbucket | whole-list `PUT` on the PR. Sends `title` too (a `PUT` omitting it blanks the title) and builds the array as a *vector*, since `json-encode` renders the empty list as `null` rather than the `[]` needed to clear everyone. |
| GitHub | `POST`/`DELETE` deltas on `requested_reviewers`, diffed against the current list so existing reviewers are never re-notified. |

An unchanged selection short-circuits without any API call.
