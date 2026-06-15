# Development guide

Notes for hacking on `helm-git-platform` (and for AI agents — see
[`../AGENTS.md`](../AGENTS.md), which points here).

## Dev loop: edit → test → reload (no Emacs restart)

With a running Emacs server (`(server-start)` in your init), changes can be
tried live without a restart:

```sh
./run-tests.sh        # full ERT suite, fully offline (mocked Bitbucket)
./reload.sh           # hot-reload all *.el into the live Emacs
```

`reload.sh` talks to the live Emacs via `emacsclient`:

| Command | Effect |
|---|---|
| `./reload.sh` | force-load every `*.el`, re-`require` the umbrella, re-import env vars |
| `./reload.sh helm` | reload, then open `M-x gp-helm` in the live session |
| `./reload.sh watch` | reload, then toggle `gp-watch-mode` |
| `./reload.sh eval 'ELISP'` | eval arbitrary elisp in the live session (for probing) |

One gotcha it handles: `defvar`/`defvar-keymap` do **not** reassign an
already-bound variable, so a naive reload keeps stale keymaps and defcustom
defaults. `reload.sh` unbinds the package's `*-map` vars before loading so
keymap and default edits actually take effect — don't bypass it.

If `emacsclient` can't connect, fall back to `emacs --batch -Q` with
`package-initialize` and `-L .` (see `run-tests.sh`).

## Debugging via the log buffer

`gp-log.el` writes to `*gp-log*`: every API request (method, path,
status, timing), errors with response bodies, and key actions. Read the log
before speculating:

```sh
./reload.sh eval '(with-current-buffer "*gp-log*"
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
   `gp-*` functions, which dispatch to the active backend; a different forge is
   just another backend.
3. **Bitbucket backend** — `bitbucket-api.el` and `git-platform-bitbucket.el`,
   plus the `bitbucket-*` customs. These are deliberately Bitbucket-specific.

`bitbucket-env.el` (importing `BITBUCKET_*` from a shell rc file) is an opt-in
convenience and is **not** required by the umbrella.

Each file's header comment states its responsibility, with a matching
`tests/<name>-test.el`.

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
| `RET` | Open the changed file / jump to comment line / fold |
| `TAB` | Fold or unfold the section at point |
| `b` | Back to the PR list |
| `o` | Autostash & checkout the PR branch, then open the repo |
| `d` | Show the branch diff in Magit (no checkout) |
| `w` | View the PR on Bitbucket |
| `i` | Overlay this PR's inline comments onto its local files |
| `r` | Reply to the comment at point |
| `e` | Edit your own comment at point |
| `g` | Refresh (non-blocking) |
| `x` | Resolve / reopen the comment at point |
| `D` | Convert to draft / mark ready (your own PRs) |

On an inline comment **overlay** (in a checked-out file), under the `C-c b`
prefix so they don't collide with the file's own bindings:

| Key | Action |
|---|---|
| `C-c b r` | Reply to the comment at point |
| `C-c b R` | Resolve it (round-trips to the PR) |
| `C-c b k` | Reopen a resolved comment |
| `C-c b n` | New inline comment on the line at point |
| `C-c b TAB` | Minimise / expand this comment |
| `C-c b g` | Refetch and redraw overlays |

In the Markdown compose buffer: `C-c C-c` post · `C-c C-p` preview ·
`C-c C-k` cancel.

## Known limitations / TODO

- **No side-by-side diff view.** Diffs render unified. Side-by-side would
  normally come from [`git-delta`](https://github.com/dandavison/delta) via
  `--side-by-side`, but that mode is incompatible with the `--color-only` mode
  `magit-delta` relies on to overlay onto Magit's diff (see
  [magit-delta#9](https://github.com/dandavison/magit-delta/issues/9)). Until
  resolved upstream, side-by-side isn't offered.
