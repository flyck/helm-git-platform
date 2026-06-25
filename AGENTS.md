# AGENTS.md — helm-git-platform

Guidance for AI agents (and humans) working on this Emacs package.

The full development guide lives in [`docs/DEVELOPMENT.md`](docs/DEVELOPMENT.md):
the edit → test → reload loop, the testing discipline, conventions,
architecture, and the API-spec drift check. **Read it first.**

The short version:

- **Default to the live loop.** Edit → `./run-tests.sh` → `./reload.sh` →
  optionally `./reload.sh eval '...'` to confirm in the running Emacs. Don't
  ask the user to restart Emacs.
- **Reloading is not installing.** The user runs an *installed copy* at
  `~/.emacs.d/elpa/helm-git-platform` (its own git checkout of this same
  remote, with byte-compiled `.elc` files). Reloading `.el` from this working
  tree only patches the running session — it does **not** update the installed
  copy, so the change is lost on the next Emacs restart. When the user wants
  the latest version "installed" / "in my `~/.emacs.d`", you must also sync
  that copy: `cd ~/.emacs.d/elpa/helm-git-platform && git fetch <this-repo>
  main && git reset --hard <commit>`, **preserving** the untracked
  package-manager files (`*-autoloads.el`, `*-pkg.el`), then byte-recompile
  (`emacsclient -e '(byte-recompile-directory "<that-dir>/" 0 t)'`) so no stale
  `.elc` shadows the new source.
- **Every new behaviour ships with an ERT test in the same change.** The suite
  is fully offline (Bitbucket is mocked); a green run precedes every reload.
- **One network choke-point** (`bitbucket-api-request`); **pure core, impure
  shell**; **no hardcoded workspace/host** — everything is a `defcustom` with a
  Bitbucket-Cloud default, credentials resolve customs → env → `auth-source`.
- **Byte-compile clean** before declaring done.
- **Read the `*gp-log*` buffer** before guessing at a reported bug.

## Layout

`helm-git-platform.el` is the umbrella / `use-package` entry point. The generic
`gp-*` / `git-platform-*` layer is backend-free; the `bitbucket-*` files are the
Bitbucket backend. Each file's header comment states its responsibility, with a
matching `tests/<name>-test.el`. See `README.md` for the user-facing overview.
