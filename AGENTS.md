# AGENTS.md — helm-git-platform

Guidance for AI agents (and humans) working on this Emacs package.

The full development guide lives in [`docs/DEVELOPMENT.md`](docs/DEVELOPMENT.md):
the edit → test → reload loop, the testing discipline, conventions,
architecture, and the API-spec drift check. **Read it first.**

The short version:

- **Default to the live loop.** Edit → `./run-tests.sh` → `./reload.sh` →
  optionally `./reload.sh eval '...'` to confirm in the running Emacs. Don't
  ask the user to restart Emacs.
- **Every new behaviour ships with an ERT test in the same change.** The suite
  is fully offline (Bitbucket is mocked); a green run precedes every reload.
- **One network choke-point** (`bitbucket-api-request`); **pure core, impure
  shell**; **no hardcoded workspace/host** — everything is a `defcustom` with a
  Bitbucket-Cloud default, credentials resolve customs → env → `auth-source`.
- **Byte-compile clean** before declaring done.
- **Read the `*bitbucket-log*` buffer** before guessing at a reported bug.

## Layout

`helm-git-platform.el` is the umbrella / `use-package` entry point. The generic
`gp-*` / `git-platform-*` layer is backend-free; the `bitbucket-*` files are the
Bitbucket backend. Each file's header comment states its responsibility, with a
matching `tests/<name>-test.el`. See `README.md` for the user-facing overview.
