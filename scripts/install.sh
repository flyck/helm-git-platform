#!/usr/bin/env bash
# Promote the current (pushed) HEAD into the long-lived Emacs install at
# ~/.emacs.d/elpa/helm-git-platform, then reload the live session so both
# copies agree.
#
#   ./scripts/install.sh              # test -> install -> reload -> verify
#   ./scripts/install.sh --skip-tests # same, minus the ERT gate (quick re-install)
#
# Why this exists: `scripts/reload.sh` only patches the *running* Emacs from this
# working tree -- the change is gone on the next restart.  The installed copy
# is its own `package-vc' git checkout of the same remote, and the sanctioned
# way to move it is `package-vc-upgrade' (it fetches, resets the checkout and
# byte-recompiles in one step).  Never rsync or hand-edit files in there: that
# fights package.el and clobbers the untracked *-autoloads.el / *-pkg.el files
# it owns.
#
# This script never commits, pushes, stashes or resets anything.  If a
# precondition fails it says what to do and exits non-zero -- the worst
# outcome is "it refused", never "it moved my branch".
#
# Requires a running `M-x server-start' Emacs.
set -euo pipefail
SCRIPTS="$(cd "$(dirname "$0")" && pwd)"
# Git commands, run-tests.sh and the package itself are all relative to the
# repo root, which is one level up now that this script sits in scripts/.
DIR="$(cd "$SCRIPTS/.." && pwd)"
cd "$DIR"

PKG="helm-git-platform"
ELPA_DIR="${HOME}/.emacs.d/elpa/${PKG}"

RUN_TESTS=1

usage() {
  cat <<EOF
usage: ./scripts/install.sh [--skip-tests|-n] [--help|-h]

Test, install into ${ELPA_DIR}, reload the live Emacs, and verify.

  -n, --skip-tests   skip the ERT suite (scripts/run-tests.sh) gate
  -h, --help         show this help
EOF
}

while [ $# -gt 0 ]; do
  case "$1" in
    -n|--skip-tests) RUN_TESTS=0; shift ;;
    -h|--help)       usage; exit 0 ;;
    *)               echo "error: unknown argument: $1" >&2; echo >&2; usage >&2; exit 1 ;;
  esac
done

say()  { printf '\n==> %s\n' "$*"; }
warn() { printf 'warning: %s\n' "$*" >&2; }
die()  { printf 'error: %s\n' "$*" >&2; exit 1; }

ec() { emacsclient --eval "$1"; }

# --- Step 1: preflight -------------------------------------------------------

say "Preflight"

# A live server is needed both to drive package-vc-upgrade and to reload.
emacsclient --eval t >/dev/null 2>&1 \
  || die "cannot reach the Emacs server.
  Start one with M-x server-start (or add (server-start) to your init)."

[ -d "$ELPA_DIR/.git" ] \
  || die "$ELPA_DIR is not a git checkout.
  Expected a package-vc install.  Install it first with the use-package :vc
  recipe from README.md, then re-run this script."

# Only *tracked* modifications matter: .elc files are gitignored build output
# and .claude/ is a live directory of git worktrees in normal use.  Untracked
# entries are reported but don't block.
git diff --quiet \
  || die "working tree has unstaged changes; what you test wouldn't be what
  you install.  Commit or stash them first (git status)."
git diff --cached --quiet \
  || die "working tree has staged-but-uncommitted changes.
  Commit or stash them first (git status)."

# .claude/ holds agent scaffolding and git worktrees; it is always there and
# never part of the package, so don't nag about it.
untracked="$(git ls-files --others --exclude-standard -- . ':!.claude/')"
if [ -n "$untracked" ]; then
  warn "untracked files present -- these are NOT installed, since
  package-vc-upgrade pulls from the remote:"
  printf '%s\n' "$untracked" | sed 's/^/    /' >&2
fi

# package-vc-upgrade fetches from the *remote*, so unpushed commits are
# invisible to it.  This is the gate that matters most.
upstream="$(git rev-parse --abbrev-ref --symbolic-full-name '@{u}' 2>/dev/null)" \
  || die "the current branch has no upstream, so there is nothing for
  package-vc-upgrade to fetch.  Set one with:
      git push -u origin $(git rev-parse --abbrev-ref HEAD)"

echo "  branch:   $(git rev-parse --abbrev-ref HEAD) -> ${upstream}"
echo "  fetching ${upstream%%/*} ..."
git fetch --quiet "${upstream%%/*}" || die "git fetch failed."

read -r behind ahead <<<"$(git rev-list --left-right --count "${upstream}...HEAD")"

if [ "$ahead" -gt 0 ]; then
  die "${ahead} unpushed commit(s) on this branch.
  package-vc-upgrade only sees what has been pushed, so installing now would
  silently leave them out.  Run:
      git push"
fi
if [ "$behind" -gt 0 ]; then
  warn "${upstream} is ${behind} commit(s) ahead of your local HEAD; the
  install will include remote work you don't have locally."
fi

LOCAL_HEAD="$(git rev-parse --short HEAD)"
echo "  local HEAD is ${LOCAL_HEAD} (in sync with ${upstream})"

# --- Step 2: test gate -------------------------------------------------------

if [ "$RUN_TESTS" -eq 1 ]; then
  say "Running the ERT suite"
  # Note: run-tests.sh calls package-initialize, so the *installed* copy is on
  # load-path alongside the explicit -L . sources.  The explicit -l ordering
  # wins in practice; for a genuinely hermetic run use `eldev -p -dtT -C test'
  # (see scripts/run-ci-locally.sh).
  "$SCRIPTS/run-tests.sh" || die "tests failed; not installing.
  Fix them, or re-run with --skip-tests if you know what you're doing."
else
  say "Skipping tests (--skip-tests)"
fi

# --- Step 3: install ---------------------------------------------------------

say "Installing into ${ELPA_DIR}"

BEFORE="$(git -C "$ELPA_DIR" rev-parse --short HEAD 2>/dev/null || echo unknown)"
echo "  installed revision before: ${BEFORE}"

# package-vc-upgrade fetches, hard-resets the checkout and byte-recompiles.
if ! out="$(ec "(let ((pkg (cadr (assq '${PKG} package-alist))))
                  (if pkg
                      (progn (package-vc-upgrade pkg) \"upgraded\")
                    (error \"${PKG} is not installed via package-vc\")))" 2>&1)"; then
  printf '%s\n' "$out" >&2
  die "package-vc-upgrade failed.  The installed copy may be unchanged; check
  ${ELPA_DIR} and the *Messages* buffer in Emacs."
fi
# "already at latest" is a success, not an error -- no special-casing needed.
printf '%s\n' "$out" | sed 's/^/  /'

# --- Step 4: reload the live session ----------------------------------------

# Without this the running Emacs keeps serving the pre-upgrade .elc until the
# next restart, so the installed and running copies would disagree.  Delegate
# to reload.sh, which already handles the defvar/keymap-unbinding gotcha.
say "Reloading the live session"
"$SCRIPTS/reload.sh" >/dev/null || warn "reload.sh failed; the install itself is
  fine, but the running Emacs still has the old code until you restart it."

# --- Step 5: verify and report ----------------------------------------------

say "Verifying"

AFTER="$(git -C "$ELPA_DIR" rev-parse --short HEAD 2>/dev/null || echo unknown)"

if [ "$BEFORE" = "$AFTER" ]; then
  echo "  installed revision: ${AFTER} (already up to date)"
else
  echo "  installed revision: ${BEFORE} -> ${AFTER}"
fi

status=0
if [ "$AFTER" != "$LOCAL_HEAD" ]; then
  warn "installed revision (${AFTER}) does not match local HEAD
  (${LOCAL_HEAD}).  The upgrade did NOT land what you expected -- check
  ${ELPA_DIR} and whether the remote really has your commit."
  status=1
else
  echo "  matches local HEAD (${LOCAL_HEAD}) ✓"
fi

smoke="$(ec "(and (featurep '${PKG}) (fboundp 'gp-helm))" 2>&1 || true)"
if [ "$smoke" = "t" ]; then
  echo "  live session has ${PKG} loaded and gp-helm bound ✓"
else
  warn "smoke check returned '${smoke}' instead of t; ${PKG} may not be
  loaded in the running Emacs."
  status=1
fi

# Stale-bytecode canary: package-vc-upgrade should have recompiled everything.
stale=0
for el in "$ELPA_DIR"/*.el; do
  [ -e "$el" ] || continue
  elc="${el}c"
  if [ -e "$elc" ] && [ "$el" -nt "$elc" ]; then
    warn "$(basename "$elc") is older than its source"
    stale=1
  fi
done
[ "$stale" -eq 0 ] && echo "  byte-compiled files are up to date ✓"

if [ "$status" -eq 0 ]; then
  say "Done -- ${PKG} ${AFTER} is installed and live."
  echo "  Note: a reload does not refresh changed defcustom defaults.  If this"
  echo "  revision changed one, makunbound it in the live session; the"
  echo "  installed copy will have it right after the next restart anyway."
else
  say "Finished with warnings (see above)."
fi
exit "$status"
