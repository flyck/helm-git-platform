#!/usr/bin/env bash
# Reproduce the GitHub Actions CI run locally, as faithfully as a local
# machine allows.  Use this before pushing -- a plain `eldev test` passes
# in situations CI fails, for two reasons this script removes:
#
#   1. Credentials.  Your shell (and ~/.emacs.d/.env) export
#      BITBUCKET_USER_EMAIL / BITBUCKET_API_TOKEN / GITHUB_TOKEN, so code
#      that reaches the network succeeds locally and signals "credentials
#      missing" in CI.  Rendering and pure logic must not need auth; this
#      script unsets them so that assumption is actually tested.
#
#   2. Warnings-as-errors.  CI compiles with `--set all
#      --warnings-as-errors'; plain `eldev compile' demotes those to
#      warnings you will not notice scrolling past.
#
# What this still does NOT cover: CI runs Emacs 28.2 / 29.4 / 30.1, and
# some compiler diagnostics exist only on older versions (e.g. Emacs 30's
# "value from call to `equal' is unused").  Pass a specific binary to
# check one: EMACS=/path/to/emacs-30 ./run-ci-locally.sh
set -euo pipefail
cd "$(dirname "$0")"

EMACS_BIN="${EMACS:-emacs}"
echo "==> $("$EMACS_BIN" --version | head -1)"

# CI has no credentials and no sqlite in some builds; drop ours so the
# suite exercises the same unauthenticated paths.
run_clean() {
  env -u BITBUCKET_USER_EMAIL \
      -u BITBUCKET_API_TOKEN \
      -u BITBUCKET_TOKEN \
      -u BITBUCKET_WORKSPACE \
      -u GITHUB_TOKEN \
      ELDEV_EMACS="$EMACS_BIN" \
      "$@"
}

echo "==> byte-compiling with warnings as errors (CI: compile job)"
run_clean eldev -dtT -C compile --set all --warnings-as-errors

echo "==> running tests without credentials (CI: test job)"
run_clean eldev -p -dtT -C test

echo "==> OK: compile clean and tests green under CI conditions"
