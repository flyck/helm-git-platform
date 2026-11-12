#!/usr/bin/env bash
# Run the helm-git-platform ERT suite against the centralized mock service.
# Uses the user's installed packages (magit-section, transient) via
# package-initialize, but loads no user config (-Q-ish, isolated).
# Subr trampolines are off: stubbing a primitive (cl-letf on `yes-or-no-p')
# would otherwise native-compile one, which fails without a working
# libgccjit.  The code under test is loaded as source, so stubs still bite.
set -euo pipefail
cd "$(dirname "$0")/.."

emacs --batch -Q \
  --eval "(progn (require 'package) (package-initialize) (setq native-comp-enable-subr-trampolines nil))" \
  -L . -L tests \
  -l tests/gp-test-helper.el \
  -l bitbucket-env.el \
  -l gp-log.el \
  -l git-platform.el \
  -l bitbucket-api.el \
  -l git-platform-bitbucket.el \
  -l git-platform-mock.el \
  -l github-api.el \
  -l git-platform-github.el \
  -l gp-local.el \
  -l gp-checkout.el \
  -l gp-compose.el \
  -l gp-create.el \
  -l gp-reviewers.el \
  -l gp-helm-terminal.el \
  -l gp-helm-terminal-iterm2.el \
  -l gp-helm-terminal-ghostty.el \
  -l gp-pr-actions.el \
  -l gp-stealth.el \
  -l gp-pipeline.el \
  -l gp-overlay.el \
  -l gp-watch.el \
  -l gp-ui.el \
  -l gp-magit.el \
  -l gp-helm.el \
  -l gp-deploy-watch.el \
  -l tests/bitbucket-mock.el \
  -l tests/github-mock.el \
  -l tests/bitbucket-env-test.el \
  -l tests/gp-log-test.el \
  -l tests/bitbucket-api-test.el \
  -l tests/github-api-test.el \
  -l tests/bitbucket-pipeline-test.el \
  -l tests/git-platform-test.el \
  -l tests/git-platform-mock-test.el \
  -l tests/gp-api-drift-test.el \
  -l tests/gp-local-test.el \
  -l tests/gp-checkout-test.el \
  -l tests/gp-compose-test.el \
  -l tests/gp-create-test.el \
  -l tests/gp-reviewers-test.el \
  -l tests/gp-helm-terminal-test.el \
  -l tests/gp-helm-terminal-ghostty-test.el \
  -l tests/gp-pr-actions-test.el \
  -l tests/gp-stealth-test.el \
  -l tests/gp-ui-test.el \
  -l tests/gp-pipeline-test.el \
  -l tests/gp-overlay-test.el \
  -l tests/gp-watch-test.el \
  -l tests/gp-magit-test.el \
  -l tests/gp-helm-test.el \
  -l tests/gp-deploy-watch-test.el \
  -l tests/gp-e2e-detail-test.el \
  -l tests/gp-e2e-list-test.el \
  -f ert-run-tests-batch-and-exit
