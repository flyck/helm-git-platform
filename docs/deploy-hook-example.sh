#!/usr/bin/env bash
# Example `gp-pipeline-deploy-script' hook.
#
# helm-git-platform runs this to advance a gated manual deploy step, because
# Bitbucket Cloud has no REST endpoint that can (BCLOUD-20050).  All context
# arrives in the environment -- see the `gp-pipeline-deploy-script' docstring:
#
#   GP_WORKSPACE GP_REPO GP_FULL_NAME GP_BRANCH
#   GP_PIPELINE_ID GP_PIPELINE_UUID
#   GP_STEP_NAME GP_STEP_UUID GP_STEP_STATE
#   GP_PR_ID GP_WEB_URL
#
# Wire it up with:
#   (setq gp-pipeline-deploy-script '("~/bin/gp-deploy"))
#
# This example adapts those variables to the `bb-manual-step' browser tool
# from the bitbucket-general skill, which resolves steps by NAME (step uuids
# change on every re-run, so a uuid is the wrong key to pass).
set -euo pipefail

TOOL_DIR="${GP_DEPLOY_TOOL_DIR:-$HOME/git/ai-skills/plugins/whereversim-skills/skills/bitbucket-general/scripts/manual-step}"

: "${GP_REPO:?GP_REPO not set — is this being run by helm-git-platform?}"
: "${GP_STEP_NAME:?GP_STEP_NAME not set}"

echo "step:     ${GP_STEP_NAME}"
echo "repo:     ${GP_FULL_NAME:-$GP_REPO}"
echo "build:    ${GP_PIPELINE_ID:-<unknown>}"
echo "branch:   ${GP_BRANCH:-<unknown>}"
echo "url:      ${GP_WEB_URL:-<none>}"
echo

args=(trigger --repo "$GP_REPO" --step "$GP_STEP_NAME")
[[ -n "${GP_WORKSPACE:-}" ]]   && args+=(--workspace "$GP_WORKSPACE")
[[ -n "${GP_PIPELINE_ID:-}" ]] && args+=(--build "$GP_PIPELINE_ID")
[[ -n "${GP_BRANCH:-}" ]]      && args+=(--branch "$GP_BRANCH")

cd "$TOOL_DIR"
exec bun run src/cli.ts "${args[@]}"
