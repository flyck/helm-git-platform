# TODO

## GitHub labels support

GitHub PRs carry labels (their equivalent of a tag). Bitbucket has no
equivalent concept, so this is GitHub-specific — likely a `gp-defop` that
the Bitbucket backend implements as a no-op/empty list.

- `C-c C-c` on a label in either view should filter/jump to the set of PRs
  carrying that label (open PRs with that label, workspace/repo-wide).

Depends on the GitHub backend (`git-platform-github.el`) landing first.

## Notice when the PR's branch has uncommitted local work

No easy way to see there is work in progress on a PR's branch.  A single
line in the detail view would be enough:

    ⚠ branch has uncommitted local work

**Only when actually on that branch.**  Guessing about a checkout parked
on some other branch is noise -- and misleading, since the dirty files
would belong to whatever is checked out, not to this PR.  So the
condition is: a local checkout exists, AND its current branch equals the
PR's source branch, AND the tree is dirty.

Everything needed already exists, so this is mostly wiring:

- `gp-checkout-dirty-p' (gp-checkout.el:58) -- `git status --porcelain'
- `gp-checkout-current-branch' (gp-checkout.el:64)
- `gp-local-find-checkout' for the directory, `gp-pr-source-branch' for
  the branch to compare against

Open questions for whoever builds it:

- Both helpers shell out to git.  The detail view must not block on that
  during a render (the reaction summary already learned this the hard
  way -- a fetch mid-render re-entered the renderer), so compute it in
  the async load beside the stats, or cache per (dir . branch).
- Worth showing *what* is dirty (a file count, say) rather than a bare
  flag?  `git status --porcelain' already returns the list.
- `gp-watch-mode' visits files on the branch anyway; the same notice may
  belong in the mode line there.

## Pipeline steps are not properly integrated with GitHub

The pipeline/CI section works against Bitbucket Pipelines but is only
partly wired up for GitHub Actions.  Needs a pass over `gp-pipeline.el`
against a real Actions run to find what is missing or mismapped --
step-level state, logs, re-running a single job, and the manual-gate
handoff are the likely gaps (Bitbucket's halted-manual-step model has no
direct Actions equivalent; Actions uses environment approvals instead).

## Done: comment reactions (GitHub only)

Built behind `reactions-supported-p` / `reaction-choices` /
`comment-reactions` / `set-comment-reaction`, with `+` (quick 👍) and `!`
(picker) in the detail view.

The original note assumed "for bitbucket only like is possible" -- that turned
out not to hold for Bitbucket **Cloud**:

- its `swagger.json` has zero occurrences of reaction/emoji/like as a path,
  operation or field, and `/comments/{id}/likes` 404s while `/resolve` 405s,
  so the endpoint genuinely does not exist;
- the web UI *does* show a Like button, but no public v2.0 route exposes it,
  and emoji reactions are still only a feature request
  ([BCLOUD-21346](https://jira.atlassian.com/browse/BCLOUD-21346), "Gathering
  Interest" since 2021);
- Bitbucket **Server/DC** is different: it has a documented comment-likes REST
  API and real emoji reactions in the UI.  A Server backend could therefore
  implement the four ops with a one-element `reaction-choices` -- which is why
  they speak in opaque content tokens rather than assuming GitHub's eight.

## Done: inline comment targets are pre-checked (GitHub)

GitHub accepts an inline review comment only on a line inside one of the
PR's diff hunks; anything else comes back as a bare 422
(`pull_request_review_thread.path/line could not be resolved`) *after* the
comment has been written.  Bitbucket accepts a comment on any line of any
file, so the same workflow silently breaks when switching forges.

`gp-inline-target-problem' now checks the target against the diff before
posting and refuses with the commentable ranges named, keeping the compose
buffer intact.  Failing writes also log their request payload (credentials
redacted), since a 422 naming a field is only meaningful next to the value
that was sent.
