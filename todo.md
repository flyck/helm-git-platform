# TODO

## GitHub labels support

GitHub PRs carry labels (their equivalent of a tag). Bitbucket has no
equivalent concept, so this is GitHub-specific — likely a `gp-defop` that
the Bitbucket backend implements as a no-op/empty list.

- Show labels in the PR list/overview (helm and/or the non-helm UI list).
- Show labels in the PR detail view.
- `C-c C-c` on a label in either view should filter/jump to the set of PRs
  carrying that label (open PRs with that label, workspace/repo-wide).

Depends on the GitHub backend (`git-platform-github.el`) landing first.
