# TODO

## GitHub labels support

- `C-c C-c` on a label in either view should filter/jump to the set of PRs
  carrying that label (open PRs with that label, workspace/repo-wide).

## Notice when the PR's branch has uncommitted local work

Show `⚠ branch has uncommitted local work` in the detail view, but only when
the local checkout is actually on the PR's source branch -- otherwise the
dirty files belong to whatever else is checked out.

`gp-checkout-dirty-p' and `gp-checkout-current-branch' already exist; both
shell out to git, so compute it in the async load, not during a render.

## Declare the optional deps properly

`Package-Requires' only lists `magit-section' and `transient', so helm,
magit, markdown-mode and emojify are left to the user to install by hand
and the README has to explain the split in a table.  Decide what the
package should actually do -- declare them, or `use-package' extras the
recipe can pull -- instead of documenting a manual step.

## Merge button

Merge a PR from the detail view.  The catch is the strategy:

- GitHub lets you pick per PR (merge / squash / rebase), and a repo can
  disable some of them -- so offer the choice, but only among the ones the
  repo actually allows.
- Bitbucket also allows a per-PR choice in principle, but our projects
  enforce a workspace-wide fast-forward-only policy, so there is nothing
  to ask about -- prompting would be noise.

So the strategy is a backend question, not a UI one: ask the backend what
it offers and only prompt when there is more than one answer.

## Close a PR, with an optional reason

Close/decline a PR from the detail view without merging it.  As with the
merge button, the shape differs per forge:

- GitHub distinguishes *why* -- a closed PR can be marked "completed" or
  "not planned" (the same state-reason Issues use), which shows on the
  timeline.
- Bitbucket just declines; there is no reason field, though a farewell
  comment is the usual convention.

So the reason is optional and backend-dependent: ask what reasons the
backend accepts and skip the prompt entirely when it has none.

## Pipeline steps are not properly integrated with GitHub

`gp-pipeline.el' works against Bitbucket Pipelines but is only partly wired
up for GitHub Actions: step-level state, logs, re-running a single job, and
the manual-gate handoff (Actions uses environment approvals, which has no
direct equivalent to Bitbucket's halted manual step).
