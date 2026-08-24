# TODO

## Coloring

would be nice if the progress animation were blue and not red.

## Add a queued status for pipeline steps

we should have a loading indecator icon here for spinning

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

## Bitbucket conflict detection needs different credentials

Conflicts are detected on GitHub (`mergeable_state' "dirty") but not on
Bitbucket: its PR payload has no such field, and the
`GET .../pullrequests/{id}/conflicts' endpoint 302s to
`.../file-conflicts/{spec}', which rejects Atlassian API-token auth with
403 ("This resource does not support authentication using the provided
token") where ordinary PR reads answer 200 on the same credentials.
Would need an app password / OAuth to be usable.

## Exclude generated files from the changed-lines counter

Lockfiles swamp the `+N -M' counts and the changed-files list -- a
one-line change reads as +4000 when `Pipfile.lock' moved.  Add a pattern
list of paths to leave out of the counter (still listed, just not counted,
or hidden entirely -- decide which reads better), defaulting to
`Pipfile.lock' and the obvious siblings (`poetry.lock',
`package-lock.json', `yarn.lock', `go.sum', `Cargo.lock').

## Fold "recent runs" into the commits list

The "Recent runs on this branch (N)" block is noise: it repeats the same
commit summary four times with a status each, and nothing is actionable
there.  Drop it and instead put each run's status and pipeline number
beside its commit in the Commits section -- one line per commit, carrying
the build result.  Keep the merge-commit run (see the merged-PR section),
which is a different question.

## Pipeline steps are not properly integrated with GitHub

`gp-pipeline.el' works against Bitbucket Pipelines but is only partly wired
up for GitHub Actions: step-level state, logs, re-running a single job, and
the manual-gate handoff (Actions uses environment approvals, which has no
direct equivalent to Bitbucket's halted manual step).
