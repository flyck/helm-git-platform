;;; git-platform-bitbucket.el --- Bitbucket backend for git-platform -*- lexical-binding: t; -*-

;;; Commentary:

;; The Bitbucket Cloud implementation of the `git-platform' protocol.  Each
;; method delegates to the corresponding endpoint function in
;; `bitbucket-api.el', which remains the single network choke-point (and the
;; thing the test-suite mocks).  Credentials/workspace are read through the
;; existing `bitbucket-*' customs/env, so no extra configuration is needed.

;;; Code:

(require 'eieio)
(require 'git-platform)
(require 'bitbucket-api)

(defclass git-platform-bitbucket (git-platform) ()
  :documentation "Bitbucket Cloud backend.")

;;;; Network operations -------------------------------------------------------

(cl-defmethod gp--user-uuid ((_ git-platform-bitbucket))
  (bitbucket-user-uuid))
(cl-defmethod gp--workspace-pull-requests ((_ git-platform-bitbucket) &optional uuid state max-items)
  (bitbucket-workspace-pull-requests uuid state max-items))
(cl-defmethod gp--workspace-pull-requests-async ((_ git-platform-bitbucket) callback
                                                 &optional uuid state max-items)
  (bitbucket-workspace-pull-requests-async callback uuid state max-items))
(cl-defmethod gp--reviewing-pull-requests ((_ git-platform-bitbucket) &optional uuid limit states)
  (bitbucket-reviewing-pull-requests uuid limit states))
(cl-defmethod gp--reviewing-pull-requests-async ((_ git-platform-bitbucket) uuid states on-batch on-done &optional limit)
  (bitbucket-reviewing-pull-requests-async uuid states on-batch on-done limit))
(cl-defmethod gp--open-pull-requests-async ((_ git-platform-bitbucket) states on-batch on-done &optional limit)
  (bitbucket-open-pull-requests-async states on-batch on-done limit))
(cl-defmethod gp--pull-request ((_ git-platform-bitbucket) full-name id)
  (bitbucket-pull-request full-name id))
(cl-defmethod gp--pull-request-async ((_ git-platform-bitbucket) full-name id callback)
  (bitbucket-pull-request-async full-name id callback))
(cl-defmethod gp--pull-request-comments ((_ git-platform-bitbucket) full-name id &optional max-items)
  (bitbucket-pull-request-comments full-name id max-items))
(cl-defmethod gp--pull-request-comments-async ((_ git-platform-bitbucket) full-name id callback &optional max-items)
  (bitbucket-pull-request-comments-async full-name id callback max-items))
(cl-defmethod gp--pull-request-diff ((_ git-platform-bitbucket) full-name id &optional commit)
  (bitbucket-pull-request-diff full-name id commit))
(cl-defmethod gp--pull-request-stats ((_ git-platform-bitbucket) full-name id &optional pr)
  (bitbucket-pull-request-stats full-name id pr))
(cl-defmethod gp--pull-request-diff-async ((_ git-platform-bitbucket) full-name id commit pr callback)
  (bitbucket-pull-request-diff-async full-name id commit pr callback))
(cl-defmethod gp--pull-request-stats-async ((_ git-platform-bitbucket) full-name id pr callback)
  (bitbucket-pull-request-stats-async full-name id pr callback))
(cl-defmethod gp--create-comment ((_ git-platform-bitbucket) full-name id text &optional inline parent-id)
  (bitbucket-create-comment full-name id text inline parent-id))
(cl-defmethod gp--resolve-comment ((_ git-platform-bitbucket) full-name id comment-id)
  (bitbucket-resolve-comment full-name id comment-id))
(cl-defmethod gp--reopen-comment ((_ git-platform-bitbucket) full-name id comment-id)
  (bitbucket-reopen-comment full-name id comment-id))
(cl-defmethod gp--edit-comment ((_ git-platform-bitbucket) full-name id comment-id text)
  (bitbucket-edit-comment full-name id comment-id text))
(cl-defmethod gp--delete-comment ((_ git-platform-bitbucket) full-name id comment-id)
  (bitbucket-delete-comment full-name id comment-id))
(cl-defmethod gp--set-pull-request-draft ((_ git-platform-bitbucket) full-name id draft &optional title)
  (bitbucket-set-pull-request-draft full-name id draft title))
(cl-defmethod gp--approve-pr ((_ git-platform-bitbucket) full-name id &optional unapprove _reason)
  (bitbucket-approve-pr full-name id unapprove))
(cl-defmethod gp--request-changes-pr ((_ git-platform-bitbucket) full-name id &optional unrequest _reason)
  (bitbucket-request-changes-pr full-name id unrequest))
(cl-defmethod gp--review-retraction-kind ((_ git-platform-bitbucket))
  'retract)
;; Bitbucket Cloud PRs have no labels: no field on the PR, no repo-level
;; label pool, nothing to set.  Reporting that here (rather than returning an
;; empty list from `gp--pr-labels' alone) is what lets the UI drop the label
;; affordances entirely instead of showing a slot that can never fill.
(cl-defmethod gp--labels-supported-p ((_ git-platform-bitbucket)) nil)
;; The web UI has a single binary Like, but the public v2.0 API exposes no
;; route for it and emoji reactions are only a feature request
;; (BCLOUD-21346, "Gathering Interest" since 2021).  Bitbucket *Server/DC*
;; does have a documented comment-likes API -- a different product; a
;; Server backend could implement these four with a one-element
;; `reaction-choices'.
;; Bitbucket accepts a comment on any line of any file, so there is nothing
;; to pre-check.
(cl-defmethod gp--inline-target-problem ((_ git-platform-bitbucket) _fn _id _path _line) nil)
(cl-defmethod gp--reactions-supported-p ((_ git-platform-bitbucket)) nil)
(cl-defmethod gp--reaction-choices ((_ git-platform-bitbucket)) nil)
(cl-defmethod gp--comment-reactions ((_ git-platform-bitbucket) _full-name _comment) nil)
(cl-defmethod gp--set-comment-reaction ((_ git-platform-bitbucket) _full-name _comment _content _on)
  (user-error "Bitbucket Cloud's API has no reactions on comments"))
(cl-defmethod gp--pr-labels ((_ git-platform-bitbucket) _pr) nil)
(cl-defmethod gp--repo-labels ((_ git-platform-bitbucket) _full-name) nil)
(cl-defmethod gp--set-pull-request-labels ((_ git-platform-bitbucket) _full-name _id _labels)
  (user-error "Bitbucket pull requests do not support labels"))
(cl-defmethod gp--open-pr-for-branch ((_ git-platform-bitbucket) full-name branch)
  (bitbucket-open-pr-for-branch full-name branch))
(cl-defmethod gp--repo-default-branch ((_ git-platform-bitbucket) full-name)
  (bitbucket-repo-default-branch full-name))
(cl-defmethod gp--repo-default-reviewers ((_ git-platform-bitbucket) full-name)
  (bitbucket-repo-default-reviewers full-name))
(cl-defmethod gp--repo-suggested-reviewers ((_ git-platform-bitbucket) full-name)
  (bitbucket-repo-suggested-reviewers full-name))
(cl-defmethod gp--pull-request-merge-strategies ((_ git-platform-bitbucket) full-name id)
  (bitbucket-pull-request-merge-strategies full-name id))
;; Bitbucket Cloud's PR payload carries no mergeability field.  There is a
;; `.../pullrequests/{id}/conflicts' endpoint, but it 302s to
;; `.../file-conflicts/{spec}', which rejects Atlassian API-token auth with
;; 403 ("This resource does not support authentication using the provided
;; token") while ordinary PR reads succeed on the same credentials -- so it
;; is unreachable with the credentials this package uses.  Returning nil
;; means "cannot answer", which callers must not treat as a conflict.
(cl-defmethod gp--pull-request-mergeability ((_ git-platform-bitbucket) _full-name _id) nil)
;; Bitbucket Cloud has diff/diffstat but no ahead/behind commit counts.
(cl-defmethod gp--pull-request-divergence ((_ git-platform-bitbucket) _fn _base _head) nil)
(cl-defmethod gp--merge-pull-request ((_ git-platform-bitbucket) full-name id
                                      &optional strategy message close-source-branch)
  (bitbucket-merge-pull-request full-name id strategy message close-source-branch))
(cl-defmethod gp--set-pull-request-title ((_ git-platform-bitbucket) full-name id title)
  (bitbucket-set-pull-request-title full-name id title))
(cl-defmethod gp--set-pull-request-description ((_ git-platform-bitbucket) full-name id description &optional title)
  (bitbucket-set-pull-request-description full-name id description title))
(cl-defmethod gp--set-pull-request-reviewers ((_ git-platform-bitbucket)
                                              full-name id reviewer-ids
                                              &optional _current-ids)
  ;; whole-list PUT: the current set is irrelevant, the desired one replaces it
  (bitbucket-set-pull-request-reviewers full-name id reviewer-ids))
(cl-defmethod gp--create-pull-request ((_ git-platform-bitbucket) full-name source dest title &optional description draft close-source-branch reviewer-uuids)
  (bitbucket-create-pull-request full-name source dest title
                                 :description description
                                 :draft draft
                                 :close-source-branch close-source-branch
                                 :reviewer-uuids reviewer-uuids))
(cl-defmethod gp--repo-open-pr-count ((_ git-platform-bitbucket) full-name)
  (bitbucket-repo-open-pr-count full-name))
(cl-defmethod gp--repo-pull-requests ((_ git-platform-bitbucket) full-name &optional state)
  (bitbucket-repo-pull-requests full-name state))
(cl-defmethod gp--pull-request-commits-async ((_ git-platform-bitbucket) full-name id callback &optional max-items)
  (bitbucket-pull-request-commits-async full-name id callback max-items))
(cl-defmethod gp--commit-build-states ((_ git-platform-bitbucket) full-name hash)
  (bitbucket-commit-build-states full-name hash))
(cl-defmethod gp--resolve-mentions ((_ git-platform-bitbucket) text)
  (bitbucket-resolve-mentions text))
(cl-defmethod gp--commit-build-states-async ((_ git-platform-bitbucket) full-name hash callback)
  (bitbucket-api-get-async
   (format "/repositories/%s/commit/%s/statuses" full-name hash)
   '(("fields" . "values.state,next"))
   (lambda (page)
     (funcall callback
              (and page (mapcar (lambda (s) (alist-get 'state s))
                                (alist-get 'values page)))))))

;; Pipelines
(cl-defmethod gp--pipelines-for-branch ((_ git-platform-bitbucket) full-name branch &optional max-items commit)
  (bitbucket-pipelines-for-branch full-name branch max-items commit))
(cl-defmethod gp--pipelines-for-branch-async ((_ git-platform-bitbucket) full-name branch max-items commit callback)
  (bitbucket-pipelines-for-branch-async full-name branch max-items commit callback))
(cl-defmethod gp--pipeline-steps ((_ git-platform-bitbucket) full-name pipeline-uuid)
  (bitbucket-pipeline-steps full-name pipeline-uuid))
(cl-defmethod gp--pipeline-steps-async ((_ git-platform-bitbucket) full-name pipeline-uuid callback)
  (bitbucket-pipeline-steps-async full-name pipeline-uuid callback))
(cl-defmethod gp--pipeline-stop ((_ git-platform-bitbucket) full-name pipeline-uuid)
  (bitbucket-pipeline-stop full-name pipeline-uuid))
(cl-defmethod gp--pipeline-trigger ((_ git-platform-bitbucket) full-name branch &optional selector variables)
  (bitbucket-pipeline-trigger full-name branch selector variables))
(cl-defmethod gp--pipeline-run-manual-step ((_ git-platform-bitbucket) full-name branch pipeline step)
  (bitbucket-pipeline-run-manual-step full-name branch pipeline step))
(cl-defmethod gp--pipeline-step-rerun ((_ git-platform-bitbucket) full-name _pipeline-uuid _step)
  (ignore full-name)
  (user-error "Bitbucket Pipelines has no per-step re-run; re-trigger the pipeline instead"))
(cl-defmethod gp--pipeline-web-url ((_ git-platform-bitbucket) full-name pipeline &optional step)
  (bitbucket-pipeline-web-url full-name pipeline step))
(cl-defmethod gp--pipeline-step-log ((_ git-platform-bitbucket) full-name pipeline-uuid step-uuid)
  (bitbucket-pipeline-step-log full-name pipeline-uuid step-uuid))
(cl-defmethod gp--pipeline-step-log-classify-line ((_ git-platform-bitbucket) line)
  (bitbucket-pipeline-step-log-classify-line line))

;;;; Field accessors ----------------------------------------------------------

;; The JSON-shape logic lives in bitbucket-api.el; methods delegate to it so
;; there is a single Bitbucket implementation.
(cl-defmethod gp--pr-full-name ((_ git-platform-bitbucket) pr)
  (let-alist pr .destination.repository.full_name))
(cl-defmethod gp--pr-source-branch ((_ git-platform-bitbucket) pr)
  (let-alist pr .source.branch.name))
(cl-defmethod gp--pr-source-commit ((_ git-platform-bitbucket) pr)
  (let-alist pr .source.commit.hash))
(cl-defmethod gp--pr-destination-branch ((_ git-platform-bitbucket) pr)
  (let-alist pr .destination.branch.name))
(cl-defmethod gp--pr-web-url ((_ git-platform-bitbucket) pr)
  (let-alist pr .links.html.href))
(cl-defmethod gp--comment-web-url ((_ git-platform-bitbucket) comment)
  (let-alist comment .links.html.href))
(cl-defmethod gp--pr-draft-p ((_ git-platform-bitbucket) pr)
  (bitbucket-pr-draft-p pr))
(cl-defmethod gp--pr-authored-by-p ((_ git-platform-bitbucket) pr uuid)
  (bitbucket-pr-authored-by-p pr uuid))
(cl-defmethod gp--pr-author-name ((_ git-platform-bitbucket) pr)
  (let-alist pr .author.display_name))
(cl-defmethod gp--pr-author-avatar ((_ git-platform-bitbucket) pr)
  (let-alist pr .author.links.avatar.href))
(cl-defmethod gp--pr-open-p ((_ git-platform-bitbucket) pr)
  (and (member (alist-get 'state pr) '("OPEN" nil)) t))
(cl-defmethod gp--pr-closed-reason ((_ git-platform-bitbucket) pr)
  (let ((r (alist-get 'reason pr)))
    (unless (or (null r) (string-empty-p (string-trim r))) r)))
(cl-defmethod gp--pr-merged-at ((_ git-platform-bitbucket) pr)
  ;; no `merged_at'; `updated_on' is the closest thing Bitbucket offers
  (alist-get 'updated_on pr))
(cl-defmethod gp--pr-merge-commit ((_ git-platform-bitbucket) pr)
  (let-alist pr .merge_commit.hash))
(cl-defmethod gp--pr-merged-p ((_ git-platform-bitbucket) pr)
  (equal (alist-get 'state pr) "MERGED"))
(cl-defmethod gp--pr-repo-slug ((_ git-platform-bitbucket) pr)
  (let-alist pr .destination.repository.slug))
(cl-defmethod gp--pr-review-tally ((_ git-platform-bitbucket) pr)
  (bitbucket-pr-review-tally pr))
(cl-defmethod gp--pr-review-tally-async ((_ git-platform-bitbucket) pr callback)
  (funcall callback (bitbucket-pr-review-tally pr)))
(cl-defmethod gp--pr-reviewers-async ((_ git-platform-bitbucket) pr callback)
  (funcall callback (bitbucket-pr-reviewers pr)))
(cl-defmethod gp--pr-my-review-state ((_ git-platform-bitbucket) pr uuid)
  (bitbucket-pr-my-review-state pr uuid))
(cl-defmethod gp--pr-comment-count ((_ git-platform-bitbucket) pr)
  (alist-get 'comment_count pr))
(cl-defmethod gp--pr-description ((_ git-platform-bitbucket) pr)
  (let ((d (alist-get 'description pr)))
    (unless (or (null d) (string-empty-p (string-trim d))) d)))
(cl-defmethod gp--comment-resolved-p ((_ git-platform-bitbucket) comment)
  (bitbucket-comment-resolved-p comment))
(cl-defmethod gp--comment-resolvable-p ((_ git-platform-bitbucket) _comment)
  t)
(cl-defmethod gp--comment-own-p ((_ git-platform-bitbucket) comment uuid)
  (bitbucket-comment-own-p comment uuid))
(cl-defmethod gp--backend-name ((_ git-platform-bitbucket)) 'bitbucket)

;; Pipeline / step shape accessors
(cl-defmethod gp--pipeline-state ((_ git-platform-bitbucket) pipeline)
  (bitbucket-pipeline-state pipeline))
(cl-defmethod gp--pipeline-result ((_ git-platform-bitbucket) pipeline)
  (bitbucket-pipeline-result pipeline))
(cl-defmethod gp--pipeline-finished-p ((_ git-platform-bitbucket) pipeline)
  (bitbucket-pipeline-finished-p pipeline))
(cl-defmethod gp--pipeline-id ((_ git-platform-bitbucket) pipeline)
  (alist-get 'uuid pipeline))
(cl-defmethod gp--pipeline-step-id ((_ git-platform-bitbucket) step)
  (alist-get 'uuid step))
(cl-defmethod gp--pipeline-number ((_ git-platform-bitbucket) pipeline)
  (bitbucket-pipeline-number pipeline))
(cl-defmethod gp--pipeline-commit ((_ git-platform-bitbucket) pipeline)
  (bitbucket-pipeline-commit pipeline))
(cl-defmethod gp--commit-message ((_ git-platform-bitbucket) full-name hash)
  (bitbucket-commit-message full-name hash))
(cl-defmethod gp--commit-message-async ((_ git-platform-bitbucket) full-name hash callback)
  (bitbucket-commit-message-async full-name hash callback))
(cl-defmethod gp--commit-summary ((_ git-platform-bitbucket) message)
  (bitbucket-commit-summary message))
(cl-defmethod gp--pipeline-step-state ((_ git-platform-bitbucket) step)
  (bitbucket-pipeline-step-state step))
(cl-defmethod gp--pipeline-step-result ((_ git-platform-bitbucket) step)
  (bitbucket-pipeline-step-result step))
(cl-defmethod gp--pipeline-step-running-p ((_ git-platform-bitbucket) step)
  (bitbucket-pipeline-step-running-p step))
(cl-defmethod gp--pipeline-step-manual-p ((_ git-platform-bitbucket) step)
  (bitbucket-pipeline-step-manual-p step))
(cl-defmethod gp--pipeline-step-runnable-manual-p ((_ git-platform-bitbucket) step)
  (bitbucket-pipeline-step-runnable-manual-p step))
(cl-defmethod gp--pipeline-step-rerunnable-p ((_ git-platform-bitbucket) _step)
  nil)
(cl-defmethod gp--pipelines-sort ((_ git-platform-bitbucket) pipelines step-counts)
  (bitbucket-pipelines-sort pipelines step-counts))
(cl-defmethod gp--pipelines-match-commit ((_ git-platform-bitbucket) pipelines commit)
  (bitbucket-pipelines-match-commit pipelines commit))

(provide 'git-platform-bitbucket)
;;; git-platform-bitbucket.el ends here
