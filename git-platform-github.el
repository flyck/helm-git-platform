;;; git-platform-github.el --- GitHub backend for git-platform -*- lexical-binding: t; -*-

;;; Commentary:

;; The GitHub implementation of the `git-platform' protocol.  Each method
;; delegates to the corresponding endpoint function in `github-api.el',
;; which remains the single network choke-point (and the thing the
;; test-suite mocks).  Credentials are read through the existing
;; `github-*' customs/env, so no extra configuration is needed beyond
;; setting `git-platform-default-backend' to `github'.
;;
;; A handful of protocol operations have no clean GitHub equivalent (see
;; github-api.el's Commentary for the full list -- comment resolution via
;; GraphQL, no un-approve, no draft-toggle-back, no default reviewers, no
;; per-job manual run).  Those methods here simply surface whatever
;; `github-api.el' does (a `user-error' or a documented nil), rather than
;; papering over the gap.

;;; Code:

(require 'eieio)
(require 'git-platform)
(require 'github-api)

(defclass git-platform-github (git-platform) ()
  :documentation "GitHub backend.")

;;;; Network operations ---------------------------------------------------------

(cl-defmethod gp--user-uuid ((_ git-platform-github))
  (github-user-login))
(cl-defmethod gp--workspace-pull-requests ((_ git-platform-github) &optional uuid state max-items)
  (github-workspace-pull-requests uuid state max-items))
(cl-defmethod gp--reviewing-pull-requests ((_ git-platform-github) &optional uuid limit states)
  (github-reviewing-pull-requests uuid limit states))
(cl-defmethod gp--reviewing-pull-requests-async ((_ git-platform-github) uuid states on-batch on-done &optional limit)
  (github-reviewing-pull-requests-async uuid states on-batch on-done limit))
(cl-defmethod gp--open-pull-requests-async ((_ git-platform-github) states on-batch on-done &optional limit)
  (github-open-pull-requests-async states on-batch on-done limit))
(cl-defmethod gp--pull-request ((_ git-platform-github) full-name id)
  (github-pull-request full-name id))
(cl-defmethod gp--pull-request-async ((_ git-platform-github) full-name id callback)
  (github-pull-request-async full-name id callback))
(cl-defmethod gp--pull-request-comments ((_ git-platform-github) full-name id &optional max-items)
  (github-pull-request-comments full-name id max-items))
(cl-defmethod gp--pull-request-comments-async ((_ git-platform-github) full-name id callback &optional max-items)
  (github-pull-request-comments-async full-name id callback max-items))
(cl-defmethod gp--pull-request-diff ((_ git-platform-github) full-name id &optional commit)
  (github-pull-request-diff full-name id commit))
(cl-defmethod gp--pull-request-stats ((_ git-platform-github) full-name id &optional pr)
  (github-pull-request-stats full-name id pr))
(cl-defmethod gp--create-comment ((_ git-platform-github) full-name id text &optional inline parent-id)
  (github-create-comment full-name id text inline parent-id))
(cl-defmethod gp--resolve-comment ((_ git-platform-github) full-name id comment-id)
  (github-resolve-comment full-name id comment-id))
(cl-defmethod gp--reopen-comment ((_ git-platform-github) full-name id comment-id)
  (github-reopen-comment full-name id comment-id))
(cl-defmethod gp--edit-comment ((_ git-platform-github) full-name id comment-id text)
  (github-edit-comment full-name id comment-id text))
(cl-defmethod gp--delete-comment ((_ git-platform-github) full-name id comment-id)
  (github-delete-comment full-name id comment-id))
(cl-defmethod gp--set-pull-request-draft ((_ git-platform-github) full-name id draft &optional title)
  (github-set-pull-request-draft full-name id draft title))
(cl-defmethod gp--approve-pr ((_ git-platform-github) full-name id &optional unapprove reason)
  (github-approve-pr full-name id unapprove reason))
(cl-defmethod gp--request-changes-pr ((_ git-platform-github) full-name id &optional unrequest reason)
  (github-request-changes-pr full-name id unrequest reason))
(cl-defmethod gp--review-retraction-kind ((_ git-platform-github))
  (github-review-retraction-kind))
(cl-defmethod gp--open-pr-for-branch ((_ git-platform-github) full-name branch)
  (github-open-pr-for-branch full-name branch))
(cl-defmethod gp--repo-default-branch ((_ git-platform-github) full-name)
  (github-repo-default-branch full-name))
(cl-defmethod gp--repo-default-reviewers ((_ git-platform-github) full-name)
  (github-repo-default-reviewers full-name))
(cl-defmethod gp--repo-suggested-reviewers ((_ git-platform-github) full-name)
  (github-repo-suggested-reviewers full-name))
(cl-defmethod gp--set-pull-request-reviewers ((_ git-platform-github)
                                              full-name id reviewer-ids
                                              &optional current-ids)
  ;; needs the current set to send POST/DELETE deltas
  (github-set-pull-request-reviewers full-name id reviewer-ids current-ids))
(cl-defmethod gp--create-pull-request ((_ git-platform-github) full-name source dest title &optional description draft close-source-branch reviewer-uuids)
  (github-create-pull-request full-name source dest title
                              :description description
                              :draft draft
                              :close-source-branch close-source-branch
                              :reviewer-uuids reviewer-uuids))
(cl-defmethod gp--repo-open-pr-count ((_ git-platform-github) full-name)
  (github-repo-open-pr-count full-name))
(cl-defmethod gp--repo-pull-requests ((_ git-platform-github) full-name &optional state)
  (github-repo-pull-requests full-name state))
(cl-defmethod gp--commit-build-states ((_ git-platform-github) full-name hash)
  (github-commit-build-states full-name hash))
(cl-defmethod gp--commit-build-states-async ((_ git-platform-github) full-name hash callback)
  ;; github-api.el has no async combined-status fetcher (Bitbucket's
  ;; version exists only because gp-helm.el used to call its async GET
  ;; primitive directly); an idle timer gives the same non-blocking
  ;; contract without duplicating a whole async client path for one call.
  (run-with-idle-timer
   0 nil
   (lambda ()
     (funcall callback (ignore-errors (github-commit-build-states full-name hash))))))
(cl-defmethod gp--resolve-mentions ((_ git-platform-github) text)
  ;; GitHub mentions are already literal "@username" text -- nothing to
  ;; resolve, unlike Bitbucket's "@{account_id}" token indirection.
  text)

;; Pipelines (GitHub Actions workflow runs -- see github-api.el's CI mapping note)
(cl-defmethod gp--pipelines-for-branch ((_ git-platform-github) full-name branch &optional max-items commit)
  (github-pipelines-for-branch full-name branch max-items commit))
(cl-defmethod gp--pipeline-steps ((_ git-platform-github) full-name pipeline-uuid)
  (github-pipeline-steps full-name pipeline-uuid))
(cl-defmethod gp--pipeline-stop ((_ git-platform-github) full-name pipeline-uuid)
  (github-pipeline-stop full-name pipeline-uuid))
(cl-defmethod gp--pipeline-trigger ((_ git-platform-github) full-name branch &optional selector variables)
  (github-pipeline-trigger full-name branch selector variables))
(cl-defmethod gp--pipeline-run-manual-step ((_ git-platform-github) full-name branch pipeline step)
  (github-pipeline-run-manual-step full-name branch pipeline step))
(cl-defmethod gp--pipeline-step-rerun ((_ git-platform-github) full-name pipeline-uuid step)
  (github-pipeline-step-rerun full-name pipeline-uuid step))
(cl-defmethod gp--pipeline-web-url ((_ git-platform-github) full-name pipeline &optional step)
  (github-pipeline-web-url full-name pipeline step))
(cl-defmethod gp--pipeline-step-log ((_ git-platform-github) full-name pipeline-uuid step-uuid)
  (github-pipeline-step-log full-name pipeline-uuid step-uuid))

;;;; Field accessors ----------------------------------------------------------

(cl-defmethod gp--pr-full-name ((_ git-platform-github) pr)
  (let-alist pr .base.repo.full_name))
(cl-defmethod gp--pr-source-branch ((_ git-platform-github) pr)
  (let-alist pr .head.ref))
(cl-defmethod gp--pr-source-commit ((_ git-platform-github) pr)
  (let-alist pr .head.sha))
(cl-defmethod gp--pr-destination-branch ((_ git-platform-github) pr)
  (let-alist pr .base.ref))
(cl-defmethod gp--pr-draft-p ((_ git-platform-github) pr)
  (github-pr-draft-p pr))
(cl-defmethod gp--pr-authored-by-p ((_ git-platform-github) pr uuid)
  (github-pr-authored-by-p pr uuid))
(cl-defmethod gp--pr-author-name ((_ git-platform-github) pr)
  (let-alist pr .user.login))
(cl-defmethod gp--pr-author-avatar ((_ git-platform-github) pr)
  (let-alist pr .user.avatar_url))
(cl-defmethod gp--pr-open-p ((_ git-platform-github) pr)
  (equal (alist-get 'state pr) "open"))
(cl-defmethod gp--pr-merged-p ((_ git-platform-github) pr)
  (and (alist-get 'merged pr) t))
(cl-defmethod gp--pr-repo-slug ((_ git-platform-github) pr)
  ;; Prefer .base.repo.name when present, but don't depend on it -- some
  ;; response shapes (and this project's own PR-list search fetches)
  ;; only guarantee full_name, so fall back to its slug half.
  (let-alist pr
    (or .base.repo.name
        (when .base.repo.full_name
          (car (last (split-string .base.repo.full_name "/")))))))
(cl-defmethod gp--pr-review-tally ((_ git-platform-github) pr)
  (github-pr-review-tally pr))
(cl-defmethod gp--pr-review-tally-async ((_ git-platform-github) pr callback)
  (github-pr-review-tally-async pr callback))
(cl-defmethod gp--pr-reviewers-async ((_ git-platform-github) pr callback)
  (github-pr-reviewers-async pr callback))
(cl-defmethod gp--pr-my-review-state ((_ git-platform-github) pr uuid)
  (github-pr-my-review-state pr uuid))
(cl-defmethod gp--pr-comment-count ((_ git-platform-github) pr)
  (+ (or (alist-get 'comments pr) 0) (or (alist-get 'review_comments pr) 0)))
(cl-defmethod gp--pr-description ((_ git-platform-github) pr)
  ;; GitHub sends JSON null for an empty body, which the parser gives us as
  ;; :null rather than nil.
  (let ((b (alist-get 'body pr)))
    (when (stringp b)
      (unless (string-empty-p (string-trim b)) b))))
(cl-defmethod gp--comment-resolved-p ((_ git-platform-github) comment)
  (github-comment-resolved-p comment))
(cl-defmethod gp--comment-resolvable-p ((_ git-platform-github) comment)
  (github-comment-resolvable-p comment))
(cl-defmethod gp--comment-own-p ((_ git-platform-github) comment uuid)
  (github-comment-own-p comment uuid))
(cl-defmethod gp--backend-name ((_ git-platform-github)) 'github)

;; Pipeline / step shape accessors
(cl-defmethod gp--pipeline-state ((_ git-platform-github) pipeline)
  (github-pipeline-state pipeline))
(cl-defmethod gp--pipeline-result ((_ git-platform-github) pipeline)
  (github-pipeline-result pipeline))
(cl-defmethod gp--pipeline-finished-p ((_ git-platform-github) pipeline)
  (github-pipeline-finished-p pipeline))
(cl-defmethod gp--pipeline-number ((_ git-platform-github) pipeline)
  (github-pipeline-number pipeline))
(cl-defmethod gp--pipeline-commit ((_ git-platform-github) pipeline)
  (github-pipeline-commit pipeline))
(cl-defmethod gp--commit-message ((_ git-platform-github) full-name hash)
  (github-commit-message full-name hash))
(cl-defmethod gp--commit-summary ((_ git-platform-github) message)
  (github-commit-summary message))
(cl-defmethod gp--pipeline-step-state ((_ git-platform-github) step)
  (github-pipeline-step-state step))
(cl-defmethod gp--pipeline-step-result ((_ git-platform-github) step)
  (github-pipeline-step-result step))
(cl-defmethod gp--pipeline-step-running-p ((_ git-platform-github) step)
  (github-pipeline-step-running-p step))
(cl-defmethod gp--pipeline-step-manual-p ((_ git-platform-github) step)
  (github-pipeline-step-manual-p step))
(cl-defmethod gp--pipeline-step-runnable-manual-p ((_ git-platform-github) step)
  (github-pipeline-step-runnable-manual-p step))
(cl-defmethod gp--pipeline-step-rerunnable-p ((_ git-platform-github) step)
  (github-pipeline-step-rerunnable-p step))
(cl-defmethod gp--pipelines-sort ((_ git-platform-github) pipelines step-counts)
  (github-pipelines-sort pipelines step-counts))
(cl-defmethod gp--pipelines-match-commit ((_ git-platform-github) pipelines commit)
  (github-pipelines-match-commit pipelines commit))

(provide 'git-platform-github)
;;; git-platform-github.el ends here
