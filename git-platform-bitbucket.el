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
(cl-defmethod gp--reviewing-pull-requests ((_ git-platform-bitbucket) &optional uuid limit states)
  (bitbucket-reviewing-pull-requests uuid limit states))
(cl-defmethod gp--reviewing-pull-requests-async ((_ git-platform-bitbucket) uuid states on-batch on-done &optional limit)
  (bitbucket-reviewing-pull-requests-async uuid states on-batch on-done limit))
(cl-defmethod gp--open-pull-requests-async ((_ git-platform-bitbucket) states on-batch on-done &optional limit)
  (bitbucket-open-pull-requests-async states on-batch on-done limit))
(cl-defmethod gp--pull-request ((_ git-platform-bitbucket) full-name id)
  (bitbucket-pull-request full-name id))
(cl-defmethod gp--pull-request-comments ((_ git-platform-bitbucket) full-name id &optional max-items)
  (bitbucket-pull-request-comments full-name id max-items))
(cl-defmethod gp--pull-request-diff ((_ git-platform-bitbucket) full-name id)
  (bitbucket-pull-request-diff full-name id))
(cl-defmethod gp--pull-request-stats ((_ git-platform-bitbucket) full-name id &optional pr)
  (bitbucket-pull-request-stats full-name id pr))
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
(cl-defmethod gp--open-pr-for-branch ((_ git-platform-bitbucket) full-name branch)
  (bitbucket-open-pr-for-branch full-name branch))
(cl-defmethod gp--repo-open-pr-count ((_ git-platform-bitbucket) full-name)
  (bitbucket-repo-open-pr-count full-name))
(cl-defmethod gp--repo-pull-requests ((_ git-platform-bitbucket) full-name &optional state)
  (bitbucket-repo-pull-requests full-name state))
(cl-defmethod gp--commit-build-states ((_ git-platform-bitbucket) full-name hash)
  (bitbucket-commit-build-states full-name hash))

;;;; Field accessors ----------------------------------------------------------

;; The JSON-shape logic lives in bitbucket-api.el; methods delegate to it so
;; there is a single Bitbucket implementation.
(cl-defmethod gp--pr-full-name ((_ git-platform-bitbucket) pr)
  (let-alist pr .destination.repository.full_name))
(cl-defmethod gp--pr-source-branch ((_ git-platform-bitbucket) pr)
  (let-alist pr .source.branch.name))
(cl-defmethod gp--pr-destination-branch ((_ git-platform-bitbucket) pr)
  (let-alist pr .destination.branch.name))
(cl-defmethod gp--pr-draft-p ((_ git-platform-bitbucket) pr)
  (bitbucket-pr-draft-p pr))
(cl-defmethod gp--pr-authored-by-p ((_ git-platform-bitbucket) pr uuid)
  (bitbucket-pr-authored-by-p pr uuid))
(cl-defmethod gp--pr-review-tally ((_ git-platform-bitbucket) pr)
  (bitbucket-pr-review-tally pr))
(cl-defmethod gp--comment-resolved-p ((_ git-platform-bitbucket) comment)
  (bitbucket-comment-resolved-p comment))
(cl-defmethod gp--comment-own-p ((_ git-platform-bitbucket) comment uuid)
  (bitbucket-comment-own-p comment uuid))

(provide 'git-platform-bitbucket)
;;; git-platform-bitbucket.el ends here
