;;; github-api-test.el --- Tests for the GitHub API layer -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for github-api.el shape functions, driven entirely by the
;; centralized mock service (no network).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'github-api)
(require 'github-mock)

(ert-deftest github-test-comment-resolvable-p-review-comment ()
  "An inline (review) comment, carrying an `inline' key, is resolvable."
  (should (github-comment-resolvable-p
           '((id . 1) (content (raw . "nit"))
             (inline (path . "a.el") (from . 3) (to . 3))))))

(ert-deftest github-test-comment-resolvable-p-issue-comment ()
  "A plain issue (general discussion) comment has no `inline' key
and is not resolvable -- GitHub has no \"resolve\" concept for it."
  (should-not (github-comment-resolvable-p
               '((id . 2) (content (raw . "lgtm"))))))

(ert-deftest github-test-comment-resolved-p-independent-of-resolvable ()
  "Resolved-p and resolvable-p answer different questions."
  (let ((resolved-review '((inline (path . "a.el") (to . 1))
                           (resolution (user (display_name . "GitHub")))))
        (open-review '((inline (path . "a.el") (to . 1)))))
    (should (github-comment-resolved-p resolved-review))
    (should (github-comment-resolvable-p resolved-review))
    (should-not (github-comment-resolved-p open-review))
    (should (github-comment-resolvable-p open-review))))

(ert-deftest github-test-suggested-reviewers-excludes-self-and-reshapes ()
  "Collaborators are reshaped to the shared reviewer alist shape,
with the authenticated user (\"ada\", per `github-mock--user') excluded."
  (github-mock-with-service
    (should (equal (github-repo-suggested-reviewers "acme/web")
                   '(((uuid . "bea") (display_name . "bea")))))))

(ert-deftest github-test-suggested-reviewers-empty-on-error ()
  "An unreadable collaborator list degrades to nil, not an error."
  (github-mock-with-service
    (cl-letf (((symbol-function 'github-api-paged)
               (lambda (&rest _) (error "boom"))))
      (should (null (github-repo-suggested-reviewers "acme/web"))))))

(ert-deftest github-test-pull-request-id-is-the-repo-scoped-number ()
  "`id' on a returned PR must be the per-repo `number' GitHub's own
endpoints accept, not the global database id -- every `gp-*' caller
(approve, comments, draft-toggle, the detail view's re-fetch, …)
treats `id' as THE identifier to hand back to those endpoints.
`github-mock--pr-1' has id 501 but number 42; a caller using 501
against a real repo-scoped endpoint 404s (see `github--reshape-pr')."
  (github-mock-with-service
    (let ((pr (github-pull-request "acme/web" 42)))
      (should (equal (alist-get 'id pr) 42))
      (should (equal (alist-get 'gh-database-id pr) 501)))))

(ert-deftest github-test-pull-request-async-id-is-the-repo-scoped-number ()
  "The async fetch path reshapes `id' the same way the sync path does.
`github-api-get-async' itself isn't stubbed by the mock (only the
sync `github-api-request'/`github-api-paged' are), so this stubs the
async primitive directly to hand back the sync mock's PR fixture."
  (github-mock-with-service
    (cl-letf (((symbol-function 'github-api-get-async)
               (lambda (path _params callback)
                 (funcall callback (github-mock-request "GET" path)))))
      (let (result)
        (github-pull-request-async "acme/web" 42 (lambda (ok pr) (setq result (cons ok pr))))
        (should (car result))
        (should (equal (alist-get 'id (cdr result)) 42))))))

(ert-deftest github-test-repo-pull-requests-id-is-the-repo-scoped-number ()
  "A repo PR listing also reshapes every entry's `id' to its `number'."
  (github-mock-with-service
    (let ((prs (github-repo-pull-requests "acme/web")))
      (should (equal (mapcar (lambda (pr) (alist-get 'id pr)) prs) '(42 43))))))

(ert-deftest github-test-pull-request-stats-includes-file-list ()
  "`github-pull-request-stats' must populate :file-list -- the changed-
files section in gp-ui.el (`gp--insert-changed-files') reads it off
`gp--detail-stats' and silently renders nothing without it, unlike
Bitbucket's diffstat endpoint whose response IS the per-file list."
  (github-mock-with-service
    (let ((stats (github-pull-request-stats "acme/web" 42)))
      (should (equal (plist-get stats :file-list)
                     '((:path "widget.el" :status "modified" :added 4 :removed 1)))))))

(ert-deftest github-test-set-pull-request-draft-ready-to-draft-uses-graphql ()
  "Converting a ready PR to draft is a real GitHub capability (the web
UI offers it), just GraphQL-only -- `convertPullRequestToDraft', the
mirror of `markPullRequestReadyForReview'.  Must NOT `user-error'."
  (github-mock-with-service
    (github-set-pull-request-draft "acme/web" 42 t)
    (should (cl-some (lambda (c) (string-match-p "convertPullRequestToDraft" (car c)))
                     github-mock-graphql-calls))))

(ert-deftest github-test-set-pull-request-draft-draft-to-ready-uses-graphql ()
  (github-mock-with-service
    (github-set-pull-request-draft "acme/web" 42 nil)
    (should (cl-some (lambda (c) (string-match-p "markPullRequestReadyForReview" (car c)))
                     github-mock-graphql-calls))))

(provide 'github-api-test)
;;; github-api-test.el ends here
