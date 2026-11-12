;;; github-api-test.el --- Tests for the GitHub API layer -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for github-api.el shape functions, driven entirely by the
;; centralized mock service (no network).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'github-api)
(require 'github-mock)
(require 'git-platform-github)

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

(defun github-test--pending-review (comment-nodes)
  "A `reviews(states:PENDING...)' GraphQL response holding COMMENT-NODES."
  (let* ((comments `((nodes . ,comment-nodes)))
         (review `((id . "REVIEW_1") (databaseId . 900) (comments . ,comments)))
         (reviews `((nodes ,review)))
         (pr `((reviews . ,reviews)))
         (repo `((pullRequest . ,pr))))
    `((repository . ,repo))))

(defconst github-test--pending-comment-node
  '((id . "NODE_1") (databaseId . 1)
    (body . "x") (path . "a.el") (line . 1)
    (author (login . "ada") (avatarUrl . "u"))
    (createdAt . "2026-01-01T00:00:00Z")
    (url . "https://x")))

(defconst github-test--rest-404
  (list (cons "/comments/1\\'" (lambda (&rest _) (error "Not Found"))))
  "`github-mock-overrides' making comment 1 404 on both REST endpoints,
as a comment still in a PENDING review does.")

(ert-deftest github-test-delete-comment-falls-back-to-review-batch ()
  "A comment id that 404s on both REST endpoints is deleted via GraphQL,
by looking its node id up among the pending review's own comments:
the mutation must carry the node id \"NODE_1\", not the databaseId 1."
  (github-mock-with-service
    (let ((github-mock-overrides github-test--rest-404)
          (pending-review (github-test--pending-review
                           (list github-test--pending-comment-node)))
          (deletes nil))
      (cl-letf (((symbol-function 'github-graphql-request)
                 (lambda (query &optional variables)
                   (cond
                    ((string-match-p "reviews(states:PENDING" query) pending-review)
                    ((string-match-p "deletePullRequestReviewComment" query)
                     (push variables deletes)
                     '((deletePullRequestReviewComment (pullRequestReview (id . "REVIEW_1")))))
                    (t (error "unexpected query: %s" query))))))
        (github-delete-comment "acme/web" 42 1)
        ;; both REST kinds were tried first
        (should (equal (sort (mapcar (lambda (c) (concat (nth 0 c) " " (nth 1 c)))
                                     github-mock-calls)
                             #'string<)
                       '("DELETE /repos/acme/web/issues/comments/1"
                         "DELETE /repos/acme/web/pulls/comments/1")))
        (should (equal deletes '(((id . "NODE_1")))))))))

(ert-deftest github-test-delete-comment-not-in-pending-review-errors ()
  "A comment id absent from every fallback (REST or the pending review)
signals a clear user-error rather than a bare GraphQL failure."
  (github-mock-with-service
    (let ((github-mock-overrides
           (list (cons "/comments/999\\'" (lambda (&rest _) (error "Not Found"))))))
      (cl-letf (((symbol-function 'github-graphql-request)
                 (lambda (&rest _) (github-test--pending-review nil))))
        (should-error (github-delete-comment "acme/web" 42 999) :type 'user-error)))))

(ert-deftest github-test-edit-comment-falls-back-to-review-batch ()
  "Editing a comment still in the pending review updates it via GraphQL,
sending the node id and the new body, and returns the shared pending
comment shape keyed by the databaseId with the node id alongside."
  (github-mock-with-service
    (let ((github-mock-overrides github-test--rest-404)
          (pending-review (github-test--pending-review
                           (list github-test--pending-comment-node)))
          (updates nil))
      (cl-letf (((symbol-function 'github-graphql-request)
                 (lambda (query &optional variables)
                   (cond
                    ((string-match-p "reviews(states:PENDING" query) pending-review)
                    ((string-match-p "updatePullRequestReviewComment" query)
                     (push variables updates)
                     ;; GitHub echoes the comment with the body it stored
                     `((updatePullRequestReviewComment
                        (pullRequestReviewComment
                         (databaseId . 1) (body . ,(alist-get 'body variables))
                         (path . "a.el") (line . 1)
                         (author (login . "ada") (avatarUrl . "u"))
                         (createdAt . "2026-01-01T00:00:00Z") (url . "https://x")))))
                    (t (error "unexpected query: %s" query))))))
        (let ((updated (github-edit-comment "acme/web" 42 1 "new")))
          (should (equal updates '(((id . "NODE_1") (body . "new")))))
          (should (gp-comment-pending-p updated))
          (should (equal (alist-get 'id updated) 1))
          (should (equal (alist-get 'node-id updated) "NODE_1"))
          (should (equal (let-alist updated .inline.path) "a.el")))))))

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

;; NOTE: `github-mock-graphql-calls' is newest-first.
(ert-deftest github-test-set-pull-request-draft-uses-graphql-with-the-pr-node-id ()
  "Both draft directions are GraphQL-only mutations keyed by the PR's
`node_id' (\"PR_42\"), not its number or database id.  Converting a
ready PR to draft is a real GitHub capability (the web UI offers it),
the mirror of `markPullRequestReadyForReview'; neither may `user-error'."
  (github-mock-with-service
    (github-set-pull-request-draft "acme/web" 42 t)
    (let ((call (car github-mock-graphql-calls)))
      (should (string-match-p "convertPullRequestToDraft" (nth 0 call)))
      (should (equal (nth 1 call) '((id . "PR_42")))))
    (setq github-mock-graphql-calls nil)
    (github-set-pull-request-draft "acme/web" 42 nil)
    (should (= (length github-mock-graphql-calls) 1))
    (let ((call (car github-mock-graphql-calls)))
      (should (string-match-p "markPullRequestReadyForReview" (nth 0 call)))
      (should (equal (nth 1 call) '((id . "PR_42")))))))

(defconst github-test--reviews
  '(((id . 1) (state . "CHANGES_REQUESTED")
     (user (login . "bea") (avatar_url . "https://example.com/bea.png")))
    ((id . 2) (state . "CHANGES_REQUESTED")
     (user (login . "cid") (avatar_url . "https://example.com/cid.png")))
    ;; bea's later approval supersedes her earlier change request
    ((id . 3) (state . "APPROVED")
     (user (login . "bea") (avatar_url . "https://example.com/bea.png"))))
  "Raw `/pulls/42/reviews' rows, oldest first, as GitHub lists them.")

(defvar github-test--paged-routes nil
  "Alist of (PATH-REGEXP . VALUE) served ahead of `github-mock-paged'.
VALUE may be a function of (PATH PARAMS) -- e.g. one that signals, to
fake a failed listing.  `github-mock-overrides' covers only single
GETs; this is its paged counterpart, reaching both the sync and the
async paged mock (which share `github-mock-paged').")

(defvar github-test--orig-mock-paged nil)

(defun github-test--routed-paged (path &optional params max-items)
  "`github-mock-paged', consulting `github-test--paged-routes' first."
  (let ((route (cl-find-if (lambda (r) (string-match-p (car r) path))
                           github-test--paged-routes)))
    (if (not route)
        (funcall github-test--orig-mock-paged path params max-items)
      (push (list "GET" path params nil) github-mock-calls)
      (if (functionp (cdr route)) (funcall (cdr route) path params) (cdr route)))))

(defmacro github-test--with-paged-routes (routes &rest body)
  "Run BODY (inside `github-mock-with-service') with paged ROUTES added."
  (declare (indent 1) (debug t))
  `(let ((github-test--paged-routes ,routes)
         (github-test--orig-mock-paged (symbol-function 'github-mock-paged)))
     (cl-letf (((symbol-function 'github-mock-paged) #'github-test--routed-paged))
       ,@body)))

(defmacro github-test--with-reviews (&rest body)
  "Run BODY in the mock with PR 42 carrying `github-test--reviews'.
PR 42 also requests dee, who has not reviewed, and bea, who has.
The reviews route is served by both the sync and async paged mock."
  (declare (indent 0) (debug t))
  `(github-mock-with-service
     (let ((github-mock-overrides
            (list (cons "/pulls/42\\'"
                        (cons '(requested_reviewers
                                ((login . "dee") (avatar_url . "https://example.com/dee.png"))
                                ((login . "bea") (avatar_url . "https://example.com/bea.png")))
                              (assq-delete-all 'requested_reviewers
                                               (copy-alist github-mock--pr-1)))))))
       (github-test--with-paged-routes
           (list (cons "/pulls/42/reviews\\'" github-test--reviews))
         ,@body))))

(ert-deftest github-test-pr-review-tally-latest-review-per-user-wins ()
  "Only each reviewer's latest review counts: bea's CHANGES_REQUESTED is
superseded by her APPROVED, cid requested changes, and dee -- requested
but silent -- is pending.  bea is still listed as requested but has
reviewed, so she is not pending too.  Sync and async agree."
  (github-test--with-reviews
    (let ((pr (github-pull-request "acme/web" 42))
          async-result)
      (should (equal (github-pr-review-tally pr)
                     '(:approved 1 :changes 1 :pending 1)))
      (github-pr-review-tally-async pr (lambda (tally) (setq async-result tally)))
      (should (equal async-result '(:approved 1 :changes 1 :pending 1))))))

(ert-deftest github-test-pr-reviewers-async-shape ()
  "One plist per person with :id/:name/:avatar/:state, :state taken
from their latest review; a requested reviewer with no review is
`pending' with the avatar from `requested_reviewers'."
  (github-test--with-reviews
    (let ((pr (github-pull-request "acme/web" 42))
          (reviewers :unset))
      (github-pr-reviewers-async pr (lambda (r) (setq reviewers r)))
      (should (equal (sort (copy-sequence reviewers)
                           (lambda (a b) (string< (plist-get a :name)
                                                  (plist-get b :name))))
                     '((:id "bea" :name "bea" :avatar "https://example.com/bea.png"
                        :state approved)
                       (:id "cid" :name "cid" :avatar "https://example.com/cid.png"
                        :state changes)
                       (:id "dee" :name "dee" :avatar "https://example.com/dee.png"
                        :state pending)))))))

(ert-deftest github-test-pr-comment-count-sums-issue-and-review-comments ()
  (should (= (gp--pr-comment-count (git-platform-github)
                                   '((comments . 3) (review_comments . 2)))
             5)))

;;;; PR commits ----------------------------------------------------------------

(ert-deftest github-test-commit-entry-normalises-to-bitbucket-shape ()
  "GitHub's nested commit JSON flattens to the same plist Bitbucket yields."
  (let ((entry (github--commit-entry
                '((sha . "1a2b3c4d5e6f")
                  (author (login . "ada"))
                  (commit (message . "add the feature\n\ndetails")
                          (author (name . "Ada Lovelace")
                                  (date . "2026-07-14T09:00:00Z")))))))
    (should (equal (plist-get entry :hash) "1a2b3c4d5e6f"))
    (should (equal (plist-get entry :summary) "add the feature"))
    ;; the linked account login wins over the raw git name
    (should (equal (plist-get entry :author) "ada"))
    (should (equal (plist-get entry :date) "2026-07-14T09:00:00Z"))))

(ert-deftest github-test-commit-entry-falls-back-to-git-author-name ()
  "A commit whose email matches no GitHub account has no `author.login'."
  (let ((entry (github--commit-entry
                '((sha . "deadbeef")
                  (commit (message . "drive-by")
                          (author (name . "Outside Contributor")))))))
    (should (equal (plist-get entry :author) "Outside Contributor"))))

(ert-deftest github-test-pull-request-commits-async-returns-newest-first ()
  "GitHub lists PR commits oldest-first; we invert to match Bitbucket.
A cap must then keep the NEWEST commits, not the oldest ones."
  (let ((oldest-first
         '(((sha . "c1") (commit (message . "first")  (author (name . "A"))))
           ((sha . "c2") (commit (message . "second") (author (name . "A"))))
           ((sha . "c3") (commit (message . "third")  (author (name . "A")))))))
    (cl-letf (((symbol-function 'github-api-paged-async)
               (lambda (_path &optional _params callback _max)
                 (funcall callback t oldest-first))))
      (let (result)
        (github-pull-request-commits-async
         "acme/web" 42 (lambda (cs) (setq result cs)))
        (should (equal (mapcar (lambda (c) (plist-get c :hash)) result)
                       '("c3" "c2" "c1"))))
      ;; capped: the two NEWEST, not the two oldest
      (let (result)
        (github-pull-request-commits-async
         "acme/web" 42 (lambda (cs) (setq result cs)) 2)
        (should (equal (mapcar (lambda (c) (plist-get c :hash)) result)
                       '("c3" "c2")))))))

(ert-deftest github-test-pr-labels-read-off-the-payload ()
  "Labels come from the PR object itself -- no request of their own.
GitHub embeds `labels' in the PR payload, which is what makes a plain
PR re-fetch refresh them; if this ever needed its own endpoint, the
detail view would show stale labels after an edit."
  (github-mock-with-service
    (let* ((github-mock-calls nil)
           (pr (github-pull-request "acme/web" 42))
           (labels (github-pr-labels pr)))
      (should (equal (mapcar (lambda (l) (plist-get l :name)) labels)
                     '("bug" "ui")))
      (should (equal (plist-get (car labels) :color) "d73a4a"))
      ;; exactly the one PR fetch; nothing label-specific
      (should-not (cl-find-if (lambda (c) (string-match-p "labels" (nth 1 c)))
                              github-mock-calls)))))

(ert-deftest github-test-pr-labels-absent ()
  "A PR with no `labels' key, or an empty array, yields nil."
  (should-not (github-pr-labels '((id . 43) (title . "no labels here"))))
  (should-not (github-pr-labels (github--parse-json "{\"id\": 43, \"labels\": []}"))))

(ert-deftest github-test-label-drops-nameless-entries ()
  "A malformed label (no name) is dropped rather than rendered blank."
  (should (equal (github-pr-labels '((labels ((color . "ff0000"))
                                              ((name . "ok") (color . "00ff00")))))
                 '((:name "ok" :color "00ff00")))))

(ert-deftest github-test-label-missing-color-is-nil ()
  "A label with no colour keeps its name and reports colour nil,
so the renderer can fall back to a plain face."
  (should (equal (github-pr-labels '((labels ((name . "plain")))))
                 '((:name "plain" :color nil)))))

(ert-deftest github-test-repo-labels-returns-the-pool-and-caches ()
  "The repo label pool is fetched once and served from cache after."
  (github-mock-with-service
    (let ((github-mock-calls nil))
      (should (equal (mapcar (lambda (l) (plist-get l :name))
                             (github-repo-labels "acme/web"))
                     '("bug" "ui" "chore")))
      (let ((fetches (cl-count-if (lambda (c) (string-match-p "/labels\\'" (nth 1 c)))
                                  github-mock-calls)))
        (should (= fetches 1))
        ;; second call must not re-page the pool
        (github-repo-labels "acme/web")
        (should (= (cl-count-if (lambda (c) (string-match-p "/labels\\'" (nth 1 c)))
                                github-mock-calls)
                   1))))))

(ert-deftest github-test-set-pull-request-labels-puts-the-whole-set ()
  "Setting labels PUTs the complete list to the issues endpoint.
Callers pass the desired end state, so a single PUT both adds and
removes -- no delta requests."
  (github-mock-with-service
    (let ((github-mock-calls nil))
      (should (github-set-pull-request-labels "acme/web" 42 '("bug" "chore")))
      (let ((call (car github-mock-calls)))
        (should (equal (nth 0 call) "PUT"))
        (should (equal (nth 1 call) "/repos/acme/web/issues/42/labels"))
        ;; a vector, as JSON encoding requires -- a list would serialise wrong
        (should (equal (alist-get 'labels (nth 3 call)) ["bug" "chore"]))))))

(ert-deftest github-test-set-pull-request-labels-can-clear-all ()
  "An empty desired set is a legitimate PUT that strips every label."
  (github-mock-with-service
    (let ((github-mock-calls nil))
      (should (github-set-pull-request-labels "acme/web" 42 '()))
      (should (equal (alist-get 'labels (nth 3 (car github-mock-calls))) [])))))

(ert-deftest github-test-set-description-patches-only-the-body ()
  "Editing the body is a plain PATCH -- no title, so nothing else changes.
The Bitbucket backend has to resend the title because its update is a
whole-object PUT; GitHub's is partial, so passing a title here would
be noise at best and an unintended title edit at worst."
  (github-mock-with-service
    (let ((github-mock-calls nil))
      (github-set-pull-request-description "acme/web" 42 "New body" "Some Title")
      (let ((call (car github-mock-calls)))
        (should (equal (nth 0 call) "PATCH"))
        (should (string-suffix-p "/repos/acme/web/pulls/42" (nth 1 call)))
        (should (equal (alist-get 'body (nth 3 call)) "New body"))
        ;; the title argument is accepted for signature parity and dropped
        (should-not (assq 'title (nth 3 call)))))))

(ert-deftest github-test-set-description-clears-with-empty-string ()
  "Clearing sends \"\" rather than nil, which would encode as JSON null."
  (github-mock-with-service
    (let ((github-mock-calls nil))
      (github-set-pull-request-description "acme/web" 42 nil)
      (should (equal (alist-get 'body (nth 3 (car github-mock-calls))) "")))))

;;;; Error logging --------------------------------------------------------------

(ert-deftest github-test-redact-for-log-hides-credentials ()
  "A credential-shaped key is replaced; ordinary fields are kept.
The request body is normally innocuous, but the log buffer is something
users paste into issue reports."
  (let ((out (github--redact-for-log
              '((body . "a comment") (path . "gp-helm.el") (line . 12)
                (token . "ghp_supersecret")))))
    (should (string-match-p "gp-helm" out))
    (should (string-match-p "<redacted>" out))
    (should-not (string-match-p "ghp_supersecret" out))))

(ert-deftest github-test-failed-write-logs-the-request-payload ()
  "A failing write logs what it SENT, not just the response body.
A 422 names the field it rejected (`…thread.line'), which only means
something next to the value actually sent -- without this the log
cannot explain an out-of-diff inline comment, which is exactly the
case that prompted it.  Drives the real `github-api-request' against
a raw HTTP 422 response, so the status line is parsed for real too."
  (let ((gp-log-enabled t)
        (gp-log-buffer-name (generate-new-buffer-name " *gp-test-log*"))
        (github-api-token "t")
        err)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'url-retrieve-synchronously)
                     (lambda (&rest _)
                       (let ((buf (generate-new-buffer " *gh-test-http*")))
                         (with-current-buffer buf
                           (set-buffer-multibyte nil) ;; raw, like url.el
                           (insert "HTTP/1.1 422 Unprocessable Entity\n"
                                   "Content-Type: application/json\n\n"
                                   "{\"message\":\"Validation Failed\"}"))
                         buf))))
            (setq err (should-error
                       (github-api-request "POST" "/repos/acme/web/pulls/7/comments" nil
                                           '((body . "note") (path . "gp-helm.el")
                                             (line . 500))))))
          ;; the status line was parsed and the API's message surfaced
          (should (string-match-p "HTTP 422: Validation Failed"
                                  (error-message-string err)))
          (let ((all (with-current-buffer gp-log-buffer-name (buffer-string))))
            ;; the payload is there, with the values that explain the rejection
            (should (string-match-p "sent:.*gp-helm\\.el" all))
            (should (string-match-p "sent:.*500" all))
            ;; and the response body is still logged too
            (should (string-match-p "body: .*Validation Failed" all))))
      (when (get-buffer gp-log-buffer-name) (kill-buffer gp-log-buffer-name)))))

;;;; Inline comment targets ----------------------------------------------------

(defconst github-test--diff
  (concat "diff --git a/gp-helm.el b/gp-helm.el\n"
          "--- a/gp-helm.el\n"
          "+++ b/gp-helm.el\n"
          "@@ -142,15 +142,60 @@ some context\n"
          "+added\n"
          "@@ -787,12 +832,12 @@ more context\n"
          "+added\n"
          "diff --git a/tests/x-test.el b/tests/x-test.el\n"
          "--- a/tests/x-test.el\n"
          "+++ b/tests/x-test.el\n"
          "@@ -1,0 +1,4 @@\n"
          "+added\n")
  "A diff with two hunks in one file and one in another.")

(ert-deftest github-test-diff-commentable-lines-parses-new-side-ranges ()
  "Hunk headers yield the new-side line ranges, per file."
  (should (equal (github--diff-commentable-lines github-test--diff)
                 '(("gp-helm.el" (142 . 201) (832 . 843))
                   ("tests/x-test.el" (1 . 4))))))

(ert-deftest github-test-diff-commentable-lines-ignores-deleted-files ()
  "A file deleted by the diff has no commentable new side."
  (should-not
   (assoc "gone.el"
          (github--diff-commentable-lines
           (concat "diff --git a/gone.el b/gone.el\n"
                   "--- a/gone.el\n"
                   "+++ /dev/null\n"
                   "@@ -1,3 +0,0 @@\n"
                   "-was here\n")))))

(ert-deftest github-test-inline-target-problem-accepts-a-line-in-a-hunk ()
  "A line inside a hunk is fine, and reports no problem."
  (cl-letf (((symbol-function 'github-pull-request-diff)
             (lambda (&rest _) github-test--diff)))
    (should-not (github-inline-target-problem "acme/web" 7 "gp-helm.el" 150))
    ;; boundaries count as inside
    (should-not (github-inline-target-problem "acme/web" 7 "gp-helm.el" 142))
    (should-not (github-inline-target-problem "acme/web" 7 "gp-helm.el" 201))))

(ert-deftest github-test-inline-target-problem-names-the-available-lines ()
  "A line outside every hunk is refused, and the message says which lines
would work -- the whole point of checking before posting rather than
letting GitHub answer a bare 422."
  (cl-letf (((symbol-function 'github-pull-request-diff)
             (lambda (&rest _) github-test--diff)))
    (let ((msg (github-inline-target-problem "acme/web" 7 "gp-helm.el" 500)))
      (should msg)
      (should (string-match-p "line 500" msg))
      (should (string-match-p "142-201" msg))
      (should (string-match-p "832-843" msg)))))

(ert-deftest github-test-inline-target-problem-names-the-changed-files ()
  "A file the PR does not touch is refused, listing the ones it does."
  (cl-letf (((symbol-function 'github-pull-request-diff)
             (lambda (&rest _) github-test--diff)))
    (let ((msg (github-inline-target-problem "acme/web" 7 "untouched.el" 10)))
      (should msg)
      (should (string-match-p "untouched\\.el" msg))
      (should (string-match-p "gp-helm\\.el" msg)))))

(ert-deftest github-test-inline-target-problem-defers-when-diff-unreadable ()
  "An unreadable diff must not block a comment: let the API decide.
Refusing locally on a failed fetch would turn a transient error into a
hard block on commenting at all."
  (cl-letf (((symbol-function 'github-pull-request-diff) (lambda (&rest _) nil)))
    (should-not (github-inline-target-problem "acme/web" 7 "anything.el" 1))))

;;;; Title ------------------------------------------------------------------------

(ert-deftest github-test-set-title-patches-only-the-title ()
  "Retitling sends just the title -- a PATCH leaves the body alone.
Unlike Bitbucket there is nothing to resend, and sending the body would
risk overwriting a description edited elsewhere in the meantime."
  (github-mock-with-service
    (let ((github-mock-calls nil))
      (github-set-pull-request-title "acme/web" 42 "A better title")
      (let ((call (car github-mock-calls)))
        (should (equal (nth 0 call) "PATCH"))
        (should (string-suffix-p "/pulls/42" (nth 1 call)))
        (should (equal (alist-get 'title (nth 3 call)) "A better title"))
        (should-not (assq 'body (nth 3 call)))))))

(ert-deftest github-test-set-title-rejects-an-empty-title ()
  "An empty title fails locally rather than as a remote 422."
  (github-mock-with-service
    (should-error (github-set-pull-request-title "acme/web" 42 ""))
    (should-error (github-set-pull-request-title "acme/web" 42 nil))))

;;;; Merging ---------------------------------------------------------------------

(ert-deftest github-test-merge-puts-method-and-message ()
  "Merging PUTs `merge_method' and the commit message."
  (github-mock-with-service
    (let ((github-mock-calls nil))
      (github-merge-pull-request "acme/web" 42 "squash" "ship it")
      (let ((call (car github-mock-calls)))
        (should (equal (nth 0 call) "PUT"))
        (should (string-suffix-p "/pulls/42/merge" (nth 1 call)))
        (should (equal (alist-get 'merge_method (nth 3 call)) "squash"))
        (should (equal (alist-get 'commit_message (nth 3 call)) "ship it"))))))

(ert-deftest github-test-merge-ignores-close-source-branch ()
  "GitHub has no per-merge branch deletion, so the argument is dropped.
Deletion follows the repository's `delete_branch_on_merge'; sending
anything here would be a silently ignored field at best."
  (github-mock-with-service
    (let ((github-mock-calls nil))
      (github-merge-pull-request "acme/web" 42 "merge" nil t)
      (let ((data (nth 3 (car github-mock-calls))))
        (should-not (assq 'close_source_branch data))
        (should-not (assq 'delete_branch data))))))

(ert-deftest github-test-merge-rejects-a-bitbucket-strategy ()
  "Bitbucket's tokens are not GitHub's; `fast_forward' is not a method."
  (github-mock-with-service
    (should-error (github-merge-pull-request "acme/web" 42 "fast_forward"))))

(ert-deftest github-test-merge-strategies-come-from-repo-settings ()
  "Only the methods the repository permits are offered."
  (github-mock-with-service
    (let ((github-mock-overrides
           '(("/repos/acme/web\\'" . ((allow_merge_commit . t)
                                     (allow_squash_merge)
                                     (allow_rebase_merge . t))))))
      (let ((info (github-pull-request-merge-strategies "acme/web" 42)))
        (should (equal (car info) '("merge" "rebase")))
        ;; the default is the first permitted one, GitHub's own order
        (should (equal (cdr info) "merge"))))
    ;; a repo that permits no method at all offers nothing, not an empty pair
    (let ((github-mock-overrides
           '(("/repos/acme/web\\'" . ((allow_merge_commit)
                                     (allow_squash_merge)
                                     (allow_rebase_merge))))))
      (should-not (github-pull-request-merge-strategies "acme/web" 42)))))

(ert-deftest github-test-mergeability-distinguishes-dirty-from-unknown ()
  "A conflict and \"not computed yet\" must not be confused.
`mergeable' is lazy -- GitHub answers null until it has judged -- and
this package's parser maps both null and false to nil, so `mergeable'
alone cannot tell them apart.  `mergeable_state' is what decides:
\"dirty\" is a real conflict, anything else unproven.  Getting this
backwards would refuse to merge a perfectly clean PR."
  (github-mock-with-service
    (cl-labels ((probe (fields)
                  (let ((github-mock-overrides
                         `(("/pulls/42\\'" . ,fields))))
                    (github-pull-request-mergeability "acme/web" 42))))
      ;; a stated conflict -> not mergeable
      (should (equal (probe '((mergeable) (mergeable_state . "dirty")))
                     '(nil . "dirty")))
      ;; explicitly mergeable
      (should (equal (probe '((mergeable . t) (mergeable_state . "clean")))
                     '(t . "clean")))
      ;; not judged yet -> unknown, which must NOT block a merge
      (should (equal (probe '((mergeable) (mergeable_state . "unknown")))
                     '(unknown . "unknown")))
      ;; GitHub judged it mergeable, merely blocked (review required) -> t
      (should (equal (probe '((mergeable . t) (mergeable_state . "blocked")))
                     '(t . "blocked")))
      ;; null `mergeable' with a non-conflict state -> unknown, not a conflict
      (should (equal (probe '((mergeable) (mergeable_state . "blocked")))
                     '(unknown . "blocked")))
      (should (equal (probe '((mergeable) (mergeable_state . "behind")))
                     '(unknown . "behind"))))))

;;;; Pipeline identity ----------------------------------------------------------

(ert-deftest github-test-pipeline-id-ignores-a-bitbucket-uuid ()
  "On GitHub a run is keyed by `id' and has no `uuid'; a `uuid' field
must not be mistaken for the id.  `gp-pipeline.el' used to read `uuid'
directly, so every steps/stop/log lookup got nil and the detail view
showed \"(no steps)\" for every run.  The positive per-backend cases
live in `gp-test-pipeline-id-differs-per-backend'."
  (let ((git-platform-current-backend (git-platform-github)))
    (should-not (gp-pipeline-id '((uuid . "{bitbucket-shaped}"))))))

;;;; Pipeline step log line classification --------------------------------------

(ert-deftest github-test-pipeline-step-log-classify-line-group ()
  (should (equal (github-pipeline-step-log-classify-line
                  "2026-08-26T13:49:56.1234567Z ##[group]Run npm test")
                 '(group . "Run npm test")))
  (should (equal (github-pipeline-step-log-classify-line
                  "2026-08-26T13:50:14.7654321Z ##[endgroup]")
                 '(group . ""))))

(ert-deftest github-test-pipeline-step-log-classify-line-error-and-warning ()
  (should (equal (github-pipeline-step-log-classify-line
                  "2026-08-26T13:50:15.0000000Z ##[error]Process completed with exit code 1.")
                 '(error . "Process completed with exit code 1.")))
  (should (equal (car (github-pipeline-step-log-classify-line
                       "2026-08-26T13:50:15.0000000Z ##[warning]deprecated action version"))
                 'warning))
  (should (equal (car (github-pipeline-step-log-classify-line
                       "2026-08-26T13:50:15.0000000Z ##[notice]using cache"))
                 'warning)))

(ert-deftest github-test-pipeline-step-log-classify-line-strips-timestamp-only ()
  "An ordinary output line just loses its timestamp prefix, no marker."
  (should (equal (github-pipeline-step-log-classify-line
                  "2026-08-26T13:49:56.2345678Z hello from the job")
                 '(nil . "hello from the job"))))

(ert-deftest github-test-pipeline-step-log-classify-line-tolerates-no-timestamp ()
  "A line without the expected timestamp prefix still classifies, unmangled."
  (should (equal (github-pipeline-step-log-classify-line "plain line, no prefix")
                 '(nil . "plain line, no prefix"))))

;;;; Reactions ----------------------------------------------------------------

(ert-deftest github-test-reaction-base-differs-per-comment-kind ()
  "An inline review comment and a general comment need different bases.
Their ids come from separate sequences, so the wrong collection would
404 or -- worse -- hit an unrelated comment that happens to share the
number."
  (should (equal (github--reaction-base "acme/web" '((id . 7) (inline (path . "a.el"))))
                 "/repos/acme/web/pulls/comments/7/reactions"))
  (should (equal (github--reaction-base "acme/web" '((id . 7)))
                 "/repos/acme/web/issues/comments/7/reactions")))

(ert-deftest github-test-add-reaction-posts-content ()
  "Adding a reaction POSTs just the content token."
  (github-mock-with-service
    (let ((github-mock-calls nil))
      (github-add-comment-reaction "acme/web" '((id . 7)) "+1")
      (let ((call (car github-mock-calls)))
        (should (equal (nth 0 call) "POST"))
        (should (string-suffix-p "/issues/comments/7/reactions" (nth 1 call)))
        (should (equal (alist-get 'content (nth 3 call)) "+1"))))))

(ert-deftest github-test-add-reaction-rejects-unknown-content ()
  "A token GitHub does not accept fails locally, not with a remote 422."
  (github-mock-with-service
    (should-error (github-add-comment-reaction "acme/web" '((id . 7)) "thumbs_up"))))

(ert-deftest github-test-add-reaction-is-idempotent ()
  "Adding the same reaction twice leaves exactly one.
GitHub answers 200 (rather than creating a duplicate) when the user
already holds that reaction, so callers can toggle blindly."
  (github-mock-with-service
    (github-add-comment-reaction "acme/web" '((id . 7)) "heart")
    (github-add-comment-reaction "acme/web" '((id . 7)) "heart")
    (should (= (length (github-comment-reactions "acme/web" '((id . 7)))) 1))))

(ert-deftest github-test-user-can-hold-several-distinct-reactions ()
  "Unlike a binary Like, GitHub allows many reactions per user per comment."
  (github-mock-with-service
    (dolist (c '("+1" "heart" "rocket"))
      (github-add-comment-reaction "acme/web" '((id . 7)) c))
    (should (equal (sort (mapcar (lambda (r) (alist-get 'content r))
                                (github-comment-reactions "acme/web" '((id . 7))))
                        #'string<)
                   '("+1" "heart" "rocket")))))

(ert-deftest github-test-reaction-counts-ride-along-on-comments ()
  "Comments carry per-emoji counts, so drawing them needs no request.
GitHub ships a `reactions' object with every comment; only the reactor
names need the reactions endpoint."
  (should (equal (github--reaction-counts
                  '((reactions (total_count . 3) (\+1 . 2) (\-1 . 0)
                               (heart . 1) (rocket . 0))))
                 '(("+1" . 2) ("heart" . 1))))
  ;; no reactions at all -> nothing, so the renderer inserts nothing
  (should-not (github--reaction-counts '((reactions (total_count . 0) (\+1 . 0)))))
  (should-not (github--reaction-counts '((id . 1)))))

(ert-deftest github-test-reshaped-comments-carry-reaction-counts ()
  "Both comment kinds expose the counts under the shared key."
  (should (equal (alist-get 'reaction-counts
                            (github--reshape-issue-comment
                             '((id . 1) (body . "x") (reactions (\+1 . 2)))))
                 '(("+1" . 2))))
  (should (equal (alist-get 'reaction-counts
                            (github--reshape-review-comment
                             '((id . 2) (body . "y") (reactions (heart . 1)))))
                 '(("heart" . 1)))))

(ert-deftest github-test-graphql-reaction-tokens-map-to-rest ()
  "GraphQL's enum spells the thumbs differently from REST.
`THUMBS_UP' must become `+1', or the viewer's own reaction would never
match the counts keyed by REST tokens."
  (should (equal (github--reaction-token "THUMBS_UP") "+1"))
  (should (equal (github--reaction-token "THUMBS_DOWN") "-1"))
  (should (equal (github--reaction-token "HEART") "heart"))
  ;; an enum value we don't know about still degrades to something usable
  (should (equal (github--reaction-token "SPARKLES") "sparkles")))

(ert-deftest github-test-reshaped-comments-carry-viewer-reactions ()
  "A comment says which reactions are the viewer's own.
REST ships counts but no `viewerHasReacted', so this comes from the
GraphQL prefetch cache -- without it the UI cannot show that a click
would REMOVE your reaction rather than add one."
  (let ((github--viewer-reactions (make-hash-table :test 'eql)))
    (puthash 1 '("+1" "heart") github--viewer-reactions)
    (should (equal (alist-get 'reaction-mine
                              (github--reshape-issue-comment
                               '((id . 1) (body . "x") (reactions (\+1 . 2)))))
                   '("+1" "heart")))
    ;; a comment nobody reacted to as the viewer gets nil, not a stale hit
    (should-not (alist-get 'reaction-mine
                           (github--reshape-issue-comment
                            '((id . 99) (body . "y")))))))

(ert-deftest github-test-reactions-expose-user-as-uuid ()
  "A reaction's reactor must be readable as `uuid', not only `login'.
`gp-user-uuid' returns a login on this backend and every other comment
accessor exposes identity as `uuid'; a reaction that only carried
`login' made \"is this mine?\" read nil and silently turned the toggle
into add-only."
  (github-mock-with-service
    (github-add-comment-reaction "acme/web" '((id . 7)) "+1")
    (let ((r (car (github-comment-reactions "acme/web" '((id . 7))))))
      (should (equal (let-alist r .user.uuid) (github-user-login)))
      ;; login is kept too, for anything reading GitHub's own field name
      (should (equal (let-alist r .user.login) (github-user-login))))))

(ert-deftest github-test-remove-reaction-deletes-by-reaction-id ()
  "Removal needs the reaction's own id, so it is looked up first.
The DELETE route is keyed on the reaction id, not the content -- a
DELETE aimed at the content would 404."
  (github-mock-with-service
    (github-add-comment-reaction "acme/web" '((id . 7)) "+1")
    (let ((github-mock-calls nil))
      (should (github-remove-comment-reaction "acme/web" '((id . 7)) "+1"))
      (let ((del (seq-find (lambda (c) (equal (nth 0 c) "DELETE")) github-mock-calls)))
        (should del)
        (should (string-match-p "/reactions/[0-9]+\\'" (nth 1 del)))))
    (should (null (github-comment-reactions "acme/web" '((id . 7)))))))

(ert-deftest github-test-remove-reaction-absent-is-nil-not-an-error ()
  "Removing one the user never added reports nil rather than signalling."
  (github-mock-with-service
    (should-not (github-remove-comment-reaction "acme/web" '((id . 7)) "eyes"))))

(ert-deftest github-test-remove-reaction-only-removes-your-own ()
  "Someone else's identical reaction is left alone."
  (github-mock-with-service
    ;; a row owned by another login, seeded directly
    (setq github-mock--reactions
          (list (list 4242 "/repos/acme/web/issues/comments/7/reactions" "+1" "bea")))
    (should-not (github-remove-comment-reaction "acme/web" '((id . 7)) "+1"))
    (should (= (length (github-comment-reactions "acme/web" '((id . 7)))) 1))))

;;;; Async workspace PR listing ----------------------------------------------
;;
;; Regression coverage for a real bug: the async listing was first written as
;; the SYNCHRONOUS scan wrapped in `run-at-time 0'.  That looks async but is
;; not: the search and every per-hit PR fetch still block, so Emacs never
;; reaches redisplay and the caller's spinner stays invisible for the whole
;; multi-second scan.  These pin down that BOTH stages go through the async
;; API -- if either reverts to a blocking call, the stubs here are bypassed
;; and the test fails rather than silently freezing the UI again.

(defvar github-test--blocking-calls nil
  "Paths requested through a synchronous choke point during an async test.")

(defun github-test--pr-numbered (_method path &rest _)
  "The mock's PR fixture, renumbered to the number at the end of PATH."
  (let ((n (string-to-number (car (last (split-string path "/"))))))
    (cons (cons 'number n)
          (assq-delete-all 'number (copy-alist github-mock--pr-1)))))

(defmacro github-test--with-async-workspace (search-hits &rest body)
  "Run BODY in the mock with the issue search answering SEARCH-HITS.
Every `/pulls/N' GET serves the PR fixture renumbered to N.  The sync
choke points `github-api-request'/`github-api-paged' are trip-wired:
the async mock reaches the fixtures without them, so anything landing
in `github-test--blocking-calls' is a blocking call the async path
should never make."
  (declare (indent 1) (debug t))
  `(github-mock-with-service
     (let ((github-test--blocking-calls nil)
           (github-mock-overrides
            (list (cons "/pulls/[0-9]+\\'" #'github-test--pr-numbered))))
       (github-test--with-paged-routes
           (list (cons "\\`/search/issues\\'" ,search-hits))
         (cl-letf (((symbol-function 'github-api-request)
                    (lambda (_method path &rest _) (push path github-test--blocking-calls) nil))
                   ((symbol-function 'github-api-paged)
                    (lambda (path &rest _) (push path github-test--blocking-calls) nil)))
           ,@body)))))

(defconst github-test--two-hits
  '(((number . 7) (repository_url . "https://api.github.com/repos/acme/web"))
    ((number . 9) (repository_url . "https://api.github.com/repos/acme/api")))
  "Two search hits in two repos, in search order.")

(ert-deftest github-test-workspace-prs-async-uses-only-async-endpoints ()
  "The async listing never falls back to a blocking search or PR fetch,
and each hit is fetched in full from its own repo and reshaped."
  (github-test--with-async-workspace github-test--two-hits
    (let (result)
      (github-workspace-pull-requests-async
       (lambda (ok prs) (setq result (list ok prs)))
       "ada")
      (should (car result))
      ;; reshaped: `id' is the repo-scoped number, the full PR's title kept
      (should (equal (mapcar (lambda (p) (alist-get 'id p)) (cadr result)) '(7 9)))
      (should (equal (alist-get 'title (car (cadr result))) "Add the widget toggle"))
      (should (equal (sort (mapcar #'cadr (seq-filter (lambda (c) (string-match-p "/pulls/" (cadr c)))
                                                      github-mock-calls))
                           #'string<)
                     '("/repos/acme/api/pulls/9" "/repos/acme/web/pulls/7")))
      ;; the whole point: no blocking call was made
      (should-not github-test--blocking-calls))))

(ert-deftest github-test-workspace-prs-async-preserves-search-order ()
  "Out-of-order async completions still yield the search's ordering."
  (github-test--with-async-workspace
      '(((number . 1) (repository_url . "https://api.github.com/repos/o/a"))
        ((number . 2) (repository_url . "https://api.github.com/repos/o/b"))
        ((number . 3) (repository_url . "https://api.github.com/repos/o/c")))
    (let ((pending '()) result)
      ;; defer each PR GET instead of answering inline, so we control the order
      (cl-letf (((symbol-function 'github-api-get-async)
                 (lambda (path params callback)
                   (push (lambda () (github-mock-get-async path params callback))
                         pending))))
        (github-workspace-pull-requests-async
         (lambda (ok prs) (setq result (and ok (mapcar (lambda (p) (alist-get 'id p)) prs))))
         "ada")
        (should (= (length pending) 3))
        (should-not result)
        ;; `pending' is newest-first: complete in reverse arrival order
        (mapc #'funcall pending)
        (should (equal result '(1 2 3)))))))

(ert-deftest github-test-workspace-prs-async-drops-failed-fetches ()
  "A PR fetch that fails is skipped, not fatal to the whole list."
  (github-test--with-async-workspace github-test--two-hits
    (let ((github-mock-overrides
           (cons (cons "/acme/api/pulls/9\\'" (lambda (&rest _) (error "HTTP 404")))
                 github-mock-overrides))
          result)
      (github-workspace-pull-requests-async
       (lambda (ok prs) (setq result (list ok prs))) "ada")
      (should (car result))
      (should (equal (mapcar (lambda (p) (alist-get 'id p)) (cadr result)) '(7))))))

(ert-deftest github-test-workspace-prs-async-empty-search-completes ()
  "An empty search still calls back exactly once, with an empty list."
  (github-test--with-async-workspace '()
    (let ((calls 0) result)
      (github-workspace-pull-requests-async
       (lambda (ok prs) (setq calls (1+ calls) result (list ok prs))) "ada")
      (should (= calls 1))
      (should (equal result '(t nil))))))

(ert-deftest github-test-workspace-prs-async-search-failure-reports-not-ok ()
  "A failed search reports OK nil rather than an empty success."
  (github-test--with-async-workspace (lambda (&rest _) (error "HTTP 422"))
    (let (result)
      (github-workspace-pull-requests-async
       (lambda (ok prs) (setq result (list ok prs))) "ada")
      (should (equal result '(nil nil))))))

;;;; Async comments and stats/diff -------------------------------------------
;;
;; Same class of regression as the async listing above: these were the
;; SYNCHRONOUS fetches wrapped in `run-at-time', which blocks for the whole
;; ~1s of a real detail load.  The stubs assert the blocking twins are never
;; reached.

(ert-deftest github-test-comments-async-uses-only-async-endpoints ()
  "The async comment fetch reaches no blocking endpoint."
  (let ((blocking nil) result)
    (cl-letf (((symbol-function 'github-graphql-request-async)
               (lambda (_q _v callback) (funcall callback t nil)))
              ((symbol-function 'github-api-paged-async)
               (lambda (path _params callback &optional _max)
                 (funcall callback t
                          (if (string-match-p "issues" path)
                              (list '((id . 1) (body . "general")
                                      (user (login . "ada"))))
                            (list '((id . 2) (body . "inline")
                                    (path . "a.el")
                                    (user (login . "bob"))))))))
              ((symbol-function 'github-graphql-request)
               (lambda (&rest _) (push 'graphql blocking) nil))
              ((symbol-function 'github-api-paged)
               (lambda (&rest _) (push 'paged blocking) nil)))
      (github-pull-request-comments-async
       "acme/web" 5 (lambda (ok comments) (setq result (list ok (length comments)))))
      (should (equal result '(t 2)))
      (should-not blocking))))

(ert-deftest github-test-comments-async-survives-graphql-failure ()
  "GraphQL is optional enrichment: its failure still yields REST comments.
Both REST listings land -- the general issue comment without an
`inline' key, the review comment with its own file path."
  (github-mock-with-service
    (cl-letf (((symbol-function 'github-graphql-request-async)
               (lambda (_q _v callback) (funcall callback nil nil))))
      (let (result)
        (github-pull-request-comments-async
         "acme/web" 42 (lambda (ok comments) (setq result (list ok comments))))
        (should (car result))
        (let ((by-id (mapcar (lambda (c) (cons (alist-get 'id c) c)) (cadr result))))
          (should (equal (sort (mapcar #'car by-id) #'<) '(9001 9002)))
          (should-not (alist-get 'inline (alist-get 9001 by-id)))
          (should (equal (let-alist (alist-get 9002 by-id) .inline.path) "widget.el")))))))

(ert-deftest github-test-comments-async-reshapes-after-graphql-lands ()
  "Resolution markers survive GraphQL answering AFTER the REST comments.
The reshapers read `github--resolved-thread-comment-ids', so reshaping
per-response instead of at the end would drop the marker."
  (let ((github--resolved-thread-comment-ids (make-hash-table :test 'eql))
        graphql-cb result)
    (cl-letf (((symbol-function 'github-graphql-request-async)
               (lambda (_q _v callback) (setq graphql-cb callback)))
              ((symbol-function 'github-api-paged-async)
               (lambda (path _params callback &optional _max)
                 (funcall callback t
                          (if (string-match-p "issues" path)
                              nil
                            (list '((id . 42) (body . "inline")
                                    (path . "a.el")
                                    (user (login . "bob")))))))))
      (github-pull-request-comments-async
       "acme/web" 5 (lambda (_ok comments) (setq result comments)))
      ;; REST already answered; nothing delivered yet
      (should-not result)
      ;; now GraphQL reports comment 42 as resolved
      (funcall graphql-cb t
               '((repository (pullRequest (reviewThreads
                                           (nodes . (((isResolved . t)
                                                      (comments (nodes . (((databaseId . 42))))))))))))) 
      (should (= (length result) 1))
      (should (alist-get 'resolution (car result))))))

(ert-deftest github-test-stats-async-uses-pr-totals-without-refetch ()
  "Stats come off the supplied PR; only the file list is fetched."
  (let ((blocking nil) result)
    (cl-letf (((symbol-function 'github-api-paged-async)
               (lambda (_path _params callback &optional _max)
                 (funcall callback t (list '((filename . "a.el") (status . "modified")
                                             (additions . 3) (deletions . 1))))))
              ((symbol-function 'github-pull-request)
               (lambda (&rest _) (push 'pull-request blocking) nil))
              ((symbol-function 'github-api-paged)
               (lambda (&rest _) (push 'paged blocking) nil)))
      (github-pull-request-stats-async
       "acme/web" 5
       '((changed_files . 1) (additions . 3) (deletions . 1) (commits . 2))
       (lambda (stats) (setq result stats)))
      (should (= (plist-get result :files) 1))
      (should (= (plist-get result :commits) 2))
      (should (= (length (plist-get result :file-list)) 1))
      (should-not blocking))))

(ert-deftest github-test-stats-async-tolerates-failed-file-listing ()
  "A failed /files listing still yields the PR's totals."
  (let (result)
    (cl-letf (((symbol-function 'github-api-paged-async)
               (lambda (_path _params callback &optional _max) (funcall callback nil nil))))
      (github-pull-request-stats-async
       "acme/web" 5 '((changed_files . 4) (commits . 2))
       (lambda (stats) (setq result stats)))
      (should (= (plist-get result :files) 4))
      (should-not (plist-get result :file-list)))))

(ert-deftest github-test-diff-async-uses-raw-async-endpoint ()
  "The async diff goes through the raw async fetcher, not the blocking one."
  (let ((blocking nil) result)
    (cl-letf (((symbol-function 'github-api-get-raw-async)
               (lambda (_path callback &optional _headers) (funcall callback "diff-text")))
              ((symbol-function 'github-api-request-raw)
               (lambda (&rest _) (push 'raw blocking) "")))
      (github-pull-request-diff-async
       "acme/web" 5 nil nil (lambda (text) (setq result text)))
      (should (equal result "diff-text"))
      (should-not blocking))))

(ert-deftest github-test-repo-branches-sorted-names ()
  "Extracts just the name field of each listed branch, alphabetically."
  (github-mock-with-service
    (cl-letf (((symbol-function 'github-api-paged)
               (lambda (&rest _)
                 '(((name . "main")) ((name . "develop")) ((name . "release/1.0"))))))
      (should (equal (github-repo-branches "acme/web")
                     '("develop" "main" "release/1.0"))))))

(ert-deftest github-test-repo-branches-empty-on-error ()
  "A failed branch listing yields nil, not an error."
  (github-mock-with-service
    (cl-letf (((symbol-function 'github-api-paged)
               (lambda (&rest _) (error "boom"))))
      (should (null (github-repo-branches "acme/web"))))))

(ert-deftest github-test-normalize-newlines-crlf-and-lone-cr ()
  "CRLF and lone CR both collapse to LF; non-strings pass through.
GitHub's web textareas submit CRLF per the HTML form spec, and the
REST API echoes the body back verbatim."
  (should (equal (github--normalize-newlines "a\r\nb\r\n\r\nc") "a\nb\n\nc"))
  (should (equal (github--normalize-newlines "a\rb") "a\nb"))
  ;; already-LF text (what Bitbucket sends) is untouched
  (should (equal (github--normalize-newlines "a\nb") "a\nb"))
  (should (equal (github--normalize-newlines "") ""))
  (should (eq (github--normalize-newlines nil) nil)))

(ert-deftest github-test-reshape-pr-normalizes-body-crlf ()
  "A PR body typed in GitHub's web UI loses its CRLFs at the boundary,
and the caller's own alist is left unmutated."
  (let* ((raw '((id . 501) (number . 42)
                (body . "Adds the toggle.\r\n\r\n- [x] tests\r\n")))
         (out (github--reshape-pr raw)))
    (should (equal (alist-get 'body out) "Adds the toggle.\n\n- [x] tests\n"))
    ;; reshaping must not rewrite the object it was handed
    (should (equal (alist-get 'body raw) "Adds the toggle.\r\n\r\n- [x] tests\r\n"))))

(ert-deftest github-test-comment-bodies-normalize-crlf ()
  "Issue and review comment bodies are web-authored too, so both
reshapers strip CRLF before the shared `content.raw' accessor sees it."
  (let ((issue (github--reshape-issue-comment
                '((id . 9001) (body . "line one\r\nline two")
                  (user (login . "bea")))))
        (review (github--reshape-review-comment
                 '((id . 9002) (body . "nit:\r\n\r\nrename this")
                   (path . "widget.el") (line . 10)
                   (user (login . "bea"))))))
    (should (equal (let-alist issue .content.raw) "line one\nline two"))
    (should (equal (let-alist review .content.raw) "nit:\n\nrename this"))))

(ert-deftest github-test-commit-message-normalizes-crlf ()
  "Commit messages written in GitHub's web editor carry CRLF as well."
  (github-mock-with-service
    (let ((github-mock-overrides
           '(("/commits/abc123\\'" . ((commit (message . "subject\r\n\r\nbody line\r\n")))))))
      (should (equal (github-commit-message "acme/web" "abc123")
                     "subject\n\nbody line\n")))))
(ert-deftest github-test-build-states-merge-status-and-checks ()
  "Both CI channels are consulted, so a repo using either reports state."
  (github-mock-with-service
   (let ((states (github-commit-build-states "acme/web" "abc123")))
     ;; the legacy commit status AND the check run both land in the list
     (should (equal states '("SUCCESSFUL" "SUCCESSFUL")))
     (should (eq (gp-build-states-summary states) 'successful)))))

(ert-deftest github-test-check-conclusion-translation ()
  "Incomplete runs are INPROGRESS; neutral/skipped are not verdicts."
  (should (equal (github--check-conclusion->bitbucket "completed" "success") "SUCCESSFUL"))
  (should (equal (github--check-conclusion->bitbucket "completed" "failure") "FAILED"))
  (should (equal (github--check-conclusion->bitbucket "completed" "timed_out") "FAILED"))
  (should (equal (github--check-conclusion->bitbucket "completed" "cancelled") "STOPPED"))
  ;; still running: conclusion is null, status decides
  (should (equal (github--check-conclusion->bitbucket "in_progress" nil) "INPROGRESS"))
  (should (equal (github--check-conclusion->bitbucket "queued" nil) "INPROGRESS"))
  ;; a skipped matrix leg must not drag a green bubble to grey
  (should (null (github--check-conclusion->bitbucket "completed" "skipped")))
  (should (null (github--check-conclusion->bitbucket "completed" "neutral")))
  (should (eq (gp-build-states-summary
               (delq nil (list (github--check-conclusion->bitbucket "completed" "success")
                               (github--check-conclusion->bitbucket "completed" "skipped"))))
              'successful)))

(ert-deftest github-test-empty-combined-status-is-not-a-running-build ()
  "A repo posting no commit statuses answers pending with total_count 0.
That is missing data, not CI in flight: reporting INPROGRESS would
strand the helm bubble on blue forever.  With no check runs either,
there are no states at all, so the bubble stays blank."
  (cl-letf (((symbol-function 'github-api-request)
             (lambda (_m path &rest _)
               (if (string-suffix-p "/status" path)
                   '((state . "pending") (total_count . 0) (statuses . ()))
                 '((total_count . 0) (check_runs . ()))))))
    (should (null (github-commit-build-states "acme/web" "abc123")))
    (should (null (gp-build-states-summary
                   (github-commit-build-states "acme/web" "abc123"))))))

(ert-deftest github-test-checks-only-repo-reports-failure ()
  "Actions-only repo (no commit statuses): the failing check still wins.
This is the case that showed no bubble at all before check runs were
consulted -- e.g. a repo whose CI is purely GitHub Actions."
  (cl-letf (((symbol-function 'github-api-request)
             (lambda (_m path &rest _)
               (if (string-suffix-p "/status" path)
                   '((state . "pending") (total_count . 0) (statuses . ()))
                 '((total_count . 3)
                   (check_runs . (((status . "completed") (conclusion . "success"))
                                  ((status . "completed") (conclusion . "skipped"))
                                  ((status . "completed") (conclusion . "failure")))))))))
    (should (eq (gp-build-states-summary
                 (github-commit-build-states "acme/web" "abc123"))
                'failed))))

(ert-deftest github-test-checks-only-repo-still-running ()
  "An Actions-only repo mid-run reports running, not blank."
  (cl-letf (((symbol-function 'github-api-request)
             (lambda (_m path &rest _)
               (if (string-suffix-p "/status" path)
                   '((state . "pending") (total_count . 0) (statuses . ()))
                 '((total_count . 2)
                   (check_runs . (((status . "completed") (conclusion . "success"))
                                  ((status . "in_progress") (conclusion . nil)))))))))
    (should (eq (gp-build-states-summary
                 (github-commit-build-states "acme/web" "abc123"))
                'running))))

(provide 'github-api-test)
;;; github-api-test.el ends here
