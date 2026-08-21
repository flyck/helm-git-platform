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

(defmacro github-test--with-sync-paged-async (&rest body)
  "Run BODY with `github-api-paged-async' delegating to the sync mock.
`github-api-paged-async' isn't itself stubbed by `github-mock.el'
\(only the sync `github-api-request'/`github-api-paged' are) since it
does real `url-retrieve' I/O even against a mocked host; this makes
it call back immediately using the already-mocked sync path instead."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'github-api-paged-async)
              (lambda (path &optional params callback _max-items)
                (funcall callback t (github-mock-paged path params)))))
     ,@body))

(ert-deftest github-test-pr-review-tally-async-matches-sync ()
  (github-mock-with-service
    (github-test--with-sync-paged-async
      (let ((pr (github-pull-request "acme/web" 42))
            async-result)
        (github-pr-review-tally-async pr (lambda (tally) (setq async-result tally)))
        (should (equal async-result (github-pr-review-tally pr)))))))

(ert-deftest github-test-pr-reviewers-async-shape ()
  "Each reviewer plist has :name/:avatar/:state; :state is a symbol
in `(approved changes pending)', matching `gp-pr-reviewers-async's contract."
  (github-mock-with-service
    (github-test--with-sync-paged-async
      (let ((pr (github-pull-request "acme/web" 42))
            reviewers)
        (github-pr-reviewers-async pr (lambda (r) (setq reviewers r)))
        (should (listp reviewers))
        (dolist (r reviewers)
          (should (plist-member r :name))
          (should (plist-member r :avatar))
          (should (memq (plist-get r :state) '(approved changes pending))))))))

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

(ert-deftest github-test-pr-labels-absent-and-null ()
  "A PR with no `labels' key, or a JSON null, yields nil -- not an error.
GitHub sends `:null' rather than nil for an empty field, which would
break a bare `append'."
  (should-not (github-pr-labels '((id . 43) (title . "no labels here"))))
  (should-not (github-pr-labels '((id . 43) (labels . :null))))
  (should-not (github-pr-labels '((id . 43) (labels . [])))))

(ert-deftest github-test-label-drops-nameless-entries ()
  "A malformed label (no name) is dropped rather than rendered blank."
  (should (equal (github-pr-labels '((labels . [((color . "ff0000"))
                                                ((name . "ok") (color . "00ff00"))])))
                 '((:name "ok" :color "00ff00")))))

(ert-deftest github-test-label-missing-color-is-nil ()
  "A label with no colour keeps its name and reports colour nil,
so the renderer can fall back to a plain face."
  (should (equal (github-pr-labels '((labels . [((name . "plain"))])))
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

(provide 'github-api-test)
;;; github-api-test.el ends here
