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
case that prompted it.  Drives the real `github-api-request' with a
stubbed 422 transport."
  (let ((logged '()))
    (cl-letf (((symbol-function 'gp-log-error)
               (lambda (fmt &rest args) (push (apply #'format fmt args) logged)))
              ;; a canned 422, shaped like GitHub's
              ((symbol-function 'github--split-response)
               (lambda (&rest _)
                 (list 422 nil "{\"message\":\"Validation Failed\"}")))
              ((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _) (generate-new-buffer " *stub*")))
              ((symbol-function 'github-api-token-value) (lambda () "t")))
      (should-error
       (github-api-request "POST" "/repos/acme/web/pulls/7/comments" nil
                           '((body . "note") (path . "gp-helm.el") (line . 500)))))
    (let ((all (mapconcat #'identity logged " ")))
      ;; the payload is there, with the values that explain the rejection
      (should (string-match-p "sent:" all))
      (should (string-match-p "gp-helm\\.el" all))
      (should (string-match-p "500" all))
      ;; and the response body is still logged too
      (should (string-match-p "Validation Failed" all)))))

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

(provide 'github-api-test)
;;; github-api-test.el ends here
