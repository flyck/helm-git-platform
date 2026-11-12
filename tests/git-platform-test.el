;;; git-platform-test.el --- Protocol conformance tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Protocol conformance: the `gp-' protocol driven through each
;; backend's mock (`bitbucket-mock-with-service' /
;; `github-mock-with-service') with exact expected values, plus the
;; shared helpers (cache, diff, ticket links) in git-platform.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-platform)
(require 'git-platform-bitbucket)
(require 'bitbucket-mock)
(require 'git-platform-github)
(require 'github-mock)

;; The conformance tests drive one protocol through each backend's mock
;; and compare everything against exact, hand-derived values.  The data
;; is chosen so every predicate has both a t and a nil case and every
;; category of `gp-categorize-pull-requests' is non-empty.

(defun git-platform-test--pr-row (pr uuid)
  "Every shape accessor's value for PR, as one comparable list.
Order: id, full name, source, destination, draft, authored by UUID,
open, merged, author name, author avatar, repo slug, comment count,
review tally."
  (list (alist-get 'id pr)
        (gp-pr-full-name pr) (gp-pr-source-branch pr)
        (gp-pr-destination-branch pr)
        (gp-pr-draft-p pr) (gp-pr-authored-by-p pr uuid)
        (gp-pr-open-p pr) (gp-pr-merged-p pr)
        (gp-pr-author-name pr) (gp-pr-author-avatar pr)
        (gp-pr-repo-slug pr) (gp-pr-comment-count pr)
        (gp-pr-review-tally pr)))

(defun git-platform-test--comment-row (comment uuid)
  "(ID RESOLVED RESOLVABLE OWN PENDING) for COMMENT as seen by UUID."
  (list (alist-get 'id comment)
        (gp-comment-resolved-p comment) (gp-comment-resolvable-p comment)
        (gp-comment-own-p comment uuid) (gp-comment-pending-p comment)))

(defun git-platform-test--await (fn pr)
  "Call async FN on PR and return what its callback got.
Fails when the callback never fires: the mocks answer synchronously,
so a callback that has not run by now never will."
  (let ((got 'never-called))
    (funcall fn pr (lambda (v) (setq got v)))
    (should-not (eq got 'never-called))
    got))

(defun git-platform-test--ids (prs)
  "The ids of PRS, in order."
  (mapcar (lambda (p) (alist-get 'id p)) prs))

(defconst git-platform-test--bb-me "{21d7839d-779f-44b2-8c40-6f43ac90be06}"
  "The uuid tests/fixtures/user.json answers for the current user.")

(defun git-platform-test--bb-pr (id author-uuid author-name &rest extra)
  "A Bitbucket PR object ID by AUTHOR-UUID/AUTHOR-NAME, EXTRA keys first."
  (append extra
          `((id . ,id) (title . ,(format "PR %d" id))
            (author (display_name . ,author-name) (uuid . ,author-uuid))
            (destination (branch (name . "main"))
                         (repository (full_name . "acme/web") (slug . "web")))
            (source (branch (name . ,(format "feature/f%d" id)))
                    (commit (hash . ,(format "c0ffee%d" id)))))))

(defun git-platform-test--bb-participant (role uuid name &rest extra)
  "A Bitbucket participant with ROLE, user UUID/NAME and EXTRA keys."
  (append `((role . ,role) (user (uuid . ,uuid) (display_name . ,name)))
          extra))

(ert-deftest git-platform-test-bitbucket-conformance ()
  "The Bitbucket backend satisfies the git-platform protocol."
  (let* ((me git-platform-test--bb-me)
         (mine (git-platform-test--bb-pr
                180 me "Ada Lovelace"
                '(state . "OPEN") '(comment_count . 13)
                `(participants
                  ,(git-platform-test--bb-participant
                    "REVIEWER" "{rita}" "Rita" '(state . "approved") '(approved . t))
                  ,(git-platform-test--bb-participant
                    "REVIEWER" "{sam}" "Sam" '(state . "changes_requested"))
                  ,(git-platform-test--bb-participant "REVIEWER" "{tom}" "Tom")
                  ;; a non-reviewer's approval is not counted
                  ,(git-platform-test--bb-participant
                    "PARTICIPANT" "{pat}" "Pat" '(approved . t)))))
         (draft (git-platform-test--bb-pr
                 181 me "Ada Lovelace" '(state . "OPEN") '(draft . t)))
         (other (git-platform-test--bb-pr
                 182 "{bea}" "Bea" '(state . "MERGED") '(comment_count . 2)))
         (all (list mine draft other)))
    (bitbucket-mock-with-service
      (let ((bitbucket-mock-overrides
             `(("/workspaces/[^/]+/pullrequests/" . ((values . ,all)))
               ("/pullrequests/180/comments\\'" .
                ((values
                  ((id . 1) (user (uuid . ,me)) (content (raw . "mine")))
                  ((id . 2) (user (uuid . "{bea}")) (content (raw . "theirs"))
                   (resolution (user (display_name . "Bea"))))
                  ((id . 3) (user (uuid . ,me)) (content (raw . "draft note"))
                   (pending . t))
                  ((id . 4) (user (uuid . ,me)) (deleted . t)))))
               ;; the async tally/reviewers refetch the PR by id
               ("/pullrequests/[0-9]+\\'" .
                ,(lambda (_m path &rest _)
                   (let ((id (string-to-number (car (last (split-string path "/"))))))
                     (cl-find id all :key (lambda (p) (alist-get 'id p)))))))))
        (should (equal (gp-user-uuid) me))
        (let ((prs (gp-workspace-pull-requests)))
          (should (equal (mapcar (lambda (p) (git-platform-test--pr-row p me)) prs)
                         `((180 "acme/web" "feature/f180" "main" nil t t nil
                                "Ada Lovelace" nil "web" 13
                                (:approved 1 :changes 1 :pending 1))
                           (181 "acme/web" "feature/f181" "main" t t t nil
                                "Ada Lovelace" nil "web" nil
                                (:approved 0 :changes 0 :pending 0))
                           (182 "acme/web" "feature/f182" "main" nil nil nil t
                                "Bea" nil "web" 2
                                (:approved 0 :changes 0 :pending 0)))))
          (let ((pr (car prs)))
            (should (equal (git-platform-test--await #'gp-pr-review-tally-async pr)
                           '(:approved 1 :changes 1 :pending 1)))
            (should (equal (git-platform-test--await #'gp-pr-reviewers-async pr)
                           '((:id "{rita}" :name "Rita" :avatar nil :state approved)
                             (:id "{sam}" :name "Sam" :avatar nil :state changes)
                             (:id "{tom}" :name "Tom" :avatar nil :state pending))))
            ;; deleted comment 4 is dropped; 1/3 are mine, 2 is resolved,
            ;; 3 is pending; every Bitbucket comment is resolvable
            (should (equal (mapcar (lambda (c) (git-platform-test--comment-row c me))
                                   (gp-pull-request-comments "acme/web" 180))
                           '((1 nil t t nil) (2 t t nil nil) (3 nil t t t)))))
          (let ((cat (gp-categorize-pull-requests prs me)))
            (should (equal (git-platform-test--ids (plist-get cat :mine)) '(180)))
            (should (equal (git-platform-test--ids (plist-get cat :drafts)) '(181)))
            (should (equal (git-platform-test--ids (plist-get cat :reviewing)) '(182))))
          (let ((split (gp-partition-pull-requests prs me)))
            (should (equal (git-platform-test--ids (car split)) '(180 181)))
            (should (equal (git-platform-test--ids (cdr split)) '(182)))))))))

(ert-deftest git-platform-test-github-conformance ()
  "The GitHub backend satisfies the git-platform protocol."
  (github-mock-with-service
    (let ((github-mock-overrides
           ;; pr-1 with one still-requested reviewer, so tally/reviewers
           ;; have something to report; 44 is the user's own draft
           `(("/pulls/42\\'" .
              ,(cons '(requested_reviewers
                       ((login . "cy") (avatar_url . "https://example.com/cy.png")))
                     github-mock--pr-1))
             ("/pulls/43\\'" . ,github-mock--pr-2)
             ("/pulls/44\\'" .
              ,(append '((id . 504) (number . 44) (draft . t)
                         (head (ref . "wip/thing") (sha . "aaa444")
                               (repo (full_name . "acme/web"))))
                       github-mock--pr-1)))))
      (should (equal (gp-user-uuid) "ada"))
      ;; the search only finds the user's PR 42; 43/44 are fetched directly
      (let* ((found (gp-workspace-pull-requests))
             (prs (append found (list (gp-pull-request "acme/web" 43)
                                      (gp-pull-request "acme/web" 44)))))
        (should (equal (git-platform-test--ids found) '(42)))
        (should (equal (mapcar (lambda (p) (git-platform-test--pr-row p "ada")) prs)
                       '((42 "acme/web" "feature/widget" "main" nil t t nil
                              "ada" "https://example.com/ada.png" "web" 3
                              (:approved 0 :changes 0 :pending 1))
                         (43 "acme/web" "fix/flaky" "main" t nil t nil
                              "bea" "https://example.com/bea.png" "web" 0
                              (:approved 0 :changes 0 :pending 0))
                         (44 "acme/web" "wip/thing" "main" t t t nil
                              "ada" "https://example.com/ada.png" "web" 3
                              (:approved 0 :changes 0 :pending 0)))))
        (let ((pr (car prs)))
          (should (equal (git-platform-test--await #'gp-pr-review-tally-async pr)
                         '(:approved 0 :changes 0 :pending 1)))
          (should (equal (git-platform-test--await #'gp-pr-reviewers-async pr)
                         '((:id "cy" :name "cy" :avatar "https://example.com/cy.png"
                                :state pending))))
          ;; both mock comments are bea's; only the inline one is resolvable
          (let ((comments (gp-pull-request-comments "acme/web" 42)))
            (should (equal (mapcar (lambda (c) (git-platform-test--comment-row c "ada"))
                                   comments)
                           '((9001 nil nil nil nil) (9002 nil t nil nil))))
            (should (cl-every (lambda (c) (gp-comment-own-p c "bea")) comments))))
        (let ((cat (gp-categorize-pull-requests prs "ada")))
          (should (equal (git-platform-test--ids (plist-get cat :mine)) '(42)))
          (should (equal (git-platform-test--ids (plist-get cat :drafts)) '(44)))
          (should (equal (git-platform-test--ids (plist-get cat :reviewing)) '(43))))
        (let ((split (gp-partition-pull-requests prs "ada")))
          (should (equal (git-platform-test--ids (car split)) '(42 44)))
          (should (equal (git-platform-test--ids (cdr split)) '(43))))))))

(ert-deftest git-platform-test-backend-lazy-default ()
  "`git-platform-backend' builds a Bitbucket backend by default."
  (let ((git-platform-current-backend nil)
        (git-platform-default-backend 'bitbucket))
    (should (object-of-class-p (git-platform-backend) 'git-platform-bitbucket))))

(ert-deftest git-platform-test-bitbucket-review-batch-add ()
  "Bitbucket: adding a review-batch comment POSTs it with pending:t,
anchored inline at the given path/line."
  (bitbucket-mock-with-service
    (gp-add-review-batch-comment "acme/web" 42 "Looks off" (cons "a.el" 10))
    (should (= (length bitbucket-mock-calls) 1))
    (pcase-let ((`(,method ,path ,_params ,data) (car bitbucket-mock-calls)))
      (should (equal method "POST"))
      (should (equal path "/repositories/acme/web/pullrequests/42/comments"))
      (should (eq (alist-get 'pending data) t))
      (should (equal (let-alist data .content.raw) "Looks off"))
      (should (equal (let-alist data .inline.path) "a.el"))
      (should (equal (let-alist data .inline.to) 10)))))

(ert-deftest git-platform-test-bitbucket-review-batch-list-and-submit ()
  "Bitbucket: only pending comments list as the batch; submitting
recreates each pending comment as an ordinary one and deletes the
pending original, since the public API has no way to un-pend one in
place (see `bitbucket-submit-review-batch'), then applies EVENT."
  (let ((git-platform-current-backend (git-platform-bitbucket))
        (full-name "acme/web") (id 42))
    (bitbucket-mock-with-service
      (let ((bitbucket-mock-overrides
             `(("/pullrequests/42/comments\\'" .
                ((values . (((id . 1) (pending . t) (deleted . nil)
                             (content (raw . "Looks off"))
                             (inline (path . "a.el") (to . 10)))
                            ((id . 2) (pending . nil) (deleted . nil)
                             (content (raw . "already posted"))))))))))
        (let ((batch (gp-review-batch-comments full-name id)))
          (should (= (length batch) 1))
          (should (= (alist-get 'id (car batch)) 1)))
        (let (async-batch)
          (gp-review-batch-comments-async full-name id (lambda (v) (setq async-batch v)))
          (should (= (length async-batch) 1)))
        (setq bitbucket-mock-calls nil)
        (gp-submit-review-batch full-name id 'approve)
        ;; recreated as an ordinary (non-pending) comment, same text/inline
        (let ((created (cl-find-if
                        (lambda (c) (and (equal (nth 0 c) "POST")
                                        (string-match-p "/comments\\'" (nth 1 c))))
                        bitbucket-mock-calls)))
          (should created)
          (should (equal (let-alist (nth 3 created) .content.raw) "Looks off"))
          (should (equal (let-alist (nth 3 created) .inline.path) "a.el"))
          (should (equal (let-alist (nth 3 created) .inline.to) 10))
          (should-not (assq 'pending (nth 3 created))))
        ;; the writes, oldest first: recreate, delete the pending original
        ;; (only it -- comment 2 was never pending), and approve last
        (should (equal (mapcar (lambda (c) (list (nth 0 c) (nth 1 c)))
                               (cl-remove-if (lambda (c) (equal (nth 0 c) "GET"))
                                             (reverse bitbucket-mock-calls)))
                       '(("POST" "/repositories/acme/web/pullrequests/42/comments")
                         ("DELETE" "/repositories/acme/web/pullrequests/42/comments/1")
                         ("POST" "/repositories/acme/web/pullrequests/42/approve"))))))))

(ert-deftest git-platform-test-bitbucket-review-batch-discard ()
  "Bitbucket: discarding the batch deletes each pending comment, and
leaves already-posted ones alone."
  (bitbucket-mock-with-service
    (let ((bitbucket-mock-overrides
           `(("/pullrequests/42/comments\\'" .
              ((values . (((id . 1) (pending . t) (content (raw . "Looks off")))
                          ((id . 2) (pending . nil) (content (raw . "posted")))
                          ((id . 3) (pending . t) (content (raw . "Typo"))))))))))
      (gp-discard-review-batch "acme/web" 42)
      (should (equal (sort (mapcar (lambda (c) (nth 1 c))
                                   (cl-remove-if-not
                                    (lambda (c) (equal (nth 0 c) "DELETE"))
                                    bitbucket-mock-calls))
                           #'string<)
                     '("/repositories/acme/web/pullrequests/42/comments/1"
                       "/repositories/acme/web/pullrequests/42/comments/3")))
      ;; nothing else is written
      (should-not (cl-find-if (lambda (c) (member (nth 0 c) '("POST" "PUT")))
                              bitbucket-mock-calls)))))

(defun git-platform-test--gh-pending-comment-node ()
  "One GraphQL comment node, as `github--pending-review-query' returns it."
  '((id . "NODE_1") (databaseId . 1)
    (body . "Looks off") (path . "a.el") (line . 10)
    (author (login . "ada") (avatarUrl . "u"))
    (createdAt . "2026-01-01T00:00:00Z")
    (url . "https://x")))

(defun git-platform-test--gh-pending-review (comment-nodes)
  "A `reviews(states:PENDING...)' GraphQL response holding COMMENT-NODES."
  (let* ((comments `((nodes . ,comment-nodes)))
         (review `((id . "REVIEW_1") (databaseId . 900) (comments . ,comments)))
         (reviews `((nodes ,review)))
         (pr `((reviews . ,reviews)))
         (repo `((pullRequest . ,pr))))
    `((repository . ,repo))))

(ert-deftest git-platform-test-github-review-batch-round-trip ()
  "GitHub: add/list/submit a review batch, dispatched through gp-, via GraphQL."
  (let ((git-platform-current-backend (git-platform-github)))
    (github-mock-with-service
      (let ((full-name "acme/web") (id 42)
            (graphql-responses
             (list
              ;; 1: github--ensure-pending-review's own lookup -- none yet
              '((repository (pullRequest (reviews (nodes)))))
              ;; 2: addPullRequestReview
              '((addPullRequestReview (pullRequestReview (id . "REVIEW_1"))))
              ;; 3: addPullRequestReviewThread
              '((addPullRequestReviewThread (thread (id . "THREAD_1")))))))
        (cl-letf (((symbol-function 'github-graphql-request)
                   (lambda (query &optional variables)
                     (push (list query variables) github-mock-graphql-calls)
                     (pop graphql-responses))))
          (gp-add-review-batch-comment full-name id "Looks off" (cons "a.el" 10)))
        ;; a pending review was created on the PR's node, and the comment
        ;; was threaded onto it at the given path/line
        (let ((vars (lambda (re)
                      (cadr (cl-find-if (lambda (c) (string-match-p re (car c)))
                                        github-mock-graphql-calls)))))
          (should (equal (funcall vars "addPullRequestReview(") '((pr . "PR_42"))))
          (should (equal (funcall vars "addPullRequestReviewThread(")
                         '((review . "REVIEW_1") (path . "a.el") (line . 10)
                           (body . "Looks off")))))
        (setq github-mock-graphql-calls nil)
        ;; now simulate the review existing with one comment, for list/submit
        (let ((pending-review
               (git-platform-test--gh-pending-review
                (list (git-platform-test--gh-pending-comment-node)))))
          (cl-letf (((symbol-function 'github-graphql-request)
                     (lambda (query &optional variables)
                       (push (list query variables) github-mock-graphql-calls)
                       (cond
                        ((string-match-p "reviews(states:PENDING" query) pending-review)
                        ((string-match-p "submitPullRequestReview" query)
                         '((submitPullRequestReview (pullRequestReview (id . "REVIEW_1")))))
                        (t '((ok . t))))))
                    ;; no mock for the async GraphQL entry point (nothing else
                    ;; exercises it yet); delegate to the already-mocked sync
                    ;; path, same trick `git-platform-test-github-conformance'
                    ;; uses for `github-api-paged-async'.
                    ((symbol-function 'github-graphql-request-async)
                     (lambda (query variables callback)
                       (funcall callback t (github-graphql-request query variables)))))
            (let ((batch (gp-review-batch-comments full-name id)))
              (should (= (length batch) 1))
              (should (gp-comment-pending-p (car batch)))
              (should (= (alist-get 'id (car batch)) 1)))
            (let (async-batch)
              (gp-review-batch-comments-async full-name id (lambda (v) (setq async-batch v)))
              (should (= (length async-batch) 1)))
            (gp-submit-review-batch full-name id 'approve)
            ;; the existing pending review is submitted with the mapped event
            (should (equal (cadr (cl-find-if
                                  (lambda (c) (string-match-p "submitPullRequestReview" (car c)))
                                  github-mock-graphql-calls))
                           '((review . "REVIEW_1") (event . "APPROVE") (body))))
            ;; ...and no second review is opened
            (should-not (cl-find-if
                         (lambda (c) (string-match-p "addPullRequestReview(" (car c)))
                         github-mock-graphql-calls))))))))

(ert-deftest git-platform-test-cache-remove ()
  (let ((gp--result-cache (make-hash-table :test 'equal))
        (gp-cache-ttl 300))
    (gp-cache-put '(pull-request "acme/web" 1) 'value)
    (should (car (gp-cache-get '(pull-request "acme/web" 1))))
    (gp-cache-remove '(pull-request "acme/web" 1))
    (should-not (car (gp-cache-get '(pull-request "acme/web" 1))))))

(ert-deftest git-platform-test-cache-remove-matching ()
  (let ((gp--result-cache (make-hash-table :test 'equal))
        (gp-cache-ttl 300))
    (gp-cache-put '(mine "u1" nil) 'a)
    (gp-cache-put '(reviewing "u1" ("OPEN")) 'b)
    (gp-cache-put '(pull-request "acme/web" 1) 'c)
    (gp-cache-remove-matching (lambda (k) (memq (car-safe k) '(mine reviewing))))
    (should-not (car (gp-cache-get '(mine "u1" nil))))
    (should-not (car (gp-cache-get '(reviewing "u1" ("OPEN")))))
    ;; unrelated key untouched
    (should (car (gp-cache-get '(pull-request "acme/web" 1))))))

(ert-deftest git-platform-test-invalidate-pr-caches ()
  "Clears the PR's own per-PR entries and every list-level cache,
leaving unrelated keys alone."
  (let ((gp--result-cache (make-hash-table :test 'equal))
        (gp-cache-ttl 300)
        (pr '((id . 1) (destination (repository (full_name . "acme/web")))
              (source (commit (hash . "abc123"))))))
    (let ((git-platform-current-backend (git-platform-bitbucket)))
      (gp-cache-put '(pull-request "acme/web" 1) 'stale-pr)
      (gp-cache-put '(pr-stats "acme/web" 1 "abc123") 'stale-stats)
      (gp-cache-put '(pr-diff "acme/web" 1 "abc123") 'stale-diff)
      (gp-cache-put '(mine "u1" nil) 'stale-list)
      (gp-cache-put '(other-thing) 'untouched)
      (gp-invalidate-pr-caches pr)
      (should-not (car (gp-cache-get '(pull-request "acme/web" 1))))
      (should-not (car (gp-cache-get '(pr-stats "acme/web" 1 "abc123"))))
      (should-not (car (gp-cache-get '(pr-diff "acme/web" 1 "abc123"))))
      (should-not (car (gp-cache-get '(mine "u1" nil))))
      (should (car (gp-cache-get '(other-thing)))))))

(ert-deftest git-platform-test-diff-chunk-new-lines ()
  "New-side line set counts context and added lines, not deletions."
  (let* ((chunk "diff --git a/f b/f
--- a/f
+++ b/f
@@ -10,4 +10,4 @@
 ctx10
-removed11
+added11
 ctx12
")
         (present (gp-diff-chunk-new-lines chunk)))
    ;; new side body lines: 10 ctx, 11 added, 12 ctx (removed line is old-only)
    (should (gethash 10 present))
    (should (gethash 11 present))
    (should (gethash 12 present))
    (should-not (gethash 13 present))
    (should-not (gethash 9 present))))

(ert-deftest git-platform-test-comment-outdated-p ()
  "A comment is outdated only when its file is in the diff but its line is not."
  (let* ((diff "diff --git a/a.el b/a.el
--- a/a.el
+++ b/a.el
@@ -1,3 +1,3 @@
 one
-two-old
+two-new
 three
")
         (dbf (gp-split-diff-by-file diff))
         (in-diff   '((inline (path . "a.el") (to . 2))))   ;; line 2 present
         (stale     '((inline (path . "a.el") (to . 99))))  ;; line gone
         (other     '((inline (path . "z.el") (to . 1))))   ;; file not in diff
         (general   '((content (raw . "hi")))))             ;; not inline
    (should-not (gp-comment-outdated-p in-diff dbf))
    (should     (gp-comment-outdated-p stale dbf))
    (should-not (gp-comment-outdated-p other dbf))   ;; can't prove -> not outdated
    (should-not (gp-comment-outdated-p general dbf))
    (should-not (gp-comment-outdated-p stale nil)))) ;; no diff -> not outdated

;;;; Buffer naming ------------------------------------------------------------

(ert-deftest gp-test-buffer-name-carries-the-shared-tag ()
  (let ((gp-buffer-name-prefix "zz: "))
    (should (equal (gp--buffer-name "PRs") "*zz: PRs*"))))

(ert-deftest gp-test-pipeline-id-differs-per-backend ()
  "Each backend names its pipeline identifier differently.
Bitbucket keys a pipeline by `uuid', GitHub Actions a run by `id'.
Reading either field directly works on one forge and silently returns
nil on the other."
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (should (equal (gp-pipeline-id '((uuid . "{abc}"))) "{abc}"))
    (should (equal (gp-pipeline-step-id '((uuid . "{step}"))) "{step}")))
  (let ((git-platform-current-backend (git-platform-github)))
    (should (equal (gp-pipeline-id '((id . 42))) 42))
    (should (equal (gp-pipeline-step-id '((id . 7))) 7))))

(ert-deftest gp-test-pr-web-url-differs-per-backend ()
  "Each forge names the browser URL differently.
Bitbucket nests it at `links.html.href', GitHub has a flat `html_url'
and no `links' object at all -- so reading Bitbucket's shape directly
made \"open in browser\" fail on every GitHub PR with \"No URL for this
PR\"."
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (should (equal (gp-pr-web-url '((links (html (href . "https://bb/pr/1")))))
                   "https://bb/pr/1")))
  (let ((git-platform-current-backend (git-platform-github)))
    (should (equal (gp-pr-web-url '((html_url . "https://gh/pull/1")))
                   "https://gh/pull/1"))))

(ert-deftest gp-test-comment-web-url-differs-per-backend ()
  "Same split for a comment's browser URL."
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (should (equal (gp-comment-web-url '((links (html (href . "https://bb/c/1")))))
                   "https://bb/c/1")))
  (let ((git-platform-current-backend (git-platform-github)))
    (should (equal (gp-comment-web-url '((links (html (href . "https://gh/c/1")))))
                   "https://gh/c/1"))))

(ert-deftest gp-test-ticket-key-pattern-matches-shape ()
  "2-3 letters, a dash, 1-5 digits -- and, deliberately, never a bare
GitHub-style \"#123\" reference (no letter prefix to match at all)."
  (should (string-match-p gp-ticket-key-pattern "WP-1231"))
  (should (string-match-p gp-ticket-key-pattern "ab-7"))
  (should (string-match-p gp-ticket-key-pattern "XYZ-99999"))
  (should-not (string-match-p gp-ticket-key-pattern "#123"))
  (should-not (string-match-p gp-ticket-key-pattern "A-1"))          ;; only 1 letter
  (should-not (string-match-p gp-ticket-key-pattern "ABCD-1"))       ;; 4 letters
  (should-not (string-match-p gp-ticket-key-pattern "WP-123456")))   ;; 6 digits

(ert-deftest gp-test-ticket-url-for-honours-configured-format ()
  (let ((gp-ticket-url-format "https://co.atlassian.net/browse/%s"))
    (should (equal (gp-ticket-url-for "WP-1231")
                   "https://co.atlassian.net/browse/WP-1231")))
  (let ((gp-ticket-url-format nil))
    (should-not (gp-ticket-url-for "WP-1231"))))

(ert-deftest gp-test-ticket-linkify-string-links-configured-key ()
  (let* ((gp-ticket-url-format "https://co.atlassian.net/browse/%s")
         (s (gp-ticket-linkify-string "fix: handle WP-1231 correctly")))
    (should (equal (substring-no-properties s) "fix: handle WP-1231 correctly"))
    (let ((pos (string-match "WP-1231" s)))
      (should (eq (get-text-property pos 'face s) 'link))
      (should (equal (get-text-property pos 'help-echo s)
                     "https://co.atlassian.net/browse/WP-1231")))))

(ert-deftest gp-test-ticket-linkify-string-unconfigured-still-highlights ()
  "Unconfigured: still linked/highlighted (so it visibly reads as a
recognized ticket key), but the tooltip says to configure it instead
of a guessed or blank URL."
  (let* ((gp-ticket-url-format nil)
         (s (gp-ticket-linkify-string "fix: handle WP-1231 correctly"))
         (pos (string-match "WP-1231" s)))
    (should (eq (get-text-property pos 'face s) 'link))
    (should (string-match-p "gp-ticket-url-format" (get-text-property pos 'help-echo s)))))

(ert-deftest gp-test-ticket-linkify-string-ignores-github-issue-refs ()
  (let* ((gp-ticket-url-format "https://co.atlassian.net/browse/%s")
         (s (gp-ticket-linkify-string "closes #123, relates to WP-9")))
    (should-not (eq (get-text-property (string-match "#123" s) 'face s) 'link))
    (should (eq (get-text-property (string-match "WP-9" s) 'face s) 'link))))

(ert-deftest gp-test-ticket-linkify-string-multiple-keys ()
  (let* ((gp-ticket-url-format "https://co.atlassian.net/browse/%s")
         (s (gp-ticket-linkify-string "WP-1 and ABC-22 both here")))
    (should (eq (get-text-property (string-match "WP-1" s) 'face s) 'link))
    (should (eq (get-text-property (string-match "ABC-22" s) 'face s) 'link))))

(ert-deftest gp-test-ticket-linkify-string-nil-is-nil ()
  (should-not (gp-ticket-linkify-string nil)))

(ert-deftest gp-test-ticket-keymap-binds-mouse-2 ()
  "`mouse-1-click-follows-link' translates a plain mouse-1 click on a
`follow-link' region into `mouse-2' BEFORE keymap lookup happens --
a keymap binding only `mouse-1' silently never gets that translated
event, and the click falls through to whatever mouse-2 does by
default instead (e.g. `mouse-yank-primary', or `set-mark-command'
depending on context) -- see `gp-ticket--keymap'."
  (let ((m (gp-ticket--keymap "WP-1231"))
        (gp-ticket-url-format "https://co.atlassian.net/browse/%s"))
    ;; every binding opens the ticket's URL
    (dolist (key (list [mouse-1] [mouse-2] (kbd "RET")))
      (let (opened)
        (cl-letf (((symbol-function 'browse-url)
                   (lambda (url &rest _) (push url opened))))
          (call-interactively (lookup-key m key)))
        (should (equal opened '("https://co.atlassian.net/browse/WP-1231")))))))
(ert-deftest gp-test-cache-put-honours-explicit-ttl ()
  "An explicit TTL scopes one entry without touching the global default."
  (let ((gp--result-cache (make-hash-table :test 'equal))
        (gp-cache-ttl 300))
    ;; a short-lived entry expires on its own schedule...
    (gp-cache-put '(k short) 'v 60)
    ;; omitting TTL keeps the previous behaviour (gp-cache-ttl)
    (gp-cache-put '(k default) 'v)
    (should (equal (gp-cache-get '(k short)) '(t . v)))
    (let ((later (+ (float-time) 100)))
      (cl-letf (((symbol-function 'float-time) (lambda (&rest _) later)))
        (should-not (car (gp-cache-get '(k short))))
        ;; ...while the default-TTL entry written at the same time still lives
        (should (equal (gp-cache-get '(k default)) '(t . v)))))
    ;; a non-positive TTL skips caching entirely
    (gp-cache-put '(k none) 'v 0)
    (should-not (car (gp-cache-get '(k none))))))

(provide 'git-platform-test)
;;; git-platform-test.el ends here
