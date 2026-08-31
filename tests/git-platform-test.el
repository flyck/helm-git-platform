;;; git-platform-test.el --- Protocol conformance tests -*- lexical-binding: t; -*-

;;; Commentary:

;; A backend-agnostic conformance suite: it exercises the `gp-' protocol
;; against a configured backend + its mock and asserts the documented
;; return shapes.  Today it runs against the Bitbucket backend with
;; `bitbucket-mock-with-service'.  A future GitHub backend reuses
;; `git-platform-test--run' with its own mock, proving both satisfy one
;; contract.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-platform)
(require 'git-platform-bitbucket)
(require 'bitbucket-mock)
(require 'git-platform-github)
(require 'github-mock)

(defun git-platform-test--run ()
  "Run protocol assertions against the active backend (mock must be live)."
  (let* ((uuid (gp-user-uuid))
         (prs (gp-workspace-pull-requests)))
    (should (stringp uuid))
    (should (listp prs))
    (let ((pr (car prs)))
      (should pr)
      ;; accessors return the documented shapes
      (should (string-match-p "/" (gp-pr-full-name pr)))   ;; owner/slug
      (should (stringp (gp-pr-source-branch pr)))
      (should (stringp (gp-pr-destination-branch pr)))
      (should (memq (gp-pr-draft-p pr) '(t nil)))
      (should (memq (gp-pr-authored-by-p pr uuid) '(t nil)))
      (should (memq (gp-pr-open-p pr) '(t nil)))
      (should (memq (gp-pr-merged-p pr) '(t nil)))
      (should (or (null (gp-pr-author-name pr)) (stringp (gp-pr-author-name pr))))
      (should (or (null (gp-pr-author-avatar pr)) (stringp (gp-pr-author-avatar pr))))
      (should (or (null (gp-pr-repo-slug pr)) (stringp (gp-pr-repo-slug pr))))
      (should (or (null (gp-pr-comment-count pr)) (integerp (gp-pr-comment-count pr))))
      (let ((tally (gp-pr-review-tally pr)))
        (should (integerp (plist-get tally :approved)))
        (should (integerp (plist-get tally :changes)))
        (should (integerp (plist-get tally :pending))))
      (let (async-tally)
        (gp-pr-review-tally-async pr (lambda (v) (setq async-tally v)))
        (should (integerp (plist-get async-tally :approved))))
      (let (async-reviewers)
        (gp-pr-reviewers-async pr (lambda (v) (setq async-reviewers v)))
        (should (listp async-reviewers)))
      ;; categorize/partition produce the documented structure
      (let ((cat (gp-categorize-pull-requests prs uuid)))
        (should (= (length prs)
                   (+ (length (plist-get cat :mine))
                      (length (plist-get cat :reviewing))
                      (length (plist-get cat :drafts))))))
      (let ((split (gp-partition-pull-requests prs uuid)))
        (should (= (length prs) (+ (length (car split)) (length (cdr split))))))
      ;; comments + their accessors
      (let* ((fn (gp-pr-full-name pr))
             (id (alist-get 'id pr))
             (comments (gp-pull-request-comments fn id)))
        (should (listp comments))
        (when comments
          (should (memq (gp-comment-resolved-p (car comments)) '(t nil)))
          (should (memq (gp-comment-resolvable-p (car comments)) '(t nil)))
          (should (memq (gp-comment-own-p (car comments) uuid) '(t nil))))))))

(ert-deftest git-platform-test-bitbucket-conformance ()
  "The Bitbucket backend satisfies the git-platform protocol."
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (bitbucket-mock-with-service
      (git-platform-test--run))))

(ert-deftest git-platform-test-github-conformance ()
  "The GitHub backend satisfies the git-platform protocol."
  (let ((git-platform-current-backend (git-platform-github)))
    (github-mock-with-service
      ;; `github-api-paged-async' isn't itself stubbed by github-mock.el (it
      ;; does real `url-retrieve' I/O even against a mocked host); delegate
      ;; it to the already-mocked sync path so gp-pr-review-tally-async /
      ;; gp-pr-reviewers-async (exercised by `git-platform-test--run') get
      ;; a real callback instead of hanging.
      (cl-letf (((symbol-function 'github-api-paged-async)
                 (lambda (path &optional params callback _max-items)
                   (funcall callback t (github-mock-paged path params)))))
        (git-platform-test--run)))))

(ert-deftest git-platform-test-backend-lazy-default ()
  "`git-platform-backend' builds a Bitbucket backend by default."
  (let ((git-platform-current-backend nil)
        (git-platform-default-backend 'bitbucket))
    (should (object-of-class-p (git-platform-backend) 'git-platform-bitbucket))))

(ert-deftest git-platform-test-public-wrappers-need-no-backend ()
  "The public gp- functions take no backend argument."
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (bitbucket-mock-with-service
      ;; would error if gp-user-uuid still required a backend arg
      (should (stringp (gp-user-uuid))))))

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
  (should (equal (gp--buffer-name "PRs") "*gp: PRs*"))
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
                   "https://gh/pull/1"))
    ;; a reshaped object carrying the shared shape still works
    (should (equal (gp-pr-web-url '((links (html (href . "https://gh/pull/2")))))
                   "https://gh/pull/2"))))

(ert-deftest gp-test-comment-web-url-differs-per-backend ()
  "Same split for a comment's browser URL."
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (should (equal (gp-comment-web-url '((links (html (href . "https://bb/c/1")))))
                   "https://bb/c/1")))
  (let ((git-platform-current-backend (git-platform-github)))
    (should (equal (gp-comment-web-url '((links (html (href . "https://gh/c/1")))))
                   "https://gh/c/1"))
    (should (equal (gp-comment-web-url '((html_url . "https://gh/c/2")))
                   "https://gh/c/2"))))

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

(ert-deftest gp-test-ticket-linkify-string-pads-surrounding-whitespace ()
  "A short ticket key is an easy mouse target to miss by a column --
padding the clickable/highlighted region into an adjacent SPACE (but
never into another word) closes that gap.  A click landing on the
space right before/after \"WP-1231\" must still be `link'-faced."
  (let* ((gp-ticket-url-format "https://co.atlassian.net/browse/%s")
         (s (gp-ticket-linkify-string "see WP-1231 now")))
    (let ((key-start (string-match "WP-1231" s)))
      ;; the space right before the key is now also `link'-faced...
      (should (eq (get-text-property (1- key-start) 'face s) 'link))
      ;; ...but the word before THAT space is untouched
      (should-not (eq (get-text-property (- key-start 2) 'face s) 'link))
      ;; the space right after the key is also `link'-faced...
      (should (eq (get-text-property (+ key-start 7) 'face s) 'link))
      ;; ...but the word after THAT space is untouched
      (should-not (eq (get-text-property (+ key-start 8) 'face s) 'link)))))

(ert-deftest gp-test-ticket-linkify-string-padding-never-eats-a-neighbour ()
  "Two ticket keys separated by exactly one space must not both claim
that same space -- whichever is processed first would otherwise
overwrite the other's padding claim on it."
  (let* ((gp-ticket-url-format "https://co.atlassian.net/browse/%s")
         (s (gp-ticket-linkify-string "WP-1 AB-99")))
    ;; the single space between them is still faced (claimed by one of
    ;; the two neighbours, whichever ran second, harmlessly) but the
    ;; keys themselves are intact and distinctly identifiable
    (should (eq (get-text-property (string-match "WP-1" s) 'face s) 'link))
    (should (eq (get-text-property (string-match "AB-99" s) 'face s) 'link))
    (should (equal (substring-no-properties s) "WP-1 AB-99"))))

(provide 'git-platform-test)
;;; git-platform-test.el ends here
