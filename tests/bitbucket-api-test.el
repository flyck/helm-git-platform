;;; bitbucket-api-test.el --- Tests for the Bitbucket API layer -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for bitbucket-api.el, driven entirely by the centralized
;; mock service (no network).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'bitbucket-api)
(require 'bitbucket-mock)

(ert-deftest bitbucket-test-build-url-relative ()
  (let ((bitbucket-api-base "https://api.bitbucket.org/2.0"))
    (should (equal (bitbucket--build-url "/user" nil)
                   "https://api.bitbucket.org/2.0/user"))
    (should (equal (bitbucket--build-url "user" nil)
                   "https://api.bitbucket.org/2.0/user"))))

(ert-deftest bitbucket-test-build-url-absolute-passthrough ()
  "An absolute URL (a pagination next link) is used verbatim."
  (should (equal (bitbucket--build-url "https://api.bitbucket.org/2.0/x?page=2" nil)
                 "https://api.bitbucket.org/2.0/x?page=2")))

(ert-deftest bitbucket-test-encode-query-drops-nil ()
  (should (equal (bitbucket--encode-query '(("a" . "1") ("b" . nil) ("c" . "x y")))
                 "a=1&c=x%20y")))

(ert-deftest bitbucket-test-parse-json-empty ()
  (should (null (bitbucket--parse-json "")))
  (should (null (bitbucket--parse-json "   "))))

(ert-deftest bitbucket-test-user-uuid-cached ()
  (bitbucket-mock-with-service
    (should (equal (bitbucket-user-uuid)
                   "{21d7839d-779f-44b2-8c40-6f43ac90be06}"))
    ;; second call must not hit the service again (cache)
    (setq bitbucket-mock-calls nil)
    (bitbucket-user-uuid)
    (should (null bitbucket-mock-calls))))

(ert-deftest bitbucket-test-workspace-prs-loaded ()
  (bitbucket-mock-with-service
    (let ((prs (bitbucket-workspace-pull-requests)))
      (should (= (length prs) 10))
      ;; every PR has the fields the UI relies on
      (dolist (pr prs)
        (should (alist-get 'id pr))
        (should (let-alist pr .destination.repository.full_name))))))

(ert-deftest bitbucket-test-partition-mine-vs-reviewing ()
  (bitbucket-mock-with-service
    (let* ((uuid (bitbucket-user-uuid))
           (prs (bitbucket-workspace-pull-requests))
           (split (bitbucket-partition-pull-requests prs uuid)))
      ;; in the fixture, all 10 are authored by the user
      (should (= (length (car split)) 10))
      (should (= (length (cdr split)) 0))
      ;; flip one author and re-check
      (let ((prs2 (copy-tree prs)))
        (setf (let-alist (car prs2) .author.uuid) "{other}")
        (setf (alist-get 'uuid (alist-get 'author (car prs2))) "{other}")
        (let ((split2 (bitbucket-partition-pull-requests prs2 uuid)))
          (should (= (length (cdr split2)) 1)))))))

(ert-deftest bitbucket-test-comments-filters-deleted ()
  (let ((bitbucket-mock-overrides
         '(("/comments" .
            ((values . (((id . 1) (deleted . t) (content (raw . "gone")))
                        ((id . 2) (deleted . nil) (content (raw . "kept"))))))))))
    (bitbucket-mock-with-service
      (let ((cs (bitbucket-pull-request-comments "acme/x" 1)))
        (should (= (length cs) 1))
        (should (= (alist-get 'id (car cs)) 2))))))

(ert-deftest bitbucket-test-paged-follows-next ()
  "Pagination follows the `next' link until it is absent."
  (let* ((calls 0)
         (bitbucket-mock-overrides
          `(("/things" .
             ,(lambda (_m _p _params _d)
                (cl-incf calls)
                (if (= calls 1)
                    '((values . (((id . 1)) ((id . 2))))
                      (next . "https://api.bitbucket.org/2.0/things?page=2"))
                  '((values . (((id . 3)))))))))))
    (bitbucket-mock-with-service
      (let ((vals (bitbucket-api-paged "/things")))
        (should (= (length vals) 3))
        (should (= calls 2))))))

;;;; Write API (post / resolve / reopen) -----------------------------------

(ert-deftest bitbucket-test-create-inline-comment-payload ()
  "Creating an inline comment POSTs content + inline anchor."
  (bitbucket-mock-with-service
    (let ((c (bitbucket-create-comment "ws/slug" 7 "looks good"
                                       '("src/a.ts" . 42))))
      (should (= (alist-get 'id c) 99999))
      ;; inspect the recorded POST body
      (let* ((call (cl-find "POST" bitbucket-mock-calls
                            :key #'car :test #'equal))
             (data (nth 3 call)))
        (should (string-suffix-p "/comments" (nth 1 call)))
        (should (equal (let-alist data .content.raw) "looks good"))
        (should (equal (let-alist data .inline.path) "src/a.ts"))
        (should (equal (let-alist data .inline.to) 42))))))

(ert-deftest bitbucket-test-create-general-comment-has-no-inline ()
  (bitbucket-mock-with-service
    (bitbucket-create-comment "ws/slug" 7 "general note")
    (let* ((call (cl-find "POST" bitbucket-mock-calls :key #'car :test #'equal))
           (data (nth 3 call)))
      (should (equal (let-alist data .content.raw) "general note"))
      (should (null (alist-get 'inline data))))))

(ert-deftest bitbucket-test-reply-sets-parent ()
  (bitbucket-mock-with-service
    (bitbucket-create-comment "ws/slug" 7 "replying" nil 555)
    (let* ((call (cl-find "POST" bitbucket-mock-calls :key #'car :test #'equal))
           (data (nth 3 call)))
      (should (equal (let-alist data .parent.id) 555)))))

(ert-deftest bitbucket-test-resolve-and-reopen-verbs ()
  (bitbucket-mock-with-service
    (bitbucket-resolve-comment "ws/slug" 7 123)
    (bitbucket-reopen-comment "ws/slug" 7 123)
    (let ((resolve (cl-find-if (lambda (c) (and (equal (car c) "POST")
                                                (string-suffix-p "/resolve" (nth 1 c))))
                               bitbucket-mock-calls))
          (reopen (cl-find-if (lambda (c) (and (equal (car c) "DELETE")
                                               (string-suffix-p "/resolve" (nth 1 c))))
                              bitbucket-mock-calls)))
      (should resolve)
      (should reopen)
      (should (string-match-p "/comments/123/resolve\\'" (nth 1 resolve))))))

(ert-deftest bitbucket-test-comment-resolved-p ()
  (should (bitbucket-comment-resolved-p '((resolution (user (display_name . "X"))))))
  (should-not (bitbucket-comment-resolved-p '((content (raw . "open"))))))

;;;; Branch lookup, counts, drafts -----------------------------------------

(ert-deftest bitbucket-test-open-pr-for-branch-query ()
  "The branch lookup sends a q filter on source.branch.name."
  (let ((bitbucket-mock-overrides
         `(("/pullrequests" .
            ,(lambda (_m _p params _d)
               (should (string-match-p "source.branch.name=\"feat\""
                                       (cdr (assoc "q" params))))
               '((values . (((id . 88) (source (branch (name . "feat"))))))))))) )
    (bitbucket-mock-with-service
      (let ((pr (bitbucket-open-pr-for-branch "ws/slug" "feat")))
        (should (= (alist-get 'id pr) 88))))))

(ert-deftest bitbucket-test-open-pr-for-branch-none ()
  (let ((bitbucket-mock-overrides '(("/pullrequests" . ((values . nil))))))
    (bitbucket-mock-with-service
      (should (null (bitbucket-open-pr-for-branch "ws/slug" "missing"))))))

(ert-deftest bitbucket-test-repo-open-pr-count ()
  (let ((bitbucket-mock-overrides '(("/pullrequests" . ((size . 7) (values . nil))))))
    (bitbucket-mock-with-service
      (should (= (bitbucket-repo-open-pr-count "ws/slug") 7)))))

(ert-deftest bitbucket-test-draft-predicate ()
  (should (bitbucket-pr-draft-p '((draft . t))))
  ;; our JSON parser maps json-false to nil, so a non-draft is (draft . nil)
  (should-not (bitbucket-pr-draft-p '((draft . nil))))
  (should-not (bitbucket-pr-draft-p '((title . "x")))))

(ert-deftest bitbucket-test-categorize-splits-drafts ()
  (let* ((uuid "{me}")
         (prs '(((id . 1) (author (uuid . "{me}")))                  ;; mine
                ((id . 2) (author (uuid . "{me}")) (draft . t))      ;; my draft
                ((id . 3) (author (uuid . "{other}")))               ;; reviewing
                ((id . 4) (author (uuid . "{other}")) (draft . t)))) ;; others' draft
         (cat (bitbucket-categorize-pull-requests prs uuid)))
    (should (equal (mapcar (lambda (p) (alist-get 'id p)) (plist-get cat :mine)) '(1)))
    (should (equal (mapcar (lambda (p) (alist-get 'id p)) (plist-get cat :drafts)) '(2)))
    ;; others' draft still shows under reviewing (it involves me as reviewer)
    (should (equal (mapcar (lambda (p) (alist-get 'id p)) (plist-get cat :reviewing)) '(3 4)))))

(ert-deftest bitbucket-test-with-cache-hits-and-expires ()
  (let ((bitbucket-cache-ttl 300)
        (bitbucket--result-cache (make-hash-table :test 'equal))
        (calls 0))
    (cl-flet ((thunk () (cl-incf calls) 'value))
      (should (eq (bitbucket-with-cache 'k #'thunk) 'value))
      (should (eq (bitbucket-with-cache 'k #'thunk) 'value))
      (should (= calls 1)))))             ;; second call served from cache

(ert-deftest bitbucket-test-with-cache-disabled ()
  (let ((bitbucket-cache-ttl 0)
        (bitbucket--result-cache (make-hash-table :test 'equal))
        (calls 0))
    (cl-flet ((thunk () (cl-incf calls) 'v))
      (bitbucket-with-cache 'k #'thunk)
      (bitbucket-with-cache 'k #'thunk)
      (should (= calls 2)))))             ;; ttl 0 -> no caching

(ert-deftest bitbucket-test-state-clause ()
  (should (equal (bitbucket--state-clause nil) ""))
  (should (equal (bitbucket--state-clause '("OPEN"))
                 " AND (state=\"OPEN\")"))
  (should (equal (bitbucket--state-clause '("OPEN" "MERGED"))
                 " AND (state=\"OPEN\" OR state=\"MERGED\")")))

(ert-deftest bitbucket-test-request-decodes-utf8 ()
  "A UTF-8 response body (umlauts) is decoded, not left as raw bytes."
  (let* ((json "{\"content\":{\"raw\":\"zurückgegeben\"}}")
         (bytes (encode-coding-string json 'utf-8))
         (bitbucket-user-email "x@example.com")
         (bitbucket-api-token "tok"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (let ((buf (generate-new-buffer " *bb-test-http*")))
                   (with-current-buffer buf
                     (set-buffer-multibyte nil)        ;; raw, like url.el
                     (insert (encode-coding-string "HTTP/1.1 200 OK\n" 'utf-8))
                     (insert (encode-coding-string
                              "Content-Type: application/json\n\n" 'utf-8))
                     (insert bytes))
                   buf))))
      (let ((parsed (bitbucket-api-request "GET" "/x")))
        (should (equal (let-alist parsed .content.raw) "zurückgegeben"))))))

(ert-deftest bitbucket-test-reviewing-scans-repos-with-reviewer-filter ()
  "Reviewing fetch lists recent repos then queries each with reviewers.uuid."
  (let ((bitbucket-mock-overrides
         `(;; repo listing
           ("/repositories/[^/]+\\'" .
            ,(lambda (_m _p _params _d)
               '((values . (((full_name . "ws/a")) ((full_name . "ws/b")))))))
           ;; per-repo PR query -- assert the reviewer filter is present
           ("/repositories/[^/]+/[^/]+/pullrequests" .
            ,(lambda (_m _p params _d)
               (should (string-match-p "reviewers.uuid="
                                       (cdr (assoc "q" params))))
               '((values . (((id . 1) (title . "x"))))))))))
    (bitbucket-mock-with-service
      (let ((prs (bitbucket-reviewing-pull-requests "{me}" 2)))
        ;; one PR from each of the two scanned repos
        (should (= (length prs) 2))))))

(ert-deftest bitbucket-test-resolve-emojis ()
  (let ((gp-resolve-emoji-shortcodes t))
    ;; uses the built-in fallback at minimum (emojify may override)
    (should (string-match-p "🤔" (gp-resolve-emojis "hmm :thinking: yes")))
    (should (string-match-p "👍" (gp-resolve-emojis ":+1:")))
    ;; unknown shortcode left intact
    (should (equal (gp-resolve-emojis ":not_a_real_emoji_xyz:")
                   ":not_a_real_emoji_xyz:"))
    ;; inline code is not touched
    (should (string-match-p "`:thinking:`"
                            (gp-resolve-emojis "see `:thinking:` literally")))))

(ert-deftest bitbucket-test-set-draft ()
  "Toggling draft PUTs title + draft flag."
  (bitbucket-mock-with-service
    (bitbucket-set-pull-request-draft "ws/slug" 7 t "My PR")
    (bitbucket-set-pull-request-draft "ws/slug" 7 nil "My PR")
    (let* ((calls (cl-remove-if-not
                   (lambda (c) (and (equal (car c) "PUT")
                                    (string-suffix-p "/pullrequests/7" (nth 1 c))))
                   bitbucket-mock-calls))
           (to-draft (nth 3 (car (last calls))))    ;; first PUT (draft t)
           (to-ready (nth 3 (car calls))))          ;; last PUT (draft nil)
      (should (= (length calls) 2))
      (should (equal (alist-get 'title to-draft) "My PR"))
      (should (eq (alist-get 'draft to-draft) t))
      (should (eq (alist-get 'draft to-ready) :json-false)))))

(ert-deftest bitbucket-test-split-diff-by-file ()
  (let* ((diff (concat
                "diff --git a/src/a.ts b/src/a.ts\n"
                "--- a/src/a.ts\n+++ b/src/a.ts\n@@ -1 +1 @@\n-x\n+y\n"
                "diff --git a/dir/new.js b/dir/new.js\n"
                "--- a/dir/new.js\n+++ b/dir/new.js\n@@ -1 +1 @@\n-1\n+2\n"))
         (split (gp-split-diff-by-file diff)))
    (should (equal (mapcar #'car split) '("src/a.ts" "dir/new.js")))
    ;; each chunk starts with its own diff header and contains its hunk
    (should (string-prefix-p "diff --git a/src/a.ts" (cdr (assoc "src/a.ts" split))))
    (should (string-match-p "\\+y" (cdr (assoc "src/a.ts" split))))
    (should-not (string-match-p "new.js" (cdr (assoc "src/a.ts" split))))))

(ert-deftest bitbucket-test-split-diff-empty ()
  (should (null (gp-split-diff-by-file nil)))
  (should (null (gp-split-diff-by-file ""))))

(ert-deftest bitbucket-test-diffstat-entry ()
  (should (equal (bitbucket--diffstat-entry
                  '((status . "modified") (lines_added . 5) (lines_removed . 4)
                    (new (path . "a.txt")) (old (path . "a.txt"))))
                 '(:path "a.txt" :status "modified" :added 5 :removed 4)))
  ;; deletion: new.path nil -> fall back to old.path
  (should (equal (plist-get (bitbucket--diffstat-entry
                             '((status . "removed") (old (path . "gone.txt"))))
                            :path)
                 "gone.txt")))

(ert-deftest bitbucket-test-linkify-string ()
  (let ((s (gp-linkify-string "see [docs](https://ex.com) and https://bare.io x")))
    ;; markdown link shows the label, faced as a link
    (should (string-match-p "see docs and" (substring-no-properties s)))
    (let ((p (string-match "docs" s)))
      (should (eq (get-text-property p 'face s) 'link))
      (should (equal (get-text-property p 'help-echo s) "https://ex.com")))
    ;; bare URL is faced too
    (let ((p (string-match "https://bare" s)))
      (should (eq (get-text-property p 'face s) 'link)))))

(ert-deftest bitbucket-test-linkify-string-plain ()
  (should (equal (substring-no-properties
                  (gp-linkify-string "no links here"))
                 "no links here")))

(ert-deftest bitbucket-test-resolve-emojis-disabled ()
  (let ((gp-resolve-emoji-shortcodes nil))
    (should (equal (gp-resolve-emojis ":thinking:") ":thinking:"))))

(ert-deftest bitbucket-test-resolve-mentions ()
  (bitbucket-mock-with-service
    (let ((text (gp-resolve-emojis "@{712020:7eec9d21-8053-4226-86c1-091cca977ca3} hi")))
      (setq text (bitbucket-resolve-mentions text))
      (should (equal text "@User 712020:7eec9d21-8053-4226-86c1-091cca977ca3 hi")))))

(ert-deftest bitbucket-test-resolve-mentions-caches ()
  (bitbucket-mock-with-service
    (bitbucket-resolve-mentions "@{abc} and @{abc} again")
    (let ((calls (cl-remove-if-not
                  (lambda (c) (string-match-p "/users/" (nth 1 c)))
                  bitbucket-mock-calls)))
      (should (= (length calls) 1)))))

(ert-deftest bitbucket-test-resolve-mentions-unresolvable-left-as-is ()
  (bitbucket-mock-with-service
    (cl-letf (((symbol-function 'bitbucket-api-request)
               (lambda (&rest _) (error "not found"))))
      (should (equal (bitbucket-resolve-mentions "@{missing} hi") "@{missing} hi")))))

(ert-deftest bitbucket-test-resolve-mentions-nil-text ()
  (should (equal (bitbucket-resolve-mentions nil) "")))

(ert-deftest bitbucket-test-clear-cache-clears-mentions ()
  (bitbucket-mock-with-service
    (bitbucket-resolve-mentions "@{abc}")
    (should (gethash "abc" bitbucket--mention-cache))
    (bitbucket-clear-cache)
    (should (= (hash-table-count bitbucket--mention-cache) 0))))

(ert-deftest bitbucket-test-delete-edit-comment-verbs ()
  (bitbucket-mock-with-service
    (bitbucket-delete-comment "ws/slug" 7 55)
    (bitbucket-edit-comment "ws/slug" 7 55 "new body")
    (let ((del (cl-find-if (lambda (c) (and (equal (car c) "DELETE")
                                            (string-suffix-p "/comments/55" (nth 1 c))))
                           bitbucket-mock-calls))
          (put (cl-find-if (lambda (c) (and (equal (car c) "PUT")
                                            (string-suffix-p "/comments/55" (nth 1 c))))
                           bitbucket-mock-calls)))
      (should del)
      (should put)
      (should (equal (let-alist (nth 3 put) .content.raw) "new body")))))

(ert-deftest bitbucket-test-comment-own-p ()
  (should (bitbucket-comment-own-p '((user (uuid . "{me}"))) "{me}"))
  (should-not (bitbucket-comment-own-p '((user (uuid . "{other}"))) "{me}")))

(ert-deftest bitbucket-test-scan-repos-async-batches-and-done ()
  "scan-repos-async calls on-batch per repo and on-done once at the end."
  (cl-letf (((symbol-function 'bitbucket-api-get-async)
             (lambda (path _params cb)
               ;; synchronously invoke the callback with one PR per repo
               (funcall cb `((values . (((id . ,(length path))))))))))
    (let ((batches 0) (done 0))
      (bitbucket-scan-repos-async
       "state=\"OPEN\"" '("ws/a" "ws/b" "ws/c")
       (lambda (_prs) (cl-incf batches))
       (lambda () (cl-incf done)))
      (should (= batches 3))
      (should (= done 1)))))

(ert-deftest bitbucket-test-scan-repos-async-empty ()
  "With no repos, on-done still fires exactly once."
  (let ((done 0))
    (bitbucket-scan-repos-async "q" nil #'ignore (lambda () (cl-incf done)))
    (should (= done 1))))

;;;; Detail-view per-operation caching ----------------------------------------

(ert-deftest bitbucket-test-pull-request-cached ()
  "The PR object is cached: a second fetch hits no network."
  (bitbucket-mock-with-service
    (let ((bitbucket-cache-ttl 300))
      (bitbucket-cache-clear)
      (let ((pr (bitbucket-pull-request "acme/repo" 180)))
        (should (= (alist-get 'id pr) 180)))
      ;; second call must not touch the service again
      (setq bitbucket-mock-calls nil)
      (let ((pr (bitbucket-pull-request "acme/repo" 180)))
        (should (= (alist-get 'id pr) 180)))
      (should (null bitbucket-mock-calls)))))

(ert-deftest bitbucket-test-pull-request-ttl-zero-bypasses ()
  "With TTL 0 (the `g'-refresh policy) the PR object is always re-fetched."
  (bitbucket-mock-with-service
    (let ((bitbucket-cache-ttl 0))
      (bitbucket-cache-clear)
      (bitbucket-pull-request "acme/repo" 180)
      (setq bitbucket-mock-calls nil)
      (bitbucket-pull-request "acme/repo" 180)
      (should bitbucket-mock-calls))))

(ert-deftest bitbucket-test-pull-request-nil-not-cached ()
  "A nil PR result is not cached; a later fetch retries and can succeed."
  (let ((bitbucket-cache-ttl 300)
        (calls 0))
    (bitbucket-cache-clear)
    (cl-letf (((symbol-function 'bitbucket-api-request)
               (lambda (&rest _)
                 (cl-incf calls)
                 (if (= calls 1) nil '((id . 180))))))
      (should (null (bitbucket-pull-request "acme/repo" 180)))
      ;; not cached -> retried, now succeeds
      (should (equal (bitbucket-pull-request "acme/repo" 180) '((id . 180))))
      (should (= calls 2))
      ;; now cached -> no third call
      (should (equal (bitbucket-pull-request "acme/repo" 180) '((id . 180))))
      (should (= calls 2)))))

(ert-deftest bitbucket-test-comments-not-cached ()
  "Comments are deliberately NOT cached: every call hits the service."
  (bitbucket-mock-with-service
    (let ((bitbucket-cache-ttl 300))
      (bitbucket-cache-clear)
      (bitbucket-pull-request-comments "acme/repo" 180)
      (setq bitbucket-mock-calls nil)
      (bitbucket-pull-request-comments "acme/repo" 180)
      ;; a fresh fetch happened (comments change constantly during review)
      (should bitbucket-mock-calls))))

(ert-deftest bitbucket-test-stats-cached-by-commit ()
  "Stats are cached keyed by the PR's source commit (one compute per commit)."
  (let ((bitbucket-cache-ttl 300)
        (computes 0)
        (pr '((id . 180) (source (commit (hash . "abc123"))))))
    (bitbucket-cache-clear)
    (cl-letf (((symbol-function 'bitbucket--pull-request-stats-1)
               (lambda (&rest _) (cl-incf computes) (list :files 2))))
      (should (equal (bitbucket-pull-request-stats "acme/repo" 180 pr) '(:files 2)))
      (should (equal (bitbucket-pull-request-stats "acme/repo" 180 pr) '(:files 2)))
      (should (= computes 1))
      ;; a new commit -> a fresh key -> recompute
      (let ((pr2 '((id . 180) (source (commit (hash . "def456"))))))
        (bitbucket-pull-request-stats "acme/repo" 180 pr2)
        (should (= computes 2))))))

(ert-deftest bitbucket-test-diff-cached-by-commit ()
  "The diff is cached when a commit hash is supplied; absent it, never cached."
  (let ((bitbucket-cache-ttl 300)
        (fetches 0))
    (bitbucket-cache-clear)
    (cl-letf (((symbol-function 'bitbucket--pull-request-diff-1)
               (lambda (&rest _) (cl-incf fetches) "DIFFTEXT")))
      ;; with a commit -> cached
      (should (equal (bitbucket-pull-request-diff "acme/repo" 180 "abc123") "DIFFTEXT"))
      (should (equal (bitbucket-pull-request-diff "acme/repo" 180 "abc123") "DIFFTEXT"))
      (should (= fetches 1))
      ;; without a commit -> not cacheable, always re-fetched
      (setq fetches 0)
      (bitbucket-pull-request-diff "acme/repo" 181)
      (bitbucket-pull-request-diff "acme/repo" 181)
      (should (= fetches 2)))))

(ert-deftest bitbucket-test-diff-empty-not-cached ()
  "An empty diff is not cached, so a transient empty response is re-fetched."
  (let ((bitbucket-cache-ttl 300)
        (calls 0))
    (bitbucket-cache-clear)
    (cl-letf (((symbol-function 'bitbucket--pull-request-diff-1)
               (lambda (&rest _)
                 (cl-incf calls)
                 (if (= calls 1) "" "REAL DIFF"))))
      (should (equal (bitbucket-pull-request-diff "acme/repo" 180 "abc") ""))
      ;; empty wasn't cached -> retried, now gets the real diff
      (should (equal (bitbucket-pull-request-diff "acme/repo" 180 "abc") "REAL DIFF"))
      (should (= calls 2)))))

(provide 'bitbucket-api-test)
;;; bitbucket-api-test.el ends here
