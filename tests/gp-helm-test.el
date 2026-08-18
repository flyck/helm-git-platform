;;; gp-helm-test.el --- Tests for the helm front-end builders -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit-tests the pure candidate/parse helpers of gp-helm.el.
;; The interactive `helm' sessions themselves are not launched in batch;
;; we test the data they are built from.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gp-helm)
(require 'bitbucket-mock)

(ert-deftest gp-test-helm-pr-candidates ()
  (let* ((prs (alist-get 'values (bitbucket-mock--fixture "workspace-prs.json")))
         (cands (gp-helm--pr-candidates prs)))
    (should (= (length cands) (length prs)))
    ;; each candidate is (DISPLAY-STRING . PR-ALIST)
    (should (stringp (caar cands)))
    (should (eq (cdar cands) (car prs)))
    ;; display carries the PR id
    (should (string-match-p (format "#%s" (alist-get 'id (car prs)))
                            (caar cands)))))

(ert-deftest gp-test-helm-pr-display-columns ()
  "The display line has id, title, repo and author as distinct columns."
  (let* ((pr '((id . 42) (title . "Add a thing")
               (destination (repository (slug . "my-repo")))
               (author (display_name . "Ann Smith"))
               (comment_count . 3)))
         (line (gp-helm--pr-display pr))
         (plain (substring-no-properties line)))
    (should (string-match-p "#42" plain))
    (should (string-match-p "Add a thing" plain))
    (should (string-match-p "my-repo" plain))
    (should (string-match-p "Ann Smith" plain))
    (should (string-match-p "💬3" plain))
    ;; the id column is faced distinctly (found at the "#42" position)
    (should (eq (get-text-property (string-match "#42" line) 'face line)
                'gp-helm-id-face))))

(ert-deftest gp-test-pr-review-tally ()
  (let ((pr '((participants
               . (((role . "REVIEWER") (state . "approved"))
                  ((role . "REVIEWER") (approved . t) (state . nil))
                  ((role . "REVIEWER") (state . "changes_requested"))
                  ((role . "REVIEWER") (state . nil))
                  ((role . "PARTICIPANT") (state . nil)))))))   ;; ignored
    (let ((tally (gp-pr-review-tally pr)))
      (should (= (plist-get tally :approved) 2))
      (should (= (plist-get tally :changes) 1))
      (should (= (plist-get tally :pending) 1)))))

(ert-deftest gp-test-helm-review-badge ()
  "Reads the async `gp-helm--review-tally-cache' by PR id -- populating
it here simulates `gp-helm--scan-review-tallies-async''s fetch having
already landed (see `gp-test-helm-pipeline-bubble' for the identical
pattern with pipeline status)."
  (let ((gp-helm--review-tally-cache (make-hash-table :test 'eql))
        (pr '((id . 1))))
    (puthash 1 '(:approved 1 :changes 1 :pending 1) gp-helm--review-tally-cache)
    (let ((gp-helm-review-style 'tally))
      (let ((s (substring-no-properties (gp-helm--review-badge pr))))
        (should (string-match-p "✅1" s))
        (should (string-match-p "❌1" s))
        (should (string-match-p "⏳1" s))))
    (let ((gp-helm-review-style 'dots))
      (should (string-match-p "✅❌⏳"
                              (substring-no-properties
                               (gp-helm--review-badge pr)))))
    (let ((gp-helm-review-style nil))
      (should (equal (gp-helm--review-badge pr) "")))))

(ert-deftest gp-test-helm-review-badge-empty ()
  "No reviewers (all-zero tally) -> empty badge."
  (let ((gp-helm--review-tally-cache (make-hash-table :test 'eql))
        (pr '((id . 1))))
    (puthash 1 '(:approved 0 :changes 0 :pending 0) gp-helm--review-tally-cache)
    (should (equal (gp-helm--review-badge pr) ""))))

(ert-deftest gp-test-helm-review-badge-loading ()
  "Not yet fetched -> empty badge (same \"loading\" convention as the
pipeline bubble's neutral state, just blank instead of a glyph since
the badge has no natural neutral symbol)."
  (let ((gp-helm--review-tally-cache (make-hash-table :test 'eql))
        (pr '((id . 1))))
    (should (equal (gp-helm--review-badge pr) ""))))

(ert-deftest gp-test-helm-scan-review-tallies-async-populates-cache ()
  (let ((gp-helm--review-tally-cache (make-hash-table :test 'eql))
        (pr '((id . 1))))
    (cl-letf (((symbol-function 'gp-pr-review-tally-async)
               (lambda (_pr callback) (funcall callback '(:approved 2 :changes 0 :pending 0))))
              ((symbol-function 'gp-helm--refresh-if-alive) #'ignore))
      (gp-helm--scan-review-tallies-async (list pr))
      (should (equal (gethash 1 gp-helm--review-tally-cache)
                     '(:approved 2 :changes 0 :pending 0))))))

(ert-deftest gp-test-build-states-summary ()
  (should (null (gp-build-states-summary nil)))
  (should (eq (gp-build-states-summary '("SUCCESSFUL" "FAILED")) 'failed))
  (should (eq (gp-build-states-summary '("SUCCESSFUL" "INPROGRESS")) 'running))
  (should (eq (gp-build-states-summary '("STOPPED")) 'stopped))
  (should (eq (gp-build-states-summary '("SUCCESSFUL" "SUCCESSFUL")) 'successful)))

(ert-deftest gp-test-helm-pipeline-bubble ()
  (let ((gp-helm--pipeline-cache (make-hash-table :test 'equal))
        (pr '((source (commit (hash . "abc"))))))
    ;; not yet fetched -> loading bubble
    (should (string-match-p "⚫" (gp-helm--pipeline-bubble pr)))
    (gp-helm--pipeline-cache-put "abc" 'failed)
    (should (string-match-p "🔴" (gp-helm--pipeline-bubble pr)))
    (gp-helm--pipeline-cache-put "abc" 'running)
    (should (string-match-p "🔵" (gp-helm--pipeline-bubble pr)))
    (gp-helm--pipeline-cache-put "abc" 'successful)
    (should (string-match-p "🟢" (gp-helm--pipeline-bubble pr)))
    (gp-helm--pipeline-cache-put "abc" 'stopped)
    (should (string-match-p "⚪" (gp-helm--pipeline-bubble pr)))))

(ert-deftest gp-test-helm-pipeline-bubble-github-shape ()
  "GitHub PRs carry the head commit at .head.sha, not .source.commit.hash.
The bubble must resolve the hash through `gp-pr-source-commit' so it
reads the same cache key `gp-helm--scan-pipelines-async' wrote."
  (require 'git-platform-github)
  (let* ((git-platform-current-backend (git-platform-github))
         (gp-helm--pipeline-cache (make-hash-table :test 'equal))
         (pr '((head (sha . "deadbeef")))))
    (should (string-match-p "⚫" (gp-helm--pipeline-bubble pr)))
    (gp-helm--pipeline-cache-put "deadbeef" 'successful)
    (should (string-match-p "🟢" (gp-helm--pipeline-bubble pr)))
    (gp-helm--pipeline-cache-put "deadbeef" 'failed)
    (should (string-match-p "🔴" (gp-helm--pipeline-bubble pr)))))

(ert-deftest gp-test-helm-pipeline-cache-expires-non-terminal-states ()
  "A running build must not be cached forever.
This is the bug where a PR scanned mid-build kept its blue bubble
long after CI went green: the entry was written once and the scan
only ever re-fetched on a miss, so it was never revisited."
  (let ((gp-helm--pipeline-cache (make-hash-table :test 'equal))
        (gp-helm-pipeline-settling-ttl 60))
    (gp-helm--pipeline-cache-put "abc" 'running)
    ;; fresh: still served
    (should (equal (gp-helm--pipeline-cache-get "abc") '(t . running)))
    ;; once the settling window passes it reads as a miss, so the next
    ;; scan re-fetches and can discover the build finished
    (let ((later (+ (float-time) 100)))
      (cl-letf (((symbol-function 'float-time) (lambda (&rest _) later)))
        (should-not (car (gp-helm--pipeline-cache-get "abc")))))))

(ert-deftest gp-test-helm-pipeline-cache-keeps-terminal-states ()
  "Terminal verdicts describe an immutable commit, so they never expire."
  (let ((gp-helm--pipeline-cache (make-hash-table :test 'equal))
        (gp-helm-pipeline-settling-ttl 60))
    (dolist (state '(failed successful stopped))
      (gp-helm--pipeline-cache-put "abc" state)
      ;; no expiry stored at all
      (should (null (car (gethash "abc" gp-helm--pipeline-cache))))
      ;; and still a hit far in the future
      (let ((later (+ (float-time) 86400)))
        (cl-letf (((symbol-function 'float-time) (lambda (&rest _) later)))
          (should (equal (gp-helm--pipeline-cache-get "abc") (cons t state))))))))

(ert-deftest gp-test-helm-pipeline-cache-nil-state-is-not-terminal ()
  "\"No build reported\" is not a verdict -- CI may not have registered yet.
Caching it forever would blank the bubble permanently for a PR whose
pipeline starts a moment later."
  (should-not (gp-helm--pipeline-terminal-p nil))
  (should-not (gp-helm--pipeline-terminal-p 'running))
  (should (gp-helm--pipeline-terminal-p 'failed))
  (let ((gp-helm--pipeline-cache (make-hash-table :test 'equal))
        (gp-helm-pipeline-settling-ttl 60))
    (gp-helm--pipeline-cache-put "abc" nil)
    (should (car (gethash "abc" gp-helm--pipeline-cache)))))

(ert-deftest gp-test-helm-pipeline-bubble-refetches-after-settling ()
  "End to end: the bubble goes blue, then green once the state is re-read."
  (let* ((gp-helm--pipeline-cache (make-hash-table :test 'equal))
         (gp-helm-pipeline-settling-ttl 60)
         (pr '((source (commit (hash . "abc"))))))
    (gp-helm--pipeline-cache-put "abc" 'running)
    (should (string-match-p "🔵" (gp-helm--pipeline-bubble pr)))
    ;; after the window the stale running entry no longer wins; the bubble
    ;; falls back to the loading glyph until the re-fetch lands
    (let ((later (+ (float-time) 100)))
      (cl-letf (((symbol-function 'float-time) (lambda (&rest _) later)))
        (should (string-match-p "⚫" (gp-helm--pipeline-bubble pr)))))
    (gp-helm--pipeline-cache-put "abc" 'successful)
    (should (string-match-p "🟢" (gp-helm--pipeline-bubble pr)))))

(ert-deftest gp-test-helm-pad-truncates-and-faces ()
  (should (= (string-width (gp-helm--pad "abcdef" 4)) 4))
  (should (string-suffix-p "…" (gp-helm--pad "abcdef" 4)))
  (should (eq (get-text-property 0 'face (gp-helm--pad "x" 3 'bold)) 'bold)))

(ert-deftest gp-test-helm-draft-rows-dimmed ()
  (let ((line (gp-helm--pr-display
               '((id . 1) (title . "wip") (author (display_name . "me"))
                 (destination (repository (slug . "r"))))
               t)))
    (should (eq (get-text-property 0 'face line) 'gp-helm-draft-face))))

(ert-deftest gp-test-helm-header-shows-count ()
  (should (equal (gp-helm--header "My drafts" '(a b c)) "My drafts (3)"))
  (should (equal (gp-helm--header "Empty" nil) "Empty (0)")))

(ert-deftest gp-test-helm-source-omits-empty ()
  "An empty section yields no source (so it isn't shown)."
  (should (null (gp-helm--source "Empty" nil nil)))
  (when (require 'helm nil t)
    (should (gp-helm--source
             "Full" '(((id . 1) (title . "t")
                       (author (display_name . "a"))
                        (destination (repository (slug . "r")))))
              nil))))

(ert-deftest gp-test-helm-prs-for-branch ()
  (let* ((prs '(((id . 1) (source (branch (name . "feat/a"))))
                ((id . 2) (source (branch (name . "feat/b"))))
                ((id . 3) (source (branch (name . "feat/a"))))))
         (matches (gp-helm--prs-for-branch prs "feat/a")))
    (should (equal (mapcar (lambda (pr) (alist-get 'id pr)) matches)
                   '(1 3)))))

(ert-deftest gp-test-helm-default-action-is-detail ()
  "RET / click should open the detail buffer (first action)."
  (should (eq (cdr (car (gp-helm--pr-actions))) #'gp-show-pr)))

(ert-deftest gp-test-helm-diff-files ()
  (let ((diff (concat
               "diff --git a/src/a.ts b/src/a.ts\n"
               "--- a/src/a.ts\n+++ b/src/a.ts\n@@ -1 +1 @@\n-x\n+y\n"
               "diff --git a/old.txt b/old.txt\n"
               "deleted file mode 100644\n"
               "--- a/old.txt\n+++ /dev/null\n"
               "diff --git a/dir/new.js b/dir/new.js\n"
               "--- a/dir/new.js\n+++ b/dir/new.js\n@@ -1 +1 @@\n-1\n+2\n")))
    (should (equal (gp-helm--diff-files diff)
                   '("src/a.ts" "dir/new.js")))   ;; /dev/null deletion dropped
    (should (equal (gp-helm--file-candidates diff)
                   '(("src/a.ts" . "src/a.ts")
                     ("dir/new.js" . "dir/new.js"))))))

(ert-deftest gp-test-helm-comment-candidates ()
  (let* ((comments (alist-get 'values (bitbucket-mock--fixture "pr-comments.json")))
         (cands (gp-helm--comment-candidates comments)))
    (should (= (length cands) (length comments)))
    (should (stringp (caar cands)))
    ;; an inline comment's display includes its file:line location head
    (let ((inline-cand
           (cl-find-if (lambda (c) (let-alist (cdr c) .inline.path)) cands)))
      (should inline-cand)
      ;; display begins with the (possibly truncated) file path location
      (let-alist (cdr inline-cand)
        (should (string-prefix-p (substring .inline.path 0 10)
                                 (string-trim-left (car inline-cand))))))))

(ert-deftest gp-test-helm-pr-actions-shape ()
  "The PR action alist exposes the key drill-downs and the checkout."
  (let ((actions (gp-helm--pr-actions)))
    (should (assoc "Browse files (helm)" actions))
    (should (assoc "Browse comments (helm)" actions))
    (should (assoc "Checkout branch & open" actions))
    (should (cl-every (lambda (a) (functionp (cdr a))) actions))))

(ert-deftest gp-test-helm-buffer-names-are-tagged ()
  "Helm session buffers carry the shared prefix like every other buffer.
`gp-helm-buffer' is also looked up by name in `gp-helm--title-width',
so it must stay a single source of truth rather than a literal."
  (should (string-prefix-p "*gp: " gp-helm-buffer))
  (dolist (suffix '("files" "comments" "open" "repo" "repo-branch"))
    (should (string-prefix-p "*gp: " (gp-helm--buffer suffix)))))


;;;; Reviewing partition --------------------------------------------------------

(ert-deftest gp-test-helm-partition-reviewing-splits-on-own-approval ()
  "PRs the user already approved move out of the needs-action list."
  (cl-letf (((symbol-function 'gp-pr-my-review-state)
             (lambda (pr _uuid) (alist-get 'my-state pr))))
    (let* ((todo1 '((id . 1)))
           (todo2 '((id . 2) (my-state . nil)))
           (done1 '((id . 3) (my-state . approved)))
           (res (gp-helm--partition-reviewing
                 (list todo1 done1 todo2) "{me}")))
      (should (equal (mapcar (lambda (p) (alist-get 'id p)) (nth 0 res)) '(1 2)))
      (should (equal (mapcar (lambda (p) (alist-get 'id p)) (nth 1 res)) '(3))))))

(ert-deftest gp-test-helm-changes-requested-stays-pending ()
  "A PR sent back with changes requested is still the user's to re-review.
So it stays under \"Needs my review\" rather than being filed away as
done in \"Approved by me\"."
  (cl-letf (((symbol-function 'gp-pr-my-review-state)
             (lambda (pr _uuid) (alist-get 'my-state pr))))
    (let ((res (gp-helm--partition-reviewing
                '(((id . 1) (my-state . changes))
                  ((id . 2) (my-state . approved)))
                "{me}")))
      (should (equal (mapcar (lambda (p) (alist-get 'id p)) (nth 0 res)) '(1)))
      (should (equal (mapcar (lambda (p) (alist-get 'id p)) (nth 1 res)) '(2))))))

(ert-deftest gp-test-helm-partition-reviewing-preserves-order ()
  "Both halves keep the incoming order, so the list does not jump around."
  (cl-letf (((symbol-function 'gp-pr-my-review-state)
             (lambda (pr _uuid) (alist-get 'my-state pr))))
    (let ((res (gp-helm--partition-reviewing
                '(((id . 1)) ((id . 2)) ((id . 3))) "{me}")))
      (should (equal (mapcar (lambda (p) (alist-get 'id p)) (nth 0 res))
                     '(1 2 3)))
      (should (null (nth 1 res)))
      (should (null (nth 2 res))))))

(ert-deftest gp-test-helm-my-vote-tolerates-missing-backend-op ()
  "A backend without `gp-pr-my-review-state' reports no vote, not an error.
The mock has no implementation, so this must degrade rather than break
the whole list."
  (cl-letf (((symbol-function 'gp-pr-my-review-state)
             (lambda (&rest _) (error "no applicable method"))))
    (should-not (gp-helm--my-vote '((id . 1)) "{me}"))
    (should-not (gp-helm--voted-p '((id . 1)) "{me}"))))

(ert-deftest gp-test-helm-my-vote-nil-uuid-is-no-vote ()
  "With no known identity nothing is classed as already-voted."
  (should-not (gp-helm--my-vote '((id . 1)) nil)))


;;;; Quorum: covered by other reviewers -----------------------------------------

(defmacro gp-test-with-reviewers (alist &rest body)
  "Run BODY with `gp-helm--reviewers-cache' seeded from ALIST (id . plists)."
  (declare (indent 1))
  `(let ((gp-helm--reviewers-cache (make-hash-table :test 'eql)))
     (dolist (kv ,alist)
       (puthash (car kv) (cdr kv) gp-helm--reviewers-cache))
     ,@body))

(ert-deftest gp-test-helm-covered-counts-only-others ()
  "The user's own approval does not count toward the quorum."
  (let ((gp-helm-min-approvals 2) (gp-helm-min-rejections 2))
    ;; two approvals, but one is the user's -> only one other -> not covered
    (gp-test-with-reviewers
        '((1 . ((:id "{me}" :state approved)
                (:id "{a}" :state approved))))
      (should-not (gp-helm--covered-by-others-p '((id . 1)) "{me}")))
    ;; two approvals from other people -> covered
    (gp-test-with-reviewers
        '((1 . ((:id "{a}" :state approved)
                (:id "{b}" :state approved))))
      (should (gp-helm--covered-by-others-p '((id . 1)) "{me}")))))

(ert-deftest gp-test-helm-covered-by-rejections ()
  "Enough changes-requested from others also covers the PR."
  (let ((gp-helm-min-approvals 2) (gp-helm-min-rejections 2))
    (gp-test-with-reviewers
        '((1 . ((:id "{a}" :state changes) (:id "{b}" :state changes))))
      (should (gp-helm--covered-by-others-p '((id . 1)) "{me}")))
    (gp-test-with-reviewers
        '((1 . ((:id "{a}" :state changes))))
      (should-not (gp-helm--covered-by-others-p '((id . 1)) "{me}")))))

(ert-deftest gp-test-helm-covered-unknown-is-not-covered ()
  "A PR whose reviewer data has not arrived is never treated as covered.
The scan is asynchronous; assuming quorum would make rows vanish from
the pending list and then reappear once the data lands."
  (let ((gp-helm-min-approvals 2))
    (gp-test-with-reviewers '()
      (should-not (gp-helm--covered-by-others-p '((id . 1)) "{me}")))))

(ert-deftest gp-test-helm-covered-threshold-zero-disables ()
  "A threshold of 0 switches that half of the rule off."
  (gp-test-with-reviewers
      '((1 . ((:id "{a}" :state approved) (:id "{b}" :state approved))))
    (let ((gp-helm-min-approvals 0) (gp-helm-min-rejections 2))
      (should-not (gp-helm--covered-by-others-p '((id . 1)) "{me}")))))

(ert-deftest gp-test-helm-partition-three-way ()
  "Own approval beats quorum: it lands in APPROVED, not COVERED."
  (cl-letf (((symbol-function 'gp-pr-my-review-state)
             (lambda (pr _uuid) (alist-get 'my-state pr))))
    (let ((gp-helm-min-approvals 2) (gp-helm-min-rejections 2))
      (gp-test-with-reviewers
          '((1 . ((:id "{a}" :state pending)))
            (2 . ((:id "{a}" :state approved) (:id "{b}" :state approved)))
            (3 . ((:id "{a}" :state approved) (:id "{b}" :state approved))))
        (let* ((todo '((id . 1)))
               (covered '((id . 2)))
               (mine '((id . 3) (my-state . approved)))
               (res (gp-helm--partition-reviewing (list todo covered mine) "{me}")))
          (should (equal (mapcar (lambda (p) (alist-get 'id p)) (nth 0 res)) '(1)))
          (should (equal (mapcar (lambda (p) (alist-get 'id p)) (nth 1 res)) '(3)))
          (should (equal (mapcar (lambda (p) (alist-get 'id p)) (nth 2 res)) '(2))))))))

(provide 'gp-helm-test)
;;; gp-helm-test.el ends here
