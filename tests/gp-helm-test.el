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
    (puthash "abc" 'failed gp-helm--pipeline-cache)
    (should (string-match-p "🔴" (gp-helm--pipeline-bubble pr)))
    (puthash "abc" 'running gp-helm--pipeline-cache)
    (should (string-match-p "🔵" (gp-helm--pipeline-bubble pr)))
    (puthash "abc" 'successful gp-helm--pipeline-cache)
    (should (string-match-p "🟢" (gp-helm--pipeline-bubble pr)))
    (puthash "abc" 'stopped gp-helm--pipeline-cache)
    (should (string-match-p "⚪" (gp-helm--pipeline-bubble pr)))))

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

(ert-deftest gp-test-helm-partition-reviewing-splits-on-own-vote ()
  "PRs the user already voted on move out of the needs-action list."
  (cl-letf (((symbol-function 'gp-pr-my-review-state)
             (lambda (pr _uuid) (alist-get 'my-state pr))))
    (let* ((todo1 '((id . 1)))
           (todo2 '((id . 2) (my-state . nil)))
           (done1 '((id . 3) (my-state . approved)))
           (done2 '((id . 4) (my-state . changes)))
           (res (gp-helm--partition-reviewing
                 (list todo1 done1 todo2 done2) "{me}")))
      (should (equal (mapcar (lambda (p) (alist-get 'id p)) (car res)) '(1 2)))
      (should (equal (mapcar (lambda (p) (alist-get 'id p)) (cdr res)) '(3 4))))))

(ert-deftest gp-test-helm-partition-reviewing-preserves-order ()
  "Both halves keep the incoming order, so the list does not jump around."
  (cl-letf (((symbol-function 'gp-pr-my-review-state)
             (lambda (pr _uuid) (alist-get 'my-state pr))))
    (let ((res (gp-helm--partition-reviewing
                '(((id . 1)) ((id . 2)) ((id . 3))) "{me}")))
      (should (equal (mapcar (lambda (p) (alist-get 'id p)) (car res))
                     '(1 2 3)))
      (should (null (cdr res))))))

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

(provide 'gp-helm-test)
;;; gp-helm-test.el ends here
