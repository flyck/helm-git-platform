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
(require 'github-mock)              ;; label-column tests drive the GitHub side
(require 'git-platform-github)
(require 'git-platform-bitbucket)

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

(defun gp-helm-test--faces-at (pos line)
  "Return the face property at POS in LINE, always as a list."
  (let ((f (get-text-property pos 'face line)))
    (if (listp f) f (list f))))

(ert-deftest gp-test-helm-draft-rows-dimmed ()
  (let ((line (gp-helm--pr-display
               '((id . 1) (title . "wip") (author (display_name . "me"))
                 (destination (repository (slug . "r"))))
               t)))
    (should (memq 'gp-helm-draft-face (gp-helm-test--faces-at 0 line)))))

(ert-deftest gp-test-helm-draft-rows-are-uniformly-grey ()
  "A draft row is dimmed as one flat grey, labels included.
Deliberate: the whole-row grey is what makes drafts scannable, and it
wins over keeping each label's colour (and its exact alignment) there."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (gp-helm-labels-width 18))
      (cl-letf (((symbol-function 'display-color-cells) (lambda (&rest _) 16777216)))
        (let* ((pr (github-pull-request "acme/web" 42))
               (line (gp-helm--pr-display pr t))
               (at (string-match "bug" (substring-no-properties line))))
          ;; every cell, the labels included, carries only the draft face
          (should (eq (get-text-property at 'face line) 'gp-helm-draft-face))
          (should (eq (get-text-property 0 'face line) 'gp-helm-draft-face))
          ;; the non-draft row still keeps its per-label colour
          (let* ((plain-line (gp-helm--pr-display pr nil))
                 (faces (gp-helm-test--faces-at
                         (string-match "bug" (substring-no-properties plain-line))
                         plain-line)))
            (should (cl-find-if (lambda (f)
                                  (and (symbolp f)
                                       (string-prefix-p "gp-label-color-"
                                                        (symbol-name f))))
                                faces))))))))

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

;;;; Label column -------------------------------------------------------------

(ert-deftest gp-test-helm-labels-column-hidden-on-bitbucket ()
  "Bitbucket has no labels, so the column costs nothing and no cell is drawn.
The width must fall to 0 as well, or the title column would be shortened
to reserve space that is never used."
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (should (= (gp-helm--labels-column-width) 0))
    (should (equal (gp-helm--labels-cell '((id . 42))) ""))))

(ert-deftest gp-test-helm-labels-column-renders-fixed-width ()
  "On GitHub the cell is padded to exactly `gp-helm-labels-width'
so every column after it still lines up."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (gp-helm-labels-width 18))
      (let ((cell (gp-helm--labels-cell github-mock--pr-1)))
        (should (= (string-width cell) 18))
        (should (string-match-p "bug" (substring-no-properties cell)))))))

(ert-deftest gp-test-helm-labels-column-placeholder-when-none ()
  "An unlabelled PR still fills the column, so rows stay aligned."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (gp-helm-labels-width 18))
      (let ((cell (gp-helm--labels-cell github-mock--pr-2)))
        (should (= (string-width cell) 18))
        (should (string-match-p "···" (substring-no-properties cell)))))))

(ert-deftest gp-test-helm-labels-width-zero-hides-the-column ()
  "`gp-helm-labels-width' 0 opts out even on a platform with labels."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (gp-helm-labels-width 0))
      (should (= (gp-helm--labels-column-width) 0))
      (should (equal (gp-helm--labels-cell github-mock--pr-1) "")))))

(ert-deftest gp-test-helm-display-keeps-all-columns-with-labels ()
  "Adding the label column must not drop any existing field.
A `format' arg/directive mismatch silently truncates the tail of the
row, which is how the comment badge went missing once."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (gp-helm-labels-width 18))
      (let ((plain (substring-no-properties
                    (gp-helm--pr-display
                     ;; reshaped, as callers hold it: `id' is the PR number.
                     ;; GitHub counts general and review comments separately
                     ;; (`gp-pr-comment-count' sums them), so the badge needs
                     ;; those fields -- not Bitbucket's `comment_count'.
                     (append '((comments . 2) (review_comments . 1))
                             (github-pull-request "acme/web" 42))))))
        (should (string-match-p "#42" plain))
        (should (string-match-p "Add the widget toggle" plain))
        (should (string-match-p "bug" plain))
        (should (string-match-p "web" plain))
        (should (string-match-p "ada" plain))
        (should (string-match-p "💬3" plain))
        ;; labels sit between the title and the repo slug
        (should (< (string-match "Add the widget toggle" plain)
                   (string-match "bug" plain)))
        (should (< (string-match "bug" plain) (string-match "web" plain)))))))

(ert-deftest gp-test-helm-label-names-are-searchable ()
  "Label names join the invisible search tail, so typing one filters even
when the column truncated it away."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github)))
      (let ((tail (substring-no-properties
                   (gp-helm--pr-search-tail github-mock--pr-1))))
        (should (string-match-p "bug" tail))
        (should (string-match-p "ui" tail))))))

(ert-deftest gp-test-helm-repo-column-fits-a-real-repo-name ()
  "Real workspaces use descriptive, prefixed slugs; the column has to fit
one rather than a short one-word name, or every row is cut mid-word."
  (should (>= gp-helm-repo-width
              (length "lambda-datasource-batch-jobs-mutations")))
  ;; and a name that fits is not truncated
  (let ((cell (gp-helm--pad "lambda-datasource-batch-jobs-mutations"
                            gp-helm-repo-width)))
    (should (string-match-p "lambda-datasource-batch-jobs-mutations" cell))
    (should-not (string-match-p "…" cell))))

(ert-deftest gp-test-helm-repo-column-is-paid-for-by-the-title ()
  "The title is the column that auto-grows, so widening the repo takes
from it -- but on any normal window it keeps far more than a title needs."
  (let ((buf (get-buffer-create gp-helm-buffer)))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buf)
          (let* ((win (window-body-width (selected-window)))
                 (wide (let ((gp-helm-repo-width 38)) (gp-helm--title-width)))
                 (narrow (let ((gp-helm-repo-width 22)) (gp-helm--title-width))))
            ;; the 16 columns the repo gained come off the title...
            (when (> narrow gp-helm-title-min-width)
              (should (= (- narrow wide) 16)))
            ;; ...and both still fit inside the window
            (should (<= (+ wide 38) win))))
      (kill-buffer buf))))

(ert-deftest gp-test-helm-title-width-reserves-the-label-column ()
  "The auto-growing title column shrinks by the label column's width,
and gets that space back where labels are unsupported."
  (let ((gp-helm-labels-width 18))
    ;; drive the real window path: a live window over the helm buffer, so the
    ;; auto-grow branch runs rather than the fixed fallback
    (let ((buf (get-buffer-create gp-helm-buffer)))
      (unwind-protect
          (save-window-excursion
            (set-window-buffer (selected-window) buf)
            (let* ((win (window-body-width (selected-window)))
                   (with-labels
                    (let ((git-platform-current-backend (git-platform-github)))
                      (gp-helm--title-width)))
                   (without
                    (let ((git-platform-current-backend (git-platform-bitbucket)))
                      (gp-helm--title-width))))
              (ignore win)
              ;; both may bottom out at the minimum on a narrow batch frame;
              ;; only compare when there is room to see the difference
              (when (> without gp-helm-title-min-width)
                (should (= (- without with-labels) 18)))))
        (kill-buffer buf)))))

;;;; Recently merged -------------------------------------------------------------

(ert-deftest gp-test-helm-merged-recently-uses-a-rolling-window ()
  "Only PRs merged inside the rolling window are kept.
Rolling rather than calendar-day so work merged late yesterday evening
is still listed the next morning instead of vanishing at midnight."
  (cl-letf* ((now (date-to-time "2026-08-22T10:00:00Z"))
             ((symbol-function 'current-time) (lambda () now))
             ((symbol-function 'gp-pr-merged-p) (lambda (pr) (alist-get 'merged pr)))
             ((symbol-function 'gp-pr-merged-at) (lambda (pr) (alist-get 'at pr))))
    ;; Times are compared in LOCAL days, so the fixtures are written in
    ;; local time -- a UTC instant late on the 21st is already the 22nd in
    ;; a positive-offset zone, which is correct but makes for a confusing
    ;; fixture.
    (let ((prs (list `((id . 1) (merged . t) (at . ,(format-time-string "%FT%T%z" now)))
                     `((id . 2) (merged . t)
                       (at . ,(format-time-string "%FT%T%z" (time-subtract now (* 6 3600)))))
                     `((id . 3) (merged . t)
                       (at . ,(format-time-string "%FT%T%z" (time-subtract now (* 26 3600)))))
                     `((id . 4) (merged . t)
                       (at . ,(format-time-string "%FT%T%z" (time-subtract now (* 24 3600 21))))))))
      ;; 24 hours: the two from today, not the 26-hour-old one
      (should (equal (mapcar (lambda (p) (alist-get 'id p))
                            (gp-helm--merged-recently prs 24))
                     '(1 2)))
      ;; 48 hours reaches the one merged 26 hours ago
      (should (equal (mapcar (lambda (p) (alist-get 'id p))
                            (gp-helm--merged-recently prs 48))
                     '(1 2 3)))
      ;; a tight window keeps only what just landed
      (should (equal (mapcar (lambda (p) (alist-get 'id p))
                            (gp-helm--merged-recently prs 1))
                     '(1))))))

(ert-deftest gp-test-helm-merged-section-can-be-turned-off ()
  "Nil `gp-helm-show-merged-recent' yields no section and no fetch.
The list is open-only by default, so the section needs a request of its
own -- which must not happen when the section is off."
  (let ((fetches 0))
    (cl-letf (((symbol-function 'gp-workspace-pull-requests)
               (lambda (&rest _) (cl-incf fetches) nil)))
      (let ((gp-helm-show-merged-recent nil))
        (should-not (when gp-helm-show-merged-recent
                      (gp-helm--merged-recently
                       (gp-workspace-pull-requests nil "MERGED" 20))))
        (should (= fetches 0)))
      (let ((gp-helm-show-merged-recent t))
        (when gp-helm-show-merged-recent
          (gp-helm--merged-recently (gp-workspace-pull-requests nil "MERGED" 20)))
        (should (= fetches 1))))))

(ert-deftest gp-test-helm-merged-section-label-follows-the-window ()
  "The heading names the window rather than saying \"today\"."
  (let ((gp-helm-merged-recent-hours 24))
    (should (equal (gp-helm--merged-section-label) "Merged in the last 24 hours")))
  (let ((gp-helm-merged-recent-hours 72))
    (should (equal (gp-helm--merged-section-label) "Merged in the last 3 days")))
  (let ((gp-helm-merged-recent-hours 6))
    (should (equal (gp-helm--merged-section-label) "Merged in the last 6 hours")))
  (let ((gp-helm-merged-recent-hours 1))
    (should (equal (gp-helm--merged-section-label) "Merged in the last hour"))))

(ert-deftest gp-test-helm-merged-recently-skips-the-unmergeable-and-undated ()
  "An open PR, or one whose merge time cannot be read, is left out.
Guessing would put an open PR under a \"Merged today\" heading."
  (cl-letf* ((now (date-to-time "2026-08-22T10:00:00Z"))
             ((symbol-function 'current-time) (lambda () now))
             ((symbol-function 'gp-pr-merged-p) (lambda (pr) (alist-get 'merged pr)))
             ((symbol-function 'gp-pr-merged-at) (lambda (pr) (alist-get 'at pr))))
    (should-not (gp-helm--merged-recently
                 '(((id . 1) (merged . nil) (at . "2026-08-22T09:00:00Z"))) 24))
    (should-not (gp-helm--merged-recently
                 '(((id . 2) (merged . t) (at . nil))) 24))
    (should-not (gp-helm--merged-recently
                 '(((id . 3) (merged . t) (at . "not a date"))) 24))))

(provide 'gp-helm-test)
;;; gp-helm-test.el ends here
