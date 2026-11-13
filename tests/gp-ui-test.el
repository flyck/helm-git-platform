;;; gp-ui-test.el --- Tests for the PR list/detail UI -*- lexical-binding: t; -*-

;;; Commentary:
;; Drives the magit-section renderers against mock data in a real
;; (batch) buffer and asserts on the produced text and section tree --
;; this is the "simulate UI" coverage without a live display.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gp-ui)
(require 'gp-create)
(require 'gp-compose)
(require 'gp-reviewers)
(require 'gp-log)
(require 'bitbucket-mock)
(require 'github-mock)
(require 'git-platform-github)
(require 'git-platform-bitbucket)   ;; `git-platform-bitbucket' constructor, used below

(defun gp-test--mock-prs ()
  (alist-get 'values (bitbucket-mock--fixture "workspace-prs.json")))

(ert-deftest gp-test-pr-heading-contains-id-title ()
  (let* ((pr (car (gp-test--mock-prs)))
         (h (substring-no-properties (gp--pr-heading pr))))
    (should (string-match-p (format "#%s" (alist-get 'id pr)) h))
    (should (string-match-p (regexp-quote (alist-get 'title pr)) h))))

(ert-deftest gp-test-pr-heading-linkifies-ticket-key-in-title ()
  "A ticket key in the title becomes a link, without losing the title
face on the rest of the string (see `gp--pr-heading')."
  (let* ((gp-ticket-url-format "https://co.atlassian.net/browse/%s")
         (pr '((id . 42) (title . "WP-1231: add the widget toggle")
               (destination (repository (slug . "web")))
               (author (display_name . "Ada"))))
         (h (gp--pr-heading pr))
         (ticket-pos (string-match "WP-1231" h))
         (rest-pos (string-match "widget toggle" h)))
    (should (eq (get-text-property ticket-pos 'face h) 'link))
    (should (equal (get-text-property ticket-pos 'help-echo h)
                   "https://co.atlassian.net/browse/WP-1231"))
    ;; the rest of the title keeps its own face, not `link'
    (should (eq (get-text-property rest-pos 'face h) 'gp-pr-title-face))))

(ert-deftest gp-test-render-list-builds-groups ()
  "Rendering partitions PRs by authorship into the two groups, with one
section per PR."
  (let* ((uuid "{21d7839d-779f-44b2-8c40-6f43ac90be06}")
         ;; the fixture is all the user's own; add one by somebody else
         (theirs (append '((id . 901) (title . "Someone else's change")
                           (author (display_name . "Bea") (uuid . "{bea}")))
                         (cl-remove-if (lambda (c) (memq (car c) '(id title author)))
                                       (car (gp-test--mock-prs)))))
         (prs (cons theirs (gp-test--mock-prs)))
         (git-platform-current-backend (git-platform-bitbucket)))
    (with-temp-buffer
      (gp-list-mode)
      (let ((inhibit-read-only t))
        (gp--render-list prs uuid))
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "Needs my review (1)" text))
        (should (string-match-p "My pull requests (10)" text))
        ;; the other author's PR is listed under review, before "My pull requests"
        (should (string-match-p "Needs my review (1)\\(?:.\\|\n\\)*#901\\(?:.\\|\n\\)*My pull requests" text))
        ;; a known PR id from the fixture appears
        (should (string-match-p "#239" text)))
      ;; someone else's draft does not need my review
      (let ((inhibit-read-only t))
        (erase-buffer)
        (gp--render-list (cons (append '((id . 902) (draft . t)) theirs) prs) uuid))
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "Needs my review (1)" text))
        (should-not (string-match-p "#902" text)))
      ;; section tree: root has children, and PR sections carry their pr
      (let* ((root magit-root-section)
             (pr-secs (cl-remove-if-not
                       (lambda (s) (object-of-class-p s 'gp-pr-section))
                       (gp-test--all-sections root))))
        (should (= (length pr-secs) 11))
        (should (alist-get 'id (oref (car pr-secs) value)))))))

(defun gp-test--all-sections (section)
  "Flatten SECTION and all descendants into a list."
  (cons section
        (cl-mapcan #'gp-test--all-sections
                   (oref section children))))

;;;; GitHub-shaped PRs render correctly, not just Bitbucket's ----------------
;;
;; Regression coverage for a real bug: gp--pr-heading/gp--insert-pr/
;; gp--render-detail used to read .source.branch.name/.destination.branch.
;; name/.author.display_name/.state directly off the PR alist via
;; let-alist -- correct for Bitbucket's shape, but GitHub's PRs nest
;; branches under .head/.base and the author under .user.login, and use
;; a lowercase "open" state, so every one of those fields silently read
;; as nil and rendered as "?".  These assert against the real GitHub
;; mock fixture (git-platform-github + github-mock--pr-1) specifically
;; so a future call site that bypasses the gp-pr-* accessors again would
;; fail here, not just look fine against Bitbucket's shape.

(defun gp-test--gh-pr ()
  "Return `github-mock--pr-1' as the UI really holds it.
Every producer (`github-pull-request', the list fetch) passes PRs
through `github--reshape-pr', so `id' is the per-repo number 42, not
the raw REST database id."
  (github--reshape-pr github-mock--pr-1))

(ert-deftest gp-test-pr-heading-shows-real-author-and-repo-slug-for-github-pr ()
  (github-mock-with-service
    (let* ((git-platform-current-backend (git-platform-github))
           (pr (gp-test--gh-pr))
           (h (substring-no-properties (gp--pr-heading pr))))
      (should (string-match-p "ada" h))
      (should (string-match-p (regexp-quote "[web]") h)))))

(ert-deftest gp-test-insert-pr-shows-real-branches-for-github-pr ()
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (pr (gp-test--gh-pr)))
      (with-temp-buffer
        (gp-list-mode)
        (let ((inhibit-read-only t))
          (gp--insert-pr pr))
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "feature/widget" text))
          (should (string-match-p "main" text))
          (should-not (string-match-p (regexp-quote "? → ?") text)))))))

(ert-deftest gp-test-render-detail-shows-real-branches-and-author-for-github-pr ()
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (pr (gp-test--gh-pr)))
      (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "someone-else")))
        (with-temp-buffer
          (gp-detail-mode)
          (let ((inhibit-read-only t))
            (gp--render-detail pr nil))
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "feature/widget → main" text))
            (should (string-match-p "👤 ada" text))
            (should-not (string-match-p (regexp-quote "? → ?") text))
            (should-not (string-match-p (regexp-quote "👤 ?") text))))))))

(ert-deftest gp-test-render-detail-linkifies-ticket-key-in-title ()
  "A ticket key in the PR title is linked in the detail buffer too
\(previously only the description and commit summaries were)."
  (let* ((gp-ticket-url-format "https://co.atlassian.net/browse/%s")
         (pr '((id . 42) (title . "WP-1231: add the widget toggle")
               (state . "OPEN")
               (destination (repository (full_name . "acme/web") (slug . "web"))
                            (branch (name . "main")))
               (source (branch (name . "feature/widget")))
               (author (display_name . "Ada")))))
    (with-temp-buffer
      (gp-detail-mode)
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}")))
          (gp--render-detail pr nil)))
      (let* ((text (buffer-string))
             (ticket-pos (string-match "WP-1231" text))
             (rest-pos (string-match "widget toggle" text)))
        (should (eq (get-text-property ticket-pos 'face text) 'link))
        (should (equal (get-text-property ticket-pos 'help-echo text)
                       "https://co.atlassian.net/browse/WP-1231"))
        ;; the rest of the title keeps the detail-view title face
        (should (eq (get-text-property rest-pos 'face text) 'gp-detail-title-face))))))

(ert-deftest gp-test-render-detail-shows-mark-ready-for-open-github-pr-authored-by-me ()
  "The draft-toggle button must key off `gp-pr-open-p', not a raw
\"OPEN\" string comparison -- GitHub's `state' is lowercase \"open\"."
  (github-mock-with-service
    ;; the mock's /user is ada, the PR's author
    (let ((pr (append '((draft . t)) (gp-test--gh-pr))))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr nil))
        (should (string-match-p "Mark ready \\[D\\]"
                                (substring-no-properties (buffer-string))))))))

;;;; In-place action spinner (no layout shift while a mutation is in flight)

;;;; Commits section ----------------------------------------------------------

(defconst gp-test--commits
  '((:hash "87c8054110c84d42edc3a4e89184ffd1a15d3a8d"
     :summary "fix indentation of the wscat script"
     :author "Felix Brilej" :date "2026-07-15T17:28:26+00:00")
    (:hash "1a2b3c4d5e6f7788990011223344556677889900"
     :summary "add GraphQL subscription example"
     :author "Ada Lovelace" :date "2026-07-14T09:00:00+00:00"))
  "Two normalised commit plists, as the backends produce them.")

(defun gp-test--render-commits (commits)
  "Render COMMITS into a temp detail buffer; return its text."
  (with-temp-buffer
    (gp-detail-mode)
    (setq gp--detail-commits commits)
    (let ((inhibit-read-only t))
      (magit-insert-section (gp-root)
        (gp--insert-commits)))
    (substring-no-properties (buffer-string))))

(ert-deftest gp-test-detail-commits-render ()
  "Each commit renders as short hash + summary + author + relative date."
  (let ((text (gp-test--render-commits gp-test--commits)))
    (should (string-match-p "Commits (2)" text))
    ;; abbreviated, not the full 40-char hash
    (should (string-match-p "87c80541" text))
    (should-not (string-match-p "87c8054110c84d42" text))
    (should (string-match-p "fix indentation of the wscat script" text))
    (should (string-match-p "Felix Brilej" text))
    (should (string-match-p "Ada Lovelace" text))
    ;; the date is relative ("N days ago"), not the raw ISO timestamp
    (should (string-match-p "Felix Brilej  [0-9]+ [a-z]+ ago" text))
    (should (string-match-p "Ada Lovelace  [0-9]+ [a-z]+ ago" text))
    (should-not (string-match-p "2026-07-15" text))))

(ert-deftest gp-test-detail-commits-summary-linkifies-ticket-keys ()
  "A ticket key in a commit summary (\"fix: WP-1231 ...\") becomes a link."
  (let* ((gp-ticket-url-format "https://co.atlassian.net/browse/%s")
         (commits (list (list :hash "abc123" :summary "fix: WP-1231 handle the edge case"
                              :author "Felix" :date "2026-07-15T17:28:26+00:00"))))
    (with-temp-buffer
      (gp-detail-mode)
      (setq gp--detail-commits commits)
      (let ((inhibit-read-only t))
        (magit-insert-section (gp-root)
          (gp--insert-commits)))
      (let* ((text (buffer-string))
             (pos (string-match "WP-1231" text)))
        (should pos)
        (should (eq (get-text-property pos 'face text) 'link))
        (should (equal (get-text-property pos 'help-echo text)
                       "https://co.atlassian.net/browse/WP-1231"))))))

(ert-deftest gp-test-detail-commits-show-pipeline-status ()
  "A commit with a matching pipeline (from EITHER :current, the head
commit's own runs, or :recent, an older commit's) shows a status
glyph + pipeline number on its own line -- this is what used to be a
separate \"Recent runs on this branch\" block (see
`gp-test-pipeline-render-section', which now asserts that block is
gone) that repeated the commit summary a second time for no reason."
  (let* ((head-pipeline '((state (name . "COMPLETED") (result (name . "SUCCESSFUL")))
                          (build_number . 42)
                          (target (commit (hash . "87c8054110c84d42edc3a4e89184ffd1a15d3a8d")))))
         (older-pipeline '((state (name . "COMPLETED") (result (name . "FAILED")))
                           (build_number . 40)
                           (target (commit (hash . "1a2b3c4d5e6f7788990011223344556677889900")))))
         (git-platform-current-backend (git-platform-bitbucket)))
    (with-temp-buffer
      (gp-detail-mode)
      (setq gp--pr '((destination (repository (full_name . "acme/web"))))
            gp--detail-commits gp-test--commits
            gp--detail-pipelines (list :current (list (cons head-pipeline nil))
                                       :recent (list (cons older-pipeline "older commit summary"))))
      (let ((inhibit-read-only t))
        (magit-insert-section (gp-root)
          (gp--insert-commits)))
      (let ((text (buffer-string)))
        ;; the head commit's line carries its own (successful) pipeline
        (should (string-match-p "87c80541.*#42" text))
        ;; the older commit's line carries ITS pipeline, not the head's
        (should (string-match-p "1a2b3c4d.*#40" text))
        ;; and does NOT repeat as a separate "Recent runs" block/summary
        (should-not (string-match-p "Recent runs" text))
        (should-not (string-match-p "older commit summary" text))
        ;; each pipeline number is a link to its Bitbucket results page
        (should (equal (get-text-property (string-match "42" text) 'help-echo text)
                       "https://bitbucket.org/acme/web/pipelines/results/42"))
        (should (equal (get-text-property (string-match "40" text) 'help-echo text)
                       "https://bitbucket.org/acme/web/pipelines/results/40"))))))

(ert-deftest gp-test-detail-commits-no-pipeline-match-is-quiet ()
  "A commit with no matching pipeline just has no status glyph -- not
an error, not a placeholder, and not some other commit's pipeline."
  (let ((stray '((state (name . "COMPLETED") (result (name . "SUCCESSFUL")))
                 (build_number . 77)
                 (target (commit (hash . "ffffffffffffffffffffffffffffffffffffffff")))))
        (git-platform-current-backend (git-platform-bitbucket)))
    (with-temp-buffer
      (gp-detail-mode)
      ;; a real PR, so the per-commit pipeline lookup actually runs
      (setq gp--pr '((destination (repository (full_name . "acme/web"))))
            gp--detail-commits gp-test--commits
            gp--detail-pipelines (list :current nil :recent (list (cons stray "s"))))
      (let ((inhibit-read-only t))
        (magit-insert-section (gp-root)
          (gp--insert-commits)))
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "fix indentation of the wscat script" text))
        (should (string-match-p "add GraphQL subscription example" text))
        (should-not (string-match-p "#77" text))
        (should-not (string-match-p "#" text))))))

(defun gp-test--steps-fetches ()
  "Number of pipeline-steps requests in `bitbucket-mock-calls'."
  (cl-count-if (lambda (c) (string-match-p "/pipelines/[^/]+/steps\\'" (nth 1 c)))
               bitbucket-mock-calls))

(defconst gp-test--running-state
  '(state (name . "IN_PROGRESS") (stage (name . "RUNNING")))
  "An unfinished pipeline's state, as tests/fixtures/pipelines.json has it.")

(ert-deftest gp-test-detail-commits-current-pipeline-shows-step-progress-immediately ()
  "The head commit's own pipeline already carries its steps for free
\(from the main pipeline load) -- its \"N/total\" count must show up
on the very first render, with no fetch and no rerender needed."
  (bitbucket-mock-with-service
    (let* ((pipeline `((uuid . "{pipe-0}") ,gp-test--running-state (build_number . 42)
                       (target (commit (hash . "87c8054110c84d42edc3a4e89184ffd1a15d3a8d")))))
           (steps (list '((state (name . "COMPLETED") (result (name . "SUCCESSFUL"))))
                        '((state (name . "COMPLETED") (result (name . "SUCCESSFUL"))))
                        '((state (name . "COMPLETED") (result (name . "SUCCESSFUL"))))
                        '((state (name . "PENDING") (stage (name . "PAUSED"))))))
           (gp--recent-pipeline-steps-cache (make-hash-table :test 'equal)))
      (with-temp-buffer
        (gp-detail-mode)
        (setq gp--pr '((destination (repository (full_name . "acme/web"))))
              gp--detail-commits gp-test--commits
              gp--detail-pipelines (list :current (list (cons pipeline steps)) :recent nil))
        (let ((inhibit-read-only t))
          (magit-insert-section (gp-root)
            (gp--insert-commits)))
        (should (string-match-p "#42 (3/4)" (buffer-string)))
        ;; the steps were already known -- no request went out at all
        (should (= (gp-test--steps-fetches) 0))))))

(ert-deftest gp-test-detail-commits-recent-pipeline-fetches-steps-once ()
  "An unfinished :recent pipeline has no steps yet, so the first render
kicks off a fetch; once that resolves (and caches, see
`gp--recent-pipeline-steps-cache'), a SECOND render for the SAME
pipeline uuid must not fetch again, and shows the fetched count."
  (bitbucket-mock-with-service
    (let ((pipeline `((uuid . "{pipe-1}") ,gp-test--running-state (build_number . 40)
                      (target (commit (hash . "1a2b3c4d5e6f7788990011223344556677889900")))))
          (gp--recent-pipeline-steps-cache (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'gp--detail-rerender) #'ignore))
        (with-temp-buffer
          (gp-detail-mode)
          (setq gp--pr '((destination (repository (full_name . "acme/web"))))
                gp--detail-commits gp-test--commits
                gp--detail-pipelines (list :current nil :recent (list (cons pipeline "s"))))
          (let ((inhibit-read-only t))
            (magit-insert-section (gp-root)
              (gp--insert-commits)))
          ;; first render: no cached steps yet, so exactly one steps request
          (should (= (gp-test--steps-fetches) 1))
          (should (equal (nth 1 (car bitbucket-mock-calls))
                         "/repositories/acme/web/pipelines/{pipe-1}/steps"))
          ;; steps are cached now -- a second render must not fetch again
          (let ((inhibit-read-only t))
            (erase-buffer)
            (magit-insert-section (gp-root)
              (gp--insert-commits)))
          (should (= (gp-test--steps-fetches) 1))
          ;; pipeline-steps.json: 3 steps, 1 SUCCESSFUL
          (should (string-match-p "#40 (1/3)" (buffer-string))))))))

(ert-deftest gp-test-detail-commits-finished-recent-pipeline-never-fetches-steps ()
  "A FINISHED older-commit pipeline's glyph is already the whole
story -- fetching its steps just to show a redundant \"N/total (all
succeeded)\" would be a wasted call for no new information."
  (bitbucket-mock-with-service
    (let ((pipeline '((uuid . "{pipe-2}")
                      (state (name . "COMPLETED") (result (name . "SUCCESSFUL")))
                      (build_number . 40)
                      (target (commit (hash . "1a2b3c4d5e6f7788990011223344556677889900")))))
          (gp--recent-pipeline-steps-cache (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'gp--detail-rerender) #'ignore))
        (with-temp-buffer
          (gp-detail-mode)
          (setq gp--pr '((destination (repository (full_name . "acme/web"))))
                gp--detail-commits gp-test--commits
                gp--detail-pipelines (list :current nil :recent (list (cons pipeline "s"))))
          (let ((inhibit-read-only t))
            (magit-insert-section (gp-root)
              (gp--insert-commits)))
          (should (string-match-p "#40" (buffer-string)))
          (should-not (string-match-p "#40 (" (buffer-string)))
          (should (= (gp-test--steps-fetches) 0)))))))

(ert-deftest gp-test-detail-commits-empty-is-noop ()
  "No commits -> no section at all (not an empty heading)."
  (should (equal (gp-test--render-commits nil) "")))

(ert-deftest gp-test-detail-commits-are-sections-carrying-their-plist ()
  "Each commit line is its own section whose value is the commit plist.
`gp-detail-show-commit' reads the hash off that value, so losing it
would break RET on a commit."
  (with-temp-buffer
    (gp-detail-mode)
    (setq gp--detail-commits gp-test--commits)
    (let ((inhibit-read-only t))
      (magit-insert-section (gp-root)
        (gp--insert-commits)))
    (let ((secs (gp-test--all-sections magit-root-section)))
      (let ((commit-secs (cl-remove-if-not
                          (lambda (s) (object-of-class-p s 'gp-commit-section))
                          secs)))
        (should (= (length commit-secs) 2))
        (should (equal (plist-get (oref (car commit-secs) value) :hash)
                       "87c8054110c84d42edc3a4e89184ffd1a15d3a8d"))))))

(ert-deftest gp-test-detail-ret-on-commit-shows-commit ()
  "RET on a rendered commit line opens that commit, not the section fold."
  (with-temp-buffer
    (gp-detail-mode)
    (setq gp--detail-commits gp-test--commits)
    (let ((inhibit-read-only t))
      (magit-insert-section (gp-root)
        (gp--insert-commits)))
    (let (shown)
      (cl-letf (((symbol-function 'gp-detail-show-commit)
                 (lambda () (interactive)
                   (setq shown (plist-get (oref (magit-current-section) value) :hash))))
                ((symbol-function 'magit-section-toggle)
                 (lambda (&rest _) (interactive) (setq shown 'toggled))))
        (goto-char (point-min))
        (should (search-forward "add GraphQL subscription example" nil t))
        (gp-detail-ret)
        (should (equal shown "1a2b3c4d5e6f7788990011223344556677889900"))))))

(ert-deftest gp-test-detail-show-commit-errors-off-a-commit ()
  "Away from a commit section it reports rather than acting on nothing."
  (cl-letf (((symbol-function 'magit-current-section) (lambda () nil)))
    (should-error (gp-detail-show-commit) :type 'user-error)))

(defmacro gp-test--with-changed-files (files &rest body)
  "Render FILES as the changed-files section in a detail buffer, run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (gp-detail-mode)
     (setq gp--detail-stats (list :file-list ,files))
     (let ((inhibit-read-only t))
       (magit-insert-section (gp-root)
         (gp--insert-changed-files)))
     ,@body))

(defconst gp-test--file-list
  '((:path "scripts/wscat.sh" :added 12 :removed 3 :status "modified")
    (:path "docs/graphql.md" :added 40 :removed 0 :status "added"))
  "Two changed-file plists, as `gp--detail-stats' carries them.")

;;;; Uncounted (generated) files ---------------------------------------------

(ert-deftest gp-test-uncounted-file-p-matches-base-names ()
  (should (gp--uncounted-file-p "Pipfile.lock"))
  (should (gp--uncounted-file-p "services/api/package-lock.json"))
  (should-not (gp--uncounted-file-p "src/lock.py"))
  (should-not (gp--uncounted-file-p nil))
  (let ((gp-uncounted-files '("*.min.js")))
    (should (gp--uncounted-file-p "web/app.min.js"))
    (should-not (gp--uncounted-file-p "Pipfile.lock"))))

(ert-deftest gp-test-counted-lines-subtracts-lockfiles ()
  "Lockfile lines come off the totals; a cut-short file list still subtracts."
  (should (equal (gp--counted-lines
                  '(:added 4012 :removed 7
                    :file-list ((:path "app.py" :added 1 :removed 1)
                                (:path "Pipfile.lock" :added 4011 :removed 6))))
                 '(1 1 1)))
  ;; no file list (or nothing generated): totals as reported
  (should (equal (gp--counted-lines '(:added 5 :removed 2)) '(5 2 0))))

(ert-deftest gp-test-stats-line-leaves-lockfiles-out ()
  (should (equal (substring-no-properties
                  (gp--stats-line
                   '(:commits 1 :files 2 :added 4012 :removed 7
                     :file-list ((:path "app.py" :added 1 :removed 1)
                                 (:path "Pipfile.lock" :added 4011 :removed 6)))))
                 "📊 1 commit  2 files  +1 -1  (1 file not counted)\n"))
  (should (equal (substring-no-properties
                  (gp--stats-line '(:commits 3 :files 1 :added 9 :removed 2)))
                 "📊 3 commits  1 file  +9 -2\n")))

(ert-deftest gp-test-changed-files-marks-lockfile-not-counted ()
  (gp-test--with-changed-files
      '((:path "app.py" :added 1 :removed 1 :status "modified")
        (:path "Pipfile.lock" :added 4011 :removed 6 :status "modified"))
    (let ((text (substring-no-properties (buffer-string))))
      (should (string-match-p "Pipfile.lock  \\+4011 -6  (not counted)" text))
      (should-not (string-match-p "app.py.*not counted" text)))))

(defun gp-test--goto-file-line (name)
  "Move point to the start of the line rendering file NAME."
  (goto-char (point-min))
  (should (search-forward name nil t))
  (beginning-of-line))

(ert-deftest gp-test-file-path-anywhere-on-the-line ()
  "The path resolves from any column of the file's line, not just the name.
Only the name is buttonised, so requiring point to sit on the button
made RET miss from the indentation and the +N/-N stat columns."
  (gp-test--with-changed-files gp-test--file-list
    (gp-test--goto-file-line "scripts/wscat.sh")
    (let ((eol (line-end-position)))
      ;; every column of the line, indentation and stats included
      (while (<= (point) eol)
        (should (equal (gp--file-path-at-line) "scripts/wscat.sh"))
        (forward-char 1)))))

(ert-deftest gp-test-file-path-stays-on-its-own-line ()
  "Each file line resolves to its own path; other lines resolve to nil."
  (gp-test--with-changed-files gp-test--file-list
    (gp-test--goto-file-line "docs/graphql.md")
    (should (equal (gp--file-path-at-line) "docs/graphql.md"))
    ;; the heading line carries no file button
    (goto-char (point-min))
    (should-not (gp--file-path-at-line))
    ;; nor does the blank line closing the section (point-max is on it)
    (goto-char (point-max))
    (beginning-of-line)
    (should-not (gp--file-path-at-line))))

(ert-deftest gp-test-detail-ret-opens-file-from-the-stat-columns ()
  "RET past the file name still opens the file instead of folding the section."
  (gp-test--with-changed-files gp-test--file-list
    (let (opened)
      (cl-letf (((symbol-function 'gp-ui-open-file)
                 (lambda (_pr path) (setq opened path)))
                ((symbol-function 'magit-section-toggle)
                 (lambda (&rest _) (interactive) (setq opened 'toggled))))
        (gp-test--goto-file-line "scripts/wscat.sh")
        (end-of-line)                   ;; well past the buttonised name
        (gp-detail-ret)
        (should (equal opened "scripts/wscat.sh"))))))

(ert-deftest gp-test-detail-visit-file-errors-off-a-file-line ()
  "Away from any file line it reports rather than opening something."
  (gp-test--with-changed-files gp-test--file-list
    (goto-char (point-min))             ;; the "Changed files (2)" heading
    (should-error (gp-detail-visit-file) :type 'user-error)))

(ert-deftest gp-test-detail-run-action-shows-spinner-during-thunk ()
  "While `gp--detail-run-action's THUNK is running, the buffer already
shows the spinner in that button's slot (not just after it returns)."
  (github-mock-with-service
    (with-temp-buffer
      (gp-detail-mode)
      ;; ada's own draft PR, so the draft toggle button is drawn
      (gp--render-detail-into (current-buffer)
                              (append '((draft . t)) (gp-test--gh-pr)) nil nil)
      (should (string-match-p "Mark ready \\[D\\]" (buffer-string)))
      (let (text-during-thunk)
        (gp--detail-run-action
         (current-buffer) 'draft
         (lambda () (setq text-during-thunk (buffer-string))))
        (should (string-match-p "⏳" text-during-thunk))
        (should-not (string-match-p "Mark ready" text-during-thunk))
        ;; cleared once the thunk returns
        (should-not gp--detail-pending-action)))))

(ert-deftest gp-test-detail-run-action-clears-pending-on-error ()
  "The pending flag is cleared even if THUNK signals."
  (with-temp-buffer
    (gp-detail-mode)
    (ignore-errors
      (gp--detail-run-action (current-buffer) 'draft (lambda () (error "boom"))))
    (should-not gp--detail-pending-action)))

(ert-deftest gp-test-action-button-spinner-shows-spinner-when-pending ()
  (with-temp-buffer
    (gp-detail-mode)
    (let ((inhibit-read-only t))
      (setq gp--detail-pending-action 'draft)
      (gp--insert-action-button/spinner 'draft "✅ Mark ready [D]" "help" #'ignore)
      (should (string-match-p "⏳" (buffer-string)))
      (should-not (string-match-p "Mark ready" (buffer-string))))))

(ert-deftest gp-test-action-button-spinner-shows-label-when-not-pending ()
  (with-temp-buffer
    (gp-detail-mode)
    (let ((inhibit-read-only t))
      (setq gp--detail-pending-action nil)
      (gp--insert-action-button/spinner 'draft "✅ Mark ready [D]" "help" #'ignore)
      (should (string-match-p "Mark ready" (buffer-string)))
      (should-not (string-match-p "⏳" (buffer-string))))))

(ert-deftest gp-test-action-button-spinner-uses-equal-not-eq ()
  "Compound tags (per-comment actions) are fresh conses each render, so
the comparison must be `equal', not `eq' -- this is the exact bug a
naive `eq' implementation would hit across two separate renders."
  (with-temp-buffer
    (gp-detail-mode)
    (let ((inhibit-read-only t))
      (setq gp--detail-pending-action (cons 'resolution 77))
      (gp--insert-action-button/spinner (cons 'resolution 77) "resolve [X]" "help" #'ignore)
      (should (string-match-p "⏳" (buffer-string))))))

(ert-deftest gp-test-action-button-spinner-per-comment-tags-are-independent ()
  "A pending action on comment 77 must not show a spinner on comment 99's button."
  (with-temp-buffer
    (gp-detail-mode)
    (let ((inhibit-read-only t))
      (setq gp--detail-pending-action (cons 'resolution 77))
      (gp--insert-action-button/spinner (cons 'resolution 99) "resolve [X]" "help" #'ignore)
      (should (string-match-p "resolve" (buffer-string)))
      (should-not (string-match-p "⏳" (buffer-string))))))

;;;; PR description (field name differs per backend) --------------------------

(ert-deftest gp-test-pr-description-reads-bitbucket-description ()
  "Bitbucket keeps the body in `description'."
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (should (equal (gp-pr-description '((id . 1) (description . "why this PR")))
                   "why this PR"))
    ;; blank/absent collapses to nil so callers can skip the section
    (should-not (gp-pr-description '((id . 1) (description . ""))))
    (should-not (gp-pr-description '((id . 1) (description . "   \n"))))
    (should-not (gp-pr-description '((id . 1))))))

(ert-deftest gp-test-pr-description-reads-github-body ()
  "GitHub keeps the body in `body'; an empty or null body reads as none."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github)))
      (should (equal (gp-pr-description (gp-test--gh-pr))
                     "Adds the toggle.\n\n- [x] tests\n- [ ] docs"))
      (should-not (gp-pr-description '((id . 42) (body . ""))))
      (should-not (gp-pr-description (github--parse-json "{\"id\": 42, \"body\": null}")))
      (should-not (gp-pr-description '((id . 42)))))))

(ert-deftest gp-test-linkify-buffer-links-ticket-keys ()
  "`gp--linkify' (used on the rendered description, alongside URLs)
also turns a ticket key into a link -- the same pass `gp--render-markdown'
runs on PR descriptions."
  (let ((gp-ticket-url-format "https://co.atlassian.net/browse/%s"))
    (with-temp-buffer
      (insert "See WP-1231 and https://example.com for details.")
      (gp--linkify (point-min))
      (let ((text (buffer-string)))
        (let ((ticket-pos (string-match "WP-1231" text)))
          (should (eq (get-text-property ticket-pos 'face text) 'link))
          (should (equal (get-text-property ticket-pos 'help-echo text)
                         "https://co.atlassian.net/browse/WP-1231")))
        ;; the URL pass still works alongside the ticket pass
        (let ((url-pos (string-match "https://example.com" text)))
          (should (eq (get-text-property url-pos 'face text) 'link)))))))

(ert-deftest gp-test-linkify-buffer-ticket-unconfigured-still-highlights ()
  (let ((gp-ticket-url-format nil))
    (with-temp-buffer
      (insert "See WP-1231 for details.")
      (gp--linkify (point-min))
      (let* ((text (buffer-string))
             (pos (string-match "WP-1231" text)))
        (should (eq (get-text-property pos 'face text) 'link))
        (should (string-match-p "gp-ticket-url-format" (get-text-property pos 'help-echo text)))))))
;; A CRLF body fetched from GitHub renders with clean lines.
(ert-deftest gp-test-pr-description-strips-github-crlf ()
  "A GitHub body authored in the web UI arrives CRLF-terminated; once
fetched (`github-pull-request') and rendered, the detail buffer shows
clean lines with no ^M."
  (github-mock-with-service
    (let ((github-mock-overrides
           `(("/pulls/42\\'" . ,(cons '(body . "Adds the toggle.\r\n\r\n- [x] tests")
                                       (assq-delete-all 'body (copy-alist github-mock--pr-1)))))))
      (let ((pr (github-pull-request "acme/web" 42)))
        (should (equal (gp-pr-description pr) "Adds the toggle.\n\n- [x] tests"))
        (with-temp-buffer
          (gp-detail-mode)
          (let ((inhibit-read-only t))
            (gp--render-detail pr nil))
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "Adds the toggle\\.\n" text))
            (should-not (string-match-p "\r" text))))))))

(ert-deftest gp-test-render-detail-shows-description-for-github-pr ()
  "The detail view renders the description section from GitHub's `body'."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github)))
      (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "someone-else")))
        (with-temp-buffer
          (gp-detail-mode)
          (let ((inhibit-read-only t))
            (gp--render-detail (gp-test--gh-pr) nil))
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "Description" text))
            (should (string-match-p "Adds the toggle" text))))))))

(ert-deftest gp-test-render-detail-shows-empty-description-state-when-editable ()
  "An open PR with no description gets the section anyway, as an empty state.
This reverses the older \"no description means no heading\" rule: back
then the heading was inert text and an empty one was pure noise, but it
now carries the `✎ edit [E]' button, and a PR with no body is exactly
when you need a way to write the first one.  Without it `E' is invisible
precisely where it is most useful."
  (let ((pr (append '((description . "")) (car (gp-test--mock-prs)))))
    (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}")))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr nil))
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "Description" text))
          (should (string-match-p "no description yet" text))
          (should (string-match-p "edit \\[E\\]" text)))))))

(ert-deftest gp-test-render-detail-shows-comments ()
  (bitbucket-mock-with-service
    (let ((pr (car (gp-test--mock-prs)))
          (comments (alist-get 'values (bitbucket-mock--fixture "pr-comments.json"))))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr comments))
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p (format "Comments (%d)" (length comments)) text))
          ;; an inline comment's location label shows file:line
          (should (string-match-p "\\.ts:[0-9]+" text))
          (should (string-match-p "send to terminal \\\[t\\\]" text))
          (should (string-match-p "view in browser \\\[w\\\]" text)))))))

(ert-deftest gp-test-render-detail-offers-edit-and-delete-on-your-own-comment ()
  "The signed-in user (user.json) wrote one fixture comment: only that
one offers edit and delete."
  (bitbucket-mock-with-service
    (let ((pr (car (gp-test--mock-prs)))
          (comments (bitbucket-pull-request-comments "acme/web-frontend" 180))
          (gp-comment-delete-others nil))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr comments))
        (let ((text (substring-no-properties (buffer-string)))
              (case-fold-search nil))
          (should (= 1 (how-many "edit \\[e\\]" 1 (point-max))))
          (should (= 1 (how-many "delete \\[K\\]" 1 (point-max))))
          (should (string-match-p
                   "Thanks, addressed the constant\\(?:.\\|\n\\)*?edit \\[e\\]"
                   text)))))))

(ert-deftest gp-test-render-detail-hides-delete-on-others-comments-by-default ()
  "With `gp-comment-delete-others' nil, only your own comments offer delete."
  (let ((pr (car (gp-test--mock-prs)))
        (comments (alist-get 'values (bitbucket-mock--fixture "pr-comments.json")))
        (gp-comment-delete-others nil))
    ;; a uuid nobody in the fixture owns, so every comment is "someone else's"
    (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{nobody}")))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr comments))
        (let ((text (substring-no-properties (buffer-string)))
              ;; `[e]' is the per-comment edit button; without this the
              ;; match folds case and also hits the description's `[E]'.
              (case-fold-search nil))
          (should-not (string-match-p "delete \\[K\\]" text))
          (should-not (string-match-p "edit \\[e\\]" text)))))))

(ert-deftest gp-test-render-detail-shows-delete-on-others-when-permitted ()
  "Enabling `gp-comment-delete-others' for the backend exposes delete
on other people's comments -- but never the edit action, which no API
allows on someone else's text."
  (let ((pr (car (gp-test--mock-prs)))
        (comments (alist-get 'values (bitbucket-mock--fixture "pr-comments.json")))
        (gp-comment-delete-others '(bitbucket))
        (git-platform-current-backend (git-platform-bitbucket)))
    ;; the real Bitbucket backend answers `gp-backend-name'; a uuid nobody
    ;; in the fixture owns makes every comment "someone else's"
    (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{nobody}")))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr comments))
        (let ((text (substring-no-properties (buffer-string)))
              (case-fold-search nil))   ;; see above: `[e]' is not `[E]'
          (should (string-match-p "delete \\[K\\]" text))
          (should-not (string-match-p "edit \\[e\\]" text)))))))

(ert-deftest gp-test-comment-delete-others-scoped-per-backend ()
  "A backend absent from the list gets no elevated delete power."
  (let ((comment '((id . 1) (user (uuid . "{them}")))))
    (let ((git-platform-current-backend (git-platform-github)))
      (let ((gp-comment-delete-others '(bitbucket)))
        (should-not (gp-comment-deletable-p comment "{me}")))
      (let ((gp-comment-delete-others '(github)))
        (should (gp-comment-deletable-p comment "{me}")))
      (let ((gp-comment-delete-others t))
        (should (gp-comment-deletable-p comment "{me}")))
      ;; your own comment stays deletable regardless of the setting
      (let ((gp-comment-delete-others nil))
        (should (gp-comment-deletable-p '((id . 1) (user (uuid . "{me}"))) "{me}"))))
    ;; and the same list read from the other side
    (let ((git-platform-current-backend (git-platform-bitbucket)))
      (let ((gp-comment-delete-others '(github)))
        (should-not (gp-comment-deletable-p comment "{me}")))
      (let ((gp-comment-delete-others '(bitbucket)))
        (should (gp-comment-deletable-p comment "{me}"))))))

(ert-deftest gp-test-detail-mode-map-mutating-actions-are-capitalised ()
  "Write actions on comments sit on capitals; the lowercase keys are free.
Read-only actions stay lowercase."
  (should (eq (lookup-key gp-detail-mode-map "v") #'gp-detail-show-commit))
  (should (eq (lookup-key gp-detail-mode-map "R") #'gp-detail-reply))
  (should (eq (lookup-key gp-detail-mode-map "X") #'gp-detail-resolve))
  (should (eq (lookup-key gp-detail-mode-map "K") #'gp-detail-delete))
  (should (eq (lookup-key gp-detail-mode-map "P")
              #'gp-detail-pipeline-rerun-step))
  ;; unchanged neighbours, so the reshuffle didn't clobber them
  (should (eq (lookup-key gp-detail-mode-map "D") #'gp-detail-toggle-draft))
  (should (eq (lookup-key gp-detail-mode-map "T")
              #'gp-detail-pipeline-trigger-or-run-manual))
  (dolist (key '("r" "x"))
    (should-not (eq (lookup-key gp-detail-mode-map key) #'gp-detail-reply))
    (should-not (eq (lookup-key gp-detail-mode-map key) #'gp-detail-resolve))))

(ert-deftest gp-test-list-find-pr-point-locates-section ()
  "`gp--list-find-pr-point' returns the start of the matching PR section."
  (let* ((prs (gp-test--mock-prs))
         (uuid "{21d7839d-779f-44b2-8c40-6f43ac90be06}")
         (target-id (alist-get 'id (nth 2 prs))))
    (with-temp-buffer
      (gp-list-mode)
      (let ((inhibit-read-only t))
        (gp--render-list prs uuid))
      (let ((pos (gp--list-find-pr-point target-id)))
        (should pos)
        (goto-char pos)
        (should (string-match-p (format "#%s" target-id)
                                (buffer-substring-no-properties
                                 (point) (line-end-position))))))))

(ert-deftest gp-test-list-find-pr-point-nil-when-absent ()
  (let* ((prs (gp-test--mock-prs))
         (uuid "{21d7839d-779f-44b2-8c40-6f43ac90be06}"))
    (with-temp-buffer
      (gp-list-mode)
      (let ((inhibit-read-only t))
        (gp--render-list prs uuid))
      (should-not (gp--list-find-pr-point -1)))))

(ert-deftest gp-test-render-detail-shows-reviewers ()
  "The overview section lists reviewers and their approval state.
`gp--detail-reviewers' is populated asynchronously (see
`gp--detail-load-reviewers') -- setting it directly here simulates
that fetch having already landed, which is what `gp--render-detail'
actually reads (not the raw PR's Bitbucket-shaped `participants',
which GitHub PRs don't carry at all)."
  (let ((pr (car (gp-test--mock-prs))))
    (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}")))
      (with-temp-buffer
        (gp-detail-mode)
        (setq gp--detail-reviewers
              (list (list :name "Alice" :state 'approved)
                    (list :name "Bob" :state 'changes)
                    (list :name "Carol" :state 'pending)))
        (let ((inhibit-read-only t))
          (gp--render-detail pr nil))
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "✅ Alice" text))
          (should (string-match-p "❌ Bob" text))
          (should (string-match-p "⏳ Carol" text)))))))

(ert-deftest gp-test-render-detail-no-reviewers-offers-to-add-some ()
  "An open PR with no reviewers still shows the line, with an edit button.
That is precisely when you need a way in -- hiding the line would
leave no entry point for adding the first reviewer."
  (let ((pr (car (gp-test--mock-prs))))
    (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}")))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr nil))
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "👥" text))
          (should (string-match-p "no reviewers" text))
          (should (string-match-p "edit \\[V\\]" text)))))))

(ert-deftest gp-test-render-detail-closed-pr-hides-reviewer-editing ()
  "A merged/closed PR with no reviewers cannot be edited: no line and no button."
  (let ((pr (cons '(state . "MERGED")
                  (assq-delete-all 'state (copy-alist (car (gp-test--mock-prs)))))))
    (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}")))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr nil))
        (should-not (string-match-p "edit \\[V\\]" (buffer-string)))
        ;; no "👥 no reviewers" line either
        (should-not (string-match-p "👥" (buffer-string)))
        (should-not (string-match-p "no reviewers" (buffer-string)))))))

(ert-deftest gp-test-comment-location-inline-vs-general ()
  (should (equal (gp--comment-location
                  '((inline (path . "a/b.ts") (to . 42))))
                 "a/b.ts:42"))
  (should (equal (gp--comment-location '((content (raw . "hi"))))
                 "general")))

(ert-deftest gp-test-comment-threads ()
  "Replies are ordered under their parent, one level deeper."
  (let* ((comments '(((id . 1))
                     ((id . 2) (parent (id . 1)))   ;; reply to 1
                     ((id . 3))                      ;; another root
                     ((id . 4) (parent (id . 2)))   ;; reply to reply
                     ((id . 5) (parent (id . 1)))))  ;; second reply to 1
         (threads (gp--comment-threads comments))
         (order (mapcar (lambda (cd) (cons (alist-get 'id (car cd)) (cdr cd)))
                        threads)))
    ;; depth-first: 1, 2(reply), 4(reply-of-reply), 5(reply), then root 3
    (should (equal order '((1 . 0) (2 . 1) (4 . 2) (5 . 1) (3 . 0))))))

(ert-deftest gp-test-comment-threads-newest-first ()
  "Top-level comments sort newest first; replies stay chronological."
  (let* ((comments '(((id . 1) (created_on . "2026-07-01T10:00:00+00:00"))
                     ((id . 2) (created_on . "2026-07-03T09:00:00+00:00"))
                     ((id . 3) (parent (id . 1))
                      (created_on . "2026-07-02T08:00:00+00:00"))
                     ((id . 4) (parent (id . 1))
                      (created_on . "2026-07-02T09:00:00+00:00"))))
         (order (mapcar (lambda (cd) (cons (alist-get 'id (car cd)) (cdr cd)))
                        (gp--comment-threads comments))))
    ;; root 2 (newer) before root 1; 1's replies keep chronological order
    (should (equal order '((2 . 0) (1 . 0) (3 . 1) (4 . 1))))))

(ert-deftest gp-test-comment-heading-relative-time ()
  "The comment heading shows a relative timestamp."
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (magit-section)
        (gp--insert-comment
         `((id . 1)
           (created_on . ,(format-time-string
                           "%Y-%m-%dT%H:%M:%S+00:00"
                           (time-subtract (current-time) (* 5 60)) t))
           (user (display_name . "Alice"))
           (content (raw . "hi"))))))
    (should (string-match-p "5 minutes ago" (buffer-string)))))

(ert-deftest gp-test-detail-buffer-visible-p-checks-every-tab ()
  "A buffer showing in a window on a BACKGROUND tab-bar tab still
counts as visible -- `get-buffer-window' alone only sees the
currently selected tab's window tree, since tab-bar stores every
other tab's window state separately rather than keeping it live.
Without also checking `tab-bar-get-buffer-tab', switching away from
a PR's tab looked identical to that buffer not being displayed
anywhere at all, and its pipeline poll would stop silently."
  (with-temp-buffer
    (let ((buf (current-buffer)))
      ;; on the selected tab's window tree: get-buffer-window alone finds it
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'a-window))
                ((symbol-function 'tab-bar-get-buffer-tab) (lambda (&rest _) nil)))
        (should (gp--detail-buffer-visible-p buf)))
      ;; on a background tab only: get-buffer-window alone misses it,
      ;; tab-bar-get-buffer-tab is what catches it
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                ((symbol-function 'tab-bar-get-buffer-tab) (lambda (&rest _) '(tab))))
        (should (gp--detail-buffer-visible-p buf)))
      ;; not displayed anywhere at all
      (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil))
                ((symbol-function 'tab-bar-get-buffer-tab) (lambda (&rest _) nil)))
        (should-not (gp--detail-buffer-visible-p buf))))))

(ert-deftest gp-test-pipeline-poll-mode ()
  "Poll/watch only while the buffer is displayed."
  (let ((gp-detail-pipeline-poll-interval 6)
        (gp-detail-pipeline-watch-interval 1)
        (running '(:current ((((state (name . "IN_PROGRESS")))) ) :recent (r)))
        (finished '(:current ((((state (name . "COMPLETED")))) ) :recent (r)))
        (waiting '(:current nil :recent (r)))
        (no-history '(:current nil :recent nil)))
    (should (eq (gp--detail-pipeline-poll-mode running t) 'poll))
    ;; an unfinished run in an off-screen buffer is not worth an N+1 fetch
    (should-not (gp--detail-pipeline-poll-mode running nil))
    ;; head commit has runs and they're done: nothing to schedule
    (should-not (gp--detail-pipeline-poll-mode finished t))
    ;; no run for the head commit yet: watch, but only while displayed
    (should (eq (gp--detail-pipeline-poll-mode waiting t) 'watch))
    (should-not (gp--detail-pipeline-poll-mode waiting nil))
    ;; branch without any pipeline history is never watched
    (should-not (gp--detail-pipeline-poll-mode no-history t))
    ;; both features disabled
    (let ((gp-detail-pipeline-poll-interval 0)
          (gp-detail-pipeline-watch-interval 0))
      (should-not (gp--detail-pipeline-poll-mode running t))
      (should-not (gp--detail-pipeline-poll-mode waiting t)))))

(ert-deftest gp-test-pipeline-load-replaces-pending-timer ()
  "A second load cancels the first instead of stacking a second fetch."
  (let* ((pr '((id . 1) (source (branch (name . "b")) (commit (hash . "c")))))
         (cancelled 0)
         ;; a real timer object so `timerp' (in the cancel helper) is satisfied
         (stub (timer-create)))
    (with-temp-buffer
      (gp-detail-mode)
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (_) (setq cancelled (1+ cancelled))))
                ;; keep the fetch itself out of this test
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) stub)))
        (gp--detail-load-pipelines (current-buffer) pr)
        (should (eq gp--detail-pipeline-timer stub))
        (should (= cancelled 0))
        ;; second entry must cancel the pending one before re-arming
        (gp--detail-load-pipelines (current-buffer) pr)
        (should (= cancelled 1))
        (should (eq gp--detail-pipeline-timer stub))))))

(ert-deftest gp-test-pipeline-load-never-blocks ()
  "The poll path must use the async fetch, never the blocking one.
Regression: `gp--detail-load-pipelines' called `gp-pipeline-fetch-for-pr'
synchronously, so every `gp-detail-pipeline-poll-interval' seconds it
froze Emacs for a branch fetch plus one step fetch per current run --
a visible stall once per interval in any open detail buffer."
  (let* ((pr '((id . 1) (source (branch (name . "b")) (commit (hash . "c")))))
         (blocking-calls 0)
         (async-calls 0)
         (thunk nil))
    (with-temp-buffer
      (gp-detail-mode)
      (cl-letf (((symbol-function 'gp-pipeline-fetch-for-pr)
                 (lambda (&rest _) (setq blocking-calls (1+ blocking-calls)) nil))
                ((symbol-function 'gp-pipeline-fetch-for-pr-async)
                 (lambda (_pr cb) (setq async-calls (1+ async-calls))
                   (funcall cb '(:current nil :recent nil))))
                ;; capture the deferred body instead of waiting on a real timer
                ((symbol-function 'run-at-time)
                 (lambda (_secs _rep fn &rest _) (setq thunk fn) (timer-create))))
        (gp--detail-load-pipelines (current-buffer) pr)
        (should thunk)
        (funcall thunk)
        (should (= async-calls 1))
        (should (= blocking-calls 0))))))

(ert-deftest gp-test-comment-threads-orphan-parent ()
  "A reply whose parent isn't in the set is treated as a root."
  (let ((threads (gp--comment-threads
                  '(((id . 9) (parent (id . 999)))))))
    ;; not dropped; appears at depth 0
    (should (= (length threads) 1))
    (should (= (cdr (car threads)) 0))))

(ert-deftest gp-test-detail-send-to-terminal-delegates ()
  (let* ((pr '((id . 7) (destination (repository (full_name . "acme/x")))))
         (comment '((id . 55) (content (raw . "Please fix this"))))
         (gp--pr pr)
         (called nil))
    (cl-letf (((symbol-function 'magit-current-section)
               (lambda () (let ((s (gp-comment-section))) (oset s value comment) s)))
              ((symbol-function 'gp-ui-send-comment-to-terminal)
               (lambda (seen-pr seen-comment)
                 (setq called (list seen-pr seen-comment)))))
      (gp-detail-send-to-terminal)
      (should (equal called (list pr comment))))))

(ert-deftest gp-test-detail-toggle-mark-tracks-comment-id ()
  (let* ((comment '((id . 55) (content (raw . "Please fix this"))))
         (rerendered nil))
    (with-temp-buffer
      (gp-detail-mode)
      (setq gp--detail-comments (list comment))
      (cl-letf (((symbol-function 'magit-current-section)
                 (lambda () (let ((s (gp-comment-section))) (oset s value comment) s)))
                ((symbol-function 'gp--detail-rerender)
                 (lambda (&rest _) (setq rerendered t))))
        (gp-detail-toggle-mark)
        (should rerendered)
        (should (equal gp--detail-marked-comment-ids '(55)))
        (setq rerendered nil)
        (gp-detail-toggle-mark)
        (should rerendered)
        (should-not gp--detail-marked-comment-ids)))))

(ert-deftest gp-test-detail-send-to-terminal-sends-marked-comments-as-batch ()
  (let* ((pr '((id . 7) (destination (repository (full_name . "acme/x")))))
         (comment-1 '((id . 55) (content (raw . "First"))))
         (comment-2 '((id . 56) (content (raw . "Second"))))
         (gp--pr pr)
         (gp--detail-comments (list comment-1 comment-2))
         (gp--detail-marked-comment-ids '(56 55))
         (called nil)
         (rerendered nil))
    (cl-letf (((symbol-function 'gp-ui-send-comments-to-terminal)
               (lambda (seen-pr seen-comments)
                 (setq called (list seen-pr seen-comments))))
              ((symbol-function 'gp--detail-rerender)
               (lambda (&rest _) (setq rerendered t))))
      (gp-detail-send-to-terminal)
      (should (equal called (list pr (list comment-1 comment-2))))
      (should rerendered)
      (should-not gp--detail-marked-comment-ids))))

(ert-deftest gp-test-detail-send-to-terminal-keeps-marks-on-batch-error ()
  (let* ((pr '((id . 7) (destination (repository (full_name . "acme/x")))))
         (comment-1 '((id . 55) (content (raw . "First"))))
         (gp--pr pr)
         (gp--detail-comments (list comment-1))
         (gp--detail-marked-comment-ids '(55))
         (rerendered nil))
    (cl-letf (((symbol-function 'gp-ui-send-comments-to-terminal)
               (lambda (&rest _) (error "boom")))
              ((symbol-function 'gp--detail-rerender)
               (lambda (&rest _) (setq rerendered t))))
      (should-error (gp-detail-send-to-terminal))
      (should-not rerendered)
      (should (equal gp--detail-marked-comment-ids '(55))))))

(defmacro gp-test--with-git-root (&rest body)
  "Run BODY with `gp-local-git-root' a temp dir holding a real clone
of acme/x (dir `x', branch `feature', origin on bitbucket.org).
`repo' is bound to the clone's directory."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "gp-ui-git-" t))
          (repo (expand-file-name "x" root))
          (gp-local-git-root root)
          (gp-local--remote-cache (make-hash-table :test 'equal))
          (git-platform-current-backend (git-platform-bitbucket)))
     (unwind-protect
         (progn
           (make-directory repo)
           (let ((default-directory (file-name-as-directory repo))
                 (process-environment
                  (append '("GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@t"
                            "GIT_COMMITTER_NAME=t" "GIT_COMMITTER_EMAIL=t@t")
                          process-environment)))
             (dolist (args '(("init" "-q" "-b" "feature")
                             ("remote" "add" "origin" "git@bitbucket.org:acme/x.git")
                             ("commit" "-q" "--allow-empty" "-m" "init")))
               (should (zerop (apply #'call-process "git" nil nil nil args)))))
           ,@body)
       (delete-directory root t))))

(defun gp-test--git-branch (dir)
  "Return DIR's checked-out branch name."
  (let ((default-directory (file-name-as-directory dir)))
    (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))

(ert-deftest gp-test-detail-open-local-opens-magit-without-checkout ()
  "Opens the PR's local clone in Magit and leaves its branch alone."
  (skip-unless (and (executable-find "git") (require 'magit nil t)))
  (gp-test--with-git-root
    (let ((gp--pr '((id . 7) (source (branch (name . "other")))
                    (destination (repository (full_name . "acme/x")))))
          opened)
      (cl-letf (((symbol-function 'magit-status-setup-buffer)
                 (lambda (dir) (setq opened dir))))
        (gp-detail-open-local))
      (should (equal (file-truename (directory-file-name opened))
                     (file-truename repo)))
      ;; no branch switch: still on the clone's own branch
      (should (equal (gp-test--git-branch repo) "feature")))))

(ert-deftest gp-test-detail-open-local-errors-without-a-clone ()
  "With no local clone of the PR's repo it reports instead of opening."
  (skip-unless (and (executable-find "git") (require 'magit nil t)))
  (gp-test--with-git-root
    (let ((gp--pr '((id . 7) (destination (repository (full_name . "acme/elsewhere")))))
          opened)
      (cl-letf (((symbol-function 'magit-status-setup-buffer)
                 (lambda (dir) (setq opened dir))))
        (should-error (gp-detail-open-local) :type 'user-error))
      (should-not opened))))

(ert-deftest gp-test-render-detail-shows-mark-action-for-comments ()
  "Every comment, your own and others', offers mark."
  (bitbucket-mock-with-service
    (let ((pr (car (gp-test--mock-prs)))
          (comments (alist-get 'values (bitbucket-mock--fixture "pr-comments.json"))))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr comments))
        (let ((text (substring-no-properties (buffer-string))))
          (should (= (length comments) (how-many "mark \\[m\\]" 1 (point-max))))
          (should (string-match-p "mark \\[m\\]" text)))))))

(ert-deftest gp-test-render-detail-shows-pending-comment-and-submit-action ()
  "A pending review-batch comment renders distinctly, with only edit and
remove-from-batch actions, and the header gains a Submit review action."
  (bitbucket-mock-with-service
    (let* ((pr (car (gp-test--mock-prs)))
           (all (alist-get 'values (bitbucket-mock--fixture "pr-comments.json")))
           (ordinary (car all))
           ;; a pending batch comment is always your own (user.json owns 808625667)
           (own (cl-find 808625667 all :key (lambda (c) (alist-get 'id c))))
           (pending (append '((pending . t) (id . 999999)) own))
           (comments (list ordinary pending)))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr comments))
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "pending review" text))
          (should (string-match-p "remove from batch \\[K\\]" text))
          (should (string-match-p "Submit review (1) \\[S\\]" text)))))))

(ert-deftest gp-test-render-detail-hides-submit-action-with-no-pending-comments ()
  (bitbucket-mock-with-service
    (let ((pr (car (gp-test--mock-prs)))
          (comments (alist-get 'values (bitbucket-mock--fixture "pr-comments.json"))))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr comments))
        (let ((text (substring-no-properties (buffer-string))))
          (should-not (string-match-p "Submit review" text)))))))

(defmacro gp-test--submitting-review (pr label summary &rest body)
  "Run BODY after `gp-ui-submit-review-batch' on PR.
LABEL is the verdict picked; SUMMARY answers the summary prompt (nil
declines the \"Add a summary comment?\" question).  `offered' is bound
to the verdict choices shown.  Call inside a mock service."
  (declare (indent 3) (debug t))
  `(let ((offered nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt collection &rest _)
                  (setq offered collection)
                  ,label))
               ((symbol-function 'yes-or-no-p) (lambda (&rest _) (and ,summary t)))
               ((symbol-function 'read-string) (lambda (&rest _) (or ,summary "")))
               ((symbol-function 'gp-detail-refresh) #'ignore))
       (gp-ui-submit-review-batch ,pr))
     (ignore offered)                   ;; not every BODY looks at it
     ,@body))

(defun gp-test--github-submitted-review ()
  "The variables of the submitPullRequestReview mutation the mock saw."
  (cadr (cl-find-if (lambda (c) (string-match-p "submitPullRequestReview" (car c)))
                    github-mock-graphql-calls)))

(ert-deftest gp-test-submit-review-batch-github-sends-the-chosen-verdict ()
  "Each verdict label maps to GitHub's review event; an empty summary
is sent as no body at all, a typed one verbatim."
  (github-mock-with-service
    (gp-test--submitting-review (gp-test--gh-pr) "Approve" nil
      (should (equal offered '("Approve" "Request changes" "Comment only (no verdict)")))
      (let ((vars (gp-test--github-submitted-review)))
        (should (equal (alist-get 'event vars) "APPROVE"))
        (should (assq 'body vars))
        (should-not (alist-get 'body vars)))))
  (github-mock-with-service
    (gp-test--submitting-review (gp-test--gh-pr) "Request changes" "Please split this up"
      (let ((vars (gp-test--github-submitted-review)))
        (should (equal (alist-get 'event vars) "REQUEST_CHANGES"))
        (should (equal (alist-get 'body vars) "Please split this up")))))
  (github-mock-with-service
    (gp-test--submitting-review (gp-test--gh-pr) "Comment only (no verdict)" "Looks fine so far"
      (let ((vars (gp-test--github-submitted-review)))
        (should (equal (alist-get 'event vars) "COMMENT"))
        (should (equal (alist-get 'body vars) "Looks fine so far"))))))

(defun gp-test--bitbucket-verdict-posts ()
  "The approve / request-changes POST paths the Bitbucket mock saw."
  (mapcar (lambda (c) (nth 1 c))
          (cl-remove-if-not
           (lambda (c) (and (equal (car c) "POST")
                            (string-match-p "/\\(approve\\|request-changes\\)\\'" (nth 1 c))))
           bitbucket-mock-calls)))

(ert-deftest gp-test-submit-review-batch-bitbucket-sends-the-chosen-verdict ()
  "On Bitbucket the verdict becomes an approve / request-changes call;
\"Comment only\" sends neither."
  (bitbucket-mock-with-service
    (gp-test--submitting-review '((id . 11) (destination (repository (full_name . "acme/x")))) "Approve" nil
      (should (equal offered '("Approve" "Request changes" "Comment only (no verdict)")))
      (should (equal (gp-test--bitbucket-verdict-posts)
                     '("/repositories/acme/x/pullrequests/11/approve")))))
  (bitbucket-mock-with-service
    (gp-test--submitting-review '((id . 11) (destination (repository (full_name . "acme/x")))) "Request changes" nil
      (should (equal (gp-test--bitbucket-verdict-posts)
                     '("/repositories/acme/x/pullrequests/11/request-changes")))))
  (bitbucket-mock-with-service
    (gp-test--submitting-review '((id . 11) (destination (repository (full_name . "acme/x")))) "Comment only (no verdict)" nil
      (should-not (gp-test--bitbucket-verdict-posts))
      ;; it still went through the batch (fetching the pending comments)
      (should (cl-find "/repositories/acme/x/pullrequests/11/comments"
                       bitbucket-mock-calls :key #'cadr :test #'equal)))))

(defmacro gp-test--without-diff-fetch (&rest body)
  "Run BODY with the deferred stats/diff fetch answering nil at once.
`bitbucket-mock-with-service' does not serve the raw-text diff
\(`bitbucket-api-get-text-async'), so this keeps the refresh offline."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'gp-pull-request-stats-async)
              (lambda (_fn _id _pr cb) (funcall cb nil)))
             ((symbol-function 'gp-pull-request-diff-async)
              (lambda (_fn _id _c _pr cb) (funcall cb nil))))
     ,@body))

(ert-deftest gp-test-detail-refresh-uses-async-path-with-cached-content ()
  "`g' refetches the PR and its comments through the async path (no
full `gp-show-pr' reload), and swaps them in.
Kept beside the end-to-end refresh flows (gp-e2e-detail-test.el) for
its last part, cached stats/diff surviving a failed refetch: no flow
can reach that yet, since the Bitbucket mock has no diffstat route and
GitHub's stats fetch reports a failed files request as an empty list."
  (bitbucket-mock-with-service
    (let ((pr '((id . 180) (title . "Old")
                (destination (repository (full_name . "acme/web-frontend")))))
          (show-pr-called nil))
      (with-temp-buffer
        (gp-detail-mode)
        (setq gp--pr pr
              gp--detail-comments '(((id . 0) (content (raw . "old"))))
              gp--detail-stats '(:commits 1 :files 1 :added 0 :removed 0)
              gp--detail-diff '(("a" . "diff"))
              gp--detail-pipelines '(:recent nil))
        (cl-letf (((symbol-function 'gp-show-pr)
                   (lambda (&rest _) (setq show-pr-called t))))
          (gp-test--without-diff-fetch (gp-detail-refresh)))
        (should-not show-pr-called)
        ;; both fetches went out, for this PR
        (let ((paths (mapcar #'cadr bitbucket-mock-calls)))
          (should (member "/repositories/acme/web-frontend/pullrequests/180" paths))
          (should (member "/repositories/acme/web-frontend/pullrequests/180/comments" paths)))
        ;; and their results replaced the old ones
        (should (equal (alist-get 'title gp--pr) "Add feature toggle for the new widget"))
        (should (equal (mapcar (lambda (c) (alist-get 'id c)) gp--detail-comments)
                       (mapcar (lambda (c) (alist-get 'id c))
                               (alist-get 'values (bitbucket-mock--fixture "pr-comments.json")))))
        (should (string-match-p "Add feature toggle for the new widget"
                                (substring-no-properties (buffer-string))))
        ;; cached heavier content survives a refresh whose fetch came back empty
        (should (equal gp--detail-stats '(:commits 1 :files 1 :added 0 :removed 0)))
        (should (equal gp--detail-diff '(("a" . "diff"))))))))

(defun gp-test--refresh-busted ()
  "Run `gp-detail-refresh' on a Bitbucket PR; return what the deploy-cache
bust received, or `none' when it was not called (or not defined)."
  (bitbucket-mock-with-service
    (let ((pr '((id . 180) (source (commit (hash . "abc")))
                (destination (repository (full_name . "acme/web-frontend")))))
          (busted 'none))
      (with-temp-buffer
        (gp-detail-mode)
        (setq gp--pr pr gp--detail-pipelines '(:recent nil))
        (when (fboundp 'gp-helm--deploy-cache-bust)
          (advice-add 'gp-helm--deploy-cache-bust :override
                      (lambda (p) (setq busted p)) '((name . gp-test-bust))))
        (unwind-protect (gp-test--without-diff-fetch (gp-detail-refresh))
          (when (fboundp 'gp-helm--deploy-cache-bust)
            (advice-remove 'gp-helm--deploy-cache-bust 'gp-test-bust)))
        busted))))

(ert-deftest gp-test-detail-refresh-busts-deploy-cache-when-helm-loaded ()
  "`gp-detail-refresh' forgets the overview's cached deploy verdict for
this PR -- but only when `gp-helm' is actually loaded (the fboundp
guard), since this file does not require it."
  ;; gp-helm loaded: the PR shown at refresh time is busted
  (cl-letf (((symbol-function 'gp-helm--deploy-cache-bust) #'ignore))
    (should (equal (alist-get 'id (gp-test--refresh-busted)) 180)))
  ;; gp-helm not loaded: refresh still works and nothing is called
  (let ((orig (and (fboundp 'gp-helm--deploy-cache-bust)
                   (symbol-function 'gp-helm--deploy-cache-bust))))
    (fmakunbound 'gp-helm--deploy-cache-bust)
    (unwind-protect
        (should (eq (gp-test--refresh-busted) 'none))
      (when orig (fset 'gp-helm--deploy-cache-bust orig)))))

(ert-deftest gp-test-every-buffer-name-is-tagged ()
  "No buffer this package opens may escape the shared prefix.
Untagged buffers are the thing `gp-buffer-name-prefix' exists to
prevent, so each producer is checked rather than trusted."
  (let ((tag "*gp: "))
    (dolist (name (list gp-list-buffer-name
                        gp-create-buffer
                        gp-compose-preview-buffer
                        gp-log-buffer-name
                        (gp--detail-buffer-name '((id . 7) (title . "t")))
                        (gp-reviewers--buffer-name '((id . 7)))))
      (should (string-prefix-p tag name)))))

;;;; Resolved-thread collapsing -------------------------------------------------

(ert-deftest gp-test-thread-resolved-follows-parent ()
  "A reply counts as belonging to a resolved thread via its parent.
Bitbucket sets `resolution' only on the comment the resolve action
targeted, so the reply's own resolution is nil."
  (let* ((root '((id . 1) (resolution . ((type . "resolved")))))
         (reply '((id . 2) (parent . ((id . 1)))))
         (by-id (gp--comments-by-id (list root reply))))
    (should (gp--comment-thread-resolved-p root by-id))
    (should (gp--comment-thread-resolved-p reply by-id))
    ;; the reply itself is NOT resolved -- only its thread is
    (should-not (gp-comment-resolved-p reply))))

(ert-deftest gp-test-thread-resolved-unresolved-thread ()
  "Replies in an unresolved thread stay expanded."
  (let* ((root '((id . 1)))
         (reply '((id . 2) (parent . ((id . 1)))))
         (by-id (gp--comments-by-id (list root reply))))
    (should-not (gp--comment-thread-resolved-p root by-id))
    (should-not (gp--comment-thread-resolved-p reply by-id))))

(ert-deftest gp-test-thread-resolved-survives-cycle ()
  "A cyclic parent chain terminates instead of looping forever."
  (let* ((a '((id . 1) (parent . ((id . 2)))))
         (b '((id . 2) (parent . ((id . 1)))))
         (by-id (gp--comments-by-id (list a b))))
    (should-not (gp--comment-thread-resolved-p a by-id))))

(ert-deftest gp-test-thread-resolved-orphan-reply ()
  "A reply whose parent is absent falls back to its own status."
  (let* ((reply '((id . 2) (parent . ((id . 99)))))
         (by-id (gp--comments-by-id (list reply))))
    (should-not (gp--comment-thread-resolved-p reply by-id))))

;;;; PR labels ---------------------------------------------------------------

(ert-deftest gp-test-labels-render-in-the-list-heading ()
  "A GitHub PR's labels appear in the overview heading, after the title."
  (github-mock-with-service
    (let* ((git-platform-current-backend (git-platform-github))
           (h (substring-no-properties (gp--pr-heading (gp-test--gh-pr)))))
      (should (string-match-p "bug" h))
      (should (string-match-p "ui" h))
      ;; after the title, before the repo slug
      (should (< (string-match "Add the widget toggle" h) (string-match "bug" h)))
      (should (< (string-match "bug" h) (string-match (regexp-quote "[web]") h))))))

(ert-deftest gp-test-labels-absent-from-heading-leave-no-gap ()
  "A PR with no labels renders the heading exactly as before.
The separator lives with the labels, so an unlabelled PR must not carry
a stray double space where they would have gone."
  (github-mock-with-service
    (let* ((git-platform-current-backend (git-platform-github))
           (h (substring-no-properties (gp--pr-heading github-mock--pr-2))))
      (should (string-match-p (regexp-quote "Fix the flaky test  [web]") h)))))

(defun gp-test--repo-line (text)
  "Return the 📁 line of rendered detail TEXT, or nil."
  (seq-find (lambda (l) (string-prefix-p "📁" l)) (split-string text "\n")))

(ert-deftest gp-test-detail-shows-the-repo-above-the-branches ()
  "A detail buffer reached from a workspace-wide list has to say which
repo it is for -- the branch line answers where-to-where, not in-what.
The owner is what tells two same-named repos apart, so the full name
is shown rather than the bare slug, on both backends."
  (bitbucket-mock-with-service
    (with-temp-buffer
      (gp-detail-mode)
      (let ((inhibit-read-only t))
        (gp--render-detail (car (gp-test--mock-prs)) nil))
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "acme/web-frontend" (gp-test--repo-line text)))
        ;; above the branch line, below the title
        (should (< (string-match "📁" text) (string-match "🔀" text))))))
  (github-mock-with-service
    (with-temp-buffer
      (gp-detail-mode)
      (let ((inhibit-read-only t))
        (gp--render-detail (gp-test--gh-pr) nil))
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "acme/web\\'" (gp-test--repo-line text)))
        (should (< (string-match "📁" text) (string-match "🔀" text)))))))

(ert-deftest gp-test-detail-repo-glyph-is-not-reused-in-the-header ()
  "Two different things wearing one glyph defeats the scanning it is for:
the checkout button already owns 📦."
  (bitbucket-mock-with-service
    (with-temp-buffer
      (gp-detail-mode)
      (let ((inhibit-read-only t))
        (gp--render-detail (car (gp-test--mock-prs)) nil))
      (let* ((text (substring-no-properties (buffer-string)))
             (repo-line (car (seq-filter
                              (lambda (l) (string-match-p "web-frontend" l))
                              (split-string text "\n")))))
        (should (string-prefix-p "📁" repo-line))))))

(ert-deftest gp-test-detail-warns-about-uncommitted-local-changes ()
  "The notice fires from `gp--detail-local-dirty' alone, and names how
many files and which branch they are sitting on."
  (bitbucket-mock-with-service
    (with-temp-buffer
      (gp-detail-mode)
      (setq gp--detail-local-dirty
            (list :dir "/repo" :branch "feat/x" :count 3))
      (let ((inhibit-read-only t))
        (gp--render-detail (car (gp-test--mock-prs)) nil))
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "⚠ Uncommitted local changes" text))
        (should (string-match-p "3 files on feat/x" text))))))

(ert-deftest gp-test-detail-dirty-warning-sits-near-the-top ()
  "It has to land above the description and the file list: its whole job
is to be read BEFORE the checkout buttons a few lines up get pressed."
  (bitbucket-mock-with-service
    (with-temp-buffer
      (gp-detail-mode)
      (setq gp--detail-local-dirty (list :dir "/repo" :branch "main" :count 1))
      (let ((inhibit-read-only t))
        (gp--render-detail (car (gp-test--mock-prs)) nil))
      (let* ((text (substring-no-properties (buffer-string)))
             (warn (string-match "Uncommitted local changes" text))
             (comments (string-match "Comments (" text)))
        (should warn)
        (should comments)
        (should (< warn comments))
        ;; and below the header block it is warning you about
        (should (< (string-match "🔀" text) warn))))))

(ert-deftest gp-test-detail-dirty-warning-singular-file ()
  "One file does not read as \"1 files\"."
  (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}")))
    (with-temp-buffer
      (gp-detail-mode)
      (setq gp--detail-local-dirty (list :dir "/repo" :branch "main" :count 1))
      (let ((inhibit-read-only t))
        (gp--render-detail '((id . 9) (title . "t")) nil))
      (should (string-match-p "1 file on main"
                              (substring-no-properties (buffer-string)))))))

(ert-deftest gp-test-detail-dirty-warning-absent-when-tree-is-clean ()
  "A clean tree (or no clone, or not yet checked) draws nothing at all --
the state is nil in every one of those cases."
  (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}")))
    (with-temp-buffer
      (gp-detail-mode)
      (setq gp--detail-local-dirty nil)
      (let ((inhibit-read-only t))
        (gp--render-detail '((id . 9) (title . "t")) nil))
      (should-not (string-match-p "Uncommitted"
                                  (substring-no-properties (buffer-string)))))))

(ert-deftest gp-test-detail-dirty-warning-survives-a-missing-branch ()
  "A detached HEAD gives no branch name; the count still says the useful
part rather than the whole notice disappearing."
  (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}")))
    (with-temp-buffer
      (gp-detail-mode)
      (setq gp--detail-local-dirty (list :dir "/repo" :branch nil :count 2))
      (let ((inhibit-read-only t))
        (gp--render-detail '((id . 9) (title . "t")) nil))
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "Uncommitted local changes" text))
        (should (string-match-p "(2 files)" text))))))

(ert-deftest gp-test-detail-dirty-render-never-shells-out-to-git ()
  "The renderer reads the cached plist only.  A `git status' during
redisplay would block Emacs and re-enter the renderer -- the same rule
the mergeability fetch follows."
  (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}"))
            ((symbol-function 'gp-checkout-dirty-count)
             (lambda (&rest _) (error "render asked git")))
            ((symbol-function 'gp-local-find-checkout)
             (lambda (&rest _) (error "render resolved a checkout"))))
    (with-temp-buffer
      (gp-detail-mode)
      (setq gp--detail-local-dirty (list :dir "/repo" :branch "main" :count 1))
      (let ((inhibit-read-only t))
        (gp--render-detail '((id . 9) (title . "t")) nil))
      (should (string-match-p "Uncommitted"
                              (substring-no-properties (buffer-string)))))))

(ert-deftest gp-test-detail-load-local-dirty-reads-a-real-clone ()
  "End to end against real git: a clone of the PR's repo with two
uncommitted files yields a count of 2 on its checked-out branch, and
the rerendered buffer shows the warning."
  (skip-unless (executable-find "git"))
  (gp-test--with-git-root
    (with-temp-file (expand-file-name "a.txt" repo) (insert "a"))
    (with-temp-file (expand-file-name "b.txt" repo) (insert "b"))
    (with-temp-buffer
      (gp-detail-mode)
      (let ((pr '((id . 9) (title . "t")
                  (source (branch (name . "feature")))
                  (destination (branch (name . "main"))
                               (repository (full_name . "acme/x"))))))
        (setq gp--pr pr)
        (bitbucket-mock-with-service      ;; answers the rerender's /user
          (cl-letf (((symbol-function 'run-at-time) ;; run the deferred body now
                     (lambda (_time _rep fn &rest args) (apply fn args))))
            (gp--detail-load-local-dirty (current-buffer) pr)))
        (should (equal (plist-get gp--detail-local-dirty :count) 2))
        (should (equal (plist-get gp--detail-local-dirty :branch) "feature"))
        (should (equal (file-truename (plist-get gp--detail-local-dirty :dir))
                       (file-truename repo)))
        (should (string-match-p "2 files on feature"
                                (substring-no-properties (buffer-string))))))))

(ert-deftest gp-test-detail-load-local-dirty-fills-the-state ()
  "The loader resolves the checkout, counts, and stores the plist."
  (with-temp-buffer
    (gp-detail-mode)
    (let ((pr '((id . 9) (title . "t")
                (source (branch (name . "feat/x")))
                (destination (branch (name . "main"))))))
      (setq gp--pr pr)
      (cl-letf (((symbol-function 'gp-pr-full-name) (lambda (_pr) "acme/web"))
                ((symbol-function 'gp-local-find-checkout) (lambda (_fn) "/repo"))
                ((symbol-function 'gp-checkout-dirty-count) (lambda (_dir) 4))
                ((symbol-function 'gp-checkout-current-branch) (lambda (_dir) "feat/x"))
                ((symbol-function 'gp--detail-rerender) (lambda (&rest _) nil))
                ;; run the deferred body immediately
                ((symbol-function 'run-at-time)
                 (lambda (_time _rep fn &rest args) (apply fn args))))
        (gp--detail-load-local-dirty (current-buffer) pr)
        (should (equal gp--detail-local-dirty
                       (list :dir "/repo" :branch "feat/x" :count 4)))))))

(ert-deftest gp-test-detail-load-local-dirty-clean-tree-stays-nil ()
  "Zero changed files must not produce a state -- the renderer keys the
whole notice off non-nil."
  (with-temp-buffer
    (gp-detail-mode)
    (let ((pr '((id . 9))))
      (setq gp--pr pr)
      (cl-letf (((symbol-function 'gp-pr-full-name) (lambda (_pr) "acme/web"))
                ((symbol-function 'gp-local-find-checkout) (lambda (_fn) "/repo"))
                ((symbol-function 'gp-checkout-dirty-count) (lambda (_dir) 0))
                ((symbol-function 'gp--detail-rerender) (lambda (&rest _) nil))
                ((symbol-function 'run-at-time)
                 (lambda (_time _rep fn &rest args) (apply fn args))))
        (gp--detail-load-local-dirty (current-buffer) pr)
        (should-not gp--detail-local-dirty)))))

(ert-deftest gp-test-detail-load-local-dirty-never-clones ()
  "Painting a warning must not clone a repo or switch a branch: with no
local checkout the loader resolves nothing and stores nothing."
  (with-temp-buffer
    (gp-detail-mode)
    (let ((pr '((id . 9)))
          (ensured nil))
      (setq gp--pr pr)
      (cl-letf (((symbol-function 'gp-pr-full-name) (lambda (_pr) "acme/web"))
                ((symbol-function 'gp-local-find-checkout) (lambda (_fn) nil))
                ((symbol-function 'gp-local-ensure-checkout)
                 (lambda (&rest _) (setq ensured t) "/repo"))
                ((symbol-function 'gp-checkout-dirty-count)
                 (lambda (_dir) (error "counted without a checkout")))
                ((symbol-function 'gp--detail-rerender) (lambda (&rest _) nil))
                ((symbol-function 'run-at-time)
                 (lambda (_time _rep fn &rest args) (apply fn args))))
        (gp--detail-load-local-dirty (current-buffer) pr)
        (should-not gp--detail-local-dirty)
        (should-not ensured)))))

(ert-deftest gp-test-detail-load-local-dirty-skips-redraw-when-unchanged ()
  "A clean tree is the common case; re-rendering the buffer to draw
nothing would throw away point and section visibility on every refresh."
  (with-temp-buffer
    (gp-detail-mode)
    (let ((pr '((id . 9)))
          (renders 0))
      (setq gp--pr pr
            gp--detail-local-dirty (list :dir "/repo" :branch "main" :count 2))
      (cl-letf (((symbol-function 'gp-pr-full-name) (lambda (_pr) "acme/web"))
                ((symbol-function 'gp-local-find-checkout) (lambda (_fn) "/repo"))
                ((symbol-function 'gp-checkout-dirty-count) (lambda (_dir) 2))
                ((symbol-function 'gp-checkout-current-branch) (lambda (_dir) "main"))
                ((symbol-function 'gp--detail-rerender)
                 (lambda (&rest _) (cl-incf renders)))
                ((symbol-function 'run-at-time)
                 (lambda (_time _rep fn &rest args) (apply fn args))))
        ;; same answer as what is already stored -> no redraw
        (gp--detail-load-local-dirty (current-buffer) pr)
        (should (= renders 0))
        ;; a real change does redraw
        (cl-letf (((symbol-function 'gp-checkout-dirty-count) (lambda (_dir) 5)))
          (gp--detail-load-local-dirty (current-buffer) pr))
        (should (= renders 1))))))

(ert-deftest gp-test-detail-load-local-dirty-survives-a-git-failure ()
  "A git error is logged, not signalled: a broken status check must not
take down the rest of the detail load."
  (with-temp-buffer
    (gp-detail-mode)
    (let ((pr '((id . 9))))
      (setq gp--pr pr)
      (cl-letf (((symbol-function 'gp-pr-full-name) (lambda (_pr) "acme/web"))
                ((symbol-function 'gp-local-find-checkout)
                 (lambda (_fn) (error "boom")))
                ((symbol-function 'gp--detail-rerender) (lambda (&rest _) nil))
                ((symbol-function 'run-at-time)
                 (lambda (_time _rep fn &rest args) (apply fn args))))
        (should-not (gp--detail-load-local-dirty (current-buffer) pr))
        (should-not gp--detail-local-dirty)))))
(ert-deftest gp-test-label-carries-its-name-and-keymap ()
  "A rendered label knows which label it is, so a click on it needs no
re-derivation from the surrounding text."
  (github-mock-with-service
    (let* ((git-platform-current-backend (git-platform-github))
           (rendered (gp--format-labels (gp-pr-labels (gp-test--gh-pr))))
           (pos (string-match "bug" rendered)))
      (should (equal (get-text-property pos 'gp-label rendered) "bug"))
      (should (eq (get-text-property pos 'keymap rendered) gp-label-map))
      ;; and the click target is discoverable before clicking it
      (should (get-text-property pos 'mouse-face rendered))
      (should (string-match-p "click to filter"
                              (get-text-property pos 'help-echo rendered))))))

(ert-deftest gp-test-label-map-binds-click-and-key ()
  "Both routes into the filter are bound on the label itself, not on the
mode map, so `C-c C-c' keeps its meaning everywhere else in the buffer."
  (should (eq (lookup-key gp-label-map [mouse-1]) #'gp-filter-by-label))
  (should (eq (lookup-key gp-label-map (kbd "C-c C-c")) #'gp-filter-by-label))
  (should-not (lookup-key gp-list-mode-map (kbd "C-c C-c")))
  (should-not (lookup-key gp-detail-mode-map (kbd "C-c C-c"))))

(ert-deftest gp-test-label-at-point-reads-the-heading ()
  "The label is readable from a real rendered buffer, at the position a
click would land on."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github)))
      (with-temp-buffer
        (gp-list-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (gp-root)
            (gp--insert-pr (gp-test--gh-pr))))
        (goto-char (point-min))
        (should (search-forward "bug" nil t))
        (goto-char (match-beginning 0))
        (should (equal (gp-label-at-point) "bug"))
        ;; off the label there is nothing to filter by
        (goto-char (point-min))
        (should-not (gp-label-at-point))))))

(ert-deftest gp-test-filter-by-label-hands-off-to-helm ()
  "Where Helm is loaded the filter goes into its search box, so the
narrowing is visible and can be widened by ordinary editing."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (asked nil))
      (cl-letf (((symbol-function 'gp-helm-list-by-label)
                 (lambda (name) (setq asked name))))
        (with-temp-buffer
          (gp-list-mode)
          (let ((inhibit-read-only t))
            (magit-insert-section (gp-root)
              (gp--insert-pr (gp-test--gh-pr))))
          (goto-char (point-min))
          (search-forward "bug")
          (goto-char (match-beginning 0))
          (gp-filter-by-label)
          (should (equal asked "bug")))))))

(ert-deftest gp-test-filter-by-label-errors-off-a-label ()
  "Called where there is no label, it says so rather than guessing one."
  (with-temp-buffer
    (gp-list-mode)
    (should-error (gp-filter-by-label) :type 'user-error)))

(ert-deftest gp-test-list-labelled-filters-loaded-prs ()
  "The Helm-less fallback filters the PRs already in hand -- no refetch
on a click -- and shows only the ones actually carrying the label."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (buf nil))
      (unwind-protect
          (with-temp-buffer
            (gp-list-mode)
            (setq gp--prs (list (gp-test--gh-pr) (github--reshape-pr github-mock--pr-2)))
            (cl-letf (((symbol-function 'pop-to-buffer)
                       (lambda (b &rest _) (setq buf b))))
              (gp--list-labelled "bug"))
            (with-current-buffer buf
              (let ((text (substring-no-properties (buffer-string))))
                (should (string-match-p "Labelled bug" text))
                (should (string-match-p "Add the widget toggle" text))
                ;; pr-2 has no labels at all, so it must not be listed
                (should-not (string-match-p "Fix the flaky test" text)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest gp-test-list-labelled-errors-when-nothing-matches ()
  "A label no loaded PR carries opens no empty buffer."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github)))
      (with-temp-buffer
        (gp-list-mode)
        (setq gp--prs (list github-mock--pr-2))
        (should-error (gp--list-labelled "bug") :type 'user-error)))))

(ert-deftest gp-test-labels-hidden-entirely-on-bitbucket ()
  "Bitbucket has no labels, so nothing label-shaped is rendered at all --
not the list segment, and not a \"no labels\" placeholder in the detail
view.  A slot that can never fill is worse than no slot."
  (let* ((git-platform-current-backend (git-platform-bitbucket))
         (pr (car (gp-test--mock-prs))))
    (should-not (gp-labels-supported-p))
    (should-not (gp-pr-labels pr))
    (with-temp-buffer
      (gp-detail-mode)
      (let ((inhibit-read-only t))
        (gp--insert-labels-line pr))
      (should (equal (buffer-string) "")))))

(ert-deftest gp-test-labels-line-in-detail-top-section ()
  "The detail view shows labels in its top section, with an edit button."
  (github-mock-with-service
    (with-temp-buffer
      (gp-detail-mode)
      (let ((inhibit-read-only t))
        (gp--render-detail (gp-test--gh-pr) nil))
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "🏷" text))
        (should (string-match-p "bug" text))
        (should (string-match-p "edit \\[L\\]" text))))))

(ert-deftest gp-test-labels-line-offers-edit-with-none-yet ()
  "With no labels on an open PR the line still appears, so the first one
can be added -- same reasoning as the reviewers line."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github)))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--insert-labels-line (github--reshape-pr github-mock--pr-2)))
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "no labels" text))
          (should (string-match-p "edit \\[L\\]" text)))))))

(ert-deftest gp-test-labels-line-has-no-edit-button-on-closed-pr ()
  "A merged/closed PR shows its labels read-only."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (pr (append '((state . "closed")) (gp-test--gh-pr))))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--insert-labels-line pr))
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "bug" text))
          (should-not (string-match-p "edit" text)))))))

;;;; Description section ------------------------------------------------------

(ert-deftest gp-test-description-heading-offers-edit-on-open-pr ()
  "The description heading carries a REAL edit button, like reviewers/labels.
Matching the label text is not enough: `insert-button' keeps its
properties in an overlay, which does not survive `buffer-string', so
building the heading as a string first yields plain text that merely
looks like a button.  Assert the button object, its face, and that
activating it actually calls the edit command -- and that the heading
text still gets `magit-section-heading'."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (pr (append '((body . "Some body text")) (gp-test--gh-pr)))
          (called nil))
      (cl-letf (((symbol-function 'gp-ui-edit-description)
                 (lambda (_pr) (setq called t))))
        (with-temp-buffer
          (gp-detail-mode)
          (let ((inhibit-read-only t))
            (gp--insert-description pr))
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "Description" text))
            (should (string-match-p "Some body text" text)))
          ;; the heading itself keeps magit's heading face
          (should (eq (get-text-property (point-min) 'face) 'magit-section-heading))
          ;; and there is a genuine, clickable button carrying the link face
          (let ((b (next-button (point-min))))
            (should b)
            (should (equal (button-label b) "✎ edit [E]"))
            (should (eq (button-get b 'face) 'gp-link-face))
            (button-activate b)
            (should called)))))))

(ert-deftest gp-test-description-empty-state-offers-edit-on-open-pr ()
  "The empty state carries the edit button, so `E' is discoverable there."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (pr (append '((body . "")) (gp-test--gh-pr))))
      (should (gp-pr-open-p pr))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--insert-description pr))
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "Description" text))
          (should (string-match-p "no description yet" text)))
        ;; a real button, not just matching text (see the test above)
        (let ((b (next-button (point-min))))
          (should b)
          (should (equal (button-label b) "✎ edit [E]")))))))

(ert-deftest gp-test-description-has-no-edit-button-on-closed-pr ()
  "A merged/closed PR shows its description read-only: the forges only
allow mutating an open PR, so offering the button would just produce an
API error."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (pr (append '((state . "closed") (body . "Some body text"))
                      (gp-test--gh-pr))))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--insert-description pr))
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "Some body text" text))
          (should-not (string-match-p "edit" text)))
        (should-not (next-button (point-min)))))))

(ert-deftest gp-test-description-absent-entirely-on-closed-pr-with-none ()
  "A CLOSED PR with no description still renders nothing at all.
The empty state exists to offer the edit button; where editing is
impossible it would be a slot that can never fill."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (pr (append '((state . "closed") (body . "")) (gp-test--gh-pr))))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--insert-description pr))
        (should (equal (buffer-string) ""))))))

(defun gp-test--buttons ()
  "Return the buffer's button overlays, ordered by position.
`overlays-in' returns them in an unspecified order -- and Emacs 28.2
really does differ from 29+ here, which broke a test that assumed the
order it happened to see locally."
  (sort (seq-filter (lambda (o) (overlay-get o 'button))
                    (overlays-in (point-min) (point-max)))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun gp-test--button-labels ()
  "Return the buffer's button labels, ordered by position."
  (mapcar (lambda (o) (buffer-substring-no-properties
                       (overlay-start o) (overlay-end o)))
          (gp-test--buttons)))

;;;; Title editing ---------------------------------------------------------------

(ert-deftest gp-test-title-edit-button-only-on-open-prs ()
  "The title carries an edit button while open, and none once closed."
  (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}")))
    (let ((open (car (gp-test--mock-prs))))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t)) (gp--render-detail open nil))
        (should (string-match-p "✎ \\[N\\]" (substring-no-properties (buffer-string))))))
    (let ((merged (append '((state . "MERGED")) (car (gp-test--mock-prs)))))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t)) (gp--render-detail merged nil))
        (should-not (string-match-p "✎ \\[N\\]"
                                    (substring-no-properties (buffer-string))))))))

;;;; Merge affordance -----------------------------------------------------------

(defun gp-test--render-with-mergeability (pr mergeability)
  "Render PR's detail with MERGEABILITY cached; return the text."
  (with-temp-buffer
    (gp-detail-mode)
    (setq gp--detail-mergeability mergeability)
    (let ((inhibit-read-only t)) (gp--render-detail pr nil))
    (substring-no-properties (buffer-string))))

(ert-deftest gp-test-merge-button-shown-when-mergeable ()
  "A clean open PR offers the merge button and no warning -- GitHub's
\"clean\", and Bitbucket's own (t . \"unverified\") answer."
  (github-mock-with-service
    (let ((txt (gp-test--render-with-mergeability (gp-test--gh-pr) '(t . "clean"))))
      (should (string-match-p "Merge \\[M\\]" txt))
      (should-not (string-match-p "Cannot merge" txt))))
  (bitbucket-mock-with-service
    (let ((txt (gp-test--render-with-mergeability
                (car (gp-test--mock-prs)) '(t . "unverified"))))
      (should (string-match-p "Merge \\[M\\]" txt))
      (should-not (string-match-p "Cannot merge" txt)))))

(ert-deftest gp-test-merge-blocked-and-warned-on-conflict ()
  "A conflicted PR replaces the button with the reason, and warns above.
The state is the useful part: leaving a live button to fail on click
tells the user nothing about why."
  (github-mock-with-service
    (let ((txt (gp-test--render-with-mergeability (gp-test--gh-pr) '(nil . "dirty"))))
      (should (string-match-p "Cannot merge: conflicts with main  (dirty)" txt))
      ;; the action simply goes away; the warning above is the one place
      ;; the reason is stated
      (should-not (string-match-p "Merge \\[M\\]" txt))))
  ;; Bitbucket's conflict comes from the diffstat, flagged as unverified
  (bitbucket-mock-with-service
    (let ((txt (gp-test--render-with-mergeability
                (car (gp-test--mock-prs)) '(nil . "unverified"))))
      (should (string-match-p "Cannot merge: conflicts with main  (unverified)" txt))
      (should-not (string-match-p "Merge \\[M\\]" txt)))))

(ert-deftest gp-test-merge-not-blocked-when-mergeability-unknown ()
  "`unknown' and nil must not warn or hide the button.
GitHub answers `unknown' until it has judged, and Bitbucket cannot
answer at all without a diffstat -- treating either as a conflict
would block merging a perfectly clean PR and show a warning about a
problem that may not exist."
  (github-mock-with-service
    (let ((txt (gp-test--render-with-mergeability (gp-test--gh-pr) '(unknown . "unknown"))))
      (should (string-match-p "Merge \\[M\\]" txt))
      (should-not (string-match-p "Cannot merge" txt))))
  (bitbucket-mock-with-service
    (let ((txt (gp-test--render-with-mergeability (car (gp-test--mock-prs)) nil)))
      (should (string-match-p "Merge \\[M\\]" txt))
      (should-not (string-match-p "Cannot merge" txt)))))

(ert-deftest gp-test-merge-affordance-absent-on-closed-pr ()
  "A merged/closed PR offers no merge action at all."
  (github-mock-with-service
    (should-not (string-match-p
                 "Merge \\[M\\]"
                 (gp-test--render-with-mergeability
                  (append '((state . "closed")) (gp-test--gh-pr)) '(t . "clean")))))
  (bitbucket-mock-with-service
    (should-not (string-match-p
                 "Merge \\[M\\]"
                 (gp-test--render-with-mergeability
                  (append '((state . "MERGED")) (car (gp-test--mock-prs)))
                  '(t . "unverified"))))))

(ert-deftest gp-test-outcome-banner-states-the-result ()
  "A closed PR announces MERGED or DECLINED; an open one says nothing.
An open PR's state is the whole detail view, so a banner there would be
noise -- the outcome only becomes the headline once it is decided."
  (cl-labels ((banner (pr)
                (with-temp-buffer
                  (gp-detail-mode)
                  (let ((inhibit-read-only t)) (gp--insert-outcome-banner pr))
                  (substring-no-properties (buffer-string)))))
    (let ((base (car (gp-test--mock-prs))))
      ;; open -> nothing at all
      (should (equal (banner base) ""))
      ;; merged -> MERGED
      (let ((merged (append '((state . "MERGED")) base)))
        (should (string-match-p "MERGED" (banner merged)))
        (should-not (string-match-p "DECLINED" (banner merged))))
      ;; declined -> DECLINED
      (let ((declined (append '((state . "DECLINED")) base)))
        (should (string-match-p "DECLINED" (banner declined)))))))

(ert-deftest gp-test-outcome-banner-shows-a-decline-reason-when-there-is-one ()
  "A declined PR shows its reason where the backend keeps one.
Bitbucket carries a free-text `reason'; GitHub has none, and must simply
show no reason rather than an empty line."
  (cl-labels ((banner (pr)
                (with-temp-buffer
                  (gp-detail-mode)
                  (let ((inhibit-read-only t)) (gp--insert-outcome-banner pr))
                  (substring-no-properties (buffer-string)))))
    (let ((base (car (gp-test--mock-prs)))
          (git-platform-current-backend (git-platform-bitbucket)))
      (let ((with-reason (append '((state . "DECLINED")
                                   (reason . "superseded by #12"))
                                 base)))
        (should (string-match-p "superseded by #12" (banner with-reason))))
      ;; no reason -> just the banner, no stray blank line claiming one
      (let ((without (append '((state . "DECLINED")) base)))
        (should (string-match-p "DECLINED" (banner without)))
        (should-not (string-match-p "superseded" (banner without))))
      ;; a merged PR never shows a reason even if the field is present
      (let ((merged (append '((state . "MERGED") (reason . "ignored")) base)))
        (should-not (string-match-p "ignored" (banner merged)))))
    ;; GitHub: closed unmerged reads as DECLINED, with no reason line at all
    (github-mock-with-service
      (let ((closed (append '((state . "closed") (merged . nil)) (gp-test--gh-pr))))
        (should (equal (banner closed) "  DECLINED  \n\n"))))))

(ert-deftest gp-test-merge-pipelines-omit-other-commits-runs ()
  "Only the merge commit's own run is shown, not the branch's history.
`:recent' is every other run on the destination branch -- everything
else that landed on main -- which has nothing to do with this PR."
  (bitbucket-mock-with-service
    (let ((pr (append '((state . "MERGED")) (car (gp-test--mock-prs))))
          (mine '((uuid . "{merge-run}") (build_number . 501)
                  (state (name . "COMPLETED") (result (name . "SUCCESSFUL")))
                  (target (commit (hash . "aaaa1111")))))
          (other '((uuid . "{other-run}") (build_number . 497)
                   (state (name . "COMPLETED") (result (name . "FAILED")))
                   (target (commit (hash . "bbbb2222"))))))
      (with-temp-buffer
        (gp-detail-mode)
        (setq gp--pr pr
              gp--detail-merge-pipelines
              (list :current (list (cons mine nil))
                    :recent (list (cons other "unrelated commit on main"))))
        (let ((inhibit-read-only t)) (gp--insert-merge-pipelines pr))
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "After merge (on main)" text))
          (should (string-match-p "501" text))
          (should-not (string-match-p "497" text))
          (should-not (string-match-p "unrelated commit on main" text)))
        ;; with no merge-commit run yet, other runs on main must not
        ;; conjure up a Pipelines section ("no pipeline for ...") either
        (setq gp--detail-merge-pipelines
              (list :current nil
                    :recent (list (cons other "unrelated commit on main"))))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (gp--insert-merge-pipelines pr))
        (should-not (string-match-p "Pipelines\\|no pipeline"
                                    (substring-no-properties (buffer-string))))))))

(ert-deftest gp-test-merge-pipelines-shown-only-on-a-merged-pr ()
  "The destination-branch run appears on a merged PR and nowhere else.
After a merge the interesting build is the one on the merge commit --
that is what deploys -- and it lives on the destination branch, so the
PR-branch fetch never sees it."
  ;; ((PIPELINE . STEPS) …).  Bitbucket nests the state, so it is
  ;; `.state.name' / `.state.result.name', not plain strings.
  (let ((data (list :current
                    (list (cons '((uuid . "{p1}") (build_number . 1)
                                  (state (name . "COMPLETED")
                                         (result (name . "SUCCESSFUL"))))
                                nil))
                    :recent nil)))
    ;; merged -> shown, naming the destination branch
    (let ((pr (append '((state . "MERGED")) (car (gp-test--mock-prs)))))
      (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}")))
        (with-temp-buffer
          (gp-detail-mode)
          (setq gp--detail-merge-pipelines data)
          (let ((inhibit-read-only t)) (gp--insert-merge-pipelines pr))
          (should (string-match-p "After merge"
                                  (substring-no-properties (buffer-string)))))))
    ;; still open -> nothing, even with data cached
    (let ((pr (car (gp-test--mock-prs))))
      (with-temp-buffer
        (gp-detail-mode)
        (setq gp--detail-merge-pipelines data)
        (let ((inhibit-read-only t)) (gp--insert-merge-pipelines pr))
        (should (equal (buffer-string) ""))))))

(ert-deftest gp-test-conflict-warning-offers-the-terminal-handoff ()
  "The warning carries a button to hand the conflict to the AI session."
  (let ((pr (gp-test--gh-pr))
        (git-platform-current-backend (git-platform-github))
        (called nil))
    (cl-letf (((symbol-function 'gp-ui-send-conflict-to-terminal)
               (lambda (_pr) (setq called t))))
      (with-temp-buffer
        (gp-detail-mode)
        (setq gp--detail-mergeability '(nil . "dirty"))
        (let ((inhibit-read-only t)) (gp--insert-merge-conflict-warning pr))
        (let ((b (seq-find (lambda (o) (overlay-get o 'button))
                           (overlays-in (point-min) (point-max)))))
          (should b)
          (should (equal (buffer-substring-no-properties
                          (overlay-start b) (overlay-end b))
                         "resolve in terminal [t]"))
          (funcall (overlay-get b 'action) b)
          (should called))))))

(ert-deftest gp-test-t-is-context-aware-on-the-conflict-warning ()
  "`t' hands over the conflict on the warning, a comment elsewhere.
A single global binding used to assume a comment at point, so pressing
`t' on the warning failed with \"Point is not on a comment\"."
  (let ((pr (gp-test--gh-pr))
        (git-platform-current-backend (git-platform-github))
        (sent nil))
    (cl-letf (((symbol-function 'gp-ui-send-conflict-to-terminal)
               (lambda (_pr) (setq sent 'conflict)))
              ((symbol-function 'gp-ui-send-comment-to-terminal)
               (lambda (&rest _) (setq sent 'comment)))
              ((symbol-function 'gp-detail--comment-at-point)
               (lambda () '((id . 1)))))
      (with-temp-buffer
        (gp-detail-mode)
        (setq gp--pr pr
              gp--detail-mergeability '(nil . "dirty"))
        (let ((inhibit-read-only t))
          (gp--insert-merge-conflict-warning pr)
          (insert "elsewhere\n"))
        ;; inside the warning -> the conflict
        (goto-char (point-min))
        (gp-detail-send-to-terminal)
        (should (eq sent 'conflict))
        ;; past it -> the ordinary comment handoff
        (setq sent nil)
        (goto-char (point-max))
        (forward-line -1)
        (gp-detail-send-to-terminal)
        (should (eq sent 'comment))))))

(ert-deftest gp-test-conflict-warning-names-how-far-behind ()
  "The behind count is shown when known, and omitted when not.
A zero would claim the branch is up to date when the truth is that the
backend cannot say (Bitbucket has no ahead/behind counts).  Only GitHub
reports a divergence, so this is a GitHub PR."
  (let ((pr (gp-test--gh-pr))
        (git-platform-current-backend (git-platform-github)))
    (cl-labels ((warning-text (divergence)
                  (with-temp-buffer
                    (gp-detail-mode)
                    (setq gp--detail-mergeability '(nil . "dirty")
                          gp--detail-divergence divergence)
                    (let ((inhibit-read-only t))
                      (gp--insert-merge-conflict-warning pr))
                    (substring-no-properties (buffer-string)))))
      (should (string-match-p "17 commits behind" (warning-text '(3 . 17))))
      (should (string-match-p "1 commit behind" (warning-text '(1 . 1))))
      ;; unknown, or genuinely level -> no claim either way
      (should-not (string-match-p "behind" (warning-text nil)))
      (should-not (string-match-p "behind" (warning-text '(3 . 0)))))))

;;;; General comments -----------------------------------------------------------

(ert-deftest gp-test-comments-heading-offers-a-general-comment-button ()
  "The Comments heading carries a real button to add a general comment.
General comments post to the issues endpoint -- no path, no line -- so
they are the one comment kind that cannot fail an out-of-diff check."
  (let ((pr (car (gp-test--mock-prs)))
        (called nil))
    (cl-letf (((symbol-function 'gp-ui-add-general-comment)
               (lambda (_pr) (setq called t)))
              ((symbol-function 'gp-user-uuid) (lambda () "{me}")))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr nil))
        (let* ((buttons (gp-test--buttons))
               (b (seq-find (lambda (o)
                              (equal (buffer-substring-no-properties
                                      (overlay-start o) (overlay-end o))
                                     "✎ comment [C]"))
                            buttons)))
          (should b)
          (should (eq (overlay-get b 'face) 'gp-link-face))
          ;; the heading text keeps magit's face despite the button beside it
          (goto-char (point-min))
          (should (re-search-forward "Comments (0)" nil t))
          (should (eq (get-text-property (match-beginning 0) 'face)
                      'magit-section-heading))
          (funcall (overlay-get b 'action) b)
          (should called))))))

(ert-deftest gp-test-general-comment-button-hidden-on-closed-pr ()
  "A merged/closed PR takes no new comments, so no button is offered."
  (let ((pr (append '((state . "MERGED")) (car (gp-test--mock-prs)))))
    (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}")))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr nil))
        (should-not (string-match-p "comment \\[C\\]"
                                    (substring-no-properties (buffer-string))))))))

(ert-deftest gp-test-general-comment-target-has-no-inline ()
  "The compose target must carry no :inline, or it would be posted as a
review comment and hit the very restriction this action avoids."
  (let ((pr (car (gp-test--mock-prs)))
        captured)
    (cl-letf (((symbol-function 'gp-compose) (lambda (target) (setq captured target) nil)))
      (gp-ui-add-general-comment pr))
    (should captured)
    (should-not (plist-get captured :inline))
    (should-not (plist-get captured :parent))
    (should (equal (plist-get captured :what) "general comment"))))

;;;; Reactions -----------------------------------------------------------------

(ert-deftest gp-test-reaction-tooltip-names-the-reactors ()
  "Hovering a reaction says who reacted, and what a click will do."
  (let ((tip (gp--reaction-tooltip "+1" 2 '("Ada Lovelace" "Bea") t)))
    (should (string-match-p "2 \\+1" tip))
    (should (string-match-p "Ada Lovelace, Bea" tip))
    (should (string-match-p "remove yours" tip)))
  ;; not mine yet -> the click adds
  (should (string-match-p "add yours" (gp--reaction-tooltip "heart" 1 '("Cy") nil))))

(ert-deftest gp-test-reaction-tooltip-abbreviates-a-long-list ()
  "A widely-liked comment must not produce an unreadable echo line."
  (let* ((names '("a" "b" "c" "d" "e" "f" "g"))
         (gp-reaction-names-max 3)
         (tip (gp--reaction-tooltip "+1" 7 names nil)))
    (should (string-match-p "a, b, c" tip))
    (should (string-match-p "\\+4 more" tip))
    (should-not (string-match-p "\\bd\\b" tip))))

(ert-deftest gp-test-reaction-emoji-falls-back-to-the-token ()
  "An unmapped token still renders readably rather than blank.
Guards against a platform adding a ninth reaction: better to show
\"sparkles\" than an empty button."
  (should (equal (gp-reaction-emoji "+1") "👍"))
  (should (equal (gp-reaction-emoji "rocket") "🚀"))
  (should (equal (gp-reaction-emoji "sparkles") "sparkles")))

(ert-deftest gp-test-reactions-unsupported-on-bitbucket ()
  "Bitbucket Cloud's API has no reactions, so the platform reports none.
Its web UI does have a binary Like, but no public v2.0 route exposes it
\(BCLOUD-21346 is still only a feature request), so the UI must hide
the affordance rather than offer an action that can only fail."
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (should-not (gp-reactions-supported-p))
    (should-not (gp-reaction-choices))
    (should-not (gp-comment-reactions "ws/repo" '((id . 1))))
    ;; and attempting one is a clear user-error, not a confusing API failure
    (should-error (gp-set-comment-reaction "ws/repo" '((id . 1)) "+1" t)
                  :type 'user-error)))

(ert-deftest gp-test-reactions-supported-on-github ()
  "GitHub reports that it has reactions, and offers some to pick from."
  (let ((git-platform-current-backend (git-platform-github)))
    (should (gp-reactions-supported-p))
    (should (member "+1" (gp-reaction-choices)))))

(ert-deftest gp-test-no-reaction-buttons-rendered-on-bitbucket ()
  "A platform without reactions renders no reaction affordance at all --
even for a comment that somehow carries `reaction-counts'."
  (bitbucket-mock-with-service
    (let* ((pr (car (gp-test--mock-prs)))
           (comments (alist-get 'values (bitbucket-mock--fixture "pr-comments.json")))
           (comments (cons (append '((reaction-counts . (("+1" . 2))))
                                   (car comments))
                           (cdr comments))))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr comments))
        (let ((text (substring-no-properties (buffer-string))))
          (should-not (string-match-p "react \\[!\\]" text))
          (should-not (string-match-p "👍" text)))))))

(defun gp-test--gh-comment (&optional reactions mine)
  "Bea's general comment 7 as `github--reshape-comment' shapes it.
REACTIONS is REST's inline `reactions' object; MINE the contents the
viewer reacted with (what the GraphQL prefetch would have recorded)."
  (let ((github--viewer-reactions (make-hash-table :test 'eql)))
    (when mine (puthash 7 mine github--viewer-reactions))
    (github--reshape-comment
     `((id . 7) (body . "hi")
       (user (login . "bea") (avatar_url . "https://example.com/bea.png"))
       (created_at . "2026-01-01T00:00:00Z")
       (html_url . "https://github.com/acme/web/pull/42#issuecomment-7")
       ,@(when reactions `((reactions . ,reactions)))))))

(ert-deftest gp-test-reaction-buttons-rendered-when-supported ()
  "Existing reactions render as buttons, counts in brackets, straight
from the comment's own `reaction-counts' -- drawing them must not hit
the network (a fetch mid-redisplay once re-entered the renderer and
drew each comment several times over).  The pill carries no ownership
glyph: a tick reads as \"done\" rather than \"me\"."
  (github-mock-with-service
    (with-temp-buffer
      (gp-detail-mode)
      (let ((inhibit-read-only t))
        (gp--insert-reactions
         (gp-test--gh-pr)
         (gp-test--gh-comment '((\+1 . 2) (rocket . 1)) '("+1"))))
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "👍 (2)" text))
        (should (string-match-p "🚀 (1)" text))
        ;; yours is not decorated -- same shape either way
        (should-not (string-match-p "✓" text)))
      ;; real buttons, one per distinct reaction, in the given order.
      ;; Counted via overlays: `insert-button' stores its properties
      ;; there, and walking `next-button' past the last one signals.
      (should (equal (gp-test--button-labels) '("👍 (2)" "🚀 (1)")))
      ;; and nothing was requested to draw them
      (should-not github-mock-calls)
      (should-not github-mock-graphql-calls))))

(ert-deftest gp-test-like-button-label-always-names-the-plus-key ()
  "The quick action stays [+] in both directions.
The bracket names the KEY, and `+' is what toggles either way -- `-' is
`negative-argument' globally, so labelling it [-] promised a key that
does nothing.  Which way the toggle will go is in the help text."
  (github-mock-with-service
    (let ((gp-comment-delete-others nil))
      (dolist (mine '(("+1") nil))
        (with-temp-buffer
          (gp-detail-mode)
          (let ((inhibit-read-only t))
            (gp--insert-comment (gp-test--gh-comment '((\+1 . 1)) mine)
                                (gp-test--gh-pr) 0 nil))
          (let ((txt (substring-no-properties (buffer-string))))
            (should (string-match-p "👍 \\[\\+\\]" txt))
            (should-not (string-match-p "👍 \\[-\\]" txt)))
          ;; but the help text still says which way a press will go
          (let* ((b (seq-find (lambda (o)
                                (equal (buffer-substring-no-properties
                                        (overlay-start o) (overlay-end o))
                                       "👍 [+]"))
                              (gp-test--buttons)))
                 (help (and b (overlay-get b 'help-echo))))
            (should (string-match-p (if mine "Remove" "Add") help))))))))

(ert-deftest gp-test-action-row-indent-matches-with-and-without-reactions ()
  "The action row sits at the same indent either way.
The reactions line used to have its indent inserted by the caller, which
left that whitespace on the action row when a comment had no reactions --
so removing your last reaction visibly shifted the row."
  (github-mock-with-service
    (cl-labels
        ((action-indent (comment)
           (with-temp-buffer
             (gp-detail-mode)
             (let ((inhibit-read-only t))
               (gp--insert-comment comment (gp-test--gh-pr) 0 nil))
             (let ((line (seq-find (lambda (l) (string-match-p "reply \\[R\\]" l))
                                   (split-string (substring-no-properties
                                                  (buffer-string))
                                                 "\n"))))
               (should line)
               (- (length line) (length (string-trim-left line)))))))
      (let ((with (gp-test--gh-comment '((\+1 . 1))))
            (without (gp-test--gh-comment)))
        ;; the two really differ in having reactions
        (should (alist-get 'reaction-counts with))
        (should-not (alist-get 'reaction-counts without))
        (should (= (action-indent with) (action-indent without)))))))

(defun gp-test--reaction-fetches ()
  "Number of reaction-list requests in `github-mock-calls'."
  (cl-count-if (lambda (c) (and (equal (car c) "GET")
                                (string-match-p "/reactions\\'" (nth 1 c))))
               github-mock-calls))

(ert-deftest gp-test-reaction-help-echo-is-lazy-and-names-reactors ()
  "The tooltip fetches only when shown, and then names who reacted.
Also decides `mine' from those rows: the comment payload carries counts
but not reactors, so a button cannot know whose reaction it is."
  (github-mock-with-service
    (let ((path "/repos/acme/web/issues/comments/7/reactions"))
      (setq github-mock--reactions
            (list (list 5002 path "+1" "bea")
                  (list 5001 path "+1" "ada")
                  (list 5003 path "heart" "cy")))
      (let ((fn (gp--reaction-help-echo (gp-test--gh-pr) (gp-test--gh-comment)
                                        "+1" 2 nil)))
        ;; building it fetches nothing
        (should (= (gp-test--reaction-fetches) 0))
        (let ((tip (funcall fn)))
          (should (= (gp-test--reaction-fetches) 1))
          (should (member path (mapcar #'cadr github-mock-calls)))
          ;; only the +1 reactors, not cy's heart
          (should (string-match-p "ada, bea" tip))
          (should-not (string-match-p "cy" tip))
          ;; "ada" is the signed-in user (mock /user), so the click removes
          (should (string-match-p "remove yours" tip)))))))

(ert-deftest gp-test-label-face-uses-the-platform-color ()
  "Each distinct colour gets its own face, interned once and reused.
Text colour flips with background luminance so a dark label stays
readable."
  (cl-letf (((symbol-function 'display-color-cells) (lambda (&rest _) 16777216)))
    (let ((gp--label-face-cache (make-hash-table :test 'equal))
          (gp-label-colors t))
      (let ((dark (gp--label-face "d73a4a"))
            (light (gp--label-face "a2eeef")))
        (should-not (eq dark light))
        (should (equal (face-attribute dark :background) "#d73a4a"))
        (should (equal (face-attribute dark :foreground) "white"))
        (should (equal (face-attribute light :foreground) "black"))
        ;; same hex -> same face object, not a second one
        (should (eq dark (gp--label-face "d73a4a")))))))

(ert-deftest gp-test-label-face-falls-back-without-color-or-on-tty ()
  "No colour, a malformed one, `gp-label-colors' nil, or a low-colour
display all fall back to the single themable face."
  (cl-letf (((symbol-function 'display-color-cells) (lambda (&rest _) 16777216)))
    (let ((gp--label-face-cache (make-hash-table :test 'equal))
          (gp-label-colors t))
      (should (eq (gp--label-face nil) 'gp-label-face))
      (should (eq (gp--label-face "nothex") 'gp-label-face))
      (should (eq (gp--label-face "abc") 'gp-label-face))
      (let ((gp-label-colors nil))
        (should (eq (gp--label-face "d73a4a") 'gp-label-face)))))
  ;; a 16-colour terminal cannot place arbitrary backgrounds
  (cl-letf (((symbol-function 'display-color-cells) (lambda (&rest _) 16)))
    (let ((gp--label-face-cache (make-hash-table :test 'equal))
          (gp-label-colors t))
      (should (eq (gp--label-face "d73a4a") 'gp-label-face)))))

(defun gp-test--label-puts ()
  "The (PATH LABELS) of every label PUT in `github-mock-calls', oldest first."
  (mapcar (lambda (c) (list (nth 1 c) (append (alist-get 'labels (nth 3 c)) nil)))
          (reverse (cl-remove-if-not
                    (lambda (c) (and (equal (car c) "PUT")
                                     (string-match-p "/labels\\'" (nth 1 c))))
                    github-mock-calls))))

(ert-deftest gp-test-edit-labels-no-change-skips-the-write ()
  "Leaving the pre-filled set untouched must not PUT or refresh --
a no-op edit should not churn the PR or invalidate caches."
  (github-mock-with-service
    (let ((refreshed nil))
      (cl-letf (((symbol-function 'completing-read-multiple)
                 ;; same set the PR already has, in the other order
                 (lambda (&rest _) '("ui" "bug")))
                ((symbol-function 'gp-detail-refresh)
                 (lambda (&rest _) (setq refreshed t))))
        (gp-ui-edit-labels (gp-test--gh-pr))
        (should-not (gp-test--label-puts))
        (should-not refreshed)))))

(ert-deftest gp-test-edit-labels-rejects-a-name-outside-the-pool ()
  "An unknown name is refused rather than silently creating a new label
on the repo, which is what GitHub's endpoint would otherwise do."
  (github-mock-with-service
    (cl-letf (((symbol-function 'completing-read-multiple)
               (lambda (&rest _) '("typoo"))))
      (should-error (gp-ui-edit-labels (gp-test--gh-pr)) :type 'user-error)
      (should-not (gp-test--label-puts)))))

(ert-deftest gp-test-edit-labels-refuses-on-bitbucket-and-closed-prs ()
  "The command reports rather than acting where labels cannot apply."
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (should-error (gp-ui-edit-labels (car (gp-test--mock-prs))) :type 'user-error))
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github)))
      (should-error (gp-ui-edit-labels (append '((state . "closed"))
                                               (gp-test--gh-pr)))
                    :type 'user-error))))

(ert-deftest gp-test-edit-labels-is-bound-to-a-capital-key ()
  "Label editing mutates the PR, so it takes a capital key (see `R'/`V')."
  (should (eq (lookup-key gp-detail-mode-map "L") #'gp-detail-edit-labels))
  ;; lowercase `l' keeps its read-only pipeline-log meaning
  (should (eq (lookup-key gp-detail-mode-map "l") #'gp-detail-pipeline-step-log)))

;;;; Async stats/diff loading ---------------------------------------------------

;; `gp--detail-load-stats-diff' used to call the synchronous
;; `gp-pull-request-stats'/`gp-pull-request-diff', which blocked Emacs for the
;; whole round-trip (~1.2s measured) a beat after every `g'.

(defconst gp-test--two-file-diff
  (concat "diff --git a/app.py b/app.py\n"
          "--- a/app.py\n+++ b/app.py\n"
          "@@ -1,1 +1,1 @@\n-old\n+new\n"
          "diff --git a/docs/readme.md b/docs/readme.md\n"
          "--- a/docs/readme.md\n+++ b/docs/readme.md\n"
          "@@ -1,0 +1,1 @@\n+hello\n")
  "A unified diff touching two files, as the diff endpoints return it.")

(ert-deftest gp-test-load-stats-diff-folds-in-each-result ()
  "Stats and diff land independently, each rendering what has arrived,
and only through the async ops -- never the blocking twins."
  (let ((pr '((id . 7) (title . "t")
              (destination (repository (full_name . "acme/x")))
              (source (commit (hash . "abc123")))))
        (git-platform-current-backend (git-platform-bitbucket))
        (sync-calls nil))
    (with-temp-buffer
      (gp-detail-mode)
      (setq gp--pr pr)
      (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}"))
                ((symbol-function 'gp-pull-request-stats-async)
                 (lambda (_fn _id _pr cb) (funcall cb '(:commits 2 :files 3 :added 9 :removed 1))))
                ((symbol-function 'gp-pull-request-diff-async)
                 (lambda (_fn _id _c _pr cb) (funcall cb gp-test--two-file-diff)))
                ;; tripwires: a blocking fetch would freeze Emacs
                ((symbol-function 'gp-pull-request-stats)
                 (lambda (&rest _) (push 'stats-sync sync-calls) nil))
                ((symbol-function 'gp-pull-request-diff)
                 (lambda (&rest _) (push 'diff-sync sync-calls) nil)))
        (gp--detail-load-stats-diff (current-buffer) pr 0))
      (should-not sync-calls)
      (should (equal (plist-get gp--detail-stats :files) 3))
      ;; the diff is split per file, keyed by path
      (should (equal (mapcar #'car gp--detail-diff) '("app.py" "docs/readme.md")))
      (should (string-match-p "\\+hello" (cdr (assoc "docs/readme.md" gp--detail-diff))))
      (should-not (string-match-p "hello" (cdr (assoc "app.py" gp--detail-diff)))))))

(ert-deftest gp-test-load-stats-diff-failure-keeps-previous-content ()
  "A failed fetch must not blank stats/diff that are already displayed.
The two fetches land independently, so a nil result means \"this one
failed\", not \"the PR has no stats\"."
  (let ((pr '((id . 7) (title . "t")
              (destination (repository (full_name . "acme/x")))
              (source (commit (hash . "abc123")))))
        (good-stats '(:commits 1 :files 2 :added 5 :removed 0))
        (good-diff '(("x" . "old diff")))
        (git-platform-current-backend (git-platform-bitbucket)))
    (with-temp-buffer
      (gp-detail-mode)
      (setq gp--pr pr gp--detail-stats good-stats gp--detail-diff good-diff)
      (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}"))
                ((symbol-function 'gp-pull-request-stats-async)
                 (lambda (_fn _id _pr cb) (funcall cb nil)))
                ((symbol-function 'gp-pull-request-diff-async)
                 (lambda (_fn _id _c _pr cb) (funcall cb nil))))
        (gp--detail-load-stats-diff (current-buffer) pr 0))
      (should (equal gp--detail-stats good-stats))
      (should (equal gp--detail-diff good-diff)))))

(ert-deftest gp-test-load-stats-diff-ignores-results-for-another-pr ()
  "A result arriving after the buffer moved on is dropped."
  (let ((git-platform-current-backend (git-platform-bitbucket))
        (pr '((id . 7) (title . "t")
              (destination (repository (full_name . "acme/x")))
              (source (commit (hash . "abc123")))))
        (other '((id . 99) (title . "other")
                 (destination (repository (full_name . "acme/x"))))))
    (with-temp-buffer
      (gp-detail-mode)
      ;; buffer is showing a DIFFERENT PR than the one being fetched
      (setq gp--pr other gp--detail-stats nil gp--detail-diff nil)
      (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}"))
                ((symbol-function 'gp-pull-request-stats-async)
                 (lambda (_fn _id _pr cb) (funcall cb '(:commits 1 :files 3 :added 0 :removed 0))))
                ((symbol-function 'gp-pull-request-diff-async)
                 (lambda (_fn _id _c _pr cb) (funcall cb gp-test--two-file-diff))))
        (gp--detail-load-stats-diff (current-buffer) pr 0))
      ;; both results were real, and both are dropped
      (should-not gp--detail-stats)
      (should-not gp--detail-diff))))

;;;; Async overview refresh --------------------------------------------------
;;
;; Regression coverage for a real bug: `gp-refresh' fetched the PR list
;; synchronously, so pressing `g' in the overview froze Emacs for the whole
;; round trip -- and the spinner only appeared once the fetch had already
;; returned.  These assert the ordering the fix depends on: the spinner is
;; painted BEFORE the fetch is dispatched, the old content survives the
;; in-flight window, and a slow earlier refresh cannot redraw over a newer one.

(defconst gp-test--list-fetch-uuid "{21d7839d-779f-44b2-8c40-6f43ac90be06}"
  "The fixture PRs' author uuid, so a rendered list partitions as \"mine\".")

(defmacro gp-test--with-stubbed-list-fetch (captured &rest body)
  "Run BODY with `gp-workspace-pull-requests-async' captured, not dispatched.
CAPTURED is a variable bound to a list the callback is pushed onto, so the
test controls exactly when, and whether, the fetch completes."
  (declare (indent 1))
  `(let ((,captured '()))
     (cl-letf (((symbol-function 'gp-workspace-pull-requests-async)
                (lambda (callback &optional _uuid &rest _)
                  (push callback ,captured)))
               ((symbol-function 'gp-user-uuid)
                (lambda () gp-test--list-fetch-uuid)))
       ,@body)))

(ert-deftest gp-test-refresh-shows-spinner-before-fetching ()
  "The ⏳ marker is on screen while the fetch is still in flight."
  (gp-test--with-stubbed-list-fetch calls
    (with-temp-buffer
      (gp-list-mode)
      (gp-refresh)
      ;; the fetch was dispatched but has NOT called back yet
      (should (= (length calls) 1))
      ;; a fresh (blank) buffer gets the inline loading line
      (should (string-match-p "⏳" (substring-no-properties (buffer-string)))))))

(ert-deftest gp-test-refresh-keeps-old-list-visible-while-loading ()
  "An in-flight refresh leaves the previously rendered PRs readable."
  (gp-test--with-stubbed-list-fetch calls
    (with-temp-buffer
      (gp-list-mode)
      (let ((inhibit-read-only t))
        (gp--render-list (gp-test--mock-prs) gp-test--list-fetch-uuid))
      (gp-refresh)
      ;; still the old content, plus a spinner overlay rather than a wipe
      (should (string-match-p "#239" (substring-no-properties (buffer-string))))
      (should gp--list-loading)
      (should (string-match-p "refreshing"
                              (overlay-get gp--list-loading 'after-string))))))

(ert-deftest gp-test-refresh-renders-and-clears-spinner-on-completion ()
  "When the fetch lands, the list is drawn and the spinner goes away."
  (gp-test--with-stubbed-list-fetch calls
    (with-temp-buffer
      (gp-list-mode)
      (gp-refresh)
      (funcall (car calls) t (gp-test--mock-prs))
      (should-not gp--list-loading)
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "My pull requests (10)" text))
        (should-not (string-match-p "⏳" text)))
      (should (= (length gp--prs) 10)))))

(ert-deftest gp-test-refresh-ignores-stale-callback ()
  "A slow earlier refresh must not redraw over a newer one's result."
  (gp-test--with-stubbed-list-fetch calls
    (with-temp-buffer
      (gp-list-mode)
      (gp-refresh)                      ; first (will finish LAST)
      (gp-refresh)                      ; second, supersedes it
      (should (= (length calls) 2))
      ;; newest callback lands first and renders the real list
      (funcall (car calls) t (gp-test--mock-prs))
      (should (= (length gp--prs) 10))
      ;; the stale one then reports an empty list and must be dropped
      (funcall (cadr calls) t '())
      (should (= (length gp--prs) 10))
      (should (string-match-p "My pull requests (10)"
                              (substring-no-properties (buffer-string)))))))

(ert-deftest gp-test-refresh-failure-keeps-previous-list ()
  "A failed fetch clears the spinner but does not blank the buffer."
  (gp-test--with-stubbed-list-fetch calls
    (with-temp-buffer
      (gp-list-mode)
      (let ((inhibit-read-only t))
        (gp--render-list (gp-test--mock-prs) gp-test--list-fetch-uuid))
      (gp-refresh)
      (funcall (car calls) nil nil)
      (should-not gp--list-loading)
      (should (string-match-p "#239" (substring-no-properties (buffer-string)))))))
;;;; Collapse state across a refresh ----------------------------------------

(defun gp--test-find-comment-section (id)
  "Return the `gp-comment-section' for comment ID in the current buffer."
  (let (found)
    (cl-labels ((walk (sec)
                  (when (and (not found)
                             (object-of-class-p sec 'gp-comment-section)
                             (equal (alist-get 'id (oref sec value)) id))
                    (setq found sec))
                  (when (slot-boundp sec 'children)
                    (mapc #'walk (oref sec children)))))
      (walk magit-root-section))
    found))

;; magit restores section visibility on redraw by matching sections between the
;; old and new tree via `magit-section-ident'.  Its default identity is the
;; section's whole `value', so a refetched comment whose `updated_on' (or body,
;; or reply count) changed no longer matched, and a section the user collapsed
;; came back expanded.

(ert-deftest gp-test-comment-section-ident-ignores-mutable-fields ()
  "A comment section is identified by its id, not its whole alist."
  (with-temp-buffer
    (gp-detail-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (gp-root)
        (magit-insert-section (gp-comment-section
                               '((id . 7) (updated_on . "T1")))
          (insert "body\n"))))
    (let ((sec (car (oref magit-root-section children))))
      ;; the id alone identifies it -- the timestamp is not part of the ident
      (should (equal (magit-section-ident-value sec) 7))
      (should (member '(gp-comment-section . 7) (magit-section-ident sec))))))

(ert-deftest gp-test-commit-section-ident-uses-hash ()
  "A commit section is identified by its hash, not its whole plist."
  (with-temp-buffer
    (gp-detail-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (gp-root)
        (magit-insert-section (gp-commit-section '(:hash "abc123" :summary "s"))
          (insert "commit\n"))))
    (let ((sec (car (oref magit-root-section children))))
      (should (equal (magit-section-ident-value sec) "abc123")))))

(ert-deftest gp-test-collapsed-comment-stays-collapsed-when-comment-changes ()
  "Redrawing a collapsed comment keeps it collapsed even if its fields changed.
This is the refresh case: the comment comes back from the API with a new
`updated_on', which used to defeat the visibility cache."
  (with-temp-buffer
    (gp-detail-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (gp-root)
        (magit-insert-section (gp-comment-section
                               '((id . 7) (updated_on . "T1")))
          (magit-insert-heading "comment 7")
          (insert "body\n"))))
    ;; user collapses it
    (magit-section-hide (car (oref magit-root-section children)))
    (should (oref (car (oref magit-root-section children)) hidden))
    (should (gp--test-body-invisible-p (car (oref magit-root-section children))))
    ;; refresh: same comment id, changed timestamp
    (let ((inhibit-read-only t))
      (erase-buffer)
      (magit-insert-section (gp-root)
        (magit-insert-section (gp-comment-section
                               '((id . 7) (updated_on . "T2")))
          (magit-insert-heading "comment 7")
          (insert "body\n"))))
    (gp--apply-section-visibility)
    (let ((sec (car (oref magit-root-section children))))
      (should (oref sec hidden))
      ;; and the body text is really hidden, not just the slot
      (should (gp--test-body-invisible-p sec)))))

(ert-deftest gp-test-expanded-comment-stays-expanded-across-refresh ()
  "The converse: an expanded comment is not spuriously collapsed on redraw."
  (with-temp-buffer
    (gp-detail-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (gp-root)
        (magit-insert-section (gp-comment-section '((id . 7) (updated_on . "T1")))
          (insert "body\n"))))
    (should-not (oref (car (oref magit-root-section children)) hidden))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (magit-insert-section (gp-root)
        (magit-insert-section (gp-comment-section '((id . 7) (updated_on . "T2")))
          (insert "body\n"))))
    (should-not (oref (car (oref magit-root-section children)) hidden))))

(ert-deftest gp-test-sibling-groups-have-distinct-idents ()
  "Each PR group is identified by its title, so collapse state is per-group."
  (with-temp-buffer
    (gp-list-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (gp-root)
        (gp--insert-group "Needs my review" nil)
        (gp--insert-group "My pull requests" nil)))
    (let* ((kids (oref magit-root-section children))
           (a (nth 0 kids))
           (b (nth 1 kids)))
      (should-not (equal (magit-section-ident a) (magit-section-ident b)))
      ;; collapsing one leaves the other alone
      (magit-section-hide a)
      (should (oref a hidden))
      (should-not (oref b hidden)))))

(ert-deftest gp-test-resolved-collapse-only-forced-on-first-render ()
  "A resolved thread the user expanded stays expanded on refresh.
Resolved threads start collapsed, but after the first render the user's
own state wins -- `gp--collapse-resolved-sections' must not re-hide it."
  (bitbucket-mock-with-service
    (let ((resolved '((id . 7) (updated_on . "T1")
                      (resolution . ((type . "resolved"))))))
      (with-temp-buffer
        (gp-detail-mode)
        (should (gp-comment-resolved-p resolved))
        ;; first render: forced collapsed
        (gp--render-detail-into (current-buffer) '((id . 1) (title . "t"))
                                (list resolved) nil)
        (let ((sec (gp--test-find-comment-section 7)))
          (should sec)
          (should (oref sec hidden))
          ;; user expands it deliberately
          (magit-section-show sec)
          (should-not (oref sec hidden)))
        ;; refresh must not force it shut again
        (gp--render-detail-into (current-buffer) '((id . 1) (title . "t"))
                                (list resolved) nil)
        (let ((sec (gp--test-find-comment-section 7)))
          (should sec)
          (should-not (oref sec hidden)))))))

(defun gp--test-body-invisible-p (section)
  "Return non-nil if SECTION's body is actually hidden in the buffer.
Checks for the `invisible' overlay, not just the `hidden' slot -- the two
came apart on refresh, which is what made collapsed sections look
re-expanded."
  (and (oref section content)
       (cl-some (lambda (o) (overlay-get o 'invisible))
                (overlays-in (oref section content) (oref section end)))))

(ert-deftest gp-test-collapsed-section-stays-hidden-in-buffer-after-refresh ()
  "A collapsed section is still visually hidden after a redraw.
`magit-section-hide' hides a body with an `evaporate' overlay, so the
`erase-buffer' in `gp--render-detail-into' destroys it.  The rebuilt
section keeps its `hidden' slot but used to lose the overlay, so the
body rendered in full while the model said collapsed."
  (bitbucket-mock-with-service
    ;; a description makes `gp--insert-description' emit a real section
    (let ((pr '((id . 1) (title . "t")
                (description . "a multi-line\nPR description")))
          (comments nil))
      (with-temp-buffer
        (gp-detail-mode)
        (gp--render-detail-into (current-buffer) pr comments nil)
        (let ((sec (cl-find 'gp-description (oref magit-root-section children)
                            :key (lambda (s) (oref s type)))))
          (should sec)
          ;; user collapses it -- slot and overlay agree
          (magit-section-hide sec)
          (should (oref sec hidden))
          (should (gp--test-body-invisible-p sec)))
        ;; refresh
        (gp--render-detail-into (current-buffer) pr comments nil)
        (let ((sec (cl-find 'gp-description (oref magit-root-section children)
                            :key (lambda (s) (oref s type)))))
          (should sec)
          (should (oref sec hidden))
          ;; the regression: slot said hidden but the text was visible
          (should (gp--test-body-invisible-p sec)))))))

(ert-deftest gp-test-apply-section-visibility-rebuilds-overlays ()
  "`gp--apply-section-visibility' re-hides every section whose slot says so."
  (with-temp-buffer
    (gp-detail-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (gp-root)
        (magit-insert-section (gp-comment-section '((id . 7)))
          (magit-insert-heading "head")
          (insert "body\n"))))
    (let ((sec (car (oref magit-root-section children))))
      (magit-section-hide sec)
      (should (gp--test-body-invisible-p sec))
      ;; simulate what erase-buffer does to `evaporate' overlays
      (remove-overlays (point-min) (point-max) 'invisible t)
      (should-not (gp--test-body-invisible-p sec))
      ;; the slot still says hidden, so reapplying must restore the overlay
      (should (oref sec hidden))
      (gp--apply-section-visibility)
      (should (oref sec hidden))
      (should (gp--test-body-invisible-p sec)))))

(ert-deftest gp-test-apply-section-visibility-leaves-expanded-sections-alone ()
  "Reapplying visibility does not hide sections the user has expanded."
  (with-temp-buffer
    (gp-detail-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (gp-root)
        (magit-insert-section (gp-comment-section '((id . 7)))
          (magit-insert-heading "head")
          (insert "body\n"))))
    (let ((sec (car (oref magit-root-section children))))
      (should-not (oref sec hidden))
      (gp--apply-section-visibility)
      (should-not (oref sec hidden))
      (should-not (gp--test-body-invisible-p sec)))))

(provide 'gp-ui-test)
;;; gp-ui-test.el ends here
