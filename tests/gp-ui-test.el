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

(ert-deftest gp-test-render-list-builds-groups ()
  "Rendering produces both group headings and one section per PR."
  (let ((prs (gp-test--mock-prs))
        (uuid "{21d7839d-779f-44b2-8c40-6f43ac90be06}"))
    (with-temp-buffer
      (gp-list-mode)
      (let ((inhibit-read-only t))
        (gp--render-list prs uuid))
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "Needs my review (0)" text))
        (should (string-match-p "My pull requests (10)" text))
        ;; a known PR id from the fixture appears
        (should (string-match-p "#239" text)))
      ;; section tree: root has children, and PR sections carry their pr
      (let* ((root magit-root-section)
             (pr-secs (cl-remove-if-not
                       (lambda (s) (object-of-class-p s 'gp-pr-section))
                       (gp-test--all-sections root))))
        (should (= (length pr-secs) 10))
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

(defun gp-test--github-backend-pr ()
  "Return (git-platform-github . github-mock--pr-1), for use inside
`github-mock-with-service'."
  (cons (git-platform-github) github-mock--pr-1))

(ert-deftest gp-test-pr-heading-shows-real-author-and-repo-slug-for-github-pr ()
  (github-mock-with-service
    (let* ((git-platform-current-backend (git-platform-github))
           (pr github-mock--pr-1)
           (h (substring-no-properties (gp--pr-heading pr))))
      (should (string-match-p "ada" h))
      (should (string-match-p (regexp-quote "[web]") h)))))

(ert-deftest gp-test-insert-pr-shows-real-branches-for-github-pr ()
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (pr github-mock--pr-1))
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
          (pr github-mock--pr-1))
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

(ert-deftest gp-test-render-detail-shows-mark-ready-for-open-github-pr-authored-by-me ()
  "The draft-toggle button must key off `gp-pr-open-p', not a raw
\"OPEN\" string comparison -- GitHub's `state' is lowercase \"open\"."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (pr (append '((draft . t)) github-mock--pr-1)))
      (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "ada")))
        (with-temp-buffer
          (gp-detail-mode)
          (let ((inhibit-read-only t))
            (gp--render-detail pr nil))
          (should (string-match-p "Mark ready \\[D\\]"
                                  (substring-no-properties (buffer-string)))))))))

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
    (should (string-match-p "Ada Lovelace" text))))

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
  "RET on a commit section opens that commit, not the section fold."
  (let ((called nil))
    (cl-letf (((symbol-function 'magit-current-section)
               (lambda () (let ((s (gp-commit-section)))
                            (oset s value (car gp-test--commits)) s)))
              ((symbol-function 'gp-detail-show-commit)
               (lambda () (interactive) (setq called t)))
              ((symbol-function 'magit-section-toggle)
               (lambda (&rest _) (interactive) (setq called 'toggled))))
      (gp-detail-ret)
      (should (eq called t)))))

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

(ert-deftest gp-test-detail-show-commit-is-bound-to-a-lowercase-key ()
  "Read-only actions stay lowercase; capitals are reserved for writes."
  (should (eq (lookup-key gp-detail-mode-map "v") #'gp-detail-show-commit))
  ;; the write keys keep their meaning
  (should (eq (lookup-key gp-detail-mode-map "R") #'gp-detail-reply))
  (should (eq (lookup-key gp-detail-mode-map "K") #'gp-detail-delete)))

(ert-deftest gp-test-detail-run-action-shows-spinner-during-thunk ()
  "While `gp--detail-run-action's THUNK is running, the buffer already
shows the spinner in that button's slot (not just after it returns)."
  (with-temp-buffer
    (gp-detail-mode)
    (let (seen-during-thunk)
      (gp--detail-run-action
       (current-buffer) 'draft
       (lambda ()
         (setq seen-during-thunk gp--detail-pending-action)))
      (should (eq seen-during-thunk 'draft))
      ;; cleared once the thunk returns
      (should-not gp--detail-pending-action))))

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
  "GitHub keeps the body in `body', and sends :null when empty."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github)))
      (should (equal (gp-pr-description github-mock--pr-1)
                     "Adds the toggle.\n\n- [x] tests\n- [ ] docs"))
      (should-not (gp-pr-description '((id . 42) (body . ""))))
      ;; a JSON null arrives as a non-string, not as nil
      (should-not (gp-pr-description '((id . 42) (body . :null))))
      (should-not (gp-pr-description '((id . 42)))))))

(ert-deftest gp-test-render-detail-shows-description-for-github-pr ()
  "The detail view renders the description section from GitHub's `body'."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github)))
      (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "someone-else")))
        (with-temp-buffer
          (gp-detail-mode)
          (let ((inhibit-read-only t))
            (gp--render-detail github-mock--pr-1 nil))
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
  (let ((pr (car (gp-test--mock-prs)))
        (comments (alist-get 'values (bitbucket-mock--fixture "pr-comments.json"))))
    (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}")))
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
        (gp-comment-delete-others '(bitbucket)))
    (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{nobody}"))
              ((symbol-function 'gp-backend-name) (lambda () 'bitbucket)))
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
    (cl-letf (((symbol-function 'gp-backend-name) (lambda () 'github)))
      (let ((gp-comment-delete-others '(bitbucket)))
        (should-not (gp-comment-deletable-p comment "{me}")))
      (let ((gp-comment-delete-others '(github)))
        (should (gp-comment-deletable-p comment "{me}")))
      (let ((gp-comment-delete-others t))
        (should (gp-comment-deletable-p comment "{me}")))
      ;; your own comment stays deletable regardless of the setting
      (let ((gp-comment-delete-others nil))
        (should (gp-comment-deletable-p '((id . 1) (user (uuid . "{me}"))) "{me}"))))))

(ert-deftest gp-test-detail-mode-map-mutating-actions-are-capitalised ()
  "Write actions on comments sit on capitals; the lowercase keys are free."
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
  "A merged/closed PR cannot be edited, so no line and no button."
  (let ((pr (cons '(state . "MERGED")
                  (assq-delete-all 'state (copy-alist (car (gp-test--mock-prs)))))))
    (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}")))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr nil))
        (should-not (string-match-p "edit \\[V\\]" (buffer-string)))))))

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

(ert-deftest gp-test-detail-delete-own-comment ()
  "`gp-detail-delete' deletes an own comment at point (confirmed)."
  (let* ((uuid "{me}")
         (pr '((id . 7) (destination (repository (full_name . "acme/x")))))
         (own '((id . 55) (user (uuid . "{me}"))))
         (gp--pr pr)
         (deleted nil))
    (cl-letf (((symbol-function 'magit-current-section)
               (lambda () (let ((s (gp-comment-section))) (oset s value own) s)))
              ((symbol-function 'gp-user-uuid) (lambda () uuid))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'gp-detail-refresh) #'ignore)
              ((symbol-function 'gp-delete-comment)
               (lambda (fn id cid) (setq deleted (list fn id cid)))))
      (gp-detail-delete)
      (should (equal deleted '("acme/x" 7 55))))))

(ert-deftest gp-test-detail-delete-rejects-foreign-comment ()
  "Deleting someone else's comment signals and does not call the API."
  (let* ((pr '((id . 7) (destination (repository (full_name . "acme/x")))))
         (other '((id . 55) (user (uuid . "{someone-else}"))))
         (gp--pr pr)
         (called nil))
    (cl-letf (((symbol-function 'magit-current-section)
               (lambda () (let ((s (gp-comment-section))) (oset s value other) s)))
              ((symbol-function 'gp-user-uuid) (lambda () "{me}"))
              ((symbol-function 'gp-delete-comment)
               (lambda (&rest _) (setq called t))))
      (should-error (gp-detail-delete) :type 'user-error)
      (should-not called))))

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

(ert-deftest gp-test-detail-open-local-opens-magit-without-checkout ()
  (let* ((pr '((id . 7) (destination (repository (full_name . "acme/x")))))
         (gp--pr pr)
         (opened nil))
    (cl-letf (((symbol-function 'require) (lambda (feature &optional _filename _noerror) (eq feature 'magit)))
              ((symbol-function 'gp-local-find-checkout) (lambda (_full-name) "/tmp/repo"))
              ((symbol-function 'magit-status) (lambda (dir) (setq opened dir))))
      (gp-detail-open-local)
      (should (equal opened "/tmp/repo")))))

(ert-deftest gp-test-render-detail-shows-mark-action-for-comments ()
  (let ((pr (car (gp-test--mock-prs)))
        (comments (alist-get 'values (bitbucket-mock--fixture "pr-comments.json"))))
    (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}")))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr comments))
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "mark \\[m\\]" text)))))))

(ert-deftest gp-test-detail-browse-opens-comment-url-at-comment-point ()
  (let ((comment '((links (html (href . "https://example.test/comment/1")))))
        (opened nil))
    (cl-letf (((symbol-function 'magit-current-section)
               (lambda () (let ((s (gp-comment-section))) (oset s value comment) s)))
              ((symbol-function 'browse-url)
               (lambda (url &rest _) (setq opened url))))
      (gp-detail-browse)
      (should (equal opened "https://example.test/comment/1")))))

(ert-deftest gp-test-detail-refresh-uses-async-path-with-cached-content ()
  (let* ((pr '((id . 7) (title . "Old")
               (destination (repository (full_name . "acme/x")))))
         (fresh-pr '((id . 7) (title . "New")
                     (destination (repository (full_name . "acme/x")))))
         (comments '(((id . 1) (content (raw . "new")))))
         (show-pr-called nil))
    (with-temp-buffer
      (gp-detail-mode)
      (setq gp--pr pr
            gp--detail-comments '(((id . 0) (content (raw . "old"))))
            gp--detail-stats '(:commits 1 :files 1 :added 0 :removed 0)
            gp--detail-diff '(("a" . "diff"))
            gp--detail-pipelines '(:recent nil))
      (cl-letf (((symbol-function 'gp-show-pr)
                 (lambda (&rest _) (setq show-pr-called t)))
                ((symbol-function 'gp-user-uuid) (lambda () "{me}"))
                ((symbol-function 'bitbucket-pull-request-async)
                 (lambda (_full-name _id callback)
                   (funcall callback t fresh-pr)))
                ((symbol-function 'bitbucket-pull-request-comments-async)
                 (lambda (_full-name _id callback &optional _max-items)
                   (funcall callback t comments)))
                ;; the deferred stats/diff fetch is async now, so it runs
                ;; within this test rather than on a timer that never fires
                ((symbol-function 'gp-pull-request-stats-async)
                 (lambda (_fn _id _pr cb) (funcall cb nil)))
                ((symbol-function 'gp-pull-request-diff-async)
                 (lambda (_fn _id _c _pr cb) (funcall cb nil))))
        (gp-detail-refresh)
        (should-not show-pr-called)
        (should (equal gp--pr fresh-pr))
        (should (equal gp--detail-comments comments))
        (should (equal gp--detail-stats '(:commits 1 :files 1 :added 0 :removed 0)))
        (should (equal gp--detail-diff '(("a" . "diff"))))))))

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
           (h (substring-no-properties (gp--pr-heading github-mock--pr-1))))
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
    (let ((git-platform-current-backend (git-platform-github)))
      (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "ada")))
        (with-temp-buffer
          (gp-detail-mode)
          (let ((inhibit-read-only t))
            (gp--render-detail github-mock--pr-1 nil))
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "🏷" text))
            (should (string-match-p "bug" text))
            (should (string-match-p "edit \\[L\\]" text))))))))

(ert-deftest gp-test-labels-line-offers-edit-with-none-yet ()
  "With no labels on an open PR the line still appears, so the first one
can be added -- same reasoning as the reviewers line."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github)))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--insert-labels-line github-mock--pr-2))
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "no labels" text))
          (should (string-match-p "edit \\[L\\]" text)))))))

(ert-deftest gp-test-labels-line-has-no-edit-button-on-closed-pr ()
  "A merged/closed PR shows its labels read-only."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (pr (append '((state . "closed")) github-mock--pr-1)))
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
          (pr (append '((body . "Some body text")) github-mock--pr-1))
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
          (pr (append '((body . "")) github-mock--pr-1)))
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
                      github-mock--pr-1)))
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
          (pr (append '((state . "closed") (body . "")) github-mock--pr-1)))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--insert-description pr))
        (should (equal (buffer-string) ""))))))

(ert-deftest gp-test-edit-description-refreshes-the-detail-buffer ()
  "Saving a description redraws the DETAIL buffer, not the compose one.
`:on-success' runs while the compose buffer is current, where `gp--pr'
is nil and a bare `gp-detail-refresh' silently does nothing -- the edit
reaches the server but the view keeps showing the old text."
  (let* ((pr (append '((description . "old body")) (car (gp-test--mock-prs))))
         (detail-buf (gp--detail-buffer-name pr))
         (refreshed-in nil)
         (sent nil))
    (unwind-protect
        (progn
          ;; a stand-in for the real detail buffer, so `buffer-live-p' holds
          (get-buffer-create detail-buf)
          (cl-letf (((symbol-function 'gp-set-pull-request-description)
                     (lambda (_fn _id text &optional _title)
                       (setq sent text)
                       (append `((description . ,text)) pr)))
                    ((symbol-function 'gp-detail-refresh)
                     (lambda () (setq refreshed-in (buffer-name))))
                    ((symbol-function 'gp-invalidate-pr-caches) #'ignore))
            (let ((compose (gp-ui-edit-description pr)))
              (unwind-protect
                  (with-current-buffer compose
                    (erase-buffer)
                    (insert "new body")
                    (gp-compose-submit))
                (when (buffer-live-p compose) (kill-buffer compose)))))
          (should (equal sent "new body"))
          ;; the redraw must have happened inside the detail buffer
          (should (equal refreshed-in detail-buf)))
      (when (get-buffer detail-buf) (kill-buffer detail-buf)))))

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

(ert-deftest gp-test-reaction-summary-groups-and-marks-mine ()
  "Rows collapse to (CONTENT COUNT MINE-P), keeping first-seen order."
  (let ((rows '(((content . "+1")   (user (uuid . "bea")))
                ((content . "heart")(user (uuid . "ada")))
                ((content . "+1")   (user (uuid . "ada")))
                ((content . "+1")   (user (uuid . "cy"))))))
    (should (equal (gp--reaction-summary rows "ada")
                   '(("+1" 3 t ("bea" "ada" "cy")) ("heart" 1 t ("ada")))))
    ;; a user with none of their own gets the same counts, all unmarked
    (should (equal (gp--reaction-summary rows "zed")
                   '(("+1" 3 nil ("bea" "ada" "cy")) ("heart" 1 nil ("ada")))))))

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

(ert-deftest gp-test-reaction-summary-falls-back-when-no-display-name ()
  "A reactor with only an id still gets named rather than dropped."
  (should (equal (gp--reaction-summary
                  '(((content . "+1") (user (uuid . "bea"))))
                  "ada")
                 '(("+1" 1 nil ("bea"))))))

(ert-deftest gp-test-reaction-summary-empty-is-nil ()
  "No reactions summarises to nil, so the renderer inserts nothing."
  (should (null (gp--reaction-summary nil "ada"))))

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
  "GitHub reports its eight reaction tokens."
  (let ((git-platform-current-backend (git-platform-github)))
    (should (gp-reactions-supported-p))
    (should (equal (gp-reaction-choices)
                   '("+1" "-1" "laugh" "confused" "heart" "hooray" "rocket" "eyes")))))

(ert-deftest gp-test-no-reaction-buttons-rendered-on-bitbucket ()
  "A platform without reactions renders no reaction affordance at all."
  (let* ((git-platform-current-backend (git-platform-bitbucket))
         (pr (car (gp-test--mock-prs)))
         (comments (alist-get 'values (bitbucket-mock--fixture "pr-comments.json"))))
    (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}")))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr comments))
        (let ((text (substring-no-properties (buffer-string))))
          (should-not (string-match-p "react \\[!\\]" text))
          (should-not (string-match-p "👍" text)))))))

(ert-deftest gp-test-reaction-buttons-rendered-when-supported ()
  "Existing reactions render as buttons, counts in brackets.
The counts come from the comment's own `reaction-counts' -- see
`gp-test-reaction-render-never-fetches'."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github)))
      (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "ada")))
        (with-temp-buffer
          (gp-detail-mode)
          (let ((inhibit-read-only t))
            (gp--insert-reactions
             '((id . 42) (destination (repository (full_name . "acme/web"))))
             '((id . 7) (reaction-counts . (("+1" . 2) ("rocket" . 1))))))
          (let ((text (substring-no-properties (buffer-string))))
            (should (string-match-p "👍 (2)" text))
            (should (string-match-p "🚀 (1)" text)))
          ;; real buttons, one per distinct reaction, in the given order.
          ;; Counted via overlays: `insert-button' stores its properties
          ;; there, and walking `next-button' past the last one signals.
          (should (equal (gp-test--button-labels) '("👍 (2)" "🚀 (1)"))))))))

(ert-deftest gp-test-reaction-pill-is-just-emoji-and-count ()
  "The pill carries no ownership glyph.
A tick there reads as \"resolved\"/\"done\" rather than \"me\"; the quick
action's [+]/[-] label and the tooltip carry that instead."
  (cl-letf (((symbol-function 'gp-reactions-supported-p) (lambda () t))
            ((symbol-function 'gp-user-uuid) (lambda () "ada")))
    (with-temp-buffer
      (gp-detail-mode)
      (let ((inhibit-read-only t))
        (gp--insert-reactions
         '((id . 42))
         '((id . 7) (reaction-counts . (("+1" . 2) ("rocket" . 1)))
                    (reaction-mine . ("+1")))))
      (let ((txt (substring-no-properties (buffer-string))))
        (should (string-match-p "👍 (2)" txt))
        (should (string-match-p "🚀 (1)" txt))
        ;; yours is not decorated -- same shape either way
        (should-not (string-match-p "✓" txt))))))

(ert-deftest gp-test-like-button-label-always-names-the-plus-key ()
  "The quick action stays [+] in both directions.
The bracket names the KEY, and `+' is what toggles either way -- `-' is
`negative-argument' globally, so labelling it [-] promised a key that
does nothing.  Which way the toggle will go is in the help text."
  (cl-letf (((symbol-function 'gp-reactions-supported-p) (lambda () t))
            ((symbol-function 'gp-user-uuid) (lambda () "ada"))
            ((symbol-function 'gp-comment-resolvable-p) (lambda (_) nil))
            ((symbol-function 'gp-comment-own-p) (lambda (&rest _) nil))
            ((symbol-function 'gp-comment-deletable-p) (lambda (&rest _) nil)))
    (dolist (mine '(("+1") nil))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--insert-comment
           `((id . 7) (content (raw . "hi")) (user (uuid . "bea"))
             (reaction-counts . (("+1" . 1))) (reaction-mine . ,mine))
           '((id . 42)) 0 nil))
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
          (should (string-match-p (if mine "Remove" "Add") help)))))))

(ert-deftest gp-test-action-row-indent-matches-with-and-without-reactions ()
  "The action row sits at the same indent either way.
The reactions line used to have its indent inserted by the caller, which
left that whitespace on the action row when a comment had no reactions --
so removing your last reaction visibly shifted the row."
  (cl-letf (((symbol-function 'gp-reactions-supported-p) (lambda () t))
            ((symbol-function 'gp-user-uuid) (lambda () "ada"))
            ((symbol-function 'gp-comment-resolvable-p) (lambda (_) nil))
            ((symbol-function 'gp-comment-own-p) (lambda (&rest _) nil))
            ((symbol-function 'gp-comment-deletable-p) (lambda (&rest _) nil)))
    (cl-labels
        ((action-indent (comment)
           (with-temp-buffer
             (gp-detail-mode)
             (let ((inhibit-read-only t))
               (gp--insert-comment comment '((id . 42)) 0 nil))
             (let ((line (seq-find (lambda (l) (string-match-p "reply \\[R\\]" l))
                                   (split-string (substring-no-properties
                                                  (buffer-string))
                                                 "\n"))))
               (should line)
               (- (length line) (length (string-trim-left line)))))))
      (let ((with (action-indent '((id . 7) (content (raw . "hi"))
                                   (user (uuid . "bea"))
                                   (reaction-counts . (("+1" . 1))))))
            (without (action-indent '((id . 7) (content (raw . "hi"))
                                      (user (uuid . "bea"))))))
        (should (= with without))))))

(ert-deftest gp-test-reaction-render-never-fetches ()
  "Drawing reactions must not hit the network.
It did once: the renderer called `gp-comment-reactions' per comment, and
a fetch mid-redisplay re-entered the renderer and drew each comment
several times over (a single comment showed four action rows).  Counts
ride along on the comment instead."
  (let ((fetches 0))
    (cl-letf (((symbol-function 'gp-reactions-supported-p) (lambda () t))
              ((symbol-function 'gp-user-uuid) (lambda () "ada"))
              ((symbol-function 'gp-comment-reactions)
               (lambda (&rest _) (cl-incf fetches) nil)))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--insert-reactions
           '((id . 42))
           '((id . 7) (reaction-counts . (("+1" . 3))))))
        (should (string-match-p "👍 (3)" (substring-no-properties (buffer-string))))))
    (should (= fetches 0))))

(ert-deftest gp-test-reaction-help-echo-is-lazy-and-names-reactors ()
  "The tooltip fetches only when shown, and then names who reacted.
Also decides `mine' from those rows: the comment payload carries counts
but not reactors, so a button cannot know whose reaction it is."
  (let ((fetches 0))
    (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "ada"))
              ((symbol-function 'gp-pr-full-name) (lambda (_) "acme/web"))
              ((symbol-function 'gp-comment-reactions)
               (lambda (&rest _)
                 (cl-incf fetches)
                 '(((content . "+1") (user (uuid . "ada") (display_name . "Ada")))
                   ((content . "+1") (user (uuid . "bea") (display_name . "Bea")))))))
      (let ((fn (gp--reaction-help-echo '((id . 42)) '((id . 7)) "+1" 2 nil)))
        ;; building it fetches nothing
        (should (= fetches 0))
        (let ((tip (funcall fn)))
          (should (= fetches 1))
          (should (string-match-p "Ada, Bea" tip))
          ;; "ada" is the current user, so it knows the click removes
          (should (string-match-p "remove yours" tip)))))))

(ert-deftest gp-test-toggle-reaction-adds-then-removes ()
  "The same entry point adds when absent and removes when present,
which is what makes a rendered reaction a toggle."
  (let* ((pr '((id . 42) (destination (repository (full_name . "acme/web")))))
         (comment '((id . 7)))
         (held nil)
         (calls nil))
    (cl-letf (((symbol-function 'gp-reactions-supported-p) (lambda () t))
              ((symbol-function 'gp-user-uuid) (lambda () "ada"))
              ((symbol-function 'gp-comment-reactions)
               (lambda (_fn _c) (when held '(((content . "+1") (user (uuid . "ada")))))))
              ((symbol-function 'gp-set-comment-reaction)
               (lambda (_fn _c content on) (push (cons content on) calls) (setq held on) t))
              ((symbol-function 'gp-invalidate-pr-caches) #'ignore)
              ((symbol-function 'gp-detail-refresh) #'ignore))
      (gp-ui-toggle-reaction pr comment "+1")      ;; none held -> add
      (gp-ui-toggle-reaction pr comment "+1"))     ;; now held -> remove
    (should (equal (reverse calls) '(("+1" . t) ("+1" . nil))))))

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

(ert-deftest gp-test-edit-labels-sends-the-chosen-set ()
  "Editing labels PUTs the complete chosen set and refreshes the buffer."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          sent refreshed)
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (&rest _) '("bug" "chore")))
                ((symbol-function 'gp-set-pull-request-labels)
                 (lambda (fn id labels) (setq sent (list fn id labels)) t))
                ((symbol-function 'gp-invalidate-pr-caches) (lambda (&rest _) nil))
                ((symbol-function 'gp-detail-refresh)
                 (lambda (&rest _) (setq refreshed t))))
        ;; a PR as callers actually hold it: `github--reshape-pr' has already
        ;; put the PR *number* in `id', which is what the endpoints take
        (gp-ui-edit-labels (github-pull-request "acme/web" 42))
        (should (equal sent '("acme/web" 42 ("bug" "chore"))))
        (should refreshed)))))

(ert-deftest gp-test-edit-labels-no-change-skips-the-write ()
  "Leaving the pre-filled set untouched must not PUT or refresh --
a no-op edit should not churn the PR or invalidate caches."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (wrote nil) (refreshed nil))
      (cl-letf (((symbol-function 'completing-read-multiple)
                 ;; same set the PR already has, in the other order
                 (lambda (&rest _) '("ui" "bug")))
                ((symbol-function 'gp-set-pull-request-labels)
                 (lambda (&rest _) (setq wrote t)))
                ((symbol-function 'gp-detail-refresh)
                 (lambda (&rest _) (setq refreshed t))))
        (gp-ui-edit-labels github-mock--pr-1)
        (should-not wrote)
        (should-not refreshed)))))

(ert-deftest gp-test-edit-labels-rejects-a-name-outside-the-pool ()
  "An unknown name is refused rather than silently creating a new label
on the repo, which is what GitHub's endpoint would otherwise do."
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github))
          (wrote nil))
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (&rest _) '("typoo")))
                ((symbol-function 'gp-set-pull-request-labels)
                 (lambda (&rest _) (setq wrote t))))
        (should-error (gp-ui-edit-labels github-mock--pr-1) :type 'user-error)
        (should-not wrote)))))

(ert-deftest gp-test-edit-labels-refuses-on-bitbucket-and-closed-prs ()
  "The command reports rather than acting where labels cannot apply."
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (should-error (gp-ui-edit-labels (car (gp-test--mock-prs))) :type 'user-error))
  (github-mock-with-service
    (let ((git-platform-current-backend (git-platform-github)))
      (should-error (gp-ui-edit-labels (append '((state . "closed"))
                                               github-mock--pr-1))
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

(ert-deftest gp-test-load-stats-diff-uses-async-ops ()
  "The loader must reach for the async ops, never the blocking twins."
  (let ((async-calls nil)
        (sync-calls nil)
        (pr '((id . 7) (title . "t")
              (destination (repository (full_name . "acme/x")))
              (source (commit (hash . "abc123"))))))
    (with-temp-buffer
      (gp-detail-mode)
      (setq gp--pr pr)
      (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}"))
                ((symbol-function 'gp-pull-request-stats-async)
                 (lambda (_fn _id _pr cb) (push 'stats-async async-calls)
                   (funcall cb '(:commits 1 :files 1 :added 0 :removed 0))))
                ((symbol-function 'gp-pull-request-diff-async)
                 (lambda (_fn _id _c _pr cb) (push 'diff-async async-calls) (funcall cb "diff")))
                ((symbol-function 'gp-pull-request-stats)
                 (lambda (&rest _) (push 'stats-sync sync-calls) nil))
                ((symbol-function 'gp-pull-request-diff)
                 (lambda (&rest _) (push 'diff-sync sync-calls) nil)))
        (gp--detail-load-stats-diff (current-buffer) pr 0))
      (should (memq 'stats-async async-calls))
      (should (memq 'diff-async async-calls))
      ;; the whole point: no blocking call
      (should-not sync-calls))))

(ert-deftest gp-test-load-stats-diff-folds-in-each-result ()
  "Stats and diff land independently, each rendering what has arrived."
  (let ((pr '((id . 7) (title . "t")
              (destination (repository (full_name . "acme/x")))
              (source (commit (hash . "abc123"))))))
    (with-temp-buffer
      (gp-detail-mode)
      (setq gp--pr pr)
      (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}"))
                ((symbol-function 'gp-pull-request-stats-async)
                 (lambda (_fn _id _pr cb) (funcall cb '(:commits 2 :files 3 :added 9 :removed 1))))
                ((symbol-function 'gp-pull-request-diff-async)
                 (lambda (_fn _id _c _pr cb) (funcall cb "diff --git a/x b/x")))
                ((symbol-function 'gp-split-diff-by-file)
                 (lambda (d) (list (cons "x" d)))))
        (gp--detail-load-stats-diff (current-buffer) pr 0))
      (should (equal (plist-get gp--detail-stats :files) 3))
      (should (equal gp--detail-diff '(("x" . "diff --git a/x b/x")))))))

(ert-deftest gp-test-load-stats-diff-failure-keeps-previous-content ()
  "A failed fetch must not blank stats/diff that are already displayed.
The two fetches land independently, so a nil result means \"this one
failed\", not \"the PR has no stats\"."
  (let ((pr '((id . 7) (title . "t")
              (destination (repository (full_name . "acme/x")))
              (source (commit (hash . "abc123")))))
        (good-stats '(:commits 1 :files 2 :added 5 :removed 0))
        (good-diff '(("x" . "old diff"))))
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
  (let ((pr '((id . 7) (title . "t")
              (destination (repository (full_name . "acme/x")))
              (source (commit (hash . "abc123")))))
        (other '((id . 99) (title . "other")
                 (destination (repository (full_name . "acme/x"))))))
    (with-temp-buffer
      (gp-detail-mode)
      ;; buffer is showing a DIFFERENT PR than the one being fetched
      (setq gp--pr other gp--detail-stats nil)
      (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}"))
                ((symbol-function 'gp-pull-request-stats-async)
                 (lambda (_fn _id _pr cb) (funcall cb '(:commits 1 :files 3 :added 0 :removed 0))))
                ((symbol-function 'gp-pull-request-diff-async)
                 (lambda (_fn _id _c _pr cb) (funcall cb nil))))
        (gp--detail-load-stats-diff (current-buffer) pr 0))
      (should-not gp--detail-stats))))

(provide 'gp-ui-test)
;;; gp-ui-test.el ends here
