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
        (let ((text (substring-no-properties (buffer-string))))
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
        (let ((text (substring-no-properties (buffer-string))))
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
                   (funcall callback t comments))))
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

(provide 'gp-ui-test)
;;; gp-ui-test.el ends here
