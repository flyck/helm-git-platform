;;; gp-ui-test.el --- Tests for the PR list/detail UI -*- lexical-binding: t; -*-

;;; Commentary:
;; Drives the magit-section renderers against mock data in a real
;; (batch) buffer and asserts on the produced text and section tree --
;; this is the "simulate UI" coverage without a live display.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gp-ui)
(require 'bitbucket-mock)

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
  "The overview section lists reviewers and their approval state."
  (let* ((base (car (gp-test--mock-prs)))
         (pr (append
              `((participants
                 . (((role . "REVIEWER") (state . "approved") (approved . t)
                     (user (display_name . "Alice")))
                    ((role . "REVIEWER") (state . "changes_requested")
                     (user (display_name . "Bob")))
                    ((role . "REVIEWER") (state . nil)
                     (user (display_name . "Carol")))
                    ((role . "PARTICIPANT") (state . "approved") (approved . t)
                     (user (display_name . "NotAReviewer"))))))
              base)))
    (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}")))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr nil))
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "✅ Alice" text))
          (should (string-match-p "❌ Bob" text))
          (should (string-match-p "⏳ Carol" text))
          (should-not (string-match-p "NotAReviewer" text)))))))

(ert-deftest gp-test-render-detail-no-reviewers-no-line ()
  "When PR has no participants, no reviewers line is inserted."
  (let ((pr (car (gp-test--mock-prs))))
    (cl-letf (((symbol-function 'gp-user-uuid) (lambda () "{me}")))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (gp--render-detail pr nil))
        (should-not (string-match-p "👥" (buffer-string)))))))

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
  "Poll while running; watch when a visible buffer has no current run."
  (let ((gp-detail-pipeline-poll-interval 6)
        (gp-detail-pipeline-watch-interval 1)
        (running '(:current ((((state (name . "IN_PROGRESS")))) ) :recent (r)))
        (finished '(:current ((((state (name . "COMPLETED")))) ) :recent (r)))
        (waiting '(:current nil :recent (r)))
        (no-history '(:current nil :recent nil)))
    (should (eq (gp--detail-pipeline-poll-mode running t) 'poll))
    (should (eq (gp--detail-pipeline-poll-mode running nil) 'poll))
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

(provide 'gp-ui-test)
;;; gp-ui-test.el ends here
