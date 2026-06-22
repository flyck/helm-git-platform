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
         (called nil))
    (cl-letf (((symbol-function 'gp-ui-send-comments-to-terminal)
               (lambda (seen-pr seen-comments)
                 (setq called (list seen-pr seen-comments)))))
      (gp-detail-send-to-terminal)
      (should (equal called (list pr (list comment-1 comment-2)))))))

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
