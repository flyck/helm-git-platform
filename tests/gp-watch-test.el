;;; gp-watch-test.el --- Tests for the global watch mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests the TTL cache, the resolution pipeline, and the mode-line
;; formatting.  Time is injected via `gp-watch-now-function' so
;; expiry is deterministic; API and git calls are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gp-watch)
(require 'bitbucket-mock)
(require 'github-mock)

(defmacro gp-watch-test--with-clock (&rest body)
  "Run BODY with a controllable clock bound to `clock' (a settable var)."
  (declare (indent 0) (debug t))
  `(let* ((clock 1000.0)
          (gp-watch-now-function (lambda () clock)))
     (gp-watch-clear-cache)
     ,@body))

(ert-deftest gp-test-watch-cache-ttl-expiry ()
  (gp-watch-test--with-clock
    (let ((tbl (make-hash-table :test 'equal)))
      (gp-watch--cache-put tbl "k" "v" 100)
      (should (equal (gp-watch--cache-get tbl "k") "v"))
      (setq clock 1099.0)
      (should (equal (gp-watch--cache-get tbl "k") "v"))
      (setq clock 1101.0)                 ;; past expiry
      (should (eq (gp-watch--cache-get tbl "k") 'miss)))))

(ert-deftest gp-test-watch-cache-negative ()
  "A cached nil is a negative hit, not a miss."
  (gp-watch-test--with-clock
    (let ((tbl (make-hash-table :test 'equal)))
      (gp-watch--cache-put tbl "k" nil 100)
      (should (null (gp-watch--cache-get tbl "k")))   ;; not 'miss
      (should-not (eq (gp-watch--cache-get tbl "k") 'miss)))))

(ert-deftest gp-test-watch-repo-lookup-cached ()
  "Repo membership is resolved once then served from cache."
  (gp-watch-test--with-clock
    (let ((calls 0))
      (cl-letf (((symbol-function 'gp-local--dir-remote)
                 (lambda (_dir) (cl-incf calls) "ws/slug"))
                ((symbol-function 'locate-dominating-file)
                 (lambda (_d _n) "/repo/")))
        (should (equal (gp-watch--repo-for-file "/repo/a.el") "ws/slug"))
        (should (equal (gp-watch--repo-for-file "/repo/b.el") "ws/slug"))
        (should (= calls 1))               ;; second call hit the cache
        ;; after TTL it re-resolves
        (setq clock (+ clock gp-watch-repo-cache-ttl 1))
         (gp-watch--repo-for-file "/repo/a.el")
         (should (= calls 2))))))

(ert-deftest gp-test-watch-repo-lookup-negative-expires-sooner ()
  "A failed repo lookup is retried long before a successful one would be.
A miss can mean the active backend talks to the other forge, which
changes within a session -- so it must not be pinned for a whole day."
  (gp-watch-test--with-clock
    (let ((calls 0))
      (cl-letf (((symbol-function 'gp-local--dir-remote)
                 (lambda (_dir) (cl-incf calls) nil))
                ((symbol-function 'locate-dominating-file)
                 (lambda (_d _n) "/repo/")))
        (should-not (gp-watch--repo-for-file "/repo/a.el"))
        (should (= calls 1))
        ;; still inside the short negative window: served from cache
        (setq clock (+ clock (max 0 (- gp-watch-repo-negative-cache-ttl 1))))
        (should-not (gp-watch--repo-for-file "/repo/a.el"))
        (should (= calls 1))
        ;; past it -- and still far short of the positive TTL -- so re-resolved
        (setq clock (+ clock 2))
        (should (< gp-watch-repo-negative-cache-ttl gp-watch-repo-cache-ttl))
        (should-not (gp-watch--repo-for-file "/repo/a.el"))
        (should (= calls 2))))))

(ert-deftest gp-test-watch-repo-lookup-positive-keeps-long-ttl ()
  "A successful lookup still gets the full day-long cache."
  (gp-watch-test--with-clock
    (let ((calls 0))
      (cl-letf (((symbol-function 'gp-local--dir-remote)
                 (lambda (_dir) (cl-incf calls) "ws/slug"))
                ((symbol-function 'locate-dominating-file)
                 (lambda (_d _n) "/repo/")))
        (should (equal (gp-watch--repo-for-file "/repo/a.el") "ws/slug"))
        (setq clock (+ clock gp-watch-repo-negative-cache-ttl 1))
        (should (equal (gp-watch--repo-for-file "/repo/a.el") "ws/slug"))
        (should (= calls 1))))))

(ert-deftest gp-test-watch-repo-lookup-for-directory ()
  "Directory-backed buffers resolve via the repo root too."
  (gp-watch-test--with-clock
    (let ((calls 0))
      (cl-letf (((symbol-function 'gp-local--dir-remote)
                 (lambda (_dir) (cl-incf calls) "ws/slug"))
                ((symbol-function 'locate-dominating-file)
                 (lambda (_d _n) "/repo/"))
                ((symbol-function 'file-directory-p)
                 (lambda (path) (equal path "/repo/"))))
        (should (equal (gp-watch--repo-for-path "/repo/") "ws/slug"))
        (should (= calls 1))))))

(ert-deftest gp-test-watch-pr-for-branch-cached ()
  (gp-watch-test--with-clock
    (let ((calls 0))
      (cl-letf (((symbol-function 'gp-open-pr-for-branch)
                 (lambda (_fn _br) (cl-incf calls) '((id . 5)))))
        (should (= (alist-get 'id (gp-watch--pr-for "ws/slug" "feat")) 5))
        (gp-watch--pr-for "ws/slug" "feat")
        (should (= calls 1))))))

(ert-deftest gp-test-watch-mode-line-counts-github-comments ()
  "GitHub has no `comment_count'; the counter sums general and inline
comments of the full PR, which the list endpoint does not carry."
  (github-mock-with-service
    (let ((pr (gp-open-pr-for-branch "acme/web" "feature/widget")))
      (should (string-match-p
               "💬3" (substring-no-properties
                      (gp-watch--format-mode-line nil pr)))))))

(ert-deftest gp-test-watch-mode-line-format ()
  "Bitbucket PRs carry `comment_count'; see the GitHub test above."
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (should (equal (gp-watch--format-mode-line nil nil) ""))
    ;; just the repo open-PR count
    (let ((s (substring-no-properties (gp-watch--format-mode-line 3 nil))))
      (should (equal s " BB:3")))
    ;; on a branch PR: clickable comment counter with the 💬 emoji
    (let ((s (substring-no-properties
              (gp-watch--format-mode-line 2 '((id . 1) (comment_count . 5))))))
      (should (string-match-p "BB:2" s))
      (should (string-match-p "💬5" s)))
    ;; branch PR with no repo count still shows the comment counter
    (let ((s (substring-no-properties
              (gp-watch--format-mode-line nil '((id . 9) (comment_count . 0))))))
      (should (string-match-p "💬0" s)))))

(ert-deftest gp-test-overlay-reapply-after-revert ()
  "Stored lines are re-applied (overlays redrawn) after a simulated revert."
  (require 'gp-overlay)
  (with-temp-buffer
    (dotimes (i 5) (insert (format "line %d\n" (1+ i))))
    (gp-overlay-apply-to-buffer
     (current-buffer)
     '((2 . (((id . 1) (user (display_name . "A")) (content (raw . "hi")))))))
    (should (= (length gp-overlay--list) 1))
    ;; simulate a revert: overlays gone, but buffer-local lines remain
    (gp-overlay-clear)
    (should (null gp-overlay--list))
    (gp-overlay--reapply-after-revert)
    (should (= (length gp-overlay--list) 1))))

(ert-deftest gp-test-watch-overlays-only-when-comments-match-file ()
  "Overlays are drawn only for comments whose inline path is THIS file.
The fixture PR has two inline comments on options-dialog.component.ts
\(lines 40 and 105) and others on different files; a file with none
gets no overlay at all."
  (bitbucket-mock-with-service
    (cl-letf (((symbol-function 'gp-local-find-checkout) (lambda (_fn) "/repo/")))
      (with-temp-buffer
        (setq buffer-file-name
              "/repo/src/app/dialogs/options-dialog/options-dialog.component.ts")
        (dotimes (i 120) (insert (format "line %d\n" (1+ i))))
        (should (= (gp-watch--overlay-if-comments '((id . 180)) "acme/web-frontend") 2))
        (should (equal (sort (mapcar (lambda (ov) (line-number-at-pos (overlay-start ov)))
                                     gp-overlay--list)
                             #'<)
                       '(40 105)))
        (should (cl-find "/repositories/acme/web-frontend/pullrequests/180/comments"
                         bitbucket-mock-calls :key #'cadr :test #'equal))
        (set-buffer-modified-p nil))
      (with-temp-buffer
        (setq buffer-file-name "/repo/src/app/untouched.ts")
        (insert "x\n")
        (should (null (gp-watch--overlay-if-comments '((id . 180)) "acme/web-frontend")))
        (should (null gp-overlay--list))
        (set-buffer-modified-p nil)))))

(ert-deftest gp-test-watch-activates-in-magit-buffer-from-directory ()
  "Magit buffers use `default-directory' for repo and branch PR resolution.
The open-PR count and the branch PR come from the (GitHub) mock: two
open PRs, #42 on feature/widget with 1 general + 2 inline comments."
  (github-mock-with-service
    (let ((gp-watch-mode t))
      (gp-watch-clear-cache)
      (with-temp-buffer
        (setq default-directory "/repo/")
        (cl-letf (((symbol-function 'derived-mode-p)
                   (lambda (&rest modes) (memq 'magit-mode modes)))
                  ((symbol-function 'gp-watch--repo-for-path)
                   (lambda (path)
                     (should (equal path "/repo/"))
                     "acme/web"))
                  ((symbol-function 'gp-watch--current-branch)
                   (lambda (path)
                     (should (equal path "/repo/"))
                     "feature/widget"))
                  ((symbol-function 'gp-watch--overlay-if-comments)
                   (lambda (&rest _) (ert-fail "should not overlay in non-file Magit buffer"))))
          (gp-watch--maybe-activate)
          (should (equal gp-watch--repo "acme/web"))
          (should (= gp-watch--pr-count 2))
          (should (= (alist-get 'id gp-watch--branch-pr) 42))
          (should (member '("GET" "/repos/acme/web/pulls"
                            (("state" . "open") ("head" . "acme:feature/widget")))
                          (mapcar (lambda (c) (seq-take c 3)) github-mock-calls)))
          (let ((line (substring-no-properties gp-watch-mode-line)))
            (should (string-match-p ":2\\b" line))
            (should (string-match-p "💬3" line))))))
    (gp-watch-clear-cache)))

(ert-deftest gp-test-watch-visit-branch-pr-opens-single-match ()
  "A single branch match opens the PR detail directly."
  (let ((shown nil)
        (gp-watch--repo "ws/slug")
        (gp-watch--branch-pr '((id . 7))))
    (with-temp-buffer
      (setq default-directory "/repo/")
      (cl-letf (((symbol-function 'gp-watch--context-path) (lambda () "/repo/"))
                ((symbol-function 'gp-watch--current-branch) (lambda (_path) "feat/demo"))
                ((symbol-function 'gp-repo-pull-requests)
                 (lambda (_full-name)
                   '(((id . 7) (source (branch (name . "feat/demo")))))))
                ((symbol-function 'gp-show-pr)
                 (lambda (pr) (setq shown pr))))
        (gp-watch-visit-branch-pr)
        (should (= (alist-get 'id shown) 7))))))

(ert-deftest gp-test-watch-visit-branch-pr-opens-helm-on-multiple-matches ()
  "Multiple branch matches open the branch-filtered Helm picker."
  (let ((called nil)
        (gp-watch--repo "ws/slug")
        (gp-watch--branch-pr '((id . 7))))
    (with-temp-buffer
      (setq default-directory "/repo/")
      (cl-letf (((symbol-function 'gp-watch--context-path) (lambda () "/repo/"))
                ((symbol-function 'gp-watch--current-branch) (lambda (_path) "feat/demo"))
                ((symbol-function 'gp-repo-pull-requests)
                 (lambda (_full-name)
                   '(((id . 7) (source (branch (name . "feat/demo"))))
                     ((id . 8) (source (branch (name . "feat/demo")))))))
                ((symbol-function 'gp-helm-repo-branch)
                 (lambda (full-name branch)
                   (setq called (list full-name branch)))))
        (gp-watch-visit-branch-pr)
        (should (equal called '("ws/slug" "feat/demo")))))))

(provide 'gp-watch-test)
;;; gp-watch-test.el ends here
