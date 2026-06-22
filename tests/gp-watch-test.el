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

(ert-deftest gp-test-watch-mode-line-format ()
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
    (should (string-match-p "💬0" s))))

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
  "Overlays are drawn only for comments whose inline path is THIS file."
  (bitbucket-mock-with-service
    (cl-letf (((symbol-function 'gp-local-find-checkout)
               (lambda (_fn) "/repo"))
              ;; pretend the visited file is one with comments in the fixture
              ((symbol-function 'gp-pull-request-comments)
               (lambda (&rest _)
                 '(((id . 1) (user (display_name . "A"))
                    (content (raw . "hi")) (inline (path . "wanted.el") (to . 2)))
                   ((id . 2) (user (display_name . "B"))
                    (content (raw . "yo")) (inline (path . "other.el") (to . 9)))))))
      (with-temp-buffer
        (setq buffer-file-name "/repo/wanted.el")
        (dotimes (i 5) (insert (format "line %d\n" (1+ i))))
        (cl-letf (((symbol-function 'file-relative-name)
                   (lambda (_f _d) "wanted.el")))
          (let ((n (gp-watch--overlay-if-comments '((id . 7)) "ws/slug")))
            (should (= n 1))))            ;; only the wanted.el comment
        (set-buffer-modified-p nil)))))

(ert-deftest gp-test-watch-no-overlay-when-file-absent-from-comments ()
  (bitbucket-mock-with-service
    (cl-letf (((symbol-function 'gp-local-find-checkout)
               (lambda (_fn) "/repo"))
              ((symbol-function 'gp-pull-request-comments)
               (lambda (&rest _)
                 '(((id . 2) (user (display_name . "B"))
                    (content (raw . "yo")) (inline (path . "other.el") (to . 9))))))
              ((symbol-function 'file-relative-name)
               (lambda (_f _d) "wanted.el")))
      (with-temp-buffer
        (setq buffer-file-name "/repo/wanted.el")
        (insert "x\n")
        (should (null (gp-watch--overlay-if-comments '((id . 7)) "ws/slug")))
        (set-buffer-modified-p nil)))))

(ert-deftest gp-test-watch-activates-in-magit-buffer-from-directory ()
  "Magit buffers use `default-directory' for repo and branch PR resolution."
  (let ((gp-watch-mode t))
    (with-temp-buffer
      (setq default-directory "/repo/")
      (cl-letf (((symbol-function 'derived-mode-p)
                 (lambda (&rest modes) (memq 'magit-mode modes)))
                ((symbol-function 'gp-watch--repo-for-path)
                 (lambda (path)
                   (should (equal path "/repo/"))
                   "ws/slug"))
                ((symbol-function 'gp-watch--open-count)
                 (lambda (_full-name) 4))
                ((symbol-function 'gp-watch--current-branch)
                 (lambda (path)
                   (should (equal path "/repo/"))
                   "feature/demo"))
                ((symbol-function 'gp-watch--pr-for)
                 (lambda (_full-name branch)
                   (should (equal branch "feature/demo"))
                   '((id . 8) (comment_count . 2))))
                ((symbol-function 'gp-watch--overlay-if-comments)
                 (lambda (&rest _) (ert-fail "should not overlay in non-file Magit buffer"))))
        (gp-watch--maybe-activate)
        (should (equal gp-watch--repo "ws/slug"))
        (should (= gp-watch--pr-count 4))
        (should (= (alist-get 'id gp-watch--branch-pr) 8))
        (should (string-match-p "BB:4" (substring-no-properties gp-watch-mode-line)))
        (should (string-match-p "💬2" (substring-no-properties gp-watch-mode-line)))))))

(provide 'gp-watch-test)
;;; gp-watch-test.el ends here
