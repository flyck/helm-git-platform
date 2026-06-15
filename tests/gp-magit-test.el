;;; gp-magit-test.el --- Tests for magit-diff comment mapping -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests the pure diff-line -> buffer-position mapping used to anchor PR
;; comment overlays in a magit-diff buffer.  The magit interaction itself is
;; not exercised here.

;;; Code:

(require 'ert)
(require 'gp-magit)

(ert-deftest gp-magit-comment-line-positions ()
  "Added (+) lines are mapped to their new-side line numbers."
  (with-temp-buffer
    (insert
     "modified   src/a.el\n"
     "@@ -1,3 +1,4 @@\n"
     " context-1\n"      ;; new line 1
     "-removed\n"        ;; old only -- new side unchanged
     "+added-2\n"        ;; new line 2
     "+added-3\n"        ;; new line 3
     " context-4\n")     ;; new line 4
    ;; the function matches on "+++ b/PATH"; insert that marker form instead
    (erase-buffer)
    (insert
     "+++ b/src/a.el\n"
     "@@ -1,3 +1,4 @@\n"
     " context-1\n"
     "-removed\n"
     "+added-2\n"
     "+added-3\n"
     " context-4\n")
    (let ((pos (gp-magit--comment-line-positions "src/a.el")))
      ;; only the two added lines are recorded, at new-side line numbers 2 and 3
      (should (equal (mapcar #'car pos) '(2 3)))
      ;; positions point at the start of those + lines
      (should (cl-every #'integerp (mapcar #'cdr pos))))))

(ert-deftest gp-magit-comment-line-positions-other-file ()
  "Lines from a different file are not attributed to PATH."
  (with-temp-buffer
    (insert
     "+++ b/other.el\n"
     "@@ -1 +1 @@\n"
     "+nope\n"
     "+++ b/wanted.el\n"
     "@@ -1 +1,2 @@\n"
     " ctx\n"
     "+yes\n")
    (let ((pos (gp-magit--comment-line-positions "wanted.el")))
      (should (equal (mapcar #'car pos) '(2))))))

(provide 'gp-magit-test)
;;; gp-magit-test.el ends here
