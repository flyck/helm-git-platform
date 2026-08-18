;;; gp-magit-test.el --- Tests for magit-diff comment mapping -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests the pure diff-line -> buffer-position mapping used to anchor PR
;; comment overlays in a magit-diff buffer.  The magit interaction itself is
;; not exercised here.

;;; Code:

(require 'ert)
(require 'gp-magit)
(require 'magit-diff nil t)

;; The require above is noerror: in a clean CI checkout magit can fail to
;; load outright (a transient version conflict is enough).  Declare what
;; the tests call so byte-compilation does not fail on undefined
;; functions; the tests themselves skip when magit is absent.
(declare-function magit-diff-range "magit-diff")
(declare-function magit-diff-hunk-line "magit-diff")
(declare-function magit-diff-mode "magit-diff")
(declare-function magit-current-section "magit-section")
(declare-function magit-get-mode-buffer "magit-mode")

;; `gp-watch' is required at RUNTIME inside `skip-unless', so its minor-mode
;; variable is not special at compile time -- a `let' on it would otherwise
;; read as an unused lexical binding.
(defvar gp-watch-mode)

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

(ert-deftest gp-magit-comment-line-positions-magit-section-tree ()
  "Positions are found in a real magit-diff buffer, which has no `+++ b/'.
Magit washes the diff: the `--- a/PATH' / `+++ b/PATH' headers are
removed from the buffer and stashed in the file section's `header'
slot, leaving only a \"modified   PATH\" heading.  This is the format
the feature actually runs against, so it must be covered."
  (skip-unless (require 'magit-diff nil t))
  (let ((dir (make-temp-file "gp-magit-test" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory dir)))
          (call-process "git" nil nil nil "init" "-q")
          (call-process "git" nil nil nil "config" "user.email" "t@example.com")
          (call-process "git" nil nil nil "config" "user.name" "T")
          (with-temp-file (expand-file-name "a.el" dir)
            (insert "one\ntwo\nthree\n"))
          (call-process "git" nil nil nil "add" "-A")
          (call-process "git" nil nil nil "commit" "-q" "-m" "init")
          (with-temp-file (expand-file-name "a.el" dir)
            (insert "one\ntwo\nadded-3\nadded-4\nthree\n"))
          (call-process "git" nil nil nil "add" "-A")
          (call-process "git" nil nil nil "commit" "-q" "-m" "change")
          (magit-diff-range "HEAD~1")
          (let ((buf (magit-get-mode-buffer 'magit-diff-mode)))
            (should buf)
            (with-current-buffer buf
              ;; sanity: the washed buffer really has no +++ header
              (should-not (save-excursion
                            (goto-char (point-min))
                            (re-search-forward "^\\+\\+\\+ b/" nil t)))
              (let ((pos (gp-magit--comment-line-positions "a.el")))
                ;; the two added lines land on new-side lines 3 and 4
                (should (equal (mapcar #'car pos) '(3 4)))
                ;; and every position agrees with magit's own line number
                (dolist (p pos)
                  (save-excursion
                    (goto-char (cdr p))
                    (should (equal (magit-diff-hunk-line
                                    (magit-current-section) nil)
                                   (car p)))))))))
      (delete-directory dir t))))

(ert-deftest gp-magit-keys-win-over-gp-watch-prefix ()
  "`C-c B n' reaches gp-magit, without killing gp-watch's other keys.
Both modes use the `C-c B' prefix, and minor-mode maps outrank the
buffer-local map -- so the magit keys have to go through
`minor-mode-overriding-map-alist' to win.  That override must still
let gp-watch's unshadowed keys (`C-c B p') through."
  (skip-unless (and (require 'magit-diff nil t) (require 'gp-watch nil t)))
  (let ((gp-watch-mode t))
    (with-temp-buffer
      (magit-diff-mode)
      (gp-magit--activate-keys)
      (should (eq (key-binding (kbd "C-c B n")) 'gp-magit-add-comment))
      (should (eq (key-binding (kbd "C-c B g")) 'gp-magit-refresh-comments))
      (should (eq (key-binding (kbd "C-c B p")) 'gp-watch-visit-branch-pr)))))

(provide 'gp-magit-test)
;;; gp-magit-test.el ends here
