;;; gp-compose-test.el --- Tests for the compose buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Drives the compose buffer's submit/cancel logic with a stubbed
;; submit-function (no network) and checks the target plumbing.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gp-compose)

(ert-deftest gp-test-compose-submit-delegates-target ()
  "Submitting passes the buffer text and target fields to the submit fn."
  (let* ((captured nil)
         (target (list :full-name "ws/slug" :id 12
                       :inline '("a.ts" . 5) :parent 7
                       :submit-function
                       (lambda (fn id text inline parent)
                         (setq captured (list fn id text inline parent))
                         '((id . 1)))))
         (buf (gp-compose target)))
    (unwind-protect
        (with-current-buffer buf
          (insert "hello **world**")
          (cl-letf (((symbol-function 'set-window-configuration) #'ignore))
            (gp-compose-submit))
          (should (equal captured
                         '("ws/slug" 12 "hello **world**" ("a.ts" . 5) 7))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest gp-test-compose-empty-is-rejected ()
  (let ((buf (gp-compose
              (list :full-name "ws/slug" :id 1
                    :submit-function (lambda (&rest _) (error "should not run"))))))
    (unwind-protect
        (with-current-buffer buf
          (should-error (gp-compose-submit) :type 'user-error))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest gp-test-compose-on-success-runs ()
  (let* ((got nil)
         (target (list :full-name "ws/slug" :id 1
                       :submit-function (lambda (&rest _) '((id . 42)))
                       :on-success (lambda (c) (setq got (alist-get 'id c)))))
         (buf (gp-compose target)))
    (unwind-protect
        (with-current-buffer buf
          (insert "x")
          (cl-letf (((symbol-function 'set-window-configuration) #'ignore))
            (gp-compose-submit))
          (should (= got 42)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest gp-test-compose-describe-target ()
  (should (string-match-p "Reply"
                          (gp-compose--describe-target
                           '(:id 3 :parent 9))))
  (should (string-match-p "Inline comment on a\\.ts:5"
                          (gp-compose--describe-target
                           '(:id 3 :inline ("a.ts" . 5)))))
  (should (string-match-p "Comment on PR #3"
                          (gp-compose--describe-target '(:id 3)))))

(ert-deftest gp-test-compose-render-markdown ()
  "Rendering returns a buffer containing the source text."
  (let ((buf (gp-compose-render-markdown "# Title\n\nsome *text*")))
    (unwind-protect
        (with-current-buffer buf
          (should (string-match-p "Title" (buffer-string))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest gp-test-compose-hard-breaks ()
  "Single newlines get trailing two spaces; blanks/code fences don't."
  (let ((out (gp-compose--apply-hard-breaks
              "line one\nline two\nline three\n\npara two")))
    ;; lines followed by another text line get a hard break
    (should (string-match-p "line one  \n" out))
    (should (string-match-p "line two  \n" out))
    ;; the line before a blank (paragraph break) does NOT
    (should (string-match-p "line three\n\npara two" out))
    ;; final line never gets trailing spaces
    (should (string-suffix-p "para two" out)))
  ;; code fence content is left untouched
  (let ((out (gp-compose--apply-hard-breaks "```\ncode line\nmore\n```")))
    (should-not (string-match-p "code line  " out))))

(ert-deftest gp-test-compose-emoji-capf ()
  "After typing a colon prefix, the capf offers matching shortcodes."
  (with-temp-buffer
    (insert "great work :thi")
    (let ((res (gp-compose-emoji-capf)))
      (should res)
      (cl-destructuring-bind (start end cands &rest _) res
        (should (= start (- (point) (length ":thi"))))
        (should (= end (point)))
        ;; candidates include :thinking: (from emojify or the fallback)
        (should (member ":thinking:" cands))))))

(ert-deftest gp-test-compose-emoji-capf-nil-without-colon ()
  (with-temp-buffer
    (insert "no shortcode here")
    (should (null (gp-compose-emoji-capf)))))

(provide 'gp-compose-test)
;;; gp-compose-test.el ends here
