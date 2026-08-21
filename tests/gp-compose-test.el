;;; gp-compose-test.el --- Tests for the compose buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Drives the compose buffer's submit/cancel logic with a stubbed
;; submit-function (no network) and checks the target plumbing.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gp-compose)

;; Forward declaration: defined by the optional `markdown-mode' package,
;; only referenced here under `skip-unless (require 'markdown-mode nil t)'.
(defvar markdown-hide-markup)

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

(ert-deftest gp-test-compose-submit-kills-lingering-preview-buffer ()
  "A preview opened before submitting must not linger afterwards."
  (let* ((target (list :full-name "ws/slug" :id 12
                       :submit-function (lambda (&rest _) '((id . 1)))))
         (buf (gp-compose target)))
    (unwind-protect
        (with-current-buffer buf
          (insert "hello **world**")
          (gp-compose-preview)
          (should (get-buffer gp-compose-preview-buffer))
          (cl-letf (((symbol-function 'set-window-configuration) #'ignore))
            (gp-compose-submit))
          (should-not (get-buffer gp-compose-preview-buffer)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest gp-test-compose-cancel-kills-lingering-preview-buffer ()
  "A preview opened before cancelling must not linger afterwards."
  (let* ((target (list :full-name "ws/slug" :id 12))
         (buf (gp-compose target)))
    (unwind-protect
        (with-current-buffer buf
          (insert "hello **world**")
          (gp-compose-preview)
          (should (get-buffer gp-compose-preview-buffer))
          (cl-letf (((symbol-function 'set-window-configuration) #'ignore))
            (gp-compose-cancel))
          (should-not (get-buffer gp-compose-preview-buffer)))
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

(ert-deftest gp-test-compose-render-markdown-hides-markup ()
  "The preview must actually RENDER markdown (hide `**'/`#' markup and
show real bold/heading faces), not just syntax-color the raw
characters -- a bug report was \"I just see colored markdown syntax\",
traced to `markdown-hide-markup' never being turned on."
  (skip-unless (require 'markdown-mode nil t))
  (let ((buf (gp-compose-render-markdown "# Title\n\n**bold**")))
    (unwind-protect
        (with-current-buffer buf
          (should (eq major-mode 'gfm-mode))
          (should markdown-hide-markup))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest gp-test-compose-render-markdown-resets-stale-view-mode-buffer ()
  "A second render into the same (already `view-mode', read-only)
preview buffer must fully replace the old content, not append to or
fail against it."
  (let ((buf (gp-compose-render-markdown "first draft")))
    (unwind-protect
        (progn
          (should (with-current-buffer buf view-mode))
          (let ((buf2 (gp-compose-render-markdown "second draft")))
            (should (eq buf buf2))
            (with-current-buffer buf2
              (should (equal (buffer-string) "second draft"))
              (should-not (string-match-p "first draft" (buffer-string))))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest gp-test-compose-kill-preview-buffer-noop-when-absent ()
  (when (get-buffer gp-compose-preview-buffer) (kill-buffer gp-compose-preview-buffer))
  (should-not (get-buffer gp-compose-preview-buffer))
  (gp-compose--kill-preview-buffer)  ;; must not error
  (should-not (get-buffer gp-compose-preview-buffer)))

(ert-deftest gp-test-compose-kill-preview-buffer-kills-existing ()
  (gp-compose-render-markdown "some text")
  (should (get-buffer gp-compose-preview-buffer))
  (gp-compose--kill-preview-buffer)
  (should-not (get-buffer gp-compose-preview-buffer)))

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
      (should (= (nth 0 res) (- (point) (length ":thi"))))
      (should (= (nth 1 res) (point)))
      ;; candidates include :thinking: (from emojify or the fallback)
      (should (member ":thinking:" (nth 2 res))))))

(ert-deftest gp-test-compose-emoji-capf-nil-without-colon ()
  (with-temp-buffer
    (insert "no shortcode here")
    (should (null (gp-compose-emoji-capf)))))


(ert-deftest gp-test-compose-soft-wraps-not-auto-fill ()
  "The compose buffer must not hard-wrap what the user types.
`gfm-mode' derives from `text-mode', so a `text-mode-hook' running
`turn-on-auto-fill' would insert real newlines mid-paragraph -- which
`gp-compose-hard-line-breaks' then posts as Markdown hard breaks,
freezing the editor's wrap points into the published comment."
  (let ((text-mode-hook '(turn-on-auto-fill)))
    (with-temp-buffer
      (gp-compose--base-mode)
      (should-not auto-fill-function)
      (should (bound-and-true-p visual-line-mode))
      ;; typing past `fill-column' must not introduce a newline
      (setq-local fill-column 20)
      (let ((line "aaa bbb ccc ddd eee fff ggg hhh"))
        (dolist (ch (append line nil))
          (let ((last-command-event ch))
            (self-insert-command 1)))
        (should (equal (buffer-string) line))
        (should-not (string-match-p "\n" (buffer-string)))))))

;;;; Reusing the editor for something that isn't a comment ------------------

(ert-deftest gp-test-compose-noun-defaults-to-comment ()
  "`:what' renames what the buffer thinks it is editing."
  (should (equal (gp-compose--noun '(:id 1)) "comment"))
  (should (equal (gp-compose--noun '(:id 1 :what "description")) "description")))

(ert-deftest gp-test-compose-describe-target-uses-what ()
  "A `:what' target gets its own header, not \"Comment on …\"."
  (should (equal (gp-compose--describe-target '(:id 7 :what "description"))
                 "Description of PR #7"))
  ;; unchanged for the comment cases
  (should (equal (gp-compose--describe-target '(:id 7)) "Comment on PR #7"))
  (should (equal (gp-compose--describe-target '(:id 7 :parent 3))
                 "Reply on PR #7")))

(ert-deftest gp-test-compose-no-hard-breaks-target-keeps-newlines ()
  "`:no-hard-breaks' suppresses the hard-break rewrite on submit.
A PR description is a document: appending \"  \" to its every line on
each save would slowly mangle tables and lists.  This guards the
opt-out travelling on the target, since `gp-compose-submit' applies
the rewrite before the `:submit-function' ever sees the text."
  (let (sent)
    (with-temp-buffer
      (insert "line one
line two")
      (setq gp-compose--target
            (list :full-name "ws/repo" :id 7 :what "description"
                  :no-hard-breaks t
                  :submit-function (lambda (_fn _id text &rest _)
                                     (setq sent text) '((id . 1))))
            gp-compose--return-window nil)
      (gp-compose-submit))
    (should (equal sent "line one
line two"))
    (should-not (string-match-p "  
" sent))))

(ert-deftest gp-test-compose-hard-breaks-still-apply-without-the-opt-out ()
  "The default (comment) path is unchanged by the opt-out's existence."
  (let (sent)
    (with-temp-buffer
      (insert "line one
line two")
      (setq gp-compose--target
            (list :full-name "ws/repo" :id 7
                  :submit-function (lambda (_fn _id text &rest _)
                                     (setq sent text) '((id . 1))))
            gp-compose--return-window nil)
      (gp-compose-submit))
    (should (equal sent "line one  
line two"))))

(ert-deftest gp-test-compose-empty-is-an-error-but-clearable-with-allow-empty ()
  "Empty text aborts a comment, but can clear an `:allow-empty' target."
  ;; a comment: refused outright, no confirmation offered
  (with-temp-buffer
    (setq gp-compose--target '(:full-name "ws/repo" :id 7)
          gp-compose--return-window nil)
    (should-error (gp-compose-submit) :type 'user-error))
  ;; a description: confirmed, then sent as an empty string
  (let (sent called)
    (with-temp-buffer
      (setq gp-compose--target
            (list :full-name "ws/repo" :id 7 :what "description"
                  :allow-empty t
                  :submit-function (lambda (_fn _id text &rest _)
                                     (setq sent text called t) '((id . 1))))
            gp-compose--return-window nil)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (gp-compose-submit)))
    (should called)
    (should (equal sent "")))
  ;; declining the confirmation sends nothing at all
  (let (called)
    (with-temp-buffer
      (setq gp-compose--target
            (list :full-name "ws/repo" :id 7 :what "description"
                  :allow-empty t
                  :submit-function (lambda (&rest _) (setq called t) nil))
            gp-compose--return-window nil)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (should-error (gp-compose-submit) :type 'user-error)))
    (should-not called)))

;;;; Inline target pre-check ---------------------------------------------------

(ert-deftest gp-test-compose-refuses-an-unpostable-inline-target ()
  "A bad inline target aborts BEFORE posting, keeping the buffer.
GitHub answers an out-of-diff path/line with a bare 422 after the
request goes out; the text the user just wrote is what is at stake, so
the check has to happen here and must not kill the buffer."
  (let ((posted nil))
    (with-temp-buffer
      (insert "a careful review note")
      (setq gp-compose--target
            (list :full-name "acme/web" :id 7
                  :inline (cons "gp-helm.el" 500)
                  :submit-function (lambda (&rest _) (setq posted t) '((id . 1))))
            gp-compose--return-window nil)
      (cl-letf (((symbol-function 'gp-inline-target-problem)
                 (lambda (&rest _) "line 500 of gp-helm.el is not in PR #7's diff")))
        (should-error (gp-compose-submit) :type 'user-error))
      ;; nothing was sent, and the text is still here to retry with
      (should-not posted)
      (should (equal (string-trim (buffer-string)) "a careful review note")))))

(ert-deftest gp-test-compose-posts-when-the-inline-target-is-fine ()
  "No problem reported means the comment goes out as before."
  (let ((posted nil))
    (with-temp-buffer
      (insert "looks good")
      (setq gp-compose--target
            (list :full-name "acme/web" :id 7
                  :inline (cons "gp-helm.el" 150)
                  :submit-function (lambda (&rest _) (setq posted t) '((id . 1))))
            gp-compose--return-window nil)
      (cl-letf (((symbol-function 'gp-inline-target-problem) (lambda (&rest _) nil)))
        (gp-compose-submit))
      (should posted))))

(ert-deftest gp-test-compose-skips-the-check-for-replies-and-general ()
  "A reply carries a parent and a general comment has no target at all,
so neither needs the diff lookup -- and must not pay for one."
  (let ((checked 0) (posted 0))
    (cl-letf (((symbol-function 'gp-inline-target-problem)
               (lambda (&rest _) (cl-incf checked) nil)))
      ;; a reply: has :inline for context, but also :parent
      (with-temp-buffer
        (insert "replying")
        (setq gp-compose--target
              (list :full-name "acme/web" :id 7 :parent 99
                    :inline (cons "gp-helm.el" 500)
                    :submit-function (lambda (&rest _) (cl-incf posted) '((id . 1))))
              gp-compose--return-window nil)
        (gp-compose-submit))
      ;; a general comment: no :inline
      (with-temp-buffer
        (insert "general note")
        (setq gp-compose--target
              (list :full-name "acme/web" :id 7
                    :submit-function (lambda (&rest _) (cl-incf posted) '((id . 1))))
              gp-compose--return-window nil)
        (gp-compose-submit)))
    (should (= posted 2))
    (should (= checked 0))))

(provide 'gp-compose-test)
;;; gp-compose-test.el ends here
