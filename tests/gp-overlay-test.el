;;; gp-overlay-test.el --- Tests for inline comment overlays -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercises the comment-grouping logic and the buffer overlay drawing,
;; including against the real captured comments fixture.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gp-overlay)
(require 'bitbucket-mock)

(ert-deftest gp-test-group-comments-by-file ()
  (let* ((comments
          '(((inline (path . "a.ts") (to . 10)) (content (raw . "c1")))
            ((inline (path . "a.ts") (to . 10)) (content (raw . "c2")))
            ((inline (path . "a.ts") (to . 3))  (content (raw . "c3")))
            ((inline (path . "b.ts") (to . 5))  (content (raw . "c4")))
            ((content (raw . "general - skipped")))))
         (grouped (gp-overlay-comments-by-file comments)))
    (should (= (length grouped) 2))
    (let ((a (cdr (assoc "a.ts" grouped))))
      ;; lines sorted ascending: 3 then 10
      (should (equal (mapcar #'car a) '(3 10)))
      ;; line 10 accumulated both comments
      (should (= (length (cdr (assq 10 a))) 2)))))

(ert-deftest gp-test-group-skips-non-inline ()
  (should (null (gp-overlay-comments-by-file
                 '(((content (raw . "x"))))))))

(ert-deftest gp-test-group-real-fixture ()
  "The real captured comments contain 5 inline comments."
  (let* ((comments (alist-get 'values (bitbucket-mock--fixture "pr-comments.json")))
         (grouped (gp-overlay-comments-by-file comments))
         (n (cl-reduce #'+ grouped
                       :key (lambda (fe) (cl-reduce #'+ (cdr fe)
                                                    :key (lambda (le) (length (cdr le)))))
                       :initial-value 0)))
    (should (> (length grouped) 0))
    (should (= n 5))))

(ert-deftest gp-test-apply-overlays-to-buffer ()
  "Overlays land at the right line and carry the comment text + buttons."
  (with-temp-buffer
    (dotimes (i 20) (insert (format "line %d\n" (1+ i))))
    (let* ((line-alist
            `((3 . (((id . 1) (user (display_name . "Ann")) (content (raw . "fix this")))))
              (10 . (((id . 2) (user (display_name . "Bob")) (content (raw . "ok")))))))
           (n (gp-overlay-apply-to-buffer (current-buffer) line-alist)))
      (should (= n 2))
      (let* ((ov (cl-find 3 gp-overlay--list
                          :key (lambda (o)
                                 (line-number-at-pos (overlay-start o))))))
        (should ov)
        (let ((s (overlay-get ov 'after-string)))
          (should (string-match-p "fix this" s))
          (should (string-match-p "\\[reply\\]" s))
          (should (string-match-p "\\[resolve\\]" s)))
        ;; the overlay stores its comments for action lookup
        (should (= (alist-get 'id (car (overlay-get ov 'gp-comments))) 1)))
      (gp-overlay-clear)
      (should (null gp-overlay--list)))))

(ert-deftest gp-test-apply-overlays-out-of-range-line ()
  "A line past end-of-buffer is skipped, not an error."
  (with-temp-buffer
    (insert "only one line\n")
    (should (= 0 (gp-overlay-apply-to-buffer
                  (current-buffer)
                  '((99 . (((id . 5) (user (display_name . "X")) (content (raw . "y"))))))))) ))

(ert-deftest gp-test-resolved-comment-renders-distinctly ()
  "A resolved comment shows the resolved marker and a reopen button."
  (let ((c '((id . 9) (user (display_name . "Ann"))
             (content (raw . "please fix"))
             (resolution (user (display_name . "Bob"))))))
    (with-temp-buffer
      (let ((s (gp-overlay--comment-string c)))
        (should (string-match-p "✓ resolved" s))
        (should (string-match-p "\\[reopen\\]" s))
        (should-not (string-match-p "\\[resolve\\]" s))))))

(ert-deftest gp-test-non-resolvable-comment-hides-resolve-button ()
  "A comment the backend reports as non-resolvable shows neither
[resolve] nor [reopen] (e.g. a GitHub general/issue comment)."
  (let ((c '((id . 10) (user (display_name . "Ann"))
             (content (raw . "just a note")))))
    (with-temp-buffer
      (cl-letf (((symbol-function 'gp-comment-resolvable-p) (lambda (_) nil)))
        (let ((s (gp-overlay--comment-string c)))
          (should-not (string-match-p "\\[resolve\\]" s))
          (should-not (string-match-p "\\[reopen\\]" s))
          (should (string-match-p "\\[reply\\]" s)))))))

(ert-deftest gp-test-toggle-collapse-shortens-render ()
  "Collapsing a multi-line comment renders only its first line."
  (with-temp-buffer
    (let ((c '((id . 7) (user (display_name . "Ann"))
               (content (raw . "first line\nsecond line\nthird")))))
      ;; expanded: contains the later lines
      (should (string-match-p "second line" (gp-overlay--comment-string c)))
      (gp-overlay-toggle-collapse c)
      ;; collapsed: only the first line survives, with a … marker
      (let ((s (gp-overlay--comment-string c)))
        (should (string-match-p "first line" s))
        (should-not (string-match-p "second line" s))
        (should (string-match-p "…" s)))
      ;; toggling again expands
      (gp-overlay-toggle-collapse c)
      (should (string-match-p "second line" (gp-overlay--comment-string c))))))

(ert-deftest gp-test-comment-at-point-from-overlay ()
  "Point on an overlaid line resolves to that line's comment."
  (with-temp-buffer
    (dotimes (i 5) (insert (format "line %d\n" (1+ i))))
    (gp-overlay-apply-to-buffer
     (current-buffer)
     '((2 . (((id . 42) (user (display_name . "A")) (content (raw . "hi")))))))
    (goto-char (point-min))
    (forward-line 1)                    ;; line 2
    (let ((c (gp-overlay-comment-at-point)))
      (should c)
      (should (= (alist-get 'id c) 42)))))

(ert-deftest gp-test-resolve-action-calls-api-and-redraws ()
  "Resolving the comment at point hits the API and flips local state."
  (require 'gp-local)
  (with-temp-buffer
    (dotimes (i 5) (insert (format "line %d\n" (1+ i))))
    (setq gp-overlay--pr
          '((id . 3) (destination (repository (full_name . "ws/slug")))))
    (gp-overlay-apply-to-buffer
     (current-buffer)
     '((2 . (((id . 77) (user (display_name . "A")) (content (raw . "x")))))))
    (goto-char (point-min)) (forward-line 1)
    (let ((called nil))
      (cl-letf (((symbol-function 'gp-resolve-comment)
                 (lambda (fn id cid) (setq called (list fn id cid)) t)))
        (gp-overlay-resolve))
      (should (equal called '("ws/slug" 3 77)))
      ;; the stored comment is now resolved, so a redraw shows it
      (let ((c (car (overlay-get (car gp-overlay--list) 'gp-comments))))
        (should (gp-comment-resolved-p c))))))

(ert-deftest gp-test-overlay-hides-resolved ()
  "Resolved comments are dropped from the grouped overlays by default."
  (clrhash gp--comment-outdated-cache)
  (let ((comments
         '(((id . 1) (inline (path . "a.el") (to . 2)) (content (raw . "open")))
           ((id . 2) (inline (path . "a.el") (to . 5)) (content (raw . "done"))
            (resolution (user (display_name . "X")))))))
    (let ((gp-overlay-show-resolved nil))
      (should (equal (mapcar #'car (cdr (assoc "a.el" (gp-overlay-comments-by-file comments))))
                     '(2))))                       ;; only line 2 (the open one)
    (let ((gp-overlay-show-resolved t))
      (should (= (length (cdr (assoc "a.el" (gp-overlay-comments-by-file comments)))) 2)))))

(ert-deftest gp-test-overlay-hides-reply-to-resolved-parent ()
  "A reply to a resolved thread is hidden even though its own
`resolution' is unset -- Bitbucket only stamps the comment the
resolve action targeted, usually the thread root."
  (clrhash gp--comment-outdated-cache)
  (let ((comments
         '(((id . 1) (inline (path . "a.el") (to . 2)) (content (raw . "open")))
           ((id . 2) (inline (path . "a.el") (to . 5)) (content (raw . "done"))
            (resolution (user (display_name . "X"))))
           ((id . 3) (inline (path . "a.el") (to . 5)) (content (raw . "thanks!"))
            (parent (id . 2))))))
    (let ((gp-overlay-show-resolved nil))
      (should (equal (mapcar #'car (cdr (assoc "a.el" (gp-overlay-comments-by-file comments))))
                     '(2))))                       ;; only line 2 (the open one)
    (let* ((gp-overlay-show-resolved t)
           (line5 (cdr (assq 5 (cdr (assoc "a.el" (gp-overlay-comments-by-file comments)))))))
      (should (= (length line5) 2)))))               ;; parent + reply both shown

(ert-deftest gp-test-overlay-hides-outdated ()
  "Comments anchored to lines absent from the diff are dropped by default.
Outdated-ness is computed from the diff: line 2 is in the hunk
\(present), line 5 is not (outdated)."
  (clrhash gp--comment-outdated-cache)
  (let* ((comments
          '(((id . 1) (inline (path . "a.el") (to . 2)) (content (raw . "open")))
            ((id . 2) (inline (path . "a.el") (to . 5)) (content (raw . "stale")))))
         ;; a hunk covering new-side lines 1-3 only; line 5 is not present
         (diff "diff --git a/a.el b/a.el
--- a/a.el
+++ b/a.el
@@ -1,3 +1,3 @@
 one
-two-old
+two-new
 three
")
         (dbf (gp-split-diff-by-file diff)))
    (let ((gp-overlay-show-outdated nil))
      (should (equal (mapcar #'car (cdr (assoc "a.el" (gp-overlay-comments-by-file comments dbf))))
                     '(2))))                       ;; only line 2 (still in the diff)
    (let ((gp-overlay-show-outdated t))
      (should (= (length (cdr (assoc "a.el" (gp-overlay-comments-by-file comments dbf)))) 2)))
    ;; with no diff and a cold cache, nothing is treated as outdated
    ;; (graceful degrade); use fresh ids so the monotonic cache is empty
    (clrhash gp--comment-outdated-cache)
    (let ((fresh '(((id . 91) (inline (path . "a.el") (to . 2)) (content (raw . "open")))
                   ((id . 92) (inline (path . "a.el") (to . 5)) (content (raw . "stale"))))))
      (should (= (length (cdr (assoc "a.el" (gp-overlay-comments-by-file fresh)))) 2)))))

(ert-deftest gp-test-comment-outdated-cache-is-monotonic ()
  "Once proven outdated, a comment stays outdated even with no diff."
  (clrhash gp--comment-outdated-cache)
  (let ((c '((id . 7) (inline (path . "a.el") (to . 5)) (content (raw . "stale"))))
        (dbf (gp-split-diff-by-file "diff --git a/a.el b/a.el
--- a/a.el
+++ b/a.el
@@ -1,1 +1,1 @@
 one
")))
    (should (gp-comment-outdated-p c dbf))      ;; proven outdated from the diff
    (should (gp-comment-outdated-p c nil))))    ;; sticky: cached, no diff needed

(ert-deftest gp-test-overlay-global-toggle-suppresses-draw ()
  "With overlays globally off, apply-to-buffer draws nothing but keeps lines."
  (with-temp-buffer
    (dotimes (i 5) (insert (format "line %d\n" (1+ i))))
    (let ((gp-overlay-enabled nil))
      (gp-overlay-apply-to-buffer
       (current-buffer)
       '((2 . (((id . 1) (user (display_name . "A")) (content (raw . "hi")))))))
      (should (null gp-overlay--list))             ;; nothing drawn
      (should gp-overlay--lines))                  ;; but remembered
    (let ((gp-overlay-enabled t))
      (gp-overlay-apply-to-buffer (current-buffer) gp-overlay--lines)
      (should (= (length gp-overlay--list) 1)))))

(provide 'gp-overlay-test)
;;; gp-overlay-test.el ends here
