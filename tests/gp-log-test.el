;;; gp-log-test.el --- Tests for the diagnostic log -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'gp-log)

(defmacro gp-log-test--fresh (&rest body)
  (declare (indent 0) (debug t))
  `(let ((gp-log-enabled t)
         (gp-log-buffer-name "*bb-log-test*"))
     (when (get-buffer gp-log-buffer-name)
       (kill-buffer gp-log-buffer-name))
     (unwind-protect (progn ,@body)
       (when (get-buffer gp-log-buffer-name)
         (kill-buffer gp-log-buffer-name)))))

(ert-deftest gp-test-log-appends-line ()
  (gp-log-test--fresh
    (gp-log 'info "hello %d" 42)
    (with-current-buffer gp-log-buffer-name
      (should (string-match-p "\\[INFO\\] hello 42" (buffer-string))))))

(ert-deftest gp-test-log-error-level ()
  (gp-log-test--fresh
    (gp-log-error "boom %s" "x")
    (with-current-buffer gp-log-buffer-name
      (should (string-match-p "\\[ERROR\\] boom x" (buffer-string))))))

(ert-deftest gp-test-log-disabled-noop ()
  (gp-log-test--fresh
    (let ((gp-log-enabled nil))
      (gp-log 'info "should not appear"))
    (should (or (null (get-buffer gp-log-buffer-name))
                (with-current-buffer gp-log-buffer-name
                  (= (buffer-size) 0))))))

(ert-deftest gp-test-log-trims-to-max ()
  (gp-log-test--fresh
    (let ((gp-log-max-lines 10))
      (dotimes (i 50) (gp-log 'info "line %d" i))
      (with-current-buffer gp-log-buffer-name
        (should (<= (count-lines (point-min) (point-max)) 12))
        ;; the most recent line survived
        (should (string-match-p "line 49" (buffer-string)))))))

(ert-deftest gp-test-log-never-signals ()
  "Logging with a bad format spec must not raise."
  (gp-log-test--fresh
    (should (progn (gp-log 'info "%d" "not-a-number") t))))

(provide 'gp-log-test)
;;; gp-log-test.el ends here
