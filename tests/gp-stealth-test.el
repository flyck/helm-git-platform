;;; gp-stealth-test.el --- Tests for the Stealth PR action -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'json)
(require 'gp-stealth)

(defconst gp-test-stealth--ctx
  (list :repo "acme/repo" :id 42 :title "Implement thing"
        :url "https://example.com/pr/42"
        :source-branch "feat/demo" :destination-branch "main"
        :dir "/tmp/repo"))

(ert-deftest gp-test-stealth-default-prompt-names-repo-pr-branch-and-folder ()
  (let ((text (gp-stealth-default-prompt gp-test-stealth--ctx)))
    (should (string-match-p "^Repo: acme/repo$" text))
    (should (string-match-p "^PR: #42 Implement thing$" text))
    (should (string-match-p "^Branch: feat/demo → main$" text))
    (should (string-match-p "^Folder: /tmp/repo$" text))
    (should (string-match-p "^URL: https://example.com/pr/42$" text))
    (should-not (string-match-p "Note:" text))
    (should (string-suffix-p "\n\n" text))))

(ert-deftest gp-test-stealth-default-prompt-notes-a-failed-checkout ()
  (let ((text (gp-stealth-default-prompt
               (append (list :checkout-error "conflict") gp-test-stealth--ctx))))
    (should (string-match-p "switching to the PR branch failed (conflict)" text))))

(ert-deftest gp-test-stealth-open-session-writes-request-and-calls-open ()
  (let (handed request)
    (cl-letf (((symbol-function 'gp-stealth--hand-over)
               (lambda (file)
                 (setq handed file
                       request (with-temp-buffer
                                 (let ((coding-system-for-read 'utf-8))
                                   (insert-file-contents file))
                                 (json-parse-string (buffer-string) :object-type 'alist)))
                 nil)))
      (unwind-protect
          (progn
            (gp-stealth-open-session gp-test-stealth--ctx)
            (should (string-suffix-p ".stealth-session" handed))
            (should (equal (alist-get 'workspace request) "/tmp/repo"))
            (should (string-match-p "Branch: feat/demo → main" (alist-get 'prompt request)))
            (should (equal (mapcar #'car request) '(workspace prompt))))
        (when handed (ignore-errors (delete-file handed)))))))

(ert-deftest gp-test-stealth-open-session-cleans-up-and-errors-on-failure ()
  (let (handed)
    (cl-letf (((symbol-function 'gp-stealth--hand-over)
               (lambda (file) (setq handed file) "LSOpen failed -10820"))
              ((symbol-function 'gp-log-error) #'ignore))
      (should-error (gp-stealth-open-session gp-test-stealth--ctx) :type 'user-error)
      (should-not (file-exists-p handed)))))

(ert-deftest gp-test-stealth-open-session-needs-a-dir ()
  (should-error (gp-stealth-open-session (list :repo "acme/repo")) :type 'user-error))

(ert-deftest gp-test-stealth-prompt-function-is-pluggable ()
  (let (request)
    (cl-letf (((symbol-function 'gp-stealth--hand-over)
               (lambda (file)
                 (setq request (with-temp-buffer (insert-file-contents file)
                                                 (json-parse-string (buffer-string) :object-type 'alist)))
                 (delete-file file) nil)))
      (let ((gp-stealth-prompt-function (lambda (ctx) (format "review %s" (plist-get ctx :id)))))
        (gp-stealth-open-session gp-test-stealth--ctx)
        (should (equal (alist-get 'prompt request) "review 42"))))))

(provide 'gp-stealth-test)
;;; gp-stealth-test.el ends here
