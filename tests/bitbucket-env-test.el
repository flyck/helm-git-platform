;;; bitbucket-env-test.el --- Tests for the env importer -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests the rc-file parser and the load/overwrite semantics without
;; touching the user's real environment beyond temporary `setenv's.

;;; Code:

(require 'ert)
(require 'bitbucket-env)

(ert-deftest bitbucket-test-env-parse-export-and-bare ()
  (let ((bitbucket-env-prefix "BITBUCKET"))
    (should (equal
             (bitbucket-env--parse
              (concat
               "# a comment\n"
               "export BITBUCKET_WORKSPACE=\"acme\"\n"
               "BITBUCKET_USER_EMAIL='me@example.com'\n"
               "export OTHER_VAR=ignored\n"
               "export BITBUCKET_API_TOKEN=abc123\n"))
             '(("BITBUCKET_WORKSPACE" . "acme")
               ("BITBUCKET_USER_EMAIL" . "me@example.com")
               ("BITBUCKET_API_TOKEN" . "abc123"))))))

(ert-deftest bitbucket-test-env-parse-strips-trailing-comment ()
  (should (equal
           (bitbucket-env--parse "export BITBUCKET_WORKSPACE=ws   # inline note\n")
           '(("BITBUCKET_WORKSPACE" . "ws")))))

(ert-deftest bitbucket-test-env-parse-keeps-hash-inside-quotes ()
  (should (equal
           (bitbucket-env--parse "export BITBUCKET_API_TOKEN=\"a#b#c\"\n")
           '(("BITBUCKET_API_TOKEN" . "a#b#c")))))

(ert-deftest bitbucket-test-env-load-respects-existing ()
  "By default an already-set variable is not overwritten."
  (let ((file (make-temp-file "bbenv" nil ".sh"
                              "export BITBUCKET_WORKSPACE=fromfile\n")))
    (unwind-protect
        (progn
          (setenv "BITBUCKET_WORKSPACE" "preset")
          (let ((bitbucket-env-overwrite nil))
            (should (null (bitbucket-env-load file)))
            (should (equal (getenv "BITBUCKET_WORKSPACE") "preset")))
          (let ((bitbucket-env-overwrite t))
            (should (member "BITBUCKET_WORKSPACE" (bitbucket-env-load file)))
            (should (equal (getenv "BITBUCKET_WORKSPACE") "fromfile"))))
      (setenv "BITBUCKET_WORKSPACE" nil)
      (delete-file file))))

(ert-deftest bitbucket-test-env-load-missing-file-noop ()
  (should (null (bitbucket-env-load "/no/such/rc/file/here"))))

(ert-deftest bitbucket-test-env-parse-imports-both-platforms ()
  "The default prefix list covers both backends' variables.
GUI Emacs never sources ~/.zshrc, so a GitHub token exported there has
to be imported the same way the Bitbucket ones are -- otherwise
switching `git-platform-default-backend' to `github' leaves the client
unauthenticated for no visible reason."
  (should (equal
           (bitbucket-env--parse
            (concat "export BITBUCKET_WORKSPACE=acme\n"
                    "export GITHUB_TOKEN=ghp_secret\n"
                    "export OTHER_VAR=ignored\n"))
           '(("BITBUCKET_WORKSPACE" . "acme")
             ("GITHUB_TOKEN" . "ghp_secret")))))

(ert-deftest bitbucket-test-env-parse-accepts-a-bare-prefix-string ()
  "A single string still works, so existing configs keep behaving."
  (let ((bitbucket-env-prefix "GITHUB"))
    (should (equal (bitbucket-env--parse
                    (concat "export BITBUCKET_WORKSPACE=acme\n"
                            "export GITHUB_TOKEN=ghp_secret\n"))
                   '(("GITHUB_TOKEN" . "ghp_secret"))))))

(ert-deftest bitbucket-test-env-prefix-does-not-match-substrings ()
  "A prefix anchors at the start of the name, never mid-word."
  (should-not (bitbucket-env--parse "export MY_GITHUB_TOKEN=nope\n")))

(provide 'bitbucket-env-test)
;;; bitbucket-env-test.el ends here
