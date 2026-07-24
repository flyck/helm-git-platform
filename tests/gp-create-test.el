;;; gp-create-test.el --- Tests for the PR-creation mask -*- lexical-binding: t; -*-

;;; Commentary:
;; Drives the pure pieces of `gp-create': title derivation from commit
;; messages, body building, and the mask template/parse round-trip.  No
;; git or network is touched.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gp-create)

;;;; Common prefix / title derivation -----------------------------------------

(ert-deftest gp-create-test-common-prefix-shared-scope ()
  "A shared conventional-commit scope is the common denominator."
  (should (equal (gp-create--common-prefix
                  '("feat(api): add x" "feat(api): drop y"))
                 "feat(api)")))

(ert-deftest gp-create-test-common-prefix-trims-separators ()
  "Trailing separators are trimmed off the shared prefix."
  (should (equal (gp-create--common-prefix '("auth: add x" "auth: drop y"))
                 "auth")))

(ert-deftest gp-create-test-common-prefix-none ()
  "Disjoint messages have no usable common prefix."
  (should (null (gp-create--common-prefix '("add login" "remove cache")))))

(ert-deftest gp-create-test-title-single-commit ()
  "One commit: the title is that commit's summary, verbatim-trimmed."
  (should (equal (gp-create--derive-title '("Fix the thing") "feature/x")
                 "Fix the thing")))

(ert-deftest gp-create-test-title-common-denominator ()
  "Several commits sharing a scope use that scope as the title."
  (should (equal (gp-create--derive-title
                  '("payments: add stripe" "payments: handle refunds")
                  "feature/payments")
                 "payments")))

(ert-deftest gp-create-test-title-falls-back-to-branch ()
  "With no meaningful shared prefix, the branch name is humanised."
  (should (equal (gp-create--derive-title
                  '("add login" "remove cache")
                  "feature/user-auth")
                 "User auth")))

(ert-deftest gp-create-test-title-no-commits ()
  "No commits at all: humanise the branch."
  (should (equal (gp-create--derive-title nil "fix/broken-thing")
                 "Broken thing")))

(ert-deftest gp-create-test-humanise-branch-empty ()
  "An empty/nil branch yields a sensible default title."
  (should (equal (gp-create--humanise-branch nil) "New pull request"))
  (should (equal (gp-create--humanise-branch "") "New pull request")))

;;;; Body ---------------------------------------------------------------------

(ert-deftest gp-create-test-body-bullets ()
  "The description is a bullet list of commit summaries, in order."
  (should (equal (gp-create--body '("add x" "add y"))
                 "- add x\n- add y")))

(ert-deftest gp-create-test-body-empty ()
  "No commits: an empty description."
  (should (equal (gp-create--body nil) "")))

;;;; Create request body ------------------------------------------------------

(ert-deftest gp-create-test-request-body-options ()
  "Draft, close-source-branch and reviewers all reach the POST body."
  (require 'bitbucket-api)
  (let (captured)
    (cl-letf (((symbol-function 'bitbucket-api-request)
               (lambda (_m _p &optional _params data)
                 (setq captured data) '((id . 7)))))
      (bitbucket-create-pull-request
       "ws/slug" "feature/x" "main" "T"
       :description "body" :draft t :close-source-branch t
       :reviewer-uuids '("{uuid-a}" "{uuid-b}")))
    (should (equal (alist-get 'title captured) "T"))
    (should (eq (alist-get 'draft captured) t))
    (should (eq (alist-get 'close_source_branch captured) t))
    (should (equal (alist-get 'reviewers captured)
                   '(((uuid . "{uuid-a}")) ((uuid . "{uuid-b}")))))))

(ert-deftest gp-create-test-request-body-minimal ()
  "Unset options are omitted entirely (no close_source_branch/reviewers keys)."
  (require 'bitbucket-api)
  (let (captured)
    (cl-letf (((symbol-function 'bitbucket-api-request)
               (lambda (_m _p &optional _params data)
                 (setq captured data) '((id . 7)))))
      (bitbucket-create-pull-request "ws/slug" "feature/x" "main" "T"))
    (should (null (assq 'draft captured)))
    (should (null (assq 'close_source_branch captured)))
    (should (null (assq 'reviewers captured)))
    (should (equal (alist-get 'destination captured)
                   '((branch . ((name . "main"))))))))

;;;; Default-reviewers caching ------------------------------------------------

(ert-deftest gp-create-test-default-reviewers-empty-not-cached ()
  "An empty/failed reviewer fetch is not cached; a later call re-fetches."
  (require 'bitbucket-api)
  (let ((bitbucket-cache-ttl 300)
        (calls 0))
    (bitbucket-cache-clear)
    (cl-letf (((symbol-function 'bitbucket-api-paged)
               (lambda (&rest _)
                 (cl-incf calls)
                 ;; first call: empty (transient); second: real data
                 (if (= calls 1) nil
                   '(((uuid . "{u1}") (display_name . "Alice")))))))
      (should (null (bitbucket-repo-default-reviewers "ws/slug")))
      ;; not cached -> the second call actually re-fetches and now succeeds
      (should (equal (bitbucket-repo-default-reviewers "ws/slug")
                     '(((uuid . "{u1}") (display_name . "Alice")))))
      (should (= calls 2))
      ;; now it IS cached -> no third fetch
      (should (equal (bitbucket-repo-default-reviewers "ws/slug")
                     '(((uuid . "{u1}") (display_name . "Alice")))))
      (should (= calls 2)))))

;;;; Default vs. suggested reviewer checkboxes ---------------------------------

(ert-deftest gp-create-test-default-reviewers-checked-by-default ()
  "Default reviewers get a checked checkbox."
  (require 'wid-edit)
  (cl-letf (((symbol-function 'gp-repo-default-reviewers)
             (lambda (_) '(((uuid . "alice") (display_name . "Alice")))))
            ((symbol-function 'gp-repo-suggested-reviewers)
             (lambda (_) nil)))
    (with-temp-buffer
      (let ((alist (gp-create--insert-reviewers "acme/web")))
        (should (equal (mapcar #'car alist) '("alice")))
        (should (widget-value (cdr (assoc "alice" alist))))))))

(ert-deftest gp-create-test-suggested-reviewers-unchecked-by-default ()
  "Suggested reviewers (GitHub collaborators) get an unchecked checkbox."
  (require 'wid-edit)
  (cl-letf (((symbol-function 'gp-repo-default-reviewers)
             (lambda (_) nil))
            ((symbol-function 'gp-repo-suggested-reviewers)
             (lambda (_) '(((uuid . "bob") (display_name . "Bob"))))))
    (with-temp-buffer
      (let ((alist (gp-create--insert-reviewers "acme/web")))
        (should (equal (mapcar #'car alist) '("bob")))
        (should-not (widget-value (cdr (assoc "bob" alist))))))))

(ert-deftest gp-create-test-default-and-suggested-reviewers-combined ()
  "Both groups render together, defaults checked and suggestions not,
and `gp-create--selected-reviewer-uuids' only picks up checked ones."
  (require 'wid-edit)
  (cl-letf (((symbol-function 'gp-repo-default-reviewers)
             (lambda (_) '(((uuid . "alice") (display_name . "Alice")))))
            ((symbol-function 'gp-repo-suggested-reviewers)
             (lambda (_) '(((uuid . "bob") (display_name . "Bob"))))))
    (with-temp-buffer
      (setq-local gp-create--w-reviewers (gp-create--insert-reviewers "acme/web"))
      (should (equal (mapcar #'car gp-create--w-reviewers) '("alice" "bob")))
      (should (equal (gp-create--selected-reviewer-uuids) '("alice"))))))

(ert-deftest gp-create-test-no-reviewers-blank-section ()
  "Neither defaults nor suggestions: a friendly empty message, no widgets."
  (require 'wid-edit)
  (cl-letf (((symbol-function 'gp-repo-default-reviewers) (lambda (_) nil))
            ((symbol-function 'gp-repo-suggested-reviewers) (lambda (_) nil)))
    (with-temp-buffer
      (should (null (gp-create--insert-reviewers "acme/web")))
      (should (string-match-p "no default or suggested reviewers"
                              (buffer-string))))))

(provide 'gp-create-test)
;;; gp-create-test.el ends here
