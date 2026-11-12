;;; gp-create-test.el --- Tests for the PR-creation mask -*- lexical-binding: t; -*-

;;; Commentary:
;; Drives the pure pieces of `gp-create': title derivation from commit
;; messages, body building, and the mask template/parse round-trip.  No
;; git or network is touched.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gp-create)
;; Not just for the functions: the caching tests `let'-bind
;; `bitbucket-cache-ttl', and a runtime-only `require' inside a test body
;; leaves the compiler treating that as an unused lexical variable (and
;; every bitbucket-* call as an unknown function).
(require 'bitbucket-api)
(require 'bitbucket-mock)

;;;; Common prefix / title derivation -----------------------------------------

(ert-deftest gp-create-test-common-prefix-shared-scope ()
  "A shared conventional-commit scope is the common denominator."
  (should (equal (gp-create--common-prefix
                  '("feat(api): add x" "feat(api): drop y"))
                 "feat(api)")))

(ert-deftest gp-create-test-common-prefix-none ()
  "Disjoint messages have no usable common prefix."
  (should (null (gp-create--common-prefix '("add login" "remove cache")))))

(ert-deftest gp-create-test-title-single-commit ()
  "One commit: the title is that commit's summary, verbatim-trimmed."
  (should (equal (gp-create--derive-title '("Fix the thing") "feature/x")
                 "Fix the thing")))

(ert-deftest gp-create-test-title-common-denominator ()
  "Several commits sharing a scope use that scope as the title, with the
trailing separator trimmed off the shared prefix."
  (should (equal (gp-create--common-prefix
                  '("payments: add stripe" "payments: handle refunds"))
                 "payments"))
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

;;;; Destination-branch candidates ---------------------------------------------

(ert-deftest gp-create-test-dest-candidates-default-and-dest-lead ()
  "The default branch and the current destination sort to the front,
the rest stays alphabetical."
  (should (equal (gp-create--dest-candidates
                  '("develop" "main" "release/1.0" "staging") "main" "staging")
                 '("main" "staging" "develop" "release/1.0"))))

(ert-deftest gp-create-test-dest-candidates-dedup-when-default-is-dest ()
  "DEFAULT and DEST are usually the same branch; it must not repeat."
  (should (equal (gp-create--dest-candidates '("develop" "main") "main" "main")
                 '("main" "develop"))))

(ert-deftest gp-create-test-dest-candidates-empty-branch-list ()
  "A failed/empty branch fetch still surfaces DEFAULT/DEST alone."
  (should (equal (gp-create--dest-candidates nil "main" "main") '("main"))))

(ert-deftest gp-create-test-dest-candidates-nil-default ()
  "No default branch known (e.g. the API call failed): DEST still leads."
  (should (equal (gp-create--dest-candidates '("develop" "main") nil "main")
                 '("main" "develop"))))

;;;; Create request body ------------------------------------------------------

(defun gp-create-test--post-pr-route (_method _path _params data)
  "Mock answer to the create-PR POST: echo DATA back with an id."
  (append '((id . 7)) data))

(defun gp-create-test--create-posts ()
  "The create-PR POSTs recorded by the Bitbucket mock, oldest first."
  (reverse
   (cl-remove-if-not
    (lambda (c) (and (equal (nth 0 c) "POST")
                     (string-match-p "/repositories/ws/slug/pullrequests\\'"
                                     (nth 1 c))))
    bitbucket-mock-calls)))

(ert-deftest gp-create-test-request-body-options ()
  "Through the backend protocol, every form field reaches the POST body:
title, source, destination, description, draft, close-source-branch
and reviewers."
  (bitbucket-mock-with-service
    (let ((bitbucket-mock-overrides
           `(("/repositories/ws/slug/pullrequests\\'"
              . gp-create-test--post-pr-route))))
      (should (equal (alist-get 'id (gp-create-pull-request
                                     "ws/slug" "feature/x" "main" "T" "body"
                                     t t '("{uuid-a}" "{uuid-b}")))
                     7))
      (let* ((posts (gp-create-test--create-posts))
             (body (nth 3 (car posts))))
        (should (= (length posts) 1))
        (should (equal (alist-get 'title body) "T"))
        (should (equal (alist-get 'source body)
                       '((branch . ((name . "feature/x"))))))
        (should (equal (alist-get 'destination body)
                       '((branch . ((name . "main"))))))
        (should (equal (alist-get 'description body) "body"))
        (should (eq (alist-get 'draft body) t))
        (should (eq (alist-get 'close_source_branch body) t))
        (should (equal (alist-get 'reviewers body)
                       '(((uuid . "{uuid-a}")) ((uuid . "{uuid-b}")))))))))

(ert-deftest gp-create-test-request-body-minimal ()
  "Unset options are omitted entirely: an empty description (what the
form's trimmed blank field yields), no draft, no close_source_branch
and no reviewers keys -- but source and destination are always sent."
  (bitbucket-mock-with-service
    (let ((bitbucket-mock-overrides
           `(("/repositories/ws/slug/pullrequests\\'"
              . gp-create-test--post-pr-route))))
      (gp-create-pull-request "ws/slug" "feature/x" "main" "T" "" nil nil nil)
      (let* ((posts (gp-create-test--create-posts))
             (body (nth 3 (car posts))))
        (should (= (length posts) 1))
        (should (equal (alist-get 'title body) "T"))
        (should (equal (alist-get 'source body)
                       '((branch . ((name . "feature/x"))))))
        (should (equal (alist-get 'destination body)
                       '((branch . ((name . "main"))))))
        (should-not (assq 'description body))
        (should-not (assq 'draft body))
        (should-not (assq 'close_source_branch body))
        (should-not (assq 'reviewers body))))))

;;;; Default-reviewers caching ------------------------------------------------

(defun gp-create-test--count-calls (regexp)
  "Number of mock requests whose path matches REGEXP."
  (cl-count-if (lambda (c) (string-match-p regexp (nth 1 c)))
               bitbucket-mock-calls))

(ert-deftest gp-create-test-default-reviewers-empty-not-cached ()
  "An empty/failed reviewer fetch is not cached; a later call re-fetches,
and once it returns reviewers those are cached."
  (bitbucket-mock-with-service
    (let* ((bitbucket-cache-ttl 300)
           (n 0)
           (bitbucket-mock-overrides
            `(("/repositories/ws/slug/default-reviewers"
               . ,(lambda (&rest _)
                    (cl-incf n)
                    ;; first fetch: empty (transient); later: real data
                    (if (= n 1) '((values))
                      '((values ((uuid . "{u1}") (display_name . "Alice"))))))))))
      (should (null (bitbucket-repo-default-reviewers "ws/slug")))
      (should (equal (bitbucket-repo-default-reviewers "ws/slug")
                     '(((uuid . "{u1}") (display_name . "Alice")))))
      (should (= (gp-create-test--count-calls "/default-reviewers") 2))
      ;; now it IS cached -> no third request
      (should (equal (bitbucket-repo-default-reviewers "ws/slug")
                     '(((uuid . "{u1}") (display_name . "Alice")))))
      (should (= (gp-create-test--count-calls "/default-reviewers") 2)))))

;;;; Bitbucket suggested reviewers (workspace members) -------------------------

(defconst gp-create-test--me "{21d7839d-779f-44b2-8c40-6f43ac90be06}"
  "The uuid of the mock's authenticated user (fixtures/user.json).")

(defun gp-create-test--members-route (_method path &rest _)
  "Two pages of workspace members; the first links to the second.
Page 1 holds the authenticated user and Alice, page 2 holds Bob."
  (if (string-match-p "page=2" path)
      '((values ((user . ((uuid . "{u2}") (display_name . "Bob"))))))
    `((values ((user . ((uuid . ,gp-create-test--me) (display_name . "Me"))))
              ((user . ((uuid . "{u1}") (display_name . "Alice")))))
      (next . "https://api.bitbucket.org/2.0/workspaces/ws/members?page=2"))))

(ert-deftest gp-create-test-bitbucket-suggested-unwraps-members ()
  "Workspace members arrive nested under `user' and across pages;
candidates are flat, in order, without yourself."
  (bitbucket-mock-with-service
    (let ((bitbucket-mock-overrides
           '(("/default-reviewers" . ((values)))
             ("/workspaces/ws/members" . gp-create-test--members-route))))
      (should (equal (bitbucket-repo-suggested-reviewers "ws/slug")
                     '(((uuid . "{u1}") (display_name . "Alice"))
                       ((uuid . "{u2}") (display_name . "Bob")))))
      ;; both pages were actually requested
      (should (= (gp-create-test--count-calls "/workspaces/ws/members") 2)))))

(ert-deftest gp-create-test-bitbucket-suggested-excludes-self-and-defaults ()
  "You are never your own reviewer, and defaults are not offered twice."
  (bitbucket-mock-with-service
    (let ((bitbucket-mock-overrides
           '(("/repositories/ws/slug/default-reviewers"
              . ((values ((uuid . "{u1}") (display_name . "Alice")))))
             ("/workspaces/ws/members" . gp-create-test--members-route))))
      (should (equal (bitbucket-repo-suggested-reviewers "ws/slug")
                     '(((uuid . "{u2}") (display_name . "Bob"))))))))

(ert-deftest gp-create-test-bitbucket-suggested-survives-api-failure ()
  "A members fetch the token cannot make must not break the create form,
and the failure is not cached: the next call asks again."
  (bitbucket-mock-with-service
    (let ((bitbucket-mock-overrides
           `(("/default-reviewers" . ((values)))
             ("/workspaces/ws/members"
              . ,(lambda (&rest _) (error "403 Forbidden"))))))
      (should (null (bitbucket-repo-suggested-reviewers "ws/slug")))
      (should (null (bitbucket-repo-suggested-reviewers "ws/slug")))
      (should (= (gp-create-test--count-calls "/workspaces/ws/members") 2)))))

;;;; Preferred reviewers -------------------------------------------------------

(ert-deftest gp-create-test-preferred-matches-uuid-name-and-nickname ()
  "A preferred entry matches on uuid, display name or nickname."
  (let ((r '((uuid . "{abc}") (display_name . "Michael Gertz")
             (nickname . "mgertz"))))
    (let ((gp-create-preferred-reviewers '("{abc}")))
      (should (gp-create--preferred-p r)))
    (let ((gp-create-preferred-reviewers '("Michael Gertz")))
      (should (gp-create--preferred-p r)))
    (let ((gp-create-preferred-reviewers '("mgertz")))
      (should (gp-create--preferred-p r)))
    ;; names are matched case-insensitively, uuids exactly
    (let ((gp-create-preferred-reviewers '("michael gertz")))
      (should (gp-create--preferred-p r)))
    (let ((gp-create-preferred-reviewers '("{ABC}")))
      (should-not (gp-create--preferred-p r)))
    (let ((gp-create-preferred-reviewers '("Someone Else")))
      (should-not (gp-create--preferred-p r)))
    (let ((gp-create-preferred-reviewers nil))
      (should-not (gp-create--preferred-p r)))))

(ert-deftest gp-create-test-preferred-reviewer-is-pre-checked ()
  "A preferred reviewer is checked even in the unchecked (suggested) group."
  (let ((gp-create-preferred-reviewers '("{keep}"))
        (reviewers '(((uuid . "{keep}") (display_name . "Keep Me"))
                     ((uuid . "{skip}") (display_name . "Skip Me")))))
    (with-temp-buffer
      (let ((widgets (gp-create--insert-reviewer-list reviewers nil)))
        (should (widget-value (cdr (assoc "{keep}" widgets))))
        (should-not (widget-value (cdr (assoc "{skip}" widgets))))
        ;; and the row says why it came ticked
        (should (string-match-p "Keep Me *★" (buffer-string)))))))

(provide 'gp-create-test)
;;; gp-create-test.el ends here
