;;; github-mock.el --- Centralized mock GitHub service -*- lexical-binding: t; -*-

;;; Commentary:

;; A single, centralized fake GitHub service used by the test suite, in
;; the same spirit as tests/bitbucket-mock.el.
;;
;; The real client routes all REST access through `github-api-request'
;; (and paged listing through `github-api-paged'); GraphQL goes through
;; `github-graphql-request'.  Here we provide drop-in replacements that
;; dispatch on method/path and return small, hand-written GitHub-shaped
;; fixtures (there is no captured live-API fixture set to reuse, unlike
;; Bitbucket's file-based fixtures).
;;
;; Usage in a test:
;;
;;   (github-mock-with-service
;;     (github-pull-request "acme/web" 42))   ;; hits the mock, no network

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'github-api)

(defvar github-mock-calls nil
  "List of (METHOD PATH PARAMS DATA) recorded during a mocked session.
Newest first (pushed).")

(defvar github-mock-graphql-calls nil
  "List of (QUERY VARIABLES) recorded during a mocked session.")

(defvar github-mock-overrides nil
  "Alist of (PATH-REGEXP . RESPONSE) overriding the default fixtures.
RESPONSE may be a parsed Lisp object or a function of (method path
params data) returning one.  Consulted before the built-in routes.")

(defconst github-mock--user
  '((login . "ada") (id . 1) (node_id . "U_ada")))

(defconst github-mock--pr-1
  '((id . 501) (number . 42) (node_id . "PR_42")
    (title . "Add the widget toggle") (state . "open") (draft . nil)
    (body . "Adds the toggle.\n\n- [x] tests\n- [ ] docs")
    (user (login . "ada") (avatar_url . "https://example.com/ada.png"))
    (head (ref . "feature/widget") (sha . "abc123") (repo (full_name . "acme/web")))
    (base (ref . "main") (repo (full_name . "acme/web")))
    (requested_reviewers . [])
    (labels . [((id . 1) (name . "bug") (color . "d73a4a"))
               ((id . 2) (name . "ui") (color . "a2eeef"))])
    (additions . 12) (deletions . 3) (changed_files . 2) (commits . 1)
    (html_url . "https://github.com/acme/web/pull/42")))

(defconst github-mock--pr-2
  '((id . 502) (number . 43) (node_id . "PR_43")
    (title . "Fix the flaky test") (state . "open") (draft . t)
    (user (login . "bea") (avatar_url . "https://example.com/bea.png"))
    (head (ref . "fix/flaky") (sha . "def456") (repo (full_name . "acme/web")))
    (base (ref . "main") (repo (full_name . "acme/web")))
    (requested_reviewers . [])
    (additions . 4) (deletions . 1) (changed_files . 1) (commits . 1)
    (html_url . "https://github.com/acme/web/pull/43")))

(defconst github-mock--repo-labels
  '[((id . 1) (name . "bug") (color . "d73a4a"))
    ((id . 2) (name . "ui") (color . "a2eeef"))
    ((id . 3) (name . "chore") (color . "cfd3d7"))]
  "The repo's whole label pool, as GET /repos/:o/:r/labels returns it.
A superset of any one PR's labels, so tests can add one that is in the
pool but not yet on the PR.")

(defconst github-mock--search-hit
  '((number . 42) (repository_url . "https://api.github.com/repos/acme/web")))

(defconst github-mock--issue-comment
  '((id . 9001) (body . "General comment")
    (user (login . "bea") (avatar_url . "https://example.com/bea.png"))
    (created_at . "2026-01-01T00:00:00Z")
    (html_url . "https://github.com/acme/web/pull/42#issuecomment-9001")))

(defconst github-mock--review-comment
  '((id . 9002) (body . "Inline comment") (path . "widget.el") (line . 10)
    (user (login . "bea") (avatar_url . "https://example.com/bea.png"))
    (created_at . "2026-01-01T00:05:00Z")
    (html_url . "https://github.com/acme/web/pull/42#discussion_r9002")))

(defvar github-mock--reactions nil
  "Reaction rows the mock is holding: (ID PATH CONTENT LOGIN).
Stateful so an add/remove round-trip can be driven through the real
`github-add-comment-reaction' / `github-remove-comment-reaction'.")

(defvar github-mock--next-reaction-id 5000)

(defun github-mock--reaction-list (path)
  "Return the reactions the mock holds for PATH, GitHub's shape."
  (mapcar (lambda (r)
            (pcase-let ((`(,id ,_p ,content ,login) r))
              `((id . ,id) (content . ,content)
                (user . ((login . ,login))))))
          (seq-filter (lambda (r) (equal (nth 1 r) path))
                      (reverse github-mock--reactions))))

(defun github-mock-request (method path &optional params data extra-headers)
  "Mock implementation of `github-api-request'."
  (push (list method path params data) github-mock-calls)
  (ignore extra-headers)
  (or
   (cl-loop for (re . resp) in github-mock-overrides
            when (string-match-p re path)
            return (if (functionp resp) (funcall resp method path params data) resp))
   (cond
    ((string-suffix-p "/user" path) github-mock--user)
    ;; reactions: POST adds (idempotent), DELETE removes by reaction id
    ((and (equal method "POST") (string-match-p "/reactions\\'" path))
     (let* ((content (alist-get 'content data))
            (login (alist-get 'login github-mock--user))
            (existing (seq-find (lambda (r)
                                  (and (equal (nth 1 r) path)
                                       (equal (nth 2 r) content)
                                       (equal (nth 3 r) login)))
                                github-mock--reactions)))
       (or (and existing `((id . ,(car existing)) (content . ,content)
                           (user . ((login . ,login)))))
           (let ((id (cl-incf github-mock--next-reaction-id)))
             (push (list id path content login) github-mock--reactions)
             `((id . ,id) (content . ,content) (user . ((login . ,login))))))))
    ((and (equal method "DELETE") (string-match-p "/reactions/[0-9]+\\'" path))
     (let ((id (string-to-number (car (last (split-string path "/"))))))
       (setq github-mock--reactions
             (seq-remove (lambda (r) (equal (car r) id)) github-mock--reactions))
       t))
    ((and (equal method "POST") (string-match-p "/pulls/[0-9]+/reviews\\'" path))
     `((id . 7001) (user . ,github-mock--user) (state . ,(alist-get 'event data))))
    ((string-match-p "/pulls/[0-9]+/reviews\\'" path) [])
    ((string-match-p "/pulls/[0-9]+\\'" path) github-mock--pr-1)
    ((and (equal method "POST") (string-match-p "/issues/[0-9]+/comments\\'" path))
     (append `((id . 9099) (created_at . "2026-01-01T00:10:00Z")) data))
    ((and (equal method "POST") (string-match-p "/pulls/[0-9]+/comments\\'" path))
     (append `((id . 9098) (created_at . "2026-01-01T00:11:00Z")) data))
    ((and (equal method "PUT") (string-match-p "/issues/[0-9]+/labels\\'" path))
     ;; GitHub echoes back the resulting label set
     (vconcat (mapcar (lambda (n) `((name . ,n) (color . "ededed")))
                      (append (alist-get 'labels data) nil))))
    ((string-match-p "/repos/[^/]+/[^/]+\\'" path) '((default_branch . "main")))
    ((string-match-p "/commits/[^/]+/status\\'" path) '((state . "success")))
    ((and (equal method "PUT") (string-match-p "/merge\\'" path))
     '((sha . "deadbeef") (merged . t) (message . "Pull Request successfully merged")))
    (t (error "github-mock: no route for %s %s" method path)))))

(defun github-mock-paged (path &optional params max-items)
  "Mock implementation of `github-api-paged'; ignores real pagination."
  (push (list "GET" path params nil) github-mock-calls)
  (ignore max-items)
  (cond
   ((string-match-p "/reactions\\'" path) (github-mock--reaction-list path))
   ((string-match-p "\\`/search/issues\\'" path) (list github-mock--search-hit))
   ((string-match-p "/pulls/[0-9]+/reviews\\'" path) nil)
   ((string-match-p "/issues/[0-9]+/comments\\'" path) (list github-mock--issue-comment))
   ((string-match-p "/pulls/[0-9]+/comments\\'" path) (list github-mock--review-comment))
   ((string-match-p "/pulls\\'" path) (list github-mock--pr-1 github-mock--pr-2))
   ((string-match-p "/actions/runs\\'" path) nil)
   ((string-match-p "/labels\\'" path) (append github-mock--repo-labels nil))
   ((string-match-p "/collaborators\\'" path)
    (list '((login . "ada") (name . "Ada Lovelace"))
          '((login . "bea") (name . nil))))
   ((string-match-p "/pulls/[0-9]+/files\\'" path)
    (list '((filename . "widget.el") (status . "modified") (additions . 4) (deletions . 1))))
   (t (error "github-mock: no paged route for %s" path))))

(defun github-mock-graphql (query &optional variables)
  "Mock implementation of `github-graphql-request'."
  (push (list query variables) github-mock-graphql-calls)
  (cond
   ((string-match-p "reviewThreads" query)
    '((repository (pullRequest (reviewThreads (nodes . [])))) ))
   (t '((ok . t)))))

(defmacro github-mock-with-service (&rest body)
  "Run BODY with the GitHub API replaced by the mock service.
Seeds a token, clears caches and the call log."
  (declare (indent 0) (debug t))
  `(let ((github-api-token "mock-token")
         (github--login-cache nil)
         (github-mock-calls nil)
         (github-mock-graphql-calls nil)
         ;; reaction state is per-test: a leftover row would make the next
         ;; test's "did I already react" answer depend on run order
         (github-mock--reactions nil)
         (github-mock--next-reaction-id 5000)
         (github--resolved-thread-comment-ids (make-hash-table :test 'eql))
         (github--thread-id-cache (make-hash-table :test 'equal))
         ;; `github-repo-labels' caches through the shared `gp-cache-*' table;
         ;; rebind it so a pool cached by one test can't answer another's fetch
         (gp--result-cache (make-hash-table :test 'equal)))
     (cl-letf (((symbol-function 'github-api-request) #'github-mock-request)
               ((symbol-function 'github-api-paged) #'github-mock-paged)
               ((symbol-function 'github-graphql-request) #'github-mock-graphql))
       ,@body)))

(provide 'github-mock)
;;; github-mock.el ends here
