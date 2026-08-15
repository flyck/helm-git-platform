;;; bitbucket-mock.el --- Centralized mock Bitbucket service -*- lexical-binding: t; -*-

;;; Commentary:

;; A single, centralized fake Bitbucket service used by the test suite.
;;
;; The real package routes ALL network access through
;; `bitbucket-api-request'.  Here we provide `bitbucket-mock-request',
;; a drop-in replacement that dispatches on the request PATH and returns
;; canned JSON loaded from tests/fixtures/ (captured from the live API).
;;
;; Usage in a test:
;;
;;   (bitbucket-mock-with-service
;;     (bitbucket-workspace-pull-requests))   ;; hits the mock, no network
;;
;; The macro rebinds `bitbucket-api-request' and the diff helper, seeds
;; credentials/workspace, and tracks every call in `bitbucket-mock-calls'
;; so tests can assert on what was requested.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'bitbucket-api)

(defconst bitbucket-mock-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this file, used to locate fixtures.")

(defvar bitbucket-mock-calls nil
  "List of (METHOD PATH PARAMS DATA) recorded during a mocked session.
Newest first (pushed).")

(defvar bitbucket-mock-overrides nil
  "Alist of (PATH-REGEXP . RESPONSE) overriding the default fixtures.
RESPONSE may be a parsed Lisp object or a function of (method path
params data) returning one.  Consulted before the built-in routes.")

(defun bitbucket-mock--fixture (name)
  "Read and parse fixtures/NAME (a JSON file).
The fixture's `next' pagination link is stripped: the mock serves a
single page, so a caller following `next' would otherwise loop on
the same fixture forever.  Multi-page behaviour is tested via
`bitbucket-mock-overrides'."
  (let ((file (expand-file-name (format "fixtures/%s" name) bitbucket-mock-dir)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((parsed (bitbucket--parse-json (buffer-string))))
        (when (assq 'next parsed)
          (setq parsed (assq-delete-all 'next parsed)))
        parsed))))

(defun bitbucket-mock-request (method path &optional params data)
  "Mock implementation of `bitbucket-api-request'.
Dispatches on PATH to a fixture.  Records the call and honours
`bitbucket-mock-overrides'."
  (push (list method path params data) bitbucket-mock-calls)
  (or
   ;; 1. user-supplied overrides
   (cl-loop for (re . resp) in bitbucket-mock-overrides
            when (string-match-p re path)
            return (if (functionp resp)
                       (funcall resp method path params data)
                     resp))
   ;; 2. built-in routes (order matters: most specific first)
   (cond
    ;; resolve / reopen a comment
    ((string-match-p "/comments/[0-9]+/resolve\\'" path)
     (if (equal method "DELETE") nil '((user (display_name . "Me")))))
    ;; create a comment (POST .../comments) -> echo it back with an id
    ((and (equal method "POST")
          (string-match-p "/pullrequests/[0-9]+/comments\\'" path))
     (append '((id . 99999)) data))
    ((string-match-p "/pullrequests/[0-9]+/comments" path)
     (bitbucket-mock--fixture "pr-comments.json"))
    ((string-match-p "/workspaces/[^/]+/pullrequests/" path)
     (bitbucket-mock--fixture "workspace-prs.json"))
    ((string-match-p "/pullrequests/[0-9]+\\'" path)
     ;; a single PR object: reuse the first list entry, fleshed out
     (car (alist-get 'values (bitbucket-mock--fixture "workspace-prs.json"))))
    ((string-suffix-p "/user" path)
     (bitbucket-mock--fixture "user.json"))
    ((string-match-p "/users/" path)
     `((display_name . ,(format "User %s" (car (last (split-string path "/" t)))))))
    ;; pipelines: steps, stop, trigger, list (most specific first)
    ((string-match-p "/pipelines/[^/]+/steps" path)
     (bitbucket-mock--fixture "pipeline-steps.json"))
    ((string-match-p "/pipelines/[^/]+/stopPipeline\\'" path)
     '((status . "stopped")))
    ((and (equal method "POST") (string-match-p "/pipelines/?\\'" path))
     ;; trigger -> echo a freshly-created pipeline
     (append '((uuid . "{pipeline-new}") (build_number . 99)
               (state (name . "PENDING")))
             data))
    ((string-match-p "/pipelines/?\\'" path)
     (bitbucket-mock--fixture "pipelines.json"))
    ;; commit lookup (for pipeline recent-run summaries)
    ((string-match-p "/commit/[0-9a-f]+\\'" path)
     '((message . "Fix the widget toggle\n\nlonger body ignored")))
    (t (error "bitbucket-mock: no route for %s %s" method path)))))

(defun bitbucket-mock-diff (_full-name _id)
  "Mock diff: return a tiny but valid unified diff."
  (concat "diff --git a/file.txt b/file.txt\n"
          "--- a/file.txt\n+++ b/file.txt\n"
          "@@ -1,1 +1,1 @@\n-old\n+new\n"))

(defun bitbucket-mock-get-async (path params callback)
  "Mock implementation of `bitbucket-api-get-async'.
Serves the same fixtures as `bitbucket-mock-request' and calls
CALLBACK immediately -- the tests stay synchronous, so a caller that
assumed \"async means later\" would fail here, which is deliberate:
the live code must tolerate an immediate callback (a warm
commit-message cache does exactly that)."
  (funcall callback (ignore-errors (bitbucket-mock-request "GET" path params))))

(defun bitbucket-mock-paged-async (path &optional params callback max-items)
  "Mock implementation of `bitbucket-api-paged-async'.
Serves one fixture page and reports (OK VALUES), honouring MAX-ITEMS."
  (let* ((page (ignore-errors (bitbucket-mock-request "GET" path params)))
         (values (alist-get 'values page)))
    (when (and max-items values (> (length values) max-items))
      (setq values (seq-take values max-items)))
    (funcall callback (and page t) values)))

(defmacro bitbucket-mock-with-service (&rest body)
  "Run BODY with the Bitbucket API replaced by the mock service.
Seeds credentials and workspace, clears caches and the call log.
Both the synchronous choke-point (`bitbucket-api-request') and the
async ones are mocked, so async callers hit fixtures too."
  (declare (indent 0) (debug t))
  `(let ((bitbucket-user-email "ada@example.com")
         (bitbucket-api-token "mock-token")
         (bitbucket-workspace "acme")
         (bitbucket--uuid-cache nil)
         (bitbucket-mock-calls nil))
     (clrhash bitbucket--mention-cache)
     (cl-letf (((symbol-function 'bitbucket-api-request) #'bitbucket-mock-request)
               ((symbol-function 'bitbucket-api-get-async) #'bitbucket-mock-get-async)
               ((symbol-function 'bitbucket-api-paged-async) #'bitbucket-mock-paged-async)
               ((symbol-function 'bitbucket-pull-request-diff) #'bitbucket-mock-diff))
       ,@body)))

(provide 'bitbucket-mock)
;;; bitbucket-mock.el ends here
