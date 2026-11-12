;;; github-api.el --- GitHub REST/GraphQL API layer -*- lexical-binding: t; -*-

;; Author: Felix Brilej
;; Keywords: tools, vc
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Thin, testable layer over the GitHub REST API (v3) and, for the one
;; operation REST cannot do (resolving a review comment thread), the
;; GraphQL v4 API.
;;
;; All REST access funnels through `github-api-request', which can be
;; rebound (e.g. with `cl-letf' in tests) to a mock service so the
;; higher-level functions here and in `git-platform-github.el' can be
;; unit tested without touching the network.  See tests/github-mock.el.
;;
;; Credentials are read from the environment by default (GITHUB_TOKEN),
;; so they can live in ~/.zshrc, but every value can be overridden via
;; customize.  Unlike Bitbucket, a token is not mandatory: unauthenticated
;; GET requests against public repos work (subject to GitHub's lower
;; unauthenticated rate limit); only write operations and anything on a
;; private repo require a token, and those simply fail with GitHub's own
;; 401/403 when one is missing.
;;
;; Known GitHub product/API gaps this file has to work around (each
;; documented again at its call site):
;;   * No REST endpoint resolves/reopens a PR review comment thread --
;;     only the GraphQL `resolveReviewThread'/`unresolveReviewThread'
;;     mutations can. See `github-resolve-comment'.
;;   * No REST/GraphQL way to retract your own review; the closest is
;;     dismissing it, which is a different semantic action and requires
;;     write access. See `github-approve-pr'.
;;   * No repo-level "default reviewers" endpoint (closest is CODEOWNERS,
;;     not queryable as a flat list). See `github-repo-default-reviewers'.
;;   * No per-job "run this (waiting/gated) step" trigger; the closest
;;     is re-dispatching the whole workflow. See
;;     `github-pipeline-run-manual-step'.  GitHub DOES support the
;;     related-but-different "rerun this already-finished job" action;
;;     see `github-pipeline-step-rerun'/`github-pipeline-step-rerunnable-p'.

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)
(require 'auth-source)
(require 'gp-log)
(require 'git-platform)                 ; `gp-cache-get'/`gp-cache-put'

(defcustom github-api-host "api.github.com"
  "Host of the GitHub REST API."
  :type 'string
  :group 'bitbucket)

(defcustom github-api-base "https://api.github.com"
  "Base URL for the GitHub REST API (v3)."
  :type 'string
  :group 'bitbucket)

(defcustom github-graphql-url "https://api.github.com/graphql"
  "URL of the GitHub GraphQL (v4) endpoint."
  :type 'string
  :group 'bitbucket)

(defcustom github-web-base "https://github.com"
  "Base URL of the GitHub web UI (for browser deep links)."
  :type 'string
  :group 'bitbucket)

(defcustom github-api-token-env "GITHUB_TOKEN"
  "Name of the environment variable holding the default API token."
  :type 'string
  :group 'bitbucket)

(defcustom github-api-token nil
  "GitHub personal access token used for Bearer auth.

When nil, falls back first to the variable named by
`github-api-token-env', then to `auth-source' (host
`github-api-host') so you can keep the token in ~/.authinfo.gpg
instead of the environment.  A token is optional: without one,
requests go out unauthenticated, which works for reading public repos."
  :type '(choice (const :tag "From environment / auth-source" nil) string)
  :group 'bitbucket)

(defcustom github-request-timeout 20
  "Seconds to wait for a synchronous API response before giving up."
  :type 'integer
  :group 'bitbucket)

(defvar github--login-cache nil
  "Cached login of the authenticated user.")

;;;; Credentials --------------------------------------------------------------

(defun github-api-token-value ()
  "Return the configured API token, or nil if unauthenticated.
Consults `github-api-token', then the environment, then `auth-source'."
  (or github-api-token
      (getenv github-api-token-env)
      (let ((found (car (auth-source-search
                         :host github-api-host
                         :max 1))))
        (when found
          (let ((secret (plist-get found :secret)))
            (if (functionp secret) (funcall secret) secret))))))

(defun github--auth-header ()
  "Return the Bearer auth header cons cell, or nil when no token is set.
Nil means the request goes out unauthenticated -- fine for public
GET traffic, but GitHub will 401/403 anything that needs a token."
  (let ((token (github-api-token-value)))
    (when token
      (cons "Authorization" (concat "Bearer " token)))))

(defun github--require-auth (why)
  "Signal a clear `user-error' if no token is configured, naming WHY."
  (unless (github-api-token-value)
    (user-error "GitHub token required to %s (set github-api-token or $%s)"
                why github-api-token-env)))

;;;; Low-level request ----------------------------------------------------------

(defun github--encode-query (params)
  "Encode PARAMS, an alist of (KEY . VALUE), as a URL query string."
  (mapconcat
   (lambda (kv)
     (concat (url-hexify-string (format "%s" (car kv)))
             "="
             (url-hexify-string (format "%s" (cdr kv)))))
   (cl-remove-if-not #'cdr params)
   "&"))

(defun github--build-url (path params)
  "Build a full request URL from PATH and PARAMS.
PATH may be an absolute URL (used verbatim, e.g. a paginated \"next\"
Link) or a path relative to `github-api-base'."
  (let ((base (if (string-prefix-p "http" path)
                  path
                (concat github-api-base
                        (if (string-prefix-p "/" path) "" "/")
                        path))))
    (if params
        (concat base (if (string-search "?" base) "&" "?")
                (github--encode-query params))
      base)))

(defun github--parse-json (string)
  "Parse STRING as JSON into alists/lists, tolerating an empty body."
  (if (or (null string) (string-empty-p (string-trim string)))
      nil
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol)
          (json-false nil)
          (json-null nil))
      (json-read-from-string string))))

(defun github--split-response (buf)
  "Split BUF's raw HTTP response into (STATUS HEADERS . BODY); kill BUF.
HEADERS is the raw header block text (for Link-header parsing);
BODY is the UTF-8 decoded response body."
  (unwind-protect
      (with-current-buffer buf
        (goto-char (point-min))
        (let ((status (if (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                          (string-to-number (match-string 1))
                        0))
              (headers-start (point-min)))
          (goto-char (point-min))
          (let* ((headers-end (if (re-search-forward "\n\n" nil t)
                                  (match-beginning 0)
                                (point-max)))
                 (headers (buffer-substring-no-properties headers-start headers-end))
                 (body (decode-coding-string
                        (buffer-substring-no-properties (point) (point-max))
                        'utf-8)))
            (list status headers body))))
    (kill-buffer buf)))

(defun github--link-next (headers)
  "Return the \"next\" URL from a raw HEADERS block's Link header, or nil."
  (when (string-match "^Link: *\\(.+\\)$" headers)
    (let ((line (match-string 1 headers)))
      (when (string-match "<\\([^>]+\\)>; *rel=\"next\"" line)
        (match-string 1 line)))))

(defconst github--log-redact-keys '(token access_token password secret key)
  "Request-body keys whose values are replaced before logging.")

(defun github--redact-for-log (data)
  "Render DATA for the log, replacing any credential-shaped value."
  (let ((printed
         (cond
          ((and (consp data) (consp (car data)))
           (mapcar (lambda (cell)
                     (if (memq (car-safe cell) github--log-redact-keys)
                         (cons (car cell) "<redacted>")
                       cell))
                   data))
          (t data))))
    (format "%S" printed)))

;;;; Text normalisation ---------------------------------------------------------

(defun github--normalize-newlines (text)
  "Return TEXT with CRLF and lone CR turned into plain LF.
Non-strings (nil for a JSON null) are returned unchanged.

Text typed into github.com's web forms (PR bodies, comments, web-editor
commit messages) comes back from the API with CRLF, per the HTML
textarea spec, and would otherwise render as stray ^M.

Deliberately NOT applied to diffs/patch text or Action logs: there a
carriage return can be part of the content being shown."
  (if (not (stringp text))
      text
    (replace-regexp-in-string "\r\n?" "\n" text)))

(defun github-api-request (method path &optional params data extra-headers)
  "Perform a synchronous GitHub API request and return parsed JSON.

METHOD is a string like \"GET\" or \"POST\".  PATH is a relative API
path or an absolute URL.  PARAMS is an alist of query parameters.
DATA, when non-nil, is a Lisp object serialised as a JSON request
body.  EXTRA-HEADERS is an alist of additional request headers
merged over the defaults (used e.g. to ask for a diff instead of
JSON).

The return value is the parsed JSON with objects as alists, arrays
as lists, and null as nil.  Signals an error on transport or HTTP
failures.

This is the single network choke-point: redefine it (see the test
mock) to run the whole client offline."
  (let* ((url (github--build-url path params))
         (url-request-method method)
         (auth (github--auth-header))
         (url-request-extra-headers
          `(,@(when auth (list auth))
            ("Accept" . "application/vnd.github+json")
            ,@(when data '(("Content-Type" . "application/json")))
            ,@extra-headers))
         (url-request-data
          (when data (encode-coding-string (json-encode data) 'utf-8)))
         (start (float-time))
         (buf (url-retrieve-synchronously url t t github-request-timeout)))
    (unless buf
      (gp-log-error "%s %s -> TIMEOUT (%ss)" method url github-request-timeout)
      (error "GitHub request timed out: %s %s" method url))
    (pcase-let ((`(,status ,_headers ,body) (github--split-response buf)))
      (let ((parsed (github--parse-json body)))
        (when gp-log-requests
          (gp-log (if (and (>= status 200) (< status 300)) 'http 'error)
                  "%s %s -> %d (%.0fms)" method path
                  status (* 1000 (- (float-time) start))))
        (when (or (< status 200) (>= status 300))
          ;; A 422 names the rejected field; log the payload to make it useful.
          (when data
            (gp-log-error "  sent: %s" (github--redact-for-log data)))
          (gp-log-error "  body: %s" (string-trim body))
          (error "GitHub API %s %s -> HTTP %d: %s"
                 method url status
                 (or (alist-get 'message parsed) body)))
        parsed))))

(defun github-api-request-raw (method path &optional params extra-headers)
  "Like `github-api-request' but return the raw (undecoded-as-JSON) body.
Used for endpoints that return text/plain (diffs, Action logs)."
  (let* ((url (github--build-url path params))
         (url-request-method method)
         (auth (github--auth-header))
         (url-request-extra-headers `(,@(when auth (list auth)) ,@extra-headers))
         (buf (url-retrieve-synchronously url t t github-request-timeout)))
    (unless buf (error "GitHub request timed out: %s %s" method url))
    (pcase-let ((`(,status ,_headers ,body) (github--split-response buf)))
      (when (and (>= status 400) (not (equal status 404)))
        (error "GitHub API %s %s -> HTTP %d" method url status))
      (if (equal status 404) "" body))))

(defun github-api-get-async (path params callback)
  "GET PATH with PARAMS asynchronously; call CALLBACK with parsed JSON.
CALLBACK receives the parsed value, or nil on any error (logged).
Non-blocking: returns immediately.  Single page only (no
pagination) -- intended for fan-out scans."
  (let* ((url (github--build-url path params))
         (url-request-method "GET")
         (auth (github--auth-header))
         (url-request-extra-headers
          `(,@(when auth (list auth))
            ("Accept" . "application/vnd.github+json")))
         (start (float-time)))
    (url-retrieve
     url
     (lambda (status-plist)
       (let (result)
         (condition-case e
             (if-let* ((err (plist-get status-plist :error)))
                 (gp-log-error "async %s -> %S" path err)
               (pcase-let ((`(,code ,_headers ,body) (github--split-response (current-buffer))))
                 (when gp-log-requests
                   (gp-log (if (and (>= code 200) (< code 300)) 'http 'error)
                           "GET %s -> %d (%.0fms, async)"
                           path code (* 1000 (- (float-time) start))))
                 (when (and (>= code 200) (< code 300))
                   (setq result (github--parse-json body)))))
           (error (gp-log-error "async %s parse: %s" path (error-message-string e))))
         (funcall callback result)))
     nil t t)))

;;;; Pagination -----------------------------------------------------------------

(defun github--unwrap-page (parsed)
  "Return the list of items in a GitHub list-endpoint response PARSED.
Most list endpoints return a bare JSON array.  Search
(`/search/issues') wraps it in `items'; Actions runs/jobs listings
wrap it in `workflow_runs'/`jobs'.  Unwrap whichever is present, else
assume PARSED is already the array."
  (cond
   ((not (listp parsed)) parsed)
   ((assq 'items parsed) (alist-get 'items parsed))
   ((assq 'workflow_runs parsed) (alist-get 'workflow_runs parsed))
   ((assq 'jobs parsed) (alist-get 'jobs parsed))
   (t parsed)))

(defun github-api-paged (path &optional params max-items)
  "GET PATH following GitHub's Link-header pagination.
PARAMS is the initial query alist.  Stops after MAX-ITEMS values
when that argument is non-nil.  See `github--unwrap-page' for how a
page's body is turned into a plain list of values."
  (let ((acc '())
        (next path)
        (next-params (append params '(("per_page" . "100")))))
    (catch 'done
      (while next
        (let* ((url (github--build-url next next-params))
               (url-request-method "GET")
               (auth (github--auth-header))
               (url-request-extra-headers
                `(,@(when auth (list auth))
                  ("Accept" . "application/vnd.github+json")))
               (buf (url-retrieve-synchronously url t t github-request-timeout)))
          (unless buf (error "GitHub request timed out: GET %s" url))
          (pcase-let ((`(,status ,headers ,body) (github--split-response buf)))
            (when (or (< status 200) (>= status 300))
              (error "GitHub API GET %s -> HTTP %d: %s" url status body))
            (let* ((parsed (github--parse-json body))
                   (values (github--unwrap-page parsed)))
              (dolist (v values)
                (push v acc)
                (when (and max-items (>= (length acc) max-items))
                  (throw 'done nil)))
              (setq next (github--link-next headers))
              (setq next-params nil))))))
    (nreverse acc)))

(defun github-api-paged-async (path &optional params callback max-items)
  "GET PATH following Link-header pagination asynchronously.
CALLBACK is called with (OK VALUES): OK non-nil and VALUES the
collected list on success; OK nil on any error."
  (let ((acc '()) (count 0))
    (cl-labels
        ((finish (ok values) (funcall callback ok values))
         (step (next next-params)
           (let* ((url (github--build-url next next-params))
                  (url-request-method "GET")
                  (auth (github--auth-header))
                  (url-request-extra-headers
                   `(,@(when auth (list auth))
                     ("Accept" . "application/vnd.github+json"))))
             (url-retrieve
              url
              (lambda (status-plist)
                (if (plist-get status-plist :error)
                    (finish nil nil)
                  (condition-case _e
                      (pcase-let ((`(,code ,headers ,body)
                                   (github--split-response (current-buffer))))
                        (if (or (< code 200) (>= code 300))
                            (finish nil nil)
                          (let* ((parsed (github--parse-json body))
                                 (values (github--unwrap-page parsed))
                                 (done nil))
                            (dolist (v values)
                              (push v acc)
                              (setq count (1+ count))
                              (when (and max-items (>= count max-items))
                                (setq done t)))
                            (let ((next-url (and (not done) (github--link-next headers))))
                              (if next-url
                                  (step next-url nil)
                                (finish t (nreverse acc)))))))
                    (error (finish nil nil)))))
              nil t t))))
      (step path (append params '(("per_page" . "100")))))))

;;;; GraphQL --------------------------------------------------------------------

(defun github-graphql-request (query &optional variables)
  "Run GraphQL QUERY (a string) with VARIABLES (an alist); return parsed data.
Signals on transport/HTTP failure or a GraphQL-level `errors' array."
  (github--require-auth "use the GitHub GraphQL API")
  (let* ((data `((query . ,query) (variables . ,(or variables '#s(hash-table)))))
         (url-request-method "POST")
         (auth (github--auth-header))
         (url-request-extra-headers
          `(,@(when auth (list auth))
            ("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode data) 'utf-8))
         (buf (url-retrieve-synchronously github-graphql-url t t github-request-timeout)))
    (unless buf (error "GitHub GraphQL request timed out"))
    (pcase-let ((`(,status ,_headers ,body) (github--split-response buf)))
      (let ((parsed (github--parse-json body)))
        (when (or (< status 200) (>= status 300))
          (error "GitHub GraphQL -> HTTP %d: %s" status body))
        (when (alist-get 'errors parsed)
          (error "GitHub GraphQL errors: %S" (alist-get 'errors parsed)))
        (alist-get 'data parsed)))))

(defun github-graphql-request-async (query variables callback)
  "Run GraphQL QUERY with VARIABLES asynchronously; CALLBACK gets (OK DATA).
Non-blocking twin of `github-graphql-request'.  Errors -- transport,
HTTP, or a GraphQL-level `errors' array -- are reported as OK nil
rather than signalled, since the callers use GraphQL for optional
enrichment (thread resolution) and must degrade to plain REST data
instead of failing the whole fetch.  CALLBACK runs exactly once."
  (if (not (github-api-token-value))
      (run-at-time 0 nil (lambda () (funcall callback nil nil)))
    (let* ((data `((query . ,query) (variables . ,(or variables '#s(hash-table)))))
           (url-request-method "POST")
           (auth (github--auth-header))
           (url-request-extra-headers
            `(,@(when auth (list auth))
              ("Content-Type" . "application/json")))
           (url-request-data (encode-coding-string (json-encode data) 'utf-8)))
      (url-retrieve
       github-graphql-url
       (lambda (status-plist)
         (let ((ok nil) (out nil))
           (condition-case e
               (if (plist-get status-plist :error)
                   (gp-log-error "async graphql -> %S" (plist-get status-plist :error))
                 (pcase-let ((`(,status ,_headers ,body)
                              (github--split-response (current-buffer))))
                   (let ((parsed (github--parse-json body)))
                     (cond
                      ((or (< status 200) (>= status 300))
                       (gp-log-error "async graphql -> HTTP %d" status))
                      ((alist-get 'errors parsed)
                       (gp-log-error "async graphql errors: %S" (alist-get 'errors parsed)))
                      (t (setq ok t out (alist-get 'data parsed)))))))
             (error (gp-log-error "async graphql: %s" (error-message-string e))))
           (funcall callback ok out)))
       nil t t))))

;;;; High-level endpoints: identity ---------------------------------------------

(defun github-current-user ()
  "Return the authenticated user object (alist).  Requires a token."
  (github--require-auth "identify the authenticated user")
  (github-api-request "GET" "/user"))

(defun github-user-login ()
  "Return the authenticated user's login, cached."
  (or github--login-cache
      (setq github--login-cache (alist-get 'login (github-current-user)))))

(defun github-clear-cache ()
  "Forget the cached login so the next call re-resolves it."
  (setq github--login-cache nil))

;;;; Pull requests ----------------------------------------------------------------

(defun github--full-name-of-search-hit (hit)
  "Return \"owner/repo\" parsed from search HIT's `repository_url'."
  (let ((url (alist-get 'repository_url hit)))
    (when (and url (string-match "/repos/\\(.+\\)\\'" url))
      (match-string 1 url))))

(defun github--search-pull-requests (q &optional max-items)
  "Run issue-search Q (a GitHub search qualifier string), fetch full PRs.
Search hits are issue-shaped (no head/base, reviewers or draft flag),
so each hit costs one follow-up PR fetch."
  (let ((hits (github-api-paged "/search/issues"
                                `(("q" . ,q) ("per_page" . "100"))
                                max-items)))
    (delq nil
          (mapcar
           (lambda (hit)
             (let ((full-name (github--full-name-of-search-hit hit))
                   (number (alist-get 'number hit)))
               (and full-name number
                    (ignore-errors (github-pull-request full-name number)))))
           hits))))

(defun github-workspace-pull-requests (&optional login state max-items)
  "Return PRs across GitHub authored by LOGIN (default the authenticated user).
STATE is \"OPEN\"/\"MERGED\"/\"DECLINED\"-shaped like Bitbucket's, but
GitHub search only distinguishes open/closed -- \"MERGED\"/\"DECLINED\"
are both mapped to a closed-PR search, and the caller can tell them
apart afterwards via `merged_at'."
  (github--search-pull-requests
   (github--workspace-pr-query (or login (github-user-login)) state)
   max-items))

(defun github--workspace-pr-query (login state)
  "Return the issue-search query for LOGIN's PRs in STATE."
  (format "is:pr author:%s %s"
          login
          (if (or (null state) (equal state "OPEN")) "is:open" "is:closed")))

(defun github--search-pull-requests-async (q callback &optional max-items)
  "Async twin of `github--search-pull-requests'; CALLBACK gets (OK PRS).
Genuinely non-blocking: the search, then one `github-pull-request-async'
per hit.  Wrapping the sync version in a timer would only relocate the
freeze, so every hit must go through the async fetch too.

PR fetches that fail are dropped.  CALLBACK runs exactly once, after
the last outstanding fetch settles."
  (github-api-paged-async
   "/search/issues"
   `(("q" . ,q))
   (lambda (ok hits)
     (if (not ok)
         (funcall callback nil nil)
       (let* ((targets
               (delq nil
                     (mapcar (lambda (hit)
                               (let ((full-name (github--full-name-of-search-hit hit))
                                     (number (alist-get 'number hit)))
                                 (and full-name number (cons full-name number))))
                             hits)))
              (pending (length targets))
              ;; Fill by index to keep search order; fetches finish out of order.
              (slots (make-vector (length targets) nil)))
         (if (zerop pending)
             (funcall callback t nil)
           (let ((i -1))
             (dolist (target targets)
               (setq i (1+ i))
               (let ((idx i))
                 (github-pull-request-async
                  (car target) (cdr target)
                  (lambda (pr-ok pr)
                    (when pr-ok (aset slots idx pr))
                    (setq pending (1- pending))
                    (when (zerop pending)
                      (funcall callback t (delq nil (append slots nil)))))))))))))
   max-items))

(defun github-workspace-pull-requests-async (callback &optional login state max-items)
  "Async twin of `github-workspace-pull-requests'; CALLBACK gets (OK PRS).
Pass LOGIN when known: resolving it via `github-user-login' blocks.
When LOGIN is nil that lookup is deferred onto a timer so the caller
can still paint first."
  (if login
      (github--search-pull-requests-async
       (github--workspace-pr-query login state) callback max-items)
    (run-at-time
     0 nil
     (lambda ()
       (condition-case e
           (github--search-pull-requests-async
            (github--workspace-pr-query (github-user-login) state)
            callback max-items)
         (error
          (gp-log-error "github workspace PR scan: %s" (error-message-string e))
          (funcall callback nil nil)))))))

(defun github-reviewing-pull-requests (&optional login limit states)
  "Return PRs across GitHub where LOGIN is a requested reviewer.
LIMIT caps the number of search results.
STATES nil/(\"OPEN\") means open only; `all' means both."
  (let* ((login (or login (github-user-login)))
         (open-p (or (null states) (equal states '("OPEN"))))
         (q (format "is:pr review-requested:%s%s"
                    login (if open-p " is:open" ""))))
    (github--search-pull-requests q limit)))

(defun github-reviewing-pull-requests-async (login states on-batch on-done &optional limit)
  "Async twin of `github-reviewing-pull-requests'.
Runs the single global search on a timer and delivers one batch.
Uses `run-at-time', NOT `run-with-idle-timer': ongoing `url-retrieve'
I/O can starve idle timers so the scan never fires."
  (run-at-time
   0 nil
   (lambda ()
     (condition-case e
         (let ((prs (github-reviewing-pull-requests login limit states)))
           (funcall on-batch prs)
           (funcall on-done))
       (error
        (gp-log-error "github reviewing scan: %s" (error-message-string e))
        (funcall on-done))))))

(defun github-open-pull-requests-async (states on-batch on-done &optional limit)
  "Async scan of ALL open PRs across every repo the token can see.
GitHub has no useful unscoped \"every open PR\" query, so this
searches PRs involving the authenticated user as a stand-in.  Uses
`run-at-time', NOT `run-with-idle-timer' (see
`github-reviewing-pull-requests-async')."
  (let ((login (ignore-errors (github-user-login))))
    (run-at-time
     0 nil
     (lambda ()
       (condition-case e
           (let* ((open-p (or (null states) (equal states '("OPEN")) (equal states "OPEN")))
                  (q (format "is:pr%s%s"
                            (if open-p " is:open" "")
                            (if login (format " involves:%s" login) "")))
                  (prs (github--search-pull-requests q limit)))
             (funcall on-batch prs)
             (funcall on-done))
         (error
          (gp-log-error "github open-PR scan: %s" (error-message-string e))
          (funcall on-done)))))))

(defun github--reshape-pr (pr)
  "Return a copy of PR with `id' overwritten to hold its per-repo `number'.
Callers treat `id' as the identifier to hand back to the backend, but
GitHub's PR `id' is a global database id no per-repo endpoint accepts.
The original survives under `gh-database-id'.  The body's newlines are
normalised too."
  (when pr
    (let* ((number (alist-get 'number pr))
           (pr (copy-alist pr))
           (body (assq 'body pr)))
      (when body (setcdr body (github--normalize-newlines (cdr body))))
      (if (not number)
          pr
        (cons (cons 'id number)
              (cons (cons 'gh-database-id (alist-get 'id pr))
                    (assq-delete-all 'id (assq-delete-all 'gh-database-id pr))))))))

(defun github-pull-request (full-name number)
  "Return the full PR object for FULL-NAME (\"owner/repo\") and PR NUMBER."
  (github--reshape-pr
   (github-api-request "GET" (format "/repos/%s/pulls/%s" full-name number))))

(defun github-pull-request-async (full-name number callback)
  "Fetch PR NUMBER in FULL-NAME asynchronously, calling CALLBACK with (OK PR)."
  (github-api-get-async
   (format "/repos/%s/pulls/%s" full-name number)
   nil
   (lambda (pr) (funcall callback (and pr t) (github--reshape-pr pr)))))

(defun github-pr-draft-p (pr)
  "Return non-nil if PR is a draft pull request."
  (and (alist-get 'draft pr) t))

(defun github-pr-authored-by-p (pr login)
  "Return non-nil if PR was authored by LOGIN."
  (equal (let-alist pr .user.login) login))

(defun github--pr-reviews (full-name number)
  "Return the list of review objects for PR NUMBER in FULL-NAME."
  (github-api-paged (format "/repos/%s/pulls/%s/reviews" full-name number)))

(defun github--latest-review-per-user (reviews)
  "Return an alist of LOGIN -> latest review object from REVIEWS.
GitHub returns every review ever submitted; only the most recent
one per reviewer counts toward the current approval state."
  (let ((by-user (make-hash-table :test 'equal)))
    (dolist (r reviews)
      (let ((login (let-alist r .user.login)))
        (when login (puthash login r by-user))))
    (let (acc)
      (maphash (lambda (k v) (push (cons k v) acc)) by-user)
      acc)))

(defun github--review-tally-from (reviews requested-logins)
  "Return a plist (:approved N :changes N :pending N).
REVIEWS is the raw review list (as from `github--pr-reviews'),
REQUESTED-LOGINS the PR's `requested_reviewers' logins.  Counts each
reviewer's most recent review only, plus any reviewer still
requested but who has not submitted a review at all (pending)."
  (let* ((latest (github--latest-review-per-user reviews))
         (approved 0) (changes 0) (pending 0))
    (dolist (kv latest)
      (pcase (alist-get 'state (cdr kv))
        ("APPROVED" (setq approved (1+ approved)))
        ("CHANGES_REQUESTED" (setq changes (1+ changes)))
        (_ nil)))
    (dolist (login requested-logins)
      (unless (assoc login latest)
        (setq pending (1+ pending))))
    (list :approved approved :changes changes :pending pending)))

(defun github-pr-review-tally (pr)
  "Return a plist (:approved N :changes N :pending N) over PR's reviews.
Fetches synchronously; see `github-pr-review-tally-async'."
  (let* ((full-name (let-alist pr .base.repo.full_name))
         (number (alist-get 'number pr))
         (reviews (and full-name number (github--pr-reviews full-name number)))
         (requested (mapcar (lambda (u) (alist-get 'login u))
                            (alist-get 'requested_reviewers pr))))
    (github--review-tally-from reviews requested)))

(defun github-pr-review-tally-async (pr callback)
  "Fetch PR's review tally asynchronously; CALLBACK gets the plist."
  (let* ((full-name (let-alist pr .base.repo.full_name))
         (number (alist-get 'number pr))
         (requested (mapcar (lambda (u) (alist-get 'login u))
                            (alist-get 'requested_reviewers pr))))
    (if (not (and full-name number))
        (funcall callback (github--review-tally-from nil requested))
      (github-api-paged-async
       (format "/repos/%s/pulls/%s/reviews" full-name number)
       nil
       (lambda (ok reviews)
         (funcall callback (github--review-tally-from (and ok reviews) requested)))))))

(defun github--reviewers-from (reviews requested-reviewers)
  "Return a list of plists (:id :name :avatar :state) for the detail view.
REVIEWS is the raw review list; REQUESTED-REVIEWERS is the PR's
`requested_reviewers' array (used for pending reviewers, who have
no review to carry their avatar).  Counts only the most recent
review per person, like `github--review-tally-from'.

:ID is the login `github-set-pull-request-reviewers' takes.  It equals
:NAME here, but is kept distinct because Bitbucket's :ID is a uuid."
  (let ((latest (github--latest-review-per-user reviews))
        (reviewed-logins (make-hash-table :test 'equal))
        out)
    (dolist (kv latest)
      (let* ((login (car kv)) (r (cdr kv)))
        (puthash login t reviewed-logins)
        (push (list :id login
                    :name login
                    :avatar (let-alist r .user.avatar_url)
                    :state (pcase (alist-get 'state r)
                             ("APPROVED" 'approved)
                             ("CHANGES_REQUESTED" 'changes)
                             (_ 'pending)))
              out)))
    (seq-doseq (u requested-reviewers)
      (let ((login (alist-get 'login u)))
        (unless (gethash login reviewed-logins)
          (push (list :id login :name login
                      :avatar (alist-get 'avatar_url u) :state 'pending)
                out))))
    (nreverse out)))

(defun github-pr-reviewers-async (pr callback)
  "Fetch PR's individual reviewers asynchronously; CALLBACK gets the list.
The list is shaped as by `github--reviewers-from'."
  (let* ((full-name (let-alist pr .base.repo.full_name))
         (number (alist-get 'number pr))
         (requested (alist-get 'requested_reviewers pr)))
    (if (not (and full-name number))
        (funcall callback (github--reviewers-from nil requested))
      (github-api-paged-async
       (format "/repos/%s/pulls/%s/reviews" full-name number)
       nil
       (lambda (ok reviews)
         (funcall callback (github--reviewers-from (and ok reviews) requested)))))))

(defun github-pr-my-review-state (pr login)
  "Return LOGIN's own review state on PR: `approved', `changes', or nil."
  (let* ((full-name (let-alist pr .base.repo.full_name))
         (number (alist-get 'number pr))
         (reviews (and full-name number (github--pr-reviews full-name number)))
         (mine (cdr (assoc login (github--latest-review-per-user reviews)))))
    (pcase (and mine (alist-get 'state mine))
      ("APPROVED" 'approved)
      ("CHANGES_REQUESTED" 'changes)
      (_ nil))))

(defconst github-merge-methods '("merge" "squash" "rebase")
  "The `merge_method' values GitHub's merge endpoint accepts.
A repository can disable any of them; ask rather than assume (see
`github-pull-request-merge-strategies').")

(defun github-pull-request-merge-strategies (full-name _number)
  "Return (STRATEGIES . DEFAULT) permitted in FULL-NAME.
This is a repository setting on GitHub, so NUMBER is ignored.  DEFAULT
is the first permitted method in GitHub's order, as the API has no
\"default method\" field."
  (let* ((repo (ignore-errors (github-api-request "GET" (format "/repos/%s" full-name))))
         (allowed (delq nil
                       (list (and (eq (alist-get 'allow_merge_commit repo) t) "merge")
                             (and (eq (alist-get 'allow_squash_merge repo) t) "squash")
                             (and (eq (alist-get 'allow_rebase_merge repo) t) "rebase")))))
    (when allowed (cons allowed (car allowed)))))

(defun github-pull-request-mergeability (full-name number)
  "Return (MERGEABLE . STATE) for PR NUMBER in FULL-NAME.
MERGEABLE is t, nil (conflicts) or `unknown'; STATE is GitHub's own
`mergeable_state' word (\"clean\", \"dirty\", \"blocked\", \"behind\",
\"unstable\", \"draft\", \"unknown\").

`mergeable' is computed lazily (null until GitHub has tested the PR),
and the JSON parser maps both null and false to nil, so only
`mergeable_state' separates a real conflict (\"dirty\") from
\"not computed yet\" (\"unknown\")."
  (let* ((pr (ignore-errors
               (github-api-request "GET" (format "/repos/%s/pulls/%s" full-name number))))
         (raw (alist-get 'mergeable pr))
         (state (alist-get 'mergeable_state pr)))
    (when pr
      (cons (cond ((eq raw t) t)
                  ;; a stated conflict, whatever `mergeable' came back as
                  ((equal state "dirty") nil)
                  ;; not judged yet, or blocked for a reason that is not a
                  ;; conflict (review required, behind, failing checks)
                  (t 'unknown))
            state))))

(defun github-pull-request-divergence (full-name base head)
  "Return (AHEAD . BEHIND) commit counts of HEAD against BASE in FULL-NAME.
BEHIND is how many commits BASE has that HEAD lacks -- \"how far behind
the target branch this PR is\".  Nil when the comparison fails."
  (when (and full-name base head)
    (let ((r (ignore-errors
               (github-api-request
                "GET" (format "/repos/%s/compare/%s...%s" full-name base head)))))
      (when r
        (cons (or (alist-get 'ahead_by r) 0)
              (or (alist-get 'behind_by r) 0))))))

(defun github-merge-pull-request (full-name number &optional strategy message _close-source-branch)
  "Merge PR NUMBER in FULL-NAME, returning the API's merge result.
STRATEGY is one of `github-merge-methods' (nil lets GitHub choose).
MESSAGE becomes the merge commit's message body.  CLOSE-SOURCE-BRANCH is
ignored: only the repo's `delete_branch_on_merge' setting decides.
Requires Pull requests: write."
  (when (and strategy (not (member strategy github-merge-methods)))
    (error "Not a GitHub merge method: %s" strategy))
  (github-api-request
   "PUT" (format "/repos/%s/pulls/%s/merge" full-name number)
   nil
   (append (when strategy `((merge_method . ,strategy)))
           (when (and message (not (string-empty-p message)))
             `((commit_message . ,message))))))

(defun github-set-pull-request-title (full-name number title)
  "Set PR NUMBER in FULL-NAME's title to TITLE; return the updated PR.
Requires Pull requests: write."
  (when (or (null title) (string-empty-p (string-trim title)))
    (error "A pull request title cannot be empty"))
  (github-api-request
   "PATCH" (format "/repos/%s/pulls/%s" full-name number)
   nil `((title . ,title))))

(defun github-set-pull-request-description (full-name number description &optional _title)
  "Set PR NUMBER in FULL-NAME's body to DESCRIPTION; return the updated PR.
TITLE is ignored: a partial PATCH leaves the title untouched.
Requires Pull requests: write."
  (github-api-request
   "PATCH" (format "/repos/%s/pulls/%s" full-name number)
   nil `((body . ,(or description "")))))

(defun github-set-pull-request-draft (full-name number draft &optional _title)
  "Set PR NUMBER in FULL-NAME's draft flag to DRAFT; return the updated PR.
REST cannot toggle draft state; both directions are GraphQL mutations."
  (let* ((pr (github-pull-request full-name number))
         (node-id (alist-get 'node_id pr))
         (mutation (if draft
                       "mutation($id:ID!){convertPullRequestToDraft(input:{pullRequestId:$id}){pullRequest{id}}}"
                     "mutation($id:ID!){markPullRequestReadyForReview(input:{pullRequestId:$id}){pullRequest{id}}}")))
    (github-graphql-request mutation `((id . ,node-id)))
    (github-pull-request full-name number)))

(defun github-open-pr-for-branch (full-name branch)
  "Return the open PR in FULL-NAME whose head branch is BRANCH, or nil.
The full PR is fetched, since the list endpoint omits comment counts."
  (when-let* ((hit (car (github-api-paged
                         (format "/repos/%s/pulls" full-name)
                         `(("state" . "open")
                           ("head" . ,(format "%s:%s"
                                              (car (split-string full-name "/"))
                                              branch)))
                         1))))
    (github-pull-request full-name (alist-get 'number hit))))

(defun github-repo-default-branch (full-name)
  "Return repo FULL-NAME's default branch name, or nil."
  (ignore-errors
    (alist-get 'default_branch (github-api-request "GET" (format "/repos/%s" full-name)))))

(defun github-repo-branches (full-name)
  "Return every branch name in FULL-NAME, alphabetically."
  (ignore-errors
    (sort (mapcar (lambda (b) (alist-get 'name b))
                  (github-api-paged (format "/repos/%s/branches" full-name)))
          #'string<)))

(cl-defun github-create-pull-request
    (full-name source dest title
     &key description draft close-source-branch reviewer-uuids)
  "Open a pull request in FULL-NAME from branch SOURCE into DEST.
CLOSE-SOURCE-BRANCH has no GitHub equivalent and is ignored.
REVIEWER-UUIDS are GitHub logins (the protocol's generic name)."
  (let* ((data (list (cons 'title title)
                     (cons 'head source)
                     (cons 'base dest))))
    (when (and description (not (string-empty-p description)))
      (setq data (append data (list (cons 'body description)))))
    (when draft
      (setq data (append data (list (cons 'draft t)))))
    (let ((pr (github-api-request "POST" (format "/repos/%s/pulls" full-name) nil data)))
      (when reviewer-uuids
        (ignore-errors
          (github-api-request
           "POST"
           (format "/repos/%s/pulls/%s/requested_reviewers" full-name (alist-get 'number pr))
           nil `((reviewers . ,reviewer-uuids)))))
      (ignore close-source-branch)
      (github--reshape-pr pr))))

(defun github-set-pull-request-reviewers (full-name id reviewer-logins
                                                    &optional current-logins)
  "Make REVIEWER-LOGINS the requested reviewers of PR ID in FULL-NAME.
There is no whole-list endpoint, so the target is diffed against
CURRENT-LOGINS and only additions (POST) and removals (DELETE) are
sent; re-POSTing an existing reviewer would re-notify them.

Removing a login that already submitted a review has no effect, so
callers must not offer it (`gp-ui-edit-reviewers' locks them).
Requires Pull requests: write.  Returns non-nil on success."
  (let* ((add (cl-remove-if (lambda (l) (member l current-logins)) reviewer-logins))
         (del (cl-remove-if (lambda (l) (member l reviewer-logins)) current-logins))
         (path (format "/repos/%s/pulls/%s/requested_reviewers" full-name id)))
    (when del
      (github-api-request "DELETE" path nil `((reviewers . ,(vconcat del)))))
    (when add
      (github-api-request "POST" path nil `((reviewers . ,(vconcat add)))))
    t))

(defun github-repo-default-reviewers (full-name)
  "Return repo FULL-NAME's default reviewers: always nil.
GitHub has no repo-level default-reviewers endpoint (CODEOWNERS is not
a queryable list), and an empty list is a legitimate answer."
  (ignore full-name)
  nil)

(defun github-repo-suggested-reviewers (full-name)
  "Return repo FULL-NAME's collaborators as reviewer suggestions.
GitHub's GraphQL `suggestedReviewers' needs an existing PR, so for the
pre-PR create form this approximates it with the collaborators,
excluding the authenticated user.  Each entry is an alist with `uuid'
= login and `display_name' = name or login.  Errors yield nil.

A non-empty result is cached (`gp-cache-ttl'); an empty one is not, so
a transient failure doesn't stick."
  (let* ((key (list 'github-collaborators full-name))
         (hit (gp-cache-get key)))
    (if (car hit)
        (cdr hit)
      (let ((suggestions
             (ignore-errors
               (let ((me (github-user-login)))
                 (delq nil
                       (mapcar (lambda (c)
                                 (let ((login (alist-get 'login c)))
                                   (unless (equal login me)
                                     `((uuid . ,login)
                                       (display_name . ,(or (alist-get 'name c) login))))))
                               (github-api-paged
                                (format "/repos/%s/collaborators" full-name))))))))
        (when suggestions (gp-cache-put key suggestions))
        suggestions))))

(defun github-repo-open-pr-count (full-name)
  "Return the number of OPEN pull requests in repo FULL-NAME."
  (length (github-api-paged (format "/repos/%s/pulls" full-name) '(("state" . "open")))))

(defun github-repo-pull-requests (full-name &optional state)
  "Return the pull requests in repo FULL-NAME.  STATE defaults to \"open\"."
  (mapcar #'github--reshape-pr
          (github-api-paged
           (format "/repos/%s/pulls" full-name)
           `(("state" . ,(downcase (or state "open"))) ("sort" . "updated") ("direction" . "desc")))))

;;;; Labels ------------------------------------------------------------------

;; A PR's own labels are embedded in the PR payload (never cached); only the
;; repo-wide candidate pool costs a request, cached via `gp-cache-*'.

(defun github--label (label)
  "Return LABEL (a GitHub label alist) as a (:name :color) plist.
The colour stays bare hex with no leading \"#\", as GitHub sends it.
Returns nil for anything without a name."
  (let ((name (alist-get 'name label))
        (color (alist-get 'color label)))
    (when (and (stringp name) (not (string-empty-p name)))
      (list :name name
            :color (and (stringp color) (not (string-empty-p color)) color)))))

(defun github--label-list (labels)
  "Return LABELS (a GitHub `labels' list) as `github--label' plists."
  (delq nil (mapcar #'github--label labels)))

(defun github-pr-labels (pr)
  "Return PR's labels as (:name :color) plists, read off the PR payload."
  (github--label-list (alist-get 'labels pr)))

(defun github-repo-labels (full-name)
  "Return every label defined in repo FULL-NAME, as (:name :color) plists.
A non-empty result is cached (`gp-cache-ttl'); an empty one is not, so
a transient failure cannot stick as \"no labels\"."
  (let* ((key (list 'github-repo-labels full-name))
         (hit (gp-cache-get key)))
    (if (car hit)
        (cdr hit)
      (let ((labels (github--label-list
                     (github-api-paged (format "/repos/%s/labels" full-name)))))
        (when labels (gp-cache-put key labels))
        labels))))

(defun github-set-pull-request-labels (full-name number labels)
  "Set PR NUMBER in FULL-NAME to carry exactly LABELS (a list of names).
The complete set is PUT to the issues endpoint, so any name left out
comes off.  Requires Issues: write.  Returns non-nil on success."
  (github-api-request "PUT" (format "/repos/%s/issues/%s/labels" full-name number)
                      nil `((labels . ,(vconcat labels))))
  t)

;;;; Reactions ----------------------------------------------------------------

(defconst github-reaction-contents
  '("+1" "-1" "laugh" "confused" "heart" "hooray" "rocket" "eyes")
  "The reaction `content' values GitHub accepts, in its own display order.
Anything else is rejected by the API with 422.")

(defun github--reaction-base (full-name comment)
  "Return the reactions endpoint path for COMMENT in FULL-NAME.
Inline (`pulls') and general (`issues') comments live in separate
collections with separate id sequences, so the wrong base could hit an
unrelated comment.  COMMENT is a reshaped alist; its `inline' key
decides."
  (format "/repos/%s/%s/comments/%s/reactions"
          full-name
          (if (alist-get 'inline comment) "pulls" "issues")
          (alist-get 'id comment)))

(defun github--reshape-reaction (r)
  "Reshape a REST reaction R into the shared shape.
The reactor's login is also exposed as `uuid', which is what callers
compare against `gp-user-uuid'; without it a toggle becomes add-only."
  `((id . ,(alist-get 'id r))
    (content . ,(alist-get 'content r))
    (user (login . ,(let-alist r .user.login))
          (uuid . ,(let-alist r .user.login))
          (display_name . ,(let-alist r .user.login)))))

(defun github-comment-reactions (full-name comment)
  "Return the reactions on COMMENT in FULL-NAME as a list of alists.
Each entry has `id', `content' and a `user' whose `uuid' matches what
`gp-user-uuid' reports."
  (mapcar #'github--reshape-reaction
          (github-api-paged (github--reaction-base full-name comment))))

(defun github-add-comment-reaction (full-name comment content)
  "React to COMMENT in FULL-NAME with CONTENT (see `github-reaction-contents').
Returns the created reaction.  Idempotent: an existing reaction is
returned rather than duplicated.  Requires Pull requests: write (review
comments) or Issues: write (general ones)."
  (unless (member content github-reaction-contents)
    (error "Not a GitHub reaction: %s" content))
  (github-api-request "POST" (github--reaction-base full-name comment)
                      nil `((content . ,content))))

(defun github-remove-comment-reaction (full-name comment content &optional login)
  "Remove LOGIN's CONTENT reaction from COMMENT in FULL-NAME.
LOGIN defaults to the authenticated user.  Deleting needs the
reaction's id, so it is looked up first; returns non-nil when one was
found and deleted."
  (let* ((login (or login (github-user-login)))
         (mine (seq-find (lambda (r)
                           (and (equal (alist-get 'content r) content)
                                ;; reshaped rows carry both; match either
                                (member login (list (let-alist r .user.login)
                                                    (let-alist r .user.uuid)))))
                         (github-comment-reactions full-name comment))))
    (when mine
      (github-api-request
       "DELETE" (format "%s/%s" (github--reaction-base full-name comment)
                        (alist-get 'id mine)))
      t)))

;;;; Comments -----------------------------------------------------------------

(defun github--issue-comments (full-name number)
  "Return general (non-inline) comments on PR NUMBER in FULL-NAME."
  (github-api-paged (format "/repos/%s/issues/%s/comments" full-name number)))

(defun github--review-comments (full-name number)
  "Return inline review comments on PR NUMBER in FULL-NAME."
  (github-api-paged (format "/repos/%s/pulls/%s/comments" full-name number)))

(defvar github--resolved-thread-comment-ids (make-hash-table :test 'eql)
  "Comment id -> t for review comments known to sit in a resolved thread.
Populated by `github--refresh-resolved-threads'.")

(defconst github--resolved-threads-query
  "query($owner:String!,$repo:String!,$number:Int!){
     repository(owner:$owner,name:$repo){
       pullRequest(number:$number){
         reviewThreads(first:100){nodes{
           isResolved
           comments(first:100){nodes{databaseId}}}}}}}"
  "GraphQL query listing a PR's review threads and their resolution state.
Shared by the sync and async resolved-thread refreshes.")

(defun github--record-resolved-threads (data)
  "Cache the comment ids of every resolved review thread in DATA."
  (let-alist data
    (dolist (thread .repository.pullRequest.reviewThreads.nodes)
      (when (alist-get 'isResolved thread)
        (dolist (c (let-alist thread .comments.nodes))
          (puthash (alist-get 'databaseId c) t
                   github--resolved-thread-comment-ids))))))

(defun github--refresh-resolved-threads (full-name number)
  "Query GraphQL for PR NUMBER's resolved review threads; cache their comment ids.
Populates `github--resolved-thread-comment-ids'.  Swallows errors
(GraphQL needs a token) so plain REST comment listing still works
without one; resolution just won't be reflected."
  (ignore-errors
    (let ((owner (car (split-string full-name "/")))
          (repo (cadr (split-string full-name "/"))))
      (github--record-resolved-threads
       (github-graphql-request
        github--resolved-threads-query
        `((owner . ,owner) (repo . ,repo) (number . ,number)))))))

(defvar github--viewer-reactions (make-hash-table :test 'eql)
  "Comment id -> list of reaction contents the viewer has reacted with.
Populated by `github--refresh-viewer-reactions'.  REST never says
whether the viewer reacted, and the renderer must not fetch per comment
mid-render.")

(defun github--refresh-viewer-reactions (full-name number)
  "Cache which reactions the viewer holds on PR NUMBER's comments.
One GraphQL query covers every comment of both kinds via
`viewerHasReacted'.  Errors are swallowed (GraphQL needs a token); only
the \"is it mine\" hint is lost."
  (ignore-errors
    (let* ((owner (car (split-string full-name "/")))
           (repo (cadr (split-string full-name "/")))
           (data (github-graphql-request
                  "query($owner:String!,$repo:String!,$number:Int!){
                     repository(owner:$owner,name:$repo){
                       pullRequest(number:$number){
                         comments(first:100){nodes{
                           databaseId
                           reactionGroups{content viewerHasReacted}}}
                         reviewThreads(first:100){nodes{
                           comments(first:100){nodes{
                             databaseId
                             reactionGroups{content viewerHasReacted}}}}}}}}"
                  `((owner . ,owner) (repo . ,repo) (number . ,number)))))
      (let-alist data
        (cl-labels
            ((record (c)
               (let ((id (alist-get 'databaseId c))
                     (mine '()))
                 (dolist (g (alist-get 'reactionGroups c))
                   (when (eq (alist-get 'viewerHasReacted g) t)
                     (push (github--reaction-token (alist-get 'content g)) mine)))
                 (when id (puthash id (nreverse mine) github--viewer-reactions)))))
          (dolist (c .repository.pullRequest.comments.nodes) (record c))
          (dolist (thread .repository.pullRequest.reviewThreads.nodes)
            (dolist (c (let-alist thread .comments.nodes)) (record c))))))))

(defconst github--graphql-reaction-tokens
  '(("THUMBS_UP" . "+1") ("THUMBS_DOWN" . "-1") ("LAUGH" . "laugh")
    ("CONFUSED" . "confused") ("HEART" . "heart") ("HOORAY" . "hooray")
    ("ROCKET" . "rocket") ("EYES" . "eyes"))
  "GraphQL `ReactionContent' enum -> the REST content token.")

(defun github--reaction-token (graphql-content)
  "Return the REST token for GRAPHQL-CONTENT (a `ReactionContent' value)."
  (or (cdr (assoc graphql-content github--graphql-reaction-tokens))
      (downcase (or graphql-content ""))))

(defun github-pull-request-comments (full-name number &optional max-items)
  "Return the merged (issue + review) comments for PR NUMBER in FULL-NAME.
Each is reshaped into a Bitbucket-like alist so the shared
`gp--comment-resolved-p'/`gp--comment-own-p' accessors work
unchanged: `content.raw', `user.display_name', `user.uuid' (the
login), `resolution' (present when GraphQL reports the comment's
review thread resolved -- see `github--refresh-resolved-threads'),
and, for inline comments, `inline.path'/`inline.to'."
  (github--refresh-resolved-threads full-name number)
  (github--refresh-viewer-reactions full-name number)
  (let* ((issue (mapcar #'github--reshape-issue-comment (github--issue-comments full-name number)))
         (review (mapcar #'github--reshape-review-comment (github--review-comments full-name number)))
         (all (append issue review)))
    (if max-items (cl-subseq all 0 (min max-items (length all))) all)))

(defun github-pull-request-comments-async (full-name number callback &optional max-items)
  "Async twin of `github-pull-request-comments'.  CALLBACK gets (OK COMMENTS).
Genuinely non-blocking: the GraphQL thread-resolution query and the
issue and review comment listings run concurrently; the merged result
is delivered once the last settles.  A timer around the sync version
would still block redisplay.

A GraphQL failure is tolerated; the REST comments are still returned."
  (let ((pending 3)
        (issue nil)
        (review nil)
        (failed nil))
    (cl-labels
        ((settle ()
           (setq pending (1- pending))
           (when (zerop pending)
             (let ((all (append
                         (mapcar #'github--reshape-issue-comment issue)
                         (mapcar #'github--reshape-review-comment review))))
               ;; Reshape only now: the reshapers read
               ;; `github--resolved-thread-comment-ids', which the GraphQL
               ;; stage fills in, possibly last.
               (funcall callback (not failed)
                        (if max-items
                            (cl-subseq all 0 (min max-items (length all)))
                          all))))))
      (let ((owner (car (split-string full-name "/")))
            (repo (cadr (split-string full-name "/"))))
        (github-graphql-request-async
         github--resolved-threads-query
         `((owner . ,owner) (repo . ,repo) (number . ,number))
         (lambda (ok data)
           (when ok (github--record-resolved-threads data))
           (settle))))
      (github-api-paged-async
       (format "/repos/%s/issues/%s/comments" full-name number) nil
       (lambda (ok values)
         (if ok (setq issue values) (setq failed t))
         (settle)))
      (github-api-paged-async
       (format "/repos/%s/pulls/%s/comments" full-name number) nil
       (lambda (ok values)
         (if ok (setq review values) (setq failed t))
         (settle))))))

(defun github--reaction-counts (c)
  "Extract ((CONTENT . COUNT) …) from comment C's inline `reactions' object."
  (when-let* ((r (alist-get 'reactions c)))
    ;; The thumbs keys need escaping as symbols (`\+1' / `\-1'); their
    ;; `symbol-name' is the plain "+1" / "-1" the rest of the code uses.
    (delq nil
          (mapcar (lambda (k)
                    (let ((n (alist-get k r)))
                      (when (and (numberp n) (> n 0))
                        (cons (symbol-name k) n))))
                  '(\+1 \-1 laugh confused heart hooray rocket eyes)))))

(defun github--reshape-comment (c &optional extra)
  "Reshape REST comment C into the shared alist shape, splicing in EXTRA."
  `((reaction-counts . ,(github--reaction-counts c))
    (reaction-mine . ,(gethash (alist-get 'id c) github--viewer-reactions))
    (id . ,(alist-get 'id c))
    (content (raw . ,(github--normalize-newlines (alist-get 'body c))))
    (user (display_name . ,(let-alist c .user.login))
          (uuid . ,(let-alist c .user.login))
          (links (avatar (href . ,(let-alist c .user.avatar_url)))))
    (created_on . ,(alist-get 'created_at c))
    (links (html (href . ,(alist-get 'html_url c))))
    ,@extra
    ,@(when (gethash (alist-get 'id c) github--resolved-thread-comment-ids)
        '((resolution (user (display_name . "GitHub")))))))

(defun github--reshape-issue-comment (c)
  "Reshape a REST issue comment C into the shared comment alist shape."
  (github--reshape-comment c))

(defun github--reshape-review-comment (c)
  "Reshape a REST review (inline) comment C into the shared alist shape."
  (github--reshape-comment
   c `((inline (path . ,(alist-get 'path c))
               (from . ,(alist-get 'line c))
               (to . ,(alist-get 'line c)))
       (parent . ,(when-let* ((pid (alist-get 'in_reply_to_id c)))
                    `((id . ,pid)))))))

(defun github-comment-resolved-p (comment)
  "Return non-nil if COMMENT's review thread was reported resolved."
  (and (alist-get 'resolution comment) t))

(defun github-comment-resolvable-p (comment)
  "Return non-nil if COMMENT belongs to a resolvable review thread.
Only inline review comments (those with an `inline' key) have one."
  (and (alist-get 'inline comment) t))

(defun github-comment-own-p (comment login)
  "Return non-nil if COMMENT was written by LOGIN."
  (equal (let-alist comment .user.uuid) login))

(defun github--diff-commentable-lines (diff)
  "Return ((PATH . ((START . END) …)) …): the new-side hunk ranges in DIFF.
GitHub only accepts an inline comment on a line in the PR's diff.  Only
the new side is considered, matching the `side' the package posts on."
  (let (out path)
    (dolist (line (split-string (or diff "") "\n"))
      (cond
       ((string-prefix-p "+++ b/" line)
        (setq path (substring line 6))
        ;; /dev/null on a deletion -- nothing commentable there
        (when (equal path "/dev/null") (setq path nil)))
       ((and path (string-prefix-p "@@" line))
        (when (string-match "\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?" line)
          (let* ((start (string-to-number (match-string 1 line)))
                 (count (if (match-string 2 line)
                            (string-to-number (match-string 2 line))
                          1))
                 (cell (assoc path out)))
            ;; a zero-length range means the hunk only removes lines
            (when (> count 0)
              (if cell
                  (setcdr cell (cons (cons start (+ start count -1)) (cdr cell)))
                (push (cons path (list (cons start (+ start count -1)))) out))))))))
    (mapcar (lambda (cell) (cons (car cell) (nreverse (cdr cell)))) (nreverse out))))

(defun github-inline-target-problem (full-name number path line)
  "Return a human explanation if PATH:LINE cannot take an inline comment.
Nil means the target is fine.  GitHub rejects a bad target with an
opaque 422 after the comment was written; checking first lets the
message name the lines that are available."
  (let* ((ranges (github--diff-commentable-lines
                  (github-pull-request-diff full-name number)))
         (for-file (cdr (assoc path ranges))))
    (cond
     ((null ranges) nil)               ;; couldn't read the diff -- let the API judge
     ((null for-file)
      (format "%s is not among the files changed by PR #%s (changed: %s)"
              path number
              (mapconcat #'car ranges ", ")))
     ((not (seq-some (lambda (r) (and (>= line (car r)) (<= line (cdr r)))) for-file))
      (format "line %d of %s is not in PR #%s's diff (commentable lines: %s)"
              line path number
              (mapconcat (lambda (r)
                           (if (= (car r) (cdr r))
                               (number-to-string (car r))
                             (format "%d-%d" (car r) (cdr r))))
                         for-file ", "))))))

(defun github-create-comment (full-name number text &optional inline parent-id)
  "Create a comment on PR NUMBER in FULL-NAME with raw TEXT.
INLINE, when non-nil, is a cons (PATH . LINE) anchoring an inline
review comment on the new side of the diff (requires the PR's head
commit sha).  PARENT-ID, when non-nil, replies to that review
comment.  Otherwise this is a plain issue (general discussion)
comment."
  (cond
   (parent-id
    (github--reshape-review-comment
     (github-api-request
      "POST" (format "/repos/%s/pulls/%s/comments/%s/replies" full-name number parent-id)
      nil `((body . ,text)))))
   (inline
    (let* ((pr (github-pull-request full-name number))
           (sha (let-alist pr .head.sha)))
      (github--reshape-review-comment
       (github-api-request
        "POST" (format "/repos/%s/pulls/%s/comments" full-name number)
        nil `((body . ,text) (commit_id . ,sha)
              (path . ,(car inline)) (line . ,(cdr inline)) (side . "RIGHT"))))))
   (t
    (github--reshape-issue-comment
     (github-api-request
      "POST" (format "/repos/%s/issues/%s/comments" full-name number)
      nil `((body . ,text)))))))

;;;; Comment resolution (GraphQL only) -----------------------------------------

(defvar github--thread-id-cache (make-hash-table :test 'equal)
  "(FULL-NAME NUMBER COMMENT-ID) -> GraphQL review thread node id.")

(defun github--review-thread-id (full-name number comment-id)
  "Return the GraphQL node id of the review thread containing COMMENT-ID.
Queries and caches every thread's comment ids for PR NUMBER in
FULL-NAME the first time any of its comments needs resolving."
  (let ((key (list full-name number comment-id)))
    (or (gethash key github--thread-id-cache)
        (let* ((owner (car (split-string full-name "/")))
               (repo (cadr (split-string full-name "/")))
               (data (github-graphql-request
                      "query($owner:String!,$repo:String!,$number:Int!){
                         repository(owner:$owner,name:$repo){
                           pullRequest(number:$number){
                             reviewThreads(first:100){nodes{
                               id
                               comments(first:100){nodes{databaseId}}}}}}}"
                      `((owner . ,owner) (repo . ,repo) (number . ,number)))))
          (let-alist data
            (dolist (thread .repository.pullRequest.reviewThreads.nodes)
              (let ((tid (alist-get 'id thread)))
                (dolist (c (let-alist thread .comments.nodes))
                  (puthash (list full-name number (alist-get 'databaseId c)) tid
                           github--thread-id-cache)))))
          (gethash key github--thread-id-cache)))))

(defun github-resolve-comment (full-name number comment-id)
  "Mark the review thread containing COMMENT-ID on PR NUMBER as resolved.
Only GraphQL's `resolveReviewThread' can, and it takes the thread, so
COMMENT-ID is first mapped via `github--review-thread-id'.  Signals a
`user-error' when COMMENT-ID is in no review thread (e.g. a general
PR comment)."
  (let ((tid (github--review-thread-id full-name number comment-id)))
    (unless tid
      (user-error "Comment %s has no resolvable review thread (likely a general PR comment, not an inline review comment -- GitHub has no \"resolve\" concept for those)" comment-id))
    (github-graphql-request
     "mutation($id:ID!){resolveReviewThread(input:{threadId:$id}){thread{id}}}"
     `((id . ,tid)))))

(defun github-reopen-comment (full-name number comment-id)
  "Reopen (un-resolve) the review thread containing COMMENT-ID.
See `github-resolve-comment'."
  (let ((tid (github--review-thread-id full-name number comment-id)))
    (unless tid
      (user-error "Comment %s has no resolvable review thread" comment-id))
    (github-graphql-request
     "mutation($id:ID!){unresolveReviewThread(input:{threadId:$id}){thread{id}}}"
     `((id . ,tid)))))

(defun github-delete-comment (full-name number comment-id)
  "Delete COMMENT-ID on PR NUMBER in FULL-NAME.
The comment's kind is unknown, so this tries the review-comment
endpoint, then the issue-comment one, then
`github-delete-review-batch-comment' for a comment in the caller's
PENDING review (REST 404s on those)."
  (condition-case nil
      (github-api-request "DELETE" (format "/repos/%s/pulls/comments/%s" full-name comment-id))
    (error
     (condition-case nil
         (github-api-request "DELETE" (format "/repos/%s/issues/comments/%s" full-name comment-id))
       (error (github-delete-review-batch-comment full-name number comment-id))))))

(defun github--pending-comment-node-id (full-name number comment-id)
  "Return the GraphQL node id of pending COMMENT-ID (a databaseId).
GraphQL's review-comment mutations take the node id, not the databaseId
REST and this package use.  Signals a `user-error' when not found."
  (let ((review (github--pending-review full-name number)))
    (or (cl-loop for c in (append (let-alist review .comments.nodes) nil)
                 when (equal (alist-get 'databaseId c) comment-id)
                 return (alist-get 'id c))
        (user-error "Comment %s is not in your pending review" comment-id))))

(defun github-delete-review-batch-comment (full-name number comment-id)
  "Delete pending COMMENT-ID (a databaseId) from the caller's review batch."
  (let ((node-id (github--pending-comment-node-id full-name number comment-id)))
    (github-graphql-request
     "mutation($id:ID!){deletePullRequestReviewComment(input:{id:$id}){
        pullRequestReview{id}}}"
     `((id . ,node-id)))))

(defun github-edit-comment (full-name number comment-id text)
  "Replace COMMENT-ID's body with raw TEXT on PR NUMBER in FULL-NAME.
Tries the same endpoints in the same order as `github-delete-comment',
ending with `github-edit-review-batch-comment'."
  (condition-case nil
      (github--reshape-review-comment
       (github-api-request "PATCH" (format "/repos/%s/pulls/comments/%s" full-name comment-id)
                           nil `((body . ,text))))
    (error
     (condition-case nil
         (github--reshape-issue-comment
          (github-api-request "PATCH" (format "/repos/%s/issues/comments/%s" full-name comment-id)
                              nil `((body . ,text))))
       (error (github-edit-review-batch-comment full-name number comment-id text))))))

(defun github-edit-review-batch-comment (full-name number comment-id text)
  "Replace the body of pending COMMENT-ID (a databaseId) with TEXT."
  (let ((node-id (github--pending-comment-node-id full-name number comment-id)))
    (let ((data (github-graphql-request
                 "mutation($id:ID!,$body:String!){
                    updatePullRequestReviewComment(input:{
                      pullRequestReviewCommentId:$id,body:$body}){
                      pullRequestReviewComment{databaseId body path line
                        author{login avatarUrl} createdAt url}}}"
                 `((id . ,node-id) (body . ,text)))))
      (let-alist data
        (github--reshape-pending-comment
         (append .updatePullRequestReviewComment.pullRequestReviewComment `((id . ,node-id))))))))

;;;; Reviews (approve / request changes) ---------------------------------------

(defun github--dismiss-own-review (full-name number event reason)
  "Dismiss the authenticated user's latest review of EVENT state, if any.
Approximates \"unapprove\"/\"unrequest changes\", which GitHub cannot
retract; the dismissal shows on the PR timeline with REASON.  Signals
when no matching review exists."
  (let* ((login (github-user-login))
         (reviews (github--pr-reviews full-name number))
         (mine (cl-find-if (lambda (r) (and (equal (let-alist r .user.login) login)
                                            (equal (alist-get 'state r) event)))
                           reviews)))
    (unless mine
      (user-error "No %s review by you found to retract" (downcase event)))
    (github-api-request
     "PUT" (format "/repos/%s/pulls/%s/reviews/%s/dismissals" full-name number (alist-get 'id mine))
     nil `((message . ,(or reason "Dismissed via helm-git-platform"))))))

(defun github-approve-pr (full-name number &optional unapprove reason)
  "Approve PR NUMBER in FULL-NAME.
With UNAPPROVE non-nil, dismiss your latest APPROVED review instead,
with REASON as the dismissal message."
  (if unapprove
      (github--dismiss-own-review full-name number "APPROVED" reason)
    (github-api-request
     "POST" (format "/repos/%s/pulls/%s/reviews" full-name number)
     nil '((event . "APPROVE")))))

(defun github-request-changes-pr (full-name number &optional unrequest reason)
  "Request changes on PR NUMBER in FULL-NAME.
With UNREQUEST non-nil, dismiss your latest CHANGES_REQUESTED review,
with REASON as the dismissal message."
  (if unrequest
      (github--dismiss-own-review full-name number "CHANGES_REQUESTED" reason)
    (github-api-request
     "POST" (format "/repos/%s/pulls/%s/reviews" full-name number)
     nil '((event . "REQUEST_CHANGES") (body . "Changes requested.")))))

(defun github-review-retraction-kind ()
  "Return `dismiss': GitHub has no true review retraction.
Withdrawing leaves a visible dismissal event on the PR's timeline."
  'dismiss)

;;;; Review batch (pending review, GraphQL only) --------------------------------

;; REST cannot add a comment to an existing PENDING review; GraphQL's
;; `addPullRequestReviewThread' can, so the whole batch flow uses GraphQL.

(defun github--pr-node-id (full-name number)
  "Return the GraphQL node id of PR NUMBER in FULL-NAME."
  (alist-get 'node_id (github-pull-request full-name number)))

(defconst github--pending-review-query
  "query($owner:String!,$repo:String!,$number:Int!){
     repository(owner:$owner,name:$repo){
       pullRequest(number:$number){
         reviews(states:PENDING,first:1,author:\"me\"){nodes{
           id databaseId
           comments(first:100){nodes{
             id databaseId body path line
             author{login avatarUrl}
             createdAt url}}}}}}}"
  "GraphQL query behind `github--pending-review' and its async twin.
Each comment's node `id' is fetched for the review-comment mutations
\(see `github--pending-comment-node-id').")

(defun github--pending-review (full-name number)
  "Return the caller's own PENDING review object on PR NUMBER, or nil.
Never cached: a stale review id would 404 on submit or reuse a review
already submitted or deleted from the web UI."
  (let* ((owner (car (split-string full-name "/")))
         (repo (cadr (split-string full-name "/")))
         (data (github-graphql-request
                github--pending-review-query
                `((owner . ,owner) (repo . ,repo) (number . ,number)))))
    (let-alist data
      (car (append .repository.pullRequest.reviews.nodes nil)))))

(defun github--ensure-pending-review (full-name number)
  "Return the Node ID of the caller's PENDING review, creating one if needed."
  (or (alist-get 'id (github--pending-review full-name number))
      (let* ((pr-id (github--pr-node-id full-name number))
             (data (github-graphql-request
                    "mutation($pr:ID!){addPullRequestReview(input:{pullRequestId:$pr}){
                       pullRequestReview{id}}}"
                    `((pr . ,pr-id)))))
        (let-alist data .addPullRequestReview.pullRequestReview.id))))

(defun github--reshape-pending-comment (c)
  "Reshape a pending-review GraphQL comment node C into the shared shape.
Mirrors `github--reshape-review-comment', with `pending' set for
`github-comment-pending-p'.  REST does not surface pending comments."
  (let-alist c
    `((pending . t)
      (id . ,.databaseId)
      (node-id . ,.id)
      (content (raw . ,.body))
      (user (display_name . ,.author.login)
            (uuid . ,.author.login)
            (links (avatar (href . ,.author.avatarUrl))))
      (created_on . ,.createdAt)
      (links (html (href . ,.url)))
      (inline (path . ,.path) (from . ,.line) (to . ,.line)))))

(defun github-add-review-batch-comment (full-name number text inline &optional parent-id)
  "Add a PENDING inline comment on PR NUMBER in FULL-NAME to the caller's batch.
INLINE is a cons (PATH . LINE).  PARENT-ID (a databaseId) replies to
an existing review comment."
  (let* ((review-id (github--ensure-pending-review full-name number)))
    (if parent-id
        (github-graphql-request
         "mutation($review:ID!,$comment:Int!,$body:String!){
            addPullRequestReviewThreadReply(input:{
              pullRequestReviewId:$review,inReplyTo:$comment,body:$body}){
              comment{databaseId}}}"
         `((review . ,review-id) (comment . ,parent-id) (body . ,text)))
      (github-graphql-request
       "mutation($review:ID!,$path:String!,$line:Int!,$body:String!){
          addPullRequestReviewThread(input:{
            pullRequestReviewId:$review,path:$path,line:$line,body:$body}){
            thread{id}}}"
       `((review . ,review-id) (path . ,(car inline)) (line . ,(cdr inline))
         (body . ,text))))))

(defun github-review-batch-comments (full-name number)
  "Return the caller's pending comments on PR NUMBER in FULL-NAME."
  (let ((review (github--pending-review full-name number)))
    (mapcar #'github--reshape-pending-comment
            (append (let-alist review .comments.nodes) nil))))

(defun github-review-batch-comments-async (full-name number callback)
  "Async twin of `github-review-batch-comments'.  CALLBACK gets the list, or nil."
  (let ((owner (car (split-string full-name "/")))
        (repo (cadr (split-string full-name "/"))))
    (github-graphql-request-async
     github--pending-review-query
     `((owner . ,owner) (repo . ,repo) (number . ,number))
     (lambda (ok data)
       (funcall callback
                (when ok
                  (let-alist data
                    (mapcar #'github--reshape-pending-comment
                            (append (let-alist (car (append .repository.pullRequest.reviews.nodes nil))
                                      .comments.nodes)
                                    nil)))))))))

(defun github-submit-review-batch (full-name number &optional event body)
  "Submit the caller's pending review on PR NUMBER in FULL-NAME.
EVENT is `approve', `changes', `comment', or nil (treated as
`comment': GitHub requires an explicit event to submit at all).
BODY is an optional summary attached to the review."
  (let ((review-id (github--ensure-pending-review full-name number))
        (gh-event (pcase event
                    ('approve "APPROVE")
                    ('changes "REQUEST_CHANGES")
                    (_ "COMMENT"))))
    (github-graphql-request
     "mutation($review:ID!,$event:PullRequestReviewEvent!,$body:String){
        submitPullRequestReview(input:{
          pullRequestReviewId:$review,event:$event,body:$body}){
          pullRequestReview{id}}}"
     `((review . ,review-id) (event . ,gh-event) (body . ,body)))))

(defun github-discard-review-batch (full-name number)
  "Delete the caller's pending review on PR NUMBER in FULL-NAME, if any."
  (when-let* ((review (github--pending-review full-name number))
              (review-id (alist-get 'id review)))
    (github-graphql-request
     "mutation($review:ID!){deletePullRequestReview(input:{pullRequestReviewId:$review}){
        clientMutationId}}"
     `((review . ,review-id)))))

(defun github-comment-pending-p (comment)
  "Return non-nil if COMMENT is part of the caller's unsubmitted review batch."
  (eq (alist-get 'pending comment) t))

;;;; Diff / stats ---------------------------------------------------------------

(defun github-pull-request-diff (full-name number &optional _commit)
  "Return the unified diff text for PR NUMBER in FULL-NAME.
Served off the PR resource via content negotiation, so COMMIT is unused."
  (github-api-request-raw
   "GET" (format "/repos/%s/pulls/%s" full-name number) nil
   '(("Accept" . "application/vnd.github.v3.diff"))))

(defun github--diffstat-entry (f)
  "Return a plist (:path :status :added :removed) for `/files' entry F.
Same shape as `bitbucket--diffstat-entry'."
  (list :path (alist-get 'filename f)
        :status (alist-get 'status f)
        :added (or (alist-get 'additions f) 0)
        :removed (or (alist-get 'deletions f) 0)))

(defun github--commit-entry (c)
  "Normalise a GitHub PR commit C to (:hash :summary :author :date).
Same shape as `bitbucket--commit-entry'.  The author is the linked
account's login, else the raw git author name."
  (let ((name (or (let-alist c .author.login)
                  (let-alist c .commit.author.name))))
    (list :hash (alist-get 'sha c)
          :summary (github-commit-summary
                    (github--normalize-newlines (let-alist c .commit.message)))
          :author name
          :date (let-alist c .commit.author.date))))

(defun github-pull-request-commits-async (full-name number callback &optional max-items)
  "Fetch PR FULL-NAME/NUMBER's commits; CALLBACK gets normalised plists.
Returned newest-first (GitHub lists oldest-first), so MAX-ITEMS keeps
the newest commits."
  (if (not (and full-name number))
      (funcall callback nil)
    (github-api-paged-async
     (format "/repos/%s/pulls/%s/commits" full-name number)
     nil
     (lambda (ok values)
       (funcall callback
                (and ok (let ((newest-first (reverse values)))
                          (mapcar #'github--commit-entry
                                  (if max-items
                                      (seq-take newest-first max-items)
                                    newest-first))))))
     ;; no max-items here: the cap must apply after reversing
     nil)))

(defun github--stats-plist (pr files)
  "Return the stats plist for PR's totals and its `/files' entries FILES."
  (list :files (or (alist-get 'changed_files pr) 0)
        :added (or (alist-get 'additions pr) 0)
        :removed (or (alist-get 'deletions pr) 0)
        :commits (or (alist-get 'commits pr) 0)
        :file-list (mapcar #'github--diffstat-entry files)))

(defun github-pull-request-stats (full-name number &optional pr)
  "Return a plist (:files :added :removed :commits :file-list) for a PR.
Totals come from PR (fetched when nil); :file-list needs a separate
`/pulls/{number}/files' request."
  (let* ((pr (or pr (github-pull-request full-name number)))
         (files (ignore-errors (github-api-paged (format "/repos/%s/pulls/%s/files" full-name number)))))
    (github--stats-plist pr files)))

(defun github-api-get-raw-async (path callback &optional extra-headers)
  "GET PATH asynchronously and call CALLBACK with the raw body text.
CALLBACK receives the body, or nil on error (logged).  The
JSON-parsing twin is `github-api-get-async'."
  (let* ((url (github--build-url path nil))
         (url-request-method "GET")
         (auth (github--auth-header))
         (url-request-extra-headers `(,@(when auth (list auth)) ,@extra-headers))
         (start (float-time)))
    (url-retrieve
     url
     (lambda (status-plist)
       (let (result)
         (condition-case e
             (if-let* ((err (plist-get status-plist :error)))
                 (gp-log-error "async raw %s -> %S" path err)
               (pcase-let ((`(,code ,_headers ,body) (github--split-response (current-buffer))))
                 (when gp-log-requests
                   (gp-log (if (and (>= code 200) (< code 300)) 'http 'error)
                           "GET %s -> %d (%.0fms, async raw)"
                           path code (* 1000 (- (float-time) start))))
                 (setq result (cond ((equal code 404) "")
                                    ((and (>= code 200) (< code 300)) body)))))
           (error (gp-log-error "async raw %s: %s" path (error-message-string e))))
         (funcall callback result)))
     nil t t)))

(defun github-pull-request-diff-async (full-name number _commit _pr callback)
  "Fetch PR FULL-NAME/NUMBER's unified diff asynchronously.
CALLBACK gets the diff text, or nil on error.  The commit and PR
arguments exist for protocol parity and are unused."
  (github-api-get-raw-async
   (format "/repos/%s/pulls/%s" full-name number)
   callback
   '(("Accept" . "application/vnd.github.v3.diff"))))

(defun github-pull-request-stats-async (full-name number pr callback)
  "Fetch PR FULL-NAME/NUMBER's stats plist asynchronously.
CALLBACK gets the plist.  The totals come off PR itself; only the
per-file list is fetched."
  (github-api-paged-async
   (format "/repos/%s/pulls/%s/files" full-name number) nil
   (lambda (_ok files)
     (funcall callback (github--stats-plist pr files)))))

;;;; Commit statuses ------------------------------------------------------------

(defun github--status-state->bitbucket (state)
  "Translate a GitHub combined-status STATE string to Bitbucket vocabulary.
`gp-build-states-summary' (git-platform.el) only understands
SUCCESSFUL/FAILED/INPROGRESS/STOPPED."
  (pcase state
    ("success" "SUCCESSFUL")
    ("failure" "FAILED")
    ("error" "FAILED")
    ("pending" "INPROGRESS")
    (_ "STOPPED")))

(defun github--check-conclusion->bitbucket (status conclusion)
  "Translate one check run's STATUS/CONCLUSION to Bitbucket vocabulary.
A run that has not completed is INPROGRESS whatever CONCLUSION says.
`neutral' and `skipped' deliberately map to nil: they mean \"this
check declined to judge\", so they must not drag a green bubble to
grey -- a matrix leg skipped by a path filter is normal."
  (if (not (equal status "completed"))
      "INPROGRESS"
    (pcase conclusion
      ("success" "SUCCESSFUL")
      ((or "failure" "timed_out" "action_required" "startup_failure") "FAILED")
      ("cancelled" "STOPPED")
      (_ nil))))

(defun github-commit-check-states (full-name hash)
  "Return build state strings from the check runs for HASH in FULL-NAME.
The checks API is where GitHub Actions actually reports; a repo whose
CI posts no legacy commit statuses has results only here."
  (when hash
    (let ((res (ignore-errors
                 (github-api-request
                  "GET" (format "/repos/%s/commits/%s/check-runs" full-name hash)
                  '(("per_page" . "100"))))))
      (delq nil
            (mapcar (lambda (run)
                      (let-alist run
                        (github--check-conclusion->bitbucket .status .conclusion)))
                    (alist-get 'check_runs res))))))

(defun github-commit-build-states (full-name hash)
  "Return the build state strings for commit HASH in FULL-NAME.

Concatenates legacy commit statuses and check runs (where GitHub
Actions reports), since a repo may use either or both.

The combined-status endpoint answers \"pending\" with a zero
`total_count' when a repo posts no statuses; that is dropped rather
than reported as INPROGRESS, which would stick forever."
  (when hash
    (let* ((combined (ignore-errors
                       (github-api-request
                        "GET" (format "/repos/%s/commits/%s/status" full-name hash))))
           (n (or (alist-get 'total_count combined) 0))
           (status-states (when (> n 0)
                            (list (github--status-state->bitbucket
                                   (alist-get 'state combined))))))
      (append status-states (github-commit-check-states full-name hash)))))

(defun github-commit-message (full-name hash)
  "Return the commit message for HASH in FULL-NAME, or nil."
  (when (and full-name hash)
    (ignore-errors
      (github--normalize-newlines
       (let-alist (github-api-request "GET" (format "/repos/%s/commits/%s" full-name hash))
         .commit.message)))))

(defun github-commit-message-async (full-name hash callback)
  "Fetch the commit message for HASH in FULL-NAME; CALLBACK gets it (or nil).
Commit messages are immutable, so the result is cached; a warm entry
calls CALLBACK synchronously."
  (if (not (and full-name hash))
      (funcall callback nil)
    (let* ((key (list 'commit-msg full-name hash))
           (hit (gp-cache-get key)))
      (if (car hit)
          (funcall callback (cdr hit))
        (github-api-get-async
         (format "/repos/%s/commits/%s" full-name hash)
         nil
         (lambda (parsed)
           (let ((msg (github--normalize-newlines
                       (and parsed (let-alist parsed .commit.message)))))
             (when msg (gp-cache-put key msg))
             (funcall callback msg))))))))

(defun github-commit-summary (message)
  "Return the first non-empty line of commit MESSAGE, trimmed, or \"\"."
  (if (not message) "" (string-trim (car (split-string message "\n" t)))))

;;;; CI: GitHub Actions workflow runs (Bitbucket Pipelines analogue) -----------

;; CI mapping notes (see also the Commentary):
;;   * a workflow run  ~ a Bitbucket "pipeline"
;;   * a run's jobs    ~ a pipeline's "steps" (a job's own internal
;;     `steps' array is not surfaced)
;;   * triggering needs a workflow file/id: Actions has no default pipeline
;;   * no per-job "run this step" endpoint exists

(defun github-pipelines-for-branch (full-name branch &optional max-items commit)
  "Return workflow runs in FULL-NAME for BRANCH, newest first.
When COMMIT is non-nil, only runs whose head sha matches it are kept."
  (when (and full-name branch)
    (github-pipelines-match-commit
     (github-api-paged
      (format "/repos/%s/actions/runs" full-name)
      `(("branch" . ,branch))
      (or max-items 20))
     commit)))

(defun github-pipelines-for-branch-async (full-name branch max-items commit callback)
  "Fetch workflow runs in FULL-NAME for BRANCH; CALLBACK gets them (or nil).
Non-blocking twin of `github-pipelines-for-branch'."
  (if (not (and full-name branch))
      (funcall callback nil)
    (github-api-paged-async
     (format "/repos/%s/actions/runs" full-name)
     `(("branch" . ,branch))
     (lambda (ok values)
       (funcall callback
                (and ok (github-pipelines-match-commit values commit))))
     (or max-items 20))))

(defun github-pipeline-steps (full-name run-id)
  "Return the jobs of workflow RUN-ID in FULL-NAME, in order."
  (when (and full-name run-id)
    (alist-get 'jobs
               (github-api-request "GET" (format "/repos/%s/actions/runs/%s/jobs" full-name run-id)))))

(defun github-pipeline-steps-async (full-name run-id callback)
  "Fetch the jobs of workflow RUN-ID in FULL-NAME; CALLBACK gets them (or nil).
Non-blocking twin of `github-pipeline-steps'."
  (if (not (and full-name run-id))
      (funcall callback nil)
    (github-api-paged-async
     (format "/repos/%s/actions/runs/%s/jobs" full-name run-id)
     nil
     (lambda (ok values) (funcall callback (and ok values))))))

(defun github-pipeline-stop (full-name run-id)
  "Cancel workflow RUN-ID in FULL-NAME."
  (github-api-request "POST" (format "/repos/%s/actions/runs/%s/cancel" full-name run-id)))

(defun github-pipeline-trigger (full-name branch &optional selector variables)
  "Dispatch a workflow run in FULL-NAME for BRANCH.
SELECTOR, the workflow file (e.g. \"ci.yml\") or numeric id, is
required: Actions has no default pipeline.  VARIABLES becomes the
dispatch's `inputs'."
  (unless selector
    (user-error "GitHub Actions requires a workflow file name or id to trigger (pass SELECTOR)"))
  (let ((data `((ref . ,branch))))
    (when variables
      (setq data (append data
                         (list (cons 'inputs
                                     (mapcar (lambda (kv) (cons (intern (car kv)) (cdr kv)))
                                             variables))))))
    (github-api-request
     "POST" (format "/repos/%s/actions/workflows/%s/dispatches" full-name selector)
     nil data)))

(defun github-pipeline-run-manual-step (full-name branch pipeline step)
  "Signal that GitHub Actions has no per-job manual-run endpoint.
Re-dispatching the whole workflow is different enough that faking
it would mislead, so this always signals a `user-error'.  Distinct
from `github-pipeline-step-rerun', which GitHub does support."
  (ignore full-name branch pipeline step)
  (user-error "GitHub Actions has no per-job manual-run endpoint; re-dispatch the workflow instead"))

(defun github-pipeline-step-rerunnable-p (step)
  "Return non-nil if job STEP can be individually re-run in place.
Only a finished job can be re-run.  This is distinct from a manual
gate (see `github-pipeline-step-manual-p')."
  (equal (alist-get 'status step) "completed"))

(defun github-pipeline-step-rerun (full-name _run-id step)
  "Re-run job STEP (in FULL-NAME) in place via its rerun endpoint.
Signals a `user-error' when STEP has not finished yet."
  (unless (github-pipeline-step-rerunnable-p step)
    (user-error "Job %S has not finished yet; only a completed job can be re-run"
                (or (alist-get 'name step) "?")))
  (github-api-request
   "POST" (format "/repos/%s/actions/jobs/%s/rerun" full-name (alist-get 'id step))))

(defun github-pipeline-web-url (full-name pipeline &optional step)
  "Return the web-UI URL for workflow-run PIPELINE in FULL-NAME.
Deep-links to STEP's job when given."
  (concat github-web-base
          (format "/%s/actions/runs/%s" full-name (alist-get 'id pipeline))
          (when-let* ((jid (and step (alist-get 'id step))))
            (format "/job/%s" jid))))

(defun github-pipeline-step-log (full-name run-id job-id)
  "Return the captured log text for JOB-ID of RUN-ID in FULL-NAME.
The endpoint redirects to a plain-text blob, which is followed."
  (ignore run-id)
  (when (and full-name job-id)
    (github-api-request-raw "GET" (format "/repos/%s/actions/jobs/%s/logs" full-name job-id))))

(defconst github--log-timestamp-re
  "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\.[0-9]+Z "
  "Matches the RFC3339 timestamp GitHub Actions prefixes every log line with.")

(defun github-pipeline-step-log-classify-line (line)
  "Classify LINE for `gp-pipeline-step-log-classify-line'.
The timestamp prefix is stripped and \"##[group]\"-style workflow
command markers are recognised.  Actions has no per-command echo."
  (let* ((rest (if (string-match github--log-timestamp-re line)
                    (substring line (match-end 0))
                  line)))
    (cond
     ((string-prefix-p "##[group]" rest)
      (cons 'group (substring rest (length "##[group]"))))
     ((string-prefix-p "##[endgroup]" rest)
      (cons 'group ""))
     ((string-prefix-p "##[error]" rest)
      (cons 'error (substring rest (length "##[error]"))))
     ((string-prefix-p "##[warning]" rest)
      (cons 'warning (substring rest (length "##[warning]"))))
     ((string-prefix-p "##[notice]" rest)
      (cons 'warning (substring rest (length "##[notice]"))))
     (t (cons nil rest)))))

;;;; Pipeline pure helpers (shape-aware, no network) ---------------------------

(defun github-pipeline-commit (pipeline)
  "Return PIPELINE (workflow run)'s target commit hash, or nil."
  (alist-get 'head_sha pipeline))

(defun github-pipeline-state (pipeline)
  "Return PIPELINE's coarse state: QUEUED/PENDING/IN_PROGRESS/COMPLETED.
Translated from GitHub's `status' to the Bitbucket vocabulary."
  (pcase (alist-get 'status pipeline)
    ("completed" "COMPLETED")
    ("in_progress" "IN_PROGRESS")
    ("queued" "QUEUED")
    (_ "PENDING")))

(defun github-pipeline-result (pipeline)
  "Return PIPELINE's result string (SUCCESSFUL/FAILED/STOPPED/…) or nil.
Translated from GitHub's `conclusion'."
  (when-let* ((c (alist-get 'conclusion pipeline)))
    (pcase c
      ("success" "SUCCESSFUL")
      ("failure" "FAILED")
      ("cancelled" "STOPPED")
      ("timed_out" "FAILED")
      ("action_required" "FAILED")
      (_ (upcase c)))))

(defun github-pipeline-finished-p (pipeline)
  "Return non-nil when PIPELINE (workflow run) has finished."
  (equal (alist-get 'status pipeline) "completed"))

(defun github-pipeline-number (pipeline)
  "Return PIPELINE's run number, or nil."
  (alist-get 'run_number pipeline))

(defun github-pipelines-match-commit (pipelines commit)
  "Return the PIPELINES whose head sha matches COMMIT (prefix match either way)."
  (if (not commit)
      pipelines
    (cl-remove-if-not
     (lambda (p)
       (let ((h (github-pipeline-commit p)))
         (and h (or (string-prefix-p h commit) (string-prefix-p commit h)))))
     pipelines)))

(defun github-pipeline-step-state (step)
  "Return STEP (job)'s coarse state, as `github-pipeline-state' does."
  (github-pipeline-state step))

(defun github-pipeline-step-result (step)
  "Return STEP (job)'s result string, or nil."
  (when-let* ((c (alist-get 'conclusion step)))
    (pcase c
      ("success" "SUCCESSFUL")
      ("failure" "FAILED")
      ("cancelled" "STOPPED")
      (_ (upcase c)))))

(defun github-pipeline-step-running-p (step)
  "Return non-nil when STEP (job) is currently running."
  (equal (alist-get 'status step) "in_progress"))

(defun github-pipeline-step-manual-p (_step)
  "Return nil: Actions jobs expose no \"manual/gated\" concept."
  nil)

(defun github-pipeline-step-runnable-manual-p (_step)
  "Return nil, like `github-pipeline-step-manual-p'."
  nil)

(defun github-pipelines-sort (pipelines step-counts)
  "Return PIPELINES sorted by job count descending, then newest first."
  (let ((count-of
         (lambda (p)
           (let ((id (alist-get 'id p)))
             (or (if (hash-table-p step-counts)
                     (gethash id step-counts)
                   (cdr (assoc id step-counts)))
                 0)))))
    (sort (copy-sequence pipelines)
          (lambda (a b)
            (let ((ca (funcall count-of a)) (cb (funcall count-of b)))
              (if (= ca cb)
                  (string> (or (alist-get 'created_at a) "") (or (alist-get 'created_at b) ""))
                (> ca cb)))))))

(provide 'github-api)
;;; github-api.el ends here
