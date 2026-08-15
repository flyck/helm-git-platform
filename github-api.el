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
;; for the provider-agnostic TTL cache (`gp-cache-get'/`gp-cache-put'),
;; same dependency direction `bitbucket-api.el' already takes
(require 'git-platform)

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
instead of the environment.  Unlike Bitbucket, a token is optional:
unauthenticated requests are sent (without an Authorization header)
when none is configured, which works for read-only access to public
repos."
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

(defconst github--issue-search-fields "is:pr"
  "Base search-qualifier applied to every issue-search PR query.")

(defun github--full-name-of-search-hit (hit)
  "Return \"owner/repo\" for search HIT (an /search/issues item).
The search API only gives the issue's `repository_url', not a
`repository' object, so the owner/repo has to be parsed out of it."
  (let ((url (alist-get 'repository_url hit)))
    (when (and url (string-match "/repos/\\(.+\\)\\'" url))
      (match-string 1 url))))

(defun github--search-pull-requests (q &optional max-items)
  "Run issue-search Q (a GitHub search qualifier string), fetch full PRs.
The search API returns issue-shaped hits, not full PR objects (no
head/base branch, no requested reviewers, no draft flag) -- so each
hit is followed up with a real PR fetch.  This costs one extra
request per result, but there is no server-side field projection
like Bitbucket's `fields' parameter to avoid it."
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
apart afterwards via `github-pr-draft-p'/`merged_at'."
  (let* ((login (or login (github-user-login)))
         (open-p (or (null state) (equal state "OPEN")))
         (q (format "is:pr author:%s %s" login (if open-p "is:open" "is:closed"))))
    (github--search-pull-requests q max-items)))

(defun github-reviewing-pull-requests (&optional login limit states)
  "Return PRs across GitHub where LOGIN is a requested reviewer.
LIMIT is accepted for signature parity with the Bitbucket op (which
uses it to cap a repo scan) but GitHub's search already covers the
whole account in one query, so it is applied as MAX-ITEMS instead.
STATES nil/(\"OPEN\") means open only; `all' means both."
  (let* ((login (or login (github-user-login)))
         (open-p (or (null states) (equal states '("OPEN"))))
         (q (format "is:pr review-requested:%s%s"
                    login (if open-p " is:open" ""))))
    (github--search-pull-requests q limit)))

(defun github-reviewing-pull-requests-async (login states on-batch on-done &optional limit)
  "Async twin of `github-reviewing-pull-requests'.
GitHub's search is already a single global query (unlike Bitbucket's
per-repo fan-out), so this just runs synchronously on a timer and
delivers everything as one batch.  Uses a wall-clock `run-at-time',
NOT `run-with-idle-timer' -- see `github-pull-request-comments-async'
for why: ongoing `url-retrieve' I/O elsewhere can starve idle timers
scheduled around the same moment, silently never firing this scan."
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
GitHub has no single \"every open PR\" query scoped usefully without
naming an org/user, so this searches PRs involving the authenticated
user as a broad stand-in (mirrors `github-reviewing-pull-requests'
without the reviewer restriction) -- good enough for the \"others'
open PRs\" list in a personal/demo setting.  Uses a wall-clock
`run-at-time', NOT `run-with-idle-timer' -- see
`github-pull-request-comments-async' for why."
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
  "Return PR with `id' overwritten to hold its per-repo `number'.
Every `gp-*' caller (approve, comments, draft-toggle, the detail
view's re-fetch, …) treats `(alist-get \\='id pr)' as THE identifier
to hand back to the backend's own endpoints -- correct for Bitbucket,
where `id' genuinely is the value its PR URLs take. GitHub's PR `id'
is a global, opaque database id that no per-repo endpoint accepts;
the value every GitHub endpoint actually wants is `number'. Rather
than change every `id' call site in gp-ui.el, every PR object handed
back across the protocol boundary is reshaped here so `id' already
holds `number' -- the rest of the app never needs to know GitHub
draws this distinction.  The original database id survives under
`gh-database-id' in case something ever needs it."
  (when pr
    (let ((number (alist-get 'number pr)))
      (if (not number)
          pr
        (cons (cons 'id number)
              (cons (cons 'gh-database-id (alist-get 'id pr))
                    (assq-delete-all 'id (assq-delete-all 'gh-database-id (copy-alist pr)))))))))

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
requested but who has not submitted a review at all (pending).
Shared by `github-pr-review-tally' (sync) and
`github-pr-review-tally-async' so the two can never drift."
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
Fetches synchronously -- see `gp-pr-review-tally-async' for a
non-blocking twin suited to rendering many PRs at once."
  (let* ((full-name (let-alist pr .base.repo.full_name))
         (number (alist-get 'number pr))
         (reviews (and full-name number (github--pr-reviews full-name number)))
         (requested (mapcar (lambda (u) (alist-get 'login u))
                            (alist-get 'requested_reviewers pr))))
    (github--review-tally-from reviews requested)))

(defun github-pr-review-tally-async (pr callback)
  "Fetch PR's review tally asynchronously; CALLBACK gets the plist.
Non-blocking twin of `github-pr-review-tally'."
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

:ID is the login -- the identifier
`github-set-pull-request-reviewers' takes -- so a reviewer shown in
the UI maps back to an API identity.  It equals :NAME here (GitHub
has no separate display name on these payloads); the two are kept
distinct because the Bitbucket side's :ID is an opaque uuid."
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
See `gp-pr-reviewers-async'."
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

(defun github-set-pull-request-draft (full-name number draft &optional _title)
  "Set PR NUMBER in FULL-NAME's draft flag to DRAFT.
GitHub's REST API has no way to toggle draft state at all -- both
directions are GraphQL-only: `markPullRequestReadyForReview' takes a
PR out of draft, `convertPullRequestToDraft' puts a ready PR back
into draft.  (The GitHub web UI does offer \"Convert to draft\" on an
open PR, which is this same mutation -- there is no REST-visible gap
here, just a REST/GraphQL split, same as comment resolution.)"
  (let* ((pr (github-pull-request full-name number))
         (node-id (alist-get 'node_id pr))
         (mutation (if draft
                       "mutation($id:ID!){convertPullRequestToDraft(input:{pullRequestId:$id}){pullRequest{id}}}"
                     "mutation($id:ID!){markPullRequestReadyForReview(input:{pullRequestId:$id}){pullRequest{id}}}")))
    (github-graphql-request mutation `((id . ,node-id)))
    (github-pull-request full-name number)))

(defun github-open-pr-for-branch (full-name branch)
  "Return the open PR in FULL-NAME whose head branch is BRANCH, or nil."
  (github--reshape-pr
   (car (github-api-paged
         (format "/repos/%s/pulls" full-name)
         `(("state" . "open") ("head" . ,(format "%s:%s"
                                                 (car (split-string full-name "/"))
                                                 branch)))
         1))))

(defun github-repo-default-branch (full-name)
  "Return repo FULL-NAME's default branch name, or nil."
  (ignore-errors
    (alist-get 'default_branch (github-api-request "GET" (format "/repos/%s" full-name)))))

(cl-defun github-create-pull-request
    (full-name source dest title
     &key description draft close-source-branch reviewer-uuids)
  "Open a pull request in FULL-NAME from branch SOURCE into DEST.
CLOSE-SOURCE-BRANCH is accepted for signature parity with Bitbucket
but has no GitHub equivalent at creation time (GitHub always leaves
branch deletion to a separate, later action) and is ignored.
REVIEWER-UUIDS are GitHub logins despite the Bitbucket-flavoured
parameter name -- the shared protocol names it generically."
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
GitHub has no whole-list endpoint here: `requested_reviewers' is
mutated by POSTing additions and DELETEing removals.  So the desired
end state is diffed against CURRENT-LOGINS (the PR's present
reviewers) and only the difference is sent -- re-POSTing an existing
reviewer would needlessly re-notify them.

Removing a login that already submitted a review has no effect: the
review stays attached to the PR and GitHub does not treat the author
as a requested reviewer anymore.  Callers must therefore not offer to
remove reviewed people (`gp-ui-edit-reviewers' locks them).

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
  "Return repo FULL-NAME's default reviewers.
Always nil: GitHub has no repo-level \"default reviewers\" endpoint
comparable to Bitbucket's (the closest concept, CODEOWNERS, is a
file-based routing rule, not a queryable list of users), and an
empty list is a legitimate, non-erroring answer here, unlike an
actual capability gap that would need a `user-error' instead."
  (ignore full-name)
  nil)

(defun github-repo-suggested-reviewers (full-name)
  "Return repo FULL-NAME's collaborators as reviewer suggestions.
GitHub's real \"suggested reviewers\" (GraphQL `suggestedReviewers',
based on blame/recent-review history) is a field on an *existing*
PullRequest, so it cannot populate a pre-PR create form -- there is
no PR yet to compute suggestions against.  As a pre-PR approximation,
this lists the repo's collaborators instead (`GET
/repos/{owner}/{repo}/collaborators'), excluding the authenticated
user (you can't request a review from yourself).  Reshaped to the
same alist shape as `github-repo-default-reviewers' would use
\(`uuid' = login, `display_name' = name or login) so callers don't
need to special-case the source.  Errors are swallowed -- an
unreadable collaborator list should degrade to \"no suggestions\",
same as `github-repo-default-reviewers', not break the form.

A non-empty result is cached (`gp-cache-ttl') so reopening the create
form doesn't re-page the collaborator list; an empty one is not, so a
transient failure doesn't stick as \"nobody to suggest\"."
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

(defun github--refresh-resolved-threads (full-name number)
  "Query GraphQL for PR NUMBER's resolved review threads; cache their comment ids.
Populates `github--resolved-thread-comment-ids'.  Swallows errors
(GraphQL needs a token) so plain REST comment listing still works
without one; resolution just won't be reflected."
  (ignore-errors
    (let* ((owner (car (split-string full-name "/")))
           (repo (cadr (split-string full-name "/")))
           (data (github-graphql-request
                  "query($owner:String!,$repo:String!,$number:Int!){
                     repository(owner:$owner,name:$repo){
                       pullRequest(number:$number){
                         reviewThreads(first:100){nodes{
                           isResolved
                           comments(first:100){nodes{databaseId}}}}}}}"
                  `((owner . ,owner) (repo . ,repo) (number . ,number)))))
      (let-alist data
        (dolist (thread .repository.pullRequest.reviewThreads.nodes)
          (when (alist-get 'isResolved thread)
            (dolist (c (let-alist thread .comments.nodes))
              (puthash (alist-get 'databaseId c) t github--resolved-thread-comment-ids))))))))

(defun github-pull-request-comments (full-name number &optional max-items)
  "Return the merged (issue + review) comments for PR NUMBER in FULL-NAME.
Each is reshaped into a Bitbucket-like alist so the shared
`gp--comment-resolved-p'/`gp--comment-own-p' accessors work
unchanged: `content.raw', `user.display_name', `user.uuid' (the
login), `resolution' (present when GraphQL reports the comment's
review thread resolved -- see `github--refresh-resolved-threads'),
and, for inline comments, `inline.path'/`inline.to'."
  (github--refresh-resolved-threads full-name number)
  (let* ((issue (mapcar #'github--reshape-issue-comment (github--issue-comments full-name number)))
         (review (mapcar #'github--reshape-review-comment (github--review-comments full-name number)))
         (all (append issue review)))
    (if max-items (cl-subseq all 0 (min max-items (length all))) all)))

(defun github-pull-request-comments-async (full-name number callback &optional max-items)
  "Async twin of `github-pull-request-comments'.  CALLBACK gets (OK COMMENTS).
Uses a wall-clock `run-at-time', NOT `run-with-idle-timer' -- this is
typically fired alongside `github-pull-request-async', whose
`url-retrieve' network I/O can keep Emacs from ever registering a
fresh idle period in the right window, in which case an idle timer
here would silently never fire and the caller's pending-count would
never reach zero (see the identical note on `gp--detail-load-stats-diff'
in gp-ui.el, and `gp--detail-load-pipelines', which hit this exact bug)."
  (run-at-time
   0 nil
   (lambda ()
     (condition-case e
         (funcall callback t (github-pull-request-comments full-name number max-items))
       (error
        (gp-log-error "github comments fetch: %s" (error-message-string e))
        (funcall callback nil nil))))))

(defun github--reshape-issue-comment (c)
  "Reshape a REST issue comment C into the shared comment alist shape."
  `((id . ,(alist-get 'id c))
    (content (raw . ,(alist-get 'body c)))
    (user (display_name . ,(let-alist c .user.login))
          (uuid . ,(let-alist c .user.login))
          (links (avatar (href . ,(let-alist c .user.avatar_url)))))
    (created_on . ,(alist-get 'created_at c))
    (links (html (href . ,(alist-get 'html_url c))))
    ,@(when (gethash (alist-get 'id c) github--resolved-thread-comment-ids)
        '((resolution (user (display_name . "GitHub")))))))

(defun github--reshape-review-comment (c)
  "Reshape a REST review (inline) comment C into the shared alist shape."
  `((id . ,(alist-get 'id c))
    (content (raw . ,(alist-get 'body c)))
    (user (display_name . ,(let-alist c .user.login))
          (uuid . ,(let-alist c .user.login))
          (links (avatar (href . ,(let-alist c .user.avatar_url)))))
    (created_on . ,(alist-get 'created_at c))
    (links (html (href . ,(alist-get 'html_url c))))
    (inline (path . ,(alist-get 'path c))
            (from . ,(alist-get 'line c))
            (to . ,(alist-get 'line c)))
    (parent . ,(when-let* ((pid (alist-get 'in_reply_to_id c))) `((id . ,pid))))
    ,@(when (gethash (alist-get 'id c) github--resolved-thread-comment-ids)
        '((resolution (user (display_name . "GitHub")))))))

(defun github-comment-resolved-p (comment)
  "Return non-nil if COMMENT's review thread was reported resolved."
  (and (alist-get 'resolution comment) t))

(defun github-comment-resolvable-p (comment)
  "Return non-nil if COMMENT belongs to a resolvable review thread.
Only inline review comments do -- reshaped by
`github--reshape-review-comment', which is the only place that adds
an `inline' key.  Plain issue (general discussion) comments, from
`github--reshape-issue-comment', have no `inline' key and no
GraphQL review thread, so GitHub has no \"resolve\" concept for
them at all."
  (and (alist-get 'inline comment) t))

(defun github-comment-own-p (comment login)
  "Return non-nil if COMMENT was written by LOGIN."
  (equal (let-alist comment .user.uuid) login))

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
REST v3 has no endpoint for this at all -- only GraphQL's
`resolveReviewThread' mutation can, and it operates on the *thread*,
not the comment, so the comment id is first mapped to its thread's
GraphQL node id (see `github--review-thread-id').  Signals a clear
`user-error' when COMMENT-ID is not part of any review thread (e.g.
it is a plain issue comment, which GitHub has no concept of
\"resolving\")."
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
Tries the review-comments endpoint first, falling back to the issue
comment one, since the caller only has a comment id and not which
kind it is."
  (ignore number)
  (condition-case nil
      (github-api-request "DELETE" (format "/repos/%s/pulls/comments/%s" full-name comment-id))
    (error
     (github-api-request "DELETE" (format "/repos/%s/issues/comments/%s" full-name comment-id)))))

(defun github-edit-comment (full-name number comment-id text)
  "Replace COMMENT-ID's body with raw TEXT on PR NUMBER in FULL-NAME.
Tries the review-comments endpoint first, then falls back to the
issue comment endpoint (same ambiguity as `github-delete-comment')."
  (ignore number)
  (condition-case nil
      (github--reshape-review-comment
       (github-api-request "PATCH" (format "/repos/%s/pulls/comments/%s" full-name comment-id)
                           nil `((body . ,text))))
    (error
     (github--reshape-issue-comment
      (github-api-request "PATCH" (format "/repos/%s/issues/comments/%s" full-name comment-id)
                          nil `((body . ,text)))))))

;;;; Reviews (approve / request changes) ---------------------------------------

(defun github--dismiss-own-review (full-name number event reason)
  "Dismiss the authenticated user's latest review of EVENT state, if any.
Used to approximate \"unapprove\"/\"unrequest changes\", which GitHub
has no direct retraction for -- dismissing is the closest action,
though it is semantically a different thing (a dismissal is visible
in the PR timeline as an explicit event carrying REASON, not a
silent retraction -- see `gp-review-retraction-kind').  Signals when
no matching review can be found to dismiss."
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
With UNAPPROVE non-nil, dismiss your latest APPROVED review instead
of retracting it outright, with REASON as the dismissal message --
see `github--dismiss-own-review'."
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
Withdrawing a review here always goes through
`github--dismiss-own-review', which leaves a visible dismissal event
on the PR's timeline rather than making the review disappear."
  'dismiss)

;;;; Diff / stats ---------------------------------------------------------------

(defun github-pull-request-diff (full-name number &optional _commit)
  "Return the unified diff text for PR NUMBER in FULL-NAME.
GitHub serves the diff directly off the PR resource via content
negotiation -- no signed-URL indirection like Bitbucket needs."
  (github-api-request-raw
   "GET" (format "/repos/%s/pulls/%s" full-name number) nil
   '(("Accept" . "application/vnd.github.v3.diff"))))

(defun github--diffstat-entry (f)
  "Return a plist (:path :status :added :removed) for `/files' entry F.
Mirrors `bitbucket--diffstat-entry''s shape so `gp--insert-changed-files'
in gp-ui.el needs no backend-specific handling."
  (list :path (alist-get 'filename f)
        :status (alist-get 'status f)
        :added (or (alist-get 'additions f) 0)
        :removed (or (alist-get 'deletions f) 0)))

(defun github-pull-request-stats (full-name number &optional pr)
  "Return a plist (:files :added :removed :commits :file-list) for a PR.
:file-list is fetched separately via `/pulls/{number}/files' -- the PR
resource's own `changed_files'/`additions'/`deletions' are just totals,
with no per-file breakdown (unlike Bitbucket's diffstat endpoint,
whose response IS the per-file list)."
  (let* ((pr (or pr (github-pull-request full-name number)))
         (files (ignore-errors (github-api-paged (format "/repos/%s/pulls/%s/files" full-name number)))))
    (list :files (or (alist-get 'changed_files pr) 0)
          :added (or (alist-get 'additions pr) 0)
          :removed (or (alist-get 'deletions pr) 0)
          :commits (or (alist-get 'commits pr) 0)
          :file-list (mapcar #'github--diffstat-entry files))))

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

(defun github-commit-build-states (full-name hash)
  "Return the build state strings for commit HASH in FULL-NAME.
Reads the combined status (statuses API); GitHub Actions results
also appear here as ordinary commit statuses when the repo has any
status-posting integration/check enabled."
  (when hash
    (let ((combined (ignore-errors
                      (github-api-request
                       "GET" (format "/repos/%s/commits/%s/status" full-name hash)))))
      (when combined
        (list (github--status-state->bitbucket (alist-get 'state combined)))))))

(defun github-commit-message (full-name hash)
  "Return the commit message for HASH in FULL-NAME, or nil."
  (when (and full-name hash)
    (ignore-errors
      (let-alist (github-api-request "GET" (format "/repos/%s/commits/%s" full-name hash))
        .commit.message))))

(defun github-commit-message-async (full-name hash callback)
  "Fetch the commit message for HASH in FULL-NAME; CALLBACK gets it (or nil).
Non-blocking twin of `github-commit-message'.  Commit messages are
immutable, so the result is cached under the same key shape the
Bitbucket side uses; a warm entry calls CALLBACK synchronously."
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
           (let ((msg (and parsed (let-alist parsed .commit.message))))
             (when msg (gp-cache-put key msg))
             (funcall callback msg))))))))

(defun github-commit-summary (message)
  "Return the first non-empty line of commit MESSAGE, trimmed, or \"\"."
  (if (not message) "" (string-trim (car (split-string message "\n" t)))))

;;;; CI: GitHub Actions workflow runs (Bitbucket Pipelines analogue) -----------

;; Mapping notes (see also the Commentary block at the top of this file):
;;   * a workflow run  ~ a Bitbucket "pipeline"
;;   * a run's jobs    ~ a pipeline's "steps" (one level; a job's own
;;     internal `steps' array is not surfaced, to keep the mapping simple)
;;   * triggering requires a workflow id/filename -- there is no
;;     "run whatever the branch's default pipeline is" concept in
;;     Actions, so `github-pipeline-trigger' requires SELECTOR (used as
;;     the workflow file name, e.g. "ci.yml") and errors clearly without it
;;   * there is no per-job "run this step" endpoint; the closest is
;;     re-dispatching the whole workflow, which is not the same thing, so
;;     `github-pipeline-run-manual-step' user-errors instead of faking it

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
  "Return the jobs of workflow RUN-ID in FULL-NAME, in order.
Mapped 1:1 to Bitbucket \"steps\" -- see the Commentary/CI note above."
  (when (and full-name run-id)
    (alist-get 'jobs
               (github-api-request "GET" (format "/repos/%s/actions/runs/%s/jobs" full-name run-id)))))

(defun github-pipeline-steps-async (full-name run-id callback)
  "Fetch the jobs of workflow RUN-ID in FULL-NAME; CALLBACK gets them (or nil).
Non-blocking twin of `github-pipeline-steps'.  `github--unwrap-page'
already unwraps the `jobs' envelope, so the collected values are the
job list itself."
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
SELECTOR names the workflow file (e.g. \"ci.yml\") or numeric
workflow id -- REQUIRED, since Actions (unlike Bitbucket Pipelines)
has no single default-pipeline concept to fall back to when omitted.
VARIABLES becomes the dispatch's `inputs'."
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
Bitbucket gates a step and lets the API start just that step;
Actions has no equivalent -- the closest analogues (re-dispatching
the whole workflow, or approving a deployment environment) are
different enough in effect that faking this would mislead the user,
so this always errors clearly instead.  Note this is a different
capability from `github-pipeline-step-rerun', which GitHub DOES
support (re-running a single already-finished job)."
  (ignore full-name branch pipeline step)
  (user-error "GitHub Actions has no per-job manual-run endpoint; re-dispatch the workflow instead"))

(defun github-pipeline-step-rerunnable-p (step)
  "Return non-nil if job STEP can be individually re-run in place.
GitHub Actions supports rerunning a single job via
`POST /actions/jobs/{job_id}/rerun', but only once it has actually
finished -- a queued or in-progress job has nothing to rerun yet.
This is a real, distinct capability from Bitbucket's \"manual gate\"
concept (see `github-pipeline-step-manual-p'): it restarts a
finished (typically failed) job, not a step waiting for its first
run, so it never reports as manual/runnable-manual."
  (equal (alist-get 'status step) "completed"))

(defun github-pipeline-step-rerun (full-name _run-id step)
  "Re-run job STEP (in FULL-NAME) in place via its rerun endpoint.
Signals a clear `user-error' when STEP has not finished yet, since
GitHub's rerun endpoint only applies to a completed job."
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
The endpoint redirects to a plain-text blob; `url-retrieve-synchronously'
follows redirects transparently, so this reads the final body as text."
  (ignore run-id)
  (when (and full-name job-id)
    (github-api-request-raw "GET" (format "/repos/%s/actions/jobs/%s/logs" full-name job-id))))

;;;; Pipeline pure helpers (shape-aware, no network) ---------------------------

(defun github-pipeline-commit (pipeline)
  "Return PIPELINE (workflow run)'s target commit hash, or nil."
  (alist-get 'head_sha pipeline))

(defun github-pipeline-state (pipeline)
  "Return PIPELINE's coarse state: PENDING/IN_PROGRESS/COMPLETED.
Translated from GitHub's `status' (queued/in_progress/completed) to
the vocabulary the UI already expects from Bitbucket."
  (pcase (alist-get 'status pipeline)
    ("completed" "COMPLETED")
    ("in_progress" "IN_PROGRESS")
    ("queued" "PENDING")
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
  "Non-nil when PIPELINE (workflow run) has finished."
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
  "Return STEP (job)'s coarse state string (PENDING/IN_PROGRESS/COMPLETED/…)."
  (pcase (alist-get 'status step)
    ("completed" "COMPLETED")
    ("in_progress" "IN_PROGRESS")
    ("queued" "PENDING")
    (_ "PENDING")))

(defun github-pipeline-step-result (step)
  "Return STEP (job)'s result string, or nil."
  (when-let* ((c (alist-get 'conclusion step)))
    (pcase c
      ("success" "SUCCESSFUL")
      ("failure" "FAILED")
      ("cancelled" "STOPPED")
      (_ (upcase c)))))

(defun github-pipeline-step-running-p (step)
  "Non-nil when STEP (job) is currently running."
  (equal (alist-get 'status step) "in_progress"))

(defun github-pipeline-step-manual-p (_step)
  "Always nil: GitHub Actions jobs have no \"manual/gated\" concept exposed here.
See the CI mapping note at the top of the Actions section."
  nil)

(defun github-pipeline-step-runnable-manual-p (_step)
  "Always nil -- see `github-pipeline-step-manual-p'."
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
