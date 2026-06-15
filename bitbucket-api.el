;;; bitbucket-api.el --- Bitbucket Cloud API layer -*- lexical-binding: t; -*-

;; Author: Felix Brilej
;; Keywords: tools, vc
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Thin, testable layer over the Bitbucket Cloud REST API (2.0).
;;
;; All network access funnels through `bitbucket-api-request', which can be
;; rebound (e.g. with `cl-letf' in tests) to a mock service so that the
;; higher-level functions in this file and in `bitbucket.el' can be unit
;; tested without touching the network.  See tests/bitbucket-mock.el.
;;
;; Credentials are read from the environment by default
;; (BITBUCKET_USER_EMAIL, BITBUCKET_API_TOKEN, BITBUCKET_WORKSPACE) so they
;; can live in ~/.zshrc, but every value can be overridden via customize.

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)
(require 'auth-source)
(require 'gp-log)

(defgroup bitbucket nil
  "Browse Bitbucket Cloud pull requests from Emacs."
  :group 'tools
  :prefix "bitbucket-")

(defcustom bitbucket-api-host "api.bitbucket.org"
  "Host of the Bitbucket REST API.
Defaults to Bitbucket Cloud.  Override for a different deployment."
  :type 'string)

(defcustom bitbucket-api-base "https://api.bitbucket.org/2.0"
  "Base URL for the Bitbucket REST API (v2.0).
Defaults to Bitbucket Cloud."
  :type 'string)

(defcustom bitbucket-workspace-env "BITBUCKET_WORKSPACE"
  "Name of the environment variable holding the default workspace slug."
  :type 'string)

(defcustom bitbucket-user-email-env "BITBUCKET_USER_EMAIL"
  "Name of the environment variable holding the default account email."
  :type 'string)

(defcustom bitbucket-api-token-env "BITBUCKET_API_TOKEN"
  "Name of the environment variable holding the default API token."
  :type 'string)

(defcustom bitbucket-workspace nil
  "Bitbucket workspace slug.
When nil, read at runtime from the variable named by
`bitbucket-workspace-env'.  Set this via use-package to pin a
workspace without relying on the environment."
  :type '(choice (const :tag "From environment" nil) string))

(defcustom bitbucket-user-email nil
  "Atlassian account email used for Basic auth.
When nil, read at runtime from `bitbucket-user-email-env'."
  :type '(choice (const :tag "From environment" nil) string))

(defcustom bitbucket-api-token nil
  "Bitbucket / Atlassian API token used for Basic auth.

When nil, falls back first to the variable named by
`bitbucket-api-token-env', then to `auth-source' (host
`bitbucket-api-host', user `bitbucket-user-email') so you can keep
the token in ~/.authinfo.gpg instead of the environment."
  :type '(choice (const :tag "From environment / auth-source" nil) string))

(defun bitbucket-workspace-value ()
  "Return the configured workspace slug, or signal if unset."
  (or bitbucket-workspace
      (getenv bitbucket-workspace-env)
      (user-error "No Bitbucket workspace set (bitbucket-workspace or $%s)"
                  bitbucket-workspace-env)))

(defun bitbucket-user-email-value ()
  "Return the configured account email, or nil."
  (or bitbucket-user-email (getenv bitbucket-user-email-env)))

(defcustom bitbucket-request-timeout 20
  "Seconds to wait for a synchronous API response before giving up."
  :type 'integer)

(defvar bitbucket--uuid-cache nil
  "Cached UUID of the authenticated user, as a \"{...}\" string.")

;;;; Credentials ------------------------------------------------------------

(defun bitbucket-api-token-value ()
  "Return the API token, consulting the environment then `auth-source'."
  (or bitbucket-api-token
      (getenv bitbucket-api-token-env)
      (let ((found (car (auth-source-search
                         :host bitbucket-api-host
                         :user (bitbucket-user-email-value)
                         :max 1))))
        (when found
          (let ((secret (plist-get found :secret)))
            (if (functionp secret) (funcall secret) secret))))))

(defun bitbucket--auth-header ()
  "Return the Basic auth header value for the configured credentials."
  (let ((email (bitbucket-user-email-value))
        (token (bitbucket-api-token-value)))
    (unless (and email token)
      (user-error "Bitbucket credentials missing: set bitbucket-user-email and bitbucket-api-token"))
    (concat "Basic "
            (base64-encode-string (concat email ":" token) t))))

;;;; Low-level request ------------------------------------------------------

(defun bitbucket--encode-query (params)
  "Encode PARAMS, an alist of (KEY . VALUE), as a URL query string.
Both keys and values are percent-encoded.  Nil values are dropped."
  (mapconcat
   (lambda (kv)
     (concat (url-hexify-string (format "%s" (car kv)))
             "="
             (url-hexify-string (format "%s" (cdr kv)))))
   (cl-remove-if-not #'cdr params)
   "&"))

(defun bitbucket--build-url (path params)
  "Build a full request URL from PATH and PARAMS.
PATH may be an absolute URL (used verbatim, e.g. a paginated
\"next\" link) or a path relative to `bitbucket-api-base'."
  (let ((base (if (string-prefix-p "http" path)
                  path
                (concat bitbucket-api-base
                        (if (string-prefix-p "/" path) "" "/")
                        path))))
    (if params
        (concat base (if (string-search "?" base) "&" "?")
                (bitbucket--encode-query params))
      base)))

(defun bitbucket-api-request (method path &optional params data)
  "Perform a synchronous Bitbucket API request and return parsed JSON.

METHOD is a string like \"GET\" or \"POST\".  PATH is a relative
API path or an absolute URL.  PARAMS is an alist of query
parameters.  DATA, when non-nil, is a Lisp object serialised as a
JSON request body.

The return value is the parsed JSON with objects as alists,
arrays as lists, and null as nil.  Signals an error on transport
or HTTP failures.

This is the single network choke-point: redefine it (see the
test mock) to run the whole package offline."
  (let* ((url (bitbucket--build-url path params))
         (url-request-method method)
         (url-request-extra-headers
          `(("Authorization" . ,(bitbucket--auth-header))
            ("Accept" . "application/json")
            ,@(when data '(("Content-Type" . "application/json")))))
         (url-request-data
          (when data (encode-coding-string (json-encode data) 'utf-8)))
         (start (float-time))
         (buf (url-retrieve-synchronously url t t bitbucket-request-timeout)))
    (unless buf
      (gp-log-error "%s %s -> TIMEOUT (%ss)" method url
                           bitbucket-request-timeout)
      (error "Bitbucket request timed out: %s %s" method url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (let ((status (if (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                            (string-to-number (match-string 1))
                          0)))
            (re-search-forward "\n\n" nil t)
            ;; The response buffer holds raw (unibyte) bytes; decode as
            ;; UTF-8 so multibyte content (umlauts, emoji, …) survives.
            (let* ((body (decode-coding-string
                          (buffer-substring-no-properties (point) (point-max))
                          'utf-8))
                   (parsed (bitbucket--parse-json body)))
              (when gp-log-requests
                (gp-log (if (and (>= status 200) (< status 300)) 'http 'error)
                               "%s %s -> %d (%.0fms)" method
                               (bitbucket--log-path path params)
                               status (* 1000 (- (float-time) start))))
              (when (or (< status 200) (>= status 300))
                (gp-log-error "  body: %s" (string-trim body))
                (error "Bitbucket API %s %s -> HTTP %d: %s"
                       method url status
                       (or (let-alist parsed .error.message) body)))
              parsed)))
      (kill-buffer buf))))

(defun bitbucket--log-path (path params)
  "Return a compact PATH (+ q filter) for logging, hiding noisy fields."
  (let ((q (cdr (assoc "q" params))))
    (concat path (if q (format " [q=%s]" q) ""))))

(defun bitbucket--read-response (buf)
  "Parse HTTP response in BUF into (STATUS . PARSED-JSON); kill BUF.
The body is decoded as UTF-8.  Does not signal on HTTP errors --
the caller inspects STATUS."
  (unwind-protect
      (with-current-buffer buf
        (goto-char (point-min))
        (let ((status (if (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                          (string-to-number (match-string 1))
                        0)))
          (re-search-forward "\n\n" nil t)
          (let ((body (decode-coding-string
                       (buffer-substring-no-properties (point) (point-max))
                       'utf-8)))
            (cons status (bitbucket--parse-json body)))))
    (when (buffer-live-p buf) (kill-buffer buf))))

(defun bitbucket-api-get-async (path params callback)
  "GET PATH with PARAMS asynchronously; call CALLBACK with parsed JSON.
CALLBACK receives the parsed value, or nil on any error (logged).
Non-blocking: returns immediately.  Single page only (no
pagination) -- intended for fan-out scans."
  (let* ((url (bitbucket--build-url path params))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(bitbucket--auth-header))
            ("Accept" . "application/json")))
         (start (float-time)))
    (url-retrieve
     url
     (lambda (status-plist)
       (let (result)
         (condition-case e
             (if-let* ((err (plist-get status-plist :error)))
                 (gp-log-error "async %s -> %S" path err)
               (let* ((sc (bitbucket--read-response (current-buffer)))
                      (code (car sc)))
                 (when gp-log-requests
                   (gp-log (if (and (>= code 200) (< code 300)) 'http 'error)
                                  "GET %s -> %d (%.0fms, async)"
                                  (bitbucket--log-path path params)
                                  code (* 1000 (- (float-time) start))))
                 (when (and (>= code 200) (< code 300))
                   (setq result (cdr sc)))))
           (error (gp-log-error "async %s parse: %s"
                                       path (error-message-string e))))
         (funcall callback result)))
     nil t t)))

(defun bitbucket--parse-json (string)
  "Parse STRING as JSON into alists/lists, tolerating an empty body."
  (if (or (null string) (string-empty-p (string-trim string)))
      nil
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol)
          (json-false nil)
          (json-null nil))
      (json-read-from-string string))))

;;;; Pagination -------------------------------------------------------------

(defun bitbucket-api-paged (path &optional params max-items)
  "GET PATH following Bitbucket pagination, returning a list of values.
PARAMS is the initial query alist.  Stops after MAX-ITEMS values
when that argument is non-nil."
  (let ((acc '())
        (next path)
        (next-params params))
    (catch 'done
      (while next
        (let* ((page (bitbucket-api-request "GET" next next-params))
               (values (alist-get 'values page)))
          (setq next-params nil)        ;; the "next" link already carries params
          (dolist (v values)
            (push v acc)
            (when (and max-items (>= (length acc) max-items))
              (throw 'done nil)))
          (setq next (alist-get 'next page)))))
    (nreverse acc)))

;;;; TTL result cache ---------------------------------------------------------

(defcustom bitbucket-cache-ttl 300
  "Seconds to cache PR-list results (default 5 minutes).
Set to 0 to disable caching."
  :type 'integer
  :group 'bitbucket)

(defvar bitbucket--result-cache (make-hash-table :test 'equal)
  "KEY -> (EXPIRY . VALUE) cache for PR-list fetches.")

(defvar bitbucket--repo-list-cache nil)   ;; defined fully further down

(defun bitbucket-cache-clear ()
  "Clear cached PR-list results and the repo list (forces a fresh fetch)."
  (interactive)
  (clrhash bitbucket--result-cache)
  (setq bitbucket--repo-list-cache nil))

(defun bitbucket-cache-get (key)
  "Return (FOUND . VALUE) for KEY from the result cache.
FOUND is nil on a miss/expiry.  Honours `bitbucket-cache-ttl' = 0
\(always a miss)."
  (if (<= bitbucket-cache-ttl 0)
      (cons nil nil)
    (let ((entry (gethash key bitbucket--result-cache)))
      (if (and entry (< (float-time) (car entry)))
          (progn (gp-log 'cache "hit %S" key) (cons t (cdr entry)))
        (cons nil nil)))))

(defun bitbucket-cache-put (key value)
  "Cache VALUE under KEY for `bitbucket-cache-ttl' seconds (no-op if 0)."
  (when (> bitbucket-cache-ttl 0)
    (puthash key (cons (+ (float-time) bitbucket-cache-ttl) value)
             bitbucket--result-cache))
  value)

(defun bitbucket-with-cache (key thunk)
  "Return cached value for KEY, or call THUNK, caching for `bitbucket-cache-ttl'."
  (let ((hit (bitbucket-cache-get key)))
    (if (car hit)
        (cdr hit)
      (bitbucket-cache-put key (funcall thunk)))))

;;;; High-level endpoints ----------------------------------------------------

(defun bitbucket-current-user ()
  "Return the authenticated user object (alist)."
  (bitbucket-api-request "GET" "/user"))

(defun bitbucket-user-uuid ()
  "Return the authenticated user's UUID (a \"{...}\" string), cached."
  (or bitbucket--uuid-cache
      (setq bitbucket--uuid-cache
            (alist-get 'uuid (bitbucket-current-user)))))

(defun bitbucket-clear-cache ()
  "Forget cached identity so the next call re-resolves it."
  (setq bitbucket--uuid-cache nil))

(defun bitbucket-workspace-pull-requests (&optional uuid state max-items)
  "Return all pull requests in the workspace involving UUID.

This uses the workspace-level endpoint
/workspaces/{ws}/pullrequests/{uuid}, which returns, in a single
paginated query, every PR across the whole workspace that the
user authored or is a reviewer on -- so we never have to iterate
over individual repositories.

UUID defaults to the authenticated user.  STATE defaults to
\"OPEN\".  MAX-ITEMS caps the number of PRs fetched."
  (let* ((uuid (or uuid (bitbucket-user-uuid)))
         (path (format "/workspaces/%s/pullrequests/%s"
                       (bitbucket-workspace-value) uuid)))
    (bitbucket-api-paged
     path
     `(("state" . ,(or state "OPEN"))
       ("fields" . ,(concat
                     "values.id,values.title,values.state,values.draft,"
                     "values.author.uuid,values.author.display_name,"
                     "values.author.links.avatar.href,"
                     "values.source.branch.name,values.source.commit.hash,"
                     "values.destination.branch.name,"
                     "values.destination.repository.full_name,"
                     "values.destination.repository.slug,"
                     "values.comment_count,values.created_on,values.updated_on,"
                     "values.participants.role,values.participants.approved,"
                     "values.participants.state,"
                     "values.links.html.href,next")))
     max-items)))

(defun bitbucket-pr-draft-p (pr)
  "Return non-nil if PR is a draft pull request."
  (and (alist-get 'draft pr) t))

(defun bitbucket-pr-review-tally (pr)
  "Return a plist (:approved N :changes N :pending N) over PR's reviewers.
Only `REVIEWER' participants are counted: approved (state
\"approved\" or `approved' t), changes-requested (state
\"changes_requested\"), else pending."
  (let ((approved 0) (changes 0) (pending 0))
    (dolist (p (alist-get 'participants pr))
      (when (equal (alist-get 'role p) "REVIEWER")
        (let ((state (alist-get 'state p)))
          (cond
           ((or (equal state "approved") (eq (alist-get 'approved p) t))
            (setq approved (1+ approved)))
           ((equal state "changes_requested")
            (setq changes (1+ changes)))
           (t (setq pending (1+ pending)))))))
    (list :approved approved :changes changes :pending pending)))

(defun bitbucket-commit-build-states (full-name hash)
  "Return the list of build STATE strings for commit HASH in FULL-NAME.
Each is one of SUCCESSFUL/FAILED/INPROGRESS/STOPPED.  Empty when
no pipeline ran."
  (when hash
    (mapcar (lambda (s) (alist-get 'state s))
            (bitbucket-api-paged
             (format "/repositories/%s/commit/%s/statuses" full-name hash)
             '(("fields" . "values.state,next"))))))

(defcustom bitbucket-reviewing-repo-scan-limit 15
  "How many recently-updated repos to scan for PRs awaiting your review.

The workspace pull-request endpoint only returns PRs you authored,
not ones where you are merely a reviewer, and Bitbucket offers no
workspace-wide \"reviewing\" query.  So we scan the most recently
updated repositories (this many) for open PRs that list you as a
reviewer.  Raise it to cover more repos at the cost of more API
calls; lower it to go faster."
  :type 'integer
  :group 'bitbucket)

(defcustom bitbucket-reviewing-max-prs 30
  "Cap on the number of reviewer PRs collected, to keep the list snappy."
  :type 'integer
  :group 'bitbucket)

(defcustom bitbucket-repo-list-ttl 86400
  "Seconds to cache the workspace repo list (default 1 day).
The set of repositories changes rarely, so this avoids a slow
listing call on every reviewer scan."
  :type 'integer
  :group 'bitbucket)

(defun bitbucket--recent-repo-slugs (&optional limit)
  "Return up to LIMIT recently-updated repo \"ws/slug\" names in the workspace.
Cached for `bitbucket-repo-list-ttl' seconds."
  (let ((limit (or limit bitbucket-reviewing-repo-scan-limit)))
    (if (and bitbucket--repo-list-cache
             (< (float-time) (nth 0 bitbucket--repo-list-cache))
             (>= (nth 1 bitbucket--repo-list-cache) limit))
        (cl-subseq (cddr bitbucket--repo-list-cache) 0
                   (min limit (length (cddr bitbucket--repo-list-cache))))
      (let* ((ws (bitbucket-workspace-value))
             (slugs (mapcar (lambda (r) (alist-get 'full_name r))
                            (bitbucket-api-paged
                             (format "/repositories/%s" ws)
                             '(("sort" . "-updated_on")
                               ("fields" . "values.full_name,next"))
                             limit))))
        (setq bitbucket--repo-list-cache
              (cons (+ (float-time) bitbucket-repo-list-ttl) (cons limit slugs)))
        slugs))))

(defun bitbucket--state-clause (states)
  "Return a `q' sub-expression restricting to STATES, or \"\" for all.
STATES is a list like (\"OPEN\") or (\"OPEN\" \"MERGED\"); nil means
no restriction."
  (if (null states) ""
    (concat " AND ("
            (mapconcat (lambda (s) (format "state=\"%s\"" s)) states " OR ")
            ")")))

(defconst bitbucket--pr-list-fields
  (concat "values.id,values.title,values.state,values.draft,"
          "values.author.uuid,values.author.display_name,"
          "values.author.links.avatar.href,"
          "values.source.branch.name,values.source.commit.hash,"
          "values.destination.branch.name,"
          "values.destination.repository.full_name,"
          "values.destination.repository.slug,"
          "values.comment_count,values.created_on,values.updated_on,"
          "values.participants.role,values.participants.approved,"
          "values.participants.state,"
          "values.links.html.href,next")
  "Field selector for repo PR-list scans.")

(defun bitbucket--reviewer-q (uuid states)
  "Return the `q' filter string for reviewer UUID restricted to STATES."
  (format "reviewers.uuid=\"%s\"%s" uuid
          (bitbucket--state-clause (if (eq states 'all) nil
                                     (or states '("OPEN"))))))

(defun bitbucket-reviewing-pull-requests (&optional uuid limit states)
  "Return PRs in the workspace where UUID is a reviewer (synchronous).
Scans the LIMIT most recently-updated repositories (default
`bitbucket-reviewing-repo-scan-limit').  STATES restricts by PR
state (default (\"OPEN\")); pass nil/`all' for all.  Prefer
`bitbucket-reviewing-pull-requests-async' interactively."
  (let* ((uuid (or uuid (bitbucket-user-uuid)))
         (q (bitbucket--reviewer-q uuid states))
         (acc '())
         (repos (bitbucket--recent-repo-slugs limit)))
    (gp-log 'info "reviewing scan (sync): %d repos" (length repos))
    (catch 'enough
      (dolist (full-name repos)
        (setq acc
              (nconc acc
                     (bitbucket-api-paged
                      (format "/repositories/%s/pullrequests" full-name)
                      `(("q" . ,q) ("fields" . ,bitbucket--pr-list-fields)))))
        (when (>= (length acc) bitbucket-reviewing-max-prs)
          (throw 'enough nil))))
    (if (> (length acc) bitbucket-reviewing-max-prs)
        (cl-subseq acc 0 bitbucket-reviewing-max-prs)
      acc)))

(defun bitbucket-scan-repos-async (q repos on-batch on-done)
  "Fire a parallel PR-list query (filter Q) across REPOS, non-blocking.
For each repo, call ON-BATCH with the list of PRs found there as
its response arrives; call ON-DONE when every request has
returned.  All requests are in flight at once, so wall-clock is
one round-trip rather than the sum."
  (let ((remaining (length repos)))
    (if (zerop remaining)
        (funcall on-done)
      (dolist (full-name repos)
        (bitbucket-api-get-async
         (format "/repositories/%s/pullrequests" full-name)
         `(("q" . ,q) ("fields" . ,bitbucket--pr-list-fields))
         (lambda (page)
           (when page
             (funcall on-batch (alist-get 'values page)))
           (setq remaining (1- remaining))
           (when (zerop remaining) (funcall on-done))))))))

(defun bitbucket-reviewing-pull-requests-async (uuid states on-batch on-done &optional limit)
  "Scan repos in parallel for reviewer PRs of UUID (states STATES).
ON-BATCH is called with PR lists as they arrive; ON-DONE when the
scan finishes.  Non-blocking."
  (let ((q (bitbucket--reviewer-q uuid states))
        (repos (bitbucket--recent-repo-slugs limit)))
    (gp-log 'info "reviewing scan (async): %d repos" (length repos))
    (bitbucket-scan-repos-async q repos on-batch on-done)))

(defun bitbucket-open-pull-requests-async (states on-batch on-done &optional limit)
  "Scan repos in parallel for all PRs in STATES (default OPEN).
ON-BATCH receives PR lists as they arrive; ON-DONE at the end.
Non-blocking.  No reviewer/author filter -- the caller decides
what to keep (e.g. excluding their own)."
  (let* ((states (if (eq states 'all) '("OPEN") (or states '("OPEN"))))
         (q (mapconcat (lambda (s) (format "state=\"%s\"" s)) states " OR "))
         (repos (bitbucket--recent-repo-slugs limit)))
    (gp-log 'info "open-PR scan (async): %d repos" (length repos))
    (bitbucket-scan-repos-async q repos on-batch on-done)))

(defun bitbucket-pull-request (full-name id)
  "Return the full PR object for FULL-NAME (\"ws/slug\") and PR ID."
  (bitbucket-api-request
   "GET" (format "/repositories/%s/pullrequests/%s" full-name id)))

(defun bitbucket-set-pull-request-draft (full-name id draft &optional title)
  "Set the draft flag of PR ID in FULL-NAME to DRAFT (a boolean).
PUT replaces the PR, so TITLE is sent to preserve it (fetched when
not given).  Requires Pull-requests:Write.  Returns the updated PR."
  (let ((title (or title (alist-get 'title (bitbucket-pull-request full-name id)))))
    (bitbucket-api-request
     "PUT" (format "/repositories/%s/pullrequests/%s" full-name id)
     nil `((title . ,title) (draft . ,(if draft t :json-false))))))

(defun bitbucket-open-pr-for-branch (full-name branch)
  "Return the open PR in FULL-NAME whose source branch is BRANCH, or nil.
Uses the repository PR endpoint with a `q' filter on the source
branch, returning the first (most recent) match."
  (car
   (bitbucket-api-paged
    (format "/repositories/%s/pullrequests" full-name)
    `(("q" . ,(format "source.branch.name=\"%s\" AND state=\"OPEN\"" branch))
      ("sort" . "-updated_on")
      ("fields" . ,(concat
                    "values.id,values.title,values.state,"
                    "values.author.uuid,values.author.display_name,"
                    "values.source.branch.name,"
                    "values.destination.branch.name,"
                    "values.destination.repository.full_name,"
                    "values.destination.repository.slug,"
                    "values.comment_count,values.links.html.href,next")))
    1)))

(defun bitbucket-repo-open-pr-count (full-name)
  "Return the number of OPEN pull requests in repo FULL-NAME.
Cheap: requests a single-item page and reads the `size' total."
  (let ((page (bitbucket-api-request
               "GET" (format "/repositories/%s/pullrequests" full-name)
               '(("state" . "OPEN") ("pagelen" . "1") ("fields" . "size")))))
    (or (alist-get 'size page) 0)))

(defun bitbucket-repo-pull-requests (full-name &optional state)
  "Return the OPEN pull requests in repo FULL-NAME (one call).
STATE defaults to \"OPEN\"."
  (bitbucket-api-paged
   (format "/repositories/%s/pullrequests" full-name)
   `(("state" . ,(or state "OPEN"))
     ("sort" . "-updated_on")
     ("fields" . ,bitbucket--pr-list-fields))))

(defun bitbucket-pull-request-comments (full-name id &optional max-items)
  "Return comments for PR ID in FULL-NAME (\"ws/slug\").
Deleted comments are filtered out.  MAX-ITEMS caps the count."
  (cl-remove-if
   (lambda (c) (alist-get 'deleted c))
   (bitbucket-api-paged
    (format "/repositories/%s/pullrequests/%s/comments" full-name id)
    `(("fields" . ,(concat
                    "values.id,values.deleted,values.content.raw,"
                    "values.user.display_name,values.user.uuid,"
                    "values.user.links.avatar.href,values.created_on,"
                    "values.resolution.user.display_name,"
                    "values.inline.path,values.inline.from,values.inline.to,"
                    "values.links.html.href,"
                    "values.parent.id,next")))
    max-items)))

(defun bitbucket-comment-resolved-p (comment)
  "Return non-nil if COMMENT has been marked resolved on the PR."
  (and (alist-get 'resolution comment) t))

(defun bitbucket-create-comment (full-name id text &optional inline parent-id)
  "Create a comment on PR ID in FULL-NAME with raw TEXT.
INLINE, when non-nil, is a cons (PATH . LINE) anchoring an inline
comment on the new side of the diff.  PARENT-ID, when non-nil,
makes this a reply to that comment.  Returns the created comment.
Requires a token with Pull-requests:Write."
  (let ((data `((content . ((raw . ,text))))))
    (when inline
      (push `(inline . ((path . ,(car inline)) (to . ,(cdr inline)))) data))
    (when parent-id
      (push `(parent . ((id . ,parent-id))) data))
    (bitbucket-api-request
     "POST"
     (format "/repositories/%s/pullrequests/%s/comments" full-name id)
     nil data)))

(defun bitbucket-resolve-comment (full-name id comment-id)
  "Mark COMMENT-ID on PR ID in FULL-NAME as resolved.
Requires Pull-requests:Write.  Returns the resolution object."
  (bitbucket-api-request
   "POST"
   (format "/repositories/%s/pullrequests/%s/comments/%s/resolve"
           full-name id comment-id)))

(defun bitbucket-reopen-comment (full-name id comment-id)
  "Reopen (un-resolve) COMMENT-ID on PR ID in FULL-NAME.
Requires Pull-requests:Write."
  (bitbucket-api-request
   "DELETE"
   (format "/repositories/%s/pullrequests/%s/comments/%s/resolve"
           full-name id comment-id)))

(defun bitbucket-delete-comment (full-name id comment-id)
  "Delete COMMENT-ID on PR ID in FULL-NAME.  Requires Pull-requests:Write."
  (bitbucket-api-request
   "DELETE"
   (format "/repositories/%s/pullrequests/%s/comments/%s"
           full-name id comment-id)))

(defun bitbucket-edit-comment (full-name id comment-id text)
  "Replace COMMENT-ID's body with raw TEXT on PR ID in FULL-NAME.
Requires Pull-requests:Write.  Returns the updated comment."
  (bitbucket-api-request
   "PUT"
   (format "/repositories/%s/pullrequests/%s/comments/%s"
           full-name id comment-id)
   nil `((content . ((raw . ,text))))))

(defun bitbucket-comment-own-p (comment uuid)
  "Return non-nil if COMMENT was written by the user with UUID."
  (equal (let-alist comment .user.uuid) uuid))

(defun bitbucket-pull-request-stats (full-name id &optional pr)
  "Return a plist (:files N :added N :removed N :commits N) for a PR.
The diffstat lives at a per-PR signed URL exposed as the PR's
`links.diffstat.href' (the constructed path 404s), so fetch the
PR object (or accept it via PR) and follow that link.  Commits
come from the commits endpoint."
  (let* ((pr (or pr (bitbucket-pull-request full-name id)))
         (diffstat-url (let-alist pr .links.diffstat.href))
         (stat (when diffstat-url
                 (bitbucket-api-paged diffstat-url nil)))
         (commits (bitbucket-api-paged
                   (format "/repositories/%s/pullrequests/%s/commits" full-name id)
                   '(("fields" . "values.hash,next")))))
    (list :files (length stat)
          :added (apply #'+ (mapcar (lambda (s) (or (alist-get 'lines_added s) 0)) stat))
          :removed (apply #'+ (mapcar (lambda (s) (or (alist-get 'lines_removed s) 0)) stat))
          :commits (length commits)
          :file-list (mapcar #'bitbucket--diffstat-entry stat))))

(defun bitbucket--diffstat-entry (s)
  "Return a plist (:path :status :added :removed) for diffstat entry S.
Uses the new path, falling back to the old (for deletions)."
  (list :path (or (let-alist s .new.path) (let-alist s .old.path))
        :status (alist-get 'status s)
        :added (or (alist-get 'lines_added s) 0)
        :removed (or (alist-get 'lines_removed s) 0)))

(defun bitbucket-pull-request-diff (full-name id)
  "Return the unified diff text for PR ID in FULL-NAME."
  ;; The diff endpoint returns text/plain, not JSON; reuse the request
  ;; machinery but read the raw body.
  (let* ((url (bitbucket--build-url
               (format "/repositories/%s/pullrequests/%s/diff" full-name id)
               nil))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(bitbucket--auth-header))))
         (buf (url-retrieve-synchronously url t t bitbucket-request-timeout)))
    (unless buf (error "Bitbucket diff request timed out"))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "\n\n" nil t)
          (decode-coding-string
           (buffer-substring-no-properties (point) (point-max)) 'utf-8))
      (kill-buffer buf))))

;;;; Classification ----------------------------------------------------------

(defun bitbucket-pr-authored-by-p (pr uuid)
  "Return non-nil if PR was authored by the user with UUID."
  (equal (let-alist pr .author.uuid) uuid))

(defun bitbucket-partition-pull-requests (prs uuid)
  "Split PRS into a cons (MINE . REVIEWING) based on UUID authorship.
Drafts are included in whichever side they fall on; use
`bitbucket-categorize-pull-requests' to separate them."
  (let (mine reviewing)
    (dolist (pr prs)
      (if (bitbucket-pr-authored-by-p pr uuid)
          (push pr mine)
        (push pr reviewing)))
    (cons (nreverse mine) (nreverse reviewing))))

(defun bitbucket-categorize-pull-requests (prs uuid)
  "Categorise PRS for UUID into a plist (:reviewing :mine :drafts).
A draft authored by the user goes to :drafts; non-draft authored
PRs to :mine; everything else (PRs the user reviews) to
:reviewing.  Order within each group is preserved."
  (let (mine reviewing drafts)
    (dolist (pr prs)
      (cond
       ((and (bitbucket-pr-authored-by-p pr uuid)
             (bitbucket-pr-draft-p pr))
        (push pr drafts))
       ((bitbucket-pr-authored-by-p pr uuid)
        (push pr mine))
       (t (push pr reviewing))))
    (list :reviewing (nreverse reviewing)
          :mine (nreverse mine)
          :drafts (nreverse drafts))))

;;;; Pipelines -----------------------------------------------------------------

;; Bitbucket Pipelines API.  Platform constraints baked in here:
;;   * stop and trigger are PIPELINE-level only -- there is no per-step
;;     stop/trigger endpoint.  A waiting *manual* step is advanced by
;;     re-triggering its pipeline with a custom selector (see
;;     `bitbucket-pipeline-run-manual-step').
;;   * the step-log endpoint returns the captured log as text/plain; there is
;;     no streaming endpoint, so "live" logs are polled (see gp-pipeline-log).
;;   * stop/trigger go through `bitbucket-api-request', which SIGNALS on non-2xx
;;     (e.g. 403 when the token lacks Pipelines:Write) -- callers report it.

(defun bitbucket-pipeline-commit (pipeline)
  "Return PIPELINE's target commit hash, or nil."
  (let-alist pipeline .target.commit.hash))

(defun bitbucket-commit-message (full-name hash)
  "Return the commit message for HASH in FULL-NAME, or nil.
Cached, since commit messages are immutable.  The pipelines list
carries only the commit hash, so this is a separate lookup (needs
Repositories:Read)."
  (when (and full-name hash)
    (bitbucket-with-cache
     (list 'commit-msg full-name hash)
     (lambda ()
       (ignore-errors
         (alist-get 'message
                    (bitbucket-api-request
                     "GET"
                     (format "/repositories/%s/commit/%s" full-name hash)
                     '(("fields" . "message")))))))))

(defun bitbucket-commit-summary (message)
  "Return the first non-empty line of commit MESSAGE, trimmed, or \"\"."
  (if (not message)
      ""
    (string-trim (car (split-string message "\n" t)))))

(defun bitbucket-pipelines-match-commit (pipelines commit)
  "Return the PIPELINES whose target commit matches COMMIT.
Matches by hash prefix in either direction, since the API may
return short or full hashes.  With COMMIT nil, returns PIPELINES
unchanged."
  (if (not commit)
      pipelines
    (cl-remove-if-not
     (lambda (p)
       (let ((h (bitbucket-pipeline-commit p)))
         (and h (or (string-prefix-p h commit)
                    (string-prefix-p commit h)))))
     pipelines)))

(defun bitbucket-pipelines-for-branch (full-name branch &optional max-items commit)
  "Return pipelines in FULL-NAME for BRANCH, newest first.
When COMMIT is non-nil, only pipelines whose target commit matches
it are returned -- a PR's relevant pipelines are the ones for its
current head commit, not every run on the branch.  Caps the
*fetched* set at MAX-ITEMS (default 20) before the commit filter."
  (when (and full-name branch)
    (bitbucket-pipelines-match-commit
     (bitbucket-api-paged
      (format "/repositories/%s/pipelines" full-name)
      `(("sort" . "-created_on")
        ("target.ref_name" . ,branch))
      (or max-items 20))
     commit)))

(defun bitbucket-pipeline-steps (full-name pipeline-uuid)
  "Return the steps of PIPELINE-UUID in FULL-NAME, in execution order."
  (when (and full-name pipeline-uuid)
    (bitbucket-api-paged
     (format "/repositories/%s/pipelines/%s/steps" full-name pipeline-uuid))))

(defun bitbucket-pipeline-stop (full-name pipeline-uuid)
  "Signal a stop of PIPELINE-UUID in FULL-NAME (all incomplete steps).
Requires a token with Pipelines:Write.  Signals on HTTP error."
  (bitbucket-api-request
   "POST"
   (format "/repositories/%s/pipelines/%s/stopPipeline"
           full-name pipeline-uuid)))

(defun bitbucket-pipeline-trigger (full-name branch &optional selector variables)
  "Trigger a pipeline in FULL-NAME for BRANCH.
With no SELECTOR, runs the branch's default pipeline.  SELECTOR is a
cons (TYPE . PATTERN), e.g. (\"custom\" . \"my-pipeline\"), to run a
named pipeline.  VARIABLES is an alist of (KEY . VALUE) build
variables.  Returns the created pipeline object.  Requires a token
with Pipelines:Write."
  (let* ((target `((type . "pipeline_ref_target")
                   (ref_type . "branch")
                   (ref_name . ,branch)))
         (target (if selector
                     (append target
                             `((selector . ((type . ,(car selector))
                                            (pattern . ,(cdr selector))))))
                   target))
         (data `((target . ,target))))
    (when variables
      (push `(variables . ,(mapcar (lambda (kv)
                                     `((key . ,(car kv)) (value . ,(cdr kv))))
                                   variables))
            data))
    (bitbucket-api-request
     "POST" (format "/repositories/%s/pipelines" full-name) nil data)))

(defun bitbucket-pipeline-run-manual-step (full-name branch pipeline step)
  "Run a waiting manual STEP of PIPELINE in FULL-NAME on BRANCH.
Bitbucket has no \"run this step\" endpoint; a manual step is started
by re-triggering its pipeline with the custom selector naming the
pipeline pattern.  Errors unless STEP is a manual step."
  (unless (bitbucket-pipeline-step-manual-p step)
    (user-error "Step %S is not a manual step"
                (or (alist-get 'name step) "?")))
  (let* ((pattern (let-alist pipeline .target.selector.pattern))
         (selector (when pattern (cons "custom" pattern))))
    (bitbucket-pipeline-trigger full-name branch selector)))

(defun bitbucket-pipeline-step-log (full-name pipeline-uuid step-uuid)
  "Return the captured log text for STEP-UUID of PIPELINE-UUID in FULL-NAME.
Returns an empty string when the step has produced no log yet.  The
endpoint returns text/plain, so this reads the raw body (like the
diff endpoint) rather than parsing JSON."
  (when (and full-name pipeline-uuid step-uuid)
    (let* ((url (bitbucket--build-url
                 (format "/repositories/%s/pipelines/%s/steps/%s/log"
                         full-name pipeline-uuid step-uuid)
                 nil))
           (url-request-method "GET")
           (url-request-extra-headers
            `(("Authorization" . ,(bitbucket--auth-header))))
           (buf (url-retrieve-synchronously url t t bitbucket-request-timeout)))
      (if (not buf)
          ""
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-min))
              (if (save-excursion
                    (re-search-forward "^HTTP/[0-9.]+ 404"
                                       (line-end-position) t))
                  ""                    ;; no log produced yet
                (re-search-forward "\n\n" nil t)
                (decode-coding-string
                 (buffer-substring-no-properties (point) (point-max)) 'utf-8)))
          (kill-buffer buf))))))

;;;; Pipeline pure helpers (shape-aware, no network) ---------------------------

(defun bitbucket-pipeline-state (pipeline)
  "Return PIPELINE's coarse state string (PENDING/IN_PROGRESS/COMPLETED) or nil."
  (let-alist pipeline .state.name))

(defun bitbucket-pipeline-result (pipeline)
  "Return PIPELINE's result/stage string (SUCCESSFUL/FAILED/STOPPED/…) or nil.
A finished pipeline carries a `result'; a running one may carry a `stage'."
  (let-alist pipeline (or .state.result.name .state.stage.name)))

(defun bitbucket-pipeline-finished-p (pipeline)
  "Non-nil when PIPELINE has finished (state COMPLETED)."
  (equal (bitbucket-pipeline-state pipeline) "COMPLETED"))

(defun bitbucket-pipeline-number (pipeline)
  "Return PIPELINE's build number, or nil."
  (alist-get 'build_number pipeline))

(defun bitbucket-pipeline-step-state (step)
  "Return STEP's coarse state string (PENDING/IN_PROGRESS/COMPLETED/…)."
  (let-alist step .state.name))

(defun bitbucket-pipeline-step-result (step)
  "Return STEP's result string (SUCCESSFUL/FAILED/STOPPED/…) or nil."
  (let-alist step .state.result.name))

(defun bitbucket-pipeline-step-running-p (step)
  "Non-nil when STEP is currently running (state IN_PROGRESS)."
  (equal (bitbucket-pipeline-step-state step) "IN_PROGRESS"))

(defun bitbucket-pipeline-step-manual-p (step)
  "Non-nil when STEP is a manual step (gated, started on demand).
The real API marks these with a `trigger' type of
\"pipeline_step_trigger_manual\"; older/other shapes use plain
\"manual\".  Match any trigger type CONTAINING \"manual\"."
  (let* ((trigger (alist-get 'trigger step))
         (type (and trigger (alist-get 'type trigger))))
    (and type (string-match-p "manual" (downcase (format "%s" type))))))

(defun bitbucket-pipeline-step-pending-p (step)
  "Non-nil when STEP is waiting to run (state PENDING / paused / ready).
A manual step you can start now is both manual and pending."
  (member (bitbucket-pipeline-step-state step)
          '("PENDING" "READY" "NOT_RUN" "PAUSED")))

(defun bitbucket-pipeline-step-runnable-manual-p (step)
  "Non-nil when STEP is a manual step that is waiting and can be started now."
  (and (bitbucket-pipeline-step-manual-p step)
       (bitbucket-pipeline-step-pending-p step)))

(defun bitbucket-pipelines-sort (pipelines step-counts)
  "Return PIPELINES sorted by step count descending, then newest first.
STEP-COUNTS maps a pipeline's uuid to its number of steps (a hash
table or alist keyed by the `uuid' string).  Pipelines with an
unknown count sort as zero."
  (let ((count-of
         (lambda (p)
           (let ((uuid (alist-get 'uuid p)))
             (or (if (hash-table-p step-counts)
                     (gethash uuid step-counts)
                   (cdr (assoc uuid step-counts)))
                 0)))))
    (sort (copy-sequence pipelines)
          (lambda (a b)
            (let ((ca (funcall count-of a))
                  (cb (funcall count-of b)))
              (if (= ca cb)
                  ;; tie: newer created_on first (ISO-8601 strings compare)
                  (string> (or (alist-get 'created_on a) "")
                           (or (alist-get 'created_on b) ""))
                (> ca cb)))))))

(provide 'bitbucket-api)
;;; bitbucket-api.el ends here
