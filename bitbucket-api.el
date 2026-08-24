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
(require 'git-platform)

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

(defcustom bitbucket-web-base "https://bitbucket.org"
  "Base URL of the Bitbucket web UI (for browser deep links).
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

(defvar bitbucket--mention-cache (make-hash-table :test 'equal)
  "ACCOUNT-ID -> display name, for resolving @{account_id} comment mentions.
Unlike `gp--result-cache' (git-platform.el) this never expires on its own -- a
mentioned user's display name is effectively permanent for our purposes,
so it is only forgotten via `bitbucket-clear-cache'.")

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

(defun bitbucket--read-response-text (buf)
  "Read HTTP response in BUF into (STATUS . BODY-TEXT); kill BUF.
The body is decoded as UTF-8 and returned unparsed -- the diff
endpoint serves text/plain.  Does not signal on HTTP errors; the
caller inspects STATUS.  `bitbucket--read-response' is the
JSON-parsing twin and shares this header/body split."
  (unwind-protect
      (with-current-buffer buf
        (goto-char (point-min))
        (let ((status (if (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                          (string-to-number (match-string 1))
                        0)))
          (re-search-forward "\n\n" nil t)
          (cons status
                (decode-coding-string
                 (buffer-substring-no-properties (point) (point-max))
                 'utf-8))))
    (when (buffer-live-p buf) (kill-buffer buf))))

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

(defun bitbucket-api-get-text-async (url callback)
  "GET URL asynchronously and call CALLBACK with the raw body text.
CALLBACK receives the decoded body, or nil on any error (logged).
The JSON-parsing twin is `bitbucket-api-get-async'; the diff endpoint
returns text/plain, so it needs a reader that does not parse."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(bitbucket--auth-header))))
         (start (float-time)))
    (url-retrieve
     url
     (lambda (status-plist)
       (let (result)
         (condition-case e
             (if-let* ((err (plist-get status-plist :error)))
                 (gp-log-error "async diff -> %S" err)
               (let* ((sc (bitbucket--read-response-text (current-buffer)))
                      (code (car sc)))
                 (when gp-log-requests
                   (gp-log (if (and (>= code 200) (< code 300)) 'http 'error)
                           "GET diff -> %d (%.0fms, async)"
                           code (* 1000 (- (float-time) start))))
                 (when (and (>= code 200) (< code 300))
                   (setq result (cdr sc)))))
           (error (gp-log-error "async diff read: %s" (error-message-string e))))
         (funcall callback result)))
     nil t t)))

(defun bitbucket-api-paged-async (path &optional params callback max-items)
  "GET PATH following pagination asynchronously; call CALLBACK with (OK VALUES).
On success, OK is non-nil and VALUES is the collected list. On any error, OK is
nil and VALUES is nil."
  (let ((acc '())
        (count 0))
    (cl-labels
        ((finish (ok values)
           (funcall callback ok values))
         (step (next next-params)
           (bitbucket-api-get-async
            next next-params
            (lambda (page)
              (if (null page)
                  (finish nil nil)
                (let ((done nil))
                  (dolist (v (alist-get 'values page))
                    (push v acc)
                    (setq count (1+ count))
                    (when (and max-items (>= count max-items))
                      (setq done t)))
                  (let ((next-url (and (not done) (alist-get 'next page))))
                    (if next-url
                        (step next-url nil)
                      (finish t (nreverse acc))))))))))
      (step path params))))

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

;; The cache itself is provider-agnostic and now lives in git-platform.el
;; as `gp-cache-*'/`gp-cache-ttl', shared with the GitHub backend and the
;; UI layers.  These names are kept as aliases so existing callers (and
;; the dynamic `(let ((bitbucket-cache-ttl 0)) ...)' rebindings sprinkled
;; through gp-ui.el/git-platform-mock.el/the test suite) keep working
;; unchanged.
(defvaralias 'bitbucket-cache-ttl 'gp-cache-ttl)
(defalias 'bitbucket-cache-get #'gp-cache-get)
(defalias 'bitbucket-cache-put #'gp-cache-put)
(defalias 'bitbucket-with-cache #'gp-cache-with-cache)

(defvar bitbucket--repo-list-cache nil)   ;; defined fully further down; Bitbucket-only

(defun bitbucket-cache-clear ()
  "Clear cached PR-list results and Bitbucket's repo list (forces a fresh fetch)."
  (interactive)
  (gp-cache-clear)
  (setq bitbucket--repo-list-cache nil))

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
  (setq bitbucket--uuid-cache nil)
  (clrhash bitbucket--mention-cache))

(defun bitbucket-user (account-id)
  "Return the user object (alist) for ACCOUNT-ID, or nil if unresolvable.
Failures (e.g. a deactivated or unknown account) are swallowed -- a
mention that cannot be resolved should fall back to showing the raw
token, not break comment rendering."
  (ignore-errors (bitbucket-api-request "GET" (format "/users/%s" account-id))))

(defun bitbucket-mention-display-name (account-id)
  "Return the display name for ACCOUNT-ID, cached, or nil if unresolvable."
  (let ((cached (gethash account-id bitbucket--mention-cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (let ((name (alist-get 'display_name (bitbucket-user account-id))))
        (puthash account-id name bitbucket--mention-cache)
        name))))

(defun bitbucket-resolve-mentions (text)
  "Replace @{account_id} mention tokens in TEXT with \"@Display Name\".
Tokens that cannot be resolved (deactivated/unknown accounts, or a
network error) are left as-is."
  (if (null text)
      ""
    (replace-regexp-in-string
     "@{\\([^{}]+\\)}"
     (lambda (m)
       ;; `bitbucket-mention-display-name' runs regex ops of its own (in the
       ;; cache lookup, the API layer, or a caller's mock); without
       ;; `save-match-data' those clobber the match data that
       ;; `replace-regexp-in-string' itself still needs after this function
       ;; returns, corrupting the rest of the substitution.
       (save-match-data
         (let ((name (bitbucket-mention-display-name (match-string 1 m))))
           (if name (concat "@" name) m))))
     text t t)))

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
                     "values.participants.state,values.participants.user.uuid,"
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

(defun bitbucket-pr-reviewers (pr)
  "Return PR's `REVIEWER' participants as plists (:id :name :avatar :state).
STATE is `approved', `changes', or `pending', matching
`bitbucket-pr-review-tally''s classification of the same data.
:ID is the account uuid -- the same identifier
`bitbucket-set-pull-request-reviewers' takes, so a reviewer shown in
the UI can be mapped back to an API identity."
  (let (out)
    (dolist (p (alist-get 'participants pr))
      (when (equal (alist-get 'role p) "REVIEWER")
        (let-alist p
          (push (list :id .user.uuid
                      :name (or .user.display_name "?")
                      :avatar .user.links.avatar.href
                      :state (cond
                              ((or (equal .state "approved") (eq .approved t)) 'approved)
                              ((equal .state "changes_requested") 'changes)
                              (t 'pending)))
                out))))
    (nreverse out)))

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
          "values.participants.state,values.participants.user.uuid,"
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
  "Return the full PR object for FULL-NAME (\"ws/slug\") and PR ID.
The PR object (title, branches, author, state) changes rarely, so a
non-nil result is cached under `bitbucket-cache-ttl'; a `g' refresh
bypasses the cache by binding that TTL to 0.  A nil/error result is
not cached, so a later open re-fetches."
  (let ((key (list 'pull-request full-name id)))
    (let ((hit (bitbucket-cache-get key)))
      (if (car hit)
          (cdr hit)
        (let ((pr (bitbucket-api-request
                   "GET" (format "/repositories/%s/pullrequests/%s" full-name id))))
          (when pr (bitbucket-cache-put key pr))
          pr)))))

(defun bitbucket-pull-request-async (full-name id callback)
  "Fetch PR ID in FULL-NAME asynchronously, calling CALLBACK with (OK PR)."
  (bitbucket-api-get-async
   (format "/repositories/%s/pullrequests/%s" full-name id)
   nil
   (lambda (pr)
     (funcall callback (and pr t) pr))))

(defun bitbucket-set-pull-request-draft (full-name id draft &optional title)
  "Set the draft flag of PR ID in FULL-NAME to DRAFT (a boolean).
PUT replaces the PR, so TITLE is sent to preserve it (fetched when
not given).  Requires Pull-requests:Write.  Returns the updated PR."
  (let ((title (or title (alist-get 'title (bitbucket-pull-request full-name id)))))
    (bitbucket-api-request
     "PUT" (format "/repositories/%s/pullrequests/%s" full-name id)
     nil `((title . ,title) (draft . ,(if draft t :json-false))))))

(defconst bitbucket-merge-strategies
  '("merge_commit" "squash" "fast_forward"
    "squash_fast_forward" "rebase_fast_forward" "rebase_merge")
  "Every merge strategy Bitbucket Cloud's merge endpoint accepts.
A repository permits a subset of these per destination branch; ask the
PR rather than assuming (see `bitbucket-pull-request-merge-strategies').")

(defun bitbucket-pull-request-merge-strategies (full-name id)
  "Return (STRATEGIES . DEFAULT) permitted for PR ID in FULL-NAME.
Both live on the destination branch, and neither is returned unless
asked for explicitly -- the default PR payload carries only the branch
`name', so this re-fetches with a `fields' selector.  Returns nil when
the fields are unavailable, so callers fall back rather than guess."
  (let* ((r (ignore-errors
              (bitbucket-api-request
               "GET" (format "/repositories/%s/pullrequests/%s" full-name id)
               '(("fields" . "+destination.branch.merge_strategies,+destination.branch.default_merge_strategy")))))
         (strategies (let-alist r .destination.branch.merge_strategies))
         (default (let-alist r .destination.branch.default_merge_strategy)))
    (when strategies
      (cons (append strategies nil) default))))

(defun bitbucket-merge-pull-request (full-name id &optional strategy message close-source-branch)
  "Merge PR ID in FULL-NAME, returning the updated pull request.
STRATEGY is one of `bitbucket-merge-strategies' (nil lets Bitbucket
apply the destination branch's own default).  MESSAGE overrides the
merge commit message.  CLOSE-SOURCE-BRANCH asks Bitbucket to delete the
source branch afterwards -- it does that itself, so callers must not
also delete the remote branch.  Requires Pull-requests:Write."
  (when (and strategy (not (member strategy bitbucket-merge-strategies)))
    (error "Not a Bitbucket merge strategy: %s" strategy))
  (bitbucket-api-request
   "POST" (format "/repositories/%s/pullrequests/%s/merge" full-name id)
   nil
   (append '((type . "pullrequest"))
           (when strategy `((merge_strategy . ,strategy)))
           (when (and message (not (string-empty-p message))) `((message . ,message)))
           (when close-source-branch '((close_source_branch . t))))))

(defun bitbucket-set-pull-request-title (full-name id title)
  "Set the title of PR ID in FULL-NAME to TITLE.
The endpoint is a whole-object PUT, so the DESCRIPTION has to be resent
alongside -- the mirror image of `bitbucket-set-pull-request-description'
resending the title.  Omitting it would silently blank the description.
Bitbucket only allows mutating OPEN pull requests.  Requires
Pull-requests:Write.  Returns the updated PR."
  (when (or (null title) (string-empty-p (string-trim title)))
    (error "A pull request title cannot be empty"))
  (let ((description (or (alist-get 'description (bitbucket-pull-request full-name id)) "")))
    (bitbucket-api-request
     "PUT" (format "/repositories/%s/pullrequests/%s" full-name id)
     nil `((title . ,title) (description . ,description)))))

(defun bitbucket-set-pull-request-description (full-name id description &optional title)
  "Set the description of PR ID in FULL-NAME to DESCRIPTION.
PUT replaces the PR, so TITLE is sent to preserve it (fetched when
not given), for the same reason `bitbucket-set-pull-request-draft'
does.  Bitbucket only allows mutating OPEN pull requests.  Requires
Pull-requests:Write.  Returns the updated PR."
  (let ((title (or title (alist-get 'title (bitbucket-pull-request full-name id)))))
    (bitbucket-api-request
     "PUT" (format "/repositories/%s/pullrequests/%s" full-name id)
     nil `((title . ,title) (description . ,(or description ""))))))

(defun bitbucket-set-pull-request-reviewers (full-name id reviewer-uuids)
  "Set PR ID in FULL-NAME's reviewers to exactly REVIEWER-UUIDS.
The endpoint is a whole-object PUT: the `reviewers' array it receives
replaces the existing one, so REVIEWER-UUIDS must be the complete
desired list and not just the additions.  `title' is sent alongside
for the same reason `bitbucket-set-pull-request-draft' does -- a PUT
that omits it would blank the title.

Bitbucket only allows mutating OPEN pull requests.  Requires
Pull-requests:Write.  Returns the updated PR.

The array is built as a vector, not a list: `json-encode' renders the
empty list as `null', which Bitbucket rejects, whereas the empty
vector gives the `[]' needed to clear every reviewer."
  (let ((title (alist-get 'title (bitbucket-pull-request full-name id))))
    (bitbucket-api-request
     "PUT" (format "/repositories/%s/pullrequests/%s" full-name id)
     nil `((title . ,title)
           (reviewers . ,(vconcat (mapcar (lambda (u) (list (cons 'uuid u)))
                                          reviewer-uuids)))))))

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

(defun bitbucket-repo-default-branch (full-name)
  "Return repo FULL-NAME's default (main) branch name, or nil.
Reads the repository's `mainbranch' field (needs Repositories:Read).
Cached, since it changes rarely."
  (bitbucket-with-cache
   (list 'default-branch full-name)
   (lambda ()
     (ignore-errors
       (let-alist (bitbucket-api-request
                   "GET" (format "/repositories/%s" full-name)
                   '(("fields" . "mainbranch.name")))
         .mainbranch.name)))))

(cl-defun bitbucket-create-pull-request
    (full-name source dest title
     &key description draft close-source-branch reviewer-uuids)
  "Open a pull request in FULL-NAME from branch SOURCE into DEST.
TITLE and optional DESCRIPTION (Markdown) seed it.  Keyword args:
DRAFT opens it as a draft; CLOSE-SOURCE-BRANCH marks the source
branch to be deleted on merge; REVIEWER-UUIDS is a list of account
uuids to add as reviewers.  Returns the created PR object.
Requires a token with Pull-requests:Write; SOURCE must already
exist on the remote (push it first)."
  (let ((data (list (cons 'title title)
                    (cons 'source (list (cons 'branch (list (cons 'name source)))))
                    (cons 'destination (list (cons 'branch (list (cons 'name dest))))))))
    (when (and description (not (string-empty-p description)))
      (setq data (append data (list (cons 'description description)))))
    (when draft
      (setq data (append data (list (cons 'draft t)))))
    (when close-source-branch
      (setq data (append data (list (cons 'close_source_branch t)))))
    (when reviewer-uuids
      (setq data (append data
                         (list (cons 'reviewers
                                     (mapcar (lambda (u) (list (cons 'uuid u)))
                                             reviewer-uuids))))))
    (bitbucket-api-request
     "POST" (format "/repositories/%s/pullrequests" full-name)
     nil data)))

(defun bitbucket-repo-default-reviewers (full-name)
  "Return repo FULL-NAME's default reviewers as a list of user alists.
Each element has at least `uuid' and `display_name'.  Empty list
when none are configured (needs Repositories:Read).

A non-empty result is cached; an empty result (whether genuinely
empty or from a transient error) is NOT cached, so a later open
re-fetches rather than sticking on a stale empty list."
  (let ((key (list 'default-reviewers full-name)))
    (let ((hit (bitbucket-cache-get key)))
      (if (car hit)
          (cdr hit)
        (let ((reviewers
               (ignore-errors
                 (bitbucket-api-paged
                  (format "/repositories/%s/default-reviewers" full-name)
                  '(("fields" . "values.uuid,values.display_name,values.nickname,next"))))))
          (when reviewers
            (bitbucket-cache-put key reviewers))
          reviewers)))))

(defun bitbucket-repo-suggested-reviewers (full-name)
  "Return workspace members of FULL-NAME's workspace as reviewer candidates.
Bitbucket has no per-PR \"suggested reviewers\" resource, and
`bitbucket-repo-default-reviewers' only covers reviewers the repo
admin pre-configured -- so picking any other colleague was
impossible from the create form.  The workspace member list is the
closest available candidate pool.

Yourself and anyone already in the default-reviewer list are
filtered out, so the create form never shows a name twice.  Needs
Account:Read; failures degrade to an empty list rather than
breaking the create form.  Cached like the default reviewers, and
likewise only when non-empty."
  (let* ((workspace (car (split-string full-name "/")))
         (key (list 'workspace-members workspace))
         (hit (bitbucket-cache-get key))
         (members
          (if (car hit)
              (cdr hit)
            (let ((fetched
                   (ignore-errors
                     (bitbucket-api-paged
                      (format "/workspaces/%s/members" workspace)
                      '(("fields" . "values.user.uuid,values.user.display_name,values.user.nickname,next"))))))
              (when fetched (bitbucket-cache-put key fetched))
              fetched)))
         ;; the members endpoint nests the account under `user'
         (users (delq nil (mapcar (lambda (m) (alist-get 'user m)) members)))
         (exclude (cons (ignore-errors (bitbucket-user-uuid))
                        (mapcar (lambda (r) (alist-get 'uuid r))
                                (bitbucket-repo-default-reviewers full-name)))))
    (cl-remove-if (lambda (u) (member (alist-get 'uuid u) exclude)) users)))

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

(defun bitbucket-pull-request-comments-async (full-name id callback &optional max-items)
  "Fetch comments for PR ID in FULL-NAME asynchronously.
CALLBACK receives (OK COMMENTS). Deleted comments are filtered out."
  (bitbucket-api-paged-async
   (format "/repositories/%s/pullrequests/%s/comments" full-name id)
   `(("fields" . ,(concat
                   "values.id,values.deleted,values.content.raw,"
                   "values.user.display_name,values.user.uuid,"
                   "values.user.links.avatar.href,values.created_on,"
                   "values.resolution.user.display_name,"
                   "values.inline.path,values.inline.from,values.inline.to,"
                   "values.links.html.href,"
                   "values.parent.id,next")))
   (lambda (ok comments)
     (funcall callback ok
              (if ok
                  (cl-remove-if (lambda (c) (alist-get 'deleted c)) comments)
                nil)))
   max-items))

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

(defun bitbucket-approve-pr (full-name id &optional unapprove)
  "Approve PR ID in FULL-NAME (\"ws/slug\").
With UNAPPROVE non-nil, retract a previous approval instead.
Requires Pull-requests:Write."
  (bitbucket-api-request
   (if unapprove "DELETE" "POST")
   (format "/repositories/%s/pullrequests/%s/approve" full-name id)))

(defun bitbucket-request-changes-pr (full-name id &optional unrequest)
  "Request changes on PR ID in FULL-NAME (\"ws/slug\").
With UNREQUEST non-nil, retract a previous changes-request instead.
Requires Pull-requests:Write."
  (bitbucket-api-request
   (if unrequest "DELETE" "POST")
   (format "/repositories/%s/pullrequests/%s/request-changes" full-name id)))

(defun bitbucket-pr-my-review-state (pr uuid)
  "Return UUID's own review state on PR: `approved', `changes', or nil.
Reads the PR's participants; nil means UUID has neither approved
nor requested changes (or is not a participant)."
  (let (state)
    (dolist (p (alist-get 'participants pr))
      (when (equal (let-alist p .user.uuid) uuid)
        (let ((s (alist-get 'state p)))
          (cond
           ((or (equal s "approved") (eq (alist-get 'approved p) t))
            (setq state 'approved))
           ((equal s "changes_requested")
            (setq state 'changes))))))
    state))

(defun bitbucket-pull-request-stats (full-name id &optional pr)
  "Return a plist (:files N :added N :removed N :commits N) for a PR.
The diffstat lives at a per-PR signed URL exposed as the PR's
`links.diffstat.href' (the constructed path 404s), so fetch the
PR object (or accept it via PR) and follow that link.  Commits
come from the commits endpoint.

Stats only change when the PR's source commit does, so a non-nil
result is cached keyed by that commit hash (a `g' refresh binds
`bitbucket-cache-ttl' to 0 to force a fresh fetch).  When the
commit hash is unavailable the result is not cached."
  (let* ((pr (or pr (bitbucket-pull-request full-name id)))
         (commit (let-alist pr .source.commit.hash))
         (key (and commit (list 'pr-stats full-name id commit)))
         (hit (and key (bitbucket-cache-get key))))
    (if (and hit (car hit))
        (cdr hit)
      (let ((stats (bitbucket--pull-request-stats-1 full-name id pr)))
        (when (and key stats) (bitbucket-cache-put key stats))
        stats))))

(defun bitbucket--pull-request-stats-1 (full-name id pr)
  "Compute the diffstat plist for PR (uncached).
See `bitbucket-pull-request-stats'."
  (let* ((diffstat-url (let-alist pr .links.diffstat.href))
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

(defun bitbucket-pull-request-stats-async (full-name id pr callback)
  "Fetch the stats plist for PR FULL-NAME/ID asynchronously.
CALLBACK gets the plist, or nil on error.  Caching matches the
synchronous `bitbucket-pull-request-stats': keyed by source commit,
served from cache when warm, and only non-nil results are stored.

Async because this is deferred detail-view data: the synchronous twin
blocks Emacs for the whole round-trip (two paged fetches), which is
felt as a freeze after every `g'."
  (let* ((commit (let-alist pr .source.commit.hash))
         (key (and commit (list 'pr-stats full-name id commit)))
         (hit (and key (bitbucket-cache-get key))))
    (if (and hit (car hit))
        (funcall callback (cdr hit))
      (let ((diffstat-url (let-alist pr .links.diffstat.href)))
        (if (not diffstat-url)
            (funcall callback nil)
          ;; two independent paged fetches; join them when both land
          (let ((stat nil) (commits nil) (pending 2) (failed nil))
            (cl-labels
                ((done ()
                   (setq pending (1- pending))
                   (when (zerop pending)
                     (let ((stats
                            (unless failed
                              (list :files (length stat)
                                    :added (apply #'+ (mapcar (lambda (s) (or (alist-get 'lines_added s) 0)) stat))
                                    :removed (apply #'+ (mapcar (lambda (s) (or (alist-get 'lines_removed s) 0)) stat))
                                    :commits (length commits)
                                    :file-list (mapcar #'bitbucket--diffstat-entry stat)))))
                       (when (and key stats) (bitbucket-cache-put key stats))
                       (funcall callback stats)))))
              (bitbucket-api-paged-async
               diffstat-url nil
               (lambda (ok values)
                 (unless ok (setq failed t))
                 (setq stat values)
                 (done)))
              (bitbucket-api-paged-async
               (format "/repositories/%s/pullrequests/%s/commits" full-name id)
               '(("fields" . "values.hash,next"))
               (lambda (ok values)
                 (unless ok (setq failed t))
                 (setq commits values)
                 (done))))))))))

(defun bitbucket-pull-request-diff-async (full-name id commit pr callback)
  "Fetch the unified diff text for PR FULL-NAME/ID asynchronously.
CALLBACK gets the diff string, or nil on error.  Caching matches the
synchronous `bitbucket-pull-request-diff': keyed by source COMMIT, and
an empty diff is never cached so a transient empty response re-fetches.

PR supplies `links.diff.href' -- the constructed /diff path is rejected,
so the pre-signed link is the only one that works (see
`bitbucket--pull-request-diff-1')."
  (let* ((key (and commit (list 'pr-diff full-name id commit)))
         (hit (and key (bitbucket-cache-get key))))
    (if (and hit (car hit))
        (funcall callback (cdr hit))
      (let ((url (or (let-alist pr .links.diff.href)
                     (bitbucket--build-url
                      (format "/repositories/%s/pullrequests/%s/diff" full-name id)
                      nil))))
        (bitbucket-api-get-text-async
         url
         (lambda (diff)
           (when (and key diff (not (string-empty-p diff)))
             (bitbucket-cache-put key diff))
           (funcall callback diff)))))))

(defun bitbucket--commit-entry (c)
  "Normalise a Bitbucket PR commit C to (:hash :summary :author :date).
The display name lives under `author.user', but that is absent when the
commit's email matches no Bitbucket account (outside contributors, or a
mis-set local git config) -- fall back to the raw \"Name <email>\" header
and keep just the name part, so those commits still show an author."
  (let* ((author (alist-get 'author c))
         (name (or (let-alist author .user.display_name)
                   (let ((raw (alist-get 'raw author)))
                     (when raw
                       (string-trim (car (split-string raw "<" t))))))))
    (list :hash (alist-get 'hash c)
          :summary (bitbucket-commit-summary (alist-get 'message c))
          :author name
          :date (alist-get 'date c))))

(defun bitbucket-pull-request-commits-async (full-name id callback &optional max-items)
  "Fetch PR FULL-NAME/ID's commits; CALLBACK gets normalised plists (or nil).
Newest first, as the API returns them.  See `bitbucket--commit-entry'."
  (if (not (and full-name id))
      (funcall callback nil)
    (bitbucket-api-paged-async
     (format "/repositories/%s/pullrequests/%s/commits" full-name id)
     '(("fields" . "values.hash,values.message,values.date,values.author,next"))
     (lambda (ok values)
       (funcall callback (and ok (mapcar #'bitbucket--commit-entry values))))
     max-items)))

(defun bitbucket--diffstat-entry (s)
  "Return a plist (:path :status :added :removed) for diffstat entry S.
Uses the new path, falling back to the old (for deletions)."
  (list :path (or (let-alist s .new.path) (let-alist s .old.path))
        :status (alist-get 'status s)
        :added (or (alist-get 'lines_added s) 0)
        :removed (or (alist-get 'lines_removed s) 0)))

(defun bitbucket-pull-request-diff (full-name id &optional commit)
  "Return the unified diff text for PR ID in FULL-NAME.
A PR's diff only changes when its source commit does, so when the
source COMMIT hash is supplied a non-empty result is cached under
that key; a `g' refresh binds `bitbucket-cache-ttl' to 0 to force
fresh.  Without COMMIT the result is not cached (we can't key it
safely).  An empty diff is not cached, so a transient empty
response is re-fetched."
  (let* ((key (and commit (list 'pr-diff full-name id commit)))
         (hit (and key (bitbucket-cache-get key))))
    (if (and hit (car hit))
        (cdr hit)
      (let ((diff (bitbucket--pull-request-diff-1 full-name id)))
        (when (and key diff (not (string-empty-p diff)))
          (bitbucket-cache-put key diff))
        diff))))

(defun bitbucket--pull-request-diff-1 (full-name id)
  "Fetch the unified diff text for PR ID in FULL-NAME (uncached)."
  ;; The diff endpoint returns text/plain, not JSON; reuse the request
  ;; machinery but read the raw body.  The constructed
  ;; /pullrequests/ID/diff path is rejected (\"you may not have access\")
  ;; -- like diffstat, the working URL is the PR's pre-signed
  ;; `links.diff.href', so prefer that and fall back to the path.
  (let* ((url (or (ignore-errors
                    (let-alist (bitbucket-pull-request full-name id)
                      .links.diff.href))
                  (bitbucket--build-url
                   (format "/repositories/%s/pullrequests/%s/diff" full-name id)
                   nil)))
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
;;     stop/trigger endpoint (POST .../steps/STEP-UUID/run returns 404).
;;     A waiting *manual* step is advanced by re-triggering its pipeline
;;     with a custom selector (see `bitbucket-pipeline-run-manual-step'),
;;     which starts a NEW pipeline run.
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

(defun bitbucket-commit-message-async (full-name hash callback)
  "Fetch the commit message for HASH in FULL-NAME; CALLBACK gets it (or nil).
Shares `bitbucket-commit-message's cache, so a warm entry answers
without any network round-trip -- CALLBACK is then invoked directly,
NOT deferred to a timer (callers must tolerate a synchronous call)."
  (if (not (and full-name hash))
      (funcall callback nil)
    (let* ((key (list 'commit-msg full-name hash))
           (hit (gp-cache-get key)))
      (if (car hit)
          (funcall callback (cdr hit))
        (bitbucket-api-get-async
         (format "/repositories/%s/commit/%s" full-name hash)
         '(("fields" . "message"))
         (lambda (parsed)
           (let ((msg (and parsed (alist-get 'message parsed))))
             ;; only cache a real answer: caching nil would pin a transient
             ;; failure for the whole TTL
             (when msg (gp-cache-put key msg))
             (funcall callback msg))))))))

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

(defun bitbucket-pipelines-for-branch-async (full-name branch max-items commit callback)
  "Fetch pipelines in FULL-NAME for BRANCH; CALLBACK gets the list (or nil).
Non-blocking twin of `bitbucket-pipelines-for-branch', with the same
COMMIT filter and MAX-ITEMS cap applied to the fetched set."
  (if (not (and full-name branch))
      (funcall callback nil)
    (bitbucket-api-paged-async
     (format "/repositories/%s/pipelines" full-name)
     `(("sort" . "-created_on")
       ("target.ref_name" . ,branch))
     (lambda (ok values)
       (funcall callback
                (and ok (bitbucket-pipelines-match-commit values commit))))
     (or max-items 20))))

(defun bitbucket-pipeline-steps (full-name pipeline-uuid)
  "Return the steps of PIPELINE-UUID in FULL-NAME, in execution order."
  (when (and full-name pipeline-uuid)
    (bitbucket-api-paged
     (format "/repositories/%s/pipelines/%s/steps" full-name pipeline-uuid))))

(defun bitbucket-pipeline-steps-async (full-name pipeline-uuid callback)
  "Fetch the steps of PIPELINE-UUID in FULL-NAME; CALLBACK gets them (or nil).
Non-blocking twin of `bitbucket-pipeline-steps'."
  (if (not (and full-name pipeline-uuid))
      (funcall callback nil)
    (bitbucket-api-paged-async
     (format "/repositories/%s/pipelines/%s/steps" full-name pipeline-uuid)
     nil
     (lambda (ok values) (funcall callback (and ok values))))))

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
Bitbucket Cloud has no public per-step run endpoint -- the web UI's
play button calls an internal API, and POSTing to
.../steps/STEP-UUID/run returns 404.  The only documented way to
advance a manual step is to re-trigger its pipeline with the custom
selector naming the pipeline pattern, which starts a NEW pipeline
run.  Errors unless STEP is a manual step.  Requires Pipelines:Write."
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

(defun bitbucket-pipeline-web-url (full-name pipeline &optional step)
  "Return the web-UI URL for PIPELINE in FULL-NAME, deep-linked to STEP.
The web UI is the only place a paused manual step can be run in
place -- the public API has no per-step trigger (BCLOUD-20050)."
  (concat bitbucket-web-base
          (format "/%s/pipelines/results/%s"
                  full-name (bitbucket-pipeline-number pipeline))
          (when-let* ((uuid (and step (alist-get 'uuid step))))
            (format "/steps/%s" uuid))))

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
