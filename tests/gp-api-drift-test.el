;;; gp-api-drift-test.el --- Detect Bitbucket API spec drift -*- lexical-binding: t; -*-

;;; Commentary:

;; A scoped drift check: it fetches Bitbucket's official OpenAPI/Swagger spec
;; (https://api.bitbucket.org/swagger.json) and asserts that every endpoint
;; this package actually calls -- path + HTTP method -- still exists.  Unlike
;; a full oasdiff of the whole spec, this only looks at the ~12 endpoints we
;; depend on, so it has no noise from the other ~185 paths.
;;
;; It is NOT part of the offline unit suite: it needs the network, so it
;; skips when the spec can't be fetched.  Run it deliberately (or on a
;; schedule) to learn when Bitbucket changes something under us.  The spec is
;; cached in /tmp for a day to avoid refetching ~1MB on every run.
;;
;;   emacs --batch -Q -L . -l ... -l tests/gp-api-drift-test.el \
;;     --eval '(ert-run-tests-batch-and-exit "drift")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'url)

(defconst gp-api-drift-spec-url "https://api.bitbucket.org/swagger.json"
  "URL of Bitbucket Cloud's official OpenAPI (Swagger 2.0) spec.")

(defconst gp-api-drift-cache-file
  (expand-file-name "gp-bitbucket-swagger.json" temporary-file-directory)
  "Where the fetched spec is cached.")

(defconst gp-api-drift-cache-ttl 86400
  "Seconds the cached spec is considered fresh (1 day).")

(defconst gp-api-drift-endpoints
  ;; (METHOD . TEMPLATED-PATH) -- the endpoints this package calls.  Paths use
  ;; the spec's own parameter names ({workspace}, {repo_slug}, ...).
  '(("get"    . "/user")
    ("get"    . "/workspaces/{workspace}/pullrequests/{selected_user}")
    ("get"    . "/repositories/{workspace}")
    ("get"    . "/repositories/{workspace}/{repo_slug}/default-reviewers")
    ("get"    . "/repositories/{workspace}/{repo_slug}/pullrequests")
    ("post"   . "/repositories/{workspace}/{repo_slug}/pullrequests")
    ("get"    . "/repositories/{workspace}/{repo_slug}/pullrequests/{pull_request_id}")
    ("put"    . "/repositories/{workspace}/{repo_slug}/pullrequests/{pull_request_id}")
    ("get"    . "/repositories/{workspace}/{repo_slug}/pullrequests/{pull_request_id}/comments")
    ("post"   . "/repositories/{workspace}/{repo_slug}/pullrequests/{pull_request_id}/comments")
    ("put"    . "/repositories/{workspace}/{repo_slug}/pullrequests/{pull_request_id}/comments/{comment_id}")
    ("delete" . "/repositories/{workspace}/{repo_slug}/pullrequests/{pull_request_id}/comments/{comment_id}")
    ("post"   . "/repositories/{workspace}/{repo_slug}/pullrequests/{pull_request_id}/comments/{comment_id}/resolve")
    ("delete" . "/repositories/{workspace}/{repo_slug}/pullrequests/{pull_request_id}/comments/{comment_id}/resolve")
    ("get"    . "/repositories/{workspace}/{repo_slug}/pullrequests/{pull_request_id}/diff")
    ("get"    . "/repositories/{workspace}/{repo_slug}/pullrequests/{pull_request_id}/diffstat")
    ("get"    . "/repositories/{workspace}/{repo_slug}/pullrequests/{pull_request_id}/commits")
    ("get"    . "/repositories/{workspace}/{repo_slug}/commit/{commit}/statuses"))
  "Endpoints (METHOD . PATH) the package relies on; checked against the spec.")

(defun gp-api-drift--fetch-spec ()
  "Return the parsed Bitbucket spec, or nil if unreachable.
Uses a /tmp cache fresh for `gp-api-drift-cache-ttl' seconds."
  (let ((cache gp-api-drift-cache-file))
    (unless (and (file-exists-p cache)
                 (< (float-time (time-subtract (current-time)
                                               (nth 5 (file-attributes cache))))
                    gp-api-drift-cache-ttl))
      (ignore-errors
        (let ((buf (url-retrieve-synchronously gp-api-drift-spec-url t t 30)))
          (when buf
            (unwind-protect
                (with-current-buffer buf
                  (set-buffer-multibyte nil)
                  (goto-char (point-min))
                  (when (re-search-forward "\n\n" nil t)
                    (let ((body (buffer-substring-no-properties (point) (point-max)))
                          (coding-system-for-write 'binary))
                      (with-temp-file cache
                        (set-buffer-multibyte nil)
                        (insert body)))))
              (kill-buffer buf))))))
    (when (file-exists-p cache)
      (ignore-errors
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8))
            (insert-file-contents cache))
          (let ((json-object-type 'hash-table) (json-array-type 'list))
            (json-read-from-string (buffer-string))))))))

(ert-deftest gp-api-drift-endpoints-present ()
  "Every endpoint the package calls still exists in Bitbucket's spec.
Skipped when the spec cannot be fetched (offline)."
  (let ((spec (gp-api-drift--fetch-spec)))
    (skip-unless spec)
    (let* ((paths (gethash "paths" spec))
           (missing '()))
      (pcase-dolist (`(,method . ,path) gp-api-drift-endpoints)
        (let ((entry (and paths (gethash path paths))))
          (cond
           ((null entry) (push (format "%s %s (path gone)" (upcase method) path) missing))
           ((not (gethash method entry))
            (push (format "%s %s (method gone)" (upcase method) path) missing)))))
      (when missing
        (ert-fail (format "Bitbucket API drift -- endpoints changed:\n  %s"
                          (string-join (nreverse missing) "\n  "))))
      (should (null missing)))))

(provide 'gp-api-drift-test)
;;; gp-api-drift-test.el ends here
