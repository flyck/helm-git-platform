;;; gp-stealth.el --- Open a PR in a new Stealth session -*- lexical-binding: t; -*-

;;; Commentary:

;; A PR action (see `gp-pr-actions') that starts a session in the Stealth
;; agent app, in the PR's checkout, with the PR's context drafted in the
;; composer and left unsent.
;;
;; Stealth's contract: write {"workspace": DIR, "prompt": TEXT} to a
;; file ending in ".stealth-session" and hand it over with
;; `open -a Stealth FILE'.  Stealth reads the file once and deletes it.
;; This works whether Stealth is running or not.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'gp-log)

(defcustom gp-stealth-application "Stealth"
  "Name or path of the Stealth application, as `open -a' takes it."
  :type 'string :group 'bitbucket)

(defcustom gp-stealth-prompt-function #'gp-stealth-default-prompt
  "Function building the drafted message from a PR action context.
Called with the plist `gp-pr-action-context' returns; returns a string."
  :type 'function :group 'bitbucket)

(defun gp-stealth-default-prompt (ctx)
  "Return the drafted message for CTX: where the PR is, ready to add to."
  (string-join
   (delq nil
         (list (format "Repo: %s" (plist-get ctx :repo))
               (format "PR: #%s %s" (plist-get ctx :id) (or (plist-get ctx :title) ""))
               (when-let* ((url (plist-get ctx :url))) (format "URL: %s" url))
               (format "Branch: %s → %s"
                       (or (plist-get ctx :source-branch) "?")
                       (or (plist-get ctx :destination-branch) "?"))
               (format "Folder: %s" (plist-get ctx :dir))
               (when-let* ((err (plist-get ctx :checkout-error)))
                 (format "Note: switching to the PR branch failed (%s)" err))
               ""
               ""))
   "\n"))

(defun gp-stealth--request-json (dir prompt)
  "Return the request file text for DIR and PROMPT."
  (json-encode `((workspace . ,(directory-file-name (expand-file-name dir)))
                 (prompt . ,prompt))))

(defun gp-stealth--hand-over (file)
  "Hand request FILE to Stealth; return nil on success or the error text."
  (with-temp-buffer
    (let ((status (call-process "open" nil t nil "-a" gp-stealth-application file)))
      (unless (eql status 0)
        (string-trim (buffer-string))))))

(defun gp-stealth-open-session (ctx)
  "Start a Stealth session in CTX's checkout, its PR context drafted.
CTX is a PR action context with :dir (see `gp-pr-actions')."
  (let ((dir (or (plist-get ctx :dir)
                 (user-error "Open in Stealth needs a checkout (:checkout t)")))
        (file (make-temp-file "gp-pr-" nil ".stealth-session")))
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file file
        (insert (gp-stealth--request-json dir (funcall gp-stealth-prompt-function ctx)))))
    (gp-log 'info "Stealth: session request for %s in %s" (plist-get ctx :repo) dir)
    (if-let* ((failure (gp-stealth--hand-over file)))
        (progn
          (ignore-errors (delete-file file))
          (gp-log-error "Stealth hand-over failed: %s" failure)
          (user-error "Could not open Stealth: %s" failure))
      (message "Opening a Stealth session in %s%s" dir
               (if-let* ((err (plist-get ctx :checkout-error)))
                   (format " (checkout failed: %s)" err)
                 (format " on %s" (plist-get ctx :source-branch)))))))

(provide 'gp-stealth)
;;; gp-stealth.el ends here
