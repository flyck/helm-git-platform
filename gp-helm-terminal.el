;;; gp-helm-terminal.el --- Send PR comments to AI terminal sessions -*- lexical-binding: t; -*-

;;; Commentary:

;; Hands a PR comment off to a local AI terminal session: reuses a session
;; in the repo running Claude/OpenCode, else opens a new window in that
;; checkout and sends the prompt there.  Backends: iTerm2 (tty/job-name
;; session discovery) and Ghostty (native AppleScript `terminal' ids).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'git-platform)
(require 'gp-local)
(require 'gp-log)

(declare-function gp-pr-action-context "gp-pr-actions")

(cl-defstruct gp-helm-terminal-session
  id path job-name name promptp)

(defcustom gp-helm-terminal-backend 'iterm2
  "Backend used for terminal handoff."
  :type '(choice (const :tag "iTerm2" iterm2)
                 (const :tag "Ghostty" ghostty))
  :group 'bitbucket)

(defcustom gp-helm-terminal-ai-job-regexp "\\`\\(?:claude\\|opencode\\)\\'"
  "Regexp matching AI terminal jobs we should reuse.
Matched against the terminal session's job name as reported by the backend."
  :type '(choice (const :tag "Any job" nil) regexp)
  :group 'bitbucket)

(defcustom gp-helm-terminal-launch-command "claude"
  "Command to launch in a fresh terminal session before sending the prompt.
Set this to `claude' if that is your preferred interactive tool, or nil to
open a plain shell and only paste the prompt text."
  :type '(choice (const :tag "None" nil) string)
  :group 'bitbucket)

(defcustom gp-helm-terminal-launch-delay 1.5
  "Seconds to wait after launching a fresh AI command before sending text."
  :type 'number
  :group 'bitbucket)

(defcustom gp-helm-terminal-submit-immediately t
  "When non-nil, send the prompt with a trailing newline."
  :type 'boolean
  :group 'bitbucket)

(defcustom gp-helm-terminal-prompt-prefix "please implement this comment:"
  "Prefix inserted ahead of comment text when sending to the terminal."
  :type 'string
  :group 'bitbucket)

(defun gp-helm-terminal--normalize-path (path)
  "Return PATH expanded and directory-normalized, or nil."
  (when (and path (not (string-empty-p path)))
    (directory-file-name (expand-file-name path))))

(defun gp-helm-terminal--comment-location (comment)
  "Return a human label for COMMENT's inline location, or nil."
  (let-alist comment
    (when .inline.path
      (format "%s:%s" .inline.path (or .inline.to .inline.from "?")))))

(defun gp-helm-terminal--launch-command ()
  "Return the effective launch command for a fresh terminal session.
When the configured command starts Claude and does not already pick a
permission mode, append `--permission-mode auto'."
  (let ((command gp-helm-terminal-launch-command))
    (if (and command
             (string-match-p "\\`claude\\(?:\\s-\\|\\'\\)" command)
             (not (string-match-p "--permission-mode\\(?:=\\|\\s-\\)" command)))
        (concat command " --permission-mode auto")
      command)))

(defun gp-helm-terminal--build-prompt (pr comment)
  "Build the terminal prompt text for COMMENT on PR."
  (let* ((repo (gp-pr-full-name pr))
         (pr-id (alist-get 'id pr))
         (title (or (alist-get 'title pr) ""))
         (location (gp-helm-terminal--comment-location comment))
         (body (string-trim (or (let-alist comment .content.raw) ""))))
    (string-join
     (delq nil
           (list gp-helm-terminal-prompt-prefix
                 ""
                 (format "Repo: %s" repo)
                 (format "PR: #%s %s" pr-id title)
                 (when location (format "Comment: %s" location))
                 ""
                 body))
     "\n")))

(defun gp-helm-terminal--build-multi-prompt (pr comments)
  "Build one terminal prompt covering COMMENTS on PR."
  (if (= (length comments) 1)
      (gp-helm-terminal--build-prompt pr (car comments))
    (let* ((repo (gp-pr-full-name pr))
           (pr-id (alist-get 'id pr))
           (title (or (alist-get 'title pr) ""))
           (blocks
            (cl-loop for comment in comments
                     for idx from 1
                     for location = (or (gp-helm-terminal--comment-location comment) "general")
                     for body = (string-trim (or (let-alist comment .content.raw) ""))
                     collect (format "%d. Comment: %s\n%s" idx location body))))
      (string-join
       (append (list "please implement these comments:"
                     ""
                     (format "Repo: %s" repo)
                     (format "PR: #%s %s" pr-id title)
                     "")
               blocks)
       "\n\n"))))

(defun gp-helm-terminal--path-match-p (session-path repo-dir)
  "Return non-nil when SESSION-PATH belongs to REPO-DIR."
  (let ((session-path (gp-helm-terminal--normalize-path session-path))
        (repo-dir (gp-helm-terminal--normalize-path repo-dir)))
    (and session-path repo-dir
         (or (equal session-path repo-dir)
             (file-in-directory-p session-path repo-dir)))))

(defun gp-helm-terminal--ai-session-p (session)
  "Return non-nil when SESSION looks like an AI terminal session.
If `gp-helm-terminal-ai-job-regexp' is nil, any session qualifies."
  (let ((regexp gp-helm-terminal-ai-job-regexp)
        (job-name (or (gp-helm-terminal-session-job-name session)
                      (gp-helm-terminal-session-name session))))
    (or (null regexp)
        (and job-name (string-match-p regexp job-name)))))

(defun gp-helm-terminal--session-score (session repo-dir)
  "Return a score for SESSION relative to REPO-DIR.
Higher scores are preferred. Exact repo-root matches outrank subdirectories."
  (let ((session-path (gp-helm-terminal--normalize-path
                       (gp-helm-terminal-session-path session)))
        (repo-dir (gp-helm-terminal--normalize-path repo-dir)))
    (+ (if (equal session-path repo-dir) 100 50)
       (if (gp-helm-terminal--ai-session-p session) 20 0)
       (if (gp-helm-terminal-session-promptp session) 0 1))))

(defun gp-helm-terminal--choose-session (sessions repo-dir)
  "Pick the best SESSION from SESSIONS for REPO-DIR, or nil.
Only repo-local sessions are considered. If an AI job regexp is configured,
only matching sessions are reused; otherwise a fresh session is opened."
  (let* ((repo-matches (cl-remove-if-not
                        (lambda (session)
                          (gp-helm-terminal--path-match-p
                           (gp-helm-terminal-session-path session) repo-dir))
                        sessions))
         (eligible (if gp-helm-terminal-ai-job-regexp
                       (cl-remove-if-not #'gp-helm-terminal--ai-session-p repo-matches)
                     repo-matches)))
    (car (sort eligible
               (lambda (a b)
                 (> (gp-helm-terminal--session-score a repo-dir)
                    (gp-helm-terminal--session-score b repo-dir)))))))

(defun gp-helm-terminal--plan (sessions repo-dir)
  "Return a dispatch plan plist for SESSIONS and REPO-DIR."
  (if-let* ((session (gp-helm-terminal--choose-session sessions repo-dir)))
      (list :action 'reuse :session session)
    (list :action 'open :directory repo-dir
          :command (gp-helm-terminal--launch-command)
          :delay gp-helm-terminal-launch-delay)))

(defun gp-helm-terminal--run-applescript (label lines &rest args)
  "Run AppleScript LINES with `osascript', passing ARGS via argv.
ARGS are never interpolated into the script text, so arbitrary text is
safe.  LABEL names the backend in logs and errors.  Return stdout
trimmed, or signal `user-error' with stderr details."
  (gp-log 'info "%s AppleScript: %d lines, %d args" label (length lines) (length args))
  (with-temp-buffer
    (let ((status (apply #'call-process
                         "osascript" nil (current-buffer) nil
                         "-l" "AppleScript"
                         (append (cl-mapcan (lambda (line) (list "-e" line)) lines)
                                 (cons "--" args)))))
      (let ((output (string-trim (buffer-string))))
        (if (and (integerp status) (zerop status))
            (progn
              (unless (string-empty-p output)
                (gp-log 'info "%s AppleScript output: %s" label output))
              output)
          (gp-log-error "%s AppleScript failed (status=%s): %s"
                        label status (if (string-empty-p output) "unknown error" output))
          (user-error "%s AppleScript failed: %s"
                      label (if (string-empty-p output) "unknown error" output)))))))

(defun gp-helm-terminal--backend-fn (op)
  "Load the configured backend and return its function for OP.
OP is a string such as \"send-text\"; the result is the symbol
`gp-helm-terminal-BACKEND-OP'."
  (pcase gp-helm-terminal-backend
    ((and (or 'iterm2 'ghostty) backend)
     (let ((feature (intern (format "gp-helm-terminal-%s" backend))))
       (require feature)
       (intern (format "%s-%s" feature op))))
    (_ (user-error "Unsupported terminal backend: %S" gp-helm-terminal-backend))))

(defun gp-helm-terminal--list-sessions ()
  "Return backend session descriptors."
  (funcall (gp-helm-terminal--backend-fn "list-sessions")))

(defun gp-helm-terminal--safe-list-sessions ()
  "Return backend session descriptors, or nil when discovery fails.
Discovery failures are logged and treated as a signal to open a fresh
terminal session instead of aborting the handoff."
  (condition-case err
      (gp-helm-terminal--list-sessions)
    (error
     (gp-log 'warn "terminal session discovery failed, opening new session: %s"
             (error-message-string err))
     nil)))

(defun gp-helm-terminal--execute (plan text)
  "Execute PLAN by sending TEXT through the configured backend."
  (gp-log 'info "terminal dispatch: backend=%s action=%s"
          gp-helm-terminal-backend (plist-get plan :action))
  (let ((send (gp-helm-terminal--backend-fn "send-text"))
        (open (gp-helm-terminal--backend-fn "open-session")))
    (pcase (plist-get plan :action)
      ('reuse
       (funcall send (gp-helm-terminal-session-id (plist-get plan :session))
                text gp-helm-terminal-submit-immediately))
      ('open
       (funcall open (plist-get plan :directory) text
                (plist-get plan :command) (plist-get plan :delay)
                gp-helm-terminal-submit-immediately))
      (_ (user-error "Unknown terminal action: %S" (plist-get plan :action))))))

(defcustom gp-helm-terminal-conflict-prompt
  "please resolve the merge conflicts with %s by rebasing this branch onto it, keeping both sides' intent where they overlap"
  "Prompt sent to the terminal session to resolve a PR's merge conflicts.
A single %s, if present, is replaced by the destination branch name.
Sent with the same repo/PR context every terminal handoff carries."
  :type 'string :group 'bitbucket)

(defun gp-helm-terminal--dispatch (pr text label)
  "Send TEXT to PR's terminal session, logging the handoff as LABEL."
  (let* ((dir (gp-local-ensure-checkout pr))
         (plan (gp-helm-terminal--plan (gp-helm-terminal--safe-list-sessions) dir)))
    (gp-log 'info "%s: repo=%s pr=%s dir=%s action=%s"
            label (gp-pr-full-name pr) (alist-get 'id pr) dir (plist-get plan :action))
    (gp-helm-terminal--execute plan text)
    (message "%s terminal session for %s"
             (if (eq (plist-get plan :action) 'reuse) "Updated" "Opened")
             (gp-pr-full-name pr))))

(defun gp-helm-terminal-send-text (pr text)
  "Send TEXT to PR's terminal session, opening or reusing it as needed.
The entry point for anything that is not a comment."
  (condition-case err
      (gp-helm-terminal--dispatch pr text "terminal handoff (text)")
    (error
     (gp-log-error "terminal handoff failed: %s" (error-message-string err))
     (user-error "Terminal handoff failed: %s" (error-message-string err)))))

(defun gp-helm-terminal-send-conflict (pr)
  "Ask PR's terminal session to resolve its merge conflicts.
Uses `gp-helm-terminal-conflict-prompt', followed by the repo and PR."
  (let* ((dest (or (ignore-errors (gp-pr-destination-branch pr)) "the target branch"))
         (ask (if (string-match-p "%s" gp-helm-terminal-conflict-prompt)
                  (format gp-helm-terminal-conflict-prompt dest)
                gp-helm-terminal-conflict-prompt)))
    (gp-helm-terminal-send-text
     pr
     (string-join
      (list ask
            ""
            (format "Repo: %s" (gp-pr-full-name pr))
            (format "PR: #%s %s" (alist-get 'id pr) (or (alist-get 'title pr) ""))
            (format "Target branch: %s" dest))
      "\n"))))

(defun gp-helm-terminal-send-comment (pr comment)
  "Send COMMENT from PR to the configured terminal backend."
  (gp-helm-terminal-send-comments pr (list comment)))

(defun gp-helm-terminal-send-comments (pr comments)
  "Send COMMENTS from PR to the configured terminal backend."
  (condition-case err
      (gp-helm-terminal--dispatch
       pr (gp-helm-terminal--build-multi-prompt pr comments) "terminal handoff")
    (error
     (gp-log-error "terminal handoff failed: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

(defun gp-helm-terminal-shell-action (ctx)
  "Open a new terminal window with a plain shell in CTX's checkout.
CTX is a PR action context (see `gp-pr-actions'): :dir is the checkout,
already on the PR's source branch unless :checkout-error says why not.
Never reuses an existing session and sends no command or prompt."
  (let ((dir (or (plist-get ctx :dir)
                 (user-error "Open in terminal needs a checkout (:checkout t)")))
        (failure (plist-get ctx :checkout-error)))
    (gp-log 'info "terminal shell: repo=%s pr=%s dir=%s checkout-failed=%s"
            (plist-get ctx :repo) (plist-get ctx :id) dir (and failure t))
    (gp-helm-terminal--execute
     (list :action 'open :directory dir :command "" :delay 0) "")
    (if failure
        (message "Opened shell in %s, but checkout failed: %s" dir failure)
      (message "Opened shell in %s on %s" dir (plist-get ctx :source-branch)))))

(defun gp-helm-terminal-open-shell (pr)
  "Check out PR's source branch and open a plain shell there.
If the checkout fails, the shell opens in the local clone as it is and
the failure is reported (see `gp-pr-action-context')."
  (require 'gp-pr-actions)
  (gp-helm-terminal-shell-action (gp-pr-action-context pr t)))

(provide 'gp-helm-terminal)
;;; gp-helm-terminal.el ends here
