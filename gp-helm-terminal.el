;;; gp-helm-terminal.el --- Send PR comments to AI terminal sessions -*- lexical-binding: t; -*-

;;; Commentary:

;; A small service layer for handing a PR comment off to a local AI terminal
;; session. It reuses a matching session in the repo when it finds one running
;; Claude/OpenCode, otherwise it opens a new window in that checkout and sends
;; the prompt there. Two backends are supported: iTerm2 (tty/job-name based
;; session discovery) and Ghostty (native AppleScript `terminal' ids).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'git-platform)
(require 'gp-local)
(require 'gp-log)

(declare-function gp-helm-terminal-iterm2-list-sessions "gp-helm-terminal-iterm2")
(declare-function gp-helm-terminal-iterm2-open-session "gp-helm-terminal-iterm2")
(declare-function gp-helm-terminal-iterm2-send-text "gp-helm-terminal-iterm2")
(declare-function gp-helm-terminal-ghostty-list-sessions "gp-helm-terminal-ghostty")
(declare-function gp-helm-terminal-ghostty-open-session "gp-helm-terminal-ghostty")
(declare-function gp-helm-terminal-ghostty-send-text "gp-helm-terminal-ghostty")

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

(defun gp-helm-terminal--list-sessions ()
  "Return backend session descriptors."
  (pcase gp-helm-terminal-backend
    ('iterm2
     (require 'gp-helm-terminal-iterm2)
     (gp-helm-terminal-iterm2-list-sessions))
    ('ghostty
     (require 'gp-helm-terminal-ghostty)
     (gp-helm-terminal-ghostty-list-sessions))
    (_ (user-error "Unsupported terminal backend: %S" gp-helm-terminal-backend))))

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
  (pcase gp-helm-terminal-backend
    ('iterm2
     (require 'gp-helm-terminal-iterm2)
     (pcase (plist-get plan :action)
       ('reuse
        (gp-helm-terminal-iterm2-send-text
         (gp-helm-terminal-session-id (plist-get plan :session))
         text gp-helm-terminal-submit-immediately))
       ('open
        (gp-helm-terminal-iterm2-open-session
         (plist-get plan :directory)
         text
         (plist-get plan :command)
         (plist-get plan :delay)
         gp-helm-terminal-submit-immediately))
       (_ (user-error "Unknown terminal action: %S" (plist-get plan :action)))))
    ('ghostty
     (require 'gp-helm-terminal-ghostty)
     (pcase (plist-get plan :action)
       ('reuse
        (gp-helm-terminal-ghostty-send-text
         (gp-helm-terminal-session-id (plist-get plan :session))
         text gp-helm-terminal-submit-immediately))
       ('open
        (gp-helm-terminal-ghostty-open-session
         (plist-get plan :directory)
         text
         (plist-get plan :command)
         (plist-get plan :delay)
         gp-helm-terminal-submit-immediately))
       (_ (user-error "Unknown terminal action: %S" (plist-get plan :action)))))
    (_ (user-error "Unsupported terminal backend: %S" gp-helm-terminal-backend))))

(defcustom gp-helm-terminal-conflict-prompt
  "please resolve the merge conflicts with %s by rebasing this branch onto it, keeping both sides' intent where they overlap"
  "Prompt sent to the terminal session to resolve a PR's merge conflicts.
A single %s, if present, is replaced by the destination branch name.
Sent with the same repo/PR context every terminal handoff carries."
  :type 'string :group 'bitbucket)

(defun gp-helm-terminal-send-text (pr text)
  "Send TEXT to PR's terminal session, opening or reusing it as needed.
The comment handoffs build their own prompt and then come through here;
this is the entry point for anything that is not a comment."
  (condition-case err
      (let* ((dir (gp-local-ensure-checkout pr))
             (plan (gp-helm-terminal--plan (gp-helm-terminal--safe-list-sessions) dir)))
        (gp-log 'info "terminal handoff (text): repo=%s pr=%s dir=%s action=%s"
                (gp-pr-full-name pr) (alist-get 'id pr) dir (plist-get plan :action))
        (gp-helm-terminal--execute plan text)
        (message "%s terminal session for %s"
                 (if (eq (plist-get plan :action) 'reuse) "Updated" "Opened")
                 (gp-pr-full-name pr)))
    (error
     (gp-log-error "terminal handoff failed: %s" (error-message-string err))
     (user-error "Terminal handoff failed: %s" (error-message-string err)))))

(defun gp-helm-terminal-send-conflict (pr)
  "Ask PR's terminal session to resolve its merge conflicts.
Uses `gp-helm-terminal-conflict-prompt', with the repo and PR named the
same way the comment handoffs do so the session has its bearings."
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
      (let* ((dir (gp-local-ensure-checkout pr))
             (text (gp-helm-terminal--build-multi-prompt pr comments))
             (plan (gp-helm-terminal--plan (gp-helm-terminal--safe-list-sessions) dir)))
        (gp-log 'info "terminal handoff: repo=%s pr=%s dir=%s action=%s"
                (gp-pr-full-name pr) (alist-get 'id pr) dir (plist-get plan :action))
        (gp-helm-terminal--execute plan text)
        (message "%s terminal session for %s"
                 (if (eq (plist-get plan :action) 'reuse) "Updated" "Opened")
                 (gp-pr-full-name pr)))
    (error
     (gp-log-error "terminal handoff failed: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

(provide 'gp-helm-terminal)
;;; gp-helm-terminal.el ends here
