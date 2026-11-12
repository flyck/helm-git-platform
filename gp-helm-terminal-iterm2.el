;;; gp-helm-terminal-iterm2.el --- iTerm2 backend for gp-helm-terminal -*- lexical-binding: t; -*-

;;; Commentary:

;; iTerm2 backend for `gp-helm-terminal'.  Sessions are discovered by our
;; `user.gpRepo' session variable, else by the processes on their tty; text
;; is written into a matching session or a new tab in the repo checkout.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gp-helm-terminal)
(require 'gp-log)

(defcustom gp-helm-terminal-iterm2-application "iTerm2"
  "Application name used for AppleScript calls."
  :type 'string
  :group 'bitbucket)

(defconst gp-helm-terminal-iterm2--field-separator (string 31)
  "Field separator used between AppleScript session columns.")

(defun gp-helm-terminal-iterm2--pid-cwd (pid)
  "Return the current working directory of PID, or nil."
  (let ((out (shell-command-to-string
              (format "lsof -p %s -a -d cwd -Fn 2>/dev/null"
                      (shell-quote-argument pid)))))
    (when (string-match "^n\\(.+\\)$" out)
      (string-trim (match-string 1 out)))))

(defun gp-helm-terminal-iterm2--tty-procs (tty)
  "Return a list of (PID . COMMAND) for the processes attached to TTY.
TTY is a device path such as \"/dev/ttys006\".  Ordered as `ps' returns
them (leader first, deepest child last)."
  (when (and tty (not (string-empty-p tty)))
    (let* ((name (replace-regexp-in-string "\\`/dev/" "" tty))
           (out (shell-command-to-string
                 (format "ps -t %s -o pid=,comm= 2>/dev/null"
                         (shell-quote-argument name)))))
      (cl-loop for line in (split-string out "\n" t)
               when (string-match "\\`[ \t]*\\([0-9]+\\)[ \t]+\\(.+\\)\\'" line)
               collect (cons (match-string 1 line)
                             (string-trim (match-string 2 line)))))))

(defun gp-helm-terminal-iterm2--procs-cwd (procs)
  "Return the deepest non-empty cwd among PROCS, or nil.
PROCS is a list of (PID . COMMAND); the login/shell leader has no cwd of
its own, so the last non-empty cwd (the deepest child) is the real one."
  (let ((cwd nil))
    (dolist (proc procs cwd)
      (when-let* ((c (gp-helm-terminal-iterm2--pid-cwd (car proc))))
        (unless (string-empty-p c)
          (setq cwd c))))))

(defun gp-helm-terminal-iterm2--procs-job (procs)
  "Return the AI command among PROCS, or nil.
Matches each process command's basename against
`gp-helm-terminal-ai-job-regexp', so a session running `claude' or
`opencode' is recognised regardless of the iTerm2 window title."
  (let ((regexp gp-helm-terminal-ai-job-regexp))
    (cl-loop for (_pid . comm) in procs
             for base = (file-name-nondirectory comm)
             when (and regexp base (string-match-p regexp base))
             return base)))

(defun gp-helm-terminal-iterm2--session-from-fields (fields)
  "Build a terminal session struct from parsed FIELD strings.
If the session carries our `user.gpRepo' tag (FIELDS index 2) it is the
authoritative repo path and AI marker: it survives the AI tool's title
rewrites and pty wrappers (e.g. kiro-cli-term) that hide the real
process from the tty.  Untagged sessions derive cwd and job from the
tty's processes."
  (let* ((tag (let ((v (nth 2 fields)))
                ;; AppleScript renders an unset variable as "missing value".
                (and v (not (string-empty-p v))
                     (not (string-equal v "missing value"))
                     v)))
         (procs (unless tag
                  (gp-helm-terminal-iterm2--tty-procs (nth 1 fields)))))
    (make-gp-helm-terminal-session
     :id (nth 0 fields)
     :path (or tag (gp-helm-terminal-iterm2--procs-cwd procs))
     ;; A tag means we opened it for an AI tool; keep it eligible.
     :job-name (if tag "claude" (gp-helm-terminal-iterm2--procs-job procs))
     :name (nth 3 fields)
     :promptp (string-equal (nth 4 fields) "true"))))

(defun gp-helm-terminal-iterm2--run-script (lines &rest args)
  "Run AppleScript LINES via `gp-helm-terminal--run-applescript' with ARGS."
  (apply #'gp-helm-terminal--run-applescript "iTerm2" lines args))

(defun gp-helm-terminal-iterm2--parse-sessions (text)
  "Parse iTerm2 session listing TEXT into session structs."
  (let ((sep gp-helm-terminal-iterm2--field-separator))
    (cl-loop for line in (split-string text "\n" t)
             for fields = (split-string line sep)
             when (= (length fields) 5)
             collect (gp-helm-terminal-iterm2--session-from-fields fields))))

(defun gp-helm-terminal-iterm2-list-sessions ()
  "Return iTerm2 sessions as `gp-helm-terminal-session' structs."
  (gp-helm-terminal-iterm2--parse-sessions
   (gp-helm-terminal-iterm2--run-script
    (list
     ;; NB: `rows' is an iTerm2 dictionary term (a session/window dimension),
     ;; so `set rows to {}' inside the tell block fails with -10006.  Use
     ;; non-colliding local names (gpRows / gpWin / gpTab / gpSess).
     "set oldTids to AppleScript's text item delimiters"
     "set gpRows to {}"
     (format "tell application %S" gp-helm-terminal-iterm2-application)
     "repeat with gpWin in windows"
     "repeat with gpTab in tabs of gpWin"
     "repeat with s in sessions of gpTab"
     "set sid to unique id of s"
     "set stty to \"\""
     "set sjob to \"\""
     "set sname to \"\""
     "set sprompt to \"false\""
     "try"
     "set stty to (tty of s) as string"
     "end try"
     "try"
     "set sname to (name of s) as string"
     "end try"
     ;; Our own tag: the repo this session was opened for (set on open).
     "try"
     (format "set gpTag to (variable s named %S)" "user.gpRepo")
     "if gpTag is not missing value then set sjob to (gpTag as string)"
     "end try"
     "try"
     "set sprompt to ((is at shell prompt of s) as string)"
     "end try"
     (format "copy (sid & %S & stty & %S & sjob & %S & sname & %S & sprompt) to end of gpRows"
             gp-helm-terminal-iterm2--field-separator
             gp-helm-terminal-iterm2--field-separator
             gp-helm-terminal-iterm2--field-separator
             gp-helm-terminal-iterm2--field-separator)
     "end repeat"
     "end repeat"
     "end repeat"
     "end tell"
     "set AppleScript's text item delimiters to linefeed"
     "set outText to gpRows as string"
     "set AppleScript's text item delimiters to oldTids"
     "return outText"))))

(defun gp-helm-terminal-iterm2--call-with-paste-file (text fn)
  "Write TEXT to a temporary file, call FN with its path, then delete it.
A file-backed paste keeps multi-line input one block; `write text'
would press Enter on every newline."
  (let ((file (make-temp-file "gp-iterm2-paste-")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert text))
          (funcall fn file))
      (when (file-exists-p file)
        (delete-file file)))))

(defun gp-helm-terminal-iterm2-send-text (session-id text submit)
  "Send TEXT to the running REPL in SESSION-ID via iTerm2.
TEXT is pasted from a temporary file so its newlines stay literal.  When
SUBMIT is non-nil, a separate carriage return is sent afterwards to submit
the prompt."
  (gp-helm-terminal-iterm2--call-with-paste-file
   text
   (lambda (file)
     (gp-helm-terminal-iterm2--run-script
      (list
       "on run argv"
       "set targetId to item 1 of argv"
       "set payloadFile to POSIX file (item 2 of argv)"
       "set submitFlag to item 3 of argv"
       (format "tell application %S" gp-helm-terminal-iterm2-application)
       "activate"
       "repeat with w in windows"
       "repeat with t in tabs of w"
       "repeat with s in sessions of t"
       "if (unique id of s) is targetId then"
       "tell w to select"
       "tell t to select"
       "tell s"
       "select"
       "write contents of file payloadFile"
       "if submitFlag is \"true\" then"
       "delay 0.1"
       "write text (ASCII character 13) newline NO"
       "end if"
       "end tell"
       "return \"ok\""
       "end if"
       "end repeat"
       "end repeat"
       "end repeat"
       "end tell"
       "error \"No matching iTerm2 session found\""
       "end run")
      session-id file (if submit "true" "false")))))

(defun gp-helm-terminal-iterm2--cd-launch-line (directory command)
  "Return a short shell line that enters DIRECTORY and starts COMMAND.
When COMMAND is empty, only the `cd' is emitted.  The prompt is pasted
separately, so this line stays short and free of newlines."
  (let ((cd (format "cd %s" (shell-quote-argument directory))))
    (if (and command (not (string-empty-p command)))
        (format "%s && %s" cd command)
      cd)))

(defun gp-helm-terminal-iterm2-open-session (directory text command delay submit)
  "Open a new iTerm2 window in DIRECTORY, start COMMAND, and send TEXT.
After DELAY seconds (so a tool like Claude has started) TEXT is
delivered as one file-backed paste, followed by a submit when SUBMIT
is non-nil.  An empty TEXT sends nothing."
  (let ((cd-launch (gp-helm-terminal-iterm2--cd-launch-line directory command)))
    (gp-helm-terminal-iterm2--call-with-paste-file
     text
     (lambda (file)
       (gp-log 'info "iTerm2 open: dir=%s command=%s delay=%s" directory command delay)
       (gp-helm-terminal-iterm2--run-script
        (list
         "on run argv"
         "set cdLaunch to item 1 of argv"
         "set payloadFile to POSIX file (item 2 of argv)"
         "set submitFlag to item 3 of argv"
         "set repoTag to item 4 of argv"
         ;; A literal, not an argv string: `\"0.6\" as number' fails under
         ;; decimal-comma locales (error -1700).
         (format "set launchDelay to %s"
                 (number-to-string (max 0 (or delay 1.0))))
         (format "tell application %S" gp-helm-terminal-iterm2-application)
         "activate"
         "if (count of windows) > 0 then"
         "tell current window"
         "set newTab to (create tab with default profile)"
         "end tell"
         "else"
         "set newWindow to (create window with default profile)"
         "end if"
         "delay 0.2"
         "set gpSession to current session of current tab of current window"
         ;; Tag the session with the repo so later discovery can reuse it.
         (format "set variable gpSession named %S to repoTag" "user.gpRepo")
         "tell gpSession"
         "write text cdLaunch"
         "if (item 5 of argv) is \"true\" then"
         "delay launchDelay"
         "write contents of file payloadFile"
         "if submitFlag is \"true\" then"
         "delay 0.1"
         "write text (ASCII character 13) newline NO"
         "end if"
         "end if"
         "end tell"
         "end tell"
         "return \"ok\""
         "end run")
        cd-launch file (if submit "true" "false") directory
        (if (string-empty-p (or text "")) "false" "true"))))))

(provide 'gp-helm-terminal-iterm2)
;;; gp-helm-terminal-iterm2.el ends here
