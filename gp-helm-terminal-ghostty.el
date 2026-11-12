;;; gp-helm-terminal-ghostty.el --- Ghostty backend for gp-helm-terminal -*- lexical-binding: t; -*-

;;; Commentary:

;; Ghostty backend for `gp-helm-terminal', using Ghostty's native
;; AppleScript dictionary (since 1.3.0): `terminal' objects with a stable
;; `id' and `working directory', and `input text'/`send key' addressed by
;; that id, so no window-focus juggling is needed (System Events window
;; enumeration is unreliable for Ghostty anyway).  There is no tty/job-name
;; introspection, so AI-session detection matches the terminal title.
;;
;; New surfaces run the login shell, never `command of cfg': that execs
;; directly and bypasses shell aliases (a `claude' alias would not
;; resolve).  The launch command is typed into the shell instead.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gp-helm-terminal)
(require 'gp-log)

(defcustom gp-helm-terminal-ghostty-application "Ghostty"
  "Application name used for AppleScript calls."
  :type 'string
  :group 'bitbucket)

(defun gp-helm-terminal-ghostty--run-script (lines &rest args)
  "Run AppleScript LINES via `gp-helm-terminal--run-applescript' with ARGS."
  (apply #'gp-helm-terminal--run-applescript "Ghostty" lines args))

(defconst gp-helm-terminal-ghostty--field-separator (string 31)
  "Field separator used between AppleScript terminal columns.")

(defun gp-helm-terminal-ghostty--parse-sessions (text)
  "Parse Ghostty terminal listing TEXT into session structs."
  (let ((sep gp-helm-terminal-ghostty--field-separator))
    (cl-loop for line in (split-string text "\n" t)
             for fields = (split-string line sep)
             when (= (length fields) 3)
             collect (make-gp-helm-terminal-session
                      :id (nth 0 fields)
                      :path (nth 1 fields)
                      :job-name nil
                      :name (nth 2 fields)
                      :promptp nil))))

(defun gp-helm-terminal-ghostty-list-sessions ()
  "Return Ghostty terminals as `gp-helm-terminal-session' structs.
`job-name' is left nil, so `gp-helm-terminal--ai-session-p' matches the
terminal title (`name') instead."
  (gp-helm-terminal-ghostty--parse-sessions
   (gp-helm-terminal-ghostty--run-script
    (list
     "set oldTids to AppleScript's text item delimiters"
     "set gpRows to {}"
     (format "tell application %S" gp-helm-terminal-ghostty-application)
     "repeat with s in terminals"
     "set sid to id of s"
     "set sdir to \"\""
     "set sname to \"\""
     "try"
     "set sdir to (working directory of s) as string"
     "end try"
     "try"
     "set sname to (name of s) as string"
     "end try"
     (format "copy (sid & %S & sdir & %S & sname) to end of gpRows"
             gp-helm-terminal-ghostty--field-separator
             gp-helm-terminal-ghostty--field-separator)
     "end repeat"
     "end tell"
     "set AppleScript's text item delimiters to linefeed"
     "set outText to gpRows as string"
     "set AppleScript's text item delimiters to oldTids"
     "return outText"))))

(defun gp-helm-terminal-ghostty-send-text (terminal-id text submit)
  "Send TEXT to the running terminal TERMINAL-ID via Ghostty.
When SUBMIT is non-nil, a separate Enter key event is sent afterwards."
  (gp-helm-terminal-ghostty--run-script
   (list
    "on run argv"
    "set targetId to item 1 of argv"
    "set theText to item 2 of argv"
    "set submitFlag to item 3 of argv"
    (format "tell application %S" gp-helm-terminal-ghostty-application)
    "input text theText to terminal id targetId"
    "if submitFlag is \"true\" then"
    "send key \"enter\" to terminal id targetId"
    "end if"
    "end tell"
    "end run")
   terminal-id text (if submit "true" "false")))

(defun gp-helm-terminal-ghostty-open-session (directory text command delay submit)
  "Open a new Ghostty window in DIRECTORY, start COMMAND, and send TEXT.
COMMAND, when given, is typed into the login shell (see Commentary).
After DELAY seconds TEXT is delivered via `input text', followed by a
submit when SUBMIT is non-nil.  An empty TEXT sends nothing, which is
how a plain shell is opened."
  (gp-log 'info "Ghostty open: dir=%s command=%s delay=%s" directory command delay)
  (gp-helm-terminal-ghostty--run-script
   (list
    "on run argv"
    "set theDir to item 1 of argv"
    "set theCommand to item 2 of argv"
    "set launchDelay to (item 3 of argv) as real"
    "set theText to item 4 of argv"
    "set submitFlag to item 5 of argv"
    (format "tell application %S" gp-helm-terminal-ghostty-application)
    "activate"
    "set cfg to new surface configuration"
    "set initial working directory of cfg to theDir"
    "set w to new window with configuration cfg"
    "delay 0.3"
    "set t to focused terminal of selected tab of w"
    "set targetId to id of t"
    "if theCommand is not \"\" then"
    "input text theCommand to terminal id targetId"
    "send key \"enter\" to terminal id targetId"
    "end if"
    "if theText is not \"\" then"
    "delay launchDelay"
    "input text theText to terminal id targetId"
    "if submitFlag is \"true\" then"
    "send key \"enter\" to terminal id targetId"
    "end if"
    "end if"
    "end tell"
    "return targetId"
    "end run")
   directory (or command "") (number-to-string (max 0 (or delay 1.0))) text
   (if submit "true" "false")))

(provide 'gp-helm-terminal-ghostty)
;;; gp-helm-terminal-ghostty.el ends here
