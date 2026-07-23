;;; gp-helm-terminal-ghostty.el --- Ghostty backend for gp-helm-terminal -*- lexical-binding: t; -*-

;;; Commentary:

;; Ghostty backend for `gp-helm-terminal'. Ghostty ships a native AppleScript
;; scripting dictionary (since 1.3.0) exposing `terminal' objects with a
;; stable `id' and `working directory', plus `input text'/`send key' commands
;; addressed directly by that id. Unlike iTerm2, there is no tty/job-name
;; introspection available, so AI-session detection here falls back to the
;; terminal's title (`name'), which AI CLIs (Claude, opencode) typically set.
;;
;; Because targeting is always by stable id, no window/frontmost focus juggling
;; is required (and none would be reliable -- Ghostty's Accessibility/System
;; Events window enumeration is not trustworthy across multiple processes).
;;
;; New surfaces always run the user's login shell -- never Ghostty's
;; `command of cfg', which execs directly and so bypasses shell aliases
;; (e.g. a `claude' shell alias would silently fail to resolve, exiting the
;; surface before a prompt could ever be sent). A launch command is instead
;; typed into that shell as ordinary input, just as the user would type it.

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
  "Run AppleScript LINES with `osascript', passing ARGS via argv.
ARGS are passed positionally (never interpolated into the script text) so
arbitrary text -- including shell metacharacters and embedded newlines --
is safe. Return stdout trimmed, or signal `user-error' with stderr details."
  (gp-log 'info "Ghostty AppleScript: %d lines, %d args" (length lines) (length args))
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
                (gp-log 'info "Ghostty AppleScript output: %s" output))
              output)
          (gp-log-error "Ghostty AppleScript failed (status=%s): %s"
                        status (if (string-empty-p output) "unknown error" output))
          (user-error "Ghostty AppleScript failed: %s"
                      (if (string-empty-p output) "unknown error" output)))))))

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
No tty/job-name introspection is available, so `job-name' is left nil;
`gp-helm-terminal--ai-session-p' falls back to matching the terminal title
(`name') against `gp-helm-terminal-ai-job-regexp'."
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
When SUBMIT is non-nil, a separate Enter key event is sent afterwards to
submit the prompt. TEXT is passed as an argv item, never interpolated into
the script text, so embedded newlines and shell metacharacters are safe."
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
The new terminal runs the user's normal login shell (never Ghostty's
`command of cfg', which execs directly and so never sees shell aliases --
e.g. a `claude' alias defined in .zshrc would silently fail to resolve
and the surface would exit before the prompt could be sent). Instead
COMMAND, when given, is typed into that shell as ordinary input, exactly
as if the user had typed it. After DELAY seconds (to let COMMAND finish
starting), the prompt TEXT is delivered via `input text', followed by a
submit when SUBMIT is non-nil."
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
    "delay launchDelay"
    "input text theText to terminal id targetId"
    "if submitFlag is \"true\" then"
    "send key \"enter\" to terminal id targetId"
    "end if"
    "end tell"
    "return targetId"
    "end run")
   directory (or command "") (number-to-string (max 0 (or delay 1.0))) text
   (if submit "true" "false")))

(provide 'gp-helm-terminal-ghostty)
;;; gp-helm-terminal-ghostty.el ends here
