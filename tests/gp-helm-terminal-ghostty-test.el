;;; gp-helm-terminal-ghostty-test.el --- Tests for the Ghostty terminal backend -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests the pure parts of the Ghostty backend: session-listing parsing and
;; the AppleScript command shape for send/open. `osascript' execution is
;; stubbed via `gp-helm-terminal-ghostty--run-script'.

;;; Code:

(require 'ert)
(require 'gp-helm-terminal)
(require 'gp-helm-terminal-ghostty)

(ert-deftest gp-test-terminal-ghostty-parse-sessions ()
  (let* ((sep gp-helm-terminal-ghostty--field-separator)
         (text (concat "abc" sep "/tmp/repo" sep "Claude" "\n"
                        "def" sep "/tmp/other" sep "zsh"))
         (sessions (gp-helm-terminal-ghostty--parse-sessions text)))
    (should (= (length sessions) 2))
    (should (equal (gp-helm-terminal-session-id (nth 0 sessions)) "abc"))
    (should (equal (gp-helm-terminal-session-path (nth 0 sessions)) "/tmp/repo"))
    (should (equal (gp-helm-terminal-session-name (nth 0 sessions)) "Claude"))
    (should-not (gp-helm-terminal-session-job-name (nth 0 sessions)))
    (should (equal (gp-helm-terminal-session-id (nth 1 sessions)) "def"))))

(ert-deftest gp-test-terminal-ghostty-list-sessions-parses-run-script-output ()
  (let ((sep gp-helm-terminal-ghostty--field-separator))
    (cl-letf (((symbol-function 'gp-helm-terminal-ghostty--run-script)
               (lambda (&rest _) (concat "abc" sep "/tmp/repo" sep "Claude"))))
      (let ((sessions (gp-helm-terminal-ghostty-list-sessions)))
        (should (= (length sessions) 1))
        (should (equal (gp-helm-terminal-session-path (car sessions)) "/tmp/repo"))))))

(ert-deftest gp-test-terminal-ghostty-send-text-passes-args-via-argv ()
  "Send-text must pass the terminal id, text and submit flag as argv items
\(never interpolated into the script text), and submit via a separate
`send key' when requested."
  (let (captured-lines captured-args)
    (cl-letf (((symbol-function 'gp-helm-terminal-ghostty--run-script)
               (lambda (lines &rest args)
                 (setq captured-lines lines)
                 (setq captured-args args)
                 "")))
      (gp-helm-terminal-ghostty-send-text "term-1" "hello\nworld $USER" t)
      (should (equal captured-args '("term-1" "hello\nworld $USER" "true")))
      (should (member "input text theText to terminal id targetId" captured-lines))
      (should (member "send key \"enter\" to terminal id targetId" captured-lines)))))

(ert-deftest gp-test-terminal-ghostty-send-text-skips-submit-when-nil ()
  (let (captured-args)
    (cl-letf (((symbol-function 'gp-helm-terminal-ghostty--run-script)
               (lambda (_lines &rest args) (setq captured-args args) "")))
      (gp-helm-terminal-ghostty-send-text "term-1" "hello" nil)
      (should (equal (nth 2 captured-args) "false")))))

(ert-deftest gp-test-terminal-ghostty-open-session-passes-directory-command-and-text ()
  (let (captured-args)
    (cl-letf (((symbol-function 'gp-helm-terminal-ghostty--run-script)
               (lambda (_lines &rest args) (setq captured-args args) "new-term-id")))
      (let ((result (gp-helm-terminal-ghostty-open-session
                     "/tmp/my repo" "prompt text" "claude" 1.5 t)))
        (should (equal result "new-term-id"))
        (should (equal (nth 0 captured-args) "/tmp/my repo"))
        (should (equal (nth 1 captured-args) "claude"))
        (should (equal (nth 2 captured-args) "1.5"))
        (should (equal (nth 3 captured-args) "prompt text"))
        (should (equal (nth 4 captured-args) "true"))))))

(ert-deftest gp-test-terminal-ghostty-open-session-defaults-command-to-empty ()
  (let (captured-args)
    (cl-letf (((symbol-function 'gp-helm-terminal-ghostty--run-script)
               (lambda (_lines &rest args) (setq captured-args args) "")))
      (gp-helm-terminal-ghostty-open-session "/tmp/repo" "text" nil nil nil)
      (should (equal (nth 1 captured-args) ""))
      (should (equal (nth 2 captured-args) "1.0")))))

(ert-deftest gp-test-terminal-ghostty-open-session-types-command-instead-of-execing ()
  "The launch command must be typed into the shell as ordinary input, never
set via `command of cfg' -- that execs directly and bypasses shell aliases
\(e.g. a `claude' alias defined in .zshrc), silently killing the surface
before the prompt can be sent."
  (let (captured-lines)
    (cl-letf (((symbol-function 'gp-helm-terminal-ghostty--run-script)
               (lambda (lines &rest _args) (setq captured-lines lines) "term-id")))
      (gp-helm-terminal-ghostty-open-session "/tmp/repo" "prompt" "claude" 1.5 t)
      (should-not (cl-some (lambda (l) (string-match-p "command of cfg" l)) captured-lines))
      (should (member "input text theCommand to terminal id targetId" captured-lines)))))

(ert-deftest gp-test-terminal-ghostty-send-comment-dispatches-via-backend ()
  "The generic dispatcher (`gp-helm-terminal-send-comment') must route to the
Ghostty backend functions when `gp-helm-terminal-backend' is `ghostty'."
  (let* ((pr '((id . 42)
               (title . "Implement thing")
               (source (branch (name . "feat/demo")))
               (destination (branch (name . "main"))
                            (repository (full_name . "acme/repo")))))
         (comment '((content (raw . "Please rename this helper."))))
         (sent nil))
    (cl-letf (((symbol-function 'gp-local-ensure-checkout) (lambda (_pr) "/tmp/repo"))
              ((symbol-function 'gp-helm-terminal--list-sessions)
               (lambda ()
                 (list (make-gp-helm-terminal-session
                        :id "ghost-1" :path "/tmp/repo" :job-name nil :name "Claude"))))
              ((symbol-function 'gp-helm-terminal-ghostty-send-text)
               (lambda (id text submit) (setq sent (list id text submit)))))
      (let ((gp-helm-terminal-backend 'ghostty))
        (gp-helm-terminal-send-comment pr comment)
        (should (equal (car sent) "ghost-1"))
        (should (string-match-p "Please rename this helper\\." (cadr sent)))
        (should (caddr sent))))))

(provide 'gp-helm-terminal-ghostty-test)
;;; gp-helm-terminal-ghostty-test.el ends here
