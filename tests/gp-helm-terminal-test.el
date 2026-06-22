;;; gp-helm-terminal-test.el --- Tests for terminal handoff service -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests the pure parts of the terminal handoff service: prompt building and
;; session-selection/planning. Backend AppleScript execution is stubbed.

;;; Code:

(require 'ert)
(require 'gp-helm-terminal)
(require 'gp-helm-terminal-iterm2)

(ert-deftest gp-test-terminal-build-prompt ()
  (let* ((pr '((id . 42)
               (title . "Implement thing")
               (destination (repository (full_name . "acme/repo")))))
         (comment '((content (raw . "Please rename this helper."))
                    (inline (path . "src/app.ts") (to . 17))))
         (text (gp-helm-terminal--build-prompt pr comment)))
    (should (string-match-p "please implement this comment:" text))
    (should (string-match-p "Repo: acme/repo" text))
    (should (string-match-p "PR: #42 Implement thing" text))
    (should (string-match-p "Comment: src/app.ts:17" text))
    (should (string-match-p "Please rename this helper\." text))))

(ert-deftest gp-test-terminal-choose-session-prefers-exact-ai-match ()
  (let* ((repo "/tmp/repo")
         (sessions (list (make-gp-helm-terminal-session
                          :id "1" :path "/tmp/repo/sub" :job-name "opencode")
                         (make-gp-helm-terminal-session
                          :id "2" :path "/tmp/repo" :job-name "opencode")
                         (make-gp-helm-terminal-session
                          :id "3" :path "/tmp/repo" :job-name "zsh"))))
    (should (equal (gp-helm-terminal-session-id
                    (gp-helm-terminal--choose-session sessions repo))
                   "2"))))

(ert-deftest gp-test-terminal-plan-opens-when-no-ai-session-matches ()
  (let* ((repo "/tmp/repo")
         (gp-helm-terminal-launch-command "claude")
         (gp-helm-terminal-launch-delay 1.2)
         (sessions (list (make-gp-helm-terminal-session
                          :id "1" :path "/tmp/repo" :job-name "zsh")))
         (plan (gp-helm-terminal--plan sessions repo)))
    (should (eq (plist-get plan :action) 'open))
    (should (equal (plist-get plan :directory) repo))
    (should (equal (plist-get plan :command) "claude --permission-mode auto"))
    (should (= (plist-get plan :delay) 1.2))))

(ert-deftest gp-test-terminal-launch-command-keeps-explicit-permission-mode ()
  (let ((gp-helm-terminal-launch-command "claude --permission-mode default"))
    (should (equal (gp-helm-terminal--launch-command)
                   "claude --permission-mode default"))))

(ert-deftest gp-test-terminal-send-comment-opens-or-reuses-via-backend ()
  (let* ((pr '((id . 42)
               (title . "Implement thing")
               (source (branch (name . "feat/demo")))
               (destination (branch (name . "main"))
                            (repository (full_name . "acme/repo")))))
         (comment '((content (raw . "Please rename this helper."))))
         (opened nil)
         (sent nil))
    (cl-letf (((symbol-function 'gp-local-ensure-checkout) (lambda (_pr) "/tmp/repo"))
              ((symbol-function 'gp-helm-terminal--list-sessions)
               (lambda ()
                 (list (make-gp-helm-terminal-session
                        :id "abc" :path "/tmp/repo" :job-name "opencode"))))
              ((symbol-function 'gp-helm-terminal-iterm2-send-text)
               (lambda (id text submit) (setq sent (list id text submit))))
              ((symbol-function 'gp-helm-terminal-iterm2-open-session)
               (lambda (&rest args) (setq opened args))))
      (let ((gp-helm-terminal-backend 'iterm2))
        (gp-helm-terminal-send-comment pr comment)
        (should (equal (car sent) "abc"))
        (should (string-match-p "Please rename this helper\." (cadr sent)))
        (should (caddr sent))
        (should-not opened)))))

(ert-deftest gp-test-terminal-send-comment-logs-errors ()
  (let* ((pr '((id . 42)
               (title . "Implement thing")
               (source (branch (name . "feat/demo")))
               (destination (branch (name . "main"))
                            (repository (full_name . "acme/repo")))))
         (comment '((content (raw . "Please rename this helper."))))
         (logged nil))
    (cl-letf (((symbol-function 'gp-local-ensure-checkout) (lambda (_pr) "/tmp/repo"))
              ((symbol-function 'gp-helm-terminal--safe-list-sessions) (lambda () nil))
              ((symbol-function 'gp-helm-terminal--execute)
               (lambda (&rest _) (error "boom")))
              ((symbol-function 'gp-log-error)
               (lambda (fmt &rest args)
                 (setq logged (apply #'format fmt args)))))
      (should-error (gp-helm-terminal-send-comment pr comment))
      (should (string-match-p "terminal handoff failed: boom" logged)))))

(ert-deftest gp-test-terminal-send-comment-falls-back-to-opening-session ()
  (let* ((pr '((id . 42)
               (title . "Implement thing")
               (source (branch (name . "feat/demo")))
               (destination (branch (name . "main"))
                            (repository (full_name . "acme/repo")))))
         (comment '((content (raw . "Please rename this helper."))))
         (opened nil)
         (warned nil))
    (cl-letf (((symbol-function 'gp-local-ensure-checkout) (lambda (_pr) "/tmp/repo"))
              ((symbol-function 'gp-helm-terminal--list-sessions)
               (lambda () (error "access denied")))
              ((symbol-function 'gp-log)
               (lambda (level fmt &rest args)
                 (when (eq level 'warn)
                   (setq warned (apply #'format fmt args)))))
              ((symbol-function 'gp-helm-terminal-iterm2-open-session)
               (lambda (&rest args) (setq opened args))))
      (let ((gp-helm-terminal-backend 'iterm2))
        (gp-helm-terminal-send-comment pr comment)
        (should (string-match-p "terminal session discovery failed" warned))
        (should (equal (car opened) "/tmp/repo"))))))

(ert-deftest gp-test-terminal-iterm2-session-from-fields-uses-tag ()
  "A tagged session (field 2 = user.gpRepo) takes the tag as its path and
is treated as an AI session, without touching the tty."
  (cl-letf (((symbol-function 'gp-helm-terminal-iterm2--tty-procs)
             (lambda (&rest _) (error "tty must not be consulted when tagged"))))
    (let ((session (gp-helm-terminal-iterm2--session-from-fields
                    '("abc" "/dev/ttys123" "/tmp/repo" "Claude" "true"))))
      (should (equal (gp-helm-terminal-session-id session) "abc"))
      (should (equal (gp-helm-terminal-session-path session) "/tmp/repo"))
      (should (gp-helm-terminal--ai-session-p session))
      (should (equal (gp-helm-terminal-session-name session) "Claude"))
      (should (gp-helm-terminal-session-promptp session)))))

(ert-deftest gp-test-terminal-iterm2-session-from-fields-falls-back-to-tty ()
  "An untagged session (empty field 2) derives cwd and job from the tty's
processes."
  (cl-letf (((symbol-function 'gp-helm-terminal-iterm2--tty-procs)
             (lambda (tty)
               (should (equal tty "/dev/ttys123"))
               '(("100" . "/usr/bin/login") ("101" . "claude")))))
    ;; cwd resolution is per-pid; stub it to a fixed dir.
    (cl-letf (((symbol-function 'gp-helm-terminal-iterm2--pid-cwd)
               (lambda (_pid) "/tmp/repo")))
      (let ((session (gp-helm-terminal-iterm2--session-from-fields
                      '("abc" "/dev/ttys123" "" "Claude" "false"))))
        (should (equal (gp-helm-terminal-session-path session) "/tmp/repo"))
        (should (equal (gp-helm-terminal-session-job-name session) "claude"))
        (should-not (gp-helm-terminal-session-promptp session))))))

(ert-deftest gp-test-terminal-iterm2-cd-launch-line-starts-command ()
  "The cd-launch line enters the repo and starts the command, with no
prompt embedded inline (so it stays short and newline-free)."
  (let ((line (gp-helm-terminal-iterm2--cd-launch-line "/tmp/my repo" "claude")))
    (should (string-match-p "cd /tmp/my\\\\ repo" line))
    (should (string-match-p "&& claude" line))
    (should-not (string-match-p "\n" line)))
  ;; With no command, only the cd is emitted.
  (should (equal (gp-helm-terminal-iterm2--cd-launch-line "/tmp/repo" nil)
                 "cd /tmp/repo"))
  (should (equal (gp-helm-terminal-iterm2--cd-launch-line "/tmp/repo" "")
                 "cd /tmp/repo")))

(ert-deftest gp-test-terminal-iterm2-call-with-paste-file-writes-and-cleans-up ()
  "The file-backed paste helper must expose TEXT to the callback, then delete it."
  (let ((text "line one\nline two & 'q' $v `t`")
        (seen-path nil)
        (seen-contents nil))
    (gp-helm-terminal-iterm2--call-with-paste-file
     text
     (lambda (file)
       (setq seen-path file)
       (setq seen-contents
             (with-temp-buffer
               (insert-file-contents file)
               (buffer-string)))
       (should (file-exists-p file))))
    (should (equal seen-contents text))
    (should seen-path)
    (should-not (file-exists-p seen-path))))

(ert-deftest gp-test-terminal-iterm2-send-text-uses-file-backed-paste-and-cr-submit ()
  "Reused-session delivery should paste from a file and submit with CR."
  (let ((captured-lines nil)
        (captured-args nil)
        (seen-contents nil))
    (cl-letf (((symbol-function 'gp-helm-terminal-iterm2--run-script)
               (lambda (lines &rest args)
                 (setq captured-lines lines)
                 (setq captured-args args)
                 (with-temp-buffer
                   (insert-file-contents (nth 1 args))
                   (setq seen-contents (buffer-string)))
                 "ok")))
      (gp-helm-terminal-iterm2-send-text "sess-1" "hello\nworld" t)
      (should (equal (car captured-args) "sess-1"))
      (should (equal (nth 2 captured-args) "true"))
      (should (equal seen-contents "hello\nworld"))
      (should (member "tell s" captured-lines))
      (should (member "select" captured-lines))
      (should (member "write contents of file payloadFile" captured-lines))
      (should (member "write text (ASCII character 13) newline NO" captured-lines))
      (should (member "end tell" captured-lines)))))

(provide 'gp-helm-terminal-test)
;;; gp-helm-terminal-test.el ends here
