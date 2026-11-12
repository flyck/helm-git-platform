;;; gp-test-helper.el --- Suite-wide test isolation -*- lexical-binding: t; -*-

;;; Commentary:

;; Keeps the suite from reaching the developer's desktop: notifications
;; go to `gp-test-notifications' instead of the OS, and any osascript
;; call (notifications, iTerm2/Ghostty automation) that a test forgot to
;; stub signals instead of running.  Loaded first by run-tests.sh and
;; required by the mock services, so Eldev picks it up too.

;;; Code:

(defvar gp-test-notifications nil
  "Notifications raised during the run, newest first: (TITLE BODY URGENT).")

(defun gp-test--record-notification (title body urgent)
  "Record a notification with TITLE, BODY and URGENT instead of showing it."
  (push (list title body urgent) gp-test-notifications))

;; Set whether or not git-platform.el is loaded yet: a defcustom keeps an
;; existing value.
(setq gp-notify-function #'gp-test--record-notification)

(defconst gp-test--desktop-programs '("osascript" "terminal-notifier")
  "Programs that act on the desktop and must never run from a test.")

(defun gp-test--refuse-desktop-program (program &rest _)
  "Signal when PROGRAM would act on the developer's desktop."
  (when (member (and (stringp program) (file-name-nondirectory program))
                gp-test--desktop-programs)
    (error "Test reached %s; stub it" program)))

;; No trampoline: building one needs a working libgccjit, and the package
;; is loaded as source or bytecode, which honours the advice anyway.
(defvar native-comp-enable-subr-trampolines)
(let ((native-comp-enable-subr-trampolines nil))
  (advice-add 'call-process :before #'gp-test--refuse-desktop-program)
  (advice-add 'start-process :before
              (lambda (_name _buffer program &rest _)
                (gp-test--refuse-desktop-program program))))

(provide 'gp-test-helper)
;;; gp-test-helper.el ends here
