;;; gp-pr-actions-test.el --- Tests for user-pluggable PR actions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'gp-pr-actions)

(defconst gp-test-actions--pr
  '((id . 42)
    (title . "Implement thing")
    (source (branch (name . "feat/demo")))
    (destination (branch (name . "main"))
                 (repository (full_name . "acme/repo")))))

(ert-deftest gp-test-pr-action-context-without-checkout-skips-git ()
  (cl-letf (((symbol-function 'gp-local-ensure-checkout)
             (lambda (_pr) (error "must not check out"))))
    (let ((ctx (gp-pr-action-context gp-test-actions--pr)))
      (should (equal (plist-get ctx :repo) "acme/repo"))
      (should (equal (plist-get ctx :id) 42))
      (should (equal (plist-get ctx :source-branch) "feat/demo"))
      (should (equal (plist-get ctx :destination-branch) "main"))
      (should-not (plist-member ctx :dir)))))

(ert-deftest gp-test-pr-action-context-checkout-falls-back-to-clone ()
  (cl-letf (((symbol-function 'gp-local-ensure-checkout)
             (lambda (_pr) (user-error "conflict")))
            ((symbol-function 'gp-local-find-checkout) (lambda (_n) "/tmp/clone")))
    (let ((ctx (gp-pr-action-context gp-test-actions--pr t)))
      (should (equal (plist-get ctx :dir) "/tmp/clone"))
      (should (equal (plist-get ctx :checkout-error) "conflict")))))

(ert-deftest gp-test-pr-action-run-passes-context-to-user-function ()
  (let (got)
    (cl-letf (((symbol-function 'gp-local-ensure-checkout) (lambda (_pr) "/tmp/repo")))
      (gp-pr-action-run (list :label "Mine" :checkout t
                              :function (lambda (ctx) (setq got ctx)))
                        gp-test-actions--pr)
      (should (equal (plist-get got :dir) "/tmp/repo"))
      (should-not (plist-get got :checkout-error))
      (should (equal (plist-get got :pr) gp-test-actions--pr)))))

(ert-deftest gp-test-pr-action-run-rejects-a-non-function ()
  (should-error (gp-pr-action-run '(:label "Bad" :function no-such-fn-xyz)
                                  gp-test-actions--pr)
                :type 'user-error))

(ert-deftest gp-test-pr-actions-install-keys-binds-and-clears-stale-keys ()
  (let ((map (make-sparse-keymap))
        (gp-pr-actions--bound-keys nil)
        (ran nil))
    (let ((gp-pr-actions (list (list :key "Y" :label "Y"
                                     :function (lambda (_ctx) (setq ran t))))))
      (gp-pr-actions-install-keys map)
      (should (commandp (lookup-key map "Y")))
      (let ((gp--pr gp-test-actions--pr))
        (funcall (lookup-key map "Y")))
      (should ran))
    (let ((gp-pr-actions (list (list :key "Q" :label "Q" :function #'ignore))))
      (gp-pr-actions-install-keys map)
      (should-not (lookup-key map "Y"))
      (should (commandp (lookup-key map "Q"))))))

(ert-deftest gp-test-pr-action-button-label-shows-key ()
  (should (equal (gp-pr-action-button-label '(:key "z" :label "Stealth")) "Stealth [z]"))
  (should (equal (gp-pr-action-button-label '(:label "Plain")) "Plain")))

(ert-deftest gp-test-pr-actions-defaults-are-terminal-and-stealth ()
  (let ((fns (mapcar (lambda (a) (plist-get a :function))
                     (eval (car (get 'gp-pr-actions 'standard-value))))))
    (should (memq 'gp-helm-terminal-shell-action fns))
    (should (memq 'gp-stealth-open-session fns))
    (dolist (fn fns) (should (functionp fn)))))

(ert-deftest gp-test-pr-actions-keys-follow-a-plain-setq-on-next-detail-buffer ()
  (let ((gp-pr-actions--bound-keys nil)
        (gp-pr-actions (list (list :key "C-c y" :label "Y" :function #'ignore))))
    (with-temp-buffer
      (gp-detail-mode)
      (should (commandp (lookup-key gp-detail-mode-map (kbd "C-c y")))))
    (setq gp-pr-actions nil)
    (with-temp-buffer
      (gp-detail-mode)
      (should-not (lookup-key gp-detail-mode-map (kbd "C-c y"))))))

(provide 'gp-pr-actions-test)
;;; gp-pr-actions-test.el ends here
