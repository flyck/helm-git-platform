;;; gp-pr-actions.el --- User-pluggable PR actions for the detail buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; A PR action is an entry in `gp-pr-actions': a key, a label and a
;; function.  Every entry becomes a button in the PR detail header, a key
;; in `gp-detail-mode-map', and a choice in the `x' menu
;; (`gp-detail-pr-action').  Adding your own is one entry:
;;
;;   (add-to-list 'gp-pr-actions
;;                '(:key "Y" :label "Copy branch"
;;                  :function my-copy-branch :checkout nil) t)
;;
;; The function receives one context plist (see `gp-pr-action-context'):
;; :pr, :repo, :id, :title, :url, :source-branch, :destination-branch,
;; and -- when the entry has :checkout -- :dir (the local checkout, on the
;; PR's source branch) and :checkout-error (why switching branch failed;
;; :dir is then the clone on whatever branch it was on).
;;
;; Shipped entries: a plain shell in the terminal (`$') and a drafted
;; Stealth session (`z', see `gp-stealth').

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'git-platform)
(require 'gp-local)
(require 'gp-log)

(defvar gp-detail-mode-map)
(defvar gp--pr)

;; The shipped actions live in their own files, loaded on first use.
(autoload 'gp-helm-terminal-shell-action "gp-helm-terminal")
(autoload 'gp-stealth-open-session "gp-stealth")

(defun gp-pr-actions--set (symbol value)
  "Set SYMBOL to VALUE and rebind the detail keys when the map exists."
  (set-default symbol value)
  (when (boundp 'gp-detail-mode-map)
    (gp-pr-actions-install-keys gp-detail-mode-map)))

(defcustom gp-pr-actions
  '((:key "$" :label "🐚 Open in terminal" :function gp-helm-terminal-shell-action
     :checkout t
     :help "Check out the PR branch and open a plain shell there (gp-helm-terminal-backend)")
    (:key "z" :label "🥷 Open in Stealth" :function gp-stealth-open-session
     :checkout t
     :help "Check out the PR branch and start a Stealth session there, its context drafted"))
  "Actions offered on a pull request in the detail buffer.
Each entry is a plist:
  :key       key in the detail buffer (a `kbd' string), or nil for none
  :label     button and menu text
  :function  called with the context plist from `gp-pr-action-context'
  :checkout  non-nil to check out the PR's source branch first and
             pass its directory as :dir
  :help      optional tooltip
Changing it through Customize (or `setopt') rebinds the keys at once;
after a plain `setq' or `add-to-list' they follow when the next detail
buffer opens."
  :type '(repeat (plist :options ((:key string) (:label string)
                                  (:function function) (:checkout boolean)
                                  (:help string))))
  :set #'gp-pr-actions--set
  :group 'bitbucket)

(defvar gp-pr-actions--bound-keys nil
  "Keys the last `gp-pr-actions-install-keys' bound, so stale ones are cleared.")

(defun gp-pr-actions--checkout (pr)
  "Return (DIR . ERROR) for PR's checkout on its source branch.
When switching branch fails but a clone exists, DIR is that clone and
ERROR the reason; with no clone at all the error is re-signalled."
  (condition-case err
      (cons (gp-local-ensure-checkout pr) nil)
    (error
     (if-let* ((dir (ignore-errors (gp-local-find-checkout (gp-pr-full-name pr)))))
         (cons dir (error-message-string err))
       (signal (car err) (cdr err))))))

(defun gp-pr-action-context (pr &optional checkout)
  "Return the context plist an action receives for PR.
With CHECKOUT, also check out the source branch and add :dir and
:checkout-error (see `gp-pr-actions')."
  (let ((ctx (list :pr pr
                   :repo (gp-pr-full-name pr)
                   :id (alist-get 'id pr)
                   :title (alist-get 'title pr)
                   :url (ignore-errors (gp-pr-web-url pr))
                   :source-branch (gp-pr-source-branch pr)
                   :destination-branch (gp-pr-destination-branch pr))))
    (if (not checkout)
        ctx
      (pcase-let ((`(,dir . ,failure) (gp-pr-actions--checkout pr)))
        (when failure
          (gp-log 'warn "PR action: checkout of %s failed, using %s: %s"
                  (plist-get ctx :repo) dir failure))
        (append ctx (list :dir dir :checkout-error failure))))))

(defun gp-pr-action-run (action pr)
  "Run ACTION (an entry of `gp-pr-actions') on PR."
  (let ((fn (plist-get action :function)))
    (unless (functionp fn)
      (user-error "PR action %S: %S is not a function" (plist-get action :label) fn))
    (gp-log 'info "PR action: %s on %s #%s"
            (plist-get action :label) (gp-pr-full-name pr) (alist-get 'id pr))
    (funcall fn (gp-pr-action-context pr (plist-get action :checkout)))))

(defun gp-pr-action-command (action)
  "Return an interactive command running ACTION on the detail buffer's PR."
  (lambda ()
    (interactive)
    (gp-pr-action-run action gp--pr)))

(defun gp-pr-actions-install-keys (map)
  "Bind every keyed entry of `gp-pr-actions' in MAP.
Keys a previous call bound and the list no longer names are cleared."
  (dolist (key gp-pr-actions--bound-keys)
    (define-key map (kbd key) nil t))
  (setq gp-pr-actions--bound-keys nil)
  (dolist (action gp-pr-actions)
    (when-let* ((key (plist-get action :key)))
      (define-key map (kbd key) (gp-pr-action-command action))
      (push key gp-pr-actions--bound-keys))))

(defun gp-pr-action-button-label (action)
  "Return ACTION's button text, with its key when it has one."
  (if-let* ((key (plist-get action :key)))
      (format "%s [%s]" (plist-get action :label) key)
    (plist-get action :label)))

(defun gp-detail-pr-action ()
  "Pick one of `gp-pr-actions' and run it on this buffer's PR."
  (interactive)
  (unless gp-pr-actions (user-error "No PR actions configured (gp-pr-actions)"))
  (let* ((choices (mapcar (lambda (a) (cons (gp-pr-action-button-label a) a))
                          gp-pr-actions))
         (pick (completing-read "PR action: " choices nil t)))
    (gp-pr-action-run (cdr (assoc pick choices)) gp--pr)))

(provide 'gp-pr-actions)
;;; gp-pr-actions.el ends here
