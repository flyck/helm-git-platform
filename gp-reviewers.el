;;; gp-reviewers.el --- Edit the reviewers of an existing PR -*- lexical-binding: t; -*-

;;; Commentary:

;; A checkbox form for changing who reviews an *existing* pull request,
;; the counterpart to the reviewer section `gp-create.el' shows when the PR
;; is first opened.  Candidates come from the same two protocol ops the
;; create form uses (`gp-repo-default-reviewers' and
;; `gp-repo-suggested-reviewers'), so both backends offer a real pool:
;; Bitbucket's workspace members, GitHub's repo collaborators.
;;
;; Three groups are rendered, in this order:
;;
;;   * Current   -- already on the PR, pre-checked.  Anyone who has
;;                  actually submitted a review (approved / requested
;;                  changes) is *locked*: unticking them is refused,
;;                  because a submitted review stays attached to the PR
;;                  and cannot be withdrawn by dropping the person from
;;                  the reviewer list.
;;   * Default   -- the repo's default reviewers not yet on the PR.
;;   * Suggested -- everyone else who could review.
;;
;; Saving hands the *complete* desired list to
;; `gp-set-pull-request-reviewers'; each backend reaches that end state its
;; own way (Bitbucket a whole-list PUT, GitHub POST/DELETE deltas).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'git-platform)
(require 'gp-local)
(require 'wid-edit)

(declare-function widget-create "wid-edit")
(declare-function widget-insert "wid-edit")
(declare-function widget-value "wid-edit")
(declare-function widget-value-set "wid-edit")
(declare-function widget-setup "wid-edit")
(declare-function widget-forward "wid-edit")
(declare-function widget-backward "wid-edit")
(declare-function widget-button-press "wid-edit")
(declare-function gp-detail-refresh "gp-ui")
(defvar widget-keymap)

(defgroup gp-reviewers nil
  "Editing the reviewers of an existing pull request."
  :group 'bitbucket)

(defvar-local gp-reviewers--pr nil
  "The PR whose reviewers this buffer edits.")

(defvar-local gp-reviewers--return-window nil
  "Window configuration to restore when the form closes.")

(defvar-local gp-reviewers--widgets nil
  "List of plists (:id :name :widget :locked :state) for each candidate row.")

(defvar-local gp-reviewers--origin-buffer nil
  "Detail buffer to refresh after a successful save, if it is still live.")

(defface gp-reviewers-heading '((t :inherit bold :height 1.2))
  "Face for the form's top heading." :group 'gp-reviewers)
(defface gp-reviewers-section '((t :inherit font-lock-keyword-face :weight bold))
  "Face for section labels." :group 'gp-reviewers)

(defun gp-reviewers--buffer-name (pr)
  "Return the reviewer-form buffer name for PR."
  (gp--buffer-name (format "reviewers #%s" (alist-get 'id pr))))

;;;; Candidate assembly (pure) -------------------------------------------------

(defun gp-reviewers--candidates (current defaults suggested)
  "Return the grouped candidate rows to render.
CURRENT is the PR's reviewer plists (as from `gp-pr-reviewers-async',
carrying :id/:name/:state); DEFAULTS and SUGGESTED are user alists
from the repo-level protocol ops.  Returns a list of
\(GROUP-LABEL . ROWS), each row a plist (:id :name :state :on
:locked).  Anyone in CURRENT is omitted from the other two groups, so
no person appears twice."
  (let* ((seen (make-hash-table :test 'equal))
         (row (lambda (id name state on locked)
                (puthash id t seen)
                (list :id id :name (or name id) :state state
                      :on on :locked locked)))
         (current-rows
          (delq nil
                (mapcar (lambda (r)
                          (when-let* ((id (plist-get r :id)))
                            (funcall row id (plist-get r :name)
                                     (plist-get r :state) t
                                     ;; a submitted review cannot be undone here
                                     (memq (plist-get r :state)
                                           '(approved changes)))))
                        current)))
         (rest (lambda (users)
                 (delq nil
                       (mapcar (lambda (u)
                                 (let ((id (alist-get 'uuid u)))
                                   (unless (or (null id) (gethash id seen))
                                     (funcall row id
                                              (or (alist-get 'display_name u)
                                                  (alist-get 'nickname u))
                                              nil nil nil))))
                               users)))))
    (delq nil
          (list (when current-rows (cons "Current" current-rows))
                (when-let* ((rows (funcall rest defaults)))
                  (cons "Default reviewers for this repo" rows))
                (when-let* ((rows (funcall rest suggested)))
                  (cons "Suggested" rows))))))

(defun gp-reviewers--state-badge (state)
  "Return a short label for reviewer STATE."
  (pcase state
    ('approved (propertize "✓ approved" 'face 'success))
    ('changes (propertize "✗ changes requested" 'face 'error))
    ('pending (propertize "⏳ pending" 'face 'shadow))
    (_ "")))

;;;; Form ---------------------------------------------------------------------

(defvar-keymap gp-reviewers-mode-map
  :parent widget-keymap
  "C-c C-c" #'gp-reviewers-save
  "C-c C-k" #'gp-reviewers-cancel
  "C"       #'gp-reviewers-save
  "q"       #'gp-reviewers-cancel
  "SPC"     #'gp-reviewers-toggle
  "TAB"     #'widget-forward
  "<backtab>" #'widget-backward)

(define-derived-mode gp-reviewers-mode fundamental-mode "PR-Reviewers"
  "Major mode for the reviewer-editing form.
\\<gp-reviewers-mode-map>`\\[gp-reviewers-save]' saves;
`\\[gp-reviewers-cancel]' cancels.  TAB / S-TAB move between rows,
SPC toggles the row at point."
  (setq-local cursor-type 'box))

(defun gp-reviewers--row-at-point ()
  "Return the candidate row whose checkbox is at point, or nil."
  (let ((w (get-char-property (point) 'button)))
    (and w (cl-find-if (lambda (r) (eq (plist-get r :widget) w))
                       gp-reviewers--widgets))))

(defun gp-reviewers-toggle ()
  "Toggle the checkbox at point, refusing to untick a locked reviewer."
  (interactive)
  (let ((row (gp-reviewers--row-at-point)))
    (cond
     ((and row (plist-get row :locked) (widget-value (plist-get row :widget)))
      (user-error "%s has already reviewed; that cannot be withdrawn here"
                  (plist-get row :name)))
     ((get-char-property (point) 'button) (widget-button-press (point)))
     (t (message "Move onto a reviewer (TAB) to toggle it")))))

(defun gp-reviewers--insert-group (label rows)
  "Insert one checkbox per entry in ROWS under the heading LABEL.
Returns the rows, each with its :widget filled in."
  (widget-insert (propertize label 'face 'gp-reviewers-section) "\n")
  (prog1
      (mapcar
       (lambda (r)
         (let ((cb (widget-create 'checkbox
                                  :notify
                                  ;; a locked row snaps straight back when
                                  ;; toggled by mouse or RET, which the
                                  ;; keymap's `user-error' cannot intercept
                                  (lambda (w &rest _)
                                    (when (and (plist-get r :locked)
                                               (not (widget-value w)))
                                      (widget-value-set w t)
                                      (message
                                       "%s has already reviewed; that cannot be withdrawn here"
                                       (plist-get r :name))))
                                  (plist-get r :on))))
           (widget-insert " " (plist-get r :name))
           (let ((badge (gp-reviewers--state-badge (plist-get r :state))))
             (unless (string-empty-p badge)
               (widget-insert "   " badge)))
           (when (plist-get r :locked)
             (widget-insert (propertize "  (locked)" 'face 'shadow)))
           (widget-insert "\n")
           (plist-put (copy-sequence r) :widget cb)))
       rows)
    (widget-insert "\n")))

(defun gp-reviewers--build-form (pr groups)
  "Render the reviewer form for PR from GROUPS into the current buffer."
  (remove-overlays)
  (widget-insert (propertize "Edit reviewers\n" 'face 'gp-reviewers-heading))
  (widget-insert (propertize (format "#%s  %s\n\n"
                                     (alist-get 'id pr)
                                     (or (alist-get 'title pr) ""))
                             'face 'shadow))
  (setq gp-reviewers--widgets
        (apply #'append
               (mapcar (pcase-lambda (`(,label . ,rows))
                         (gp-reviewers--insert-group label rows))
                       groups)))
  (unless gp-reviewers--widgets
    (widget-insert
     (propertize "  (no reviewer candidates for this repo)\n\n" 'face 'shadow)))
  (widget-create 'push-button
                 :notify (lambda (&rest _) (gp-reviewers-save))
                 "Save [C]")
  (widget-insert "   ")
  (widget-create 'push-button
                 :notify (lambda (&rest _) (gp-reviewers-cancel))
                 "Cancel [q]")
  (widget-insert "\n")
  (widget-setup))

;;;; Commands -----------------------------------------------------------------

(defun gp-reviewers--selected-ids ()
  "Return the ids of every reviewer that should end up on the PR.
A locked row (someone who already submitted a review) counts as
selected however its checkbox currently reads: the widget can be
unticked by paths that never reach `gp-reviewers-toggle' -- a mouse
click, RET, `widget-value-set' -- so the rule is enforced here, where
every save must pass, rather than only at the keystroke."
  (delq nil (mapcar (lambda (r)
                      (and (or (plist-get r :locked)
                               (widget-value (plist-get r :widget)))
                           (plist-get r :id)))
                    gp-reviewers--widgets)))

(defun gp-reviewers--current-ids ()
  "Return the ids the PR already had when the form opened."
  (delq nil (mapcar (lambda (r) (and (plist-get r :on) (plist-get r :id)))
                    gp-reviewers--widgets)))

(defun gp-reviewers-save ()
  "Send the ticked reviewers as the PR's complete reviewer list."
  (interactive)
  (let* ((pr gp-reviewers--pr)
         (wanted (gp-reviewers--selected-ids))
         (current (gp-reviewers--current-ids))
         (winconf gp-reviewers--return-window)
         (origin gp-reviewers--origin-buffer)
         (buf (current-buffer)))
    (unless pr (user-error "No PR associated with this form"))
    (if (equal (sort (copy-sequence wanted) #'string<)
               (sort (copy-sequence current) #'string<))
        (progn
          (when winconf (set-window-configuration winconf))
          (when (buffer-live-p buf) (kill-buffer buf))
          (message "Reviewers unchanged"))
      (message "Updating reviewers…")
      (gp-set-pull-request-reviewers (gp-pr-full-name pr) (alist-get 'id pr)
                                     wanted current)
      (gp-invalidate-pr-caches pr)
      (when winconf (set-window-configuration winconf))
      (when (buffer-live-p buf) (kill-buffer buf))
      (message "Reviewers updated (%d)" (length wanted))
      (when (buffer-live-p origin)
        (with-current-buffer origin
          (when (fboundp 'gp-detail-refresh) (gp-detail-refresh)))))))

(defun gp-reviewers-cancel ()
  "Discard the form without touching the PR."
  (interactive)
  (let ((winconf gp-reviewers--return-window)
        (buf (current-buffer)))
    (when winconf (set-window-configuration winconf))
    (when (buffer-live-p buf) (kill-buffer buf))
    (message "Reviewer edit cancelled")))

(defun gp-reviewers--open (pr current origin winconf)
  "Render the reviewer form for PR with CURRENT reviewers and show it.
ORIGIN is the buffer to refresh after a save; WINCONF the window
configuration to restore on close."
  (let* ((full-name (gp-pr-full-name pr))
         (groups (gp-reviewers--candidates
                  current
                  (gp-repo-default-reviewers full-name)
                  (gp-repo-suggested-reviewers full-name)))
         (buf (get-buffer-create (gp-reviewers--buffer-name pr))))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer))
      (gp-reviewers-mode)
      (setq gp-reviewers--pr pr
            gp-reviewers--return-window winconf
            gp-reviewers--origin-buffer origin)
      (gp-local-anchor-to-checkout pr)
      (gp-reviewers--build-form pr groups)
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (widget-forward 1)
    buf))

;;;###autoload
(defun gp-reviewers-edit (pr &optional current)
  "Open the reviewer-editing form for PR.
CURRENT is the PR's existing reviewer plists when the caller already
has them -- the detail buffer caches them in `gp--detail-reviewers'.
Without them the list is fetched first: `gp-pr-reviewers-async' is
genuinely asynchronous on GitHub, so the form is built in its
callback rather than from a synchronous read that would render an
empty \"Current\" group and then offer to save it."
  (let ((origin (current-buffer))
        (winconf (current-window-configuration)))
    (if current
        (gp-reviewers--open pr current origin winconf)
      (message "Loading reviewers…")
      (gp-pr-reviewers-async
       pr
       (lambda (reviewers)
         (gp-reviewers--open pr reviewers origin winconf))))))

(provide 'gp-reviewers)
;;; gp-reviewers.el ends here
