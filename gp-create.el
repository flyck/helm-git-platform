;;; gp-create.el --- Create a pull request from a branch -*- lexical-binding: t; -*-

;;; Commentary:

;; When you are in a git repo on a branch that has no open pull request
;; yet, `gp-create-pr' opens a Customize-style widget form pre-filled with
;;
;;   * a TITLE derived from the branch's commit messages (their common
;;     denominator -- a shared prefix, or the lone commit's summary);
;;   * a DESCRIPTION listing those commit summaries;
;;   * toggles for "create as draft" (`gp-create-draft', on by default)
;;     and "delete source branch after merge"; and
;;   * a checkbox per reviewer candidate: default reviewers (checked --
;;     the platform auto-adds these) and, where the backend has them,
;;     suggested reviewers (unchecked -- an opt-in).
;;
;; The destination defaults to the repo's main branch.  `C' (or the
;; "Create PR" button / C-c C-c) creates the PR -- pushing the branch to
;; origin first if it is not there yet, never pushing a protected branch;
;; `q' (or "Cancel" / C-c C-k) discards it and falls back to `gp-helm'.
;;
;; The pure pieces (title derivation, body building) are factored out so
;; the tests drive them directly; only `gp-create-pr' and the create
;; action touch git/network state.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'git-platform)
(require 'gp-checkout)
(require 'gp-local)

(declare-function gp-show-pr "gp-ui")
(declare-function gp-helm--list "gp-helm")

(defgroup gp-create nil
  "Creating pull requests from a branch."
  :group 'bitbucket)

(defcustom gp-create-buffer (gp--buffer-name "create PR")
  "Name of the PR-creation mask buffer.
Defaults to the shared `gp-buffer-name-prefix' tag."
  :type 'string :group 'gp-create)

;;;; Pure title / body derivation --------------------------------------------

(defun gp-create--common-prefix (strings)
  "Return the longest common whitespace-trimmed prefix of STRINGS, or nil.
The result is trimmed of trailing separators (space, colon, dash,
slash) so a shared scope like \"feat(api): \" yields \"feat(api)\"."
  (when strings
    (let ((prefix (car strings)))
      (dolist (s (cdr strings))
        (let ((i 0) (max (min (length prefix) (length s))))
          (while (and (< i max) (eq (aref prefix i) (aref s i)))
            (setq i (1+ i)))
          (setq prefix (substring prefix 0 i))))
      (let ((trimmed (string-trim
                      (replace-regexp-in-string "[ \t:/_-]+\\'" "" prefix))))
        (unless (string-empty-p trimmed) trimmed)))))

(defun gp-create--derive-title (summaries branch)
  "Derive a PR title from commit SUMMARIES, falling back to BRANCH.
One commit: its summary.  Several: their common denominator (a
shared prefix) if there is a meaningful one, else BRANCH humanised."
  (cond
   ((null summaries) (gp-create--humanise-branch branch))
   ((= (length summaries) 1) (string-trim (car summaries)))
   (t (or (let ((p (gp-create--common-prefix summaries)))
            (and p (>= (length p) 3) p))
          (gp-create--humanise-branch branch)))))

(defun gp-create--humanise-branch (branch)
  "Turn a BRANCH name into a rough title (drop a type/ prefix, dashes->spaces)."
  (if (or (null branch) (string-empty-p branch))
      "New pull request"
    (let* ((base (replace-regexp-in-string
                  "\\`\\(?:feature\\|feat\\|fix\\|bugfix\\|chore\\|hotfix\\)/"
                  "" branch))
           (words (string-trim (replace-regexp-in-string "[-_/]+" " " base))))
      (if (string-empty-p words)
          "New pull request"
        ;; sentence case: capitalise only the first letter
        (concat (upcase (substring words 0 1)) (substring words 1))))))

(defun gp-create--body (summaries)
  "Return a Markdown description listing commit SUMMARIES (newest first)."
  (if (null summaries)
      ""
    (mapconcat (lambda (s) (concat "- " (string-trim s))) summaries "\n")))

;;;; Buffer template & parsing -----------------------------------------------

(defcustom gp-create-draft t
  "Default for the \"Create as draft\" toggle in the mask.
Non-nil opens new pull requests as drafts unless you untick the box,
which keeps a PR out of reviewers' queues until you mark it ready."
  :type 'boolean :group 'gp-create)

(defcustom gp-create-close-source-branch t
  "Default for the \"Delete source branch after merge\" toggle in the mask."
  :type 'boolean :group 'gp-create)

(defcustom gp-create-preferred-reviewers nil
  "Reviewers to pre-check in the create form, identified by account id.

For repos whose platform cannot answer \"who reviews this by
default\" -- a Bitbucket repo with no configured default reviewers
returns an empty list, and GitHub has no queryable equivalent at all
\(CODEOWNERS is not one) -- so the people you always add ended up
being ticked by hand every time.

Prefer account ids: a Bitbucket uuid (braces included, e.g.
\"{4efc3327-362a-4f4f-b573-c4435b0f2232}\") or a GitHub login.  They
are stable, so a colleague renaming themselves cannot silently stop
pre-checking them.  A display name or nickname is also accepted,
matched case-insensitively, for readability at the cost of surviving
renames.

Entries that match no candidate are logged (see `gp-log-enabled')
rather than silently ignored, so a stale id surfaces instead of just
quietly not pre-checking someone.  Matching only ever pre-checks
people the platform already offers as candidates -- it never invents
a reviewer, so a stale entry cannot add the wrong person to a PR.

To find the ids, open a create form with `gp-log-enabled' non-nil and
run \\[gp-create-show-reviewer-ids].

Example:

    (setq gp-create-preferred-reviewers
          \\='(\"{fbdc7812-c312-45bc-9a3a-64819c0a962a}\"   ; Michael Gertz
            \"{4efc3327-362a-4f4f-b573-c4435b0f2232}\"   ; Florian Pracht
            \"{01a8f5d1-a3ed-49ed-b835-443190e33ea6}\")) ; Thomas Zahari"
  :type '(repeat string) :group 'gp-create)

;;;; Widget form --------------------------------------------------------------

(require 'wid-edit)

(declare-function widget-create "wid-edit")
(declare-function widget-insert "wid-edit")
(declare-function widget-value "wid-edit")
(declare-function widget-setup "wid-edit")
(declare-function widget-field-at "wid-edit")
(declare-function widget-forward "wid-edit")
(declare-function widget-backward "wid-edit")
(defvar widget-keymap)

(defvar-local gp-create--ctx nil
  "Plist describing this mask: (:full-name :dir :source :dest).")

(defvar-local gp-create--return-window nil
  "Window configuration to restore when the mask closes.")

(defvar-local gp-create--w-title nil "Title field widget.")
(defvar-local gp-create--w-desc nil "Description field widget.")
(defvar-local gp-create--w-draft nil "Draft checkbox widget.")
(defvar-local gp-create--w-close nil "Close-source-branch checkbox widget.")
(defvar-local gp-create--w-reviewers nil
  "Alist (UUID . CHECKBOX-WIDGET) for the reviewer toggles.")

(defface gp-create-heading '((t :inherit bold :height 1.2))
  "Face for the form's top heading." :group 'gp-create)
(defface gp-create-section '((t :inherit font-lock-keyword-face :weight bold))
  "Face for section labels in the form." :group 'gp-create)

(defvar-keymap gp-create-mode-map
  :parent widget-keymap
  "C-c C-c" #'gp-create-submit
  "C-c C-k" #'gp-create-cancel
  "C"       #'gp-create-submit-key
  "q"       #'gp-create-cancel-key
  "SPC"     #'gp-create-toggle-or-self-insert
  "TAB"     #'widget-forward
  "<backtab>" #'widget-backward
  ;; Keep static labels/headings inert: printable keys and deletion only
  ;; do anything inside the editable Title/Description fields.
  "<remap> <self-insert-command>" #'gp-create-self-insert
  "DEL"     #'gp-create-delete-backward
  "<backspace>" #'gp-create-delete-backward)

(defun gp-create-self-insert (n &optional c)
  "Self-insert only inside an editable field; elsewhere do nothing."
  (interactive "p")
  (if (gp-create--in-field-p)
      (self-insert-command n c)
    (message "Move into a field (TAB) to edit")))

(defun gp-create-delete-backward (n)
  "Delete backwards only inside an editable field; elsewhere do nothing."
  (interactive "p")
  (when (and (gp-create--in-field-p)
             (> (point) (or (ignore-errors (widget-field-start (widget-field-at (point))))
                            (point-min))))
    (delete-char (- n))))

(define-derived-mode gp-create-mode fundamental-mode "PR-Create"
  "Major mode for the pull-request creation form.
\\<gp-create-mode-map>`\\[gp-create-submit]' creates the PR;
`\\[gp-create-cancel]' cancels.  TAB / S-TAB move between fields."
  (setq-local cursor-type 'box))

(defun gp-create--in-field-p ()
  "Return non-nil if point is inside an editable widget text field.
Only text fields capture `C'/`q' as literal input; on checkboxes
and buttons those keys stay the global create/cancel actions
\(checkboxes toggle with RET or SPC)."
  (and (widget-field-at (point)) t))

;;;; Form construction --------------------------------------------------------

(defun gp-create--preferred-p (reviewer)
  "Return non-nil if REVIEWER is named in `gp-create-preferred-reviewers'."
  (let ((name (alist-get 'display_name reviewer))
        (nick (alist-get 'nickname reviewer))
        (uuid (alist-get 'uuid reviewer)))
    (seq-some (lambda (want)
                (or (and name (string-equal-ignore-case want name))
                    (and nick (string-equal-ignore-case want nick))
                    (equal want uuid)))
              gp-create-preferred-reviewers)))

(defun gp-create--log-unmatched-preferred (candidates)
  "Log any `gp-create-preferred-reviewers' entry CANDIDATES cannot satisfy.
A preferred name that matches nobody is almost always a rename or a
typo; failing loudly in the log beats quietly not pre-checking them."
  (dolist (want gp-create-preferred-reviewers)
    (unless (seq-some (lambda (r)
                        (let ((name (alist-get 'display_name r))
                              (nick (alist-get 'nickname r)))
                          (or (and name (string-equal-ignore-case want name))
                              (and nick (string-equal-ignore-case want nick))
                              (equal want (alist-get 'uuid r)))))
                      candidates)
      (gp-log 'info "preferred reviewer %S matched no candidate for this repo"
              want))))

(defun gp-create--insert-reviewer-list (reviewers checked)
  "Insert one CHECKBOX per entry in REVIEWERS, each defaulting to CHECKED.
A reviewer named in `gp-create-preferred-reviewers' is checked
regardless of CHECKED -- that is the whole point of listing them --
and gets a marker so it is clear why it came pre-ticked.
Returns an alist (UUID . CHECKBOX-WIDGET) in REVIEWERS order."
  (mapcar
   (lambda (r)
     (let* ((uuid (alist-get 'uuid r))
            (name (or (alist-get 'display_name r)
                      (alist-get 'nickname r) uuid))
            (preferred (gp-create--preferred-p r))
            (cb (widget-create 'checkbox (or checked preferred))))
       (widget-insert " " (or name "?"))
       (when preferred
         (widget-insert (propertize "  ★" 'face 'shadow)))
       (widget-insert "\n")
       (cons uuid cb)))
   reviewers))

;;;###autoload
(defun gp-create-show-reviewer-ids (full-name)
  "List FULL-NAME's reviewer candidates with their account ids.
A lookup helper for filling in `gp-create-preferred-reviewers': the
create form shows names, but the custom wants stable ids.  Reads the
same candidate pool the form does, so anyone listed here can be
pre-checked."
  (interactive
   (list (or (and (bound-and-true-p gp-create--ctx)
                  (plist-get gp-create--ctx :full-name))
             (and default-directory
                  (ignore-errors (gp-local--dir-remote default-directory)))
             (read-string "Repository (workspace/slug): "))))
  (let ((candidates (append (gp-repo-default-reviewers full-name)
                            (gp-repo-suggested-reviewers full-name))))
    (if (null candidates)
        (message "No reviewer candidates for %s" full-name)
      (with-current-buffer (get-buffer-create (gp--buffer-name "reviewer ids"))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Reviewer candidates for %s\n\n" full-name))
          (insert "Copy the ids you want into `gp-create-preferred-reviewers'.\n\n")
          (dolist (r candidates)
            (insert (format "%-28s %s\n"
                            (or (alist-get 'display_name r)
                                (alist-get 'nickname r) "?")
                            (or (alist-get 'uuid r) "?"))))
          (goto-char (point-min)))
        (special-mode)
        (display-buffer (current-buffer))))))

(defun gp-create--insert-reviewers (full-name)
  "Insert the reviewers section for FULL-NAME, returning the alist.
Default reviewers (the platform auto-adds them; Bitbucket) are
checked by default -- opting out is the unusual case.  Suggested
reviewers (GitHub's repo collaborators, standing in for a real
per-PR suggestion the create form can't ask for yet -- see
`gp-repo-suggested-reviewers') are unchecked -- picking one is an
explicit opt-in, except for anyone in
`gp-create-preferred-reviewers', who is pre-checked wherever they
appear.  Returns the combined alist (UUID . CHECKBOX-WIDGET)
across both groups.  A blank section if neither backend has anything
to offer for this repo."
  (let ((defaults (gp-repo-default-reviewers full-name))
        (suggested (gp-repo-suggested-reviewers full-name)))
    (gp-create--log-unmatched-preferred (append defaults suggested))
    (widget-insert (propertize "Reviewers" 'face 'gp-create-section) "\n")
    (if (and (null defaults) (null suggested))
        (progn
          (widget-insert
           (propertize "  (no default or suggested reviewers for this repo)\n"
                       'face 'shadow))
          nil)
      (append
       (gp-create--insert-reviewer-list defaults t)
       (when suggested
         (widget-insert (propertize "Suggested (unchecked)" 'face 'shadow) "\n")
         (gp-create--insert-reviewer-list suggested nil))))))

(defun gp-create--build-form (ctx)
  "Render the widget form for CTX into the current buffer."
  (let ((full-name (plist-get ctx :full-name))
        (source (plist-get ctx :source))
        (dest (plist-get ctx :dest)))
    (remove-overlays)
    (widget-insert (propertize "Create pull request\n" 'face 'gp-create-heading))
    (widget-insert (propertize (format "%s  →  %s\n" source dest) 'face 'shadow))
    (widget-insert (propertize (format "in %s\n\n" full-name) 'face 'shadow))

    (widget-insert (propertize "Title" 'face 'gp-create-section) "\n")
    (setq gp-create--w-title
          (widget-create 'editable-field
                         :size 60 :format "%v\n\n"
                         (or (plist-get ctx :title) "")))

    (widget-insert (propertize "Description" 'face 'gp-create-section) "\n")
    (setq gp-create--w-desc
          (widget-create 'text :format "%v\n"
                         (or (plist-get ctx :description) "")))
    (widget-insert "\n")

    (widget-insert (propertize "Options" 'face 'gp-create-section) "\n")
    (setq gp-create--w-draft (widget-create 'checkbox gp-create-draft))
    (widget-insert " Create as draft\n")
    (setq gp-create--w-close
          (widget-create 'checkbox gp-create-close-source-branch))
    (widget-insert " Delete source branch after merge\n\n")

    (setq gp-create--w-reviewers (gp-create--insert-reviewers full-name))
    (widget-insert "\n")

    ;; Shortcut lives in the button label (like the detail view's
    ;; "reply [R]" / "resolve [X]" buttons), not in a separate help line.
    (widget-create 'push-button
                   :notify (lambda (&rest _) (gp-create-submit))
                   "Create PR [C]")
    (widget-insert "   ")
    (widget-create 'push-button
                   :notify (lambda (&rest _) (gp-create-cancel))
                   "Cancel [q]")
    (widget-insert "\n")
    (widget-setup)))

;;;; Submit / cancel ----------------------------------------------------------

(defun gp-create--selected-reviewer-uuids ()
  "Return the uuids of reviewers whose checkbox is currently ticked."
  (delq nil
        (mapcar (lambda (cell)
                  (and (widget-value (cdr cell)) (car cell)))
                gp-create--w-reviewers)))

(defun gp-create-submit ()
  "Create the pull request described by the form.
Pushes SOURCE to origin first if it is not there yet (never a
protected branch).  Opens the new PR's detail buffer on success."
  (interactive)
  (let* ((ctx gp-create--ctx)
         (winconf gp-create--return-window)
         (title (string-trim (widget-value gp-create--w-title)))
         (desc (string-trim (widget-value gp-create--w-desc)))
         (draft (widget-value gp-create--w-draft))
         (close (widget-value gp-create--w-close))
         (reviewers (gp-create--selected-reviewer-uuids))
         (full-name (plist-get ctx :full-name))
         (dir (plist-get ctx :dir))
         (source (plist-get ctx :source))
         (dest (plist-get ctx :dest)))
    (when (string-empty-p title)
      (user-error "Title is empty"))
    ;; ensure the source branch is on the remote (push if needed)
    (unless (gp-checkout-branch-on-remote-p dir source)
      (message "Pushing %s to origin…" source)
      (let ((res (gp-checkout-push-branch dir source)))
        (unless (plist-get res :ok)
          (user-error "Push failed: %s" (plist-get res :log)))))
    (message "Creating pull request…")
    (let ((pr (gp-create-pull-request full-name source dest title desc
                                      draft close reviewers))
          (buf (or (get-buffer gp-create-buffer) (current-buffer))))
      (when winconf (set-window-configuration winconf))
      (when (buffer-live-p buf) (kill-buffer buf))
      (message "Created PR #%s: %s" (alist-get 'id pr) title)
      (require 'gp-ui)
      (when (fboundp 'gp-show-pr) (gp-show-pr pr))
      pr)))

(defun gp-create-toggle-or-self-insert ()
  "Toggle the checkbox/button at point, or self-insert SPC in a text field."
  (interactive)
  (cond
   ((gp-create--in-field-p) (self-insert-command 1 ?\s))
   ((get-char-property (point) 'button) (widget-button-press (point)))
   (t (self-insert-command 1 ?\s))))

(defun gp-create-submit-key ()
  "Create the PR when `C' is pressed outside a text field, else insert `C'."
  (interactive)
  (if (gp-create--in-field-p)
      (self-insert-command 1 ?C)
    (gp-create-submit)))

(defun gp-create-cancel-key ()
  "Cancel when `q' is pressed outside a text field, else insert `q'."
  (interactive)
  (if (gp-create--in-field-p)
      (self-insert-command 1 ?q)
    (gp-create-cancel)))

(defun gp-create-cancel ()
  "Discard the form and fall back to the normal PR list (`gp-helm').
Kills the form buffer outright (not merely buries it) so a later
open always starts from a clean slate."
  (interactive)
  ;; Grab the buffer/winconf before touching window state, then kill the
  ;; named form buffer explicitly -- works no matter which window or
  ;; `:notify' callback we were invoked from.
  (let ((winconf gp-create--return-window)
        (buf (or (get-buffer gp-create-buffer) (current-buffer))))
    (when winconf (set-window-configuration winconf))
    (when (buffer-live-p buf) (kill-buffer buf))
    (message "PR creation cancelled")
    (when (fboundp 'gp-helm--list)
      (require 'gp-helm)
      (gp-helm--list nil))))

;;;; Entry point --------------------------------------------------------------

(defun gp-create--context (dir full-name source dest)
  "Build the form context: derive title/description from commits.
Returns a plist (:title :description :source :dest :full-name :dir)."
  (let* ((summaries (gp-checkout-commit-summaries dir dest source))
         (title (gp-create--derive-title summaries source))
         (description (gp-create--body summaries)))
    (list :title title :description description
          :source source :dest dest :full-name full-name :dir dir)))

;;;###autoload
(defun gp-create-pr (dir full-name source &optional dest)
  "Open the PR-creation form for branch SOURCE in repo FULL-NAME (clone DIR).
DEST defaults to the repo's main branch.  The form is pre-filled
from the branch's commit messages, with toggles for draft, delete
source branch after merge, and the repo's default reviewers.
`C' / `C-c C-c' creates, `q' / `C-c C-k' cancels."
  (let* ((dest (or dest
                   (gp-repo-default-branch full-name)
                   "main"))
         (ctx (gp-create--context dir full-name source dest))
         (winconf (current-window-configuration))
         buf)
    (when (equal source dest)
      (user-error "Branch %s is the destination branch; nothing to open a PR from"
                  source))
    ;; Kill any stale form buffer rather than reusing it: a reused buffer can
    ;; carry over old widgets/overlays from a previous, never-closed session.
    (when-let* ((old (get-buffer gp-create-buffer)))
      (kill-buffer old))
    (setq buf (get-buffer-create gp-create-buffer))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (gp-create-mode)
        (setq gp-create--ctx (list :full-name full-name :dir dir
                                   :source source :dest dest)
              gp-create--return-window winconf
              header-line-format
              (format "Create PR  %s → %s" source dest))
        (gp-create--build-form ctx)
        ;; land on the title field for a quick edit
        (goto-char (point-min))
        (widget-forward 1)))
    (pop-to-buffer buf)
    buf))

(provide 'gp-create)
;;; gp-create.el ends here
