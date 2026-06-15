;;; git-platform.el --- Backend-agnostic PR protocol -*- lexical-binding: t; -*-

;;; Commentary:

;; A small abstraction over a code-review platform (Bitbucket today, GitHub
;; later) so the UI/overlay/helm layers talk to one protocol rather than to
;; Bitbucket directly.
;;
;; Consumers call the backend-free `gp-' functions (e.g. `gp-pull-request-
;; comments').  Each is a thin wrapper that injects the active backend and
;; dispatches to a `cl-defgeneric' method (`gp--...') implemented per
;; platform -- so callers never pass a backend around.  The active backend
;; is configured once (`git-platform-default-backend') and built lazily.
;;
;; Both network operations and field accessors are generic, because each
;; platform's JSON shape differs.  Pure, shape-free helpers (categorize,
;; partition) live here and go through the accessors.

;;; Code:

(require 'eieio)
(require 'cl-lib)

(defclass git-platform () ()
  :abstract t
  :documentation "Abstract base for a code-review platform backend.")

(defcustom git-platform-default-backend 'bitbucket
  "Which backend `git-platform-backend' builds by default."
  :type '(choice (const :tag "Bitbucket Cloud" bitbucket))
  :group 'bitbucket)

(defvar git-platform-current-backend nil
  "The active `git-platform' backend instance, or nil (built lazily).")

(declare-function git-platform-bitbucket "git-platform-bitbucket")

(defun git-platform-backend ()
  "Return the active backend, constructing the default lazily.
Set `git-platform-current-backend' (or `git-platform-default-backend')
once to choose the platform; callers then use the backend-free
`gp-' functions and never pass a backend explicitly."
  (or git-platform-current-backend
      (setq git-platform-current-backend
            (pcase git-platform-default-backend
              ('bitbucket (require 'git-platform-bitbucket)
                          (git-platform-bitbucket))
              (other (error "Unknown git-platform backend: %s" other))))))

;;;; Protocol definition helper ----------------------------------------------

;; Each operation is declared as a generic (`gp--NAME', dispatching on the
;; backend) plus a backend-free public wrapper (`gp-NAME') that injects the
;; active backend.  `gp-defop' generates both so the two never drift.

(defmacro gp-defop (name arglist &optional doc)
  "Define protocol operation NAME with ARGLIST (excluding the backend).
Creates the generic `gp--NAME' (backend is its first argument) and
the public `gp-NAME' wrapper that supplies `(git-platform-backend)'.
ARGLIST may contain &optional; &rest is not supported here."
  (let* ((generic (intern (format "gp--%s" name)))
         (public  (intern (format "gp-%s" name)))
         ;; strip &optional markers to build the call argument list
         (call-args (cl-remove '&optional arglist)))
    `(progn
       (cl-defgeneric ,generic (backend ,@arglist) ,(or doc ""))
       (defun ,public ,arglist
         ,(or doc "")
         (,generic (git-platform-backend) ,@call-args)))))

;;;; Network operations -------------------------------------------------------

(gp-defop user-uuid ()
  "Return the authenticated user's id.")
(gp-defop workspace-pull-requests (&optional uuid state max-items)
  "Return PRs authored by UUID.")
(gp-defop reviewing-pull-requests (&optional uuid limit states)
  "Return PRs where UUID is a reviewer (synchronous).")
(gp-defop reviewing-pull-requests-async (uuid states on-batch on-done &optional limit)
  "Scan reviewer PRs for UUID, calling ON-BATCH/ON-DONE.")
(gp-defop open-pull-requests-async (states on-batch on-done &optional limit)
  "Scan all open PRs, calling ON-BATCH/ON-DONE.")
(gp-defop pull-request (full-name id)
  "Return the full PR object for FULL-NAME/ID.")
(gp-defop pull-request-comments (full-name id &optional max-items)
  "Return comments for PR FULL-NAME/ID.")
(gp-defop pull-request-diff (full-name id)
  "Return the unified diff text for PR FULL-NAME/ID.")
(gp-defop pull-request-stats (full-name id &optional pr)
  "Return a stats plist for PR FULL-NAME/ID.")
(gp-defop create-comment (full-name id text &optional inline parent-id)
  "Create a comment on PR FULL-NAME/ID.")
(gp-defop resolve-comment (full-name id comment-id)
  "Resolve COMMENT-ID on PR FULL-NAME/ID.")
(gp-defop reopen-comment (full-name id comment-id)
  "Reopen COMMENT-ID on PR FULL-NAME/ID.")
(gp-defop edit-comment (full-name id comment-id text)
  "Replace COMMENT-ID's body with TEXT on PR FULL-NAME/ID.")
(gp-defop delete-comment (full-name id comment-id)
  "Delete COMMENT-ID on PR FULL-NAME/ID.")
(gp-defop set-pull-request-draft (full-name id draft &optional title)
  "Set PR FULL-NAME/ID draft flag to DRAFT.")
(gp-defop open-pr-for-branch (full-name branch)
  "Return the open PR in FULL-NAME whose source branch is BRANCH.")
(gp-defop repo-open-pr-count (full-name)
  "Return the open-PR count for repo FULL-NAME.")
(gp-defop repo-pull-requests (full-name &optional state)
  "Return the open PRs in repo FULL-NAME.")
(gp-defop commit-build-states (full-name hash)
  "Return the build state strings for commit HASH in FULL-NAME.")

;;;; Field accessors (JSON shape differs per platform) ------------------------

(gp-defop pr-full-name (pr)
  "Return the repository \"owner/slug\" for PR.")
(gp-defop pr-source-branch (pr)
  "Return PR's source branch name.")
(gp-defop pr-destination-branch (pr)
  "Return PR's destination (base) branch name.")
(gp-defop pr-draft-p (pr)
  "Return non-nil if PR is a draft.")
(gp-defop pr-authored-by-p (pr uuid)
  "Return non-nil if PR was authored by UUID.")
(gp-defop pr-review-tally (pr)
  "Return a plist (:approved :changes :pending) over PR's reviewers.")
(gp-defop comment-resolved-p (comment)
  "Return non-nil if COMMENT is resolved.")
(gp-defop comment-own-p (comment uuid)
  "Return non-nil if COMMENT was written by UUID.")

;;;; Platform-agnostic helpers ------------------------------------------------

(defun gp-partition-pull-requests (prs uuid)
  "Split PRS into a cons (MINE . REVIEWING) by UUID authorship."
  (let (mine reviewing)
    (dolist (pr prs)
      (if (gp-pr-authored-by-p pr uuid)
          (push pr mine)
        (push pr reviewing)))
    (cons (nreverse mine) (nreverse reviewing))))

(defun gp-categorize-pull-requests (prs uuid)
  "Categorise PRS for UUID into a plist (:reviewing :mine :drafts).
A draft authored by the user goes to :drafts; non-draft authored
PRs to :mine; everything else to :reviewing."
  (let (mine reviewing drafts)
    (dolist (pr prs)
      (cond
       ((and (gp-pr-authored-by-p pr uuid) (gp-pr-draft-p pr))
        (push pr drafts))
       ((gp-pr-authored-by-p pr uuid)
        (push pr mine))
       (t (push pr reviewing))))
    (list :reviewing (nreverse reviewing)
          :mine (nreverse mine)
          :drafts (nreverse drafts))))

(defun gp-build-states-summary (states)
  "Reduce build STATES to one symbol: `failed', `running', `stopped',
`successful', or nil (no builds).  Failure dominates, then running."
  (cond ((null states) nil)
        ((member "FAILED" states) 'failed)
        ((member "INPROGRESS" states) 'running)
        ((member "STOPPED" states) 'stopped)
        ((seq-every-p (lambda (s) (equal s "SUCCESSFUL")) states) 'successful)
        (t 'successful)))

(defun gp-split-diff-by-file (diff)
  "Split unified DIFF text into an alist of (PATH . CHUNK).
PATH is the new-side path (\"+++ b/PATH\"), falling back to the
old side for deletions.  CHUNK is that file's full diff text."
  (when diff
    (let ((case-fold-search nil) starts result)
      (with-temp-buffer
        (insert diff)
        (goto-char (point-min))
        (while (re-search-forward "^diff --git " nil t)
          (push (match-beginning 0) starts))
        (setq starts (nreverse starts))
        (cl-loop for (beg . rest) on starts
                 for end = (or (car rest) (point-max))
                 do (let* ((full (buffer-substring-no-properties beg end))
                           (path (cond
                                  ((string-match "^\\+\\+\\+ b/\\(.+\\)$" full)
                                   (match-string 1 full))
                                  ((string-match "^--- a/\\(.+\\)$" full)
                                   (match-string 1 full)))))
                      (when path
                        (push (cons (string-trim-right path) full) result)))))
      (nreverse result))))

;;;; Markdown / emoji rendering helpers ---------------------------------------

(defcustom gp-resolve-emoji-shortcodes t
  "When non-nil, turn :shortcode: tokens in comments into emoji.
Uses the `emojify' package's database when available; otherwise a
small built-in fallback covers the common ones."
  :type 'boolean :group 'bitbucket)

(defconst gp--emoji-fallback
  '((":thinking:" . "🤔") (":smile:" . "😄") (":+1:" . "👍") (":-1:" . "👎")
    (":tada:" . "🎉") (":rocket:" . "🚀") (":fire:" . "🔥") (":eyes:" . "👀")
    (":warning:" . "⚠️") (":bug:" . "🐛") (":sparkles:" . "✨")
    (":heavy_check_mark:" . "✔️") (":x:" . "❌") (":wave:" . "👋")
    (":pray:" . "🙏") (":raised_hands:" . "🙌") (":100:" . "💯")
    (":heart:" . "❤️") (":laughing:" . "😆") (":thumbsup:" . "👍")
    (":thumbsdown:" . "👎") (":ok_hand:" . "👌") (":clap:" . "👏"))
  "Fallback shortcode->emoji map used when `emojify' is unavailable.")

(declare-function emojify-get-emoji "emojify")
(declare-function emojify-create-emojify-emojis "emojify")
(declare-function ht-get "ht")

(defun gp--emoji-for (shortcode)
  "Return the unicode emoji for SHORTCODE (e.g. \":thinking:\"), or nil."
  (or (when (require 'emojify nil t)
        (ignore-errors
          (emojify-create-emojify-emojis)
          (let ((e (emojify-get-emoji shortcode)))
            (and e (ht-get e "unicode")))))
      (cdr (assoc shortcode gp--emoji-fallback))))

(defun gp-resolve-emojis (text)
  "Replace :shortcode: tokens in TEXT with their emoji, when enabled.
Tokens inside inline/fenced code are left untouched."
  (if (or (not gp-resolve-emoji-shortcodes) (null text))
      (or text "")
    (let ((case-fold-search t))
      (replace-regexp-in-string
       "\\(`[^`]*`\\)\\|:\\([a-z0-9_+-]+\\):"
       (lambda (m)
         (if (match-string 1 m)
             m
           (or (gp--emoji-for (downcase m)) m)))
       text t t))))

(defun gp-linkify-string (text)
  "Return TEXT with markdown [label](url) and bare URLs turned into links.
\[label](url) is shown as LABEL; both forms get the `link' face and
a keymap opening the URL on RET/mouse-1.  Pure -- returns a fresh string."
  (when text
    (let* ((open (lambda (url)
                   (let ((m (make-sparse-keymap)))
                     (define-key m [mouse-1] (lambda () (interactive) (browse-url url)))
                     (define-key m (kbd "RET") (lambda () (interactive) (browse-url url)))
                     m)))
           (link-props (lambda (url)
                         (list 'face 'link 'mouse-face 'highlight
                               'help-echo url 'follow-link t
                               'keymap (funcall open url))))
           (s (replace-regexp-in-string
               "\\[\\([^]]+\\)\\](\\(https?://[^)]+\\))"
               (lambda (m)
                 (apply #'propertize (match-string 1 m)
                        (funcall link-props (match-string 2 m))))
               text t t)))
      (let ((i 0))
        (while (string-match "\\(https?://[^ \t\n)]+\\)" s i)
          (let ((b (match-beginning 1)) (e (match-end 1)))
            (if (eq (get-text-property b 'face s) 'link)
                (setq i e)
              (add-text-properties b e (funcall link-props (match-string 1 s)) s)
              (setq i e)))))
      s)))

(provide 'git-platform)
;;; git-platform.el ends here
