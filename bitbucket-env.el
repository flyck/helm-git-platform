;;; bitbucket-env.el --- Import BITBUCKET_* env vars from a shell rc file -*- lexical-binding: t; -*-

;;; Commentary:

;; GUI Emacs on macOS does not source ~/.zshrc, so `export BITBUCKET_*' /
;; `export GITHUB_TOKEN' lines there never reach `getenv'.  Rather than pull
;; in exec-path-from-shell just for a handful of variables, this reads the rc
;; file directly and copies any matching `export NAME=VALUE' lines into the
;; Emacs environment.
;;
;; It is intentionally narrow: by default it only imports variables whose
;; names start with "BITBUCKET" or "GITHUB", and it parses the `export' lines
;; (it does NOT run the shell), so a slow or interactive ~/.zshrc cannot
;; hang Emacs and arbitrary rc code is never executed.
;;
;; Usage (e.g. in the use-package :init):
;;
;;   (require 'bitbucket-env)
;;   (bitbucket-env-load)            ;; reads ~/.zshrc by default
;;
;; If your variables live elsewhere, point `bitbucket-env-rc-file' at it,
;; or just set the variables directly with `setenv' / the `bitbucket-*'
;; customs and skip this entirely.

;;; Code:

(defgroup bitbucket-env nil
  "Importing Bitbucket credentials from a shell rc file."
  :group 'bitbucket)

(defcustom bitbucket-env-rc-file "~/.zshrc"
  "Shell rc file scanned for `export NAME=VALUE' lines."
  :type 'file
  :group 'bitbucket-env)

(defcustom bitbucket-env-prefix '("BITBUCKET" "GITHUB")
  "Prefixes of the environment variables to import.
Either one prefix string or a list of them.  Both platforms' variables
are imported by default: which backend is active is a separate setting
\(`git-platform-default-backend'), and importing the other one's token
costs nothing, so switching backends needs no change here.

Kept deliberately narrow -- an rc file holds far more than credentials,
and only names matching a prefix are ever copied into Emacs."
  :type '(choice (string :tag "One prefix")
                 (repeat :tag "Several prefixes" string))
  :group 'bitbucket-env)

(defun bitbucket-env--prefixes ()
  "Return `bitbucket-env-prefix' as a list of strings."
  (if (listp bitbucket-env-prefix) bitbucket-env-prefix
    (list bitbucket-env-prefix)))

(defcustom bitbucket-env-overwrite nil
  "When non-nil, imported values overwrite variables already set.
By default a variable that already has a value in the environment
is left untouched, so a real exported value wins over the rc file."
  :type 'boolean
  :group 'bitbucket-env)

(defun bitbucket-env--strip-quotes (value)
  "Remove a single pair of surrounding single or double quotes from VALUE."
  (if (and (>= (length value) 2)
           (memq (aref value 0) '(?\" ?\'))
           (eq (aref value 0) (aref value (1- (length value)))))
      (substring value 1 -1)
    value))

(defun bitbucket-env--parse (text)
  "Parse TEXT, returning an alist of (NAME . VALUE) from its export lines.
Only names beginning with one of `bitbucket-env-prefix' are returned.
Lines like `export NAME=VALUE' and bare `NAME=VALUE' are matched;
inline `# comments' after an unquoted value are dropped."
  (let* ((case-fold-search nil)
         (re (concat "^[ \t]*\\(?:export[ \t]+\\)?\\("
                     (regexp-opt (bitbucket-env--prefixes))
                     "[A-Za-z0-9_]*\\)=\\(.*\\)$"))
         (acc '()))
    (dolist (line (split-string text "\n"))
      (when (string-match re line)
        (let* ((name (match-string 1 line))
               (raw (string-trim (match-string 2 line)))
               ;; strip a trailing unquoted comment
               (raw (if (string-match-p "\\`[\"']" raw)
                        raw
                      (replace-regexp-in-string "[ \t]+#.*\\'" "" raw)))
               (value (bitbucket-env--strip-quotes raw)))
          (push (cons name value) acc))))
    (nreverse acc)))

;;;###autoload
(defun bitbucket-env-load (&optional rc-file)
  "Import `bitbucket-env-prefix'-matching vars from RC-FILE.
RC-FILE defaults to `bitbucket-env-rc-file'.
Returns the list of variable names that were set.  Existing
values are kept unless `bitbucket-env-overwrite' is non-nil.
Missing rc file is a no-op (returns nil)."
  (interactive)
  (let ((file (expand-file-name (or rc-file bitbucket-env-rc-file)))
        (set-names '()))
    (when (file-readable-p file)
      (let ((pairs (with-temp-buffer
                     (insert-file-contents file)
                     (bitbucket-env--parse (buffer-string)))))
        (dolist (kv pairs)
          (when (or bitbucket-env-overwrite
                    (null (getenv (car kv))))
            (setenv (car kv) (cdr kv))
            (push (car kv) set-names)))))
    (when (called-interactively-p 'interactive)
      (message "bitbucket-env: set %s"
               (if set-names (string-join (nreverse set-names) ", ") "nothing")))
    (nreverse set-names)))

(provide 'bitbucket-env)
;;; bitbucket-env.el ends here
