;;; helm-git-platform.el --- Browse Bitbucket/GitHub pull requests in Emacs -*- lexical-binding: t; -*-

;; Author: Felix Brilej
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (magit-section "3.0") (transient "0.3") (helm "3.0"))
;; Keywords: tools, vc
;; URL: https://github.com/flyck/helm-git-platform

;;; Commentary:

;; A modern, magit-flavoured client for code-review platforms (Bitbucket
;; Cloud today, with room for other forges behind a backend protocol).
;; List the pull requests across your whole workspace, split into "needs my
;; review" and "mine"; open a PR's comment thread; jump to the matching
;; local checkout and check out its branch; see inline review comments as
;; overlays on the code; and watch live counts in the mode line.
;;
;; This file is the umbrella: requiring `helm-git-platform' loads every
;; component and is the intended `use-package' entry point.
;;
;;   (use-package helm-git-platform
;;     :vc (:url "https://github.com/flyck/helm-git-platform" :rev :newest)
;;     :after (magit emojify)
;;     :commands (gp-helm gp-list gp-watch-mode)
;;     :bind ("C-c b" . gp-helm)
;;     :custom
;;     ;; identity & workspace -- otherwise read from the env vars named
;;     ;; by bitbucket-*-env (defaults BITBUCKET_WORKSPACE / _USER_EMAIL /
;;     ;; _API_TOKEN), or from auth-source for the token.
;;     (bitbucket-workspace "your-workspace")
;;     ;; where local clones live, and how to open one:
;;     (gp-local-git-root "~/git")
;;     (gp-open-function #'magit-status)
;;     ;; let the checkout service clone missing repos:
;;     (gp-checkout-clone-base "git@bitbucket.org:")
;;     :config
;;     (gp-watch-mode 1)            ; per-repo PR count + auto comment overlays
;;     (gp-magit-mode 1))           ; PR comments in magit diffs
;;
;; Nothing is hardcoded to a particular workspace or host: every value is
;; a defcustom with a Bitbucket-Cloud default.  Credentials fall back to
;; the environment then `auth-source'; see `bitbucket-api.el'.  Reading
;; the BITBUCKET_* exports out of a shell rc file is an optional macOS
;; convenience -- load `bitbucket-env' yourself if you want it (see README).

;;; Code:

(require 'gp-log)
(require 'bitbucket-api)
(require 'git-platform)
(require 'git-platform-bitbucket)
(require 'github-api)
(require 'git-platform-github)
(require 'gp-local)
(require 'gp-checkout)
(require 'gp-compose)
(require 'gp-create)
(require 'gp-reviewers)
(require 'gp-helm-terminal)
(require 'gp-pipeline)
(require 'gp-overlay)
(require 'gp-watch)
(require 'gp-ui)
;; magit-diff comment overlays are optional: only if magit is available.
(when (require 'magit nil t)
  (require 'gp-magit))
;; helm front-end is optional: only load it if helm is available.
(when (require 'helm nil t)
  (require 'gp-helm))

;; `bitbucket-env' (importing BITBUCKET_* from a shell rc file) is an
;; opt-in convenience and is deliberately NOT required here -- load it
;; yourself if you want it: (require 'bitbucket-env) (bitbucket-env-load).

;;;###autoload
(autoload 'gp-helm "gp-helm"
  "List pull requests with Helm." t)

;;;###autoload
(autoload 'gp-helm-open-prs "gp-helm"
  "List others' open pull requests across the workspace." t)

;;;###autoload
(autoload 'gp-helm-repo "gp-helm"
  "List the open pull requests in one repository." t)

;;;###autoload
(autoload 'gp-helm-repo-branch "gp-helm"
  "List the open pull requests in one repository on one branch." t)

;;;###autoload
(autoload 'gp-create-pr "gp-create"
  "Open the pull-request creation mask for a branch." t)

;;;###autoload
(autoload 'gp-list "gp-ui"
  "Open the pull-request list (magit-section)." t)

;;;###autoload
(autoload 'gp-watch-mode "gp-watch"
  "Toggle auto-overlay of PR comments on visited files." t)

;;;###autoload
(autoload 'gp-magit-mode "gp-magit"
  "Toggle PR comment overlays inside magit-diff buffers." t)

;;;###autoload
(autoload 'gp-overlay-toggle-globally "gp-overlay"
  "Globally turn inline PR comment overlays off or on." t)

;; Backward-compatible command names (the package was once Bitbucket-only).
(define-obsolete-function-alias 'bitbucket-helm #'gp-helm "gp 1.0")
(define-obsolete-function-alias 'bitbucket-list #'gp-list "gp 1.0")
(define-obsolete-function-alias 'bitbucket-watch-mode #'gp-watch-mode "gp 1.0")

(defun gp-overlay-current-pr ()
  "Overlay inline comments of the PR at point onto its local files."
  (interactive)
  (gp-overlay-pr (gp-current-pr)))

;; expose the overlay action from the list/detail keymaps
(with-eval-after-load 'gp-ui
  (keymap-set gp-list-mode-map "i" #'gp-overlay-current-pr)
  (keymap-set gp-detail-mode-map "i" #'gp-overlay-current-pr)
  (transient-append-suffix 'gp-dispatch '(0 -1)
    '("i" "Inline overlays" gp-overlay-current-pr)))

;; The package used to provide the feature `bitbucket'; keep that name as
;; an alias so existing `(require 'bitbucket)' and `(with-eval-after-load
;; 'bitbucket ...)' keep working.
(provide 'bitbucket)
(provide 'helm-git-platform)
;;; helm-git-platform.el ends here
