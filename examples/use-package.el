;;; use-package.el --- Example helm-git-platform configuration -*- lexical-binding: t; -*-

;; Copy the form below into your init and adjust the customs.  Nothing here is
;; required verbatim: the package works with just the install recipe and the
;; BITBUCKET_* environment variables set.

(use-package helm-git-platform
  ;; Install straight from GitHub.  On Emacs 30+ use the built-in :vc:
  :vc (:url "https://github.com/flyck/helm-git-platform" :rev :newest)
  ;; On Emacs 29 or earlier, use straight.el instead:
  ;; :straight (helm-git-platform :host github :repo "flyck/helm-git-platform")
  ;; Or, if you cloned it by hand:
  ;; :load-path "~/.emacs.d/lisp/helm-git-platform"

  ;; magit gives the diff / "open in IDE"; emojify renders :emoji: shortcodes.
  ;; markdown-mode and helm are used by the UI when available.
  :after (magit emojify)

  ;; Autoloaded entry points -- so the package loads lazily on first use.
  :commands (gp-helm gp-list gp-watch-mode gp-magit-mode)
  :bind ("C-c b" . gp-helm)

  :custom
  ;; --- identity & workspace -------------------------------------------------
  ;; Usually left unset: they default to the env vars named by bitbucket-*-env
  ;; (BITBUCKET_WORKSPACE / _USER_EMAIL / _API_TOKEN), with auth-source as a
  ;; token fallback.  Uncomment to hardcode instead:
  ;; (bitbucket-workspace "your-workspace")
  ;; (bitbucket-user-email "you@example.com")

  ;; --- local checkouts ------------------------------------------------------
  (gp-local-git-root "~/git")               ; where your clones live
  (gp-open-function #'magit-status)         ; how "open the checkout" opens it
  (gp-checkout-clone-base "git@bitbucket.org:") ; clone missing repos from here
  (gp-checkout-remote "origin")             ; the PR's remote (fetch/checkout/diff base)

  ;; --- polling --------------------------------------------------------------
  (gp-watch-pr-cache-ttl 300)               ; seconds to cache PR/comment lookups

  :init
  ;; OPTIONAL (macOS GUI Emacs only): import the BITBUCKET_* exports out of
  ;; ~/.zshrc, since a GUI Emacs launched from Finder/dock does not source it.
  ;; This parses the file -- it never runs the shell -- and only imports the
  ;; BITBUCKET prefix.  Skip this entirely if your env is already populated.
  (when (eq system-type 'darwin)
    (require 'bitbucket-env)
    (bitbucket-env-load))                   ; reads ~/.zshrc by default

  :config
  ;; OPTIONAL features -- all default-friendly, comment out what you don't want:
  (gp-watch-mode 1)        ; global: per-repo PR count + auto comment overlays
  (gp-magit-mode 1)        ; PR comments drawn inside magit-diff buffers
  ;; Inline overlays are on by default; disable globally with:
  ;; (setq gp-overlay-enabled nil)
  )

;;; use-package.el ends here
