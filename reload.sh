#!/usr/bin/env bash
# Reload the helm-git-platform package into the *running* Emacs server, so
# changes can be tried without restarting Emacs.
#
#   ./reload.sh            # reload all *.el into the live session
#   ./reload.sh helm       # open M-x gp-helm in the live session
#   ./reload.sh watch      # toggle gp-watch-mode
#   ./reload.sh eval 'ELISP'   # eval arbitrary elisp in the live session
#
# Requires a running `M-x server-start` (or `(server-start)` in init) Emacs.
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"

ec() { emacsclient --eval "$1"; }

reload() {
  # Force-reload every source file (ignoring stale .elc) and re-require the umbrella.
  ec "(let ((load-path (cons \"$DIR\" load-path)))
        ;; defvar/defvar-keymap won't reassign already-bound vars, so a plain
        ;; reload keeps stale keymaps/defcustoms.  Unbind the package's vars
        ;; first so edits to keymaps and defaults actually take effect.
        (mapatoms
         (lambda (s)
           (when (and (boundp s)
                      (let ((n (symbol-name s)))
                        (or (string-prefix-p \"bitbucket-\" n)
                            (string-prefix-p \"gp-\" n)
                            (string-prefix-p \"git-platform\" n)))
                      (or (string-suffix-p \"-mode-map\" (symbol-name s))
                          (string-suffix-p \"-map\" (symbol-name s))))
             (makunbound s))))
        ;; load components first, the umbrella (helm-git-platform.el) last so
        ;; its with-eval-after-load bodies see freshly-defined keymaps
        (dolist (f (directory-files \"$DIR\" t \"\\\\.el\\\\'\"))
          (unless (string-suffix-p \"/helm-git-platform.el\" f)
            (load f nil t t)))
        (load (expand-file-name \"helm-git-platform.el\" \"$DIR\") nil t t)
        (require 'helm-git-platform)
        (when (fboundp 'bitbucket-env-load) (bitbucket-env-load))
        \"helm-git-platform reloaded\")"
}

case "${1:-reload}" in
  reload) reload ;;
  helm)   reload >/dev/null; ec "(progn (run-with-timer 0 nil #'gp-helm) \"launching gp-helm\")" ;;
  watch)  reload >/dev/null; ec "(progn (gp-watch-mode 'toggle) (format \"watch-mode: %s\" gp-watch-mode))" ;;
  eval)   shift; ec "$*" ;;
  *)      echo "usage: $0 [reload|helm|watch|eval ELISP]"; exit 1 ;;
esac
