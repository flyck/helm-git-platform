;;; gp-local-test.el --- Tests for local repo linking -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests remote parsing and checkout resolution.  Filesystem and git are
;; faked: we build a temp ~/git tree and stub `git remote get-url'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gp-local)
;; Not just for the functions: the resolve-dir tests `let'-bind
;; `gp-checkout-clone-base', and a runtime-only `require' leaves the
;; compiler treating that as an unused lexical variable.
(require 'gp-checkout)
(require 'bitbucket-mock)
(require 'git-platform-bitbucket)
(require 'git-platform-github)

(ert-deftest gp-test-parse-remote-ssh ()
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (should (equal (gp-local--parse-remote
                    "git@bitbucket.org:acme/web-frontend.git")
                   "acme/web-frontend"))))

(ert-deftest gp-test-parse-remote-https ()
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (should (equal (gp-local--parse-remote
                    "https://ada@bitbucket.org/acme/api.git")
                   "acme/api"))
    ;; trailing slash, no .git
    (should (equal (gp-local--parse-remote
                    "https://bitbucket.org/acme/api/")
                   "acme/api"))))

(ert-deftest gp-test-parse-remote-bitbucket-url-while-github-active ()
  "A Bitbucket remote resolves to nil while the GitHub backend is
active -- no network call against the active backend could ever
reach it, so it should read as \"no repo here\", not a full-name."
  (let ((git-platform-current-backend (git-platform-github)))
    (should (null (gp-local--parse-remote
                   "git@bitbucket.org:acme/web-frontend.git")))))

(ert-deftest gp-test-parse-remote-github-ssh ()
  (let ((git-platform-current-backend (git-platform-github)))
    (should (equal (gp-local--parse-remote
                    "git@github.com:foo/bar.git")
                   "foo/bar"))))

(ert-deftest gp-test-parse-remote-github-https ()
  (let ((git-platform-current-backend (git-platform-github)))
    (should (equal (gp-local--parse-remote
                    "https://ada@github.com/foo/bar.git")
                   "foo/bar"))
    (should (equal (gp-local--parse-remote
                    "https://github.com/foo/bar/")
                   "foo/bar"))))

(ert-deftest gp-test-parse-remote-github-url-while-bitbucket-active ()
  "A GitHub remote resolves to nil while the Bitbucket backend is
active -- the mirror case of
`gp-test-parse-remote-bitbucket-url-while-github-active'."
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (should (null (gp-local--parse-remote
                   "git@github.com:foo/bar.git")))))

(ert-deftest gp-test-parse-remote-unrecognised-host ()
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (should (null (gp-local--parse-remote
                   "git@gitlab.com:foo/bar.git")))))

(defmacro gp-test-with-fake-tree (specs &rest body)
  "Run BODY with a temp git root populated per SPECS.
SPECS is a list of (DIRNAME . REMOTE-URL); each becomes a
subdirectory with a .git entry, and git is stubbed (at the
`gp-local--git-output' seam) so each dir is a work tree whose
`origin' resolves to the mapped REMOTE-URL.  A nil REMOTE-URL
makes that dir report no remotes (still a work tree)."
  (declare (indent 1) (debug t))
  `(let* ((root (make-temp-file "bb-git-" t))
          (gp-local-git-root root)
          (remotes (make-hash-table :test 'equal)))
     (gp-local-clear-cache)
     (dolist (spec ,specs)
       (let ((dir (expand-file-name (car spec) root)))
         (make-directory (expand-file-name ".git" dir) t)
         (puthash (directory-file-name dir) (cdr spec) remotes)))
     ;; Stub git itself: any dir under the fake tree is a work tree; its
     ;; sole remote `origin' maps to the spec URL (nil -> no remotes).
     (cl-letf (((symbol-function 'gp-local--git-output)
                (lambda (&rest args)
                  (let* ((key (directory-file-name
                               (directory-file-name
                                (expand-file-name default-directory))))
                         (url (gethash key remotes)))
                    (pcase args
                      (`("rev-parse" "--is-inside-work-tree") "true")
                      (`("remote" "get-url" "origin") url)
                      (`("remote") (and url "origin"))
                      (_ nil))))))
       (unwind-protect (progn ,@body)
         (delete-directory root t)))))

(ert-deftest gp-test-find-checkout-fast-path ()
  (gp-test-with-fake-tree
      '(("web-frontend" . "git@bitbucket.org:acme/web-frontend.git"))
    (should (string-suffix-p
             "web-frontend"
             (gp-local-find-checkout "acme/web-frontend")))))

(ert-deftest gp-test-find-checkout-renamed-folder ()
  "A folder named differently from the slug still resolves via its remote."
  (gp-test-with-fake-tree
      '(("my-frontend-clone" . "git@bitbucket.org:acme/web-frontend.git"))
    (should (string-suffix-p
             "my-frontend-clone"
             (gp-local-find-checkout "acme/web-frontend")))))

(ert-deftest gp-test-dir-remote-prefers-origin ()
  "`origin' wins when it is a Bitbucket remote."
  (gp-local-clear-cache)
  (cl-letf (((symbol-function 'gp-local--git-output)
             (lambda (&rest args)
               (pcase args
                 (`("rev-parse" "--is-inside-work-tree") "true")
                 (`("remote" "get-url" "origin")
                  "git@bitbucket.org:acme/web.git")
                 (`("remote") "origin\nupstream")
                 (_ nil)))))
    (should (equal (gp-local--dir-remote "/tmp/x") "acme/web"))))

(ert-deftest gp-test-dir-remote-works-in-worktree ()
  "A linked worktree (where .git is a FILE, not a dir) still resolves.
We never touch the filesystem -- git reports it is a work tree."
  (gp-local-clear-cache)
  (cl-letf (((symbol-function 'gp-local--git-output)
             (lambda (&rest args)
               (pcase args
                 (`("rev-parse" "--is-inside-work-tree") "true")
                 (`("remote" "get-url" "origin")
                  "git@bitbucket.org:acme/api.git")
                 (`("remote") "origin")
                 (_ nil)))))
    (should (equal (gp-local--dir-remote "/tmp/wt") "acme/api"))))

(ert-deftest gp-test-dir-remote-falls-back-to-other-remote ()
  "When `origin' is not a recognised forge, scan other remotes for one that is."
  (gp-local-clear-cache)
  (cl-letf (((symbol-function 'gp-local--git-output)
             (lambda (&rest args)
               (pcase args
                 (`("rev-parse" "--is-inside-work-tree") "true")
                 (`("remote" "get-url" "origin")
                  "git@gitlab.com:fork/api.git")  ; not Bitbucket or GitHub
                 (`("remote") "origin\nbitbucket")
                 (`("remote" "get-url" "bitbucket")
                  "git@bitbucket.org:acme/api.git")
                 (_ nil)))))
    (should (equal (gp-local--dir-remote "/tmp/y") "acme/api"))))

(ert-deftest gp-test-dir-remote-nil-outside-work-tree ()
  "Outside a work tree (git says so), resolution yields nil -- no crash."
  (gp-local-clear-cache)
  (cl-letf (((symbol-function 'gp-local--git-output)
             ;; Every git call fails here -- `rev-parse --is-inside-work-tree'
             ;; exits non-zero outside a work tree, and nothing past it should
             ;; be reached.  A `pcase' singling that one call out would compile
             ;; to a dead `equal' (Emacs 30 errors on the unused value), since
             ;; both branches answer nil anyway.
             (lambda (&rest _) nil)))
    (should (null (gp-local--dir-remote "/tmp/nope")))))

(ert-deftest gp-test-find-checkout-missing ()
  (gp-test-with-fake-tree
      '(("unrelated" . "git@bitbucket.org:acme/something-else.git"))
    (should (null (gp-local-find-checkout "acme/not-here")))))

(ert-deftest gp-test-anchor-to-checkout-sets-default-directory ()
  "Anchoring points `default-directory' at the PR's local checkout."
  (bitbucket-mock-with-service
    (let ((pr (car (alist-get 'values
                             (bitbucket-mock--fixture "workspace-prs.json")))))
      (gp-test-with-fake-tree
          '(("web-frontend" . "git@bitbucket.org:acme/web-frontend.git"))
        (with-temp-buffer
          (setq default-directory "/")
          (gp-local-anchor-to-checkout pr)
          (should (string-suffix-p "web-frontend/" default-directory)))))))

(ert-deftest gp-test-anchor-to-checkout-keeps-dir-when-no-clone ()
  "With no local clone the inherited `default-directory' is left alone.
An inherited directory beats a wrong one."
  (bitbucket-mock-with-service
    (let ((pr (car (alist-get 'values
                             (bitbucket-mock--fixture "workspace-prs.json")))))
      (gp-test-with-fake-tree
          '(("unrelated" . "git@bitbucket.org:acme/something-else.git"))
        (with-temp-buffer
          (setq default-directory "/somewhere/else/")
          (should (null (gp-local-anchor-to-checkout pr)))
          (should (equal default-directory "/somewhere/else/")))))))

(ert-deftest gp-test-anchor-to-checkout-never-clones ()
  "Anchoring resolves locally only -- painting a buffer must not hit the network."
  (bitbucket-mock-with-service
    (let ((pr (car (alist-get 'values
                             (bitbucket-mock--fixture "workspace-prs.json")))))
      (cl-letf (((symbol-function 'gp-local-find-checkout)
                 (lambda (_fn) nil))
                ((symbol-function 'gp-local-ensure-checkout)
                 (lambda (&rest _) (error "must not clone or switch branches")))
                ((symbol-function 'gp-checkout-ensure-clone)
                 (lambda (&rest _) (error "must not clone"))))
        (with-temp-buffer
          (should (null (gp-local-anchor-to-checkout pr))))))))

(ert-deftest gp-test-checkout-branch-delegates-to-service ()
  "checkout-branch resolves the dir and runs the checkout service there."
  (require 'gp-checkout)
  (bitbucket-mock-with-service
    (let* ((pr (car (alist-get 'values (bitbucket-mock--fixture "workspace-prs.json"))))
           (called-dir nil) (called-branch nil))
      (cl-letf (((symbol-function 'gp-local-find-checkout)
                 (lambda (_fn) "/tmp/fake"))
                ((symbol-function 'gp-checkout-run)
                 (lambda (dir branch &optional _base)
                   (setq called-dir dir called-branch branch)
                   (list :ok t :stashed nil :log ""))))
        (let ((res (gp-local-checkout-branch pr)))
          (should (equal called-dir "/tmp/fake"))
          (should (equal called-branch (gp-pr-source-branch pr)))
          (should (equal (plist-get res :dir) "/tmp/fake"))
          (should (plist-get res :ok)))))))

(ert-deftest gp-test-resolve-dir-clones-when-missing ()
  "resolve-dir clones (when enabled) if no checkout exists."
  (require 'gp-checkout)
  (let ((gp-checkout-clone-base "git@bitbucket.org:")
        (cloned nil))
    (cl-letf (((symbol-function 'gp-local-find-checkout) (lambda (_fn) nil))
              ((symbol-function 'gp-checkout-ensure-clone)
               (lambda (fn dest) (setq cloned (list fn dest)) dest)))
      (let ((dir (gp-local-resolve-dir "ws/slug" t)))
        (should (string-suffix-p "slug" dir))
        (should (equal (car cloned) "ws/slug"))))))

(ert-deftest gp-test-resolve-dir-errors-when-clone-disabled ()
  "Without a checkout and with cloning off, resolve-dir signals clearly."
  (require 'gp-checkout)
  (let ((gp-checkout-clone-base nil))
    (cl-letf (((symbol-function 'gp-local-find-checkout) (lambda (_fn) nil)))
      (should-error (gp-local-resolve-dir "ws/slug" nil) :type 'user-error))))

(ert-deftest gp-test-resolve-dir-uses-existing ()
  "When a checkout exists, resolve-dir returns it without cloning."
  (cl-letf (((symbol-function 'gp-local-find-checkout) (lambda (_fn) "/have/it"))
            ((symbol-function 'gp-checkout-ensure-clone)
             (lambda (&rest _) (error "should not clone"))))
    (should (equal (gp-local-resolve-dir "ws/slug" t) "/have/it"))))

(provide 'gp-local-test)
;;; gp-local-test.el ends here
