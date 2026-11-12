;;; gp-e2e-list-test.el --- End-to-end flows outside the PR detail buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; Drives the user-facing entry points that live outside the detail
;; buffer -- the `gp-list' buffer and its keymap, the `gp-helm' sources
;; and actions, the reviewer form, the PR-creation form, branch checkout
;; and `gp-watch-mode' -- all the way down to the mocked network, and
;; asserts on what reached the mock (`bitbucket-mock-calls' /
;; `github-mock-calls') and on the resulting buffers.
;;
;; Only the network (the mock services), user prompts, `browse-url',
;; timers and the `helm' session itself (a minibuffer prompt: its
;; sources are captured and their candidates/actions run for real) are
;; replaced.  Checkout and creation run real git against a temp clone
;; whose fetch remote is a local bare repository.  No gp-*/bitbucket-*/
;; github-* function is stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'wid-edit)
(require 'magit-section)
(require 'helm)
(require 'gp-test-helper)
(require 'git-platform)
(require 'git-platform-bitbucket)
(require 'git-platform-github)
(require 'bitbucket-api)
(require 'github-api)
(require 'gp-local)
(require 'gp-checkout)
(require 'gp-create)
(require 'gp-reviewers)
(require 'gp-overlay)
(require 'gp-watch)
(require 'gp-ui)
(require 'gp-helm)
(require 'bitbucket-mock)
(require 'github-mock)

;;;; Harness -------------------------------------------------------------------




(defvar gp-e2e-list--browsed nil "URLs handed to `browse-url', newest first.")
(defvar gp-e2e-list--opened nil "Directories handed to `gp-open-function'.")
(defvar gp-e2e-list--helm nil "Plist of the last `helm' call's arguments.")



(defun gp-e2e-list--no-network (&rest args)
  "Trip-wire: signal for any real HTTP request (ARGS name it)."
  (error "Test reached the network: %S" (car args)))

(defmacro gp-e2e-list--with-env (&rest body)
  "Run BODY isolated: queued timers, captured helm/browse/open, clean caches.
Binds a fresh empty `gp-local-git-root'.  Kills every buffer BODY
created and deletes the temp root."
  (declare (indent 0) (debug t))
  `(let (         (gp-e2e-list--browsed nil)
         (gp-e2e-list--opened nil)
         (gp-e2e-list--helm nil)
         (gp-local-git-root (make-temp-file "gp-e2e-list-root" t))
         (gp-open-function (lambda (dir) (push dir gp-e2e-list--opened)))
         (gp-detail-show-avatars nil)
         (gp-detail-help-at-point nil)
         (gp-helm--last-visited-pr-id nil)
         (gp-helm--mine-cache nil) (gp-helm--mine-uuid nil)
         (gp-helm--reviewing-cache nil) (gp-helm--reviewing-uuid nil)
         (gp-helm--merged-recent-cache nil)
         (gp-helm--pipeline-cache (make-hash-table :test 'equal))
         (gp-helm--deploy-cache (make-hash-table :test 'equal))
         (gp-helm--live-deploy-cache (make-hash-table :test 'equal))
         (gp-helm--review-tally-cache (make-hash-table :test 'eql))
         (gp-helm--reviewers-cache (make-hash-table :test 'eql))
         (gp-helm--comment-resolution-cache (make-hash-table :test 'eql))
         (bitbucket--repo-list-cache nil)
         (gp-local--remote-cache (make-hash-table :test 'equal))
         (gp-watch--repo-cache (make-hash-table :test 'equal))
         (gp-watch--pr-cache (make-hash-table :test 'equal))
         (gp-watch--count-cache (make-hash-table :test 'equal))
         (gp--recent-pipeline-steps-cache (make-hash-table :test 'equal))
         ;; a detail load records comments outdated against its diff here,
         ;; keyed by comment id only; the mock reuses ids across tests
         (gp--comment-outdated-cache (make-hash-table :test 'eql))
         (gp-checkout-remote "upstream")
         (process-environment
          (append '("GIT_AUTHOR_NAME=Test" "GIT_AUTHOR_EMAIL=t@example.com"
                    "GIT_COMMITTER_NAME=Test" "GIT_COMMITTER_EMAIL=t@example.com"
                    "GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1")
                  process-environment))
         (before (buffer-list)))
     (unwind-protect
         (gp-test-with-queued-timers
           (cl-letf (((symbol-function 'browse-url)
                      (lambda (url &rest _) (push url gp-e2e-list--browsed)))
                     ((symbol-function 'helm)
                      (lambda (&rest args) (setq gp-e2e-list--helm args) nil))
                     ((symbol-function 'url-retrieve) #'gp-e2e-list--no-network)
                     ((symbol-function 'url-retrieve-synchronously) #'gp-e2e-list--no-network))
             (save-window-excursion ,@body)))
       (when gp-watch-mode (gp-watch-mode -1))
       (dolist (b (buffer-list))
         (unless (memq b before)
           (with-current-buffer b (set-buffer-modified-p nil))
           (let ((kill-buffer-query-functions nil)) (kill-buffer b))))
       (delete-directory gp-local-git-root t))))

(defun gp-e2e-list--key (keys)
  "Press KEYS (a `kbd' string) in the current buffer, then drain timers."
  (switch-to-buffer (current-buffer))
  (execute-kbd-macro (kbd keys))
  (gp-test-drain-timers))

(defun gp-e2e-list--goto (text)
  "Move point to the start of the first occurrence of TEXT."
  (goto-char (point-min))
  (unless (search-forward text nil t)
    (error "Text %S not in buffer:\n%s" text (buffer-string)))
  (goto-char (match-beginning 0)))

(defun gp-e2e-list--text (&optional buffer)
  "BUFFER's (default current) text without properties."
  (with-current-buffer (or buffer (current-buffer))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun gp-e2e-list--calls (log method &optional regexp)
  "Calls in LOG with METHOD whose path matches REGEXP, oldest first."
  (reverse (seq-filter (lambda (c) (and (equal (car c) method)
                                        (or (null regexp)
                                            (string-match-p regexp (nth 1 c)))))
                       log)))

(defun gp-e2e-list--bb (method &optional regexp)
  "Bitbucket mock calls with METHOD matching REGEXP, oldest first."
  (gp-e2e-list--calls bitbucket-mock-calls method regexp))

(defun gp-e2e-list--gh (method &optional regexp)
  "GitHub mock calls with METHOD matching REGEXP, oldest first."
  (gp-e2e-list--calls github-mock-calls method regexp))

(defun gp-e2e-list--buffer-named (regexp)
  "The live buffer whose name matches REGEXP, or nil."
  (seq-find (lambda (b) (string-match-p regexp (buffer-name b))) (buffer-list)))

;;;; Fixture shapes --------------------------------------------------------------

(defconst gp-e2e-list--me "{21d7839d-779f-44b2-8c40-6f43ac90be06}"
  "user.json's uuid: the authenticated Bitbucket user.")

(defun gp-e2e-list--bb-prs ()
  "The fixture's workspace PR list."
  (alist-get 'values (bitbucket-mock--fixture "workspace-prs.json")))

(defconst gp-e2e-list--bb-foreign-pr
  '((id . 500) (title . "Tidy the logging config") (state . "OPEN")
    (comment_count . 3)
    (author (display_name . "Grace Hopper")
            (uuid . "{6b0d3a6e-2f4c-4e8e-9a51-3c1f0e7d2b10}"))
    (participants ((role . "REVIEWER") (state . nil) (approved . :json-false)
                   (user (uuid . "{21d7839d-779f-44b2-8c40-6f43ac90be06}")
                         (display_name . "Ada Lovelace"))))
    (destination (branch (name . "main"))
                 (repository (full_name . "acme/web-frontend") (slug . "web-frontend")))
    (source (branch (name . "chore/logging")) (commit (hash . "feedface0500")))
    (links (html (href . "https://bitbucket.org/acme/web-frontend/pull-requests/500"))))
  "A Bitbucket PR by someone else that the user reviews.")

(defun gp-e2e-list--bb-workspace-with-foreign ()
  "Override answering the workspace list with the fixture plus a foreign PR."
  `("/workspaces/[^/]+/pullrequests/"
    . ((values . ,(append (gp-e2e-list--bb-prs)
                          (list gp-e2e-list--bb-foreign-pr))))))

(defun gp-e2e-list--gh-two-prs-overrides ()
  "GitHub overrides: the search finds #42 (ada's) and #43 (bea's)."
  `(("\\`/search/issues\\'"
     . (((number . 42) (repository_url . "https://api.github.com/repos/acme/web"))
        ((number . 43) (repository_url . "https://api.github.com/repos/acme/web"))))
    ("/pulls/43\\'" . ,github-mock--pr-2)))

;;;; gp-list buffer --------------------------------------------------------------

(defun gp-e2e-list--open-list ()
  "Run `gp-list' interactively, drain, and make the list buffer current."
  (let ((buf (call-interactively #'gp-list)))
    (gp-test-drain-timers)
    (set-buffer buf)
    buf))

(defun gp-e2e-list--group-of (title)
  "The heading of the group the line holding TITLE sits in."
  (save-excursion
    (gp-e2e-list--goto title)
    (re-search-backward "^\\(Needs my review\\|My pull requests\\) (")
    (match-string-no-properties 1)))

(ert-deftest gp-e2e-list-groups-prs-by-authorship-bitbucket ()
  "`gp-list' fetches the workspace PRs for the user and files a PR by
someone else under \"Needs my review\" and the user's own under \"My
pull requests\", with each row's repo, author and source → target."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let ((bitbucket-mock-overrides (list (gp-e2e-list--bb-workspace-with-foreign))))
        (gp-e2e-list--open-list)
        (should (equal (mapcar #'cadr (gp-e2e-list--bb "GET" "/workspaces/"))
                       (list (format "/workspaces/acme/pullrequests/%s"
                                     gp-e2e-list--me))))
        (let ((text (gp-e2e-list--text)))
          (should (string-match-p "^Needs my review (1)$" text))
          (should (string-match-p "^My pull requests (10)$" text))
          (should (string-match-p
                   "#500 Tidy the logging config  \\[web-frontend\\] Grace Hopper  💬3"
                   text))
          (should (string-match-p "chore/logging → main" text)))
        (should (equal (gp-e2e-list--group-of "#500 ") "Needs my review"))
        (should (equal (gp-e2e-list--group-of "#180 ") "My pull requests"))))))

(ert-deftest gp-e2e-list-groups-prs-by-authorship-github ()
  "On GitHub the list comes from an author search plus one PR fetch per
hit; bea's draft #43 lands under review, ada's #42 under her own."
  (github-mock-with-service
    (gp-e2e-list--with-env
      (let ((github-mock-overrides (gp-e2e-list--gh-two-prs-overrides)))
        (gp-e2e-list--open-list)
        (should (equal (cdr (assoc "q" (nth 2 (car (gp-e2e-list--gh "GET" "/search/issues")))))
                       "is:pr author:ada is:open"))
        (should (equal (mapcar #'cadr (gp-e2e-list--gh "GET" "/pulls/[0-9]+\\'"))
                       '("/repos/acme/web/pulls/42" "/repos/acme/web/pulls/43")))
        (let ((text (gp-e2e-list--text)))
          (should (string-match-p "^Needs my review (1)$" text))
          (should (string-match-p "^My pull requests (1)$" text))
          (should (string-match-p "#42 Add the widget toggle   bug   ui   \\[web\\] ada" text))
          (should (string-match-p "feature/widget → main" text)))
        (should (equal (gp-e2e-list--group-of "#43 ") "Needs my review"))
        (should (equal (gp-e2e-list--group-of "#42 ") "My pull requests"))))))

(ert-deftest gp-e2e-list-g-refetches-and-redraws ()
  "`g' asks the server again and redraws from the new answer."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let ((bitbucket-mock-overrides nil))
        (gp-e2e-list--open-list)
        (should (string-match-p "^Needs my review (0)$" (gp-e2e-list--text)))
        (setq bitbucket-mock-overrides (list (gp-e2e-list--bb-workspace-with-foreign)))
        (gp-e2e-list--key "g")
        (should (= (length (gp-e2e-list--bb "GET" "/workspaces/")) 2))
        (should (string-match-p "^Needs my review (1)$" (gp-e2e-list--text)))
        (should (string-match-p "#500 Tidy the logging config" (gp-e2e-list--text)))))))

(ert-deftest gp-e2e-list-failed-refresh-keeps-the-list ()
  "A failing refresh leaves the rows on screen and logs the failure,
rather than blanking the buffer."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let ((bitbucket-mock-overrides nil))
        (gp-e2e-list--open-list)
        (let ((shown (gp-e2e-list--text)))
          (should (string-match-p "#180 " shown))
          (setq bitbucket-mock-overrides
                '(("/workspaces/" . (lambda (&rest _) (error "HTTP 503")))))
          (gp-e2e-list--key "g")
          (should (= (length (gp-e2e-list--bb "GET" "/workspaces/")) 2))
          (should (equal (gp-e2e-list--text) shown)))))))

(ert-deftest gp-e2e-list-ret-opens-the-pr-bitbucket ()
  "RET on a row opens that PR's detail buffer, which loads the PR and its
comments from the server."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let ((bitbucket-mock-overrides
             (list (gp-e2e-list--bb-workspace-with-foreign)
                   (cons "/pullrequests/500\\'" gp-e2e-list--bb-foreign-pr))))
        (gp-e2e-list--open-list)
        (gp-e2e-list--goto "#500 ")
        (gp-e2e-list--key "RET")
        (let ((detail (current-buffer)))
          (should (eq major-mode 'gp-detail-mode))
          (should (string-match-p "PR #500 Tidy the logging config (web-frontend)"
                                  (buffer-name detail)))
          (should (equal (alist-get 'id gp--pr) 500))
          (should (gp-e2e-list--bb "GET" "/repositories/acme/web-frontend/pullrequests/500\\'"))
          (should (gp-e2e-list--bb "GET" "/pullrequests/500/comments")))))))

(ert-deftest gp-e2e-list-ret-opens-the-pr-github ()
  "Same on GitHub: RET on #43 fetches /pulls/43 and both comment lists."
  (github-mock-with-service
    (gp-e2e-list--with-env
      (let ((github-mock-overrides (gp-e2e-list--gh-two-prs-overrides)))
        (gp-e2e-list--open-list)
        (setq github-mock-calls nil)
        (gp-e2e-list--goto "#43 ")
        (gp-e2e-list--key "RET")
        (should (eq major-mode 'gp-detail-mode))
        (should (equal (alist-get 'id gp--pr) 43))
        (should (string-match-p "Fix the flaky test" (gp-e2e-list--text)))
        (should (gp-e2e-list--gh "GET" "\\`/repos/acme/web/pulls/43\\'"))
        (should (gp-e2e-list--gh "GET" "/issues/43/comments\\'"))
        (should (gp-e2e-list--gh "GET" "/pulls/43/comments\\'"))))))

(ert-deftest gp-e2e-list-w-browses-the-row-under-point ()
  "`w' opens the web page of the PR at point, on both backends."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (gp-e2e-list--open-list)
      (gp-e2e-list--goto "#22 ")
      (gp-e2e-list--key "w")
      (should (equal gp-e2e-list--browsed
                     '("https://bitbucket.org/acme/lambda-data-service/pull-requests/22")))))
  (github-mock-with-service
    (gp-e2e-list--with-env
      (let ((github-mock-overrides (gp-e2e-list--gh-two-prs-overrides)))
        (gp-e2e-list--open-list)
        (gp-e2e-list--goto "#43 ")
        (gp-e2e-list--key "w")
        (should (equal gp-e2e-list--browsed '("https://github.com/acme/web/pull/43")))))))

;;;; gp-helm list ----------------------------------------------------------------

(defun gp-e2e-list--fresh-iso (hours-ago)
  "An ISO-8601 UTC timestamp HOURS-AGO hours before now."
  (format-time-string "%FT%T+00:00"
                      (time-subtract (current-time) (* hours-ago 3600)) t))

(defun gp-e2e-list--bb-merged-pr (id hours-ago)
  "A merged Bitbucket PR #ID of the user's, last updated HOURS-AGO."
  `((id . ,id) (title . ,(format "Merged change %d" id)) (state . "MERGED")
    (updated_on . ,(gp-e2e-list--fresh-iso hours-ago))
    (author (display_name . "Ada Lovelace") (uuid . ,gp-e2e-list--me))
    (merge_commit (hash . ,(format "m%d" id)))
    (destination (branch (name . "main"))
                 (repository (full_name . "acme/web-frontend") (slug . "web-frontend")))
    (source (branch (name . ,(format "feature/m%d" id))))))

(defun gp-e2e-list--bb-helm-overrides (merged)
  "Bitbucket overrides for a full `gp-helm' run.
The workspace list answers OPEN with the fixture and MERGED with
MERGED; the reviewer scan sees one repo holding the foreign PR."
  `(("/workspaces/[^/]+/pullrequests/"
     . ,(lambda (_m _p params _d)
          (if (equal (cdr (assoc "state" params)) "MERGED")
              `((values . ,merged))
            `((values . ,(gp-e2e-list--bb-prs))))))
    ("\\`/repositories/acme\\'" . ((values ((full_name . "acme/web-frontend")))))
    ("\\`/repositories/acme/web-frontend/pullrequests\\'"
     . ,(lambda (_m _p params _d)
          (if (string-match-p "reviewers.uuid" (or (cdr (assoc "q" params)) ""))
              `((values ,gp-e2e-list--bb-foreign-pr))
            '((values)))))
    ("/pullrequests/500\\'" . ,gp-e2e-list--bb-foreign-pr)))

(defun gp-e2e-list--run-helm (&optional prefix)
  "Run `gp-helm' interactively (PREFIX as its arg), drain, return the sources."
  (let ((current-prefix-arg prefix))
    (call-interactively #'gp-helm))
  (gp-test-drain-timers)
  (plist-get gp-e2e-list--helm :sources))

(defun gp-e2e-list--source (sources prefix)
  "The source in SOURCES whose name starts with PREFIX."
  (seq-find (lambda (s) (string-prefix-p prefix (alist-get 'name s))) sources))

(defun gp-e2e-list--candidate-ids (source)
  "The PR ids SOURCE's candidates currently hold (loading rows dropped)."
  (let ((cands (alist-get 'candidates source)))
    (delq nil (mapcar (lambda (c) (alist-get 'id (cdr c)))
                      (if (functionp cands) (funcall cands) cands)))))

(defun gp-e2e-list--run-action (source name pr)
  "Run SOURCE's action called NAME on PR, then drain."
  (let ((fn (cdr (assoc name (alist-get 'action source)))))
    (should fn)
    (prog1 (funcall fn pr) (gp-test-drain-timers))))

(ert-deftest gp-e2e-list-helm-sections-bitbucket ()
  "`gp-helm' opens at once and its sections fill from the background
fetches: the user's own open PRs, the reviewer scan, and the merged
section, which keeps only PRs merged inside the window."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let* ((gp-helm-show-merged-recent t)
             (gp-helm-merged-recent-hours 24)
             (bitbucket-mock-overrides
              (gp-e2e-list--bb-helm-overrides
               (list (gp-e2e-list--bb-merged-pr 901 2)
                     (gp-e2e-list--bb-merged-pr 902 30))))
             (sources (gp-e2e-list--run-helm)))
        (should (equal (mapcar (lambda (s) (alist-get 'name s)) sources)
                       '("Needs my review" "My pull requests"
                         "Approved by me"
                         "Covered by others" "My drafts"
                         "Merged in the last 24 hours")))
        (should (equal (gp-e2e-list--candidate-ids
                        (gp-e2e-list--source sources "Needs my review"))
                       '(500)))
        (should (equal (gp-e2e-list--candidate-ids
                        (gp-e2e-list--source sources "My pull requests"))
                       (mapcar (lambda (p) (alist-get 'id p)) (gp-e2e-list--bb-prs))))
        (should (equal (gp-e2e-list--candidate-ids
                        (gp-e2e-list--source sources "Merged in"))
                       '(901)))
        ;; the reviewer scan asked the repo for PRs where the user reviews
        (should (string-match-p
                 (regexp-quote (format "reviewers.uuid=\"%s\"" gp-e2e-list--me))
                 (cdr (assoc "q" (nth 2 (car (gp-e2e-list--bb
                                               "GET" "/acme/web-frontend/pullrequests\\'")))))))))))

(ert-deftest gp-e2e-list-helm-sections-github ()
  "Same sections on GitHub, from three searches: authored+open, review
requested, authored+closed (kept only when `merged_at' is recent)."
  (github-mock-with-service
    (gp-e2e-list--with-env
      (let* ((gp-helm-show-merged-recent t)
             (merged `((id . 544) (number . 44) (title . "Old merged work")
                       (state . "closed") (merged . t) (merged_at . ,(gp-e2e-list--fresh-iso 3))
                       (user (login . "ada"))
                       (head (ref . "feature/old") (sha . "aaa111") (repo (full_name . "acme/web")))
                       (base (ref . "main") (repo (full_name . "acme/web")))))
             (hit (lambda (n) `((number . ,n)
                                (repository_url . "https://api.github.com/repos/acme/web"))))
             (github-mock-overrides
              `(("\\`/search/issues\\'"
                 . ,(lambda (_m _p params _d)
                      (let ((q (cdr (assoc "q" params))))
                        (cond ((string-match-p "review-requested:ada" q) (list (funcall hit 43)))
                              ((string-match-p "is:closed" q) (list (funcall hit 44)))
                              (t (list (funcall hit 42)))))))
                ("/pulls/43\\'" . ,github-mock--pr-2)
                ("/pulls/44\\'" . ,merged)))
             (sources (gp-e2e-list--run-helm)))
        (should (equal (gp-e2e-list--candidate-ids
                        (gp-e2e-list--source sources "Needs my review"))
                       '(43)))
        (should (equal (gp-e2e-list--candidate-ids
                        (gp-e2e-list--source sources "My pull requests"))
                       '(42)))
        (should (equal (gp-e2e-list--candidate-ids
                        (gp-e2e-list--source sources "Merged in"))
                       '(44)))
        (should (equal (sort (mapcar (lambda (c) (cdr (assoc "q" (nth 2 c))))
                                     (gp-e2e-list--gh "GET" "/search/issues"))
                             #'string<)
                       '("is:pr author:ada is:closed" "is:pr author:ada is:open"
                         "is:pr review-requested:ada is:open")))))))

(ert-deftest gp-e2e-list-helm-merged-section-off-skips-the-fetch ()
  "With the merged section turned off there is no MERGED request and
no merged section; turned on, exactly one MERGED request is made."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let* ((bitbucket-mock-overrides (gp-e2e-list--bb-helm-overrides nil))
             (merged-requests
              (lambda ()
                (seq-filter (lambda (c) (equal (cdr (assoc "state" (nth 2 c))) "MERGED"))
                            (gp-e2e-list--bb "GET" "/workspaces/")))))
        (let* ((gp-helm-show-merged-recent nil)
               (sources (gp-e2e-list--run-helm)))
          (should (gp-e2e-list--source sources "My pull requests"))
          (should-not (gp-e2e-list--source sources "Merged in"))
          (should-not (funcall merged-requests)))
        (let* ((gp-helm-show-merged-recent t)
               (gp--result-cache (make-hash-table :test 'equal))
               (sources (gp-e2e-list--run-helm)))
          (should (gp-e2e-list--source sources "Merged in"))
          (should (= (length (funcall merged-requests)) 1)))))))

(ert-deftest gp-e2e-list-helm-default-action-opens-the-pr ()
  "RET on a helm row (the default action) opens that PR's detail buffer."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let* ((bitbucket-mock-overrides (gp-e2e-list--bb-helm-overrides nil))
             (sources (gp-e2e-list--run-helm))
             (src (gp-e2e-list--source sources "Needs my review"))
             (cand (car (funcall (alist-get 'candidates src)))))
        (should (equal (caar (alist-get 'action src)) "Open detail buffer"))
        (funcall (cdar (alist-get 'action src)) (cdr cand))
        (gp-test-drain-timers)
        (should (eq major-mode 'gp-detail-mode))
        (should (equal (alist-get 'id gp--pr) 500))
        (should (string-match-p "Tidy the logging config" (gp-e2e-list--text)))
        (should (gp-e2e-list--bb "GET" "/pullrequests/500/comments"))))))

(ert-deftest gp-e2e-list-helm-browse-action-opens-the-web-page ()
  "The \"Browse on web\" action hands the PR's real URL to the browser."
  (github-mock-with-service
    (gp-e2e-list--with-env
      (let* ((github-mock-overrides (gp-e2e-list--gh-two-prs-overrides))
             (sources (gp-e2e-list--run-helm))
             (src (gp-e2e-list--source sources "My pull requests"))
             (pr (cdar (funcall (alist-get 'candidates src)))))
        (gp-e2e-list--run-action src "Browse on web" pr)
        (should (equal gp-e2e-list--browsed '("https://github.com/acme/web/pull/42")))))))

;;;; Reviewer form ---------------------------------------------------------------

(defun gp-e2e-list--bb-user (uuid name)
  "A Bitbucket account alist."
  `((uuid . ,uuid) (display_name . ,name)))

(defun gp-e2e-list--bb-reviewer (uuid name state)
  "A Bitbucket REVIEWER participant."
  `((role . "REVIEWER") (state . ,state) (approved . ,(if (equal state "approved") t :json-false))
    (user ,@(gp-e2e-list--bb-user uuid name))))

(defun gp-e2e-list--bb-reviewer-overrides ()
  "PR 180 with Bea (approved) and Bob (pending); Erin is a default
reviewer; the member list spans two pages and includes the user."
  (let ((pr (cons `(participants ,(gp-e2e-list--bb-reviewer "{bea}" "Bea" "approved")
                                 ,(gp-e2e-list--bb-reviewer "{bob}" "Bob" nil))
                  (car (gp-e2e-list--bb-prs)))))
    `(("/pullrequests/180\\'" . ,pr)
      ("/default-reviewers\\'" . ((values ,(gp-e2e-list--bb-user "{erin}" "Erin"))))
      ("members\\?page=2\\'"
       . ((values ((user ,@(gp-e2e-list--bb-user "{dan}" "Dan")))
                  ((user ,@(gp-e2e-list--bb-user gp-e2e-list--me "Ada Lovelace")))
                  ((user ,@(gp-e2e-list--bb-user "{erin}" "Erin"))))))
      ("/workspaces/acme/members\\'"
       . ((values ((user ,@(gp-e2e-list--bb-user "{carol}" "Carol"))))
          (next . "https://api.bitbucket.org/2.0/workspaces/acme/members?page=2"))))))

(defun gp-e2e-list--line (name)
  "The text of the form line naming NAME."
  (save-excursion
    (gp-e2e-list--goto (concat " " name))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun gp-e2e-list--toggle (name)
  "Put point on NAME's checkbox and press SPC."
  (gp-e2e-list--goto (concat " " name))
  (beginning-of-line)
  (gp-e2e-list--key "SPC"))

(defun gp-e2e-list--open-reviewers (pr)
  "Open PR's detail buffer, press V, return the reviewer form buffer."
  (set-buffer (gp-show-pr pr))
  (gp-test-drain-timers)
  (gp-e2e-list--key "V")
  (let ((form (gp-e2e-list--buffer-named "reviewers #")))
    (should form)
    (set-buffer form)
    form))

(ert-deftest gp-e2e-list-reviewers-bitbucket-saves-the-whole-list ()
  "V in the detail buffer opens the form with the PR's reviewers ticked,
the repo default and the (two-page) workspace members offered, minus
the user.  Unticking Bob and ticking Carol PUTs the complete new list,
keeps the PR's title, closes the form and reloads the PR."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let ((bitbucket-mock-overrides (gp-e2e-list--bb-reviewer-overrides)))
        (let ((detail (gp-show-pr (car (gp-e2e-list--bb-prs)))))
          (set-buffer detail)
          (gp-test-drain-timers)
          (gp-e2e-list--key "V")
          (set-buffer (gp-e2e-list--buffer-named "reviewers #180"))
          (should (equal (gp-e2e-list--line "Bea") "[X] Bea   ✓ approved  (locked)"))
          (should (equal (gp-e2e-list--line "Bob") "[X] Bob   ⏳ pending"))
          (should (equal (gp-e2e-list--line "Erin") "[ ] Erin"))
          (should (equal (gp-e2e-list--line "Carol") "[ ] Carol"))
          (should (equal (gp-e2e-list--line "Dan") "[ ] Dan"))
          (should-not (string-match-p "Ada Lovelace" (gp-e2e-list--text)))
          ;; Erin is offered once, as a default, not again as a member
          (should (= (cl-count-if (lambda (l) (string-match-p "Erin" l))
                                  (split-string (gp-e2e-list--text) "\n"))
                     1))
          (gp-e2e-list--toggle "Bob")
          (gp-e2e-list--toggle "Carol")
          (setq bitbucket-mock-calls nil)
          (gp-e2e-list--key "C")
          (let ((puts (gp-e2e-list--bb "PUT")))
            (should (= (length puts) 1))
            (should (equal (nth 1 (car puts))
                           "/repositories/acme/web-frontend/pullrequests/180"))
            (should (equal (alist-get 'reviewers (nth 3 (car puts)))
                           [((uuid . "{bea}")) ((uuid . "{carol}"))]))
            (should (equal (alist-get 'title (nth 3 (car puts)))
                           "Add feature toggle for the new widget")))
          (should-not (gp-e2e-list--buffer-named "reviewers #180"))
          ;; the detail buffer reloaded the PR after the save
          (should (gp-e2e-list--bb "GET" "/pullrequests/180/comments")))))))

(ert-deftest gp-e2e-list-reviewers-github-sends-only-the-delta ()
  "On GitHub the form lists requested reviewers and collaborators (minus
the user); dropping bea and adding dan sends one DELETE and one POST to
requested_reviewers, carrying only those logins."
  (github-mock-with-service
    (gp-e2e-list--with-env
      (let ((github-mock-overrides
             `(("/pulls/42\\'"
                . ,(cons '(requested_reviewers ((login . "bea")))
                         (assq-delete-all 'requested_reviewers
                                          (copy-alist github-mock--pr-1))))
               ("/collaborators\\'"
                . (((login . "ada") (name . "Ada Lovelace"))
                   ((login . "bea") (name . nil))
                   ((login . "dan") (name . "Dan Abramov")))))))
        (gp-e2e-list--open-reviewers (github--reshape-pr github-mock--pr-1))
        (should (equal (gp-e2e-list--line "bea") "[X] bea   ⏳ pending"))
        (should (equal (gp-e2e-list--line "Dan Abramov") "[ ] Dan Abramov"))
        (should-not (string-match-p "Ada Lovelace" (gp-e2e-list--text)))
        (gp-e2e-list--toggle "bea")
        (gp-e2e-list--toggle "Dan Abramov")
        (setq github-mock-calls nil)
        (gp-e2e-list--key "C")
        (let ((writes (append (gp-e2e-list--gh "DELETE") (gp-e2e-list--gh "POST"))))
          (should (= (length writes) 2))
          (should (member '("DELETE" "/repos/acme/web/pulls/42/requested_reviewers"
                            nil ((reviewers . ["bea"])))
                          writes))
          (should (member '("POST" "/repos/acme/web/pulls/42/requested_reviewers"
                            nil ((reviewers . ["dan"])))
                          writes)))
        (should-not (gp-e2e-list--buffer-named "reviewers #42"))))))

(ert-deftest gp-e2e-list-reviewers-locked-reviewer-cannot-be-dropped ()
  "SPC on a reviewer who already approved refuses, and the save still
keeps them: Bea stays on the PR when only Carol is added."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let ((bitbucket-mock-overrides (gp-e2e-list--bb-reviewer-overrides)))
        (gp-e2e-list--open-reviewers (car (gp-e2e-list--bb-prs)))
        (gp-e2e-list--goto " Bea")
        (beginning-of-line)
        (should-error (execute-kbd-macro (kbd "SPC")) :type 'user-error)
        (should (equal (gp-e2e-list--line "Bea") "[X] Bea   ✓ approved  (locked)"))
        (gp-e2e-list--toggle "Carol")
        (setq bitbucket-mock-calls nil)
        (gp-e2e-list--key "C")
        (should (equal (alist-get 'reviewers (nth 3 (car (gp-e2e-list--bb "PUT"))))
                       [((uuid . "{bea}")) ((uuid . "{bob}")) ((uuid . "{carol}"))]))))))

(ert-deftest gp-e2e-list-reviewers-cancel-and-unchanged-send-nothing ()
  "q discards the form, and C with nothing changed closes it, both
without a single write."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let ((bitbucket-mock-overrides (gp-e2e-list--bb-reviewer-overrides)))
        (gp-e2e-list--open-reviewers (car (gp-e2e-list--bb-prs)))
        (gp-e2e-list--toggle "Carol")
        (gp-e2e-list--key "q")
        (should-not (gp-e2e-list--buffer-named "reviewers #180"))
        (gp-e2e-list--open-reviewers (car (gp-e2e-list--bb-prs)))
        (gp-e2e-list--key "C")
        (should-not (gp-e2e-list--buffer-named "reviewers #180"))
        (should-not (gp-e2e-list--bb "PUT"))))))

;;;; Real git repositories --------------------------------------------------------

(defun gp-e2e-list--git (dir &rest args)
  "Run git ARGS in DIR; return trimmed stdout, signalling on failure."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (let ((code (apply #'call-process "git" nil t nil args)))
        (unless (eq code 0)
          (error "git %S failed in %s: %s" args dir (buffer-string)))
        (string-trim (buffer-string))))))

(defun gp-e2e-list--commit (dir file content message)
  "Write CONTENT to FILE in DIR and commit it with MESSAGE."
  (let ((path (expand-file-name file dir)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert content)))
  (gp-e2e-list--git dir "add" "--" file)
  (gp-e2e-list--git dir "commit" "-q" "-m" message))

(defun gp-e2e-list--make-clone (slug forge-url &optional remote-branches)
  "Create a clone of SLUG under `gp-local-git-root'; return its directory.
Its `origin' is FORGE-URL, which is what identifies the repo.  Fetch
and push go to `upstream' (`gp-checkout-remote'), a local bare repo
holding main plus REMOTE-BRANCHES, each with a commit of its own."
  (let* ((root (expand-file-name gp-local-git-root))
         (bare (expand-file-name (concat slug "-upstream.git") root))
         (dir (expand-file-name slug root)))
    (make-directory dir t)
    (gp-e2e-list--git root "init" "-q" "--bare" "-b" "main" bare)
    (gp-e2e-list--git dir "init" "-q" "-b" "main")
    (gp-e2e-list--git dir "remote" "add" "origin" forge-url)
    (gp-e2e-list--git dir "remote" "add" "upstream" bare)
    (gp-e2e-list--commit dir "README.md" "hello\n" "Initial commit")
    (gp-e2e-list--git dir "push" "-q" "upstream" "main")
    (dolist (b remote-branches)
      (gp-e2e-list--git dir "checkout" "-q" "-b" b)
      (gp-e2e-list--commit dir (concat (replace-regexp-in-string "/" "-" b) ".txt")
                           "work\n" (concat "Work on " b))
      (gp-e2e-list--git dir "push" "-q" "upstream" b)
      (gp-e2e-list--git dir "checkout" "-q" "main")
      (gp-e2e-list--git dir "branch" "-q" "-D" b))
    dir))

(defun gp-e2e-list--branch (dir)
  "The branch checked out in DIR."
  (gp-e2e-list--git dir "rev-parse" "--abbrev-ref" "HEAD"))

;;;; PR creation -------------------------------------------------------------------

(defun gp-e2e-list--magit-status (dir)
  "Open a real magit status buffer for DIR and make it current."
  (require 'magit)
  (let ((buf (magit-status-setup-buffer dir)))
    (set-buffer buf)
    buf))

(defun gp-e2e-list--bb-create-overrides (created)
  "Bitbucket overrides for creating a PR in acme/web-frontend.
The repo has no open PRs; POST answers CREATED merged with the body."
  `(("\\`/repositories/acme/web-frontend\\'" . ((mainbranch (name . "main"))))
    ("/refs/branches\\'" . ((values ((name . "main")) ((name . "develop")))))
    ("/default-reviewers\\'" . ((values ,(gp-e2e-list--bb-user "{erin}" "Erin"))))
    ("/workspaces/acme/members\\'"
     . ((values ((user ,@(gp-e2e-list--bb-user "{carol}" "Carol"))))))
    ("/pullrequests/777\\'" . ,created)
    ("\\`/repositories/acme/web-frontend/pullrequests\\'"
     . ,(lambda (method _p _params data)
          (if (equal method "POST") (append created data) '((values)))))))

(defconst gp-e2e-list--bb-created
  '((id . 777) (title . "New thing") (state . "OPEN")
    (author (display_name . "Ada Lovelace")
            (uuid . "{21d7839d-779f-44b2-8c40-6f43ac90be06}"))
    (destination (branch (name . "main"))
                 (repository (full_name . "acme/web-frontend") (slug . "web-frontend")))
    (source (branch (name . "feature/new-thing"))))
  "The PR the Bitbucket stand-in answers the create POST with.")

(defun gp-e2e-list--prepare-feature (dir branch)
  "Check out a new local BRANCH in DIR with two commits on it."
  (gp-e2e-list--git dir "checkout" "-q" "-b" branch)
  (gp-e2e-list--commit dir "api.py" "x = 1\n" "Add widget endpoint")
  (gp-e2e-list--commit dir "docs.md" "# api\n" "Document widget endpoint"))

(ert-deftest gp-e2e-list-create-from-magit-bitbucket ()
  "`gp-helm' in a magit buffer on a branch with no PR opens the create
form, pre-filled from the commits.  Typing into the title, ticking a
suggested reviewer and unticking draft, then C, pushes the branch and
POSTs exactly that PR, then shows it."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let* ((gp-create-draft t)
             (gp-create-close-source-branch t)
             (gp-create-preferred-reviewers nil)
             (dir (gp-e2e-list--make-clone
                   "web-frontend" "git@bitbucket.org:acme/web-frontend.git"))
             (bitbucket-mock-overrides
              (gp-e2e-list--bb-create-overrides gp-e2e-list--bb-created)))
        (gp-e2e-list--prepare-feature dir "feature/new-thing")
        (gp-e2e-list--magit-status dir)
        (call-interactively #'gp-helm)
        (gp-test-drain-timers)
        (should-not gp-e2e-list--helm)
        (set-buffer gp-create-buffer)
        (let ((text (gp-e2e-list--text)))
          (should (string-match-p "feature/new-thing  →  main" text))
          (should (string-match-p "in acme/web-frontend" text))
          (should (string-match-p "^New thing" text))
          (should (string-match-p "- Document widget endpoint\n- Add widget endpoint" text))
          (should (string-match-p "\\[X\\] Erin" text))
          (should (string-match-p "\\[ \\] Carol" text)))
        ;; type in the title field: C and q insert there, never submit
        (gp-e2e-list--goto "New thing")
        (forward-char (length "New thing"))
        (gp-e2e-list--key "SPC C q")
        (gp-e2e-list--toggle "Create as draft")
        (gp-e2e-list--toggle "Carol")
        (goto-char (point-min))
        (gp-e2e-list--key "C")
        ;; pushed to the fetch remote before the PR was opened
        (should (string-match-p "refs/heads/feature/new-thing"
                                (gp-e2e-list--git dir "ls-remote" "--heads" "upstream")))
        (let ((posts (gp-e2e-list--bb "POST")))
          (should (= (length posts) 1))
          (should (equal (nth 1 (car posts))
                         "/repositories/acme/web-frontend/pullrequests"))
          (should (equal (nth 3 (car posts))
                         '((title . "New thing Cq")
                           (source (branch (name . "feature/new-thing")))
                           (destination (branch (name . "main")))
                           (description . "- Document widget endpoint\n- Add widget endpoint")
                           (close_source_branch . t)
                           (reviewers ((uuid . "{erin}")) ((uuid . "{carol}")))))))
        (should-not (get-buffer gp-create-buffer))
        (should (eq major-mode 'gp-detail-mode))
        (should (equal (alist-get 'id gp--pr) 777))))))

(ert-deftest gp-e2e-list-create-github ()
  "On GitHub the form POSTs head/base/draft to /pulls, then requests the
ticked collaborator as reviewer on the new PR number."
  (github-mock-with-service
    (gp-e2e-list--with-env
      (let* ((gp-create-draft t)
             (gp-create-preferred-reviewers nil)
             (dir (gp-e2e-list--make-clone "web" "git@github.com:acme/web.git"))
             (created (append '((number . 77) (id . 9077) (state . "open")
                                (user (login . "ada"))
                                (head (ref . "feature/new-thing") (repo (full_name . "acme/web")))
                                (base (ref . "main") (repo (full_name . "acme/web")))
                                (html_url . "https://github.com/acme/web/pull/77"))))
             (github-mock-overrides
              `(("\\`/repos/acme/web/pulls\\'"
                 . ,(lambda (method _p _params data)
                      (if (equal method "POST")
                          (append created (list (assq 'title data)))
                        nil)))
                ("/pulls/77\\'" . ,(cons '(title . "New thing") created))
                ("/branches\\'" . (((name . "main")) ((name . "develop")))))))
        (gp-e2e-list--prepare-feature dir "feature/new-thing")
        (gp-e2e-list--magit-status dir)
        (call-interactively #'gp-helm)
        (gp-test-drain-timers)
        (set-buffer gp-create-buffer)
        (should (string-match-p "\\[ \\] bea" (gp-e2e-list--text)))
        (gp-e2e-list--toggle "bea")
        (goto-char (point-min))
        (gp-e2e-list--key "C")
        (should (string-match-p "refs/heads/feature/new-thing"
                                (gp-e2e-list--git dir "ls-remote" "--heads" "upstream")))
        (should (equal (mapcar (lambda (c) (list (nth 1 c) (nth 3 c))) (gp-e2e-list--gh "POST"))
                       '(("/repos/acme/web/pulls"
                          ((title . "New thing") (head . "feature/new-thing") (base . "main")
                           (body . "- Document widget endpoint\n- Add widget endpoint")
                           (draft . t)))
                         ("/repos/acme/web/pulls/77/requested_reviewers"
                          ((reviewers "bea"))))))
        (should (equal (alist-get 'id gp--pr) 77))))))

(ert-deftest gp-e2e-list-create-cancel-sends-nothing ()
  "With no reviewer to offer (no defaults, members forbidden) the form
still opens with a note.  q outside a field drops it without pushing or
POSTing and falls back to the Helm PR list."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let* ((dir (gp-e2e-list--make-clone
                   "web-frontend" "git@bitbucket.org:acme/web-frontend.git"))
             (bitbucket-mock-overrides
              (append `(("\\`/repositories/acme\\'" . ((values)))
                        ("/default-reviewers\\'" . ((values)))
                        ("/members\\'" . ,(lambda (&rest _) (error "HTTP 403"))))
                      (gp-e2e-list--bb-create-overrides gp-e2e-list--bb-created))))
        (gp-e2e-list--prepare-feature dir "feature/new-thing")
        (gp-e2e-list--magit-status dir)
        (call-interactively #'gp-helm)
        (gp-test-drain-timers)
        (set-buffer gp-create-buffer)
        ;; nobody to offer (no defaults, members forbidden): a note, no boxes
        (should (string-match-p "(no default or suggested reviewers for this repo)"
                                (gp-e2e-list--text)))
        (goto-char (point-min))
        (gp-e2e-list--key "q")
        (should-not (get-buffer gp-create-buffer))
        (should-not (gp-e2e-list--bb "POST"))
        (should (string-empty-p (gp-e2e-list--git dir "ls-remote" "--heads" "upstream"
                                                  "feature/new-thing")))
        (should (gp-e2e-list--source (plist-get gp-e2e-list--helm :sources)
                                     "My pull requests"))))))

(ert-deftest gp-e2e-list-magit-branch-with-a-pr-opens-it ()
  "`gp-helm' in a magit buffer on a branch that already has an open PR
skips the form and the list and opens that PR."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let* ((dir (gp-e2e-list--make-clone
                   "web-frontend" "git@bitbucket.org:acme/web-frontend.git"))
             (pr (car (gp-e2e-list--bb-prs)))
             (bitbucket-mock-overrides
              `(("\\`/repositories/acme/web-frontend/pullrequests\\'" . ((values ,pr))))))
        (gp-e2e-list--git dir "checkout" "-q" "-b" "feature/widget-toggle")
        (gp-e2e-list--magit-status dir)
        (call-interactively #'gp-helm)
        (gp-test-drain-timers)
        (should-not gp-e2e-list--helm)
        (should-not (get-buffer gp-create-buffer))
        (should (eq major-mode 'gp-detail-mode))
        (should (equal (alist-get 'id gp--pr) 180))))))

;;;; Checkout ----------------------------------------------------------------------

(ert-deftest gp-e2e-list-b-checks-out-the-pr-branch-stashing-work ()
  "`b' on a list row fetches the PR's branch from the remote into the
matching local clone, stashing uncommitted work first, then opens it."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let ((dir (gp-e2e-list--make-clone
                  "web-frontend" "git@bitbucket.org:acme/web-frontend.git"
                  '("feature/widget-toggle"))))
        (with-temp-file (expand-file-name "README.md" dir) (insert "edited\n"))
        ;; forget the remote's main, so fetching the PR's target is visible
        (gp-e2e-list--git dir "update-ref" "-d" "refs/remotes/upstream/main")
        (gp-e2e-list--open-list)
        (gp-e2e-list--goto "#180 ")
        (gp-e2e-list--key "b")
        (should (gp-e2e-list--git dir "rev-parse" "--verify" "upstream/main"))
        (should (equal (gp-e2e-list--branch dir) "feature/widget-toggle"))
        (should (file-exists-p (expand-file-name "feature-widget-toggle.txt" dir)))
        (should (equal (gp-e2e-list--git dir "stash" "list" "--format=%s")
                       "On main: gp-auto: WIP on main"))
        (should (equal (gp-e2e-list--git dir "status" "--porcelain") ""))
        (should (equal gp-e2e-list--opened (list dir)))))))

(ert-deftest gp-e2e-list-o-opens-the-clone-without-switching ()
  "`o' opens the matching clone -- found by its remote, not its folder
name -- and leaves its branch alone; a PR with no clone is refused."
  (github-mock-with-service
    (gp-e2e-list--with-env
      (let* ((github-mock-overrides (gp-e2e-list--gh-two-prs-overrides))
             (dir (gp-e2e-list--make-clone "my-web-copy" "https://github.com/acme/web.git")))
        (gp-e2e-list--open-list)
        (gp-e2e-list--goto "#42 ")
        (gp-e2e-list--key "o")
        (should (equal gp-e2e-list--opened (list dir)))
        (should (equal (gp-e2e-list--branch dir) "main")))))
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (gp-e2e-list--open-list)
      (gp-e2e-list--goto "#70 ")
      (should-error (execute-kbd-macro (kbd "o")) :type 'user-error)
      (should-not gp-e2e-list--opened))))

(ert-deftest gp-e2e-list-helm-checkout-action-github ()
  "The helm \"Checkout branch & open\" action switches the GitHub clone
to the PR's head branch and opens it."
  (github-mock-with-service
    (gp-e2e-list--with-env
      (let* ((github-mock-overrides (gp-e2e-list--gh-two-prs-overrides))
             (dir (gp-e2e-list--make-clone "web" "git@github.com:acme/web.git"
                                           '("feature/widget")))
             (sources (gp-e2e-list--run-helm))
             (src (gp-e2e-list--source sources "My pull requests"))
             (pr (cdar (funcall (alist-get 'candidates src)))))
        (gp-e2e-list--run-action src "Checkout branch & open" pr)
        (should (equal (gp-e2e-list--branch dir) "feature/widget"))
        (should (string-empty-p (gp-e2e-list--git dir "stash" "list")))
        (should (equal gp-e2e-list--opened (list dir)))))))

;;;; gp-watch-mode ------------------------------------------------------------------

(defun gp-e2e-list--overlay-lines ()
  "Line numbers carrying a PR-comment overlay in the current buffer."
  (sort (delq nil (mapcar (lambda (ov) (overlay-get ov 'gp-line))
                          (overlays-in (point-min) (point-max))))
        #'<))

(defun gp-e2e-list--lines (n)
  "N numbered lines of text."
  (mapconcat (lambda (i) (format "line %d" i)) (number-sequence 1 n) "\n"))

(ert-deftest gp-e2e-list-watch-bitbucket-mode-line-and-overlays ()
  "Visiting a file of a clone on a PR branch shows the repo's open-PR
count and the branch PR's comment count in the mode line, and draws the
PR's inline comments for that file."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let* ((file "src/app/shared/popover/popover.component.ts")
             (dir (gp-e2e-list--make-clone
                   "web-frontend" "git@bitbucket.org:acme/web-frontend.git"))
             (bitbucket-mock-overrides
              `(("\\`/repositories/acme/web-frontend/pullrequests\\'"
                 . ,(lambda (_m _p params _d)
                      (cond ((assoc "pagelen" params) '((size . 4)))
                            ((string-match-p "source.branch.name=\"feature/widget-toggle\""
                                             (or (cdr (assoc "q" params)) ""))
                             `((values ,(car (gp-e2e-list--bb-prs)))))
                            (t '((values)))))))))
        (gp-e2e-list--git dir "checkout" "-q" "-b" "feature/widget-toggle")
        (gp-e2e-list--commit dir file (gp-e2e-list--lines 80) "Add popover")
        (gp-watch-mode 1)
        (with-current-buffer (find-file-noselect (expand-file-name file dir))
          (should (equal (substring-no-properties gp-watch-mode-line) " BB:4 💬13"))
          (should (equal gp-watch--repo "acme/web-frontend"))
          (should (gp-e2e-list--bb "GET" "/pullrequests/180/comments"))
          (should (equal (gp-e2e-list--overlay-lines) '(61))))
        ;; a file the PR never commented on: mode line, but no overlays
        (with-current-buffer (find-file-noselect (expand-file-name "README.md" dir))
          (should (equal (substring-no-properties gp-watch-mode-line) " BB:4 💬13"))
          (should-not (gp-e2e-list--overlay-lines)))))))

(ert-deftest gp-e2e-list-watch-github-mode-line-click-opens-the-pr ()
  "On GitHub the count is the repo's open PRs and the comment counter
adds issue and review comments; clicking it opens the branch's PR."
  (github-mock-with-service
    (gp-e2e-list--with-env
      (let ((dir (gp-e2e-list--make-clone "web" "git@github.com:acme/web.git")))
        (gp-e2e-list--git dir "checkout" "-q" "-b" "feature/widget")
        (gp-e2e-list--commit dir "widget.el" (gp-e2e-list--lines 20) "Add widget")
        (gp-watch-mode 1)
        (with-current-buffer (find-file-noselect (expand-file-name "widget.el" dir))
          (should (equal (substring-no-properties gp-watch-mode-line) " BB:2 💬3"))
          (should (equal (gp-e2e-list--overlay-lines) '(10)))
          ;; the branch PR was looked up by head, not picked from the list
          (should (member '(("state" . "open") ("head" . "acme:feature/widget"))
                          (mapcar (lambda (c) (nth 2 c))
                                  (gp-e2e-list--gh "GET" "/repos/acme/web/pulls\\'"))))
          (call-interactively #'gp-watch-visit-branch-pr)
          (gp-test-drain-timers))
        (let ((detail (gp-e2e-list--buffer-named "PR #42 ")))
          (should detail)
          (should (equal (alist-get 'id (buffer-local-value 'gp--pr detail)) 42)))))))

(ert-deftest gp-e2e-list-watch-off-branch-shows-only-the-count ()
  "On a branch without a PR the mode line has the repo count and no
comment counter; outside any forge clone it is blank."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let* ((dir (gp-e2e-list--make-clone
                   "web-frontend" "git@bitbucket.org:acme/web-frontend.git"))
             (bitbucket-mock-overrides
              `(("\\`/repositories/acme/web-frontend/pullrequests\\'"
                 . ,(lambda (_m _p params _d)
                      (if (assoc "pagelen" params) '((size . 0)) '((values)))))))
             (plain (expand-file-name "plain" gp-local-git-root)))
        (gp-watch-mode 1)
        (with-current-buffer (find-file-noselect (expand-file-name "README.md" dir))
          (should (equal (substring-no-properties gp-watch-mode-line) " BB:0")))
        (make-directory plain t)
        (gp-e2e-list--git plain "init" "-q")
        (with-temp-file (expand-file-name "x.txt" plain) (insert "x"))
        (with-current-buffer (find-file-noselect (expand-file-name "x.txt" plain))
          (should (equal gp-watch-mode-line "")))))))

(ert-deftest gp-e2e-list-create-preferred-reviewers-come-ticked ()
  "A preferred reviewer (matched by name, case-insensitively) comes
ticked and starred; a preferred id matching nobody is logged, never
invented; the POST carries the default plus the preferred one."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let* ((gp-create-preferred-reviewers '("{ghost}" "carol"))
             (gp-log-enabled t)
             (gp-log-buffer-name "*gp-e2e-list log*")
             (dir (gp-e2e-list--make-clone
                   "web-frontend" "git@bitbucket.org:acme/web-frontend.git"))
             (bitbucket-mock-overrides
              (gp-e2e-list--bb-create-overrides gp-e2e-list--bb-created)))
        (gp-e2e-list--prepare-feature dir "feature/new-thing")
        (gp-e2e-list--magit-status dir)
        (call-interactively #'gp-helm)
        (gp-test-drain-timers)
        (set-buffer gp-create-buffer)
        (should (string-match-p "\\[X\\] Carol *★" (gp-e2e-list--text)))
        (should-not (string-match-p "ghost" (gp-e2e-list--text)))
        (let ((log (gp-e2e-list--text gp-log-buffer-name)))
          (should (string-match-p "preferred reviewer \"{ghost}\" matched no candidate" log))
          (should-not (string-match-p "\"carol\" matched no" log)))
        (goto-char (point-min))
        (gp-e2e-list--key "C")
        (should (equal (alist-get 'reviewers (nth 3 (car (gp-e2e-list--bb "POST"))))
                       '(((uuid . "{erin}")) ((uuid . "{carol}")))))))))

(ert-deftest gp-e2e-list-b-clones-a-missing-repo-when-allowed ()
  "`b' on a PR with no local clone refuses, naming the setting, while
cloning is off; with `gp-checkout-clone-base' set it clones the repo
under the git root and checks the PR's branch out there."
  (bitbucket-mock-with-service
    (gp-e2e-list--with-env
      (let* ((staging (make-temp-file "gp-e2e-list-remote" t))
             (gp-checkout-remote "origin")
             (gp-checkout-clone-base nil))
        (unwind-protect
            (progn
              (let ((gp-local-git-root staging))
                (gp-e2e-list--make-clone "web-frontend" "git@bitbucket.org:acme/web-frontend.git"
                                         '("feature/widget-toggle")))
              (make-directory (expand-file-name "acme" staging))
              (rename-file (expand-file-name "web-frontend-upstream.git" staging)
                           (expand-file-name "acme/web-frontend.git" staging))
              (gp-e2e-list--open-list)
              (gp-e2e-list--goto "#180 ")
              (let ((err (should-error (execute-kbd-macro (kbd "b")) :type 'user-error)))
                (should (string-match-p "set gp-checkout-clone-base" (cadr err))))
              (should-not gp-e2e-list--opened)
              (setq gp-checkout-clone-base (file-name-as-directory staging))
              (gp-e2e-list--key "b")
              (let ((dir (expand-file-name "web-frontend" gp-local-git-root)))
                (should (equal (gp-e2e-list--branch dir) "feature/widget-toggle"))
                (should (equal gp-e2e-list--opened (list dir)))))
          (delete-directory staging t))))))

(ert-deftest gp-e2e-list-watch-magit-buffer-shows-the-branch-pr ()
  "A magit status buffer has no file, so watch resolves repo and branch
from its directory: the mode line gets the count and the branch PR's
comments, and nothing is overlaid."
  (require 'magit)
  (github-mock-with-service
    (gp-e2e-list--with-env
      (let ((dir (gp-e2e-list--make-clone "web" "git@github.com:acme/web.git")))
        (gp-e2e-list--git dir "checkout" "-q" "-b" "feature/widget")
        (gp-watch-mode 1)
        (gp-e2e-list--magit-status dir)
        (should (derived-mode-p 'magit-status-mode))
        (should (equal gp-watch--repo "acme/web"))
        (should (equal (substring-no-properties gp-watch-mode-line) " BB:2 💬3"))
        (should (equal (alist-get 'id gp-watch--branch-pr) 42))
        (should-not (gp-e2e-list--overlay-lines))))))

(ert-deftest gp-e2e-list-watch-click-with-two-branch-prs-offers-a-picker ()
  "Clicking the comment counter when two open PRs share the branch opens
the branch-scoped Helm picker over exactly those two."
  (github-mock-with-service
    (gp-e2e-list--with-env
      (let* ((twin (cons '(head (ref . "feature/widget") (sha . "fff999")
                                (repo (full_name . "acme/web")))
                         (assq-delete-all 'head (copy-alist github-mock--pr-2))))
             (github-mock-overrides
              `(("\\`/repos/acme/web/pulls\\'"
                 . ,(mapcar #'github-mock--simple-pr (list github-mock--pr-1 twin)))))
             (dir (gp-e2e-list--make-clone "web" "git@github.com:acme/web.git")))
        (gp-e2e-list--git dir "checkout" "-q" "-b" "feature/widget")
        (gp-watch-mode 1)
        (with-current-buffer (find-file-noselect (expand-file-name "README.md" dir))
          (call-interactively #'gp-watch-visit-branch-pr))
        ;; a single source, passed bare rather than in a list
        (let ((src (plist-get gp-e2e-list--helm :sources)))
          (should (equal (alist-get 'name src) "Open PRs · acme/web · feature/widget"))
          (should (equal (gp-e2e-list--candidate-ids src) '(42 43))))
        (should-not (gp-e2e-list--buffer-named "PR #4[23] "))))))

(provide 'gp-e2e-list-test)
;;; gp-e2e-list-test.el ends here
