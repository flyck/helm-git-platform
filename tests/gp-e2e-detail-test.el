;;; gp-e2e-detail-test.el --- End-to-end flows through the PR detail buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; Drives the PR detail buffer from its real entry point (`gp-show-pr')
;; and its real keymap (`gp-detail-mode-map', via `execute-kbd-macro')
;; all the way down to the mocked network, then asserts on what reached
;; the mock (`bitbucket-mock-calls' / `github-mock-calls') and on the
;; rendered buffer.  Only the network (the mock services), user prompts,
;; `browse-url' and timers are replaced; no gp-*/bitbucket-*/github-*
;; function is stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'magit-section)
(require 'gp-test-helper)
(require 'git-platform)
(require 'git-platform-bitbucket)
(require 'git-platform-github)
(require 'gp-local)
(require 'gp-compose)
(require 'gp-pipeline)
(require 'gp-ui)
(require 'bitbucket-mock)
(require 'github-mock)

;;;; Harness -------------------------------------------------------------------




;; The shared mocks replace the JSON choke points but not the two raw-text
;; ones the detail buffer's diff load uses.  These are network-level
;; stand-ins in the same style (recorded in the mock's call log, answered
;; immediately), not stubs of package logic.




(defun gp-e2e--no-network (&rest args)
  "Trip-wire: signal for any real HTTP request (ARGS name it)."
  (error "Test reached the network: %S" (car args)))

(defmacro gp-e2e--with-env (&rest body)
  "Run BODY with deferred work queued (see `gp-test-drain-timers') and no clones.
Kills every buffer BODY created."
  (declare (indent 0) (debug t))
  `(let (         (gp-local-git-root (make-temp-file "gp-e2e-root" t))
         (gp-detail-show-avatars nil)
         (gp-detail-help-at-point nil)
         (gp-local--remote-cache (make-hash-table :test 'equal))
         (gp--recent-pipeline-steps-cache (make-hash-table :test 'equal))
         (before (buffer-list)))
     (unwind-protect
         (gp-test-with-queued-timers
           (cl-letf (((symbol-function 'browse-url) #'ignore)
                     ((symbol-function 'url-retrieve) #'gp-e2e--no-network)
                     ((symbol-function 'url-retrieve-synchronously) #'gp-e2e--no-network))
             (save-window-excursion ,@body)))
       (dolist (b (buffer-list))
         (unless (memq b before)
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory gp-local-git-root t))))

(defun gp-e2e--open (pr)
  "Open PR's detail buffer through `gp-show-pr', drain loads, return buffer."
  (let ((buf (gp-show-pr pr)))
    (gp-test-drain-timers)
    (set-buffer buf)
    buf))

(defun gp-e2e--key (keys)
  "Press KEYS (a `kbd' string) in the current buffer through its keymap.
The buffer is shown in the selected window with point kept, since the
command loop reads point from there.  Deferred work is drained after,
and the current buffer is restored."
  (let ((buf (current-buffer))
        (pos (point)))
    (set-window-buffer (selected-window) buf)
    (set-window-point (selected-window) pos)
    (execute-kbd-macro (kbd keys))
    (gp-test-drain-timers)
    (when (buffer-live-p buf) (set-buffer buf))))

(defun gp-e2e--text ()
  "The current buffer's text without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun gp-e2e--goto (text)
  "Move point to the start of the first occurrence of TEXT."
  (goto-char (point-min))
  (unless (search-forward text nil t)
    (error "Text %S not in buffer:\n%s" text (gp-e2e--text)))
  (goto-char (match-beginning 0)))

(defun gp-e2e--compose-buffer ()
  "Return the live compose buffer, or nil."
  (seq-find (lambda (b) (buffer-local-value 'gp-compose-mode b)) (buffer-list)))

(defun gp-e2e--submit-compose (text)
  "Replace the compose buffer's text with TEXT and submit it with C-c C-c.
Returns to the buffer that was current before."
  (let ((back (current-buffer))
        (buf (gp-e2e--compose-buffer)))
    (should buf)
    (set-buffer buf)
    (erase-buffer)
    (insert text)
    (gp-e2e--key "C-c C-c")
    (should-not (buffer-live-p buf))
    (set-buffer back)))

(defun gp-e2e--bb-calls (method &optional regexp)
  "Bitbucket mock calls with METHOD (nil: any) and path matching REGEXP.
Oldest first."
  (reverse (seq-filter (lambda (c) (and (or (null method) (equal (car c) method))
                                        (or (null regexp) (string-match-p regexp (nth 1 c)))))
                       bitbucket-mock-calls)))

(defun gp-e2e--gh-calls (method &optional regexp)
  "GitHub mock calls with METHOD (nil: any) and path matching REGEXP.
Oldest first."
  (reverse (seq-filter (lambda (c) (and (or (null method) (equal (car c) method))
                                        (or (null regexp) (string-match-p regexp (nth 1 c)))))
                       github-mock-calls)))

(defun gp-e2e--bb-pr ()
  "The Bitbucket overview PR #180 as the list hands it to `gp-show-pr'."
  (car (alist-get 'values (bitbucket-mock--fixture "workspace-prs.json"))))

(defun gp-e2e--gh-pr ()
  "The GitHub overview PR #42 as the list hands it to `gp-show-pr'."
  (github--reshape-pr (github-mock--simple-pr github-mock--pr-1)))

;;;; Fixtures --------------------------------------------------------------------

(defconst gp-e2e--bb-me "{21d7839d-779f-44b2-8c40-6f43ac90be06}"
  "Ada's uuid: the mock's /user, and the author of PR #180.")

(defconst gp-e2e--bb-grace
  '((display_name . "Grace Hopper") (uuid . "{6b0d3a6e-2f4c-4e8e-9a51-3c1f0e7d2b10}"))
  "A second Bitbucket user, as the fixtures spell her.")

(defun gp-e2e--bb-full-pr (&rest changes)
  "The single-PR answer for #180, with CHANGES (an alist) layered on top."
  (append changes
          (cl-remove-if (lambda (cell) (assq (car cell) changes))
                        (copy-tree (gp-e2e--bb-pr)))))

(defun gp-e2e--stateful (cell-symbol)
  "Return an override serving the PR in CELL-SYMBOL's value.
GET answers it; PUT/PATCH merge the request body into it first, the way
the forges answer a partial update with the updated object."
  (lambda (method _path _params data)
    (when (member method '("PUT" "PATCH"))
      (set cell-symbol
           (append data (cl-remove-if (lambda (cell) (assq (car cell) data))
                                      (symbol-value cell-symbol)))))
    (if (equal method "POST") nil (symbol-value cell-symbol))))

(defvar gp-e2e--pr-state nil
  "The PR a stateful override (see `gp-e2e--stateful') is serving.")

(defun gp-e2e--gh-full-pr (&rest changes)
  "GET /pulls/42's answer, with CHANGES (an alist) layered on top."
  (append changes
          (cl-remove-if (lambda (cell) (assq (car cell) changes))
                        (copy-tree github-mock--pr-1))))

(defconst gp-e2e--gh-own-comment
  '((id . 9010) (body . "My own note on the toggle")
    (user (login . "ada") (avatar_url . "https://example.com/ada.png"))
    (created_at . "2026-01-01T00:20:00Z")
    (html_url . "https://github.com/acme/web/pull/42#issuecomment-9010"))
  "A general comment written by the mock's signed-in user.")

(defun gp-e2e--gh-own-comments-override ()
  "Override listing `gp-e2e--gh-own-comment' beside the mock's general one.
Only GET answers; other methods fall through to the built-in routes."
  (cons "/issues/42/comments\\'"
        (lambda (method &rest _)
          (when (equal method "GET")
            (list github-mock--issue-comment gp-e2e--gh-own-comment)))))

;;;; Opening a PR ------------------------------------------------------------------

(ert-deftest gp-e2e-detail-open-renders-pr/bitbucket ()
  "Opening a Bitbucket PR fetches it and its comments and renders both."
  (let ((bitbucket-mock-overrides
         `(("/pullrequests/180\\'" . ,(gp-e2e--bb-full-pr
                                       '(description . "Gates the widget behind a flag."))))))
    (bitbucket-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--bb-pr))
        (should (equal major-mode 'gp-detail-mode))
        (should (gp-e2e--bb-calls "GET" "/repositories/acme/web-frontend/pullrequests/180\\'"))
        (should (gp-e2e--bb-calls "GET" "/pullrequests/180/comments\\'"))
        (let ((text (gp-e2e--text)))
          (should (string-match-p "#180  Add feature toggle for the new widget" text))
          (should (string-match-p "feature/widget-toggle → main" text))
          (should (string-match-p "Gates the widget behind a flag\\." text))
          (should (string-match-p "Comments (9)" text))
          (should (string-match-p "Follow-up: please confirm the feature flag" text))
          (should (string-match-p "src/app/service/data.service.ts:1229" text))
          ;; the loading marker is gone once both fetches landed
          (should-not (string-match-p "⏳" text)))))))

(ert-deftest gp-e2e-detail-open-renders-pr/github ()
  "Opening a GitHub PR fetches it, its comments and files, and renders them."
  (github-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--gh-pr))
      (should (gp-e2e--gh-calls "GET" "\\`/repos/acme/web/pulls/42\\'"))
      (should (gp-e2e--gh-calls "GET" "/issues/42/comments\\'"))
      (should (gp-e2e--gh-calls "GET" "/pulls/42/comments\\'"))
      (let ((text (gp-e2e--text)))
        (should (string-match-p "#42  Add the widget toggle" text))
        (should (string-match-p "Adds the toggle\\." text))
        (should (string-match-p "Comments (2)" text))
        (should (string-match-p "General comment" text))
        (should (string-match-p "Inline comment" text))
        (should (string-match-p "widget\\.el  \\+4 -1" text))
        (should (string-match-p "bug +ui" text))))))

(ert-deftest gp-e2e-detail-shows-reviewers/bitbucket ()
  "Reviewers come from the PR's participants, each with their verdict."
  (let ((bitbucket-mock-overrides
         `(("/pullrequests/180\\'"
            . ,(gp-e2e--bb-full-pr
                `(participants
                  ((role . "REVIEWER") (approved . t) (state . "approved")
                   (user ,@gp-e2e--bb-grace))
                  ((role . "REVIEWER") (approved . :json-false)
                   (user (display_name . "Review Bot") (uuid . "{bot}")))
                  ((role . "PARTICIPANT") (user (display_name . "Lurker")))))))))
    (bitbucket-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--bb-pr))
        (let ((line (progn (gp-e2e--goto "👥")
                           (buffer-substring-no-properties (point) (line-end-position)))))
          (should (string-match-p "Grace Hopper" line))
          (should (string-match-p "Review Bot" line))
          (should-not (string-match-p "Lurker" line))
          (should-not (string-match-p "no reviewers" line)))))))

(ert-deftest gp-e2e-detail-shows-reviewers/github ()
  "Reviewers merge submitted reviews with still-requested reviewers."
  (let ((github-mock-overrides
         `(("/pulls/42\\'" . ,(gp-e2e--gh-full-pr '(requested_reviewers ((login . "cy")))))
           ("/pulls/42/reviews\\'"
            . (((id . 1) (user (login . "bea")) (state . "APPROVED")))))))
    (github-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--gh-pr))
        (should (gp-e2e--gh-calls "GET" "/pulls/42/reviews\\'"))
        (let ((line (progn (gp-e2e--goto "👥")
                           (buffer-substring-no-properties (point) (line-end-position)))))
          (should (string-match-p "bea" line))
          (should (string-match-p "cy" line))
          (should-not (string-match-p "no reviewers" line)))))))

(ert-deftest gp-e2e-detail-shows-pipelines/bitbucket ()
  "The commit's pipelines load after the paint and render with their steps."
  (bitbucket-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--bb-pr))
      (should (gp-e2e--bb-calls "GET" "/repositories/acme/web-frontend/pipelines/?\\'"))
      (should (gp-e2e--bb-calls "GET" "/pipelines/[^/]+/steps"))
      (let ((text (gp-e2e--text)))
        (should (string-match-p "Pipelines (2 for this commit)" text))
        (should (string-match-p "#42" text))))))

(ert-deftest gp-e2e-detail-refresh-refetches-and-redraws/bitbucket ()
  "`g' re-fetches the PR and comments, bypassing the cache, and redraws."
  (let* ((title "Add feature toggle for the new widget")
         (bitbucket-mock-overrides
          `(("/pullrequests/180\\'"
             . ,(lambda (&rest _) (gp-e2e--bb-full-pr `(title . ,title)))))))
    (bitbucket-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--bb-pr))
        (let ((pr-gets (length (gp-e2e--bb-calls "GET" "/pullrequests/180\\'")))
              (comment-gets (length (gp-e2e--bb-calls "GET" "/pullrequests/180/comments\\'")))
              (diff-gets (length (gp-e2e--bb-calls "GET" "/pullrequests/180/diff\\'"))))
          (should (= 1 diff-gets))
          (setq title "Retitled on the web")
          (gp-e2e--key "g")
          ;; the diff is cached by commit; `g' must bypass that cache
          (should (= 2 (length (gp-e2e--bb-calls "GET" "/pullrequests/180/diff\\'"))))
          (should (> (length (gp-e2e--bb-calls "GET" "/pullrequests/180\\'")) pr-gets))
          (should (> (length (gp-e2e--bb-calls "GET" "/pullrequests/180/comments\\'"))
                     comment-gets))
          (should (string-match-p "#180  Retitled on the web" (gp-e2e--text))))))))

(ert-deftest gp-e2e-detail-refresh-refetches-and-redraws/github ()
  "`g' re-fetches the PR and its comments and shows what changed."
  (let* ((extra nil)
         (github-mock-overrides
          `(("/issues/42/comments\\'"
             . ,(lambda (method &rest _)
                  (when (equal method "GET")
                    (cons github-mock--issue-comment extra)))))))
    (github-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--gh-pr))
        (should-not (string-match-p "My own note" (gp-e2e--text)))
        (setq extra (list gp-e2e--gh-own-comment))
        (gp-e2e--key "g")
        (should (= 2 (length (gp-e2e--gh-calls "GET" "/issues/42/comments\\'"))))
        (should (string-match-p "Comments (3)" (gp-e2e--text)))
        (should (string-match-p "My own note on the toggle" (gp-e2e--text)))))))

;;;; Browsing and pipelines ----------------------------------------------------

(defmacro gp-e2e--capturing-browse (var &rest body)
  "Run BODY with `browse-url' recording each URL into VAR (newest first)."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'browse-url)
              (lambda (url &rest _) (push url ,var))))
     ,@body))

(ert-deftest gp-e2e-detail-browse-pipeline-step-or-pr/bitbucket ()
  "`w' opens the pipeline at point, the step at point, else the PR."
  (bitbucket-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--bb-pr))
      (let (opened)
        (gp-e2e--capturing-browse opened
          (gp-e2e--goto "Pipeline #42")
          (gp-e2e--key "w")
          (gp-e2e--goto "Deploy to DEV")
          (gp-e2e--key "w")
          (goto-char (point-min))
          (gp-e2e--key "w"))
        (should (equal (reverse opened)
                       '("https://bitbucket.org/acme/web-frontend/pipelines/results/42"
                         "https://bitbucket.org/acme/web-frontend/pipelines/results/42/steps/{step-2}"
                         "https://bitbucket.org/acme/web-frontend/pull-requests/180")))))))

(ert-deftest gp-e2e-detail-browse-comment/github ()
  "`w' on a comment opens that comment's own URL."
  (github-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--gh-pr))
      (let (opened)
        (gp-e2e--capturing-browse opened
          (gp-e2e--goto "General comment")
          (gp-e2e--key "w"))
        (should (equal opened '("https://github.com/acme/web/pull/42#issuecomment-9001")))))))

(ert-deftest gp-e2e-detail-stop-pipeline/bitbucket ()
  "`s' on a running pipeline asks, then stops that pipeline by uuid."
  (bitbucket-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--bb-pr))
      (gp-e2e--goto "Pipeline #42")
      (cl-letf (((symbol-function 'yes-or-no-p) #'ignore))
        (gp-e2e--key "s"))
      (should-not (gp-e2e--bb-calls "POST" "/stopPipeline\\'"))
      (gp-e2e--goto "Pipeline #42")
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (gp-e2e--key "s"))
      (should (equal (mapcar #'cadr (gp-e2e--bb-calls "POST" "/stopPipeline\\'"))
                     '("/repositories/acme/web-frontend/pipelines/{pipeline-aaaa-1111}/stopPipeline"))))))

(ert-deftest gp-e2e-detail-trigger-pipeline/bitbucket ()
  "`T' off a step triggers a new run of the PR's branch after asking."
  (bitbucket-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--bb-pr))
      (gp-e2e--goto "Pipeline #42")
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (gp-e2e--key "T"))
      (let ((posts (gp-e2e--bb-calls "POST" "/pipelines/?\\'")))
        (should (= 1 (length posts)))
        (should (equal (let-alist (nth 3 (car posts)) .target.ref_name)
                       "feature/widget-toggle"))))))

(ert-deftest gp-e2e-detail-run-waiting-manual-step/bitbucket ()
  "`T' on a waiting manual step runs it, here as a new run of its pipeline.
The step, not the pipeline, decides: no trigger prompt is asked."
  (bitbucket-mock-with-service
    (gp-e2e--with-env
      (let ((gp-pipeline-deploy-script nil))
        (gp-e2e--open (gp-e2e--bb-pr))
        (gp-e2e--goto "Deploy to LIVE")
        (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?n "new run")))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest _) (error "Asked to trigger instead"))))
          (gp-e2e--key "T"))
        (let ((posts (gp-e2e--bb-calls "POST" "/pipelines/?\\'")))
          (should (= 1 (length posts)))
          (should (equal (let-alist (nth 3 (car posts)) .target.ref_name)
                         "feature/widget-toggle"))
          ;; re-run with the pipeline's own selector
          (should (equal (let-alist (nth 3 (car posts)) .target.selector)
                         '((type . "branches") (pattern . "feature/widget-toggle")))))))))

;;;; Comments ----------------------------------------------------------------------

(ert-deftest gp-e2e-detail-reply/bitbucket ()
  "`R' on a comment composes a reply posted with that comment as parent."
  (bitbucket-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--bb-pr))
      (let ((detail (current-buffer))
            (gets (length (gp-e2e--bb-calls "GET" "/pullrequests/180/comments\\'"))))
        (gp-e2e--goto "Follow-up: please confirm")
        (gp-e2e--key "R")
        (should (gp-e2e--compose-buffer))
        (gp-e2e--submit-compose "Confirmed: it defaults to off.")
        (set-buffer detail)
        (let ((post (car (gp-e2e--bb-calls "POST" "/pullrequests/180/comments\\'"))))
          (should post)
          (should (equal (let-alist (nth 3 post) .content.raw) "Confirmed: it defaults to off."))
          (should (equal (let-alist (nth 3 post) .parent.id) 808619475)))
        ;; the detail buffer re-fetched its comments
        (should (> (length (gp-e2e--bb-calls "GET" "/pullrequests/180/comments\\'")) gets))))))

(ert-deftest gp-e2e-detail-reply-to-inline-comment/github ()
  "`R' on an inline review comment posts to that comment's replies."
  (let ((github-mock-overrides
         '(("/pulls/42/comments/9002/replies\\'"
            . ((id . 9100) (body . "Done") (user (login . "ada"))
               (created_at . "2026-01-01T00:30:00Z"))))))
    (github-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--gh-pr))
        (gp-e2e--goto "Inline comment")
        (gp-e2e--key "R")
        (gp-e2e--submit-compose "Done")
        (should (equal (mapcar (lambda (c) (nth 3 c))
                               (gp-e2e--gh-calls "POST" "/replies\\'"))
                       '(((body . "Done")))))))))

(ert-deftest gp-e2e-detail-general-comment/bitbucket ()
  "`C' composes a general comment: no parent, no inline anchor."
  (bitbucket-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--bb-pr))
      (gp-e2e--key "C")
      (gp-e2e--submit-compose "Ship it once CI is green.")
      (let ((posts (gp-e2e--bb-calls "POST" "/pullrequests/180/comments\\'")))
        (should (= 1 (length posts)))
        (should (equal (let-alist (nth 3 (car posts)) .content.raw)
                       "Ship it once CI is green."))
        (should-not (assq 'parent (nth 3 (car posts))))
        (should-not (assq 'inline (nth 3 (car posts))))))))

(ert-deftest gp-e2e-detail-general-comment/github ()
  "`C' posts a general comment to the PR's issue thread."
  (github-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--gh-pr))
      (gp-e2e--key "C")
      (gp-e2e--submit-compose "Ship it.")
      (should (equal (mapcar (lambda (c) (list (nth 1 c) (nth 3 c)))
                             (gp-e2e--gh-calls "POST" "comments\\'"))
                     '(("/repos/acme/web/issues/42/comments" ((body . "Ship it.")))))))))

(ert-deftest gp-e2e-detail-empty-general-comment-sends-nothing/bitbucket ()
  "Submitting an empty compose buffer is refused and posts nothing."
  (bitbucket-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--bb-pr))
      (gp-e2e--key "C")
      (let ((buf (gp-e2e--compose-buffer)))
        (set-buffer buf)
        (should-error (gp-e2e--key "C-c C-c") :type 'user-error)
        (should (buffer-live-p buf)))
      (should-not (gp-e2e--bb-calls "POST")))))

(ert-deftest gp-e2e-detail-resolve-then-reopen/bitbucket ()
  "`X' resolves an open comment; on a resolved one it reopens it."
  (let* ((resolved nil)
         (bitbucket-mock-overrides
          `(("/pullrequests/180/comments\\'"
             . ,(lambda (method &rest _)
                  (when (equal method "GET")
                    (let ((page (bitbucket-mock--fixture "pr-comments.json")))
                      `((values
                         . ,(mapcar (lambda (c)
                                      (if (and resolved (equal (alist-get 'id c) 808631489))
                                          (cons '(resolution (user (display_name . "Ada"))) c)
                                        c))
                                    (alist-get 'values page))))))))
            ("/comments/808631489/resolve\\'"
             . ,(lambda (method &rest _)
                  (setq resolved (equal method "POST"))
                  '((ok . t)))))))
    (bitbucket-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--bb-pr))
        (gp-e2e--goto "mapToOption")
        (gp-e2e--key "X")
        (should (equal (mapcar #'car (gp-e2e--bb-calls nil "/resolve\\'"))
                       '("POST")))
        (should (string-match-p "✓ resolved" (gp-e2e--text)))
        ;; the resolved thread starts folded only on first render: the
        ;; heading is still there to act on
        (gp-e2e--goto "options-dialog.component.ts:105")
        (gp-e2e--key "X")
        (should (equal (mapcar #'car (gp-e2e--bb-calls nil "/resolve\\'"))
                       '("POST" "DELETE")))
        (should-not (string-match-p "✓ resolved" (gp-e2e--text)))))))

(defun gp-e2e--gh-graphql-with-thread (query &optional variables)
  "Network stand-in: the mock's GraphQL plus one review thread holding 9002.
The shared mock answers every reviewThreads query with no threads."
  (if (string-match-p "reviewThreads" query)
      (progn (push (list query variables) github-mock-graphql-calls)
             '((repository
                (pullRequest
                 (reviewThreads
                  (nodes ((id . "RT_1") (comments (nodes ((databaseId . 9002)))))))))))
    (github-mock-graphql query variables)))

(ert-deftest gp-e2e-detail-resolve/github ()
  "`X' on an inline comment resolves its review thread by thread id."
  (github-mock-with-service
    (cl-letf (((symbol-function 'github-graphql-request) #'gp-e2e--gh-graphql-with-thread))
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--gh-pr))
        (gp-e2e--goto "Inline comment")
        (gp-e2e--key "X")
        (let ((mutation (seq-find (lambda (c) (string-match-p "resolveReviewThread" (car c)))
                                  github-mock-graphql-calls)))
          (should mutation)
          (should-not (string-match-p "unresolve" (car mutation)))
          (should (equal (cadr mutation) '((id . "RT_1")))))))))

(ert-deftest gp-e2e-detail-resolve-general-comment-refused/github ()
  "A general GitHub comment has no thread: `X' refuses and sends nothing."
  (github-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--gh-pr))
      (gp-e2e--goto "General comment")
      (should-error (gp-e2e--key "X") :type 'user-error)
      (should-not (seq-find (lambda (c) (string-match-p "mutation" (car c)))
                            github-mock-graphql-calls)))))

(ert-deftest gp-e2e-detail-edit-own-comment/bitbucket ()
  "`e' on your own comment opens it prefilled and PUTs the new text."
  (bitbucket-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--bb-pr))
      (gp-e2e--goto "Thanks, addressed the constant")
      (gp-e2e--key "e")
      (should (string-match-p "Thanks, addressed the constant"
                              (with-current-buffer (gp-e2e--compose-buffer) (buffer-string))))
      (gp-e2e--submit-compose "Thanks, all addressed.")
      (should (equal (mapcar (lambda (c) (list (nth 1 c) (nth 3 c)))
                             (gp-e2e--bb-calls "PUT"))
                     '(("/repositories/acme/web-frontend/pullrequests/180/comments/808625667"
                        ((content (raw . "Thanks, all addressed."))))))))))

(ert-deftest gp-e2e-detail-edit-foreign-comment-refused/bitbucket ()
  "`e' on someone else's comment refuses without opening a compose buffer."
  (bitbucket-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--bb-pr))
      (gp-e2e--goto "Follow-up: please confirm")
      (should-error (gp-e2e--key "e") :type 'user-error)
      (should-not (gp-e2e--compose-buffer))
      (should-not (gp-e2e--bb-calls "PUT")))))

(ert-deftest gp-e2e-detail-edit-own-comment/github ()
  "`e' on your own general comment PATCHes the issue comment."
  (let ((github-mock-overrides
         (list (gp-e2e--gh-own-comments-override)
               (cons "/issues/comments/9010\\'"
                     (lambda (method _path _params data)
                       (when (equal method "PATCH")
                         (append data (cl-remove 'body gp-e2e--gh-own-comment :key #'car))))))))
    (github-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--gh-pr))
        (gp-e2e--goto "My own note")
        (gp-e2e--key "e")
        (gp-e2e--submit-compose "My revised note")
        (should (equal (mapcar (lambda (c) (list (nth 1 c) (nth 3 c)))
                               (gp-e2e--gh-calls "PATCH" "/issues/comments/"))
                       '(("/repos/acme/web/issues/comments/9010" ((body . "My revised note"))))))))))

(ert-deftest gp-e2e-detail-delete-own-comment/bitbucket ()
  "`K' asks first; yes DELETEs your comment, no sends nothing."
  (bitbucket-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--bb-pr))
      (gp-e2e--goto "Thanks, addressed the constant")
      (cl-letf (((symbol-function 'yes-or-no-p) #'ignore))
        (gp-e2e--key "K"))
      (should-not (gp-e2e--bb-calls "DELETE"))
      (gp-e2e--goto "Thanks, addressed the constant")
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (gp-e2e--key "K"))
      (should (equal (mapcar #'cadr (gp-e2e--bb-calls "DELETE"))
                     '("/repositories/acme/web-frontend/pullrequests/180/comments/808625667"))))))

(ert-deftest gp-e2e-detail-delete-foreign-comment-refused/bitbucket ()
  "`K' on someone else's comment refuses unless deleting others is granted."
  (bitbucket-mock-with-service
    (gp-e2e--with-env
      (let ((gp-comment-delete-others nil))
        (gp-e2e--open (gp-e2e--bb-pr))
        (gp-e2e--goto "Follow-up: please confirm")
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (should-error (gp-e2e--key "K") :type 'user-error))
        (should-not (gp-e2e--bb-calls "DELETE"))))))

(ert-deftest gp-e2e-detail-delete-own-comment/github ()
  "`K' on your own general comment falls back to the issue-comment endpoint."
  (let ((github-mock-overrides
         (list (gp-e2e--gh-own-comments-override)
               (cons "/issues/comments/9010\\'"
                     (lambda (method &rest _) (equal method "DELETE"))))))
    (github-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--gh-pr))
        (gp-e2e--goto "My own note")
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (gp-e2e--key "K"))
        (should (equal (mapcar #'cadr (gp-e2e--gh-calls "DELETE"))
                       '("/repos/acme/web/pulls/comments/9010"
                         "/repos/acme/web/issues/comments/9010")))))))

;;;; Reactions (GitHub) ----------------------------------------------------------

(ert-deftest gp-e2e-detail-like-toggles-reaction/github ()
  "`+' adds your 👍 to the comment at point; pressing it again removes it."
  (github-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--gh-pr))
      (gp-e2e--goto "General comment")
      (gp-e2e--key "+")
      (should (equal (mapcar (lambda (c) (list (nth 1 c) (nth 3 c)))
                             (gp-e2e--gh-calls "POST" "/reactions\\'"))
                     '(("/repos/acme/web/issues/comments/9001/reactions"
                        ((content . "+1"))))))
      (should (string-match-p "👍" (gp-e2e--text)))
      (gp-e2e--goto "General comment")
      (gp-e2e--key "+")
      (should (equal (mapcar #'cadr (gp-e2e--gh-calls "DELETE" "/reactions/"))
                     '("/repos/acme/web/issues/comments/9001/reactions/5001")))
      (should-not github-mock--reactions))))

(ert-deftest gp-e2e-detail-react-picks-a-reaction/github ()
  "`!' offers the reactions and posts the picked one on an inline comment."
  (github-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--gh-pr))
      (gp-e2e--goto "Inline comment")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (seq-find (lambda (k) (string-suffix-p " heart" k))
                             (mapcar #'car table)))))
        (gp-e2e--key "!"))
      (should (equal (mapcar (lambda (c) (list (nth 1 c) (nth 3 c)))
                             (gp-e2e--gh-calls "POST" "/reactions\\'"))
                     '(("/repos/acme/web/pulls/comments/9002/reactions"
                        ((content . "heart")))))))))

(ert-deftest gp-e2e-detail-like-refused/bitbucket ()
  "Bitbucket has no reactions: `+' refuses and sends nothing."
  (bitbucket-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--bb-pr))
      (gp-e2e--goto "Follow-up: please confirm")
      (should-error (gp-e2e--key "+") :type 'user-error)
      (should-not (gp-e2e--bb-calls "POST")))))

;;;; Reviews -----------------------------------------------------------------------

(ert-deftest gp-e2e-detail-approve-then-unapprove/bitbucket ()
  "`a' approves someone else's PR; once the PR reports it, `a' retracts."
  (let* ((approved nil)
         (bitbucket-mock-overrides
          `(("/pullrequests/180\\'"
             . ,(lambda (&rest _)
                  (gp-e2e--bb-full-pr
                   `(author ,@gp-e2e--bb-grace)
                   `(participants
                     ((role . "REVIEWER") (approved . ,(if approved t :json-false))
                      (state . ,(and approved "approved"))
                      (user (display_name . "Ada Lovelace") (uuid . ,gp-e2e--bb-me)))))))
            ("/pullrequests/180/approve\\'"
             . ,(lambda (method &rest _)
                  (setq approved (equal method "POST"))
                  '((ok . t)))))))
    (bitbucket-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--bb-pr))
        (gp-e2e--key "a")
        (should (equal (mapcar #'car (gp-e2e--bb-calls nil "/approve\\'")) '("POST")))
        (should approved)
        (gp-e2e--key "a")
        (should (equal (mapcar #'car (gp-e2e--bb-calls nil "/approve\\'")) '("POST" "DELETE")))
        (should-not approved)))))

(ert-deftest gp-e2e-detail-approve-own-pr-refused/bitbucket ()
  "`a' on your own PR refuses and sends nothing."
  (bitbucket-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--bb-pr))
      (should-error (gp-e2e--key "a") :type 'user-error)
      (should-not (gp-e2e--bb-calls "POST" "/approve\\'")))))

(defun gp-e2e--gh-reviews-override (reviews-symbol)
  "Override for /pulls/42/reviews keeping submitted reviews in REVIEWS-SYMBOL."
  (cons "/pulls/42/reviews\\'"
        (lambda (method _path _params data)
          (if (equal method "POST")
              (let ((r `((id . 7001) (user (login . "ada"))
                         (state . ,(pcase (alist-get 'event data)
                                     ("APPROVE" "APPROVED")
                                     ("REQUEST_CHANGES" "CHANGES_REQUESTED")
                                     (e e))))))
                (set reviews-symbol (append (symbol-value reviews-symbol) (list r)))
                r)
            (symbol-value reviews-symbol)))))

(defvar gp-e2e--gh-reviews nil
  "Reviews the GitHub reviews override (`gp-e2e--gh-reviews-override') holds.")

(ert-deftest gp-e2e-detail-approve-then-dismiss/github ()
  "`a' submits an APPROVE review; again, it dismisses it with a reason."
  (let* ((gp-e2e--gh-reviews nil)
         (github-mock-overrides
          (list (cons "/pulls/42\\'" (gp-e2e--gh-full-pr '(user (login . "bea"))))
                (gp-e2e--gh-reviews-override 'gp-e2e--gh-reviews)
                (cons "/reviews/7001/dismissals\\'" '((state . "DISMISSED"))))))
    (github-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--gh-pr))
        (gp-e2e--key "a")
        (should (equal (mapcar (lambda (c) (nth 3 c)) (gp-e2e--gh-calls "POST" "/reviews\\'"))
                       '(((event . "APPROVE")))))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Pushed again")))
          (gp-e2e--key "a"))
        (should (equal (mapcar (lambda (c) (list (nth 1 c) (nth 3 c)))
                               (gp-e2e--gh-calls "PUT" "/dismissals\\'"))
                       '(("/repos/acme/web/pulls/42/reviews/7001/dismissals"
                          ((message . "Pushed again"))))))))))

(ert-deftest gp-e2e-detail-request-changes/bitbucket ()
  "`c' requests changes on someone else's PR."
  (let ((bitbucket-mock-overrides
         `(("/pullrequests/180\\'" . ,(gp-e2e--bb-full-pr `(author ,@gp-e2e--bb-grace))))))
    (bitbucket-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--bb-pr))
        (gp-e2e--key "c")
        (should (equal (mapcar #'cadr (gp-e2e--bb-calls "POST" "/request-changes\\'"))
                       '("/repositories/acme/web-frontend/pullrequests/180/request-changes")))))))

(ert-deftest gp-e2e-detail-request-changes/github ()
  "`c' submits a REQUEST_CHANGES review."
  (let* ((gp-e2e--gh-reviews nil)
         (github-mock-overrides
          (list (cons "/pulls/42\\'" (gp-e2e--gh-full-pr '(user (login . "bea"))))
                (gp-e2e--gh-reviews-override 'gp-e2e--gh-reviews))))
    (github-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--gh-pr))
        (gp-e2e--key "c")
        (should (equal (mapcar (lambda (c) (nth 3 c)) (gp-e2e--gh-calls "POST" "/reviews\\'"))
                       '(((event . "REQUEST_CHANGES") (body . "Changes requested.")))))))))

;;;; PR-level actions ------------------------------------------------------------

(ert-deftest gp-e2e-detail-merge/bitbucket ()
  "`M' merges after confirmation; declining sends nothing."
  (bitbucket-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--bb-pr))
      (cl-letf (((symbol-function 'yes-or-no-p) #'ignore))
        (should-error (gp-e2e--key "M") :type 'user-error))
      (should-not (gp-e2e--bb-calls "POST" "/merge\\'"))
      (let ((gp-merge-close-source-branch t))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (gp-e2e--key "M")))
      (let ((merges (gp-e2e--bb-calls "POST" "/merge\\'")))
        (should (equal (mapcar #'cadr merges)
                       '("/repositories/acme/web-frontend/pullrequests/180/merge")))
        (should (eq (alist-get 'close_source_branch (nth 3 (car merges))) t))))))

(ert-deftest gp-e2e-detail-merge/github ()
  "`M' merges through GitHub's merge endpoint after confirmation."
  (github-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--gh-pr))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (gp-e2e--key "M"))
      (should (equal (mapcar #'cadr (gp-e2e--gh-calls "PUT" "/merge\\'"))
                     '("/repos/acme/web/pulls/42/merge"))))))

(ert-deftest gp-e2e-detail-toggle-draft/bitbucket ()
  "`D' on your open PR converts it to a draft, keeping its title."
  (let* ((gp-e2e--pr-state (gp-e2e--bb-full-pr))
         (bitbucket-mock-overrides
          `(("/pullrequests/180\\'" . ,(gp-e2e--stateful 'gp-e2e--pr-state)))))
    (bitbucket-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--bb-pr))
        (gp-e2e--key "D")
        (should (equal (mapcar (lambda (c) (nth 3 c)) (gp-e2e--bb-calls "PUT"))
                       '(((title . "Add feature toggle for the new widget") (draft . t)))))
        (should (string-match-p "Mark ready" (gp-e2e--text)))))))

(ert-deftest gp-e2e-detail-toggle-draft/github ()
  "`D' converts the PR to a draft through GraphQL, addressed by node id."
  (github-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--gh-pr))
      (gp-e2e--key "D")
      (let ((m (seq-find (lambda (c) (string-match-p "convertPullRequestToDraft" (car c)))
                         github-mock-graphql-calls)))
        (should m)
        (should (equal (cadr m) '((id . "PR_42"))))))))

(ert-deftest gp-e2e-detail-edit-title/bitbucket ()
  "`N' saves a changed title with the description resent, and redraws."
  (let* ((gp-e2e--pr-state (gp-e2e--bb-full-pr '(description . "Keep me")))
         (bitbucket-mock-overrides
          `(("/pullrequests/180\\'" . ,(gp-e2e--stateful 'gp-e2e--pr-state)))))
    (bitbucket-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--bb-pr))
        ;; unchanged: nothing is written
        (cl-letf (((symbol-function 'read-string) (lambda (_p initial &rest _) initial)))
          (gp-e2e--key "N"))
        (should-not (gp-e2e--bb-calls "PUT"))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "  Widget toggle  ")))
          (gp-e2e--key "N"))
        (should (equal (mapcar (lambda (c) (nth 3 c)) (gp-e2e--bb-calls "PUT"))
                       '(((title . "Widget toggle") (description . "Keep me")))))
        (should (string-match-p "#180  Widget toggle" (gp-e2e--text)))))))

(ert-deftest gp-e2e-detail-edit-title/github ()
  "`N' PATCHes just the title, and the redraw shows it."
  (let* ((gp-e2e--pr-state (gp-e2e--gh-full-pr))
         (github-mock-overrides
          `(("/pulls/42\\'" . ,(gp-e2e--stateful 'gp-e2e--pr-state)))))
    (github-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--gh-pr))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Widget toggle")))
          (gp-e2e--key "N"))
        (should (equal (mapcar (lambda (c) (nth 3 c)) (gp-e2e--gh-calls "PATCH"))
                       '(((title . "Widget toggle")))))
        (should (string-match-p "#42  Widget toggle" (gp-e2e--text)))))))

(ert-deftest gp-e2e-detail-edit-title-refuses-empty/bitbucket ()
  "`N' with a blank title refuses and writes nothing."
  (bitbucket-mock-with-service
    (gp-e2e--with-env
      (gp-e2e--open (gp-e2e--bb-pr))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   ")))
        (should-error (gp-e2e--key "N") :type 'user-error))
      (should-not (gp-e2e--bb-calls "PUT")))))

(ert-deftest gp-e2e-detail-edit-labels/github ()
  "`L' PUTs the whole chosen label set and the redraw shows it."
  (let* ((gp-e2e--pr-state (gp-e2e--gh-full-pr))
         (github-mock-overrides
          `(("/pulls/42\\'" . ,(gp-e2e--stateful 'gp-e2e--pr-state))
            ("/issues/42/labels\\'"
             . ,(lambda (method _path _params data)
                  (when (equal method "PUT")
                    (let ((set (mapcar (lambda (n) `((name . ,n) (color . "ededed")))
                                       (append (alist-get 'labels data) nil))))
                      (setq gp-e2e--pr-state
                            (cons (cons 'labels set)
                                  (assq-delete-all 'labels (copy-sequence gp-e2e--pr-state))))
                      set)))))))
    (github-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--gh-pr))
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (_prompt candidates &rest _)
                     ;; the repo's pool is offered, not just the PR's labels
                     (should (member "chore" candidates))
                     '("bug" "chore"))))
          (gp-e2e--key "L"))
        (should (equal (mapcar (lambda (c) (list (nth 1 c) (append (alist-get 'labels (nth 3 c)) nil)))
                               (gp-e2e--gh-calls "PUT" "/labels\\'"))
                       '(("/repos/acme/web/issues/42/labels" ("bug" "chore")))))
        (let ((line (progn (gp-e2e--goto "🏷")
                           (buffer-substring-no-properties (point) (line-end-position)))))
          (should (string-match-p "chore" line))
          (should-not (string-match-p "\\bui\\b" line)))))))

(ert-deftest gp-e2e-detail-edit-description/bitbucket ()
  "`E' opens the description prefilled, PUTs it verbatim, and redraws."
  (let* ((gp-e2e--pr-state (gp-e2e--bb-full-pr '(description . "Old text")))
         (bitbucket-mock-overrides
          `(("/pullrequests/180\\'" . ,(gp-e2e--stateful 'gp-e2e--pr-state)))))
    (bitbucket-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--bb-pr))
        (gp-e2e--key "E")
        (should (equal (with-current-buffer (gp-e2e--compose-buffer) (buffer-string))
                       "Old text"))
        ;; newlines kept verbatim: descriptions opt out of hard breaks
        (gp-e2e--submit-compose "| a | b |\n|---|---|\n| 1 | 2 |")
        (should (equal (mapcar (lambda (c) (nth 3 c)) (gp-e2e--bb-calls "PUT"))
                       '(((title . "Add feature toggle for the new widget")
                          (description . "| a | b |\n|---|---|\n| 1 | 2 |")))))
        (should (string-match-p "| 1 | 2 |" (gp-e2e--text)))
        (should-not (string-match-p "Old text" (gp-e2e--text)))))))

(ert-deftest gp-e2e-detail-edit-description/github ()
  "`E' PATCHes the PR body."
  (let* ((gp-e2e--pr-state (gp-e2e--gh-full-pr))
         (github-mock-overrides
          `(("/pulls/42\\'" . ,(gp-e2e--stateful 'gp-e2e--pr-state)))))
    (github-mock-with-service
      (gp-e2e--with-env
        (gp-e2e--open (gp-e2e--gh-pr))
        (gp-e2e--key "E")
        (gp-e2e--submit-compose "New body")
        (should (equal (mapcar (lambda (c) (nth 3 c)) (gp-e2e--gh-calls "PATCH"))
                       '(((body . "New body")))))
        (should (string-match-p "New body" (gp-e2e--text)))))))

(provide 'gp-e2e-detail-test)
;;; gp-e2e-detail-test.el ends here
