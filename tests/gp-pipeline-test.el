;;; gp-pipeline-test.el --- Tests for the pipelines detail section -*- lexical-binding: t; -*-

;;; Commentary:
;; Drives the pipeline renderer in a real buffer, checks status glyph /
;; duration formatting, the sorted fetch, and the section tree.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gp-pipeline)
(require 'gp-ui)
(require 'bitbucket-mock)
(require 'git-platform-bitbucket)
(require 'git-platform-github)
(require 'git-platform-mock)

;;;; Pure formatting -----------------------------------------------------------

(ert-deftest gp-test-pipeline-status-glyph ()
  (should (eq (cdr (gp-pipeline--status-glyph "IN_PROGRESS" nil))
              'gp-pipeline-spinner-face))
  (should (eq (cdr (gp-pipeline--status-glyph "COMPLETED" "SUCCESSFUL"))
              'gp-pipeline-success-face))
  (should (eq (cdr (gp-pipeline--status-glyph "COMPLETED" "FAILED"))
              'gp-pipeline-failed-face))
  (should (eq (cdr (gp-pipeline--status-glyph "COMPLETED" "STOPPED"))
              'gp-pipeline-stopped-face)))

(ert-deftest gp-test-pipeline-status-glyph-paused-and-step ()
  "Paused-at-manual-gate pipelines and in-progress entries get own glyphs."
  ;; a paused pipeline (state IN_PROGRESS, stage PAUSED) is not \"running\"
  (should (equal (gp-pipeline--status-glyph "IN_PROGRESS" "PAUSED")
                 '("⏸" . gp-pipeline-paused-face)))
  ;; in-progress renders the animated spinner, marked for repainting
  (dolist (step '(nil step))
    (let ((glyph (car (gp-pipeline--status-glyph "IN_PROGRESS" nil step))))
      (should (member (substring-no-properties glyph)
                      (append gp-pipeline-spinner-frames nil)))
      (should (get-text-property 0 'gp-pipeline-spinner glyph))))
  ;; finished glyphs are unaffected by the step flag
  (should (equal (car (gp-pipeline--status-glyph "COMPLETED" "SUCCESSFUL" 'step)) "✔"))
  (gp-pipeline--spinner-stop))

(ert-deftest gp-test-pipeline-spinner-frames-uniform-width ()
  "Every spinner frame is one character wide, so animating never reflows."
  (mapc (lambda (f)
          (should (= (length f) 1))
          (should (= (string-width f) (string-width (aref gp-pipeline-spinner-frames 0)))))
        gp-pipeline-spinner-frames))

(ert-deftest gp-test-pipeline-spinner-survives-rerender-gap ()
  "A tick landing in a rerender's erase window must not stop the animation.
Regression: the timer stopped on the FIRST spinner-less tick, and only a
render restarted it -- but the pipeline poll skips the render while data
is unchanged, so the glyph froze mid-run."
  (unwind-protect
      (let ((buf (get-buffer-create " *gp-spinner-test*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (magit-section-mode)
                (let ((inhibit-read-only t))
                  (insert "x " (gp-pipeline--spinner-glyph) "\n")))
              (gp-pipeline--spinner-ensure)
              ;; buffer erased mid-rerender: a few empty ticks
              (with-current-buffer buf
                (let ((inhibit-read-only t)) (erase-buffer)))
              (dotimes (_ 3) (gp-pipeline--spinner-tick))
              (should (timerp gp-pipeline--spinner-timer))
              ;; glyph reappears -- grace window resets
              (with-current-buffer buf
                (let ((inhibit-read-only t))
                  (insert "x " (gp-pipeline--spinner-glyph) "\n")))
              (gp-pipeline--spinner-tick)
              (should (timerp gp-pipeline--spinner-timer))
              (should (= gp-pipeline--spinner-misses 0)))
          (kill-buffer buf)))
    (gp-pipeline--spinner-stop)))

(ert-deftest gp-test-pipeline-spinner-stops-when-really-gone ()
  "The timer still retires once the grace window is exhausted."
  (unwind-protect
      (progn
        (gp-pipeline--spinner-ensure)
        (should (timerp gp-pipeline--spinner-timer))
        (dotimes (_ (1+ gp-pipeline--spinner-grace-ticks))
          (gp-pipeline--spinner-tick))
        (should (null gp-pipeline--spinner-timer)))
    (gp-pipeline--spinner-stop)))

(ert-deftest gp-test-pipeline-spinner-repaint-in-place ()
  "Repainting swaps the spinner char only, leaving the rest of the line intact."
  (with-temp-buffer
    (insert "  " (gp-pipeline--spinner-glyph) " build  12s\n")
    (let ((before (buffer-size)))
      (gp-pipeline--spinner-repaint-buffer "⠿")
      (should (= (buffer-size) before))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "  ⠿ build  12s\n"))
      ;; still marked, so the next tick finds it again
      (should (gp-pipeline--spinner-repaint-buffer "⠋"))
      (should (= (buffer-size) before))))
  (gp-pipeline--spinner-stop))

(ert-deftest gp-test-pipeline-web-url ()
  "Web URL deep-links to the pipeline run and optionally the step."
  (let ((p '((build_number . 728)))
        (step '((uuid . "{step-3}"))))
    (should (equal (bitbucket-pipeline-web-url "acme/x" p)
                   "https://bitbucket.org/acme/x/pipelines/results/728"))
    (should (equal (bitbucket-pipeline-web-url "acme/x" p step)
                   "https://bitbucket.org/acme/x/pipelines/results/728/steps/{step-3}"))))

(ert-deftest gp-test-pipeline-label-manual-gate ()
  "A running pipeline stalled at an open manual gate shows ⏸, not ▶.
Bitbucket reports stage RUNNING for these, so the gate is read off
the steps."
  (let* ((p '((build_number . 728)
              (state (name . "IN_PROGRESS") (stage (name . "RUNNING")))))
         (done '((state (name . "COMPLETED") (result (name . "SUCCESSFUL")))
                 (trigger (type . "pipeline_step_trigger_automatic"))))
         (gate '((state (name . "PENDING") (stage (name . "PAUSED")))
                 (trigger (type . "pipeline_step_trigger_manual"))))
         (running '((state (name . "IN_PROGRESS"))
                    (trigger (type . "pipeline_step_trigger_automatic")))))
    ;; done + open gate, nothing executing: paused
    (should (gp-pipeline--manual-gate-open-p p (list done gate)))
    (let ((label (substring-no-properties
                  (gp-pipeline--label p (list done gate)))))
      (should (string-prefix-p "⏸" label))
      (should (string-match-p "manual gate open" label)))
    ;; a step is actually executing: still the running (spinner) glyph
    (should-not (gp-pipeline--manual-gate-open-p p (list running gate)))
    (let ((frames (append gp-pipeline-spinner-frames nil)))
      (should (member (substring (substring-no-properties
                                  (gp-pipeline--label p (list running gate)))
                                 0 1)
                      frames))
      ;; no steps at hand: unchanged running label
      (should (member (substring (substring-no-properties
                                  (gp-pipeline--label p))
                                 0 1)
                      frames)))
    (gp-pipeline--spinner-stop)))

(ert-deftest gp-test-pipeline-format-duration ()
  (should (equal (gp-pipeline--format-duration '((duration_in_seconds . 5))) "5s"))
  (should (equal (gp-pipeline--format-duration '((duration_in_seconds . 95))) "1m35s"))
  (should (equal (gp-pipeline--format-duration '((duration_in_seconds . 3700))) "1h01m"))
  (should (equal (gp-pipeline--format-duration '((name . "x"))) "")))

(ert-deftest gp-test-pipeline-format-duration-running-step ()
  ;; A running step has no duration_in_seconds yet; elapsed time comes
  ;; from started_on.
  (let* ((started (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                                      (time-subtract (current-time) 90)))
         (step `((state (name . "IN_PROGRESS"))
                 (started_on . ,started))))
    (should (string-match-p "\\`1m3[01]s\\'"
                            (gp-pipeline--format-duration step)))
    ;; live elapsed wins over a stale zero duration while running
    (should (string-match-p "\\`1m3[01]s\\'"
                            (gp-pipeline--format-duration
                             (cons '(duration_in_seconds . 0) step))))
    ;; a running step without started_on still renders nothing extra
    (should (equal (gp-pipeline--format-duration
                    '((state (name . "IN_PROGRESS"))))
                   ""))))

(ert-deftest gp-test-pipeline-label-has-number-and-status ()
  (let ((p '((build_number . 42) (state (name . "COMPLETED")
                                         (result (name . "SUCCESSFUL"))))))
    (should (string-match-p "#42"
                            (substring-no-properties (gp-pipeline--label p))))
    (should (string-match-p "SUCCESSFUL"
                            (substring-no-properties (gp-pipeline--label p))))))

;;;; Fetch (sorted, with steps) ------------------------------------------------

(ert-deftest gp-test-pipeline-fetch-current-vs-recent ()
  (bitbucket-mock-with-service
    (let* ((pr (car (alist-get 'values (bitbucket-mock--fixture "workspace-prs.json"))))
           (result (gp-pipeline-fetch-for-pr pr))
           (current (plist-get result :current))
           (recent (plist-get result :recent)))
      ;; two pipelines on the PR head commit -> :current (with steps)
      (should (= (length current) 2))
      (should (cl-every (lambda (pp) (listp (cdr pp))) current))
      (should (= (length (cdr (car current))) 3))   ;; steps fetched
      ;; the one pipeline on an older commit -> :recent (PIPELINE . SUMMARY)
      (should (= (length recent) 1))
      (should (consp (car recent)))
      ;; the summary is the first line of the mocked commit message
      (should (equal (cdr (car recent)) "Fix the widget toggle")))))

(ert-deftest gp-test-pipeline-fetch-async-matches-sync ()
  "The async fetch produces exactly the shape the synchronous one does.
The detail view polls the async twin every few seconds; if the two
ever diverge, the rendered section silently changes with them."
  (bitbucket-mock-with-service
    (let* ((pr (car (alist-get 'values (bitbucket-mock--fixture "workspace-prs.json"))))
           (gp-cache-ttl 0)             ;; no warm commit-message cache either way
           (sync (gp-pipeline-fetch-for-pr pr))
           (async 'unset))
      (gp-pipeline-fetch-for-pr-async pr (lambda (d) (setq async d)))
      (should-not (eq async 'unset))    ;; the callback actually ran
      (should (equal async sync)))))

(ert-deftest gp-test-pipeline-fetch-async-fans-out-steps-and-summaries ()
  "Async fetch resolves steps per current run and a summary per recent run."
  (bitbucket-mock-with-service
    (let* ((pr (car (alist-get 'values (bitbucket-mock--fixture "workspace-prs.json"))))
           (gp-cache-ttl 0)
           (data nil))
      (gp-pipeline-fetch-for-pr-async pr (lambda (d) (setq data d)))
      (let ((current (plist-get data :current))
            (recent (plist-get data :recent)))
        (should (= (length current) 2))
        ;; every current run got its steps fanned out and folded back in
        (should (cl-every (lambda (pp) (consp pp)) current))
        (should (= (length (cdr (car current))) 3))
        ;; the recent run got its commit message resolved to a summary
        (should (= (length recent) 1))
        (should (equal (cdr (car recent)) "Fix the widget toggle"))))))

(ert-deftest gp-test-pipeline-fetch-async-reports-nil-on-failed-branch-fetch ()
  "A failed branch fetch calls back with nil (caller keeps its stale data)."
  (bitbucket-mock-with-service
    (let ((pr (car (alist-get 'values (bitbucket-mock--fixture "workspace-prs.json"))))
          (called nil)
          (data 'unset))
      (cl-letf (((symbol-function 'bitbucket-api-paged-async)
                 (lambda (_path &optional _params callback _max)
                   (funcall callback nil nil))))
        (gp-pipeline-fetch-for-pr-async pr (lambda (d) (setq called t data d))))
      (should called)
      (should (null data)))))

(ert-deftest gp-test-pipeline-fetch-async-fires-callback-once ()
  "The fan-out counter must settle exactly once, not once per sub-fetch."
  (bitbucket-mock-with-service
    (let* ((pr (car (alist-get 'values (bitbucket-mock--fixture "workspace-prs.json"))))
           (gp-cache-ttl 0)
           (calls 0))
      (gp-pipeline-fetch-for-pr-async pr (lambda (_) (setq calls (1+ calls))))
      (should (= calls 1)))))

(ert-deftest gp-test-commit-summary-first-line ()
  (should (equal (gp-commit-summary "first line\nsecond") "first line"))
  (should (equal (gp-commit-summary "  spaced  \nx") "spaced"))
  (should (equal (gp-commit-summary nil) "")))

;;;; Rendering -----------------------------------------------------------------

(defun gp-test--render-pipelines (pipelines)
  "Render PIPELINES into a temp detail buffer; return its text."
  (with-temp-buffer
    (gp-detail-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (gp-root)
        (gp--insert-pipelines pipelines)))
    (substring-no-properties (buffer-string))))

(ert-deftest gp-test-pipeline-step-rerun-hint-names-its-key ()
  "A rerunnable step advertises the key actually bound to the rerun command.
Bitbucket steps are never rerunnable, so the mock fixtures cannot
cover this hint -- which is how it went stale once already."
  (cl-letf (((symbol-function 'gp-pipeline-step-rerunnable-p) (lambda (_) t))
            ((symbol-function 'gp-pipeline-step-runnable-manual-p) (lambda (_) nil))
            ((symbol-function 'gp-pipeline-step-manual-p) (lambda (_) nil)))
    (with-temp-buffer
      (gp-detail-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (gp-root)
          (gp-pipeline--insert-step '((name . "Deploy")))))
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "\\[rerun ▸ P\\]" text))
        (should (eq (lookup-key gp-detail-mode-map "P")
                    #'gp-detail-pipeline-rerun-step))))))

(ert-deftest gp-test-pipeline-render-section ()
  (bitbucket-mock-with-service
    (let* ((pr (car (alist-get 'values (bitbucket-mock--fixture "workspace-prs.json"))))
           (pipelines (gp-pipeline-fetch-for-pr pr))
           (text (gp-test--render-pipelines pipelines)))
      (should (string-match-p "Pipelines (2 for this commit)" text))
      (should (string-match-p "Pipeline #42" text))
      ;; step names from the fixture appear
      (should (string-match-p "Build and test" text))
      (should (string-match-p "Deploy to LIVE" text))
      ;; the waiting manual step is flagged as runnable
      (should (string-match-p "\\[manual ▸ T\\]" text))
      ;; the older-commit run no longer shows here as a "Recent runs"
      ;; block -- it renders beside its own commit in the Commits
      ;; section instead, see `gp-test-detail-commits-show-pipeline-status'
      (should-not (string-match-p "Recent runs" text)))))

(ert-deftest gp-test-pipeline-trigger-or-run-manual-prefers-manual-step ()
  (let ((called nil)
        (step '((state (name . "PENDING") (stage (name . "PAUSED")))
                (trigger (type . "pipeline_step_trigger_manual")))))
    (cl-letf (((symbol-function 'magit-current-section)
               (lambda () (let ((s (gp-pipeline-step-section))) (oset s value step) s)))
              ((symbol-function 'gp-detail-pipeline-run-manual)
               (lambda () (setq called 'manual)))
              ((symbol-function 'gp-detail-pipeline-trigger)
               (lambda () (setq called 'trigger))))
      (gp-detail-pipeline-trigger-or-run-manual)
      (should (eq called 'manual)))))

(ert-deftest gp-test-pipeline-render-empty-is-noop ()
  (should (equal (gp-test--render-pipelines nil) "")))

(ert-deftest gp-test-pipeline-finished-collapsed-flag ()
  "Finished pipelines render collapsed; running ones expanded."
  (bitbucket-mock-with-service
    (let* ((pr (car (alist-get 'values (bitbucket-mock--fixture "workspace-prs.json"))))
           (pipelines (gp-pipeline-fetch-for-pr pr)))
      (with-temp-buffer
        (gp-detail-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (gp-root)
            (gp--insert-pipelines pipelines)))
        (let ((pipe-secs
               (cl-remove-if-not
                (lambda (s) (object-of-class-p s 'gp-pipeline-section))
                (gp-test--all-pipeline-sections magit-root-section))))
          (should (= (length pipe-secs) 2))
          ;; each section's value is (PIPELINE . STEPS)
          (should (cl-every (lambda (s) (consp (oref s value))) pipe-secs)))))))

(defun gp-test--all-pipeline-sections (section)
  "Flatten SECTION and descendants."
  (cons section
        (cl-mapcan #'gp-test--all-pipeline-sections
                   (and (slot-boundp section 'children)
                        (oref section children)))))


(ert-deftest gp-test-elapsed-repaint-advances-in-place ()
  "A running step's elapsed time reticks without a full rerender.
Regression: the duration was computed at render time only, and the
poll skips the rerender while a running step's JSON is unchanged, so
the counter sat frozen until a manual `g'."
  (with-temp-buffer
    (let* ((started (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                        (time-subtract (current-time) 65)
                                        t))
           (inhibit-read-only t))
      ;; stale text: what a render 60s ago would have produced
      (insert "  x "
              (propertize "  5s" 'face 'shadow 'gp-pipeline-elapsed started)
              "\n")
      (should (gp-pipeline--elapsed-repaint-buffer))
      ;; now recomputed from `started_on', so it reflects the real elapsed
      (should (string-match-p "1m0[0-9]s" (buffer-string)))
      (should-not (string-match-p "  5s" (buffer-string)))
      ;; still tagged, so the next tick finds it again
      (should (text-property-not-all (point-min) (point-max)
                                     'gp-pipeline-elapsed nil)))))

(ert-deftest gp-test-elapsed-repaint-ignores-untagged-text ()
  "Only tagged (running) durations are reticked; finished ones stay put."
  (with-temp-buffer
    (insert "  x " (propertize "  1m30s" 'face 'shadow) "\n")
    (let ((before (buffer-string)))
      (should-not (gp-pipeline--elapsed-repaint-buffer))
      (should (equal before (buffer-string))))))

;;;; Step log rendering ---------------------------------------------------------

(ert-deftest gp-test-pipeline-log-render-decodes-ansi-instead-of-literal-codes ()
  "Real SGR escapes become a coloured overlay, not literal \"[31m\"-style text.
Regression: the raw captured log (Bitbucket or GitHub) was inserted
verbatim, so a viewer saw the escape codes as text instead of colour.
`ansi-color-apply-on-region' works via overlays, not text properties,
so this checks the buffer's overlays rather than `face' text props."
  (let ((git-platform-current-backend (git-platform-mock)))
    (with-temp-buffer
      (gp-pipeline-log-mode)
      (gp-pipeline-log--render "\x1b[31mred text\x1b[0m" nil)
      (should (equal (buffer-string) "red text"))
      (let ((ov (car (overlays-at (point-min)))))
        (should ov)
        (should (equal (overlay-get ov 'face) '(:foreground "red3")))))))

(ert-deftest gp-test-pipeline-log-render-highlights-bitbucket-command-echo ()
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (with-temp-buffer
      (gp-pipeline-log-mode)
      (gp-pipeline-log--render "+ rm -f /tmp/x\nordinary output" nil)
      (should (equal (buffer-string) "rm -f /tmp/x\nordinary output"))
      (should (eq (get-text-property (point-min) 'face)
                  'gp-pipeline-log-command-face))
      (should-not (get-text-property (1+ (length "rm -f /tmp/x\n")) 'face)))))

(ert-deftest gp-test-pipeline-log-render-highlights-github-group-and-strips-timestamp ()
  (let ((git-platform-current-backend (git-platform-github)))
    (with-temp-buffer
      (gp-pipeline-log-mode)
      (gp-pipeline-log--render
       (concat "2026-08-26T13:49:56.1234567Z ##[group]Run npm test\n"
               "2026-08-26T13:49:57.0000000Z npm test")
       nil)
      (should (equal (buffer-string) "Run npm test\nnpm test"))
      (should (eq (get-text-property (point-min) 'face)
                  'gp-pipeline-log-group-face)))))

(ert-deftest gp-test-pipeline-log-render-running-banner-follows-last-line ()
  "The \"tailing…\" banner must not gain a spurious blank line above it."
  (let ((git-platform-current-backend (git-platform-mock)))
    (with-temp-buffer
      (gp-pipeline-log-mode)
      (gp-pipeline-log--render "one\ntwo" t)
      (should (string-prefix-p "one\ntwo\n⏳" (buffer-string))))))

;;;; Manual-step deploy hook ----------------------------------------------------

(defconst gp-test--deploy-pipeline
  '((uuid . "{pipe-uuid}") (build_number . 4242)))

(defconst gp-test--deploy-step
  '((uuid . "{step-uuid}") (name . "Deploy to DEV") (state (name . "HALTED"))))

(ert-deftest gp-test-deploy-env-exports-context ()
  "The script gets repo/pipeline/step context through GP_* variables."
  (let* ((env (gp-pipeline--deploy-env
               "acme/web" "feature/x"
               gp-test--deploy-pipeline gp-test--deploy-step
               '((id . 77))))
         (get (lambda (k)
                (let ((hit (seq-find (lambda (e) (string-prefix-p (concat k "=") e))
                                     env)))
                  (and hit (substring hit (1+ (length k))))))))
    (should (equal (funcall get "GP_WORKSPACE") "acme"))
    (should (equal (funcall get "GP_REPO") "web"))
    (should (equal (funcall get "GP_FULL_NAME") "acme/web"))
    (should (equal (funcall get "GP_BRANCH") "feature/x"))
    (should (equal (funcall get "GP_STEP_NAME") "Deploy to DEV"))
    (should (equal (funcall get "GP_STEP_UUID") "{step-uuid}"))
    (should (equal (funcall get "GP_PIPELINE_UUID") "{pipe-uuid}"))
    (should (equal (funcall get "GP_PR_ID") "77"))))

(ert-deftest gp-test-deploy-env-omits-missing-values ()
  "A value that cannot be resolved is left unset, not exported empty.
Lets a script distinguish absent from empty with a plain -z test."
  (let ((env (gp-pipeline--deploy-env
              "acme/web" nil
              '((uuid . "{p}")) '((uuid . "{s}"))
              nil)))
    (should-not (seq-find (lambda (e) (string-prefix-p "GP_BRANCH=" e)) env))
    (should-not (seq-find (lambda (e) (string-prefix-p "GP_PR_ID=" e)) env))
    (should-not (seq-find (lambda (e) (string-prefix-p "GP_STEP_NAME=" e)) env))
    ;; what IS resolvable still comes through
    (should (seq-find (lambda (e) (string-prefix-p "GP_FULL_NAME=" e)) env))))

(ert-deftest gp-test-deploy-run-requires-configuration ()
  "Running with no script configured is a clear user error, not a crash."
  (let ((gp-pipeline-deploy-script nil))
    (should-error (gp-pipeline--deploy-run
                   "acme/web" "b" gp-test--deploy-pipeline
                   gp-test--deploy-step nil)
                  :type 'user-error)))

(ert-deftest gp-test-deploy-run-rejects-missing-program ()
  "A configured but nonexistent program fails before spawning anything."
  (let ((gp-pipeline-deploy-script (list "/nonexistent/gp-deploy-xyz")))
    (should-error (gp-pipeline--deploy-run
                   "acme/web" "b" gp-test--deploy-pipeline
                   gp-test--deploy-step nil)
                  :type 'user-error)))

(ert-deftest gp-test-notify-is-non-fatal ()
  "A failing notifier must not break the operation that called it."
  (let ((gp-notify t)
        (gp-notify-function (lambda (&rest _) (error "notifier is broken"))))
    (should-not (gp-notify "t" "b" t))))

(ert-deftest gp-test-notify-master-switch-silences ()
  "`gp-notify' nil suppresses everything, without calling the notifier."
  (let* ((called nil)
         (gp-notify nil)
         (gp-notify-function (lambda (&rest _) (setq called t))))
    (should-not (gp-notify "t" "b"))
    (should-not called)))

(ert-deftest gp-test-notify-uses-custom-function ()
  "`gp-notify-function' takes precedence over the built-in backends."
  (let* ((got nil)
         (gp-notify t)
         (gp-notify-function
          (lambda (title body urgent) (setq got (list title body urgent)))))
    (should (gp-notify "T" "B" t))
    (should (equal got '("T" "B" t)))))


(ert-deftest gp-test-deploy-refresh-targets-originating-buffer ()
  "The sentinel refreshes the detail buffer the deploy was started from.
Regression: `gp-detail-refresh' was called bare from the sentinel, which
runs in the process buffer -- where `gp--pr' is nil, so nothing was
refreshed and the finished deploy sat stale until a manual `g'."
  (let* ((refreshed nil)
         (detail (generate-new-buffer " *gp-detail-fake*"))
         (script (make-temp-file "gp-deploy-test" nil ".sh")))
    (unwind-protect
        (progn
          (with-temp-file script (insert "#!/bin/sh\nexit 0\n"))
          (set-file-modes script #o755)
          (with-current-buffer detail
            (setq-local gp--pr '((id . 1) (source (branch (name . "b"))))))
          (cl-letf (((symbol-function 'gp-detail-refresh)
                     (lambda () (setq refreshed (current-buffer)))))
            (let ((gp-pipeline-deploy-script (list script))
                  (gp-pipeline-deploy-notify nil))
              (with-current-buffer detail
                (gp-pipeline--deploy-run
                 "acme/web" "b" gp-test--deploy-pipeline
                 gp-test--deploy-step '((id . 1))))
              ;; let the process run to completion
              (let ((deadline (+ (float-time) 5)))
                (while (and (not refreshed) (< (float-time) deadline))
                  (accept-process-output nil 0.05)))))
          (should (eq refreshed detail)))
      (kill-buffer detail)
      (delete-file script))))

(ert-deftest gp-test-deploy-run-busts-repo-deploy-cache-on-success ()
  "A finished deploy busts the WHOLE repo's cached deploy verdicts, not
just this commit's -- a deploy to a shared environment can supersede
what an EARLIER pr's own successful deploy left behind, and that
earlier pr's commit never changes, so nothing else would ever
re-check it (see `gp-helm--deploy-cache-bust-repo')."
  (let* ((busted 'unset)
         (detail (generate-new-buffer " *gp-detail-fake*"))
         (script (make-temp-file "gp-deploy-test" nil ".sh")))
    (unwind-protect
        (progn
          (with-temp-file script (insert "#!/bin/sh\nexit 0\n"))
          (set-file-modes script #o755)
          (with-current-buffer detail
            (setq-local gp--pr '((id . 1) (source (branch (name . "b"))))))
          (cl-letf (((symbol-function 'gp-detail-refresh) #'ignore)
                    ((symbol-function 'gp-helm--deploy-cache-bust-repo)
                     (lambda (full-name) (setq busted full-name))))
            (let ((gp-pipeline-deploy-script (list script))
                  (gp-pipeline-deploy-notify nil))
              (with-current-buffer detail
                (gp-pipeline--deploy-run
                 "acme/web" "b" gp-test--deploy-pipeline
                 gp-test--deploy-step '((id . 1))))
              (let ((deadline (+ (float-time) 5)))
                (while (and (eq busted 'unset) (< (float-time) deadline))
                  (accept-process-output nil 0.05)))))
          (should (equal busted "acme/web")))
      (kill-buffer detail)
      (delete-file script))))

(ert-deftest gp-test-deploy-run-does-not-bust-cache-on-failure ()
  "A failed deploy leaves other PRs' cached verdicts alone -- nothing
about them changed."
  (let* ((busted 'unset)
         (detail (generate-new-buffer " *gp-detail-fake*"))
         (script (make-temp-file "gp-deploy-test" nil ".sh")))
    (unwind-protect
        (progn
          (with-temp-file script (insert "#!/bin/sh\nexit 1\n"))
          (set-file-modes script #o755)
          (with-current-buffer detail
            (setq-local gp--pr '((id . 1) (source (branch (name . "b"))))))
          (cl-letf (((symbol-function 'gp-detail-refresh) #'ignore)
                    ((symbol-function 'gp-helm--deploy-cache-bust-repo)
                     (lambda (full-name) (setq busted full-name))))
            (let ((gp-pipeline-deploy-script (list script))
                  (gp-pipeline-deploy-notify nil))
              (with-current-buffer detail
                (gp-pipeline--deploy-run
                 "acme/web" "b" gp-test--deploy-pipeline
                 gp-test--deploy-step '((id . 1))))
              ;; give the (short-lived, failing) process time to exit
              (let ((deadline (+ (float-time) 5)))
                (while (and (process-live-p (get-buffer-process gp-pipeline-deploy-buffer))
                            (< (float-time) deadline))
                  (accept-process-output nil 0.05)))))
          (should (eq busted 'unset)))
      (kill-buffer detail)
      (delete-file script))))

(provide 'gp-pipeline-test)
;;; gp-pipeline-test.el ends here
