;;; gp-deploy-watch-test.el --- Tests for the deploy watcher -*- lexical-binding: t; -*-

;;; Commentary:
;; Drives the watcher's decision loop with hand-built pipeline data, so the
;; step ordering it must respect is exercised without a live backend: the
;; watcher may only fire once the backend itself reports the gate open.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gp-deploy-watch)
(require 'gp-pipeline)
(require 'gp-ui)
(require 'git-platform-bitbucket)
(require 'git-platform-github)
(require 'bitbucket-mock)

;;;; Fixtures --------------------------------------------------------------------

(defconst gp-dw-test--pr
  '((id . 42) (title . "Add the widget toggle")
    (source (branch (name . "feature/widget"))
            (commit (hash . "abc123")))
    (destination (repository (full_name . "acme/web") (slug . "web"))))
  "A Bitbucket-shaped PR the watcher can read repo/branch/commit off.")

(defun gp-dw-test--step (name &rest kv)
  "Return a manual step called NAME, with KV merged over the defaults."
  (append kv
          `((name . ,name)
            (trigger (type . "pipeline_step_trigger_manual")))))

(defun gp-dw-test--gate (name)
  "Return a manual step NAME that is waiting and startable."
  (gp-dw-test--step name '(state (name . "PENDING") (stage (name . "PAUSED")))))

(defun gp-dw-test--unreached-gate (name)
  "Return a manual step NAME the build has not reached yet.
Deliberately NOT state NOT_RUN: Bitbucket counts that as pending, and
therefore already startable (`bitbucket-pipeline-step-pending-p'), so
it would be a gate that is open rather than one still to come."
  (gp-dw-test--step name '(state (name . "IN_PROGRESS"))))

(defun gp-dw-test--pending-auto (name)
  "Return an automatic step NAME that has not finished yet."
  `((name . ,name)
    (state (name . "IN_PROGRESS"))
    (trigger (type . "pipeline_step_trigger_automatic"))))

(defun gp-dw-test--data (pipeline steps)
  "Return fetch-shaped data for PIPELINE with STEPS as the current run."
  (list :current (list (cons pipeline steps)) :recent nil))

(defconst gp-dw-test--running-pipeline
  '((build_number . 728) (uuid . "{p728}")
    (state (name . "IN_PROGRESS") (stage (name . "RUNNING"))))
  "A pipeline still executing.")

(defconst gp-dw-test--finished-pipeline
  '((build_number . 728) (uuid . "{p728}")
    (state (name . "COMPLETED") (result (name . "SUCCESSFUL"))))
  "A pipeline that has finished.")

(defmacro gp-dw-test--with-clean-registry (&rest body)
  "Run BODY with an empty watcher registry, restoring it afterwards.
Watchers are global by design, so a test must not leak one into the
next -- a stray timer would keep polling for the rest of the run."
  (declare (indent 0))
  `(let ((gp-deploy-watch--registry (make-hash-table :test 'equal))
         ;; never let a test schedule a real timer or ask a real question
         (gp-deploy-watch-confirm nil)
         (git-platform-current-backend (git-platform-bitbucket)))
     (cl-letf (((symbol-function 'gp-deploy-watch--rearm) #'ignore)
               ((symbol-function 'gp-deploy-watch--tick) #'ignore))
       ,@body)))

(defun gp-dw-test--arm (&optional step-name)
  "Arm a watcher on STEP-NAME (default \"deploy-dev\") and return it."
  (gp-deploy-watch-arm "acme/web" "feature/widget" "abc123"
                       (or step-name "deploy-dev") gp-dw-test--pr))

;;;; Identity and registry ---------------------------------------------------------

(ert-deftest gp-test-dw-key-is-repo-branch-step ()
  "A watcher is identified by what the user armed -- repo, branch, step
name -- not by a pipeline or step id, because the trigger can start a
new run in which every id is different."
  (should (equal (gp-deploy-watch--key "acme/web" "feature/x" "deploy-dev")
                 "acme/web@feature/x#deploy-dev"))
  ;; distinct steps on one branch do not collide
  (should-not (equal (gp-deploy-watch--key "acme/web" "feature/x" "deploy-dev")
                     (gp-deploy-watch--key "acme/web" "feature/x" "deploy-prod"))))

(ert-deftest gp-test-dw-arm-registers-and-logs ()
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm)))
      (should (eq w (gp-deploy-watch-get "acme/web" "feature/widget" "deploy-dev")))
      (should (eq (gp-deploy-watch-state w) 'waiting))
      (should (gp-deploy-watch-active-p w))
      ;; the arming itself is the first thing in the log
      (should (= 1 (length (gp-deploy-watch-log w))))
      (should (string-match-p "armed" (cdar (gp-deploy-watch-log w)))))))

(ert-deftest gp-test-dw-arming-twice-is-refused ()
  "Two watchers on one gate would both fire it."
  (gp-dw-test--with-clean-registry
    (gp-dw-test--arm)
    (should-error (gp-dw-test--arm) :type 'user-error)))

(ert-deftest gp-test-dw-rearming-a-finished-watcher-is-allowed ()
  "Once a watcher is done the gate is free again, so the step can be
armed for the next build without having to clear the old one first."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm)))
      (gp-deploy-watch-cancel-watcher w)
      (should-not (gp-deploy-watch-active-p w))
      (let ((w2 (gp-dw-test--arm)))
        (should (gp-deploy-watch-active-p w2))
        (should-not (eq w w2))))))

;;;; The waiting rule -- step order is respected ------------------------------------

(ert-deftest gp-test-dw-waits-while-earlier-steps-run ()
  "The whole point: with the gate not yet open, nothing is fired.
The watcher never reasons about the dependency graph itself -- it
waits for the backend to report the step runnable."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm))
          (fired nil))
      (cl-letf (((symbol-function 'gp-deploy-watch--fire)
                 (lambda (&rest _) (setq fired t))))
        (gp-deploy-watch--consider
         w (gp-dw-test--data
            gp-dw-test--running-pipeline
            ;; an earlier step still running, and the gate NOT yet startable
            (list (gp-dw-test--pending-auto "test")
                  (gp-dw-test--unreached-gate "deploy-dev")))))
      (should-not fired)
      (should (eq (gp-deploy-watch-state w) 'waiting)))))

(ert-deftest gp-test-dw-fires-when-the-gate-opens ()
  "Once the backend reports the manual step waiting, it fires."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm))
          (fired nil))
      (cl-letf (((symbol-function 'gp-deploy-watch--fire)
                 (lambda (_w _p step) (setq fired (alist-get 'name step)))))
        (gp-deploy-watch--consider
         w (gp-dw-test--data gp-dw-test--running-pipeline
                             (list (gp-dw-test--gate "deploy-dev")))))
      (should (equal fired "deploy-dev"))
      (should (eq (gp-deploy-watch-state w) 'firing)))))

(ert-deftest gp-test-dw-fires-only-its-own-step ()
  "Another step's gate opening is not this watcher's cue."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm "deploy-dev"))
          (fired nil))
      (cl-letf (((symbol-function 'gp-deploy-watch--fire)
                 (lambda (&rest _) (setq fired t))))
        (gp-deploy-watch--consider
         w (gp-dw-test--data gp-dw-test--running-pipeline
                             (list (gp-dw-test--gate "deploy-prod")))))
      (should-not fired)
      (should (eq (gp-deploy-watch-state w) 'waiting)))))

(ert-deftest gp-test-dw-gives-up-when-the-build-finishes-unfired ()
  "A finished run will never open the gate, so waiting on it is over."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm)))
      (gp-deploy-watch--consider
       w (gp-dw-test--data gp-dw-test--finished-pipeline
                           (list (gp-dw-test--unreached-gate "deploy-dev"))))
      (should (eq (gp-deploy-watch-state w) 'failed))
      (should (string-match-p "without opening the gate"
                              (gp-deploy-watch-detail w))))))

(ert-deftest gp-test-dw-empty-fetch-does-not-end-the-wait ()
  "A failed fetch reports nil exactly as a run-less branch does, so it
must not be read as \"the build vanished\"."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm)))
      (gp-deploy-watch--consider w nil)
      (should (eq (gp-deploy-watch-state w) 'waiting))
      (should (string-match-p "no pipeline data"
                              (cdar (gp-deploy-watch-log w)))))))

(ert-deftest gp-test-dw-step-absent-from-the-run-keeps-waiting ()
  "The step may not exist yet in a run that is still starting up."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm)))
      (gp-deploy-watch--consider
       w (gp-dw-test--data gp-dw-test--running-pipeline
                           (list (gp-dw-test--pending-auto "build"))))
      (should (eq (gp-deploy-watch-state w) 'waiting)))))

(ert-deftest gp-test-dw-cancelled-watcher-ignores-later-polls ()
  "An in-flight fetch can land after a cancel; it must not resurrect it."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm))
          (fired nil))
      (gp-deploy-watch-cancel-watcher w)
      (cl-letf (((symbol-function 'gp-deploy-watch--fire)
                 (lambda (&rest _) (setq fired t))))
        (gp-deploy-watch--consider
         w (gp-dw-test--data gp-dw-test--running-pipeline
                             (list (gp-dw-test--gate "deploy-dev")))))
      (should-not fired)
      (should (eq (gp-deploy-watch-state w) 'cancelled)))))

(defvar gp-dw-test--real-tick (symbol-function 'gp-deploy-watch--tick)
  "The real tick, captured before any test stubs it out.")

(ert-deftest gp-test-dw-times-out-rather-than-polling-forever ()
  "A build that never reaches the gate must not poll until Emacs exits."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm)))
      ;; armed one second past the limit
      (setf (gp-deploy-watch-started w)
            (- (float-time) (1+ gp-deploy-watch-timeout)))
      (should (gp-deploy-watch--timed-out-p w))
      ;; the real tick, not the macro's stub -- and it must retire the
      ;; watcher without asking the API anything
      (cl-letf (((symbol-function 'gp-pipeline-fetch-for-branch-async)
                 (lambda (&rest _) (error "must not fetch after timing out"))))
        (funcall gp-dw-test--real-tick w))
      (should (eq (gp-deploy-watch-state w) 'failed))
      (should (string-match-p "gave up" (gp-deploy-watch-detail w))))))

;;;; Firing routes -------------------------------------------------------------------

(ert-deftest gp-test-dw-fires-via-the-deploy-script-when-configured ()
  "A configured script is the only route that advances THIS build's gate."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm))
          (script-run nil))
      (cl-letf (((symbol-function 'gp-pipeline--deploy-run)
                 (lambda (&rest _) (setq script-run t)))
                ((symbol-function 'gp-pipeline-run-manual-step)
                 (lambda (&rest _) (error "must not re-trigger with a script set")))
                ((symbol-function 'gp-deploy-watch--notify) #'ignore))
        (let ((gp-pipeline-deploy-script '("/bin/true")))
          (gp-deploy-watch--fire w gp-dw-test--running-pipeline
                                 (gp-dw-test--gate "deploy-dev"))))
      (should script-run)
      (should (eq (gp-deploy-watch-state w) 'done)))))

(ert-deftest gp-test-dw-falls-back-to-the-api-without-a-script ()
  "With no script it uses the backend's manual-step API -- and says that
this re-runs the earlier steps, rather than reporting a plain success."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm))
          (api-called nil))
      (cl-letf (((symbol-function 'gp-pipeline-run-manual-step)
                 (lambda (&rest _) (setq api-called t)))
                ((symbol-function 'gp-deploy-watch--notify) #'ignore))
        (let ((gp-pipeline-deploy-script nil))
          (gp-deploy-watch--fire w gp-dw-test--running-pipeline
                                 (gp-dw-test--gate "deploy-dev"))))
      (should api-called)
      (should (eq (gp-deploy-watch-state w) 'done))
      (should (string-match-p "re-runs" (gp-deploy-watch-detail w))))))

(ert-deftest gp-test-dw-trigger-failure-is-recorded-not-swallowed ()
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm)))
      (cl-letf (((symbol-function 'gp-pipeline-run-manual-step)
                 (lambda (&rest _) (error "403 forbidden")))
                ((symbol-function 'gp-deploy-watch--notify) #'ignore))
        (let ((gp-pipeline-deploy-script nil))
          (gp-deploy-watch--fire w gp-dw-test--running-pipeline
                                 (gp-dw-test--gate "deploy-dev"))))
      (should (eq (gp-deploy-watch-state w) 'failed))
      (should (string-match-p "403 forbidden" (gp-deploy-watch-detail w))))))

;;;; Log ------------------------------------------------------------------------------

(ert-deftest gp-test-dw-log-is-bounded ()
  "A long-lived watcher must not grow an unbounded log."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm))
          (gp-deploy-watch-log-max 5))
      (dotimes (i 20) (gp-deploy-watch--log w "event %d" i))
      (should (= 5 (length (gp-deploy-watch-log w))))
      ;; newest kept, oldest dropped
      (should (string-match-p "event 19" (cdar (gp-deploy-watch-log w)))))))

(ert-deftest gp-test-dw-log-buffer-renders-events-oldest-first ()
  "A log reads forwards, even though it is stored newest-first."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm)))
      (gp-deploy-watch--log w "first thing")
      (gp-deploy-watch--log w "second thing")
      (with-temp-buffer
        (gp-deploy-watch--render-log w)
        (let ((text (substring-no-properties (buffer-string))))
          (should (string-match-p "deploy-dev" text))
          (should (< (string-match "first thing" text)
                     (string-match "second thing" text))))))))

;;;; Step marker and the list ------------------------------------------------------

(ert-deftest gp-test-dw-step-marker-only-on-watched-steps ()
  (gp-dw-test--with-clean-registry
    (gp-dw-test--arm "deploy-dev")
    (should (string-match-p
             "▸ A"
             (substring-no-properties
              (gp-deploy-watch-step-marker
               (gp-dw-test--gate "deploy-dev") "acme/web" "feature/widget"))))
    ;; a different step, and a different branch, are not THIS watcher's --
    ;; but each is still schedulable on its own, so it gets the plain
    ;; unarmed "▸ A" hint rather than nil (see
    ;; `gp-test-dw-step-marker-hints-unarmed-schedulable-steps')
    (should (string-match-p
             "▸ A"
             (substring-no-properties
              (gp-deploy-watch-step-marker
               (gp-dw-test--gate "deploy-prod") "acme/web" "feature/widget"))))
    (should (string-match-p
             "▸ A"
             (substring-no-properties
              (gp-deploy-watch-step-marker
               (gp-dw-test--gate "deploy-dev") "acme/web" "other-branch"))))))

(ert-deftest gp-test-dw-step-marker-hints-unarmed-schedulable-steps ()
  "A schedulable step with no watcher yet still gets a plain `▸ A' hint
\(unconditionally, like `[manual ▸ T]'/`[rerun ▸ P]' on the other
pipeline commands) -- without this, `A' had no visible sign it
existed until the user already knew to press it once."
  (gp-dw-test--with-clean-registry
    (should (string-match-p
             "▸ A"
             (substring-no-properties
              (gp-deploy-watch-step-marker
               (gp-dw-test--gate "deploy-dev") "acme/web" "feature/widget"))))))

(ert-deftest gp-test-dw-step-marker-nil-on-non-schedulable-step ()
  "A step that is not a manual gate at all (nothing `A' could ever
arm) gets no marker -- not even the unarmed hint."
  (gp-dw-test--with-clean-registry
    (should-not (gp-deploy-watch-step-marker
                 '((name . "build") (state (name . "COMPLETED")))
                 "acme/web" "feature/widget"))))

(ert-deftest gp-test-dw-step-marker-shows-the-state ()
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm "deploy-dev")))
      (should (string-match-p
               "waiting"
               (substring-no-properties
                (gp-deploy-watch-step-marker
                 (gp-dw-test--gate "deploy-dev") "acme/web" "feature/widget"))))
      (gp-deploy-watch-cancel-watcher w)
      (should (string-match-p
               "cancelled"
               (substring-no-properties
                (gp-deploy-watch-step-marker
                 (gp-dw-test--gate "deploy-dev") "acme/web" "feature/widget")))))))

(ert-deftest gp-test-dw-list-renders-and-carries-its-watchers ()
  "Each row carries its watcher, so the row commands need no lookup."
  (gp-dw-test--with-clean-registry
    (gp-dw-test--arm "deploy-dev")
    (with-temp-buffer
      (gp-deploy-watch--render-list)
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "deploy-dev" text))
        (should (string-match-p "waiting" text)))
      (goto-char (point-min))
      (should (search-forward "deploy-dev" nil t))
      (should (gp-deploy-watch--at-point)))))

(ert-deftest gp-test-dw-list-columns-do-not-collide ()
  "The state is a symbol; `%-Ns' pads strings, so it has to be formatted
before padding or the state and step name run together."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm "deploy-dev")))
      (gp-deploy-watch-cancel-watcher w)
      (with-temp-buffer
        (gp-deploy-watch--render-list)
        (should (string-match-p "cancelled +deploy-dev"
                                (substring-no-properties (buffer-string))))))))

(ert-deftest gp-test-dw-status-glyph-is-shared-across-the-ui ()
  "One glyph per state, used by the notification, the step marker, the
list and the log header, so an outcome looks the same wherever it is read."
  (should (equal (gp-deploy-watch--state-glyph 'done) "🟢"))
  (should (equal (gp-deploy-watch--state-glyph 'failed) "🔴"))
  (should (equal (gp-deploy-watch--state-glyph 'waiting) "⏳"))
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm "deploy-dev")))
      ;; the step line
      (should (string-match-p
               "⏳" (substring-no-properties
                    (gp-deploy-watch-step-marker
                     (gp-dw-test--gate "deploy-dev") "acme/web" "feature/widget"))))
      ;; the list
      (with-temp-buffer
        (gp-deploy-watch--render-list)
        (should (string-match-p "⏳" (substring-no-properties (buffer-string)))))
      ;; the log header
      (gp-deploy-watch-cancel-watcher w)
      (with-temp-buffer
        (gp-deploy-watch--render-log w)
        (should (string-match-p "⚪" (substring-no-properties (buffer-string))))))))

(ert-deftest gp-test-dw-list-is-empty-without-watchers ()
  (gp-dw-test--with-clean-registry
    (with-temp-buffer
      (gp-deploy-watch--render-list)
      (should (string-match-p "(none armed)"
                              (substring-no-properties (buffer-string)))))))

(ert-deftest gp-test-dw-clear-finished-keeps-active-ones ()
  (gp-dw-test--with-clean-registry
    (let ((live (gp-dw-test--arm "deploy-dev"))
          (dead (gp-dw-test--arm "deploy-prod")))
      (gp-deploy-watch-cancel-watcher dead)
      (gp-deploy-watch-clear-finished)
      (should (eq live (gp-deploy-watch-get "acme/web" "feature/widget" "deploy-dev")))
      (should-not (gp-deploy-watch-get "acme/web" "feature/widget" "deploy-prod")))))

;;;; Arming from the detail view --------------------------------------------------

(ert-deftest gp-test-dw-toggle-refuses-a-non-manual-step ()
  "There is no gate to wait for on an automatic step."
  (gp-dw-test--with-clean-registry
    (let ((gp--pr gp-dw-test--pr))
      (cl-letf (((symbol-function 'gp-pipeline--step-at-point)
                 (lambda () (gp-dw-test--pending-auto "build"))))
        (should-error (gp-deploy-watch-toggle-at-point) :type 'user-error)))))

(ert-deftest gp-test-dw-toggle-arms-then-cancels ()
  "`A' is a toggle: the second press on an armed step cancels it."
  (gp-dw-test--with-clean-registry
    (let ((gp--pr gp-dw-test--pr))
      (cl-letf (((symbol-function 'gp-pipeline--step-at-point)
                 (lambda () (gp-dw-test--gate "deploy-dev")))
                ((symbol-function 'gp-detail-refresh) #'ignore))
        (gp-deploy-watch-toggle-at-point)
        (let ((w (gp-deploy-watch-get "acme/web" "feature/widget" "deploy-dev")))
          (should (gp-deploy-watch-active-p w))
          (gp-deploy-watch-toggle-at-point)
          (should (eq (gp-deploy-watch-state w) 'cancelled)))))))

(ert-deftest gp-test-dw-only-triggerable-steps-are-schedulable ()
  "Only a step somebody could press can be scheduled -- waiting for a
step that runs itself would be waiting for nothing."
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (should (gp-deploy-watch-schedulable-p (gp-dw-test--gate "deploy-dev")))
    (should-not (gp-deploy-watch-schedulable-p
                 (gp-dw-test--pending-auto "build")))
    (should-not (gp-deploy-watch-schedulable-p nil))))

(ert-deftest gp-test-dw-backend-support-is-asked-not-hardcoded ()
  "Bitbucket has triggerable steps; GitHub Actions has none in this
model, and the difference is asked of the backend rather than branching
on which forge is in use."
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (should (gp-deploy-watch-backend-supports-p)))
  (let ((git-platform-current-backend (git-platform-github)))
    (should-not (gp-deploy-watch-backend-supports-p))
    ;; and so nothing on GitHub is schedulable
    (should-not (gp-deploy-watch-schedulable-p (gp-dw-test--gate "deploy-dev")))))

;;;; Chained gates -- deploy-live behind deploy-dev ----------------------------------

(ert-deftest gp-test-dw-steps-before-is-run-order ()
  "\"Earlier\" means earlier in the run's own step list -- the only
ordering the backend gives us, and the only one that matters."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm "deploy-live"))
          (steps (list (gp-dw-test--pending-auto "lint")
                       (gp-dw-test--pending-auto "build")
                       (gp-dw-test--gate "deploy-dev")
                       (gp-dw-test--gate "deploy-live")
                       (gp-dw-test--gate "deploy-after"))))
      (should (equal (mapcar (lambda (s) (alist-get 'name s))
                             (gp-deploy-watch--steps-before w steps))
                     '("lint" "build" "deploy-dev")))
      ;; a gate AFTER the target is not in the way
      (should-not (member "deploy-after"
                          (mapcar (lambda (s) (alist-get 'name s))
                                  (gp-deploy-watch--steps-before w steps)))))))

(ert-deftest gp-test-dw-presses-an-earlier-gate-then-keeps-waiting ()
  "Scheduling deploy-live auto-approves the deploy-dev gate in its way,
and does NOT finish -- pressing dev is a step on the road to live."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm "deploy-live"))
          (pressed nil))
      (cl-letf (((symbol-function 'gp-pipeline-run-manual-step)
                 (lambda (_f _b _p s) (push (alist-get 'name s) pressed) t)))
        (let ((gp-pipeline-deploy-script nil))
          (gp-deploy-watch--consider
           w (gp-dw-test--data
              gp-dw-test--running-pipeline
              (list (gp-dw-test--gate "deploy-dev")
                    (gp-dw-test--unreached-gate "deploy-live"))))))
      (should (equal pressed '("deploy-dev")))
      ;; still waiting for the target it was actually armed on
      (should (eq (gp-deploy-watch-state w) 'waiting)))))

(ert-deftest gp-test-dw-presses-each-earlier-gate-only-once ()
  "A gate keeps reporting open for a moment after it is triggered;
pressing it every poll would launch it again and again."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm "deploy-live"))
          (presses 0))
      (cl-letf (((symbol-function 'gp-pipeline-run-manual-step)
                 (lambda (&rest _) (setq presses (1+ presses)) t)))
        (let ((gp-pipeline-deploy-script nil)
              (data (gp-dw-test--data
                     gp-dw-test--running-pipeline
                     (list (gp-dw-test--gate "deploy-dev")
                           (gp-dw-test--unreached-gate "deploy-live")))))
          (gp-deploy-watch--consider w data)
          (gp-deploy-watch--consider w data)
          (gp-deploy-watch--consider w data)))
      (should (= presses 1)))))

(ert-deftest gp-test-dw-target-wins-over-an-earlier-open-gate ()
  "When the target itself is ready it fires now, never deferred behind
a gate that is no longer in its way."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm "deploy-live"))
          (fired nil))
      (cl-letf (((symbol-function 'gp-deploy-watch--fire)
                 (lambda (_w _p s) (setq fired (alist-get 'name s)))))
        (gp-deploy-watch--consider
         w (gp-dw-test--data gp-dw-test--running-pipeline
                             (list (gp-dw-test--gate "deploy-dev")
                                   (gp-dw-test--gate "deploy-live")))))
      (should (equal fired "deploy-live")))))

(ert-deftest gp-test-dw-automatic-step-ahead-is-not-pressed ()
  "An automatic step in the way is just work still to do, not a button."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm "deploy-live"))
          (pressed nil))
      (cl-letf (((symbol-function 'gp-pipeline-run-manual-step)
                 (lambda (&rest _) (setq pressed t) t)))
        (gp-deploy-watch--consider
         w (gp-dw-test--data gp-dw-test--running-pipeline
                             (list (gp-dw-test--pending-auto "build")
                                   (gp-dw-test--unreached-gate "deploy-live")))))
      (should-not pressed)
      (should (eq (gp-deploy-watch-state w) 'waiting)))))

(ert-deftest gp-test-dw-aborts-when-a-step-before-the-gate-fails ()
  "A failed earlier step makes the target unreachable, so the watcher
stops and names the step that broke rather than waiting out the timeout."
  (gp-dw-test--with-clean-registry
    (let ((w (gp-dw-test--arm "deploy-live")))
      (gp-deploy-watch--consider
       w (gp-dw-test--data
          gp-dw-test--running-pipeline
          (list `((name . "build")
                  (state (name . "COMPLETED") (result (name . "FAILED")))
                  (trigger (type . "pipeline_step_trigger_automatic")))
                (gp-dw-test--unreached-gate "deploy-live"))))
      (should (eq (gp-deploy-watch-state w) 'failed))
      (should (string-match-p "build" (gp-deploy-watch-detail w)))
      (should (string-match-p "unreachable" (gp-deploy-watch-detail w))))))

(ert-deftest gp-test-dw-terminal-states-notify ()
  "A watcher runs unattended, so an outcome must reach the user outside
the frame -- above all a failure.  Cancelling does not notify: the user
just did it."
  (gp-dw-test--with-clean-registry
    (let ((notes nil))
      (cl-letf (((symbol-function 'gp-notify)
                 (lambda (title _body &optional urgent)
                   (push (cons title urgent) notes))))
        (let ((w (gp-dw-test--arm "deploy-live")))
          (gp-deploy-watch--set-state w 'failed "build broke")
          (should (= 1 (length notes)))
          (should (cdar notes))                      ; urgent
          (should (string-match-p "blocked" (caar notes))))
        (setq notes nil)
        (let ((w (gp-dw-test--arm "deploy-dev")))
          (gp-deploy-watch--set-state w 'done "fired")
          (should (= 1 (length notes)))
          (should-not (cdar notes)))
        (setq notes nil)
        (let ((w (gp-dw-test--arm "deploy-other")))
          (gp-deploy-watch-cancel-watcher w)
          (should (null notes)))))))

(provide 'gp-deploy-watch-test)
;;; gp-deploy-watch-test.el ends here
