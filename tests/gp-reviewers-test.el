;;; gp-reviewers-test.el --- Tests for editing reviewers -*- lexical-binding: t; -*-

;;; Commentary:

;; Covers the pure candidate grouping, the locked-reviewer rule (a
;; submitted review cannot be withdrawn by dropping the person), the
;; whole-list vs delta split between the two backends, and the save path.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gp-reviewers)
(require 'wid-edit)

;;;; Candidate grouping (pure) -------------------------------------------------

(ert-deftest gp-reviewers-test-groups-current-default-suggested ()
  "Rows are grouped, and nobody already on the PR reappears below."
  (let* ((current '((:id "{alice}" :name "Alice" :state pending)))
         (defaults '(((uuid . "{alice}") (display_name . "Alice"))
                     ((uuid . "{bob}") (display_name . "Bob"))))
         (suggested '(((uuid . "{carol}") (display_name . "Carol"))))
         (groups (gp-reviewers--candidates current defaults suggested)))
    (should (equal (mapcar #'car groups)
                   '("Current" "Default reviewers for this repo" "Suggested")))
    ;; Alice is current, so she is NOT repeated in the defaults group
    (should (equal (mapcar (lambda (r) (plist-get r :id)) (cdr (nth 0 groups)))
                   '("{alice}")))
    (should (equal (mapcar (lambda (r) (plist-get r :id)) (cdr (nth 1 groups)))
                   '("{bob}")))
    (should (equal (mapcar (lambda (r) (plist-get r :id)) (cdr (nth 2 groups)))
                   '("{carol}")))))

(ert-deftest gp-reviewers-test-current-rows-are-preticked ()
  "Existing reviewers start checked; candidates start unchecked."
  (let* ((groups (gp-reviewers--candidates
                  '((:id "{alice}" :name "Alice" :state pending))
                  nil
                  '(((uuid . "{bob}") (display_name . "Bob"))))))
    (should (plist-get (car (cdr (assoc "Current" groups))) :on))
    (should-not (plist-get (car (cdr (assoc "Suggested" groups))) :on))))

(ert-deftest gp-reviewers-test-reviewed-people-are-locked ()
  "Approved / changes-requested reviewers are locked; pending ones are not."
  (let* ((groups (gp-reviewers--candidates
                  '((:id "{a}" :name "A" :state approved)
                    (:id "{b}" :name "B" :state changes)
                    (:id "{c}" :name "C" :state pending))
                  nil nil))
         (rows (cdr (assoc "Current" groups))))
    (should (plist-get (nth 0 rows) :locked))
    (should (plist-get (nth 1 rows) :locked))
    (should-not (plist-get (nth 2 rows) :locked))))

(ert-deftest gp-reviewers-test-empty-when-nothing-to-offer ()
  (should (null (gp-reviewers--candidates nil nil nil))))

(ert-deftest gp-reviewers-test-skips-reviewers-without-an-id ()
  "A reviewer plist with no :id cannot be mapped to an API identity."
  (let ((groups (gp-reviewers--candidates
                 '((:name "Ghost" :state pending)) nil nil)))
    (should (null groups))))

;;;; Form rendering + save ----------------------------------------------------

(defun gp-reviewers-test--form (pr current defaults suggested)
  "Render the form for PR into the current buffer and return it."
  (gp-reviewers-mode)
  (setq gp-reviewers--pr pr)
  (gp-reviewers--build-form
   pr (gp-reviewers--candidates current defaults suggested))
  (current-buffer))

(ert-deftest gp-reviewers-test-form-shows-groups-and-badges ()
  (with-temp-buffer
    (gp-reviewers-test--form
     '((id . 7) (title . "Add widget"))
     '((:id "{alice}" :name "Alice" :state approved)
       (:id "{bob}" :name "Bob" :state pending))
     nil
     '(((uuid . "{carol}") (display_name . "Carol"))))
    (let ((text (substring-no-properties (buffer-string))))
      (should (string-match-p "#7  Add widget" text))
      (should (string-match-p "Current" text))
      (should (string-match-p "Alice.*approved" text))
      (should (string-match-p "(locked)" text))
      (should (string-match-p "Bob.*pending" text))
      (should (string-match-p "Suggested" text))
      (should (string-match-p "Carol" text))
      (should (string-match-p "Save \\[C\\]" text))
      (should (string-match-p "Cancel \\[q\\]" text)))))

(ert-deftest gp-reviewers-test-selected-ids-follow-the-checkboxes ()
  "Ticking a suggestion adds it; unticking a pending reviewer drops it."
  (with-temp-buffer
    (gp-reviewers-test--form
     '((id . 7))
     '((:id "{bob}" :name "Bob" :state pending))
     nil
     '(((uuid . "{carol}") (display_name . "Carol"))))
    (should (equal (gp-reviewers--selected-ids) '("{bob}")))
    (should (equal (gp-reviewers--current-ids) '("{bob}")))
    ;; tick Carol, untick Bob
    (let ((bob (cl-find "{bob}" gp-reviewers--widgets
                        :key (lambda (r) (plist-get r :id)) :test #'equal))
          (carol (cl-find "{carol}" gp-reviewers--widgets
                          :key (lambda (r) (plist-get r :id)) :test #'equal)))
      (widget-value-set (plist-get carol :widget) t)
      (widget-value-set (plist-get bob :widget) nil))
    (should (equal (gp-reviewers--selected-ids) '("{carol}")))
    ;; the "current" baseline is unchanged by editing
    (should (equal (gp-reviewers--current-ids) '("{bob}")))))

(ert-deftest gp-reviewers-test-locked-reviewer-snaps-back-on-toggle ()
  "Toggling a reviewed person (as a click or RET does) is undone at once.
`widget-toggle-action' sets the value and then fires `:notify', which
is where the lock re-ticks the box."
  (with-temp-buffer
    (gp-reviewers-test--form
     '((id . 7)) '((:id "{alice}" :name "Alice" :state approved)) nil nil)
    (let ((w (plist-get (car gp-reviewers--widgets) :widget)))
      (should (widget-value w))
      (widget-apply w :action nil)       ; the click path
      (should (widget-value w)))))       ; forced back on

(ert-deftest gp-reviewers-test-locked-reviewer-survives-a-raw-untick ()
  "Even if the checkbox is unticked by a path that skips `:notify',
the save still includes the locked reviewer -- the rule is enforced
where it matters, not only at the keystroke."
  (with-temp-buffer
    (gp-reviewers-test--form
     '((id . 7)) '((:id "{alice}" :name "Alice" :state approved)) nil nil)
    (widget-value-set (plist-get (car gp-reviewers--widgets) :widget) nil)
    (should (equal (gp-reviewers--selected-ids) '("{alice}")))))

(ert-deftest gp-reviewers-test-save-sends-complete-list-and-baseline ()
  "Save passes the desired end state plus the original list."
  (let (captured)
    (cl-letf (((symbol-function 'gp-set-pull-request-reviewers)
               (lambda (fn id wanted current)
                 (setq captured (list fn id wanted current))))
              ((symbol-function 'gp-pr-full-name) (lambda (_) "acme/web"))
              ((symbol-function 'gp-invalidate-pr-caches) #'ignore))
      (with-temp-buffer
        (gp-reviewers-test--form
         '((id . 7))
         '((:id "{bob}" :name "Bob" :state pending))
         nil
         '(((uuid . "{carol}") (display_name . "Carol"))))
        (widget-value-set
         (plist-get (cl-find "{carol}" gp-reviewers--widgets
                             :key (lambda (r) (plist-get r :id)) :test #'equal)
                    :widget)
         t)
        (gp-reviewers-save))
      (should (equal captured '("acme/web" 7 ("{bob}" "{carol}") ("{bob}")))))))

(ert-deftest gp-reviewers-test-save-noops-when-unchanged ()
  "An unchanged selection must not fire an API call."
  (let ((called nil))
    (cl-letf (((symbol-function 'gp-set-pull-request-reviewers)
               (lambda (&rest _) (setq called t)))
              ((symbol-function 'gp-pr-full-name) (lambda (_) "acme/web"))
              ((symbol-function 'gp-invalidate-pr-caches) #'ignore))
      (with-temp-buffer
        (gp-reviewers-test--form
         '((id . 7)) '((:id "{bob}" :name "Bob" :state pending)) nil nil)
        (gp-reviewers-save))
      (should-not called))))

;;;; Backend semantics --------------------------------------------------------

(ert-deftest gp-reviewers-test-bitbucket-puts-whole-list-as-array ()
  "Bitbucket replaces the list, and an empty one must encode as [] not null."
  (require 'bitbucket-api)
  (let (captured)
    (cl-letf (((symbol-function 'bitbucket-pull-request)
               (lambda (&rest _) '((title . "Keep me"))))
              ((symbol-function 'bitbucket-api-request)
               (lambda (method path &optional _params data)
                 (setq captured (list method path data)))))
      (bitbucket-set-pull-request-reviewers "acme/web" 7 '("{a}" "{b}"))
      (pcase-let ((`(,method ,path ,data) captured))
        (should (equal method "PUT"))
        (should (string-match-p "/pullrequests/7\\'" path))
        ;; the title is preserved -- a PUT without it would blank it
        (should (equal (alist-get 'title data) "Keep me"))
        (should (equal (alist-get 'reviewers data)
                       [((uuid . "{a}")) ((uuid . "{b}"))])))
      ;; clearing everyone: a vector, so json-encode emits []
      (bitbucket-set-pull-request-reviewers "acme/web" 7 nil)
      (should (equal (json-encode
                      (list (cons 'reviewers (alist-get 'reviewers (nth 2 captured)))))
                     "{\"reviewers\":[]}")))))

(ert-deftest gp-reviewers-test-github-sends-only-the-delta ()
  "GitHub POSTs additions and DELETEs removals, leaving the rest alone."
  (require 'github-api)
  (let (calls)
    (cl-letf (((symbol-function 'github-api-request)
               (lambda (method path &optional _params data)
                 (push (list method path (alist-get 'reviewers data)) calls))))
      (github-set-pull-request-reviewers "acme/web" 7 '("alice" "carol")
                                         '("alice" "bob"))
      (setq calls (nreverse calls))
      ;; bob removed, carol added, alice untouched (never re-notified)
      (should (equal (mapcar (lambda (c) (list (nth 0 c) (nth 2 c))) calls)
                     '(("DELETE" ["bob"]) ("POST" ["carol"])))))))

(ert-deftest gp-reviewers-test-github-skips-empty-delta ()
  "No change means no request at all."
  (require 'github-api)
  (let ((called nil))
    (cl-letf (((symbol-function 'github-api-request)
               (lambda (&rest _) (setq called t))))
      (github-set-pull-request-reviewers "acme/web" 7 '("alice") '("alice"))
      (should-not called))))

(provide 'gp-reviewers-test)
;;; gp-reviewers-test.el ends here
