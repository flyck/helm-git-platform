;;; git-platform-test.el --- Protocol conformance tests -*- lexical-binding: t; -*-

;;; Commentary:

;; A backend-agnostic conformance suite: it exercises the `gp-' protocol
;; against a configured backend + its mock and asserts the documented
;; return shapes.  Today it runs against the Bitbucket backend with
;; `bitbucket-mock-with-service'.  A future GitHub backend reuses
;; `git-platform-test--run' with its own mock, proving both satisfy one
;; contract.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'git-platform)
(require 'git-platform-bitbucket)
(require 'bitbucket-mock)

(defun git-platform-test--run ()
  "Run protocol assertions against the active backend (mock must be live)."
  (let* ((uuid (gp-user-uuid))
         (prs (gp-workspace-pull-requests)))
    (should (stringp uuid))
    (should (listp prs))
    (let ((pr (car prs)))
      (should pr)
      ;; accessors return the documented shapes
      (should (string-match-p "/" (gp-pr-full-name pr)))   ;; owner/slug
      (should (stringp (gp-pr-source-branch pr)))
      (should (stringp (gp-pr-destination-branch pr)))
      (should (memq (gp-pr-draft-p pr) '(t nil)))
      (should (memq (gp-pr-authored-by-p pr uuid) '(t nil)))
      (let ((tally (gp-pr-review-tally pr)))
        (should (integerp (plist-get tally :approved)))
        (should (integerp (plist-get tally :changes)))
        (should (integerp (plist-get tally :pending))))
      ;; categorize/partition produce the documented structure
      (let ((cat (gp-categorize-pull-requests prs uuid)))
        (should (= (length prs)
                   (+ (length (plist-get cat :mine))
                      (length (plist-get cat :reviewing))
                      (length (plist-get cat :drafts))))))
      (let ((split (gp-partition-pull-requests prs uuid)))
        (should (= (length prs) (+ (length (car split)) (length (cdr split))))))
      ;; comments + their accessors
      (let* ((fn (gp-pr-full-name pr))
             (id (alist-get 'id pr))
             (comments (gp-pull-request-comments fn id)))
        (should (listp comments))
        (when comments
          (should (memq (gp-comment-resolved-p (car comments)) '(t nil)))
          (should (memq (gp-comment-own-p (car comments) uuid) '(t nil))))))))

(ert-deftest git-platform-test-bitbucket-conformance ()
  "The Bitbucket backend satisfies the git-platform protocol."
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (bitbucket-mock-with-service
      (git-platform-test--run))))

(ert-deftest git-platform-test-backend-lazy-default ()
  "`git-platform-backend' builds a Bitbucket backend by default."
  (let ((git-platform-current-backend nil)
        (git-platform-default-backend 'bitbucket))
    (should (object-of-class-p (git-platform-backend) 'git-platform-bitbucket))))

(ert-deftest git-platform-test-public-wrappers-need-no-backend ()
  "The public gp- functions take no backend argument."
  (let ((git-platform-current-backend (git-platform-bitbucket)))
    (bitbucket-mock-with-service
      ;; would error if gp-user-uuid still required a backend arg
      (should (stringp (gp-user-uuid))))))

(provide 'git-platform-test)
;;; git-platform-test.el ends here
