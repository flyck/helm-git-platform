;;; gp-log.el --- Diagnostic log buffer for helm-git-platform -*- lexical-binding: t; -*-

;;; Commentary:

;; A lightweight logging facility writing to the *gp-log* buffer, so
;; that when something misbehaves the error -- and the API traffic leading
;; up to it -- can be read directly instead of guessed at.
;;
;; `gp-log' appends a timestamped line; `gp-log-error' marks
;; errors; the API layer wraps each request to log method/path/status and
;; elapsed time when `gp-log-requests' is non-nil.  Nothing here ever
;; signals -- logging must never break the thing it observes.

;;; Code:

(require 'subr-x)

(defcustom gp-log-enabled t
  "When non-nil, write diagnostic lines to the `*gp-log*' buffer."
  :type 'boolean :group 'bitbucket)

(defcustom gp-log-requests t
  "When non-nil, log every API request (method, path, status, timing)."
  :type 'boolean :group 'bitbucket)

(defcustom gp-log-buffer-name "*gp-log*"
  "Name of the diagnostic log buffer."
  :type 'string :group 'bitbucket)

(defcustom gp-log-max-lines 2000
  "Trim the log buffer to roughly this many lines."
  :type 'integer :group 'bitbucket)

(defvar gp-log--depth 0
  "Re-entrancy guard so logging an error can't recurse.")

(defun gp-log--timestamp ()
  "Return a short HH:MM:SS.mmm timestamp string."
  (format-time-string "%H:%M:%S.%3N"))

(defun gp-log (level fmt &rest args)
  "Append a LEVEL line (a symbol) built from FMT and ARGS to the log buffer.
Never signals; respects `gp-log-enabled'."
  (when (and gp-log-enabled (zerop gp-log--depth))
    (let ((gp-log--depth 1))
      (ignore-errors
        (with-current-buffer (get-buffer-create gp-log-buffer-name)
          (let ((inhibit-read-only t)
                (was-eob (eobp)))
            (save-excursion
              (goto-char (point-max))
              (insert (format "%s [%s] %s\n"
                              (gp-log--timestamp)
                              (upcase (symbol-name level))
                              (apply #'format fmt args)))
              ;; trim if overgrown
              (when (> (count-lines (point-min) (point-max))
                       gp-log-max-lines)
                (goto-char (point-min))
                (forward-line (- (count-lines (point-min) (point-max))
                                 gp-log-max-lines))
                (delete-region (point-min) (point))))
            (when (and was-eob (get-buffer-window (current-buffer)))
              (goto-char (point-max)))))))))

(defun gp-log-error (fmt &rest args)
  "Log an error line built from FMT and ARGS."
  (apply #'gp-log 'error fmt args))

;;;###autoload
(defun gp-log-show ()
  "Pop to the diagnostic log buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create gp-log-buffer-name)))

;;;###autoload
(defun gp-log-clear ()
  "Erase the diagnostic log buffer."
  (interactive)
  (when-let* ((buf (get-buffer gp-log-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer)))))

(provide 'gp-log)
;;; gp-log.el ends here
