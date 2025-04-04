;;; relysium-ui.el --- UI components for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains UI components and interaction patterns for relysium,
;; including the transient menu for handling code changes.

;;; Code:

(require 'transient)
(require 'smerge-mode)

;; Define a transient menu for Elysium with compact layout
(defun relysium-keep-all-changes ()
  "Keep all of the LLM suggestions."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (funcall #'smerge-keep-lower))
    (while (ignore-errors (not (smerge-next)))
      (funcall #'smerge-keep-lower))
    (smerge-mode -1)
    (message "All suggested changes applied")))

(defun relysium-discard-all-changes ()
  "Discard all of the LLM suggestions."
  (interactive)
  (undo)
  (smerge-mode -1)
  (message "All suggested changes discarded"))

(defun relysium-navigate-next-change ()
  "Navigate to the next change suggestion and keep the transient menu active."
  (interactive)
  (if (ignore-errors (smerge-next))
      (message "Navigated to next change")
    (message "No more changes"))
  ;; Keep the transient menu active
  (relysium-transient-menu))

(defun relysium-navigate-prev-change ()
  "Navigate to the previous change suggestion and keep the transient menu active."
  (interactive)
  (if (ignore-errors (smerge-prev))
      (message "Navigated to previous change")
    (message "No more changes"))
  ;; Keep the transient menu active
  (relysium-transient-menu))

(defun relysium-keep-current-change ()
  "Keep the current suggested change and move to the next one."
  (interactive)
  (smerge-keep-lower)
  (unless (ignore-errors (smerge-next))
    (progn
      (message "Applied change - move to next")
      ;; Keep the transient menu active
      (relysium-transient-menu))
    (progn
      (message "All changes reviewed - no more conflicts")
      (smerge-mode -1))))

(defun relysium-discard-current-change ()
  "Reject the current suggested change and move to the next one."
  (interactive)
  (smerge-keep-upper)
  (unless (ignore-errors (smerge-next))
    (progn
      (message "Rejected change - move to next")
      ;; Keep the transient menu active
      (relysium-transient-menu))
    (progn
      (message "All changes reviewed - no more conflicts")
      (smerge-mode -1))))

;; We're deferring the definition of relysium-retry-query to command modules
;; since it's specific to each command type

(provide 'relysium-commands)
;;; relysium-ui.el ends here
