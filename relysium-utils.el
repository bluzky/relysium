;;; relysium-utils.el --- Utility functions for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains utility functions for the relysium package, including:
;; - Diff generation and application
;; - Code change handling
;; - Debug logging utilities

;;; Code:

(defcustom relysium-debug-mode nil
  "When non-nil, log LLM responses and other debug information."
  :group 'relysium
  :type 'boolean)

(defcustom relysium-debug-buffer-name "*relysium-debug*"
  "Name of the buffer for debug logging."
  :group 'relysium
  :type 'string)

(defun relysium-debug-log (message &rest args)
  "Log MESSAGE with ARGS to the debug buffer if debug mode is enabled."
  (when relysium-debug-mode
    (let ((debug-buffer (get-buffer-create relysium-debug-buffer-name)))
      (with-current-buffer debug-buffer
        (goto-char (point-max))
        (let ((start (point)))
          (insert (format "[%s] " (format-time-string "%Y-%m-%d %H:%M:%S")))
          (insert (apply #'format message args))
          (insert "\n\n")
          ;; Add some properties to make it easier to read
          (add-text-properties start (point) '(face font-lock-comment-face)))))))

(defun relysium-toggle-debug-mode ()
  "Toggle elysium debug mode."
  (interactive)
  (setq relysium-debug-mode (not relysium-debug-mode))
  (message "Elysium debug mode %s" (if relysium-debug-mode "enabled" "disabled"))
  (when relysium-debug-mode
    (display-buffer (get-buffer-create relysium-debug-buffer-name))))

(defun relysium-clear-debug-buffer ()
  "Clear the elysium debug buffer."
  (interactive)
  (when-let ((buffer (get-buffer relysium-debug-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "[%s] Debug buffer cleared\n\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S"))))))


(defun relysium-render-template (template-string variables)
  "Render TEMPLATE-STRING by replacing variables from VARIABLES plist.
VARIABLES should be a plist where properties are :var-name and values follow.
Returns the processed string with all variables replaced."
  (with-temp-buffer
    (insert template-string)
    (goto-char (point-min))
    (while (re-search-forward "\\${\\([^}]+\\)}" nil t)
      (let* ((var-name (match-string 1))
             (var-key (intern (concat ":" var-name)))
             (var-value (plist-get variables var-key))
             (replacement (cond
                           ((null var-value) (format "[undefined:%s]" var-name))
                           ((functionp var-value) (funcall var-value))
                           (t (if (stringp var-value)
                                  var-value
                                (format "%s" var-value))))))
        (replace-match replacement t t)))
    (buffer-string)))

(provide 'relysium-utils)
;;; relysium-utils.el ends here
