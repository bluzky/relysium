;;; relysium-edit.el --- Edit functionality for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the functions related to editing code through
;; LLM-generated suggestions using the relysium package.

;;; Code:

(require 'gptel)
(require 'relysium-context)
(require 'relysium-prompt-edit)  ; Import the edit prompt module
(require 'relysium-buffer)
(require 'relysium-patch)        ; Hypothetical unified patch module
(require 'relysium-commands)

(defvar relysium-retry-query)

(defvar relysium--last-query nil
  "The last query sent to the LLM.")

(defvar relysium--last-code-buffer nil
  "The buffer that was last modified by Elysium.")

;;;###autoload
(defun relysium-edit (user-query)
  "Send USER-QUERY to elysium from the current buffer."
  (interactive (list (read-string "User Query: ")))

  (let* ((code-buffer (current-buffer))
         (chat-buffer (relysium-buffer-get-chat-buffer))
         ;; Get context using our unified function
         (context (relysium-context-gather))
         ;; Build prompts using our specialized builders
         (system-prompt (relysium-prompt-edit-system))
         (user-prompt (relysium-prompt-edit-user context user-query)))

    ;; Store for later use
    (setq relysium--last-query user-query)
    (setq relysium--last-code-buffer code-buffer)

    ;; Store region info locally in the chat buffer
    (with-current-buffer chat-buffer
      (setq-local relysium--using-region (plist-get context :using-region))
      (setq-local relysium--region-start-line (plist-get context :start-line))
      (setq-local relysium--region-end-line (plist-get context :end-line)))

    ;; Update chat buffer and send request
    (relysium-buffer-append-user-message user-prompt)

    (with-current-buffer chat-buffer
      (gptel--sanitize-model)
      (gptel--update-status " Waiting..." 'warning))

    (message "Querying %s for lines %d-%d..."
             (gptel-backend-name gptel-backend)
             (plist-get context :start-line)
             (plist-get context :end-line))
    (deactivate-mark)

    (gptel-request user-prompt
      :system system-prompt
      :buffer chat-buffer
      :callback (apply-partially #'relysium-handle-response code-buffer))))

(defun relysium-handle-response (code-buffer response info)
  "Handle the RESPONSE from gptel.
The changes will be applied to CODE-BUFFER in a git merge format.
INFO is passed into this function from the `gptel-request' function."
  (when response
    ;; Log the full response if debug mode is enabled
    (relysium-debug-log "LLM Response:\n%s" response)

    ;; Add this section to show the full response in the chat buffer
    (relysium-buffer-append-assistant-message response)

    ;; Use shared extraction utilities instead of local function
    (let ((code-change (relysium-extraction-code-block response))
          (chat-buffer (plist-get info :buffer)))

      ;; Log the extracted code if debug mode is enabled
      (relysium-debug-log "Extracted code change: %s"
                          (if code-change
                              (format "%s" code-change)
                            "None found"))

      ;; Apply change if code was extracted
      (when code-change
        ;; mark undo boundary
        (with-current-buffer code-buffer
          (undo-boundary))

        ;; Get context from the chat buffer's local variables
        (let* ((using-region (buffer-local-value 'relysium--using-region chat-buffer))
               (start-line (buffer-local-value 'relysium--region-start-line chat-buffer))
               (end-line (buffer-local-value 'relysium--region-end-line chat-buffer))
               ;; Create change plist
               (change (list :action 'replace
                             :start start-line
                             :end (if using-region (1+ end-line) start-line) ;; Use exclusive range
                             :code code-change)))

          ;; Apply using shared patch utilities
          (relysium-patch-apply code-buffer (list change))

          ;; Activate smerge mode and show transient menu
          (with-current-buffer code-buffer
            (smerge-mode 1)
            (goto-char (point-min))
            (ignore-errors (smerge-next))
            ;; Set retry function to relysium-retry-query
            (setq relysium-retry-query 'relysium-retry-query)
            (relysium-transient-menu)))))

    ;; Update status
    (with-current-buffer chat-buffer
      (gptel--update-status " Ready" 'success))
    ))

(defun relysium-retry-query ()
  "Retry the last query with modifications, preserving the previously marked region."
  (interactive)
  (let ((new-query (read-string "Modify query: " relysium--last-query)))
    (when new-query
      (with-current-buffer relysium--last-code-buffer
        ;; Discard current suggestions
        (relysium-discard-all-suggested-changes)

        ;; Restore the region if a region was previously used
        (let ((chat-buffer (relysium-buffer-get-chat-buffer)))
          (when (buffer-local-value 'relysium--using-region chat-buffer)
            (let* ((point-min (point-min))
                   (start-line (buffer-local-value 'relysium--region-start-line chat-buffer))
                   (end-line (buffer-local-value 'relysium--region-end-line chat-buffer))
                   start-pos end-pos)
              ;; Set point to start line
              (setq start-pos (goto-char point-min))
              (forward-line (1- start-line))
              (setq start-pos (point))
              ;; Set mark to end line
              (goto-char point-min)
              (forward-line (1- end-line))
              (end-of-line)
              (setq end-pos (point))
              (set-mark start-pos))))

        ;; Execute the new query
        (relysium-edit new-query)))))

(provide 'relysium-edit)
;;; relysium-edit.el ends here
