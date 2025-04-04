;;; relysium-suggest.el --- Suggestions functionality for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the functions related to getting and applying AI-generated
;; code suggestions to the current buffer using the relysium package.

;;; Code:

(require 'gptel)
(require 'relysium-context)
(require 'relysium-prompt-suggest)
(require 'relysium-buffer)
(require 'relysium-extraction)
(require 'relysium-patch)
(require 'relysium-commands)
(require 'relysium-utils)

;;;###autoload
(defun relysium-suggest (additional-prompt)
  "Send whole buffer to LLM for code improvement suggestions.
The LLM will return suggestions in XML format that will be applied to the buffer.
ADDITIONAL-PROMPT allows users to provide specific instructions."
  (interactive "sInstruction: ")

  (let* ((code-buffer (current-buffer))
         (chat-buffer (relysium-buffer-get-chat-buffer))
         ;; Get context for the whole buffer
         (context (relysium-context-gather))
         ;; Build prompts using our specialized builders
         (system-prompt (relysium-prompt-suggest-system))
         (user-prompt (relysium-prompt-suggest-user context additional-prompt)))

    ;; Update chat buffer with the query
    (relysium-buffer-append-user-message user-prompt)
    (relysium-debug-log "System prompt: %s" system-prompt)
    (relysium-debug-log "Context: %s" context)
    (relysium-debug-log "User prompt: %s" user-prompt)

    ;; Update status and send request
    (with-current-buffer chat-buffer
      (gptel--sanitize-model)
      (gptel--update-status " Waiting..." 'warning))

    (message "Requesting code suggestions from %s..." (gptel-backend-name gptel-backend))

    (gptel-request user-prompt
      :system system-prompt
      :buffer chat-buffer
      :callback (apply-partially #'relysium-handle-suggestions code-buffer))))

(defun relysium-handle-suggestions (code-buffer response info)
  "Handle the XML suggestions RESPONSE from gptel.
The suggestions will be applied to CODE-BUFFER.
INFO is passed from the `gptel-request' function."
  (when response
    ;; Log the full response if debug mode is enabled
    (relysium-debug-log "LLM Suggestion Response:\n%s" response)

    ;; Add response to the chat buffer
    (relysium-buffer-append-assistant-message response)

    ;; Extract suggestions using shared extraction module
    (let ((suggestions (relysium-extraction-suggestions response)))
      ;; Log the extracted suggestions if debug mode is enabled
      (relysium-debug-log "Extracted suggestions: %s"
                          (if suggestions
                              (format "%s" suggestions)
                            "None found"))

      (if suggestions
          (with-current-buffer code-buffer
            ;; Mark undo boundary before making changes
            (undo-boundary)

            ;; Apply the changes using shared patch module
            (relysium-patch-apply code-buffer suggestions)

            ;; Activate smerge mode and show transient menu
            (smerge-mode 1)
            (goto-char (point-min))
            (ignore-errors (smerge-next))
            (relysium-transient-menu)
            (message "Applied %d suggestion(s). Review with the merge menu." (length suggestions)))
        (message "No applicable suggestions found.")))

    ;; Update status
    (let ((chat-buffer (plist-get info :buffer)))
      (with-current-buffer chat-buffer
        (gptel--update-status " Ready" 'success)))))

(provide 'relysium-suggest)
;;; relysium-suggest.el ends here
