;;; relysium-ask.el --- Ask functionality for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the functions related to asking questions about code
;; without making changes, using the relysium LLM integration.

;;; Code:

(require 'gptel)
(require 'relysium-context)
(require 'relysium-prompt-ask)
(require 'relysium-buffer)
(require 'relysium-extraction)
(require 'relysium-utils)

;;;###autoload
(defun relysium-ask (user-prompt)
  "Ask a question about the selected code region"
  (interactive "sAsk about code: ")
  (if (not (use-region-p))
      (message "Please select a region of code first")

    (let* ((chat-buffer (relysium-buffer-get-chat-buffer))
           ;; Get context using our unified function - only include selected code
           (context (relysium-context-gather t))
           ;; Build prompts using our specialized builders
           (system-prompt relysium-prompt-ask-system)
           (user-prompt-text (relysium-prompt-ask-user context user-prompt)))

      (relysium-debug-log "Context:\n%s" context)
      (relysium-debug-log "System Prompt:\n%s" system-prompt)
      (relysium-debug-log "User Prompt:\n%s" user-prompt-text)

      ;; Update chat buffer with the query
      (relysium-buffer-append-user-message user-prompt-text)

      ;; Show the chat window, and focus back to the code buffer
      (relysium-buffer-setup-windows t)

      ;; Update status and send request
      (with-current-buffer chat-buffer
        (gptel--sanitize-model)
        (gptel--update-status " Waiting..." 'warning))

      (message "Asking LLM about selected code...")

      (gptel-request user-prompt-text
        :system system-prompt
        :buffer chat-buffer
        :callback (apply-partially #'relysium-ask-callback chat-buffer)))))

(defun relysium-ask-callback (chat-buffer response _info)
  "Handle the RESPONSE from LLM for relysium-ask.
CHAT-BUFFER is the buffer to append the response to.
_INFO is unused but required by the gptel callback interface."
  (relysium-debug-log "LLM Response:\n%s" response)
  (when response
    ;; Process response (though for ask, we generally use it as-is)

    ;; Append the response to the chat buffer
    (relysium-buffer-append-assistant-message response chat-buffer)

    ;; Update status in the chat buffer
    (with-current-buffer chat-buffer
      (gptel--update-status " Ready" 'success))

    (message "LLM response received")))

(provide 'relysium-ask)
;;; relysium-ask.el ends here
