;;; relysium-generate.el --- Generate code from comments -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file provides functionality to generate code from special comments.
;; Users can add comments that start with "AI:" to indicate tasks they want
;; the AI to complete. The AI will analyze these comments and generate
;; appropriate code suggestions.

;;; Code:

(require 'gptel)
(require 'relysium-context)
(require 'relysium-buffer)
(require 'relysium-extraction)
(require 'relysium-patch)
(require 'relysium-utils)
(require 'relysium-prompt-generate)

;; Forward declarations for the compiler
(declare-function relysium-transient-menu "relysium-commands.el")

;;;###autoload
(defun relysium-generate-from-comments ()
  "Generate code from comments that start with 'AI:'.
Analyzes the buffer or selected region for special comments and generates
code suggestions to fulfill the described tasks."
  (interactive)

  (let* ((code-buffer (current-buffer))
         (chat-buffer (relysium-buffer-get-chat-buffer))
         ;; Get context - use the whole buffer if no region is selected
         (context (relysium-context-gather))
         ;; Build prompts using our specialized builders
         (system-prompt (relysium-prompt-generate-system))
         (user-prompt (relysium-prompt-generate-user context)))

    ;; Update chat buffer with the query
    (relysium-buffer-append-user-message user-prompt)

    ;; Log debug information if enabled
    (relysium-debug-log "System prompt: %s" system-prompt)
    (relysium-debug-log "Context: %s" context)
    (relysium-debug-log "User prompt: %s" user-prompt)

    ;; Update status and send request
    (with-current-buffer chat-buffer
      (gptel--sanitize-model)
      (gptel--update-status " Waiting..." 'warning))

    (message "Analyzing comments and generating code...")

    (gptel-request user-prompt
      :system system-prompt
      :buffer chat-buffer
      :callback (apply-partially #'relysium-handle-generated-code code-buffer))))

(defun relysium-handle-generated-code (code-buffer response info)
  "Handle the XML suggestions RESPONSE from gptel.
The generated code will be applied to CODE-BUFFER.
INFO is passed from the `gptel-request' function."
  (when response
    ;; Log the full response if debug mode is enabled
    (relysium-debug-log "LLM Generation Response:\n%s" response)

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
            (message "Generated code for %d comment tasks. Review with the merge menu."
                     (length suggestions)))
        (message "No 'AI:' comments found or unable to generate code suggestions.")))

    ;; Update status
    (let ((chat-buffer (plist-get info :buffer)))
      (with-current-buffer chat-buffer
        (gptel--update-status " Ready" 'success)))))


(provide 'relysium-generate)
;;; relysium-generate.el ends here
