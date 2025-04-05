;;; relysium-core.el --- Core request-response flow for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the core request-response cycle that all relysium
;; commands build upon. It standardizes the flow of:
;; 1. Building prompts
;; 2. Sending requests to the LLM
;; 3. Processing responses
;; 4. Applying changes (if needed)

;;; Code:

(require 'gptel)
(require 'relysium-buffer)
(require 'relysium-extraction)
(require 'relysium-patch)
(require 'relysium-utils)
(require 'relysium-commands)

(defvar relysium--last-query nil
  "The last query sent to the LLM.")

(defvar relysium--last-code-buffer nil
  "The buffer that was last modified by relysium.")

;; Shared response handler

(defun relysium-core-shared-handler (code-buffer response info)
  "Shared response handler that performs common operations.
CODE-BUFFER is the original code buffer.
RESPONSE is the LLM's response text.
INFO is additional data from gptel's callback.

This handler:
1. Logs the response
2. Adds the response to the chat buffer
3. Calls the specific handler from info
4. Updates the status
5. Calls any post-callback"
  (when response
    ;; Log the full response if debug mode is enabled
    (relysium-debug-log "LLM Response:\n%s" response)

    ;; Add response to the chat buffer
    (relysium-buffer-append-assistant-message response)

    ;; Call the specific handler if provided
    (when-let ((specific-handler (plist-get info :specific-handler)))
      (funcall specific-handler code-buffer response info))

    ;; Update status
    (with-current-buffer (plist-get info :buffer)
      (gptel--update-status " Ready" 'success))

    ;; Call post-callback if provided
    (when-let ((post-callback (plist-get info :post-callback)))
      (funcall post-callback code-buffer response info))))

;; Built-in response processors for different response types
(defun relysium-core-process-code-block (code-buffer response info)
  "Process a code block response.
CODE-BUFFER is the original code buffer.
RESPONSE is the LLM's response text.
INFO is additional data."
  ;; Extract code block from response
  (let ((code-block (relysium-extraction-code-block response))
        (chat-buffer (plist-get info :buffer)))

    ;; Log the extracted code if debug mode is enabled
    (relysium-debug-log "Extracted code: %s"
                        (if code-block
                            (format "%s" code-block)
                          "None found"))

    ;; Apply the code if it was extracted
    (if code-block
        (with-current-buffer code-buffer
          ;; Get context from the chat buffer's local variables
          (let* ((using-region (buffer-local-value 'relysium--using-region chat-buffer))
                 (start-line (buffer-local-value 'relysium--region-start-line chat-buffer))
                 (end-line (buffer-local-value 'relysium--region-end-line chat-buffer))
                 ;; Create change plist
                 (change (list :action 'replace
                               :start start-line
                               :end (if using-region (1+ end-line) start-line) ;; Use exclusive range
                               :code code-block)))

            ;; Apply change with undo boundary
            (undo-boundary)
            (relysium-patch-apply code-buffer (list change))

            ;; Set up smerge mode
            (smerge-mode 1)
            (goto-char (point-min))
            (ignore-errors (smerge-next))

            ;; Set retry function if provided
            (when-let ((retry-fn (plist-get info :retry-fn)))
              (setq relysium-retry-query retry-fn))

            ;; Show the transient menu
            (relysium-transient-menu)

            (message "Code change applied. Review with the merge menu.")))
      (message "No applicable code found in response."))))

(defun relysium-core-process-suggestions (code-buffer response info)
  "Process a suggestions response.
CODE-BUFFER is the original code buffer.
RESPONSE is the LLM's response text.
INFO is additional data."
  ;; Extract suggestions using shared extraction module
  (let ((suggestions (relysium-extraction-suggestions response)))
    ;; Log the extracted suggestions if debug mode is enabled
    (relysium-debug-log "Extracted suggestions: %s"
                        (if suggestions
                            (format "%s" suggestions)
                          "None found"))

    (if suggestions
        (with-current-buffer code-buffer
          ;; Apply suggestions with undo boundary
          (undo-boundary)
          (relysium-patch-apply code-buffer suggestions)

          ;; Set up smerge mode
          (smerge-mode 1)
          (goto-char (point-min))
          (ignore-errors (smerge-next))

          ;; Set retry function if provided
          (when-let ((retry-fn (plist-get info :retry-fn)))
            (setq relysium-retry-query retry-fn))

          ;; Show the transient menu
          (relysium-transient-menu)

          (message "Applied %d suggestion(s). Review with the merge menu." (length suggestions)))
      (message "No applicable suggestions found."))))

;; The main request function

(defun relysium-core-request (options)
  "Execute a relysium LLM request with provided OPTIONS.
OPTIONS is a plist that can include:
- :context - The context plist gathered from relysium-context-gather
- :system-prompt - System prompt string
- :user-prompt - User prompt string (user-query will be appended to this)
- :response-handler - Function to process specific response content
- :retry-fn - Function to handle retry requests
- :post-callback - Function called after response is handled with (code-buffer response info) args"
  (let* ((code-buffer (current-buffer))
         (chat-buffer (relysium-buffer-get-chat-buffer))
         (context (plist-get options :context))
         (system-prompt (plist-get options :system-prompt))
         (user-prompt (plist-get options :user-prompt))
         (response-handler (plist-get options :response-handler))
         (post-callback (plist-get options :post-callback))
         (retry-fn (plist-get options :retry-fn)))

    ;; Store for later use
    (setq relysium--last-query user-prompt)
    (setq relysium--last-code-buffer code-buffer)

    ;; Store essential context locally in the chat buffer
    (with-current-buffer chat-buffer
      (setq-local relysium--using-region (plist-get context :using-region))
      (setq-local relysium--region-start-line (plist-get context :start-line))
      (setq-local relysium--region-end-line (plist-get context :end-line))
      (setq-local relysium--cursor-line (plist-get context :cursor-line)))

    ;; Log debug information
    (relysium-debug-log "System Prompt:\n%s" system-prompt)
    (relysium-debug-log "User Prompt:\n%s" user-prompt)

    ;; Update chat buffer with the query
    (relysium-buffer-append-user-message user-prompt)

    ;; Update status and send request
    (with-current-buffer chat-buffer
      (gptel--sanitize-model)
      (gptel--update-status " Waiting..." 'warning))

    (message "Sending request to %s..." (gptel-backend-name gptel-backend))

    ;; Deactivate mark if it's active
    (when (use-region-p)
      (deactivate-mark))

    ;; Send request to LLM
    (gptel-request user-prompt
      :system system-prompt
      :buffer chat-buffer
      :callback (lambda (response info)
                  ;; Add additional data to info
                  (plist-put info :retry-fn retry-fn)
                  (plist-put info :post-callback post-callback)
                  (plist-put info :specific-handler response-handler)

                  ;; Call the shared handler
                  (relysium-core-shared-handler code-buffer response info)))))

(provide 'relysium-core)
;;; relysium-core.el ends here
