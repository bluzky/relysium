;;; relysium-complete-cursor.el --- Auto-complete at cursor position -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the functions for auto-completing code at the current
;; cursor position, using the context of the entire buffer to understand what
;; code should be generated.

;;; Code:

(require 'gptel)
(require 'relysium-context)
(require 'relysium-buffer)
(require 'relysium-extraction)
(require 'relysium-patch)
(require 'relysium-utils)
(require 'relysium-commands)
(require 'relysium-prompt-complete-cursor)

;; Forward declaration for the compiler
(declare-function relysium-retry-query "relysium-edit.el")

;;;###autoload
(defun relysium-complete-cursor (user-query)
  "Generate code at the current cursor position based on USER-QUERY.
Uses the entire buffer context to understand the task, with a focus on the
cursor line and its surroundings."
  (interactive "sTask description: ")

  (let* ((code-buffer (current-buffer))
         (chat-buffer (relysium-buffer-get-chat-buffer))
         ;; Get context from the entire buffer
         (full-context (relysium-context-gather))
         ;; Extract relevant positions
         (cursor-pos (plist-get full-context :cursor-pos))
         (cursor-line (plist-get full-context :cursor-line))
         ;; Prepare prompt components
         (system-prompt (relysium-prompt-complete-cursor-system))
         (user-prompt (relysium-prompt-complete-cursor-user full-context user-query)))

    ;; Store for later use
    (setq relysium--last-query user-query)
    (setq relysium--last-code-buffer code-buffer)

    ;; Store context locally in the chat buffer
    (with-current-buffer chat-buffer
      (setq-local relysium--cursor-line (plist-get full-context :cursor-line)))

    ;; Update chat buffer with the query
    (relysium-buffer-append-user-message user-prompt)

    ;; Update status and send request
    (with-current-buffer chat-buffer
      (gptel--sanitize-model)
      (gptel--update-status " Waiting..." 'warning))

    (message "Requesting code completion at line %d..." cursor-line)

    (gptel-request user-prompt
      :system system-prompt
      :buffer chat-buffer
      :callback (apply-partially #'relysium-complete-cursor-callback code-buffer))))

(defun relysium-complete-cursor-callback (code-buffer response info)
  "Handle the RESPONSE from LLM for code completion.
Apply the generated code to CODE-BUFFER at the cursor position.
INFO is passed from the `gptel-request` function."
  (when response
    ;; Log the full response if debug mode is enabled
    (relysium-debug-log "LLM Completion Response:\n%s" response)

    ;; Add response to the chat buffer
    (relysium-buffer-append-assistant-message response)

    ;; Extract code block from response
    (let ((code-block (relysium-extraction-code-block response))
          (chat-buffer (plist-get info :buffer)))

      ;; Log the extracted code if debug mode is enabled
      (relysium-debug-log "Extracted code completion: %s"
                          (if code-block
                              (format "%s" code-block)
                            "None found"))

      ;; Apply the completion if code was extracted
      (if code-block
          (with-current-buffer code-buffer
            ;; Mark undo boundary
            (undo-boundary)

            ;; Get the cursor line from chat buffer's local variables
            (let* ((cursor-line (buffer-local-value 'relysium--cursor-line chat-buffer))
                   ;; Create change plist
                   (change (list :action 'replace
                                 :start cursor-line
                                 :end (1+ cursor-line)  ;; Exclusive range
                                 :code code-block)))

              ;; Apply using shared patch utilities
              (relysium-patch-apply code-buffer (list change))

              ;; Activate smerge mode and show transient menu
              (smerge-mode 1)
              (goto-char (point-min))
              (ignore-errors (smerge-next))
              ;; Set the retry query function for the transient menu
              (setq relysium-retry-query 'relysium-retry-completion)
              (relysium-transient-menu)
              (message "Code completion applied. Review with the merge menu.")))
        (message "No applicable code completion found."))

      ;; Update status
      (with-current-buffer chat-buffer
        (gptel--update-status " Ready" 'success))
      )
    ))

;; Function to retry the completion with a modified query
(defun relysium-retry-completion ()
  "Retry the code completion with a modified query."
  (interactive)
  (let ((new-query (read-string "Modify completion task: " relysium--last-query)))
    (when new-query
      (with-current-buffer relysium--last-code-buffer
        ;; Discard current suggestions
        (relysium-discard-all-changes)

        ;; Execute the new query
        (relysium-complete-cursor new-query)))))

;; Forward declaration for the compiler
(defvar relysium-retry-query)

;; The retry function is dynamically assigned later in the callback

(provide 'relysium-complete-cursor)
;;; relysium-complete-cursor.el ends here
