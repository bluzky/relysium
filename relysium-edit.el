;;; relysium-edit.el --- Edit prompts for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the prompt components and builders for the edit command.

;;; Code:

(require 'relysium-utils)
(require 'relysium-core)
(require 'relysium-context)
(require 'relysium-commands)


;; Common components that could be shared across commands
(defvar relysium-prompt-edit-base
  "Act as an expert software developer.
Always use best practices when coding.
Respect and use existing conventions, libraries, etc that are already present in the code base.

Make sure code comments are in English when generating them.
Your task is to modify the provided code according to the user's request. Follow these instructions precisely:")

(defvar relysium-prompt-edit-format
  "Response format rules:
   - *DO NOT* include three backticks: ``` in your suggestion! Treat the suggested code AS IS.
   - The code you return must be wrapped in <code></code>, and cannot contain any other <code>.")

;; Edit-specific components
(defvar relysium-prompt-edit-guidelines
  "Code modification rules:
   - *DO NOT* include any explanations, comments.
   - Ensure the returned code is complete and can be directly used as a replacement for the original code.
   - Only modify the specific lines requested in the range - no more, no less
   - Maintain the *SAME INDENTATION* in the returned code as in the source code
   - *ONLY* return the new code snippets to be updated, *DO NOT* return the entire file content.
   - If no selected code is provided, *DO NOT* return the entire file content or any surrounding code.
   - If no selected code is provided, suggest code modifications at the cursor position. Carefully analyze the original code, paying close attention to its structure and the cursor position

Remember that Your response SHOULD CONTAIN ONLY THE MODIFIED CODE to be used as DIRECT REPLACEMENT to the original file.")

(defvar relysium-prompt-edit-example
  "There is an example below:

Selected code:
Line range: 1-2
```python
def add(a, b):
    return a + b
```

User request:
Modify code to print the result

Your response:
<code>
def add(a, b):
    print(a + b)
    return a + b
</code>")

(defvar relysium-prompt-edit-user-template
  "Language: ${language-name}
Line range: ${start-line}-${end-line}

Selected code: ${selected-code}

Your task: ${user-query}")

;; System prompt builder for edit command
(defun relysium-prompt-edit-system ()
  "Build the system prompt for edit command."
  (relysium-build-prompt
   (list
    :a_intro relysium-prompt-edit-base
    :b_format relysium-prompt-edit-format
    :d_guidelines relysium-prompt-edit-guidelines
    :e_example relysium-prompt-edit-example)))


;;;###autoload
(defun relysium-edit (user-query)
  "Send USER-QUERY to relysium from the current buffer."
  (interactive (list (read-string "User Query: ")))

  (let* ((context (relysium-context-gather))
         (system-prompt (relysium-prompt-edit-system))
         (user-prompt (relysium-render-template relysium-prompt-edit-user-template (plist-put context :user-query user-query))))

    (relysium-core-request
     (list :context context
           :system-prompt system-prompt
           :user-prompt user-prompt
           :response-handler #'relysium-core-process-code-block
           :retry-fn #'relysium-retry-query))))

(defun relysium-retry-query ()
  "Retry the last query with modifications."
  (interactive)
  (let ((new-query (read-string "Modify query: " relysium--last-query)))
    (when new-query
      (with-current-buffer relysium--last-code-buffer
        ;; Discard current suggestions
        (relysium-discard-all-changes)

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
