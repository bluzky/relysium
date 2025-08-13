;;; relysium-edit.el --- Edit prompts for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the prompt components and builders for the edit command.
;; Uses simple-template.el for template rendering.

;;; Code:

(require 'relysium-utils)
(require 'relysium-core)
(require 'relysium-context)
(require 'relysium-commands)
(require 'simple-template)


;; Common components that could be shared across commands
(defvar relysium-prompt-edit-base
  "Act as an expert software developer.
Always use best practices when coding.
Respect and use existing conventions, libraries, etc that are already present in the code base.

Make sure code comments are in English when generating them.
Your task is to modify the provided code according to the user's request. Follow these instructions precisely:

TOOLS GUIDELINES: Donot use tools unless necessary. If a tool is not required, respond as normal."
  "Base system prompt for edit commands.")

(defvar relysium-prompt-edit-format
  "Response format rules:
   - *DO NOT* include three backticks: ``` in your suggestion! Treat the suggested code AS IS.
   - The code you return must be wrapped in <code></code>, and cannot contain any other <code>."
  "Format instructions for edit responses.")

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

Remember that Your response SHOULD CONTAIN ONLY THE MODIFIED CODE to be used as DIRECT REPLACEMENT to the original file."
  "Guidelines for code modifications.")

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
</code>"
  "Example for edit command.")

(defvar relysium-prompt-edit-user-template
  "Language: ${language_name}
{{if using_region}}
Line range: ${start_line}-${end_line}

Selected code:
```${language_name}
${selected_code}
```
{{else}}
Cursor position: line ${cursor_line}

Full file content:
```${language_name}
${buffer_content}
```
{{endif}}

Your task: ${user_query}"
  "User prompt template for edit command, using simple-template format.")

;; System prompt builder for edit command
(defun relysium-prompt-edit-system ()
  "Build the system prompt for edit command."
  (relysium-build-prompt
   (list
    :a_intro relysium-prompt-edit-base
    :b_format relysium-prompt-edit-format
    :c_guidelines relysium-prompt-edit-guidelines
    :d_example relysium-prompt-edit-example)))


;;;###autoload
(defun relysium-edit (user-query)
  "Send USER-QUERY to relysium from the current buffer.
Edits the selected region or suggests code at cursor position."
  (interactive (list (read-string "User Query: ")))

  (let* ((context (relysium-context-gather))
         ;; Add user query to the context
         (template-context (plist-put context :user_query user-query))
         (system-prompt (relysium-prompt-edit-system))
         (user-prompt (simple-template-render-template
                       relysium-prompt-edit-user-template
                       template-context)))

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
