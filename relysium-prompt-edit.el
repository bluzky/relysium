;;; relysium-prompt-edit.el --- Edit prompts for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the prompt components and builders for the edit command.

;;; Code:

(require 'relysium-prompt-utils)

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

;; System prompt builder for edit command
(defun relysium-prompt-edit-system ()
  "Build the system prompt for edit command."
  (relysium-build-prompt
   (list
    :a_intro relysium-prompt-edit-base
    :b_format relysium-prompt-edit-format
    :d_guidelines relysium-prompt-edit-guidelines
    :e_example relysium-prompt-edit-example)))

;; User prompt builder for edit command
(defun relysium-prompt-edit-user (context user-query)
  "Build an edit user prompt with CONTEXT and USER-QUERY."
  (let* ((lang-name (plist-get context :language-name))
         (using-region (plist-get context :using-region))
         (start-line (plist-get context :start-line))
         (end-line (plist-get context :end-line))
         (cursor-line (plist-get context :cursor-line))
         (selected-code (plist-get context :selected-code))
         (buffer-content (plist-get context :buffer-content)))

    (if using-region
        (relysium-build-prompt
         (list
          :a_file_info (format "File type: %s\nLine range: %d-%d\nCursor line: %d"
                               lang-name start-line end-line cursor-line)
          :b_code (format "Selected code:\n%s"
                          (relysium-format-code-block lang-name selected-code))
          :c_query user-query))

      (relysium-build-prompt
       (list
        :a_file_info (format "File type: %s\nCursor line: %d\n%s"
                             lang-name cursor-line
                             "No code selected - using cursor line as context")
        :b_code (format "Whole source code:\n%s"
                        (relysium-format-code-block lang-name buffer-content))
        :c_query user-query)))))

(provide 'relysium-prompt-edit)
;;; relysium-prompt-edit.el ends here
