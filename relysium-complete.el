;;; relysium-complete.el --- Prompts for code completion -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the prompt components and builders for the code completion
;; at point functionality.
;; Uses simple-template.el for template rendering.

;;; Code:
(require 'relysium-utils)
(require 'relysium-core)
(require 'relysium-context)
(require 'relysium-commands)
(require 'simple-template)

;; Base prompt components
(defvar relysium-prompt-complete-cursor-system
  "Act as an expert software developer specializing in the current language.
Your task is to generate code that would complete or extend the functionality at the cursor position in the provided code. Follow these instructions precisely:

1. Analyze the code context thoroughly to understand what the user is trying to accomplish.
2. If the cursor is inside a function, generate code that completes that function's logic.
3. If the cursor is outside any function, generate appropriate code based on the user's task description.
4. The generated code MUST be syntactically valid and match the style of the surrounding code.
5. Maintain consistent naming conventions, indentation style, and comment style.

Response format rules:
- Return ONLY the code to be inserted, nothing else.
- Wrap your code in <code></code> tags.
- Place your <code> tag on its own line.
- Place the closing </code> tag on its own line after your code.
- Do NOT include explanations or commentary outside the code tags.
- The code will REPLACE the current line at the cursor position.
- Ensure your response maintains proper indentation relative to the surrounding code.
- If you need to generate a multi-line solution, ensure all lines have correct indentation.

Here's an example interaction:

USER:
File type: python
Cursor line: 12

Full source code:
```python
def calculate_total_price(items, discount_rate=0):
    \"\"\"
    Calculate the total price of items with an optional discount rate.

    Args:
        items: List of dictionaries with 'price' and 'quantity' keys
        discount_rate: Float between 0 and 1 representing discount percentage

    Returns:
        float: Total price after discount
    \"\"\"
    total = 0

    # Calculate total here

    # Apply discount
    final_price = total * (1 - discount_rate)
    return final_price
```

The cursor is positioned at line 12: # Calculate total here

Task: Complete the calculation of the total price by adding up the price * quantity for each item

A:
<code>
    for item in items:
        total += item['price'] * item['quantity']
</code>")

;; User prompt template using simple-template format
(defvar relysium-prompt-complete-cursor-user-template
  "File type: ${language_name}
Cursor line: ${cursor_line}

Full source code:
```${language_name}
${buffer_content}
```

The cursor is positioned at line ${cursor_line}: ${cursor_line_content}

Task: ${user_query}")

;;;###autoload
(defun relysium-complete (user-query)
  "Generate code at the current cursor position based on USER-QUERY."
  (interactive "sTask description: ")

  (let* ((context (relysium-context-gather))
         ;; Get the content of the cursor line
         (cursor-line-content (save-excursion
                                (goto-char (point-min))
                                (forward-line (1- (plist-get context :cursor_line)))
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))))

         ;; Add cursor line content to context for template rendering
         (template-context (append context (list :cursor_line_content cursor-line-content
                                                 :user_query user-query)))

         (system-prompt (relysium-prompt-complete-cursor-system))
         (user-prompt (simple-template-render-template
                       relysium-prompt-complete-cursor-user-template
                       template-context)))

    (relysium-core-request
     (list :context context
           :system-prompt system-prompt
           :user-prompt user-prompt
           :response-handler #'relysium-core-process-code-block
           :retry-fn #'relysium-retry-completion))))

(defun relysium-retry-completion ()
  "Retry the code completion with a modified query."
  (interactive)
  (let ((new-query (read-string "Modify completion task: " relysium--last-query)))
    (when new-query
      (with-current-buffer relysium--last-code-buffer
        ;; Discard current suggestions
        (relysium-discard-all-changes)

        ;; Execute the new query
        (relysium-complete new-query)))))

(provide 'relysium-complete)
;;; relysium-complete.el ends here
