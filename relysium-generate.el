;;; relysium-generate.el --- Prompts for code generation from comments -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the prompt components and builders for the code generation
;; from comments functionality.

;;; Code:

(require 'relysium-utils)
(require 'relysium-core)
(require 'relysium-context)
(require 'relysium-commands)


;; Base prompt components
(defvar relysium-prompt-generate-base
  "Act as an expert software developer specialized in the current programming language.
Your task is to analyze special comments that start with 'AI:' and generate code to fulfill those tasks.
Follow these instructions precisely:")

(defvar relysium-prompt-generate-guidelines
  "1. Look for comments that start with 'AI:' followed by a task description
2. Generate appropriate code to fulfill each task
3. If the comment is inside a function:
   - The generated code must seamlessly work with the existing code
   - The code must maintain proper control flow and variable scope
   - It must work with other tasks within the same function
4. If the comment is outside a function:
   - Generate code appropriate for the context
   - Take into account the surrounding code and scope
5. The generated code must be syntactically valid and follow the conventions of the language
6. The code SHOULD replace the 'AI:' comment line
7. Maintain the same indentation and coding style as the surrounding code")

(defvar relysium-prompt-generate-format
  "Response format rules:
- Return each suggestion in XML format using <suggestion> tags with these attributes:
  - start_row: The starting row where the code should be inserted (starting from 1, inclusive)
  - end_row: The ending row where the code should be inserted (starting from 1, inclusive)
  - action: Either 'insert' or 'replace'
- For insert action, use start_row=end_row (insert at the beginning of the specified line)
- For replace action: if replacing lines 10-12, use start_row=10, end_row=12
- Each suggestion should look like this:
  <suggestion start_row=\"10\" end_row=\"10\" action=\"insert\">
  // Generated code here
  </suggestion>
- DO NOT include explanations outside the suggestion tags
- REPLACE the original 'AI:' comments")

(defvar relysium-prompt-generate-example
  "Example:

Given source code with line numbers:
1: function calculateTotal(items) {
2:   let total = 0;
3:   // AI: Calculate sum of all item prices
4:
5:   // AI: Apply tax if total > 100
6:
7:   return total;
8: }

Your response should be:
<suggestion start_row=\"3\" end_row=\"4\" action=\"replace\">
  for (const item of items) {
    total += item.price;
  }
</suggestion>

<suggestion start_row=\"5\" end_row=\"6\" action=\"replace\">
  if (total > 100) {
    total *= 1.08; // Apply 8% tax
  }
</suggestion>")

;; Function to build the system prompt
(defun relysium-prompt-generate-system ()
  "Build the system prompt for code generation from comments."
  (relysium-build-prompt
   (list
    :a_intro relysium-prompt-generate-base
    :b_guidelines relysium-prompt-generate-guidelines
    :c_format relysium-prompt-generate-format
    :d_example relysium-prompt-generate-example)))

;; Function to build the user prompt
(defun relysium-prompt-generate-user (context)
  "Build the user prompt for code generation with CONTEXT."
  (let* ((lang-name (plist-get context :language-name))
         (cursor-line (plist-get context :cursor-line))
         (using-region (plist-get context :using-region))
         (buffer-content (plist-get context :buffer-content))
         (selected-code (plist-get context :selected-code))
         (start-line (plist-get context :start-line))
         (end-line (plist-get context :end-line))
         (code-to-analyze (if using-region selected-code buffer-content))
         (start-line (if using-region (plist-get context :start-line) 1))
         (annotated-code (relysium-format-with-line-numbers code-to-analyze start-line)))

    (relysium-build-prompt
     (list
      :a_file_info (format "File type: %s\n%s"
                           lang-name
                           cursor-line
                           (if using-region
                               (format "\nSelected region: lines %d-%d" start-line end-line)
                             ""))
      :b_code (format "Source code with line numbers:\n%s"
                      (relysium-format-code-block lang-name annotated-code))
      :c_task (format "Please analyze the code and look for comments that start with 'AI:'.\nGenerate code suggestions to fulfill the tasks described in these comments.\nThe suggestions should REPLACE each 'AI:' comment line.")))))

;;;###autoload
(defun relysium-generate-from-comments ()
  "Generate code from comments that start with 'AI:'."
  (interactive)

  (let* ((context (relysium-context-gather))
         (system-prompt (relysium-prompt-generate-system))
         (user-prompt (relysium-prompt-generate-user context)))

    (relysium-core-request
     (list :system-prompt system-prompt
           :context context
           :user-prompt user-prompt
           :response-handler #'relysium-core-process-suggestions))))

(provide 'relysium-generate)
;;; relysium-generate.el ends here
