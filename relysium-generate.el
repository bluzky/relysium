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
(require 'relysium-prompt-template)


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
6. The code suggestion MUST REPLACE the 'AI:' comment line
7. Maintain the same indentation and coding style as the surrounding code.
8. The suggestion must seamlessly integrate into the existing code without breaking it syntax and semantics")

(defvar relysium-prompt-generate-example
  "Example:

Given source code with line numbers:
1: def process_data(items):
2:     results = []
3:     # AI: Initialize total and processed counters
4:
5:     # AI: Optimize this loop to use enumerate
6:     for i in range(len(items)):
7:
8:         item = items[i]
9:         # AI: Skip items with status='invalid'
10:
11:        # AI: Calculate score based on value*weight
12:
13:        # AI: Add to results if score > 75
14:
15:     # AI: Return results with summary stats
16:     return results

Your response should be:
<suggestion start_row=\"3\" end_row=\"3\" action=\"replace\">
    total = 0
    processed = 0
</suggestion>

<suggestion start_row=\"5\" end_row=\"8\" action=\"replace\">
    for i, item in enumerate(items):
</suggestion>

<suggestion start_row=\"9\" end_row=\"9\" action=\"replace\">
        if item.get('status') == 'invalid':
            continue
        processed += 1
</suggestion>

<suggestion start_row=\"11\" end_row=\"11\" action=\"replace\">
        score = item.get('value', 0) * item.get('weight', 1)
        total += score
</suggestion>

<suggestion start_row=\"13\" end_row=\"13\" action=\"replace\">
        if score > 75:
            results.append({
                'id': item.get('id'),
                'score': score
            })
</suggestion>

<suggestion start_row=\"15\" end_row=\"15\" action=\"replace\">
    return {
        'results': results,
        'total_score': total,
        'processed': processed,
        'count': len(results)
    }
</suggestion>")

;; Function to build the system prompt
(defun relysium-prompt-generate-system ()
  "Build the system prompt for code generation from comments."
  (relysium-build-prompt
   (list
    :a_intro relysium-prompt-generate-base
    :b_guidelines relysium-prompt-generate-guidelines
    :c_format relysium-prompt-template-multi-suggestion-format
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
      :c_task (format "Please analyze the code and look for comments that start with 'AI:'.\nGenerate code suggestions to fulfill the tasks described in these comments.")))))

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
