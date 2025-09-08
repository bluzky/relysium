;;; relysium-generate.el --- Prompts for code generation from comments -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the prompt components and builders for the code generation
;; from comments functionality.
;; Uses simple-template.el for template rendering.

;;; Code:

(require 'relysium-utils)
(require 'relysium-core)
(require 'relysium-context)
(require 'relysium-commands)
(require 'relysium-prompt-template)
(require 'simple-template)

;; Common components that could be shared across commands
(defvar relysium-prompt-generate-system
  "Act as an expert software developer specialized in the current programming language.
Your task is to analyze special comments that start with 'AI:' and generate code to fulfill those tasks.
Follow these instructions precisely:

1. Look for comments that start with 'AI:' followed by a task description
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
8. The suggestion must seamlessly integrate into the existing code without breaking it syntax and semantics

TOOLS GUIDELINES: Do not use tools unless necessary. If a tool is not required, respond as normal.

Example:

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
</suggestion>

${templates.suggestion_format}")

;; User prompt template using simple-template format
(defvar relysium-prompt-generate-user-template
  "File type: ${language_name}
{{if using_region}}
Selected region (lines: ${start_line} - ${end_line}):

```${language_name}
${source_code}
```
{{else}}
Source code:

```${language_name}
${source_code}
```
{{endif}}

Please analyze the code and look for comments that start with 'AI:'.
Generate code suggestions to fulfill the tasks described in these comments.")

;;;###autoload
(defun relysium-generate-from-comments ()
  "Generate code from comments that start with 'AI:'."
  (interactive)

  (let* ((context (relysium-context-gather))
         (code-to-format (if (plist-get context :using_region)
                             (plist-get context :selected_code)
                           (plist-get context :buffer_content)))
         (formatted-code (relysium-format-with-line-numbers
                          code-to-format
                          (if (plist-get context :using_region)
                              (plist-get context :start_line) 1)))

         ;; Prepare template context
         (template-context (append context (list :source_code formatted-code
                                                 :templates relysium-base-templates)))

         (system-prompt (simple-template-render-template
                         relysium-prompt-generate-system
                         template-context))
         ;; Render the user prompt template
         (user-prompt (simple-template-render-template
                       relysium-prompt-generate-user-template
                       template-context)))

    ;; Store the current context in the request for later use
    (relysium-core-request
     (list :context context
           :system-prompt system-prompt
           :user-prompt user-prompt
           :response-handler #'relysium-core-process-suggestions))))

(provide 'relysium-generate)
;;; relysium-generate.el ends here
