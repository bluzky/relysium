;;; relysium-suggest.el --- Suggest prompts for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the prompt components and builders for the suggest command.
;; The suggest command analyzes code and provides targeted improvements.
;; It can work on either the entire buffer or just a selected region.

;;; Code:

(require 'relysium-utils)
(require 'relysium-core)
(require 'relysium-context)
(require 'relysium-commands)

;; Common components that could be shared across commands
(defvar relysium-prompt-suggest-base
  "Act as an expert software developer with deep knowledge of software design patterns, best practices, and the specific language I'm working in.
Be precise, thoughtful, and comprehensive in your assessment of code.
Focus on:
1. Code quality improvements (readability, maintainability)
2. Performance optimizations
3. Bug prevention and edge case handling
4. Modern patterns and idioms in the language
5. Documentation and clarity enhancements

Your suggestions should respect and enhance existing code structure, naming conventions, and design decisions unless they are clearly problematic.
Never suggest changes that would alter the core functionality unless explicitly requested.")

(defvar relysium-prompt-suggest-format
  "Response format rules:
   - Return each suggestion in XML format using <suggestion> tags with the following attributes:
     - start_row: The starting row of the original code snippet to replace (starting from 1, inclusive)
     - end_row: The ending row of the original code snippet to replace (starting from 1, inclusive)
     - action: Either 'insert' or 'replace'
     - reason: (optional) Brief explanation of why this change is suggested
   - For insert action use start_row=end_row
   - For replace action: if modifying lines 10-12, use start_row=10, end_row=12
   - IMPORTANT: If a suggestion would change the same line(s) as another suggestion, combine them into one suggestion instead of creating overlapping ranges
   - Example of invalid overlapping suggestions: One suggesting changing line 5, another suggesting changing lines 5-7
   - Sort all suggestions by start_row in ascending order (from top to bottom of the file)")

;; Suggest-specific components
(defvar relysium-prompt-suggest-guidelines
  "Code suggestion requirements:
   - CRITICAL: Preserve original indentation and whitespace style precisely
   - CRITICAL: Return suggestions in ascending order by start_row (lowest to highest line numbers)
   - CRITICAL: Suggestion line ranges MUST NOT overlap with each other
   - Each suggestion must be applicable independently
   - Analyze the full context before making suggestions
   - Prioritize changes that have the highest impact on code quality
   - Each suggestion should look like this:
     <suggestion start_row=\"1\" end_row=\"1\" action=\"insert\" reason=\"Adds documentation\">
     new code to be inserted
     </suggestion>
   - DO NOT include explanations or comments outside the suggestion tags
   - Only return the new code to be inserted or replaced
   - Each suggestion is a COMPLETE code snippet that can directly replace the original
   - Combine related changes into one suggestion when logical
   - For function or class suggestions, include the entire definition
   - If two potential suggestions would overlap, choose the more important one or split them into non-overlapping changes
   - Respect existing naming conventions, even if you would personally use different ones
   - Consider linter rules that might be in effect based on codebase style")

(defvar relysium-prompt-suggest-example
  "Example:

Source code with line numbers:
1: def calculate_total(items):
2:     total = 0
3:     for item in items:
4:         total += item
5:     return total

User request:
Improve this code with type hints and better error handling

Your response: (Important: Suggestions should be sorted by start_row and MUST NOT overlap)
<suggestion start_row=\"1\" end_row=\"1\" action=\"replace\" reason=\"Add type hints and docstring\">
def calculate_total(items: list[float]) -> float:
    \"\"\"Calculate the sum of all items in the list.

    Args:
        items: A list of numbers to sum

    Returns:
        The total sum of all items
    \"\"\"
</suggestion>

<suggestion start_row=\"2\" end_row=\"2\" action=\"replace\" reason=\"Initialize with correct type\">
    total: float = 0.0
</suggestion>

<suggestion start_row=\"3\" end_row=\"5\" action=\"replace\" reason=\"Add error handling for non-numeric items\">
    for item in items:
        try:
            total += float(item)
        except (TypeError, ValueError):
            raise TypeError(f\"Expected numeric value, got {type(item).__name__}\")
    return total
</suggestion>")

(defvar relysium-prompt-suggest-contextual-analysis
  "Code Analysis Process:
   1. First examine the code thoroughly, looking for:
      - Unclear or missing documentation
      - Potentially buggy code patterns
      - Inefficient implementations
      - Inconsistent styling
      - Missing error handling
      - Code duplications or repetitions
      - Opportunities for better abstractions
   2. Then consider the user's specific request, prioritizing those aspects
   3. For each potential improvement, consider:
      - The actual value added vs. complexity introduced
      - How the change fits with surrounding code style
      - The risk of introducing bugs or changing behavior
   4. Ensure suggested changes don't create overlapping line ranges
   5. Arrange suggestions in ascending order by line number
   6. Finally, suggest only the changes that would provide clear improvements

   Remember that smaller, targeted suggestions are often more useful than complete rewrites.")

;; Add region specific instructions
(defvar relysium-prompt-suggest-region-guidelines
  "Region-specific instructions:
   - You are analyzing a selected region of a larger file
   - Focus your suggestions ONLY on the code within the provided line range
   - All suggestions MUST be within the bounds of the selection (from start_line to end_line inclusive)
   - Be aware that the selected region may be a partial function or class definition
   - The line numbers in your suggestions should match the absolute line numbers in the file")

;; System prompt builder for suggest command
(defun relysium-prompt-suggest-system (using-region)
  "Build the system prompt for suggest command.
If USING-REGION is non-nil, include region-specific instructions."
  (let ((components (list
                     :a_intro relysium-prompt-suggest-base
                     :b_analysis relysium-prompt-suggest-contextual-analysis
                     :c_format relysium-prompt-suggest-format
                     :d_guidelines relysium-prompt-suggest-guidelines)))

    ;; Add region-specific guidelines if working with a region
    (when using-region
      (setq components (plist-put components :d2_region_guidelines
                                  relysium-prompt-suggest-region-guidelines)))

    ;; Always add the example at the end
    (setq components (plist-put components :e_example relysium-prompt-suggest-example))

    (relysium-build-prompt components)))


(defvar relysium-prompt-suggest-user-template
  "File type: ${language-name}

Source code:
${source-code}

Task: ${user-query}
")

(defvar relysium-prompt-suggest-region-template
  "File type: ${language-name}

Selected region (lines ${start-line} to ${end-line}):
${source-code}

Task: ${user-query}
")

;;;###autoload
(defun relysium-suggest (user-query)
  "Send code to LLM for improvement suggestions.
When a region is active, only that region will be analyzed.
Otherwise, the whole buffer will be analyzed.
USER-QUERY specifies the type of improvements to suggest."
  (interactive "sInstruction: ")

  (let* ((context (relysium-context-gather))
         (using-region (plist-get context :using-region))
         (system-prompt (relysium-prompt-suggest-system using-region))
         (template (if using-region
                       relysium-prompt-suggest-region-template
                     relysium-prompt-suggest-user-template))
         (code-to-format (if using-region
                             (plist-get context :selected-code)
                           (plist-get context :buffer-content)))
         (formatted-code (relysium-format-with-line-numbers
                          code-to-format
                          (if using-region (plist-get context :start-line) 1)))
         (user-prompt (relysium-render-template
                       template
                       (plist-put (plist-put context :user-query user-query)
                                  :source-code formatted-code))))

    ;; Store the current context in the request for later use
    (relysium-core-request
     (list :context context
           :system-prompt system-prompt
           :user-prompt user-prompt
           :response-handler #'relysium-core-process-suggestions
           :retry-fn #'relysium-retry-suggest-query))))

(defun relysium-retry-suggest-query ()
  "Retry the suggest query with modifications."
  (interactive)
  (let ((new-query (read-string "Modify suggestion request: " relysium--last-query)))
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
        (relysium-suggest new-query)))))

(provide 'relysium-suggest)
;;; relysium-suggest.el ends here
