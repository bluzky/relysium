;;; relysium-prompt-template.el --- Configuration for Relysium prompt settings  -*- lexical-binding: t -*-

;; Author: Your Name
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience
;; URL: https://github.com/yourusername/relysium-prompt

;;; Commentary:
;; This package provides configuration settings for the Relysium prompt system.
;; It includes a variable to define the format of suggestions returned by the system.

;;; Code:
(defvar relysium-prompt-template-multi-suggestion-format
  "RESPONSE FORMAT RULES:
- Return each suggestion in XML format using <suggestion> tags with these attributes:
  - start_row: The starting row of the original code where the new code should be inserted of replaced (starting from 1, inclusive)
  - end_row: The ending row of the original code where the code should be inserted or replaced (starting from 1, inclusive)
  - action: Either 'insert' or 'replace'
  - reason: (optional) Brief explanation of why this change is suggested
- For insert action, use start_row=end_row (insert at the beginning of the specified line), DO NOT include the original line in the suggestion.
- For replace action: if replacing lines 10-12, use start_row=10, end_row=12
- IMPORTANT: If a suggestion would change the same line(s) as another suggestion, combine them into one suggestion instead of creating overlapping ranges
- Example of invalid overlapping suggestions: One suggesting changing line 5, another suggesting changing lines 5-7
- Sort all suggestions by start_row in ascending order (from top to bottom of the file)
- Each suggestion should look like this:
  <suggestion start_row=\"10\" end_row=\"10\" action=\"insert\">
  // Generated code here
  </suggestion>
- DO NOT include explanations outside the suggestion tags")

(defvar relysium-base-templates (list
                                 :suggestion_format relysium-prompt-template-multi-suggestion-format))

(provide 'relysium-prompt-template)

;;; relysium-prompt-template.el ends here
