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


(defvar relysium-prompt--tool-guidelines
  "TOOLS USAGE GUIDE:
  - You have access to tools, but only use them when necessary. If a tool is not required, respond as normal.
  - Please DON'T be so aggressive in using tools, as many tasks can be better completed without tools.
  - Files will be provided to you as context through <file> tag!
  - Before using the `view` tool each time, always repeatedly check whether the file is already in the <file> tag. If it is already there, do not use the `view` tool, just read the file content directly from the <file> tag.
  - If you use the `read_file` tool when file content is already provided in the <file> tag, you will be fired!
  - If you encounter a URL, prioritize using the `read_url` tool to obtain its content.
  - If you have information that you don't know, please proactively use the tools provided by users! Especially the `web_search` tool.
  - When available tools cannot meet the requirements, please try to use the `run_command` tool to solve the problem whenever possible.
  - When attempting to modify a file that is not in the context, please first use the `list_files` tool and `search_files` tool to check if the file you want to modify exists, then use the `view` tool to read the file content. Don't modify blindly!
  - When generating files, first use `list_files` tool to read the directory structure, don't generate blindly!
  - When creating files, first check if the directory exists. If it doesn't exist, create the directory before creating the file.
  - After `web_search` tool returns, if you don't get detailed enough information, do not continue use `web_search` tool, just continue using the `read_url` tool to get more information you need from the links in the search results.
  - For any mathematical calculation problems, please prioritize using the `python` tool to solve them. Please try to avoid mathematical symbols in the return value of the `python` tool for mathematical problems and directly output human-readable results, because large models don't understand mathematical symbols, they only understand human natural language.
  - Do not use the `python` tool to read or modify files! If you use the `python` tool to read or modify files, you will be fired!!!!!
  - Do not use the `bash` tool to read or modify files! If you use the `bash` tool to read or modify files, you will be fired!!!!!
  - If you are provided with the `write_file` tool, there's no need to output your change suggestions, just directly use the `write_file` tool to complete the changes.
  - Before each tool call, explain the reason why you're using the tool
")

(defvar relysium-base-templates (list
                                 :suggestion_format relysium-prompt-template-multi-suggestion-format
                                 :tool_guidelines relysium-prompt--tool-guidelines))

(provide 'relysium-prompt-template)

;;; relysium-prompt-template.el ends here
