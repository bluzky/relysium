;;; relysium-ask.el --- Ask functionality for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the functions related to asking questions about code
;; without making changes, using the relysium LLM integration.
;; It uses simple-template.el for template rendering.

;;; Code:

(require 'relysium-utils)
(require 'relysium-context)
(require 'relysium-buffer)
(require 'relysium-core)
(require 'simple-template)

;; Ask-specific template components
(defvar relysium-prompt-ask-system
  "You are an expert programmer and coding assistant.
Your task is to provide helpful, accurate, and relevant information about the code provided.
Be concise yet thorough in your explanations.
Your answers should be directly related to the code provided and the question asked.
Do not explain too much or provide unnecessary details EXCEPT when asked.

TOOLS GUIDELINES:
- You have access to tools, but only use them when necessary. If a tool is not required, respond as normal.
- Please DON'T be so aggressive in using tools, as many tasks can be better completed without tools.
- Before using tool, summarize what you will do and why.
- After using tool, summarize what you did successfully and what didn't.
"
  "System prompt for ask command.")

(defvar relysium-prompt-ask-user
  "{{if using_region}}
Selected code (lines ${start_line}-${end_line}):
```${language_name}
${selected_code}
```
{{else}}
File: ${buffer_name}
```${language_name}
${buffer_content}
```
{{endif}}

Question: ${user_query}"
  "User prompt template for ask command, using simple-template format.")

;;;###autoload
(defun relysium-ask (question)
  "Ask a question about the selected code region.
If no region is selected, the entire buffer content is used."
  (interactive "sAsk about code: ")

  (let* ((context (relysium-context-gather))
         ;; Add user query to the context
         (template-context (plist-put context :user_query question))
         ;; Use simple-template to render the user prompt
         (user-prompt (simple-template-render-template
                       relysium-prompt-ask-user
                       template-context)))

    ;; Update chat buffer with the query
    (relysium-buffer-append-user-message user-prompt)

    ;; Send request to LLM
    (relysium-core-request
     (list :context context
           :system-prompt relysium-prompt-ask-system
           :user-prompt user-prompt
           :response-handler #'relysium-core-process-chat-block))

    ;; Set up window and display
    (relysium-buffer-setup-windows t)))

(provide 'relysium-ask)
;;; relysium-ask.el ends here
