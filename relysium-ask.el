;;; relysium-ask.el --- Ask functionality for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the functions related to asking questions about code
;; without making changes, using the relysium LLM integration.

;;; Code:

(require 'relysium-utils)
(require 'relysium-context)
(require 'relysium-buffer)
(require 'relysium-core)

;; Ask-specific components
(defvar relysium-prompt-ask-system
  "You are an expert programmer and coding assistant.
Your task is to provide helpful, accurate, and relevant information about the code provided.
Be concise yet thorough in your explanations.
Your answers should be short, clear and directly related to the code provided and the question asked.
Do not explain too much or provide unnecessary details EXCEPT when asked."
  "System prompt for ask command.")

(defvar relysium-prompt-ask-user
  "Selected code:
```${language-name}
${selected-code}
```

Question: ${user-query}")


;;;###autoload
(defun relysium-ask (question)
  "Ask a question about the selected code region."
  (interactive "sAsk about code: ")
  (if (not (use-region-p))
      (message "Please select a region of code first")
    (let* ((context (relysium-context-gather t))
           (system-prompt relysium-prompt-ask-system)
           (user-prompt-text (relysium-render-template relysium-prompt-ask-user (plist-put context :user-query question))
                             ))

      (relysium-core-request
       (list :context context
             :system-prompt system-prompt
             :user-prompt user-prompt-text))
      (relysium-buffer-setup-windows t)
      )))

(provide 'relysium-ask)
;;; relysium-ask.el ends here
