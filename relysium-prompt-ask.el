;;; relysium-prompt-ask.el --- Ask prompts for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the prompt components and builders for the ask command.

;;; Code:

(require 'relysium-prompt-utils)

;; Ask-specific components
(defvar relysium-prompt-ask-system
  "You are an expert programmer and coding assistant.
Your task is to provide helpful, accurate, and relevant information about the code provided.
Be concise yet thorough in your explanations.
Your answers should be short, clear and directly related to the code provided and the question asked.
Do not explain too much or provide unnecessary details EXCEPT when asked."
  "System prompt for ask command.")

;; User prompt builder for ask command
(defun relysium-prompt-ask-user (context question)
  "Build an ask user prompt with CONTEXT and QUESTION."
  (let* ((lang-name (plist-get context :language-name))
         (selected-code (plist-get context :selected-code)))

    (relysium-build-prompt
     (list
      :a_info (format "Code (%s):" lang-name)
      :b_code (relysium-format-code-block lang-name selected-code)
      :c_question (format "Question: %s" question)))))

(provide 'relysium-prompt-ask)
;;; relysium-prompt-ask.el ends here
