;;; relysium-prompt-utils.el --- Prompt utilities for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains utilities for building prompts from components
;; to make prompt management easier and more flexible.

;;; Code:

(defun relysium-build-prompt (components &optional separator)
  "Build a prompt string from COMPONENTS plist.
Each value in the plist will be included in the final prompt.
The keys determine the order: values are sorted alphabetically by key.
SEPARATOR is the string to place between components (defaults to double newline)."
  (let ((sep (or separator "\n\n"))
        (result '()))

    ;; Get all keys and sort them alphabetically
    (let ((keys '()))
      (dolist (prop (nreverse (copy-sequence components)))
        (when (keywordp prop)
          (push prop keys)))

      ;; For each key, add its value to the result if non-nil and non-empty
      (dolist (key (sort keys #'string<))
        (let ((val (plist-get components key)))
          (when (and val (not (string-empty-p val)))
            (push val result)))))

    ;; Join all components with the separator
    (mapconcat #'identity (nreverse result) sep)))

(defun relysium-format-code-block (lang-name code)
  "Format CODE with LANG-NAME as a markdown code block."
  (format "```%s\n%s\n```" lang-name code))

(defun relysium-format-with-line-numbers (code &optional start-line)
  "Format CODE with line numbers for clearer reference.
When START-LINE is provided, begin numbering from that line instead of 1."
  (let* ((start (or start-line 1))
         (lines (split-string code "\n"))
         (result ""))
    (cl-loop for line in lines
             for i from start
             do (setq result (concat result (format "%3d: %s\n" i line))))
    result))

(provide 'relysium-prompt-utils)
;;; relysium-prompt-utils.el ends here
