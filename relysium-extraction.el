;;; relysium-extraction.el --- Response extraction utilities for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains functions for extracting various types of content
;; from LLM responses, such as code blocks and XML suggestions.

;;; Code:

(defun relysium-extraction-code-block (response)
  "Extract code from the first <code> block in RESPONSE.
Returns the code as a string, or nil if no code block is found."
  (let ((code-block-regex
         "<code>\n\\(\\(?:.\\|\n\\)*?\\)</code>"))
    ;; Extract just the first code block
    (if (string-match code-block-regex response)
        ;; Return the extracted code as a string
        (match-string 1 response)
      ;; No code block found
      nil)))

(defun relysium-extraction-suggestions (response)
  "Extract XML suggestions from LLM RESPONSE.
Returns a list of suggestion plists or nil if no suggestions found."
  (let ((suggestions nil)
        (start 0))
    (while (string-match "<suggestion\\s-+\\(.*?\\)>" response start)
      (let* ((attrs-str (match-string 1 response))
             (content-start (match-end 0))
             (tag-end (string-match "</suggestion>" response content-start))
             (start-row nil)
             (end-row nil)
             (action nil))

        (when tag-end
          ;; Extract attributes
          (when (string-match "start_row=\"\\([0-9]+\\)\"" attrs-str)
            (setq start-row (string-to-number (match-string 1 attrs-str))))

          (when (string-match "end_row=\"\\([0-9]+\\)\"" attrs-str)
            (setq end-row (string-to-number (match-string 1 attrs-str))))

          (when (string-match "action=\"\\([a-z]+\\)\"" attrs-str)
            (setq action (match-string 1 attrs-str)))

          ;; Extract content
          (when (and start-row end-row)
            (let* ((content (substring response content-start tag-end))
                   (trimmed-content (replace-regexp-in-string "\\`[\n\r]+" ""
                                                              (replace-regexp-in-string "[\n\r]+\\'" "" content)))
                   (exclusive-end-row (if (string= action "replace") (1+ end-row) end-row)))

              ;; Create a plist for this suggestion and add to list
              (push (list :start start-row
                          :end exclusive-end-row
                          :action action
                          :code trimmed-content)
                    suggestions)))
          ;; Update search position
          (setq start (+ tag-end (length "</suggestion>"))))

        ;; If no closing tag found, move past this point to avoid infinite loop
        (unless tag-end
          (setq start (length response)))))

    ;; Return the reversed list to maintain original order
    ;; Only if we found suggestions
    (when suggestions
      (nreverse suggestions))))

(provide 'relysium-extraction)
;;; relysium-extraction.el ends here
