;;; relysium-context.el --- Context gathering for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains utilities for gathering context from the current buffer,
;; including code selection, cursor position, and language information.

;;; Code:

(defun relysium-context-gather (&optional selection-only)
  "Gather context information from the current buffer.
When SELECTION-ONLY is non-nil, only include selected code.
Returns a plist with context information."
  (let* ((using-region (use-region-p))
         (cursor-pos (point))
         (cursor-line (line-number-at-pos cursor-pos))
         (file-type (symbol-name major-mode))
         (lang-name (replace-regexp-in-string "-mode$\\|-ts-mode$" "" file-type))

         ;; Region information
         (start-pos (if using-region (region-beginning) cursor-pos))
         (end-pos (if using-region (region-end) cursor-pos))

         ;; Get exact line positions
         (start-line-pos (save-excursion
                           (goto-char start-pos)
                           (line-beginning-position)))
         (end-line-pos (save-excursion
                         (goto-char end-pos)
                         (if (and (> end-pos start-pos)
                                  (= (line-beginning-position) end-pos))
                             ;; If at beginning of line, use previous line's end
                             (progn (backward-char 1)
                                    (line-end-position))
                           (line-end-position))))

         ;; Selected code and line numbers
         (selected-code (buffer-substring-no-properties start-line-pos end-line-pos))
         (start-line (line-number-at-pos start-line-pos))
         (end-line (line-number-at-pos end-line-pos))

         ;; Whole buffer content if needed
         (buffer-content (unless selection-only
                           (buffer-substring-no-properties (point-min) (point-max))))

         ;; Apply trimming and line adjustment for regions
         (adjustment-result (when using-region
                              (relysium-context-trim-adjust selected-code start-line end-line))))

    ;; Build the context plist
    (list :using-region using-region
          :cursor-pos cursor-pos
          :cursor-line cursor-line
          :file-type file-type
          :language-name lang-name
          :start-line (if adjustment-result (nth 1 adjustment-result) start-line)
          :end-line (if adjustment-result (nth 2 adjustment-result) end-line)
          :selected-code (if adjustment-result (nth 0 adjustment-result) selected-code)
          :buffer-content buffer-content)))

(defun relysium-context-trim-adjust (string start-line end-line)
  "Trim leading and trailing empty lines and adjust line numbers.
STRING is the text to trim, START-LINE and END-LINE are the line numbers.
Returns a list with the trimmed string and adjusted line numbers."
  (with-temp-buffer
    ;; Insert the string into a temporary buffer
    (insert string)

    ;; Count leading empty lines
    (goto-char (point-min))
    (let ((leading-empty-lines 0)
          (trailing-empty-lines 0)
          (adjusted-start-line start-line)
          (adjusted-end-line end-line)
          (trimmed-string nil))

      ;; Count leading empty lines
      (while (and (not (eobp))
                  (looking-at "^\\s-*$"))
        (setq leading-empty-lines (1+ leading-empty-lines))
        (forward-line 1))

      ;; Count trailing empty lines
      (goto-char (point-max))
      (when (not (bobp))
        (forward-line -1))
      (while (and (not (bobp))
                  (looking-at "^\\s-*$"))
        (setq trailing-empty-lines (1+ trailing-empty-lines))
        (forward-line -1))

      ;; Adjust start and end line numbers
      (setq adjusted-start-line (+ start-line leading-empty-lines))
      (setq adjusted-end-line (- end-line trailing-empty-lines))

      ;; Extract the trimmed string
      (goto-char (point-min))
      (when (> leading-empty-lines 0)
        (forward-line leading-empty-lines)
        (delete-region (point-min) (point)))

      (goto-char (point-max))
      (when (> trailing-empty-lines 0)
        (forward-line (- trailing-empty-lines))
        (delete-region (point) (point-max)))

      (setq trimmed-string (buffer-string))

      ;; Return a list with the trimmed string and adjusted line numbers
      (list trimmed-string adjusted-start-line adjusted-end-line))))

(provide 'relysium-context)
;;; relysium-context.el ends here
