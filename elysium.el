;;; elysium.el --- Automatically apply LLM-created code-suggestions -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Lance Bergeron

;; Author: Lance Bergeron <bergeron.lance6@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))
;; URL: https://github.com/lanceberge/elysium/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package extends on gptel.el.  It uses that package to generate code
;; suggestions based on the user's request.  Those code suggestions will then
;; automatically be applied to the buffer in the format of a git merge.
;; After applying changes, it enters smerge-mode and provides a transient menu
;; to approve, reject, or retry with a new query.

;;; Code:
(require 'gptel)
(require 'smerge-mode)
(require 'transient)
(require 'diff-mode)

(defgroup elysium nil
  "Apply code changes using gptel."
  :group 'hypermedia)

(defcustom elysium-apply-changes-hook nil
  "Hook run after code changes have been applied on a buffer."
  :group 'elysium
  :type 'hook)

(defcustom elysium-window-size 0.33
  "Size of the elysium chat window as a fraction of the frame.
Must be a number between 0 and 1, exclusive."
  :type 'float
  :group 'elysium
  :set (lambda (symbol value)
         (if (and (numberp value)
                  (< 0 value 1))
             (set-default symbol value)
           (user-error "Elysium-window-size must be a number between 0 and 1, exclusive"))))

(defcustom elysium-window-style 'vertical
  "Specify the orientation.  It can be \='horizontal, '\=vertical, or nil."
  :type '(choice (const :tag "Horizontal" horizontal)
                 (const :tag "Vertical" vertical)
                 (const :tag "None" nil)))


(defvar elysium--chat-buffer nil
  "Buffer used for LLM interaction.")

(defvar elysium--last-query nil
  "The last query sent to the LLM.")

(defvar elysium--last-code-buffer nil
  "The buffer that was last modified by Elysium.")

(defcustom elysium-debug-mode nil
  "When non-nil, log LLM responses and other debug information."
  :group 'elysium
  :type 'boolean)

(defcustom elysium-debug-buffer-name "*elysium-debug*"
  "Name of the buffer for debug logging."
  :group 'elysium
  :type 'string)


(setq elysium-base-prompt
  (concat
"Your primary task is to suggest code modifications with precise line numbers:\n"
"1. Count ALL lines starting from 1 (including empty lines and comments).\n"
"2. Use this exact format:\n"
"   Replace lines: {start_line}-{end_line}\n"
"   ```{language}\n"
"   {suggested_code}\n"
"   ```\n"
"   {brief explanation}\n"
"3. Important rules:\n"
"   - Line ranges are INCLUSIVE\n"
"   - Make ONLY the requested changes\n"
"   - Maintain original indentation and comments\n"
"   - Include COMPLETE code for the specified range\n"
"   - Use the same language as the question\n"
"4. Verify before submitting:\n"
"   - Line numbers are accurate\n"
"   - All affected lines are included\n"
"   - No unrelated code is modified\n"
"DO NOT show the full content after modifications.\n"
))

(defun elysium-toggle-window ()
  "Toggle the elysium chat window."
  (interactive)
  (if (and (buffer-live-p elysium--chat-buffer)
           (get-buffer-window elysium--chat-buffer))
      (delete-window (get-buffer-window elysium--chat-buffer))

    (elysium-setup-windows)))

(defun elysium-setup-windows ()
  "Set up the coding assistant layout with the chat window."
  (unless (buffer-live-p elysium--chat-buffer)
    (setq elysium--chat-buffer
          (gptel "*elysium*")))

  (when elysium-window-style
    (delete-other-windows)

    (let* ((main-buffer (current-buffer))
           (main-window (selected-window))
           (split-size (floor (* (if (eq elysium-window-style 'vertical)
                                     (frame-width)
                                   (frame-height))
                                 (- 1 elysium-window-size)))))
      (with-current-buffer elysium--chat-buffer)
      (if (eq elysium-window-style 'vertical)
          (split-window-right split-size)
        (split-window-below split-size))
      (set-window-buffer main-window main-buffer)
      (other-window 1)
      (set-window-buffer (selected-window) elysium--chat-buffer))))


;;;###autoload
(defun elysium-query (user-query)
  "Send USER-QUERY to elysium from the current buffer."
  (interactive (list (read-string "User Query: ")))
  (unless (buffer-live-p elysium--chat-buffer)
    (setq elysium--chat-buffer (gptel "*elysium*")))

  (let* ((code-buffer (current-buffer))
         (chat-buffer elysium--chat-buffer)
         (using-region (use-region-p))
         (start-pos (if using-region
                        (region-beginning)
                      (point-min)))
         (end-pos (if using-region
                      (region-end)
                    (point-max)))
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
         (start-line (line-number-at-pos start-line-pos))
         (end-line (line-number-at-pos end-line-pos))
         (selected-code (buffer-substring-no-properties start-line-pos end-line-pos))
         (file-type (symbol-name major-mode))
         ;; Include indentation info in the query
         (full-query (format "\n\nFile type: %s\nLine range: %d-%d\n%s\n\nCode:\n```\n%s\n```\n\n%s"
                             file-type
                             start-line
                             end-line
                             ""
                             selected-code
                             user-query)))

    (setq elysium--last-query user-query)
    (setq elysium--last-code-buffer code-buffer)

    ;; Store region info for later use
    (setq-local elysium--using-region using-region)
    (setq-local elysium--region-start-line start-line)
    (setq-local elysium--region-end-line end-line)

    (gptel--update-status " Waiting..." 'warning)
    (message "Querying %s for lines %d-%d..."
             (gptel-backend-name gptel-backend)
             start-line end-line)
    (deactivate-mark)
    (with-current-buffer chat-buffer
      (goto-char (point-max))
      (insert user-query)
      (insert "\n\n"))

    (gptel-request full-query
      :system elysium-base-prompt
      :buffer chat-buffer
      :callback (apply-partially #'elysium-handle-response code-buffer))))

(defun elysium-handle-response (code-buffer response info)
  "Handle the RESPONSE from gptel.
The changes will be applied to CODE-BUFFER in a git merge format.
INFO is passed into this function from the `gptel-request' function."
  (when response
    ;; Log the full response if debug mode is enabled
    (elysium-debug-log "LLM Response:\n%s" response)

    (let* ((extracted-data (elysium-extract-changes response))
           (changes (plist-get extracted-data :changes))
           (using-region (buffer-local-value 'elysium--using-region code-buffer)))

      ;; Log the extracted changes if debug mode is enabled
      (when elysium-debug-mode
        (elysium-debug-log "Extracted %d change(s)" (length changes))
        (dolist (change changes)
          (elysium-debug-log "Change - Lines %d-%d:\n%s"
                             (plist-get change :start)
                             (plist-get change :end)
                             (plist-get change :code))))

      (when changes
        ;; Apply changes
        (with-current-buffer code-buffer
          ;; Adjust changes if we're working with a region
          (when using-region
            (setq changes (elysium--adjust-changes-for-region changes))
            (when elysium-debug-mode
              (elysium-debug-log "Region-adjusted changes")))

          (elysium-apply-code-changes code-buffer changes)

          ;; Activate smerge mode and show transient menu
          (smerge-mode 1)
          (goto-char (point-min))
          (ignore-errors (smerge-next))
          (elysium-transient-menu)))

      ;; Update status
      (with-current-buffer elysium--chat-buffer
        (gptel--sanitize-model)
        (gptel--update-status " Ready" 'success)))))


(defun elysium-debug-log (message &rest args)
  "Log MESSAGE with ARGS to the debug buffer if debug mode is enabled."
  (when elysium-debug-mode
    (let ((debug-buffer (get-buffer-create elysium-debug-buffer-name)))
      (with-current-buffer debug-buffer
        (goto-char (point-max))
        (let ((start (point)))
          (insert (format "[%s] " (format-time-string "%Y-%m-%d %H:%M:%S")))
          (insert (apply #'format message args))
          (insert "\n\n")
          ;; Add some properties to make it easier to read
          (add-text-properties start (point) '(face font-lock-comment-face)))))))

(defun elysium--adjust-changes-for-region (changes)
  "Adjust CHANGES line numbers based on the selected region.
Makes sure changes are properly aligned with the actual lines in the buffer."
  (when (and (boundp 'elysium--region-start-line)
             (local-variable-p 'elysium--region-start-line)
             elysium--region-start-line)
    ;; We need to offset all line numbers by the start line of the region
    (let ((offset (1- elysium--region-start-line)))
      (mapcar (lambda (change)
                (list :start (+ (plist-get change :start) offset)
                      :end (+ (plist-get change :end) offset)
                      :code (plist-get change :code)))
              changes)))
  changes)

(defun elysium-extract-changes (response)
  "Extract the code-changes and explanations from RESPONSE.
Explanations will be of the format:
{Initial explanation}

1st Code Change:
{Code Change}

2nd Code Change:
{Code Change}"
  (let ((changes '())
        (explanations '())
        (start 0)
        (change-count 0)
        (code-block-regex
         "Replace [Ll]ines:? \\([0-9]+\\)-\\([0-9]+\\)\n```\\(?:[[:alpha:]-]+\\)?\n\\(\\(?:.\\|\n\\)*?\\)```"))
    (while (string-match code-block-regex response start)
      (let ((change-start (string-to-number (match-string 1 response)))
            (change-end (string-to-number (match-string 2 response)))
            (code (match-string 3 response))
            (explanation-text (substring response start (match-beginning 0))))
        ;; the initial explanation won't be preceded by nth Code Change
        (when (not (string-empty-p explanation-text))
          (push (if (= 0 change-count)
                    explanation-text  ; For the first explanation, just use the text as is
                  (format "%s Code Change:\n%s"
                          (elysium--ordinal change-count)
                          explanation-text))
                explanations)
          (setq change-count (1+ change-count)))
        (push (list :start change-start
                    :end change-end
                    :code code)
              changes)

        ;; Update start index in the response string
        (setq start (match-end 0))))

    ;; Add any remaining text as the last explanation
    (let ((remaining-text (substring response start)))
      (when (not (string-empty-p remaining-text))
        (push (if (= 0 change-count)
                  remaining-text
                (format "%s Code Change:\n%s"
                        (elysium--ordinal change-count)
                        remaining-text))
              explanations)))
    (list :explanations (nreverse explanations)
          :changes (nreverse changes))))

(defun elysium-apply-code-changes (buffer code-changes)
  "Apply CODE-CHANGES to BUFFER in a git merge format.
Uses simple conflict markers to highlight the differences between
original and suggested code. Breaks down large changes into smaller chunks
for easier review."
  (with-current-buffer buffer
    (save-excursion
      (let ((offset 0))
        (dolist (change code-changes)
          (let* ((start (plist-get change :start))
                 (end (plist-get change :end))
                 (new-code (string-trim-right (plist-get change :code)))
                 (orig-code-start (progn
                                    (goto-char (point-min))
                                    (forward-line (1- (+ start offset)))
                                    (point)))
                 (orig-code-end (progn
                                  (goto-char (point-min))
                                  (forward-line (1- (+ end offset 1)))
                                  (point)))
                 (orig-code (buffer-substring-no-properties orig-code-start orig-code-end)))

            ;; If the change is multi-line, try to refine the diff
            (if (and (> (length (split-string orig-code "\n" t)) 1)
                     (> (length (split-string new-code "\n" t)) 1))
                (elysium--apply-refined-change orig-code-start orig-code-end orig-code new-code)
              ;; For single-line changes or very small changes, use the simple approach
              (elysium--apply-simple-change orig-code-start orig-code-end orig-code new-code))

            ;; Update offset - We need to recalculate the total lines now
            (let* ((new-line-count (count-lines orig-code-start (point)))
                   (original-line-count (- end start -1)) ; -1 because line range is inclusive
                   (line-diff (- new-line-count original-line-count)))
              (setq offset (+ offset line-diff)))))))
    (run-hooks 'elysium-apply-changes-hook)))

(defun elysium--apply-simple-change (start end orig-code new-code)
  "Apply a simple change with conflict markers.
Replace the region from START to END containing ORIG-CODE with conflict markers
containing both ORIG-CODE and NEW-CODE."
  (delete-region start end)
  (goto-char start)
  (insert (concat "<<<<<<< HEAD\n"
                 orig-code
                 "=======\n"
                 new-code
                 "\n>>>>>>> " (gptel-backend-name gptel-backend) "\n")))

(defun elysium--apply-refined-change (start end orig-code new-code)
  "Apply a refined change that breaks code into smaller conflict chunks.
Replace the region from START to END containing ORIG-CODE with a refined diff
against NEW-CODE, using conflict markers for each meaningful chunk."
  (delete-region start end)
  (goto-char start)

  ;; Split both code blocks into lines
  (let* ((orig-lines (split-string orig-code "\n" t))
         (new-lines (split-string new-code "\n" t))
         (chunks (elysium--create-diff-chunks orig-lines new-lines))
         (insertion-point start))

    ;; Insert each chunk with appropriate conflict markers
    (dolist (chunk chunks)
      (let ((chunk-type (car chunk))
            (orig-chunk-lines (nth 1 chunk))
            (new-chunk-lines (nth 2 chunk)))

        (cond
         ;; Lines that are the same in both versions - no conflict needed
         ((eq chunk-type 'same)
          (let ((text (string-join orig-chunk-lines "\n")))
            (insert text)
            (when (> (length text) 0)
              (insert "\n"))))

         ;; Lines that differ - add conflict markers
         ((eq chunk-type 'diff)
          (let ((orig-text (string-join (reverse orig-chunk-lines) "\n"))
                (new-text (string-join (reverse new-chunk-lines) "\n")))
            (insert "<<<<<<< HEAD\n")
            (when (> (length orig-text) 0)
              (insert orig-text "\n"))
            (insert "=======\n")
            (when (> (length new-text) 0)
              (insert new-text "\n"))
            (insert ">>>>>>> " (gptel-backend-name gptel-backend) "\n"))))))))

(defun elysium--create-diff-chunks (orig-lines new-lines)
  "Create a list of diff chunks between ORIG-LINES and NEW-LINES.
Each chunk is of the form (TYPE ORIG-CHUNK NEW-CHUNK) where:
- TYPE is either 'same or 'diff
- ORIG-CHUNK is a list of lines from the original text
- NEW-CHUNK is a list of lines from the new text

For 'same chunks, ORIG-CHUNK and NEW-CHUNK contain the same lines."
  (let ((chunks nil)
        (i 0)
        (j 0)
        (orig-len (length orig-lines))
        (new-len (length new-lines))
        (current-chunk-type nil)
        (current-orig-chunk nil)
        (current-new-chunk nil)
        (context-lines 1)) ; Number of context lines to keep before/after changes

    ;; Compare lines and build chunks
    (while (or (< i orig-len) (< j new-len))
      (let ((orig-line (when (< i orig-len) (nth i orig-lines)))
            (new-line (when (< j new-len) (nth j new-lines)))
            (lines-match (and (< i orig-len)
                              (< j new-len)
                              (string= (nth i orig-lines) (nth j new-lines)))))

        (if lines-match
            ;; Lines match - they're part of a 'same' chunk
            (progn
              ;; If we were in a 'diff' chunk, finalize it
              (when (eq current-chunk-type 'diff)
                (push (list 'diff current-orig-chunk current-new-chunk) chunks)
                (setq current-orig-chunk nil
                      current-new-chunk nil))

              ;; Add to or start a 'same' chunk
              (if (eq current-chunk-type 'same)
                  (progn
                    (push orig-line current-orig-chunk)
                    (push new-line current-new-chunk))
                (setq current-chunk-type 'same
                      current-orig-chunk (list orig-line)
                      current-new-chunk (list new-line)))

              ;; Move to next lines
              (cl-incf i)
              (cl-incf j))

          ;; Lines don't match - they're part of a 'diff' chunk
          (progn
            ;; If we were in a 'same' chunk, finalize it
            (when (eq current-chunk-type 'same)
              ;; Reverse the lists to restore order
              (push (list 'same (reverse current-orig-chunk) (reverse current-new-chunk)) chunks)
              (setq current-orig-chunk nil
                    current-new-chunk nil))

            ;; Add to or start a 'diff' chunk
            (setq current-chunk-type 'diff)

            ;; The heuristic below finds the 'best' way to advance through the diff
            ;; Look ahead to find matching lines
            (let ((match-distance-i nil)
                  (match-distance-j nil))

              ;; Look ahead in orig-lines to find a match with current new-line
              (when (and new-line (< i orig-len))
                (let ((k 0))
                  (while (and (< (+ i k) orig-len)
                              (< k 10) ; Limit how far we look ahead
                              (not match-distance-i))
                    (when (string= (nth (+ i k) orig-lines) new-line)
                      (setq match-distance-i k))
                    (cl-incf k))))

              ;; Look ahead in new-lines to find a match with current orig-line
              (when (and orig-line (< j new-len))
                (let ((k 0))
                  (while (and (< (+ j k) new-len)
                              (< k 10) ; Limit how far we look ahead
                              (not match-distance-j))
                    (when (string= (nth (+ j k) new-lines) orig-line)
                      (setq match-distance-j k))
                    (cl-incf k))))

              ;; Decide which way to advance based on match distances
              (cond
               ;; If we're at the end of either list, consume the other
               ((>= i orig-len)
                (when new-line
                  (push new-line current-new-chunk)
                  (cl-incf j)))
               ((>= j new-len)
                (when orig-line
                  (push orig-line current-orig-chunk)
                  (cl-incf i)))

               ;; If we found a match in both directions, take the shortest path
               ((and match-distance-i match-distance-j)
                (if (< match-distance-i match-distance-j)
                    (progn
                      (push orig-line current-orig-chunk)
                      (cl-incf i))
                  (push new-line current-new-chunk)
                  (cl-incf j)))

               ;; If we found a match in just one direction, go that way
               (match-distance-i
                (push orig-line current-orig-chunk)
                (cl-incf i))
               (match-distance-j
                (push new-line current-new-chunk)
                (cl-incf j))

               ;; No match found, just advance both
               (t
                (push orig-line current-orig-chunk)
                (push new-line current-new-chunk)
                (cl-incf i)
                (cl-incf j)))))))

      ;; End of main loop
      )

    ;; Finalize the last chunk
    (when current-chunk-type
      (if (eq current-chunk-type 'same)
          (push (list 'same (reverse current-orig-chunk) (reverse current-new-chunk)) chunks)
        (push (list 'diff (reverse current-orig-chunk) (reverse current-new-chunk)) chunks)))

    ;; Return the chunks in correct order
    (reverse chunks)))

(defun elysium-clear-buffer ()
  "Clear the elysium buffer."
  (interactive)
  (with-current-buffer elysium--chat-buffer
    (erase-buffer)
    (insert (gptel-prompt-prefix-string))))

(defun elysium-add-context (content)
  "Add CONTENT as context to the elysium chat buffer."
  (interactive
   (list (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (buffer-substring-no-properties (point-min) (point-max)))))
  ;; Ensure chat buffer exists
  (unless (buffer-live-p elysium--chat-buffer)
    (setq elysium--chat-buffer (gptel "*elysium*")))

  (let ((code-buffer-language
         (string-trim-right
          (string-trim-right (symbol-name major-mode) "-ts-mode$") "-mode$")))
    (with-current-buffer elysium--chat-buffer
      (goto-char (point-max))
      (insert "\n")
      (insert (format "```%s\n%s\n```" code-buffer-language content))
      (insert "\n"))))

(defun elysium-keep-all-suggested-changes ()
  "Keep all of the LLM suggestions."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (funcall #'smerge-keep-lower))
    (while (ignore-errors (not (smerge-next)))
      (funcall #'smerge-keep-lower))
    (smerge-mode -1)
    (message "All suggested changes applied")))

(defun elysium-discard-all-suggested-changes ()
  "Discard all of the LLM suggestions."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (funcall #'smerge-keep-upper))
    (while (ignore-errors (not (smerge-next)))
      (funcall #'smerge-keep-upper))
    (smerge-mode -1)
    (message "All suggested changes discarded")))

(defun elysium-navigate-next-change ()
  "Navigate to the next change suggestion and keep the transient menu active."
  (interactive)
  (if (ignore-errors (smerge-next))
      (message "Navigated to next change")
    (message "No more changes"))
  ;; Keep the transient menu active
  (elysium-transient-menu))

(defun elysium-navigate-prev-change ()
  "Navigate to the previous change suggestion and keep the transient menu active."
  (interactive)
  (if (ignore-errors (smerge-prev))
      (message "Navigated to previous change")
    (message "No more changes"))
  ;; Keep the transient menu active
  (elysium-transient-menu))

(defun elysium-keep-current-change ()
  "Keep the current suggested change and move to the next one."
  (interactive)
  (smerge-keep-lower)
  (if (ignore-errors (not (smerge-next)))
      (progn
        (message "All changes reviewed - no more conflicts")
        (smerge-mode -1))
    (message "Applied change - move to next")
    ;; Keep the transient menu active if there are more changes
    (elysium-transient-menu)))

(defun elysium-reject-current-change ()
  "Reject the current suggested change and move to the next one."
  (interactive)
  (smerge-keep-upper)
  (if (ignore-errors (not (smerge-next)))
      (progn
        (message "All changes reviewed - no more conflicts")
        (smerge-mode -1))
    (message "Rejected change - move to next")
    ;; Keep the transient menu active if there are more changes
    (elysium-transient-menu)))

(defun elysium-retry-query ()
  "Retry the last query with modifications, preserving the previously marked region."
  (interactive)
  (let ((new-query (read-string "Modify query: " elysium--last-query)))
    (when new-query
      (with-current-buffer elysium--last-code-buffer
        ;; Discard current suggestions
        (elysium-discard-all-suggested-changes)

        ;; Restore the region if a region was previously used
        (when (buffer-local-value 'elysium--using-region elysium--last-code-buffer)
          (let* ((point-min (point-min))
                 (start-line (buffer-local-value 'elysium--region-start-line elysium--last-code-buffer))
                 (end-line (buffer-local-value 'elysium--region-end-line elysium--last-code-buffer))
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
            (set-mark start-pos)))

        ;; Execute the new query
        (elysium-query new-query)))))

(defun elysium--ordinal (n)
  "Convert integer N to its ordinal string representation."
  (let ((suffixes '("th" "st" "nd" "rd" "th" "th" "th" "th" "th" "th")))
    (if (and (> n 10) (< n 14))
        (concat (number-to-string n) "th")
      (concat (number-to-string n)
              (nth (mod n 10) suffixes)))))

;; Add command to toggle debug mode
(defun elysium-toggle-debug-mode ()
  "Toggle elysium debug mode."
  (interactive)
  (setq elysium-debug-mode (not elysium-debug-mode))
  (message "Elysium debug mode %s" (if elysium-debug-mode "enabled" "disabled"))
  (when elysium-debug-mode
    (display-buffer (get-buffer-create elysium-debug-buffer-name))))

;; Add command to clear debug buffer
(defun elysium-clear-debug-buffer ()
  "Clear the elysium debug buffer."
  (interactive)
  (when-let ((buffer (get-buffer elysium-debug-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "[%s] Debug buffer cleared\n\n"
                       (format-time-string "%Y-%m-%d %H:%M:%S"))))))


;; Define a transient menu for Elysium with compact layout
(transient-define-prefix elysium-transient-menu ()
  "Elysium actions menu."
  ["Actions"
   :class transient-row
   ("n" "Next" elysium-navigate-next-change)
   ("p" "Prev" elysium-navigate-prev-change)
   ("a" "Accept" elysium-keep-current-change)
   ("d" "Reject" elysium-reject-current-change)
   ("RET" "Accept all" elysium-keep-all-suggested-changes)
   ("x" "Discard all" elysium-discard-all-suggested-changes)
   ("r" "Retry" elysium-retry-query)
   ("q" "Quit" transient-quit-one)])


;; Add key binding for C-<return> (Ctrl+Enter) to trigger elysium-query in prog-mode
(defun elysium-query-dwim ()
  "Query elysium with the region if active, otherwise prompt for a query."
  (interactive)
  (if (use-region-p)
      (call-interactively 'elysium-query)
    (let ((current-prefix-arg '(4))) ; Simulate C-u prefix to prompt for region
      (call-interactively 'elysium-query))))

;; Define a keymap for programming modes
(defvar elysium-prog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") 'elysium-query-dwim)
    (define-key map (kbd "C-c e t") 'elysium-toggle-window)
    (define-key map (kbd "C-c e a") 'elysium-add-context)
    (define-key map (kbd "C-c e c") 'elysium-clear-buffer)
    (define-key map (kbd "C-c e d") 'elysium-toggle-debug-mode)
    (define-key map (kbd "C-c e l") 'elysium-debug-log)
    (define-key map (kbd "C-c e l") 'elysium-transient-menu)
    map)
  "Keymap for elysium in programming modes.")

;; Set up a minor mode to attach the keymap
(define-minor-mode elysium-prog-mode
  "Minor mode for elysium in programming modes."
  :lighter " Elysium"
  :keymap elysium-prog-mode-map)


(provide 'elysium)

;;; elysium.el ends here
