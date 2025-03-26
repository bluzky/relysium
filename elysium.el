;;; elysium.el --- Automatically apply LLM-created code-suggestions -*- lexical-binding: t; -*-

;; No Copyright for my changes :v

;; Author: Daniel Nguyen <bluesky.1289@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))
;; URL: https://github.com/bluzky/relysium/

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

(defvar elysium-ask-prompt
  "You are an expert programmer and coding assistant.
Your task is to provide helpful, accurate, and relevant information about the code provided.
Be concise yet thorough in your explanations.
Your answers should be clear, informative, and directly related to the code provided.
Your answer should be short and focus ONLY on the questions asked.")

(defvar elysium-edit-prompt "Your task is to create exact code modifications with explicit line number ranges.
Act as an expert software developer.
Always use best practices when coding.
Respect and use existing conventions, libraries, etc that are already present in the code base.

Make sure code comments are in English when generating them.

Your task is to modify the provided code according to the user's request. Follow these instructions precisely:

1. Line numbering requirements:
   - Count EVERY line starting from 1, including all empty lines, comments, and whitespace lines
   - Line ranges are ALWAYS inclusive (both start_line and end_line are part of the replacement)
   - For single-line changes, use identical numbers for start and end (e.g., 42-42)

2. Response rules:
   - *DO NOT* include three backticks: ``` in your suggestion! Treat the suggested code AS IS.
   - The code you return must be wrapped in <code></code>, and cannot contain any other code.
   - *MUST* strictly follows format:
   Replace lines: {start_line}-{end_line}
   <code>
   {complete_replacement_code}
   </code>

3. Code modification rules:
   - *DO NOT* include any explanations, comments.
   - Ensure the returned code is complete and can be directly used as a replacement for the original code.
   - Preserve the original structure, indentation, and formatting of the code as much as possible.
   - Only modify the specific lines requested in the range - no more, no less
   - Maintain the *SAME INDENTATION* in the returned code as in the source code
   - *ONLY* return the new code snippets to be updated, *DO NOT* return the entire file content.

4. Multi-change handling:
   - For multiple distinct changes, provide separate 'Replace lines:' blocks for each change
   - Do not overlap line ranges between different changes
   - List changes in ascending line number order

Remember that Your response SHOULD CONTAIN ONLY THE MODIFIED CODE to be used as DIRECT REPLACEMENT to the original file.

There is an example below:

Original code:
```python
def add(a, b):
    return a + b

result = add(2, 3)
print(result)
```

Selected code:
Line range: 1-2
```python
def add(a, b):
    return a + b
```

User request:
Print the result

Your response:
Replace lines: 1-1
<code>
def add(a, b):
    print(a + b)
</code>
")

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
(defun elysium-ask (user-prompt)
  "Ask a question about the selected code region"
  (interactive "sAsk about code: ")
  (if (not (use-region-p))
      (message "Please select a region of code first")
    ;; Region is selected, proceed with LLM query
    (unless (buffer-live-p elysium--chat-buffer)
      (setq elysium--chat-buffer (gptel "*elysium*")))

    (let* ((chat-buffer elysium--chat-buffer)
           (selected-code (buffer-substring-no-properties (region-beginning) (region-end)))
           (file-type (symbol-name major-mode))
           (lang-name (replace-regexp-in-string "-mode$\\|-ts-mode$" "" file-type))
           ;; Create the full prompt with code context
           (full-prompt (format "Code (%s):\n```%s\n%s\n```\n\nQuestion: %s"
                                lang-name
                                lang-name
                                selected-code
                                user-prompt)))

      ;; Update chat buffer with the query
      (with-current-buffer chat-buffer
        (goto-char (point-max))
        (insert "\n\n### USER:\n")
        (insert full-prompt)
        (insert "\n"))

      ;; Show the chat window
      (elysium-setup-windows)

      ;; Update status and send request
      (gptel--update-status " Waiting..." 'warning)
      (message "Asking LLM about selected code...")
      (deactivate-mark)

      (gptel-request full-prompt
        :system elysium-ask-prompt
        :buffer chat-buffer
        :callback 'elysium-ask-callback))))

(defun elysium-ask-callback (response _info)
  "Handle the RESPONSE from LLM for elysium-ask.
_INFO is unused but required by the gptel callback interface."
  (when response
    (with-current-buffer elysium--chat-buffer
      (goto-char (point-max))
      (insert "\n\n### ASSISTANT:\n")
      (insert response)
      (insert "\n")

      ;; Update status
      (gptel--sanitize-model)
      (gptel--update-status " Ready" 'success))
    (message "LLM response received")))

(defun trim-empty-lines-and-adjust (string start-line end-line)
  "Trim leading and trailing empty lines and adjust line numbers."
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
         (selected-code (buffer-substring-no-properties start-line-pos end-line-pos))
         (start-line (line-number-at-pos start-line-pos))
         (end-line (line-number-at-pos end-line-pos))
         (cursor-line (line-number-at-pos (point)))
         (file-type (symbol-name major-mode))
         ;; Apply trimming and line adjustment
         (adjustment-result (when using-region
                              (trim-empty-lines-and-adjust selected-code start-line end-line)))
         (final-code (if using-region (nth 0 adjustment-result) selected-code))
         (final-start-line (if using-region (nth 1 adjustment-result) start-line))
         (final-end-line (if using-region (nth 2 adjustment-result) end-line))
         ;; Include indentation info in the query
         (full-query (format "\n\nFile type: %s\nLine range: %d-%d\nCursor line: %d\n%s\n\nCode:\n```\n%s\n```\n\n%s"
                             file-type
                             final-start-line
                             final-end-line
                             cursor-line
                             ""
                             final-code
                             user-query)))

    (setq elysium--last-query user-query)
    (setq elysium--last-code-buffer code-buffer)

    ;; Store region info for later use
    (setq-local elysium--using-region using-region)
    (setq-local elysium--region-start-line final-start-line)
    (setq-local elysium--region-end-line final-end-line)

    (with-current-buffer chat-buffer
      (goto-char (point-max))
      (insert "\n\n### USER:\n")
      (insert full-query)
      (insert "\n"))

    (gptel--update-status " Waiting..." 'warning)
    (message "Querying %s for lines %d-%d..."
             (gptel-backend-name gptel-backend)
             final-start-line final-end-line)
    (deactivate-mark)
    (with-current-buffer chat-buffer
      (goto-char (point-max))
      (insert user-query)
      (insert "\n\n"))

    (gptel-request full-query
      :system elysium-edit-prompt
      :buffer chat-buffer
      :callback (apply-partially #'elysium-handle-response code-buffer))))

(defun elysium-handle-response (code-buffer response info)
  "Handle the RESPONSE from gptel.
The changes will be applied to CODE-BUFFER in a git merge format.
INFO is passed into this function from the `gptel-request' function."
  (when response
    ;; Log the full response if debug mode is enabled
    (elysium-debug-log "LLM Response:\n%s" response)

    ;; Add this section to show the full response in the chat buffer
    (with-current-buffer elysium--chat-buffer
      (goto-char (point-max))
      (insert "\n\n### ASSISTANT:\n")
      (insert response)
      (insert "\n\n### "))

    (let* ((extracted-data (elysium-extract-changes response))
           (changes (plist-get extracted-data :changes))
           (using-region (buffer-local-value 'elysium--using-region code-buffer)))

      ;; Log the extracted changes if debug mode is enabled
      (when elysium-debug-mode
        (elysium-debug-log "Extracted %d change(s)" (length changes)))

      ;; mark undo boundary
      (undo-boundary)


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
  "Extract the code-changes from RESPONSE.
Replace lines: 1-2
<code>
{Code Change}
</code>

Replace lines: 4-4
<code>
{Code Change}
</code>"
  (let ((changes '())
        (start 0)
        (code-block-regex
         "Replace [Ll]ines:? \\([0-9]+\\)-\\([0-9]+\\)\n<code>\n\\(\\(?:.\\|\n\\)*?\\(?:\n\\)\\)</code>"))
    (while (string-match code-block-regex response start)
      (let ((change-start (string-to-number (match-string 1 response)))
            (change-end (string-to-number (match-string 2 response)))
            (code (match-string 3 response)))
        (push (list :start change-start
                    :end change-end
                    :code code)
              changes)

        ;; Update start index in the response string
        (setq start (match-end 0))))
    (list :changes (nreverse changes))))

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
                 (new-code (plist-get change :code))
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
            (if (and (> (length (split-string orig-code "\n")) 1)
                     (> (length (split-string new-code "\n")) 1))
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
  (let* ((orig-lines (split-string orig-code "\n"))
         (new-lines (split-string new-code "\n"))
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
          (let ((orig-text (string-join orig-chunk-lines "\n"))
                (new-text (string-join new-chunk-lines "\n")))
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
        (current-new-chunk nil))

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
                (push (list 'diff (reverse current-orig-chunk) (reverse current-new-chunk)) chunks)
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
                (when orig-line
                  (push orig-line current-orig-chunk))
                (when new-line
                  (push new-line current-new-chunk))
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
  (undo)
  (smerge-mode -1)
  (message "All suggested changes discarded"))

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
    (define-key map (kbd "C-c a") 'elysium-ask)
    (define-key map (kbd "C-c e t") 'elysium-toggle-window)
    (define-key map (kbd "C-c e a") 'elysium-add-context)
    (define-key map (kbd "C-c e c") 'elysium-ask)
    (define-key map (kbd "C-c e d") 'elysium-clear-buffer)
    (define-key map (kbd "C-c e L") 'elysium-toggle-debug-mode)
    (define-key map (kbd "C-c e l") 'elysium-debug-log)
    (define-key map (kbd "C-c e m") 'elysium-transient-menu)
    map)
  "Keymap for elysium in programming modes.")

;; Set up a minor mode to attach the keymap
(define-minor-mode elysium-prog-mode
  "Minor mode for elysium in programming modes."
  :lighter " Elysium"
  :keymap elysium-prog-mode-map)


(provide 'elysium)

;;; elysium.el ends here
