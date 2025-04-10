;;; relysium-buffer.el --- Simplified buffer management for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains minimal buffer management functionality for the relysium package.
;; It maintains a single shared chat buffer with essential functions for window management
;; and chat interaction.

;;; Code:

(require 'gptel)

;; Core buffer tracking variables
(defvar relysium--chat-buffer nil
  "The single shared relysium chat buffer.")

(defvar relysium--active-code-buffer nil
  "The code buffer currently associated with the chat buffer.")

(defvar relysium--chat-window nil
  "The window displaying the chat buffer.")

;; Customization options
(defcustom relysium-window-size 0.33
  "Size of the relysium chat window as a fraction of the frame.
Must be a number between 0 and 1, exclusive."
  :type 'float
  :group 'relysium
  :set (lambda (symbol value)
         (if (and (numberp value)
                  (< 0 value 1))
             (set-default symbol value)
           (user-error "Relysium-window-size must be a number between 0 and 1, exclusive"))))

(defcustom relysium-window-style 'vertical
  "Specify the orientation of the chat window.
It can be 'horizontal, 'vertical, or nil."
  :type '(choice (const :tag "Horizontal" horizontal)
                 (const :tag "Vertical" vertical)
                 (const :tag "None" nil))
  :group 'relysium)

;;;###autoload
(defun relysium-buffer-get-chat-buffer ()
  "Get or create the shared chat buffer.
Returns the chat buffer."
  ;; Create the chat buffer if needed
  (unless (and relysium--chat-buffer
               (buffer-live-p relysium--chat-buffer))
    ;; Create the chat buffer with fixed name
    (setq relysium--chat-buffer (gptel "*relysium-chat*"))

    ;; Configure the chat buffer
    (with-current-buffer relysium--chat-buffer
      (set-buffer-modified-p nil)))

  ;; Track active code buffer
  (setq relysium--active-code-buffer (current-buffer))

  ;; Return the chat buffer
  relysium--chat-buffer)

;;;###autoload
(defun relysium-buffer-setup-windows (&optional keep-focus)
  "Set up the coding assistant layout with the shared chat window.
When KEEP-FOCUS is non-nil, keep the focus on the code buffer after setup."
  (let* ((code-buffer (current-buffer))
         (chat-buffer (relysium-buffer-get-chat-buffer))
         (main-window (selected-window)))

    (when relysium-window-style
      (delete-other-windows)

      ;; Calculate window split position
      (let ((split-size (floor (* (if (eq relysium-window-style 'vertical)
                                      (frame-width)
                                    (frame-height))
                                  (- 1 relysium-window-size)))))
        ;; Create the split
        (if (eq relysium-window-style 'vertical)
            (split-window-right split-size)
          (split-window-below split-size))

        ;; Set up the windows
        (set-window-buffer main-window code-buffer)
        (other-window 1)
        (set-window-buffer (selected-window) chat-buffer)

        ;; Jump to the end of the chat
        (with-selected-window (selected-window)
          (goto-char (point-max)))

        ;; Track chat window
        (setq relysium--chat-window (selected-window))

        ;; Return focus to code buffer if requested
        (when keep-focus
          (select-window main-window))))))

;;;###autoload
(defun relysium-buffer-toggle-window ()
  "Toggle the visibility of the chat window."
  (interactive)
  (if (and relysium--chat-buffer
           (get-buffer-window relysium--chat-buffer))
      ;; Close the chat window
      (delete-window (get-buffer-window relysium--chat-buffer))
    ;; Open the chat window
    (relysium-buffer-setup-windows)))

;;;###autoload
(defun relysium-buffer-clear ()
  "Clear the chat buffer."
  (interactive)
  (let ((chat-buffer (relysium-buffer-get-chat-buffer)))
    (with-current-buffer chat-buffer
      (erase-buffer)
      (insert (gptel-prompt-prefix-string)))))

;;;###autoload
(defun relysium-buffer-add-context (content)
  "Add CONTENT as context to the chat buffer."
  (interactive
   (list (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (buffer-substring-no-properties (point-min) (point-max)))))

  (let* ((chat-buffer (relysium-buffer-get-chat-buffer))
         (buffer-name (buffer-name))
         (code-buffer-language
          (string-trim-right
           (string-trim-right (symbol-name major-mode) "-ts-mode$") "-mode$")))

    (with-current-buffer chat-buffer
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "Context from buffer %s:\n" buffer-name))
      (insert (format "```%s\n%s\n```" code-buffer-language content))
      (insert "\n"))))

;;;###autoload
(defun relysium-buffer-append-user-message (message &optional buffer)
  "Append user MESSAGE to the chat buffer.
If BUFFER is non-nil, use it; otherwise, use the shared chat buffer."
  (let ((chat-buffer (or buffer (relysium-buffer-get-chat-buffer))))
    (with-current-buffer chat-buffer
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert message)
      (insert "\n"))))

;;;###autoload
;; AI: update docstring with more details
(defun relysium-buffer-append-assistant-message (message &optional buffer)
  "Append assistant MESSAGE to the chat buffer.
If BUFFER is non-nil, use it; otherwise, use the shared chat buffer."
  (let ((chat-buffer (or buffer (relysium-buffer-get-chat-buffer))))
    (with-current-buffer chat-buffer
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      ;; AI: optimize following insert actions
      (insert "\n### 🤖 ASSISTANT:\n")
      (insert message)
      (insert "\n\n--- \n### 🎙️ USER: "))))

(provide 'relysium-buffer)
;;; relysium-buffer.el ends here
