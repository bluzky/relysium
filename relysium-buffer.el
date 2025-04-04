;;; relysium-buffer.el --- Buffer management for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains buffer management functionality for the relysium package.
;; It handles the creation and management of a single chat buffer that gets
;; reassociated with different code buffers as the user switches between them.

;;; Code:

(require 'gptel)

(defvar relysium--active-chat-buffer nil
  "The currently active relysium chat buffer.")

(defvar relysium--active-code-buffer nil
  "The code buffer currently associated with the active chat buffer.")

(defvar relysium--chat-window nil
  "The window displaying the chat buffer.")

(defvar relysium--chat-window-active nil
  "Flag to track if the chat window is currently visible.")

(defcustom relysium-window-size 0.33
  "Size of the elysium chat window as a fraction of the frame.
Must be a number between 0 and 1, exclusive."
  :type 'float
  :group 'relysium
  :set (lambda (symbol value)
         (if (and (numberp value)
                  (< 0 value 1))
             (set-default symbol value)
           (user-error "Relysium-window-size must be a number between 0 and 1, exclusive"))))

(defcustom relysium-window-style 'vertical
  "Specify the orientation.  It can be 'horizontal, 'vertical, or nil."
  :type '(choice (const :tag "Horizontal" horizontal)
                 (const :tag "Vertical" vertical)
                 (const :tag "None" nil)))

;; Setup kill-buffer-hook to clean up chat buffers
(defun relysium-cleanup-on-buffer-kill ()
  "Kill associated chat buffer when the code buffer is killed."
  (when (and relysium--active-chat-buffer
             (buffer-live-p relysium--active-chat-buffer)
             (eq (current-buffer) relysium--active-code-buffer))
    ;; Clear association with this buffer
    (setq relysium--active-code-buffer nil)
    ;; If chat window is not active, we can kill the chat buffer
    (unless relysium--chat-window-active
      (kill-buffer relysium--active-chat-buffer)
      (setq relysium--active-chat-buffer nil))))

;; Add buffer tracking when switching buffers
(defun relysium-update-chat-on-buffer-switch ()
  "Update chat window when switching buffers if chat window is active."
  (when (and relysium--chat-window-active
             (window-live-p relysium--chat-window)
             ;; Make sure we're in a buffer that would have a chat buffer (typically code)
             (derived-mode-p 'prog-mode)
             ;; Don't activate for temporary or special buffers
             (not (string-prefix-p " " (buffer-name)))
             (not (string-prefix-p "*" (buffer-name)))
             ;; Only update if we switched to a different buffer
             (not (eq (current-buffer) relysium--active-code-buffer)))

    ;; Update the active code buffer reference
    (setq relysium--active-code-buffer (current-buffer))

    ;; Update chat buffer title to match new code buffer
    (when (buffer-live-p relysium--active-chat-buffer)
      (with-current-buffer relysium--active-chat-buffer
        (rename-buffer (format "*relysium:%s*" (buffer-name relysium--active-code-buffer)))))

    ;; Show updated chat buffer in chat window
    (when (window-live-p relysium--chat-window)
      (with-selected-window relysium--chat-window
        (switch-to-buffer relysium--active-chat-buffer)))))

;;;###autoload
(defun relysium-buffer-get-chat-buffer ()
  "Get or create the chat buffer for the current code buffer.
Returns a single chat buffer that's reassociated with the current code buffer."
  ;; Create chat buffer if it doesn't exist or if it's not live
  (unless (and relysium--active-chat-buffer
               (buffer-live-p relysium--active-chat-buffer))
    ;; Create the chat buffer
    (setq relysium--active-chat-buffer (gptel (format "*relysium:%s*" (buffer-name))))

    ;; Hide it from buffer list and set up
    (with-current-buffer relysium--active-chat-buffer
      ;; Using buffer-list nil hides the buffer from buffer-list functions
      (setq-local buffer-list-update-hook
                  (cons (lambda () (setq list-buffers-directory nil))
                        buffer-list-update-hook))
      (set-buffer-modified-p nil)
      ;; Mark the buffer as auxiliary (hidden in many UIs)
      (when (fboundp 'doom-mark-buffer-as-real-h)
        (unwind-protect
            (doom-mark-buffer-as-real-h)
          ;; avoid error if using doom
          (ignore-errors 'doom-mark-buffer-as-real-h)))
      (set-buffer-modified-p nil)))

  ;; Associate chat buffer with current code buffer
  (setq relysium--active-code-buffer (current-buffer))

  ;; Ensure kill-buffer-hook is set up (but only once)
  (remove-hook 'kill-buffer-hook #'relysium-cleanup-on-buffer-kill t)
  (add-hook 'kill-buffer-hook #'relysium-cleanup-on-buffer-kill nil t)

  relysium--active-chat-buffer)

;;;###autoload
(defun relysium-buffer-setup-windows (&optional keep-focus)
  "Set up the coding assistant layout with the shared chat window.
When KEEP-FOCUS is non-nil, keep the focus on the code buffer after setup."
  (let* ((code-buffer (current-buffer))
         (chat-buffer (relysium-buffer-get-chat-buffer))
         (main-window (selected-window)))

    (when relysium-window-style
      (delete-other-windows)

      (let ((split-size (floor (* (if (eq relysium-window-style 'vertical)
                                      (frame-width)
                                    (frame-height))
                                  (- 1 relysium-window-size)))))
        (if (eq relysium-window-style 'vertical)
            (split-window-right split-size)
          (split-window-below split-size))
        (set-window-buffer main-window code-buffer)
        (other-window 1)
        (set-window-buffer (selected-window) chat-buffer)

        ;; Set flag to track chat window visibility
        (setq relysium--chat-window-active t)
        ;; Store the chat window reference
        (setq relysium--chat-window (selected-window))

        ;; Remove hook before adding to avoid duplicates
        (remove-hook 'buffer-list-update-hook #'relysium-update-chat-on-buffer-switch)
        (add-hook 'buffer-list-update-hook #'relysium-update-chat-on-buffer-switch)

        ;; Move focus back to the code buffer if requested
        (when keep-focus
          (other-window 1))))))

;;;###autoload
(defun relysium-buffer-toggle-window ()
  "Toggle the chat window."
  (interactive)
  (if (and relysium--active-chat-buffer
           (get-buffer-window relysium--active-chat-buffer))
      (progn
        ;; Remove buffer switch hook when closing window
        (setq relysium--chat-window-active nil)
        (setq relysium--chat-window nil)
        (remove-hook 'buffer-list-update-hook #'relysium-update-chat-on-buffer-switch)
        (delete-window (get-buffer-window relysium--active-chat-buffer)))
    (progn
      (relysium-buffer-setup-windows))))

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
         (code-buffer-language
          (string-trim-right
           (string-trim-right (symbol-name major-mode) "-ts-mode$") "-mode$")))
    (with-current-buffer chat-buffer
      (goto-char (point-max))
      (insert "\n")
      (insert (format "```%s\n%s\n```" code-buffer-language content))
      (insert "\n"))))

;;;###autoload
(defun relysium-buffer-switch-to-chat ()
  "Switch to the chat buffer."
  (interactive)
  (let ((chat-buffer (relysium-buffer-get-chat-buffer)))
    (switch-to-buffer chat-buffer)))

;;;###autoload
(defun relysium-buffer-append-user-message (message &optional buffer)
  "Append user MESSAGE to the chat buffer.
If BUFFER is non-nil, use it; otherwise, use the shared chat buffer."
  (let ((chat-buffer (or buffer (relysium-buffer-get-chat-buffer))))
    (with-current-buffer chat-buffer
      (goto-char (point-max))
      (insert "\n\n### USER:\n")
      (insert message)
      (insert "\n"))))

;;;###autoload
(defun relysium-buffer-append-assistant-message (message &optional buffer)
  "Append assistant MESSAGE to the chat buffer.
If BUFFER is non-nil, use it; otherwise, use the shared chat buffer."
  (let ((chat-buffer (or buffer (relysium-buffer-get-chat-buffer))))
    (with-current-buffer chat-buffer
      (goto-char (point-max))
      (insert "\n\n### ASSISTANT:\n")
      (insert message)
      (insert "\n\n### "))))

(provide 'relysium-buffer)
;;; relysium-buffer.el ends here
