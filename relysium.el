;;; relysium.el --- Automatically apply LLM-created code-suggestions -*- lexical-binding: t; -*-

;; No Copyright for my changes :v

;; Author: Daniel Nguyen <bluesky.1289@gmail.com>
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0") (transient "0.4.0"))
;; URL: https://github.com/bluzky/relysium/
;; Keywords: convenience, tools, llm, code-assistance, ai

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

;; Core modules
(require 'relysium-context)
(require 'relysium-buffer)
(require 'relysium-extraction)
(require 'relysium-patch)
(require 'relysium-commands)

;; Command modules - these will internally require their prompt modules
(require 'relysium-edit)
(require 'relysium-ask)
(require 'relysium-suggest)
(require 'relysium-complete-cursor)
(require 'relysium-generate)

;; Debugging utilities
(require 'relysium-utils)

(defgroup relysium nil
  "Apply code changes using gptel."
  :group 'hypermedia)

;;;###autoload
(defun relysium-edit-dwim (user-query)
  "Intelligent AI assistance based on context.
If a region is active, edits the selected code using USER-QUERY.
Otherwise, completes code at the current cursor position.

This provides a single entry point for the most common AI assistance tasks."
  (interactive "sTask description: ")
  (if (use-region-p)
      ;; With active region, use edit to modify selected code
      (relysium-edit user-query)
    ;; Without region, use complete-at-point for code completion
    (relysium-complete-cursor user-query)))


;;;###autoload
(define-minor-mode relysium-prog-mode
  "Minor mode for elysium in programming modes.
Provides keybindings and integration for elysium code assistance."
  :lighter " Elysium"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-<return>") 'relysium-edit-dwim)
            (define-key map (kbd "C-c a") 'relysium-ask)
            (define-key map (kbd "C-c e t") 'relysium-buffer-toggle-window)
            (define-key map (kbd "C-c e b") 'relysium-buffer-add-context)
            (define-key map (kbd "C-c e a") 'relysium-ask)
            (define-key map (kbd "C-c e c") 'relysium-buffer-clear)
            (define-key map (kbd "C-c e D") 'relysium-toggle-debug-mode)
            (define-key map (kbd "C-c e d") 'relysium-debug-log)
            (define-key map (kbd "C-c e m") 'relysium-transient-menu)
            (define-key map (kbd "C-c e s") 'relysium-suggest)
            (define-key map (kbd "C-c e p") 'relysium-complete-at-point)
            (define-key map (kbd "C-c e w") 'relysium-buffer-switch-to-chat)
            (define-key map (kbd "C-c e g") 'relysium-generate-from-comments)
            map))

(transient-define-prefix relysium-transient-menu ()
  "Elysium actions menu."
  ["Actions"
   :class transient-row
   ("n" "Next" relysium-navigate-next-change)
   ("p" "Prev" relysium-navigate-prev-change)
   ("a" "Accept" relysium-keep-current-change)
   ("d" "Reject" relysium-discard-current-change)
   ("RET" "Accept all" relysium-keep-all-changes)
   ("x" "Discard all" relysium-discard-all-changes)
   ("r" "Retry" relysium-retry-query)
   ("q" "Quit" transient-quit-one)])


;; Autoload the main commands
;;;###autoload (autoload 'relysium-edit "relysium-edit" nil t)
;;;###autoload (autoload 'relysium-ask "relysium-ask" nil t)
;;;###autoload (autoload 'relysium-suggest "relysium-suggest" nil t)
;;;###autoload (autoload 'relysium-generate-from-comments "relysium-generate" nil t)

(provide 'relysium)

;;; relysium.el ends here
