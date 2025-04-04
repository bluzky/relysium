;;; relysium-patch.el --- Patch application for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains functions for applying code changes to buffers
;; using git-style merge conflict markers.

;;; Code:

(require 'simple-diff)

(defcustom relysium-apply-changes-hook nil
  "Hook run after code changes have been applied on a buffer."
  :group 'relysium
  :type 'hook)

(defun relysium-patch-apply (buffer changes)
  "Apply CHANGES to BUFFER in a git merge format.
CHANGES is a list of plists, each with :start, :end, :code, and :action keys.
Uses simple conflict markers to highlight the differences between
original and suggested code."
  (with-current-buffer buffer
    (save-excursion
      (let ((offset 0))
        (dolist (change changes)
          (let* ((start (plist-get change :start))
                 (end (plist-get change :end))
                 (new-code (plist-get change :code))
                 (action (plist-get change :action))
                 ;; Detect insert operations where start == end (exclusive range)
                 (is-insert (= start end))
                 (orig-code-start (progn
                                    (goto-char (point-min))
                                    (forward-line (1- (+ start offset)))
                                    (point)))
                 (orig-code-end (progn
                                  (goto-char (point-min))
                                  (forward-line (1- (+ end offset)))
                                  (point)))
                 (orig-code (buffer-substring-no-properties orig-code-start orig-code-end)))
            (when (string= orig-code "\n")
              (setq orig-code ""))

            (relysium-debug-log "Current offset: %d\nApplying change:\n---%s---\n" offset change)
            (relysium-debug-log "Original code:\n---%s---\n" orig-code)

            ;; Apply the appropriate type of change
            (if is-insert
                ;; Insert-only change
                (relysium--apply-insert-change orig-code-start new-code)
              ;; Normal replacement change
              (if (< (- end start) 15)
                  (relysium--apply-simple-change orig-code-start orig-code-end orig-code new-code)
                (relysium--apply-refined-change orig-code-start orig-code-end orig-code new-code)))

            ;; Update offset - We need to recalculate the total lines now
            (let* ((new-line-count (count-lines orig-code-start (point)))
                   (original-line-count (- end start)) ;; For exclusive ranges
                   (line-diff (- new-line-count original-line-count)))
              (setq offset (+ offset line-diff)))))))
    (run-hooks 'relysium-apply-changes-hook)))

(defun relysium--apply-insert-change (position new-code)
  "Handle insert-only changes with conflict markers.
Insert at POSITION a conflict section containing just the NEW-CODE."
  (goto-char position)
  (insert (concat "<<<<<<< HEAD\n"
                  "=======\n"
                  new-code
                  "\n>>>>>>> Relysium\n")))

(defun relysium--apply-simple-change (start end orig-code new-code)
  "Apply a simple change with conflict markers.
Replace the region from START to END containing ORIG-CODE with conflict markers
containing both ORIG-CODE and NEW-CODE."
  (delete-region start end)
  (goto-char start)
  (insert (concat "<<<<<<< HEAD\n"
                  orig-code
                  "=======\n"
                  new-code
                  "\n>>>>>>> Relysium\n")))

(defun relysium--apply-refined-change (start end orig-code new-code)
  "Apply a refined change that breaks code into smaller conflict chunks.
Replace the region from START to END containing ORIG-CODE with a refined diff
against NEW-CODE, using conflict markers for each meaningful chunk."
  (delete-region start end)
  (goto-char start)
  (insert (simple-diff-merge-strings orig-code new-code "HEAD" "Relysium")))

(provide 'relysium-patch)
;;; relysium-patch.el ends here
