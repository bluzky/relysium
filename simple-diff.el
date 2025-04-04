;;; simple-diff.el --- A simple diff implementation

;;; Commentary:
;; This implements a simplified version of the Patience Diff algorithm
;; for comparing two strings and generating diff output.

;;; Code:

(defun simple-diff-strings (str1 str2 &optional normalize-endings)
  "Compare STR1 and STR2 line by line and return a list of diff operations.
Each operation is a list with format (TYPE LINE-NUMBER CONTENT) where:
TYPE is 'added, 'removed, or 'unchanged
LINE-NUMBER is the line number in the original string
CONTENT is the line content.

If NORMALIZE-ENDINGS is non-nil, convert all line endings to LF before comparing."
  ;; Optionally normalize line endings
  (when normalize-endings
    (setq str1 (replace-regexp-in-string "\r\n" "\n" str1))
    (setq str2 (replace-regexp-in-string "\r\n" "\n" str2)))

  ;; Handle special case of comparing empty strings
  (cond
   ;; Both strings are empty - no diff
   ((and (string-empty-p str1) (string-empty-p str2))
    '())

   ;; First string empty - everything is added
   ((string-empty-p str1)
    (let ((lines (split-string str2 "\n")))
      (cl-loop for line in lines
               for i from 0
               collect (list 'added (1+ i) line))))

   ;; Second string empty - everything is removed
   ((string-empty-p str2)
    (let ((lines (split-string str1 "\n")))
      (cl-loop for line in lines
               for i from 0
               collect (list 'removed (1+ i) line))))

   ;; Normal case - both strings have content
   (t
    (let* ((lines1 (split-string str1 "\n"))
           (lines2 (split-string str2 "\n"))
           (unique-lines1 (simple-diff--find-unique-lines lines1 lines2))
           (unique-lines2 (simple-diff--find-unique-lines lines2 lines1)))
      (simple-diff--compare-regions lines1 lines2 0 0
                                    (length lines1) (length lines2)
                                    unique-lines1 unique-lines2)))))

(defun simple-diff--find-unique-lines (lines1 lines2)
  "Find lines in LINES1 that appear exactly once in both LINES1 and LINES2.
Returns a hash table mapping line content to its index in LINES1."
  (let ((counts (make-hash-table :test 'equal))
        (positions (make-hash-table :test 'equal))
        (result (make-hash-table :test 'equal)))

    ;; Count occurrences in lines1
    (cl-loop for line in lines1
             for idx from 0
             do (progn
                  (puthash line (1+ (gethash line counts 0)) counts)
                  (puthash line idx positions)))

    ;; Only keep lines that appear exactly once in lines1
    (maphash (lambda (line count)
               (when (= count 1)
                 (puthash line (gethash line positions) result)))
             counts)

    ;; Remove lines that don't appear exactly once in lines2
    (setq counts (make-hash-table :test 'equal))
    (cl-loop for line in lines2
             do (puthash line (1+ (gethash line counts 0)) counts))

    (maphash (lambda (line idx)
               (unless (= (gethash line counts 0) 1)
                 (remhash line result)))
             result)

    result))

(defun simple-diff--compare-regions (lines1 lines2 start1 start2 end1 end2
                                     unique-lines1 unique-lines2)
  "Compare regions of LINES1 and LINES2 using patience diff algorithm.
START1, END1, START2, END2 define the regions to compare.
UNIQUE-LINES1 and UNIQUE-LINES2 are hash tables of unique lines.
Returns a list of diff operations."
  (if (or (= start1 end1) (= start2 end2))
      ;; One of the regions is empty, so just return adds/removes
      (append
       (cl-loop for i from start1 below end1
                collect (list 'removed (1+ i) (nth i lines1)))
       (cl-loop for i from start2 below end2
                collect (list 'added (1+ i) (nth i lines2))))

    ;; Find the longest common subsequence of unique lines
    (let ((lcs (simple-diff--find-lcs lines1 lines2 start1 start2 end1 end2
                                     unique-lines1 unique-lines2)))
      (if (null lcs)
          ;; No unique common lines, fall back to basic comparison
          (simple-diff--basic-compare lines1 lines2 start1 start2 end1 end2)

        ;; We have a common line, use it as an anchor
        (let* ((common-line (car lcs))
               (idx1 (gethash common-line unique-lines1))
               (idx2 (gethash common-line unique-lines2))
               (before (simple-diff--compare-regions
                        lines1 lines2 start1 start2 idx1 idx2
                        unique-lines1 unique-lines2))
               (middle (list (list 'unchanged (1+ idx1) common-line)))
               (after (simple-diff--compare-regions
                       lines1 lines2 (1+ idx1) (1+ idx2) end1 end2
                       unique-lines1 unique-lines2)))
          (append before middle after))))))

(defun simple-diff--find-lcs (lines1 lines2 start1 start2 end1 end2
                             unique-lines1 unique-lines2)
  "Find a common line between regions of LINES1 and LINES2.
Returns a list of common lines or nil if none found."
  (let ((common-lines '()))
    (cl-loop for i from start1 below end1
             for line = (nth i lines1)
             when (and (gethash line unique-lines1)
                       (gethash line unique-lines2)
                       (>= (gethash line unique-lines2) start2)
                       (< (gethash line unique-lines2) end2))
             do (push line common-lines))
    (sort common-lines (lambda (a b)
                         (< (gethash a unique-lines1)
                            (gethash b unique-lines1))))))

(defun simple-diff--basic-compare (lines1 lines2 start1 start2 end1 end2)
  "Compare regions line by line without looking for unique anchors.
This is a fallback when no unique common lines are found."
  (let ((result '())
        (i start1)
        (j start2))
    (while (and (< i end1) (< j end2))
      (if (string= (nth i lines1) (nth j lines2))
          (progn
            (push (list 'unchanged (1+ i) (nth i lines1)) result)
            (setq i (1+ i))
            (setq j (1+ j)))
        ;; Lines differ, use a simple heuristic to decide which to advance
        (let ((next-match1 (cl-position (nth j lines2) lines1
                                        :start i :end end1 :test 'string=))
              (next-match2 (cl-position (nth i lines1) lines2
                                        :start j :end end2 :test 'string=)))
          (if (and next-match1 (or (not next-match2) (<= next-match1 next-match2)))
              ;; Add removals until next match
              (progn
                (push (list 'removed (1+ i) (nth i lines1)) result)
                (setq i (1+ i)))
            ;; Add addition
            (progn
              (push (list 'added (1+ j) (nth j lines2)) result)
              (setq j (1+ j)))))))

    ;; Handle remaining lines
    (cl-loop for k from i below end1
             do (push (list 'removed (1+ k) (nth k lines1)) result))
    (cl-loop for k from j below end2
             do (push (list 'added (1+ k) (nth k lines2)) result))

    (nreverse result)))

(defun simple-diff-format (diff-result)
  "Format DIFF-RESULT as a simple diff string with +/- line prefixes."
  (let ((result ""))
    (dolist (entry diff-result)
      (let ((type (car entry))
            (line-num (cadr entry))
            (content (caddr entry)))
        (setq result
              (concat result
                      (cond
                       ((eq type 'added) "+")
                       ((eq type 'removed) "-")
                       (t " "))
                      content "\n"))))
    result))

(defun simple-diff-conflict-format (diff-result &optional marker1 marker2)
  "Format DIFF-RESULT as a conflict-style diff string.
The format is similar to Git's merge conflict markers:
<<<<<<< MARKER1
original content
=======
modified content
>>>>>>> MARKER2

MARKER1 and MARKER2 are optional strings to use in the conflict markers.
If not provided, they default to 'HEAD' and 'MODIFIED'."
  (let ((result "")
        (in-conflict nil)
        (removed-lines '())
        (added-lines '())
        (m1 (or marker1 "HEAD"))
        (m2 (or marker2 "MODIFIED"))
        (has-diff nil))

    ;; Process all diff entries
    (dolist (entry diff-result)
      (let ((type (car entry))
            (line-num (cadr entry))
            (content (caddr entry)))
        (cond
         ;; Unchanged line - output any pending conflict and the unchanged line
         ((eq type 'unchanged)
          (when (or removed-lines added-lines)
            (setq result (concat result (simple-diff--format-conflict-block
                                         removed-lines added-lines m1 m2)))
            (setq removed-lines '())
            (setq added-lines '())
            (setq has-diff t))
          (setq result (concat result content "\n")))

         ;; Removed line - collect for conflict block
         ((eq type 'removed)
          (push content removed-lines))

         ;; Added line - collect for conflict block
         ((eq type 'added)
          (push content added-lines)))))

    ;; Output any final pending conflict
    (when (or removed-lines added-lines)
      (setq result (concat result (simple-diff--format-conflict-block
                                   removed-lines added-lines m1 m2)))
      (setq has-diff t))

    ;; For identical strings, don't add a trailing newline if the original didn't have one
    (if (not has-diff)
        (substring result 0 (if (string= "" result) 0 (1- (length result))))
      result)))

(defun simple-diff--format-conflict-block (removed-lines added-lines marker1 marker2)
  "Format a single conflict block with the given REMOVED-LINES and ADDED-LINES.
MARKER1 and MARKER2 are used for the conflict markers."
  (let ((result ""))
    ;; Start conflict marker
    (setq result (concat result "<<<<<<< " marker1 "\n"))

    ;; Original content (reversed to preserve order)
    (dolist (line (nreverse removed-lines))
      (setq result (concat result line "\n")))

    ;; Separator
    (setq result (concat result "=======\n"))

    ;; Modified content (reversed to preserve order)
    (dolist (line (nreverse added-lines))
      (setq result (concat result line "\n")))

    ;; End conflict marker
    (setq result (concat result ">>>>>>> " marker2 "\n"))

    result))


(defun simple-diff-merge-strings (str1 str2 &optional marker1 marker2 normalize-endings)
  "Merge STR1 and STR2 with conflict markers.
Returns a string containing both contents with conflict markers.
MARKER1 and MARKER2 are used as the conflict markers.
If NORMALIZE-ENDINGS is non-nil, convert all line endings to LF before diffing."
  (let* ((m1 (or marker1 "ORIGINAL"))
         (m2 (or marker2 "MODIFIED"))
         (diff-result (simple-diff-strings str1 str2 normalize-endings)))
    (simple-diff-conflict-format diff-result m1 m2)))


(provide 'simple-diff)
;;; simple-diff.el ends here
