;;; simple-diff-tests.el --- Tests for simple-diff.el

;;; Commentary:
;; Unit tests for the simple-diff.el package
;; Run with M-x ert or M-x ert-run-tests-interactively

;;; Code:

(require 'ert)
(require 'simple-diff)

(ert-deftest simple-diff-test-merge-strings-identical ()
  "Test that identical strings produce no conflicts."
  (let ((str "Line 1\nLine 2\nLine 3"))
    (should (string= str (simple-diff-merge-strings str str)))))

(ert-deftest simple-diff-test-merge-strings-different ()
  "Test conflict markers for completely different strings."
  (let ((str1 "This is string one")
        (str2 "This is string two")
        (expected "<<<<<<< ORIGINAL
This is string one
=======
This is string two
>>>>>>> MODIFIED
"))
    (should (string= expected (simple-diff-merge-strings str1 str2)))))

(ert-deftest simple-diff-test-merge-strings-partial-change ()
  "Test conflict markers for strings with partial differences."
  (let ((str1 "Line 1\nLine 2\nLine 3\nLine 4")
        (str2 "Line 1\nChanged line\nLine 3\nLine 4")
        (expected "Line 1
<<<<<<< ORIGINAL
Line 2
=======
Changed line
>>>>>>> MODIFIED
Line 3
Line 4
"))
    (should (string= expected (simple-diff-merge-strings str1 str2)))))

(ert-deftest simple-diff-test-merge-strings-addition ()
  "Test conflict markers when lines are added."
  (let ((str1 "Line 1\nLine 2\nLine 4")
        (str2 "Line 1\nLine 2\nLine 3\nLine 4")
        (expected "Line 1
Line 2
<<<<<<< ORIGINAL
=======
Line 3
>>>>>>> MODIFIED
Line 4
"))
    (should (string= expected (simple-diff-merge-strings str1 str2)))))

(ert-deftest simple-diff-test-merge-strings-deletion ()
  "Test conflict markers when lines are deleted."
  (let ((str1 "Line 1\nLine 2\nLine 3\nLine 4")
        (str2 "Line 1\nLine 4")
        (expected "Line 1
<<<<<<< ORIGINAL
Line 2
Line 3
=======
>>>>>>> MODIFIED
Line 4
"))
    (should (string= expected (simple-diff-merge-strings str1 str2)))))

(ert-deftest simple-diff-test-merge-strings-custom-markers ()
  "Test custom conflict markers."
  (let ((str1 "Line A\nLine B")
        (str2 "Line A\nLine C")
        (expected "Line A
<<<<<<< MASTER
Line B
=======
Line C
>>>>>>> FEATURE
"))
    (should (string= expected (simple-diff-merge-strings str1 str2 "MASTER" "FEATURE")))))

(ert-deftest simple-diff-test-merge-strings-multiple-conflicts ()
  "Test multiple conflicts in the same string."
  (let ((str1 "Line 1\nLine 2\nLine 3\nLine 4\nLine 5")
        (str2 "Line 1\nModified 2\nLine 3\nModified 4\nLine 5")
        (expected "Line 1
<<<<<<< ORIGINAL
Line 2
=======
Modified 2
>>>>>>> MODIFIED
Line 3
<<<<<<< ORIGINAL
Line 4
=======
Modified 4
>>>>>>> MODIFIED
Line 5
"))
    (should (string= expected (simple-diff-merge-strings str1 str2)))))

(ert-deftest simple-diff-test-merge-strings-adjacent-conflicts ()
  "Test adjacent conflicts are properly separated."
  (let ((str1 "A\nB\nC\nD\nE")
        (str2 "A\nX\nY\nD\nE")
        (expected "A
<<<<<<< ORIGINAL
B
C
=======
X
Y
>>>>>>> MODIFIED
D
E
"))
    (should (string= expected (simple-diff-merge-strings str1 str2)))))

(ert-deftest simple-diff-test-merge-strings-empty-string ()
  "Test with one empty string."
  (let ((str1 "")
        (str2 "Some content")
        (expected "<<<<<<< ORIGINAL
=======
Some content
>>>>>>> MODIFIED
"))
    (should (string= expected (simple-diff-merge-strings str1 str2)))))

(ert-deftest simple-diff-test-merge-strings-line-endings ()
  "Test handling of different line ending styles."
  (let ((str1 "Line 1\nLine 2\nLine 3")
        ;; Use literal CR+LF to avoid interpretation issues
        (str2 (concat "Line 1" (string ?\r ?\n)
                     "Line 2" (string ?\r ?\n)
                     "Line 3"))
        ;; With normalization, strings should be considered identical
        (expected "Line 1\nLine 2\nLine 3"))
    (should (string= expected (simple-diff-merge-strings str1 str2 "ORIGINAL" "MODIFIED" t)))))

(ert-deftest simple-diff-test-merge-strings-line-endings-without-normalization ()
  "Test handling of different line ending styles without normalization."
  (let* ((str1 "Line 1\nLine 2\nLine 3")
         ;; Use literal CR+LF to avoid interpretation issues
         (str2 (concat "Line 1" (string ?\r ?\n)
                      "Line 2" (string ?\r ?\n)
                      "Line 3"))
         ;; Get the actual result
         (result (simple-diff-merge-strings str1 str2 "ORIGINAL" "MODIFIED" nil)))

    ;; Verify it contains conflict markers
    (should (string-match-p "<<<<<<< ORIGINAL" result))
    (should (string-match-p "=======" result))
    (should (string-match-p ">>>>>>> MODIFIED" result))

    ;; Verify content is there
    (should (string-match-p "Line 1" result))
    (should (string-match-p "Line 2" result))
    (should (string-match-p "Line 3" result))

    ;; Verify CR characters are retained in the version with CRLF
    (should (string-match-p (string ?\r) result))))

(ert-deftest simple-diff-test-merge-strings-whitespace ()
  "Test merging strings with only whitespace differences."
  (let ((str1 "Line 1\nLine 2\nLine 3")
        (str2 "Line 1\nLine  2\nLine 3")  ; extra space in "Line  2"
        (expected "Line 1
<<<<<<< ORIGINAL
Line 2
=======
Line  2
>>>>>>> MODIFIED
Line 3
"))
    (should (string= expected (simple-diff-merge-strings str1 str2)))))

(ert-deftest simple-diff-test-merge-strings-both-empty ()
  "Test with both strings empty."
  (let ((str1 "")
        (str2 ""))
    (should (string= "" (simple-diff-merge-strings str1 str2)))))

(ert-deftest simple-diff-test-merge-strings-escape-sequences ()
  "Test strings with escape sequences."
  (let ((str1 "Normal\nWith\\ttab\nWith\\nnewline")
        (str2 "Normal\nNo tabs\nWith\\nnewline")
        (expected "Normal
<<<<<<< ORIGINAL
With\\ttab
=======
No tabs
>>>>>>> MODIFIED
With\\nnewline
"))
    (should (string= expected (simple-diff-merge-strings str1 str2)))))

(ert-deftest simple-diff-test-merge-strings-very-long-line ()
  "Test with a very long line that changes."
  (let* ((long-str (make-string 1000 ?x))
         (long-str2 (concat (make-string 990 ?x) (make-string 10 ?y)))
         (str1 (concat "Short line\n" long-str "\nAnother line"))
         (str2 (concat "Short line\n" long-str2 "\nAnother line"))
         (expected (concat "Short line\n"
                          "<<<<<<< ORIGINAL\n"
                          long-str
                          "\n=======\n"
                          long-str2
                          "\n>>>>>>> MODIFIED\n"
                          "Another line\n")))
    (should (string= expected (simple-diff-merge-strings str1 str2)))))

(ert-deftest simple-diff-test-merge-strings-multiline-conflicts ()
  "Test handling of multiline conflict regions."
  (let ((str1 "Header\n\nParagraph one.\nStill paragraph one.\n\nParagraph two.\n\nFooter")
        (str2 "Header\n\nParagraph 1 modified.\nStill paragraph 1 but changed.\n\nParagraph 2 is unchanged.\n\nFooter")
        (expected "Header

<<<<<<< ORIGINAL
Paragraph one.
Still paragraph one.
=======
Paragraph 1 modified.
Still paragraph 1 but changed.
>>>>>>> MODIFIED

<<<<<<< ORIGINAL
Paragraph two.
=======
Paragraph 2 is unchanged.
>>>>>>> MODIFIED

Footer
"))
    (should (string= expected (simple-diff-merge-strings str1 str2)))))

(ert-deftest simple-diff-test-merge-strings-only-trailing-differences ()
  "Test conflict when only the end of the file differs."
  (let ((str1 "Same content\nSame content\nOriginal ending")
        (str2 "Same content\nSame content\nModified ending")
        (expected "Same content
Same content
<<<<<<< ORIGINAL
Original ending
=======
Modified ending
>>>>>>> MODIFIED
"))
    (should (string= expected (simple-diff-merge-strings str1 str2)))))

(ert-deftest simple-diff-test-merge-strings-only-leading-differences ()
  "Test conflict when only the beginning of the file differs."
  (let ((str1 "Original start\nSame content\nSame content")
        (str2 "Modified start\nSame content\nSame content")
        (expected "<<<<<<< ORIGINAL
Original start
=======
Modified start
>>>>>>> MODIFIED
Same content
Same content
"))
    (should (string= expected (simple-diff-merge-strings str1 str2)))))

(defun simple-diff-run-tests ()
  "Run all tests for simple-diff."
  (interactive)
  (ert-run-tests-interactively "^simple-diff-test-"))

(provide 'simple-diff-tests)
;;; simple-diff-tests.el ends here
