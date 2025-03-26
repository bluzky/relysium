;;; refine-diff-tests.el --- Tests for elysium.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for elysium.el, focusing on the elysium--apply-refined-change function
;; This test suite uses mocks to avoid dependence on gptel

;;; Code:
(require 'elysium)
(require 'ert)

;; Test helper functions
(defun elysium-test--setup-buffer (content)
  "Set up a temporary buffer with CONTENT for testing."
  (let ((buffer (generate-new-buffer " *elysium-test*")))
    (with-current-buffer buffer
      (insert content))
    buffer))

(defun elysium-test--buffer-content (buffer)
  "Get the content of BUFFER as a string."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun elysium-test--cleanup-buffer (buffer)
  "Clean up the test BUFFER."
  (when (buffer-live-p buffer)
    (kill-buffer buffer)))

(ert-deftest elysium-test--apply-refined-change-simple ()
  "Test applying a simple single-line change."
  (let ((buffer (elysium-test--setup-buffer "function test() {\n  return 42;\n}\n"))
        (orig-code "function test() {\n  return 42;\n}\n")
        (new-code "function test() {\n  return 43;\n}\n"))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (elysium--apply-refined-change (point-min) (point-max) orig-code new-code))
          (let ((result (elysium-test--buffer-content buffer)))
            (should (string-match "<<<<<<< HEAD" result))
            (should (string-match "  return 42;" result))
            (should (string-match "=======" result))
            (should (string-match "  return 43;" result))
            (should (string-match ">>>>>>> MockLLM" result))))
      (elysium-test--cleanup-buffer buffer))))

(ert-deftest elysium-test--apply-refined-change-multiline ()
  "Test applying a multi-line change."
  (let ((buffer (elysium-test--setup-buffer "function sum(a, b) {\n  return a + b;\n}\n\nfunction test() {\n  return sum(1, 2);\n}\n"))
        (orig-code "function sum(a, b) {\n  return a + b;\n}\n\nfunction test() {\n  return sum(1, 2);\n}\n")
        (new-code "function sum(a, b) {\n  return a + b;\n}\n\nfunction test() {\n  // Add logging\n  console.log('Testing');\n  return sum(1, 2);\n}\n"))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (elysium--apply-refined-change (point-min) (point-max) orig-code new-code))
          (let ((result (elysium-test--buffer-content buffer)))
            ;; The function should create a conflict that keeps the common parts
            ;; and only marks the differences
            (should (string-match "function sum(a, b) {" result))
            (should (string-match "<<<<<<< HEAD" result))
            (should (string-match "  return sum(1, 2);" result))
            (should (string-match "=======" result))
            (should (string-match "  // Add logging" result))
            (should (string-match "  console.log('Testing');" result))
            (should (string-match ">>>>>>> MockLLM" result))))
      (elysium-test--cleanup-buffer buffer))))

(ert-deftest elysium-test--apply-refined-change-identical ()
  "Test applying a change that is identical to the original."
  (let ((buffer (elysium-test--setup-buffer "function identity(x) {\n  return x;\n}\n"))
        (orig-code "function identity(x) {\n  return x;\n}\n")
        (new-code "function identity(x) {\n  return x;\n}\n"))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (elysium--apply-refined-change (point-min) (point-max) orig-code new-code))
          (let ((result (elysium-test--buffer-content buffer)))
            ;; Since code is identical, we should not see conflict markers
            (should-not (string-match "<<<<<<< HEAD" result))
            (should-not (string-match "=======" result))
            (should-not (string-match ">>>>>>> MockLLM" result))
            (should (string= orig-code result))))
      (elysium-test--cleanup-buffer buffer))))

(ert-deftest elysium-test--apply-refined-change-empty ()
  "Test applying a change where the original or new code is empty."
  (let ((buffer (elysium-test--setup-buffer ""))
        (orig-code "")
        (new-code "function newFunc() {\n  return 'new';\n}\n"))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (elysium--apply-refined-change (point-min) (point-max) orig-code new-code))
          (let ((result (elysium-test--buffer-content buffer)))
            (should (string-match "<<<<<<< HEAD" result))
            (should (string-match "=======" result))
            (should (string-match "function newFunc" result))
            (should (string-match "  return 'new';" result))
            (should (string-match ">>>>>>> MockLLM" result))))
      (elysium-test--cleanup-buffer buffer))))

(ert-deftest elysium-test--apply-refined-change-partial-match ()
  "Test applying a change with partial line matches."
  (let ((buffer (elysium-test--setup-buffer "const config = {\n  port: 3000,\n  host: 'localhost',\n  debug: false\n};\n"))
        (orig-code "const config = {\n  port: 3000,\n  host: 'localhost',\n  debug: false\n};\n")
        (new-code "const config = {\n  port: 8080,\n  host: 'localhost',\n  debug: true,\n  timeout: 5000\n};\n"))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (elysium--apply-refined-change (point-min) (point-max) orig-code new-code))
          (let ((result (elysium-test--buffer-content buffer)))
            ;; Should create multiple conflict chunks for non-matching sections
            (should (string-match "const config = {" result))

            ;; First conflict section: port change
            (should (string-match "<<<<<<< HEAD\n  port: 3000," result))
            (should (string-match "=======\n  port: 8080," result))

            ;; The host line should be outside conflict markers
            (should (string-match "  host: 'localhost'," result))

            ;; Second conflict section: debug and timeout changes
            (should (string-match "<<<<<<< HEAD\n  debug: false" result))
            (should (string-match "=======\n  debug: true,\n  timeout: 5000" result))))
      (elysium-test--cleanup-buffer buffer))))

;; More complex test cases to verify chunking behavior

(ert-deftest elysium-test--apply-refined-change-large-code ()
  "Test applying changes to a larger code block with multiple isolated changes."
  (let* ((lines-original (cl-loop for i from 1 to 20
                                 collect (format "// Line %d" i)))
         (code-orig (concat (string-join lines-original "\n") "\n"))
         (buffer (elysium-test--setup-buffer code-orig))
         ;; Modified code: changed lines 5, 10, and 15
         (lines-modified (cl-loop for i from 1 to 20
                                 collect (if (member i '(5 10 15))
                                            (format "// MODIFIED Line %d" i)
                                          (format "// Line %d" i))))
         (code-new (concat (string-join lines-modified "\n") "\n")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (elysium--apply-refined-change (point-min) (point-max) code-orig code-new))
          (let ((result (elysium-test--buffer-content buffer)))
            ;; Test for the presence of conflict markers
            (should (string-match "<<<<<<< HEAD" result))
            (should (string-match "=======" result))
            (should (string-match ">>>>>>> MockLLM" result))

            ;; Test for the presence of modified content
            (should (string-match "// MODIFIED Line 5" result))
            (should (string-match "// MODIFIED Line 10" result))
            (should (string-match "// MODIFIED Line 15" result))

            ;; Test for retention of original content
            (should (string-match "// Line 5" result))
            (should (string-match "// Line 10" result))
            (should (string-match "// Line 15" result))))
      (elysium-test--cleanup-buffer buffer))))

(ert-deftest elysium-test--apply-refined-change-adjacent-changes ()
  "Test applying adjacent line changes."
  (let ((buffer (elysium-test--setup-buffer "line1\nline2\nline3\nline4\nline5\n"))
        (orig-code "line1\nline2\nline3\nline4\nline5\n")
        (new-code "line1\nmodified2\nmodified3\nline4\nline5\n"))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (elysium--apply-refined-change (point-min) (point-max) orig-code new-code))
          (let ((result (elysium-test--buffer-content buffer)))
            ;; Test for basic markers and modified content
            (should (string-match "<<<<<<< HEAD" result))
            (should (string-match "line2" result))
            (should (string-match "line3" result))
            (should (string-match "=======" result))
            (should (string-match "modified2" result))
            (should (string-match "modified3" result))
            (should (string-match ">>>>>>> MockLLM" result))))
      (elysium-test--cleanup-buffer buffer))))

(ert-deftest elysium-test--apply-refined-change-insertions ()
  "Test applying changes that insert new lines."
  (let ((buffer (elysium-test--setup-buffer "function test() {\n  return 42;\n}\n"))
        (orig-code "function test() {\n  return 42;\n}\n")
        (new-code "function test() {\n  // New comment\n  console.log('test');\n  return 42;\n}\n"))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (elysium--apply-refined-change (point-min) (point-max) orig-code new-code))
          (let ((result (elysium-test--buffer-content buffer)))
            ;; Check for the presence of conflict markers and new inserted code
            (should (string-match "<<<<<<< HEAD" result))
            (should (string-match "=======" result))
            (should (string-match "// New comment" result))
            (should (string-match "console.log('test');" result))
            (should (string-match ">>>>>>> MockLLM" result))))
      (elysium-test--cleanup-buffer buffer))))

(ert-deftest elysium-test--apply-refined-change-deletions ()
  "Test applying changes that delete lines."
  (let ((buffer (elysium-test--setup-buffer "function test() {\n  // Comment line\n  let x = 1;\n  return 42;\n}\n"))
        (orig-code "function test() {\n  // Comment line\n  let x = 1;\n  return 42;\n}\n")
        (new-code "function test() {\n  return 42;\n}\n"))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (elysium--apply-refined-change (point-min) (point-max) orig-code new-code))
          (let ((result (elysium-test--buffer-content buffer)))
            ;; Check for basic conflict markers and content
            (should (string-match "<<<<<<< HEAD" result))
            (should (string-match "// Comment line" result))
            (should (string-match "let x = 1;" result))
            (should (string-match "=======" result))
            (should (string-match ">>>>>>> MockLLM" result))))
      (elysium-test--cleanup-buffer buffer))))

(ert-deftest elysium-test--apply-refined-change-newline-differences ()
  "Test applying changes when original code has trailing newlines but new code doesn't."
  (let ((buffer (elysium-test--setup-buffer "function test() {\n  return 42;\n}\n"))
        (orig-code "function test() {\n  return 42;\n}\n")
        (new-code "function test() {\n  return 43;\n}"))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (elysium--apply-refined-change (point-min) (point-max) orig-code new-code))
          (let ((result (elysium-test--buffer-content buffer)))
            ;; Check for basic conflict markers
            (should (string-match "<<<<<<< HEAD" result))
            (should (string-match "=======" result))
            (should (string-match ">>>>>>> MockLLM" result))

            ;; Check for content with and without trailing newlines
            (should (string-match "return 42;" result))
            (should (string-match "return 43;" result))

            ;; The result should include both versions - one with newline and one without
            (should (string-match "}\n" result))
            (should (string-match "^\n" result))
            (should (string-match "}" result))))
      (elysium-test--cleanup-buffer buffer))))

(ert-deftest elysium-test--apply-refined-change-no-newlines-to-newlines ()
  "Test applying changes when original code lacks trailing newlines but new code has them."
  (let ((buffer (elysium-test--setup-buffer "function test() {\n  return 42;\n}"))
        (orig-code "function test() {\n  return 42;\n}")
        (new-code "function test() {\n  return 43;\n}\n"))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (elysium--apply-refined-change (point-min) (point-max) orig-code new-code))
          (let ((result (elysium-test--buffer-content buffer)))
            ;; Check for basic conflict markers
            (should (string-match "<<<<<<< HEAD" result))
            (should (string-match "=======" result))
            (should (string-match ">>>>>>> MockLLM" result))

            ;; Check for content
            (should (string-match "return 42;" result))
            (should (string-match "return 43;" result))))
      (elysium-test--cleanup-buffer buffer))));;; elysium-tests.el --- Tests for elysium.el -*- lexical-binding: t; -*-


(provide 'elysium-tests)
;;; elysium-tests.el ends here
