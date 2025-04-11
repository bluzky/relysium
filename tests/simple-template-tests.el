;;; simple-template-tests.el --- Tests for simple-template -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file provides automated tests for the simple-template.el module.
;; Tests cover all key functionalities including:
;; - Variable substitution
;; - Expression evaluation
;; - Conditional rendering
;; - Loop rendering
;; - Nested structures
;; - Error handling
;;
;; Run tests with: M-x ert-run-tests-interactively RET t RET

;;; Code:

(require 'ert)
(require 'simple-template)

;; Helper function to simplify test cases
(defun simple-template-test-render (template context expected &optional test-name)
  "Test rendering TEMPLATE with CONTEXT and check against EXPECTED result.
Optional TEST-NAME provides more context for failures."
  (should (equal (simple-template-render-template template context) expected)))

;; Basic Variable Substitution Tests
(ert-deftest simple-template-test-basic-variable-substitution ()
  "Test basic variable substitution functionality."
  (simple-template-test-render
   "Hello, ${name}!"
   '(:name "World")
   "Hello, World!")

  (simple-template-test-render
   "The ${animal} jumped over the ${obstacle}."
   '(:animal "fox" :obstacle "fence")
   "The fox jumped over the fence.")

  (simple-template-test-render
   "Value: ${missing_var}"
   '(:something_else "value")
   "Value: ")

  (simple-template-test-render
   "${first} ${middle} ${last}"
   '(:first "John" :last "Doe")
   "John  Doe"))

;; Expression Evaluation Tests
(ert-deftest simple-template-test-expression-evaluation ()
  "Test expression evaluation in templates."
  (simple-template-test-render
   "The sum of 2 and 3 is ${=(+ 2 3)}."
   '()
   "The sum of 2 and 3 is 5.")

  (simple-template-test-render
   "Hello, ${=(upcase name)}!"
   '(:name "world")
   "Hello, WORLD!")

  (simple-template-test-render
   "Length: ${=(length text)} characters"
   '(:text "Hello World")
   "Length: 11 characters")

  (simple-template-test-render
   "Status: ${=(if active \"Active\" \"Inactive\")}"
   '(:active t)
   "Status: Active")

  (simple-template-test-render
   "Status: ${=(if active \"Active\" \"Inactive\")}"
   '(:active nil)
   "Status: Inactive"))

;; Conditional Tests
(ert-deftest simple-template-test-conditionals ()
  "Test conditional rendering in templates."
  (simple-template-test-render
   "{{if is_admin}}Admin{{else}}User{{endif}}"
   '(:is_admin t)
   "Admin")

  (simple-template-test-render
   "{{if is_admin}}Admin{{else}}User{{endif}}"
   '(:is_admin nil)
   "User")

  (simple-template-test-render
   "Role: {{if role=\"admin\"}}Administrator{{else}}Regular User{{endif}}"
   '(:role "admin")
   "Role: Administrator")

  (simple-template-test-render
   "Role: {{if role=\"admin\"}}Administrator{{else}}Regular User{{endif}}"
   '(:role "user")
   "Role: Regular User")

  (simple-template-test-render
   "{{if active}}Active{{endif}}"
   '(:active t)
   "Active")

  (simple-template-test-render
   "{{if active}}Active{{endif}}"
   '(:active nil)
   ""))

;; For Loop Tests
(ert-deftest simple-template-test-for-loops ()
  "Test for loop rendering in templates."
  (simple-template-test-render
   "Items:{{for item in items}} ${item}{{endfor}}"
   '(:items ["Apple" "Banana" "Cherry"])
   "Items: Apple Banana Cherry")

  (simple-template-test-render
   "Numbers:{{for num in numbers}} ${num}=${=(* num num)}{{endfor}}"
   '(:numbers [1 2 3 4 5])
   "Numbers: 1=1 2=4 3=9 4=16 5=25")

  (simple-template-test-render
   "Empty list:{{for item in items}} ${item}{{endfor}}"
   '(:items [])
   "Empty list:")

  (simple-template-test-render
   "No list:{{for item in non_existent}} ${item}{{endfor}}"
   '(:something_else "value")
   "No list:"))

;; Nested Loops Tests
(ert-deftest simple-template-test-nested-loops ()
  "Test nested for loops in templates."
  (simple-template-test-render
   "Matrix:{{for row in matrix}}{{for cell in row}} ${=(substring (symbol-name cell) 1)}{{endfor}}|{{endfor}}"
   '(:matrix [[:a :b :c] [:d :e :f] [:g :h :i]])
   "Matrix: a b c| d e f| g h i|"))

;; Loops with Conditionals Tests
(ert-deftest simple-template-test-loops-with-conditionals ()
  "Test loops with conditionals in templates."
  (simple-template-test-render
   "Users:{{for user in users}} ${user.user}{{if user.user_verified}}✓{{endif}}{{endfor}}"
   '(:users [(:user "Alice" :user_verified t)
             (:user "Bob" :user_verified nil)
             (:user "Charlie" :user_verified t)])
   "Users: Alice✓ Bob Charlie✓"
   "Loops with conditionals test"))

;; Conditionals with Loops Tests
(ert-deftest simple-template-test-conditionals-with-loops ()
  "Test conditionals with loops in templates."
  (simple-template-test-render
   "{{if has_items}}Items:{{for item in items}} ${item}{{endfor}}{{else}}No items{{endif}}"
   '(:has_items t :items ["Apple" "Banana"])
   "Items: Apple Banana")

  (simple-template-test-render
   "{{if has_items}}Items:{{for item in items}} ${item}{{endfor}}{{else}}No items{{endif}}"
   '(:has_items nil :items ["Apple" "Banana"])
   "No items"))

;; Nested Conditionals Tests
(ert-deftest simple-template-test-nested-conditionals ()
  "Test nested conditionals in templates."
  (simple-template-test-render
   "{{if outer}}Outer{{if inner}}Inner{{endif}}{{else}}Not Outer{{endif}}"
   '(:outer t :inner t)
   "OuterInner")

  (simple-template-test-render
   "{{if outer}}Outer{{if inner}}Inner{{endif}}{{else}}Not Outer{{endif}}"
   '(:outer t :inner nil)
   "Outer")

  (simple-template-test-render
   "{{if outer}}Outer{{if inner}}Inner{{endif}}{{else}}Not Outer{{endif}}"
   '(:outer nil :inner t)
   "Not Outer")

  (simple-template-test-render
   "{{if outer}}Outer{{if inner}}Inner{{endif}}{{else}}Not Outer{{endif}}"
   '(:outer nil :inner nil)
   "Not Outer"))

;; Complex Template Tests
(ert-deftest simple-template-test-complex-template ()
  "Test a complex template with multiple features."
  (simple-template-test-render
   "{{! User Profile Template }}
User: ${name}
{{if age}}Age: ${age} (${=(if (>= age 18) \"Adult\" \"Minor\")}){{endif}}
{{if has_roles}}
Roles:{{for role in roles}}
- ${role}{{if role=\"admin\"}} (Administrator){{endif}}{{endfor}}
{{else}}
No roles assigned.
{{endif}}"
   '(:name "John Doe"
           :age 25
           :has_roles t
           :roles ["user" "admin" "editor"])
   "
User: John Doe
Age: 25 (Adult)

Roles:
- user
- admin (Administrator)
- editor
"))

;; Comment Tests
(ert-deftest simple-template-test-comments ()
  "Test that comments are properly removed."
  (simple-template-test-render
   "Hello{{! This is a comment }}World"
   '()
   "HelloWorld")

  (simple-template-test-render
   "{{! Beginning comment }}Template{{! Ending comment }}"
   '()
   "Template"))

;; Expression in Conditionals Tests
(ert-deftest simple-template-test-expression-in-conditionals ()
  "Test expressions used in conditionals."
  (simple-template-test-render
   "{{if =(> count 5)}}Many{{else}}Few{{endif}}"
   '(:count 10)
   "Many")

  (simple-template-test-render
   "{{if =(> count 5)}}Many{{else}}Few{{endif}}"
   '(:count 3)
   "Few")

  (simple-template-test-render
   "{{if =(string= (downcase name) \"admin\")}}Admin Access{{else}}No Access{{endif}}"
   '(:name "ADMIN")
   "Admin Access"))

;; Error Handling Tests
(ert-deftest simple-template-test-malformed-templates ()
  "Test how the template engine handles malformed templates."
  ;; Test unmatched if tag
  (should-error
   (simple-template-render-template "{{if condition}}Content" '(:condition t)))

  ;; Test unmatched for tag
  (should-error
   (simple-template-render-template "{{for item in items}}${item}" '(:items ["a" "b"])))

  ;; Test else without if
  (should-error
   (simple-template-render-template "Content{{else}}Other" '()))

  ;; Test endif without if
  (should-error
   (simple-template-render-template "Content{{endif}}" '()))

  ;; Test endfor without for
  (should-error
   (simple-template-render-template "Content{{endfor}}" '()))

  ;; Test invalid for loop syntax
  (should-error
   (simple-template-render-template "{{for invalid syntax}}Content{{endfor}}" '())))

;; Edge Cases Tests
(ert-deftest simple-template-test-edge-cases ()
  "Test edge cases for the template engine."
  ;; Empty template
  (should (equal (simple-template-render-template "" '(:name "value")) ""))

  ;; Template with only whitespace
  (simple-template-test-render
   "   \n   "
   '(:name "value")
   "   \n   ")

  ;; Empty context
  (simple-template-test-render
   "Hello ${name}"
   '()
   "Hello ")

  ;; Function as variable value
  (simple-template-test-render
   "Current time: ${current_time}"
   (list :current_time (lambda () "12:34:56"))
   "Current time: 12:34:56"))

;; Performance Tests (optional)
(ert-deftest simple-template-test-performance ()
  "Test template rendering performance with large templates."
  (let* ((item-template "- Item ${index}: ${=(* index index)}\n")
         (items 100)
         (template (concat "Items:\n"
                           (mapconcat (lambda (i)
                                        (replace-regexp-in-string
                                         "${index}"
                                         (number-to-string i)
                                         item-template))
                                      (number-sequence 1 items)
                                      "")))
         (context '())
         (start-time (current-time))
         (result (simple-template-render-template template context))
         (elapsed-time (float-time (time-subtract (current-time) start-time))))
    ;; Simply ensure it completes without error
    (should (stringp result))
    ;; Optional timing check - adjust threshold based on your environment
    (should (< elapsed-time 1.0))))

(defun simple-template-run-tests ()
  "Run all tests for simple-diff."
  (interactive)
  (ert-run-tests-interactively "^simple-template-test-"))


(provide 'simple-template-tests)
;;; simple-template-tests.el ends here
