;;; simple-template.el --- Simple template engine for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file provides a simplified template engine for Relysium with support for:
;; - Variable substitution: ${var-name}
;; - Conditionals: {{if condition}}...{{else}}...{{endif}}
;; - For loops: {{for item in collection}}...{{endfor}}
;; - Comments: {{! comment }}
;; - Expression evaluation: ${=expression}
;;
;; Features:
;; - Handles nested conditionals and for loops
;; - Supports string and numerical comparison in conditions
;; - Allows expressions in both variable substitutions and conditions
;; - Uses a robust tokenizing and parsing approach for reliable rendering
;;
;; This is a reliable version without complex buffer manipulation.

;;; Code:

(defun simple-template--get-value (var-name context)
  "Get the value of VAR-NAME from CONTEXT plist.
Supports nested properties using dot notation (e.g., user.name)."
  (if (string-match "\\`\\([^.]+\\)\\.\\([^.]+\\)\\'" var-name)
      (let* ((obj-name (match-string 1 var-name))
             (prop-name (match-string 2 var-name))
             (obj-value (plist-get context (intern (concat ":" obj-name)))))
        (when (and obj-value (listp obj-value))
          (plist-get obj-value (intern (concat ":" prop-name)))))
    (plist-get context (intern (concat ":" var-name)))))

;; Condition evaluation function
(defun simple-template--eval-condition (condition context)
  "Evaluate CONDITION in the context of CONTEXT plist.
Supports expression evaluation with =, simple boolean values,
variable checks, equality comparisons, and nested properties."
  (let ((result nil))
    (cond
     ;; Expression evaluation with = prefix
     ((string-match "^=\\(.*\\)$" condition)
      (let ((expression (match-string 1 condition)))
        (condition-case nil
            ;; Create a temporary function with lexical bindings
            (let ((func-body (read expression))
                  (bindings nil))
              ;; Create bindings from context
              (dolist (pair (seq-partition context 2))
                (when (keywordp (car pair))
                  (push (cons (intern (substring (symbol-name (car pair)) 1))
                              (cadr pair))
                        bindings)))
              ;; Evaluate with lexical binding
              (eval `(funcall (lambda ,(mapcar #'car bindings)
                                ,func-body)
                              ,@(mapcar #'cdr bindings))))
          (error nil))))

     ;; Simple boolean values
     ((string= condition "t") t)
     ((string= condition "nil") nil)

     ;; Check for nested property conditions (user.property)
     ((string-match "^\\([a-zA-Z0-9_-]+\\)\\.\\([a-zA-Z0-9_-]+\\)$" condition)
      (let* ((obj-name (match-string 1 condition))
             (prop-name (match-string 2 condition))
             (obj-value (simple-template--get-value obj-name context)))
        (when (and obj-value (listp obj-value))
          (plist-get obj-value (intern (concat ":" prop-name))))))

     ;; Simple variable check
     ((string-match "^\\([a-zA-Z0-9_-]+\\)$" condition)
      (let* ((var-name (match-string 1 condition))
             (var-value (simple-template--get-value var-name context)))
        var-value))

     ;; Comparison: equals with nested property
     ((string-match "^\\([a-zA-Z0-9_.-]+\\)\\s-*=\\s-*\\(.+\\)$" condition)
      (let* ((var-path (match-string 1 condition))
             (var-value (if (string-match "\\." var-path)
                            (let* ((parts (split-string var-path "\\."))
                                   (obj-name (car parts))
                                   (prop-name (cadr parts))
                                   (obj-value (simple-template--get-value obj-name context)))
                              (when (and obj-value (listp obj-value))
                                (plist-get obj-value (intern (concat ":" prop-name)))))
                          (simple-template--get-value var-path context)))
             (compare-value (match-string 2 condition)))
        ;; Strip quotes if present
        (when (and (stringp compare-value)
                   (string-match "^[\"']\\(.*\\)[\"']$" compare-value))
          (setq compare-value (match-string 1 compare-value)))
        (equal var-value compare-value)))

     ;; Default to false for complex conditions
     (t nil))))

;; New approach: tokenize the template first, then process tokens

(defun simple-template--tokenize (template)
  "Convert TEMPLATE string into a list of tokens.
Each token is a plist with properties :type, :content, and :position."
  (let ((pos 0)
        (tokens '())
        (tag-regex "{{\\(if\\|else\\|endif\\|for\\|endfor\\|!\\)\\([^}]*\\)}}\\|\\${\\([^}]+\\)}")
        (len (length template)))

    (while (< pos len)
      (if (string-match tag-regex template pos)
          (let ((match-start (match-beginning 0))
                (match-end (match-end 0))
                (tag-type (or (match-string 1 template) "var"))
                (tag-content (or (match-string 2 template)
                                 (match-string 3 template))))

            ;; If there's text before the tag, add it as a text token
            (when (< pos match-start)
              (push (list :type 'text
                          :content (substring template pos match-start)
                          :position pos)
                    tokens))

            ;; Add the tag token
            (push (list :type (cond
                               ((string= tag-type "if") 'if-open)
                               ((string= tag-type "else") 'else)
                               ((string= tag-type "endif") 'if-close)
                               ((string= tag-type "for") 'for-open)
                               ((string= tag-type "endfor") 'for-close)
                               ((string= tag-type "!") 'comment)
                               ((string-prefix-p "=" tag-content) 'expression)
                               (t 'variable))
                        :content (string-trim tag-content)
                        :position match-start
                        :length (- match-end match-start))
                  tokens)

            (setq pos match-end))

        ;; No more tags, add remaining text
        (push (list :type 'text
                    :content (substring template pos)
                    :position pos)
              tokens)
        (setq pos len)))

    ;; Return tokens in original order
    (nreverse tokens)))

(defun simple-template--parse-for-loop (content)
  "Parse a for loop tag content in the format 'item in collection'.
Returns a cons cell (ITEM-VAR . COLLECTION-VAR)."
  (if (string-match "^\\([a-zA-Z0-9_]+\\)\\s-+in\\s-+\\([a-zA-Z0-9_]+\\)$" content)
      (cons (match-string 1 content) (match-string 2 content))
    (error "Invalid for loop syntax: %s. Expected format: 'item in collection'" content)))

(defun simple-template--build-parse-tree (tokens)
  "Build a parse tree from TOKENS representing the template structure."
  (let ((stack '())        ;; Stack for tracking open blocks
        (result '())       ;; Root level nodes
        (current-level '()) ;; Current level nodes
        (i 0))

    (while (< i (length tokens))
      (let* ((token (nth i tokens))
             (token-type (plist-get token :type)))

        (cond
         ;; Handle opening if tags
         ((eq token-type 'if-open)
          (push (list :node-type 'if-block
                      :condition (plist-get token :content)
                      :position (plist-get token :position)
                      :if-branch '()
                      :else-branch '()
                      :current-branch 'if) ;; Initialize branch tracking
                stack)
          (setq current-level (car stack)))

         ;; Handle opening for tags
         ((eq token-type 'for-open)
          (let* ((for-content (plist-get token :content))
                 (loop-vars (simple-template--parse-for-loop for-content)))
            (push (list :node-type 'for-block
                        :item-var (car loop-vars)
                        :collection-var (cdr loop-vars)
                        :position (plist-get token :position)
                        :body '())
                  stack)
            (setq current-level (car stack))))

         ;; Handle else tags
         ((eq token-type 'else)
          (unless stack
            (error "Unexpected {{else}} without matching {{if}}"))
          (unless (eq (plist-get (car stack) :node-type) 'if-block)
            (error "Unexpected {{else}} inside a for loop"))
          ;; Switch to the else branch
          (let ((current-if (car stack)))
            (plist-put current-if :current-branch 'else)))

         ;; Handle closing if tags
         ((eq token-type 'if-close)
          (unless stack
            (error "Unexpected {{endif}} without matching {{if}}"))
          (unless (eq (plist-get (car stack) :node-type) 'if-block)
            (error "Mismatched {{endif}} - expected {{endfor}}"))
          (let ((completed-if (pop stack)))
            ;; Add the completed if block to its parent
            (if stack
                (let* ((parent (car stack))
                       (parent-type (plist-get parent :node-type)))
                  (cond
                   ((eq parent-type 'if-block)
                    (let* ((parent-branch (plist-get parent :current-branch))
                           (branch-key (if (eq parent-branch 'else)
                                           :else-branch
                                         :if-branch)))
                      (plist-put parent branch-key
                                 (append (plist-get parent branch-key) (list completed-if)))))
                   ((eq parent-type 'for-block)
                    (plist-put parent :body
                               (append (plist-get parent :body) (list completed-if))))))
              ;; Top level
              (push completed-if result)))
          ;; Reset current level
          (setq current-level (if stack (car stack) '())))

         ;; Handle closing for tags
         ((eq token-type 'for-close)
          (unless stack
            (error "Unexpected {{endfor}} without matching {{for}}"))
          (unless (eq (plist-get (car stack) :node-type) 'for-block)
            (error "Mismatched {{endfor}} - expected {{endif}}"))
          (let ((completed-for (pop stack)))
            ;; Add the completed for block to its parent
            (if stack
                (let* ((parent (car stack))
                       (parent-type (plist-get parent :node-type)))
                  (cond
                   ((eq parent-type 'if-block)
                    (let* ((parent-branch (plist-get parent :current-branch))
                           (branch-key (if (eq parent-branch 'else)
                                           :else-branch
                                         :if-branch)))
                      (plist-put parent branch-key
                                 (append (plist-get parent branch-key) (list completed-for)))))
                   ((eq parent-type 'for-block)
                    (plist-put parent :body
                               (append (plist-get parent :body) (list completed-for))))))
              ;; Top level
              (push completed-for result)))
          ;; Reset current level
          (setq current-level (if stack (car stack) '())))

         ;; Skip comments
         ((eq token-type 'comment)
          nil)

         ;; Regular content (text, variables, expressions)
         (t
          (if stack
              (let* ((parent (car stack))
                     (parent-type (plist-get parent :node-type)))
                (cond
                 ((eq parent-type 'if-block)
                  (let* ((parent-branch (plist-get parent :current-branch))
                         (branch-key (if (eq parent-branch 'else)
                                         :else-branch
                                       :if-branch)))
                    (plist-put parent branch-key
                               (append (plist-get parent branch-key) (list token)))))
                 ((eq parent-type 'for-block)
                  (plist-put parent :body
                             (append (plist-get parent :body) (list token))))))
            ;; Top level
            (push token result))))

        (setq i (1+ i))))

    ;; Check for unmatched opening tags
    (when stack
      (let* ((top (car stack))
             (node-type (plist-get top :node-type)))
        (error "Unmatched %s tag at position %d"
               (if (eq node-type 'if-block) "{{if}}" "{{for}}")
               (plist-get top :position))))

    ;; Return the parse tree in original order
    (nreverse result)))

(defun simple-template--render-parse-tree (tree context)
  "Render the parse TREE using the given CONTEXT."
  (let ((result ""))
    ;; Changed to use a safer iteration pattern
    (let ((tail tree))
      (while tail
        (let* ((node (car tail))
               (node-type (if (plist-member node :node-type)
                              (plist-get node :node-type)
                            (plist-get node :type))))

          (cond
           ;; If block
           ((eq node-type 'if-block)
            (let ((condition (plist-get node :condition))
                  (if-branch (plist-get node :if-branch))
                  (else-branch (plist-get node :else-branch)))

              (if (simple-template--eval-condition condition context)
                  (setq result (concat result (simple-template--render-parse-tree if-branch context)))
                (setq result (concat result (simple-template--render-parse-tree else-branch context))))))

           ;; For block
           ((eq node-type 'for-block)
            (let* ((item-var (plist-get node :item-var))
                   (collection-var (plist-get node :collection-var))
                   (collection (simple-template--get-value collection-var context))
                   (body (plist-get node :body))
                   (loop-result ""))

              ;; Only process if collection is a list/vector and not empty
              (when (and collection (or (listp collection) (vectorp collection)))
                (let ((collection-list (if (vectorp collection)
                                           (append collection nil)
                                         collection)))
                  (dolist (item collection-list)
                    ;; Create a new context with the loop variable for each iteration
                    (let ((loop-context (append (list (intern (concat ":" item-var)) item) context)))
                      (setq loop-result (concat loop-result
                                                (simple-template--render-parse-tree body loop-context)))))))
              (setq result (concat result loop-result))))

           ;; Text node
           ((eq node-type 'text)
            (setq result (concat result (plist-get node :content))))

           ;; Variable node
           ((eq node-type 'variable)
            (let* ((var-name (plist-get node :content))
                   (var-value (simple-template--get-value var-name context))
                   (replacement (cond
                                 ((null var-value) "")
                                 ((functionp var-value) (funcall var-value))
                                 (t (if (stringp var-value)
                                        var-value
                                      (format "%s" var-value))))))
              (setq result (concat result replacement))))

           ;; Expression node
           ((eq node-type 'expression)
            (let* ((expression (substring (plist-get node :content) 1)) ; Remove the '=' prefix
                   (replacement ""))
              (condition-case err
                  (let ((expr-value
                         ;; Create a temporary function with lexical environment from context
                         (let ((func-body (read expression))
                               (bindings nil))
                           ;; Create bindings from context
                           (dolist (pair (seq-partition context 2))
                             (when (keywordp (car pair))
                               (push (cons (intern (substring (symbol-name (car pair)) 1))
                                           (cadr pair))
                                     bindings)))
                           ;; Evaluate with lexical binding
                           (eval `(funcall (lambda ,(mapcar #'car bindings)
                                             ,func-body)
                                           ,@(mapcar #'cdr bindings))))))
                    (setq replacement (if (stringp expr-value)
                                          expr-value
                                        (format "%s" expr-value))))
                (error (setq replacement (format "[Error evaluating %s: %s]"
                                                 expression (error-message-string err)))))
              (setq result (concat result replacement))))))

        (setq tail (cdr tail))))

    result))

(defun simple-template-render-template (template context)
  "Render TEMPLATE with CONTEXT plist using the improved parsing approach."
  (if (or (null template) (string-empty-p template))
      "" ; Return empty string for empty templates
    (let* ((tokens (simple-template--tokenize template))
           (parse-tree (simple-template--build-parse-tree tokens))
           (result (simple-template--render-parse-tree parse-tree context)))
      result)))


(provide 'simple-template)
;;; simple-template.el ends here
