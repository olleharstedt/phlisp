; Concat both numbers and strings
(defun concat-all-formatted (items)
    (format nil "~{~a~}" items))

; CLISP test
(defun select-helper (ast)
  (cond
    ((null ast) "")
    ((stringp ast) (list (concat-all-formatted (list "`" ast "`"))))
    ((numberp ast) (list (format nil "~A" ast)))
    ((symbolp ast) (list (symbol-name ast)))
    ; ast is a cons
    ((consp ast)
     ;; match on first node
     (let* ((node (first ast)))
       (cond
         ; Continue as normal if node is number
         ((numberp node)
            (cons (car ast)
                  (mapcar #'select-helper (cdr ast))))
         ; Not a number
         ((symbolp node)
          (cond 
            ; ROUND function
            ((equal (symbol-name node) "ROUND")
             (list (concat-all-formatted (list "ROUND(" (select-helper (second ast)) "," (select-helper (third ast)) ")"))))
            ((equal (symbol-name node) "-")
             (list (concat-all-formatted (list (select-helper (second ast)) " - " (select-helper (third ast))))))
            (t (error (string-concat "ERROR: Unsupported operator: " (symbol-name node))))))
         ((equal ast "+") (list "ASD"))
         ((null 
            (cons (car ast)
                  (mapcar #'select-helper (cdr ast))))
          )
    (t (error "Unknown node type: ~A" ast)))))))

(defmacro select (ast)
  `(select-helper ',ast))

(defun test (l)
  (if (null l) NIL
    (progn
      (print (first l))
      (test (rest l)))))

(defun double (x)
  (if (null x) NIL
      (cons (* (first x) 2)
          (double (rest x)))))

; (test '(1 2 3))

(print (select (round (* 100 (- 1 (/ purchase_price selling_price))) 2)))
; (print (select (round (- "article_selling_price" 1) 2)))

; https://lispcookbook.github.io/cl-cookbook/macros.html for x in list macro
