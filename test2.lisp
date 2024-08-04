; CLISP test
(defun select-helper (ast)
  (cond
    ((null ast) nil)
    ((numberp ast) (list (format nil "~A" ast)))
    ((symbolp ast) (list (symbol-name ast)))
    ; ast is a cons
    ((consp ast)
     ;; match on first node
     (let ((node (first ast)))
       (cond
         ((symbolp node) (list "QWE"))
         ((= ast "+") (list "ASD"))
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
; (print (select (1 2 3)))
