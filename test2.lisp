; CLISP test
(defun select-helper (ast)
  (if (null ast)
    nil
    (cons
      (cond
          (format nil "~A" (first ast))
          (select-helper (rest ast))))))

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
