; CLISP test
(defmacro test (ast)
  (cons
      (princ-to-string (first ast))
      (test (rest ast))))
(print (test (select (round (* 100 (- 1 (/ purchase_price selling_price))) 2))))
