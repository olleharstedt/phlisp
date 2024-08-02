(do
  (defmacro quote2 (fn* (ast)
     (list (fn* () ast))))

  (defmacro infix (fn* (ast)
    (list (second ast) (first ast) (last ast))))

  (println (infix (2 * 3)))

  (def z "Hello\n")
  (println z)

  (def nil? (fn* (x) (= x nil)))
  (def x (hash-map))
  (set x "a" 12)
  (println (nil? x))
  (println (get x "a"))

  (defmacro select (fn* (ast)
    (list ast)))

  (def report
       `(columns
          (column
            (title "Article")
            ,(select hello)
            ; (select (round (* 100 (- 1 (/ purchase_price selling_price))) 2))
            ,(+ 1 2)
            ,(infix (2 + 3))
            )
          )
       )
)
