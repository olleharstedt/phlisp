(do
  (defmacro cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons 'cond (rest (rest xs)))))))

  (defmacro infix (fn* (ast)
    (list (second ast) (first ast) (last ast))))

  (let*
    (x 11)
    (cond
      (> x 10) (println "hello!")
      t (println "moo")))

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

  (println (string-concat '(1 2 "qwe")))

  (def select-helper
       (fn* (ast)
            (cond
              (nil? ast) ""
              (string? ast) (list (string-concat (list "`" ast "`")))
              (number? ast) (list (string ast))
              (symbol? ast) (list (symbol-name ast))
              ; ast is a cons
              (list? ast)
               ;; match on first node
               (let* (node (first ast))
                 (cond
                   ; Continue as normal if node is number
                   ((number? node)
                    (cons (car ast)
                          (mapcar #'select-helper (cdr ast))))
                   ; Not a number
                   ((symbol? node)
                    (cond 
                      ; ROUND function
                      (= (symbol-name node) "ROUND")
                       (pre-operator node ast)
                      (= (symbol-name node) "-")
                       (in-operator node ast)
                      (= (symbol-name node) "+")
                       (in-operator node ast)
                      (= (symbol-name node) "/")
                       (in-operator node ast)
                      (= (symbol-name node) "*")
                       (in-operator node ast)
                      t (error (string-concat "ERROR: Unsupported operator: " (symbol-name node)))))
                   (= ast "+") (list "ASD")
                   (nil?
                      (cons (car ast)
                            (mapcar #'select-helper (cdr ast))))
                   t (error "Unknown node type: ~A" ast))))))

  (defmacro select (fn* (ast)
    `(select-helper ,ast)))

   (println (select 1))
)
