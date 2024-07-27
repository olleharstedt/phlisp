($begin
  (display "hello world")

  ($define! $lambda
            ($vau (ptree . body) static-env
                  (wrap (eval (list* $vau ptree #ignore body)
                              static-env))))

  ($define! $cond
            ($vau clauses env
                  ($define! aux
                            ($lambda ((test . body) . clauses)
                                     ($if (eval test env)
                                          (apply (wrap $sequence) body env)
                                          (apply (wrap $cond) clauses env))))
                  ($if (null? clauses)
                       #inert
                       (apply aux clauses))))

  ($define! $if
            ($vau (x y z) env
                  ($cond ((eval x env) (eval y env))
                         (#t (eval z env)
                          )
                         )
                  )
            )

  ($if 0
       (display "1")
       (display "2")
       )
  (display "end")
  )
