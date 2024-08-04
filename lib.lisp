(do
  (def _report (hash-map))
  (set _report "columns" nil)
  (def title (fn* (x) (setq '_title x)))
  (def table (fn* (x) (set _report "table" x)))
  (def column
       (fn* (x)
            (set _report "columns" (list x (get _report "columns")))))
  (defmacro select
    (fn* (ast) (loop-aux ast)))
  )
