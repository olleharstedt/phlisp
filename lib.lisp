(do
  (def _report (hash-map))
  (set _report "columns" nil)
  (def title (fn* (x) (set _report "title" x)))
  (def table (fn* (x) (set _report "table" x)))
  (def column
       (fn* (x y)
            (set _report "columns" (list x y (get _report "columns")))))
  )
