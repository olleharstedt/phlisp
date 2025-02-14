(load "echo")
(defun say-hello (a b) (echo (concat a b)))
(say-hello "hello" "world!")
