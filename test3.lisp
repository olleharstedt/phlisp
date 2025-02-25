(load "echo")
(defun say-hello (a b) (echo (concat a b)))
(setq h "Hello")
(say-hello h "world!")
