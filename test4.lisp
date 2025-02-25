; Load a bunch of keywords
(load "include")
(load-lib "phpunit")

(include "Foo.php")

(test-class 'Foo)
(setq x 20)
(test-method 'getBar x)
(expect 10)
