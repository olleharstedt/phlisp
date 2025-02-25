; Load a bunch of keywords
(load-lib "phpunit")

(test-class 'Foo)
(setq x 20)
(test-method 'getBar x)
(expect 10)
