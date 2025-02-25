(load-lib "phpunit")       ; Load phpunit keywords
(load "include")           ; Load 'include' keyword
(include "Foo.php")        ; Include Foo PHP class

(test-class 'Foo)          ; Set class under test
(setq x 10)                ; Set variable x to 20
(test-method 'getBar x)    ; Run test
(expect 20 "Should be 20") ; Check result
