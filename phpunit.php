<?php

return [
    new CustomOp(
        'test-class',
        function($that, $sexpr) {
            $sym = $sexpr->shift();
            $s = $that->eval($sym);
            if (class_exists($s)) {
                $that->env['__class_under_test'] = new $s;
            } else {
                throw new RuntimeException('Found no class ' . $s);
            }
        }
    ),
    new CustomOp(
        'test-method',
        function($that, $sexpr) {
            if (!isset($that->env['__class_under_test'])) {
                throw new RuntimeException('No class under test, please run test-class first');
            }
            $class = $that->env['__class_under_test'];
        }
    ),
    new CustomOp(
        'expect',
        function($that, $sexpr) {
        }
    )
];
