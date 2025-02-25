<?php

return [
    new CustomOp(
        'test-class',
        function($that, $sexpr) {
            $sym = $sexpr->shift();
            $s = $that->eval($sym);
            if (class_exists($s)) {
                $that->env['__class_under_test'] = new $s();
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
            $method = $that->eval($sexpr->shift());
            $arg = $that->eval($sexpr->shift());
            $class = $that->env['__class_under_test'];
            $that->env['__class_under_test_result'] = $class->$method($arg);
        }
    ),
    new CustomOp(
        'expect',
        function($that, $sexpr) {
            $expected = $that->eval($sexpr->shift());
            if (count($sexpr) > 0) {
                $message = $that->eval($sexpr->shift());
            } else {
                $message = '';
            }
            $result = $that->env['__class_under_test_result'];
            if ($expected !== $result) {
                echo "\033[31m•\033[0m";
            } else {
                echo "•";
            }
        }
    )
];
