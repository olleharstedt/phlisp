<?php

return new CustomOp(
    'echo',
    function($that, $sexpr) {
        $s = $sexpr->shift();
        if ($s instanceof Str) {
            echo $s->s;
            return;
        }
        throw new Exception('$s is not a string');
    }
);
