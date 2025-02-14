<?php

return new CustomOp(
    'echo',
    function($that, $sexpr) {
        $next = $sexpr->shift();
        if ($next instanceof Str) {
            echo $next->s;
        } else {
            $s = $this->eval($next);
            echo $s->s;
        }
    }
);
