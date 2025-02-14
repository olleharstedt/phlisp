<?php

return new CustomOp(
    'tostring',
    function($that, $sexpr) {
        $next = $sexpr->shift();
        return new Str($this->eval($next));
    }
);
