<?php

return new CustomOp(
    'include',
    function($that, $sexpr) {
        $f = $this->eval($sexpr->shift());
        include($f);
    }
);
