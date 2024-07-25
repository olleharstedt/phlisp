<?php

$stack = new SplStack();
$stack->push('a');
$stack->push('b');
$stack->push('c');
var_dump($stack);
$stack->offsetSet(count($stack) - 2 - 1, 'd');
var_dump($stack);
