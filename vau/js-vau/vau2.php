<?php

define("UNDEFINED", -99999);

class Symbol
{
    public $name;
    function __construct($name)
    {
        $this->name = $name;
    }
}

class Keyword extends Symbol {}

class Pair {
    public $car;
    public $cdr;
    function __construct($a, $b)
    {
        $this->car = $a;
        $this->cdr = $b;
    }
}

function reverse($xs, $rightmost)
{
    $result = $rightmost ?? null;
    while ($xs) {
        $result = new Pair($xs->car, $result);
        $xs = $xs->cdr;
    }
    return $result;
}

function listToArray($xs)
{
    $result = new ArrayObject();
    while ($xs) {
        $result->push($xs->car);
        $xs = $xs->cdr;
    }
    return $result;
};

function arrayToList($a, $rightmost) {
    $result = $rightmost ?? null;
    for ($i = $a->length - 1; $i >= 0; $i--) {
        $result = new Pair($a[$i], $result);
    }
    return $result;
};

function read($str)
{
    $stack = null;
    $closers = null;
    $result = UNDEFINED;
    $match;

    $ws_re = <<<REG
/^(\s+|;.*)/
REG;
    $num_re = <<<REG
/^-?[0-9]+(\.[0-9]*)?([eE][-+]?[0-9]+)?/
REG;
    $str_re = <<<REG
/^"([^\\"]|\\"|\\\\)*"/
REG;
    $kw_re = /**/ <<<REG
/^#[-!@$%^&*_=+:<>?a-zA-Z][-!@$%^&*_=+:<>?a-zA-Z0-9]*/
REG;
    $sym_re = /**/ <<<REG
/^[-!@$%^&*_=+:<>?a-zA-Z][-!@$%^&*_=+:<>?a-zA-Z0-9]*/
REG;
    // Those /**/ comments are to unconfuse emacs' lexer.

    $probe = function($re) use (&$str, &$match) {
        $match = null;
        preg_match($re, $str, $match);
        if ($match) {
            $match = $match[0];
            $str = substr($str, strlen($match));
            return true;
        } else {
            return false;
        }
    };

    $emit = function($v) use (&$stack, &$result) {
        if ($stack !== null) {
            $stack->car = new Pair($v, $stack->car);
        } else {
            $result = $v;
        }
    };

    $stackPush = function($closer) use (&$str, &$stack, &$closers) {
        $str = substr($str, 1);
        $stack = new Pair(null, $stack);
        $closers = new Pair($closer, $closers);
    };

    $badInput = function() use (&$str) {
        throw new Exception("Illegal input: " . $str);
    };

    while ($str && ($result === UNDEFINED)) {
        if ($probe($ws_re)) {
        } else if ($probe($num_re)) {
            $emit(+$match); // converts to a number (!!!)
        } else if ($probe($kw_re)) {
            switch ($match) {
                case "#t": $emit(true); break;
                case "#f": $emit(false); break;
                default: $emit(new Keyword($match)); break;
            }
        } else if ($probe($str_re)) {
            die('todo');
            //$raw = $match.substring(1, match.length - 1);
            //$emit(raw.replace("/\\(\"|\\)/g", function ($wholematch, $escaped) { return $escaped; }));
        } else if ($probe($sym_re)) {
            $emit(new Symbol($match));
        } else if ($str[0] === '(') {
            $stackPush(')');
        } else if ($closers === null) {
            $badInput();
        } else if ($str[0] === '.') {
            if ($closers->car !== ')') $badInput();
            $str = substr($str, 1);
            $closers = new Pair('.', $closers);
        } else if ($str[0] === ')' && $closers->car === '.') {
            $str = substr($str, 1);
            $closers = $closers->cdr->cdr; // both the '.' and the ')'
            $tail = $stack->car->car;
            $val = $stack->car->cdr;
            $stack = $stack->cdr;
            $emit(reverse($val, $tail));
        } else if ($str[0] === $closers->car) {
            $str = substr($str, 1);
            $closers = $closers->cdr;
            $val = $stack->car;
            $stack = $stack->cdr;
            $emit(reverse($val, null));
        } else {
            $badInput();
        }
    }

    return [$result, $str];
}

$res = read("(+ 1 1)");
var_dump($res);
