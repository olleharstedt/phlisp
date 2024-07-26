<?php

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

function arrayToList(a, rightmost) {
    var result = rightmost || null;
    for (var i = a.length - 1; i >= 0; i--) {
        result = new Vau.Pair(a[i], result);
    }
    return result;
};

function read($str)
{
    $stack = null;
    $closers = null;
    $result = undefined;
    $match;

    $ws_re = "/^(\s+|;.*)/";
    $num_re = "/^-?[0-9]+(\.[0-9]*)?([eE][-+]?[0-9]+)?/";
    $str_re = <<<REG
/^"([^\\"]|\\"|\\\\)*"/
REG;
    $kw_re = /**/ "/^#[-!@$%^&*_=+:<>/?a-zA-Z][-!@$%^&*_=+:<>/?a-zA-Z0-9]*/";
    $sym_re = /**/ "/^[-!@$%^&*_=+:<>/?a-zA-Z][-!@$%^&*_=+:<>/?a-zA-Z0-9]*/";
    // Those /**/ comments are to unconfuse emacs' lexer.

    function probe(re) {
        match = str.match(re);
        if (match) {
            match = match[0];
            str = str.substring(match.length);
            return true;
        } else {
            return false;
        }
    }

    function emit(v) {
        if (stack !== null) {
            stack.car = new Vau.Pair(v, stack.car);
        } else {
            result = v;
        }
    }

    function stackPush(closer) {
        str = str.substring(1);
        stack = new Vau.Pair(null, stack);
        closers = new Vau.Pair(closer, closers);
    }

    function badInput() {
        throw {message: "Illegal input", str: str};
    }

    while (str && (result === undefined)) {
        if (probe(ws_re)) {
        } else if (probe(num_re)) {
            emit(+match); // converts to a number (!!!)
        } else if (probe(kw_re)) {
            switch (match) {
                case "#t": emit(true); break;
                case "#f": emit(false); break;
                default: emit(new Vau.Keyword(match)); break;
            }
        } else if (probe(str_re)) {
            var raw = match.substring(1, match.length - 1);
            emit(raw.replace(/\\("|\\)/g, function (wholematch, escaped) { return escaped; }));
        } else if (probe(sym_re)) {
            emit(new Vau.Symbol(match));
        } else if (str.charAt(0) === '(') {
            stackPush(')');
            // } else if (str.charAt(0) === '[') {
            //     stackPush(']');
        } else if (closers === null) {
            badInput();
        } else if (str.charAt(0) === '.') {
            if (closers.car !== ')') badInput();
            str = str.substring(1);
            closers = new Vau.Pair('.', closers);
        } else if (str.charAt(0) === ')' && closers.car === '.') {
            str = str.substring(1);
            closers = closers.cdr.cdr; // both the '.' and the ')'
            var tail = stack.car.car;
            var val = stack.car.cdr;
            stack = stack.cdr;
            emit(Vau.reverse(val, tail));
        } else if (str.charAt(0) === closers.car) {
            str = str.substring(1);
            closers = closers.cdr;
            var val = stack.car;
            stack = stack.cdr;
            emit(Vau.reverse(val));
        } else {
            badInput();
        }
    }

    return [result, str];
}
