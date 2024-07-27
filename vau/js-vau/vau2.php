<?php

//---------------------------------------------------------------------------
// Data structures, utilities, and reader

define("UNDEFINED", -99999);

class Symbol
{
    public $name;
    function __construct($name)
    {
        $this->name = $name;
    }
}

class Keyword
{
    public $name;
    function __construct($name)
    {
        $this->name = $name;
    }
}

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
        $result[] = $xs->car;
        $xs = $xs->cdr;
    }
    return $result;
};

function arrayToList($a, $rightmost) {
    $result = $rightmost ?? null;
    for ($i = count($a) - 1; $i >= 0; $i--) {
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
/^"([^\"]|\"|\\\\)*"/
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
            $raw = substr($match, strlen($match) - 1);
            //$emit(str_replace($raw, "/\\(\"|\\)/g", function ($wholematch, $escaped) { return $escaped; }));
            $emit(str_replace('"', '', $raw));
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

//---------------------------------------------------------------------------
// Evaluator core, Operatives, Applicatives, Primitives

function eval_($exp, $env) {
    $vm = new VM($exp, $env);
    return $vm->run();
}

class VM
{
    public $a = null;
    public $k = null;

    function __construct($exp, $env)
    {
        $this->pushEval($exp, $env);
    }

    function pushEval($exp, $env)
    {
        $this->a = $exp;
        $this->k = new KEval($env, $this->k);
    }

    function run()
    {
        while ($this->k) {
            $frame = $this->k;
            $this->k = "deliberately undefined continuation frame";
            $frame->invoke($this);
        }
        return $this->a;
    }
}

class KEval 
{
    public $env;
    public $k;
    public $name = "KEval";

    function __construct($env, $k)
    {
        $this->env = $env;
        $this->k   = $k;
    }

    function invoke ($vm) {
        if ($vm->a instanceof Symbol) {
            $val = $this->env[$vm->a->name] ?? UNDEFINED;
            if ($val === UNDEFINED) {
                var_dump($vm->a->name);
                throw new Exception("Undefined variable ");
            }
            $vm->a = $val;
            $vm->k = $this->k;
        } else if (!($vm->a instanceof Pair)) {
            $vm->k = $this->k;
        } else {
            $vm->k = new KCombination($vm->a->cdr, $this->env, $this->k);
            $vm->pushEval($vm->a->car, $this->env);
        }
    }
}

class KCombination 
{
    public $argtree;
    public $env;
    public $k;
    public $name = "KCombination";
    function __construct($argtree, $env, $k) {
        $this->argtree = $argtree;
        $this->env = $env;
        $this->k = $k;
    }

    function invoke ($vm) {
        if (is_callable($vm->a)) {
            evalargs($vm, $this->argtree, null, $this->env, new KPrimitiveApplier($vm->a, $this->k));
        } else if (is_object($vm->a) && $vm->a !== null && method_exists($vm->a, 'invokeOp')) {
            $vm->k = $this->k;
            $vm->a->invokeOp($vm, $this->env, $this->argtree);
        } else {
            print_r($vm);
            throw new Exception("Attempt to invoke non-callable");
        }
    }
}

class KPrimitiveApplier 
{
    public $rator;
    public $k;
    public $name = "KPrimitiveApplier";
    function __construct($rator, $k)
    {
        $this->rator = $rator;
        $this->k = $k;
    }
    function invoke($vm)
    {
        $vm->k = $this->k;
        //$vm->a = $this->rator->apply(null, listToArray($vm->a));
        $vm->a  =call_user_func($this->rator, listToArray($vm->a));
    }
}

function evalargs($vm, $args, $revacc, $env, $k) {
    if ($args === null) {
        $vm->a = reverse($revacc, null);
        $vm->k = $k;
    } else {
        $vm->k = new KEvalArgs($args->cdr, $revacc, $env, $k);
        $vm->pushEval($args->car, $env);
    }
};

class KEvalArgs 
{
    public $name = "KEvalArgs";
    public $remainder;
    public $revacc;
    public $env;
    public $k;
    function __construct($remainder, $revacc, $env, $k) {
        $this->remainder = $remainder;
        $this->revacc = $revacc;
        $this->env = $env;
        $this->k = $k;
    }
    function invoke($vm) {
        evalargs($vm, $this->remainder, new Pair($vm->a, $this->revacc), $this->env, $this->k);
    }
}

/* todo
function extend(baseenv) {
    function c() {}
        c->prototype = baseenv;
    return new c();
};
 */

class Operative 
{
    public $formals;
    public $envformal;
    public $body;
    public $staticenv;
    function __construct($formals, $envformal, $body, $staticenv) {
        $this->formals = $formals;
        $this->envformal = $envformal;
        $this->body = $body;
        $this->staticenv = $staticenv;
    }
    function invokeOp($vm, $dynenv, $argtree) {
        $staticenv = clone $this->staticenv;
        match($staticenv, $this->formals, $argtree);
        if ($this->envformal->name !== "#ignore") {
            $staticenv[$this->envformal->name] = $dynenv;
        }
        $vm->pushEval($this->body, $staticenv);
    }
}

function match($env, $pattern, $value) {
    if ($pattern instanceof Keyword && $pattern->name === "#ignore") {
        // no-op.
    } else if ($pattern instanceof Symbol) {
        $env[$pattern->name] = $value;
    } else if ($pattern instanceof Pair && $value instanceof Pair) {
        match($env, $pattern->car, $value->car);
        match($env, $pattern->cdr, $value->cdr);
    } else if ($pattern === $value) {
        // no-op.
    } else {
        throw new Exception("Argument tree mismatch");
    }
}

class Applicative 
{
    public $underlying;
    function __construct($underlying) {
        $this->underlying = $underlying;
    }
    function invokeOp($vm, $env, $argtree) {
        evalargs($vm, $argtree, null, $env, new KApplicativeApplier($this->underlying, $env, $vm->k));
    }
}

class KApplicativeApplier 
{
    public $name = "KApplicativeApplier";
    public $underlying;
    public $env;
    public $k;
    function __construct($underlying, $env, $k) {
        $this->underlying = $underlying;
        $this->env = $env;
        $this->k = $k;
    }
    function invoke($vm) {
        $vm->k = $this->k;
        $vm->pushEval(new Pair($this->underlying, $vm->a), $this->env);
    }
}

class Primitive 
{
    public $underlying;
    function __construct($underlying) {
        $this->underlying = $underlying;
    }

    public function invokeOp($vm, $env, $argtree) {
        //$this->underlying($vm, $env, $argtree);
        call_user_func(
            $this->underlying,
            $vm, $env, $argtree
        );
    }
}

//---------------------------------------------------------------------------
// Core environment

$coreenv = new ArrayObject();

$coreenv['eval'] = new Primitive(function ($vm, $dynenv, $argtree) {
    $vm->k = new KEvalPrim1($argtree->car, $dynenv, $vm->k);
    $vm->pushEval($argtree->cdr->car, $dynenv);
});

class KEvalPrim1
{
    public $name = "KEvalPrim1";
    public $expexp;
    public $env;
    public $k;
    function __construct($expexp, $env, $k) {
        $this->expexp = $expexp;
        $this->env = $env;
        $this->k = $k;
    }
    function invoke($vm)
    {
        $vm->k = new KEvalPrim2($vm->a, $this->k);
        $vm->pushEval($this->expexp, $this->env);
    }
}

class KEvalPrim2 
{
    public $name = "KEvalPrim2";
    public $env;
    public $k;
    function __construct($env, $k) {
        $this->env = $env;
        $this->k = $k;
    }
    function invoke($vm) {
        $vm->k = $this->k;
        $vm->pushEval($vm->a, $this->env);
    }
}

$coreenv['$define!'] = new Primitive(function ($vm, $env, $argtree) {
    $name = $argtree->car;
    if (!($name instanceof Symbol)) {
        throw new Exception('$define!: needs symbol name');
    }
    $vm->k = new KDefine($env, $name, $vm->k);
    $vm->pushEval($argtree->cdr->car, $env);
});

class KDefine 
{
    public $name = "KDefine";
    public $env;
    public $k;
    function __construct($env, $name, $k) {
        $this->env = $env;
        $this->name = $name;
        $this->k = $k;
    }
    function invoke($vm) {
        $this->env[$this->name->name] = $vm->a;
        $vm->k = $this->k;
    }
}

$coreenv['$begin'] = new Primitive(function ($vm, $env, $argtree) {
    if ($argtree === null) {
        $vm->a = UNDEFINED;
    } else {
        begin1($vm, $env, $argtree);
    }
});

function begin1($vm, $env, $argtree) {
    if ($argtree->cdr !== null) {
        $vm->k = new KBegin($argtree->cdr, $env, $vm->k);
    }
    $vm->pushEval($argtree->car, $env);
}

class KBegin 
{
    public $name = "KBegin";
    public $exps;
    public $env;
    public $k;
    function __construct($exps, $env, $k) {
        $this->exps = $exps;
        $this->env = $env;
        $this->k = $k;
    }

    function invoke($vm) {
        $vm->k = $this->k;
        begin1($vm, $this->env, $this->exps);
    }
}

$coreenv['$vau'] = new Primitive(function ($vm, $dynenv, $argtree) {
    $body = $argtree->cdr->cdr;
    if ($body->cdr === null) {
        $body = $body->car;
    } else {
        $body = new Pair($coreenv['$begin'], $body);
    }
    $vm->a = new Operative($argtree->car, $argtree->cdr->car, $body, $dynenv);
});

$coreenv['wrap'] = function ($underlying) {
    return new Applicative($underlying);
};

$coreenv['unwrap'] = function ($applicative) {
    if ($applicative instanceof Applicative) {
        return $applicative->underlying;
    } else if (is_callable($applicative)) {
        return new Primitive(function ($vm, $dynenv, $argtree) {
            $vm->a = $applicative->apply(null, listToArray($argtree));
        });
    } else {
        throw new Exception("Attempt to unwrap non-unwrappable object");
    }
};

//---------------------------------------------------------------------------
// Base environment (extends core environment)

$baseenv = clone $coreenv;

$baseenv['$if'] = new Primitive(function ($vm, $dynenv, $argtree) {
    $vm->k = new KIf($argtree->cdr->car, $argtree->cdr->cdr->car, dynenv, $vm->k);
    $vm->pushEval($argtree->car, dynenv);
});

class KIf
{
    public $name = "KIf";
    public $trueBranch;
    public $falseBranch;
    public $env;
    public $k;
    function __construct($trueBranch, $falseBranch, $env, $k) {
        $this->trueBranch = $trueBranch;
        $this->falseBranch = $falseBranch;
        $this->env = $env;
        $this->k = $k;
    }
    function invoke($vm) {
        $vm->k = $this->k;
        $vm->pushEval($vm->a ? $this->trueBranch : $this->falseBranch, $this->env);
    }
}

$baseenv['cons'] = function ($a, $d) {
    return new Pair($a, $d);
};

$baseenv["pair?"] = function ($x) {
    return $x instanceof Pair;
};

$baseenv["null?"] = function ($x) {
    return $x === null;
};

$baseenv['car'] = function ($x) { return $x->car; };
$baseenv['cdr'] = function ($x) { return $x->cdr; };

$baseenv["list*"] = function () {
    $arguments = func_get_args();
    return arrayToList(
        array_slice($arguments, 0, count($arguments) - 1),
        $arguments[count($arguments) - 1]
    );
};

$baseenv["env-set!"] = function ($env, $sym, $val) {
    $env[$sym->name] = $val;
    return $val;
};

$baseenv["env-lookup"] = function ($env, $sym) {
    return $env[$sym->name];
};

$baseenv["extend-env"] = function ($env) {
    return clone $env;
};

$baseenv["primitive-applicative?"] = function ($x) {
    return is_callable($x);
};

$baseenv["primitive-operative?"] = function ($x) {
    return $x instanceof Primitive;
};

$baseenv["applicative?"] = function ($x) {
    return $x instanceof Applicative;
};

$baseenv["operative?"] = function ($x) {
    return $x instanceof Operative;
};

$baseenv["symbol?"] = function ($x) {
    return $x instanceof Symbol;
};

$baseenv["keyword?"] = function ($x) {
    return $x instanceof Keyword;
};

$baseenv["==="] = function ($a, $b) {
    return $a === $b;
};

function to_string($x) {
    if (is_string($x)) return $x;
    if ($x instanceof Symbol) return $x->name;
    if ($x instanceof Keyword) return substr($x->name, 1);
    throw new Exception("Cannot extract string from object");
};

$baseenv["raw@"] = new Primitive(function ($vm, $dynenv, $argtree) {
    $vm->a = $argtree->car[to_string($argtree->cdr->car)];
});

$baseenv["raw@="] = new Primitive(function ($vm, $dynenv, $argtree) {
    $argtree->car[to_string($argtree->cdr->car)] = $argtree->cdr->cdr->car;
    $vm->a = null;
});

$baseenv["display"] = function ($x) {
    print_r($x);
};

$input = '
($begin
  ($define! *base-env*
    (($vau #ignore env env)))
  ($define! $lambda
    ($vau (formals . body) dynenv
      (wrap (eval (list* $vau formals #ignore body) dynenv))))
  ($define! list ($lambda x x))
  (list 1 2 3)
)
';
$readOutput = read($input);
$form = $readOutput[0];
$input = $readOutput[1];
if ($form === UNDEFINED) die('No form');
$result = eval_($form, $baseenv);
var_dump($result);
