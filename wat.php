<?php
// Framework: PHP

define("NONE", -9999999);

# Objects
class Nil
{
    function wat_match($e, $rhs)
    {
        if ((!$rhs instanceof Nil)) { fail("NIL expected, but got: " . json_encode($rhs)); }
    }
    function __toString()
    {
        return 'null';
    }
}

$NIL = new Nil();

class Ign
{
    function wat_match($e, $rhs) { }
    function __toString() { return '#ignore'; }
}
$IGN = new Ign();

function fail($m)
{
    throw new Exception($m);
}

function quote($obj) {
    try {
        return '"' . $obj . '"';
    } catch (Throwable $e) {
        return strval($obj);
    }
}

class Continuation {
    public $function;
    public $next;

    function __construct($function, $next) {
        $this->function = $function;
        $this->next = $next;
    }
}

class Capture {
    public $prompt;
    public $handler;
    public $k;

    function __construct($prompt, $handler) {
        $this->prompt = $prompt;
        $this->handler = $handler;
        $this->k = NONE;
    }
}

function captureFrame($capture, $function) {
    $capture->k = new Continuation($function, $capture->k);
}

function continueFrame($k, $f) {
    return ($k->function)($k->next, $f);
}

// Evaluation Core
function evaluate($e, $k, $f, $x) {
    if (method_exists($x, 'wat_eval')) {
        return $x->wat_eval($e, $k, $f);
    } else {
        return $x;
    }
}

class Sym {
    public $name;
    function __construct($name) {
        $this->name = $name;
    }
    function wat_eval($e, $k, $f) {
        return lookup($e, $this->name);
    }
    function wat_match($e, $rhs) {
        if ($e === NONE) {
            fail("undefined argument: " . $this->name);
        }
        $e->bindings[$this->name] = $rhs;
        return $e->bindings[$this->name];
    }
    function __toString() {
        return quote($this->name);
    }

    function debugInfo() {
        return $this->__toString();
    }
}

class Cons {
    public $car;
    public $cdr;
    function __construct($car, $cdr) {
        $this->car = $car;
        $this->cdr = $cdr;
    }
    function wat_eval($e, $k, $f) {
        if (($k instanceof Continuation)) {
            $op = continueFrame($k, $f);
        } else {
            $op = evaluate($e, NONE, NONE, car($this));
        }
        if ($op instanceof Capture) {
            $other = $this;
            captureFrame($op, function($k, $f) use ($other, $e) {
                return $other->wat_eval($e, $k, $f);
            });
            return $op;
        }
        return combine($e, NONE, NONE, $op, $this->cdr);
    }

    function wat_match($e, $rhs) {
        car($this)->wat_match($e, car($rhs));
        cdr($this)->wat_match($e, cdr($rhs));
    }

    function __toString() {
        return '[' . $this->_lst() . ']';
    }

    function _lst() {
        $car = (string) $this->car;
        if ($this->cdr instanceof Nil) {
            return $car;
        }
        try {
            return $car . ', ' . $this->cdr->_lst();
        } catch (Throwable $e) {
            return $car . ', ' . (string)$this->cdr;
        }
    }
    function debugInfo() {
        return $this->__toString();
    }
}

// Operative & Applicative Combiners
function combine($e, $k, $f, $cmb, $o) {
    if (is_object($cmb) && property_exists($cmb, 'wat_combine')) {
        return $cmb->wat_combine($e, $k, $f, $o);
    } else {
        fail("not a combiner: " . strval($cmb));
    }
}

class Opv {
    public $p;
    public $ep;
    public $x;
    public $e;

    function __construct($p, $ep, $x, $e) {
        $this->p = $p;
        $this->ep = $ep;
        $this->x = $x;
        $this->e = $e;
    }
    function wat_combine($e, $k, $f, $o) {
        $xe = make_env($this->e);
        bind($xe, $this->p, $o);
        bind($xe, $this->ep, $e);
        return evaluate($xe, $k, $f, $this->x);
    }
}

function wrap($combiner) { return new Apv($combiner); }
function unwrap($apv) { return $apv->cmb; }

class Apv {
    public $cmb;
    function __construct($cmb) {
        $this->cmb = $cmb;
    }
    function wat_combine($e, $k, $f, $obj) {
        global $NIL;
        if ($k instanceof Continuation) {
            $args = continueFrame($k, $f);
        } else {
            $args = evalArgs($e, null, null, $obj, $NIL);
        }

        if ($args instanceof Capture) {
            $other = $this;
            captureFrame($args, function($k, $f) use ($other, $e, $obj) {
                return $other->wat_combine($e, $k, $f, $obj);
            });
            return $args;
        }

        return $this->cmb->wat_combine($e, null, null, $args);
    }

    function __toString() {
        return '(Apv ' . strval($this->cmb) . ')';
    }
}

function evalArgs($e, $k, $f, $todo, $done) {
    global $NIL;
    if ($todo instanceof Nil) {
        return reverse_list($done);
    }
    if ($k instanceof Continuation) {
        $arg = continueFrame($k, $f);
    } else {
        $arg = evaluate($e, NONE, NONE, car($todo));
    }
    if ($arg instanceof Capture) {
        captureFrame($arg, function($k, $f) use ($e, $todo, $done) {
            return evalArgs($e, $k, $f, $todo, $done);
        });
        return $arg;
    }
    return evalArgs($e, null, null, cdr($todo), cons($arg, $done));
}

// Built-in Combiners 
class __Vau {
    function wat_combine($e, $k, $f, $obj) {
        return new Opv(elt($obj, 0), elt($obj, 1), elt($obj, 2), $e);
    }
    function __toString() {
        return '__Vau';
    }
}

class Def {
    function wat_combine($e, $k, $f, $o) {
        if ($k instanceof Continuation) {
            $value = continueFrame($k, $f);
        } else {
            $value = evaluate($e, NONE, NONE, elt($o, 1));
        }
        if ($value instanceof Capture) {
            captureFrame($value, function($k, $f) use ($e, $o) {
                return $this->wat_combine($e, $k, $f, $o);
            });
            return $value;
        }

        return bind($e, elt($o, 0), $value);
    }

    function __toString() {
        return 'Def';
    }
}

class _Eval
{
    function wat_combine($e, $k, $f, $o) {
        return evaluate(elt($o, 1), $k, $f, elt($o, 0));
    }
}
        
# First-order Control 
class Begin
{
    function wat_combine($e, $k, $f, $o) {
        global $NIL;
        if ($o instanceof Nil) { return NONE; }
        else { return begin($e, $k, $f, $o); }
    }
    function __toString() { return 'Begin'; }
}

function begin($e, $k, $f, $xs) {
    global $NIL;
    if ($k instanceof Continuation) {
        $res = continueFrame($k, $f);
    } else { 
        $res = evaluate($e, NONE, NONE, car($xs))    ;
    }
    if ($res instanceof Capture) {
        captureFrame($res, fn ($k, $f) => begin($e, $k, $f, $xs));
        return $res;
    }
    $kdr = cdr($xs);
    if ($kdr instanceof Nil) { return $res; }
    else { return begin($e, null, null, $kdr); }
}

class _If
{
    function wat_combine($e, $k, $f, $o) {
        if ($k instanceof Continuation) {
            $test = continueFrame($k, $f);
        } else { $test = evaluate($e, NONE, NONE, elt($o, 0)); }
        if ($test instanceof Capture) {
            captureFrame($test, fn($k, $f) => $this->wat_combine($e, $k, $f, $o));
            return $test;
        }
        if ($test) { return evaluate($e, NONE, NONE, elt($o, 1)); }
        else {    return evaluate($e, NONE, NONE, elt($o, 2)); }
    }
}

class __Loop
{
    function wat_combine($e, $k, $f, $o)
    {
        $first = true;
        while (true) {
            if ($first && ($k instanceof Continuation)) { $res = continueFrame($k, $f); }
            else { $res = evaluate($e, NONE, NONE, elt($o, 0)); }
            $first = false;
            if ($res instanceof Capture) {
                captureFrame($res, fn($k, $f) => $this->wat_combine($e, $k, $f, $o));
                return $res;
            }
        }
    }
}

/*
class __Catch()
{
    function wat_combine($e, $k, $f, $o)
    {
        $th = elt($o, 0)
        $handler = elt($o, 1)
        try:
            if (k instanceof Continuation): res = continueFrame(k, f)
            else: res = combine(e, null, null, th, NIL)    
        except Exception as exc:        
            # unwrap handler to prevent eval if exc is sym or cons
            res = combine(e, null, null, unwrap(handler), _list(exc))
        if (res instanceof Capture):
            captureFrame(res, lambda k,f: $this->wat_combine(e, k, f, o))
        return res
    }
}
 */

/*
class Finally()
{
    function wat_combine(e, k, f, o)
    {
        prot, cleanup  = elt(o, 0), elt(o, 1)
        try:
            if (k instanceof Continuation): res = continueFrame(k, f)
            else: res = evaluate(e, NONE, NONE, prot)
            if (res instanceof Capture):
                captureFrame(res, lambda k, f: $this->wat_combine(e, k, f, o))
        finally:                
            if (res instanceof Capture): return res
            else: return doCleanup(e, null, null, cleanup, res)
    }
}

function doCleanup(e, k, f, cleanup, res)
{
    if (k instanceof Continuation): fres = continueFrame(k, f)
    else: fres = evaluate(e, NONE, NONE, cleanup)
    if (fres instanceof Capture):
        captureFrame(fres, lambda k,f: doCleanup(e, k, f, cleanup, res))
        return fres
    else:    
        return res
}
 */

# Delimited Control
class __PushPrompt
{
    function wat_combine($e, $k, $f, $o)
    {
        global $NIL;
        $prompt = elt($o, 0);
        $th = elt($o, 1);
        if ($k instanceof Continuation) { $res = continueFrame($k, $f); }
        else { $res = combine($e, NONE, NONE, $th, $NIL); }
        if ($res instanceof Capture) {
            if ($res->prompt == $prompt) {
                $continuation = $res->k;
                $handler = $res->handler;
                return combine($e, NONE, NONE, $handler, cons($continuation, $NIL));
            }
            else {
                captureFrame($res, fn($k, $f) => $this->wat_combine($e, $k, $f, $o));
                return $res;
            }
        }
        else {
            return $res;
        }
    }
}

class __TakeSubcont
{
    function wat_combine($e, $k, $f, $o)
    {
        global $NIL;
        $prompt = elt($o, 0);
        $handler = elt($o, 1);
        $cap = Capture($prompt, $handler);
        captureFrame($cap, fn( $k, $thef) => combine($e, NONE, NONE, $thef, $NIL));
        return $cap;
    }
}

class __PushSubcont
{
    function wat_combine($e, $k, $f, $o)
    {
        $thek = elt($o, 0);
        $thef = elt($o, 1);
        if ($k instanceof Continuation) { $res = continueFrame($k, $f); }
        else { $res = continueFrame($thek, $thef); }
        if ($res instanceof Capture) {
            captureFrame($res, fn($k, $f) => $this->wat_combine($e, $k, $f, $o));
        }
        return $res;
    }
}

# Dynamic Variables
class DV
{
    function __construct($val)
    {
        $this->val = val;
    }
}

class DNew
{
    function wat_combine($e, $k, $f, $o)
    {
        return DV(elt($o, 0));
    }
}

class DRef
{
    function wat_combine($e, $k, $f, $o)
    {
        return elt($o, 0)->val;
    }
}

/*
class __DLet
{
    function wat_combine($e, $k, $f, $o)
    {
        $dv, $val, $th = elt($o, 0), elt($o, 1), elt($o, 2)
        $oldVal = $dv->val
        $dv->val = $val
        try:
            if ($k instanceof Continuation): $res = continueFrame($k, $f)
            else: $res = combine($e, NONE, NONE, $th, $NIL)
            if (res instanceof Capture):
                captureFrame(res, lambda k, f: $this->wat_combine(e, k, f, o))
            return res
        finally:
            dv->val = oldVal
    }
}
 */

function cons($car, $cdr)
{
    return new Cons($car, $cdr);
}
function car($cons)
{
    return $cons->car;
}
function cdr($cons)
{
    return $cons->cdr;
}
function elt($cons, $i)
{
    if ($i == 0) { return car($cons); }
    else { return elt(cdr($cons), $i - 1); }
}
function sym_name($sym)
{
    return $sym->name;
}

class Env
{
    function __construct($parent)
    {
        if ($parent !== NONE) {
            $this->bindings = dict($parent->bindings);
        }
        else { $this->bindings = []; }
    }
    function __toString() { return str($this->bindings); }
}
    
function make_env($parent = NONE)
{
    return new Env($parent);
}
function lookup($e, $name)
{
    try { return $e->bindings[$name]; }
    catch (Throwable $ex) { fail("unbound: " . $name); }
}

function bind($e, $lhs, $rhs)
{
    $lhs->wat_match($e, $rhs);
    return $rhs;
}
            
function _list()
{
    return array_to_list(func_get_args());
}
function list_star()
{
    global $NIL;
    $args = func_get_args();
    $length = len(args);
    if ($length >= 1) { $c = $args[-1]; }
    else { $c = $NIL; }
    $i = $length - 1;
    while ($i > 0) {
        $c = cons($args[$i - 1], $c);
        $i -= 1;
    }
    return $c;
}

function array_to_list($array, $end = NONE)
{
    global $NIL;
    // c = end and end or NIL
    $c = $end ?? $NIL;
    $i = count($array);
    while ($i > 0) {
        $c = cons($array[$i - 1], $c);
        $i -= 1;
    }
    return $c;
}

function list_to_array($c)
{
    global $NIL;
    $res = [];
    while ($c instanceof Nil) {
        $res->append(car($c));
        $c = cdr($c);
    }
    return $res;
}

function reverse_list($l)
{
    global $NIL;
    $res = $NIL;
    while ($l instanceof Nil) {
        $res = cons(car($l), $res);
        $l = cdr($l);
    }
    return $res;
}

# Parser 
function parse_json_value($obj)
{
    global $IGN;
    if (is_string($obj)) {
        if ($obj == "#ignore") { return $IGN; }
        else { return new Sym($obj); }
    }
    if (is_array($obj)) {
        return parse_json_array($obj);
    }
    return $obj;
}

function parse_json_array($arr)
{
    try {
        throw new Exception('here');
        $i = $arr["#rest"];
        //$front = $arr[:i]
        $front = array_slice($arr, $i);
        return array_to_list(array_map('parse_json_value', $front), parse_json_value($arr[$i + 1]));
    } catch (Throwable $ex) {
        return array_to_list(array_map('parse_json_value', $arr));
    }
}

# PyNI
class PyFun
{
    function __construct($pyfun) {
        if (!is_callable($pyfun)) { 
            fail('no fun');
        }
        $this->pyfun = $pyfun;
    }
    function wat_combine($e, $k, $f, $o) {
        return $this->pyfun(list_to_array($o));
    }
}

function pywrap($pyfun) 
{
    return wrap(new PyFun($pyfun));
}
function py_unop($op)
{
    $f = function ($a) {
        return _eval(str($op) + str($a));
    };
    return pywrap($f);
}
function py_binop($op)
{
    $f = function ($a,$b) {
        return eval(str($a) + str($op) + str($b));
    };
    return pywrap($f);
}
function py_invoke($obj, $method_name, &$args)
{
    $args = func_get_args();
    return getattr($obj, $method_name)($args);
}

function py_callback($cmb)
{
    $f = function($args) { 
        return combine(make_env(), NONE, NONE, cmb, array_to_list($args));
    };
    return $f;
}

#TODO: we need to distinguish between setting an item and an attribute - this is not JavaScript
function setElement(&$obj, $i, $v)
{
    $obj[$i] = $v;
}
function getElement($obj, $i)
{
    try {
        return $obj[$i];
    } catch (Throwable $ex) {
        try {
            return getattr($obj, str($i));
        } catch (Throwable $ex) {
            fail('Python object does not have attribute "' + str($i) + '"');
        }
    }
}
function run($x, $e)
{
    return evaluate($e, NONE, NONE, parse_json_value($x));
}
# Primitives
$primitives = ["BEGIN",
    # CORE
    # FEXPRS
    //["DEF", "--VAU", new __VAU()],
    //["DEF", "EVAL", WRAP(new _EVAL())],
]; /*
    ["DEF", "MAKE-ENVIRONMENT", PYWRAP('make_env')],
    ["DEF", "WRAP", PYWRAP('WRAP')],
    ["DEF", "UNWRAP", PYWRAP('UNWRAP')],
    # VALUES
    ["DEF", "CONS", PYWRAP('CONS')],
    ["DEF", "CONS?", PYWRAP(fn($OBJ) => TYPE($OBJ) == TYPE(CONS(NULL,NULL)))],
    ["DEF", "NIL?", PYWRAP(fn($OBJ) => $OBJ == $NIL)],
    ["DEF", "SYMBOL?", PYWRAP(fn($OBJ) => TYPE($OBJ) == TYPE(SYM('SYMBOL')))],
    ["DEF", "SYMBOL-NAME", PYWRAP('SYM_NAME')],
    # FIRST-ORDER CONTROL
    ["DEF", "IF", new _IF()],
    ["DEF", "--LOOP", new __LOOP()],
    //["DEF", "THROW", PYWRAP(FAIL)],
    //["DEF", "--CATCH", WRAP(__CATCH())],
    //["DEF", "FINALLY", FINALLY()],
    # DELIMITED CONTROL
    ["DEF", "--PUSH-PROMPT", WRAP(new __PUSHPROMPT())],
    ["DEF", "--TAKE-SUBCONT", WRAP(new __TAKESUBCONT())],
    ["DEF", "--PUSH-SUBCONT", WRAP(new __PUSHSUBCONT())],
    # DYNAMICALLY-SCOPED VARIABLES
    ["DEF", "DNEW", WRAP(new DNEW())],
    //["DEF", "--DLET", WRAP(new __DLET())],
    ["DEF", "DREF", WRAP(new DREF())],
    # PYTHON INTERFACE
    ["DEF", "PY-WRAP", PYWRAP('PYWRAP')],
    ["DEF", "PY-UNOP", PYWRAP('PY_UNOP')],
    ["DEF", "PY-BINOP", PYWRAP('PY_BINOP')],
    ["DEF", "PY-ELEMENT", PYWRAP('GETELEMENT')],
    ["DEF", "PY-SET-ELEMENT", PYWRAP('SETELEMENT')],
    ["DEF", "PY-INVOKE", PYWRAP('PY_INVOKE')],
    ["DEF", "PY-CALLBACK", PYWRAP('PY_CALLBACK')],
    ["DEF", "LIST-TO-ARRAY", PYWRAP('LIST_TO_ARRAY')],
    # OPTIMIZATION
    ["DEF", "LIST*", PYWRAP('LIST_STAR')],

    # Primitives
    ["def", "quote", ["--vau", ["x"], "#ignore", "x"]],
    ["def", "list", ["wrap", ["--vau", "arglist", "#ignore", "arglist"]]],
    ["def", "string", ["--vau", ["sym"], "#ignore", ["symbol-name", "sym"]]],

    ["def", "make-macro-expander",
    ["wrap",
    ["--vau", ["expander"], "#ignore",
    ["--vau", "operands", "env",
    ["eval", ["eval", ["cons", "expander", "operands"], ["make-environment"]], "env"]]]]],

    ["def", "vau",
    ["make-macro-expander",
    ["--vau", ["params", "env-param", "#rest", "body"], "#ignore",
    ["list", "--vau", "params", "env-param", ["cons", "begin", "body"]]]]],

    ["def", "macro",
    ["make-macro-expander",
    ["vau", ["params", "#rest", "body"], "#ignore",
    ["list", "make-macro-expander", ["list*", "vau", "params", "#ignore", "body"]]]]],

    ["def", "lambda",
    ["macro", ["params", "#rest", "body"],
    ["list", "wrap", ["list*", "vau", "params", "#ignore", "body"]]]],
    ["def", "loop",
    ["macro", "body",
    ["list", "--loop", ["list*", "begin", "body"]]]],
    ["def", "catch",
    ["macro", ["protected", "handler"],
    ["list", "--catch", ["list", "lambda", [], "protected"], "handler"]]],

    ["def", "push-prompt",
    ["macro", ["prompt", "#rest", "body"],
    ["list", "--push-prompt", "prompt", ["list*", "lambda", [], "body"]]]],
    ["def", "take-subcont",
    ["macro", ["prompt", "k", "#rest", "body"],
    ["list", "--take-subcont", "prompt", ["list*", "lambda", ["list", "k"], "body"]]]],
    ["def", "push-subcont",
    ["macro", ["k", "#rest", "body"],
    ["list", "--push-subcont", "k", ["list*", "lambda", [], "body"]]]],

    # Python
    ["def", "array", ["lambda", "args", ["list-to-array", "args"]]],

    ["def", "define-py-unop",
    ["macro", ["op"],
    ["list", "def", "op", ["list", "py-unop", ["list", "string", "op"]]]]],

    ["define-py-unop", "not"],
    ["define-py-unop", "type"],
    ["define-py-unop", "~"],

    ["def", "define-py-binop",
    ["macro", ["op"],
    ["list", "def", "op", ["list", "py-binop", ["list", "string", "op"]]]]],

    ["define-py-binop", "!="],
    ["define-py-binop", "!=="],
    ["define-py-binop", "%"],
    ["define-py-binop", "&"],
    ["define-py-binop", "&&"],
    ["define-py-binop", "*"],
    ["define-py-binop", "+"],
    ["define-py-binop", "-"],
    ["define-py-binop", "/"],
    ["define-py-binop", "<"],
    ["define-py-binop", "<<"],
    ["define-py-binop", "<="],
    ["define-py-binop", "=="],
    ["define-py-binop", "==="],
    ["define-py-binop", ">"],
    ["define-py-binop", ">>"],
    ["define-py-binop", ">>>"],
    ["define-py-binop", "^"],
    ["define-py-binop", "in"],
    ["define-py-binop", "instanceof"],
    ["define-py-binop", "|"],
    ["define-py-binop", "or"],

    ["def", ".",
    ["macro", ["field", "obj"],
    ["list", "py-element", "obj", ["list", "string", "field"]]]],

    ["def", "#",
    ["macro", ["method", "obj", "#rest", "args"],
    ["list*", "py-invoke", "obj", ["list", "string", "method"], "args"]]],
];
*/
# Init 
$e = make_env();
bind($e, new Sym("def"), new Def());
bind($e, new Sym("begin"), new Begin());
# API
run($primitives, $e);

/*
if __name__ == '__main__':
    # Tests
    program = ['begin',
        ['def','x',10],
        ['*', 'x', ['*','x','x']]]
    assert 1000 == run(program)
    assert 14 == run(['begin',
         ["def", "bar", ["lambda", ["x"], ["*", 'x', 2]]],
         ['bar', 7]])

    # Call a Python method
    class Foo:
        function baaz(o): print o; $this->txt = o
    foo = Foo()
    run(['#', 'baaz', foo, ['string', 'hello world! yabba dabba doo!']])
    assert foo.txt.startswith('hello')
    assert run(['.', 'txt', foo]) .startswith('hello')

    # Check AST converted to string is JSON (and Wat)
    assert 1000 == run(list_to_array(parse_json_value(program)))

    # TODO:
    # Produce some HTML with embedded Wat (JS) 
    # https://raw.github.com/manuel/wat-js/master/wat.js
*/
//$program = ['begin', ['def','x',10], ['*', 'x', ['*','x','x']]];
//run($program, $e);
