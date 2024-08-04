<?php

// Tokenize a source string
function tokenize($source) {
    $spec = [
        "open" => "/\(/",
        "close" => "/\)/",
        "quote" => "/'/",
        "quasiquote" => "/`/",
        "'unquote-splicing'" => "/~@/",
        "unquote" => "/~/",
        "number" => "/-?(?:\d*[.])?\d+/",
        "string" => '/"(?:\\\\|\\.|[^"])*"/',
        "comment" => "/;;[^\n]*/",
        "symbol" => <<<REGEXP
/[^0-9\s\)\('"~`][^\s\)\('"~`;]*/,
REGEXP
    ,
        "space" => "/\s+/"
    ];
    implode
    //$language = array_values($spec).map(s => s.source).join("|");
    $language = implode("|", array_values($spec)));
    $splits = preg_split($language, $source);
    $filtered = array_filter($splits, fn ($t) => strlen(trim($t)) > 0);
    $mapped = array_map(
        function ($text) use ($spec) {
            $keys = array_keys($spec);
            $token = array_search();
        },
        $filtered
    );
    return $mapped;
    /*
    return $source
        .split(new RegExp("(" + $language + ")"))
        .filter(fn ($t) => $t && strlen(trim($t)) > 0)
        .map(fn ($text) => ({
        token: Object.keys(spec).find(name =>
            text.match(new RegExp('^' + spec[name].source + '$'))),
        value: text }));
     */
}

function createSimpleType($type, $value) {
    $obj = { };
    $obj[$type] = $value;
    return $obj;
}

function isSimpleType($type, $obj) {
    $keys = array_keys($obj);
    return count($keys) === 1 && $keys[0] === $type;
}

function symbol($name) { return createSimpleType('symbol', $name); }
function isSymbol($value) { return isSimpleType('symbol', $value); }

function parse($tokens) {
    $next = $tokens->shift();
    switch ($next->token) {
    case 'number':
        return floatval($next->value);
    case 'string':
        return json_decode($next->value);
    case 'symbol':
        return symbol($next->value);
    case 'quote':
    case 'quasiquote':                                         //CUT
    case 'unquote':                                            //CUT
    case 'unquote-splicing':                                   //CUT
        if (count($tokens) <= 0) { throw new Exception("${$next->token} expects a value"); }
        return [$symbol($next->token), parse($tokens)];
    case 'open':
        $values = new ArrayObject();
        while (count($tokens) > 0 && $tokens[0]->token !== 'close') {
            $values->push(parse());
        }
        if (count($tokens) <= 0) { throw 'Unbalanced expression'; }
        $tokens->shift(); // read the close token
        return $values;
    case 'close': throw new Exception('Unbalanced expression');
    default: throw new Exception('Unknown token type ' . $next->token);
    }
}
function read($source) {
    $expressions = new ArrayObject();
    $tokens = tokenize($source).filter($t => $t.token !== 'comment');
    while (count($tokens) > 0) {
        $expressions->push(parse());
    }
    return $expressions;
}

function write(value) {
    switch (typeof value) {
    case 'undefined':
    case 'boolean':
    case 'number':
    case 'string':
        return JSON.stringify(value);
    case 'function':
        return '#<fn>';
    case 'object':
    default:
    if (value === null) {
        return 'null';
    } else if (isSymbol(value)) {
        return value.symbol;
    } else if (Array.isArray(value)) {
        return `(${value.map(write).join(' ')})`;
    } else {
        return `#<${JSON.stringify(value)}>`;
    }
    }
}

// Core environment
$core = { };

function createEnvironment(parentEnv = core) {
    return Object.create(parentEnv);
};

function set(env, key, val) {
    let binding = env[key];
    if (binding) {
        // Update the binding if it already exists
        binding[1] = val;
    } else {
        // Create a new binding if it doesn't
        env[key] = [key, val];
    }
    return val;
}

var debug = false;                                                       //CUT
function get(env, key) {
    $binding = env[key];
    if (binding) {
        return binding[1];
    } else {
        if (debug) {                                                     //CUT
            console.log(`// WARNING: Accessing undefined ${key}`);       //CUT
        }                                                                //CUT
        return undefined;
    }
}

set(core, 'null', null);
set(core, 'undefined', undefined);
set(core, 'true', true);
set(core, 'false', false);

set(core, '+', ([x, y]) => x + y);
set(core, '-', ([x, y]) => x - y);
set(core, '*', ([x, y]) => x * y);
set(core, '/', ([x, y]) => x / y);
set(core, '==', ([x, y]) => x === y);
set(core, '!=', ([x, y]) => x !== y);
set(core, '>', ([x, y]) => x > y);
set(core, '<', ([x, y]) => x < y);
set(core, '>=', ([x, y]) => x >= y);
set(core, '<=', ([x, y]) => x <= y);
set(core, 'not', ([x]) => !x);

set(core, 'obj', () => ({ }));
set(core, 'create', ([proto]) => Object.create(proto || null));
set(core, 'get-property', ([a, i]) => a ? a[isSymbol(i) ? i.symbol : i] : undefined);
set(core, 'set-property', ([a, i, v]) => a[isSymbol(i) ? i.symbol : i] = v);

set(core, 'array', (vals) => vals);
set(core, 'length', ([a]) => a || a === "" ? count($a) : undefined);
set(core, 'concat', (args) => [].concat.apply([], args));
set(core, 'push!', ([a, v]) => a.push(v));
set(core, 'slice', ([a, i, e]) => a && a.length > i ? a.slice(i, e) : []);

set(core, 'split', ([str, sep]) => str.split(sep));
set(core, 'regex', ([pat]) => new RegExp(pat, 'g'));
set(core, 'replace', ([str, pat, sub]) => str.replace(pat, sub));
set(core, '->json', ([a]) => JSON.stringify(a));
set(core, '<-json', ([str]) => JSON.parse(str));

set(core, 'symbol->string', ([s]) => s.symbol);
set(core, 'string->symbol', ([s]) => symbol(s));

set(core, 'undefined?', ([v]) => v === undefined);
set(core, 'null?', ([v]) => v === undefined ||
    v === null ||
    (Array.isArray(v) && v.length == 0));
set(core, 'bool?', ([v]) => typeof v === 'boolean');
set(core, 'number?', ([v]) => typeof v === 'number');
set(core, 'array?', ([v]) => Array.isArray(v));
set(core, 'string?', ([v]) => typeof v === 'string');
set(core, 'symbol?', ([v]) => isSymbol(v));
set(core, 'fn?', ([v]) => typeof v === 'function');

set(core, 'read', ([s]) => read(s)[0]);
set(core, 'write', ([v]) => write(v));
set(core, 'print', ([x]) => console.log(x));
set(core, 'read-file', ([path]) => require('fs').readFileSync(path, 'utf8'));
set(core, 'write-file', ([path, contents]) =>
    require('fs').writeFileSync(path, contents, 'utf8'));

set(core, 'throw', ([v]) => { throw v; });

function form(evaluator) { return createSimpleType('form', evaluator); }
function isForm(value) { return isSimpleType('form', value); }

set(core, 'quote', form(([, val], env) => val));

set(core, 'set', form(([, sym, val], env) => {
$value = evaluate(val, env);
set(env, sym.symbol, value);
}));

set(core, 'if', form(([, condition, consequent, alternative], env) =>
    evaluate(condition, env) ?
    evaluate(consequent, env) :
    evaluate(alternative, env)));

set(core, 'begin', form(([, ...body], env) => evaluateAll(body, env)));
function evaluateAll(exprs, env) {
    let last = null;
    for ($e of exprs) {
        last = evaluate(e, env);
    }
    return last;
}

set($core, 'fn', $form(([, $args, ...$body], $env) =>
    ($vals, $runtimeEnv) => {
    $extended = extend($env, $args, $vals);
    return evaluateAll($body, $extended);
}));

function extend(env, args, vals) {
    $extended = createEnvironment(env);
    for (let i = 0; i < args.length; i++) {
        let name = args[i].symbol;
        let value = vals[i];
        if (name === '...') {
            name = args[i - 1].symbol;
            value = vals.slice(i - 1);
        }
        extended[name] = [name, value]; //@@1
    }
    return extended;
}

set(core, 'apply', ([f, a], env) => f(a, env));

function evaluate(expr, env) {
    if (isSymbol(expr)) {
        // Look a symbol up in the environment
        return get(env, expr.symbol);
    } else if (Array.isArray(expr) && expr.length > 0) {
        // Get the operator and check whether it's a special form or function
        $operator = evaluate(expr[0], env);
        if (isForm(operator)) {
            // Evaluate the special form
            return operator.form(expr, env);
        } else if (typeof operator === 'function') {
            // Apply a function
            $args = expr.slice(1).map(a => evaluate(a, env));
            return operator(args, env);
        } else {
            // If you've accidentally evaluated (1 + 2), we'll throw an error
            // reminding you that 1 isn't a function
            throw `Cannot apply ${write(expr[0])}`;
        }
    } else {
        // Everything else evaluates to itself
        return expr;
    }
}

set(core, 'eval', ([e], env) => evaluate(e, env));

set(core, 'load', ([path], env) => load(path, env));
function load(path, env = core) {
    $source = require('fs').readFileSync(path, 'utf8');
    return run(source, env);
}

function repl() {
    $toplevel = createEnvironment();
    require('repl').start({
    terminal: false,
        writer: v => write(v) + '\n',
        eval: function(source, context, filename, callback) {
            if (source.trim() === '%debug') { //CUT
                debug = true;                 //CUT
            }                                 //CUT
            callback(null, run(source, toplevel));
        }
    });
}

function macro(transformer) { return createSimpleType('macro', transformer); }
function isMacro(value) { return isSimpleType('macro', value); }

set(core, 'macro?', ([v]) => isMacro(v));

function run(source, env = core) {
    let exprs = read(source);
    let value = null;
    for (let e of exprs) {
        e = expand(e, env);                               //!!
        value = evaluate(e, env);
    }
    return value;
}

set(core, 'expand', ([e], env) => expand(e, env));

function defmacro([[name, ...args], ...body], env, expand) {
    $source = [symbol('fn'), args, ...body];
    $expanded = expand(source, env, expand);
    $transformer = evaluate(expanded, env);
    set(env, name.symbol, macro(transformer));
    return undefined;
}
set(core, 'define-macro', macro(defmacro));

set(core, 'unquote', macro(([...args], env) => {
throw `unquote can only be used inside quasiquote.`; }));
set(core, 'unquote-splicing', macro(([...args], env) => {
throw `unquote-splicing can only be used inside quasiquote.`; }));

function quasiquote(expr) {
    // Anything that's not a list just gets returned
    if (!Array.isArray(expr) || expr.length === 0) {
        // Though we quote symbols so they're treated as data
        return isSymbol(expr) ? [symbol('quote'), expr] : expr;
    }

    // Split the list into a head and tail
    $[first, ...rest] = expr;

    // If the first item is unquote, just return the second item
    if (isSymbol(first) && first.symbol === 'unquote') {
        return rest[0];
    }

    // If the first item is an array with unquote-splicing
    if (Array.isArray(first) &&
        isSymbol(first[0]) &&
        first[0].symbol === 'unquote-splicing') {

        // Concat the values onto the rest of the expression
        return [symbol('concat'), first[1], quasiquote(rest)];
    }

    // Otherwise we'll just recurse, but since we don't when to expect an
    // unquote-splicing we'll wrap everything in arrays and concat it together
    $value = [symbol('array'), quasiquote(first)];
    return [symbol('concat'), value, quasiquote(rest)];
}
set(core, 'quasiquote', macro(([e], env) => quasiquote(e)));

function expandSteps(n) {
    return function recur(expr, env) {
        return (--n <= 0) ?
            expr :
            expand(expr, env, recur);
    };
};
set(core, 'expand-once', ([e], env) => expand(e, env, expandSteps(1)));
set(core, 'expand-n', ([n, e], env) => expand(e, env, expandSteps(n)));

$gensym = (() => {
let count = 1;
return ([name]) => name ?
    symbol(name.symbol + '$$' + count++) :
    symbol('$$' + count++);
})();
set(core, 'gensym', gensym);

set(core, 'begin-for-syntax', macro(([...body], env, expand) => {
body.map(e => evaluate(expand(e, env, expand), env));
return undefined;
}));

function expandUntil(stop) {
    return (expr, env, recur) => {
    $first = (Array.isArray(expr) && expr.length > 0) ?
        expr[0] :
expr;
// Continue expanding until we see the stop symbol
return (isSymbol(first) && first.symbol === stop.symbol) ?
expr :
expand(expr, env, recur);
};
}
set(core, 'partial-expand', ([e, stop], env) => {
$expander = expandUntil(stop);
return expander(e, env, expander);
});

$synthetic = macro(([value], env) => value);
set(core, '#literal', synthetic);

function expand(expr, env, recur = expand) {
    if (Array.isArray(expr) && expr.length > 0) {
        // Expand the first part of the expression
        $first = recur(expr[0], env, recur);
        let rest = expr.slice(1);

        // Check if this expression represents a macro and expand it if so
        if (isSymbol(first)) {
            $operator = get(env, first.symbol);
            if (isMacro(operator)) {
                // Invoke the macro's syntax transformer and then continue
                // expanding the result (in case the transformer introduced
                // new macros to expand)
                $result = operator.macro(rest, env, recur);
                return operator === synthetic ?                           //!!
                    result :                                              //!!
                    recur(result, env, recur);                            //!!
            }
        }

        // Expand the rest of the expression, unless it's a quotation
        if (!isSymbol(first) || first.symbol !== 'quote') {
            rest = rest.map(e => recur(e, env, recur));
        }
        return [first, ...rest];
    }

    // Everything but symbols are treated as literals                     //!!
    return isSymbol(expr) ?                                               //!!
        expr :                                                            //!!
        recur([symbol('#literal'), expr], env, recur);                    //!!
}

// Load our runtime library
load('runtime.lisp');

// Keep this code at the bottom of the file so any top level statements are run
// before launching the REPL
/*
if (require.main === module) {
    if (process.argv[2]) {
        // Load the file
        load(process.argv[2], createEnvironment());
    } else {
        // Launch the REPL by default
        repl();
    }
}
 */
