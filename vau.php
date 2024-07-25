<?php

/**
 * http://gliese1337.blogspot.com/2012/04/schrodingers-equation-of-software.html
 * https://sep.turbifycdn.com/ty/cdn/paulgraham/bellanguage.txt?t=1688221954&
 * https://news.ycombinator.com/item?id=26162673
 */

class Vau
{
    // "Evaluate an expression in an environment."
    function eval($x, $env)
    {
        if ($x instanceof Symbol) {    # variable referenc
            return $env[$x];
        } elseif ($x instanceof List_) {    # procedure call
            $proc = $this->eval($x[0], $env);
            if (is_callable($proc)) {
                unset($x[0]);
                return $proc($env, ...$x);
            }
            throw new ValueError(sprintf("%s = %s is not a procedure", $x[0], $proc));
        }
        return $x; # literal constant
    }

    function vau($close_env, $vars, $call_env_sym, $body)
    {
        $closure = function($call_env, &$args) {
            //$new_env = Env(array_map(null, $this->vars, $args), $this->clos_env);
        };
        return $closure;
    }

    /*
def vau(clos_env, vars, call_env_sym, body):
    def closure(call_env, *args):
        new_env = Env(zip(self.vars, args), self.clos_env)
        new_env[self.sym] = call_env
        return eval(self.body, new_env)
    return closure

def defvar(v,var,e):
    val = eval(e, v)
    v[var] = val
    return val
 
def setvar(v,var,e):
    val = eval(e, v)
    env.find(var)[var] = val
    return val

def cond(v,*x):
    for (p, e) in x:
        if eval(p, v):
            return eval(e, v)
    raise ValueError("No Branch Evaluates to True")

def sequence(v,*x):
    for e in x[:-1]: eval(e, v)
    return eval(x[-1], v)
 
def wrap(v,p):
    p = eval(p,v)
    return lambda v,*x: p(v,*[eval(expr,v) for expr in x])

global_env = Env({
    '+': lambda v,x,y:eval(x,v) + eval(y,v),
    '-': lambda v,x,y:eval(x,v) - eval(y,v),
    '*': lambda v,x,y:eval(x,v) * eval(y,v),
    '/': lambda v,x,y:eval(x,v) / eval(y,v),
    '>': lambda v,x,y:eval(x,v) > eval(y,v),
    '<': lambda v,x,y:eval(x,v) < eval(y,v),
    '>=': lambda v,x,y:eval(x,v) >= eval(y,v),
    '<=': lambda v,x,y:eval(x,v) <= eval(y,v),
    '=': lambda v,x,y:eval(x,v) == eval(y,v),
    'eq?': lambda v,x,y:
    (lambda vx,vy: (not isa(vx, list)) and (vx == vy))(eval(x,v),eval(y,v)),

    'cons': lambda v,x,y:[eval(x,v)]+eval(y,v),
    'car': lambda v,x:eval(x,v)[0],
    'cdr': lambda v,x:eval(x,v)[1:],
    'list': lambda v,*x:[eval(expr, v) for expr in x],
    'append': lambda v,x,y:eval(x,v)+eval(y,v),
    'len':  lambda v,x:len(eval(x,v)),
    'symbol?': lambda v,x:isa(eval(x,v),Symbol),
    'list?': lambda v,x:isa(eval(x,v),list),
    'atom?': lambda v,x:not isa(eval(x,v), list),
    'True':  True,
    'False': False,
    'if':  lambda v,z,t,f: eval((t if eval(z,v) else f), v),
    'cond':  cond,
    'define': defvar,
    'set!':  setvar,
    'vau':  vau,
    'begin': sequence,
    'eval': lambda v,e,x: eval(eval(x,v),eval(e,v)),
    'wrap': wrap
})
     */
}
