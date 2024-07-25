<?php
class LispInterpreter {
    private $env;

    public function __construct() {
        $this->env = $this->defaultEnv();
    }

    private function defaultEnv() {
        return [
            '+' => function ($args) { return array_sum($args); },
            '-' => function ($args) { return $args[0] - $args[1]; },
            '*' => function ($args) { return array_product($args); },
            '/' => function ($args) { return $args[0] / $args[1]; },
            'def' => function ($args) {
                $this->env[$args[0]] = $this->eval($args[1]);
                return $this->env[$args[0]];
            },
            'macro' => function ($args) {
                $name = array_shift($args);
                $params = array_shift($args);
                $body = array_shift($args);
                $this->env[$name] = ['macro', $params, $body];
            }
        ];
    }

    public function parse($code) {
        $code = preg_replace('/[\s,]*([\(\)])[,\s]*/', ' $1 ', $code);
        $tokens = array_filter(explode(' ', $code), function($token) {
            return $token !== '';
        });
        return $this->readFromTokens($tokens);
    }

    private function readFromTokens(&$tokens) {
        if (count($tokens) === 0) {
            throw new Exception("Unexpected EOF");
        }
        $token = array_shift($tokens);
        if ($token === '(') {
            $list = [];
            while ($tokens[0] !== ')') {
                $list[] = $this->readFromTokens($tokens);
            }
            array_shift($tokens);
            return $list;
        } elseif ($token === ')') {
            throw new Exception("Unexpected )");
        } else {
            return $this->atom($token);
        }
    }

    private function atom($token) {
        if (is_numeric($token)) {
            return intval($token);
        } else {
            return $token;
        }
    }

    public function eval($x) {
        if (is_string($x)) {
            return $this->env[$x];
        } elseif (!is_array($x)) {
            return $x;
        }

        $op = $x[0];
        $args = array_slice($x, 1);

        if ($op === 'quote') {
            return $args[0];
        } elseif ($op === 'if') {
            $test = $args[0];
            $conseq = $args[1];
            $alt = $args[2];
            $exp = $this->eval($test) ? $conseq : $alt;
            return $this->eval($exp);
        } elseif ($op === 'def') {
            return $this->env['def']($args);
        } elseif ($op === 'macro') {
            return $this->env['macro']($args);
        } elseif (isset($this->env[$op]) && is_callable($this->env[$op])) {
            $values = array_map([$this, 'eval'], $args);
            return call_user_func($this->env[$op], $values);
        } elseif (isset($this->env[$op]) && is_array($this->env[$op]) && $this->env[$op][0] === 'macro') {
            list($_, $params, $body) = $this->env[$op];
            $expansion = $this->macroExpand($params, $args, $body);
            return $this->eval($expansion);
        } else {
            throw new Exception("Unknown operator: $op");
        }
    }

    private function macroExpand($params, $args, $body) {
        $env = [];
        foreach ($params as $index => $param) {
            $env[$param] = $args[$index];
        }
        return $this->substitute($body, $env);
    }

    private function substitute($body, $env) {
        if (is_string($body) && isset($env[$body])) {
            return $env[$body];
        } elseif (is_array($body)) {
            return array_map(function($elem) use ($env) {
                return $this->substitute($elem, $env);
            }, $body);
        } else {
            return $body;
        }
    }
}

// Example usage
$interpreter = new LispInterpreter();
$code = '(def square (macro (x) (list "* x x"))) (square 5)';
$parsed = $interpreter->parse($code);
$result = $interpreter->eval($parsed);
var_dump($result);
