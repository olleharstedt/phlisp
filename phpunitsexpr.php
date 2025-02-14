<?php

class StringNode
{
    public $value;
    public function __construct($value)
    {
        $this->value = $value;
    }
}

class SexprParser
{
    /**
     * @return SplStack<string>
     */
    public function parse(string $sc)
    {
        // Remove comments
        $sc = preg_replace('/;.*$/m', '', $sc);
        // Normalize string
        $sc = trim((string) preg_replace('/[\t\n\r\s]+/', ' ', $sc));
        $current = new SplStack();
        $base = $current;
        $prev = null;
        $history = new SplStack();
        $buffer = '';
        $inside_string = 0;
        // Build tree structure
        for ($i = 0; $i < strlen($sc); $i++) {
            $char = $sc[$i];
            if ($char === '(') {
                $prev = $current;
                $history->push($current);
                $current = new SplStack();
                $prev->push($current);
            } elseif ($char === ')') {
                if ($buffer) {
                    $current->push($buffer);
                    $buffer = '';
                }
                $current = $history->pop();
            } elseif ($char === '"') {
                $inside_string = 1 - $inside_string;
                if (!$inside_string) {
                    $current->push(new StringNode($buffer));
                    $buffer = '';
                }
            } elseif ($char === ' ' && !$inside_string) {
                if ($buffer !== '') {
                    $current->push($buffer);
                    $buffer = '';
                }
            } else {
                $buffer .= $char;
            }
        } 
        return $base;
    }
}

$code = <<<SCHEME
(load 123 234)
(load "123 234")
SCHEME;

$parser = new SexprParser();
$result = $parser->parse($code);
print_r($result);
