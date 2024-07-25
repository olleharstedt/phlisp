<?php
class SExpressionParser {
    private $tokens;
    private $index;

    public function __construct($input) {
        $this->tokens = $this->tokenize($input);
        $this->index = 0;
    }

    private function tokenize($input) {
        // Split input into tokens (atoms and parentheses)
        return preg_split('/\s+|\(|\)/', $input, -1, PREG_SPLIT_NO_EMPTY);
    }

    public function parse() {
        $stack = new SplStack();

        while ($this->index < count($this->tokens)) {
            $token = $this->tokens[$this->index];

            if ($token === '(') {
                $this->index++;
                $subExpr = $this->parse(); // Recurse for nested expressions
                $stack->push($subExpr);
            } elseif ($token === ')') {
                $this->index++;
                break; // End of current expression
            } else {
                // Handle atoms (numbers or symbols)
                $stack->push($this->parseAtom($token));
            }
        }

        return $stack;
    }

    private function parseAtom($token) {
        // Handle numeric atoms
        if (is_numeric($token)) {
            return (float)$token;
        }

        // Handle other atoms (e.g., symbols)
        return $token;
    }
}

// Example usage
$input = '(+ 2 (- 3 1))';
$parser = new SExpressionParser($input);
$result = $parser->parse();

// Print the result
echo "Parsed S-expression: ";
while (!$result->isEmpty()) {
    echo $result->pop() . ' ';
}
echo "\n";
