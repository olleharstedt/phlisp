<?php

$s = <<<FORTH
( Pre-defined helper words )
: set-sql only sql ;
: end-sql only main ;
: set-php only php ;
: end-php only main ;
: compliment 1 swap - ;
: % 100 swap * 2 round ;

var report          \ Create variable report
new table report !  \ Save table data structure to new variable
report @ "Article report" set title
report @ "articles" set table

var joins           \ New variable for SQL joins
new list joins !
var join
new table join !
join @ "categories" set table
join @ "articles.cat_id = categories.id" set on
joins @ join @ push
report @ joins @ set joins
unset joins
unset join

var columns         \ New variable for report columns
new list columns !  \ Create list

var column
new table column !
column @ "Artnr" set title
column @ "article_id" set select
columns @ column @ push

new table column !
column @ "Diff" set title
column @ "diff" set as
column @ set-sql 
    "purchase_price" "selling_price" / compliment %
end-sql set select
columns @ column @ push

report @ columns @ set columns
unset columns
unset column

var rows
run-query rows !
report @ rows @ set rows

var totals
new list totals !

var total
new table total !
total @ "diff" set for
total @ set-php
    rows @ sum diff 
    count rows
    /
end-php set result
totals @ total @ push

new table total !
total @ "diff_perc" set for
total @ set-php
    rows @ sum diff_perc
    count rows
    /
end-php set result

totals @ total @ push
report @ totals @ set totals
unset totals
unset total
FORTH;

class StringBuffer
{
    /** @var string */
    private $buffer;

    /** @var int */
    private $pos = 0;

    private $inside_quote = 0;

    public function __construct(string $s)
    {
        // Remove comments
        $s = preg_replace('/\\\.*$/m', '', $s);
        // Normalize string
        $s = trim((string) preg_replace('/[\t\n\r\s]+/', ' ', $s));
        // Two extra spaces for the while-loop to work
        $this->buffer = $s. '  ';
    }

    public function next()
    {
        if ($this->buffer[$this->pos] === '"') {
            $this->inside_quote = 1 - $this->inside_quote;
        }
        if ($this->buffer[$this->pos] === '(' && $this->inside_quote === 0) {
            $endComment = strpos($this->buffer, ')', $this->pos + 1);
            // TODO: What if result is a new comment?
            $result = substr($this->buffer, $this->pos, $endComment - $this->pos + 1);
            $this->pos = $endComment + 2;
        }
        if ($this->inside_quote === 1) {
            $nextQuote = strpos($this->buffer, '"', $this->pos + 1);
            $result = substr($this->buffer, $this->pos, $nextQuote - $this->pos + 1);
            $this->pos = $nextQuote + 2;
            $this->inside_quote = 0;
        } else {
            $nextSpace = strpos($this->buffer, ' ', $this->pos);
            $result = substr($this->buffer, $this->pos, $nextSpace - $this->pos);
            $this->pos = $nextSpace + 1;
        }
        return $result;
    }
}

/**
 * Main eval loop
 */
function eval_buffer(StringBuffer $buffer, Dicts $dicts): SplStack
{
    $stack  = new SplStack();
    while ($word = $buffer->next()) {
        $fn = $dicts->getWord($word);
        if (trim($word, '"') !== $word) {
            $stack->push($word);
            // Digit
        } elseif (ctype_digit($word)) {
            $stack->push($word);
            // Execute dict word
        } elseif ($fn) {
            $fn($stack, $buffer, $word);
        } else {
            throw new RuntimeException('Word is not a string, not a number, and not in dictionary: ' . $word);
        }
    }
    return $stack;
}

/**
 * Parsing the string buffer populates the environment, which is returned.
 *
 * @return string
 */
function getSelectFromColumns(SplStack $cols)
{
    $sql = '';
    foreach ($cols as $col) {
        $sql .= trim($col['select'], '"') . ', ';
    }
    return trim(trim($sql), ',');
}


class Dict extends ArrayObject
{
    public function addWord(string $word, callable $fn)
    {
        $this[$word] = $fn;
    }

    public function removeWord(string $word)
    {
        unset($this[$word]);
    }
}

class Dicts
{
    public $dicts = [];
    public $currentDict = 'main';

    public function addDict(string $name, Dict $d)
    {
        $this->dicts[$name] = $d;
    }

    public function setCurrent(string $name)
    {
        $this->currentDict = $name;
    }

    public function getCurrentDict()
    {
        return $this->dicts[$this->currentDict];
    }

    /**
     * @return ?string
     */
    public function getWord(string $word)
    {
        $dict = $this->getCurrentDict();
        if (isset($dict[$word])) {
            return $dict[$word];
        }
        $dict = $this->dicts['root'];
        if (isset($dict[$word])) {
            return $dict[$word];
        }
        return null;
    }
}

class Array_ extends ArrayObject
{
    public $name;
}

// Memory to save variables etc in
$mem = new ArrayObject();
$dicts = new Dicts();

// SQL dictionary
$sqlDict = new Dict();
$sqlDict->addWord('/', function($stack, $buffer, $word) {
    $b = $stack->pop();
    $a = $stack->pop();
    $stack->push('(' . $a . ' / ' . $b . ')');
});
$sqlDict->addWord('-', function($stack, $buffer, $word) {
    $b = $stack->pop();
    $a = $stack->pop();
    $stack->push('(' . $a . ' - ' . $b . ')');
});
$sqlDict->addWord('*', function($stack, $buffer, $word) {
    $b = $stack->pop();
    $a = $stack->pop();
    $stack->push('(' . $a . ' * ' . $b . ')');
});
$sqlDict->addWord('round', function($stack, $buffer, $word) {
    $b = $stack->pop();
    $a = $stack->pop();
    $stack->push('round(' . $a . ', ' . $b . ')');
});
$dicts->addDict('sql', $sqlDict);

// PHP dictionary
$phpDict = new Dict();
$phpDict->addWord('count', function ($stack, $buffer, $word) use ($mem) {
    $varName = $buffer->next();
    $var = $mem[$varName];
    $stack->push(count($var));
});
// TODO: Hard-coded variable?
$phpDict->addWord('rows', function ($stack, $buffer, $word) use ($mem) {
    $stack->push($word);
});
$phpDict->addWord('sum', function ($stack, $buffer, $word) use ($mem) {
    $fieldName = $buffer->next();
    $data = $stack->pop();
    $sum = 0;
    foreach ($data as $row) {
        $sum += $row[$fieldName];
    }
    $stack->push($sum);
});
$phpDict->addWord('/', function($stack, $buffer, $word) {
    $a = (float) $stack->pop();
    $b = (float) $stack->pop();
    $stack->push($b / $a);
});
$dicts->addDict('php', $phpDict);

// Root words, available from all dictionaries
$rootDict = new Dict();
$rootDict->addWord('only', function ($stack, $buffer, $word) {
    global $dicts;
    $dictname = $buffer->next();
    $dicts->setCurrent($dictname);
});
$rootDict->addWord('@', function($stack, $buffer, $word) use ($mem) {
    $varname = $stack->pop();
    $stack->push($mem[$varname]);
});
$rootDict->addWord('.', function ($stack, $buffer, $word) {
    $a = $stack->pop();
    echo $a;
});
$rootDict->addWord('drop', function ($stack, $buffer, $word) {
    $stack->pop();
});
$rootDict->addWord('swap', function ($stack, $buffer, $word) use ($sqlDict) {
    $a = $stack->pop();
    $b = $stack->pop();
    $stack->push($a);
    $stack->push($b);
});
$dicts->addDict('root', $rootDict);

// Main, default dictionary
$mainDict = new Dict();
$mainDict->addWord('swap', function ($stack, $buffer, $word) {
    $a = $stack->pop();
    $b = $stack->pop();
    $stack->push($a);
    $stack->push($b);
});
$mainDict->addWord('dup', function ($stack, $buffer, $word) {
    $a = $stack->pop();
    $stack->push(clone $a);
    $stack->push(clone $a);
});

$mainDict->addWord('+', function ($stack, $buffer, $word) {
    $a = $stack->pop();
    $b = $stack->pop();
    $stack->push($a + $b);
});

$mainDict->addWord('run-query', function($stack, $buffer, $word) use ($mem) {
    $report = $mem['report'];
    //var_dump($report);die;
    $select = getSelectFromColumns($report['columns']);
    $table = $report['table'];
    $joins = $report['joins'];
    $sql  = "SELECT $select FROM $table";
    // todo: run query here
    $data = [
        [
            'id' => 1,
            'diff' => 2,
            'diff_perc' => 11
        ],
        [
            'id' => 2,
            'diff' => 4,
            'diff_perc' => 12
        ],
        [
            'id' => 3,
            'diff' => 6,
            'diff_perc' => 13
        ]
    ];
    $stack->push($data);
});
$mainDict->addWord('!', function($stack, $buffer, $word) use ($mem) {
    $name = $stack->pop();
    $value = $stack->pop();
    $mem[$name] = $value;
});
$mainDict->addWord('var', function($stack, $buffer, $word) use ($mem, $mainDict) {
    $varName = $buffer->next();
    $mainDict->addWord($varName, function($stack, $buffer, $word) use ($mem) {
        $stack->push($word);
    });
});

// Remove variable from memory
$mainDict->addWord('unset', function($stack, $buffer, $word) use ($mem, $mainDict) {
    $varName = $buffer->next();
    unset($mem[$varName]);
});

$mainDict->addWord('const', function($stack, $buffer, $word) use ($mainDict) {
    $value = $stack->pop();
    $name = $buffer->next();
    $mainDict->addWord($name, function($stack, $buffer, $word) use ($value) {
        $stack->push($value);
    });
});
$mainDict->addWord('new', function($stack, $buffer, $word) {
    $type = $buffer->next();
    switch ($type) {
        case 'table':
            // Fallthru
        case 'array':
            $stack->push(new ArrayObject());
            break;
        case 'list':
            // Fallthru
        case 'stack':
            $stack->push(new SplStack());
            break;
        default:
            throw new RuntimeException('Unknown type for new: ' . $type);
    }
});
$mainDict->addWord('push', function($stack, $buffer, $word) use ($mainDict) {
    $value = $stack->pop();
    $s = $stack->pop();
    $s->push($value);
});
$mainDict->addWord('pop', function($stack, $buffer, $word) use ($mainDict) {
    $s   = $stack->pop();
    $stack->push($s->pop());
});
$mainDict->addWord('set', function($stack, $buffer, $word) use ($mainDict) {
    $value = $stack->pop();
    $table = $stack->pop();
    $key   = $buffer->next();
    $table[$key] = $value;
});
$rootDict->addWord(':', function ($stack, $buffer, $word) use ($rootDict) {
    $wordsToRun = [];
    while (($w = $buffer->next()) !== ';') {
        $wordsToRun[] = $w;
    }

    $name = $wordsToRun[0];
    unset($wordsToRun[0]);

    $rootDict->addWord($name, function ($stack, $buffer, $_word) use ($wordsToRun) {
        global $dicts;
        $b = new StringBuffer(implode(' ', $wordsToRun));
        while ($word = $b->next()) {
            // TODO: Add support for string
            if (ctype_digit($word)) {
                $stack->push($word);
            } else {
                $fn = $dicts->getWord($word);
                if ($fn) {
                    $fn($stack, $b, $word);
                } else {
                    throw new RuntimeException('Found no word inside : def: ' . $word);
                }
            }
        }
    });
});
$dicts->addDict('main', $mainDict);
$stack = eval_buffer(new StringBuffer($s), $dicts);
print_r($mem['report']);
