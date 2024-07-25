<?php

$sc = <<<SCHEME
(report
    (title "Stock report")
    ; Main table to fetch from
    (table  "articles")
    (join 
        (table "categories")
        (type left)
        (on "articles.cat_id" "categories.id")
    )
    ; List of columns to show in the report
    (columns
        (column 
            (title "Art nr")
            (select "articles.article_id")
            (as "id")
        )   
        (column 
            (title "Diff")
            (css "right-align")
            (select (- "articles.selling_price" "articles.purchase_price"))
            (as "diff")
        )
        (column 
            (title "Diff perc")
            (css "right-align")
            ; Inline SQL
            (select (round (* 100 (- 1 (/ purchase_price selling_price))) 2))
            (as "diff_perc")
        )
    )
    ; Sums and totals
    (totals
        (title "Average")
        (total
            (for "diff")
            (do (/ (sum diff) (count rows)))
        )
        (total
            (for "diff_perc")
            (do (/ (sum diff_perc) (count rows)))
        )
    )
)
SCHEME;

abstract class SexprBase
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
        $inside_quote = 0;
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
                $inside_quote = 1 - $inside_quote;
            } elseif ($char === ' ' && !$inside_quote) {
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

class MathSexpr extends SexprBase
{
    /**
     * @param SplStack<mixed>|string $sexpr
     */
    public function mathEval($sexpr): int
    {
        if (is_string($sexpr)) {
            return intval($sexpr);
        }
        $result = 0;
        $op = $sexpr->shift();
        if ($op instanceof SplStack) {
            return $this->mathEval($op);
        }
        switch ($op) {
            case '+':
                $arg1 = $sexpr->shift();
                $arg2 = $sexpr->shift();
                return $this->mathEval($arg1) + $this->mathEval($arg2);
            case '-':
                $arg1 = $sexpr->shift();
                $arg2 = $sexpr->shift();
                return $this->mathEval($arg1) - $this->mathEval($arg2);
            default:
                return intval($op);
        }
    }
}

class ReportSexpr extends SexprBase
{
    /**
     * @param SplStack<mixed> $sexp
     * @return ?SplStack<mixed>
     */
    public function findFirst(SplStack $sexp, string $symbol): ?SplStack
    {
        foreach ($sexp as $s) {
            if ($s instanceof SplStack) {
                if ($s->bottom() === $symbol) {
                    return $s;
                }
            }
        }
        return null;
    }

    /**
     * @param SplStack<mixed> $sexp
     * @return array<mixed>
     */
    public function findAll(SplStack $sexp, string $symbol): array
    {
        $result = [];
        foreach ($sexp as $s) {
            if ($s instanceof SplStack) {
                if ($s->bottom() === $symbol) {
                    $result[] = $s;
                }
            }
        }
        return $result;
    }

    /**
     * @param SplStack<mixed>|string $top
     */
    public function evalSelect($top): string
    {
        $sql = '';
        switch (gettype($top)) {
            case 'int':
                $sql .= $top;
                break;
            case 'string':
                $sql .= $top;
                break;
            case 'object':
                $op = $top->bottom();
                switch ($op) {
                    case '-':
                        $a = $top->pop();
                        $b = $top->pop();
                        $sql .= '(' . $this->evalSelect($b) . ' - ' . $this->evalSelect($a) . ')';
                        break;
                    case '*':
                        $a = $top->pop();
                        $b = $top->pop();
                        $sql .= '(' . $this->evalSelect($b) . ' * ' . $this->evalSelect($a) . ')';
                        break;
                    case '/':
                        $a = $top->pop();
                        $b = $top->pop();
                        $sql .= '(' . $this->evalSelect($b) . ' / ' . $this->evalSelect($a) . ')';
                        break;
                    case 'round':
                        $a = $top->pop();
                        $b = $top->pop();
                        $sql .= 'ROUND('  
                            . $this->evalSelect($b) . ', ' 
                            . $this->evalSelect($a) . ')';
                        break;
                    default:
                        throw new RuntimeException('Unknown op: ' . $op);
                }
                break;
        }
        return $sql;
    }

    /**
     * Recursive method to evaluate a total expression.
     * 
     * Example:
     *   (do (/ (sum diff) (count rows)))
     */
    public function evalTotal(SplStack $sexp, array $data): float
    {
        $op = $sexp->bottom();
        switch ($op) {
            case '/':
                $a = $sexp->pop();
                $b = $sexp->pop();
                return $this->evalTotal($b, $data) / $this->evalTotal($a, $data);
                break;
            case 'sum':
                $variableName = $sexp->pop();
                $sum = 0;
                foreach ($data as $row) {
                    $sum += $row[$variableName];
                }
                return $sum;
                break;
            case 'count':
                $typeOfCount = $sexp->pop();
                if ($typeOfCount === 'rows') {
                    return count($data);
                } else {
                    throw new RuntimeException('Unsupported count type: ' . $typeOfCount);
                }
                break;
            default:
                throw new RuntimeException('Unknown function: ' . $op);
        }
    }

    /**
     * @param SplStack<mixed> $columns
     */
    public function getSelect($columns): string
    {
        $columns = $this->findAll($columns, 'column');
        $sql = '';
        foreach ($columns as $column) {
            $select = $this->findFirst($column, 'select');
            if ($select === null) {
                throw new RuntimeException('Found no select');
            }
            $sql .= $this->evalSelect($select->top());
            $as = $this->findFirst($column, 'as');
            if ($as) {
                $sql .= ' AS ' . $as->top();
            }
            $sql .= ', ';
        }
        return trim(trim($sql), ',');
    }

    /**
     * @param SplStack<mixed> $sexp
     */
    public function getQuery(SplStack $sexp): string
    {
        $report = $this->findFirst($sexp, 'report');
        if ($report === null) {
            throw new RuntimeException('Found no report');
        }
        $table = $this->findFirst($report, 'table');
        if ($table === null) {
            throw new RuntimeException('Found no table');
        }
        $columns = $this->findFirst($report, 'columns');
        if ($columns === null) {
            throw new RuntimeException('Found no columns');
        }
        $select = $this->getSelect($columns);
        $sql  = "SELECT $select FROM {$table->pop()} ";
        return $sql;
    }

    /**
     * @todo Move to SplStack?
     */
    public function getHeaders(SplStack $sexp)
    {
        $headers = [];
        $report = $this->findFirst($sexp, 'report');
        $columns = $this->findFirst($report, 'columns');
        $columns = $this->findAll($columns, 'column');
        foreach ($columns as $column) {
            $title = $this->findFirst($column, 'title');
            $headers[] = $title->top();
        }
        return $headers;
    }

    /**
     *
     */
    public function getTotals(SplStack $sexp, array $data)
    {
        $totals = [];
        $report = $this->findFirst($sexp, 'report');
        if ($report) {
            $totalsNode = $this->findFirst($report, 'totals');
            if ($totalsNode) {
                $totalsNode = $this->findAll($totalsNode, 'total');
                foreach ($totalsNode as $total) {
                    $for = $this->findFirst($total, 'for');
                    if ($for) {
                        $totals[$for->top()] = $this->evalTotal($this->findFirst($total, 'do')->top(), $data['rows']);
                    }
                }
            }
        }
        return $totals;
    }

    public function getTableHeader(array $titles)
    {
        return "<tr>" . array_reduce(
            $titles,
            function($html, $title) {
                return <<<HTML
<th>{$title}</th>
HTML
                . $html;
            }
        ) . "</tr>\n";
    }

    public function getTableRows(SplStack $sexp, array $data)
    {
        $html = '';
        foreach ($data['rows'] as $row) {
            $html .= '<tr>';
            foreach ($row as $cell) {
                $html .= "<td>$cell</td>";
            }
            $html .= "</tr>\n";
        }
        return $html;
    }

    /**
     *
     */
    public function getTotalRows(SplStack $sexp, array $data, array $totals): string
    {
        $html = '';
        $html .= '<tr>';
        foreach ($data['rows'] as $row) {
            foreach ($row as $key => $_) {
                if (isset($totals[$key])) {
                    $html .= "<td>{$totals[$key]}</td>";
                } else {
                    $html .= "<td></td>";
                }
            }
            break;
        }
        $html .= "</tr>\n";
        return $html;
    }

    public function getHtml(SplStack $sexp, array $data): string
    {
        $totals = $this->getTotals($sexp, $data);

        return <<<HTML
<table>
{$this->getTableHeader($this->getHeaders($sexp))}
{$this->getTableRows($sexp, $data)}
{$this->getTotalRows($sexp, $data, $totals)}
</table>
HTML;
    }
}

$report = new ReportSexpr();
$sexp = $report->parse($sc);
echo $report->getQuery($sexp);
echo "\n";
// todo Use query to get data
$data = [
    'rows' => [
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
    ]
];
//print_r($report->getTotals($sexp, $data));
echo "\n";
echo $report->getHtml($sexp, $data);
echo "\n";
