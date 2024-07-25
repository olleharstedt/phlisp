<?php

/**
 * Throws exception if token is not allowed.
 */
function validate_token(string $token)
{
    $allowed_words = [
        'ROUND',
        'purchase_price',
        'selling_price'
    ];
    if (ctype_alnum(str_replace(['_'], '', $token))) {
        if ((string) intval($token) === $token) {
            // OK
        } elseif (in_array($token, $allowed_words)) {
            // OK
        } else {
            throw new RuntimeException('Token is not in whitelist: ' . json_encode($token));
        }
    } elseif ($token === ')'
        || $token === '('
        || $token === '-'
        || $token === '+'
        || $token === '/'
        || $token === ''
        || $token === ','
        || $token === '*') {
        // OK
    } else {
        throw new RuntimeException('Invalid token: ' . json_encode($token));
    }
}

$src = 'ROUND((1 - (purchase_price / selling_price)) * 100, 2)';
$tokens = token_get_all('<?php ' . $src);
$sql = '';
foreach ($tokens as $token)
{
    if (is_string($token)) {
        validate_token(trim($token));
        $sql .= trim($token);
    } elseif (is_array($token)) {
        if ($token[1] === '<?php ') {
        } else {
            validate_token(trim($token[1]));
            $sql .= trim($token[1]);
        }
    }
}
echo $sql . "\n";
