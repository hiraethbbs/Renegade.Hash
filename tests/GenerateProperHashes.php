<?php

$algos = [
  'sha1',
  'sha224',
  'sha256',
  'sha384',
  'sha512',
];
$testWord = 'testing';
$result = [];

foreach($algos as $algo)
{
    $hash = hash($algo, $testWord);
    $result[$algo] = [
      'sha' => (int) trim(filter_var($algo, FILTER_SANITIZE_NUMBER_INT), '+-'),
      'length' => (int)strlen($hash),
      'word' => $testWord,
      'hash' => $hash,
    ];
}

file_put_contents('sha_hashes.json', json_encode($result, JSON_PRETTY_PRINT));