<?php

$db = array(
  'hostname' => 'localhost',
  'username' => 'root',
  'password' => 'root',
  'database' => 'tomkren',
);

// Connect to database
try {
  $dbh = new PDO(
    'mysql:host=' . $db['hostname'] . ';dbname=' . $db['database'], 
    $db['username'], 
    $db['password']);
}
catch(PDOException $e) {
  die($e->getMessage());
}
$dbh -> exec("SET CHARACTER SET utf8");


header('Cache-Control: no-cache, must-revalidate');
//header("Content-Type: application/json; charset=utf-8");
header("Content-Type: text/html; charset=utf-8");

//echo 'hello world';

switch($_SERVER['REQUEST_METHOD']){

  case 'GET':  
    $key = $_GET['key'];
    $q = "SELECT big_object.val AS val FROM `big_object` WHERE big_object.key = '$key'";
    $result = $dbh->query($q);
    print $result->fetchObject()->val;
    break;
  

}