<?php

//header("Content-Type: text/html; charset=utf-8");



$url = $_GET['url'];
$response = file_get_contents($url);
echo $response;

  

