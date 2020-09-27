<?php

// Stuff that is special for this wiki

$plotdict = 'http://hamsterrepublic.com/ohrrpgce/docs/plotdict.xml';

$req = $_SERVER['REQUEST_URI'];
if(preg_match('/Plot:([^&]*)/', $req, $matches)){
  $cmd = $matches[1];
  $cmd = strtolower($cmd);
  $cmd = str_ireplace("_", "", $cmd);

  if($cmd == 'index'){
    $url = $plotdict;
  }else{
    $url = $plotdict . "#about-" . $cmd;
  }

  header("HTTP/1.1 301 Moved Permanently");
  header("Location: ".$url);
  exit();
}

?>
