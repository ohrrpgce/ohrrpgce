<?php

function not_found(){
?><big>Error 404</big>
<p>
Unfortunately, the page you were looking for does not exist.
Try here instead:
<a href="http://mirror.motherhamster.org/cp/castleparadox.com/search-gamelist.html">http://mirror.motherhamster.org/cp/castleparadox.com/search-gamelist.html</a>
</p><?php
exit;
}

function game_info($game){
  include('id-map.php');
  if(!is_numeric($game)) die(sprintf('%s is not a number. only numeric game ID numbers are allowed!',$game));
  $dest = sprintf('/cp/castleparadox.com/gamelist-display-%d.html',$map[$game]);
  header(sprintf('Location: %s',$dest));
  exit;
}

function download($game){
  include('id-map.php');
  if(!is_numeric($game)) die(sprintf('%s is not a number. only numeric game ID numbers are allowed!',$game));
  $dest = sprintf('/cp/castleparadox.com/%s',$dl[$game]);
  header(sprintf('Location: %s',$dest));
  exit;
}

//----------------------------------------------------------------------------

$url = $_SERVER['REQUEST_URI'];

if(preg_match('/gamelist-display\.php\?game=(\d+)/',$url,$match)) {
  game_info($match[1]);
}

if(preg_match('/game\.php\?mode=edit&game=(\d+)/',$url,$match)) {
  game_info($match[1]);
}

if(preg_match('/download\.php\?game=(\d+)/',$url,$match)) {
  download($match[1]);
}

not_found();

?>
