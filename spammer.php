<?php
/////////////////////////////////////////////////////////////////////////////
// This PHP script displays the spammer-log as a more readable html formatted
// table. See http://gilgamesh.hamsterrepublic.com/wiki/ohrrpgce/spammer.php

$LOG = 'spammer.log';

$fh = fopen($LOG, 'r');
if(!$fh) die('cannot open '.$LOG);

echo '<table border="1">'."\n";
while($line = fgets($fh)){
  $broken = explode("\t", $line);
  echo '<tr>';
  $date = $broken[0];
  $ip = $broken[1];
  $page = $broken[2];
  $reason = $broken[3];
  foreach(array($date,$ip,$page,$reason) as $field){
    printf('<td>%s</td>', $field);
  }
  echo '</tr>';
}
echo '</table>'."\n";

fclose($fh);

?>
