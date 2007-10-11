<html>
<head>
<title>OHRRPGCE - Archive of Nightly WIP Builds</title>
</head>
<body>
<p><big><b>OHRRPGCE - Archive of Nightly <a href="http://hamsterrepublic.com/ohrrpgce/index.php/What_is_a_WIP_.html">WIP</a> Builds</b></big></p>
<?php

$folder_icon = '/icons/folder.gif';

$today = date('Y-m-d');
$yesterday = date('Y-m-d', time()-(86400));

$list = array();
$d = dir("./");
while (false !== ($entry = $d->read())) {
  $list[] = $entry;
}
rsort($list);
foreach($list as $entry){
  if(preg_match('/^\d{4}-\d{2}-\d{2}$/', $entry)){
    $color = "white";
    if($entry == $today) $color = "lightgreen";
    if($entry == $yesterday) $color = "yellow";
    printf('<div style="float:left;margin:0.25em;background-color:%s;"><img src="%s"><a href="%s">%s</a></div>'."\n"
          , $color
          , $folder_icon
          , urlencode($entry)
          , htmlspecialchars($entry));
  }
}
$d->close();

?>
</body>
</html>
