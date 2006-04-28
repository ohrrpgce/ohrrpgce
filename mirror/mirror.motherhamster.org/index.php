<?php

function mirror_info($dir){
  $url = trim(file_get_contents($dir."/url.txt"));
  $date = trim(file_get_contents($dir."/date.txt"));
  printf('mirror of <a href="%s">%s</a><br>'."\n",$url,$url);
  printf('last updated %s'."\n",$date);
}

?>
<html>
<head>
<title>Mirrors at motherhamster.org</title>
<style>
<!--
  p { margin:1em; }
-->
</style>
</head>
<body>
<center>
<table>
<tr>

<td>
<center>
<p>
<a href="cp/">Castle Paradox Game List Mirror<br>
<img src="cp.jpg"><br></a>
<small>
<?php mirror_info("cp") ?>
</small>
</p>
</center>
</td>

<td>
<center>
<p>
<a href="wiki/">OHRRPGCE Documentation Wiki Mirror<br>
<img src="wiki.png"><br></a>
<small>
<?php mirror_info("wiki") ?>
</small>
</p>
</center>
</td>

<td>
<center>
<p>
<a href="tsugumo/">Tsugumo's "So You Want To Be A Pixel Artist?" Mirror<br>
<img src="tsugumo.gif"><br></a>
<small>
mirror of http://pixeltutorial.cjb.net/ (dead link)
</small>
</p>
</center>
</td>

</tr>
</table>
</center>
</body>
</html>
