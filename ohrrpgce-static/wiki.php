<?php

 $path = '/'.urlencode(substr($_SERVER['PATH_INFO'],1));
 header(sprintf('Location: http://gilgamesh.HamsterRepublic.com/wiki/ohrrpgce/index.php%s',$path));

?>
