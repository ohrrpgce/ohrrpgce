<?php

function ohr_page_top($title){
?>
<html>
<head>
<title>OHRRPGCE - <?php echo($title); ?></title>
<?php include('headers.php'); ?>
</head>
<body>
<?php
include('nav.php');
}

function ohr_page_bottom(){
?>
<center>
<a href="/">
 <img src="/img/b_hrhp.gif" alt="[Back to the Hamster Republic Homepage]" width="260" height="30" border="0"><br>
</a>
</center>
</body>
</html>
<?php
}

?>
