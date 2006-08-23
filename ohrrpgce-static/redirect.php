<?php

function redirect($url){
  header ('HTTP/1.1 301 Moved Permanently');
  header(sprintf('Location: %s', $url));
  exit;
}

function wiki($old_page,$wiki_page){
  return sprintf('%s /ohrrpgce/index.php/%s.html', $old_page, $wiki_page);
}

$r = array();
$r[] = 'buglist.php http://gilgamesh.hamsterrepublic.com/cgi-bin/bugzilla/buglist.cgi?query_format=specific&order=relevance+desc&bug_status=__open__&product=&content=';
$r[] = wiki('news.php', 'News');
$r[] = wiki('info.php', 'Project_Info');
$r[] = wiki('download.php', 'Downloads');
$r[] = wiki('docs.php', 'Documentation');
$r[] = wiki('community.php', 'Community');
$r[] = wiki('source.php', 'Source');
$r[] = wiki('others.php', 'Alternatives');
$r[] = wiki('contact.php', 'Forums');
$r[] = wiki('faq.php', 'FAQ');
$r[] = wiki('howto.php', 'HOWTO');
$r[] = wiki('ircinfo.php', 'IRC_Chat');
$r[] = wiki('miscfiles.php', 'Add-ons');
$r[] = wiki('usefulprogs.php', 'Other_Useful_Programs');
$r[] = wiki('wheel.php', 'What_is_HamsterWheel_');

$where = basename($_SERVER['REQUEST_URI']);

foreach($r as $redir){
  $broken = explode(' ',$redir);
  $from = $broken[0];
  $to = $broken[1];
  if($from == $where) redirect($to);
}

redirect('http://HamsterRepublic.com/ohrrpgce/');

?>
