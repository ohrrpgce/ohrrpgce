<?php
///////////////////////////////////////////////////////////////////////
// Silent spam blocker for WikiMedia. Blocks spam using Wikimedia's
// $wgFilterCallback and then attemtps to deceive the spammer into
// believing that thier edit was posted successfully
//
// USAGE: In LocalSettings.php:
//
//    require_once('mediawiki-spamcallback.php');
//    $wgFilterCallback = spamCallBack;
//
///////////////////////////////////////////////////////////////////////

$basePath = '/var/www/wiki/ohrrpgce/wiki/';
//$basePath = '/home/finalfan/public_html/ohr/wiki/';

$spamNOTEVIL   = $basePath . '../not.evil.txt';
$spamCHONGQED  = $basePath . '../blacklist.chongqed.org.txt';
$spamSPAMWORDS = $basePath . '../blacklist.spamwords.txt';
$spamLOG       = $basePath . 'spammer.log';

///////////////////////////////////////////////////////////////////////
function checkBlackList($filename,$name,$body,&$reason){
  if($handle=fopen($filename,'r')){
    while (!feof($handle)) {
      $regex = trim(fgets($handle));
      if($regex and $regex[0] != '#'){
        if(preg_match('/'.$regex.'/', $body, $match)){
          $reason = sprintf('%s blacklist match: %s',$name,$match[0]);
          fclose($handle);
          return true;
        }
      }
    }
    fclose($handle);
  }
  return false;
}

///////////////////////////////////////////////////////////////////////
function checkWhiteList($filename,$who){
  if($handle=fopen($filename,'r')){
    while (!feof($handle)) {
      $line = trim(fgets($handle));
      if($line and $line[0] != '#'){
        if(strcasecmp($line,$who) == 0){
          fclose($handle);
          return true;
        }
      }
    }
    fclose($handle);
  }
  return false;
}

///////////////////////////////////////////////////////////////////////
function spamCallBack($title, $body, $section){
  global $spamNOTEVIL;
  global $spamCHONGQED;
  global $spamSPAMWORDS;
  global $spamLOG;

  global $wgEmergencyContact;
  global $wgSitename;
  global $wgOut;
  global $wgParser;
  global $wgUser;

  $block = false;
  $do_filter = true;
  $who = $wgUser->mName;

  if(in_array('sysop',$wgUser->mRights)){
    //no filtering for sysops
    $do_filter = false;
  }

  if(checkWhiteList($spamNOTEVIL,$who)){
    $do_filter = false;
  }

  if($do_filter){
	
    //Create a diff, for better filtering  
    $old_page = new Article($title);
    $old = $old_page->fetchContent();
    $diff = getDiff($old,$body);
    $diff = implode("\n",$diff);
    unset($old_page);

    // special handling if there are any external links
    if(preg_match('/https?:\/\//', $diff)){
      // check the chonqed blocklist for spammer urls.
      // the blacklist is downloaded to a text file by a nightly cron job
      $block = checkBlackList($spamCHONGQED,'chongqed',$diff,$reason);
    }
    if (!$block){
      // check the spammy keyword list
      $block = checkBlackList($spamSPAMWORDS,'spammy keyword',$diff,$reason);
    }
    if (!$block){
      // The main page is the most-spammed, and therefore may need extra rules  
      if(!$block and 'Main Page' == $title->mTextform and !$title->mPrefixedText){
        if(preg_match('/http:\/\//',$diff)){
          $reason = 'direct links are forbidden on the main page';
          $block = true;
        }
      }
    }
    if (!$block){
      // special handling for non-logged-in users
      if(preg_match('/\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}/', $who)){
        // no special processing currently enabled
      }
    }
  }

  if($block){
    // log the spam attempt
    $ip = $_SERVER['REMOTE_ADDR'];
    $ip_name = gethostbyaddr($ip);
    $log_name = $who;
    if($who != $ip) $log_name .= " ".$ip;
    if($ip != $ip_name) $log_name .= " ".$ip_name;
    if (is_writable($spamLOG)){
      if($fh = fopen($spamLOG,'a')){
        fwrite($fh, sprintf("%s\t%s\t%s\t%s\n",
                    date('Y-m-d H:i:s'),
                    $log_name,
                    $title->mTextform,
                    $reason));
        fclose($fh);
      }
    }

    // alert the administrator of the spam attempt.
    mail($wgEmergencyContact,
         sprintf('%s %s',$wgSitename,$title->mTextform),
         sprintf("spam attempt blocked from \"%s\"\nReason: %s\n\n%s",
                 $log_name,$reason,$diff),
         sprintf('From: %s',$wgEmergencyContact));

    // attempt to deceive the spammer into thinking their edit succeeded
    $parserOptions = ParserOptions::newFromUser( $wgUser );
    $parserOutput = $wgParser->parse( $body, $title, $parserOptions );
    $deceitHTML = $parserOutput->mText;
    $wgOut->addHTML($deceitHTML);
    $wgOut->addHTML( "<br style=\"clear:both;\" />\n" );
    return true;
  }
  return false;
}

/**
* Get a diff, as an array of changed lines.
* Returns false on error
*/
function getDiff($old, $new) {
  $hash = md5(mt_rand(1,1000000));
  $o = fopen($oldfile = "/tmp/$hash.wiki.old","w");
  $n = fopen($newfile = "/tmp/$hash.wiki.new","w");
  
  fwrite($o,$old);
  fwrite($n,$new);
  
  fclose($o);
  fclose($n);
  
  $res = shell_exec("diff $oldfile $newfile");
  
  unlink($oldfile);
  unlink($newfile);
  
  $line = "";
  $lines = explode("\n",$res);
  
  $diff = array();
  
  for($i=0;$i<count($lines);$i++,$line=$lines[$i]) {
    if($line[0] == ">") {
        $diff[] = substr($line,2);
    }
  }
  return $diff;
}
?>
