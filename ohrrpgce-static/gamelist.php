<?php

$FILE_ROOT='/w2/h/www.hamsterrepublic.com/html';
$PAGE_ROOT='http://HamsterRepublic.com';
$SCREENSHOT_DIR=$FILE_ROOT.'/ohrrpgce/gamelist-images';
$SCREENSHOT_URL=$PAGE_ROOT.'/ohrrpgce/gamelist-images';
$NEW_ENTRY_URL='https://secure.cyberverse.com/h/hamsterrepublic/identity/client-linklist.php';
$DEFAULT_PAGE_SIZE=8;

include($FILE_ROOT.'/../data/db.php');

$PATH_INFO=$_SERVER['PATH_INFO'];
$THIS_SCRIPT=basename($_SERVER['SCRIPT_NAME']);

//--------------------------------------------------------------------

function start_box($class, $percent=90){
  printf('<div style="float:left;width:%s%%;margin:1em;" class="shaded%s%s"><b>'."\n",$percent,($class?'-':''),$class);
}

function end_box(){
  echo("</b></div>\n");
}

function display_page_links($offset,$limit,$total){
  global $THIS_SCRIPT;
  if($total<=$limit) return;
  printf("<tt>\n");
  if($offset>0){
    printf('<a href="%s?offset=%d">&lt;&lt;</a> ',$THIS_SCRIPT,$offset-$limit);
  }else{
    printf('&nbsp;&nbsp; ');
  }
  for($i=0;$i<$total;$i+=$limit){
    if($i==$offset){
      printf('<big>%d</big> ',$i/$limit+1);
    }else{
      printf('<a href="%s?offset=%d">%d</a> ',$THIS_SCRIPT,$i,$i/$limit+1);
    }
    $last=$i;
  }
  if($offset<$last){
    printf('<a href="%s?offset=%d">&gt;&gt;</a> ',$THIS_SCRIPT,$offset+$limit);
  }else{
    printf('&nbsp;&nbsp; ');
  }
  printf("<br>\n");
  printf("</tt>\n");
}

//--------------------------------------------------------------------

/* only allow valid sort types */
$sortdir='ASC';
switch ($_REQUEST['sort']){
  case 'title':
  case 'entryupdated':
       $sort=$_REQUEST['sort'];
       break;
  default:
       $sort='entryupdated';
}
if ($sort=='entryupdated') $sortdir='DESC';

/* only allow numeric offset and limit */
$offset=$_REQUEST['offset'];
if(!is_numeric($offset)) $offset=0;
$limit=$_REQUEST['limit'];
if(!is_numeric($limit)) $limit=$DEFAULT_PAGE_SIZE;

if($sql=mysql_pconnect($SQL_SERVER,$SQL_USER,$SQL_PW)){
if(mysql_select_db($SQL_DB)){

/*---Begin Output-------------------*/

  include('common.php');
  ohr_page_top('Game List');

  /* make type default to rpg */
  if (!$type) $type='rpg';

    //---have selected a specific category---
    if($type=='rpg'){
      //--selected a valid category--
      //--do a full query to calculate the list-size--
      $query=sprintf('select record from linklist');
      if($result=mysql_query($query)){
        $total=mysql_num_rows($result);
        start_box('header',95);
        display_page_links($offset,$limit,$total);
        end_box();
      }else{
        echo('listing size query failed');
      }
      //--show the list
      $query=sprintf('select * from linklist order by %s %s limit %s,%s',$sort,$sortdir,$offset,$limit);
      if($result=mysql_query($query)){
        $imgtog=0;
        while ($row = mysql_fetch_array($result)) {
          if ($tog) {$tog='';} else {$tog='alt';}
          start_box($tog);
          foreach($row as $key => $value){
            $row[$key]=stripslashes($row[$key]);
          }
          if($row['screenshot']){
            if(file_exists($SCREENSHOT_DIR.'/'.$row['screenshot'])){
              printf('<a href="%s/%s"><img src="%s/t_%s" alt="[screenshot]" class="img%s"></a>'."\n",$SCREENSHOT_URL,$row['screenshot'],$SCREENSHOT_URL,$row['screenshot'],$imgtog);
              if($imgtog==0) $imgtog=1; else $imgtog=0;
            }
          }
          printf('<b>%s</b><br>'."\n",$row['title']);
          if($row['url']){
            printf('<tt>Download..</tt>'."\n");
            $urllist=explode("\n",$row['url']);
            foreach($urllist as $thisurl){
              if (substr($thisurl,0,7)=='http://' or substr($thisurl,0,8)=='https://' or substr($thisurl,0,6)=='ftp://'){
                printf('[<a href="%s">%s</a>] '."\n",$thisurl,$thisurl);
              }
            }
            echo('<br>'."\n");
          }
          if($row['email'] || $row['author']){
            if($row['email']){
              if($row['author']){
                printf('<tt>Author....</tt><a href="mailto:%s">%s</a><br>'."\n",$row['email'],$row['author']);
              }else{//no author
                printf('<tt>Author....</tt><a href="mailto:%s">%s</a><br>'."\n",$row['email'],$row['email']);
              }
            }else{ //no email
              printf('<tt>Author....</tt>%s<br>'."\n",$row['author']);
            }
          }
          if($row['homepage']){
            printf('<tt>Homepage..</tt><a href="%s">%s</a><br>'."\n",$row['homepage'],$row['homepage']);
          }
          if($row['description']){
            printf('%s<br>'."\n",substr($row['description'],0,500));
          }
          end_box();
        }
        start_box('header',95);
        display_page_links($offset,$limit,$total);
        /* Link for adding and editing entries */
        printf('[<a href="%s">Add &amp; Edit Games</a>]'."\n",$NEW_ENTRY_URL);
        end_box();
      }else{
        echo('listing query failed');
      }
    }else{
      //--not a valid category
      printf('%s is not a valid category!<br>'."\n",strtoupper($type));
    }//--end valid category check
  flush();

?>

</body>
</html>

<?php

}else{echo('cannot open database');}
}else{echo('cannot connect to database');}

?>
