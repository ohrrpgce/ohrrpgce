<?php
	for($i = ord('a'); $i <= ord('z'); $i++) {
		exec('java -jar d:\saxon\saxon8.jar -o wiki_' . chr($i) . '.xml plotdict.xml wikiimport.xsl "letter=' . chr($i) . '"');
	}
	exec('java -jar d:\saxon\saxon8.jar -o wiki_index.xml plotdict.xml wikiimport.xsl "letter=!"');
?>
