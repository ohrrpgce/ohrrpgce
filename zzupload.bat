cd \wander 
copy wander.rpg \temp
copy game.exe \temp
copy ohrrpgce.mas \temp
copy ohrrpgce.fnt \temp
copy wh_demo.txt \temp
cd \temp
pkzip wh_demo
move wh_demo.zip \wander
echo y|del \temp\*.*
cd \wander
copy custom.exe \temp
copy game.exe \temp
copy ohrrpgce.pal \temp
copy ohrrpgce.fnt \temp
copy ohrrpgce.mas \temp
copy ohrrpgce.all \temp
copy ohrrpgce.new \temp
copy custom.txt \temp
copy howto.txt \temp
copy cleanup.bat \temp
cd \temp
copy d:\backup\sample.rpg .
copy d:\backup\npc_tag.rpg .
md import
pkunzip d:\backup\import.zip \temp\import
pkzip custom -p -r
move custom.zip \wander
echo y|del \temp\*.*
echo y|del \temp\import\*.*
rmdir import
move \wander\wh_demo.zip \temp
move \wander\custom.zip \temp
copy \wander\wh_demo.txt \temp
copy \wander\custom.txt \temp
copy \wander\howto.txt \temp