rem call zip.bat
cd \backup
givefile wander.zip
echo y|del \temp\*.*
cd \wander 
copy wander.rpg \temp
copy game.exe \temp
copy ohrrpgce.mas \temp
copy ohrrpgce.fnt \temp
copy wh_demo.txt \temp
cd \temp
pkzip wh_demo
givefile wh_demo.*
del wh_demo.*
del wander.rpg
cd \wander
copy custom.exe \temp
copy ohrrpgce.pal \temp
copy ohrrpgce.all \temp
copy ohrrpgce.new \temp
copy custom.txt \temp
copy howto.txt \temp
copy cleanup.bat \temp
cd \temp
copy \backup\sample.rpg c:
copy \backup\npc_tag.rpg c:
md import
pkunzip \backup\import.zip \temp\import
pkzip custom -p -r
givefile custom.zip
givefile custom.txt
echo y|del *.*
echo y|del import\*.*
rmdir import