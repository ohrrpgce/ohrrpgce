cd %OHRRPGCE%

del tmp\*.???

copy game.exe tmp
copy custom.exe tmp
copy hspeak.exe tmp
copy unlump.exe tmp
copy ohrrpgce.mas tmp
copy ohrrpgce.fnt tmp
copy ohrrpgce.new tmp
copy custom.txt tmp

ECHO unfinished
EXIT

copy LICENSE-binary.txt tmp

del distrib\pushpush.zip

cd tmp
..\support\pkzip ..\distrib\pushpush.zip *.*
