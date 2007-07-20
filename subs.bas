'OHRRPGCE CUSTOM - Misc unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions

#include "udts.bi"

DECLARE FUNCTION str2lng& (stri$)
DECLARE FUNCTION str2int% (stri$)
DECLARE FUNCTION readshopname$ (shopnum%)
DECLARE FUNCTION filenum$ (n%)
DECLARE SUB standardmenu (menu$(), size%, vis%, pt%, top%, x%, y%, page%, edge%)
DECLARE FUNCTION charpicker$ ()
DECLARE FUNCTION readenemyname$ (index%)
DECLARE FUNCTION readitemname$ (index%)
DECLARE SUB clearallpages ()
DECLARE SUB enforceflexbounds (menuoff%(), menutype%(), menulimits%(), recbuf%(), min%(), max%())
DECLARE FUNCTION editflexmenu% (nowindex%, menutype%(), menuoff%(), menulimits%(), datablock%(), mintable%(), maxtable%())
DECLARE SUB updateflexmenu (mpointer%, nowmenu$(), nowdat%(), size%, menu$(), menutype%(), menuoff%(), menulimits%(), datablock%(), caption$(), maxtable%(), recindex%)
DECLARE SUB setactivemenu (workmenu%(), newmenu%(), pt%, top%, size%)
DECLARE SUB addcaption (caption$(), indexer%, cap$)
DECLARE SUB testflexmenu ()
DECLARE FUNCTION readattackname$ (index%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE FUNCTION pal16browse% (curpal%, usepic%, picx%, picy%, picw%, pich%, picpage%)
DECLARE FUNCTION xintgrabber% (n%, pmin%, pmax%, nmin%, nmax%, less%, more%)
DECLARE FUNCTION zintgrabber% (n%, min%, max%, less%, more%)
DECLARE FUNCTION intgrabber (n%, min%, max%, less%, more%)
DECLARE SUB strgrabber (s$, maxl%)
DECLARE FUNCTION needaddset (pt%, check%, what$)
DECLARE SUB cropafter (index%, limit%, flushafter%, lump$, bytes%, prompt%)
DECLARE SUB herotags (hero as HeroDef ptr)
DECLARE SUB cycletile (cycle%(), tastuf%(), pt%(), skip%())
DECLARE SUB testanimpattern (tastuf%(), taset%)
DECLARE FUNCTION onoroff$ (n%)
DECLARE FUNCTION lmnemonic$ (index%)
DECLARE FUNCTION rotascii$ (s$, o%)
DECLARE SUB editbitset (array%(), wof%, last%, names$())
DECLARE SUB formation ()
DECLARE SUB enemydata ()
DECLARE SUB herodata ()
DECLARE SUB attackdata ()
DECLARE SUB getnames (stat$(), max%)
DECLARE SUB statname ()
DECLARE FUNCTION sublist% (num%, s$())
DECLARE SUB maptile (font%())
DECLARE FUNCTION itemstr$ (it%, hiden%, offbyone%)
DECLARE FUNCTION getsongname$ (num%)
DECLARE FUNCTION getsfxname$ (num%)
DECLARE FUNCTION isStringField(mnu%)
DECLARE FUNCTION scriptbrowse$ (trigger%, triggertype%, scrtype$)
DECLARE FUNCTION scrintgrabber (n%, BYVAL min%, BYVAL max%, BYVAL less%, BYVAL more%, scriptside%, triggertype%)

#include "compat.bi"
#include "allmodex.bi"
#include "common.bi"
#include "cglobals.bi"

#include "const.bi"
#include "scrconst.bi"

REM $STATIC
FUNCTION charpicker$

STATIC pt

DIM f(255)

last = -1
FOR i = 32 TO 255
 last = last + 1
 f(last) = i
NEXT i

linesize = 16
xoff = 160 - (linesize * 9) \ 2
yoff = 100 - ((last \ linesize) * 9) \ 2

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN charpicker$ = "": EXIT DO

 IF keyval(72) > 1 THEN pt = large(pt - linesize, 0)
 IF keyval(80) > 1 THEN pt = small(pt + linesize, last)
 IF keyval(75) > 1 THEN pt = large(pt - 1, 0)
 IF keyval(77) > 1 THEN pt = small(pt + 1, last)

 IF keyval(28) > 1 OR keyval(57) > 1 THEN charpicker$ = CHR$(f(pt)): EXIT DO

 FOR i = 0 TO last
  textcolor 7, 8
  IF (i MOD linesize) = (pt MOD linesize) OR (i \ linesize) = (pt \ linesize) THEN textcolor 7, 1
  IF pt = i THEN textcolor 14 + tog, 0
  printstr CHR$(f(i)), xoff + (i MOD linesize) * 9, yoff + (i \ linesize) * 9, dpage
 NEXT i

 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

END FUNCTION

SUB clearallpages

FOR i = 0 TO 3
 clearpage i
NEXT i

END SUB

SUB enemydata

'--stat names
DIM names$(32), nof(11), elemtype$(2)
elemtype$(0) = readglobalstring$(127, "Weak to", 10)
elemtype$(1) = readglobalstring$(128, "Strong to", 10)
elemtype$(2) = readglobalstring$(129, "Absorbs ", 10)
getnames names$(), 32
'--name offsets
nof(0) = 0
nof(1) = 1
nof(2) = 2
nof(3) = 3
nof(4) = 5
nof(5) = 6
nof(6) = 29
nof(7) = 30
nof(8) = 8
nof(9) = 7
nof(10) = 31
nof(11) = 4

'--preview stuff
DIM workpal(7), previewsize(2)
previewsize(0) = 34
previewsize(1) = 50
previewsize(2) = 80

clearallpages
rectangle 219, 99, 82, 82, 7, 3
rectangle 220, 100, 80, 80, 8, 3

'-------------------------------------------------------------------------

'--bitsets
DIM ebit$(61)

FOR i = 0 TO 7
 ebit$(0 + i) = elemtype$(0) + " " + names$(17 + i)
 ebit$(8 + i) = elemtype$(1) + " " + names$(17 + i)
 ebit$(16 + i) = elemtype$(2) + " " + names$(17 + i)
 ebit$(24 + i) = "Is " + names$(9 + i)
NEXT i
FOR i = 32 TO 53
 ebit$(i) = "" 'preferable to be blank, so we can hide it
NEXT i
ebit$(54) = "Harmed by Cure"
ebit$(55) = "MP Idiot"
ebit$(56) = "Boss"
ebit$(57) = "Unescapable"
ebit$(58) = "Die Without Boss"
ebit$(59) = "Flee instead of Die"
ebit$(60) = "Untargetable by Enemies"
ebit$(61) = "Untargetable by Heros"

'-------------------------------------------------------------------------

'--record buffer
DIM recbuf(160)

CONST EnDatName = 0' to 16
CONST EnDatStealAvail = 17
CONST EnDatStealItem = 18
CONST EnDatStealItemP = 19
CONST EnDatStealRItem = 20
CONST EnDatStealRItemP = 21
'22 to 52 unused
CONST EnDatPic = 53
CONST EnDatPal = 54
CONST EnDatPicSize = 55
CONST EnDatGold = 56
CONST EnDatExp = 57
CONST EnDatItem = 58
CONST EnDatItemP = 59
CONST EnDatRareItem = 60
CONST EnDatRareItemP = 61
CONST EnDatStat = 62' to 73
CONST EnDatBitset = 74' to 78
CONST EnDatSpawnDeath = 79
CONST EnDatSpawnNEDeath = 80
CONST EnDatSpawnAlone = 81
CONST EnDatSpawnNEHit = 82
CONST EnDatSpawnElement = 83' to 90
CONST EnDatSpawnNum = 91
CONST EnDatAtkNormal = 92' to 96
CONST EnDatAtkDesp = 97'   to 101
CONST EnDatAtkAlone = 102' to 106
'107 to 114 unimplemented elemental triggers
'115 to 158 unused

'-------------------------------------------------------------------------

capindex = 0
DIM caption$(5)
DIM max(23), min(23)
'Limit 0 is not used

CONST EnLimPic = 1
max(EnLimPic) = gen(27) 'or 28 or 29. Must be updated!

CONST EnLimUInt = 2
max(EnLimUInt) = 32767

CONST EnLimPicSize = 3
max(EnLimPicSize) = 2
EnCapPicSize = capindex
addcaption caption$(), capindex, "Small 34x34"
addcaption caption$(), capindex, "Medium 50x50"
addcaption caption$(), capindex, "Big 80x80"

CONST EnLimItem = 4
max(EnLimItem) = 254

CONST EnLimPercent = 5
max(EnLimPercent) = 100

CONST EnLimStat = 6' to 17
FOR i = 0 TO 1:  max(EnLimStat + i) = 32767: NEXT i ' HP and MP
FOR i = 2 TO 8:  max(EnLimStat + i) = 999:   NEXT i ' regular stats
FOR i = 9 TO 10: max(EnLimStat + i) = 100:   NEXT i ' focus, counter
max(EnLimStat + 11) = 10        ' max hits

CONST EnLimSpawn = 18
max(EnLimSpawn) = gen(36) + 1 'must be updated!

CONST EnLimSpawnNum = 19
max(EnLimSpawnNum) = 8

CONST EnLimAtk = 20
max(EnLimAtk) = gen(34) + 1

CONST EnLimStr16 = 21
max(EnLimStr16) = 16

CONST EnLimStealAvail = 22
min(EnLimStealAvail) = -1
max(EnLimStealAvail) = 1
addcaption caption$(), capindex, "Disabled"
EnCapStealAvail = capindex
addcaption caption$(), capindex, "Only one"
addcaption caption$(), capindex, "Unlimited"

CONST EnLimPal16 = 23
max(EnLimPal16) = 32767
min(EnLimPal16) = -1

'--next limit 24, remeber to update dim!

'-------------------------------------------------------------------------
'--menu content
DIM menu$(66), menutype(66), menuoff(66), menulimits(66)

CONST EnMenuBackAct = 0
menu$(EnMenuBackAct) = "Previous Menu"
menutype(EnMenuBackAct) = 1

CONST EnMenuChooseAct = 1
menu$(EnMenuChooseAct) = "Enemy"
menutype(EnMenuChooseAct) = 5

CONST EnMenuName = 2
menu$(EnMenuName) = "Name:"
menutype(EnMenuName) = 4
menuoff(EnMenuName) = EnDatName
menulimits(EnMenuName) = EnLimStr16

CONST EnMenuAppearAct = 3
menu$(EnMenuAppearAct) = "Appearance..."
menutype(EnMenuAppearAct) = 1

CONST EnMenuRewardAct = 4
menu$(EnMenuRewardAct) = "Rewards..."
menutype(EnMenuRewardAct) = 1

CONST EnMenuStatAct = 5
menu$(EnMenuStatAct) = "Stats..."
menutype(EnMenuStatAct) = 1

CONST EnMenuBitsetAct = 6
menu$(EnMenuBitsetAct) = "Bitsets..."
menutype(EnMenuBitsetAct) = 1

CONST EnMenuSpawnAct = 7
menu$(EnMenuSpawnAct) = "Spawning..."
menutype(EnMenuSpawnAct) = 1

CONST EnMenuAtkAct = 8
menu$(EnMenuAtkAct) = "Attacks..."
menutype(EnMenuAtkAct) = 1

CONST EnMenuPic = 9
menu$(EnMenuPic) = "Picture:"
menutype(EnMenuPic) = 0
menuoff(EnMenuPic) = EnDatPic
menulimits(EnMenuPic) = EnLimPic

CONST EnMenuPal = 10
menu$(EnMenuPal) = "Palette:"
menutype(EnMenuPal) = 12
menuoff(EnMenuPal) = EnDatPal
menulimits(EnMenuPal) = EnLimPal16

CONST EnMenuPicSize = 11
menu$(EnMenuPicSize) = "Picture Size:"
menutype(EnMenuPicSize) = 2000 + EnCapPicSize
menuoff(EnMenuPicSize) = EnDatPicSize
menulimits(EnMenuPicSize) = EnLimPicSize

CONST EnMenuGold = 12
menu$(EnMenuGold) = "Gold:"
menutype(EnMenuGold) = 0
menuoff(EnMenuGold) = EnDatGold
menulimits(EnMenuGold) = EnLimUInt

CONST EnMenuExp = 13
menu$(EnMenuExp) = "Experience Points:"
menutype(EnMenuExp) = 0
menuoff(EnMenuExp) = EnDatExp
menulimits(EnMenuExp) = EnLimUInt

CONST EnMenuItem = 14
menu$(EnMenuItem) = "Item:"
menutype(EnMenuItem) = 8
menuoff(EnMenuItem) = EnDatItem
menulimits(EnMenuItem) = EnLimItem

CONST EnMenuItemP = 15
menu$(EnMenuItemP) = "Item%:"
menutype(EnMenuItemP) = 0
menuoff(EnMenuItemP) = EnDatItemP
menulimits(EnMenuItemP) = EnLimPercent

CONST EnMenuRareItem = 16
menu$(EnMenuRareItem) = "Rare Item:"
menutype(EnMenuRareItem) = 8
menuoff(EnMenuRareItem) = EnDatRareItem
menulimits(EnMenuRareItem) = EnLimItem

CONST EnMenuRareItemP = 17
menu$(EnMenuRareItemP) = "Rare Item%:"
menutype(EnMenuRareItemP) = 0
menuoff(EnMenuRareItemP) = EnDatRareItemP
menulimits(EnMenuRareItemP) = EnLimPercent

CONST EnMenuStat = 18' to 29
FOR i = 0 TO 11
 menu$(EnMenuStat + i) = names$(nof(i)) + ":"
 menutype(EnMenuStat + i) = 0
 menuoff(EnMenuStat + i) = EnDatStat + i
 menulimits(EnMenuStat + i) = EnLimStat + i
NEXT i

CONST EnMenuSpawnDeath = 30
menu$(EnMenuSpawnDeath) = "Spawn on Death:"
menutype(EnMenuSpawnDeath) = 9
menuoff(EnMenuSpawnDeath) = EnDatSpawnDeath
menulimits(EnMenuSpawnDeath) = EnLimSpawn

CONST EnMenuSpawnNEDeath = 31
menu$(EnMenuSpawnNEDeath) = "on Non-Elemental Death:"
menutype(EnMenuSpawnNEDeath) = 9
menuoff(EnMenuSpawnNEDeath) = EnDatSpawnNEDeath
menulimits(EnMenuSpawnNEDeath) = EnLimSpawn

CONST EnMenuSpawnAlone = 32
menu$(EnMenuSpawnAlone) = "Spawn When Alone:"
menutype(EnMenuSpawnAlone) = 9
menuoff(EnMenuSpawnAlone) = EnDatSpawnAlone
menulimits(EnMenuSpawnAlone) = EnLimSpawn

CONST EnMenuSpawnNEHit = 33
menu$(EnMenuSpawnNEHit) = "on Non-Elemental Hit:"
menutype(EnMenuSpawnNEHit) = 9
menuoff(EnMenuSpawnNEHit) = EnDatSpawnNEHit
menulimits(EnMenuSpawnNEHit) = EnLimSpawn

CONST EnMenuSpawnElement = 34' to 41
FOR i = 0 TO 7
 menu$(EnMenuSpawnElement + i) = "on " + names$(17 + i) + " Hit:"
 menutype(EnMenuSpawnElement + i) = 9
 menuoff(EnMenuSpawnElement + i) = EnDatSpawnElement + i
 menulimits(EnMenuSpawnElement + i) = EnLimSpawn
NEXT i

CONST EnMenuSpawnNum = 42
menu$(EnMenuSpawnNum) = "How Many to Spawn:"
menutype(EnMenuSpawnNum) = 0
menuoff(EnMenuSpawnNum) = EnDatSpawnNum
menulimits(EnMenuSpawnNum) = EnLimSpawnNum

CONST EnMenuAtkNormal = 43' to 47
FOR i = 0 TO 4
 menu$(EnMenuAtkNormal + i) = "Normal:"
 menutype(EnMenuAtkNormal + i) = 7
 menuoff(EnMenuAtkNormal + i) = EnDatAtkNormal + i
 menulimits(EnMenuAtkNormal + i) = EnLimAtk
NEXT i

CONST EnMenuAtkDesp = 48' to 52
FOR i = 0 TO 4
 menu$(EnMenuAtkDesp + i) = "Desperation:"
 menutype(EnMenuAtkDesp + i) = 7
 menuoff(EnMenuAtkDesp + i) = EnDatAtkDesp + i
 menulimits(EnMenuAtkDesp + i) = EnLimAtk
NEXT i

CONST EnMenuAtkAlone = 53' to 57
FOR i = 0 TO 4
 menu$(EnMenuAtkAlone + i) = "Alone:"
 menutype(EnMenuAtkAlone + i) = 7
 menuoff(EnMenuAtkAlone + i) = EnDatAtkAlone + i
 menulimits(EnMenuAtkAlone + i) = EnLimAtk
NEXT i

CONST EnMenuStealItem = 58
menu$(EnMenuStealItem) = "Stealable Item:"
menutype(EnMenuStealItem) = 8
menuoff(EnMenuStealItem) = EnDatStealItem
menulimits(EnMenuStealItem) = EnLimItem

CONST EnMenuStealRItem = 59
menu$(EnMenuStealRItem) = "Rare Stealable Item:"
menutype(EnMenuStealRItem) = 8
menuoff(EnMenuStealRItem) = EnDatStealRItem
menulimits(EnMenuStealRItem) = EnLimItem

CONST EnMenuStealItemP = 60
menu$(EnMenuStealItemP) = "Steal Rate%:"
menutype(EnMenuStealItemP) = 0
menuoff(EnMenuStealItemP) = EnDatStealItemP
menulimits(EnMenuStealItemP) = EnLimPercent

CONST EnMenuStealRItemP = 61
menu$(EnMenuStealRItemP) = "Rare Steal Rate%:"
menutype(EnMenuStealRItemP) = 0
menuoff(EnMenuStealRItemP) = EnDatStealRItemP
menulimits(EnMenuStealRItemP) = EnLimPercent

CONST EnMenuStealAvail = 62
menu$(EnMenuStealAvail) = "Steal Availability:"
menutype(EnMenuStealAvail) = 2000 + EnCapStealAvail
menuoff(EnMenuStealAvail) = EnDatStealAvail
menulimits(EnMenuStealAvail) = EnLimStealAvail

'-------------------------------------------------------------------------
'--menu structure
DIM workmenu(15), dispmenu$(15)
pt = 0: top = 0: size = 0

DIM mainMenu(8)
mainMenu(0) = EnMenuBackAct
mainMenu(1) = EnMenuChooseAct
mainMenu(2) = EnMenuName
mainMenu(3) = EnMenuAppearAct
mainMenu(4) = EnMenuRewardAct
mainMenu(5) = EnMenuStatAct
mainMenu(6) = EnMenuBitsetAct
mainMenu(7) = EnMenuSpawnAct
mainMenu(8) = EnMenuAtkAct

DIM appearMenu(3)
appearMenu(0) = EnMenuBackAct
appearMenu(1) = EnMenuPicSize
appearMenu(2) = EnMenuPic
appearMenu(3) = EnMenuPal

DIM rewardMenu(11)
rewardMenu(0) = EnMenuBackAct
rewardMenu(1) = EnMenuGold
rewardMenu(2) = EnMenuExp
rewardMenu(3) = EnMenuItem
rewardMenu(4) = EnMenuItemP
rewardMenu(5) = EnMenuRareItem
rewardMenu(6) = EnMenuRareItemP
rewardMenu(7) = EnMenuStealAvail
rewardMenu(8) = EnMenuStealItem
rewardMenu(9) = EnMenuStealItemP
rewardMenu(10) = EnMenuStealRItem
rewardMenu(11) = EnMenuStealRItemP

DIM statMenu(12)
statMenu(0) = EnMenuBackAct
FOR i = 0 TO 11
 statMenu(1 + i) = EnMenuStat + i
NEXT i

DIM spawnMenu(13)
spawnMenu(0) = EnMenuBackAct
spawnMenu(1) = EnMenuSpawnNum
spawnMenu(2) = EnMenuSpawnDeath
spawnMenu(3) = EnMenuSpawnNEDeath
spawnMenu(4) = EnMenuSpawnAlone
spawnMenu(5) = EnMenuSpawnNEHit
FOR i = 0 TO 7
 spawnMenu(6 + i) = EnMenuSpawnElement + i
NEXT i

DIM atkMenu(15)
atkMenu(0) = EnMenuBackAct
FOR i = 0 TO 4
 atkMenu(1 + i) = EnMenuAtkNormal + i
 atkMenu(6 + i) = EnMenuAtkDesp + i
 atkMenu(11 + i) = EnMenuAtkAlone + i
NEXT i

'--default starting menu
setactivemenu workmenu(), mainMenu(), pt, top, size

menudepth = 0
lastptr = 0
lasttop = 0
recindex = 0

'load data here
GOSUB EnLoadSub

'------------------------------------------------------------------------
'--main loop

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN     'ESC
  IF menudepth = 1 THEN
   GOSUB EnBackSub
  ELSE
   EXIT DO
  END IF
 END IF

 '--CTRL+BACKSPACE
 IF keyval(29) > 0 AND keyval(14) THEN
  cropafter recindex, gen(36), 0, game$ + ".dt1", 320, 1
 END IF

 dummy = usemenu(pt, top, 0, size, 22)

 IF workmenu(pt) = EnMenuChooseAct OR (keyval(56) > 0 and NOT isStringField(menutype(workmenu(pt)))) THEN
  lastindex = recindex
  IF keyval(77) > 1 AND recindex = gen(36) AND recindex < 32767 THEN
   '--attempt to add a new set
   '--save current
   saveenemydata recbuf(), lastindex
   '--increment
   recindex = recindex + 1
   '--make sure we really have permission to increment
   IF needaddset(recindex, gen(genMaxEnemy), "enemy") THEN
    flusharray recbuf(), 159, 0
    recbuf(54) = -1 'default palette
    GOSUB EnUpdateMenu
   END IF
  ELSE
   IF intgrabber(recindex, 0, gen(36), 75, 77) THEN
    saveenemydata recbuf(), lastindex
    GOSUB EnLoadSub
   END IF
  END IF
 END IF

 IF keyval(28) > 1 OR keyval(57) > 1 THEN
  SELECT CASE workmenu(pt)
   CASE EnMenuBackAct
    IF menudepth = 1 THEN
     GOSUB EnBackSub
    ELSE
     EXIT DO
    END IF
   CASE EnMenuAppearAct
    GOSUB EnPushPtrSub
    setactivemenu workmenu(), appearMenu(), pt, top, size
    GOSUB EnUpdateMenu
   CASE EnMenuRewardAct
    GOSUB EnPushPtrSub
    setactivemenu workmenu(), rewardMenu(), pt, top, size
    GOSUB EnUpdateMenu
   CASE EnMenuStatAct
    GOSUB EnPushPtrSub
    setactivemenu workmenu(), statMenu(), pt, top, size
    GOSUB EnUpdateMenu
   CASE EnMenuSpawnAct
    GOSUB EnPushPtrSub
    setactivemenu workmenu(), spawnMenu(), pt, top, size
    GOSUB EnUpdateMenu
   CASE EnMenuAtkAct
    GOSUB EnPushPtrSub
    setactivemenu workmenu(), atkMenu(), pt, top, size
    GOSUB EnUpdateMenu
   CASE EnMenuPal
    recbuf(EnDatPal) = pal16browse(recbuf(EnDatPal), 1, 0, 0, previewsize(recbuf(EnDatPicSize)), previewsize(recbuf(EnDatPicSize)), 2)
    GOSUB EnUpdateMenu
   CASE EnMenuBitsetAct
    editbitset recbuf(), EnDatBitset, 61, ebit$()
  END SELECT
 END IF

 IF keyval(56) = 0 or isStringField(menutype(workmenu(pt))) THEN 'not pressing ALT, or not allowed to
  IF editflexmenu(workmenu(pt), menutype(), menuoff(), menulimits(), recbuf(), min(), max()) THEN
   GOSUB EnUpdateMenu
  END IF
 END IF

 GOSUB EnPreviewSub

 standardmenu dispmenu$(), size, 22, pt, top, 0, 0, dpage, 0
 IF keyval(56) > 0 THEN 'holding ALT
  tmp$ = readbadbinstring$(recbuf(), EnDatName, 15, 0) + XSTR$(recindex)
  textcolor 15, 1
  printstr tmp$, 320 - LEN(tmp$) * 8, 0, dpage
 END IF

 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP

'--save what we were last working on
saveenemydata recbuf(), recindex

clearallpages

EXIT SUB

'-----------------------------------------------------------------------

EnUpdateMenu:

'--in case new enemies have been added
max(EnLimSpawn) = gen(36) + 1

'--in case the PicSize has changed
max(EnLimPic) = gen(27 + bound(recbuf(EnDatPicSize), 0, 2))

'--re-enforce bounds, as they might have just changed
enforceflexbounds menuoff(), menutype(), menulimits(), recbuf(), min(), max()

updateflexmenu pt, dispmenu$(), workmenu(), size, menu$(), menutype(), menuoff(), menulimits(), recbuf(), caption$(), max(), recindex

'--load the picture and palette
setpicstuf buffer(), (previewsize(recbuf(EnDatPicSize)) ^ 2) / 2, 2
loadset game$ + ".pt" + STR$(1 + recbuf(EnDatPicSize)), recbuf(EnDatPic), 0
getpal16 workpal(), 0, recbuf(EnDatPal), 1 + recbuf(EnDatPicSize), recbuf(EnDatPic)

RETRACE

'-----------------------------------------------------------------------

EnPreviewSub:
loadsprite buffer(), 0, 0, 0, previewsize(recbuf(EnDatPicSize)), previewsize(recbuf(EnDatPicSize)), 2
wardsprite buffer(), 0, workpal(), 0, 260 - previewsize(recbuf(EnDatPicSize)) / 2, 180 - previewsize(recbuf(EnDatPicSize)), dpage
RETRACE

'-----------------------------------------------------------------------

EnBackSub:
setactivemenu workmenu(), mainMenu(), pt, top, size
menudepth = 0
pt = lastptr
top = lasttop
GOSUB EnUpdateMenu
RETRACE

'-----------------------------------------------------------------------

EnPushPtrSub:
lastptr = pt
lasttop = top
menudepth = 1
RETRACE

'-----------------------------------------------------------------------

EnLoadSub:
loadenemydata recbuf(), recindex
GOSUB EnUpdateMenu
RETRACE

'-----------------------------------------------------------------------
END SUB

SUB formation
DIM a(40), b(160), c(24), s(7), w(7), h(7), menu$(10), ename$(7), max(10), min(10), z(7), bmenu$(22), pal16(64)
clearpage 0
clearpage 1
clearpage 2
clearpage 3
setdiskpages buffer(), 200, 0
csr = 0: bcsr = 0

menu$(0) = "Return to Main Menu"
menu$(1) = "Edit Individual Formations..."
menu$(2) = "Construct Formation Sets..."
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 dummy = usemenu(csr, 0, 0, 2, 24)
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF csr = 0 THEN EXIT DO
  IF csr = 1 THEN GOSUB editform
  IF csr = 2 THEN GOSUB formsets
 END IF

 standardmenu menu$(), 2, 22, csr, 0, 0, 0, dpage, 0

 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
clearpage 0
clearpage 1
clearpage 2
clearpage 3
EXIT SUB

formsets:
bmenu$(0) = "Previous Menu"
pt = 0
GOSUB loadfset
GOSUB lpreviewform
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN
  GOSUB savefset
  RETRACE
 END IF
 IF usemenu(bcsr, 0, 0, 22, 24) THEN GOSUB lpreviewform
 IF keyval(28) > 1 OR keyval(57) > 1 THEN
  IF bcsr = 0 THEN
   GOSUB savefset
   RETRACE
  END IF
 END IF
 IF bcsr = 1 THEN
  IF keyval(75) > 1 THEN
   GOSUB savefset
   gptr = large(gptr - 1, 0)
   GOSUB loadfset
  END IF
  IF keyval(77) > 1 THEN
   GOSUB savefset
   gptr = small(gptr + 1, 255)
   GOSUB loadfset
  END IF
 END IF
 IF bcsr = 2 THEN dummy = intgrabber(c(0), 0, 99, 75, 77)
 IF bcsr > 2 THEN
  IF zintgrabber(c(bcsr - 2), -1, gen(37), 75, 77) THEN
   GOSUB lpreviewform
  END IF
  IF pt >= 0 THEN
   '--preview form
   GOSUB formsprite
  END IF
 END IF
 bmenu$(1) = CHR$(27) + "Formation Set" + XSTR$(gptr + 1) + CHR$(26)
 bmenu$(2) = "Battle Frequency:" + XSTR$(c(0))
 FOR i = 3 TO 22
  bmenu$(i) = "Formation" + XSTR$(c(i - 2) - 1)
  IF c(i - 2) = 0 THEN bmenu$(i) = "Empty"
 NEXT i

 standardmenu bmenu$(), 22, 22, bcsr, 0, 0, 0, dpage, 1

 SWAP vpage, dpage
 setvispage vpage
 IF bcsr > 2 AND pt >= 0 THEN
  copypage 2, dpage
 ELSE
  clearpage dpage
 END IF
 dowait
LOOP

lpreviewform:
IF bcsr > 2 THEN
 '--have form selected
 pt = c(bcsr - 2) - 1
 IF pt >= 0 THEN
  '--form not empty
  GOSUB loadform
  GOSUB formpics
 END IF
END IF
RETRACE

savefset:
setpicstuf c(), 50, -1
storeset game$ + ".efs", gptr, 0
RETRACE

loadfset:
setpicstuf c(), 50, -1
loadset game$ + ".efs", gptr, 0
RETRACE

editform:
'--???  well, you see..
max(1) = gen(genMaxBackdrop) - 1   'genMaxBackdrop is number of backdrops, but is necessary
max(2) = gen(genMaxSong) + 1   'genMaxSongs is number of last song, but is optional
max(3) = 50
max(4) = 1000
pt = 0: csr2 = -6: csr3 = 0
GOSUB loadform
GOSUB formpics
setkeys

menu$(3) = "Previous Menu"

DO
 setwait timing(), 90
 setkeys
 tog = tog XOR 1
 flash = flash + 1: IF flash > 14 THEN flash = -13
 IF csr3 = 1 THEN
  '--enemy positioning mode
  IF keyval(1) > 1 OR keyval(57) > 1 OR keyval(28) > 1 THEN setkeys: csr3 = 0
  movpix = 1 + (7 * SGN(keyval(56)))
  IF keyval(72) > 0 AND a(csr2 * 4 + 2) > 0 THEN a(csr2 * 4 + 2) = a(csr2 * 4 + 2) - movpix
  IF keyval(80) > 0 AND a(csr2 * 4 + 2) < 199 - w(csr2) THEN a(csr2 * 4 + 2) = a(csr2 * 4 + 2) + movpix
  IF keyval(75) > 0 AND a(csr2 * 4 + 1) > 0 THEN a(csr2 * 4 + 1) = a(csr2 * 4 + 1) - movpix
  IF keyval(77) > 0 AND a(csr2 * 4 + 1) < 250 - w(csr2) THEN a(csr2 * 4 + 1) = a(csr2 * 4 + 1) + movpix
 END IF
 IF csr3 = 0 THEN
  '--menu mode
  IF keyval(1) > 1 THEN
   GOSUB saveform
   RETRACE
  END IF
  IF keyval(29) > 0 AND keyval(14) THEN cropafter pt, gen(37), 0, game$ + ".for", 80, 1
  dummy = usemenu(csr2, -6, -6, 7, 25)
  IF keyval(57) > 1 OR keyval(28) > 1 THEN
   IF csr2 = -6 THEN
    GOSUB saveform
    RETRACE
   END IF
   IF csr2 >= 0 THEN IF a(csr2 * 4 + 0) > 0 THEN csr3 = 1
  END IF
  IF (csr2 >= -5 AND csr2 <= -4) OR (csr2 >= -1 AND csr2 < 0) THEN
   IF intgrabber(a(36 + csr2), 0, max(csr2 + 5), 75, 77) THEN
    GOSUB saveform
    GOSUB loadform
   END IF
  END IF
  IF csr2 = -3 THEN
   IF zintgrabber(a(36 + csr2), -2, max(csr2 + 5), 75, 77) THEN
    GOSUB saveform
    GOSUB loadform
   END IF
  END IF
  IF csr2 = -2 THEN
   IF xintgrabber(a(36 + csr2), 2, max(csr2 + 5), 0, 0, 75, 77) THEN
    GOSUB saveform
    GOSUB loadform
   END IF
  END IF
  IF csr2 = -5 THEN '---SELECT A DIFFERENT FORMATION
   remptr = pt
   IF intgrabber(pt, 0, gen(37), 51, 52) THEN
    SWAP pt, remptr
    GOSUB saveform
    SWAP pt, remptr
    GOSUB loadform
    GOSUB formpics
   END IF
   IF keyval(75) > 1 AND pt > 0 THEN
    GOSUB saveform
    pt = large(pt - 1, 0)
    GOSUB loadform
    GOSUB formpics
   END IF
   IF keyval(77) > 1 AND pt < 32767 THEN
    GOSUB saveform
    pt = pt + 1
    IF needaddset(pt, gen(37), "formation") THEN GOSUB clearformation
    GOSUB loadform
    GOSUB formpics
   END IF
  END IF'--DONE SELECTING DIFFERENT FORMATION
  IF csr2 >= 0 THEN
   IF zintgrabber(a(csr2 * 4 + 0), -1, gen(36), 75, 77) THEN
    GOSUB formpics
   END IF
  END IF
 END IF
 GOSUB formsprite
 FOR i = 0 TO 3
  rectangle 240 + i * 8, 75 + i * 22, 32, 40, 18 + i * 2, dpage
 NEXT i
 IF csr3 = 0 THEN
  menu$(4) = CHR$(27) + "formation" + XSTR$(pt) + CHR$(26)
  menu$(5) = "Backdrop screen:" + XSTR$(a(32))
  menu$(6) = "Battle Music:"
  IF a(33) = -1 THEN
    menu$(6) = menu$(6) + " -same music as map-"
  ELSEIF a(33) = 0 THEN
    menu$(6) = menu$(6) + " -silence-"
  ELSEIF a(33) > 0 THEN
    menu$(6) = menu$(6) + XSTR$(a(33) - 1) + " " + getsongname$(a(33) - 1)
  END IF
  menu$(7) = "Backdrop Frames:"
  IF a(34) = 0 THEN menu$(7) = menu$(7) + " no animation" ELSE menu$(7) = menu$(7) + XSTR$(a(34) + 1)
  menu$(8) = "Backdrop Speed:" + XSTR$(a(35))
  FOR i = 0 TO 5
   col = 7: IF csr2 + 6 = i THEN col = 14 + tog
   edgeprint menu$(i + 3), 1, 1 + (i * 10), col, dpage
  NEXT i
  FOR i = 0 TO 7
   col = 7: IF csr2 = i THEN col = 14 + tog
   edgeprint "Enemy:" + ename$(i), 1, 61 + (i * 10), col, dpage
  NEXT i
 END IF
 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP

formsprite:
FOR i = 0 TO 7
 z(i) = i
NEXT i
FOR o = 1 TO 7
 insertval = z(o)
 searchval = a(insertval * 4 + 2) + h(insertval)
 FOR i = o - 1 TO 0 STEP -1
  IF searchval < a(z(i) * 4 + 2) + h(z(i)) THEN
   z(i + 1) = z(i)
  ELSE
   EXIT FOR
  END IF
 NEXT
 z(i + 1) = insertval
NEXT
FOR i = 0 TO 7
 IF a(z(i) * 4 + 0) > 0 THEN
  loadsprite buffer(), 0, 0, z(i) * 10, w(z(i)), w(z(i)), 3
  drawsprite buffer(), 0, pal16(), 16 * z(i), a(z(i) * 4 + 1), a(z(i) * 4 + 2), dpage
  IF csr2 = z(i) THEN textcolor 176 + ABS(flash), 0: printstr CHR$(25), a(z(i) * 4 + 1) + (w(z(i)) * .5) - 4, a(z(i) * 4 + 2), dpage
 END IF
NEXT i
RETRACE

clearformation:
FOR i = 0 TO 40
 a(i) = 0
NEXT i
a(33) = gen(4)
setpicstuf a(), 80, -1
storeset game$ + ".for", pt, 0
RETRACE

saveform:
setpicstuf a(), 80, -1
storeset game$ + ".for", pt, 0
RETRACE

loadform:
setpicstuf a(), 80, -1
loadset game$ + ".for", pt, 0
loadpage game$ + ".mxs", a(32), 2
RETRACE

formpics:
FOR i = 0 TO 7
 ename$(i) = "-EMPTY-"
 IF a(i * 4 + 0) > 0 THEN
  loadenemydata b(), a(i * 4 + 0) - 1
  ename$(i) = STR$(a(i * 4 + 0) - 1) + ":"
  FOR o = 1 TO b(0)
   ename$(i) = ename$(i) + CHR$(b(o))
  NEXT o
  getpal16 pal16(), i, b(54), 1 + b(55), b(53)
  IF b(55) = 0 THEN s(i) = 578: w(i) = 34: h(i) = 34: f$ = ".pt1"
  IF b(55) = 1 THEN s(i) = 1250: w(i) = 50: h(i) = 50: f$ = ".pt2"
  IF b(55) = 2 THEN s(i) = 3200: w(i) = 80: h(i) = 80: f$ = ".pt3"
  setpicstuf buffer(), s(i), 3
  loadset game$ + f$, b(53), i * 10
 END IF
NEXT i
RETRACE

END SUB

SUB herodata
DIM names$(100), menu$(8), bmenu$(40), max(40), min(40), nof(12), attack$(24), b(40), opt$(10), hbit$(-1 TO 25), hmenu$(4), pal16(16), elemtype$(2)
DIM AS HeroDef her, blankhero
wd = 1: wc = 0: wx = 0: wy = 0: hmax = 32
leftkey = 0: rightkey = 0
nof(0) = 0: nof(1) = 1: nof(2) = 2: nof(3) = 3: nof(4) = 5: nof(5) = 6: nof(6) = 29: nof(7) = 30: nof(8) = 8: nof(9) = 7: nof(10) = 31: nof(11) = 4
clearpage 0
clearpage 1
clearpage 2
clearpage 3
getnames names$(), hmax
elemtype$(0) = readglobalstring$(127, "Weak to", 10)
elemtype$(1) = readglobalstring$(128, "Strong to", 10)
elemtype$(2) = readglobalstring$(129, "Absorbs ", 10)
frame = -1

pt = 0
csr = 1
FOR i = 0 TO 7
 hbit$(i) = elemtype$(0) + " " + names$(17 + i)
 hbit$(i + 8) = elemtype$(1) + " " + names$(17 + i)
 hbit$(i + 16) = elemtype$(2) + " " + names$(17 + i)
NEXT i
hbit$(24) = "Rename when added to party"
hbit$(25) = "Permit renaming on status screen"

menu$(0) = "Return to Main Menu"
menu$(1) = CHR$(27) + "Pick Hero" + XSTR$(pt) + CHR$(26)
menu$(2) = "Name:"
menu$(3) = "Appearance and Misc..."
menu$(4) = "Edit Stats..."
menu$(5) = "Edit Spell Lists..."
menu$(6) = "Name Spell Lists..."
menu$(7) = "Bitsets..."
menu$(8) = "Hero Tags..."
nam$ = ""
GOSUB thishero

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 GOSUB movesmall
 IF keyval(1) > 1 THEN EXIT DO
 IF keyval(29) > 0 AND keyval(14) THEN
  cropafter pt, gen(35), -1, game$ + ".dt0", 636, 1
 END IF
 dummy = usemenu(csr, 0, 0, 8, 24)
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF csr = 0 THEN EXIT DO
  IF csr = 3 THEN GOSUB picnpal
  IF csr = 4 THEN GOSUB levstats
  IF csr = 5 THEN GOSUB speltypes '--spell list contents
  IF csr = 6 THEN GOSUB heromenu '--spell list names
  IF csr = 7 THEN editbitset her.bits(), 0, 25, hbit$()
  IF csr = 8 THEN herotags @her
 END IF
 IF csr = 1 THEN
  remptr = pt
  IF intgrabber(pt, 0, gen(35), 51, 52) THEN
   SWAP pt, remptr
   GOSUB lasthero
   SWAP pt, remptr
   GOSUB thishero
  END IF
  IF keyval(75) > 1 AND pt > 0 THEN
   GOSUB lasthero
   pt = pt - 1
   GOSUB thishero
  END IF
  IF keyval(77) > 1 AND pt < 59 THEN
   GOSUB lasthero
   pt = pt + 1
   IF needaddset(pt, gen(35), "hero") THEN GOSUB clearhero
   GOSUB thishero
  END IF
 END IF
 IF csr = 2 THEN
  strgrabber nam$, 16
  menu$(2) = "Name:" + nam$
 END IF

 standardmenu menu$(), 8, 22, csr, 0, 0, 0, dpage, 0

 GOSUB heropreview
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
GOSUB lasthero
clearpage 0
clearpage 1
clearpage 2
clearpage 3
EXIT SUB

heromenu:
bmenu$(0) = "Previous Menu": bctr = 0
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETRACE
 IF (keyval(57) > 1 OR keyval(28) > 1) AND bctr = 0 THEN RETRACE
 dummy = usemenu(bctr, 0, 0, 4, 24)
 IF bctr > 0 THEN
  strgrabber hmenu$(bctr - 1), 10
 END IF
 bmenu$(1) = "Spell List 1:" + hmenu$(0)
 bmenu$(2) = "Spell List 2:" + hmenu$(1)
 bmenu$(3) = "Spell List 3:" + hmenu$(2)
 bmenu$(4) = "Spell List 4:" + hmenu$(3)

 standardmenu bmenu$(), 4, 22, bctr, 0, 0, 0, dpage, 0

 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

speltypes:
FOR i = 0 TO 3
 IF her.list_type(i) > 10 OR her.list_type(i) < 0 THEN her.list_type(i) = 0
NEXT i
bctr = -1
opt$(0) = "Spells (MP Based)"
opt$(1) = "Spells (FF1 Style)"
opt$(2) = "Random Effects"
opt$(3) = "Item Consuming (not implemented)"
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETRACE
 dummy = usemenu(bctr, -1, -1, 3, 24)
 IF bctr >= 0 THEN dummy = intgrabber(her.list_type(bctr), 0, 2, 75, 77)
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF bctr = -1 THEN RETRACE
  IF bctr >= 0 AND bctr < 4 THEN
   listnum = bctr
   GOSUB spells
   bctr = listnum
  END IF
 END IF
 textcolor 7, 0: IF bctr = -1 THEN textcolor 14 + tog, 0
 printstr "Previous Menu", 0, 0, dpage
 FOR i = 0 TO 3
  textcolor 7, 0: IF bctr = i THEN textcolor 14 + tog, 0
  printstr "Type" + XSTR$(i) + " Spells:" + opt$(her.list_type(i)), 0, 8 + i * 8, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

picnpal:
bctr = 0
bmenu$(0) = "Previous Menu"
min(1) = 0: max(1) = gen(genMaxHeroPic)
min(2) = -1: max(2) = 32767
min(3) = 0: max(3) = gen(genMaxNPCPic)
min(4) = -1: max(4) = 32767
min(5) = -1: max(5) = 99
min(6) = 0: max(6) = 254
min(7) = 0: max(7) = 16
min(8) = -100:max(8) = 100
min(9) = -100:max(9) = 100
it$ = itemstr(her.def_weapon, 0, 1)
setkeys
frame = 0
DO
 GOSUB genheromenu
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 GOSUB movesmall
 IF keyval(1) > 1 THEN frame = -1: RETRACE
 IF (keyval(52) > 1 AND frame = 0) OR (keyval(51) > 1 AND frame = 1) THEN
  frame = frame xor 1
 END IF
 dummy = usemenu(bctr, 0, 0, 9, 24)
 IF (keyval(28) > 1 OR keyval(57) > 1) AND bctr = 0 THEN frame = -1: RETRACE
 IF bctr > 0 THEN
  SELECT CASE bctr
   CASE 1
    IF intgrabber(her.sprite, min(bctr), max(bctr), 75, 77) THEN
      GOSUB heropics
    END IF
   CASE 2
    IF intgrabber(her.sprite_pal, min(bctr), max(bctr), 75, 77) THEN
      GOSUB heropics
    END IF
   CASE 3
    IF intgrabber(her.walk_sprite, min(bctr), max(bctr), 75, 77) THEN
      GOSUB heropics
    END IF
   CASE 4
    IF intgrabber(her.walk_sprite_pal, min(bctr), max(bctr), 75, 77) THEN
      GOSUB heropics
    END IF
   CASE 5
    dummy = intgrabber(her.def_level, min(bctr), max(bctr), 75, 77)
   CASE 6
    IF intgrabber(her.def_weapon, min(bctr), max(bctr), 75, 77) THEN
      it$ = itemstr$(her.def_weapon, 0, 1)
    END IF
   CASE 7
    dummy = intgrabber(her.max_name_len, min(bctr), max(bctr), 75, 77)
   CASE 8
    IF frame = 0 THEN
      dummy = intgrabber(her.hand_a_x, min(bctr), max(bctr), 75, 77)
    ELSE
      dummy = intgrabber(her.hand_b_x, min(bctr), max(bctr), 75, 77)
    END IF
   CASE 9
    IF frame = 0 THEN
      dummy = intgrabber(her.hand_a_y, min(bctr), max(bctr), 75, 77)
    ELSE
      dummy = intgrabber(her.hand_b_y, min(bctr), max(bctr), 75, 77)
    END IF
  END SELECT
  IF keyval(28) > 1 OR keyval(57) > 1 THEN
   IF bctr = 2 THEN
    her.sprite_pal = pal16browse(her.sprite_pal, 8, 0, 0, 32, 40, 2)
   ELSEIF bctr = 4 THEN
    her.walk_sprite_pal = pal16browse(her.walk_sprite_pal, 8, 0, 16, 20, 20, 2)
   END IF
   GOSUB heropics
  END IF
 END IF

 standardmenu bmenu$(), 9, 22, bctr, 0, 8, 0, dpage, 0

 GOSUB heropreview
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

heropreview:
IF frame <> -1 THEN
 loadsprite buffer(), 0, 640 * (frame + 2), 0, 32, 40, 2
ELSE
 loadsprite buffer(), 0, 640 * tog, 0, 32, 40, 2
END IF
drawsprite buffer(), 0, pal16(), 0, 250, 25, dpage
loadsprite buffer(), 0, (wd * 400) + (200 * tog), 16, 20, 20, 2
drawsprite buffer(), 0, pal16(), 16, 230 + wx, 5 + wy, dpage
IF frame <> -1 THEN
 IF frame = 0 THEN
  handx = her.hand_a_x
  handy = her.hand_a_y
 ELSE
  handx = her.hand_b_x
  handy = her.hand_b_y
 END IF
 drawline 248 + handx,25 + handy,249 + handx, 25 + handy,14 + tog,dpage
 drawline 250 + handx,23 + handy,250 + handx, 24 + handy,14 + tog,dpage
 drawline 251 + handx,25 + handy,252 + handx, 25 + handy,14 + tog,dpage
 drawline 250 + handx,26 + handy,250 + handx, 27 + handy,14 + tog,dpage
 printstr XSTR$(frame),256,18,dpage
 IF frame = 1 THEN printstr "<",256,18,dpage
 IF frame = 0 THEN printstr ">",272,18,dpage
END IF

RETRACE

genheromenu:
bmenu$(1) = "Battle Picture:" + XSTR$(her.sprite)
bmenu$(2) = "Battle Palette:" + defaultint$(her.sprite_pal)
bmenu$(3) = "Walkabout Picture:" + XSTR$(her.walk_sprite)
bmenu$(4) = "Walkabout Palette:" + defaultint$(her.walk_sprite_pal)
bmenu$(5) = "Base Level:" + XSTR$(her.def_level)
IF her.def_level < 0 THEN bmenu$(5) = "Base Level: Party Average"
bmenu$(6) = "Default Weapon:" + it$
bmenu$(7) = "Max Name Length:"
IF her.max_name_len THEN
 bmenu$(7) = bmenu$(7) + XSTR$(her.max_name_len)
ELSE
 bmenu$(7) = bmenu$(7) + " default"
END IF
IF frame = 0 THEN
 bmenu$(8) = "Hand X:" + XSTR$(her.hand_a_x)
 bmenu$(9) = "Hand Y:" + XSTR$(her.hand_a_y)
ELSEIF frame = 1 THEN
 bmenu$(8) = "Hand X:" + XSTR$(her.hand_b_x)
 bmenu$(9) = "Hand Y:" + XSTR$(her.hand_b_y)
END IF
RETRACE

levstats:
bctr = 0
bmenu$(0) = "Previous Menu"
FOR i = 1 TO 24: min(i) = 0: max(i) = 999: NEXT
FOR i = 1 TO 4: max(i) = 9999: NEXT i
FOR i = 19 TO 22: max(i) = 100: NEXT i
FOR i = 23 TO 24: max(i) = 10: NEXT i
GOSUB smi
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETRACE
 IF keyval(72) > 1 THEN bctr = large(bctr - 2, 0)
 IF keyval(80) > 1 AND bctr > 0 THEN bctr = small(bctr + 2, 24)
 IF keyval(80) > 1 AND bctr = 0 THEN bctr = bctr + 1
 IF keyval(75) > 1 AND bctr > 0 THEN bctr = bctr - 1
 IF keyval(77) > 1 AND bctr < 24 THEN bctr = bctr + 1
 IF (keyval(28) > 1 OR keyval(57) > 1) AND bctr = 0 THEN RETRACE
 IF bctr > 0 THEN
  changed = 0
  IF (bctr AND 1) = 1 THEN ' odd numbers are level 0
   IF intgrabber(her.Lev0.sta((bctr - 1) \ 2), min(bctr), max(bctr), 51, 52) THEN changed = -1
  ELSE' even numbers are level 99
   IF intgrabber(her.Lev99.sta((bctr - 2) \ 2), min(bctr), max(bctr), 51, 52) THEN changed = -1
  END IF
  IF changed THEN GOSUB smi
 END IF
 textcolor 7, 0
 IF 0 = bctr THEN textcolor 14 + tog, 0
 printstr bmenu$(0), 8, 0, dpage
 textcolor 11, 0
 printstr "LEVEL ZERO", 8, 12, dpage
 printstr "LEVEL NINETY-NINE", 160, 12, dpage
 FOR i = 0 TO 11
  textcolor 7, 0
  IF 1 + i * 2 = bctr THEN textcolor 14 + tog, 0
  printstr bmenu$(1 + i * 2), 8, 20 + i * 8, dpage
  textcolor 7, 0
  IF 2 + i * 2 = bctr THEN textcolor 14 + tog, 0
  printstr bmenu$(2 + i * 2), 160, 20 + i * 8, dpage
 NEXT i
 IF bctr > 0 THEN GOSUB graph
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

spells:
bctr = 0
colcsr = 0
sticky = 0
GOSUB setsticky
FOR o = 1 TO 24
 GOSUB gosubatkname
NEXT o
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF sticky THEN
  IF keyval(1) > 1 THEN sticky = 0: GOSUB setsticky
 ELSE
  IF keyval(1) > 1 THEN RETRACE
  IF usemenu(bctr, 0, 0, 24, 24) THEN
   IF her.spell_lists(listnum, bctr-1).attack = 0 THEN colcsr = 0
  END IF
  IF keyval(75) > 1 OR keyval(77) > 1 THEN
   colcsr = colcsr XOR 1
   IF her.spell_lists(listnum, bctr-1).attack = 0 THEN colcsr = 0
  END IF
 END IF
 IF bctr > 0 THEN
  IF colcsr = 0 THEN
   IF zintgrabber(her.spell_lists(listnum, bctr-1).attack, -1, gen(genMaxAttack), leftkey, rightkey) THEN
    o = bctr
    GOSUB gosubatkname
   END IF
  END IF
  IF colcsr = 1 THEN dummy = zintgrabber(her.spell_lists(listnum, bctr-1).learned, -1, 99, leftkey, rightkey)
 END IF
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF bctr = 0 THEN
   '--exit menu
   RETRACE
  ELSE
   '--sticky-typing mode
   sticky = sticky XOR 1
   GOSUB setsticky
  END IF
 END IF
 textcolor 10, 0: printstr UCASE$(opt$(her.list_type(listnum))), 300 - LEN(opt$(her.list_type(listnum))) * 8, 0, dpage
 textcolor 7, 0: IF bctr = 0 THEN textcolor 14 + tog, 0
 printstr "Previous Menu", 0, 0, dpage
 FOR i = 1 TO 24
  textcolor 7, 0: IF bctr = i THEN textcolor 14 + tog, 0
  temp1$ = attack$(i)
  WITH her.spell_lists(listnum, i-1)
   IF .attack > 0 THEN
    IF .learned = 0 THEN temp2$ = "Learned from Item"
    IF .learned > 0 THEN temp2$ = "Learned at Level" + XSTR$(.learned - 1)
   ELSE
    temp2$ = ""
   END IF
  END WITH
  textcolor 7, 0: IF bctr = i AND colcsr = 0 THEN textcolor 14 + tog, 0 + sticky
  printstr temp1$, 0, 8 * i, dpage
  textcolor 7, 0: IF bctr = i AND colcsr = 1 THEN textcolor 14 + tog, 0 + sticky
  printstr temp2$, 160, 8 * i, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

setsticky:
IF sticky THEN
 leftkey = 75
 rightkey = 77
ELSE
 leftkey = 51
 rightkey = 52
END IF
RETRACE

gosubatkname:
WITH her.spell_lists(listnum, o-1)
 IF .attack = 0 THEN
  attack$(o) = "EMPTY"
 ELSE
  attack$(o) = STR$(.attack - 1) + ":" + readattackname$(.attack - 1)
 END IF
END WITH
RETRACE

graph:
o = INT((bctr - 1) / 2)
textcolor 7, 0
printstr names$(nof(o)), 310 - LEN(names$(nof(o))) * 8, 180, dpage
FOR i = 0 TO 99 STEP 4
 ii = (.8 * i / 50) * i
 n0 = her.Lev0.sta(o)
 n99 = her.Lev99.sta(o)
 ii = ii * ((n99 - n0) / 100) + n0
 ii = large(ii, 0)
 j = (ii) * (100 / max(bctr))
 rectangle 290 + (i / 4), 176 - j, 1, j + 1, 7, dpage
NEXT i
RETRACE

smi:
FOR i = 0 TO 11
 bmenu$(i * 2 + 1) = names$(nof(i)) + XSTR$(her.Lev0.sta(i))
 bmenu$(i * 2 + 2) = names$(nof(i)) + XSTR$(her.Lev99.sta(i))
NEXT i
RETRACE

movesmall:
wc = wc + 1: IF wc >= 15 THEN wc = 0: wd = wd + 1: IF wd > 3 THEN wd = 0
IF wd = 0 THEN wy = wy - 4
IF wd = 1 THEN wx = wx + 4
IF wd = 2 THEN wy = wy + 4
IF wd = 3 THEN wx = wx - 4
RETRACE

clearhero:
blankhero.sprite_pal = -1      'default battle palette
blankhero.walk_sprite_pal = -1 'default walkabout palette
saveherodata @blankhero, pt
RETRACE

lasthero:
her.name = nam$
FOR i = 0 TO 3
 her.list_name(i) = hmenu$(i)
NEXT i
saveherodata @her, pt
RETRACE

thishero:
loadherodata @her, pt
nam$ = her.name
FOR i = 0 TO 3
 hmenu$(i) = her.list_name(i)
NEXT i
menu$(2) = "Name:" + nam$
menu$(1) = CHR$(27) + "Pick Hero" + XSTR$(pt) + CHR$(26)
GOSUB heropics
RETRACE

heropics:
setpicstuf buffer(), 5120, 2
loadset game$ + ".pt0", her.sprite, 0
getpal16 pal16(), 0, her.sprite_pal, 0, her.sprite
setpicstuf buffer(), 1600, 2
loadset game$ + ".pt4", her.walk_sprite, 16
getpal16 pal16(), 1, her.walk_sprite_pal, 4, her.walk_sprite
RETRACE

END SUB

SUB herotags (hero AS HeroDef ptr)

DIM menu$(5)
menu$(0) = "Previous Menu"
menu$(1) = "have hero TAG"
menu$(2) = "is alive TAG"
menu$(3) = "is leader TAG"
menu$(4) = "is in party now TAG"

WITH *hero

pt = 0
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 dummy = usemenu(pt, 0, 0, 4, 24)
 SELECT CASE pt
  CASE 0
   IF keyval(57) > 1 OR keyval(28) > 1 THEN EXIT DO
  CASE 1
   dummy = intgrabber(.have_tag, 0, 999, 75, 77)
  CASE 2
   dummy = intgrabber(.alive_tag, 0, 999, 75, 77)
  CASE 3
   dummy = intgrabber(.leader_tag, 0, 999, 75, 77)
  CASE 4
   dummy = intgrabber(.active_tag, 0, 999, 75, 77)
 END SELECT
 FOR i = 0 TO 4
  textcolor 7, 0
  IF pt = i THEN textcolor 14 + tog, 0
  SELECT CASE i
   CASE 1
    a$ = lmnemonic(.have_tag)
   CASE 2
    a$ = lmnemonic(.alive_tag)
   CASE 3
    a$ = lmnemonic(.leader_tag)
   CASE 4
    a$ = lmnemonic(.active_tag)
  END SELECT
  printstr menu$(i) & " (" & a$ & ")", 0, i * 8, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
END WITH
EXIT SUB

END SUB

FUNCTION intgrabber (n, min, max, less, more)
STATIC clip
old = n

IF more <> 0 AND keyval(more) > 1 THEN
 n = loopvar(n, min, max, 1)
ELSEIF less <> 0 AND keyval(less) > 1 THEN
 n = loopvar(n, min, max, -1)
ELSE
 s = SGN(n)
 n = ABS(n)
 IF keyval(14) > 1 THEN n \= 10
 FOR i = 1 TO 9
  IF keyval(i + 1) > 1 THEN n = n * 10 + i
 NEXT i
 IF keyval(11) > 1 THEN n *= 10
 IF min < 0 AND max > 0 THEN
  IF keyval(12) > 1 OR keyval(13) > 1 OR keyval(74) > 1 OR keyval(78) > 1 THEN s = s * -1
 END IF
 IF min < 0 AND (s < 0 OR max = 0) THEN n = -n
 'CLIPBOARD
 IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83)) OR (keyval(29) > 0 AND keyval(46) > 1) THEN clip = n
 IF ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(82) > 1) OR (keyval(29) > 0 AND keyval(47) > 1) THEN n = clip
 n = large(min, n)
 n = small(max, n)
END IF

IF old = n THEN
 intgrabber = 0
ELSE
 intgrabber = 1
END IF

END FUNCTION

SUB itemdata
DIM names$(100), a(99), menu$(20), bmenu$(40), nof(12), b(40), ibit$(-1 TO 59), item$(-1 TO 255), eqst$(5), max(18), min(18), sbmax(11), workpal(8), elemtype$(2), frame
DIM her AS HeroDef ' This is only used in equipbit
imax = 32
nof(0) = 0: nof(1) = 1: nof(2) = 2: nof(3) = 3: nof(4) = 5: nof(5) = 6: nof(6) = 29: nof(7) = 30: nof(8) = 8: nof(9) = 7: nof(10) = 31: nof(11) = 4
clearpage 0
clearpage 1
clearpage 2
clearpage 3
getnames names$(), imax
elemtype$(0) = readglobalstring$(127, "Weak to", 10)
elemtype$(1) = readglobalstring$(128, "Strong to", 10)
elemtype$(2) = readglobalstring$(129, "Absorbs ", 10)

eqst$(0) = "NEVER EQUIPPED"
eqst$(1) = "Weapon"
FOR i = 0 TO 3
 eqst$(i + 2) = names$(25 + i)
NEXT i
FOR i = 0 TO 1
 sbmax(i) = 9999
NEXT i
FOR i = 2 TO 8
 sbmax(i) = 999
NEXT i
FOR i = 9 TO 10
 sbmax(i) = 100
NEXT i
sbmax(11) = 10

csr = 0: top = -1: pt = 0
GOSUB litemname
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 IF keyval(29) > 0 AND keyval(14) AND csr >= 0 THEN
  cropafter csr, 254, -1, game$ + ".itm", 200, 1
  GOSUB litemname
 END IF
 dummy = usemenu(csr, top, -1, 254, 23)
 dummy = intgrabber(csr, -1, 254, 75, 77)
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF csr = -1 THEN EXIT DO
  IF csr <= 254 THEN
   GOSUB edititem
   saveitemdata a(), csr
   i = csr: GOSUB sitemname
  END IF
 END IF
 FOR i = top TO top + 23
  IF i <= 254 THEN
   textcolor 7, 0
   IF i = csr THEN textcolor 14 + tog, 0
   temp$ = XSTR$(i) + " " + item$(i)
   IF i < 0 THEN temp$ = "Return to Main Menu"
   printstr temp$, 0, (i - top) * 8, dpage
  END IF
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
clearpage 0
clearpage 1
clearpage 2
clearpage 3
EXIT SUB

edititem:
loaditemdata a(), csr
info$ = readbadbinstring$(a(), 9, 35, 0)

menu$(0) = "Back to Item Menu"
menu$(18) = "Stat Bonuses..."
menu$(19) = "Equipment Bits..."
menu$(20) = "Who Can Equip?..."
max(3) = 32767
max(4) = gen(34) + 1
max(5) = gen(34) + 1
max(6) = 5
max(7) = gen(34) + 1
max(8) = gen(34) + 1
max(9) = gen(31)
max(10) = 32767
min(10) = -1
max(11) = 2
max(12) = 999
max(13) = 999
max(14) = 999
max(15) = 999

loaditemdata a(), csr
GOSUB itemmenu

setpicstuf buffer(), 576, 2
loadset game$ + ".pt5", a(52), 0
getpal16 workpal(), 0, a(53), 5, a(52)

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETRACE
 IF (keyval(51) > 1 AND frame = 0) OR (keyval(52) > 1 AND frame = 1) THEN
  frame = frame XOR 1
  GOSUB itemmenu
 END IF
 dummy = usemenu(pt, 0, 0, 20, 24)
 IF keyval(28) > 1 OR keyval(57) > 1 THEN
  IF pt = 0 THEN RETRACE
  IF a(49) > 0 THEN
   IF pt = 18 THEN
    GOSUB statbon
    GOSUB itemmenu
   END IF
   IF pt = 19 THEN
    GOSUB ibitset
    GOSUB itemmenu
   END IF
   IF pt = 20 THEN
    GOSUB equipbit
    GOSUB itemmenu
   END IF
  END IF
  IF pt = 10 THEN '--palette picker
   a(46 + (pt - 3)) = pal16browse(a(53), 2, 0, 0, 24, 24, 2)
   getpal16 workpal(), 0, a(53), 5, a(52)
   GOSUB itemmenu
  END IF
 END IF
 SELECT CASE pt
  CASE 1
   strgrabber item$(csr), 8
   menu$(1) = "Name:" + item$(csr)
  CASE 2
   strgrabber info$, 34
   menu$(2) = "Info:" + info$
  CASE 3, 6, 9, 10
   IF intgrabber(a(46 + (pt - 3)), min(pt), max(pt), 75, 77) THEN GOSUB itemmenu
  CASE 4, 5, 7
   IF zintgrabber(a(46 + (pt - 3)), -1, max(pt), 75, 77) THEN GOSUB itemmenu
  CASE 8
   IF xintgrabber(a(46 + (pt - 3)), 0, max(pt), -1, gen(39) * -1, 75, 77) THEN GOSUB itemmenu
  CASE 11
   IF intgrabber(a(73), 0, 2, 75, 77) THEN GOSUB itemmenu
  CASE 12 TO 15
   IF intgrabber(a(74 + (pt - 12)), 0, max(pt), 75, 77) THEN GOSUB itemmenu
  CASE 16, 17
   IF intgrabber(a(78 + (pt - 16) + frame * 2), -100, 100,75,77) THEN GOSUB itemmenu
 END SELECT
 FOR i = 0 TO 20
  textcolor 7, 0
  IF pt = i THEN textcolor 14 + tog, 0
  IF (i >= 18 AND a(49) = 0) OR ((i = 16 OR i = 17) AND a(49) <> 1) THEN
   textcolor 8, 0
   IF pt = i THEN textcolor 6 + tog, 0
  END IF
  printstr menu$(i), 0, i * 8, dpage
 NEXT i
 IF a(49) = 1 THEN
  loadsprite buffer(), 0, (1-frame) * 288, 0, 24, 24, 2
  drawsprite buffer(), 0, workpal(), 0, 280, 160, dpage
  textcolor 7, 0
  IF frame = 0 THEN printstr "<",280,152,dpage
  printstr XSTR$(1 - frame),281,152,dpage
  IF frame = 1 THEN printstr ">",296,152,dpage
   drawline 278 + a(78 + frame * 2),160 + a(79 + frame * 2),279 + a(78 + frame * 2), 160 + a(79 + frame * 2),14 + tog,dpage
   drawline 280 + a(78 + frame * 2),158 + a(79 + frame * 2),280 + a(78 + frame * 2), 159 + a(79 + frame * 2),14 + tog,dpage
   drawline 281 + a(78 + frame * 2),160 + a(79 + frame * 2),282 + a(78 + frame * 2), 160 + a(79 + frame * 2),14 + tog,dpage
   drawline 280 + a(78 + frame * 2),161 + a(79 + frame * 2),280 + a(78 + frame * 2), 162 + a(79 + frame * 2),14 + tog,dpage
 END IF
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

itemmenu:
menu$(1) = "Name:" + item$(csr)
menu$(2) = "Info:" + info$
menu$(3) = "Value" + XSTR$(a(46))
n = 47: GOSUB itatkname
menu$(4) = "When used in battle- " + temp$
n = 48: GOSUB itatkname
menu$(5) = "When used as a Weapon- " + temp$
menu$(6) = "Equippable as- " + eqst$(large(small(a(49), 5), 0))
n = 50: GOSUB itatkname
menu$(7) = "Teach Spell- " + temp$
IF a(51) >= 0 THEN
 n = 51: GOSUB itatkname
 menu$(8) = "When used out of battle- " + temp$
ELSE
 menu$(8) = "When used out of battle- Text" + XSTR$(ABS(a(51)))
END IF
menu$(9) = "Weapon Picture" + XSTR$(a(52))
menu$(10) = "Weapon Palette" + defaultint$(a(53))
IF a(49) <> 1 THEN menu$(9) = "Weapon Picture N/A": menu$(10) = "Weapon Palette N/A"
menu$(11) = "Unlimited Use"
IF a(73) = 1 THEN menu$(11) = "Consumed By Use"
IF a(73) = 2 THEN menu$(11) = "Cannot be Sold/Dropped"
menu$(12) = "own item TAG" + XSTR$(a(74)) + " " + lmnemonic(a(74))
menu$(13) = "is in inventory TAG" + XSTR$(a(75)) + " " + lmnemonic(a(75))
menu$(14) = "is equipped TAG" + XSTR$(a(76)) + " " + lmnemonic(a(76))
menu$(15) = "eqpt by active hero TAG" + XSTR$(a(77)) + " " + lmnemonic(a(77))
menu$(16) = "Handle X:"
menu$(17) = "Handle Y:"
IF a(49) = 1 THEN
 menu$(16) = menu$(16) + XSTR$(a(78 + frame * 2))
 menu$(17) = menu$(17) + XSTR$(a(79 + frame * 2))
ELSE
 menu$(16) = menu$(16) + "N/A"
 menu$(17) = menu$(17) + "N/A"
END IF
IF pt = 9 OR pt = 10 THEN
 'oldframe = frame
 setpicstuf buffer(), 576, 2
 loadset game$ + ".pt5", a(52), 0
 getpal16 workpal(), 0, a(53), 5, a(52)
END IF
RETRACE

statbon:
ptr2 = 0
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETRACE
 dummy = usemenu(ptr2, 0, -1, 11, 24)
 IF keyval(28) > 1 OR keyval(57) > 1 THEN
  IF ptr2 = -1 THEN RETRACE
 END IF
 IF ptr2 >= 0 THEN
  dummy = intgrabber(a(54 + ptr2), sbmax(ptr2) * -1, sbmax(ptr2), 75, 77)
 END IF
 textcolor 7, 0
 IF ptr2 = -1 THEN textcolor 14 + tog, 0
 printstr "Previous Menu", 0, 0, dpage
 FOR i = 0 TO 11
  textcolor 7, 0
  IF ptr2 = i THEN textcolor 14 + tog, 0
  printstr names$(nof(i)) + " Bonus:" + XSTR$(a(54 + i)), 0, 8 + i * 8, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

itatkname: 'n is the offset
temp$ = "NOTHING"
IF a(n) <= 0 THEN RETRACE
temp$ = XSTR$(a(n) - 1) + " " + readattackname$(a(n) - 1)
RETRACE

litemname:
FOR i = 0 TO 254
 loaditemdata a(), i
 item$(i) = readbadbinstring$(a(), 0, 8, 0)
NEXT i
RETRACE

sitemname:
loaditemdata a(), i
a(0) = LEN(item$(i))
FOR o = 1 TO a(0)
 a(o) = ASC(MID$(item$(i), o, 1))
NEXT o
a(9) = LEN(info$)
FOR o = 10 TO 9 + a(9)
 a(o) = ASC(MID$(info$, o - 9, 1))
NEXT o
saveitemdata a(), i
RETRACE

ibitset:
FOR i = 0 TO 7
 ibit$(i) = elemtype$(0) + " " + names$(17 + i)
 ibit$(i + 8) = elemtype$(1) + " " + names$(17 + i)
 ibit$(i + 16) = elemtype$(2) + " " + names$(17 + i)
NEXT i
editbitset a(), 70, 23, ibit$()
RETRACE

equipbit:
'"DIM her AS HeroDef" is only used here but is dimmed at the top of the itemdata sub to avoid branch crossing
FOR i = 0 TO 59
 loadherodata @her, i
 ibit$(i) = "Equipable by " + her.name
NEXT i
editbitset a(), 66, 59, ibit$()
RETRACE

'SHOP STUFF
'0       Name length
'1-8     Name
'9       description length
'10-45   description
'46      cash value
'47      attack when used in battle
'48      attack as weapon
'49      Equip style
'        0 never
'        1 weapon
'        2 shield
'        3 armor
'        4 head
'        5 ring/gauntlet
'50      Learn when used out of battle
'51      Attack when use oob
'52      Weapon picture
'53      weapon pal
'54      Hp bonus
'55      Mp bonus
'56      Str bonus
'57      Acc bonus
'58      Def bonus
'59      Dodge bonus
'60      Mag bonus
'61      Will bonus
'62      Speed bonus
'63      Counter bonus
'64      Focus bonus
'65      Xhits bonus
'66-69   equipability bitsets
'70-72   bitsetmask
'73      Consumed by use
'74      when have tag
'75      is in inventory
'76      is equiped tag
'77      is equiped by hero in active party

END SUB

FUNCTION itemstr$ (it%, hidden%, offbyone%)
 'it - the item number
 'hidden - whether to *not* prefix the item number
 'offbyone - whether it is the item number (1), or the itemnumber + 1 (0)
 IF it = 0 AND offbyone = 0 THEN itemstr$ = " NONE": EXIT FUNCTION
 IF offbyone THEN itn = it ELSE itn = it - 1

 loaditemdata buffer(), itn
 re$ = ""
 re$ = readbadbinstring$(buffer(), 0, 8, 0)
 IF hidden = 0 THEN re$ = XSTR$(itn) + " " + re$
 itemstr$ = re$
END FUNCTION

SUB npcdef (npc(), pt)
DIM npc$(15), unpc(15), lnpc(15), mtype$(10), push$(7), stepi(5), info$(5, 1), pal16(288)

clearpage 0: clearpage 1
setvispage vpage

npc$(0) = "Picture"
npc$(1) = "Palette"
npc$(2) = "Move Type"
npc$(3) = "Move Speed"
npc$(4) = "Display Text"
npc$(5) = "When Activated"
npc$(6) = "Give Item:"
npc$(7) = "Pushability"
npc$(8) = "Activation"
npc$(9) = "Appear if Tag"
npc$(10) = "Appear if Tag"
npc$(11) = "Usable"
npc$(12) = "Run Script: "
npc$(13) = "Script Argument"
npc$(14) = "Vehicle: "

unpc(0) = 119
unpc(1) = 32767
unpc(2) = 8
unpc(3) = 5
unpc(4) = -1
unpc(5) = 2
unpc(6) = 255
unpc(7) = 7
unpc(8) = 2
unpc(9) = 999
unpc(10) = 999
unpc(11) = 1
unpc(12) = 0
unpc(13) = 32767
unpc(14) = 0
FOR i = 0 TO 14
 lnpc(i) = 0
NEXT i
lnpc(1) = -1
lnpc(9) = -999
lnpc(10) = -999
lnpc(13) = -32767

unpc(4) = gen(39)'max text boxes
unpc(12) = gen(43)'max scripts
unpc(14) = gen(55) + 1'max vehicles
mtype$(0) = "Stand Still"
mtype$(1) = "Wander"
mtype$(2) = "Pace"
mtype$(3) = "Right Turns"
mtype$(4) = "Left Turns"
mtype$(5) = "Random Turns"
mtype$(6) = "Chase You"
mtype$(7) = "Avoid You"
mtype$(8) = "Walk In Place"
push$(0) = " Off"
push$(1) = " Full"
push$(2) = " Vertical"
push$(3) = " Horizontal"
push$(4) = " Up only"
push$(5) = " Right Only"
push$(6) = " Down Only"
push$(7) = " Left Only"
stepi(0) = 0
stepi(1) = 1
stepi(2) = 2
stepi(3) = 10
stepi(4) = 4
stepi(5) = 5
info$(0, 0) = ":Use"
info$(1, 0) = ":Touch"
info$(2, 0) = ":Step On"
info$(0, 1) = " Change Direction"
info$(1, 1) = " Face Player"
info$(2, 1) = " Do Not Face Player"
unpc(0) = gen(30)

csr = 0
cur = 0: top = 0
FOR i = 0 TO 35
 GOSUB loadnpcpic
NEXT i
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 dummy = usemenu(cur, top, 0, 35, 7)
 IF (keyval(57) > 1 OR keyval(28) > 1) THEN GOSUB npcstats
 FOR i = top TO top + 7
  textcolor 7, 0
  IF cur = i THEN textcolor 14 + tog, 0
  printstr XSTR$(i), 0, ((i - top) * 25), dpage
  loadsprite buffer(), 0, 800, 5 * i, 20, 20, 2
  drawsprite buffer(), 0, pal16(), 16 * i, 32, ((i - top) * 25), dpage
 NEXT
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
clearpage 0
clearpage 1
clearpage 2
EXIT SUB

loadnpcpic:
setpicstuf buffer(), 1600, 2
loadset game$ + ".pt4", npc(i * 15 + 0), 5 * i
getpal16 pal16(), i, npc(i * 15 + 1), 4, npc(i * 15 + 0)
RETRACE

npcstats:
it$ = itemstr(npc(cur * 15 + 6), 0, 0)
x$ = ""
scrname$ = scriptname$(npc(cur * 15 + 12), plottrigger)
GOSUB frstline
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF npc(cur * 15 + 2) > 0 THEN walk = walk + 1: IF walk > 3 THEN walk = 0
 IF keyval(1) > 1 THEN RETRACE
 dummy = usemenu(csr, 0, -1, 14, 24)
 IF csr = 12 THEN
  IF keyval(28) > 1 OR keyval(57) > 1 THEN
   scrname$ = scriptbrowse$(npc(cur * 15 + 12), plottrigger, "NPC use plotscript")
  ELSEIF scrintgrabber(npc(cur * 15 + 12), 0, 0, 75, 77, 1, plottrigger) THEN
   scrname$ = scriptname$(npc(cur * 15 + 12), plottrigger)
  END IF
 END IF
 IF csr = 11 THEN
  IF keyval(75) > 1 OR keyval(77) > 1 OR keyval(57) > 1 OR keyval(28) > 1 THEN GOSUB onetimetog
 END IF
 IF (csr >= 1 AND csr < 11) OR csr > 12 THEN
  IF intgrabber(npc(cur * 15 + csr), lnpc(csr), unpc(csr), 75, 77) THEN
   IF csr = 1 THEN getpal16 pal16(), cur, npc(cur * 15 + 1), 4, npc(cur * 15 + 0)
   IF csr = 6 THEN it$ = itemstr(npc(cur * 15 + 6), 0, 0)
   IF csr = 4 THEN GOSUB frstline
  END IF
 END IF
 IF csr = 1 AND (keyval(28) > 1 OR keyval(57) > 1) THEN
  npc(cur * 15 + csr) = pal16browse(npc(cur * 15 + csr), 8, 0, 5 * cur, 20, 20, 2)
  getpal16 pal16(), cur, npc(cur * 15 + 1), 4, npc(cur * 15 + 0) 
 END IF
 IF csr = 0 THEN
  IF intgrabber(npc(cur * 15 + csr), lnpc(csr), unpc(csr), 75, 77) = 1 THEN
   i = cur
   GOSUB loadnpcpic
  END IF
 END IF
 IF (keyval(57) > 1 OR keyval(28) > 1) AND csr = -1 THEN RETRACE
 textcolor 7, 0
 IF csr = -1 THEN textcolor 14 + tog, 0
 printstr "Previous Menu", 0, 0, dpage
 FOR i = 0 TO 14
  textcolor 7, 0
  IF csr = i THEN textcolor 14 + tog, 0
  temp$ = XSTR$(npc(cur * 15 + i))
  SELECT CASE i
   CASE 1
    temp$ = defaultint$(npc(cur * 15 + i))
   CASE 2
    temp$ = " = " + mtype$(npc(cur * 15 + i))
   CASE 3
    temp$ = XSTR$(stepi(npc(cur * 15 + i)))
   CASE 5
    temp$ = info$(npc(cur * 15 + i), 1)
   CASE 6
    temp$ = it$
   CASE 7
    temp$ = push$(npc(cur * 15 + i))
   CASE 8
    temp$ = info$(npc(cur * 15 + i), 0)
   CASE 9, 10
    IF npc(cur * 15 + i) THEN
     temp$ = XSTR$(ABS(npc(cur * 15 + i))) + " = " + onoroff$(npc(cur * 15 + i)) + " (" + lmnemonic$(ABS(npc(cur * 15 + i))) + ")"
    ELSE
     temp$ = " 0 (N/A)"
    END IF
   CASE 11
    IF npc(cur * 15 + i) THEN temp$ = " Only Once (tag" + XSTR$(1000 + npc(cur * 15 + i)) + ")" ELSE temp$ = " Repeatedly"
   CASE 12 'script
    temp$ = scrname$
   CASE 13 'script arg
    IF npc(cur * 15 + 12) = 0 THEN temp$ = " N/A"
   CASE 14 'vehicle
    IF npc(cur * 15 + 14) <= 0 THEN
     temp$ = "No"
    ELSE
     setpicstuf buffer(), 80, -1
     loadset game$ + ".veh", npc(cur * 15 + 14) - 1, 0
     temp$ = STRING$(bound(buffer(0) AND 255, 0, 15), 0)
     array2str buffer(), 1, temp$
    END IF
  END SELECT
  printstr npc$(i) + temp$, 0, 8 + (8 * i), dpage
 NEXT i
 rectangle 9, 139, 22, 22, 15, dpage
 rectangle 10, 140, 20, 20, 7, dpage
 loadsprite buffer(), 0, 800 + (200 * INT(walk / 2)), 5 * cur, 20, 20, 2
 drawsprite buffer(), 0, pal16(), 16 * cur, 10, 140, dpage
 a$ = "Appears if tag" + XSTR$(ABS(npc(cur * 15 + 9))) + " = " + onoroff$(npc(cur * 15 + 9)) + " and tag" + XSTR$(ABS(npc(cur * 15 + 10))) + " = " + onoroff$(npc(cur * 15 + 10))
 IF npc(cur * 15 + 9) <> 0 AND npc(cur * 15 + 10) = 0 THEN a$ = "Appears if tag" + XSTR$(ABS(npc(cur * 15 + 9))) + " = " + onoroff$(npc(cur * 15 + 9))
 IF npc(cur * 15 + 9) = 0 AND npc(cur * 15 + 10) <> 0 THEN a$ = "Appears if tag" + XSTR$(ABS(npc(cur * 15 + 10))) + " = " + onoroff$(npc(cur * 15 + 10))
 IF npc(cur * 15 + 9) = 0 AND npc(cur * 15 + 10) = 0 THEN a$ = "Appears all the time"
 textcolor 15, 0
 printstr a$, 0, 190, dpage
 textcolor 15, 1
 printstr x$, 0, 170, dpage
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

onetimetog:
IF npc(cur * 15 + 11) > 0 THEN
 setbit gen(), 106, npc(cur * 15 + 11) - 1, 0
 npc(cur * 15 + 11) = 0: RETRACE
END IF

i = 0
DO
 gen(105) = loopvar(gen(105), 0, 999, 1)
 i = i + 1: IF i > 1000 THEN RETRACE
LOOP UNTIL readbit(gen(), 106, gen(105)) = 0
npc(cur * 15 + 11) = gen(105) + 1
setbit gen(), 106, gen(105), 1
RETRACE

frstline:
x$ = ""
IF npc(cur * 15 + 4) = 0 THEN RETRACE
x$ = STRING$(38, 0)
setpicstuf buffer(), 400, -1
loadset game$ + ".say", npc(cur * 15 + 4), 0
array2str buffer(), 0, x$
RETRACE

'npc(i * 15 + 0) = picture
'npc(i * 15 + 1) = palette
'npc(i * 15 + 2) = move type
'npc(i * 15 + 3) = move speed
'npc(i * 15 + 4) = display text
'npc(i * 15 + 5) = when activated
'npc(i * 15 + 6) = give item
'npc(i * 15 + 7) = pushability
'npc(i * 15 + 8) = activation
'npc(i * 15 + 9) = appear if tag
'npc(i * 15 + 10) = appear if secondary tag
'npc(i * 15 + 11) = usable
'npc(i * 15 + 12) = run script
'npc(i * 15 + 13) = script argument
'npc(i * 15 + 14) = vehicle
'Picture,Palette,Move Type,Move Speed,Display Text,When Activated,"Give Item:"
'Pushability,Activation,Appear if Tag,Appear if Tag,Usable,*Unused*

END SUB

FUNCTION readattackname$ (index)

'--clobbers buffer!!!

readattackname$ = readbadgenericname$(index, game$ + ".dt6", 80, 24, 10, 1)

END FUNCTION

FUNCTION readenemyname$ (index)

'--clobbers buffer!!!

readenemyname$ = readbadgenericname$(index, game$ + ".dt1", 320, 0, 16, 0)

END FUNCTION

FUNCTION readitemname$ (index)

'--clobbers buffer!!!

readitemname$ = readbadgenericname$(index, game$ + ".itm", 200, 0, 8, 0)

END FUNCTION

SUB readscatter (s$, lhold, start)
DIM stray(10)
s$ = STRING$(20, "!")

FOR i = 0 TO lhold
 setbit stray(), 0, i, readbit(gen(), start - 1, gen(start + i))
NEXT i

array2str stray(), 0, s$
s$ = LEFT$(s$, INT((lhold + 1) / 8))

END SUB

FUNCTION readshopname$ (shopnum)

'clobbers buffer!

readshopname$ = readbadgenericname$(shopnum, game$ + ".sho", 40, 0, 15, 0)

END FUNCTION

SUB stredit (s$, maxl)
STATIC clip$

'--copy support
IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83)) OR (keyval(29) > 0 AND keyval(46) > 1) THEN clip$ = s$

'--paste support
IF ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(82) > 1) OR (keyval(29) > 0 AND keyval(47) > 1) THEN s$ = LEFT$(clip$, maxl)

'--insert cursor movement
IF keyval(29) = 0 THEN 'not CTRL
 IF keyval(75) > 1 THEN insert = large(0, insert - 1)
 IF keyval(77) > 1 THEN insert = small(LEN(s$), insert + 1)
ELSE 'CTRL
 IF keyval(75) > 1 THEN insert = 0
 IF keyval(77) > 1 THEN insert = LEN(s$)
END IF

IF insert < 0 THEN insert = LEN(s$)
insert = bound(insert, 0, LEN(s$))

pre$ = LEFT$(s$, insert)
post$ = RIGHT$(s$, LEN(s$) - insert)

'--BACKSPACE support
IF keyval(14) > 1 AND LEN(pre$) > 0 THEN
 pre$ = LEFT$(pre$, LEN(pre$) - 1)
 insert = large(0, insert - 1)
END IF

'--DEL support
IF keyval(83) > 1 AND LEN(post$) > 0 THEN post$ = RIGHT$(post$, LEN(post$) - 1)

'--SHIFT support
shift = 0
IF keyval(54) > 0 OR keyval(42) > 0 THEN shift = 1

'--ALT support
IF keyval(56) THEN shift = shift + 2

'--adding chars
IF LEN(pre$) + LEN(post$) < maxl THEN
 L = LEN(pre$)
 IF keyval(57) > 1 THEN
  IF keyval(29) = 0 THEN
   '--SPACE support
   pre$ = pre$ + " "
  ELSE
   '--charlist support
   pre$ = pre$ + charpicker$
  END IF
 ELSE
  IF keyval(29) = 0 THEN
   '--all other keys
   FOR i = 2 TO 53
    IF keyval(i) > 1 AND keyv(i, shift) > 0 THEN
     pre$ = pre$ + CHR$(keyv(i, shift))
     EXIT FOR
    END IF
   NEXT i
  END IF
 END IF
 IF LEN(pre$) > L THEN insert = insert + 1
END IF

s$ = pre$ + post$

END SUB

SUB strgrabber (s$, maxl)
STATIC clip$

'--BACKSPACE support
IF keyval(14) > 1 AND LEN(s$) > 0 THEN s$ = LEFT$(s$, LEN(s$) - 1)

'--copy support
IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83)) OR (keyval(29) > 0 AND keyval(46) > 1) THEN clip$ = s$

'--paste support
IF ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(82) > 1) OR (keyval(29) > 0 AND keyval(47) > 1) THEN s$ = LEFT$(clip$, maxl)

'--SHIFT support
shift = 0
IF keyval(54) > 0 OR keyval(42) > 0 THEN shift = 1

'--ALT support
IF keyval(56) THEN shift = shift + 2

'--adding chars
IF LEN(s$) < maxl THEN

 IF keyval(57) > 1 THEN
  IF keyval(29) = 0 THEN
   '--SPACE support
   s$ = s$ + " "
  ELSE
   '--charlist support
   s$ = s$ + charpicker$
  END IF
 ELSE
  IF keyval(29) = 0 THEN
   '--all other keys
   FOR i = 2 TO 53
    IF keyval(i) > 1 AND keyv(i, shift) > 0 THEN
     s$ = s$ + CHR$(keyv(i, shift))
     EXIT FOR
    END IF
   NEXT i
  END IF
 END IF

END IF

END SUB

SUB writescatter (s$, lhold, start)
DIM stray(10)

s$ = LEFT$(s$, 20)
lhold = LEN(s$) * 8 - 1
str2array s$, stray(), 0

FOR i = 0 TO lhold
 trueb = readbit(stray(), 0, i)
 DO
  scatb = INT(RND * (16 + (i * 16)))
 LOOP UNTIL readbit(gen(), start - 1, scatb) = trueb
 gen(start + i) = scatb
NEXT i

FOR i = lhold + 1 TO 159
 gen(start + i) = INT(RND * 4444)
NEXT i
END SUB

FUNCTION zintgrabber (n, min, max, less, more)
'--adjust for entries that are offset by +1
'--what a hack!
'--all entries <= 0 are special options not meant to be enumerated
'--supply the min & max as visible, not actual range for n
'--eg a menu with 'A' = -2, 'B' = -1, 'C' = 0, 'item 0 - item 99' = 1 - 100 would have min = -3, max = 99
old = n
temp = n - 1
'--must adjust to always be able to type in a number
IF temp < 0 THEN
 FOR i = 2 TO 11
  IF keyval(i) > 1 THEN temp = 0
 NEXT i
END IF
dummy = intgrabber(temp, min, max, less, more)
n = temp + 1
IF old = 1 AND keyval(14) > 1 THEN n = 0

IF old = n THEN
 zintgrabber = 0
ELSE
 zintgrabber = 1
END IF

END FUNCTION

