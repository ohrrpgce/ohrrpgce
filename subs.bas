'OHRRPGCE CUSTOM - Misc unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE FUNCTION readshopname$ (shopnum%)
DECLARE FUNCTION filenum$ (n%)
DECLARE SUB standardmenu (menu$(), size%, vis%, ptr%, top%, x%, y%, page%, edge%)
DECLARE FUNCTION charpicker$ ()
DECLARE FUNCTION readbinstring$ (array%(), offset%, maxlen%)
DECLARE FUNCTION readbadbinstring$ (array%(), offset%, maxlen%, skipword%)
DECLARE SUB writeenemydata (buf%(), index%)
DECLARE FUNCTION readenemyname$ (index%)
DECLARE FUNCTION readbadgenericname$ (index%, filename$, recsize%, offset%, size%, skip%)
DECLARE FUNCTION readitemname$ (index%)
DECLARE SUB clearallpages ()
DECLARE SUB enforceflexbounds (menuoff%(), menutype%(), menulimits%(), recbuf%(), min%(), max%())
DECLARE FUNCTION editflexmenu% (nowindex%, menutype%(), menuoff%(), menulimits%(), datablock%(), mintable%(), maxtable%())
DECLARE SUB updateflexmenu (nowmenu$(), nowdat%(), size%, menu$(), menutype%(), menuoff%(), menulimits%(), datablock%(), caption$(), maxtable%(), recindex%)
DECLARE SUB setactivemenu (workmenu%(), newmenu%(), ptr%, top%, size%)
DECLARE SUB addcaption (caption$(), indexer%, cap$)
DECLARE SUB testflexmenu ()
DECLARE SUB setbinsize (id%, size%)
DECLARE SUB writeattackdata (array%(), index%)
DECLARE SUB readattackdata (array%(), index%)
DECLARE SUB flusharray (array%(), size%, value%)
DECLARE FUNCTION getbinsize% (id%)
DECLARE FUNCTION readattackname$ (index%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE FUNCTION pal16browse% (curpal%, usepic%, picx%, picy%, picw%, pich%, picpage%)
DECLARE SUB getpal16 (array%(), aoffset%, foffset%)
DECLARE FUNCTION zintgrabber% (n%, min%, max%, less%, more%)
DECLARE FUNCTION intgrabber (n%, min%, max%, less%, more%)
DECLARE SUB strgrabber (s$, maxl%)
DECLARE SUB xbload (f$, array%(), e$)
DECLARE FUNCTION scriptname$ (num%, f$)
DECLARE FUNCTION needaddset (ptr%, check%, what$)
DECLARE SUB cropafter (index%, limit%, flushafter%, lump$, bytes%, prompt%)
DECLARE SUB herotags (a%())
DECLARE SUB cycletile (cycle%(), tastuf%(), ptr%(), skip%())
DECLARE SUB testanimpattern (tastuf%(), taset%)
DECLARE FUNCTION usemenu (ptr%, top%, first%, last%, size%)
DECLARE FUNCTION heroname$ (num%, cond%(), a%())
DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE FUNCTION onoroff$ (n%)
DECLARE FUNCTION intstr$ (n%)
DECLARE FUNCTION lmnemonic$ (index%)
DECLARE FUNCTION rotascii$ (s$, o%)
DECLARE SUB debug (s$)
DECLARE SUB bitset (array%(), wof%, last%, name$())
DECLARE FUNCTION usemenu (ptr%, top%, first%, last%, size%)
DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE SUB formation (song$())
DECLARE SUB enemydata ()
DECLARE SUB herodata ()
DECLARE SUB attackdata ()
DECLARE SUB getnames (stat$(), max%)
DECLARE SUB statname ()
DECLARE SUB textage (song$())
DECLARE FUNCTION sublist% (num%, s$())
DECLARE SUB maptile (master%(), font%())
DECLARE FUNCTION small% (n1%, n2%)
DECLARE FUNCTION large% (n1%, n2%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)

'$INCLUDE: 'allmodex.bi'
'$INCLUDE: 'cglobals.bi'

REM $STATIC
FUNCTION bound (n, lowest, highest)
bound = n
IF n < lowest THEN bound = lowest
IF n > highest THEN bound = highest
END FUNCTION

FUNCTION charpicker$

STATIC ptr

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
 
 IF keyval(72) > 1 THEN ptr = large(ptr - linesize, 0)
 IF keyval(80) > 1 THEN ptr = small(ptr + linesize, last)
 IF keyval(75) > 1 THEN ptr = large(ptr - 1, 0)
 IF keyval(77) > 1 THEN ptr = small(ptr + 1, last)
 
 IF keyval(28) > 1 OR keyval(57) > 1 THEN charpicker$ = CHR$(f(ptr)): EXIT DO
 
 FOR i = 0 TO last
  textcolor 7, 8
  IF (i MOD linesize) = (ptr MOD linesize) OR (i \ linesize) = (ptr \ linesize) THEN textcolor 7, 1
  IF ptr = i THEN textcolor 14 + tog, 0
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

SUB edgeprint (s$, x, y, c, p)
textcolor 240, 0
printstr s$, x, y + 1, p
printstr s$, x + 1, y, p
printstr s$, x + 2, y + 1, p
printstr s$, x + 1, y + 2, p
textcolor c, 0
printstr s$, x + 1, y + 1, p
END SUB

SUB enemydata

'--stat names
DIM name$(32), nof(11), elemtype$(2)
elemtype$(0) = readglobalstring$(127, "Weak to", 10)
elemtype$(1) = readglobalstring$(128, "Strong to", 10)
elemtype$(2) = readglobalstring$(129, "Absorbs ", 10)
getnames name$(), 32
'--name offsets
nof(0) = 0: nof(1) = 1: nof(2) = 2: nof(3) = 3: nof(4) = 5: nof(5) = 6: nof(6) = 29: nof(7) = 30: nof(8) = 8: nof(9) = 7: nof(10) = 31: nof(11) = 4

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
 ebit$(0 + i) = elemtype$(0) + " " + name$(17 + i)
 ebit$(8 + i) = elemtype$(1) + " " + name$(17 + i)
 ebit$(16 + i) = elemtype$(2) + " " + name$(17 + i)
 ebit$(24 + i) = "Is " + name$(9 + i)
NEXT i
FOR i = 32 TO 53
 ebit$(i) = "(placeholder" + STR$(i) + ")"
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
DIM max(22), min(22)
'Limit 0 is not used

CONST EnLimPic = 1
max(EnLimPic) = general(27) 'or 28 or 29. Must be updated!

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
max(EnLimSpawn) = general(36) + 1 'must be updated!

CONST EnLimSpawnNum = 19
max(EnLimSpawnNum) = 8

CONST EnLimAtk = 20
max(EnLimAtk) = general(34) + 1

CONST EnLimStr16 = 21
max(EnLimStr16) = 16

CONST EnLimStealAvail = 22
min(EnLimStealAvail) = -1
max(EnLimStealAvail) = 1
addcaption caption$(), capindex, "Disabled"
EnCapStealAvail = capindex
addcaption caption$(), capindex, "Only one"
addcaption caption$(), capindex, "Unlimited"

'--next limit 23, remeber to update dim!

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
menutype(EnMenuPal) = 0
menuoff(EnMenuPal) = EnDatPal
menulimits(EnMenuPal) = EnLimUInt

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
 menu$(EnMenuStat + i) = name$(nof(i)) + ":"
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
 menu$(EnMenuSpawnElement + i) = "on " + name$(17 + i) + " Hit:"
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
ptr = 0: top = 0: size = 0

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
setactivemenu workmenu(), mainMenu(), ptr, top, size

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
  cropafter recindex, general(36), 0, game$ + ".dt1", 320, 1
 END IF
 
 dummy = usemenu(ptr, top, 0, size, 22)
 
 IF workmenu(ptr) = EnMenuChooseAct THEN
  lastindex = recindex
  IF keyval(77) > 1 AND recindex = general(36) AND recindex < 32767 THEN
   '--attempt to add a new set
   '--save current
   writeenemydata recbuf(), lastindex
   '--increment
   recindex = recindex + 1
   '--make sure we really have permission to increment
   IF needaddset(recindex, general(36), "enemy") THEN
    flusharray recbuf(), 159, 0
    GOSUB EnUpdateMenu
   END IF
  ELSE
   IF intgrabber(recindex, 0, general(36), 75, 77) THEN
    writeenemydata recbuf(), lastindex
    GOSUB EnLoadSub
   END IF
  END IF
 END IF
 
 IF keyval(28) > 1 OR keyval(57) > 1 THEN
  SELECT CASE workmenu(ptr)
   CASE EnMenuBackAct
    IF menudepth = 1 THEN
     GOSUB EnBackSub
    ELSE
     EXIT DO
    END IF
   CASE EnMenuAppearAct
    GOSUB EnPushPtrSub
    setactivemenu workmenu(), appearMenu(), ptr, top, size
    GOSUB EnUpdateMenu
   CASE EnMenuRewardAct
    GOSUB EnPushPtrSub
    setactivemenu workmenu(), rewardMenu(), ptr, top, size
    GOSUB EnUpdateMenu
   CASE EnMenuStatAct
    GOSUB EnPushPtrSub
    setactivemenu workmenu(), statMenu(), ptr, top, size
    GOSUB EnUpdateMenu
   CASE EnMenuSpawnAct
    GOSUB EnPushPtrSub
    setactivemenu workmenu(), spawnMenu(), ptr, top, size
    GOSUB EnUpdateMenu
   CASE EnMenuAtkAct
    GOSUB EnPushPtrSub
    setactivemenu workmenu(), atkMenu(), ptr, top, size
    GOSUB EnUpdateMenu
   CASE EnMenuPal
    recbuf(EnDatPal) = pal16browse(recbuf(EnDatPal), 1, 0, 0, previewsize(recbuf(EnDatPicSize)), previewsize(recbuf(EnDatPicSize)), 2)
    GOSUB EnUpdateMenu
   CASE EnMenuBitsetAct
    bitset recbuf(), EnDatBitset, 61, ebit$()
  END SELECT
 END IF
 
 IF editflexmenu(workmenu(ptr), menutype(), menuoff(), menulimits(), recbuf(), min(), max()) THEN
  GOSUB EnUpdateMenu
 END IF
 
 GOSUB EnPreviewSub
 
 standardmenu dispmenu$(), size, 22, ptr, top, 0, 0, dpage, 0
 
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP

'--save what we were last working on
writeenemydata recbuf(), recindex

clearallpages

EXIT SUB

'-----------------------------------------------------------------------

EnUpdateMenu:

'--in case new enemies have been added
max(EnLimSpawn) = general(36) + 1

'--in case the PicSize has changed
max(EnLimPic) = general(27 + bound(recbuf(EnDatPicSize), 0, 2))

'--re-enforce bounds, as they might have just changed
enforceflexbounds menuoff(), menutype(), menulimits(), recbuf(), min(), max()

updateflexmenu dispmenu$(), workmenu(), size, menu$(), menutype(), menuoff(), menulimits(), recbuf(), caption$(), max(), recindex

'--load the picture and palette
setpicstuf buffer(), (previewsize(recbuf(EnDatPicSize)) ^ 2) / 2, 2
loadset game$ + ".pt" + LTRIM$(STR$(1 + recbuf(EnDatPicSize))) + CHR$(0), recbuf(EnDatPic), 0
getpal16 workpal(), 0, recbuf(EnDatPal)

RETURN

'-----------------------------------------------------------------------

EnPreviewSub:
loadsprite buffer(), 0, 0, 0, previewsize(recbuf(EnDatPicSize)), previewsize(recbuf(EnDatPicSize)), 2
wardsprite buffer(), 0, workpal(), 0, 260 - previewsize(recbuf(EnDatPicSize)) / 2, 180 - previewsize(recbuf(EnDatPicSize)), dpage
RETURN

'-----------------------------------------------------------------------

EnBackSub:
setactivemenu workmenu(), mainMenu(), ptr, top, size
menudepth = 0
ptr = lastptr
top = lasttop
GOSUB EnUpdateMenu
RETURN

'-----------------------------------------------------------------------

EnPushPtrSub:
lastptr = ptr
lasttop = top
menudepth = 1
RETURN

'-----------------------------------------------------------------------

EnLoadSub:
setpicstuf recbuf(), 320, -1
loadset game$ + ".dt1" + CHR$(0), recindex, 0
GOSUB EnUpdateMenu
RETURN

'-----------------------------------------------------------------------
END SUB

SUB formation (song$())
DIM a(40), b(160), c(24), s(7), w(7), menu$(10), ename$(7), max(10), z(7), bmenu$(22), pal16(64)
clearpage 0
clearpage 1
clearpage 2
clearpage 3
setdiskpages buffer(), 200, 0

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
GOSUB loadfset
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN GOSUB savefset: RETURN
 'IF keyval(72) > 1 THEN
 '  bcsr = large(bcsr - 1, 0)
 '  GOSUB lpreviewform
 'END IF
 'IF keyval(80) > 1 THEN
 '  bcsr = small(bcsr + 1, 22)
 '  GOSUB lpreviewform
 'END IF
 IF usemenu(bcsr, 0, 0, 22, 24) THEN GOSUB lpreviewform
 IF keyval(28) > 1 OR keyval(57) > 1 THEN
  IF bcsr = 0 THEN GOSUB savefset: RETURN
 END IF
 IF bcsr = 1 THEN
  IF keyval(75) > 1 THEN GOSUB savefset: gptr = large(gptr - 1, 0): GOSUB loadfset
  IF keyval(77) > 1 THEN GOSUB savefset: gptr = small(gptr + 1, 255): GOSUB loadfset
 END IF
 IF bcsr = 2 THEN dummy = intgrabber(c(0), 0, 99, 75, 77)
 IF bcsr > 2 THEN
  IF zintgrabber(c(bcsr - 2), -1, general(37), 75, 77) THEN
   GOSUB lpreviewform
  END IF
  IF ptr >= 0 THEN
   '--preview form
   GOSUB formsprite
  END IF
 END IF
 bmenu$(1) = CHR$(27) + "Formation Set" + STR$(gptr + 1) + CHR$(26)
 bmenu$(2) = "Battle Frequency:" + STR$(c(0))
 FOR i = 3 TO 22
  bmenu$(i) = "Formation" + STR$(c(i - 2) - 1)
  IF c(i - 2) = 0 THEN bmenu$(i) = "Empty"
 NEXT i
 
 standardmenu bmenu$(), 22, 22, bcsr, 0, 0, 0, dpage, 1
 
 SWAP vpage, dpage
 setvispage vpage
 IF bcsr > 2 AND ptr >= 0 THEN
  copypage 2, dpage
 ELSE
  clearpage dpage
 END IF
 dowait
LOOP

lpreviewform:
IF bcsr > 2 THEN
 '--have form selected
 ptr = c(bcsr - 2) - 1
 IF ptr >= 0 THEN
  '--form not empty
  GOSUB loadform
  GOSUB formpics
 END IF
END IF
RETURN

savefset:
setpicstuf c(), 50, -1
storeset game$ + ".efs" + CHR$(0), gptr, 0
RETURN

loadfset:
setpicstuf c(), 50, -1
loadset game$ + ".efs" + CHR$(0), gptr, 0
RETURN

editform:
menu$(3) = "Previous Menu"
max(0) = general(100) - 1
max(1) = 100
max(2) = 999
ptr = 0: csr2 = -5: csr3 = 0
GOSUB loadform
GOSUB formpics
setkeys
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
  IF keyval(1) > 1 THEN GOSUB saveform: RETURN
  IF keyval(29) > 0 AND keyval(14) THEN cropafter ptr, general(37), 0, game$ + ".for", 80, 1
  dummy = usemenu(csr2, -5, -5, 7, 24)
  IF keyval(57) > 1 OR keyval(28) > 1 THEN
   IF csr2 = -5 THEN GOSUB saveform: RETURN
   IF csr2 >= 0 THEN IF a(csr2 * 4 + 0) > 0 THEN csr3 = 1
  END IF
  IF csr2 > -4 AND csr2 < 0 THEN
   IF intgrabber(a(35 + csr2), 0, max(csr2 + 3), 75, 77) THEN GOSUB saveform: GOSUB loadform
  END IF
  IF csr2 = -4 THEN '---SELECT A DIFFERENT FORMATION
   remptr = ptr
   IF intgrabber(ptr, 0, general(37), 51, 52) THEN
    SWAP ptr, remptr
    GOSUB saveform
    SWAP ptr, remptr
    GOSUB loadform
    GOSUB formpics
   END IF
   IF keyval(75) > 1 AND ptr > 0 THEN GOSUB saveform: ptr = large(ptr - 1, 0): GOSUB loadform: GOSUB formpics
   IF keyval(77) > 1 AND ptr < 32767 THEN
    GOSUB saveform
    ptr = ptr + 1
    IF needaddset(ptr, general(37), "formation") THEN GOSUB clearformation
    GOSUB loadform
    GOSUB formpics
   END IF
  END IF'--DONE SELECTING DIFFERENT FORMATION
  IF csr2 >= 0 THEN
   IF zintgrabber(a(csr2 * 4 + 0), -1, general(36), 75, 77) THEN
    GOSUB formpics
   END IF
  END IF
 END IF
 GOSUB formsprite
 FOR i = 0 TO 3
  rectangle 240 + i * 8, 75 + i * 22, 32, 40, 18 + i * 2, dpage
 NEXT i
 IF csr3 = 0 THEN
  menu$(4) = CHR$(27) + "formation" + STR$(ptr) + CHR$(26)
  menu$(5) = "Backdrop screen:" + STR$(a(32))
  menu$(6) = "Battle Music:"
  IF a(33) = 0 THEN menu$(6) = menu$(6) + " -none-" ELSE menu$(6) = menu$(6) + STR$(a(33)) + " " + song$(a(33) - 1)
  menu$(7) = "*Unused*:" + STR$(a(34))
  FOR i = 0 TO 4
   col = 7: IF csr2 + 5 = i THEN col = 14 + tog
   edgeprint menu$(i + 3), 1, 1 + (i * 10), col, dpage
  NEXT i
  FOR i = 0 TO 7
   col = 7: IF csr2 = i THEN col = 14 + tog
   edgeprint "Enemy:" + ename$(i), 1, 50 + (i * 10), col, dpage
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
FOR i = 0 TO 6
 temp = 200
 FOR o = 7 TO i STEP -1
  IF a(z(o) * 4 + 2) < temp THEN temp = a(z(o) * 4 + 2): j = o
 NEXT o
 SWAP z(j), z(i)
NEXT i
FOR i = 0 TO 7
 IF a(z(i) * 4 + 0) > 0 THEN
  loadsprite buffer(), 0, 0, z(i) * 10, w(z(i)), w(z(i)), 3
  drawsprite buffer(), 0, pal16(), 16 * z(i), a(z(i) * 4 + 1), a(z(i) * 4 + 2), dpage
  IF csr2 = z(i) THEN textcolor 176 + ABS(flash), 0: printstr CHR$(25), a(z(i) * 4 + 1) + (w(z(i)) * .5) - 4, a(z(i) * 4 + 2), dpage
 END IF
NEXT i
RETURN

clearformation:
FOR i = 0 TO 40
 a(i) = 0
NEXT i
setpicstuf a(), 80, -1
storeset game$ + ".for" + CHR$(0), ptr, 0
RETURN

saveform:
setpicstuf a(), 80, -1
storeset game$ + ".for" + CHR$(0), ptr, 0
RETURN

loadform:
setpicstuf a(), 80, -1
loadset game$ + ".for" + CHR$(0), ptr, 0
loadpage game$ + ".mxs" + CHR$(0), a(32), 2
IF a(33) = 0 THEN a(33) = general(4)
RETURN

formpics:
FOR i = 0 TO 7
 ename$(i) = "-EMPTY-"
 IF a(i * 4 + 0) > 0 THEN
  setpicstuf b(), 320, -1
  loadset game$ + ".dt1" + CHR$(0), a(i * 4 + 0) - 1, 0
  ename$(i) = LTRIM$(STR$(a(i * 4 + 0) - 1)) + ":"
  FOR o = 1 TO b(0)
   ename$(i) = ename$(i) + CHR$(b(o))
  NEXT o
  getpal16 pal16(), i, b(54)
  IF b(55) = 0 THEN s(i) = 578: w(i) = 34: f$ = ".pt1"
  IF b(55) = 1 THEN s(i) = 1250: w(i) = 50: f$ = ".pt2"
  IF b(55) = 2 THEN s(i) = 3200: w(i) = 80: f$ = ".pt3"
  setpicstuf buffer(), s(i), 3
  loadset game$ + f$ + CHR$(0), b(53), i * 10
 END IF
NEXT i
RETURN

END SUB

FUNCTION getbinsize (id)

IF isfile(workingdir$ + "\binsize.bin" + CHR$(0)) THEN
 fh = FREEFILE
 OPEN workingdir$ + "\binsize.bin" FOR BINARY AS #fh
 GET #fh, 1 + id * 2, recordsize
 CLOSE #fh
 getbinsize = recordsize
ELSE
 getbinsize = 0
END IF

'0  ATTACK.BIN

END FUNCTION

SUB herodata
DIM name$(100), a(318), menu$(8), bmenu$(40), max(40), min(40), nof(12), attack$(24), b(40), option$(10), hbit$(-1 TO 25), hmenu$(4), pal16(16), elemtype$(2)
wd = 1: max = 32
nof(0) = 0: nof(1) = 1: nof(2) = 2: nof(3) = 3: nof(4) = 5: nof(5) = 6: nof(6) = 29: nof(7) = 30: nof(8) = 8: nof(9) = 7: nof(10) = 31: nof(11) = 4
clearpage 0
clearpage 1
clearpage 2
clearpage 3
getnames name$(), max
elemtype$(0) = readglobalstring$(127, "Weak to", 10)
elemtype$(1) = readglobalstring$(128, "Strong to", 10)
elemtype$(2) = readglobalstring$(129, "Absorbs ", 10)

csr = 1
FOR i = 0 TO 7
 hbit$(i) = elemtype$(0) + " " + name$(17 + i)
 hbit$(i + 8) = elemtype$(1) + " " + name$(17 + i)
 hbit$(i + 16) = elemtype$(2) + " " + name$(17 + i)
NEXT i
hbit$(24) = "Rename when added to party"
hbit$(25) = "Permit renaming on status screen"

menu$(0) = "Return to Main Menu"
menu$(1) = CHR$(27) + "Pick Hero" + STR$(ptr) + CHR$(26)
menu$(2) = "Name:"
menu$(3) = "Appearance and Misc..."
menu$(4) = "Edit Stats..."
menu$(5) = "Edit Spell Lists..."
menu$(6) = "Name Spell Lists..."
menu$(7) = "Bitsets..."
menu$(8) = "Hero Tags..."
GOSUB thishero

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 GOSUB movesmall
 IF keyval(1) > 1 THEN EXIT DO
 IF keyval(29) > 0 AND keyval(14) THEN
  cropafter ptr, general(35), -1, game$ + ".dt0", 636, 1
 END IF
 dummy = usemenu(csr, 0, 0, 8, 24)
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF csr = 0 THEN EXIT DO
  IF csr = 3 THEN GOSUB picnpal
  IF csr = 4 THEN GOSUB levstats
  IF csr = 5 THEN GOSUB speltypes '--spell list contents
  IF csr = 6 THEN GOSUB heromenu '--spell list names
  IF csr = 7 THEN bitset a(), 240, 25, hbit$()
  IF csr = 8 THEN herotags a()
 END IF
 IF csr = 1 THEN
  remptr = ptr
  IF intgrabber(ptr, 0, general(35), 51, 52) THEN
   SWAP ptr, remptr
   GOSUB lasthero
   SWAP ptr, remptr
   GOSUB thishero
  END IF
  IF keyval(75) > 1 AND ptr > 0 THEN GOSUB lasthero: ptr = ptr - 1: GOSUB thishero
  IF keyval(77) > 1 AND ptr < 59 THEN
   GOSUB lasthero
   ptr = ptr + 1
   IF needaddset(ptr, general(35), "hero") THEN GOSUB clearhero
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
 IF keyval(1) > 1 THEN RETURN
 IF (keyval(57) > 1 OR keyval(28) > 1) AND bctr = 0 THEN RETURN
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
 IF a(288 + i) > 10 OR a(288 + i) < 0 THEN a(288 + i) = 0
NEXT i
bctr = -1
option$(0) = "Spells (MP Based)"
option$(1) = "Spells (FF1 Style)"
option$(2) = "Random Effects"
option$(3) = "Item Consuming (not implemented)"
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETURN
 dummy = usemenu(bctr, -1, -1, 3, 24)
 IF bctr >= 0 THEN dummy = intgrabber(a(288 + bctr), 0, 2, 75, 77)
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF bctr = -1 THEN RETURN
  IF bctr >= 0 AND bctr < 4 THEN temp = bctr: GOSUB spells: bctr = temp
 END IF
 textcolor 7, 0: IF bctr = -1 THEN textcolor 14 + tog, 0
 printstr "Previous Menu", 0, 0, dpage
 FOR i = 0 TO 3
  textcolor 7, 0: IF bctr = i THEN textcolor 14 + tog, 0
  printstr "Type" + STR$(i) + " Spells:" + option$(a(288 + i)), 0, 8 + i * 8, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

picnpal:
bctr = 0
bmenu$(0) = "Previous Menu"
min(1) = 0: max(1) = general(26)
min(2) = 0: max(2) = 32767
min(3) = 0: max(3) = general(30)
min(4) = 0: max(4) = 32767
min(5) = -1: max(5) = 99
min(6) = 0: max(6) = 254
min(7) = 0: max(7) = 16
GOSUB itstrh
setkeys
DO
 GOSUB genheromenu
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 GOSUB movesmall
 IF keyval(1) > 1 THEN RETURN
 dummy = usemenu(bctr, 0, 0, 7, 24)
 IF (keyval(28) > 1 OR keyval(57) > 1) AND bctr = 0 THEN RETURN
 IF bctr > 0 THEN
  SELECT CASE bctr
   CASE 0 TO 6
    IF intgrabber(a(16 + bctr), min(bctr), max(bctr), 75, 77) THEN
     IF bctr >= 1 OR bctr <= 4 THEN GOSUB heropics
     IF bctr = 6 THEN GOSUB itstrh
    END IF
   CASE 7
    dummy = intgrabber(a(296), min(bctr), max(bctr), 75, 77)
  END SELECT
  IF (bctr = 2 OR bctr = 4) AND (keyval(28) > 1 OR keyval(57) > 1) THEN
   IF bctr = 2 THEN
    a(16 + bctr) = pal16browse(a(16 + bctr), 8, 0, 0, 32, 40, 2)
   ELSE
    a(16 + bctr) = pal16browse(a(16 + bctr), 8, 0, 16, 20, 20, 2)
   END IF
   GOSUB heropics
  END IF
 END IF
 
 standardmenu bmenu$(), 7, 22, bctr, 0, 8, 0, dpage, 0
 
 GOSUB heropreview
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

heropreview:
loadsprite buffer(), 0, 640 * tog, 0, 32, 40, 2
drawsprite buffer(), 0, pal16(), 0, 250, 25, dpage
loadsprite buffer(), 0, (wd * 400) + (200 * tog), 16, 20, 20, 2
drawsprite buffer(), 0, pal16(), 16, 230 + wx, 5 + wy, dpage
RETURN

genheromenu:
bmenu$(1) = "Battle Picture:" + STR$(a(17))
bmenu$(2) = "Battle Palette:" + STR$(a(18))
bmenu$(3) = "Walkabout Picture:" + STR$(a(19))
bmenu$(4) = "Walkabout Palette:" + STR$(a(20))
bmenu$(5) = "Base Level:" + STR$(a(21))
IF a(21) < 0 THEN bmenu$(5) = "Base Level: Party Average"
bmenu$(6) = "Default Weapon:" + it$
bmenu$(7) = "Max Name Length:"
IF a(296) THEN
 bmenu$(7) = bmenu$(7) + STR$(a(296))
ELSE
 bmenu$(7) = bmenu$(7) + " default"
END IF
RETURN

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
 IF keyval(1) > 1 THEN RETURN
 IF keyval(72) > 1 THEN bctr = large(bctr - 2, 0)
 IF keyval(80) > 1 AND bctr > 0 THEN bctr = small(bctr + 2, 24)
 IF keyval(80) > 1 AND bctr = 0 THEN bctr = bctr + 1
 IF keyval(75) > 1 AND bctr > 0 THEN bctr = bctr - 1
 IF keyval(77) > 1 AND bctr < 24 THEN bctr = bctr + 1
 IF (keyval(28) > 1 OR keyval(57) > 1) AND bctr = 0 THEN RETURN
 IF bctr > 0 THEN
  IF intgrabber(a(22 + bctr), min(bctr), max(bctr), 51, 52) THEN GOSUB smi
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
offset = 47 + (temp * 48)
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
  IF keyval(1) > 1 THEN RETURN
  IF usemenu(bctr, 0, 0, 24, 24) THEN
   IF a(offset + (bctr - 1) * 2) = 0 THEN colcsr = 0
  END IF
  IF keyval(75) > 1 OR keyval(77) > 1 THEN
   colcsr = colcsr XOR 1
   IF a(offset + (bctr - 1) * 2) = 0 THEN colcsr = 0
  END IF
 END IF
 IF bctr > 0 THEN
  IF colcsr = 0 THEN
   IF zintgrabber(a(offset + (bctr - 1) * 2), -1, general(34), leftkey, rightkey) THEN
    o = bctr
    GOSUB gosubatkname
   END IF
  END IF
  IF colcsr = 1 THEN dummy = zintgrabber(a(offset + (bctr - 1) * 2 + 1), -1, 99, leftkey, rightkey)
 END IF
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF bctr = 0 THEN
   '--exit menu
   RETURN
  ELSE
   '--sticky-typing mode
   sticky = sticky XOR 1
   GOSUB setsticky
  END IF
 END IF
 textcolor 10, 0: printstr UCASE$(option$(a(288 + temp))), 300 - LEN(option$(a(288 + temp))) * 8, 0, dpage
 textcolor 7, 0: IF bctr = 0 THEN textcolor 14 + tog, 0
 printstr "Previous Menu", 0, 0, dpage
 FOR i = 1 TO 24
  textcolor 7, 0: IF bctr = i THEN textcolor 14 + tog, 0
  temp1$ = attack$(i)
  IF a(offset + (i - 1) * 2) > 0 THEN
   IF a(offset + (i - 1) * 2 + 1) = 0 THEN temp2$ = "Learned from Item"
   IF a(offset + (i - 1) * 2 + 1) > 0 THEN temp2$ = "Learned at Level" + STR$(a(offset + (i - 1) * 2 + 1) - 1)
  ELSE
   temp2$ = ""
  END IF
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
RETURN

gosubatkname:
IF a(offset + (o - 1) * 2) = 0 THEN attack$(o) = "EMPTY": RETURN
attack$(o) = LTRIM$(STR$(a(offset + (o - 1) * 2) - 1)) + ":"
attack$(o) = attack$(o) + readattackname$(a(offset + (o - 1) * 2) - 1)
RETURN

graph:
o = INT((bctr - 1) / 2)
textcolor 7, 0
printstr name$(nof(o)), 310 - LEN(name$(nof(o))) * 8, 180, dpage
FOR i = 0 TO 99 STEP 4
 ii = (.8 * i / 50) * i * ((a(24 + o * 2) - a(23 + o * 2)) / 100) + a(23 + o * 2)
 ii = large(ii, 0)
 j = (ii) * (100 / max(bctr))
 rectangle 290 + (i / 4), 176 - j, 1, j + 1, 7, dpage
NEXT i
RETURN

smi:
FOR i = 0 TO 11
 bmenu$(i * 2 + 1) = name$(nof(i)) + STR$(a(i * 2 + 23))
 bmenu$(i * 2 + 2) = name$(nof(i)) + STR$(a(i * 2 + 24))
NEXT i
RETURN

movesmall:
wc = wc + 1: IF wc >= 15 THEN wc = 0: wd = wd + 1: IF wd > 3 THEN wd = 0
IF wd = 0 THEN wy = wy - 4
IF wd = 1 THEN wx = wx + 4
IF wd = 2 THEN wy = wy + 4
IF wd = 3 THEN wx = wx - 4
RETURN

clearhero:
FOR i = 0 TO 318
 a(i) = 0
NEXT i
setpicstuf a(), 636, -1
storeset game$ + ".dt0" + CHR$(0), ptr, 0
RETURN

lasthero:
a(0) = LEN(nam$)
FOR i = 1 TO a(0)
 a(i) = ASC(MID$(nam$, i, 1))
NEXT i
FOR i = 0 TO 3
 a(243 + i * 11) = LEN(hmenu$(i))
 FOR o = 1 TO LEN(hmenu$(i))
  a(243 + (i * 11) + o) = ASC(MID$(hmenu$(i), o, 1))
 NEXT o
NEXT i
setpicstuf a(), 636, -1
storeset game$ + ".dt0" + CHR$(0), ptr, 0
RETURN

thishero:
setpicstuf a(), 636, -1
loadset game$ + ".dt0" + CHR$(0), ptr, 0
nam$ = readbadbinstring$(a(), 0, 16, 0)
FOR i = 0 TO 3
 hmenu$(i) = readbadbinstring$(a(), 243 + i * 11, 10, 0)
NEXT i
menu$(2) = "Name:" + nam$
menu$(1) = CHR$(27) + "Pick Hero" + STR$(ptr) + CHR$(26)
GOSUB heropics
RETURN

heropics:
setpicstuf buffer(), 5120, 2
loadset game$ + ".pt0" + CHR$(0), a(17), 0
getpal16 pal16(), 0, a(18)
setpicstuf buffer(), 1600, 2
loadset game$ + ".pt4" + CHR$(0), a(19), 16
getpal16 pal16(), 1, a(20)
RETURN

itstrh:
setpicstuf buffer(), 200, -1
loadset game$ + ".itm" + CHR$(0), a(22), 0
it$ = ""
FOR o = 1 TO small(buffer(0), 20)
 IF buffer(o) < 256 AND buffer(o) > -1 THEN it$ = it$ + CHR$(buffer(o)) ELSE it$ = ""
NEXT o
RETURN

'0-318,636 bytes
'0       name length
'1-16    name content
'17      pic
'18      pal
'19      walkabout pic
'20      walkabout pal
'21      base level
'22      default weapon
'23-46   stats
'47-238  spell lists
'240-242 bitsets
'243-286 spell list names
'288-291 spell menu types
'292-295 hero tags
'296     max rename length

END SUB

SUB herotags (a())

DIM menu$(5)
menu$(0) = "Previous Menu"
menu$(1) = "have hero TAG"
menu$(2) = "is alive TAG"
menu$(3) = "is leader TAG"
menu$(4) = "is in party now TAG"

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 dummy = usemenu(ptr, 0, 0, 4, 24)
 SELECT CASE ptr
  CASE 0
   IF keyval(57) > 1 OR keyval(28) > 1 THEN EXIT DO
  CASE ELSE
   dummy = intgrabber(a(291 + ptr), 0, 999, 75, 77)
 END SELECT
 FOR i = 0 TO 4
  a$ = ""
  IF i > 0 THEN
   a$ = STR$(a(291 + i)) + " (" + lmnemonic(a(291 + i)) + ")"
  END IF
  textcolor 7, 0
  IF ptr = i THEN textcolor 14 + tog, 0
  printstr menu$(i) + a$, 0, i * 8, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
EXIT SUB

'292     have hero tag
'293     is alive tag
'294     is leader tag
'295     is in active party tag
END SUB

FUNCTION intgrabber (n, min, max, less, more)
STATIC clip
old = n

IF keyval(more) > 1 THEN
 n = loopvar(n, min, max, 1)
ELSEIF keyval(less) > 1 THEN
 n = loopvar(n, min, max, -1)
ELSE
 s = SGN(n)
 n$ = intstr$(ABS(n))
 IF keyval(14) > 1 AND LEN(n$) > 0 THEN n$ = LEFT$(n$, LEN(n$) - 1)
 FOR i = 1 TO 9
  IF keyval(i + 1) > 1 THEN n$ = n$ + intstr$(i)
 NEXT i
 IF keyval(11) > 1 THEN n$ = n$ + "0"
 IF min < 0 THEN
  IF keyval(12) > 1 OR keyval(13) > 1 OR keyval(74) > 1 OR keyval(78) > 1 THEN s = s * -1
 END IF
 capper& = INT(VAL(n$))
 IF capper& > 32767 THEN capper& = 32767
 IF capper& < -32767 THEN capper& = -32767
 n = capper&
 IF s <> 0 THEN n = n * s
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
DIM name$(100), a(99), menu$(18), bmenu$(40), nof(12), b(40), ibit$(-1 TO 59), item$(-1 TO 255), eqst$(5), max(18), sbmax(11), workpal(8), elemtype$(2)
max = 32
nof(0) = 0: nof(1) = 1: nof(2) = 2: nof(3) = 3: nof(4) = 5: nof(5) = 6: nof(6) = 29: nof(7) = 30: nof(8) = 8: nof(9) = 7: nof(10) = 31: nof(11) = 4
clearpage 0
clearpage 1
clearpage 2
clearpage 3
getnames name$(), max
elemtype$(0) = readglobalstring$(127, "Weak to", 10)
elemtype$(1) = readglobalstring$(128, "Strong to", 10)
elemtype$(2) = readglobalstring$(129, "Absorbs ", 10)

eqst$(0) = "NEVER EQUIPPED"
eqst$(1) = "Weapon"
FOR i = 0 TO 3
 eqst$(i + 2) = name$(25 + i)
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

csr = 0: top = -1
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
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF csr = -1 THEN EXIT DO
  IF csr <= 254 THEN
   GOSUB edititem
   setpicstuf a(), 200, -1
   storeset game$ + ".itm" + CHR$(0), csr, 0
   i = csr: GOSUB sitemname
  END IF
 END IF
 FOR i = top TO top + 23
  IF i <= 254 THEN
   textcolor 7, 0
   IF i = csr THEN textcolor 14 + tog, 0
   temp$ = STR$(i) + " " + item$(i)
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
setpicstuf a(), 200, -1
loadset game$ + ".itm" + CHR$(0), csr, 0
info$ = readbadbinstring$(a(), 9, 35, 0)

menu$(0) = "Back to Item Menu"
menu$(16) = "Stat Bonuses..."
menu$(17) = "Equipment Bits..."
menu$(18) = "Who Can Equip?..."
max(3) = 32767
max(4) = general(34) + 1
max(5) = general(34) + 1
max(6) = 5
max(7) = general(34) + 1
max(8) = general(34) + 1
max(9) = general(31)
max(10) = 32767
max(11) = 2
max(12) = 999
max(13) = 999
max(14) = 999
max(15) = 999

setpicstuf a(), 200, -1
loadset game$ + ".itm" + CHR$(0), csr, 0
GOSUB itemmenu

setpicstuf buffer(), 576, 2
loadset game$ + ".pt5" + CHR$(0), a(52), 0
getpal16 workpal(), 0, a(53)

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETURN
 dummy = usemenu(ptr, 0, 0, 18, 24)
 IF keyval(28) > 1 OR keyval(57) > 1 THEN
  IF ptr = 0 THEN RETURN
  IF a(49) > 0 THEN
   IF ptr = 16 THEN GOSUB statbon: GOSUB itemmenu
   IF ptr = 17 THEN GOSUB ibitset: GOSUB itemmenu
   IF ptr = 18 THEN GOSUB equipbit: GOSUB itemmenu
  END IF
  IF ptr = 10 THEN '--palette picker
   a(46 + (ptr - 3)) = pal16browse(a(46 + (ptr - 3)), 2, 0, 0, 24, 24, 2)
   getpal16 workpal(), 0, a(46 + (ptr - 3))
   GOSUB itemmenu
  END IF
 END IF
 IF ptr >= 3 AND ptr <= 10 THEN
  min = 0: IF ptr = 8 THEN min = general(39) * -1
  IF intgrabber(a(46 + (ptr - 3)), min, max(ptr), 75, 77) THEN GOSUB itemmenu
 END IF
 IF ptr = 11 THEN
  IF intgrabber(a(73), 0, 2, 75, 77) THEN GOSUB itemmenu
 END IF
 IF ptr >= 12 AND ptr <= 15 THEN
  IF intgrabber(a(74 + (ptr - 12)), 0, max(ptr), 75, 77) THEN GOSUB itemmenu
 END IF
 IF ptr = 1 THEN
  strgrabber item$(csr), 8
  menu$(1) = "Name:" + item$(csr)
 END IF
 IF ptr = 2 THEN
  strgrabber info$, 34
  menu$(2) = "Info:" + info$
 END IF
 FOR i = 0 TO 18
  textcolor 7, 0
  IF ptr = i THEN textcolor 14 + tog, 0
  IF i >= 16 AND a(49) = 0 THEN
   textcolor 8, 0
   IF ptr = i THEN textcolor 6 + tog, 0
  END IF
  printstr menu$(i), 0, i * 8, dpage
 NEXT i
 IF a(49) = 1 THEN
  loadsprite buffer(), 0, 288, 0, 24, 24, 2
  drawsprite buffer(), 0, workpal(), 0, 280, 160, dpage
 END IF
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

itemmenu:
menu$(1) = "Name:" + item$(csr)
menu$(2) = "Info:" + info$
menu$(3) = "Value" + STR$(a(46))
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
 menu$(8) = "When used out of battle- Text" + STR$(ABS(a(51)))
END IF
menu$(9) = "Weapon Picture" + STR$(a(52))
menu$(10) = "Weapon Palette" + STR$(a(53))
IF a(49) <> 1 THEN menu$(9) = "Weapon Picture N/A": menu$(10) = "Weapon Palette N/A"
menu$(11) = "Unlimited Use"
IF a(73) = 1 THEN menu$(11) = "Consumed By Use"
IF a(73) = 2 THEN menu$(11) = "Cannot be Sold/Dropped"
menu$(12) = "own item TAG" + STR$(a(74)) + " " + lmnemonic(a(74))
menu$(13) = "is in inventory TAG" + STR$(a(75)) + " " + lmnemonic(a(75))
menu$(14) = "is equipped TAG" + STR$(a(76)) + " " + lmnemonic(a(76))
menu$(15) = "eqpt by active hero TAG" + STR$(a(77)) + " " + lmnemonic(a(77))
IF ptr = 9 OR ptr = 10 THEN
 setpicstuf buffer(), 576, 2
 loadset game$ + ".pt5" + CHR$(0), a(52), 0
 getpal16 workpal(), 0, a(53)
END IF
RETURN

statbon:
ptr2 = 0
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETURN
 dummy = usemenu(ptr2, 0, -1, 11, 24)
 IF keyval(28) > 1 OR keyval(57) > 1 THEN
  IF ptr2 = -1 THEN RETURN
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
  printstr name$(nof(i)) + " Bonus:" + STR$(a(54 + i)), 0, 8 + i * 8, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

itatkname: 'n is the offset
temp$ = "NOTHING"
IF a(n) <= 0 THEN RETURN
temp$ = readattackname$(a(n) - 1)
RETURN

litemname:
setpicstuf a(), 200, -1
FOR i = 0 TO 254
 loadset game$ + ".itm" + CHR$(0), i, 0
 item$(i) = readbadbinstring$(a(), 0, 8, 0)
NEXT i
RETURN

sitemname:
setpicstuf a(), 200, -1
loadset game$ + ".itm" + CHR$(0), i, 0
a(0) = LEN(item$(i))
FOR o = 1 TO a(0)
 a(o) = ASC(MID$(item$(i), o, 1))
NEXT o
a(9) = LEN(info$)
FOR o = 10 TO 9 + a(9)
 a(o) = ASC(MID$(info$, o - 9, 1))
NEXT o
storeset game$ + ".itm" + CHR$(0), i, 0
RETURN

ibitset:
FOR i = 0 TO 7
 ibit$(i) = elemtype$(0) + " " + name$(17 + i)
 ibit$(i + 8) = elemtype$(1) + " " + name$(17 + i)
 ibit$(i + 16) = elemtype$(2) + " " + name$(17 + i)
NEXT i
bitset a(), 70, 23, ibit$()
RETURN

equipbit:
FOR i = 0 TO 59
 setpicstuf buffer(), 636, -1
 loadset game$ + ".dt0" + CHR$(0), i, 0
 ibit$(i) = readbadbinstring$(buffer(), 0, 16, 0)
 ibit$(i) = "Equipable by " + ibit$(i)
NEXT i
bitset a(), 66, 59, ibit$()
RETURN

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

FUNCTION large (n1, n2)
large = n1
IF n2 > n1 THEN large = n2
END FUNCTION

FUNCTION loopvar (var, min, max, inc)
a = var + inc
IF a > max THEN a = a - ((max - min) + 1): loopvar = a: EXIT FUNCTION
IF a < min THEN a = a + ((max - min) + 1): loopvar = a: EXIT FUNCTION
loopvar = a
END FUNCTION

SUB npcdef (npc(), ptr, npc$(), unpc(), lnpc())
DIM mtype$(10), push$(7), stepi(5), info$(5, 1), pal16(288)
clearpage 0: clearpage 1
setvispage vpage
unpc(4) = general(39)'max text boxes
unpc(12) = general(43)'max scripts
unpc(14) = general(55) + 1'max vehicles
mtype$(0) = "Stand Still"
mtype$(1) = "Wander"
mtype$(2) = "Pace"
mtype$(3) = "Right Turns"
mtype$(4) = "Left Turns"
mtype$(5) = "Random Turns"
mtype$(6) = "Chase You"
mtype$(7) = "Avoid You"
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
unpc(0) = general(30)

npcpick:
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
  printstr STR$(i), 0, ((i - top) * 25), dpage
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
loadset game$ + ".pt4" + CHR$(0), npc(i * 15 + 0), 5 * i
getpal16 pal16(), i, npc(i * 15 + 1)
RETURN

npcstats:
GOSUB itstr
GOSUB frstline
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF npc(cur * 15 + 2) = 1 THEN walk = walk + 1: IF walk > 3 THEN walk = 0
 IF keyval(1) > 1 THEN RETURN
 'IF keyval(72) > 1 AND csr > -1 THEN csr = csr - 1
 'IF keyval(80) > 1 AND csr < 14 THEN csr = csr + 1
 dummy = usemenu(csr, 0, -1, 14, 24)
 IF csr = 11 THEN
  IF keyval(75) > 1 OR keyval(77) > 1 OR keyval(57) > 1 OR keyval(28) > 1 THEN GOSUB onetimetog
 END IF
 IF (csr >= 1 AND csr < 11) OR csr > 11 THEN
  IF intgrabber(npc(cur * 15 + csr), lnpc(csr), unpc(csr), 75, 77) THEN
   IF csr = 1 THEN getpal16 pal16(), cur, npc(cur * 15 + 1)
   IF csr = 6 THEN GOSUB itstr
   IF csr = 4 THEN GOSUB frstline
  END IF
 END IF
 IF csr = 1 AND (keyval(28) > 1 OR keyval(57) > 1) THEN
  npc(cur * 15 + csr) = pal16browse(npc(cur * 15 + csr), 8, 0, 5 * cur, 20, 20, 2)
  getpal16 pal16(), cur, npc(cur * 15 + csr)
 END IF
 IF csr = 0 THEN
  IF intgrabber(npc(cur * 15 + csr), lnpc(csr), unpc(csr), 75, 77) = 1 THEN
   i = cur
   GOSUB loadnpcpic
  END IF
 END IF
 IF (keyval(57) > 1 OR keyval(28) > 1) AND csr = -1 THEN RETURN
 textcolor 7, 0
 IF csr = -1 THEN textcolor 14 + tog, 0
 printstr "Previous Menu", 0, 0, dpage
 FOR i = 0 TO 14
  textcolor 7, 0
  IF csr = i THEN textcolor 14 + tog, 0
  temp$ = STR$(npc(cur * 15 + i))
  SELECT CASE i
   CASE 2
    temp$ = " = " + mtype$(npc(cur * 15 + i))
   CASE 3
    temp$ = STR$(stepi(npc(cur * 15 + i)))
   CASE 5
    temp$ = info$(npc(cur * 15 + i), 1)
   CASE 6
    temp$ = " " + it$
   CASE 7
    temp$ = push$(npc(cur * 15 + i))
   CASE 8
    temp$ = info$(npc(cur * 15 + i), 0)
   CASE 9, 10
    IF npc(cur * 15 + i) THEN
     temp$ = STR$(ABS(npc(cur * 15 + i))) + " = " + onoroff$(npc(cur * 15 + i)) + " (" + lmnemonic$(ABS(npc(cur * 15 + i))) + ")"
    ELSE
     temp$ = " 0 (N/A)"
    END IF
   CASE 11
    IF npc(cur * 15 + i) THEN temp$ = " Only Once (tag" + STR$(1000 + npc(cur * 15 + i)) + ")" ELSE temp$ = " Repeatedly"
   CASE 12 'script
    temp$ = scriptname$(npc(cur * 15 + i), "plotscr.lst")
   CASE 13 'script arg
    IF npc(cur * 15 + 12) = 0 THEN temp$ = " N/A"
   CASE 14 'vehicle
    IF npc(cur * 15 + 14) <= 0 THEN
     temp$ = "No"
    ELSE
     setpicstuf buffer(), 80, -1
     loadset game$ + ".veh" + CHR$(0), npc(cur * 15 + 14) - 1, 0
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
 a$ = "Appears if tag" + STR$(ABS(npc(cur * 15 + 9))) + " = " + onoroff$(npc(cur * 15 + 9)) + " and tag" + STR$(ABS(npc(cur * 15 + 10))) + " = " + onoroff$(npc(cur * 15 + 10))
 IF npc(cur * 15 + 9) <> 0 AND npc(cur * 15 + 10) = 0 THEN a$ = "Appears if tag" + STR$(ABS(npc(cur * 15 + 9))) + " = " + onoroff$(npc(cur * 15 + 9))
 IF npc(cur * 15 + 9) = 0 AND npc(cur * 15 + 10) <> 0 THEN a$ = "Appears if tag" + STR$(ABS(npc(cur * 15 + 10))) + " = " + onoroff$(npc(cur * 15 + 10))
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
 setbit general(), 106, npc(cur * 15 + 11) - 1, 0
 npc(cur * 15 + 11) = 0: RETURN
END IF

i = 0
DO
 general(105) = loopvar(general(105), 0, 999, 1)
 i = i + 1: IF i > 1000 THEN RETURN
LOOP UNTIL readbit(general(), 106, general(105)) = 0
npc(cur * 15 + 11) = general(105) + 1
setbit general(), 106, general(105), 1
RETURN

itstr:
it$ = "NONE"
IF npc(cur * 15 + 6) = 0 THEN RETURN
setpicstuf buffer(), 200, -1
loadset game$ + ".itm" + CHR$(0), npc(cur * 15 + 6) - 1, 0
it$ = ""
FOR o = 1 TO small(buffer(0), 16)
 IF buffer(o) > 255 OR buffer(o) < 0 THEN buffer(o) = 0
 it$ = it$ + CHR$(buffer(o))
NEXT o
RETURN

frstline:
x$ = ""
IF npc(cur * 15 + 4) = 0 THEN RETURN
x$ = STRING$(38, 0)
setpicstuf buffer(), 400, -1
loadset game$ + ".say" + CHR$(0), npc(cur * 15 + 4), 0
array2str buffer(), 0, x$
RETURN

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

SUB readattackdata (array(), index)

flusharray array(), 99, 0

'--load 40 elements from the .dt6 lump
setpicstuf array(), 80, -1
loadset game$ + ".dt6" + CHR$(0), index, 0

'--load the rest from the attack.bin lump (120 bytes)
size = getbinsize(0)

IF size THEN
 IF isfile(workingdir$ + "\attack.bin" + CHR$(0)) THEN
  setpicstuf buffer(), size, -1
  loadset workingdir$ + "\attack.bin" + CHR$(0), index, 0
  FOR i = 0 TO 59
   array(40 + i) = buffer(i)
  NEXT i
 END IF
END IF

END SUB

FUNCTION readattackname$ (index)

'--clobbers buffer!!!

readattackname$ = readbadgenericname$(index, game$ + ".dt6", 80, 24, 10, 1)

END FUNCTION

FUNCTION readbadgenericname$ (index, filename$, recsize, offset, size, skip)

'--clobbers buffer!

result$ = ""

IF index >= 0 THEN
 setpicstuf buffer(), recsize, -1
 loadset filename$ + CHR$(0), index, 0
 result$ = readbadbinstring$(buffer(), offset, size, skip)
END IF

readbadgenericname = result$

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
 setbit stray(), 0, i, readbit(general(), start - 1, general(start + i))
NEXT i

array2str stray(), 0, s$
s$ = LEFT$(s$, INT((lhold + 1) / 8))

END SUB

FUNCTION readshopname$ (shopnum)

'clobbers buffer!

readshopname$ = readbadgenericname$(shopnum, game$ + ".sho", 40, 0, 15, 0)

END FUNCTION

SUB setbinsize (id, size)

fh = FREEFILE
OPEN workingdir$ + "\binsize.bin" FOR BINARY AS #fh
PUT #fh, 1 + id * 2, size
CLOSE #fh

END SUB

FUNCTION small (n1, n2)
small = n1
IF n2 < n1 THEN small = n2
END FUNCTION

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
  '--all other keys
  FOR i = 2 TO 53
   IF keyval(i) > 1 AND keyv(i, shift) > 0 THEN
    s$ = s$ + CHR$(keyv(i, shift))
    EXIT FOR
   END IF
  NEXT i
 END IF
 
END IF

END SUB

SUB writeattackdata (array(), index)

setpicstuf array(), 80, -1
storeset game$ + ".dt6" + CHR$(0), index, 0

FOR i = 0 TO 59
 buffer(i) = array(40 + i)
NEXT i

're-enforce what shuld already be set (in case we are creating the file, cxu ne?)
setbinsize 0, 120

setpicstuf buffer(), 120, -1
storeset workingdir$ + "\attack.bin" + CHR$(0), index, 0

END SUB

SUB writeenemydata (buf(), index)
setpicstuf buf(), 320, -1
storeset game$ + ".dt1" + CHR$(0), index, 0
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
 LOOP UNTIL readbit(general(), start - 1, scatb) = trueb
 general(start + i) = scatb
NEXT i

FOR i = lhold + 1 TO 159
 general(start + i) = INT(RND * 4444)
NEXT i
END SUB

FUNCTION zintgrabber (n, min, max, less, more)
'--adjust for entries that are offset by +1
'--what a hack!
old = n
temp = n - 1
'--must adjust for -1 being 0
IF temp = -1 THEN
 FOR i = 2 TO 11
  IF keyval(i) > 1 THEN temp = 0
 NEXT i
END IF
dummy = intgrabber(temp, min, max, less, more)
IF temp = 0 AND keyval(14) > 1 THEN temp = -1
n = temp + 1

IF old = n THEN
 zintgrabber = 0
ELSE
 zintgrabber = 1
END IF

END FUNCTION

