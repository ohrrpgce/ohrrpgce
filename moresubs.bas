'OHRRPGCE GAME - Various unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE FUNCTION rangel% (n&, r%)
DECLARE FUNCTION str2int% (stri$)
DECLARE FUNCTION str2lng& (stri$)
DECLARE SUB innRestore (stat%())
DECLARE SUB renamehero (who%)
DECLARE FUNCTION strgrabber (s$, maxl) AS INTEGER
DECLARE SUB loadtemppage (page%)
DECLARE SUB savetemppage (page%)
DECLARE SUB calibrate ()
DECLARE FUNCTION settingstring% (searchee$, setting$, result$)
DECLARE SUB writejoysettings ()
DECLARE SUB writescriptvar (id%, newval%)
DECLARE FUNCTION readscriptvar% (id%)
DECLARE FUNCTION gethighbyte% (n%)
DECLARE SUB vishero (stat%())
DECLARE SUB sellstuff (id%, storebuf%(), stock%(), stat%())
DECLARE SUB buystuff (id%, shoptype%, storebuf%(), stock%(), stat%())
DECLARE SUB textfatalerror (e$)
DECLARE SUB playtimer ()
DECLARE FUNCTION averagelev% (stat%())
DECLARE FUNCTION countitem% (it%)
DECLARE FUNCTION movdivis% (xygo%)
DECLARE FUNCTION onwho% (w$, alone)
DECLARE SUB minimap (x%, y%, tastuf%())
DECLARE SUB heroswap (iAll%, stat%())
DECLARE FUNCTION shoption (inn%, price%, needf%, stat%())
DECLARE SUB savegame (slot%, map%, foep%, stat%(), stock%())
DECLARE FUNCTION runscript% (n%, index%, newcall%, er$, trigger%)
DECLARE SUB scripterr (e$)
DECLARE SUB itstr (i%)
DECLARE FUNCTION findhero% (who%, f%, l%, d%)
DECLARE FUNCTION howmanyh% (f%, l%)
DECLARE FUNCTION consumeitem% (index%)
DECLARE FUNCTION istag% (num%, zero%)
DECLARE SUB doswap (s%, d%, stat%())
DECLARE SUB control ()
DECLARE FUNCTION picksave% (load%)
DECLARE SUB equip (pt%, stat%())
DECLARE FUNCTION items% (stat%())
DECLARE SUB getitem (getit%, num%)
DECLARE SUB spells (pt%, stat%())
DECLARE SUB getnames (stat$())
DECLARE SUB resetlmp (slot%, lev%)
DECLARE FUNCTION battle (form%, fatal%, exstat%())
DECLARE SUB addhero (who%, slot%, stat%())
DECLARE FUNCTION atlevel% (now%, a0%, a99%)
DECLARE FUNCTION range% (n%, r%)
DECLARE SUB snapshot ()
DECLARE FUNCTION exptolevel& (level%)
DECLARE SUB deletetemps ()
DECLARE FUNCTION decodetrigger (trigger%, trigtype%)
DECLARE Sub MenuSound(byval s as integer)
DECLARE SUB freescripts (mem%)
DECLARE FUNCTION loadscript% (n%)
DECLARE SUB killallscripts ()
DECLARE SUB remove_menu (record AS INTEGER)

#include "compat.bi"
#include "allmodex.bi"
#include "common.bi"
#include "gglobals.bi"
#include "const.bi"
#include "scrconst.bi"
#include "uiconst.bi"
#include "loading.bi"

REM $STATIC
SUB addhero (who, slot, stat())
DIM wbuf(100), thishbits(4)

dim her as herodef

'--load hero's data
loadherodata @her, who - 1

'--load data of hero's default weapon
loaditemdata wbuf(), her.def_weapon

'--do average level enforcement
IF her.def_level < 0 THEN her.def_level = averagelev(stat())

'--formally add hero
hero(slot) = who

'---MUST SET DEFAULT EQUIP---
wep = large(wbuf(48), 1)
FOR i = 0 TO 4
 eqstuf(slot, i) = 0
NEXT i
eqstuf(slot, 0) = her.def_weapon + 1

'--fill in stats
FOR i = 0 TO 11
 stat(slot, 0, i) = atlevel(her.def_level, her.Lev0.sta(i), her.Lev99.sta(i)) + wbuf(54 + i)
 stat(slot, 1, i) = stat(slot, 0, i)
NEXT i
'--weapon picture and palette
stat(slot, 0, 13) = wbuf(52)
stat(slot, 1, 13) = wbuf(53)

'--weapon attack
bmenu(slot, 0) = wep

'--clear spell lists
FOR i = 1 TO 5
 bmenu(slot, i) = 0
NEXT i

'--include spell lists that have names
o = 1
FOR i = 0 TO 3
 IF her.list_name(i) <> "" THEN bmenu(slot, o) = (i + 1) * -1: o = o + 1
NEXT i

'--add item list to the end
bmenu(slot, o) = -10

'--put spells in spell list
FOR i = 0 TO 3
 FOR o = 0 TO 23
  spell(slot, i, o) = 0
  IF her.spell_lists(i,o).attack > 0 AND her.spell_lists(i,o).learned - 1 <= her.def_level AND her.spell_lists(i,o).learned > 0 THEN spell(slot, i, o) = her.spell_lists(i,o).attack
 NEXT o
NEXT i

'--elemental bitsets
FOR i = 0 TO 2
 thishbits(i) = her.bits(i)
 nativehbits(slot, i) = her.bits(i)
NEXT i

'--reset levelmp
resetlmp slot, her.def_level

'--setup experience
stat(slot, 0, 12) = her.def_level
stat(slot, 1, 12) = 0
exlev&(slot, 0) = 0
exlev&(slot, 1) = exptolevel(her.def_level)

'--heros are added unlocked
setbit hmask(), 0, who - 1, 0

'--appearance settings
' udts are self documenting
stat(slot, 0, 14) = her.sprite
stat(slot, 0, 15) = her.sprite_pal
stat(slot, 1, 14) = her.walk_sprite
stat(slot, 1, 15) = her.walk_sprite_pal
stat(slot, 0, 16) = her.def_weapon + 1'default weapon

'--read hero's name (doing this last for no real reason)
names$(slot) = her.name
'--if renaming is permitted, do it
IF readbit(thishbits(), 0, 24) THEN
 '--add-hero rename is allowed
 renamehero slot
END IF

END SUB

FUNCTION atlevel (now, a0, a99)

'CLS : a = 80: b = 8500: PRINT : FOR i = 0 TO 99 STEP 5: PRINT i; " "; atlevel(i, a, b): LINE (640, i)-(640 - atlevel(i, a, b) / 100, i), 4: NEXT i

'atlevel = (.8 + now / 50) * now * ((a99 - a0) / 100) + a0 + .1
IF now < 0 THEN atlevel = 0: EXIT FUNCTION
atlevel = (.8 + now / 50) * now * ((a99 - a0) / 275.222) + a0 + .1

END FUNCTION

FUNCTION averagelev (stat())
average = 0
count = 0
FOR i = 0 TO 3
 IF hero(i) > 0 THEN average = average + stat(i, 0, 12): count = count + 1
NEXT i
IF count > 0 THEN average = average / count
averagelev = average
END FUNCTION

SUB calibrate

state = 0
state$ = "Center Joystick and Press Button"
midx = 0
midy = 0
button = 0
disabled = 10

setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 FOR i = 0 TO 1
  IF readjoy(joy(), i) THEN EXIT FOR
 NEXT i
 SELECT CASE button
  CASE 0'no button
   IF joy(3) = 0 THEN joy(13) = 3: joy(14) = 2: button = 1
   IF joy(2) = 0 THEN joy(13) = 2: joy(14) = 3: button = 1
  CASE 1'button down
   IF joy(2) <> 0 AND joy(3) <> 0 THEN button = 2
  CASE 2
   button = 0
 END SELECT
 disabled = disabled - SGN(disabled)
 SELECT CASE state
  CASE 0
   IF (button = 2) AND (disabled = 0) THEN
    midx = joy(0)
    midy = joy(1)
    state$ = "Push UP and Press Button"
    tx = 160
    ty = 45
    state = 1
   END IF
  CASE 1
   IF button = 2 THEN
    joy(9) = joy(1) + (midy - joy(1)) * .33
    state$ = "Push DOWN and Press Button"
    ty = 155
    state = 2
   END IF
  CASE 2
   IF button = 2 THEN
    joy(10) = joy(1) - (joy(1) - midy) * .33
    state$ = "Push LEFT and Press Button"
    tx = 50
    ty = 110
    state = 3
   END IF
  CASE 3
   IF button = 2 THEN
    joy(11) = joy(0) + (midx - joy(0)) * .33
    state$ = "Push RIGHT and Press Button"
    tx = 260
    state = 4
   END IF
  CASE 4
   IF button = 2 THEN
    joy(12) = joy(0) - (joy(0) - midx) * .33
    state$ = "Press the USE button"
    state = 5
   END IF
  CASE 5
   IF button = 2 THEN
    disabled = 4
    state$ = ""
    state = 6
   END IF
  CASE 6
   IF disabled = 0 THEN
    writejoysettings
    gen(60) = 1
    EXIT DO
   END IF
 END SELECT
 centerbox 160, 100, 100, 80, 1, dpage
 centerbox 160, 100, 20, 20, 3, dpage
 IF state > 0 THEN
  centerbox 160 + (joy(0) - midx) * .1, 100 + (joy(1) - midy) * .1, 10, 10, 15, dpage
 END IF
 IF state > 0 AND state < 5 THEN
  edgeprint "This way!", tx - 36, ty - 5, uilook(uiDescription), dpage
 END IF
 edgeprint "Calibrate Joystick", 88, 8, uilook(uiText), dpage
 edgeprint state$, 160 - 4 * LEN(state$), 174, uilook(uiSelectedItem + tog), dpage
 jpos$ = "X=" + STR$(joy(0)) + " Y=" + STR$(joy(1))
 edgeprint jpos$, 160 - 4 * LEN(jpos$), 184, uilook(uiSelectedItem + tog), dpage
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

END SUB

FUNCTION consumeitem (index)
'--subtracts one of an item at a location. If the item is depleted, returns true. If there are some of the item left, it returns false
consumeitem = 0
inventory(index).num -= 1
IF inventory(index).num <= 0 THEN
 inventory(index).used = 0
 consumeitem = -1
END IF
itstr index
END FUNCTION

FUNCTION countitem (it)
total = 0
FOR o = 0 TO inventoryMax
 IF inventory(o).used AND it - 1 = inventory(o).id THEN
  total += inventory(o).num
 END IF
NEXT o
countitem = total
END FUNCTION

SUB cycletile (cycle(), tastuf(), pt(), skip())

FOR i = 0 TO 1
 IF NOT istag(tastuf(1 + 20 * i), 0) THEN
  skip(i) = large(skip(i) - 1, 0)
  IF skip(i) = 0 THEN
   notstuck = 10
   DO
    SELECT CASE tastuf(2 + 20 * i + pt(i))
     CASE 0
      IF pt(i) <> 0 THEN cycle(i) = 0
      pt(i) = 0
     CASE 1
      cycle(i) = cycle(i) - tastuf(11 + 20 * i + pt(i)) * 16
      pt(i) = loopvar(pt(i), 0, 8, 1)
     CASE 2
      cycle(i) = cycle(i) + tastuf(11 + 20 * i + pt(i)) * 16
      pt(i) = loopvar(pt(i), 0, 8, 1)
     CASE 3
      cycle(i) = cycle(i) + tastuf(11 + 20 * i + pt(i))
      pt(i) = loopvar(pt(i), 0, 8, 1)
     CASE 4
      cycle(i) = cycle(i) - tastuf(11 + 20 * i + pt(i))
      pt(i) = loopvar(pt(i), 0, 8, 1)
     CASE 5
      skip(i) = tastuf(11 + 20 * i + pt(i))
      pt(i) = loopvar(pt(i), 0, 8, 1)
     CASE 6
      IF istag(tastuf(11 + 20 * i + pt(i)), 0) THEN
       pt(i) = loopvar(pt(i), 0, 8, 1)
      ELSE
       pt(i) = 0
       cycle(i) = 0
      END IF
     CASE ELSE
      pt(i) = loopvar(pt(i), 0, 8, 1)
    END SELECT
    notstuck = large(notstuck - 1, 0)
   LOOP WHILE notstuck AND skip(i) = 0
  END IF
 END IF
NEXT i

END SUB

SUB delitem (it, amount)
FOR o = 0 TO inventoryMax
 IF inventory(o).used AND it - 1 = inventory(o).id THEN
  IF inventory(o).num <= amount THEN
   amount -= inventory(o).num
   inventory(o).used = 0
  ELSE
   inventory(o).num -= amount
   amount = 0
  END IF
  itstr o
  IF amount = 0 THEN EXIT FOR
 END IF
NEXT o
END SUB

SUB doswap (s, d, stat())

'---swap hmask (bitsets which tell which heros are locked)
a = readbit(hmask(), 0, d)
setbit hmask(), 0, d, readbit(hmask(), 0, s)
setbit hmask(), 0, s, a

'---Hero index
SWAP hero(s), hero(d)

'---Battle menu
FOR i = 0 TO 5
 SWAP bmenu(s, i), bmenu(d, i)
NEXT i

'---Spell lists
FOR i = 0 TO 3
 FOR o = 0 TO 23
  SWAP spell(s, i, o), spell(d, i, o)
 NEXT o
NEXT i

'---hero stats
FOR i = 0 TO 1
 FOR o = 0 TO 16
  SWAP stat(s, i, o), stat(d, i, o)
 NEXT o
NEXT i

'---Level-MP
FOR i = 0 TO 7
 SWAP lmp(s, i), lmp(d, i)
NEXT i

'---Experience
FOR i = 0 TO 1
 SWAP exlev&(s, i), exlev&(d, i)
NEXT i

'--name
SWAP names$(s), names$(d)

'---Equipment
FOR i = 0 TO 4
 SWAP eqstuf(s, i), eqstuf(d, i)
NEXT i

'---elemental bitsets
FOR i = 0 TO 4
 SWAP nativehbits(s, i), nativehbits(d, i)
NEXT i

'---reload hero pictures and palettes
vishero stat()

'hero(40), bmenu(40,5), spell(40,3,23), stat(40,1,13), lmp(40,7), exlev&(40,1), names$(40), eqstuf(40,4)
END SUB

SUB drawsay (saybit(), sayenh(), say$(), showsay, choose$(), choosep)
STATIC tog AS INTEGER
tog = tog XOR 1
IF readbit(saybit(), 0, 1) = 0 THEN
 IF readbit(saybit(), 0, 2) = 0 THEN
  centerfuz 160, 48 + (sayenh(0) * 4) - (sayenh(1) * 2), 312, 88 - (sayenh(1) * 4), sayenh(3) + 1, dpage
 ELSE
  centerbox 160, 48 + (sayenh(0) * 4) - (sayenh(1) * 2), 312, 88 - (sayenh(1) * 4), sayenh(3) + 1, dpage
 END IF '---TO FUZZ OR NOT TO FUZZ?-----
END IF
col = uilook(uiText): IF sayenh(2) > 0 THEN col = sayenh(2)
FOR i = 0 TO 8 - showsay
 edgeprint say$(i), 7, (8 + i * 10) + (sayenh(0) * 4), col, dpage
NEXT i

IF showsay > 1 THEN
	showsay = showsay - 1
	if showsay <= 7 then
		if trim(say$(7 - showsay)) <> "" then menusound gen(genTextboxLetter)
	end if
END IF
	
IF readbit(saybit(), 0, 0) THEN
 tempy = 100 + (sayenh(0) * 4) - (sayenh(1) * 4)
 IF tempy > 160 THEN tempy = 20
 centerbox 160, tempy + 12, 10 + large(LEN(choose$(0)) * 8, LEN(choose$(1)) * 8), 24, sayenh(3) + 1, dpage
 FOR i = 0 TO 1
  col = uilook(uiMenuItem): IF choosep = i THEN col = uilook(uiSelectedItem + tog)
  edgeprint choose$(i), xstring(choose$(i), 160), tempy + 2 + (i * 10), col, dpage
 NEXT i
END IF
END SUB

SUB evalherotag (stat())

leader = -1
FOR i = 3 TO 0 STEP -1
 IF hero(i) > 0 THEN leader = hero(i) - 1
NEXT i

FOR i = 0 TO large(gen(35), 59) '--for each available hero
 FOR j = 0 TO 3
  IF herobits(i, j) > 1 THEN setbit tag(), 0, herobits(i, j), 0
 NEXT j
 FOR j = 0 TO 40
  IF hero(j) - 1 = i THEN
   IF herobits(i, 0) > 1 THEN setbit tag(), 0, herobits(i, 0), 1 '---HAVE HERO
   IF herobits(i, 1) > 1 AND stat(j, 0, 0) THEN setbit tag(), 0, herobits(i, 1), 1 '---IS ALIVE
   IF herobits(i, 2) > 1 AND i = leader THEN setbit tag(), 0, herobits(i, 2), 1 '---IS LEADER
   IF herobits(i, 3) > 1 AND j < 4 THEN setbit tag(), 0, herobits(i, 3), 1 '---IN PARTY
  END IF
 NEXT j
NEXT i

'292     have hero tag
'293     is alive tag
'294     is leader tag
'295     is in active party tag
END SUB

SUB evalitemtag

FOR i = 0 TO 255
 'clear all four bits
 FOR j = 0 TO 3
  IF itembits(i, j) > 1 THEN setbit tag(), 0, itembits(i, j), 0
 NEXT j
NEXT i

'search inventory slots
FOR j = 0 TO inventoryMax
 'get item ID
 id = inventory(j).id
 IF inventory(j).used THEN 'there is an item in this slot
  IF itembits(id, 0) > 1 THEN setbit tag(), 0, itembits(id, 0), 1 'you have it
  IF itembits(id, 1) > 1 THEN setbit tag(), 0, itembits(id, 1), 1 'it is in your inventory
 END IF
NEXT j

FOR j = 0 TO 40 'search hero list
 FOR k = 0 TO 4 'search equipment slots
  id = eqstuf(j, k) - 1
  IF id >= 0 THEN ' there is an item equipped in this slot
   IF itembits(id, 0) > 1 THEN setbit tag(), 0, itembits(id, 0), 1 'you have it
   IF itembits(id, 2) > 1 THEN setbit tag(), 0, itembits(id, 2), 1 'it is equipped
   IF j < 4 AND itembits(id, 3) > 1 THEN setbit tag(), 0, itembits(id, 3), 1   'it is equipped by an active hero
  END IF
 NEXT k
NEXT j

'itembits(n,0)      when have tag
'itembits(n,1)      is in inventory
'itembits(n,2)      is equiped tag
'itembits(n,3)      is equiped by hero in active party

END SUB

FUNCTION findhero (who, f, l, d)
result = -1
FOR i = f TO l STEP d
 IF hero(i) = who OR (who = -1 AND hero(i)) THEN result = i: EXIT FOR
NEXT i
findhero = result
END FUNCTION

SUB getnames (stat$())
DIM bytecount AS UBYTE
IF isfile(game$ + ".stt") THEN
 fh = FREEFILE
 OPEN game$ + ".stt" FOR BINARY AS #fh
 max = 32
 FOR i = 0 TO max
  GET #fh, 1 + (11 * i), bytecount
  IF bytecount = 0 THEN
   stat$(i) = ""
  ELSE
   stat$(i) = STRING$(bytecount, CHR$(0))
   GET #fh, 2 + (11 * i), stat$(i)
  END IF
 NEXT i
 CLOSE #fh
END IF
END SUB

SUB heroswap (iAll%, stat())

'Page 2 has the npcs, which don't need to be reloaded afterward
'Page 3 holds a copy of vpage.
savetemppage 3
copypage dpage, 3

DIM swindex(40), swname$(40)

swapme = -1
ecsr = -1
acsr = 0
top = 0
la = -1
numhero = 0
high = 0
wide = 0

GOSUB resetswap

IF hero(acsr) THEN info$ = names$(acsr) ELSE info$ = ""

MenuSound gen(genAcceptSFX)
setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 IF carray(5) > 1 THEN
  IF swapme >= 0 THEN
   MenuSound gen(genCancelSFX)
   swapme = -1
  ELSE
   loadtemppage 3
   EXIT DO
  END IF
 END IF
 IF iAll THEN
  IF carray(0) > 1 THEN
   MenuSound gen(genCursorSFX)
   IF ecsr < 0 THEN
    ecsr = la
    GOSUB refreshemenu
   ELSE
    ecsr = loopvar(ecsr, -1, la, -1)
    GOSUB refreshemenu
   END IF
  END IF
  IF carray(1) > 1 THEN
   MenuSound gen(genCursorSFX)
   IF ecsr < 0 THEN
    ecsr = 0
    GOSUB refreshemenu
   ELSE
    ecsr = loopvar(ecsr, -1, la, 1)
    GOSUB refreshemenu
   END IF
  END IF
 END IF
 IF carray(2) > 1 AND ecsr < 0 THEN
  MenuSound gen(genCursorSFX)
  acsr = loopvar(acsr, 0, 3, -1)
  IF hero(acsr) AND ecsr < 0 THEN info$ = names$(acsr) ELSE info$ = ""
 END IF
 IF carray(3) > 1 AND ecsr < 0 THEN
  MenuSound gen(genCursorSFX)
  acsr = loopvar(acsr, 0, 3, 1)
  IF hero(acsr) AND ecsr < 0 THEN info$ = names$(acsr) ELSE info$ = ""
 END IF
 IF carray(4) > 1 THEN
  IF swapme = -1 THEN
   MenuSound gen(genAcceptSFX)
   IF ecsr < 0 THEN
    swapme = acsr
   ELSE
    swapme = 4 + ecsr
   END IF
  ELSE
   MenuSound gen(genAcceptSFX)
   DO
    IF swapme < 4 THEN
     IF (numhero < 2 AND ecsr = la) OR (readbit(hmask(), 0, swapme) AND ecsr > -1) THEN EXIT DO
    ELSE
     IF swapme - 4 = la AND ecsr = -1 AND numhero < 2 THEN EXIT DO
     IF readbit(hmask(), 0, acsr) AND ecsr = -1 THEN EXIT DO
    END IF
    '---IDENTIFY DESTINATION---
    IF ecsr < 0 THEN
     temp = acsr
    ELSE
     temp = swindex(ecsr)
    END IF
    '---IDENTIFY SOURCE---
    IF swapme < 4 THEN
     temp2 = swapme
    ELSE
     temp2 = swindex(swapme - 4)
    END IF
    doswap temp, temp2, stat()
    swapme = -1
    GOSUB resetswap
    EXIT DO
   LOOP
  END IF
 END IF

 GOSUB showswapmenu
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP
FOR t = 4 TO 5: carray(t) = 0: NEXT t
MenuSound gen(genCancelSFX)
EXIT SUB

refreshemenu:
IF ecsr < top THEN top = large(ecsr, 0)
IF ecsr > top + 7 THEN top = ecsr - 7
IF hero(acsr) AND ecsr < 0 THEN info$ = names$(acsr) ELSE info$ = ""
RETRACE

'---DRAWS SWAP MENU AND CURRENT SELECTION----
showswapmenu:
centerbox 160, 66, 130, 38, 1, dpage
o = 0
FOR i = 0 TO 3
 IF i = swapme OR hero(i) > 0 THEN rectangle 105 + (30 * i), 60, 20, 20, uilook(uiTextBox), dpage
 IF hero(i) THEN
  loadsprite buffer(), 0, 200 * 4, o * 5, 20, 20, 2
  drawsprite buffer(), 0, pal16(), o * 16, 105 + (30 * i), 60 + (i = swapme) * 6, dpage
  o = o + 1
 END IF
NEXT i
IF ecsr < 0 THEN edgeprint CHR$(24), 111 + 30 * acsr, 52, uilook(uiSelectedItem + tog), dpage
IF iAll THEN
 centerbox 160, 100 + small(high, 8) * 5, wide * 8 + 16, small(high, 8) * 10 + 10, 1, dpage
 FOR i = top TO small(top + 7, la)
  'Some of the colours are a bit bizarre, here, especially the time bar stuff below
  c = uilook(uiMenuItem)
  IF swapme = i + 4 THEN c = uilook(uiSelectedDisabled) '6
  IF ecsr = i THEN
   c = uilook(uiSelectedItem + tog)
   IF swapme = i + 4 THEN c = uilook(uiSelectedDisabled + tog) '6 + 8 * tog
  END IF
  IF swapme > -1 AND swapme < 4 THEN
   IF (numhero < 2 AND i = la) OR readbit(hmask(), 0, acsr) THEN c = uilook(uiTimeBar + ((ecsr = i) * tog)) '8 + ((ecsr = i) * tog)
  END IF
  edgeprint swname$(i), xstring(swname$(i), 160), 100 + (i - top) * 10, c, dpage
 NEXT i
END IF
IF LEN(info$) THEN
 centerbox 160, 44, (LEN(info$) + 2) * 8, 14, 1, dpage
 edgeprint info$, xstring(info$, 160), 39, uilook(uiText), dpage
END IF
RETRACE

'---MAPS OUT ONLY VALID SWAPABLE HEROS PLUS A BLANK-----
resetswap:
la = -1
wide = 0
FOR i = 4 TO 40
 IF readbit(hmask(), 0, i) = 0 AND hero(i) THEN
  la = la + 1
  swindex(la) = i
  swname$(la) = names$(i)
  wide = large(wide, LEN(swname$(la)))
 END IF
NEXT i
la = la + 1
FOR i = 40 TO 4 STEP -1
 IF hero(i) = 0 THEN
  swindex(la) = i
  swname$(la) = readglobalstring$(48, "-REMOVE-", 10)
  wide = large(wide, 7)
 END IF
NEXT i
high = small(8, la + 1)
numhero = 0
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  numhero = numhero + 1
 END IF
NEXT i
IF hero(acsr) AND ecsr < 0 THEN info$ = names$(acsr) ELSE info$ = ""
RETRACE
END SUB

FUNCTION howmanyh (f, l)
temp = 0
FOR i = f TO l
 IF hero(i) THEN temp = temp + 1
NEXT i
howmanyh = temp
END FUNCTION

FUNCTION istag (num, zero)
IF num = 0 THEN RETURN zero 'why go through all that just to return defaults?
IF num = 1 THEN RETURN 0
IF num = -1 THEN RETURN -1
IF ABS(num) >= UBOUND(tag) * 16 + 16 THEN RETURN zero ' use default in case of an invalid tag

ret = readbit(tag(), 0, ABS(num)) 'raw bit: 0 or -1

IF num > 0 AND ret <> 0 THEN RETURN -1
IF num < 0 AND ret = 0 THEN RETURN -1
RETURN 0
END FUNCTION

SUB loaddoor (map, door() as door)
'--clobbers buffer!
IF gen(95) < 2 THEN
 '--obsolete doors
ELSE
 '--THE RIGHT WAY--
 'setpicstuf buffer(), 600, -1
 'loadset game$ + ".dox", map, 0
 'FOR i = 0 TO 99
'  door(i) = buffer(i)
  'door(100 + i) = buffer(100 + i)
  'setbit door(), 200, i, buffer(200 + i)
 'NEXT i
 
 DeSerDoors(game$ + ".dox", door(), map)
END IF
END SUB

SUB loadgame (slot, map, foep, stat(), stock())

DIM gmaptmp(dimbinsize(4))

'--return gen to defaults
xbload game$ + ".gen", gen(), "General data is missing from " + game$

sg$ = savefile$
setpicstuf buffer(), 30000, -1
loadset sg$, slot * 2, 0

savver = buffer(0)
IF savver < 2 OR savver > 3 THEN EXIT SUB
map = buffer(1)
loadrecord gmaptmp(), game$ + ".map", getbinsize(4) / 2, map
catx(0) = buffer(2) + gmaptmp(20) * 20
caty(0) = buffer(3) + gmaptmp(21) * 20
catd(0) = buffer(4)
foep = buffer(5)
'leader = buffer(6)
mapx = buffer(7)
mapy = buffer(8)

temp$ = ""
FOR i = 0 TO 24
 IF buffer(i + 9) < 0 OR buffer(i + 9) > 255 THEN buffer(i + 9) = 0
 IF buffer(i + 9) > 0 THEN temp$ = temp$ + CHR$(buffer(i + 9))
NEXT i
gold& = str2lng&(temp$)

z = 34
FOR i = 0 TO 500
 SELECT CASE i
  'Only certain gen() values should be read from the saved game.
  'See http://HamsterRepublic.com/ohrrpgce/index.php/GEN.html
  CASE 42, 44 TO 54, 57 TO 76
   gen(i) = buffer(z)
 END SELECT
 z = z + 1
NEXT i

DeserNPCL npc(),z,buffer(),300,gmaptmp(20),gmaptmp(21)
z=z+1 'fix an old bug

FOR i = 0 TO 126
 tag(i) = buffer(z): z = z + 1
NEXT i
FOR i = 0 TO 40
 hero(i) = buffer(z): z = z + 1
NEXT i
FOR i = 0 TO 500
 '--used to be the useless a() buffer
 dummy = buffer(z): z = z + 1
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 1
  FOR j = 0 TO 13
   stat(i, o, j) = buffer(z): z = z + 1
  NEXT j
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 5
  bmenu(i, o) = buffer(z): z = z + 1
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 3
  FOR j = 0 TO 23
   spell(i, o, j) = buffer(z): z = z + 1
  NEXT j
  z = z + 1'--skip extra data
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 7
  lmp(i, o) = buffer(z): z = z + 1
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 1
  temp$ = ""
  FOR j = 0 TO 25
   IF buffer(z) < 0 OR buffer(z) > 255 THEN buffer(z) = 0
   IF buffer(z) > 0 THEN temp$ = temp$ + CHR$(buffer(z))
   z = z + 1
  NEXT j
  exlev&(i, o) = str2lng&(temp$)
 NEXT o
NEXT i
FOR i = 0 TO 40
 temp$ = ""
 FOR j = 0 TO 16
  IF buffer(z) < 0 OR buffer(z) > 255 THEN buffer(z) = 0
  IF buffer(z) > 0 THEN temp$ = temp$ + CHR$(buffer(z))
  z = z + 1
 NEXT j
 names$(i) = temp$
NEXT i

DeserInventory inventory(), z, buffer()

FOR i = 0 TO 40
 FOR o = 0 TO 4
  eqstuf(i, o) = buffer(z): z = z + 1
 NEXT o
NEXT i

'RECORD 2

setpicstuf buffer(), 30000, -1
loadset sg$, slot * 2 + 1, 0

z = 0

FOR i = 0 TO 99
 FOR o = 0 TO 49
  stock(i, o) = buffer(z): z = z + 1
 NEXT o
NEXT i
FOR i = 0 TO 3
 hmask(i) = buffer(z): z = z + 1
NEXT i
FOR i = 1 TO 3
 catx(i * 5) = buffer(z) + gmaptmp(20) * 20: z = z + 1
 caty(i * 5) = buffer(z) + gmaptmp(21) * 20: z = z + 1
 catd(i * 5) = buffer(z): z = z + 1
NEXT i
FOR i = 0 TO 1024
 global(i) = buffer(z): z = z + 1
NEXT i
FOR i = 0 TO 21
 veh(i) = buffer(z): z = z + 1
NEXT i
'--picture and palette
picpalmagicnum = buffer(z): z = z + 1
FOR i = 0 TO 40
 FOR o = 0 TO 1
  FOR j = 14 TO 16
   IF picpalmagicnum = 4444 THEN stat(i, o, j) = buffer(z)
   z = z + 1
  NEXT j
 NEXT o
NEXT i
'native hero bitsets
nativebitmagicnum = buffer(z): z = z + 1
FOR i = 0 TO 40
 FOR o = 0 TO 4
  IF nativebitmagicnum = 4444 THEN nativehbits(i, o) = buffer(z)
  z = z + 1
 NEXT o
NEXT i
'top global variable bits
FOR i = 0 TO 1024
 global(i) or= buffer(z) shl 16: z = z + 1
NEXT i

'---BLOODY BACKWARD COMPATABILITY---
'fix doors...
IF savver = 2 THEN gen(95) = 3

dim her as herodef

IF picpalmagicnum <> 4444 THEN
 '--fix appearance settings
 FOR sl = 0 TO 40
  IF hero(sl) > 0 THEN
   loadherodata @her, hero(sl) - 1
   stat(sl, 0, 14) = her.sprite
   stat(sl, 0, 15) = her.sprite_pal
   stat(sl, 1, 14) = her.walk_sprite
   stat(sl, 1, 15) = her.walk_sprite_pal
   stat(sl, 0, 16) = her.def_weapon + 1'default weapon
  END IF
 NEXT sl
END IF

IF nativebitmagicnum <> 4444 THEN
 '--fix native hero bits
 FOR sl = 0 TO 40
  IF hero(sl) > 0 THEN
   loadherodata @her, hero(sl) - 1
   FOR i = 0 TO 2 '??
    nativehbits(sl, i) = her.bits(i)
   NEXT i
  END IF
 NEXT sl
END IF

'ALL THE STUFF THAT MUST BE SAVED
'map,x,y,d,foep,gold&,gen(500),npcl(2100),tag(126),hero(40),stat(40,1,13),bmenu(40,5),spell(40,3,23),lmp(40,7),exlev&(40,1),names$(40),item(-3 to 199),item$(-3 to 199),eqstuf(40,4)
'ALL THE STUFF THAT MUST BE PASSED
'slot,map,x,y,d,foep,gold&,stat(),bmenu(),spell(),lmp(),exlev&(),item(),item$()
'30000
END SUB

SUB loadglobalvars (slot, first, last)
DIM buf(last - first)
IF isfile$(savefile$) THEN
 fh = FREEFILE
 OPEN savefile$ FOR BINARY AS #fh
 SEEK #fh, 60000 * slot + 2 * first + 40027  '30000 + 5013 * 2 + 1
 loadrecord buf(), fh, last - first + 1, -1
 FOR i = 0 TO last - first
  global(first + i) = buf(i)
 NEXT
 SEEK #fh, 60000 * slot + 2 * first + 43027  '30000 + 6513 * 2 + 1
 loadrecord buf(), fh, last - first + 1, -1
 FOR i = 0 TO last - first
  global(first + i) or= buf(i) shl 16
 NEXT
 CLOSE #fh
ELSE
 FOR i = 0 TO last - first
  global(first + i) = 0
 NEXT
END IF
END SUB

SUB minimap (x, y, tastuf())

centerfuz 160, 100, 304, 184, 1, vpage
centerbox 159, 99, scroll(0) + 3, scroll(1) + 3, 15, vpage
setmapdata scroll(), buffer(), 0, 0
zoom = 1
IF scroll(0) < 160 AND scroll(1) < 100 THEN zoom = 2
IF scroll(0) < 106 AND scroll(1) < 66 THEN zoom = 3
IF scroll(0) < 80 AND scroll(1) < 50 THEN zoom = 4
IF scroll(0) < 64 AND scroll(1) < 40 THEN zoom = 5
topcorner = 160 - INT(scroll(0) * .5) * zoom
leftcorner = 100 - INT(scroll(1) * .5) * zoom
FOR i = 0 TO scroll(1) - 1
 FOR o = 0 TO scroll(0) - 1
  block = readmapblock(o, i, 0)
  'ignore tile animation
  IF block > 207 THEN block = (block - 207) + tastuf(20)
  IF block > 159 THEN block = (block - 159) + tastuf(0)
  mx = block - (INT(block / 16) * 16)
  my = INT(block / 16)
  '--larger minimap for smaller maps
  FOR zx = 0 to zoom - 1
   FOR zy = 0 to zoom - 1
    subtile = 20 \ zoom
    pixel = readpixel((mx*20) + (zx*subtile) + INT(RND*subtile), (my*20) + (zy*subtile) + INT(RND*subtile), 3)
    putpixel topcorner + o * zoom + zx, leftcorner + i * zoom + zy, pixel, vpage
   NEXT zy
  NEXT zx
 NEXT
NEXT
MenuSound gen(genAcceptSFX)
copypage vpage, dpage
setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 IF carray(4) > 1 OR carray(5) > 1 THEN EXIT DO
 FOR i = 1 TO 99
  IF keyval(i) > 1 THEN EXIT DO
 NEXT i
 rectangle topcorner + (x / 20) * zoom, leftcorner + (y / 20) * zoom, zoom, zoom, uilook(uiSelectedItem) * tog, dpage
 copypage dpage, vpage
 setvispage vpage
 dowait
LOOP
setkeys
FOR i = 0 TO 7: carray(i) = 0: NEXT i
MenuSound gen(genCancelSFX)
END SUB

FUNCTION movdivis (xygo)
IF (xygo \ 20) * 20 = xygo AND xygo <> 0 THEN
 movdivis = -1
ELSE
 movdivis = 0
END IF
END FUNCTION

FUNCTION onwho (w$, alone)

'-- pre-select the first hero
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  w = i
  EXIT FOR
 END IF
NEXT i

'-- if there is only one hero, return immediately
'--unless we are in alone-mode
IF alone = 0 AND howmanyh(0, 3) <= 1 THEN onwho = w: setkeys: EXIT FUNCTION

menusound gen(genAcceptSFX)
copypage dpage, vpage
setvispage vpage
setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 wtg = loopvar(wtg, 0, 3, 1)
 IF carray(5) > 1 THEN
  onwho = -1
  menusound gen(genCancelSFX)
  EXIT DO
 END IF
 IF carray(2) > 1 THEN
  DO: w = loopvar(w, 0, 3, -1): LOOP UNTIL hero(w) > 0
  menusound gen(genCursorSFX)
 END IF
 IF carray(3) > 1 THEN
  DO: w = loopvar(w, 0, 3, 1): LOOP UNTIL hero(w) > 0
  menusound gen(genCursorSFX)
 END IF
 IF carray(4) > 1 THEN onwho = w: EXIT DO
 centerbox 160, 100, 140, 52, 1, dpage
 o = 0
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN
   wt = 0: IF w = i THEN wt = INT(wtg / 2)
   loadsprite buffer(), 0, 200 * ((2 * 2) + wt), o * 5, 20, 20, 2
   drawsprite buffer(), 0, pal16(), o * 16, 100 + i * 30, 100, dpage
   o = o + 1
  END IF
 NEXT i
 edgeprint CHR$(25), 106 + w * 30, 90, uilook(uiSelectedItem + tog), dpage
 edgeprint w$, xstring(w$, 160), 80, uilook(uiText), dpage
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP

setkeys
flusharray carray(), 7, 0

END FUNCTION

FUNCTION range (n, r)
a = (n / 100) * r
range = n + INT(RND * (a * 2)) - a
END FUNCTION

FUNCTION rangel (n&, r)
a = (n& / 100) * r
rangel = n& + INT(RND * (a * 2)) - a
END FUNCTION

SUB readjoysettings

IF isfile(exepath$ + SLASH + "joyset.ini") THEN
 '--use joyset.ini
 fh = FREEFILE
 OPEN exepath$ + SLASH + "joyset.ini" FOR INPUT AS #fh
 safety = 0
 n$ = ""
 DO WHILE NOT EOF(fh) AND safety < 100
  LINE INPUT #fh, a$
  IF settingstring(a$, "UPTHRESH", n$) THEN
   joy(9) = str2int(n$)
  END IF
  IF settingstring(a$, "DOWNTHRESH", n$) THEN
   joy(10) = str2int(n$)
  END IF
  IF settingstring(a$, "LEFTTHRESH", n$) THEN
   joy(11) = str2int(n$)
  END IF
  IF settingstring(a$, "RIGHTTHRESH", n$) THEN
   joy(12) = str2int(n$)
  END IF
  IF settingstring(a$, "USEBUTTON", n$) THEN
   joy(13) = bound(str2int(n$) + 2, 2, 3)
  END IF
  IF settingstring(a$, "MENUBUTTON", n$) THEN
   joy(14) = bound(str2int(n$) + 2, 2, 3)
  END IF
  safety = safety + 1
 LOOP
 CLOSE #fh
 '--wait a little to make sure the buttons clear
 setwait timing(), speedcontrol
 dowait
 gen(60) = 1
ELSE
 '--no joyset.ini file, must recalibrate
 'calibrate
END IF

END SUB

FUNCTION readscriptvar (id)

SELECT CASE id
 CASE IS < 0 'local variable
  readscriptvar = heap(scrat(nowscript).heap + ABS(id) - 1)
 CASE 0 TO 1024 'global variable
  readscriptvar = global(id)
 CASE ELSE
  scripterr "Cannot read global" + XSTR$(id) + ". out of range"
END SELECT

END FUNCTION

SUB renamehero (who)

dim her as herodef
loadherodata @her, hero(who) - 1

limit = her.max_name_len
IF limit = 0 THEN limit = 16

prompt$ = readglobalstring$(137, "Name the Hero", 20)
spacer$ = STRING$(large(limit, LEN(names$(who))), " ")
remember$ = names$(who)
rememberjoycal = gen(60)
gen(60) = 1'--disable joystick calibration

copypage dpage, vpage
setvispage vpage
IF fadestate = 0 THEN
 fadein
 needfadeout = 1
END IF

setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 playtimer
 control
 centerbox 160, 100, 168, 32, 1, dpage
 IF carray(4) > 1 AND keyval(57) = 0 THEN EXIT DO
 IF carray(5) > 1 THEN names$(who) = remember$
 strgrabber names$(who), limit
 edgeprint prompt$, xstring(prompt$, 160), 90, uilook(uiText), dpage
 textcolor uilook(uiHighlight), uiLook(uiHighlight)
 printstr spacer$, xstring(spacer$, 161), 101, dpage
 edgeprint names$(who), xstring(names$(who), 160), 100, uilook(uiMenuItem), dpage
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP

gen(60) = rememberjoycal '-- restore joystick calibration setting

IF needfadeout = 1 THEN
 fadeout 0, 0, 0
END IF

END SUB

SUB resetgame (map, foep, stat(), stock(), showsay, scriptout$, sayenh())
map = 0
catx(0) = 0
caty(0) = 0
catd(0) = 0
foep = 0
'leader = 0
mapx = 0
mapy = 0
gold& = 0
showsay = 0
scriptout$ = ""
'--return gen to defaults
xbload game$ + ".gen", gen(), "General data is missing from " + game$

'flusharray npcl(), 2100, 0
CleanNPCL npc(),300
flusharray tag(), 126, 0
flusharray hero(), 40, 0
FOR i = 0 TO 40
 FOR o = 0 TO 1
  FOR j = 0 TO 16
   stat(i, o, j) = 0
  NEXT j
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 5
  bmenu(i, o) = 0
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 3
  FOR j = 0 TO 23
   spell(i, o, j) = 0
  NEXT j
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 7
  lmp(i, o) = 0
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 1
  exlev&(i, o) = 0
 NEXT o
NEXT i
FOR i = 0 TO 40
 names$(i) = ""
NEXT i
CleanInventory(inventory())
FOR i = 0 TO 40
 FOR o = 0 TO 4
  eqstuf(i, o) = 0
 NEXT o
NEXT i

'RECORD 2 (applies only to saves)

FOR i = 0 TO 99
 FOR o = 0 TO 49
  stock(i, o) = 0
 NEXT o
NEXT i
flusharray hmask(), 3, 0
flusharray global(), 1024, 0
flusharray veh(), 21, 0
flusharray sayenh(), 6, 0

FOR i = 0 TO 31
 with plotstr(i)
  .s = ""
  .col = 15
  .BGCol = 0
  .X = 0
  .Y = 0
  .Bits = 0
 end with
NEXT i

loadpalette master(), gen(genMasterPal)

FOR i = topmenu TO 0 STEP -1
 remove_menu i
NEXT i

'delete temp files that are part of the game state
deletetemps

killallscripts

'ALL THE STUFF THAT MUST BE RESET
'map,foep,gold&,gen(500),npcl(2100),tag(126),hero(40),stat(40,1,13),bmenu(40,5),spell(40,3,23),lmp(40,7),exlev&(40,1),names$(40),item(-3 to 199),item$(-3 to 199),eqstuf(40,4)
'30000
END SUB

SUB resetlmp (slot, lev)
FOR i = 0 TO 7
 lmp(slot, i) = 0
NEXT i
o = 0: j = 0
FOR i = 0 TO lev
 lmp(slot, o) = lmp(slot, o) + 1
 o = o + 1
 IF o > j THEN o = 0: j = j + 1
 IF j > 7 THEN j = 0
NEXT i
END SUB

SUB rpgversion (v)
current = 6
'last added midi music, change shop stuff and song name formats

IF v >= 5 AND v <= current THEN EXIT SUB
needf = 1
clearpage 0
clearpage 1
setvispage 0
centerbox 160, 100, 240, 100, 3, 0
IF v < 5 THEN
 ' Versions older than 5 do not support graceful backwards compatability
 edgeprint "Obsolete RPG File", 52, 70, uilook(uiSelectedItem), 0
 textcolor uilook(uiMenuItem), 0
 printstr "this game was created with", 52, 82, 0
 printstr "an obsolete version of the", 52, 90, 0
 printstr "OHRRPGCE. It may not run", 52, 98, 0
 printstr "as intended.", 52, 106, 0
END IF
IF v > current THEN
 'Versions newer than current cannot support graceful forward compatability
 edgeprint "Unsupported RPG File", 52, 70, uilook(uiText), 0
 textcolor uilook(uiMenuItem), 0
 printstr "this game has features", 52, 82, 0
 printstr "that are not supported in", 52, 90, 0
 printstr "this version of the", 52, 98, 0
 printstr "OHRRPGCE. Download the", 52, 106, 0
 printstr "latest version at", 52, 114, 0
 printstr "http://HamsterRepublic.com", 52, 122, 0
END IF
fadein
setvispage 0
w = getkey
fadeout 0, 0, 0
END SUB

FUNCTION runscript (id, index, newcall, er$, trigger)

IF trigger <> 0 THEN n = decodetrigger(id, trigger) ELSE n = id

IF n = 0 THEN
 runscript = 2 '--quiet failure (though decodetrigger might have shown a scripterr)
 EXIT FUNCTION
END IF

IF index > 127 THEN
 scripterr "interpreter overloaded"
 runscript = 0 '--error
 scripterr "failed to load " + er$ + " script " & n
 EXIT FUNCTION
END IF

IF newcall AND index > 0 THEN
 IF n = scrat(index - 1).id AND readbit(gen(), 101, 10) = 0 THEN
  'fail quietly
  '--scripterr "script " & n & " is already running"
  runscript = 2 '--quiet failure
  EXIT FUNCTION
 END IF
END IF

WITH scrat(index)
 '-- Load the script (or return the reference if already loaded)
 .scrnum = loadscript(n)
 IF .scrnum = -1 THEN
  '--failed to load
  runscript = 0'--error
  scripterr "Failed to load " + er$ + " script " & n
  EXIT FUNCTION
 END IF
 script(.scrnum).totaluse += 1
 scriptctr += 1
 script(.scrnum).lastuse = scriptctr
 'increment refcount once loading is successful

 'erase state, pointer, return value and depth, set id
 .state = stread
 .ptr = 0
 .ret = 0
 .depth = 0
 .id = n
 
 scrat(index + 1).heap = .heap + script(.scrnum).vars

 IF scrat(index + 1).heap > 2048 THEN
  scripterr "Script heap overflow"
  runscript = 0'--error
  scripterr "failed to load " + er$ + " script " & n
  EXIT FUNCTION
 END IF

 FOR i = 0 TO script(.scrnum).vars - 1
  heap(.heap + i) = 0
 NEXT i

 '--suspend the previous script...Why was I doing this?
 IF newcall AND index > 0 THEN
  scrat(index - 1).state *= -1
 END IF

 '--we are successful, so now its safe to increment this
 script(.scrnum).refcount += 1
 nowscript += 1

 'debug scriptname$(.id) & " in script(" & .scrnum & "): totaluse = " & script(.scrnum).totaluse & " refc = " & script(.scrnum).refcount & " lastuse = " & script(.scrnum).lastuse
END WITH

RETURN 1 '--success

END FUNCTION

FUNCTION loadscript (n)
 DIM thisscr as ScriptData
 DIM temp as short

 '-- Check if this script has already been loaded into memory
 FOR i = 0 TO 127
  IF script(i).id = n THEN RETURN i
 NEXT

 '--load the script from file
 IF isfile(tmpdir$ & n & ".hsz") THEN
  scriptfile$ = tmpdir$ & n & ".hsz"
 ELSEIF isfile(tmpdir$ & n & ".hsx") THEN
  scriptfile$ = tmpdir$ & n & ".hsx"
 END IF

 IF NOT isfile(scriptfile$) THEN
  scripterr "script id " & n & " does not exist"
  RETURN -1
 END IF

 '--may have to free some loaded scripts before we have a free script() slot
 WITH thisscr

  f = FREEFILE
  OPEN scriptfile$ FOR BINARY AS #f

  GET #f, 1, temp
  skip = temp

  GET #f, 3, temp
  'some HSX files seem to have an illegal negative number of variables
  .vars = temp
  .vars = large(.vars, 0)
 
  IF skip >= 6 THEN
   GET #f, 5, temp
   .args = temp
  ELSE
   .args = 999
  END IF

  IF skip >= 8 THEN
   GET #f, 7, temp
   scrformat = temp
  ELSE
   scrformat = 0
  END IF
  IF scrformat > 2 THEN
   scripterr "script" + XSTR$(n) + " is in an unsupported format"
   CLOSE #f
   RETURN -1
  END IF
  IF scrformat >= 1 THEN wordsize = 4 ELSE wordsize = 2

  IF skip >= 12 THEN
   GET #f, 9, .strtable
   .strtable = (.strtable - skip) \ wordsize
  ELSEIF skip = 10 THEN
   GET #f, 9, temp
   .strtable = (temp - skip) \ wordsize
  ELSE
   .strtable = 0
  END IF

  'set an arbitrary max script buffer size (scriptmemMax in const.bi), individual scripts must also obey
  .size = (LOF(f) - skip) \ wordsize
  IF .size > scriptmemMax THEN
   scripterr "Script " & n & " exceeds maximum size"
   CLOSE #f
   RETURN -1
  END IF

  IF .size + totalscrmem > scriptmemMax OR numloadedscr = 128 THEN
   'debug "opps! size = " & .size & " totalscrmem = " & totalscrmem & ", calling freescripts"
   freescripts(scriptmemMax - .size)
  END IF

  .ptr = allocate(.size * sizeof(integer))
  IF .ptr = 0 THEN
   scripterr "Could not allocate memory to load script"
   CLOSE #f
   RETURN -1
  END IF

  IF wordsize = 2 THEN
   FOR i = skip TO LOF(f) - wordsize STEP wordsize
    GET #f, 1 + i, temp
    .ptr[(i - skip) \ 2] = temp
   NEXT
  ELSE
   GET #f, skip + 1, *.ptr, .size
  END IF
  CLOSE #f

  .id = n
  .refcount = 0
  .totaluse = 0
  .lastuse = 0
  numloadedscr += 1
  totalscrmem += .size
 END WITH

 '--copy to an empty script() slot
 FOR i = 0 TO 127
  IF script(i).id = 0 THEN EXIT FOR
 NEXT

 'macro disabled for fb 0.15 compat
 'copyobj(script(i), thisscr)
 memcpy(@script(i),@thisscr,LEN(thisscr))
 RETURN i
END FUNCTION

SUB freescripts (mem)
'frees loaded scripts until totalscrmem <= mem (measured in 4-byte ints) (probably alot lower)
'always frees at least one script to provide an opening in script()
'call freescripts(0) to cleanup all scripts

'give each script a score (the lower, the more likely to throw) and sort them
'this is roughly a least recently used list
DIM LRUlist(127, 1)
listtail = -1

FOR i = 0 TO 127
 WITH script(i)
  IF .id THEN
   score = .lastuse + iif(.totaluse < 200, .totaluse, 200) - .size \ 256
   FOR j = listtail TO 0 STEP -1
    IF score >= LRUlist(j, 1) THEN EXIT FOR
    LRUlist(j + 1, 0) = LRUlist(j, 0)
    LRUlist(j + 1, 1) = LRUlist(j, 1)
   NEXT
   LRUlist(j + 1, 0) = i
   LRUlist(j + 1, 1) = score
   listtail += 1
  END IF
 END WITH
 IF listtail + 1 = numloadedscr THEN EXIT FOR
NEXT

'debug "listing scripts:"
'FOR i = 0 TO listtail
' debug i & ": script(" & LRUlist(i, 0) & ") score = " & LRUlist(i, 1)
' WITH script(LRUlist(i, 0))
'  debug "id = " & .id & " " & scriptname$(.id)
'  debug "refcount = " & .refcount
'  debug "totaluse = " & .totaluse
'  debug "lastuse = " & .lastuse
'  debug "size = " & .size
' END WITH
'NEXT


targetmem = mem
IF mem > scriptmemMax \ 2 THEN targetmem = mem * (1 - 0.5 * (mem - scriptmemMax \ 2) / scriptmemMax)

'debug "mem = " & mem & " target = " & targetmem

FOR i = 0 TO listtail
 WITH script(LRUlist(i, 0))
  totalscrmem -= .size
  numloadedscr -= 1
  deallocate(.ptr)
  .id = 0
  IF .refcount THEN
   FOR j = 0 TO nowscript
    IF scrat(j).scrnum = LRUlist(i, 0) THEN scrat(j).scrnum = -1
   NEXT
  END IF
  IF totalscrmem <= targetmem THEN EXIT SUB
 END WITH
NEXT

END SUB

SUB savegame (slot, map, foep, stat(), stock())

DIM gmaptmp(dimbinsize(4))

'--FLUSH BUFFER---
FOR i = 0 TO 16000
 buffer(i) = 0
NEXT i

buffer(0) = 3        'SAVEGAME VERSION NUMBER
buffer(1) = map
loadrecord gmaptmp(), game$ + ".map", getbinsize(4) / 2, map
buffer(2) = catx(0) - gmaptmp(20) * 20
buffer(3) = caty(0) - gmaptmp(21) * 20
buffer(4) = catd(0)
buffer(5) = foep
buffer(6) = 0    'was leader
buffer(7) = mapx
buffer(8) = mapy

temp$ = XSTR$(gold&)
FOR i = 0 TO 24
 IF i < LEN(temp$) THEN
  IF MID$(temp$, i + 1, 1) <> "" THEN buffer(i + 9) = ASC(MID$(temp$, i + 1, 1))
 ELSE
  buffer(i + 9) = 0
 END IF
NEXT i

z = 34
FOR i = 0 TO 500
 IF i <= 104 THEN
  buffer(z) = gen(i)
 ELSE
  buffer(z) = 0
 END IF
 z = z + 1
NEXT i
' FOR i = 0 TO 2100
'  buffer(z) = npcl(i): z = z + 1
' NEXT i
SerNPCL npc(), z, buffer(), 300, gmaptmp(20), gmaptmp(21)
z=z+1 'fix an old bug
FOR i = 0 TO 126
 buffer(z) = tag(i): z = z + 1
NEXT i
FOR i = 0 TO 40
 buffer(z) = hero(i): z = z + 1
NEXT i
FOR i = 0 TO 500
 '--placeholder for old useless a() buffer
 buffer(z) = 0: z = z + 1
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 1
  FOR j = 0 TO 13
   buffer(z) = stat(i, o, j): z = z + 1
  NEXT j
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 5
  buffer(z) = bmenu(i, o): z = z + 1
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 3
  FOR j = 0 TO 23
   buffer(z) = spell(i, o, j): z = z + 1
  NEXT j
  z = z + 1
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 7
  buffer(z) = lmp(i, o): z = z + 1
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 1
  temp$ = XSTR$(exlev&(i, o))
  FOR j = 0 TO 25
   IF j < LEN(temp$) THEN
    IF MID$(temp$, j + 1, 1) <> "" THEN buffer(z) = ASC(MID$(temp$, j + 1, 1))
   ELSE
    buffer(z) = 0
   END IF
   z = z + 1
  NEXT j
 NEXT o
NEXT i
FOR i = 0 TO 40
 temp$ = names$(i)
 FOR j = 0 TO 16
  IF j < LEN(temp$) THEN
   IF MID$(temp$, j + 1, 1) <> "" THEN buffer(z) = ASC(MID$(temp$, j + 1, 1))
  END IF
  z = z + 1
 NEXT j
NEXT i
SerInventory inventory(), z, buffer()
FOR i = 0 TO 40
 FOR o = 0 TO 4
  buffer(z) = eqstuf(i, o): z = z + 1
 NEXT o
NEXT i

setpicstuf buffer(), 30000, -1
sg$ = savefile$
storeset sg$, slot * 2, 0

'---RECORD 2

'--FLUSH BUFFER---
FOR i = 0 TO 16000
 buffer(i) = 0
NEXT i

z = 0

FOR i = 0 TO 99
 FOR o = 0 TO 49
  buffer(z) = stock(i, o): z = z + 1
 NEXT o
NEXT i
FOR i = 0 TO 3
 buffer(z) = hmask(i): z = z + 1
NEXT i
FOR i = 1 TO 3
 buffer(z) = catx(i * 5) - gmaptmp(20) * 20: z = z + 1
 buffer(z) = caty(i * 5) - gmaptmp(21) * 20: z = z + 1
 buffer(z) = catd(i * 5): z = z + 1
NEXT i
'--bottom 16 bits of each global variable in 5013 - 6037
FOR i = 0 TO 1024
 buffer(z) = global(i): z = z + 1
NEXT i
FOR i = 0 TO 21
 buffer(z) = veh(i): z = z + 1
NEXT i
'--picture and palette
buffer(z) = 4444: z = z + 1 'magic number
FOR i = 0 TO 40
 FOR o = 0 TO 1
  FOR j = 14 TO 16
   buffer(z) = stat(i, o, j): z = z + 1
  NEXT j
 NEXT o
NEXT i
'--native hero bitsets
buffer(z) = 4444: z = z + 1 'magic number
FOR i = 0 TO 40
 FOR o = 0 TO 4
  buffer(z) = nativehbits(i, o): z = z + 1
 NEXT o
NEXT i
'--top 16 bits of each global variable in 6513 - 7539
FOR i = 0 TO 1024
 buffer(z) = global(i) shr 16: z = z + 1
NEXT

' z = 7540 here

setpicstuf buffer(), 30000, -1
sg$ = savefile$
storeset sg$, slot * 2 + 1, 0


'ALL THE STUFF THAT MUST BE SAVED
'map,x,y,d,foep,gold&,gen(500),npcl(2100),tag(126),hero(40),stat(40,1,13),bmenu(40,5),spell(40,3,23),lmp(40,7),exlev&(40,1),names$(40),item(-3 to 199),item$(-3 to 199),eqstuf(40,4)
'ALL THE STUFF THAT MUST BE PASSED
'slot,map,x,y,d,foep,gold&,stat(),bmenu(),spell(),lmp(),exlev&(),item(),item$()
'30000
END SUB

SUB saveglobalvars (slot, first, last)
DIM buf(last - first)
fh = FREEFILE
OPEN savefile$ FOR BINARY AS #fh
SEEK #fh, 60000 * slot + 2 * first + 40027  '30000 + 5013 * 2 + 1
FOR i = 0 TO last - first
 buf(i) = global(first + i)
NEXT
storerecord buf(), fh, last - first + 1, -1
SEEK #fh, 60000 * slot + 2 * first + 43027  '30000 + 6513 * 2 + 1
FOR i = 0 TO last - first
 buf(i) = global(first + i) shr 16
NEXT
storerecord buf(), fh, last - first + 1, -1
CLOSE #fh
END SUB

SUB scripterr (e$)

'errormode = 1

'SELECT CASE errormode
' CASE 1'--show error on screen
  debug "Scripterr: " & e$

  textcolor uilook(uiText), 0
  clearpage vpage
  setpal master()
  centerbox 160, 20, 310, 30, 3, vpage
  printstr "Script Error!", 108, 10, vpage
  printstr e$, 160 - 4 * LEN(e$), 20, vpage
  setvispage vpage
  w = getkey
' CASE 2'--write error to file
'  debug e$
'END SELECT

END SUB

SUB scriptmath
SELECT CASE scrat(nowscript).curvalue
 CASE 0' random
  lowest& = retvals(0)
  highest& = retvals(1)
  scriptret = retvals(0) + INT(RND * (highest& - lowest& + 1))
 CASE 1' exponent
  scriptret = retvals(0) ^ retvals(1)
 CASE 2' modulus
  IF retvals(1) = 0 THEN
   scripterr "division by zero"
  ELSE
   scriptret = retvals(0) MOD retvals(1)
  END IF
 CASE 3' divide
  IF retvals(1) = 0 THEN
   scripterr "division by zero"
  ELSE
   scriptret = retvals(0) \ retvals(1)
  END IF
 CASE 4'multiply
  scriptret = retvals(0) * retvals(1)
 CASE 5'subtract
  scriptret = retvals(0) - retvals(1)
 CASE 6'add
  scriptret = retvals(0) + retvals(1)
 CASE 7'xor
  scriptret = retvals(0) XOR retvals(1)
 CASE 8'or
  scriptret = retvals(0) OR retvals(1)
 CASE 9'and
  scriptret = retvals(0) AND retvals(1)
 CASE 10'equal
  scriptret = (retvals(0) = retvals(1)) * -1
 CASE 11'not equal
  scriptret = (retvals(0) <> retvals(1)) * -1
 CASE 12'less than
  scriptret = (retvals(0) < retvals(1)) * -1
 CASE 13'greater than
  scriptret = (retvals(0) > retvals(1)) * -1
 CASE 14'less than or equal to
  scriptret = (retvals(0) <= retvals(1)) * -1
 CASE 15'greater than or equal to
  scriptret = (retvals(0) >= retvals(1)) * -1
 CASE 16'set variable
  writescriptvar retvals(0), retvals(1)
 CASE 17'increment
  writescriptvar retvals(0), readscriptvar(retvals(0)) + retvals(1)
 CASE 18'decrement
  writescriptvar retvals(0), readscriptvar(retvals(0)) - retvals(1)
 CASE 19'not
  IF retvals(0) = 0 THEN
   scriptret = 1
  ELSE
   scriptret = 0
  END IF
 CASE 20'&&
  IF retvals(1) <> 0 THEN scriptret = 1 ELSE scriptret = 0
 CASE 21'||
  IF retvals(1) <> 0 THEN scriptret = 1 ELSE scriptret = 0
 CASE 22'^^
  IF retvals(0) <> 0 XOR retvals(1) <> 0 THEN scriptret = 1 ELSE scriptret = 0
 CASE ELSE
  scripterr "unsupported math"
END SELECT
END SUB

FUNCTION settingstring (searchee$, setting$, result$)

' checks to see if searchee$ begins with setting$=
' if so, sets result$ to the uppercased space-trimmed value that
' follows the = sign and returns true. If not found, returns false

settingstring = 0

IF UCASE$(LEFT$(searchee$, LEN(setting$) + 1)) = setting$ + "=" THEN
 result$ = UCASE$(LTRIM$(RTRIM$(MID$(searchee$, LEN(setting$) + 2, 32))))
 settingstring = -1
END IF

END FUNCTION

SUB shop (id, needf, stock(), stat(), map, foep, tastuf())

DIM storebuf(40), menu$(10), menuid(10)

FOR i = 0 TO 7
 menuid(i) = i
NEXT i

menu$(0) = readglobalstring$(70, "Buy", 10)
menu$(1) = readglobalstring$(71, "Sell", 10)
menu$(2) = readglobalstring$(73, "Hire", 10)
menu$(3) = readglobalstring$(72, "Inn", 10)
menu$(4) = readglobalstring$(63, "Equip", 10)
menu$(5) = readglobalstring$(66, "Save", 10)
menu$(6) = readglobalstring$(68, "Map", 10)
menu$(7) = readglobalstring$(65, "Team", 10)

sn$ = ""
last = -1
GOSUB initshop
IF last = -1 THEN EXIT SUB
IF last = 0 THEN autopick = 1
last = last + 1: menu$(last) = readglobalstring$(74, "Exit", 10)

GOSUB repaintback
menusound gen(genAcceptSFX)

setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 IF carray(0) > 1 THEN pt = large(pt - 1, 0) : menusound gen(genCursorSFX)
 IF carray(1) > 1 THEN pt = small(pt + 1, last) : menusound gen(genCursorSFX)
 IF carray(5) > 1 THEN EXIT DO
 IF carray(4) > 1 OR autopick THEN
  IF pt = last THEN EXIT DO
  IF menuid(pt) = 0 THEN '--BUY
   buystuff id, 0, storebuf(), stock(), stat()
  END IF
  IF menuid(pt) = 1 THEN '--SELL
   sellstuff id, storebuf(), stock(), stat()
  END IF
  IF menuid(pt) = 2 THEN '--HIRE
   buystuff id, 1, storebuf(), stock(), stat()
  END IF
  IF menuid(pt) = 6 THEN '--MAP
   loadpage game$ + ".til", gmap(0), 3
   minimap catx(0), caty(0), tastuf()
  END IF
  IF menuid(pt) = 7 THEN '--TEAM
   heroswap 1, stat()
  END IF
  IF menuid(pt) = 4 THEN '--EQUIP
   w = onwho(readglobalstring$(108, "Equip Who?", 20), 0)
   IF w >= 0 THEN
    equip w, stat()
   END IF
  END IF
  IF menuid(pt) = 5 THEN '--SAVE
   temp = picksave(0)
   IF temp >= 0 THEN savegame temp, map, foep, stat(), stock()
   vishero stat()
  END IF
  IF menuid(pt) = 3 THEN '--INN
   inn = 0
   IF shoption(inn, storebuf(18), needf, stat()) THEN
    IF inn = 0 THEN
     innRestore stat()
    END IF
    IF storebuf(19) > 0 THEN
     '--Run animation for Inn
     rsr = runscript(storebuf(19), nowscript + 1, -1, "inn", plottrigger)
     IF rsr = 1 THEN
      EXIT DO
     END IF
    ELSE
     '--Inn has no script, do simple fade
     fadeout 0, 0, 80
     needf = 1
    END IF
   END IF
  END IF
  IF autopick THEN EXIT SUB
  GOSUB repaintback
 END IF
 h = (last + 2) * 10
 centerbox 160, 104 + (h * .5), 96, h, 1, dpage
 centerbox 160, 90, LEN(sn$) * 8 + 8, 16, 1, dpage
 edgeprint sn$, xstring(sn$, 160), 85, uilook(uiText), dpage
 FOR i = 0 TO last
  c = uilook(uiMenuItem): IF pt = i THEN c = uilook(uiSelectedItem + tog)
  edgeprint menu$(i), xstring(menu$(i), 160), 109 + i * 10, c, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 IF needf = 1 THEN needf = 0: fadein: setkeys
 IF needf > 1 THEN needf = needf - 1
 dowait
LOOP
menusound gen(genCancelSFX)
FOR t = 4 TO 5: carray(t) = 0: NEXT t
EXIT SUB

repaintback:
loadpage game$ + ".til", gmap(0), 3
setmapdata scroll(), buffer(), 0, 0
drawmap mapx, mapy,0, 0, dpage
if readbit(gmap(), 19, 0) then drawmap mapx, mapy, 1, 0, dpage, 1
if readbit(gmap(), 19, 1) then drawmap mapx, mapy, 2, 0, dpage, 1
copypage dpage, 3
RETRACE

initshop:
setpicstuf storebuf(), 40, -1
loadset game$ + ".sho", id, 0
sn$ = readbadbinstring$(storebuf(), 0, 15, 0)
o = 0
FOR i = 0 TO 7
 IF readbit(storebuf(), 17, i) THEN
  SWAP menu$(i), menu$(o)
  SWAP menuid(i), menuid(o)
  last = o
  o = o + 1
 END IF
NEXT i
RETRACE
END SUB

FUNCTION shoption (inn, price, needf, stat())
DIM menu$(1), sname$(40)

savetemppage 3
copypage dpage, 3

shoption = 0

getnames sname$()

menu$(0) = readglobalstring$(49, "Pay", 10)
menu$(1) = readglobalstring$(50, "Cancel", 10)
inncost$ = readglobalstring$(143, "THE INN COSTS", 20)
youhave$ = readglobalstring$(145, "You have", 20)
setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 IF carray(5) > 1 THEN inn = 1: EXIT DO
 IF carray(0) > 1 OR carray(1) > 1 OR carray(2) > 1 OR carray(3) > 1 THEN inn = inn XOR 1
 IF carray(4) > 1 THEN
  IF inn = 0 AND gold& >= price THEN
   gold& = gold& - price
   shoption = -1
   EXIT DO
  END IF
  IF inn = 1 THEN EXIT DO
 END IF
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN
   col = uilook(uiText)
   edgeprint names$(i), 128 - LEN(names$(i)) * 8, 5 + i * 10, col, dpage
   edgeprint STR$(ABS(stat(i, 0, 0))) + "/" + STR$(ABS(stat(i, 1, 0))), 136, 5 + i * 10, col, dpage
  END IF
 NEXT i
 centerfuz 160, 90, 200, 60, 1, dpage
 rectangle 130, 92, 60, 22, uilook(uiHighlight), dpage 'orig colour 20
 edgeprint inncost$ + XSTR$(price) + " " + sname$(32), 160 - LEN(inncost$ + XSTR$(price) + " " + sname$(32)) * 4, 70, uilook(uiText), dpage
 edgeprint youhave$ + XSTR$(gold&) + " " + sname$(32), 160 - LEN(youhave$ + XSTR$(gold&) + " " + sname$(32)) * 4, 80, uilook(uiText), dpage
 FOR i = 0 TO 1
  col = uilook(uiMenuItem): IF inn = i THEN col = uilook(uiSelectedItem + tog)
  edgeprint menu$(i), 160 - LEN(menu$(i)) * 4, 94 + i * 8, col, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 IF needf = 1 THEN needf = 0: fadein: setkeys
 IF needf > 1 THEN needf = needf - 1
 dowait
LOOP
loadtemppage 3

END FUNCTION

SUB snapshot
pre$ = trimextension$(sourcerpg$)

n = 0
DO
 shot$ = pre$ + STR$(ABS(n)) + ".bmp"
 IF isfile(shot$) = 0 THEN EXIT DO
 n = n + 1
LOOP UNTIL n > 999

#IFDEF __FB_LINUX__
IF NOT fileiswriteable(shot$) THEN
  shot$ = ENVIRON$("HOME") + SLASH + trimpath$(shot$)
END IF
#ENDIF

screenshot shot$, vpage, master()

IF isfile(shot$) THEN
 fh = FREEFILE
 OPEN shot$ FOR BINARY AS #fh
 a$ = CHR$(0)
 PUT #fh, 48, a$
 PUT #fh, 52, a$
 CLOSE #fh
END IF

END SUB

SUB tagdisplay
STATIC pt, top
DIM buf(20)

pt = large(pt, 0)

IF keyval(74) > 1 OR keyval(12) > 1 THEN
 '--minus
 IF keyval(29) > 0 THEN
  setbit tag(), 0, pt, 0
 ELSE
  pt = large(pt - 1, 0)
 END IF
END IF
IF keyval(78) > 1 OR keyval(13) > 1 THEN
 '--plus
 IF keyval(29) > 0 THEN
  setbit tag(), 0, pt, 1
 ELSE
  pt = small(pt + 1, 1999)
 END IF
END IF

top = bound(top, pt - 4, pt)

setpicstuf buf(), 42, -1
fuzzyrect 0, 0, 208, 50, uilook(uiOutline), dpage
FOR i = top TO top + 4
 temp$ = XSTR$(i) + " "
 buf(0) = 0
 SELECT CASE i
  CASE 0, 1
   temp$ = temp$ + " Reserved Tag"
  CASE IS > 1
   loadset game$ + ".tmn", i, 0
   FOR j = 1 TO small(buf(0), 20)
    temp$ = temp$ + CHR$(large(small(buf(j), 255), 0))
   NEXT j
 END SELECT
 c = uilook(uiDisabledItem)
 IF istag(i, 0) THEN c = uilook(uiHighlight) 'hmm
 'c = 8 + (-7 * istag(i, 0))
 edgeprint temp$, 8, (i - top) * 10, c, dpage
 IF i = pt THEN edgeprint "->", 0, (i - top) * 10, uilook(uiText), dpage
NEXT i

END SUB

SUB textfatalerror (e$)

'--show error
PRINT e$

'--crash out
SYSTEM

END SUB

SUB writejoysettings
fh = FREEFILE
OPEN exepath$ + SLASH + "joyset.ini" FOR OUTPUT AS #fh
PRINT #fh, "#Joystick/gamepad configuration"
PRINT #fh, "UPTHRESH=" + STR$(joy(9))
PRINT #fh, "DOWNTHRESH=" + STR$(joy(10))
PRINT #fh, "LEFTTHRESH=" + STR$(joy(11))
PRINT #fh, "RIGHTTHRESH=" + STR$(joy(12))
PRINT #fh, "USEBUTTON=" + STR$(joy(13) - 2)
PRINT #fh, "MENUBUTTON=" + STR$(joy(14 - 2))
CLOSE #fh
END SUB

SUB writescriptvar (id, newval)

SELECT CASE id
 CASE IS < 0 'local variable
  heap(scrat(nowscript).heap + ABS(id) - 1) = newval
 CASE 0 TO 1024 'global variable
  global(id) = newval
 CASE ELSE
  scripterr "Cannot write global" + XSTR$(id) + ". out of range"
END SELECT

END SUB

FUNCTION getdisplayname$ (default$)
 '--Get game's display name
 f$ = workingdir$ + SLASH + "browse.txt"
 IF isfile(f$) THEN
  setpicstuf buffer(), 40, -1
  loadset f$, 0, 0
  s$ = STRING$(bound(buffer(0), 0, 38), " ")
  array2str buffer(), 2, s$
  IF LEN(s$) > 0 THEN
  	getdisplayname$ = s$
  	EXIT FUNCTION
  END IF
 END IF
 getdisplayname$ = default$
END FUNCTION
