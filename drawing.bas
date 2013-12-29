'OHRRPGCE CUSTOM - Mostly drawing-related routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

#include "config.bi"
#include "udts.bi"
#include "custom_udts.bi"

'External subs and functions
DECLARE SUB loadpasdefaults (byref defaults as integer vector, tilesetnum as integer)
DECLARE SUB savepasdefaults (byref defaults as integer vector, tilesetnum as integer)
DECLARE FUNCTION importmasterpal (f as string, byval palnum as integer) as integer

'Local SUBs and FUNCTIONS
DECLARE FUNCTION importbmp_import(mxslump as string, imagenum as integer, srcbmp as string, pmask() as RGBcolor) as bool

DECLARE SUB picktiletoedit (byref tmode as integer, byval pagenum as integer, mapfile as string, bgcolor as integer)
DECLARE SUB editmaptile (ts as TileEditState, mover() as integer, mouse as MouseInfo, area() as MouseArea, bgcolor as integer)
DECLARE SUB tilecut (ts as TileEditState, mouse as MouseInfo)
DECLARE SUB refreshtileedit (mover() as integer, state as TileEditState)
DECLARE SUB writeundoblock (mover() as integer, state as TileEditState)
DECLARE SUB readundoblock (mover() as integer, state as TileEditState)
DECLARE SUB fliptile (mover() as integer, ts as TileEditState)
DECLARE SUB scrolltile (mover() as integer, ts as TileEditState, byval shiftx as integer, byval shifty as integer)
DECLARE SUB clicktile (mover() as integer, ts as TileEditState, byval newkeypress as integer, byref clone as TileCloneBuffer)
DECLARE SUB tilecopy (cutnpaste() as integer, ts as TileEditState)
DECLARE SUB tilepaste (cutnpaste() as integer, ts as TileEditState)
DECLARE SUB tiletranspaste (cutnpaste() as integer, ts as TileEditState)
DECLARE SUB copymapblock (buf() as integer, byref sx as integer, byref sy as integer, byref sp as integer, byref dx as integer, byref dy as integer, byref dp as integer)
DECLARE SUB changepal (byref palval as integer, byval palchange as integer, workpal() as integer, byval aindex as integer)
DECLARE SUB airbrush (byval x as integer, byval y as integer, byval d as integer, byval m as integer, byval c as integer, byval p as integer)
DECLARE FUNCTION mouseover (byval mousex as integer, byval mousey as integer, byref zox as integer, byref zoy as integer, byref zcsr as integer, area() as MouseArea) as integer
DECLARE SUB testanimpattern (tastuf() as integer, byref taset as integer)
DECLARE SUB setanimpattern (tastuf() as integer, taset as integer, tilesetnum as integer)
DECLARE SUB setanimpattern_refreshmenu(byval pt as integer, menu() as string, menu2() as string, tastuf() as integer, byval taset as integer, llim() as integer, ulim() as integer)
DECLARE SUB setanimpattern_forcebounds(tastuf() as integer, byval taset as integer, llim() as integer, ulim() as integer)
DECLARE SUB maptile ()
DECLARE SUB tileedit_set_tool (ts as TileEditState, toolinfo() as ToolInfoType, byval toolnum as integer)
DECLARE SUB tile_anim_draw_range(tastuf() as integer, byval taset as integer)
DECLARE SUB tile_anim_set_range(tastuf() as integer, byval taset as integer, byval pagenum as integer)
DECLARE SUB tile_animation(byval pagenum as integer)
DECLARE SUB tile_edit_mode_picker(byval pagenum as integer, mapfile as string, byref bgcolor as integer)

DECLARE SUB spriteedit_load_what_you_see(byval j as integer, byval top as integer, byval sets as integer, ss as SpriteEditState, byval soff as integer, placer() as integer, workpal() as integer, poffset() as integer)
DECLARE SUB spriteedit_save_what_you_see(byval j as integer, byval top as integer, byval sets as integer, ss as SpriteEditState, byval soff as integer, placer() as integer, workpal() as integer, poffset() as integer)
DECLARE SUB spriteedit_save_all_you_see(byval top as integer, byval sets as integer, ss as SpriteEditState, byval soff as integer, placer() as integer, workpal() as integer, poffset() as integer)
DECLARE SUB spriteedit_load_all_you_see(byval top as integer, byval sets as integer, ss as SpriteEditState, byval soff as integer, placer() as integer, workpal() as integer, poffset() as integer)
DECLARE SUB sprite_editor(byref ss as SpriteEditState, byref ss_save as SpriteEditStatic, state as MenuState, soff as integer, workpal() as integer, poffset() as integer, info() as string, byval sets as integer)
DECLARE SUB init_sprite_zones(area() as MouseArea, ss as SpriteEditState)
DECLARE SUB spriteedit_draw_icon(ss as SpriteEditState, icon as string, area() as MouseArea, byval areanum as integer, byval highlight as integer = NO)
DECLARE SUB spriteedit_display(ss as SpriteEditState, ss_save as SpriteEditStatic, state as MenuState, placer() as integer, workpal() as integer, poffset() as integer, info() as string, toolinfo() as ToolInfoType, area() as MouseArea, mouse as MouseInfo)
DECLARE SUB spriteedit_import16(byref ss as SpriteEditState, byref ss_save as SpriteEditStatic, byref state as MenuState, placer() as integer, workpal() as integer, poffset() as integer)
DECLARE SUB spriteedit_scroll (placer() as integer, ss as SpriteEditState, byval shiftx as integer, byval shifty as integer)
DECLARE SUB spriteedit_rotate_sprite_buffer(sprbuf() as integer, nulpal() as integer, counterclockwise as integer=NO)
DECLARE SUB spriteedit_rotate_sprite(sprbuf() as integer, ss as SpriteEditState, counterclockwise as integer=NO)
DECLARE SUB spriteedit_clip (placer() as integer, ss as SpriteEditState)
DECLARE SUB writeundospr (placer() as integer, ss as SpriteEditState, is_rotate as integer=NO)
DECLARE FUNCTION spriteedit_export_name (ss as SpriteEditState, state as MenuState) as string
DECLARE SUB spriteedit_export OVERLOAD (default_name as string, placer() as integer, nulpal() as integer, palnum as integer)
DECLARE SUB spriteedit_export OVERLOAD (default_name as string, img as GraphicPair)

#include "allmodex.bi"
#include "common.bi"
#include "loading.bi"
#include "customsubs.bi"
#include "cglobals.bi"

#include "const.bi"

SUB airbrush (byval x as integer, byval y as integer, byval d as integer, byval m as integer, byval c as integer, byval p as integer)
'airbrush thanks to Ironhoof (Russel Hamrick)

'AirBrush this rutine works VERY well parameters as fallows:
' AIRBRUSH x , y , diameter , mist_amount , color , page
' diameter sets the width & hight by square radius
' mist_amount sets how many pixels to place i put 100 and it ran fast so
' it works EXCELLENTLY with a mouse on the DTE =)

FOR count as integer = 1 TO randint(m)
 DIM x2 as integer = randint(d)
 DIM y2 as integer = randint(d)
 DIM x3 as integer = x - d / 2
 DIM y3 as integer = y - d / 2
 IF ABS((x3 + x2) - x) ^ 2 + ABS((y3 + y2) - y) ^ 2 <= d ^ 2 / 4 THEN
  putpixel x3 + x2, y3 + y2, c, p
 END IF
NEXT

END SUB

SUB changepal (byref palval as integer, byval palchange as integer, workpal() as integer, byval aindex as integer)

storepal16 workpal(), aindex, palval
palval = bound(palval + palchange, 0, 32767)
getpal16 workpal(), aindex, palval

END SUB

SUB copymapblock (buf() as integer, byref sx as integer, byref sy as integer, byref sp as integer, byref dx as integer, byref dy as integer, byref dp as integer)

'buf() is a 20-byte array

FOR i as integer = 0 TO 19
 loadsprite buf(), 0, sx, sy + i, 40, 1, sp
 stosprite buf(), 0, dx, dy + i, dp
NEXT i

END SUB

SUB importbmp (f as string, cap as string, byref count as integer)
STATIC default as string
DIM pmask(255) as RGBcolor
DIM menu(6) as string
DIM mstate as MenuState
mstate.size = 24
mstate.last = UBOUND(menu)
DIM menuopts as MenuOptions
menuopts.edged = YES
menu(0) = "Return to Main Menu"
menu(1) = CHR(27) + "Browse 0" + CHR(26)
menu(2) = "Replace current " + cap
menu(3) = "Append a new " + cap
menu(4) = "Disable palette colors for import"
menu(5) = "Export " + cap + " as BMP"
menu(6) = "Full screen view"
DIM srcbmp as string
DIM csr2 as integer
DIM tog as integer
DIM cx as integer
DIM cy as integer
DIM pt as integer = 0 'backdrop number

IF count = 0 THEN count = 1
loadpalette pmask(), activepalette
loadmxs game & f, pt, vpages(2)

setkeys
DO
 setwait 55
 setkeys
 IF keyval(scCtrl) > 0 AND keyval(scBackspace) > 1 THEN
  DIM crop_this as integer = count - 1
  cropafter pt, crop_this, 3, game + f, 64000
  count = crop_this + 1
 END IF
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "importbmp"
 usemenu mstate
 IF intgrabber(pt, 0, count - 1) THEN
  menu(1) = CHR(27) + "Browse " & pt & CHR(26)
  loadmxs game + f, pt, vpages(2)
 END IF
 IF enter_space_click(mstate) THEN
  IF mstate.pt = 0 THEN EXIT DO
  IF mstate.pt = 2 THEN
   'Replace current
   srcbmp = browse(3, default, "*.bmp", "",,"browse_import_" & cap)
   IF srcbmp <> "" THEN
    importbmp_import(game & f, pt, srcbmp, pmask())
   END IF
   loadmxs game + f, pt, vpages(2)
  END IF
  IF mstate.pt = 3 AND count < 32767 THEN
   'Append new
   srcbmp = browse(3, default, "*.bmp", "",,"browse_import_" & cap)
   IF srcbmp <> "" THEN
    IF importbmp_import(game & f, count, srcbmp, pmask()) THEN
     pt = count
     count = pt + 1
    END IF
   END IF
   menu(1) = CHR(27) + "Browse " & pt & CHR(26)
   loadmxs game + f, pt, vpages(2)
  END IF
  IF mstate.pt = 4 THEN GOSUB disable
  IF mstate.pt = 5 THEN
   DIM outfile as string
   outfile = inputfilename("Name of file to export to?", ".bmp", "", "input_file_export_screen", trimextension(trimpath(sourcerpg)) & " " & cap & pt)
   IF outfile <> "" THEN frame_export_bmp8 outfile & ".bmp", vpages(2), master()
  END IF
 END IF
 copypage 2, dpage
 IF mstate.pt <> 6 THEN
  standardmenu menu(), mstate, 0, 0, dpage, menuopts
 END IF
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
clearpage 2
EXIT SUB

disable:
csr2 = 0
setpal pmask()
setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(scESC) > 1 THEN setpal master(): RETRACE
 IF keyval(scF1) > 1 THEN show_help "importbmp_disable"
 IF csr2 = 0 THEN
  IF enter_or_space() THEN setpal master(): RETRACE
  IF keyval(scDown) > 1 THEN csr2 = 1: cy = -1
 END IF
 IF csr2 = 1 THEN
  IF keyval(scLeft) > 1 THEN cx = large(cx - 1, 0)
  IF keyval(scRight) > 1 THEN cx = small(cx + 1, 15)
  IF keyval(scDown) > 1 THEN cy = small(cy + 1, 15)
  IF keyval(scUp) > 1 THEN cy = cy - 1: IF cy < 0 THEN cy = 0: csr2 = 0
  IF enter_or_space() THEN
   pmask(cy * 16 + cx).r xor= master(cy * 16 + cx).r
   pmask(cy * 16 + cx).g xor= master(cy * 16 + cx).g
   pmask(cy * 16 + cx).b xor= master(cy * 16 + cx).b
   setpal pmask()
  END IF
 END IF
 copypage 2, dpage
 textcolor uilook(uiMenuItem), 0: IF csr2 = 0 THEN textcolor uilook(uiSelectedItem + tog), 0
 printstr "Previous Menu", 0, 0, dpage
 IF csr2 = 1 THEN rectangle 0 + cx * 10, 8 + cy * 10, 10, 10, uilook(uiSelectedItem + tog), dpage
 FOR i as integer = 0 TO 15
  FOR o as integer = 0 TO 15
   rectangle 1 + o * 10, 9 + i * 10, 8, 8, i * 16 + o, dpage
  NEXT o
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
END SUB

'Returns true if imported, false if cancelled
FUNCTION importbmp_import(mxslump as string, imagenum as integer, srcbmp as string, pmask() as RGBcolor) as bool
 DIM bmpd as BitmapInfoHeader
 DIM menu(2) as string
 DIM paloption as integer
 DIM img as Frame ptr
 DIM temppal(255) as RGBcolor
 DIM palmapping(255) as integer

 bmpinfo(srcbmp, bmpd)
 IF bmpd.biBitCount <= 8 THEN
  paloption = 2  'no remapping
  loadbmppal srcbmp, temppal()
  IF memcmp(@temppal(0), @master(0), 256 * sizeof(RGBcolor)) <> 0 THEN
   'the palette is inequal to the master palette 
   clearpage vpage
   menu(0) = "Remap to current Master Palette"
   menu(1) = "Import with new Master Palette"
   menu(2) = "Do not remap colours"
   paloption = multichoice("This BMP's palette is not identical to your master palette", _
                           menu(), , , "importbmp_palette")
   IF paloption = -1 THEN RETURN NO
   IF paloption = 1 THEN
    importmasterpal srcbmp, gen(genMaxMasterPal) + 1
    activepalette = gen(genMaxMasterPal)
    setpal master()
    LoadUIColors uilook(), boxlook(), activepalette
   END IF
  END IF
  img = frame_import_bmp_raw(srcbmp)
  IF paloption = 0 THEN
   convertbmppal srcbmp, pmask(), palmapping()
   FOR y as integer = 0 TO img->h - 1
    FOR x as integer = 0 TO img->w - 1
     putpixel img, x, y, palmapping(readpixel(img, x, y))
    NEXT
   NEXT
  END IF
 ELSE
  img = frame_import_bmp24_or_32(srcbmp, pmask())
 END IF
 storemxs mxslump, imagenum, img
 frame_unload @img
 loadpalette pmask(), activepalette
 RETURN YES
END FUNCTION

'Draw a Frame (specially a tileset) onto another Frame with the transparent
'colour replaced either with another colour, or with a chequer pattern.
'bgcolor is either between 0 and 255 (a colour), -1 (a scrolling chequered
'background), or -2 (a non-scrolling chequered background)
'chequer_scroll is a counter variable which the calling function should increment once per tick.
SUB frame_draw_with_background (byval src as Frame ptr, byval pal as Palette16 ptr = NULL, byval x as integer, byval y as integer, byval scale as integer = 1, byval bgcolor as integer, byref chequer_scroll as integer, byval dest as Frame ptr)
 draw_background x, y, src->w * scale, src->h * scale, bgcolor, chequer_scroll, dest
 'Draw transparently
 frame_draw src, pal, x, y, scale, YES, dest
END SUB

SUB maptile ()
STATIC bgcolor as integer = 0
DIM menu() as string
DIM mapfile as string = game & ".til"
DIM pagenum as integer
DIM top as integer = -1
DIM chequer_scroll as integer

DIM state as MenuState
state.top = -1
state.pt = -1
state.first = -1
state.last = gen(genMaxTile)
state.size = 20
state.need_update = YES

clearpage 3
setkeys
DO
 chequer_scroll += 1
 setwait 55
 setkeys
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "maptile_pickset"
 IF keyval(scCtrl) > 0 AND keyval(scBackspace) > 1 AND state.pt > -1 THEN
  cropafter state.pt, gen(genMaxTile), 3, game + ".til", 64000
  state.last = gen(genMaxTile)
  state.need_update = YES
 END IF
 DIM tempnum as integer = large(state.pt, 0)
 IF intgrabber(tempnum, 0, gen(genMaxTile), , , YES) THEN
  state.pt = tempnum
  state.need_update = YES
 END IF
 IF keyval(scDown) > 1 AND state.pt = gen(genMaxTile) AND gen(genMaxTile) < 32767 THEN
  state.pt += 1
  IF needaddset(state.pt, gen(genMaxTile), "tile set") THEN
   clearpage 3
   storemxs mapfile, state.pt, vpages(3)  'lazy
   state.last = gen(genMaxTile)
   state.need_update = YES
  END IF
 ELSEIF usemenu(state) THEN
  state.need_update = YES
 END IF
 IF enter_space_click(state) AND state.pt = -1 THEN EXIT DO
 IF enter_space_click(state) AND state.pt > -1 THEN
  pagenum = state.pt
  tile_edit_mode_picker pagenum, mapfile, bgcolor
  state.need_update = YES
 END IF

 IF state.need_update THEN
  state.need_update = NO
  REDIM menu(-1 TO gen(genMaxTile))
  menu(-1) = "Return to Main Menu"
  FOR i as integer = 0 TO gen(genMaxTile)
   menu(i) = "Tile Set " & i
  NEXT
  IF state.pt = -1 THEN clearpage 3 ELSE loadmxs mapfile, state.pt, vpages(3)
 END IF

 frame_draw_with_background vpages(3), , 0, 0, , bgcolor, chequer_scroll, vpages(dpage)
 DIM menuopts as MenuOptions
 menuopts.edged = YES
 standardmenu menu(), state, 10, 8, dpage, menuopts
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
clearpage 3
clearpage 2
clearpage 1
clearpage 0
'tileset_empty_cache
'Robust againts tileset leaks
sprite_update_cache_tilesets

END SUB
 
SUB tile_edit_mode_picker(byval pagenum as integer, mapfile as string, byref bgcolor as integer)
 DIM chequer_scroll as integer
 DIM menu(6) as string
 menu(0) = "Draw Tiles"
 menu(1) = "Cut Tiles from Tilesets"
 menu(2) = "Cut Tiles from Backdrops"
 menu(3) = "Set Default Passability"
 menu(4) = "Define Tile Animation"
 menu(6) = "Cancel"

 DIM state as MenuState
 init_menu_state state, menu()
 state.need_update = YES
 
 DIM menuopt as menuOptions
 menuopt.edged = YES
 
 setkeys
 DO
  chequer_scroll += 1
  setwait 55
  setkeys
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "maptile_tilemode"
  usemenu state
  IF enter_space_click(state) THEN
   SELECT CASE state.pt
    CASE 0, 1, 2, 3
     picktiletoedit state.pt, pagenum, mapfile, bgcolor
    CASE 4
     tile_animation pagenum
    CASE 5
     bgcolor = color_browser_256(large(bgcolor, 0))
    CASE 6
     EXIT DO
   END SELECT
  END IF
  IF state.pt = 5 THEN intgrabber(bgcolor, -2, 255)
  frame_draw_with_background vpages(3), , 0, 0, , bgcolor, chequer_scroll, vpages(dpage)
  IF bgcolor = -2 THEN
   menu(5) = "Background: chequer"
  ELSEIF bgcolor = -1 THEN
   menu(5) = "Background: scrolling chequer"
  ELSE
   menu(5) = "Background color: " & bgcolor
  END IF
  standardmenu menu(), state, 10, 8, dpage, menuopt
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

END SUB

SUB tile_animation(byval pagenum as integer)
 DIM tastuf(40) as integer
 DIM taset as integer = 0
 loadtanim pagenum, tastuf()

 DIM menu(5) as string 
 menu(0) = "Previous Menu"
 menu(2) = "Set Animation Range"
 menu(3) = "Set Animation Pattern"
 menu(5) = "Test Animations"
 
 DIM state as MenuState
 init_menu_state state, menu()
 state.need_update = YES
 
 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "maptile_tileanim"
  IF usemenu(state) THEN state.need_update = YES
  IF state.pt = 4 THEN
   IF tag_grabber(tastuf(1 + 20 * taset)) THEN state.need_update = YES
  ELSE
   IF intgrabber(taset, 0, 1) THEN state.need_update = YES
  END IF
  IF enter_space_click(state) THEN
   SELECT CASE state.pt
    CASE 0: EXIT DO
    CASE 2: tile_anim_set_range tastuf(), taset, pagenum
    CASE 3: setanimpattern tastuf(), taset, pagenum
    CASE 5: testanimpattern tastuf(), taset
   END SELECT
  END IF
  IF state.need_update THEN
   menu(1) = CHR(27) & "Animation set " & taset & CHR(26)
   menu(4) = tag_condition_caption(tastuf(1 + 20 * taset), "Disable if Tag", "No tag check")
   state.need_update = YES
  END IF
  clearpage dpage
  standardmenu menu(), state, 0, 0, dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 savetanim pagenum, tastuf()
END SUB

SUB tile_anim_set_range(tastuf() as integer, byval taset as integer, byval pagenum as integer)
 DIM tog as integer
 DIM mouse as MouseInfo

 setkeys
 DO
  setwait 55
  setkeys
  tog = tog XOR 1
  IF keyval(scESC) > 1 OR enter_or_space() THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "maptile_setanimrange"
  IF keyval(scUp) > 1 THEN tastuf(0 + 20 * taset) = large(tastuf(0 + 20 * taset) - 16, 0)
  IF keyval(scDown) > 1 THEN tastuf(0 + 20 * taset) = small(tastuf(0 + 20 * taset) + 16, 112)
  IF keyval(scLeft) > 1 THEN tastuf(0 + 20 * taset) = large(tastuf(0 + 20 * taset) - 1, 0)
  IF keyval(scRight) > 1 THEN tastuf(0 + 20 * taset) = small(tastuf(0 + 20 * taset) + 1, 112)
  mouse = readmouse()
  WITH mouse
   IF .clickstick AND mouseleft THEN
    IF rect_collide_point(str_rect("ESC when done", 0, 0), .x, .y) THEN
     EXIT DO
    ELSE
     tastuf(0 + 20 * taset) = small(cint(int(.x / 20) + int(.y / 20) * 16), 112)
    END IF
   END IF
  END WITH
  copypage 3, dpage
  tile_anim_draw_range tastuf(), taset
  edgeprint "ESC when done", 0, 0, uilook(uiText), dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 savetanim pagenum, tastuf()
END SUB

SUB tile_anim_draw_range(tastuf() as integer, byval taset as integer)
 DIM animpos as XYPair
 animpos.x = 0
 animpos.y = 0
 FOR i as integer = 0 TO 159
  IF i < tastuf(0 + 20 * taset) OR i > tastuf(0 + 20 * taset) + 47 THEN
   fuzzyrect animpos.x * 20, animpos.y * 20, 20, 20, uilook(uiText), dpage
  END IF
  animpos.x += 1
  IF animpos.x > 15 THEN
   animpos.x = 0
   animpos.y += 1
  END IF
 NEXT i
END SUB

FUNCTION mouseover (byval mousex as integer, byval mousey as integer, byref zox as integer, byref zoy as integer, byref zcsr as integer, area() as MouseArea) as integer

FOR i as integer = UBOUND(area) TO 0 STEP -1
 IF area(i).w <> 0 AND area(i).h <> 0 THEN
  IF mousex >= area(i).x AND mousex < area(i).x + area(i).w THEN
   IF mousey >= area(i).y AND mousey < area(i).y + area(i).h THEN
    zox = mousex - area(i).x
    zoy = mousey - area(i).y
    zcsr = area(i).hidecursor
    mouseover = i + 1
    EXIT FUNCTION
   END IF 'Y OKAY---
  END IF 'X OKAY---
 END IF 'VALID ZONE---
NEXT i

END FUNCTION

SUB setanimpattern (tastuf() as integer, taset as integer, tilesetnum as integer)
DIM menu(10) as string
DIM menu2(1) as string
DIM llim(7) as integer
DIM ulim(7) as integer
menu(0) = "Previous Menu"
FOR i as integer = 1 TO 2
 llim(i) = 0
 ulim(i) = 9
NEXT i
FOR i as integer = 3 TO 4
 llim(i) = 0
 ulim(i) = 159
NEXT i
llim(5) = 0
ulim(5) = 32767
llim(6) = -max_tag()
ulim(6) = max_tag()

DIM context as integer = 0
DIM index as integer = 0
DIM tog as integer

DIM state as MenuState
init_menu_state state, menu()
state.need_update = YES

DIM state2 as MenuState
init_menu_state state2, menu2()

setkeys
DO
 setwait 55
 setkeys
 IF keyval(scF1) > 1 THEN show_help "maptile_setanimpattern"
 SELECT CASE context
  CASE 0 '---PICK A STATEMENT---
   IF keyval(scESC) > 1 THEN EXIT DO
   IF usemenu(state) THEN state.need_update = YES
   IF enter_space_click(state) THEN
    IF state.pt = 0 THEN
     EXIT DO
    ELSE
     context = 1
    END IF
   END IF
  CASE 1 '---EDIT THAT STATEMENT---
   IF keyval(scESC) > 1 THEN
    savetanim tilesetnum, tastuf()
    context = 0
   END IF
   usemenu state2
   index = bound(state.pt - 1, 0, 8) + 20 * taset
   IF state2.pt = 0 THEN
    IF intgrabber(tastuf(2 + index), 0, 6) THEN state.need_update = YES
   END IF
   IF state2.pt = 1 THEN
    IF tastuf(2 + index) = 6 THEN
     IF tag_grabber(tastuf(11 + index)) THEN state.need_update = YES
    ELSE
     IF intgrabber(tastuf(11 + index), llim(tastuf(2 + index)), ulim(tastuf(2 + index))) THEN state.need_update = YES
    END IF
   END IF
   IF enter_space_click(state2) THEN context = 0
 END SELECT
 IF state.need_update THEN
  setanimpattern_refreshmenu state.pt, menu(), menu2(), tastuf(), taset, llim(), ulim()
  state.need_update = NO
 END IF
 '--Draw screen
 clearpage dpage
 state.active = (context = 0)
 standardmenu menu(), state, 0, 0, dpage
 IF state.pt > 0 THEN
  state2.active = (context = 1)
  standardmenu menu2(), state2, 0, 100, dpage
 END IF
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
setanimpattern_forcebounds tastuf(), taset, llim(), ulim()
EXIT SUB

END SUB

SUB setanimpattern_refreshmenu(byval pt as integer, menu() as string, menu2() as string, tastuf() as integer, byval taset as integer, llim() as integer, ulim() as integer)
 DIM animop(7) as string
 animop(0) = "end of animation"
 animop(1) = "up"
 animop(2) = "down"
 animop(3) = "right"
 animop(4) = "left"
 animop(5) = "wait"
 animop(6) = "if tag do rest"
 animop(7) = "unknown command"

 setanimpattern_forcebounds tastuf(), taset, llim(), ulim()
 FOR i as integer = 1 TO 9
  menu(i) = "-"
 NEXT i
 menu(10) = ""
 DIM anim_a as integer
 DIM anim_b as integer
 DIM anim_i as integer
 FOR anim_i = 0 TO 8
  anim_a = bound(tastuf((2 + anim_i) + 20 * taset), 0, 7)
  anim_b = tastuf((11 + anim_i) + 20 * taset)
  menu(anim_i + 1) = animop(anim_a)
  IF anim_a = 0 THEN EXIT FOR
  IF anim_a > 0 AND anim_a < 6 THEN menu(anim_i + 1) = menu(anim_i + 1) & " " & anim_b
  IF anim_a = 6 THEN menu(anim_i + 1) = menu(anim_i + 1) & " (" & load_tag_name(anim_b) & ")"
 NEXT anim_i
 IF anim_i = 8 THEN menu(10) = "end of animation"

 menu2(0) = "Action=" + animop(bound(tastuf(2 + bound(pt - 1, 0, 8) + 20 * taset), 0, 7))
 menu2(1) = "Value="
 DIM ta_temp as integer
 ta_temp = tastuf(11 + bound(pt - 1, 0, 8) + 20 * taset)
 SELECT CASE tastuf(2 + bound(pt - 1, 0, 8) + 20 * taset)
  CASE 1 TO 4
   menu2(1) &= ta_temp & " Tiles"
  CASE 5
   menu2(1) &= ta_temp & " Ticks"
  CASE 6
   menu2(1) &= tag_condition_caption(ta_temp, , "Never")
  CASE ELSE
   menu2(1) &= "N/A"
 END SELECT
END SUB

SUB setanimpattern_forcebounds(tastuf() as integer, byval taset as integer, llim() as integer, ulim() as integer)
 DIM tmp as integer
 FOR i as integer = 0 TO 8
  tmp = bound(i, 0, 8) + 20 * taset
  tastuf(2 + tmp) = bound(tastuf(2 + tmp), 0, 7)
  tastuf(11 + tmp) = bound(tastuf(11 + tmp), llim(tastuf(2 + tmp)), ulim(tastuf(2 + tmp)))
 NEXT i
END SUB

SUB testanimpattern (tastuf() as integer, byref taset as integer)

DIM sample as TileMap
DIM tilesetview as TileMap
DIM tanim_state(1) as TileAnimState
DIM tileset as Frame ptr = NULL
DIM tog as integer
DIM csr as integer
DIM x as integer
DIM y as integer

tileset = frame_to_tileset(vpages(3))

cleantilemap tilesetview, 16, 3
FOR y as integer = 0 TO 2
 FOR x as integer = 0 TO 15
  writeblock tilesetview, x, y, tastuf(20 * taset) + x + y * 16
 NEXT
NEXT

GOSUB setupsample

setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "maptile_testanimpattern"
 IF keyval(scUp) > 1 THEN csr = loopvar(csr, 0, 47, -16): GOSUB setupsample
 IF keyval(scDown) > 1 THEN csr = loopvar(csr, 0, 47, 16): GOSUB setupsample
 IF keyval(scLeft) > 1 THEN csr = loopvar(csr, 0, 47, -1): GOSUB setupsample
 IF keyval(scRight) > 1 THEN csr = loopvar(csr, 0, 47, 1): GOSUB setupsample
 '--draw available animating tiles--
 clearpage dpage
 drawmap tilesetview, 0, 0, tileset, dpage, , , , 10, 60
 '--draw sample--
 setanim tastuf(0) + tanim_state(0).cycle, tastuf(20) + tanim_state(1).cycle
 cycletile tanim_state(), tastuf()
 drawmap sample, -130, 0, tileset, dpage, , , , 100, 60
 '--Draw cursor--
 y = INT(csr / 16)
 x = csr - y * 16
 rectangle 20 * x, 10 + 20 * y, 20, 1, uilook(uiSelectedItem + tog), dpage
 rectangle 20 * x, 10 + 20 * y, 1, 20, uilook(uiSelectedItem + tog), dpage
 rectangle 20 * x, 29 + 20 * y, 20, 1, uilook(uiSelectedItem + tog), dpage
 rectangle 20 * x + 19, 10 + 20 * y, 1, 20, uilook(uiSelectedItem + tog), dpage
 
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
frame_unload @tileset
unloadtilemap sample
unloadtilemap tilesetview
EXIT SUB

setupsample:
cleantilemap sample, 3, 3
FOR x = 0 TO 2
 FOR y = 0 TO 2
  writeblock sample, x, y, 160 + (taset * 48) + csr
 NEXT
NEXT
RETRACE

END SUB

SUB picktiletoedit (byref tmode as integer, byval pagenum as integer, mapfile as string, bgcolor as integer)
STATIC cutnpaste(19, 19) as integer
STATIC oldpaste as integer
DIM ts as TileEditState
DIM mover(12) as integer
DIM area(24) as MouseArea
DIM mouse as MouseInfo
ts.tilesetnum = pagenum
ts.drawframe = frame_new(20, 20, , YES)
DIM chequer_scroll as integer
DIM tog as integer
ts.gotmouse = havemouse()
hidemousecursor
ts.canpaste = oldpaste
ts.drawcursor = 1
ts.airsize = 5
ts.mist = 4
area(0).x = 80   'Tile at 8x zoom
area(0).y = 0
area(0).w = 160
area(0).h = 160
area(0).hidecursor = YES
area(1).x = 0
area(1).y = 160
area(1).w = 320
area(1).h = 32
'TOOLS (more at 21+)
FOR i as integer = 0 TO 5
 area(2 + i).x = 4 + i * 9
 area(2 + i).y = 32
 area(2 + i).w = 8
 area(2 + i).h = 8
NEXT i
FOR i as integer = 0 TO 3
 area(12 + i).x = 4 + i * 9
 area(12 + i).y = 42
 area(12 + i).w = 8
 area(12 + i).h = 8
NEXT i
'Areas 11 and 12 used only in tile cutter
'LESS AIRBRUSH AREA
area(16).x = 12
area(16).y = 60
area(16).w = 8
area(16).h = 8
area(16).hidecursor = NO
'LESS AIRBRUSH MIST
area(17).x = 12
area(17).y = 76
area(17).w = 8
area(17).h = 8
area(17).hidecursor = NO
'MORE AIRBRUSH AREA
area(18).x = 36
area(18).y = 60
area(18).w = 8
area(18).h = 8
area(18).hidecursor = NO
'MORE AIRBRUSH MIST
area(19).x = 36
area(19).y = 76
area(19).w = 8
area(19).h = 8
area(19).hidecursor = NO
FOR i as integer = 0 TO 1  'mark and clone
 area(21 + i).x = 49 + i * 9
 area(21 + i).y = 42
 area(21 + i).w = 8
 area(21 + i).h = 8
NEXT i
area(23).x = 58  'airbrush
area(23).y = 32
area(23).w = 8
area(23).h = 8
area(24).x = 40  'scroll tool
area(24).y = 42
area(24).w = 8
area(24).h = 8

DIM pastogkey(7) as integer
DIM bitmenu(10) as string
IF tmode = 3 THEN
 pastogkey(0) = scUp
 pastogkey(1) = scRight
 pastogkey(2) = scDown
 pastogkey(3) = scLeft
 pastogkey(4) = scA
 pastogkey(5) = scB
 pastogkey(6) = scH
 pastogkey(7) = scO
 loadpasdefaults ts.defaultwalls, pagenum
 bitmenu(0) = "Impassable to the North"
 bitmenu(1) = "Impassable to the East"
 bitmenu(2) = "Impassable to the South"
 bitmenu(3) = "Impassable to the West"
 bitmenu(4) = "A-type vehicle Tile"
 bitmenu(5) = "B-type vehicle Tile"
 bitmenu(6) = "Harm Tile"
 bitmenu(7) = "Overhead Tile (OBSOLETE!)"
END IF    

loadmxs mapfile, pagenum, vpages(3)
'pick block to draw/import/default
DIM bnum as integer = 0
setkeys
DO
 setwait 17, 70
 setkeys
 IF ts.gotmouse THEN
  mouse = readmouse
 END IF
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN
  IF tmode = 3 THEN
   show_help "default_passability"
  ELSE
   show_help "picktiletoedit"
  END IF
 END IF
 IF ts.gotmouse THEN
  bnum = (mouse.y \ 20) * 16 + mouse.x \ 20
 END IF
 IF tmode <> 3 OR keyval(scCtrl) = 0 THEN
  DIM movedcsr as integer = NO
  IF slowkey(scLeft, 100) THEN bnum = (bnum + 159) MOD 160: movedcsr = YES
  IF slowkey(scRight, 100) THEN bnum = (bnum + 1) MOD 160: movedcsr = YES
  IF slowkey(scUp, 100) THEN bnum = (bnum + 144) MOD 160: movedcsr = YES
  IF slowkey(scDown, 100) THEN bnum = (bnum + 16) MOD 160: movedcsr = YES
  IF movedcsr AND ts.gotmouse THEN
   mouse.x = (mouse.x MOD 20) + (bnum MOD 16) * 20
   mouse.y = (mouse.y MOD 20) + (bnum \ 16) * 20
   movemouse mouse.x, mouse.y
  END IF
 END IF
 IF tmode = 3 THEN
  '--pass mode shortcuts
  FOR i as integer = 0 TO 7
   IF keyval(scCtrl) > 0 OR i > 3 THEN
    IF keyval(pastogkey(i)) > 1 THEN
     ts.defaultwalls[bnum] XOR= 1 SHL i
    END IF
   END IF
  NEXT i
 END IF
 IF copy_keychord() THEN tilecopy cutnpaste(), ts
 IF paste_keychord() THEN tilepaste cutnpaste(), ts
 IF (keyval(scCtrl) > 0 AND keyval(scT) > 1) THEN tiletranspaste cutnpaste(), ts
 ts.tilex = bnum AND 15
 ts.tiley = bnum \ 16
 IF enter_or_space() OR mouse.clicks > 0 THEN
  setkeys
  IF tmode = 0 THEN
   editmaptile ts, mover(), mouse, area(), bgcolor
  END IF
  IF tmode = 1 THEN
   ts.cuttileset = YES
   ts.cutfrom = small(ts.cutfrom, gen(genMaxTile))
   tilecut ts, mouse
  END IF 
  IF tmode = 2 THEN
   ts.cuttileset = NO
   ts.cutfrom = small(ts.cutfrom, gen(genNumBackdrops) - 1)
   tilecut ts, mouse
  END IF 
  IF tmode = 3 THEN
   DIM buf() as integer
   vector_to_array buf(), ts.defaultwalls
   editbitset buf(), bnum, 7, bitmenu()
   array_to_vector ts.defaultwalls, buf()
  END IF
  IF slave_channel <> NULL_CHANNEL THEN storemxs mapfile, pagenum, vpages(3)
 END IF

 frame_draw_with_background vpages(3), , 0, 0, , bgcolor, chequer_scroll, vpages(dpage)
 IF tmode = 1 OR tmode = 2 THEN
  'Show tile number
  edgeprint "Tile " & bnum, 0, IIF(bnum < 112, 190, 0), uilook(uiText), dpage
 END IF
 IF tmode = 3 THEN
  FOR o as integer = 0 TO 9
   FOR i as integer = 0 TO 15
    IF (ts.defaultwalls[i + o * 16] AND 1) THEN rectangle i * 20, o * 20, 20, 3, uilook(uiMenuItem + tog), dpage
    IF (ts.defaultwalls[i + o * 16] AND 2) THEN rectangle i * 20 + 17, o * 20, 3, 20, uilook(uiMenuItem + tog), dpage
    IF (ts.defaultwalls[i + o * 16] AND 4) THEN rectangle i * 20, o * 20 + 17, 20, 3, uilook(uiMenuItem + tog), dpage
    IF (ts.defaultwalls[i + o * 16] AND 8) THEN rectangle i * 20, o * 20, 3, 20, uilook(uiMenuItem + tog), dpage
    textcolor uilook(uiSelectedItem + tog), 0
    IF (ts.defaultwalls[i + o * 16] AND 16) THEN printstr "A", i * 20, o * 20, dpage
    IF (ts.defaultwalls[i + o * 16] AND 32) THEN printstr "B", i * 20 + 10, o * 20, dpage
    IF (ts.defaultwalls[i + o * 16] AND 64) THEN printstr "H", i * 20, o * 20 + 10, dpage
    IF (ts.defaultwalls[i + o * 16] AND 128) THEN printstr "O", i * 20 + 10, o * 20 + 10, dpage
   NEXT i
  NEXT o
 END IF
 rectangle ts.tilex * 20 + 7, ts.tiley * 20 + 7, 6, 6, IIF(tog, uilook(uiBackground), uilook(uiText)), dpage
 IF ts.gotmouse THEN
  IF tog THEN
   textcolor uilook(uiText), 0
  ELSE
   textcolor uilook(uiDescription), 0
  END IF
  printstr CHR(2), mouse.x - 2, mouse.y - 2, dpage
 END IF
 SWAP dpage, vpage
 setvispage vpage
 IF dowait THEN
  tog = tog XOR 1
  chequer_scroll += 1
 END IF
LOOP
storemxs mapfile, pagenum, vpages(3)
IF tmode = 3 THEN
 savepasdefaults ts.defaultwalls, pagenum
END IF
v_free ts.defaultwalls
oldpaste = ts.canpaste
frame_unload @ts.drawframe
unhidemousecursor
END SUB

SUB refreshtileedit (mover() as integer, state as TileEditState)
copymapblock mover(), state.tilex * 20, state.tiley * 20, 3, 280, 10 + (state.undo * 21), 2
frame_draw vpages(3), NULL, -state.tilex * 20, -state.tiley * 20, , NO, state.drawframe  'Blit the tile onto state.drawframe
END SUB

SUB writeundoblock (mover() as integer, state as TileEditState)
state.undo = loopvar(state.undo, 0, 5, 1)
copymapblock mover(), state.tilex * 20, state.tiley * 20, 3, 280, 10 + (state.undo * 21), 2
textcolor uilook(uiMenuItem), 0
printstr ">", 270, 16 + (state.undo * 21), 2
state.allowundo = 1
END SUB

SUB readundoblock (mover() as integer, state as TileEditState)
FOR j as integer = 0 TO 5
 rectangle 270, 16 + (j * 21), 8, 8, 0, 2
NEXT j
copymapblock mover(), 280, 10 + (state.undo * 21), 2, state.tilex * 20, state.tiley * 20, 3
textcolor uilook(uiMenuItem), 0
printstr ">", 270, 16 + (state.undo * 21), 2
refreshtileedit mover(), state
END SUB

SUB editmaptile (ts as TileEditState, mover() as integer, mouse as MouseInfo, area() as MouseArea, bgcolor as integer)
STATIC clone as TileCloneBuffer
DIM spot as XYPair

DIM toolinfo(SPRITEEDITOR_NUM_TOOLS - 1) as ToolInfoType
WITH toolinfo(0)
 .name = "Draw"
 .icon = CHR(3)
 .shortcut = scD
 .cursor = 0
 .areanum = 2
END WITH
WITH toolinfo(1)
 .name = "Box"
 .icon = CHR(4)
 .shortcut = scB
 .cursor = 1
 .areanum = 3
END WITH
WITH toolinfo(2)
 .name = "Line"
 .icon = CHR(5)
 .shortcut = scL
 .cursor = 2
 .areanum = 4
END WITH
WITH toolinfo(3)
 .name = "Fill"
 .icon = "F"
 .shortcut = scF
 .cursor = 3
 .areanum = 5
END WITH
WITH toolinfo(4)
 .name = "Oval"
 .icon = "O"
 .shortcut = scO
 .cursor = 2
 .areanum = 7
END WITH
WITH toolinfo(5)
 .name = "Air"
 .icon = "A"
 .shortcut = scA
 .cursor = 3
 .areanum = 23
END WITH
WITH toolinfo(6)
 .name = "Mark"
 .icon = "M"
 .shortcut = scM
 .cursor = 3
 .areanum = 21
END WITH
WITH toolinfo(7)
 .name = "Clone"
 .icon = "C"
 .shortcut = scC
 .cursor = 3
 .areanum = 22
END WITH
WITH toolinfo(8)
 .name = "Replace"
 .icon = "R"
 .shortcut = scR
 .cursor = 3
 .areanum = 6
END WITH
WITH toolinfo(9)
 .name = "Scroll"
 .icon = "S"
 .shortcut = scS
 .cursor = 2
 .areanum = 24
END WITH

DIM overlay as Frame ptr
overlay = frame_new(20, 20, , YES)
DIM overlaypal as Palette16 ptr
overlaypal = palette16_new()

DIM chequer_scroll as integer = 0
DIM tog as integer = 0
DIM tick as integer = 0
ts.lastcpos = TYPE(ts.x, ts.y)
ts.justpainted = 0
ts.didscroll = NO
ts.undo = 0
ts.allowundo = 0
ts.delay = 10
DIM zox as integer = ts.x * 8 + 4
DIM zoy as integer = ts.y * 8 + 4
DIM zcsr as integer
DIM overlay_use_palette as integer
DIM fgcol as integer
DIM bgcol as integer
mouse.x = area(0).x + zox
mouse.y = area(0).y + zoy
movemouse mouse.x, mouse.y
clearpage 2
'--Undo boxes
FOR i as integer = 0 TO 5
 edgebox 279, 9 + (i * 21), 22, 22, uilook(uiBackground), uilook(uiMenuItem), 2
NEXT i
refreshtileedit mover(), ts
textcolor uilook(uiMenuItem), 0
printstr ">", 270, 16 + (ts.undo * 21), 2
'--Draw master palette
FOR j as integer = 0 TO 7
 FOR i as integer = 0 TO 15
  rectangle i * 10, j * 4 + 160, 10, 4, j * 16 + i, 2
  rectangle i * 10 + 160, j * 4 + 160, 10, 4, j * 16 + i + 128, 2
 NEXT i
NEXT j
'--frame around the drawing area
rectangle 79, 0, 162, 160, uilook(uiText), 2
'---EDIT BLOCK---
setkeys
DO
 setwait 17, 110
 setkeys
 IF ts.gotmouse THEN
  mouse = readmouse
  zcsr = 0
  ts.zone = mouseover(mouse.x, mouse.y, zox, zoy, zcsr, area())
 END IF

 ts.delay = large(ts.delay - 1, 0)
 ts.justpainted = large(ts.justpainted - 1, 0)
 IF keyval(scEsc) > 1 THEN
  IF ts.hold = YES THEN
   ts.hold = NO
  ELSE
   EXIT DO
  END IF
 END IF
 IF keyval(scF1) > 1 THEN show_help "editmaptile"
 IF keyval(scAlt) = 0 THEN
  DIM fixmouse as integer = NO
  IF ts.tool <> scroll_tool THEN
   IF slowkey(scLeft, 100) THEN ts.x = large(ts.x - 1, 0): fixmouse = YES
   IF slowkey(scRight, 100) THEN ts.x = small(ts.x + 1, 19): fixmouse = YES
   IF slowkey(scUp, 100) THEN ts.y = large(ts.y - 1, 0): fixmouse = YES
   IF slowkey(scDown, 100) THEN ts.y = small(ts.y + 1, 19): fixmouse = YES
  ELSE
   DIM scrolloff as XYPair
   IF slowkey(scLeft, 100) THEN scrolloff.x = -1
   IF slowkey(scRight, 100) THEN scrolloff.x = 1
   IF slowkey(scUp, 100) THEN scrolloff.y = -1
   IF slowkey(scDown, 100) THEN scrolloff.y = 1
   scrolltile mover(), ts, scrolloff.x, scrolloff.y
   IF scrolloff.x OR scrolloff.y THEN fixmouse = YES
   ts.x = (ts.x + scrolloff.x + 20) MOD 20
   ts.y = (ts.y + scrolloff.y + 20) MOD 20
  END IF
  IF fixmouse AND ts.zone = 1 THEN
   zox = ts.x * 8 + 4
   zoy = ts.y * 8 + 4
   mouse.x = area(0).x + zox
   mouse.y = area(0).y + zoy
   movemouse mouse.x, mouse.y
  END IF
 END IF 
 IF keyval(scCtrl) > 0 AND keyval(scS) > 1 THEN
  storemxs game + ".til", ts.tilesetnum, vpages(3)
 END IF
 '---KEYBOARD SHORTCUTS FOR TOOLS------------
 IF keyval(scCtrl) = 0 THEN
  FOR i as integer = 0 TO UBOUND(toolinfo)
   IF keyval(toolinfo(i).shortcut) > 1 THEN
    tileedit_set_tool ts, toolinfo(), i
   END IF
  NEXT i
 END IF
 '----------
 IF keyval(scComma) > 1 OR (keyval(scAlt) > 0 AND keyval(scLeft) > 1) THEN
  ts.curcolor = (ts.curcolor + 255) MOD 256
  IF ts.curcolor MOD 16 = 15 THEN ts.curcolor = (ts.curcolor + 144) MOD 256
 END IF
 IF keyval(scPeriod) > 1 OR (keyval(scAlt) > 0 AND keyval(scRight) > 1) THEN
  ts.curcolor += 1
  IF ts.curcolor MOD 16 = 0 THEN ts.curcolor = (ts.curcolor + 112) MOD 256
 END IF
 IF keyval(scAlt) > 0 AND keyval(scUp) > 1 THEN ts.curcolor = (ts.curcolor + 240) MOD 256
 IF keyval(scAlt) > 0 AND keyval(scDown) > 1 THEN ts.curcolor = (ts.curcolor + 16) MOD 256
 IF keyval(scTilde) > 1 THEN ts.hidemouse = ts.hidemouse XOR 1
 IF keyval(scCtrl) > 0 AND keyval(scZ) > 1 AND ts.allowundo THEN
  ts.undo = loopvar(ts.undo, 0, 5, -1)
  readundoblock mover(), ts
  ts.didscroll = NO  'save a new undo block upon scrolling
 END IF
 IF keyval(scSpace) > 0 THEN clicktile mover(), ts, keyval(scSpace) AND 4, clone
 IF keyval(scEnter) > 1 THEN ts.curcolor = readpixel(ts.tilex * 20 + ts.x, ts.tiley * 20 + ts.y, 3)
 SELECT CASE ts.zone
 CASE 1
  'Drawing area
  ts.x = zox \ 8
  ts.y = zoy \ 8
  IF ts.tool = clone_tool THEN
   ' For clone brush tool, enter/right-click moves the handle point
   IF ts.readjust THEN
    IF keyval(scEnter) = 0 AND mouse.buttons = 0 THEN ' click or key release
     ts.readjust = NO
     clone.offset.x += (ts.x - ts.adjustpos.x)
     clone.offset.y += (ts.y - ts.adjustpos.y)
     ts.adjustpos.x = 0
     ts.adjustpos.y = 0
    END IF
   ELSE
    IF (keyval(scEnter) AND 5) ORELSE (mouse.buttons AND mouseRight) THEN
     ts.readjust = YES
     ts.adjustpos.x = ts.x
     ts.adjustpos.y = ts.y
    END IF
   END IF
  ELSEIF ts.tool = scroll_tool THEN
   'Handle scrolling by dragging the mouse
   'Did this drag start inside the sprite box? If not, ignore
   IF mouse.dragging ANDALSO mouseover(mouse.clickstart.x, mouse.clickstart.y, 0, 0, 0, area()) = 1 THEN
    scrolltile mover(), ts, ts.x - ts.lastcpos.x, ts.y - ts.lastcpos.y
   END IF
  ELSE
   'for all other tools, pick a color
   IF mouse.buttons AND mouseRight THEN
    ts.curcolor = readpixel(ts.tilex * 20 + ts.x, ts.tiley * 20 + ts.y, 3)
   END IF
  END IF
  IF mouse.buttons AND mouseLeft THEN clicktile mover(), ts, (mouse.clicks AND mouseLeft), clone
 CASE 2
  'Colour selector
  IF mouse.clicks AND mouseLeft THEN
   ts.curcolor = ((zoy \ 4) * 16) + ((zox MOD 160) \ 10) + (zox \ 160) * 128
  END IF
 CASE 13 TO 16
  IF mouse.clicks AND mouseLeft THEN fliptile mover(), ts
 END SELECT
 FOR i as integer = 0 TO UBOUND(toolinfo)
  IF toolinfo(i).areanum = ts.zone - 1 THEN
   IF mouse.clicks AND mouseLeft THEN
    tileedit_set_tool ts, toolinfo(), i
   END IF
  END IF
 NEXT i
 '--mouse over undo
 IF mouse.x >= 280 AND mouse.x < 300 THEN
  FOR i as integer = 0 TO 5
   IF mouse.y >= (10 + (i * 21)) AND mouse.y < (30 + (i * 21)) THEN
    IF (mouse.clicks AND mouseLeft) ANDALSO ts.allowundo THEN
     ts.undo = i
     readundoblock mover(), ts
    END IF
   END IF
  NEXT i
 END IF
 IF ts.tool = airbrush_tool THEN '--adjust airbrush
  IF mouse.buttons AND mouseLeft THEN
   IF ts.zone = 17 THEN ts.airsize = large(ts.airsize - tick, 1)
   IF ts.zone = 19 THEN ts.airsize = small(ts.airsize + tick, 30)
   IF ts.zone = 18 THEN ts.mist = large(ts.mist - tick, 1)
   IF ts.zone = 20 THEN ts.mist = small(ts.mist + tick, 99)
  END IF
  IF keyval(scMinus) > 1 OR keyval(scNumpadMinus) > 1 THEN
   IF keyval(scCtrl) > 0 THEN
    ts.mist = large(ts.mist - 1, 1)
   ELSE
    ts.airsize = large(ts.airsize - 1, 1)
   END IF
  END IF
  IF keyval(scPlus) > 1 OR keyval(scNumpadPlus) > 1 THEN
   IF keyval(scCtrl) > 0 THEN
    ts.mist = small(ts.mist + 1, 99)
   ELSE
    ts.airsize = small(ts.airsize + 1, 80)
   END IF
  END IF
 END IF
 IF keyval(scBackspace) > 1 OR keyval(scLeftBracket) > 1 OR keyval(scRightBracket) > 1 THEN fliptile mover(), ts
 DIM cy as integer = (ts.curcolor \ 16) MOD 8
 DIM cx as integer = (ts.curcolor AND 15) + (ts.curcolor \ 128) * 16
 ts.lastcpos = TYPE<XYPair>(ts.x, ts.y)

 '--Draw screen (Some of the editor is predrawn to page 2)
 copypage 2, dpage
 frame_draw_with_background ts.drawframe, NULL, 80, 0, 8, bgcolor, chequer_scroll, vpages(dpage)  'Draw the tile, at 8x zoom with background
 frame_clear overlay
 overlay_use_palette = YES  'OK, this is a bit of a hack
 overlaypal->col(1) = ts.curcolor

 rectangle cx * 10 + 4, cy * 4 + 162, 3, 1, IIF(tog, uilook(uiBackground), uilook(uiText)), dpage  'Selected colour marker
 'rectangle 60 + ts.x * 10, ts.y * 8, 10, 8, readpixel(ts.tilex * 20 + ts.x, ts.tiley * 20 + ts.y, 3), dpage  '??? Looks redundant
 'rectangle ts.x * 10 + 64, ts.y * 8 + 3, 3, 2, IIF(tog, uilook(uiBackground), uilook(uiText)), dpage  'Selecter pixel marker
 rectangle ts.x * 8 + 83, ts.y * 8 + 3, 3, 2, IIF(tog, uilook(uiBackground), uilook(uiText)), dpage  'Selected pixel marker
 IF ts.tool = airbrush_tool THEN
  IF tog THEN
   ellipse overlay, ts.x, ts.y, ts.airsize / 2 - 0.5, 1
   'paintat overlay, ts.x, ts.y, 1
  END IF
 END IF
 IF ts.tool = clone_tool AND tog = 0 THEN
  IF clone.exists = YES THEN
   overlay_use_palette = NO  'Don't use the palette, so colour 0 is drawn transparently
   FOR i as integer = 0 TO clone.size.y - 1
    FOR j as integer = 0 TO clone.size.x - 1
     spot.x = ts.x - clone.offset.x + j
     spot.y = ts.y - clone.offset.y + i
     IF ts.readjust = YES THEN
      spot.x -= (ts.x - ts.adjustpos.x)
      spot.y -= (ts.y - ts.adjustpos.y)
     END IF
     putpixel overlay, spot.x, spot.y, clone.buf(j, i)
    NEXT j
   NEXT i
  END IF
 END IF
 IF ts.hold = YES THEN
  DIM select_rect as RectType
  corners_to_rect_inclusive Type(ts.x, ts.y), ts.holdpos, select_rect
  SELECT CASE ts.tool
   CASE box_tool
    rectangle overlay, select_rect.x, select_rect.y, select_rect.wide, select_rect.high, 1
   CASE line_tool
    drawline overlay, ts.x, ts.y, ts.holdpos.x, ts.holdpos.y, 1
   CASE oval_tool
    ts.radius = SQR((ts.holdpos.x - ts.x)^2 + (ts.holdpos.y - ts.y)^2)
    IF ts.zone = 1 THEN
     'Use mouse pointer instead of draw cursor for finer grain control of radius
     ts.radius = SQR( (ts.holdpos.x - (mouse.x - area(0).x - 4) / 8)^2 + (ts.holdpos.y - (mouse.y - area(0).y - 4) / 8)^2 )
    END IF
    ellipse overlay, ts.holdpos.x, ts.holdpos.y, ts.radius, 1
   CASE mark_tool
    IF tog = 0 THEN drawbox overlay, select_rect.x, select_rect.y, select_rect.wide, select_rect.high, 15, 1
    overlaypal->col(15) = randint(10)
  END SELECT
 END IF
 frame_draw overlay, iif(overlay_use_palette, overlaypal, NULL), 80, 0, 8, YES, dpage  'Draw tool overlay, at 8x zoom

 textcolor uilook(uiText), uilook(uiHighlight)
 printstr toolinfo(ts.tool).name, 8, 8, dpage
 printstr "Tool", 8, 16, dpage
 printstr "Undo", 274, 1, dpage
 FOR i as integer = 0 TO UBOUND(toolinfo)
  fgcol = uilook(uiMenuItem)
  bgcol = uilook(uiDisabledItem)
  IF ts.tool = i THEN fgcol = uilook(uiText): bgcol = uilook(uiMenuItem)
  IF ts.zone - 1 = toolinfo(i).areanum THEN bgcol = uilook(uiSelectedDisabled)
  textcolor fgcol, bgcol
  printstr toolinfo(i).icon, area(toolinfo(i).areanum).x, area(toolinfo(i).areanum).y, dpage
 NEXT i
 FOR i as integer = 0 TO 3
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem): IF ts.zone = 13 + i THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
  printstr CHR(7 + i), 4 + i * 9, 42, dpage
 NEXT i
 IF ts.tool = airbrush_tool THEN
  textcolor uilook(uiMenuItem), 0
  printstr "SIZE", 12, 52, dpage
  printstr STR(ts.airsize), 20, 60, dpage
  printstr "MIST", 12, 68, dpage
  printstr STR(ts.mist), 20, 76, dpage
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem): IF ts.zone = 17 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
  printstr CHR(27), 12, 60, dpage
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem): IF ts.zone = 18 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
  printstr CHR(27), 12, 76, dpage
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem): IF ts.zone = 19 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
  printstr CHR(26), 36, 60, dpage
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem): IF ts.zone = 20 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
  printstr CHR(26), 36, 76, dpage
 END IF
 IF ts.gotmouse THEN
  DIM c as integer = zcsr
  IF c = -1 THEN
   c = ts.drawcursor
   IF ts.hidemouse THEN c = -2
  END IF
  IF tog THEN
   textcolor uilook(uiText), 0
  ELSE
   textcolor uilook(uiDescription), 0
  END IF
  printstr CHR(2 + c), mouse.x - 2, mouse.y - 2, dpage
 END IF
 SWAP dpage, vpage
 setvispage vpage
 tick = 0
 IF dowait THEN
  tick = 1
  tog = tog XOR 1
  chequer_scroll += 1
 END IF
LOOP
IF ts.gotmouse THEN
 movemouse ts.tilex * 20 + 10, ts.tiley * 20 + 10
END IF
frame_unload @overlay
palette16_unload @overlaypal
END SUB

SUB tileedit_set_tool (ts as TileEditState, toolinfo() as ToolInfoType, byval toolnum as integer)
 IF ts.tool <> toolnum AND toolnum = scroll_tool THEN ts.didscroll = NO
 ts.tool = toolnum
 ts.hold = NO
 ts.drawcursor = toolinfo(ts.tool).cursor + 1
END SUB

SUB clicktile (mover() as integer, ts as TileEditState, byval newkeypress as integer, byref clone as TileCloneBuffer)
DIM spot as XYPair

IF ts.delay > 0 THEN EXIT SUB
SELECT CASE ts.tool
 CASE draw_tool
  IF ts.justpainted = 0 THEN writeundoblock mover(), ts
  ts.justpainted = 3
  putpixel 280 + ts.x, 10 + (ts.undo * 21) + ts.y, ts.curcolor, 2
  rectangle ts.tilex * 20 + ts.x, ts.tiley * 20 + ts.y, 1, 1, ts.curcolor, 3
  refreshtileedit mover(), ts
 CASE box_tool
  IF newkeypress THEN
   IF ts.hold = YES THEN
    writeundoblock mover(), ts
    DIM select_rect as RectType
    corners_to_rect_inclusive Type(ts.x, ts.y), ts.holdpos, select_rect
    rectangle ts.tilex * 20 + select_rect.x, ts.tiley * 20 + select_rect.y, select_rect.wide, select_rect.high, ts.curcolor, 3
    refreshtileedit mover(), ts
    ts.hold = NO
   ELSE
    ts.hold = YES
    ts.holdpos.x = ts.x
    ts.holdpos.y = ts.y
   END IF
  END IF
 CASE line_tool
  IF newkeypress THEN
   IF ts.hold = YES THEN
    writeundoblock mover(), ts
    drawline ts.tilex * 20 + ts.x, ts.tiley * 20 + ts.y, ts.tilex * 20 + ts.holdpos.x, ts.tiley * 20 + ts.holdpos.y, ts.curcolor, 3
    refreshtileedit mover(), ts
    ts.hold = NO
   ELSE
    ts.hold = YES
    ts.holdpos.x = ts.x
    ts.holdpos.y = ts.y
   END IF
  END IF
 CASE fill_tool
  IF newkeypress THEN
   writeundoblock mover(), ts
   rectangle 0, 0, 22, 22, ts.curcolor, dpage
   FOR i as integer = 0 TO 19
    FOR j as integer = 0 TO 19
     putpixel 1 + i, 1 + j, readpixel(ts.tilex * 20 + i, ts.tiley * 20 + j, 3), dpage
    NEXT j
   NEXT i
   paintat vpages(dpage), 1 + ts.x, 1 + ts.y, ts.curcolor
   FOR i as integer = 0 TO 19
    FOR j as integer = 0 TO 19
     putpixel ts.tilex * 20 + i, ts.tiley * 20 + j, readpixel(1 + i, 1 + j, dpage), 3
    NEXT j
   NEXT i
   refreshtileedit mover(), ts
   rectangle 0, 0, 22, 22, uilook(uiBackground), dpage
  END IF
 CASE replace_tool
  IF newkeypress THEN
   writeundoblock mover(), ts
   replacecolor vpages(3), readpixel(ts.tilex * 20 + ts.x, ts.tiley * 20 + ts.y, 3), ts.curcolor, ts.tilex * 20, ts.tiley * 20, 20, 20
   refreshtileedit mover(), ts
  END IF
 CASE oval_tool
  IF newkeypress THEN
   IF ts.hold = YES THEN
    writeundoblock mover(), ts
    rectangle 0, 0, 22, 22, uilook(uiText), dpage
    FOR i as integer = 0 TO 19
     FOR j as integer = 0 TO 19
      putpixel 1 + i, 1 + j, readpixel(ts.tilex * 20 + i, ts.tiley * 20 + j, 3), dpage
     NEXT j
    NEXT i
    ellipse vpages(dpage), 1 + ts.holdpos.x, 1 + ts.holdpos.y, ts.radius, ts.curcolor
    FOR i as integer = 0 TO 19
     FOR j as integer = 0 TO 19
      putpixel ts.tilex * 20 + i, ts.tiley * 20 + j, readpixel(1 + i, 1 + j, dpage), 3
     NEXT j
    NEXT i
    refreshtileedit mover(), ts
    rectangle 0, 0, 22, 22, uilook(uiBackground), dpage
    ts.hold = NO
   ELSE
    ts.hold = YES
    ts.holdpos.x = ts.x
    ts.holdpos.y = ts.y
   END IF
  END IF
 CASE airbrush_tool
  IF ts.justpainted = 0 THEN writeundoblock mover(), ts
  ts.justpainted = 3
  rectangle 19, 119, 22, 22, uilook(uiText), dpage
  FOR i as integer = 0 TO 19
   FOR j as integer = 0 TO 19
    putpixel 20 + i, 120 + j, readpixel(ts.tilex * 20 + i, ts.tiley * 20 + j, 3), dpage
   NEXT j
  NEXT i
  airbrush 20 + ts.x, 120 + ts.y, ts.airsize, ts.mist, ts.curcolor, dpage
  FOR i as integer = 0 TO 19
   FOR j as integer = 0 TO 19
    putpixel ts.tilex * 20 + i, ts.tiley * 20 + j, readpixel(20 + i, 120 + j, dpage), 3
   NEXT j
  NEXT i
  refreshtileedit mover(), ts
 CASE mark_tool
  IF newkeypress THEN
   IF ts.hold = YES THEN
    DIM select_rect as RectType
    corners_to_rect_inclusive Type(ts.x, ts.y), ts.holdpos, select_rect
    clone.size.x = select_rect.wide
    clone.size.y = select_rect.high
    FOR i as integer = 0 TO clone.size.y - 1
     FOR j as integer = 0 TO clone.size.x - 1
      clone.buf(j, i) = readpixel(ts.tilex * 20 + select_rect.x + j, ts.tiley * 20 + select_rect.y + i, 3)
     NEXT j
    NEXT i
    clone.offset.x = clone.size.x \ 2
    clone.offset.y = clone.size.y \ 2
    ts.readjust = NO
    ts.adjustpos.x = 0
    ts.adjustpos.y = 0
    clone.exists = YES
    refreshtileedit mover(), ts
    ts.hold = NO
    ts.tool = clone_tool ' auto-select the clone tool after marking
   ELSE
    ts.hold = YES
    ts.holdpos.x = ts.x
    ts.holdpos.y = ts.y
   END IF
  END IF
 CASE clone_tool
  IF newkeypress THEN
   IF ts.justpainted = 0 THEN writeundoblock mover(), ts
   ts.justpainted = 3
   IF clone.exists = YES THEN
    FOR i as integer = 0 TO clone.size.y - 1
     FOR j as integer = 0 TO clone.size.x - 1
      spot.x = ts.x - clone.offset.x + j + ts.adjustpos.x
      spot.y = ts.y - clone.offset.y + i + ts.adjustpos.y
      IF spot.x >= 0 AND spot.x <= 19 AND spot.y >= 0 AND spot.y <= 19 AND clone.buf(j, i) > 0 THEN
       putpixel ts.tilex * 20 + spot.x, ts.tiley * 20 + spot.y, clone.buf(j, i), 3
      END IF
     NEXT j
    NEXT i
    refreshtileedit mover(), ts
   ELSE
    'if no clone buffer, switch to mark tool
    ts.tool = mark_tool
    ts.hold = YES
    ts.holdpos.x = ts.x
    ts.holdpos.y = ts.y
   END IF
  END IF
END SELECT
END SUB

SUB scrolltile (mover() as integer, ts as TileEditState, byval shiftx as integer, byval shifty as integer)
 'Save an undo before the first of a consecutive scrolls
 IF shiftx = 0 AND shifty = 0 THEN EXIT SUB
 IF ts.didscroll = NO THEN writeundoblock mover(), ts
 ts.didscroll = YES

 rectangle 0, 0, 20, 20, uilook(uiBackground), dpage
 DIM tempx as integer
 DIM tempy as integer
 FOR i as integer = 0 TO 19
  FOR j as integer = 0 TO 19
   tempx = (i + shiftx + 20) MOD 20
   tempy = (j + shifty + 20) MOD 20
   putpixel tempx, tempy, readpixel(ts.tilex * 20 + i, ts.tiley * 20 + j, 3), dpage
  NEXT j
 NEXT i
 FOR i as integer = 0 TO 19
  FOR j as integer = 0 TO 19
   putpixel ts.tilex * 20 + i, ts.tiley * 20 + j, readpixel(i, j, dpage), 3
  NEXT j
 NEXT i
 refreshtileedit mover(), ts
 rectangle 0, 0, 20, 20, uilook(uiBackground), dpage
END SUB

SUB fliptile (mover() as integer, ts as TileEditState)
writeundoblock mover(), ts
rectangle 0, 0, 20, 20, uilook(uiBackground), dpage
DIM flipx as integer = 0
DIM flipy as integer = 0
DIM tempx as integer
DIM tempy as integer
IF (ts.zone = 13 OR ts.zone = 16) OR keyval(scLeftBracket) > 1 OR (keyval(scBackspace) > 1 AND keyval(scCtrl) = 0) THEN flipx = 19
IF ts.zone = 14 OR ts.zone = 15 OR keyval(scRightBracket) > 1 OR (keyval(scBackspace) > 1 AND keyval(scCtrl) > 0) THEN flipy = 19
FOR i as integer = 0 TO 19
 FOR j as integer = 0 TO 19
  tempx = ABS(i - flipx)
  tempy = ABS(j - flipy)
  IF (ts.zone = 15 OR ts.zone = 16) OR (keyval(scLeftBrace) > 1 OR keyval(scRightBrace) > 1) THEN SWAP tempx, tempy
  putpixel tempx, tempy, readpixel(ts.tilex * 20 + i, ts.tiley * 20 + j, 3), dpage
 NEXT j
NEXT i
FOR i as integer = 0 TO 19
 FOR j as integer = 0 TO 19
  putpixel ts.tilex * 20 + i, ts.tiley * 20 + j, readpixel(i, j, dpage), 3
 NEXT j
NEXT i
refreshtileedit mover(), ts
rectangle 0, 0, 20, 20, uilook(uiBackground), dpage
END SUB

SUB tilecut (ts as TileEditState, mouse as MouseInfo)
DIM area(24) as MouseArea
'"Prev" button
area(10).x = 8
area(10).y = 190
area(10).w = 32
area(10).h = 10
'"Next" button
area(11).x = 280
area(11).y = 190
area(11).w = 32
area(11).h = 10

IF ts.gotmouse THEN
 movemouse ts.x, ts.y
END IF
ts.delay = 3
DIM previewticks as integer = 0
IF ts.cuttileset THEN
 loadmxs game + ".til", ts.cutfrom, vpages(2)
ELSE
 loadmxs game + ".mxs", ts.cutfrom, vpages(2)
END IF
DIM tog as integer
DIM zcsr as integer
DIM inc as integer
setkeys
DO
 setwait 110
 setkeys
 tog = tog XOR 1
 ts.delay = large(ts.delay - 1, 0)
 IF ts.gotmouse THEN
  mouse = readmouse
  zcsr = 0
  ts.zone = mouseover(mouse.x, mouse.y, 0, 0, zcsr, area())
  IF keyval(scAlt) > 0 THEN
   ts.x = mouse.x - mouse.x MOD 20
   ts.y = mouse.y - mouse.y MOD 20
  ELSE
   ts.x = small(mouse.x, 300)
   ts.y = small(mouse.y, 180)
  END IF
 END IF
 IF keyval(scESC) > 1 THEN
  EXIT DO
 END IF
 IF keyval(scF1) > 1 THEN show_help "tilecut"
 inc = 1: IF keyval(scLeftShift) OR keyval(scRightShift) THEN inc = 20
 IF keyval(scUp) AND 5 THEN ts.y = large(ts.y - inc, 0): IF ts.gotmouse THEN movemouse ts.x, ts.y
 IF keyval(scDown) AND 5 THEN ts.y = small(ts.y + inc, 180): IF ts.gotmouse THEN movemouse ts.x, ts.y
 IF keyval(scLeft) AND 5 THEN ts.x = large(ts.x - inc, 0): IF ts.gotmouse THEN movemouse ts.x, ts.y
 IF keyval(scRight) AND 5 THEN ts.x = small(ts.x + inc, 300): IF ts.gotmouse THEN movemouse ts.x, ts.y
 IF enter_or_space() OR (mouse.clicks > 0 AND ts.zone = 0) THEN
  IF ts.delay = 0 THEN
   FOR i as integer = 0 TO 19
    FOR j as integer = 0 TO 19
     putpixel ts.tilex * 20 + i, ts.tiley * 20 + j, readpixel(ts.x + i, ts.y + j, 2), 3
    NEXT j
   NEXT i
   IF keyval(scEnter) > 1 OR (mouse.clicks AND (mouseRight OR mouseMiddle)) THEN
    ts.tiley = (ts.tiley + (ts.tilex + 1) \ 16) MOD 10
    ts.tilex = (ts.tilex + 1) AND 15
    ts.x += 20
    IF ts.x > 300 THEN
     ts.x = 0
     ts.y += 20
     IF ts.y > 180 THEN ts.y = 0
    END IF
    IF ts.gotmouse THEN movemouse ts.x, ts.y
    previewticks = 6
   ELSE
    EXIT DO
   END IF
  END IF
 END IF
 '---PICK BACKGROUND PAGE------
 DIM oldcut as integer = ts.cutfrom
 DIM maxset as integer
 IF ts.cuttileset THEN
  maxset = gen(genMaxTile)
 ELSE
  maxset = gen(genNumBackdrops) - 1
 END IF
 intgrabber ts.cutfrom, 0, maxset, scLeftCaret, scRightCaret
 IF ts.zone = 11 AND mouse.clicks > 0 THEN ts.cutfrom = loopvar(ts.cutfrom, 0, maxset, -1)
 IF ts.zone = 12 AND mouse.clicks > 0 THEN ts.cutfrom = loopvar(ts.cutfrom, 0, maxset, 1)
 IF oldcut <> ts.cutfrom THEN
  IF ts.cuttileset THEN loadmxs game + ".til", ts.cutfrom, vpages(2) ELSE loadmxs game + ".mxs", ts.cutfrom, vpages(2)
 END IF
 '----
 IF previewticks THEN
  DIM preview as Frame ptr
  DIM previewy as integer = bound(ts.tiley * 20 - 20, 0, 140)
  preview = frame_new_view(vpages(3), 0, previewy, vpages(3)->w, 59)

  copypage 2, dpage
  IF ts.y < 100 THEN
   'preview 59 pixels of tileset at bottom of screen
   frame_draw preview, , 0, 141, , NO, dpage
   rectangle 0, 139, 320, 2, uilook(uiSelectedItem + tog), dpage
   drawbox ts.tilex * 20, 141 + ts.tiley * 20 - previewy, 20, 20, uilook(uiSelectedItem + tog), 1, dpage
  ELSE
   'tileset preview at top of screen
   frame_draw preview, , 0, 0, , NO, dpage
   rectangle 0, 59, 320, 2, uilook(uiSelectedItem + tog), dpage
   drawbox ts.tilex * 20, ts.tiley * 20 - previewy, 20, 20, uilook(uiSelectedItem + tog), 1, dpage
  END IF
  frame_unload @preview
  previewticks -= 1
 ELSE
  copypage 2, dpage
 END IF

 drawbox ts.x, ts.y, 20, 20, iif(tog, uilook(uiText), uilook(uiDescription)), 1, dpage
 textcolor uilook(uiMenuItem + tog), 1
 IF ts.zone = 11 THEN textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight)
 printstr "Prev", 8, 190, dpage
 textcolor uilook(uiMenuItem + tog), 1
 IF ts.zone = 12 THEN textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight)
 printstr "Next", 280, 190, dpage
 textcolor uilook(uiText), uilook(uiHighlight)
 DIM temp as string = ts.cutfrom & " "
 printstr temp, 160 - LEN(temp) * 4, 190, dpage
 IF ts.gotmouse THEN
  IF tog THEN
   textcolor uilook(uiText), 0
  ELSE
   textcolor uilook(uiDescription), 0
  END IF
  printstr CHR(2), mouse.x - 2, mouse.y - 2, dpage
 END IF
 SWAP dpage, vpage
 setvispage vpage
 dowait
LOOP
IF ts.gotmouse THEN
 movemouse ts.tilex * 20 + 10, ts.tiley * 20 + 10
END IF
END SUB

SUB tilecopy (cutnpaste() as integer, ts as TileEditState)
FOR i as integer = 0 TO 19
 FOR j as integer = 0 TO 19
  cutnpaste(i, j) = readpixel(ts.tilex * 20 + i, ts.tiley * 20 + j, 3)
 NEXT j
NEXT i
ts.canpaste = 1
END SUB

SUB tilepaste (cutnpaste() as integer, ts as TileEditState)
IF ts.canpaste THEN
 FOR i as integer = 0 TO 19
  FOR j as integer = 0 TO 19
   putpixel ts.tilex * 20 + i, ts.tiley * 20 + j, cutnpaste(i, j), 3
  NEXT j
 NEXT i
 IF slave_channel <> NULL_CHANNEL THEN storemxs game + ".til", ts.tilesetnum, vpages(3)
END IF 
END SUB

SUB tiletranspaste (cutnpaste() as integer, ts as TileEditState)
IF ts.canpaste THEN
 FOR i as integer = 0 TO 19
  FOR j as integer = 0 TO 19
   IF cutnpaste(i, j) THEN putpixel ts.tilex * 20 + i, ts.tiley * 20 + j, cutnpaste(i, j), 3
  NEXT j
 NEXT i
 IF slave_channel <> NULL_CHANNEL THEN storemxs game + ".til", ts.tilesetnum, vpages(3)
END IF
END SUB

SUB sprite (byval xw as integer, byval yw as integer, byref sets as integer, byval perset as integer, byval soff as integer, info() as string, byval zoom as integer, byval fileset as integer, byval fullset as integer=NO, byval cursor_start as integer=0, byval cursor_top as integer=0)
STATIC ss_save as SpriteEditStatic

DIM ss as SpriteEditState
WITH ss
 .fileset = fileset
 .spritefile = game & ".pt" & .fileset
 .framenum = 0
 .wide = xw
 .high = yw
 .perset = perset
 .size = .wide * .high / 2
 .setsize = .size * .perset
 .zoom = zoom
 .at_a_time = INT(200 / (.high + 5)) - 1
 .fullset = fullset
 '--Editor
 .x = 0
 .y = 0
 .zone.x = 0
 .zone.y = 0
 .hold = NO
 .gotmouse = havemouse()
 .didscroll = NO
 .drawcursor = 1
 .tool = draw_tool
 .airsize = 5
 .mist = 4
 .palindex = 1
 .previewpos.x = 319 - .wide
 .previewpos.y = 119
END WITH

DIM placer(2 + (ss.wide * ss.high * ss.perset) \ 4) as integer
DIM workpal(8 * (ss.at_a_time + 1)) as integer
REDIM poffset(large(sets, ss.at_a_time)) as integer
DIM as integer do_paste = 0
DIM as integer paste_transparent = 0
DIM as integer debug_palettes = 0
DIM caption as string
'FOR Loop counters
DIM as integer i, j, o

DIM state as MenuState
WITH state
 .pt = cursor_start
 .top = cursor_top
 .size = ss.at_a_time
END WITH

FOR i = 0 TO 15
 'nulpal is used for getsprite and can go away once we convert to use Frame
 poke8bit ss.nulpal(), i, i
NEXT i
loaddefaultpals ss.fileset, poffset(), sets
spriteedit_load_all_you_see state.top, sets, ss, soff, placer(), workpal(), poffset()

setkeys
DO
 setwait 55
 setkeys
 state.tog = state.tog XOR 1
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN
  IF ss.fullset THEN
   show_help "sprite_pickset_fullset"
  ELSE
   show_help "sprite_pickset"
  END IF
 END IF
 IF keyval(scCtrl) > 0 AND keyval(scBackspace) > 1 THEN
  spriteedit_save_all_you_see state.top, sets, ss, soff, placer(), workpal(), poffset()
  cropafter state.pt, sets, 0, ss.spritefile, ss.setsize
  clearpage 3
  spriteedit_load_all_you_see state.top, sets, ss, soff, placer(), workpal(), poffset()
 END IF
 IF enter_or_space() THEN
  spriteedit_save_all_you_see state.top, sets, ss, soff, placer(), workpal(), poffset()
  sprite_editor ss, ss_save, state, soff, workpal(), poffset(), info(), sets
  spriteedit_load_all_you_see state.top, sets, ss, soff, placer(), workpal(), poffset()
 END IF
 IF keyval(scCtrl) > 0 AND keyval(scF) > 1 THEN
  IF ss.fullset = NO AND ss.perset > 1 THEN
   spriteedit_save_all_you_see state.top, sets, ss, soff, placer(), workpal(), poffset()
   savedefaultpals ss.fileset, poffset(), sets
   sprite ss.wide * ss.perset, ss.high, sets, 1, soff, info(), 1, ss.fileset, YES, state.pt, state.top
   REDIM PRESERVE poffset(large(sets, ss.at_a_time))
   loaddefaultpals ss.fileset, poffset(), sets
   spriteedit_load_all_you_see state.top, sets, ss, soff, placer(), workpal(), poffset()
  END IF
 END IF
 DIM oldtop as integer = state.top
 IF intgrabber_with_addset(state.pt, 0, sets, 32767, "graphics", scUp, scDown) THEN
  IF state.pt > sets THEN
   sets = state.pt
   '--Add a new blank sprite set
   setpicstuf buffer(), ss.setsize, -1
   FOR i = 0 TO ss.setsize / 2
    buffer(i) = 0
   NEXT i
   storeset ss.spritefile, state.pt, 0
   '-- re-size the array that stores the default palette offset
   REDIM PRESERVE poffset(large(sets, ss.at_a_time))
   '--add a new blank default palette
   poffset(state.pt) = 0
   spriteedit_load_all_you_see state.top, sets, ss, soff, placer(), workpal(), poffset()
  END IF
  state.top = bound(state.top, state.pt - state.size, state.pt)
 ELSE
  state.last = sets
  usemenu state
 END IF
 IF oldtop <> state.top THEN
  spriteedit_save_all_you_see oldtop, sets, ss, soff, placer(), workpal(), poffset()
  spriteedit_load_all_you_see state.top, sets, ss, soff, placer(), workpal(), poffset()
 END IF
 IF keyval(scLeft) > 1 THEN ss.framenum = large(ss.framenum - 1, 0)
 IF keyval(scRight) > 1 THEN ss.framenum = small(ss.framenum + 1, ss.perset - 1)
 IF keyval(scLeftBrace) > 1 THEN
  changepal poffset(state.pt), -1, workpal(), state.pt - state.top
 END IF
 IF keyval(scRightBrace) > 1 THEN
  changepal poffset(state.pt), 1, workpal(), state.pt - state.top
 END IF
 '--copying
 IF copy_keychord() THEN 
  loadsprite ss_save.spriteclip(), 0, ss.framenum * ss.size, soff * (state.pt - state.top), ss.wide, ss.high, 3
  ss_save.paste = YES
  ss_save.clipsize.x = ss.wide
  ss_save.clipsize.y = ss.high
 END IF
 '--pasting
 do_paste = 0
 IF paste_keychord() AND ss_save.paste = YES THEN
  do_paste = -1
  paste_transparent = 0
 END IF
 IF (keyval(scCtrl) > 0 AND keyval(scT) > 1) AND ss_save.paste = YES THEN
  do_paste = -1
  paste_transparent = -1
 END IF
 IF do_paste THEN
  do_paste = 0
  loadsprite placer(), 0, ss.framenum * ss.size, soff * (state.pt - state.top), ss.wide, ss.high, 3
  rectangle 0, 0, ss.wide, ss.high, 0, dpage
  drawsprite placer(), 0, ss.nulpal(), 0, 0, 0, dpage
  IF NOT paste_transparent THEN
   rectangle 0, 0, ss_save.clipsize.x, ss_save.clipsize.y, 0, dpage
  END IF
  drawsprite ss_save.spriteclip(), 0, ss.nulpal(), 0, 0, 0, dpage
  getsprite placer(), 0, 0, 0, ss.wide, ss.high, dpage
  stosprite placer(), 0, ss.framenum * ss.size, soff * (state.pt - state.top), 3
  spriteedit_save_what_you_see(state.pt, state.top, sets, ss, soff, placer(), workpal(), poffset())
 END IF
 IF keyval(scF2) > 1 THEN
  debug_palettes = debug_palettes XOR 1
 END IF
 IF keyval(scE) > 1 THEN
  loadsprite placer(), 0, ss.framenum * ss.size, soff * (state.pt - state.top), ss.wide, ss.high, 3
  spriteedit_export spriteedit_export_name(ss, state), placer(), ss.nulpal(), poffset(state.pt)
 END IF
 'draw sprite sets
 rectangle 0, 0, 320, 200, uilook(uiDisabledItem), dpage
 rectangle 4 + (ss.framenum * (ss.wide + 1)), (state.pt - state.top) * (ss.high + 5), ss.wide + 2, ss.high + 2, uilook(uiText), dpage
 FOR i = state.top TO small(state.top + ss.at_a_time, sets)
  FOR o = 0 TO ss.perset - 1
   rectangle 5 + (o * (ss.wide + 1)), 1 + ((i - state.top) * (ss.high + 5)), ss.wide, ss.high, 0, dpage
   loadsprite placer(), 0, ss.size * o, soff * (i - state.top), ss.wide, ss.high, 3
   drawsprite placer(), 0, workpal(), (i - state.top) * 16, 5 + (o * (ss.wide + 1)), 1 + ((i - state.top) * (ss.high + 5)), dpage
  NEXT o
 NEXT i
 textcolor uilook(uiMenuItem), 0
 printstr "Palette " & poffset(state.pt), 320 - (LEN("Palette " & poffset(state.pt)) * 8), 0, dpage
 FOR i = 0 TO 15
  rectangle 271 + i * 3, 8, 3, 8, peek8bit(workpal(), (state.pt - state.top) * 16 + i), dpage
 NEXT i
 IF debug_palettes THEN
   FOR j = 0 TO ss.at_a_time
     FOR i = 0 TO 15
      rectangle 271 + i * 3, 40 + j * 5, 3, 4, peek8bit(workpal(), j * 16 + i), dpage
     NEXT i
   NEXT j
 END IF
 '--text captions
 caption = "Set " & state.pt
 printstr caption, 320 - (LEN(caption) * 8), 16, dpage
 caption = info(ss.framenum)
 printstr caption, 320 - (LEN(caption) * 8), 24, dpage
 caption = ""
 IF ss.fullset = NO AND ss.perset > 1 THEN
  caption = "CTRL+F Full-Set Mode, "
 ELSEIF ss.fullset = YES THEN
  caption = "ESC back to Single-Frame Mode, "
 END IF
 caption = caption & "F1 Help"
 printstr caption, 320 - (LEN(caption) * 8), 192, dpage
 '--screen update
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
changepal poffset(state.pt), 0, workpal(), state.pt - state.top
spriteedit_save_all_you_see state.top, sets, ss, soff, placer(), workpal(), poffset()
savedefaultpals ss.fileset, poffset(), sets
'sprite_empty_cache
'Robust against sprite leaks
IF ss.fileset > -1 THEN sprite_update_cache_pt ss.fileset

clearpage 0
clearpage 1
clearpage 2
clearpage 3
EXIT SUB

END SUB '----END of sprite()

SUB spriteedit_clip (placer() as integer, ss as SpriteEditState)
 'clip possibly rotated sprite buffer to sprite's frame size
 DIM holdscreen as integer
 holdscreen = allocatepage
 drawsprite placer(), 0, ss.nulpal(), 0, 0, 0, holdscreen
 getsprite placer(), 0, 0, 0, ss.wide, ss.high, holdscreen
 freepage holdscreen
END SUB

SUB writeundospr (placer() as integer, ss as SpriteEditState, is_rotate as integer=NO)
 IF placer(0) <> ss.wide OR placer(1) <> ss.high THEN
  IF is_rotate THEN
   '--if we haven't done anything since the last rotate, skip this undo write entirely
   EXIT SUB
  END IF
  'This is a hack to compensate for the fact that the sprite buffer
  'remains in a rotated state after a rotation operation
  spriteedit_clip placer(), ss
 END IF
 stosprite placer(), 0, ss.undoslot * ss.size, 100, 3
 ss.undoslot = loopvar(ss.undoslot, 0, ss.undomax, 1)
 ss.undodepth = small(ss.undodepth + 1, ss.undomax + 1)
END SUB

SUB spriteedit_display(ss as SpriteEditState, ss_save as SpriteEditStatic, state as MenuState, placer() as integer, workpal() as integer, poffset() as integer, info() as string, toolinfo() as ToolInfoType, area() as MouseArea, mouse as MouseInfo)
 ss.curcolor = peek8bit(workpal(), ss.palindex + (state.pt - state.top) * 16)
 rectangle 247 + ((ss.curcolor - ((ss.curcolor \ 16) * 16)) * 4), 0 + ((ss.curcolor \ 16) * 6), 5, 7, uilook(uiText), dpage
 DIM as integer i, o
 FOR i = 0 TO 15
  FOR o = 0 TO 15
   rectangle 248 + (i * 4), 1 + (o * 6), 3, 5, o * 16 + i, dpage
  NEXT o
 NEXT i
 textcolor uilook(uiText), uilook(uiDisabledItem): IF ss.zonenum = 5 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
 printstr CHR(27), 248, 100, dpage
 textcolor uilook(uiText), uilook(uiDisabledItem): IF ss.zonenum = 6 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
 printstr CHR(26), 304, 100, dpage
 textcolor uilook(uiText), 0
 IF ss.showcolnum > 0 THEN
  printstr " Col" & ss.curcolor, 248, 100, dpage
  IF keyval(scAlt) = 0 THEN
   ss.showcolnum -= 1
  END IF
 ELSE
  printstr LEFT(" Pal", 4 - (LEN(STR(poffset(state.pt))) - 3)) & poffset(state.pt), 248, 100, dpage
 END IF
 rectangle 247 + (ss.palindex * 4), 110, 5, 7, uilook(uiText), dpage
 FOR i = 0 TO 15
  rectangle 248 + (i * 4), 111, 3, 5, peek8bit(workpal(), i + (state.pt - state.top) * 16), dpage
 NEXT
 drawspritex placer(), 0, workpal(), (state.pt - state.top) * 16, 4, 1, dpage, ss.zoom, NO
 ss.curcolor = peek8bit(workpal(), ss.palindex + (state.pt - state.top) * 16)

 DIM select_rect as RectType
 corners_to_rect_inclusive Type(ss.x, ss.y), ss.holdpos, select_rect

 IF ss.hold = YES AND ss.tool = box_tool THEN
  rectangle 4 + select_rect.x * ss.zoom, 1 + select_rect.y * ss.zoom, select_rect.wide * ss.zoom, select_rect.high * ss.zoom, ss.curcolor, dpage
  rectangle 4 + ss.holdpos.x * ss.zoom, 1 + ss.holdpos.y * ss.zoom, ss.zoom, ss.zoom, IIF(state.tog, uilook(uiBackground), uilook(uiText)), dpage
 END IF
 drawsprite placer(), 0, workpal(), (state.pt - state.top) * 16, ss.previewpos.x, ss.previewpos.y, dpage, 0

 DIM overlay as Frame ptr
 overlay = frame_new(ss.wide, ss.high, , YES)
 'hack: We don't draw real palette colours to overlay, otherwise we couldn't draw colour 0
 DIM pal16 as Palette16 ptr
 pal16 = palette16_new()
 pal16->col(1) = ss.curcolor

 IF ss.hold = YES AND ss.tool = box_tool THEN
  rectangle ss.previewpos.x + select_rect.x, ss.previewpos.y + select_rect.y, select_rect.wide, select_rect.high, ss.curcolor, dpage
  putpixel ss.previewpos.x + ss.holdpos.x, ss.previewpos.y + ss.holdpos.y, state.tog * 15, dpage
 END IF
 IF ss.hold = YES AND ss.tool = line_tool THEN
  drawline ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, ss.previewpos.x + ss.holdpos.x, ss.previewpos.y + ss.holdpos.y, ss.curcolor, dpage
  drawline overlay, ss.x, ss.y, ss.holdpos.x, ss.holdpos.y, 1
 END IF
 IF ss.hold = YES AND ss.tool = oval_tool THEN
  ellipse vpages(dpage), ss.previewpos.x + ss.holdpos.x, ss.previewpos.y + ss.holdpos.y, ss.radius, ss.curcolor, , ss.ellip_minoraxis, ss.ellip_angle
  ellipse overlay, ss.holdpos.x, ss.holdpos.y, ss.radius, 1, , ss.ellip_minoraxis, ss.ellip_angle
 END IF
 IF ss.tool = airbrush_tool THEN
  ellipse vpages(dpage), ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, ss.airsize / 2, ss.curcolor
  ellipse vpages(dpage), 5 + (ss.x * ss.zoom), 2 + (ss.y * ss.zoom), (ss.airsize / 2) * ss.zoom, ss.curcolor
 END IF

 frame_draw overlay, pal16, 4, 1, ss.zoom, YES, dpage
 frame_unload @overlay
 palette16_unload @pal16

 IF ss.tool <> clone_tool THEN
  'Pixel at cursor position
  rectangle 4 + (ss.x * ss.zoom), 1 + (ss.y * ss.zoom), ss.zoom, ss.zoom, IIF(state.tog, uilook(uiBackground), uilook(uiText)), dpage
  putpixel ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, state.tog * 15, dpage
 END IF
 IF ss.hold = YES AND ss.tool = mark_tool AND state.tog = 0 THEN
  ss.curcolor = randint(255) ' Random color when marking a clone region
  drawbox 4 + select_rect.x * ss.zoom, 1 + select_rect.y * ss.zoom, select_rect.wide * ss.zoom, select_rect.high * ss.zoom, ss.curcolor, ss.zoom, dpage
  drawbox ss.previewpos.x + select_rect.x, ss.previewpos.y + select_rect.y, select_rect.wide, select_rect.high, ss.curcolor, 1, dpage
 END IF
 DIM temppos as XYPair
 IF ss.tool = clone_tool AND ss_save.clonemarked = YES AND state.tog = 0 THEN
  temppos.x = ss.x - ss_save.clonepos.x
  temppos.y = ss.y - ss_save.clonepos.y
  IF ss.readjust THEN
   temppos.x += (ss.adjustpos.x - ss.x)
   temppos.y += (ss.adjustpos.y - ss.y)
  END IF
  drawspritex ss_save.clonebuf(), 0, workpal(), (state.pt - state.top) * 16, 4 + temppos.x * ss.zoom, 1 + temppos.y * ss.zoom, dpage, ss.zoom
  drawsprite ss_save.clonebuf(), 0, workpal(), (state.pt - state.top) * 16, ss.previewpos.x + temppos.x, ss.previewpos.y + temppos.y, dpage
 END IF
 textcolor uilook(uiMenuItem), 0
 printstr "x=" & ss.x & " y=" & ss.y, 0, 190, dpage
 printstr "Tool:" & toolinfo(ss.tool).name, 0, 182, dpage
 printstr info(ss.framenum), 0, 174, dpage
 FOR i = 0 TO UBOUND(toolinfo)
  spriteedit_draw_icon ss, toolinfo(i).icon, area(), toolinfo(i).areanum, (ss.tool = i)
 NEXT i
 spriteedit_draw_icon ss, CHR(7), area(), 3  'horizontal flip
 spriteedit_draw_icon ss, "I", area(), 12
 spriteedit_draw_icon ss, "E", area(), 25

 IF ss.undodepth = 0 THEN
  textcolor uilook(uiBackground), uilook(uiDisabledItem)
 ELSE
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem)
 END IF
 IF ss.zonenum = 20 AND ss.undodepth > 0 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
 printstr "UNDO", 170, 182, dpage

 IF ss.tool = airbrush_tool THEN
  textcolor uilook(uiMenuItem), 0
  printstr "SIZE" & ss.airsize, 228, 182, dpage
  printstr "MIST" & ss.mist, 228, 190, dpage
  spriteedit_draw_icon ss, CHR(27), area(), 14
  spriteedit_draw_icon ss, CHR(27), area(), 15
  spriteedit_draw_icon ss, CHR(26), area(), 16
  spriteedit_draw_icon ss, CHR(26), area(), 17
 END IF
 IF ss.tool <> airbrush_tool THEN
  textcolor uilook(uiMenuItem), 0
  printstr "ROTATE", 228, 190, dpage
  spriteedit_draw_icon ss, CHR(27), area(), 15
  spriteedit_draw_icon ss, CHR(26), area(), 17
 END IF
 IF ss.gotmouse THEN
  IF ss.zonecursor = -1 THEN
   IF ss.hidemouse THEN ss.zonecursor = -2 ELSE ss.zonecursor = ss.drawcursor
  END IF
  IF state.tog THEN
   textcolor uilook(uiText), 0
  ELSE
   textcolor uilook(uiDescription), 0
  END IF
  printstr CHR(2 + ss.zonecursor), mouse.x - 2, mouse.y - 2, dpage
 END IF
END SUB

'Draw one of the clickable areas (obviously this will all be replaced with slices eventually)
SUB spriteedit_draw_icon(ss as SpriteEditState, icon as string, area() as MouseArea, byval areanum as integer, byval highlight as integer = NO)
 DIM bgcol as integer
 DIM fgcol as integer
 fgcol = uilook(uiMenuItem)
 bgcol = uilook(uiDisabledItem)
 IF highlight THEN
  fgcol = uilook(uiText)
  bgcol = uilook(uiMenuItem)
 END IF
 IF ss.zonenum = areanum + 1 THEN bgcol = uilook(uiSelectedDisabled)
 textcolor fgcol, bgcol
 printstr icon, area(areanum).x, area(areanum).y, dpage
END SUB

SUB init_sprite_zones(area() as MouseArea, ss as SpriteEditState)
 DIM i as integer
 'DRAWING ZONE
 area(0).x = 4
 area(0).y = 1
 area(0).hidecursor = YES
 'PALETTE ZONE
 area(1).x = 248
 area(1).y = 111
 area(1).w = 64
 area(1).h = 6
 area(1).hidecursor = NO
 'MASTER PAL ZONE
 area(2).x = 248
 area(2).y = 1
 area(2).w = 64
 area(2).h = 96
 area(2).hidecursor = NO
 'FLIP BUTTON
 area(3).x = 184
 area(3).y = 190
 area(3).w = 8
 area(3).h = 10
 area(3).hidecursor = NO
 'PREV PAL BUTTON
 area(4).x = 248
 area(4).y = 100
 area(4).w = 8
 area(4).h = 8
 area(4).hidecursor = NO
 'NEXT PAL BUTTON
 area(5).x = 304
 area(5).y = 100
 area(5).w = 8
 area(5).h = 8
 area(5).hidecursor = NO
 'TOOL BUTTONS (more below at 21-24)
 FOR i = 0 TO 5
  area(6 + i).x = 80 + i * 10
  area(6 + i).y = 190
  area(6 + i).w = 8
  area(6 + i).h = 10
  area(6 + i).hidecursor = NO
 NEXT i
 'IMPORT BUTTON
 area(12).x = 196
 area(12).y = 190
 area(12).w = 8
 area(12).h = 10
 area(12).hidecursor = NO
 'SMALL DRAWING AREA
 area(13).x = ss.previewpos.x
 area(13).y = ss.previewpos.y
 area(13).hidecursor = YES
 'LESS AIRBRUSH AREA
 area(14).x = 220
 area(14).y = 182
 area(14).w = 8
 area(14).h = 8
 area(14).hidecursor = NO
 'LESS AIRBRUSH MIST
 area(15).x = 220
 area(15).y = 190
 area(15).w = 8
 area(15).h = 8
 area(15).hidecursor = NO
 'MORE AIRBRUSH AREA
 area(16).x = 276
 area(16).y = 182
 area(16).w = 8
 area(16).h = 8
 area(16).hidecursor = NO
 'MORE AIRBRUSH MIST
 area(17).x = 276
 area(17).y = 190
 area(17).w = 8
 area(17).h = 8
 area(17).hidecursor = NO
 'PALETTE NUMBER
 area(18).x = 256
 area(18).y = 100
 area(18).w = 48
 area(18).h = 8
 area(18).hidecursor = NO
 'UNDO BUTTON
 area(19).x = 170
 area(19).y = 182
 area(19).w = 32
 area(19).h = 8
 area(19).hidecursor = NO
 'area 20 is what???
 'MORE TOOLS
 FOR i = 0 TO 3
  area(21 + i).x = 140 + i * 10
  area(21 + i).y = 190
  area(21 + i).w = 8
  area(21 + i).h = 10
  area(21 + i).hidecursor = NO
 NEXT i
 'EXPORT BUTTON
 area(25).x = 206
 area(25).y = 190
 area(25).w = 8
 area(25).h = 10
 area(25).hidecursor = NO

END SUB

SUB spriteedit_save_all_you_see(byval top as integer, byval sets as integer, ss as SpriteEditState, byval soff as integer, placer() as integer, workpal() as integer, poffset() as integer)
 FOR j as integer = top TO top + ss.at_a_time
  spriteedit_save_what_you_see(j, top, sets, ss, soff, placer(), workpal(), poffset()) 
 NEXT j
END SUB

SUB spriteedit_load_all_you_see(byval top as integer, byval sets as integer, ss as SpriteEditState, byval soff as integer, placer() as integer, workpal() as integer, poffset() as integer)
 FOR j as integer = top TO top + ss.at_a_time
  spriteedit_load_what_you_see(j, top, sets, ss, soff, placer(), workpal(), poffset())
 NEXT j
END SUB

SUB spriteedit_load_what_you_see(byval j as integer, byval top as integer, byval sets as integer, ss as SpriteEditState, byval soff as integer, placer() as integer, workpal() as integer, poffset() as integer)
 DIM i as integer
 getpal16 workpal(), j - top, poffset(j)
 IF j <= sets THEN
  setpicstuf buffer(), ss.setsize, 2
  loadset ss.spritefile, large(j, 0), 0
  FOR i = 0 TO (ss.perset - 1)
   loadsprite placer(), 0, ss.size * i, 0, ss.wide, ss.high, 2
   stosprite placer(), 0, ss.size * i, soff * (j - top), 3
  NEXT i
 END IF
END SUB

SUB spriteedit_save_what_you_see(byval j as integer, byval top as integer, byval sets as integer, ss as SpriteEditState, byval soff as integer, placer() as integer, workpal() as integer, poffset() as integer)
 DIM i as integer
 IF j <= sets THEN
  setpicstuf buffer(), ss.setsize, 2
  FOR i = 0 TO (ss.perset - 1)
   loadsprite placer(), 0, ss.size * i, soff * (j - top), ss.wide, ss.high, 3
   stosprite placer(), 0, ss.size * i, 0, 2
  NEXT i
  storeset ss.spritefile, large(j, 0), 0
 END IF
END SUB

FUNCTION spriteedit_export_name (ss as SpriteEditState, state as MenuState) as string
 DIM s as string
 s = trimpath(trimextension(sourcerpg)) & " " & exclude(LCASE(sprite_sizes(ss.fileset).name), " ")
 IF ss.fullset THEN
  s &= " set " & state.pt
 ELSE
  s &= " " & state.pt
  IF ss.perset > 1 THEN s &= " frame " & ss.framenum
 END IF
 RETURN s
END FUNCTION

SUB spriteedit_export(default_name as string, placer() as integer, nulpal() as integer, palnum as integer)
 'palnum is offset into pal lump of the 16 color palette this sprite should use

 DIM img as GraphicPair
 img.sprite = frame_new(placer(0), placer(1), 1, YES, NO)
 img.pal = palette16_load(palnum)

 DIM pg as integer
 pg = registerpage(img.sprite)
 drawsprite placer(), 0, nulpal(), 0, 0, 0, pg
 freepage pg
 
 '--hand off the frame and palette to the real export function
 spriteedit_export default_name, img
 
 '--cleanup
 frame_unload @(img.sprite)
 palette16_unload @(img.pal)
END SUB

SUB spriteedit_export(default_name as string, img as GraphicPair)
 DIM outfile as string
 outfile = inputfilename("Export to bitmap file", ".bmp", "", "input_file_export_sprite", default_name)
 IF outfile <> "" THEN
  frame_export_bmp4 outfile & ".bmp", img.sprite, master(), img.pal
 END IF
END SUB

SUB frame_to_4bit_buffer(byval spr as Frame ptr, buf() as integer, byval wid as integer, byval high as integer)
 'Allocate a 320x200 page instead of just registering spr as a page, because it might be smaller than wid*high
 DIM holdscreen as integer
 holdscreen = allocatepage
 frame_draw spr, NULL, 0, 0, 1, YES, holdscreen
 getsprite buf(), 0, 0, 0, wid, high, holdscreen
 freepage holdscreen
END SUB

'Load a BMP of any bitdepth into a Frame which has just 16 colours: those in pal16
SUB spriteedit_import16_loadbmp(byref ss as SpriteEditState, workpal() as integer, byval palno as integer, srcbmp as string, byref impsprite as Frame ptr, byref pal16 as Palette16 ptr)
 pal16 = palette16_new()

 DIM bmpd as BitmapInfoHeader
 bmpinfo(srcbmp, bmpd)
 'debuginfo "import16_load: bitdepth " & bmpd.biBitCount

 'Map from impsprite colors to master pal indices
 DIM bmppal(255) as integer
 'Put color index hints in bmppal(), which are used if they are an exact match.
 FOR i as integer = 0 TO 15
  bmppal(i) = peek8bit(workpal(), i + palno * 16)
 NEXT

 IF bmpd.biBitCount <= 4 THEN
  'If 4 bit or below, we preserve the colour indices from the BMP

  impsprite = frame_import_bmp_raw(srcbmp)

  convertbmppal(srcbmp, master(), bmppal())
  FOR i as integer = 0 TO 15
   pal16->col(i) = bmppal(i)
  NEXT 

 ELSE
  'For higher bitdepths, try to shift used colour indices into 0-15

  'map from impsprite colors to master pal indices
  DIM bmppal(255) as integer

  IF bmpd.biBitCount > 8 THEN
   'notification srcbmp & " is a " & bmpd.biBitCount & "-bit BMP file. Colors will be mapped to the nearest entry in the master palette." _
   '             " It's recommended that you save your graphics as 4-bit BMPs so that you can control which colours are used."
   impsprite = frame_import_bmp24_or_32(srcbmp, master())
   FOR i as integer = 0 TO 255
    bmppal(i) = i
   NEXT
  ELSE  'biBitCount = 8
   impsprite = frame_import_bmp_raw(srcbmp)
   convertbmppal(srcbmp, master(), bmppal())
  END IF

  IF impsprite <> NULL THEN
   'First special case (intended for 8 bit bmps): don't do any remapping if the indices are already 0-15

   DIM require_remap as bool = NO
   FOR x as integer = 0 TO impsprite->w - 1
    FOR y as integer = 0 TO impsprite->h - 1
     IF readpixel(impsprite, x, y) >= 16 THEN require_remap = YES
    NEXT
   NEXT

   IF require_remap = NO THEN
    FOR i as integer = 0 TO 15
     pal16->col(i) = bmppal(i)
    NEXT i
   ELSE

    'Find the set of colours used in impsprite, and remap impsprite
    'to those colour indices
    DIM vpal16 as integer vector
    v_new vpal16
    'v_append vpal16, 0

    FOR x as integer = 0 TO impsprite->w - 1   'small(impsprite->w, ss.wide) - 1
     FOR y as integer = 0 TO impsprite->h - 1  'small(impsprite->h, ss.high) - 1
      DIM col as integer = bmppal(readpixel(impsprite, x, y))
      DIM at as integer = v_find(vpal16, col)
      IF at = -1 THEN
       v_append vpal16, col
       col = v_len(vpal16) - 1
      ELSE
       col = at
      END IF
      putpixel(impsprite, x, y, col)
     NEXT
    NEXT
    debuginfo srcbmp & " contains " & v_len(vpal16) & " colors"

    IF v_len(vpal16) > 16 THEN
     notification "This image contains " & v_len(vpal16) & " colors (after finding nearest-matches to the master palette). At most 16 are allowed." _
                  " Reduce the number of colors and try importing again."
     palette16_unload @pal16
     frame_unload @impsprite
     v_free vpal16
     EXIT SUB
    ELSE
     FOR i as integer = 0 TO v_len(vpal16) - 1
      pal16->col(i) = vpal16[i]
     NEXT i
    END IF

    v_free vpal16
   END IF
  END IF
 END IF

 IF impsprite = NULL THEN
  notification "Could not load " & srcbmp
  palette16_unload @pal16
  EXIT SUB
 END IF
END SUB

'Lets the use pick one of the colour/pixels in impsprite, returns the colour index
'Returns -1 is cancelled
FUNCTION spriteedit_import16_pick_bgcol(byref ss as SpriteEditState, byref impsprite as Frame ptr, byref pal16 as Palette16 ptr) as integer
 DIM tog as integer
 DIM pickpos as XYPair
 DIM picksize as XYPair
 pickpos.x = 0
 pickpos.y = 0
 picksize.x = small(320, small(impsprite->w, ss.wide))
 picksize.y = small(200, small(impsprite->h, ss.high))

 setkeys
 DO
  setwait 55
  setkeys
  tog XOR= 1
  IF keyval(scESC) > 1 THEN RETURN -1
  IF keyval(scF1) > 1 THEN show_help "frame_import16_pickbackground"
  IF enter_or_space() THEN EXIT DO

  DIM movespeed as integer
  IF keyval(scALT) THEN movespeed = 9 ELSE movespeed = 1
  IF keyval(scUp) > 0 THEN pickpos.y = large(pickpos.y - movespeed, 0)
  IF keyval(scDown) > 0 THEN pickpos.y = small(pickpos.y + movespeed, picksize.y - 1)
  IF keyval(scleft) > 0 THEN pickpos.x = large(pickpos.x - movespeed, 0)
  IF keyval(scRight) > 0 THEN pickpos.x = small(pickpos.x + movespeed, picksize.x - 1)

  clearpage dpage
  frame_draw impsprite, pal16, 4, 1, ss.zoom, NO, dpage
  'Draw box around the proportion of the image that will be used
  drawbox 3, 0, ss.wide * ss.zoom + 2, ss.high * ss.zoom + 2, uilook(uiText), 1, dpage

  '--Draw the pixel cursor
  DIM col as integer
  IF tog THEN col = uilook(uiBackground) ELSE col = uilook(uiText)
  IF ss.zoom = 1 THEN
   'A single pixel is too small
   textcolor col, 0
   printstr CHR(5), 4 + pickpos.x - 2, 1 + pickpos.y - 2, dpage
  ELSE
   rectangle 4 + pickpos.x * ss.zoom, 1 + pickpos.y * ss.zoom, ss.zoom, ss.zoom, col, dpage
  END IF

  edgeprint "Pick background (transparent) color", 0, 190, uilook(uiMenuItem), dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

 RETURN readpixel(impsprite, pickpos.x, pickpos.y)
END FUNCTION

'Set can_remap to whether a mapping from pal16 to workpal() exists,
'and if so write it in palmapping().
SUB spriteedit_import16_compare_palettes(workpal() as integer, byval palno as integer, byval pal16 as Palette16 ptr, palmapping() as integer, byref can_remap as bool, byref is_identical as bool)
 DIM existingpal(15) as integer
 FOR i as integer = 0 TO 15
  existingpal(i) = peek8bit(workpal(), i + palno * 16)
 NEXT

 can_remap = YES
 is_identical = YES
 FOR i as integer = 1 TO 15
  'IF pal16->col(i) <> existingpal(i) THEN is_identical = NO
  IF color_distance(master(), pal16->col(i), existingpal(i)) > 0 THEN is_identical = NO
  DIM found as bool = NO
  FOR j as integer = 1 TO 15
   'IF pal16->col(i) = existingpal(j) THEN
   IF color_distance(master(), pal16->col(i), existingpal(j)) = 0 THEN
    palmapping(i) = j
    found = YES
    EXIT FOR
   END IF
  NEXT
  IF found = NO THEN can_remap = NO
 NEXT
END SUB

'Return value: see retval()
'Also returns contents of palmapping()
FUNCTION spriteedit_import16_remap_menu(byref ss as SpriteEditState, byref ss_save as SpriteEditStatic, byref state as MenuState, workpal() as integer, poffset() as integer, byref impsprite as Frame ptr, byref pal16 as Palette16 ptr, palmapping() as integer) as integer
 DIM can_remap as bool
 DIM is_identical as bool
 DIM usepal as Palette16 ptr = palette16_new()
 DIM ret as integer

 DIM pmenu(2) as string
 DIM retval(2) as integer
 DIM palstate as MenuState 
 palstate.pt = 0
 palstate.last = 2
 palstate.size = 2
 palstate.need_update = YES
 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scESC) > 1 THEN ret = 3 : EXIT DO
  IF keyval(scF1) > 1 THEN show_help "frame_import16"
  IF keyval(scLeft) > 1 OR keyval(scLeftBrace) > 1 THEN
   changepal poffset(state.pt), -1, workpal(), state.pt - state.top
   palstate.need_update = YES
  END IF
  IF keyval(scRight) > 1 OR keyval(scRightBrace) > 1 THEN
   changepal poffset(state.pt), 1, workpal(), state.pt - state.top
   palstate.need_update = YES
  END IF
  IF usemenu(palstate) THEN palstate.need_update = YES
  IF enter_space_click(palstate) THEN ret = retval(palstate.pt) : EXIT DO

  IF palstate.need_update THEN
   palstate.need_update = NO
   spriteedit_import16_compare_palettes workpal(), state.pt - state.top, pal16, palmapping(), can_remap, is_identical

   pmenu(0) = "Overwrite Current Palette"
   retval(0) = 0
   IF can_remap THEN
    pmenu(1) = "Remap into Current Palette"
    retval(1) = 1
   ELSE
    pmenu(1) = "Import Without Palette"
    retval(1) = 2
   END IF 
   pmenu(2) = "Cancel Import"
   retval(2) = 3

   IF palstate.pt = 1 AND can_remap = NO THEN
    'Preview import without palette
    FOR i as integer = 0 TO 15
     usepal->col(i) = peek8bit(workpal(), i + (state.pt - state.top) * 16)
    NEXT
   ELSE
    FOR i as integer = 0 TO 15
     usepal->col(i) = pal16->col(i)
    NEXT
   END IF
  END IF

  'Page 2 has rectangles around the sprite and preview and palette
  copypage 2, dpage
  frame_draw impsprite, usepal, 4, 1, ss.zoom, NO, dpage
  frame_draw impsprite, usepal, ss.previewpos.x, ss.previewpos.y, 1, NO, dpage

  'Draw palettes
  textcolor uilook(uiText), 0
  printstr bgcol_text(CHR(27), uilook(uiDisabledItem)) _
           & rpad("Pal"  & poffset(state.pt), " ", 6) _
           & bgcol_text(CHR(26), uilook(uiDisabledItem)), 248, 100, dpage, YES
  drawbox 246, 109, 67, 8, uilook(uiText), 1, dpage
  FOR i as integer = 0 TO 15
   rectangle 248 + (i * 4), 111, 3, 5, peek8bit(workpal(), i + (state.pt - state.top) * 16), dpage
  NEXT
  printstr "Image Pal", 248, 80, dpage
  drawbox 246, 89, 67, 8, uilook(uiText), 1, dpage
  FOR i as integer = 0 TO 15
   rectangle 248 + (i * 4), 91, 3, 5, pal16->col(i), dpage
  NEXT

  rectangle 4, 144, 224, 32, uilook(uiDisabledItem), dpage
  standardmenu pmenu(), palstate, 8, 148, dpage
  edgeprint "(Press LEFT or RIGHT to select palette)", 0, 188, uilook(uiMenuItem), dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

 palette16_unload @usepal
 RETURN ret
END FUNCTION

'state.pt is the current palette number
SUB spriteedit_import16(byref ss as SpriteEditState, byref ss_save as SpriteEditStatic, byref state as MenuState, placer() as integer, workpal() as integer, poffset() as integer)
 DIM srcbmp as string
 STATIC default as string

 'Any BMP, any size
 srcbmp = browse(2, default, "*.bmp", "", , "browse_import_sprite")
 IF srcbmp = "" THEN EXIT SUB

 DIM as Frame ptr impsprite, impsprite2
 DIM pal16 as Palette16 ptr
 spriteedit_import16_loadbmp ss, workpal(), state.pt - state.top, srcbmp, impsprite, pal16
 IF impsprite = NULL THEN EXIT SUB
 'frame_export_bmp4 "debug0.bmp", impsprite, master(), pal16

 'Pick background color
 DIM bgcol as integer
 bgcol = spriteedit_import16_pick_bgcol(ss, impsprite, pal16)
 IF bgcol = -1 THEN  'cancelled
  frame_unload @impsprite
  palette16_unload @pal16
  EXIT SUB
 END IF

 'Swap the transparent pixels to 0
 frame_swap_colors impsprite, 0, bgcol
 SWAP pal16->col(0), pal16->col(bgcol)

 'Trim or expand the image to final dimensions (only for spritedit_import16_remap_menu)
 impsprite2 = frame_new(ss.wide, ss.high, , YES)
 frame_draw impsprite, NULL, 0, 0, , NO, impsprite2
 SWAP impsprite, impsprite2
 frame_unload @impsprite2
 'frame_export_bmp4 "debug1.bmp", impsprite, master(), pal16

 'Check whether pal16 can be mapped directly onto the existing palette
 '(ignoring background colours), and whether it's actually the same
 DIM can_remap as bool
 DIM is_identical as bool
 DIM palmapping(15) as integer
 spriteedit_import16_compare_palettes workpal(), state.pt - state.top, pal16, palmapping(), can_remap, is_identical

 'Prompt about remapping palette
 DIM remap as integer
 IF is_identical THEN
  remap = 2
  debuginfo "spriteedit_import16: is identical"
 ELSE
  remap = spriteedit_import16_remap_menu(ss, ss_save, state, workpal(), poffset(), impsprite, pal16, palmapping())
 END IF

 IF remap = 0 THEN
  'Overwrite current palette
  FOR i as integer = 0 TO 15
   poke8bit workpal(), i + (state.pt - state.top) * 16, pal16->col(i)
  NEXT i
  'If the palette has changed, update genMaxPal
  gen(genMaxPal) = large(gen(genMaxPal), poffset(state.pt))

 ELSEIF remap = 1 THEN
  'Remap into current palette
  FOR x as integer = 0 TO impsprite->w - 1
   FOR y as integer = 0 TO impsprite->h - 1
    DIM col as integer = palmapping(readpixel(impsprite, x, y))
    putpixel(impsprite, x, y, col)
   NEXT
  NEXT

 ELSEIF remap = 2 THEN
  'Import without palette

 ELSEIF remap = 3 THEN
  'Cancel Import
  frame_unload @impsprite
  palette16_unload @pal16
  EXIT SUB
 END IF

 'convert sprite
 frame_to_4bit_buffer impsprite, placer(), ss.wide, ss.high

 frame_unload @impsprite
 palette16_unload @pal16
END SUB


SUB spriteedit_rotate_sprite(sprbuf() as integer, ss as SpriteEditState, counterclockwise as integer=NO)
 writeundospr sprbuf(), ss, YES
 spriteedit_rotate_sprite_buffer sprbuf(), ss.nulpal(), counterclockwise
END SUB

SUB spriteedit_rotate_sprite_buffer(sprbuf() as integer, nulpal() as integer, counterclockwise as integer=NO)
 DIM as Frame ptr spr1, spr2
 spr1 = frame_new_from_buffer(sprbuf(), 0)

 IF counterclockwise THEN
  spr2 = frame_rotated_90(spr1)
 ELSE
  spr2 = frame_rotated_270(spr1)
 END IF
 frame_unload @spr1

 DIM holdscreen as integer
 holdscreen = registerpage(spr2)

 DIM size as XYPair
 size.x = sprbuf(0)
 size.y = sprbuf(1)
 getsprite sprbuf(), 0, 0, 0, size.y, size.x, holdscreen

 freepage holdscreen
 frame_unload @spr2
END SUB

SUB sprite_editor(byref ss as SpriteEditState, byref ss_save as SpriteEditStatic, state as MenuState, soff as integer, workpal() as integer, poffset() as integer, info() as string, byval sets as integer)
 'spriteage

 DIM placer(2 + (ss.wide * ss.high * ss.perset) \ 4) as integer
 DIM mouse as MouseInfo

 DIM pclip(8) as integer

 DIM area(25) as MouseArea
 init_sprite_zones area(), ss

 DIM toolinfo(SPRITEEDITOR_NUM_TOOLS - 1) as ToolInfoType
 WITH toolinfo(0)
  .name = "Draw"
  .icon = CHR(3)
  .shortcut = scD
  .cursor = 0
  .areanum = 6
 END WITH
 WITH toolinfo(1)
  .name = "Box"
  .icon = CHR(4)
  .shortcut = scB
  .cursor = 1
  .areanum = 7
 END WITH
 WITH toolinfo(2)
  .name = "Line"
  .icon = CHR(5)
  .shortcut = scL
  .cursor = 2
  .areanum = 8
 END WITH
 WITH toolinfo(3)
  .name = "Fill"
  .icon = "F"
  .shortcut = scF
  .cursor = 3
  .areanum = 9
 END WITH
 WITH toolinfo(4)
  .name = "Oval"
  .icon = "O"
  .shortcut = scO
  .cursor = 2
  .areanum = 11
 END WITH
 WITH toolinfo(5)
  .name = "Air"
  .icon = "A"
  .shortcut = scA
  .cursor = 3
  .areanum = 21
 END WITH
 WITH toolinfo(6)
  .name = "Mark"
  .icon = "M"
  .shortcut = scM
  .cursor = 2
  .areanum = 23
 END WITH
 WITH toolinfo(7)
  .name = "Clone"
  .icon = "C"
  .shortcut = scC
  .cursor = 3
  .areanum = 24
 END WITH
 WITH toolinfo(8)
  .name = "Replace"
  .icon = "R"
  .shortcut = scR
  .cursor = 3
  .areanum = 10
 END WITH
 WITH toolinfo(scroll_tool)
  .name = "Scroll"
  .icon = "S"
  .shortcut = scS
  .cursor = 2
  .areanum = 22
 END WITH

 DIM as integer tick = 0

 ss.delay = 10
 ss.lastpos.x = -1
 ss.lastpos.y = -1
 ss.undodepth = 0
 ss.undoslot = 0
 ss.undomax = (32000 \ ss.size) - 1
 GOSUB spedbak
 loadsprite placer(), 0, ss.framenum * ss.size, soff * (state.pt - state.top), ss.wide, ss.high, 3
 hidemousecursor
 setkeys
 DO
  setwait 17, 70
  setkeys
  IF ss.gotmouse THEN
   mouse = readmouse
   ss.zonecursor = 0
   ss.zonenum = mouseover(mouse.x, mouse.y, ss.zone.x, ss.zone.y, ss.zonecursor, area())
  END IF
  IF keyval(scESC) > 1 THEN
   IF ss.hold = YES THEN
    GOSUB resettool
   ELSE
    spriteedit_clip placer(), ss
    stosprite placer(), 0, ss.framenum * ss.size, soff * (state.pt - state.top), 3
    GOSUB resettool
    EXIT DO
   END IF
  END IF
  IF keyval(scF1) > 1 THEN show_help "sprite_editor"
  IF ss.delay = 0 THEN
   GOSUB sprctrl
  END IF
  ss.delay = large(ss.delay - 1, 0)
  copypage 2, dpage  'moved this here to cover up residue on dpage (which was there before I got here!)
  spriteedit_display ss, ss_save, state, placer(), workpal(), poffset(), info(), toolinfo(), area(), mouse
  SWAP vpage, dpage
  setvispage vpage
  'blank the sprite area
  rectangle ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, 0, dpage
  tick = 0
  IF dowait THEN tick = 1: state.tog = state.tog XOR 1
 LOOP
 unhidemousecursor
 spriteedit_save_what_you_see(state.pt, state.top, sets, ss, soff, placer(), workpal(), poffset())
 changepal poffset(state.pt), 0, workpal(), state.pt - state.top
EXIT SUB

sprctrl:
'Debug keys
IF keyval(scCtrl) > 0 AND keyval(sc3) > 1 THEN setvispage 3: waitforanykey
'Normal keys
IF mouse.buttons = 0 AND keyval(scSpace) = 0 THEN
 ss.lastpos.x = -1
 ss.lastpos.y = -1
END IF
IF keyval(scTilde) > 1 THEN ss.hidemouse = ss.hidemouse XOR 1
IF keyval(scComma) > 1 AND ss.palindex > 0 THEN
 ss.palindex -= 1
 ss.showcolnum = 18
END IF
IF keyval(scPeriod) > 1 AND ss.palindex < 15 THEN
 ss.palindex += 1
 ss.showcolnum = 18
END IF
IF ss.zonenum = 2 THEN
 IF mouse.clicks > 0 THEN
  ss.palindex = small(ss.zone.x \ 4, 15)
  ss.showcolnum = 18
 END IF
END IF
IF keyval(scLeftBrace) > 1 OR (ss.zonenum = 5 AND mouse.clicks > 0) THEN
 changepal poffset(state.pt), -1, workpal(), state.pt - state.top
END IF
IF keyval(scRightBrace) > 1 OR (ss.zonenum = 6 AND mouse.clicks > 0) THEN
 changepal poffset(state.pt), 1, workpal(), state.pt - state.top
END IF
IF keyval(scP) > 1 OR (ss.zonenum = 19 AND mouse.clicks > 0) THEN '--call palette browser
 '--write changes so far
 stosprite placer(), 0, ss.framenum * ss.size, soff * (state.pt - state.top), 3
 '--save current palette
 storepal16 workpal(), state.pt - state.top, poffset(state.pt)
 poffset(state.pt) = pal16browse(poffset(state.pt), ss.fileset, state.pt)
 clearkey(scEnter)
 clearkey(scSpace)
 getpal16 workpal(), state.pt - state.top, poffset(state.pt)
END IF
'--UNDO
IF (keyval(scCtrl) > 0 AND keyval(scZ) > 1) OR (ss.zonenum = 20 AND mouse.clicks > 0) THEN GOSUB readundospr
'--COPY (CTRL+INS,SHIFT+DEL,CTRL+C)
IF copy_keychord() THEN
 ss_save.clipsize.x = ss.wide
 ss_save.clipsize.y = ss.high
 stosprite placer(), 0, ss.framenum * ss.size, soff * (state.pt - state.top), 3
 loadsprite ss_save.spriteclip(), 0, ss.framenum * ss.size, soff * (state.pt - state.top), ss.wide, ss.high, 3
 ss_save.paste = YES
END IF
'--PASTE (SHIFT+INS,CTRL+V)
IF paste_keychord() AND ss_save.paste = YES THEN
 rectangle 0, 0, ss.wide, ss.high, 0, dpage
 drawsprite placer(), 0, ss.nulpal(), 0, 0, 0, dpage
 drawsprite ss_save.spriteclip(), 0, ss.nulpal(), 0, 0, 0, dpage, 0
 getsprite placer(), 0, 0, 0, ss.wide, ss.high, dpage
END IF
'--TRANSPARENT PASTE (CTRL+T)
IF (keyval(scCtrl) > 0 AND keyval(scT) > 1) AND ss_save.paste = YES THEN
 rectangle 0, 0, ss.wide, ss.high, 0, dpage
 drawsprite placer(), 0, ss.nulpal(), 0, 0, 0, dpage
 drawsprite ss_save.spriteclip(), 0, ss.nulpal(), 0, 0, 0, dpage
 getsprite placer(), 0, 0, 0, ss.wide, ss.high, dpage
END IF
'--COPY PALETTE (ALT+C)
IF keyval(scAlt) > 0 AND keyval(scC) > 1 THEN
 FOR i as integer = 0 TO 7
  pclip(i) = workpal(i + (state.pt - state.top) * 8)
 NEXT
 ss.clippedpal = YES
END IF
'--PASTE PALETTE (ALT+V)
IF keyval(scAlt) > 0 AND keyval(scV) > 1 THEN
 IF ss.clippedpal THEN
  FOR i as integer = 0 TO 8
   workpal(i + (state.pt - state.top) * 8) = pclip(i)
  NEXT
 END IF
END IF
ss.curcolor = peek8bit(workpal(), (state.pt - state.top) * 16 + ss.palindex)
IF keyval(scAlt) > 0 THEN
 IF keyval(scUp) > 1 AND ss.curcolor > 15 THEN ss.curcolor -= 16 : ss.showcolnum = 18
 IF keyval(scDown) > 1 AND ss.curcolor < 240 THEN ss.curcolor += 16 : ss.showcolnum = 18
 IF keyval(scLeft) > 1 AND ss.curcolor > 0 THEN ss.curcolor -= 1 : ss.showcolnum = 18
 IF keyval(scRight) > 1 AND ss.curcolor < 255 THEN ss.curcolor += 1 : ss.showcolnum = 18
 'If the palette has changed, update genMaxPal
 gen(genMaxPal) = large(gen(genMaxPal), poffset(state.pt))
END IF
IF (mouse.clicks AND mouseLeft) ANDALSO ss.zonenum = 3 THEN
 ss.curcolor = ((ss.zone.y \ 6) * 16) + (ss.zone.x \ 4)
 ss.showcolnum = 18
 'If the palette has changed, update genMaxPal
 gen(genMaxPal) = large(gen(genMaxPal), poffset(state.pt))
END IF
poke8bit workpal(), (state.pt - state.top) * 16 + ss.palindex, ss.curcolor
IF keyval(scAlt) = 0 THEN
 DIM fixmouse as integer = NO
 WITH ss
  fixmouse = NO
  IF slowkey(scUp, 100) THEN .y = large(0, .y - 1):      fixmouse = YES
  IF slowkey(scDown, 100) THEN .y = small(ss.high - 1, .y + 1): fixmouse = YES
  IF slowkey(scLeft, 100) THEN .x = large(0, .x - 1):      fixmouse = YES
  IF slowkey(scRight, 100) THEN .x = small(ss.wide - 1, .x + 1): fixmouse = YES
 END WITH
 IF fixmouse THEN
  IF ss.zonenum = 1 THEN
   ss.zone.x = ss.x * ss.zoom + (ss.zoom \ 2)
   ss.zone.y = ss.y * ss.zoom + (ss.zoom \ 2)
   mouse.x = area(0).x + ss.zone.x 
   mouse.y = area(0).y + ss.zone.y
   movemouse mouse.x, mouse.y
  END IF 
  IF ss.zonenum = 14 THEN
   ss.zone.x = ss.x
   ss.zone.y = ss.y
   mouse.x = area(13).x + ss.zone.x 
   mouse.y = area(13).y + ss.zone.y
   movemouse mouse.x, mouse.y
  END IF
 END IF
END IF
IF ss.zonenum = 1 THEN
 ss.x = ss.zone.x \ ss.zoom
 ss.y = ss.zone.y \ ss.zoom
END IF
IF ss.tool = airbrush_tool THEN '--adjust airbrush
 IF mouse.buttons AND mouseLeft THEN
  IF ss.zonenum = 15 THEN ss.airsize = large(ss.airsize - tick, 1)
  IF ss.zonenum = 17 THEN ss.airsize = small(ss.airsize + tick, 80)
  IF ss.zonenum = 16 THEN ss.mist = large(ss.mist - tick, 1)
  IF ss.zonenum = 18 THEN ss.mist = small(ss.mist + tick, 99)
 END IF
 IF keyval(scMinus) > 1 OR keyval(scNumpadMinus) > 1 THEN
  IF keyval(scCtrl) > 0 THEN
   ss.mist = large(ss.mist - 1, 1)
  ELSE
   ss.airsize = large(ss.airsize - 1, 1)
  END IF
 END IF
 IF keyval(scPlus) > 1 OR keyval(scNumpadPlus) > 1 THEN
  IF keyval(scCtrl) > 0 THEN
   ss.mist = small(ss.mist + 1, 99)
  ELSE
   ss.airsize = small(ss.airsize + 1, 80)
  END IF
 END IF
END IF
IF ss.tool = clone_tool THEN
 '--When clone tool is active, rotate the clone buffer
 IF mouse.buttons AND mouseLeft THEN
  IF ss_save.clonemarked THEN
   IF ss.zonenum = 16 THEN
    spriteedit_rotate_sprite_buffer ss_save.clonebuf(), ss.nulpal(), YES
    ss.delay = 20
   END IF
   IF ss.zonenum = 18 THEN
    spriteedit_rotate_sprite_buffer ss_save.clonebuf(), ss.nulpal()
    ss.delay = 20
   END IF
  END IF
 END IF
ELSEIF ss.tool <> airbrush_tool THEN
 '--when other tools are active, rotate the whole buffer
 '--except for the airbrush tool because it's buttons collide.
 IF mouse.buttons AND mouseLeft THEN
  IF ss.zonenum = 16 THEN
   spriteedit_rotate_sprite placer(), ss, YES
   ss.delay = 20
  END IF
  IF ss.zonenum = 18 THEN
   spriteedit_rotate_sprite placer(), ss
   ss.delay = 20
  END IF
 END IF
END IF
IF ss.zonenum = 14 THEN
 ss.x = ss.zone.x
 ss.y = ss.zone.y
END IF
IF ((ss.zonenum = 1 OR ss.zonenum = 14) ANDALSO (mouse.buttons AND mouseLeft)) OR keyval(scSpace) > 0 THEN
 SELECT CASE ss.tool
  CASE draw_tool
   GOSUB putdot
  CASE box_tool
   IF mouse.clicks > 0 OR keyval(scSpace) > 1 THEN
    IF ss.hold THEN
     ss.hold = NO: GOSUB drawsquare
    ELSE
     ss.hold = YES
     ss.holdpos.x = ss.x
     ss.holdpos.y = ss.y
    END IF
   END IF
  CASE line_tool
   IF mouse.clicks > 0 OR keyval(scSpace) > 1 THEN
    IF ss.hold = YES THEN
     ss.hold = NO
     GOSUB straitline
    ELSE
     ss.hold = YES
     ss.holdpos.x = ss.x
     ss.holdpos.y = ss.y
    END IF
   END IF
  CASE fill_tool
   IF mouse.clicks > 0 OR keyval(scSpace) > 1 THEN
    GOSUB floodfill
   END IF
  CASE replace_tool
   IF mouse.clicks > 0 OR keyval(scSpace) > 1 THEN
    GOSUB replacecol
   END IF
  CASE oval_tool
   IF mouse.clicks > 0 OR keyval(scSpace) > 1 THEN
    IF ss.hold = NO THEN
     '--start oval
     ss.holdpos.x = ss.x
     ss.holdpos.y = ss.y
     ss.ellip_angle = 0.0
     ss.ellip_minoraxis = 0.0
     ss.radius = 0.0
     ss.hold = YES
    ELSE
     GOSUB drawoval
     ss.hold = NO
    END IF
   END IF
  CASE airbrush_tool
   GOSUB sprayspot
  CASE mark_tool
   IF mouse.clicks > 0 OR keyval(scSpace) > 1 THEN
    IF ss.hold THEN
     ss.hold = NO
     drawsprite placer(), 0, ss.nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
     getsprite ss_save.clonebuf(), 0, ss.previewpos.x + small(ss.x, ss.holdpos.x), ss.previewpos.y + small(ss.y, ss.holdpos.y), ABS(ss.x - ss.holdpos.x) + 1, ABS(ss.y - ss.holdpos.y) + 1, dpage
     ss_save.clonepos.x = ss_save.clonebuf(0) \ 2
     ss_save.clonepos.y = ss_save.clonebuf(1) \ 2
     ss_save.clonemarked = YES
     ss.tool = clone_tool ' auto-select the clone tool after marking
    ELSE
     ss.hold = YES
     ss.holdpos.x = ss.x
     ss.holdpos.y = ss.y
    END IF
   END IF
  CASE clone_tool
   IF mouse.clicks > 0 OR keyval(scSpace) > 1 THEN
    IF ss_save.clonemarked THEN
     IF ss.lastpos.x = -1 AND ss.lastpos.y = -1 THEN
      writeundospr placer(), ss
     END IF
     drawsprite placer(), 0, ss.nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
     drawsprite ss_save.clonebuf(), 0, ss.nulpal(), 0, ss.previewpos.x + ss.x - ss_save.clonepos.x, ss.previewpos.y + ss.y - ss_save.clonepos.y, dpage
     getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
     ss.lastpos.x = ss.x
     ss.lastpos.y = ss.y
    ELSE
     ss.tool = mark_tool ' select selection tool if clone is not available
     ss.hold = YES
     ss.holdpos.x = ss.x
     ss.holdpos.y = ss.y
    END IF
   END IF
 END SELECT
END IF
IF ss.hold = YES AND ss.tool = oval_tool THEN
 ss.radius = SQR((ss.x - ss.holdpos.x)^2 + (ss.y - ss.holdpos.y)^2)
 IF ss.zonenum = 1 THEN
  'Use mouse pointer instead of draw cursor for finer grain control of radius
  ss.radius = SQR( (ss.holdpos.x + 0.5 - ss.zone.x / ss.zoom)^2 + (ss.holdpos.y + 0.5 - ss.zone.y / ss.zoom)^2 )
 END IF
END IF
FOR i as integer = 0 TO UBOUND(toolinfo)
 IF (mouse.clicks > 0 AND ss.zonenum = toolinfo(i).areanum + 1) OR keyval(toolinfo(i).shortcut) > 1 THEN
  IF ss.tool <> i THEN ss.didscroll = NO
  ss.tool = i
  GOSUB resettool
  ss.drawcursor = toolinfo(i).cursor + 1
 END IF
NEXT i
IF ss.tool <> clone_tool AND ss.tool <> airbrush_tool THEN
 IF keyval(scPlus) > 1 THEN
  spriteedit_rotate_sprite placer(), ss
 END IF
 IF keyval(scMinus) > 1 THEN
  spriteedit_rotate_sprite placer(), ss, YES
 END IF
END IF
IF ss.tool = clone_tool THEN
 ' For clone brush tool, enter/right-click moves the handle point
 IF ss.readjust THEN
  IF keyval(scEnter) = 0 AND mouse.buttons = 0 THEN ' click or key release
   ss.readjust = NO
   ss_save.clonepos.x += (ss.x - ss.adjustpos.x)
   ss_save.clonepos.y += (ss.y - ss.adjustpos.y)
   ss.adjustpos.x = 0
   ss.adjustpos.y = 0
  END IF
 ELSE
  IF (keyval(scEnter) AND 5) OR mouse.buttons = mouseRight THEN
   ss.readjust = YES
   ss.adjustpos.x = ss.x
   ss.adjustpos.y = ss.y
  END IF
 END IF
 ' clone buffer rotation
 IF ss_save.clonemarked THEN
  IF keyval(scPlus) > 1 THEN
   spriteedit_rotate_sprite_buffer ss_save.clonebuf(), ss.nulpal()
  END IF
  IF keyval(scMinus) > 1 THEN
   spriteedit_rotate_sprite_buffer ss_save.clonebuf(), ss.nulpal(), YES
  END IF
 END IF
ELSE
 ' For all other tools, pick a color
 IF keyval(scEnter) > 1 OR (ss.zonenum = 1 AND mouse.buttons = mouseRight) THEN
  drawsprite placer(), 0, ss.nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
  ss.palindex = readpixel(ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, dpage)
  ss.showcolnum = 18
 END IF
END IF
IF keyval(scBackspace) > 1 OR (ss.zonenum = 4 AND mouse.clicks > 0) THEN wardsprite placer(), 0, ss.nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage: getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
IF ss.tool = scroll_tool AND (ss.zonenum = 1 OR ss.zonenum = 14) THEN
 'Handle scrolling by dragging the mouse
 'Did this drag start inside the sprite box? If not, ignore
 IF mouse.dragging ANDALSO mouseover(mouse.clickstart.x, mouse.clickstart.y, 0, 0, 0, area()) = ss.zonenum THEN
  spriteedit_scroll placer(), ss, ss.x - ss.lastcpos.x, ss.y - ss.lastcpos.y
 END IF
END IF
IF ss.tool = scroll_tool AND keyval(scAlt) = 0 THEN
 DIM scrolloff as XYPair
 IF slowkey(scLeft, 100) THEN scrolloff.x = -1
 IF slowkey(scRight, 100) THEN scrolloff.x = 1
 IF slowkey(scUp, 100) THEN scrolloff.y = -1
 IF slowkey(scDown, 100) THEN scrolloff.y = 1
 spriteedit_scroll placer(), ss, scrolloff.x, scrolloff.y
END IF
IF keyval(scI) > 1 OR (ss.zonenum = 13 AND mouse.clicks > 0) THEN
 spriteedit_import16 ss, ss_save, state, placer(), workpal(), poffset()
 GOSUB spedbak
END IF
IF keyval(scE) > 1 OR (ss.zonenum = 26 AND mouse.clicks > 0) THEN
 changepal poffset(state.pt), 0, workpal(), state.pt - state.top '--this saves the current palette in case it has changed
 spriteedit_export spriteedit_export_name(ss, state), placer(), ss.nulpal(), poffset(state.pt)
END IF
ss.lastcpos = TYPE(ss.x, ss.y)
RETRACE

resettool:
ss.hold = NO
ss.readjust = NO
ss.adjustpos.x = 0
ss.adjustpos.y = 0
RETRACE

spedbak:
clearpage 2
rectangle 3, 0, ss.wide * ss.zoom + 2, ss.high * ss.zoom + 2, uilook(uiText), 2
rectangle 4, 1, ss.wide * ss.zoom, ss.high * ss.zoom, 0, 2
rectangle 246, 109, 67, 8, uilook(uiText), 2
rectangle 247, 110, 65, 6, uilook(uiBackground), 2
rectangle ss.previewpos.x - 1, ss.previewpos.y - 1, ss.wide + 2, ss.high + 2, uilook(uiText), 2
rectangle ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, 0, 2
area(0).w = ss.wide * ss.zoom
area(0).h = ss.high * ss.zoom
area(13).w = ss.wide
area(13).h = ss.high
RETRACE

floodfill:
writeundospr placer(), ss
rectangle ss.previewpos.x - 1, ss.previewpos.y - 1, ss.wide + 2, ss.high + 2, ss.palindex, dpage
rectangle ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, 0, dpage
drawsprite placer(), 0, ss.nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
paintat vpages(dpage), ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, ss.palindex
getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
RETRACE

replacecol:
writeundospr placer(), ss
rectangle ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, 0, dpage
drawsprite placer(), 0, ss.nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
replacecolor vpages(dpage), readpixel(ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, dpage), ss.palindex, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high
getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
RETRACE

sprayspot:
IF ss.lastpos.x = -1 AND ss.lastpos.y = -1 THEN writeundospr placer(), ss
drawsprite placer(), 0, ss.nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
airbrush ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, ss.airsize, ss.mist, ss.palindex, dpage
getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
ss.lastpos.x = ss.x
ss.lastpos.y = ss.y
RETRACE

readundospr:
IF ss.undodepth > 0 THEN
 ss.undodepth = ss.undodepth - 1
 ss.undoslot = loopvar(ss.undoslot, 0, ss.undomax, -1)
 loadsprite placer(), 0, ss.undoslot * ss.size, 100, ss.wide, ss.high, 3

 ss.didscroll = NO  'save a new undo block upon scrolling
END IF
RETRACE

putdot:
drawsprite placer(), 0, ss.nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
IF ss.lastpos.x = -1 AND ss.lastpos.y = -1 THEN
 writeundospr placer(), ss
 putpixel ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, ss.palindex, dpage
ELSE
 drawline ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, ss.previewpos.x + ss.lastpos.x, ss.previewpos.y + ss.lastpos.y, ss.palindex, dpage
END IF
getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
ss.lastpos.x = ss.x
ss.lastpos.y = ss.y
RETRACE

drawoval:
writeundospr placer(), ss
drawsprite placer(), 0, ss.nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
ellipse vpages(dpage), ss.previewpos.x + ss.holdpos.x, ss.previewpos.y + ss.holdpos.y, ss.radius, ss.palindex, , ss.ellip_minoraxis, ss.ellip_angle
getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
RETRACE

drawsquare:
writeundospr placer(), ss
drawsprite placer(), 0, ss.nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
rectangle ss.previewpos.x + small(ss.x, ss.holdpos.x), ss.previewpos.y + small(ss.y, ss.holdpos.y), ABS(ss.x - ss.holdpos.x) + 1, ABS(ss.y - ss.holdpos.y) + 1, ss.palindex, dpage
getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
RETRACE

straitline:
writeundospr placer(), ss
drawsprite placer(), 0, ss.nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
drawline ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, ss.previewpos.x + ss.holdpos.x, ss.previewpos.y + ss.holdpos.y, ss.palindex, dpage
getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
RETRACE
END SUB

SUB spriteedit_scroll (placer() as integer, ss as SpriteEditState, byval shiftx as integer, byval shifty as integer)
 'Save an undo before the first of a consecutive scrolls
 IF shiftx = 0 AND shifty = 0 THEN EXIT SUB
 IF ss.didscroll = NO THEN writeundospr placer(), ss
 ss.didscroll = YES

 rectangle ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, 0, dpage
 drawsprite placer(), 0, ss.nulpal(), 0, ss.previewpos.x + shiftx, ss.previewpos.y + shifty, dpage
 getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
END SUB

