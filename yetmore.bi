#ifndef YETMORE_BI
#define YETMORE_BI

DECLARE SUB add_rem_swap_lock_hero (box AS TextBox, stat())
DECLARE FUNCTION checksaveslot (slot)
DECLARE SUB erasesaveslot (slot)
DECLARE SUB doihavebits
DECLARE SUB embedtext (text$, limit=0)
DECLARE SUB scriptstat (id, stat())
DECLARE SUB forceparty (stat())
DECLARE FUNCTION functiondone
DECLARE SUB subread (si as ScriptInst)
DECLARE SUB subdoarg (si as ScriptInst)
DECLARE FUNCTION gethighbyte (n)
DECLARE FUNCTION getnpcref (seekid, offset)
DECLARE SUB greyscalepal
DECLARE FUNCTION herobyrank (slot)
DECLARE SUB initgame
DECLARE SUB interpolatecat
DECLARE SUB npcplot
DECLARE SUB onkeyscript (scriptnum)
DECLARE FUNCTION partybyrank (slot)
DECLARE FUNCTION playtime$ (d, h, m)
DECLARE SUB playtimer
DECLARE FUNCTION rankincaterpillar (heroid)
DECLARE FUNCTION readfoemap (x, y, wide, high, fh)
DECLARE SUB scriptadvanced (id)
DECLARE SUB scriptdump (s$)
DECLARE SUB scriptmisc (id)
DECLARE SUB scriptnpc (id)
DECLARE SUB breakpoint (mode, callspot)
DECLARE SUB scriptwatcher (mode, drawloop)
DECLARE SUB setdebugpan
DECLARE SUB subreturn (si AS ScriptInst)
DECLARE SUB unwindtodo (si AS ScriptInst, levels)
DECLARE SUB templockexplain
DECLARE SUB tweakpalette
DECLARE FUNCTION vehiclestuff (disx, disy, vehedge)
DECLARE FUNCTION vehpass (n, tile, default)
DECLARE SUB vishero (stat())
DECLARE SUB wrapaheadxy (x, y, direction, distance, unitsize)
DECLARE SUB cropposition (BYREF x, BYREF y, unitsize)
DECLARE FUNCTION wrappass (x, y, xgo, ygo, isveh)
DECLARE FUNCTION wrapcollision (xa, ya, xgoa, ygoa, xb, yb, xgob, ygob)
DECLARE FUNCTION wraptouch (x1, y1, x2, y2, distance)
DECLARE SUB wrappedsong (songnumber)
DECLARE SUB stopsong
DECLARE SUB wrapxy (x, y, wide, high)
DECLARE SUB readstackcommand (state as ScriptInst, stk as Stack, i)
DECLARE FUNCTION localvariablename$ (value, scriptargs)
DECLARE FUNCTION mathvariablename$ (value, scriptargs)
DECLARE FUNCTION scriptstate$ (targetscript)
DECLARE FUNCTION backcompat_sound_id (id AS INTEGER)
DECLARE SUB loadsay (box_id)
DECLARE SUB load_text_box_portrait (BYREF box AS TextBox, BYREF gfx AS GraphicPair)
DECLARE FUNCTION valid_plotsprite(byval s as integer, byval cmd as string) as integer

#endif