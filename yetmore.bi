#ifndef YETMORE_BI
#define YETMORE_BI

DECLARE SUB add_rem_swap_lock_hero (box AS TextBox, stat() as integer)
DECLARE FUNCTION checksaveslot (slot as integer) as integer
DECLARE SUB erasesaveslot (slot as integer)
DECLARE SUB doihavebits
DECLARE SUB embedtext (text as string, limit as integer=0)
DECLARE SUB scriptstat (id as integer, stat() as integer)
DECLARE SUB forceparty (stat() as integer)
DECLARE FUNCTION functiondone as integer
DECLARE SUB subread (si as ScriptInst)
DECLARE SUB subdoarg (si as ScriptInst)
DECLARE FUNCTION gethighbyte (n as integer) as integer
DECLARE FUNCTION getnpcref (seekid as integer, offset as integer) as integer
DECLARE SUB greyscalepal
DECLARE FUNCTION herobyrank (slot as integer) as integer
DECLARE SUB initgame
DECLARE SUB interpolatecat
DECLARE SUB npcplot
DECLARE SUB onkeyscript (scriptnum as integer)
DECLARE FUNCTION partybyrank (slot as integer) as integer
DECLARE FUNCTION playtime (d as integer, h as integer, m as integer) as string
DECLARE SUB playtimer
DECLARE FUNCTION rankincaterpillar (heroid as integer) as integer
DECLARE FUNCTION readfoemap (x as integer, y as integer, wide as integer, high as integer, fh as integer) as integer
DECLARE SUB scriptadvanced (id as integer)
DECLARE SUB scriptdump (s as string)
DECLARE SUB scriptmisc (id as integer)
DECLARE SUB scriptnpc (id as integer)
DECLARE SUB breakpoint (mode as integer, callspot as integer)
DECLARE SUB scriptwatcher (mode as integer, drawloop as integer)
DECLARE SUB setdebugpan
DECLARE SUB subreturn (si AS ScriptInst)
DECLARE SUB unwindtodo (si AS ScriptInst, levels as integer)
DECLARE SUB templockexplain
DECLARE SUB tweakpalette
DECLARE FUNCTION vehiclestuff (disx as integer, disy as integer, vehedge as integer) as integer
DECLARE FUNCTION vehpass (n as integer, tile as integer, default as integer) as integer
DECLARE SUB vishero (stat() as integer)
DECLARE SUB wrapaheadxy (x as integer, y as integer, direction as integer, distance as integer, unitsize as integer)
DECLARE SUB cropposition (BYREF x as integer, BYREF y as integer, unitsize as integer)
DECLARE FUNCTION wrappass (x as integer, y as integer, xgo as integer, ygo as integer, isveh as integer) as integer
DECLARE FUNCTION wrapcollision (xa as integer, ya as integer, xgoa as integer, ygoa as integer, xb as integer, yb as integer, xgob as integer, ygob as integer) as integer
DECLARE FUNCTION wraptouch (x1 as integer, y1 as integer, x2 as integer, y2 as integer, distance as integer) as integer
DECLARE SUB wrappedsong (songnumber as integer)
DECLARE SUB stopsong
DECLARE SUB wrapxy (x as integer, y as integer, wide as integer, high as integer)
DECLARE SUB readstackcommand (state as ScriptInst, stk as Stack, i as integer)
DECLARE FUNCTION localvariablename (value as integer, scriptargs as integer) as string
DECLARE FUNCTION mathvariablename (value as integer, scriptargs as integer) as string
DECLARE FUNCTION scriptstate (targetscript as integer) as string
DECLARE FUNCTION backcompat_sound_id (id AS INTEGER) as integer
DECLARE SUB loadsay (box_id as integer)
DECLARE SUB load_text_box_portrait (BYREF box AS TextBox, BYREF gfx AS GraphicPair)
DECLARE FUNCTION valid_plotsprite(byval s as integer, byval cmd as string) as integer

#endif