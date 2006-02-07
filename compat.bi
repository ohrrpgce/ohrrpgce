'' Compatibility functions/definitions for OHRRPGCE
' This file is specific to either FreeBasic or QuickBasic and
' defines the necessary functions to maintain compatibility
' between the two

'' FreeBasic version

option nokeyword getkey
option nokeyword setmouse

#ifndef DEMACRO
#ifndef DONESTR
option nokeyword clear
#define CLEAR dummyclear
option nokeyword str
#define str$(x) xstr$((x))
#define fbdim dim as short
option nokeyword peek
#define peek(x) xpeek(x)
option nokeyword poke
#define poke xpoke
#define DONESTR
#endif
#endif

#ifdef __FB_LINUX__
#define PATH_SEP "/"
#else
#define PATH_SEP "\"
#endif

' keyword(?) in QB
' had to change reg params to any because type not declared yet
DECLARE SUB INTERRUPTX (intnum AS INTEGER,inreg AS any, outreg AS any)
declare function xstr$ overload (x as integer)
declare function xstr$ (x as single)
declare function xstr$ (x as double)

declare sub defseg(byref var as integer)
declare function xpeek (byval idx as integer) as integer
declare sub xpoke(byval idx as integer, byval v as integer)

DECLARE SUB getdefaultfont (font() as integer)
DECLARE SUB xbload (f$, array%(), e$)
DECLARE SUB xbsave (f$, array%(), bsize%)
'only used in game.bas, maybe don't declare here?
DECLARE SUB crashexplain ()
declare sub dummyclear (arg1%=0, arg2%=0, arg3%=0)
declare sub togglewindowed()
declare sub storecommandline()
declare function getcommandline() as string
DECLARE FUNCTION canplay (file$)
DECLARE SUB playsongnum (songnum%)
DECLARE SUB romfontchar (font%(), char%)
