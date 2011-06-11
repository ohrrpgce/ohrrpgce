'OHRRPGCE - Some Custom/Game common code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
' This file is for code that is shared between GAME and CUSTOM.
' Code that is not OHRRPGCE-specific that would be of general use
' to any FreeBasic program belongs in util.bas instead

#include "config.bi"
#include "ver.txt"
#include "const.bi"
#include "allmodex.bi"
#include "os.bi"
#include "cutil.bi"
#include "string.bi"

#include "udts.bi"
#include "scrconst.bi"
#include "uiconst.bi"
#include "common.bi"
#include "slices.bi"

#include "music.bi"
#include "loading.bi"

'Subs and functions only used here
DECLARE SUB setup_sprite_sizes ()

#IFDEF IS_GAME
DECLARE SUB embedtext (text$, limit=0)
DECLARE FUNCTION istag (num, zero) as integer
DECLARE SUB scripterr (e as string, errorlevel as integer = 5)
DECLARE FUNCTION commandname (byval id as integer) as string
DECLARE SUB exitprogram (BYVAL needfade as integer, BYVAL errorout as integer = NO)
EXTERN insideinterpreter as integer
EXTERN curcmd as ScriptCommand ptr
#ENDIF

#IFDEF IS_CUSTOM
DECLARE FUNCTION charpicker() AS STRING
#ENDIF


''''' Global variables (anything else in common.bi missing here will be in game.bas or custom.bas)

'Allocate sprite size table
REDIM sprite_sizes(-1 TO 10) AS SpriteSize
setup_sprite_sizes

'holds commandline args not recognised by the backends or anything else
REDIM cmdline_args(0) AS STRING

'holds the directory to dump logfiles into
DIM log_dir AS STRING

'It is very important for this to be populated _before_ any calls to CHDIR
DIM orig_dir AS STRING

'Used on Mac to point to the app bundle Resources directory
DIM data_dir AS STRING

#IFDEF IS_CUSTOM
 'show/fatalerror option (Custom only): have we started editing?
 'If not, we should cleanup working.tmp instead of preserving it
 DIM cleanup_on_error AS INTEGER = YES
#ENDIF

''''' Module-local variables

'a primitive system for printing messages that scroll
TYPE ConsoleData
 AS INTEGER x = 0, y = 0, top = 0, h = 200, c = 0
END TYPE
DIM SHARED console AS ConsoleData

'don't black out the screen to show upgrade messages if there aren't any
DIM SHARED upgrademessages AS INTEGER

'upgrading timing stuff
DIM SHARED time_rpg_upgrade AS INTEGER = NO  'Change to YES, or pass --time-upgrade
DIM SHARED last_upgrade_time AS DOUBLE
DIM SHARED upgrade_overhead_time AS DOUBLE
DIM SHARED upgrade_start_time AS DOUBLE

'Upgrade all game data (required for writing), or only enough to allow reading?
#IFDEF IS_CUSTOM
DIM SHARED full_upgrade AS INTEGER = YES
#ELSE
DIM SHARED full_upgrade AS INTEGER = NO
#ENDIF

'don't delete the debug file at end of play
DIM SHARED importantdebug AS INTEGER = 0

'When restarting the log, the previous path if significant
DIM SHARED lastlogfile AS STRING

'.stt lump read into memory
DIM SHARED global_strings_buffer AS STRING


FUNCTION common_setoption(opt as string, arg as string) as integer
 IF opt = "time-upgrade" THEN
  time_rpg_upgrade = YES
  RETURN 1  'arg not used
 ELSEIF opt = "full-upgrade" THEN
  full_upgrade = YES
  RETURN 1  'arg not used
 END IF
END FUNCTION

'fade in and out not actually used in custom
SUB fadein ()
fadestate = 1
fadetopal master()
END SUB

SUB fadeout (red, green, blue)
fadestate = 0
fadeto red, green, blue
END SUB

FUNCTION filesize (file as string) as string
 'returns size of a file in formatted string
 DIM as integer size, spl
 DIM as string fsize, units
 IF isfile(file) THEN
  size = FILELEN(file)
  units = " B"
  spl = 0
  IF size > 1024 THEN spl = 1 : units = " KB"
  IF size > 1048576 THEN spl = 1 : size = size / 1024 : units = " MB"
  fsize = STR(size)
  IF spl <> 0 THEN
   size = size / 102.4
   fsize = STR(size \ 10)
   IF size < 1000 THEN fsize = fsize + "." + STR(size MOD 10)
  END IF
  RETURN fsize + units
 ELSE
  RETURN "N/A"
 END IF
END FUNCTION

FUNCTION usemenu (state AS MenuState, deckey = scUp, inckey = scDown) as integer
 WITH state
  RETURN usemenu(.pt, .top, .first, .last, .size, deckey, inckey)
 END WITH
END FUNCTION

FUNCTION usemenu (pt, top, first, last, size, deckey = scUp, inckey = scDown) as integer

oldptr = pt
oldtop = top

IF keyval(deckey) > 1 THEN pt = loopvar(pt, first, last, -1)
IF keyval(inckey) > 1 THEN pt = loopvar(pt, first, last, 1)
IF keyval(scPageup) > 1 THEN pt = pt - size
IF keyval(scPagedown) > 1 THEN pt = pt + size
IF keyval(scHome) > 1 THEN pt = first
IF keyval(scEnd) > 1 THEN pt = last
pt = small(large(pt, first), last)  '=last when last<first, ie. menu empty
top = bound(top, pt - size, pt)

IF oldptr = pt AND oldtop = top THEN
 usemenu = 0
ELSE
 usemenu = 1
END IF

END FUNCTION

FUNCTION usemenu (state as MenuState, menudata() as SimpleMenu, BYVAL deckey as integer = scUp, BYVAL inckey as integer = scDown) as integer
'a version for menus with unselectable items, skip items for which menudata().enabled = 0

WITH state
 '.pt = -1 when the menu has no selectable items
 IF .pt = -1 THEN RETURN 0

 oldptr = .pt
 oldtop = .top
 d = 0
 moved_d = 0

 IF keyval(deckey) > 1 THEN d = -1
 IF keyval(inckey) > 1 THEN d = 1
 IF keyval(scPageup) > 1 THEN
  .pt = large(.pt - .size, .first)
  WHILE menudata(.pt).enabled = 0 AND .pt > .first : .pt = loopvar(.pt, .first, .last, -1) : WEND
  IF menudata(.pt).enabled = 0 THEN d = 1
  moved_d = -1
 END IF
 IF keyval(scPagedown) > 1 THEN
  .pt = small(.pt + .size, .last)
  WHILE menudata(.pt).enabled = 0 AND .pt < .last : .pt = loopvar(.pt, .first, .last, 1) : WEND
  IF menudata(.pt).enabled = 0 THEN d = -1
  moved_d = 1
 END IF
 IF keyval(scHome) > 1 THEN .pt = .last : d = 1
 IF keyval(scEnd) > 1 THEN .pt = .first : d = -1

 IF d THEN 
  moved_d = d
  DO
   .top = bound(.top, .pt - .size, .pt)
   .pt = loopvar(.pt, .first, .last, d)
  LOOP WHILE menudata(.pt).enabled = 0
 END IF

 IF moved_d THEN
  'we look ahead of the actual cursor, to bring unselectable items at the ends of the menu into view
  DIM lookahead AS INTEGER = .pt
  DO
   lookahead += moved_d
  LOOP WHILE bound(lookahead, .first, .last) = lookahead ANDALSO menudata(lookahead).enabled = 0
  lookahead = bound(lookahead, .first, .last)
  .top = bound(.top, lookahead - .size, lookahead)
 END IF
 .top = bound(.top, .pt - .size, .pt)

 IF oldptr = .pt AND oldtop = .top THEN
  usemenu = 0
 ELSE
  usemenu = 1
 END IF
END WITH
END FUNCTION

FUNCTION usemenu (state as MenuState, enabled() as INTEGER, BYVAL deckey as integer = scUp, BYVAL inckey as integer = scDown) as integer
'a version for menus with unselectable items, skip items for which enabled = 0

WITH state
 '.pt = -1 when the menu has no selectable items
 IF .pt = -1 THEN RETURN 0

 oldptr = .pt
 oldtop = .top
 d = 0
 moved_d = 0

 IF keyval(deckey) > 1 THEN d = -1
 IF keyval(inckey) > 1 THEN d = 1
 IF keyval(scPageup) > 1 THEN
  .pt = large(.pt - .size, .first)
  WHILE enabled(.pt) = 0 AND .pt > .first : .pt = loopvar(.pt, .first, .last, -1) : WEND
  IF enabled(.pt) = 0 THEN d = 1
  moved_d = -1
 END IF
 IF keyval(scPagedown) > 1 THEN
  .pt = small(.pt + .size, .last)
  WHILE enabled(.pt) = 0 AND .pt < .last : .pt = loopvar(.pt, .first, .last, 1) : WEND
  IF enabled(.pt) = 0 THEN d = -1
  moved_d = 1
 END IF
 IF keyval(scHome) > 1 THEN .pt = .last : d = 1
 IF keyval(scEnd) > 1 THEN .pt = .first : d = -1

 IF d THEN
  moved_d = d
  DO
   .top = bound(.top, .pt - .size, .pt)
   .pt = loopvar(.pt, .first, .last, d)
  LOOP WHILE enabled(.pt) = 0
 END IF

 IF moved_d THEN
  'we look ahead of the actual cursor, to bring unselectable items at the ends of the menu into view
  DIM lookahead AS INTEGER = .pt
  DO
   lookahead += moved_d
  LOOP WHILE bound(lookahead, .first, .last) = lookahead ANDALSO enabled(lookahead) = 0
  lookahead = bound(lookahead, .first, .last)
  .top = bound(.top, lookahead - .size, lookahead)
 END IF
 .top = bound(.top, .pt - .size, .pt)

 IF oldptr = .pt AND oldtop = .top THEN
  usemenu = 0
 ELSE
  usemenu = 1
 END IF
END WITH
END FUNCTION

'scrollmenu is like usemenu for menus where no menu item is selected:
'you just want to scroll a menu up and down (modifies .top; .pt is ignored).
FUNCTION scrollmenu (state AS MenuState, BYVAL deckey as integer = scUp, BYVAL inckey as integer = scDown) as integer
 WITH state
  DIM oldtop as integer = .top
  DIM lasttop as integer = large(.first, .last - .size)
  IF keyval(deckey) > 1 THEN .top = loopvar(.top, .first, lasttop, -1)
  IF keyval(inckey) > 1 THEN .top = loopvar(.top, .first, lasttop, 1)
  IF keyval(scPageup) > 1 THEN .top = large(.first, .top - .size)
  IF keyval(scPagedown) > 1 THEN .top = small(lasttop, .top + .size)
  IF keyval(scHome) > 1 THEN .top = .first
  IF keyval(scEnd) > 1 THEN .top = lasttop
  RETURN (.top <> oldtop)
 END WITH
END FUNCTION

FUNCTION soundfile (sfxnum%) as string
 DIM as string sfxbase

 sfxbase = workingdir & SLASH & "sfx" & sfxnum%
 IF isfile(sfxbase & ".ogg") THEN
  RETURN sfxbase & ".ogg"
 ELSEIF isfile(sfxbase & ".mp3") THEN
  RETURN sfxbase & ".mp3"
 ELSEIF isfile(sfxbase & ".wav") THEN
  RETURN sfxbase & ".wav"
 ELSE
  RETURN ""
 END IF
END FUNCTION

SUB start_new_debug
 CONST buflen = 128 * 1024

 DIM as string logfile, oldfile
 #IFDEF IS_GAME
   logfile = log_dir & "g_debug.txt"
   oldfile = log_dir & "g_debug_archive.txt"
 #ELSE
   logfile = log_dir & "c_debug.txt"
   oldfile = log_dir & "c_debug_archive.txt"
 #ENDIF
 'If we just closed a debug file, don't archive it, or we'll never notice it
 IF lastlogfile = absolute_path(logfile) THEN EXIT SUB
 IF NOT isfile(logfile) THEN EXIT SUB

 dlog = FREEFILE
 OPEN logfile FOR BINARY AS dlog
 archive = FREEFILE
 OPEN oldfile FOR BINARY AS archive

 DIM AS UBYTE PTR buf = ALLOCATE(buflen)

 copyamount = bound(buflen - LOF(dlog), 0, LOF(archive))

 IF copyamount THEN
  SEEK #archive, LOF(archive) - copyamount + 1
  'don't cut the file in the middle of a line
  DO
   GET #archive, , buf[0]
  LOOP UNTIL buf[0] = 10

  GET #archive, , buf[0], buflen, copyamount
 END IF

 CLOSE #archive
 safekill oldfile
 archive = FREEFILE
 OPEN oldfile FOR BINARY AS archive
 PUT #archive, , *buf, copyamount

 IF LOF(dlog) > buflen THEN
  SEEK #dlog, LOF(dlog) - buflen + 1
  DO
   GET #dlog, , buf[0]
  LOOP UNTIL buf[0] = 10
 END IF
 
 GET #dlog, , buf[0], buflen, copyamount
 PUT #archive, , LINE_END " \....----~~~~````\" LINE_END
 PUT #archive, , *buf, copyamount
 CLOSE #dlog
 CLOSE #archive

 DEALLOCATE(buf)
 safekill logfile
END SUB

SUB end_debug
 DIM filename AS STRING
 #IFDEF IS_GAME
   filename = "g_debug.txt"
 #ELSE
   filename = "c_debug.txt"
 #ENDIF
 IF NOT importantdebug THEN
   safekill log_dir & filename
 ELSE
   'Remember not to archive the log if we restart the log in the same directory
   lastlogfile = absolute_path(log_dir & filename)
 END IF
 importantdebug = 0
END SUB

SUB debug (s$)
 importantdebug = -1
 debuginfo s$
END SUB

SUB debugc CDECL (BYVAL s as zstring ptr, BYVAL errorlevel as integer)
 'Fine grained errorlevels unimplemented, but here's the current suggestion:
 ' 1 current debuginfo
 ' 2 current debug, mostly for debugging, including "trace"
 ' 3 recovered error, log and ask the user (when they quit?) to send the debug log
 ' 4 possibly recoverable error, ask the user whether they want to continue
 ' 5 fatal error, tell the user to report, and then quit
 ' 6 fatal error which prevents the display of a visible error message;
 '   attempt to print a stacktrace and quit

 IF errorlevel >= 2 THEN importantdebug = -1
 debuginfo *s
END SUB

SUB debuginfo (s AS STRING)
 'use for throwaway messages like upgrading
 STATIC sizeerror = 0
 DIM filename AS STRING
 #IFDEF IS_GAME
   filename = "g_debug.txt"
 #ELSE
   filename = "c_debug.txt"
 #ENDIF
 fh = FREEFILE
 OPEN log_dir & filename FOR APPEND AS #fh
 IF LOF(fh) > 2 * 1024 * 1024 THEN
  IF sizeerror = 0 THEN PRINT #fh, "too much debug() output, not printing any more messages"
  sizeerror = -1
  CLOSE #fh
  EXIT SUB
 END IF
 sizeerror = 0
 PRINT #fh, s
 CLOSE #fh
END SUB

'Draw a wrapped string in the middle of the page, in a box
SUB basic_textbox (msg as string, byval col as integer, byval page as integer)
 WITH *vpages(page)
  DIM captlines() AS STRING
  split(wordwrap(msg, (.w - 20) \ 8), captlines())
  centerbox .w \ 2, .h \ 2, .w - 10, 26 + 10 * UBOUND(captlines), 3, page
  DIM y AS INTEGER = .h \ 2 - (UBOUND(captlines) + 1) * 5
  FOR i AS INTEGER = 0 TO UBOUND(captlines)
   edgeprint captlines(i), 10, y + i * 10, col, page, YES
  NEXT
 END WITH
END SUB

SUB notification (msg AS STRING)
 basic_textbox msg, uilook(uiText), vpage
 setvispage vpage
 waitforanykey
END SUB

SUB visible_debug (msg as string)
 debuginfo msg
 notification msg + !"\nPress any key..."
' pop_warning msg
END SUB

FUNCTION getfixbit(BYVAL bitnum AS INTEGER) AS INTEGER
 DIM f AS STRING
 f = workingdir + SLASH + "fixbits.bin"
 IF NOT isfile(f) THEN RETURN 0
 DIM fh AS INTEGER = FREEFILE
 IF OPEN(f FOR BINARY ACCESS READ AS fh) THEN debug "Could not read " & f : RETURN 0
 DIM ub AS UBYTE
 GET #fh, (bitnum \ 8) + 1, ub
 CLOSE #fh
 RETURN BIT(ub, bitnum MOD 8)  'BIT is a standard macro
END FUNCTION

SUB setfixbit(BYVAL bitnum AS INTEGER, BYVAL bitval AS INTEGER)
 DIM f AS STRING
 f = workingdir + SLASH + "fixbits.bin"
 DIM fh AS INTEGER = FREEFILE
 IF OPEN(f FOR BINARY AS fh) THEN fatalerror "Impossible to upgrade game: Could not write " & f  'Really bad!
 extendfile fh, (bitnum \ 8) + 1  'Prevent writing garbage
 DIM ub AS UBYTE
 GET #fh, (bitnum \ 8) + 1, ub
 IF bitval THEN ub = BITSET(ub, bitnum MOD 8) ELSE ub = BITRESET(ub, bitnum MOD 8)
 PUT #fh, (bitnum \ 8) + 1, ub
 CLOSE #fh
END SUB

'Note: Custom doesn't use this function
FUNCTION acquiretempdir () as string
 DIM tmp as string
 #IFDEF __FB_WIN32__
  'Windows only behavior
  tmp = environ("TEMP")
  IF NOT diriswriteable(tmp) THEN tmp = environ("TMP")
  IF NOT diriswriteable(tmp) THEN tmp = exepath$
  IF NOT diriswriteable(tmp) THEN tmp = ""
  IF NOT diriswriteable(tmp) THEN fatalerror "Unable to find any writable temp dir"
  IF RIGHT$(tmp, 1) <> SLASH THEN tmp = tmp & SLASH
  tmp = tmp & "ohrrpgce"
 #ELSE
  'Unix only behavior
  tmp = environ("HOME") + SLASH + ".ohrrpgce"
  IF NOT isdir(tmp) THEN makedir(tmp)
  tmp = tmp & SLASH
 #ENDIF
 DIM as string d = DATE, t = TIME
 tmp += MID(d,7,4) & MID(d,1,2) & MID(d,4,2) & MID(t,1,2) & MID(t,4,2) & MID(t,7,2) & "." & CINT(RND * 1000) & ".tmp" & SLASH
 RETURN tmp
END FUNCTION

'Backwards compatibility wrapper
SUB centerbox (x, y, w, h, c, p)
 IF c < 0 OR c > 15 THEN debug "Warning: invalid box style " & c & " in centerbox"
 center_edgeboxstyle x, y, w, h, c - 1, p
END SUB

SUB center_edgeboxstyle (x, y, w, h, boxstyle, p, fuzzy=NO, supress_borders=NO)
 edgeboxstyle x - w / 2, y - h / 2, w, h, boxstyle, p, fuzzy, supress_borders
END SUB

SUB edgeboxstyle (rect as RectType, BYVAL boxstyle, BYVAL p, BYVAL fuzzy=NO, BYVAL supress_borders=NO)
 edgeboxstyle rect.x, rect.y, rect.wide, rect.high, boxstyle, p, fuzzy, supress_borders
END SUB

SUB edgeboxstyle (BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL boxstyle, BYVAL p, BYVAL fuzzy=NO, BYVAL supress_borders=NO)
 IF boxstyle < 0 OR boxstyle > 14 THEN
  debug "edgeboxstyle: invalid boxstyle " & boxstyle
  EXIT SUB
 END IF
 DIM col AS INTEGER = uilook(uiTextBox + 2 * boxstyle)
 DIM bordercol AS INTEGER = uilook(uiTextBox + 2 * boxstyle + 1)
 DIM borders AS INTEGER = boxstyle
 DIM trans AS RectTransTypes = transOpaque
 IF supress_borders THEN borders = -1
 IF fuzzy THEN trans = transFuzzy
 edgebox x, y, w, h, col, bordercol, p, trans, borders
END SUB

SUB edgebox (x, y, w, h, col, bordercol, p, trans AS RectTransTypes=transOpaque, border=-1, fuzzfactor=50)
 edgebox x, y, w, h, col, bordercol, vpages(p), trans, border, fuzzfactor
END SUB

SUB edgebox (x, y, w, h, col, bordercol, BYVAL fr AS Frame Ptr, trans AS RectTransTypes=transOpaque, border=-1, fuzzfactor=50)
'--border: -2 is none, -1 is simple line, 0+ is styled box edge
IF trans = transFuzzy THEN
 fuzzyrect fr, x, y, w, h, col, fuzzfactor
ELSEIF trans = transOpaque THEN
 rectangle fr, x, y, w, h, col
END IF
IF border = -1 THEN
 '--Simple line border
 drawbox fr, x, y, w, h, bordercol
ELSEIF border >= 0 AND border <= 14 THEN
 '--Normal Border
 IF trans <> transHollow THEN drawbox fr, x, y, w, h, bordercol
 DIM AS INTEGER i, borderindex
 DIM border_gfx AS GraphicPair
 borderindex = uilook(uiTextBoxFrame + border) - 1
 IF borderindex >= 0 THEN
  load_sprite_and_pal border_gfx, 7, borderindex
 END IF
 WITH border_gfx
  IF .sprite THEN ' Only proceed if a sprite is actually selected
   'Draw edges
   'ensure we are clipping the correct page (there are many ways of doing this)
   setclip , , , , fr
   '--Top and bottom edges
   FOR i = x + 8 TO x + w - 24 STEP 16
    setclip , , , y + h - 1
    frame_draw .sprite + 2, .pal, i, y - 8, 1, YES, fr
    setclip , y, , 
    frame_draw .sprite + 13, .pal, i, y + h - 8, 1, YES, fr
   NEXT i
   '--Left and right edges
   FOR i = y + 8 TO y + h - 24 STEP 16
    setclip , , x + w - 1, 
    frame_draw .sprite + 7, .pal, x - 8, i, 1, YES, fr
    setclip x, , , 
    frame_draw .sprite + 8, .pal, x + w - 8, i, 1, YES, fr
   NEXT i
   'Draw end-pieces
   IF w > 26 THEN
    '--Top end pieces
    setclip , , , y + h - 1
    frame_draw .sprite + 3, .pal, x + w - 24, y - 8, 1, YES, fr
    frame_draw .sprite + 1, .pal, x + 8, y - 8, 1, YES, fr
    '--Bottom end pieces
    setclip , y, , 
    frame_draw .sprite + 14, .pal, x + w - 24, y + h - 8, 1, YES, fr
    frame_draw .sprite + 12, .pal, x + 8, y + h - 8, 1, YES, fr
   ELSEIF w > 16 THEN
    '--Not enough space for the end pieces, have to draw part of the edge after all
    '--Top and bottom edges
    setclip x + 8, , x + w - 9, y + h - 1
    frame_draw .sprite + 2, .pal, x + 8, y - 8, 1, YES, fr
    setclip x + 8, y, x + w - 9, 
    frame_draw .sprite + 13, .pal, x + 8, y + h - 8, 1, YES, fr
   END IF
   IF h > 26 THEN
    '--Left side end pieces
    setclip , , x + w - 1, 
    frame_draw .sprite + 9, .pal, x - 8, y + h - 24, 1, YES, fr
    frame_draw .sprite + 5, .pal, x - 8, y + 8, 1, YES, fr
    '--Right side end pieces
    setclip x, , , 
    frame_draw .sprite + 10, .pal, x + w - 8, y + h - 24, 1, YES, fr
    frame_draw .sprite + 6, .pal, x + w - 8, y + 8, 1, YES, fr
   ELSEIF h > 16 THEN
    '--Not enough space for the end pieces, have to draw part of the edge after all
    '--Left and right edges
    setclip , y + 8, x + w - 1, y + h - 9
    frame_draw .sprite + 7, .pal, x - 8, y + 8, 1, YES, fr
    setclip x, y + 8, , y + h - 9
    frame_draw .sprite + 8, .pal, x + w - 8, y + 8, 1, YES, fr
   END IF
   'Draw corners
   'If the box is really tiny, we need to only draw part of each corner
   setclip , , x + w - 1, y + h - 1
   frame_draw .sprite, .pal, x - 8, y - 8, 1, YES, fr
   setclip x, , , y + h - 1
   frame_draw .sprite + 4, .pal, x + w - 8, y - 8, 1, YES, fr
   setclip , y, x + w - 1,
   frame_draw .sprite + 11, .pal, x - 8, y + h - 8, 1, YES, fr
   setclip x, y, , 
   frame_draw .sprite + 15, .pal, x + w - 8, y + h - 8, 1, YES, fr
   setclip
  END IF
 END WITH
 unload_sprite_and_pal border_gfx
END IF
END SUB

SUB centerfuz (x, y, w, h, c, p)
 IF c < 0 OR c > 15 THEN debug "Warning: invalid box style " & c & " in centerbox"
 center_edgeboxstyle x, y, w, h, c - 1, p, YES
END SUB

'======== FIXME: move this up as code gets cleaned up ===========
OPTION EXPLICIT

FUNCTION read32bitstring (array(), offset) as string
DIM as string result = SPACE(array(offset))
memcpy(STRPTR(result), @array(offset + 1), array(offset))
return result
END FUNCTION

FUNCTION read32bitstring (stringptr as integer ptr) as string
DIM as string result = SPACE(stringptr[0])
memcpy(STRPTR(result), @stringptr[1], stringptr[0])
return result
END FUNCTION

FUNCTION readbadgenericname (index AS INTEGER, filename AS STRING, recsize AS INTEGER, offset AS INTEGER, size AS INTEGER, skip AS INTEGER = 0) as string
 'recsize is in BYTES!
 'FIXME: there isn't any good reason to load the whole record
 '       just to get the string field
 IF index >= 0 THEN
  DIM buf(recsize \ 2 - 1) as integer
  IF loadrecord(buf(), filename, recsize \ 2, index) THEN
   RETURN readbadbinstring(buf(), offset, size, skip)
  END IF
 END IF
 RETURN ""  'failure
END FUNCTION

FUNCTION isbit (bb() as INTEGER, BYVAL w as INTEGER, BYVAL b as INTEGER) as INTEGER
 IF readbit (bb(), w, b) THEN
  RETURN -1
 ELSE
  RETURN 0
 END IF
END FUNCTION

FUNCTION scriptname (num AS INTEGER, trigger AS INTEGER = 0) as string
DIM a AS STRING

#ifdef IS_GAME
 'remember script names; can be a large speed up in script debugger 
 STATIC cache(24) as IntStrPair
 a = search_string_cache(cache(), num, game)
 IF LEN(a) THEN RETURN a
#endif

DIM buf(19) AS INTEGER
IF num >= 16384 AND trigger > 0 THEN
 IF loadrecord (buf(), workingdir + SLASH + "lookup" + STR(trigger) + ".bin", 20, num - 16384, NO) THEN
  DIM sname AS STRING = readbinstring(buf(), 1, 36)
  IF buf(0) THEN
   a = sname
  ELSE
   a = "[" & sname & "]"
  END IF
  GOTO theend
 END IF
END IF

IF num THEN
 a = "[id " & STR(num) & "]"
 DIM fh AS INTEGER = FREEFILE
 OPEN workingdir & SLASH & "plotscr.lst" FOR BINARY AS #fh
 WHILE loadrecord (buf(), fh, 20)
  IF buf(0) = num THEN
   a = STRING(small(large(buf(1), 0), 38), " ")
   array2str buf(), 4, a
   EXIT WHILE
  END IF
 WEND
 CLOSE fh
ELSE
 a = "[none]"
END IF

theend:
#ifdef IS_GAME
 add_string_cache cache(), num, a
#endif
return a
END FUNCTION

Function seconds2str(byval sec as integer, f as string = "%m:%S") as string
  dim ret as string
  dim as integer s, m, h
  s = sec mod 60
  m = (sec \ 60) mod 60
  h = (sec \ 3600) mod 60

  dim as integer i
  for i = 0 to len(f) - 1
    if f[i] = asc("%") then
      i+=1
      select case as const f[i]
        case asc("s")
          ret = ret & sec
        case asc("S")
          if s < 10 then ret = ret & "0"
          ret = ret & s
        case asc("m")
          ret = ret & (sec \ 60)
        case asc("M")
          if m < 10 then ret = ret & "0"
          ret = ret & m
        case asc("h")
          ret = ret & (sec \ 3600)
        case asc("H")
          if h < 10 then ret = ret & "0"
          ret = ret & h
        case asc("%")
          ret = ret & "%"
      end select
    else
      ret = ret & chr(f[i])
    end if
  next

  return ret
end function

FUNCTION getdefaultpal(fileset AS INTEGER, index AS INTEGER) as integer
 DIM v AS SHORT
 DIM f AS STRING = workingdir & SLASH & "defpal" & fileset & ".bin"
 IF isfile(f) THEN
  DIM fh AS INTEGER = FREEFILE
  OPEN f FOR BINARY AS #fh
  GET #fh, 1 + index * 2, v
  CLOSE #fh
  RETURN v
 ELSE
  'currently extended NPCs palettes are initialised to -1, which means lots of debug spam in old games
  'debug "Default palette file " & f & " does not exist"
  RETURN -1
 END IF
END FUNCTION

FUNCTION abs_pal_num(byval num as integer, byval sprtype as integer, byval spr as integer) as integer
 IF num >= 0 THEN RETURN num
 IF num = -1 THEN RETURN getdefaultpal(sprtype, spr)
 debug "decode_default_pal: invalid palette " & num
 RETURN 0
END FUNCTION

SUB loaddefaultpals(fileset AS INTEGER, poffset() AS INTEGER, sets AS INTEGER)
 DIM v AS SHORT
 DIM f AS STRING = workingdir & SLASH & "defpal" & fileset & ".bin"
 IF isfile(f) THEN
  DIM fh AS INTEGER = FREEFILE
  OPEN f FOR BINARY AS #fh
  FOR i AS INTEGER = 0 to sets
   GET #fh, 1 + i * 2, v
   poffset(i) = v
  NEXT i
  CLOSE #fh
 ELSE
  guessdefaultpals fileset, poffset(), sets
 END IF
END SUB

SUB savedefaultpals(fileset AS INTEGER, poffset() AS INTEGER, sets AS INTEGER)
 DIM v AS SHORT
 DIM f AS STRING = workingdir & SLASH & "defpal" & fileset & ".bin"
 DIM fh AS INTEGER = FREEFILE
 OPEN f FOR BINARY AS #fh
 FOR i AS INTEGER = 0 to sets
  v = poffset(i)
  PUT #fh, 1 + i * 2, v
 NEXT i
 CLOSE #fh
END SUB

SUB guessdefaultpals(fileset AS INTEGER, poffset() AS INTEGER, sets AS INTEGER)
 DIM her as herodef
 DIM i AS INTEGER
 DIM j AS INTEGER
 DIM found AS INTEGER
 
 flusharray poffset(), sets, 0
 SELECT CASE fileset
 CASE 0 'Heroes
  FOR j = 0 TO gen(genMaxHero)
   loadherodata @her, j
   IF her.sprite >= 0 AND her.sprite <= sets THEN poffset(her.sprite) = her.sprite_pal
  NEXT
 CASE 1 TO 3 'Enemies
  'Inefficient
  DIM enemy AS EnemyDef
  FOR j = 0 TO gen(genMaxEnemy)
   loadenemydata enemy, j
   IF enemy.size + 1 = fileset THEN
    IF enemy.pic >= 0 AND enemy.pic <= sets THEN poffset(enemy.pic) = enemy.pal
   END IF
  NEXT j
 CASE 4 'Walkabouts
  FOR j = 0 TO gen(genMaxHero)
   loadherodata @her, j
   IF her.walk_sprite >= 0 AND her.walk_sprite <= sets THEN
	poffset(her.walk_sprite) = her.walk_sprite_pal
   END IF
  NEXT j
  REDIM npcbuf(0) AS NPCType
  FOR mapi AS INTEGER = 0 TO gen(genMaxMap)
   LoadNPCD maplumpname(mapi, "n"), npcbuf()
   FOR j = 0 to UBOUND(npcbuf)
	IF npcbuf(j).picture >= 0 AND npcbuf(j).picture <= sets THEN
	 poffset(npcbuf(j).picture) = npcbuf(j).palette
	END IF
   NEXT j
  NEXT mapi
 CASE 5 'Weapons
  REDIM buf(dimbinsize(binITM))
  FOR j = 0 TO gen(genMaxItem)
   loaditemdata buf(), j
   IF buf(49) = 1 THEN
    IF buf(52) >= 0 AND buf(52) <= sets THEN poffset(buf(52)) = buf(53)
   END IF
  NEXT
 CASE 6 'Attacks
  REDIM buf(40 + dimbinsize(binATTACK))
  FOR j = 0 TO gen(genMaxAttack)
   loadattackdata buf(), j
   IF buf(0) >= 0 AND buf(0) <= sets THEN poffset(buf(0)) = buf(1)
  NEXT
 CASE ELSE 'Portraits and later
  'Default palettes were implemented before portraits, so this can only be called
  'the first time you ever open the portrait editor in an old game -- no point
  'implementing this
 END SELECT
END SUB

FUNCTION defbinsize (id AS INTEGER) as integer
 'returns the default size in BYTES to use for getbinsize() when no BINSIZE data is available at all
 IF id = 0 THEN RETURN 0  'attack.bin
 IF id = 1 THEN RETURN 64 '.stf
 IF id = 2 THEN RETURN 0  'songdata.bin
 IF id = 3 THEN RETURN 0  'sfxdata.bin
 IF id = 4 THEN RETURN 40 '.map
 IF id = 5 THEN RETURN 0  'menus.bin
 IF id = 6 THEN RETURN 0  'menuitem.bin
 IF id = 7 THEN RETURN 0  'uicolors.bin
 IF id = 8 THEN RETURN 400  '.say
 IF id = 9 THEN RETURN 30   '.n##
 IF id = 10 THEN RETURN 636 '.dt0
 IF id = 11 THEN RETURN 320 '.dt1
 IF id = 12 THEN RETURN 200 '.itm
 RETURN 0
END FUNCTION

FUNCTION curbinsize (id AS INTEGER) as integer
 'returns the native size in BYTES of the records for the version you are running
 IF id = 0 THEN RETURN 546 'attack.bin
 IF id = 1 THEN RETURN 84  '.stf
 IF id = 2 THEN RETURN 32  'songdata.bin
 IF id = 3 THEN RETURN 34  'sfxdata.bin
 IF id = 4 THEN RETURN 64  '.map
 IF id = 5 THEN RETURN 52  'menus.bin
 IF id = 6 THEN RETURN 64  'menuitem.bin
 IF id = 7 THEN RETURN 126 'uicolors.bin
 IF id = 8 THEN RETURN 412 '.say
 IF id = 9 THEN RETURN 32  '.n##
 IF id = 10 THEN RETURN 858 '.dt0
 IF id = 11 THEN RETURN 734 '.dt1
 IF id = 12 THEN RETURN 420 '.itm
 RETURN 0
END FUNCTION

FUNCTION getbinsize (id AS INTEGER) as integer
'returns the current size in BYTES of the records in the specific binary file you are working with
IF isfile(workingdir + SLASH + "binsize.bin") THEN
 DIM as short recordsize
 DIM fh AS INTEGER = FREEFILE
 OPEN workingdir + SLASH + "binsize.bin" FOR BINARY AS #fh
 IF LOF(fh) < 2 * id + 2 THEN
  getbinsize = defbinsize(id)
 ELSE
  GET #fh, 1 + id * 2, recordsize
  getbinsize = recordsize
 END IF
 CLOSE #fh
ELSE
 getbinsize = defbinsize(id)
END IF

END FUNCTION

'INTS, not bytes!
FUNCTION dimbinsize (id AS INTEGER) as integer
 'curbinsize is size supported by current version of engine
 'getbinsize is size of records in RPG file
 dimbinsize = large(curbinsize(id), getbinsize(id)) \ 2 - 1
END FUNCTION

SUB setbinsize (id AS INTEGER, size AS INTEGER)
 DIM fh AS INTEGER = FREEFILE
 OPEN workingdir & SLASH & "binsize.bin" FOR BINARY AS #fh
 PUT #fh, 1 + id * 2, CAST(short, size)
 CLOSE #fh
END SUB

'Normally gamedir will be workingdir, and sourcefile will be sourcerpg
FUNCTION readarchinym (gamedir as string, sourcefile as string) as string
 DIM iname as string
 DIM fh as integer
 IF isfile(gamedir + SLASH + "archinym.lmp") THEN
  fh = FREEFILE
  OPEN gamedir + SLASH + "archinym.lmp" FOR INPUT AS #fh
  LINE INPUT #fh, iname
  CLOSE #fh
  iname = LCASE(iname)
  'IF isfile(gamedir + SLASH + iname + ".gen") THEN
   RETURN iname
  'ELSE
  ' debug gamedir + SLASH + "archinym.lmp" + " invalid, ignored"
  'END IF
 ELSE
  debuginfo gamedir + SLASH + "archinym.lmp" + " unreadable"
 END IF

 ' for backwards compatibility with ancient games that lack archinym.lmp
 iname = LCASE(trimextension(trimpath(sourcefile)))
 'IF isfile(gamedir + SLASH + iname + ".gen") = 0 THEN
 ' fatalerror "archinym.lmp unusable, and internal name could not be determined"
 'ENDIF
 RETURN iname
END FUNCTION

FUNCTION maplumpname (map AS INTEGER, oldext AS STRING) as string
 IF map < 100 THEN
  return game & "." & oldext & RIGHT("0" & map, 2)
 ELSE
  return workingdir & SLASH & map & "." & oldext
 END IF
END FUNCTION

SUB fatalerror (msg AS STRING)
 showerror msg, YES
END SUB

'cleanup: (Custom only) whether to delete working.tmp
SUB showerror (msg AS STRING, BYVAL isfatal AS INTEGER = NO)
 STATIC entered = 0  'don't allow reentry
 IF entered THEN EXIT SUB
 entered = 1
 debug "error: " + msg
 PRINT "error: " + msg

 DIM quitmsg as string
 quitmsg = !"\n\n"   '"${K" & uilook(uiMenuItem) & "}"
 quitmsg += "An error has occurred!"
 IF isfatal THEN
  quitmsg += " Press any key to quit."
 ELSE
  quitmsg += " Press ESC to cleanly quit, or any other key to ignore the error and try to continue."
 END IF
 #IFDEF IS_CUSTOM
  IF cleanup_on_error = NO THEN
   quitmsg += !"\nThe editing state of the game will be preserved"
   IF isfatal = NO THEN quitmsg += " if you quit immediately"
   quitmsg += "; run " + CUSTOMEXE + " again and you will be asked whether you want to recover it."
  END IF
 #ENDIF
 quitmsg += !"\nIf this error is unexpected, please send an e-mail to ohrrpgce-crash@HamsterRepublic.com"

 'Reset palette (in case the error happened in a fade-to-black or due to
 'corrupt/missing palette or UI colours)
 load_default_master_palette master()
 DefaultUIColors uilook()
 setpal master()
 clearpage 0
 basic_textbox msg + quitmsg, uilook(uiText), 0
 setvispage 0

 setwait 200  'Give user a chance to let go of keys
 dowait
 DIM w AS INTEGER = getkey
 IF isfatal ORELSE w = scEsc THEN
 #IFDEF IS_CUSTOM
  IF cleanup_on_error THEN
   touchfile workingdir & SLASH & "__danger.tmp"
   killdir workingdir
  END IF

  closemusic
  restoremode

  'no need for end_debug
  SYSTEM
 #ELSE
  exitprogram 0, 1
 #ENDIF
 END IF

 #IFDEF IS_CUSTOM
  'Continuing to edit
  setwait 200  'Give user a chance to let go of keys
  dowait
  clearpage 0
  basic_textbox "Warning! If you had unsaved changes to your game you should backup the old .RPG file " _
                "before attempting to save, because there is a chance that saving will produce a corrupt file.", _
                uilook(uiText), 0
  setvispage 0
  getkey
 #ENDIF

 'Restore game's master palette (minus fades or palette changes...
 'but that's the least of your worries at this point)
 loadpalette master(), gen(genMasterPal)
 LoadUIColors uilook(), gen(genMasterPal)
 setpal master()

 entered = 0
END SUB

'Returns left edge x coord of a string centred at given x
FUNCTION xstring (s AS STRING, x AS INTEGER) as integer
 return small(large(x - LEN(s) * 4, 0), 319 - LEN(s) * 8)
END FUNCTION

FUNCTION defaultint (n AS INTEGER, default_caption AS STRING="default") AS STRING
 IF n = -1 THEN RETURN default_caption
 RETURN STR(n)
END FUNCTION

FUNCTION caption_or_int (n AS INTEGER, captions() AS STRING) AS STRING
 IF n >= LBOUND(captions) AND n <= UBOUND(captions) THEN RETURN captions(n)
 RETURN STR(n)
END FUNCTION

SUB poke8bit (array16() AS INTEGER, index AS INTEGER, val8 AS INTEGER)
 IF index \ 2 > UBOUND(array16) THEN
  debug "Dang rotten poke8bit(array(" & UBOUND(array16) & ")," & index & "," & val8 & ") out of range"
  EXIT SUB
 END IF
 IF val8 <> (val8 AND &hFF) THEN
   debug "Warning: " & val8 & " is not an 8-bit number. Discarding bits: " & (val8 XOR &hFF)
   val8 = val8 AND &hFF
 END IF
 DIM element AS INTEGER = array16(index \ 2)
 DIM lb AS INTEGER = element AND &hFF
 DIM hb AS INTEGER = (element AND &hFF00) SHR 8
 IF index AND 1 THEN
  hb = val8
 ELSE
  lb = val8
 END IF
 element = lb OR (hb SHL 8)
 array16(index \ 2) = element
END SUB

FUNCTION peek8bit (array16() AS INTEGER, index AS INTEGER) as integer
 IF index \ 2 > UBOUND(array16) THEN
  debug "Dang rotten peek8bit(array(" & UBOUND(array16) & ")," & index & ") out of range"
  RETURN 0
 END IF
 DIM element AS INTEGER = array16(index \ 2)
 IF index AND 1 THEN
  RETURN (element AND &hFF00) SHR 8
 ELSE
  RETURN element AND &hFF
 END IF
END FUNCTION

SUB loadpalette(pal() as RGBcolor, palnum)
IF palnum < 0 THEN
 debug "loadpalette: invalid palnum " & palnum
 palnum = 0
END IF
IF NOT isfile(workingdir + SLASH + "palettes.bin") THEN
 '.MAS fallback, palnum ignored because it doesn't matter
 DIM oldpalbuf(767)
 xbload game + ".mas", oldpalbuf(), "master palette missing from " + sourcerpg
 convertpalette oldpalbuf(), pal()
ELSE
 DIM AS SHORT headsize, recsize
 DIM palbuf(767) as UBYTE

 DIM fh AS INTEGER = FREEFILE
 OPEN workingdir + SLASH + "palettes.bin" FOR BINARY AS #fh
 GET #fh, , headsize
 GET #fh, , recsize
 GET #fh, recsize * palnum + headsize + 1, palbuf()
 CLOSE #fh
 FOR i AS INTEGER = 0 TO 255
  pal(i).r = palbuf(i * 3)
  pal(i).g = palbuf(i * 3 + 1)
  pal(i).b = palbuf(i * 3 + 2)
 NEXT
END IF
'Uncomment the line below if you want the palette in text format for updating load_default_master_palette
'dump_master_palette_as_hex pal()
END SUB

SUB savepalette(pal() as RGBcolor, palnum)
 DIM AS SHORT headsize = 4, recsize = 768  'Defaults

 DIM fh AS INTEGER = FREEFILE
 OPEN workingdir + SLASH + "palettes.bin" FOR BINARY AS #fh
 IF LOF(fh) >= 4 THEN
  GET #fh, 1, headsize
  GET #fh, 3, recsize
 ELSE
  PUT #fh, 1, headsize
  PUT #fh, 3, recsize
 END IF

 DIM palbuf(recsize - 1) as UBYTE
 FOR i AS INTEGER = 0 TO 255
  palbuf(i * 3) = pal(i).r
  palbuf(i * 3 + 1) = pal(i).g
  palbuf(i * 3 + 2) = pal(i).b
 NEXT
 PUT #fh, recsize * palnum + headsize + 1, palbuf()
 CLOSE #fh
END SUB

SUB convertpalette(oldpal() as integer, newpal() as RGBcolor)
'takes a old QB style palette (as 768 ints), translates it to
'8 bits per component and writes it to the provided RGBcolor array
DIM r AS INTEGER
DIM g AS INTEGER
DIM b AS INTEGER

FOR i AS INTEGER = 0 TO 255
 r = oldpal(i * 3)
 g = oldpal(i * 3 + 1)
 b = oldpal(i * 3 + 2)
 'newpal(i).r = r shl 2 or r shr 4
 'newpal(i).g = g shl 2 or g shr 4
 'newpal(i).b = b shl 2 or b shr 4
 newpal(i).r = iif(r, r shl 2 + 3, 0)   'Mapping as Neo suggested
 newpal(i).g = iif(g, g shl 2 + 3, 0)
 newpal(i).b = iif(b, b shl 2 + 3, 0)
NEXT
END SUB

SUB unconvertpalette()
'Takes the default new format palette and saves it in the old QB style palette
'format. This is only here to help out old graphics tools
DIM newpal(255) as RGBcolor, oldpal(767) as INTEGER
loadpalette newpal(), gen(genMasterPal)
FOR i AS INTEGER = 0 TO 255
 oldpal(i * 3) = newpal(i).r \ 4
 oldpal(i * 3 + 1) = newpal(i).g \ 4
 oldpal(i * 3 + 2) = newpal(i).b \ 4
NEXT
xbsave game + ".mas", oldpal(), 1536
END SUB

FUNCTION getmapname (m) as string
 DIM nameread(39) AS INTEGER
 loadrecord nameread(), game + ".mn", 40, m
 DIM a AS STRING = STRING(small((nameread(0) AND 255), 39), " ")
 array2str nameread(), 1, a
 RETURN a
END FUNCTION

FUNCTION createminimap (map() AS TileMap, tilesets() AS TilesetData ptr, BYREF zoom AS INTEGER = -1) AS Frame PTR
 'if zoom is -1, calculate and store it

 IF zoom = -1 THEN
  'auto-detect best zoom
  zoom = bound(small(vpages(vpage)->w \ map(0).wide, vpages(vpage)->h \ map(0).high), 1, 20)
 END IF

 DIM mini as Frame Ptr
 mini = frame_new(zoom * map(0).wide, zoom * map(0).high)

 DIM AS SINGLE fraction
 fraction = 20 / zoom

 DIM tx AS INTEGER
 DIM ty AS INTEGER
 DIM x AS INTEGER
 DIM y AS INTEGER
 DIM block AS INTEGER
 DIM pixel AS INTEGER
 
 FOR j AS INTEGER = 0 TO zoom * map(0).high - 1
  FOR i AS INTEGER = 0 TO zoom * map(0).wide - 1
   tx = i \ zoom
   ty = j \ zoom
   x = INT(((i MOD zoom) + RND) * fraction)
   y = INT(((j MOD zoom) + RND) * fraction)
   'layers but not overhead tiles
   pixel = 0
   FOR k AS INTEGER = UBOUND(map) TO 0 STEP -1
    block = readblock(map(k), tx, ty)
    IF block = 0 AND map(k).layernum > 0 THEN CONTINUE FOR

    WITH *tilesets(k)
     IF block > 207 THEN block = (block - 48 + .tastuf(20) + .anim(1).cycle) MOD 160
     IF block > 159 THEN block = (block + .tastuf(0) + .anim(0).cycle) MOD 160
     pixel = .spr->image[block * 400 + y * 20 + x]
     IF pixel <> 0 THEN EXIT FOR
    END WITH
   NEXT
   mini->image[i + j * mini->w] = pixel
  NEXT
 NEXT

 RETURN mini
END FUNCTION

FUNCTION createminimap (layer AS TileMap, tileset AS TilesetData ptr, BYREF zoom AS INTEGER = -1) AS Frame PTR
 DIM layers(0) AS TileMap
 DIM tilesets(0) AS TilesetData ptr
 layers(0) = layer
 tilesets(0) = tileset
 RETURN createminimap(layers(), tilesets(), zoom)
END FUNCTION

FUNCTION readattackname (index) as string
 RETURN readbadgenericname(index, game + ".dt6", 80, 24, 10, 1)
END FUNCTION

FUNCTION readenemyname (index) as string
 RETURN readbadgenericname(index, game + ".dt1", getbinsize(binDT1), 0, 16, 0)
END FUNCTION

FUNCTION readitemname (index) as string
 RETURN readbadgenericname(index, game + ".itm", getbinsize(binITM), 0, 8, 0)
END FUNCTION

FUNCTION readitemdescription (byval index as integer) as string
 RETURN readbadgenericname(index, game + ".itm", getbinsize(binITM), 9, 35, 0)
END FUNCTION

FUNCTION readshopname (shopnum) as string
 RETURN readbadgenericname(shopnum, game + ".sho", 40, 0, 15, 0)
END FUNCTION

FUNCTION getsongname (num AS INTEGER, prefixnum AS INTEGER = 0) as string
 DIM songd(dimbinsize(binSONGDATA)) AS INTEGER
 DIM s AS STRING
 IF num = -1 THEN RETURN "-none-"
 s = ""
 IF prefixnum THEN s = num & " "
 setpicstuf songd(), curbinsize(binSONGDATA), -1
 loadset workingdir + SLASH + "songdata.bin", num, 0
 s = s & readbinstring(songd(), 0, 30)
 RETURN s
END FUNCTION

FUNCTION getsfxname (num AS INTEGER) as string
 DIM sfxd(dimbinsize(binSFXDATA)) AS INTEGER
 setpicstuf sfxd(), curbinsize(binSFXDATA), -1
 loadset workingdir & SLASH & "sfxdata.bin", num, 0
 RETURN readbinstring (sfxd(), 0, 30)
END FUNCTION

'Modify an integer according to key input (less and more are scancodes for decrementing and incrementing)
'If returninput is true, returns whether the user tried to modify the int,
'otherwise returns true only if the int actually changed.
FUNCTION intgrabber (BYREF n AS INTEGER, BYVAL min AS INTEGER, BYVAL max AS INTEGER, BYVAL less AS INTEGER=scLeft, BYVAL more AS INTEGER=scRight, BYVAL returninput AS INTEGER=NO, BYVAL use_clipboard AS INTEGER=YES) AS INTEGER
 DIM AS LONGINT temp = n
 intgrabber = intgrabber(temp, cast(longint, min), cast(longint, max), less, more, returninput, use_clipboard)
 n = temp
END FUNCTION

'See above for documentation
FUNCTION intgrabber (BYREF n AS LONGINT, BYVAL min AS LONGINT, BYVAL max AS LONGINT, BYVAL less AS INTEGER=scLeft, BYVAL more AS INTEGER=scRight, BYVAL returninput AS INTEGER=NO, BYVAL use_clipboard AS INTEGER=YES) AS INTEGER
 STATIC clip AS LONGINT
 DIM old AS LONGINT = n
 DIM typed AS INTEGER = NO

 IF more <> 0 AND keyval(more) > 1 THEN
  n = loopvar(n, min, max, 1)
  typed = YES
 ELSEIF less <> 0 AND keyval(less) > 1 THEN
  n = loopvar(n, min, max, -1)
  typed = YES
 ELSE
  DIM sign AS INTEGER = SGN(n)
  n = ABS(n)
  IF keyval(scBackspace) > 1 THEN n \= 10: typed = YES

  DIM intext AS STRING = getinputtext
  FOR i AS INTEGER = 0 TO LEN(intext) - 1
   IF isdigit(intext[i]) THEN
    n = n * 10 + (intext[i] - ASC("0"))
    typed = YES
   END IF
  NEXT

  IF min < 0 AND max > 0 THEN
   IF keyval(scMinus) > 1 OR keyval(scNumpadMinus) > 1 THEN sign = sign * -1: typed = YES
   IF (keyval(scPlus) > 1 OR keyval(scNumpadPlus) > 1) AND sign < 0 THEN sign = sign * -1: typed = YES
  END IF
  IF min < 0 AND (sign < 0 OR max = 0) THEN n = -n
  'CLIPBOARD
  IF use_clipboard THEN
   IF copy_keychord() THEN clip = n
   IF paste_keychord() THEN
    n = clip
    typed = YES
   END IF
  END IF
  n = bound(n, min, max)
 END IF

 IF returninput THEN
  RETURN typed
 ELSE
  RETURN (old <> n)
 END IF
END FUNCTION

FUNCTION zintgrabber (n AS INTEGER, min AS INTEGER, max AS INTEGER, less AS INTEGER=75, more AS INTEGER=77) AS INTEGER
 '--adjust for entries that are offset by +1
 '--what a hack!
 '--all entries <= 0 are special options not meant to be enumerated
 '--supply the min & max as visible, not actual range for n
 '--eg a menu with 'A' = -2, 'B' = -1, 'C' = 0, 'item 0 - item 99' = 1 - 100 would have min = -3, max = 99
 DIM old AS INTEGER = n
 DIM temp AS INTEGER = n - 1
 '--must adjust to always be able to type in a number
 IF temp < 0 THEN
  FOR i AS INTEGER = 2 TO 11
   IF keyval(i) > 1 THEN temp = 0
  NEXT i
 END IF
 intgrabber temp, min, max, less, more
 n = temp + 1
 IF old = 1 AND keyval(scBackspace) > 1 THEN n = 0

 RETURN (old <> n)
END FUNCTION

FUNCTION xintgrabber (n AS INTEGER, pmin AS INTEGER, pmax AS INTEGER, nmin AS INTEGER=1, nmax AS INTEGER=1, less AS INTEGER=scLeft, more AS INTEGER=scRight) AS INTEGER
 '--quite a bit of documentation required:
 '--like zintgrabber, but for cases where positive values mean one thing, negatives
 '--another, and 0 means none.

 'Requirements:  nmax <= nmin <= 0 <= pmin <= pmax
 'Omit nmax and nmin for no negative range
 'nmin to nmax is the visible range of negative values
 'eg. nmin = -1 nmax = -100: negatives indicate a number between 1 and 100
 'pmin to pmax is positive range
 'eg. 2 - 50 means n==1 is '2', n==49 is '50', and 0 - 1 means n==1 is '0' and n==2 is '1'

 DIM old AS INTEGER = n

 'calculate the range of n
 DIM AS INTEGER valmin, valmax
 IF nmin <> 1 THEN
  valmin = -1 + (nmax - nmin)
 END IF
 valmax = 1 + (pmax - pmin)

 'calculate the visible value
 DIM AS INTEGER visval, oldvisval
 IF n > 0 THEN
  visval = n + pmin - 1
 ELSEIF n < 0 THEN
  visval = n + nmin + 1
 ELSE
  visval = 0
 END IF
 oldvisval = visval

 IF more <> 0 ANDALSO keyval(more) > 1 THEN
  'easy case
  n = loopvar(n, valmin, valmax, 1)
 ELSEIF less <> 0 ANDALSO keyval(less) > 1 THEN
  'easy case
  n = loopvar(n, valmin, valmax, -1)

/'--Why on earth do we want to support negation anyway?
 ELSEIF nmin < 0 AND pmax > 0 AND _
        (keyval(scMinus) > 1 OR keyval(scNumpadMinus) > 1 OR _
        ((keyval(scPlus) > 1 OR keyval(scNumpadPlus) > 1) AND s < 0)) THEN
  'nasty case: negate n based on *displayed* value
  visval = bound(-visval, nmax, pmax)
  n = ...
'/

 ELSE
  'horrible case: change n based on *displayed* value
  visval = ABS(visval)

  IF keyval(scBackspace) > 1 THEN
   visval \= 10

   'Special case for when backspace changes to None. Isolate this case to allow
   'some sanity in the rest of the logic
   IF (oldvisval = 0) OR (n > 0 AND visval < pmin) OR (n < 0 AND -visval > nmin) THEN
    n = 0
    RETURN YES
   END IF

  ELSE
   FOR i AS INTEGER = 1 TO 9
    IF keyval(i - 1 + sc1) > 1 THEN visval = visval * 10 + i
   NEXT i
   IF keyval(sc0) > 1 THEN visval *= 10
  END IF

  'convert absolute visval back to n
  'None can become positive, but positive remains positive and negative remains negative
  IF old = 0 THEN
   IF visval <> oldvisval THEN
    visval = bound(visval, pmin, pmax)
    n = visval - pmin + 1
   END IF
  ELSEIF old > 0 THEN
   visval = bound(visval, pmin, pmax)
   n = visval - pmin + 1
  ELSE
   visval = bound(-visval, nmax, nmin)
   n = visval - nmin - 1
  END IF
 END IF

 RETURN (old <> n)
END FUNCTION

FUNCTION stredit (s AS STRING, BYREF insert AS INTEGER, BYVAL maxl AS INTEGER, BYVAL numlines AS INTEGER=1, BYVAL wrapchars AS INTEGER=1) AS INTEGER
 'Return value is the line that the cursor is on, or 0 if numlines=1
 'insert is the position of the cursor (range 0..LEN(s)-1), and is modified BYREF. Set to -1 to move automatically to end of string
 stredit = 0
 
 STATIC clip AS STRING

 '--copy+paste support
 IF copy_keychord() THEN clip = s
 IF paste_keychord() THEN s = LEFT(clip, maxl)

 '--insert cursor movement
 IF keyval(scCtrl) = 0 THEN 'not CTRL
  IF keyval(scLeft) > 1 THEN insert = large(0, insert - 1)
  IF keyval(scRight) > 1 THEN insert = small(LEN(s), insert + 1)
 ELSE 'CTRL
  IF keyval(scLeft) > 1 THEN 'move by word
   IF insert > 0 THEN 'searching from position -1 searches from the end
    insert = INSTRREV(s, ANY !" \n", insert - 1)  'different argument order: the FB devs, they are so funny
   END IF
  END IF
  IF keyval(scRight) > 1 THEN
   insert = INSTR(insert + 1, s, ANY !" \n")
   IF insert = 0 THEN insert = LEN(s)
  END IF
  IF keyval(scHome) > 1 THEN insert = 0
  IF keyval(scEnd) > 1 THEN insert = LEN(s)
 END IF

 '--up and down arrow keys
 IF numlines > 1 THEN
  DIM wrapped AS STRING
  wrapped = wordwrap(s, large(1, wrapchars))
  DIM lines() AS STRING
  split(wrapped, lines())
  DIM count AS INTEGER = 0
  DIM found_insert AS INTEGER = -1
  DIM line_chars AS INTEGER
  DIM move_lines AS INTEGER = 0
  FOR i AS INTEGER = 0 TO UBOUND(lines)
   IF count + LEN(lines(i)) >= insert THEN
    found_insert = i
    line_chars = insert - count
    EXIT FOR
   END IF
   count += LEN(lines(i)) + 1
  NEXT i
  IF found_insert >= 0 THEN
   '--set return value
   stredit = found_insert
   IF keyval(scUp) > 1 THEN move_lines = -1
   IF keyval(scDown) > 1 THEN move_lines = 1
   IF keyval(scPageUp) > 1 THEN move_lines = -(numlines - 2)
   IF keyval(scPageDown) > 1 THEN move_lines = numlines - 2
   IF move_lines THEN
    found_insert = bound(found_insert + move_lines, 0, UBOUND(lines))
    insert = 0
    FOR i AS INTEGER = 0 TO found_insert - 1
     insert += LEN(lines(i)) + 1
    NEXT i
    insert += small(line_chars, LEN(lines(large(found_insert, 0))))
    '--set return value
    stredit = found_insert
   END IF
   '--end of special handling for up and down motion
  END IF
  '--Home and end keys: go to previous/next newline,
  '--unless Ctrl is pressed, which is handled above
  IF keyval(scCtrl) = 0 THEN
   IF keyval(scHome) > 1 THEN
    IF insert > 0 THEN 'searching from position -1 searches from the end
     insert = INSTRREV(s, CHR(10), insert - 1)
    END IF
   END IF
   IF keyval(scEnd) > 1 THEN
    insert = INSTR(insert + 1, s, CHR(10))
    IF insert = 0 THEN insert = LEN(s) ELSE insert -= 1
   END IF
  END IF
  '--end of special keys that only work in multiline mode
 END IF

 IF insert < 0 THEN insert = LEN(s)
 insert = bound(insert, 0, LEN(s))

 DIM pre AS STRING = LEFT(s, insert)
 DIM post AS STRING = RIGHT(s, LEN(s) - insert)

 '--BACKSPACE support
 IF keyval(scBackspace) > 1 AND LEN(pre) > 0 THEN
  pre = LEFT(pre, LEN(pre) - 1)
  insert = large(0, insert - 1)
 END IF

 '--DEL support
 IF keyval(scDelete) > 1 AND LEN(post) > 0 THEN post = RIGHT(post, LEN(post) - 1)

 '--adding chars
 IF LEN(pre) + LEN(post) < maxl THEN
  DIM oldlen AS INTEGER = LEN(pre)
  IF keyval(scSpace) > 1 AND keyval(scCtrl) > 0 THEN
#IFDEF IS_CUSTOM
   '--charlist support
   pre = pre & charpicker()
#ENDIF
  ELSEIF keyval(scEnter) > 1 THEN
   IF numlines > 1 THEN
    pre = pre & CHR(10)
   END IF
  ELSE
   pre = LEFT(pre & getinputtext, maxl)
  END IF
  insert += (LEN(pre) - oldlen)
 END IF

 s = pre & post
 
END FUNCTION

SUB pop_warning(s AS STRING)
 
 '--Construct the warning UI (This will be hella easier later when the Slice Editor can save/load)
 DIM root AS Slice Ptr
 root = NewSliceOfType(slRoot)
 WITH *root
  .Y = 200
  .Fill = NO
 END WITH
 DIM outer_box AS Slice Ptr
 outer_box = NewSliceOfType(slContainer, root)
 WITH *outer_box
  .paddingTop = 20
  .paddingBottom = 20
  .paddingLeft = 20
  .paddingRight = 20
  .Fill = Yes
 END WITH
 DIM inner_box AS Slice Ptr
 inner_box = NewSliceOfType(slRectangle, outer_box)
 WITH *inner_box
  .paddingTop = 8
  .paddingBottom = 8
  .paddingLeft = 8
  .paddingRight = 8
  .Fill = YES
  ChangeRectangleSlice inner_box, 2
 END WITH
 DIM text_area AS Slice Ptr
 text_area = NewSliceOfType(slText, inner_box)
 WITH *text_area
  .Fill = YES
  ChangeTextSlice text_area, s, , , YES
 END WITH
 DIM animate AS Slice Ptr
 animate = root

 '--Preserve whatever screen was already showing as a background
 DIM holdscreen AS INTEGER
 holdscreen = allocatepage
 copypage vpage, holdscreen
 copypage vpage, dpage

 DIM dat AS TextSliceData Ptr
 dat = text_area->SliceData
 dat->line_limit = 15

 DIM deadkeys AS INTEGER = 25
 DIM cursor_line AS INTEGER = 0
 DIM scrollbar_state AS MenuState
 scrollbar_state.size = 16

 '--Now loop displaying text
 setkeys
 DO
  setwait 17
  setkeys
  
  IF deadkeys = 0 THEN 
   IF keyval(scESC) > 1 OR enter_or_space() THEN EXIT DO
   IF keyval(scUp) > 1 THEN dat->first_line -= 1
   IF keyval(scDown) > 1 THEN dat->first_line += 1
   dat->first_line = bound(dat->first_line, 0, large(0, dat->line_count - dat->line_limit))
  END IF
  deadkeys = large(deadkeys -1, 0)

  'Animate the arrival of the pop-up
  animate->Y = large(animate->Y - 20, 0)

  DrawSlice root, dpage
  
  WITH scrollbar_state
   .top = dat->first_line
   .last = dat->line_count - 1
  END WITH
  draw_fullscreen_scrollbar scrollbar_state, , dpage

  SWAP vpage, dpage
  setvispage vpage
  copypage holdscreen, dpage
  dowait
 LOOP

 '--Animate the removal of the help screen
 DO
  setkeys
  setwait 17
  animate->Y = animate->Y + 20
  IF animate->Y > 200 THEN EXIT DO
  DrawSlice root, dpage
  SWAP vpage, dpage
  setvispage vpage
  copypage holdscreen, dpage
  dowait
 LOOP
  
 freepage holdscreen
 DeleteSlice @root
END SUB

SUB show_help(helpkey AS STRING)
 DIM help_str AS STRING
 help_str = load_help_file(helpkey)
 
 '--Construct the help UI (This will be hella easier later when the Slice Editor can save/load)
 DIM help_root AS Slice Ptr
 help_root = NewSliceOfType(slRoot)
 WITH *help_root
  .Y = 200
  .Fill = NO
 END WITH
 DIM help_outer_box AS Slice Ptr
 help_outer_box = NewSliceOfType(slContainer, help_root)
 WITH *help_outer_box
  .paddingTop = 4
  .paddingBottom = 4
  .paddingLeft = 4
  .paddingRight = 4
  .Fill = Yes
 END WITH
 DIM help_box AS Slice Ptr
 help_box = NewSliceOfType(slRectangle, help_outer_box)
 WITH *help_box
  .paddingTop = 8
  .paddingBottom = 8
  .paddingLeft = 8
  .paddingRight = 8
  .Fill = YES
  ChangeRectangleSlice help_box, 1
 END WITH
 DIM help_text AS Slice Ptr
 help_text = NewSliceOfType(slText, help_box)
 WITH *help_text
  .Fill = YES
  ChangeTextSlice help_text, help_str, , , YES
 END WITH
 DIM animate AS Slice Ptr
 animate = help_root

 '--Preserve whatever screen was already showing as a background
 DIM holdscreen AS INTEGER
 holdscreen = allocatepage
 copypage vpage, holdscreen
 copypage vpage, dpage

 DIM dat AS TextSliceData Ptr
 dat = help_text->SliceData
 dat->line_limit = 18
 dat->insert = 0

 DIM editing AS INTEGER = NO
 DIM deadkeys AS INTEGER = 25
 DIM cursor_line AS INTEGER = 0
 DIM scrollbar_state AS MenuState
 scrollbar_state.size = 17

 '--Now loop displaying help
 setkeyrepeat  'reset repeat rate
 setkeys
 DO
  IF editing THEN
   setwait 30
  ELSE
   setwait 17
  END IF
  setkeys
  
  IF editing THEN  
   cursor_line = stredit(dat->s, dat->insert, 32767, dat->line_limit, help_text->Width \ 8)
   'The limit of 32767 chars is totally arbitrary and maybe not a good limit
  END IF

  IF deadkeys = 0 THEN 
   IF keyval(scESC) > 1 THEN
    '--If there are any changes to the help screen, offer to save them
    IF help_str = dat->s THEN
     EXIT DO
    ELSE
     'Prevent attempt to quit the program, stop and wait for response first
     DIM quitting as integer = keyval(-1)
     clearkey(-1)
     DIM choice AS INTEGER = twochoice("Save changes to help for """ & helpkey & """?", "Yes", "No", 0, -1)
     IF keyval(-1) THEN choice = 1  'Second attempt to quit: discard
     IF choice <> -1 THEN
      IF quitting THEN setquitflag
      IF choice = 0 THEN save_help_file helpkey, dat->s
      EXIT DO
     END IF
    END IF
   END IF
   IF keyval(scE) > 1 THEN
    IF fileiswriteable(get_help_dir() & SLASH & helpkey & ".txt") THEN
     editing = YES
     dat->show_insert = YES
     ChangeRectangleSlice help_box, , uilook(uiBackground), , 0
    ELSE
     pop_warning "Your """ & get_help_dir() & """ folder is not writable. Try making a copy of it at """ & homedir & SLASH & "ohrhelp"""
    END IF
   END IF
   IF keyval(scF1) and helpkey <> "helphelp" THEN
    show_help "helphelp"
   END IF
   IF editing THEN
    dat->first_line = small(dat->first_line, cursor_line - 1)
    dat->first_line = large(dat->first_line, cursor_line - (dat->line_limit - 2))
   ELSE
    '--not editing, just browsing
    IF keyval(scUp) > 1 THEN dat->first_line -= 1
    IF keyval(scDown) > 1 THEN dat->first_line += 1
    IF keyval(scPageUp) > 1 THEN dat->first_line -= dat->line_limit - 1
    IF keyval(scPageDown) > 1 THEN dat->first_line += dat->line_limit - 1
    IF keyval(scHome) > 1 THEN dat->first_line = 0
    IF keyval(scEnd) > 1 THEN dat->first_line = dat->line_count
   END IF
   dat->first_line = bound(dat->first_line, 0, large(0, dat->line_count - dat->line_limit))
  END IF
  deadkeys = large(deadkeys -1, 0)

  'Animate the arrival of the help screen
  animate->Y = large(animate->Y - 20, 0)

  copypage holdscreen, vpage

  DrawSlice help_root, vpage
  
  WITH scrollbar_state
   .top = dat->first_line
   .last = dat->line_count - 1
  END WITH
  draw_fullscreen_scrollbar scrollbar_state, , vpage

  setvispage vpage
  dowait
 LOOP

 '--Animate the removal of the help screen
 DO
  setkeys
  setwait 17
  animate->Y = animate->Y + 20
  IF animate->Y > 200 THEN EXIT DO
  copypage holdscreen, vpage
  DrawSlice help_root, vpage
  setvispage vpage
  dowait
 LOOP
  
 freepage holdscreen
 DeleteSlice @help_root
END SUB

FUNCTION multiline_string_editor(s AS STRING, helpkey AS STRING="") AS STRING
 'probably contains more code duplication than is apropriate when comared to the help_editor
 
 '--Construct the UI (loading a slice collection might be better here? but not from the RPG file!)
 DIM root AS Slice Ptr
 root = NewSliceOfType(slRoot)
 WITH *root
  .Y = 200
  .Fill = NO
 END WITH
 DIM outer_box AS Slice Ptr
 outer_box = NewSliceOfType(slContainer, root)
 WITH *outer_box
  .paddingTop = 4
  .paddingBottom = 4
  .paddingLeft = 4
  .paddingRight = 4
  .Fill = Yes
 END WITH
 DIM box AS Slice Ptr
 box = NewSliceOfType(slRectangle, outer_box)
 WITH *box
  .paddingTop = 8
  .paddingBottom = 8
  .paddingLeft = 8
  .paddingRight = 8
  .Fill = YES
  ChangeRectangleSlice box, , uilook(uiBackground), , 0
 END WITH
 DIM text AS Slice Ptr
 text = NewSliceOfType(slText, box)
 WITH *text
  .Fill = YES
  ChangeTextSlice text, s, , , YES
 END WITH
 DIM animate AS Slice Ptr
 animate = root

 '--Preserve whatever screen was already showing as a background
 DIM holdscreen AS INTEGER
 holdscreen = allocatepage
 copypage vpage, holdscreen
 copypage vpage, dpage

 DIM dat AS TextSliceData Ptr
 dat = text->SliceData
 dat->line_limit = 18
 dat->insert = 0
 dat->show_insert = YES

 DIM deadkeys AS INTEGER = 25
 DIM cursor_line AS INTEGER = 0
 DIM scrollbar_state AS MenuState
 scrollbar_state.size = 17

 '--Now loop displaying help
 setkeyrepeat  'reset repeat rate
 setkeys
 DO
  setwait 30
  setkeys
  
  cursor_line = stredit(dat->s, dat->insert, 32767, dat->line_limit, text->Width \ 8)
  'The limit of 32767 chars is totally arbitrary and maybe not a good limit

  IF keyval(scESC) > 1 THEN
   '--If there are any changes to the help screen, offer to save them
   IF s = dat->s THEN
    EXIT DO
   ELSE
    DIM choice AS INTEGER = twochoice("Keep changes to this text?", "Yes", "No", 0, -1)
    IF choice = 1 THEN dat->s = s '--don't use changes!
    IF choice >= 0 THEN EXIT DO
   END IF
  END IF
  IF deadkeys = 0 THEN 
   IF keyval(scF1) AND helpkey <> "" THEN show_help helpkey
   dat->first_line = small(dat->first_line, cursor_line - 1)
   dat->first_line = large(dat->first_line, cursor_line - (dat->line_limit - 2))
   dat->first_line = bound(dat->first_line, 0, large(0, dat->line_count - dat->line_limit))
  END IF
  deadkeys = large(deadkeys -1, 0)

  'Animate the arrival of the help screen
  animate->Y = large(animate->Y - 20, 0)

  copypage holdscreen, vpage

  DrawSlice root, vpage
  
  WITH scrollbar_state
   .top = dat->first_line
   .last = dat->line_count - 1
  END WITH
  draw_fullscreen_scrollbar scrollbar_state, , vpage

  setvispage vpage
  dowait
 LOOP

 '--Animate the removal of the multiline text editor
 DO
  setkeys
  setwait 17
  animate->Y = animate->Y + 20
  IF animate->Y > 200 THEN EXIT DO
  copypage holdscreen, vpage
  DrawSlice root, vpage
  setvispage vpage
  dowait
 LOOP

 DIM result AS STRING
 result = dat->s
 
 freepage holdscreen
 DeleteSlice @root
 
 RETURN result
END FUNCTION

FUNCTION multichoice(capt AS STRING, choices() AS STRING, defaultval AS INTEGER=0, escval AS INTEGER=-1, helpkey AS STRING="") AS INTEGER
 DIM state AS MenuState
 DIM menu AS MenuDef
 ClearMenuData menu
 DIM result AS INTEGER
 DIM captlines() AS STRING
 DIM wide AS INTEGER

 split(wordwrap(capt, 37), captlines())
 FOR i AS INTEGER = 0 TO UBOUND(captlines)
  wide = large(wide, LEN(captlines(i)))
 NEXT

 FOR i AS INTEGER = 0 TO UBOUND(choices)
  append_menu_item menu, choices(i)
 NEXT

 state.active = YES
 menu.maxrows = 10
 init_menu_state state, menu
 state.pt = defaultval
 menu.offset.Y = -20 + 5 * UBOUND(captlines)
 menu.anchor.Y = -1

 'Keep whatever was on the screen already as a background (NOTE: this doesn't always work (not necessarily vpage))
 DIM holdscreen AS INTEGER
 holdscreen = allocatepage
 copypage vpage, holdscreen

 setkeys
 DO
  setwait 55
  setkeys

  IF keyval(scEsc) > 1 THEN
   result = escval
   state.active = NO
  END IF

  IF keyval(scF1) > 1 ANDALSO LEN(helpkey) > 0 THEN
   show_help helpkey
  END IF

  IF enter_or_space() THEN
   result = state.pt
   state.active = NO
  END IF

  IF state.active = NO THEN EXIT DO
  
  usemenu state

  copypage holdscreen, vpage
  centerbox 160, 70, 16 + wide * 8, 16 + 10 * UBOUND(captlines), 2, vpage
  FOR i AS INTEGER = 0 TO UBOUND(captlines)
   edgeprint captlines(i), xstring(captlines(i), 160), 65 - 5 * UBOUND(captlines) + i * 10, uilook(uiMenuItem), vpage
  NEXT
  draw_menu menu, state, vpage
  IF LEN(helpkey) > 0 THEN
   edgeprint "F1 Help", 0, 190, uilook(uiMenuItem), vpage
  END IF
  setvispage vpage
  dowait
 LOOP
 setkeys
 freepage holdscreen
 ClearMenuData menu

 RETURN result
END FUNCTION

FUNCTION twochoice(capt AS STRING, strA AS STRING="Yes", strB AS STRING="No", defaultval AS INTEGER=0, escval AS INTEGER=-1, helpkey AS STRING="") AS INTEGER
 DIM choices(1) AS STRING = {strA, strB}
 RETURN multichoice(capt, choices(), defaultval, escval, helpkey)
END FUNCTION

'Asks a yes-or-no pop-up question.
'(Not to be confused with yesorno(), which returns a yes/no string)
FUNCTION yesno(capt AS STRING, BYVAL defaultval AS INTEGER=YES, escval AS INTEGER=NO) AS INTEGER
 IF defaultval THEN defaultval = 0 ELSE defaultval = 1
 IF escval THEN escval = 0 ELSE escval = 1
 DIM result AS INTEGER
 result = twochoice(capt, "Yes", "No", defaultval, escval)
 IF result = 0 THEN RETURN YES
 IF result = 1 THEN RETURN NO
END FUNCTION

SUB playsongnum (songnum%)
  DIM songbase AS STRING, songfile AS STRING

  songbase = workingdir & SLASH & "song" & songnum%
  songfile = ""
  
  IF isfile(songbase & ".mp3") THEN
    songfile = songbase & ".mp3"
  ELSEIF isfile(songbase & ".ogg") THEN
    songfile = songbase & ".ogg"
  ELSEIF isfile(songbase & ".mod") THEN
    songfile = songbase & ".mod"
  ELSEIF isfile(songbase & ".xm") THEN
    songfile = songbase & ".xm"
  ELSEIF isfile(songbase & ".s3m") THEN
    songfile = songbase & ".s3m"
  ELSEIF isfile(songbase & ".it") THEN
    songfile = songbase & ".it"
  ELSEIF isfile(songbase & ".mid") THEN
    songfile = songbase & ".mid"
  ELSEIF isfile(songbase & ".bam") THEN
    songfile = songbase & ".bam"
  ELSEIF isfile(game & "." & songnum) THEN
    songfile = game & "." & songnum ' old-style BAM naming scheme
  END IF

  if songfile = "" then exit sub
  loadsong songfile
END SUB

FUNCTION spawn_and_wait (app AS STRING, args AS STRING) as string
 'Run a commandline program in a terminal emulator and wait for it to finish. 
 'On Windows the program is run asynchronously and users are offered the option to kill it.
 'On other platforms the program just freezes.
 'You can of course also kill the program on all platforms with Ctrl+C
 'Returns an error message, or "" if no apparent failure

 'It may be better to pass arguments in an array (the Unix way), so that
 'we can do all the necessary quoting required for Windows here.

#IFDEF __FB_DARWIN__

 basic_textbox "Please wait, running " & trimpath(app), uilook(uiText), vpage
 setvispage vpage

 'Wow that's a crazy amount of indirection!
 'Running Terminal.app is the only way to get a terminal, but 'open' is for opening a file with an application only,
 'so we use an AppleScript script embedded in Terminal_wrapper.sh to run HSpeak

 DIM term_wrap as string = find_helper_app("Terminal_wrapper.sh")
 IF term_wrap = "" THEN RETURN missing_helper_message("Terminal_wrapper.sh")

 DIM fh as integer
 DIM dummyscript as string = tmpdir + "dummyscript" & RND * 10000 & ".sh"
 fh = FREEFILE
 OPEN dummyscript FOR OUTPUT AS #fh
 PRINT #fh, "#!/bin/sh"
 PRINT #fh, "cd " & curdir()
 PRINT #fh, "clear"
 PRINT #fh, app + " " + args
 CLOSE #fh
 SHELL "chmod +x " + dummyscript
 SHELL term_wrap + " " + dummyscript
 safekill dummyscript
 RETURN ""

#ENDIF

#IFDEF __FB_WIN32__

 DIM handle AS ProcessHandle
 handle = open_console_process(app, args)
 IF handle = 0 THEN
  RETURN "Could not run " & app
 END IF

 DIM dots AS INTEGER = 0
 DIM exitcode AS INTEGER
 setkeys
 DO
  setwait 400
  setkeys
  IF process_running(handle, @exitcode) = NO THEN
   cleanup_process @handle
   IF exitcode THEN
    'Error, or the user might have killed the program some other way
    RETURN trimpath(app) + " reported failure."
   END IF
   RETURN ""
  END IF
  IF keyval(scEsc) > 1 THEN
   kill_process handle
   cleanup_process @handle
   setkeys
   RETURN "User cancelled."
  END IF

  dots = (dots + 1) MOD 5
  centerbox 160, 100, 300, 36, 3, vpage
  edgeprint "Please wait, running " & trimpath(app) & STRING(dots, "."), 15, 90, uilook(uiText), vpage
  edgeprint "Press ESC to cancel", 15, 100, uilook(uiMenuItem), vpage
  setvispage vpage

  dowait
 LOOP

#ENDIF

 'Generic UNIX: xterm is everywhere, isn't it?

 'os_* process handling functions only currently implemented on Windows
 SHELL "xterm -bg black -fg gray90 -e """ & app & " " & args & """"
 RETURN ""

END FUNCTION

FUNCTION find_madplay () AS STRING
 STATIC cached AS INTEGER = 0
 STATIC cached_app AS STRING
 IF cached THEN RETURN cached_app
 cached_app = find_helper_app("madplay")
 cached = -1
 RETURN cached_app
END FUNCTION

FUNCTION find_oggenc () AS STRING
 STATIC cached AS INTEGER = 0
 STATIC cached_app AS STRING
 IF cached THEN RETURN cached_app
 cached_app = find_helper_app("oggenc")
 IF cached_app = "" THEN cached_app = find_helper_app("oggenc2")
 cached = -1
 RETURN cached_app
END FUNCTION

FUNCTION find_helper_app (appname AS STRING) AS STRING
'Returns an empty string if the app is not found, or the full path if it is found

'Look in the same folder as CUSTOM/GAME
IF isfile(exepath & SLASH & appname & DOTEXE) THEN RETURN exepath & SLASH & appname & DOTEXE

#IFDEF __FB_DARWIN__
IF isfile(exepath & "/support/" & appname) THEN RETURN exepath & "/support/" & appname
#ENDIF
#IFDEF __UNIX__
'--Find helper app on Unix
DIM AS INTEGER fh
DIM AS STRING tempfile
DIM AS STRING s
tempfile = tmpdir & "find_helper_app." & INT(RND * 10000) & ".tmp"
'Use the standard util "which" to search the path
SHELL "which " & appname & " > " & tempfile
IF NOT isfile(tempfile) THEN debug "find_helper_app(" & appname & ") failed" : RETURN ""
fh = FREEFILE
OPEN tempfile FOR INPUT AS #fh
LINE INPUT #fh, s
CLOSE #fh
KILL tempfile
s = TRIM(s)
RETURN s
#ELSE
'Then look in the support subdirectory
IF isfile(exepath & "\support\" & appname & ".exe") THEN RETURN exepath & "\support\" & appname & ".exe"
RETURN ""
#ENDIF
END FUNCTION

'Not used
FUNCTION can_convert_mp3 () AS INTEGER
 IF find_madplay() = "" THEN RETURN 0
 RETURN can_convert_wav()
END FUNCTION

'Not used
FUNCTION can_convert_wav () AS INTEGER
 IF find_oggenc() = "" THEN RETURN 0
 RETURN -1 
END FUNCTION

'There is way too much stuff in this function, would probably be cleaner to remove it
FUNCTION missing_helper_message (appname as string) as string
 DIM ret as string
 DIM mult as integer = INSTR(appname, " ")

 ret = appname + DOTEXE + iif_string(mult, " are both missing (only one required).", " is missing.")

 #IFDEF __FB_WIN32__
  IF appname = "hspeak" THEN
   'support/hspeak.exe WILL work, but that's not where we package it
   ret += " Check that it is in the same folder as custom.exe."
  ELSE
   ret += " Check that it is in the support folder."
  END IF
 #ELSEIF DEFINED(__FB_DARWIN__)
  ret += " This ought to be included inside OHRRPGCE-Custom! Please report this."
 #ELSE
  ret += " You must install it on your system."
 #ENDIF

 'Linux nightly builds are full distributions, while on Windows they are missing much.
 #IF DEFINED(__FB_WIN32__)
  IF version_branch = "wip" THEN
   ret += CHR(10) + "You are using a nightly build. Did you unzip the nightly on top of a full install of a stable release, as you are meant to?"
   IF INSTR(appname, "oggenc") OR INSTR(appname, "madplay") THEN
    ret += " Alternatively, download oggenc+madplay.zip from the nightly ""alternative backends"" folder."
   END IF
  END IF
 #ENDIF
 RETURN ret
END FUNCTION

'Returns error message, or "" on success
FUNCTION mp3_to_ogg (in_file AS STRING, out_file AS STRING, quality AS INTEGER = 4) AS STRING
 DIM AS STRING tempwav
 DIM AS STRING ret
 tempwav = tmpdir & "temp." & INT(RND * 100000) & ".wav"
 ret = mp3_to_wav(in_file, tempwav)
 IF LEN(ret) THEN RETURN ret
 ret = wav_to_ogg(tempwav, out_file, quality)
 safekill tempwav
 RETURN ret
END FUNCTION

'Returns error message, or "" on success
FUNCTION mp3_to_wav (in_file AS STRING, out_file AS STRING) AS STRING
 DIM AS STRING app, args, ret
 IF NOT isfile(in_file) THEN RETURN "mp3 to wav conversion: " & in_file & " does not exist"
 app = find_madplay()
 IF app = "" THEN RETURN "Can not read MP3 files: " + missing_helper_message("madplay" + DOTEXE)

 args = " -o wave:""" & out_file & """ """ & in_file & """"
 ret = spawn_and_wait(app, args)
 IF LEN(ret) THEN
  safekill out_file
  RETURN ret
 END IF

 IF NOT isfile(out_file) THEN RETURN "Could not find " + out_file + ": " + app + " must have failed"
 RETURN ""
END FUNCTION

'Returns error message, or "" on success
FUNCTION wav_to_ogg (in_file AS STRING, out_file AS STRING, quality AS INTEGER = 4) AS STRING
 DIM AS STRING app, args, ret
 IF NOT isfile(in_file) THEN RETURN "wav to ogg conversion: " & in_file & " does not exist"
 app = find_oggenc()
 IF app = "" THEN RETURN "Can not convert to OGG: " + missing_helper_message("oggenc" DOTEXE " and oggenc2" DOTEXE)

 args = " -q " & quality & " -o """ & out_file & """ """ & in_file & """"
 ret = spawn_and_wait(app, args)
 IF LEN(ret) THEN
  safekill out_file
  RETURN "wav to ogg conversion failed."
 END IF

 IF NOT isfile(out_file) THEN RETURN "Could not find " + out_file + ": " + app + " must have failed"
 RETURN ""
END FUNCTION

SUB upgrade_message (s AS STRING)
 IF NOT upgrademessages THEN
  upgrademessages = -1
  reset_console 20, vpages(vpage)->h - 20, uilook(uiBackground)
  show_message("Auto-Updating obsolete RPG file")
 END IF
 DIM temptime AS DOUBLE
 IF time_rpg_upgrade THEN
  temptime = TIMER
  upgrade_overhead_time -= temptime
  IF last_upgrade_time <> 0.0 THEN
   debuginfo "...done in " & FORMAT((temptime - last_upgrade_time) * 1000, ".#") & "ms"
  END IF
 END IF
 debuginfo "rpgfix:" & s
 show_message(s)
 IF time_rpg_upgrade THEN
  temptime = TIMER
  last_upgrade_time = temptime
  upgrade_overhead_time += temptime
 END IF
END SUB

'admittedly, these 'console' functions suck
SUB reset_console (top AS INTEGER = 0, h AS INTEGER = 200, c AS INTEGER = 0)
 WITH console
  .top = top
  .h = h
  .x = 0
  .y = top
  .c = c
  DIM tempfr as Frame ptr
  tempfr = frame_new_view(vpages(vpage), 0, .top, vpages(vpage)->w, .h)
  frame_clear tempfr, c
  frame_unload @tempfr
 END WITH
END SUB

SUB show_message (s AS STRING)
 WITH console
  IF .x > 0 THEN .x = 0 : .y += 8
  append_message s
 END WITH
END SUB

SUB append_message (s AS STRING)
 DIM AS INTEGER display = YES
 IF RIGHT(TRIM(s), 1) = "," THEN display = NO
 WITH console
  IF .x > 0 AND LEN(s) * 8 + .x > vpages(vpage)->w THEN .x = 0 : .y += 8: display = YES
  IF .y >= .top + .h - 8 THEN
   'scroll page up 2 lines
   DIM as Frame ptr tempfr, copied
   tempfr = frame_new_view(vpages(vpage), 0, .top + 16, vpages(vpage)->w, .h - 16)
   copied = frame_duplicate(tempfr)
   frame_clear tempfr, .c
   frame_draw copied, , 0, .top, , NO, vpage
   .y -= 16
   frame_unload @copied
   frame_unload @tempfr
  END IF
  printstr s, .x, .y, vpage
  .x += LEN(s) * 8
  IF display THEN setvispage vpage
 END WITH
END SUB

SUB animatetilesets (tilesets() AS TilesetData ptr)
 FOR i AS INTEGER = 0 TO UBOUND(tilesets)
  'Animate each tileset...
  FOR j AS INTEGER = 0 TO i - 1
   '--unless of course we already animated it for this frame
   '  because the same tileset can be used on more than one layer...
   IF tilesets(i) = tilesets(j) THEN CONTINUE FOR, FOR
  NEXT
  cycletile tilesets(i)->anim(), tilesets(i)->tastuf()
 NEXT
END SUB

SUB cycletile (tanim_state() AS TileAnimState, tastuf() AS INTEGER)
 DIM i AS INTEGER
 DIM notstuck AS INTEGER
 FOR i = 0 TO 1
#IFDEF IS_GAME
  IF istag(tastuf(1 + 20 * i), 0) THEN CONTINUE FOR
#ENDIF
  WITH tanim_state(i)
   .skip = large(.skip - 1, 0)
   IF .skip = 0 THEN
    notstuck = 10
    DO
     SELECT CASE tastuf(2 + 20 * i + .pt)
      CASE 0
       IF .pt <> 0 THEN .cycle = 0  'this is done for the tile animation plotscript commands
       .pt = 0
      CASE 1
       .cycle = .cycle - tastuf(11 + 20 * i + .pt) * 16
       .pt = loopvar(.pt, 0, 8, 1)
      CASE 2
       .cycle = .cycle + tastuf(11 + 20 * i + .pt) * 16
       .pt = loopvar(.pt, 0, 8, 1)
      CASE 3
       .cycle = .cycle + tastuf(11 + 20 * i + .pt)
       .pt = loopvar(.pt, 0, 8, 1)
      CASE 4
       .cycle = .cycle - tastuf(11 + 20 * i + .pt)
       .pt = loopvar(.pt, 0, 8, 1)
      CASE 5
       .skip = tastuf(11 + 20 * i + .pt)
       .pt = loopvar(.pt, 0, 8, 1)
#IFDEF IS_GAME
      CASE 6
       IF istag(tastuf(11 + 20 * i + .pt), 0) THEN
        .pt = loopvar(.pt, 0, 8, 1)
       ELSE
        .pt = 0
        .cycle = 0
       END IF
#ENDIF
      CASE ELSE
       .pt = loopvar(.pt, 0, 8, 1)
     END SELECT
     notstuck = large(notstuck - 1, 0)
    LOOP WHILE notstuck AND .skip = 0
   END IF
  END WITH
 NEXT i
END SUB

FUNCTION finddatafile(filename AS STRING) AS STRING
'Current dir
IF isfile(filename) THEN RETURN filename
'platform-specific relative data files path (Mac OS X bundles)
IF isfile(data_dir & SLASH & filename) THEN RETURN data_dir & SLASH & filename
'same folder as executable
IF isfile(exepath & SLASH & filename) THEN RETURN exepath & SLASH & filename
#IFDEF __UNIX__
'~/.ohrrpgce/
IF isfile(tmpdir & SLASH & filename) THEN RETURN tmpdir & SLASH & filename
#IFDEF DATAFILES
IF isfile(DATAFILES & SLASH & filename) THEN RETURN DATAFILES & SLASH & filename
#ENDIF
#ENDIF
RETURN ""
END FUNCTION

FUNCTION finddatadir(dirname AS STRING) AS STRING
'Current dir
IF isdir(dirname) THEN RETURN dirname
'platform-specific relative data files path (Mac OS X bundles)
IF isdir(data_dir & SLASH & dirname) THEN RETURN data_dir & SLASH & dirname
'same folder as executable
IF isdir(exepath & SLASH & dirname) THEN RETURN exepath & SLASH & dirname
#IFDEF __UNIX__
'~/.ohrrpgce/
IF isdir(tmpdir & SLASH & dirname) THEN RETURN tmpdir & SLASH & dirname
#IFDEF DATAFILES
IF isdir(DATAFILES & SLASH & dirname) THEN RETURN DATAFILES & SLASH & dirname
#ENDIF
#ENDIF
RETURN ""
END FUNCTION

SUB updaterecordlength (lumpf AS STRING, BYVAL bindex AS INTEGER, BYVAL headersize AS INTEGER = 0, BYVAL repeating AS INTEGER = NO)
'If the length of records in this lump has changed (increased) according to binsize.bin, stretch it, padding records with zeroes.
'Note: does not create a lump if it doesn't exist.
'Pass 'repeating' as true when more than one lump with this bindex exists.
''headersize' is the number of bytes before the first record.

IF getbinsize(bindex) < curbinsize(bindex) THEN

 DIM oldsize AS INTEGER = getbinsize(bindex)
 DIM newsize AS INTEGER = curbinsize(bindex)

 upgrade_message trimpath(lumpf) & " record size = " & newsize

 'Only bother to do this for records of nonzero size (this implies the file doesn't exist, right?)
 IF oldsize > 0 ANDALSO isfile(lumpf) THEN

  DIM tempf AS STRING = lumpf & ".resize.tmp"

  'This tends to break (it's a C/unix system call), hence all the paranoia
  IF rename(lumpf, tempf) THEN
   DIM err_string AS STRING = *get_sys_err_string()  'errno would get overwritten while building the error message
   fatalerror "Impossible to upgrade game: Could not rename " & lumpf & " to " & tempf & " (exists=" & isfile(tempf) & ") Reason: " & err_string
  END IF

  DIM inputf AS INTEGER = FREEFILE
  OPEN tempf FOR BINARY AS inputf
  DIM outputf AS INTEGER = FREEFILE
  OPEN lumpf FOR BINARY AS outputf

  DIM records AS INTEGER = (LOF(inputf) - headersize) \ oldsize

  IF headersize > 0 THEN
   DIM headerbuf(headersize - 1) AS BYTE
   GET #inputf, , headerbuf()
   PUT #outputf, , headerbuf()
  END IF

  DIM buf(newsize \ 2 - 1) AS INTEGER
  FOR i AS INTEGER = 0 TO records - 1
   loadrecord buf(), inputf, oldsize \ 2
   storerecord buf(), outputf, newsize \ 2
  NEXT

  CLOSE inputf
  CLOSE outputf
  KILL tempf
 END IF

 'If we are repeating, we need to keep the old binsize intact
 IF repeating = NO THEN setbinsize bindex, newsize

END IF
END SUB

'Clamp a value to within a range with warning
SUB clamp_value (BYREF value as integer, BYVAL min as integer, BYVAL max as integer, argname as string)
 DIM oldval as integer = value
 IF value < min THEN value = min
 IF value > max THEN value = max
 IF value <> oldval THEN debug "Clamped invalid " + argname + " value " & oldval & " to " & value
END SUB

FUNCTION passwordhash (p as string) as ushort
 'Just a simple stupid 9-bit hash.
 'The idea is just to make the password unretrieveable, without using a cryptographic hash.
 IF p = "" THEN RETURN 0
 DIM hash as ushort
 FOR i as integer = 0 TO LEN(p) - 1
  hash = hash * 3 + p[i] * 31
 NEXT
 RETURN (hash AND 511) OR 512  'Never return 0
END FUNCTION

'If someone forgets their password, call this function to generate a new one
FUNCTION generatepassword(hash as integer) as string
 IF hash = 0 THEN RETURN ""
 IF hash - 512 < 0 OR hash - 512 > 511 THEN RETURN "<invalid password hash " & hash & ">"
 DO
  DIM p as string = ""
  FOR i as integer = 0 TO 3
   p += CHR(ASC("a") + RND * 25)
  NEXT
  IF passwordhash(p) = hash THEN RETURN p
 LOOP
END FUNCTION

SUB writepassword (pass AS STRING)
 gen(genPassVersion) = 257
 gen(genPW4Hash) = passwordhash(pass)

 '--Provide limited back-compat for PW3 (not possible to open a passworded
 '--file with an older version of Custom even if you know the password)
 DIM dummypw as string
 IF pass = "" THEN
  '--Write empty 3rd-style password
  dummypw = STRING(17, 0)
 ELSE
  '--Write unguessable garbage 3rd-style password
  FOR i AS INTEGER = 1 TO 17
   dummypw += CHR(CINT(RND * 254))
  NEXT i
 END IF
 gen(genPW3Rot) = 0
 str2array dummypw, gen(), 14
END SUB

'Read old-old-old password (very similar to PW3)
FUNCTION read_PW1_password () as string
 DIM rpas as string
 FOR i as integer = 1 TO gen(genPW1Length)
  IF gen(4 + i) >= 0 AND gen(4 + i) <= 255 THEN rpas = rpas + CHR(loopvar(gen(4 + i), 0, 255, gen(genPW1Offset) * -1))
 NEXT i
 RETURN rpas
END FUNCTION

'Read old-old scattertable password format
FUNCTION read_PW2_password () as string
 DIM stray(10) as integer
 DIM pass as string = STRING(20, "!")

 FOR i AS INTEGER = 0 TO gen(genPW2Length)
  setbit stray(), 0, i, readbit(gen(), 200 - 1, gen(200 + i))
 NEXT i

 array2str stray(), 0, pass
 pass = LEFT(pass, INT((gen(genPW2Length) + 1) / 8))

 RETURN rotascii(pass, gen(genPW2Offset) * -1)
END FUNCTION

FUNCTION read_PW3_password () as string
 '--read a 17-byte string from GEN at word offset 7
 '--(Note that array2str uses the byte offset not the word offset)
 DIM pass AS STRING
 pass = STRING(17, 0)
 array2str gen(), 14, pass

 '--reverse ascii rotation / weak obfuscation
 pass = rotascii(pass, gen(genPW3Rot) * -1)

 '-- discard ascii chars lower than 32
 DIM pass2 AS STRING = ""
 FOR i AS INTEGER = 1 TO 17
  DIM c AS STRING = MID(pass, i, 1)
  IF ASC(c) >= 32 THEN pass2 += c
 NEXT i

 RETURN pass2
END FUNCTION

'Return true if it passes.
'Supports all password formats, because this is called before upgrade
FUNCTION checkpassword (pass as string) as integer
 IF gen(genPassVersion) > 257 THEN
  'Please let this never happen
  RETURN NO
 ELSEIF gen(genPassVersion) = 257 THEN
  RETURN (passwordhash(pass) = gen(genPW4Hash))
 ELSEIF gen(genPassVersion) = 256 THEN
  '--new format password
  RETURN (pass = read_PW3_password)
 ELSEIF gen(genVersion) >= 3 THEN
  '--old scattertable format
  RETURN (pass = read_PW2_password)
 ELSE
  '--ancient format
  RETURN (pass = read_PW1_password)
 END IF
END FUNCTION

'Used for forgotten password retrieval. Move along.
FUNCTION getpassword () as string
 IF gen(genPassVersion) = 257 THEN
  RETURN "Random password: " & generatepassword(gen(genPW4Hash))
 ELSEIF gen(genPassVersion) = 256 THEN
  RETURN read_PW3_password
 ELSEIF gen(genVersion) >= 3 THEN
  RETURN read_PW2_password
 ELSE
  RETURN read_PW1_password
 END IF 
END FUNCTION

SUB upgrade (font())
DIM pal16(8)
DIM AS INTEGER i, j, o, p, y
DIM temp AS INTEGER
DIM fh AS INTEGER

upgrademessages = 0
last_upgrade_time = 0.0
upgrade_start_time = TIMER
upgrade_overhead_time = 0.0

'Custom and Game should both have provided a writeable workingdir. Double check.
'(This is partially in vain, as we could crash if any of the lumps are unwriteable)
IF NOT diriswriteable(workingdir) THEN fatalerror "Upgrade failure: " + workingdir + " not writeable"

IF full_upgrade THEN
 debuginfo "Full game data upgrade..."
ELSE
 debuginfo "Partial game data upgrade..."
END IF

IF getfixbit(fixNumElements) = 0 THEN
 setfixbit(fixNumElements, 1)
 'This has to be set before we start loading and saving things
 gen(genNumElements) = 16
END IF

IF gen(genNumElements) < 1 THEN
 upgrade_message "genNumElements was " & gen(genNumElements) & ", fixing"
 gen(genNumElements) = 1
END IF

IF gen(genVersion) = 0 THEN
 upgrade_message "Ancient Pre-1999 format (1)"
 gen(genVersion) = 1
 upgrade_message "Flushing New Text Data..."
 DIM box AS TextBox
 FOR o = 0 TO 999
  LoadTextBox box, o
  'Zero out the data members that contained random garbage before 1999
  WITH box
   .money_tag      = 0
   .money          = 0
   .door_tag       = 0
   .door           = 0
   .item_tag       = 0
   .item           = 0
   .choice_enabled = NO
   .no_box         = NO
   .opaque         = NO
   .restore_music  = NO
   .choice(0)      = ""
   .choice_tag(0)  = 0
   .choice(1)      = ""
   .choice_tag(1)  = 0
   .menu_tag       = 0
   .vertical_offset = 0
   .shrink         = 0
   .textcolor      = 0
   .boxstyle       = 0
   .backdrop       = 0
   .music          = 0
   .menu           = 0
  END WITH
  SaveTextBox box, o
 NEXT o
END IF
IF gen(genVersion) = 1 THEN
 upgrade_message "June 18 1999 format (2)"
 gen(genVersion) = 2
 upgrade_message "Updating Door Format..."
 FOR o = 0 TO 19
  IF isfile(game + ".dor") THEN xbload game + ".dor", buffer(), "No doors"
  FOR i = 0 TO 299
   buffer(i) = buffer(o * 300 + i)
  NEXT i
  setpicstuf buffer(), 600, -1
  storeset game + ".dox", o, 0
 NEXT o
 upgrade_message "Enforcing default font"
 getdefaultfont font()
 xbsave game + ".fnt", font(), 2048
 upgrade_message "rpgfix:Making AniMaptiles Backward Compatible"
 FOR i = 0 TO 39
  buffer(i) = 0
 NEXT i
 FOR i = 0 TO 1
  o = i * 20
  buffer(0 + o) = 112
  buffer(1 + o) = 0
  '--wait 3--
  buffer(2 + o + 0) = 5
  buffer(11 + o + 0) = 3
  '--right 1--
  buffer(2 + o + 1) = 3
  buffer(11 + o + 1) = 1
  '--wait 3--
  buffer(2 + o + 2) = 5
  buffer(11 + o + 2) = 3
  '--left 1--
  buffer(2 + o + 3) = 4
  buffer(11 + o + 3) = 1
 NEXT i
 FOR i = 0 TO 14
  savetanim i, buffer()
 NEXT i
 DIM tx AS INTEGER, ty AS INTEGER
 DIM tmap AS TileMap
 FOR i = 0 TO gen(genMaxMap)
  upgrade_message " map " & i
  loadtilemap tmap, maplumpname(i, "t")
  FOR tx = 0 TO tmap.wide - 1
   FOR ty = 0 TO tmap.high - 1
    IF readblock(tmap, tx, ty) = 158 THEN writeblock tmap, tx, ty, 206
   NEXT ty
  NEXT tx
  savetilemap tmap, maplumpname(i, "t")
 NEXT i
 unloadtilemap tmap
END IF
'---VERSION 3---
IF gen(genVersion) = 2 THEN
 upgrade_message "July 8 1999 format (3)"
 gen(genVersion) = 3
 writepassword read_PW1_password
 'No need to remove the old password: we just overwrote it with
 'a back-compat PW3 blank/garbage password

 upgrade_message "Put record count defaults in GEN..."
 gen(genMaxHeroPic)   = 40
 gen(genMaxEnemy1Pic) = 149
 gen(genMaxEnemy2Pic) = 79
 gen(genMaxEnemy3Pic) = 29
 gen(genMaxNPCPic)    = 119
 gen(genMaxWeaponPic) = 149
 gen(genMaxAttackPic) = 99
 gen(genMaxTile)      = 14
 gen(genMaxAttack)    = 200
 gen(genMaxHero)      = 59
 gen(genMaxEnemy)     = 500
 gen(genMaxFormation) = 1000
 gen(genMaxPal)       = 99
 gen(genMaxTextbox)   = 999
END IF
'--VERSION 4--
IF gen(genVersion) = 3 THEN
 upgrade_message "Sept 15 2000 format (4)"
 gen(genVersion) = 4
 upgrade_message "Clearing New Attack Bitsets..."
 setpicstuf buffer(), 80, -1
 FOR o = 0 TO gen(genMaxAttack)
  loadoldattackdata buffer(), o
  buffer(18) = 0
  IF readbit(buffer(), 20, 60) THEN buffer(18) = 1
  setbit buffer(), 20, 2, 0
  FOR i = 21 TO 58
   setbit buffer(), 20, i, 0
  NEXT i
  FOR i = 60 TO 63
   setbit buffer(), 20, i, 0
  NEXT i
  saveoldattackdata buffer(), o
 NEXT o
 setbit gen(), 101, 6, 0 'no hide readymeter
 setbit gen(), 101, 7, 0 'no hide health meter
END IF
'--VERSION 5--
IF gen(genVersion) = 4 THEN
 upgrade_message "March 31 2001 format (5)"
 gen(genVersion) = 5
 upgrade_message "Upgrading 16-color Palette Format..."
 setpicstuf pal16(), 16, -1
 xbload game + ".pal", buffer(), "16-color palettes missing from " + sourcerpg
 KILL game + ".pal"
 '--find last used palette
 DIM last AS INTEGER = 99
 DIM foundpal AS INTEGER = 0
 FOR j = 99 TO 0 STEP -1
  FOR i = 0 TO 7
   IF buffer(j * 8 + i) <> 0 THEN
    last = j
    foundpal = 1
    EXIT FOR
   END IF
  NEXT i
  IF foundpal THEN EXIT FOR
 NEXT j
 upgrade_message "Last used palette is " & last
 '--write header
 pal16(0) = 4444
 pal16(1) = last
 FOR i = 2 TO 7
  pal16(i) = 0
 NEXT i
 storeset game + ".pal", 0, 0
 '--convert palettes
 FOR j = 0 TO last
  FOR i = 0 TO 7
   pal16(i) = buffer(j * 8 + i)
  NEXT i
  storeset game + ".pal", 1 + j, 0
 NEXT j
END IF
'--VERSION 6--
IF gen(genVersion) = 5 THEN
 upgrade_message "Serendipity format (6)"
 'Shop stuff and song name formats changed, MIDI music added
 'Sub version info also added
 'Clear battle formation animation data
 FOR i = 0 TO gen(genMaxFormation)
  setpicstuf buffer(), 80, -1
  loadset game + ".for", i, 0
  buffer(34) = 0
  buffer(35) = 0
  storeset game + ".for", i, 0
 NEXT i
 gen(genVersion) = 6
END IF
'--VERSION 7 and up!
' It is a good idea to increment this number each time a major feature
' has been added, if opening a new game in an old editor would cause data-loss
' Don't be afraid to increment this. Backcompat warnings are a good thing!
IF gen(genVersion) < CURRENT_RPG_VERSION THEN
 upgrade_message "Bumping RPG format version number from " & gen(genVersion) & " to " & CURRENT_RPG_VERSION
 gen(genVersion) = CURRENT_RPG_VERSION '--update me in const.bi
END IF

IF NOT isfile(workingdir + SLASH + "archinym.lmp") THEN
 upgrade_message "generate default archinym.lmp"
 '--create archinym information lump
 fh = FREEFILE
 OPEN workingdir + SLASH + "archinym.lmp" FOR OUTPUT AS #fh
 PRINT #fh, RIGHT(game, LEN(game) - LEN(workingdir + SLASH))
 PRINT #fh, version + "(previous)"
 CLOSE #fh
END IF

'This is corruption recovery, not upgrade, but Custom has always done this
IF NOT isfile(game + ".fnt") THEN
 debug game + ".fnt missing (which should never happen)"
 getdefaultfont font()
 xbsave game + ".fnt", font(), 2048
END IF

IF NOT isfile(game + ".veh") THEN
 upgrade_message "add vehicle data"
 '--make sure vehicle lump is present
 DIM templatefile AS STRING = finddatafile("ohrrpgce.new")
 IF templatefile <> "" THEN
  unlumpfile(templatefile, "ohrrpgce.veh", tmpdir)
  'Recall it's best to avoid moving files across filesystems
  filecopy tmpdir & SLASH & "ohrrpgce.veh", game & ".veh"
  safekill tmpdir & SLASH & "ohrrpgce.veh"
  gen(genMaxVehicle) = 2
 END IF
END IF

'--make sure binsize.bin is full. why are we doing this? Otherwise as lumps are upgraded
'--and binsize.bin is extended, records in binsize which are meant to default
'--because they don't exist would become undefined instead
FOR i = 0 TO sizebinsize
 setbinsize i, getbinsize(i)
NEXT

IF NOT isfile(workingdir + SLASH + "attack.bin") THEN
 upgrade_message "Init extended attack data..."
 setbinsize binATTACK, curbinsize(binATTACK)
 flusharray buffer(), dimbinsize(binAttack), 0
 FOR i = 0 TO gen(genMaxAttack)
  savenewattackdata buffer(), i
 NEXT i

 '--and while we are at it, clear the old death-string from enemies
 upgrade_message "Re-init recycled enemy data..."
 FOR i = 0 TO gen(genMaxEnemy)
  loadenemydata buffer(), i
  FOR j = 17 TO 52
   buffer(j) = 0
  NEXT j
  saveenemydata buffer(), i
 NEXT i
END IF

IF NOT isfile(workingdir + SLASH + "songdata.bin") THEN
 upgrade_message "Upgrading Song Name format..."
 DIM song(99) AS STRING
 fh = FREEFILE
 OPEN game + ".sng" FOR BINARY AS #fh
 temp = LOF(fh)
 CLOSE #fh
 IF temp > 0 THEN
  fh = FREEFILE
  OPEN game + ".sng" FOR INPUT AS #fh
  FOR i = 0 TO 99
   INPUT #fh, song(i)
  NEXT i
  CLOSE #fh
 END IF

 FOR i = 99 TO 1 STEP -1
  '-- check for midis as well 'cause some people might use a WIP custom or whatnot
  IF song(i) <> "" OR isfile(game + "." + STR(i)) OR isfile(workingdir + SLASH + "song" + STR(i) + ".mid") THEN
   gen(genMaxSong) = i
   EXIT FOR
  END IF
 NEXT

 setbinsize binSONGDATA, curbinsize(binSONGDATA)
 flusharray buffer(), dimbinsize(binSONGDATA), 0
 setpicstuf buffer(), curbinsize(binSONGDATA), -1
 FOR i = 0 TO gen(genMaxSong)
  writebinstring song(i), buffer(), 0, 30
  storeset workingdir + SLASH + "songdata.bin", i, 0
 NEXT
 ERASE song
END IF

'Safety-check for negative gen(genMasterPal) because of one known game that somehow had -2
gen(genMasterPal) = large(0, gen(genMasterPal))

IF NOT isfile(workingdir + SLASH + "palettes.bin") THEN
 upgrade_message "Upgrading Master Palette format..."
 IF NOT isfile(game + ".mas") THEN
  debug "Warning: " & game & ".mas does not exist (which should never happen)"
  load_default_master_palette master()
 ELSE
  loadpalette master(), 0  'Loads from .mas
 END IF
 savepalette master(), 0  'Saves to palettes.bin
END IF
'This is not necessary in the slightest, but we copy the default master palette
'back to the .MAS lump, to give old graphics utilities some chance of working
unconvertpalette()

IF gen(genHeroWeakHP) = 0 THEN
 gen(genHeroWeakHP) = 20
END IF

IF gen(genEnemyWeakHP) = 0 THEN
 gen(genEnemyWeakHP) = 20
END IF

'--If no stf lump exists, create an empty one.
IF NOT isfile(game + ".stf") THEN touchfile game + ".stf"

'--check variable record size lumps and reoutput them if records have been extended
'--all of the files below should exist, be non zero length and have non zero record size by this point
updaterecordlength workingdir + SLASH + "attack.bin", binATTACK
updaterecordlength game + ".stf", binSTF
updaterecordlength workingdir + SLASH + "songdata.bin", binSONGDATA
updaterecordlength workingdir + SLASH + "sfxdata.bin", binSFXDATA
updaterecordlength game + ".map", binMAP
updaterecordlength workingdir + SLASH + "menus.bin", binMENUS
updaterecordlength workingdir + SLASH + "menuitem.bin", binMENUITEM
IF NOT isfile(workingdir + SLASH + "menuitem.bin") THEN
 upgrade_message "Creating default menu file..."
 DIM menu_set AS MenuSet
 menu_set.menufile = workingdir + SLASH + "menus.bin"
 menu_set.itemfile = workingdir + SLASH + "menuitem.bin"
 DIM menu AS MenuDef
 create_default_menu menu
 SaveMenuData menu_set, menu, 0
 ClearMenuData menu
END IF
updaterecordlength game & ".say", binSAY
updaterecordlength game & ".dt0", binDT0
updaterecordlength game & ".dt1", binDT1
updaterecordlength game & ".itm", binITM
'Don't update .N binsize until all records have been stretched
FOR i = 0 TO gen(genMaxMap)
 updaterecordlength maplumpname(i, "n"), binN, 7, YES
NEXT
setbinsize binN, curbinsize(binN)

'If you want to add more colours to uicolors.bin, you'll want to record what the old
'record length was (one record per palette), then updaterecordlength, then fill in
'the new colours, which start zeroed out.
'However, if uicolors.bin is completely empty/missing, then just let the block below
'initialise the lump.
updaterecordlength workingdir + SLASH + "uicolors.bin", binUICOLORS

'--give each palette a default ui color set
DIM uirecords AS INTEGER = FILELEN(workingdir + SLASH + "uicolors.bin") \ getbinsize(binUICOLORS)
IF uirecords < gen(genMaxMasterPal) + 1 THEN
 upgrade_message "Adding default UI colors..."
 DIM defaultcols(uiColors)
 OldDefaultUIColors defaultcols()
 FOR i = uirecords TO gen(genMaxMasterPal)
  SaveUIColors defaultcols(), i
 NEXT
END IF

IF gen(genPassVersion) = 256 THEN
 '--Update PW3 to PW4
 upgrade_message "Updating PW3 password storage format"
 writepassword read_PW3_password
ELSEIF gen(genPassVersion) < 256 THEN
 '--At this point we know the password format is PW2 (not PW1), scattertable
 upgrade_message "Updating PW2 password storage format"
 writepassword read_PW2_password

 '--Zero out PW2 scatter table
 FOR i as integer = 199 TO 359
  gen(i) = 0
 NEXT
END IF

'Zero out new attack item cost (ammunition) data
IF getfixbit(fixAttackitems) = 0 THEN
  upgrade_message "Zero new ammunition data..."
  setfixbit(fixAttackitems, 1)
  fh = freefile
  OPEN workingdir + SLASH + "attack.bin" FOR BINARY AS #FH
  REDIM dat(curbinsize(binATTACK)/2 - 1) AS SHORT
  p = 1
  FOR i = 0 to gen(genMaxAttack)

    GET #fh,p,dat()
    FOR y = 53 TO 59
      dat(y) = 0
    NEXT

    PUT #fh,p,dat()
    p+=curbinsize(binATTACK)
  NEXT
  CLOSE #fh
END IF

IF getfixbit(fixWeapPoints) = 0 THEN
 upgrade_message "Reset hero hand points..."
 DO
  setfixbit(fixWeapPoints, 1)
  fh = freefile
  OPEN game + ".dt0" FOR BINARY AS #fh
  REDIM dat(dimbinsize(binDT0)) AS SHORT
  FOR i = 0 to gen(genMaxHero)
   GET #fh,,dat()
   IF dat(297) <> 0 OR dat(298) <> 0 OR dat(299) <> 0 OR dat(300) <> 0 THEN
    close #fh
    EXIT DO 'they already use hand points, abort!
   END IF
  NEXT
  
  p = 1
  DIM recsize as integer = getbinsize(binDT0)
  FOR i = 0 to gen(genMaxHero)
   GET #fh, p, dat()
   dat(297) = 24
   dat(299) = -20
   PUT #fh, p, dat()
   p += recsize
  NEXT
  close #fh
  EXIT DO
 LOOP
END IF

'Upgrade attack data
DIM fix_stun as integer = (getfixbit(fixStunCancelTarg) = 0)
DIM fix_dam_mp as integer = (getfixbit(fixRemoveDamageMP) = 0) AND full_upgrade
DIM fix_elem_fails as integer = (getfixbit(fixAttackElementFails) = 0) AND full_upgrade
IF fix_stun OR fix_dam_mp OR fix_elem_fails THEN
 IF fix_stun THEN
  upgrade_message "Target disabling old stun attacks..."
  setfixbit(fixStunCancelTarg, 1)
 END IF
 IF fix_dam_mp THEN
  upgrade_message "Remove obsolete 'Damage MP' bit..."
  setfixbit(fixRemoveDamageMP, 1)
 END IF
 IF fix_elem_fails THEN
  upgrade_message "Initialised attack elemental failure conditions..."
  setfixbit(fixAttackElementFails, 1)
 END IF
 REDIM dat(40 + dimbinsize(binATTACK)) AS INTEGER
 DIM cond AS AttackElementCondition
 FOR i = 0 to gen(genMaxAttack)
  DIM saveattack as integer = NO
  loadattackdata dat(), i

  IF fix_stun AND dat(18) = 14 THEN '--Target stat is stun register
   IF readbit(dat(), 20, 0) THEN GOTO skipfix '--cure instead of harm
   IF dat(5) = 5 OR dat(5) = 6 THEN '--set to percentage
    IF dat(11) >= 0 THEN GOTO skipfix '-- set to >= 100%
   END IF
   'Turn on the disable target attack bit
   setbit dat(), 65, 12, YES
   saveattack = YES
  END IF
  skipfix:

  IF fix_dam_mp THEN
   IF readbit(dat(), 20, 60) THEN '--Damage MP
    setbit dat(), 20, 60, NO
    saveattack = YES
    IF dat(18) = statHP THEN dat(18) = statMP
   END IF
  END IF

  IF fix_elem_fails THEN
   FOR j = 0 TO 63
    loadoldattackelementalfail cond, dat(), j
    SerAttackElementCond cond, dat(), 121 + j * 3
   NEXT
   saveattack = YES
  END IF

  IF saveattack THEN saveattackdata dat(), i
 NEXT
END IF

IF getfixbit(fixDefaultDissolve) = 0 THEN
 upgrade_message "Initializing default enemy fade..."
 setfixbit(fixDefaultDissolve, 1)
 gen(genEnemyDissolve) = 0
END IF

IF getfixbit(fixDefaultDissolveEnemy) = 0 THEN
 upgrade_message "Initializing default enemy fade (per enemy)..."
 setfixbit(fixDefaultDissolveEnemy, 1)
 DIM enemy AS EnemyDef
 FOR i = 0 to gen(genMaxEnemy)
  loadenemydata enemy, i
  enemy.dissolve = 0
  enemy.dissolve_length = 0
  saveenemydata enemy, i
 NEXT
END IF

IF getfixbit(fixPushNPCBugCompat) = 0 THEN
 upgrade_message "Enabling 'Simulate pushable NPC bug' bitset..."
 setfixbit(fixPushNPCBugCompat, 1)
 setbit gen(), genBits2, 0, 1 ' For backcompat
END IF

IF getfixbit(fixDefaultMaxItem) = 0 THEN
 upgrade_message "Store max item number in GEN..."
 setfixbit(fixDefaultMaxItem, 1)
 gen(genMaxItem) = 254
END IF

IF getfixbit(fixBlankDoorLinks) = 0 THEN
 upgrade_message "Disable redundant blank door links..."
 setfixbit(fixBlankDoorLinks, 1)
 DIM doorlink_temp(199) AS DoorLink
 DIM found_first AS INTEGER
 FOR i = 0 TO gen(genMaxMap)
  deserDoorLinks maplumpname(i, "d"), doorlink_temp()
  found_first = NO
  FOR j = 0 TO UBOUND(doorlink_temp)
   WITH doorlink_temp(j)
    IF .source = 0 AND .tag1 = 0 AND .tag2 = 0 THEN
     IF found_first = NO THEN
      'Ignore the first "always" link for door 0
      found_first = YES
     ELSE
      IF .dest = 0 AND .dest_map = 0 THEN
       .source = -1 ' Mark redundant all-zero links as unused
      END IF
     END IF
    END IF
   END WITH
  NEXT j
  serDoorLinks maplumpname(i, "d"), doorlink_temp()
 NEXT i
END IF

IF getfixbit(fixShopSounds) = 0 THEN
 upgrade_message "Set default soundeffects..."
 setfixbit(fixShopSounds, 1)
 gen(genItemLearnSFX) = gen(genAcceptSFX)
 gen(genCantLearnSFX) = gen(genCancelSFX)
 gen(genBuySFX) = gen(genAcceptSFX)
 gen(genHireSFX) = gen(genAcceptSFX)
 gen(genSellSFX) = gen(genAcceptSFX)
 gen(genCantBuySFX) = gen(genCancelSFX)
 gen(genCantSellSFX) = gen(genCancelSFX)
END IF

IF getfixbit(fixExtendedNPCs) = 0 THEN
 upgrade_message "Initialize extended NPC data..."
 setfixbit(fixExtendedNPCs, 1)
 REDIM npctemp(0) AS NPCType 
 FOR i = 0 TO gen(genMaxMap)
  ' These are the garbage data left over from somewhere in the late 90's when
  ' James decided to make the .N lumps big enough to hold 100 NPC definitions
  ' even though there was only enough memory available for 36 NPC sprites at a time
  LoadNPCD maplumpname(i, "n"), npctemp()
  REDIM PRESERVE npctemp(35)
  SaveNPCD maplumpname(i, "n"), npctemp()
 NEXT i
END IF

IF getfixbit(fixHeroPortrait) = 0 OR getfixbit(fixHeroElementals) = 0 THEN
 DIM as integer do_portraits = (getfixbit(fixHeroPortrait) = 0)
 DIM as integer do_elements = (getfixbit(fixHeroElementals) = 0)
 setfixbit(fixHeroPortrait, 1)
 setfixbit(fixHeroElementals, 1)

 DIM as string msgtemp = "Initialize hero "
 IF do_portraits THEN msgtemp += "portraits"
 IF do_portraits AND do_elements THEN msgtemp += " and "
 IF do_elements THEN msgtemp += "elemental resists"
 upgrade_message msgtemp

 DIM her AS HeroDef
 FOR i = 0 TO gen(genMaxHero)
  loadherodata @her, i

  WITH her
   IF do_portraits THEN
    .portrait = -1 'Disable
    .portrait_pal = -1 'Default
   END IF

   IF do_elements THEN
    '.elementals() not initialised, load from old bits
    FOR i as integer = 0 TO small(7, gen(genNumElements) - 1)
     .elementals(i) = backcompat_element_dmg(xreadbit(.bits(), i), xreadbit(.bits(), 8 + i), xreadbit(.bits(), 16 + i))
    NEXT
    'gen(genNumElements) will be more than 8 even in old games after enemytypes are converted to elements
    FOR i as integer = 8 TO gen(genNumElements) - 1
     .elementals(i) = 1
    NEXT
   END IF
  END WITH

  saveherodata @her, i
 NEXT i
END IF

'This fixbit was introduced at the same time as textbox portraits,
'so if it's not on, then the game doesn't use portraits, so it doesn't
'need to be fixed for Game.
IF full_upgrade AND getfixbit(fixTextBoxPortrait) = 0 THEN
 upgrade_message "Initialize text box portrait data..."
 setfixbit(fixTextBoxPortrait, 1)
 'DIM box AS TextBox
 DIM boxbuf(dimbinsize(binSAY)) AS INTEGER
 DIM recsize as integer = getbinsize(binSAY) \ 2
 fh = FREEFILE
 OPEN game & ".say" FOR BINARY ACCESS READ WRITE AS #fh
 FOR i = 0 TO gen(genMaxTextBox)
  'This was stupefying slow, by far the slowest of all upgrades
  'LoadTextBox box, i
  'box.portrait_pal = -1 'Default palette
  'SaveTextBox box, i
  loadrecord boxbuf(), fh, recsize, i
  boxbuf(202) = -1 'Default palette
  storerecord boxbuf(), fh, recsize, i
 NEXT i
 CLOSE #fh
END IF

IF getfixbit(fixInitDamageDisplay) = 0 THEN
 upgrade_message "Initialize damage display time/distance data..."
 setfixbit(fixInitDamageDisplay, 1)
 gen(genDamageDisplayTicks) = 7
 gen(genDamageDisplayRise) = 14
END IF

IF getfixbit(fixDefaultLevelCap) = 0 THEN
 upgrade_message "Set level cap to 99..."
 setfixbit(fixDefaultLevelCap, 1)
 gen(genLevelCap) = 99
END IF

IF getfixbit(fixOldElementalFailBit) = 0 THEN
 upgrade_message "Enabling 'Simulate old fail vs. element resist bit' bitset"
 setfixbit(fixOldElementalFailBit, 1)
 setbit gen(), genBits2, 9, 1
END IF

IF full_upgrade ANDALSO getfixbit(fixEnemyElementals) = 0 THEN
 upgrade_message "Initialising enemy elemental resists..."
 setfixbit(fixEnemyElementals, 1)
 REDIM dat(dimbinsize(binDT1)) AS INTEGER
 FOR i = 0 TO gen(genMaxEnemy)
  loadenemydata dat(), i
  FOR j = 0 TO 63
   SerSingle(dat(), 239 + j*2, loadoldenemyresist(dat(), j))
  NEXT
  saveenemydata dat(), i
 NEXT
END IF

IF full_upgrade ANDALSO getfixbit(fixItemElementals) = 0 THEN
 upgrade_message "Initialising equipment elemental resists..."
 setfixbit(fixItemElementals, 1)
 REDIM dat(dimbinsize(binITM)) AS INTEGER
 FOR i = 0 TO gen(genMaxItem)
  loaditemdata dat(), i
  FOR j = 0 TO 63
   SerSingle(dat(), 82 + j*2, LoadOldItemElemental(dat(), j))
  NEXT
  saveitemdata dat(), i
 NEXT
END IF

'Update record-count for all fixed-length lumps.
IF time_rpg_upgrade THEN upgrade_message "Updating record counts"
FOR i = 0 TO 8
 fix_sprite_record_count i
NEXT i
fix_record_count gen(genMaxTile),     320 * 200, game & ".til", "Tilesets"
fix_record_count gen(genNumBackdrops), 320 * 200, game & ".mxs", "Backdrops", , -1
'FIXME: .dt0 lump is always padded up to 60 records
'fix_record_count gen(genMaxHero),     getbinsize(binDT0), game & ".dt0", "Heroes"
'FIXME: Attack data is split over two lumps. Must handle mismatch
fix_record_count gen(genMaxEnemy),     getbinsize(binDT1), game & ".dt1", "Enemies"
fix_record_count gen(genMaxFormation), 80, game & ".for", "Battle Formations"
fix_record_count gen(genMaxPal),       16, game & ".pal", "16-color Palettes", 16
fix_record_count gen(genMaxTextbox),   getbinsize(binSAY), game & ".say", "Text Boxes"
fix_record_count gen(genMaxVehicle),   80, game & ".veh", "Vehicles"
fix_record_count gen(genMaxTagname),   42, game & ".tmn", "Tag names", -84 'Note: no records for tags 0 and 1, so we handle that with a negative header size.
'FIXME: What is wrong with my menu record sizes?
'fix_record_count gen(genMaxMenu),      getbinsize(binMENUS), workingdir & SLASH & "menus.bin", "Menus"
'fix_record_count gen(genMaxMenuItem),  getbinsize(binMENUITEM), workingdir & SLASH & "menus.bin", "Menu Items"
fix_record_count gen(genMaxItem), getbinsize(binITM), game & ".itm", "Items"
'Warning: don't deduce number of map from length of .MAP or .MN: may be appended with garbage

IF time_rpg_upgrade THEN
 upgrade_message "Upgrades complete."
 debuginfo "Total upgrade time = " & FORMAT(TIMER - upgrade_start_time, ".###") & "s, time wasted on messages = " & FORMAT(upgrade_overhead_time, ".###") & "s"
END IF

IF gen(genErrorLevel) = 0 THEN
 #IFDEF IS_CUSTOM
  IF twochoice("Set script error reporting level to new default, showing all warnings and error messages?", "Yes (Best)", "No (Safest)", 1, 1, "script_error_new_default") = 0 THEN
   gen(genErrorLevel) = 2
  ELSE
   gen(genErrorLevel) = 5
  END IF
 #ELSE
  gen(genErrorLevel) = 5
 #ENDIF
END IF

'Save changes to GEN lump (important when exiting to the title screen and loading a SAV)
xbsave game + ".gen", gen(), 1000

'wow! this is quite a big and ugly routine!
END SUB

SUB fix_record_count(BYREF last_rec_index AS INTEGER, BYREF record_byte_size AS INTEGER, lumpname AS STRING, info AS STRING, skip_header_bytes AS INTEGER=0, count_offset AS INTEGER=0)
 DIM rec_count AS INTEGER = last_rec_index + 1 + count_offset
 IF NOT isfile(lumpname) THEN
  'debug "fix_record_count: " & info & " lump " & trimpath(lumpname) & " does not exist." 
  EXIT SUB
 END IF
 DIM fh AS INTEGER
 fh = FREEFILE
 OPEN lumpname FOR BINARY ACCESS READ AS #fh
 DIM total_bytes AS INTEGER = LOF(fh) - skip_header_bytes
 CLOSE #fh
 IF total_bytes MOD record_byte_size <> 0 THEN
  DIM diffsize AS INTEGER
  diffsize = total_bytes - record_byte_size * rec_count
  DIM mismatch AS STRING
  IF diffsize < 0 THEN
   mismatch = "file short by " & diffsize & " bytes"
  ELSE
   mismatch = "file long by " & diffsize & " bytes"
  END IF
  debug "fix_record_count mismatch for " & info & " lump, " & total_bytes & " is not evenly divisible by " & record_byte_size & " (" & mismatch & ")"
  '--expand the lump to have a valid total size
  fh = FREEFILE
  OPEN lumpname FOR BINARY AS #fh
  DO WHILE total_bytes MOD record_byte_size <> 0
   total_bytes += 1
   PUT #fh, skip_header_bytes + total_bytes, CHR(0)
  LOOP
  CLOSE #fh
  debug "Expanded " & info & " lump to " & total_bytes & " bytes"
 END IF
 DIM records AS INTEGER = total_bytes / record_byte_size
 IF records <> rec_count THEN
  upgrade_message "Adjusting record count for " & info & " lump, " & rec_count & " -> " & records & " (" & records - rec_count & ")"
  last_rec_index = records - 1 - count_offset
 END IF
END SUB

SUB fix_sprite_record_count(BYVAL pt_num AS INTEGER)
 WITH sprite_sizes(pt_num)
  DIM bytes AS INTEGER = .size.x * .size.y * .frames / 2 '--we divide by 2 because there are 2 pixels per byte
  DIM lump AS STRING = game & ".pt" & pt_num
  fix_record_count gen(.genmax), bytes, lump, .name & " sprites"
 END WITH
END SUB

SUB standardmenu (menu() AS STRING, state AS MenuState, x AS INTEGER, y AS INTEGER, page AS INTEGER, edge AS INTEGER=NO, hidecursor AS INTEGER=NO, wide AS INTEGER=999, highlight AS INTEGER=NO, toggle=YES)
 DIM p AS INTEGER
 WITH state
  p = .pt
  IF hidecursor THEN p = .first - 1
  standardmenu menu(), .last, .size, p, .top, x, y, page, edge, wide, highlight, NULL, toggle
 END WITH
END SUB

'Version which allows items to be greyed out/disabled/shaded
SUB standardmenu (menu() AS STRING, state AS MenuState, shaded() AS INTEGER, x AS INTEGER, y AS INTEGER, page AS INTEGER, edge AS INTEGER=NO, hidecursor AS INTEGER=NO, wide AS INTEGER=999, highlight AS INTEGER=NO, toggle=YES)
 DIM p AS INTEGER
 IF LBOUND(shaded) > LBOUND(menu) OR UBOUND(shaded) < UBOUND(menu) THEN fatalerror "standardmenu: shaded() too small"
 WITH state
  p = .pt
  IF hidecursor THEN p = .first - 1
  standardmenu menu(), .last, .size, p, .top, x, y, page, edge, wide, highlight, @shaded(0), toggle
 END WITH
END SUB

SUB standardmenu (menu() AS STRING, size, vis, pt, top, x, y, page, edge=NO, wide=999, highlight=NO, shaded AS INTEGER PTR=NULL, toggle=YES)
'the default for wide is 999 until I know whether it'd break anything to set it to 40
STATIC rememtog
DIM tog AS INTEGER
DIM i AS INTEGER
DIM col AS INTEGER
DIM text AS STRING

IF toggle THEN
 rememtog = rememtog XOR 1
 tog = rememtog
ELSE
 'This menu isn't the active one or we otherwise don't want it to animate
 tog = 0
END IF

FOR i = top TO top + vis
 IF i <= size THEN
  text = menu(i)
  IF pt = i THEN text = RIGHT(text, wide)
  IF pt = i AND highlight THEN
   DIM w AS INTEGER
   w = 8 * LEN(text)
   IF w = 0 THEN w = small(wide * 8, 320)
   rectangle x + 0, y + (i - top) * 8, w, 8, uilook(uiHighlight), page
  END IF
  IF shaded ANDALSO shaded[i] THEN
   col = uilook(uiDisabledItem)
   IF pt = i THEN col = uilook(uiSelectedDisabled + tog)
  ELSE
   col = uilook(uiMenuItem)
   IF pt = i THEN col = uilook(uiSelectedItem + tog)
  END IF
  IF edge THEN
   edgeprint text, x + 0, y + (i - top) * 8, col, page, YES
  ELSE
   textcolor col, 0
   printstr text, x + 0, y + (i - top) * 8, page, YES
  END IF
 END IF
NEXT i

END SUB

SUB future_rpg_warning ()
 'This sub displays forward-compat warnings when a new RPG file is loaded in
 'an old copy of game, or an old version of custom (ypsiliform or newer)

 'future_rpg_warning can get called multiple times per game
 STATIC warned_sourcerpg as string
 IF sourcerpg = warned_sourcerpg THEN EXIT SUB
 warned_sourcerpg = sourcerpg

 debug "Unsupported RPG file!"

 DIM hilite as string = "${K" & uilook(uiText) & "}"
 DIM msg as string = hilite + "Unsupported RPG File ${K-1}"
 msg += !"\n\nThis game has features that are not supported in this version of the OHRRPGCE. Download the latest version at http://HamsterRepublic.com\n"
 msg += "Press any key to continue, but "
 #IFDEF IS_GAME
  msg += "be aware that some things might not work right..."
 #ELSE
  msg += hilite + "DO NOT SAVE the game${K-1}, as this will lead to almost certain data corruption!!"
 #ENDIF
 clearpage 0
 basic_textbox msg, uilook(uiMenuItem), 0
 setvispage 0
 fadein
 waitforanykey
 #IFDEF IS_GAME
'  fadeout 0, 0, 0
 #ENDIF
END SUB

'Check for corruption and unsupported RPG features (maybe someone forgot to update CURRENT_RPG_VERSION)
SUB rpg_sanity_checks
 DIM i as integer

 'Check binsize.bin is not from future
 DIM flen as integer = filelen(workingdir + SLASH + "binsize.bin")
 IF flen > 2 * (sizebinsize + 1) THEN
  debug "binsize.bin length " & flen
  future_rpg_warning
 ELSE
  FOR bindex as integer = 0 TO sizebinsize
   IF curbinsize(bindex) MOD 2 <> 0 THEN
    'curbinsize is INSANE, scream bloody murder to prevent data corruption!
    fatalerror "Oh noes! curbinsize(" & bindex & ")=" & curbinsize(bindex) & " please complain to the devs, who may have just done something stupid!"
   END IF
   DIM binsize as integer = getbinsize(bindex)
   IF binsize > curbinsize(bindex) THEN
    debug "getbinsize(" & bindex & ") = " & binsize & ", but curbinsize = " & curbinsize(bindex)
    future_rpg_warning
   END IF
  NEXT
 END IF

 'Check fixbits.bin is not from future
 DIM maxbits as integer = filelen(workingdir + SLASH + "fixbits.bin") * 8
 FOR i = sizefixbits + 1 TO maxbits - 1
  IF getfixbit(i) THEN
   debug "Unknown fixbit " & i & " set"
   future_rpg_warning
  END IF
 NEXT

 FOR i = 0 TO gen(genMaxMap)
  'Game actually runs just fine when anything except the foemap is missing; Custom
  'has some (crappy) map lump fix code in the map editor
  IF NOT isfile(maplumpname(i, "t")) THEN showerror "map" + filenum(i) + " tilemap is missing!"
  IF NOT isfile(maplumpname(i, "p")) THEN showerror "map" + filenum(i) + " passmap is missing!"
  IF NOT isfile(maplumpname(i, "e")) THEN showerror "map" + filenum(i) + " foemap is missing!"
  IF NOT isfile(maplumpname(i, "l")) THEN showerror "map" + filenum(i) + " NPClocations are missing!"
  IF NOT isfile(maplumpname(i, "n")) THEN showerror "map" + filenum(i) + " NPCdefinitions are missing!"
  IF NOT isfile(maplumpname(i, "d")) THEN showerror "map" + filenum(i) + " doorlinks are missing!"
 NEXT

 'Should this be in upgrade? I can't make up my mind!
 IF gen(genNumElements) > 64 THEN
  future_rpg_warning
  'We would definitely crash if we didn't cap this
  gen(genNumElements) = 64
 END IF
END SUB

SUB draw_menu (menu AS MenuDef, state AS MenuState, page AS INTEGER)
 DIM i AS INTEGER
 DIM elem AS INTEGER
 DIM cap AS STRING
 DIM col AS INTEGER
 DIM where AS XYPair

 'we actually calculate each menu item caption twice: once also in position_menu
 position_menu menu, page
 
 WITH menu.rect
  IF menu.no_box = NO THEN
   edgeboxstyle .x, .y, .wide, .high, menu.boxstyle, page, menu.translucent
  END IF
  IF menu.no_scrollbar = NO THEN
   draw_scrollbar state, menu, page
  END IF
 END WITH

 state.tog = state.tog XOR 1

 FOR i = 0 TO state.size
  elem = state.top + i
  IF elem >= 0 AND elem < menu.numitems THEN
   col = menu.textcolor
   IF col = 0 THEN col = uilook(uiMenuItem)
   IF state.pt = elem AND state.active THEN col = uilook(uiSelectedItem + state.tog)
   WITH *menu.items[elem]
    IF .disabled THEN
     col = uilook(uiDisabledItem)
     IF state.pt = elem AND state.active THEN col = uilook(uiSelectedDisabled + state.tog)
    END IF
    IF NOT (.disabled AND .hide_if_disabled) THEN
     cap = get_menu_item_caption(*menu.items[elem], menu)
     position_menu_item menu, cap, i, where
     IF .t = 1 AND .sub_t = 11 THEN ' volume meter
      edgeboxstyle where.x, where.y, get_music_volume * 48, 10, menu.boxstyle, page, NO, YES
     END IF
     edgeprint cap, where.x, where.y, col, page
    END IF
   END WITH
  END IF
 NEXT i
 
END SUB

SUB position_menu_item (menu AS MenuDef, cap AS STRING, i AS INTEGER, BYREF where AS XYPair)
 DIM bord AS INTEGER
 bord = 8 + menu.bordersize
 WITH menu.rect
  SELECT CASE menu.align
   CASE -1
    where.x = .x + bord
   CASE 0
    where.x = .x + .wide / 2 - LEN(cap) * 4
   CASE 1
    where.x = .x + .wide - bord - LEN(cap) * 8
  END SELECT
  where.y = .y + bord + (i * 10)
 END WITH
END SUB

SUB position_menu (menu AS MenuDef, page AS INTEGER)
 DIM i AS INTEGER
 DIM cap AS STRING
 DIM bord AS INTEGER
 bord = 8 + menu.bordersize

 menu.rect.wide = bord * 2
 menu.rect.high = bord * 2

 FOR i = 0 TO menu.numitems - 1
  WITH *menu.items[i]
   cap = get_menu_item_caption(*menu.items[i], menu)
   menu.rect.wide = large(menu.rect.wide, LEN(cap) * 8 + bord * 2)
   IF .disabled AND .hide_if_disabled THEN CONTINUE FOR 'hidden matter for auto-width but not auto-height
   menu.rect.high = menu.rect.high + 10
  END WITH
 NEXT i
 '--enforce min width
 menu.rect.wide = large(menu.rect.wide, menu.min_chars * 8 + bord * 2)
 '--enforce screen boundaries
 menu.rect.wide = small(menu.rect.wide, vpages(page)->w)
 menu.rect.high = small(menu.rect.high, vpages(page)->h)
 IF menu.maxrows > 0 THEN menu.rect.high = small(menu.rect.high, menu.maxrows * 10 + bord * 2)

 WITH menu
  .rect.x = vpages(page)->w \ 2 - anchor_point(.anchor.x, .rect.wide) + menu.offset.x
  .rect.y = vpages(page)->h \ 2 - anchor_point(.anchor.y, .rect.high) + menu.offset.y
 END WITH
END SUB

FUNCTION anchor_point(anchor AS INTEGER, size AS INTEGER) AS INTEGER
 SELECT CASE anchor
  CASE -1
   RETURN 0
  CASE 0
   RETURN size \ 2
  CASE 1
   RETURN size
 END SELECT
END FUNCTION

'(Re-)initialise menu state, preserving .pt if valid
SUB init_menu_state (BYREF state AS MenuState, menu AS MenuDef)
 WITH state
  .first = 0
  .last = count_menu_items(menu) - 1
  .size = menu.maxrows - 1
  IF .size = -1 THEN .size = 17
  .pt = small(large(.pt, .first), .last)  'explicitly -1 when empty
  IF .pt <> -1 THEN .top = bound(.top, .pt - .size, .pt)
  .top = bound(.top, 0, large(.last - .size, 0))
 END WITH
END SUB

'(Re-)initialise menu state, preserving .pt if valid
'pickenabled: move .pt to an enabled menu item. (Note that .enabled may mean either "greyed out"
'or "unselectable")
SUB init_menu_state (BYREF state AS MenuState, menu() AS SimpleMenu, BYVAL pickenabled AS INTEGER = YES)
 WITH state
  .first = 0
  .last = UBOUND(menu)
  IF .size <= 0 THEN .size = 20
  .pt = bound(.pt, .first, .last)  '.first <= .last
  IF pickenabled THEN
   IF menu(.pt).enabled = NO THEN
    .pt = -1  'explicitly -1 when nothing selectable
    FOR i AS INTEGER = 0 TO UBOUND(menu)
     IF menu(i).enabled THEN .pt = i: EXIT FOR
    NEXT
   END IF
   'Menus with unselectable items have lookahead, which these +1,-1
   'attempt to simulate. Not perfect, but prevents some flickering
   IF .pt <> -1 THEN .top = bound(.top, .pt - .size + 1, .pt - 1)
  END IF
  .top = bound(.top, 0, large(.last - .size, 0))
 END WITH
END SUB

FUNCTION count_menu_items (menu AS MenuDef) as integer
 DIM i AS INTEGER
 DIM count AS INTEGER = 0
 FOR i = 0 TO menu.numitems - 1
  WITH *menu.items[i]
   IF .disabled AND .hide_if_disabled THEN CONTINUE FOR
   count += 1
  END WITH
 NEXT i
 RETURN count
END FUNCTION

SUB loadglobalstrings
'we load the whole lump into memory because readglobalstring can be called
'hunderds of times a second. It's stored in a raw format; good enough.
DIM fh AS INTEGER = FREEFILE
OPEN game + ".stt" FOR BINARY AS #fh
IF LOF(fh) > 0 THEN
 global_strings_buffer = STRING(LOF(fh), 0)
 GET #fh, 1, global_strings_buffer
END IF
CLOSE #fh
END SUB

FUNCTION readglobalstring (index AS INTEGER, default AS STRING, maxlen AS INTEGER=10) as string
IF index * 11 + 2 > LEN(global_strings_buffer) THEN
 RETURN default
ELSE
 DIM namelen AS UBYTE = global_strings_buffer[index * 11]
 IF maxlen < namelen THEN namelen = maxlen
 RETURN MID(global_strings_buffer, index * 11 + 2, namelen)
END IF
END FUNCTION

FUNCTION get_menu_item_caption (mi AS MenuDefItem, menu AS MenuDef) AS STRING
 DIM cap AS STRING
 cap = mi.caption
 IF LEN(cap) = 0 THEN
  'No caption, use the default
  SELECT CASE mi.t
   CASE 1 ' special screen
    cap = get_special_menu_caption(mi.sub_t, menu.edit_mode)
   CASE 2 ' another menu
    cap = getmenuname(mi.sub_t)
    IF cap = "" THEN cap = "Menu " & mi.sub_t
   CASE 3 ' Text Box
    cap = "Text Box " & mi.sub_t
   CASE 4 ' Run Script
    cap = scriptname(mi.sub_t, plottrigger)
  END SELECT
 END IF
 IF menu.edit_mode = YES AND LEN(TRIM(cap)) = 0 THEN cap = "[BLANK]" 
 #IFDEF IS_GAME
  embedtext cap
 #ENDIF
 IF menu.max_chars > 0 THEN ' Crop overlength
  IF menu.align = 1 THEN ' right align
   cap = RIGHT(cap, menu.max_chars)
  ELSE ' left and center align
   cap = LEFT(cap, menu.max_chars)
  END IF
 END IF
 RETURN cap
END FUNCTION

FUNCTION get_special_menu_caption(subtype AS INTEGER, edit_mode AS INTEGER= NO) AS STRING
 DIM cap AS STRING
 SELECT CASE subtype
  CASE 0: cap = readglobalstring(60, "Items", 10)
  CASE 1: cap = readglobalstring(61, "Spells", 10)
  CASE 2: cap = readglobalstring(62, "Status", 10)
  CASE 3: cap = readglobalstring(63, "Equip", 10)
  CASE 4: cap = readglobalstring(64, "Order", 10)
  CASE 5: cap = readglobalstring(65, "Team", 10)
  CASE 6
   IF readbit(gen(), genBits, 5) THEN
    cap = readglobalstring(65, "Team", 10)
   ELSE
    cap = readglobalstring(64, "Order", 10)
   END IF
   IF edit_mode = YES THEN cap = cap & " [general bitset]"
  CASE 7,12:
   cap = readglobalstring(68, "Map", 10)
   IF subtype = 7 AND edit_mode = YES THEN cap = cap & " [if allowed by map]"
  CASE 8,13:
   cap = readglobalstring(66, "Save", 10)
   IF subtype = 8 AND edit_mode = YES THEN cap = cap & " [if allowed by map]"
  CASE 9: cap = "Load" ' FIXME: Needs a global text string
  CASE 10: cap = readglobalstring(67, "Quit", 10)
  CASE 11: cap = readglobalstring(69, "Volume", 10)
 END SELECT
 RETURN cap
END FUNCTION

SUB create_default_menu(menu AS MenuDef)
 DIM i AS INTEGER
 ClearMenuData menu
 FOR i = 0 TO 3  ' item, spell, status, equip
  append_menu_item(menu, "", 1, i)
 NEXT i
 append_menu_item(menu, "", 1, 6)  ' Order/Status menu
 FOR i = 7 TO 8  ' map, save
  append_menu_item(menu, "", 1, i)
  menu.last->hide_if_disabled = YES
 NEXT
 FOR i = 10 TO 11  ' quit, volume
  append_menu_item(menu, "", 1, i)
 NEXT
 menu.translucent = YES
 menu.min_chars = 14
END SUB

FUNCTION read_menu_int (menu AS MenuDef, intoffset AS INTEGER) as integer
 '--This function allows read access to integers in a menu for the plotscripting interface
 '--intoffset is the integer offset, same as appears in the MENUS.BIN lump documentation
 DIM bits(0) AS INTEGER
 WITH menu
  SELECT CASE intoffset
   CASE 12: RETURN .boxstyle
   CASE 13: RETURN .textcolor
   CASE 14: RETURN .maxrows
   CASE 15:
    MenuBitsToArray menu, bits()
    RETURN bits(0)
   CASE 16: RETURN .offset.x
   CASE 17: RETURN .offset.y
   CASE 18: RETURN .anchor.x
   CASE 19: RETURN .anchor.y
   CASE 20: RETURN .align
   CASE 21: RETURN .min_chars
   CASE 22: RETURN .max_chars
   CASE 23: RETURN .bordersize
   CASE 24: RETURN .on_close
   CASE 25: RETURN .esc_menu
   CASE ELSE
    debug "read_menu_int: " & intoffset & " is an invalid integer offset"
  END SELECT
 END WITH
 RETURN 0
END FUNCTION

SUB write_menu_int (menu AS MenuDef, intoffset AS INTEGER, n AS INTEGER)
 '--This sub allows write access to integers in a menu for the plotscripting interface
 '--intoffset is the integer offset, same as appears in the MENUS.BIN lump documentation
 DIM bits(0) AS INTEGER
 WITH menu
  SELECT CASE intoffset
   CASE 12: .boxstyle = n
   CASE 13: .textcolor = n
   CASE 14: .maxrows = n
   CASE 15:
    bits(0) = n
    MenuBitsFromArray menu, bits()
   CASE 16: .offset.x = n
   CASE 17: .offset.y = n
   CASE 18: .anchor.x = n
   CASE 19: .anchor.y = n
   CASE 20: .align = n
   CASE 21: .min_chars = n
   CASE 22: .max_chars = n
   CASE 23: .bordersize = n
   CASE 24: .on_close = n
   CASE 25: .esc_menu = n
   CASE ELSE
    debug "write_menu_int: " & intoffset & " is an invalid integer offset"
  END SELECT
 END WITH
END SUB

FUNCTION read_menu_item_int (mi AS MenuDefItem, intoffset AS INTEGER) as integer
 '--This function allows read access to integers in a menu item for the plotscripting interface
 '--intoffset is the integer offset, same as appears in the MENUITEM.BIN lump documentation
 DIM bits(0) AS INTEGER
 WITH mi
  SELECT CASE intoffset
   CASE 22: RETURN .t
   CASE 23: RETURN .sub_t
   CASE 24: RETURN .tag1
   CASE 25: RETURN .tag2
   CASE 26: RETURN .settag
   CASE 27: RETURN .togtag
   CASE 28:
    MenuItemBitsToArray mi, bits()
    RETURN bits(0)
   CASE 29 TO 31: RETURN .extra(intoffset - 29)
   CASE ELSE
    debug "read_menu_item_int: " & intoffset & " is an invalid integer offset"
  END SELECT
 END WITH
 RETURN 0
END FUNCTION

SUB write_menu_item_int (mi AS MenuDefItem, intoffset AS INTEGER, n AS INTEGER)
 '--This sub allows write access to integers in a menu item for the plotscripting interface
 '--intoffset is the integer offset, same as appears in the MENUITEM.BIN lump documentation
 DIM bits(0) AS INTEGER
 WITH mi
  SELECT CASE intoffset
   CASE 22: .t = n
   CASE 23: .sub_t = n
   CASE 24: .tag1 = n
   CASE 25: .tag2 = n
   CASE 26: .settag = n
   CASE 27: .togtag = n
   CASE 28:
    bits(0) = n
    MenuItemBitsFromArray mi, bits()
   CASE 29 TO 31: .extra(intoffset - 29) = n
   CASE ELSE
    debug "write_menu_item_int: " & intoffset & " is an invalid integer offset"
  END SELECT
 END WITH
END SUB

FUNCTION bound_arg(n AS INTEGER, min AS INTEGER, max AS INTEGER, argname AS ZSTRING PTR, context AS ZSTRING PTR=nulzstr, fromscript AS INTEGER=YES, errlvl AS INTEGER = 4) AS INTEGER
 'This function takes zstring ptr arguments because passing strings is actually really expensive
 '(it performs an allocation, copy, delete), and would be easily noticeable by scripts.
 IF n < min OR n > max THEN
#IFDEF IS_GAME
  IF fromscript THEN
   IF *context = "" ANDALSO curcmd->kind = tyfunct THEN
    scripterr commandname(curcmd->value) + ": invalid " & *argname & " " & n, errlvl
   ELSE
    scripterr *context & ": invalid " & *argname & " " & n, errlvl
   END IF
   RETURN NO
  END IF
#ENDIF
  debug *context & ": invalid " & *argname & " " & n
  RETURN NO
 END IF
 RETURN YES
END FUNCTION

SUB reporterr(msg AS STRING, errlvl AS INTEGER = 5)
 'this is a placeholder for some more detailed replacement of debug, so scripterrs can be thrown from slices.bas
#IFDEF IS_GAME
 IF insideinterpreter THEN
  DIM msg2 AS STRING = msg  'Don't modify passed-in strings
  IF curcmd->kind = tyfunct THEN msg2 = commandname(curcmd->value) + ": " + msg2
  scripterr msg2, errlvl
 ELSE
  debug msg
 END IF
#ELSE
 debug msg
#ENDIF
END SUB

FUNCTION tag_set_caption(n AS INTEGER, prefix AS STRING="Set Tag") AS STRING
 RETURN tag_condition_caption(n, prefix, "N/A", "Unchangeable", "Unchangeable")
END FUNCTION

FUNCTION tag_condition_caption(n AS INTEGER, prefix AS STRING="Tag", zerocap AS STRING="", onecap AS STRING="", negonecap AS STRING="") AS STRING
 DIM s AS STRING
 DIM cap AS STRING
 s = prefix
 IF LEN(s) > 0 THEN s = s & " "
 s = s & ABS(n) & "=" & onoroff(n)
 cap = load_tag_name(n)
 IF n = 0 AND LEN(zerocap) > 0 THEN cap = zerocap
 IF n = 1 AND LEN(onecap) > 0 THEN cap = onecap
 IF n = -1 AND LEN(negonecap) > 0 THEN cap = negonecap
 cap = TRIM(cap)
 IF LEN(cap) > 0 THEN s = s & " (" & cap & ")"
 RETURN s
END FUNCTION

FUNCTION onoroff (n AS INTEGER) AS STRING
 IF n >= 0 THEN RETURN "ON"
 RETURN "OFF"
END FUNCTION

'Returns a YES/NO string. Not to be confused with yesno() (in this file)
'which asks an interactive yes/no question
FUNCTION yesorno (n AS INTEGER, yes_cap AS STRING="YES", no_cap AS STRING="NO") AS STRING
 IF n THEN RETURN yes_cap
 RETURN no_cap
END FUNCTION

'This is mostly equivalent to '(float * 100) & "%"', however it doesn't show
'exponentials, and it rounds to some number of significant places
FUNCTION format_percent(byval float as double, byval sigfigs as integer = 5) as string
 DIM deciplaces as integer = sigfigs - (INT(LOG(ABS(float * 100)) / LOG(10)) + 1)
 IF deciplaces > sigfigs THEN deciplaces = sigfigs
 DIM repr as string = FORMAT(float * 100, "0." & STRING(deciplaces, "#"))
 'Unlike STR, FORMAT will add a trailing point
 IF repr[LEN(repr) - 1] = ASC(".") THEN repr = LEFT(repr, LEN(repr) - 1)
 RETURN repr + "%"
END FUNCTION

FUNCTION load_tag_name (index AS INTEGER) AS STRING
 IF index = 0 THEN RETURN ""
 IF index = 1 THEN RETURN "Never"
 IF index = -1 THEN RETURN "Always"
 RETURN readbadgenericname(ABS(index), game + ".tmn", 42, 0, 20)
END FUNCTION

SUB save_tag_name (tagname AS STRING, index AS INTEGER)
 DIM buf(20)
 setpicstuf buf(), 42, -1
 writebadbinstring tagname, buf(), 0, 20
 storeset game + ".tmn", index, 0
END SUB

SUB dump_master_palette_as_hex (master_palette() AS RGBColor)
 DIM i AS INTEGER
 DIM hexstring AS STRING = " DIM colorcodes(255) AS INTEGER = {"
 FOR i = 0 to 255
  hexstring = hexstring & "&h" & hex(master_palette(i).col, 6)
  IF i <> 255 THEN hexstring = hexstring & ","
  IF LEN(hexstring) > 88 THEN
   hexstring = hexstring & "_"
   debug hexstring
   hexstring = "        "
  END IF
 NEXT i
 hexstring = hexstring & "}"
 debug hexstring
END SUB

SUB load_default_master_palette (master_palette() AS RGBColor)
 'To regenerate this if the default master palette changes, use dump_master_palette_as_hex
 DIM colorcodes(255) AS INTEGER = {&h000000,&h232222,&h312F2B,&h3F3B34,&h4C483C,&h5D5747,_
        &h716A54,&h857C61,&h9A8F6D,&hAFA277,&hC4B581,&hD8C68B,&hEAD694,&hFDBC3B,&hFC9D47,_
        &hFA7D53,&h0D0F0D,&h121111,&h2A2426,&h41323B,&h583D51,&h6F456D,&h81577B,&h916684,_
        &hA2778D,&hB28997,&hC39CA3,&hD3B0B0,&hE3C9C4,&hEEDCD6,&hF4E7E4,&hFAF3F3,&h1F221E,_
        &h0C0E1C,&h1C203E,&h2A305E,&h39407D,&h495198,&h5962B1,&h6975C4,&h8084D0,&h9793DD,_
        &hAEA2EA,&hC1B8F1,&hD3CEF7,&hE2DFFC,&hECEBFF,&hF6F6FF,&h2F342E,&h15091C,&h2B1239,_
        &h411B56,&h562473,&h6C2D90,&h8236AC,&h9740C9,&hAD49E6,&hC154FF,&hCB68FF,&hD57CFF,_
        &hDF90FF,&hE9A4FF,&hF2B8FF,&hFCCCFF,&h40463E,&h060E27,&h0E2059,&h153289,&h1B45AE,_
        &h1E5DC0,&h2179D3,&h2294DD,&h24B0E6,&h25CEF0,&h27EEF9,&h3DFFFA,&h75FFFB,&hA3FFFC,_
        &hC4FFFD,&hE4FFFE,&h4F595A,&h170000,&h340000,&h500000,&h6B0000,&h870000,&hA30000,_
        &hBF0000,&hDC2D2D,&hFA5F5F,&hFF7F7F,&hFF9D9D,&hFFB9B9,&hFFD0D0,&hFFE1E1,&hFFF1F1,_
        &h5F6B75,&h140F00,&h2D2200,&h463400,&h5E4600,&h765800,&h8E6B00,&hA67D00,&hBF8F00,_
        &hD7A100,&hEFB300,&hFFC70D,&hFFDD30,&hFFEF4D,&hFFFC62,&hFFFFB4,&h707D8F,&h140614,_
        &h2E0F2E,&h471747,&h5F1F5F,&h782878,&h913091,&hAB3CA2,&hC74AB0,&hE358BE,&hFF67CC,_
        &hFF8AD8,&hFFACE3,&hFFC7EC,&hFFDBF3,&hFFEFF9,&h898CA0,&h1B0904,&h3D150A,&h5A2419,_
        &h76352C,&h91463F,&hAC5752,&hBF6666,&hD17579,&hE4848C,&hF693A0,&hFFA8B5,&hFFC2CB,_
        &hFFD7DD,&hFFE7EA,&hFFF6F7,&hA19CB0,&h080B0E,&h121921,&h1B2733,&h363B45,&h4D484B,_
        &h61504B,&h75584B,&h89614B,&h9E694B,&hB1774F,&hC38C56,&hD3A560,&hDFC171,&hE8D67D,_
        &hF1EA89,&hBAABC1,&h091207,&h162911,&h20411C,&h285B2A,&h45692A,&h647729,&h7B8639,_
        &h90964E,&hA2A860,&hAFBE6C,&hBDD379,&hC9E784,&hD3F88E,&hDFFFA7,&hF1FFD8,&hCCBDD0,_
        &h0F0F0F,&h232221,&h363331,&h494D3C,&h436443,&h4F7A54,&h5A8F67,&h64A57D,&h6DBA96,_
        &h76CFB1,&h7DE5D0,&h84F9F1,&hA4FFFB,&hC4FFFC,&hE5FFFE,&hDAD0DD,&h161010,&h322524,_
        &h4D3836,&h6A4C44,&h836052,&h9A7360,&hAF846C,&hC29478,&hD4A484,&hE1B494,&hEDC2A2,_
        &hF6D2B6,&hF9E2CF,&hFBECE1,&hFDF7F2,&hE6E0E8,&h0B230B,&h0E300E,&h123D12,&h154C15,_
        &h196119,&h1E771E,&h228B22,&h379F37,&h3CB23A,&h44C53D,&h65D95D,&h6BEB61,&h98FA90,_
        &hCCFFCA,&hE5FFE9,&hEFEBF0,&h180B09,&h371916,&h52281E,&h6B3824,&h84492A,&h9E5A30,_
        &hB56E24,&hCD8316,&hDF9814,&hE6AD33,&hECC253,&hF3D773,&hF7E69A,&hFAEFBE,&hFCF7E2,_
        &hFFFFFF,&h000000,&h001D48,&h002C6F,&h003B95,&h004BBC,&h005AE2,&h076DFF,&h258BFF,_
        &h43A9FF,&h61C7FF,&h85D6FF,&hA8E2FF,&hC5EBFF,&hDAF2FF,&hEEF9FF}
 DIM i AS INTEGER
 FOR i = 0 TO 255
  master_palette(i).col = colorcodes(i)
 NEXT i
END SUB

FUNCTION enter_or_space () AS INTEGER
 RETURN keyval(scEnter) > 1 OR keyval(scSpace) > 1
END FUNCTION

FUNCTION copy_keychord () AS INTEGER
 RETURN (keyval(scCtrl) > 0 AND keyval(scInsert) > 1) OR (keyval(scShift) > 0 AND keyval(scDelete) > 0) OR (keyval(scCtrl) > 0 AND keyval(scC) > 1)
END FUNCTION

FUNCTION paste_keychord () AS INTEGER
 RETURN (keyval(scShift) > 0 AND keyval(scInsert) > 1) OR (keyval(scCtrl) > 0 AND keyval(scV) > 1)
END FUNCTION

'Simple... and yet, more options than a regular menu item
'Can also insert instead of appending... bad name
SUB append_simplemenu_item (menu() as SimpleMenu, caption as string, BYVAL enabled as integer = YES, BYVAL col as integer = -1, BYVAL dat as integer = 0, BYVAL where as integer = -1)
 IF col = -1 THEN col = uilook(uiText)
 IF where = -1 THEN
  REDIM PRESERVE menu(LBOUND(menu) TO UBOUND(menu) + 1)
  where = UBOUND(menu)
 END IF
 WITH menu(where)
  .text = caption
  .enabled = enabled
  .col = col
  .dat = dat
 END WITH
END SUB

FUNCTION append_menu_item(BYREF menu AS MenuDef, caption AS STRING, t AS INTEGER=0, sub_t AS INTEGER=0) as integer
 DIM i AS INTEGER
 DIM item AS MenuDefItem ptr
 item = NEW MenuDefItem
 WITH *item
  .caption = caption
  .t = t
  .sub_t = sub_t
 END WITH

 dlist_append(menu.itemlist, item) 'updates .numitems

 'rather than call SortMenuItems, shuffle hidden items down a slot and insert new item
 menu.items = REALLOCATE(menu.items, menu.numitems * SIZEOF(any ptr))
 FOR i = menu.numitems - 2 TO 0 STEP -1  'last item in array is garbage
  IF menu.items[i]->disabled AND menu.items[i]->hide_if_disabled THEN
   SWAP menu.items[i], menu.items[i + 1]
  ELSE
   EXIT FOR
  END IF
 NEXT
 menu.items[i + 1] = item

 RETURN menu.numitems - 1
END FUNCTION

SUB remove_menu_item(BYREF menu AS MenuDef, BYVAL mi AS MenuDefItem ptr)
 dlist_remove menu.itemlist, mi
 DELETE mi
 'rebuild menu.items[]
 SortMenuItems menu
END SUB

SUB remove_menu_item(BYREF menu AS MenuDef, BYVAL mislot AS INTEGER)
 remove_menu_item menu, menu.items[mislot]
END SUB

SUB swap_menu_items(BYREF menu1 AS MenuDef, BYVAL mislot1 AS INTEGER, BYREF menu2 AS MenuDef, BYVAL mislot2 AS INTEGER)
 dlist_swap(menu1.itemlist, menu1.items[mislot1], menu2.itemlist, menu2.items[mislot2])
 SortMenuItems menu1
 SortMenuItems menu2
END SUB

SUB write_npc_int (npcdata AS NPCType, intoffset AS INTEGER, n AS INTEGER)
 '--intoffset is the integer offset, same as appears in the .N lump documentation
 WITH npcdata
  SELECT CASE intoffset
   CASE 0: .picture = n
   CASE 1: .palette = n
   CASE 2: .movetype = n
   CASE 3: .speed = n
   CASE 4: .textbox = n
   CASE 5: .facetype = n
   CASE 6: .item = n
   CASE 7: .pushtype = n
   CASE 8: .activation = n
   CASE 9: .tag1 = n
   CASE 10: .tag2 = n
   CASE 11: .usetag = n
   CASE 12: .script = n
   CASE 13: .scriptarg = n
   CASE 14: .vehicle = n
   CASE 15: .defaultzone = n
   CASE ELSE
    debug "write_npc_int: " & intoffset & " is an invalid integer offset"
  END SELECT
 END WITH
END SUB

FUNCTION read_npc_int (npcdata AS NPCType, intoffset AS INTEGER) AS INTEGER
 '--intoffset is the integer offset, same as appears in the .N lump documentation
 WITH npcdata
  SELECT CASE intoffset
   CASE 0: RETURN .picture
   CASE 1: RETURN .palette
   CASE 2: RETURN .movetype
   CASE 3: RETURN .speed
   CASE 4: RETURN .textbox
   CASE 5: RETURN .facetype
   CASE 6: RETURN .item
   CASE 7: RETURN .pushtype
   CASE 8: RETURN .activation
   CASE 9: RETURN .tag1
   CASE 10: RETURN .tag2
   CASE 11: RETURN .usetag
   CASE 12: RETURN .script
   CASE 13: RETURN .scriptarg
   CASE 14: RETURN .vehicle
   CASE 15: RETURN .defaultzone
   CASE ELSE
    debug "read_npc_int: " & intoffset & " is an invalid integer offset"
  END SELECT
 END WITH
 RETURN 0
END FUNCTION

SUB lockstep_tile_animation (tilesets() AS TilesetData ptr, layer AS INTEGER)
 'Called after changing a layer's tileset to make sure its tile animation is in phase with other layers of the same tileset
 FOR i AS INTEGER = 0 TO UBOUND(tilesets)
  IF i <> layer ANDALSO tilesets(i) ANDALSO tilesets(i)->num = tilesets(layer)->num THEN
   tilesets(layer)->anim(0) = tilesets(i)->anim(0)
   tilesets(layer)->anim(1) = tilesets(i)->anim(1)
   EXIT SUB
  END IF
 NEXT
END SUB

SUB unloadtilesetdata (BYREF tileset AS TilesetData ptr)
 IF tileset <> NULL THEN
  'debug "unloading tileset " & tileset->num
  frame_unload @tileset->spr
  DELETE tileset
  tileset = NULL
 END IF
END SUB

SUB maptilesetsprint (tilesets() AS TilesetData ptr)
 DIM i AS INTEGER
 FOR i = 0 TO UBOUND(tilesets)
  IF tilesets(i) = NULL THEN
   debug i & ": NULL"
  ELSE 
   debug i & ": " & tilesets(i)->num
  END IF
 NEXT
END SUB

SUB loadtilesetdata (tilesets() AS TilesetData ptr, BYVAL layer AS INTEGER, BYVAL tilesetnum AS INTEGER, BYVAL lockstep AS INTEGER = YES)
'the tileset may already be loaded
'note that tile animation data is NOT reset if the old tileset was the same	

 IF tilesets(layer) = NULL ORELSE tilesets(layer)->num <> tilesetnum THEN
  unloadtilesetdata tilesets(layer)
  tilesets(layer) = NEW TilesetData

  WITH *tilesets(layer)
   .num = tilesetnum
   'debug "loading tileset " & tilesetnum

   .spr = tileset_load(tilesetnum)
   loadtanim tilesetnum, .tastuf()
  END WITH
 END IF
 FOR i AS INTEGER = 0 TO 1
  WITH tilesets(layer)->anim(i)
   .cycle = 0
   .pt = 0
   .skip = 0
  END WITH
 NEXT
 IF lockstep THEN lockstep_tile_animation tilesets(), layer
END SUB

FUNCTION layer_tileset_index(BYVAL layer AS INTEGER) AS INTEGER
'return the gmap() index containing a layer's tileset
 IF layer <= 2 THEN RETURN 22 + layer ELSE RETURN 23 + layer
END FUNCTION

SUB loadmaptilesets (tilesets() AS TilesetData ptr, gmap() AS INTEGER, BYVAL resetanimations AS INTEGER = YES)
'tilesets() may contain already loaded tilesets. In this case, we can reuse them
 DIM AS INTEGER i, j
 DIM tileset AS INTEGER

 FOR i = 0 TO UBOUND(tilesets)
  tileset = gmap(layer_tileset_index(i))
  IF tileset <> 0 THEN
   tileset = tileset - 1
  ELSE
   tileset = gmap(0)
  END IF

  loadtilesetdata tilesets(), i, tileset
  IF resetanimations THEN
   FOR j = 0 TO 1
    WITH tilesets(i)->anim(j)
     .cycle = 0
     .pt = 0
     .skip = 0
    END WITH
   NEXT
  END IF
 NEXT
END SUB

SUB unloadmaptilesets (tilesets() AS TilesetData ptr)
 DIM i AS INTEGER
 FOR i = 0 TO UBOUND(tilesets)
  unloadtilesetdata tilesets(i)
 NEXT
END SUB

FUNCTION xreadbit (bitarray() AS INTEGER, bitoffset AS INTEGER, intoffset AS INTEGER=0) AS INTEGER
 'This is a wrapper for readbit that returns YES/NO and accepts a default arg of zero for the integer offset
 RETURN readbit(bitarray(), intoffset, bitoffset) <> 0 
END FUNCTION

FUNCTION getheroname (hero_id AS INTEGER) AS STRING
 DIM her AS HeroDef
 IF hero_id >= 0 THEN
  loadherodata @her, hero_id
  RETURN her.name
 END IF
 RETURN ""
END FUNCTION

FUNCTION getmenuname(record AS INTEGER) AS STRING
 DIM as string ret
#IFDEF IS_GAME
 STATIC cache(32) as IntStrPair
 ret = search_string_cache(cache(), record, game)
 IF ret <> "" THEN RETURN ret
#ENDIF

 DIM menu_set AS MenuSet
 menu_set.menufile = workingdir + SLASH + "menus.bin"
 menu_set.itemfile = workingdir + SLASH + "menuitem.bin"
 DIM menu AS MenuDef
 LoadMenuData menu_set, menu, record, YES
 ret = menu.name
 ClearMenuData menu

#IFDEF IS_GAME
 add_string_cache cache(), record, ret
#ENDIF
 RETURN ret
END FUNCTION

SUB draw_scrollbar(state AS MenuState, menu AS MenuDef, page AS INTEGER)
 draw_scrollbar state, menu.rect, menu.boxstyle, page
END SUB

SUB draw_scrollbar(state AS MenuState, rect AS RectType, boxstyle AS INTEGER=0, page AS INTEGER)
 DIM count AS INTEGER = state.last - state.first + 1
 draw_scrollbar state, rect, count, boxstyle, page
END SUB

'count being the number of (visible) menu items
SUB draw_scrollbar(state AS MenuState, rect AS RectType, count AS INTEGER, boxstyle AS INTEGER=0, page AS INTEGER)
 'recall state.size is off-by-1
 IF state.top > state.first OR count > (state.size + 1) THEN
  IF count > 0 THEN
   DIM sbar AS RectType
   DIM slider AS RectType
   sbar.x = rect.x + rect.wide - 6
   sbar.y = rect.y + 2
   sbar.wide = 4
   sbar.high = rect.high - 4
   WITH sbar
    slider.y = .high / count * (state.top - state.first)
    slider.high = .high / count * (state.size + 1)
    rectangle .x, .y, .wide, .high, uilook(uiBackground), page
    rectangle .x, .y + slider.y, .wide, slider.high, uilook(uiTextBox + boxstyle * 2 + 1), page
   END WITH
  END IF
 END IF
END SUB

SUB draw_fullscreen_scrollbar(state AS MenuState, boxstyle AS INTEGER=0, page AS INTEGER)
 DIM rect AS RectType
 rect.wide = 320
 rect.high = 200
 draw_scrollbar state, rect, boxstyle, page
END SUB

FUNCTION get_text_box_height(BYREF box AS TextBox) AS INTEGER
 IF box.shrink >= 0 THEN RETURN 88 - box.shrink * 4
 FOR i AS INTEGER = UBOUND(box.text) TO 0 STEP -1
  IF LEN(TRIM(box.text(i))) > 0 THEN
   DIM vsize AS INTEGER = 20 + i * 10
   IF vsize < 32 AND vsize > 24 THEN RETURN 32
   IF vsize <= 24 THEN RETURN 16
   RETURN vsize
  END IF
 NEXT i
 RETURN 88
END FUNCTION

FUNCTION last_inv_slot() AS INTEGER
 '--If genMaxInventory is 0, return the default inventory size
 IF gen(genMaxInventory) = 0 THEN RETURN inventoryMax
 '--Otherwise round genMaxInventory up to the nearest
 '-- multiple of three (counting the zero-slot) and return it.
 RETURN ((gen(genMaxInventory) + 3) \ 3) * 3 - 1
END FUNCTION

SUB setup_sprite_sizes ()
 'Populates the global sprite_sizes
 WITH sprite_sizes(0)
  .name = "Hero"
  .size.x = 32
  .size.y = 40
  .frames = 8
  .genmax = genMaxHeroPic
 END WITH
 WITH sprite_sizes(1)
  .name = "Small Enemy"
  .size.x = 34
  .size.y = 34
  .frames = 1
  .genmax = genMaxEnemy1Pic
 END WITH
 WITH sprite_sizes(2)
  .name = "Medium Enemy"
  .size.x = 50
  .size.y = 50
  .frames = 1
  .genmax = genMaxEnemy2Pic
 END WITH
 WITH sprite_sizes(3)
  .name = "Large Enemy"
  .size.x = 80
  .size.y = 80
  .frames = 1
  .genmax = genMaxEnemy3Pic
 END WITH
 WITH sprite_sizes(4)
  .name = "Walkabout"
  .size.x = 20
  .size.y = 20
  .frames = 8
  .genmax = genMaxNPCPic
 END WITH
 WITH sprite_sizes(5)
  .name = "Weapon"
  .size.x = 24
  .size.y = 24
  .frames = 2
  .genmax = genMaxWeaponPic
 END WITH
 WITH sprite_sizes(6)
  .name = "Attack"
  .size.x = 50
  .size.y = 50
  .frames = 3
  .genmax = genMaxAttackPic
 END WITH
 WITH sprite_sizes(7)
  .name = "Box Border"
  .size.x = 16
  .size.y = 16
  .frames = 16
  .genmax = genMaxBoxBorder
 END WITH
 WITH sprite_sizes(8)
  .name = "Portrait"
  .size.x = 50
  .size.y = 50
  .frames = 1
  .genmax = genMaxPortrait
 END WITH
 WITH sprite_sizes(sprTypeMXS)  '9
  .name = "Backdrop"
  .size.x = 320
  .size.y = 200
  .frames = 1
  .genmax = genNumBackdrops
  .genmax_offset = -1
 END WITH
 WITH sprite_sizes(sprTypeFrame)   '10
  .name = "Pixel array"
  .frames = 1
 END WITH
 
 WITH sprite_sizes(-1) '--Only use for temporary use in the secret desting/debugging menu
  .name = "Experimental"
  .frames = 1
  .genmax = -1 'this should throw an error rather than corrupting gen(0)
 END WITH
END SUB

SUB load_sprite_and_pal (BYREF img AS GraphicPair, BYVAL spritetype AS INTEGER, BYVAL index AS INTEGER, BYVAL palnum AS INTEGER=-1)
 unload_sprite_and_pal img
 IF spritetype = sprTypeMXS THEN
  img.sprite = loadmxs(game + ".mxs", index)
 ELSEIF spritetype >= 0 AND spritetype <= 8 THEN
  img.sprite = frame_load(spritetype, index)
  img.pal    = palette16_load(palnum, spritetype, index)
 ELSE
  debug "load_sprite_and_pal: bad spritetype " & spritetype & " (index " & index & " pal " & palnum & ")"
 END IF
END SUB

SUB unload_sprite_and_pal (BYREF img AS GraphicPair)
 frame_unload @img.sprite
 palette16_unload @img.pal
END SUB

FUNCTION decode_backslash_codes(s AS STRING) AS STRING
 DIM result AS STRING = ""
 DIM i AS INTEGER = 1
 DIM ch AS STRING
 DIM mode AS INTEGER = 0
 DIM nstr AS STRING
 DIM num AS INTEGER
 DO
  ch = MID(s, i, 1)
  SELECT CASE mode
   CASE 0'--normal
    IF ch = "\" THEN
      mode = 1
      nstr = ""
    ELSE
      result &= ch
    END IF
   CASE 1'--parsing backslash
    SELECT CASE ch
     CASE "\" '--an escaped backslash
      result &= "\"
      mode = 0
     CASE "n" '-- a newline
      result &= CHR(10)
      mode = 0
     CASE "r" '-- a carriage return
      result &= CHR(13)
      mode = 0
     CASE "t" '-- a tab
      result &= CHR(9)
      mode = 0
     CASE "0", "1", "2"
      nstr &= ch
      mode = 2
     CASE ELSE '--not a valid backslash code, resume without discarding the backslash
      result &= "\" & ch
      mode = 0
    END SELECT
   CASE 2'--parsing ascii code number
    SELECT CASE ch
     CASE "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
      nstr &= ch
     CASE ELSE 'busted backslash code, print warning
      debug "Bogus backslash ascii code in string """ & s & """"
      mode = 0
    END SELECT
    IF LEN(nstr) >= 3 THEN
     num = str2int(nstr)
     IF num > 255 THEN
      debug "Bogus backslash ascii code in string """ & s & """"
     ELSE
      result &= CHR(num)
     END IF
     mode = 0
    END IF
  END SELECT
  i += 1
 LOOP UNTIL i > LEN(s)
 IF mode <> 0 THEN
  debug "decode_backslash_codes: exited while parsing a backslash code (mode=" & mode & ")"
 END IF
 RETURN result
END FUNCTION

FUNCTION escape_nonprintable_ascii(s AS STRING) AS STRING
 DIM result AS STRING = ""
 DIM nstr AS STRING
 DIM ch AS STRING
 FOR i AS INTEGER = 1 TO LEN(s)
  ch = MID(s, i, 1)
  SELECT CASE ASC(ch)
   CASE 32 TO 91, 93 TO 126
    result &= ch
   CASE 92 '--Backslash
    result &= "\\"
   CASE 10
    result &= "\n"
   CASE 13
    result &= "\r"
   CASE 9
    result &= "\t"
   CASE ELSE
    nstr = STR(ASC(ch))
    WHILE LEN(nstr) < 3
     nstr = "0" & nstr
    WEND
    result &= "\" & nstr
  END SELECT
 NEXT i
 RETURN result
END FUNCTION

SUB clamp_menu_state (BYREF state AS MenuState)
 WITH state
  IF .pt < .top THEN .top = .pt
  IF .pt > .top + .size THEN .top = large(.top, .top + .size)
 END WITH
END SUB

FUNCTION getdisplayname (default AS STRING) AS STRING
 '--Get game's display name
 DIM f AS STRING
 f = workingdir & SLASH & "browse.txt"
 IF isfile(f) THEN
  setpicstuf buffer(), 40, -1
  loadset f, 0, 0
  DIM s AS STRING
  s = STRING(bound(buffer(0), 0, 38), " ")
  array2str buffer(), 2, s
  IF LEN(s) > 0 THEN
   RETURN s
  END IF
 END IF
 RETURN default
END FUNCTION

SUB getstatnames(statnames() AS STRING)
 REDIM statnames(11)
 statnames(0) = readglobalstring(0, "HP")
 statnames(1) = readglobalstring(1, "MP")
 statnames(2) = readglobalstring(2, "Atk")
 statnames(3) = readglobalstring(3, "Aim")
 statnames(4) = readglobalstring(5, "Def")
 statnames(5) = readglobalstring(6, "Dog")
 statnames(6) = readglobalstring(29, "Mag")
 statnames(7) = readglobalstring(30, "Wil")
 statnames(8) = readglobalstring(8, "Speed")
 statnames(9) = readglobalstring(7, "Counter")
 statnames(10) = readglobalstring(31, "Focus")
 statnames(11) = readglobalstring(4, "HitX")
END SUB

SUB getelementnames(elmtnames() AS STRING)
 REDIM elmtnames(gen(genNumElements) - 1)
 FOR i AS INTEGER = 0 TO gen(genNumElements) - 1
  DIM default AS STRING
  default = "Element" & i+1
  IF i < 8 THEN
   'Original indices changed so maxlen could be expanded
   default = readglobalstring(17 + i, default, 10)
  ELSEIF i < 16 THEN
   'Next 8 elements map to old enemytypes
   default = LEFT(readglobalstring(1 + i, "EnemyType" & i, 10) + "-killer", 14)
  END IF
  elmtnames(i) = readglobalstring(174 + i*2, default, 14)
 NEXT i
END SUB

SUB writebinstring (savestr AS STRING, array() AS INTEGER, offset AS INTEGER, maxlen AS INTEGER)
 DIM s AS STRING

 '--pad savestr to (at least) the right length
 s = savestr + STRING(maxlen - LEN(s), CHR(0))

 '--odd lengths would result in (harmless) garbage
 IF maxlen AND 1 THEN s += CHR(0): maxlen += 1

 '--write length (current not max)
 array(offset) = small(LEN(savestr), maxlen)

 FOR i AS INTEGER = 1 TO maxlen \ 2
  array(offset + i) = s[2 * i - 2] OR (s[2 * i - 1] SHL 8)
 NEXT
END SUB

SUB writebinstring (savestr AS STRING, array() AS SHORT, offset AS INTEGER, maxlen AS INTEGER)
 DIM s AS STRING

 '--pad savestr to (at least) the right length
 s = savestr + STRING(maxlen - LEN(s), CHR(0))

 '--odd lengths would result in (harmless) garbage
 IF maxlen AND 1 THEN s += CHR(0): maxlen += 1

 '--write length (current not max)
 array(offset) = small(LEN(savestr), maxlen)

 memcpy(@array(offset + 1), @s[0], maxlen)
END SUB

SUB writebadbinstring (savestr AS STRING, array() AS INTEGER, offset AS INTEGER, maxlen AS INTEGER, skipword AS INTEGER=0)
 '--write current length
 array(offset) = small(LEN(savestr), maxlen)

 DIM i AS INTEGER

 FOR i = 1 TO small(LEN(savestr), maxlen)
  array(offset + skipword + i) = savestr[i - 1]
 NEXT i

 FOR i = LEN(savestr) + 1 TO maxlen
  array(offset + skipword + i) = 0
 NEXT i

END SUB

FUNCTION readbinstring (array() AS INTEGER, offset AS INTEGER, maxlen AS INTEGER) AS STRING

 DIM result AS STRING = ""
 DIM strlen AS INTEGER = bound(array(offset), 0, maxlen)
 DIM i AS INTEGER
 DIM n AS INTEGER

 i = 1
 DO WHILE LEN(result) < strlen
  '--get an int
  n = array(offset + i)
  i = i + 1

  '--append the lowbyte as a char
  result = result & CHR(n AND &HFF)

  '--if we still care about the highbyte, append it as a char too
  IF LEN(result) < strlen THEN
   result = result & CHR((n SHR 8) AND &HFF)
  END IF

 LOOP

 RETURN result
END FUNCTION

FUNCTION readbinstring (array() AS SHORT, offset AS INTEGER, maxlen AS INTEGER) AS STRING
 DIM strlen AS INTEGER = bound(array(offset), 0, maxlen)
 DIM result AS STRING = STRING(strlen, 0)
 memcpy(@result[0], @array(offset + 1), strlen)
 RETURN result
END FUNCTION

FUNCTION readbadbinstring (array() AS INTEGER, offset AS INTEGER, maxlen AS INTEGER, skipword AS INTEGER=0) AS STRING
 DIM result AS STRING = ""
 DIM strlen AS INTEGER = bound(array(offset), 0, maxlen)
 DIM n AS INTEGER

 FOR i AS INTEGER = 1 TO strlen
  '--read and int
  n = array(offset + skipword + i)
  '--if the int is a char use it.
  IF n >= 0 AND n <= 255 THEN
   '--use it
   result = result & CHR(n)
  END IF
 NEXT i

 RETURN result
END FUNCTION

SUB set_homedir()
#IFDEF __UNIX__
 homedir = ENVIRON$("HOME")
#ELSE
 homedir = ENVIRON$("USERPROFILE") & SLASH & "My Documents" 'Is My Documents called something else for non-English versions of Windows?
 IF NOT isdir(homedir) THEN
  'Windows Vista uses "Documents" instead of "My Documents"
  homedir = ENVIRON$("USERPROFILE") & SLASH & "Documents"
 END IF
#ENDIF
END SUB

PRIVATE FUNCTION help_dir_helper(dirname AS STRING, fname AS STRING) AS INTEGER
 IF LEN(fname) THEN RETURN isfile(dirname + SLASH + fname) ELSE RETURN isdir(dirname)
END FUNCTION

FUNCTION get_help_dir(helpfile AS STRING="") AS STRING
 'what happened to prefsdir? [James: prefsdir only exists for game not custom right now]
 IF help_dir_helper(homedir & SLASH & "ohrhelp", helpfile) THEN RETURN homedir & SLASH & "ohrhelp"
 IF help_dir_helper(exepath & SLASH & "ohrhelp", helpfile) THEN RETURN exepath & SLASH & "ohrhelp"
 'platform-specific relative data files path (Mac OS X bundles)
 IF help_dir_helper(data_dir & SLASH & "ohrhelp", helpfile) THEN RETURN data_dir & SLASH & "ohrhelp"
 #IFDEF __UNIX__
 #IFDEF DATAFILES
  IF help_dir_helper(DATAFILES & SLASH & "ohrhelp", helpfile) THEN RETURN DATAFILES & SLASH & "ohrhelp"
 #ENDIF
 #ENDIF
 '-- if all else fails, use exepath even if invalid
 RETURN exepath & SLASH & "ohrhelp"
END FUNCTION

FUNCTION load_help_file(helpkey AS STRING) AS STRING
 DIM help_dir AS STRING
 help_dir = get_help_dir(helpkey & ".txt")
 IF isdir(help_dir) THEN
  DIM helpfile AS STRING
  helpfile = help_dir & SLASH & helpkey & ".txt"
  IF isfile(helpfile) THEN
   DIM fh AS INTEGER = FREEFILE
   OPEN helpfile FOR INPUT ACCESS READ AS #fh
   DIM helptext AS STRING = ""
   DIM s AS STRING
   DO WHILE NOT EOF(fh)
    LINE INPUT #fh, s
    helptext = helptext & s & CHR(10)
   LOOP
   CLOSE #fh
   RETURN helptext
  END IF
 END IF
 RETURN "No help found for """ & helpkey & """"
END FUNCTION

SUB save_help_file(helpkey AS STRING, text AS STRING)
 DIM help_dir AS STRING
 help_dir = get_help_dir()
 IF NOT isdir(help_dir) THEN
  IF makedir(help_dir) THEN
   visible_debug """" & help_dir & """ does not exist and could not be created."
   EXIT SUB
  END IF
 END IF
 DIM helpfile AS STRING
 helpfile = help_dir & SLASH & helpkey & ".txt"
 DIM fh AS INTEGER = FREEFILE
 IF OPEN(helpfile FOR OUTPUT ACCESS READ WRITE AS #fh) = 0 THEN
  DIM trimmed_text AS STRING
  trimmed_text = RTRIM(text, ANY " " & CHR(13) & CHR(10))
  PRINT #fh, trimmed_text
  CLOSE #fh
 ELSE
  visible_debug "help file """ & helpfile & """ is not writeable."
 END IF
END SUB

FUNCTION filenum (n AS INTEGER) AS STRING
 IF n < 100 THEN
  RETURN RIGHT("00" + STR(n), 2)
 ELSE
  RETURN STR(n)
 END IF
END FUNCTION
