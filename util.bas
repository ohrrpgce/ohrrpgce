'OHRRPGCE - General FreeBASIC utility code
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
'
' This file contains utility subs and functions which would be useful for
' any FreeBasic program. Nothing in here can depend on Allmodex, nor on any
' gfx or music backend, nor on any other part of the OHR

CONST STACK_SIZE_INC = 512 ' in integers

#include "config.bi"
#include "datetime.bi" 'FB header
#include "string.bi"  'FB header
#include "util.bi"
#include "miscc.bi"
#include "unicode.bi"
#include "fb/fb.bi"
#include "lib/sha1.bi"
#include "os.bi"
#include "common_base.bi"
#include "lumpfile.bi"
#ifdef __FB_MAIN__
#include "testing.bi"
#endif


Type FBSTRING as string
'Resize a FB string
Declare Function fb_hStrRealloc Alias "fb_hStrRealloc" (byval s as FBSTRING ptr, byval size as ssize_t, byval preserve as long) as FBSTRING ptr
'Resize a FB string or allocate a new one, and mark it temporary (equals SPEED)
Declare Function fb_hStrAllocTemp Alias "fb_hStrAllocTemp" (byval s as FBSTRING ptr, byval size as ssize_t) as FBSTRING ptr
'Although unused, documenting this here: free a temporary FBstring which is returned (Normally would destroy it with fb_StrAssign)
'(returns error code)
'Declare Function fb_hStrDelTemp Alias "fb_hStrDelTemp" (s as FBSTRING ptr) as long

DECLARE FUNCTION integerptr_compare CDECL (byval a as integer ptr ptr, byval b as integer ptr ptr) as long
DECLARE FUNCTION stringptr_compare CDECL (byval a as string ptr ptr, byval b as string ptr ptr) as long


'It is very important for this to be populated _before_ any calls to CHDIR
DIM orig_dir as string

DIM tmpdir as string

DIM exename as string

DIM program_start_timer as double

'Dummy value used to indicate the default value of a byref arg
DIM default_arg as integer

#ifdef timer_variables
DIM timer_variables
#endif

'---------------- Initialization -----------------


DIM SHARED filetype_names(fileTypeError) as string
filetype_names(fileTypeNonexistent) = "nonexistent"
filetype_names(fileTypeFile)        = "a file"
filetype_names(fileTypeDirectory)   = "a directory"
filetype_names(fileTypeOther)       = "a special file"
filetype_names(fileTypeError)       = "unreadable"


'Set up an error handler for the errors FB throws when compiled with -exx. The
'default handler prints a message which is lost on Windows.
'Note also that this installs signal handlers, but some of those signal handlers
'are overridden on Unix by setup_exception_handler()
SUB setup_fb_error_handler()
  'There seems to be a gengcc bug at play: passing the address of a label to a
  'function doesn't work (gcc docs say it's undefined behaviour) and the address
  'of this function gets passed instead. So we see this function reentering if
  'an error occurs, rather than starting at QB_error_handler!
  STATIC as bool already_setup
  IF already_setup THEN GOTO QB_error_handler
  already_setup = YES

  'What's this wacky QB stuff doing in a FB codebase!
  ON ERROR GOTO QB_error_handler
  EXIT SUB

 QB_error_handler:
  STATIC as integer reentered
  IF reentered <> 0 THEN SYSTEM 99 'fatal_error_shutdown
  reentered += 1

  'Warning: any code using anonymous temporary string variables here seems to crash,
  'unless compiling with -gen gcc, because the function prologue hasn't occurred!
  DIM as integer err_num = ERR, err_line = ERL
  DIM as zstring ptr func_name = ERFN, mod_name = ERMN
  DIM as zstring ptr message = ANY
  message = format_FB_error_message(err_num, err_line, mod_name, func_name)
  DIM interrupt_signal as bool = (err_num = fberrSIGINT) OR (err_num = fberrSIGQUIT) OR (err_num = fberrSIGTERM)
  fb_error_hook message, interrupt_signal
END SUB

SUB remove_fb_error_handler()
  ON ERROR GOTO 0
END SUB

'Called if FB catches a fatal error, signal, or exception, such as (if compiled
'with -exx) array out-of-bounds read, bad REDIM call, SIGSEGV. (Unless
'CrashRpt/etc is installed, then it catches signals.)  This hook is installed in
'one of two different ways by calling hook_fb_End or setup_fb_error_handler (not
'both!) Called on ASSERT failure only if fb_End hooked.
EXTERN "C"
SUB fb_error_hook(message as const zstring ptr, interrupt_signal as boolint)
  'Yes, this function is redundant, but it makes the control flow clearer.
  IF interrupt_signal THEN
   fatalerror message
  ELSE
   fatalbug message
  END IF
END SUB
END EXTERN

LOCAL SUB lowlevel_error (msg as zstring ptr)
#IFDEF __FB_WIN32__
 'We can't print to the console because that won't work in programs compiled for
 'the Windows 'gui' subsystem (and util.bas gets linked into both kinds of exe;
 'could use is_console_program() to detect)
 error_message_box(msg)
#ELSE
 PRINT *msg
#ENDIF
END SUB

'Gets called at the top of the main module for each executable just by including util.bi.
'This is the place to put initialisation code common to everything.
SUB lowlevel_init()
  'Note: if compiled with -e, FB will insert a call to fb_InitSignals()
  'at the top of main(), installing signals handlers for SIGSEGV, etc.
  'and will also, on x86 only, call fb_CpuDetect and print an error and quit if
  'the CPU isn't 686+ (Pentium Pro+) or (if compiled with -fpu sse) doesn't
  'support SSE & SSE2. We duplicate that since we don't always compile with -e

  program_start_timer = TIMER

  'Android only
  external_log "main() started..."

  init_crt   'setlocale

  os_init
  #IF defined(__FB_X86__) AND NOT defined(__FB_64BIT__) AND NOT defined(NO_SSE)
   'Check for SSE and SSE2 support
   IF (fb_CpuDetect() AND &h6000000) <> &h6000000 THEN
    #IFDEF __FB_WIN32__
     'Unfortunately if you're using a default (gfx_sdl2) build you won't see this
     'message due to dll errors unless you're running WinXP+ on an ancient machine.
     lowlevel_error "Your CPU doesn't support SSE2 instructions (Pentium 4+). Go to https://ohrrpgce.com and download a 'Win95' build of the OHRRPGCE for older computers"
    #ELSE
     lowlevel_error "Your CPU doesn't support SSE2 instructions (Pentium 4+)?! Recompile the OHRRPGCE for older CPUs."
    #ENDIF
    SYSTEM 1
   END IF
  #ENDIF

  'Install FB error handler, either by hooking fb_End (which is a kludge)
  'or using ON ERROR GOTO (which is so yuck it's also a kludge).
  'Hooking fb_End is preferred (but only works in FB 1.02+) because how ON ERROR
  'GOTO works as of FB 1.07, the topmost frame gets clobbered by a computed
  'goto, bad for debugging: though we would still be able to read the line
  'number from g/c_debug.txt in that case, we can't inspect locals.
  'It also catches fb_Assert().
  'If nothing if hooked then the program would terminate without an exception
  'when the rtlib throws an error, and CrashRpt would not be invoked!
  'We could almost just use atexit() instead, because fb's rtlib cleanup happens
  '(afterwards?) in a global destructor function, except not under emscripten,
  'and libfbgfx destroys the window before then.
  IF hook_fb_End() = NO THEN
   setup_fb_error_handler
   'Note: if --rawexx cmdline arg given, we later call remove_fb_error_handler.
  END IF

  exename = trimextension(trimpath(COMMAND(0)))

  'Requires exename
  setup_exception_handler

  disable_extended_precision

  reseed_prng TIMER * 1e9
END SUB

'------------- Basic datatypes -------------

DEFINE_VECTOR_OF_TYPE(XYPair, XYPair)

#MACRO XYPAIR_OPERATOR_AND_XY(OP)
  OPERATOR OP (lhs as XYPair, rhs as XYPair) as bool
    RETURN lhs.x OP rhs.x ANDALSO lhs.y OP rhs.y
  END OPERATOR
#ENDMACRO

#MACRO XYPAIR_OPERATOR_AND_INT(OP)
  OPERATOR OP (lhs as XYPair, rhs as integer) as bool
    RETURN lhs.x OP rhs ANDALSO lhs.y OP rhs
  END OPERATOR
#ENDMACRO

XYPAIR_OPERATOR_AND_XY (=)
XYPAIR_OPERATOR_AND_INT(=)
XYPAIR_OPERATOR_AND_XY(<)
XYPAIR_OPERATOR_AND_INT(<)
XYPAIR_OPERATOR_AND_XY(<=)
XYPAIR_OPERATOR_AND_INT(<=)
XYPAIR_OPERATOR_AND_XY(>)
XYPAIR_OPERATOR_AND_INT(>)
XYPAIR_OPERATOR_AND_XY(>=)
XYPAIR_OPERATOR_AND_INT(>=)

OPERATOR <> (lhs as XYPair, rhs as XYPair) as bool
  RETURN lhs.x <> rhs.x ORELSE lhs.y <> rhs.y
END OPERATOR

OPERATOR <> (lhs as XYPair, rhs as integer) as bool
  RETURN lhs.x <> rhs ORELSE lhs.y <> rhs
END OPERATOR

OPERATOR XYPair.CAST () as Float2
  RETURN XYF(x, y)
END OPERATOR

OPERATOR XYPair.CAST () as string
  RETURN x & "," & y
END OPERATOR

OPERATOR WHSimple.CAST () as string
  RETURN w & "*" & h
END OPERATOR

OPERATOR XYPair.+= (rhs as XYPair)
  x += rhs.x
  y += rhs.y
END OPERATOR

OPERATOR XYPair.LET (value as integer)
  x = value
  y = value
END OPERATOR

OPERATOR XYPair.LET (rhs as Float2)
  x = rhs.x
  y = rhs.y
END OPERATOR

OPERATOR + (lhs as XYPair, rhs as XYPair) as XYPair
  RETURN TYPE(lhs.x + rhs.x, lhs.y + rhs.y)
END OPERATOR

OPERATOR + (lhs as XYPair, rhs as integer) as XYPair
  RETURN TYPE(lhs.x + rhs, lhs.y + rhs)
END OPERATOR

OPERATOR - (lhs as XYPair, rhs as XYPair) as XYPair
  RETURN TYPE(lhs.x - rhs.x, lhs.y - rhs.y)
END OPERATOR

OPERATOR - (lhs as XYPair, rhs as integer) as XYPair
  RETURN TYPE(lhs.x - rhs, lhs.y - rhs)
END OPERATOR

OPERATOR * (lhs as XYPair, rhs as XYPair) as XYPair
  RETURN TYPE(lhs.x * rhs.x, lhs.y * rhs.y)
END OPERATOR

OPERATOR * (lhs as XYPair, rhs as integer) as XYPair
  RETURN TYPE(lhs.x * rhs, lhs.y * rhs)
END OPERATOR

OPERATOR * (lhs as XYPair, rhs as double) as XYPair
  RETURN TYPE(lhs.x * rhs, lhs.y * rhs)
END OPERATOR

OPERATOR \ (lhs as XYPair, rhs as XYPair) as XYPair
  RETURN TYPE(lhs.x \ rhs.x, lhs.y \ rhs.y)
END OPERATOR

OPERATOR \ (lhs as XYPair, rhs as integer) as XYPair
  RETURN TYPE(lhs.x \ rhs, lhs.y \ rhs)
END OPERATOR

OPERATOR / (lhs as XYPair, rhs as XYPair) as XYPair
  RETURN TYPE(lhs.x / rhs.x, lhs.y / rhs.y)
END OPERATOR

OPERATOR / (lhs as XYPair, rhs as double) as XYPair
  RETURN TYPE(lhs.x / rhs, lhs.y / rhs)
END OPERATOR

OPERATOR ABS (lhs as XYPair) as XYPair
  RETURN TYPE(ABS(lhs.x), ABS(lhs.y))
END OPERATOR

OPERATOR MOD (lhs as XYPair, rhs as XYPair) as XYPair
  RETURN TYPE(lhs.x MOD rhs.x, lhs.y MOD rhs.y)
END OPERATOR

OPERATOR MOD (lhs as XYPair, rhs as integer) as XYPair
  RETURN TYPE(lhs.x MOD rhs, lhs.y MOD rhs)
END OPERATOR

OPERATOR - (lhs as XYPair) as XYPair
  RETURN TYPE(-lhs.x, -lhs.y)
END OPERATOR


OPERATOR Float2.CAST () as string
  RETURN x & "," & y
END OPERATOR

OPERATOR Float2.+= (rhs as Float2)
  x += rhs.x
  y += rhs.y
END OPERATOR

OPERATOR = (lhs as Float2, rhs as Float2) as bool
  RETURN lhs.x = rhs.x andalso lhs.y = rhs.y
END OPERATOR

OPERATOR <> (lhs as Float2, rhs as Float2) as bool
  RETURN lhs.x <> rhs.x orelse lhs.y <> rhs.y
END OPERATOR

OPERATOR + (lhs as Float2, rhs as Float2) as Float2
  RETURN TYPE(lhs.x + rhs.x, lhs.y + rhs.y)
END OPERATOR

OPERATOR + (lhs as Float2, rhs as double) as Float2
  RETURN TYPE(lhs.x + rhs, lhs.y + rhs)
END OPERATOR

OPERATOR - (lhs as Float2, rhs as Float2) as Float2
  RETURN TYPE(lhs.x - rhs.x, lhs.y - rhs.y)
END OPERATOR

OPERATOR - (lhs as Float2, rhs as double) as Float2
  RETURN TYPE(lhs.x - rhs, lhs.y - rhs)
END OPERATOR

OPERATOR * (lhs as Float2, rhs as Float2) as Float2
  RETURN TYPE(lhs.x * rhs.x, lhs.y * rhs.y)
END OPERATOR

OPERATOR * (lhs as Float2, rhs as double) as Float2
  RETURN TYPE(lhs.x * rhs, lhs.y * rhs)
END OPERATOR

OPERATOR / (lhs as Float2, rhs as Float2) as Float2
  RETURN TYPE(lhs.x / rhs.x, lhs.y / rhs.y)
END OPERATOR

OPERATOR / (lhs as Float2, rhs as double) as Float2
  RETURN TYPE(lhs.x / rhs, lhs.y / rhs)
END OPERATOR


OPERATOR = (lhs as RectType, rhs as RectType) as bool
  RETURN memcmp(@lhs, @rhs, sizeof(RectType)) = 0
END OPERATOR

OPERATOR <> (lhs as RectType, rhs as RectType) as bool
  RETURN memcmp(@lhs, @rhs, sizeof(RectType)) <> 0
END OPERATOR

OPERATOR + (lhs as RectType, rhs as RectType) as RectType
  RETURN XYWH(lhs.x + rhs.x, lhs.y + rhs.y, lhs.wide + rhs.wide, lhs.high + rhs.high)
END OPERATOR

OPERATOR + (lhs as RectType, rhs as XYPair) as RectType
  RETURN XYWH(lhs.x + rhs.x, lhs.y + rhs.y, lhs.wide, lhs.high)
END OPERATOR

OPERATOR - (lhs as RectType, rhs as XYPair) as RectType
  RETURN XYWH(lhs.x - rhs.x, lhs.y - rhs.y, lhs.wide, lhs.high)
END OPERATOR

OPERATOR * (lhs as RectType, rhs as integer) as RectType
  RETURN XYWH(lhs.x * rhs, lhs.y * rhs, lhs.wide * rhs, lhs.high * rhs)
END OPERATOR

OPERATOR RectType.CAST () as string
  RETURN x & "," & y & ",w" & wide & ",h" & high
END OPERATOR

FUNCTION xypair_direction (v as XYPair, byval axis as integer, byval default as DirNum = dirNone) as DirNum
 IF axis = 0 THEN
  IF v.x < 0 THEN RETURN dirLeft
  IF v.x > 0 THEN RETURN dirRight
 ELSEIF axis = 1 THEN
  IF v.y < 0 THEN RETURN dirUp
  IF v.y > 0 THEN RETURN dirDown
 END IF
 RETURN default
END FUNCTION

FUNCTION xypair_to_direction (v as XYPair) as DirNum
 IF v.x = 0 AND v.y = 0 THEN RETURN dirNone
 IF ABS(v.x) > ABS(v.y) THEN
  IF v.x < 0 THEN RETURN dirLeft
  IF v.x > 0 THEN RETURN dirRight
 ELSE
  IF v.y < 0 THEN RETURN dirUp
  IF v.y > 0 THEN RETURN dirDown
 END IF
END FUNCTION

SUB xypair_move (v as XYPair, byval direction as integer, byval amount as integer = 1)
 SELECT CASE direction
  CASE 0: v.y -= amount
  CASE 1: v.x += amount
  CASE 2: v.y += amount
  CASE 3: v.x -= amount
  CASE ELSE
   showbug "xypair_move: invalid direction " & direction
 END SELECT
END SUB

FUNCTION xypair_manhattan_distance(v1 as XYPair, v2 as XYPair) as integer
 DIM diff as XYPair = v2 - v1
 RETURN ABS(diff.x) + ABS(diff.y)
END FUNCTION

FUNCTION xypair_distance_squared(v1 as XYPair, v2 as XYPair) as integer
 DIM diff as XYPair = v2 - v1
 RETURN diff.x * diff.x + diff.y * diff.y
END FUNCTION

#IFDEF __FB_MAIN__
startTest(XYPairOperators)
  DIM as XYPair A = (1,2), B = (3,4)

  IF A <> A THEN fail
  IF A <> TYPE<XYPair>(1,2) THEN fail
  IF XY(101,-34) <> TYPE<XYPair>(101,-34) THEN fail
  IF (A = XY(1,2)) <> YES THEN fail

  IF A < 2 THEN fail
  IF NOT A < 3 THEN fail
  IF NOT A > 0 THEN fail
  IF NOT A >= 1 THEN fail
  IF A >= 2 THEN fail

  A += B
  IF A <> XY(4,6) THEN fail
  IF -A <> XY(-4,-6) THEN fail
  IF A + 4 <> XY(8,10) THEN fail
  IF A - A <> XY(0,0) THEN fail
  IF A * 10 <> XY(40,60) THEN fail
  IF A * B <> XY(12,24) THEN fail
  IF A \ 3 <> XY(1,2) THEN fail
  IF A \ XY(2,-1) <> XY(2,-6) THEN fail
  IF A * 5 \ 5 <> A THEN fail
  IF A / 3 <> XY(1,2) THEN fail
  IF A / 4 <> XY(1,2) THEN fail
  IF A / 1.5 <> XY(3,4) THEN fail
  IF A / XY(-1,4) <> XY(-4,2) THEN fail
  IF STR(A) <> "4,6" THEN fail
  IF STR(A.wh) <> "4*6" THEN fail
  IF ABS(XY(-4,-5)) <> XY(4,5) THEN fail
  IF A MOD 3 <> XY(1,0) THEN fail
  IF A MOD XY(3,5) <> XY(1,1) THEN fail
endTest

startTest(RectTypeOperators)
  DIM as RectType A = (1,2,1,1), B = (1,2,1,0)

  IF A <> A THEN fail
  IF A = B THEN fail
endTest
#ENDIF

FUNCTION dirX(dirn as DirNum, dist as integer = 1) as integer
 IF dirn = dirLeft THEN RETURN -dist
 IF dirn = dirRight THEN RETURN -dist
END FUNCTION

FUNCTION dirY(dirn as DirNum, dist as integer = 1) as integer
 IF dirn = dirUp THEN RETURN -dist
 IF dirn = dirDown THEN RETURN -dist
END FUNCTION

'------------- Math operations -------------

FUNCTION bitcount (byval v as unsigned integer) as integer
  'From the "Software Optimization Guide for AMD Athlon 64 and Opteron Processors". Thanks, AMD!
  v = v - ((v SHR 1) AND &h55555555)
  v = (v AND &h33333333) + ((v SHR 2) AND &h33333333)
  RETURN ((v + (v SHR 4) AND &hF0F0F0F) * &h1010101) SHR 24
END FUNCTION

FUNCTION ceiling (byval n as integer) as integer
 RETURN INT(n * -1) * -1
END FUNCTION

FUNCTION bound (byval n as integer, byval lowest as integer, byval highest as integer) as integer
 bound = n
 IF n < lowest THEN bound = lowest
 IF n > highest THEN bound = highest
END FUNCTION

FUNCTION bound (byval n as longint, byval lowest as longint, byval highest as longint) as longint
 bound = n
 IF n < lowest THEN bound = lowest
 IF n > highest THEN bound = highest
END FUNCTION

FUNCTION bound (byval n as double, byval lowest as double, byval highest as double) as double
 bound = n
 IF n < lowest THEN bound = lowest
 IF n > highest THEN bound = highest
END FUNCTION

FUNCTION bound (point as XYPair, lefttop as XYPair, rightbottom as XYPair) as XYPair
 DIM ret as XYPair = any
 ret.x = IIF(point.x > rightbottom.x, rightbottom.x, IIF(point.x < lefttop.x, lefttop.x, point.x))
 ret.y = IIF(point.y > rightbottom.y, rightbottom.y, IIF(point.y < lefttop.y, lefttop.y, point.y))
 RETURN ret
END FUNCTION

FUNCTION in_bound (byval n as integer, byval lowest as integer, byval highest as integer) as integer
 RETURN (n >= lowest) AND (n <= highest)
END FUNCTION

'Clamp a value to within a range with warning
SUB clamp_value (byref value as integer, byval min as integer, byval max as integer, argname as string)
 DIM oldval as integer = value
 IF value < min THEN value = min
 IF value > max THEN value = max
 IF value <> oldval THEN debug "Clamped invalid " + argname + " value " & oldval & " to " & value
END SUB

FUNCTION large (byval n1 as integer, byval n2 as integer) as integer
 large = n1
 IF n2 > n1 THEN large = n2
END FUNCTION

FUNCTION large (byval n1 as longint, byval n2 as longint) as longint
 large = n1
 IF n2 > n1 THEN large = n2
END FUNCTION

FUNCTION large (byval n1 as double, byval n2 as double) as double
 IF n2 > n1 THEN RETURN n2 ELSE RETURN n1
END FUNCTION

FUNCTION large (xy1 as XYPair, xy2 as XYPair) as XYPair
 DIM ret as XYPair = any
 ret.x = IIF(xy1.x < xy2.x, xy2.x, xy1.x)
 ret.y = IIF(xy1.y < xy2.y, xy2.y, xy1.y)
 RETURN ret
END FUNCTION

SUB loopvar (byref value as integer, min as integer, max as integer, inc as integer = 1)
 value = POSMOD((value + inc) - min, (max - min) + 1) + min
END SUB

SUB loopvar (byref value as longint, min as longint, max as longint, inc as longint = 1)
 value = POSMOD((value + inc) - min, (max - min) + 1) + min
END SUB

FUNCTION small (byval n1 as integer, byval n2 as integer) as integer
 small = n1
 IF n2 < n1 THEN small = n2
END FUNCTION

FUNCTION small (byval n1 as longint, byval n2 as longint) as longint
 small = n1
 IF n2 < n1 THEN small = n2
END FUNCTION

FUNCTION small (byval n1 as double, byval n2 as double) as double
 IF n2 < n1 THEN RETURN n2 ELSE RETURN n1
END FUNCTION

FUNCTION small (xy1 as XYPair, xy2 as XYPair) as XYPair
 DIM ret as XYPair = any
 ret.x = IIF(xy1.x > xy2.x, xy2.x, xy1.x)
 ret.y = IIF(xy1.y > xy2.y, xy2.y, xy1.y)
 RETURN ret
END FUNCTION

' Split a RelPos into offset, alignment, and anchor.
SUB RelPos_decode(pos as RelPos, byref offset as integer, byref align as AlignType, byref anchor as AlignType, byref show as AlignType)
 DIM as integer highpart, lowpart
 lowpart = ((pos + _rMargin) MOD _rFactor) - _rMargin
 highpart = (pos + _rMargin) \ _rFactor
 IF highpart >= 0 AND highpart < 27 AND ABS(lowpart) <= _rMargin THEN
  offset = lowpart
  align = (highpart MOD 9) \ 3
  anchor = highpart MOD 3
  IF highpart >= 18 THEN
   show = alignRight
  ELSEIF highpart >= 9 THEN
   show = alignLeft
  ELSE
   show = alignCenter
  END IF
 ELSE
  offset = pos
  align = alignLeft
  anchor = alignLeft
  show = alignCenter  'None
 END IF
END SUB

' Converts a RelPos value like "rCenter + 30" to "width\2 + 30", and so forth,
' for combinations of at most one of
'   rTop, rLeft, rRight, rCenter, rBottom, rWidth, rHeight,
'   (which edge of the screen this position is relative to)
' and at most one of
'   ancTop, ancLeft, ancRight, ancCenter, ancBottom,
'   (which edge of this object the position describes)
' (although in some contexts, like when specifying width of a rect,
' anchor constants don't make sense and do nothing).
' Finally, showLeft, showRight can be added to clip the position so that either
' the left-most or right-most part of the object is on-screen
FUNCTION relative_pos(pos as RelPos, pagewidth as integer, objwidth as integer = 0) as integer
 DIM offset as integer
 DIM as AlignType align, anchor, show
 RelPos_decode pos, offset, align, anchor, show
 IF align  = alignCenter THEN offset += pagewidth \ 2
 IF align  = alignRight  THEN offset += pagewidth
 IF anchor = alignCenter THEN offset -= objwidth \ 2
 IF anchor = alignRight  THEN offset -= objwidth
 ' show = alignCenter means no clipping
 IF show = alignRight   THEN offset = large(offset, 0)    'Don't go over left screen edge
 IF show <> alignCenter THEN offset = small(offset, pagewidth - objwidth) 'Don't go over right screen edge
 IF show = alignLeft    THEN offset = large(offset, 0)    'Don't go over left screen edge
 RETURN offset
END FUNCTION

FUNCTION relative_pos(pos as RelPosXY, pagesize as XYPair, objsize as XYPair = XY(0,0)) as XYPair
 DIM ret as XYPair = pos
 ret.x = relative_pos(pos.x, pagesize.x, objsize.x)
 ret.y = relative_pos(pos.y, pagesize.y, objsize.y)
 RETURN ret
END FUNCTION

#IFDEF __FB_MAIN__
startTest(RelPos)
  DIM offset as integer
  DIM as AlignType align, anchor, show
  RelPos_decode rCenter + ancRight - 12359 + showLeft, offset, align, anchor, show
  testEqual(offset, -12359)
  testEqual(align, alignCenter)
  testEqual(anchor, alignRight)
  testEqual(show, alignLeft)

  'Test that these values close to a power of 10 aren't treated as containing flags, as documented
  DIM testvals(...) as integer = {INT_MAX, 99999, 999999, 9999999, 999999999, 1050000, -1, -50000, -60050000}
  FOR i as integer = 0 TO UBOUND(testvals)
    RelPos_decode testvals(i), offset, align, anchor, show
    testEqual(offset, testvals(i))
    testEqual(align, alignLeft)
    testEqual(anchor, alignLeft)
    testEqual(show, alignCenter)
  NEXT

  testEqual(rCenter + rCenter, rRight)
  testEqual(rRight - rLeft, rRight)
  testEqual((rRight + 100) \ 2, rCenter + 50)
   
  testEqual(relative_pos(0, 0, 0), 0)
  testEqual(relative_pos(-3499, 1000), -3499)
  testEqual(relative_pos(rLeft + 1312334, 1000), 1312334)
  testEqual(relative_pos(rCenter + 50000, 20000), 60000)
  testEqual(relative_pos(rCenter - 1000, 20000), 9000)
  testEqual(relative_pos(rRight - 50000, 200042), 150042)
  testEqual(relative_pos(rRight - 20000, 20000), 0)
  testEqual(relative_pos(ancCenter - 90, 20000, 0), -90)
  testEqual(relative_pos(ancCenter - 90, 20000, 100), -140)
  testEqual(relative_pos(ancRight  - 90, 20000, 100), -190)
  testEqual(relative_pos(rCenter + ancCenter + 9, 20100, 100), 10009)
  testEqual(relative_pos(rCenter + ancRight + 9, 20200, 100), 10009)
  testEqual(relative_pos(rRight + ancRight + 9, 20200, 100), 20109)
  testEqual(relative_pos(rRight + ancCenter + 9, 20200, 100), 20159)

  testEqual(relative_pos(67 + showLeft, 100, 10), 67)
  testEqual(relative_pos(67 + showRight, 100, 10), 67)
  testEqual(relative_pos(showRight, 100, 110), -10)
  testEqual(relative_pos(rRight + showLeft, 100, 10), 90)
  testEqual(relative_pos(rRight + showRight, 100, 10), 90)
  testEqual(relative_pos(rRight + showRight, 100, 1000), -900)
  testEqual(relative_pos(rRight + showLeft - 50, 100, 10), 50)
  testEqual(relative_pos(rRight + showRight - 50, 100, 10), 50)
  testEqual(relative_pos(rCenter + showLeft, 100, 1000), 0)
  testEqual(relative_pos(rCenter + showRight, 100, 1000), -900)
  testEqual(relative_pos(rRight + ancRight - 120 + showLeft, 100, 1000), 0)
  testEqual(relative_pos(rRight + ancRight - 120 + showRight, 100, 1000), -900)

  'Test RelPosXY overload
  testEqual(relative_pos(XY(rLeft + 1312334, rCenter + 50000), XY(1000, 20000)), XY(1312334, 60000))
endtest
#ENDIF

'Find dimensions of a rect, given opposite corners 
SUB corners_to_rect (p1 as XYPair, p2 as XYPair, result as RectType)
 IF p1.x < p2.x THEN result.x = p1.x ELSE result.x = p2.x
 result.wide = ABS(p1.x - p2.x)
 IF p1.y < p2.y THEN result.y = p1.y ELSE result.y = p2.y
 result.high = ABS(p1.y - p2.y)
END SUB

'As above, but include the corner positions inside the rect (for use when coordinates measure on a discrete grid)
SUB corners_to_rect_inclusive (p1 as XYPair, p2 as XYPair, result as RectType)
 corners_to_rect p1, p2, result
 result.wide += 1
 result.high += 1
END SUB

FUNCTION rect_collide_point (r as RectType, p as XYPair) as bool
 RETURN p.x >= r.x ANDALSO p.y >= r.y ANDALSO p.x < r.x + r.wide ANDALSO p.y < r.y + r.high
END FUNCTION

FUNCTION rect_collide_rect (r1 as RectType, r2 as RectType) as bool
 IF r1.w + r2.w <= ABS(2 * r1.x + r1.w - 2 * r2.x - r2.w) THEN RETURN NO
 IF r1.h + r2.h <= ABS(2 * r1.y + r1.h - 2 * r2.y - r2.h) THEN RETURN NO
 RETURN YES
END FUNCTION

FUNCTION rect_collide_point_vertical_chunk (r as RectType, p as XYPair, chunk_spacing as integer) as integer
 'Divide a rect into vertical chunks (like a menu) and return the
 'index of the one the point collides with. Returns -1 if none collide
 IF chunk_spacing = 0 THEN debug "divide by 0: chunk_spacing=0" : RETURN -1
 IF rect_collide_point(r, p) THEN
  RETURN (p.y - r.y) \ chunk_spacing
 END IF
 RETURN -1
END FUNCTION

FUNCTION rando () as double
 'STATIC count as integer = 0
 'This is a simple wrapper for RND to facilitate debugging
 DIM n as double = RND
 'debug count & " RND=" & n
 'count += 1
 RETURN n
END FUNCTION

FUNCTION randint (byval limit as integer) as integer
 'Returns a random integer >=0 and < limit
 RETURN INT(rando() * limit)
END FUNCTION

'Simple low quality psuedo random number generator with exposed state. Same speed as RND.
'Initialise prng_state to a seed before first call.
'Returns a float in the range [0.0, 1.0) but no where near a full double of precision.
FUNCTION simple_rand (byref prng_state as uinteger) as double
 prng_state = (prng_state * 1103515245 + 12345)
 RETURN CAST(double, prng_state) * (1.0 / &hffffffffU)
END FUNCTION

'Simple low quality psuedo random number generator. Initialise prng_state to a seed before first call.
'Returns an integer in the range 0 to limit - 1. limit should be <= 2^20
FUNCTION simple_randint (byref prng_state as uinteger, byval limit as integer) as uinteger
 prng_state = (prng_state * 1103515245 + 12345)
 RETURN CINT((CAST(longint, prng_state) * limit) SHR 32)
END FUNCTION

'simple_rand simple test: create a bmp
/'
 DIM timestart as double = TIMER
 DIM tframe as frame ptr = frame_new(256, 256)
 DIM pstate as unsigned integer = 0
 FOR yy as integer = 0 to 255
  FOR xx as integer = 0 to 255
   putpixel tframe, xx, yy, simple_randint(pstate, 16) * 15
   'putpixel tframe, xx, yy, CINT(simple_rand(pstate) * 16) * 15
  NEXT
 NEXT
 frame_export_bmp8 "randtest.bmp", tframe, master()
 debug "testframe in " & (TIMER - timestart) * 1000 & "ms"
 frame_unload @tframe
'/

SUB reseed_prng (seed as double)
 'Note: the fractional part of seed is ignored!
 RANDOMIZE seed, 3
END SUB

' Returns number +/- up to percent%
' Note that the average value returned is number - 0.5! Don't change this
' without adjusting attack damage calculation
FUNCTION range (number as integer, percent as integer) as integer
 DIM a as longint
 a = (number / 100) * percent
 ' number - a <= RETURN < number + a
 a = number + INT((rando() * a) * 2) - a
 RETURN bound(a, -2147483648LL, 2147483647LL)
END FUNCTION

FUNCTION isnan (byval value as double) as integer
 RETURN value <> value
END FUNCTION

FUNCTION isnan (byval value as single) as integer
 RETURN value <> value
END FUNCTION

FUNCTION isfinite (byval value as double) as integer
 RETURN DBL_MAX >= value AND value >= -DBL_MAX
END FUNCTION

FUNCTION isfinite (byval value as single) as integer
 RETURN FLT_MAX >= value AND value >= -FLT_MAX
END FUNCTION

'A fuzzy equivalent to 'iif(value >= low+high/2, 1.0, 0.0)'
'Swap low,high to reverse the comparison
FUNCTION fuzzythreshold (byval value as double, byval low as double, byval high as double) as double
 IF low > high THEN
  low = -low
  high = -high
  value = -value
 END IF
 IF value <= low THEN
  RETURN 0.0
 ELSEIF value >= high THEN
  RETURN 1.0
 ELSE
  RETURN (value - low) / (high - low)
 END IF
END FUNCTION


'---------------- String operations --------------


'strprintf is actually defined in miscc.c, but test it here
#IFDEF __FB_MAIN__
startTest(strprintf)
 IF strprintf("") <> "" THEN fail
 IF strprintf(" ") <> " " THEN fail
 IF strprintf(" %d ", 23) <> " 23 " THEN fail
 IF strprintf(" %s ", @"42") <> " 42 " THEN fail
 IF strprintf(" %d %d%d", 1, 2, -3) <> " 1 2-3" THEN fail
 IF strprintf("%04dd %.2f %s %%", -1, 2.4, @",") <> "-001d 2.40 , %" THEN fail
 IF strprintf("%-4d", -23) <> "-23 " THEN fail
 IF strprintf("%-04d", -23) <> "-23 " THEN fail
 IF strprintf("%0-4d", -23) <> "-23 " THEN fail
endTest
#ENDIF

'FB will usually produce a NULL ptr when converting an empty string to a zstring ptr,
'which is not acceptable in C
FUNCTION cstring (s as string) as zstring ptr
 DIM ret as zstring ptr = strptr(s)
 IF ret = NULL THEN RETURN strptr("")
 RETURN ret
END FUNCTION

'Allocate a copy of a null-terminated zstring. In case you don't want to use FB strings for some reason.
FUNCTION copy_zstring (str_ptr as zstring ptr) as zstring ptr
 DIM ret as zstring ptr = allocate(strlen(str_ptr) + 1)
 strcpy(ret, str_ptr)
 RETURN ret
END FUNCTION

'FB's string assignment will always do a strlen on zstring arguments, (even if you call fb_StrAssign
'manually with the right length!) so if the source is a binary blob containing null bytes then
'this function should be used instead.
FUNCTION blob_to_string (byval str_ptr as zstring ptr, byval str_len as integer) as string
 DIM ret as string
 'Consider this a testcase for "Can you trust TMC to implement a FreeBASIC compiler?"
 'OK, OK, you could just do ret = SPACE(str_len), but this is faster
 fb_hStrAllocTemp(@ret, str_len)
 memcpy(@ret[0], str_ptr, str_len)
 ret[str_len] = 0
 RETURN ret
END FUNCTION

'Add padding to the right of a string, and possible clip it to 'size'
FUNCTION rpad (s as string, pad_char as zstring ptr = @" ", size as integer, clip as clipDir = clipNone) as string
 IF clip = clipNone AND LEN(s) >= size THEN RETURN s
 DIM temp as string
 temp = IIF(clip = clipRight, LEFT(s, size), RIGHT(s, size))
 RETURN temp & STRING(size - LEN(temp), *pad_char)
END FUNCTION

'Add padding to the left of a string, and possible clip it to 'size'
FUNCTION lpad (s as string, pad_char as zstring ptr = @" ", size as integer, clip as clipDir = clipNone) as string
 IF clip = clipNone AND LEN(s) >= size THEN RETURN s
 DIM temp as string
 temp = IIF(clip = clipRight, LEFT(s, size), RIGHT(s, size))
 RETURN STRING(size - LEN(temp), *pad_char) & temp
END FUNCTION

' First pad right, then pad left
FUNCTION rlpad (s as string, pad_char as zstring ptr = @" ", pad_right as integer, pad_left as integer, clip as clipDir = clipNone) as string
 RETURN lpad(rpad(s, pad_char, pad_right, NO), pad_char, pad_left, clip)
END FUNCTION

'Like INSTR, but return the n-th match
'Returns 0 if not found
FUNCTION Instr_nth (byval start as integer, s as string, substring as string, byval nth as integer = 1) as integer
 DIM temp as integer = start - 1
 IF nth < 1 THEN RETURN 0
 FOR n as integer = 1 TO nth
  temp = INSTR(temp + 1, s, substring)
  IF temp = 0 THEN RETURN 0
 NEXT
 RETURN temp
END FUNCTION

'Like INSTR without start point, but return the n-th match
'Returns 0 if not found
FUNCTION Instr_nth (s as string, substring as string, byval nth as integer = 1) as integer
 RETURN Instr_nth(1, s, substring, nth)
END FUNCTION

'Returns the number of characters at the start of two strings that are equal
FUNCTION length_matching(s1 as string, s2 as string) as integer
 DIM as byte ptr p1 = @s1[0], p2 = @s2[0]
 DIM as integer ret = 0
 WHILE *p1 AND *p2
  IF *p1 <> *p2 THEN RETURN ret
  p1 += 1
  p2 += 1
  ret += 1
 WEND
 RETURN ret
END FUNCTION

'Advance 'idx' (1-based) over copies of tok. Returns number of copies skipped over.
'If tok is more than one character long, only skips over whole matches.
FUNCTION skip_over(text as string, byref idx as integer, tok as zstring ptr, maxskips as integer = -1) as integer
  IF LEN(text) = 0 THEN RETURN 0  'Protect against zero-length strings with null pointers
  DIM ret as integer = 0
  DIM toklen as integer = strlen(tok)
  WHILE strncmp(@text[idx - 1], tok, toklen) = 0
    idx += toklen
    ret += 1
    IF ret = maxskips THEN EXIT WHILE
  WEND
  RETURN ret
END FUNCTION

'Try to parse a string into an int, returning true on success and optionally
'putting the results in *ret (*ret is unmodified on failure).
'This is stricter than VALINT: the string must be composed only of digits and
'possible leading - and leading space.  If strict=YES, then leading spaces and
'leading zeroes (but not -0) are also rejected.
FUNCTION parse_int (stri as zstring ptr, ret as integer ptr=NULL, strict as bool=NO) as bool
 IF stri = NULL THEN RETURN NO  'Empty string
 DIM s as unsigned byte ptr = stri

 IF strict = NO THEN
  's = LTRIM(s)
  WHILE isspace(s[0]): s += 1 : WEND
 END IF
 IF s[0] = 0 THEN RETURN NO  'length 0

 DIM sign as integer = 1
 IF s[0] = ASC("-") THEN
  sign = -1
  s += 1  's = MID(s, 2)
  IF s[0] = 0 THEN RETURN NO  'length 0
 END IF

 'Reject leading zeroes (check length is >= 2)
 IF strict ANDALSO s[0] = ASC("0") ANDALSO s[1] THEN RETURN NO

 DIM n as integer = 0
 WHILE s[0]
  DIM c as integer = s[0] - 48
  s += 1
  IF c >= 0 AND c <= 9 THEN
   n = n * 10 + (c * sign)
  ELSE
   RETURN NO
  END IF
 WEND

 IF ret THEN *ret = n
 RETURN YES
END FUNCTION

'Use this in contrast to FB's VALINT.
'It is stricter, and returns a default on failure: the string
'must be composed only of digits and possible leading - and leading space.
'If strict=YES, then leading spaces and leading zeroes (but not -0) are rejected.
FUNCTION str2int (stri as zstring ptr, default as integer=0, strict as bool=NO) as integer
 DIM ret as integer
 IF parse_int(stri, @ret, strict) THEN RETURN ret
 RETURN default
END FUNCTION

'Split a string composed of alphabetic text and an integer concatenated without
'whitespace, like "H32" or "Font-1" into action ("Font") and arg (-1).
'Returns false if isn't strictly formatted like that (eg whitespace anywhere, or eg "H00")
FUNCTION split_str_int(z as zstring ptr, byref action as string, byref arg as integer) as bool
 'Written to avoid temporary strings, for speed
 IF z = NULL THEN RETURN NO
 DIM chidx as integer
 FOR chidx = 0 TO LEN(z) - 1
  IF isalpha(CAST(ubyte ptr, z)[chidx]) = 0 THEN EXIT FOR
 NEXT
 IF chidx = 0 THEN RETURN NO
 action = LEFT(*z, chidx)
 RETURN parse_int(z + chidx, @arg, YES)  'strict=YES
END FUNCTION

#IFDEF __FB_MAIN__
startTest(parse_int)
 DIM n as integer
 IF parse_int(NULL) THEN fail
 IF parse_int("") THEN fail
 IF parse_int(" ") THEN fail
 IF parse_int("-") THEN fail
 IF parse_int("2 ", @n) THEN fail
 IF parse_int(" - 2", @n) THEN fail
 IF parse_int("1 2", @n) THEN fail
 IF parse_int("1") = NO THEN fail
 IF parse_int("00", @n) = NO ORELSE n <> 0 THEN fail
 IF parse_int("-001234", @n) = NO ORELSE n <> -1234 THEN fail
 IF parse_int("2147483647", @n) = NO ORELSE n <> 2147483647 THEN fail
 IF parse_int("-2147483648", @n) = NO ORELSE n <> -2147483648 THEN fail
 IF parse_int("0", @n) = NO ORELSE n <> 0 THEN fail
 IF parse_int("-0", @n) = NO ORELSE n <> 0 THEN fail
 IF parse_int(" -2", @n) = NO ORELSE n <> -2 THEN fail

 'Test strictness
 IF parse_int("00", , YES) THEN fail
 IF parse_int("01", , YES) THEN fail
 IF parse_int("-001234", , YES) THEN fail
 IF parse_int(" 2", , YES) THEN fail
 IF parse_int(" -2", , YES) THEN fail
 IF parse_int("0", @n, YES) = NO ORELSE n <> 0 THEN fail
 IF parse_int("-0", @n, YES) = NO ORELSE n <> 0 THEN fail
endTest
#ENDIF

'Lenient, accepts any number or the strings yes, no, true, false, on, off.
'Otherwise returns default.
FUNCTION str2bool(q as string, default as integer = NO) as integer
 DIM v as string = TRIM(LCASE(q))
 IF LEN(v) = 0 THEN RETURN default
 IF v = "yes" ORELSE v = "true" ORELSE v = "on" THEN RETURN YES
 IF v = "no" ORELSE v = "false" ORELSE v = "off" THEN RETURN NO
 DIM value as integer
 IF parse_int(v, @value) THEN RETURN value <> 0
 RETURN default
END FUNCTION

'Ancient password encryption/decryption function
FUNCTION rotascii (s as string, o as integer) as string
 DIM as string temp = ""
 FOR i as integer = 1 TO LEN(s)
  temp &= CHR(POSMOD(ASC(MID(s, i, 1)) + o, 256))
 NEXT i
 RETURN temp
END FUNCTION

'Capitalise first letter, lower-case the rest. Assumes one word.
FUNCTION titlecase(word as string) as string
 RETURN UCASE(LEFT(word, 1)) & LCASE(MID(word, 2))
END FUNCTION

FUNCTION escape_string(s as string, chars as string) as string
 DIM i as integer
 DIM c as string
 DIM result as string
 result = ""
 FOR i = 1 to LEN(s)
  c = MID(s, i, 1)
  IF INSTR(chars, c) THEN
   result = result & "\"
  END IF
  result = result & c
 NEXT i
 RETURN result
END FUNCTION

'Replace occurrences of a substring with the result of replacefun. Modifies 'buffer'!
'Returns the number of replacements done. Inserted text is not eligible for further replacements.
'Optionally limit the number of times to do the replacement by passing maxtimes; no limit if < 0
FUNCTION replacestr (byref buffer as string, replacewhat as string, replacefunc as FnReplacement, arg as any ptr, maxtimes as integer = -1, caseinsensitive as bool = NO) as integer
 DIM pt as integer
 DIM count as integer
 DIM start as integer = 1

 WHILE maxtimes < 0 OR count < maxtimes
  IF caseinsensitive THEN
   pt = INSTR(start, LCASE(buffer), LCASE(replacewhat))  'inefficient
  ELSE
   pt = INSTR(start, buffer, replacewhat)
  END IF
  IF pt = 0 THEN RETURN count
  DIM withwhat as string = replacefunc(MID(buffer, pt, LEN(replacewhat)), arg)
  buffer = MID(buffer, 1, pt - 1) + withwhat + MID(buffer, pt + LEN(replacewhat))
  start = pt + LEN(withwhat)
  count += 1
 WEND
 RETURN count
END FUNCTION

LOCAL FUNCTION _get_replacement(original as string, arg as any ptr) as string
 RETURN *CAST(string ptr, arg)
END FUNCTION

'Replace occurrences of a substring with the result of replacefun. Modifies 'buffer'!
'Returns the number of replacements done. Inserted text is not eligible for further replacements.
'Optionally limit the number of times to do with replacement by passing maxtimes; no limit if < 0
FUNCTION replacestr (byref buffer as string, replacewhat as string, withwhat as string, maxtimes as integer = -1, caseinsensitive as bool = NO) as integer
 RETURN replacestr(buffer, replacewhat, @_get_replacement, @withwhat, maxtimes, caseinsensitive)
END FUNCTION

' Change the type of newline in a string, which might contain mixed win/unix line endings
' (but doesn't current support Mac style \r line endings)
' newline is usually either !"\n" or !"\r\n", but could be anything. eg. " ".
FUNCTION normalize_newlines (buffer as string, newline as string = LINE_END) as string
 DIM ret as string = buffer
 IF newline = !"\n" THEN
  replacestr(ret, !"\r\n", newline)
 ELSE
  replacestr(ret, !"\r\n", !"\n")
  replacestr(ret, !"\n", newline)
 END IF
 RETURN ret
END FUNCTION

#IFDEF __FB_MAIN__
startTest(normalize_newlines)
 IF normalize_newlines(!"a\n b\r\n", !"\n") <> !"a\n b\n" THEN fail
 IF normalize_newlines(!"a\n b\r\n", !"\r\n") <> !"a\r\n b\r\n" THEN fail
 IF normalize_newlines(!"a\n b\r\n", " ") <> !"a  b " THEN fail
endTest
#ENDIF

FUNCTION exclude (s as string, x as string) as string
 DIM ret as string = ""
 FOR i as integer = 1 TO LEN(s)
  DIM tmp as string = MID(s, i, 1)
  IF INSTR(x, tmp) = 0 THEN
   ret &= tmp
  END IF
 NEXT i
 RETURN ret
END FUNCTION

FUNCTION exclusive (s as string, x as string) as string
 DIM ret as string = ""
 FOR i as integer = 1 TO LEN(s)
  DIM tmp as string = MID(s, i, 1)
  IF INSTR(x, tmp) THEN
   ret &= tmp
  END IF
 NEXT i
 RETURN ret
END FUNCTION

'------------- Stack -------------

SUB createstack (st as Stack)
  WITH st
    .size = STACK_SIZE_INC - 4
    .bottom = allocate(STACK_SIZE_INC * sizeof(integer))
    IF .bottom = 0 THEN
      'oh dear
      'debug "Not enough memory for stack"
      EXIT SUB
    END IF
    .pos = .bottom
  END WITH
END SUB

SUB destroystack (st as Stack)
  IF st.bottom <> 0 THEN
    deallocate st.bottom
    st.size = -1
  END IF
END SUB

SUB checkoverflow (st as Stack, byval amount as integer = 1)
  WITH st
    IF .pos - .bottom + amount >= .size THEN
      .size += STACK_SIZE_INC
      IF .size > STACK_SIZE_INC * 4 THEN .size += STACK_SIZE_INC
      'debug "new stack size = " & .size & " * 4  pos = " & (.pos - .bottom) & " amount = " & amount
      'debug "nowscript = " & nowscript & " " & scrat(nowscript).id & " " & scriptname(scrat(nowscript).id) 

      DIM newptr as integer ptr
      newptr = reallocate(.bottom, .size * sizeof(integer))
      IF newptr = 0 THEN
        'debug "stack: out of memory"
        EXIT SUB
      END IF
      .pos += newptr - .bottom
      .bottom = newptr
    END IF
  END WITH
END SUB

SUB setstackposition (st as Stack, byval position as integer)
  BUG_IF(position < 0 ORELSE position > stackposition(st), "invalid, " & position)
  st.pos = st.bottom + position
END SUB

'------------- Old allmodex stack  -------------

dim shared stackbottom as integer ptr
dim shared stackptr as integer ptr    'Where to put the next pushed dword
dim shared stacksize as integer = -1  'In dwords

SUB setupstack ()
	stacksize = 8192
	stackbottom = callocate(sizeof(integer) * stacksize)
	if stackbottom = 0 then
		'oh dear
		debug "Not enough memory for stack"
                stacksize = -1
		exit sub
	end if
	stackptr = stackbottom
end SUB

SUB pushdw (byval dword as integer)
	if stackptr - stackbottom >= stacksize then
		dim newptr as integer ptr
		stacksize += 8192
		newptr = reallocate(stackbottom, sizeof(integer) * stacksize)
		if newptr = 0 then
			debug "stack: out of memory"
			exit sub
		end if
		stackptr += newptr - stackbottom
		stackbottom = newptr
	end if
	*stackptr = dword
	stackptr += 1
end SUB

FUNCTION popdw () as integer
	dim pdw as integer

	if (stackptr >= stackbottom + 1) then
		stackptr -= 1
		pdw = *stackptr
	else
		pdw = 0
		showbug "Stack underflow"
	end if

	popdw = pdw
end FUNCTION

SUB releasestack ()
	if stacksize > 0 then
		deallocate stackbottom
		stacksize = -1
	end if
end SUB

'Number of dwords that have been pushed to the stack
FUNCTION stackpos () as integer
	stackpos = stackptr - stackbottom
end FUNCTION

'read an int from the stack relative to current position (eg -1 is last word pushed - off should be negative)
FUNCTION readstackdw (byval off as integer) as integer
	if stackptr + off >= stackbottom then
		return *(stackptr + off)
	end if
END FUNCTION

'------------- End allmodex stack -------------

FUNCTION sign_string(n as integer, neg_str as string, zero_str as string, pos_str as string) as string
 IF n < 0 THEN RETURN neg_str
 IF n > 0 THEN RETURN pos_str
 RETURN zero_str
END FUNCTION

'See also defaultint
FUNCTION zero_default(n as integer, default_caption as string="default") as string
 IF n = 0 THEN RETURN default_caption
 RETURN STR(n)
END FUNCTION

'See also zero_default
FUNCTION defaultint (n as integer, default_caption as string="default", default_value as integer=-1) as string
 IF n = default_value THEN RETURN default_caption
 RETURN STR(n)
END FUNCTION

FUNCTION blank_default(s as string, blankcaption as string="[default]") as string
 IF s = "" THEN RETURN blankcaption
 RETURN s
END FUNCTION

FUNCTION caption_or_int (captions() as string, n as integer) as string
 IF n >= LBOUND(captions) AND n <= UBOUND(captions) THEN RETURN captions(n)
 RETURN STR(n)
END FUNCTION

FUNCTION safe_caption(caption_array() as string, index as integer, description as string = "value") as string
 IF index >= LBOUND(caption_array) AND index <= UBOUND(caption_array) THEN
  RETURN caption_array(index)
 ELSE
  RETURN "Invalid " & description & " " & index
 END IF
END FUNCTION

'Overload for storing captions in a zstring ptr array, useful because unlike string arrays,
'those can be initialised (with  = {...}) at file scope..
'Because of https://sourceforge.net/p/fbc/bugs/666/ (fixed in FB 1.09) it's not possible to define an overload for
'safe_caption which takes a zstring ptr array, hence the different name.
FUNCTION safe_captionz(caption_array() as zstring ptr, index as integer, description as string = "value") as string
 IF index >= LBOUND(caption_array) AND index <= UBOUND(caption_array) THEN
  RETURN *caption_array(index)
 ELSE
  RETURN "Invalid " & description & " " & index
 END IF
END FUNCTION

'Returns a copy of the string with separators inserted, replacing spaces, so that there's at most 'wid'
'characters between separators; use together with split()
Function wordwrap(z as string, byval wid as integer, sep as string = chr(10)) as string
 dim as string ret, in
 in = z
 if len(in) <= wid then return in
 
 dim as integer i, j
 do
  'Need to add a separator? See if there's one already. Look up to one character past end of line
  for i = 1 to small(wid + 1, len(in))
   if mid(in, i, 1) = sep then
    ret &= left(in, i - 1) & sep
    in = mid(in, i + 1)
    continue do
   end if
  next
  
  if len(in) <= wid then
   'We reached the end of input, and it will fit on a line
   ret &= in
   in = ""
   exit do
  end if

  'Look for the last space in the second half of the line (ugly to wrap near the beginning of the line)
  for j = i - 1 to wid \ 2 step -1
   if mid(in, j, 1) = " " then
    'bingo! (separator overwrites the space)
    ret &= left(in, j - 1) & sep
    in = mid(in, j + 1)
    continue do
   end if
  next

  'No space found; the last word's too long, we need to cut it off
  ret &= left(in, wid) & sep
  in = mid(in, wid + 1)
 loop while in <> ""

 return ret
end function

'After calling wordwrap() and split(), this calculates the start position of each line
'in the lines array produced by split(), and puts the result in line_starts() after REDIM'ing it.
'Offsets are 0-based
sub split_line_positions(original_text as string, lines() as string, line_starts() as integer, sep as string = chr(10))
 dim offset as integer = 0
 redim line_starts(ubound(lines))
 dim idx as integer
 for idx = 0 TO ubound(lines)
  line_starts(idx) = offset
  offset += len(lines(idx))
  if offset >= len(original_text) then exit for
  if original_text[offset] = asc(" ") or original_text[offset] = asc(sep) then
   'This character was transformed to/is a seperator, and excluded by split()
   offset += 1
  end if
 next
 'Always exits early on last iteration
 if idx <> ubound(lines) or offset <> len(original_text) then
  showbug "split_line_positions buggy or called with bad args"
 end if
end sub

'Splits text at the separators; use together with wordwrap() to do wrapping
'sep must be length 1. ret() must be resizeable. If in == "", then ret() is redimmed to length 1.
sub split(in as string, ret() as string, sep as string = chr(10))
 redim ret(0)
 dim as integer i = 0, i2 = 1, j = 0
 i = instr(i2, in, sep)
 if i = 0 then
  ret(0) = in
  exit sub
 end if
 do
  redim preserve ret(j)
  if i = 0 then
   ret(j) = mid(in, i2)
   exit do
  else
   ret(j) = mid(in, i2, i - i2)
  end if
  i2 = i + 1
  i = instr(i2, in, sep)
  j+=1
 loop
end sub

function split_chunk(in as string, index as integer, sep as string = chr(10), default as string="") as string
 ' Split a string by a separater, return one chunk.
 ' 0 is the first chunk.
 ' negative index counts from the end of the list, python-style
 dim chunks() as string
 split(in, chunks(), sep)
 if index < 0 then
  ' convert negative index
  index = ubound(chunks) + 1 + index
 end if
 if index < lbound(chunks) or index > ubound(chunks) then return default
 return chunks(index)
end function

FUNCTION days_since_datestr (datestr as string) as integer
 'Returns the number of days since a date given as a string in the format YYYY-MM-DD
 IF LEN(datestr) <> 10 THEN
  debug "days_since_datestr: bad datestr " & datestr
  RETURN 0
 END IF
 DIM y as integer = str2int(MID(datestr, 1, 4))
 DIM m as integer = str2int(MID(datestr, 6, 2))
 DIM d as integer = str2int(MID(datestr, 9, 2))
 RETURN NOW - DateSerial(y, m, d)
END FUNCTION

'Format a duration as a string with given precision, like '2m23.1s' or '0.4s'.
'decimal_places = 1 means 0.1s precision, = 0 means 1s precision, etc
FUNCTION format_duration(length as double, decimal_places as integer = 1) as string
 DIM subseconds as string
 IF decimal_places > 0 THEN subseconds = "." & STRING(decimal_places, "0")

 DIM seconds as double = fmod(length, 60)
 DIM minutes as integer = INT(length) \ 60
 IF seconds > 60 - 0.5 * 0.1 ^ decimal_places THEN
  'Avoid e.g. printing 1m60.0s for length 119.99
  seconds = 0.
  minutes += 1
 END IF

 DIM msg as string
 IF minutes >= 1 THEN
  IF minutes >= 60 THEN
   msg = (minutes \ 60) & "h"
   minutes = minutes MOD 60
  END IF
  msg &= minutes & "m"
  msg &= FORMAT(seconds, "00" & subseconds) & "s"
 ELSE
  msg = FORMAT(seconds, "0" & subseconds) & "s"
 END IF
 RETURN msg
END FUNCTION

SUB flusharray (array() as integer, byval size as integer=-1, byval value as integer=0)
 'If size is -1, then flush the entire array
 IF size = -1 THEN size = UBOUND(array)
 FOR i as integer = LBOUND(array) TO size
  array(i) = value
 NEXT i
END SUB

#MACRO MAKE_ARRAY_APPEND(Typename)
 'Insert a new element into an array at position 'pos'.
 SUB a_append (array() as Typename, value as Typename)
  REDIM PRESERVE array(LBOUND(array) TO UBOUND(array) + 1)
  array(UBOUND(array)) = value
 END SUB
#ENDMACRO

'MAKE_ARRAY_APPEND(string)
MAKE_ARRAY_APPEND(integer)

SUB a_append (array() as string, value as zstring ptr)
 REDIM PRESERVE array(LBOUND(array) TO UBOUND(array) + 1)
 array(UBOUND(array)) = *value
END SUB

SUB a_append (array() as IntStrPair, byval k as integer, s as zstring ptr)
 REDIM PRESERVE array(LBOUND(array) TO UBOUND(array) + 1)
 array(UBOUND(array)).i = k
 array(UBOUND(array)).s = *s
END SUB

#MACRO MAKE_ARRAY_INSERT(Typename)
 'Insert a new element into an array at position 'pos'.
 SUB a_insert(array() as Typename, pos as integer, value as Typename)
  BUG_IF(pos < LBOUND(array) ORELSE pos > UBOUND(array) + 1, "out of bounds: " & pos)
  REDIM PRESERVE array(LBOUND(array) TO UBOUND(array) + 1)
  array(UBOUND(array)) = value
  FOR idx as integer = UBOUND(array) - 1 TO pos STEP -1
   SWAP array(idx), array(idx + 1)
  NEXT
 END SUB
#ENDMACRO

MAKE_ARRAY_INSERT(string)
MAKE_ARRAY_INSERT(integer)

#MACRO MAKE_ARRAY_POP(Typename)
 ' Remove array(which) (default last), shuffling everything else down
 SUB a_pop (array() as Typename, which as integer = -&h7FFFFFFF)
  IF which = -&h7FFFFFFF THEN which = UBOUND(array)
  IF which >= LBOUND(array) ANDALSO which <= UBOUND(array) THEN
   a_shuffle_to_end array(), which
   IF LBOUND(array) < UBOUND(array) THEN
    REDIM PRESERVE array(LBOUND(array) TO UBOUND(array) - 1)
   ELSE
    ERASE array
   END IF
  END IF
 END SUB
#ENDMACRO

MAKE_ARRAY_POP(string)
MAKE_ARRAY_POP(integer)

#MACRO MAKE_ARRAY_FIND(Subname, ArrayTypename, ValTypename, EqualCheck)
 ' Return index of first item in array equal to 'value', or 'notfound'.
 FUNCTION Subname (array() as ArrayTypename, value as ValTypename, notfound as integer = -1) as integer
  FOR i as integer = LBOUND(array) TO UBOUND(array)
   IF EqualCheck THEN RETURN i
  NEXT
  RETURN notfound
 END FUNCTION
#ENDMACRO

MAKE_ARRAY_FIND(a_find,      string,     string,  array(i) = value)
MAKE_ARRAY_FIND(a_findcasei, string,     string,  LCASE(array(i)) = LCASE(value))
MAKE_ARRAY_FIND(a_find,      integer,    integer, array(i) = value)
MAKE_ARRAY_FIND(a_find,      IntStrPair, integer, array(i).i = value)
MAKE_ARRAY_FIND(a_find,      IntStrPair, string,  array(i).s = value)

#MACRO MAKE_ARRAY_SHUFFLE_TO_END(Typename)
 'Preserves order of everything except element at position 'which'. OK to give invalid 'which'
 SUB a_shuffle_to_end(array() as Typename, which as integer)
  IF which < LBOUND(array) THEN EXIT SUB
  FOR idx as integer = which TO UBOUND(array) - 1
   SWAP array(idx), array(idx + 1)
  NEXT
 END SUB
#ENDMACRO

MAKE_ARRAY_SHUFFLE_TO_END(string)
MAKE_ARRAY_SHUFFLE_TO_END(integer)

#MACRO MAKE_ARRAY_REMOVE(Typename)
 'Remove the first instance of value, resizing the dynamic array.
 'No error or warning if it isn't found.
 'Returns the index of the item if it was found, or -1 if not
 FUNCTION a_remove (array() as TypeName, value as TypeName) as integer
  DIM idx as integer
  idx = a_find(array(), value, -&h7FFFFFFF)
  IF idx = -&h7FFFFFFF THEN RETURN -1
  a_pop array(), idx
  RETURN idx
 END FUNCTION
#ENDMACRO

MAKE_ARRAY_REMOVE(string)
MAKE_ARRAY_REMOVE(integer)

SUB a_copy (fromarray() as integer, toarray() as integer)
 DIM as integer low = LBOUND(fromarray), high = UBOUND(fromarray)
 REDIM toarray(low TO high)
 memcpy @toarray(low), @fromarray(low), sizeof(integer) * (high - low + 1)
END SUB

SUB a_copy (fromarray() as string, toarray() as string)
 DIM as integer low = LBOUND(fromarray), high = UBOUND(fromarray)
 REDIM toarray(low TO high)
 FOR i as integer = low TO high
  toarray(i) = fromarray(i)
 NEXT
END SUB

'Only have tests for some of the above functions here...
#IFDEF __FB_MAIN__
startTest(array_operators)
 REDIM arr() as integer
 IF UBOUND(arr) <> -1 THEN fail
 a_append arr(), 100
 IF UBOUND(arr) <> 0 THEN fail
 IF arr(0) <> 100 THEN fail
 a_insert arr(), 0, 101
 a_append arr(), 103
 a_insert arr(), 3, 102 'Insert 1 past end
 IF UBOUND(arr) <> 3 THEN fail
 IF arr(0) <> 101 THEN fail
 IF arr(1) <> 100 THEN fail
 IF arr(2) <> 103 THEN fail
 IF arr(3) <> 102 THEN fail
 a_pop arr(), 1         'Remove 100
 IF UBOUND(arr) <> 2 THEN fail
 IF a_remove(arr(), 100) <> -1 THEN fail  'Does nothing
 IF arr(1) <> 103 THEN fail
 a_pop arr()            'Remove arr(2)=102
 IF UBOUND(arr) <> 1 THEN fail
 IF arr(1) <> 103 THEN fail
 IF a_remove(arr(), 101) <> 0 THEN fail  'Remove arr(0)
 IF UBOUND(arr) <> 0 THEN fail
 IF arr(0) <> 103 THEN fail  'arr = {103}
 a_pop arr()
 IF LBOUND(arr) <> 0 THEN fail
 IF UBOUND(arr) <> -1 THEN fail
 a_pop arr()  'noop, already empty
 IF UBOUND(arr) <> -1 THEN fail
endTest

'Test how static dynamic behave in FB, especially before they are first REDIMed.
'This behaviour was different in older versions (0.90?)

#MACRO STATIC_TEST_FUNC(Typename)
FUNCTION test_static_##Typename##_array (push as Typename, expectubound as integer) as Typename
  STATIC array() as Typename
  DIM ret as Typename
  IF LBOUND(array) <> 0 THEN RETURN push            'This will cause a fail
  IF UBOUND(array) <> expectubound THEN RETURN push 'ditto
  IF a_find(array(), push) <> -1 THEN RETURN push   'ditto
  IF UBOUND(array) >= 0 THEN ret = array(UBOUND(array))
  a_append array(), push
  'After 3 pushes, reset
  IF UBOUND(array) = 2 THEN ERASE array
  RETURN ret
END FUNCTION
#ENDMACRO

STATIC_TEST_FUNC(integer)
STATIC_TEST_FUNC(string)

startTest(staticDynamicArray)
 IF test_static_integer_array(30, -1) <> 0 THEN fail
 IF test_static_integer_array(31, 0) <> 30 THEN fail
 IF test_static_integer_array(32, 1) <> 31 THEN fail  'Erases the array
 IF test_static_integer_array(33, -1) <> 0 THEN fail
 IF test_static_integer_array(34, 0) <> 33 THEN fail
 IF test_static_string_array("a", -1) <> "" THEN fail
 IF test_static_string_array("ab", 0) <> "a" THEN fail
endTest
#ENDIF


'Stable (insert) sort of integers. Running time is O(n^2), but much faster on nearly-sorted lists.
'Use qsort_integers_indices instead when speed is needed.
'Returns, in indices() (assumed to already have been dimmed large enough), indices for
'visiting the data (an array of some kind of struct containing an integer) in ascending order.
'start points to the integer in the first element, stride is the size of an array element, in integers
SUB sort_integers_indices(indices() as integer, byval start as integer ptr, byval number as integer, byval stride as integer)
 IF number = 0 THEN number = UBOUND(indices) + 1
 DIM keys(number - 1) as integer
 DIM as integer i, temp
 FOR i = 0 TO number - 1
  keys(i) = *start
  start = CAST(integer ptr, CAST(byte ptr, start) + stride) 'yuck
 NEXT

 indices(0) = 0
 FOR j as integer = 1 TO number - 1
  temp = keys(j)
  FOR i = j - 1 TO 0 STEP -1
   IF keys(i) <= temp THEN EXIT FOR
   keys(i + 1) = keys(i)
   indices(i + 1) = indices(i)
  NEXT
  keys(i + 1) = temp
  indices(i + 1) = j
 NEXT
END SUB

'CRT Quicksort. Running time is *usually* O(n*log(n)). NOT STABLE
'See sort_integer_indices.
'
'A comparison (dependent on your libc) of sort_integers_indices and qsort_integers_indices speed:
'For random integers, the quicksort is faster
'for arrays over length about 80. For arrays which are 90% sorted appended with 10% random data,
'the cut off is about 600 (insertion sort did ~5x better on nearly-sort data at the 600 mark)
SUB qsort_indices(indices() as integer, byval start as any ptr, byval number as integer = 0, byval stride as integer, byval compare_fn as FnCompare)
 IF number = 0 THEN number = UBOUND(indices) + 1

 DIM keys(number - 1) as any ptr
 DIM i as integer
 FOR i = 0 TO number - 1
  keys(i) = start + stride * i
 NEXT

 qsort(@keys(0), number, sizeof(any ptr), compare_fn)

 FOR i = 0 TO number - 1
  indices(i) = CAST(integer, keys(i) - start) \ stride
 NEXT
END SUB

SUB qsort_integers_indices(indices() as integer, byval start as integer ptr, byval number as integer, byval stride as integer)
 qsort_indices indices(), start, number, stride, CAST(FnCompare, @integerptr_compare)
END SUB

SUB qsort_strings_indices(indices() as integer, byval start as string ptr, byval number as integer, byval stride as integer)
 qsort_indices indices(), start, number, stride, CAST(FnCompare, @stringptr_compare)
END SUB


FUNCTION ptr_compare CDECL (byval a as any ptr ptr, byval b as any ptr ptr) as long
 IF *a < *b THEN RETURN -1
 IF *a > *b THEN RETURN 1
 'implicitly RETURN 0 (it's faster to omit the RETURN :-)
END FUNCTION

FUNCTION integer_compare CDECL (byval a as integer ptr, byval b as integer ptr) as long
 IF *a < *b THEN RETURN -1
 IF *a > *b THEN RETURN 1
 'implicitly RETURN 0 (it's faster to omit the RETURN :-)
END FUNCTION

FUNCTION integerptr_compare CDECL (byval a as integer ptr ptr, byval b as integer ptr ptr) as long
 IF **a < **b THEN RETURN -1
 IF **a > **b THEN RETURN 1
 'implicitly RETURN 0
END FUNCTION

'a string ptr is a pointer to a FB string descriptor
FUNCTION string_compare CDECL (a as string, b as string) as integer
 'This is equivalent, but the code below can be adapted for case insensitive compare (and is faster (what, how?!))
 'Yep, still twice as fast as of 2019.
 'RETURN fb_StrCompare(a, -1, b, -1)

 DIM ret as integer = 0
 DIM somenull as bool = NO
 'Zero-length strings in FB may have either a NULL data pointer or a pointer to a '\0', deal with that.
 IF @a[0] = 0 THEN ret -= 1: somenull = YES
 IF @b[0] = 0 THEN ret += 1: somenull = YES
 IF somenull THEN RETURN ret

 DIM k as integer = 0
 DIM chara as ubyte
 DIM charb as ubyte
 DO
  chara = a[k]
  charb = b[k]
  IF chara < charb THEN
   RETURN -1
  ELSEIF chara > charb THEN
   RETURN 1
  END IF
  k += 1
 LOOP WHILE chara OR charb
 RETURN 0
END FUNCTION

FUNCTION stringptr_compare CDECL (byval a as string ptr ptr, byval b as string ptr ptr) as long
 RETURN string_compare(**a, **b)
END FUNCTION

FUNCTION numeric_string_compare CDECL (a as string, b as string, case_insen as bool = NO) as integer
 DIM ptra as zstring ptr = @a[0]
 DIM ptrb as zstring ptr = @b[0]
 DO
  DIM as ubyte chara, charb
  IF ptra THEN chara = *ptra  'On the first iteration one of the strings might be NULL
  IF ptrb THEN charb = *ptrb
  IF case_insen THEN
   chara = tolower(chara)
   charb = tolower(charb)
  END IF

  IF chara = 0 ANDALSO charb = 0 THEN
   RETURN 0
  ELSEIF chara <> ASC("0") ANDALSO isdigit(chara) ANDALSO charb <> ASC("0") ANDALSO isdigit(charb) THEN
   'Two numbers which both don't start with a leading zero.
   'If either number has a leading zero, then the one with the most leading zeroes
   'is first, eg "09" before "1". Avoids having to check length if the values are equal.
   DIM as longint numa, numb
   DIM as zstring ptr aftera, afterb
   'strtol is quite similar to VALINT
   numa = strtoll(ptra, @aftera, 10)
   numb = strtoll(ptrb, @afterb, 10)
   IF numa < numb THEN
    RETURN -1
   ELSEIF numa > numb THEN
    RETURN 1
   ELSEIF numa = LLONG_MAX THEN
    'If overflowed, don't compare by value, but by digits.
    '(Can't allow numa/numb to overflow, because we ignore negative signs)
   ELSE
    'Skip over the number
    ptra = aftera
    ptrb = afterb
    CONTINUE DO
   END IF
  END IF
  IF chara < charb THEN
   RETURN -1
  ELSEIF chara > charb THEN
   RETURN 1
  END IF
  ptra += 1
  ptrb += 1
 LOOP
END FUNCTION

#IFDEF __FB_MAIN__
startTest(numeric_string_compare)
 DIM nul as string, notnul as string = "x"
 notnul[0] = 0
 IF numeric_string_compare(notnul, nul) <> 0 THEN fail
 IF numeric_string_compare("a", "0") <> 1 THEN fail
 IF numeric_string_compare("0", "0") <> 0 THEN fail
 IF numeric_string_compare("abc", "aBc", YES) <> 0 THEN fail
 IF numeric_string_compare("abc", "aBc", NO) <> 1 THEN fail
 IF numeric_string_compare("ab5c", "ab0d") <> 1 THEN fail
 IF numeric_string_compare("ab5c", "ab5d") <> -1 THEN fail
 IF numeric_string_compare("12345678900", "12345678900") <> 0 THEN fail  '32-bit int
 IF numeric_string_compare("1234567890000", "1234567890000") <> 0 THEN fail  '64-bit int
 IF numeric_string_compare("1234567890000", "10000000000000") <> -1 THEN fail  '64-bit int
 'Note: huge integers aren't compared by value
 IF numeric_string_compare("9223372036854775809", "9223372036854775808") <> 1 THEN fail  '>64-bit int
 IF numeric_string_compare("09", "1") <> -1 THEN fail
endTest
#ENDIF

'Invert a (possibly partial) permutation such as that returned by sort_integers_indices;
'indices() should normally contain the integers 0 to UBOUND(inverse),
'result stored in inverse().
'If an integer x between 0 and UBOUND(inverse) is missing from indices(),
'then inverse(x) contains garbage. You may want to clear inverse() first.
SUB invert_permutation(indices() as integer, inverse() as integer)
 FOR i as integer = 0 TO UBOUND(indices)
  DIM index as integer = indices(i)
  IF index >= 0 AND index <= UBOUND(inverse) THEN
   inverse(index) = i
  END IF
 NEXT
END SUB

SUB invert_permutation(indices() as integer)
 DIM inverse(UBOUND(indices)) as integer
 invert_permutation indices(), inverse()
 'Copy back
 memcpy(@indices(0), @inverse(0), sizeof(integer) * (UBOUND(indices) + 1))
END SUB

'These cache functions store a 'resetter' string, which causes search_string_cache
'to automatically empty the cache when its value changes (eg, different game).
'Normally you would just use the game_unique_id global for this.
'Note that you can resize the cache arrays (in either direction!) as you want at any time.
FUNCTION search_string_cache (cache() as IntStrPair, byval key as integer, resetter as string) as string
 IF cache(0).s <> resetter THEN
  cache(0).s = resetter
  cache(0).i = 0  'used to loop through the indices when writing
  
  FOR i as integer = 1 TO UBOUND(cache)
   cache(i).i = -1099999876
   cache(i).s = ""
  NEXT
 END IF

 FOR i as integer = 1 TO UBOUND(cache)
  IF cache(i).i = key THEN RETURN cache(i).s
 NEXT
END FUNCTION

SUB add_string_cache (cache() as IntStrPair, byval key as integer, value as string)
 DIM i as integer
 FOR i = 1 TO UBOUND(cache)
  IF cache(i).i = -1099999876 THEN
   cache(i).i = key
   cache(i).s = value
   EXIT SUB
  END IF
 NEXT
 'overwrite an existing entry, in a loop
 i = 1 + (cache(0).i MOD UBOUND(cache))
 cache(i).i = key
 cache(i).s = value
 cache(0).i = i
END SUB

SUB remove_string_cache (cache() as IntStrPair, byval key as integer)
 FOR i as integer = 1 TO UBOUND(cache)
  IF cache(i).i = key THEN
   cache(i).i = -1099999876
   cache(i).s = ""
   EXIT SUB
  END IF
 NEXT
END SUB


'---------------- Hash Functions --------------

'Return the SHA1 hash of a file.
SUB file_hash_SHA1(filename as string, result_out as SHA160 ptr)
  DIM fh as integer
  IF OPENFILE(filename, FOR_BINARY + ACCESS_READ, fh) THEN
    debug "file_hash: couldn't open " & filename
    memset result_out, 0, 20
    EXIT SUB
  END IF
  file_hash_SHA1(fh, result_out)
  CLOSE #fh
END SUB

'Read a whole file (clobbering file position) and return its SHA1 hash.
SUB file_hash_SHA1(fh as integer, result_out as SHA160 ptr)
  DIM size as integer = LOF(fh)
  SEEK fh, 1
  DIM buf(4095) as ubyte
  DIM ctx as SHA1_CTX
  SHA1Init(@ctx)
  WHILE size > 0
    DIM readamnt as size_t
    fgetiob fh, , @buf(0), 4096, @readamnt
    IF readamnt <= 0 THEN
      debug "file_hash: fgetiob failed!"
      memset result_out, 0, 20
      EXIT SUB
    END IF
    SHA1Update(@ctx, cptr(zstring ptr, @buf(0)), readamnt)
    size -= readamnt
  WEND
  SHA1Final(result_out, @ctx)
END SUB

'Return a 64 bit hash (first 64 bits of SHA1) of a file
'(The bytes are reversed so that HEX() prints the first 8 bytes of the hash)
FUNCTION file_hash64(filename as string) as ulongint
  DIM hash as SHA160
  file_hash_SHA1 filename, @hash
  RETURN int64_to_bigendian(*CAST(ulongint ptr, @hash))
END FUNCTION

'Return a 64 bit hash (first 64 bits of SHA1) of a file (clobbers file position)
'(The bytes are reversed so that HEX() prints the first 8 bytes of the hash)
FUNCTION file_hash64(fh as integer) as ulongint
  DIM hash as SHA160
  file_hash_SHA1 fh, @hash
  RETURN int64_to_bigendian(*CAST(ulongint ptr, @hash))
END FUNCTION

'Format a SHA1 hash to a hex string (length 40)
FUNCTION SHA1_to_string(hash as SHA160) as string
  DIM ret as string
  FOR idx as integer = 0 TO 19
   ret &= LCASE(HEX(hash[idx], 2))
  NEXT
  RETURN ret
END FUNCTION

'A fast hash function suitable for hashmaps
FUNCTION strhash(hstr as string) as unsigned integer
  RETURN stringhash(cptr(zstring ptr, strptr(hstr)), len(hstr))
END FUNCTION


'---------- Path and File functions -----------

' Same as os.path.join in Python.
' path1 is a directory (possibly empty) and
FUNCTION join_path(path1 as string, path2 as string) as string
 IF is_absolute_path(path2) THEN RETURN path2
 IF LEN(path1) = 0 THEN RETURN path2
 RETURN trim_trailing_slashes(path1) & SLASH & path2
END FUNCTION

'Change / to \ in paths on Windows
FUNCTION normalize_path(filename as string) as string
  DIM ret as string = filename
#IFDEF __FB_WIN32__
  FOR i as integer = 0 TO LEN(ret) - 1 
    IF ispathsep(ret[i]) THEN ret[i] = ASC(SLASH)
  NEXT
#ENDIF
  RETURN ret
END FUNCTION

#IFDEF __FB_MAIN__

#DEFINE testjoin(path1, path2, expected) testEqual(join_path(path1, path2), expected)

startTest(join_path)
  testjoin("",         "foo.bar", "foo.bar")
  testjoin("",         "",        "")
  #IFDEF __FB_WIN32__
    testjoin("foo/bar/", "qux",   "foo/bar\qux")
    testjoin("foo.bar",  "",      "foo.bar\")
    testjoin("foo\bar",  "qux\",  "foo\bar\qux\")
    testjoin("c:\foo",   "bar",   "c:\foo\bar")
    testjoin("foo",      "c:\bar","c:\bar")
  #ELSE
    testjoin("foo/bar/", "qux",   "foo/bar/qux")
    testjoin("foo.bar",  "",      "foo.bar/")
    testjoin("foo\bar",  "qux\",  "foo\bar/qux\")
    testjoin("/foo",     "bar",   "/foo/bar")
    testjoin("foo",      "/bar",  "/bar")
  #ENDIF
endTest

#DEFINE testnorm(path, expected) testEqual(normalize_path(path), expected)

startTest(normalize_path)
  #IFDEF __FB_WIN32__
    testnorm("a/b/cat//",  "a\b\cat\\")
    testnorm("/cat/",      "\cat\")
    testnorm("\cat\",      "\cat\")
    testnorm("c:/",        "c:\")
  #ELSE
    testnorm("a/b/cat//",  "a/b/cat//")
    testnorm("/cat/",      "/cat/")
    testnorm("\cat\",      "\cat\")
    testnorm("c:/",        "c:/")
  #ENDIF
  testnorm("",          "")
endTest
#ENDIF

FUNCTION add_trailing_slash (dirname as string) as string
  IF LEN(dirname) = 0 THEN RETURN dirname  'For safety
  #IFDEF __FB_WIN32__
    'Also valid on Windows
    IF RIGHT(dirname, 1) = "/" THEN RETURN dirname
  #ENDIF
  IF RIGHT(dirname, 1) = SLASH THEN RETURN dirname
  RETURN dirname + SLASH
END FUNCTION

'On Windows both slashes are trimmed, on Unix only /
'This will NOT trim a trailing slash if it's part of the root,
'"/" on Unix; "X:\" or "X:/" on Windows.
FUNCTION trim_trailing_slashes(filename as string) as string
  DIM root_length as integer = LEN(get_path_root(filename))
  DIM retend as integer = LEN(filename)
  WHILE retend > root_length
    DIM ch as byte = filename[retend - 1]
    IF ispathsep(ch) THEN
      retend -= 1
    ELSE
      EXIT WHILE
    END IF
  WEND
  RETURN LEFT(filename, retend)
END FUNCTION

FUNCTION trimpath(filename as string) as string
  'Return the file/directory name without path, and without trailing (or any) slashes.
  'See testcases below
  DIM temp as string = trim_trailing_slashes(filename)
  FOR i as integer = LEN(temp) TO 1 STEP -1
    IF ispathsep(temp[i - 1]) THEN
      RETURN MID(temp, i + 1, LEN(temp) - (i + 1) + 1)
    END IF
  NEXT
  RETURN temp
END FUNCTION

#IFDEF __FB_MAIN__

#DEFINE testtrims(path, expected) testEqual(trim_trailing_slashes(path), expected)

startTest(trim_trailing_slashes)
  testtrims("a/b/cat//", "a/b/cat")
  testtrims("a/b/cat/",  "a/b/cat")
  testtrims("a/b/cat",   "a/b/cat")
  testtrims("cat/",      "cat")
  testtrims("/cat/",     "/cat")
  testtrims("/c",        "/c")
  #IFDEF __FB_WIN32__
    testtrims("c:\",      "c:\")
    testtrims("c:\/",     "c:\")
    testtrims("c:\foo",   "c:\foo")
    testtrims("c:\f\",    "c:\f")
    testtrims("c:\foo/",  "c:\foo")
  #ELSE
    testtrims("/",        "/")
    testtrims("//",       "/")
  #ENDIF
  testtrims("",          "")
endTest

#DEFINE testtrimp(path, expected) testEqual(trimpath(path), expected)

startTest(trimpath)
  testtrimp("a/b/cat//", "cat")
  testtrimp("a/b/cat/",  "cat")
  testtrimp("a/b/cat",   "cat")
  testtrimp("a/b//cat",  "cat")
  testtrimp("cat/",      "cat")
  testtrimp("/cat/",     "cat")
  #IFDEF __FB_WIN32__
    testtrimp("c:\",      "")
    testtrimp("c:\foo",   "foo")
    testtrimp("c:\foo\",  "foo")
    testtrimp("c:\foo/",  "foo")
  #ELSE
    testtrimp("/",        "")
  #ENDIF
  testtrimp("",          "")
endTest
#ENDIF

'FIXME: this function is terribly misnamed; rename it or change semantics
FUNCTION trimfilename (filename as string) as string
  'Trim the last component of a path (which may be a directory rather than file!)
  'Return path without trailing slash (unless need to return root dir). See testcases.
  'This is the complement to trimpath
  'Quite similar to parentdir().
  DIM ret as string = trim_trailing_slashes(normalize_path(filename))
  ret = MID(ret, 1, large(0, INSTRREV(ret, SLASH) - 1))
  IF is_absolute_path(filename) AND is_absolute_path(ret) = NO THEN
    'Whoops, we deleted the / or \ corresponding to the root, put it back
    RETURN normalize_path(get_path_root(filename))
  END IF
  return ret
END FUNCTION

#IFDEF __FB_MAIN__
#DEFINE testtrimf(path, expected) testEqual(trimfilename(path), normalize_path(expected))

startTest(trimfilename)
  testtrimf("a/b/cat//", "a/b")
  testtrimf("a/b/cat/",  "a/b")
  testtrimf("a/b/cat",   "a/b")
  testtrimf("cat/",      "")
  testtrimf("/cat/",     "/")
  testtrimf("/",         "/")
  #IFDEF __FB_WIN32__
    testtrimf("c:/vak",          "c:\")
    testtrimf("c:\vak/",         "c:\")
    testtrimf("c:\",             "c:\")
  #ENDIF
  testtrimf("",          "")
endTest
#ENDIF

FUNCTION trimextension (filename as string) as string
  'Return the filename (including path) without extension
  'Periods at the beginning of file/folder names are not counted as beginning an extension
  DIM at as integer = INSTRREV(filename, ".")
  DIM at2 as integer = INSTRREV(filename, "/")
#IFDEF __FB_WIN32__
  at2 = large(at2, INSTRREV(filename, "\"))
#ENDIF
  IF at >= at2 + 2 THEN
    RETURN MID(filename, 1, at - 1)
  ELSE
    RETURN filename
  END IF
END FUNCTION

FUNCTION justextension (filename as string) as string
  'Return only the extension (everything after the *last* period)
  'Periods at the beginning of file/folder names are not counted as beginning an extension
  DIM at as integer = INSTRREV(filename, ".")
  DIM at2 as integer = INSTRREV(filename, "/")
#IFDEF __FB_WIN32__
  at2 = large(at2, INSTRREV(filename, "\"))
#ENDIF
  IF at >= at2 + 2 THEN
    RETURN MID(filename, at + 1)
  ELSE
    RETURN ""
  END IF
END FUNCTION

#IFDEF __FB_MAIN__
#DEFINE testTrimExt(path, expected) testEqual(trimextension(path), normalize_path(expected))
#DEFINE testJustExt(path, expected) testEqual(justextension(path), normalize_path(expected))

startTest(TrimExtension)
  testTrimExt("", "")
  testTrimExt("File", "File")
  testTrimExt("file.e", "file")
  testTrimExt("file.tar.gz", "file.tar")
  testTrimExt("dir/game.rpgdir", "dir/game")
  testTrimExt("dir/game.rpgdir/", "dir/game.rpgdir/")
  testTrimExt(".config", ".config")
  testTrimExt(".config.TxT", ".config")
  testTrimExt("bad filename. no?", "bad filename")

  testJustExt("", "")
  testJustExt("File", "")
  testJustExt("file.e", "e")
  testJustExt("file.tar.gz", "gz")
  testJustExt("dir/game.rpgdir", "rpgdir")
  testJustExt("dir/game.rpgdir/", "")
  testJustExt(".config", "")
  testJustExt(".config.TxT", "TxT")
  testJustExt("bad filename. no?", " no?")
endTest
#ENDIF

'Return the root of a Windows path, regardless of the current OS: X:/ or X:\ or / or \
'Otherwise return ""
'FIXME: should handle network paths on Windows too
FUNCTION get_windows_path_root (pathname as string) as string
  DIM first as string = LCASE(LEFT(pathname, 1))
  DIM temp as string = MID(pathname, 2, 2)
  IF first >= "a" ANDALSO first <= "z" ANDALSO (temp = ":\" OR temp = ":/") THEN
    RETURN MID(pathname, 1, 3)
  END IF
END FUNCTION

'If a path is absolute return the root directory: / on Unix, X:/ or X:\ or / or \ on Windows
'Otherwise return ""
'FIXME: should handle network paths on Windows too
FUNCTION get_path_root (pathname as string) as string
#IFDEF __FB_WIN32__
  DIM root as string = get_windows_path_root(pathname)
  IF LEN(root) THEN RETURN root
#ENDIF
  IF LEN(pathname) ANDALSO ispathsep(pathname[0]) THEN RETURN MID(pathname, 1, 1)
  RETURN ""
END FUNCTION

'Strip / or X:\ from path, if any
FUNCTION trim_path_root (pathname as string) as string
  DIM root as string = get_path_root(pathname)
  IF LEN(root) THEN
    RETURN MID(pathname, LEN(root) + 1)
  ELSE
    RETURN pathname
  END IF
END FUNCTION

FUNCTION is_absolute_path (sDir as string) as bool
  RETURN LEN(get_path_root(sDir)) <> 0
END FUNCTION

'Return whether a path is absolute on this or any other OS
FUNCTION is_possibly_absolute_path (pathname as string) as bool
  IF LEN(pathname) THEN
    IF pathname[0] = ASC("/") OR pathname[0] = ASC("\") THEN RETURN YES
  END IF
  IF LEN(get_windows_path_root(pathname)) THEN RETURN YES
  RETURN NO
END FUNCTION

'Make a path absolute. See also absolute_with_orig_path
FUNCTION absolute_path(pathname as string) as string
  IF NOT is_absolute_path(pathname) THEN RETURN CURDIR & SLASH & pathname
  RETURN pathname
END FUNCTION


FUNCTION absolute_with_orig_path(file_or_dir as string, byval add_slash as bool = NO) as string
  DIM d as string = file_or_dir
  IF NOT is_absolute_path(d) THEN d = orig_dir & SLASH & d
  IF add_slash THEN d = add_trailing_slash(d)
  RETURN d
END FUNCTION

'Remove redundant ../, ./, // in a path. Handles both relative and absolute paths
'Result has normalised slashes and no trailing slash (unless it's the root /).
'See testcases below
FUNCTION simplify_path(sDir as string) as string
  DIM piecesarray() as string
  DIM pieces as string vector
  DIM pathname as string = normalize_path(sDir)
  DIM isabsolute as integer = is_absolute_path(pathname)
  'remove drive letter
  DIM ret as string = get_path_root(pathname)
  'Trim everything except the final slash of the root
  IF LEN(ret) THEN
   pathname = MID(pathname, LEN(ret))
  END IF

  split pathname, piecesarray(), SLASH
  array_to_vector pieces, piecesarray()
  DIM i as integer = 0
  DIM leading_updots as integer = 0  'The number of "../"s at the start
  WHILE i < v_len(pieces)
    IF pieces[i] = "" OR pieces[i] = "." THEN
      v_delete_slice pieces, i, i+1
    ELSEIF pieces[i] = ".." THEN
      IF i = 0 ANDALSO isabsolute THEN
        'Can't go up in the root directory
        v_delete_slice pieces, i, i+1
      ELSEIF i > leading_updots THEN
        v_delete_slice pieces, i-1, i+1
        i -= 1
      ELSE
        leading_updots += 1
        i += 1
      END IF
    ELSE
      i += 1
    END IF
  WEND
  FOR i = 0 TO v_len(pieces) - 1
    IF i <> 0 THEN ret += SLASH
    ret += pieces[i]
  NEXT
  v_free pieces
  IF ret = "" THEN ret = "."   'so that appending a slash is safe
  RETURN ret
END FUNCTION

#IFDEF __FB_MAIN__
#DEFINE testsimplify(path, expected) testEqual(simplify_path(path), normalize_path(expected))

startTest(simplify_path)
  testsimplify("testcases",         "testcases")
  testsimplify(".././../foo/",      "../../foo")
  testsimplify(".././a/../../foo/", "../../foo")
  testsimplify("/..",   "/")
  testsimplify("",      ".")
  testsimplify(".",     ".")
  testsimplify("/../.", "/")
  testsimplify("./../../../../a",   "../../../../a")
  testsimplify("//.//../a/../c/b/../d", "/c/d")
endTest
#ENDIF

'If one path is absolute and the other isn't, returns false!
'It doesn't make a difference if one has a trailing slash and not the other.
FUNCTION paths_equal(path1 as string, path2 as string) as bool
  #IFDEF __FB_WIN32__
    RETURN simplify_path(LCASE(path1)) = simplify_path(LCASE(path2))
  #ELSE
    RETURN simplify_path(path1) = simplify_path(path2)
  #ENDIF
END FUNCTION

'Run pathname through simplify_path, and also make it relative if it's below
''fromwhere' (which ought to be a dir, not a file, and defaults to CURDIR).
'Returns "." if both are equal.
'It would be possible to also possibly return something starting with some ../'s, but it's more trouble
'Warning, either both paths should be absolute, or both relative, otherwise
'no parts of the path can be removed.
FUNCTION simplify_path_further(pathname as string, fromwhere as string = "") as string
  DIM path as string = simplify_path(pathname)
  DIM source as string = fromwhere
  IF source = "" THEN source = CURDIR
  source = simplify_path(source)
  'source and path now have no trailing slash (unless they're the root dir)
  #IFDEF __FB_WIN32__
    DIM matchlen as integer = length_matching(LCASE(source), LCASE(path))
  #ELSE
    DIM matchlen as integer = length_matching(source, path)
  #ENDIF
  IF matchlen = LEN(source) THEN
    IF matchlen = LEN(path) THEN
      'They are equal
      RETURN "."
    ELSEIF ispathsep(path[matchlen]) THEN
      'Need a slash following the part matching source, so we don't match foo and foo.rpgdir
      'Strip the slash.
      RETURN MID(path, matchlen + 2)
    END IF
  END IF
  RETURN path
END FUNCTION

#IFDEF __FB_MAIN__
#DEFINE testsimplify2(path, fromwhere, expected) testEqual(simplify_path_further(path, fromwhere), normalize_path(expected))

startTest(simplify_path_further)
  testsimplify2("testcases",    "",          "testcases")
  testsimplify2("testcases/",   "",          "testcases")
  testsimplify2("testcases",    "testcases", ".")
  testsimplify2("testcases",    "test",      "testcases")
  testsimplify2("/foo/bar",     "foo",       "/foo/bar")
  testsimplify2("/foo/bar/",    "foo/",      "/foo/bar")
  testsimplify2("/foo/bar",     "/foo",      "bar")
  testsimplify2("/foo/bar/",    "/foo/",     "bar")
  testsimplify2("../bar",       "..",        "bar")
  testsimplify2(".././../foo/", "",          "../../foo")
  testsimplify2(".././../foo/", "..",        "../foo")
  testsimplify2(".././../foo/", "..",        "../foo")
  testsimplify2(".././a/../../foo/", "",     "../../foo")
  testsimplify2("/..",           "",         "/")
  testsimplify2("",              "",         ".")
  testsimplify2(".",             ".",        ".")
  testsimplify2("/../.",         "",         "/")
  testsimplify2("//.//../a/../c/b/../d",  "/c",  "d")
  testsimplify2(CURDIR & SLASH & "bar",   "",    "bar")
endTest
#ENDIF

'Go up a number of directories. Simplifies and normalises.
'path is interpreted as a directory even if missing the final slash!
FUNCTION parentdir (path as string, byval upamount as integer = 1) as string
  DIM pathname as string = path + SLASH
  FOR i as integer = 0 TO upamount - 1
   pathname += ".." + SLASH
  NEXT
  RETURN add_trailing_slash(simplify_path(pathname))
END FUNCTION

#IFDEF __FB_MAIN__
startTest(parentdir)
  testEqual(parentdir("foo/bar/qux/", 1), "foo" SLASH "bar" SLASH)
  testEqual(parentdir("foo/bar/qux", 1), "foo" SLASH "bar" SLASH)
  testEqual(parentdir("foo/bar/qux", 2), "foo" SLASH)
  testEqual(parentdir("foo/bar/qux", 3), "." SLASH)
  testEqual(parentdir("qux/", 0), "qux" SLASH)
  testEqual(parentdir("qux", 0), "qux" SLASH)
  testEqual(parentdir("qux", 1), "." SLASH)
  testEqual(parentdir(SLASH, 1), SLASH)
  #IFDEF __FB_WIN32__
    testEqual(parentdir("C:\", 1), "C:\")
  #ENDIF
endTest
#ENDIF

' Given a relative path to a file or dir by a user clueless about Unix/Windows
' differences, try to find that file, returning either a simplified/normalised
' path or an error message. Use isfile() or isdir() to test for success.
' 'path' is not modified.
' In particular:
' - \ is treated as a path separator even on Unix
' - The path is treated as case insensitive, and we search for a file matching that pattern
' - If there are multiple files matching case insensitively, return an error
' - If the path would be absolute on any platform, give up, this can't be made portable
' FIXME: Need to disallow non-portable characters such as :
'
' NOTE: while on UNIX and Mac we return the path with the actual capitalisation, on Windows we don't bother
' to do so, and return the original path (normalised and simplified).
' Note that on Mac the filesystem may or may not be case sensitive, but we use the Unix/case sensitive
' codepath to handle both cases.
FUNCTION find_file_portably (path as string) as string
  IF is_possibly_absolute_path(path) THEN RETURN "Absolute path not allowed: " + path

  CONST findhidden = YES

  DIM _path as string = path
  #IFDEF __FB_UNIX__
    replacestr _path, "\", "/"
    _path = simplify_path(_path)

    DIM ret as string
    DIM filenames as string vector

    ' Walk the path
    REDIM components() as string
    split _path, components(), SLASH

    FOR idx as integer = 0 TO UBOUND(components)
      DIM namemask as string = anycase(components(idx))
      IF idx > 0 THEN ret += SLASH

      IF components(idx) = ".." THEN
        ret += components(idx)
        CONTINUE FOR
      END IF

      IF idx = UBOUND(components) THEN
        ' We don't know whether we're looking for a file or directory, allow either
        v_move filenames, list_files_or_subdirs(ret, namemask, findhidden, -1)
      ELSE
        v_move filenames, list_subdirs(ret, namemask, findhidden)
      END IF

      IF v_len(filenames) = 0 THEN
        ' Failure
        v_free filenames
        RETURN "Can't find " + ret + components(idx)
      ELSEIF v_len(filenames) > 1 THEN
        ' Return an error
        DIM errmsg as string = "Found multiple paths"
        IF LEN(ret) THEN errmsg += " (in " + ret + ")"
        ' Sort just so the error message is deterministic, for testcases
        v_sort filenames
        errmsg += " with same case-insensitive name: " + v_str(filenames)
        v_free filenames
        RETURN errmsg
      END IF
      ret += filenames[0]
    NEXT

    v_free filenames
    RETURN ret

  #ELSE
    ' On Windows, no searching needed
    _path = simplify_path(path)
    IF isfile(_path) ORELSE isdir(_path) THEN RETURN _path
    RETURN "Can't find " + _path
  #ENDIF
END FUNCTION

'If the given file or directory with possibly changed case already exists then
'return its filename, otherwise returns 'path' unchanged.
'Also, path can contain ? or * wildcards in the final component.
'(Only does case-insensitive matching of the final component. Try find_file_portably for the whole path)
FUNCTION find_file_anycase(path as string, file_type as FileTypeEnum = fileTypeFile) as string
 #IFDEF __FB_WIN32__
  'If the path contains a wildcard, then we have to run it through findfiles to get rid of it,
  'otherwise there's no need to do a search
  IF INSTR(path, "*") = 0 ANDALSO INSTR(path, "?") = 0 THEN RETURN path
 #ENDIF
 DIM filelist() as string
 DIM dirname as string = trimfilename(path)
 IF LEN(dirname) = 0 THEN dirname = CURDIR
 'findfiles is always case-insensitive
 findfiles dirname, trimpath(path), file_type, YES, filelist()
 IF UBOUND(filelist) < 0 THEN RETURN path
 IF UBOUND(filelist) > 0 THEN
  debug "find_path_anycase: multiple files case-insensitively match " & path
 END IF
 RETURN dirname & SLASH & filelist(0)
END FUNCTION

#IFDEF __FB_MAIN__
' Have to allow find_file_portably to not resolve the true capitalisation on Windows
#IFDEF __FB_UNIX__
  #DEFINE testfindfile(path, expected_unix, expected_windows) testEqual(find_file_portably(path), expected_unix)
  #DEFINE testanycase( path, expected_unix, expected_windows) testEqual(find_file_anycase (path), expected_unix)
#ELSE
  #DEFINE testfindfile(path, expected_unix, expected_windows) testEqual(find_file_portably(path), expected_windows)
  #DEFINE testanycase( path, expected_unix, expected_windows) testEqual(find_file_anycase (path), expected_windows)
#ENDIF
#DEFINE testfilemissing(path, expected) testEqual(find_file_portably(path), "Can't find " + normalize_path(expected))
#DEFINE testfileabsolute(path) testEqual(find_file_portably(path), "Absolute path not allowed: " + path)

'This also indirectly tests findfiles a bit (Unix only)
startTest(find_file_portably_and_anycase)
  CONST tempdir = "_Testdir.tmp"
  CONST tempdir2 = tempdir + SLASH + "subDir"
  IF makedir(tempdir) <> 0 THEN fail
  IF makedir(tempdir2) <> 0 THEN fail
  touchfile(tempdir + "/Foo.Tmp")
  touchfile(tempdir2 + "/bar.TMP")
  #IFDEF __FB_UNIX__
    touchfile(tempdir + "/file1.TMP")
    touchfile(tempdir + "/FILE1.tmp")
    IF makedir(tempdir + "/Subdir1") <> 0 THEN fail
    #IFNDEF __FB_DARWIN__
      IF makedir(tempdir + "/SUBDIR1") <> 0 THEN fail
    #ENDIF
  #ENDIF

  ' Test finding files
  testfindfile(tempdir + "/foo.tmp",          tempdir + "/Foo.Tmp",  tempdir + "\foo.tmp")
  testfindfile(tempdir + "\Foo.tmp",          tempdir + "/Foo.Tmp",  tempdir + "\Foo.tmp")
  testfindfile(UCASE(tempdir) + "\foo.TMP",   tempdir + "/Foo.Tmp",  UCASE(tempdir) + "\foo.TMP")
  testfindfile(tempdir2 + "/..\foo.tmp",      tempdir + "/Foo.Tmp",  tempdir + "\foo.tmp")
  testfindfile(tempdir2 + "\../foo.tmp",      tempdir + "/Foo.Tmp",  tempdir + "\foo.tmp")
  testfindfile(tempdir2 + "/Bar.tmp",         tempdir2 + "/bar.TMP", tempdir2 + "\Bar.tmp")
  testfindfile(UCASE(tempdir2) + "\bar.tmp",  tempdir2 + "/bar.TMP", UCASE(tempdir2) + "\bar.tmp")
  ' Test finding directories
  testfindfile(tempdir,                       tempdir,  tempdir)
  testfindfile(tempdir2,                      tempdir2, tempdir2)
  testfindfile(UCASE(tempdir2),               tempdir2, UCASE(tempdir2))
  testfindfile(tempdir2 + "/",                tempdir2, tempdir2)
  testfindfile(tempdir2 + "\",                tempdir2, tempdir2)
  testfindfile(tempdir2 + "//.",              tempdir2, tempdir2)
  testfindfile(tempdir2 + "\\.",              tempdir2, tempdir2)
  testfindfile(tempdir2 + "/..",              tempdir,  tempdir)
  testfindfile(tempdir2 + "\..",              tempdir,  tempdir)
  ' Test files that don't exist
  testfilemissing(tempdir + "\Not.A.Directory\",        tempdir + SLASH + "Not.A.Directory")
  testfilemissing(tempdir + "/Not here",                tempdir + SLASH + "Not here")
  testfilemissing(tempdir2 + "\nor THERE",              tempdir2 + SLASH + "nor THERE")
  testfilemissing(tempdir + "/FLOOP\..\not anywhere!",  tempdir + SLASH + "not anywhere!")  ' Should simplify
  ' Test parent directories
  CHDIR tempdir2
  testfindfile("bar.tmp",                     "bar.TMP",    "bar.tmp")
  testfindfile("..\foo.tmp",                  "../Foo.Tmp", "..\foo.tmp")
  testfindfile("..\subdir",                   "../subDir",  "..\subdir")
  testfindfile("..",                          "..",         "..")
  CHDIR "../.."

  ' Test disallowed paths
  testfileabsolute("c:/Invalid")
  testfileabsolute("c:\Invalid")
  testfileabsolute("/Invalid")
  ' Test multiple files with same case-collapsed path (can't happen on Windows)
  ' Also skip this test on Mac because there the filesystem is usually case-insensitive, so this can't happen
  #IF defined(__FB_UNIX__) and not defined(__FB_DARWIN__)
    DIM normed as string = normalize_path(tempdir + "/")
    testEqual(find_file_portably(tempdir + "/file1.tmp"), _
              "Found multiple paths (in " + normed + ") with same case-insensitive name: [""FILE1.tmp"", ""file1.TMP""]")
    testEqual(find_file_portably(tempdir + "/subdir1/file"), _
              "Found multiple paths (in " + normed + ") with same case-insensitive name: [""SUBDIR1"", ""Subdir1""]")
  #ENDIF

  ' Test find_file_anycase
  testanycase(tempdir + "/foo.tmp",          tempdir + "/Foo.Tmp",  tempdir + "/foo.tmp")
  testanycase(tempdir + "/Foo.tmp",          tempdir + "/Foo.Tmp",  tempdir + "/Foo.tmp")
  CHDIR tempdir2
  testanycase("bar.tmp",                     CURDIR + "/bar.TMP",   "bar.tmp")
  testanycase(".." SLASH "foo.tmp",          ".." SLASH "Foo.Tmp", ".." SLASH "foo.tmp")
  '(Looking for a file, not a subdir)
  testanycase("../subdir",                   "../subdir",  "../subdir")
  testanycase("..",                          "..",         "..")
  ' Test wildcard support
  testanycase("*.tmp",                       CURDIR + "/bar.TMP",   CURDIR + "\bar.TMP")
  testanycase("bar.?mp",                     CURDIR + "/bar.TMP",   CURDIR + "\bar.TMP")
  testanycase("doesn't exist *",             "doesn't exist *",     "doesn't exist *")
  CHDIR "../.."

  killdir(tempdir, YES)  'recursively

  IF isdir(tempdir) THEN fail
endTest
#ENDIF


FUNCTION anycase (filename as string) as string
  'create a case-insensitive regex from a filename
#IFDEF __FB_WIN32__
  'Windows filenames are always case-insenstitive
  RETURN filename
#ELSE
  DIM ascii as integer
  DIM as string result = ""
  FOR i as integer = 1 TO LEN(filename)
    ascii = ASC(MID(filename, i, 1))
    IF ascii >= 65 AND ascii <= 90 THEN
      result = result + "[" + CHR(ascii) + CHR(ascii + 32) + "]"
    ELSEIF ascii >= 97 AND ascii <= 122 THEN
      result = result + "[" + CHR(ascii - 32) + CHR(ascii) + "]"
    ELSE
      result = result + CHR(ascii)
    END IF
  NEXT i
  RETURN result
#ENDIF
END FUNCTION

FUNCTION escape_filename (filename as string) as string
  'This is intended for escaping filenames for use in shells
#IFDEF __FB_UNIX__
  'Don't escape '
  RETURN """" & escape_string(filename, """`\$") & """"
#ELSE
  'Note " is not allowed in filenames
  IF RIGHT(filename, 1) = "\" ANDALSO RIGHT(filename, 2) <> "\\" THEN
   'If the filename ends in \ then the \ would escape the ", escape it
   RETURN """" & filename & "\"""
  ELSE
   RETURN """" & filename & """"
  END IF
#ENDIF
END FUNCTION

FUNCTION escape_filenamec CDECL (byval filename as zstring ptr) as zstring ptr
  DIM ret as string = escape_filename(*filename)
  DIM retz as zstring ptr = ALLOCATE(LEN(ret) + 1)
  strcpy retz, cstring(ret)
  RETURN retz
END FUNCTION

'Exclude chars disallowed in filenames (on any platform), and also
'wildcards that will be a problem if passed to findfiles. Printable ASCII only.
'Does NOT disallow characters that need escaping.
FUNCTION fixfilename (filename as string) as string
  '/\: should definitely be excluded.
  'For findfiles also exclude *?, and also [] (supported by fnmatch/findfiles only on Unix)
  'and <> (undocumented wildcards supported by Windows DIR command:
  ' https://ss64.com/nt/syntax-wildcards.html
  'Probably not supported by FB's DIR, but to be safe...)
  DIM temp as string = exclude(filename, "/\:*?[]<>")
  DIM result as string
  FOR i as integer = 0 TO LEN(temp) - 1
    SELECT CASE temp[i]
      CASE 32 TO 127 : result &= CHR(temp[i])
    END SELECT
  NEXT
  RETURN result
END FUNCTION

'This is called to check whether the name of lump file is valid. fixfilename is
'too restrictive for that, because script source files lumped into source.lumped
'are unrestricted.
FUNCTION lump_filename_valid (filename as string) as bool
  IF INSTR(filename, ANY "/\") THEN RETURN NO
  FOR i as integer = 0 TO LEN(filename) - 1
    IF filename[i] < 32 THEN RETURN NO
  NEXT
  RETURN YES
END FUNCTION

'The hostname part of a URL, eg HamsterRepublic.com
FUNCTION url_hostname (url as string) as string
  DIM ret as string = url
  DIM idx as integer
  idx = INSTR(ret, "://")
  IF idx THEN ret = MID(ret, idx + 3)
  idx = INSTR(ret, "/")
  IF idx THEN ret = LEFT(ret, idx - 1)
  RETURN ret
END FUNCTION

'This is a replacement for SHELL, meaning it invokes a command interpreter like
'cmd.exe instead of just running a program, and the standard search paths are searched,
'don't need to get a full path.
'Unlike SHELL on Windows, it doesn't pop up a terminal window.
'Also it needs to be used on Windows if the executable was escaped (so contains quotes).
'Returns the exit code, or -1 if it couldn't be run, or -2 if it timed out
'NOTE: use instead run_and_get_output and check stderr if you want better ability to catch errors
FUNCTION safe_shell (cmd as string, timeout as double = 5., log_it as bool = YES) as integer
  IF log_it THEN debuginfo cmd
#IFDEF __FB_WIN32__
  'SHELL wraps system() which calls cmd.exe (or command.com on non-NT Windows)
  DIM handle as ProcessHandle
  IF is_windows_9x() THEN
   handle = open_process("command.com", "/C " & cmd, YES, NO)
  ELSE
   'cmd.exe will remove the first and last quotes from the string and leave the rest.
   'Therefore, there need to be two quotes at the beginning of the string!
   handle = open_process("cmd.exe", "/C """ & cmd & """", YES, NO)
  END IF
  RETURN wait_for_process(@handle, timeout * 1000)
#ELSE
  ' Replacement for SHELL which checks the return code in more detail
  ' Doesn't timeout. Only implemented on Unix.
  RETURN checked_system(STRPTR(cmd))
#ENDIF
END FUNCTION

'Like safe_shell, but passes back the output in the 'stdout' string, and optionally 'stderr' in a string too.
'By default stderr also gets debuginfo logged, and cmd too when log_it is true.
'If stderr = "<ignore>" then stderr isn't captured, and writes to stderr pass through to our stderr stream.
'
'The return value is generally -1 on an error invoking the shell, -2 on timeout,
'-4444/-4445 on an error running or capturing the output,
'and otherwise the shell exit code, generally equal to the program exitcode and 0 on success.
'
'(There is a second implementation of this as run_process_and_get_output in os_unix.c
' which does't support stderr, but doesn't use temporary files or run the shell)
'(If you want to SEE the program's output in a terminal emulator, use spawn_and_wait instead)
FUNCTION run_and_get_output(cmd as string, byref stdout_s as string, byref stderr_s as string = "", log_it as bool = YES) as integer
  DIM ret as integer
  DIM as string stdout_file, stderr_file, cmdline
  DIM as bool grab_stderr
  grab_stderr = (stderr_s <> "<ignore>")
  IF is_windows_9x() THEN
   grab_stderr = NO
   stderr_s = "(Can't grab errors on Windows 9x/ME)"
  END IF

  stdout_file = tmpdir & "temp_stdout." & randint(1000000) & ".tmp"
  cmdline = cmd & " > " & escape_filename(stdout_file)
  IF grab_stderr THEN
    stderr_file = tmpdir & "temp_stderr." & randint(1000000) & ".tmp"
    ' This redirection works on Windows too with cmd.exe, but not command.com
    cmdline &= " 2> " & escape_filename(stderr_file)
  END IF
  ret = safe_shell(cmdline, , log_it)

  IF grab_stderr THEN
    IF isfile(stderr_file) THEN
      stderr_s = string_from_file(stderr_file)
      killfile stderr_file
    ELSE
      stderr_s = "(redirection failed)"
      ret = -4445
    END IF
  END IF

  IF isfile(stdout_file) THEN
    stdout_s = string_from_file(stdout_file)
    killfile stdout_file
  ELSE
    stdout_s = ""
    ret -= -4000
  END IF

  IF ret ORELSE (grab_stderr AND LEN(stderr_s)) THEN
   debuginfo "safe_shell(" & IIF(log_it, "", cmd) & ")=" & ret & " stderr:" & stderr_s
  END IF

  RETURN ret
END FUNCTION

'Create if it doesn't exist. Does NOT update its timestamp!
SUB touchfile (filename as string)
  DIM as integer fh
  IF OPENFILE(filename, FOR_BINARY + ACCESS_READ_WRITE, fh) THEN
    debug "touchfile(): could not open " + filename
    EXIT SUB
  END IF
  CLOSE #fh
END SUB

'Increases (never decreases) the length of a file by appending NUL bytes as required.
'Writing off the end of a file writes garbage between the new data and the end of the old file.
'Use this function to extend the file first.
SUB extendfile (byval fh as integer, byval length as integer)
 DIM curlen as integer = LOF(fh)
 IF curlen < length THEN
  DIM oldpos as integer = SEEK(fh)
  DIM buf(length - curlen - 1) as ubyte
  PUT #fh, curlen + 1, buf()
  SEEK #fh, oldpos
 END IF
END SUB

' Convert a UTF8 string to Latin-1. Any codepoints not in Latin-1 (ie above 255) get converted to '?'
' NOTE: you should normally use utf8_to_OHR instead!
FUNCTION utf8_to_latin1(utf8string as ustring) as string
  IF LEN(utf8string) = 0 THEN RETURN ""

  'Avoid FB's builtin conversion to wstring because it's locale-dependent
  DIM length as integer = utf8_length(strptr(utf8string))
  IF length < 0 THEN
    debug "utf8_length(" & utf8string & ") failed"
    'Maybe this garbage string is actually already latin-1? E.g. if some application
    'put a string in the clipboard without encoding it in UTF8
    RETURN utf8string ' "[CORRUPTED]"
  END IF

  DIM widestr as wstring ptr
  widestr = utf8_decode(strptr(utf8string), @length)
  IF widestr = NULL THEN RETURN "[CORRUPTED]"  'Shouldn't ever happen

  DIM ret as string = SPACE(length)
  length = wstring_to_latin1(widestr, strptr(ret), length + 1)
  ' The result might be shorter
  ret = LEFT(ret, length)
  DEALLOCATE widestr
  RETURN ret
END FUNCTION

' Just leaves all the icon characters alone
FUNCTION latin1_to_utf8(s as string) as ustring
  DIM buf as ustring = SPACE(LEN(s) * 2)  'At most a 2x blowup

  DIM outchar as ubyte ptr = @buf[0]
  FOR idx as integer = 0 TO LEN(s) - 1
    outchar += utf8_encode_char(outchar, s[idx])
  NEXT
  RETURN LEFT(buf, outchar - @buf[0])
END FUNCTION

' Translates from utf8 to the OS native multibyte string encoding (that is, to
' the OS codepage (for non-unicode programs) on Windows and to the LC_CTYPE
' encoding on Unix), which is used for printing to console.
' Returns original u8str on failure
FUNCTION utf8_to_mbs(u8str as ustring) as string
  'First convert from utf8 to wstring so we can call wcstombs
  DIM wcstr as wstring ptr = utf8_decode(strptr(u8str))
  IF wcstr = NULL THEN debug "utf8_decode failed" : RETURN u8str
  DIM length as integer = wcstombs(NULL, wcstr, 0)
  IF length < 0 THEN RETURN u8str
  DIM mbstr as zstring ptr = calloc(length + 1, 1)
  IF wcstombs(mbstr, wcstr, length) = cast(size_t, -1) THEN RETURN u8str
  RETURN *mbstr
END FUNCTION

' This translates a filename, e.g. returned from browse() or findfiles() to
' Latin-1 so it can be displayed normally.
' FIXME: check the font type instead of assuming Latin-1.
' FIXME: check the Windows ANSI codepage instead of assuming 1252 (extension of Latin-1).
'
' Filenames may be in various encodings depending on OS, filesystem, and locale.
' In practice, on Unix the encoding might be anything, and is determined by
' the LANG and LC_CTYPE envvars.
' On Windows, there are ANSI (8-bit) and UTF-16 filenames for each file, depending
' on which variant of winapi functions get called; FB uses the ANSI ones
' meaning filenames are encoded in the system codepage, often Windows-1252
' (an extension of Latin1).
' FIXME: filenames stored as 8-bit (ANSI) strings on Windows can't be used to open
' filenames that can't be encoded in the ANSI codepage; FindNextFileA does lossy conversion.
'
' The engine recieves filenames in the unknown encoding, treats them as byte
' strings (aside from dependable ASCII characters like / \ .) and then hands
' them back to the OS, and must never attempt to muck with the encoding
' along the way. This function must ONLY be used for display, as it is lossy!
FUNCTION decode_filename(filename as string) as string
  IF LEN(filename) = 0 THEN RETURN filename
#ifdef __FB_UNIX__
  DIM length as integer
  DIM unicode as wstring ptr

#ifdef __FB_ANDROID__
  'Android NDK doesn't support mbstowcs or non-C locales (only exposed to Java apps),
  'and is always UTF8
  length = utf8_length(strptr(filename))
  IF length < 0 THEN
    debuginfo "decode_filename(" & filename & ") failed, " & length
    RETURN filename
  END IF
  unicode = utf8_decode(strptr(filename), @length)
  IF unicode = NULL THEN RETURN filename  'Shouldn't happen

#else
/' This is just equivalent to assigning a string to a wstring!
  length = mbstowcs(NULL, STRPTR(filename), 0)
  IF length = -1 THEN
    debuginfo "decode_filename(" & filename & ") failed"
    RETURN filename   'not valid UTF-8 (Note: we continue on valid ASCII)
  END IF
  unicode = allocate(SIZEOF(wstring) * (length + 1))
  mbstowcs(unicode, STRPTR(filename), length + 1)
'/
  length = LEN(filename)
  unicode = allocate(SIZEOF(wstring) * (length + 1))
  *unicode = filename
#endif

  DIM ret as string = SPACE(length)
  length = wstring_to_latin1(unicode, strptr(ret), length + 1)
  ' The result might be shorter
  ret = LEFT(ret, length)
  deallocate unicode

#elseif defined(__FB_WIN32__)

  'Internally FB uses legacy ANSI file IO functions, so all filenames are in
  'the system ANSI codepage, if possible (they might contain ?'s and be impossible to open!)
  'FIXME: we assume the system codepage is Windows-1252, which is extremely wrong!
  'Convert Windows-1252 to Latin-1 by removing the extra characters
  '(There's little point doing this)
  DIM ret as string = filename
  FOR i as integer = 0 TO LEN(ret) - 1
    IF ret[i] >= 127 AND ret[i] <= 160 THEN
      ret[i] = ASC("?")
    END IF
  NEXT

#endif

  'debug "decode_filename(" & filename & ") = " & ret
  RETURN ret
END FUNCTION

'Finds files in a directory, writing them into an array without their path
'(If you want to find a single file, use find_file_portably())
'filelist() must be resizeable; it'll be resized so that LBOUND = -1, with files, if any, in filelist(0) up
'By default, find all files in directory, otherwise namemask is a case-insensitive filename mask
'filetype is one of fileTypeFile, fileTypeDirectory, fileTypeFileOrDir
SUB findfiles (directory as string, namemask as string = "", filetype as FileTypeEnum = fileTypeFile, findhidden as bool = NO, filelist() as string)
  REDIM filelist(-1 TO -1)
  IF directory = "" THEN
   ' For safety and bug catching: for example deletetemps() calls findfiles
   ' and then deletes everything.
   showbug "findfiles called with empty directory"
   EXIT SUB
  END IF
  IF filetype <> fileTypeDirectory and filetype <> fileTypeFile and filetype <> fileTypeFileOrDir THEN
   showbug "findfiles: bad filetype"
   EXIT SUB
  END IF
  DIM as string searchdir = add_trailing_slash(directory)
  DIM as string nmask = anycase(namemask)
  IF LEN(nmask) = 0 THEN nmask = ALLFILES
#ifdef DEBUG_FILE_IO
  DIM filetypestr as string = "fileTypeFile"
  IF filetype = fileTypeDirectory THEN filetypestr = "fileTypeDirectory"
  IF filetype = fileTypeFileOrDir THEN filetypestr = "fileTypeFileOrDir"
  debuginfo "findfiles(directory = " & directory & ", namemask = " & namemask & ", " _
            & filetypestr & ", findhidden = " & findhidden & ")"
#endif

#ifdef __FB_UNIX__
  DIM filenames as string vector

  IF filetype = fileTypeDirectory THEN
    v_move filenames, list_subdirs(searchdir, nmask, findhidden)
  ELSEIF filetype = fileTypeFile THEN
    v_move filenames, list_files(searchdir, nmask, findhidden)
  ELSE
    v_move filenames, list_files_or_subdirs(searchdir, nmask, findhidden, -1)
  END IF
        
  FOR i as integer = 0 TO v_len(filenames) - 1
    IF filetype = fileTypeDirectory THEN
     'Filter out some Linux dirs that we should not be browsing around in.
     IF filenames[i] = "dev" ORELSE filenames[i] = "proc" ORELSE filenames[i] = "sys" THEN CONTINUE FOR
     'Maybe we should filter out some other dirs on Mac and on Android?
    END IF
    a_append filelist(), filenames[i]
  NEXT

  v_free filenames

#else
  'FIXME: On Windows, non-unicode-enabled programs automatically get their filenames downconverted to the current
  '"ANSI" codepage. If any character in the filename isn't in that codepage, then opening the file is impossible,
  'and DIR will return a useless filename containing ?'s. (If we're searching for directories, the
  'extra DIR call to test the filetype will then filter it out).
  'If the filename does fit in the codepage we can open it, but we use Latin-1 internally (a subset
  'of Windows-1252), and if the filename has characters not in Latin-1 then they'll display wrong, as something
  'else. Ee want to support more than just Latin-1 filenames, we will have to rewrite
  'this properly, using winapi calls, because FB's DIR has no support. Or fix FB itself.

  DIM foundfile as string
  DIM attrib as integer
  /'---Windows directory attributes
  CONST attribReadOnly = 1
  CONST attribHidden = 2
  CONST attribSystem = 4
  ' 8 is not used
  CONST attribDirectory = 16
  CONST attribArchive = 32
  CONST attribDevice = 64
  CONST attribNormal = 128  '"A file that does not have other attributes set." (Not a real attribute?)
  CONST attribReserved = 64+128
  CONST attribAlmostAll = 255-16-2 ' All except directory and hidden
  '/
  ' Recall that DIR returns all things in a directory except those
  ' with a bit that we didn't specify
  IF filetype = fileTypeDirectory THEN
    attrib = 32+16+4+1
  ELSEIF filetype = fileTypeFile THEN
    attrib = 255 XOR (16+2)
  ELSE
    attrib = 255 XOR 2
  END IF
  IF findhidden THEN attrib += 2
  foundfile = DIR(searchdir + nmask, attrib)
  IF foundfile = "" THEN EXIT SUB
  REDIM tempfilelist(-1 TO -1) as string
  DO UNTIL foundfile = ""
    a_append tempfilelist(), foundfile
    foundfile = DIR '("", attrib)
  LOOP
  FOR i as integer = 0 TO UBOUND(tempfilelist)
    foundfile = tempfilelist(i)
    IF foundfile = "." ORELSE foundfile = ".." THEN CONTINUE FOR
    IF filetype = fileTypeDirectory THEN
      'alright, we want directories, but DIR is too broken to give them to us
      'files with attribute 0 appear in the list, so single those out
      IF DIR(searchdir + foundfile, 32+16+4+2+1) = "" OR DIR(searchdir + foundfile, 32+4+2+1) <> "" THEN CONTINUE FOR
    END IF
    a_append filelist(), foundfile
  NEXT

  'If DIR is not called until it returns "" then internally it holds a HANDLE for the search,
  'which makes the directory on which it was last run undeletable. So reset it by
  'doing another search (C:\ is actually invalid, the search string can't end in \)
  DIR("C:\")

#endif
END SUB

'Returns true on success
FUNCTION writeablecopyfile(src as string, dest as string) as bool
 IF copyfile(src, dest) = NO THEN RETURN NO
 #IFDEF __FB_WIN32__
  IF setwriteable(dest, YES) = NO THEN RETURN NO
 #ENDIF
 RETURN YES
END FUNCTION

'Copy files in one directory to another (ignores directories)
SUB copyfiles(src as string, dest as string, copyhidden as bool = NO, lowercase as bool = NO)
 DIM filelist() as string
 findfiles src, ALLFILES, fileTypeFile, copyhidden, filelist()
 FOR i as integer = 0 TO UBOUND(filelist)
  DIM destname as string = IIF(lowercase, LCASE(filelist(i)), filelist(i))
  writeablecopyfile src + SLASH + filelist(i), dest + SLASH + destname
 NEXT
END SUB

FUNCTION copydirectory (src as string, dest as string, byval copyhidden as bool = YES) as string
 'Recursively copy directory src to directory dest. Dest should not already exist
 'returns "" on success, or an error string on failure. Failure might leave behind a partial copy.
 IF isdir(dest) THEN RETURN "copydirectory: Destination """ & dest & """ must not already exist"
 
 '--create the dest directory
 IF makedir(dest) <> 0 THEN RETURN "copydirectory: Couldn't create """ & dest & """"

 '--copy all the files
 DIM filelist() as string
 findfiles src, ALLFILES, fileTypeFile, copyhidden, filelist()
 FOR i as integer = 0 TO UBOUND(filelist)
  writeablecopyfile src & SLASH & filelist(i), dest & SLASH & filelist(i)
  IF NOT isfile(dest & SLASH & filelist(i)) THEN
   RETURN "copydirectory: Couldn't copy file """ & dest & SLASH & filelist(i) & """"
  END IF
 NEXT i

 '--recursively copy all the subdirectories
 DIM result as string = ""
 DIM dirlist() as string
 findfiles src, ALLFILES, fileTypeDirectory, copyhidden, dirlist()
 FOR i as integer = 0 TO UBOUND(dirlist)
  IF dirlist(i) = "." ORELSE dirlist(i) = ".." THEN CONTINUE FOR
  result = copydirectory(src & SLASH & dirlist(i), dest & SLASH & dirlist(i), copyhidden)
  IF result <> "" THEN RETURN result
 NEXT i
 
 RETURN ""
 
END FUNCTION

SUB killdir(directory as string, recurse as bool = NO)
#ifdef DEBUG_FILE_IO
  debuginfo "killdir(" & directory & ", recurse = " & recurse & ")"
#endif
  close_lazy_files
  DIM filetype as FileTypeEnum = get_file_type(directory)
  IF filetype <> fileTypeDirectory THEN
    DIM msg as string
    msg = "killdir: '" & directory & "' is not a directory, it is " & safe_caption(filetype_names(), filetype, "filetype") & ". Skipping."
    IF filetype <> fileTypeNonexistent THEN visible_debug msg ELSE debuginfo msg
    EXIT SUB
  END IF
  ' For safety. (You ought to pass absolute paths.) Check
  ' writability so we don't recurse if started from e.g. /home until
  ' we hit something deletable (this happened to me)!
  IF LEN(directory) < 5 ORELSE diriswriteable(directory) = NO THEN
   showbug "killdir: refusing suspicious attempt to delete directory '" & directory & "'"
   EXIT SUB
  END IF
  DIM filelist() as string
  findfiles directory, ALLFILES, fileTypeFile, YES, filelist()
  FOR i as integer = 0 TO UBOUND(filelist)
    killfile directory + SLASH + filelist(i)
  NEXT
  IF recurse THEN
   DIM dirlist() as string
   findfiles directory, ALLFILES, fileTypeDirectory, YES, dirlist()
   FOR i as integer = 0 TO UBOUND(dirlist)
    IF dirlist(i) = "." ORELSE dirlist(i) = ".." THEN CONTINUE FOR
    'debuginfo "recurse to " & directory & SLASH & dirlist(i)
    killdir directory & SLASH & dirlist(i), YES
   NEXT i
  END IF
  IF RMDIR(directory) THEN
    'errno would get overwritten while building the error message
    DIM err_string as string = *get_sys_err_string()
    debug "Could not rmdir(" & directory & "): " & err_string
  END IF
'  IF isdir(directory) THEN
'    debug "Failed to delete directory " & directory
'  END IF
END SUB

'Returns zero on success (including if already exists)
FUNCTION makedir (directory as string) as integer
  IF isdir(directory) THEN
    debuginfo "makedir: " & directory & " already exists"
    RETURN 0
  END IF
#ifdef DEBUG_FILE_IO
  debuginfo "makedir(" & directory & ")"
#endif
  IF MKDIR(directory) THEN
    'errno would get overwritten while building the error message
    DIM err_string as string = *get_sys_err_string()
    'The heck? On Windows at least, MKDIR throws this false error
#ifdef __FB_WIN32__
    IF err_string = "File exists" THEN RETURN 0
#endif
    debug "Could not mkdir(" & directory & "): " & err_string
    RETURN 1
  END IF
#ifdef __FB_UNIX__  ' I don't know on which OSes this is necessary
  ' work around broken file permissions in dirs created by linux version
  ' MKDIR creates with mode 644, should create with mode 755
  safe_shell "chmod +x " + escape_filename(directory), , NO
#endif
  RETURN 0
END FUNCTION

'True on successful deletion, false if couldn't or didn't exist
FUNCTION killfile (filename as string) as bool
  'KILL is a thin wrapper around C's remove(), however by calling it directly we can get a textual error message
#ifdef DEBUG_FILE_IO
  debuginfo "killfile(" & filename & ")"
#endif
  close_lazy_files
  IF remove(strptr(filename)) THEN
    DIM err_string as string = *get_sys_err_string()
    debug "Could not remove(" & filename & "): " & err_string

    'NOTE: on Windows, even if deletion doesn't happen because the file is open, the file will be marked
    'to be deleted once everyone closes it. Also, it will no longer be possible to open it.
    'However if someone opened the file without FILE_SHARE_DELETE access, it can't be deleted.
    'Note that FB's OPEN and OPENFILE don't open with FILE_SHARE_DELETE!
    'On Unix, you can unlink a file even when someone else has it open.
    RETURN NO
  END IF
  ' FIXME: send a message to Game if live-previewing, or else special case all
  ' places where it matters ie. music/sfx (like we do for RELOAD's use of renamefile)
  RETURN YES
END FUNCTION

'True on success or didn't exist, false if couldn't delete.
'Call this instead of killfile if you're not sure the file exists, it avoid error messages if it doesn't.
FUNCTION safekill (filename as string) as bool
  DIM exists as bool = real_isfile(filename)
  IF exists THEN RETURN killfile(filename)
#ifdef DEBUG_FILE_IO
  debuginfo "safekill(" & filename & ") exists = NO"
#endif
  RETURN YES
END FUNCTION

SUB safekill_pattern (dirname as string, filepattern as string)
 'Is this "safe"? That is "debatable"
 DIM filelist() as string
 IF LEN(dirname) = 0 THEN
  debuginfo "safekill_pattern: empty dirname, do nothing."
  EXIT SUB
 END IF
 IF NOT isdir(dirname) THEN
  debuginfo "safekill_pattern: """ & dirname & """ directory does not exist, do nothing."
  EXIT SUB
 END IF
 'findfiles is always case-insensitive
 findfiles dirname, filepattern, fileTypeFile, YES, filelist()
 FOR i as integer = 0 TO UBOUND(filelist)
  safekill dirname & SLASH & filelist(i)
 NEXT i
END SUB

'Looking for local_file_move? Now called renamefile, in filelayer.cpp

FUNCTION fileisreadable(filename as string) as bool
  if len(filename) = 0 then debug "fileisreadable: no filename" : return NO
  dim ret as bool = NO
  dim fh as integer, err_code as integer

  ' Check this first, to exclude directories on Linux (you can open a directory read-only)
  if get_file_type(filename) = fileTypeFile then
    err_code = openfile(filename, for_binary + access_read, fh)
    if err_code <> fberrOK then
      'if err_code fberrNOTFOUND, it doesn't exist. Shouldn't happen unless there's a race condition
      debuginfo "fileisreadable: Error " & err_code & " reading " & filename
    else
      close #fh
      ret = YES
    end if
  end if
#ifdef DEBUG_FILE_IO
  debuginfo "fileisreadable(" & filename & ") = " & ret
#endif
  return ret
END FUNCTION

' Whether an existing file can be opened for writing, or else if a new file can be written.
FUNCTION fileiswriteable(filename as string) as bool
  if len(filename) = 0 then debug "fileiswriteable: no filename" : return NO
  dim ret as bool = NO
  dim fh as integer
  dim exists as bool = (get_file_type(filename) <> fileTypeNonexistent)
  ' Attempting to open read-write means that opening directories fails on Linux, unlike read-only
  if openfile(filename, for_binary + access_read_write, fh) = fberrOK then
    close #fh
    ' Delete the file we just created
    if exists = NO then killfile(filename)
    ret = YES
  end if
#ifdef DEBUG_FILE_IO
  debuginfo "fileiswriteable(" & filename & ") = " & ret & " exists=" & exists
#endif
  return ret
END FUNCTION

FUNCTION diriswriteable(filename as string) as bool
  dim ret as bool = NO
  dim testfile as string
  dim testdir as string = filename
  if filename = "" then testdir = curdir

  ' Kludge to detect an rpgdir full of unwriteable files: on Windows you don't seem
  ' able to mark a folder read-only, instead it makes the contents read-only.
  ' (If testdir isn't a directory, these checks will print debug messages,
  ' which I think is a good thing.)
  testfile = testdir + SLASH + "archinym.lmp"
  if real_isfile(testfile) = NO then
    ' If archinym.lmp doesn't exist, then ohrrpgce.gen does
    testfile = testdir + SLASH + "ohrrpgce.gen"
    if real_isfile(testfile) = NO then testfile = ""
  end if
  ' In the case of an .rpgdir, we test both an existing file and a new one for writability.
  if len(testfile) andalso fileiswriteable(testfile) = NO then
    ret = NO
  else
    testfile = testdir & SLASH & "__testwrite_" & randint(100000) & ".tmp"
    if fileiswriteable(testfile) then
      ret = YES
    end if
  end if
  #ifdef DEBUG_FILE_IO
    debuginfo "diriswriteable(" & filename & ") = " & ret
  #endif
  return ret
END FUNCTION

' This is a simple wrapper for fileisreadable, and there's now a lot of code
' that might depend on that. If you want to *really* test if something is a file, use real_isfile
FUNCTION isfile (filename as string) as bool
  return fileisreadable(filename)
END FUNCTION

FUNCTION real_isfile(filename as string) as bool
  if len(filename) = 0 then debug "real_isfile: no filename" : return NO
  dim filetype as FileTypeEnum = get_file_type(filename)
  dim ret as bool = (filetype = fileTypeFile)
  #ifdef DEBUG_FILE_IO
    debuginfo "real_isfile(" & filename & ") = " & ret
    IF ret = NO ANDALSO filetype <> fileTypeNonexistent THEN debuginfo " filetype=" & filetype
  #endif
  return ret
END FUNCTION

'Check whether a filename doesn't refer to a file/dir/etc
FUNCTION is_not_file (filename as string) as bool
  dim filetype as FileTypeEnum = get_file_type(filename)
  dim ret as bool = (filetype = fileTypeNonexistent)
  #ifdef DEBUG_FILE_IO
    debuginfo "is_not_file(" & filename & ") = " & ret & " filetype=" & filetype
  #endif
  return ret
END FUNCTION

' Is a directory. Return true for "" (the current directory)
FUNCTION isdir (filename as string) as bool
  dim ret as bool = (get_file_type(filename) = fileTypeDirectory)
  #ifdef DEBUG_FILE_IO
    debuginfo "isdir(" & filename & ") = " & ret
  #endif
  return ret
END FUNCTION


'--------- File read/write helpers ---------

function ReadShort(byval fh as integer, byval p as long=-1) as short
  dim ret as short
  if p = -1 then
    get #fh, , ret
  elseif p >= 0 then
    get #fh, p, ret
  end if
  return ret
end function

function ReadShort(filename as string, byval p as integer) as short
  dim ret as short
  dim fh as integer
  openfile(filename, for_binary + access_read, fh)
  get #fh, p, ret
  close #fh
  return ret
end function

function ReadByte(byval fh as integer, byval p as long=-1) as ubyte
  dim ret as ubyte
  if p = -1 then
    get #fh, , ret
  elseif p >= 0 then
    get #fh, p, ret
  end if
  return ret
end function

sub WriteShort(byval fh as integer, byval p as long, byval v as integer)
  WriteShort(fh,p,cshort(v))
end sub

sub WriteShort(byval fh as integer, byval p as long, byval v as short)
  if p = -1 then
    put #fh, , v
  elseif p >= 0 then
    put #fh, p, v
  end if
end sub

sub WriteShort(filename as string, byval p as integer, byval v as integer)
  dim fh as integer
  openfile(filename, FOR_BINARY, fh)
  put #fh, p, cshort(v)
  close #fh
end sub

sub WriteByte(byval fh as integer, byval v as ubyte, byval p as long=-1)
  if p = -1 then
    put #fh, , v
  elseif p >= 0 then
    put #fh, p, v
  end if
end sub

function ReadVStr(byval fh as integer, byval maxlen as integer) as string
  dim length as short, ret as string, c as short, i as integer
  length = readshort(fh)

  for i = 0 to maxlen - 1
    c = readshort(fh)
    if i < length then ret &= chr(c and 255)
  next

  return ret
end function

sub WriteVStr(byval fh as integer, byval maxlen as integer, s as string)
  dim i as integer
  writeshort(fh, -1, small(maxlen, len(s)))

  for i = 0 to maxlen - 1
    if i < len(s) then writeshort(fh, -1, cint(s[i])) else writeshort(fh, -1, 0)
  next
end sub

function ReadByteStr(byval fh as integer, byval maxlen as integer) as string
  dim length as short, ret as string, c as ubyte, i as integer
  length = readshort(fh)

  for i = 0 to maxlen - 1
    c = readbyte(fh)
    if i < length then ret = ret & chr(c)
  next

  return ret
end function

sub WriteByteStr(byval fh as integer, byval maxlen as integer, s as string)
  dim i as integer
  writeshort(fh, -1, small(maxlen, len(s)))

  for i = 0 to maxlen - 1
    if i < len(s) then writebyte(fh, cubyte(s[i])) else writebyte(fh, 0)
  next
end sub


'--------- Doubly Linked List ---------

#define DLFOLLOW(someptr)  cast(DListItem(Any) ptr, cast(byte ptr, someptr) + this.memberoffset)

SUB dlist_construct (byref this as DoubleList(Any), byval itemoffset as integer)
  this.numitems = 0
  this.first = NULL
  this.last = NULL
  this.memberoffset = itemoffset
END SUB

'NULL as beforeitem inserts at end
SUB dlist_insertat (byref this as DoubleList(Any), byval beforeitem as any ptr, byval newitem as any ptr)
  dim litem as DListItem(Any) ptr = DLFOLLOW(newitem)

  litem->next = beforeitem

  if beforeitem = NULL then
    litem->prev = this.last
    this.last = newitem
  else
    dim bitem as DListItem(Any) ptr = DLFOLLOW(beforeitem)
    litem->prev = bitem->prev
    bitem->prev = newitem
  end if

  if litem->prev then
    DLFOLLOW(litem->prev)->next = newitem
  else
    this.first = newitem
  end if

  this.numitems += 1
END SUB

SUB dlist_remove (byref this as DoubleList(Any), byval item as any ptr)
  dim litem as DListItem(Any) ptr = DLFOLLOW(item)

  'check whether item isn't the member of a list
  if litem->next = NULL andalso item <> this.last then exit sub

  if litem->prev then
    DLFOLLOW(litem->prev)->next = litem->next
  else
    this.first = litem->next
  end if
  if litem->next then
    DLFOLLOW(litem->next)->prev = litem->prev
  else
    this.last = litem->prev
  end if
  litem->next = NULL
  litem->prev = NULL

  this.numitems -= 1
END SUB

SUB dlist_swap (byref this as DoubleList(Any), byval item1 as any ptr, byref that as DoubleList(Any), byval item2 as any ptr)
  'dlist_insertat can't move items from one list to another
  if item1 = item2 then exit sub
  dim dest2 as any ptr = DLFOLLOW(item1)->next
  dlist_remove(this, item1)
  if dest2 = item2 then
    'items are arranged like  -> item1 -> item2 ->
    dlist_insertat(that, DLFOLLOW(item2)->next, item1)
  else
    dlist_insertat(that, item2, item1)
    dlist_remove(that, item2)
    dlist_insertat(this, dest2, item2)
  end if
END SUB

FUNCTION dlist_find (byref this as DoubleList(Any), byval item as any ptr) as integer
  dim n as integer = 0
  dim lit as any ptr = this.first
  while lit
    if lit = item then return n
    n += 1
    lit = DLFOLLOW(lit)->next
  wend
  return -1
END FUNCTION

FUNCTION dlist_walk (byref this as DoubleList(Any), byval item as any ptr, byval n as integer) as any ptr
  if item = NULL then item = this.first
  while n > 0 andalso item
    item = DLFOLLOW(item)->next
    n -= 1
  wend
  while n < 0 andalso item
    item = DLFOLLOW(item)->prev
    n += 1
  wend
  return item
END FUNCTION

/'
SUB dlist_print (byref this as DoubleList(Any))
  dim ptt as any ptr = this.first
  debug "numitems=" & this.numitems & " first=" & hex(ptt) & " last=" & hex(this.last) & " items:"
  while ptt
    debug " 0x" & hex(ptt) & " n:0x" & hex(DLFOLLOW(ptt)->next) & " p:0x" & hex(DLFOLLOW(ptt)->prev) '& " " & get_menu_item_caption(*ptt, menudata)
    ptt = DLFOLLOW(ptt)->next
  wend
END SUB
'/

'------------- Hash Table -------------
'See util.bi for documentation.

DEFINE_VECTOR_OF_TYPE(HashBucketItem, HashBucketItem)

sub HashTable.construct(tablesize as integer = 31)
  this.destruct()
  this.numitems = 0
  if tablesize > 4096 then tablesize = 4096  'Prevent overflow of iter() 'state'
  this.tablesize = tablesize
  this.table = callocate(sizeof(any ptr) * this.tablesize)
  this.key_compare = NULL
  this.key_hash = NULL
  this.key_length = 0
  this.key_copy = NULL
  this.key_delete = NULL
  this.key_is_integer = YES
  this.value_copy = NULL
  this.value_delete = NULL
  this.value_is_string = NO
  this.value_is_zstring = NO
end sub

sub HashTable.construct(tablesize as integer = 31, key_type as TypeTable, copy_and_delete_keys as bool, value_type as TypeTable, copy_and_delete_values as bool)
  this.construct(tablesize)
  if @key_type <> @type_table(integer) then
    this.key_compare = key_type.comp
    this.key_hash = key_type.hash
    this.key_length = key_type.element_len
    if copy_and_delete_keys then
      this.key_copy = key_type.copy
      this.key_delete = key_type._delete
    end if
    this.key_is_integer = NO
  end if
  if @value_type <> @type_table(integer) then
    if copy_and_delete_values then
      this.value_copy = value_type.copy
      this.value_delete = value_type._delete
    end if
  end if
  this.value_is_string = (@value_type = @type_table(string))
  this.value_is_zstring = (@value_type = @type_table(zstring))
end sub

sub HashTable.destruct()
  this.clear()
  deallocate(this.table)
  this.table = NULL
  this.tablesize = 0
end sub

destructor HashTable()
  this.destruct()
end destructor

function HashTable.constructed() as bool
  return this.tablesize > 0
end function

'Look for a key in a bucket vector, return NULL on failure
local function hash_search_bucket(this as HashTable, bucket as HashBucketItem vector, hash as integer, key as any ptr = NULL) as HashBucketItem ptr
  for bucketidx as integer = 0 to v_len(bucket) - 1
    dim it as HashBucketItem ptr = @bucket[bucketidx]
    if it->hash = hash then
      if it->key = key then
        return it
      elseif this.key_compare then
        'One or both keys may be NULL
        if this.key_compare(it->key, key) = 0 then
          return it
        end if
      else
        'showbug "HashTable: need a key compare function"
        'Shallow comparison of key pointers: take as inequal
      end if
    end if
  next
  return NULL
end function

function HashTable.hash_key(key as any ptr) as integer
  if this.key_hash then
    return this.key_hash(key)
  elseif key then
    if this.key_length = 0 then showbug "HashTable: can't hash ptr; use TypeTable constructor"
    return stringhash(key, this.key_length)
  end if
  return 0
end function

sub HashTable.add(hash as integer, value as any ptr, _key as any ptr = NULL)
  BUG_IF(this.table = NULL, "construct() not called")
  dim byref bucket as HashBucketItem vector = this.table[cuint(hash) mod this.tablesize]
  dim item as HashBucketItem ptr = any
  if bucket = NULL then
    v_new bucket, 1
    item = @bucket[0]
  else
    item = v_expand(bucket)
  end if
  item->hash = hash
  item->key = iif(this.key_copy, this.key_copy(_key), _key)
  item->value = iif(this.value_copy, this.value_copy(value), value)
  this.numitems += 1
end sub

sub HashTable.add(hash as integer, value as integer)
  this.add(hash, canyptr(value), NULL)
end sub

sub HashTable.add(key as any ptr, value as any ptr)
  this.add(this.hash_key(key), value, key)
end sub

sub HashTable.add(key as any ptr, value as integer)
  this.add(this.hash_key(key), canyptr(value), key)
end sub

sub HashTable.set(hash as integer, value as any ptr, _key as any ptr = NULL)
  BUG_IF(this.table = NULL, "construct() not called")
  dim bucket as HashBucketItem vector = this.table[cuint(hash) mod this.tablesize]
  dim it as HashBucketItem ptr = hash_search_bucket(this, bucket, hash, _key)
  if it then
    if this.value_delete andalso it->value then this.value_delete(it->value)
    it->value = iif(this.value_copy, this.value_copy(value), value)
  else
    this.add(hash, value, _key)
  end if
end sub

sub HashTable.set(hash as integer, value as integer)
  this.set(hash, canyptr(value), NULL)
end sub

sub HashTable.set(key as any ptr, value as any ptr)
  this.set(this.hash_key(key), value, key)
end sub

sub HashTable.set(key as any ptr, value as integer)
  this.set(this.hash_key(key), canyptr(value), key)
end sub

function HashTable.get(hash as integer, default as any ptr = NULL, _key as any ptr = NULL) as any ptr
  BUG_IF(this.table = NULL, "construct() not called", NULL)
  dim bucket as HashBucketItem vector = this.table[cuint(hash) mod this.tablesize]
  dim it as HashBucketItem ptr = hash_search_bucket(this, bucket, hash, _key)
  if it = NULL then return default
  return it->value
end function

function HashTable.get(key as any ptr, default as any ptr = 0) as any ptr
  return this.get(this.hash_key(key), default, key)
end function

function HashTable.get_int(hash as integer, default as integer = 0) as integer
  return cintptr32(this.get(hash, canyptr(default), NULL))
end function

function HashTable.get_int(key as any ptr, default as integer = 0) as integer
  return cintptr32(this.get(this.hash_key(key), canyptr(default), key))
end function

function HashTable.get_str(hash as integer, default as zstring ptr = @"", _key as any ptr = NULL) as string
  'return *cast(string ptr, this.get(hash, @default, _key))
  'Avoiding initialising a new string from default if not needed, but want to still allow NULL as a value
  dim ret as any ptr = this.get(hash, canyptr(-1234), _key)
  if ret = canyptr(-1234) then return *default
  if this.value_is_string then
    return *cast(string ptr, ret)
  elseif this.value_is_zstring then
    return *cast(zstring ptr, ret)
  else
    showbug "HashTable.get_str shouldn't be called!"
  end if
end function

function HashTable.get_str(key as any ptr, default as zstring ptr = @"") as string
  return this.get_str(this.hash_key(key), default, key)
  'if ret = canyptr(-1) then return *default else return *ret
end function

function HashTable.remove(hash as integer, _key as any ptr = NULL) as bool
  dim byref bucket as HashBucketItem vector = this.table[cuint(hash) mod this.tablesize]
  dim it as HashBucketItem ptr = hash_search_bucket(this, bucket, hash, _key)
  if it = NULL then return NO
  if this.key_delete andalso it->key then this.key_delete(it->key)
  if this.value_delete andalso it->value then this.value_delete(it->value)
  dim idx as integer = it - @bucket[0]
  v_delete_slice bucket, idx, idx + 1
  this.numitems -= 1
  return YES
end function

function HashTable.remove(key as any ptr) as bool
  return this.remove(this.hash_key(key), key)
end function

sub HashTable.clear()
  for bucketnum as integer = 0 to this.tablesize - 1
    dim byref bucket as HashBucketItem vector = this.table[bucketnum]
    for bucketidx as integer = 0 to v_len(bucket) - 1
      with bucket[bucketidx]
        if this.key_delete andalso .key then this.key_delete(.key)
        if this.value_delete andalso .value then this.value_delete(.value)
      end with
    next
    v_free bucket
  next
  this.numitems = 0
end sub

function HashTable.iter(byref state as uinteger, prev_value as any ptr, byref key as any ptr = NULL) as any ptr
  if state = &hFFFFFFFF then
    key = NULL
    return NULL
  end if

  'If the length of a bucket exceeds &hfffff = 1048575, then we are in trouble!
  'However, even then iteration will still work if you delete each item you iterate over.
  dim as integer bucketnum = state shr 20, bucketidx = state and &hfffff

  if state <> 0 then
    'Check whether something got deleted from the hash table since last .iter(), upsetting the iteration,
    'and if so search backwards in this bucket to find the correct bucketidx.
    'NOTE: this assumes values are unique!!
    '(If new items got added to the hash table they get added to the end of a bucket, which is no problem.
    'They may or may get included in the iteration.)

    dim bucket as HashBucketItem vector = this.table[bucketnum]
    'bucketidx was incremented on the last iter() call, so we expect to see prev_value at bucketidx-1
    bucketidx -= 1
    bucketidx = small(bucketidx, v_len(bucket) - 1)  'The bucket might have shrunk
    while bucketidx >= 0
      if bucket[bucketidx].value = prev_value then exit while
      bucketidx -= 1
    wend
    if bucketidx < 0 then
      showbug "HashTable.iter: item has been deleted from the table"
      state = &hFFFFFFFF
      return NULL
    end if
    'Increment past prev_value
    bucketidx += 1
  end if

  'Advance to next non-empty bucket if we've reached the end of this one
  while bucketidx >= v_len(this.table[bucketnum])
    bucketnum += 1
    bucketidx = 0
    if bucketnum >= this.tablesize then
      key = NULL
      state = &hFFFFFFFF
      return NULL
    end if
  wend

  state = (bucketnum shl 20) or (bucketidx + 1)  '+1 to advance to next item on next call
  with this.table[bucketnum][bucketidx]
    key = .key
    return .value
  end with
end function

function HashTable.keys() as any ptr vector
  dim ret as any ptr vector
  'v_new ret, 0, this.numitems
  ' dim typetbl as TypeType ptr
  ' if typetbl = NULL then typetbl = @type_table(any_ptr)
  array_new ret, this.numitems, 0, iif(this.key_is_integer, @type_table(integer), @type_table(any_ptr))

  for bucketnum as integer = 0 to this.tablesize - 1
    dim byref bucket as HashBucketItem vector = this.table[bucketnum]
    for bucketidx as integer = 0 to v_len(bucket) - 1
      if this.key_is_integer then
        v_append ret, @bucket[bucketidx].hash
      else
        v_append ret, @bucket[bucketidx].key
      end if
    next
  next
  return ret
end function

/' This seems unnecessary, so commenting out to reduce bloat. (Testcase below is commented out too)
function HashTable.hashes() as integer vector
  dim ret as integer vector
  dim idx as integer
  v_new ret, this.numitems, 0
  for bucketnum as integer = 0 to this.tablesize - 1
    dim byref bucket as HashBucketItem vector = this.table[bucketnum]
    for bucketidx as integer = 0 to v_len(bucket) - 1
      ret[idx] = bucket[bucketidx].hash
      idx += 1
    next
  next
  return ret
end function
'/

function HashTable.items() as HashBucketItem vector
  dim ret as HashBucketItem vector
  v_new ret, 0, this.numitems
  for idx as integer = 0 to this.tablesize - 1
    v_extend(ret, this.table[idx])
  next
  return ret
end function

dim shared HashTable_key_compare as FnCompare

local function HashBucketItem_compare cdecl (a as HashBucketItem ptr, b as HashBucketItem ptr) as long
  if HashTable_key_compare then return HashTable_key_compare(a->key, b->key)
  if a->hash < b->hash then return -1
  if a->hash > b->hash then return 1
end function

function HashTable.items_sorted() as HashBucketItem vector
  dim ret as HashBucketItem vector = this.items()
  HashTable_key_compare = this.key_compare
  qsort(@ret[0], this.numitems, sizeof(HashBucketItem), cast(FnCompare, @HashBucketItem_compare))
  return ret
end function

#IFDEF __FB_MAIN__
startTest(HashTableIntToInt)
  dim as HashTable tbl
  tbl.construct(64)
  'tbl.construct(64, type_table(integer), NO, type_table(any_ptr), NO)  'Equivalent to above

  tbl.add(65, 11)  ' |
  tbl.add(1, 10)   ' |
  tbl.add(129, 9)  ' v
  tbl.add(-255, 8)  'All in same bucket
  tbl.add(20, 7)
  tbl.add(20, -6)  'Duplicates
  if tbl.numitems <> 6 then fail

  if tbl.get(1) <> 10 then fail  'Interestingly FB allows comparing a ptr to an int without warning
  if tbl.get(65) <> 11 then fail
  if tbl.get(129) <> 9 then fail
  if tbl.get(-255) <> 8 then fail
  if tbl.get(20) <> 7 then fail
  dim tmp as integer = tbl.get_int(20)  'But here you do need get_int()
  if tmp <> 7 then fail
  if tbl.get_int(-255, -1) <> 8 then fail  'And also when providing a default value
  if tbl.get_int(0, -1) <> -1 then fail

  tbl.set(20, 0)
  if tbl.remove(-255) <> YES then fail
  if tbl.remove(84) <> NO then fail

  if tbl.get_int(20, -1) <> 0 then fail

  dim items as HashBucketItem vector = tbl.items_sorted()

  if v_len(items) <> 5 then fail
  if items[0].hash <> 1 then fail
  if items[1].hash <> 20 then fail
  if items[2].hash <> 20 then fail
  if items[3].hash <> 65 then fail
  if items[4].hash <> 129 then fail
  if items[0].value_int <> 10 then fail
  'The order of these two values (0, -6) may be random or depend on the CRT implementation
  if items[1].value_int + items[2].value_int <> -6 then fail
  if items[3].value_int <> 11 then fail
  if items[4].value_int <> 9 then fail

  v_free items

  /'
  dim hashes as uinteger vector = tbl.hashes()
  v_sort hashes
  if v_str(hashes) <> "[1, 20, 20, 65, 129]" then fail
  v_free hashes
  '/

  tbl.destruct()
  if tbl.numitems <> 0 then fail
endTest

startTest(HashTableStrings)
  dim tbl as HashTable
  tbl.construct( , type_table(string), YES, type_table(integer), NO)

  dim as string a = "A", b = "B", c = "C", d, e, f
  tbl.add @a, 100
  tbl.add @b, 200
  tbl.set @c, 300

  c = "foo"
  d = "C"
  'Note: you can NOT write 'tbl.get(@"C")', that's a zstring ptr not a string ptr
  if tbl.get(@c) <> 0 then fail
  if tbl.get(@d) <> 300 then fail

  tbl.destruct()
  if a <> "A" then fail  'Shouldn't have touched the originals

  'Try out reinitialising a table with different types.
  'This time, don't copy the strings (note, this is really dangerous, will
  'crash if the strings on the stack go out of scope, so don't do this!)
  tbl.construct(1 , type_table(string), NO, type_table(string), NO)

  c = "C"
  tbl.add @a, @d
  tbl.add @b, @e
  tbl.set @c, @f
  if tbl.numitems <> 3 then fail

  a = "not A"
  if tbl.get(@a) <> NULL then fail
  c = "B"
  tbl.set @c, @d  'Overwrites "B" key, since we are comparing by value, not pointer
  if tbl.numitems <> 3 then fail
  if tbl.get(@b) <> @d then fail
  if tbl.get(@c) <> @d then fail  'Since b and c are equal, the @c->@f item is shadowed
  c = "C"
  if tbl.get(@c) <> @f then fail  'But is still there

  if tbl.get_str(@e) <> "" then fail  '@e = "", which is not in the table
  if tbl.get_str(@e, "none") <> "none" then fail
  if tbl.get_str(@a) <> "" then fail  '@e = "", not in the table either
  if tbl.get_str(@a, "none") <> "none" then fail

  if tbl.get_str(@c) <> "" then fail  'In the table; value is ""
  if tbl.get_str(@c, "none") <> "" then fail
  if tbl.get_str(@b, "none") <> "C" then fail

  tbl.destruct()
endTest

startTest(HashTableZstring)
  dim tbl as HashTable
  tbl.construct(1 , type_table(zstring), YES, type_table(zstring), NO)

  dim as zstring ptr a = @"A", b = @"B", c = @"C"
  tbl.add a, a
  tbl.add b, b
  tbl.set c, c
  if tbl.get(b) <> b then fail
  if tbl.get_str(b) <> "B" then fail
  if tbl.get_int(@"D", 42) <> 42 then fail

  dim items as HashBucketItem vector = tbl.items()
  'Check the key is a deep copy, value is shallow
  if items[0].key = a then fail
  if *cast(zstring ptr, items[0].key) <> *a then fail
  if items[0].value <> a then fail
  v_free items

  if tbl.get(b) <> b then fail

  dim as string a2 = "A"
  if strptr(a2) = a then fail
  if tbl.get(strptr(a2)) <> a then fail

  tbl.destruct()
endTest

#ENDIF


'----------------- StrHashTable -----------------

sub StrHashTable.construct(tablesize as integer = 31, value_type as TypeTable = type_table(any_ptr), copy_and_delete_values as bool)
  base.construct(tablesize, type_table(string), YES, value_type, copy_and_delete_values)
end sub

sub StrHashTable.add(key as string, value as any ptr)
  'Could just cast to string ptr and pass to base.add, but calling stringhash
  'directly avoids a couple extra function calls
  base.add(stringhash(strptr(key), len(key)), value, @key)
end sub

sub StrHashTable.add(key as string, value as integer)
  base.add(stringhash(strptr(key), len(key)), canyptr(value), @key)
end sub

sub StrHashTable.add(key as string, value as string)
  base.add(stringhash(strptr(key), len(key)), canyptr(@value), @key)
end sub

sub StrHashTable.set(key as string, value as any ptr)
  base.set(stringhash(strptr(key), len(key)), value, @key)
end sub

sub StrHashTable.set(key as string, value as integer)
  base.set(stringhash(strptr(key), len(key)), canyptr(value), @key)
end sub

sub StrHashTable.set(key as string, value as string)
  base.set(stringhash(strptr(key), len(key)), canyptr(@value), @key)
end sub

function StrHashTable.get(key as string, default as any ptr = NULL) as any ptr
  return base.get(stringhash(strptr(key), len(key)), default, @key)
end function

function StrHashTable.get_int(key as string, default as integer = 0) as integer
  return cintptr32(base.get(stringhash(strptr(key), len(key)), canyptr(default), @key))
end function

function StrHashTable.get_str(key as string, default as zstring ptr = @"") as string
  'return base.get_str(stringhash(strptr(key), len(key)), default, @key)
  'Avoiding initialising a new string from default if not needed, but want to still allow NULL as a value
  dim ret as string ptr = base.get(stringhash(strptr(key), len(key)), canyptr(-1234), @key)
  if ret = canyptr(-1234) then return *default else return *ret
end function

function StrHashTable.remove(key as string) as bool
  return base.remove(stringhash(strptr(key), len(key)), @key)
end function

#ifdef __FB_MAIN__

startTest(StrStrHashTable)
  dim tbl as StrHashTable
  tbl.construct(32, type_table(string), YES)

  dim as string a = "A", q = "q"
  tbl.add a, "42"
  if tbl.get_str("A") <> "42" then fail
  if tbl.get_str("A", "default") <> "42" then fail
  if tbl.get_str("B") <> "" then fail
  a = "B"
  if tbl.get_str(a) <> "" then fail
  tbl.set "A", q
  if tbl.get_str("A") <> "q" then fail
  q = "not q"
  if tbl.get_str("A") <> "q" then fail
  if tbl.get_str("B", "default") <> "default" then fail
  tbl.set "Q", " "
  if tbl.numitems <> 2 then fail
  tbl.remove "A"
  if tbl.numitems <> 1 then fail
endTest

dim shared testdata_init_cnt as integer
dim shared testdata_failed as bool

type TestDataType
  name as string
  dat as integer
  declare constructor(d as integer)
  declare destructor()
end type

constructor TestDataType(d as integer)
  dat = d
  name = "data" & dat
  testdata_init_cnt += 1
end constructor

destructor TestDataType()
  if name <> "data" & dat then testdata_failed = YES
  name = "blank"
  testdata_init_cnt -= 1
end destructor

DECLARE_VECTOR_OF_TYPE(TestDataType, TestDataType)
DEFINE_VECTOR_OF_CLASS(TestDataType, TestDataType)

' Test mapping to a datatype ptr, which is owned (deleted but not copied in).
' Also tests dupicate keys a bit.
startTest(StrHashTable_NoCopydata)
  dim tbl as StrHashTable
  tbl.construct(32, type_table(TestDataType), YES)
  tbl.value_copy = NULL   'Delete but don't copy

  'Add items
  for idx as integer = 1 to 100
   tbl.add("key" & idx, new TestDataType(idx))
   'Duplicate data
   tbl.add("key" & idx, new TestDataType(1000 + idx))
   if testdata_init_cnt <> 2*idx then fail
  next

  'Check and delete items
  for idx as integer = 100 to 50 step -1
   'var vptr = cptr(TestDataType ptr, tbl.get("key" & idx))
   dim vptr as TestDataType ptr

   vptr = tbl.get("key" & idx)
   if vptr = null then fail
   if vptr->name <> "data" & idx then fail
   'Delete first copy of the key, then lookup the second
   if tbl.remove("key" & idx) = NO then fail

   vptr = tbl.get("key" & idx)
   if vptr = null then fail
   if vptr->name <> "data" & (1000 + idx) then fail

   if testdata_init_cnt <> 100 + idx - 1 then fail
  next

  tbl.clear()
  if testdata_init_cnt <> 0 then fail
  if testdata_failed then fail
endTest

#endif

'------------- Old allmodex stuff -------------

SUB array2str (arr() as integer, byval o as integer, dest as string)
'String dest is already filled out with spaces to the requisite size
'o is the offset in bytes from the start of the buffer
'the buffer will be packed 2 bytes to an int, for compatibility, even
'though FB ints are 4 bytes long.
'TODO: this sub is redundant to readbinstring
	dim i as integer
	dim bi as integer
	dim bp as integer ptr
	dim toggle as integer
	dim size as integer = len(dest)

	bp = @arr(0)
	bi = o \ 2 'offset is in bytes
	toggle = o mod 2

	if bi < lbound(arr) orelse o + len(dest) > (ubound(arr) + 1) * 2 then
		showbug "array2str: input array too small: " & ubound(arr)
		size = (ubound(arr) + 1) * 2 - o
	end if

	for i = 0 to size - 1
		if toggle = 0 then
			dest[i] = bp[bi] and &hff
			toggle = 1
		else
			dest[i] = (bp[bi] and &hff00) shr 8
			toggle = 0
			bi = bi + 1
		end if
	next

END SUB

SUB str2array (src as string, arr() as integer, byval o as integer)
'strangely enough, this does the opposite of the above
	dim i as integer
	dim bi as integer
	dim bp as integer ptr
	dim toggle as integer
	dim size as integer = len(src)

	bp = @arr(0)
	bi = o \ 2 'offset is in bytes
	toggle = o mod 2

	if bi < lbound(arr) orelse o + len(src) > (ubound(arr) + 1) * 2 then
		showbug "str2array: input array too small: " & ubound(arr)
		size = (ubound(arr) + 1) * 2 - o
	end if

	'debug "String is " + str(len(src)) + " chars"
	for i = 0 to size - 1
		if toggle = 0 then
			bp[bi] = src[i] and &hff
			toggle = 1
		else
			bp[bi] = (bp[bi] and &hff) or (src[i] shl 8)
			'check sign
			if (bp[bi] and &h8000) > 0 then
				bp[bi] = bp[bi] or &hffff0000 'make -ve
			end if
			toggle = 0
			bi = bi + 1
		end if
	next
end SUB

SUB xbload (filename as string, array() as integer, errmsg as string)
	IF isfile(filename) THEN
		dim ff as integer, byt as ubyte, seg as short, offset as short, length as short
		dim ilength as integer
		dim i as integer
		
		IF OPENFILE(filename, FOR_BINARY + ACCESS_READ, ff) THEN
			fatalerror errmsg
		END IF
		GET #ff,, byt 'Magic number, always 253
		IF byt <> 253 THEN
			CLOSE #ff
			fatalerror errmsg & " (bad header)"  'file may also be zero length
		END IF
		GET #ff,, seg 'Segment, no use anymore
		GET #ff,, offset 'Offset into the array, not used now
		GET #ff,, length 'Length
		'length is in bytes, so divide by 2, and subtract 1 because 0-based
		ilength = (length / 2) - 1

		dim buf(ilength) as short

		GET #ff,, buf()
		CLOSE #ff

		for i = 0 to small(ilength, ubound(array))
			array(i) = buf(i)
		next i

	ELSE
		fatalerror errmsg
	END IF
END SUB

SUB xbsave (filename as string, array() as integer, bsize as integer)
	dim ff as integer, byt as UByte, seg as uShort, offset as Short, length as Short
	dim ilength as integer
	dim i as integer
	dim needbyte as integer
	
	seg = &h9999
	offset = 0
	'Because we're working with a short array, but the data is in bytes
	'we need to check if there's an odd size, and therefore a spare byte
	'we'll need to add at the end.
	ilength = (bsize \ 2) - 1	'will lose an odd byte in the division
	needbyte = bsize mod 2		'write an extra byte at the end?
	length = bsize	'bsize is in bytes
	byt = 253

	'copy array to shorts
	DIM buf(ilength) as short
	for i = 0 to small(ilength, ubound(array))
		buf(i) = array(i)
	next

	OPENFILE(filename, FOR_BINARY + ACCESS_WRITE, ff)  'Truncate
	PUT #ff, , byt				'Magic number
	PUT #ff, , seg				'segment - obsolete
	PUT #ff, , offset			'offset - obsolete
	PUT #ff, , length			'size in bytes

	PUT #ff,, buf()
	if needbyte = 1 then
		i = small(ilength + 1, ubound(array)) 'don't overflow
		byt = array(i) and &hff
		put #ff, , byt
	end if
	CLOSE #ff
END SUB

'Set a bit in an integer if 'value', given a bitmask instead of a bit number
SUB setbitmask (byref bitsets as integer, bitmask as integer, value as bool)
	bitsets and= not bitmask
	if value then bitsets or= bitmask
END SUB

'Turn a bit in an array of 16bit words on or off.
'bitwords(): bit array, 16 bits per integer (rest ignored)
'wordnum:    index in bitwords() to start at (index where bits 0-15 are)
'bitnum:     bit number (counting from bitwords(wordnum))
'value:      whether to set the bit on
SUB setbit (bitwords() as integer, wordnum as integer, bitnum as integer, value as bool)
	dim wordoff as integer
	dim wordbit as integer

	wordoff = wordnum + (bitnum \ 16)
	wordbit = bitnum mod 16

	if wordoff < lbound(bitwords) orelse wordoff > ubound(bitwords) then
		showbug strprintf("setbit overflow: ub %d, wordnum %d, bitnum %d, v %d", _
                                  ubound(bitwords), wordnum, bitnum, value)
		exit sub
	end if

	setbitmask bitwords(wordoff), 1 shl wordbit, value
end SUB

'Returns 0 or 1. Use xreadbit if you want NO or YES instead.
'See setbit for full documentation
FUNCTION readbit (bitwords() as integer, wordnum as integer = 0, bitnum as integer) as integer
	dim wordoff as integer
	dim wordbit as integer

	wordoff = wordnum + (bitnum \ 16)
	wordbit = bitnum mod 16

	if wordoff < lbound(bitwords) orelse wordoff > ubound(bitwords) then
		showbug strprintf("readbit overflow: ub %d, wordnum %d, bitnum %d", _
                                  ubound(bitwords), wordnum, bitnum)
		return 0
	end if

	if bitwords(wordoff) and (1 shl wordbit) then
		readbit = 1
	else
		readbit = 0
	end if
end FUNCTION

'This is a wrapper for readbit that returns YES/NO
FUNCTION xreadbit (bitwords() as integer, bitnum as integer, wordoffset as integer = 0) as bool
	RETURN readbit(bitwords(), wordoffset, bitnum) <> 0
END FUNCTION

'Returns what a scancode is if Shift isn't pressed.
'longname: if false, a one-character string for the character that the key
'  inserts is returned if available, e.g. "[" instead of "Left Bracket", with
'  exception of "Space" and "Enter" and numpad keys like "Numpad Minus" which
'  always returned as long names.
'Perhaps doesn't belong here because scancodes are OHR-specific. However, OHR
'scancodes are 95% the same as FB scancodes
FUNCTION scancodename (k as KBScancode, longname as bool = NO) as string
 'static scancodenames(...) as string * 18 = { ... }
 'static joybuttonnames(...) as string * 20  = { ... }
 #INCLUDE "scancodenames.bi"

 DIM scname as string
 IF k >= LBOUND(scancodenames) ANDALSO k <= UBOUND(scancodenames) THEN
  scname = scancodenames(k)
 ELSEIF k >= scJoyButton1 ANDALSO k <= scJoyLAST THEN
  IF k >= LBOUND(joybuttonnames) ANDALSO k <= UBOUND(joybuttonnames) THEN
   scname = joybuttonnames(k)
  END IF
  IF LEN(scname) = 0 THEN
   scname = "Gamepad Button " & (k - scJoyButton1 + 1)
  END IF
 END IF
 IF LEN(scname) THEN
  'scname is either "" or formatted like "Long Name" or "X Long Name"
  IF LEN(scname) THEN
   IF LEN(scname) >= 3 ANDALSO scname[1] = ASC(" ") THEN
    IF longname THEN scname = MID(scname, 3) ELSE scname = LEFT(scname, 1)
   END IF
   RETURN scname
  END IF
 END IF
 RETURN "Scancode " & k
END FUNCTION

FUNCTION special_char_sanitize(s as string) as string
 'This is a datalossy function.
 'Remove special characters from an OHR string to make it 7-bit ASCII safe.
 'Also translates the old OHR and the new Latin-1 copyright chars to (C)
 DIM result as string = ""
 FOR i as integer = 0 TO LEN(s) - 1
  SELECT CASE s[i]
   CASE 32 TO 126:
    result &= CHR(s[i])
   CASE 134, 169:
    result &= "(C)"
  END SELECT
 NEXT i
 RETURN result
END FUNCTION

FUNCTION starts_with(s as string, prefix as string) as bool
 'Return YES if the string begins with a specific prefix
 RETURN LEFT(s, LEN(prefix)) = prefix
END FUNCTION

FUNCTION ends_with(s as string, suffix as string) as bool
 'Return YES if the string ends with a specific suffix
 RETURN RIGHT(s, LEN(suffix)) = suffix
END FUNCTION

FUNCTION count_directory_size(directory as string, byref file_count as integer = 0, limit as integer = 999999) as integer
 '--Count the bytes in all the files in a directory and all subdirectories.
 '--This doesn't consider the space taken by the directories themselves,
 '--nor does it consider blocksize or any other filesystem details.
 'file_count: returns total number of files/directories
 'limit: number of files to consider before stopping early. <= 0 for no limit.
 DIM bytes as integer = 0
 DIM filelist() as string

 '--First count files
 findfiles directory, ALLFILES, fileTypeFile, YES, filelist()
 FOR i as integer = 0 TO UBOUND(filelist)
  bytes += filelen(directory & SLASH & filelist(i))
  file_count += 1
  IF file_count >= limit THEN RETURN bytes
 NEXT

 '--Then count subdirectories
 findfiles directory, ALLFILES, fileTypeDirectory, YES, filelist()
 FOR i as integer = 0 TO UBOUND(filelist)
  bytes += count_directory_size(directory & SLASH & filelist(i), file_count, limit)
  file_count += 1
  IF file_count >= limit THEN RETURN bytes
 NEXT

 RETURN bytes
END FUNCTION

'Return contents of a binary file as a string
'For text files use string_from_file instead.
FUNCTION read_file (filename as string, expect_exists as bool = YES, byref success as bool = NO) as string
 success = NO
 DIM fh as integer
 IF OPENFILE(filename, FOR_BINARY + ACCESS_READ + IIF(expect_exists, OR_ERROR, 0), fh) THEN RETURN ""
 DIM buflen as integer = LOF(fh)  'race condition!
 DIM buf as string = STRING(buflen, 0)
 IF buflen > 0 THEN  'Otherwise FB throws an illegal function call error
  GET #fh, , buf
 END IF
 CLOSE #fh
 success = YES
 RETURN buf
END FUNCTION

'Write binary data (as a string) to a file
'For text files use string_to_file instead.
SUB write_file (filename as string, outdata as string)
 DIM fh as integer
 IF OPENFILE(filename, FOR_BINARY + ACCESS_WRITE, fh) = fberrOK then  'truncate to 0 length
  PUT #fh, , outdata
  CLOSE #fh
 END IF
END SUB

FUNCTION string_from_first_line_of_file (filename as string) as string
 'Read the first line of a text file and return it as a string.
 'ignore/removes any line-ending chars
 DIM fh as integer
 DIM result as string
 OPENFILE(filename, for_input, fh)
 LINE INPUT #fh, result
 CLOSE #fh
 RETURN result
END FUNCTION

FUNCTION string_from_file (filename as string, expect_exists as bool = YES, byref success as bool = NO) as string
 'Read an entire text file as a string and
 'convert the line endings to LF only
 RETURN normalize_newlines(read_file(filename, expect_exists, success), !"\n")
END FUNCTION

SUB string_to_file (string_to_write as string, filename as string)
 'Write a string to a text file using native line endings
 write_file filename, normalize_newlines(string_to_write, LINE_END)
END SUB

'Read each line of a file into a string array. Return true on success
'See also string_from_first_line_of_file() if you only want the first.
FUNCTION lines_from_file(strarray() as string, filename as string, expect_exists as bool = YES) as bool
 REDIM strarray(-1 TO -1)
 DIM as integer fh, openerr
 openerr = OPENFILE(filename, FOR_INPUT, fh)
 IF openerr = fberrNOTFOUND THEN
  IF expect_exists THEN showerror "Missing file: " & filename
  RETURN NO
 ELSEIF openerr <> fberrOK THEN
  showerror "lines_from_file: Couldn't open " & filename
  RETURN NO
 END IF
 DO UNTIL EOF(fh)
  DIM text as string
  LINE INPUT #fh, text
  a_append strarray(), text
 LOOP
 CLOSE #fh
 RETURN YES
END FUNCTION

'Write an array of strings to a file, one-per-line.
'The specified line endings will automatically be added (you can pass "" if not needed)
FUNCTION lines_to_file(strarray() as string, filename as string, lineending as string = !"\n") as bool
 DIM fh as integer
 IF OPENFILE(filename, FOR_BINARY + ACCESS_WRITE, fh) THEN  'truncate to 0 length
  showerror "lines_to_file: Couldn't open " & filename
  RETURN NO
 END IF
 FOR i as integer = 0 TO UBOUND(strarray)
  PUT #fh, , strarray(i) & lineending
 NEXT i
 CLOSE #fh
 RETURN YES
END FUNCTION

'Note: Custom doesn't use this function
FUNCTION get_tmpdir () as string
 DIM tmp as string
 #IFDEF __FB_WIN32__
  'Windows only behavior
  tmp = environ("TEMP")
  IF NOT diriswriteable(tmp) THEN tmp = environ("TMP")
  IF NOT diriswriteable(tmp) THEN tmp = EXEPATH
  IF NOT diriswriteable(tmp) THEN tmp = CURDIR
  IF NOT diriswriteable(tmp) THEN fatalerror "Unable to find any writable temp dir"
 #ELSEIF DEFINED(__FB_ANDROID__)
  'SDL sets initial directory to .../com.hamsterrepublic.ohrrpgce.game/files
  tmp = orig_dir
 #ELSEIF DEFINED(__FB_DARWIN__)
  'This matches fallback behaviour of get_settings_dir. See comments there.
  '(TODO: We only care about using .ohrrpgce if is already exists so that we can delete
  'crashed playing.tmp dirs)
  'We use Caches instead of Application Support; looks like it only matters on iOS
  '(where the OS is free to delete Caches).
  tmp = ENVIRON("HOME") & "/.ohrrpgce"
  IF isdir(tmp) = NO THEN
   tmp = ENVIRON("HOME") & "/Library/Caches/OHRRPGCE"
  END IF
 #ELSEIF DEFINED(__FB_UNIX__)
  tmp = environ("HOME") + SLASH + ".ohrrpgce"
 #ELSE
  #ERROR "Unknown OS"
 #ENDIF
 IF NOT isdir(tmp) THEN
  IF makedir(tmp) <> 0 THEN fatalerror "Temp directory " & tmp & " missing and unable to create it"
 END IF
 tmp = add_trailing_slash(tmp)
 DIM as string d = DATE, t = TIME
 tmp += "ohrrpgce" & MID(d,7,4) & MID(d,1,2) & MID(d,4,2) & MID(t,1,2) & MID(t,4,2) & MID(t,7,2) & "." & randint(1000) & ".tmp" & SLASH
 IF NOT isdir(tmp) THEN
  IF makedir(tmp) <> 0 THEN fatalerror "Unable to create temp directory " & tmp
 END IF
 RETURN tmp
END FUNCTION


'----------------------------------------------------------------------
'                       Commandline processing


'Returns true if opt is a flag (prefixed with -,--,/) and removes the prefix
function commandline_flag(opt as string) as bool
	dim temp as string
	temp = left(opt, 1)
	'/ should not be a flag under unix
#ifdef __FB_UNIX__
	if temp = "-" then
#else
	if temp = "-" or temp = "/" then
#endif
		temp = mid(opt, 2, 1)
		if temp = "-" then  '--
			opt = mid(opt, 3)
		else
			opt = mid(opt, 2)
		end if
		return YES
	end if
	return NO
end function

'Read commandline arguments from actual commandline and from args_file
local sub get_commandline_args(cmdargs() as string, args_file as string = "")
	if len(args_file) andalso isfile(args_file) then
		debuginfo "Reading additional commandline arguments from " & args_file 
		lines_from_file cmdargs(), args_file
	end if

	dim i as integer = 1
	while command(i) <> ""
		a_append(cmdargs(), command(i))
		i += 1
	wend
end sub

' Processes all commandline switches by calling opt_handler function,
' and put any arguments that aren't recognised in nonoption_args() (must be a dynamic array).
' args_file optionally provides additional arguments.
sub processcommandline(nonoption_args() as string, opt_handler as FnSetOption, args_file as string = "")
	dim cnt as integer = 0
	dim opt as string
	dim arg as string
	redim cmdargs(-1 to -1) as string
	redim nonoption_args(-1 to -1) as string

	get_commandline_args cmdargs(), args_file

	while cnt <= ubound(cmdargs)
		dim argsused as integer = 0

		opt = cmdargs(cnt)
		if commandline_flag(opt) then
			#ifdef __FB_DARWIN__
				'Passed when started from Finder. Ignore. (Fix for bug #1171)
                                'This is currently only necessary when using gfx_sdl2 because it doesn't link
                                'mac/SDLMain.m; if using gfx_sdl then this argument is stripped in SDLMain.m.
				if starts_with(opt, "psn_") then
					cnt += 1
					continue while
				end if
			#endif

			if cnt + 1 <= ubound(cmdargs) then
				arg = cmdargs(cnt + 1)
				if commandline_flag(arg) then arg = ""
			else
				arg = ""
			end if

			argsused = opt_handler(opt, arg)

			'debuginfo "commandline option = '" & opt & "' arg = '" & arg & "' used = " & argsused
		end if

		if argsused = 0 then
			'Everything else falls through and is stored for the program to catch
			'(we could prehaps move their handling into functions as well)
			a_append(nonoption_args(), cmdargs(cnt))
			argsused = 1
			'debuginfo "commandline arg " & (ubound(nonoption_args) - 1) & ": stored " & cmdargs(cnt)
		end if
		cnt += argsused
	wend
end sub

'----------------------------------------------------------------------
'                        ini file read/write


SUB write_ini_value (ini_filename as string, key as string, value as string)
 REDIM ini(-1 TO -1) as string
 IF isfile(ini_filename) THEN
  lines_from_file ini(), ini_filename
 END IF
 write_ini_value ini(), key, value
 lines_to_file ini(), ini_filename, LINE_END
END SUB

SUB write_ini_value (ini_filename as string, key as string, value as integer)
 write_ini_value ini_filename, key, STR(value)
END SUB

SUB write_ini_value (ini_filename as string, key as string, value as double)
 write_ini_value ini_filename, key, STR(value)
END SUB

SUB write_ini_value (ini() as string, key as string, value as string)
 'Key is case insensitive but case preservative
 'If the key is matched more than once, all copies of it will be changed
 BUG_IF(LEN(key) = 0, "Can't write empty key to ini file")
 DIM found as bool = NO
 FOR i as integer = 0 TO UBOUND(ini)
  IF ini_key_match(ini(i), key) THEN
   ini(i) = key & " = " & value
   found = YES
  END IF
 NEXT i
 IF NOT found THEN
  a_append ini(), key & " = " & value
 END IF
END SUB

FUNCTION read_ini_str (ini_filename as string, key as string, default as string="", byref linenum as integer = 0) as string
 REDIM ini(-1 TO -1) as string
 IF isfile(ini_filename) THEN
  lines_from_file ini(), ini_filename
 END IF
 RETURN read_ini_str(ini(), key, default, linenum)
END FUNCTION

'Given the content of an .ini as an array of lines, return the value of the
'first line of form "key = value".
'Optionally returns the line number (0-indexed) on which the match was found in linenum, or else -1.
FUNCTION read_ini_str (ini() as string, key as string, default as string="", byref linenum as integer = 0) as string
 linenum = -1
 BUG_IF(LEN(key) = 0, "Can't read empty key from ini file", default)
 DIM value as string
 FOR i as integer = 0 TO UBOUND(ini)
  IF ini_key_match(ini(i), key, value) THEN
   linenum = i
   RETURN value
  END IF
 NEXT i
 RETURN default
END FUNCTION

FUNCTION read_ini_int (ini_filename as string, key as string, default as integer=0) as integer
 RETURN str2int(read_ini_str(ini_filename, key), default)
END FUNCTION

'This is not strict. A non-numerical value will return 0., not default!
FUNCTION read_ini_double (ini_filename as string, key as string, default as double=0.) as double
 DIM value as string = read_ini_str(ini_filename, key)
 IF LEN(value) THEN RETURN VAL(value)
 RETURN default
END FUNCTION

'A case insensitive match for regex "^key *= *value *". Returns true and sets "value" on a match
FUNCTION ini_key_match(text as string, key as string, byref value as string = "") as bool
 DIM eqpos as integer
 IF LCASE(LEFT(text, LEN(key))) <> LCASE(key) THEN RETURN NO
 eqpos = LEN(key) + 1
 WHILE MID(text, eqpos, 1) = " "
  eqpos += 1
 WEND
 IF MID(text, eqpos, 1) <> "=" THEN RETURN NO
 value = TRIM(MID(text, eqpos + 1))
 RETURN YES
END FUNCTION

'----------------------------------------------------------------------

' For commandline utilities. Wait for a keypress and return it.
FUNCTION readkey () as string
  DO
    DIM w as string = INKEY
    IF w <> "" THEN RETURN w
  LOOP
END FUNCTION

'----------------------------------------------------------------------

destructor SmoothedTimer()
  v_free times
end destructor

Sub SmoothedTimer.start()
  timing = timer
end sub

'Returns true if smoothed value was updated
function SmoothedTimer.stop() as bool
  timing = timer - timing
  if times = NULL then v_new times
  v_append times, timing
  if v_len(times) = 9 then
    'Update with median buffer value
    v_sort times
    if smoothtime > times[4] * 4/3 orelse smoothtime < times[4] * 3/4 then
      smoothtime = times[4]   'Reset
    else
      smoothtime = smoothtime * 0.6 + 0.4 * times[4]
    end if
    v_new times  'Clear buffer
    return YES
  end if
end function

function SmoothedTimer.tell() as string
  return "done in " & cint(1e6 * timing) & !"us;\tsmoothed: " & cint(1e6 * smoothtime) & "us"
end function

sub SmoothedTimer.stop_and_print()
  if stop() then ? tell()
end sub

'----------------------------------------------------------------------

'Begin a new time step and start()
sub ExpSmoothedTimer.begin_timestep()
  cur_time = -timer
end sub

'Stop if not already, and finish a time step by updating smooth_time with decay
function ExpSmoothedTimer.finish_timestep(halflife as double, update_display as bool = YES) as double
  'if cur_time < 0 then cur_time += timer  '.stop(). Breaks timers using negative add_time
  dim decay as double = exp2(-1 / halflife)
  smooth_time = decay * smooth_time + (1 - decay) * cur_time
  if update_display then display_time = smooth_time
  return smooth_time
end function

'Resume adding time to this timer
sub ExpSmoothedTimer.start()
  cur_time -= timer
end sub

'Stop adding time
sub ExpSmoothedTimer.stop()
  cur_time += timer
end sub

operator ExpSmoothedTimer.+=(rhs as ExpSmoothedTimer)
  this.cur_time += rhs.cur_time
  this.smooth_time += rhs.smooth_time
  this.display_time += rhs.display_time
end operator


'----------------------------------------------------------------------

'Reset state and start the Default timer
sub MultiTimer.begin_timestep()
  for idx as integer = lbound(timers) to ubound(timers)
    timers(idx).cur_time = 0.
  next
  timers(TimerIDs.Total).start()
  'num_timer_calls = 1
  subtimer = TimerIDs.Default
  enabled = YES
end sub

'Stop all timers and apply smoothing
sub MultiTimer.finish_timestep(halflife as double, update_display as bool = YES)
  if enabled = NO then exit sub
  if subtimer > TimerIDs.Default then timers(subtimer).stop()  'Shouldn't happen
  timers(TimerIDs.Total).stop()
  'num_timer_calls += 1

  'Calculate Total & Default.
  'For efficiency we don't stop/start Default timer all the time.
  timers(TimerIDs.Total).cur_time -= timers(TimerIDs.Pause).cur_time
  timers(TimerIDs.Default).cur_time = timers(TimerIDs.Total).cur_time
  for idx as integer = TimerIDs.FIRST to ubound(timers)
    timers(TimerIDs.Default).cur_time -= timers(idx).cur_time
  next

  for idx as integer = lbound(timers) to ubound(timers)
    timers(idx).finish_timestep(halflife, update_display)
  next
  enabled = NO
  subtimer = TimerIDs.None
end sub

'If enabled=NO or a subtimer (not Default) is already running, does nothing and
'returns 0 (Default).
'Can use substart(TimerIDs.Pause) to pause.
'The return value can optionally be used to more efficiently skip the matching substop().
function MultiTimer.substart(new_subtimer as TimerIDs) as TimerIDs
  if subtimer <> TimerIDs.Default then return TimerIDs.Default
  subtimer = new_subtimer
  timers(subtimer).cur_time -= timer  '.start()
  'num_timer_calls += 1
  return subtimer
end function

'Pass in the ID of the subtimer to end, so can ignore nested substart/substop pairs; does nothing
'when passed 0 (Default) or subtimer other than the active one.
sub MultiTimer.substop(cur_subtimer as TimerIDs)
  if cur_subtimer <> subtimer then exit sub
  if subtimer = TimerIDs.Default then exit sub
  timers(subtimer).cur_time += timer  '.stop()
  'num_timer_calls += 1
  subtimer = TimerIDs.Default
end sub

'Override the current subtimer, including switching to Default or Pause.
'Returns previous subtimer ID, which can be passed back to switch to resume it:
'you must do so to use this for nesting subtimers.
'Returns TimerIDs.None if not running.
function MultiTimer.switch(new_subtimer as TimerIDs) as TimerIDs
  if subtimer = TimerIDs.None or new_subtimer = TimerIDs.None then return TimerIDs.None
  dim time as double = timer
  'num_timer_calls += 1
  if subtimer <> TimerIDs.Default then timers(subtimer).cur_time += time  '.stop()
  function = subtimer
  subtimer = new_subtimer
  if subtimer <> TimerIDs.Default then timers(subtimer).cur_time -= time  '.start()
end function

sub MultiTimer.add_time(to_subtimer as TimerIDs, amount as double)
  timers(TimerIDs.Total).cur_time += amount
  timers(to_subtimer).cur_time += amount
end sub


'----------------------------------------------------------------------
'                       other misc functions

' Argument is a timeserial
FUNCTION format_date(timeser as double) as string
 IF timeser = 0 THEN RETURN "0"
 RETURN FORMAT(timeser, "yyyy mmm dd hh:mm:ss")
END FUNCTION
