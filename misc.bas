'OHRRPGCE Common - Odd header/module left over from the QuickBasic to FreeBASIC move
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'FIXME: move this crud elsewhere

#include "config.bi"
#include "util.bi"
#include "ver.txt"
#include "misc.bi"
#include "allmodex.bi"
#include "fontdata.bi"
#include "gfx.bi"
#include "common.bi"
#include "music.bi"
#ifdef IS_GAME
#include "yetmore2.bi"
#endif
#ifdef IS_CUSTOM
#include "customsubs.bi"
#endif

'An option was given on commandline to set zoom or fullscreen/windowed (overrides the games' settings)
dim overrode_default_zoom as bool = NO
dim overrode_default_fullscreen as bool = NO
dim overrode_log_dir as bool = NO

#ifdef timer_variables
dim timer_variables
#endif

SUB getdefaultfont(fnt() as integer)
	for i as integer = 0 to 1023
		fnt(i) = default_font(i)
	next
END SUB

SUB getbrowserfont(fnt() as integer)
	for i as integer = 0 to 1023
		fnt(i) = browser_font(i)
	next
END SUB

function global_setoption(opt as string, arg as string) as integer
	dim help as string = ""
	if opt = "v" or opt = "version" then
		help = help & long_version & build_info & LINE_END
		help = help & "(C) Copyright 1997-2017 James Paige and Hamster Republic Productions" & LINE_END
		help = help & "This game engine is free software under the terms of the GNU GPL v2+" & LINE_END
		help = help & "For source-code see http://HamsterRepublic.com/ohrrpgce/source.php" & LINE_END
		help = help & "Game data copyright and license will vary." & LINE_END
		display_help_string help
		return 1
	elseif opt = "?" or opt = "help" or opt = "h" then
		load_preferred_gfx_backend()

		help = help & "Usage: " & trimpath(command(0)) & " [options] [.rpg or .rpgdir or initial browser directory]" & LINE_END
#IFDEF IS_CUSTOM
		help = help & "   Or: " & trimpath(command(0)) & " [options] <.rpg or .rpgdir> <.hss file>" & LINE_END
		help = help & "          (Import the scripts and then quit. See also --nowait)" & LINE_END
#ENDIF
		help &= LINE_END
		help = help & "If a file named ohrrpgce_arguments.txt exists in the current directory then" & LINE_END
		help = help & "additional command line arguments will be read from it, one per line." & LINE_END
		help &= LINE_END
		help = help & "-? -h -help         Display this help screen" & LINE_END
		help = help & "-v -version         Show version and build info" & LINE_END
		help = help & "-log foldername     Log debug messages to a specific folder" & LINE_END
#IFDEF IS_GAME
		help = help & "-full-upgrade       Upgrade game data completely, as Custom does (only useful for bughunting)" & LINE_END
		help = help & "-autosnap N         Automatically save a screen snapshot every N ticks" & LINE_END
		help = help & "-autotest           Run quickly and write screenshots on _checkpoints" & LINE_END
		help = help & "-errlvl [level]     Override script error suppression level (" & serrBound & " default, " & serrSuspicious & " hide warnings, " & serrBadOp & " hide all but corruption/bugs)" & LINE_END
		help = help & "-scriptlog          Script logging initially turned on (Ctrl+F10 to toggle)" LINE_END
                'Hidden options:
                'help = help & "-slave channel     IPC channel to use to receive messages from Custom" & LINE_END
                'help = help & "-debugkeys         Turn on debug keys" & LINE_END
#ENDIF
#IFDEF IS_CUSTOM
		help = help & "-distrib [zip|win|mac|tarball|debian|all] When opening a game, export a copy for" & LINE_END
		help = help & "                    distribution in the requested format. Not all formats are " & LINE_END
		help = help & "                    available on all platforms. See c_debug.txt for error messages" & LINE_END
		help = help & "-nowait             When importing scripts (ignored otherwise) quit immediately on success" & LINE_END
		help = help & "-export-trans file  Export translations to a file" & LINE_END
#ENDIF
		help = help & "-rawexx             Don't catch -exx fatal errors, let gdbgame/gdbcustom.sh/bat catch them" & LINE_END
		help = help & "-recordinput file   Record keyboard input to a file" & LINE_END
		help = help & "-replayinput file   Replay keyboard input from a previously recorded file" & LINE_END
		help = help & "-runfast            Run as quickly as possible (no FPS throttling)" & LINE_END
		help = help & "-logslow            Log to c/g_debug.txt when certain backend calls took a long time" & LINE_END
		help = help & "-maxfps fps         The maximum frames-per-second before starting to skip frames (default " & max_display_fps & ")" & LINE_END
		help = help & "-giffps fps         Set the maximum frames-per-second for a recorded .gif (default 30)" & LINE_END
		help = help & "-recordoverlays     Include overlays like FPS counter and macro playback info in screenshots/.gifs" & LINE_END
		help = help & "-hideoverlays       Never draw overlays" & LINE_END
		help = help & "-showkeys           Show keyboard keys as they're pressed while recording a .gif" & LINE_END
		help = help & "-showmouse          Show mouse position while recording a .gif" & LINE_END
		help = help & "-gfx backendname    Select graphics backend. This build supports:" & LINE_END
		help = help & "                      " & SUPPORTED_GFX & " (tried in that order)" & LINE_END
		help = help & "-no-native-kbd      Use US keyboard layout instead of OS-based text input (Default under X11 (ie. Linux, BSD))" & LINE_END
		help = help & "-native-kbd         Use OS-based text input instead of US keyboard layout (Default on Windows, Mac OS X, Android)" & LINE_END
		help = help & "-f -fullscreen      Start in full-screen mode if possible" & LINE_END
		help = help & "-w -windowed        Start in windowed mode (default)" & LINE_END
		help = help & " Backend-specific options for gfx_" & gfxbackend & ": (use -gfx XYX -help to see others)" & LINE_END
		if gfx_describe_options() then
			help = help & *gfx_describe_options()
		end if
		display_help_string help
		return 1
	elseif opt = "log" then
		dim d as string = absolute_with_orig_path(arg, YES)
		if isdir(d) ANDALSO diriswriteable(d) then
                        if paths_equal(d, log_dir) = NO then
                                end_debug
                                log_dir = d
                                start_new_debug "Starting log (-log option)"
                        end if
			return 2
		else
			help = "log dir """ & d & """ is not valid." & LINE_END
			help = help & "a valid argument to -log must be a writable folder that exists."
			display_help_string help
			return 1
		end if
	elseif opt = "rawexx" then
		' The purpose of this flag is that ON ERROR GOTO causes the topmost stack frame
		' (where the error actually occurred) to be lost.
		remove_exx_handler  'setup_exx_handler already called
		' Could also uninstall the Windows exception handler, if that's useful?
		return 1
	end if
	return 0
end function

' Custom and Game share this function for processing commandline flags:
' poll various modules that accept options.
function gamecustom_setoption(opt as string, arg as string) as integer
	dim argsused as integer = 0

	' Check for commandline options that we want to do something about but without consuming.
	'dim value as integer = str2int(arg, -1)
	if opt = "zoom" or opt = "z" or opt = "width" or opt = "w" then
		overrode_default_zoom = YES
	end if
	if opt = "fullscreen" or opt = "f" or opt = "windowed" or opt = "w" then
		overrode_default_fullscreen = YES
	end if
	if opt = "log" then
		overrode_log_dir = YES
	end if

	' Delegate
	if argsused = 0 then argsused = allmodex_setoption(opt, arg)
	if argsused = 0 then argsused = global_setoption(opt, arg)
	if argsused = 0 then argsused = common_setoption(opt, arg)  'common.rbas
	#ifdef IS_GAME
		if argsused = 0 then argsused = game_setoption(opt, arg)
	#endif
	#ifdef IS_CUSTOM
		if argsused = 0 then argsused = custom_setoption(opt, arg)
	#endif
	if argsused = 0 then argsused = backends_setoption(opt, arg)  'this always loads a backend
	if argsused = 0 andalso gfx_setoption then
		argsused = gfx_setoption(cstring(opt), cstring(arg))
	end if

	return argsused
end function

sub display_help_string(help as string)
	dim k as string
	print help    ' display to text console (doesn't work under Windows unless compiled without -s gui)
#ifdef __FB_WIN32__
	'Don't do this under Unix, it's annoying and adds fbgfx as a dependency
	if len(help) > 500 then
		screen 19   ' create a graphical fake text console (800x600, 100x37 characters)
	else
		screen 11
	end if
	print help,   ' display the help on the graphical console
	k = input(1)  ' use FreeBasic-style keypress checking because our keyhandler isn't set up yet
#endif
	SYSTEM        ' terminate the program
end sub


' This function exists to be hooked by gdb after we've opened
' the channel (without which, Custom's attempt to open it times out).
' It needs to be in a module other than yetmore2.bas so that it doesn't get inlined.
SUB hook_after_attach_to_master(success as bool)
END SUB

FUNCTION ReadShort(byval fh as integer, byval p as long=-1) as short
	DIM ret as short
	IF p = -1 THEN
		GET #fh,,ret
	ELSEIF p >= 0 THEN
		GET #fh,p,ret
	END IF
	return ret
END FUNCTION

FUNCTION ReadShort(filename as string, byval p as integer) as short
	DIM ret as short
	DIM fh as integer
	OPENFILE(filename, for_binary + access_read, fh)
	GET #fh, p, ret
	CLOSE #fh
	return ret
END FUNCTION

FUNCTION ReadByte(byval fh as integer, byval p as long=-1) as ubyte
	DIM ret as ubyte
	IF p = -1 THEN
		GET #fh,,ret
	ELSEIF p >= 0 THEN
		GET #fh,p,ret
	END IF
	return ret
END FUNCTION

Sub WriteShort(byval fh as integer, byval p as long, byval v as integer)
	WriteShort(fh,p,cshort(v))
END SUB

Sub WriteShort(byval fh as integer, byval p as long, byval v as short)
	IF p = -1 THEN
		PUT #fh,,v
	ELSEIF p >= 0 THEN
		PUT #fh,p,v
	END IF
END SUB

Sub WriteShort(filename as string, byval p as integer, byval v as integer)
	DIM fh as integer
	OPENFILE(filename, FOR_BINARY, fh)
	PUT #fh, p, cshort(v)
	CLOSE #fh
END SUB

Sub WriteByte(byval fh as integer, byval v as ubyte, byval p as long=-1)
	IF p = -1 THEN
		PUT #fh,,v
	ELSEIF p >= 0 THEN
		PUT #fh,p,v
	END IF
END SUB

Function ReadVStr(byval fh as integer, byval maxlen as integer) as string
	dim length as short, ret as string, c as short, i as integer
	length = readshort(fh)
	
	for i = 0 to maxlen - 1
		c = readshort(fh)
		if i < length then ret &= chr(c AND 255)
	next
	
	return ret
end function

Sub WriteVStr(byval fh as integer, byval maxlen as integer, s as string)
	dim i as integer
	writeshort(fh, -1, small(maxlen, len(s)))
	
	for i = 0 to maxlen - 1
		if i < len(s) then writeshort(fh, -1, cint(s[i])) else writeshort(fh, -1, 0)
	next
end sub

Function ReadByteStr(byval fh as integer, byval maxlen as integer) as string
	dim length as short, ret as string, c as ubyte, i as integer
	length = readshort(fh)
	
	for i = 0 to maxlen - 1
		c = readbyte(fh)
		if i < length then ret = ret & chr(c)
	next
	
	return ret
end function

Sub WriteByteStr(byval fh as integer, byval maxlen as integer, s as string)
	dim i as integer
	writeshort(fh, -1, small(maxlen, len(s)))
	
	for i = 0 to maxlen - 1
		if i < len(s) then writebyte(fh, cubyte(s[i])) else writebyte(fh, 0)
	next
end sub
