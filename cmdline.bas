'OHRRPGCE - Common Game/Custom commandline arg processing
'(C) Copyright 1997-2022 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

'Many commandline args are handled by *_setoption functions in individual modules because they access internal data

#include "config.bi"
#include "util.bi"
#include "allmodex.bi"
#include "gfx.bi"
#include "common.bi"
#include "cmdline.bi"
#include "backends.bi"
#ifdef IS_GAME
#include "yetmore2.bi"
#endif
#ifdef IS_CUSTOM
#include "customsubs.bi"
#endif


'An option was given on commandline to set zoom or fullscreen/windowed (overrides the game's settings
'and remembered window zoom)
dim overrode_default_zoom as bool = NO
dim overrode_default_fullscreen as bool = NO
dim overrode_log_dir as bool = NO


function global_setoption(opt as string, arg as string) as integer
	dim help as string = ""
	if opt = "v" or opt = "version" then
		help = help & long_version & build_info & LINE_END
		help = help & "(C) Copyright 1997-2022 James Paige, Ralph Versteegen, and the OHRRPGCE Developers" & LINE_END
		help = help & "Dual licensed under the GNU GPL v2+ and MIT Licenses." & LINE_END
		help = help & "Read LICENSE.txt for terms and disclaimer of liability." & LINE_END
		help = help & "For source-code see http://HamsterRepublic.com/ohrrpgce/source.php" & LINE_END
		help = help & "Game data copyright and license will vary." & LINE_END
		display_help_string help
		terminate_program
		return 1
	elseif opt = "?" or opt = "help" or opt = "h" then
		load_preferred_gfx_backend()

		help = help & "Usage: " & trimpath(command(0)) & " [options] [.rpg or .rpgdir or initial browser directory]" & LINE_END
#IFDEF IS_CUSTOM
		help = help & "   Or: " & trimpath(command(0)) & " [options] <.rpg or .rpgdir> <.hss file>" & LINE_END
		help = help & "          (Import the scripts and then quit. See also --nowait, --hsflags)" & LINE_END
#ENDIF
		help &= LINE_END
		help = help & "If a file named ohrrpgce_arguments.txt exists in the current directory then" & LINE_END
		help = help & "additional command line arguments will be read from it, one per line." & LINE_END
		help &= LINE_END
		help = help & "-? -h -help         Display this help screen" & LINE_END
		help = help & "-v -version         Show version and build info" & LINE_END
		help = help & "-log foldername     Log debug messages to a specific folder" & LINE_END
                'help = help & "-appdir foldername  Installation directory (Must be first option!)" & LINE_END
		help = help & "-list-embeds        Print the list of embedded data files" & LINE_END
		help = help & "-dump-embed file    Extract an embedded file, written to the current directory" & LINE_END
#IFDEF IS_GAME
		help = help & "-full-upgrade       Upgrade game data completely, as Custom does (only useful for bughunting)" & LINE_END
		help = help & "-autosnap N         Automatically save a screen snapshot every N ticks" & LINE_END
		help = help & "-autotest           Run quickly and write screenshots on _checkpoints" & LINE_END
		help = help & "-errlvl [level]     Override script error suppression level (" & serrBound & " default, " & serrSuspicious & " hide warnings, " & serrBadOp & " hide all but corruption/bugs)" & LINE_END
		help = help & "-scriptlog          Script logging initially turned on (Ctrl+F10 to toggle)" LINE_END
		'help = help & "-timecmd cmdid      ID (from plotscr.hsd) of a command to time in detail when command profiling" LINE_END
#IFNDEF __FB_WIN32__   'Can't print to console from a GUI application on Windows
		help = help & "-print              Print 'trace' and 'trace value' commands also to stdout" LINE_END
		help = help & "-printonly          Print 'trace' and 'trace value' commands only to stdout" LINE_END
#ENDIF
		help = help & "-debug-achieve      Start with achievement debug logging enabled" LINE_END
		help = help & "-reset_platform_achievements  Wipe rewarded Steam achievements" LINE_END
                'Hidden options:
                'help = help & "-from_Custom channel  IPC channel to use to receive messages from Custom" & LINE_END
                'help = help & "-debugkeys         Turn on debug keys" & LINE_END
#ENDIF
#IFDEF IS_CUSTOM
		help = help & "-distrib [zip|win|mac[32|64]|tarball[32|64]|debian[32|64]|all]" & LINE_END
		help = help & "                    When opening a game, export a copy for distribution in the requested" & LINE_END
		help = help & "                    format. mac/tarball/debian are aliases for mac64/tarball64/debian64." & LINE_END
		help = help & "                    Not all formats are available on all platforms." & LINE_END
		help = help & "                    See c_debug.txt for error messages" & LINE_END
		help = help & "-nowait             When importing scripts (ignored otherwise) quit immediately on success" & LINE_END
		help = help & "-hsflags            When importing scripts (ignored otherwise), extra arguments to pass to hspeak" & LINE_END
		help = help & "                    which should be NOT preceded with -, e.g. ""--hsflags w"" to skip warnings" & LINE_END
		help = help & "-export-trans file  Export translations to a file (experimental)" & LINE_END
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
		help = help & "                      " & supported_gfx_backends & " (tried in that order)" & LINE_END
		help = help & "-input-debug        Print extra debug info to c/g_debug.txt related to keyboard, mouse, etc. input" LINE_END
		help = help & "-nojoy              Disable joystick/gamepad support" & LINE_END
		help = help & "-nonumpad           Disable remapping numpad .0-9 to arrows/pageup/insert/etc. Can also toggle with Ctrl-Shift-N" & LINE_END
		help = help & "-no-native-kbd      Use US keyboard layout instead of OS-based text input (Default under gfx_sdl+X11 (ie. Linux, BSD))" & LINE_END
		help = help & "-native-kbd         Use OS-based text input instead of US keyboard layout (Default everywhere else)" & LINE_END
		help = help & "-f -fullscreen      Start in full-screen mode if possible" & LINE_END
		help = help & "-w -windowed        Start in windowed mode (default)" & LINE_END
		help = help & " Backend-specific options for gfx_" & gfxbackend & ": (use -gfx XYX -help to see others)" & LINE_END
		if gfx_describe_options() then
			help = help & *gfx_describe_options()
		end if
		display_help_string help
		terminate_program
		return 1
	elseif opt = "appdir" then
		'This special commandline arg is processed early, in get_app
		return 2
	elseif opt = "dump-embed" then
		dim success as bool = dump_embedded_file(arg, trimpath(arg))
		terminate_program iif(success, 0, 1)
		return 2
	elseif opt = "list-embeds" then
		'Prints to both stdout and *debug.txt so that you can see it on Windows
		list_embedded_files
		terminate_program
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
		remove_fb_error_handler  'setup_fb_error_handler already called
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
#if __FB_GUI__
	'Printing to the console doesn't work under Windows if compiled with -s gui so create a window.
	'Don't do this under Unix, it's annoying and adds fbgfx as a dependency
	if len(help) > 500 then
		screen 20   ' create a graphical fake text console (1024x768, 128x48 characters)
	else
		screen 11
	end if
	'Haaaaaack. fbgfx has builtin fonts which are code page 437 (US English IBM PC).
	'So translate a couple characters we might use from Latin-1 (CP1252) to CP437
	'Update: hróðvitnir is behind us, don't need this anymore!
	'replacestr help, !"\&hF0", !"\&hEB"   ' ð -> δ
	'replacestr help, !"\&hF3", !"\&hA2"   ' ó

	print help,   ' display the help on the graphical console
	dim k as string = input(1)  ' use FreeBasic-style keypress checking because our keyhandler isn't set up yet
	screen 0
#else
	'Convert the string to system (multibyte) encoding
	print utf8_to_mbs(latin1_to_utf8(help))
#endif
end sub

' This function exists to be hooked by gdb after we've opened
' the channel (without which, Custom's attempt to open it times out).
' It needs to be in a module other than yetmore2.bas so that it doesn't get inlined.
SUB hook_after_attach_to_Custom(success as bool)
END SUB
