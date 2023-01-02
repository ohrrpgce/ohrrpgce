'OHRRPGCE - Minimal graphics test program
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
'
#include "config.bi"
#include "allmodex.bi"
#include "common.bi"

DIM music_file as string = ""   'Leave blank, or else should be ogg, mp3, or mod/it/s3m/xm

'======================== Setup directories & debug log =======================

orig_dir = CURDIR
'Note: debug log messages go in CURDIR until log_dir set below

'Processes the -appdir commandline flag
'set_app_dir

log_dir = CURDIR & SLASH

'Once log_dir is set, can create debug log.
start_new_debug "Starting OHRRPGCE Custom"
debuginfo DATE & " " & TIME
debuginfo long_version & build_info
debuginfo "exepath: " & EXEPATH & ", exe: " & COMMAND(0)
debuginfo "orig_dir: " & orig_dir
debuginfo "curdir: " & CURDIR
' Load these three strings with info collectable before backend initialisation
read_backend_info()
debuginfo "Runtime info: " & gfxbackendinfo & "  " & musicbackendinfo & "  " & systeminfo

settings_dir = get_settings_dir()
'documents_dir = get_documents_dir()  'may depend on app_dir
'debuginfo "documents_dir: " & documents_dir
'Plus, tmpdir is shared between all running copies of Custom, which could cause problems.
tmpdir = settings_dir & SLASH
IF NOT isdir(tmpdir) THEN fatalerror "Unable to create temp directory " & tmpdir

'set_global_config_file
'debuginfo "config: " & global_config_file

'======================= Initialise backends/graphics =========================


load_gfx_defaults  'Loads master(), uilook(), boxlook(), current_font()

set_resolution 320, 200
setmodex

IF music_file <> "" THEN
	debuginfo musicbackendinfo  'Preliminary info before initialising backend
	setupmusic

	loadsong music_file
END IF

'=================================== Test =====================================


SUB draw_gfx(it as integer)
	clearpage vpage

	wrapprint long_version & build_info & !"\n" & gfxbackendinfo & !"\n" & musicbackendinfo & !"\n" & systeminfo, 0, 20, , vpage

	FOR radius as integer = 16 TO 80 STEP 8
		DIM as double semimajor = 0.3333 * radius
		DIM as double semiminor = 1 * radius
		DIM as double angle = it / 60 * radius / 100
		DIM as XYPair el_center = XY(160, 140)

		DIM col as integer = findrgb(0, 3 * radius, 255 - 2 * radius)
		ellipse vpages(vpage), el_center.x, el_center.y, semimajor, col, , semiminor, angle
	NEXT
END SUB

SUB draw_loop
	switch_to_32bit_vpages()
	toggle_fps_display

	FOR it as integer = 0 TO 999999
		setwait 16.666
		setkeys
		IF getquitflag ORELSE keyval(ccCancel) > 1 THEN EXIT FOR

		draw_gfx it
		draw_basic_mouse_cursor vpage
		draw_keys_overlay vpage
		setvispage vpage
		dowait
	NEXT
END SUB


draw_loop

closemusic
restoremode
debuginfo "End."
