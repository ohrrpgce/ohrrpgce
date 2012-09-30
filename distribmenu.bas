'OHRRPGCE CUSTOM - Distribute game menu
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'Except, this module isn't much crappier than it needs to be
'
#ifdef LANG_DEPRECATED
 #define __langtok #lang
 __langtok "deprecated"
 OPTION STATIC
 OPTION EXPLICIT
#endif

#include "config.bi"
#include "ver.txt"
#include "udts.bi"
#include "const.bi"
#include "allmodex.bi"
#include "common.bi"
#include "loading.bi"
#include "customsubs.bi"
#include "flexmenu.bi"
#include "slices.bi"
#include "cglobals.bi"
#include "uiconst.bi"
#include "scrconst.bi"

DECLARE SUB distribute_game_as_zip ()
DECLARE SUB distribute_game_as_windows_installer ()
DECLARE FUNCTION get_windows_gameplayer() as string
DECLARE FUNCTION get_linux_gameplayer() as string
DECLARE FUNCTION get_mac_gameplayer() as string
DECLARE FUNCTION find_or_download_innosetup () as string
DECLARE FUNCTION find_innosetup () as string
DECLARE FUNCTION win_or_wine_drive(letter as string) as string
DECLARE FUNCTION win_or_wine_spawn_and_wait (cmd as string, args as string="") as string
DECLARE SUB write_innosetup_script (basename as string, gamename as string, isstmp as string)
DECLARE SUB add_innosetup_file (s as string, filename as string)
DECLARE FUNCTION win_path (filename as string) as string
DECLARE FUNCTION copy_or_relump (src_rpg_or_rpgdir as string, dest_rpg as string) as integer
DECLARE FUNCTION copy_windows_gameplayer (gameplayer as string, basename as string, destdir as string) as integer
DECLARE SUB find_required_dlls(gameplayer as string, byref files as string vector)
DECLARE FUNCTION copy_linux_gameplayer (gameplayer as string, basename as string, destdir as string) as integer
DECLARE SUB distribute_game_as_debian_package ()
DECLARE FUNCTION get_debian_package_version() as string
DECLARE FUNCTION get_debian_package_name() as string
DECLARE SUB write_linux_menu_file(title as string, filename as string, basename as string)
DECLARE SUB write_linux_desktop_file(title as string, filename as string, basename as string)
DECLARE SUB write_debian_binary_file (filename as string)
DECLARE SUB write_debian_control_file(controlfile as string, basename as string, pkgver as string, size_in_kibibytes as integer, byref distinfo as DistribState)
DECLARE SUB write_debian_copyright_file (filename as string)
DECLARE FUNCTION gzip_file (filename as string) as integer
DECLARE FUNCTION gunzip_file (filename as string) as integer
DECLARE FUNCTION create_tarball(start_in_dir as string, tarball as string, files as string) as integer
DECLARE FUNCTION extract_tarball(into_dir as string, tarball as string, files as string) as integer
DECLARE FUNCTION create_ar_archive(start_in_dir as string, archive as string, files as string) as integer
DECLARE SUB fix_deb_group_permissions(start_at_dir as string)
DECLARE SUB write_debian_postrm_script (filename as string)
DECLARE SUB write_debian_postinst_script (filename as string)
DECLARE FUNCTION can_run_windows_exes () as integer
DECLARE FUNCTION can_make_debian_packages () as integer
DECLARE FUNCTION can_make_mac_packages () as integer
DECLARE SUB edit_distrib_info ()
DECLARE FUNCTION sanitize_pkgname(s as string) as string
DECLARE FUNCTION sanitize_email(s as string) as string
DECLARE FUNCTION sanitize_url(s as string) as string
DECLARE SUB export_readme_text_file (LE as string=LINE_END, byval wrap as integer=72)
DECLARE SUB write_readme_text_file (filename as string, LE as string=LINE_END, byval wrap as integer=72)
DECLARE SUB maybe_write_license_text_file (filename as string)
DECLARE FUNCTION is_known_license(license_code as string) as integer
DECLARE FUNCTION generate_copyright_line(distinfo as DistribState) as string
DECLARE FUNCTION browse_licenses(old_license as string) as string
DECLARE SUB distribute_game_as_mac_app ()

CONST distmenuEXIT as integer = 1
CONST distmenuZIP as integer = 2
CONST distmenuWINSETUP as integer = 3
CONST distmenuMACSETUP as integer = 4
CONST distmenuDEBSETUP as integer = 5
CONST distmenuINFO as integer = 6
CONST distmenuREADME as integer = 7

SUB distribute_game ()
 
 DIM menu as SimpleMenuItem vector
 v_new menu, 0
 append_simplemenu_item menu, "Previous Menu...", , , distmenuEXIT
 append_simplemenu_item menu, " Game file: " & trimpath(sourcerpg), YES, uilook(uiDisabledItem)

 append_simplemenu_item menu, "Edit distribution info...", , , distmenuINFO

 append_simplemenu_item menu, "Export .ZIP", , , distmenuZIP

 IF can_run_windows_exes() THEN
  append_simplemenu_item menu, "Export Windows Installer", , , distmenuWINSETUP
 ELSE
  append_simplemenu_item menu, "Can't Export Windows Installer", YES, uilook(uiDisabledItem)
  append_simplemenu_item menu, " (requires Windows or wine)", YES, uilook(uiDisabledItem)
 END IF

 append_simplemenu_item menu, "Export Mac OS X App Bundle", , , distmenuMACSETUP
 IF NOT can_make_mac_packages() THEN
  append_simplemenu_item menu, " (requires tar+gzip)", YES, uilook(uiDisabledItem)
 END IF

 append_simplemenu_item menu, "Export Debian Linux Package", , , distmenuDEBSETUP
 IF NOT can_make_debian_packages() THEN
  append_simplemenu_item menu, " (requires ar+tar+gzip)", YES, uilook(uiDisabledItem)
 END IF

 append_simplemenu_item menu, "Export README text file", , , distmenuREADME

 DIM st AS MenuState
 init_menu_state st, cast(BasicMenuItem vector, menu)

 DO
  setwait 55
  setkeys

  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "distribute_game"
  IF enter_or_space() THEN
   SELECT CASE menu[st.pt].dat
    CASE distmenuEXIT: EXIT DO
    CASE distmenuZIP:
     save_current_game
     distribute_game_as_zip
    CASE distmenuWINSETUP:
     save_current_game
     distribute_game_as_windows_installer
    CASE distmenuMACSETUP:
     save_current_game
     distribute_game_as_mac_app
    CASE distmenuDEBSETUP:
     save_current_game
     distribute_game_as_debian_package
    CASE distmenuINFO:
     edit_distrib_info
    CASE distmenuREADME:
     export_readme_text_file
   END SELECT
  END IF

  usemenu st, cast(BasicMenuItem vector, menu)
  
  IF st.need_update THEN
  END IF

  clearpage dpage
  standardmenu cast(BasicMenuItem vector, menu), st, 0, 0, dpage
  
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 setkeys
 v_free menu
END SUB

SUB edit_distrib_info ()

 DIM rootsl as Slice Ptr
 rootsl = NewSliceOfType(slRoot)
 rootsl->Fill = YES
 DIM infosl as Slice Ptr
 infosl = NewSliceOfType(slText, rootsl)
 infosl->Width = 320
 infosl->AnchorVert = 2
 infosl->AlignVert = 2
 ChangeTextSlice infosl, , uilook(uiText), YES, YES

 DIM distinfo as DistribState
 load_distrib_state distinfo

 DIM menu as SimpleMenuItem vector
 v_new menu, 0

 append_simplemenu_item menu, "Previous Menu..."

 append_simplemenu_item menu, "Package name: " & distinfo.pkgname
 append_simplemenu_item menu, "Game name: " & distinfo.gamename
 append_simplemenu_item menu, "Author: " & distinfo.author
 append_simplemenu_item menu, "Email: " & distinfo.email
 append_simplemenu_item menu, "Description: " & distinfo.description
 append_simplemenu_item menu, "More Description: " & distinfo.more_description
 append_simplemenu_item menu, "Website: " & distinfo.website
 append_simplemenu_item menu, "Copyright year: " & distinfo.copyright_year
 append_simplemenu_item menu, "License: " & distinfo.license

 DIM st AS MenuState
 init_menu_state st, cast(BasicMenuItem vector, menu)
 st.need_update = YES

 DO
  setwait 55
  setkeys YES

  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN
   SELECT CASE st.pt
    CASE 1: show_help "edit_distrib_info_pkgname"
    CASE 2: show_help "edit_distrib_info_gamename"
    CASE 3: show_help "edit_distrib_info_author"
    CASE 4: show_help "edit_distrib_info_email"
    CASE 5: show_help "edit_distrib_info_description"
    CASE 6: show_help "edit_distrib_info_more_description"
    CASE 7: show_help "edit_distrib_info_website"
    CASE 8: show_help "edit_distrib_info_copyright_year"
    CASE 9: show_help "edit_distrib_info_license"
    CASE ELSE
     show_help "edit_distrib_info"
   END SELECT
  END IF
  IF enter_or_space() THEN
   SELECT CASE st.pt
    CASE 0: EXIT DO
   END SELECT
  END IF
  IF keyval(scEnter) > 1 THEN
   SELECT CASE st.pt
    CASE 1: distinfo.pkgname = multiline_string_editor(distinfo.pkgname, "edit_distrib_info_pkgname")
    CASE 2: distinfo.gamename = multiline_string_editor(distinfo.gamename, "edit_distrib_info_gamename")
    CASE 3: distinfo.author = multiline_string_editor(distinfo.author, "edit_distrib_info_author")
    CASE 4: distinfo.email = multiline_string_editor(distinfo.email, "edit_distrib_info_email")
    CASE 5: distinfo.description = multiline_string_editor(distinfo.description, "edit_distrib_info_description")
    CASE 6: distinfo.more_description = multiline_string_editor(distinfo.more_description, "edit_distrib_info_more_description")
    CASE 7: distinfo.website = multiline_string_editor(distinfo.website, "edit_distrib_info_website")
    CASE 8: distinfo.copyright_year = multiline_string_editor(distinfo.copyright_year, "edit_distrib_info_copyright_year")
    CASE 9: distinfo.license = browse_licenses(distinfo.license)
   END SELECT
   st.need_update = YES
  END IF
   
  SELECT CASE st.pt
   CASE 1: IF strgrabber(distinfo.pkgname, 32767) THEN st.need_update = YES
   CASE 2: IF strgrabber(distinfo.gamename, 32767) THEN st.need_update = YES
   CASE 3: IF strgrabber(distinfo.author, 32767) THEN st.need_update = YES
   CASE 4: IF strgrabber(distinfo.email, 32767) THEN st.need_update = YES
   CASE 5: IF strgrabber(distinfo.description, 32767) THEN st.need_update = YES
   CASE 6: IF strgrabber(distinfo.more_description, 32767) THEN st.need_update = YES
   CASE 7: IF strgrabber(distinfo.website, 32767) THEN st.need_update = YES
   CASE 8: IF strgrabber(distinfo.copyright_year, 32767) THEN st.need_update = YES
  END SELECT

  IF usemenu(st, cast(BasicMenuItem vector, menu)) THEN st.need_update = YES
  
  IF st.need_update THEN
   distinfo.pkgname = sanitize_pkgname(distinfo.pkgname)
   distinfo.gamename = special_char_sanitize(exclude(distinfo.gamename, "/\""" & CHR(10)))
   distinfo.author = special_char_sanitize(exclude(distinfo.author, "<>@""" & CHR(10)))
   distinfo.email = sanitize_email(distinfo.email)
   distinfo.website = sanitize_url(distinfo.website)
   distinfo.copyright_year = exclusive(distinfo.copyright_year, "0123456789 -,")
   menu[1].text = "Package name: " & distinfo.pkgname
   menu[2].text = "Game name: " & distinfo.gamename
   menu[3].text = "Author: " & distinfo.author
   menu[4].text = "Email: " & distinfo.email
   menu[5].text = "Description: " & distinfo.description
   menu[6].text = "More Description: " & distinfo.more_description
   menu[7].text = "Website: " & distinfo.website
   menu[8].text = "Copyright year: " & distinfo.copyright_year
   menu[9].text = "License: " & distinfo.license
   IF st.pt = 8 ORELSE st.pt = 9 THEN
    ChangeTextSlice infosl, generate_copyright_line(distinfo)
   ELSEIF (st.pt >= 5 ANDALSO st.pt <= 6) ORELSE LEN(menu[st.pt].text) >= 40 THEN
    ChangeTextSlice infosl, "Press ENTER to edit multiple lines"
   ELSE
    ChangeTextSlice infosl, ""
   END IF
   st.need_update = NO
  END IF

  clearpage dpage
  DrawSlice rootsl, dpage
  standardmenu cast(BasicMenuItem vector, menu), st, 0, 0, dpage
  
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 setkeys
 v_free menu

 save_distrib_state distinfo

END SUB

FUNCTION sanitize_pkgname(s as string) as string
 RETURN LCASE(special_char_sanitize(exclude(s, "/\ ""'" + CHR(10))))
END FUNCTION

FUNCTION sanitize_email(s as string) as string
 '--This e-mail address sanitization is far from perfect, but good enough for most cases
 RETURN special_char_sanitize(exclusive(s, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_@.+"))
END FUNCTION

FUNCTION sanitize_url(s as string) as string
 '--This website address sanitization is far from perfect, but probably good enough for most cases
 RETURN special_char_sanitize(exclusive(s, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-.:/_+%:;?@=&'"))
END FUNCTION

SUB export_readme_text_file (LE as string=LINE_END, byval wrap as integer=72)

 DIM distinfo as DistribState
 load_distrib_state distinfo

 DIM txtfile as string = trimfilename(sourcerpg) & SLASH & "README-" & distinfo.pkgname & ".txt"
 
 DIM shortname as string = trimpath(txtfile)
 IF isfile(txtfile) THEN
  IF yesno(shortname & " already exists, are you sure you want to overwrite it?", NO) = NO THEN RETURN
 END IF
 safekill txtfile
 write_readme_text_file txtfile, LE
 IF isfile(txtfile) THEN visible_debug "Created " & shortname
 
END SUB

SUB write_readme_text_file (filename as string, LE as string=LINE_END, byval wrap as integer=72)
 'LE is passed instead of using LINE_END directly so we can override it easily.

 DIM LF as string = CHR(10)
 
 DIM distinfo as DistribState
 load_distrib_state distinfo

 '--Construct the file
 DIM s as string = ""
 
 s &= distinfo.gamename & LF
 s &= generate_copyright_line(distinfo) & LF
 
 s &= LF & distinfo.description
 IF NOT ends_with(s, LF) THEN s &= LF
 
 IF LEN(TRIM(distinfo.more_description)) THEN
  s &= LF & distinfo.more_description
 END IF
 IF NOT ends_with(s, LF) THEN s &= LF

 s &= LF

 IF LEN(distinfo.website) THEN s &= distinfo.website & LF
 IF LEN(distinfo.email) THEN s &= distinfo.email & LF
 
 '--format the lines
 s = wordwrap(s, wrap)
 
 IF LF <> LE THEN
  'If we want this text to have DOS/Windows line endings, convert it now
  replacestr(s, LF, LE)
 END IF

 '--write the file to disk
 DIM fh as integer = FREEFILE
 OPEN filename for binary as #fh
 PUT #fh, , s
 CLOSE #fh
  
END SUB

SUB maybe_write_license_text_file (filename as string)

 'For some types of licenses, include a text copy

 DIM distinfo as DistribState
 load_distrib_state distinfo

 IF distinfo.license = "COPYRIGHT" THEN EXIT SUB
 IF distinfo.license = "PUBLICDOMAIN" THEN EXIT SUB
 
 DIM helpdir as string = get_help_dir()
 DIM lic_file as string = helpdir & SLASH & "license_" & LCASE(distinfo.license) & ".txt"
 
 IF NOT isfile(lic_file) THEN debug lic_file & " does not exist": EXIT SUB
 
 copyfile lic_file, filename
 
END SUB

SUB write_debian_copyright_file (filename as string)

 DIM LF as string = CHR(10)

 DIM distinfo as DistribState
 load_distrib_state distinfo

 '--Construct the file
 DIM s as string = ""
 
 s &= distinfo.gamename & LF
 s &= generate_copyright_line(distinfo) & LF
 IF distinfo.license = "GPL" THEN
  s &= LF & "See /usr/share/common-licenses/GPL-3 for details" & LF
 END IF

 '--write the file to disk
 DIM fh as integer = FREEFILE
 OPEN filename for binary as #fh
 PUT #fh, , s
 CLOSE #fh
END SUB

FUNCTION browse_licenses(old_license as string) as string
 'duplicated known_licenses because global string arrays are a pain in the ass
 DIM known_licenses(9) as string = {"COPYRIGHT", "PUBLICDOMAIN", "GPL", "MIT", "CC-BY", "CC-BY-SA", "CC-BY-ND", "CC-BY-NC", "CC-BY-NC-SA", "CC-BY-NC-ND"}
 DIM old_index as integer = 0
 FOR i as integer = 0 TO UBOUND(known_licenses)
  IF old_license = known_licenses(i) THEN old_index = i
 NEXT i
 DIM which as integer
 which = multichoice("Choose a copyright license", known_licenses(), old_index, , "edit_distrib_info_license")
 IF which = -1 THEN RETURN old_license
 RETURN known_licenses(which)
END FUNCTION

FUNCTION is_known_license(license_code as string) as integer
 'duplicated known_licenses because global string arrays are a pain in the ass
 DIM known_licenses(9) as string = {"COPYRIGHT", "PUBLICDOMAIN", "GPL", "MIT", "CC-BY", "CC-BY-SA", "CC-BY-ND", "CC-BY-NC", "CC-BY-NC-SA", "CC-BY-NC-ND"}
 FOR i as integer = 0 TO UBOUND(known_licenses)
  IF license_code = known_licenses(i) THEN RETURN YES
 NEXT i
 RETURN NO
END FUNCTION

FUNCTION generate_copyright_line(distinfo as DistribState) as string
 DIM c_y_by as string = "(C) Copyright " & distinfo.copyright_year & " " & distinfo.author
 SELECT CASE distinfo.license
  CASE "COPYRIGHT":
   RETURN c_y_by
  CASE "PUBLICDOMAIN":
   RETURN "Copyright released by " & distinfo.author & " into the Public Domain."
  CASE "GPL":
   RETURN c_y_by & ". this game is distributed under the terms of the GNU General Public License."
  CASE "MIT":
   RETURN c_y_by & ". this game is distributed under the terms of the MIT license."
  CASE "CC-BY":
   RETURN c_y_by & ". This game is licensed under a Creative Commons Attribution 3.0 Unported License."
  CASE "CC-BY-SA":
   RETURN c_y_by & ". This game is licensed under a Creative Commons Attribution-ShareAlike 3.0 Unported License."
  CASE "CC-BY-ND":
   RETURN c_y_by & ". This game is licensed under a Creative Commons Attribution-NoDerivs 3.0 Unported License."
  CASE "CC-BY-NC":
   RETURN c_y_by & ". This game is licensed under a Creative Commons Attribution-NonCommercial 3.0 Unported License."
  CASE "CC-BY-NC-SA":
   RETURN c_y_by & ". This game is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License."
  CASE "CC-BY-NC-ND":
   RETURN c_y_by & ". This game is licensed under a Creative Commons Attribution-NonCommercial-NoDerivs 3.0 Unported License."
  CASE ELSE:
   RETURN distinfo.license & " is not in the list of licenses that this program understands"
 END SELECT
END FUNCTION

SUB distribute_game_as_zip ()

 DIM distinfo as DistribState
 load_distrib_state distinfo

 DIM zip as string = find_helper_app("zip", YES)
 IF zip = "" THEN
  visible_debug "Can't create zip files: " & missing_helper_message("zip" + DOTEXE)
  RETURN
 END IF

 DIM destzip as string = trimfilename(sourcerpg) & SLASH & distinfo.pkgname & ".zip"
 DIM shortzip as string = trimpath(destzip)
 IF isfile(destzip) THEN
  IF yesno(shortzip & " already exists. Overwrite it?") = NO THEN RETURN
  safekill destzip
 END IF

 DIM ziptmp as string = trimfilename(sourcerpg) & SLASH & "zip.tmp"
 IF isdir(ziptmp) THEN
  killdir ziptmp
 END IF

 DIM use_gameplayer as integer = YES
 DIM gameplayer as string
 gameplayer = get_windows_gameplayer()
 IF gameplayer = "" THEN
  IF yesno("game.exe is not available, continue anyway?") = NO THEN RETURN
  use_gameplayer = NO
 END IF

 makedir ziptmp
 IF NOT isdir(ziptmp) THEN
  visible_debug "ERROR: unable to create temporary folder"
  RETURN
 END IF

 DIM spawn_ret as string

 DO 'Single-pass loop for operations after ziptmp exists
  
  DIM basename as string = distinfo.pkgname
  
  IF copy_or_relump(sourcerpg, ziptmp & SLASH & basename & ".rpg") = NO THEN EXIT DO

  IF use_gameplayer THEN
   IF copy_windows_gameplayer(gameplayer, basename, ziptmp) = NO THEN EXIT DO
  END IF
 
  'Write readme with DOS/Window line endings
  write_readme_text_file ziptmp & SLASH & "README-" & basename & ".txt", CHR(13) & CHR(10)
  maybe_write_license_text_file ziptmp & SLASH & "LICENSE.txt"
 
  DIM args as string = "-r -j """ & destzip & """ """ & ziptmp & """"
  spawn_ret = spawn_and_wait(zip, args)
  IF LEN(spawn_ret) ORELSE NOT isfile(destzip) THEN
   safekill destzip
   visible_debug "Zip file creation failed." & spawn_ret
   RETURN
  END IF
  
  visible_debug "Successfully created " & shortzip

  EXIT DO 'single pass, never really loops.
 LOOP
 'Cleanup ziptmp
 killdir ziptmp
 
END SUB

FUNCTION copy_or_relump (src_rpg_or_rpgdir as string, dest_rpg as string) as integer
 'Return true on success, false on fail

 DIM extension as string = LCASE(justextension(src_rpg_or_rpgdir))

 IF extension = "rpgdir" THEN
  DIM relump as string
  relump = find_helper_app("relump", YES)
  IF relump = "" THEN visible_debug "Can't find relump" & DOTEXE & " utility." : RETURN NO
  DIM spawn_ret as string
  spawn_ret = spawn_and_wait(relump, """" & src_rpg_or_rpgdir & """ """ & dest_rpg & """")
  IF LEN(spawn_ret) ORELSE NOT isfile(dest_rpg) THEN
   visible_debug "ERROR: failed relumping " & src_rpg_or_rpgdir & " " & spawn_ret 
   RETURN NO
  END IF
 ELSE 'simple case for regular .rpg files
  IF confirmed_copy(src_rpg_or_rpgdir, dest_rpg) = NO THEN
   visible_debug "ERROR: failed to copy " & src_rpg_or_rpgdir
  END IF
 END IF
 RETURN YES
END FUNCTION

FUNCTION copy_windows_gameplayer (gameplayer as string, basename as string, destdir as string) as integer
 'Returns true on success, false on failure
 IF confirmed_copy(gameplayer, destdir & SLASH & basename & ".exe") = NO THEN RETURN NO
 DIM gamedir as string = trimfilename(gameplayer)

 
 DIM otherf as string vector
 v_new otherf
 v_append otherf, "LICENSE-binary.txt"

 find_required_dlls gameplayer, otherf
 
 FOR i as integer = 0 TO v_len(otherf) - 1
  IF confirmed_copy(gamedir & SLASH & otherf[i], destdir & SLASH & otherf[i]) = NO THEN
   v_free otherf
   RETURN NO
  END IF
 NEXT i

 v_free otherf
 RETURN YES
END FUNCTION

SUB find_required_dlls(gameplayer as string, byref files as string vector)

#IFDEF __FB_WIN32__
 IF gameplayer = exepath & SLASH & "game.exe" THEN
  '--if we are using a copy of the currently windows version,
  '--the backend might be non-default
  SELECT CASE gfxbackend
   CASE "directx":
    IF v_find(files, "gfx_directx.dll") = -1 THEN v_append(files, "gfx_directx.dll")
   CASE "sdl":
    IF v_find(files, "SDL.dll") = -1 THEN v_append(files, "SDL.dll")
   CASE "alleg":
    IF v_find(files, "alleg40.dll") = -1 THEN v_append(files, "alleg40.dll")
   CASE "fb":
    'gfx_fb requires no dll files
  END SELECT
  SELECT CASE musicbackend
   CASE "sdl":
    IF v_find(files, "SDL.dll") = -1 THEN v_append(files, "SDL.dll")
    IF v_find(files, "SDL_mixer.dll") = -1 THEN v_append(files, "SDL_mixer.dll")
   CASE "native", "native2":
    IF v_find(files, "audiere.dll") = -1 THEN v_append(files, "audiere.dll")
   CASE "silence":
    'music_silence requires no dll files
  END SELECT
  EXIT SUB
 END IF
#ENDIF 
 
 '--for all other cases and all other platforms, we use
 '--the dll files for the default backend(s) on windows
 v_append files, "gfx_directx.dll"
 v_append files, "SDL.dll"
 v_append files, "SDL_mixer.dll"
END SUB

FUNCTION copy_linux_gameplayer (gameplayer as string, basename as string, destdir as string) as integer
 'Returns true on success, false on failure
 IF confirmed_copy(gameplayer, destdir & SLASH & basename) = NO THEN RETURN NO
#IFDEF __UNIX__
  '--just in case we are playing with a debug build,
  '--strip the copy of the binary that goes in the distribution file.
  SHELL "strip '" & destdir & SLASH & basename & "'"
  '--fix the permissions
  SHELL "chmod +x '" & destdir & SLASH & basename & "'"
#ENDIF
 RETURN YES
END FUNCTION

FUNCTION get_windows_gameplayer() as string
 'On Windows, Return the full path to game.exe
 'On other platforms, download game.exe, unzip it, and return the full path
 'Returns "" for failure.

#IFDEF __FB_WIN32__

 '--If this is Windows, we already have the correct version of game.exe
 IF isfile(exepath & SLASH & "game.exe") THEN
  RETURN exepath & SLASH & "game.exe"
 ELSE
  visible_debug "ERROR: game.exe wasn't found in the same folder as custom.exe. (This shouldn't happen!)" : RETURN ""
 END IF

#ENDIF
 '--For Non-Windows platforms, we need to download game.exe
 '(NOTE: This all should work fine on Windows too, but it is best to use the installed game.exe)

 '--Find the folder that we are going to download game.exe into
 DIM dldir as string = settings_dir & SLASH & "_gameplayer"
 IF NOT isdir(dldir) THEN makedir dldir
 IF NOT isdir(dldir) THEN visible_debug "ERROR: Unable to create """ & dldir & """ directory": RETURN ""
  
 '--Decide which url to download
 DIM url as string
 DIM dlfile as string
 IF version_branch = "wip" THEN
  '--If running a nightly wip, download the latest nightly wip
  url = "http://hamsterrepublic.com/ohrrpgce/nightly/ohrrpgce-wip-default.zip"
  dlfile = "ohrrpgce-wip-default.zip"
 ELSE
  '--If running any stable release, download the latest stable release.
  url = "http://hamsterrepublic.com/dl/ohrrpgce-minimal.zip"
  dlfile = "ohrrpgce-minimal.zip"
 END IF

 '--Ask the user for permission the first time we download (subsequent updates don't ask)
 DIM destzip as string = dldir & SLASH & dlfile
 IF NOT isfile(destzip) THEN
  IF yesno("Is it okay to download the Windows version of OHRRPGCE game.exe from HamsterRepublic.com now?") = NO THEN RETURN ""
 END IF

 '--Actually download the dang file
 download_file url, dldir
 
 '--Find the unzip tool
 DIM unzip as string = find_helper_app("unzip", YES)
 IF unzip = "" THEN visible_debug "ERROR: Couldn't find unzip tool": RETURN ""
 
 '--Unzip the desired files
 DIM args as string = "-o """ & destzip & """ game.exe gfx_directx.dll SDL.dll SDL_mixer.dll LICENSE-binary.txt -d """ & dldir & """"
 DIM spawn_ret as string = spawn_and_wait(unzip, args)
 IF LEN(spawn_ret) > 0 THEN visible_debug "ERROR: unzip failed: " & spawn_ret : RETURN ""
 
 IF NOT isfile(dldir & SLASH & "game.exe")           THEN visible_debug "ERROR: Failed to unzip game.exe" : RETURN ""
 IF NOT isfile(dldir & SLASH & "gfx_directx.dll")    THEN visible_debug "ERROR: Failed to unzip gfx_directx.dll" : RETURN ""
 IF NOT isfile(dldir & SLASH & "SDL.dll")            THEN visible_debug "ERROR: Failed to unzip SDL.dll" : RETURN ""
 IF NOT isfile(dldir & SLASH & "SDL_mixer.dll")      THEN visible_debug "ERROR: Failed to unzip SDL_mixer.dll" : RETURN ""
 IF NOT isfile(dldir & SLASH & "LICENSE-binary.txt") THEN visible_debug "ERROR: Failed to unzip LICENSE-binary.txt" : RETURN ""
 
 RETURN dldir & SLASH & "game.exe"
END FUNCTION

FUNCTION get_linux_gameplayer() as string
 'On Linux, Return the full path to ohrrpgce-game
 'On other platforms, download a precompiled i386 binary of ohrrpgce-game,
 'unzip it, and return the full path.
 'Returns "" for failure.

#IFDEF __UNIX__
#IFNDEF __FB_DARWIN__

 '--If this is Linux, we already have the correct version of ohrrpgce-game
 IF isfile(exepath & SLASH & "ohrrpgce-game") THEN
  RETURN exepath & SLASH & "ohrrpgce-game"
 ELSE
  visible_debug "ERROR: ohrrpgce-game wasn't found in the same directory as ohrrpgce-custom. (This probably shouldn't happen!)" : RETURN ""
 END IF

#ENDIF
#ENDIF

 '--For Non-Linux platforms, we need to download ohrrpgce-game
 '(NOTE: This all should work fine on Linux too, but it is best to use the installed ohrrpgce-game)

 '--Find the folder that we are going to download ohrrpgce-game into
 DIM dldir as string = settings_dir & SLASH & "_gameplayer"
 IF NOT isdir(dldir) THEN makedir dldir
 IF NOT isdir(dldir) THEN visible_debug "ERROR: Unable to create """ & dldir & """ directory": RETURN ""
  
 '--Decide which url to download
 DIM url as string
 DIM dlfile as string
 

 IF version_branch = "wip" THEN
  'If using any wip release, get the latest wip release
  url = "http://hamsterrepublic.com/ohrrpgce/nightly/ohrrpgce-player-linux-bin-minimal.zip"
 ELSE
  'If using any stable release, get the latest stable release
  url = "http://hamsterrepublic.com/dl/ohrrpgce-player-linux-bin-minimal.zip"
 END IF
 dlfile = "ohrrpgce-player-linux-bin-minimal.zip"

 '--Ask the user for permission the first time we download (subsequent updates don't ask)
 DIM destzip as string = dldir & SLASH & dlfile
 IF NOT isfile(destzip) THEN
  IF yesno("Is it okay to download the Linux version of OHRRPGCE ohrrpgce-game from HamsterRepublic.com now?") = NO THEN RETURN ""
 END IF

 '--Actually download the dang file
 download_file url, dldir
 
 '--Find the unzip tool
 DIM unzip as string = find_helper_app("unzip", YES)
 IF unzip = "" THEN visible_debug "ERROR: Couldn't find unzip tool": RETURN ""
 
 '--Unzip the desired files
 DIM args as string = "-o """ & destzip & """ ohrrpgce-game LICENSE-binary.txt -d """ & dldir & """"
 DIM spawn_ret as string = spawn_and_wait(unzip, args)
 IF LEN(spawn_ret) > 0 THEN visible_debug "ERROR: unzip failed: " & spawn_ret : RETURN ""
 
 IF NOT isfile(dldir & SLASH & "ohrrpgce-game")      THEN visible_debug "ERROR: Failed to unzip ohrrpgce-game" : RETURN ""
 IF NOT isfile(dldir & SLASH & "LICENSE-binary.txt") THEN visible_debug "ERROR: Failed to unzip LICENSE-binary.txt" : RETURN ""
 
 RETURN dldir & SLASH & "ohrrpgce-game"

END FUNCTION

SUB distribute_game_as_windows_installer ()

 DIM distinfo as DistribState
 load_distrib_state distinfo

 DIM basename as string = distinfo.pkgname
 DIM installer as string = trimfilename(sourcerpg) & SLASH & "setup-" & basename & ".exe"

 IF isfile(installer) THEN
  IF yesno(trimpath(installer) & " already exists. Overwrite it?") = NO THEN RETURN
  safekill installer
 END IF

 DIM iscc as string = find_or_download_innosetup()
 IF iscc = "" THEN RETURN
 
 DIM isstmp as string = trimfilename(sourcerpg) & SLASH & "innosetup.tmp"
 IF isdir(isstmp) THEN
  killdir isstmp
 END IF
 makedir isstmp

 DO '--single pass loop for breaking

  IF copy_or_relump(sourcerpg, isstmp & SLASH & basename & ".rpg") = NO THEN EXIT DO
 
  DIM gameplayer as string
  gameplayer = get_windows_gameplayer()
  IF gameplayer = "" THEN visible_debug "ERROR: game.exe is not available" : EXIT DO
  IF copy_windows_gameplayer(gameplayer, basename, isstmp) = NO THEN EXIT DO

  'Write readme with DOS/Window line endings
  write_readme_text_file isstmp & SLASH & "README-" & basename & ".txt", CHR(13) & CHR(10)

  maybe_write_license_text_file isstmp & SLASH & "LICENSE.txt"
  
  write_innosetup_script basename, distinfo.gamename, isstmp

  DIM iss_script as string = isstmp & SLASH & "innosetup_script.iss"
 
  DIM args as string
  args = """" & win_path(iss_script) & """"
  
  DIM spawn_ret as string
  spawn_ret = win_or_wine_spawn_and_wait(iscc,  args)
  IF LEN(spawn_ret) THEN visible_debug "ERROR: iscc.exe failed: " & spawn_ret : EXIT DO
  IF confirmed_copy(isstmp & SLASH & "Output" & SLASH & "setup-" & basename & ".exe", installer) = NO THEN
   visible_debug "ERROR: iscc.exe completed but installer was not created"
   EXIT DO
  END IF

  visible_debug trimpath(installer) & " was successfully created!"
  EXIT DO 'this loop is only ever one pass
 LOOP

 '--Cleanup temp files
 IF isdir(isstmp & SLASH & "Output") THEN killdir isstmp & SLASH & "Output"
 killdir isstmp
 
END SUB

SUB write_innosetup_script (basename as string, gamename as string, isstmp as string)

 DIM iss_script as string = isstmp & SLASH & "innosetup_script.iss"

 DIM s as string
 DIM E as string = !"\r\n" ' E is End of line
 s &= "; Inno Setup script generated by OHRRPGCE custom" & E
 
 s &= E & "[Setup]" & E
 s &= "AppName=" & gamename & E
 s &= "AppVersion=" & MID(DATE, 7, 4) & "." & MID(DATE, 1, 2) & "." & MID(DATE, 4, 2) & E
 s &= "DefaultDirName={pf}\OHRRPGCE Games\" & gamename & E
 s &= "DefaultGroupName=" & gamename & E
 s &= "SolidCompression=yes" & E
 s &= "OutputBaseFilename=setup-" & basename & E
 s &= "InfoAfterFile=README-" & basename & ".txt" & E
 IF isfile(isstmp & SLASH & "LICENSE.txt") THEN
  s &= "LicenseFile=LICENSE.txt" & E
 END IF

 s &= E & "[Languages]" & E
 s &= "Name: ""eng""; MessagesFile: ""compiler:Default.isl""" & E

 s &= E & "[Files]" & E
 add_innosetup_file s, isstmp & SLASH & basename & ".rpg"
 add_innosetup_file s, isstmp & SLASH & basename & ".exe"

 'include whichever .dll files are in the isstmp folder
 DIM dlls() as string
 findfiles isstmp, "*.dll", fileTypeFile, YES, dlls()
 FOR i as integer = 0 TO UBOUND(dlls)
  add_innosetup_file s, isstmp & SLASH & dlls(i)
 NEXT i
 
 add_innosetup_file s, isstmp & SLASH & "LICENSE-binary.txt"
 add_innosetup_file s, isstmp & SLASH & "README-" & basename & ".txt"
 IF isfile(isstmp & SLASH & "LICENSE.txt") THEN
  add_innosetup_file s, isstmp & SLASH & "LICENSE.txt"
 END IF

 s &= E & "[Icons]" & E
 s &= "Name: ""{userdesktop}\" & gamename & """; Filename: ""{app}\" & basename & ".exe""; WorkingDir: ""{app}"";" & E
 s &= "Name: ""{group}\" & gamename & """; Filename: ""{app}\" & basename & ".exe""; WorkingDir: ""{app}"";" & E
 
 debuginfo s
 
 DIM fh as integer = FREEFILE
 OPEN iss_script FOR BINARY AS #fh
 PUT #fh, 1, s
 CLOSE #fh

END SUB

SUB add_innosetup_file (s as string, filename as string)
 DIM E as string = !"\r\n" ' E is End of line
 s &= "Source: """ & win_path(filename) & """; DestDir: ""{app}""; Flags: ignoreversion" & E
END SUB

FUNCTION win_path (filename as string) as string
#IFDEF __FB_WIN32__
 'This is a do-nothing on real Windows
 RETURN filename
#ELSE
 'When using wine, paths that start with $HOME can be translated to Z:
 IF LEFT(filename, 1) <> "/" THEN
  visible_debug "ERROR: Unable to translate path for wine: " & filename 
  RETURN filename
 END IF
 DIM winepath as string = "z:" & filename
 replacestr winepath, "/", "\"
 RETURN winepath
#ENDIF
END FUNCTION

FUNCTION find_or_download_innosetup () as string
 DIM iscc as string = find_innosetup()
 IF iscc = "" THEN
  IF yesno("Inno Setup 5 is required to create windows installation packages. Would you like to download it from jrsoftware.org now?") THEN
   download_file "http://www.jrsoftware.org/download.php/is.exe", settings_dir, "is.exe"
   DIM spawn_ret as string
   spawn_ret = win_or_wine_spawn_and_wait(settings_dir & SLASH & "is.exe")
   safekill settings_dir & SLASH & "is.exe"
   IF LEN(spawn_ret) THEN visible_debug "ERROR: Inno Setup installer failed: " & spawn_ret : RETURN ""
   '--re-search for iscc now that it may have been installed
   iscc = find_innosetup()
  END IF
  IF iscc = "" THEN visible_debug "Canceling export. Inno Setup 5 is not available." : RETURN ""
 END IF
 RETURN iscc
END FUNCTION

FUNCTION find_innosetup () as string
 DIM c_drive as string = win_or_wine_drive("c")

 DIM iscc as string
 iscc = c_drive & SLASH & "Program Files" & SLASH & "Inno Setup 5" & SLASH & "ISCC.exe"
 IF isfile(iscc) THEN RETURN iscc
 iscc = c_drive & SLASH & "Program Files (x86)" & SLASH & "Inno Setup 5" & SLASH & "ISCC.exe"
 IF isfile(iscc) THEN RETURN iscc

 RETURN "" 'Not found
END FUNCTION

FUNCTION win_or_wine_drive(letter as string) as string
#IFDEF __FB_WIN32__
 RETURN letter & ":"
#ELSE
 RETURN environ("HOME") & "/.wine/dosdevices/" & letter & ":"
#ENDIF
END FUNCTION

FUNCTION win_or_wine_spawn_and_wait (cmd as string, args as string="") as string
 'For running Windows programs only. On Windows run natively, on Linux Unix Mac, try to run with Wine
 'Currently only needed for installing and running innosetup. Hopefully we won't ever need it for anything else
 DIM spawn_ret as string
#IFDEF __FB_WIN32__
 'On Windows this is nice and simple
 debuginfo "spawn_and_wait: " & cmd & " " & args
 RETURN spawn_and_wait(cmd, args)
#ELSE
 DIM wine_args as string = """" & cmd & """ " & escape_string(args, "\")
 debuginfo "spawn_and_wait: wine " & cmd & " " & wine_args
 debuginfo "wine_args =" & wine_args
 RETURN spawn_and_wait("wine", wine_args)
#ENDIF
 
END FUNCTION

SUB distribute_game_as_debian_package ()

 DIM distinfo as DistribState
 load_distrib_state distinfo

 DIM basename as string = get_debian_package_name()
 DIM pkgver as string = get_debian_package_version()
 DIM debname as string = trimfilename(sourcerpg) & SLASH & basename & "_" & pkgver & "_i386.deb"

 IF isfile(debname) THEN
  IF yesno(trimpath(debname) & " already exists. Overwrite it?") = NO THEN RETURN
  safekill debname
 END IF

 DIM debtmp as string = trimfilename(sourcerpg) & SLASH & "debpkg.tmp"
 IF isdir(debtmp) THEN
  debuginfo "Clean up old " & debtmp
  killdir debtmp, YES
 END IF
 
 debuginfo "Prepare package data files..."
 makedir debtmp
 makedir debtmp & SLASH & "usr"
 makedir debtmp & SLASH & "usr" & SLASH & "share"
 makedir debtmp & SLASH & "usr" & SLASH & "share" & SLASH & "games"
 DIM bindir as string = debtmp & SLASH & "usr" & SLASH & "games"
 makedir bindir
 DIM datadir as string = debtmp & SLASH & "usr" & SLASH & "share" & SLASH & "games" & SLASH & basename
 makedir datadir

 DO '--single pass loop for breaking

  debuginfo "Copy rpg file"
  IF copy_or_relump(sourcerpg, datadir & SLASH & basename & ".rpg") = NO THEN EXIT DO

  debuginfo "Copy linux game player" 
  DIM gameplayer as string
  gameplayer = get_linux_gameplayer()
  IF gameplayer = "" THEN visible_debug "ERROR: ohrrpgce-game is not available" : EXIT DO
  IF copy_linux_gameplayer(gameplayer, basename, bindir) = NO THEN EXIT DO
  
  debuginfo "Create menu file"
  DIM menudir as string = debtmp & SLASH & "usr" & SLASH & "share" & SLASH & "menu"
  MKDIR menudir
  write_linux_menu_file distinfo.gamename, menudir & SLASH & basename, basename

  debuginfo "Create desktop file"
  DIM applicationsdir as string = debtmp & SLASH & "usr" & SLASH & "share" & SLASH & "applications"
  MKDIR applicationsdir
  write_linux_desktop_file distinfo.gamename, applicationsdir & SLASH & basename & ".desktop", basename

  debuginfo "Create docs"
  DIM docsdir as string = debtmp & SLASH & "usr" & SLASH & "share" & SLASH & "doc"
  MKDIR docsdir
  DIM gamedocsdir as string = docsdir & SLASH & basename
  MKDIR gamedocsdir
  write_readme_text_file gamedocsdir & SLASH & "README.txt", CHR(10)
  gzip_file gamedocsdir & SLASH & "README.txt"
  write_debian_copyright_file gamedocsdir & SLASH & "copyright"

  IF distinfo.license <> "GPL" THEN
   '--only write non-GPL license files because Debian policy prefers referencing the local copy
   DIM lic_file as string = gamedocsdir & SLASH & "LICENSE-" & distinfo.license & ".txt"
   maybe_write_license_text_file lic_file
   IF isfile(lic_file) THEN
    gzip_file lic_file
   END IF
  END IF

  debuginfo "Calculate Installed-Size"
  DIM size_in_kibibytes as integer = count_directory_size(debtmp & SLASH & "usr") / 1024
 
  debuginfo "Create debian-binary version file"
  write_debian_binary_file debtmp & SLASH & "debian-binary"

  debuginfo "Create debian control file"
  write_debian_control_file debtmp & SLASH & "control", basename, pkgver, size_in_kibibytes, distinfo
  IF NOT isfile(debtmp & SLASH & "control") THEN visible_debug "Couldn't create debian control file" : EXIT DO
  write_debian_postinst_script debtmp & SLASH & "postinst"
  write_debian_postrm_script debtmp & SLASH & "postrm"

  fix_deb_group_permissions debtmp & SLASH & "control"
  fix_deb_group_permissions debtmp & SLASH & "postinst"
  fix_deb_group_permissions debtmp & SLASH & "postrm"

  IF create_tarball(debtmp, debtmp & SLASH & "control.tar.gz", "control postinst postrm") = NO THEN EXIT DO

  fix_deb_group_permissions debtmp & SLASH & "usr"
  
  IF create_tarball(debtmp, debtmp & SLASH & "data.tar.gz", "usr") = NO THEN EXIT DO

  IF create_ar_archive(debtmp, debname, "debian-binary control.tar.gz data.tar.gz") = NO THEN EXIT DO
  
  visible_debug trimpath(debname) & " was successfully created!"
  EXIT DO 'this loop is only ever one pass
 LOOP

 '--Cleanup temp files
 killdir debtmp, YES
 
END SUB

SUB write_debian_postinst_script (filename as string)
 DIM LF as string = CHR(10)
 DIM fh as integer = FREEFILE
 OPEN filename for output as #fh
 PUT #fh, , "#!/bin/sh" & LF
 PUT #fh, , "set -e" & LF
 PUT #fh, , "if [ ""$1"" = ""configure"" ] && [ -x ""`which update-menus 2>/dev/null`"" ]; then" & LF
 PUT #fh, , "    update-menus" & LF
 PUT #fh, , "fi" & LF
 CLOSE #fh
 #IFDEF __UNIX__
 DIM cmd as string
 cmd = "chmod +x " & filename
 debuginfo cmd
 SHELL cmd
 #ENDIF
END SUB

SUB write_debian_postrm_script (filename as string)
 DIM LF as string = CHR(10)
 DIM fh as integer = FREEFILE
 OPEN filename for output as #fh
 PUT #fh, , "#!/bin/sh" & LF
 PUT #fh, , "set -e" & LF
 PUT #fh, , "if [ -x ""`which update-menus 2>/dev/null`"" ]; then" & LF
 PUT #fh, , "    update-menus" & LF
 PUT #fh, , "fi" & LF
 CLOSE #fh
 #IFDEF __UNIX__
 DIM cmd as string
 cmd = "chmod +x " & filename
 debuginfo cmd
 SHELL cmd
 #ENDIF
END SUB

SUB fix_deb_group_permissions(start_at_dir as string)
#IFDEF __UNIX__
 'This is needed because the user's umask might have given group write access to the files
 DIM cmd as string
 cmd = "chmod -R g-w " & start_at_dir
 debuginfo cmd
 SHELL cmd
#ENDIF
END SUB

SUB write_linux_menu_file(title as string, filename as string, basename as string)
 DIM fh as integer = FREEFILE
 OPEN filename for output as #fh
 PUT #fh, , "?package(" & basename & "): needs=""X11"" title=""" & title & """ command=""/usr/games/" & basename & """ section=""Games/Adventure""" & CHR(10)
 CLOSE #fh
END SUB

SUB write_linux_desktop_file(title as string, filename as string, basename as string)
 DIM LF as string = CHR(10)
 DIM fh as integer = FREEFILE
 OPEN filename for output as #fh
 PUT #fh, , "[Desktop Entry]" & LF
 PUT #fh, , "Name=" & title & LF
 PUT #fh, , "Exec=/usr/games/" & basename & LF
 PUT #fh, , "Terminal=false" & LF
 PUT #fh, , "Type=Application" & LF
 PUT #fh, , "Categories=Application;Game;" & LF
 CLOSE #fh
END SUB

FUNCTION create_ar_archive(start_in_dir as string, archive as string, files as string) as integer
 '--Returns YES if successful, or NO if failed

 ' start_in_dir only applies to the ar command. The archive filename should still be either absolute or relative to the default CURDIR

 'files is a list of space-separated filenames and directory names to include in the tarball
 'if they contain spaces they must be quoted

 DIM ar as string = find_helper_app("ar", YES)
 IF ar = "" THEN visible_debug "ERROR: ar is not available" : RETURN NO

 DIM args as string
 args = " qc"
 #IFNDEF __FB_DARWIN__
 'Non-mac platforms can use the D arg for deterministic mode
 args &= "D"
 #ENDIF
 args &= " """ & archive & """ " & files
 'debug ar & " " & args
 DIM spawn_ret as string
 
 DIM olddir as string = CURDIR
 CHDIR start_in_dir
 spawn_ret = spawn_and_wait(ar, args)
 CHDIR olddir
 
 IF LEN(spawn_ret) THEN visible_debug spawn_ret : RETURN NO
 IF NOT isfile(archive) THEN visible_debug "Could not create " & archive : RETURN NO
 RETURN YES
 
END FUNCTION

FUNCTION create_tarball(start_in_dir as string, tarball as string, files as string) as integer
 '--Returns YES if successful, or NO if failed

 ' start_in_dir only applies to the tar command. The tarball filename should still be either absolute or relative to the default CURDIR

 'files is a list of space-separated filenames and directory names to include in the tarball
 'if they contain spaces they must be quoted
 
 DIM tar as string = find_helper_app("tar", YES)
 IF tar = "" THEN visible_debug "ERROR: tar is not available": RETURN NO

 DIM gzip as string = find_helper_app("gzip", YES)
 IF gzip = "" THEN visible_debug "ERROR: gzip is not available": RETURN NO

 DIM uncompressed as string = trimextension(tarball)

 DIM more_args as string = ""
 #IFDEF __UNIX__
 #IFNDEF __FB_DARWIN__
 'These arguments are broken on Windows tar.exe for some stupid reason
 more_args = " --owner=root --group=root"
 #ENDIF
 #ENDIF
 #IFDEF __FB_WIN32__
 'This is a hack to replace tar.exe's horrendous default permissions, and to (clumsily) mark the executables with the executable bit
  more_args = " --mode=755"
 #ENDIF
 
 DIM spawn_ret as string
 DIM args as string

 args = " -c " & more_args & " -f """ & uncompressed & """ " & files
 'debug tar & " " & args
 
 DIM olddir as string = CURDIR
 CHDIR start_in_dir
 spawn_ret = spawn_and_wait(tar, args)
 CHDIR olddir
 
 IF LEN(spawn_ret) THEN visible_debug spawn_ret : RETURN NO

 IF gzip_file(uncompressed) = NO THEN RETURN NO
 
 IF NOT isfile(tarball) THEN visible_debug "Could not create " & tarball : RETURN NO
 RETURN YES
END FUNCTION

FUNCTION extract_tarball(into_dir as string, tarball as string, files as string) as integer
 '--Returns YES if successful, or NO if failed
 
 'The tarball must already be decompressed. Don't pass in a .tar.gz (this is inconsistent
 'with create_tarball, I know. )

 ' into_dir only applies to the tar command. The tarball filename should still be either absolute or relative to the default CURDIR

 'files is a list of space-separated filenames and directory names to include in the tarball
 'if they contain spaces they must be quoted
 
 
 DIM tar as string = find_helper_app("tar", YES)
 IF tar = "" THEN visible_debug "ERROR: tar is not available": RETURN NO

 DIM spawn_ret as string
 DIM args as string

 args = " -x -f """ & tarball & """ " & files
 'debug tar & " " & args
 
 DIM olddir as string = CURDIR
 CHDIR into_dir
 spawn_ret = spawn_and_wait(tar, args)
 CHDIR olddir
 
 IF LEN(spawn_ret) THEN visible_debug spawn_ret : RETURN NO
 
 RETURN YES

END FUNCTION

FUNCTION gzip_file (filename as string) as integer
 'Returns YES on success, NO on failure
 DIM gzip as string = find_helper_app("gzip", YES)
 IF gzip = "" THEN visible_debug "ERROR: gzip is not available": RETURN NO
 
 DIM args as string
 args = """" & filename & """"
 DIM spawn_ret as string
 spawn_ret = spawn_and_wait(gzip, args)
 IF LEN(spawn_ret) THEN visible_debug spawn_ret : RETURN NO
 IF NOT isfile(filename & ".gz") THEN
  visible_debug "ERROR: gzip completed but " & filename & ".gz was not created"
 END IF

 RETURN YES
END FUNCTION

FUNCTION gunzip_file (filename as string) as integer
 'Returns YES on success, NO on failure
 DIM gzip as string = find_helper_app("gzip", YES)
 IF gzip = "" THEN visible_debug "ERROR: gzip is not available": RETURN NO
 
 DIM args as string
 args = " -d -f """ & filename & """"
 DIM spawn_ret as string
 spawn_ret = spawn_and_wait(gzip, args)
 IF LEN(spawn_ret) THEN visible_debug spawn_ret : RETURN NO
 IF NOT isfile(trimextension(filename)) THEN
  visible_debug "ERROR: gzip -d completed but " & filename & ".gz was not uncompressed"
 END IF

 RETURN YES
END FUNCTION

SUB write_debian_binary_file (filename as string)
 DIM fh as integer = FREEFILE
 OPEN filename for binary as #fh
 PUT #fh, ,"2.0" & CHR(10)
 CLOSE #fh
END SUB

SUB write_debian_control_file(controlfile as string, basename as string, pkgver as string, size_in_kibibytes as integer, byref distinfo as DistribState)
 DIM LF as string = CHR(10)

 DIM author as string = distinfo.author
 IF author = "" THEN author = "Anonymous"
 DIM email as string = distinfo.email
 IF email = "" THEN email = "anonymous_author@no.email.specified"
 DIM website as string = distinfo.website
 IF LEN(website) > 0 THEN
  IF NOT starts_with(website, "http://") ANDALSO NOT starts_with(website, "https://") THEN
   website = "http://" & website
  END IF
 END IF

 DIM fh as integer = FREEFILE
 OPEN controlfile for output as #fh
 PUT #fh, , "Package: " & basename & LF
 PUT #fh, , "Priority: optional" & LF 
 PUT #fh, , "Section: games" & LF
 PUT #fh, , "Maintainer: """ & author & """ <" & email & ">" & LF
 PUT #fh, , "Architecture: i386" & LF
 PUT #fh, , "Version: " & pkgver & LF
 PUT #fh, , "Installed-Size: " & size_in_kibibytes & LF
 'FIXME: the Depends: line could vary depending on gfx and music backends
 'FIXME: Is there an easy way to verify which is the minimum libc version to depend upon?
 PUT #fh, , "Depends: libc6 (>= 2.3), libncurses5 (>= 5.4), libsdl-mixer1.2 (>= 1.2), libsdl1.2debian (>> 1.2), libx11-6, libxext6, libxpm4, libxrandr2, libxrender1" & LF
 IF LEN(website) > 0 THEN
  PUT #fh, , "Homepage: " & website & LF
 END IF
 PUT #fh, , "Description: " & special_char_sanitize(distinfo.gamename) & LF
 IF LEN(TRIM(distinfo.description)) > 0 THEN
  PUT #fh, , " ." & LF
  PUT #fh, , " " & TRIM(exclude(distinfo.description, LF)) & LF
 END IF
 CLOSE #fh
END SUB

FUNCTION get_debian_package_name() as string
 DIM distinfo as DistribState
 load_distrib_state distinfo
 DIM s as string
 s = distinfo.pkgname
 s = LCASE(s)
 s = exclude(s, "'")
 DIM result as string = ""
 DIM ch as string
 DIM dash as integer = NO
 FOR i as integer = 1 TO LEN(s)
   ch = MID(s, i, 1)
   IF ch >= "a" ANDALSO ch <= "z" THEN
    result &= ch
    dash = NO
   ELSE
    IF NOT dash ANDALSO LEN(result) > 0 ANDALSO i <> LEN(s) THEN
     result &= "-"
     dash = YES
    END IF
   END IF
 NEXT i
 RETURN result
END FUNCTION

FUNCTION get_debian_package_version() as string
 DIM d as string = DATE
 RETURN MID(d, 7, 4) & "." & MID(d, 1, 2) & "." & MID(d, 4, 2)
 '--if we wanted the hour we would add this too
 '& "." & MID(TIME, 1, 2)
END FUNCTION

FUNCTION can_run_windows_exes () as integer
#IFDEF __FB_WIN32__
 '--Of course we can always run exe files on Windows
 RETURN YES
#ENDIF
'--Unixen and Macs can only run exe files with wine
IF find_helper_app("wine") = "" THEN RETURN NO
IF NOT isdir(environ("HOME") & "/.wine/dosdevices/c:") THEN RETURN NO
RETURN YES
END FUNCTION

FUNCTION can_make_debian_packages () as integer
'--check to see if we can find the tools needed to create a .deb package
IF find_helper_app("ar") = "" THEN RETURN NO
IF find_helper_app("tar") = "" THEN RETURN NO
IF find_helper_app("gzip") = "" THEN RETURN NO
RETURN YES
END FUNCTION

FUNCTION can_make_mac_packages () as integer
'--check to see if we can find the tools needed to compress a mac .app package
IF find_helper_app("tar") = "" THEN RETURN NO
IF find_helper_app("gzip") = "" THEN RETURN NO
RETURN YES
END FUNCTION

SUB distribute_game_as_mac_app ()

 DIM distinfo as DistribState
 load_distrib_state distinfo

 DIM destname as string = trimfilename(sourcerpg) & SLASH & distinfo.pkgname & "-mac.tar.gz"

 IF isfile(destname) THEN
  IF yesno(trimpath(destname) & " already exists. Overwrite it?") = NO THEN RETURN
  safekill destname
 END IF

 DIM apptmp as string = trimfilename(sourcerpg) & SLASH & "macapp.tmp"
 IF isdir(apptmp) THEN
  debuginfo "Clean up old " & apptmp
  killdir apptmp, YES
 END IF
 
 makedir apptmp

 DO '--single pass loop for breaking

  debuginfo "Rename mac game player" 
  DIM gameplayer as string
  gameplayer = get_mac_gameplayer()
  IF gameplayer = "" THEN visible_debug "ERROR: OHRRPGCE-Game.app is not available" : EXIT DO
  DIM app as string = apptmp & SLASH & distinfo.pkgname & ".app"
#IFDEF __FB_WIN32__
  IF confirmed_copydirectory(gameplayer, app) = NO THEN visible_debug "Couldn't copy " & gameplayer & " to " & app : EXIT DO
#ELSE
  'Mac and Linux do it this way to preserve symlinks and permissions
  IF os_shell_move(gameplayer, app) = NO THEN visible_debug "Couldn't move " & gameplayer & " to " & app : EXIT DO
#ENDIF
  IF confirmed_copy(trimfilename(gameplayer) & SLASH & "LICENSE-binary.txt", apptmp & SLASH & "LICENSE-binary.txt") = NO THEN EXIT DO

  debuginfo "Copy rpg file"
  DIM gameshortname as string
  gameshortname = trimextension(trimpath(sourcerpg))
  DIM resources as string
  resources = app & SLASH & "Contents" & SLASH & "Resources"
  IF copy_or_relump(sourcerpg, resources & SLASH & gameshortname & ".rpg") = NO THEN EXIT DO
  
  debuginfo "Create bundledgame file"
  DIM fh as integer = FREEFILE
  OPEN resources & SLASH & "bundledgame" FOR OUTPUT AS #fh
  PRINT #fh, gameshortname
  CLOSE #fh

  write_readme_text_file apptmp & SLASH & "README-" & distinfo.pkgname & ".txt", CHR(10)

  maybe_write_license_text_file apptmp & SLASH & "LICENSE.txt"

  DIM olddir as string = CURDIR
  CHDIR apptmp
  IF create_tarball(apptmp, destname, "*.app *.txt") = NO THEN
   CHDIR olddir
   EXIT DO
  END IF
  CHDIR olddir
  
  visible_debug trimpath(destname) & " was successfully created!"
  EXIT DO 'this loop is only ever one pass
 LOOP

 '--Cleanup temp files
 killdir apptmp, YES

END SUB

FUNCTION get_mac_gameplayer() as string
 'Download OHRRPGCE-Game.app,
 'unzip it, and return the full path.
 'Returns "" for failure.

 '--Find the folder that we are going to download OHRRPGCE-Game.app into
 DIM dldir as string = settings_dir & SLASH & "_gameplayer"
 IF NOT isdir(dldir) THEN makedir dldir
 IF NOT isdir(dldir) THEN visible_debug "ERROR: Unable to create """ & dldir & """ directory": RETURN ""
  
 '--Decide which url to download
 DIM url as string
 DIM dlfile as string

#IFDEF __FB_WIN32__
 'Windows tar cannot preserve symlinks so we need to use the bloated linkless tarball
 dlfile = "ohrrpgce-mac-minimal-linkless.tar.gz"
#ELSE
 dlfile = "ohrrpgce-mac-minimal.tar.gz"
#ENDIF

 IF version_branch = "wip" THEN
  'If using any wip release, get the latest wip release
  url = "http://hamsterrepublic.com/ohrrpgce/nightly/" & dlfile
 ELSE
  'If using any stable release, get the latest stable release
  url = "http://hamsterrepublic.com/dl/" & dlfile
 END IF

 '--Ask the user for permission the first time we download (subsequent updates don't ask)
 DIM destgz as string = dldir & SLASH & dlfile
 DIM desttar as string = trimextension(destgz)
 IF NOT isfile(destgz) ANDALSO NOT isfile(desttar) THEN
  IF yesno("Is it okay to download the Mac OS X version of OHRRPGCE from HamsterRepublic.com now?") = NO THEN RETURN ""
 END IF

 '--Actually download the dang file
 download_file url, dldir

 '--remove the old uncompressed files
 safekill dldir & SLASH & "LICENSE-binary.txt"
 killdir dldir & SLASH & "OHRRPGCE-Game.app", YES
 
 '--Untar the desired files
 IF gunzip_file(destgz) = NO THEN RETURN ""
 IF extract_tarball(dldir, desttar, "OHRRPGCE-Game.app LICENSE-binary.txt") = NO THEN RETURN ""
 
 IF NOT isdir(dldir & SLASH & "OHRRPGCE-Game.app")   THEN visible_debug "ERROR: Failed to untar OHRRPGCE-Game.app" : RETURN ""
 IF NOT isfile(dldir & SLASH & "OHRRPGCE-Game.app" & SLASH & "Contents" & SLASH & "MacOS" & SLASH & "ohrrpgce-game")   THEN visible_debug "ERROR: Failed to completely untar OHRRPGCE-Game.app" : RETURN ""
 IF NOT isfile(dldir & SLASH & "LICENSE-binary.txt") THEN visible_debug "ERROR: Failed to untar LICENSE-binary.txt" : RETURN ""
 
 RETURN dldir & SLASH & "OHRRPGCE-Game.app"

END FUNCTION
