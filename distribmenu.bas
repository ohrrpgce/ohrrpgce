'OHRRPGCE CUSTOM - Distribute Game menu
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#include "config.bi"
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
#include "vbcompat.bi"  'DATESERIAL, NOW

DECLARE SUB distribute_game_as_zip (dest_override as string = "")
DECLARE SUB distribute_game_as_windows_installer (dest_override as string = "")
DECLARE SUB distribute_game_as_linux_tarball (which_arch as string, dest_override as string = "")
DECLARE FUNCTION get_windows_gameplayer() as string
DECLARE FUNCTION get_linux_gameplayer(which_arch as string) as string
DECLARE FUNCTION get_mac_gameplayer(which_arch as string) as string
DECLARE FUNCTION agreed_to_download(agree_file as string, description as string) as bool
DECLARE FUNCTION download_gameplayer_if_needed(url as string, destfile as string, agree_filename as string, description as string) as bool
DECLARE FUNCTION sanity_check_buildinfo(buildinfo_file as string) as bool
DECLARE FUNCTION find_or_download_innosetup () as string
DECLARE FUNCTION find_innosetup () as string
DECLARE FUNCTION win_or_wine_drive(letter as string) as string
DECLARE FUNCTION win_or_wine_spawn_and_wait (cmd as string, args as string="") as string
DECLARE SUB write_innosetup_script (basename as string, gamename as string, isstmp as string)
DECLARE SUB add_innosetup_file (s as string, filename as string)
DECLARE FUNCTION win_path (filename as string) as string
DECLARE FUNCTION copy_or_relump (src_rpg_or_rpgdir as string, dest_rpg as string) as bool
DECLARE FUNCTION copy_windows_gameplayer (gameplayer as string, basename as string, destdir as string) as bool
DECLARE SUB insert_windows_exe_icon (exe_name as string, ico_name as string)
DECLARE SUB find_required_dlls(gameplayer as string, byref files as string vector)
DECLARE FUNCTION copy_linux_gameplayer (gameplayer as string, basename as string, destdir as string) as bool
DECLARE SUB distribute_game_as_debian_package (which_arch as string, dest_override as string = "")
DECLARE FUNCTION get_debian_package_version() as string
DECLARE FUNCTION get_debian_package_name() as string
DECLARE SUB write_linux_menu_file(title as string, filename as string, basename as string)
DECLARE SUB write_linux_desktop_file(title as string, filename as string, basename as string)
DECLARE SUB write_debian_binary_file (filename as string)
DECLARE SUB write_debian_control_file(controlfile as string, basename as string, pkgver as string, size_in_kibibytes as integer, byref distinfo as DistribState, deb_arch as string)
DECLARE SUB write_debian_copyright_file (filename as string)
DECLARE FUNCTION gzip_file (filename as string) as bool
DECLARE FUNCTION gunzip_file (filename as string) as bool
DECLARE FUNCTION create_zipfile(start_in_dir as string, zipfile as string, files as string) as bool
DECLARE FUNCTION create_tarball(start_in_dir as string, tarball as string, files as string) as bool
DECLARE FUNCTION extract_tarball(into_dir as string, tarball as string, files as string) as bool
DECLARE FUNCTION create_ar_archive(start_in_dir as string, archive as string, files as string) as bool
DECLARE SUB fix_deb_group_permissions(start_at_dir as string)
DECLARE SUB write_debian_postrm_script (filename as string)
DECLARE SUB write_debian_postinst_script (filename as string)
DECLARE FUNCTION can_make_tarballs () as bool
DECLARE FUNCTION can_run_windows_exes () as bool
DECLARE FUNCTION can_make_debian_packages () as bool
DECLARE FUNCTION can_make_mac_packages () as bool
DECLARE SUB edit_distrib_info ()
DECLARE SUB sanitize_distinfo OVERLOAD ()
DECLARE SUB sanitize_distinfo OVERLOAD (distinfo as DistribState)
DECLARE FUNCTION sanitize_pkgname(s as string) as string
DECLARE FUNCTION sanitize_email(s as string) as string
DECLARE FUNCTION sanitize_url(s as string) as string
DECLARE FUNCTION sanitize_url_chunk(byval s as string) as string
DECLARE SUB export_readme_text_file (LE as string=LINE_END, byval wrap as integer=72)
DECLARE SUB write_readme_text_file (filename as string, LE as string=LINE_END, byval wrap as integer=72)
DECLARE SUB maybe_write_license_text_file (filename as string)
DECLARE FUNCTION is_known_license(license_code as string) as bool
DECLARE FUNCTION generate_copyright_line(distinfo as DistribState) as string
DECLARE FUNCTION browse_licenses(old_license as string) as string
DECLARE SUB distribute_game_as_mac_app (which_arch as string, dest_override as string = "")
DECLARE FUNCTION fix_mac_app_executable_bit_on_windows(zipfile as string, exec_path_in_zip as string) as bool
DECLARE FUNCTION running_64bit() as bool
DECLARE SUB dist_basicstatus (s as string)
DECLARE SUB itch_io_options_menu()
DECLARE FUNCTION itch_game_url(distinfo as DistribState) as string
DECLARE FUNCTION itch_target(distinfo as DistribState) as string
DECLARE FUNCTION itch_gametarg(distinfo as DistribState) as string
DECLARE FUNCTION itch_butler_path() as string
DECLARE FUNCTION itch_butler_is_installed() as bool
DECLARE FUNCTION itch_butler_is_logged_in() as bool
DECLARE FUNCTION itch_butler_setup() as bool
DECLARE FUNCTION itch_butler_download() as bool
DECLARE SUB itch_butler_upload(distinfo as DistribState)
DECLARE FUNCTION itch_butler_error_check(out_s as string, err_s as string) as bool

CONST distmenuEXIT as integer = 1
CONST distmenuZIP as integer = 2
CONST distmenuWINSETUP as integer = 3
CONST distmenuMACSETUP as integer = 4
CONST distmenuDEBSETUP as integer = 5
CONST distmenuDEB64SETUP as integer = 6
CONST distmenuINFO as integer = 7
CONST distmenuREADME as integer = 8
CONST distmenuLINUXSETUP as integer = 9
CONST distmenuLINUX64SETUP as integer = 10
CONST distmenuMAC64SETUP as integer = 11
CONST distmenuITCHUPLOAD as integer = 12

DECLARE FUNCTION dist_yesno(capt as string, byval defaultval as bool=YES, byval escval as bool=NO) as bool
DECLARE SUB dist_info (msg as zstring ptr, errlvl as errorLevelEnum = errDebug)
DIM SHARED auto_choose_default as bool = NO

SUB distribute_game ()
 
 DIM menu as SimpleMenuItem vector
 v_new menu, 0
 append_simplemenu_item menu, "Previous Menu...", , , distmenuEXIT
 append_simplemenu_item menu, " Game file: " & decode_filename(trimpath(sourcerpg)), YES, uilook(uiDisabledItem)
 DIM as string temp, iconbase
 iconbase = trimextension(sourcerpg)
 IF isfile(iconbase & ".ico") THEN temp = " found" ELSE temp = " not found"
 append_simplemenu_item menu, " Windows icon: " & decode_filename(trimpath(iconbase)) & ".ico" & temp, YES, uilook(uiDisabledItem)
 IF isfile(iconbase & ".icns") THEN temp = " found" ELSE temp = " not found"
 append_simplemenu_item menu, " Mac icon: " & decode_filename(trimpath(iconbase)) & ".icns" & temp, YES, uilook(uiDisabledItem)

 append_simplemenu_item menu, "Edit distribution info...", , , distmenuINFO

 #IFNDEF __FB_ANDROID__

 append_simplemenu_item menu, "Export Windows .ZIP", , , distmenuZIP

 IF can_run_windows_exes() THEN
  append_simplemenu_item menu, "Export Windows Installer", , , distmenuWINSETUP
 ELSE
  append_simplemenu_item menu, "Can't Export Windows Installer", YES, uilook(uiDisabledItem)
  append_simplemenu_item menu, " (requires Windows or wine)", YES, uilook(uiDisabledItem)
 END IF

 append_simplemenu_item menu, "Export Mac OS X App Bundle (64bit)", , , distmenuMAC64SETUP
 IF NOT can_make_mac_packages() THEN
  append_simplemenu_item menu, " (requires tar+gzip)", YES, uilook(uiDisabledItem)
 END IF

 append_simplemenu_item menu, "Export Mac OS X App Bundle (old 32bit Macs)", , , distmenuMACSETUP
 IF NOT can_make_mac_packages() THEN
  append_simplemenu_item menu, " (requires tar+gzip)", YES, uilook(uiDisabledItem)
 END IF

 append_simplemenu_item menu, "Export Linux Tarball (64bit)", , , distmenuLINUX64SETUP
 IF NOT can_make_tarballs() THEN
  append_simplemenu_item menu, " (requires tar+gzip)", YES, uilook(uiDisabledItem)
 END IF

 append_simplemenu_item menu, "Export Linux Tarball (32bit)", , , distmenuLINUXSETUP
 IF NOT can_make_tarballs() THEN
  append_simplemenu_item menu, " (requires tar+gzip)", YES, uilook(uiDisabledItem)
 END IF

 append_simplemenu_item menu, "Export Debian Linux Package (64bit)", , , distmenuDEB64SETUP
 IF NOT can_make_debian_packages() THEN
  append_simplemenu_item menu, " (requires ar+tar+gzip)", YES, uilook(uiDisabledItem)
 END IF

 append_simplemenu_item menu, "Export Debian Linux Package (32bit)", , , distmenuDEBSETUP
 IF NOT can_make_debian_packages() THEN
  append_simplemenu_item menu, " (requires ar+tar+gzip)", YES, uilook(uiDisabledItem)
 END IF

 append_simplemenu_item menu, "Upload this game to itch.io ...", , , distmenuITCHUPLOAD

 #ENDIF

 append_simplemenu_item menu, "Export README text file", , , distmenuREADME

 DIM st as MenuState
 init_menu_state st, cast(BasicMenuItem vector, menu)

 DO
  setwait 55
  setkeys

  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "distribute_game"
  IF enter_space_click(st) THEN
   SELECT CASE menu[st.pt].dat
    CASE distmenuEXIT: EXIT DO
    CASE distmenuZIP:
     save_current_game 0
     distribute_game_as_zip
    CASE distmenuWINSETUP:
     save_current_game 0
     distribute_game_as_windows_installer
    CASE distmenuMAC64SETUP:
     save_current_game 0
     distribute_game_as_mac_app "x86_64"
    CASE distmenuMACSETUP:
     save_current_game 0
     distribute_game_as_mac_app "x86"
    CASE distmenuDEB64SETUP:
     save_current_game 0
     distribute_game_as_debian_package "x86_64"
    CASE distmenuDEBSETUP:
     save_current_game 0
     distribute_game_as_debian_package "x86"
    CASE distmenuLINUX64SETUP:
     save_current_game 0
     distribute_game_as_linux_tarball "x86_64"
    CASE distmenuLINUXSETUP:
     save_current_game 0
     distribute_game_as_linux_tarball "x86"
    CASE distmenuINFO:
     edit_distrib_info
    CASE distmenuREADME:
     export_readme_text_file
    CASE distmenuITCHUPLOAD:
     itch_io_options_menu
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
 'Revert genCurrentDebugMode to the author's choice in genDebugMode
 gen(genCurrentDebugMode) = gen(genDebugMode)
END SUB

SUB edit_distrib_info ()

 DIM rootsl as Slice Ptr
 rootsl = NewSliceOfType(slContainer)
 rootsl->Fill = YES
 DIM infosl as Slice Ptr
 infosl = NewSliceOfType(slText, rootsl)
 infosl->Width = 320
 infosl->AnchorVert = alignBottom
 infosl->AlignVert = alignBottom
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

 DIM st as MenuState
 init_menu_state st, cast(BasicMenuItem vector, menu)
 st.need_update = YES

 DO
  setwait 55
  setkeys YES

  IF keyval(ccCancel) > 1 THEN EXIT DO
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
  IF enter_space_click(st) THEN
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
   sanitize_distinfo distinfo
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
    ChangeTextSlice infosl, "Press F1 for details"
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

SUB sanitize_distinfo(distinfo as DistribState)
 distinfo.pkgname = sanitize_pkgname(distinfo.pkgname)
 distinfo.gamename = special_char_sanitize(exclude(distinfo.gamename, "/\""" & CHR(10)))
 distinfo.author = special_char_sanitize(exclude(distinfo.author, "<>@""" & CHR(10)))
 distinfo.email = sanitize_email(distinfo.email)
 distinfo.website = sanitize_url(distinfo.website)
 distinfo.copyright_year = exclusive(distinfo.copyright_year, "0123456789 -,")
END SUB

SUB sanitize_distinfo()
 DIM distinfo as DistribState
 load_distrib_state distinfo
 sanitize_distinfo distinfo
 save_distrib_state distinfo
END SUB

FUNCTION sanitize_pkgname(s as string) as string
 RETURN LCASE(special_char_sanitize(exclude(s, "<>?./\ ""'" + CHR(10))))
END FUNCTION

FUNCTION sanitize_email(s as string) as string
 '--This e-mail address sanitization is far from perfect, but good enough for most cases
 RETURN special_char_sanitize(exclusive(s, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_@.+"))
END FUNCTION

FUNCTION sanitize_url(s as string) as string
 '--This website address sanitization is far from perfect, but probably good enough for most cases
 RETURN special_char_sanitize(exclusive(s, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-.:/_+%:;?@=&'"))
END FUNCTION

FUNCTION sanitize_url_chunk(byval s as string) as string
 replacestr s, " ", "-"
 replacestr s, "_", "-"
 RETURN special_char_sanitize(exclusive(LCASE(s), "abcdefghijklmnopqrstuvwxyz0123456789-"))
END FUNCTION

SUB export_readme_text_file (LE as string=LINE_END, byval wrap as integer=72)

 DIM distinfo as DistribState
 load_distrib_state distinfo

 DIM txtfile as string = trimfilename(sourcerpg) & SLASH & "README-" & distinfo.pkgname & ".txt"
 
 DIM shortname as string = decode_filename(trimpath(txtfile))
 IF isfile(txtfile) THEN
  IF dist_yesno(shortname & " already exists, are you sure you want to overwrite it?", NO) = NO THEN RETURN
 END IF
 write_readme_text_file txtfile, LE
 IF isfile(txtfile) THEN dist_info "Created " & shortname, errInfo
 
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
 OPEN filename for binary access write as #fh
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
 
 writeablecopyfile lic_file, filename
 
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

FUNCTION is_known_license(license_code as string) as bool
 'duplicated known_licenses because global string arrays are a pain in the ass
 DIM known_licenses(9) as string = {"COPYRIGHT", "PUBLICDOMAIN", "GPL", "MIT", "CC-BY", "CC-BY-SA", "CC-BY-ND", "CC-BY-NC", "CC-BY-NC-SA", "CC-BY-NC-ND"}
 FOR i as integer = 0 TO UBOUND(known_licenses)
  IF license_code = known_licenses(i) THEN RETURN YES
 NEXT i
 RETURN NO
END FUNCTION

FUNCTION generate_copyright_line(distinfo as DistribState) as string
 DIM c_y_by as string = RTRIM("(C) Copyright " & distinfo.copyright_year & " " & distinfo.author)
 SELECT CASE distinfo.license
  CASE "COPYRIGHT":
   RETURN c_y_by
  CASE "PUBLICDOMAIN":
   RETURN "Copyright released by " & distinfo.author & " into the Public Domain."
  CASE "GPL":
   RETURN c_y_by & ". This game is distributed under the terms of the GNU General Public License."
  CASE "MIT":
   RETURN c_y_by & ". This game is distributed under the terms of the MIT license."
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
   showerror distinfo.license & " is not in the list of licenses that this program understands"
   RETURN ""
 END SELECT
END FUNCTION

SUB distribute_game_as_zip (dest_override as string = "")
 debuginfo "  distribute_game_as_zip():"

 DIM distinfo as DistribState
 load_distrib_state distinfo

 DIM zip as string = find_helper_app("zip", YES)
 IF zip = "" THEN
  dist_info "Can't create zip files: " & missing_helper_message("zip" DOTEXE)
  RETURN
 END IF

 DIM destzip as string = trimfilename(sourcerpg)
 IF dest_override <> "" THEN destzip = dest_override
 destzip &= SLASH & distinfo.pkgname & ".zip"
 DIM shortzip as string = decode_filename(trimpath(destzip))
 IF isfile(destzip) THEN
  IF dist_yesno(shortzip & " already exists. Overwrite it?") = NO THEN RETURN
  'Okay to overwrite, but do the overwrite later
 END IF

 DIM ziptmp as string = trimfilename(sourcerpg) & SLASH & "zip.tmp"
 IF isdir(ziptmp) THEN
  killdir ziptmp
 END IF

 DIM use_gameplayer as bool = YES
 DIM gameplayer as string
 gameplayer = get_windows_gameplayer()
 IF gameplayer = "" THEN
  IF dist_yesno("game.exe is not available, continue anyway?", NO) = NO THEN RETURN
  use_gameplayer = NO
 END IF

 makedir ziptmp
 IF NOT isdir(ziptmp) THEN
  dist_info "ERROR: unable to create temporary folder"
  RETURN
 END IF

 DIM spawn_ret as string

 DO 'Single-pass loop for operations after ziptmp exists
  
  DIM basename as string = distinfo.pkgname
  
  IF copy_or_relump(sourcerpg, ziptmp & SLASH & basename & ".rpg") = NO THEN EXIT DO

  IF use_gameplayer THEN
   IF copy_windows_gameplayer(gameplayer, basename, ziptmp) = NO THEN EXIT DO
   insert_windows_exe_icon ziptmp & SLASH & basename & ".exe", trimextension(sourcerpg) & ".ico"
  END IF
 
  'Write readme with DOS/Window line endings
  write_readme_text_file ziptmp & SLASH & "README-" & basename & ".txt", CHR(13) & CHR(10)
  maybe_write_license_text_file ziptmp & SLASH & "LICENSE.txt"

  'Remove the old zip
  safekill destzip
 
  'Create the new zip
  DIM args as string = "-r -j " & escape_filename(destzip) & " " & escape_filename(ziptmp)
  spawn_ret = spawn_and_wait(zip, args)
  IF LEN(spawn_ret) ORELSE NOT isfile(destzip) THEN
   safekill destzip
   dist_info "Zip file creation failed. " & spawn_ret
   RETURN
  END IF
  
  dist_info "Successfully created " & shortzip, errInfo

  EXIT DO 'single pass, never really loops.
 LOOP
 'Cleanup ziptmp
 killdir ziptmp
 
END SUB

SUB dist_basicstatus (s as string)
 basic_textbox s, uilook(uiText), vpage
 setvispage vpage, NO
END SUB

FUNCTION copy_or_relump (src_rpg_or_rpgdir as string, dest_rpg as string) as bool
 'Return true on success, false on fail

 DIM extension as string = LCASE(justextension(src_rpg_or_rpgdir))

 IF extension = "rpgdir" THEN
  dist_basicstatus "LUMPING DATA: please wait..."
  IF NOT write_rpg_or_rpgdir(src_rpg_or_rpgdir, dest_rpg) THEN
   'Already showed error
   RETURN NO
  END IF
 ELSE 'simple case for regular .rpg files
  IF confirmed_copy(src_rpg_or_rpgdir, dest_rpg) = NO THEN
   dist_info "ERROR: failed to copy " & src_rpg_or_rpgdir
  END IF
 END IF
 RETURN YES
END FUNCTION

FUNCTION copy_windows_gameplayer (gameplayer as string, basename as string, destdir as string) as bool
 'Returns true on success, false on failure
 DIM dest_exe as string = destdir & SLASH & basename & ".exe"
 IF confirmed_copy(gameplayer, dest_exe) = NO THEN RETURN NO
 
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

SUB insert_windows_exe_icon (exe_name as string, ico_name as string)
 IF NOT isfile(exe_name) THEN debuginfo exe_name & " not found, ignoring attempt to change its icon" : EXIT SUB
 IF NOT isfile(ico_name) THEN debuginfo ico_name & " does not exist" : EXIT SUB

 DIM rcedit as string = find_windows_helper_app("rcedit", YES)
 IF rcedit = "" THEN dist_info "WARNING: rcedit not found, can't set the icon": EXIT SUB

 DIM args as string = escape_filename(exe_name) & " --set-icon " & escape_filename(ico_name)
 DIM spawn_ret as string
 spawn_ret = win_or_wine_spawn_and_wait(rcedit, args)
 IF LEN(spawn_ret) > 0 THEN dist_info "WARNING: rcedit failed when trying to set the icon: " & spawn_ret : EXIT SUB
END SUB

SUB add_file(byref files as string vector, fname as string)
 'Could use a StrHashTable instead of a vector
 IF v_find(files, fname) = -1 THEN v_append(files, fname)
END SUB

SUB find_required_dlls(gameplayer as string, byref files as string vector)

#IFDEF __FB_WIN32__
 IF gameplayer = exepath & SLASH & "game.exe" THEN
  '--If we are using a copy of the current Windows version,
  '--the backend might be non-default
  DIM gfxbackend_to_use as string = gfxbackend
  IF gen(genResolutionX) <> 320 OR gen(genResolutionY) <> 200 THEN
   'Note: This code is duplicated in apply_game_window_settings
   'This really seems too complicated...
   IF gfx_supports_variable_resolution() = NO THEN
    DIM varresbackends(...) as string = {"sdl2", "sdl", "fb"}
    FOR idx as integer = 0 TO UBOUND(varresbackends)
     IF have_gfx_backend(varresbackends(idx)) THEN
      gfxbackend_to_use = varresbackends(idx)
      EXIT FOR
     END IF
    NEXT
   END IF
  END IF

  SELECT CASE gfxbackend_to_use
   CASE "directx":  add_file files, "gfx_directx.dll"
   CASE "sdl":      add_file files, "SDL.dll"
   CASE "sdl2":     add_file files, "SDL2.dll"
   CASE "alleg":    add_file files, "alleg40.dll"
   CASE "fb":
    'gfx_fb requires no dll files
  END SELECT
  SELECT CASE musicbackend
   CASE "sdl":
    add_file files, "SDL.dll"
    add_file files, "SDL_mixer.dll"
   CASE "sdl2":
    add_file files, "SDL2.dll"
    add_file files, "SDL2_mixer.dll"
   CASE "native", "native2":
    add_file files, "audiere.dll"
   CASE "silence":
    'music_silence requires no dll files
  END SELECT
  EXIT SUB
 END IF
#ENDIF 
 
 '-- for all other cases and all other platforms, we just use whatever
 '-- *.dll files are found in the same folder where we downloaded the
 '-- game player
 DIM filelist() as string
 DIM dirname as string = trimfilename(gameplayer)
 findfiles dirname, "*.dll", fileTypeFile, YES, filelist()
 FOR i as integer = 0 TO UBOUND(filelist)
  add_file files, filelist(i)
 NEXT i
END SUB

FUNCTION copy_linux_gameplayer (gameplayer as string, basename as string, destdir as string) as bool
 'Returns true on success, false on failure
 IF confirmed_copy(gameplayer, destdir & SLASH & basename) = NO THEN RETURN NO
#IFDEF __FB_UNIX__
  '--just in case we are playing with a debug build,
  '--strip the copy of the binary that goes in the distribution file.
  safe_shell "strip " & escape_filename(destdir & SLASH & basename)
  '--fix the permissions
  safe_shell "chmod +x " & escape_filename(destdir & SLASH & basename)
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
  dist_info "ERROR: game.exe wasn't found in the same folder as custom.exe. (This shouldn't happen!)" : RETURN ""
 END IF

#ENDIF
 '--For Non-Windows platforms, we need to download game.exe
 '(NOTE: This all should work fine on Windows too, but it is best to use the installed game.exe)

 '--Find the folder that we are going to download game.exe into
 DIM dldir as string = settings_dir & SLASH & "_gameplayer"
 IF NOT isdir(dldir) THEN makedir dldir
 IF NOT isdir(dldir) THEN dist_info "ERROR: Unable to create """ & dldir & """ directory": RETURN ""
  
 '--Decide which url to download
 DIM url as string
 DIM dlfile as string
 IF version_branch = "wip" THEN
  '--If running a nightly wip, download the default build of the latest nightly wip
  '--(Before 2020-11-17 nightlies downloaded ohrrpgce-player-win-wip.zip instead,
  '--which was assumed to be gfx_directx+sdl[+fb], music_sdl)
  '--(Used to be called ohrrpgce-player-win-wip-sdl2.zip which is now symlinked to the new name)
  dlfile = "ohrrpgce-player-win-sdl2-wip.zip"
  url = "http://hamsterrepublic.com/ohrrpgce/nightly/" & dlfile
 ELSE
  '--Use this stable release
  '--(Previously, always downloaded the latest stable release from
  '--http://hamsterrepublic.com/dl/
  '-- gorgonzola and older downloaded ohrrpgce-player-win.zip (a music_sdl build)
  '-- hróðvitnir downloaded ohrrpgce-player-win-minimal-sdl2.zip
  '--see web/ohrstable.sh for info on server symlinks)

  dlfile = "ohrrpgce-player-win-sdl2" & version_release_tag & ".zip"
  url = "http://hamsterrepublic.com/ohrrpgce/archive/" & dlfile
 END IF

 DIM destzip as string = dldir & SLASH & dlfile

 '--Prompt & download if missing or out of date
 IF NOT download_gameplayer_if_needed(url, destzip, "win.download.agree", "the Windows OHRRPGCE game player") THEN RETURN ""
 
 '--Find the unzip tool
 DIM unzip as string = find_helper_app("unzip", YES)
 IF unzip = "" THEN dist_info "ERROR: Couldn't find unzip tool": RETURN ""
 
 '--remove the old files first
 safekill dldir & SLASH & "game.exe"
 safekill dldir & SLASH & "LICENSE-binary.txt"
 safekill_pattern dldir, "*.dll"
 
 '--Unzip the desired files
 DIM args as string = "-o " & escape_filename(destzip) & " game.exe buildinfo.ini SDL2.dll SDL2_mixer.dll LICENSE-binary.txt -d " & escape_filename(dldir)
 DIM spawn_ret as string = spawn_and_wait(unzip, args)
 IF LEN(spawn_ret) > 0 THEN dist_info "ERROR: unzip failed: " & spawn_ret : RETURN ""

 IF NOT isfile(dldir & SLASH & "game.exe")           THEN dist_info "ERROR: Failed to unzip game.exe" : RETURN ""
 IF NOT isfile(dldir & SLASH & "LICENSE-binary.txt") THEN dist_info "ERROR: Failed to unzip LICENSE-binary.txt" : RETURN ""
 IF sanity_check_buildinfo(dldir & SLASH & "buildinfo.ini") THEN RETURN ""
 '--We might be downloading a future version, and don't know with certainty what dll files it might include
 IF NOT isfile(dldir & SLASH & "SDL2.dll")           THEN debuginfo "WARN: Expected to unzip SDL2.dll but it wasn't there"
 IF NOT isfile(dldir & SLASH & "SDL2_mixer.dll")     THEN debuginfo "WARN: Expected to unzip SDL2_mixer.dll but it wasn't there"

 RETURN dldir & SLASH & "game.exe"
END FUNCTION

FUNCTION sanity_check_buildinfo(buildinfo_file as string) as bool
 'Return YES if the sanity check has failed
 IF NOT isfile(buildinfo_file) THEN dist_info "ERROR: Failed to read buildinfo.ini" : RETURN YES
 DIM ver as integer = read_ini_int(buildinfo_file, "packaging_version", 0)
 IF ver > 1 THEN
  dist_info "ERROR: The buildinfo version is " & ver & " but your copy of the OHRRPGCE only supports version 1. This means you should upgrade to a newer version if you want to keep using the Distribute Game feature.": RETURN YES
 ELSEIF ver < 1 THEN
  dist_info "ERROR: The buildinfo version is missing or invalid. Please report this as a bug to the OHRRPGCE developers": RETURN YES
 END IF
 RETURN NO
END FUNCTION

FUNCTION running_64bit() as bool
#IFDEF __FB_64BIT__
 RETURN YES
#ELSE
 RETURN NO
#ENDIF
END FUNCTION

FUNCTION get_linux_gameplayer(which_arch as string) as string
 'In most cases, download a precompiled binary of ohrrpgce-game,
 'unzip it, and return the full path.
 '
 'Returns "" for failure.
 '
 'On Linux, if you are running a non-debug binary of the requested
 'architecture, then the installed binary will be used rather than
 're-downloading.

DIM arch_suffix as string
SELECT CASE which_arch
 CASE "x86":
  arch_suffix = ""
 CASE "x86_64":
  arch_suffix = "-x86_64"
 CASE ELSE  'Should never happen
  dist_info "get_linux_gameplayer: Requested unsupported arch """ & which_arch & """. The only supported vaues are x86 and x86_64"
  RETURN ""
END SELECT

#IFDEF __GNU_LINUX__

 '--If this is Linux, we might already have the correct version of ohrrpgce-game
 DIM installed_player as string = exepath & SLASH & "ohrrpgce-game"
 debuginfo "Checking for " & installed_player
 IF isfile(installed_player) THEN
  DIM playerversion as string
  IF run_and_get_output(installed_player & " --version", playerversion) = 0 THEN
   'If it's a portable build it's probably a release build too
   IF INSTR(playerversion, which_arch & " ") ANDALSO INSTR(playerversion, "portable") THEN
    debuginfo "Using installed binary " & installed_player
    RETURN installed_player
   ELSE
    debuginfo installed_player & " isn't right arch or not a portable build, don't use it for distribute"
   END IF
  ELSE
   debuginfo "Couldn't run " & installed_player
  END IF
 END IF

#ENDIF

 '--For Non-Linux platforms, we need to download ohrrpgce-game
 '(NOTE: This all should work fine on Linux too, but it is best to use the installed ohrrpgce-game when possible)

 '--Find the folder that we are going to download ohrrpgce-game into
 DIM dldir as string = settings_dir & SLASH & "_gameplayer"
 IF NOT isdir(dldir) THEN makedir dldir
 IF NOT isdir(dldir) THEN dist_info "ERROR: Unable to create """ & dldir & """ directory": RETURN ""
  
 '--Decide which url to download
 DIM url as string
 DIM dlfile as string
 IF version_branch = "wip" THEN
  'If using any wip release, get the latest wip release
  dlfile = "ohrrpgce-player-linux-bin-minimal" & arch_suffix & ".zip"
  url = "http://hamsterrepublic.com/ohrrpgce/nightly/" & dlfile
 ELSE
  'Use this stable release
  dlfile = "ohrrpgce-player-linux-bin-minimal" & version_release_tag & arch_suffix & ".zip"
  url = "http://hamsterrepublic.com/ohrrpgce/archive/" & dlfile
 END IF

 DIM destzip as string = dldir & SLASH & dlfile

 '--Prompt & download if missing or out of date
 IF NOT download_gameplayer_if_needed(url, destzip, "linux.download.agree", "the Linux OHRRPGCE game player") THEN RETURN ""
 
 '--Find the unzip tool
 DIM unzip as string = find_helper_app("unzip", YES)
 IF unzip = "" THEN dist_info "ERROR: Couldn't find unzip tool": RETURN ""
 
 '--Unzip the desired files
 DIM args as string = "-o " & escape_filename(destzip) & " ohrrpgce-game buildinfo.ini LICENSE-binary.txt -d " & escape_filename(dldir)
 DIM spawn_ret as string = spawn_and_wait(unzip, args)
 IF LEN(spawn_ret) > 0 THEN dist_info "ERROR: unzip failed: " & spawn_ret : RETURN ""
 
 IF NOT isfile(dldir & SLASH & "ohrrpgce-game")      THEN dist_info "ERROR: Failed to unzip ohrrpgce-game" : RETURN ""
 IF NOT isfile(dldir & SLASH & "LICENSE-binary.txt") THEN dist_info "ERROR: Failed to unzip LICENSE-binary.txt" : RETURN ""
 IF sanity_check_buildinfo(dldir & SLASH & "buildinfo.ini") THEN RETURN ""
 
 RETURN dldir & SLASH & "ohrrpgce-game"

END FUNCTION

SUB distribute_game_as_windows_installer (dest_override as string = "")
 debuginfo "  distribute_game_as_windows_installer():"

 DIM distinfo as DistribState
 load_distrib_state distinfo

 DIM basename as string = distinfo.pkgname
 DIM installer as string = trimfilename(sourcerpg)
 IF dest_override <> "" THEN installer = dest_override
 installer &= SLASH & "setup-" & basename & ".exe"

 IF isfile(installer) THEN
  IF dist_yesno(decode_filename(trimpath(installer)) & " already exists. Overwrite it?") = NO THEN RETURN
  'Okay to overwrite (but actually do the overwrite later)
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
  IF gameplayer = "" THEN dist_info "ERROR: game.exe is not available" : EXIT DO
  IF copy_windows_gameplayer(gameplayer, basename, isstmp) = NO THEN EXIT DO
  
  insert_windows_exe_icon isstmp & SLASH & basename & ".exe", trimextension(sourcerpg) & ".ico"

  'Write readme with DOS/Window line endings
  write_readme_text_file isstmp & SLASH & "README-" & basename & ".txt", CHR(13) & CHR(10)

  maybe_write_license_text_file isstmp & SLASH & "LICENSE.txt"
  
  write_innosetup_script basename, distinfo.gamename, isstmp

  DIM iss_script as string = isstmp & SLASH & "innosetup_script.iss"
 
  DIM args as string
  'FIXME: The following does not escape all problem characters that could occur in iss_script,
  'but I'm not sure what the best way to do that is, so leaving it for now.
  args = """" & win_path(iss_script) & """"
  
  DIM spawn_ret as string
  spawn_ret = win_or_wine_spawn_and_wait(iscc, args)
  IF LEN(spawn_ret) THEN dist_info "ERROR: iscc.exe failed: " & spawn_ret : EXIT DO
  'Remove the old copy of the installer
  safekill installer
  'Move the new installer to the correct location
  IF confirmed_copy(isstmp & SLASH & "Output" & SLASH & "setup-" & basename & ".exe", installer) = NO THEN
   dist_info "ERROR: iscc.exe completed but installer was not created"
   EXIT DO
  END IF

  dist_info trimpath(installer) & " was successfully created!", errInfo
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
 DIM appversion as string = MID(DATE, 7, 4) & "." & MID(DATE, 1, 2) & "." & MID(DATE, 4, 2)
 s &= "; Inno Setup script generated by OHRRPGCE custom" & E
 
 s &= E & "[Setup]" & E
 s &= "AppName=" & gamename & E
 s &= "AppVersion=" & appversion & E
 'According to the docs AppVerName is not required if both AppVerName and AppName are given,
 'but apparently older Innosetup require it
 s &= "AppVerName=" & gamename & " version " & appversion & E
 s &= "DefaultDirName={pf}\" & gamename & E
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
  dist_info "ERROR: Unable to translate path for wine: " & filename 
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
  IF dist_yesno("Inno Setup 5 is required to create windows installation packages. Would you like to download it from jrsoftware.org now?") THEN
   'Download 5.4.3 (from 2011), the last version to run on non-NT Windows.
   'In future we could switch to 5.6.x (2019), the last version to support Win 2000 and XP.
   'As a separate problem, the following link redirects to https, and wget fails to create a SSL
   'connection on Win XP and older.
   DIM is_exe as string = settings_dir & SLASH & "is.exe"
   'download_file "http://www.jrsoftware.org/download.php/is.exe", is_exe  'Latest version
   download_file "http://files.jrsoftware.org/is/5/isetup-5.4.3.exe", is_exe
   DIM spawn_ret as string
   spawn_ret = win_or_wine_spawn_and_wait(is_exe)
   safekill is_exe
   IF LEN(spawn_ret) THEN dist_info "ERROR: Inno Setup installer failed: " & spawn_ret : RETURN ""
   '--re-search for iscc now that it may have been installed
   iscc = find_innosetup()
  END IF
  IF iscc = "" THEN dist_info "Cancelling export. Inno Setup 5 is not available." : RETURN ""
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
 'Currently only needed for installing and running Innosetup and rcedit (for icons).
 'Hopefully we won't ever need it for anything else
 DIM spawn_ret as string
#IFDEF __FB_WIN32__
 'On Windows this is nice and simple
 RETURN spawn_and_wait(cmd, args)
#ELSE
 'args is probably insufficiently escaped, but not sure how to fix that
 DIM wine_args as string = escape_filename(cmd) & " " & escape_string(args, "\")
 RETURN spawn_and_wait("wine", wine_args)
#ENDIF
 
END FUNCTION

SUB distribute_game_as_debian_package (which_arch as string, dest_override as string = "")
 debuginfo "  distribute_game_as_debian_package():"

 DIM deb_arch as string
 SELECT CASE which_arch
  CASE "x86":
   deb_arch = "i386"
  CASE "x86_64":
   deb_arch = "amd64"
  CASE ELSE:
   dist_info "Unknown arch """ & which_arch & """ should be one of x86 or x86_64"
   EXIT SUB
 END SELECT

 DIM distinfo as DistribState
 load_distrib_state distinfo

 DIM basename as string = get_debian_package_name()
 DIM pkgver as string = get_debian_package_version()
 DIM debname as string = trimfilename(sourcerpg)
 IF dest_override <> "" THEN debname = dest_override
 debname &= SLASH & basename & "_" & pkgver & "_" & deb_arch & ".deb"

 IF isfile(debname) THEN
  IF dist_yesno(trimpath(debname) & " already exists. Overwrite it?") = NO THEN RETURN
  'Okay to overwrite, but do it later
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
  gameplayer = get_linux_gameplayer(which_arch)
  IF gameplayer = "" THEN dist_info "ERROR: ohrrpgce-game is not available" : EXIT DO
  IF copy_linux_gameplayer(gameplayer, basename, bindir) = NO THEN EXIT DO
  
  debuginfo "Create menu file"
  DIM menudir as string = debtmp & SLASH & "usr" & SLASH & "share" & SLASH & "menu"
  makedir menudir
  write_linux_menu_file distinfo.gamename, menudir & SLASH & basename, basename

  debuginfo "Create desktop file"
  DIM applicationsdir as string = debtmp & SLASH & "usr" & SLASH & "share" & SLASH & "applications"
  makedir applicationsdir
  write_linux_desktop_file distinfo.gamename, applicationsdir & SLASH & basename & ".desktop", basename

  debuginfo "Create docs"
  DIM docsdir as string = debtmp & SLASH & "usr" & SLASH & "share" & SLASH & "doc"
  makedir docsdir
  DIM gamedocsdir as string = docsdir & SLASH & basename
  makedir gamedocsdir
  write_readme_text_file gamedocsdir & SLASH & "README.txt", CHR(10)
  IF gzip_file(gamedocsdir & SLASH & "README.txt") = NO THEN EXIT DO
  write_debian_copyright_file gamedocsdir & SLASH & "copyright"

  IF distinfo.license <> "GPL" THEN
   '--only write non-GPL license files because Debian policy prefers referencing the local copy
   DIM lic_file as string = gamedocsdir & SLASH & "LICENSE-" & distinfo.license & ".txt"
   maybe_write_license_text_file lic_file
   IF isfile(lic_file) THEN
    IF gzip_file(lic_file) = NO THEN EXIT DO
   END IF
  END IF

  debuginfo "Calculate Installed-Size"
  DIM size_in_kibibytes as integer = count_directory_size(debtmp & SLASH & "usr") / 1024
 
  debuginfo "Create debian-binary version file"
  write_debian_binary_file debtmp & SLASH & "debian-binary"

  debuginfo "Create debian control file"
  write_debian_control_file debtmp & SLASH & "control", basename, pkgver, size_in_kibibytes, distinfo, deb_arch
  IF NOT isfile(debtmp & SLASH & "control") THEN dist_info "Couldn't create debian control file" : EXIT DO
  write_debian_postinst_script debtmp & SLASH & "postinst"
  write_debian_postrm_script debtmp & SLASH & "postrm"

  fix_deb_group_permissions debtmp & SLASH & "control"
  fix_deb_group_permissions debtmp & SLASH & "postinst"
  fix_deb_group_permissions debtmp & SLASH & "postrm"

  IF create_tarball(debtmp, debtmp & SLASH & "control.tar.gz", "control postinst postrm") = NO THEN EXIT DO

  fix_deb_group_permissions debtmp & SLASH & "usr"
  
  IF create_tarball(debtmp, debtmp & SLASH & "data.tar.gz", "usr") = NO THEN EXIT DO

  'Remove old deb
  safekill debname
  
  'Create new deb
  IF create_ar_archive(debtmp, debname, "debian-binary control.tar.gz data.tar.gz") = NO THEN EXIT DO
  
  dist_info trimpath(debname) & " was successfully created!", errInfo
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
 #IFDEF __FB_UNIX__
  safe_shell "chmod +x " & escape_filename(filename)
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
 #IFDEF __FB_UNIX__
  safe_shell "chmod +x " & escape_filename(filename)
 #ENDIF
END SUB

SUB fix_deb_group_permissions(start_at_dir as string)
 #IFDEF __FB_UNIX__
  'This is needed because the user's umask might have given group write access to the files
  safe_shell "chmod -R g-w " & escape_filename(start_at_dir)
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

FUNCTION create_ar_archive(start_in_dir as string, archive as string, files as string) as bool
 '--Returns YES if successful, or NO if failed

 ' start_in_dir only applies to the ar command. The archive filename should still be either absolute or relative to the default CURDIR

 'files is a list of space-separated filenames and directory names to include in the tarball
 'if they contain spaces they must be quoted

 DIM ar as string = find_helper_app("ar", YES)
 IF ar = "" THEN dist_info "ERROR: ar is not available" : RETURN NO

 DIM args as string
 args = " qc"
 #IFNDEF __FB_DARWIN__
 'Non-mac platforms can use the D arg for deterministic mode
 args &= "D"
 #ENDIF
 args &= " " & escape_filename(archive) & " " & files
 'debug ar & " " & args
 DIM spawn_ret as string
 
 DIM olddir as string = CURDIR
 CHDIR start_in_dir
 spawn_ret = spawn_and_wait(ar, args)
 CHDIR olddir
 
 IF LEN(spawn_ret) THEN dist_info spawn_ret : RETURN NO
 IF NOT isfile(archive) THEN dist_info "Could not create " & archive : RETURN NO
 RETURN YES
END FUNCTION

FUNCTION create_zipfile(start_in_dir as string, zipfile as string, files as string) as bool
 '--Returns YES if successful, or NO if failed

 ' start_in_dir only applies to the zip command. The zipfile filename should still be either absolute or relative to the default CURDIR

 'files is a list of space-separated filenames and directory names to include in the tarball
 'if they contain spaces they must be quoted
 
 DIM zip as string = find_helper_app("zip", YES)
 IF zip = "" THEN dist_info "ERROR: zip is not available": RETURN NO

 DIM spawn_ret as string
 DIM args as string

 args = " -r -9 " & escape_filename(zipfile) & " " & files
 'debug zip & " " & args
 
 DIM olddir as string = CURDIR
 CHDIR start_in_dir
 spawn_ret = spawn_and_wait(zip, args)
 CHDIR olddir
 
 IF LEN(spawn_ret) THEN dist_info spawn_ret : RETURN NO

 IF NOT isfile(zipfile) THEN dist_info "Could not create " & zipfile : RETURN NO
 RETURN YES
END FUNCTION

FUNCTION create_tarball(start_in_dir as string, tarball as string, files as string) as bool
 '--Returns YES if successful, or NO if failed

 ' start_in_dir only applies to the tar command. The tarball filename should still be either absolute or relative to the default CURDIR

 'files is a list of space-separated filenames and directory names to include in the tarball
 'if they contain spaces they must be quoted
 
 DIM tar as string = find_helper_app("tar", YES)
 IF tar = "" THEN dist_info "ERROR: tar is not available": RETURN NO

 DIM gzip as string = find_helper_app("gzip", YES)
 IF gzip = "" THEN dist_info "ERROR: gzip is not available": RETURN NO

 DIM as string tarversion, errstr
 IF run_and_get_output(tar & " --version", tarversion, errstr) THEN
  dist_info !"ERROR: couldn't run tar\n" & errstr
  RETURN NO
 END IF

 DIM gnutar as bool = INSTR(tarversion, "GNU tar")
 'bsdtar (frontend to libarchive) is a modern tar implementation, shipped on
 'FreeBSD, NetBSD, OSX, Win10 19xx+. Its arguments are quite different from gnutar.
 'DIM bsdtar as bool = INSTR(tarversion, "libarchive")
 'There are also other tars, like the one used by OpenBSD, and busybox

 #IFDEF __FB_WIN32__
  IF gnutar = NO THEN
   'We need gnutar to be able to set the correct permissions (so I assume),
   'see below. So download it. (Note that we look in support before PATH)
   debuginfo tar & " isn't gnutar: " & tarversion
   tar = install_windows_helper_app("tar")
   IF tar = "" THEN dist_info "ERROR: gnutar is not available": RETURN NO
   gnutar = YES
  END IF
 #ENDIF

 DIM more_args as string = ""

 IF gnutar THEN
  #IFNDEF __FB_WIN32__
   'These arguments are broken on Windows tar.exe for some stupid reason
   '("root: invalid owner". Using uid/gid 0 instead doesn't work either.)
   'Instead the current username gets put in the .tar. Presumably it doesn't
   'matter, since the root user would be installing a .deb anyway.
   more_args = " --owner=root --group=root"
  #ENDIF
 ELSE 'ELSEIF bsdtar THEN
  more_args = " --uname=root --gname=root"
 END IF

 #IFDEF __FB_WIN32__
  IF gnutar THEN
   'This is a hack to replace tar.exe's horrendous default permissions, and to (clumsily) mark the executables with the executable bit
   '(Both bsdtar and gnutar use modes 777 for .exe files and 666 for all others.
   'Unfortunately bsdtar doesn't have a way to override the mode.
    more_args &= " --mode=755"

   'This is a workaround for the dumbest misfeature: some versions of tar.exe
   '(the one in msys, but not the one we distribute) will treat the name of the tarball
   'as being a remote filename if it contains a ':'... even on Windows!!!!11asjdajd
   more_args &= " --force-local"
  END IF
 #ENDIF
 
 DIM uncompressed as string = trimextension(tarball)
 DIM args as string

 args = " -c " & more_args & " -f " & escape_filename(uncompressed) & " " & files

 DIM olddir as string = CURDIR
 CHDIR start_in_dir
 DIM spawn_ret as string
 spawn_ret = spawn_and_wait(tar, args)
 CHDIR olddir
 
 IF LEN(spawn_ret) THEN dist_info spawn_ret : RETURN NO

 IF gzip_file(uncompressed) = NO THEN RETURN NO
 
 IF NOT isfile(tarball) THEN dist_info "Could not create " & tarball : RETURN NO
 RETURN YES
END FUNCTION

FUNCTION extract_tarball(into_dir as string, tarball as string, files as string) as bool
 '--Returns YES if successful, or NO if failed
 
 'The tarball must already be decompressed. Don't pass in a .tar.gz (this is inconsistent
 'with create_tarball, I know. )

 ' into_dir only applies to the tar command. The tarball filename should still be either absolute or relative to the default CURDIR

 'files is a list of space-separated filenames and directory names to extract from the tarball
 'if they contain spaces they must be quoted
 
 
 DIM tar as string = find_helper_app("tar", YES)
 IF tar = "" THEN dist_info "ERROR: tar utility is not available": RETURN NO

 DIM spawn_ret as string
 DIM args as string

 args = " -x -f " & escape_filename(tarball) & " " & files
 'debug tar & " " & args
 
 DIM olddir as string = CURDIR
 CHDIR into_dir
 spawn_ret = spawn_and_wait(tar, args)
 CHDIR olddir
 
 IF LEN(spawn_ret) THEN dist_info spawn_ret : RETURN NO
 
 RETURN YES
END FUNCTION

FUNCTION gzip_file (filename as string) as bool
 'Returns YES on success, NO on failure
 DIM gzip as string = find_helper_app("gzip", YES)
 IF gzip = "" THEN dist_info "ERROR: gzip is not available": RETURN NO
 
 DIM args as string
 args = escape_filename(filename)
 DIM spawn_ret as string
 spawn_ret = spawn_and_wait(gzip, args)
 IF LEN(spawn_ret) THEN dist_info spawn_ret : RETURN NO
 IF NOT isfile(filename & ".gz") THEN
  dist_info "ERROR: gzip completed but " & filename & ".gz was not created"
  RETURN NO
 END IF

 RETURN YES
END FUNCTION

FUNCTION gunzip_file (filename as string) as bool
 'Extracts a .gz file, creating a new file minus the .gz extension next to it.
 'Doesn't delete the .gz.
 'Returns YES on success, NO on failure
 DIM gzip as string = find_helper_app("gzip", YES)
 IF gzip = "" THEN dist_info "ERROR: gzip is not available": RETURN NO
 
 DIM args as string
 'Note, the gzip.exe we ship originally shipped didn't support -k. Version 1.3.12-1 does.
 args = " -d -f -k " & escape_filename(filename)
 DIM spawn_ret as string
 spawn_ret = spawn_and_wait(gzip, args)
 IF LEN(spawn_ret) THEN dist_info spawn_ret : RETURN NO
 IF NOT isfile(trimextension(filename)) THEN
  dist_info "ERROR: gzip -d completed but " & filename & " was not uncompressed"
  RETURN NO
 END IF

 RETURN YES
END FUNCTION

SUB write_debian_binary_file (filename as string)
 DIM fh as integer = FREEFILE
 OPEN filename for binary as #fh
 PUT #fh, ,"2.0" & CHR(10)
 CLOSE #fh
END SUB

SUB write_debian_control_file(controlfile as string, basename as string, pkgver as string, size_in_kibibytes as integer, byref distinfo as DistribState, deb_arch as string)
 CONST LF as string = CHR(10)

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
 PUT #fh, , "Architecture: " & deb_arch & LF
 PUT #fh, , "Version: " & pkgver & LF
 PUT #fh, , "Installed-Size: " & size_in_kibibytes & LF
 'FIXME: the Depends: line could vary depending on gfx and music backends
 'This minimum libc version is taken from "scons portable=1" output (see nightly build logs)
 PUT #fh, , "Depends: libc6 (>= 2.14), libncurses5 (>= 5.4), libsdl2-mixer-2.0-0 (>= 2.0.1), libsdl2-2.0-0 (>= 2.0.5), libx11-6, libxext6, libxpm4, libxrandr2, libxrender1" & LF
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

'Much stricter version of sanitize_pkgname.
'distinfo.pkgname sanitized to contain only lowercase a-z and non-adjacent non-leading non-trailing - dashes
FUNCTION get_debian_package_name() as string
 DIM distinfo as DistribState
 load_distrib_state distinfo
 DIM s as string
 s = distinfo.pkgname
 s = LCASE(s)
 s = exclude(s, "'")
 DIM result as string = ""
 DIM ch as string
 DIM dash as bool = NO
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

FUNCTION can_run_windows_exes () as bool
#IFDEF __FB_WIN32__
 '--Of course we can always run exe files on Windows
 RETURN YES
#ELSE
 '--Unixen and Macs can only run exe files with wine
 IF find_helper_app("wine") = "" THEN RETURN NO
 IF NOT isdir(environ("HOME") & "/.wine/dosdevices/c:") THEN RETURN NO
 RETURN YES
#ENDIF
END FUNCTION

FUNCTION can_make_tarballs () as bool
 '--check to see if we can find the tools needed to create a .tar.gz tarball
 IF find_helper_app("tar") = "" THEN RETURN NO
 IF find_helper_app("gzip") = "" THEN RETURN NO
 RETURN YES
END FUNCTION

FUNCTION can_make_debian_packages () as bool
 '--check to see if we can find the tools needed to create a .deb package
 IF find_helper_app("ar") = "" THEN RETURN NO
 IF find_helper_app("tar") = "" THEN RETURN NO
 IF find_helper_app("gzip") = "" THEN RETURN NO
 RETURN YES
END FUNCTION

FUNCTION can_make_mac_packages () as bool
 '--check to see if we can find the tools needed to compress a mac .app package
 IF find_helper_app("tar") = "" THEN RETURN NO
 IF find_helper_app("gzip") = "" THEN RETURN NO
 RETURN YES
END FUNCTION

SUB distribute_game_as_mac_app (which_arch as string, dest_override as string = "")
 debuginfo "  distribute_game_as_mac_app():"

 DIM distinfo as DistribState
 load_distrib_state distinfo

 DIM basename as string = distinfo.pkgname
 DIM destname as string = trimfilename(sourcerpg)
 IF dest_override <> "" THEN destname = dest_override
 destname &= SLASH & basename
 IF which_arch = "x86" THEN
  'This is an obsolete arch, add a suffix
  destname &= "-mac-32bit.zip"
 ELSE
  destname &= "-mac.zip"
 END IF
 DIM destshortname as string = trimpath(destname)

 IF isfile(destname) THEN
  IF dist_yesno(destshortname & " already exists. Overwrite it?") = NO THEN RETURN
  'Okay to overwrite! (but actually do the overwriting later on)
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
  gameplayer = get_mac_gameplayer(which_arch)
  IF gameplayer = "" THEN dist_info "ERROR: OHRRPGCE-Game.app for " & which_arch & " is not available" : EXIT DO
  DIM app as string = apptmp & SLASH & basename & ".app"
#IFDEF __FB_WIN32__
  IF confirmed_copydirectory(gameplayer, app) = NO THEN dist_info "Couldn't copy " & gameplayer & " to " & app : EXIT DO
#ELSE
  'Mac and Linux do it this way to preserve symlinks and permissions
  IF os_shell_move(gameplayer, app) = NO THEN dist_info "Couldn't move " & gameplayer & " to " & app : EXIT DO
#ENDIF
  IF confirmed_copy(trimfilename(gameplayer) & SLASH & "LICENSE-binary.txt", apptmp & SLASH & "LICENSE-binary.txt") = NO THEN EXIT DO

  debuginfo "Copy rpg file"
  'Renaming the .rpg file to pkgname only for consistency with all other packagers
  DIM resources as string
  resources = app & SLASH & "Contents" & SLASH & "Resources"
  IF copy_or_relump(sourcerpg, resources & SLASH & basename & ".rpg") = NO THEN EXIT DO
  
  debuginfo "Create bundledgame file"
  DIM fh as integer = FREEFILE
  OPEN resources & SLASH & "bundledgame" FOR OUTPUT AS #fh
  PRINT #fh, basename
  CLOSE #fh
  
  DIM icns_file as string = trimextension(sourcerpg) & ".icns"
  IF isfile(icns_file) THEN
   confirmed_copy(icns_file, resources & SLASH & "game.icns")
  END IF

  write_readme_text_file apptmp & SLASH & "README-" & basename & ".txt", CHR(10)

  maybe_write_license_text_file apptmp & SLASH & "LICENSE.txt"

  'Remove the old copy that we are replacing
  safekill destname

  'Package as a .zip file. NOTE: When created on Unix, .zip files contain file permissions
  'including the all-important +x bit in the "external attributes", but on Windows they don't.
  'On older versions of MacOS we could rely on a quirk of the old Mac Finder, which would set
  'the +x bit on every file when extracting files from a Windows/DOS .zip file. Newer Mac
  'archivers do not do this. Instead we use the zip_exec tool to add the +x bit manually.
  'See fix_mac_app_executable_bit_on_windows(). An alternate unused solution to actually
  'create Mac .zip packages with correct file permissions from Windows, is in
  'prepare_mac_app_zip()
  DIM olddir as string = CURDIR
  CHDIR apptmp
  IF create_zipfile(apptmp, destname, "*.app *.txt") = NO THEN
   CHDIR olddir
   EXIT DO
  END IF
  CHDIR olddir
  
  'Fix the executable bit on windows
#IFDEF __FB_WIN32__
  DIM execname as string = basename & ".app/Contents/MacOS/ohrrpgce-game"
  IF fix_mac_app_executable_bit_on_windows(destname, execname) = NO THEN
   dist_info "Was not able to set the executable bit on the Mac app bundle zip file. The app may not work as expected."
  END IF
#ENDIF

  dist_info destshortname & " was successfully created!", errInfo
  'I have seen someone -- on Linux, even -- wipe the +x flag on ohrrpgce-game
  'by unzipping and rezipping.
  dist_info "Note: Don't unzip and re-zip " & destshortname & ": that might wipe critical metadata from " _
            & basename & ".app and prevent it from running!", errInfo
  EXIT DO 'this loop is only ever one pass
 LOOP

 '--Cleanup temp files
 killdir apptmp, YES

END SUB

FUNCTION fix_mac_app_executable_bit_on_windows(zipfile as string, exec_path_in_zip as string) as bool
 'Return YES on success, NO on failure

 DIM zip_exec as string = find_helper_app("zip_exec", YES)
 IF zip_exec = "" THEN dist_info "ERROR: zip_exec is not available": RETURN NO

 DIM cmd as string
 cmd = escape_filename(zip_exec) & " " & escape_filename(zipfile) & " " & escape_filename(exec_path_in_zip)
 'debug cmd

 DIM cmd_stdout as string
 DIM res as integer = run_and_get_output(cmd, cmd_stdout)

 'zip_exec itself doesn't return an exit code. It prints an error message to stdout
 IF res ORELSE INSTR(cmd_stdout, "error:") THEN
  dist_info cmd & !" failed:\n" & cmd_stdout & IIF(res, !"\nexitcode=" & res, "")
  RETURN NO
 END IF

 RETURN YES
END FUNCTION

/' This works, but isn't used. Won't be needed now that we have zip_exec. See comment above.
FUNCTION prepare_mac_app_zip(zipfile as string, gamename as string) as bool
 '--Renames OHRRPGCE-Game.app to gamename.app inside a zip file, without extracting it
 DIM unzip as string = find_helper_app("unzip", YES)
 IF unzip = "" THEN dist_info "ERROR: unzip is not available": RETURN NO
 DIM ziptool as string = find_helper_app("ziptool", YES)
 IF ziptool = "" THEN dist_info "ERROR: ziptool is not available": RETURN NO

 'First get list of files/directories
 DIM as string stdout_s, stderr_s
 IF run_and_get_output(unzip & " -Z -1 " & escape_filename(zipfile), stdout_s, stderr_s) THEN
  dist_info !"Couldn't examine zip file:\n" & stderr_s
  RETURN NO
 END IF
 DIM files() as string
 split stdout_s, files()

 'Rename each entry
 DIM renamed as integer = 0
 DIM args as string
 FOR idx as integer = 0 TO UBOUND(files)
  DIM newname as string = files(idx)
  IF replacestr(newname, "OHRRPGCE-Game.app", gamename & ".app") THEN
   renamed += 1
   args += " rename " & idx & " " & escape_filename(newname)
  END IF
 NEXT

 IF renamed < 5 THEN  'Sanity check
  dist_info "Looks like contents of " & zipfile & " isn't right"
  RETURN NO
 END IF

 IF safe_shell(ziptool & " " & escape_filename(zipfile) & args) THEN
  dist_info "Modifying " & zipfile & " with ziptool failed"
  RETURN NO
 END IF

 RETURN YES
END FUNCTION
'/

FUNCTION get_mac_gameplayer(which_arch as string) as string
 'Download OHRRPGCE-Game.app,
 'unzip it, and return the full path.
 'Returns "" for failure.

 DIM arch_suffix as string
 SELECT CASE which_arch
  CASE "x86", "x86_64":
   arch_suffix = "-" & which_arch
  CASE ELSE:
   dist_info "Unknown arch """ & which_arch & """; should be one of x86 or x86_64"
   RETURN ""
 END SELECT

 '--Find the folder that we are going to download OHRRPGCE-Game.app into
 DIM dldir as string = settings_dir & SLASH & "_gameplayer"
 IF NOT isdir(dldir) THEN makedir dldir
 IF NOT isdir(dldir) THEN dist_info "ERROR: Unable to create """ & dldir & """ directory": RETURN ""
  
 '--Decide which url to download
 DIM url as string
 DIM dlfile as string

 IF version_branch = "wip" THEN
  'If using any wip release, get the latest wip release
  dlfile = "ohrrpgce-mac-minimal" & arch_suffix & ".tar.gz"
  url = "http://hamsterrepublic.com/ohrrpgce/nightly/" & dlfile
 ELSE
  'Use this stable release
  dlfile = "ohrrpgce-mac-minimal" & version_release_tag & arch_suffix & ".tar.gz"
  url = "http://hamsterrepublic.com/ohrrpgce/archive/" & dlfile
 END IF

 DIM destgz as string = dldir & SLASH & dlfile
 DIM desttar as string = trimextension(destgz)

 '--Prompt & download if missing or out of date
 IF NOT download_gameplayer_if_needed(url, destgz, "mac.download.agree", "the Mac OHRRPGCE game player") THEN RETURN ""

 DIM game_app as string = dldir & SLASH & "OHRRPGCE-Game.app"

 '--remove the old uncompressed files
 safekill dldir & SLASH & "LICENSE-binary.txt"
 IF isdir(game_app) THEN killdir game_app, YES
 
 '--Untar the desired files
 IF gunzip_file(destgz) = NO THEN RETURN ""
 IF extract_tarball(dldir, desttar, "OHRRPGCE-Game.app buildinfo.ini LICENSE-binary.txt") = NO THEN RETURN ""
 
 IF NOT isdir(game_app) THEN dist_info "ERROR: Failed to untar OHRRPGCE-Game.app" : RETURN ""
 IF NOT isfile(game_app & SLASH & "Contents" & SLASH & "MacOS" & SLASH & "ohrrpgce-game")   THEN dist_info "ERROR: Failed to completely untar OHRRPGCE-Game.app" : RETURN ""
 IF NOT isfile(dldir & SLASH & "LICENSE-binary.txt") THEN dist_info "ERROR: Failed to untar LICENSE-binary.txt" : RETURN ""
 IF sanity_check_buildinfo(dldir & SLASH & "buildinfo.ini") THEN RETURN ""
 
 RETURN game_app
END FUNCTION

SUB distribute_game_as_linux_tarball (which_arch as string, dest_override as string = "")
 debuginfo "  distribute_game_as_linux_tarball():"

 DIM arch_suffix as string
 SELECT CASE which_arch
  CASE "x86", "x86_64":
   arch_suffix = "-" & which_arch
  CASE ELSE:
   dist_info "Unknown arch """ & which_arch & """; should be one of x86 or x86_64"
   EXIT SUB
 END SELECT

 DIM distinfo as DistribState
 load_distrib_state distinfo

 DIM destname as string = trimfilename(sourcerpg)
 IF dest_override <> "" THEN destname = dest_override
 destname &= SLASH & distinfo.pkgname & "-linux" & arch_suffix & ".tar.gz"

 IF isfile(destname) THEN
  IF dist_yesno(trimpath(destname) & " already exists. Overwrite it?") = NO THEN RETURN
  'Okay to overwrite! (but actually do the overwriting later on)
 END IF

 DIM apptmp as string = trimfilename(sourcerpg) & SLASH & "linuxtarball.tmp"
 IF isdir(apptmp) THEN
  debuginfo "Clean up old " & apptmp
  killdir apptmp, YES
 END IF
 
 makedir apptmp

 DO '--single pass loop for breaking

  DIM basename as string = distinfo.pkgname

  debuginfo "Rename linux game player" 
  DIM gameplayer as string
  gameplayer = get_linux_gameplayer(which_arch)
  IF gameplayer = "" THEN dist_info "ERROR: ohrrpgce-game is not available" : EXIT DO
  debuginfo " exe: " & gameplayer
  DIM tarballdir_base as string = distinfo.pkgname & "-linux"
  DIM tarballdir as string = apptmp & SLASH & tarballdir_base
  debuginfo " tarballdir: " & tarballdir
  makedir tarballdir
  DIM dest_gameplayer as string = tarballdir & SLASH & basename
  IF confirmed_copy(gameplayer, dest_gameplayer) = NO THEN dist_info "Couldn't copy " & gameplayer & " to " & dest_gameplayer : EXIT DO
  #IFDEF __FB_UNIX__
   'Mac and Linux fix the permissions
   safe_shell "chmod +x " & escape_filename(dest_gameplayer)
  #ENDIF
  DIM license as string = finddatafile("LICENSE-binary.txt")
  IF confirmed_copy(license, tarballdir & SLASH & "LICENSE-binary.txt") = NO THEN EXIT DO

  debuginfo "Copy rpg file"
  IF copy_or_relump(sourcerpg, tarballdir & SLASH & basename & ".rpg") = NO THEN EXIT DO

  write_readme_text_file tarballdir & SLASH & "README-" & basename & ".txt", CHR(10)

  maybe_write_license_text_file tarballdir & SLASH & "LICENSE.txt"

  'Remove the old copy that we are replacing
  safekill destname
  DIM olddir as string = CURDIR
  CHDIR apptmp
  IF create_tarball(apptmp, destname, escape_filename(tarballdir_base)) = NO THEN
   CHDIR olddir
   EXIT DO
  END IF
  CHDIR olddir
  
  dist_info trimpath(destname) & " was successfully created!", errInfo
  EXIT DO 'this loop is only ever one pass
 LOOP

 '--Cleanup temp files
 killdir apptmp, YES

END SUB

FUNCTION dist_yesno(capt as string, byval defaultval as bool=YES, byval escval as bool=NO) as bool
 IF auto_choose_default THEN RETURN defaultval
 RETURN yesno(capt, defaultval, escval)
END FUNCTION

SUB dist_info (msg as zstring ptr, errlvl as errorLevelEnum = errDebug)
 IF auto_choose_default = NO THEN
  IF errlvl = errInfo  THEN errlvl = errShowInfo
  IF errlvl = errDebug THEN errlvl = errShowDebug
 END IF
 debugc errlvl, msg
END SUB

SUB auto_export_distribs (distrib_type as string)
 debuginfo "Auto-export: " & distrib_type

 auto_choose_default = YES
 IF distrib_type = "zip" ORELSE distrib_type = "all" THEN
  distribute_game_as_zip
 END IF
 IF distrib_type = "win" ORELSE distrib_type = "all" THEN
  IF can_run_windows_exes() THEN
   distribute_game_as_windows_installer
  ELSE
   dist_info "auto distrib: windows installer export unavailable"
  END IF
 END IF
 IF distrib_type = "mac32" ORELSE distrib_type = "all" THEN
  IF can_make_mac_packages() THEN
   distribute_game_as_mac_app "x86"
  ELSE
   dist_info "auto distrib: mac 32-bit app export unavailable"
  END IF
 END IF
 IF distrib_type = "mac64" ORELSE distrib_type = "mac" ORELSE distrib_type = "all" THEN
  IF can_make_mac_packages() THEN
   distribute_game_as_mac_app "x86_64"
  ELSE
   dist_info "auto distrib: mac 64-bit app export unavailable"
  END IF
 END IF
 IF distrib_type = "debian32" ORELSE distrib_type = "all" THEN
  IF can_make_debian_packages() THEN
   distribute_game_as_debian_package "x86"
  ELSE
   dist_info "auto distrib: debian 32bit package export unavailable"
  END IF
 END IF
 IF distrib_type = "debian64" ORELSE distrib_type = "debian" ORELSE distrib_type = "all" THEN
  IF can_make_debian_packages() THEN
   distribute_game_as_debian_package "x86_64"
  ELSE
   dist_info "auto distrib: debian 64bit package export unavailable"
  END IF
 END IF
 IF distrib_type = "tarball32" ORELSE distrib_type = "all" THEN
  IF can_make_tarballs() THEN
   distribute_game_as_linux_tarball "x86"
  ELSE
   dist_info "auto distrib: linux 32bit tarball export unavailable"
  END IF
 END IF
 IF distrib_type = "tarball64" ORELSE distrib_type = "tarball" ORELSE distrib_type = "all" THEN
  IF can_make_tarballs() THEN
   distribute_game_as_linux_tarball "x86_64"
  ELSE
   dist_info "auto distrib: linux 64bit tarball export unavailable"
  END IF
 END IF
 auto_choose_default = NO
END SUB

/'
'Ask whether to download the latest version of a file if it's been days_per_check since the last prompt
FUNCTION occasionally_prompt_to_download(destfile as string, description as string)
 DIM last_check_key as string = "downloads.last_check." & trimpath(destfile)

 DIM check_interval as integer = read_config_int("downloads.days_per_check", 7)
 DIM last_check as double = VAL(read_config_str(last_check_key))  '0.0 if invalid
 DIM old_mtime as double = FILEDATETIME(destfile)
 last_check = large(last_check, old_mtime)
 IF last_check + check_interval < NOW THEN
  RETURN dist_yesno("You are running " & short_version & !"\n" & _
                    "You have a " & description & " from " & format_date(old_mtime) & !"\n" & _
                    "Do you want to download the latest " & description & "?", _
                    NO, NO)
 END IF
 RETURN NO
END FUNCTION
'/

'Ask the user for permission the first time we download (subsequent updates don't ask)
FUNCTION agreed_to_download(agree_file as string, description as string) as bool
 IF NOT isfile(agree_file) THEN
  IF dist_yesno("Is it okay to download " & description & " now?") = NO THEN RETURN NO
  touchfile agree_file
 END IF
 RETURN YES
END FUNCTION

'Download a game player package, with appropriate prompts.
'For 'wip' versions, downloads a new nightly build if destfile is older than
'Custom, otherwise downloads only if missing.
'Returns true if we have destfile and it's either up-to-date or the user agreed
'to use an older copy.
FUNCTION download_gameplayer_if_needed(url as string, destfile as string, agree_filename as string, description as string) as bool
 DIM dldir as string = settings_dir & SLASH & "_gameplayer"
 DIM as double old_mtime, new_mtime
 DIM download as bool = NO

 IF isfile(destfile) = NO THEN
  download = YES
 ELSEIF version_branch = "wip" THEN
  'We could extract buildinfo.ini from the archive instead of using the mtime,
  'but both wget and curl should set the mtime to match the original mtime on
  'the server, so this should work.  Just not sure about timezones...
  'but since we don't include the time this should download if the archive is from yesterday.
  old_mtime = FILEDATETIME(destfile)

  'Could also use FILEDATETIME(EXEPATH) instead of version_date
  DIM builddate as double = DATESERIAL(version_date \ 10000, (version_date \ 100) MOD 100, version_date MOD 100)

  debuginfo trimpath(destfile) & " mtime " & format_date(old_mtime)
  debuginfo "Custom version_date " & format_date(builddate)
  debuginfo CUSTOMEXE & " mtime " & format_date(FILEDATETIME(EXEPATH))

  'Always download if older than Custom
  IF old_mtime < builddate THEN download = YES
 END IF

 IF download THEN
  'Ask for permission the first time we download
  IF agreed_to_download(dldir & SLASH & agree_filename, description & " from HamsterRepublic.com") = NO THEN RETURN NO

  IF version_branch = "wip" THEN
   dist_info "The latest nightly WIP version of the OHRRPGCE will be used, even if that is newer than the version you are currently using.", errInfo
  END IF

  IF download_file(url, destfile) = NO THEN
   IF isfile(destfile) THEN
    'A previous download, which hasn't been replaced.
    'This should only happen when downloading a nightly build
    RETURN dist_yesno(!"Downloading the latest version failed.\n" & _
                      "You are running " & short_version & !"\n" & _
                      "You still have " & description & " from " & FORMAT(old_mtime, "yyyy-mm-dd") & !" (" & version_branch & !")\n" & _
                      "Continue with this old version?", _
                      NO, NO)
   ELSE
    RETURN NO
   END IF
  END IF
 END IF

 IF isfile(destfile) THEN
  'Log that we've used this file, so that we can cleanup the least recently used
  'downloads (TODO: not implemented yes) which are the top-most files in the
  '.ini due to using shuffle_to_end=YES
  write_ini_prefixed_str(dldir & SLASH & "lastused.ini", trimpath(destfile), STR(NOW), YES, YES)

  new_mtime = FILEDATETIME(destfile)
  debuginfo destfile & " mtime after download " & format_date(new_mtime)

  RETURN YES
 END IF
END FUNCTION


CONST itchmenuEXIT as integer = 1
CONST itchmenuUSER as integer = 2
CONST itchmenuUPLOAD as integer = 3
CONST itchmenuBUTLERSETUP as integer = 4
CONST itchmenuGAMENAME as integer = 5

SUB itch_io_options_menu ()

 DIM distinfo as DistribState
 load_distrib_state distinfo

 DIM menu as SimpleMenuItem vector
 v_new menu, 0

 DIM st as MenuState
 st.need_update = YES

 DO
  setwait 55
  setkeys YES

  IF st.need_update THEN
   v_new menu, 0
   append_simplemenu_item menu, "Previous Menu...", , , itchmenuEXIT
   append_simplemenu_item menu, " Game package name: " & distinfo.pkgname, YES, uilook(uiDisabledItem)
   append_simplemenu_item menu, "Your itch.io username: " & distinfo.itch_user, , , itchmenuUSER
   append_simplemenu_item menu, "itch.io game name: " & IIF(LEN(distinfo.itch_gamename) = 0, "<defaults to package name>", distinfo.itch_gamename) , , , itchmenuGAMENAME
   IF LEN(distinfo.itch_user) = 0 THEN
    append_simplemenu_item menu, " Type username to estimate url", YES, uilook(uiDisabledItem)
   ELSE
    append_simplemenu_item menu, " " & itch_game_url(distinfo), YES, uilook(uiDisabledItem)
    append_simplemenu_item menu, " butler target = " & itch_target(distinfo), YES, uilook(uiDisabledItem)
   END IF
   IF itch_butler_is_logged_in() THEN
    append_simplemenu_item menu, "Upload to itch.io now", , , itchmenuUPLOAD
   ELSE
    append_simplemenu_item menu, "Set up itch.io butler tool now", , , itchmenuBUTLERSETUP
   END IF
   init_menu_state st, cast(BasicMenuItem vector, menu)
  END IF

  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "upload_game_itch_io"
  IF enter_space_click(st) THEN
   SELECT CASE menu[st.pt].dat
    CASE itchmenuEXIT:
     EXIT DO
    CASE itchmenuBUTLERSETUP:
     itch_butler_setup()
    CASE itchmenuUPLOAD:
     save_current_game 0
     itch_butler_upload(distinfo)
   END SELECT
  END IF

  SELECT CASE menu[st.pt].dat
   CASE itchmenuUSER:
    IF strgrabber(distinfo.itch_user, 63) THEN
     distinfo.itch_user = sanitize_url_chunk(distinfo.itch_user)
     st.need_update = YES
    END IF
   CASE itchmenuGAMENAME:
    IF strgrabber(distinfo.itch_gamename, 63) THEN
     distinfo.itch_gamename = sanitize_url_chunk(distinfo.itch_gamename)
     st.need_update = YES
    END IF
  END SELECT

  usemenu st, cast(BasicMenuItem vector, menu)
  
  clearpage dpage
  standardmenu cast(BasicMenuItem vector, menu), st, 0, 0, dpage
  
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 setkeys
 v_free menu
 save_distrib_state distinfo
END SUB

FUNCTION itch_butler_setup() as bool
 ' Returns NO if setup failed

 DIM butler_path as string = itch_butler_path()
 
 IF NOT itch_butler_is_installed() THEN
  IF NOT itch_butler_download() THEN RETURN NO
 END IF

 'If we got this far, butler should be installed. Try to log in
 IF NOT itch_butler_is_logged_in() THEN
  dist_info "The itch.io butler tool will try to launch your web browser to log in. When finished, you can come back to this window."
  DIM spawn_ret as string = spawn_and_wait(butler_path, "login")
  IF LEN(spawn_ret) > 0 THEN dist_info "ERROR: butler login command reported failure: " & spawn_ret : RETURN NO
  dist_info "After you are finished with the web browser, come back here..."
 END IF
 
 RETURN YES
END FUNCTION

FUNCTION itch_butler_platform_version() as string
 DIM prefix as string
 DIM suffix as string

 #IFNDEF __FB_X86__
 dist_info "Unfortunately, butler is only available for x86 and x86_64."
 RETURN ""
 #ENDIF

 #IFDEF __FB_WIN32__
 prefix = "windows"
 #ELSEIF DEFINED(__FB_DARWIN__)
 prefix = "darwin"
 #IFNDEF __FB_64BIT__
 IF dist_yesno("No butler download for 32-bit Mac is available. Download the 64-bit version instead?") = NO THEN RETURN ""
 #ENDIF
 #ELSEIF DEFINED(__GNU_LINUX__)
 prefix = "linux"
 #ELSE
 dist_info "Unfortunately, butler is only available for Windows, Mac and GNU/Linux."
 RETURN ""
 #ENDIF

 #IF defined(__FB_64BIT__) or defined(__FB_DARWIN__)
 suffix = "amd64"
 #ELSE
 suffix = "386"
 #ENDIF

 RETURN prefix & "-" & suffix
END FUNCTION

'TODO: couldn't this be merged with install_windows_helper_app?
FUNCTION itch_butler_download() as bool
 DIM butler_path as string = itch_butler_path()

 DIM butler_platform as string = itch_butler_platform_version()
 IF butler_platform = "" THEN RETURN NO  'Not available

 '--Ask the user for permission the first time we download (subsequent updates don't ask)
 DIM agree_file as string = get_support_dir() & SLASH & "itch.butler.download.agree"
 IF agreed_to_download(agree_file, "the itch.io butler tool") = NO THEN RETURN NO
 
 DIM destzip as string = get_support_dir() & SLASH & "butler.zip"
 '--Actually download the dang file
 DIM url as string = "https://broth.itch.ovh/butler/" & butler_platform & "/LATEST/archive/default"
 IF NOT download_file(url, destzip) THEN
  dist_info "ERROR: Failed to download itch.io butler.zip" : RETURN NO
 END IF
 
 '--Find the unzip tool
 DIM unzip as string = find_helper_app("unzip", YES)
 IF unzip = "" THEN dist_info "ERROR: Couldn't find unzip tool": RETURN NO

 '-- remove the old copy (if it exists)
 safekill butler_path
 
 '--Unzip the files
 DIM args as string = "-o " & escape_filename(destzip) & " -d " & escape_filename(get_support_dir())
 DIM spawn_ret as string = spawn_and_wait(unzip, args)
 IF LEN(spawn_ret) > 0 THEN dist_info "ERROR: unzip failed: " & spawn_ret : RETURN NO
 
 IF NOT isfile(butler_path) THEN dist_info "ERROR: Failed to unzip itch.io butler tool" : RETURN NO
 
 RETURN YES
END FUNCTION

FUNCTION itch_game_url(distinfo as DistribState) as string
 RETURN "https://" & sanitize_url_chunk(distinfo.itch_user) & ".itch.io/" & itch_gametarg(distinfo)
END FUNCTION

FUNCTION itch_target(distinfo as DistribState) as string
 RETURN sanitize_url_chunk(distinfo.itch_user) & "/" & itch_gametarg(distinfo)
END FUNCTION

FUNCTION itch_gametarg(distinfo as DistribState) as string
 RETURN sanitize_url_chunk(IIF(LEN(distinfo.itch_gamename) = 0, distinfo.pkgname, distinfo.itch_gamename))
END FUNCTION

'TODO: this could be replaced with find_helder_app, why treat butler differently from all others?!
FUNCTION itch_butler_path() as string
 RETURN get_support_dir() & SLASH & "butler" & DOTEXE
END FUNCTION

FUNCTION itch_butler_is_installed() as bool
 DIM butler as string = itch_butler_path()
 IF butler = "" THEN RETURN NO
 RETURN isfile(butler)
END FUNCTION

FUNCTION itch_butler_is_logged_in() as bool
 ' Can't be logged in if it ain't installed
 IF NOT itch_butler_is_installed() THEN RETURN NO
 DIM butler_creds as string
 #IFDEF __FB_WIN32__
 butler_creds = ENVIRON("USERPROFILE") & "\.config\itch\butler_creds"
 #ELSEIF DEFINED(__FB_DARWIN__)
 butler_creds = ENVIRON("HOME") & "/Library/Application Support/itch/butler_creds"
 #ELSE
 butler_creds = ENVIRON("HOME") & "/.config/itch/butler_creds"
 #ENDIF
 RETURN isfile(butler_creds)
END FUNCTION

FUNCTION itch_butler_error_check(out_s as string, err_s as string) as bool
 'Returns YES if there is an error
 'Generic error checking for butler calls
 'Special error checking may also be required separately
 DIM ret as bool = NO
 IF LEN(err_s) > 0 THEN
  dist_info !"butler status error:\n" & err_s
  ret = YES
 END IF
 IF INSTR(LCASE(out_s), " api error ") THEN
  dist_info !"butler api error\n" & out_s
  ret = YES
 END IF
 RETURN ret
END FUNCTION

SUB itch_butler_upload(distinfo as DistribState)
 debuginfo "itch_butler_upload"
 dist_basicstatus "Checking butler status..."

 DIM butler as string = itch_butler_path()
 DIM target as string = itch_target(distinfo)
 DIM out_s as string
 DIM err_s as string
 run_and_get_output butler & " status " & target, out_s, err_s
 IF INSTR(LCASE(out_s), "invalid game") THEN
  dist_info "The target game " & target & " doesn't exist yet. Create a new game named """ & itch_gametarg(distinfo) & """ on your itch.io account. That is where you can add screenshots and a description. Then come back here and try again."
  EXIT SUB
 END IF
 IF itch_butler_error_check(out_s, err_s) THEN EXIT SUB

 IF dist_yesno("This will replace the older version of this game at " & itch_game_url(distinfo) & ", are you sure you want to continue?", NO) = NO THEN EXIT SUB

 DIM itch_temp_dir as string = CURDIR & SLASH & "_itch_io_exports.tmp"
 makedir itch_temp_dir
 debuginfo "Exporting files to " & itch_temp_dir
 auto_choose_default = YES
 dist_basicstatus "Exporting for Windows..."
 distribute_game_as_zip itch_temp_dir
 dist_basicstatus "Exporting for Mac..."
 distribute_game_as_mac_app "x86_64", itch_temp_dir
 dist_basicstatus "Exporting for Linux..."
 distribute_game_as_linux_tarball "x86_64", itch_temp_dir
 auto_choose_default = NO

 dist_basicstatus "Verifying exported files..."
 DIM win_zip as string = itch_temp_dir & SLASH & distinfo.pkgname & ".zip"
 DIM mac_app as string = itch_temp_dir & SLASH & distinfo.pkgname & "-mac.zip"
 DIM linux_tarball as string = itch_temp_dir & SLASH & distinfo.pkgname & "-linux-x86_64.tar.gz"
 err_s = ""
 IF NOT isfile(win_zip) THEN err_s &= !"\nWindows zip failed!"
 IF NOT isfile(mac_app) THEN err_s &= !"\nMacOS App failed!"
 IF NOT isfile(linux_tarball) THEN err_s &= !"\nLinux tarball failed!"
 IF err_s <> "" THEN
  dist_info !"Some files failed to export:" & err_s
  EXIT SUB
 END IF
 
 debuginfo "Upload exports with butler..."

 dist_basicstatus "Upload " & itch_gametarg(distinfo) & " (Windows) ..."
 run_and_get_output butler & " push " & escape_filename(win_zip) & " " & target & ":windows", out_s, err_s
 IF itch_butler_error_check(out_s, err_s) THEN EXIT SUB

 dist_basicstatus "Upload " & itch_gametarg(distinfo) & " (Mac) ..."
 run_and_get_output butler & " push " & escape_filename(mac_app) & " " & target & ":mac", out_s, err_s
 IF itch_butler_error_check(out_s, err_s) THEN EXIT SUB

 dist_basicstatus "Upload " & itch_gametarg(distinfo) & " (Linux) ..."
 run_and_get_output butler & " push " & escape_filename(linux_tarball) & " " & target & ":linux", out_s, err_s
 IF itch_butler_error_check(out_s, err_s) THEN EXIT SUB

 dist_basicstatus "Cleaning up temp files..."
 killdir itch_temp_dir

 dist_info "Upload of " & itch_gametarg(distinfo) & " to " & itch_game_url(distinfo) & " finished (Windows, Mac, Linux)"

END SUB


