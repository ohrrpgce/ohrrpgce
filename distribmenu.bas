'OHRRPGCE CUSTOM - Distribute Game menu
'(C) Copyright 1997-2023 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
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
#include "editorkit.bi"

TYPE FnGatherFiles as FUNCTION (build_variant as string, basename as string, destdir as string, distinfo as DistribState) as bool

DECLARE FUNCTION distribute_game_as_windows_zip (dest_override as string = "") as string
DECLARE FUNCTION distribute_game_as_windows_installer (dest_override as string = "") as string
DECLARE FUNCTION distribute_game_as_linux_tarball (which_arch as string, dest_override as string = "") as string
DECLARE FUNCTION distribute_game_as_web_zip (dest_override as string = "") as string
DECLARE FUNCTION gather_common_files (basename as string, destdir as string, distinfo as DistribState, newline as string) as bool
DECLARE FUNCTION gather_files_for_windows (buildname as string, basename as string, destdir as string, distinfo as DistribState) as bool
DECLARE FUNCTION gather_files_for_linux (which_arch as string, basename as string, destdir as string, distinfo as DistribState) as bool
DECLARE FUNCTION gather_files_for_mac (which_arch as string, basename as string, destdir as string, distinfo as DistribState) as bool
DECLARE FUNCTION gather_files_for_web (buildname as string, basename as string, destdir as string, distinfo as DistribState) as bool
DECLARE FUNCTION add_windows_gameplayer(basename as string, destdir as string) as string
DECLARE FUNCTION add_linux_gameplayer(which_arch as string, basename as string, destdir as string) as string
DECLARE FUNCTION add_mac_gameplayer(which_arch as string, basename as string, destdir as string) as string
DECLARE FUNCTION add_web_gameplayer(basename as string, destdir as string) as string
DECLARE FUNCTION package_game (build_variant as string, byref destname as string, dest_override as string = "", use_subdir as bool, gather_files as FnGatherFiles, byref basename as string = "") as string
DECLARE FUNCTION agreed_to_download(agree_file as string, description as string) as bool
DECLARE FUNCTION download_gameplayer_if_needed(url as string, destfile as string, agree_filename as string, description as string) as bool
DECLARE FUNCTION sanity_check_buildinfo(buildinfo_file as string) as bool
DECLARE FUNCTION valid_arch(which_arch as string, options as string) as bool
DECLARE FUNCTION dist_prompt_overwrite_file(destname as string) as bool
DECLARE FUNCTION dist_make_temp_dir(dirname as string = "package") as string
DECLARE FUNCTION gameplayer_dir() as string
DECLARE FUNCTION find_or_download_innosetup () as string
DECLARE FUNCTION find_innosetup () as string
DECLARE FUNCTION win_or_wine_drive(letter as string) as string
DECLARE FUNCTION win_or_wine_spawn_and_wait (cmd as string, args as string="") as string
DECLARE FUNCTION write_innosetup_script (basename as string, gamename as string, isstmp as string) as string
DECLARE FUNCTION win_path (filename as string) as string
DECLARE FUNCTION copy_or_relump (src_rpg_or_rpgdir as string, dest_rpg as string) as bool
DECLARE FUNCTION find_and_copy_windows_gameplayer (basename as string, destdir as string) as bool
DECLARE SUB insert_windows_exe_icon (exe_name as string, ico_name as string)
DECLARE SUB needed_windows_libs(gameplayer as string, byref files as string vector, buildinfo() as string)
DECLARE FUNCTION copy_linux_gameplayer (gameplayer as string, basename as string, destdir as string) as bool
DECLARE FUNCTION distribute_game_as_debian_package (which_arch as string, dest_override as string = "") as string
DECLARE FUNCTION get_debian_package_version() as string
DECLARE FUNCTION get_debian_package_name() as string
DECLARE SUB write_linux_menu_file(title as string, filename as string, basename as string)
DECLARE SUB write_linux_desktop_file(title as string, filename as string, basename as string)
DECLARE SUB write_debian_binary_file (filename as string)
DECLARE SUB write_debian_control_file(controlfile as string, basename as string, pkgver as string, size_in_kibibytes as integer, byref distinfo as DistribState, deb_arch as string)
DECLARE SUB write_debian_copyright_file (filename as string, license_binary as string)
DECLARE FUNCTION gzip_file (filename as string) as bool
DECLARE FUNCTION gunzip_file (filename as string) as bool
DECLARE FUNCTION create_zipfile(zipfile as string, start_in_dir as string, files as string) as bool
DECLARE FUNCTION extract_zipfile(zipfile as string, into_dir as string) as bool
DECLARE FUNCTION create_tarball(tarball as string, start_in_dir as string, files as string) as bool
DECLARE FUNCTION extract_tarball(tarball as string, into_dir as string, files as string = "") as bool
DECLARE FUNCTION create_ar_archive(start_in_dir as string, archive as string, files as string) as bool
DECLARE FUNCTION create_win_installer(installer as string, isstmp as string, basename as string, distinfo as DistribState) as bool
DECLARE SUB fix_deb_group_permissions(start_at_dir as string)
DECLARE SUB write_debian_postrm_script (filename as string)
DECLARE SUB write_debian_postinst_script (filename as string)
DECLARE FUNCTION dist_find_helper_app(appname as string) as string
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
DECLARE FUNCTION get_game_license_text_file() as string
DECLARE SUB maybe_write_license_text_file (filename as string)
DECLARE FUNCTION is_known_license(license_code as string) as bool
DECLARE FUNCTION generate_copyright_line(distinfo as DistribState) as string
DECLARE FUNCTION distribute_game_as_mac_app (which_arch as string, dest_override as string = "") as string
DECLARE FUNCTION fix_mac_app_executable_bit_on_windows(zipfile as string, exec_path_in_zip as string) as bool
DECLARE SUB dist_basicstatus (s as string)
DECLARE FUNCTION extract_web_data_files(js_file as string, data_file as string, output_dir as string) as bool
DECLARE FUNCTION recreate_web_data_file(js_file as string, data_file as string, from_dir as string) as bool
DECLARE FUNCTION web_data_cleanup(output_dir as string) as bool

DECLARE SUB itch_io_options_menu()
DECLARE FUNCTION itch_game_url(distinfo as DistribState) as string
DECLARE FUNCTION itch_target(distinfo as DistribState) as string
DECLARE FUNCTION itch_gametarg(distinfo as DistribState) as string
DECLARE FUNCTION itch_butler_is_logged_in() as bool
DECLARE FUNCTION itch_butler_setup() as bool
DECLARE FUNCTION itch_butler_download() as bool
DECLARE SUB itch_butler_upload(distinfo as DistribState)
DECLARE FUNCTION itch_butler_error_check(out_s as string, err_s as string) as bool

DECLARE FUNCTION steamworks_zip() as string
DECLARE SUB download_steamworks()
DECLARE FUNCTION add_steamworks_lib(libname as string, outdir as string) as string
DECLARE FUNCTION distribute_game_for_steam_linux (which_arch as string) as string
DECLARE FUNCTION distribute_game_for_steam_windows () as string

DECLARE FUNCTION dist_yesno(capt as string, byval defaultval as bool=YES, byval escval as bool=NO) as bool
DECLARE SUB dist_info (msg as zstring ptr, errlvl as errorLevelEnum = errDebug)
DIM SHARED auto_choose_default as bool = NO

'#################################### Menu #####################################

TYPE DistribMenu EXTENDS EditorKit
 known_licenses(9) as StringEnumOption = {(@"COPYRIGHT"), (@"PUBLICDOMAIN"), (@"GPL"), (@"MIT"), (@"CC-BY"), (@"CC-BY-SA"), (@"CC-BY-ND"), (@"CC-BY-NC"), (@"CC-BY-NC-SA"), (@"CC-BY-NC-ND")}

 distinfo as DistribState
 tools_for_mac as bool
 tools_for_win_installer as bool
 tools_for_linux as bool
 tools_for_debian as bool
 butler_logged_in as bool
 have_steamworks_libs as bool

 DECLARE SUB refresh_tools()
 DECLARE SUB def_dist_str(title as zstring ptr, byref datum as string, helpkey_suffix as string, multline_hint as bool = NO)
 DECLARE FUNCTION def_steam_export(title as zstring ptr) as bool

 DECLARE SUB toplevel_menu()
 DECLARE SUB distinfo_menu()
 DECLARE SUB itch_io_menu()
 DECLARE SUB steam_menu()
 DECLARE SUB contents_menu()
 DECLARE VIRTUAL SUB define_items()
END TYPE

SUB DistribMenu.refresh_tools()
 tools_for_mac = can_make_mac_packages()
 tools_for_win_installer = can_run_windows_exes()
 tools_for_linux = can_make_tarballs()
 tools_for_debian = can_make_debian_packages()
END SUB

#DEFINE presave  save_current_game 0

SUB DistribMenu.toplevel_menu()
 helpkey = "distribute_game"

 IF defitem_act("Edit distribution info...") THEN enter_submenu "distinfo"
 IF defitem_act("Customize package contents...") THEN enter_submenu "contents"

 IF defitem_act("Export README text file") THEN export_readme_text_file

 #IFDEF MINIMAL_OS

  defunselectable "Packaging is unsupported on this platform"
  defunselectable "Download a desktop copy of the OHRRPGCE."

 #ELSE

 IF defitem_act("Export Windows .zip") THEN presave : distribute_game_as_windows_zip

 IF tools_for_win_installer THEN
  IF defitem_act("Export Windows Installer") THEN presave : distribute_game_as_windows_installer
 ELSE
  'Unlike others, can't autoinstall the needed tools
  defdisabled "Can't Export Windows Installer"
  defunselectable " (requires Windows or wine)"
 END IF

 IF defitem_act("Export Mac OS X App Bundle (64bit)") THEN presave : distribute_game_as_mac_app "x86_64"
 IF NOT tools_for_mac THEN
  defunselectable " (requires tar+gzip)"
 END IF

 IF defitem_act("Export Linux Tarball (64bit)") THEN presave : distribute_game_as_linux_tarball "x86_64"
 IF NOT tools_for_linux THEN
  defunselectable " (requires tar+gzip)"
 END IF

 IF defitem_act("Export Debian Linux Package (64bit)") THEN presave : distribute_game_as_debian_package "x86_64"
 IF NOT tools_for_debian THEN
  defunselectable " (requires ar+tar+gzip)"
 END IF

 IF defitem_act("Export Web Browser .zip") THEN presave : distribute_game_as_web_zip

 IF defitem_act("Upload this game to itch.io...") THEN enter_submenu "itch_io"
 IF defitem_act("Package for Steam...") THEN enter_submenu "steam"

 section "Obsolete targets"

 IF defitem_act("Export Mac OS X App Bundle (old 32bit Macs)") THEN presave : distribute_game_as_mac_app "x86"
 IF NOT tools_for_mac THEN
  defunselectable " (requires tar+gzip)"
 END IF

 IF defitem_act("Export Linux Tarball (32bit)") THEN presave : distribute_game_as_linux_tarball "x86"
 IF NOT tools_for_linux THEN
  defunselectable " (requires tar+gzip)"
 END IF

 IF defitem_act("Export Debian Linux Package (32bit)") THEN presave : distribute_game_as_debian_package "x86"
 IF NOT tools_for_debian THEN
  defunselectable " (requires ar+tar+gzip)"
 END IF

 #ENDIF

 spacer
 defunselectable " Game file: " & decode_filename(trimpath(sourcerpg))

 DIM iconbase as string = trimextension(sourcerpg)
 DIM iconname as string = decode_filename(trimpath(iconbase))
 defunselectable " Windows icon: " & iconname & ".ico"
 IF isfile(iconbase & ".ico") = NO THEN set_caption "not found" : set_disabled
 defunselectable " Mac icon: " & iconname & ".icns"
 IF isfile(iconbase & ".icns") = NO THEN set_caption "not found" : set_disabled

 'Any of the distribute options might have downloaded a tool
 IF want_activate AND NOT want_exit THEN refresh_tools
END SUB

SUB DistribMenu.def_dist_str(title as zstring ptr, byref datum as string, helpkey_suffix as string, multline_hint as bool = NO)
 defstr title, datum
 set_helpkey "edit_distrib_info_" & helpkey_suffix
 IF multline_hint THEN set_tooltip "Press ENTER to edit multiple lines"
 multiline_editable
 IF edited THEN
  sanitize_distinfo distinfo
  dont_write
 END IF
END SUB

SUB DistribMenu.distinfo_menu ()
 helpkey = "edit_distrib_info"

 def_dist_str "Package name:",     distinfo.pkgname, "pkgname"
 def_dist_str "Game name:",        distinfo.gamename, "gamename"
 def_dist_str "Author:",           distinfo.author, "author"
 def_dist_str "Email:",            distinfo.email, "email"
 def_dist_str "Description:",      distinfo.description, "description", YES
 def_dist_str "More Description:", distinfo.more_description, "more_description", YES
 def_dist_str "Website:",          distinfo.website, "website"
 def_dist_str "Copyright year:",   distinfo.copyright_year, "copyright_year"
 set_tooltip generate_copyright_line(distinfo)

 defitem "License:"
 edit_str_enum distinfo.license, known_licenses()
 set_helpkey "edit_distrib_info_license"
 set_tooltip generate_copyright_line(distinfo)

 'Save when exiting this submenu
 IF want_exit THEN save_distrib_state distinfo
END SUB

'Check, normalise, and add a file/dir to the extra_files list
FUNCTION add_extra_file(path as string, distinfo as DistribState) as bool
 IF LEN(path) = 0 THEN RETURN NO  'Cancelled browse()

 path = simplify_path_further(path)
 #IFDEF __FB_WIN32__
  'Unix path separators for better portability
  replacestr path, "\", "/"
 #ENDIF

 IF isdir(path) THEN
  DIM file_count as integer
  count_directory_size(path, file_count, 1000)
  IF file_count >= 1000 THEN
   dist_info path & " contains over 1000 files, refusing to add it.", errInfo
   RETURN NO
  END IF
 ELSEIF isfile(path) THEN
 ELSE
  RETURN NO
 END IF

 a_append distinfo.extra_files(), path
 RETURN YES
END FUNCTION

SUB DistribMenu.contents_menu ()
 helpkey = "edit_distrib_contents"

 STATIC license_file as string
 IF refresh THEN license_file = get_game_license_text_file()

 section "Packages will include:"
 defunselectable distinfo.pkgname & ".rpg"
 set_caption " (Always, except Mac)"
 defunselectable distinfo.pkgname & ".exe/" & distinfo.pkgname & ".app/" & distinfo.pkgname
 set_caption " (Win/Mac/Linux)"
 defunselectable "(Software libraries)"
 set_caption " (As needed)"
 defunselectable "LICENSE-binary.txt"
 set_caption " (Always)"

 IF license_file <> "" THEN
  defbool "LICENSE.txt  (" & distinfo.license & "):", distinfo.omit_license
  captions_bool "Include", "Omit"
 END IF

 defbool "README-" & distinfo.pkgname & ".txt  (Autogenerated):", distinfo.omit_readme
 captions_bool "Include", "Omit"

 subsection " Custom extra files:"

 FOR idx as integer = 0 TO UBOUND(distinfo.extra_files)
  DIM path as string = distinfo.extra_files(idx)
  DIM info as string = ""
  defitem path

  IF refresh THEN  'Can't do this every frame
   SELECT CASE get_file_type(path)
    CASE fileTypeFile
     info = " (" & filesize(path) & ")"
    CASE fileTypeDirectory
     DIM file_count as integer
     DIM file_size as integer = count_directory_size(path, file_count, 1000)
     info = " (" & file_count & " files, " & format_filesize(file_size) & ")"
    CASE fileTypeNonexistent
     set_caption " (MISSING)"
     set_disabled
    CASE fileTypeError
     set_caption " (UNREADABLE)"
     set_disabled
    CASE ELSE  'fileTypeOther (e.g. symlink)
     set_caption " (INVALID)"
     set_disabled
   END SELECT
  END IF

  DIM pathinfo as string
  IF trimfilename(path) <> "" THEN pathinfo = path & " -> " & trimpath(path) & !"\n"
  set_tooltip pathinfo & "Press DELETE to remove"

  IF delete_action() THEN
   a_pop distinfo.extra_files(), idx
   edited = YES
   EXIT FOR
  END IF

  SELECT CASE get_file_type(path)
   CASE fileTypeFile, fileTypeDirectory
    defunselectable info
  END SELECT
 NEXT

 IF defitem_act("Add file...") THEN
  IF add_extra_file(browse(browseAny, , "*", "browse_for_distrib_extra"), distinfo) THEN edited = YES
 END IF

 IF defitem_act("Add directory...") THEN
  IF add_extra_file(browse(browseDir, , , "browse_for_folder"), distinfo) THEN edited = YES
 END IF

 IF want_exit THEN save_distrib_state distinfo
END SUB

SUB DistribMenu.itch_io_menu ()
 helpkey = "distribute_game_itch_io"

 defstr "Your itch.io username:", distinfo.itch_user, 63
 IF edited THEN valuestr = sanitize_url_chunk(valuestr)

 defstr "itch.io game name:", distinfo.itch_gamename, 63
 caption_default_or_str "<defaults to package name>"
 IF edited THEN valuestr = sanitize_url_chunk(valuestr)

 IF butler_logged_in THEN
  IF defitem_act("Upload to itch.io now") THEN presave : itch_butler_upload(distinfo)
 ELSE
  IF defitem_act("Set up itch.io butler tool now") THEN
   itch_butler_setup()
   butler_logged_in = itch_butler_is_logged_in()
  END IF
 END IF

 spacer
 defunselectable " Game package name: " & distinfo.pkgname
 IF LEN(distinfo.itch_user) = 0 THEN
  defunselectable " Enter username to estimate URL"
 ELSE
  defunselectable " " & itch_game_url(distinfo)
  defunselectable " butler target: " & itch_target(distinfo)
 END IF

 IF want_exit THEN save_distrib_state distinfo
END SUB

FUNCTION DistribMenu.def_steam_export(title as zstring ptr) as bool
 defitem title
 IF distinfo.steam_appid = 0 ORELSE have_steamworks_libs = NO THEN set_disabled
 RETURN activate
END FUNCTION

SUB DistribMenu.steam_menu()
 helpkey = "distribute_game_steam"

 defint "Steam appid:", distinfo.steam_appid, 0, INT_MAX
 IF edited THEN save_distrib_state distinfo
 set_helpkey "edit_distrib_info_steam_appid"

 IF def_steam_export("Export Windows build for Steam") THEN presave : distribute_game_for_steam_windows
 IF def_steam_export("Export Linux 64-bit build for Steam") THEN presave : distribute_game_for_steam_linux "x86_64"
 IF def_steam_export("Export Linux 32-bit (obsolete) build for Steam") THEN presave : distribute_game_for_steam_linux "x86"

 IF distinfo.steam_appid = 0 THEN
  defunselectable " (Missing appid)"
 END IF
 IF have_steamworks_libs = NO THEN
  defunselectable " (Missing steamworks)"
  IF defitem_act("Download Steamworks libraries...") THEN
   download_steamworks()
   have_steamworks_libs = isfile(steamworks_zip())
  END IF
 END IF
END SUB

SUB DistribMenu.define_items()
 SELECT CASE submenu
  CASE "": toplevel_menu
  CASE "distinfo": distinfo_menu
  CASE "contents": contents_menu
  CASE "itch_io": itch_io_menu
  CASE "steam": steam_menu
 END SELECT
END SUB

SUB distribute_game_menu ()
 DIM menu as DistribMenu
 load_distrib_state menu.distinfo
 menu.refresh_tools()
 menu.butler_logged_in = itch_butler_is_logged_in()
 menu.have_steamworks_libs = isfile(steamworks_zip())
 menu.run()
 'Submenus already save menu.distinfo when leaving
 'Revert genCurrentDebugMode to the author's choice in genDebugMode... I think this is only needed
 'if using Test Game at the same time
 gen(genCurrentDebugMode) = gen(genDebugMode)
END SUB

'################################################################################

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
 
 IF dist_prompt_overwrite_file(txtfile) = NO THEN RETURN

 write_readme_text_file txtfile, LE
 dist_info "Created " & trimpath(txtfile), errInfo
 
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

 write_file filename, s
END SUB

FUNCTION get_game_license_text_file() as string
 DIM distinfo as DistribState
 load_distrib_state distinfo

 IF distinfo.license = "COPYRIGHT" THEN RETURN ""
 IF distinfo.license = "PUBLICDOMAIN" THEN RETURN ""

 DIM helpdir as string = get_help_dir()
 DIM lic_file as string = helpdir & SLASH & "license_" & LCASE(distinfo.license) & ".txt"

 IF isfile(lic_file) THEN RETURN lic_file
 dist_info lic_file & " does not exist"
 RETURN ""
END FUNCTION

' For some types of licenses, include a text copy
SUB maybe_write_license_text_file (filename as string)

 DIM lic_file as string = get_game_license_text_file()
 IF lic_file <> "" THEN writeablecopyfile lic_file, filename
END SUB

' Create /usr/share/doc/$basename/copyright file according to requirements
' https://www.debian.org/doc/debian-policy/ch-docs.html#copyright-information
SUB write_debian_copyright_file (filename as string, license_binary as string)

 DIM LF as string = CHR(10)

 DIM distinfo as DistribState
 load_distrib_state distinfo

 '--Construct the file
 DIM s as string = ""
 
 s &= distinfo.gamename & LF
 s &= generate_copyright_line(distinfo) & LF
 IF distinfo.license = "GPL" THEN
  s &= LF & "See /usr/share/common-licenses/GPL-3 for license terms" & LF
 ELSE
  DIM lic_file as string = get_game_license_text_file()
  IF lic_file <> "" THEN
   s &= LF & string_from_file(lic_file)  'Normalises to Unix lineendings
  END IF
 END IF

 s &= LF & "The license governing the executable follows:" & LF
 s &= "=============================================================================" & LF
 s &= string_from_file(license_binary)

 write_file filename, s
END SUB

FUNCTION is_known_license(license_code as string) as bool
 'duplicated known_licenses because global string arrays are a pain in the ass
 DIM known_licenses(9) as string = {"COPYRIGHT", "PUBLICDOMAIN", "GPL", "MIT", "CC-BY", "CC-BY-SA", "CC-BY-ND", "CC-BY-NC", "CC-BY-NC-SA", "CC-BY-NC-ND"}
 RETURN a_find(known_licenses(), license_code) > -1
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


' Check which_arch is in options.
' options should be a comma delimited string like "x86, x86_64"
FUNCTION valid_arch(which_arch as string, options as string) as bool
 ' Prevent x86 from matching against x86_64
 IF INSTR(options & ",", which_arch & ",") THEN RETURN YES
 dist_info "Unknown arch """ & which_arch & """. The only supported values for this target are " & options
END FUNCTION

FUNCTION dist_prompt_overwrite_file(destname as string) as bool
 IF isfile(destname) THEN
  IF dist_yesno(decode_filename(trimpath(destname)) & " already exists. Overwrite it?") = NO THEN RETURN NO
  'Okay to overwrite! (but actually do the overwriting later on)
 END IF
 RETURN YES
END FUNCTION

FUNCTION dist_make_temp_dir(dirname as string = "package") as string
 DIM pkgtmp as string = trimfilename(sourcerpg) & SLASH & dirname & ".tmp"
 IF isdir(pkgtmp) THEN
  killdir pkgtmp, YES
 END IF
 IF makedir(pkgtmp) THEN
  dist_info "ERROR: unable to create temporary folder " & pkgtmp
  RETURN ""
 END IF
 RETURN pkgtmp
END FUNCTION

'Find/create the folder that we are going to download ohrrpgce-player-* archives into
FUNCTION gameplayer_dir() as string
 DIM dldir as string = settings_dir & SLASH & "_gameplayer"
 IF NOT isdir(dldir) THEN
  IF makedir(dldir) THEN
   dist_info "ERROR: Unable to create """ & dldir & """ directory"
   RETURN ""
  END IF
 END IF
 RETURN dldir
END FUNCTION

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

'Run game to extract its embedded buildinfo.ini into destdir
FUNCTION extract_buildinfo(gameplayer as string, destdir as string) as bool
 DIM spawn_ret as string
 DIM olddir as string = CURDIR
 CHDIR destdir
 safekill "buildinfo.ini"
 spawn_ret = spawn_and_wait(gameplayer, "--dump-embed buildinfo.ini")
 DIM exists as bool = isfile("buildinfo.ini")
 CHDIR olddir

 IF LEN(spawn_ret) THEN dist_info spawn_ret
 IF exists = NO THEN dist_info "ERROR: Couldn't extract buildinfo.ini from game.exe"
 RETURN exists
END FUNCTION

#IFDEF __FB_WIN32__
FUNCTION find_and_copy_windows_gameplayer (basename as string, destdir as string) as bool
 'This is used only when not using a downloaded Windows build (on Windows)
 'Returns true on success, false on failure

 DIM game_exe as string = exepath & SLASH & "game.exe"
 IF isfile(game_exe) = NO THEN
  dist_info "game.exe wasn't found in the same folder as custom.exe. Downloading instead."
  RETURN NO
 END IF

 REDIM buildinfo() as string
 IF extract_buildinfo(game_exe, destdir) THEN
  lines_from_file buildinfo(), destdir & SLASH & "buildinfo.ini"
  safekill destdir & SLASH & "buildinfo.ini"
  debuginfo "Found game.exe, version: " & read_ini_str(buildinfo(), "long_version")
 END IF

 'Check buildname
 DIM buildname as string
 buildname = read_ini_str(buildinfo(), "buildname", "unknown (obsolete version?)")
 IF buildname <> "sdl2" THEN
  IF buildname = "" THEN buildname = "blank (unofficial build)"
  IF dist_yesno("ERROR: game.exe next to custom.exe doesn't appear to be a default (sdl2) official build (its buildname is " & buildname & "). Download an official build instead? (Recommended)") THEN RETURN NO
 END IF

 'Vector of all files to copy
 DIM otherf as string vector
 v_new otherf
 v_append otherf, "game.exe"
 v_append otherf, "LICENSE-binary.txt"
 needed_windows_libs game_exe, otherf, buildinfo()

 FOR i as integer = 0 TO v_len(otherf) - 1
  DIM dest_fname as string = otherf[i]
  IF otherf[i] = "game.exe" THEN dest_fname = basename & ".exe"
  IF confirmed_copy(exepath & SLASH & otherf[i], destdir & SLASH & dest_fname) = NO THEN
   v_free otherf
   dist_info otherf[i] & " missing/unreadable. Using your existing game.exe failed, so downloading instead."
   RETURN NO
  END IF
 NEXT i

 v_free otherf
 RETURN YES
END FUNCTION
#ENDIF

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

#IFDEF __FB_WIN32__
SUB needed_windows_libs(gameplayer as string, byref files as string vector, buildinfo() as string)
 'Add .dlls to files needed by the gfx/music backends in buildinfo(). This is
 'used only when not using a downloaded ohrrpgce-player-* zip (on Windows).
 'Matches needed_windows_libs in ohrpackage.py.
 DIM as string gfx(), music()

 split read_ini_str(buildinfo(), "gfx", "sdl2"), gfx()
 FOR idx as integer = 0 TO UBOUND(gfx)
  SELECT CASE gfx(idx)
   CASE "directx":  v_append_once files, "gfx_directx.dll"
   CASE "sdl":      v_append_once files, "SDL.dll"
   CASE "sdl2":     v_append_once files, "SDL2.dll"
   CASE "alleg":    v_append_once files, "alleg40.dll"
   CASE "fb":       'None
  END SELECT
 NEXT

 split read_ini_str(buildinfo(), "music", "sdl2"), music()
 FOR idx as integer = 0 TO UBOUND(music)
  SELECT CASE music(idx)
   CASE "sdl":
    v_append_once files, "SDL.dll"
    v_append_once files, "SDL_mixer.dll"
   CASE "sdl2":
    v_append_once files, "SDL2.dll"
    v_append_once files, "SDL2_mixer.dll"
   CASE "native", "native2":
    v_append_once files, "audiere.dll"
   CASE "allegro":
    v_append_once files, "alleg40.dll"
   CASE "silence":
    'None
  END SELECT
 NEXT
END SUB
#ENDIF

FUNCTION copy_linux_gameplayer (gameplayer as string, basename as string, destdir as string) as bool
 'Used only for debian. Returns true on success.
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


FUNCTION add_windows_gameplayer(basename as string, destdir as string) as string
 'Add game.exe and all other files for the Windows game player (including LICENSE-binary.txt) in destdir.
 'On Windows, try to copy game.exe and libraries. Otherwise, download an ohrrpgce-player package.
 'game.exe is renamed to $basename.exe, and its full path is returned.
 'Returns "" for failure.

 DIM destexe as string = destdir & SLASH & basename & ".exe"

#IFDEF __FB_WIN32__
 IF find_and_copy_windows_gameplayer(basename, destdir) THEN RETURN destexe
#ENDIF

 DIM dldir as string = gameplayer_dir()
 IF dldir = "" THEN RETURN ""
  
 '--Decide which url to download
 DIM url as string
 DIM dlfile as string
 IF version_branch = "wip" THEN
  '--If running a nightly wip, download the default build of the latest nightly wip
  '--(Before 2020-11-17 nightlies downloaded ohrrpgce-player-win-wip.zip instead,
  '--which was assumed to be gfx_directx+sdl[+fb], music_sdl)
  '--(Was for a while in 2022-23 called ohrrpgce-player-win-sdl2-wip.zip)
  dlfile = "ohrrpgce-player-win-wip-sdl2.zip"
  url = "http://hamsterrepublic.com/ohrrpgce/nightly/" & dlfile
 ELSE
  '--Use this stable release
  '--(Previously, always downloaded the latest stable release from
  '--http://hamsterrepublic.com/dl/
  '-- gorgonzola and older downloaded ohrrpgce-player-win.zip (a music_sdl build)
  '-- hróðvitnir downloaded ohrrpgce-player-win-minimal-sdl2.zip
  '--see web/ohrstable.sh for info on server symlinks)

  dlfile = "ohrrpgce-player-win" & version_release_tag & "-sdl2.zip"
  url = "http://hamsterrepublic.com/ohrrpgce/archive/" & dlfile
 END IF

 DIM destzip as string = dldir & SLASH & dlfile

 '--Prompt & download if missing or out of date
 IF NOT download_gameplayer_if_needed(url, destzip, "win.download.agree", "the Windows OHRRPGCE game player") THEN RETURN ""
 
 IF extract_zipfile(destzip, destdir) = NO THEN RETURN ""
 
 'Sanity check contents
 IF sanity_check_buildinfo(destdir & SLASH & "buildinfo.ini") = NO THEN RETURN ""
 IF NOT isfile(destdir & SLASH & "game.exe") THEN dist_info "ERROR: Failed to unzip game.exe" : RETURN ""

 IF renamefile(destdir & SLASH & "game.exe", destexe) = NO THEN dist_info "Couldn't rename game.exe" : RETURN ""
 RETURN destexe
END FUNCTION

FUNCTION sanity_check_buildinfo(buildinfo_file as string) as bool
 'Return YES if the sanity check has passed
 DIM ver as integer = read_ini_int(buildinfo_file, "packaging_version", 0)
 IF ver > 1 THEN
  dist_info "ERROR: The buildinfo version in this downloaded game player is " & ver & " but your copy of the OHRRPGCE only supports version 1. This means you should upgrade to a newer version if you want to keep using the Distribute Game feature.": RETURN NO
 ELSEIF ver < 1 THEN  'Including a missing file
  dist_info "ERROR: The buildinfo version in this downloaded game player is missing or invalid. Please report this as a bug to the OHRRPGCE developers": RETURN NO
 END IF
 RETURN YES
END FUNCTION

FUNCTION add_linux_gameplayer(which_arch as string, basename as string, destdir as string) as string
 'Download a precompiled Linux player package,
 'extract it to destdir (creating it but not clearing if already existing),
 'and return the full path to game.sh therein.
 '
 'Returns "" for failure.

 DIM destexe as string = destdir & SLASH & basename

 /'
 'Don't try to use a local build, it's too much trouble to gather up and check all the necessary files,
 'which won't even exist if installed from a .deb or "scons install".
 'This old code needs update.
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
    IF copy_linux_gameplayer(installed_player, "ohrrpgce-game", destdir) THEN RETURN destdir & "/ohrrpgce-game"
   ELSE
    debuginfo installed_player & " isn't right arch or not a portable build, don't use it for distribute"
   END IF
  ELSE
   debuginfo "Couldn't run " & installed_player
  END IF
 END IF

#ENDIF
 '/

 DIM dldir as string = gameplayer_dir()
 IF dldir = "" THEN RETURN ""
  
 '--Decide which url to download
 DIM url as string
 DIM dlfile as string
 IF version_branch = "wip" THEN
  'If using any wip release, get the latest wip release
  dlfile = "ohrrpgce-player-linux-wip-" & which_arch & ".zip"
  url = "http://hamsterrepublic.com/ohrrpgce/nightly/" & dlfile
 ELSE
  'Use this stable release
  dlfile = "ohrrpgce-player-linux" & version_release_tag & "-" & which_arch & ".zip"
  url = "http://hamsterrepublic.com/ohrrpgce/archive/" & dlfile
 END IF

 DIM destzip as string = dldir & SLASH & dlfile

 '--Prompt & download if missing or out of date
 IF NOT download_gameplayer_if_needed(url, destzip, "linux.download.agree", "the Linux OHRRPGCE game player") THEN RETURN ""
 
 IF extract_zipfile(destzip, destdir) = NO THEN RETURN ""
 
 'Sanity check contents
 IF sanity_check_buildinfo(destdir & SLASH & "buildinfo.ini") = NO THEN RETURN ""
 IF NOT isfile(destdir & SLASH & "game.sh") THEN dist_info "ERROR: Failed to unzip game.sh" : RETURN ""

 IF renamefile(destdir & SLASH & "game.sh", destexe) = NO THEN dist_info "ERROR: Couldn't rename game.sh" : RETURN ""
 RETURN destexe
END FUNCTION

'isstmp: directory containing files to package
FUNCTION create_win_installer(installer as string, isstmp as string, basename as string, distinfo as DistribState) as bool
 DIM iscc as string = find_or_download_innosetup()
 IF iscc = "" THEN RETURN NO

 DIM iss_script as string
 iss_script = write_innosetup_script(basename, distinfo.gamename, isstmp)
 
 DIM args as string
 'FIXME: The following does not escape all problem characters that could occur in iss_script,
 'but I'm not sure what the best way to do that is, so leaving it for now.
 args = """" & win_path(iss_script) & """"
 
 DIM spawn_ret as string
 spawn_ret = win_or_wine_spawn_and_wait(iscc, args)
 IF LEN(spawn_ret) THEN dist_info "ERROR: iscc.exe failed: " & spawn_ret : RETURN NO
 safekill installer
 'Move the new installer to the correct location
 IF renamefile(isstmp & SLASH & "Output" & SLASH & "setup.exe", installer) = NO THEN
  dist_info "ERROR: iscc.exe completed but installer was not created"
  RETURN NO
 END IF

 RETURN YES
END FUNCTION

FUNCTION write_innosetup_script (basename as string, gamename as string, isstmp as string) as string

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
 s &= "OutputBaseFilename=setup" & E
 s &= "InfoAfterFile=README-" & basename & ".txt" & E
 IF isfile(isstmp & SLASH & "LICENSE.txt") THEN
  s &= "LicenseFile=LICENSE.txt" & E
 END IF

 s &= E & "[Languages]" & E
 s &= "Name: ""eng""; MessagesFile: ""compiler:Default.isl""" & E

 s &= E & "[Files]" & E
 s &= "Source: ""*""; Excludes: ""innosetup_script.iss""; DestDir: ""{app}""; Flags: ignoreversion recursesubdirs" & E

 s &= E & "[Icons]" & E
 s &= "Name: ""{userdesktop}\" & gamename & """; Filename: ""{app}\" & basename & ".exe""; WorkingDir: ""{app}"";" & E
 s &= "Name: ""{group}\" & gamename & """; Filename: ""{app}\" & basename & ".exe""; WorkingDir: ""{app}"";" & E
 
 'debuginfo s

 write_file iss_script, s

 RETURN iss_script
END FUNCTION

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

FUNCTION distribute_game_as_debian_package (which_arch as string, dest_override as string = "") as string
 debuginfo "  distribute_game_as_debian_package():"

 IF NOT valid_arch(which_arch, "x86, x86_64") THEN RETURN ""
 DIM deb_arch as string
 SELECT CASE which_arch
  CASE "x86":    deb_arch = "i386"
  CASE "x86_64": deb_arch = "amd64"
 END SELECT

 DIM distinfo as DistribState
 load_distrib_state distinfo

 DIM basename as string = get_debian_package_name()
 DIM pkgver as string = get_debian_package_version()
 DIM debname as string = trimfilename(sourcerpg)
 IF dest_override <> "" THEN debname = dest_override
 debname &= SLASH & basename & "_" & pkgver & "_" & deb_arch & ".deb"

 IF dist_prompt_overwrite_file(debname) = NO THEN RETURN ""

 DIM debtmp as string = dist_make_temp_dir()
 IF debtmp = "" THEN RETURN ""
 
 debuginfo "Prepare package data files..."
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
  DIM extractdir as string = debtmp & SLASH "extract.tmp"
  ' Extracts a bunch of files which we mostly ignore, including the path to game.sh it returns
  IF add_linux_gameplayer(which_arch, "dummy", extractdir) = "" THEN EXIT DO
  gameplayer = extractdir & SLASH & "linux" & SLASH & which_arch & SLASH & "ohrrpgce-game"
  IF isfile(gameplayer) = NO THEN dist_info "ERROR: didn't find ohrrpgce-game in downloaded zip" : EXIT DO
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

  'Write the copyright file, which contains game copyright and license and LICENSE-binary.txt
  write_debian_copyright_file gamedocsdir & SLASH & "copyright", extractdir & SLASH & "LICENSE-binary.txt"

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

  IF create_tarball(debtmp & SLASH & "control.tar.gz", debtmp, "control postinst postrm") = NO THEN EXIT DO

  fix_deb_group_permissions debtmp & SLASH & "usr"
  
  IF create_tarball(debtmp & SLASH & "data.tar.gz", debtmp, "usr") = NO THEN EXIT DO

  'Remove old deb
  safekill debname
  
  'Create new deb
  IF create_ar_archive(debtmp, debname, "debian-binary control.tar.gz data.tar.gz") = NO THEN EXIT DO
  
  dist_info trimpath(debname) & " was successfully created!", errInfo
  EXIT DO 'this loop is only ever one pass
 LOOP

 '--Cleanup temp files
 killdir debtmp, YES

 RETURN debname
END FUNCTION

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

FUNCTION dist_find_helper_app(appname as string) as string
 DIM path as string = find_helper_app(appname, YES)
 IF path = "" THEN dist_info "ERROR: " & missing_helper_message(appname & DOTEXE)
 RETURN path
END FUNCTION

FUNCTION create_ar_archive(start_in_dir as string, archive as string, files as string) as bool
 '--Returns YES if successful, or NO if failed

 ' start_in_dir only applies to the ar command. The archive filename should still be either absolute or relative to the default CURDIR

 'files is a list of space-separated filenames and directory names to include in the tarball
 'if they contain spaces they must be quoted

 DIM ar as string = dist_find_helper_app("ar")
 IF ar = "" THEN RETURN NO

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

'Create (replacing) zipfile, returns true on success.
'files is a list of space-separated filenames and directory names or wildcards to include in the zip
'relative to start_in_dir. If they contain spaces/etc they must be quoted
FUNCTION create_zipfile(zipfile as string, start_in_dir as string, files as string) as bool
 DIM zip as string = dist_find_helper_app("zip")
 IF zip = "" THEN RETURN NO

 safekill zipfile

 DIM spawn_ret as string
 DIM args as string

 zipfile = absolute_path(zipfile)

 '-6 is default, -8 may be 2x slower, -9 much slower again
 args = "-r -8 " & escape_filename(zipfile) & " " & files
 
 DIM olddir as string = CURDIR
 CHDIR start_in_dir
 spawn_ret = spawn_and_wait(zip, args)
 CHDIR olddir
 
 IF LEN(spawn_ret) THEN dist_info spawn_ret : RETURN NO

 IF NOT isfile(zipfile) THEN dist_info "Could not create " & zipfile : RETURN NO
 RETURN YES
END FUNCTION

FUNCTION extract_zipfile (zipfile as string, into_dir as string) as bool
 DIM unzip as string = dist_find_helper_app("unzip")
 IF unzip = "" THEN RETURN NO

 IF NOT isdir(into_dir) THEN makedir into_dir
 DIM args as string = "-o " & escape_filename(zipfile) & " -d " & escape_filename(into_dir)
 DIM spawn_ret as string = spawn_and_wait(unzip, args)
 IF LEN(spawn_ret) > 0 THEN dist_info "ERROR: unzip failed: " & spawn_ret : RETURN NO
 RETURN YES
END FUNCTION

FUNCTION create_tarball(tarball as string, start_in_dir as string, files as string) as bool
 'Create a .tar.gz file (requiring that extension). Returns true on success.

 ' start_in_dir only applies to the tar command. The tarball filename should still be either absolute or relative to the default CURDIR

 'files is a list of space-separated filenames and directory names to include in the tarball
 'if they contain spaces they must be quoted
 
 DIM tar as string = dist_find_helper_app("tar")
 IF tar = "" THEN RETURN NO

 DIM gzip as string = dist_find_helper_app("gzip")
 IF gzip = "" THEN RETURN NO

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
 
 DIM uncompressed as string = trimextension(tarball)  'Remove just .gz
 DIM args as string

 args = " -c " & more_args & " -f " & escape_filename(uncompressed) & " " & files

 DIM olddir as string = CURDIR
 CHDIR start_in_dir

 DIM spawn_ret as string
 spawn_ret = spawn_and_wait(tar, args)
 CHDIR olddir
 
 IF LEN(spawn_ret) THEN dist_info spawn_ret : RETURN NO

 IF gzip_file(uncompressed) = NO THEN RETURN NO
 
 RETURN YES
END FUNCTION

FUNCTION extract_tarball(tarball as string, into_dir as string, files as string = "") as bool
 '--Returns YES if successful, or NO if failed
 
 'The tarball must already be decompressed. Don't pass in a .tar.gz (this is inconsistent
 'with create_tarball, I know. )

 ' into_dir only applies to the tar command. The tarball filename should still be either absolute or relative to the default CURDIR

 'files is a list of space-separated files and directories to extract from the tarball, or blank for all.
 'If they contain spaces they must be quoted

 DIM tar as string = dist_find_helper_app("tar")
 IF tar = "" THEN RETURN NO

 DIM spawn_ret as string
 DIM args as string

 args = " -x -f " & escape_filename(tarball) & " " & files

 DIM olddir as string = CURDIR
 CHDIR into_dir
 spawn_ret = spawn_and_wait(tar, args)
 CHDIR olddir
 
 IF LEN(spawn_ret) THEN dist_info spawn_ret : RETURN NO
 
 RETURN YES
END FUNCTION

FUNCTION gzip_file (filename as string) as bool
 'Returns YES on success, NO on failure
 DIM gzip as string = dist_find_helper_app("gzip")
 IF gzip = "" THEN RETURN NO
 
 safekill filename & ".gz"
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
 DIM gzip as string = dist_find_helper_app("gzip")
 IF gzip = "" THEN RETURN NO
 
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

FUNCTION gather_files_for_mac (which_arch as string, basename as string, destdir as string, distinfo as DistribState) as bool
 DIM app as string
 app = add_mac_gameplayer(which_arch, basename, destdir)
 IF app = "" THEN RETURN NO 'dist_info "ERROR: OHRRPGCE-Game.app for " & which_arch & " is not available" : EXIT DO

 debuginfo "Copy rpg file"
 'Renaming the .rpg file to pkgname only for consistency with all other packagers
 DIM resources as string
 resources = app & SLASH & "Contents" & SLASH & "Resources"
 IF copy_or_relump(sourcerpg, resources & SLASH & basename & ".rpg") = NO THEN RETURN NO

 write_file resources & SLASH & "bundledgame", basename

 DIM icns_file as string = trimextension(sourcerpg) & ".icns"
 IF isfile(icns_file) THEN
  confirmed_copy(icns_file, resources & SLASH & "game.icns")
 END IF

 RETURN gather_common_files(basename, destdir, distinfo, !"\n")
END FUNCTION

FUNCTION distribute_game_as_mac_app (which_arch as string, dest_override as string = "") as string
 IF NOT valid_arch(which_arch, "x86, x86_64") THEN RETURN ""

 DIM destname as string = "$pkgname"
 IF which_arch = "x86" THEN
  'This is an obsolete arch, add a suffix
  destname &= "-mac-32bit.zip"
 ELSE
  destname &= "-mac.zip"
 END IF

 DIM basename as string
 destname = package_game(which_arch, destname, dest_override, NO, @gather_files_for_mac, basename)
 IF destname <> "" THEN
  'Package as a .zip file. NOTE: When created on Unix, .zip files contain file permissions
  'including the all-important +x bit in the "external attributes", but on Windows they don't.
  'On older versions of MacOS we could rely on a quirk of the old Mac Finder, which would set
  'the +x bit on every file when extracting files from a Windows/DOS .zip file. Newer Mac
  'archivers do not do this. Instead we use the zip_exec tool to add the +x bit manually.
  'See fix_mac_app_executable_bit_on_windows(). An alternate unused solution to actually
  'create Mac .zip packages with correct file permissions from Windows, is in
  'prepare_mac_app_zip()
  DIM warnxbit as bool = YES
#IFDEF __FB_WIN32__
  'So fix the executable bit on windows.
  DIM execname as string = basename & ".app/Contents/MacOS/ohrrpgce-game"
  IF fix_mac_app_executable_bit_on_windows(destname, execname) = NO THEN
   dist_info "WARNING: Couldn't set the executable bit on the executable the zip file. The app likely won't run on recent macOS."
   warnxbit = NO
  END IF
#ENDIF

  'I have seen someone -- on Linux, even -- wipe the +x flag on ohrrpgce-game
  'by unzipping and rezipping.
  IF warnxbit THEN
   dist_info "Note: Don't unzip and re-zip " & trimpath(destname) & ": that might wipe critical metadata from " _
             & basename & ".app and prevent it from running!", errInfo
  END IF
 END IF

 RETURN destname
END FUNCTION

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

'Copies all the needed files for a Mac package into destdir. Returns true on success
FUNCTION add_mac_gameplayer(which_arch as string, basename as string, destdir as string) as string

 DIM dldir as string = gameplayer_dir()
 IF dldir = "" THEN RETURN ""
  
 '--Decide which url to download
 DIM url as string
 DIM dlfile as string

 'Until ichorescent, used to download ohrrpgce-mac-minimal-$ARCH.tar.gz (nightlies)
 'or http://hamsterrepublic.com/dl/ohrrpgce-mac-minimal-$ARCH.tar.gz (stable)
 IF version_branch = "wip" THEN
  'If using any wip release, get the latest wip release
  dlfile = "ohrrpgce-player-mac-wip-" & which_arch & ".tar.gz"
  url = "http://hamsterrepublic.com/ohrrpgce/nightly/" & dlfile
 ELSE
  'Use this stable release
  dlfile = "ohrrpgce-player-mac" & version_release_tag & "-" & which_arch & ".tar.gz"
  url = "http://hamsterrepublic.com/ohrrpgce/archive/" & dlfile
 END IF

 DIM destgz as string = dldir & SLASH & dlfile
 DIM desttar as string = trimextension(destgz)

 '--Prompt & download if missing or out of date
 IF NOT download_gameplayer_if_needed(url, destgz, "mac.download.agree", "the Mac OHRRPGCE game player") THEN RETURN ""

 DIM game_app as string = destdir & SLASH & "OHRRPGCE-Game.app"
 DIM dest_app as string = destdir & SLASH & basename & ".app"
 
 'Extract (Would be nice if we had a tar.exe that could invoke gzip)
 IF gunzip_file(destgz) = NO THEN RETURN ""
 DIM extracted as bool = extract_tarball(desttar, destdir)
 safekill desttar
 IF extracted = NO THEN RETURN ""

 'Sanity checks
 IF sanity_check_buildinfo(destdir & SLASH & "buildinfo.ini") = NO THEN RETURN ""
 IF NOT isdir(game_app) THEN dist_info "ERROR: Failed to untar OHRRPGCE-Game.app" : RETURN ""
 IF NOT isfile(game_app & SLASH & "Contents" & SLASH & "MacOS" & SLASH & "ohrrpgce-game")   THEN dist_info "ERROR: Failed to completely untar OHRRPGCE-Game.app" : RETURN ""

 IF renamefile(game_app, dest_app) = NO THEN dist_info "Couldn't rename OHRRPGCE-Game.app" : RETURN ""

 RETURN dest_app
END FUNCTION

'Create an archive named fname (ending in either .zip, .tar.gz, or .exe for a Windows installer).
'build_variant: arch or buildname (varies by target)
'fname: filename (without path) to write; '$pkgname' in it is replaced with distinfo.pkgname.
'dest_override: directory to place it, or next to the .rpg if blank.
'use_subdir: whether the game should be placed in a directory in the archive named $pkgname
'gather_files: a function called to copy all files into a temp directory.
'basename: passed back for convenience
FUNCTION package_game (build_variant as string, fname as string, dest_override as string = "", use_subdir as bool, gather_files as FnGatherFiles, byref basename as string = "") as string
 DIM distinfo as DistribState
 load_distrib_state distinfo
 basename = distinfo.pkgname

 DIM destname as string = trimfilename(sourcerpg)
 IF dest_override <> "" THEN destname = dest_override
 destname &= SLASH & fname
 replacestr destname, "$pkgname", distinfo.pkgname

 debuginfo "--package_game: " & destname

 IF dist_prompt_overwrite_file(destname) = NO THEN RETURN ""

 DIM pkgtmp as string = dist_make_temp_dir()
 IF pkgtmp = "" THEN RETURN ""

 DIM ret as string = ""

 DO
  DIM subdir as string
  IF use_subdir THEN
   subdir = basename
   IF makedir(pkgtmp & SLASH & subdir) THEN EXIT DO
  END IF

  IF gather_files(build_variant, basename, pkgtmp & SLASH & subdir, distinfo) = NO THEN EXIT DO

  DIM files_arg as string
  IF use_subdir THEN files_arg = escape_filename(subdir) ELSE files_arg = "*"

  IF ends_with(fname, ".tar.gz") THEN
   IF create_tarball(destname, pkgtmp, files_arg) = NO THEN EXIT DO
  ELSEIF ends_with(fname, ".zip") THEN
   IF create_zipfile(destname, pkgtmp, files_arg) = NO THEN EXIT DO
  ELSEIF ends_with(fname, ".exe") THEN
   IF create_win_installer(destname, pkgtmp, basename, distinfo) = NO THEN EXIT DO
  ELSE
   showbug ""
   EXIT DO
  END IF

  dist_info decode_filename(trimpath(destname)) & " was successfully created!", errInfo
  ret = destname

  EXIT DO
 LOOP

 '--Cleanup temp files
 killdir pkgtmp, YES
 RETURN ret
END FUNCTION

'Add files common to all targets, and remove common unwanted ones
FUNCTION gather_common_files(basename as string, destdir as string, distinfo as DistribState, newline as string) as bool
 IF NOT isfile(destdir & SLASH & "LICENSE-binary.txt") THEN dist_info "ERROR: LICENSE-binary.txt missing" : RETURN NO

 'All files in the ohrrpgce-player-* archives should be distributed with games (except in .debs) except these two
 safekill destdir & SLASH & "README-player-only.txt"
 safekill destdir & SLASH & "buildinfo.ini"

 IF distinfo.omit_readme = NO THEN
  write_readme_text_file destdir & SLASH & "README-" & basename & ".txt", newline
 END IF

 IF distinfo.omit_license = NO THEN
  maybe_write_license_text_file destdir & SLASH & "LICENSE.txt"
 END IF

 FOR idx as integer = 0 TO UBOUND(distinfo.extra_files)
  'src is relative to the .rpg, which is in the current directory
  DIM src as string = distinfo.extra_files(idx)
  DIM dest as string = destdir & SLASH & trimpath(src)
  DIM res as bool = NO
  IF isdir(src) THEN
   res = confirmed_copydirectory(src, dest)
  ELSEIF real_isfile(src) THEN
   res = confirmed_copy(src, dest)
  END IF
  IF res = NO THEN
   IF dist_yesno("Extra file/dir " & src & " is missing, unreadable or couldn't be copied. Continue anyway?", NO, NO) = NO THEN
    RETURN NO
   END IF
  END IF
 NEXT idx

 RETURN YES
END FUNCTION

'Copies all the needed files for a windows distribution into destdir. Returns true on success.
FUNCTION gather_files_for_windows (buildname as string, basename as string, destdir as string, distinfo as DistribState) as bool
 DIM gameplayer as string
 gameplayer = add_windows_gameplayer(basename, destdir)
 IF gameplayer = "" THEN RETURN NO

 IF copy_or_relump(sourcerpg, destdir & SLASH & basename & ".rpg") = NO THEN RETURN NO

 insert_windows_exe_icon gameplayer, trimextension(sourcerpg) & ".ico"

 'Write readme with DOS/Window line endings
 RETURN gather_common_files(basename, destdir, distinfo, !"\r\n")
END FUNCTION

FUNCTION gather_files_for_steam_windows (which_arch as string, basename as string, destdir as string, distinfo as DistribState) as bool
 IF gather_files_for_windows(which_arch, basename, destdir, distinfo) = NO THEN RETURN NO

 IF add_steamworks_lib("steam_api.dll", destdir) = "" THEN RETURN NO

 write_file destdir & SLASH & "steam_appid.txt", STR(distinfo.steam_appid)

 RETURN YES
END FUNCTION

FUNCTION distribute_game_as_windows_zip (dest_override as string = "") as string
 RETURN package_game("sdl2", "$pkgname.zip", dest_override, NO, @gather_files_for_windows)
END FUNCTION

FUNCTION distribute_game_as_windows_installer (dest_override as string = "") as string
 RETURN package_game("sdl2", "$pkgname-setup.exe", dest_override, NO, @gather_files_for_windows)
END FUNCTION

FUNCTION distribute_game_for_steam_windows () as string
 RETURN package_game("sdl2", "$pkgname-steam-windows.zip", , NO, @gather_files_for_steam_windows)
END FUNCTION

'Copies all the needed files for a linux tarball into destdir. Returns true on success
FUNCTION gather_files_for_linux (which_arch as string, basename as string, destdir as string, distinfo as DistribState) as bool
 IF add_linux_gameplayer(which_arch, basename, destdir) = "" THEN RETURN NO

 IF copy_or_relump(sourcerpg, destdir & SLASH & basename & ".rpg") = NO THEN RETURN NO

 RETURN gather_common_files(basename, destdir, distinfo, !"\n")
END FUNCTION

FUNCTION gather_files_for_steam_linux (which_arch as string, basename as string, destdir as string, distinfo as DistribState) as bool
 IF gather_files_for_linux(which_arch, basename, destdir, distinfo) = NO THEN RETURN NO

 DIM libname as string
 libname = IIF(which_arch = "x86", "linux32", "linux64") & SLASH "libsteam_api.so"

 IF add_steamworks_lib(libname, destdir & SLASH & "linux" & SLASH & which_arch) = "" THEN RETURN NO

 write_file destdir & SLASH & "steam_appid.txt", STR(distinfo.steam_appid)

 RETURN YES
END FUNCTION

FUNCTION distribute_game_as_linux_tarball (which_arch as string, dest_override as string = "") as string
 IF NOT valid_arch(which_arch, "x86, x86_64") THEN RETURN ""

 'use_subdir = YES
 RETURN package_game(which_arch, "$pkgname-linux-" & which_arch & ".tar.gz", dest_override, YES, @gather_files_for_linux)
END FUNCTION

FUNCTION distribute_game_for_steam_linux (which_arch as string) as string
 IF NOT valid_arch(which_arch, "x86, x86_64") THEN RETURN ""

 'use_subdir = NO
 DIM basename as string
 VAR ret = package_game(which_arch, "$pkgname-steam-linux-" & which_arch & ".zip", , NO, @gather_files_for_steam_linux, basename)
 IF ret <> "" THEN
  dist_info "In Steamworks, create a Launch Option for Linux+SteamOS that runs '" & basename & "'", errInfo
 END IF
 RETURN ret
END FUNCTION

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
  distribute_game_as_windows_zip
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
 IF distrib_type = "web" ORELSE distrib_type = "all" THEN
  distribute_game_as_web_zip
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

FUNCTION itch_butler_setup() as bool
 ' Returns NO if setup failed

 DIM butler_path as string = find_helper_app("butler")
 IF butler_path = "" THEN
  IF NOT itch_butler_download() THEN RETURN NO
  butler_path = find_helper_app("butler")
  IF butler_path = "" THEN RETURN NO  'Shouldn't happen
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

'Somewhat similar to install_windows_helper_app
FUNCTION itch_butler_download() as bool
 DIM support_dir as string = get_support_dir()
 DIM butler_path as string = support_dir & SLASH & "butler" DOTEXE

 DIM butler_platform as string = itch_butler_platform_version()
 IF butler_platform = "" THEN RETURN NO  'Not available

 '--Ask the user for permission the first time we download (subsequent updates don't ask)
 DIM agree_file as string = support_dir & SLASH & "itch.butler.download.agree"
 IF agreed_to_download(agree_file, "the itch.io butler tool") = NO THEN RETURN NO
 
 DIM destzip as string = support_dir & SLASH & "butler.zip"
 '--Actually download the dang file
 DIM url as string = "https://broth.itch.ovh/butler/" & butler_platform & "/LATEST/archive/default"
 IF NOT download_file(url, destzip) THEN
  dist_info "ERROR: Failed to download itch.io butler.zip" : RETURN NO
 END IF
 
 '-- remove the old copy (if it exists)
 safekill butler_path

 IF extract_zipfile(destzip, support_dir) = NO THEN RETURN NO

 IF NOT isfile(butler_path) THEN dist_info "ERROR: Didn't find itch.io butler tool in the downloaded zip" : RETURN NO
 
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

FUNCTION itch_butler_is_logged_in() as bool
 ' Can't be logged in if it ain't installed
 IF find_helper_app("butler") = "" THEN RETURN NO
 DIM butler_creds as string
 #IFDEF __FB_WIN32__
 butler_creds = ENVIRON("USERPROFILE") & "\.config\itch\butler_creds"
 #ELSEIF DEFINED(__FB_DARWIN__)
 butler_creds = ENVIRON("HOME") & "/Library/Application Support/itch/butler_creds"
 #ELSE
 butler_creds = ENVIRON("HOME") & "/.config/itch/butler_creds"
 #ENDIF
 'Note this creds file can be years old and still works. TODO: What if you want
 'to switch user?
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

 'When this function is called we've already checked butler is present and logged in
 DIM butler as string = find_helper_app("butler")
 BUG_IF(butler = "", "butler missing")
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

 'Needs to be on the same filesystem as other other dist_make_temp_dir directories
 DIM itch_temp_dir as string = dist_make_temp_dir("itch_io_exports")
 IF itch_temp_dir = "" THEN EXIT SUB

 debuginfo "Exporting files to " & itch_temp_dir
 auto_choose_default = YES
 dist_basicstatus "Exporting for Windows..."
 DIM win_zip as string = distribute_game_as_windows_zip(itch_temp_dir)
 IF win_zip = "" THEN dist_info "Aborting itch.io upload" : EXIT SUB
 dist_basicstatus "Exporting for Mac..."
 DIM mac_app as string = distribute_game_as_mac_app("x86_64", itch_temp_dir)
 IF mac_app = "" THEN dist_info "Aborting itch.io upload" : EXIT SUB
 dist_basicstatus "Exporting for Linux..."
 DIM linux_tarball as string = distribute_game_as_linux_tarball("x86_64", itch_temp_dir)
 IF linux_tarball = "" THEN dist_info "Aborting itch.io upload" : EXIT SUB
 auto_choose_default = NO

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


CONST steamworks_version = 155

FUNCTION steamworks_zip() as string
 RETURN get_support_dir(NO) & SLASH & "steamworks_v" & steamworks_version & ".zip"
END FUNCTION

SUB download_steamworks()
 DIM dlfile as string = steamworks_zip()
 DIM fname as string = trimpath(dlfile)
 DIM download_url as string = "http://HamsterRepublic.com/ohrrpgce/support/" & fname
 IF yesno("Download the redistributable libraries for Steam from " & url_hostname(download_url) & "?") THEN
  IF NOT download_file(download_url, dlfile) THEN
   visible_debug "Unable to download " & download_url
  END IF
 END IF
END SUB

'Extract a library from the steamworks zip into destdir.
'libname: the filename and its parent directory, inside the steamworks .zip
FUNCTION add_steamworks_lib(libname as string, destdir as string) as string
 DIM outfile as string = destdir & SLASH & trimpath(libname)
 
 DIM unzip as string = dist_find_helper_app("unzip")
 IF unzip = "" THEN RETURN ""

 DIM arglist as string
 ' -q quiet -o overwrite -j junk directories
 arglist =  " -qoj " & escape_filename(steamworks_zip()) & " " & escape_filename("sdk/redistributable_bin/" & libname) & " -d " & escape_filename(destdir)
 DIM spawn_ret as string
 spawn_ret = spawn_and_wait(unzip, arglist)
 IF NOT isfile(outfile) THEN
  visible_debug "Unable to unzip " & libname & " from " & steamworks_zip()
  RETURN ""
 END IF

 RETURN outfile
END FUNCTION

FUNCTION distribute_game_as_web_zip (dest_override as string = "") as string
 IF dist_yesno("Warning: The web port does not currently support persistent save games. Save slots are deleted when the browser tab closes. Continue anyway?", YES, NO) = NO THEN RETURN ""
 RETURN package_game("", "$pkgname-web.zip", dest_override, NO, @gather_files_for_web)
END FUNCTION

'Copies all the needed files for a web distribution into destdir. Returns true on success.
FUNCTION gather_files_for_web (buildname as string, basename as string, destdir as string, distinfo as DistribState) as bool
 DIM gameplayer as string
 gameplayer = add_web_gameplayer(basename, destdir)
 IF gameplayer = "" THEN RETURN NO

 IF copy_or_relump(sourcerpg, join_path(destdir, basename & ".rpg")) = NO THEN RETURN NO

 DIM js_file as string = join_path(destdir, "ohrrpgce-game.js")
 DIM data_file as string = join_path(destdir, "ohrrpgce-game.data")
 DIM output_dir as string = join_path(destdir, "data")
 IF NOT extract_web_data_files(js_file, data_file, output_dir) THEN
  dist_info "ERROR: Failed to extract web data files" : RETURN NO
 END IF
 
 IF NOT web_data_cleanup(destdir) THEN
  dist_info "ERROR: Failed to clean up web data files" : RETURN NO
 END IF

 IF NOT recreate_web_data_file(js_file, data_file, output_dir) THEN
  dist_info "ERROR: Failed to recreate web data file" : RETURN NO
 END IF
 killdir output_dir

 RETURN gather_common_files(basename, destdir, distinfo, !"\n")
END FUNCTION

FUNCTION extract_web_data_files(js_file as string, data_file as string, output_dir as string) as bool
 'js-file is the minified javascript file to search for the loadPackage metadata
 '  it may fail if this is not minified, but that is okay, it is always minified :)
 '
 'data_file is the package of data files to extract
 '
 'output_dir will be created, and all the data files are dumped in there, preserving directory structure
 IF NOT isdir(output_dir) THEN
  IF makedir(output_dir) THEN
   dist_info "ERROR: Failed to create output dir " & output_dir : RETURN NO
  END IF
 END IF
 IF NOT diriswriteable(output_dir) THEN
  dist_info "ERROR: Unable to write to output dir " & output_dir : RETURN NO
 END IF

 DIM js_text as string = string_from_file(js_file)
 DIM success as bool
 DIM loadPackage_text as string = extract_string_chunk(js_text, "loadPackage(", ")", success)
 IF NOT success THEN
  dist_info "ERROR: Failed to extract loadPackage() from " & js_file : RETURN NO
 END IF

 DIM remains as string = loadPackage_text
 DIM fh_in as integer
 DIM fh_out as integer
 DIM filename as string
 DIM out_filename as string
 DIM start_str as string
 DIM start_pos as integer
 DIM end_str as string
 DIM end_pos as integer
 DIM found as bool
 DIM foundat as integer
 DIM foundcount as integer = 0
 DIM parsefail as bool = NO
 DIM buflen as integer
 REDIM buf(0) as ubyte

 fh_in = FREEFILE
 OPEN data_file FOR BINARY ACCESS READ as #fh_in

 debug "Parsing web data files..." 
 DO
   filename = extract_string_chunk(remains, """filename"":""", """", found, foundat)
   IF NOT found THEN EXIT DO
   foundcount += 1
   remains = MID(remains, foundat + len(filename))
   start_str = "?"
   end_str = "?"

   start_str = extract_string_chunk(remains, """start"":", ",", found, foundat)
   IF NOT found THEN parsefail = YES : EXIT DO
   IF parse_int(start_str, @start_pos, YES) = NO THEN parsefail = YES : EXIT DO
   remains = MID(remains, foundat + len(start_str))

   end_str = extract_string_chunk(remains, """end"":", "}", found, foundat)
   IF NOT found THEN parsefail = YES : EXIT DO
   IF parse_int(end_str, @end_pos, YES) = NO THEN parsefail = YES : EXIT DO
   remains = MID(remains, foundat + len(end_str))

   debuginfo "Found data filename=" & filename & " from " & start_str & " to " & end_str
   
   out_filename = join_path(output_dir, trim_path_root(filename))
   makedir_recurse trimfilename(out_filename)
   fh_out = FREEFILE
   OPEN out_filename FOR BINARY ACCESS WRITE as #fh_out
    buflen = end_pos - start_pos
    REDIM buf(buflen - 1) as ubyte
    GET #fh_in, 1 + start_pos, buf()
    PUT #fh_out, 1, buf()
   CLOSE #fh_out
   debuginfo "Wrote data file to " & out_filename
 
 LOOP
 CLOSE #fh_in

 IF parsefail THEN
  dist_info "ERROR: Failed to parse offsets for data file " & filename & " (""" & start_str & """,""" & end_str & """) from " & js_file : RETURN NO
 END IF

 IF foundcount = 0 THEN
  dist_info "ERROR: Failed to find any filenames in loadPackage() " & js_file : RETURN NO
 END IF

 RETURN YES
END FUNCTION

FUNCTION recreate_web_data_file(js_file as string, data_file as string, from_dir as string) as bool
 'js-file is the minified javascript file to insert the new loadPackage metadata
 'data_file is the package of data files to recreate. The old existing file will be deleted.
 'from_dir contains the files and folders to add to the data package. It isn't deleted afterwards. The caller is responsible for that.
 'Returns true for success

 REDIM filelist(-1 TO -1) as string
 recursefiles from_dir, ALLFILES, YES, filelist()
 DIM howmany AS integer = UBOUND(filelist)
 DIM size(howmany) as integer
 DIM cur_offset as integer = 0
 DIM offset(howmany) as integer
 DIM pkgpath(howmany) as string
 DIM fh_in as integer
 DIM fh_out as integer
 REDIM buf(0) as ubyte
 
 'Remove the old data file
 killfile data_file

 'Open the new data file for writing 
 fh_out = FREEFILE
 OPEN data_file FOR BINARY ACCESS WRITE as #fh_out

 'Build a list of files and loop through it
 FOR i as integer = 0 TO UBOUND(filelist)
  pkgpath(i) = filelist(i)
  replacestr(pkgpath(i), trim_trailing_slashes(from_dir), "", 1)
  size(i) = FILELEN(filelist(i))
  offset(i) = cur_offset
  cur_offset += size(i)
  debuginfo "Repackaging web data file " & filelist(i) & " -> " & pkgpath(i) & " (size=" & size(i) & ", offset=" & offset(i) & ")"
  'Now read the file and write it to the data package
  fh_in = FREEFILE
  OPEN filelist(i) FOR BINARY ACCESS READ as #fh_in
  REDIM buf(size(i)) as ubyte
  GET #fh_in, 1, buf()
  PUT #fh_out, 1 + offset(i), buf()
  CLOSE #fh_in
 NEXT i

 CLOSE #fh_out

 'Now we generate the json metadata
 DIM metadata as string = "{""files"":["
 FOR i as integer = 0 TO UBOUND(filelist)
  IF i <> 0 THEN metadata &= ","
  metadata &= "{""filename"":""" & escape_string(pkgpath(i), """") & ""","
  metadata &= """start"":" & offset(i) & ","
  metadata &= """end"":" & offset(i) + size(i) & "}"
 NEXT i
 metadata &= "],""remote_package_size"":" & FILELEN(data_file) & "}"
 
 'Now insert the metadata into the javascript file
 DIM js_text as string = string_from_file(js_file)
 DIM replace_ok as bool
 js_text = replace_string_chunk(js_text, "loadPackage(", ")", metadata, replace_ok)
 IF NOT replace_ok THEN
  dist_info "ERROR: Failed to to update loadPackage() metadata in " & js_file : RETURN NO
 END IF
 string_to_file(js_text, js_file)

 RETURN YES
END FUNCTION

FUNCTION add_web_gameplayer(basename as string, destdir as string) as string
 'Download a precompiled web player package,
 'extract it to destdir (creating it but not clearing if already existing),
 'and return the full path to index.html therein.
 '
 'Returns "" for failure.

 DIM destexe as string = destdir & SLASH & "index.html"

 DIM dldir as string = gameplayer_dir()
 IF dldir = "" THEN RETURN ""
  
 '--Decide which url to download
 DIM url as string
 DIM dlfile as string
 IF version_branch = "wip" THEN
  'If using any wip release, get the latest wip release
  dlfile = "ohrrpgce-player-web-wip.zip"
  url = "http://hamsterrepublic.com/ohrrpgce/nightly/" & dlfile
 ELSE
  'Use this stable release
  dlfile = "ohrrpgce-player-web" & version_release_tag & ".zip"
  url = "http://hamsterrepublic.com/ohrrpgce/archive/" & dlfile
 END IF

 DIM destzip as string = dldir & SLASH & dlfile

 '--Prompt & download if missing or out of date
 IF NOT download_gameplayer_if_needed(url, destzip, "web.download.agree", "the Web OHRRPGCE game player") THEN RETURN ""
 
 IF extract_zipfile(destzip, destdir) = NO THEN RETURN ""
 
 'Sanity check contents
 IF sanity_check_buildinfo(destdir & SLASH & "buildinfo.ini") = NO THEN RETURN ""
 IF NOT isfile(destdir & SLASH & "ohrrpgce-game.html") THEN dist_info "ERROR: Failed to unzip ohrrpgce-game.html" : RETURN ""
 IF NOT isfile(destdir & SLASH & "ohrrpgce-game.js") THEN dist_info "ERROR: Failed to unzip ohrrpgce-game.js" : RETURN ""

 IF renamefile(destdir & SLASH & "ohrrpgce-game.html", destexe) = NO THEN dist_info "ERROR: Couldn't rename ohrrpgce-game.html" : RETURN ""
 RETURN destexe
END FUNCTION

FUNCTION web_data_cleanup(output_dir as string) as bool
 'Assumes we have already unpacked the web data files into output_dir/data/*
 'Deletes unwanted files, and moves RPG files into the correct place
 'Returns true on success, false on failure
 DIM data_dir as string = join_path(output_dir, "data")
 IF NOT isdir(data_dir) THEN
  dist_info "ERROR: Could not find temp data dir " & data_dir : RETURN NO
 END IF
 
 'Remove sample games folder
 killdir join_path(data_dir, "games"), YES
 
 'Move the RPG file(s)
 DIM src_rpg as string
 DIM dest_rpg as string
 REDIM rpgfiles(0) as string
 findfiles output_dir, "*.rpg", , , rpgfiles()
 IF UBOUND(rpgfiles) = -1 THEN
  dist_info "ERROR: Could not find " & data_dir & SLASH & "*.rpg" : RETURN NO
 END IF
 FOR i as integer = 0 TO UBOUND(rpgfiles)
  src_rpg = join_path(output_dir, rpgfiles(i))
  dest_rpg = join_path(data_dir, rpgfiles(i))
  debuginfo "Move " & src_rpg & " -> " & dest_rpg
  IF renamefile(src_rpg, dest_rpg) = NO THEN
   dist_info "ERROR: Could not move " & src_rpg & " to " & dest_rpg : RETURN NO
  END IF
 NEXT i
 
 'Create the ohrrpgce_arguments.txt file
 DIM rpg_name as string = trimextension(trimpath(sourcerpg)) & ".rpg"
 string_to_file rpg_name, join_path(data_dir, "ohrrpgce_arguments.txt")
 
 'Delete the midi soundfont if it is unused
 IF game_uses_midi_or_bam() THEN
  debuginfo "This game uses midi or bam, keep the soundfont"
 ELSE
  debuginfo "This game uses midi or bam, discard the soundfont"
  killdir join_path(data_dir, "soundfonts"), YES
  killdir join_path(data_dir, "etc"), YES
 END IF
 
 RETURN YES
END FUNCTION

