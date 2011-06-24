'$lang: "fb"
#include "scancodes.bi"

DECLARE FUNCTION keyname (byval k as integer) as string
DECLARE SUB start_replaying_input (filename as string)
DECLARE SUB stop_replaying_input (msg as string="")
DECLARE SUB replay_input ()
DECLARE SUB print_usage (errmsg as string = "")

dim shared key2text(53) as string*1 => {"", "", "1","2","3","4","5","6","7","8","9","0","-","=","","","q","w","e","r","t","y","u","i","o","p","[","]","","","a","s","d","f","g","h","j","k","l",";","'","`","","\","z","x","c","v","b","n","m",",",".","/"}
dim shared play_input_file as integer

FUNCTION keyname (byval k as integer) as string
 SELECT CASE k
  CASE scEsc: return "ESC"
  CASE scEnter: return "ENTER"
  CASE scSpace: return "SPACE"
  CASE scAlt: return "ALT"
  CASE scCtrl: return "CTRL"
  CASE scUp: return "Up"
  CASE scRight: return "Right"
  CASE scDown: return "Down"
  CASE scLeft: return "Left"
  CASE scNumLock: return "NumLock"
 END SELECT
 IF k >= lbound(key2text) and k <= ubound(key2text) THEN
  IF key2text(k) <> "" THEN return key2text(k)
 END IF
 return "scancode" & k
END FUNCTION

SUB start_replaying_input (filename as string)
 print "Displaying keyboard input from: """ & filename & """"
 play_input_file = FREEFILE
 open filename for binary access read as #play_input_file
 dim header as string = STRING(12, 0)
 GET #play_input_file,, header
 if header <> "OHRRPGCEkeys" then stop_replaying_input "No OHRRPGCEkeys header in """ & filename & """"
 print header
 dim ohrkey_ver as integer = -1
 GET #play_input_file,, ohrkey_ver
 if ohrkey_ver <> 0 then stop_replaying_input "Unknown ohrkey version code " & ohrkey_ver & " in """ & filename & """. Only know how to understand version 0"
 print "ohrkey version: " & ohrkey_ver
END SUB

SUB stop_replaying_input (msg as string="")
 if msg <> "" then
  print "STOP: " & msg
 end if
 close #play_input_file
END SUB

SUB replay_input ()
 DIM replaytick as integer
 DIM info AS string
 do
  if EOF(play_input_file) then
   stop_replaying_input "The end of the input playback file was reached."
   exit sub
  end if
  GET #play_input_file,, replaytick
  info = "T:" & replaytick 
  dim code as ubyte
  GET #play_input_file,, code
  select case code
   case 0: 'key data only
   case 1: 'random seed present
    dim seed as double
    GET #play_input_file,, seed
    info = info & " seed=" & seed
   case else:
    stop_replaying_input "input replay tick " & replaytick & " has unknown code " & code
    exit sub
  end select
  dim presses as integer
  GET #play_input_file,, presses
  if presses < 0 orelse presses > 128 then
   stop_replaying_input "input replay tick " & replaytick & " has invalid number of keypresses " & presses
   exit sub
  end if
  info = info & " ("
  dim key as ubyte
  dim kb as ubyte
  for i as integer = 1 to presses
   GET #play_input_file,, key
   GET #play_input_file,, kb
   info = info & " " & keyname(key) & "=" & kb
  next i
  info = info & " )"
  print info
 loop
END SUB

SUB print_usage (errmsg as string = "")
 print "USAGE: dumpohrkey filename.ohrkey"
 print ""
 print "This utility will print out an OHRRPGCE input recording file for debugging"
 print ""
 if errmsg <> "" then
  print "ERROR: " & errmsg
 end if
 SYSTEM
END SUB

'-----------------------------------------------------------------------

DIM filename as string = COMMAND
IF filename = "" THEN print_usage
start_replaying_input(filename)
replay_input

