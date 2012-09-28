'$lang: "fb"
#include "util.bi"

DECLARE SUB start_replaying_input (filename as string)
DECLARE SUB stop_replaying_input (msg as string="")
DECLARE SUB replay_input ()
DECLARE SUB print_usage (errmsg as string = "")

dim shared play_input_file as integer
dim shared ohrkey_ver as integer = -1

SUB start_replaying_input (filename as string)
 print "Displaying keyboard input from: """ & filename & """"
 play_input_file = FREEFILE
 open filename for binary access read as #play_input_file
 dim header as string = STRING(12, 0)
 GET #play_input_file,, header
 if header <> "OHRRPGCEkeys" then stop_replaying_input "No OHRRPGCEkeys header in """ & filename & """"
 print header
 GET #play_input_file,, ohrkey_ver
 if ohrkey_ver <> 4 then
  stop_replaying_input "Unknown ohrkey version code " & ohrkey_ver & " in """ & filename & """. Only know how to understand version 4"
  EXIT SUB
 end if
 print "ohrkey version: " & ohrkey_ver
 dim seed as double
 GET #play_input_file,, seed
 print "Random seed=" & seed
END SUB

SUB stop_replaying_input (msg as string="")
 if msg <> "" then
  print "STOP: " & msg
 end if
 print LOF(play_input_file) - LOC(play_input_file) & " bytes left over"
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
  DIM fpos as integer = LOC(play_input_file)
  GET #play_input_file,, replaytick
  info = "L:" & fpos & " T:" & replaytick 
  dim as ubyte elapsed_ms
  GET #play_input_file,, elapsed_ms
  info &= " ms:" & elapsed_ms

  dim presses as ubyte
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

  dim input_len as ubyte
  GET #play_input_file,, input_len
  dim inputtext as string
  if input_len then
    inputtext = space(input_len)
    GET #play_input_file,, inputtext
    info = info & " input: '" & inputtext & "'"
  end if

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

