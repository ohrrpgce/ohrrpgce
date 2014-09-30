#include "config.bi"
#include "common.bi"
#include "const.bi"
#include "util.bi"
#include "music.bi"
#include "audwrap/audwrap.bi"


Declare Function SoundSlot(byval num as integer) as integer

TYPE SoundEffect
  used as integer 'sentinel, not 0 = this entry contains valid data
  effectID as integer 'OHR sound effect
  soundID as integer 'Audiere number
  paused as integer
END TYPE

'music_audiere has no limit on number sound effects playing at once
redim Shared SoundPool(10) as SoundEffect

dim shared sound_inited as integer 'must be non-zero for anything but _init to work


sub sound_init
  sound_inited += 1
  'debug "sound init = " & sound_inited

  if sound_inited <> 1 then exit sub

  if AudInit() then
    exit sub ':(
  end if

  'music_init 'for safety (don't worry, they won't recurse (much))

  'at this point, we're basically done

end sub

sub sound_close
  sound_inited -= 1
  'debug "sound close = " & sound_inited

  'trying to free something that's already freed... bad!
  if sound_inited <> 0 then exit sub
'  debug "sound_close"

  sound_reset()

  AudClose()

  'music_close
end sub

sub sound_reset
  dim i as integer = 0
  for i = 0 to Ubound(SoundPool)
    UnloadSound(i)
  next
end sub

sub sound_play(byval num as integer, byval l as integer,  byval s as integer = 0)
  'debug ">>sound_play(" & num & ", " & l & ")"
  dim slot as integer

  if s then
    slot = num
  else
    slot = SoundSlot(num)
    if slot = -1 then
      'debug "sound not loaded, loading."
      slot = LoadSound(num)
    end if
  end if
  if slot = -1 then exit sub

  'debug "slot " & slot
  with SoundPool(slot)
  'debug str(AudIsPlaying(.soundID))
    if AudIsPlaying(.soundID) <> 0 and .paused = 0 then
      'debug "<<already playing"
      exit sub
    end if

    AudPlay(.soundID)

    'for consistency with other backends, can't change loop behaviour of a paused effect
    if .paused = 0 then AudSetRepeat(.soundID, l)
    .paused = 0
  end with
'  debug "<<done"
end sub

sub sound_pause(byval num as integer,  byval s as integer = 0)
  'debug ">>sound_pause(" + trim(str(slot)) + ")"
  dim slot as integer
  if not s then slot = SoundSlot(num) else slot = num
  if slot = -1 then exit sub

  with SoundPool(slot)
    if sound_playing(slot, -1) = 0 OR .paused then
      exit sub
    end if

    .paused = -1
    AudPause(.soundID)
  end with
  'debug "<<sound_pause"
end sub

sub sound_stop(byval num as integer,  byval s as integer = 0)
  'debug ">>sound_stop(" + trim(str(slot)) + ")"
  dim slot as integer
  if not s then slot = SoundSlot(num) else slot = num
  if slot = -1 then exit sub

  with SoundPool(slot)

    AudStop(.soundID)

    .paused = 0
  end with
  'debug "<<sound_stop"
end sub

sub sound_free(byval num as integer)
  dim i as integer

  for i = 0 to Ubound(SoundPool)
    if SoundPool(i).used and SoundPool(i).effectID = num then
      UnloadSound(i)
    end if
  next
end sub

function sound_playing(byval num as integer,  byval s as integer = 0) as bool
  dim slot as integer
  if not s then slot = SoundSlot(num) else slot = num
  if slot = -1 then return NO

  return AudIsPlaying(SoundPool(slot).soundID) <> 0

end function



'-------------------------------------------------------------------------------



'Returns the slot in the sound pool which corresponds to the given sound effect
' if the sound is not loaded, returns -1

Function SoundSlot(byval num as integer) as integer
  dim i as integer
  for i = 0 to Ubound(SoundPool)
    with SoundPool(i)
      'debug "i = " & i & ", used = " & .used & ", effID = " & .effectID & ", sndID = " & .soundID & ", AudIsValid = " & AudIsValidSound(.soundID)
      if .used AND (.effectID = num OR num = -1) AND AudIsValidSound(.soundID) then return i
    end with
  next

  return -1
End Function

'Loads an OHR sound (num) into a slot. Returns the slot number, or -1 if an error
' ocurrs
Function LoadSound overload(byval num as integer) as integer
  ' 0 - sanity checks

  'eh... we don't really need to throw an error if the sound is already loaded...
  dim ret as integer
  ret = SoundSlot(num)
  if ret >= 0 then return ret

  return LoadSound(soundfile(num), num)
End Function

Function LoadSound overload(byval lump as Lump ptr,  byval num as integer = -1) as integer
  return -1
End Function

Function LoadSound(f as string,  byval num as integer = -1) as integer
  dim ret as integer
  'the steps
  ' 1. allocate space in the sound pool
  ' 2. load the sound

  ' 1. make room

  dim i as integer

  'iterate through the pool
  for i = 0 to Ubound(SoundPool)
    if SoundPool(i).used <> -1 then exit for 'if we found a free spot, coo'
  next

  'otherwise, i will be left =ing SoundPool size + 1
  if i = Ubound(SoundPool) + 1 then
    'Grow the sound pool
    redim preserve SoundPool(Ubound(SoundPool) * 1.5)
  end if

  'ok, now i points at a valid slot. goody.

  ' 2. load the sound

  'TODO: abstract file name to a function or something
  'loadWaveFileToBuffer soundfile(num), @derbuffer

  dim fe as string = lcase(right(f,3))
  dim s as integer
'  debug f & ": " & fe
  if fe = "mp3" or fe = "ogg" then 'intended for streaming
    s = AudLoadSound(f, 1)
  else
    s = AudLoadSound(f, 0)
  end if

'  debug "slot is " & s

  if s = -1 then return -1 'crap

  'if we got this far, yes!
  with SoundPool(i)
    .used = -1
    .soundID = s
    .effectID = num
  end with

  return i


End Function

'Unloads a sound loaded in a slot. TAKES A SLOT, NOT AN SFX NUMBER!
Sub UnloadSound(byval slot as integer)
  with SoundPool(slot)
    if not .used then exit sub
    if AudIsValidSound(.soundID) then AudUnloadSound(.soundID)
    .used = 0
    .paused = 0
    .soundID = 0
    .effectID = 0
  end with
End Sub
