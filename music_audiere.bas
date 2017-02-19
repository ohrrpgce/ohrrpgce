#include "config.bi"
#include "common.bi"
#include "const.bi"
#include "util.bi"
#include "music.bi"
#include "audwrap/audwrap.bi"


TYPE SoundEffect
  used as bool        'Whether this entry contains valid data
  effectID as integer 'OHR sound effect
  soundID as integer  'Audiere number
  paused as bool
END TYPE

'music_audiere has no limit on number sound effects playing at once
redim Shared SoundPool(10) as SoundEffect

'Number of times sound_init called. Must be non-zero for anything but _init to work
dim shared sound_init_count as integer


sub sound_init
  sound_init_count += 1
  'debug "sound init = " & sound_init_count

  if sound_init_count <> 1 then exit sub

  if AudInit() then
    exit sub ':(
  end if

  'music_init 'for safety (don't worry, they won't recurse (much))
end sub

sub sound_close
  sound_init_count -= 1
  'debug "sound close = " & sound_init_count

  'trying to free something that's already freed... bad!
  if sound_init_count <> 0 then exit sub
  'debug "sound_close"

  sound_reset()

  AudClose()

  'music_close
end sub

sub sound_reset
  for slot as integer = 0 to ubound(SoundPool)
    sound_unload(slot)
  next
end sub

sub sound_play(num as integer, loopcount as integer, num_is_slot as bool = NO)
  'debug ">>sound_play(" & num & ", " & l & ")"
  dim slot as integer

  if num_is_slot then
    slot = num
  else
    slot = sound_slot_with_id(num)
    if slot = -1 then
      'debug "sound not loaded, loading."
      slot = sound_load(soundfile(num), num)
    end if
  end if
  if slot = -1 then exit sub

  'debug "slot " & slot
  with SoundPool(slot)
  'debug str(AudIsPlaying(.soundID))
    if AudIsPlaying(.soundID) <> 0 and .paused = NO then
      'debug "<<already playing"
      exit sub
    end if

    AudPlay(.soundID)

    'for consistency with other backends, can't change loop behaviour of a paused effect
    if .paused = NO then AudSetRepeat(.soundID, loopcount)
    .paused = NO
  end with
  'debug "<<done"
end sub

sub sound_pause(num as integer, num_is_slot as bool = NO)
  'debug ">>sound_pause(" + trim(str(slot)) + ")"
  dim slot as integer
  slot = iif(num_is_slot, num, sound_slot_with_id(num))
  if slot = -1 then exit sub

  with SoundPool(slot)
    if sound_playing(slot, -1) = 0 OR .paused then
      exit sub
    end if

    .paused = YES
    AudPause(.soundID)
  end with
  'debug "<<sound_pause"
end sub

sub sound_stop(num as integer, num_is_slot as bool = NO)
  'debug ">>sound_stop(" + trim(str(slot)) + ")"
  dim slot as integer
  slot = iif(num_is_slot, num, sound_slot_with_id(num))
  if slot = -1 then exit sub

  with SoundPool(slot)
    AudStop(.soundID)
    .paused = NO
  end with
  'debug "<<sound_stop"
end sub

sub sound_free(num as integer)
  for slot as integer = 0 to ubound(SoundPool)
    if SoundPool(slot).used and SoundPool(slot).effectID = num then
      sound_unload(slot)
    end if
  next
end sub

function sound_playing(num as integer, num_is_slot as bool = NO) as bool
  dim slot as integer
  slot = iif(num_is_slot, num, sound_slot_with_id(num))
  if slot = -1 then return NO

  return AudIsPlaying(SoundPool(slot).soundID) <> 0
end function



'-------------------------------------------------------------------------------



' Returns the first sound slot with the given sound effect ID (num);
' if the sound is not loaded, returns -1.
function sound_slot_with_id(num as integer) as integer
  dim slot as integer
  for slot = 0 to ubound(SoundPool)
    with SoundPool(slot)
      'debug "slot = " & slot & ", used = " & .used & ", effID = " _
      '      & .effectID & ", sndID = " & .soundID & ", AudIsValid = " & AudIsValidSound(.soundID)
      if .used andalso (.effectID = num or num = -1) andalso AudIsValidSound(.soundID) then return slot
    end with
  next
  return -1
end function

'Loads a sound into a slot, and marks its ID num (equal to OHR sfx number).
'Returns the slot number, or -1 if an error occurs.
function sound_load overload(lump as Lump ptr, num as integer = -1) as integer
  return -1
end function

function sound_load(fname as string, num as integer = -1) as integer
  ' 1. allocate space in the sound pool
  ' 2. load the sound

  dim slot as integer

  'iterate through the pool
  for slot = 0 to ubound(SoundPool)
    if SoundPool(slot).used = NO then exit for
  next

  'otherwise, slot will be left =ing SoundPool size + 1
  if slot = ubound(SoundPool) + 1 then
    'Grow the sound pool
    redim preserve SoundPool(ubound(SoundPool) * 1.5)
  end if

  'ok, now slot points at a valid slot. goody.

  ' 2. load the sound

  'TODO: abstract file name to a function or something
  'loadWaveFileToBuffer soundfile(num), @derbuffer

  dim extn as string = justextension(fname)
  dim audslot as integer  'Audiere sound number
  if extn = "mp3" or extn = "ogg" then 'intended for streaming
    audslot = AudLoadSound(fname, 1)
  else
    audslot = AudLoadSound(fname, 0)
  end if
  'debug "slot is " & audslot

  if audslot = -1 then return -1 'crap

  'if we got this far, yes!
  with SoundPool(slot)
    .used = YES
    .soundID = audslot
    .effectID = num
  end with

  return slot
end function

'Unloads a sound loaded in a slot. TAKES A SLOT, NOT AN SFX NUMBER!
sub sound_unload(slot as integer)
  with SoundPool(slot)
    if not .used then exit sub
    if AudIsValidSound(.soundID) then AudUnloadSound(.soundID)
    .used = NO
    .paused = NO
    .soundID = 0
    .effectID = 0
  end with
end sub
