'OHRRPGCE - gfx_sdl/gfx_sdl2 shared routines
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
' This file contains code common to gfx_sdl and gfx_sdl2 graphics backends.
' Note that it is included directly into gfx_sdl.bas and gfx_sdl2.bas and compiled twice
' with macros SDL12 and SDL2.


#ifdef USE_SDL2
 DECLARE FUNCTION sdl2_update_gamepad(joynum as integer, state as IOJoystickState ptr) as integer
#endif


SUB GFX_SDL(close)()
  'debug "quit_joystick_subsystem"
  IF SDL_WasInit(SDL_INIT_JOYSTICK) THEN
    quit_joystick_subsystem()
  END IF

  'debug "quit_video_subsystem"
  IF SDL_WasInit(SDL_INIT_VIDEO) THEN
    quit_video_subsystem()

    IF SDL_WasInit(0) = 0 THEN
      SDL_Quit()
    END IF
  END IF
END SUB


'==============================================================================
'                                   Joysticks

DIM SHARED joystickhandles(maxJoysticks - 1) as SDL_Joystick ptr
DIM joystickinfo(maxJoysticks - 1) as JoystickInfo
'The following is only used by gfx_sdl2, for button press events, to track
'fast presses that are released between ticks.
DIM SHARED joystickbuttons(maxJoysticks - 1) as uinteger

LOCAL SUB quit_joystick_subsystem()
  FOR i as integer = 0 TO small(SDL_NumJoysticks(), maxJoysticks) - 1
    IF joystickhandles(i) <> NULL THEN SDL_JoystickClose(joystickhandles(i))
    joystickhandles(i) = NULL
    joystickinfo(i).instance_id = -1
  NEXT
  SDL_QuitSubSystem(SDL_INIT_JOYSTICK)
END SUB

'Return -1 if not found
LOCAL FUNCTION instance_to_joynum(instance_id as integer) as integer
  FOR joynum as integer = 0 TO UBOUND(joystickinfo)
    IF joystickinfo(joynum).instance_id = instance_id THEN RETURN joynum
  NEXT
  RETURN -1
END FUNCTION

'Check a joystick is valid, and open it if not open yet, reading info
'Same return values as io_get_joystick_state in gfx.bi
LOCAL FUNCTION get_joystick(byval joynum as integer) as integer
  'SDL1 & 2 report joystick state even when the app isn't focused (under both Linux and Windows)
  #ifdef USE_SDL2
    IF (SDL_GetWindowFlags(mainwindow) AND SDL_WINDOW_INPUT_FOCUS) = 0 THEN RETURN 3
  #else
    IF (SDL_GetAppState() AND SDL_APPINPUTFOCUS) = 0 THEN RETURN 3
  #endif

  IF joynum < 0 ORELSE joynum >= maxJoysticks THEN RETURN 1
  IF joynum > SDL_NumJoysticks() - 1 THEN RETURN 1
  'FIXME: this doesn't handle joysticks getting renumbered as they are plugged/unplugged
  '(SDL 1.2 doesn't support that, though)
  DIM byref joy as SDL_Joystick ptr = joystickhandles(joynum)
  IF joy THEN RETURN 0  'Success

  joy = SDL_JoystickOpen(joynum)
  IF joy = NULL THEN
    debug "Couldn't open joystick " & joynum & ": " & *SDL_GetError
    RETURN 1
  END IF

  STATIC joystick_counter as integer

  WITH joystickinfo(joynum)
    .structsize = JOYSTICKINFO_SZ
    DIM joyname as const zstring ptr
    #ifdef USE_SDL2
      joyname = SDL_JoystickNameForIndex(joynum)
    #else
      joyname = SDL_JoystickName(joynum)
    #endif
    IF joyname = NULL THEN joyname = @"(NULL)"
    .name = *joyname

    .num_buttons = SDL_JoystickNumButtons(joy)
    .num_axes = SDL_JoystickNumAxes(joy)
    .num_hats = SDL_JoystickNumHats(joy)
    .num_balls = SDL_JoystickNumBalls(joy)
    #ifdef USE_SDL2
      .instance_id = SDL_JoystickInstanceID(joy)

      DIM GUID as SDL_JoystickGUID = SDL_JoystickGetGUID(joy)
      memcpy(@.model_guid(0), @GUID, sizeof(SDL_JoystickGUID))

      DIM GUIDstr as string * 33
      SDL_JoystickGetGUIDString(GUID, @GUIDstr[0], 32)
      debuginfo "Joystick " & joynum & " GUID " & GUIDstr & " instance_id " & .instance_id

      'Open the controller, but no need to store the pointer because we can look it up again
      .have_bindings = SDL_IsGameController(joynum)
      IF .have_bindings THEN
        VAR controller = SDL_GameControllerOpen(joynum)
        IF controller THEN
          'This name may vary from SDL_JoystickNameForIndex
          debuginfo " Opened as gamecontroller " & SDL_GameControllerName(controller)
          .num_buttons = large(.num_buttons, joyLASTGAMEPAD)
          .num_axes = large(.num_buttons, axisLASTGAMEPAD)
        ELSE
          debug "Couldn't open gamecontroller " & joynum & ": " & *SDL_GetError
          .have_bindings = NO
        END IF
      END IF

    #else
      joystick_counter += 1
      .instance_id = joystick_counter
      'Can't even retrieve guid under SDL1.2
    #endif

    debuginfo strprintf("Opened joystick %d %s (id %d) -- %d buttons %d axes %d hats %d balls", _
                        joynum, joyname, .instance_id, .num_buttons, .num_axes, .num_hats, .num_balls)

    .num_buttons = small(32, .num_buttons)
    .num_axes = small(8, .num_axes)
    .num_hats = small(4, .num_hats)
  END WITH
  RETURN -1  'Acquired joystick
END FUNCTION

FUNCTION IO_SDL(get_joystick_state)(byval joynum as integer, byval state as IOJoystickState ptr) as integer
  DIM ret as integer = get_joystick(joynum)
  IF ret > 0 THEN RETURN ret  'Failure

  DIM byref joy as SDL_Joystick ptr = joystickhandles(joynum)

  'Fixed joystick info
  state->info = @joystickinfo(joynum)

  'We can assume that state has already been cleared.
  DIM idx as integer
  WITH *state

    .buttons_new = joystickbuttons(joynum)
    joystickbuttons(joynum) = 0

    IF .info->have_bindings THEN
      #ifdef USE_SDL2
        ret = sdl2_update_gamepad(joynum, state)
      #endif
    ELSE
      '?"no binding"
      FOR idx = 0 TO .info->num_buttons - 1
        IF SDL_JoystickGetButton(joy, idx) THEN .buttons_down OR= 1 SHL idx
      NEXT

      FOR idx = 0 TO .info->num_axes - 1
        'Has range -32768 (SDL_JOYSTICK_AXIS_MIN) to 32767 (SDL_JOYSTICK_AXIS_MAX), which is pretty odd...
        .axes(idx) = large(-1000, SDL_JoystickGetAxis(joy, idx) * 1000 \ 32767)
      NEXT

      FOR idx = 0 TO .info->num_hats - 1
        DIM vec as integer = SDL_JoystickGetHat(joy, idx)
        IF vec AND SDL_HAT_LEFT  THEN .hats(idx) OR= 1
        IF vec AND SDL_HAT_RIGHT THEN .hats(idx) OR= 2
        IF vec AND SDL_HAT_UP    THEN .hats(idx) OR= 4
        IF vec AND SDL_HAT_DOWN  THEN .hats(idx) OR= 8
      NEXT

    END IF

    IF debugging_io THEN
      STATIC last_state(maxJoysticks - 1) as string
      DIM temp as string = lpad(BIN(.buttons_down), "0", .info->num_buttons)
      DIM msg as string = strprintf("joy %d buttons: 0b%s", joynum, STRPTR(temp))
      FOR idx = 0 TO .info->num_axes - 1
        msg &= strprintf(" axis%d: %6d", idx, .axes(idx))
      NEXT
      FOR idx = 0 TO .info->num_hats - 1
        msg &= strprintf(" hat%d: 0x%x", idx, .hats(idx))  'Value from 0-15
      NEXT
      FOR idx = 0 TO .info->num_balls - 1
        DIM as integer bx, by
        'NOTE: This is relative movement since last call, so will break if we ever support balls!
        SDL_JoystickGetBall(joy, idx, @bx, @by)
        msg &= strprintf(" ball%d: %d,%d", idx, bx, by)
      NEXT
      IF last_state(joynum) <> msg THEN
        debuginfo msg
        last_state(joynum) = msg
      END IF
    END IF
  END WITH

  RETURN ret
END FUNCTION

#ifdef USE_SDL2

#include "scancodes.bi"

END EXTERN ' Can't put assignment statements in an extern block

DIM SHARED ohr_gamepad_axes(0 to SDL_CONTROLLER_AXIS_MAX) as JoyAxis
ohr_gamepad_axes(SDL_CONTROLLER_AXIS_LEFTX) = axisX
ohr_gamepad_axes(SDL_CONTROLLER_AXIS_LEFTY) = axisY
ohr_gamepad_axes(SDL_CONTROLLER_AXIS_RIGHTX) = axisRightX
ohr_gamepad_axes(SDL_CONTROLLER_AXIS_RIGHTY) = axisRightY
ohr_gamepad_axes(SDL_CONTROLLER_AXIS_TRIGGERLEFT) = axisL2
ohr_gamepad_axes(SDL_CONTROLLER_AXIS_TRIGGERRIGHT) = axisR2

DIM SHARED ohr_gamepad_buttons(0 to SDL_CONTROLLER_BUTTON_MAX) as JoyButton
ohr_gamepad_buttons(SDL_CONTROLLER_BUTTON_A) = joyA
ohr_gamepad_buttons(SDL_CONTROLLER_BUTTON_B) = joyB
ohr_gamepad_buttons(SDL_CONTROLLER_BUTTON_X) = joyX
ohr_gamepad_buttons(SDL_CONTROLLER_BUTTON_Y) = joyY
ohr_gamepad_buttons(SDL_CONTROLLER_BUTTON_BACK) = joyBack
ohr_gamepad_buttons(SDL_CONTROLLER_BUTTON_GUIDE) = joyGuide
ohr_gamepad_buttons(SDL_CONTROLLER_BUTTON_START) = joyStart
ohr_gamepad_buttons(SDL_CONTROLLER_BUTTON_LEFTSTICK) = joyLeftStick
ohr_gamepad_buttons(SDL_CONTROLLER_BUTTON_RIGHTSTICK) = joyRightStick
ohr_gamepad_buttons(SDL_CONTROLLER_BUTTON_LEFTSHOULDER) = joyL1
ohr_gamepad_buttons(SDL_CONTROLLER_BUTTON_RIGHTSHOULDER) = joyR1
ohr_gamepad_buttons(SDL_CONTROLLER_BUTTON_DPAD_UP) = joyUp
ohr_gamepad_buttons(SDL_CONTROLLER_BUTTON_DPAD_DOWN) = joyDown
ohr_gamepad_buttons(SDL_CONTROLLER_BUTTON_DPAD_LEFT) = joyLeft
ohr_gamepad_buttons(SDL_CONTROLLER_BUTTON_DPAD_RIGHT) = joyRight

EXTERN "C"

'Return value: see io_get_joystick_state
FUNCTION sdl2_update_gamepad(joynum as integer, state as IOJoystickState ptr) as integer
  WITH *state
    '.attached = SDL_ControllerGetAttached(controller)

    DIM controller as SDL_GameController ptr
    #IFNDEF SDL_203
      'This returns NULL if not already opened, and it doesn't increment refcount
      controller = SDL_GameControllerFromInstanceID(state->info->instance_id)
    #ELSE
      controller = SDL_GameControllerOpen(joynum)
    #ENDIF
    IF controller = NULL THEN
      'FIXME: not really handling renumbering properly
      'SDL_GetError doesn't report anything
      debug "Lost game controller " & state->info->name
      state->info->instance_id = -1
      state->info->have_bindings = NO
      RETURN 2  'Lost joystick
    END IF

    DIM buttons as uinteger = 0

    FOR idx as integer = 0 TO SDL_CONTROLLER_BUTTON_MAX
      IF SDL_GameControllerGetButton(controller, idx) THEN
        'io_get_joystick_state returns gamepad buttons starting with joyButton1 in the first bit
        '? "raw SDL button " & idx & " " & *SDL_GameControllerGetStringForButton(idx)
        buttons OR= 1 SHL (ohr_gamepad_buttons(idx) - 1)
      END IF
    NEXT

    .buttons_down = buttons

    FOR idx as integer = 0 TO SDL_CONTROLLER_AXIS_TRIGGERRIGHT  'Axes SDL_CONTROLLER_AXIS_MAX and above not supported
      DIM off as integer = SDL_GameControllerGetAxis(controller, idx)  'Range -32768 to 32767
      IF off THEN
        '? "raw SDL axis " & idx & " ohrax=" & ohr_gamepad_axes(idx) &  " " & off
      END IF
      off = (off * 1000.0) / 32767.
      .axes(ohr_gamepad_axes(idx)) = off 'bound(CINT(off), -1000, 1000)
    NEXT

    #IFDEF SDL_203
      SDL_GameControllerClose(controller)
    #ENDIF

  END WITH
  RETURN 0  'Success
END FUNCTION

'Record a button press (when a button down event happens)
FUNCTION sdl2_joy_button_press(btn as integer, instance_id as integer) as bool
  DIM joynum as integer = instance_to_joynum(instance_id)
  IF joynum < 0 THEN RETURN NO
  joystickbuttons(joynum) OR= 1 SHL (ohr_gamepad_buttons(btn) - 1)
  RETURN YES
END FUNCTION

#endif  'USE_SDL2
