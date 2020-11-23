'' This file contains code common to gfx_sdl and gfx_sdl2 graphics backends.
'' Note that it is included directly into gfx_sdl.bas and gfx_sdl2.bas and compiled twice
'' with macros SDL12 and SDL2.
''
'' Part of the OHRRPGCE - See LICENSE.txt for GNU GPL License details and disclaimer of liability




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

CONST maxJoysticks = 8

DIM SHARED joystickhandles(maxJoysticks - 1) as SDL_Joystick ptr
DIM SHARED joystickinfo(maxJoysticks - 1) as JoystickInfo


LOCAL SUB quit_joystick_subsystem()
  FOR i as integer = 0 TO small(SDL_NumJoysticks(), maxJoysticks) - 1
    IF joystickhandles(i) <> NULL THEN SDL_JoystickClose(joystickhandles(i))
    joystickhandles(i) = NULL
  NEXT
  SDL_QuitSubSystem(SDL_INIT_JOYSTICK)
END SUB

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
    joystick_counter += 1
    .instance_id = joystick_counter
    'Can't retrieve guid

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
