'OHRRPGCE - gfx_sdl/sdl2 shared code
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
'NOTE: this header should only be included in gfx_sdl.bas and gfx_sdl2.bas,
'because it declares private functions which might be compiled multiple times.


#if defined(USE_SDL)
  #define IO_SDL(name)  io_sdl_##name
  #define GFX_SDL(name) gfx_sdl_##name
#elseif defined(USE_SDL2)
  #define IO_SDL(name)  io_sdl2_##name
  #define GFX_SDL(name) gfx_sdl2_##name
#else
  #error Must define USE_SDL or USE_SDL2
#endif

CONST maxJoysticks = 8

DECLARE SUB GFX_SDL(close)()
DECLARE SUB quit_joystick_subsystem()
DECLARE FUNCTION IO_SDL(get_joystick_state)(byval joynum as integer, byval state as IOJoystickState ptr) as integer

DECLARE FUNCTION instance_to_joynum(instance_id as integer) as integer
DECLARE FUNCTION sdl2_joy_button_press(btn as integer, instance_id as integer) as bool

EXTERN joystickinfo(maxJoysticks - 1) as JoystickInfo
