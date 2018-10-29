'NOTE: this header should only be included in gfx_sdl.bas and gfx_sdl2.bas,
'because it declares private functions which might be compiled multiple times.


#if defined(USE_SDL)
  #define IO_SDL(name) io_sdl_##name
#elseif defined(USE_SDL2)
  #define IO_SDL(name) io_sdl2_##name
#else
  #error Must define USE_SDL or USE_SDL2
#endif

DECLARE SUB quit_joystick_subsystem()
DECLARE FUNCTION IO_SDL(get_joystick_state)(byval joynum as integer, byval state as IOJoystickState ptr) as integer
