'' Backend-independent graphics structs

#IFNDEF RGBcolor
'this is also defined in allmodex.bi

type RGBcolor
  as ubyte r, g, b
end type

#ENDIF

'' External graphics and IO functions

declare sub gfx_init()		'initilization, including gfx_screenres
declare sub gfx_screenres()		'set screen res, etc
declare sub gfx_close()		'put it back how we found it
declare sub gfx_showpage(byval raw as ubyte ptr) 'the main event
declare sub gfx_setpal(pal() as RGBcolor) 'set colour palette
declare function gfx_screenshot(fname as string, byval page as integer) as integer
declare sub gfx_setwindowed(byval iswindow as integer)
declare sub gfx_togglewindowed()
declare sub gfx_windowtitle(title as string)
declare sub gfx_setoption(opt as string, byval value as integer = -1)

declare sub io_init()
declare sub io_updatekeys(keybd() as integer)
declare function io_keypressed(byval scancode as integer) as integer
declare function io_enablemouse() as integer
declare sub io_getmouse(mx as integer, my as integer, mwheel as integer, mbuttons as integer)
declare sub io_setmouse(byval x as integer, byval y as integer)
declare sub io_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
declare function io_readjoy(joybuf() as integer, byval joynum as integer) as integer
declare function io_readjoysane(byval as integer, byref as integer, byref as integer, byref as integer) as integer
