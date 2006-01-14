'' External graphics and IO functions

declare sub gfx_init()		'set screen res, etc
declare sub gfx_close()		'put it back how we found it
declare sub gfx_showpage(byval raw as ubyte ptr) 'the main event
declare sub gfx_setpal(pal() as integer) 'set colour palette
declare function gfx_screenshot(fname as string, byval page as integer) as integer

declare sub io_init()
declare function io_keypressed(byval scancode as integer) as integer
declare function io_enablemouse() as integer
declare sub io_getmouse(mx as integer, my as integer, mwheel as integer, mbuttons as integer)
declare sub io_setmouse(byval x as integer, byval y as integer)
declare sub io_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
declare function io_readjoy(joybuf() as integer, byval joynum as integer) as integer
