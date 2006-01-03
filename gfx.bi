'' External graphics functions

declare sub gfx_init()		'set screen res, etc
declare sub gfx_close()		'put it back how we found it
declare sub gfx_showpage(byval raw as ubyte ptr) 'the main event
declare sub gfx_setpal(pal() as integer) 'set colour palette

declare function io_keypressed(byval scancode as integer) as integer