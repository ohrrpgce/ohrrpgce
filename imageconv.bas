'This is just an example of a commandline program that links to allmodex.bas, common.rbas, etc.
'NOTE: to compile, run "scons gfx=dummy imageconv"

#include "config.bi"
#include "common.bi"
#include "allmodex.bi"

dim src as string = COMMAND(1)
dim dest as string = COMMAND(2)

if COMMAND = "" orelse isfile(src) = NO then
        print "Convert 1/2/4/8/24-bit image from one supported file format"
        print "(BMP, PNG, JPG) to another (BMP, PNG, JPG, GIF)."
        print "Usage: " & COMMAND(0) & " <infile> <outfile>"
	end 0
end if

'Needed to load Surface function pointers
prefer_gfx_backend "dummy"
setmodex

dim info as ImageFileInfo
info = image_read_info(src)
print trimpath(src) & ": " & info.info

if info.supported = NO then  ' Unreadable, invalid, or unsupported
	print "Not supported: " & info.error
	end 1
end if

if info.paletted then
        ' <= 8 bit, write a paletted output file. PNG and BMP only.
        dim pal(255) as RGBColor
        dim fr as Frame ptr
        fr = image_import_as_frame_paletted(src, pal())
        if fr then frame_export_image fr, dest, pal()
else
        ' 24/32 bit (but alpha channel will be dropped)
        dim surf as Surface ptr
        surf = image_import_as_surface(src, YES)  'always_32bit=YES
        if surf then surface_export_image surf, dest
end if
