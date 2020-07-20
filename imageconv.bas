'This is just an example of a commandline program that links to allmodex.bas, common.rbas, etc.
'NOTE: to compile, run "scons gfx=dummy imageconv"

#include "config.bi"
#include "common.bi"
#include "allmodex.bi"

sub usage()
        print "Convert 1/2/4/8/24-bit image from one supported file format"
        print "(BMP, PNG, JPG) to another (BMP, PNG, JPG, GIF)."
        print "Usage: " & COMMAND(0) & " [options] <infile> <outfile>"
        print "Options:"
        print " -pal: Convert to paletted (affects PNG/BMP output only)"
        print " -24:  Convert to 24-bit (affects PNG/BMP output only)"
        print " -d:   Dither (only when converting to paletted)"
	end 2
end sub

dim to_pal as bool = NO
dim to_24 as bool = NO
dim dither as bool = NO
dim quality as integer = 95
dim cmdidx as integer = 1
do
        if COMMAND(cmdidx) = "-pal" then
                to_pal = YES
        elseif COMMAND(cmdidx) = "-24" then
                to_24 = YES
        elseif COMMAND(cmdidx) = "-d" then
                dither = YES
        elseif COMMAND(cmdidx) = "-q" then
                'For jpeg 0-100, for png 0-2
                cmdidx += 1
                quality = VALINT(COMMAND(cmdidx))
                print "Quality " & quality
        else
                exit do
        end if
        cmdidx += 1
loop

dim src as string = COMMAND(cmdidx)
dim dest as string = COMMAND(cmdidx + 1)

if isfile(src) = NO orelse dest = "" then usage
if COMMAND(cmdidx + 2) <> "" then usage


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

dim pal(255) as RGBColor

if info.paletted andalso to_24 = NO then
        ' Paletted input and output. Copy palette.
        dim fr as Frame ptr
        fr = image_import_as_frame_paletted(src, pal())
        if fr then frame_export_image fr, dest, pal()

elseif to_pal then
        ' 24/32 bit input, <= 8 bit output (alpha channel dropped)
        dim options as QuantizeOptions
        options.compute_palette = YES
        options.dither = dither
        options.dither_maxerror = 50
        dim pal(255) as RGBColor
        dim fr as Frame ptr
        fr = image_import_as_frame_quantized(src, pal(), options)
        if fr then frame_export_image fr, dest, pal()

else
        ' Any input, 24 bit output (alpha channel will be dropped)
        ' Ugly kludge to disable dither in lib/gif.h by adjusting this global.
        if dither = NO then kGifMaxAccumError = 0
        dim surf as Surface ptr
        surf = image_import_as_surface(src, YES)  'always_32bit=YES
        if surf = 0 then fatalerror "Couldn't read file"
        if image_file_type(dest) = imJPEG then
                surface_export_jpeg surf, dest, quality
        elseif image_file_type(dest) = imPNG then
                if quality = 95 then quality = 1  'Default
                surface_export_png surf, dest, pal(), , quality  'pal() ignored
        else
                surface_export_image surf, dest
        end if
end if
