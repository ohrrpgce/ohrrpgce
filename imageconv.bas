'OHRRPGCE - Image format conversion test utility
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
'
'This is just an example of a commandline program that links to allmodex.bas, common.rbas, etc.
'NOTE: to compile, run "scons gfx=dummy imageconv"

#include "config.bi"
#include "common.bi"
#include "allmodex.bi"
#include "lib/lodepng_gzip.bi"

sub usage()
        print "Convert 1/2/4/8/24-bit image from one supported file format"
        print "(BMP, PNG, JPG) to another (BMP, PNG, JPG, GIF)."
        print "Usage: " & COMMAND(0) & " [options] <infile> <outfile>"
        print "Options:"
        print " -q #: Quality: for JPEG 0-100 (default 95);"
        print "                for PNG 0-2 (default 1), affects speed/ratio only"
        print " -pal: Convert to paletted (affects PNG/BMP output only)"
        print " -24:  Convert to 24-bit (affects PNG/BMP output only)"
        print " -d:   Dither (only when converting to paletted)"
	end 2
end sub

dim to_pal as bool = NO
dim to_24 as bool = NO
dim dither as bool = NO
dim quality as integer = -1
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

dim desttype as ImageFileTypes = image_file_type(dest)
if desttype = imJPEG then
        to_24 = YES
        to_pal = NO
end if
if desttype = imJPEG then
        if quality < 0 then quality = 95
elseif desttype = imPNG then
        if quality < 0 then quality = 1
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

dim pal(255) as RGBColor

if ends_with(dest, ".bmp.gz") then
	dim surf as Surface ptr
	surf = image_import_as_surface(src, YES)  'always_32bit=YES
	if surf = 0 then fatalerror "Couldn't read file"
	image_load_palette(src, pal())

	dim tempfile as string = "_imageconv.bmp.tmp"
	surface_export_bmp tempfile, surf, pal()
	dim indata as string = read_file(tempfile)
	kill tempfile

	'Compress
	dim outdata as byte ptr
	dim outdatasize as size_t
	dim starttime as double = timer
	starttime = timer
	if compress_gzip(strptr(indata), len(indata), @outdata, @outdatasize, quality) then fatalerror "Compress failed"
	print "lode-gzip compressed in " & cint((timer - starttime) * 1e3) & " ms"

	dim fil as FILE ptr
	fil = fopen(strptr(dest), "wb")
	if fil = NULL then fatalerror "fopen dest failed"
	if fwrite(outdata, 1, outdatasize, fil) <> outdatasize then fatalerror "fwrite to dest failed"
	fclose(fil)
	deallocate outdata

elseif info.paletted andalso to_24 = NO then
        ' Paletted input and output. Copy palette.
        dim fr as Frame ptr
        fr = image_import_as_frame_paletted(src, pal())
        if fr = 0 then fatalerror "Couldn't read file"
        if desttype = imPNG then
                frame_export_png fr, dest, pal(), , quality
        else
                frame_export_image fr, dest, pal()
        end if

elseif to_pal then
        ' 24/32 bit input, <= 8 bit output (alpha channel dropped)
        dim options as QuantizeOptions
        options.compute_palette = YES
        options.dither = dither
        options.dither_maxerror = 50
        dim pal(255) as RGBColor
        dim fr as Frame ptr
        fr = image_import_as_frame_quantized(src, pal(), options)
        if fr = 0 then fatalerror "Couldn't read file"
        if desttype = imPNG then
                frame_export_png fr, dest, pal(), , quality
        else
                frame_export_image fr, dest, pal()
        end if

else
        ' Any input, 24 bit output (alpha channel will be dropped)
        ' Ugly kludge to disable dither in lib/gif.h by adjusting this global.
        if dither = NO then kGifMaxAccumError = 0
        dim surf as Surface ptr
        surf = image_import_as_surface(src, YES)  'always_32bit=YES
        if surf = 0 then fatalerror "Couldn't read file"
        if desttype = imJPEG then
                surface_export_jpeg surf, dest, quality
        elseif desttype = imPNG then
                surface_export_png surf, dest, pal(), , quality  'pal() ignored
        else
                surface_export_image surf, dest
        end if
end if
