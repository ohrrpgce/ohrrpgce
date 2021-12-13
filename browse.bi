'OHRRPGCE - File Browser
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#ifndef BROWSE_BI
#define BROWSE_BI

'Each file types has a set of default file extensions it looks for. Pass
'fmask only if the defaults aren't right (e.g. only .slice rather than all RELOAD files)
ENUM BrowseFileType
 browseAny,           'no preview (fmask must be given!)
 browseMusic,         'any supported music (.bam, .mid, .ogg, .mp3, .mod, .xm, .it, .s3m)
 browseSfx,           'any supported SFX (.ogg, .wav, .mp3)
 browseRPG,           'RPG files and .rpgdir
 browseRELOAD,        'any kind of RELOAD file
 browseScripts,       'script files (.hs, .hss, .hsp .txt)
 browseImage,         'any supported image (bmp, png, jpeg, jpg)
 browseTileset,       'tilesets: 320x200 image
 browseMasterPal,     'master palette (.mas, 8 bit or 16x16 .bmp or .png)
 browsePalettedImage, 'Paletted (<= 8 bit) image, any size
 browseDir,           'Browse for a folder
 browseTilemap,       'tilemaps (.tilemap)
END ENUM

DECLARE FUNCTION browse (filetype as BrowseFileType, byref default as string = "", fmask as string = "", helpkey as string = "", needf as bool = NO) as string
DECLARE SUB set_browse_default (default as string)

#ENDIF
