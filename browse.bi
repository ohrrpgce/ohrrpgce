'OHRRPGCE - File Browser
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#ifndef BROWSE_BI
#define BROWSE_BI


ENUM BrowseFileType
' Note: I don't think all specials that ignore fmask are documented; many assume it is correctly given
 browseAny,           'no preview
 browseSprite,        'any BMP (sprite import)
 browseTileset,       'tilesets: 320x200 images
 browseMasterPal,     'master palette (*.mas, 8 bit *.bmp, 16x16 24/32 bit *.bmp) (fmask is ignored)
 browseMusic,         'any supported music (currently *.bam, *.mid, *.ogg, *.mp3, *.mod, *.xm, *.it, *.s3m formats)  (fmask is ignored)
 browseSfx,           'any supported SFX (currently *.ogg, *.wav, *.mp3) (fmask is ignored)
 browseRPG,           'RPG files and .rpgdir (fmask ignored)
 browseRELOAD,        'any kind of RELOAD file (fmask ignored)
 browseScripts,       'script files (.hs, .hss, .txt)
 browsePalettedImage, 'Paletted (<= 8 bit) image, any size (fmask ignored)
 browseDir,           'Browse for a folder
 browseTilemap,       'tilemaps: .tilemap file (fmask is ignored)
 browseImage,         'any supported image (bmp, png, jpeg, jpg)
END ENUM

DECLARE FUNCTION browse (filetype as BrowseFileType, byref default as string, fmask as string = "", helpkey as string = "", needf as bool = NO) as string
DECLARE SUB set_browse_default (default as string)

#ENDIF
