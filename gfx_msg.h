//gfx_msg.h
//started 12/31/09
//proposal for message system to the backends (for setting/getting options, etc.)

#ifndef GFX_MSG_H
#define GFX_MSG_H

#define OM_GFX_SETWIDTH			0x1 //sets client width
#define OM_GFX_SETHEIGHT		0x2 //sets client height
#define OM_GFX_SETCLIENTAREA	0x3 //sets client area
#define OM_GFX_SETLEFT			0x4 //sets window left
#define OM_GFX_SETTOP			0x5 //sets window top
#define OM_GFX_SETPOSITION		0x6 //sets window position
#define OM_GFX_SETARP			0x7 //sets aspect ratio preservation state
#define OM_GFX_SETWINDOWED		0x8 //sets windowed/fullscreen state
#define OM_GFX_SETSMOOTH		0x9 //sets smooth state
#define OM_GFX_SETVSYNC			0xA //sets vsync state
#define OM_GFX_SETSSFORMAT		0xB //sets screenshot format
#define OM_GFX_SETZOOM			0xC //sets zoom
#define OM_GFX_SETBITDEPTH		0xD //sets bit depth
#define OM_GFX_SETBORDER		0xE //sets border state

#define OM_GFX_GETWIDTH			0x101 //gets client width
#define OM_GFX_GETHEIGHT		0x102 //gets client height
#define OM_GFX_GETCLIENTAREA	0x103 //gets client area
#define OM_GFX_GETLEFT			0x104 //gets window left
#define OM_GFX_GETTOP			0x105 //gets window top
#define OM_GFX_GETPOSITION		0x106 //gets window position
#define OM_GFX_GETARP			0x107 //gets aspect ratio preservation state
#define OM_GFX_GETWINDOWED		0x108 //gets windowed/fullscreen state
#define OM_GFX_GETSMOOTH		0x109 //gets smooth state
#define OM_GFX_GETVSYNC			0x10A //gets vsync state
#define OM_GFX_GETSSFORMAT		0x10B //gets screenshot format
#define OM_GFX_GETZOOM			0x10C //gets zoom
#define OM_GFX_GETBITDEPTH		0x10D //gets bit depth
#define OM_GFX_GETBORDER		0x10E //gets border state

#endif
