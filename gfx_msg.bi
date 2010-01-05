'gfx_msg.bi
'started 12/31/09
'proposal for message system to the backends (for setting/getting options, etc.)

#ifndef GFX_MSG_BI
#define GFX_MSG_BI

#define OM_GFX_SETWIDTH			&H1 'sets client width
#define OM_GFX_SETHEIGHT		&H2 'sets client height
#define OM_GFX_SETCLIENTAREA	&H3 'sets client area
#define OM_GFX_SETLEFT			&H4 'sets window left
#define OM_GFX_SETTOP			&H5 'sets window top
#define OM_GFX_SETPOSITION		&H6 'sets window position
#define OM_GFX_SETARP			&H7 'sets aspect ratio preservation state
#define OM_GFX_SETWINDOWED		&H8 'sets windowed/fullscreen state
#define OM_GFX_SETSMOOTH		&H9 'sets smooth state
#define OM_GFX_SETVSYNC			&HA 'sets vsync state
#define OM_GFX_SETSSFORMAT		&HB 'sets screenshot format
#define OM_GFX_SETZOOM			&HC 'sets zoom
#define OM_GFX_SETBITDEPTH		&HD 'sets bit depth
#define OM_GFX_SETBORDER		&HE 'sets border state

#define OM_GFX_GETWIDTH			&H101 'gets client width
#define OM_GFX_GETHEIGHT		&H102 'gets client height
#define OM_GFX_GETCLIENTAREA	&H103 'gets client area
#define OM_GFX_GETLEFT			&H104 'gets window left
#define OM_GFX_GETTOP			&H105 'gets window top
#define OM_GFX_GETPOSITION		&H106 'gets window position
#define OM_GFX_GETARP			&H107 'gets aspect ratio preservation state
#define OM_GFX_GETWINDOWED		&H108 'gets windowed/fullscreen state
#define OM_GFX_GETSMOOTH		&H109 'gets smooth state
#define OM_GFX_GETVSYNC			&H10A 'gets vsync state
#define OM_GFX_GETSSFORMAT		&H10B 'gets screenshot format
#define OM_GFX_GETZOOM			&H10C 'gets zoom
#define OM_GFX_GETBITDEPTH		&H10D 'gets bit depth
#define OM_GFX_GETBORDER		&H10E 'gets border state

#endif