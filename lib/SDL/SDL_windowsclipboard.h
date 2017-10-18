/*
  Windows Clipboard routines -- adapted from SDL 2 for the OHRRPGCE.
  Copyright 2017. This file is distributed under the original license, as
  follows, rather than the OHRRPGCE's license.
  Simple DirectMedia Layer
  Copyright (C) 1997-2017 Sam Lantinga <slouken@libsdl.org>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
*/

#ifndef _SDL_windowsclipboard_h
#define _SDL_windowsclipboard_h

#include <stdbool.h>

#define WIN32_LEAN_AND_MEAN
#define STRICT
#define UNICODE 1
#undef _WIN32_WINNT
#define _WIN32_WINNT  0x501   /* Need 0x410 for AlphaBlend() and 0x500 for EnumDisplayDevices(), 0x501 for raw input */
#include <windows.h>

extern int WIN_SetClipboardText(HWND hWindow, const char *text);
extern char *WIN_GetClipboardText(HWND hWindow);
extern bool WIN_HasClipboardText(HWND hWindow);

#endif /* _SDL_windowsclipboard_h */

/* vi: set ts=4 sw=4 expandtab: */
