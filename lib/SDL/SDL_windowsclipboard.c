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

#define WIN32_LEAN_AND_MEAN
#define STRICT
#define UNICODE 1
#define _WIN32_WINNT 0x0500  // For GetClipboardSequenceNumber

#include "SDL_windowsclipboard.h"
#include "internal.h"
#include "../../misc.h"
#include "../../gfx_common/ohrstring.hpp"

#define TEXT_FORMAT  CF_UNICODETEXT
//#define TEXT_FORMAT  CF_TEXT

int clipboard_count = 0;

////<< The following are from SDL_windows.c

/* Sets an error message based on GetLastError() */
int
WIN_SetErrorFromHRESULT(const char *prefix, HRESULT hr)
{
    TCHAR buffer[1024];
    char *message;
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, hr, 0,
                  buffer, SDL_arraysize(buffer), NULL);
    message = WstringToMBstring(buffer, CP_UTF8);
    debug(errInfo, "%s%s%s", prefix ? prefix : "", prefix ? ": " : "", message);
    free(message);
    return -1;
}

/* Sets an error message based on GetLastError() */
int
WIN_SetError(const char *prefix)
{
    return WIN_SetErrorFromHRESULT(prefix, GetLastError());
}

////>> End SDL_windows.c

int
WIN_SetClipboardText(HWND hWindow, const char *text)
{
    int result = 0;

    if (OpenClipboard(hWindow)) {
        HANDLE hMem;
        LPTSTR tstr;
        SIZE_T i, size;

        /* Convert the text from UTF-8 to Windows Unicode */
        tstr = MBstringToWstring(text, CP_UTF8);
        if (!tstr) {
            return -1;
        }

        /* Find out the size of the data */
        for (size = 0, i = 0; tstr[i]; ++i, ++size) {
            if (tstr[i] == '\n' && (i == 0 || tstr[i-1] != '\r')) {
                /* We're going to insert a carriage return */
                ++size;
            }
        }
        size = (size+1)*sizeof(*tstr);

        /* Save the data to the clipboard */
        hMem = GlobalAlloc(GMEM_MOVEABLE, size);
        if (hMem) {
            LPTSTR dst = (LPTSTR)GlobalLock(hMem);
            if (dst) {
                /* Copy the text over, adding carriage returns as necessary */
                for (i = 0; tstr[i]; ++i) {
                    if (tstr[i] == '\n' && (i == 0 || tstr[i-1] != '\r')) {
                        *dst++ = '\r';
                    }
                    *dst++ = tstr[i];
                }
                *dst = 0;
                GlobalUnlock(hMem);
            }

            EmptyClipboard();
            if (!SetClipboardData(TEXT_FORMAT, hMem)) {
                result = WIN_SetError("Couldn't set clipboard data");
            }
            clipboard_count = GetClipboardSequenceNumber();
        }
        free(tstr);

        CloseClipboard();
    } else {
        // Can happen if another program is using the clipboard
        result = WIN_SetError("Couldn't open clipboard");
    }
    return result;
}

char *
WIN_GetClipboardText(HWND hWindow)
{
    char *text;

    text = NULL;
    if (IsClipboardFormatAvailable(TEXT_FORMAT) &&
        OpenClipboard(hWindow)) {
        HANDLE hMem;
        LPTSTR tstr;

        hMem = GetClipboardData(TEXT_FORMAT);
        if (hMem) {
            tstr = (LPTSTR)GlobalLock(hMem);
            text = WstringToMBstring(tstr, CP_UTF8);
            GlobalUnlock(hMem);
        } else {
            // Not an error; e.g. the clipboard doesn't contain text
            //WIN_SetError("Couldn't get clipboard data");
        }
        CloseClipboard();
    }
    if (!text) {
        text = strdup("");
    }
    return text;
}

bool
WIN_HasClipboardText(HWND hWindow)
{
    bool result = false;
    char *text = WIN_GetClipboardText(hWindow);
    if (text) {
        result = text[0] != '\0';
        free(text);
    }
    return result;
}

/*
void
WIN_CheckClipboardUpdate(struct SDL_VideoData * data)
{
    const DWORD count = GetClipboardSequenceNumber();
    if (count != clipboard_count) {
        if (clipboard_count) {
            SDL_SendClipboardUpdate();
        }
        clipboard_count = count;
    }
}
*/
