/*
  Mac OSX Clipboard routines -- adapted from SDL 2 for the OHRRPGCE.
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

#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include <ApplicationServices/ApplicationServices.h>
#include <Cocoa/Cocoa.h>

int clipboard_count;

int
Cocoa_SetClipboardText(const char *text)
{ @autoreleasepool
{
    NSPasteboard *pasteboard;
    NSString *format = NSPasteboardTypeString;

    pasteboard = [NSPasteboard generalPasteboard];
    clipboard_count = [pasteboard declareTypes:[NSArray arrayWithObject:format] owner:nil];
    [pasteboard setString:[NSString stringWithUTF8String:text] forType:format];

    return 0;
}}

char *
Cocoa_GetClipboardText()
{ @autoreleasepool
{
    NSPasteboard *pasteboard;
    NSString *format = NSPasteboardTypeString;
    NSString *available;
    char *text;

    pasteboard = [NSPasteboard generalPasteboard];
    available = [pasteboard availableTypeFromArray:[NSArray arrayWithObject:format]];
    if ([available isEqualToString:format]) {
        NSString* string;
        const char *utf8;

        string = [pasteboard stringForType:format];
        if (string == nil) {
            utf8 = "";
        } else {
            utf8 = [string UTF8String];
        }
        text = strdup(utf8);
    } else {
        text = strdup("");
    }

    return text;
}}

bool
Cocoa_HasClipboardText()
{
    bool result = false;
    char *text = Cocoa_GetClipboardText();
    if (text) {
        result = text[0] != '\0';
        free(text);
    }
    return result;
}

/*
void
Cocoa_CheckClipboardUpdate()
{ @autoreleasepool
{
    NSPasteboard *pasteboard;
    NSInteger count;

    pasteboard = [NSPasteboard generalPasteboard];
    count = [pasteboard changeCount];
    if (count != clipboard_count) {
        if (clipboard_count) {
            SDL_SendClipboardUpdate();
        }
        clipboard_count = count;
    }
}}
*/
