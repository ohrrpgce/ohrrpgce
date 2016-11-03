// OHRRPGCE - Unicode routines
// 
// This file is placed under the following license:
// 
// Copyright (c) 2008-2009 Bjoern Hoehrmann <bjoern@hoehrmann.de>
// See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
// Copyright (c) 2012,2016 Ralph Versteegen
// 
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


#include <stdlib.h>
#include <stdint.h>
#include <wchar.h>

#define UTF8_ACCEPT 0
#define UTF8_REJECT 1

static const uint8_t utf8d[] = {
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 00..1f
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 20..3f
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 40..5f
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 60..7f
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9, // 80..9f
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7, // a0..bf
  8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, // c0..df
  0xa,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x3,0x4,0x3,0x3, // e0..ef
  0xb,0x6,0x6,0x6,0x5,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8,0x8, // f0..ff
  0x0,0x1,0x2,0x3,0x5,0x8,0x7,0x1,0x1,0x1,0x4,0x6,0x1,0x1,0x1,0x1, // s0..s0
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1, // s1..s2
  1,2,1,1,1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1, // s3..s4
  1,2,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,3,1,1,1,1,1,1, // s5..s6
  1,3,1,1,1,1,1,3,1,3,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // s7..s8
};

// Given an initial state and the next byte of a UTF8 string, decode it.
// *state should be initialised to UTF8_ACCEPT at the start of the string.
// After each character if *state is UTF8_ACCEPT, a complete character has
// decoded and *codep holds the codepoint, if *state is UTF8_REJECT the
// sequence is invalid (and *state stays that way), otherwise in the middle
// of a character.
static uint32_t decode_utf8_char(uint32_t* state, uint32_t* codep, uint32_t byte) {
	uint32_t type = utf8d[byte];

	*codep = (*state != UTF8_ACCEPT) ?
		(byte & 0x3fu) | (*codep << 6) :
		(0xff >> type) & (byte);

	*state = utf8d[256 + *state*16 + type];
	return *state;
}

// In codepoints. Returns negative value if invalid (actually position of bad character)
ssize_t utf8_length(unsigned char* s) {
	uint32_t codepoint = 0;
	uint32_t state = UTF8_ACCEPT;
	ssize_t count = 0;

	for (count = 0; *s; ++s) {
		if (decode_utf8_char(&state, &codepoint, *s) == UTF8_ACCEPT)
			count += 1;
		if (state == UTF8_REJECT)
			return -2 - count;
	}

	if (state != UTF8_ACCEPT)
		return -1;
	return count;
}

// Returns NULL on failure, otherwise returns an allocated UCS2 or UTF32, depending on system, string
// If length is not NULL and there was no error, then it is filled with the length
wchar_t *utf8_decode(unsigned char *input, ssize_t *length) {
	ssize_t len = utf8_length(input);
	if (len <= -1)
		return NULL;
	if (length)
		*length = len;

	uint32_t codepoint = 0;
	uint32_t state = UTF8_ACCEPT;
	wchar_t *ret, *outchar;
	outchar = ret = (wchar_t*)malloc((len + 1) * sizeof(wchar_t));

	while (*input) {
		if (decode_utf8_char(&state, &codepoint, *input++) == UTF8_ACCEPT) {
			if (codepoint > WCHAR_MAX)
				codepoint = L'?';

			*outchar++ = codepoint;
		}
	}
	*outchar = L'\0';
	return ret;
}

// Process a nul-terminated wstring into a char* buffer of size 'outsize' bytes.
void wstring_to_latin1(wchar_t *input, unsigned char *output, ssize_t outsize) {
	if (outsize <= 0) return;

	while (*input && outsize-- > 1) {
		if (*input > 255)
			*output++ = '?';
		else
			*output++ = *input;
		input++;
	}
	*output = '\0';
}
