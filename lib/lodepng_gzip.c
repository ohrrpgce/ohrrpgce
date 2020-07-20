/*
Read/write a gzip file using LodePNG's zlib implementation.
NOT part of LodePNG. compress_gzip() is based on example_gzip.cpp from LodePNG.
TODO: allow optionally compiling against zlib (on Unix), and use that instead
if compiled against it.

Copyright (c) 2018 Ralph Versteegen
Copyright (c) 2005-2012 Lode Vandevenne

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
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

    3. This notice may not be removed or altered from any source
    distribution.
*/

#include <stdlib.h>
#include "lodepng.h"
#include "lodepng_gzip.h"

// Bits in the FLG field
#define FTEXT     (1 << 0)
#define FHCRC     (1 << 1)
#define FEXTRA    (1 << 2)
#define FNAME     (1 << 3)
#define FCOMMENT  (1 << 4)
#define FRESERVED 0xe0

/*
Encodes data as a gzip file in-memory.
Returns 0 on success or a gzip_error_text() error code.
*outp and *outsizep are overwritten with the output buffer ptr/size.

See also the gzip specification, RFC 1952: http://www.gzip.org/zlib/rfc-gzip.html
*/
int compress_gzip(const unsigned char *in, size_t insize, unsigned char **outp, size_t *outsizep, int maxcomp)
{
  if (outp) *outp = NULL;
  if (outsizep) *outsizep = 0;

  if (!in || !outp || !outsizep)
    return GZ_BADCALL;

  size_t outsize = 10;
  unsigned char* out = (unsigned char*)malloc(outsize);
  out[0] = 31;  //ID1
  out[1] = 139; //ID2
  out[2] = 8; //CM (DEFLATE; only valid option)
  out[3] = 0; //FLG (No extra metadata)
  //MTIME
  out[4] = 0;
  out[5] = 0;
  out[6] = 0;
  out[7] = 0;

  out[8] = 2; //2 = slow, 4 = fast compression
  out[9] = 255; //OS unknown

  LodePNGCompressSettings settings = lodepng_default_compress_settings;
  settings.windowsize = maxcomp ? 32768 : 16384; // Default is only 2048, max is 32768.
  // LodePNG needs a windowsize of 32768 just to match gzip at default compression level.

  int ret;
  ret = lodepng_deflate(&out, &outsize, &in[0], insize, &settings);
  if (ret) {
    free(out);
    return ret;
  }

  unsigned crc = lodepng_crc32(&in[0], insize);

  size_t footer = outsize;

  outsize += 8;
  out = (unsigned char*)realloc(out, outsize);

  //CRC
  out[footer + 0] = crc % 256;
  out[footer + 1] = (crc >> 8) % 256;
  out[footer + 2] = (crc >> 16) % 256;
  out[footer + 3] = (crc >> 24) % 256;

  //ISIZE
  out[footer + 4] = insize % 256;
  out[footer + 5] = (insize >> 8) % 256;
  out[footer + 6] = (insize >> 16) % 256;
  out[footer + 7] = (insize >> 24) % 256;

  *outp = out;
  *outsizep = outsize;
  return GZ_SUCCESS;
}

/*
Decompress a gzip file in-memory.
Returns 0 on success or a gzip_error_text() error code.
*outp and *outsizep are overwritten with the output buffer ptr/size.
They might be set even if corruption is discovered and decompressing failed,
so the caller must still free them!
*/
int decompress_gzip(const unsigned char *in, size_t insize, unsigned char **outp, size_t *outsizep) {
  if (outp) *outp = NULL;
  if (outsizep) *outsizep = 0;

  if (!in || !outp || !outsizep)
    return GZ_BADCALL;
  if (insize < 18 || in[0] != 31 || in[1] != 139)
    return GZ_CORRUPT;

  int flg = in[3];

  if (in[2] != 8 || flg & FRESERVED)
    return GZ_UNSUPPORTED;  // CM isn't 8 (DEFLATE) or unknown flg bits set

  // Find start of the deflate stream

  const unsigned char* strm = in + 10;

  if (flg & FEXTRA)
  {
    // Skip over extra fields
    int xlen = strm[0] | (strm[1] << 8);
    strm += 2 + xlen;
  }

  if (flg & FNAME)
  {
    // Skip filename
    while (*strm++) {
      if (strm == in + insize)
        return GZ_CORRUPT;
    }
  }

  if (flg & FCOMMENT)
  {
    // Skip comment
    while (*strm++) {
      if (strm == in + insize)
        return GZ_CORRUPT;
    }
  }

  if (flg & FHCRC)
    strm += 2;  // Skip over 16 bit header CRC

  const unsigned char* crcp = &in[insize - 8];

  if (crcp < strm)
    return GZ_CORRUPT;  // Footer missing
  size_t strmsize = crcp - strm;

  int ret;
  ret = lodepng_inflate(outp, outsizep, strm, strmsize, &lodepng_default_decompress_settings);
  if (ret) {
    // Don't free partially decompressed data
    return ret;
  }
  unsigned int crc = crcp[0] | (crcp[1] << 8) | (crcp[2] << 16) | (crcp[3] << 24);

  if (crc != lodepng_crc32(*outp, *outsizep)) {
    // Don't free decompressed data
    return GZ_CORRUPT;
  }

  // Don't bother to check whether ISIZE was correct

  return GZ_SUCCESS;
}

const char* gzip_error_text(int code)
{
  switch (code)
  {
    case GZ_BADCALL:     return "bad function parameter";
    case GZ_CORRUPT:     return ".gz corrupt/too short";
    case GZ_UNSUPPORTED: return "unsupport compression or flags";
    default:             return lodepng_error_text(code);
  }
}
