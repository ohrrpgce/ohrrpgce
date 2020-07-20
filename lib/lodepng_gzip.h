
#ifndef LODEPNG_GZIP_H
#define LODEPNG_GZIP_H

enum {
  GZ_SUCCESS,
  GZ_BADCALL = -1,
  GZ_CORRUPT = -2,
  GZ_UNSUPPORTED = -3,
  // Positive values are LodePNG error codes
};

int compress_gzip(const unsigned char *in, size_t insize, unsigned char **outp, size_t *outsizep, int maxcomp);
int decompress_gzip(const unsigned char *in, size_t insize, unsigned char **outp, size_t *outsizep);
const char* gzip_error_text(int code);

#endif
