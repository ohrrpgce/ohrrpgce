#ifndef JO_JPEG_H
#define JO_JPEG_H

extern "C" {

typedef void jo_write_func(void *context, const void *data, int size);

// Returns false on failure
extern bool jo_write_jpg(const char *filename, const void *data, int width, int height, int comp, int quality);

// Returns false on failure
extern bool jo_write_jpg_to_func(jo_write_func *func, void *context, const void *data, int width, int height, int comp, int quality);

}

#endif
