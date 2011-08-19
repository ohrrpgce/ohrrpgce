//OHRRPGCE COMMON - Generic Unix versions of OS-specific routines
//Please read LICENSE.txt for GNU GPL License details and disclaimer of liability

#ifndef OS_H
#define OS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

#ifdef _WIN32

typedef int IPCChannel;  //dummy values
#define NULL_CHANNEL 0

#else

typedef FILE *IPCChannel;
#define NULL_CHANNEL NULL

#endif

int copy_file_replacing(const char *source, const char *destination);

//Advisory locking (actually mandatory on Windows)
int lock_file_for_write(FILE *fh, int timeout_ms);
int lock_file_for_read(FILE *fh, int timeout_ms);
void unlock_file(FILE *fh);
int test_locked(const char *filename, int writable);


//FBSTRING *channel_pick_name(const char *id, const char *tempdir, const char *rpg);
int channel_open_read(IPCChannel *result, FBSTRING *name);
int channel_open_write(IPCChannel *result, FBSTRING *name);
void channel_close(IPCChannel *channelp);
int channel_wait_for_client_connection(IPCChannel *channel, int timeout_ms);
int channel_write(IPCChannel *channel, const char *buf, int buflen);
int channel_input_line(IPCChannel *channel, FBSTRING *output);

#ifdef __cplusplus
}
#endif

#endif
