//OHHRPGCE COMMON - Generic Unix versions of OS-specific routines
//Please read LICENSE.txt for GNU GPL License details and disclaimer of liability

#ifndef OS_H
#define OS_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _WIN32

typedef int IPCChannel;  //dummy values
#define NULL_CHANNEL 0

#else

typedef FILE *IPCChannel;
#define NULL_CHANNEL NULL

#endif

int channel_open_read(FBSTRING *name, IPCChannel *result);
int channel_open_write(FBSTRING *name, IPCChannel *result);
void channel_close(IPCChannel *channel);
int channel_write(IPCChannel channel, char *buf, int buflen);
int channel_input_line(IPCChannel channel, FBSTRING *output);

#ifdef __cplusplus
}
#endif

#endif
