//OHRRPGCE COMMON - Generic Unix versions of OS-specific routines
//Please read LICENSE.txt for GNU GPL License details and disclaimer of liability

#ifndef OS_H
#define OS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

#ifdef _WIN32

typedef int IPCChannel;  //dummy types
#define NULL_CHANNEL 0
typedef void *ProcessHandle;

#else

struct PipeState;
typedef struct PipeState PipeState;
typedef PipeState *IPCChannel;
#define NULL_CHANNEL NULL
typedef FILE *ProcessHandle;

#endif

void init_runtime();

int memory_usage();
FBSTRING *memory_usage_string();

int copy_file_replacing(const char *source, const char *destination);

//Advisory locking (actually mandatory on Windows)
int lock_file_for_write(FILE *fh, int timeout_ms);
int lock_file_for_read(FILE *fh, int timeout_ms);
void unlock_file(FILE *fh);
int test_locked(const char *filename, int writable);


//FBSTRING *channel_pick_name(const char *id, const char *tempdir, const char *rpg);
int channel_open_client(IPCChannel *result, FBSTRING *name);
int channel_open_server(IPCChannel *result, FBSTRING *name);
void channel_close(IPCChannel *channelp);
int channel_wait_for_client_connection(IPCChannel *channel, int timeout_ms);
int channel_write(IPCChannel *channel, const char *buf, int buflen);
int channel_write_string(IPCChannel *channel, FBSTRING *input);
int channel_input_line(IPCChannel *channel, FBSTRING *output);

ProcessHandle open_process (FBSTRING *program, FBSTRING *args);
ProcessHandle open_piped_process (FBSTRING *program, FBSTRING *args, IPCChannel *iopipe);
ProcessHandle open_console_process (FBSTRING *program, FBSTRING *args);
int process_running (ProcessHandle process, int *exitcode);
void kill_process (ProcessHandle process);
void cleanup_process (ProcessHandle *processp);

void os_get_screen_size(int *wide, int *high);

#ifdef __cplusplus
}
#endif

#endif
