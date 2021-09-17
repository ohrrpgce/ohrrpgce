/* OHRRPGCE - routines for abstracting away OS differences
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
 * Please read LICENSE.txt for GPL License details and disclaimer of liability
 */

#ifndef OS_H
#define OS_H

#include "config.h"
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _WIN32

// in gfx_common/win_error.c
const char* win_error_str(int errcode);
struct NamedPipeInfo;
typedef struct NamedPipeInfo* IPCChannel;
#define NULL_CHANNEL (IPCChannel)0
typedef void *ProcessHandle;

#else

struct PipeState;
typedef struct PipeState PipeState;
typedef PipeState *IPCChannel;
#define NULL_CHANNEL (IPCChannel)0
struct ProcessInfo {
        boolint waitable;
        FILE *file;
        int pid;
};
typedef struct ProcessInfo *ProcessHandle;

#endif

boolint is_windows_9x();
int memory_usage();
FBSTRING *memory_usage_string();

boolint setup_exception_handler();
boolint save_backtrace(boolint show_message);
void os_open_logfile(const char *path);
void os_close_logfile();

int copy_file_replacing(const char *source, const char *destination);
boolint os_rename(const char *source, const char *destination);

typedef enum {
	fileTypeNonexistent, // Doesn't exist
	fileTypeFile,        // Regular file or a symlink to one
	fileTypeDirectory,   // Directory (or mount point) or a symlink to one
	fileTypeOther,       // A device, fifo, or other special file type
	fileTypeError,       // Something unreadable (including broken symlinks)
} FileTypeEnum;

FileTypeEnum get_file_type(FBSTRING *fname);

//Advisory locking (actually mandatory on Windows)
int lock_file_for_write(FILE *fh, const char *filename, int timeout_ms);
int lock_file_for_read(FILE *fh, const char *filename, int timeout_ms);
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

//Threads

boolint on_main_thread();

typedef intptr_t TLSKey;

TLSKey tls_alloc_key();
void tls_free_key(TLSKey key);
void *tls_get(TLSKey key);
void tls_set(TLSKey key, void *value);

//Processes

ProcessHandle open_process (FBSTRING *program, FBSTRING *args, boolint waitable, boolint show_output);
ProcessHandle open_piped_process (FBSTRING *program, FBSTRING *args, IPCChannel *iopipe);
// run_process_and_get_output is Unix only
int run_process_and_get_output(FBSTRING *program, FBSTRING *args, FBSTRING *output);
ProcessHandle open_console_process (FBSTRING *program, FBSTRING *args);
boolint process_running (ProcessHandle process, int *exitcode);
void kill_process (ProcessHandle process);
void cleanup_process (ProcessHandle *processp);
int get_process_id();

void os_get_screen_size(int *wide, int *high);

#ifdef __cplusplus
}
#endif

#endif
