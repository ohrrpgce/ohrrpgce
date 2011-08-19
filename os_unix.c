//OHHRPGCE COMMON - Generic Unix versions of OS-specific routines
//Please read LICENSE.txt for GNU GPL License details and disclaimer of liability

#define _POSIX_SOURCE  // for fdopen
//fb_stub.h MUST be included first, to ensure fb_off_t is 64 bit
#include "fb/fb_stub.h"
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include "common.h"
#include "os.h"

typedef int ProcessHandle;


//==========================================================================================
//                                       Filesystem
//==========================================================================================


int drivelist (void *drives_array) {
	// on Unix there is only one drive, the root /
	return 0;
}

FBSTRING *drivelabel (FBSTRING *drive) {
	fb_hStrDelTemp(drive);
	return &__fb_ctx.null_desc;
}

int isremovable (FBSTRING *drive) {
	fb_hStrDelTemp(drive);
	return 0;
}

int hasmedia (FBSTRING *drive) {
	fb_hStrDelTemp(drive);
	return 0;
}

void setwriteable (FBSTRING *fname) {
	//Not written because I don't know whether it's actually needed: does
	//filecopy on Unix also copy file permissions?
	fb_hStrDelTemp(fname);
}


//==========================================================================================
//                               Inter-process communication
//==========================================================================================


//Returns true on success
int channel_open_read(FBSTRING *name, IPCChannel *result) {
  int fd = open(name->data, O_RDONLY | O_NONBLOCK);
  if (fd == -1) {
    debugc(strerror(errno), 2);
    *result = NULL;
    return 0;
  }
  *result = fdopen(fd, "r");
  if (!*result) {
    debugc(strerror(errno), 2);
    return 0;
  }
  setvbuf(*result, NULL, _IOLBF, 4096);  // set line buffered
  return 1;
}

//Returns true on success
int channel_open_write(FBSTRING *name, IPCChannel *result) {
  *result = fopen(name->data, "w");
  if (!*result) {
    debugc(strerror(errno), 2);
    return 0;
  }
  setvbuf(*result, NULL, _IOLBF, 4096);  // set line buffered
  return 1;
}

void channel_close(IPCChannel *channel) {
  fclose(*channel);
  *channel = NULL;
}

//Returns true on success
int channel_write(IPCChannel channel, char *buf, int buflen) {
  if (fwrite(buf, buflen, 1, channel) == 0) {
    // whole write didn't occur  FIXME: this doesn't seem correct
    debuginfo("channel_write failed: %s\n", strerror(errno));
    //if (errno == EAGAIN || errno == EWOULDBLOCK)
    return 0;
  }
}

//Returns true on reading a line
int channel_input_line(IPCChannel channel, FBSTRING *output) {
  FILE *f = channel;
  int size = 0, readsize;
  do {
    if (!fb_hStrRealloc(output, size + 512, 1))  // set size, preserving existing
      return 0;
    if (fgets(output->data + size, 513, f) == NULL) {
      if (errno == EAGAIN || errno == EWOULDBLOCK) {
        // not sinister
      } else {
        debugc("pipe closed\n", 2);
      }

      fb_StrDelete(output);
      return 0;
    }
    //fb_hStrCopy(output->data + size, buf, 512);
    readsize = strlen(output->data + size);
    size += readsize;
  } while (readsize == 512);
  if (size > 0) size--;  //trim off the newline

  if (!fb_hStrRealloc(output, size, 1))
    return 0;
  return 1;
  //fb_StrAssign(output, -1, buf, strlen(buf), 0);
  //fb_hStrCopy(output->data + size, buf, 512);
}


//==========================================================================================
//                                       Processes
//==========================================================================================


//Returns 0 on failure.
//If successful, you should call cleanup_process with the handle after you don't need it any longer.
//This is currently designed for running console applications. Could be
//generalised in future as needed.
ProcessHandle open_console_process (FBSTRING *program, FBSTRING *args) {
	//Unimplemented and not yet used
	fb_hStrDelTemp(program);
	fb_hStrDelTemp(args);
	return 0;
}

//If exitcode is nonnull and the process exited, the exit code will be placed in it
int process_running (ProcessHandle process, int *exitcode) {
	//Unimplemented and not yet used
	return 0;
}

void kill_process (ProcessHandle process) {
	//Unimplemented and not yet used
}

//Cleans up resources associated with a ProcessHandle
void cleanup_process (ProcessHandle *processp) {
	//Unimplemented and not yet used
}
