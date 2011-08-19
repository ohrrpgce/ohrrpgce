//OHHRPGCE COMMON - Generic Unix versions of OS-specific routines
//Please read LICENSE.txt for GNU GPL License details and disclaimer of liability

#define _POSIX_SOURCE  // for fdopen
#define _BSD_SOURCE  // for usleep
//fb_stub.h MUST be included first, to ensure fb_off_t is 64 bit
#include "fb/fb_stub.h"
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <errno.h>
#include "common.h"
#include "os.h"


static long long milliseconds() {
	struct timeval tv;
	gettimeofday(&tv, NULL);
	return (long long)tv.tv_sec * 1000 + tv.tv_usec / 1000;
}


//==========================================================================================
//                                       Filesystem
//==========================================================================================

// I think all the fb_hStrDelTemp paranoia in the following is actually unnecessary...
// FB has a special calling convention for rtlib functions with different string passing? FBCALL?

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
	//(FB's) filecopy on Unix does not copy file permissions, so this isn't needed
	fb_hStrDelTemp(fname);
}


//(setq c-basic-offset 8)
//(setq indent-tabs-mode t)

#define COPYBUF_SIZE 4096
char copybuf[COPYBUF_SIZE];

//A file copy function which deals safely with the case where the file is open already. On Unix, unlink first.
//
//Based on FB's Unix FileCopy function.
//No reason for this to exist rather than just call remove and fb_FileCopy...
//Originally I was going to do file locking, but that's unneeded on Unix.
//Well, at least it has lots of added error printing
int copy_file_replacing(const char *source, const char *destination) {
	FILE *src, *dst = NULL;
	long len;
	size_t bytes_to_copy;
	
	if (remove(destination)) {
		if (errno != ENOENT) {
			debug(2, "error while trying remove(%s): %s", destination, strerror(errno));
			//Can try continuing...
		}
	}
	
	if (!(src = fopen(source, "rb"))) {
		debug(2, "copy_file_replacing: could not fopen(%s, r): %s", source, strerror(errno));
		return 0;
	}

	fseek(src, 0, SEEK_END);
	len = ftell(src);
	fseek(src, 0, SEEK_SET);
	
	if (!(dst = fopen(destination, "wb"))) {
		debug(2, "copy_file_replacing: could not fopen(%s, w): %s", destination, strerror(errno));
		goto err;
	}
	
	while (len > 0) {
		bytes_to_copy = (len >= COPYBUF_SIZE) ? COPYBUF_SIZE : len;
		if (fread(copybuf, 1, bytes_to_copy, src) != bytes_to_copy) {
			debug(2, "copy_file_replacing: fread(%s) error: %s", source, strerror(errno));
			goto err;
		}
		if (fwrite(copybuf, 1, bytes_to_copy, dst) != bytes_to_copy) {
			debug(2, "copy_file_replacing: fwrite(%s) error: %s", destination, strerror(errno));
			goto err;
		}
		len -= bytes_to_copy;
	}	
	fclose(src);
	fclose(dst);
	return 1;
 err:
	if (src) fclose(src);
	if (dst) fclose(dst);
	return 0;
}


//==========================================================================================
//                                    Advisory locking
//==========================================================================================


static int lock_file_base(FILE *fh, int timeout_ms, int flag, char *funcname) {
	int fd = fileno(fh);
	long long timeout = milliseconds() + timeout_ms;
	do {
		if (!flock(fd, flag | LOCK_NB))
			return 1;
		if (errno != EWOULDBLOCK && errno != EINTR) {
			debuginfo("%s: error: %s", funcname, strerror(errno));
			return 0;
		}
		usleep(10000);
	} while (milliseconds() < timeout);
	debuginfo("%s: timed out", funcname);
	return 0;
}

//For debugging
int test_locked(const char *filename, int writable) {
	int fd = open(filename, O_RDONLY);
	if (!flock(fd, LOCK_NB | (writable ? LOCK_EX : LOCK_SH))) {
		close(fd);
		return 0;
	}
	if (errno != EWOULDBLOCK && errno != EINTR) {
		debuginfo("test_locked: error: %s", strerror(errno));
		close(fd);
		return 0;
	}
	close(fd);
	return 1;
}

//Returns true on success
int lock_file_for_write(FILE *fh, int timeout_ms) {
	return lock_file_base(fh, timeout_ms, LOCK_EX, "lock_file_for_write");
}

//Returns true on success
int lock_file_for_read(FILE *fh, int timeout_ms) {
	return lock_file_base(fh, timeout_ms, LOCK_SH, "lock_file_for_read");
}

void unlock_file(FILE *fh) {
	flock(fileno(fh), LOCK_UN);
}


//==========================================================================================
//                               Inter-process communication
//==========================================================================================


//FBSTRING *channel_pick_name(const char *id, const char *tempdir, const char *rpg) {
//	  FBSTRING *ret;
//}

//Size of a PipeState read buffer in bytes
#define PIPEBUFSZ 2048

struct PipeState {
	char *filename;
	int fd;
	char *buf;
	int readamnt;   //Next byte to be read from buffer
	int usedamnt;   //Total amount of data in buffer, including already read
};

static PipeState *channel_new(int fd) {
	PipeState *ret = malloc(sizeof(PipeState));
	ret->fd = fd;
	ret->buf = malloc(PIPEBUFSZ);
	ret->readamnt = ret->usedamnt = 0;
	ret->filename = NULL;
	return ret;
}

static void channel_delete(PipeState *state) {
	if (!state) return;
	free(state->buf);
	free(state->filename);
	free(state);
}

//Returns true on success
int channel_open_server(PipeState **result, FBSTRING *name) {
	*result = NULL;
	remove(name->data);
	int res = mkfifo(name->data, 0777);
	if (res == -1) {
		debug(2, "mkfifo(%s) failed: %s", name->data, strerror(errno));
		return 0;
	}
	PipeState *ret = channel_new(-1);
	ret->filename = malloc(strlen(name->data) + 1);
	strcpy(ret->filename, name->data);

	// If a FIFO is opened for write in non-blocking mode and it hasn't been opened for read
	// yet, then the open fails. So we have to wait for the client to connect first:
	// see channel_wait_for_client_connection

        // write() normally throws a SIGPIPE on broken pipe; ignore those and receive EPIPE instead
        signal(SIGPIPE, SIG_IGN);

	*result = ret;
	return 1;
}

//Returns true on success
int channel_open_client(PipeState **result, FBSTRING *name) {
	int fd = open(name->data, O_RDONLY | O_NONBLOCK);
	if (fd == -1) {
		debugc(strerror(errno), 2);
		*result = NULL;
		return 0;
	}
	*result = channel_new(fd);
	return 1;
}

//Returns true on success, false on error or timeout
int channel_wait_for_client_connection(PipeState **channelp, int timeout_ms) {
	PipeState *chan = *channelp;

	if (!chan) return 0;

	long long timeout = milliseconds() + timeout_ms;

	do {
		int fd = open(chan->filename, O_WRONLY | O_NONBLOCK);
		if (fd != -1) {
			chan->fd = fd;
			return 1;
		}
		if (errno != ENXIO && errno != EINTR) {	
			debug(2, "channel_open_server(%s) error: %s", chan->filename, strerror(errno));
			channel_close(channelp);
			return 0;
		}
		usleep(10000);
	} while (milliseconds() < timeout);
	debug(2, "timeout while waiting for client connection to %s", chan->filename);
	channel_close(channelp);
	return 0;
}

void channel_close(PipeState **channelp) {
	if (!*channelp) return;
	if ((*channelp)->fd != -1) close((*channelp)->fd);
	channel_delete(*channelp);
	*channelp = NULL;
}

//Returns true on success
int channel_write(PipeState **channelp, const char *buf, int buflen) {
	if (!*channelp) return 0;

	int fd = (*channelp)->fd;
	if (fd == -1) {
		debug(3, "channel_write: no file descriptor! (forgot channel_wait_for_client_connection?)");
		return 0;
	}
	int written = 0;
	while (written < buflen) {
		int res = write(fd, buf + written, buflen - written);
		if (res == -1) {
			if (errno == EINTR)
				continue;
			if (errno == EPIPE)
				//Reading end closed
				debuginfo("channel_write: pipe closed.");
			else
				debug(2, "channel_write: error: %s", strerror(errno));
			channel_close(channelp);
			return 0;
		}
		written += res;
	}
	return 1;
}

//Returns true on success. Automatically appends a newline
int channel_write_line(PipeState **channelp, FBSTRING *buf) {
	//Temporarily replace NULL byte with a newline
	buf->data[FB_STRSIZE(buf)] = '\n';
	int ret = channel_write(channelp, buf->data, FB_STRSIZE(buf) + 1);
	buf->data[FB_STRSIZE(buf)] = '\0';
	return ret;
}

//Returns true on reading a line
int channel_input_line(PipeState **channelp, FBSTRING *output) {
	if (!*channelp) return 0;

	int wait_times = 0;
	PipeState *chan = *channelp;
	char *nl = NULL;
	int outlen = 0;
	for (;;) {
		// First try to fulfill the request from buffer
		int copylen = chan->readamnt - chan->usedamnt;
		if (copylen > 0) {
			nl = memchr(chan->buf + chan->usedamnt, '\n', copylen);
			if (nl)
				copylen = nl - (chan->buf + chan->usedamnt);
			if (!fb_hStrRealloc(output, outlen + copylen, 1))  // set length, preserving existing
				return 0;
			memcpy(output->data + outlen, chan->buf + chan->usedamnt, copylen);
			outlen += copylen;
			chan->usedamnt += copylen;
			if (nl) {
				chan->usedamnt++;
				return 1;
			}
			// Otherwise, have used up all buffered data
		}

		// Read into buffer
		int res = read(chan->fd, chan->buf, PIPEBUFSZ);
		if (res == 0) {
			// EOF: write end of pipe has closed
			debuginfo("channel_input_line: pipe closed");
			channel_close(channelp);
			goto cutshort;
		}
		if (res == -1) {
			if (errno == EINTR)
				continue;
			if (errno == EAGAIN || errno == EWOULDBLOCK) {
				// No more data available yet 
				if (outlen) {
					// Strange -- expected newline! Lets wait for a bit
					if (!wait_times)
						debug(2, "channel_read_input: unexpected blocking input");
					if (wait_times++ > 20)
						goto cutshort;
					usleep(1000);
					continue;
				}
				// not sinister
			} else {
				debug(2, "channel_input_line: pipe closed due to error %s\n", strerror(errno));
				channel_close(channelp);
			}
			goto cutshort;
		}
		chan->readamnt = res;
		chan->usedamnt = 0;
	}

 cutshort:
	if (!outlen)
		// We haven't called fb_hStrRealloc, so haven't overwritten the old contents of output
		fb_StrDelete(output);
	return (outlen > 0);
}
//fb_StrAssign(output, -1, buf, strlen(buf), 0);
//fb_hStrCopy(output->data + outlen, buf, 512);


//==========================================================================================
//                                       Processes
//==========================================================================================


//Partial implementation. Doesn't return a useful process handle
ProcessHandle open_process (FBSTRING *program, FBSTRING *args) {
	char *buf = malloc(strlen(program->data) + strlen(args->data) + 1);
	sprintf(buf, "%s %s", program->data, args->data);
	popen(buf, "r");  //No intention to read or write
	free(buf);
	fb_hStrDelTemp(program);
	fb_hStrDelTemp(args);
	return -1;  //nonzero
}

//Returns 0 on failure.
//If successful, you should call cleanup_process with the handle after you don't need it any longer.
//This is currently designed for running console applications. Could be
//generalised in future as needed.
ProcessHandle open_console_process (FBSTRING *program, FBSTRING *args) {
	return open_process(program, args);
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
	*processp = 0;
}
