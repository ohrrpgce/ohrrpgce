//OHRRPGCE COMMON - Generic Unix versions of OS-specific routines
//Please read LICENSE.txt for GNU GPL License details and disclaimer of liability

#ifndef __APPLE__
#define _POSIX_SOURCE  // for fdopen
#define _BSD_SOURCE  // for usleep
#endif
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
#include <locale.h>
#include "common.h"
#include "os.h"


void init_runtime() {
	// Needed for mbstowcs
	if (!setlocale(LC_ALL, "")) {
		// This will actually end up in ?_debug_archive.txt ...
		debug(errError, "setlocale failed");
	}
}

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
			debug(errError, "error while trying remove(%s): %s", destination, strerror(errno));
			//Can try continuing...
		}
	}
	
	if (!(src = fopen(source, "rb"))) {
		debug(errError, "copy_file_replacing: could not fopen(%s, r): %s", source, strerror(errno));
		return 0;
	}

	fseek(src, 0, SEEK_END);
	len = ftell(src);
	fseek(src, 0, SEEK_SET);
	
	if (!(dst = fopen(destination, "wb"))) {
		debug(errError, "copy_file_replacing: could not fopen(%s, w): %s", destination, strerror(errno));
		goto err;
	}
	
	while (len > 0) {
		bytes_to_copy = (len >= COPYBUF_SIZE) ? COPYBUF_SIZE : len;
		if (fread(copybuf, 1, bytes_to_copy, src) != bytes_to_copy) {
			debug(errError, "copy_file_replacing: fread(%s) error: %s", source, strerror(errno));
			goto err;
		}
		if (fwrite(copybuf, 1, bytes_to_copy, dst) != bytes_to_copy) {
			debug(errError, "copy_file_replacing: fwrite(%s) error: %s", destination, strerror(errno));
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

// Size of a PipeState readbuf in bytes
#define PIPEBUFSZ 2048

typedef struct BufferedMsg BufferedMsg;
struct BufferedMsg {
	BufferedMsg *next;
	char *msg;
	int msglen;
};

struct PipeState {
	char *basename;
	// Writing
	int writefd;
	BufferedMsg *writebuf_head;
	BufferedMsg *writebuf_tail;
	// Reading
	int readfd;
	char *readbuf; // PIPEBUFSZ bytes
	int readamnt;  // Next byte to be read from readbuf
	int usedamnt;  // Total amount of data in readbuf, including already read
};

static void channel_writebuf_push_msg(PipeState *channel, const char *buf, int buflen);
static void channel_writebuf_pop_msg(PipeState *channel);


static PipeState *channel_new(const char *basename) {
	PipeState *ret = malloc(sizeof(PipeState));
	ret->readfd = -1;
	ret->writefd = -1;
	ret->writebuf_head = ret->writebuf_tail = NULL;
	ret->readbuf = malloc(PIPEBUFSZ);
	ret->readamnt = ret->usedamnt = 0;
	ret->basename = malloc(strlen(basename) + 1);
	strcpy(ret->basename, basename);

	return ret;
}

static void channel_delete(PipeState *channel) {
	if (!channel) return;
	free(channel->readbuf);
	free(channel->basename);
	while (channel->writebuf_head)
		channel_writebuf_pop_msg(channel);
	free(channel);
}

void channel_close(PipeState **channelp) {
	PipeState *channel = *channelp;
	if (!channel) return;
	if (channel->readfd != -1) close(channel->readfd);
	if (channel->writefd != -1) close(channel->writefd);
	channel_delete(channel);
	*channelp = NULL;
}

// Attempts to open a FIFO file for reading
// Returns true on success
static int fifo_open_read(char *name, int *out_fd) {
	*out_fd = open(name, O_RDONLY | O_NONBLOCK);
	if (*out_fd == -1) {
		debug(errError, "fifo_open_read: open(%s) error: %s", name, strerror(errno));
		return 0;
	}
	return 1;
}

// Attempts to open a FIFO file for writing
// Returns true on success
static int fifo_open_write(char *filename, int *out_fd, int timeout_ms) {
	*out_fd = -1;
	long long timeout = milliseconds() + timeout_ms;
	do {
		int fd = open(filename, O_WRONLY | O_NONBLOCK);
		if (fd != -1) {
			*out_fd = fd;
			return 1;
		}
		if (errno != ENXIO && errno != EINTR) {	
			debug(errError, "fifo_open_write: open(%s) error: %s", filename, strerror(errno));
			return 0;
		}
		usleep(10000);
	} while (milliseconds() < timeout);
	debug(errError, "timeout while waiting for writer to connect to %s", filename);
	return 0;
}

// Creates a FIFO. To finish connecting, the client must connect and 
// channel_wait_for_client_connection must be called (in either order).
// Returns true on success
int channel_open_server(PipeState **result, FBSTRING *name) {
	*result = NULL;
	char *writefile = alloca(strlen(name->data) + 8 + 1);
	char *readfile = alloca(strlen(name->data) + 8 + 1);
	sprintf(writefile, "%s.2client", name->data);
	sprintf(readfile,  "%s.2server", name->data);

	remove(writefile);
	if (mkfifo(writefile, 0777) == -1) {
		debug(errError, "mkfifo(%s) failed: %s", writefile, strerror(errno));
		return 0;
	}

	remove(readfile);
	if (mkfifo(readfile, 0777) == -1) {
		debug(errError, "mkfifo(%s) failed: %s", readfile, strerror(errno));
		remove(writefile);
		return 0;
	}

	PipeState *ret = channel_new(name->data);
	if (fifo_open_read(readfile, &ret->readfd) == -1) {
		channel_close(&ret);
		remove(readfile);
		remove(writefile);
		return 0;
	}

	// If a FIFO is opened for write in non-blocking mode and it hasn't been opened for read
	// yet, then the open fails. So we have to wait for the client to connect to the 'in'
	// FIFO first; see channel_wait_for_client_connection.

	// write() normally throws a SIGPIPE on broken pipe; ignore those and receive EPIPE instead
	signal(SIGPIPE, SIG_IGN);

	*result = ret;
	return 1;
}

// Returns true on success
int channel_open_client(PipeState **result, FBSTRING *name) {
	*result = NULL;
	char *writefile = alloca(strlen(name->data) + 8 + 1);
	char *readfile = alloca(strlen(name->data) + 8 + 1);
	sprintf(writefile, "%s.2server", name->data);  // Reversed from channel_open_server
	sprintf(readfile,  "%s.2client", name->data);

	*result = channel_new(name->data);

	if (!fifo_open_read(readfile, &(*result)->readfd)) {
		channel_close(result);
		return 0;
	}

	// At this point the server is meant to already have its read end open, so this
	// should succeed immediately.
	if (!fifo_open_write(writefile, &(*result)->writefd, 200)) {
		channel_close(result);
		return 0;
	}

	// write() normally throws a SIGPIPE on broken pipe; ignore those and receive EPIPE instead
	signal(SIGPIPE, SIG_IGN);

	return 1;
}

// This is used by the server process only. Attempts to open the write side of a pair
// of pipes. Requires that the other process opens its read side first. Waits for that to happen.
// Returns true on success, false on error or timeout
int channel_wait_for_client_connection(PipeState **channelp, int timeout_ms) {
	PipeState *chan = *channelp;

	if (!chan) return 0;

	char *namebuf = alloca(strlen(chan->basename) + 8 + 1);
	sprintf(namebuf, "%s.2client", chan->basename);

	if (!fifo_open_write(namebuf, &chan->writefd, timeout_ms)) {
		channel_close(channelp);
		return 0;
	}
	return 1;
}

// Returns: 0 error, channel closed;  1 success;  2 no room in buffer
static int channel_write_internal(PipeState **channelp, const char *buf, int buflen) {
	int fd = (*channelp)->writefd;
	if (fd == -1) {
		debug(errBug, "channel_write: no file descriptor! (forgot channel_wait_for_client_connection?)");
		channel_close(channelp);
		return 0;
	}
	int written = 0;
	while (written < buflen) {
		int res = write(fd, buf + written, buflen - written);
		if (res == -1) {
			if (errno == EINTR)  // write interrupted, retry
				continue;
			if (errno == EAGAIN)
				return 2;
			if (errno == EPIPE)
				// Reading end closed
				debuginfo("channel_write: pipe closed.");
			else
				debug(errError, "channel_write: error: %s", strerror(errno));
			channel_close(channelp);
			return 0;
		}
		written += res;
	}
	return 1;
}

static void channel_writebuf_push_msg(PipeState *channel, const char *buf, int buflen) {
	BufferedMsg *elmt = malloc(sizeof(BufferedMsg));
	elmt->msg = malloc(buflen);
	memcpy(elmt->msg, buf, buflen);
	elmt->msglen = buflen;
	elmt->next = NULL;

	if (channel->writebuf_tail) {
		channel->writebuf_tail->next = elmt;
		channel->writebuf_tail = elmt;
	} else {
		channel->writebuf_head = channel->writebuf_tail = elmt;
	}
}

static void channel_writebuf_pop_msg(PipeState *channel) {
	BufferedMsg *elmt = channel->writebuf_head;
	channel->writebuf_head = elmt->next;
	if (!channel->writebuf_head)
		channel->writebuf_tail = NULL;
	free(elmt->msg);
	free(elmt);
}

// Returns true on apparent success (which may mean the output is buffered
// instead of immediately written)
int channel_write(PipeState **channelp, const char *buf, int buflen) {
	int res;
	PipeState *channel = *channelp;
	if (!channel) return 0;

	// Flush delayed writes
	while (channel->writebuf_head) {
		res = channel_write_internal(channelp, channel->writebuf_head->msg, channel->writebuf_head->msglen);
		if (res == 0) {
			channel_close(channelp);
			return 0;
		}
		if (res == 1) {
			channel_writebuf_pop_msg(channel);
		}
		if (res == 2) {
			// Still not enough room
			channel_writebuf_push_msg(channel, buf, buflen);
			return 1;
		}
	}

	res = channel_write_internal(channelp, buf, buflen);
	if (res == 2) {
		if (!channel->writebuf_head) {
			// This is the first overflow
			debuginfo("channel_write warning: OS pipe buffer full, starting internal buffering");
		}
		channel_writebuf_push_msg(channel, buf, buflen);
		return 1;
	} else {
		return res;
	}
}

// Returns true on apparent success (which may mean the output is buffered
// instead of immediately written)
// Automatically appends a newline
int channel_write_line(PipeState **channelp, FBSTRING *buf) {
	// Temporarily replace NULL byte with a newline
	buf->data[FB_STRSIZE(buf)] = '\n';
	int ret = channel_write(channelp, buf->data, FB_STRSIZE(buf) + 1);
	buf->data[FB_STRSIZE(buf)] = '\0';
	return ret;
}

// Returns true on reading a line
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
			nl = memchr(chan->readbuf + chan->usedamnt, '\n', copylen);
			if (nl)
				copylen = nl - (chan->readbuf + chan->usedamnt);
			if (!fb_hStrRealloc(output, outlen + copylen, 1))  // set length, preserving existing
				return 0;
			memcpy(output->data + outlen, chan->readbuf + chan->usedamnt, copylen);
			outlen += copylen;
			chan->usedamnt += copylen;
			if (nl) {
				chan->usedamnt++;
				return 1;
			}
			// Otherwise, have used up all buffered data
		}

		// Read into buffer
		int res = read(chan->readfd, chan->readbuf, PIPEBUFSZ);
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
						debug(errError, "channel_read_input: unexpected blocking input");
					if (wait_times++ > 20)
						goto cutshort;
					usleep(1000);
					continue;
				}
				// not sinister
			} else {
				debug(errError, "channel_input_line: pipe closed due to error %s\n", strerror(errno));
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
//program is an unescaped path. Any paths in the arguments should be escaped
ProcessHandle open_process (FBSTRING *program, FBSTRING *args) {
	ProcessHandle ret = -1;  //default success: nonzero

	char *program_escaped = escape_filenamec(program->data);
	char *buf = malloc(strlen(program_escaped) + strlen(args->data) + 2);
	sprintf(buf, "%s %s", program_escaped, args->data);

	errno = 0;
	FILE *res = popen(buf, "r");  //No intention to read or write
	int err = errno;  //errno from popen is not reliable
	if (!res) {
		debug(errError, "popen(%s, %s) failed: %s", program->data, args->data, strerror(err));
		ret = 0;
	}

	free(program_escaped);
	free(buf);
	fb_hStrDelTemp(program);
	fb_hStrDelTemp(args);
	return ret;
}

//Run a (hidden) commandline program and open a pipe which writes to its stdin & reads from stdout
//Returns 0 on failure.
//If successful, you should call cleanup_process with the handle after you don't need it any longer.
ProcessHandle open_piped_process (FBSTRING *program, FBSTRING *args, IPCChannel *iopipe) {
	//Unimplemented
	return 0;
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
