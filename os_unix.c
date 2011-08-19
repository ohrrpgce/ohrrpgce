//OHHRPGCE COMMON - Generic Unix versions of OS-specific routines
//Please read LICENSE.txt for GNU GPL License details and disclaimer of liability

#include "fb/fb_stub.h"

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
