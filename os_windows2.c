//OHHRPGCE COMMON - Windows-specific routines which require C implementations
//Please read LICENSE.txt for GNU GPL License details and disclaimer of liability

#include <locale.h>
#include "os.h"
#include "common.h"

void init_runtime() {
	// Needed for mbstowcs
	if (!setlocale(LC_ALL, "")) {
		// This will actually end up in ?_debug_archive.txt ...
		debug(2, "setlocale failed");
	}
}
