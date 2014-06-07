#ifndef ERRORLEVEL_H
#define ERRORLEVEL_H

// Constants for debugc mirrored from const.bi
// NOTE: const.bi MUST be updated when this is changed!
enum ErrorLevel {
	errInfo = 1,   //Informational spam (doesn't cause g/c_debug.txt to be kept)
	errDebug,      //Log a message and preserve g/c_debug.txt
	errError,      //Something is wrong, but it's not necessarily due to a bug
	errPromptError,//Something is wrong, but it's not necessarily due to a bug. Show an error message and log it.
	errBug,        //Engine bug detected; log but don't interrupt the program. Usually would use errPromptBug instead.
	errPrompt,     //Prompt the user whether they want to continue, otherwise quits without returning
	errPromptBug,  //As above, but indicates that it's an engine bug
	errFatal,      //Does not return!
	errFatalBug,   //Does not return!
	errDie,        //Exit immediately without attempting to show a message (especially for gfx backend errors)
};

#endif
