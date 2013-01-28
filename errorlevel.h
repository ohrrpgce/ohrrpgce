#ifndef ERRORLEVEL_H
#define ERRORLEVEL_H

//Constants for debugc
enum ErrorLevel {
	errInfo = 1,   //Informational spam (doesn't cause g/c_debug.txt to be kept)
	errDebug,      //Log a message and preserve g/c_debug.txt
	errError,      //Something is wrong, but it's not necessarily due to a bug
	errBug,        //Engine bug detected; log but don't interrupt the program. Usually would use errPromptBug instead.
	errPrompt,     //Prompt the user whether they want to continue, otherwise quits without returning
	errPromptBug, 
	errFatal,      //Does not return!
	errFatalBug,   //Does not return!
	errDie,        //Exit immediately without attempting to show a message (especially for gfx backend errors)
};

#endif
