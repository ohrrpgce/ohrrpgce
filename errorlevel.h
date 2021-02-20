/* OHRRPGCE - enum ErrorLevel
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
 * Please read LICENSE.txt for GPL License details and disclaimer of liability
 */

#ifndef ERRORLEVEL_H
#define ERRORLEVEL_H

// Constants for debugc mirrored from const.bi
// NOTE: const.bi MUST be updated when this is changed!
enum ErrorLevel {
	errInfo = 1,   //Informational spam (doesn't cause g/c_debug.txt to be kept)
	errShowInfo,   //Show and call debuginfo
	errDebug,      //Log a message and preserve g/c_debug.txt
	errShowDebug,  //Show and call debug. Minor error, but want to keep user well informed
	errError,      //Something is wrong, but it's not necessarily due to a bug. Only log it.
	errBug,        //Engine bug detected; log but don't interrupt the program. Usually would use errShowBug instead.
	errShowError,  //Something is wrong and continuing might be dubious, but it's not necessarily due to a bug.
	               //Show error and possibly prompt the user whether they want to quit (doesn't return) or continue
	errShowBug,    //As above, but indicates that it's an engine bug
	errFatalError, //Does not return!
	errFatalBug,   //Does not return!
	errDie,        //Exit immediately without attempting to show a message (especially for gfx backend errors)
};

#endif
