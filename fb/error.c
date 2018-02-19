// Stripped down version of error.c, since unfortunately messages[]
// and the formatting code are not accessible.

#include "fb_stub.h"

static const char *messages[] = {
	"",                                     /* FB_RTERROR_OK */
	"illegal function call",                /* FB_RTERROR_ILLEGALFUNCTIONCALL */
	"file not found",                       /* FB_RTERROR_FILENOTFOUND */
	"file I/O error",                       /* FB_RTERROR_FILEIO */
	"out of memory",                        /* FB_RTERROR_OUTOFMEM */
	"illegal resume",                       /* FB_RTERROR_ILLEGALRESUME */
	"out of bounds array access",           /* FB_RTERROR_OUTOFBOUNDS */
	"null pointer access",                  /* FB_RTERROR_NULLPTR */
	"no privileges",                        /* FB_RTERROR_NOPRIVILEGES */
	"\"interrupted\" signal",               /* FB_RTERROR_SIGINT */
	"\"illegal instruction\" signal",       /* FB_RTERROR_SIGILL */
	"\"floating point error\" signal",      /* FB_RTERROR_SIGFPE */
	"\"segmentation violation\" signal",    /* FB_RTERROR_SIGSEGV */
	"\"termination request\" signal",       /* FB_RTERROR_SIGTERM */
	"\"abnormal termination\" signal",      /* FB_RTERROR_SIGTERM */
	"\"quit request\" signal",              /* FB_RTERROR_SIGABRT */
	"return without gosub",                 /* FB_RTERROR_RETURNWITHOUTGOSUB */
	"end of file"                           /* FB_RTERROR_ENDOFFILE */
};


// Slightly modified version of fb_Die
char *format_FB_error_message
	( 
		int err_num, 
		int line_num, 
		const char *mod_name,
		const char *fun_name
	)
{
	int pos = 0;

	pos += snprintf( &__fb_errmsg[pos], FB_ERRMSG_SIZE - pos,
	                 "Aborting due to runtime error %d", err_num );

	if( (err_num >= 0) && (err_num < FB_RTERROR_MAX) )
		pos += snprintf( &__fb_errmsg[pos], FB_ERRMSG_SIZE - pos,
						 " (%s)", messages[err_num] );

	if( line_num > 0 )
		pos += snprintf( &__fb_errmsg[pos], FB_ERRMSG_SIZE - pos,
						 " at line %d", line_num );

	if( mod_name != NULL )
	{
		if( fun_name != NULL )
			pos += snprintf( &__fb_errmsg[pos], FB_ERRMSG_SIZE - pos,
			                 " %s %s::%s()", (char *)(line_num > 0? &"of" : &"in"),
			                 (char *)mod_name, (char *)fun_name );
		else
			pos += snprintf( &__fb_errmsg[pos], FB_ERRMSG_SIZE - pos,
			                 " %s %s()", (char *)(line_num > 0? &"of" : &"in"),
			                 (char *)mod_name );
	}
	//else
	//	pos += snprintf( &__fb_errmsg[pos], FB_ERRMSG_SIZE - pos, "\n\n" );

	__fb_errmsg[FB_ERRMSG_SIZE-1] = '\0';

	/* Let fb_hRtExit() show the message */
	//__fb_ctx.errmsg = __fb_errmsg;

	//fb_End( err_num );
	return __fb_errmsg;
}
