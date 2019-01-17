'OHRRPGCE CUSTOM - Globals
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
#include "os.bi"

EXTERN activepalette as integer
EXTERN slave_channel as IPCChannel
EXTERN slave_process as ProcessHandle
EXTERN cleanup_workingdir_on_exit as bool
EXTERN cleanup_workingdir_on_error as bool
EXTERN auto_distrib as string
EXTERN option_nowait as bool
EXTERN editing_a_game as bool
EXTERN counter_provoke_captions(provokeLAST) as string * 23
EXTERN last_active_seconds as double
EXTERN export_translations_to as string
