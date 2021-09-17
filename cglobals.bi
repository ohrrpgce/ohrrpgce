'OHRRPGCE CUSTOM - Globals
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
#include "os.bi"

EXTERN activepalette as integer
EXTERN channel_to_Game as IPCChannel
EXTERN Game_process as ProcessHandle
EXTERN cleanup_workingdir_on_exit as bool
EXTERN cleanup_workingdir_on_error as bool
EXTERN auto_distrib as string
EXTERN option_nowait as bool
EXTERN editing_a_game as bool
EXTERN inside_importscripts as bool
EXTERN counter_provoke_captions(provokeLAST) as string * 23
EXTERN last_active_seconds as double
EXTERN export_translations_to as string
