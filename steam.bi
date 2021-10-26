'OHRRPGCE GAME - code for interfacing with Steamworks
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
#IFNDEF STEAM_BI
#DEFINE STEAM_BI

namespace Steam

declare function initialize() as boolean
declare sub uninitialize()
declare function available() as boolean
declare sub run_frame()
declare sub reward_achievement(id as const string)
declare sub clear_achievement(id as string)
declare sub notify_achievement_progress(id as const string, progress as integer, max_progress as integer)

end namespace

#ENDIF
