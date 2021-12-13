'OHRRPGCE GAME - in-game support for achievements
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
'
#ifndef ACHIEVEMENTS_RUNTIME_BI
#define ACHIEVEMENTS_RUNTIME_BI

#include "reload.bi"

namespace Achievements

type AchievementProgress
  id as uinteger = 0
  value as uint64 = 0
  seen_tags as integer vector
  rewarded as boolean
  rewarded_date as double

  declare constructor
  declare destructor
end type

declare sub evaluate_tags()
declare sub runtime_reset() ' clears data, for a new game
declare sub runtime_load(node as Reload.NodePtr)
declare sub runtime_save(node as Reload.NodePtr)

end namespace
#endif
