'OHRRPGCE GAME - achievement definitions
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
'
#ifndef ACHIEVEMENTS_BI
#define ACHIEVEMENTS_BI

#include "common_base.bi"

namespace Achievements

  enum AchievementType
    flag
    count
  end enum

  type AchievementDefinition
    id as uinteger = 0
    name as string = ""
    achievement_type as AchievementType = AchievementType.flag
    max_value as uint64 = 0
    progress_interval as uinteger = 0
    latching as boolean = false
    tags as integer vector ' yay, free basic
    steam_id as string = ""

    declare constructor
    declare destructor
  end type

  declare sub definitions_load(file_path as string)
  declare sub definitions_free()
  declare sub definitions_reset()
  declare function definitions_count() as integer
  declare function get_definition_by_index(index as integer) byref as AchievementDefinition
  declare function get_definition_by_id(id as integer) byref as AchievementDefinition
  declare function is_permanent() as boolean

  extern enable_debug as boolean

  #ifdef DEBUG_ACHIEVEMENTS
  #define ach_verbose(msg) debug "achievements: " & msg
  #else
  #define ach_verbose(x)
  #endif
  declare sub ach_debug(msg as string)

end namespace

#endif
