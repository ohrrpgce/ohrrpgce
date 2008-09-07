#IFNDEF GAME_UDTS_BI
#DEFINE GAME_UDTS_BI

'This file contains UDTs that only get used in game mode, and not in custom,
'so as to prevent them from cluttering up the global udts.bi file

TYPE MapModeState
  id       AS INTEGER
  lastmap  AS INTEGER 'ID of the last loaded map
  same     AS INTEGER 'YES/NO flag that indicates when your are moving through a same-map door
  showname AS INTEGER
  name     AS STRING
  door(99) AS Door
  doorlinks(199) AS doorlink
END TYPE

TYPE GameState
  map AS MapModeState
  wonbattle AS INTEGER 'Indicates the status of the last battle, 1 for victory 0 for running away
  remembermusic AS INTEGER 'Remembers music from before battle starts
  random_battle_countdown AS INTEGER
END TYPE

#ENDIF