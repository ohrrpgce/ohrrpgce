#IFNDEF GAME_UDTS_BI
#DEFINE GAME_UDTS_BI

'This file contains UDTs that only get used in game mode, and not in custom,
'so as to prevent them from cluttering up the global udts.bi file

TYPE MapModeState
  id AS INTEGER
END TYPE

TYPE GameState
  map AS MapModeState
END TYPE

#ENDIF