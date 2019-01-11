#IFNDEF __CONST_BI__
#DEFINE __CONST_BI__

'OHRRPGCE GAME - shared constants
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'

CONST CURRENT_RPG_VERSION = 21
' It is a good idea to increment this number each time a major feature
' has been added, if opening a new game in an old editor would cause data-loss
' Don't be afraid to increment this. Backcompat warnings are a good thing!
'--version history
' 0 - Super-ancient 1998 format
' 1 - Ancient 1998-1999 format
' 2 - 1999-06-18
' 3 - 1999-07-08
' 4 - 2000-09-15
' 5 - 2001-03-31
' 6 - 2006-02-13 - serendipity added MIDI music, shop stuff
' 7 - ypsiliform wip added > 36 NPC defs (and many other features)
' 8 - ypsiliform wip added extended chaining data (and many other features)
' 9 - ypsiliform wip added text box sound effects
' 10 - ypsiliform wip added attack-based enemy transmogrification
' 11 - zenzizenzic wip added variable record size and record number .N## lumps
' 12 - zenzizenzic wip increased .N## record size
' 13 - zenzizenzic wip changed password format to PW4, older versions have broken genPassVersion handling
' 14 - zenzizenzic wip made .DT0 binsize-sized
' 15 - zenzizenzic wip made .DT1 binsize-sized, and added binsize.bin, fixbits.bit safeguards
' 16 - zenzizenzic wip made .ITM binsize-sized
' 17 - alectormancy wip increase global limit from 4095 to 16383
' 18 - beelzebufo turn-based support
' 19 - beelzebufo replaced .DT0 with heroes.reld
' 20 - callipygous release. Added general.reld (including new version system) and maxScriptCmdID checking.
' 21 - fufluns replaced all .PT# and .MXS with .rgfx

CONST CURRENT_RGFX_VERSION = 1
' Version number embedded in each .rgfx file.

CONST CURRENT_RSAV_VERSION = 3
' Increment this number any time that loading and resaving a game in either
' new or old versions of Game leads to data-loss, or major new features are
' added.
' WARNING: globals-only save files do not (and must not) have "ver" nodes, so
' we will need some other mechanism to indicate changes to script data format!
' 1 - zenzizenzic wip removed nativehbits related nodes
' 2 - alectormancy wip increased global limit from 4095 to 16383
' 3 - stopped writing obsolete battle_menus node
' unversioned - optional saving of slices
' 4 - callipygous added optional saving of strings

CONST CURRENT_TESTING_IPC_VERSION = 4
' Version of the IPC protocol used when live-previewing

'For CURRENT_GFX_API_VERSION, see gfx.bi and gfx.h

CONST CURRENT_HSZ_VERSION = 3
' .hsz script binary format

CONST CURRENT_HSP_VERSION = 1
' .hs/.hsp file format version
' 0 - HS Header doesn't contain a version number
' 1 - HSpeak 3P

CONST RECOMMENDED_HSPEAK_VERSION = "3S "
'When importing scripts, an out-of-date warning is shown if
'HSpeak version is older than this. Older versions will still
'work though.


'---GENERAL GAME DATA (.GEN) constants---
CONST genMaxMap = 0             'max map ID
CONST genTitle = 1              'title screen backdrop
CONST genTitleMus = 2           'title music
CONST genVictMus  = 3           'victory music
CONST genBatMus = 4             'default battle music
CONST genPassVersion = 5        'passcode format number
CONST genPW3Rot = 6             'old (third style) passcode rotator
'7-25: first style or third style encoded passcode
CONST genMaxHeroPic = 26        'max hero graphic number in .PT0
CONST genMaxEnemy1Pic = 27      'max small enemy graphic number in .PT1
CONST genMaxEnemy2Pic = 28      'max medium enemy graphic number in .PT2
CONST genMaxEnemy3Pic = 29      'max large enemy graphic number in .PT3
CONST genMaxNPCPic = 30         'max npc graphic number in .PT4
CONST genMaxWeaponPic = 31      'max weapon graphic number in .PT5
CONST genMaxAttackPic = 32      'max attack graphic number in .PT6
CONST genMaxTile = 33           'max tileset number in .TIL
CONST genMaxAttack = 34         'max attack definition number in .DT6
CONST genMaxHero = 35           'max hero definition number in .DT0
CONST genMaxEnemy = 36          'max enemy definition number in .DT1
CONST genMaxFormation = 37      'max formation number in .FOR
CONST genMaxPal = 38            'max palette number in .PAL
CONST genMaxTextbox = 39        'max text box number in .SAY
CONST genNumPlotscripts = 40    'number of scripts of any kind (number of records in PLOTSCR.LST)
CONST genNewGameScript = 41     'id of new-game plotscript
CONST genGameoverScript = 42    'id of game-over plotscript
CONST genMaxRegularScript = 43  'id of highest numbered non-autonumbered plotscript
CONST genSuspendBits = 44       'suspend stuff bits (suspend* constants)
CONST genCameraMode = 45        'camera mode: see the (*cam constants, e.g. herocam)
CONST genCameraArg1 = 46        '
CONST genCameraArg2 = 47        '
CONST genCameraArg3 = 48        '
CONST genCameraArg4 = 49        '
CONST genScrBackdrop = 50       'currently displaying script backdrop in .MXS + 1, 0 for none
CONST genDays = 51              'days of play
CONST genHours = 52             'hours of play
CONST genMinutes = 53           'minutes of play
CONST genSeconds = 54           'seconds of play
CONST genMaxVehicle = 55        'max vehicle type number in .VEH
CONST genMaxTagname = 56        'last named tag
CONST genLoadGameScript = 57    'load-game script
CONST genTextboxBackdrop = 58   'currently displaying text box backdrop in .MXS + 1, 0 for none
CONST genEnemyDissolve = 59     'Default dissolve animation for dying enemies
CONST genJoy = 60               'whether the joystick is enabled (not respected in many places, especially waitforanykey)
CONST genPoisonChar = 61        'poison status indicator char
CONST genStunChar = 62          'Stun status indicator char
CONST genDamageCap = 63         'Damage cap
CONST genMuteChar = 64          'Mute status indicator char
CONST genStatCap = 65           'Stat caps (genStatCap + stat) (65-76)
CONST genMaxSFX = 77            'last sound effect number
CONST genMasterPal = 78         'master palette number
CONST genMaxMasterPal = 79      'max master palette number
CONST genMaxMenu = 80           'max menu def in MENUS.BIN
CONST genMaxMenuItem = 81       'max menu item def in MENUITEM.BIN
CONST genMaxItem = 82           'max item in .ITM
CONST genMaxBoxBorder = 83      'max box border number in .PT7
CONST genMaxPortrait = 84       'max portrait graphic number in .PT8
CONST genMaxInventory = 85      'max available inventory slot (0 means use inventoryMax)
CONST genErrorLevel = 86        'value to set err_suppress_lvl to, if nonzero (NO LONGER USED)
CONST genLevelCap = 87          'Default maximum level (0 to genMaxLevel) (not to be confused with genMaxLevel)
CONST genEquipMergeFormula = 88 'Formula to use to calculate effective hero elemental resists
CONST genNumElements = 89       'Number of elements used
CONST genUnlockedReserveXP = 90 '% experience gained by unlocked reserve heroes
CONST genLockedReserveXP = 91   '% experience gained by locked reserve heroes
CONST genPW4Hash = 92           'new (4th style) password hash
CONST genPW2Offset = 93         'old-old password offset
CONST genPW2Length = 94         'old-old password length
CONST genVersion = 95           'RPG file format version (see CURRENT_RPG_VERSION above for latest)
CONST genStartMoney = 96        'starting money
CONST genMaxShop = 97           'last shop in .SHO
CONST genPW1Offset = 98         'old-old-old password offset
CONST genPW1Length = 99         'old-old-old password length
CONST genNumBackdrops = 100     'number of screens in .MXS
CONST genBits = 101             'general bitsets
CONST genStartX = 102           'starting X
CONST genStartY = 103           'starting Y
CONST genStartMap = 104         'starting Map
CONST genOneTimeNPC = 105       'one-time-NPC indexer
CONST genOneTimeNPCBits = 106   'one-time-NPC bits start here, OBSOLETE!
CONST genDefaultDeathSFX = 171  'default enemy death sound effect
CONST genMaxSong = 172          'last song number
CONST genAcceptSFX = 173        'menu interface (+1)
CONST genCancelSFX = 174        ' "       "
CONST genCursorSFX = 175        ' "       "
CONST genTextboxLine = 176      'Text box 'click'  (+1)
CONST genBits2 = 177'-178       'More general bitsets
CONST genItemLearnSFX = 179     'learn spell oob item (+1)
CONST genCantLearnSFX = 180     'hero couldn't learn spell from item (+1)
CONST genBuySFX = 181           'buy item from shop (+1)
CONST genHireSFX = 182          'hire from shop (+1)
CONST genSellSFX = 183          'sell item to shop (+1)
CONST genCantBuySFX = 184       'can't afford item/hire (+1)
CONST genCantSellSFX = 185      'unsellable item (+1)
CONST genDamageDisplayTicks = 186 'number of ticks that battle damage displays
CONST genDamageDisplayRise = 187 'number of pixels that damage display rises
CONST genHeroWeakHP = 188       '%HP for heroes to use Weak state
CONST genEnemyWeakHP = 189      '%HP for enemies to use Desperation AI
CONST genAutosortScheme = 190   'Method used to autosort inventory
CONST genMaxLevel = 191         'Maximum level (not to be confused with changeable genLevelCap)
CONST genBattleMode = 192       'Battle mode 0=Active-time, 1=Turn-based
CONST genItemStackSize = 193    'Default item stack size
CONST genResolutionX = 194      'Screen resolution (unzoomed). (Upgrade code changes 0 to default 320)
CONST genResolutionY = 195      ' " (default 200)
CONST genEscMenuScript = 196     'id of plotscript called instead of the default menu
CONST genSaveSlotCount = 197    'The number of available save slots, 1 to maxSaveSlotCount. If 0, the default of 4 will be used
CONST genMillisecPerFrame = 198 'Milliseconds per frame; upgrade() ensures not 0.
CONST genStealSuccessSFX = 199  'Sound effect numbers for steal attacks in addition to normal sfx (+1)
CONST genStealFailSFX = 200     ' "
CONST genStealNoItemSFX = 201   ' "
CONST genRegenChar = 202        ' Regen status icon character
CONST genDefaultScale = 203     ' Unused.
CONST genDebugMode = 204        ' 0=Release mode, 1=Debug mode. Author choice for script error display. This is the one that should be edited by the game author
CONST genCurrentDebugMode = 205 ' 0=Release mode, 1=Debug mode. Current choice for script error display. This is the one that should be checked in-game
CONST genStartHero = 206        ' ID of initial hero
CONST genStartTextbox = 207     ' ID of initial textbox, or 0 = none
CONST genWindowSize = 208       ' Window size about X% of screen, in multiples of 10%. 10 means maximize
CONST genLivePreviewWindowSize = 209 ' Test-Game window size about X% of screen, in multiples of 10%. 10 means maximize
CONST genFullscreen = 210       ' Whether to start in fullscreen by default
CONST genMusicVolume = 211      ' Initial music volume as a percentage.
CONST genSFXVolume = 212        ' Initial global sound effects volume as a percentage.
CONST genRungameFullscreenIndependent = 213  ' If false, fullscreen settings/config for games spawned by rungame are ignored
CONST genSkipBattleRewardsTicks = 214   ' If > 0 then the battle rewards messages will automatically advance after this many ticks
CONST genDefOnkeypressScript = 215      ' Default on-keypress script, if not overridden by map
CONST genDefEachStepScript = 216        ' Default each-step script, if not overridden by map
CONST genDefAfterBattleScript = 217     ' Default after-battle script, if not overridden by map
CONST genDefInsteadOfBattleScript = 218 ' Default instead-of-battle script, if not overridden by map
CONST genDefMapAutorunScript = 219      ' Default autorun script, if not overridden by map
CONST genMaxEnemyPic = 220       ' Max enemy spriteset id in enemies.rgfx
CONST genMinimapAlgorithm = 221  ' How to generate in-game minimaps. MinimapAlgorithmEnum
CONST genBits3 = 222'-225        ' More general bitsets
CONST genDefCounterProvoke = 226 ' CounterProkeEnum value for provokeDefault attacks (Default to provokeAlways if 0)

'Everything else up to 499 unused.
'When adding more data to gen() consider whether it should be saved in .rsav.
'Also, gen() is reloaded by resetgame() when starting a new/loaded game,
'if that's not OK the data should probably be stored elsewhere.

'--- Suspend bits (gen(genSuspendBits))
CONST suspendnpcs = 0
CONST suspendplayer = 1
CONST suspendobstruction = 2
CONST suspendherowalls = 3
CONST suspendnpcwalls = 4
CONST suspendcaterpillar = 5
CONST suspendrandomenemies = 6
CONST suspendboxadvance = 7
CONST suspendoverlay = 8
CONST suspendambientmusic = 9
CONST suspenddoors = 10
CONST suspendtimers = 11

'--- Camera mode constants (gen(genCameraMode))
CONST herocam = 0
CONST npccam = 1
CONST pancam = 2
CONST focuscam = 3
CONST slicecam = 4
CONST stopcam = -1

'---Built in stats
CONST statHP = 0
CONST statMP = 1
CONST statAtk = 2
CONST statAim = 3
CONST statDef = 4
CONST statDodge = 5
CONST statMagic = 6
CONST statWill = 7
CONST statSpeed = 8
CONST statCtr = 9
CONST statFocus = 10
CONST statHitX = 11
CONST statUser = 12              'Additional user defined stats, if any, start here
                                  'none exist yet.

CONST statLast = 11 'Index of last actually defined stat.
                     'FIXME: There are a ton of places that should use this, but don't yet
'The following constants are for addressing register stats in BattleStatsSingle.sta()
CONST statPoison = 12
CONST statRegen = 13
CONST statStun = 14
CONST statMute = 15
CONST statLastRegister = 15

'---Format fix bits
CONST fixAttackitems = 0         'zero out attack data for item cost (ammunition)
CONST fixWeapPoints  = 1         'add defaults for weapon points
CONST fixStunCancelTarg = 2      'turn on cancel target bitset for old stun attacks
CONST fixDefaultDissolve = 3     'Initialized genEnemyDissolve to default in GEN
CONST fixDefaultDissolveEnemy = 4'Initialized Enemy dissolves to default in DT1
CONST fixPushNPCBugCompat = 5    'Turned on the Simulate pushable NPC obstruction bug bitset
CONST fixDefaultMaxItem = 6      'Set genMaxItem to 254 (Obsolete)
CONST fixBlankDoorLinks = 7      'Marked redundant blank doorlinks as unused
CONST fixShopSounds = 8          'Set genItemLearnSFX..genCantSellSFX to defaults
CONST fixExtendedNPCs = 9        'Deleted or initialised garbage NPC data in IDs 36 to 99
CONST fixHeroPortrait = 10       'Initialize hero portrait data
CONST fixTextBoxPortrait = 11    'Initialize text box portrait data
CONST fixNPCLocationFormat = 12  'FIXME: not implemented ... can't remember....
CONST fixInitDamageDisplay = 13  'Initialize damage display time and distance
CONST fixDefaultLevelCap = 14    'Set level cap to 99 (not to be confused with max level)
CONST fixHeroElementals = 15     'Set the hero elemental data from the old weak/strong/absorb bits
CONST fixOldElementalFailBit = 16'Turned on the 'Simulate old fail vs. element resist bit' bitset
CONST fixAttackElementFails = 17 'Initialized all 64 attack elemental fail conditions
CONST fixEnemyElementals = 18    'Set enemy elemental resists from old weak/strong/absorb/enemytype bits
CONST fixItemElementals = 19     'Set equipment elemental resists from old weak/strong/absorb bits
CONST fixNumElements = 20        'Set genNumElements to 16
CONST fixRemoveDamageMP = 21     'Removed the obsolete Damage MP attack bit
CONST fixDefaultMaxLevel = 22    'Set max level to 99 (not to be confused with level cap)
CONST fixUNUSED23 = 23           'Don't use this bit for anything. It was wasted in a plan that changed
CONST fixWipeGEN = 24            'Zero out gen(199) to gen(499)
CONST fixSetOldAttackPosBit = 25 'Turn on "Old attack positioning at bottom-left of target" backcompat bit
CONST fixWrapCroppedMapsBit = 26 'Checked whether to turn on "Wrap map layers over edge of Crop maps" backcompat bit
CONST fixInitNonElementalSpawning = 27 'Initialize non_elemental_for_spawning data in general.reld
CONST fixInitDefaultVolumes = 28 'genMusicVolume and genSFXVolume have been initialised to 50, 75.
CONST fixAttackMultipliers = 29  'Initialise attack damage and aim multipliers, randomization, and absorption.
CONST fixCheckForBrokenTBChains = 30 'Ran check_for_broken_textbox_after_chains()

CONST sizeFixBits = 31 ' *** Update this when adding fix bits (last bit + 1) ***

'---Sizes (replaceable with variables when suitable)
CONST max_onetime = 15999 'The last available onetime use flag. (The first is 2.)
CONST maxMaxItems = 32000 'max number of items
CONST maxMaxHero = 511 'This is the max value possible for gen(genMaxHero)
CONST maxMaxAttacks = 32767 ' Max possible value for gen(genMaxAttack)
CONST maxMaxTextbox = 32767 ' Max possible value for gen(genMaxTextbox)
CONST maxMaxSFX = 32767 ' Max possible value for gen(genMaxSFX)
CONST maxMaxSong = 32767 ' Max possible value for gen(genMaxSong)
CONST maxMaxShop = 32767 ' Max possible value for gen(genMaxShop)
CONST maxSaveSlotCount = 1000 ' highest allowed value for gen(genSaveSlotCount)

CONST sizeParty = 41 'The maximum size of the entire party (length of gam.hero())
CONST inventoryMax = 599 'last inventory slot num (divisible by 3 when you count the zero)
CONST maplayerMax = 15 'The limit on the highest numbered map layer
CONST mapTilesMax = 100000 'Maximum map size, in tiles (note also a limit of 32768 tiles wide or high)
CONST dissolveTypeMax = 10 'Highest numbered frame dissolve effect
CONST maxMPLevel = 7   'Max level of FF1-style level-MP, 0-based (Note: the spell-list editor shows 1-based levels)
CONST maxElements = 64 'Maximum selectable number of elements
CONST tempZone = 10000 'ID (and up) to use for temporary zones
CONST maxDoorsPerMap = 99 '0 thru 99
CONST maxMapHistoryMem = 2000000 'In number of MapEditUndoTiles (8 bytes)
CONST maxSpriteHistoryMem = 16000000  'Max size of sprite undo history, in bytes
CONST maxScriptGlobals = 50000 'Actually the index of the last global  (also "maximum global id" in plotscr.hsd)
                               'If you update this, also update the default argument to exportglobals in plotscr.hsd
CONST maxScriptStrings = 99 'ID of last plotstring  (also "maximum string id" in plotscr.hsd)
CONST maxScriptHeap = 8192 'Maximum number of local variables in use by all running scripts
CONST maxScriptRunning = 128 'Number of scripts which can run at once
CONST maxScriptNesting = 4 'Maximum subscript nesting depth
'Amount of script data to cache (these are configurable)
CONST scriptmemMax = 65536 'in 4-byte ints (256kb)
CONST scriptTableSize = 512  'hash table size, power of 2 please
CONST scriptCheckDelay = 1.1     'How long, in seconds, before the script interpreter becomes interruptable
CONST scriptCheckInterval = 0.1  'How often, in seconds, that the script interpreter should perform checks

CONST maxScriptCmdID = 682  'Max ID number of any supported script command (checked when loading game)

'--- Binary files in BINSIZE.BIN for getbinsize()
CONST binATTACK = 0
CONST binSTF = 1
CONST binSONGDATA = 2
CONST binSFXDATA = 3
CONST binMAP = 4
CONST binMENUS = 5
CONST binMENUITEM = 6
CONST binUICOLORS = 7
CONST binSAY = 8
CONST binN = 9
CONST binDT0 = 10
CONST binDT1 = 11
CONST binITM = 12

CONST binLASTENTRY = 12 ' *** Update this when adding binsize records ***

Enum SpriteType
	sprTypeInvalid = -2
	sprTypeFirst = -1
	sprTypeFrame = -1        'A sprite created either from a Frame or an engine asset
	' Many functions only accept a type between 0 and sprTypeLastPT
	sprTypeHero = 0
	sprTypeSmallEnemy = 1
	sprTypeMediumEnemy = 2
	sprTypeLargeEnemy = 3
	sprTypeWalkabout = 4
	sprTypeWeapon = 5
	sprTypeAttack = 6
	sprTypeBoxBorder = 7
	sprTypePortrait = 8
	sprTypeLastPT = 8        'Last type that was stored in an old .pt# lump
	sprTypeBackdrop = 9      'Can't change this! Saved in .slice files
        sprTypeEnemy = 10        'New .rgfx combined enemies.rgfx file
	sprTypeLastPickable = 10 'Last sprite type selectable in slice editor
	sprTypeTileset = 11      'Free to change this later (never saved)
	sprTypeTilesetStrip = 12 'Free to change this later (never saved)
	sprTypeLastLoadable = 12 'Last type that frame_load knows about
	' Add new types before Tileset
	sprTypeLast = 12
End Enum

'Names for hero battle sprite frames
CONST frameSTAND = 0
CONST frameVICTORYB = 0
CONST frameSTEP = 1
CONST frameLAND = 2
CONST frameVICTORYA = 2
CONST frameATTACKA = 2
CONST frameATTACKB = 3
CONST frameCAST = 4
CONST frameJUMP = 4
CONST frameHURT = 5
CONST frameWEAK = 6
CONST frameDEAD = 7

'--- Misc constants

'Constants returned by get_font_type()
Enum fontTypeEnum
  ftypeASCII  = 0 'Non-extended ASCII, characters 127 and above assumed to be icons
  ftypeLatin1 = 1 'Characters between 127 and 160 inclusive are assumed to be icons
End Enum

'Constants for debugc
'NOTE: enum ErrorLevel in errorlevel.h MUST be updated when this is changed!
Enum 'ErrorLevelEnum
  errInfo = 1   'Informational spam (doesn't cause g/c_debug.txt to be kept)
                '(== debuginfo)
  errShowInfo   'Show and call debuginfo
  errDebug      'Log a message and preserve g/c_debug.txt
                '(== debug)
  errShowDebug  'Show and call debug. Minor error, but want to keep user well informed
                '(== visible_debug)
  errError      'Something is wrong, but it's not necessarily due to a bug. Only log it.
                '(== debugerror)
  errBug        'Engine bug detected; log but don't interrupt the program. Usually would use errShowBug instead.
  errShowError  'Something is wrong and continuing might be dubious, but it's not necessarily due to a bug.
                'Show error and possibly prompt the user whether they want to quit (doesn't return) or continue
                '(== showerror)
  errShowBug    'As above, but indicates that it's an engine bug
                '(== showbug)
  errFatalError 'Does not return!
                '(== fatalerror)
  errFatalBug   'Does not return!
                '(== fatalbug)
  errDie        'Exit immediately without attempting to show a message (especially for gfx backend errors)
End Enum
Type ErrorLevelEnum as integer  'For compatibility with C

'Constants for scripterr and friends
Type scriptErrEnum as integer
Enum 'scriptErrEnum
  serrIgnore = 0     'to suppress an error
  serrInfo = 1       'informative messages
  serrWarn = 2       'possibly suspicious operation, eg. re-freeing a slice
  serrSuspicious = 3 'suspicious operation on weak type or suspicious argument type (unimplemented)
  serrBound = 4      'warning on auto-bound() argument  (suppressed in old games)
  serrBadOp = 5      'bad argument/operation       (not suppressed by default)
  serrMajor = 6      'errors too big to ignore, eg interpreter can't continue or can't load script
  serrError = 7      'corrupt script data/unimplemented feature
                     'TODO: rename serrError to serrCorruption
  serrBug = 8        'impossible condition; engine bug (interpreter stops)
End Enum

Type DirNum as integer
Enum 'DirNum
  dirNone  = -1
  dirUp    = 0
  dirRight = 1
  dirDown  = 2
  dirLeft  = 3

  dirNorth = 0
  dirEast  = 1
  dirSouth = 2
  dirWest  = 3
End Enum

'map passability bits
CONST passNorthWall = 1
CONST passEastWall = 2
CONST passSouthWall = 4
CONST passWestWall = 8
CONST passUpWall = 1
CONST passRightWall = 2
CONST passDownWall = 4
CONST passLeftWall = 8
CONST passAllWalls = 15
CONST passVehA = 16
CONST passVehB = 32
CONST passHarm = 64
CONST passOverhead = 128
'Special values returned from sliding/check_wallmap_collision
CONST passNortheastCorner = &b0011 SHL 24
CONST passNorthwestCorner = &b1001 SHL 24
CONST passSoutheastCorner = &b0110 SHL 24
CONST passSouthwestCorner = &b1100 SHL 24

'Special zones
CONST zoneLASTUSER = 9999      'The highest zone editable in the map editor, everything
                               'above this is reserved for special uses.
CONST zoneOneWayExit = 10000   'Walls are one-way, allowing exit from this tile
CONST zoneLASTREADABLE = 10000 'The last zone that can be read/written by script commands


'NPC activation types
CONST npcUse = 0
CONST npcTouch = 1
CONST npcStepOn = 2

' These constants are for the .t type of a MenuDefItem for user-created menus
' (MenuDefs used elsewhere including battles are NOT free to assign other meanings
' to these values, because draw_menu or other functions may treat some of these specially!
' (Currently, just mtypeSpecial items.) So you should use menu item types after
' mtypeLAST instead.
' FIXME/WARNING: we have sooooo much code that uses type 0 just by default, so
' mtypeLabel had better never be treated specially by any menu code!
Enum MenuItemType
  mtypeLabel =    0
  mtypeSpecial =  1
  mtypeMenu =     2
  mtypeTextBox =  3
  mtypeScript =   4
  mtypeLAST =     4
End Enum

' Values for .sub_t subtype of mtypeLabel MenuDefItems
CONST lbSelectable  = 0
CONST lbDisabled    = 1
CONST lbUnselectable = 2
CONST lbLAST        = 2

' These are for the .sub_t subtypes of mtypeSpecial MenuDefItems for
' user-created menus.
CONST spItems       = 0
CONST spSpells      = 1
CONST spStatus      = 2
CONST spEquip       = 3
CONST spOrder       = 4
CONST spTeam        = 5
CONST spTeamOrOrder = 6  'Depends on general bitset
CONST spMapMaybe    = 7  'Only if allowed by map
CONST spSaveMaybe   = 8  'Only if allowed by map
CONST spLoad        = 9
CONST spQuit        = 10
CONST spVolumeMenu  = 11
CONST spMap         = 12
CONST spSave        = 13
CONST spMargins     = 14
CONST spPurchases   = 15
CONST spWindowed    = 16
CONST spFullscreen  = 17
CONST spMusicVolume = 18
CONST spSoundVolume = 19
CONST spLAST        = 19

'constants for lump reloading schemes
CONST loadmodeMerge = -1
CONST loadmodeNever = 0
CONST loadmodeAlways = 1
CONST loadmodeIfUnchanged = 2

'Constants for timer advancement modes (see game.bas:should_skip_this_timer())
Enum TimerContextEnum
        TIMER_NORMAL = 0
        TIMER_BATTLE = 1
        TIMER_BLOCKINGMENUS = 2
End Enum

'Timer .flags
CONST TIMERFLAG_CRITICAL = 1
CONST TIMERFLAG_BATTLE   = 2
CONST TIMERFLAG_MENU     = 4

'Timer .trigger
CONST TIMERTRIGGER_DEFAULT = -1
CONST TIMERTRIGGER_GAMEOVER = -2

CONST MaxResolutionX = 1280 'Arbitrary limits
CONST MaxResolutionY = 960
CONST MinResolutionX = 10
CONST MinResolutionY = 10


#include "uiconst.bi"
#include "scancodes.bi"

#ENDIF
