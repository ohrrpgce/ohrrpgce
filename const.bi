#IFNDEF __CONST_BI__
#DEFINE __CONST_BI__

'OHRRPGCE GAME - shared constants
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'---DOS directory attributes
CONST attribReadOnly = 1
CONST attribHidden = 2
CONST attribSystem = 4
CONST attribDirectory = 16
CONST attribArchive = 32
CONST attribReserved = 192 '64 OR 128
CONST attribAlmostAll = 237 ' All except directory and hidden

'---GENERAL GAME DATA (.GEN) constants---
CONST genMaxMap = 0             'number of maps
CONST genTitle = 1              'titile screen backdrop
CONST genTitleMus = 2           'title music
CONST genVictMus  = 3           'victory music
CONST genBatMus = 4             'default battle music
CONST genPassVersion = 5        'passcode format number
CONST genPassRot = 6            'new(third style) passcode rotator
CONST genMaxHeroPic = 26        'max hero graphics in .PT0
CONST genMaxEnemy1Pic = 27      'max small enemy graphics in .PT1
CONST genMaxEnemy2Pic = 28      'max medium enemy graphics in .PT2
CONST genMaxEnemy3Pic = 29      'max large enemy graphics in .PT3
CONST genMaxNPCPic = 30         'max npc graphics in .PT4
CONST genMaxWeaponPic = 31      'max weapon graphics in .PT5
CONST genMaxAttackPic = 32      'max attack graphics in .PT6
CONST genMaxTile = 33           'max tilesets in .TIL
CONST genMaxAttack = 34         'max attack definitions in .DT6
CONST genMaxHero = 35           'max hero definitions in .DT0
CONST genMaxEnemy = 36          'max enemy definitions in .DT1
CONST genMaxFormation = 37      'max formations in .FOR
CONST genMaxPal = 38            'max palettes in .PAL
CONST genMaxTextbox = 39        'max text boxes in .SAY
CONST genMaxPlotscript = 40     'max available plotscripts(number of records in PLOTSCR.LST)
CONST genNewGameScript = 41     'id of new-game plotscript
CONST genGameoverScript = 42    'id of game-over plotscript
CONST genMaxRegularScript = 43  'id highest numbered non-autonumbered plotscript
CONST genSuspendBits = 44       'suspend stuff bits
CONST genCamera = 45            'camera mode
CONST genCamArg1 = 46           'cameraarg1
CONST genCamArg2 = 47           'cameraarg2
CONST genCamArg3 = 48           'cameraarg3
CONST genCamArg4 = 49           'cameraarg4
CONST genScrBackdrop = 50       'currently displaying script backdrop in .MSX
CONST genDays = 51              'days of play
CONST genHours = 52             'hours of play
CONST genMinutes = 53           'minutes of play
CONST genSeconds = 54           'seconds of play
CONST genMaxVehicle = 55        'max vehicle types in .VEH
CONST genMaxTagname = 56        'last named tag
CONST genLoadGameScript = 57    'load-game script
CONST genTextboxBackdrop = 58   'currently displaying text box backdrop in .MXS
CONST genJoy = 60               'enable/disable joystick
CONST genPoison = 61            'poison status indicator char
CONST genStun = 62              'Stun status indicator char
CONST genDamageCap = 63	        'Damage cap
CONST genMute = 64              'Mute status indicator char
CONST genStatCap = 65           'Stat caps (genStatCap + stat)
CONST genMaxSFX = 77            'last song number
CONST genMasterPal = 78         'master palette number
CONST genMaxMasterPal = 79      'max master palette number
CONST genPW2Offset = 93         'old password offset
CONST genPW2Length = 94         'old password length
CONST genVersion = 95           'RPG file format version(6 is the latest)
CONST genStartMoney = 96        'starting money
CONST genMaxShop = 97           'last shop in .SHO
CONST genPW1Offset = 98         'old-old password offset
CONST genPW1Length = 99         'old-old password length
CONST genMaxBackdrop = 100      'number of screens in .MXS
CONST genBits = 101             'general bitsets
CONST genStartX = 102           'starting X
CONST genStartY = 103           'starting Y
CONST genStartMap = 104         'starting Map
CONST genOneTimeNPC = 105       'one-time-NPC indexer
CONST genMaxMenu = 171          'max menus in menus.dat
CONST genMaxSong = 172          'last song number
CONST genAcceptSFX = 173        'menu interface
CONST genCancelSFX = 174        ' "       "
CONST genCursorSFX = 175        ' "       "
CONST genTextboxLetter = 176    'Text box 'click'
CONST genScatterTableHead = 199 'old password scattertable head

'---Format fix bits
CONST fixAttackitems = 0        'zero out attack data for item cost (ammunition)
CONST fixWeapPoints  = 1				'add defaults for weapon points

'---Sizes (replaceable with variables when suitable)
CONST npcdMax = 35
CONST inventoryMax = 197	'last inventory slot num (3 doesn't divide into 200)
CONST scriptmemMax = 65536    'in 4-byte ints (256kb)

'--- Binary files in BINSIZE.BIN for getbinsize()
CONST binATTACK = 0
CONST binSTF = 1
CONST binSONGDATA = 2
CONST binSFXDATA = 3

'--- Misc constants
Enum constDirection
	dirUp
	dirRight
	dirDown
	dirLeft
End Enum

#IF __FB_VERSION__ < "0.17"
type intptr as integer
#else
type intptr as any ptr
#endif

#include "uiconst.bi"

#ENDIF
