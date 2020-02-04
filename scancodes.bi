'OHRRPGCE scancode constants. An extension of FreeBASIC's set.
'
'Please read LICENSE.txt for GNU GPL License details and disclaimer of liability
'

#IFNDEF SCANCODES_BI
#DEFINE SCANCODES_BI

TYPE KBScancode as integer

CONST scNone = 0  'All of allmodex treats this as a valid scancode, but it doesn't mean anything
CONST scEsc = 01
CONST sc1 = 02
CONST scExclamation = 02
CONST sc2 = 03
CONST scAtSign = 03
CONST sc3 = 04
CONST scHash = 04
CONST sc4 = 05
CONST scDollarSign = 05
CONST sc5 = 06
CONST scPercent = 06
CONST sc6 = 07
CONST scCircumflex = 07
CONST sc7 = 08
CONST scAmpersand = 08
CONST sc8 = 09
CONST scAsterisk = 09
CONST sc9 = 10
CONST scLeftParenthesis = 10
CONST sc0 = 11
CONST scRightParenthesis = 11
CONST scMinus = 12
CONST scUnderscore = 12
CONST scEquals = 13
CONST scPlus = 13
CONST scBackspace = 14
CONST scTab = 15
CONST scQ = 16
CONST scW = 17
CONST scE = 18
CONST scR = 19
CONST scT = 20
CONST scY = 21
CONST scU = 22
CONST scI = 23
CONST scO = 24
CONST scP = 25
CONST scLeftBracket = 26
CONST scLeftBrace = 26
CONST scRightBracket = 27
CONST scRightBrace = 27
CONST scEnter = 28
CONST scCtrl = 29
CONST scA = 30
CONST scS = 31
CONST scD = 32
CONST scF = 33
CONST scG = 34
CONST scH = 35
CONST scJ = 36
CONST scK = 37
CONST scL = 38
CONST scSemicolon = 39
CONST scColon = 39
CONST scQuote = 40
CONST scDoublequote = 40
CONST scApostrophe = 40
CONST scBackquote = 41
CONST scTilde = 41
CONST scLeftShift = 42
CONST scBackslash = 43
CONST scPipe = 43
CONST scZ = 44
CONST scX = 45
CONST scC = 46
CONST scV = 47
CONST scB = 48
CONST scN = 49
CONST scM = 50
CONST scComma = 51
CONST scLeftCaret = 51
CONST scPeriod = 52
CONST scRightCaret = 52
CONST scSlash = 53
CONST scQuestionMark = 53
CONST scRightShift = 54
CONST scNumpadAsterisk = 55
CONST scUnfilteredAlt = 56
CONST scSpace = 57
CONST scCapsLock = 58
CONST scF1 = 59
CONST scF2 = 60
CONST scF3 = 61
CONST scF4 = 62
CONST scF5 = 63
CONST scF6 = 64
CONST scF7 = 65
CONST scF8 = 66
CONST scF9 = 67
CONST scF10 = 68
CONST scNumLock = 69  ' On Macs this is the Clear numpad key instead
CONST scScrollLock = 70
CONST scHome = 71
CONST scUp = 72
CONST scPageUp = 73
CONST scNumpadMinus = 74
CONST scLeft = 75
CONST scRight = 77
CONST scNumpadPlus = 78
CONST scEnd = 79
CONST scDown = 80
CONST scPageDown = 81
CONST scInsert = 82
CONST scDelete = 83
CONST scF11 = 87
CONST scF12 = 88
CONST scLeftWinLogo = 91
CONST scLeftCommand = 91
CONST scLeftMeta = 91
CONST scRightWinLogo = 92
CONST scRightCommand = 92
CONST scRightMeta = 92
CONST scContext = 93

'The following scancodes deviate from QB/FB

CONST scF13 = 94
CONST scF14 = 95
CONST scF15 = 96

CONST scShift = 97
CONST scLeftAlt = 98
CONST scRightAlt = 99   ' Also Alt Gr mapped to this
CONST scLeftCtrl = 100
CONST scRightCtrl = 101

CONST scNumpadSlash = 102
CONST scNumpadEnter = 103

CONST scNumpad7 = 104
CONST scNumpad8 = 105
CONST scNumpad9 = 106
CONST scNumpad4 = 108
CONST scNumpad5 = 109
CONST scNumpad6 = 110
CONST scNumpad1 = 112
CONST scNumpad2 = 113
CONST scNumpad3 = 114
CONST scNumpad0 = 115
CONST scNumpadPeriod = 116

CONST scPrintScreen = 117
CONST scPause = 118

'This alt scancode is filtered for WM combinations
CONST scAlt = 119

'Either scEnter or scNumpadEnter
CONST scAnyEnter = 120

'Either left or right Meta key
CONST scWinLogo = 121
CONST scCommand = 121
CONST scMeta = 121

'Used for size of arrays - does NOT include joystick/mouse scancodes
'NOTE: 127 is also hardcoded into gfx.bi API and backends.
'Can't be increased anyway, due to joystick scancodes
CONST scLAST = 127

'------------- Virtual scancodes

'For convenience joystick buttons are also accessible via scancodes
CONST scJoyFIRST = 128
CONST scJoyButton1 = 128
CONST scJoyButton16 = 143
CONST scJoyLeft = 144
CONST scJoyRight = 145
CONST scJoyUp = 146
CONST scJoyDown = 147
CONST scJoyButton17 = 148
CONST scJoyButton32 = 163
CONST scJoyLAST = 163

'These are only returned by anykeypressed/waitforkey! You can't use them elsewhere!
CONST scMouseLeft = 180
CONST scMouseRight = 181
CONST scMouseMiddle = 182
'The following are only returned by waitforkey!
CONST scResize = 183   'The window was resized

CONST scAny = -1   'Any key - can NOT be used to index into carray(), only passed to keyval().
                   'Warning: not 100% equivalent to "any key" in scripts, because it excludes
                   'scUnfilteredAlt, and joystick buttons not mapped into carray aren't checked

'--- Scancodes shared with carray()
CONST ccHIGHEST = -2
CONST ccUp = -2
CONST ccDown = -3
CONST ccLeft = -4
CONST ccRight = -5
CONST ccUse = -6
CONST ccCancel = -7
CONST ccMenu = -7
CONST ccRun = -8
CONST ccLOWEST = -8


'These are the range of scancodes which can be passed to keyval().
CONST scKEYVAL_FIRST = ccLOWEST
CONST scKEYVAL_LAST = scJoyLAST


'------------------------------------------------------------------------------
'                            Joystick scancodes
'Indices used by JoystickState

ENUM 'JoyScancode
 joyButton1  = 0
 joyButton2  = 1
 joyButton3  = 2
 joyButton4  = 3
 joyButton17 = 16
 joyButton32 = 31
 joyLeft     = 32
 joyRight    = 33
 joyUp       = 34
 joyDown     = 35
 joyLAST     = 35
END ENUM
TYPE JoyScancode as integer

ENUM MouseButton
  mouseLeft = 1
  mouseRight = 2
  mouseMiddle = 4
END ENUM

#ENDIF
