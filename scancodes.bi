'OHRRPGCE - scancode constants. An extension of FreeBASIC's set.
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#IFNDEF SCANCODES_BI
#DEFINE SCANCODES_BI

'KBScancode is misnamed, as it includes cc* control codes and scJoy* codes.
'You can use the ccCode type specifically for a cc* code.
TYPE KBScancode as integer
TYPE ccCode as integer

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
'fbgfx in FB 1.10+ reports numpad 5 as 76 "Clear" key on Windows?
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

CONST scLASTASSIGNED = 121

'Update scancodenames.bi if you add more

'Used for size of arrays - does NOT include joystick/mouse scancodes
'NOTE: 127 is also hardcoded into gfx.bi API and backends.
'Can't be increased anyway, due to joystick scancodes
CONST scLAST = 127

'------------- Virtual scancodes

'For convenience joystick buttons are also accessible via scancodes
CONST scJoyOFFSET = 127  'Difference between a scJoy* scancode and a joy* button. Button 0/scancode 127 doesn't exist
CONST scJoyButton1 = 128  'First joystick button scancode
CONST scJoyLeft = 144
CONST scJoyRight = 145
CONST scJoyUp = 146
CONST scJoyDown = 147
'See JoyButton for other button meanings
CONST scJoyButton32 = 159
'Scancodes 160 to 163 were previously buttons, but no longer are. They should be reserved and ignored.
CONST scJoyLAST = 163

'These are only returned by anykeypressed/waitforkey! You can't use them elsewhere!
CONST scMouseLeft = 180
CONST scMouseRight = 181
CONST scMouseMiddle = 182
'...Up to 16 mouse buttons

'The following are only returned by waitforkey!
CONST scResize = 200   'The window was resized

'--- Control code scancodes (type: ccCode)

CONST ccLAST = -1
CONST ccAny = -1   'Any key
                   'Warning: not 100% equivalent to "any key" in scripts, because it excludes
                   'scUnfilteredAlt, and joystick buttons not mapped to controls aren't checked.
                   'Also, in scripts ccAny/ccUse/ccMenu/ccCancel will include mouse clicks if
                   '"'anykey', etc, includes mouse" is turned on, but keyval(ccAny), etc don't.
CONST ccUp = -2
CONST ccDown = -3
CONST ccLeft = -4
CONST ccRight = -5
CONST ccUse = -6
CONST ccMenu = -7
CONST ccFlee = -8
CONST ccRun = -8
CONST ccCancel = -9
CONST ccFIRST = -9
'Update scancodenames.bi if you add more

'These are the range of scancodes which can be passed to keyval().
CONST scKEYVAL_FIRST = ccFIRST
CONST scKEYVAL_LAST = scJoyLAST

'The following is NOT a real scancode and in general can't be used
CONST SftCtl = 1000  'Shift or Ctrl

'------------------------------------------------------------------------------
'                            Joystick scancodes
'Indices used by JoystickState

ENUM 'JoyButton
 'Button 0 not used, although it's a valid index, to be consistent with existing button numbering
 joyNone       = 0  'Not used
 'The following button meanings/numbering only applies if the backend can actually provide it.
 'Otherwise raw buttons are returned, which might be in any order.
 joyA          = 1  'Aka X/Cross (PS)
 joyB          = 2  'Aka O/Circle (PS)
 joyX          = 3  'Aka Square (PS)
 joyY          = 4  'Aka Triangle (PS)
 joyLeftStick  = 5  'Pressed as a button
 joyRightStick = 6
 joyBack       = 7  'Aka Select (eg PS3), Share (eg PS4), View (Xbox)
 joyGuide      = 8  'Aka PS (eg 3), XBox
 joyStart      = 9  'Aka Options (PS4), Menu (XBox)
 joyL1         = 10  'Left shoulder
 joyR1         = 11  'Right shoulder
 joyL2         = 12  'Left trigger. Also axisL2
 joyR2         = 13  'Right trigger. Also axisR2
 '14-16 are unused; Dpad buttons start at 17 because used to support only 16 buttons
 joyLeft       = 17  'Dpad
 joyRight      = 18
 joyUp         = 19
 joyDown       = 20
 joyRStickLeft = 21  'Right thumbstick (axisRightX/Y)
 joyRStickRight= 22
 joyRStickUp   = 23
 joyRStickDown = 24
 joyLASTGAMEPAD= 24  'Last button with a gamepad mapping
 'joyEXTRA      = 25  'Any other surplus buttons start here (not implemented)

 joyButton1    = 1
 'Note: buttons 33-36 never used, can't be reported by the backends
 joyLAST       = 36  'Corresponds to scJoyLAST
END ENUM
TYPE JoyButton as integer

'Translate from joystick button name to joySc* scancode, e.g. scJoy(Down)
#define scJoy(button) (scJoyOFFSET + joy##button)

ENUM 'JoyAxis
 'As for JoyButton, the backend might not be able to map the axes like this.
 axisX      = 0  'Left thumbstick, also mapped from left dpad by default
 axisY      = 1
 axisRightX = 2  'Right thumbstick
 axisRightY = 3
 axisL2     = 4  'Left trigger
 axisR2     = 5  'Right trigger
 axisLASTGAMEPAD = 5  'Last axis with a gamepad mapping
 axisLAST   = 7  'Last axis returned by io_get_joystick_state (can't be changed!)
END ENUM
TYPE JoyAxis as integer

'------------------------------------------------------------------------------

ENUM MouseButton
  mouseNone = 0
  mouseLeft = 1
  mouseRight = 2
  mouseMiddle = 4
END ENUM

#ENDIF
