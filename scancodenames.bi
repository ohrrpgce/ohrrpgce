'OHRRPGCE scancode names. An extension of FreeBASIC's set.
'
'Please read LICENSE.txt for GNU GPL license details and disclaimer of liability
'

static scancodenames(...) as string * 13  = { _
  "",                         /' 0 '/ _
  "Esc",                      /' 01 '/ _
  "1",                        /' 02 '/ _
  /' "Exclamation", '/        /' 02 '/ _
  "2",                        /' 03 '/ _
  /' "AtSign", '/             /' 03 '/ _
  "3",                        /' 04 '/ _
  /' "Hash", '/               /' 04 '/ _
  "4",                        /' 05 '/ _
  /' "DollarSign", '/         /' 05 '/ _
  "5",                        /' 06 '/ _
  /' "Percent", '/            /' 06 '/ _
  "6",                        /' 07 '/ _
  /' "Circumflex", '/         /' 07 '/ _
  "7",                        /' 08 '/ _
  /' "Ampersand", '/          /' 08 '/ _
  "8",                        /' 09 '/ _
  /' "Asterisk", '/           /' 09 '/ _
  "9",                        /' 10 '/ _
  /' "LeftParenthesis", '/    /' 10 '/ _
  "0",                        /' 11 '/ _
  /' "RightParenthesis", '/   /' 11 '/ _
  "Minus",                    /' 12 '/ _
  /' "Underscore", '/         /' 12 '/ _
  "Equals",                   /' 13 '/ _
  /' "Plus", '/               /' 13 '/ _
  "Backspace",                /' 14 '/ _
  "Tab",                      /' 15 '/ _
  "Q",                        /' 16 '/ _
  "W",                        /' 17 '/ _
  "E",                        /' 18 '/ _
  "R",                        /' 19 '/ _
  "T",                        /' 20 '/ _
  "Y",                        /' 21 '/ _
  "U",                        /' 22 '/ _
  "I",                        /' 23 '/ _
  "O",                        /' 24 '/ _
  "P",                        /' 25 '/ _
  "LeftBracket",              /' 26 '/ _
  /' "LeftBrace", '/          /' 26 '/ _
  "RightBracket",             /' 27 '/ _
  /' "RightBrace", '/         /' 27 '/ _
  "Enter",                    /' 28 '/ _
  "Ctrl",                     /' 29 '/ _
  "A",                        /' 30 '/ _
  "S",                        /' 31 '/ _
  "D",                        /' 32 '/ _
  "F",                        /' 33 '/ _
  "G",                        /' 34 '/ _
  "H",                        /' 35 '/ _
  "J",                        /' 36 '/ _
  "K",                        /' 37 '/ _
  "L",                        /' 38 '/ _
  "Semicolon",                /' 39 '/ _
  /' "Colon", '/              /' 39 '/ _
  "Quote",                    /' 40 '/ _
  /' "Doublequote", '/        /' 40 '/ _
  /' "Apostrophe", '/         /' 40 '/ _
  "Backquote",                /' 41 '/ _
  /' "Tilde", '/              /' 41 '/ _
  "LeftShift",                /' 42 '/ _
  "Backslash",                /' 43 '/ _
  /' "Pipe", '/               /' 43 '/ _
  "Z",                        /' 44 '/ _
  "X",                        /' 45 '/ _
  "C",                        /' 46 '/ _
  "V",                        /' 47 '/ _
  "B",                        /' 48 '/ _
  "N",                        /' 49 '/ _
  "M",                        /' 50 '/ _
  "Comma",                    /' 51 '/ _
  /' "LeftCaret", '/          /' 51 '/ _
  "Period",                   /' 52 '/ _
  /' "RightCaret", '/         /' 52 '/ _
  "Slash",                    /' 53 '/ _
  /' "QuestionMark", '/       /' 53 '/ _
  "RightShift",               /' 54 '/ _
  "NumpadAsterisk",           /' 55 '/ _
  "UnfilteredAlt",            /' 56 '/ _
  "Space",                    /' 57 '/ _
  "CapsLock",                 /' 58 '/ _
  "F1",                       /' 59 '/ _
  "F2",                       /' 60 '/ _
  "F3",                       /' 61 '/ _
  "F4",                       /' 62 '/ _
  "F5",                       /' 63 '/ _
  "F6",                       /' 64 '/ _
  "F7",                       /' 65 '/ _
  "F8",                       /' 66 '/ _
  "F9",                       /' 67 '/ _
  "F10",                      /' 68 '/ _
  "NumLock",                  /' 69 '/ _
  "ScrollLock",               /' 70 '/ _
  "Home",                     /' 71 '/ _
  "Up",                       /' 72 '/ _
  "PageUp",                   /' 73 '/ _
  "NumpadMinus",              /' 74 '/ _
  "Left",                     /' 75 '/ _
  "",                         /' 76 '/ _
  "Right",                    /' 77 '/ _
  "NumpadPlus",               /' 78 '/ _
  "End",                      /' 79 '/ _
  "Down",                     /' 80 '/ _
  "PageDown",                 /' 81 '/ _
  "Insert",                   /' 82 '/ _
  "Delete",                   /' 83 '/ _
  "",                         /' 84 '/ _
  "",                         /' 85 '/ _
  "",                         /' 86 '/ _
  "F11",                      /' 87 '/ _
  "F12",                      /' 88 '/ _
  "",                         /' 89 '/ _
  "",                         /' 90 '/ _
  /' "LeftWinLogo", '/        /' 91 '/ _
  /' "LeftCommand", '/        /' 91 '/ _
  "LeftMeta",                 /' 91 '/ _
  /' "RightWinLogo", '/       /' 92 '/ _
  /' "RightCommand", '/       /' 92 '/ _
  "RightMeta",                /' 92 '/ _
  "Context",                  /' 93 '/ _
  "F13",                      /' 94 '/ _
  "F14",                      /' 95 '/ _
  "F15",                      /' 96 '/ _
  "Shift",                    /' 97 '/ _
  "LeftAlt",                  /' 98 '/ _
  "RightAlt",                 /' 99 '/ _
  "LeftCtrl",                 /' 100 '/ _
  "RightCtrl",                /' 101 '/ _
  "NumpadSlash",              /' 102 '/ _
  "NumpadEnter",              /' 103 '/ _
  "Numpad7",                  /' 104 '/ _
  "Numpad8",                  /' 105 '/ _
  "Numpad9",                  /' 106 '/ _
  "",                         /' 107 '/ _
  "Numpad4",                  /' 108 '/ _
  "Numpad5",                  /' 109 '/ _
  "Numpad6",                  /' 110 '/ _
  "",                         /' 111 '/ _
  "Numpad1",                  /' 112 '/ _
  "Numpad2",                  /' 113 '/ _
  "Numpad3",                  /' 114 '/ _
  "Numpad0",                  /' 115 '/ _
  "NumpadPeriod",             /' 116 '/ _
  "PrintScreen",              /' 117 '/ _
  "Pause",                    /' 118 '/ _
  "Alt"                       /' 119 '/ _
}
