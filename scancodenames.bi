'OHRRPGCE - scancode names. An extension of FreeBASIC's set.
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#include "scancodes.bi"

static scancodenames(ccCancel to ...) as zstring * 19  = { _
  "Cancel",                    /' -9 '/ _
  "Flee",                      /' -8 '/ _
  "Menu",                      /' -7 '/ _
  "Use",                       /' -6 '/ _
  "Right",                     /' -5 '/ _
  "Left",                      /' -4 '/ _
  "Down",                      /' -3 '/ _
  "Up",                        /' -2 '/ _
  "Any Key",                   /' -1 '/ _
  "None",                      /' 0 '/ _
  "Esc",                       /' 01 '/ _
  "1",                         /' 02 '/ _
  /' "Exclamation", '/         /' 02 '/ _
  "2",                         /' 03 '/ _
  /' "At Sign", '/             /' 03 '/ _
  "3",                         /' 04 '/ _
  /' "Hash", '/                /' 04 '/ _
  "4",                         /' 05 '/ _
  /' "Dollar Sign", '/         /' 05 '/ _
  "5",                         /' 06 '/ _
  /' "Percent", '/             /' 06 '/ _
  "6",                         /' 07 '/ _
  /' "Circumflex", '/          /' 07 '/ _
  "7",                         /' 08 '/ _
  /' "Ampersand", '/           /' 08 '/ _
  "8",                         /' 09 '/ _
  /' "Asterisk", '/            /' 09 '/ _
  "9",                         /' 10 '/ _
  /' "( Left Parenthesis", '/  /' 10 '/ _
  "0",                         /' 11 '/ _
  /' ") Right Parenthesis", '/ /' 11 '/ _
  "-",                         /' 12 '/ _
  /' "_ Underscore", '/        /' 12 '/ _
  "=",                         /' 13 '/ _
  /' "+ Plus", '/              /' 13 '/ _
  "Backspace",                 /' 14 '/ _
  "Tab",                       /' 15 '/ _
  "Q",                         /' 16 '/ _
  "W",                         /' 17 '/ _
  "E",                         /' 18 '/ _
  "R",                         /' 19 '/ _
  "T",                         /' 20 '/ _
  "Y",                         /' 21 '/ _
  "U",                         /' 22 '/ _
  "I",                         /' 23 '/ _
  "O",                         /' 24 '/ _
  "P",                         /' 25 '/ _
  "[ Left Bracket",            /' 26 '/ _
  /' "{ Left Brace", '/        /' 26 '/ _
  "] Right Bracket",           /' 27 '/ _
  /' "} Right Brace", '/       /' 27 '/ _
  "Enter",                     /' 28 '/ _
  "Ctrl",                      /' 29 '/ _
  "A",                         /' 30 '/ _
  "S",                         /' 31 '/ _
  "D",                         /' 32 '/ _
  "F",                         /' 33 '/ _
  "G",                         /' 34 '/ _
  "H",                         /' 35 '/ _
  "J",                         /' 36 '/ _
  "K",                         /' 37 '/ _
  "L",                         /' 38 '/ _
  "; Semicolon",               /' 39 '/ _
  /' ": Colon", '/             /' 39 '/ _
  "' Quote",                   /' 40 '/ _
  /' """ Doublequote", '/      /' 40 '/ _
  /' "' Apostrophe", '/        /' 40 '/ _
  "` Backquote",               /' 41 '/ _
  /' "~ Tilde", '/             /' 41 '/ _
  "Left Shift",                /' 42 '/ _
  "\ Backslash",               /' 43 '/ _
  /' "| Pipe", '/              /' 43 '/ _
  "Z",                         /' 44 '/ _
  "X",                         /' 45 '/ _
  "C",                         /' 46 '/ _
  "V",                         /' 47 '/ _
  "B",                         /' 48 '/ _
  "N",                         /' 49 '/ _
  "M",                         /' 50 '/ _
  ", Comma",                   /' 51 '/ _
  /' "< Left Caret", '/        /' 51 '/ _
  ". Period",                  /' 52 '/ _
  /' "> Right Caret", '/       /' 52 '/ _
  "/ Slash",                   /' 53 '/ _
  /' "? Question Mark", '/     /' 53 '/ _
  "Right Shift",               /' 54 '/ _
  "Numpad Asterisk",           /' 55 '/ _
  "Unfiltered Alt",            /' 56 '/ _
  "Space",                     /' 57 '/ _
  "Caps Lock",                 /' 58 '/ _
  "F1",                        /' 59 '/ _
  "F2",                        /' 60 '/ _
  "F3",                        /' 61 '/ _
  "F4",                        /' 62 '/ _
  "F5",                        /' 63 '/ _
  "F6",                        /' 64 '/ _
  "F7",                        /' 65 '/ _
  "F8",                        /' 66 '/ _
  "F9",                        /' 67 '/ _
  "F10",                       /' 68 '/ _
  "Num Lock",                  /' 69 '/ _
  "Scroll Lock",               /' 70 '/ _
  "Home",                      /' 71 '/ _
  "Up",                        /' 72 '/ _
  "Page Up",                   /' 73 '/ _
  "Numpad Minus",              /' 74 '/ _
  "Left",                      /' 75 '/ _
  "",                          /' 76 '/ _
  "Right",                     /' 77 '/ _
  "Numpad Plus",               /' 78 '/ _
  "End",                       /' 79 '/ _
  "Down",                      /' 80 '/ _
  "Page Down",                 /' 81 '/ _
  "Insert",                    /' 82 '/ _
  "Delete",                    /' 83 '/ _
  "",                          /' 84 '/ _
  "",                          /' 85 '/ _
  "",                          /' 86 '/ _
  "F11",                       /' 87 '/ _
  "F12",                       /' 88 '/ _
  "",                          /' 89 '/ _
  "",                          /' 90 '/ _
  /' "Left Win Logo", '/       /' 91 '/ _
  /' "Left Command", '/        /' 91 '/ _
  "Left Command/Win",          /' 91 '/ _
  /' "Right Win Logo", '/      /' 92 '/ _
  /' "Right Command", '/       /' 92 '/ _
  "Right Command/Win",         /' 92 '/ _
  "Context",                   /' 93 '/ _
  "F13",                       /' 94 '/ _
  "F14",                       /' 95 '/ _
  "F15",                       /' 96 '/ _
  "Shift",                     /' 97 '/ _
  "Left Alt",                  /' 98 '/ _
  "Right Alt",                 /' 99 '/ _
  "Left Ctrl",                 /' 100 '/ _
  "Right Ctrl",                /' 101 '/ _
  "Numpad Slash",              /' 102 '/ _
  "Numpad Enter",              /' 103 '/ _
  "Numpad 7",                  /' 104 '/ _
  "Numpad 8",                  /' 105 '/ _
  "Numpad 9",                  /' 106 '/ _
  "",                          /' 107 '/ _
  "Numpad 4",                  /' 108 '/ _
  "Numpad 5",                  /' 109 '/ _
  "Numpad 6",                  /' 110 '/ _
  "",                          /' 111 '/ _
  "Numpad 1",                  /' 112 '/ _
  "Numpad 2",                  /' 113 '/ _
  "Numpad 3",                  /' 114 '/ _
  "Numpad 0",                  /' 115 '/ _
  "Numpad Period",             /' 116 '/ _
  "Print Screen",              /' 117 '/ _
  "Pause",                     /' 118 '/ _
  "Alt",                       /' 119 '/ _
  "Any Enter",                 /' 120 '/ _
  "Command/Win"                /' 121 '/ _
}

static joybuttonnames(128 to 151) as zstring * 22  = { _
  "Gamepad A",                      /' 128 '/ _
  "Gamepad B",                      /' 129 '/ _
  "Gamepad X",                      /' 130 '/ _
  "Gamepad Y",                      /' 131 '/ _
  "Gamepad Left Stick",             /' 132 '/ _
  "Gamepad Right Stick",            /' 133 '/ _
  "Gamepad Back",                   /' 134 '/ _
  "Gamepad Guide",                  /' 135 '/ _
  "Gamepad Start",                  /' 136 '/ _
  "Gamepad L1",                     /' 137 '/ _
  "Gamepad R1",                     /' 138 '/ _
  "Gamepad L2",                     /' 139 '/ _
  "Gamepad R2",                     /' 140 '/ _
  /' 141-143 (buttons 14-16) are unassigned, default names in scancodename() '/ _
  "",                               /' 141 '/ _
  "",                               /' 142 '/ _
  "",                               /' 143 '/ _
  "Gamepad Left",                   /' 144 '/ _
  "Gamepad Right",                  /' 145 '/ _
  "Gamepad Up",                     /' 146 '/ _
  "Gamepad Down",                   /' 147 '/ _
  "Gamepad R Stick Left",           /' 148 '/ _
  "Gamepad R Stick Right",          /' 149 '/ _
  "Gamepad R Stick Up",             /' 150 '/ _
  "Gamepad R Stick Down"            /' 151 '/ _
  /' 152-163 (buttons 21-36) are unassigned, default names in scancodename() '/ _
}
