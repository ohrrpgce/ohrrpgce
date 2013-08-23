#IFNDEF __UICONST_BI__
#DEFINE __UICONST_BI__

'OHRRPGCE - UICONST.BI - UI colour constants
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
CONST uiColors = 62				'last color entry (used by save/load for uicolors.bin lump)
CONST uiColorLast = 17
CONST uiBoxLast = 14

CONST uiBackground = 0          'background colour (not for transparent! transparent is always 0)
CONST uiMenuItem = 1			'normal menu item
CONST uiDisabledItem = 2		'disabled menu item
CONST uiSelectedItem = 3		'selection, alternates 3 & 4
CONST uiSelectedItem2 = 4		'selection, for when thou wishest no alternation
CONST uiSelectedDisabled = 5	'selected disabled item, 5 & 6
CONST uiHighlight = 7			'background to selected menu item
CONST uiHighlight2 = 8			'background used in equip menu
CONST uiTimeBar = 9				'hero's time bar
CONST uiTimeBarFull = 10		'time bar when full
CONST uiHealthBar = 11			'hero's health bar
CONST uiHealthBarFlash = 12		'health bar flash colour (?)
CONST uiText = 13				'Default text colour
CONST uiOutline = 14			'Text outline colour
CONST uiDescription = 15		'Spell description
CONST uiGold = 16				'Total cash
CONST uiShadow = 17				'Vehicle shadow
CONST uiTextBox = 18			'Text box styles, 15 * (bg & border)
CONST uiTextBoxFrame = 48			'Text box border picture set, 15 ints (not colours)

'When you add more constants you MUST update how UI colours are automatically remapped, in nearestui in menus.bas
'and also the indication of used master palette colour in masterpalettemenu
'GuessDefaultUIColors

#ENDIF
