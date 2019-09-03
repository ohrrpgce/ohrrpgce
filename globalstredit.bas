'OHRRPGCE CUSTOM - Global text strings editor
'(C) Copyright 1997-2019 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#include "config.bi"
#include "udts.bi"
#include "util.bi"
#include "allmodex.bi"
#include "common.bi"
#include "menus.bi"
#include "custom.bi"
#include "customsubs.bi"
#include "loading.bi"
#include "vbcompat.bi"


DECLARE SUB writeglobalstring (index as integer, s as string, maxlen as integer)


TYPE GlobalTextStringsMenu
 index(any) as integer
 descriptions(any) as string
 selectable(any) as bool
 shaded(any) as bool    'For headers
 text(any) as string
 defaults(any) as string
 maxlens(any) as integer
 help(any) as string

 DECLARE CONSTRUCTOR ()

 PRIVATE:
 DECLARE SUB add_item (item_index as integer, description as zstring ptr, default as zstring ptr = @"", maxlen as integer = 0, helpfile as zstring ptr = @"", item_selectable as bool = YES, item_shaded as bool = NO)
 DECLARE SUB header (description as zstring ptr)
END TYPE



SUB GlobalTextStringsMenu.add_item (item_index as integer, description as zstring ptr, default as zstring ptr = @"", maxlen as integer = 0, helpfile as zstring ptr = @"", item_selectable as bool = YES, item_shaded as bool = NO)
 a_append selectable(),   item_selectable
 a_append shaded(),       item_shaded
 a_append index(),        item_index
 a_append descriptions(), *description
 a_append text(),         readglobalstring(item_index, *default, maxlen)
 a_append defaults(),     *default
 a_append maxlens(),      maxlen
 a_append help(),         IIF(LEN(*helpfile), "globalstring_" + *helpfile, "")
END SUB

SUB GlobalTextStringsMenu.header (description as zstring ptr)
 add_item -1, "", , , , NO, YES
 add_item -1, description, , , , NO, YES
END SUB

CONSTRUCTOR GlobalTextStringsMenu ()
 '--load current names

 'getelementnames handles the double-defaulting of element names
 DIM elementnames() as string
 getelementnames elementnames()

 add_item -1,  "Return to Main Menu"

 header        " Stats"
 add_item 0,   "Health Points",              "HP", 10
 add_item 1,   "Spell Points",               "MP", 10
 add_item 2,   "Attack Power",               "Atk", 10
 add_item 3,   "Accuracy",                   "Aim", 10
 add_item 4,   "Extra Hits number",          "Hits", 10
 add_item 5,   "Defense/Blocking Power",     "Def", 10
 add_item 6,   "Dodge",                      "Dodge", 10
 add_item 7,   "Unused stat (aka Counter)",  "Ctr", 10
 add_item 8,   "Speed",                      "Speed", 10
 add_item 29,  "Spell Skill",                "Magic", 10
 add_item 30,  "Spell Defense",              "Will", 10
 add_item 31,  "Spell Cost Reduction %",     "MP~", 10

 header        " Elements"
 FOR i as integer = 0 TO gen(genNumElements) - 1
  add_item 174 + i*2, "Elemental " & i,     elementnames(i), 14
 NEXT i

 header        " Equip slots"
 add_item 38,  "Weapon",                     "Weapon", 10
 add_item 25,  "Armor 1",                    "Head", 10
 add_item 26,  "Armor 2",                    "Body", 10
 add_item 27,  "Armor 3",                    "Arms", 10
 add_item 28,  "Armor 4",                    "Legs", 10

 header        " Special Menu Item Default Captions"
 add_item 60,  "Items",                      "Items", 10
 add_item 61,  "Spells",                     "Spells", 10
 add_item 62,  "Status",                     "Status", 10
 add_item 63,  "Equip",                      "Equip", 10
 add_item 64,  "Order",                      "Order", 10
 add_item 65,  "Team",                       "Team", 10
 add_item 66,  "Save",                       "Save", 10
 add_item 322, "Load",                       "Load", 20
 add_item 67,  "Quit",                       "Quit", 10
 add_item 68,  "Minimap",                    "Map", 10
 add_item 69,  "Volume Menu [obsolete]",     "Volume", 10  'Obsolete
 add_item 318, "Music Volume",               "Music", 20
 add_item 320, "Sound Volume",               "Sound", 20
 add_item 308, "TV Margins [consoles only]", "Margins", 10
 add_item 313, "In-App Purchases",           "Purchases", 10
 add_item 314, "Switch to windowed",         "Windowed", 20
 add_item 316, "Switch to fullscreen",       "Fullscreen", 20

 header        " Item Menu"
 add_item 35,  "Exit Item Menu",             "DONE", 10
 add_item 36,  "Sort Item Menu",             "AUTOSORT", 10
 add_item 37,  "Drop Item",                  "TRASH", 10
 add_item 41,  "Drop Prompt",                "Discard", 10
 add_item 42,  "Negative Drop Prefix",       "Cannot", 10

 header        " Status Main Screen"
 add_item 43,  "Level",                      "Level", 10
 add_item 33,  "Experience",                 "Experience", 10
 add_item 47,  "(exp) for next (level)",     "for next", 10
 add_item 32,  "Money",                      "$", 10
 add_item 160, "Level MP",                   "Level MP", 20

 header        " Status Second Screen"
 add_item 302, "Elemental Effects Title",    "Elemental Effects:", 30
 add_item 130, "No Elemental Effects",       "No Elemental Effects", 30
 add_item 162, "Takes > 100% element dmg",   "Weak to $E", 30,   "elemental_resist"
 add_item 165, "Takes 0 to 100% element dmg","Strong to $E", 30, "elemental_resist"
 add_item 168, "Takes 0% element dmg",       "Immune to $E", 30, "elemental_resist"
 add_item 171, "Takes < 0% element dmg",     "Absorb $E", 30,   "elemental_resist"

 header        " Equip Menu"
 add_item 110, "Equip Nothing (unequip)",    "Nothing", 10
 add_item 39,  "Unequip All",                "-REMOVE-", 8
 add_item 40,  "Exit Equip",                 "-EXIT-", 8

 header        " Spells Menu"
 add_item 133, "(hero) has no spells",       "has no spells", 20
 add_item 46,  "Exit Spell List Menu",       "EXIT", 10
 add_item 51,  "Cancel Spell Menu",          "(CANCEL)", 10

 header        " Team/Order Menu"
 add_item 48,  "Remove Hero from Team",      "REMOVE", 10

 header        " Save/Load Menus"
 add_item 52,  "New Game",                   "NEW GAME", 10
 add_item 53,  "Exit Game",                  "EXIT", 10
 add_item 59,  "Cancel Save",                "CANCEL", 10
 add_item 102, "Replace Save Prompt",        "Replace Old Data?", 20
 add_item 44,  "Overwrite Save Yes",         "Yes", 10
 add_item 45,  "Overwrite Save No",          "No", 10
 add_item 154, "day",                        "day", 10
 add_item 155, "days",                       "days", 10
 add_item 156, "hour",                       "hour", 10
 add_item 157, "hours",                      "hours", 10
 add_item 158, "minute",                     "minute", 10
 add_item 159, "minutes",                    "minutes", 10

 header        " Quit Playing Prompt"
 add_item 55,  "Prompt",                     "Quit Playing?", 20
 add_item 57,  "Yes",                        "Yes", 10
 add_item 58,  "No",                         "No", 10

 header        " Shop Menu"
 add_item 70,  "Buy",                        "Buy", 10
 add_item 71,  "Sell",                       "Sell", 10
 add_item 72,  "Inn",                        "Inn", 10
 add_item 73,  "Hire",                       "Hire", 10
 add_item 74,  "Exit",                       "Exit", 10
 add_item 324, "You own (itemcount)",        "You own", 20
 add_item 326, "Equipped (itemcount)",       "Equipped", 20

 header        " Buy/Hire Menu"
 add_item 85,  "Trade for (items, no $)",    "Trade for", 20, "trade_for_prefix"
 add_item 328, "Trade-in amount you have",   " (have $N)", 20, "trade_item_owned"
 add_item 87,  "Hire price prefix",          "Joins for", 20
 add_item 97,  "(#) in stock",               "in stock", 20
 add_item 99,  "Equipability prefix",        "Equip:", 10
 add_item 89,  "Cannot buy prefix",          "Cannot Afford", 20
 add_item 91,  "Cannot hire prefix",         "Cannot Hire", 20
 add_item 305, "Inventory full warning",     "No room in inventory", 30
 add_item 100, "Party full warning",         "No room in party", 20
 add_item 93,  "Buy alert",                  "Purchased", 20
 add_item 95,  "Hire alert (suffix)",        "Joined!", 20
 add_item 309, "The shop is empty",          "The shop is empty", 30

 header        " Sell Menu"
 add_item 77,  "Value: Worth ($) (and...)",  "Worth", 20
 add_item 81,  "($) and a (item)",           "and a", 10
 add_item 153, "($) and (number) (item)",    "and", 10
 add_item 79,  "Value: Trade for (item)",    "Trade for", 20
 add_item 82,  "Worthless item warning",     "Worth Nothing", 20
 add_item 75,  "Unsellable item warning",    "CANNOT SELL", 20
 add_item 84,  "Sell alert",                 "Sold", 10

 header        " Inns"
 add_item 143, "THE INN COSTS (# gold)",     "THE INN COSTS", 20
 add_item 145, "You have (# gold)",          "You have", 20
 add_item 49,  "Pay at Inn",                 "Pay", 10
 add_item 50,  "Cancel Inn",                 "Cancel", 10

 header        " Battles"
 add_item 34,  "Battle Item Menu",           "Item", 10
 add_item 117, "Stole (itemname)",           "Stole", 30
 add_item 111, "Nothing to Steal",           "Has Nothing", 30
 add_item 114, "Steal Failure",              "Cannot Steal", 30
 add_item 120, "When an Attack Misses",      "miss", 20
 add_item 122, "When a Spell Fails",         "fail", 20
 add_item 147, "CANNOT RUN!",                "CANNOT RUN!", 20
 add_item 54,  "Pause",                      "PAUSE", 10
 add_item 126, "Gained (experience)",        "Gained", 10
 add_item 149, "Level up for (hero)",        "Level up for", 20
 add_item 151, "(#) levels for (hero)",      "levels for", 20
 add_item 124, "(hero) learned (spell)",     "learned", 10
 add_item 139, "Found a (item)",             "Found a", 20
 add_item 141, "Found (number) (items)",     "Found", 20
 add_item 125, "Found (gold)",               "Found", 10

 header        " Misc"
 add_item 104, "Status Prompt",              "Whose Status?", 20
 add_item 106, "Spells Prompt",              "Whose Spells?", 20
 add_item 108, "Equip Prompt",               "Equip Who?", 20
 add_item 135, "Plotscript: pick hero",      "Which Hero?", 20
 add_item 137, "Hero name prompt",           "Name the Hero", 20

 '**** next unused index is 330

 'NOTE: if you add global strings here, technically you should update the limit-checking on
 'the implementation of the "get global string" plotscripting command
 '(but that command is actually undocumented and unusable, so it doesn't matter)
END CONSTRUCTOR

SUB global_text_strings_editor()
 DIM search as string = ""
 DIM state as MenuState
 DIM as MenuOptions menuopts, menuopts2
 DIM menu as GlobalTextStringsMenu

 WITH state
  .last = UBOUND(menu.text)
  .autosize = YES
  .autosize_ignore_lines = 3
 END WITH
 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN
   IF LEN(menu.help(state.pt)) THEN
    show_help menu.help(state.pt)
   ELSE
    show_help "edit_global_strings"
   END IF
  END IF
  IF keyval(scCTRL) > 0 AND keyval(scS) > 1 THEN
   IF prompt_for_string(search, "Search (descriptions & values)") THEN
    'Search forward from state.pt
    FOR i as integer = 0 TO state.last
     DIM idx as integer = state.pt + 1 + i
     IF idx > state.last THEN idx = idx - state.last + 1

     IF INSTR(LCASE(menu.text(idx)), LCASE(search)) OR INSTR(LCASE(menu.descriptions(idx)), LCASE(search)) THEN
      state.pt = idx
      correct_menu_state state
      EXIT FOR
     END IF
    NEXT i
   END IF
  END IF
  IF keyval(scCTRL) > 0 AND keyval(scD) > 1 THEN
   'Revert to default
   IF LEN(menu.defaults(state.pt)) THEN
    menu.text(state.pt) = menu.defaults(state.pt)
   END IF
  END IF

  usemenu state, menu.selectable()
  IF state.pt = 0 THEN
   IF enter_space_click(state) THEN EXIT DO
  ELSEIF menu.index(state.pt) <> -1 THEN
   strgrabber menu.text(state.pt), menu.maxlens(state.pt)
  END IF

  clearpage dpage
  menuopts.highlight = (state.pt <> 0) 'Don't hightlight Return to Main Menu
  menuopts.scrollbar = YES
  standardmenu menu.text(), state, 232, 0, dpage, menuopts
  'Since both halves of the menu share the same state and both are active,
  'they both toggle state.tog. So work around that
  state.tog XOR= 1

  menuopts2.disabled_col = uilook(eduiHeading)
  standardmenu menu.descriptions(), state, menu.shaded(), 0, 0, dpage, menuopts2
  edgeprint "CTRL+S Search", 0, pBottom, uilook(uiDisabledItem), dpage
  IF LEN(menu.defaults(state.pt)) THEN
   edgeprint "Default: " & menu.defaults(state.pt), pRight, pBottom, uilook(uiDisabledItem), dpage
  END IF
  IF LEN(menu.help(state.pt)) THEN
   edgeprint "Press F1 for help about this string", 0, pBottom - 10, uilook(uiDisabledItem), dpage
  END IF
  IF menu.index(state.pt) >= 0 THEN
   edgeboxstyle rCenter - (menu.maxlens(state.pt) * 4), pBottom, 8 * menu.maxlens(state.pt) + 4, 10, 0, dpage, transOpaque, YES
   edgeprint menu.text(state.pt), rCenter + 2 - (menu.maxlens(state.pt) * 4), pBottom + 1, uilook(uiText), dpage
  END IF
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

 'Note: it is safe to write the strings to file out of order as long as we write
 'all of them. Any gaps in the file will be filled with garbage: do not leave
 'unused global string indices, or you won't be able to use them later!
 FOR i as integer = 0 TO UBOUND(menu.index)
  writeglobalstring menu.index(i), menu.text(i), menu.maxlens(i)
 NEXT i
 'Write defaults for all elements that don't appear in the menu
 FOR i as integer = gen(genNumElements) TO 63 'maxElements - 1
  writeglobalstring 174 + i*2, "Element" & i+1, 14
 NEXT i

 getstatnames statnames()
END SUB

SUB writeglobalstring (index as integer, s as string, maxlen as integer)
 IF index < 0 THEN EXIT SUB
 DIM fh as integer
 OPENFILE(game & ".stt", FOR_BINARY, fh)
 DIM ch as string
 ch = CHR(small(LEN(s), small(maxlen, 255)))
 PUT #fh, 1 + index * 11, ch
 ch = LEFT(s, small(maxlen, 255))
 PUT #fh, 2 + index * 11, ch
 CLOSE #fh
 loadglobalstrings
END SUB

'==========================================================================================
'                                       Translations
'==========================================================================================


FUNCTION escape_str_for_web_translation(istr as string) as string
 DIM result as string = ""
 DIM as integer icons_low, icons_high
 IF get_font_type(current_font()) = ftypeLatin1 THEN
  icons_low = 127
  icons_high = 160
 ELSE
  icons_low = 127
  icons_high = 255
 END IF
 FOR idx as integer = 1 TO LEN(istr)
  DIM ch as string = MID(istr, idx, 1)
  SELECT CASE CAST(integer, ASC(ch))
   CASE 10  'New line
    result &= LINE_END
   CASE 0 TO 31, icons_low TO icons_high
    result &= "${*" & ASC(ch) & "}"
   CASE ELSE
    result &= ch
  END SELECT
 NEXT idx
 RETURN result
END FUNCTION

'Function for sortint
LOCAL FUNCTION compare_translations(a as HashBucketItem ptr, b as HashBucketItem ptr) as integer
  #DEFINE item_sortorder(x) CAST(TranslationString ptr, x->value)->sortorder
  RETURN item_sortorder(a) - item_sortorder(b)
END FUNCTION

SUB write_translation_file_txt(fname as string, translations as StrHashTable)
  DIM fh as integer
  IF OPENFILE(fname, FOR_OUTPUT + OR_ERROR, fh) THEN EXIT SUB

  PRINT #fh, "# Translation format 0  |  <  '''$ { X }'''  *"
  PRINT #fh, "# Translations for " & trimpath(sourcerpg)
  PRINT #fh, "# Exported " & format_date(NOW)  'FILEDATETIME(sourcerpg))

  DIM items as HashBucketItem vector = translations.items()
  v_sort items, CAST(FnCompare, @compare_translations)

  FOR idx as integer = 0 TO v_len(items) - 1
    DIM code as string ptr = items[idx].key
    WITH *CAST(TranslationString ptr, items[idx].value)
      PRINT #fh,
      IF LEN(.description) THEN PRINT #fh, "# " & .description
      IF .maxlen THEN PRINT #fh, SPACE(LEN(*code) + 4) & STRING(.maxlen, "=")
      DIM tokens as string

      'Convert leading spaces to a WS token
      DIM spaces as integer = skip_over(.text, 1, " ")
      IF spaces THEN tokens &= " ws" & spaces

      IF INSTR(.text, !"\n") THEN
        PRINT #fh, *code & tokens & " <" & LINE_END & escape_str_for_web_translation(.text) & LINE_END & *code & " >"
      ELSE
        PRINT #fh, *code & tokens & " | " & escape_str_for_web_translation(.text)
      END IF
    END WITH
  NEXT

  CLOSE fh

  v_free items
END SUB

'This is a variant on textbox_lines_to_string()
FUNCTION unwrap_textbox(box as TextBox) as string
  DIM ret as string

  CONST leeway as integer = 4  'In characters
  DIM rightmargin as integer = 0  'In characters: space at right of the textbox

  debug "box_to_string"

  FOR idx as integer = 0 TO text_box_last_line(box)
    DIM lin as string = box.text(idx)
    DIM indentation as integer = skip_over(lin, 1, " ")
    DIM unwrap as bool = NO

    IF idx > 0 THEN
      'Heuristic test for whether this line is a continuation of the previous one,
      'in which case they should be joined without a newline.
      'Do this because it greatly confuses automated translators to have spurious newlines,
      'and because we will need to re-wrap the lines anyway.

      DIM firstwordend as integer = INSTR(1 + indentation, lin, " ")
      IF firstwordend = 0 THEN firstwordend = LEN(lin) + 1
      DIM firstwordlen as integer = firstwordend - (1 + indentation)

      'Cap length of the word, because if it's really long then 1) you likely would
      'split it with hyphens if you were wrapping text, 2) it's likely not a word
      firstwordlen = small(firstwordlen, 12)

      debug "firstword " & MID(lin, 1 + indentation, firstwordlen)

      IF LEN(RTRIM(box.text(idx - 1))) + 1 + firstwordlen > 38 - rightmargin - leeway THEN
        'If leeway is 0, this word couldn't have fit on the previous line.
        'But all a little leeway because people often wrap their lines early.
        ret &= " "
      ELSE
        ret &= !"\n"
      END IF
    END IF
    ret &= TRIM(lin)
  NEXT
  RETURN ret
END FUNCTION

SUB create_translations()
  init_translations

  'Textboxes are tricky because the text need to be unwrapped and rewrapped
  FOR id as integer = 0 TO gen(genMaxTextBox)
    DIM box as TextBox
    LoadTextBox box, id
    DIM text as string = unwrap_textbox(box)
    add_translation "tb" & id, text,
    IF box.choice_enabled THEN
      FOR idx as integer = 0 TO UBOUND(box.choice)
        add_translation "tb" & id & "c" & idx, box.choice(idx), "Text box " & id & " choice " & idx
      NEXT
    END IF
  NEXT
END SUB

FUNCTION export_translations (fname as string) as bool
  create_translations
  write_translation_file_txt(fname, translations)
  show_overlay_message "Wrote " & fname
  RETURN YES
END FUNCTION

SUB translations_menu ()
  export_translations "translations.txt"
END SUB
