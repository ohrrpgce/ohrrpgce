'OHRRPGCE - Custom+Game common battle system code
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#IFNDEF BCOMMON_BI
#DEFINE BCOMMON_BI

DECLARE FUNCTION equip_elemental_merge(values() as single, byval formula as integer) as single

DECLARE FUNCTION focuscost (byval cost as integer, byval focus as integer) as integer

DECLARE FUNCTION attack_cost_info(byref atk as AttackData, byval focus as integer=0, byval cur_mp as integer=0, byval max_mp as integer=0, byval magic_list_type as integer=0, byval lmp_level as integer=-1, byval cur_lmp as integer=0) as string
DECLARE FUNCTION describe_formation (formdata as Formation) as string
DECLARE FUNCTION describe_formation_by_id (byval form_id as integer) as string

DECLARE FUNCTION attack_placement_over_targetpos(attack as AttackData, targpos as XYZTriple, targsize as XYPair, targ_is_acting_hero as bool=NO, byval reverse as integer=0) as XYZTriple

DECLARE FUNCTION get_battlefield_size() as XYPair
DECLARE FUNCTION get_battle_res() as XYPair
DECLARE FUNCTION get_formation_bounds() as RectPoints

#DEFINE HERO_FORM_OFFSET XY(240, 82)

#ENDIF
