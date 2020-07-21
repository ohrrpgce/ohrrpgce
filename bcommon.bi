'OHRRPGCE - Custom+Game common battle system code
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GNU GPL details and disclaimer of liability

#IFNDEF BCOMMON_BI
#DEFINE BCOMMON_BI

DECLARE FUNCTION equip_elemental_merge(values() as single, byval formula as integer) as single

DECLARE FUNCTION focuscost (byval cost as integer, byval focus as integer) as integer

DECLARE FUNCTION attack_cost_info(byref atk as AttackData, byval focus as integer=0, byval cur_mp as integer=0, byval max_mp as integer=0, byval magic_list_type as integer=0, byval lmp_level as integer=-1, byval cur_lmp as integer=0) as string
DECLARE FUNCTION describe_formation (formdata as Formation) as string
DECLARE FUNCTION describe_formation_by_id (byval form_id as integer) as string

DECLARE FUNCTION attack_placement_over_targetpos(attack as AttackData, targpos as XYZTriple, targsize as XYPair, targ_is_acting_hero as bool) as XYZTriple

#ENDIF
