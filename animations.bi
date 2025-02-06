'OHRRPGCE - Animation system
'(C) Copyright 1997-2025 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#ifndef ANIMATIONS_BI
#define ANIMATIONS_BI

#include "config.bi"
#include "const.bi"
#include "reload.bi"


' Contexts in which an animation or animation variant name has a builtin meaning
enum AnimationContext
	acWalkaboutSprite = 1
	acHeroSprite = 2
	acEnemySprite = 4
	acAttackSprite = 8
	acWeaponSprite = 16
	acPortraitSprite = 32

	acAny       = 65535
	'Heroes, enemies, and walkabouts
	acActor     = acWalkaboutSprite or acHeroSprite or acEnemySprite
	'Walkabouts (heroes/npcs)
	acWalkabout = acWalkaboutSprite
	'In-battle heroes and enemies (BattleSprites)
	acBattler   = acHeroSprite or acEnemySprite
	'In-battle heroes
	acBatHero   = acHeroSprite
	'In-battle enemies
	acBatEnemy  = acEnemySprite
	'Walkabout and in-battle heroes
	acHero      = acWalkaboutSprite or acHeroSprite
end enum

' Describes a builtin animation or variant name
type AnimVariantInfo
	name as zstring ptr
	context as AnimationContext
	description as zstring ptr
end type

enum AnimOpType
	animOpUnknown   = -1
	animOpWait      = 0 '(ms)
	animOpWaitMS    = 1 '(ms)
	animOpFrame     = 2 '(frameid)
	animOpRepeat    = 3  '()     Start the animation over
	animOpSetOffset = 4 '(x,y)
	animOpRelOffset = 5 '(x,y)
	animOpPlayFrameGroup = 6 '(groupidx,ms)
	animOpLAST      = 6
end enum

extern anim_op_names() as string      ' Short names used for display and debug
extern anim_op_node_names() as string ' Short names used for RELOAD serialisation
extern anim_op_fullnames() as string  ' Descriptive captions used in editor

type AnimationOp
	type as AnimOpType
	arg1 as integer
	arg2 as integer
end type

#if 0
	#define  DEBUG_ANIM_CACHE(x) x
#else
	#define  DEBUG_ANIM_CACHE(x)
#endif

type Animation
	name as string
	variant as string
	'ops(any) as AnimationOp
	opsnode as Reload.NodePtr   'RELOAD-based replacement for ops()

	'Animation is refcounted only so that animations can be safely replaced in Test Game while they are playing
	refcount as integer

	declare constructor(name as string, variant as string = "")
	declare sub replace_ops(copy_from as Reload.NodePtr)

	'Inc/dec refcount, and delete self
	declare function reference() as Animation ptr
	declare sub dereference()

	declare function append(optype as AnimOpType) as Reload.Node ptr
	declare sub mutate_op(op as Reload.NodePtr, optype as AnimOpType)
end type

'No automatic deletion
DECLARE_VECTOR_OF_TYPE(Animation ptr, Animation_ptr)

type AnimationSet extends Object
	refcount as integer        'If this is an SpriteSet, is set to NOREFC
	animations as Animation ptr vector  'Owned reference to each Animation
	fallback_set as AnimationSet ptr  'AnimationSet to search after `animations`. E.g. the global animations
	                                  'for sprites of this type. May be NULL.
	                                  '(This counts as a reference)
	name as string             'Identifies this set in the editor.
	                           '(Normally blank in SpriteSet, possibly used for debugging)

	declare destructor()
	declare virtual function reference() as AnimationSet ptr
	' Recommended to call the animset_unload() wrapper instead, to zero out the pointer
	declare virtual sub dereference()

	' Note find_animation does not increment refcount!
	declare function find_animation(animvariant as string, exact as bool = NO) as Animation ptr
	declare function new_animation(name as string = "", variant as string = "") as Animation ptr
	declare sub delete_animation(animvariant as string)
	declare sub delete_all_animations(check_no_references as bool = NO)
end type

type SliceFwd as Slice

' The animation state of a SpriteSet instance
type AnimationState
	sl as SliceFwd ptr
	anim as Animation ptr      'The currently playing animation or NULL.
	                           'anim must be set using set_anim()!
	curop as Reload.NodePtr    'Current animation op. Child (future: descendent) of anim->ops
	'anim_step as integer      'Child index of curop
	anim_advanced as bool      'True immediately after anim_step changes, false if waited
	anim_wait as integer       'Equal to 0 if not waiting otherwise the number of ticks into the wait.
	anim_loop as integer       '-1:infinite, 0<:number of times to play after current
	anim_looplimit as integer  '(Private) Number of looping ops remaining before
	                           'infinite loop protection is triggered.
	offset as XYPair

	declare constructor(sl as SliceFwd ptr)
	declare constructor(rhs as AnimationState)
	declare destructor()
	declare sub set_anim(newanim as Animation ptr)

	declare function start_animation overload(name as string, loopcount as integer = 0) as Animation ptr
	declare function start_animation overload(anim as Animation ptr, loopcount as integer = 0) as Animation ptr
	declare sub stop_animation()
	declare sub reset()

	' Three ways to advance the animation:
	' Advance time by one tick
	declare function animate() as bool
	' Advance time until the next wait
	declare function skip_wait() as integer
	' Advance by one animation op (may wait instead of advancing)
	declare function animate_step() as bool
end type


declare sub set_animation_framerate(ms as integer)
declare function get_animation_framerate() as integer
declare function ms_to_frames(ms as integer) as integer
declare function frames_to_ms(frames as integer) as integer

declare sub animset_unload(pp as AnimationSet ptr ptr)
declare sub split_animvariant(animvariant as string, byref animname as string, byref variant as string)

declare sub spriteset_default_global_animations(byref animset as AnimationSet, sprtype as SpriteType)

#endif
