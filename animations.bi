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
	animOpSetProp   = 7 'key, value
	animOpSwitchAnim = 8
	animOpTween     = 9
	animOpLAST      = 9
end enum

extern anim_op_names() as string      ' Short names used for display and debug
extern anim_op_node_names() as string ' Short names used for RELOAD serialisation
extern anim_op_fullnames() as string  ' Descriptive captions used in editor

#if 0
	#define  DEBUG_ANIM_CACHE(x) x
#else
	#define  DEBUG_ANIM_CACHE(x)
#endif

type Animation
	name as string
	variant as string
	opsnode as Reload.NodePtr   'The parent node to the animation ops nodes. Never NULL

	'Animation is refcounted only so that animations can be safely replaced in Test Game while they are playing
	refcount as integer

	declare constructor(name as string, variant as string = "")
	declare destructor()
	declare sub replace_ops(copy_from as Reload.NodePtr)

	'Inc/dec refcount, and delete self
	declare function reference() as Animation ptr
	declare sub dereference()
	declare function duplicate() as Animation ptr

	declare function append(optype as AnimOpType) as Reload.Node ptr
	declare sub mutate_op(op as Reload.NodePtr, optype as AnimOpType)
end type

'No automatic deletion or copying
DECLARE_VECTOR_OF_TYPE(Animation ptr, Animation_ptr)

type AnimationSet
	refcount as integer        'If this is an SpriteSet, is set to NOREFC
	animations as Animation ptr vector  'Owned reference to each Animation
	shared_set as AnimationSet ptr   'Optional AnimationSet to search after `animations`. (referenced)
	                                 'This is used for animations shared within a collection.
	                                 '(Not implemented yet).
	                                 'We don't recurse to shared_set->fallback_set!
	fallback_set as AnimationSet ptr 'Optional, searched after fallback_set. (referenced)
	                                 'Used for spriteset defaults and global spriteset animations.
	                                 'We don't recurse to fallback_set->shared_set!
	slice_specific as bool     'True if this AnimationSet is for a specific slice rather than some fallback set
	name as string             'Identifies this set in the editor.
	                           '(Normally blank in SpriteSet, possibly used for debugging)

	declare destructor()
	declare function reference() as AnimationSet ptr
	' Recommended to call the animset_unload() wrapper instead, to zero out the pointer
	declare sub dereference()
	declare function duplicate() as AnimationSet ptr
	declare sub unload_shared_animsets()

	' Note find_animation does not increment refcount!
	declare function find_animation(animvariant as string, exact as bool = NO, recurse as bool = YES) as Animation ptr
	declare function get_animation(animvariant as string) as Animation ptr
	declare function new_animation(name as string = "", variant as string = "") as Animation ptr
	declare sub delete_animation(anim as Animation ptr)
	declare sub delete_all_animations(check_no_references as bool = NO)

  private:
	declare function find_animation_recurse(animname as string, variant as string, exact as bool, recurse1 as bool, recurse2 as bool, byref _best_score as integer) as Animation ptr
end type

type SliceFwd as Slice

' The animation state of a SpriteSet instance
type AnimationState
	sl as SliceFwd ptr
	anim as Animation ptr      'The currently playing animation or NULL.
	                           'anim must be set using set_anim()!
	curop as Reload.NodePtr    'Current animation op. Child (future: descendent) of anim->ops
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
	declare function switch_animation overload(animvariant as string, loopcount as integer = -1) as bool
	declare function switch_animation overload(to_anim as Animation ptr, loopcount as integer = -1) as bool
	declare sub stop_animation()
	declare sub reset()

	enum StepResult
		stepError
		stepWait
		stepNext
		stepEnd
	end enum

	' Three ways to advance the animation:
	' Advance time by one tick
	declare function animate() as bool
	' Advance time until the next wait
	declare function skip_wait() as integer
	' Advance by one animation op (may wait instead of advancing)
	declare function animate_step() as StepResult
end type

declare sub set_animation_framerate(ms as integer)
declare function get_animation_framerate() as integer
declare function ms_to_frames(ms as integer) as integer
declare function frames_to_ms(frames as integer) as integer

declare function get_anim_doc() as Reload.DocPtr

declare sub set_slice_property(sl as SliceFwd ptr, prop as zstring ptr, datnode as Reload.NodePtr)
declare sub interpolate_slice_property(sl as SliceFwd ptr, prop as zstring ptr, x as double, value0 as Reload.NodePtr, value1 as Reload.NodePtr)

declare sub animset_unload(pp as AnimationSet ptr ptr)
declare sub split_animvariant(animvariant as string, byref animname as string, byref variant as string)

declare sub spriteset_default_global_animations(byref animset as AnimationSet, sprtype as SpriteType)

#endif
