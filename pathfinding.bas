'OHRRPGCE GAME
'(C) Copyright 2017 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
' This module contains code for:
' -A* pathfinding on a tilemap

#include "config.bi"
#include "udts.bi"
#include "gglobals.bi"
#include "common.bi"
#include "loading.bi"
#include "allmodex.bi"
#include "game.bi"
#include "scripting.bi"
#include "moresubs.bi"
#include "pathfinding.bi"
#include "vector.bi"
#include "walkabouts.bi"

'local subs and functions


'==========================================================================================
'                          A* Pathfinding on a Map
'==========================================================================================

Constructor AStarPathfinder (startpos as XYPair, destpos as XYPair, maxsearch as integer=0)
 this.startpos = startpos
 this.destpos = destpos
 this.maxsearch = maxsearch
 v_new path
End Constructor

Destructor AStarPathfinder
 v_free path
End Destructor

Sub AStarPathfinder.calculate(byval npc as NPCInst Ptr=0, byval should_collide_with_hero as bool=NO, byval check_npcs_as_hero as bool=NO, byval should_collide_with_npcs as bool=YES)
 'should_collide_with_hero is only checked when an npc instance is provided
 'check_npcs_as_hero should only be set when the npc ptr is null

 'debug "AStarPathfinder.calculate() " & startpos.x & "," & startpos.y & " -> " & destpos.x & "," & destpos.y
 redim nodes(mapsizetiles.x - 1, mapsizetiles.y - 1) as AStarNode

 'openlist is a heap
 dim openlist as AStarNode vector
 v_new openlist

 'Flush the path before we begin
 v_resize path, 0

 if not should_collide_with_npcs then
  npc = null
  check_npcs_as_hero = NO
 end if

 'pre-cache NPC collisions, but only if we need them.
 dim npc_ccache as NPCCollisionCache
 if npc <> null then
  npc_ccache.populate(mapsizetiles, npc)
 elseif check_npcs_as_hero then
  npc_ccache.populate(mapsizetiles, null, YES)
 end if

 dim cursor as XYPair
 cursor = startpos
 getnode(cursor).p = cursor
 getnode(cursor).status = AStarNodeStatus.OPENED
 guess_cost_after_node(getnode(cursor))

 dim best_closed_node as AStarNode ptr = @(getnode(cursor))
 dim tiles_closed as integer = 0
 do
  dim byref cursornode as AStarNode = getnode(cursor)
    
  if cursor = destpos then
   'debug "Destination found!"
   'Fill the path result with the parent chain starting at destpos
   set_result_path(destpos)
   'debug_path()
   exit do
  end if

  for direction as DirNum = 0 to 3
   dim nearby as XYPair
   nearby = cursor
   wrapaheadxy nearby, direction, 1, 1
   if nearby.x >= 0 andalso nearby.y >= 0 andalso nearby.x < mapsizetiles.x andalso nearby.y < mapsizetiles.y then
    dim byref nearbynode as AStarNode = getnode(nearby)

    if nearbynode.status = AStarNodeStatus.CLOSED then continue for
    if nearbynode.status = AStarNodeStatus.OPENED then continue for
    ' Once we hit the maxsearch limit we don't open any new tiles, but we visit and close any already opened
    if maxsearch > 0 andalso v_len(openlist) + tiles_closed >= maxsearch then continue for
    
    dim collide as bool
    if npc <> null orelse check_npcs_as_hero then
     'This is a check cares about npc collisions
     dim col_type as WalkaboutCollisionType
     if npc = null then
      collide = hero_collision_check_at(0, cursor, direction, col_type, @npc_ccache)
     else
      collide = npc_collision_check_at(*npc, cursor, direction, col_type, @npc_ccache)
     end if
     if col_type = collideHero andalso should_collide_with_hero = NO then collide = NO
    else
     'This is a walls-only check
     collide = check_wall_edges(cursor.x, cursor.y, direction)
    end if
    
    if not collide then
     'Yes, the adjacent tile is reachable
     
     nearbynode.p = nearby
     'Update nearby node's parent, add to the open list
     if nearbynode.status = AStarNodeStatus.OPENED then
      'This node is already in the open list, check to see if the current
      'path cost is better than the saved path cost, and if so update it.
      if not nearbynode.has_parent then
       nearbynode.parent = cursor
      else
       if cursornode.cost_before < getnode(nearbynode.parent).cost_before then
        nearbynode.parent = cursor
        nearbynode.cost_before = cost_before_node(nearbynode)
       end if
      end if
     else
      'This node should be added to the open list
      nearbynode.parent = cursor
      nearbynode.status = AStarNodeStatus.OPENED
      nearbynode.cost_before = cost_before_node(nearbynode)
      guess_cost_after_node(nearbynode)
      v_heappush openlist, nearbynode
     end if

    end if
   end if
  next direction

  'add cursor node to the closed list
  if cursornode.status <> AStarNodeStatus.OPENED then showbug "A*: open list corrupted"
  tiles_closed += 1
  cursornode.status = AStarNodeStatus.CLOSED
  if closed_node_compare(@cursornode, best_closed_node) < 0 then
   best_closed_node = @cursornode
  end if

  if v_len(openlist) > 0 then
   'Open list still has nodes, so pick the best one to be our new cursor
   cursor = openlist[0].p
   v_heappop openlist
  else
   'Open list was empty, which means no path was found.
   'Choose the best node from the closelist to be the consolation destination
   if tiles_closed then
    set_result_path(best_closed_node->p)
   end if
   exit do
  end if

  if tiles_closed > mapsizetiles.x * mapsizetiles.y then
   showbug "A* infinite loop: " & tiles_closed & " iterations is bigger than mapsize"
   exit do
  end if
  
  'slow_debug()
 loop
 v_free openlist
End Sub

Sub AStarPathfinder.set_result_path(found_dest as XYPair)
 'We are about to regenerate the path, so flush it first
 v_resize path, 0
 v_insert path, 0, found_dest
 dim n as AStarNode = getnode(found_dest) 
 dim safety as integer = 0
 do
  if not n.has_parent then exit do
  v_append path, n.parent
  if n.parent = startpos then exit do
  n = getnode(n.parent)
  safety += 1
  if safety > mapsizetiles.x * mapsizetiles.y then
   showbug "AStar result path safety check: " & safety & " iterations is bigger than mapsize"
   'This would probably mean an endless loop caused by a corrupted parentage chain
   exit do
  end if
 loop
 v_reverse path
 'Update the consolation flag
 consolation = found_dest <> destpos
End Sub

Static Function AStarPathfinder.open_node_compare cdecl (byval a as AStarNode ptr, byval b as AStarNode ptr) as long
 'First compare by estimated node cost
 dim cost_a as integer = a->cost_before + a->cost_after
 dim cost_b as integer = b->cost_before + b->cost_after
 if cost_a < cost_b then return -1
 if cost_a > cost_b then return 1
 'Break ties with distance-squared to dest
 if a->cost_after_squared < b->cost_after_squared then return -1
 if a->cost_after_squared > b->cost_after_squared then return 1
 return 0
End Function

Static Function AStarPathfinder.closed_node_compare cdecl (byval a as AStarNode ptr, byval b as AStarNode ptr) as long
 'Only care about distance-squared to dest
 if a->cost_after_squared < b->cost_after_squared then return -1
 if a->cost_after_squared > b->cost_after_squared then return 1
 return 0
End Function

Function AStarPathfinder.getnode(p as XYPair) byref as AStarNode
 return nodes(p.x, p.y)
End Function

Function AStarPathfinder.cost_before_node(n as AStarNode) as integer
 if n.p = startpos then return 0
 if not n.has_parent then return INT_MAX
 if n.status = AStarNodeStatus.EMPTY then
  debug "ERROR empty node in cost_before_node at " & n.p
  return INT_MAX
 end if
 return 1 + getnode(n.parent).cost_before
End Function

Sub AStarPathfinder.guess_cost_after_node(n as AStarNode)
 n.cost_after = xypair_wrapped_distance(n.p, destpos, n.cost_after_squared)
End Sub

Sub AStarPathfinder.slow_debug()
 for y as integer = 0 to mapsizetiles.y - 1
  for x as integer = 0 to mapsizetiles.x - 1
   dim col as integer = 0
   select case nodes(x, y).status
    case AStarNodeStatus.OPENED: col = uilook(uiHighlight)
    case AStarNodeStatus.CLOSED: col = uilook(uiHighlight2)
   end select
   if col then fuzzyrect x * 20 - mapx, y * 20 - mapy, 20, 20, col, vpage 
  next x
 next y
 setvispage vpage
 dowait
 setwait 10
End Sub

Sub AStarPathfinder.debug_path()
 dim s as string = " A* path="
 for i as integer = 0 to v_len(path) - 1
  if i > 0 then s &= " "
  s &= path[i].x & "," & path[i].y
 next i
 debug s
End Sub

Sub AStarPathfinder.debug_list(list as AStarNode vector, expected_status as AStarNodeStatus, listname as string ="nodelist")
 dim s as string = " A* " & listname & "="
 for i as integer = 0 to v_len(list) - 1
  if i > 0 then s &= " "
  s &= list[i].p.x & "," & list[i].p.y
  if list[i].status <> expected_status then
   select case list[i].status
    case AStarNodeStatus.EMPTY: s &= "E"
    case AStarNodeStatus.OPENED: s &= "O"
    case AStarNodeStatus.CLOSED: s &= "C"
   end select
  end if
 next i
 debug s
End Sub

'------------------------------------------------------------------------------------------

Property AStarNode.parent () as XYPair
 if not has_parent then debug "AStarNode.parent: Attempted to access non-existant parent for node " & p.x & "," & p.y
 return _parent
End Property

Property AStarNode.parent (byval new_parent as XYPair)
 _parent = new_parent
 has_parent = YES
End Property

'------------------------------------------------------------------------------------------

Sub NPCCollisionCache.populate(size as XYPair, npci as NPCInst Ptr=null, byval ignore_step_on as bool=NO)
 'Loop through the npc() global and cache them
 'NPCi is a pointer to the NPC that we are checking collisions relative to
 redim obstruct(size.x - 1, size.y - 1) as bool
 dim tpos as XYPair
 for i as integer = 0 TO ubound(npc)
  if npc(i).id > 0 andalso npci <> @npc(i) andalso npc(i).not_obstruction = 0 then
   if ignore_step_on andalso npcs(npc(i).id - 1).activation = 2 then continue for
   tpos.x = (npc(i).x + 10 + npc(i).xgo) \ 20
   tpos.y = (npc(i).y + 10 + npc(i).ygo) \ 20
   'On wrapping maps have to wrap after rounding to the nearest tile, which might be x=width or y=height
   'cropposition(tpos.x, tpos.y, 1)  'Slower
   if tpos.x >= mapsizetiles.x then tpos.x = 0
   if tpos.y >= mapsizetiles.y then tpos.y = 0
   obstruct(tpos.x, tpos.y) = YES
  end if
 next i
End Sub

Sub NPCCollisionCache.debug_cache()
 for y as integer = 0 to ubound(obstruct, 2)
  for x as integer = 0 to ubound(obstruct, 1)
   if obstruct(x, y) then fuzzyrect x * 20 - mapx, y * 20 - mapy, 20, 20, uilook(uiHighlight), vpage 
  next x
 next y
 setvispage vpage
 dowait
 setwait 10
End Sub

'------------------------------------------------------------------------------------------

'Variant of xypair_manhattan_distance which finds the shortest manhattan
'distance around wrapping maps, and also optionally returns the squared distance
Function xypair_wrapped_distance(v1 as XYPair, v2 as XYPair, byref squared_dist as integer = 0) as integer
 dim diff as XYPair = v2 - v1
 diff.x = abs(diff.x)
 diff.y = abs(diff.y)
 if gmap(5) = mapEdgeWrap then
  'This is a wrapping map
  if diff.x > mapsizetiles.x \ 2 then
   diff.x = mapsizetiles.x - diff.x
  end if
  if diff.y > mapsizetiles.y \ 2 then
   diff.y = mapsizetiles.y - diff.y
  end if
 end if
 squared_dist = diff.x * diff.x + diff.y * diff.y
 return diff.x + diff.y
End Function

'------------------------------------------------------------------------------------------

'DEFINE_VECTOR_OF_TYPE(AStarNode, AStarNode)
'Set compare function
DEFINE_CUSTOM_VECTOR_TYPE(AStarNode, AStarNode, NULL, NULL, NULL, @AStarPathfinder.open_node_compare, NULL, NULL, NULL)

'------------------------------------------------------------------------------------------
