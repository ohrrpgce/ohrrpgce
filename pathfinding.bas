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

Sub AStarPathfinder.calculate(byval npc as NPCInst Ptr=0)
 'debug "AStarPathfinder.calculate() " & startpos.x & "," & startpos.y & " -> " & destpos.x & "," & destpos.y
 redim nodes(mapsizetiles.x - 1, mapsizetiles.y - 1) as AStarNode

 'openlist is a heap. closelist is just an unsorted list (we barely need it)
 dim openlist as AStarNode vector
 v_new openlist
 dim closelist as AStarNode vector
 v_new closelist

 'Flush the path before we begin
 v_resize path, 0

 dim cursor as XYPair
 cursor = startpos
 getnode(cursor).p = cursor

 dim safety as integer = 0
 do
    
  if cursor = destpos then
   'debug "Destination found!"
   'Fill the path result with the parent chain starting at destpos
   set_result_path(destpos)
   'debug_path()
   exit do
  end if

  for direction as integer = 0 to 3
   dim nearby as XYPair
   nearby = cursor
   wrapaheadxy nearby, direction, 1, 1
   if nearby.x >= 0 andalso nearby.y >= 0 andalso nearby.x < mapsizetiles.x andalso nearby.y < mapsizetiles.y then
    if getnode(nearby).status = AStarNodeStatus.CLOSED then continue for
    if getnode(nearby).status = AStarNodeStatus.OPENED then continue for
    if maxsearch > 0 andalso v_len(openlist) + v_len(closelist) >= maxsearch then continue for
    
    dim collide as bool
    if npc <> 0 then
     'This is a check for an NPC
     dim col_type as WalkaboutCollisionType
     collide = npc_collision_check_at(*npc, cursor, direction, col_type)
     if col_type = collideHero then collide = NO
    else
     'This is a walls-only check
     collide = check_wall_edges(cursor.x, cursor.y, direction)
    end if
    
    if not collide then
     'Yes, the adjacent tile is reachable
     
     getnode(nearby).p = nearby
     'Update nearby node's parent, add to the open list
     if getnode(nearby).status = AStarNodeStatus.OPENED then
      'This node is already in the open list, check to see if the current
      'path cost is better than the saved path cost
      if not getnode(nearby).has_parent then
       getnode(nearby).parent = cursor
      else
       if getnode(cursor).cost_before < getnode(getnode(nearby).parent).cost_before then
        getnode(nearby).parent = cursor
        getnode(nearby).cost_before = cost_before_node(getnode(nearby))
       end if
      end if
     else
      'This node should be added to the open list
      getnode(nearby).parent = cursor
      getnode(nearby).status = AStarNodeStatus.OPENED
      getnode(nearby).cost_before = cost_before_node(getnode(nearby))
      getnode(nearby).cost_after = guess_cost_after_node(getnode(nearby))
      getnode(nearby).dist_squared = xypair_distance_squared(nearby, destpos)
      v_heappush openlist, getnode(nearby)
     end if
     
    end if
   end if
  next direction
  'add cursor node to the closed list
  if getnode(cursor).status <> AStarNodeStatus.CLOSED then
   getnode(cursor).status = AStarNodeStatus.CLOSED
   getnode(cursor).dist_squared = xypair_distance_squared(cursor, destpos)
   v_append closelist, getnode(cursor)
  end if

  if v_len(openlist) > 0 then
   'Open list still has nodes, so pick the best one to be our new cursor
   cursor = openlist[0].p
   openlist[0].status = AStarNodeStatus.CLOSED
   v_append closelist, openlist[0]
   v_heappop openlist
  else
   'Open list was empty, which means no path was found.
   'Choose the best node from the closelist to be the consolation destination
   if v_len(closelist) > 0 then
    dim best as XYPair = best_close_node(closelist)
    set_result_path(best)
   end if
   exit do
  end if

  safety += 1
  if safety > mapsizetiles.x * mapsizetiles.y * 2 then
   debug "AStar safety check: " & safety & " iterations is bigger than double mapsize " & mapsizetiles.x * mapsizetiles.y & " * 2"
   exit do
  end if
  
  'slow_debug()
 loop
 v_free openlist
 v_free closelist
 
End Sub

Sub AStarPathfinder.set_result_path(found_dest as XYPair)
 'We are about to regenerate the path, so flush it first
 v_resize path, 0
 v_insert path, 0, found_dest
 dim n as AStarNode = getnode(found_dest) 
 dim safety as integer = 0
 do
  if not n.has_parent then exit do
  v_insert path, 0, n.parent
  if n.parent = startpos then exit do
  n = getnode(n.parent)
  safety += 1
  if safety > mapsizetiles.x * mapsizetiles.y * 2 then
   debug "AStar result path safety check: " & safety & " iterations is bigger than double mapsize " & mapsizetiles.x * mapsizetiles.y & " * 2"
   'This would probably mean an endless loop caused by a corrupted parentage chain
   exit do
  end if
 loop
 'Update the consolation flag
 consolation = found_dest <> destpos
End Sub

Function AStarPathfinder.best_close_node(list as AStarNode vector) as XYPair
 v_sort(list, cast(FnCompare, @close_node_compare))
 return list[0].p
End Function

Static Function AStarPathfinder.open_node_compare cdecl (byval a as AStarNode ptr, byval b as AStarNode ptr) as long
 'First compare by estimated node cost
 dim cost_a as integer = a->cost_before + a->cost_after
 dim cost_b as integer = b->cost_before + b->cost_after
 if cost_a < cost_b then return -1
 if cost_a > cost_b then return 1
 'Break ties with distance-squared to dest
 if a->dist_squared < b->dist_squared then return -1
 if a->dist_squared > b->dist_squared then return 1
 return 0
End Function

Static Function AStarPathfinder.close_node_compare cdecl (byval a as AStarNode ptr, byval b as AStarNode ptr) as long
 'Only care about distance-squared to dest
 if a->dist_squared < b->dist_squared then return -1
 if a->dist_squared > b->dist_squared then return 1
 return 0
End Function

Function AStarPathfinder.getnode(p as XYPair) byref as AStarNode
 return nodes(p.x, p.y)
End Function

Function AStarPathfinder.cost_before_node(n as AStarNode) as integer
 if n.p = startpos then return 0
 if not n.has_parent then return mapsizetiles.x * mapsizetiles.y
 if n.parent = startpos then return 1
 if n.status = AStarNodeStatus.EMPTY then
  debug "ERROR empty node in cost_before_node at " & n.p
  return 1
 end if
 return 1 + cost_before_node(getnode(n.parent))
End Function

Function AStarPathfinder.guess_cost_after_node(n as AStarNode) as integer
 if gmap(5) = 1 then
  'This is a wrapping map
  dim diff as XYPair = destpos - n.p
  if abs(diff.x) > mapsizetiles.x / 2 then
   diff.x = abs(diff.x) - mapsizetiles.x / 2
  end if
  if abs(diff.y) > mapsizetiles.y / 2 then
   diff.y = abs(diff.y) - mapsizetiles.y / 2 
  end if
  return abs(diff.x) + abs(diff.y)
 else
  return xypair_manhattan_distance(n.p, destpos)
 end if
End Function

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

'DEFINE_VECTOR_OF_TYPE(AStarNode, AStarNode)
'Set compare function
DEFINE_CUSTOM_VECTOR_TYPE(AStarNode, AStarNode, NULL, NULL, NULL, @AStarPathfinder.open_node_compare, NULL, NULL)

'------------------------------------------------------------------------------------------
