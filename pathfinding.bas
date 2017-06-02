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


Constructor AStarPathfinder (startpos as XYPair, destpos as XYPair, maxdist as integer=0)
 this.startpos = startpos
 this.destpos = destpos
 this.maxdist = maxdist ' Not implemented yet
 v_new path
End Constructor

Destructor AStarPathfinder
 v_free path
End Destructor

Sub AStarPathfinder.calculate()
 'debug "AStarPathfinder.calculate() " & startpos.x & "," & startpos.y & " -> " & destpos.x & "," & destpos.y
 redim nodes(mapsizetiles.x - 1, mapsizetiles.y - 1) as AStarNode

 dim openlist as AStarNode vector
 v_new openlist
 dim closelist as AStarNode vector
 v_new closelist

 v_resize path, 0

 dim cursor as XYPair
 cursor = startpos
 getnode(cursor).p = cursor

 dim safety as integer = 0
 do
    
  if cursor = destpos then
   'debug "Destination found!"
   'Fill the path result with the parent chain starting at destpos
   v_insert path, 0, destpos
   dim n as AStarNode = getnode(destpos) 
   do
    v_insert path, 0, n.parent
    if n.parent = startpos then exit do
    n = getnode(n.parent)
   loop
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
    
    if not check_wall_edges(cursor.x, cursor.y, direction) then
     'Yes, the adjacent tile is reachable
     
     getnode(nearby).p = nearby
     'Update nearby node's parent, add to the open list
     if getnode(nearby).status = AStarNodeStatus.OPENED then
      'This node is already in the open list, check to see if the current
      'path cost is better than the saved path cost
      if cost_before_node(getnode(cursor)) < cost_before_node(getnode(getnode(nearby).parent)) then
       getnode(nearby).parent = cursor
      end if
     else
      'This node should be added to the open list
      getnode(nearby).parent = cursor
      getnode(nearby).status = AStarNodeStatus.OPENED
      v_append openlist, getnode(nearby)
     end if
     
    end if
   end if
  next direction
  'add cursor node to the closed list
  if getnode(cursor).status <> AStarNodeStatus.CLOSED then
   getnode(cursor).status = AStarNodeStatus.CLOSED
   v_append closelist, getnode(cursor)
  end if

  if v_len(openlist) > 0 then
   dim best as XYPair
   dim best_cost as integer = -1
   for i as integer = 0 to v_len(openlist) - 1
    dim cost as integer = calc_cost(openlist[i])
    if cost < best_cost orelse best_cost = -1 then
     best_cost = cost
     best = openlist[i].p
    end if
   next i
   v_append closelist, getnode(best)
   v_remove openlist, getnode(best)
   getnode(best).status = AStarNodeStatus.CLOSED
   cursor = best
  else
   'Open list was empty, which means no path was found
   debug "open list was empty"
   exit do
  end if

  safety += 1
  if safety > mapsizetiles.x * mapsizetiles.y * 2 then
   debug "AStar safety check: " & safety & " iterations is bigger than double mapsize " & mapsizetiles.x * mapsizetiles.y & " * 2"
   exit do
  end if

 loop
 v_free openlist
 v_free closelist
 
End Sub

Function AStarPathfinder.getnode(p as XYPair) byref as AStarNode
 return nodes(p.x, p.y)
End Function

Function AStarPathfinder.calc_cost(n as AStarNode) as integer
 return cost_before_node(n) + guess_cost_after_node(n)
End Function

Function AStarPathfinder.cost_before_node(n as AStarNode) as integer
 if n.status = AStarNodeStatus.EMPTY then
  debug "ERROR empty node in cost_before_node at " & n.p
  return 1
 end if
 if n.parent = startpos then return 1
 return 1 + cost_before_node(getnode(n.parent))
End Function

Function AStarPathfinder.guess_cost_after_node(n as AStarNode) as integer
 if gmap(5) = 1 then
  'This is a wrapping map
  dim diff as XYPair
  diff.x = destpos.x - n.p.x
  diff.y = destpos.y - n.p.y
  if abs(diff.x) > mapsizetiles.x / 2 then
   diff.x = abs(diff.x) - mapsizetiles.x / 2
  end if
  if abs(diff.y) > mapsizetiles.y / 2 then
   diff.y = abs(diff.y) - mapsizetiles.y / 2 
  end if
  return abs(diff.x) + abs(diff.y)
 else
  return abs(n.p.x - destpos.x) + abs(n.p.y - destpos.y)
 end if
End Function

Sub AStarPathfinder.debug_path()
 dim s as string = " A* path="
 for i as integer = 0 to v_len(path) - 1
  if i > 0 then s &= " "
  s &= path[i].x & "," & path[i].y
 next i
 debug s
End Sub

'------------------------------------------------------------------------------------------

DEFINE_VECTOR_OF_TYPE(AStarNode, AStarNode)

'------------------------------------------------------------------------------------------
