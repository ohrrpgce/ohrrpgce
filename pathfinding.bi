#ifndef PATHFINDING_BI
#define PATHFINDING_BI

#include "util.bi"
#include "vector.bi"

Enum AStarNodeStatus
 EMPTY
 OPENED
 CLOSED
End Enum

Type AStarNode
 p as XYPair
 status as AStarNodeStatus
 _parent as XYPair
 has_parent as bool
 Declare Property parent () as XYPair
 Declare Property parent (byval new_parent as XYPair)
End Type
DECLARE_VECTOR_OF_TYPE(AStarNode, AStarNode)

Type AStarPathfinder

 startpos as XYPair
 destpos as XYPair

 maxdist as integer = 0 ' Zero means search the whole map.
                        ' A positive number is the max manhattan distance to search.
 
 path as XYPair vector
 consolation as bool ' This will be YES if the resulting path fails to reach the desired destpos
                        
 nodes(ANY, ANY) as AStarNode

 Declare Constructor (startpos as XYPair, destpos as XYPair, maxdist as integer=0)
 Declare Destructor ()
 
 Declare Function getnode(p as XYPair) byref as AStarNode
 
 Declare Sub calculate()
 Declare Sub set_result_path(found_dest as XYPair)
 Declare Function best_node_from_list(list as AStarNode vector) as XYPair
 Declare Static Function node_compare cdecl (byval a as AStarNode ptr, byval b as AStarNode ptr) as long

 Declare Function calc_cost(n as AStarNode) as integer
 Declare Function cost_before_node(n as AStarNode) as integer
 Declare Function guess_cost_after_node(n as AStarNode) as integer
 
 Declare Sub debug_path()
 Declare Sub debug_list(list as AStarNode vector, expected_status as AStarNodeStatus, listname as string ="nodelist")

End Type


'This global is used to hold a temporary pointer to an AStarPathfinder
'when it sorts its node lists. This is needed because the sort function
'requires a static comparison function, and doesn't have a "this" reference.
'
'The side-effect of this global is that it is almost certainly not safe to
'use two AStarPathfinder instances simultaneously in different threads
Extern _pathfinder_obj as AStarPathfinder Ptr

#endif
