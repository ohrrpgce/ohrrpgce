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
 cost_before as integer
 cost_after as integer
 dist_squared as integer
End Type
DECLARE_VECTOR_OF_TYPE(AStarNode, AStarNode)

Type AStarPathfinder

 startpos as XYPair
 destpos as XYPair

 maxsearch as integer = 0 ' Zero means search the whole map.
                        ' A positive number is the max number of open+close tiles to search.
 
 path as XYPair vector
 consolation as bool ' This will be YES if the resulting path fails to reach the desired destpos
                        
 nodes(ANY, ANY) as AStarNode

 Declare Constructor (startpos as XYPair, destpos as XYPair, maxsearch as integer=0)
 Declare Destructor ()
 
 Declare Function getnode(p as XYPair) byref as AStarNode
 
 Declare Sub calculate(byval npc as NPCInst Ptr=0)
 Declare Sub set_result_path(found_dest as XYPair)

 Declare Function best_open_node(list as AStarNode vector) as XYPair
 Declare Function best_close_node(list as AStarNode vector) as XYPair
 Declare Static Function open_node_compare cdecl (byval a as AStarNode ptr, byval b as AStarNode ptr) as long
 Declare Static Function close_node_compare cdecl (byval a as AStarNode ptr, byval b as AStarNode ptr) as long

 Declare Function cost_before_node(n as AStarNode) as integer
 Declare Function guess_cost_after_node(n as AStarNode) as integer
 
 Declare Sub debug_path()
 Declare Sub debug_list(list as AStarNode vector, expected_status as AStarNodeStatus, listname as string ="nodelist")
 Declare Sub slow_debug()

End Type

#endif
