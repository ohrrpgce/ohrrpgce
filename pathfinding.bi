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
 parent as XYPair
 status as AStarNodeStatus
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
 Declare Function calc_cost(n as AStarNode) as integer
 Declare Function cost_before_node(n as AStarNode) as integer
 Declare Function guess_cost_after_node(n as AStarNode) as integer
 
 Declare Sub debug_path()
 Declare Sub debug_list(list as AStarNode vector, expected_status as AStarNodeStatus, listname as string ="nodelist")

End Type

#endif
