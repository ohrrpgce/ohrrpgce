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
 status as AStarNodeStatus = AStarNodeStatus.EMPTY
End Type
DECLARE_VECTOR_OF_TYPE(AStarNode, AStarNode)

Type AStarPathfinder

 startpos as XYPair
 destpos as XYPair

 maxdist as integer = 0 ' Zero means search the whole map.
                        ' A positive number is the max manhattan distance to search.
 
 path as XYPair vector
                        
 nodes(ANY, ANY) as AStarNode

 Declare Constructor (startpos as XYPair, destpos as XYPair, maxdist as integer=0)
 Declare Destructor ()
 
 Declare Function getnode(p as XYPair) byref as AStarNode
 
 Declare Sub calculate()
 Declare Function calc_cost(n as AStarNode) as integer
 Declare Function cost_before_node(n as AStarNode) as integer
 Declare Function guess_cost_after_node(n as AStarNode) as integer
 
 Declare Sub debug_path()

End Type

#endif
