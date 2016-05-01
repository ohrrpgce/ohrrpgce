'matrixMath.bi
'created 5/25/11
'exposes functions for creating 3x3 2d transformation matrices, multiplying them, and multiplying 3d vectors (x,y,w) against them

#IFNDEF MATRIX_MATH_BI
#DEFINE MATRIX_MATH_BI

#include "config.bi" 

TYPE Float2
  x as single
  y as single
END TYPE

TYPE Float3
  x as single
  y as single
  w as single
END TYPE

TYPE Float3x3
  _11 as single : _12 as single : _13 as single
  _21 as single : _22 as single : _23 as single
  _31 as single : _32 as single : _33 as single
END TYPE

TYPE Rect
  left as integer
  top as integer
  right as integer
  bottom as integer
END TYPE

EXTERN "C"

'transforms from local coordinates to the specified scale, rotation (clockwise by "angle"), and translation; assembled in manner of Scale-Rotate-Transform (SRT)
DECLARE SUB matrixLocalTransform( byval pMatrixOut as float3x3 ptr, byval angle as single, byref scale as float2, byref position as float2 )

'performs the transform from the 320x200 resolution limitation to whatever the client size actually is; for back-compat
DECLARE SUB matrixOldClientTransform( byval pMatrixOut as float3x3 ptr, byval clientWidth as single, byval clientHeight as single )

'multiplies matrices together; pMatrixOut = A x B
DECLARE SUB matrixMultiply( byval pMatrixOut as float3x3 ptr, byref A as float3x3, byref B as float3x3 )

'transforms all the vectors in pVec3ArrayIn into pVec3ArrayOut by the "transformMatrix"
DECLARE SUB vec3Transform( byval pVec3ArrayOut as float3 ptr, byval destSize as integer, byval pVec3ArrayIn as float3 ptr, byval srcSize as integer, byref transformMatrix as float3x3 )

'generates the local coordinate corners of a quad based on the width and height of the passed in "surfaceRect"; to be used as input to vec3Transform
DECLARE SUB vec3GenerateCorners( byval pVecArrayOut as float3 ptr, byval destSize as integer, byref surfaceRect as Rect )

END EXTERN

#ENDIF
