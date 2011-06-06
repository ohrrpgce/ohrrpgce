'matrixMath.bi
'created 5/25/11
'exposes functions for creating 3x3 2d transformation matrices, multiplying them, and multiplying 3d vectors (x,y,w) against them

#IFNDEF MATRIX_MATH_BI
#DEFINE MATRIX_MATH_BI

TYPE Float2
  x AS SINGLE
  y AS SINGLE
END TYPE

TYPE Float3
  x AS SINGLE
  y AS SINGLE
  w AS SINGLE
END TYPE

TYPE Float3x3
  _11 AS SINGLE : _12 AS SINGLE : _13 AS SINGLE
  _21 AS SINGLE : _22 AS SINGLE : _23 AS SINGLE
  _31 AS SINGLE : _32 AS SINGLE : _33 AS SINGLE
END TYPE

TYPE Rect
  left AS LONG
  top AS LONG
  right AS LONG
  bottom AS LONG
END TYPE

EXTERN "C"

'transforms from local coordinates to the specified scale, rotation (clockwise by "angle"), and translation; assembled in manner of Scale-Rotate-Transform (SRT)
DECLARE SUB matrixLocalTransform( BYVAL pMatrixOut AS float3x3 ptr, BYVAL angle AS SINGLE, BYREF scale AS float2, BYREF position AS float2 )

'performs the transform from the 320x200 resolution limitation to whatever the client size actually is; for back-compat
DECLARE SUB matrixOldClientTransform( BYVAL pMatrixOut AS float3x3 ptr, BYVAL clientWidth AS SINGLE, BYVAL clientHeight AS SINGLE )

'multiplies matrices together; pMatrixOut = A x B
DECLARE SUB matrixMultiply( BYVAL pMatrixOut AS float3x3 ptr, BYREF A AS float3x3, BYREF B AS float3x3 )

'transforms all the vectors in pVec3ArrayIn into pVec3ArrayOut by the "transformMatrix"
DECLARE SUB vec3Transform( BYVAL pVec3ArrayOut AS float3 ptr, BYVAL destSize AS INTEGER, BYVAL pVec3ArrayIn AS float3 ptr, BYVAL srcSize AS INTEGER, BYREF transformMatrix AS float3x3 )

'generates the local coordinate corners of a quad based on the width and height of the passed in "surfaceRect"; to be used as input to vec3Transform
DECLARE SUB vec3GenerateCorners( BYVAL pVecArrayOut AS float3 ptr, BYVAL destSize AS INTEGER, BYREF surfaceRect AS Rect )

END EXTERN

#ENDIF
