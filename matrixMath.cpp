#include "matrixMath.h"
#include <string.h>
#include <math.h>
#include <memory.h>
#include <stddef.h>
#define min(a, b) (a < b ? a : b)

void matrixLocalTransform( float3x3* pMatrixOut, float angle, const float2& scale, const float2& position )
{
   if( pMatrixOut == NULL )
      return;
   memset( pMatrixOut, 0, sizeof(float3x3) );

   pMatrixOut->_11 = cos(angle) * scale.x;
   pMatrixOut->_12 = sin(angle) * scale.x;
   pMatrixOut->_21 = -sin(angle) * scale.y;
   pMatrixOut->_22 = cos(angle) * scale.y;
   pMatrixOut->_31 = position.x;
   pMatrixOut->_32 = position.y;
   pMatrixOut->_33 = 1.0f;
}

void matrixOldClientTransform( float3x3* pMatrixOut, float clientWidth, float clientHeight )
{
   if( pMatrixOut == NULL )
      return;
   memset( pMatrixOut, 0, sizeof(float3x3) );

   pMatrixOut->_11 = clientWidth / 320.0f;
   pMatrixOut->_22 = clientHeight / 200.0f;
   pMatrixOut->_33 = 1.0f;
}

void matrixMultiply( float3x3* pMatrixOut, const float3x3& A, const float3x3& B )
{
   if( pMatrixOut == NULL )
      return;
   memset( pMatrixOut, 0, sizeof(float3x3) );

   pMatrixOut->_11 = A._11 * B._11 + A._12 * B._21 + A._13 * B._31;
   pMatrixOut->_12 = A._11 * B._12 + A._12 * B._22 + A._13 * B._32;
   pMatrixOut->_13 = A._11 * B._13 + A._12 * B._23 + A._13 * B._33;

   pMatrixOut->_21 = A._21 * B._11 + A._22 * B._21 + A._23 * B._31;
   pMatrixOut->_22 = A._21 * B._12 + A._22 * B._22 + A._23 * B._32;
   pMatrixOut->_23 = A._21 * B._13 + A._22 * B._23 + A._23 * B._33;

   pMatrixOut->_31 = A._31 * B._11 + A._32 * B._21 + A._33 * B._31;
   pMatrixOut->_32 = A._31 * B._12 + A._32 * B._22 + A._33 * B._32;
   pMatrixOut->_33 = A._31 * B._13 + A._32 * B._23 + A._33 * B._33;
}

void vec3Transform( float3* pVec3ArrayOut, int destSize, const float3* pVec3ArrayIn, int srcSize, const float3x3& transformMatrix )
{
   if( pVec3ArrayOut == NULL || pVec3ArrayIn == NULL )
      return;
   memset( pVec3ArrayOut, 0, sizeof(float3) * destSize );

   for(int i = 0, maxCount = min(srcSize, destSize); i < maxCount; i++)
   {
      pVec3ArrayOut[i].x = pVec3ArrayIn[i].x * transformMatrix._11 + pVec3ArrayIn[i].y * transformMatrix._21 + pVec3ArrayIn[i].w * transformMatrix._31;
      pVec3ArrayOut[i].y = pVec3ArrayIn[i].x * transformMatrix._12 + pVec3ArrayIn[i].y * transformMatrix._22 + pVec3ArrayIn[i].w * transformMatrix._32;
      pVec3ArrayOut[i].w = pVec3ArrayIn[i].x * transformMatrix._13 + pVec3ArrayIn[i].y * transformMatrix._23 + pVec3ArrayIn[i].w * transformMatrix._33;
   }
}

void vec3GenerateCorners( float3* pVecArrayOut, int destSize, const SURFACE_RECT& surfaceRect )
{
   if( pVecArrayOut == NULL || destSize < 4 )
      return;
   memset( pVecArrayOut, 0, sizeof(float3) * destSize );

   int width = surfaceRect.right - surfaceRect.left;
   int height = surfaceRect.bottom - surfaceRect.top;

   pVecArrayOut[0].x = (float)-width / 2.0f;
   pVecArrayOut[0].y = (float)-height / 2.0f;
   pVecArrayOut[0].w = 1.0f;

   pVecArrayOut[1].x = (float)width / 2.0f;
   pVecArrayOut[1].y = (float)-height / 2.0f;
   pVecArrayOut[1].w = 1.0f;

   pVecArrayOut[2].x = (float)width / 2.0f;
   pVecArrayOut[2].y = (float)height / 2.0f;
   pVecArrayOut[2].w = 1.0f;

   pVecArrayOut[3].x = (float)-width / 2.0f;
   pVecArrayOut[3].y = (float)height / 2.0f;
   pVecArrayOut[3].w = 1.0f;
}
