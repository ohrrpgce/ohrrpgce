/* OHRRPGCE - Matrix routines
 * (C) Copyright 1997-2022 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 *
 * Functions for creating 3x3 2d transformation matrices, multiplying them, and multiplying 3d vectors (x,y,w) against them
 */

#ifndef MATRIX_MATH_H
#define MATRIX_MATH_H

struct float2 {
	union {
		struct {
			float x, y;
		};
		struct {
			float u, v;
		};
	};

	//float2() : x(0), y(0) {}
	//float2(float X, float Y) : x(X), y(Y) {}
	float2 operator+(const float2& rhs) const {return float2{{{x+rhs.x, y+rhs.y}}};}
	float2 operator-(const float2& rhs) const {return float2{{{x-rhs.x, y-rhs.y}}};}
	float2 operator*(const float2& rhs) const {return float2{{{x*rhs.x, y*rhs.y}}};}
	float2 operator/(const float2& rhs) const {return float2{{{x/rhs.x, y/rhs.y}}};}
	float2& operator+=(const float2& rhs) {x += rhs.x; y += rhs.y; return *this;}
	float2& operator-=(const float2& rhs) {x -= rhs.x; y -= rhs.y; return *this;}
	float2& operator*=(const float2& rhs) {x *= rhs.x; y *= rhs.y; return *this;}
	float2& operator/=(const float2& rhs) {x /= rhs.x; y /= rhs.y; return *this;}
	float2 operator+(float rhs) const {return float2{{{x+rhs, y+rhs}}};}
	float2 operator-(float rhs) const {return float2{{{x-rhs, y-rhs}}};}
	float2 operator*(float rhs) const {return float2{{{x*rhs, y*rhs}}};}
	float2 operator/(float rhs) const {return float2{{{x/rhs, y/rhs}}};}
	float2& operator+=(float rhs) {x += rhs; y += rhs; return *this;}
	float2& operator-=(float rhs) {x -= rhs; y -= rhs; return *this;}
	float2& operator*=(float rhs) {x *= rhs; y *= rhs; return *this;}
	float2& operator/=(float rhs) {x /= rhs; y /= rhs; return *this;}
};

struct float3 {
	float x, y, w;

	float3 operator+(const float3& rhs) const {return float3{x+rhs.x, y+rhs.y, w+rhs.w};}
	float3 operator-(const float3& rhs) const {return float3{x-rhs.x, y-rhs.y, w-rhs.w};}
	float3 operator*(const float3& rhs) const {return float3{x*rhs.x, y*rhs.y, w*rhs.w};}
	float3 operator/(const float3& rhs) const {return float3{x/rhs.x, y/rhs.y, w/rhs.w};}
};

struct float3x3 {
	float _11, _12, _13,
	      _21, _22, _23,
	      _31, _32, _33;
};

union AffineTransform {
	struct {
		float2 bottomleft;
		float2 topleft;
		float2 topright;
		//float2 bottomright;
	};
	float2 vertices[3];
};


extern "C" {

void matrixLocalTransform( float3x3* pMatrixOut, float angle, const float2& scale, const float2& position );
//void matrixMultiply( float3x3* pMatrixOut, const float3x3& A, const float3x3& B );
void vec2Transform( float2* pVec2ArrayOut, int destSize, const float2* pVec2ArrayIn, int srcSize, const float3x3& transformMatrix );
//void vec3Transform( float3* pVec3ArrayOut, int destSize, const float3* pVec3ArrayIn, int srcSize, const float3x3& transformMatrix );
void vec2GenerateCorners( float2* pVecArrayOut, int destSize, const float2& size, const float2& center );
//void vec3GenerateCorners( float3* pVecArrayOut, int destSize, const float& size, const float2& center );
double vec2Distance( float2* p1, float2* p2 );
double vec3Distance( float3* p1, float3* p2 );

}

#endif
