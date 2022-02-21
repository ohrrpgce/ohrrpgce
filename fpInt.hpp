/* OHRRPGCE - low level file interface layer
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 *
 * Written by Jay Tennant 5/31/11
 * Fixed point math with template overloads.
 * Precision is 65536 fractional steps.
 * Range is from (-32768.9998...) to (32767.9998...)
 */

#ifndef FPINT_H
#define FPINT_H

#include <stdint.h>
#include "matrixMath.h"

struct FPInt
{
	union
	{
		int32_t raw : 32;   //two words (below)
		struct
		{
			uint32_t fraction : 16; //low word
			int32_t whole : 16;     //high word
		};
	};
	// Equivalent to but far faster than whole = 0;, which optimising compilers hate
	void fractionOnly() { raw &= 0xffff; }

	//math operators
	FPInt operator+ (const FPInt& rhs) const {FPInt ret; ret.raw = raw + rhs.raw; return ret;}
	FPInt operator- (const FPInt& rhs) const {FPInt ret; ret.raw = raw - rhs.raw; return ret;}
	FPInt operator- () const {
		FPInt ret; 
		ret.raw = -raw; 
		return ret;
	}
	FPInt operator* (const FPInt& rhs) const {
		long long n = (long long)raw * (long long)rhs.raw; 
		FPInt ret; 
		ret.raw = (int32_t)(n >> 16);// & 0xffffffff; //throw out lowest and highest words, keeping only middle two from multiply
		return ret;
	}
	FPInt operator* (int rhs) const {
		FPInt ret;
		ret.raw = raw * rhs;
		return ret;
	}
	FPInt operator/ (const FPInt& rhs) const {
		long long n = ((long long)raw) << 16; //shift first item up by 16 bits
		n /= (long long)rhs.raw;
		FPInt ret;
		ret.raw = (int32_t)n;// & 0xffffffff;
		return ret;
	}

	template <class T>
	FPInt operator+ (const T& rhs) const {return operator+ (FPInt(rhs));}
	template <class T>
	FPInt operator- (const T& rhs) const {return operator- (FPInt(rhs));}
	template <class T>
	FPInt operator* (const T& rhs) const {return operator* (FPInt(rhs));}
	template <class T>
	FPInt operator/ (const T& rhs) const {return operator/ (FPInt(rhs));}


	//assignment operators
	FPInt& operator= (const FPInt& rhs) {raw = rhs.raw; return *this;}
	FPInt& operator+= (const FPInt& rhs) {raw += rhs.raw; return *this;}
	FPInt& operator-= (const FPInt& rhs) {raw -= rhs.raw; return *this;}
	FPInt& operator*= (const FPInt& rhs) {
		long long n = (long long)raw * (long long)rhs.raw; 
		raw = (int32_t)(n >> 16);// & 0xffffffff; //throw out lowest and highest words, keeping only middle two from multiply
		return *this;
	}
	FPInt& operator*= (int rhs) {raw *= rhs; return *this;}
	FPInt& operator/= (const FPInt& rhs) {
		long long n = ((long long)raw) << 16; //shift first item up by 16 bits
		n /= (long long)rhs.raw;
		raw = (int32_t)n;// & 0xffffffff;
		return *this;
	}

	template <class T>
	FPInt& operator= (const T& rhs) {return operator= (FPInt(rhs));}
	template <class T>
	FPInt& operator+= (const T& rhs) {return operator+= (FPInt(rhs));}
	template <class T>
	FPInt& operator-= (const T& rhs) {return operator-= (FPInt(rhs));}
	template <class T>
	FPInt& operator*= (const T& rhs) {return operator*= (FPInt(rhs));}
	template <class T>
	FPInt& operator/= (const T& rhs) {return operator/= (FPInt(rhs));}


	//comparison operators
	bool operator< (const FPInt& rhs) const {return raw < rhs.raw;}
	bool operator> (const FPInt& rhs) const {return raw > rhs.raw;}
	bool operator<= (const FPInt& rhs) const {return raw <= rhs.raw;}
	bool operator>= (const FPInt& rhs) const {return raw >= rhs.raw;}
	bool operator== (const FPInt& rhs) const {return raw == rhs.raw;}
	bool operator!= (const FPInt& rhs) const {return raw != rhs.raw;}

	template <class T>
	bool operator< (const T& rhs) const {return operator< (FPInt(rhs));}
	template <class T>
	bool operator> (const T& rhs) const {return operator> (FPInt(rhs));}
	template <class T>
	bool operator<= (const T& rhs) const {return operator<= (FPInt(rhs));}
	template <class T>
	bool operator>= (const T& rhs) const {return operator>= (FPInt(rhs));}
	template <class T>
	bool operator== (const T& rhs) const {return operator== (FPInt(rhs));}
	template <class T>
	bool operator!= (const T& rhs) const {return operator!= (FPInt(rhs));}


	//casting operators
	operator double () const {return ((double)whole + (double)(fraction) * 0.0000152587890625);}
	operator float () const {return ((float)whole + (float)(fraction) * 0.0000152587890625f);}
	operator long long () const {return (long long)whole;}
	operator int () const {return (int)whole;}
	operator short () const {return (short)whole;}
	operator char () const {return (char)whole;}
	operator unsigned long long () const {return (unsigned long long)whole;}
	operator unsigned int () const {return (unsigned int)whole;}
	operator unsigned short () const {return (unsigned short)whole;}
	operator unsigned char () const {return (unsigned char)whole;}

	//ctor's
	FPInt() : raw(0) {}
	FPInt(const FPInt& c) : raw(c.raw) {}
	FPInt(float n) : raw(0) {raw = (int32_t)(n * 65536.0f);}
	FPInt(double n) : raw(0) {raw = (int32_t)(n * 65536.0);}
	FPInt(char n) : raw(0) {whole = n;}
	FPInt(short n) : raw(0) {whole = n;}
	FPInt(int n) : raw(0) {whole = n;}
	FPInt(long long n) : raw(0) {whole = n;}
	FPInt(unsigned char n) : raw(0) {whole = n;}
	FPInt(unsigned short n) : raw(0) {whole = n;}
	FPInt(unsigned int n) : raw(0) {whole = n;}
	FPInt(unsigned long long n) : raw(0) {whole = n;}
};

// Unused. Just enough methods to use as TexCoord
struct FPInt2 {
	FPInt u,v;
	FPInt2() : u(0), v(0) {}
	FPInt2(FPInt U, FPInt V) : u(U), v(V) {}
	FPInt2(const float2& coord) : u(coord.u), v(coord.v) {}
	operator float2() const {return float2{{{u, v}}};}
	FPInt2 operator- (const FPInt2& rhs) const {return FPInt2{u - rhs.u, v - rhs.v};}
	FPInt2& operator+= (const FPInt2& rhs) {u += rhs.u; v += rhs.v; return *this;}
	FPInt2& operator*= (float rhs) {u *= rhs; v *= rhs; return *this;}
};


#endif
