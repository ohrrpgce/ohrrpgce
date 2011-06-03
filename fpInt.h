//fpInt2.h
//by Jay Tennant 5/31/11
//exposes fixed point math with template overloads
//precision is 65536 fractional steps 
//range is from (-32768.9998...) to (32767.9998...)

#pragma once

struct FPInt
{
	union
	{
		__int32 raw : 32;		//two words (below)
		struct
		{
			unsigned __int16 fraction : 16;		//low word
			__int16 whole : 16;					//high word
		};
	};

	//math operators
	FPInt operator+ (const FPInt& rhs) const {FPInt ret; ret.raw = raw + rhs.raw; return ret;}
	FPInt operator- (const FPInt& rhs) const {FPInt ret; ret.raw = raw - rhs.raw; return ret;}
	FPInt operator- () const {
		FPInt ret; 
		ret.raw = -raw; 
		return ret;
	}
	FPInt operator* (const FPInt& rhs) const {
		__int64 n = (__int64)raw * (__int64)rhs.raw; 
		FPInt ret; 
		ret.raw = (__int32)(n >> 16);// & 0xffffffff; //throw out lowest and highest words, keeping only middle two from multiply
		return ret;
	}
	FPInt operator/ (const FPInt& rhs) const {
		__int64 n = ((__int64)raw) << 16; //shift first item up by 16 bits
		n /= (__int64)rhs.raw;
		FPInt ret;
		ret.raw = (__int32)n;// & 0xffffffff;
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
		__int64 n = (__int64)raw * (__int64)rhs.raw; 
		raw = (__int32)(n >> 16);// & 0xffffffff; //throw out lowest and highest words, keeping only middle two from multiply
		return *this;
	}
	FPInt& operator/= (const FPInt& rhs) {
		__int64 n = ((__int64)raw) << 16; //shift first item up by 16 bits
		n /= (__int64)rhs.raw;
		raw = (__int32)n;// & 0xffffffff;
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
	operator long () const {return (long)whole;}
	operator int () const {return (int)whole;}
	operator short () const {return (short)whole;}
	operator char () const {return (char)whole;}
	operator unsigned long long () const {return (unsigned long long)whole;}
	operator unsigned long () const {return (unsigned long)whole;}
	operator unsigned int () const {return (unsigned int)whole;}
	operator unsigned short () const {return (unsigned short)whole;}
	operator unsigned char () const {return (unsigned char)whole;}

	//ctor's
	FPInt() : raw(0) {}
	FPInt(const FPInt& c) : raw(c.raw) {}
	FPInt(float n) : raw(0) {raw = (__int32)(n * 65536.0f);}
	FPInt(double n) : raw(0) {raw = (__int32)(n * 65536.0);}
	FPInt(char n) : raw(0) {whole = n;}
	FPInt(short n) : raw(0) {whole = n;}
	FPInt(int n) : raw(0) {whole = n;}
	FPInt(long n) : raw(0) {whole = n;}
	FPInt(long long n) : raw(0) {whole = n;}
	FPInt(unsigned char n) : raw(0) {whole = n;}
	FPInt(unsigned short n) : raw(0) {whole = n;}
	FPInt(unsigned int n) : raw(0) {whole = n;}
	FPInt(unsigned long n) : raw(0) {whole = n;}
	FPInt(unsigned long long n) : raw(0) {whole = n;}
};