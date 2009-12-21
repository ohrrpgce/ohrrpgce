//gfx_directx_cls_palette.h
//by Jay Tennant 10/30/09
//template class for a palette

#ifndef GFX_DIRECTX_CLS_PALETTE_H
#define GFX_DIRECTX_CLS_PALETTE_H

#include <windows.h>

namespace gfx
{
	template <class T>
	class Palette
	{
	protected:
		T *m_pData;
		UINT m_nDataSize;
		bool m_bAllocated; //false if pointer copied
	public:
		Palette();
		Palette(T *pData, UINT nSize); //copies the pointer
		Palette(const Palette<T>& c); //copies pointer, not allocation
		virtual ~Palette();

		void Allocate(UINT nSize);
		void Free();
		void Copy(T *pData, UINT nSize); //copies data, instead of copying the pointer
		void CopyPointer(T *pData, UINT nSize); //copies pointer, not data; releases previous allocations but won't release this pointer's allocation

		Palette& operator= (const Palette& rhs); //copies data, not pointer

		T& operator[] (UINT entry); //array-style referencing; warning! if entry is higher than number of elements, incorrect memory addresses will be ref'ed
		T* GetPointer(); //returns pointer to data
		UINT GetSize();
	};

	template <class T>
	Palette<T>::Palette() : m_nDataSize(0), m_bAllocated(false), m_pData(NULL)
	{
	}
	template <class T>
	Palette<T>::Palette(T *pData, UINT nSize) : m_nDataSize(nSize), m_bAllocated(false), m_pData(pData)
	{
	}
	template <class T>
	Palette<T>::Palette(const Palette<T>& c) : m_nDataSize(c.m_nDataSize), m_bAllocated(false), m_pData(c.m_pData)
	{
	}
	template <class T>
	Palette<T>::~Palette()
	{
		Free();
	}
	template <class T>
	void Palette<T>::Allocate(UINT nSize)
	{
		Free();
		m_bAllocated = true;
		m_nDataSize = nSize;
		m_pData = new T[m_nDataSize];
	}
	template <class T>
	void Palette<T>::Free()
	{
		if(m_bAllocated)
			if(m_pData)
				delete [] m_pData;
		m_pData = NULL;
		m_bAllocated = false;
		m_nDataSize = 0;
	}
	template <class T>
	void Palette<T>::Copy(T *pData, UINT nSize)
	{
		if(nSize == 0 || m_pData == pData) //size 0 to copy, or data is the same reference
			return;
		if(nSize > m_nDataSize || m_pData == NULL)
			Allocate(nSize);
		for(UINT i = 0; i < m_nDataSize; i++)
			m_pData[i] = pData[i];
	}
	template <class T>
	void Palette<T>::CopyPointer(T *pData, UINT nSize)
	{
		if(m_pData == pData) //in case its referencing the same data
			return;
		Free();
		m_pData = pData;
		m_nDataSize = nSize;
	}
	template <class T>
	Palette<T>& Palette<T>::operator =(const gfx::Palette<T> &rhs)
	{
		if(m_pData == rhs.m_pData) //in case its referencing the same data
			return *this;
		Copy(rhs.m_pData, rhs.m_nDataSize);
		return *this;
	}
	template <class T>
	T& Palette<T>::operator [](UINT entry)
	{
		return m_pData[entry];
	}
	template <class T>
	T* Palette<T>::GetPointer()
	{
		return m_pData;
	}
	template <class T>
	UINT Palette<T>::GetSize()
	{
		return m_nDataSize;
	}



	//based off the following class, converted to allow for different palette types
	//class Palette //should really make this a template, in case we move to a 128-bit floating point surface
	//{
	//protected:
	//	UINT *m_pData;
	//	UINT m_nDataSize;
	//	bool m_bAllocated; //false if pointer copied
	//public:
	//	Palette();
	//	Palette(UINT *pData, UINT nSize); //copies the pointer
	//	virtual ~Palette();

	//	void Allocate(UINT nSize);
	//  void Free();
	//	void Copy(UINT *pData, UINT nSize); //copies data, instead of copying the pointer
	//	void CopyPointer(UINT *pData, UINT nSize); //copies pointer, not data; releases previous allocations but won't release this pointer's allocation

	//	Palette& operator= (const Palette& rhs); //copies data, not pointer

	//	UINT operator[] (UINT entry); //array-style indexing by value, returns 0 if outside array
	//	UINT& operator() (UINT entry); //referencing a specific entry; warning! if entry is higher than number of elements, incorrect memory addresses will be ref'ed
	//	UINT* GetPointer(); //returns pointer to data
	//	UINT GetSize();
	//}:
}

#endif