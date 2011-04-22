//palette.h
//by Jay Tennant 10/30/09; updated 4/21/11
//template class for a palette

#pragma once

namespace gfx
{
	template <class T>
	class Palette
	{
	protected:
		T *m_pData;
		unsigned int m_nDataSize;
		bool m_bAllocated; //false if pointer copied
	public:
		Palette();
		Palette(T *pData, unsigned int nSize); //copies the pointer
		Palette(const Palette<T>& c); //copies pointer, not allocation
		virtual ~Palette();

		void allocate(unsigned int nSize);
		void free();
		void copy(T *pData, unsigned int nSize); //copies data, instead of copying the pointer
		void copyPointer(T *pData, unsigned int nSize); //copies pointer, not data; releases previous allocations but won't release this pointer's allocation

		Palette& operator= (const Palette& rhs); //copies data, not pointer

		T& operator[] (unsigned int entry); //array-style referencing; warning! if entry is higher than number of elements, incorrect memory addresses will be ref'ed
		T* getPointer(); //returns pointer to data
		unsigned int size();
	};

	template <class T>
	Palette<T>::Palette() : m_nDataSize(0), m_bAllocated(false), m_pData(0)
	{
	}
	template <class T>
	Palette<T>::Palette(T *pData, unsigned int nSize) : m_nDataSize(nSize), m_bAllocated(false), m_pData(pData)
	{
	}
	template <class T>
	Palette<T>::Palette(const Palette<T>& c) : m_nDataSize(c.m_nDataSize), m_bAllocated(false), m_pData(c.m_pData)
	{
	}
	template <class T>
	Palette<T>::~Palette()
	{
		free();
	}
	template <class T>
	void Palette<T>::allocate(unsigned int nSize)
	{
		free();
		m_bAllocated = true;
		m_nDataSize = nSize;
		m_pData = new T[m_nDataSize];
	}
	template <class T>
	void Palette<T>::free()
	{
		if(m_bAllocated)
			if(m_pData)
				delete [] m_pData;
		m_pData = 0;
		m_bAllocated = false;
		m_nDataSize = 0;
	}
	template <class T>
	void Palette<T>::copy(T *pData, unsigned int nSize)
	{
		if(nSize == 0 || m_pData == pData) //size 0 to copy, or data is the same reference
			return;
		if(nSize > m_nDataSize || m_pData == 0)
			allocate(nSize);
		for(unsigned int i = 0; i < m_nDataSize; i++)
			m_pData[i] = pData[i];
	}
	template <class T>
	void Palette<T>::copyPointer(T *pData, unsigned int nSize)
	{
		if(m_pData == pData) //in case its referencing the same data
			return;
		free();
		m_pData = pData;
		m_nDataSize = nSize;
	}
	template <class T>
	Palette<T>& Palette<T>::operator =(const gfx::Palette<T> &rhs)
	{
		if(m_pData == rhs.m_pData) //in case its referencing the same data
			return *this;
		copy(rhs.m_pData, rhs.m_nDataSize);
		return *this;
	}
	template <class T>
	T& Palette<T>::operator [](unsigned int entry)
	{
		return m_pData[entry];
	}
	template <class T>
	T* Palette<T>::getPointer()
	{
		return m_pData;
	}
	template <class T>
	unsigned int Palette<T>::size()
	{
		return m_nDataSize;
	}
}