//DefPtr.h
//by Jay Tennant 12/27/10
//provides a pointer to a default member when the pointer is invalid

#ifndef DEFPTR_H
#define DEFPTR_H

template <class T>
struct DefPtr
{
private:
	T m_default;
	T* m_ptr;
public:
	T* operator->()
	{
		if(m_ptr == NULL)
			return &m_default;
		return m_ptr;
	}
	T* operator=(T* rhs)
	{
		m_ptr = rhs;
		return m_ptr;
	}
	bool operator==(const T* rhs)
	{
		return (m_ptr == rhs);
	}
	bool operator!=(const T* rhs)
	{
		return (m_ptr != rhs);
	}
	T** operator&()
	{
		return &m_ptr;
	}
	operator T* ()
	{
		if(m_ptr == NULL)
			return &m_default;
		return m_ptr;
	}
};

#endif