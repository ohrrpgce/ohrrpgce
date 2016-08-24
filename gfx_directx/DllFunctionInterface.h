//DllFunctionInterface.h
//by Jay Tennant 12/9/10
//macros for dll function exporting, function importing, and class generation for function importing
//
//directives that the programmer may define:
//DFI_IMPORT	: defines the dll functions to be imported, instead of exported
//DFI_CLASS		: defines the import declarations to use the class setup, instead of global function pointers; this must be declared with DFI_IMPORT
//DFI_UNIQUE	: defines the declarations to not be declared with 'extern' for external linkage
//
//functions the programmer usually uses
//DFI_DECLARE( returnType, callMethod, function, paramlist... )		: declares functions with a specified calling convention
//DFI_DECLARE_CDECL( returnType, function, paramlist... )			: declares cdecl functions
//DFI_DECLARE_STDCALL( returnType, function, paramlist... )			: declares stdcall functions
//DFI_IMPLEMENT( returnType, callMethod, function, paramlist... )	: implements functions with specified calling convention
//DFI_IMPLEMENT_CDECL( returnType, function, paramlist... )			: implements cdecl functions
//DFI_IMPLEMENT_STDCALL( returnType, function, paramlist... )		: implements stdcall functions
//
//functions the programmer uses with function pointers
//DFI_LOAD_LIBRARY( dllName )	: loads a dll library
//DFI_FREE_LIBRARY( hDll )		: frees the dll library
//DFI_LOAD_FUNCTION( hDll, function )	: loads a function from a dll
//
//functions the programmer uses with classes
//DFI_CLASS_BEGIN( cls ) : begins the class 'cls' definition, which wraps the managment of the dll library; this precedes all calls to DFI_DECLARE*()
//DFI_CLASS_END( cls ) : ends the class 'cls' definition; this succeeds all calls to DFI_DECLARE*()
//
//in the dll export function definition header, if STDCALL is used, the following directive should be included, in lieu of name decoration:
//#pragma comment(linker, "/export:FunctionName=_FunctionName@N")
//where 'FunctionName' is the desired function name, and '_FunctionName@N' is the decorated name (of which 'N' is the number of bytes of the parameters)
//
//
//HOW TO USE: A simple example on how to use DFI in a dll and application using both the function pointer method and class function pointer method.
//
//I. DFI in the DLL
//	1.	The dll should include the DFI header before use so that it's functions can be declared.
//	2.	A code block should be created for any declared functions that use a calling convention other than cdecl.
//		This code block will instruct the linker how to name the declared functions in the dll to avoid name decoration problems.
//	3.	The functions can be declared with DFI_DECLARE(), DFI_DECLARE_CDECL(), or DFI_DECLARE_STDCALL(). Do not declare any with DFI_IMPLEMENT*().
//	4.	In the dll module code, to implement the exported functions, use DFI_IMPLEMENT(), DFI_IMPLEMENT_CDECL(), or DFI_IMPLEMENT_STDCALL().
//		The choice of DFI_IMPLEMENT*() should match the choice of DFI_DECLARE*(). So if declared with DFI_DECLARE(), DFI_IMPLEMENT() would be used to implement.
//		If declared with DFI_DECLARE_CDECL(), DFI_IMPLEMENT_CDECL() is used. Etc.
//
//	An Example DLL header of exported functions: "myDllFunctions.h"
//
//	/*1*/
//	#include "DllFunctionInterface.h"
//
//	/*2*/
//	#ifndef DFI_IMPORT
//	#pragma comment(linker, "/export:AddNumbers=_AddNumbers@8)
//	#pragma comment(linker, "/export:GenerateNumber=_GenerateNumber@0)
//	#endif
//
//	/*3*/
//	DFI_DECLARE_STDCALL(int, AddNumbers, int a, int b);
//	DFI_DECLARE_STDCALL(int, GenerateNumber);
//
//
//	An Example DLL source code defining the functions: "myDllFunctions.cpp"
//
//	#include "myDllFunctions.h"
//
//	/*4*/
//	DFI_IMPLEMENT_STDCALL(int, AddNumbers, int a, int b)
//	{
//		return a + b;
//	}
//	DFI_IMPLEMENT_STDCALL(int, GenerateNumber)
//	{
//		return 4; //ok, that's not very cool
//	}
//
//
//II. DFI in the Application--function pointer style
//	1.	The app should define DFI_IMPORT before including the dll header.
//		If this is the only place the dll header is included, DFI_UNIQUE should also be declared.
//		Otherwise, the dll header will use the "extern" identity.
//	2.	DFI_LOAD_LIBRARY() should be called to load the dll library, then should be assigned to a HMODULE.
//	3.	DFI_LOAD_FUNCTION() should be used to load the function from the library, then should be assigned to the function pointer.
//	4.	When the app is finished, DFI_FREE_LIBRARY() must be called on the dll handle to release the library.
//
//	An Example app: "myApp.cpp"
//
//	#include <windows.h>
//	#include <iostream>
//	using namespace std;
//
//	/*1*/
//	#define DFI_IMPORT
//	#define DFI_UNIQUE
//	#include "myDllFunctions.h"
//
//	int main()
//	{
//		/*2*/
//		HMODULE hDll = DFI_LOAD_LIBRARY( TEXT("myDll.dll") );
//		if( !hDll )
//		{
//			cout << "Dll Failed to load!" << endl;
//			system("pause");
//			return 1;
//		}
//
//		/*3*/
//		AddNumbers = DFI_LOAD_FUNCTION( hDll, AddNumbers );
//		if( !AddNumbers )
//		{
//			cout << "AddNumbers() failed to load!" << endl;
//			system("pause");
//			DFI_FREE_LIBRARY( hDll );
//			return 0;
//		}
//		GenerateNumber = DFI_LOAD_FUNCTION( hDll, GenerateNumber );
//		if( !GenerateNumber )
//		{
//			cout << "GenerateNumber() failed to load!" << endl;
//			system("pause");
//			DFI_FREE_LIBRARY( hDll );
//			return 0;
//		}
//
//		cout << "2 + 3 = " << AddNumbers( 2, 3 ) << "! :D" << endl;
//		cout << "The secret number is... " << GenerateNumber() << "! D:" << endl;
//
//		/*4*/
//		DFI_FREE_LIBRARY( hDll );
//		return 0;
//	}
//
//
//III. DFI in the Application--class function pointer style
//	1.	The app must define DFI_IMPORT and DFI_CLASS before including the DFI header.
//		NOTE: The DFI header must be included before including the dll header, even if the dll header includes it already.
//		If this is the only place the dll header is included, DFI_UNIQUE should also be declared.
//		Otherwise, the dll header will use the "extern" identity.
//	2.	After the DFI header is included, but before the dll header, call DFI_CLASS_BEGIN(), supplying a class name.
//		DO NOT call DFI_CLASS_END() here.
//	3.	Include the dll header.
//	4.	After the dll header, call DFI_CLASS_END(), supplying the class name.
//	5.	Instantiate the class, supplying the dll filename.
//	6.	Call the class's Init_*() function before each function can be used.
//	7.	Use the class's function pointers.
//		NOTE: In this setup, DFI_LOAD_LIBRARY(), DFI_LOAD_FUNCTION(), and DFI_FREE_LIBRARY() do not have to be called,
//		as those are maintained by the class with a static reference count to the number of class instances. Once this
//		reference count drops to 0, the library is automatically unloaded.
//
//	An Example app: "myClassApp.cpp"
//
//	#include <windows.h>
//	#include <iostream>
//	using namespace std;
//
//	/*1*/
//	#define DFI_IMPORT
//	#define DFI_CLASS
//	#define DFI_UNIQUE
//
//	#include "DllFunctionInterface.h"
//
//	/*2*/
//	DFI_CLASS_BEGIN( MyDllClass );
//
//	/*3*/
//	#include "myDllFunctions.h"
//
//	/*4*/
//	DFI_CLASS_END( MyDllClass );
//
//	int main()
//	{
//		/*5*/
//		MyDllClass cls(TEXT("my.dll");
//
//		/*6*/
//		if(!cls.Init_AddNumbers())
//		{
//			cout << "AddNumbers() failed to load!" << endl;
//			system("pause");
//			return 1;
//		}
//		if(!cls.Init_GenerateNumber())
//		{
//			cout << "GenerateNumber() failed to load!" << endl;
//			system("pause");
//			return 1;
//		}
//
//		/*7*/
//		cout << "2 + 3 = " << cls.AddNumbers( 2, 3 ) << "! :D" << endl;
//		cout << "The secret number is... " << cls.GenerateNumber() << "! D:" << endl;
//
//		return 0;
//	}
//
//
#pragma once

#define _DFI_MASHER(a, b) a##b
#define _DFI_STRINGER(arg) #arg

//defining call types
#define _DFI_STDCALL __stdcall
#define _DFI_CDECL __cdecl

//defining function types
#define _DFI_FNPTR_TYPE_NAME_GEN( function ) _DFI_MASHER(FNPTR_TYPE_, function)
#define _DFI_FNPTR_TYPE_DECLARE( returnType, callMethod, function, ... ) \
	typedef returnType (callMethod *_DFI_FNPTR_TYPE_NAME_GEN(function))(__VA_ARGS__)

//exporting functions
#define _DFI_EXPORT_DECLARE( returnType, callMethod, function, ... ) \
	extern "C" __declspec(dllexport) returnType callMethod function(__VA_ARGS__)

//loading functions
#define DFI_LOAD_LIBRARY( dllName ) \
	LoadLibrary(dllName)
#define DFI_FREE_LIBRARY( hDll ) \
	FreeLibrary(hDll)
#define DFI_LOAD_FUNCTION( hDll, function ) \
	(_DFI_FNPTR_TYPE_NAME_GEN(function))GetProcAddress(hDll, _DFI_STRINGER(function))


//global function importing
#define _DFI_IMPORT_FNPTR_DECLARE_UNIQUE( returnType, callMethod, function, ... ) \
	_DFI_FNPTR_TYPE_DECLARE(returnType, callMethod, function ,##__VA_ARGS__); \
	_DFI_FNPTR_TYPE_NAME_GEN(function) function
#define _DFI_IMPORT_FNPTR_DECLARE_EXTERN( returnType, callMethod, function, ... ) \
	_DFI_FNPTR_TYPE_DECLARE(returnType, callMethod, function ,##__VA_ARGS__); \
	extern _DFI_FNPTR_TYPE_NAME_GEN(function) function

#ifdef DFI_UNIQUE
#define _DFI_IMPORT_FNPTR_DECLARE( returnType, callMethod, function, ... ) \
	_DFI_IMPORT_FNPTR_DECLARE_UNIQUE(returnType, callMethod, function ,##__VA_ARGS__)
#else
#define _DFI_IMPORT_FNPTR_DECLARE( returnType, callMethod, function, ... ) \
	_DFI_IMPORT_FNPTR_DECLARE_EXTERN(returnType, callMethod, function ,##__VA_ARGS__)
#endif //DFI_UNIQUE


//class function importing
#define _DFI_IMPORT_CLASS_DLL_HANDLE m_hDll
#define _DFI_IMPORT_CLASS_FNPTR_NAME_GEN(function) _DFI_MASHER(m_lpfn_, function)
#define _DFI_IMPORT_CLASS_SHELL_BEGIN( cls ) \
	class cls \
	{ \
	private: \
		static HMODULE _DFI_IMPORT_CLASS_DLL_HANDLE; \
		static INT m_refCount; \
		static void AddRef(const TCHAR *dllName); \
		static void Release(); \
	public: \
		cls(const TCHAR *dllname); \
		virtual ~cls()

#define _DFI_IMPORT_CLASS_SHELL_END_UNIQUE( cls ) \
	}; \
	void cls::AddRef(const TCHAR *dllName) \
	{ \
		++m_refCount; \
		if(m_refCount == 1) \
			_DFI_IMPORT_CLASS_DLL_HANDLE = DFI_LOAD_LIBRARY(dllName); \
	} \
	void cls::Release() \
	{ \
		--m_refCount; \
		if(m_refCount <= 0) \
		{ \
			m_refCount = 0; \
			DFI_FREE_LIBRARY( _DFI_IMPORT_CLASS_DLL_HANDLE ); \
			_DFI_IMPORT_CLASS_DLL_HANDLE = NULL; \
		} \
	} \
	cls::cls(const TCHAR *dllName) \
	{ \
		ZeroMemory(this, sizeof(cls)); \
		AddRef(dllName); \
	} \
	cls::~cls() \
	{ \
		Release(); \
	} \
	HMODULE cls::_DFI_IMPORT_CLASS_DLL_HANDLE = NULL; \
	INT cls::m_refCount = 0

#define _DFI_IMPORT_CLASS_SHELL_END_EXTERN() }

#ifdef DFI_UNIQUE
#define _DFI_IMPORT_CLASS_SHELL_END( cls ) \
	_DFI_IMPORT_CLASS_SHELL_END_UNIQUE(cls)
#else
#define _DFI_IMPORT_CLASS_SHELL_END( cls ) \
	_DFI_IMPORT_CLASS_SHELL_END_EXTERN()
#endif //DFI_UNIQUE

#define _DFI_IMPORT_CLASS_DECLARE_UNIQUE( returnType, callMethod, function, ... ) \
	public: \
	_DFI_FNPTR_TYPE_DECLARE(returnType, callMethod, function ,##__VA_ARGS__); \
	BOOL _DFI_MASHER(Init_, function)() \
	{ \
		function = DFI_LOAD_FUNCTION(_DFI_IMPORT_CLASS_DLL_HANDLE, function); \
		return (function != NULL ? TRUE : FALSE); \
	} \
	_DFI_FNPTR_TYPE_NAME_GEN(function) function

#define _DFI_IMPORT_CLASS_DECLARE_EXTERN( returnType, callMethod, function, ... ) \
	_DFI_IMPORT_CLASS_DECLARE_UNIQUE(returnType, callMethod, function ,##__VA_ARGS__)

#ifdef DFI_UNIQUE
#define _DFI_IMPORT_CLASS_DECLARE( returnType, callMethod, function, ... ) \
	_DFI_IMPORT_CLASS_DECLARE_UNIQUE(returnType, callMethod, function ,##__VA_ARGS__)
#else
#define _DFI_IMPORT_CLASS_DECLARE( returnType, callMethod, function, ... ) \
	_DFI_IMPORT_CLASS_DECLARE_EXTERN(returnType, callMethod, function ,##__VA_ARGS__)
#endif //DFI_UNIQUE


//abstracting the global function pts's vs. class ptr's
#ifdef DFI_CLASS

#define _DFI_IMPORT_DECLARE_UNIQUE( returnType, callMethod, function, ... ) \
	_DFI_IMPORT_CLASS_DECLARE_UNIQUE(returnType, callMethod, function ,##__VA_ARGS__)
#define _DFI_IMPORT_DECLARE_EXTERN( returnType, callMethod, function, ... ) \
	_DFI_IMPORT_CLASS_DECLARE_EXTERN(returnType, callMethod, function ,##__VA_ARGS__)

#else // !DFI_CLASS

#define _DFI_IMPORT_DECLARE_UNIQUE( returnType, callMethod, function, ... ) \
	_DFI_IMPORT_FNPTR_DECLARE_UNIQUE(returnType, callMethod, function ,##__VA_ARGS__)
#define _DFI_IMPORT_DECLARE_EXTERN( returnType, callMethod, function, ... ) \
	_DFI_IMPORT_FNPTR_DECLARE_EXTERN(returnType, callMethod, function ,##__VA_ARGS__)

#endif //DFI_CLASS

#ifdef DFI_UNIQUE
#define _DFI_IMPORT_DECLARE( returnType, callMethod, function, ... ) \
	_DFI_IMPORT_DECLARE_UNIQUE( returnType, callMethod, function ,##__VA_ARGS__)
#else
#define _DFI_IMPORT_DECLARE( returnType, callMethod, function, ... ) \
	_DFI_IMPORT_DECLARE_EXTERN( returnType, callMethod, function ,##__VA_ARGS__)
#endif //DFI_UNIQUE


//abstracting whether importing or exporting
#ifdef DFI_IMPORT
#define _DFI_DECLARE( returnType, callMethod, function, ... ) \
	_DFI_IMPORT_DECLARE(returnType, callMethod, function ,##__VA_ARGS__)
#else
#define _DFI_DECLARE( returnType, callMethod, function, ... ) \
	_DFI_EXPORT_DECLARE( returnType, callMethod, function ,##__VA_ARGS__)
#endif


//programmer interfaces
#define DFI_DECLARE( returnType, callMethod, function, ... ) \
	_DFI_DECLARE(returnType, callMethod, function ,##__VA_ARGS__)
#define DFI_DECLARE_CDECL( returnType, function, ... ) \
	DFI_DECLARE(returnType, _DFI_CDECL, function ,##__VA_ARGS__)
#define DFI_DECLARE_STDCALL( returnType, function, ... ) \
	DFI_DECLARE(returnType, _DFI_STDCALL, function ,##__VA_ARGS__)

#define DFI_IMPLEMENT( returnType, callMethod, function, ... ) \
	returnType callMethod function(__VA_ARGS__)
#define DFI_IMPLEMENT_CDECL( returnType, function, ... ) \
	DFI_IMPLEMENT(returnType, _DFI_CDECL, function ,##__VA_ARGS__)
#define DFI_IMPLEMENT_STDCALL( returnType, function, ... ) \
	DFI_IMPLEMENT(returnType, _DFI_STDCALL, function ,##__VA_ARGS__)

#ifdef DFI_IMPORT

#ifdef DFI_CLASS
#define DFI_CLASS_BEGIN( cls ) \
	_DFI_IMPORT_CLASS_SHELL_BEGIN(cls)
#define DFI_CLASS_END( cls ) \
	_DFI_IMPORT_CLASS_SHELL_END(cls)
#else
#define DFI_CLASS_BEGIN( cls )
#define DFI_CLASS_END( cls )
#endif //DFI_CLASS

#else

#define DFI_CLASS_BEGIN( cls )
#define DFI_CLASS_END( cls )

#endif //DFI_IMPORT
