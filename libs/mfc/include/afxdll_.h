// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// afxdll_.h - extensions to AFXWIN.H used for the 'AFXDLL' version
// This file contains MFC library implementation details as well
//   as APIs for writing MFC Extension DLLs.
// Please refer to Technical Note 033 (TN033) for more details.

/////////////////////////////////////////////////////////////////////////////

#ifndef _AFXDLL
	#error file must be compiled with _AFXDLL
#endif

#ifdef _AFX_PACKING
#pragma pack(push, _AFX_PACKING)
#endif

#undef AFX_DATA
#define AFX_DATA AFX_CORE_DATA

/////////////////////////////////////////////////////////////////////////////

// AFX_EXTENSION_MODULE - special struct used during DLL initialization

struct AFX_EXTENSION_MODULE
{
	BOOL bInitialized;
	HMODULE hModule;
	HMODULE hResource;
	CRuntimeClass* pFirstSharedClass;
	COleObjectFactory* pFirstSharedFactory;
};

/////////////////////////////////////////////////////////////////////////////
// CDynLinkLibrary - for implementation of MFC Extension DLLs

class COleObjectFactory;

class CDynLinkLibrary : public CCmdTarget
{
	DECLARE_DYNAMIC(CDynLinkLibrary)
public:

// Constructor
	CDynLinkLibrary(AFX_EXTENSION_MODULE& state, BOOL bSystem = FALSE);
	CDynLinkLibrary(HINSTANCE hModule, HINSTANCE hResource);

// Attributes
	HMODULE m_hModule;
	HMODULE m_hResource;                // for shared resources
	CTypedSimpleList<CRuntimeClass*> m_classList;
#ifndef _AFX_NO_OLE_SUPPORT
	CTypedSimpleList<COleObjectFactory*> m_factoryList;
#endif
	BOOL m_bSystem;                     // TRUE only for MFC DLLs

// Implementation
public:
	CDynLinkLibrary* m_pNextDLL;        // simple singly linked list
	virtual ~CDynLinkLibrary();

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif //_DEBUG
};

// call in every DLL_PROCESS_ATTACH
BOOL AFXAPI AfxInitExtensionModule(AFX_EXTENSION_MODULE&, HMODULE hMod);
// call on every DLL_PROCESS_DETACH
void AFXAPI AfxTermExtensionModule(AFX_EXTENSION_MODULE&, BOOL bAll = FALSE);

// special function(s) for stand-alone DLLs (and controls)
void AFXAPI AfxCoreInitModule();
#if defined(_DEBUG) && !defined(_AFX_MONOLITHIC)
void AFXAPI AfxOleInitModule();
void AFXAPI AfxNetInitModule();
void AFXAPI AfxDbInitModule();
#else
#define AfxOleInitModule()
#define AfxNetInitModule()
#define AfxDbInitModule()
#endif

// special functions for loading and freeing MFC extension DLLs
// (necessary if your app is multithreaded and loads extension
//  DLLs dynamically)
HINSTANCE AFXAPI AfxLoadLibrary(LPCTSTR lpszModuleName);
BOOL AFXAPI AfxFreeLibrary(HINSTANCE hInstLib);

#undef AFX_DATA
#define AFX_DATA

#ifdef _AFX_PACKING
#pragma pack(pop)
#endif

/////////////////////////////////////////////////////////////////////////////
