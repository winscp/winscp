// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#ifndef __AFXDISP_H__
#define __AFXDISP_H__

#ifdef _AFX_NO_OLE_SUPPORT
	#error OLE classes not supported in this library variant.
#endif

#ifndef __AFXWIN_H__
	#include <afxwin.h>
#endif

// include necessary OLE headers
#ifndef _OBJBASE_H_
	#include <objbase.h>
#endif
#ifndef _OLEAUTO_H_
	#include <oleauto.h>
#endif
#ifndef _OLECTL_H_
	#include <olectl.h>
#endif
#ifndef __ocidl_h__
	#include <ocidl.h>
#endif

// Shell interface support
#ifndef _SHLOBJ_H_
	#include <shlobj.h>
#endif

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, off)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, on)
#endif

#ifndef _AFX_NOFORCE_LIBS

/////////////////////////////////////////////////////////////////////////////
// Win32 libraries

#ifdef _AFXDLL
	#if defined(_DEBUG) && !defined(_AFX_MONOLITHIC)
		#ifndef _UNICODE
			#pragma comment(lib, "mfco42d.lib")
		#else
			#pragma comment(lib, "mfco42ud.lib")
		#endif
	#endif
#endif

#pragma comment(lib, "oledlg.lib")
#pragma comment(lib, "ole32.lib")
#pragma comment(lib, "olepro32.lib")
#pragma comment(lib, "oleaut32.lib")
#pragma comment(lib, "uuid.lib")
#pragma comment(lib, "urlmon.lib")

#endif //!_AFX_NOFORCE_LIBS

/////////////////////////////////////////////////////////////////////////////

#ifdef _AFX_PACKING
#pragma pack(push, _AFX_PACKING)
#endif

/////////////////////////////////////////////////////////////////////////////
// AFXDISP - MFC IDispatch & ClassFactory support

// Classes declared in this file

//CException
	class COleException;            // caught by client or server
	class COleDispatchException;    // special exception for IDispatch calls

//CCmdTarget
	class COleObjectFactory;        // glue for IClassFactory -> runtime class
		class COleTemplateServer;   // server documents using CDocTemplate

class COleDispatchDriver;           // helper class to call IDispatch

class COleVariant;          // OLE VARIANT wrapper
class COleCurrency;         // Based on OLE CY
class COleDateTime;         // Based on OLE DATE
class COleDateTimeSpan;     // Based on a double
class COleSafeArray;        // Based on OLE VARIANT
/////////////////////////////////////////////////////////////////////////////

// AFXDLL support
#undef AFX_DATA
#define AFX_DATA AFX_OLE_DATA

/////////////////////////////////////////////////////////////////////////////
// OLE COM (Component Object Model) implementation infrastructure
//      - data driven QueryInterface
//      - standard implementation of aggregate AddRef and Release
// (see CCmdTarget in AFXWIN.H for more information)

#define METHOD_PROLOGUE(theClass, localClass) \
	theClass* pThis = \
		((theClass*)((BYTE*)this - offsetof(theClass, m_x##localClass))); \
	AFX_MANAGE_STATE(pThis->m_pModuleState) \
	pThis; // avoid warning from compiler \

#define METHOD_PROLOGUE_(theClass, localClass) \
	theClass* pThis = \
		((theClass*)((BYTE*)this - offsetof(theClass, m_x##localClass))); \
	pThis; // avoid warning from compiler \

#ifndef _AFX_NO_NESTED_DERIVATION
#define METHOD_PROLOGUE_EX(theClass, localClass) \
	theClass* pThis = ((theClass*)((BYTE*)this - m_nOffset)); \
	AFX_MANAGE_STATE(pThis->m_pModuleState) \
	pThis; // avoid warning from compiler \

#define METHOD_PROLOGUE_EX_(theClass, localClass) \
	theClass* pThis = ((theClass*)((BYTE*)this - m_nOffset)); \
	pThis; // avoid warning from compiler \

#else
#define METHOD_PROLOGUE_EX(theClass, localClass) \
	METHOD_PROLOGUE(theClass, localClass) \

#define METHOD_PROLOGUE_EX_(theClass, localClass) \
	METHOD_PROLOGUE_(theClass, localClass) \

#endif

// Provided only for compatibility with CDK 1.x
#define METHOD_MANAGE_STATE(theClass, localClass) \
	METHOD_PROLOGUE_EX(theClass, localClass) \

#define BEGIN_INTERFACE_PART(localClass, baseClass) \
	class X##localClass : public baseClass \
	{ \
	public: \
		STDMETHOD_(ULONG, AddRef)(); \
		STDMETHOD_(ULONG, Release)(); \
		STDMETHOD(QueryInterface)(REFIID iid, LPVOID* ppvObj); \

#ifndef _AFX_NO_NESTED_DERIVATION
#define BEGIN_INTERFACE_PART_DERIVE(localClass, baseClass) \
	class X##localClass : public baseClass \
	{ \
	public: \

#else
#define BEGIN_INTERFACE_PART_DERIVE(localClass, baseClass) \
	BEGIN_INTERFACE_PART(localClass, baseClass) \

#endif

#ifndef _AFX_NO_NESTED_DERIVATION
#define INIT_INTERFACE_PART(theClass, localClass) \
		size_t m_nOffset; \
		INIT_INTERFACE_PART_DERIVE(theClass, localClass) \

#define INIT_INTERFACE_PART_DERIVE(theClass, localClass) \
		X##localClass() \
			{ m_nOffset = offsetof(theClass, m_x##localClass); } \

#else
#define INIT_INTERFACE_PART(theClass, localClass)
#define INIT_INTERFACE_PART_DERIVE(theClass, localClass)

#endif

// Note: Inserts the rest of OLE functionality between these two macros,
//  depending upon the interface that is being implemented.  It is not
//  necessary to include AddRef, Release, and QueryInterface since those
//  member functions are declared by the macro.

#define END_INTERFACE_PART(localClass) \
	} m_x##localClass; \
	friend class X##localClass; \

struct CInterfacePlaceHolder
{
	DWORD m_vtbl;   // filled in with USE_INTERFACE_PART
	CInterfacePlaceHolder() { m_vtbl = 0; }
};

#define END_INTERFACE_PART_OPTIONAL(localClass) \
	}; \
	CInterfacePlaceHolder m_x##localClass; \
	friend class X##localClass; \

#ifdef _AFXDLL
#define END_INTERFACE_PART_STATIC END_INTERFACE_PART
#else
#define END_INTERFACE_PART_STATIC END_INTERFACE_PART
#endif

#define USE_INTERFACE_PART(localClass) \
	m_x##localClass.m_vtbl = *(DWORD*)&X##localClass(); \

#ifdef _AFXDLL
#define BEGIN_INTERFACE_MAP(theClass, theBase) \
	const AFX_INTERFACEMAP* PASCAL theClass::_GetBaseInterfaceMap() \
		{ return &theBase::interfaceMap; } \
	const AFX_INTERFACEMAP* theClass::GetInterfaceMap() const \
		{ return &theClass::interfaceMap; } \
	AFX_COMDAT const AFX_DATADEF AFX_INTERFACEMAP theClass::interfaceMap = \
		{ &theClass::_GetBaseInterfaceMap, &theClass::_interfaceEntries[0], }; \
	AFX_COMDAT const AFX_DATADEF AFX_INTERFACEMAP_ENTRY theClass::_interfaceEntries[] = \
	{ \

#else
#define BEGIN_INTERFACE_MAP(theClass, theBase) \
	const AFX_INTERFACEMAP* theClass::GetInterfaceMap() const \
		{ return &theClass::interfaceMap; } \
	AFX_COMDAT const AFX_DATADEF AFX_INTERFACEMAP theClass::interfaceMap = \
		{ &theBase::interfaceMap, &theClass::_interfaceEntries[0], }; \
	AFX_COMDAT const AFX_DATADEF AFX_INTERFACEMAP_ENTRY theClass::_interfaceEntries[] = \
	{ \

#endif

#define INTERFACE_PART(theClass, iid, localClass) \
		{ &iid, offsetof(theClass, m_x##localClass) }, \

#define INTERFACE_AGGREGATE(theClass, theAggr) \
		{ NULL, offsetof(theClass, theAggr) }, \

#define END_INTERFACE_MAP() \
		{ NULL, (size_t)-1 } \
	}; \

/////////////////////////////////////////////////////////////////////////////
// COleException - unexpected or rare OLE error returned

class COleException : public CException
{
	DECLARE_DYNAMIC(COleException)

public:
	SCODE m_sc;
	static SCODE PASCAL Process(const CException* pAnyException);

// Implementation (use AfxThrowOleException to create)
public:
	COleException();
	virtual ~COleException();

	virtual BOOL GetErrorMessage(LPTSTR lpszError, UINT nMaxError,
		PUINT pnHelpContext = NULL);
};

void AFXAPI AfxThrowOleException(SCODE sc);
void AFXAPI AfxCheckError(SCODE sc);

/////////////////////////////////////////////////////////////////////////////
// IDispatch specific exception

class COleDispatchException : public CException
{
	DECLARE_DYNAMIC(COleDispatchException)

public:
// Attributes
	WORD m_wCode;   // error code (specific to IDispatch implementation)
	CString m_strDescription;   // human readable description of the error
	DWORD m_dwHelpContext;      // help context for error

	// usually empty in application which creates it (eg. servers)
	CString m_strHelpFile;      // help file to use with m_dwHelpContext
	CString m_strSource;        // source of the error (name of server)

// Implementation
public:
	COleDispatchException(LPCTSTR lpszDescription, UINT nHelpID, WORD wCode);
	virtual ~COleDispatchException();
	static void PASCAL Process(
		EXCEPINFO* pInfo, const CException* pAnyException);

	virtual BOOL GetErrorMessage(LPTSTR lpszError, UINT nMaxError,
		PUINT pnHelpContext = NULL);

	SCODE m_scError;            // SCODE describing the error
};

void AFXAPI AfxThrowOleDispatchException(WORD wCode, LPCTSTR lpszDescription,
	UINT nHelpID = 0);
void AFXAPI AfxThrowOleDispatchException(WORD wCode, UINT nDescriptionID,
	UINT nHelpID = (UINT)-1);

/////////////////////////////////////////////////////////////////////////////
// Macros for CCmdTarget IDispatchable classes

#ifdef _AFXDLL
#define BEGIN_DISPATCH_MAP(theClass, baseClass) \
	const AFX_DISPMAP* PASCAL theClass::_GetBaseDispatchMap() \
		{ return &baseClass::dispatchMap; } \
	const AFX_DISPMAP* theClass::GetDispatchMap() const \
		{ return &theClass::dispatchMap; } \
	AFX_COMDAT const AFX_DISPMAP theClass::dispatchMap = \
		{ &theClass::_GetBaseDispatchMap, &theClass::_dispatchEntries[0], \
			&theClass::_dispatchEntryCount, &theClass::_dwStockPropMask }; \
	AFX_COMDAT UINT theClass::_dispatchEntryCount = (UINT)-1; \
	AFX_COMDAT DWORD theClass::_dwStockPropMask = (DWORD)-1; \
	AFX_COMDAT const AFX_DISPMAP_ENTRY theClass::_dispatchEntries[] = \
	{ \

#else
#define BEGIN_DISPATCH_MAP(theClass, baseClass) \
	const AFX_DISPMAP* theClass::GetDispatchMap() const \
		{ return &theClass::dispatchMap; } \
	AFX_COMDAT const AFX_DISPMAP theClass::dispatchMap = \
		{ &baseClass::dispatchMap, &theClass::_dispatchEntries[0], \
			&theClass::_dispatchEntryCount, &theClass::_dwStockPropMask }; \
	AFX_COMDAT UINT theClass::_dispatchEntryCount = (UINT)-1; \
	AFX_COMDAT DWORD theClass::_dwStockPropMask = (DWORD)-1; \
	AFX_COMDAT const AFX_DISPMAP_ENTRY theClass::_dispatchEntries[] = \
	{ \

#endif

#define END_DISPATCH_MAP() \
	{ VTS_NONE, DISPID_UNKNOWN, VTS_NONE, VT_VOID, \
		(AFX_PMSG)NULL, (AFX_PMSG)NULL, (size_t)-1, afxDispCustom } }; \

// parameter types: by value VTs
#define VTS_I2              "\x02"      // a 'short'
#define VTS_I4              "\x03"      // a 'long'
#define VTS_R4              "\x04"      // a 'float'
#define VTS_R8              "\x05"      // a 'double'
#define VTS_CY              "\x06"      // a 'CY' or 'CY*'
#define VTS_DATE            "\x07"      // a 'DATE'
#define VTS_WBSTR           "\x08"      // an 'LPCOLESTR'
#define VTS_DISPATCH        "\x09"      // an 'IDispatch*'
#define VTS_SCODE           "\x0A"      // an 'SCODE'
#define VTS_BOOL            "\x0B"      // a 'BOOL'
#define VTS_VARIANT         "\x0C"      // a 'const VARIANT&' or 'VARIANT*'
#define VTS_UNKNOWN         "\x0D"      // an 'IUnknown*'
#if defined(_UNICODE) || defined(OLE2ANSI)
	#define VTS_BSTR            VTS_WBSTR// an 'LPCOLESTR'
	#define VT_BSTRT            VT_BSTR
#else
	#define VTS_BSTR            "\x0E"  // an 'LPCSTR'
	#define VT_BSTRA            14
	#define VT_BSTRT            VT_BSTRA
#endif
#define VTS_UI1             "\x0F"      // a 'BYTE'

// parameter types: by reference VTs
#define VTS_PI2             "\x42"      // a 'short*'
#define VTS_PI4             "\x43"      // a 'long*'
#define VTS_PR4             "\x44"      // a 'float*'
#define VTS_PR8             "\x45"      // a 'double*'
#define VTS_PCY             "\x46"      // a 'CY*'
#define VTS_PDATE           "\x47"      // a 'DATE*'
#define VTS_PBSTR           "\x48"      // a 'BSTR*'
#define VTS_PDISPATCH       "\x49"      // an 'IDispatch**'
#define VTS_PSCODE          "\x4A"      // an 'SCODE*'
#define VTS_PBOOL           "\x4B"      // a 'VARIANT_BOOL*'
#define VTS_PVARIANT        "\x4C"      // a 'VARIANT*'
#define VTS_PUNKNOWN        "\x4D"      // an 'IUnknown**'
#define VTS_PUI1            "\x4F"      // a 'BYTE*'

// special VT_ and VTS_ values
#define VTS_NONE            NULL        // used for members with 0 params
#define VT_MFCVALUE         0xFFF       // special value for DISPID_VALUE
#define VT_MFCBYREF         0x40        // indicates VT_BYREF type
#define VT_MFCMARKER        0xFF        // delimits named parameters (INTERNAL USE)

// variant handling (use V_BSTRT when you have ANSI BSTRs, as in DAO)
#ifndef _UNICODE
	#define V_BSTRT(b)  (LPSTR)V_BSTR(b)
#else
	#define V_BSTRT(b)  V_BSTR(b)
#endif

/////////////////////////////////////////////////////////////////////////////
// OLE control parameter types

#define VTS_COLOR           VTS_I4      // OLE_COLOR
#define VTS_XPOS_PIXELS     VTS_I4      // OLE_XPOS_PIXELS
#define VTS_YPOS_PIXELS     VTS_I4      // OLE_YPOS_PIXELS
#define VTS_XSIZE_PIXELS    VTS_I4      // OLE_XSIZE_PIXELS
#define VTS_YSIZE_PIXELS    VTS_I4      // OLE_YSIZE_PIXELS
#define VTS_XPOS_HIMETRIC   VTS_I4      // OLE_XPOS_HIMETRIC
#define VTS_YPOS_HIMETRIC   VTS_I4      // OLE_YPOS_HIMETRIC
#define VTS_XSIZE_HIMETRIC  VTS_I4      // OLE_XSIZE_HIMETRIC
#define VTS_YSIZE_HIMETRIC  VTS_I4      // OLE_YSIZE_HIMETRIC
#define VTS_TRISTATE        VTS_I2      // OLE_TRISTATE
#define VTS_OPTEXCLUSIVE    VTS_BOOL    // OLE_OPTEXCLUSIVE

#define VTS_PCOLOR          VTS_PI4     // OLE_COLOR*
#define VTS_PXPOS_PIXELS    VTS_PI4     // OLE_XPOS_PIXELS*
#define VTS_PYPOS_PIXELS    VTS_PI4     // OLE_YPOS_PIXELS*
#define VTS_PXSIZE_PIXELS   VTS_PI4     // OLE_XSIZE_PIXELS*
#define VTS_PYSIZE_PIXELS   VTS_PI4     // OLE_YSIZE_PIXELS*
#define VTS_PXPOS_HIMETRIC  VTS_PI4     // OLE_XPOS_HIMETRIC*
#define VTS_PYPOS_HIMETRIC  VTS_PI4     // OLE_YPOS_HIMETRIC*
#define VTS_PXSIZE_HIMETRIC VTS_PI4     // OLE_XSIZE_HIMETRIC*
#define VTS_PYSIZE_HIMETRIC VTS_PI4     // OLE_YSIZE_HIMETRIC*
#define VTS_PTRISTATE       VTS_PI2     // OLE_TRISTATE*
#define VTS_POPTEXCLUSIVE   VTS_PBOOL   // OLE_OPTEXCLUSIVE*

#define VTS_FONT            VTS_DISPATCH    // IFontDispatch*
#define VTS_PICTURE         VTS_DISPATCH    // IPictureDispatch*

#define VTS_HANDLE          VTS_I4      // OLE_HANDLE
#define VTS_PHANDLE         VTS_PI4     // OLE_HANDLE*

// these DISP_ macros cause the framework to generate the DISPID
#define DISP_FUNCTION(theClass, szExternalName, pfnMember, vtRetVal, vtsParams) \
	{ _T(szExternalName), DISPID_UNKNOWN, vtsParams, vtRetVal, \
		(AFX_PMSG)(void (theClass::*)(void))&pfnMember, (AFX_PMSG)0, 0, \
		afxDispCustom }, \

#define DISP_PROPERTY(theClass, szExternalName, memberName, vtPropType) \
	{ _T(szExternalName), DISPID_UNKNOWN, NULL, vtPropType, (AFX_PMSG)0, (AFX_PMSG)0, \
		offsetof(theClass, memberName), afxDispCustom }, \

#define DISP_PROPERTY_NOTIFY(theClass, szExternalName, memberName, pfnAfterSet, vtPropType) \
	{ _T(szExternalName), DISPID_UNKNOWN, NULL, vtPropType, (AFX_PMSG)0, \
		(AFX_PMSG)(void (theClass::*)(void))&pfnAfterSet, \
		offsetof(theClass, memberName), afxDispCustom }, \

#define DISP_PROPERTY_EX(theClass, szExternalName, pfnGet, pfnSet, vtPropType) \
	{ _T(szExternalName), DISPID_UNKNOWN, NULL, vtPropType, \
		(AFX_PMSG)(void (theClass::*)(void))&pfnGet, \
		(AFX_PMSG)(void (theClass::*)(void))&pfnSet, 0, afxDispCustom }, \

#define DISP_PROPERTY_PARAM(theClass, szExternalName, pfnGet, pfnSet, vtPropType, vtsParams) \
	{ _T(szExternalName), DISPID_UNKNOWN, vtsParams, vtPropType, \
		(AFX_PMSG)(void (theClass::*)(void))&pfnGet, \
		(AFX_PMSG)(void (theClass::*)(void))&pfnSet, 0, afxDispCustom }, \

// these DISP_ macros allow the app to determine the DISPID
#define DISP_FUNCTION_ID(theClass, szExternalName, dispid, pfnMember, vtRetVal, vtsParams) \
	{ _T(szExternalName), dispid, vtsParams, vtRetVal, \
		(AFX_PMSG)(void (theClass::*)(void))&pfnMember, (AFX_PMSG)0, 0, \
		afxDispCustom }, \

#define DISP_PROPERTY_ID(theClass, szExternalName, dispid, memberName, vtPropType) \
	{ _T(szExternalName), dispid, NULL, vtPropType, (AFX_PMSG)0, (AFX_PMSG)0, \
		offsetof(theClass, memberName), afxDispCustom }, \

#define DISP_PROPERTY_NOTIFY_ID(theClass, szExternalName, dispid, memberName, pfnAfterSet, vtPropType) \
	{ _T(szExternalName), dispid, NULL, vtPropType, (AFX_PMSG)0, \
		(AFX_PMSG)(void (theClass::*)(void))&pfnAfterSet, \
		offsetof(theClass, memberName), afxDispCustom }, \

#define DISP_PROPERTY_EX_ID(theClass, szExternalName, dispid, pfnGet, pfnSet, vtPropType) \
	{ _T(szExternalName), dispid, NULL, vtPropType, \
		(AFX_PMSG)(void (theClass::*)(void))&pfnGet, \
		(AFX_PMSG)(void (theClass::*)(void))&pfnSet, 0, afxDispCustom }, \

#define DISP_PROPERTY_PARAM_ID(theClass, szExternalName, dispid, pfnGet, pfnSet, vtPropType, vtsParams) \
	{ _T(szExternalName), dispid, vtsParams, vtPropType, \
		(AFX_PMSG)(void (theClass::*)(void))&pfnGet, \
		(AFX_PMSG)(void (theClass::*)(void))&pfnSet, 0, afxDispCustom }, \

// the DISP_DEFVALUE is a special case macro that creates an alias for DISPID_VALUE
#define DISP_DEFVALUE(theClass, szExternalName) \
	{ _T(szExternalName), DISPID_UNKNOWN, NULL, VT_MFCVALUE, \
		(AFX_PMSG)0, (AFX_PMSG)0, 0, afxDispCustom }, \

#define DISP_DEFVALUE_ID(theClass, dispid) \
	{ NULL, dispid, NULL, VT_MFCVALUE, (AFX_PMSG)0, (AFX_PMSG)0, 0, \
		afxDispCustom }, \

/////////////////////////////////////////////////////////////////////////////
// Macros for creating "creatable" automation classes.

#define DECLARE_OLECREATE(class_name) \
public: \
	static AFX_DATA COleObjectFactory factory; \
	static AFX_DATA const GUID guid; \

#define IMPLEMENT_OLECREATE(class_name, external_name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8) \
	AFX_DATADEF COleObjectFactory class_name::factory(class_name::guid, \
		RUNTIME_CLASS(class_name), FALSE, _T(external_name)); \
	AFX_COMDAT const AFX_DATADEF GUID class_name::guid = \
		{ l, w1, w2, { b1, b2, b3, b4, b5, b6, b7, b8 } }; \

/////////////////////////////////////////////////////////////////////////////
// Helper class for driving IDispatch

class COleDispatchDriver
{
// Constructors
public:
	COleDispatchDriver();
	COleDispatchDriver(LPDISPATCH lpDispatch, BOOL bAutoRelease = TRUE);
	COleDispatchDriver(const COleDispatchDriver& dispatchSrc);

// Attributes
	LPDISPATCH m_lpDispatch;
	BOOL m_bAutoRelease;

// Operations
	BOOL CreateDispatch(REFCLSID clsid, COleException* pError = NULL);
	BOOL CreateDispatch(LPCTSTR lpszProgID, COleException* pError = NULL);

	void AttachDispatch(LPDISPATCH lpDispatch, BOOL bAutoRelease = TRUE);
	LPDISPATCH DetachDispatch();
		// detach and get ownership of m_lpDispatch
	void ReleaseDispatch();

	// helpers for IDispatch::Invoke
	void AFX_CDECL InvokeHelper(DISPID dwDispID, WORD wFlags,
		VARTYPE vtRet, void* pvRet, const BYTE* pbParamInfo, ...);
	void AFX_CDECL SetProperty(DISPID dwDispID, VARTYPE vtProp, ...);
	void GetProperty(DISPID dwDispID, VARTYPE vtProp, void* pvProp) const;

	// special operators
	operator LPDISPATCH();
	const COleDispatchDriver& operator=(const COleDispatchDriver& dispatchSrc);

// Implementation
public:
	~COleDispatchDriver();
	void InvokeHelperV(DISPID dwDispID, WORD wFlags, VARTYPE vtRet,
		void* pvRet, const BYTE* pbParamInfo, va_list argList);
};

/////////////////////////////////////////////////////////////////////////////
// Class Factory implementation (binds OLE class factory -> runtime class)
//  (all specific class factories derive from this class factory)

class COleObjectFactory : public CCmdTarget
{
	DECLARE_DYNAMIC(COleObjectFactory)

// Construction
public:
	COleObjectFactory(REFCLSID clsid, CRuntimeClass* pRuntimeClass,
		BOOL bMultiInstance, LPCTSTR lpszProgID);

// Attributes
	virtual BOOL IsRegistered() const;
	REFCLSID GetClassID() const;

// Operations
	virtual BOOL Register();
	BOOL Unregister();
	void Revoke();
	void UpdateRegistry(LPCTSTR lpszProgID = NULL);
		// default uses m_lpszProgID if not NULL
	BOOL IsLicenseValid();

	static BOOL PASCAL UnregisterAll();
	static BOOL PASCAL RegisterAll();
	static void PASCAL RevokeAll();
	static BOOL PASCAL UpdateRegistryAll(BOOL bRegister = TRUE);

// Overridables
protected:
	virtual CCmdTarget* OnCreateObject();
	virtual BOOL UpdateRegistry(BOOL bRegister);
	virtual BOOL VerifyUserLicense();
	virtual BOOL GetLicenseKey(DWORD dwReserved, BSTR* pbstrKey);
	virtual BOOL VerifyLicenseKey(BSTR bstrKey);

// Implementation
public:
	virtual ~COleObjectFactory();
#ifdef _DEBUG
	void AssertValid() const;
	void Dump(CDumpContext& dc) const;
#endif

public:
	COleObjectFactory* m_pNextFactory;  // list of factories maintained

protected:
	DWORD m_dwRegister;             // registry identifier
	CLSID m_clsid;                  // registered class ID
	CRuntimeClass* m_pRuntimeClass; // runtime class of CCmdTarget derivative
	BOOL m_bMultiInstance;          // multiple instance?
	LPCTSTR m_lpszProgID;           // human readable class ID
	BYTE m_bLicenseChecked;
	BYTE m_bLicenseValid;
	BYTE m_bRegistered;             // is currently registered w/ system
	BYTE m_bOAT;                          // used by COleTemplateServer to
											  // remember application type for unregistry

// Interface Maps
public:
	BEGIN_INTERFACE_PART(ClassFactory, IClassFactory2)
		INIT_INTERFACE_PART(COleObjectFactory, ClassFactory)
		STDMETHOD(CreateInstance)(LPUNKNOWN, REFIID, LPVOID*);
		STDMETHOD(LockServer)(BOOL);
		STDMETHOD(GetLicInfo)(LPLICINFO);
		STDMETHOD(RequestLicKey)(DWORD, BSTR*);
		STDMETHOD(CreateInstanceLic)(LPUNKNOWN, LPUNKNOWN, REFIID, BSTR,
			LPVOID*);
	END_INTERFACE_PART(ClassFactory)

	DECLARE_INTERFACE_MAP()

	friend SCODE AFXAPI AfxDllGetClassObject(REFCLSID, REFIID, LPVOID*);
	friend SCODE STDAPICALLTYPE DllGetClassObject(REFCLSID, REFIID, LPVOID*);
};

// Define COleObjectFactoryEx for compatibility with old CDK
#define COleObjectFactoryEx COleObjectFactory

//////////////////////////////////////////////////////////////////////////////
// COleTemplateServer - COleObjectFactory using CDocTemplates

// This enumeration is used in AfxOleRegisterServerClass to pick the
//  correct registration entries given the application type.
enum OLE_APPTYPE
{
	OAT_UNKNOWN = -1,                // no type decided yet
	OAT_INPLACE_SERVER = 0,     // server has full server user-interface
	OAT_SERVER = 1,             // server supports only embedding
	OAT_CONTAINER = 2,          // container supports links to embeddings
	OAT_DISPATCH_OBJECT = 3,    // IDispatch capable object
	OAT_DOC_OBJECT_SERVER = 4,  // sever supports DocObject embedding
	OAT_DOC_OBJECT_CONTAINER =5,// container supports DocObject clients
};

class COleTemplateServer : public COleObjectFactory
{
// Constructors
public:
	COleTemplateServer();

// Operations
	void ConnectTemplate(REFCLSID clsid, CDocTemplate* pDocTemplate,
		BOOL bMultiInstance);
		// set doc template after creating it in InitInstance
	void UpdateRegistry(OLE_APPTYPE nAppType = OAT_INPLACE_SERVER,
		LPCTSTR* rglpszRegister = NULL, LPCTSTR* rglpszOverwrite = NULL);
		// may want to UpdateRegistry if not run with /Embedded
	BOOL Register();
	BOOL Unregister();

// Implementation
protected:
	virtual BOOL OnCmdMsg(UINT nID, int nCode, void* pExtra,
			AFX_CMDHANDLERINFO* pHandlerInfo);
	virtual CCmdTarget* OnCreateObject();
	CDocTemplate* m_pDocTemplate;

private:
	void UpdateRegistry(LPCTSTR lpszProgID);
		// hide base class version of UpdateRegistry
};

/////////////////////////////////////////////////////////////////////////////
// System registry helpers

// Helper to register server in case of no .REG file loaded
BOOL AFXAPI AfxOleRegisterServerClass(
	REFCLSID clsid, LPCTSTR lpszClassName,
	LPCTSTR lpszShortTypeName, LPCTSTR lpszLongTypeName,
	OLE_APPTYPE nAppType = OAT_SERVER,
	LPCTSTR* rglpszRegister = NULL, LPCTSTR* rglpszOverwrite = NULL,
	int nIconIndex = 0, LPCTSTR lpszLocalFilterName = NULL);
BOOL AFXAPI AfxOleRegisterServerClass(
	REFCLSID clsid, LPCTSTR lpszClassName,
	LPCTSTR lpszShortTypeName, LPCTSTR lpszLongTypeName,
	OLE_APPTYPE nAppType,
	LPCTSTR* rglpszRegister, LPCTSTR* rglpszOverwrite,
	int nIconIndex, LPCTSTR lpszLocalFilterName, LPCTSTR lpszLocalFilterExt);

BOOL AFXAPI AfxOleUnregisterServerClass(
	REFCLSID clsid, LPCTSTR lpszClassName, LPCTSTR lpszShortTypeName,
	LPCTSTR lpszLongTypeName, OLE_APPTYPE nAppType = OAT_SERVER,
	LPCTSTR* rglpszRegister = NULL, LPCTSTR* rglpszOverwrite = NULL);

// AfxOleRegisterHelper is a worker function used by
//  AfxOleRegisterServerClass (available for advanced registry work)
BOOL AFXAPI AfxOleRegisterHelper(LPCTSTR const* rglpszRegister,
	LPCTSTR const* rglpszSymbols, int nSymbols, BOOL bReplace,
	HKEY hKeyRoot = ((HKEY)0x80000000)); // HKEY_CLASSES_ROOT

BOOL AFXAPI AfxOleUnregisterHelper(LPCTSTR const* rglpszRegister,
	LPCTSTR const* rglpszSymbols, int nSymbols,
	HKEY hKeyRoot = ((HKEY)0x80000000)); // HKEY_CLASSES_ROOT

/////////////////////////////////////////////////////////////////////////////
// Connection maps

#define BEGIN_CONNECTION_PART(theClass, localClass) \
	class X##localClass : public CConnectionPoint \
	{ \
	public: \
		X##localClass() \
			{ m_nOffset = offsetof(theClass, m_x##localClass); }

#define CONNECTION_IID(iid) \
		REFIID GetIID() { return iid; }

#define END_CONNECTION_PART(localClass) \
	} m_x##localClass; \
	friend class X##localClass;

#ifdef _AFXDLL
#define BEGIN_CONNECTION_MAP(theClass, theBase) \
	const AFX_CONNECTIONMAP* PASCAL theClass::_GetBaseConnectionMap() \
		{ return &theBase::connectionMap; } \
	const AFX_CONNECTIONMAP* theClass::GetConnectionMap() const \
		{ return &theClass::connectionMap; } \
	AFX_COMDAT const AFX_DATADEF AFX_CONNECTIONMAP theClass::connectionMap = \
		{ &theClass::_GetBaseConnectionMap, &theClass::_connectionEntries[0], }; \
	AFX_COMDAT const AFX_DATADEF AFX_CONNECTIONMAP_ENTRY theClass::_connectionEntries[] = \
	{ \

#else
#define BEGIN_CONNECTION_MAP(theClass, theBase) \
	const AFX_CONNECTIONMAP* theClass::GetConnectionMap() const \
		{ return &theClass::connectionMap; } \
	AFX_COMDAT const AFX_DATADEF AFX_CONNECTIONMAP theClass::connectionMap = \
		{ &(theBase::connectionMap), &theClass::_connectionEntries[0], }; \
	AFX_COMDAT const AFX_DATADEF AFX_CONNECTIONMAP_ENTRY theClass::_connectionEntries[] = \
	{ \

#endif

#define CONNECTION_PART(theClass, iid, localClass) \
		{ &iid, offsetof(theClass, m_x##localClass) }, \

#define END_CONNECTION_MAP() \
		{ NULL, (size_t)-1 } \
	}; \

/////////////////////////////////////////////////////////////////////////////
// CConnectionPoint

#ifdef _AFXDLL
class CConnectionPoint : public CCmdTarget
#else
class AFX_NOVTABLE CConnectionPoint : public CCmdTarget
#endif
{
// Constructors
public:
	CConnectionPoint();

// Operations
	POSITION GetStartPosition() const;
	LPUNKNOWN GetNextConnection(POSITION& pos) const;
	const CPtrArray* GetConnections();  // obsolete

// Overridables
	virtual LPCONNECTIONPOINTCONTAINER GetContainer();
	virtual REFIID GetIID() = 0;
	virtual void OnAdvise(BOOL bAdvise);
	virtual int GetMaxConnections();
	virtual LPUNKNOWN QuerySinkInterface(LPUNKNOWN pUnkSink);

// Implementation
	~CConnectionPoint();
	void CreateConnectionArray();
	int GetConnectionCount();

protected:
	size_t m_nOffset;
	LPUNKNOWN m_pUnkFirstConnection;
	CPtrArray* m_pConnections;

// Interface Maps
public:
	BEGIN_INTERFACE_PART(ConnPt, IConnectionPoint)
		INIT_INTERFACE_PART(CConnectionPoint, ConnPt)
		STDMETHOD(GetConnectionInterface)(IID* pIID);
		STDMETHOD(GetConnectionPointContainer)(
			IConnectionPointContainer** ppCPC);
		STDMETHOD(Advise)(LPUNKNOWN pUnkSink, DWORD* pdwCookie);
		STDMETHOD(Unadvise)(DWORD dwCookie);
		STDMETHOD(EnumConnections)(LPENUMCONNECTIONS* ppEnum);
	END_INTERFACE_PART(ConnPt)
};

/////////////////////////////////////////////////////////////////////////////
// EventSink Maps

#ifndef _AFX_NO_OCC_SUPPORT

#ifdef _AFXDLL
#define BEGIN_EVENTSINK_MAP(theClass, baseClass) \
	const AFX_EVENTSINKMAP* PASCAL theClass::_GetBaseEventSinkMap() \
		{ return &baseClass::eventsinkMap; } \
	const AFX_EVENTSINKMAP* theClass::GetEventSinkMap() const \
		{ return &theClass::eventsinkMap; } \
	const AFX_EVENTSINKMAP theClass::eventsinkMap = \
		{ &theClass::_GetBaseEventSinkMap, &theClass::_eventsinkEntries[0], \
			&theClass::_eventsinkEntryCount }; \
	UINT theClass::_eventsinkEntryCount = (UINT)-1; \
	const AFX_EVENTSINKMAP_ENTRY theClass::_eventsinkEntries[] = \
	{ \

#else
#define BEGIN_EVENTSINK_MAP(theClass, baseClass) \
	const AFX_EVENTSINKMAP* theClass::GetEventSinkMap() const \
		{ return &theClass::eventsinkMap; } \
	const AFX_EVENTSINKMAP theClass::eventsinkMap = \
		{ &baseClass::eventsinkMap, &theClass::_eventsinkEntries[0], \
			&theClass::_eventsinkEntryCount }; \
	UINT theClass::_eventsinkEntryCount = (UINT)-1; \
	const AFX_EVENTSINKMAP_ENTRY theClass::_eventsinkEntries[] = \
	{ \

#endif

#define END_EVENTSINK_MAP() \
	{ VTS_NONE, DISPID_UNKNOWN, VTS_NONE, VT_VOID, \
		(AFX_PMSG)NULL, (AFX_PMSG)NULL, (size_t)-1, afxDispCustom, \
		(UINT)-1, 0 } }; \

#define ON_EVENT(theClass, id, dispid, pfnHandler, vtsParams) \
	{ _T(""), dispid, vtsParams, VT_BOOL, \
		(AFX_PMSG)(void (theClass::*)(void))&pfnHandler, (AFX_PMSG)0, 0, \
		afxDispCustom, id, (UINT)-1 }, \

#define ON_EVENT_RANGE(theClass, idFirst, idLast, dispid, pfnHandler, vtsParams) \
	{ _T(""), dispid, vtsParams, VT_BOOL, \
		(AFX_PMSG)(void (theClass::*)(void))&pfnHandler, (AFX_PMSG)0, 0, \
		afxDispCustom, idFirst, idLast }, \

#define ON_PROPNOTIFY(theClass, id, dispid, pfnRequest, pfnChanged) \
	{ _T(""), dispid, VTS_NONE, VT_VOID, \
		(AFX_PMSG)(BOOL (CCmdTarget::*)(BOOL*))&pfnRequest, \
		(AFX_PMSG)(BOOL (CCmdTarget::*)(void))&pfnChanged, \
		1, afxDispCustom, id, (UINT)-1 }, \

#define ON_PROPNOTIFY_RANGE(theClass, idFirst, idLast, dispid, pfnRequest, pfnChanged) \
	{ _T(""), dispid, VTS_NONE, VT_VOID, \
		(AFX_PMSG)(BOOL (CCmdTarget::*)(UINT, BOOL*))&pfnRequest, \
		(AFX_PMSG)(BOOL (CCmdTarget::*)(UINT))&pfnChanged, \
		1, afxDispCustom, idFirst, idLast }, \

#define ON_DSCNOTIFY(theClass, id, pfnNotify) \
	{ _T(""), DISPID_UNKNOWN, VTS_NONE, VT_VOID, \
		(AFX_PMSG)(BOOL (CCmdTarget::*)(DSCSTATE, DSCREASON, BOOL*))&pfnNotify, (AFX_PMSG)0, \
		1, afxDispCustom, id, (UINT)-1 }, \

#define ON_DSCNOTIFY_RANGE(theClass, idFirst, idLast, pfnNotify) \
	{ _T(""), DISPID_UNKNOWN, VTS_NONE, VT_VOID, \
		(AFX_PMSG)(BOOL (CCmdTarget::*)(UINT, DSCSTATE, DSCREASON, BOOL*))&pfnNotify, (AFX_PMSG)0, \
		1, afxDispCustom, idFirst, idLast }, \

#define ON_EVENT_REFLECT(theClass, dispid, pfnHandler, vtsParams) \
	{ _T(""), dispid, vtsParams, VT_BOOL, \
		(AFX_PMSG)(void (theClass::*)(void))&pfnHandler, (AFX_PMSG)0, 0, \
		afxDispCustom, (UINT)-1, (UINT)-1 }, \

#define ON_PROPNOTIFY_REFLECT(theClass, dispid, pfnRequest, pfnChanged) \
	{ _T(""), dispid, VTS_NONE, VT_VOID, \
		(AFX_PMSG)(BOOL (CCmdTarget::*)(BOOL*))&pfnRequest, \
		(AFX_PMSG)(BOOL (CCmdTarget::*)(void))&pfnChanged, \
		1, afxDispCustom, (UINT)-1, (UINT)-1 }, \

#endif // !_AFX_NO_OCC_SUPPORT

/////////////////////////////////////////////////////////////////////////////
// Macros for type library information

CTypeLibCache* AFXAPI AfxGetTypeLibCache(const GUID* pTypeLibID);

#define DECLARE_OLETYPELIB(class_name) \
	protected: \
		virtual UINT GetTypeInfoCount(); \
		virtual HRESULT GetTypeLib(LCID, LPTYPELIB*); \
		virtual CTypeLibCache* GetTypeLibCache(); \

#define IMPLEMENT_OLETYPELIB(class_name, tlid, wVerMajor, wVerMinor) \
	UINT class_name::GetTypeInfoCount() \
		{ return 1; } \
	HRESULT class_name::GetTypeLib(LCID lcid, LPTYPELIB* ppTypeLib) \
		{ return ::LoadRegTypeLib(tlid, wVerMajor, wVerMinor, lcid, ppTypeLib); } \
	CTypeLibCache* class_name::GetTypeLibCache() \
		{ AFX_MANAGE_STATE(m_pModuleState); return AfxGetTypeLibCache(&tlid); } \

/////////////////////////////////////////////////////////////////////////////
// Init & Term helpers

BOOL AFXAPI AfxOleInit();
void AFXAPI AfxOleTerm(BOOL bJustRevoke = FALSE);
void AFXAPI AfxOleTermOrFreeLib(BOOL bTerm = TRUE, BOOL bJustRevoke = FALSE);

/////////////////////////////////////////////////////////////////////////////
// Memory management helpers (for OLE task allocator memory)

#define AfxAllocTaskMem(nSize) CoTaskMemAlloc(nSize)
#define AfxFreeTaskMem(p) CoTaskMemFree(p)

LPWSTR AFXAPI AfxAllocTaskWideString(LPCWSTR lpszString);
LPWSTR AFXAPI AfxAllocTaskWideString(LPCSTR lpszString);
LPSTR AFXAPI AfxAllocTaskAnsiString(LPCWSTR lpszString);
LPSTR AFXAPI AfxAllocTaskAnsiString(LPCSTR lpszString);

#ifdef _UNICODE
	#define AfxAllocTaskString(x) AfxAllocTaskWideString(x)
#else
	#define AfxAllocTaskString(x) AfxAllocTaskAnsiString(x)
#endif

#ifdef OLE2ANSI
	#define AfxAllocTaskOleString(x) AfxAllocTaskAnsiString(x)
#else
	#define AfxAllocTaskOleString(x) AfxAllocTaskWideString(x)
#endif

HRESULT AFXAPI AfxGetClassIDFromString(LPCTSTR lpsz, LPCLSID lpClsID);

/////////////////////////////////////////////////////////////////////////////
// Special in-proc server APIs

SCODE AFXAPI AfxDllGetClassObject(REFCLSID rclsid, REFIID riid, LPVOID* ppv);
SCODE AFXAPI AfxDllCanUnloadNow(void);

/////////////////////////////////////////////////////////////////////////////
// COleVariant class helpers

#define AFX_OLE_TRUE (-1)
#define AFX_OLE_FALSE 0

class CLongBinary;  // forward reference (see afxdb_.h)

/////////////////////////////////////////////////////////////////////////////
// COleVariant class - wraps VARIANT types

typedef const VARIANT* LPCVARIANT;

class COleVariant : public tagVARIANT
{
// Constructors
public:
	COleVariant();

	COleVariant(const VARIANT& varSrc);
	COleVariant(LPCVARIANT pSrc);
	COleVariant(const COleVariant& varSrc);

	COleVariant(LPCTSTR lpszSrc);
	COleVariant(LPCTSTR lpszSrc, VARTYPE vtSrc); // used to set to ANSI string
	COleVariant(CString& strSrc);

	COleVariant(BYTE nSrc);
	COleVariant(short nSrc, VARTYPE vtSrc = VT_I2);
	COleVariant(long lSrc, VARTYPE vtSrc = VT_I4);
	COleVariant(const COleCurrency& curSrc);

	COleVariant(float fltSrc);
	COleVariant(double dblSrc);
	COleVariant(const COleDateTime& timeSrc);

	COleVariant(const CByteArray& arrSrc);
	COleVariant(const CLongBinary& lbSrc);

	COleVariant(LPCITEMIDLIST pidl);

// Operations
public:
	void Clear();
	void ChangeType(VARTYPE vartype, LPVARIANT pSrc = NULL);
	void Attach(VARIANT& varSrc);
	VARIANT Detach();

	BOOL operator==(const VARIANT& varSrc) const;
	BOOL operator==(LPCVARIANT pSrc) const;

	const COleVariant& operator=(const VARIANT& varSrc);
	const COleVariant& operator=(LPCVARIANT pSrc);
	const COleVariant& operator=(const COleVariant& varSrc);

	const COleVariant& operator=(const LPCTSTR lpszSrc);
	const COleVariant& operator=(const CString& strSrc);

	const COleVariant& operator=(BYTE nSrc);
	const COleVariant& operator=(short nSrc);
	const COleVariant& operator=(long lSrc);
	const COleVariant& operator=(const COleCurrency& curSrc);

	const COleVariant& operator=(float fltSrc);
	const COleVariant& operator=(double dblSrc);
	const COleVariant& operator=(const COleDateTime& dateSrc);

	const COleVariant& operator=(const CByteArray& arrSrc);
	const COleVariant& operator=(const CLongBinary& lbSrc);

	void SetString(LPCTSTR lpszSrc, VARTYPE vtSrc); // used to set ANSI string

	operator LPVARIANT();
	operator LPCVARIANT() const;

// Implementation
public:
	~COleVariant();
	void _ClearCompat();
};

// COleVariant diagnostics and serialization
#ifdef _DEBUG
CDumpContext& AFXAPI operator<<(CDumpContext& dc, COleVariant varSrc);
#endif
CArchive& AFXAPI operator<<(CArchive& ar, COleVariant varSrc);
CArchive& AFXAPI operator>>(CArchive& ar, COleVariant& varSrc);

// Helper for initializing VARIANT structures
void AFXAPI AfxVariantInit(LPVARIANT pVar);

/////////////////////////////////////////////////////////////////////////////
// COleCurrency class

class COleCurrency
{
// Constructors
public:
	COleCurrency();

	COleCurrency(CURRENCY cySrc);
	COleCurrency(const COleCurrency& curSrc);
	COleCurrency(const VARIANT& varSrc);
	COleCurrency(long nUnits, long nFractionalUnits);

// Attributes
public:
	enum CurrencyStatus
	{
		valid = 0,
		invalid = 1,    // Invalid currency (overflow, div 0, etc.)
		null = 2,       // Literally has no value
	};

	CURRENCY m_cur;
	CurrencyStatus m_status;

	void SetStatus(CurrencyStatus status);
	CurrencyStatus GetStatus() const;

// Operations
public:
	const COleCurrency& operator=(CURRENCY cySrc);
	const COleCurrency& operator=(const COleCurrency& curSrc);
	const COleCurrency& operator=(const VARIANT& varSrc);

	BOOL operator==(const COleCurrency& cur) const;
	BOOL operator!=(const COleCurrency& cur) const;
	BOOL operator<(const COleCurrency& cur) const;
	BOOL operator>(const COleCurrency& cur) const;
	BOOL operator<=(const COleCurrency& cur) const;
	BOOL operator>=(const COleCurrency& cur) const;

	// Currency math
	COleCurrency operator+(const COleCurrency& cur) const;
	COleCurrency operator-(const COleCurrency& cur) const;
	const COleCurrency& operator+=(const COleCurrency& cur);
	const COleCurrency& operator-=(const COleCurrency& cur);
	COleCurrency operator-() const;

	COleCurrency operator*(long nOperand) const;
	COleCurrency operator/(long nOperand) const;
	const COleCurrency& operator*=(long nOperand);
	const COleCurrency& operator/=(long nOperand);

	operator CURRENCY() const;

	// Currency definition
	void SetCurrency(long nUnits, long nFractionalUnits);
	BOOL ParseCurrency(LPCTSTR lpszCurrency, DWORD dwFlags = 0,
		LCID = LANG_USER_DEFAULT);

	// formatting
	CString Format(DWORD dwFlags = 0, LCID lcid = LANG_USER_DEFAULT) const;
};

// COleCurrency diagnostics and serialization
#ifdef _DEBUG
CDumpContext& AFXAPI operator<<(CDumpContext& dc, COleCurrency curSrc);
#endif
CArchive& AFXAPI operator<<(CArchive& ar, COleCurrency curSrc);
CArchive& AFXAPI operator>>(CArchive& ar, COleCurrency& curSrc);

/////////////////////////////////////////////////////////////////////////////
// COleDateTime class helpers

#define AFX_OLE_DATETIME_ERROR (-1)
#define AFX_OLE_DATETIME_HALFSECOND (1.0 / (2.0 * (60.0 * 60.0 * 24.0)))

/////////////////////////////////////////////////////////////////////////////
// COleDateTime class

class COleDateTime
{
// Constructors
public:
	static COleDateTime PASCAL GetCurrentTime();

	COleDateTime();

	COleDateTime(const COleDateTime& dateSrc);
	COleDateTime(const VARIANT& varSrc);
	COleDateTime(DATE dtSrc);

	COleDateTime(time_t timeSrc);
	COleDateTime(const SYSTEMTIME& systimeSrc);
	COleDateTime(const FILETIME& filetimeSrc);

	COleDateTime(int nYear, int nMonth, int nDay,
		int nHour, int nMin, int nSec);
	COleDateTime(WORD wDosDate, WORD wDosTime);

// Attributes
public:
	enum DateTimeStatus
	{
		valid = 0,
		invalid = 1,    // Invalid date (out of range, etc.)
		null = 2,       // Literally has no value
	};

	DATE m_dt;
	DateTimeStatus m_status;

	void SetStatus(DateTimeStatus status);
	DateTimeStatus GetStatus() const;

	BOOL GetAsSystemTime(SYSTEMTIME& sysTime) const;

	int GetYear() const;
	int GetMonth() const;       // month of year (1 = Jan)
	int GetDay() const;         // day of month (0-31)
	int GetHour() const;        // hour in day (0-23)
	int GetMinute() const;      // minute in hour (0-59)
	int GetSecond() const;      // second in minute (0-59)
	int GetDayOfWeek() const;   // 1=Sun, 2=Mon, ..., 7=Sat
	int GetDayOfYear() const;   // days since start of year, Jan 1 = 1

// Operations
public:
	const COleDateTime& operator=(const COleDateTime& dateSrc);
	const COleDateTime& operator=(const VARIANT& varSrc);
	const COleDateTime& operator=(DATE dtSrc);

	const COleDateTime& operator=(const time_t& timeSrc);
	const COleDateTime& operator=(const SYSTEMTIME& systimeSrc);
	const COleDateTime& operator=(const FILETIME& filetimeSrc);

	BOOL operator==(const COleDateTime& date) const;
	BOOL operator!=(const COleDateTime& date) const;
	BOOL operator<(const COleDateTime& date) const;
	BOOL operator>(const COleDateTime& date) const;
	BOOL operator<=(const COleDateTime& date) const;
	BOOL operator>=(const COleDateTime& date) const;

	// DateTime math
	COleDateTime operator+(const COleDateTimeSpan& dateSpan) const;
	COleDateTime operator-(const COleDateTimeSpan& dateSpan) const;
	const COleDateTime& operator+=(const COleDateTimeSpan dateSpan);
	const COleDateTime& operator-=(const COleDateTimeSpan dateSpan);

	// DateTimeSpan math
	COleDateTimeSpan operator-(const COleDateTime& date) const;

	operator DATE() const;

	int SetDateTime(int nYear, int nMonth, int nDay,
		int nHour, int nMin, int nSec);
	int SetDate(int nYear, int nMonth, int nDay);
	int SetTime(int nHour, int nMin, int nSec);
	BOOL ParseDateTime(LPCTSTR lpszDate, DWORD dwFlags = 0,
		LCID lcid = LANG_USER_DEFAULT);

	// formatting
	CString Format(DWORD dwFlags = 0, LCID lcid = LANG_USER_DEFAULT) const;
	CString Format(LPCTSTR lpszFormat) const;
	CString Format(UINT nFormatID) const;

// Implementation
protected:
	void CheckRange();
	friend COleDateTimeSpan;
};

// COleDateTime diagnostics and serialization
#ifdef _DEBUG
CDumpContext& AFXAPI operator<<(CDumpContext& dc, COleDateTime dateSrc);
#endif
CArchive& AFXAPI operator<<(CArchive& ar, COleDateTime dateSrc);
CArchive& AFXAPI operator>>(CArchive& ar, COleDateTime& dateSrc);

/////////////////////////////////////////////////////////////////////////////
// COleDateTimeSpan class
class COleDateTimeSpan
{
// Constructors
public:
	COleDateTimeSpan();

	COleDateTimeSpan(double dblSpanSrc);
	COleDateTimeSpan(const COleDateTimeSpan& dateSpanSrc);
	COleDateTimeSpan(long lDays, int nHours, int nMins, int nSecs);

// Attributes
public:
	enum DateTimeSpanStatus
	{
		valid = 0,
		invalid = 1,    // Invalid span (out of range, etc.)
		null = 2,       // Literally has no value
	};

	double m_span;
	DateTimeSpanStatus m_status;

	void SetStatus(DateTimeSpanStatus status);
	DateTimeSpanStatus GetStatus() const;

	double GetTotalDays() const;    // span in days (about -3.65e6 to 3.65e6)
	double GetTotalHours() const;   // span in hours (about -8.77e7 to 8.77e6)
	double GetTotalMinutes() const; // span in minutes (about -5.26e9 to 5.26e9)
	double GetTotalSeconds() const; // span in seconds (about -3.16e11 to 3.16e11)

	long GetDays() const;       // component days in span
	long GetHours() const;      // component hours in span (-23 to 23)
	long GetMinutes() const;    // component minutes in span (-59 to 59)
	long GetSeconds() const;    // component seconds in span (-59 to 59)

// Operations
public:
	const COleDateTimeSpan& operator=(double dblSpanSrc);
	const COleDateTimeSpan& operator=(const COleDateTimeSpan& dateSpanSrc);

	BOOL operator==(const COleDateTimeSpan& dateSpan) const;
	BOOL operator!=(const COleDateTimeSpan& dateSpan) const;
	BOOL operator<(const COleDateTimeSpan& dateSpan) const;
	BOOL operator>(const COleDateTimeSpan& dateSpan) const;
	BOOL operator<=(const COleDateTimeSpan& dateSpan) const;
	BOOL operator>=(const COleDateTimeSpan& dateSpan) const;

	// DateTimeSpan math
	COleDateTimeSpan operator+(const COleDateTimeSpan& dateSpan) const;
	COleDateTimeSpan operator-(const COleDateTimeSpan& dateSpan) const;
	const COleDateTimeSpan& operator+=(const COleDateTimeSpan dateSpan);
	const COleDateTimeSpan& operator-=(const COleDateTimeSpan dateSpan);
	COleDateTimeSpan operator-() const;

	operator double() const;

	void SetDateTimeSpan(long lDays, int nHours, int nMins, int nSecs);

	// formatting
	CString Format(LPCTSTR pFormat) const;
	CString Format(UINT nID) const;

// Implementation
public:
	void CheckRange();
	friend COleDateTime;
};

// COleDateTimeSpan diagnostics and serialization
#ifdef _DEBUG
CDumpContext& AFXAPI operator<<(CDumpContext& dc,COleDateTimeSpan dateSpanSrc);
#endif
CArchive& AFXAPI operator<<(CArchive& ar, COleDateTimeSpan dateSpanSrc);
CArchive& AFXAPI operator>>(CArchive& ar, COleDateTimeSpan& dateSpanSrc);

/////////////////////////////////////////////////////////////////////////////
// Helper for initializing COleSafeArray
void AFXAPI AfxSafeArrayInit(COleSafeArray* psa);

/////////////////////////////////////////////////////////////////////////////
// CSafeArray class

typedef const SAFEARRAY* LPCSAFEARRAY;

class COleSafeArray : public tagVARIANT
{
//Constructors
public:
	COleSafeArray();
	COleSafeArray(const SAFEARRAY& saSrc, VARTYPE vtSrc);
	COleSafeArray(LPCSAFEARRAY pSrc, VARTYPE vtSrc);
	COleSafeArray(const COleSafeArray& saSrc);
	COleSafeArray(const VARIANT& varSrc);
	COleSafeArray(LPCVARIANT pSrc);
	COleSafeArray(const COleVariant& varSrc);

// Operations
public:
	void Clear();
	void Attach(VARIANT& varSrc);
	VARIANT Detach();

	COleSafeArray& operator=(const COleSafeArray& saSrc);
	COleSafeArray& operator=(const VARIANT& varSrc);
	COleSafeArray& operator=(LPCVARIANT pSrc);
	COleSafeArray& operator=(const COleVariant& varSrc);

	BOOL operator==(const SAFEARRAY& saSrc) const;
	BOOL operator==(LPCSAFEARRAY pSrc) const;
	BOOL operator==(const COleSafeArray& saSrc) const;
	BOOL operator==(const VARIANT& varSrc) const;
	BOOL operator==(LPCVARIANT pSrc) const;
	BOOL operator==(const COleVariant& varSrc) const;

	operator LPVARIANT();
	operator LPCVARIANT() const;

	// One dim array helpers
	void CreateOneDim(VARTYPE vtSrc, DWORD dwElements,
		const void* pvSrcData = NULL, long nLBound = 0);
	DWORD GetOneDimSize();
	void ResizeOneDim(DWORD dwElements);

	// Multi dim array helpers
	void Create(VARTYPE vtSrc, DWORD dwDims, DWORD* rgElements);

	// SafeArray wrapper classes
	void Create(VARTYPE vtSrc, DWORD dwDims, SAFEARRAYBOUND* rgsabounds);
	void AccessData(void** ppvData);
	void UnaccessData();
	void AllocData();
	void AllocDescriptor(DWORD dwDims);
	void Copy(LPSAFEARRAY* ppsa);
	void GetLBound(DWORD dwDim, long* pLBound);
	void GetUBound(DWORD dwDim, long* pUBound);
	void GetElement(long* rgIndices, void* pvData);
	void PtrOfIndex(long* rgIndices, void** ppvData);
	void PutElement(long* rgIndices, void* pvData);
	void Redim(SAFEARRAYBOUND* psaboundNew);
	void Lock();
	void Unlock();
	DWORD GetDim();
	DWORD GetElemSize();
	void Destroy();
	void DestroyData();
	void DestroyDescriptor();

//Implementation
public:
	~COleSafeArray();

	// Cache info to make element access (operator []) faster
	DWORD m_dwElementSize;
	DWORD m_dwDims;
};

// COleSafeArray diagnostics and serialization
#ifdef _DEBUG
CDumpContext& AFXAPI operator<<(CDumpContext& dc, COleSafeArray& saSrc);
#endif

/////////////////////////////////////////////////////////////////////////////
// DDX_ functions for OLE controls on dialogs

#ifndef _AFX_NO_OCC_SUPPORT

void AFXAPI DDX_OCText(CDataExchange* pDX, int nIDC, DISPID dispid,
	CString& value);
void AFXAPI DDX_OCTextRO(CDataExchange* pDX, int nIDC, DISPID dispid,
	CString& value);
void AFXAPI DDX_OCBool(CDataExchange* pDX, int nIDC, DISPID dispid,
	BOOL& value);
void AFXAPI DDX_OCBoolRO(CDataExchange* pDX, int nIDC, DISPID dispid,
	BOOL& value);
void AFXAPI DDX_OCInt(CDataExchange* pDX, int nIDC, DISPID dispid,
	int &value);
void AFXAPI DDX_OCIntRO(CDataExchange* pDX, int nIDC, DISPID dispid,
	int &value);
void AFXAPI DDX_OCInt(CDataExchange* pDX, int nIDC, DISPID dispid,
	long &value);
void AFXAPI DDX_OCIntRO(CDataExchange* pDX, int nIDC, DISPID dispid,
	long &value);
void AFXAPI DDX_OCShort(CDataExchange* pDX, int nIDC, DISPID dispid,
	short& value);
void AFXAPI DDX_OCShortRO(CDataExchange* pDX, int nIDC, DISPID dispid,
	short& value);
void AFXAPI DDX_OCColor(CDataExchange* pDX, int nIDC, DISPID dispid,
	OLE_COLOR& value);
void AFXAPI DDX_OCColorRO(CDataExchange* pDX, int nIDC, DISPID dispid,
	OLE_COLOR& value);
void AFXAPI DDX_OCFloat(CDataExchange* pDX, int nIDC, DISPID dispid,
	float& value);
void AFXAPI DDX_OCFloatRO(CDataExchange* pDX, int nIDC, DISPID dispid,
	float& value);
void AFXAPI DDX_OCFloat(CDataExchange* pDX, int nIDC, DISPID dispid,
	double& value);
void AFXAPI DDX_OCFloatRO(CDataExchange* pDX, int nIDC, DISPID dispid,
	double& value);

#endif // !_AFX_NO_OCC_SUPPORT

/////////////////////////////////////////////////////////////////////////////
// Function to enable containment of OLE controls

#ifndef _AFX_NO_OCC_SUPPORT
void AFX_CDECL AfxEnableControlContainer(COccManager* pOccManager=NULL);
#else
#define AfxEnableControlContainer()
#endif

/////////////////////////////////////////////////////////////////////////////
// Inline function declarations

#ifdef _AFX_PACKING
#pragma pack(pop)
#endif

#ifdef _AFX_ENABLE_INLINES
#define _AFXDISP_INLINE AFX_INLINE
#include <afxole.inl>
#undef _AFXDISP_INLINE
#endif

#undef AFX_DATA
#define AFX_DATA

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, on)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, off)
#endif

#endif //__AFXDISP_H__

/////////////////////////////////////////////////////////////////////////////
