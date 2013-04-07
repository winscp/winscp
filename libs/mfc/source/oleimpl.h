// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1997 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#undef AFX_DATA
#define AFX_DATA AFX_OLE_DATA

/////////////////////////////////////////////////////////////////////////////
// AFX_OLE_CALL - used to dynamically load the OLE32 library

#ifdef _AFXDLL

struct AFX_OLE_CALL
{
	// main OLE32.DLL entry points
	HRESULT (STDAPICALLTYPE* pfnReadFmtUserTypeStg)(LPSTORAGE pstg,
		CLIPFORMAT FAR* pcf, LPOLESTR FAR* lplpszUserType);
	HRESULT (STDAPICALLTYPE* pfnReadClassStg)(LPSTORAGE pStg, CLSID FAR* pclsid);
	HRESULT (STDAPICALLTYPE* pfnCreateFileMoniker)(LPCOLESTR lpszPathName,
		LPMONIKER FAR* ppmk);
	HRESULT (STDAPICALLTYPE* pfnStgIsStorageFile)(const OLECHAR * pwcsName);
	HRESULT (STDAPICALLTYPE* pfnStgOpenStorage)(const OLECHAR * pwcsName,
		IStorage *pstgPriority, DWORD grfMode, SNB snbExclude, DWORD reserved,
		IStorage ** ppstgOpen);
	HRESULT (STDAPICALLTYPE* pfnDoDragDrop)(LPDATAOBJECT pDataObj, LPDROPSOURCE pDropSource,
		DWORD dwOKEffects, LPDWORD pdwEffect);
	HRESULT (STDAPICALLTYPE* pfnCoLockObjectExternal)(LPUNKNOWN pUnk, BOOL fLock, BOOL fLastUnlockReleases);
	HRESULT (STDAPICALLTYPE* pfnRegisterDragDrop)(HWND hwnd, LPDROPTARGET pDropTarget);
	HRESULT (STDAPICALLTYPE* pfnOleRegGetUserType )(REFCLSID clsid, DWORD dwFormOfType,
		LPOLESTR * pszUserType);
	HRESULT (STDAPICALLTYPE* pfnStgCreateDocfile)(const OLECHAR * pwcsName, DWORD grfMode,
		DWORD reserved, IStorage** ppstgOpen);
	HRESULT (STDAPICALLTYPE* pfnRevokeDragDrop)(HWND hwnd);
	HRESULT (STDAPICALLTYPE* pfnCoRegisterClassObject)(REFCLSID rclsid, LPUNKNOWN pUnk,
		DWORD dwClsContext, DWORD flags, LPDWORD lpdwRegister);
	HRESULT (STDAPICALLTYPE* pfnCoRevokeClassObject)(DWORD dwRegister);
	HRESULT (STDAPICALLTYPE* pfnOleTranslateAccelerator)(LPOLEINPLACEFRAME lpFrame,
		LPOLEINPLACEFRAMEINFO lpFrameInfo, LPMSG lpmsg);
	BOOL (STDAPICALLTYPE* pfnIsAccelerator)(HACCEL hAccel, INT cAccelEntries,
		LPMSG lpMsg, WORD* lpwCmd);
	HOLEMENU (STDAPICALLTYPE* pfnOleCreateMenuDescriptor)(HMENU hmenuCombined,
		LPOLEMENUGROUPWIDTHS lpMenuWidths);
	HRESULT (STDAPICALLTYPE* pfnOleDestroyMenuDescriptor )(HOLEMENU holemenu);
	HRESULT (STDAPICALLTYPE* pfnGetRunningObjectTable)(DWORD reserved, LPRUNNINGOBJECTTABLE FAR* pprot);
	HRESULT (STDAPICALLTYPE* pfnWriteClassStg)(LPSTORAGE pStg, REFCLSID rclsid);
	HRESULT (STDAPICALLTYPE* pfnOleQueryLinkFromData)(LPDATAOBJECT pSrcDataObject);
	HRESULT (STDAPICALLTYPE* pfnCoRegisterMessageFilter)(LPMESSAGEFILTER lpMessageFilter,
		LPMESSAGEFILTER * lplpMessageFilter);
	HRESULT (STDAPICALLTYPE* pfnCoCreateInstance)(REFCLSID rclsid, LPUNKNOWN * pUnkOuter,
		DWORD dwClsContext, REFIID riid, LPVOID* ppv);
	HRESULT (STDAPICALLTYPE* pfnCreateBindCtx)(DWORD reserved, LPBC FAR* ppbc);
	HRESULT (STDAPICALLTYPE* pfnStringFromCLSID)(REFCLSID rclsid, LPOLESTR FAR* lplpsz);
	HRESULT (STDAPICALLTYPE* pfnCoDisconnectObject)(LPUNKNOWN pUnk, DWORD dwReserved);
	HRESULT (STDAPICALLTYPE* pfnOleRegEnumVerbs )(REFCLSID clsid, LPENUMOLEVERB FAR* ppenum);
	void (STDAPICALLTYPE* pfnOleUninitialize)(void);
	HRESULT (STDAPICALLTYPE* pfnCreateOleAdviseHolder)(LPOLEADVISEHOLDER FAR* ppOAHolder);
	HRESULT (STDAPICALLTYPE* pfnCreateDataAdviseHolder)(LPDATAADVISEHOLDER FAR* ppDAHolder);
	HRESULT (STDAPICALLTYPE* pfnOleGetAutoConvert)(REFCLSID clsidOld, LPCLSID pClsidNew);
	HRESULT (STDAPICALLTYPE* pfnCoGetClassObject)(REFCLSID rclsid, DWORD dwClsContext,
		LPVOID pvReserved, REFIID riid, LPVOID* ppv);
	HRESULT (STDAPICALLTYPE* pfnOleCreateDefaultHandler)(REFCLSID clsid,
		LPUNKNOWN pUnkOuter, REFIID riid, LPVOID* lplpvObj);
	HRESULT (STDAPICALLTYPE* pfnCreateDataCache)(
		LPUNKNOWN pUnkOuter, REFCLSID clsid, REFIID riid, LPVOID* lplpvObj);
	HRESULT (STDAPICALLTYPE* pfnReadClassStm)(LPSTREAM pStm, CLSID FAR* pclsid);
	HRESULT (STDAPICALLTYPE* pfnOleLoadFromStream)(LPSTREAM pStm, REFIID iidInterface, LPVOID FAR* ppvObj);
	int (STDAPICALLTYPE* pfnStringFromGUID2)(REFGUID rguid, LPOLESTR lpsz, int cbMax);
	void (STDAPICALLTYPE* pfnCoUninitialize)(void);
	HRESULT (STDAPICALLTYPE* pfnCoInitialize)(LPVOID pvReserved);
	HRESULT (STDAPICALLTYPE* pfnOleInitialize)(LPVOID pvReserved);
	void (STDAPICALLTYPE* pfnCoFreeUnusedLibraries)(void);
	HRESULT (STDAPICALLTYPE* pfnOleCreateFromData)(LPDATAOBJECT pSrcDataObj,
		REFIID riid, DWORD renderopt, LPFORMATETC pFormatEtc,
		LPOLECLIENTSITE pClientSite, LPSTORAGE pStg, LPVOID* ppvObj);
	HRESULT (STDAPICALLTYPE* pfnOleSetContainedObject)(LPUNKNOWN pUnknown, BOOL fContained);
	HRESULT (STDAPICALLTYPE* pfnOleLockRunning)(LPUNKNOWN pUnknown, BOOL fLock, BOOL fLastUnlockCloses);
	LPVOID (STDAPICALLTYPE* pfnCoTaskMemAlloc)(ULONG cb);
	HRESULT (STDAPICALLTYPE* pfnCLSIDFromString)(LPOLESTR lpsz, LPCLSID pclsid);
	HRESULT (STDAPICALLTYPE* pfnCLSIDFromProgID )(LPCOLESTR lpszProgID, LPCLSID lpclsid);
	HRESULT (STDAPICALLTYPE* pfnOleIsCurrentClipboard)(LPDATAOBJECT pDataObj);
	HRESULT (STDAPICALLTYPE* pfnOleFlushClipboard)(void);
	HRESULT (STDAPICALLTYPE* pfnOleSetClipboard)(LPDATAOBJECT pDataObj);
	BOOL (STDAPICALLTYPE* pfnOleIsRunning)(LPOLEOBJECT pObject);
	HRESULT (STDAPICALLTYPE* pfnOleRun)(LPUNKNOWN pUnknown);
	HRESULT (STDAPICALLTYPE* pfnOleGetClipboard)(LPDATAOBJECT FAR* ppDataObj);
	HRESULT (STDAPICALLTYPE* pfnCoTreatAsClass)(REFCLSID clsidOld, REFCLSID clsidNew);
	HRESULT (STDAPICALLTYPE* pfnOleQueryCreateFromData)(LPDATAOBJECT pSrcDataObject);
	HRESULT (STDAPICALLTYPE* pfnOleSetMenuDescriptor )(HOLEMENU holemenu,
		HWND hwndFrame, HWND hwndActiveObject, LPOLEINPLACEFRAME lpFrame,
		LPOLEINPLACEACTIVEOBJECT lpActiveObj);
	HRESULT (STDAPICALLTYPE* pfnCreateItemMoniker)(LPCOLESTR lpszDelim,
		LPCOLESTR lpszItem, LPMONIKER* ppmk);
	HRESULT (STDAPICALLTYPE* pfnCreateGenericComposite)(LPMONIKER pmkFirst,
		LPMONIKER pmkRest, LPMONIKER* ppmkComposite);
	HRESULT (STDAPICALLTYPE* pfnCreateStreamOnHGlobal)(HGLOBAL hGlobal,
		BOOL fDeleteOnRelease, LPSTREAM* ppstm);
	HRESULT (STDAPICALLTYPE* pfnOleSaveToStream)(LPPERSISTSTREAM pPStm, LPSTREAM pStm);
	HRESULT (STDAPICALLTYPE* pfnWriteClassStm)(LPSTREAM pStm, REFCLSID rclsid);
	void (STDAPICALLTYPE* pfnCoTaskMemFree)(LPVOID pv);
	HGLOBAL (STDAPICALLTYPE* pfnOleGetIconOfClass)(REFCLSID rclsid, LPOLESTR lpszLabel,
		BOOL fUseTypeAsLabel);
	void (STDAPICALLTYPE* pfnReleaseStgMedium)(LPSTGMEDIUM);
	HRESULT (STDAPICALLTYPE* pfnGetHGlobalFromILockBytes )(LPLOCKBYTES plkbyt, HGLOBAL FAR* phglobal);
	HRESULT (STDAPICALLTYPE* pfnStgOpenStorageOnILockBytes)(LPLOCKBYTES plkbyt,
		LPSTORAGE pstgPriority, DWORD grfMode, SNB snbExclude, DWORD reserved,
		LPSTORAGE* ppstgOpen);
	HRESULT (STDAPICALLTYPE* pfnCreateILockBytesOnHGlobal)(HGLOBAL hGlobal,
		BOOL fDeleteOnRelease, LPLOCKBYTES* pplkbyt);
	HRESULT (STDAPICALLTYPE* pfnStgCreateDocfileOnILockBytes)(LPLOCKBYTES plkbyt,
		DWORD grfMode, DWORD reserved, LPSTORAGE* ppstgOpen);
	HRESULT (STDAPICALLTYPE* pfnOleSave)(LPPERSISTSTORAGE pPS, LPSTORAGE pStg, BOOL fSameAsLoad);
	HRESULT (STDAPICALLTYPE* pfnOleLoad)(LPSTORAGE pStg, REFIID riid,
		LPOLECLIENTSITE pClientSite, LPVOID* ppvObj);
	HRESULT (STDAPICALLTYPE* pfnOleCreate)(REFCLSID rclsid, REFIID riid,
		DWORD renderopt, LPFORMATETC pFormatEtc, LPOLECLIENTSITE pClientSite,
		LPSTORAGE pStg, LPVOID* ppvObj);
	HRESULT (STDAPICALLTYPE* pfnOleCreateLinkToFile)(LPCOLESTR lpszFileName,
		REFIID riid, DWORD renderopt, LPFORMATETC lpFormatEtc,
		LPOLECLIENTSITE pClientSite, LPSTORAGE pStg, LPVOID* ppvObj);
	HRESULT (STDAPICALLTYPE* pfnOleCreateFromFile)(REFCLSID rclsid,
		LPCOLESTR lpszFileName, REFIID riid, DWORD renderopt,
		LPFORMATETC pFormatEtc, LPOLECLIENTSITE pClientSite, LPSTORAGE pStg,
		LPVOID* ppvObj);
	HRESULT (STDAPICALLTYPE* pfnOleCreateStaticFromData)(LPDATAOBJECT pSrcDataObj,
		REFIID riid, DWORD renderopt, LPFORMATETC pFormatEtc,
		LPOLECLIENTSITE pClientSite, LPSTORAGE pStg, LPVOID* ppvObj);
	HRESULT (STDAPICALLTYPE* pfnOleCreateLinkFromData)(LPDATAOBJECT pSrcDataObj,
		REFIID riid, DWORD renderopt, LPFORMATETC pFormatEtc,
		LPOLECLIENTSITE pClientSite, LPSTORAGE pStg, LPVOID* ppvObj);
	HRESULT (STDAPICALLTYPE* pfnSetConvertStg)(LPSTORAGE pStg, BOOL fConvert);
	HANDLE (STDAPICALLTYPE* pfnOleDuplicateData)(HANDLE hSrc, CLIPFORMAT cfFormat,
		UINT uiFlags);
	HRESULT (STDAPICALLTYPE* pfnWriteFmtUserTypeStg )(LPSTORAGE pstg, CLIPFORMAT cf, LPOLESTR lpszUserType);
	HRESULT (STDAPICALLTYPE* pfnOleRegGetMiscStatus)(REFCLSID clsid, DWORD dwAspect,
		DWORD* pdwStatus);
	HRESULT (STDAPICALLTYPE* pfnCoGetMalloc)(DWORD dwMemContext, LPMALLOC * ppMalloc);
	HRESULT (STDAPICALLTYPE* pfnStgIsStorageILockBytes)(LPLOCKBYTES plkbyt);

	// OLEAUT32.DLL entry points
	void (STDAPICALLTYPE* pfnSysFreeString)(BSTR);
#if !defined(_MAC)
	BSTR (STDAPICALLTYPE* pfnSysAllocStringByteLen)(const char FAR* psz,
		unsigned int len);
#endif
	HRESULT (STDAPICALLTYPE* pfnVariantCopy)(VARIANTARG FAR* pvargDest,
		VARIANTARG FAR* pvargSrc);
	HRESULT (STDAPICALLTYPE* pfnVariantClear)(VARIANTARG FAR* pvarg);
	HRESULT (STDAPICALLTYPE* pfnVariantChangeType)(VARIANTARG FAR* pvargDest,
		VARIANTARG FAR* pvarSrc, unsigned short wFlags, VARTYPE vt);
	BSTR (STDAPICALLTYPE* pfnSysAllocStringLen)(const OLECHAR FAR*,
		unsigned int);
	unsigned int (STDAPICALLTYPE* pfnSysStringLen)(BSTR);
	int (STDAPICALLTYPE* pfnSysReAllocStringLen)(BSTR FAR*, const OLECHAR FAR*,
		unsigned int);
	BSTR (STDAPICALLTYPE* pfnSysAllocString)(const OLECHAR FAR*);
#if !defined(_MAC)
	unsigned int (STDAPICALLTYPE* pfnSysStringByteLen)(BSTR bstr);
#endif
	HRESULT (STDAPICALLTYPE* pfnVarCyFromStr)(OLECHAR FAR* strIn, LCID lcid,
		unsigned long dwFlags, CY FAR* pcyOut);
	HRESULT (STDAPICALLTYPE* pfnVarBstrFromCy)(CY cyIn, LCID lcid,
		unsigned long dwFlags, BSTR FAR* pbstrOut);
	HRESULT (STDAPICALLTYPE* pfnVarDateFromStr)(OLECHAR FAR* strIn, LCID lcid,
		unsigned long dwFlags, DATE FAR* pdateOut);
	HRESULT (STDAPICALLTYPE* pfnVarBstrFromDate)(DATE dateIn, LCID lcid,
		unsigned long dwFlags, BSTR FAR* pbstrOut);
	HRESULT (STDAPICALLTYPE* pfnLoadTypeLib)(const OLECHAR FAR *szFile,
		ITypeLib FAR* FAR* pptlib);
	HRESULT (STDAPICALLTYPE* pfnLoadRegTypeLib)(REFGUID guid,
		WORD wVerMajor, WORD wVerMinor, LCID lcid, ITypeLib FAR* FAR* pptlib);
	HRESULT (STDAPICALLTYPE* pfnRegisterTypeLib)(ITypeLib FAR* ptlib,
		OLECHAR FAR *szFullPath, OLECHAR FAR *szHelpDir);
	int (STDAPICALLTYPE* pfnDosDateTimeToVariantTime)(unsigned short wDosDate,
		unsigned short wDosTime, double FAR* pvtime);
	SAFEARRAY FAR* (STDAPICALLTYPE* pfnSafeArrayCreate)(VARTYPE vt,
		unsigned int cDims, SAFEARRAYBOUND FAR* rgsabound);
	HRESULT (STDAPICALLTYPE* pfnSafeArrayRedim)(SAFEARRAY FAR* psa,
		SAFEARRAYBOUND FAR* psaboundNew);
	HRESULT (STDAPICALLTYPE* pfnSafeArrayAccessData)(SAFEARRAY FAR* psa,
		void HUGEP* FAR* ppvData);
	HRESULT (STDAPICALLTYPE* pfnSafeArrayUnaccessData)(SAFEARRAY FAR* psa);
	HRESULT (STDAPICALLTYPE* pfnSafeArrayGetUBound)(SAFEARRAY FAR* psa,
		unsigned int nDim, long FAR* plUbound);
	HRESULT (STDAPICALLTYPE* pfnSafeArrayGetLBound)(SAFEARRAY FAR* psa,
		unsigned int nDim, long FAR* plLbound);
	unsigned int (STDAPICALLTYPE* pfnSafeArrayGetElemsize)(SAFEARRAY FAR* psa);
	unsigned int (STDAPICALLTYPE* pfnSafeArrayGetDim)(SAFEARRAY FAR* psa);
	HRESULT (STDAPICALLTYPE* pfnSafeArrayCopy)(SAFEARRAY FAR* psa,
		SAFEARRAY FAR* FAR* ppsaOut);
	HRESULT (STDAPICALLTYPE* pfnSafeArrayAllocData)(SAFEARRAY FAR* psa);
	HRESULT (STDAPICALLTYPE* pfnSafeArrayAllocDescriptor)(unsigned int cDims,
		SAFEARRAY FAR* FAR* ppsaOut);
	HRESULT (STDAPICALLTYPE* pfnSafeArrayGetElement)(SAFEARRAY FAR* psa,
		long FAR* rgIndices, void FAR* pvData);
	HRESULT (STDAPICALLTYPE* pfnSafeArrayPutElement)(SAFEARRAY FAR* psa,
		long FAR* rgIndices, void FAR* pvData);
	HRESULT (STDAPICALLTYPE* pfnSafeArrayLock)(SAFEARRAY FAR* psa);
	HRESULT (STDAPICALLTYPE* pfnSafeArrayUnlock)(SAFEARRAY FAR* psa);
	HRESULT (STDAPICALLTYPE* pfnSafeArrayDestroy)(SAFEARRAY FAR* psa);
	HRESULT (STDAPICALLTYPE* pfnSafeArrayDestroyData)(SAFEARRAY FAR* psa);
	HRESULT (STDAPICALLTYPE* pfnSafeArrayDestroyDescriptor)(SAFEARRAY FAR* psa);
	HRESULT (STDAPICALLTYPE* pfnSafeArrayPtrOfIndex)(SAFEARRAY FAR* psa,
		long FAR* rgIndices, void HUGEP* FAR* ppvData);
#if defined(_MAC) && defined(MACOCX)
	HRESULT (STDAPICALLTYPE* pfnLoadTypeLibFSp)(const void *pfsspec, ITypeLib FAR* FAR* pptlib);
	HRESULT (STDAPICALLTYPE* pfnRegisterTypeLibFolder)(OLECHAR FAR* szFullPath);
	HRESULT (STDAPICALLTYPE* pfnQueryTypeLibFolder)(LPBSTR pbstr);
	LCID (WINAPI* pfnGetUserDefaultLCID)(void);
#endif

	// OLEDLG.DLL entry points
	BOOL (STDAPICALLTYPE* pfnOleUIAddVerbMenu)(LPOLEOBJECT lpOleObj, LPCTSTR lpszShortType,
		HMENU hMenu, UINT uPos, UINT uIDVerbMin, UINT uIDVerbMax,
		BOOL bAddConvert, UINT idConvert, HMENU FAR *lphMenu);
	UINT (STDAPICALLTYPE* pfnOleUIBusy)(LPOLEUIBUSY);
	UINT (STDAPICALLTYPE* pfnOleUIChangeIcon)(LPOLEUICHANGEICON);
	UINT (STDAPICALLTYPE* pfnOleUIChangeSource)(LPOLEUICHANGESOURCE);
	UINT (STDAPICALLTYPE* pfnOleUIConvert)(LPOLEUICONVERT);
	UINT (STDAPICALLTYPE* pfnOleUIEditLinks)(LPOLEUIEDITLINKS);
	UINT (STDAPICALLTYPE* pfnOleUIInsertObject)(LPOLEUIINSERTOBJECT);
	UINT (STDAPICALLTYPE* pfnOleUIObjectProperties)(LPOLEUIOBJECTPROPS);
	UINT (STDAPICALLTYPE* pfnOleUIPasteSpecial)(LPOLEUIPASTESPECIAL);
	BOOL (STDAPICALLTYPE* pfnOleUIUpdateLinks)(LPOLEUILINKCONTAINER lpOleUILinkCntr,
		HWND hwndParent, LPTSTR lpszTitle, int cLinks);

	// Special Mac registry entry points
#ifdef _MAC
	LONG (APIENTRY* pfnRegCloseKey)(HKEY hKey);
	LONG (APIENTRY* pfnRegOpenKey)(HKEY hKey, LPCSTR lpSubKey, PHKEY phkResult);
	LONG (APIENTRY* pfnRegSetValue)(HKEY hKey, LPCSTR lpSubKey, DWORD dwType,
		LPCSTR lpData, DWORD cbData);
	LONG (APIENTRY* pfnRegSetValueEx)(HKEY hKey, LPCTSTR lpValueName,
		DWORD Reserved, DWORD dwType, CONST BYTE* lpData, DWORD cbData);
	LONG (APIENTRY* pfnRegQueryValue)(HKEY hKey, LPCSTR lpSubKey, LPSTR lpValue,
		PLONG lpcbValue);
	LONG (APIENTRY* pfnRegQueryValueEx)(HKEY hKey, LPCTSTR lpValueName,
		LPDWORD lpReserved, LPDWORD lpType, LPBYTE lpData, LPDWORD lpcbData);
	LONG (APIENTRY* pfnRegEnumKey)(HKEY hKey, DWORD dwIndex, LPTSTR lpName,
		DWORD cbName);
	LONG (APIENTRY* pfnRegDeleteKey)(HKEY hKey, LPCTSTR lpSubKey);
	LONG (APIENTRY* pfnRegDeleteValue)(HKEY hKey, LPCTSTR lpValueName);
	LONG (APIENTRY* pfnRegCreateKey)(HKEY hKey, LPCTSTR lpSubKey,
		PHKEY phkResult);
#endif
};

#ifndef _MAC

/////////////////////////////////////////////////////////////////////////////
// AFX_URLMON_CALL - used to dynamically load URLMON.DLL

#ifndef __urlmon_h__
#include <urlmon.h>
#endif

struct AFX_URLMON_CALL
{
	HRESULT (STDAPICALLTYPE* pfnRegisterBindStatusCallback)(LPBC pBC,
		IBindStatusCallback *pBSCb, IBindStatusCallback **pBSCbPrev, DWORD dwReserved);
	HRESULT (STDAPICALLTYPE* pfnRevokeBindStatusCallback)(LPBC pBC,
		IBindStatusCallback *pBSCb);
	HRESULT (STDAPICALLTYPE* pfnIsAsyncMoniker)(IMoniker* pmk);
	HRESULT (STDAPICALLTYPE* pfnCreateURLMoniker)(LPMONIKER pMkCtx, LPCWSTR szURL, LPMONIKER FAR * ppmk);
	HRESULT (STDAPICALLTYPE* pfnCreateAsyncBindCtx)(DWORD dwReserved, IBindStatusCallback *pBSCb,
		IEnumFORMATETC *pEFetc, IBindCtx **ppBC);
};

extern AFX_DATA AFX_URLMON_CALL _afxUrlMon;

/////////////////////////////////////////////////////////////////////////////
// macros for AFX_URLMON_CALL access

#ifdef RegisterBindStatusCallback
#undef RegisterBindStatusCallback
#endif
#define RegisterBindStatusCallback _afxUrlMon.pfnRegisterBindStatusCallback

#ifdef RevokeBindStatusCallback
#undef RevokeBindStatusCallback
#endif
#define RevokeBindStatusCallback _afxUrlMon.pfnRevokeBindStatusCallback

#ifdef IsAsyncMoniker
#undef IsAsyncMoniker
#endif
#define IsAsyncMoniker _afxUrlMon.pfnIsAsyncMoniker

#ifdef CreateURLMoniker
#undef CreateURLMoniker
#endif
#define CreateURLMoniker _afxUrlMon.pfnCreateURLMoniker

#ifdef CreateAsyncBindCtx
#undef CreateAsyncBindCtx
#endif
#define CreateAsyncBindCtx _afxUrlMon.pfnCreateAsyncBindCtx

#endif //!_MAC

extern AFX_DATA AFX_OLE_CALL _afxOLE;

// OLE32.DLL mappings
#ifdef ReadFmtUserTypeStg
#undef ReadFmtUserTypeStg
#endif
#define ReadFmtUserTypeStg  _afxOLE.pfnReadFmtUserTypeStg
#ifdef ReadClassStg
#undef ReadClassStg
#endif
#define ReadClassStg    _afxOLE.pfnReadClassStg
#ifdef CreateFileMoniker
#undef CreateFileMoniker
#endif
#define CreateFileMoniker   _afxOLE.pfnCreateFileMoniker
#ifdef StgIsStorageFile
#undef StgIsStorageFile
#endif
#define StgIsStorageFile    _afxOLE.pfnStgIsStorageFile
#ifdef StgOpenStorage
#undef StgOpenStorage
#endif
#define StgOpenStorage  _afxOLE.pfnStgOpenStorage

#if defined(_AFX_OLE_IMPL)
//DoDragDrop
inline HRESULT STDAPICALLTYPE DoDragDrop(LPDATAOBJECT pDataObj,
	LPDROPSOURCE pDropSource, DWORD dwOKEffects, LPDWORD pdwEffect)
{
	return _afxOLE.pfnDoDragDrop(pDataObj, pDropSource, dwOKEffects,
		pdwEffect);
}
#endif

#ifdef CoLockObjectExternal
#undef CoLockObjectExternal
#endif
#define CoLockObjectExternal    _afxOLE.pfnCoLockObjectExternal
#ifdef RegisterDragDrop
#undef RegisterDragDrop
#endif
#define RegisterDragDrop    _afxOLE.pfnRegisterDragDrop
#ifdef OleRegGetUserType
#undef OleRegGetUserType
#endif
#define OleRegGetUserType   _afxOLE.pfnOleRegGetUserType
#ifdef StgCreateDocfile
#undef StgCreateDocfile
#endif
#define StgCreateDocfile    _afxOLE.pfnStgCreateDocfile
#ifdef RevokeDragDrop
#undef RevokeDragDrop
#endif
#define RevokeDragDrop  _afxOLE.pfnRevokeDragDrop
#ifdef CoRegisterClassObject
#undef CoRegisterClassObject
#endif
#define CoRegisterClassObject   _afxOLE.pfnCoRegisterClassObject
#ifdef CoRevokeClassObject
#undef CoRevokeClassObject
#endif
#define CoRevokeClassObject _afxOLE.pfnCoRevokeClassObject
#ifdef OleTranslateAccelerator
#undef OleTranslateAccelerator
#endif
#define OleTranslateAccelerator _afxOLE.pfnOleTranslateAccelerator
#ifdef IsAccelerator
#undef IsAccelerator
#endif
#define IsAccelerator   _afxOLE.pfnIsAccelerator
#ifdef OleCreateMenuDescriptor
#undef OleCreateMenuDescriptor
#endif
#define OleCreateMenuDescriptor _afxOLE.pfnOleCreateMenuDescriptor
#ifdef OleDestroyMenuDescriptor
#undef OleDestroyMenuDescriptor
#endif
#define OleDestroyMenuDescriptor    _afxOLE.pfnOleDestroyMenuDescriptor
#ifdef GetRunningObjectTable
#undef GetRunningObjectTable
#endif
#define GetRunningObjectTable   _afxOLE.pfnGetRunningObjectTable
#ifdef WriteClassStg
#undef WriteClassStg
#endif
#define WriteClassStg   _afxOLE.pfnWriteClassStg
#ifdef OleQueryLinkFromData
#undef OleQueryLinkFromData
#endif
#define OleQueryLinkFromData    _afxOLE.pfnOleQueryLinkFromData
#ifdef CoRegisterMessageFilter
#undef CoRegisterMessageFilter
#endif
#define CoRegisterMessageFilter _afxOLE.pfnCoRegisterMessageFilter
#ifdef CoCreateInstance
#undef CoCreateInstance
#endif
#define CoCreateInstance    _afxOLE.pfnCoCreateInstance
#ifdef CreateBindCtx
#undef CreateBindCtx
#endif
#define CreateBindCtx   _afxOLE.pfnCreateBindCtx
#ifdef StringFromCLSID
#undef StringFromCLSID
#endif
#define StringFromCLSID _afxOLE.pfnStringFromCLSID
#ifdef CoDisconnectObject
#undef CoDisconnectObject
#endif
#define CoDisconnectObject  _afxOLE.pfnCoDisconnectObject
#ifdef OleRegEnumVerbs
#undef OleRegEnumVerbs
#endif
#define OleRegEnumVerbs _afxOLE.pfnOleRegEnumVerbs
#ifdef OleUninitialize
#undef OleUninitialize
#endif
#define OleUninitialize _afxOLE.pfnOleUninitialize
#ifdef CreateOleAdviseHolder
#undef CreateOleAdviseHolder
#endif
#define CreateOleAdviseHolder   _afxOLE.pfnCreateOleAdviseHolder
#ifdef CreateDataAdviseHolder
#undef CreateDataAdviseHolder
#endif
#define CreateDataAdviseHolder  _afxOLE.pfnCreateDataAdviseHolder
#ifdef OleGetAutoConvert
#undef OleGetAutoConvert
#endif
#define OleGetAutoConvert   _afxOLE.pfnOleGetAutoConvert
#ifdef CoGetClassObject
#undef CoGetClassObject
#endif
#define CoGetClassObject    _afxOLE.pfnCoGetClassObject
#ifdef OleCreateDefaultHandler
#undef OleCreateDefaultHandler
#endif
#define OleCreateDefaultHandler _afxOLE.pfnOleCreateDefaultHandler
#ifdef CreateDataCache
#undef CreateDataCache
#endif
#define CreateDataCache _afxOLE.pfnCreateDataCache
#ifdef ReadClassStm
#undef ReadClassStm
#endif
#define ReadClassStm    _afxOLE.pfnReadClassStm
#ifdef OleLoadFromStream
#undef OleLoadFromStream
#endif
#define OleLoadFromStream   _afxOLE.pfnOleLoadFromStream
#ifdef StringFromGUID2
#undef StringFromGUID2
#endif
#define StringFromGUID2 _afxOLE.pfnStringFromGUID2
#ifdef CoUninitialize
#undef CoUninitialize
#endif
#define CoUninitialize  _afxOLE.pfnCoUninitialize
#ifdef CoInitialize
#undef CoInitialize
#endif
#define CoInitialize    _afxOLE.pfnCoInitialize
#ifdef OleInitialize
#undef OleInitialize
#endif
#define OleInitialize   _afxOLE.pfnOleInitialize
#ifdef CoFreeUnusedLibraries
#undef CoFreeUnusedLibraries
#endif
#define CoFreeUnusedLibraries   _afxOLE.pfnCoFreeUnusedLibraries
#ifdef OleCreateFromData
#undef OleCreateFromData
#endif
#define OleCreateFromData   _afxOLE.pfnOleCreateFromData
#ifdef OleSetContainedObject
#undef OleSetContainedObject
#endif
#define OleSetContainedObject   _afxOLE.pfnOleSetContainedObject
#ifdef OleLockRunning
#undef OleLockRunning
#endif
#define OleLockRunning  _afxOLE.pfnOleLockRunning
#ifdef CoTaskMemAlloc
#undef CoTaskMemAlloc
#endif
#define CoTaskMemAlloc  _afxOLE.pfnCoTaskMemAlloc
#ifdef CLSIDFromString
#undef CLSIDFromString
#endif
#define CLSIDFromString _afxOLE.pfnCLSIDFromString
#ifdef CLSIDFromProgID
#undef CLSIDFromProgID
#endif
#define CLSIDFromProgID _afxOLE.pfnCLSIDFromProgID
#ifdef OleIsCurrentClipboard
#undef OleIsCurrentClipboard
#endif
#define OleIsCurrentClipboard   _afxOLE.pfnOleIsCurrentClipboard
#ifdef OleFlushClipboard
#undef OleFlushClipboard
#endif
#define OleFlushClipboard   _afxOLE.pfnOleFlushClipboard
#ifdef OleSetClipboard
#undef OleSetClipboard
#endif
#define OleSetClipboard _afxOLE.pfnOleSetClipboard
#ifdef OleIsRunning
#undef OleIsRunning
#endif
#define OleIsRunning    _afxOLE.pfnOleIsRunning
#ifdef OleRun
#undef OleRun
#endif
#define OleRun  _afxOLE.pfnOleRun
#ifdef OleGetClipboard
#undef OleGetClipboard
#endif
#define OleGetClipboard _afxOLE.pfnOleGetClipboard
#ifdef CoTreatAsClass
#undef CoTreatAsClass
#endif
#define CoTreatAsClass  _afxOLE.pfnCoTreatAsClass
#ifdef OleQueryCreateFromData
#undef OleQueryCreateFromData
#endif
#define OleQueryCreateFromData  _afxOLE.pfnOleQueryCreateFromData
#ifdef OleSetMenuDescriptor
#undef OleSetMenuDescriptor
#endif
#define OleSetMenuDescriptor    _afxOLE.pfnOleSetMenuDescriptor
#ifdef CreateItemMoniker
#undef CreateItemMoniker
#endif
#define CreateItemMoniker   _afxOLE.pfnCreateItemMoniker
#ifdef CreateGenericComposite
#undef CreateGenericComposite
#endif
#define CreateGenericComposite  _afxOLE.pfnCreateGenericComposite
#ifdef CreateStreamOnHGlobal
#undef CreateStreamOnHGlobal
#endif
#define CreateStreamOnHGlobal   _afxOLE.pfnCreateStreamOnHGlobal
#ifdef OleSaveToStream
#undef OleSaveToStream
#endif
#define OleSaveToStream _afxOLE.pfnOleSaveToStream
#ifdef WriteClassStm
#undef WriteClassStm
#endif
#define WriteClassStm   _afxOLE.pfnWriteClassStm
#ifdef CoTaskMemFree
#undef CoTaskMemFree
#endif
#define CoTaskMemFree   _afxOLE.pfnCoTaskMemFree
#ifdef OleGetIconOfClass
#undef OleGetIconOfClass
#endif
#define OleGetIconOfClass   _afxOLE.pfnOleGetIconOfClass
#ifdef ReleaseStgMedium
#undef ReleaseStgMedium
#endif
#define ReleaseStgMedium    _afxOLE.pfnReleaseStgMedium
#ifdef GetHGlobalFromILockBytes
#undef GetHGlobalFromILockBytes
#endif
#define GetHGlobalFromILockBytes    _afxOLE.pfnGetHGlobalFromILockBytes
#ifdef StgOpenStorageOnILockBytes
#undef StgOpenStorageOnILockBytes
#endif
#define StgOpenStorageOnILockBytes  _afxOLE.pfnStgOpenStorageOnILockBytes
#ifdef CreateILockBytesOnHGlobal
#undef CreateILockBytesOnHGlobal
#endif
#define CreateILockBytesOnHGlobal   _afxOLE.pfnCreateILockBytesOnHGlobal
#ifdef StgCreateDocfileOnILockBytes
#undef StgCreateDocfileOnILockBytes
#endif
#define StgCreateDocfileOnILockBytes    _afxOLE.pfnStgCreateDocfileOnILockBytes
#ifdef OleSave
#undef OleSave
#endif
#define OleSave _afxOLE.pfnOleSave
#ifdef OleLoad
#undef OleLoad
#endif
#define OleLoad _afxOLE.pfnOleLoad
#ifdef OleCreate
#undef OleCreate
#endif
#define OleCreate   _afxOLE.pfnOleCreate
#ifdef OleCreateLinkToFile
#undef OleCreateLinkToFile
#endif
#define OleCreateLinkToFile _afxOLE.pfnOleCreateLinkToFile
#ifdef OleCreateFromFile
#undef OleCreateFromFile
#endif
#define OleCreateFromFile   _afxOLE.pfnOleCreateFromFile
#ifdef OleCreateStaticFromData
#undef OleCreateStaticFromData
#endif
#define OleCreateStaticFromData _afxOLE.pfnOleCreateStaticFromData
#ifdef OleCreateLinkFromData
#undef OleCreateLinkFromData
#endif
#define OleCreateLinkFromData   _afxOLE.pfnOleCreateLinkFromData
#ifdef SetConvertStg
#undef SetConvertStg
#endif
#define SetConvertStg   _afxOLE.pfnSetConvertStg
#ifdef OleDuplicateData
#undef OleDuplicateData
#endif
#define OleDuplicateData    _afxOLE.pfnOleDuplicateData
#ifdef WriteFmtUserTypeStg
#undef WriteFmtUserTypeStg
#endif
#define WriteFmtUserTypeStg _afxOLE.pfnWriteFmtUserTypeStg
#ifdef OleRegGetMiscStatus
#undef OleRegGetMiscStatus
#endif
#define OleRegGetMiscStatus _afxOLE.pfnOleRegGetMiscStatus
#ifdef CoGetMalloc
#undef CoGetMalloc
#endif
#define CoGetMalloc _afxOLE.pfnCoGetMalloc
#ifdef StgIsStorageILockBytes
#undef StgIsStorageILockBytes
#endif
#define StgIsStorageILockBytes  _afxOLE.pfnStgIsStorageILockBytes

#ifdef _MAC

#ifdef RegEnumKey
#undef RegEnumKey
#endif
#define RegEnumKey _afxOLE.pfnRegEnumKey

#ifdef RegDeleteKey
#undef RegDeleteKey
#endif
#define RegDeleteKey _afxOLE.pfnRegDeleteKey

#ifdef RegDeleteValue
#undef RegDeleteValue
#endif
#define RegDeleteValue _afxOLE.pfnRegDeleteValue

#ifdef RegCloseKey
#undef RegCloseKey
#endif
#define RegCloseKey _afxOLE.pfnRegCloseKey

#ifdef RegSetValue
#undef RegSetValue
#endif
#define RegSetValue _afxOLE.pfnRegSetValue

#ifdef RegCreateKey
#undef RegCreateKey
#endif
#define RegCreateKey _afxOLE.pfnRegCreateKey

#ifdef RegSetValueEx
#undef RegSetValueEx
#endif
#define RegSetValueEx _afxOLE.pfnRegSetValueEx

#ifdef RegQueryValue
#undef RegQueryValue
#endif
#define RegQueryValue _afxOLE.pfnRegQueryValue

#ifdef RegOpenKey
#undef RegOpenKey
#endif
#define RegOpenKey _afxOLE.pfnRegOpenKey

#ifdef RegQueryValueEx
#undef RegQueryValueEx
#endif
#define RegQueryValueEx _afxOLE.pfnRegQueryValueEx

#endif //_MAC

// OLEAUT32.DLL mappings
#ifdef SysFreeString
#undef SysFreeString
#endif
#define SysFreeString               _afxOLE.pfnSysFreeString

#if !defined(_MAC)
#ifdef SysAllocStringByteLen
#undef SysAllocStringByteLen
#endif
#define SysAllocStringByteLen       _afxOLE.pfnSysAllocStringByteLen
#endif

#ifdef VariantCopy
#undef VariantCopy
#endif
#define VariantCopy                 _afxOLE.pfnVariantCopy
#ifdef VariantClear
#undef VariantClear
#endif
#define VariantClear                _afxOLE.pfnVariantClear
#ifdef VariantChangeType
#undef VariantChangeType
#endif
#define VariantChangeType           _afxOLE.pfnVariantChangeType
#ifdef SysAllocStringLen
#undef SysAllocStringLen
#endif
#define SysAllocStringLen           _afxOLE.pfnSysAllocStringLen
#ifdef SysStringLen
#undef SysStringLen
#endif
#define SysStringLen                _afxOLE.pfnSysStringLen
#ifdef SysReAllocStringLen
#undef SysReAllocStringLen
#endif
#define SysReAllocStringLen         _afxOLE.pfnSysReAllocStringLen
#ifdef SysAllocString
#undef SysAllocString
#endif
#define SysAllocString              _afxOLE.pfnSysAllocString

#if !defined(_MAC)
#ifdef SysStringByteLen
#undef SysStringByteLen
#endif
#define SysStringByteLen            _afxOLE.pfnSysStringByteLen
#endif

#ifdef VarCyFromStr
#undef VarCyFromStr
#endif
#define VarCyFromStr                _afxOLE.pfnVarCyFromStr
#ifdef VarBstrFromCy
#undef VarBstrFromCy
#endif
#define VarBstrFromCy               _afxOLE.pfnVarBstrFromCy
#ifdef VarDateFromStr
#undef VarDateFromStr
#endif
#define VarDateFromStr              _afxOLE.pfnVarDateFromStr
#ifdef VarBstrFromDate
#undef VarBstrFromDate
#endif
#define VarBstrFromDate             _afxOLE.pfnVarBstrFromDate
#ifdef LoadTypeLib
#undef LoadTypeLib
#endif
#define LoadTypeLib                 _afxOLE.pfnLoadTypeLib

#if defined(_MAC) && defined(MACOCX)
#ifdef LoadTypeLibFSp
#undef LoadTypeLibFSp
#endif
#define LoadTypeLibFSp              _afxOLE.pfnLoadTypeLibFSp
#ifdef RegisterTypeLibFolder
#undef RegisterTypeLibFolder
#endif
#define RegisterTypeLibFolder       _afxOLE.pfnRegisterTypeLibFolder
#ifdef QueryTypeLibFolder
#undef QueryTypeLibFolder
#endif
#define QueryTypeLibFolder          _afxOLE.pfnQueryTypeLibFolder
#ifdef GetUserDefaultLCID
#undef GetUserDefaultLCID
#endif
#define GetUserDefaultLCID          _afxOLE.pfnGetUserDefaultLCID
#endif //_MAC && MACOCX

#ifdef LoadRegTypeLib
#undef LoadRegTypeLib
#endif
#define LoadRegTypeLib              _afxOLE.pfnLoadRegTypeLib
#ifdef RegisterTypeLib
#undef RegisterTypeLib
#endif
#define RegisterTypeLib             _afxOLE.pfnRegisterTypeLib
#ifdef DosDateTimeToVariantTime
#undef DosDateTimeToVariantTime
#endif
#define DosDateTimeToVariantTime    _afxOLE.pfnDosDateTimeToVariantTime
#ifdef SafeArrayCreate
#undef SafeArrayCreate
#endif
#define SafeArrayCreate             _afxOLE.pfnSafeArrayCreate
#ifdef SafeArrayRedim
#undef SafeArrayRedim
#endif
#define SafeArrayRedim              _afxOLE.pfnSafeArrayRedim
#ifdef SafeArrayAccessData
#undef SafeArrayAccessData
#endif
#define SafeArrayAccessData         _afxOLE.pfnSafeArrayAccessData
#ifdef SafeArrayUnaccessData
#undef SafeArrayUnaccessData
#endif
#define SafeArrayUnaccessData       _afxOLE.pfnSafeArrayUnaccessData
#ifdef SafeArrayGetUBound
#undef SafeArrayGetUBound
#endif
#define SafeArrayGetUBound          _afxOLE.pfnSafeArrayGetUBound
#ifdef SafeArrayGetLBound
#undef SafeArrayGetLBound
#endif
#define SafeArrayGetLBound          _afxOLE.pfnSafeArrayGetLBound
#ifdef SafeArrayGetElemsize
#undef SafeArrayGetElemsize
#endif
#define SafeArrayGetElemsize        _afxOLE.pfnSafeArrayGetElemsize
#ifdef SafeArrayGetDim
#undef SafeArrayGetDim
#endif
#define SafeArrayGetDim             _afxOLE.pfnSafeArrayGetDim
#ifdef SafeArrayCopy
#undef SafeArrayCopy
#endif
#define SafeArrayCopy               _afxOLE.pfnSafeArrayCopy
#ifdef SafeArrayAllocData
#undef SafeArrayAllocData
#endif
#define SafeArrayAllocData          _afxOLE.pfnSafeArrayAllocData
#ifdef SafeArrayAllocDescriptor
#undef SafeArrayAllocDescriptor
#endif
#define SafeArrayAllocDescriptor    _afxOLE.pfnSafeArrayAllocDescriptor
#ifdef SafeArrayGetElement
#undef SafeArrayGetElement
#endif
#define SafeArrayGetElement         _afxOLE.pfnSafeArrayGetElement
#ifdef SafeArrayPutElement
#undef SafeArrayPutElement
#endif
#define SafeArrayPutElement         _afxOLE.pfnSafeArrayPutElement
#ifdef SafeArrayLock
#undef SafeArrayLock
#endif
#define SafeArrayLock               _afxOLE.pfnSafeArrayLock
#ifdef SafeArrayUnlock
#undef SafeArrayUnlock
#endif
#define SafeArrayUnlock             _afxOLE.pfnSafeArrayUnlock
#ifdef SafeArrayDestroy
#undef SafeArrayDestroy
#endif
#define SafeArrayDestroy            _afxOLE.pfnSafeArrayDestroy
#ifdef SafeArrayDestroyData
#undef SafeArrayDestroyData
#endif
#define SafeArrayDestroyData        _afxOLE.pfnSafeArrayDestroyData
#ifdef SafeArrayDestroyDescriptor
#undef SafeArrayDestroyDescriptor
#endif
#define SafeArrayDestroyDescriptor  _afxOLE.pfnSafeArrayDestroyDescriptor
#ifdef SafeArrayPtrOfIndex
#undef SafeArrayPtrOfIndex
#endif
#define SafeArrayPtrOfIndex         _afxOLE.pfnSafeArrayPtrOfIndex


// OLEDLG.DLL mappings
#ifdef OleUIAddVerbMenu
#undef OleUIAddVerbMenu
#endif
#define OleUIAddVerbMenu        _afxOLE.pfnOleUIAddVerbMenu
#ifdef OleUIBusy
#undef OleUIBusy
#endif
#define OleUIBusy               _afxOLE.pfnOleUIBusy
#ifdef OleUIChangeIcon
#undef OleUIChangeIcon
#endif
#define OleUIChangeIcon         _afxOLE.pfnOleUIChangeIcon
#ifdef OleUIChangeSource
#undef OleUIChangeSource
#endif
#define OleUIChangeSource       _afxOLE.pfnOleUIChangeSource
#ifdef OleUIConvert
#undef OleUIConvert
#endif
#define OleUIConvert            _afxOLE.pfnOleUIConvert
#ifdef OleUIEditLinks
#undef OleUIEditLinks
#endif
#define OleUIEditLinks          _afxOLE.pfnOleUIEditLinks
#ifdef OleUIInsertObject
#undef OleUIInsertObject
#endif
#define OleUIInsertObject       _afxOLE.pfnOleUIInsertObject
#ifdef OleUIObjectProperties
#undef OleUIObjectProperties
#endif
#define OleUIObjectProperties   _afxOLE.pfnOleUIObjectProperties
#ifdef OleUIPasteSpecial
#undef OleUIPasteSpecial
#endif
#define OleUIPasteSpecial       _afxOLE.pfnOleUIPasteSpecial
#ifdef OleUIUpdateLinks
#undef OleUIUpdateLinks
#endif
#define OleUIUpdateLinks        _afxOLE.pfnOleUIUpdateLinks

#endif //_AFXDLL

#undef AFX_DATA
#define AFX_DATA

/////////////////////////////////////////////////////////////////////////////
