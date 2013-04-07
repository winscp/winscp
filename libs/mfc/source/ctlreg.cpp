// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#include "stdafx.h"

#ifdef AFXCTL_FACT_SEG
#pragma code_seg(AFXCTL_FACT_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

#define GUID_CCH    39  // Characters in string form of guid, including '\0'

inline BOOL _AfxRegDeleteKeySucceeded(LONG error)
{
	return (error == ERROR_SUCCESS) || (error == ERROR_BADKEY) ||
		(error == ERROR_FILE_NOT_FOUND);
}

// Under Win32, a reg key may not be deleted unless it is empty.
// Thus, to delete a tree,  one must recursively enumerate and
// delete all of the sub-keys.

LONG AFXAPI _AfxRecursiveRegDeleteKey(HKEY hParentKey, LPTSTR szKeyName)
{
	// one implementation for everybody
	return AfxDelRegTreeHelper(hParentKey, szKeyName);
}

void _AfxUnregisterInterfaces(ITypeLib* pTypeLib)
{
	TCHAR szKey[128];
	lstrcpy(szKey, _T("Interface\\"));
	LPTSTR pszGuid = szKey + lstrlen(szKey);

	int cTypeInfo = pTypeLib->GetTypeInfoCount();

	for (int i = 0; i < cTypeInfo; i++)
	{
		TYPEKIND tk;
		if (SUCCEEDED(pTypeLib->GetTypeInfoType(i, &tk)) &&
			(tk == TKIND_DISPATCH || tk == TKIND_INTERFACE))
		{
			ITypeInfo* pTypeInfo = NULL;
			if (SUCCEEDED(pTypeLib->GetTypeInfo(i, &pTypeInfo)))
			{
				TYPEATTR* pTypeAttr;
				if (SUCCEEDED(pTypeInfo->GetTypeAttr(&pTypeAttr)))
				{
#if defined(_UNICODE) || defined(OLE2ANSI)
					StringFromGUID2(pTypeAttr->guid, pszGuid, GUID_CCH);
#else
					WCHAR wszGuid[39];
					StringFromGUID2(pTypeAttr->guid, wszGuid, GUID_CCH);
					_wcstombsz(pszGuid, wszGuid, GUID_CCH);
#endif
					_AfxRecursiveRegDeleteKey(HKEY_CLASSES_ROOT, szKey);
					pTypeInfo->ReleaseTypeAttr(pTypeAttr);
				}

				pTypeInfo->Release();
			}
		}
	}
}

BOOL AFXAPI AfxOleRegisterTypeLib(HINSTANCE hInstance, REFGUID tlid,
	LPCTSTR pszFileName, LPCTSTR pszHelpDir)
{
	USES_CONVERSION;

	BOOL bSuccess = FALSE;
	CString strPathName;
	TCHAR *szPathName = strPathName.GetBuffer(_MAX_PATH);
	::GetModuleFileName(hInstance, szPathName, _MAX_PATH);
	strPathName.ReleaseBuffer();
	LPTYPELIB ptlib = NULL;

	// If a filename was specified, replace final component of path with it.
	if (pszFileName != NULL)
	{
		int iBackslash = strPathName.ReverseFind('\\');
		if (iBackslash != -1)
			strPathName = strPathName.Left(iBackslash+1);
		strPathName += pszFileName;
	}

	if (SUCCEEDED(LoadTypeLib(T2COLE(strPathName), &ptlib)))
	{
		ASSERT_POINTER(ptlib, ITypeLib);

		LPTLIBATTR pAttr;
		GUID tlidActual = GUID_NULL;

		if (SUCCEEDED(ptlib->GetLibAttr(&pAttr)))
		{
			ASSERT_POINTER(pAttr, TLIBATTR);
			tlidActual = pAttr->guid;
			ptlib->ReleaseTLibAttr(pAttr);
		}

		// Check that the guid of the loaded type library matches
		// the tlid parameter.
		ASSERT(IsEqualGUID(tlid, tlidActual));

		if (IsEqualGUID(tlid, tlidActual))
		{
			// Register the type library.
			if (SUCCEEDED(RegisterTypeLib(ptlib,
					T2OLE((LPTSTR)(LPCTSTR)strPathName), T2OLE((LPTSTR)pszHelpDir))))
				bSuccess = TRUE;
		}

		RELEASE(ptlib);
	}
	else
	{
		TRACE1("Warning: Could not load type library from %s\n", (LPCTSTR)strPathName);
	}

	return bSuccess;
}

#define TYPELIBWIN   _T("win32")
#define TYPELIBWIN_2 _T("win16")

BOOL AFXAPI AfxOleUnregisterTypeLib(REFGUID tlid, WORD wVerMajor,
	WORD wVerMinor, LCID lcid)
{
	USES_CONVERSION;

	// Load type library before unregistering it.
	ITypeLib* pTypeLib = NULL;
	if (wVerMajor != 0)
	{
		if (FAILED(LoadRegTypeLib(tlid, wVerMajor, wVerMinor, lcid, &pTypeLib)))
			pTypeLib = NULL;
	}

	// Format typelib guid as a string
	OLECHAR szTypeLibID[GUID_CCH];
	int cchGuid = ::StringFromGUID2(tlid, szTypeLibID, GUID_CCH);

	ASSERT(cchGuid == GUID_CCH);    // Did StringFromGUID2 work?
	if (cchGuid != GUID_CCH)
		return FALSE;

	TCHAR szKeyTypeLib[_MAX_PATH];
	BOOL bSurgical = FALSE;
	LONG error = ERROR_SUCCESS;

	wsprintf(szKeyTypeLib, _T("TYPELIB\\%s"), OLE2CT(szTypeLibID));

	HKEY hKeyTypeLib;
	if (RegOpenKey(HKEY_CLASSES_ROOT, szKeyTypeLib, &hKeyTypeLib) ==
		ERROR_SUCCESS)
	{
		int iKeyVersion = 0;
		HKEY hKeyVersion;
		TCHAR szVersion[_MAX_PATH];

		// Iterate through all installed versions of the control

		while (RegEnumKey(hKeyTypeLib, iKeyVersion, szVersion, _MAX_PATH) ==
			ERROR_SUCCESS)
		{
			hKeyVersion = NULL;
			BOOL bSurgicalVersion = FALSE;

			if (RegOpenKey(hKeyTypeLib, szVersion, &hKeyVersion) !=
				ERROR_SUCCESS)
			{
				++iKeyVersion;
				continue;
			}

			int iKeyLocale = 0;
			HKEY hKeyLocale;
			TCHAR szLocale[_MAX_PATH];

			// Iterate through all registered locales for this version

			while (RegEnumKey(hKeyVersion, iKeyLocale, szLocale, _MAX_PATH) ==
				ERROR_SUCCESS)
			{
				// Don't remove HELPDIR or FLAGS keys.
				if ((lstrcmpi(szLocale, _T("HELPDIR")) == 0) ||
					(lstrcmpi(szLocale, _T("FLAGS")) == 0))
				{
					++iKeyLocale;
					continue;
				}

				hKeyLocale = NULL;

				if (RegOpenKey(hKeyVersion, szLocale, &hKeyLocale) !=
					ERROR_SUCCESS)
				{
					++iKeyLocale;
					continue;
				}

				// Check if a 16-bit key is found when unregistering 32-bit
				HKEY hkey;
				if (RegOpenKey(hKeyLocale, TYPELIBWIN_2, &hkey) ==
					ERROR_SUCCESS)
				{
					RegCloseKey(hkey);

					// Only remove the keys specific to the 32-bit version
					// of control, leaving things intact for 16-bit version.
					error = _AfxRecursiveRegDeleteKey(hKeyLocale, TYPELIBWIN);
					bSurgicalVersion = TRUE;
					RegCloseKey(hKeyLocale);
				}
				else
				{
					// Delete everything for this locale.
					RegCloseKey(hKeyLocale);
					if (_AfxRecursiveRegDeleteKey(hKeyVersion, szLocale) ==
						ERROR_SUCCESS)
					{
						// Start over again, so we don't skip anything.
						iKeyLocale = 0;
						continue;
					}
				}
				++iKeyLocale;
			}
			RegCloseKey(hKeyVersion);

			if (bSurgicalVersion)
			{
				bSurgical = TRUE;
			}
			else
			{
				if (_AfxRecursiveRegDeleteKey(hKeyTypeLib, szVersion) ==
					ERROR_SUCCESS)
				{
					// Start over again, to make sure we don't skip anything.
					iKeyVersion = 0;
					continue;
				}
			}

			++iKeyVersion;
		}
		RegCloseKey(hKeyTypeLib);
	}

	if (!bSurgical)
		error = _AfxRecursiveRegDeleteKey(HKEY_CLASSES_ROOT, szKeyTypeLib);

	if (_AfxRegDeleteKeySucceeded(error))
	{
		// If type library was unregistered successfully, then also unregister
		// interfaces.
		if (pTypeLib != NULL)
		{
			ITypeLib* pDummy = NULL;
			if (FAILED(LoadRegTypeLib(tlid, wVerMajor, wVerMinor, lcid, &pDummy)))
				_AfxUnregisterInterfaces(pTypeLib);
			else
				pDummy->Release();

			pTypeLib->Release();
		}
	}

	return _AfxRegDeleteKeySucceeded(error);
}

AFX_STATIC_DATA const LPCTSTR _afxCtrlProgID[] =
{
	_T("\0") _T("%1"),
	_T("CLSID\0") _T("%2"),
	NULL
};

#define INPROCSERVER   _T("InprocServer32")
#define INPROCSERVER_2 _T("InprocServer")
#define TOOLBOXBITMAP  _T("ToolboxBitmap32")

AFX_STATIC_DATA const LPCTSTR _afxCtrlClassID[] =
{
	_T("\0") _T("%1"),
	_T("ProgID\0") _T("%2"),
	INPROCSERVER _T("\0%3"),
	TOOLBOXBITMAP _T("\0%3, %4"),
	_T("MiscStatus\0") _T("0"),
	_T("MiscStatus\\1\0") _T("%5"),
	_T("Control\0") _T(""),
	_T("TypeLib\0") _T("%6"),
	_T("Version\0") _T("%7"),
	NULL
};

BOOL AFXAPI AfxOleRegisterControlClass(HINSTANCE hInstance,
	REFCLSID clsid, LPCTSTR pszProgID, UINT idTypeName, UINT idBitmap,
	int nRegFlags, DWORD dwMiscStatus, REFGUID tlid, WORD wVerMajor,
	WORD wVerMinor)
{
	USES_CONVERSION;

	BOOL bSuccess = FALSE;

	// Format class ID as a string
	OLECHAR szClassID[GUID_CCH];
	int cchGuid = ::StringFromGUID2(clsid, szClassID, GUID_CCH);
	LPCTSTR lpszClassID = OLE2CT(szClassID);

	ASSERT(cchGuid == GUID_CCH);    // Did StringFromGUID2 work?
	if (cchGuid != GUID_CCH)
		return FALSE;

	// Format typelib guid as a string
	OLECHAR szTypeLibID[GUID_CCH];
	cchGuid = ::StringFromGUID2(tlid, szTypeLibID, GUID_CCH);

	ASSERT(cchGuid == GUID_CCH);    // Did StringFromGUID2 work?
	if (cchGuid != GUID_CCH)
		return FALSE;

	CString strPathName;
	AfxGetModuleShortFileName(hInstance, strPathName);

	CString strTypeName;
	if (!strTypeName.LoadString(idTypeName))
	{
		ASSERT(FALSE);  // Name string not present in resources
		strTypeName = lpszClassID; // Use Class ID instead
	}

	TCHAR szBitmapID[_MAX_PATH];
	_itot(idBitmap, szBitmapID, 10);

	TCHAR szMiscStatus[_MAX_PATH];
	_ltot(dwMiscStatus, szMiscStatus, 10);

	// Format version string as "major.minor"
	TCHAR szVersion[_MAX_PATH];
	wsprintf(szVersion, _T("%d.%d"), wVerMajor, wVerMinor);

	// Attempt to open registry keys.
	HKEY hkeyClassID = NULL;
	HKEY hkeyProgID = NULL;

	TCHAR szScratch[_MAX_PATH];
	wsprintf(szScratch, _T("CLSID\\%s"), lpszClassID);
	if (::RegCreateKey(HKEY_CLASSES_ROOT, szScratch, &hkeyClassID) !=
		ERROR_SUCCESS)
		goto Error;
	if (::RegCreateKey(HKEY_CLASSES_ROOT, pszProgID, &hkeyProgID) !=
		ERROR_SUCCESS)
		goto Error;

	ASSERT(hkeyClassID != NULL);
	ASSERT(hkeyProgID != NULL);

	LPCTSTR rglpszSymbols[7];
	rglpszSymbols[0] = strTypeName;
	rglpszSymbols[1] = lpszClassID;
	bSuccess = AfxOleRegisterHelper(_afxCtrlProgID, rglpszSymbols, 2,
		TRUE, hkeyProgID);

	if (!bSuccess)
		goto Error;

	rglpszSymbols[1] = pszProgID;
	rglpszSymbols[2] = strPathName;
	rglpszSymbols[3] = szBitmapID;
	rglpszSymbols[4] = szMiscStatus;
	rglpszSymbols[5] = OLE2CT(szTypeLibID);
	rglpszSymbols[6] = szVersion;
	bSuccess = AfxOleRegisterHelper(_afxCtrlClassID, rglpszSymbols, 7,
		TRUE, hkeyClassID);

	if (!bSuccess)
		goto Error;

	if (nRegFlags & afxRegInsertable)
	{
		bSuccess =
			(::RegSetValue(hkeyProgID, _T("Insertable"), REG_SZ, _T(""), 0) ==
				ERROR_SUCCESS) &&
			(::RegSetValue(hkeyClassID, _T("Insertable"), REG_SZ, _T(""), 0) ==
				ERROR_SUCCESS);
	}

	if (nRegFlags & afxRegApartmentThreading)
	{
		HKEY hkeyInprocServer32;
		bSuccess = (::RegOpenKey(hkeyClassID, INPROCSERVER,
			&hkeyInprocServer32) == ERROR_SUCCESS);
		if (!bSuccess)
			goto Error;
		ASSERT(hkeyInprocServer32 != NULL);
		static TCHAR szApartment[] = _T("Apartment");
		bSuccess = (::RegSetValueEx(hkeyInprocServer32, _T("ThreadingModel"), 0,
			REG_SZ, (const BYTE*)szApartment, (lstrlen(szApartment)+1) * sizeof(TCHAR)) ==
			ERROR_SUCCESS);
		::RegCloseKey(hkeyInprocServer32);
	}

Error:
	if (hkeyProgID != NULL)
		::RegCloseKey(hkeyProgID);

	if (hkeyClassID != NULL)
		::RegCloseKey(hkeyClassID);

	return bSuccess;
}

BOOL AFXAPI AfxOleUnregisterClass(REFCLSID clsid, LPCTSTR pszProgID)
{
	USES_CONVERSION;

	// Format class ID as a string
	OLECHAR szClassID[GUID_CCH];
	int cchGuid = ::StringFromGUID2(clsid, szClassID, GUID_CCH);
	LPCTSTR lpszClassID = OLE2CT(szClassID);

	ASSERT(cchGuid == GUID_CCH);    // Did StringFromGUID2 work?
	if (cchGuid != GUID_CCH)
		return FALSE;

	TCHAR szKey[_MAX_PATH];
	long error;
	BOOL bRetCode = TRUE;

	// check to see if a 16-bit InprocServer key is found when unregistering
	// 32-bit (or vice versa).
	wsprintf(szKey, _T("CLSID\\%s\\%s"), lpszClassID, INPROCSERVER_2);
	HKEY hkey;
	BOOL bSurgical = RegOpenKey(HKEY_CLASSES_ROOT, szKey, &hkey) ==
		ERROR_SUCCESS;

	if (bSurgical)
	{
		// Only remove the keys specific to this version of the control,
		// leaving things in tact for the other version.
		wsprintf(szKey, _T("CLSID\\%s\\%s"), lpszClassID, INPROCSERVER);
		error = RegDeleteKey(HKEY_CLASSES_ROOT, szKey);
		bRetCode = bRetCode && _AfxRegDeleteKeySucceeded(error);

		wsprintf(szKey, _T("CLSID\\%s\\%s"), lpszClassID, TOOLBOXBITMAP);
		error = RegDeleteKey(HKEY_CLASSES_ROOT, szKey);
		bRetCode = bRetCode && _AfxRegDeleteKeySucceeded(error);
	}
	else
	{
		// No other versions of this control were detected,
		// so go ahead and remove the control completely.
		wsprintf(szKey, _T("CLSID\\%s"), lpszClassID);
		error = _AfxRecursiveRegDeleteKey(HKEY_CLASSES_ROOT, szKey);
		bRetCode = bRetCode && _AfxRegDeleteKeySucceeded(error);

		if (pszProgID != NULL)
		{
			error = _AfxRecursiveRegDeleteKey(HKEY_CLASSES_ROOT,
				(LPTSTR)pszProgID);
			bRetCode = bRetCode && _AfxRegDeleteKeySucceeded(error);
		}
	}

	return bRetCode;
}

AFX_STATIC_DATA const LPCTSTR _afxPropPageClass[] =
{
	_T("\0") _T("%1"),
	INPROCSERVER _T("\0%2"),
	NULL
};

BOOL AFXAPI AfxOleRegisterPropertyPageClass(HINSTANCE hInstance,
	REFCLSID clsid, UINT idTypeName)
{
	return AfxOleRegisterPropertyPageClass(hInstance, clsid, idTypeName, 0);
}

BOOL AFXAPI AfxOleRegisterPropertyPageClass(HINSTANCE hInstance,
	REFCLSID clsid, UINT idTypeName, int nRegFlags)
{
	ASSERT(!(nRegFlags & afxRegInsertable));    // can't be insertable

	USES_CONVERSION;

	BOOL bSuccess = FALSE;

	// Format class ID as a string
	OLECHAR szClassID[GUID_CCH];
	int cchGuid = ::StringFromGUID2(clsid, szClassID, GUID_CCH);
	LPCTSTR lpszClassID = OLE2CT(szClassID);

	ASSERT(cchGuid == GUID_CCH);    // Did StringFromGUID2 work?
	if (cchGuid != GUID_CCH)
		return FALSE;

	CString strPathName;
	AfxGetModuleShortFileName(hInstance, strPathName);

	CString strTypeName;
	if (!strTypeName.LoadString(idTypeName))
	{
		ASSERT(FALSE);  // Name string not present in resources
		strTypeName = lpszClassID; // Use Class ID instead
	}

	HKEY hkeyClassID = NULL;

	TCHAR szKey[_MAX_PATH];
	wsprintf(szKey, _T("CLSID\\%s"), lpszClassID);
	if (::RegCreateKey(HKEY_CLASSES_ROOT, szKey, &hkeyClassID) !=
		ERROR_SUCCESS)
		goto Error;

	LPCTSTR rglpszSymbols[2];
	rglpszSymbols[0] = strTypeName;
	rglpszSymbols[1] = strPathName;
	bSuccess = AfxOleRegisterHelper(_afxPropPageClass, rglpszSymbols,
		2, TRUE, hkeyClassID);

	if (!bSuccess)
		goto Error;

	if (nRegFlags & afxRegApartmentThreading)
	{
		HKEY hkeyInprocServer32;
		bSuccess = (::RegOpenKey(hkeyClassID, INPROCSERVER,
			&hkeyInprocServer32) == ERROR_SUCCESS);
		if (!bSuccess)
			goto Error;
		ASSERT(hkeyInprocServer32 != NULL);
		static TCHAR szApartment[] = _T("Apartment");
		bSuccess = (::RegSetValueEx(hkeyInprocServer32, _T("ThreadingModel"), 0,
			REG_SZ, (const BYTE*)szApartment, (lstrlen(szApartment)+1) * sizeof(TCHAR)) ==
			ERROR_SUCCESS);
		::RegCloseKey(hkeyInprocServer32);
	}

Error:
	if (hkeyClassID != NULL)
		::RegCloseKey(hkeyClassID);

	return bSuccess;
}

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif
