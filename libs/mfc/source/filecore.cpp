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
#include <winnetwk.h>
#include <shlobj.h>
#include <shellapi.h>

#ifdef AFX_CORE1_SEG
#pragma code_seg(AFX_CORE1_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

AFX_STATIC inline BOOL IsDirSep(TCHAR ch)
{
	return (ch == '\\' || ch == '/');
}

#ifndef _AFX_NO_OLE_SUPPORT

#undef DEFINE_GUID

#define DEFINE_GUID(name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8) \
		EXTERN_C AFX_COMDAT const GUID afx##name \
				= { l, w1, w2, { b1, b2,  b3,  b4,  b5,  b6,  b7,  b8 } }

#define DEFINE_SHLGUID(name, l, w1, w2) DEFINE_GUID(name, l, w1, w2, 0xC0,0,0,0,0,0,0,0x46)

DEFINE_SHLGUID(CLSID_ShellLink, 0x00021401L, 0, 0);
#ifndef _UNICODE
DEFINE_SHLGUID(IID_IShellLinkA, 0x000214EEL, 0, 0);
#else
DEFINE_SHLGUID(IID_IShellLinkW, 0x000214F9L, 0, 0);
#endif
#define CLSID_ShellLink afxCLSID_ShellLink

#undef IID_IShellLink
#undef IShellLink
#ifndef _UNICODE
#define IID_IShellLink afxIID_IShellLinkA
#define IShellLink IShellLinkA
#else
#define IID_IShellLink afxIID_IShellLinkW
#define IShellLink IShellLinkW
#endif

#endif !_AFX_NO_OLE_SUPPORT

////////////////////////////////////////////////////////////////////////////
// CFile implementation

CFile::CFile()
{
	m_hFile = (UINT) hFileNull;
	m_bCloseOnDelete = FALSE;
}

CFile::CFile(int hFile)
{
	m_hFile = hFile;
	m_bCloseOnDelete = FALSE;
}

CFile::CFile(LPCTSTR lpszFileName, UINT nOpenFlags)
{
	ASSERT(AfxIsValidString(lpszFileName));

	CFileException e;
	if (!Open(lpszFileName, nOpenFlags, &e))
		AfxThrowFileException(e.m_cause, e.m_lOsError, e.m_strFileName);
}

CFile::~CFile()
{
	if (m_hFile != (UINT)hFileNull && m_bCloseOnDelete)
		Close();
}

CFile* CFile::Duplicate() const
{
	ASSERT_VALID(this);
	ASSERT(m_hFile != (UINT)hFileNull);

	CFile* pFile = new CFile(hFileNull);
	HANDLE hFile;
	if (!::DuplicateHandle(::GetCurrentProcess(), (HANDLE)m_hFile,
		::GetCurrentProcess(), &hFile, 0, FALSE, DUPLICATE_SAME_ACCESS))
	{
		delete pFile;
		CFileException::ThrowOsError((LONG)::GetLastError());
	}
	pFile->m_hFile = (UINT)hFile;
	ASSERT(pFile->m_hFile != (UINT)hFileNull);
	pFile->m_bCloseOnDelete = m_bCloseOnDelete;
	return pFile;
}

BOOL CFile::Open(LPCTSTR lpszFileName, UINT nOpenFlags,
	CFileException* pException)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidString(lpszFileName));
	ASSERT(pException == NULL ||
		AfxIsValidAddress(pException, sizeof(CFileException)));
	ASSERT((nOpenFlags & typeText) == 0);   // text mode not supported

	// CFile objects are always binary and CreateFile does not need flag
	nOpenFlags &= ~(UINT)typeBinary;

	m_bCloseOnDelete = FALSE;
	m_hFile = (UINT)hFileNull;
	m_strFileName.Empty();

	TCHAR szTemp[_MAX_PATH];
	AfxFullPath(szTemp, lpszFileName);
	m_strFileName = szTemp;

	ASSERT(sizeof(HANDLE) == sizeof(UINT));
	ASSERT(shareCompat == 0);

	// map read/write mode
	ASSERT((modeRead|modeWrite|modeReadWrite) == 3);
	DWORD dwAccess = 0;
	switch (nOpenFlags & 3)
	{
	case modeRead:
		dwAccess = GENERIC_READ;
		break;
	case modeWrite:
		dwAccess = GENERIC_WRITE;
		break;
	case modeReadWrite:
		dwAccess = GENERIC_READ|GENERIC_WRITE;
		break;
	default:
		ASSERT(FALSE);  // invalid share mode
	}

	// map share mode
	DWORD dwShareMode = 0;
	switch (nOpenFlags & 0x70)    // map compatibility mode to exclusive
	{
	default:
		ASSERT(FALSE);  // invalid share mode?
	case shareCompat:
	case shareExclusive:
		dwShareMode = 0;
		break;
	case shareDenyWrite:
		dwShareMode = FILE_SHARE_READ;
		break;
	case shareDenyRead:
		dwShareMode = FILE_SHARE_WRITE;
		break;
	case shareDenyNone:
		dwShareMode = FILE_SHARE_WRITE|FILE_SHARE_READ;
		break;
	}

	// Note: typeText and typeBinary are used in derived classes only.

	// map modeNoInherit flag
	SECURITY_ATTRIBUTES sa;
	sa.nLength = sizeof(sa);
	sa.lpSecurityDescriptor = NULL;
	sa.bInheritHandle = (nOpenFlags & modeNoInherit) == 0;

	// map creation flags
	DWORD dwCreateFlag;
	if (nOpenFlags & modeCreate)
	{
		if (nOpenFlags & modeNoTruncate)
			dwCreateFlag = OPEN_ALWAYS;
		else
			dwCreateFlag = CREATE_ALWAYS;
	}
	else
		dwCreateFlag = OPEN_EXISTING;

	// attempt file creation
	HANDLE hFile = ::CreateFile(lpszFileName, dwAccess, dwShareMode, &sa,
		dwCreateFlag, FILE_ATTRIBUTE_NORMAL, NULL);
	if (hFile == INVALID_HANDLE_VALUE)
	{
		if (pException != NULL)
		{
			pException->m_lOsError = ::GetLastError();
			pException->m_cause =
				CFileException::OsErrorToException(pException->m_lOsError);

			// use passed file name (not expanded vesion) when reporting
			// an error while opening

			pException->m_strFileName = lpszFileName;
		}
		return FALSE;
	}
	m_hFile = (HFILE)hFile;
	m_bCloseOnDelete = TRUE;

	return TRUE;
}

UINT CFile::Read(void* lpBuf, UINT nCount)
{
	ASSERT_VALID(this);
	ASSERT(m_hFile != (UINT)hFileNull);

	if (nCount == 0)
		return 0;   // avoid Win32 "null-read"

	ASSERT(lpBuf != NULL);
	ASSERT(AfxIsValidAddress(lpBuf, nCount));

	DWORD dwRead;
	if (!::ReadFile((HANDLE)m_hFile, lpBuf, nCount, &dwRead, NULL))
		CFileException::ThrowOsError((LONG)::GetLastError());

	return (UINT)dwRead;
}

void CFile::Write(const void* lpBuf, UINT nCount)
{
	ASSERT_VALID(this);
	ASSERT(m_hFile != (UINT)hFileNull);

	if (nCount == 0)
		return;     // avoid Win32 "null-write" option

	ASSERT(lpBuf != NULL);
	ASSERT(AfxIsValidAddress(lpBuf, nCount, FALSE));

	DWORD nWritten;
	if (!::WriteFile((HANDLE)m_hFile, lpBuf, nCount, &nWritten, NULL))
		CFileException::ThrowOsError((LONG)::GetLastError(), m_strFileName);

	// Win32s will not return an error all the time (usually DISK_FULL)
	if (nWritten != nCount)
		AfxThrowFileException(CFileException::diskFull, -1, m_strFileName);
}

LONG CFile::Seek(LONG lOff, UINT nFrom)
{
	ASSERT_VALID(this);
	ASSERT(m_hFile != (UINT)hFileNull);
	ASSERT(nFrom == begin || nFrom == end || nFrom == current);
	ASSERT(begin == FILE_BEGIN && end == FILE_END && current == FILE_CURRENT);

	DWORD dwNew = ::SetFilePointer((HANDLE)m_hFile, lOff, NULL, (DWORD)nFrom);
	if (dwNew  == (DWORD)-1)
		CFileException::ThrowOsError((LONG)::GetLastError());

	return dwNew;
}

DWORD CFile::GetPosition() const
{
	ASSERT_VALID(this);
	ASSERT(m_hFile != (UINT)hFileNull);

	DWORD dwPos = ::SetFilePointer((HANDLE)m_hFile, 0, NULL, FILE_CURRENT);
	if (dwPos  == (DWORD)-1)
		CFileException::ThrowOsError((LONG)::GetLastError());

	return dwPos;
}

void CFile::Flush()
{
	ASSERT_VALID(this);

	if (m_hFile == (UINT)hFileNull)
		return;

	if (!::FlushFileBuffers((HANDLE)m_hFile))
		CFileException::ThrowOsError((LONG)::GetLastError());
}

void CFile::Close()
{
	ASSERT_VALID(this);
	ASSERT(m_hFile != (UINT)hFileNull);

	BOOL bError = FALSE;
	if (m_hFile != (UINT)hFileNull)
		bError = !::CloseHandle((HANDLE)m_hFile);

	m_hFile = (UINT) hFileNull;
	m_bCloseOnDelete = FALSE;
	m_strFileName.Empty();

	if (bError)
		CFileException::ThrowOsError((LONG)::GetLastError());
}

void CFile::Abort()
{
	ASSERT_VALID(this);
	if (m_hFile != (UINT)hFileNull)
	{
		// close but ignore errors
		::CloseHandle((HANDLE)m_hFile);
		m_hFile = (UINT)hFileNull;
	}
	m_strFileName.Empty();
}

void CFile::LockRange(DWORD dwPos, DWORD dwCount)
{
	ASSERT_VALID(this);
	ASSERT(m_hFile != (UINT)hFileNull);

	if (!::LockFile((HANDLE)m_hFile, dwPos, 0, dwCount, 0))
		CFileException::ThrowOsError((LONG)::GetLastError());
}

void CFile::UnlockRange(DWORD dwPos, DWORD dwCount)
{
	ASSERT_VALID(this);
	ASSERT(m_hFile != (UINT)hFileNull);

	if (!::UnlockFile((HANDLE)m_hFile, dwPos, 0, dwCount, 0))
		CFileException::ThrowOsError((LONG)::GetLastError());
}

void CFile::SetLength(DWORD dwNewLen)
{
	ASSERT_VALID(this);
	ASSERT(m_hFile != (UINT)hFileNull);

	Seek((LONG)dwNewLen, (UINT)begin);

	if (!::SetEndOfFile((HANDLE)m_hFile))
		CFileException::ThrowOsError((LONG)::GetLastError());
}

DWORD CFile::GetLength() const
{
	ASSERT_VALID(this);

	DWORD dwLen, dwCur;

	// Seek is a non const operation
	CFile* pFile = (CFile*)this;
	dwCur = pFile->Seek(0L, current);
	dwLen = pFile->SeekToEnd();
	VERIFY(dwCur == (DWORD)pFile->Seek(dwCur, begin));

	return dwLen;
}

// CFile does not support direct buffering (CMemFile does)
UINT CFile::GetBufferPtr(UINT nCommand, UINT /*nCount*/,
	void** /*ppBufStart*/, void** /*ppBufMax*/)
{
	ASSERT(nCommand == bufferCheck);
	UNUSED(nCommand);    // not used in retail build

	return 0;   // no support
}

void PASCAL CFile::Rename(LPCTSTR lpszOldName, LPCTSTR lpszNewName)
{
	if (!::MoveFile((LPTSTR)lpszOldName, (LPTSTR)lpszNewName))
		CFileException::ThrowOsError((LONG)::GetLastError());
}

void PASCAL CFile::Remove(LPCTSTR lpszFileName)
{
	if (!::DeleteFile((LPTSTR)lpszFileName))
		CFileException::ThrowOsError((LONG)::GetLastError());
}


/////////////////////////////////////////////////////////////////////////////
// CFile implementation helpers

#ifdef AfxGetFileName
#undef AfxGetFileName
#endif

#ifndef _AFX_NO_OLE_SUPPORT

HRESULT AFX_COM::CreateInstance(REFCLSID rclsid, LPUNKNOWN pUnkOuter,
	REFIID riid, LPVOID* ppv)
{
	// find the object's class factory
	LPCLASSFACTORY pf = NULL;
	HRESULT hRes = GetClassObject(rclsid, IID_IClassFactory, (LPVOID*)&pf);
	if (FAILED(hRes))
		return hRes;

	// call it to create the instance
	ASSERT(pf != NULL);
	hRes = pf->CreateInstance(pUnkOuter, riid, ppv);

	// let go of the factory
	pf->Release();
	return hRes;
}

HRESULT AFX_COM::GetClassObject(REFCLSID rclsid, REFIID riid, LPVOID* ppv)
{
	*ppv = NULL;
	HINSTANCE hInst = NULL;

	// find server name for this class ID

	CString strCLSID = AfxStringFromCLSID(rclsid);
	CString strServer;
	if (!AfxGetInProcServer(strCLSID, strServer))
		return REGDB_E_CLASSNOTREG;

	// try to load it
	hInst = LoadLibrary(strServer);
	if (hInst == NULL)
		return REGDB_E_CLASSNOTREG;

#pragma warning(disable:4191)
	// get its entry point
	HRESULT (STDAPICALLTYPE* pfn)(REFCLSID rclsid, REFIID riid, LPVOID* ppv);
	pfn = (HRESULT (STDAPICALLTYPE*)(REFCLSID rclsid, REFIID riid, LPVOID* ppv))
		GetProcAddress(hInst, "DllGetClassObject");
#pragma warning(default:4191)

	// call it, if it worked
	if (pfn != NULL)
		return pfn(rclsid, riid, ppv);
	return CO_E_ERRORINDLL;
}

CString AFXAPI AfxStringFromCLSID(REFCLSID rclsid)
{
	TCHAR szCLSID[256];
	wsprintf(szCLSID, _T("{%08X-%04X-%04X-%02X%02X-%02X%02X%02X%02X%02X%02X}"),
		rclsid.Data1, rclsid.Data2, rclsid.Data3,
		rclsid.Data4[0], rclsid.Data4[1], rclsid.Data4[2], rclsid.Data4[3],
		rclsid.Data4[4], rclsid.Data4[5], rclsid.Data4[6], rclsid.Data4[7]);
	return szCLSID;
}

BOOL AFXAPI AfxGetInProcServer(LPCTSTR lpszCLSID, CString& str)
{
	HKEY hKey = NULL;
	BOOL b = FALSE;
	if (RegOpenKey(HKEY_CLASSES_ROOT, _T("CLSID"), &hKey) == ERROR_SUCCESS)
	{
		HKEY hKeyCLSID = NULL;
		if (RegOpenKey(hKey, lpszCLSID, &hKeyCLSID) == ERROR_SUCCESS)
		{
			HKEY hKeyInProc = NULL;
			if (RegOpenKey(hKeyCLSID, _T("InProcServer32"), &hKeyInProc) ==
				ERROR_SUCCESS)
			{
				LPTSTR lpsz = str.GetBuffer(_MAX_PATH);
				DWORD dwSize = _MAX_PATH * sizeof(TCHAR);
				DWORD dwType;
				LONG lRes = ::RegQueryValueEx(hKeyInProc, _T(""),
					NULL, &dwType, (BYTE*)lpsz, &dwSize);
				str.ReleaseBuffer();
				b = (lRes == ERROR_SUCCESS);
				RegCloseKey(hKeyInProc);
			}
			RegCloseKey(hKeyCLSID);
		}
		RegCloseKey(hKey);
	}
	return b;
}
#endif  //!_AFX_NO_OLE_SUPPORT


BOOL AFXAPI AfxResolveShortcut(CWnd* pWnd, LPCTSTR lpszFileIn,
	LPTSTR lpszFileOut, int cchPath)
{
	USES_CONVERSION;
	AFX_COM com;
	IShellLink* psl;
	*lpszFileOut = 0;   // assume failure

	SHFILEINFO info;
	if ((SHGetFileInfo(lpszFileIn, 0, &info, sizeof(info),
		SHGFI_ATTRIBUTES) == 0) || !(info.dwAttributes & SFGAO_LINK))
	{
		return FALSE;
	}

	if (FAILED(com.CreateInstance(CLSID_ShellLink, NULL, IID_IShellLink,
		(LPVOID*)&psl)))
	{
		return FALSE;
	}

	IPersistFile *ppf;
	if (SUCCEEDED(psl->QueryInterface(IID_IPersistFile, (LPVOID*)&ppf)))
	{
		if (SUCCEEDED(ppf->Load(T2COLE(lpszFileIn), STGM_READ)))
		{
			/* Resolve the link, this may post UI to find the link */
			if (SUCCEEDED(psl->Resolve(pWnd->GetSafeHwnd(),
				SLR_ANY_MATCH)))
			{
				psl->GetPath(lpszFileOut, cchPath, NULL, 0);
				ppf->Release();
				psl->Release();
				return TRUE;
			}
		}
		ppf->Release();
	}
	psl->Release();
	return FALSE;
}

// turn a file, relative path or other into an absolute path
BOOL AFXAPI AfxFullPath(LPTSTR lpszPathOut, LPCTSTR lpszFileIn)
	// lpszPathOut = buffer of _MAX_PATH
	// lpszFileIn = file, relative path or absolute path
	// (both in ANSI character set)
{
	ASSERT(AfxIsValidAddress(lpszPathOut, _MAX_PATH));

	// first, fully qualify the path name
	LPTSTR lpszFilePart;
	if (!GetFullPathName(lpszFileIn, _MAX_PATH, lpszPathOut, &lpszFilePart))
	{
#ifdef _DEBUG
		if (lpszFileIn[0] != '\0')
			TRACE1("Warning: could not parse the path '%s'.\n", lpszFileIn);
#endif
		lstrcpyn(lpszPathOut, lpszFileIn, _MAX_PATH); // take it literally
		return FALSE;
	}

	CString strRoot;
	// determine the root name of the volume
	AfxGetRoot(lpszPathOut, strRoot);

	// get file system information for the volume
	DWORD dwFlags, dwDummy;
	if (!GetVolumeInformation(strRoot, NULL, 0, NULL, &dwDummy, &dwFlags,
		NULL, 0))
	{
		TRACE1("Warning: could not get volume information '%s'.\n",
			(LPCTSTR)strRoot);
		return FALSE;   // preserving case may not be correct
	}

	// not all characters have complete uppercase/lowercase
	if (!(dwFlags & FS_CASE_IS_PRESERVED))
		CharUpper(lpszPathOut);

	// assume non-UNICODE file systems, use OEM character set
	if (!(dwFlags & FS_UNICODE_STORED_ON_DISK))
	{
		WIN32_FIND_DATA data;
		HANDLE h = FindFirstFile(lpszFileIn, &data);
		if (h != INVALID_HANDLE_VALUE)
		{
			FindClose(h);
			lstrcpy(lpszFilePart, data.cFileName);
		}
	}
	return TRUE;
}

void AFXAPI AfxGetRoot(LPCTSTR lpszPath, CString& strRoot)
{
	ASSERT(lpszPath != NULL);
	// determine the root name of the volume
	LPTSTR lpszRoot = strRoot.GetBuffer(_MAX_PATH);
	memset(lpszRoot, 0, _MAX_PATH);
	lstrcpyn(lpszRoot, lpszPath, _MAX_PATH);
	for (LPTSTR lpsz = lpszRoot; *lpsz != '\0'; lpsz = _tcsinc(lpsz))
	{
		// find first double slash and stop
		if (IsDirSep(lpsz[0]) && IsDirSep(lpsz[1]))
			break;
	}
	if (*lpsz != '\0')
	{
		// it is a UNC name, find second slash past '\\'
		ASSERT(IsDirSep(lpsz[0]));
		ASSERT(IsDirSep(lpsz[1]));
		lpsz += 2;
		while (*lpsz != '\0' && (!IsDirSep(*lpsz)))
			lpsz = _tcsinc(lpsz);
		if (*lpsz != '\0')
			lpsz = _tcsinc(lpsz);
		while (*lpsz != '\0' && (!IsDirSep(*lpsz)))
			lpsz = _tcsinc(lpsz);
		// terminate it just after the UNC root (ie. '\\server\share\')
		if (*lpsz != '\0')
			lpsz[1] = '\0';
	}
	else
	{
		// not a UNC, look for just the first slash
		lpsz = lpszRoot;
		while (*lpsz != '\0' && (!IsDirSep(*lpsz)))
			lpsz = _tcsinc(lpsz);
		// terminate it just after root (ie. 'x:\')
		if (*lpsz != '\0')
			lpsz[1] = '\0';
	}
	strRoot.ReleaseBuffer();
}

BOOL AFXAPI AfxComparePath(LPCTSTR lpszPath1, LPCTSTR lpszPath2)
{
	// use case insensitive compare as a starter
	if (lstrcmpi(lpszPath1, lpszPath2) != 0)
		return FALSE;

	// on non-DBCS systems, we are done
	if (!GetSystemMetrics(SM_DBCSENABLED))
		return TRUE;

	// on DBCS systems, the file name may not actually be the same
	// in particular, the file system is case sensitive with respect to
	// "full width" roman characters.
	// (ie. fullwidth-R is different from fullwidth-r).
	int nLen = lstrlen(lpszPath1);
	if (nLen != lstrlen(lpszPath2))
		return FALSE;
	ASSERT(nLen < _MAX_PATH);

	// need to get both CT_CTYPE1 and CT_CTYPE3 for each filename
	LCID lcid = GetThreadLocale();
	WORD aCharType11[_MAX_PATH];
	VERIFY(GetStringTypeEx(lcid, CT_CTYPE1, lpszPath1, -1, aCharType11));
	WORD aCharType13[_MAX_PATH];
	VERIFY(GetStringTypeEx(lcid, CT_CTYPE3, lpszPath1, -1, aCharType13));
	WORD aCharType21[_MAX_PATH];
	VERIFY(GetStringTypeEx(lcid, CT_CTYPE1, lpszPath2, -1, aCharType21));
#ifdef _DEBUG
	WORD aCharType23[_MAX_PATH];
	VERIFY(GetStringTypeEx(lcid, CT_CTYPE3, lpszPath2, -1, aCharType23));
#endif

	// for every C3_FULLWIDTH character, make sure it has same C1 value
	int i = 0;
	for (LPCTSTR lpsz = lpszPath1; *lpsz != 0; lpsz = _tcsinc(lpsz))
	{
		// check for C3_FULLWIDTH characters only
		if (aCharType13[i] & C3_FULLWIDTH)
		{
#ifdef _DEBUG
			ASSERT(aCharType23[i] & C3_FULLWIDTH); // should always match!
#endif

			// if CT_CTYPE1 is different then file system considers these
			// file names different.
			if (aCharType11[i] != aCharType21[i])
				return FALSE;
		}
		++i; // look at next character type
	}
	return TRUE; // otherwise file name is truly the same
}

UINT AFXAPI AfxGetFileTitle(LPCTSTR lpszPathName, LPTSTR lpszTitle, UINT nMax)
{
	ASSERT(lpszTitle == NULL ||
		AfxIsValidAddress(lpszTitle, _MAX_FNAME));
	ASSERT(AfxIsValidString(lpszPathName));

	// use a temporary to avoid bugs in ::GetFileTitle when lpszTitle is NULL
	TCHAR szTemp[_MAX_PATH];
	LPTSTR lpszTemp = lpszTitle;
	if (lpszTemp == NULL)
	{
		lpszTemp = szTemp;
		nMax = _countof(szTemp);
	}
	if (::GetFileTitle(lpszPathName, lpszTemp, (WORD)nMax) != 0)
	{
		// when ::GetFileTitle fails, use cheap imitation
		return AfxGetFileName(lpszPathName, lpszTitle, nMax);
	}
	return lpszTitle == NULL ? lstrlen(lpszTemp)+1 : 0;
}

void AFXAPI AfxGetModuleShortFileName(HINSTANCE hInst, CString& strShortName)
{
	TCHAR szLongPathName[_MAX_PATH];
	::GetModuleFileName(hInst, szLongPathName, _MAX_PATH);
	if (::GetShortPathName(szLongPathName,
		strShortName.GetBuffer(_MAX_PATH), _MAX_PATH) == 0)
	{
		// rare failure case (especially on not-so-modern file systems)
		strShortName = szLongPathName;
	}
	strShortName.ReleaseBuffer();
}

/////////////////////////////////////////////////////////////////////////////
// CFile diagnostics

#ifdef _DEBUG
void CFile::AssertValid() const
{
	CObject::AssertValid();
	// we permit the descriptor m_hFile to be any value for derived classes
}

void CFile::Dump(CDumpContext& dc) const
{
	CObject::Dump(dc);

	dc << "with handle " << (UINT)m_hFile;
	dc << " and name \"" << m_strFileName << "\"";
	dc << "\n";
}
#endif

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CFile, CObject)

/////////////////////////////////////////////////////////////////////////////
