// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1997 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#include "stdafx.h"
#include <afxinet.h>
#include "inetimpl.h"

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif


/////////////////////////////////////////////////////////////////////////////
// for dynamic load of WININET.DLL

#ifdef _AFXDLL

#ifndef _MAC
static const char szINetDLL[] = "WININET.DLL";
#else
#error WinINet isn't available on MPPC
#endif


/////////////////////////////////////////////////////////////////////////////
//

inline void PASCAL AfxINetLoad(FARPROC* pProcPtr, LPCSTR pstrEntry)
{
	HINSTANCE hInst;
	TRY
	{
		// attempt to load but catch error for custom message
		hInst = AfxLoadDll(&_afxExtDllState->m_hInstInternet, szINetDLL);
	}
	CATCH_ALL(e)
	{
		TRACE1("Error: Couldn't load %s!\n", szINetDLL);
		// Note: DELETE_EXCEPTION(e) not necessary
		AfxThrowInternetException(GetLastError());
	}
	END_CATCH_ALL

	ASSERT(hInst != NULL);

	// cache the procedure pointer
	ASSERT(pProcPtr != NULL);
	*pProcPtr = GetProcAddress(hInst, pstrEntry);
	if (*pProcPtr == NULL)
	{
		TRACE2("Error: Couldn't find %s in %s!\n", pstrEntry, szINetDLL);
		AfxThrowInternetException(GetLastError());
	}
}


#define INETLOAD(x) AfxINetLoad((FARPROC*)&_afxWinINet.pfn##x, #x)
#ifdef _UNICODE
#define INETLOADT(x) AfxINetLoad((FARPROC*)&_afxWinINet.pfn##x, #x"W")
#else
#define INETLOADT(x) AfxINetLoad((FARPROC*)&_afxWinINet.pfn##x, #x"A")
#endif

// generic connection

HINTERNET WINAPI AfxThunkINetInternetOpen(
	LPCTSTR lpszAgent,
	DWORD dwAccessType,
	LPCTSTR lpszProxy,
	LPCTSTR lpszProxyBypass,
	DWORD dwFlags)
{
	INETLOADT(InternetOpen);
	return _afxWinINet.pfnInternetOpen(lpszAgent, dwAccessType,
		lpszProxy, lpszProxyBypass, dwFlags);
}

HINTERNET WINAPI AfxThunkINetInternetCloseHandle(HINTERNET hInternet)
{
	INETLOAD(InternetCloseHandle);
	return _afxWinINet.pfnInternetCloseHandle(hInternet);
}

HINTERNET WINAPI AfxThunkINetInternetOpenUrl(HINTERNET hInternet,
	LPCTSTR pstrUrl, LPCTSTR pstrHeaders, DWORD dwHeadersLength,
	DWORD dwFlags, DWORD dwContext)
{
	INETLOADT(InternetOpenUrl);
	return _afxWinINet.pfnInternetOpenUrl(hInternet,
		pstrUrl, pstrHeaders, dwHeadersLength, dwFlags, dwContext);
}

HINTERNET WINAPI AfxThunkINetInternetReadFile(HINTERNET hInternet,
	LPVOID lpBuffer, DWORD dwNumberOfBytesToRead,
	LPDWORD lpdwNumberOfBytesRead)
{
	INETLOAD(InternetReadFile);
	return _afxWinINet.pfnInternetReadFile(hInternet, lpBuffer,
		dwNumberOfBytesToRead, lpdwNumberOfBytesRead);
}

HINTERNET WINAPI AfxThunkINetInternetWriteFile(HINTERNET hInternet,
	LPCVOID lpBuffer, DWORD dwNumberOfBytesToWrite,
	LPDWORD lpdwNumberOfBytesWritten)
{
	INETLOAD(InternetWriteFile);
	return _afxWinINet.pfnInternetWriteFile(hInternet, lpBuffer,
		dwNumberOfBytesToWrite, lpdwNumberOfBytesWritten);
}

INTERNET_STATUS_CALLBACK WINAPI AfxThunkINetInternetSetStatusCallback(
	HINTERNET hInternet, INTERNET_STATUS_CALLBACK lpfnInternetCallback)
{
	INETLOAD(InternetSetStatusCallback);
	return _afxWinINet.pfnInternetSetStatusCallback(hInternet,
		lpfnInternetCallback);
}

BOOL WINAPI AfxThunkINetInternetQueryOption(HINTERNET hInternet,
	DWORD dwOption, LPVOID lpBuffer, LPDWORD lpdwBufferLength)
{
	INETLOADT(InternetQueryOption);
	return _afxWinINet.pfnInternetQueryOption(hInternet, dwOption,
		lpBuffer, lpdwBufferLength);
}

BOOL WINAPI AfxThunkINetInternetSetOption(HINTERNET hInternet,
	DWORD dwOption, LPVOID lpBuffer, DWORD dwBufferLength)
{
	INETLOADT(InternetSetOption);
	return _afxWinINet.pfnInternetSetOption(hInternet, dwOption, lpBuffer,
		dwBufferLength);
}

BOOL WINAPI AfxThunkINetInternetSetOptionEx(HINTERNET hInternet,
	DWORD dwOption, LPVOID lpBuffer, DWORD dwBufferLength, DWORD dwFlags)
{
	INETLOADT(InternetSetOptionEx);
	return _afxWinINet.pfnInternetSetOptionEx(hInternet, dwOption, lpBuffer,
		dwBufferLength, dwFlags);
}

BOOL WINAPI AfxThunkINetInternetGetLastResponseInfo(LPDWORD lpdwError,
	LPTSTR pstrBuffer, LPDWORD lpdwBufferLength)
{
	INETLOADT(InternetGetLastResponseInfo);
	return _afxWinINet.pfnInternetGetLastResponseInfo(lpdwError,
		pstrBuffer, lpdwBufferLength);
}

BOOL WINAPI AfxThunkINetInternetFindNextFile(HINTERNET hFind,
	LPVOID lpvFindData)
{
	INETLOADT(InternetFindNextFile);
	return _afxWinINet.pfnInternetFindNextFile(hFind, lpvFindData);
}

HINTERNET WINAPI AfxThunkINetInternetConnect(HINTERNET hInternet,
	LPCTSTR pstrServerName, INTERNET_PORT nServerPort, LPCTSTR pstrUsername,
	LPCTSTR pstrPassword, DWORD dwService, DWORD dwFlags, DWORD dwContext)
{
	INETLOADT(InternetConnect);
	return _afxWinINet.pfnInternetConnect(hInternet, pstrServerName,
		nServerPort, pstrUsername, pstrPassword, dwService, dwFlags,
		dwContext);
}

DWORD WINAPI AfxThunkINetInternetSetFilePointer(HINTERNET hFile,
	LONG lDistanceToMove, PVOID reserved, DWORD dwMoveMethod, DWORD dwContext)
{
	INETLOAD(InternetSetFilePointer);
	return _afxWinINet.pfnInternetSetFilePointer(hFile, lDistanceToMove,
		reserved, dwMoveMethod, dwContext);
}

BOOL WINAPI AfxThunkINetInternetQueryDataAvailable(HINTERNET hFile,
	LPDWORD lpdwNumberOfBytesAvailable, DWORD dwFlags, DWORD dwContext)
{
	INETLOAD(InternetQueryDataAvailable);
	return _afxWinINet.pfnInternetQueryDataAvailable(hFile,
		lpdwNumberOfBytesAvailable, dwFlags, dwContext);
}


// ftp

HINTERNET WINAPI AfxThunkINetFtpFindFirstFile(HINTERNET hFtpSession,
	LPCTSTR pstrSearchFile, LPWIN32_FIND_DATA lpFindFileData,
	DWORD dwFlags, DWORD dwContext)
{
	INETLOADT(FtpFindFirstFile);
	return _afxWinINet.pfnFtpFindFirstFile(hFtpSession,
		pstrSearchFile, lpFindFileData, dwFlags, dwContext);
}

BOOL WINAPI AfxThunkINetFtpGetFile(HINTERNET hFtpSession,
	LPCTSTR pstrRemoteFile, LPCTSTR pstrNewFile, BOOL fFailIfExists,
	DWORD dwFlagsAndAttributes, DWORD dwFlags, DWORD dwContext)
{
	INETLOADT(FtpGetFile);
	return _afxWinINet.pfnFtpGetFile(hFtpSession, pstrRemoteFile,
		pstrNewFile, fFailIfExists, dwFlagsAndAttributes, dwFlags,
		dwContext);
}

BOOL WINAPI AfxThunkINetFtpPutFile(HINTERNET hFtpSession,
	LPCTSTR pstrLocalFile, LPCTSTR pstrNewRemoteFile, DWORD dwFlags,
	DWORD dwContext)
{
	INETLOADT(FtpPutFile);
	return _afxWinINet.pfnFtpPutFile(hFtpSession, pstrLocalFile,
		pstrNewRemoteFile, dwFlags, dwContext);
}

BOOL WINAPI AfxThunkINetFtpDeleteFile(HINTERNET hFtpSession,
	LPCTSTR pstrFileName)
{
	INETLOADT(FtpDeleteFile);
	return _afxWinINet.pfnFtpDeleteFile(hFtpSession, pstrFileName);
}

BOOL WINAPI AfxThunkINetFtpRenameFile(HINTERNET hFtpSession,
	LPCTSTR pstrExisting, LPCTSTR pstrNew)
{
	INETLOADT(FtpRenameFile);
	return _afxWinINet.pfnFtpRenameFile(hFtpSession, pstrExisting, pstrNew);
}

BOOL WINAPI AfxThunkINetFtpCreateDirectory(HINTERNET hFtpSession,
	LPCTSTR pstrDirectory)
{
	INETLOADT(FtpCreateDirectory);
	return _afxWinINet.pfnFtpCreateDirectory(hFtpSession, pstrDirectory);
}

BOOL WINAPI AfxThunkINetFtpRemoveDirectory(HINTERNET hFtpSession,
	LPCTSTR pstrDirectory)
{
	INETLOADT(FtpRemoveDirectory);
	return _afxWinINet.pfnFtpRemoveDirectory(hFtpSession, pstrDirectory);
}

BOOL WINAPI AfxThunkINetFtpSetCurrentDirectory(HINTERNET hFtpSession,
	LPCTSTR pstrDirectory)
{
	INETLOADT(FtpSetCurrentDirectory);
	return _afxWinINet.pfnFtpSetCurrentDirectory(hFtpSession, pstrDirectory);
}

BOOL WINAPI AfxThunkINetFtpGetCurrentDirectory(HINTERNET hFtpSession,
	LPCTSTR pstrCurrentDirectory, LPDWORD lpdwCurrentDirectory)
{
	INETLOADT(FtpGetCurrentDirectory);
	return _afxWinINet.pfnFtpGetCurrentDirectory(hFtpSession,
		pstrCurrentDirectory, lpdwCurrentDirectory);
}

BOOL WINAPI AfxThunkINetFtpCommand(HINTERNET hFtpSession,
	BOOL fExpectResponse, DWORD dwFlags, LPCTSTR pstrCommand,
	DWORD dwContext)
{
	INETLOADT(FtpCommand);
	return _afxWinINet.pfnFtpCommand(hFtpSession, fExpectResponse,
		dwFlags, pstrCommand, dwContext);
}

HINTERNET WINAPI AfxThunkINetFtpOpenFile(HINTERNET hFtpSession,
		LPCTSTR pstrFileName, DWORD dwAccess, DWORD dwFlags,
		DWORD dwContext)
{
	INETLOADT(FtpOpenFile);
	return _afxWinINet.pfnFtpOpenFile(hFtpSession, pstrFileName,
		dwAccess, dwFlags, dwContext);
}


// gopher

HINTERNET WINAPI AfxThunkINetGopherFindFirstFile(HINTERNET hGopherSession,
	LPCTSTR pstrLocator, LPCTSTR pstrSearchString,
	LPGOPHER_FIND_DATA lpFindData, DWORD dwFlags, DWORD dwContext)
{
	INETLOADT(GopherFindFirstFile);
	return _afxWinINet.pfnGopherFindFirstFile(hGopherSession,
		pstrLocator, pstrSearchString, lpFindData, dwFlags, dwContext);
}

HINTERNET WINAPI AfxThunkINetGopherOpenFile(HINTERNET hGopherSession,
	LPCTSTR pstrLocator, LPCTSTR pstrView, DWORD dwFlags,
	DWORD dwContext)
{
	INETLOADT(GopherOpenFile);
	return _afxWinINet.pfnGopherOpenFile(hGopherSession, pstrLocator,
		pstrView, dwFlags, dwContext);
}

BOOL WINAPI AfxThunkINetGopherCreateLocator(LPCTSTR pstrHost,
	INTERNET_PORT nServerPort, LPCTSTR pstrDisplayString,
	LPCTSTR pstrSelectorString, DWORD dwGopherType,
	LPTSTR pstrLocator, LPDWORD lpdwBufferLength)
{
	INETLOADT(GopherCreateLocator);
	return _afxWinINet.pfnGopherCreateLocator(pstrHost, nServerPort,
		pstrDisplayString, pstrSelectorString, dwGopherType,
		pstrLocator, lpdwBufferLength);
}

BOOL WINAPI AfxThunkINetGopherGetAttribute(HINTERNET hGopherSession,
	LPCTSTR pstrLocator, LPCTSTR pstrAttributeName, LPBYTE lpBuffer,
	DWORD dwBufferLength, LPDWORD lpdwCharactersReturned,
	GOPHER_ATTRIBUTE_ENUMERATOR lpfnEnumerator, DWORD dwContext)
{
	INETLOADT(GopherGetAttribute);
	return _afxWinINet.pfnGopherGetAttribute(hGopherSession, pstrLocator,
		pstrAttributeName, lpBuffer, dwBufferLength, lpdwCharactersReturned,
		lpfnEnumerator, dwContext);
}

BOOL WINAPI AfxThunkINetGopherGetLocatorType(LPCTSTR pstrLocator,
	LPDWORD pdwGopherType)
{
	INETLOADT(GopherGetLocatorType);
	return _afxWinINet.pfnGopherGetLocatorType(pstrLocator, pdwGopherType);
}

// http

HINTERNET WINAPI AfxThunkINetHttpOpenRequest(HINTERNET hHttpSession,
	LPCTSTR pstrVerb, LPCTSTR pstrObjectName, LPCTSTR pstrVersion,
	LPCTSTR pstrReferrer, LPCTSTR FAR * lppstrAcceptTypes,
	DWORD dwFlags, DWORD dwContext)
{
	INETLOADT(HttpOpenRequest);
	return _afxWinINet.pfnHttpOpenRequest(hHttpSession, pstrVerb,
		pstrObjectName, pstrVersion, pstrReferrer, lppstrAcceptTypes,
		dwFlags, dwContext);
}

BOOL WINAPI AfxThunkINetHttpAddRequestHeaders(HINTERNET hHttpRequest,
	LPCTSTR pstrHeaders, DWORD dwHeadersLength, DWORD dwModifiers)
{
	INETLOADT(HttpAddRequestHeaders);
	return _afxWinINet.pfnHttpAddRequestHeaders(hHttpRequest,
		pstrHeaders, dwHeadersLength, dwModifiers);
}

BOOL WINAPI AfxThunkINetHttpSendRequest(HINTERNET hHttpRequest,
	LPCTSTR pstrHeaders, DWORD dwHeadersLength, LPVOID lpOptional,
	DWORD dwOptionalLength)
{
	INETLOADT(HttpSendRequest);
	return _afxWinINet.pfnHttpSendRequest(hHttpRequest, pstrHeaders,
		dwHeadersLength, lpOptional, dwOptionalLength);
}

BOOL WINAPI AfxThunkINetHttpQueryInfo(HINTERNET hHttpRequest,
	DWORD dwInfoLevel, LPVOID lpvBuffer, LPDWORD lpdwBufferLength,
	LPDWORD lpdwIndex)
{
	INETLOADT(HttpQueryInfo);
	return _afxWinINet.pfnHttpQueryInfo(hHttpRequest, dwInfoLevel,
		lpvBuffer, lpdwBufferLength, lpdwIndex);
}

DWORD WINAPI AfxThunkINetInternetErrorDlg(HWND hWnd, HINTERNET hRequest,
	 DWORD dwError, DWORD dwFlags, LPVOID * lppvData)
{
	INETLOADT(InternetErrorDlg);
	return _afxWinINet.pfnInternetErrorDlg(hWnd, hRequest,
		dwError, dwFlags, lppvData);
}

// parsers

BOOL WINAPI AfxThunkINetCrackUrl(LPCTSTR lpszUrl, DWORD dwUrlLength,
	DWORD dwFlags, LPURL_COMPONENTS lpUrlComponents)
{
	INETLOADT(InternetCrackUrl);
	return _afxWinINet.pfnInternetCrackUrl(lpszUrl, dwUrlLength,
		dwFlags, lpUrlComponents);
}

BOOL WINAPI AfxThunkINetCanonicalizeUrl(LPCTSTR lpszUrl,
	LPTSTR lpszBuffer, LPDWORD lpdwBufferLength, DWORD dwFlags)
{
	INETLOADT(InternetCanonicalizeUrl);
	return _afxWinINet.pfnInternetCanonicalizeUrl(lpszUrl,
		lpszBuffer, lpdwBufferLength, dwFlags);
}


/////////////////////////////////////////////////////////////////////////////
//

AFX_DATADEF AFX_WININET_CALL _afxWinINet =
{
// generic connection

	{ AfxThunkINetInternetOpen, },
	{ AfxThunkINetInternetCloseHandle, },
	{ AfxThunkINetInternetOpenUrl, },
	{ AfxThunkINetInternetReadFile, },
	{ AfxThunkINetInternetWriteFile, },
	{ AfxThunkINetInternetSetStatusCallback, },
	{ AfxThunkINetInternetQueryOption, },
	{ AfxThunkINetInternetSetOption, },
	{ AfxThunkINetInternetSetOptionEx, },
	{ AfxThunkINetInternetGetLastResponseInfo, },
	{ AfxThunkINetInternetFindNextFile, },
	{ AfxThunkINetInternetConnect, },
	{ AfxThunkINetInternetSetFilePointer, },
	{ AfxThunkINetInternetQueryDataAvailable, },

// ftp

	{ AfxThunkINetFtpFindFirstFile, },
	{ AfxThunkINetFtpGetFile, },
	{ AfxThunkINetFtpPutFile, },
	{ AfxThunkINetFtpDeleteFile, },
	{ AfxThunkINetFtpRenameFile, },
	{ AfxThunkINetFtpCreateDirectory, },
	{ AfxThunkINetFtpRemoveDirectory, },
	{ AfxThunkINetFtpSetCurrentDirectory, },
	{ AfxThunkINetFtpGetCurrentDirectory, },
	{ AfxThunkINetFtpCommand, },
	{ AfxThunkINetFtpOpenFile, },

// gopher

	{ AfxThunkINetGopherFindFirstFile, },
	{ AfxThunkINetGopherOpenFile, },
	{ AfxThunkINetGopherCreateLocator, },
	{ AfxThunkINetGopherGetAttribute, },
	{ AfxThunkINetGopherGetLocatorType, },

// html

	{ AfxThunkINetHttpOpenRequest, },
	{ AfxThunkINetHttpAddRequestHeaders, },
	{ AfxThunkINetHttpSendRequest, },
	{ AfxThunkINetHttpQueryInfo, },
	{ AfxThunkINetInternetErrorDlg, },

// parsers
	{ AfxThunkINetCrackUrl, },
	{ AfxThunkINetCanonicalizeUrl, },
};



#endif //_AFXDLL (the whole file)
