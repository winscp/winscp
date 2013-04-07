// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1997 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#ifndef _MAC
#ifdef _AFXDLL

/////////////////////////////////////////////////////////////////////////////
// AFX_WININET_CALL - used to dynamically load WININET.DLL

struct AFX_WININET_CALL
{
// generic connection

	HINTERNET (WINAPI* pfnInternetOpen)(
		LPCTSTR lpszAgent, DWORD dwAccessType,
		LPCTSTR lpszProxy, LPCTSTR lpszProxyBypass, DWORD dwFlags);
	HINTERNET (WINAPI* pfnInternetCloseHandle)(HINTERNET hInternet);
	HINTERNET (WINAPI* pfnInternetOpenUrl)(HINTERNET hInternet,
		LPCTSTR pstrUrl, LPCTSTR pstrHeaders, DWORD dwHeadersLength,
		DWORD dwFlags, DWORD dwContext);
	HINTERNET (WINAPI* pfnInternetReadFile)(HINTERNET hInternet,
		LPVOID lpBuffer, DWORD dwNumberOfBytesToRead,
		LPDWORD lpdwNumberOfBytesRead);
	HINTERNET (WINAPI* pfnInternetWriteFile)(HINTERNET hInternet,
		LPCVOID lpBuffer, DWORD dwNumberOfBytesToWrite,
		LPDWORD lpdwNumberOfBytesWritten);
	INTERNET_STATUS_CALLBACK (WINAPI* pfnInternetSetStatusCallback)(
		HINTERNET hInternet, INTERNET_STATUS_CALLBACK lpfnInternetCallback);
	BOOL (WINAPI* pfnInternetQueryOption)(HINTERNET hInternet,
		DWORD dwOption, LPVOID lpBuffer, LPDWORD lpdwBufferLength);
	BOOL (WINAPI* pfnInternetSetOption)(HINTERNET hInternet,
		DWORD dwOption, LPVOID lpBuffer, DWORD dwBufferLength);
	BOOL (WINAPI* pfnInternetSetOptionEx)(HINTERNET hInternet,
		DWORD dwOption, LPVOID lpBuffer, DWORD dwBufferLength, DWORD dwFlags);
	BOOL (WINAPI* pfnInternetGetLastResponseInfo)(LPDWORD lpdwError,
		LPTSTR pstrBuffer, LPDWORD lpdwBufferLength);
	BOOL (WINAPI* pfnInternetFindNextFile)(HINTERNET hFind,
		LPVOID lpvFindData);
	HINTERNET (WINAPI* pfnInternetConnect)(HINTERNET hInternet,
		LPCTSTR pstrServerName, INTERNET_PORT nServerPort,
		LPCTSTR pstrUsername, LPCTSTR pstrPassword, DWORD dwService,
		DWORD dwFlags, DWORD dwContext);
	DWORD (WINAPI* pfnInternetSetFilePointer)(HINTERNET hFile,
		LONG lDistanceToMove, PVOID reserved, DWORD dwMoveMethod,
		DWORD dwContext);
	BOOL (WINAPI* pfnInternetQueryDataAvailable)(HINTERNET hFile,
		LPDWORD lpdwNumberOfBytesAvailable, DWORD dwFlags, DWORD dwContext);

// ftp

	HINTERNET (WINAPI* pfnFtpFindFirstFile)(HINTERNET hFtpSession,
		LPCTSTR pstrSearchFile, LPWIN32_FIND_DATA lpFindFileData,
		DWORD dwFlags, DWORD dwContext);
	BOOL (WINAPI* pfnFtpGetFile)(HINTERNET hFtpSession,
		LPCTSTR pstrRemoteFile, LPCTSTR pstrNewFile, BOOL fFailIfExists,
		DWORD dwFlagsAndAttributes, DWORD dwFlags, DWORD dwContext);
	BOOL (WINAPI* pfnFtpPutFile)(HINTERNET hFtpSession,
		LPCTSTR pstrLocalFile, LPCTSTR pstrNewRemoteFile, DWORD dwFlags,
		DWORD dwContext);
	BOOL (WINAPI* pfnFtpDeleteFile)(HINTERNET hFtpSession,
		LPCTSTR pstrFileName);
	BOOL (WINAPI* pfnFtpRenameFile)(HINTERNET hFtpSession,
		LPCTSTR pstrExisting, LPCTSTR pstrNew);
	BOOL (WINAPI* pfnFtpCreateDirectory)(HINTERNET hFtpSession,
		LPCTSTR pstrDirectory);
	BOOL (WINAPI* pfnFtpRemoveDirectory)(HINTERNET hFtpSession,
		LPCTSTR pstrDirectory);
	BOOL (WINAPI* pfnFtpSetCurrentDirectory)(HINTERNET hFtpSession,
		LPCTSTR pstrDirectory);
	BOOL (WINAPI* pfnFtpGetCurrentDirectory)(HINTERNET hFtpSession,
		LPCTSTR pstrCurrentDirectory, LPDWORD lpdwCurrentDirectory);
	BOOL (WINAPI* pfnFtpCommand)(HINTERNET hFtpSession,
		BOOL fExpectResponse, DWORD dwFlags, LPCTSTR pstrCommand,
		DWORD dwContext);
	HINTERNET (WINAPI* pfnFtpOpenFile)(HINTERNET hFtpSession,
		LPCTSTR pstrFileName, DWORD dwAccess, DWORD dwFlags, DWORD dwContext);

// gopher

	HINTERNET (WINAPI* pfnGopherFindFirstFile)(HINTERNET hGopherSession,
		LPCTSTR pstrLocator, LPCTSTR pstrSearchString,
		LPGOPHER_FIND_DATA lpFindData, DWORD dwFlags, DWORD dwContext);
	HINTERNET (WINAPI* pfnGopherOpenFile)(HINTERNET hGopherSession,
		LPCTSTR pstrLocator, LPCTSTR pstrView, DWORD dwFlags,
		DWORD dwContext);
	BOOL (WINAPI* pfnGopherCreateLocator)(LPCTSTR pstrHost,
		INTERNET_PORT nServerPort, LPCTSTR pstrDisplayString,
		LPCTSTR pstrSelectorString, DWORD dwGopherType,
		LPTSTR pstrLocator, LPDWORD lpdwBufferLength);
	BOOL (WINAPI* pfnGopherGetAttribute)(HINTERNET hGopherSession,
		LPCTSTR pstrLocator, LPCTSTR pstrAttributeName, LPBYTE lpBuffer,
		DWORD dwBufferLength, LPDWORD lpdwCharactersReturned,
		GOPHER_ATTRIBUTE_ENUMERATOR lpfnEnumerator, DWORD dwContext);
	BOOL (WINAPI* pfnGopherGetLocatorType)(LPCTSTR pstrLocator,
		LPDWORD pdwGopherType);

// html

	HINTERNET (WINAPI* pfnHttpOpenRequest)(HINTERNET hHttpSession,
		LPCTSTR pstrVerb, LPCTSTR pstrObjectName, LPCTSTR pstrVersion,
		LPCTSTR pstrReferrer, LPCTSTR FAR * lppstrAcceptTypes,
		DWORD dwFlags, DWORD dwContext);
	BOOL (WINAPI* pfnHttpAddRequestHeaders)(HINTERNET hHttpRequest,
		LPCTSTR pstrHeaders, DWORD dwHeadersLength, DWORD dwModifiers);
	BOOL (WINAPI* pfnHttpSendRequest)(HINTERNET hHttpRequest,
		LPCTSTR pstrHeaders, DWORD dwHeadersLength, LPVOID lpOptional,
		DWORD dwOptionalLength);
	BOOL (WINAPI* pfnHttpQueryInfo)(HINTERNET hHttpRequest,
		DWORD dwInfoLevel, LPVOID lpvBuffer, LPDWORD lpdwBufferLength,
		LPDWORD dwIndex);
	DWORD (WINAPI*pfnInternetErrorDlg)(HWND hWnd, HINTERNET hRequest,
		 DWORD dwError, DWORD dwFlags, LPVOID * lppvData);

// utility

	BOOL (WINAPI* pfnInternetCrackUrl)(LPCTSTR lpszUrl, DWORD dwUrlLength,
		DWORD dwFlags, LPURL_COMPONENTS lpUrlComponents);
	BOOL (WINAPI* pfnInternetCanonicalizeUrl)(LPCTSTR lpszUrl,
		LPTSTR lpszBuffer, LPDWORD lpdwBufferLength, DWORD dwFlags);
};

extern AFX_DATA AFX_WININET_CALL _afxWinINet;


/////////////////////////////////////////////////////////////////////////////
// macros for AFX_WININET_CALL access

// generic connection

#ifdef InternetOpen
#undef InternetOpen
#endif
#define InternetOpen _afxWinINet.pfnInternetOpen

#ifdef InternetCloseHandle
#undef InternetCloseHandle
#endif
#define InternetCloseHandle _afxWinINet.pfnInternetCloseHandle

#ifdef InternetOpenUrl
#undef InternetOpenUrl
#endif
#define InternetOpenUrl _afxWinINet.pfnInternetOpenUrl

#ifdef InternetReadFile
#undef InternetReadFile
#endif
#define InternetReadFile _afxWinINet.pfnInternetReadFile

#ifdef InternetWriteFile
#undef InternetWriteFile
#endif
#define InternetWriteFile _afxWinINet.pfnInternetWriteFile

#ifdef InternetSetStatusCallback
#undef InternetSetStatusCallback
#endif
#define InternetSetStatusCallback _afxWinINet.pfnInternetSetStatusCallback

#ifdef InternetSetOption
#undef InternetSetOption
#endif
#define InternetSetOption _afxWinINet.pfnInternetSetOption

#ifdef InternetSetOptionEx
#undef InternetSetOptionEx
#endif
#define InternetSetOptionEx _afxWinINet.pfnInternetSetOptionEx

#ifdef InternetQueryOption
#undef InternetQueryOption
#endif
#define InternetQueryOption _afxWinINet.pfnInternetQueryOption

#ifdef InternetGetLastResponseInfo
#undef InternetGetLastResponseInfo
#endif
#define InternetGetLastResponseInfo _afxWinINet.pfnInternetGetLastResponseInfo

#ifdef InternetFindNextFile
#undef InternetFindNextFile
#endif
#define InternetFindNextFile _afxWinINet.pfnInternetFindNextFile

#ifdef InternetConnect
#undef InternetConnect
#endif
#define InternetConnect _afxWinINet.pfnInternetConnect

#ifdef InternetSetFilePointer
#undef InternetSetFilePointer
#endif
#define InternetSetFilePointer _afxWinINet.pfnInternetSetFilePointer

#ifdef InternetQueryDataAvailable
#undef InternetQueryDataAvailable
#endif
#define InternetQueryDataAvailable _afxWinINet.pfnInternetQueryDataAvailable


// ftp

#ifdef FtpFindFirstFile
#undef FtpFindFirstFile
#endif
#define FtpFindFirstFile _afxWinINet.pfnFtpFindFirstFile

#ifdef FtpGetFile
#undef FtpGetFile
#endif
#define FtpGetFile _afxWinINet.pfnFtpGetFile

#ifdef FtpPutFile
#undef FtpPutFile
#endif
#define FtpPutFile _afxWinINet.pfnFtpPutFile

#ifdef FtpDeleteFile
#undef FtpDeleteFile
#endif
#define FtpDeleteFile _afxWinINet.pfnFtpDeleteFile

#ifdef FtpRenameFile
#undef FtpRenameFile
#endif
#define FtpRenameFile _afxWinINet.pfnFtpRenameFile

#ifdef FtpCreateDirectory
#undef FtpCreateDirectory
#endif
#define FtpCreateDirectory _afxWinINet.pfnFtpCreateDirectory

#ifdef FtpRemoveDirectory
#undef FtpRemoveDirectory
#endif
#define FtpRemoveDirectory _afxWinINet.pfnFtpRemoveDirectory

#ifdef FtpGetCurrentDirectory
#undef FtpGetCurrentDirectory
#endif
#define FtpGetCurrentDirectory _afxWinINet.pfnFtpGetCurrentDirectory

#ifdef FtpSetCurrentDirectory
#undef FtpSetCurrentDirectory
#endif
#define FtpSetCurrentDirectory _afxWinINet.pfnFtpSetCurrentDirectory

#ifdef FtpCommand
#undef FtpCommand
#endif
#define FtpCommand _afxWinINet.pfnFtpCommand

#ifdef FtpOpenFile
#undef FtpOpenFile
#endif
#define FtpOpenFile _afxWinINet.pfnFtpOpenFile

// gopher

#ifdef GopherFindFirstFile
#undef GopherFindFirstFile
#endif
#define GopherFindFirstFile _afxWinINet.pfnGopherFindFirstFile

#ifdef GopherOpenFile
#undef GopherOpenFile
#endif
#define GopherOpenFile _afxWinINet.pfnGopherOpenFile

#ifdef GopherCreateLocator
#undef GopherCreateLocator
#endif
#define GopherCreateLocator _afxWinINet.pfnGopherCreateLocator

#ifdef GopherGetAttribute
#undef GopherGetAttribute
#endif
#define GopherGetAttribute _afxWinINet.pfnGopherGetAttribute

#ifdef GopherGetLocatorType
#undef GopherGetLocatorType
#endif
#define GopherGetLocatorType _afxWinINet.pfnGopherGetLocatorType

// html

#ifdef HttpOpenRequest
#undef HttpOpenRequest
#endif
#define HttpOpenRequest _afxWinINet.pfnHttpOpenRequest

#ifdef HttpAddRequestHeaders
#undef HttpAddRequestHeaders
#endif
#define HttpAddRequestHeaders _afxWinINet.pfnHttpAddRequestHeaders

#ifdef HttpSendRequest
#undef HttpSendRequest
#endif
#define HttpSendRequest _afxWinINet.pfnHttpSendRequest

#ifdef HttpQueryInfo
#undef HttpQueryInfo
#endif
#define HttpQueryInfo _afxWinINet.pfnHttpQueryInfo

#ifdef InternetErrorDlg
#undef InternetErrorDlg
#endif
#define InternetErrorDlg _afxWinINet.pfnInternetErrorDlg

#ifdef InternetCrackUrl
#undef InternetCrackUrl
#endif
#define InternetCrackUrl _afxWinINet.pfnInternetCrackUrl

#ifdef InternetCanonicalizeUrl
#undef InternetCanonicalizeUrl
#endif
#define InternetCanonicalizeUrl _afxWinINet.pfnInternetCanonicalizeUrl

#endif  //_AFXDLL
#endif  //!_MAC
