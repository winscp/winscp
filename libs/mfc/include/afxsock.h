// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#ifndef __AFXSOCK_H__
#define __AFXSOCK_H__

#ifdef _AFX_NO_SOCKET_SUPPORT
	#error Windows Sockets classes not supported in this library variant.
#endif

#ifndef __AFXWIN_H__
	#include <afxwin.h>
#endif

#ifndef _WINSOCKAPI_
	#include <winsock.h>
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
			#pragma comment(lib, "mfcn42d.lib")
		#else
			#pragma comment(lib, "mfcn42ud.lib")
		#endif
	#endif
#endif

#pragma comment(lib, "wsock32.lib")

#endif //!_AFX_NOFORCE_LIBS

/////////////////////////////////////////////////////////////////////////////

#ifdef _AFX_PACKING
#pragma pack(push, _AFX_PACKING)
#endif

/////////////////////////////////////////////////////////////////////////////
// AFXSOCK - MFC support for Windows Sockets

// Classes declared in this file

	// CObject
		class CAsyncSocket; // Async Socket implementation and
							// base class for Synchronous Socket
			class CSocket;  // Synchronous Socket

	// CFile
		class CSocketFile; // Used with CSocket and CArchive for
						   // streaming objects on sockets.

/////////////////////////////////////////////////////////////////////////////

// AFXDLL support
#undef AFX_DATA
#define AFX_DATA AFX_NET_DATA

/////////////////////////////////////////////////////////////////////////////
// CSocketWnd -- internal use only
//  Implementation for sockets notification callbacks.
//  Future versions of MFC may or may not include this exact class.

class CSocketWnd : public CWnd
{
// Construction
public:
	CSocketWnd();

protected:
	//{{AFX_MSG(CSocketWnd)
	LRESULT OnSocketNotify(WPARAM wParam, LPARAM lParam);
	LRESULT OnSocketDead(WPARAM wParam, LPARAM lParam);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// CAsyncSocket

class CAsyncSocket : public CObject
{
	DECLARE_DYNAMIC(CAsyncSocket);
private:
	CAsyncSocket(const CAsyncSocket& rSrc);    // no implementation
	void operator=(const CAsyncSocket& rSrc);  // no implementation

// Construction
public:
	CAsyncSocket();
	BOOL Create(UINT nSocketPort = 0, int nSocketType=SOCK_STREAM,
		long lEvent = FD_READ | FD_WRITE | FD_OOB | FD_ACCEPT | FD_CONNECT | FD_CLOSE,
		LPCTSTR lpszSocketAddress = NULL);

// Attributes
public:
	SOCKET m_hSocket;

	operator SOCKET() const;
	BOOL Attach(SOCKET hSocket, long lEvent =
		FD_READ | FD_WRITE | FD_OOB | FD_ACCEPT | FD_CONNECT | FD_CLOSE);
	SOCKET Detach();

	BOOL GetPeerName(CString& rPeerAddress, UINT& rPeerPort);
	BOOL GetPeerName(SOCKADDR* lpSockAddr, int* lpSockAddrLen);

	BOOL GetSockName(CString& rSocketAddress, UINT& rSocketPort);
	BOOL GetSockName(SOCKADDR* lpSockAddr, int* lpSockAddrLen);

	BOOL SetSockOpt(int nOptionName, const void* lpOptionValue,
		int nOptionLen, int nLevel = SOL_SOCKET);
	BOOL GetSockOpt(int nOptionName, void* lpOptionValue,
		int* lpOptionLen, int nLevel = SOL_SOCKET);

	static CAsyncSocket* PASCAL FromHandle(SOCKET hSocket);
	static int PASCAL GetLastError();

// Operations
public:

	virtual BOOL Accept(CAsyncSocket& rConnectedSocket,
		SOCKADDR* lpSockAddr = NULL, int* lpSockAddrLen = NULL);

	BOOL Bind(UINT nSocketPort, LPCTSTR lpszSocketAddress = NULL);
	BOOL Bind (const SOCKADDR* lpSockAddr, int nSockAddrLen);

	virtual void Close();

	BOOL Connect(LPCTSTR lpszHostAddress, UINT nHostPort);
	BOOL Connect(const SOCKADDR* lpSockAddr, int nSockAddrLen);

	BOOL IOCtl(long lCommand, DWORD* lpArgument);

	BOOL Listen(int nConnectionBacklog=5);

	virtual int Receive(void* lpBuf, int nBufLen, int nFlags = 0);

	int ReceiveFrom(void* lpBuf, int nBufLen,
		CString& rSocketAddress, UINT& rSocketPort, int nFlags = 0);
	int ReceiveFrom(void* lpBuf, int nBufLen,
		SOCKADDR* lpSockAddr, int* lpSockAddrLen, int nFlags = 0);

	enum { receives = 0, sends = 1, both = 2 };
	BOOL ShutDown(int nHow = sends);

	virtual int Send(const void* lpBuf, int nBufLen, int nFlags = 0);

	int SendTo(const void* lpBuf, int nBufLen,
		UINT nHostPort, LPCTSTR lpszHostAddress = NULL, int nFlags = 0);
	int SendTo(const void* lpBuf, int nBufLen,
		const SOCKADDR* lpSockAddr, int nSockAddrLen, int nFlags = 0);

	BOOL AsyncSelect(long lEvent =
		FD_READ | FD_WRITE | FD_OOB | FD_ACCEPT | FD_CONNECT | FD_CLOSE);

// Overridable callbacks
protected:
	virtual void OnReceive(int nErrorCode);
	virtual void OnSend(int nErrorCode);
	virtual void OnOutOfBandData(int nErrorCode);
	virtual void OnAccept(int nErrorCode);
	virtual void OnConnect(int nErrorCode);
	virtual void OnClose(int nErrorCode);

// Implementation
public:
	virtual ~CAsyncSocket();

	static CAsyncSocket* PASCAL LookupHandle(SOCKET hSocket, BOOL bDead = FALSE);
	static void PASCAL AttachHandle(SOCKET hSocket, CAsyncSocket* pSocket, BOOL bDead = FALSE);
	static void PASCAL DetachHandle(SOCKET hSocket, BOOL bDead = FALSE);
	static void PASCAL KillSocket(SOCKET hSocket, CAsyncSocket* pSocket);
	static void PASCAL DoCallBack(WPARAM wParam, LPARAM lParam);

	BOOL Socket(int nSocketType=SOCK_STREAM, long lEvent =
		FD_READ | FD_WRITE | FD_OOB | FD_ACCEPT | FD_CONNECT | FD_CLOSE,
		int nProtocolType = 0, int nAddressFormat = PF_INET);

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	friend class CSocketWnd;

	virtual BOOL ConnectHelper(const SOCKADDR* lpSockAddr, int nSockAddrLen);
	virtual int ReceiveFromHelper(void* lpBuf, int nBufLen,
		SOCKADDR* lpSockAddr, int* lpSockAddrLen, int nFlags);
	virtual int SendToHelper(const void* lpBuf, int nBufLen,
		const SOCKADDR* lpSockAddr, int nSockAddrLen, int nFlags);
};

/////////////////////////////////////////////////////////////////////////////
// CSocket

class CSocket : public CAsyncSocket
{
	DECLARE_DYNAMIC(CSocket);
private:
	CSocket(const CSocket& rSrc);         // no implementation
	void operator=(const CSocket& rSrc);  // no implementation

// Construction
public:
	CSocket();
	BOOL Create(UINT nSocketPort = 0, int nSocketType=SOCK_STREAM,
		LPCTSTR lpszSocketAddress = NULL);

// Attributes
public:
	BOOL IsBlocking();
	static CSocket* PASCAL FromHandle(SOCKET hSocket);
	BOOL Attach(SOCKET hSocket);

// Operations
public:
	void CancelBlockingCall();

// Overridable callbacks
protected:
	virtual BOOL OnMessagePending();

// Implementation
public:
	int m_nTimeOut;

	virtual ~CSocket();

	static int PASCAL ProcessAuxQueue();

	virtual BOOL Accept(CAsyncSocket& rConnectedSocket,
		SOCKADDR* lpSockAddr = NULL, int* lpSockAddrLen = NULL);
	virtual void Close();
	virtual int Receive(void* lpBuf, int nBufLen, int nFlags = 0);
	virtual int Send(const void* lpBuf, int nBufLen, int nFlags = 0);

	int SendChunk(const void* lpBuf, int nBufLen, int nFlags);

protected:
	friend class CSocketWnd;

	BOOL* m_pbBlocking;
	int m_nConnectError;

	virtual BOOL ConnectHelper(const SOCKADDR* lpSockAddr, int nSockAddrLen);
	virtual int ReceiveFromHelper(void* lpBuf, int nBufLen,
		SOCKADDR* lpSockAddr, int* lpSockAddrLen, int nFlags);
	virtual int SendToHelper(const void* lpBuf, int nBufLen,
		const SOCKADDR* lpSockAddr, int nSockAddrLen, int nFlags);

	static void PASCAL AuxQueueAdd(UINT message, WPARAM wParam, LPARAM lParam);

	virtual BOOL PumpMessages(UINT uStopFlag);

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
};

/////////////////////////////////////////////////////////////////////////////
// CSocketFile

class CSocketFile : public CFile
{
	DECLARE_DYNAMIC(CSocketFile)
public:
//Constructors
	CSocketFile(CSocket* pSocket, BOOL bArchiveCompatible = TRUE);

// Implementation
public:
	CSocket* m_pSocket;
	BOOL m_bArchiveCompatible;

	virtual ~CSocketFile();

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	virtual UINT Read(void* lpBuf, UINT nCount);
	virtual void Write(const void* lpBuf, UINT nCount);
	virtual void Close();

// Unsupported APIs
	virtual BOOL Open(LPCTSTR lpszFileName, UINT nOpenFlags, CFileException* pError = NULL);
	virtual CFile* Duplicate() const;
	virtual DWORD GetPosition() const;
	virtual LONG Seek(LONG lOff, UINT nFrom);
	virtual void SetLength(DWORD dwNewLen);
	virtual DWORD GetLength() const;
	virtual void LockRange(DWORD dwPos, DWORD dwCount);
	virtual void UnlockRange(DWORD dwPos, DWORD dwCount);
	virtual void Flush();
	virtual void Abort();
};

/////////////////////////////////////////////////////////////////////////////
// Global functions

BOOL AFXAPI AfxSocketInit(WSADATA* lpwsaData = NULL);
void AFXAPI AfxSocketTerm();

/////////////////////////////////////////////////////////////////////////////
// Inline function declarations

#ifdef _AFX_PACKING
#pragma pack(pop)
#endif

#ifdef _AFX_ENABLE_INLINES
#define _AFXSOCK_INLINE AFX_INLINE
#include <afxsock.inl>
#undef _AFXSOCK_INLINE
#endif

#undef AFX_DATA
#define AFX_DATA

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, on)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, off)
#endif

#endif // __AFXSOCK_H__

/////////////////////////////////////////////////////////////////////////////
