// FileZilla - a Windows ftp client

// Copyright (C) 2002-2004 - Tim Kosse <tim.kosse@gmx.de>

// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

#if !defined(AFX_FTPCONTROLSOCKET_H__AE6AA44E_B09D_487A_8EF2_A23697434945__INCLUDED_)
#define AFX_FTPCONTROLSOCKET_H__AE6AA44E_B09D_487A_8EF2_A23697434945__INCLUDED_

#include "structures.h"	// Hinzugefügt von der Klassenansicht
#include "StdAfx.h"	// Hinzugefügt von der Klassenansicht
#include "FileZillaApi.h"
#include "FileZillaIntf.h"
#include "ControlSocket.h"
#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// FtpControlSocket.h : Header-Datei
//

class CTransferSocket;
class CMainThread;
/////////////////////////////////////////////////////////////////////////////
// Befehlsziel CFtpControlSocket 

class CAsyncProxySocketLayer;
class CMainThread;
class CFtpControlSocket : public CControlSocket
{
	friend CTransferSocket;
// Attribute
public:

// Operationen
public:
	CFtpControlSocket(CMainThread *pMainThread, CFileZillaTools * pTools);
	virtual ~CFtpControlSocket();

// Überschreibungen
public:
	virtual void Connect(t_server &server);
	virtual void OnTimer();
	virtual BOOL IsReady();
	virtual void List(BOOL bFinish, int nError=0, CServerPath path=CServerPath(), CString subdir=_MPT(""), int nListMode = 0);
#ifdef MPEXT
	virtual void ListFile(CServerPath path=CServerPath(), CString fileName="");
#endif
	virtual void FtpCommand(LPCTSTR pCommand);
	virtual void Disconnect();
	virtual void FileTransfer(t_transferfile *transferfile = 0, BOOL bFinish = FALSE, int nError = 0);
	virtual void Delete(CString filename, const CServerPath &path);
	virtual void Rename(CString oldName, CString newName, const CServerPath &path, const CServerPath &newPath);
	virtual void MakeDir(const CServerPath &path);
	virtual void RemoveDir(CString dirname, const CServerPath &path);
	virtual void Chmod(CString filename, const CServerPath &path, int nValue);
		
	virtual void ProcessReply();
	virtual void TransferEnd(int nMode);
	virtual void Cancel(BOOL bQuit=FALSE);

	virtual void SetAsyncRequestResult(int nAction, CAsyncRequestData *pData);
	
	
	int CheckOverwriteFile();
	virtual BOOL Create();
	void TransfersocketListenFinished(unsigned int ip,unsigned short port);
	
	BOOL m_bKeepAliveActive;
#ifndef MPEXT_NO_SSL
	BOOL m_bDidRejectCertificate;
#endif
	// Some servers are broken. Instead of an empty listing, some MVS servers
	// for example they return something "550 no members found"
	// Other servers return "550 No files found."
	bool IsMisleadingListResponse();

#ifdef MPEXT
	virtual bool UsingMlsd();
	virtual std::string GetTlsVersionStr();
	virtual std::string GetCipherName();
#endif

	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CFtpControlSocket)
	public:
	virtual void OnReceive(int nErrorCode);
	virtual void OnConnect(int nErrorCode);
	virtual void OnClose(int nErrorCode);
	virtual void OnSend(int nErrorCode);
	//}}AFX_VIRTUAL

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CFtpControlSocket)
		// HINWEIS - Der Klassen-Assistent fügt hier Member-Funktionen ein und entfernt diese.
	//}}AFX_MSG

// Implementierung
protected:
	//Called by OnTimer()
	void ResumeTransfer();
	void CheckForTimeout();
	void SendKeepAliveCommand();

	virtual int OnLayerCallback(std::list<t_callbackMsg>& callbacks);
	void SetFileExistsAction(int nAction, COverwriteRequestData *pData);
#ifndef MPEXT_NO_SSL
	void SetVerifyCertResult( int nResult, t_SslCertData *pData );
#endif
	void ResetOperation(int nSuccessful = -1);

	virtual void DoClose(int nError = 0);
	int GetReplyCode();
	CString GetReply();
	void LogOnToServer(BOOL bSkipReply = FALSE);
	BOOL Send(CString str);
	
	BOOL ParsePwdReply(CString& rawpwd);

	void DiscardLine(CStringA line);
	bool NeedModeCommand();
	bool NeedOptsCommand();
	CString GetListingCmd();

	bool InitConnect();

#ifdef MPEXT
	bool IsRoutableAddress(const CString & host);
	bool CheckForcePasvIp(CString & host);
	void TransferFinished(bool preserveFileTimeForUploads);
#endif

	CFile *m_pDataFile;
	CTransferSocket *m_pTransferSocket;
	CStringA m_MultiLine;
	CTime m_LastSendTime;
	
	CString m_ServerName;
	std::list<CStringA> m_RecvBuffer;
	CTime m_LastRecvTime;
	class CLogonData;
	class CListData;
	class CFileTransferData;
	class CMakeDirData;

#ifndef MPEXT_NO_ZLIB
	bool m_useZlib;
	bool m_zlibSupported;
	int m_zlibLevel;
#endif

	bool m_bUTF8;
	bool m_bAnnouncesUTF8;
	bool m_hasClntCmd;
#ifdef MPEXT
	TFTPServerCapabilities m_serverCapabilities;
	CStringA m_ListFile;
#endif
	bool m_isFileZilla;

	bool m_awaitsReply;
	bool m_skipReply;

	char* m_sendBuffer;
	int m_sendBufferLen;

	bool m_bProtP;

	bool m_mayBeMvsFilesystem;
	bool m_mayBeBS2000Filesystem;

private:
	BOOL m_bCheckForTimeout;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_FTPCONTROLSOCKET_H__AE6AA44E_B09D_487A_8EF2_A23697434945__INCLUDED_
