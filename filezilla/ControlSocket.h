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

// ControlSocket.h: Schnittstelle für die Klasse CControlSocket.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_CONTROLSOCKET_H__173B4310_5E72_4F4B_A4F0_4E3D4CFAF4A5__INCLUDED_)
#define AFX_CONTROLSOCKET_H__173B4310_5E72_4F4B_A4F0_4E3D4CFAF4A5__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "AsyncSocketEx.h"
#include "FileZillaApi.h"
#include "ApiLog.h"
#include "structures.h"	// Hinzugefügt von der Klassenansicht
#ifndef MPEXT_NO_IDENT
#include "IdentServerControl.h"	// Added by ClassView
#endif

#define CSMODE_NONE				0x0000
#define CSMODE_CONNECT			0x0001
#define CSMODE_COMMAND			0x0002
#define CSMODE_LIST				0x0004
#define CSMODE_TRANSFER			0x0008
#define CSMODE_DOWNLOAD			0x0010
#define CSMODE_UPLOAD			0x0020
#define CSMODE_TRANSFERERROR	0x0040
#define CSMODE_TRANSFERTIMEOUT	0x0080
#define CSMODE_DELETE			0x0100
#define CSMODE_RMDIR			0x0200
#define CSMODE_DISCONNECT		0x0400
#define CSMODE_MKDIR			0x0800
#define CSMODE_RENAME			0x1000
#define CSMODE_CHMOD			0x2000
#ifdef MPEXT
#define CSMODE_LISTFILE			0x4000
#endif

typedef struct
{
	BOOL bResume,bType;
	__int64 transfersize,transferleft,nTransferStart;
} t_transferdata;

class CMainThread;
class CAsyncProxySocketLayer;
#ifndef MPEXT_NO_SSL
class CAsyncSslSocketLayer;
#endif
#ifndef MPEXT_NO_GSS
class CAsyncGssSocketLayer;
#endif
class CTransferSocket;

#define BUFSIZE                             16384

class CControlSocket : public CAsyncSocketEx, public CApiLog
{
public:
	CControlSocket(CMainThread *pMainThread);
	virtual ~CControlSocket();

	enum transferDirection
	{
		download = 0,
		upload = 1
	};

	//Operations
	virtual void Connect(t_server &server)=0;
	virtual void List(BOOL bFinish, int nError=0, CServerPath path=CServerPath(), CString subdir=_MPT(""), int nListMode = 0)=0;
#ifdef MPEXT
	virtual void ListFile(CServerPath path=CServerPath(), CString fileName = _MPT(""))=0;
#endif
	virtual void FtpCommand(LPCTSTR pCommand)=0;
	virtual void Disconnect()=0;
	virtual void FileTransfer(t_transferfile *transferfile = 0, BOOL bFinish = FALSE, int nError = 0)=0;
	virtual void Delete(CString filename, const CServerPath &path)=0;
	virtual void Rename(CString oldName, CString newName, const CServerPath &path, const CServerPath &newPath)=0;
	virtual void MakeDir(const CServerPath &path)=0;
	virtual void RemoveDir(CString dirname, const CServerPath &path)=0;
	virtual void Cancel(BOOL bQuit=FALSE)=0;
	virtual void Chmod(CString filename, const CServerPath &path, int nValue)=0;
	
	virtual void SetAsyncRequestResult(int nAction, CAsyncRequestData *pData)=0;
	
	virtual void OnTimer()=0; //Called every 1000 msecs
	virtual BOOL IsReady()=0; //ALWAYS return return TRUE if processing a command (Return false if keepalive is in progress for example)
	virtual void ProcessReply()=0;
	virtual void TransferEnd(int nMode)=0;
	virtual void DoClose(int nErrorCode = 0)=0;
	
	t_server GetCurrentServer();
	void ShowStatus(UINT nID, int type) const;
	void ShowStatus(CString status,int type) const;

	virtual int OnLayerCallback(std::list<t_callbackMsg>& callbacks);
protected:

	void Close();
	BOOL Connect(CString hostAddress, UINT nHostPort);
	CString ConvertDomainName(CString domain);

	void SetDirectoryListing(t_directory *pDirectory, bool bSetWorkingDir = true);
	t_directory *m_pDirectoryListing;

	CMainThread *m_pOwner;
#ifndef MPEXT_NO_IDENT
	CIdentServerControl *m_pIdentControl;
#endif

	//Speed limit
public:
	BOOL RemoveActiveTransfer();
	BOOL SpeedLimitAddTransferredBytes(enum transferDirection direction, _int64 nBytesTransferred);
	
	_int64 GetSpeedLimit(enum transferDirection direction, CTime &time);

	_int64 GetAbleToTransferSize(enum transferDirection direction, bool &beenWaiting, int nBufSize = 0);

protected:
	struct t_ActiveList
	{
		CControlSocket *pOwner;
		__int64 nBytesAvailable;
		__int64 nBytesTransferred;
	};
	static std::list<t_ActiveList> m_InstanceList[2];
	static CTime m_CurrentTransferTime[2];
	static _int64 m_CurrentTransferLimit[2];
	static CCriticalSection m_SpeedLimitSync;
	_int64 GetAbleToUDSize( bool &beenWaiting, CTime &curTime, _int64 &curLimit, std::list<t_ActiveList>::iterator &iter, enum transferDirection direction, int nBufSize);
#ifndef MPEXT_NO_SPEED_LIM_RULES
	_int64 GetSpeedLimit(CTime &time, int valType, int valValue, SPEEDLIMITSLIST &list);
#else
	_int64 GetSpeedLimit(CTime &time, int valType, int valValue);
#endif
	//End Speed limit
	
	virtual void LogSocketMessage(int nMessageType, LPCTSTR pMsgFormat);

public:
	struct t_operation
	{
		int nOpMode;
		int nOpState;
		class COpData //Base class which will store operation specific parameters.
		{
		public:
			COpData() {};
			virtual ~COpData() {};
		};
		COpData *pData;
	public:
	};
protected:
	t_operation m_Operation;

	CAsyncProxySocketLayer* m_pProxyLayer;
#ifndef MPEXT_NO_SSL
	CAsyncSslSocketLayer* m_pSslLayer;
#endif
#ifndef MPEXT_NO_GSS
	CAsyncGssSocketLayer* m_pGssLayer;
#endif
	t_server m_CurrentServer;
};

#endif // !defined(AFX_CONTROLSOCKET_H__173B4310_5E72_4F4B_A4F0_4E3D4CFAF4A5__INCLUDED_)
