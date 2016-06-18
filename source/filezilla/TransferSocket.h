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

#if !defined(AFX_TRANSFERSOCKET_H__3F95A3A8_478E_45D4_BFD5_6102B42E12DA__INCLUDED_)
#define AFX_TRANSFERSOCKET_H__3F95A3A8_478E_45D4_BFD5_6102B42E12DA__INCLUDED_

#include "FtpListResult.h"	// Hinzugefügt von der Klassenansicht
#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// TransferSocket.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Befehlsziel CTransferSocket 

#include "controlsocket.h"
#include "ApiLog.h"

#ifndef MPEXT_NO_ZLIB
#include <zlib.h>
#endif

class CFtpControlSocket;
class CAsyncProxySocketLayer;
#ifndef MPEXT_NO_SSL
class CAsyncSslSocketLayer;
#endif
#ifndef MPEXT_NO_GSS
class CAsyncGssSocketLayer;
#endif

#define SPEED_SECONDS		60

class CTransferSocket : public CAsyncSocketEx, public CApiLog
{
// Attribute
public:
	CFtpListResult *m_pListResult;

// Operationen
public:
	CTransferSocket(CFtpControlSocket *pOwner, int nMode);
	virtual ~CTransferSocket();

// Überschreibungen
public:
	int m_nInternalMessageID;
	virtual void Close();
	virtual BOOL Create(
#ifndef MPEXT_NO_SSL
    BOOL bUseSsl
#endif
		);
	BOOL m_bListening;
	CFile *m_pFile;
	t_transferdata m_transferdata;
	void SetActive();
	int CheckForTimeout(int delay);
#ifndef MPEXT_NO_GSS
	void UseGSS(CAsyncGssSocketLayer *pGssLayer);
#endif
#ifndef MPEXT_NO_ZLIB
	bool InitZlib(int level);
#endif
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CTransferSocket)
	public:
	virtual void OnReceive(int nErrorCode);
	virtual void OnAccept(int nErrorCode);
	virtual void OnConnect(int nErrorCode);
	virtual void OnClose(int nErrorCode);
	virtual void OnSend(int nErrorCode);
	//}}AFX_VIRTUAL

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CTransferSocket)
		// HINWEIS - Der Klassen-Assistent fügt hier Member-Funktionen ein und entfernt diese.
	//}}AFX_MSG

// Implementierung
protected:
	
	virtual int OnLayerCallback(std::list<t_callbackMsg>& callbacks);
	int ReadDataFromFile(char *buffer, int len);
	virtual void LogSocketMessage(int nMessageType, LPCTSTR pMsgFormat);
	virtual void ConfigureSocket();

	CFtpControlSocket *m_pOwner;
	CAsyncProxySocketLayer* m_pProxyLayer;
#ifndef MPEXT_NO_SSL
	CAsyncSslSocketLayer* m_pSslLayer;
#endif
#ifndef MPEXT_NO_GSS
	CAsyncGssSocketLayer* m_pGssLayer;
#endif
	void UpdateRecvLed();
	void UpdateSendLed();
	void UpdateStatusBar(bool forceUpdate);
	BOOL m_bSentClose;
	int m_bufferpos;
	char *m_pBuffer;
#ifndef MPEXT_NO_ZLIB
	char *m_pBuffer2; // Used by zlib transfers
#endif
	BOOL m_bCheckTimeout;
	CTime m_LastActiveTime;
	BOOL m_bOK;
	CTime m_StartTime;
	BOOL m_nTransferState;
	int m_nMode;
	int m_nNotifyWaiting;
	BOOL m_bShutDown;

	void Transfered(int count, CTime time);
	void CloseAndEnsureSendClose(int Mode);
	void EnsureSendClose(int Mode);
	void CloseOnShutDownOrError(int Mode);
	void LogError(int Error);
	void SetBuffers();

	DWORD m_Transfered[SPEED_SECONDS];
	bool m_UsedForTransfer[SPEED_SECONDS];
	CTime m_TransferedFirst;
	LARGE_INTEGER m_LastUpdateTime;

#ifndef MPEXT_NO_ZLIB
	z_stream m_zlibStream;
	bool m_useZlib;
#endif
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_TRANSFERSOCKET_H__3F95A3A8_478E_45D4_BFD5_6102B42E12DA__INCLUDED_
