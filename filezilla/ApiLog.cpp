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

// ApiLog.cpp: Implementierung der Klasse CApiLog.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "ApiLog.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CApiLog::CApiLog()
{
	m_hTargetWnd=0;
	m_pApiLogParent=0;
	m_nDebugLevel=0;
}

CApiLog::~CApiLog()
{

}

BOOL CApiLog::InitLog(CApiLog *pParent)
{
	if (!pParent)
		return FALSE;
	while (pParent->m_pApiLogParent)
		pParent=pParent->m_pApiLogParent;
	m_hTargetWnd=0;
	m_pApiLogParent=pParent;
	return TRUE;
}

BOOL CApiLog::InitLog(HWND hTargetWnd, int nLogMessage)
{
	if (!hTargetWnd)
		return FALSE;
	m_hTargetWnd=hTargetWnd;
	m_nLogMessage=nLogMessage;
	m_pApiLogParent=0;
	return TRUE;
}

void CApiLog::LogMessage(int nMessageType, LPCTSTR pMsgFormat, ...) const
{
	ASSERT(nMessageType>=0 || nMessageType<=8);
	ASSERT(m_hTargetWnd || m_pApiLogParent);
	if (nMessageType>=FZ_LOG_APIERROR && (nMessageType-FZ_LOG_APIERROR)>=m_pApiLogParent->m_nDebugLevel)
		return;

	va_list ap;
    
    va_start(ap, pMsgFormat);
    CString text;
	text.FormatV(pMsgFormat, ap);
	va_end(ap);
	
	SendLogMessage(nMessageType, text);
}

void CApiLog::LogMessageRaw(int nMessageType, LPCTSTR pMsg) const
{
	ASSERT(nMessageType>=0 || nMessageType<=8);
	ASSERT(m_hTargetWnd || m_pApiLogParent);
	if (nMessageType>=FZ_LOG_APIERROR && (nMessageType-FZ_LOG_APIERROR)>=m_pApiLogParent->m_nDebugLevel)
		return;
	
	SendLogMessage(nMessageType, pMsg);
}

void CApiLog::LogMessage(int nMessageType, UINT nFormatID, ...) const
{
	ASSERT(nMessageType>=0 || nMessageType<=8);
	ASSERT(m_hTargetWnd || m_pApiLogParent);
	if (nMessageType>=FZ_LOG_APIERROR && (nMessageType-FZ_LOG_APIERROR)>=m_pApiLogParent->m_nDebugLevel)
		return;

	CString str;
	str.LoadString(nFormatID);

	va_list ap;
    
    va_start(ap, nFormatID);
    CString text;
	text.FormatV(str, ap);
	va_end(ap);
	
	SendLogMessage(nMessageType, text);
}

void CApiLog::LogMessage(CString SourceFile, int nSourceLine, void *pInstance, int nMessageType, LPCTSTR pMsgFormat, ...) const
{
	ASSERT(nMessageType>=4 || nMessageType<=8);
	ASSERT(m_hTargetWnd || m_pApiLogParent);
	ASSERT(nSourceLine>0);


	int pos=SourceFile.ReverseFind('\\');
	if (pos!=-1)
		SourceFile=SourceFile.Mid(pos+1);
	
	va_list ap;
    
	va_start(ap, pMsgFormat);
	CString text;
	text.FormatV(pMsgFormat, ap);
	va_end(ap);

	CString msg;
	msg.Format(_T("%s(%d): %s   caller=0x%08x"), SourceFile, nSourceLine, text, (int)this);
	
	SendLogMessage(nMessageType, msg);
}

#ifdef MPEXT
BOOL CApiLog::PostMessage(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam) const
{
	return m_pApiLogParent->PostMessage(hWnd, Msg, wParam, lParam);
}
#endif

void CApiLog::LogMessageRaw(CString SourceFile, int nSourceLine, void *pInstance, int nMessageType, LPCTSTR pMsg) const
{
	ASSERT(nMessageType>=4 || nMessageType<=8);
	ASSERT(m_hTargetWnd || m_pApiLogParent);
	ASSERT(nSourceLine>0);

	int pos=SourceFile.ReverseFind('\\');
	if (pos!=-1)
		SourceFile=SourceFile.Mid(pos+1);
	
	CString msg;
	msg.Format(_T("%s(%d): %s   caller=0x%08x"), SourceFile, nSourceLine, pMsg, (int)this);
	
	SendLogMessage(nMessageType, msg);
}

void CApiLog::SendLogMessage(int nMessageType, LPCTSTR pMsg) const
{
#ifdef MPEXT
	ASSERT(m_pApiLogParent);
	ASSERT(m_pApiLogParent->m_hTargetWnd == 0);
	ASSERT(m_pApiLogParent->m_nLogMessage == 0);
	if (nMessageType>=FZ_LOG_APIERROR && (nMessageType-FZ_LOG_APIERROR)>=m_pApiLogParent->m_nDebugLevel)
		return;
	//Displays a message in the message log	
	t_ffam_statusmessage *pStatus = new t_ffam_statusmessage;
	pStatus->post = TRUE;
	pStatus->status = pMsg;
	pStatus->type = nMessageType;
	if (!this->PostMessage(m_pApiLogParent->m_hTargetWnd, m_pApiLogParent->m_nLogMessage, FZ_MSG_MAKEMSG(FZ_MSG_STATUS, 0), (LPARAM)pStatus))
		delete pStatus;
#else
	if (m_hTargetWnd)
	{
		ASSERT(m_nLogMessage);
		if (nMessageType>=FZ_LOG_APIERROR && (nMessageType-FZ_LOG_APIERROR)>=m_nDebugLevel)
			return;
	}
	else
	{
		ASSERT(m_pApiLogParent);
		ASSERT(m_pApiLogParent->m_hTargetWnd);
		ASSERT(m_pApiLogParent->m_nLogMessage);
		if (nMessageType>=FZ_LOG_APIERROR && (nMessageType-FZ_LOG_APIERROR)>=m_pApiLogParent->m_nDebugLevel)
			return;
	}
	//Displays a message in the message log	
	t_ffam_statusmessage *pStatus = new t_ffam_statusmessage;
	pStatus->post = TRUE;
	pStatus->status = pMsg;
	pStatus->type = nMessageType;
	if (m_hTargetWnd)
	{
		if (!PostMessage(m_hTargetWnd, m_nLogMessage, FZ_MSG_MAKEMSG(FZ_MSG_STATUS, 0), (LPARAM)pStatus))
			delete pStatus;
	}
	else
		if (!PostMessage(m_pApiLogParent->m_hTargetWnd, m_pApiLogParent->m_nLogMessage, FZ_MSG_MAKEMSG(FZ_MSG_STATUS, 0), (LPARAM)pStatus))
			delete pStatus;
#endif
}

BOOL CApiLog::SetDebugLevel(int nDebugLevel)
{
	if (m_pApiLogParent)
		return FALSE;
	if (nDebugLevel<0 || nDebugLevel>4)
		return FALSE;
	m_nDebugLevel=nDebugLevel;
	return TRUE;
}

int CApiLog::GetDebugLevel()
{
	if (m_pApiLogParent)
		return m_pApiLogParent->m_nDebugLevel;
	return m_nDebugLevel;
}