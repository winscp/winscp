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

// ApiLog.h: Schnittstelle für die Klasse CApiLog.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_APILOG_H__D9CD5BA6_3B94_4BF4_BF59_BD20CEB61811__INCLUDED_)
#define AFX_APILOG_H__D9CD5BA6_3B94_4BF4_BF59_BD20CEB61811__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CApiLog  
{
public:
	CApiLog();
	virtual ~CApiLog();

	BOOL InitLog(HWND hTargerWnd, int nLogMessage);
	BOOL InitLog(CApiLog *pParent);

	void LogMessage(int nMessageType, LPCTSTR pMsgFormat, ...) const;
	void LogMessageRaw(int nMessageType, LPCTSTR pMsg) const;
	void LogMessage(int nMessageType, UINT nFormatID, ...) const;
	void LogMessage(CString SourceFile, int nSourceLine, void *pInstance, int nMessageType, LPCTSTR pMsgFormat, ...) const;
	void LogMessageRaw(CString SourceFile, int nSourceLine, void *pInstance, int nMessageType, LPCTSTR pMsg) const;

	BOOL SetDebugLevel(int nLogLevel);
	int GetDebugLevel();
protected:
#ifdef MPEXT
	virtual BOOL PostMessage(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam) const;
#endif
	void SendLogMessage(int nMessageType, LPCTSTR pMsg) const;
	int m_nDebugLevel;
	CApiLog *m_pApiLogParent; //Pointer to topmost parent
	int m_nLogMessage;
	HWND m_hTargetWnd;
};

#endif // !defined(AFX_APILOG_H__D9CD5BA6_3B94_4BF4_BF59_BD20CEB61811__INCLUDED_)
