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

#include "stdafx.h"

#ifndef MPEXT
#include "MFC64bitFix.h"
#endif

__int64 GetLength64(CFile &file)
{
	DWORD low;
	DWORD high;
	low=GetFileSize((void *)file.m_hFile, &high);
	_int64 size=((_int64)high<<32)+low;
	return size;
}

BOOL GetLength64(CString filename, _int64 &size)
{
	WIN32_FIND_DATA findFileData;
	HANDLE hFind = FindFirstFile(filename, &findFileData);
	if (hFind == INVALID_HANDLE_VALUE)
		return FALSE;
	VERIFY(FindClose(hFind));

	size=((_int64)findFileData.nFileSizeHigh<<32)+findFileData.nFileSizeLow;
	
	return TRUE;	
}

BOOL AFXAPI AfxFullPath(LPTSTR lpszPathOut, LPCTSTR lpszFileIn);

BOOL PASCAL GetStatus64(LPCTSTR lpszFileName, CFileStatus64& rStatus)
{
	// attempt to fully qualify path first
	if (!AfxFullPath(rStatus.m_szFullName, lpszFileName))
	{
		rStatus.m_szFullName[0] = '\0';
		return FALSE;
	}

	WIN32_FIND_DATA findFileData;
	HANDLE hFind = FindFirstFile((LPTSTR)lpszFileName, &findFileData);
	if (hFind == INVALID_HANDLE_VALUE)
		return FALSE;
	VERIFY(FindClose(hFind));

	// strip attribute of NORMAL bit, our API doesn't have a "normal" bit.
	rStatus.m_attribute = (BYTE)
		(findFileData.dwFileAttributes & ~FILE_ATTRIBUTE_NORMAL);

	rStatus.m_size = ((_int64)findFileData.nFileSizeHigh<<32)+findFileData.nFileSizeLow;

	// convert times as appropriate
	TRY
	{
		rStatus.m_ctime = CTime(findFileData.ftCreationTime);
		rStatus.m_has_ctime = true;
	}
	CATCH_ALL(e)
	{
		rStatus.m_has_ctime = false;
	}
	END_CATCH_ALL;

	TRY
	{
		rStatus.m_atime = CTime(findFileData.ftLastAccessTime);
		rStatus.m_has_atime = true;
	}
	CATCH_ALL(e)
	{
		rStatus.m_has_atime = false;
	}
	END_CATCH_ALL;

	TRY
	{
		rStatus.m_mtime = CTime(findFileData.ftLastWriteTime);
		rStatus.m_has_mtime = true;
	}
	CATCH_ALL(e)
	{
		rStatus.m_has_mtime = false;
	}
	END_CATCH_ALL;

	if (!rStatus.m_has_ctime || rStatus.m_ctime.GetTime() == 0)
	{
		if (rStatus.m_has_mtime)
		{
			rStatus.m_ctime = rStatus.m_mtime;
			rStatus.m_has_ctime = true;
		}
		else
			rStatus.m_has_ctime = false;
	}


	if (!rStatus.m_has_atime || rStatus.m_atime.GetTime() == 0)
	{
		if (rStatus.m_has_mtime)
		{
			rStatus.m_atime = rStatus.m_mtime;
			rStatus.m_has_atime = true;
		}
		else
			rStatus.m_has_atime = false;
	}

	if (!rStatus.m_has_mtime || rStatus.m_mtime.GetTime() == 0)
	{
		if (rStatus.m_has_ctime)
		{
			rStatus.m_mtime = rStatus.m_ctime;
			rStatus.m_has_mtime = true;
		}
		else
			rStatus.m_has_mtime = false;
	}

	return TRUE;
}

_int64 GetPosition64(CFile &file)
{
	LONG low=0;
	LONG high=0;
	low=SetFilePointer((HANDLE)file.m_hFile, low, &high, FILE_CURRENT);
	if (low==0xFFFFFFFF && GetLastError!=NO_ERROR)
		CFileException::ThrowOsError((LONG)::GetLastError());
	return ((_int64)high<<32)+low;
}