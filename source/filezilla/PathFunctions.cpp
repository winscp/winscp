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
#include "pathfunctions.h"

void PathRemoveArgs(CString &path)
{
	path.TrimLeft( _T(" ") );
	path.TrimRight( _T(" ") );
	if (path==_MPT(""))
		return;
	BOOL quoted=FALSE;
	if (path[0]==_MPT('\"'))
		quoted=TRUE;
	int pos=path.ReverseFind(_MPT('\\'));
	if (pos==-1)
		pos=quoted?1:0;

	int i;
	for (i=pos;i<path.GetLength();i++)
	{
		if (path[i]==_MPT('\"'))
			break;
		if (path[i]==_MPT(' ') && !quoted)
			break;
	}
	path = path.Left(i+1);
	path.TrimRight(_MPT(' '));
}

void PathUnquoteSpaces(CString &path)
{
	int pos;
	while ((pos=path.Find(_MPT('\"')))!=-1)
		path.SetAt(pos,_MPT(' '));
	path.TrimLeft( _T(" ") );
	path.TrimRight( _T(" ") );
}

CString PathFindExtension(CString path)
{
	int pos=path.ReverseFind(_MPT('.'));
	if (pos==-1)
		return _MPT("");
	return path.Mid(pos);
}

void PathRemoveFileSpec(CString &path)
{
	CFileStatus64 status;
	if (GetStatus64(path,status))
	{
		if (status.m_attribute&0x10)
		{
			path.TrimRight( _T("\\") );
			path=path+_MPT("\\");
			return;
		}
		else
			path.TrimRight( _T("\\") );
	}
	if (path.Right(1)!=_MPT("\\"))
	{
		path.TrimRight( _T("\\") );
		int pos=path.ReverseFind(_MPT('\\'));
		if (pos==-1)
			path=_MPT("");
		else
			path=path.Left(pos+1);
	}
}

CString PathAppend(CString path, LPCTSTR sub)
{
	ASSERT(sub);

	if (path.Right(1) != _T("\\"))
		path += _T("\\");
	path += sub;

	return path;
}
