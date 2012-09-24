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

#ifndef MPExtStructuresH
#define MPExtStructuresH

#pragma once

class CServerPath;

#include "FileZillaApi.h"

class t_directory
{
public:
	t_directory();
	~t_directory();
	CServerPath path;
	int num;
	class t_direntry
	{
	public:
		t_direntry();
		bool bUnsure; //Set by CControlSocket::FileTransfer when uploads fail after sending STOR/APPE
		CString name;
		CString lName; //Name in lowercase characters
		CString permissionstr;
		CString ownergroup;
		__int64 size;
		bool dir;
		bool bLink;
		class t_date
		{
		public:
			t_date();
			int year,month,day,hour,minute,second;
			bool hastime;
			bool hasseconds;
			bool hasdate;
		} date;
		CTime EntryTime;
		CString linkTarget;
	} *direntry;
	void Merge(const t_directory &directory, CTime MergeTime);
	t_server server;
#ifndef MPEXT_NO_CACHE
	BOOL bCached;
#endif
	t_directory& operator=(const t_directory &a);
};

#ifndef MPEXT
//View headers
class CComboCompletion;

typedef struct
{	SIZE m_LabelTextSize;
	CComboCompletion *m_pEdit;
	CStatic *m_pLabelBackground;
	CStatic *m_pLabel;
	BOOL bTreeHidden;
} t_LocalViewHeader;

typedef struct
{	SIZE m_LabelTextSize;
	CComboCompletion *m_pEdit;
	CStatic *m_pLabelBackground;
	CStatic *m_pLabel;
	BOOL bTreeHidden;
} t_RemoteViewHeader;
#endif

#endif // MPExtStructuresH
