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

// Crypt.h: Schnittstelle für die Klasse CCrypt.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_CRYPT_H__613C5174_16F0_42A5_9493_C7489534C080__INCLUDED_)
#define AFX_CRYPT_H__613C5174_16F0_42A5_9493_C7489534C080__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CCrypt  
{
	static char *m_key;
public:
	static CString decrypt(CString str);
	static CString encrypt(CString str);
};

#endif // !defined(AFX_CRYPT_H__613C5174_16F0_42A5_9493_C7489534C080__INCLUDED_)
