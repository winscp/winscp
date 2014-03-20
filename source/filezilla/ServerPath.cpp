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

// ServerPath.cpp: Implementierung der Klasse CServerPath.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "ServerPath.h"
#include "structures.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

#define FTP_MVS_DOUBLE_QUOTA (TCHAR)0xDC

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CServerPath::CServerPath()
{
	m_nServerType = 0;
	m_bEmpty = TRUE;
}

CServerPath::CServerPath(int nServerType)
{
	m_nServerType = nServerType;
	m_bEmpty = TRUE;
}

CServerPath::CServerPath(CString path)
{
	m_nServerType = FZ_SERVERTYPE_FTP;
	path.TrimLeft( _T(" ") );
	path.TrimRight( _T(" ") );
	if (path == _MPT(""))
	{
		m_bEmpty = TRUE;
		return;
	}
	else
		m_bEmpty = FALSE;

	int pos1 = path.Find( _T(":[") );
	if (pos1 != -1 && path.Right(1) == _MPT("]") && pos1 != (path.GetLength()-1))
		m_nServerType |= FZ_SERVERTYPE_SUB_FTP_VMS;
	else if (path.GetLength() >= 3 && _istalpha(path[0]) && path[1] == _MPT(':') && (path[2] == _MPT('\\') || path[2] == _MPT('/')))
		m_nServerType |= FZ_SERVERTYPE_SUB_FTP_WINDOWS;
	else if (path[0] == FTP_MVS_DOUBLE_QUOTA && path[path.GetLength() - 1] == FTP_MVS_DOUBLE_QUOTA)
		m_nServerType |= FZ_SERVERTYPE_SUB_FTP_MVS;
	else if (path.GetLength() > 2 && path[0] == _MPT('\'') && path.Right(1) == _T("'") && path.Find(_MPT('/')) == -1 && path.Find(_MPT('\\')) == -1)
		m_nServerType |= FZ_SERVERTYPE_SUB_FTP_MVS;

	*this = CServerPath(path, m_nServerType);
}

CServerPath::CServerPath(CString path, int nServerType)
{
	m_nServerType = nServerType;
	path.TrimLeft( _T(" ") );
	path.TrimRight( _T(" ") );
	if (path == _MPT(""))
	{
		m_bEmpty = TRUE;
		return;
	}
	else
		m_bEmpty = FALSE;

	switch (m_nServerType&FZ_SERVERTYPE_HIGHMASK)
	{
	case FZ_SERVERTYPE_FTP:
		switch(m_nServerType&FZ_SERVERTYPE_SUBMASK)
		{
		case FZ_SERVERTYPE_SUB_FTP_MVS:
		case FZ_SERVERTYPE_SUB_FTP_BS2000:
			{
				path.TrimLeft(FTP_MVS_DOUBLE_QUOTA);
				path.TrimRight(FTP_MVS_DOUBLE_QUOTA);
				path.TrimLeft(_MPT('\''));
				path.TrimRight(_MPT('\''));
				path.TrimLeft(_MPT('.'));
				while (path.Replace(_T(".."), _T(".")));

				int pos = path.Find(_T("."));
				while (pos != -1)
				{
					m_Segments.push_back(path.Left(pos));
					path = path.Mid(pos + 1);
					pos = path.Find( _T(".") );
				}
				if (path != _T(""))
					m_Segments.push_back(path);
				else
					m_Prefix = _T(".");
			}
			break;
		case FZ_SERVERTYPE_SUB_FTP_VMS:
			{
				int pos1 = path.Find( _T("[") );
				if (pos1 == -1 || path.Right(1) != _T("]"))
				{
					ASSERT(FALSE);
					m_bEmpty = TRUE;
					return;
				}
				path.TrimRight( _T("]") );
				if (pos1)
					m_Prefix = path.Left(pos1);
				path = path.Mid(pos1 + 1);
				int pos = path.Find( _T(".") );
				while (pos != -1)
				{
					m_Segments.push_back(path.Left(pos));
					path = path.Mid(pos+1);
					pos = path.Find( _T(".") );
				}
				if (path != _MPT(""))
					m_Segments.push_back(path);
			}
			break;
		default:
			path.Replace( _T("\\"), _T("/") );
			while (path.Replace( _T("//"), _T("/") ));
			path.TrimLeft( _T("/") );
			path.TrimRight( _T("/") );
			int pos = path.Find( _T("/") );
			while (pos != -1)
			{
				m_Segments.push_back(path.Left(pos));
				path = path.Mid(pos+1);
				pos = path.Find( _T("/") );
			}
			if (path != _MPT(""))
				m_Segments.push_back(path);
			break;
		}
		break;
	case FZ_SERVERTYPE_LOCAL:
		{
			path.TrimRight( _T("\\") );
			while (path.Replace( _T("\\\\"), _T("\\") ));
			int pos = path.Find( _T("\\") );
			if (pos == -1)
			{
				m_Prefix = path;
				return;
			}
			ASSERT(pos == 2);
			m_Prefix = path.Left(pos);
			path = path.Mid(pos + 1);
			pos = path.Find( _T("\\") );
			while (pos != -1)
			{
				m_Segments.push_back(path.Left(pos));
				path=path.Mid(pos + 1);
				pos=path.Find( _T("\\") );
			}
			if (path != _MPT(""))
				m_Segments.push_back(path);
		}
		break;
	default:
		ASSERT(FALSE);
	}
}

CServerPath::CServerPath(const CServerPath &path)
{
	m_nServerType = path.m_nServerType;
	m_Prefix = path.m_Prefix;
	m_bEmpty = path.m_bEmpty;
	m_Segments = path.m_Segments;
}

CServerPath::~CServerPath()
{

}

void CServerPath::SetServer(const t_server &server)
{
	m_nServerType=server.nServerType;
}

BOOL CServerPath::SetPath(CString &newpath, BOOL bIsFile /*=FALSE*/)
{
	CString file;
	CString path=newpath;

	path.TrimLeft( _T(" ") );
	path.TrimRight( _T(" ") );
	if (path != _MPT(""))
		m_bEmpty = FALSE;
	else
		m_bEmpty = TRUE;
	if (!(m_nServerType & FZ_SERVERTYPE_HIGHMASK))
		m_nServerType = FZ_SERVERTYPE_FTP;
	if (!(m_nServerType&FZ_SERVERTYPE_SUBMASK) && (m_nServerType&FZ_SERVERTYPE_HIGHMASK)==FZ_SERVERTYPE_FTP)
	{
		int pos1 = path.Find( _T(":[") );
		if (pos1!=-1 && pos1!=(path.GetLength()-2))
		{
			if (!bIsFile && path.Right(1)==_T("]"))
				m_nServerType|=FZ_SERVERTYPE_SUB_FTP_VMS;
			else if (bIsFile && path.ReverseFind(']')>(pos1+1))
				m_nServerType|=FZ_SERVERTYPE_SUB_FTP_VMS;
		}
		if (newpath.GetLength() >= 3 && _istalpha(newpath[0]) && newpath[1] == _MPT(':') && (newpath[2] == _MPT('\\') || newpath[2] == _MPT('/')))
			m_nServerType |= FZ_SERVERTYPE_SUB_FTP_WINDOWS;
		else if (path[0] == FTP_MVS_DOUBLE_QUOTA && path[path.GetLength() - 1] == FTP_MVS_DOUBLE_QUOTA)
			m_nServerType |= FZ_SERVERTYPE_SUB_FTP_MVS;
	}
	m_Segments.clear();
	m_Prefix = _MPT("");
	switch (m_nServerType&FZ_SERVERTYPE_HIGHMASK)
	{
		case FZ_SERVERTYPE_FTP:
			switch (m_nServerType&FZ_SERVERTYPE_SUBMASK)
			{
			case FZ_SERVERTYPE_SUB_FTP_MVS:
			case FZ_SERVERTYPE_SUB_FTP_BS2000:
				{
					path.TrimLeft(FTP_MVS_DOUBLE_QUOTA);
					path.TrimRight(FTP_MVS_DOUBLE_QUOTA);
					path.TrimLeft(_MPT('\''));
					path.TrimRight(_MPT('\''));
					path.TrimLeft(_MPT('.'));
					while (path.Replace(_T(".."), _T(".")));

					int pos = path.Find(_T("."));
					while (pos != -1)
					{
						m_Segments.push_back(path.Left(pos));
						path = path.Mid(pos + 1);
						pos = path.Find( _T(".") );
					}
					if (path != _MPT(""))
						m_Segments.push_back(path);
					else
						m_Prefix = _T(".");

					if (bIsFile)
					{
						if (m_Segments.empty())
							return FALSE;
						file = m_Segments.back();
						m_Segments.pop_back();

						if (file.Right(1) == _T("."))
							return FALSE;

						int pos = file.Find(_MPT('('));
						int pos2 = file.Find(_MPT(')'));
						if (pos != -1)
						{
							if (!pos || pos2 != file.GetLength() - 2)
								return FALSE;
							m_Prefix = _T("");
							m_Segments.push_back(file.Left(pos));
							file = file.Mid(pos + 1, pos2 - pos - 1);
						}
						else if (pos2 != -1)
							return FALSE;
					}
				}
				break;
			case FZ_SERVERTYPE_SUB_FTP_VMS:
				{
					int pos1=path.Find( _T("[") );
					if (pos1==-1)
						return FALSE;
					if (bIsFile)
					{
						int rpos=path.ReverseFind(_MPT(']'));
						if (rpos==-1)
							return FALSE;
						else if (rpos!=(path.GetLength()-1) )
						{
							file=file.Mid(rpos+1);
							path=path.Left(rpos+1);
						}
						else
							return FALSE;
					}
					if (path.Right(1)!=_MPT("]"))
						return FALSE;
					path.TrimRight( _T("]") );
					if (pos1)
						m_Prefix=path.Left(pos1);
					path=path.Mid(pos1+1);
					int pos=path.Find( _T(".") );
					while(pos!=-1)
					{
						m_Segments.push_back(path.Left(pos));
						path=path.Mid(pos+1);
						pos=path.Find( _T(".") );
					}
					if (path!=_MPT(""))
						m_Segments.push_back(path);
				}
				break;
			default:
				path.Replace( _T("\\"), _T("/") );
				while(path.Replace( _T("//"), _T("/") ));
				path.TrimLeft( _T("/") );
				if (bIsFile)
				{
					if (path.Right(1)!= _T("/") )
					{
						int rpos=path.ReverseFind(_MPT('/'));
						if (rpos==-1)
						{
							newpath=path;
							m_bEmpty=TRUE;
							return TRUE;
						}
						file=path.Mid(rpos+1);
						path=path.Left(rpos);
					}
					else
						return FALSE;
				}
				path.TrimRight( _T("/") );
				int pos=path.Find( _T("/") );
				while(pos!=-1)
				{
					m_Segments.push_back(path.Left(pos));
					path=path.Mid(pos+1);
					pos=path.Find( _T("/") );
				}
				if (path!=_MPT(""))
					m_Segments.push_back(path);
				break;
			}
			break;
		case FZ_SERVERTYPE_LOCAL:
		{
			if (bIsFile)
			{
				if (path.Right(1)!= _T("\\") )
				{
					int rpos=path.ReverseFind(_MPT('\\'));
					if (rpos==-1)
						return FALSE;
					
					file=path.Mid(rpos+1);
					path=path.Left(rpos);
				}
				else
					return FALSE;
			}
			path.TrimRight( _T("\\") );
			while (path.Replace( _T("\\\\"), _T("\\") ));
			int pos=path.Find( _T(":\\") );
			if (pos==-1 || pos!=1)
				return FALSE;
			else
			{
				m_Prefix=path.Left(pos+1);
				path=path.Mid(pos+2);
			}
			pos=path.Find( _T("\\") );
			while (pos!=-1)
			{
				m_Segments.push_back(path.Left(pos));
				path=path.Mid(pos+1);
				pos=path.Find( _T("\\") );
			}
			if (path!=_MPT(""))
				m_Segments.push_back(path);
		}
		break;
	}
	if (bIsFile)
		newpath = file;
	return TRUE;
}

const CString CServerPath::GetPath() const
{
	if (m_bEmpty)
		return _MPT("");
	CString path;
	tConstIter iter;
	switch (m_nServerType&FZ_SERVERTYPE_HIGHMASK)
	{
	case FZ_SERVERTYPE_FTP:
		switch (m_nServerType&FZ_SERVERTYPE_SUBMASK)
		{
		case FZ_SERVERTYPE_SUB_FTP_MVS:
		case FZ_SERVERTYPE_SUB_FTP_BS2000:
			path = _MPT("'");
			for (iter = m_Segments.begin(); iter != m_Segments.end(); iter++)
			{
				if (iter != m_Segments.begin())
					path += _T(".");
				path += *iter;
			}
			path += m_Prefix + _MPT("'");
			break;
		case FZ_SERVERTYPE_SUB_FTP_VMS:
			path = m_Prefix + _MPT("[");
			for (iter = m_Segments.begin(); iter != m_Segments.end(); iter++)
				path += *iter + _T(".");
			path.TrimRight( _T(".") );
			path += _MPT("]");
			break;
		default:
			if (!(m_nServerType & FZ_SERVERTYPE_SUB_FTP_WINDOWS))
				path=_MPT("/");
			for (iter=m_Segments.begin(); iter!=m_Segments.end(); iter++)
				path+=*iter + _T("/");
			break;
		}
		break;
	case FZ_SERVERTYPE_LOCAL:
		path=m_Prefix;
		if (!m_Segments.empty())
			path+=_MPT("\\");
		for (iter=m_Segments.begin(); iter!=m_Segments.end(); iter++)
			path+=*iter + _T("\\");

		break;
	default:
		ASSERT(FALSE);
	}
	return path;
}

CServerPath& CServerPath::operator=(const CServerPath &op)
{
	if (this == &op)
		return *this;
	m_Segments.clear();
	
	m_nServerType = op.m_nServerType;
	
	m_Prefix = op.m_Prefix;
	m_bEmpty = op.m_bEmpty;

	m_Segments = op.m_Segments;

	return *this;
}

const bool CServerPath::operator==(const CServerPath &op) const
{
	if (this == &op)
		return true;

	if (m_bEmpty != op.m_bEmpty)
		return false;
	if (m_Prefix != op.m_Prefix)
		return false;
	// excluding FZ_SERVERTYPE_LAYERMASK from comparison,
	// as this part of server type is not set in TFileZillaIntf
	const int CompareMask = FZ_SERVERTYPE_HIGHMASK | FZ_SERVERTYPE_SUBMASK;
	if ((m_nServerType & CompareMask) != (op.m_nServerType & CompareMask))
		return false;
	tConstIter iter1 = m_Segments.begin();
	tConstIter iter2 = op.m_Segments.begin();
	while (iter1 != m_Segments.end())
	{
		if (iter2 == op.m_Segments.end())
			return false;
		if (*iter1 != *iter2)
			return false;
		iter1++;
		iter2++;
	}
	if (iter2 != op.m_Segments.end())
		return false;
	return true;
}

const BOOL operator==(const CServerPath &a, const CString &b)
{
	CServerPath path(b);
	return a==path;
}

const bool CServerPath::operator!=(const CServerPath &op) const
{
	if (!this)
		return false;

	if (*this == op)
		return false;
	else
		return true;
}

CString CServerPath::GetLastSegment() const
{
	if (!HasParent())
		return _T("");
	if (m_Segments.empty())
		return _T("");
	else
		return m_Segments.back();
}

CServerPath CServerPath::GetParent() const
{
	ASSERT(HasParent());
	CServerPath path;
	path = *this;
	path.m_Segments.pop_back();
	if (m_nServerType & (FZ_SERVERTYPE_SUB_FTP_MVS | FZ_SERVERTYPE_SUB_FTP_BS2000))
		path.m_Prefix = _T(".");
	return path;
}

BOOL CServerPath::HasParent() const
{
	if (!m_Segments.empty())
		return TRUE;
	else
		return FALSE;
}

const BOOL CServerPath::IsEmpty() const
{
	return m_bEmpty;
}

CString CServerPath::GetSafePath() const
{
	if (m_bEmpty)
		return _T("");

	CString safepath;
	safepath.Format(_T("%d %d "), m_nServerType, m_Prefix.GetLength());
	if (m_Prefix!=_MPT(""))
		safepath+=m_Prefix+_MPT(" ");
	tConstIter iter = m_Segments.begin();
	while(iter!=m_Segments.end())
	{
		CString len;
		len.Format(_T("%d "), iter->GetLength());
		safepath+=len;
		safepath+=*iter;
		iter++;
		if (iter!=m_Segments.end())
			safepath+=_MPT(" ");
	}
	return safepath;
}

BOOL CServerPath::AddSubdir(CString subdir)
{
	subdir.TrimLeft( _T(" ") );
	subdir.TrimRight( _T(" ") );
	if (subdir == _MPT(""))
		return FALSE;

	if (m_nServerType & (FZ_SERVERTYPE_SUB_FTP_MVS | FZ_SERVERTYPE_SUB_FTP_BS2000) && m_Prefix != _T("."))
		return FALSE;

	m_Segments.push_back(subdir);

	if (m_nServerType & (FZ_SERVERTYPE_SUB_FTP_MVS | FZ_SERVERTYPE_SUB_FTP_BS2000) && !m_Segments.empty())
	{
		if (m_Segments.back().Right(1) == _T("."))
		{
			m_Segments.back().TrimRight(_MPT('.'));
			m_Prefix = _T(".");
		}
		else
			m_Prefix = _T("");
	}

	m_bEmpty = FALSE;
	
	return TRUE;
}

CServerPath::CServerPath(CString subdir, const CServerPath &parent)
{
	*this=parent;
	subdir.TrimLeft( _T(" ") );
	subdir.TrimRight( _T(" ") );

	if ( subdir==_T("") )
	{
		if (IsEmpty())
			ASSERT(FALSE);
		else
			return;
	}

	if (!(m_nServerType&FZ_SERVERTYPE_HIGHMASK))
		m_nServerType=FZ_SERVERTYPE_FTP;
	
	m_bEmpty = FALSE;

	switch (m_nServerType&FZ_SERVERTYPE_HIGHMASK)
	{
	case FZ_SERVERTYPE_FTP:
		switch(m_nServerType&FZ_SERVERTYPE_SUBMASK)
		{
		case FZ_SERVERTYPE_SUB_FTP_MVS:
		case FZ_SERVERTYPE_SUB_FTP_BS2000:
			{
				subdir.TrimLeft(FTP_MVS_DOUBLE_QUOTA);
				subdir.TrimRight(FTP_MVS_DOUBLE_QUOTA);

				if (subdir.Left(1) == _MPT("'"))
				{
					if (subdir.Right(1) == _MPT("'"))
					{
					   	if (!SetPath(subdir))
							m_bEmpty = true;
					}
					else
						m_bEmpty = true;
				}
				else if (subdir.Right(1) == _MPT("'"))
					m_bEmpty = true;
				else if (!m_bEmpty)
				{
					if (m_Prefix != _T("."))
						m_bEmpty  = true;
					else
					{
						subdir.TrimLeft(_MPT('.'));
						while (subdir.Replace(_T(".."), _T(".")));

						int pos = subdir.Find(_MPT('.'));
						while (pos != -1)
						{
							m_Segments.push_back(subdir.Left(pos));
							subdir = subdir.Mid(pos + 1);
							pos = subdir.Find(_MPT('.'));
						}
						if (subdir != _T(""))
						{
							m_Prefix = _T("");
							m_Segments.push_back(subdir);
						}
					}
				}
				else if (!SetPath(subdir))
					m_bEmpty = true;
			}
			break;
		case FZ_SERVERTYPE_SUB_FTP_VMS:
			{
				int pos1=subdir.Find( _T("[") );
				if (pos1==-1)
				{
					ASSERT( subdir.Right(1)!=_T("]") );
					while ( subdir.Replace( _T(".."), _T(".") ) );
				}
				else
				{
					if (subdir.Right(1)!=_MPT("]"))
						ASSERT(FALSE);
					subdir=subdir.Left(subdir.GetLength()-1);
					if (pos1)
						m_Prefix=subdir.Left(pos1);
					else
						m_Prefix=_MPT("");
					m_Segments.clear();
					subdir=subdir.Mid(pos1+1);

					pos1=subdir.Find( _T("[") );
					int pos2=subdir.Find( _T("]") );
					if (pos1!=-1 || pos2!=-1)
						ASSERT(FALSE);
				}
				int pos=subdir.Find( _T(".") );
				while(pos!=-1)
				{
					m_Segments.push_back(subdir.Left(pos));
					subdir=subdir.Mid(pos+1);
					pos=subdir.Find( _T(".") );
				}
				if (subdir!=_MPT(""))
					m_Segments.push_back(subdir);
			}
			break;
		case FZ_SERVERTYPE_SUB_FTP_WINDOWS:
			{
				subdir.Replace( _T("\\"), _T("/") );
				while(subdir.Replace( _T("//"), _T("/") ));
				if (subdir.GetLength() >= 2 && subdir[1] == _MPT(':'))
					m_Segments.clear();
				else if (subdir[0]==_MPT('/'))
				{
					CString firstSegment;
					if (m_Segments.empty())
						firstSegment = _MPT("C:");
					else
						firstSegment = m_Segments.front();
					m_Segments.clear();
					m_Segments.push_back(firstSegment);
					subdir.TrimLeft( _T("/") );
				}
				subdir.TrimRight( _T("/") );
				int pos = subdir.Find( _T("/") );
				while(pos!=-1)
				{
					m_Segments.push_back(subdir.Left(pos));
					subdir=subdir.Mid(pos+1);
					pos=subdir.Find( _T("/") );
				}
				if (subdir!=_MPT(""))
					m_Segments.push_back(subdir);
				break;
			}
		default:
			subdir.Replace( _T("\\"), _T("/") );
			while(subdir.Replace( _T("//"), _T("/") ));
			if (subdir[0]==_MPT('/'))
			{
				m_Segments.clear();
				subdir.TrimLeft( _T("/") );
			}
			subdir.TrimRight( _T("/") );
			int pos=subdir.Find( _T("/") );
			while(pos!=-1)
			{
				m_Segments.push_back(subdir.Left(pos));
				subdir=subdir.Mid(pos+1);
				pos=subdir.Find( _T("/") );
			}
			if (subdir!=_MPT(""))
				m_Segments.push_back(subdir);
			break;
		}
		break;
	case FZ_SERVERTYPE_LOCAL:
		{
			subdir.TrimRight( _T("\\") );
			while (subdir.Replace( _T("\\\\"), _T("\\") ));
			subdir.TrimLeft( _T("\\") );
			int pos=subdir.Find( _T(":") );

			if (pos==1) //subdir is absolute path
			{
				m_Segments.clear();
				m_Prefix=subdir.Left(pos+1);
				subdir=subdir.Mid(pos+1);
				subdir.TrimLeft( _T("\\") );
				if (subdir.Find( _T(":") )!=-1)
					ASSERT(FALSE);
			}
			if (pos==-1 || pos==1)
			{
				pos=subdir.Find( _T("\\") );
				while (pos!=-1)
				{
					m_Segments.push_back(subdir.Left(pos));
					subdir=subdir.Mid(pos+1);
					pos=subdir.Find( _T("\\") );
				}
				if ( subdir!=_T("") )
					m_Segments.push_back(subdir);			
			}
			else
				ASSERT(FALSE);
		}
		break;
	default:
		ASSERT(FALSE);
	}	
}

BOOL CServerPath::SetPath(CString newpath)
{
	return SetPath(newpath, FALSE);
}

CString CServerPath::FormatFilename(CString fn, bool omitPath /*=false*/) const
{
	if (m_bEmpty)
		return fn;

	if (fn == _MPT(""))
		return _MPT("");

	CString path;
	tConstIter iter;
	switch (m_nServerType&FZ_SERVERTYPE_HIGHMASK)
	{
	case FZ_SERVERTYPE_FTP:
		switch (m_nServerType&FZ_SERVERTYPE_SUBMASK)
		{
		case FZ_SERVERTYPE_SUB_FTP_MVS:
		case FZ_SERVERTYPE_SUB_FTP_BS2000:
			if (omitPath && m_Prefix == _T("."))
				return fn;

			path = _MPT("'");
			for (iter = m_Segments.begin(); iter != m_Segments.end(); iter++)
				path += *iter + _T(".");
			if (m_Prefix != _T("."))
			{
				path.TrimRight(_MPT('.'));
				path += _T("(") + fn + _T(")");
			}
			else
				path += fn;
			path += _MPT("'");
			break;
		case FZ_SERVERTYPE_SUB_FTP_VMS:
			if (omitPath)
				return fn;

			path = m_Prefix + _MPT("[");
			for (iter = m_Segments.begin(); iter != m_Segments.end(); iter++)
				path += *iter + _T(".");
			path.TrimRight( _T(".") );
			path += _MPT("]");
			path += fn;
			break;
		default:
			if (omitPath)
				return fn;
			if (!(m_nServerType & FZ_SERVERTYPE_SUB_FTP_WINDOWS))
				path=_MPT("/");
			for (iter = m_Segments.begin(); iter != m_Segments.end(); iter++)
				path+=*iter + _T("/");
			path += fn;
			break;
		}
		break;
	case FZ_SERVERTYPE_LOCAL:
		if (omitPath)
			return fn;
		path=m_Prefix;
		if (!m_Segments.empty())
			path+=_MPT("\\");
		for (iter=m_Segments.begin(); iter!=m_Segments.end(); iter++)
			path+=*iter + _T("\\");
		path += fn;
		break;
	default:
		ASSERT(FALSE);
	}
	return path;
}