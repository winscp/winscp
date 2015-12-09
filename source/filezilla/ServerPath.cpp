//---------------------------------------------------------------------------
#include "stdafx.h"
#include "ServerPath.h"
#include "structures.h"

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

CServerPath::CServerPath(CString path, bool trim)
{
  m_nServerType = FZ_SERVERTYPE_FTP;
  if (trim)
  {
    path.TrimLeft( L" " );
    path.TrimRight( L" " );
  }
  if (path == L"")
  {
    m_bEmpty = TRUE;
    return;
  }
  else
    m_bEmpty = FALSE;

  int pos1 = path.Find( L":[" );
  if (pos1 != -1 && path.Right(1) == L"]" && pos1 != (path.GetLength()-1))
    m_nServerType |= FZ_SERVERTYPE_SUB_FTP_VMS;
  else if (path.GetLength() >= 3 && _istalpha(path[0]) && path[1] == L':' && (path[2] == L'\\' || path[2] == L'/'))
    m_nServerType |= FZ_SERVERTYPE_SUB_FTP_WINDOWS;
  else if (path[0] == FTP_MVS_DOUBLE_QUOTA && path[path.GetLength() - 1] == FTP_MVS_DOUBLE_QUOTA)
    m_nServerType |= FZ_SERVERTYPE_SUB_FTP_MVS;
  else if (path.GetLength() > 2 && path[0] == L'\'' && path.Right(1) == L"'" && path.Find(L'/') == -1 && path.Find(L'\\') == -1)
    m_nServerType |= FZ_SERVERTYPE_SUB_FTP_MVS;

  *this = CServerPath(path, m_nServerType, trim);
}

CServerPath::CServerPath(CString path, int nServerType, bool trim)
{
  m_nServerType = nServerType;
  if (trim)
  {
    path.TrimLeft( L" " );
    path.TrimRight( L" " );
  }
  if (path == L"")
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
        path.TrimLeft(L'\'');
        path.TrimRight(L'\'');
        path.TrimLeft(L'.');
        while (path.Replace(L"..", L"."));

        int pos = path.Find(L".");
        while (pos != -1)
        {
          m_Segments.push_back(path.Left(pos));
          path = path.Mid(pos + 1);
          pos = path.Find( L"." );
        }
        if (path != L"")
          m_Segments.push_back(path);
        else
          m_Prefix = L".";
      }
      break;
    case FZ_SERVERTYPE_SUB_FTP_VMS:
      {
        int pos1 = path.Find( L"[" );
        if (pos1 == -1 || path.Right(1) != L"]")
        {
          DebugFail();
          m_bEmpty = TRUE;
          return;
        }
        path.TrimRight( L"]" );
        if (pos1)
          m_Prefix = path.Left(pos1);
        path = path.Mid(pos1 + 1);
        int pos = path.Find( L"." );
        while (pos != -1)
        {
          m_Segments.push_back(path.Left(pos));
          path = path.Mid(pos+1);
          pos = path.Find( L"." );
        }
        if (path != L"")
          m_Segments.push_back(path);
      }
      break;
    default:
      path.Replace( L"\\", L"/" );
      while (path.Replace( L"//", L"/" ));
      path.TrimLeft( L"/" );
      path.TrimRight( L"/" );
      int pos = path.Find( L"/" );
      while (pos != -1)
      {
        m_Segments.push_back(path.Left(pos));
        path = path.Mid(pos+1);
        pos = path.Find( L"/" );
      }
      if (path != L"")
        m_Segments.push_back(path);
      break;
    }
    break;
  case FZ_SERVERTYPE_LOCAL:
    {
      path.TrimRight( L"\\" );
      while (path.Replace( L"\\\\", L"\\" ));
      int pos = path.Find( L"\\" );
      if (pos == -1)
      {
        m_Prefix = path;
        return;
      }
      DebugAssert(pos == 2);
      m_Prefix = path.Left(pos);
      path = path.Mid(pos + 1);
      pos = path.Find( L"\\" );
      while (pos != -1)
      {
        m_Segments.push_back(path.Left(pos));
        path=path.Mid(pos + 1);
        pos=path.Find( L"\\" );
      }
      if (path != L"")
        m_Segments.push_back(path);
    }
    break;
  default:
    DebugFail();
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

  path.TrimLeft( L" " );
  path.TrimRight( L" " );
  if (path != L"")
    m_bEmpty = FALSE;
  else
    m_bEmpty = TRUE;
  if (!(m_nServerType & FZ_SERVERTYPE_HIGHMASK))
    m_nServerType = FZ_SERVERTYPE_FTP;
  if (!(m_nServerType&FZ_SERVERTYPE_SUBMASK) && (m_nServerType&FZ_SERVERTYPE_HIGHMASK)==FZ_SERVERTYPE_FTP)
  {
    int pos1 = path.Find( L":[" );
    if (pos1!=-1 && pos1!=(path.GetLength()-2))
    {
      if (!bIsFile && path.Right(1)==L"]")
        m_nServerType|=FZ_SERVERTYPE_SUB_FTP_VMS;
      else if (bIsFile && path.ReverseFind(']')>(pos1+1))
        m_nServerType|=FZ_SERVERTYPE_SUB_FTP_VMS;
    }
    if (newpath.GetLength() >= 3 && _istalpha(newpath[0]) && newpath[1] == L':' && (newpath[2] == L'\\' || newpath[2] == L'/'))
      m_nServerType |= FZ_SERVERTYPE_SUB_FTP_WINDOWS;
    else if (path[0] == FTP_MVS_DOUBLE_QUOTA && path[path.GetLength() - 1] == FTP_MVS_DOUBLE_QUOTA)
      m_nServerType |= FZ_SERVERTYPE_SUB_FTP_MVS;
  }
  m_Segments.clear();
  m_Prefix = L"";
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
          path.TrimLeft(L'\'');
          path.TrimRight(L'\'');
          path.TrimLeft(L'.');
          while (path.Replace(L"..", L"."));

          int pos = path.Find(L".");
          while (pos != -1)
          {
            m_Segments.push_back(path.Left(pos));
            path = path.Mid(pos + 1);
            pos = path.Find( L"." );
          }
          if (path != L"")
            m_Segments.push_back(path);
          else
            m_Prefix = L".";

          if (bIsFile)
          {
            if (m_Segments.empty())
              return FALSE;
            file = m_Segments.back();
            m_Segments.pop_back();

            if (file.Right(1) == L".")
              return FALSE;

            int pos = file.Find(L'(');
            int pos2 = file.Find(L')');
            if (pos != -1)
            {
              if (!pos || pos2 != file.GetLength() - 2)
                return FALSE;
              m_Prefix = L"";
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
          int pos1=path.Find( L"[" );
          if (pos1==-1)
            return FALSE;
          if (bIsFile)
          {
            int rpos=path.ReverseFind(L']');
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
          if (path.Right(1)!=L"]")
            return FALSE;
          path.TrimRight( L"]" );
          if (pos1)
            m_Prefix=path.Left(pos1);
          path=path.Mid(pos1+1);
          int pos=path.Find( L"." );
          while(pos!=-1)
          {
            m_Segments.push_back(path.Left(pos));
            path=path.Mid(pos+1);
            pos=path.Find( L"." );
          }
          if (path!=L"")
            m_Segments.push_back(path);
        }
        break;
      default:
        path.Replace( L"\\", L"/" );
        while(path.Replace( L"//", L"/" ));
        path.TrimLeft( L"/" );
        if (bIsFile)
        {
          if (path.Right(1)!= L"/" )
          {
            int rpos=path.ReverseFind(L'/');
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
        path.TrimRight( L"/" );
        int pos=path.Find( L"/" );
        while(pos!=-1)
        {
          m_Segments.push_back(path.Left(pos));
          path=path.Mid(pos+1);
          pos=path.Find( L"/" );
        }
        if (path!=L"")
          m_Segments.push_back(path);
        break;
      }
      break;
    case FZ_SERVERTYPE_LOCAL:
    {
      if (bIsFile)
      {
        if (path.Right(1)!= L"\\" )
        {
          int rpos=path.ReverseFind(L'\\');
          if (rpos==-1)
            return FALSE;

          file=path.Mid(rpos+1);
          path=path.Left(rpos);
        }
        else
          return FALSE;
      }
      path.TrimRight( L"\\" );
      while (path.Replace( L"\\\\", L"\\" ));
      int pos=path.Find( L":\\" );
      if (pos==-1 || pos!=1)
        return FALSE;
      else
      {
        m_Prefix=path.Left(pos+1);
        path=path.Mid(pos+2);
      }
      pos=path.Find( L"\\" );
      while (pos!=-1)
      {
        m_Segments.push_back(path.Left(pos));
        path=path.Mid(pos+1);
        pos=path.Find( L"\\" );
      }
      if (path!=L"")
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
    return L"";
  CString path;
  tConstIter iter;
  switch (m_nServerType&FZ_SERVERTYPE_HIGHMASK)
  {
  case FZ_SERVERTYPE_FTP:
    switch (m_nServerType&FZ_SERVERTYPE_SUBMASK)
    {
    case FZ_SERVERTYPE_SUB_FTP_MVS:
    case FZ_SERVERTYPE_SUB_FTP_BS2000:
      path = L"'";
      for (iter = m_Segments.begin(); iter != m_Segments.end(); iter++)
      {
        if (iter != m_Segments.begin())
          path += L".";
        path += *iter;
      }
      path += m_Prefix + L"'";
      break;
    case FZ_SERVERTYPE_SUB_FTP_VMS:
      path = m_Prefix + L"[";
      for (iter = m_Segments.begin(); iter != m_Segments.end(); iter++)
        path += *iter + L".";
      path.TrimRight( L"." );
      path += L"]";
      break;
    default:
      if (!(m_nServerType & FZ_SERVERTYPE_SUB_FTP_WINDOWS))
        path=L"/";
      for (iter=m_Segments.begin(); iter!=m_Segments.end(); iter++)
        path+=*iter + L"/";
      break;
    }
    break;
  case FZ_SERVERTYPE_LOCAL:
    path=m_Prefix;
    if (!m_Segments.empty())
      path+=L"\\";
    for (iter=m_Segments.begin(); iter!=m_Segments.end(); iter++)
      path+=*iter + L"\\";

    break;
  default:
    DebugFail();
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
    return L"";
  if (m_Segments.empty())
    return L"";
  else
    return m_Segments.back();
}

CServerPath CServerPath::GetParent() const
{
  DebugAssert(HasParent());
  CServerPath path;
  path = *this;
  path.m_Segments.pop_back();
  if (m_nServerType & (FZ_SERVERTYPE_SUB_FTP_MVS | FZ_SERVERTYPE_SUB_FTP_BS2000))
    path.m_Prefix = L".";
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
    return L"";

  CString safepath;
  safepath.Format(L"%d %d ", m_nServerType, m_Prefix.GetLength());
  if (m_Prefix!=L"")
    safepath+=m_Prefix+L" ";
  tConstIter iter = m_Segments.begin();
  while(iter!=m_Segments.end())
  {
    CString len;
    len.Format(L"%d ", iter->GetLength());
    safepath+=len;
    safepath+=*iter;
    iter++;
    if (iter!=m_Segments.end())
      safepath+=L" ";
  }
  return safepath;
}

BOOL CServerPath::AddSubdir(CString subdir)
{
  subdir.TrimLeft( L" " );
  subdir.TrimRight( L" " );
  if (subdir == L"")
    return FALSE;

  if (m_nServerType & (FZ_SERVERTYPE_SUB_FTP_MVS | FZ_SERVERTYPE_SUB_FTP_BS2000) && m_Prefix != L".")
    return FALSE;

  m_Segments.push_back(subdir);

  if (m_nServerType & (FZ_SERVERTYPE_SUB_FTP_MVS | FZ_SERVERTYPE_SUB_FTP_BS2000) && !m_Segments.empty())
  {
    if (m_Segments.back().Right(1) == L".")
    {
      m_Segments.back().TrimRight(L'.');
      m_Prefix = L".";
    }
    else
      m_Prefix = L"";
  }

  m_bEmpty = FALSE;

  return TRUE;
}

CServerPath::CServerPath(CString subdir, const CServerPath &parent)
{
  *this=parent;
  subdir.TrimLeft( L" " );
  subdir.TrimRight( L" " );

  if ( subdir==L"" )
  {
    if (IsEmpty())
      DebugFail();
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

        if (subdir.Left(1) == L"'")
        {
          if (subdir.Right(1) == L"'")
          {
               if (!SetPath(subdir))
              m_bEmpty = true;
          }
          else
            m_bEmpty = true;
        }
        else if (subdir.Right(1) == L"'")
          m_bEmpty = true;
        else if (!m_bEmpty)
        {
          if (m_Prefix != L".")
            m_bEmpty  = true;
          else
          {
            subdir.TrimLeft(L'.');
            while (subdir.Replace(L"..", L"."));

            int pos = subdir.Find(L'.');
            while (pos != -1)
            {
              m_Segments.push_back(subdir.Left(pos));
              subdir = subdir.Mid(pos + 1);
              pos = subdir.Find(L'.');
            }
            if (subdir != L"")
            {
              m_Prefix = L"";
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
        int pos1=subdir.Find( L"[" );
        if (pos1==-1)
        {
          DebugAssert( subdir.Right(1)!=L"]" );
          while ( subdir.Replace( L"..", L"." ) );
        }
        else
        {
          if (subdir.Right(1)!=L"]")
            DebugFail();
          subdir=subdir.Left(subdir.GetLength()-1);
          if (pos1)
            m_Prefix=subdir.Left(pos1);
          else
            m_Prefix=L"";
          m_Segments.clear();
          subdir=subdir.Mid(pos1+1);

          pos1=subdir.Find( L"[" );
          int pos2=subdir.Find( L"]" );
          if (pos1!=-1 || pos2!=-1)
            DebugFail();
        }
        int pos=subdir.Find( L"." );
        while(pos!=-1)
        {
          m_Segments.push_back(subdir.Left(pos));
          subdir=subdir.Mid(pos+1);
          pos=subdir.Find( L"." );
        }
        if (subdir!=L"")
          m_Segments.push_back(subdir);
      }
      break;
    case FZ_SERVERTYPE_SUB_FTP_WINDOWS:
      {
        subdir.Replace( L"\\", L"/" );
        while(subdir.Replace( L"//", L"/" ));
        if (subdir.GetLength() >= 2 && subdir[1] == L':')
          m_Segments.clear();
        else if (subdir[0]==L'/')
        {
          CString firstSegment;
          if (m_Segments.empty())
            firstSegment = L"C:";
          else
            firstSegment = m_Segments.front();
          m_Segments.clear();
          m_Segments.push_back(firstSegment);
          subdir.TrimLeft( L"/" );
        }
        subdir.TrimRight( L"/" );
        int pos = subdir.Find( L"/" );
        while(pos!=-1)
        {
          m_Segments.push_back(subdir.Left(pos));
          subdir=subdir.Mid(pos+1);
          pos=subdir.Find( L"/" );
        }
        if (subdir!=L"")
          m_Segments.push_back(subdir);
        break;
      }
    default:
      subdir.Replace( L"\\", L"/" );
      while(subdir.Replace( L"//", L"/" ));
      if (subdir[0]==L'/')
      {
        m_Segments.clear();
        subdir.TrimLeft( L"/" );
      }
      subdir.TrimRight( L"/" );
      int pos=subdir.Find( L"/" );
      while(pos!=-1)
      {
        m_Segments.push_back(subdir.Left(pos));
        subdir=subdir.Mid(pos+1);
        pos=subdir.Find( L"/" );
      }
      if (subdir!=L"")
        m_Segments.push_back(subdir);
      break;
    }
    break;
  case FZ_SERVERTYPE_LOCAL:
    {
      subdir.TrimRight( L"\\" );
      while (subdir.Replace( L"\\\\", L"\\" ));
      subdir.TrimLeft( L"\\" );
      int pos=subdir.Find( L":" );

      if (pos==1) //subdir is absolute path
      {
        m_Segments.clear();
        m_Prefix=subdir.Left(pos+1);
        subdir=subdir.Mid(pos+1);
        subdir.TrimLeft( L"\\" );
        if (subdir.Find( L":" )!=-1)
          DebugFail();
      }
      if (pos==-1 || pos==1)
      {
        pos=subdir.Find( L"\\" );
        while (pos!=-1)
        {
          m_Segments.push_back(subdir.Left(pos));
          subdir=subdir.Mid(pos+1);
          pos=subdir.Find( L"\\" );
        }
        if ( subdir!=L"" )
          m_Segments.push_back(subdir);
      }
      else
        DebugFail();
    }
    break;
  default:
    DebugFail();
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

  if (fn == L"")
    return L"";

  CString path;
  tConstIter iter;
  switch (m_nServerType&FZ_SERVERTYPE_HIGHMASK)
  {
  case FZ_SERVERTYPE_FTP:
    switch (m_nServerType&FZ_SERVERTYPE_SUBMASK)
    {
    case FZ_SERVERTYPE_SUB_FTP_MVS:
    case FZ_SERVERTYPE_SUB_FTP_BS2000:
      if (omitPath && m_Prefix == L".")
        return fn;

      path = L"'";
      for (iter = m_Segments.begin(); iter != m_Segments.end(); iter++)
        path += *iter + L".";
      if (m_Prefix != L".")
      {
        path.TrimRight(L'.');
        path += L"(" + fn + L")";
      }
      else
        path += fn;
      path += L"'";
      break;
    case FZ_SERVERTYPE_SUB_FTP_VMS:
      if (omitPath)
        return fn;

      path = m_Prefix + L"[";
      for (iter = m_Segments.begin(); iter != m_Segments.end(); iter++)
        path += *iter + L".";
      path.TrimRight( L"." );
      path += L"]";
      path += fn;
      break;
    default:
      if (omitPath)
        return fn;
      if (!(m_nServerType & FZ_SERVERTYPE_SUB_FTP_WINDOWS))
        path=L"/";
      for (iter = m_Segments.begin(); iter != m_Segments.end(); iter++)
        path+=*iter + L"/";
      path += fn;
      break;
    }
    break;
  case FZ_SERVERTYPE_LOCAL:
    if (omitPath)
      return fn;
    path=m_Prefix;
    if (!m_Segments.empty())
      path+=L"\\";
    for (iter=m_Segments.begin(); iter!=m_Segments.end(); iter++)
      path+=*iter + L"\\";
    path += fn;
    break;
  default:
    DebugFail();
  }
  return path;
}
