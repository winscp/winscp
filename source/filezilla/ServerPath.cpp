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
              file=path.Mid(rpos+1);
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
  }
  if (bIsFile)
    newpath = file;
  return TRUE;
}

const CString CServerPath::DoGetPath(bool unterminated) const
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
      if (unterminated)
      {
        if (path.GetLength() >= 2)
        {
          path.Delete(path.GetLength() - 1, 1);
        }
      }
      break;
    }
    break;
  default:
    DebugFail();
  }
  return path;
}

const CString CServerPath::GetPath() const
{
  return DoGetPath(false);
}

const CString CServerPath::GetPathUnterminated() const
{
  return DoGetPath(true);
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

const bool CServerPath::operator!=(const CServerPath &op) const
{
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

BOOL CServerPath::AddSubdir(CString subdir)
{
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
  default:
    DebugFail();
  }
  return path;
}
