//---------------------------------------------------------------------------
#include "stdafx.h"

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
  DebugCheck(FindClose(hFind));

  size=((_int64)findFileData.nFileSizeHigh<<32)+findFileData.nFileSizeLow;

  return TRUE;
}

BOOL PASCAL GetStatus64(LPCTSTR lpszFileName, CFileStatus64& rStatus)
{
  WIN32_FIND_DATA findFileData;
  HANDLE hFind = FindFirstFile((LPTSTR)lpszFileName, &findFileData);
  if (hFind == INVALID_HANDLE_VALUE)
  {
    return FALSE;
  }
  DebugCheck(FindClose(hFind));

  // strip attribute of NORMAL bit, our API doesn't have a "normal" bit.
  rStatus.m_attribute = (BYTE)
    (findFileData.dwFileAttributes & ~FILE_ATTRIBUTE_NORMAL);

  rStatus.m_size = ((_int64)findFileData.nFileSizeHigh<<32)+findFileData.nFileSizeLow;

  // convert times as appropriate
  try
  {
    rStatus.m_ctime = CTime(findFileData.ftCreationTime);
    rStatus.m_has_ctime = true;
  }
  catch (CException*)
  {
    rStatus.m_has_ctime = false;
  }

  try
  {
    rStatus.m_atime = CTime(findFileData.ftLastAccessTime);
    rStatus.m_has_atime = true;
  }
  catch (CException*)
  {
    rStatus.m_has_atime = false;
  }

  try
  {
    rStatus.m_mtime = CTime(findFileData.ftLastWriteTime);
    rStatus.m_has_mtime = true;
  }
  catch (CException*)
  {
    rStatus.m_has_mtime = false;
  }

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
