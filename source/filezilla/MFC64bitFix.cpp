//---------------------------------------------------------------------------
#include "FileZillaPCH.h"

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

BOOL PASCAL GetFileStatus(LPCTSTR lpszFileName, CFileStatus& rStatus)
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

  // convert times as appropriate
  rStatus.m_mtime = CTime(findFileData.ftLastWriteTime);

  if (rStatus.m_mtime.GetTime() == 0)
  {
    rStatus.m_mtime = CTime(findFileData.ftCreationTime);
  }

  return TRUE;
}
