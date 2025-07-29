//---------------------------------------------------------------------------
#ifndef MFC64bitFixH
#define MFC64bitFixH
//---------------------------------------------------------------------------
__int64 GetLength64(CFile & file);
BOOL GetLength64(CString filename, _int64 & size);
//---------------------------------------------------------------------------
struct CFileStatus
{
  CTime m_mtime;    // last modification date/time of file
  BYTE m_attribute; // logical OR of CFile::Attribute enum values
};
//---------------------------------------------------------------------------
BOOL PASCAL GetFileStatus(LPCTSTR lpszFileName, CFileStatus & rStatus);
//---------------------------------------------------------------------------
#endif // MFC64bitFixH
