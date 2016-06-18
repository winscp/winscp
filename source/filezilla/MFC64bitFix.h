//---------------------------------------------------------------------------
#ifndef MFC64bitFixH
#define MFC64bitFixH
//---------------------------------------------------------------------------
__int64 GetLength64(CFile & file);
BOOL GetLength64(CString filename, _int64 & size);
//---------------------------------------------------------------------------
struct CFileStatus64
{
  bool m_has_ctime;
  bool m_has_mtime;
  bool m_has_atime;
  CTime m_ctime;    // creation date/time of file
  CTime m_mtime;    // last modification date/time of file
  CTime m_atime;    // last access date/time of file
  _int64 m_size;    // logical size of file in bytes
  BYTE m_attribute; // logical OR of CFile::Attribute enum values
  BYTE _m_padding;  // pad the structure to a WORD
};
//---------------------------------------------------------------------------
BOOL PASCAL GetStatus64(LPCTSTR lpszFileName, CFileStatus64 & rStatus);
//---------------------------------------------------------------------------
#endif // MFC64bitFixH
