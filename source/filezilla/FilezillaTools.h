//---------------------------------------------------------------------------
#ifndef FileZillaToolsH
#define FileZillaToolsH
//---------------------------------------------------------------------------
#include <ctime>
//---------------------------------------------------------------------------
class CFileZillaTools
{
public:
  virtual void PreserveDownloadFileTime(HANDLE Handle, void * UserData) = 0;
  virtual bool GetFileModificationTimeInUtc(const wchar_t * FileName, struct tm & Time) = 0;
};
//---------------------------------------------------------------------------
#endif // FileZillaToolsH