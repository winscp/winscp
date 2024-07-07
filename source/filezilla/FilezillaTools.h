//---------------------------------------------------------------------------
#ifndef FileZillaToolsH
#define FileZillaToolsH
//---------------------------------------------------------------------------
#include <ctime>
#include <openssl/ssl.h>
//---------------------------------------------------------------------------
class CFileZillaTools
{
public:
  virtual void PreserveDownloadFileTime(HANDLE Handle, void * UserData) = 0;
  virtual bool GetFileModificationTimeInUtc(const wchar_t * FileName, struct tm & Time) = 0;
  virtual wchar_t * LastSysErrorMessage() = 0;
  virtual std::wstring GetClientString() = 0;
  virtual void SetupSsl(ssl_st * Ssl) = 0;
  virtual std::wstring CustomReason(int Err) = 0;
};
//---------------------------------------------------------------------------
#endif // FileZillaToolsH
