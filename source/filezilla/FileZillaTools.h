//---------------------------------------------------------------------------
#ifndef FileZillaToolsH
#define FileZillaToolsH
//---------------------------------------------------------------------------
#include <ctime>
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wreserved-id-macro"
#pragma clang diagnostic ignored "-Wold-style-cast"
#include <openssl/ssl.h>
#pragma clang diagnostic pop
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
