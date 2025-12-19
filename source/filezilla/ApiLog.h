//---------------------------------------------------------------------------
#ifndef ApiLogH
#define ApiLogH
//---------------------------------------------------------------------------
#include "FileZillaIntern.h"
//---------------------------------------------------------------------------
class CApiLog
{
public:
  CApiLog();
  virtual ~CApiLog();

  void InitIntern(TFileZillaIntern * Intern);
  TFileZillaIntern * GetIntern();

  bool LoggingMessageType(int nMessageType) const;

  void LogMessage(int nMessageType, const wchar_t * pMsgFormat, ...) const;
  void LogMessageRaw(int nMessageType, const wchar_t * pMsg) const;
  void LogError(int Error);

  CString GetOption(int OptionID) const;
  int GetOptionVal(int OptionID) const;

protected:
  void SendLogMessage(int nMessageType, const wchar_t * pMsg) const;

  TFileZillaIntern * FIntern;
};
//---------------------------------------------------------------------------
//!CLEANEND
#endif // ApiLogH
