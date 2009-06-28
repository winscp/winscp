//---------------------------------------------------------------------------
#ifndef ExceptionsH
#define ExceptionsH

#include <Classes.hpp>
#include <SysUtils.hpp>
#include <SysInit.hpp>
#include <System.hpp>
//---------------------------------------------------------------------------
bool __fastcall ExceptionMessage(Exception * E, AnsiString & Message);
enum TOnceDoneOperation { odoIdle, odoDisconnect, odoShutDown };
//---------------------------------------------------------------------------
class ExtException : public Sysutils::Exception
{
public:
  __fastcall ExtException(Exception* E);
  __fastcall ExtException(Exception* E, AnsiString Msg);
  // "copy the exception", just append message to the end
  __fastcall ExtException(AnsiString Msg, Exception* E);
  __fastcall ExtException(AnsiString Msg, AnsiString MoreMessages, AnsiString HelpKeyword = "");
  __fastcall ExtException(AnsiString Msg, TStrings* MoreMessages, bool Own);
  __fastcall virtual ~ExtException(void);
  __property TStrings* MoreMessages = {read=FMoreMessages};
  __property AnsiString HelpKeyword = {read=FHelpKeyword};

  inline __fastcall ExtException(const AnsiString Msg, const TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
  inline __fastcall ExtException(int Ident, const TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
  inline __fastcall ExtException(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
  inline __fastcall ExtException(const AnsiString Msg, const TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
  inline __fastcall ExtException(int Ident, int AHelpContext)/* overload */ : Exception(Ident, AHelpContext) { }
  inline __fastcall ExtException(PResStringRec ResStringRec, const TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }

protected:
  void __fastcall AddMoreMessages(Exception* E);

private:
  Classes::TStrings* FMoreMessages;
  AnsiString FHelpKeyword;
};
//---------------------------------------------------------------------------
#define DERIVE_EXT_EXCEPTION(NAME, BASE) \
  class NAME : public BASE \
  { \
  public: \
    inline __fastcall NAME(Exception* E, AnsiString Msg) : BASE(E, Msg) { } \
    inline __fastcall NAME(Exception* E, int Ident) : BASE(E, Ident) { } \
    inline __fastcall virtual ~NAME(void) { } \
    inline __fastcall NAME(const AnsiString Msg, const TVarRec * Args, const int Args_Size) : BASE(Msg, Args, Args_Size) { } \
    inline __fastcall NAME(int Ident, const TVarRec * Args, const int Args_Size) : BASE(Ident, Args, Args_Size) { } \
    inline __fastcall NAME(const AnsiString Msg, int AHelpContext) : BASE(Msg, AHelpContext) { } \
    inline __fastcall NAME(const AnsiString Msg, const TVarRec * Args, const int Args_Size, int AHelpContext) : BASE(Msg, Args, Args_Size, AHelpContext) { } \
    inline __fastcall NAME(int Ident, int AHelpContext) : BASE(Ident, AHelpContext) { } \
    inline __fastcall NAME(PResStringRec ResStringRec, const TVarRec * Args, const int Args_Size, int AHelpContext) : BASE(ResStringRec, Args, Args_Size, AHelpContext) { } \
  };
//---------------------------------------------------------------------------
DERIVE_EXT_EXCEPTION(ESsh, ExtException);
DERIVE_EXT_EXCEPTION(ETerminal, ExtException);
DERIVE_EXT_EXCEPTION(ECommand, ExtException);
DERIVE_EXT_EXCEPTION(EScp, ExtException); // SCP protocol fatal error (non-fatal in application context)
DERIVE_EXT_EXCEPTION(EScpSkipFile, ExtException);
DERIVE_EXT_EXCEPTION(EScpFileSkipped, EScpSkipFile);
//---------------------------------------------------------------------------
class EOSExtException : public ExtException
{
public:
  __fastcall EOSExtException();
  __fastcall EOSExtException(AnsiString Msg);
};
//---------------------------------------------------------------------------
class EFatal : public ExtException
{
public:
  // fatal errors are always copied, new message is only appended
  inline __fastcall EFatal(Exception* E, AnsiString Msg) : ExtException(Msg, E) { }
};
//---------------------------------------------------------------------------
#define DERIVE_FATAL_EXCEPTION(NAME, BASE) \
  class NAME : public BASE \
  { \
  public: \
    inline __fastcall NAME(Exception* E, AnsiString Msg) : BASE(E, Msg) { } \
  };
//---------------------------------------------------------------------------
DERIVE_FATAL_EXCEPTION(ESshFatal, EFatal);
//---------------------------------------------------------------------------
// exception that closes application, but displayes info message (not error message)
// = close on completionclass ESshTerminate : public EFatal
class ESshTerminate : public EFatal
{
public:
  inline __fastcall ESshTerminate(Exception* E, AnsiString Msg, TOnceDoneOperation AOperation) :
    EFatal(E, Msg),
    Operation(AOperation)
  { }

  TOnceDoneOperation Operation;
};
//---------------------------------------------------------------------------
AnsiString __fastcall LastSysErrorMessage();
//---------------------------------------------------------------------------
#endif  // Exceptions
