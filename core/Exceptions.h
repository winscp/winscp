#ifndef ExceptionsH
#define ExceptionsH

#include <Classes.hpp>
#include <SysUtils.hpp>
#include <SysInit.hpp>
#include <System.hpp>

class ExtException : public Sysutils::Exception
{
public:
	__fastcall ExtException(Exception* E, AnsiString Msg);
	__fastcall ExtException(Exception* E, int Ident);
	__fastcall virtual ~ExtException(void);
	__property TStrings* MoreMessages = {read=FMoreMessages};

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
};

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

DERIVE_EXT_EXCEPTION(ESsh, ExtException);
DERIVE_EXT_EXCEPTION(EFatal, ExtException);
DERIVE_EXT_EXCEPTION(ESshFatal, EFatal);
DERIVE_EXT_EXCEPTION(ESshTerminate, EFatal); // exception that closes application, but displayes info message (not error message) = close on completion
DERIVE_EXT_EXCEPTION(ETerminal, ExtException);
DERIVE_EXT_EXCEPTION(ECommand, ExtException);
DERIVE_EXT_EXCEPTION(EScp, ExtException); // SCP protocol fatal error (non-fatal in application context)
DERIVE_EXT_EXCEPTION(EScpSkipFile, ExtException);
DERIVE_EXT_EXCEPTION(EScpFileSkipped, EScpSkipFile);

#endif	// Exceptions


