//---------------------------------------------------------------------------
#ifndef GlobalH
#define GlobalH
//---------------------------------------------------------------------------
#define FORMAT(S, F) Format(S, ARRAYOFCONST(F))
#define FMTLOAD(I, F) FmtLoadStr(I, ARRAYOFCONST(F))
#define LENOF(x) ( (sizeof((x))) / (sizeof(*(x))))
#define FLAGSET(SET, FLAG) (((SET) & (FLAG)) == (FLAG))
#define FLAGCLEAR(SET, FLAG) (((SET) & (FLAG)) == 0)
#define FLAGMASK(ENABLE, FLAG) ((ENABLE) ? (FLAG) : 0)
//---------------------------------------------------------------------------
#include <System.SyncObjs.hpp>
//---------------------------------------------------------------------------
extern const UnicodeString EmptyString;
//---------------------------------------------------------------------------
UnicodeString NormalizeString(const UnicodeString & S);
//---------------------------------------------------------------------------
class TGuard
{
public:
  __fastcall TGuard(TCriticalSection * ACriticalSection);
  __fastcall ~TGuard();

private:
  TCriticalSection * FCriticalSection;
};
//---------------------------------------------------------------------------
class TUnguard
{
public:
  __fastcall TUnguard(TCriticalSection * ACriticalSection);
  __fastcall ~TUnguard();

private:
  TCriticalSection * FCriticalSection;
};
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#include <assert.h>
#define ACCESS_VIOLATION_TEST { (*((int*)NULL)) = 0; }
#if !defined(_DEBUG) || defined(DESIGN_ONLY)
#define DebugAssert(p)   ((void)0)
#define DebugCheck(p) (p)
#define DebugFail()
#else // if !defined(_DEBUG) || defined(DESIGN_ONLY)
void __fastcall DoAssert(wchar_t * Message, wchar_t * Filename, int LineNumber);
#define DebugAssert(p) ((p) ? (void)0 : DoAssert(TEXT(#p), TEXT(__FILE__), __LINE__))
#define DebugCheck(p) { bool __CHECK_RESULT__ = (p); DebugAssert(__CHECK_RESULT__); }
#define DebugFail() DebugAssert(false)
#endif // if !defined(_DEBUG) || defined(DESIGN_ONLY)
//---------------------------------------------------------------------------
#define DebugAlwaysTrue(p) (p)
#define DebugAlwaysFalse(p) (p)
#define DebugNotNull(p) (p)
#define TraceInitPtr(p) (p)
#define TraceInitStr(p) (p)
#define DebugUsedParam(p) ((&p) == (&p))
#define DebugUsedArg(p)
//---------------------------------------------------------------------------
#endif
