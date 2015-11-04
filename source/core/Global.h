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
#ifndef _DEBUG
#undef DebugAssert
#define DebugAssert(p)   ((void)0)
#define CHECK(p) p
#define FAIL
#define ALWAYS_TRUE(p) p
#define ALWAYS_FALSE(p) p
#define NOT_NULL(P) P
#else
#define CHECK(p) { bool __CHECK_RESULT__ = (p); DebugAssert(__CHECK_RESULT__); }
#define FAIL DebugAssert(false)
#define ALWAYS_TRUE(p) (p)
#define ALWAYS_FALSE(p) (p)
#define NOT_NULL(P) P
#endif
#define USEDPARAM(p) ((&p) == (&p))
//---------------------------------------------------------------------------
#endif
