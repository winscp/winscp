//---------------------------------------------------------------------------
#ifndef CommonH
#define CommonH

#ifndef C_ONLY
//---------------------------------------------------------------------------
#define EXCEPTION throw ExtException(NULL, "")
#define SCOPY(dest, source) \
  strncpy(dest, source, sizeof(dest)); \
  dest[sizeof(dest)-1] = '\0'
#define SAFE_DESTROY(OBJ) { TObject * PObj = OBJ; OBJ = NULL; delete PObj; }
#define ASCOPY(dest, source) SCOPY(dest, source.c_str())
#define FORMAT(S, F) Format(S, ARRAYOFCONST(F))
#define FMTLOAD(I, F) FmtLoadStr(I, ARRAYOFCONST(F))
#define LENOF(x) ( (sizeof((x))) / (sizeof(*(x))))
#define FLAGSET(SET, FLAG) (((SET) & (FLAG)) == (FLAG))
#define FLAGCLEAR(SET, FLAG) (((SET) & (FLAG)) == 0)
#define FLAGMASK(ENABLE, FLAG) ((ENABLE) ? (FLAG) : 0)
//---------------------------------------------------------------------------
extern const char EngShortMonthNames[12][4];
//---------------------------------------------------------------------------
AnsiString ReplaceChar(AnsiString Str, Char A, Char B);
AnsiString DeleteChar(AnsiString Str, Char C);
void PackStr(AnsiString &Str);
AnsiString GetTemporaryPath();
AnsiString MakeValidFileName(AnsiString FileName);
AnsiString RootKeyToStr(HKEY RootKey);
AnsiString BooleanToStr(bool B);
AnsiString BooleanToEngStr(bool B);
AnsiString CutToChar(AnsiString &Str, Char Ch, bool Trim);
AnsiString ExceptionLogString(Exception *E);
bool IsDots(const AnsiString Str);
AnsiString __fastcall SystemTemporaryDirectory();
AnsiString __fastcall StripPathQuotes(const AnsiString Path);
AnsiString __fastcall AddPathQuotes(AnsiString Path);
void __fastcall SplitCommand(AnsiString Command, AnsiString &Program,
  AnsiString & Params, AnsiString & Dir);
AnsiString __fastcall FormatCommand(AnsiString Program, AnsiString Params);
bool __fastcall IsDisplayableStr(const AnsiString Str);
AnsiString __fastcall StrToHex(const AnsiString Str);
AnsiString __fastcall HexToStr(const AnsiString Hex);
bool __fastcall RecursiveDeleteFile(const AnsiString FileName, bool ToRecycleBin);
int __fastcall CancelAnswer(int Answers);
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TProcessLocalFileEvent)
  (const AnsiString FileName, const TSearchRec Rec, void * Param);
bool __fastcall FileSearchRec(const AnsiString FileName, TSearchRec & Rec);
void __fastcall ProcessLocalDirectory(AnsiString DirName,
  TProcessLocalFileEvent CallBackFunc, void * Param = NULL, int FindAttrs = -1);
//---------------------------------------------------------------------------
#define TIME_POSIX_TO_WIN(t, ft) (*(LONGLONG*)&(ft) = \
    ((LONGLONG) (t) + (LONGLONG) 11644473600) * (LONGLONG) 10000000)
#define TIME_WIN_TO_POSIX(ft, t) ((t) = (unsigned long) \
    ((*(LONGLONG*)&(ft)) / (LONGLONG) 10000000 - (LONGLONG) 11644473600))
TDateTime __fastcall UnixToDateTime(unsigned long TimeStamp, bool ConsiderDST);
FILETIME __fastcall DateTimeToFileTime(const TDateTime DateTime, bool ConsiderDST);
TDateTime __fastcall AdjustDateTimeFromUnix(TDateTime DateTime, bool ConsiderDST);
void __fastcall UnifyDateTimePrecision(TDateTime & DateTime1, TDateTime & DateTime2);
unsigned long __fastcall ConvertTimestampToUnix(const FILETIME & FileTime,
  bool ConsiderDST);
//---------------------------------------------------------------------------
class TCriticalSection
{
public:
  __fastcall TCriticalSection();
  __fastcall ~TCriticalSection();

  void __fastcall Enter();
  void __fastcall Leave();

private:
  TRTLCriticalSection FSection;
};
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
#ifdef _DEBUG
#define TRACEENV "WINSCPTRACE"
void __fastcall Trace(const AnsiString SourceFile, const AnsiString Func,
  int Line, const AnsiString Message);
#define TRACE(MESSAGE) Trace(__FILE__, __FUNC__, __LINE__, MESSAGE)
#define TRACEFMT(MESSAGE, PARAMS) Trace(__FILE__, __FUNC__, __LINE__, FORMAT(MESSAGE, PARAMS))
#else // ifdef _DEBUG
#define TRACE(PARAMS)
#define TRACEFMT(MESSAGE, PARAMS) 
#endif // ifdef _DEBUG
//---------------------------------------------------------------------------
#endif
//---------------------------------------------------------------------------
#include <assert.h>
#ifndef _DEBUG
#undef assert
#define assert(p)   ((void)0)
#endif 
#define USEDPARAM(p) ((p) == (p))
//---------------------------------------------------------------------------
#endif
