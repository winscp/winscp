//---------------------------------------------------------------------------
#ifndef CommonH
#define CommonH

#ifndef C_ONLY
//---------------------------------------------------------------------------
#define EXCEPTION throw ExtException(NULL, "")
#define THROWIFFALSE(C) if (!(C)) EXCEPTION
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
#define SWAP(TYPE, FIRST, SECOND) \
  { TYPE __Backup = FIRST; FIRST = SECOND; SECOND = __Backup; }
//---------------------------------------------------------------------------
extern const char EngShortMonthNames[12][4];
//---------------------------------------------------------------------------
AnsiString ReplaceChar(AnsiString Str, Char A, Char B);
AnsiString DeleteChar(AnsiString Str, Char C);
void PackStr(AnsiString &Str);
AnsiString MakeValidFileName(AnsiString FileName);
AnsiString RootKeyToStr(HKEY RootKey);
AnsiString BooleanToStr(bool B);
AnsiString BooleanToEngStr(bool B);
AnsiString CutToChar(AnsiString &Str, Char Ch, bool Trim);
void __fastcall OemToAnsi(AnsiString & Str);
void __fastcall AnsiToOem(AnsiString & Str);
AnsiString ExceptionLogString(Exception *E);
bool IsDots(const AnsiString Str);
AnsiString __fastcall SystemTemporaryDirectory();
AnsiString __fastcall StripPathQuotes(const AnsiString Path);
AnsiString __fastcall AddPathQuotes(AnsiString Path);
void __fastcall SplitCommand(AnsiString Command, AnsiString &Program,
  AnsiString & Params, AnsiString & Dir);
AnsiString __fastcall ExtractProgram(AnsiString Command);
AnsiString __fastcall FormatCommand(AnsiString Program, AnsiString Params);
bool __fastcall IsDisplayableStr(const AnsiString Str);
AnsiString __fastcall StrToHex(const AnsiString Str);
AnsiString __fastcall HexToStr(const AnsiString Hex);
unsigned int __fastcall HexToInt(const AnsiString Hex);
AnsiString __fastcall DecodeUrlChars(AnsiString S);
bool __fastcall RecursiveDeleteFile(const AnsiString FileName, bool ToRecycleBin);
int __fastcall CancelAnswer(int Answers);
int __fastcall AbortAnswer(int Answers);
AnsiString __fastcall LoadStr(int Ident, unsigned int MaxLength);
AnsiString __fastcall LoadStrPart(int Ident, int Part);
struct TPasLibModule;
TPasLibModule * __fastcall FindModule(void * Instance);
//---------------------------------------------------------------------------
typedef void __fastcall (__closure* TProcessLocalFileEvent)
  (const AnsiString FileName, const TSearchRec Rec, void * Param);
bool __fastcall FileSearchRec(const AnsiString FileName, TSearchRec & Rec);
void __fastcall ProcessLocalDirectory(AnsiString DirName,
  TProcessLocalFileEvent CallBackFunc, void * Param = NULL, int FindAttrs = -1);
//---------------------------------------------------------------------------
TDateTime __fastcall UnixToDateTime(__int64 TimeStamp, bool ConsiderDST);
FILETIME __fastcall DateTimeToFileTime(const TDateTime DateTime, bool ConsiderDST);
TDateTime __fastcall AdjustDateTimeFromUnix(TDateTime DateTime, bool ConsiderDST);
void __fastcall UnifyDateTimePrecision(TDateTime & DateTime1, TDateTime & DateTime2);
__int64 __fastcall ConvertTimestampToUnix(const FILETIME & FileTime,
  bool ConsiderDST);
AnsiString __fastcall FixedLenDateTimeFormat(const AnsiString & Format);
int __fastcall CompareFileTime(TDateTime T1, TDateTime T2);
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
class TUnguard
{
public:
  __fastcall TUnguard(TCriticalSection * ACriticalSection);
  __fastcall ~TUnguard();

private:
  TCriticalSection * FCriticalSection;
};
//---------------------------------------------------------------------------
// C++B TLibModule is invalid (differs from PAS definition)
struct TPasLibModule
{
  TPasLibModule * Next;
  void * Instance;
  void * CodeInstance;
  void * DataInstance;
  void * ResInstance;
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
#define CHECK(p) p
#else
#define CHECK(p) { bool __CHECK_RESULT__ = (p); assert(__CHECK_RESULT__); }
#endif
#define USEDPARAM(p) ((p) == (p))
//---------------------------------------------------------------------------
#endif
