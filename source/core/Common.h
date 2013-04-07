//---------------------------------------------------------------------------
#ifndef CommonH
#define CommonH
//---------------------------------------------------------------------------
#define EXCEPTION throw ExtException(NULL, L"")
#define THROWOSIFFALSE(C) if (!(C)) RaiseLastOSError();
#define SAFE_DESTROY_EX(CLASS, OBJ) { CLASS * PObj = OBJ; OBJ = NULL; delete PObj; }
#define SAFE_DESTROY(OBJ) SAFE_DESTROY_EX(TObject, OBJ)
#define ASCOPY(dest, source) \
  { \
    AnsiString CopyBuf = source; \
    strncpy(dest, CopyBuf.c_str(), LENOF(dest)); \
    dest[LENOF(dest)-1] = '\0'; \
  }
#define FORMAT(S, F) Format(S, ARRAYOFCONST(F))
#define FMTLOAD(I, F) FmtLoadStr(I, ARRAYOFCONST(F))
#define LENOF(x) ( (sizeof((x))) / (sizeof(*(x))))
#define FLAGSET(SET, FLAG) (((SET) & (FLAG)) == (FLAG))
#define FLAGCLEAR(SET, FLAG) (((SET) & (FLAG)) == 0)
#define FLAGMASK(ENABLE, FLAG) ((ENABLE) ? (FLAG) : 0)
#define SWAP(TYPE, FIRST, SECOND) \
  { TYPE __Backup = FIRST; FIRST = SECOND; SECOND = __Backup; }
//---------------------------------------------------------------------------
extern const wchar_t EngShortMonthNames[12][4];
extern const char Bom[3];
extern const wchar_t TokenPrefix;
extern const wchar_t NoReplacement;
extern const wchar_t TokenReplacement;
extern const UnicodeString LocalInvalidChars;
//---------------------------------------------------------------------------
UnicodeString ReplaceChar(UnicodeString Str, wchar_t A, wchar_t B);
UnicodeString DeleteChar(UnicodeString Str, wchar_t C);
void PackStr(UnicodeString &Str);
void PackStr(RawByteString &Str);
void __fastcall Shred(UnicodeString & Str);
UnicodeString MakeValidFileName(UnicodeString FileName);
UnicodeString RootKeyToStr(HKEY RootKey);
UnicodeString BooleanToStr(bool B);
UnicodeString BooleanToEngStr(bool B);
UnicodeString DefaultStr(const UnicodeString & Str, const UnicodeString & Default);
UnicodeString CutToChar(UnicodeString &Str, wchar_t Ch, bool Trim);
UnicodeString CopyToChars(const UnicodeString & Str, int & From, UnicodeString Chs, bool Trim,
  wchar_t * Delimiter = NULL, bool DoubleDelimiterEscapes = false);
UnicodeString DelimitStr(UnicodeString Str, UnicodeString Chars);
UnicodeString ShellDelimitStr(UnicodeString Str, wchar_t Quote);
UnicodeString ExceptionLogString(Exception *E);
bool IsNumber(const UnicodeString Str);
UnicodeString __fastcall SystemTemporaryDirectory();
UnicodeString __fastcall GetShellFolderPath(int CSIdl);
UnicodeString __fastcall StripPathQuotes(const UnicodeString Path);
UnicodeString __fastcall AddPathQuotes(UnicodeString Path);
void __fastcall SplitCommand(UnicodeString Command, UnicodeString &Program,
  UnicodeString & Params, UnicodeString & Dir);
UnicodeString __fastcall ValidLocalFileName(UnicodeString FileName);
UnicodeString __fastcall ValidLocalFileName(
  UnicodeString FileName, wchar_t InvalidCharsReplacement,
  const UnicodeString & TokenizibleChars, const UnicodeString & LocalInvalidChars);
UnicodeString __fastcall ExtractProgram(UnicodeString Command);
UnicodeString __fastcall FormatCommand(UnicodeString Program, UnicodeString Params);
UnicodeString __fastcall ExpandFileNameCommand(const UnicodeString Command,
  const UnicodeString FileName);
void __fastcall ReformatFileNameCommand(UnicodeString & Command);
UnicodeString __fastcall EscapePuttyCommandParam(UnicodeString Param);
UnicodeString __fastcall ExpandEnvironmentVariables(const UnicodeString & Str);
bool __fastcall ComparePaths(const UnicodeString & Path1, const UnicodeString & Path2);
bool __fastcall CompareFileName(const UnicodeString & Path1, const UnicodeString & Path2);
bool __fastcall IsReservedName(UnicodeString FileName);
UnicodeString __fastcall DisplayableStr(const RawByteString & Str);
UnicodeString __fastcall ByteToHex(unsigned char B, bool UpperCase = true);
UnicodeString __fastcall BytesToHex(const unsigned char * B, size_t Length, bool UpperCase = true, wchar_t Separator = L'\0');
UnicodeString __fastcall BytesToHex(RawByteString Str, bool UpperCase = true, wchar_t Separator = L'\0');
UnicodeString __fastcall CharToHex(wchar_t Ch, bool UpperCase = true);
RawByteString __fastcall HexToBytes(const UnicodeString Hex);
unsigned char __fastcall HexToByte(const UnicodeString Hex);
UnicodeString __fastcall DecodeUrlChars(UnicodeString S);
UnicodeString __fastcall EncodeUrlChars(UnicodeString S, UnicodeString Ignore = L"");
UnicodeString __fastcall EncodeUrlString(UnicodeString S);
bool __fastcall RecursiveDeleteFile(const UnicodeString FileName, bool ToRecycleBin);
unsigned int __fastcall CancelAnswer(unsigned int Answers);
unsigned int __fastcall AbortAnswer(unsigned int Answers);
unsigned int __fastcall ContinueAnswer(unsigned int Answers);
UnicodeString __fastcall LoadStr(int Ident, unsigned int MaxLength);
UnicodeString __fastcall LoadStrPart(int Ident, int Part);
UnicodeString __fastcall EscapeHotkey(const UnicodeString & Caption);
bool __fastcall CutToken(UnicodeString & Str, UnicodeString & Token,
  UnicodeString * RawToken = NULL);
void __fastcall AddToList(UnicodeString & List, const UnicodeString & Value, const UnicodeString & Delimiter);
bool __fastcall Is2000();
bool __fastcall IsWin7();
bool __fastcall IsExactly2008R2();
TLibModule * __fastcall FindModule(void * Instance);
__int64 __fastcall Round(double Number);
bool __fastcall TryRelativeStrToDateTime(UnicodeString S, TDateTime & DateTime);
LCID __fastcall GetDefaultLCID();
UnicodeString __fastcall DefaultEncodingName();
UnicodeString __fastcall WindowsProductName();
bool __fastcall IsDirectoryWriteable(const UnicodeString & Path);
//---------------------------------------------------------------------------
typedef void __fastcall (__closure* TProcessLocalFileEvent)
  (const UnicodeString FileName, const TSearchRec Rec, void * Param);
bool __fastcall FileSearchRec(const UnicodeString FileName, TSearchRec & Rec);
int __fastcall FindCheck(int Result);
int __fastcall FindFirstChecked(const UnicodeString & Path, int Attr, TSearchRec & F);
int __fastcall FindNextChecked(TSearchRec & F);
void __fastcall ProcessLocalDirectory(UnicodeString DirName,
  TProcessLocalFileEvent CallBackFunc, void * Param = NULL, int FindAttrs = -1);
//---------------------------------------------------------------------------
enum TDSTMode
{
  dstmWin =  0, //
  dstmUnix = 1, // adjust UTC time to Windows "bug"
  dstmKeep = 2
};
bool __fastcall UsesDaylightHack();
TDateTime __fastcall EncodeDateVerbose(Word Year, Word Month, Word Day);
TDateTime __fastcall EncodeTimeVerbose(Word Hour, Word Min, Word Sec, Word MSec);
TDateTime __fastcall UnixToDateTime(__int64 TimeStamp, TDSTMode DSTMode);
TDateTime __fastcall ConvertTimestampToUTC(TDateTime DateTime);
TDateTime __fastcall ConvertTimestampFromUTC(TDateTime DateTime);
FILETIME __fastcall DateTimeToFileTime(const TDateTime DateTime, TDSTMode DSTMode);
TDateTime __fastcall AdjustDateTimeFromUnix(TDateTime DateTime, TDSTMode DSTMode);
void __fastcall UnifyDateTimePrecision(TDateTime & DateTime1, TDateTime & DateTime2);
TDateTime __fastcall FileTimeToDateTime(const FILETIME & FileTime);
__int64 __fastcall ConvertTimestampToUnix(const FILETIME & FileTime,
  TDSTMode DSTMode);
__int64 __fastcall ConvertTimestampToUnixSafe(const FILETIME & FileTime,
  TDSTMode DSTMode);
UnicodeString __fastcall FixedLenDateTimeFormat(const UnicodeString & Format);
UnicodeString __fastcall StandardTimestamp(const TDateTime & DateTime);
UnicodeString __fastcall StandardTimestamp();
UnicodeString __fastcall GetTimeZoneLogString();
int __fastcall CompareFileTime(TDateTime T1, TDateTime T2);
int __fastcall TimeToMSec(TDateTime T);
int __fastcall TimeToMinutes(TDateTime T);
//---------------------------------------------------------------------------
template<class MethodT>
MethodT __fastcall MakeMethod(void * Data, void * Code)
{
  MethodT Method;
  ((TMethod*)&Method)->Data = Data;
  ((TMethod*)&Method)->Code = Code;
  return Method;
}
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
#ifndef _DEBUG
#undef assert
#define assert(p)   ((void)0)
#define CHECK(p) p
#define FAIL
#define ALWAYS_TRUE(p) p
#define ALWAYS_FALSE(p) p
#define NOT_NULL(P) P
#else
#define CHECK(p) { bool __CHECK_RESULT__ = (p); assert(__CHECK_RESULT__); }
#define FAIL assert(false)
#define ALWAYS_TRUE(p) DoAlwaysTrue(p, TEXT(#p), TEXT(__FILE__), __LINE__)
#define ALWAYS_FALSE(p) DoAlwaysFalse(p, TEXT(#p), TEXT(__FILE__), __LINE__)
#define NOT_NULL(P) DoCheckNotNull(P, TEXT(#P), TEXT(__FILE__), __LINE__)
#endif
#define USEDPARAM(p) ((&p) == (&p))
//---------------------------------------------------------------------------
template<class T>
class TValueRestorer
{
public:
  __fastcall TValueRestorer(T & Target, const T & Value) :
    FTarget(Target),
    FValue(Value)
  {
  }

  __fastcall ~TValueRestorer()
  {
    FTarget = FValue;
  }

protected:
  T & FTarget;
  T FValue;
};
//---------------------------------------------------------------------------
class TBoolRestorer : TValueRestorer<bool>
{
public:
  __fastcall TBoolRestorer(bool & Target) :
    TValueRestorer<bool>(Target, !Target)
  {
  }
};
//---------------------------------------------------------------------------
class TAutoNestingCounter : TValueRestorer<int>
{
public:
  __fastcall TAutoNestingCounter(int & Target) :
    TValueRestorer<int>(Target, Target)
  {
    assert(Target >= 0);
    ++Target;
  }

  __fastcall ~TAutoNestingCounter()
  {
    assert(FTarget == (FValue + 1));
  }
};
//---------------------------------------------------------------------------
#endif
