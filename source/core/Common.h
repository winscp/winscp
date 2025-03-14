//---------------------------------------------------------------------------
#ifndef CommonH
#define CommonH
//---------------------------------------------------------------------------
#include <vector>
#include "Global.h"
//---------------------------------------------------------------------------
#define EXCEPTION throw ExtException(NULL, L"")
#define THROWOSIFFALSE(C) { if (!(C)) RaiseLastOSError(); }
#define SAFE_DESTROY_EX(CLASS, OBJ) { CLASS * PObj = OBJ; OBJ = NULL; delete PObj; }
#define SAFE_DESTROY(OBJ) SAFE_DESTROY_EX(TObject, OBJ)
#define NULL_TERMINATE(S) S[LENOF(S) - 1] = L'\0'
#define ASCOPY(dest, source) \
  { \
    AnsiString CopyBuf = source; \
    strncpy(dest, CopyBuf.c_str(), LENOF(dest)); \
    dest[LENOF(dest)-1] = '\0'; \
  }
#define SWAP(TYPE, FIRST, SECOND) \
  { TYPE __Backup = FIRST; FIRST = SECOND; SECOND = __Backup; }
//---------------------------------------------------------------------------
#define PARENTDIRECTORY L".."
#define THISDIRECTORY L"."
//---------------------------------------------------------------------------
extern const UnicodeString AnyMask;
extern const wchar_t EngShortMonthNames[12][4];
extern const char Bom[4];
extern const UnicodeString XmlDeclaration;
extern const wchar_t TokenPrefix;
extern const wchar_t NoReplacement;
extern const wchar_t TokenReplacement;
extern const UnicodeString LocalInvalidChars;
extern const UnicodeString PasswordMask;
extern const UnicodeString Ellipsis;
extern const UnicodeString TitleSeparator;
//---------------------------------------------------------------------------
extern const UnicodeString HttpProtocol;
extern const UnicodeString HttpsProtocol;
extern const UnicodeString ProtocolSeparator;
//---------------------------------------------------------------------------
UnicodeString ReplaceChar(UnicodeString Str, wchar_t A, wchar_t B);
UnicodeString DeleteChar(UnicodeString Str, wchar_t C);
void PackStr(UnicodeString & Str);
void PackStr(RawByteString & Str);
void PackStr(AnsiString & Str);
void __fastcall Shred(UnicodeString & Str);
void __fastcall Shred(UTF8String & Str);
void __fastcall Shred(AnsiString & Str);
void __fastcall Shred(RawByteString & Str);
UnicodeString AnsiToString(const RawByteString & S);
UnicodeString AnsiToString(const char * S, size_t Len);
UnicodeString UTFToString(const RawByteString & S);
UnicodeString MakeValidFileName(UnicodeString FileName);
UnicodeString RootKeyToStr(HKEY RootKey, const UnicodeString & Default = EmptyStr);
UnicodeString BooleanToStr(bool B);
UnicodeString BooleanToEngStr(bool B);
UnicodeString DefaultStr(const UnicodeString & Str, const UnicodeString & Default);
UnicodeString CutToChar(UnicodeString &Str, wchar_t Ch, bool Trim);
UnicodeString CopyToChars(const UnicodeString & Str, int & From, UnicodeString Chs, bool Trim,
  wchar_t * Delimiter = NULL, bool DoubleDelimiterEscapes = false);
UnicodeString CopyToChar(const UnicodeString & Str, wchar_t Ch, bool Trim);
UnicodeString RemoveSuffix(const UnicodeString & Str, const UnicodeString & Suffix, bool RemoveNumbersAfterSuffix = false);
UnicodeString DelimitStr(const UnicodeString & Str, wchar_t Quote = L'"');
UnicodeString MidStr(const UnicodeString & Text, int Start);
UnicodeString ShellQuoteStr(const UnicodeString & Str);
UnicodeString ExceptionLogString(Exception *E);
UnicodeString __fastcall MainInstructions(const UnicodeString & S);
bool __fastcall HasParagraphs(const UnicodeString & S);
UnicodeString __fastcall MainInstructionsFirstParagraph(const UnicodeString & S);
bool ExtractMainInstructions(UnicodeString & S, UnicodeString & MainInstructions);
UnicodeString RemoveMainInstructionsTag(UnicodeString S);
UnicodeString UnformatMessage(UnicodeString S);
UnicodeString RemoveInteractiveMsgTag(UnicodeString S);
UnicodeString RemoveEmptyLines(const UnicodeString & S);
bool IsNumber(const UnicodeString Str);
extern const wchar_t NormalizedFingerprintSeparator;
UnicodeString EncodeStrToBase64(const RawByteString & Str);
RawByteString DecodeBase64ToStr(const UnicodeString & Str);
UnicodeString Base64ToUrlSafe(const UnicodeString & S);
UnicodeString MD5ToUrlSafe(const UnicodeString & S);
bool SameChecksum(const UnicodeString & AChecksum1, const UnicodeString & AChecksum2, bool Base64);
UnicodeString __fastcall SystemTemporaryDirectory();
UnicodeString __fastcall GetShellFolderPath(int CSIdl);
UnicodeString __fastcall GetPersonalFolder();
UnicodeString __fastcall GetDesktopFolder();
UnicodeString __fastcall StripPathQuotes(const UnicodeString Path);
UnicodeString __fastcall AddQuotes(UnicodeString Str);
UnicodeString __fastcall AddPathQuotes(UnicodeString Path);
void __fastcall SplitCommand(UnicodeString Command, UnicodeString &Program,
  UnicodeString & Params, UnicodeString & Dir);
UnicodeString __fastcall ValidLocalFileName(UnicodeString FileName);
UnicodeString __fastcall ValidLocalFileName(
  UnicodeString FileName, wchar_t InvalidCharsReplacement,
  const UnicodeString & TokenizibleChars, const UnicodeString & LocalInvalidChars);
UnicodeString __fastcall ExtractProgram(UnicodeString Command);
UnicodeString __fastcall ExtractProgramName(UnicodeString Command);
UnicodeString __fastcall FormatCommand(UnicodeString Program, UnicodeString Params);
UnicodeString __fastcall ExpandFileNameCommand(const UnicodeString Command,
  const UnicodeString FileName);
void __fastcall ReformatFileNameCommand(UnicodeString & Command);
UnicodeString __fastcall EscapeParam(const UnicodeString & Param);
UnicodeString __fastcall EscapePuttyCommandParam(UnicodeString Param);
UnicodeString __fastcall StringsToParams(TStrings * Strings);
UnicodeString __fastcall ExpandEnvironmentVariables(const UnicodeString & Str);
bool __fastcall SamePaths(const UnicodeString & Path1, const UnicodeString & Path2);
UnicodeString CombinePaths(const UnicodeString & Path1, const UnicodeString & Path2);
UnicodeString GetNormalizedPath(const UnicodeString & Path);
UnicodeString GetCanonicalPath(const UnicodeString & Path);
bool __fastcall IsPathToSameFile(const UnicodeString & Path1, const UnicodeString & Path2);
int __fastcall CompareLogicalText(
  const UnicodeString & S1, const UnicodeString & S2, bool NaturalOrderNumericalSorting);
int __fastcall CompareNumber(__int64 Value1, __int64 Value2);
bool ContainsTextSemiCaseSensitive(const UnicodeString & Text, const UnicodeString & SubText);
bool __fastcall IsReservedName(UnicodeString FileName);
UnicodeString __fastcall ApiPath(UnicodeString Path);
bool IsWideChar(wchar_t Ch) { return (Ch >= L'\x80'); }
UnicodeString __fastcall DisplayableStr(const RawByteString & Str);
UnicodeString __fastcall ByteToHex(unsigned char B, bool UpperCase = true);
UnicodeString __fastcall BytesToHex(const unsigned char * B, size_t Length, bool UpperCase = true, wchar_t Separator = L'\0');
UnicodeString __fastcall BytesToHex(RawByteString Str, bool UpperCase = true, wchar_t Separator = L'\0');
UnicodeString __fastcall CharToHex(wchar_t Ch, bool UpperCase = true);
RawByteString __fastcall HexToBytes(const UnicodeString Hex);
unsigned char __fastcall HexToByte(const UnicodeString Hex);
bool __fastcall IsLowerCaseLetter(wchar_t Ch);
bool __fastcall IsUpperCaseLetter(wchar_t Ch);
bool __fastcall IsLetter(wchar_t Ch);
bool __fastcall IsDigit(wchar_t Ch);
bool __fastcall IsHex(wchar_t Ch);
UnicodeString __fastcall DecodeUrlChars(UnicodeString S);
UnicodeString __fastcall EncodeUrlString(UnicodeString S);
UnicodeString __fastcall EncodeUrlPath(UnicodeString S);
UnicodeString __fastcall AppendUrlParams(UnicodeString URL, UnicodeString Params);
UnicodeString __fastcall ExtractFileNameFromUrl(const UnicodeString & Url);
bool IsDomainOrSubdomain(const UnicodeString & FullDomain, const UnicodeString & Domain);
bool __fastcall RecursiveDeleteFile(const UnicodeString & FileName, bool ToRecycleBin = false);
int __fastcall RecursiveDeleteFileChecked(const UnicodeString & FileName, bool ToRecycleBin);
void __fastcall DeleteFileChecked(const UnicodeString & FileName);
unsigned int __fastcall CancelAnswer(unsigned int Answers);
unsigned int __fastcall AbortAnswer(unsigned int Answers);
unsigned int __fastcall ContinueAnswer(unsigned int Answers);
UnicodeString __fastcall LoadStr(int Ident, unsigned int MaxLength);
UnicodeString __fastcall LoadStrFrom(HINSTANCE Module, int Ident);
UnicodeString __fastcall LoadStrPart(int Ident, int Part);
UnicodeString __fastcall EscapeHotkey(const UnicodeString & Caption);
bool __fastcall CutToken(UnicodeString & Str, UnicodeString & Token,
  UnicodeString * RawToken = NULL, UnicodeString * Separator = NULL);
bool __fastcall CutTokenEx(UnicodeString & Str, UnicodeString & Token,
  UnicodeString * RawToken = NULL, UnicodeString * Separator = NULL);
void __fastcall AddToList(UnicodeString & List, const UnicodeString & Value, const UnicodeString & Delimiter);
void AddToShellFileListCommandLine(UnicodeString & List, const UnicodeString & Value);
bool IsWin64();
bool __fastcall IsWin7();
bool __fastcall IsWin8();
bool __fastcall IsWin10();
bool IsWin10Build(int BuildNumber);
bool IsWin11();
bool __fastcall IsWine();
void EnableUWPTestMode();
bool __fastcall IsUWP();
UnicodeString GetPackageName();
bool IsOfficialPackage();
TLibModule * __fastcall FindModule(void * Instance);
__int64 __fastcall Round(double Number);
bool __fastcall TryRelativeStrToDateTime(UnicodeString S, TDateTime & DateTime, bool Add);
bool TryStrToDateTimeStandard(const UnicodeString & S, TDateTime & Value);
bool __fastcall TryStrToSize(UnicodeString SizeStr, __int64 & Size);
UnicodeString __fastcall SizeToStr(__int64 Size);
LCID __fastcall GetDefaultLCID();
UnicodeString __fastcall DefaultEncodingName();
UnicodeString __fastcall WindowsProductName();
DWORD GetWindowsProductType();
UnicodeString __fastcall WindowsVersion();
UnicodeString __fastcall WindowsVersionLong();
bool __fastcall IsDirectoryWriteable(const UnicodeString & Path);
UnicodeString __fastcall FormatNumber(__int64 Size);
UnicodeString __fastcall FormatSize(__int64 Size);
UnicodeString __fastcall ExtractFileBaseName(const UnicodeString & Path);
TStringList * __fastcall TextToStringList(const UnicodeString & Text);
UnicodeString __fastcall StringsToText(TStrings * Strings);
TStringList * __fastcall CommaTextToStringList(const UnicodeString & CommaText);
TStrings * __fastcall CloneStrings(TStrings * Strings);
UnicodeString __fastcall TrimVersion(UnicodeString Version);
UnicodeString __fastcall FormatVersion(int MajovVersion, int MinorVersion, int Release);
TFormatSettings __fastcall GetEngFormatSettings();
int __fastcall ParseShortEngMonthName(const UnicodeString & MonthStr);
// The defaults are equal to defaults of TStringList class (except for Sorted)
TStringList * __fastcall CreateSortedStringList(bool CaseSensitive = false, System::Types::TDuplicates Duplicates = System::Types::dupIgnore);
bool SameIdent(const UnicodeString & Ident1, const UnicodeString & Ident2);
UnicodeString __fastcall FindIdent(const UnicodeString & Ident, TStrings * Idents);
UnicodeString GetTlsErrorStr(unsigned long Err);
UnicodeString GetTlsErrorStrs();
void __fastcall CheckCertificate(const UnicodeString & Path);
typedef struct x509_st X509;
typedef struct evp_pkey_st EVP_PKEY;
void __fastcall ParseCertificate(const UnicodeString & Path,
  const UnicodeString & Passphrase, X509 *& Certificate, EVP_PKEY *& PrivateKey,
  bool & WrongPassphrase);
bool __fastcall IsHttpUrl(const UnicodeString & S);
bool __fastcall IsHttpOrHttpsUrl(const UnicodeString & S);
UnicodeString __fastcall ChangeUrlProtocol(const UnicodeString & S, const UnicodeString & Protocol);
void __fastcall LoadScriptFromFile(UnicodeString FileName, TStrings * Lines, bool FallbackToAnsi = false);
UnicodeString __fastcall StripEllipsis(const UnicodeString & S);
UnicodeString __fastcall GetFileMimeType(const UnicodeString & FileName);
bool __fastcall IsRealFile(const UnicodeString & FileName);
UnicodeString GetOSInfo();
UnicodeString GetEnvironmentInfo();
void SetStringValueEvenIfEmpty(TStrings * Strings, const UnicodeString & Name, const UnicodeString & Value);
UnicodeString __fastcall GetAncestorProcessName(int Levels = 1);
UnicodeString GetAncestorProcessNames();
NORETURN void NotSupported();
NORETURN void NotImplemented();
UnicodeString GetDividerLine();
TStrings * ProcessFeatures(TStrings * Features, const UnicodeString & FeaturesOverride);
//---------------------------------------------------------------------------
struct TSearchRecSmart : public TSearchRec
{
public:
  TSearchRecSmart();
  void Clear();
  TDateTime GetLastWriteTime() const;
  bool IsRealFile() const;
  bool IsDirectory() const;
  bool IsHidden() const;
private:
  mutable FILETIME FLastWriteTimeSource;
  mutable TDateTime FLastWriteTime;
};
//---------------------------------------------------------------------------
typedef void __fastcall (__closure* TProcessLocalFileEvent)
  (const UnicodeString & FileName, const TSearchRecSmart & Rec, void * Param);
bool __fastcall FileSearchRec(const UnicodeString FileName, TSearchRec & Rec);
void CopySearchRec(const TSearchRec & Source, TSearchRec & Dest);
struct TSearchRecChecked : public TSearchRecSmart
{
  UnicodeString Path;
  UnicodeString Dir;
  bool Opened;
  UnicodeString GetFilePath() const;
};
struct TSearchRecOwned : public TSearchRecChecked
{
  ~TSearchRecOwned();
  void Close();
};
int __fastcall FindCheck(int Result, const UnicodeString & Path);
int __fastcall FindFirstUnchecked(const UnicodeString & Path, int Attr, TSearchRecChecked & F);
int __fastcall FindFirstChecked(const UnicodeString & Path, int Attr, TSearchRecChecked & F);
int __fastcall FindNextChecked(TSearchRecChecked & F);
int __fastcall FindNextUnchecked(TSearchRecChecked & F);
void __fastcall ProcessLocalDirectory(UnicodeString DirName,
  TProcessLocalFileEvent CallBackFunc, void * Param = NULL, int FindAttrs = -1);
int __fastcall FileGetAttrFix(const UnicodeString & FileName);
//---------------------------------------------------------------------------
extern const wchar_t * DSTModeNames;
enum TDSTMode
{
  dstmWin =  0, //
  dstmUnix = 1, // adjust UTC time to Windows "bug"
  dstmKeep = 2
};
bool __fastcall UsesDaylightHack();
TDateTime __fastcall EncodeDateVerbose(Word Year, Word Month, Word Day);
TDateTime __fastcall EncodeTimeVerbose(Word Hour, Word Min, Word Sec, Word MSec);
double __fastcall DSTDifferenceForTime(TDateTime DateTime);
TDateTime __fastcall SystemTimeToDateTimeVerbose(const SYSTEMTIME & SystemTime);
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
UnicodeString __fastcall StandardDatestamp();
UnicodeString __fastcall FormatTimeZone(long Sec);
UnicodeString __fastcall GetTimeZoneLogString();
bool __fastcall AdjustClockForDSTEnabled();
int __fastcall CompareFileTime(TDateTime T1, TDateTime T2);
int __fastcall TimeToMSec(TDateTime T);
int __fastcall TimeToSeconds(TDateTime T);
int __fastcall TimeToMinutes(TDateTime T);
UnicodeString FormatDateTimeSpan(const TDateTime & DateTime);
UnicodeString FormatRelativeTime(const TDateTime & ANow, const TDateTime & AThen, bool DateOnly);
TStrings * TlsCipherList();
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
enum TAssemblyLanguage { alCSharp, alVBNET, alPowerShell };
extern const UnicodeString RtfPara;
extern const UnicodeString AssemblyNamespace;
extern const UnicodeString SessionClassName;
extern const UnicodeString TransferOptionsClassName;
//---------------------------------------------------------------------
UnicodeString __fastcall RtfText(const UnicodeString & Text, bool Rtf = true);
UnicodeString __fastcall RtfColor(int Index);
UnicodeString __fastcall RtfOverrideColorText(const UnicodeString & Text);
UnicodeString __fastcall RtfColorItalicText(int Color, const UnicodeString & Text);
UnicodeString __fastcall RtfColorText(int Color, const UnicodeString & Text);
UnicodeString __fastcall RtfKeyword(const UnicodeString & Text);
UnicodeString __fastcall RtfParameter(const UnicodeString & Text);
UnicodeString __fastcall RtfString(const UnicodeString & Text);
UnicodeString __fastcall RtfLink(const UnicodeString & Link, const UnicodeString & RtfText);
UnicodeString __fastcall RtfSwitch(
  const UnicodeString & Name, const UnicodeString & Link, bool Rtf = true);
UnicodeString __fastcall RtfSwitchValue(
  const UnicodeString & Name, const UnicodeString & Link, const UnicodeString & Value, bool Rtf = true);
UnicodeString __fastcall RtfSwitch(
  const UnicodeString & Name, const UnicodeString & Link, const UnicodeString & Value, bool Rtf = true);
UnicodeString __fastcall RtfSwitch(
  const UnicodeString & Name, const UnicodeString & Link, int Value, bool Rtf = true);
UnicodeString __fastcall RtfEscapeParam(UnicodeString Param, bool PowerShellEscape);
UnicodeString __fastcall RtfRemoveHyperlinks(UnicodeString Text);
UnicodeString __fastcall ScriptCommandLink(const UnicodeString & Command);
UnicodeString __fastcall AssemblyBoolean(TAssemblyLanguage Language, bool Value);
UnicodeString __fastcall AssemblyString(TAssemblyLanguage Language, UnicodeString S);
UnicodeString __fastcall AssemblyCommentLine(TAssemblyLanguage Language, const UnicodeString & Text);
UnicodeString __fastcall AssemblyPropertyRaw(
  TAssemblyLanguage Language, const UnicodeString & ClassName, const UnicodeString & Name,
  const UnicodeString & Value, bool Inline);
UnicodeString __fastcall AssemblyProperty(
  TAssemblyLanguage Language, const UnicodeString & ClassName, const UnicodeString & Name,
  const UnicodeString & Type, const UnicodeString & Member, bool Inline);
UnicodeString __fastcall AssemblyProperty(
  TAssemblyLanguage Language, const UnicodeString & ClassName, const UnicodeString & Name,
  const UnicodeString & Value, bool Inline);
UnicodeString __fastcall AssemblyProperty(
  TAssemblyLanguage Language, const UnicodeString & ClassName, const UnicodeString & Name, int Value, bool Inline);
UnicodeString __fastcall AssemblyProperty(
  TAssemblyLanguage Language, const UnicodeString & ClassName, const UnicodeString & Name, bool Value, bool Inline);
UnicodeString __fastcall RtfLibraryMethod(const UnicodeString & ClassName, const UnicodeString & MethodName, bool Inpage);
UnicodeString __fastcall RtfLibraryClass(const UnicodeString & ClassName);
UnicodeString __fastcall AssemblyVariableName(TAssemblyLanguage Language, const UnicodeString & ClassName);
UnicodeString __fastcall AssemblyStatementSeparator(TAssemblyLanguage Language);
UnicodeString __fastcall AssemblyVariableDeclaration(TAssemblyLanguage Language);
UnicodeString __fastcall AssemblyNewClassInstance(
  TAssemblyLanguage Language, const UnicodeString & ClassName, bool Inline);
UnicodeString __fastcall AssemblyNewClassInstanceStart(
  TAssemblyLanguage Language, const UnicodeString & ClassName, bool Inline);
UnicodeString __fastcall AssemblyNewClassInstanceEnd(TAssemblyLanguage Language, bool Inline);
UnicodeString __fastcall AssemblyAddRawSettings(
  TAssemblyLanguage Language, TStrings * RawSettings, const UnicodeString & ClassName,
  const UnicodeString & MethodName);
//---------------------------------------------------------------------------
template<class T>
class TValueRestorer
{
public:
  __fastcall TValueRestorer(T & Target, const T & Value) :
    FTarget(Target),
    FValue(Target),
    FArmed(true)
  {
    FTarget = Value;
  }

  __fastcall TValueRestorer(T & Target) :
    FTarget(Target),
    FValue(Target),
    FArmed(true)
  {
  }

  void Release()
  {
    if (FArmed)
    {
      FTarget = FValue;
      FArmed = false;
    }
  }

  __fastcall ~TValueRestorer()
  {
    Release();
  }

protected:
  T & FTarget;
  T FValue;
  bool FArmed;
};
//---------------------------------------------------------------------------
class TAutoNestingCounter : public TValueRestorer<int>
{
public:
  __fastcall TAutoNestingCounter(int & Target) :
    TValueRestorer<int>(Target, Target + 1)
  {
    DebugAssert(FValue >= 0);
  }

  __fastcall ~TAutoNestingCounter()
  {
    DebugAssert(!FArmed || (FTarget == (FValue + 1)));
  }
};
//---------------------------------------------------------------------------
class TAutoFlag : public TValueRestorer<bool>
{
public:
  __fastcall TAutoFlag(bool & Target) :
    TValueRestorer<bool>(Target, true)
  {
    DebugAssert(!FValue);
  }

  __fastcall ~TAutoFlag()
  {
    DebugAssert(!FArmed || FTarget);
  }
};
//---------------------------------------------------------------------------
#include <map>
//---------------------------------------------------------------------------
template<class T1, class T2>
class BiDiMap
{
public:
  typedef std::map<T1, T2> TFirstToSecond;
  typedef typename TFirstToSecond::const_iterator const_iterator;

  void Add(const T1 & Value1, const T2 & Value2)
  {
    FFirstToSecond.insert(std::make_pair(Value1, Value2));
    FSecondToFirst.insert(std::make_pair(Value2, Value1));
  }

  T1 LookupFirst(const T2 & Value2) const
  {
    typename TSecondToFirst::const_iterator Iterator = FSecondToFirst.find(Value2);
    DebugAssert(Iterator != FSecondToFirst.end());
    return Iterator->second;
  }

  T2 LookupSecond(const T1 & Value1) const
  {
    const_iterator Iterator = FFirstToSecond.find(Value1);
    DebugAssert(Iterator != FFirstToSecond.end());
    return Iterator->second;
  }

  const_iterator begin()
  {
    return FFirstToSecond.begin();
  }

  const_iterator end()
  {
    return FFirstToSecond.end();
  }

private:
  TFirstToSecond FFirstToSecond;
  typedef std::map<T2, T1> TSecondToFirst;
  TSecondToFirst FSecondToFirst;
};
//---------------------------------------------------------------------------
template<class T>
class TMulticastEvent
{
public:
  TMulticastEvent()
  {
    // noop
  }

  TMulticastEvent(const TMulticastEvent & Other) :
    FEventHandlers(Other.FEventHandlers)
  {
  }

  explicit TMulticastEvent(T EventHandler)
  {
    Add(EventHandler);
  }

  void Add(T EventHandler)
  {
    DebugAssert(EventHandler != NULL);
    DebugAssert(Find(EventHandler) == FEventHandlers.end());
    FEventHandlers.push_back(EventHandler);
  }

  void Remove(T EventHandler)
  {
    typename TEventHandlers::iterator I = Find(EventHandler);
    if (DebugAlwaysTrue(I != FEventHandlers.end()))
    {
      FEventHandlers.erase(I);
    }
  }

  #pragma warn -inl
  template<typename P>
  void Invoke(const P & p)
  {
    typename TEventHandlers::iterator I = FEventHandlers.begin();
    while (I != FEventHandlers.end())
    {
      (*I)(p);
      ++I;
    }
  }
  #pragma warn .inl

  bool Contains(T EventHandler)
  {
    return (Find(EventHandler) != FEventHandlers.end());
  }

  bool Any() const
  {
    return (FEventHandlers.size() > 0);
  }

  bool operator==(const TMulticastEvent<T> Other) const
  {
    return (FEventHandlers == Other.FEventHandlers);
  }

  void operator=(const TMulticastEvent<T> & Other)
  {
    FEventHandlers = Other.FEventHandlers;
  }

private:
  typedef std::vector<T> TEventHandlers;
  TEventHandlers FEventHandlers;

  typename TEventHandlers::iterator Find(T EventHandler)
  {
    return std::find(FEventHandlers.begin(), FEventHandlers.end(), EventHandler);
  }

};
//---------------------------------------------------------------------------
typedef std::vector<UnicodeString> TUnicodeStringVector;
//---------------------------------------------------------------------------
#endif
