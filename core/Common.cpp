//---------------------------------------------------------------------------
#define NO_WIN32_LEAN_AND_MEAN
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "Exceptions.h"
#include "TextsCore.h"
#include "Interface.h"
#include <StrUtils.hpp>
#include <math.h>
#include <shfolder.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// TCriticalSection
//---------------------------------------------------------------------------
__fastcall TCriticalSection::TCriticalSection()
{
  FAcquired = 0;
  InitializeCriticalSection(&FSection);
}
//---------------------------------------------------------------------------
__fastcall TCriticalSection::~TCriticalSection()
{
  assert(FAcquired == 0);
  DeleteCriticalSection(&FSection);
}
//---------------------------------------------------------------------------
void __fastcall TCriticalSection::Enter()
{
  EnterCriticalSection(&FSection);
  FAcquired++;
}
//---------------------------------------------------------------------------
void __fastcall TCriticalSection::Leave()
{
  FAcquired--;
  LeaveCriticalSection(&FSection);
}
//---------------------------------------------------------------------------
// TGuard
//---------------------------------------------------------------------------
__fastcall TGuard::TGuard(TCriticalSection * ACriticalSection) :
  FCriticalSection(ACriticalSection)
{
  assert(ACriticalSection != NULL);
  FCriticalSection->Enter();
}
//---------------------------------------------------------------------------
__fastcall TGuard::~TGuard()
{
  FCriticalSection->Leave();
}
//---------------------------------------------------------------------------
// TUnguard
//---------------------------------------------------------------------------
__fastcall TUnguard::TUnguard(TCriticalSection * ACriticalSection) :
  FCriticalSection(ACriticalSection)
{
  assert(ACriticalSection != NULL);
  FCriticalSection->Leave();
}
//---------------------------------------------------------------------------
__fastcall TUnguard::~TUnguard()
{
  FCriticalSection->Enter();
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
const char EngShortMonthNames[12][4] =
  {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
//---------------------------------------------------------------------------
AnsiString ReplaceChar(AnsiString Str, Char A, Char B)
{
  for (Integer Index = 0; Index < Str.Length(); Index++)
    if (Str[Index+1] == A) Str[Index+1] = B;
  return Str;
}
//---------------------------------------------------------------------------
AnsiString DeleteChar(AnsiString Str, Char C)
{
  int P;
  while ((P = Str.Pos(C)) > 0)
  {
    Str.Delete(P, 1);
  }
  return Str;
}
//---------------------------------------------------------------------------
void PackStr(AnsiString &Str)
{
  // Following will free unnecessary bytes
  Str = Str.c_str();
}
//---------------------------------------------------------------------------
AnsiString MakeValidFileName(AnsiString FileName)
{
  AnsiString IllegalChars = ";,=+<>|\"[] \\/?*";
  for (int Index = 0; Index < IllegalChars.Length(); Index++)
  {
    FileName = ReplaceChar(FileName, IllegalChars[Index+1], '-');
  }
  return FileName;
}
//---------------------------------------------------------------------------
AnsiString RootKeyToStr(HKEY RootKey)
{
  if (RootKey == HKEY_USERS) return "HKEY_USERS";
    else
  if (RootKey == HKEY_LOCAL_MACHINE) return "HKEY_LOCAL_MACHINE";
    else
  if (RootKey == HKEY_CURRENT_USER) return "HKEY_CURRENT_USER";
    else
  if (RootKey == HKEY_CLASSES_ROOT) return "HKEY_CLASSES_ROOT";
    else
  if (RootKey == HKEY_CURRENT_CONFIG) return "HKEY_CURRENT_CONFIG";
    else
  if (RootKey == HKEY_DYN_DATA) return "HKEY_DYN_DATA";
    else
  {  Abort(); return ""; };
}
//---------------------------------------------------------------------------
AnsiString BooleanToEngStr(bool B)
{
  return B ? "Yes" : "No";
}
//---------------------------------------------------------------------------
AnsiString BooleanToStr(bool B)
{
  return B ? LoadStr(YES_STR) : LoadStr(NO_STR);
}
//---------------------------------------------------------------------------
AnsiString DefaultStr(const AnsiString & Str, const AnsiString & Default)
{
  if (!Str.IsEmpty())
  {
    return Str;
  }
  else
  {
    return Default;
  }
}
//---------------------------------------------------------------------------
AnsiString CutToChar(AnsiString &Str, Char Ch, bool Trim)
{
  Integer P = Str.Pos(Ch);
  AnsiString Result;
  if (P)
  {
    Result = Str.SubString(1, P-1);
    Str.Delete(1, P);
  }
  else
  {
    Result = Str;
    Str = "";
  }
  if (Trim)
  {
    Result = Result.TrimRight();
    Str = Str.TrimLeft();
  }
  return Result;
}
//---------------------------------------------------------------------------
AnsiString CutToChars(AnsiString & Str, AnsiString Chs, bool Trim,
  char * Delimiter)
{
  int P;
  for (P = 1; P <= Str.Length(); P++)
  {
    if (IsDelimiter(Chs, Str, P))
    {
      break;
    }
  }

  AnsiString Result;
  if (P <= Str.Length())
  {
    if (Delimiter != NULL)
    {
      *Delimiter = Str[P];
    }
    Result = Str.SubString(1, P-1);
    Str.Delete(1, P);
  }
  else
  {
    if (Delimiter != NULL)
    {
      *Delimiter = '\0';
    }
    Result = Str;
    Str = "";
  }
  if (Trim)
  {
    Result = Result.TrimRight();
    Str = Str.TrimLeft();
  }
  return Result;
}
//---------------------------------------------------------------------------
AnsiString DelimitStr(AnsiString Str, AnsiString Chars)
{
  for (int i = 1; i <= Str.Length(); i++)
  {
    if (Str.IsDelimiter(Chars, i))
    {
      Str.Insert("\\", i);
      i++;
    }
  }
  return Str;
}
//---------------------------------------------------------------------------
AnsiString ShellDelimitStr(AnsiString Str, char Quote)
{
  AnsiString Chars = "$\\";
  if (Quote == '"')
  {
    Chars += "`\"";
  }
  return DelimitStr(Str, Chars);
}
//---------------------------------------------------------------------------
AnsiString ExceptionLogString(Exception *E)
{
  assert(E);
  if (E->InheritsFrom(__classid(Exception)))
  {
    AnsiString Msg;
    Msg = FORMAT("(%s) %s", (E->ClassName(), E->Message));
    if (E->InheritsFrom(__classid(ExtException)))
    {
      TStrings * MoreMessages = ((ExtException*)E)->MoreMessages;
      if (MoreMessages)
      {
        Msg += "\n" +
          StringReplace(MoreMessages->Text, "\r", "", TReplaceFlags() << rfReplaceAll);
      }
    }
    return Msg;
  }
  else
  {
    char Buffer[1024];
    ExceptionErrorMessage(ExceptObject(), ExceptAddr(), Buffer, sizeof(Buffer));
    return AnsiString(Buffer);
  }
}
//---------------------------------------------------------------------------
bool IsDots(const AnsiString Str)
{
  char * str = Str.c_str();
  return (str[strspn(str, ".")] == '\0');
}
//---------------------------------------------------------------------------
AnsiString __fastcall SystemTemporaryDirectory()
{
  AnsiString TempDir;
  TempDir.SetLength(MAX_PATH);
  TempDir.SetLength(GetTempPath(MAX_PATH, TempDir.c_str()));
  return TempDir;
}
//---------------------------------------------------------------------------
AnsiString __fastcall GetShellFolderPath(int CSIdl)
{
  AnsiString Result;
  HMODULE Shell32Lib = LoadLibrary("SHELL32.DLL");
  if (Shell32Lib != NULL)
  {
    PFNSHGETFOLDERPATH SHGetFolderPath = (PFNSHGETFOLDERPATH)
      GetProcAddress(Shell32Lib, "SHGetFolderPathA");
    if (SHGetFolderPath != NULL)
    {
      char Path[2 * MAX_PATH + 10] = "\0";
      if (SUCCEEDED(SHGetFolderPath(NULL, CSIdl, NULL, SHGFP_TYPE_CURRENT, Path)))
      {
        Result = Path;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall StripPathQuotes(const AnsiString Path)
{
  if ((Path.Length() >= 2) &&
      (Path[1] == '\"') && (Path[Path.Length()] == '\"'))
  {
    return Path.SubString(2, Path.Length() - 2);
  }
  else
  {
    return Path;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall AddPathQuotes(AnsiString Path)
{
  Path = StripPathQuotes(Path);
  if (Path.Pos(" "))
  {
    Path = "\"" + Path + "\"";
  }
  return Path;
}
//---------------------------------------------------------------------------
void __fastcall SplitCommand(AnsiString Command, AnsiString &Program,
  AnsiString & Params, AnsiString & Dir)
{
  Command = Command.Trim();
  Params = "";
  Dir = "";
  if (!Command.IsEmpty() && (Command[1] == '\"'))
  {
    Command.Delete(1, 1);
    int P = Command.Pos('"');
    if (P)
    {
      Program = Command.SubString(1, P-1).Trim();
      Params = Command.SubString(P + 1, Command.Length() - P).Trim();
    }
    else
    {
      throw Exception(FMTLOAD(INVALID_SHELL_COMMAND, ("\"" + Command)));
    }
  }
  else
  {
    int P = Command.Pos(" ");
    if (P)
    {
      Program = Command.SubString(1, P).Trim();
      Params = Command.SubString(P + 1, Command.Length() - P).Trim();
    }
    else
    {
      Program = Command;
    }
  }
  int B = Program.LastDelimiter("\\/");
  if (B)
  {
    Dir = Program.SubString(1, B).Trim();
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall ExtractProgram(AnsiString Command)
{
  AnsiString Program;
  AnsiString Params;
  AnsiString Dir;

  SplitCommand(Command, Program, Params, Dir);

  return Program;
}
//---------------------------------------------------------------------------
AnsiString __fastcall FormatCommand(AnsiString Program, AnsiString Params)
{
  Program = Program.Trim();
  Params = Params.Trim();
  if (!Params.IsEmpty()) Params = " " + Params;
  if (Program.Pos(" ")) Program = "\"" + Program + "\"";
  return Program + Params;
}
//---------------------------------------------------------------------------
const char ShellCommandFileNamePattern[] = "!.!";
//---------------------------------------------------------------------------
void __fastcall ReformatFileNameCommand(AnsiString & Command)
{
  AnsiString Program, Params, Dir;
  SplitCommand(Command, Program, Params, Dir);
  if (Params.Pos(ShellCommandFileNamePattern) == 0)
  {
    Params = Params + (Params.IsEmpty() ? "" : " ") + ShellCommandFileNamePattern;
  }
  Command = FormatCommand(Program, Params);
}
//---------------------------------------------------------------------------
AnsiString __fastcall ExpandFileNameCommand(const AnsiString Command,
  const AnsiString FileName)
{
  return AnsiReplaceStr(Command, ShellCommandFileNamePattern,
    AddPathQuotes(FileName));
}
//---------------------------------------------------------------------------
AnsiString __fastcall ExpandEnvironmentVariables(const AnsiString & Str)
{
  AnsiString Buf;
  unsigned int Size = 1024;

  Buf.SetLength(Size);
  Buf.Unique();
  unsigned int Len = ExpandEnvironmentStrings(Str.c_str(), Buf.c_str(), Size);

  if (Len > Size)
  {
    Buf.SetLength(Len);
    Buf.Unique();
    ExpandEnvironmentStrings(Str.c_str(), Buf.c_str(), Len);
  }

  PackStr(Buf);

  return Buf;
}
//---------------------------------------------------------------------------
bool __fastcall CompareFileName(const AnsiString & Path1, const AnsiString & Path2)
{
  AnsiString ShortPath1 = ExtractShortPathName(Path1);
  AnsiString ShortPath2 = ExtractShortPathName(Path2);

  bool Result;
  // ExtractShortPathName returns empty string if file does not exist
  if (ShortPath1.IsEmpty() || ShortPath2.IsEmpty())
  {
    Result = AnsiSameText(Path1, Path2);
  }
  else
  {
    Result = AnsiSameText(ExtractShortPathName(Path1), ExtractShortPathName(Path2));
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall ComparePaths(const AnsiString & Path1, const AnsiString & Path2)
{
  // TODO: ExpandUNCFileName
  return AnsiSameText(IncludeTrailingBackslash(Path1), IncludeTrailingBackslash(Path2));
}
//---------------------------------------------------------------------------
bool __fastcall IsDisplayableStr(const AnsiString Str)
{
  bool Displayable = true;
  int Index = 1;
  while ((Index <= Str.Length()) && Displayable)
  {
    if (Str[Index] < '\32')
    {
      Displayable = false;
    }
    Index++;
  }
  return Displayable;
}
//---------------------------------------------------------------------------
AnsiString __fastcall CharToHex(char Ch)
{
  return IntToHex((unsigned char)Ch, 2);
}
//---------------------------------------------------------------------------
AnsiString __fastcall StrToHex(const AnsiString Str)
{
  AnsiString Result;
  for (int i = 1; i <= Str.Length(); i++)
  {
    Result += CharToHex(Str[i]);
  }
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall HexToStr(const AnsiString Hex)
{
  static AnsiString Digits = "0123456789ABCDEF";
  AnsiString Result;
  int L, P1, P2;
  L = Hex.Length();
  if (L % 2 == 0)
  {
    for (int i = 1; i <= Hex.Length(); i += 2)
    {
      P1 = Digits.Pos((char)toupper(Hex[i]));
      P2 = Digits.Pos((char)toupper(Hex[i + 1]));
      if (P1 <= 0 || P2 <= 0)
      {
        Result = "";
        break;
      }
      else
      {
        Result += static_cast<char>((P1 - 1) * 16 + P2 - 1);
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
unsigned int __fastcall HexToInt(const AnsiString Hex, int MinChars)
{
  static AnsiString Digits = "0123456789ABCDEF";
  int Result = 0;
  int I = 1;
  while (I <= Hex.Length())
  {
    int A = Digits.Pos((char)toupper(Hex[I]));
    if (A <= 0)
    {
      if ((MinChars < 0) || (I <= MinChars))
      {
        Result = 0;
      }
      break;
    }

    Result = (Result * 16) + (A - 1);

    I++;
  }
  return Result;
}
//---------------------------------------------------------------------------
char __fastcall HexToChar(const AnsiString Hex)
{
  return (char)HexToInt(Hex);
}
//---------------------------------------------------------------------------
bool __fastcall FileSearchRec(const AnsiString FileName, TSearchRec & Rec)
{
  int FindAttrs = faReadOnly | faHidden | faSysFile | faDirectory | faArchive;
  bool Result = (FindFirst(FileName, FindAttrs, Rec) == 0);
  if (Result)
  {
    FindClose(Rec);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall ProcessLocalDirectory(AnsiString DirName,
  TProcessLocalFileEvent CallBackFunc, void * Param,
  int FindAttrs)
{
  assert(CallBackFunc);
  if (FindAttrs < 0)
  {
    FindAttrs = faReadOnly | faHidden | faSysFile | faDirectory | faArchive;
  }
  TSearchRec SearchRec;

  DirName = IncludeTrailingBackslash(DirName);
  if (FindFirst(DirName + "*.*", FindAttrs, SearchRec) == 0)
  {
    try
    {
      do
      {
        if ((SearchRec.Name != ".") && (SearchRec.Name != ".."))
        {
          CallBackFunc(DirName + SearchRec.Name, SearchRec, Param);
        }

      } while (FindNext(SearchRec) == 0);
    }
    __finally
    {
      FindClose(SearchRec);
    }
  }
}
//---------------------------------------------------------------------------
struct TDateTimeParams
{
  TDateTime UnixEpoch;
  double BaseDifference;
  long BaseDifferenceSec;
  double CurrentDaylightDifference;
  long CurrentDaylightDifferenceSec;
  double CurrentDifference;
  long CurrentDifferenceSec;
  double StandardDifference;
  long StandardDifferenceSec;
  double DaylightDifference;
  long DaylightDifferenceSec;
  SYSTEMTIME StandardDate;
  SYSTEMTIME DaylightDate;
};
static bool DateTimeParamsInitialized = false;
static TDateTimeParams DateTimeParams;
static TCriticalSection DateTimeParamsSection;
//---------------------------------------------------------------------------
static TDateTimeParams * __fastcall GetDateTimeParams()
{
  if (!DateTimeParamsInitialized)
  {
    TGuard Guard(&DateTimeParamsSection);
    if (!DateTimeParamsInitialized)
    {
      TIME_ZONE_INFORMATION TZI;
      unsigned long GTZI;

      GTZI = GetTimeZoneInformation(&TZI);
      switch (GTZI)
      {
        case TIME_ZONE_ID_UNKNOWN:
          DateTimeParams.CurrentDifferenceSec = 0;
          DateTimeParams.CurrentDaylightDifferenceSec = 0;
          break;

        case TIME_ZONE_ID_STANDARD:
          DateTimeParams.CurrentDaylightDifferenceSec = TZI.StandardBias;
          break;

        case TIME_ZONE_ID_DAYLIGHT:
          DateTimeParams.CurrentDaylightDifferenceSec = TZI.DaylightBias;
          break;

        case TIME_ZONE_ID_INVALID:
        default:
          throw Exception(TIMEZONE_ERROR);
      }
      // Is it same as SysUtils::UnixDateDelta = 25569 ??
      DateTimeParams.UnixEpoch = EncodeDate(1970, 1, 1);

      DateTimeParams.BaseDifferenceSec = TZI.Bias;
      DateTimeParams.BaseDifference = double(TZI.Bias) / 1440;
      DateTimeParams.BaseDifferenceSec *= 60;

      DateTimeParams.CurrentDifferenceSec = TZI.Bias +
        DateTimeParams.CurrentDaylightDifferenceSec;
      DateTimeParams.CurrentDifference =
        double(DateTimeParams.CurrentDifferenceSec) / 1440;
      DateTimeParams.CurrentDifferenceSec *= 60;

      DateTimeParams.CurrentDaylightDifference =
        double(DateTimeParams.CurrentDaylightDifferenceSec) / 1440;
      DateTimeParams.CurrentDaylightDifferenceSec *= 60;

      DateTimeParams.DaylightDifferenceSec = TZI.DaylightBias * 60;
      DateTimeParams.DaylightDifference = double(TZI.DaylightBias) / 1440;
      DateTimeParams.StandardDifferenceSec = TZI.StandardBias * 60;
      DateTimeParams.StandardDifference = double(TZI.StandardBias) / 1440;

      DateTimeParams.StandardDate = TZI.StandardDate;
      DateTimeParams.DaylightDate = TZI.DaylightDate;

      DateTimeParamsInitialized = true;
    }
  }
  return &DateTimeParams;
}
//---------------------------------------------------------------------------
static void __fastcall EncodeDSTMargin(const SYSTEMTIME & Date, unsigned short Year,
  TDateTime & Result)
{
  if (Date.wYear == 0)
  {
    TDateTime Temp = EncodeDate(Year, Date.wMonth, 1);
    Result = Temp + ((Date.wDayOfWeek - DayOfWeek(Temp) + 8) % 7) +
      (7 * (Date.wDay - 1));
    if (Date.wDay == 5)
    {
      unsigned short Month = static_cast<unsigned short>(Date.wMonth + 1);
      if (Month > 12)
      {
        Month = static_cast<unsigned short>(Month - 12);
        Year++;
      }

      if (Result > EncodeDate(Year, Month, 1))
      {
        Result -= 7;
      }
    }
    Result += EncodeTime(Date.wHour, Date.wMinute, Date.wSecond,
      Date.wMilliseconds);
  }
  else
  {
    Result = EncodeDate(Year, Date.wMonth, Date.wDay) +
      EncodeTime(Date.wHour, Date.wMinute, Date.wSecond, Date.wMilliseconds);
  }
}
//---------------------------------------------------------------------------
static bool __fastcall IsDateInDST(const TDateTime & DateTime)
{
  struct TDSTCache
  {
    bool Filled;
    unsigned short Year;
    TDateTime StandardDate;
    TDateTime DaylightDate;
  };
  static TDSTCache DSTCache[10];
  static int DSTCacheCount = 0;
  static TCriticalSection Section;

  TDateTimeParams * Params = GetDateTimeParams();
  bool Result;

  if (Params->StandardDate.wMonth == 0)
  {
    Result = false;
  }
  else
  {
    unsigned short Year, Month, Day;
    DecodeDate(DateTime, Year, Month, Day);

    TDSTCache * CurrentCache = &DSTCache[0];

    int CacheIndex = 0;
    while ((CacheIndex < DSTCacheCount) && (CacheIndex < LENOF(DSTCache)) &&
      CurrentCache->Filled && (CurrentCache->Year != Year))
    {
      CacheIndex++;
      CurrentCache++;
    }

    if ((CacheIndex < DSTCacheCount) && (CacheIndex < LENOF(DSTCache)) &&
        CurrentCache->Filled)
    {
      assert(CurrentCache->Year == Year);
      Result = (DateTime >= CurrentCache->DaylightDate) &&
        (DateTime < CurrentCache->StandardDate);
    }
    else
    {
      TDSTCache NewCache;

      EncodeDSTMargin(Params->StandardDate, Year, NewCache.StandardDate);
      EncodeDSTMargin(Params->DaylightDate, Year, NewCache.DaylightDate);
      if (DSTCacheCount < LENOF(DSTCache))
      {
        TGuard Guard(&Section);
        if (DSTCacheCount < LENOF(DSTCache))
        {
          NewCache.Year = Year;
          DSTCache[DSTCacheCount] = NewCache;
          DSTCache[DSTCacheCount].Filled = true;
          DSTCacheCount++;
        }
      }
      Result = (DateTime >= NewCache.DaylightDate) &&
        (DateTime < NewCache.StandardDate);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
TDateTime __fastcall UnixToDateTime(__int64 TimeStamp, TDSTMode DSTMode)
{
  TDateTimeParams * Params = GetDateTimeParams();

  TDateTime Result;
  Result = Params->UnixEpoch + (double(TimeStamp) / 86400);

  if ((DSTMode == dstmWin) || (DSTMode == dstmUnix))
  {
    Result -= Params->CurrentDifference;
  }
  else if (DSTMode == dstmKeep)
  {
    Result -= Params->BaseDifference;
  }

  if ((DSTMode == dstmUnix) || (DSTMode == dstmKeep))
  {
    Result -= (IsDateInDST(Result) ?
      Params->DaylightDifference : Params->StandardDifference);
  }

  return Result;
}
//---------------------------------------------------------------------------
inline __int64 __fastcall Round(double Number)
{
  double Floor = floor(Number);
  double Ceil = ceil(Number);
  return ((Number - Floor) > (Ceil - Number)) ? Ceil : Floor;
}
//---------------------------------------------------------------------------
#define TIME_POSIX_TO_WIN(t, ft) (*(LONGLONG*)&(ft) = \
    ((LONGLONG) (t) + (LONGLONG) 11644473600) * (LONGLONG) 10000000)
#define TIME_WIN_TO_POSIX(ft, t) ((t) = (__int64) \
    ((*(LONGLONG*)&(ft)) / (LONGLONG) 10000000 - (LONGLONG) 11644473600))
//---------------------------------------------------------------------------
static __int64 __fastcall DateTimeToUnix(const TDateTime DateTime)
{
  TDateTimeParams * Params = GetDateTimeParams();

  return Round(double(DateTime - Params->UnixEpoch) * 86400) +
    Params->CurrentDifferenceSec;
}
//---------------------------------------------------------------------------
FILETIME __fastcall DateTimeToFileTime(const TDateTime DateTime,
  TDSTMode /*DSTMode*/)
{
  FILETIME Result;
  __int64 UnixTimeStamp = DateTimeToUnix(DateTime);

  TIME_POSIX_TO_WIN(UnixTimeStamp, Result);
  return Result;
}
//---------------------------------------------------------------------------
__int64 __fastcall ConvertTimestampToUnix(const FILETIME & FileTime,
  TDSTMode DSTMode)
{
  __int64 Result;
  TIME_WIN_TO_POSIX(FileTime, Result);

  if ((DSTMode == dstmUnix) || (DSTMode == dstmKeep))
  {
    FILETIME LocalFileTime;
    SYSTEMTIME SystemTime;
    TDateTime DateTime;
    FileTimeToLocalFileTime(&FileTime, &LocalFileTime);
    FileTimeToSystemTime(&LocalFileTime, &SystemTime);
    DateTime = SystemTimeToDateTime(SystemTime);

    TDateTimeParams * Params = GetDateTimeParams();
    Result += (IsDateInDST(DateTime) ?
      Params->DaylightDifferenceSec : Params->StandardDifferenceSec);

    if (DSTMode == dstmKeep)
    {
      Result -= Params->CurrentDaylightDifferenceSec;
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
__int64 __fastcall ConvertTimestampToUnixSafe(const FILETIME & FileTime,
  TDSTMode DSTMode)
{
  __int64 Result;
  if ((FileTime.dwLowDateTime == 0) &&
      (FileTime.dwHighDateTime == 0))
  {
    Result = DateTimeToUnix(Now());
  }
  else
  {
    Result = ConvertTimestampToUnix(FileTime, DSTMode);
  }
  return Result;
}
//---------------------------------------------------------------------------
TDateTime __fastcall AdjustDateTimeFromUnix(TDateTime DateTime, TDSTMode DSTMode)
{
  TDateTimeParams * Params = GetDateTimeParams();

  if ((DSTMode == dstmWin) || (DSTMode == dstmUnix))
  {
    DateTime = DateTime - Params->CurrentDaylightDifference;
  }

  if (!IsDateInDST(DateTime))
  {
    if (DSTMode == dstmWin)
    {
      DateTime = DateTime - Params->DaylightDifference;
    }
  }
  else
  {
    DateTime = DateTime - Params->StandardDifference;
  }

  return DateTime;
}
//---------------------------------------------------------------------------
__inline static bool __fastcall UnifySignificance(unsigned short & V1,
  unsigned short & V2)
{
  bool Result = (V1 == 0) || (V2 == 0);
  if (Result)
  {
    V1 = 0;
    V2 = 0;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall UnifyDateTimePrecision(TDateTime & DateTime1, TDateTime & DateTime2)
{
  unsigned short Y1, M1, D1, H1, N1, S1, MS1;
  unsigned short Y2, M2, D2, H2, N2, S2, MS2;
  bool Changed;

  if (DateTime1 != DateTime2)
  {
    DateTime1.DecodeDate(&Y1, &M1, &D1);
    DateTime1.DecodeTime(&H1, &N1, &S1, &MS1);
    DateTime2.DecodeDate(&Y2, &M2, &D2);
    DateTime2.DecodeTime(&H2, &N2, &S2, &MS2);
    Changed = UnifySignificance(MS1, MS2);
    if (Changed && UnifySignificance(S1, S2) && UnifySignificance(N1, N2) &&
        UnifySignificance(H1, H2) && UnifySignificance(D1, D2) &&
        UnifySignificance(M1, M2))
    {
      UnifySignificance(Y1, Y2);
    }
    if (Changed)
    {
      DateTime1 = EncodeDate(Y1, M1, D1) + EncodeTime(H1, N1, S1, MS1);
      DateTime2 = EncodeDate(Y2, M2, D2) + EncodeTime(H2, N2, S2, MS2);
    }
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall FixedLenDateTimeFormat(const AnsiString & Format)
{
  AnsiString Result = Format;
  bool AsIs = false;

  int Index = 1;
  while (Index <= Result.Length())
  {
    char F = Result[Index];
    if ((F == '\'') || (F == '\"'))
    {
      AsIs = !AsIs;
      Index++;
    }
    else if (!AsIs && ((F == 'a') || (F == 'A')))
    {
      if (Result.SubString(Index, 5).LowerCase() == "am/pm")
      {
        Index += 5;
      }
      else if (Result.SubString(Index, 3).LowerCase() == "a/p")
      {
        Index += 3;
      }
      else if (Result.SubString(Index, 4).LowerCase() == "ampm")
      {
        Index += 4;
      }
      else
      {
        Index++;
      }
    }
    else
    {
      if (!AsIs && (strchr("dDeEmMhHnNsS", F) != NULL) &&
          ((Index == Result.Length()) || (Result[Index + 1] != F)))
      {
        Result.Insert(F, Index);
      }

      while ((Index <= Result.Length()) && (F == Result[Index]))
      {
        Index++;
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
int __fastcall CompareFileTime(TDateTime T1, TDateTime T2)
{
  // "FAT" time precision
  // 1 ms more solves the rounding issues (see also CustomDirView.pas)
  static TDateTime Second(0, 0, 1, 1);
  int Result;
  if (T1 == T2)
  {
    // just optimalisation
    Result = 0;
  }
  else if ((T1 < T2) && (T2 - T1 > Second))
  {
    Result = -1;
  }
  else if ((T1 > T2) && (T1 - T2 > Second))
  {
    Result = 1;
  }
  else
  {
    Result = 0;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall RecursiveDeleteFile(const AnsiString FileName, bool ToRecycleBin)
{
  SHFILEOPSTRUCT Data;

  memset(&Data, 0, sizeof(Data));
  Data.hwnd = NULL;
  Data.wFunc = FO_DELETE;
  AnsiString FileList(FileName);
  FileList.SetLength(FileList.Length() + 2);
  FileList[FileList.Length() - 1] = '\0';
  FileList[FileList.Length()] = '\0';
  Data.pFrom = FileList.c_str();
  Data.pTo = "";
  Data.fFlags = FOF_NOCONFIRMATION | FOF_RENAMEONCOLLISION | FOF_NOCONFIRMMKDIR |
    FOF_NOERRORUI | FOF_SILENT;
  if (ToRecycleBin)
  {
    Data.fFlags |= FOF_ALLOWUNDO;
  }
  int Result = SHFileOperation(&Data);
  return (Result == 0);
}
//---------------------------------------------------------------------------
int __fastcall CancelAnswer(int Answers)
{
  int Result;
  if ((Answers & qaCancel) != 0)
  {
    Result = qaCancel;
  }
  else if ((Answers & qaNo) != 0)
  {
    Result = qaNo;
  }
  else if ((Answers & qaAbort) != 0)
  {
    Result = qaAbort;
  }
  else if ((Answers & qaOK) != 0)
  {
    Result = qaOK;
  }
  else
  {
    assert(false);
    Result = qaCancel;
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall AbortAnswer(int Answers)
{
  int Result;
  if (FLAGSET(Answers, qaAbort))
  {
    Result = qaAbort;
  }
  else
  {
    Result = CancelAnswer(Answers);
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall ContinueAnswer(int Answers)
{
  int Result;
  if (FLAGSET(Answers, qaSkip))
  {
    Result = qaSkip;
  }
  else if (FLAGSET(Answers, qaIgnore))
  {
    Result = qaIgnore;
  }
  else if (FLAGSET(Answers, qaYes))
  {
    Result = qaYes;
  }
  else if (FLAGSET(Answers, qaOK))
  {
    Result = qaOK;
  }
  else if (FLAGSET(Answers, qaRetry))
  {
    Result = qaRetry;
  }
  else
  {
    Result = CancelAnswer(Answers);
  }
  return Result;
}
//---------------------------------------------------------------------------
TPasLibModule * __fastcall FindModule(void * Instance)
{
  TPasLibModule * CurModule;
  CurModule = reinterpret_cast<TPasLibModule*>(LibModuleList);

  while (CurModule)
  {
    if (CurModule->Instance == Instance)
    {
      break;
    }
    else
    {
      CurModule = CurModule->Next;
    }
  }
  return CurModule;
}
//---------------------------------------------------------------------------
AnsiString __fastcall LoadStr(int Ident, unsigned int MaxLength)
{
  TPasLibModule * MainModule = FindModule(HInstance);
  assert(MainModule != NULL);

  AnsiString Result;
  Result.SetLength(MaxLength);
  int Length = LoadString(MainModule->ResInstance, Ident, Result.c_str(), MaxLength);
  Result.SetLength(Length);

  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall LoadStrPart(int Ident, int Part)
{
  AnsiString Result;
  AnsiString Str = LoadStr(Ident);

  while (Part > 0)
  {
    Result = CutToChar(Str, '|', false);
    Part--;
  }
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall DecodeUrlChars(AnsiString S)
{
  int i = 1;
  while (i <= S.Length())
  {
    switch (S[i])
    {
      case '+':
        S[i] = ' ';
        break;

      case '%':
        if (i <= S.Length() - 2)
        {
          S[i] = HexToStr(S.SubString(i + 1, 2))[1];
          S.Delete(i + 1, 2);
        }
        break;
    }
    i++;
  }
  return S;
}
//---------------------------------------------------------------------------
AnsiString __fastcall EncodeUrlChars(AnsiString S, AnsiString Ignore)
{
  AnsiString Encode = "/ ";
  int i = 1;
  while (i <= S.Length())
  {
    if ((Encode.Pos(S[i]) > 0) &&
        (Ignore.Pos(S[i]) == 0))
    {
      AnsiString H = CharToHex(S[i]);
      S.Insert(H, i + 1);
      S[i] = '%';
      i += H.Length();
    }
    i++;
  }
  return S;
}
//---------------------------------------------------------------------------
void __fastcall OemToAnsi(AnsiString & Str)
{
  if (!Str.IsEmpty())
  {
    Str.Unique();
    OemToChar(Str.c_str(), Str.c_str());
  }
}
//---------------------------------------------------------------------------
void __fastcall AnsiToOem(AnsiString & Str)
{
  if (!Str.IsEmpty())
  {
    Str.Unique();
    CharToOem(Str.c_str(), Str.c_str());
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall EscapeHotkey(const AnsiString & Caption)
{
  return StringReplace(Caption, "&", "&&", TReplaceFlags() << rfReplaceAll);
}
//---------------------------------------------------------------------------
bool __fastcall CutToken(AnsiString & Str, AnsiString & Token)
{
  bool Result;

  Token = "";

  // inspired by Putty's sftp_getcmd() from PSFTP.C
  int Index = 1;
  while ((Index <= Str.Length()) &&
    ((Str[Index] == ' ') || (Str[Index] == '\t')))
  {
    Index++;
  }

  if (Index <= Str.Length())
  {
    bool Quoting = false;

    while (Index <= Str.Length())
    {
      if (!Quoting && ((Str[Index] == ' ') || (Str[Index] == '\t')))
      {
        break;
      }
      else if ((Str[Index] == '"') && (Index + 1 <= Str.Length()) &&
        (Str[Index + 1] == '"'))
      {
        Index += 2;
        Token += '"';
      }
      else if (Str[Index] == '"')
      {
        Index++;
        Quoting = !Quoting;
      }
      else
      {
        Token += Str[Index];
        Index++;
      }
    }

    if (Index <= Str.Length())
    {
      Index++;
    }

    Str = Str.SubString(Index, Str.Length());

    Result = true;
  }
  else
  {
    Result = false;
    Str = "";
  }

  return Result;
}
