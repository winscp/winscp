//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "Exceptions.h"
#include "TextsCore.h"
#include "Interface.h"
#include <math.h>
#include <shellapi.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#ifdef _DEBUG
#include <stdio.h>
void __fastcall Trace(const AnsiString SourceFile, const AnsiString Func,
  int Line, const AnsiString Message)
{
  const char * FileName = getenv(TRACEENV);
  // !!!
  if (FileName == NULL)
  {
    FileName = "c:\\winscptrace.log";
  }
  // !!!
  if (FileName != NULL)
  {
    FILE * File = fopen(FileName, "a");
    if (File != NULL)
    {
      fprintf(File, "[%s] %s:%d:%s\n  %s\n",
        Now().TimeString().c_str(),
        ExtractFileName(SourceFile).c_str(), Line, Func.c_str(), Message.c_str());

      fclose(File);
    }
  }
}
#endif // ifdef _DEBUG
//---------------------------------------------------------------------------
// TCriticalSection
//---------------------------------------------------------------------------
__fastcall TCriticalSection::TCriticalSection()
{
  InitializeCriticalSection(&FSection);
}
//---------------------------------------------------------------------------
__fastcall TCriticalSection::~TCriticalSection()
{
  DeleteCriticalSection(&FSection);
}
//---------------------------------------------------------------------------
void __fastcall TCriticalSection::Enter()
{
  EnterCriticalSection(&FSection);
}
//---------------------------------------------------------------------------
void __fastcall TCriticalSection::Leave()
{
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
    ReplaceChar(FileName, IllegalChars[Index+1], '-');
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
AnsiString __fastcall FormatCommand(AnsiString Program, AnsiString Params)
{
  Program = Program.Trim();
  Params = Params.Trim();
  if (!Params.IsEmpty()) Params = " " + Params;
  if (Program.Pos(" ")) Program = "\"" + Program + "\"";
  return Program + Params;
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
AnsiString __fastcall StrToHex(const AnsiString Str)
{
  AnsiString Result;
  for (int i = 1; i <= Str.Length(); i++)
  {
    Result += IntToHex(Str[i], 2);
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
      P1 = Digits.Pos(Hex[i]);
      P2 = Digits.Pos(Hex[i + 1]);
      if (P1 <= 0 || P2 <= 0)
      {
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
      AnsiString SD = FormatDateTime("dddddd tt", NewCache.StandardDate);
      AnsiString DD = FormatDateTime("dddddd tt", NewCache.DaylightDate);
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
TDateTime __fastcall UnixToDateTime(unsigned long TimeStamp, bool ConsiderDST)
{
  TDateTimeParams * Params = GetDateTimeParams();

  TDateTime Result;
  Result = Params->UnixEpoch + (double(TimeStamp) / 86400) - Params->CurrentDifference;

  if (ConsiderDST)
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
FILETIME __fastcall DateTimeToFileTime(const TDateTime DateTime,
  bool /*ConsiderDST*/)
{
  TDateTimeParams * Params = GetDateTimeParams();

  __int64 UnixTimeStamp;
  FILETIME Result;

  UnixTimeStamp = Round(double(DateTime - Params->UnixEpoch) * 86400) +
    Params->CurrentDifferenceSec;

  TIME_POSIX_TO_WIN(UnixTimeStamp, Result);
  return Result;
}
//---------------------------------------------------------------------------
unsigned long __fastcall ConvertTimestampToUnix(const FILETIME & FileTime,
  bool ConsiderDST)
{
  unsigned long Result;
  TIME_WIN_TO_POSIX(FileTime, Result);

  if (ConsiderDST)
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
  }

  return Result;
}
//---------------------------------------------------------------------------
TDateTime __fastcall AdjustDateTimeFromUnix(TDateTime DateTime, bool ConsiderDST)
{
  TDateTimeParams * Params = GetDateTimeParams();

  DateTime = DateTime - Params->CurrentDaylightDifference;

  bool IsInDST = IsDateInDST(DateTime);
  // shift always, even with ConsiderDST == false
  DateTime = DateTime - (!IsInDST ? Params->DaylightDifference :
    Params->StandardDifference);
  if (ConsiderDST)
  {
    if (IsInDST)
    {
      //DateTime = DateTime - Params->DaylightDifference;
    }
    else
    {
      DateTime = DateTime + Params->DaylightDifference;
    }
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

