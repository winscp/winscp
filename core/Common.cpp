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
  AnsiString IllegalChars = ":;,=+<>|\"[] \\/?*";
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
  if (B)
  {
    return "Yes";
  }
  else
  {
    return "No";
  }
}
//---------------------------------------------------------------------------
AnsiString BooleanToStr(bool B)
{
  if (B)
  {
    return LoadStr(YES_STR);
  }
  else
  {
    return LoadStr(NO_STR);
  }
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
AnsiString CopyToChars(const AnsiString & Str, int & From, AnsiString Chs, bool Trim,
  char * Delimiter)
{
  int P;
  for (P = From; P <= Str.Length(); P++)
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
    Result = Str.SubString(From, P-From);
    From = P+1;
  }
  else
  {
    if (Delimiter != NULL)
    {
      *Delimiter = '\0';
    }
    Result = Str.SubString(From, Str.Length() - From + 1);
    From = P;
  }
  if (Trim)
  {
    Result = Result.TrimRight();
    while ((P <= Str.Length()) && (Str[P] == ' '))
    {
      P++;
    }
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
bool IsNumber(const AnsiString Str)
{
  int Value;
  return TryStrToInt(Str, Value);
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
  if (!Command.IsEmpty())
  {
    AnsiString Program, Params, Dir;
    SplitCommand(Command, Program, Params, Dir);
    if (Params.Pos(ShellCommandFileNamePattern) == 0)
    {
      Params = Params + (Params.IsEmpty() ? "" : " ") + ShellCommandFileNamePattern;
    }
    Command = FormatCommand(Program, Params);
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall ExpandFileNameCommand(const AnsiString Command,
  const AnsiString FileName)
{
  return AnsiReplaceStr(Command, ShellCommandFileNamePattern,
    AddPathQuotes(FileName));
}
//---------------------------------------------------------------------------
AnsiString __fastcall EscapePuttyCommandParam(AnsiString Param)
{
  bool Space = false;

  for (int i = 1; i <= Param.Length(); i++)
  {
    switch (Param[i])
    {
      case '"':
        Param.Insert("\\", i);
        i++;
        break;

      case ' ':
        Space = true;
        break;

      case '\\':
        int i2 = i;
        while ((i2 <= Param.Length()) && (Param[i2] == '\\'))
        {
          i2++;
        }
        if ((i2 <= Param.Length()) && (Param[i2] == '"'))
        {
          while (Param[i] == '\\')
          {
            Param.Insert("\\", i);
            i += 2;
          }
          i--;
        }
        break;
    }
  }

  if (Space)
  {
    Param = "\"" + Param + '"';
  }

  return Param;
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
    Result = AnsiSameText(ShortPath1, ShortPath2);
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
bool __fastcall IsReservedName(AnsiString FileName)
{
  int P = FileName.Pos(".");
  int Len = (P > 0) ? P - 1 : FileName.Length();
  if ((Len == 3) || (Len == 4))
  {
    if (P > 0)
    {
      FileName.SetLength(P - 1);
    }
    static AnsiString Reserved[] = {
      "CON", "PRN", "AUX", "NUL",
      "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9",
      "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9" };
    for (int Index = 0; Index < LENOF(Reserved); Index++)
    {
      if (SameText(FileName, Reserved[Index]))
      {
        return true;
      }
    }
  }
  return false;
}
//---------------------------------------------------------------------------
AnsiString __fastcall DisplayableStr(const AnsiString Str)
{
  bool Displayable = true;
  int Index = 1;
  while ((Index <= Str.Length()) && Displayable)
  {
    if ((Str[Index] < '\32') &&
        (Str[Index] != '\n') && (Str[Index] != '\r') && (Str[Index] != '\t') && (Str[Index] != '\b'))
    {
      Displayable = false;
    }
    Index++;
  }

  AnsiString Result;
  if (Displayable)
  {
    Result = "\"";
    for (int Index = 1; Index <= Str.Length(); Index++)
    {
      switch (Str[Index])
      {
        case '\n':
          Result += "\\n";
          break;

        case '\r':
          Result += "\\r";
          break;

        case '\t':
          Result += "\\t";
          break;

        case '\b':
          Result += "\\b";
          break;

        case '\\':
          Result += "\\\\";
          break;

        case '"':
          Result += "\\\"";
          break;

        default:
          Result += Str[Index];
          break;
      }
    }
    Result += "\"";
  }
  else
  {
    Result = "0x" + StrToHex(Str);
  }
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall CharToHex(char Ch, bool UpperCase)
{
  static char UpperDigits[] = "0123456789ABCDEF";
  static char LowerDigits[] = "0123456789abcdef";

  const char * Digits = (UpperCase ? UpperDigits : LowerDigits);
  AnsiString Result;
  Result.SetLength(2);
  Result[1] = Digits[((unsigned char)Ch & 0xF0) >> 4];
  Result[2] = Digits[ (unsigned char)Ch & 0x0F];
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall StrToHex(const AnsiString Str, bool UpperCase, char Separator)
{
  AnsiString Result;
  for (int i = 1; i <= Str.Length(); i++)
  {
    Result += CharToHex(Str[i], UpperCase);
    if ((Separator != '\0') && (i < Str.Length()))
    {
      Result += Separator;
    }
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
char __fastcall HexToChar(const AnsiString Hex, int MinChars)
{
  return (char)HexToInt(Hex, MinChars);
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
TDateTime __fastcall EncodeDateVerbose(Word Year, Word Month, Word Day)
{
  try
  {
    return EncodeDate(Year, Month, Day);
  }
  catch (EConvertError & E)
  {
    throw EConvertError(FORMAT("%s [%d-%d-%d]", (E.Message, int(Year), int(Month), int(Day))));
  }
}
//---------------------------------------------------------------------------
TDateTime __fastcall EncodeTimeVerbose(Word Hour, Word Min, Word Sec, Word MSec)
{
  try
  {
    return EncodeTime(Hour, Min, Sec, MSec);
  }
  catch (EConvertError & E)
  {
    throw EConvertError(FORMAT("%s [%d:%d:%d.%d]", (E.Message, int(Hour), int(Min), int(Sec), int(MSec))));
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
  bool DaylightHack;
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
      DateTimeParams.UnixEpoch = EncodeDateVerbose(1970, 1, 1);

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

      DateTimeParams.DaylightHack = !IsWin7() || IsExactly2008R2();

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
    TDateTime Temp = EncodeDateVerbose(Year, Date.wMonth, 1);
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

      if (Result >= EncodeDateVerbose(Year, Month, 1))
      {
        Result -= 7;
      }
    }
    Result += EncodeTimeVerbose(Date.wHour, Date.wMinute, Date.wSecond,
      Date.wMilliseconds);
  }
  else
  {
    Result = EncodeDateVerbose(Year, Date.wMonth, Date.wDay) +
      EncodeTimeVerbose(Date.wHour, Date.wMinute, Date.wSecond, Date.wMilliseconds);
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
    bool SummerDST;
  };
  static TDSTCache DSTCache[10];
  static int DSTCacheCount = 0;
  static TCriticalSection Section;

  TDateTimeParams * Params = GetDateTimeParams();
  bool Result;

  // On some systems it occurs that StandardDate is unset, while
  // DaylightDate is set. MSDN states that this is invalid and
  // should be treated as if there is no daylinght saving.
  // So check both.
  if ((Params->StandardDate.wMonth == 0) ||
      (Params->DaylightDate.wMonth == 0))
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

    TDSTCache NewCache;
    if ((CacheIndex < DSTCacheCount) && (CacheIndex < LENOF(DSTCache)) &&
        CurrentCache->Filled)
    {
      assert(CurrentCache->Year == Year);
    }
    else
    {

      EncodeDSTMargin(Params->StandardDate, Year, NewCache.StandardDate);
      EncodeDSTMargin(Params->DaylightDate, Year, NewCache.DaylightDate);
      NewCache.SummerDST = (NewCache.DaylightDate < NewCache.StandardDate);
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
      CurrentCache = &NewCache;
    }

    if (CurrentCache->SummerDST)
    {
      Result =
        (DateTime >= CurrentCache->DaylightDate) &&
        (DateTime < CurrentCache->StandardDate);
    }
    else
    {
      Result =
        (DateTime < CurrentCache->StandardDate) ||
        (DateTime >= CurrentCache->DaylightDate);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall UsesDaylightHack()
{
  return GetDateTimeParams()->DaylightHack;
}
//---------------------------------------------------------------------------
TDateTime __fastcall UnixToDateTime(__int64 TimeStamp, TDSTMode DSTMode)
{
  TDateTimeParams * Params = GetDateTimeParams();

  TDateTime Result;
  Result = Params->UnixEpoch + (double(TimeStamp) / 86400);

  if (Params->DaylightHack)
  {
    if ((DSTMode == dstmWin) || (DSTMode == dstmUnix))
    {
      Result -= Params->CurrentDifference;
    }
    else if (DSTMode == dstmKeep)
    {
      Result -= Params->BaseDifference;
    }
  }
  else
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
__int64 __fastcall Round(double Number)
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

  TDateTimeParams * Params = GetDateTimeParams();
  if (!Params->DaylightHack)
  {
    UnixTimeStamp += (IsDateInDST(DateTime) ?
      Params->DaylightDifferenceSec : Params->StandardDifferenceSec);
    UnixTimeStamp -= Params->CurrentDaylightDifferenceSec;
  }

  TIME_POSIX_TO_WIN(UnixTimeStamp, Result);

  return Result;
}
//---------------------------------------------------------------------------
TDateTime __fastcall FileTimeToDateTime(const FILETIME & FileTime)
{
  // duplicated in DirView.pas
  SYSTEMTIME SysTime;
  TDateTimeParams * Params = GetDateTimeParams();
  if (!Params->DaylightHack)
  {
    SYSTEMTIME UniverzalSysTime;
    FileTimeToSystemTime(&FileTime, &UniverzalSysTime);
    SystemTimeToTzSpecificLocalTime(NULL, &UniverzalSysTime, &SysTime);
  }
  else
  {
    FILETIME LocalFileTime;
    FileTimeToLocalFileTime(&FileTime, &LocalFileTime);
    FileTimeToSystemTime(&LocalFileTime, &SysTime);
  }
  TDateTime Result = SystemTimeToDateTime(SysTime);
  return Result;
}
//---------------------------------------------------------------------------
__int64 __fastcall ConvertTimestampToUnix(const FILETIME & FileTime,
  TDSTMode DSTMode)
{
  __int64 Result;
  TIME_WIN_TO_POSIX(FileTime, Result);

  TDateTimeParams * Params = GetDateTimeParams();
  if (Params->DaylightHack)
  {
    if ((DSTMode == dstmUnix) || (DSTMode == dstmKeep))
    {
      FILETIME LocalFileTime;
      SYSTEMTIME SystemTime;
      FileTimeToLocalFileTime(&FileTime, &LocalFileTime);
      FileTimeToSystemTime(&LocalFileTime, &SystemTime);
      TDateTime DateTime = SystemTimeToDateTime(SystemTime);
      Result += (IsDateInDST(DateTime) ?
        Params->DaylightDifferenceSec : Params->StandardDifferenceSec);

      if (DSTMode == dstmKeep)
      {
        Result -= Params->CurrentDaylightDifferenceSec;
      }
    }
  }
  else
  {
    if (DSTMode == dstmWin)
    {
      FILETIME LocalFileTime;
      SYSTEMTIME SystemTime;
      FileTimeToLocalFileTime(&FileTime, &LocalFileTime);
      FileTimeToSystemTime(&LocalFileTime, &SystemTime);
      TDateTime DateTime = SystemTimeToDateTime(SystemTime);
      Result -= (IsDateInDST(DateTime) ?
        Params->DaylightDifferenceSec : Params->StandardDifferenceSec);
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
TDateTime __fastcall ConvertTimestampToUTC(TDateTime DateTime)
{

  TDateTimeParams * Params = GetDateTimeParams();
  DateTime +=
    (IsDateInDST(DateTime) ?
      Params->DaylightDifference : Params->StandardDifference);
  DateTime += Params->BaseDifference;

  if (Params->DaylightHack)
  {
    const TDateTimeParams * CurrentParams = GetDateTimeParams();
    DateTime += CurrentParams->CurrentDaylightDifference;
  }

  return DateTime;
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

  if (Params->DaylightHack)
  {
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
  }
  else
  {
    if (DSTMode == dstmWin)
    {
      if (IsDateInDST(DateTime))
      {
        DateTime = DateTime + Params->DaylightDifference;
      }
      else
      {
        DateTime = DateTime + Params->StandardDifference;
      }
    }
  }

  return DateTime;
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
  // (when one time is seconds-precision and other is millisecond-precision,
  // we may have times like 12:00:00.000 and 12:00:01.999, which should
  // be treated the same)
  static TDateTime TwoSeconds(0, 0, 2, 0);
  int Result;
  if (T1 == T2)
  {
    // just optimalisation
    Result = 0;
  }
  else if ((T1 < T2) && (T2 - T1 >= TwoSeconds))
  {
    Result = -1;
  }
  else if ((T1 > T2) && (T1 - T2 >= TwoSeconds))
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
  int ErrorCode = SHFileOperation(&Data);
  bool Result = (ErrorCode == 0);
  if (!Result)
  {
    // according to MSDN, SHFileOperation may return following non-Win32
    // error codes
    if (((ErrorCode >= 0x71) && (ErrorCode <= 0x88)) ||
        (ErrorCode == 0xB7) || (ErrorCode == 0x402) || (ErrorCode == 0x10000) ||
        (ErrorCode == 0x10074))
    {
      ErrorCode = 0;
    }
    SetLastError(ErrorCode);
  }
  return Result;
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
          AnsiString C = HexToStr(S.SubString(i + 1, 2));
          if (C.Length() == 1)
          {
            S[i] = C[1];
            S.Delete(i + 1, 2);
          }
        }
        break;
    }
    i++;
  }
  return S;
}
//---------------------------------------------------------------------------
AnsiString __fastcall DoEncodeUrl(AnsiString S, AnsiString Chars)
{
  int i = 1;
  while (i <= S.Length())
  {
    if (Chars.Pos(S[i]) > 0)
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
AnsiString __fastcall EncodeUrlChars(AnsiString S, AnsiString Ignore)
{
  AnsiString Chars;
  if (Ignore.Pos(' ') == 0)
  {
    Chars += ' ';
  }
  if (Ignore.Pos('/') == 0)
  {
    Chars += '/';
  }
  return DoEncodeUrl(S, Chars);
}
//---------------------------------------------------------------------------
AnsiString __fastcall NonUrlChars()
{
  AnsiString S;
  for (unsigned int I = 0; I <= 255; I++)
  {
    char C = static_cast<char>(I);
    if (((C >= 'a') && (C <= 'z')) ||
        ((C >= 'A') && (C <= 'Z')) ||
        ((C >= '0') && (C <= '9')) ||
        (C == '_') || (C == '-') || (C == '.'))
    {
      // noop
    }
    else
    {
      S += C;
    }
  }
  return S;
}
//---------------------------------------------------------------------------
AnsiString __fastcall EncodeUrlString(AnsiString S)
{
  return DoEncodeUrl(S, NonUrlChars());
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
// duplicated in console's Main.cpp
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
//---------------------------------------------------------------------------
void __fastcall AddToList(AnsiString & List, const AnsiString & Value, char Delimiter)
{
  if (!List.IsEmpty() && (List[List.Length()] != Delimiter))
  {
    List += Delimiter;
  }
  List += Value;
}
//---------------------------------------------------------------------------
bool __fastcall Is2000()
{
  return (Win32MajorVersion >= 5);
}
//---------------------------------------------------------------------------
bool __fastcall IsWin7()
{
  return
    (Win32MajorVersion > 6) ||
    ((Win32MajorVersion == 6) && (Win32MinorVersion >= 1));
}
//---------------------------------------------------------------------------
bool __fastcall IsExactly2008R2()
{
  HANDLE Kernel32 = GetModuleHandle(kernel32);
  typedef BOOL WINAPI (* TGetProductInfo)(DWORD, DWORD, DWORD, DWORD, PDWORD);
  TGetProductInfo GetProductInfo =
      (TGetProductInfo)GetProcAddress(Kernel32, "GetProductInfo");
  bool Result;
  if (GetProductInfo == NULL)
  {
    Result = false;
  }
  else
  {
    DWORD Type;
    GetProductInfo(Win32MajorVersion, Win32MinorVersion, 0, 0, &Type);
    switch (Type)
    {
      case 0x0008 /*PRODUCT_DATACENTER_SERVER*/:
      case 0x000C /*PRODUCT_DATACENTER_SERVER_CORE}*/:
      case 0x0027 /*PRODUCT_DATACENTER_SERVER_CORE_V*/:
      case 0x0025 /*PRODUCT_DATACENTER_SERVER_V*/:
      case 0x000A /*PRODUCT_ENTERPRISE_SERVE*/:
      case 0x000E /*PRODUCT_ENTERPRISE_SERVER_COR*/:
      case 0x0029 /*PRODUCT_ENTERPRISE_SERVER_CORE_*/:
      case 0x000F /*PRODUCT_ENTERPRISE_SERVER_IA6*/:
      case 0x0026 /*PRODUCT_ENTERPRISE_SERVER_*/:
      case 0x002A /*PRODUCT_HYPER*/:
      case 0x001E /*PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMEN*/:
      case 0x0020 /*PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGIN*/:
      case 0x001F /*PRODUCT_MEDIUMBUSINESS_SERVER_SECURIT*/:
      case 0x0018 /*PRODUCT_SERVER_FOR_SMALLBUSINES*/:
      case 0x0023 /*PRODUCT_SERVER_FOR_SMALLBUSINESS_*/:
      case 0x0021 /*PRODUCT_SERVER_FOUNDATIO*/:
      case 0x0009 /*PRODUCT_SMALLBUSINESS_SERVE*/:
      case 0x0038 /*PRODUCT_SOLUTION_EMBEDDEDSERVE*/:
      case 0x0007 /*PRODUCT_STANDARD_SERVE*/:
      case 0x000D /*PRODUCT_STANDARD_SERVER_COR*/:
      case 0x0028 /*PRODUCT_STANDARD_SERVER_CORE_*/:
      case 0x0024 /*PRODUCT_STANDARD_SERVER_*/:
      case 0x0017 /*PRODUCT_STORAGE_ENTERPRISE_SERVE*/:
      case 0x0014 /*PRODUCT_STORAGE_EXPRESS_SERVE*/:
      case 0x0015 /*PRODUCT_STORAGE_STANDARD_SERVE*/:
      case 0x0016 /*PRODUCT_STORAGE_WORKGROUP_SERVE*/:
      case 0x0011 /*PRODUCT_WEB_SERVE*/:
      case 0x001D /*PRODUCT_WEB_SERVER_COR*/:
        Result = true;
        break;

      default:
        Result = false;
        break;
    }
  }
  return Result;
}
