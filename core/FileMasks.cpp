//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "FileMasks.h"

#include "Common.h"
#include "TextsCore.h"
#include "RemoteFiles.h"
#include "PuttyTools.h"
#include "Terminal.h"
//---------------------------------------------------------------------------
__fastcall EFileMasksException::EFileMasksException(
    AnsiString Message, int AErrorStart, int AErrorLen) :
  Exception(Message)
{
  ErrorStart = AErrorStart;
  ErrorLen = AErrorLen;
}
//---------------------------------------------------------------------------
AnsiString __fastcall MaskFilePart(const AnsiString Part, const AnsiString Mask, bool& Masked)
{
  AnsiString Result;
  int RestStart = 1;
  bool Delim = false;
  for (int Index = 1; Index <= Mask.Length(); Index++)
  {
    switch (Mask[Index])
    {
      case '\\':
        if (!Delim)
        {
          Delim = true;
          Masked = true;
          break;
        }

      case '*':
        if (!Delim)
        {
          Result += Part.SubString(RestStart, Part.Length() - RestStart + 1);
          RestStart = Part.Length() + 1;
          Masked = true;
          break;
        }

      case '?':
        if (!Delim)
        {
          if (RestStart <= Part.Length())
          {
            Result += Part[RestStart];
            RestStart++;
          }
          Masked = true;
          break;
        }

      default:
        Result += Mask[Index];
        RestStart++;
        Delim = false;
        break;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall MaskFileName(AnsiString FileName, const AnsiString Mask)
{
  if (!Mask.IsEmpty() && (Mask != "*") && (Mask != "*.*"))
  {
    bool Masked;
    int P = Mask.LastDelimiter(".");
    if (P > 0)
    {
      int P2 = FileName.LastDelimiter(".");
      // only dot at beginning of file name is not considered as
      // name/ext separator
      AnsiString FileExt = P2 > 1 ?
        FileName.SubString(P2 + 1, FileName.Length() - P2) : AnsiString();
      FileExt = MaskFilePart(FileExt, Mask.SubString(P + 1, Mask.Length() - P), Masked);
      if (P2 > 1)
      {
        FileName.SetLength(P2 - 1);
      }
      FileName = MaskFilePart(FileName, Mask.SubString(1, P - 1), Masked);
      if (!FileExt.IsEmpty())
      {
        FileName += "." + FileExt;
      }
    }
    else
    {
      FileName = MaskFilePart(FileName, Mask, Masked);
    }
  }
  return FileName;
}
//---------------------------------------------------------------------------
bool __fastcall IsFileNameMask(const AnsiString Mask)
{
  bool Masked = false;
  MaskFilePart("", Mask, Masked);
  return Masked;
}
//---------------------------------------------------------------------------
AnsiString __fastcall DelimitFileNameMask(AnsiString Mask)
{
  for (int i = 1; i <= Mask.Length(); i++)
  {
    if (strchr("\\*?", Mask[i]) != NULL)
    {
      Mask.Insert("\\", i);
      i++;
    }
  }
  return Mask;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TFileMasks::TParams::TParams() :
  Size(0)
{
}
//---------------------------------------------------------------------------
AnsiString TFileMasks::TParams::ToString() const
{
  return AnsiString("[") + IntToStr(Size) + "]";
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::IsMask(const AnsiString Mask)
{
  return (Mask.LastDelimiter("?*[") > 0);
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::IsAnyMask(const AnsiString & Mask)
{
  return Mask.IsEmpty() || (Mask == "*.*") || (Mask == "*");
}
//---------------------------------------------------------------------------
AnsiString __fastcall TFileMasks::NormalizeMask(const AnsiString & Mask, const AnsiString & AnyMask)
{
  if (IsAnyMask(Mask))
  {
    return AnyMask;
  }
  else
  {
    return Mask;
  }
}
//---------------------------------------------------------------------------
__fastcall TFileMasks::TFileMasks()
{
}
//---------------------------------------------------------------------------
__fastcall TFileMasks::TFileMasks(const TFileMasks & Source)
{
  SetStr(Source.Masks, false);
}
//---------------------------------------------------------------------------
__fastcall TFileMasks::TFileMasks(const AnsiString & AMasks)
{
  SetStr(AMasks, false);
}
//---------------------------------------------------------------------------
__fastcall TFileMasks::~TFileMasks()
{
  Clear();
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::Clear()
{
  Clear(FIncludeMasks);
  Clear(FExcludeMasks);
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::Clear(TMasks & Masks)
{
  TMasks::iterator I = Masks.begin();
  while (I != Masks.end())
  {
    ReleaseMaskMask((*I).FileNameMask);
    ReleaseMaskMask((*I).DirectoryMask);
    I++;
  }
  Masks.clear();
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::Negate()
{
  FStr = "";
  FIncludeMasks.swap(FExcludeMasks);
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::MatchesMasks(const AnsiString FileName, bool Directory,
  const AnsiString Path, const TParams * Params, const TMasks & Masks)
{
  bool Result = false;

  TMasks::const_iterator I = Masks.begin();
  while (!Result && (I != Masks.end()))
  {
    const TMask & Mask = *I;
    Result =
      (!Mask.DirectoryOnly || Directory) &&
      MatchesMaskMask(Mask.DirectoryMask, Path) &&
      MatchesMaskMask(Mask.FileNameMask, FileName);

    if (Result)
    {
      bool HasSize = !Directory && (Params != NULL);

      switch (Mask.HighSizeMask)
      {
        case TMask::None:
          Result = true;
          break;

        case TMask::Open:
          Result = HasSize && (Params->Size < Mask.HighSize);
          break;

        case TMask::Close:
          Result = HasSize && (Params->Size <= Mask.HighSize);
          break;
      }

      if (Result)
      {
        switch (Mask.LowSizeMask)
        {
          case TMask::None:
            Result = true;
            break;

          case TMask::Open:
            Result = HasSize && (Params->Size > Mask.LowSize);
            break;

          case TMask::Close:
            Result = HasSize && (Params->Size >= Mask.LowSize);
            break;
        }
      }
    }

    I++;
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::Matches(const AnsiString FileName, bool Directory,
  const AnsiString Path, const TParams * Params) const
{
  bool Result =
    (FIncludeMasks.empty() || MatchesMasks(FileName, Directory, Path, Params, FIncludeMasks)) &&
    !MatchesMasks(FileName, Directory, Path, Params, FExcludeMasks);
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::Matches(const AnsiString FileName, bool Local,
  bool Directory, const TParams * Params) const
{
  bool Result;
  if (Local)
  {
    AnsiString Path = ExtractFilePath(FileName);
    if (!Path.IsEmpty())
    {
      Path = ToUnixPath(ExcludeTrailingBackslash(Path));
    }
    Result = Matches(ExtractFileName(FileName), Directory, Path, Params);
  }
  else
  {
    Result = Matches(UnixExtractFileName(FileName), Directory,
      UnixExcludeTrailingBackslash(UnixExtractFilePath(FileName)), Params);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::operator ==(const TFileMasks & rhm) const
{
  return (Masks == rhm.Masks);
}
//---------------------------------------------------------------------------
TFileMasks & __fastcall TFileMasks::operator =(const AnsiString & rhs)
{
  Masks = rhs;
  return *this;
}
//---------------------------------------------------------------------------
TFileMasks & __fastcall TFileMasks::operator =(const TFileMasks & rhm)
{
  Masks = rhm.Masks;
  return *this;
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::operator ==(const AnsiString & rhs) const
{
  return (Masks == rhs);
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::ThrowError(int Start, int End)
{
  throw EFileMasksException(
    FMTLOAD(MASK_ERROR, (Masks.SubString(Start, End - Start + 1))),
    Start, End - Start + 1);
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::CreateMaskMask(const AnsiString & Mask, int Start, int End,
  bool Ex, TMaskMask & MaskMask)
{
  try
  {
    if (Ex && IsAnyMask(Mask))
    {
      MaskMask.Kind = TMaskMask::Any;
      MaskMask.Mask = NULL;
      assert(MaskMask.Mask == NULL);
    }
    else
    {
      MaskMask.Kind = (Ex && (Mask == "*.")) ? TMaskMask::NoExt : TMaskMask::Regular;
      MaskMask.Mask = new Masks::TMask(Mask);
    }
  }
  catch(...)
  {
    ThrowError(Start, End);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::ReleaseMaskMask(TMaskMask & MaskMask)
{
  delete MaskMask.Mask;
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::TrimEx(AnsiString & Str, int & Start, int & End)
{
  AnsiString Buf = TrimLeft(Str);
  Start += Str.Length() - Buf.Length();
  Str = TrimRight(Buf);
  End -= Buf.Length() - Str.Length();
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::MatchesMaskMask(const TMaskMask & MaskMask, const AnsiString & Str)
{
  bool Result;
  if (MaskMask.Kind == TMaskMask::Any)
  {
    Result = true;
  }
  else if ((MaskMask.Kind == TMaskMask::NoExt) && (Str.Pos(".") == 0))
  {
    Result = true;
  }
  else
  {
    Result = MaskMask.Mask->Matches(Str);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::SetMasks(const AnsiString value)
{
  if (FStr != value)
  {
    SetStr(value, false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::SetMask(const AnsiString & Mask)
{
  SetStr(Mask, true);
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::SetStr(const AnsiString Str, bool SingleMask)
{
  AnsiString Backup = FStr;
  try
  {
    FStr = Str;
    Clear();

    int NextMaskFrom = 1;
    bool Include = true;
    while (NextMaskFrom <= Str.Length())
    {
      int MaskStart = NextMaskFrom;
      char NextMaskDelimiter;
      AnsiString MaskStr;
      if (SingleMask)
      {
        MaskStr = Str;
        NextMaskFrom = Str.Length() + 1;
        NextMaskDelimiter = '\0';
      }
      else
      {
        MaskStr = CopyToChars(Str, NextMaskFrom, ";,|", false, &NextMaskDelimiter);
      }
      int MaskEnd = NextMaskFrom - 1;

      TrimEx(MaskStr, MaskStart, MaskEnd);

      if (!MaskStr.IsEmpty())
      {
        TMask Mask;
        Mask.Str = MaskStr;
        Mask.DirectoryOnly = false;
        Mask.FileNameMask.Kind = TMaskMask::Any;
        Mask.FileNameMask.Mask = NULL;
        Mask.DirectoryMask.Kind = TMaskMask::Any;
        Mask.DirectoryMask.Mask = NULL;
        Mask.HighSizeMask = TMask::None;
        Mask.LowSizeMask = TMask::None;

        char NextPartDelimiter = '\0';
        int NextPartFrom = 1;
        while (NextPartFrom <= MaskStr.Length())
        {
          char PartDelimiter = NextPartDelimiter;
          int PartFrom = NextPartFrom;
          AnsiString PartStr = CopyToChars(MaskStr, NextPartFrom, "<>", false, &NextPartDelimiter);

          int PartStart = MaskStart + PartFrom - 1;
          int PartEnd = MaskStart + NextPartFrom - 2;

          TrimEx(PartStr, PartStart, PartEnd);

          if (PartDelimiter != '\0')
          {
            bool Low = (PartDelimiter == '>');
            TMask::TSizeMask & SizeMask = (Low ? Mask.LowSizeMask : Mask.HighSizeMask);
            __int64 & Size = (Low ? Mask.LowSize : Mask.HighSize);

            bool Result = (SizeMask == TMask::None);

            if (!Result)
            {
              // include delimiter into size part
              ThrowError(PartStart - 1, PartEnd);
            }
            else
            {
              if ((PartStr.Length() >= 1) && (PartStr[1] == '='))
              {
                SizeMask = TMask::Close;
                PartStr.Delete(1, 1);
              }
              else
              {
                SizeMask = TMask::Open;
              }

              Size = ParseSize(PartStr);
            }
          }
          else if (!PartStr.IsEmpty())
          {
            int D = PartStr.LastDelimiter("\\/");

            Mask.DirectoryOnly = (D > 0) && (D == PartStr.Length());

            if (Mask.DirectoryOnly)
            {
              PartStr.SetLength(PartStr.Length() - 1);
              D = PartStr.LastDelimiter("\\/");
            }

            if (D > 0)
            {
              // make sure sole "/" (root dir) is preserved as is
              CreateMaskMask(
                UnixExcludeTrailingBackslash(ToUnixPath(PartStr.SubString(1, D))),
                PartStart, PartStart + D - 1, false,
                Mask.DirectoryMask);
              CreateMaskMask(
                PartStr.SubString(D + 1, PartStr.Length() - D),
                PartStart + D, PartEnd, true,
                Mask.FileNameMask);
            }
            else
            {
              CreateMaskMask(PartStr, PartStart, PartEnd, true, Mask.FileNameMask);
            }
          }
        }

        (Include ? FIncludeMasks : FExcludeMasks).push_back(Mask);
      }

      if (NextMaskDelimiter == '|')
      {
        if (Include)
        {
          Include = false;
        }
        else
        {
          ThrowError(NextMaskFrom - 1, Str.Length());
        }
      }
    }
  }
  catch(...)
  {
    // this does not work correctly if previous mask was set using SetMask.
    // this should not fail (the mask was validated before),
    // otherwise we end in an infinite loop
    SetStr(Backup, false);
    throw;
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#define TEXT_TOKEN '\255'
//---------------------------------------------------------------------------
const char TCustomCommand::NoQuote = '\0';
const AnsiString TCustomCommand::Quotes = "\"'";
//---------------------------------------------------------------------------
TCustomCommand::TCustomCommand()
{
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommand::GetToken(
  const AnsiString & Command, int Index, int & Len, char & PatternCmd)
{
  assert(Index <= Command.Length());
  const char * Ptr = Command.c_str() + Index - 1;

  if (Ptr[0] == '!')
  {
    PatternCmd = Ptr[1];
    if (PatternCmd == '!')
    {
      Len = 2;
    }
    else
    {
      Len = PatternLen(Index, PatternCmd);
    }

    if (Len < 0)
    {
      throw Exception(FMTLOAD(CUSTOM_COMMAND_UNKNOWN, (PatternCmd, Index)));
    }
    else if (Len > 0)
    {
      if ((Command.Length() - Index + 1) < Len)
      {
        throw Exception(FMTLOAD(CUSTOM_COMMAND_UNTERMINATED, (PatternCmd, Index)));
      }
    }
    else if (Len == 0)
    {
      const char * PatternEnd = strchr(Ptr + 1, '!');
      if (PatternEnd == NULL)
      {
        throw Exception(FMTLOAD(CUSTOM_COMMAND_UNTERMINATED, (PatternCmd, Index)));
      }
      Len = PatternEnd - Ptr + 1;
    }
  }
  else
  {
    PatternCmd = TEXT_TOKEN;
    const char * NextPattern = strchr(Ptr, '!');
    if (NextPattern == NULL)
    {
      Len = Command.Length() - Index + 1;
    }
    else
    {
      Len = NextPattern - Ptr;
    }
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCustomCommand::Complete(const AnsiString & Command,
  bool LastPass)
{
  AnsiString Result;
  int Index = 1;

  while (Index <= Command.Length())
  {
    int Len;
    char PatternCmd;
    GetToken(Command, Index, Len, PatternCmd);

    if (PatternCmd == TEXT_TOKEN)
    {
      Result += Command.SubString(Index, Len);
    }
    else if (PatternCmd == '!')
    {
      if (LastPass)
      {
        Result += '!';
      }
      else
      {
        Result += Command.SubString(Index, Len);
      }
    }
    else
    {
      char Quote = NoQuote;
      if ((Index > 1) && (Index + Len - 1 < Command.Length()) &&
          Command.IsDelimiter(Quotes, Index - 1) &&
          Command.IsDelimiter(Quotes, Index + Len) &&
          (Command[Index - 1] == Command[Index + Len]))
      {
        Quote = Command[Index - 1];
      }
      AnsiString Pattern = Command.SubString(Index, Len);
      AnsiString Replacement;
      bool Delimit = true;
      if (PatternReplacement(Index, Pattern, Replacement, Delimit))
      {
        if (!LastPass)
        {
          Replacement = StringReplace(Replacement, "!", "!!",
            TReplaceFlags() << rfReplaceAll);
        }
        if (Delimit)
        {
          DelimitReplacement(Replacement, Quote);
        }
        Result += Replacement;
      }
      else
      {
        Result += Pattern;
      }
    }

    Index += Len;
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommand::DelimitReplacement(AnsiString & Replacement, char Quote)
{
  Replacement = ShellDelimitStr(Replacement, Quote);
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommand::Validate(const AnsiString & Command)
{
  CustomValidate(Command, NULL);
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommand::CustomValidate(const AnsiString & Command,
  void * Arg)
{
  int Index = 1;

  while (Index <= Command.Length())
  {
    int Len;
    char PatternCmd;
    GetToken(Command, Index, Len, PatternCmd);
    ValidatePattern(Command, Index, Len, PatternCmd, Arg);

    Index += Len;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomCommand::FindPattern(const AnsiString & Command,
  char PatternCmd)
{
  bool Result = false;
  int Index = 1;

  while (!Result && (Index <= Command.Length()))
  {
    int Len;
    char APatternCmd;
    GetToken(Command, Index, Len, APatternCmd);
    if (((PatternCmd != '!') && (PatternCmd == APatternCmd)) ||
        ((PatternCmd == '!') && (Len == 1) && (APatternCmd != TEXT_TOKEN)))
    {
      Result = true;
    }

    Index += Len;
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommand::ValidatePattern(const AnsiString & /*Command*/,
  int /*Index*/, int /*Len*/, char /*PatternCmd*/, void * /*Arg*/)
{
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TInteractiveCustomCommand::TInteractiveCustomCommand(
  TCustomCommand * ChildCustomCommand)
{
  FChildCustomCommand = ChildCustomCommand;
}
//---------------------------------------------------------------------------
void __fastcall TInteractiveCustomCommand::Prompt(int /*Index*/,
  const AnsiString & /*Prompt*/, AnsiString & Value)
{
  Value = "";
}
//---------------------------------------------------------------------------
int __fastcall TInteractiveCustomCommand::PatternLen(int Index, char PatternCmd)
{
  int Len;
  switch (PatternCmd)
  {
    case '?':
      Len = 0;
      break;

    default:
      Len = FChildCustomCommand->PatternLen(Index, PatternCmd);
      break;
  }
  return Len;
}
//---------------------------------------------------------------------------
bool __fastcall TInteractiveCustomCommand::PatternReplacement(int Index, const AnsiString & Pattern,
  AnsiString & Replacement, bool & Delimit)
{
  bool Result;
  if ((Pattern.Length() >= 3) && (Pattern[2] == '?'))
  {
    AnsiString PromptStr;
    int Pos = Pattern.SubString(3, Pattern.Length() - 2).Pos("?");
    if (Pos > 0)
    {
      Replacement = Pattern.SubString(3 + Pos, Pattern.Length() - 3 - Pos);
      if ((Pos > 1) && (Pattern[3 + Pos - 2] == '\\'))
      {
        Delimit = false;
        Pos--;
      }
      PromptStr = Pattern.SubString(3, Pos - 1);
    }
    else
    {
      PromptStr = Pattern.SubString(3, Pattern.Length() - 3);
    }

    Prompt(Index, PromptStr, Replacement);

    Result = true;
  }
  else
  {
    Result = false;
  }

  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TCustomCommandData::TCustomCommandData()
{
}
//---------------------------------------------------------------------------
__fastcall TCustomCommandData::TCustomCommandData(TTerminal * Terminal)
{
  HostName = Terminal->SessionData->HostName;
  UserName = Terminal->SessionData->UserName;
  Password = Terminal->Password;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TFileCustomCommand::TFileCustomCommand()
{
}
//---------------------------------------------------------------------------
TFileCustomCommand::TFileCustomCommand(const TCustomCommandData & Data,
  const AnsiString & Path)
{
  FData = Data;
  FPath = Path;
}
//---------------------------------------------------------------------------
TFileCustomCommand::TFileCustomCommand(const TCustomCommandData & Data,
    const AnsiString & Path, const AnsiString & FileName,
    const AnsiString & FileList) :
  TCustomCommand()
{
  FData = Data;
  FPath = Path;
  FFileName = FileName;
  FFileList = FileList;
}
//---------------------------------------------------------------------------
int __fastcall TFileCustomCommand::PatternLen(int /*Index*/, char PatternCmd)
{
  int Len;
  switch (toupper(PatternCmd))
  {
    case '@':
    case 'U':
    case 'P':
    case '/':
    case '&':
      Len = 2;
      break;

    default:
      Len = 1;
      break;
  }
  return Len;
}
//---------------------------------------------------------------------------
bool __fastcall TFileCustomCommand::PatternReplacement(int /*Index*/,
  const AnsiString & Pattern, AnsiString & Replacement, bool & Delimit)
{
  // keep consistent with TSessionLog::OpenLogFile

  if (Pattern == "!@")
  {
    Replacement = FData.HostName;
  }
  else if (AnsiSameText(Pattern, "!u"))
  {
    Replacement = FData.UserName;
  }
  else if (AnsiSameText(Pattern, "!p"))
  {
    Replacement = FData.Password;
  }
  else if (Pattern == "!/")
  {
    Replacement = UnixIncludeTrailingBackslash(FPath);
  }
  else if (Pattern == "!&")
  {
    Replacement = FFileList;
    // already delimited
    Delimit = false;
  }
  else
  {
    assert(Pattern.Length() == 1);
    Replacement = FFileName;
  }

  return true;
}
//---------------------------------------------------------------------------
void __fastcall TFileCustomCommand::Validate(const AnsiString & Command)
{
  int Found[2] = { 0, 0 };
  CustomValidate(Command, &Found);
  if ((Found[0] > 0) && (Found[1] > 0))
  {
    throw Exception(FMTLOAD(CUSTOM_COMMAND_FILELIST_ERROR,
      (Found[1], Found[0])));
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileCustomCommand::ValidatePattern(const AnsiString & /*Command*/,
  int Index, int /*Len*/, char PatternCmd, void * Arg)
{
  int * Found = static_cast<int *>(Arg);

  assert(Index > 0);

  if (PatternCmd == '&')
  {
    Found[0] = Index;
  }
  else if ((PatternCmd != TEXT_TOKEN) && (PatternLen(Index, PatternCmd) == 1))
  {
    Found[1] = Index;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFileCustomCommand::IsFileListCommand(const AnsiString & Command)
{
  return FindPattern(Command, '&');
}
//---------------------------------------------------------------------------
bool __fastcall TFileCustomCommand::IsFileCommand(const AnsiString & Command)
{
  return FindPattern(Command, '!') || FindPattern(Command, '&');
}
//---------------------------------------------------------------------------
