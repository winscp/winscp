//---------------------------------------------------------------------------
#define NO_WIN32_LEAN_AND_MEAN
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "Exceptions.h"
#include "TextsCore.h"
#include "Interface.h"
#include <StrUtils.hpp>
#include <DateUtils.hpp>
#include <System.IOUtils.hpp>
#include <math.h>
#include <shlobj.h>
#include <limits>
#include <algorithm>
#include <memory>
#include <shlwapi.h>
#include <tlhelp32.h>
#include <psapi.h>
#include <CoreMain.h>
#include <SessionInfo.h>
#include <Soap.EncdDecd.hpp>
#include <openssl/pkcs12.h>
#include <openssl/pem.h>
#include <openssl/err.h>
#include <openssl/ssl.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
const wchar_t * DSTModeNames = L"Win;Unix;Keep";
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
const UnicodeString AnyMask = L"*.*";
const wchar_t EngShortMonthNames[12][4] =
  {L"Jan", L"Feb", L"Mar", L"Apr", L"May", L"Jun",
   L"Jul", L"Aug", L"Sep", L"Oct", L"Nov", L"Dec"};
const char Bom[4] = "\xEF\xBB\xBF";
const UnicodeString XmlDeclaration(TraceInitStr(L"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"));
const wchar_t TokenPrefix = L'%';
const wchar_t NoReplacement = wchar_t(false);
const wchar_t TokenReplacement = wchar_t(true);
// Note similar list in MakeValidFileName
const UnicodeString LocalInvalidChars(TraceInitStr(L"/\\:*?\"<>|"));
const UnicodeString PasswordMask(TraceInitStr(L"***"));
const UnicodeString Ellipsis(TraceInitStr(L"..."));
const UnicodeString TitleSeparator(TraceInitStr(L" \u2013 ")); // En-Dash
const UnicodeString OfficialPackage(TraceInitStr(L"MartinPikryl.WinSCP_tvv458r3h9r5m"));
//---------------------------------------------------------------------------
UnicodeString ReplaceChar(UnicodeString Str, wchar_t A, wchar_t B)
{
  for (Integer Index = 0; Index < Str.Length(); Index++)
    if (Str[Index+1] == A) Str[Index+1] = B;
  return Str;
}
//---------------------------------------------------------------------------
UnicodeString DeleteChar(UnicodeString Str, wchar_t C)
{
  int P;
  while ((P = Str.Pos(C)) > 0)
  {
    Str.Delete(P, 1);
  }
  return Str;
}
//---------------------------------------------------------------------------
template<typename T>
void DoPackStr(T & Str)
{
  // Following will free unnecessary bytes
  Str = Str.c_str();
}
//---------------------------------------------------------------------------
void PackStr(UnicodeString & Str)
{
  DoPackStr(Str);
}
//---------------------------------------------------------------------------
void PackStr(RawByteString & Str)
{
  DoPackStr(Str);
}
//---------------------------------------------------------------------------
void PackStr(AnsiString & Str)
{
  DoPackStr(Str);
}
//---------------------------------------------------------------------------
template<typename T>
void __fastcall DoShred(T & Str)
{
  if (!Str.IsEmpty())
  {
    // Should instead test for (StringRefCount(Str) == 1) to prevent Unique making yet another copy
    Str.Unique();
    memset(Str.c_str(), 0, Str.Length() * sizeof(*Str.c_str()));
    Str = L"";
  }
}
//---------------------------------------------------------------------------
void __fastcall Shred(UnicodeString & Str)
{
  DoShred(Str);
}
//---------------------------------------------------------------------------
void __fastcall Shred(UTF8String & Str)
{
  DoShred(Str);
}
//---------------------------------------------------------------------------
void __fastcall Shred(AnsiString & Str)
{
  DoShred(Str);
}
//---------------------------------------------------------------------------
void __fastcall Shred(RawByteString & Str)
{
  DoShred(Str);
}
//---------------------------------------------------------------------------
UnicodeString AnsiToString(const RawByteString & S)
{
  return UnicodeString(AnsiString(S));
}
//---------------------------------------------------------------------------
UnicodeString AnsiToString(const char * S, size_t Len)
{
  return UnicodeString(AnsiString(S, Len));
}
//---------------------------------------------------------------------------
UnicodeString UTFToString(const RawByteString & S)
{
  // Simply casting RawByteString to UTF8String does not work
  return UnicodeString(UTF8String(S.c_str(), S.Length()));
}
//---------------------------------------------------------------------------
// Note similar function ValidLocalFileName
UnicodeString MakeValidFileName(UnicodeString FileName)
{
  // Note similar list in LocalInvalidChars
  UnicodeString IllegalChars = L":;,=+<>|\"[] \\/?*";
  for (int Index = 0; Index < IllegalChars.Length(); Index++)
  {
    FileName = ReplaceChar(FileName, IllegalChars[Index+1], L'-');
  }
  return FileName;
}
//---------------------------------------------------------------------------
UnicodeString RootKeyToStr(HKEY RootKey, const UnicodeString & Default)
{
  if (RootKey == HKEY_USERS) return L"HKU";
    else
  if (RootKey == HKEY_LOCAL_MACHINE) return L"HKLM";
    else
  if (RootKey == HKEY_CURRENT_USER) return L"HKCU";
    else
  if (RootKey == HKEY_CLASSES_ROOT) return L"HKCR";
    else
  if (RootKey == HKEY_CURRENT_CONFIG) return L"HKCC";
    else
  if (RootKey == HKEY_DYN_DATA) return L"HKDD";
    else
  {
    if (Default.IsEmpty())
    {
      Abort();
    }
    return Default;
  };
}
//---------------------------------------------------------------------------
UnicodeString BooleanToEngStr(bool B)
{
  if (B)
  {
    return L"Yes";
  }
  else
  {
    return L"No";
  }
}
//---------------------------------------------------------------------------
UnicodeString BooleanToStr(bool B)
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
UnicodeString DefaultStr(const UnicodeString & Str, const UnicodeString & Default)
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
// For alternative with quoting support, see TFTPFileSystem::CutFeature
UnicodeString CutToChar(UnicodeString &Str, wchar_t Ch, bool Trim)
{
  Integer P = Str.Pos(Ch);
  UnicodeString Result;
  if (P)
  {
    Result = Str.SubString(1, P-1);
    Str.Delete(1, P);
  }
  else
  {
    Result = Str;
    Str = L"";
  }
  if (Trim)
  {
    Result = Result.TrimRight();
    Str = Str.TrimLeft();
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString CopyToChars(const UnicodeString & Str, int & From, UnicodeString Chs, bool Trim,
  wchar_t * Delimiter, bool DoubleDelimiterEscapes)
{
  UnicodeString Result;

  int P;
  for (P = From; P <= Str.Length(); P++)
  {
    if (IsDelimiter(Chs, Str, P))
    {
      if (DoubleDelimiterEscapes &&
          (P < Str.Length()) &&
          IsDelimiter(Chs, Str, P + 1) &&
          (Str[P + 1] == Str[P]))
      {
        Result += Str[P];
        P++;
      }
      else
      {
        break;
      }
    }
    else
    {
      Result += Str[P];
    }
  }

  if (P <= Str.Length())
  {
    if (Delimiter != NULL)
    {
      *Delimiter = Str[P];
    }
  }
  else
  {
    if (Delimiter != NULL)
    {
      *Delimiter = L'\0';
    }
  }
  // even if we reached the end, return index, as if there were the delimiter,
  // so caller can easily find index of the end of the piece by subtracting
  // 2 from From (as long as he did not asked for trimming)
  From = P+1;
  if (Trim)
  {
    Result = Result.TrimRight();
    while ((From <= Str.Length()) && (Str[From] == L' '))
    {
      From++;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString CopyToChar(const UnicodeString & Str, wchar_t Ch, bool Trim)
{
  int From = 1;
  return CopyToChars(Str, From, UnicodeString(Ch), Trim);
}
//---------------------------------------------------------------------------
UnicodeString RemoveSuffix(const UnicodeString & Str, const UnicodeString & Suffix, bool RemoveNumbersAfterSuffix)
{
  UnicodeString Result = Str;
  UnicodeString Buf = Str;
  if (RemoveNumbersAfterSuffix)
  {
    while (!Buf.IsEmpty() && IsDigit(Buf[Buf.Length()]))
    {
      Buf.SetLength(Buf.Length() - 1);
    }
  }
  if (EndsStr(Suffix, Buf))
  {
    Result.SetLength(Buf.Length() - Suffix.Length());
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString DelimitStr(const UnicodeString & Str, wchar_t Quote)
{
  UnicodeString SpecialChars;
  if (Quote != L'\'')
  {
    SpecialChars = L"$\\";
    if (Quote == L'"')
    {
      SpecialChars += L"`\"";
    }
  }
  UnicodeString Result(Str);
  for (int i = 1; i <= Result.Length(); i++)
  {
    if (Result.IsDelimiter(SpecialChars, i))
    {
      Result.Insert(L"\\", i);
      i++;
    }
  }
  if (Result.IsDelimiter(L"-", 1))
  {
    Result.Insert(L"./", 1);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString MidStr(const UnicodeString & Text, int Start)
{
  return Text.SubString(Start, Text.Length() - Start + 1);
}
//---------------------------------------------------------------------------
UnicodeString ShellQuoteStr(const UnicodeString & Str)
{
  wchar_t Quote = L'"';
  UnicodeString QuoteStr(Quote);
  return QuoteStr + DelimitStr(Str, Quote) + QuoteStr;
}
//---------------------------------------------------------------------------
UnicodeString ExceptionLogString(Exception *E)
{
  DebugAssert(E);
  if (E->InheritsFrom(__classid(Exception)))
  {
    UnicodeString Msg;
    Msg = FORMAT(L"(%s) %s", (E->ClassName(), E->Message));
    if (E->InheritsFrom(__classid(ExtException)))
    {
      TStrings * MoreMessages = ((ExtException*)E)->MoreMessages;
      if (MoreMessages)
      {
        Msg += L"\n" +
          ReplaceStr(MoreMessages->Text, L"\r", L"");
      }
    }
    return Msg;
  }
  else
  {
    wchar_t Buffer[1024];
    ExceptionErrorMessage(ExceptObject(), ExceptAddr(), Buffer, LENOF(Buffer));
    return UnicodeString(Buffer);
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall MainInstructions(const UnicodeString & S)
{
  UnicodeString MainMsgTag = LoadStr(MAIN_MSG_TAG);
  return MainMsgTag + S + MainMsgTag;
}
//---------------------------------------------------------------------------
bool __fastcall HasParagraphs(const UnicodeString & S)
{
  return (S.Pos(L"\n\n") > 0);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall MainInstructionsFirstParagraph(const UnicodeString & S)
{
  // WORKAROUND, we consider it bad practice, the highlighting should better
  // be localized (but maybe we change our mind later)
  UnicodeString Result;
  int Pos = S.Pos(L"\n\n");
  // we would not be calling this on single paragraph message
  if (DebugAlwaysTrue(Pos > 0))
  {
    Result =
      MainInstructions(S.SubString(1, Pos - 1)) +
      S.SubString(Pos, S.Length() - Pos + 1);
  }
  else
  {
    Result = MainInstructions(S);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool ExtractMainInstructions(UnicodeString & S, UnicodeString & MainInstructions)
{
  bool Result = false;
  UnicodeString MainMsgTag = LoadStr(MAIN_MSG_TAG);
  if (StartsStr(MainMsgTag, S))
  {
    int EndTagPos =
      S.SubString(MainMsgTag.Length() + 1, S.Length() - MainMsgTag.Length()).Pos(MainMsgTag);
    if (EndTagPos > 0)
    {
      MainInstructions = S.SubString(MainMsgTag.Length() + 1, EndTagPos - 1);
      S.Delete(1, EndTagPos + (2 * MainMsgTag.Length()) - 1);
      Result = true;
    }
  }

  DebugAssert(MainInstructions.Pos(MainMsgTag) == 0);
  DebugAssert(S.Pos(MainMsgTag) == 0);

  return Result;
}
//---------------------------------------------------------------------------
static int FindInteractiveMsgStart(const UnicodeString & S)
{
  int Result = 0;
  UnicodeString InteractiveMsgTag = LoadStr(INTERACTIVE_MSG_TAG);
  if (EndsStr(InteractiveMsgTag, S) &&
      (S.Length() >= 2 * InteractiveMsgTag.Length()))
  {
    Result = S.Length() - 2 * InteractiveMsgTag.Length() + 1;
    while ((Result > 0) && (S.SubString(Result, InteractiveMsgTag.Length()) != InteractiveMsgTag))
    {
      Result--;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString RemoveMainInstructionsTag(UnicodeString S)
{
  UnicodeString MainInstruction;
  if (ExtractMainInstructions(S, MainInstruction))
  {
    S = MainInstruction + S;
  }
  return S;
}
//---------------------------------------------------------------------------
UnicodeString UnformatMessage(UnicodeString S)
{
  S = RemoveMainInstructionsTag(S);

  int InteractiveMsgStart = FindInteractiveMsgStart(S);
  if (InteractiveMsgStart > 0)
  {
    S = S.SubString(1, InteractiveMsgStart - 1);
  }
  return S;
}
//---------------------------------------------------------------------------
UnicodeString RemoveInteractiveMsgTag(UnicodeString S)
{
  int InteractiveMsgStart = FindInteractiveMsgStart(S);
  if (InteractiveMsgStart > 0)
  {
    UnicodeString InteractiveMsgTag = LoadStr(INTERACTIVE_MSG_TAG);
    S.Delete(InteractiveMsgStart, InteractiveMsgTag.Length());
    S.Delete(S.Length() - InteractiveMsgTag.Length() + 1, InteractiveMsgTag.Length());
  }
  return S;
}
//---------------------------------------------------------------------------
UnicodeString RemoveEmptyLines(const UnicodeString & S)
{
  return
    ReplaceStr(
      ReplaceStr(S.TrimRight(), L"\n\n", L"\n"),
      L"\n \n", L"\n");
}
//---------------------------------------------------------------------------
bool IsNumber(const UnicodeString Str)
{
  bool Result = (Str.Length() > 0);
  for (int Index = 1; (Index < Str.Length()) && Result; Index++)
  {
    wchar_t C = Str[Index];
    if ((C < L'0') || (C > L'9'))
    {
      Result = false;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString EncodeStrToBase64(const RawByteString & Str)
{
  UnicodeString Result = EncodeBase64(Str.c_str(), Str.Length());
  Result = ReplaceStr(Result, sLineBreak, EmptyStr);
  return Result;
}
//---------------------------------------------------------------------------
RawByteString DecodeBase64ToStr(const UnicodeString & Str)
{
  TBytes Bytes = DecodeBase64(Str);
  RawByteString Result;
  if (Bytes.Length > 0)
  {
    // This might be the same as TEncoding::ASCII->GetString.
    // const_cast: The operator[] const is (badly?) implemented to return by value
    Result = RawByteString(reinterpret_cast<const char *>(&const_cast<TBytes &>(Bytes)[0]), Bytes.Length);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString Base64ToUrlSafe(const UnicodeString & S)
{
  UnicodeString Result = S;
  while (EndsStr(L"=", Result))
  {
    Result.SetLength(Result.Length() - 1);
  }
  // See https://en.wikipedia.org/wiki/Base64#Implementations_and_history
  Result = ReplaceChar(Result, L'+', L'-');
  Result = ReplaceChar(Result, L'/', L'_');
  return Result;
}
//---------------------------------------------------------------------------
const wchar_t NormalizedFingerprintSeparator = L'-';
//---------------------------------------------------------------------------
UnicodeString MD5ToUrlSafe(const UnicodeString & S)
{
  return ReplaceChar(S, L':', NormalizedFingerprintSeparator);
}
//---------------------------------------------------------------------------
bool SameChecksum(const UnicodeString & AChecksum1, const UnicodeString & AChecksum2, bool Base64)
{
  UnicodeString Checksum1(AChecksum1);
  UnicodeString Checksum2(AChecksum2);
  bool Result;
  if (Base64)
  {
    Checksum1 = Base64ToUrlSafe(Checksum1);
    Checksum2 = Base64ToUrlSafe(Checksum2);
    Result = SameStr(Checksum1, Checksum2);
  }
  else
  {
    Checksum1 = MD5ToUrlSafe(Checksum1);
    Checksum2 = MD5ToUrlSafe(Checksum2);
    Result = SameText(Checksum1, Checksum2);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall SystemTemporaryDirectory()
{
  UnicodeString TempDir;
  TempDir.SetLength(MAX_PATH);
  TempDir.SetLength(GetTempPath(MAX_PATH, TempDir.c_str()));
  return TempDir;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall GetShellFolderPath(int CSIdl)
{
  UnicodeString Result;
  wchar_t Path[2 * MAX_PATH + 10] = L"\0";
  if (SUCCEEDED(SHGetFolderPath(NULL, CSIdl, NULL, SHGFP_TYPE_CURRENT, Path)))
  {
    Result = Path;
  }
  return Result;
}
//---------------------------------------------------------------------------
static UnicodeString __fastcall GetWineHomeFolder()
{
  UnicodeString Result;

  UnicodeString WineHostHome = GetEnvironmentVariable(L"WINE_HOST_HOME");
  if (!WineHostHome.IsEmpty())
  {
    Result = L"Z:" + FromUnixPath(WineHostHome);
  }
  else
  {
    // Should we use WinAPI GetUserName() instead?
    UnicodeString UserName = GetEnvironmentVariable(L"USERNAME");
    if (!UserName.IsEmpty())
    {
      Result = L"Z:\\home\\" + UserName;
    }
  }

  if (!DirectoryExists(Result))
  {
    Result = L"";
  }

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall GetPersonalFolder()
{
  UnicodeString Result = GetShellFolderPath(CSIDL_PERSONAL);

  if (IsWine())
  {
    UnicodeString WineHome = GetWineHomeFolder();

    if (!WineHome.IsEmpty())
    {
      // if at least home exists, use it
      Result = WineHome;

      // but try to go deeper to "Documents"
      UnicodeString WineDocuments =
        IncludeTrailingBackslash(WineHome) + L"Documents";
      if (DirectoryExists(WineDocuments))
      {
        Result = WineDocuments;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall GetDesktopFolder()
{
  UnicodeString Result = GetShellFolderPath(CSIDL_DESKTOPDIRECTORY);

  if (IsWine())
  {
    UnicodeString WineHome = GetWineHomeFolder();

    if (!WineHome.IsEmpty())
    {
      UnicodeString WineDesktop =
        IncludeTrailingBackslash(WineHome) + L"Desktop";
      if (DirectoryExists(WineHome))
      {
        Result = WineDesktop;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
// Particularly needed when using file name selected by TFilenameEdit,
// as it wraps a path to double-quotes, when there is a space in the path.
UnicodeString __fastcall StripPathQuotes(const UnicodeString Path)
{
  if ((Path.Length() >= 2) &&
      (Path[1] == L'\"') && (Path[Path.Length()] == L'\"'))
  {
    return Path.SubString(2, Path.Length() - 2);
  }
  else
  {
    return Path;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall AddQuotes(UnicodeString Str)
{
  if (Str.Pos(L" ") > 0)
  {
    Str = L"\"" + Str + L"\"";
  }
  return Str;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall AddPathQuotes(UnicodeString Path)
{
  Path = StripPathQuotes(Path);
  return AddQuotes(Path);
}
//---------------------------------------------------------------------------
static wchar_t * __fastcall ReplaceChar(
  UnicodeString & FileName, wchar_t * InvalidChar, wchar_t InvalidCharsReplacement)
{
  int Index = InvalidChar - FileName.c_str() + 1;
  if (InvalidCharsReplacement == TokenReplacement)
  {
    // currently we do not support unicode chars replacement
    if (FileName[Index] > 0xFF)
    {
      EXCEPTION;
    }

    FileName.Insert(ByteToHex(static_cast<unsigned char>(FileName[Index])), Index + 1);
    FileName[Index] = TokenPrefix;
    InvalidChar = FileName.c_str() + Index + 2;
  }
  else
  {
    FileName[Index] = InvalidCharsReplacement;
    InvalidChar = FileName.c_str() + Index;
  }
  return InvalidChar;
}
//---------------------------------------------------------------------------
//  Note similar function MakeValidFileName
UnicodeString __fastcall ValidLocalFileName(UnicodeString FileName)
{
  return ValidLocalFileName(FileName, L'_', L"", LocalInvalidChars);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ValidLocalFileName(
  UnicodeString FileName, wchar_t InvalidCharsReplacement,
  const UnicodeString & TokenizibleChars, const UnicodeString & LocalInvalidChars)
{
  if (InvalidCharsReplacement != NoReplacement)
  {
    bool ATokenReplacement = (InvalidCharsReplacement == TokenReplacement);
    const wchar_t * Chars =
      (ATokenReplacement ? TokenizibleChars : LocalInvalidChars).c_str();
    wchar_t * InvalidChar = FileName.c_str();
    while ((InvalidChar = wcspbrk(InvalidChar, Chars)) != NULL)
    {
      int Pos = (InvalidChar - FileName.c_str() + 1);
      wchar_t Char;
      if (ATokenReplacement &&
          (*InvalidChar == TokenPrefix) &&
          (((FileName.Length() - Pos) <= 1) ||
           (((Char = static_cast<wchar_t>(HexToByte(FileName.SubString(Pos + 1, 2)))) == L'\0') ||
            (TokenizibleChars.Pos(Char) == 0))))
      {
        InvalidChar++;
      }
      else
      {
        InvalidChar = ReplaceChar(FileName, InvalidChar, InvalidCharsReplacement);
      }
    }

    // Windows trim trailing space or dot, hence we must encode it to preserve it
    if (!FileName.IsEmpty() &&
        ((FileName[FileName.Length()] == L' ') ||
         (FileName[FileName.Length()] == L'.')))
    {
      ReplaceChar(FileName, FileName.c_str() + FileName.Length() - 1, InvalidCharsReplacement);
    }

    if (IsReservedName(FileName))
    {
      int P = FileName.Pos(".");
      if (P == 0)
      {
        P = FileName.Length() + 1;
      }
      FileName.Insert(L"%00", P);
    }
  }
  return FileName;
}
//---------------------------------------------------------------------------
void __fastcall SplitCommand(UnicodeString Command, UnicodeString &Program,
  UnicodeString & Params, UnicodeString & Dir)
{
  Command = Command.Trim();
  Params = L"";
  Dir = L"";
  if (!Command.IsEmpty() && (Command[1] == L'\"'))
  {
    Command.Delete(1, 1);
    int P = Command.Pos(L'"');
    if (P)
    {
      Program = Command.SubString(1, P-1).Trim();
      Params = Command.SubString(P + 1, Command.Length() - P).Trim();
    }
    else
    {
      throw Exception(FMTLOAD(INVALID_SHELL_COMMAND, (L"\"" + Command)));
    }
  }
  else
  {
    int P = Command.Pos(L" ");
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
  int B = Program.LastDelimiter(L"\\/");
  if (B)
  {
    Dir = Program.SubString(1, B).Trim();
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ExtractProgram(UnicodeString Command)
{
  UnicodeString Program;
  UnicodeString Params;
  UnicodeString Dir;

  SplitCommand(Command, Program, Params, Dir);

  return Program;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ExtractProgramName(UnicodeString Command)
{
  UnicodeString Name = ExtractFileName(ExtractProgram(Command));
  int Dot = Name.LastDelimiter(L".");
  if (Dot > 0)
  {
    Name = Name.SubString(1, Dot - 1);
  }
  return Name;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FormatCommand(UnicodeString Program, UnicodeString Params)
{
  Program = Program.Trim();
  Params = Params.Trim();
  if (!Params.IsEmpty()) Params = L" " + Params;
  Program = AddQuotes(Program);
  return Program + Params;
}
//---------------------------------------------------------------------------
const wchar_t ShellCommandFileNamePattern[] = L"!.!";
//---------------------------------------------------------------------------
void __fastcall ReformatFileNameCommand(UnicodeString & Command)
{
  if (!Command.IsEmpty())
  {
    UnicodeString Program, Params, Dir;
    SplitCommand(Command, Program, Params, Dir);
    if (Params.Pos(ShellCommandFileNamePattern) == 0)
    {
      Params = Params + (Params.IsEmpty() ? L"" : L" ") + ShellCommandFileNamePattern;
    }
    Command = FormatCommand(Program, Params);
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ExpandFileNameCommand(const UnicodeString Command,
  const UnicodeString FileName)
{
  return AnsiReplaceStr(Command, ShellCommandFileNamePattern,
    AddPathQuotes(FileName));
}
//---------------------------------------------------------------------------
UnicodeString __fastcall EscapeParam(const UnicodeString & Param)
{
  // Make sure this won't break RTF syntax
  return ReplaceStr(Param, L"\"", L"\"\"");
}
//---------------------------------------------------------------------------
UnicodeString __fastcall EscapePuttyCommandParam(UnicodeString Param)
{
  bool Space = false;

  for (int i = 1; i <= Param.Length(); i++)
  {
    switch (Param[i])
    {
      case L'"':
        Param.Insert(L"\\", i);
        i++;
        break;

      case L' ':
        Space = true;
        break;

      case L'\\':
        int i2 = i;
        while ((i2 <= Param.Length()) && (Param[i2] == L'\\'))
        {
          i2++;
        }
        if ((i2 <= Param.Length()) && (Param[i2] == L'"'))
        {
          while (Param[i] == L'\\')
          {
            Param.Insert(L"\\", i);
            i += 2;
          }
          i--;
        }
        break;
    }
  }

  if (Space)
  {
    Param = L"\"" + Param + L'"';
  }

  return Param;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall StringsToParams(TStrings * Strings)
{
  UnicodeString Result;

  for (int Index = 0; Index < Strings->Count; Index++)
  {
    UnicodeString Name = Strings->Names[Index];
    UnicodeString Value = Strings->ValueFromIndex[Index];
    // Do not quote if it is all-numeric
    if (IntToStr(StrToIntDef(Value, -1)) != Value)
    {
      Value = FORMAT(L"\"%s\"", (EscapeParam(Value)));
    }
    Result += FORMAT(L" %s=%s", (Name, Value));
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ExpandEnvironmentVariables(const UnicodeString & Str)
{
  UnicodeString Buf;
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
UnicodeString GetNormalizedPath(const UnicodeString & Path)
{
  UnicodeString Result = ExcludeTrailingBackslash(Path);
  Result = FromUnixPath(Result);
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString GetCanonicalPath(const UnicodeString & Path)
{
  UnicodeString Result = ExtractShortPathName(Path);
  if (Result.IsEmpty())
  {
    Result = Path;
  }
  Result = GetNormalizedPath(Result);
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall IsPathToSameFile(const UnicodeString & Path1, const UnicodeString & Path2)
{
  UnicodeString CanonicalPath1 = GetCanonicalPath(Path1);
  UnicodeString CanonicalPath2 = GetCanonicalPath(Path2);

  bool Result = SameText(CanonicalPath1, CanonicalPath2);
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall SamePaths(const UnicodeString & Path1, const UnicodeString & Path2)
{
  // TODO: ExpandUNCFileName
  return AnsiSameText(IncludeTrailingBackslash(Path1), IncludeTrailingBackslash(Path2));
}
//---------------------------------------------------------------------------
UnicodeString CombinePaths(const UnicodeString & Path1, const UnicodeString & Path2)
{
  // Make sure that C: + foo => C:\foo and not C:foo
  UnicodeString Path1Terminated = Path1;
  if (EndsStr(L":", Path1Terminated))
  {
    Path1Terminated = IncludeTrailingBackslash(Path1Terminated);
  }
  return TPath::Combine(Path1Terminated, Path2);
}
//---------------------------------------------------------------------------
int __fastcall CompareLogicalText(
  const UnicodeString & S1, const UnicodeString & S2, bool NaturalOrderNumericalSorting)
{
  // Keep in sync with CompareLogicalTextPas

  int Result;
  if (NaturalOrderNumericalSorting)
  {
    Result = StrCmpLogicalW(S1.c_str(), S2.c_str());
  }
  else
  {
    Result = lstrcmpi(S1.c_str(), S2.c_str());
  }
  // For deterministic results
  if (Result == 0)
  {
    Result = lstrcmp(S1.c_str(), S2.c_str());
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall CompareNumber(__int64 Value1, __int64 Value2)
{
  int Result;
  if (Value1 < Value2)
  {
    Result = -1;
  }
  else if (Value1 == Value2)
  {
    Result = 0;
  }
  else
  {
    Result = 1;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool ContainsTextSemiCaseSensitive(const UnicodeString & Text, const UnicodeString & SubText)
{
  bool Result;
  if (AnsiLowerCase(SubText) == SubText)
  {
    Result = ContainsText(Text, SubText);
  }
  else
  {
    Result = ContainsStr(Text, SubText);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall IsReservedName(UnicodeString FileName)
{
  int P = FileName.Pos(L".");
  int Len = (P > 0) ? P - 1 : FileName.Length();
  if ((Len == 3) || (Len == 4))
  {
    if (P > 0)
    {
      FileName.SetLength(P - 1);
    }
    static UnicodeString Reserved[] = {
      L"CON", L"PRN", L"AUX", L"NUL",
      L"COM1", L"COM2", L"COM3", L"COM4", L"COM5", L"COM6", L"COM7", L"COM8", L"COM9",
      L"LPT1", L"LPT2", L"LPT3", L"LPT4", L"LPT5", L"LPT6", L"LPT7", L"LPT8", L"LPT9" };
    for (unsigned int Index = 0; Index < LENOF(Reserved); Index++)
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
// ApiPath support functions
// Inspired by
// https://stackoverflow.com/q/18580945/850848
// This can be reimplemented using PathCchCanonicalizeEx on Windows 8 and later
enum PATH_PREFIX_TYPE
{
  PPT_UNKNOWN,
  PPT_ABSOLUTE,           //Found absolute path that is none of the other types
  PPT_UNC,                //Found \\server\share\ prefix
  PPT_LONG_UNICODE,       //Found \\?\ prefix
  PPT_LONG_UNICODE_UNC,   //Found \\?\UNC\ prefix
};
//---------------------------------------------------------------------------
static int GetOffsetAfterPathRoot(const UnicodeString & Path, PATH_PREFIX_TYPE & PrefixType)
{
  // Checks if 'pPath' begins with the drive, share, prefix, etc
  // EXAMPLES:
  //    Path                          Return:   Points at:                 PrefixType:
  //   Relative\Folder\File.txt        0         Relative\Folder\File.txt   PPT_UNKNOWN
  //   \RelativeToRoot\Folder          1         RelativeToRoot\Folder      PPT_ABSOLUTE
  //   C:\Windows\Folder               3         Windows\Folder             PPT_ABSOLUTE
  //   \\server\share\Desktop          15        Desktop                    PPT_UNC
  //   \\?\C:\Windows\Folder           7         Windows\Folder             PPT_LONG_UNICODE
  //   \\?\UNC\server\share\Desktop    21        Desktop                    PPT_LONG_UNICODE_UNC
  // RETURN:
  //      = Index in 'pPath' after the root, or
  //      = 0 if no root was found
  int Result = 0;

  PrefixType = PPT_UNKNOWN;

  if (!Path.IsEmpty())
  {
    int Len = Path.Length();

    // Replace all /'s with \'s because PathSkipRoot and PathIsRelative can't handle /'s
    UnicodeString WinPath = ReplaceChar(Path, L'/', L'\\');

     // Now call the API
    LPCTSTR Buffer = PathSkipRoot(WinPath.c_str());
    if (Buffer != NULL)
    {
      Result = (Buffer - WinPath.c_str()) + 1;
    }

    // Now determine the type of prefix
    int IndCheckUNC = -1;

    if ((Len >= 8) &&
        (Path[1] == L'\\' || Path[1] == L'/') &&
        (Path[2] == L'\\' || Path[2] == L'/') &&
        (Path[3] == L'?') &&
        (Path[4] == L'\\' || Path[4] == L'/') &&
        (Path[5] == L'U' || Path[5] == L'u') &&
        (Path[6] == L'N' || Path[6] == L'n') &&
        (Path[7] == L'C' || Path[7] == L'c') &&
        (Path[8] == L'\\' || Path[8] == L'/'))
    {
      // Found \\?\UNC\ prefix
      PrefixType = PPT_LONG_UNICODE_UNC;

      //Check for UNC share later
      IndCheckUNC = 8;
    }
    else if ((Len >= 4) &&
        (Path[1] == L'\\' || Path[1] == L'/') &&
        (Path[2] == L'\\' || Path[2] == L'/') &&
        (Path[3] == L'?') &&
        (Path[4] == L'\\' || Path[4] == L'/'))
    {
      // Found \\?\ prefix
      PrefixType = PPT_LONG_UNICODE;
    }
    else if ((Len >= 2) &&
        (Path[1] == L'\\' || Path[1] == L'/') &&
        (Path[2] == L'\\' || Path[2] == L'/'))
    {
      // Check for UNC share later
      IndCheckUNC = 2;
    }

    if (IndCheckUNC >= 0)
    {
      // Check for UNC, i.e. \\server\share\ part
      int Index = IndCheckUNC;
      for (int SkipSlashes = 2; SkipSlashes > 0; SkipSlashes--)
      {
        for(; Index <= Len; Index++)
        {
          TCHAR z = Path[Index];
          if ((z == L'\\') || (z == L'/') || (Index >= Len))
          {
            Index++;
            if (SkipSlashes == 1)
            {
              if (PrefixType == PPT_UNKNOWN)
              {
                PrefixType = PPT_UNC;
              }
            }

            break;
          }
        }
      }
    }

    // Only if we didn't determine any other type
    if (PrefixType == PPT_UNKNOWN)
    {
      if (!PathIsRelative(WinPath.c_str()))
      {
        PrefixType = PPT_ABSOLUTE;
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall MakeUnicodeLargePath(UnicodeString Path)
{
  // Convert path from 'into a larger Unicode path, that allows up to 32,767 character length
  UnicodeString Result;

  if (!Path.IsEmpty())
  {
    // Determine the type of the existing prefix
    PATH_PREFIX_TYPE PrefixType;
    GetOffsetAfterPathRoot(Path, PrefixType);

    // Assume path to be without change
    Result = Path;

    switch (PrefixType)
    {
      case PPT_ABSOLUTE:
        {
          // First we need to check if its an absolute path relative to the root
          bool AddPrefix = true;
          if ((Path.Length() >= 1) &&
              ((Path[1] == L'\\') || (Path[1] == L'/')))
          {
            AddPrefix = FALSE;

            // Get current root path
            UnicodeString CurrentDir = GetCurrentDir();
            PATH_PREFIX_TYPE PrefixType2; // unused
            int Following = GetOffsetAfterPathRoot(CurrentDir, PrefixType2);
            if (Following > 0)
            {
              AddPrefix = true;
              Result = CurrentDir.SubString(1, Following - 1) + Result.SubString(2, Result.Length() - 1);
            }
          }

          if (AddPrefix)
          {
            // Add \\?\ prefix
            Result = L"\\\\?\\" + Result;
          }
        }
        break;

      case PPT_UNC:
        // First we need to remove the opening slashes for UNC share
        if ((Result.Length() >= 2) &&
            ((Result[1] == L'\\') || (Result[1] == L'/')) &&
            ((Result[2] == L'\\') || (Result[2] == L'/')))
        {
          Result = Result.SubString(3, Result.Length() - 2);
        }

        // Add \\?\UNC\ prefix
        Result = L"\\\\?\\UNC\\" + Result;
        break;

      case PPT_LONG_UNICODE:
      case PPT_LONG_UNICODE_UNC:
        // nothing to do
        break;
    }

  }

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ApiPath(UnicodeString Path)
{
  UnicodeString Drive = ExtractFileDrive(Path);
  // This may match even a path like "C:" or "\\server\\share", but we do not really care
  if (Drive.IsEmpty() || (Path.SubString(Drive.Length() + 1, 1) != L"\\"))
  {
    Path = ExpandFileName(Path);
  }

  // Max path for directories is 12 characters shorter than max path for files
  if (Path.Length() >= MAX_PATH - 12)
  {
    if (Configuration != NULL)
    {
      Configuration->Usage->Inc(L"LongPath");
    }
    Path = MakeUnicodeLargePath(Path);
  }
  return Path;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall DisplayableStr(const RawByteString & Str)
{
  bool Displayable = true;
  int Index = 1;
  while ((Index <= Str.Length()) && Displayable)
  {
    if (((Str[Index] < '\x20') || IsWideChar(Str[Index])) &&
        (Str[Index] != '\n') && (Str[Index] != '\r') && (Str[Index] != '\t') && (Str[Index] != '\b'))
    {
      Displayable = false;
    }
    Index++;
  }

  UnicodeString Result;
  if (Displayable)
  {
    Result = L"\"";
    for (int Index = 1; Index <= Str.Length(); Index++)
    {
      switch (Str[Index])
      {
        case '\n':
          Result += L"\\n";
          break;

        case '\r':
          Result += L"\\r";
          break;

        case '\t':
          Result += L"\\t";
          break;

        case '\b':
          Result += L"\\b";
          break;

        case '\\':
          Result += L"\\\\";
          break;

        case '"':
          Result += L"\\\"";
          break;

        default:
          Result += wchar_t(Str[Index]);
          break;
      }
    }
    Result += L"\"";
  }
  else
  {
    Result = L"0x" + BytesToHex(Str);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ByteToHex(unsigned char B, bool UpperCase)
{
  static wchar_t UpperDigits[] = L"0123456789ABCDEF";
  static wchar_t LowerDigits[] = L"0123456789abcdef";

  const wchar_t * Digits = (UpperCase ? UpperDigits : LowerDigits);
  UnicodeString Result;
  Result.SetLength(2);
  Result[1] = Digits[(B & 0xF0) >> 4];
  Result[2] = Digits[(B & 0x0F) >> 0];
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall BytesToHex(const unsigned char * B, size_t Length, bool UpperCase, wchar_t Separator)
{
  UnicodeString Result;
  for (size_t i = 0; i < Length; i++)
  {
    Result += ByteToHex(B[i], UpperCase);
    if ((Separator != L'\0') && (i < Length - 1))
    {
      Result += Separator;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall BytesToHex(RawByteString Str, bool UpperCase, wchar_t Separator)
{
  return BytesToHex(reinterpret_cast<const unsigned char *>(Str.c_str()), Str.Length(), UpperCase, Separator);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall CharToHex(wchar_t Ch, bool UpperCase)
{
  // BytesToHex would encode with opposite/unexpected endianness
  return ByteToHex(Ch >> 8, UpperCase) + ByteToHex(Ch & 0xFF, UpperCase);
}
//---------------------------------------------------------------------------
RawByteString __fastcall HexToBytes(const UnicodeString Hex)
{
  static UnicodeString Digits = L"0123456789ABCDEF";
  RawByteString Result;
  int L, P1, P2;
  L = Hex.Length();
  if (L % 2 == 0)
  {
    for (int i = 1; i <= Hex.Length(); i += 2)
    {
      P1 = Digits.Pos(towupper(Hex[i]));
      P2 = Digits.Pos(towupper(Hex[i + 1]));
      if (P1 <= 0 || P2 <= 0)
      {
        Result = L"";
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
unsigned char __fastcall HexToByte(const UnicodeString Hex)
{
  static UnicodeString Digits = L"0123456789ABCDEF";
  DebugAssert(Hex.Length() == 2);
  int P1 = Digits.Pos(towupper(Hex[1]));
  int P2 = Digits.Pos(towupper(Hex[2]));

  return
    static_cast<unsigned char>(((P1 <= 0) || (P2 <= 0)) ? 0 : (((P1 - 1) << 4) + (P2 - 1)));
}
//---------------------------------------------------------------------------
bool __fastcall IsLowerCaseLetter(wchar_t Ch)
{
  return (Ch >= 'a') && (Ch <= 'z');
}
//---------------------------------------------------------------------------
bool __fastcall IsUpperCaseLetter(wchar_t Ch)
{
  return (Ch >= 'A') && (Ch <= 'Z');
}
//---------------------------------------------------------------------------
bool __fastcall IsLetter(wchar_t Ch)
{
  return IsLowerCaseLetter(Ch) || IsUpperCaseLetter(Ch);
}
//---------------------------------------------------------------------------
bool __fastcall IsDigit(wchar_t Ch)
{
  return (Ch >= '0') && (Ch <= '9');
}
//---------------------------------------------------------------------------
bool __fastcall IsHex(wchar_t Ch)
{
  return
    IsDigit(Ch) ||
    ((Ch >= 'A') && (Ch <= 'F')) ||
    ((Ch >= 'a') && (Ch <= 'f'));
}
//---------------------------------------------------------------------------
TSearchRecSmart::TSearchRecSmart()
{
  FLastWriteTimeSource.dwLowDateTime = 0;
  FLastWriteTimeSource.dwHighDateTime = 0;
}
//---------------------------------------------------------------------------
void TSearchRecSmart::Clear()
{
  Size = 0;
  Attr = 0;
  Name = TFileName();
  ExcludeAttr = 0;
  FindHandle = 0;
  memset(&FindData, 0, sizeof(FindData));
  FLastWriteTimeSource.dwLowDateTime = 0;
  FLastWriteTimeSource.dwHighDateTime = 0;
}
//---------------------------------------------------------------------------
// This can be replaced with TSearchRec.TimeStamp
TDateTime TSearchRecSmart::GetLastWriteTime() const
{
  if ((FindData.ftLastWriteTime.dwLowDateTime != FLastWriteTimeSource.dwLowDateTime) ||
      (FindData.ftLastWriteTime.dwHighDateTime != FLastWriteTimeSource.dwHighDateTime))
  {
    FLastWriteTimeSource = FindData.ftLastWriteTime;
    FLastWriteTime = FileTimeToDateTime(FLastWriteTimeSource);
  }
  return FLastWriteTime;
}
//---------------------------------------------------------------------------
bool TSearchRecSmart::IsRealFile() const
{
  return ::IsRealFile(Name);
}
//---------------------------------------------------------------------------
bool TSearchRecSmart::IsDirectory() const
{
  return FLAGSET(Attr, faDirectory);
}
//---------------------------------------------------------------------------
bool TSearchRecSmart::IsHidden() const
{
  return FLAGSET(Attr, faHidden);
}
//---------------------------------------------------------------------------
UnicodeString TSearchRecChecked::GetFilePath() const
{
  return CombinePaths(Dir, Name);
}
//---------------------------------------------------------------------------
TSearchRecOwned::~TSearchRecOwned()
{
  if (Opened)
  {
    FindClose(*this);
  }
}
//---------------------------------------------------------------------------
void TSearchRecOwned::Close()
{
  FindClose(*this);
  Opened = false;
}
//---------------------------------------------------------------------------
int __fastcall FindCheck(int Result, const UnicodeString & Path)
{
  if ((Result != ERROR_SUCCESS) &&
      (Result != ERROR_FILE_NOT_FOUND) &&
      (Result != ERROR_NO_MORE_FILES))
  {
    throw EOSExtException(FMTLOAD(FIND_FILE_ERROR, (Path)), Result);
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall FindFirstUnchecked(const UnicodeString & Path, int Attr, TSearchRecChecked & F)
{
  F.Path = Path;
  F.Dir = ExtractFilePath(Path);
  int Result = FindFirst(ApiPath(Path), Attr, F);
  F.Opened = (Result == 0);
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall FindFirstChecked(const UnicodeString & Path, int Attr, TSearchRecChecked & F)
{
  int Result = FindFirstUnchecked(Path, Attr, F);
  return FindCheck(Result, F.Path);
}
//---------------------------------------------------------------------------
// Equivalent to FindNext, just to complement to FindFirstUnchecked
int __fastcall FindNextUnchecked(TSearchRecChecked & F)
{
  return FindNext(F);
}
//---------------------------------------------------------------------------
// It can make sense to use FindNextChecked, even if unchecked FindFirst is used.
// I.e. even if we do not care that FindFirst failed, if FindNext
// fails after successful FindFirst, it means some terrible problem
int __fastcall FindNextChecked(TSearchRecChecked & F)
{
  return FindCheck(FindNextUnchecked(F), F.Path);
}
//---------------------------------------------------------------------------
bool __fastcall FileSearchRec(const UnicodeString FileName, TSearchRec & Rec)
{
  int FindAttrs = faReadOnly | faHidden | faSysFile | faDirectory | faArchive;
  bool Result = (FindFirst(ApiPath(FileName), FindAttrs, Rec) == 0);
  if (Result)
  {
    FindClose(Rec);
  }
  return Result;
}
//---------------------------------------------------------------------------
void CopySearchRec(const TSearchRec & Source, TSearchRec & Dest)
{
  // Strangely issues a compiler warning (W8111 due to TSearchRec::Time), when used in Script.cpp, but not here.
  Dest = Source;
}
//---------------------------------------------------------------------------
bool __fastcall IsRealFile(const UnicodeString & FileName)
{
  return (FileName != THISDIRECTORY) && (FileName != PARENTDIRECTORY);
}
//---------------------------------------------------------------------------
UnicodeString GetOSInfo()
{
  UnicodeString Result = WindowsVersionLong();
  AddToList(Result, WindowsProductName(), TitleSeparator);
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString GetEnvironmentInfo()
{
  UnicodeString Result = FORMAT(L"WinSCP %s (OS %s)", (Configuration->VersionStr, GetOSInfo()));
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall ProcessLocalDirectory(UnicodeString DirName,
  TProcessLocalFileEvent CallBackFunc, void * Param,
  int FindAttrs)
{
  DebugAssert(CallBackFunc);
  if (FindAttrs < 0)
  {
    FindAttrs = faReadOnly | faHidden | faSysFile | faDirectory | faArchive;
  }

  TSearchRecOwned SearchRec;
  if (FindFirstChecked(CombinePaths(DirName, AnyMask), FindAttrs, SearchRec) == 0)
  {
    do
    {
      if (SearchRec.IsRealFile())
      {
        CallBackFunc(SearchRec.GetFilePath(), SearchRec, Param);
      }

    } while (FindNextChecked(SearchRec) == 0);
  }
}
//---------------------------------------------------------------------------
int __fastcall FileGetAttrFix(const UnicodeString & FileName)
{
  // Already called with ApiPath

  int Result;
  int Tries = 2;
  do
  {
    // WORKAROUND:
    // FileGetAttr when called for link with FollowLink set (default) will always fail on pre-Vista
    // as it calls InternalGetFileNameFromSymLink, which test for CheckWin32Version(6, 0)
    Result = GetFileAttributes(FileName.c_str());
    if ((Result >= 0) && FLAGSET(Result, faSymLink))
    {
      try
      {
        UnicodeString TargetName;
        // WORKAROUND:
        // On Samba, InternalGetFileNameFromSymLink fails and returns true but empty target.
        // That confuses FileGetAttr, which returns attributes of the parent folder instead.
        // Using FileGetSymLinkTarget solves the problem, as it returns false.
        if (!FileGetSymLinkTarget(FileName, TargetName))
        {
          // FileGetAttr would return faInvalid (-1), but we want to allow an upload from Samba,
          // so returning the symlink attributes => noop
        }
        else
        {
          Result = GetFileAttributes(ApiPath(TargetName).c_str());
        }
      }
      catch (EOSError & E)
      {
        Result = -1;
      }
      catch (EDirectoryNotFoundException & E) // throws by FileSystemAttributes
      {
        Result = -1;
      }
    }
    Tries--;
  }
  // When referring to files in some special symlinked locations
  // (like a deduplicated drive or a commvault archive), the first call to FileGetAttr failed.
  // Possibly this issue is resolved by our re-implementation of FileGetAttr above.
  // But as we have no way to test it, keeping the retry here.
  while ((Result < 0) && (Tries > 0));

  return Result;
}
//---------------------------------------------------------------------------
TDateTime __fastcall EncodeDateVerbose(Word Year, Word Month, Word Day)
{
  try
  {
    TDateTime DateTime = EncodeDate(Year, Month, Day);
    return DateTime;
  }
  catch (EConvertError & E)
  {
    throw EConvertError(FORMAT(L"%s [%d-%d-%d]", (E.Message, int(Year), int(Month), int(Day))));
  }
}
//---------------------------------------------------------------------------
TDateTime __fastcall EncodeTimeVerbose(Word Hour, Word Min, Word Sec, Word MSec)
{
  try
  {
    TDateTime DateTime = EncodeTime(Hour, Min, Sec, MSec);
    return DateTime;
  }
  catch (EConvertError & E)
  {
    throw EConvertError(FORMAT(L"%s [%d:%d:%d.%d]", (E.Message, int(Hour), int(Min), int(Sec), int(MSec))));
  }
}
//---------------------------------------------------------------------------
TDateTime __fastcall SystemTimeToDateTimeVerbose(const SYSTEMTIME & SystemTime)
{
  try
  {
    TDateTime DateTime = SystemTimeToDateTime(SystemTime);
    return DateTime;
  }
  catch (EConvertError & E)
  {
    throw EConvertError(FORMAT(L"%s [%d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d.%3.3d]", (E.Message, int(SystemTime.wYear), int(SystemTime.wMonth), int(SystemTime.wDay), int(SystemTime.wHour), int(SystemTime.wMinute), int(SystemTime.wSecond), int(SystemTime.wMilliseconds))));
  }
}
//---------------------------------------------------------------------------
struct TDateTimeParams
{
  TDateTime UnixEpoch;
  double BaseDifference;
  long BaseDifferenceSec;
  // All Current* are actually global, not per-year and
  // are valid for Year 0 (current) only
  double CurrentDaylightDifference;
  long CurrentDaylightDifferenceSec;
  double CurrentDifference;
  long CurrentDifferenceSec;
  double StandardDifference;
  long StandardDifferenceSec;
  double DaylightDifference;
  long DaylightDifferenceSec;
  SYSTEMTIME SystemStandardDate;
  SYSTEMTIME SystemDaylightDate;
  TDateTime StandardDate;
  TDateTime DaylightDate;
  UnicodeString StandardName;
  UnicodeString DaylightName;
  // This is actually global, not per-year
  bool DaylightHack;

  bool HasDST() const
  {
    // On some systems it occurs that StandardDate is unset, while
    // DaylightDate is set. MSDN states that this is invalid and
    // should be treated as if there is no daylight saving.
    // So check both.
    return
      (SystemStandardDate.wMonth != 0) &&
      (SystemDaylightDate.wMonth != 0);
  }

  bool SummerDST() const
  {
    return HasDST() && (DaylightDate < StandardDate);
  }
};
typedef std::map<int, TDateTimeParams> TYearlyDateTimeParams;
static TYearlyDateTimeParams YearlyDateTimeParams;
static std::unique_ptr<TCriticalSection> DateTimeParamsSection(TraceInitPtr(new TCriticalSection()));
static void __fastcall EncodeDSTMargin(const SYSTEMTIME & Date, unsigned short Year,
  TDateTime & Result);
//---------------------------------------------------------------------------
static unsigned short __fastcall DecodeYear(const TDateTime & DateTime)
{
  unsigned short Year, Month, Day;
  DecodeDate(DateTime, Year, Month, Day);
  return Year;
}
//---------------------------------------------------------------------------
static const TDateTimeParams * __fastcall GetDateTimeParams(unsigned short Year)
{
  TGuard Guard(DateTimeParamsSection.get());

  TDateTimeParams * Result;

  TYearlyDateTimeParams::iterator i = YearlyDateTimeParams.find(Year);
  if (i != YearlyDateTimeParams.end())
  {
    Result = &(*i).second;
  }
  else
  {
    // creates new entry as a side effect
    Result = &YearlyDateTimeParams[Year];
    TIME_ZONE_INFORMATION TZI;

    unsigned long GTZI;

    HINSTANCE Kernel32 = GetModuleHandle(kernel32);
    typedef BOOL WINAPI (* TGetTimeZoneInformationForYear)(USHORT wYear, PDYNAMIC_TIME_ZONE_INFORMATION pdtzi, LPTIME_ZONE_INFORMATION ptzi);
    TGetTimeZoneInformationForYear GetTimeZoneInformationForYear =
      (TGetTimeZoneInformationForYear)GetProcAddress(Kernel32, "GetTimeZoneInformationForYear");

    if ((Year == 0) || (GetTimeZoneInformationForYear == NULL))
    {
      GTZI = GetTimeZoneInformation(&TZI);
    }
    else
    {
      GetTimeZoneInformationForYear(Year, NULL, &TZI);
      GTZI = TIME_ZONE_ID_UNKNOWN;
    }

    switch (GTZI)
    {
      case TIME_ZONE_ID_UNKNOWN:
        Result->CurrentDaylightDifferenceSec = 0;
        break;

      case TIME_ZONE_ID_STANDARD:
        Result->CurrentDaylightDifferenceSec = TZI.StandardBias;
        break;

      case TIME_ZONE_ID_DAYLIGHT:
        Result->CurrentDaylightDifferenceSec = TZI.DaylightBias;
        break;

      case TIME_ZONE_ID_INVALID:
      default:
        throw Exception(TIMEZONE_ERROR);
    }

    Result->BaseDifferenceSec = TZI.Bias;
    Result->BaseDifference = double(TZI.Bias) / MinsPerDay;
    Result->BaseDifferenceSec *= SecsPerMin;

    Result->CurrentDifferenceSec = TZI.Bias +
      Result->CurrentDaylightDifferenceSec;
    Result->CurrentDifference =
      double(Result->CurrentDifferenceSec) / MinsPerDay;
    Result->CurrentDifferenceSec *= SecsPerMin;

    Result->CurrentDaylightDifference =
      double(Result->CurrentDaylightDifferenceSec) / MinsPerDay;
    Result->CurrentDaylightDifferenceSec *= SecsPerMin;

    Result->DaylightDifferenceSec = TZI.DaylightBias * SecsPerMin;
    Result->DaylightDifference = double(TZI.DaylightBias) / MinsPerDay;
    Result->StandardDifferenceSec = TZI.StandardBias * SecsPerMin;
    Result->StandardDifference = double(TZI.StandardBias) / MinsPerDay;

    Result->SystemStandardDate = TZI.StandardDate;
    Result->SystemDaylightDate = TZI.DaylightDate;

    unsigned short AYear = (Year != 0) ? Year : DecodeYear(Now());
    if (Result->HasDST())
    {
      EncodeDSTMargin(Result->SystemStandardDate, AYear, Result->StandardDate);
      EncodeDSTMargin(Result->SystemDaylightDate, AYear, Result->DaylightDate);
    }

    Result->StandardName = TZI.StandardName;
    Result->DaylightName = TZI.DaylightName;

    Result->DaylightHack = !IsWin7();
  }

  return Result;
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
    // Day 5 means, the last occurence of day-of-week in month
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

  const TDateTimeParams * Params = GetDateTimeParams(DecodeYear(DateTime));

  bool Result;

  // On some systems it occurs that StandardDate is unset, while
  // DaylightDate is set. MSDN states that this is invalid and
  // should be treated as if there is no daylight saving.
  // So check both.
  if (!Params->HasDST())
  {
    Result = false;
  }
  else
  {

    if (Params->SummerDST())
    {
      Result =
        (DateTime >= Params->DaylightDate) &&
        (DateTime < Params->StandardDate);
    }
    else
    {
      Result =
        (DateTime < Params->StandardDate) ||
        (DateTime >= Params->DaylightDate);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall UsesDaylightHack()
{
  return GetDateTimeParams(0)->DaylightHack;
}
//---------------------------------------------------------------------------
TDateTime __fastcall UnixToDateTime(__int64 TimeStamp, TDSTMode DSTMode)
{
  DebugAssert(int(EncodeDateVerbose(1970, 1, 1)) == UnixDateDelta);

  TDateTime Result = UnixDateDelta + (double(TimeStamp) / SecsPerDay);

  const TDateTimeParams * Params = GetDateTimeParams(DecodeYear(Result));

  if (Params->DaylightHack)
  {
    if ((DSTMode == dstmWin) || (DSTMode == dstmUnix))
    {
      const TDateTimeParams * CurrentParams = GetDateTimeParams(0);
      Result -= CurrentParams->CurrentDifference;
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
    Result -= DSTDifferenceForTime(Result);
  }

  return Result;
}
//---------------------------------------------------------------------------
__int64 __fastcall Round(double Number)
{
  double Floor = floor(Number);
  double Ceil = ceil(Number);
  return static_cast<__int64>(((Number - Floor) > (Ceil - Number)) ? Ceil : Floor);
}
//---------------------------------------------------------------------------
bool __fastcall TryRelativeStrToDateTime(UnicodeString S, TDateTime & DateTime, bool Add)
{
  S = S.Trim();
  int Index = 1;
  if (SameText(S, L"today"))
  {
    S = L"0DS";
  }
  else if (SameText(S, L"yesterday"))
  {
    S = L"1DS";
  }

  while ((Index <= S.Length()) && IsDigit(S[Index]))
  {
    Index++;
  }
  UnicodeString NumberStr = S.SubString(1, Index - 1);
  int Number;
  bool Result = TryStrToInt(NumberStr, Number);
  if (Result)
  {
    if (!Add)
    {
      Number = -Number;
    }
    S.Delete(1, Index - 1);
    S = S.Trim().UpperCase();
    bool Start = (S.Length() == 2) && (S[2] == L'S');
    if (Start)
    {
      S.SetLength(S.Length() - 1);
    }
    DateTime = Now();
    // These may not overlap with ParseSize (K, M and G)
    if (S == L"S")
    {
      DateTime = IncSecond(DateTime, Number);
      if (Start)
      {
        DateTime = IncMilliSecond(DateTime, -static_cast<int>(MilliSecondOfTheSecond(DateTime)));
      }
    }
    else if (S == L"N")
    {
      DateTime = IncMinute(DateTime, Number);
      if (Start)
      {
        DateTime = IncMilliSecond(DateTime, -static_cast<int>(MilliSecondOfTheMinute(DateTime)));
      }
    }
    else if (S == L"H")
    {
      DateTime = IncHour(DateTime, Number);
      if (Start)
      {
        DateTime = IncMilliSecond(DateTime, -static_cast<int>(MilliSecondOfTheHour(DateTime)));
      }
    }
    else if (S == L"D")
    {
      DateTime = IncDay(DateTime, Number);
      if (Start)
      {
        DateTime = IncMilliSecond(DateTime, -static_cast<int>(MilliSecondOfTheDay(DateTime)));
      }
    }
    else if (S == L"Y")
    {
      DateTime = IncYear(DateTime, Number);
      if (Start)
      {
        DateTime = IncMilliSecond(DateTime, -MilliSecondOfTheYear(DateTime));
      }
    }
    else
    {
      Result = false;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool TryStrToDateTimeStandard(const UnicodeString & S, TDateTime & Value)
{
  TFormatSettings FormatSettings = TFormatSettings::Create(GetDefaultLCID());
  FormatSettings.DateSeparator = L'-';
  FormatSettings.TimeSeparator = L':';
  FormatSettings.ShortDateFormat = "yyyy/mm/dd";
  FormatSettings.ShortTimeFormat = "hh:nn:ss";

  return TryStrToDateTime(S, Value, FormatSettings);
}
//---------------------------------------------------------------------------
const wchar_t KiloSize = L'K';
const wchar_t MegaSize = L'M';
const wchar_t GigaSize = L'G';
//---------------------------------------------------------------------------
// Keep consistent with parse_blocksize
bool __fastcall TryStrToSize(UnicodeString SizeStr, __int64 & Size)
{
  int Index = 0;
  while ((Index + 1 <= SizeStr.Length()) && IsDigit(SizeStr[Index + 1]))
  {
    Index++;
  }
  bool Result =
    (Index > 0) && TryStrToInt64(SizeStr.SubString(1, Index), Size);
  if (Result)
  {
    SizeStr = SizeStr.SubString(Index + 1, SizeStr.Length() - Index).Trim();
    if (!SizeStr.IsEmpty())
    {
      Result = (SizeStr.Length() == 1);
      if (Result)
      {
        wchar_t Unit = towupper(SizeStr[1]);
        switch (Unit)
        {
          case GigaSize:
            Size *= 1024;
            // fallthru
          case MegaSize:
            Size *= 1024;
            // fallthru
          case KiloSize:
            Size *= 1024;
            break;
          default:
            Result = false;
        }
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall SizeToStr(__int64 Size)
{
  UnicodeString Result;
  if ((Size <= 0) || ((Size % 1024) != 0))
  {
    Result = IntToStr(Size);
  }
  else
  {
    Size /= 1024;
    if ((Size % 1024) != 0)
    {
      Result = IntToStr(Size) + KiloSize;
    }
    else
    {
      Size /= 1024;
      if ((Size % 1024) != 0)
      {
        Result = IntToStr(Size) + MegaSize;
      }
      else
      {
        Size /= 1024;
        Result = IntToStr(Size) + GigaSize;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
static __int64 __fastcall DateTimeToUnix(const TDateTime DateTime)
{
  const TDateTimeParams * CurrentParams = GetDateTimeParams(0);

  DebugAssert(int(EncodeDateVerbose(1970, 1, 1)) == UnixDateDelta);

  return Round(double(DateTime - UnixDateDelta) * SecsPerDay) +
    CurrentParams->CurrentDifferenceSec;
}
//---------------------------------------------------------------------------
FILETIME __fastcall DateTimeToFileTime(const TDateTime DateTime,
  TDSTMode /*DSTMode*/)
{
  __int64 UnixTimeStamp = ::DateTimeToUnix(DateTime);

  const TDateTimeParams * Params = GetDateTimeParams(DecodeYear(DateTime));
  if (!Params->DaylightHack)
  {
    // We should probably use reversed code of FileTimeToDateTime here instead of custom implementation

    // We are incrementing and decrementing BaseDifferenceSec because it
    // can actually change between years
    // (as it did in Belarus from GMT+2 to GMT+3 between 2011 and 2012)

    UnixTimeStamp += (IsDateInDST(DateTime) ?
      Params->DaylightDifferenceSec : Params->StandardDifferenceSec) +
      Params->BaseDifferenceSec;

    const TDateTimeParams * CurrentParams = GetDateTimeParams(0);
    UnixTimeStamp -=
      CurrentParams->CurrentDaylightDifferenceSec +
      CurrentParams->BaseDifferenceSec;

  }

  FILETIME Result;
  (*(__int64*)&(Result) = (__int64(UnixTimeStamp) + 11644473600LL) * 10000000LL);

  return Result;
}
//---------------------------------------------------------------------------
// This can be replaced with TSearchRec.TimeStamp
TDateTime __fastcall FileTimeToDateTime(const FILETIME & FileTime)
{
  // duplicated in DirView.pas
  TDateTime Result;
  // The 0xFFF... is sometimes seen for invalid timestamps,
  // it would cause failure in SystemTimeToDateTime below
  if (FileTime.dwLowDateTime == std::numeric_limits<DWORD>::max())
  {
    Result = MinDateTime;
  }
  else
  {
    SYSTEMTIME SysTime;
    if (!UsesDaylightHack())
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
    Result = SystemTimeToDateTimeVerbose(SysTime);
  }
  return Result;
}
//---------------------------------------------------------------------------
__int64 __fastcall ConvertTimestampToUnix(const FILETIME & FileTime,
  TDSTMode DSTMode)
{
  __int64 Result = ((*(const __int64*)&(FileTime)) / 10000000LL - 11644473600LL);

  if (UsesDaylightHack())
  {
    if ((DSTMode == dstmUnix) || (DSTMode == dstmKeep))
    {
      FILETIME LocalFileTime;
      SYSTEMTIME SystemTime;
      FileTimeToLocalFileTime(&FileTime, &LocalFileTime);
      FileTimeToSystemTime(&LocalFileTime, &SystemTime);
      TDateTime DateTime = SystemTimeToDateTimeVerbose(SystemTime);
      const TDateTimeParams * Params = GetDateTimeParams(DecodeYear(DateTime));
      Result += (IsDateInDST(DateTime) ?
        Params->DaylightDifferenceSec : Params->StandardDifferenceSec);

      if (DSTMode == dstmKeep)
      {
        const TDateTimeParams * CurrentParams = GetDateTimeParams(0);
        Result -= CurrentParams->CurrentDaylightDifferenceSec;
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
      TDateTime DateTime = SystemTimeToDateTimeVerbose(SystemTime);
      const TDateTimeParams * Params = GetDateTimeParams(DecodeYear(DateTime));
      Result -= (IsDateInDST(DateTime) ?
        Params->DaylightDifferenceSec : Params->StandardDifferenceSec);
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
TDateTime __fastcall ConvertTimestampToUTC(TDateTime DateTime)
{

  const TDateTimeParams * Params = GetDateTimeParams(DecodeYear(DateTime));
  DateTime += DSTDifferenceForTime(DateTime);
  DateTime += Params->BaseDifference;

  if (Params->DaylightHack)
  {
    const TDateTimeParams * CurrentParams = GetDateTimeParams(0);
    DateTime += CurrentParams->CurrentDaylightDifference;
  }

  return DateTime;
}
//---------------------------------------------------------------------------
TDateTime __fastcall ConvertTimestampFromUTC(TDateTime DateTime)
{

  const TDateTimeParams * Params = GetDateTimeParams(DecodeYear(DateTime));
  DateTime -= DSTDifferenceForTime(DateTime);
  DateTime -= Params->BaseDifference;

  if (Params->DaylightHack)
  {
    const TDateTimeParams * CurrentParams = GetDateTimeParams(0);
    DateTime -= CurrentParams->CurrentDaylightDifference;
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
    Result = ::DateTimeToUnix(Now());
  }
  else
  {
    Result = ConvertTimestampToUnix(FileTime, DSTMode);
  }
  return Result;
}
//---------------------------------------------------------------------------
double __fastcall DSTDifferenceForTime(TDateTime DateTime)
{
  double Result;
  const TDateTimeParams * Params = GetDateTimeParams(DecodeYear(DateTime));
  if (IsDateInDST(DateTime))
  {
    Result = Params->DaylightDifference;
  }
  else
  {
    Result = Params->StandardDifference;
  }
  return Result;
}
//---------------------------------------------------------------------------
TDateTime __fastcall AdjustDateTimeFromUnix(TDateTime DateTime, TDSTMode DSTMode)
{
  const TDateTimeParams * Params = GetDateTimeParams(DecodeYear(DateTime));

  if (Params->DaylightHack)
  {
    if ((DSTMode == dstmWin) || (DSTMode == dstmUnix))
    {
      const TDateTimeParams * CurrentParams = GetDateTimeParams(0);
      DateTime = DateTime - CurrentParams->CurrentDaylightDifference;
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
      DateTime = DateTime + DSTDifferenceForTime(DateTime);
    }
  }

  return DateTime;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FixedLenDateTimeFormat(const UnicodeString & Format)
{
  UnicodeString Result = Format;
  bool AsIs = false;

  int Index = 1;
  while (Index <= Result.Length())
  {
    wchar_t F = Result[Index];
    if ((F == L'\'') || (F == L'\"'))
    {
      AsIs = !AsIs;
      Index++;
    }
    else if (!AsIs && ((F == L'a') || (F == L'A')))
    {
      if (Result.SubString(Index, 5).LowerCase() == L"am/pm")
      {
        Index += 5;
      }
      else if (Result.SubString(Index, 3).LowerCase() == L"a/p")
      {
        Index += 3;
      }
      else if (Result.SubString(Index, 4).LowerCase() == L"ampm")
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
      if (!AsIs && (wcschr(L"dDeEmMhHnNsS", F) != NULL) &&
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
UnicodeString __fastcall FormatTimeZone(long Sec)
{
  TTimeSpan Span = TTimeSpan::FromSeconds(Sec);
  UnicodeString Str;
  if ((Span.Seconds == 0) && (Span.Minutes == 0))
  {
    Str = FORMAT(L"%d", (-Span.Hours));
  }
  else if (Span.Seconds == 0)
  {
    Str = FORMAT(L"%d:%2.2d", (-Span.Hours, abs(Span.Minutes)));
  }
  else
  {
    Str = FORMAT(L"%d:%2.2d:%2.2d", (-Span.Hours, abs(Span.Minutes), abs(Span.Seconds)));
  }
  Str = ((Span <= TTimeSpan::Zero) ? L"+" : L"") + Str;
  return Str;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall GetTimeZoneLogString()
{
  const TDateTimeParams * CurrentParams = GetDateTimeParams(0);

  UnicodeString Result =
    FORMAT(L"Current: GMT%s", (FormatTimeZone(CurrentParams->CurrentDifferenceSec)));

  if (!CurrentParams->HasDST())
  {
    Result += FORMAT(L" (%s), No DST", (CurrentParams->StandardName));
  }
  else
  {
    Result +=
      FORMAT(L", Standard: GMT%s (%s), DST: GMT%s (%s), DST Start: %s, DST End: %s",
        (FormatTimeZone(CurrentParams->BaseDifferenceSec + CurrentParams->StandardDifferenceSec),
         CurrentParams->StandardName,
         FormatTimeZone(CurrentParams->BaseDifferenceSec + CurrentParams->DaylightDifferenceSec),
         CurrentParams->DaylightName,
         CurrentParams->DaylightDate.DateString(),
         CurrentParams->StandardDate.DateString()));
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall AdjustClockForDSTEnabled()
{
  // Windows XP deletes the DisableAutoDaylightTimeSet value when it is off
  // (the later versions set it to DynamicDaylightTimeDisabled to 0)
  bool DynamicDaylightTimeDisabled = false;
  TRegistry * Registry = new TRegistry(KEY_READ);
  try
  {
    Registry->RootKey = HKEY_LOCAL_MACHINE;
    if (Registry->OpenKey(L"SYSTEM", false) &&
        Registry->OpenKey(L"CurrentControlSet", false) &&
        Registry->OpenKey(L"Control", false) &&
        Registry->OpenKey(L"TimeZoneInformation", false))
    {
      if (Registry->ValueExists(L"DynamicDaylightTimeDisabled"))
      {
        DynamicDaylightTimeDisabled = Registry->ReadBool(L"DynamicDaylightTimeDisabled");
      }
      // WORKAROUND
      // Windows XP equivalent
      else if (Registry->ValueExists(L"DisableAutoDaylightTimeSet"))
      {
        DynamicDaylightTimeDisabled = Registry->ReadBool(L"DisableAutoDaylightTimeSet");
      }
    }
    delete Registry;
  }
  catch(...)
  {
  }
  return !DynamicDaylightTimeDisabled;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall StandardDatestamp()
{
  return FormatDateTime(L"yyyy'-'mm'-'dd", ConvertTimestampToUTC(Now()));
}
//---------------------------------------------------------------------------
UnicodeString __fastcall StandardTimestamp(const TDateTime & DateTime)
{
  return FormatDateTime(L"yyyy'-'mm'-'dd'T'hh':'nn':'ss'.'zzz'Z'", ConvertTimestampToUTC(DateTime));
}
//---------------------------------------------------------------------------
UnicodeString __fastcall StandardTimestamp()
{
  return StandardTimestamp(Now());
}
//---------------------------------------------------------------------------
static TDateTime TwoSeconds(0, 0, 2, 0);
int __fastcall CompareFileTime(TDateTime T1, TDateTime T2)
{
  // "FAT" time precision
  // (when one time is seconds-precision and other is millisecond-precision,
  // we may have times like 12:00:00.000 and 12:00:01.999, which should
  // be treated the same)
  int Result;
  if (T1 == T2)
  {
    // just optimization
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
int __fastcall TimeToMSec(TDateTime T)
{
  return int(Round(double(T) * double(MSecsPerDay)));
}
//---------------------------------------------------------------------------
int __fastcall TimeToSeconds(TDateTime T)
{
  return TimeToMSec(T) / MSecsPerSec;
}
//---------------------------------------------------------------------------
int __fastcall TimeToMinutes(TDateTime T)
{
  return TimeToSeconds(T) / SecsPerMin;
}
//---------------------------------------------------------------------------
static bool __fastcall DoRecursiveDeleteFile(
  const UnicodeString FileName, bool ToRecycleBin, UnicodeString & ErrorPath, int & Deleted)
{
  bool Result;
  Deleted = 0;

  UnicodeString AErrorPath = FileName;

  if (!ToRecycleBin)
  {
    TSearchRecChecked InitialSearchRec;
    Result = FileSearchRec(FileName, InitialSearchRec);
    if (Result)
    {
      if (!InitialSearchRec.IsDirectory())
      {
        Result = DeleteFile(ApiPath(FileName));
        if (Result)
        {
          Deleted++;
        }
      }
      else
      {
        TSearchRecOwned SearchRec;
        Result = (FindFirstUnchecked(FileName + L"\\*", faAnyFile, SearchRec) == 0);

        if (Result)
        {
          do
          {
            UnicodeString FileName2 = SearchRec.GetFilePath();
            if (SearchRec.IsDirectory())
            {
              if (SearchRec.IsRealFile())
              {
                Result = DoRecursiveDeleteFile(FileName2, DebugAlwaysFalse(ToRecycleBin), AErrorPath, Deleted);
              }
            }
            else
            {
              Result = DeleteFile(ApiPath(FileName2));
              if (!Result)
              {
                AErrorPath = FileName2;
              }
              else
              {
                Deleted++;
              }
            }
          }
          while (Result && (FindNextUnchecked(SearchRec) == 0));

          SearchRec.Close();

          if (Result)
          {
            Result = RemoveDir(ApiPath(FileName));
            if (Result)
            {
              Deleted++;
            }
          }
        }
      }
    }
  }
  else
  {
    SHFILEOPSTRUCT Data;

    memset(&Data, 0, sizeof(Data));
    Data.hwnd = NULL;
    Data.wFunc = FO_DELETE;
    // SHFileOperation does not support long paths anyway
    UnicodeString FileList(ApiPath(FileName));
    FileList.SetLength(FileList.Length() + 2);
    FileList[FileList.Length() - 1] = L'\0';
    FileList[FileList.Length()] = L'\0';
    Data.pFrom = FileList.c_str();
    Data.pTo = L"\0\0"; // this will actually give one null more than needed
    Data.fFlags = FOF_NOCONFIRMATION | FOF_RENAMEONCOLLISION | FOF_NOCONFIRMMKDIR |
      FOF_NOERRORUI | FOF_SILENT;
    if (DebugAlwaysTrue(ToRecycleBin))
    {
      Data.fFlags |= FOF_ALLOWUNDO;
    }
    int ErrorCode = SHFileOperation(&Data);
    Result = (ErrorCode == 0);
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

    if (Result)
    {
      Deleted = 1;
    }
  }

  if (!Result)
  {
    ErrorPath = AErrorPath;
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall RecursiveDeleteFile(const UnicodeString & FileName, bool ToRecycleBin)
{
  UnicodeString ErrorPath; // unused
  int Deleted;
  bool Result = DoRecursiveDeleteFile(FileName, ToRecycleBin, ErrorPath, Deleted);
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall RecursiveDeleteFileChecked(const UnicodeString & FileName, bool ToRecycleBin)
{
  UnicodeString ErrorPath;
  int Deleted;
  if (!DoRecursiveDeleteFile(FileName, ToRecycleBin, ErrorPath, Deleted))
  {
    throw EOSExtException(FMTLOAD(DELETE_LOCAL_FILE_ERROR, (ErrorPath)));
  }
  return Deleted;
}
//---------------------------------------------------------------------------
void __fastcall DeleteFileChecked(const UnicodeString & FileName)
{
  if (!DeleteFile(ApiPath(FileName)))
  {
    throw EOSExtException(FMTLOAD(DELETE_LOCAL_FILE_ERROR, (FileName)));
  }
}
//---------------------------------------------------------------------------
unsigned int __fastcall CancelAnswer(unsigned int Answers)
{
  unsigned int Result;
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
    DebugFail();
    Result = qaCancel;
  }
  return Result;
}
//---------------------------------------------------------------------------
unsigned int __fastcall AbortAnswer(unsigned int Answers)
{
  unsigned int Result;
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
unsigned int __fastcall ContinueAnswer(unsigned int Answers)
{
  unsigned int Result;
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
TLibModule * __fastcall FindModule(void * Instance)
{
  TLibModule * CurModule;
  CurModule = reinterpret_cast<TLibModule*>(LibModuleList);

  while (CurModule)
  {
    if (CurModule->Instance == (unsigned)Instance)
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
static UnicodeString __fastcall DoLoadStrFrom(HINSTANCE Module, int Ident, unsigned int MaxLength)
{
  UnicodeString Result;
  Result.SetLength(MaxLength);
  int Length = LoadString(Module, Ident, Result.c_str(), MaxLength);
  Result.SetLength(Length);

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall LoadStrFrom(HINSTANCE Module, int Ident)
{
  // 1024 = what VCL LoadStr limits the string to
  return DoLoadStrFrom(Module, Ident, 1024);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall LoadStr(int Ident, unsigned int MaxLength)
{
  TLibModule * MainModule = FindModule(HInstance);
  DebugAssert(MainModule != NULL);
  return DoLoadStrFrom((HINSTANCE)MainModule->ResInstance, Ident, MaxLength);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall LoadStrPart(int Ident, int Part)
{
  UnicodeString Result;
  UnicodeString Str = LoadStr(Ident);

  while (Part > 0)
  {
    Result = CutToChar(Str, L'|', false);
    Part--;
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall DecodeUrlChars(UnicodeString S)
{
  int i = 1;
  while (i <= S.Length())
  {
    switch (S[i])
    {
      case L'+':
        S[i] = ' ';
        break;

      case L'%':
        {
          UnicodeString Hex;
          while ((i + 2 <= S.Length()) && (S[i] == L'%') &&
                 IsHex(S[i + 1]) && IsHex(S[i + 2]))
          {
            Hex += S.SubString(i + 1, 2);
            S.Delete(i, 3);
          }

          if (!Hex.IsEmpty())
          {
            RawByteString Bytes = HexToBytes(Hex);
            UnicodeString Chars(UTF8ToString(Bytes));
            S.Insert(Chars, i);
            i += Chars.Length() - 1;
          }
        }
        break;
    }
    i++;
  }
  return S;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall DoEncodeUrl(UnicodeString S, const UnicodeString & DoNotEncode)
{
  int Index = 1;
  while (Index <= S.Length())
  {
    wchar_t C = S[Index];
    if (IsLetter(C) ||
        IsDigit(C) ||
        (C == L'_') || (C == L'-') || (C == L'.') || (C == L'*') ||
        (DoNotEncode.Pos(C) > 0))
    {
      Index++;
    }
    else
    {
      UTF8String UtfS(S.SubString(Index, 1));
      UnicodeString H;
      // BytesToHex with separator would do the same
      for (int Index2 = 1; Index2 <= UtfS.Length(); Index2++)
      {
        H += L"%" + ByteToHex(static_cast<unsigned char>(UtfS[Index2]));
      }
      S.Delete(Index, 1);
      S.Insert(H, Index);
      Index += H.Length();
    }
  }
  return S;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall EncodeUrlString(UnicodeString S)
{
  return DoEncodeUrl(S, UnicodeString());
}
//---------------------------------------------------------------------------
UnicodeString __fastcall EncodeUrlPath(UnicodeString S)
{
  return DoEncodeUrl(S, L"/");
}
//---------------------------------------------------------------------------
UnicodeString __fastcall AppendUrlParams(UnicodeString AURL, UnicodeString Params)
{
  // see also TWebHelpSystem::ShowHelp
  const wchar_t FragmentSeparator = L'#';
  UnicodeString URL = CutToChar(AURL, FragmentSeparator, false);

  if (URL.Pos(L"?") == 0)
  {
    URL += L"?";
  }
  else
  {
    URL += L"&";
  }

  URL += Params;

  AddToList(URL, AURL, FragmentSeparator);

  return URL;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ExtractFileNameFromUrl(const UnicodeString & Url)
{
  UnicodeString Result = Url;
  int P = Result.Pos(L"?");
  if (P > 0)
  {
    Result.SetLength(P - 1);
  }
  P = Result.LastDelimiter("/");
  if (DebugAlwaysTrue(P > 0))
  {
    Result.Delete(1, P);
  }
  return Result;
}
//---------------------------------------------------------------------
bool IsDomainOrSubdomain(const UnicodeString & FullDomain, const UnicodeString & Domain)
{
  return
    SameText(FullDomain, Domain) ||
    EndsText(L"." + Domain, FullDomain);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall EscapeHotkey(const UnicodeString & Caption)
{
  return ReplaceStr(Caption, L"&", L"&&");
}
//---------------------------------------------------------------------------
// duplicated in console's Main.cpp
static bool __fastcall DoCutToken(UnicodeString & Str, UnicodeString & Token,
  UnicodeString * RawToken, UnicodeString * Separator, bool EscapeQuotesInQuotesOnly)
{
  bool Result;

  Token = L"";

  // inspired by Putty's sftp_getcmd() from PSFTP.C
  int Index = 1;
  while ((Index <= Str.Length()) &&
    ((Str[Index] == L' ') || (Str[Index] == L'\t')))
  {
    Index++;
  }

  if (Index <= Str.Length())
  {
    bool Quoting = false;

    while (Index <= Str.Length())
    {
      if (!Quoting && ((Str[Index] == L' ') || (Str[Index] == L'\t')))
      {
        break;
      }
      // With EscapeQuotesInQuotesOnly we escape quotes only within quotes
      // otherwise the "" means " (quote), but it should mean empty string.
      else if ((Str[Index] == L'"') && (Index + 1 <= Str.Length()) &&
        (Str[Index + 1] == L'"') && (!EscapeQuotesInQuotesOnly || Quoting))
      {
        Index += 2;
        Token += L'"';
      }
      else if (Str[Index] == L'"')
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

    if (RawToken != NULL)
    {
      (*RawToken) = Str.SubString(1, Index - 1);
    }

    if (Index <= Str.Length())
    {
      if (Separator != NULL)
      {
        *Separator = Str.SubString(Index, 1);
      }
      Index++;
    }
    else
    {
      if (Separator != NULL)
      {
        *Separator = UnicodeString();
      }
    }

    Str = Str.SubString(Index, Str.Length());

    Result = true;
  }
  else
  {
    Result = false;
    Str = L"";
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall CutToken(UnicodeString & Str, UnicodeString & Token,
  UnicodeString * RawToken, UnicodeString * Separator)
{
  return DoCutToken(Str, Token, RawToken, Separator, false);
}
//---------------------------------------------------------------------------
bool __fastcall CutTokenEx(UnicodeString & Str, UnicodeString & Token,
  UnicodeString * RawToken, UnicodeString * Separator)
{
  return DoCutToken(Str, Token, RawToken, Separator, true);
}
//---------------------------------------------------------------------------
void __fastcall AddToList(UnicodeString & List, const UnicodeString & Value, const UnicodeString & Delimiter)
{
  if (!Value.IsEmpty())
  {
    if (!List.IsEmpty() &&
        ((List.Length() < Delimiter.Length()) ||
         (List.SubString(List.Length() - Delimiter.Length() + 1, Delimiter.Length()) != Delimiter)))
    {
      List += Delimiter;
    }
    List += Value;
  }
}
//---------------------------------------------------------------------------
void AddToShellFileListCommandLine(UnicodeString & List, const UnicodeString & Value)
{
  UnicodeString Arg = ShellQuoteStr(Value);
  AddToList(List, Arg, L" ");
}
//---------------------------------------------------------------------------
bool IsWin64()
{
  static int Result = -1;
  if (Result < 0)
  {
    Result = 0;
    BOOL Wow64Process = FALSE;
    if (IsWow64Process(GetCurrentProcess(), &Wow64Process))
    {
      if (Wow64Process)
      {
        Result = 1;
      }
    }
  }

  return (Result > 0);
}
//---------------------------------------------------------------------------
bool __fastcall IsWin7()
{
  return CheckWin32Version(6, 1);
}
//---------------------------------------------------------------------------
bool __fastcall IsWin8()
{
  return CheckWin32Version(6, 2);
}
//---------------------------------------------------------------------------
bool __fastcall IsWin10()
{
  return CheckWin32Version(10, 0);
}
//---------------------------------------------------------------------------
bool IsWin10Build(int BuildNumber)
{
  // It might be enough to check the dwBuildNumber, as we do in TWinConfiguration::IsDDExtBroken()
  return IsWin10() && (Win32BuildNumber() >= BuildNumber);
}
//---------------------------------------------------------------------------
bool IsWin11()
{
  return IsWin10Build(22000);
}
//---------------------------------------------------------------------------
bool __fastcall IsWine()
{
  HMODULE NtDll = GetModuleHandle(L"ntdll.dll");
  return
    DebugAlwaysTrue(NtDll != NULL) &&
    (GetProcAddress(NtDll, "wine_get_version") != NULL);
}
//---------------------------------------------------------------------------
static int GIsUWP = -1;
static UnicodeString GPackageName;
//---------------------------------------------------------------------------
void EnableUWPTestMode()
{
  GIsUWP = 1;
  AppLog(L"UWP test mode");
}
//---------------------------------------------------------------------------
static void NeedUWPData()
{
  if (GIsUWP < 0)
  {
    GIsUWP = 0;

    HINSTANCE Kernel32 = GetModuleHandle(kernel32);
    typedef LONG WINAPI (* GetCurrentPackageFamilyNameProc)(UINT32 * packageFamilyNameLength, PWSTR packageFamilyName);
    GetCurrentPackageFamilyNameProc GetCurrentPackageFamilyName =
      (GetCurrentPackageFamilyNameProc)GetProcAddress(Kernel32, "GetCurrentPackageFamilyName");
    UINT32 NameLen = 0;
    if ((GetCurrentPackageFamilyName != NULL) &&
        (GetCurrentPackageFamilyName(&NameLen, NULL) == ERROR_INSUFFICIENT_BUFFER))
    {
      GIsUWP = 1;
      AppLog(L"Is UWP application");
      GPackageName.SetLength(NameLen);
      if (GetCurrentPackageFamilyName(&NameLen, GPackageName.c_str()) == ERROR_SUCCESS)
      {
        PackStr(GPackageName);
      }
      else
      {
        GPackageName = L"err";
      }
      AppLogFmt(L"Package name: %s", (GPackageName));
    }
    else
    {
      AppLog(L"Is not UWP application");
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall IsUWP()
{
  NeedUWPData();
  return (GIsUWP > 0);
}
//---------------------------------------------------------------------------
UnicodeString GetPackageName()
{
  NeedUWPData();
  return GPackageName;
}
//---------------------------------------------------------------------------
bool IsOfficialPackage()
{
  return (GetPackageName() == OfficialPackage);
}
//---------------------------------------------------------------------------
LCID __fastcall GetDefaultLCID()
{
  return GetUserDefaultLCID();
}
//---------------------------------------------------------------------------
static UnicodeString ADefaultEncodingName;
UnicodeString __fastcall DefaultEncodingName()
{
  if (ADefaultEncodingName.IsEmpty())
  {
    CPINFOEX Info;
    GetCPInfoEx(CP_ACP, 0, &Info);
    ADefaultEncodingName = Info.CodePageName;
  }
  return ADefaultEncodingName;
}
//---------------------------------------------------------------------------
DWORD GetWindowsProductType()
{
  DWORD Result = 0;
  GetProductInfo(Win32MajorVersion(), Win32MinorVersion(), 0, 0, &Result);
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall WindowsProductName()
{
  UnicodeString Result;
  // On Windows 11 "ProductName" is still "Windows 10"
  if (IsWin11())
  {
    Result = L"Windows 11"; // fallback value

    HMODULE WinBrandLib = LoadLibrary(L"winbrand.dll");
    if (WinBrandLib != NULL)
    {
      typedef LPWSTR WINAPI (* TBrandingFormatString)(LPCWSTR);
      TBrandingFormatString BrandingFormatString =
        reinterpret_cast<TBrandingFormatString>(GetProcAddress(WinBrandLib, "BrandingFormatString"));
      if (BrandingFormatString != NULL)
      {
        LPWSTR Brand = BrandingFormatString(L"%WINDOWS_LONG%");
        if (Brand != NULL)
        {
          Result = Brand;
          GlobalFree(Brand);
        }
      }
      FreeLibrary(WinBrandLib);
    }
  }
  else
  {
    try
    {
      // JCL GetWindowsProductName claims that reading 64-bit key gets correct values, but on Windows 11, neither works
      unsigned int Access = KEY_READ | FLAGMASK(IsWin64(), KEY_WOW64_64KEY);
      std::unique_ptr<TRegistry> Registry(new TRegistry(Access));
      Registry->RootKey = HKEY_LOCAL_MACHINE;
      if (Registry->OpenKey(L"SOFTWARE", false) &&
          Registry->OpenKey(L"Microsoft", false) &&
          Registry->OpenKey(L"Windows NT", false) &&
          Registry->OpenKey(L"CurrentVersion", false))
      {
        Result = Registry->ReadString(L"ProductName");
      }
    }
    catch(...)
    {
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall WindowsVersion()
{
  UnicodeString Result = FORMAT(L"%d.%d.%d", (Win32MajorVersion(), Win32MinorVersion(), Win32BuildNumber()));
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall WindowsVersionLong()
{
  UnicodeString Result = WindowsVersion();
  AddToList(Result, Win32CSDVersion(), L" ");
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall IsDirectoryWriteable(const UnicodeString & Path)
{
  UnicodeString FileName =
    IncludeTrailingPathDelimiter(Path) +
    FORMAT(L"wscp_%s_%d.tmp", (FormatDateTime(L"nnzzz", Now()), int(GetCurrentProcessId())));
  HANDLE Handle = CreateFile(ApiPath(FileName).c_str(), GENERIC_READ | GENERIC_WRITE, 0, NULL,
    CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY | FILE_FLAG_DELETE_ON_CLOSE, 0);
  bool Result = (Handle != INVALID_HANDLE_VALUE);
  if (Result)
  {
    CloseHandle(Handle);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FormatNumber(__int64 Number)
{
  return FormatFloat(L"#,##0", Number);
}
//---------------------------------------------------------------------------
// simple alternative to FormatBytes
UnicodeString __fastcall FormatSize(__int64 Size)
{
  return FormatNumber(Size);
}
//---------------------------------------------------------------------------
UnicodeString FormatDateTimeSpan(const TDateTime & DateTime)
{
  UnicodeString Result;
  if ((TDateTime() <= DateTime) && (DateTime <= MaxDateTime))
  {
    TTimeStamp TimeStamp = DateTimeToTimeStamp(DateTime);
    int Days = TimeStamp.Date - DateDelta;
    if (abs(Days) >= 4)
    {
      Result = FMTLOAD(DAYS_SPAN, (Days));
    }
    else
    {
      unsigned short Hour, Min, Sec, Dummy;
      DecodeTime(DateTime, Hour, Min, Sec, Dummy);
      int TotalHours = static_cast<int>(Hour) + (Days * HoursPerDay);
      Result = FORMAT(L"%d%s%.2d%s%.2d", (TotalHours, FormatSettings.TimeSeparator, Min, FormatSettings.TimeSeparator, Sec));
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString FormatRelativeTime(const TDateTime & ANow, const TDateTime & AThen, bool DateOnly)
{
  UnicodeString Result;
  if (DateOnly)
  {
    if (IsSameDay(AThen, ANow - 1))
    {
      Result = LoadStrPart(TIME_RELATIVE, 3);
    }
    else if (IsSameDay(AThen, ANow))
    {
      Result = LoadStrPart(TIME_RELATIVE, 2);
    }
  }

  if (Result.IsEmpty())
  {
    int Part, Num;

    Num = YearsBetween(ANow, AThen);
    if (Num > 1)
    {
      Part = 18;
    }
    else if (Num == 1)
    {
      Part = 17;
    }
    else
    {
      Num = MonthsBetween(ANow, AThen);
      if (Num > 1)
      {
        Part = 16;
      }
      else if (Num == 1)
      {
        Part = 15;
      }
      else
      {
        Num = DaysBetween(ANow, AThen);
        if (Num > 1)
        {
          Part = 12;
        }
        else if (Num == 1)
        {
          Part = 11;
        }
        else
        {
          Num = static_cast<int>(HoursBetween(ANow, AThen));
          if (Num > 1)
          {
            Part = 10;
          }
          else if (Num == 1)
          {
            Part = 9;
          }
          else
          {
            Num = static_cast<int>(MinutesBetween(ANow, AThen));
            if (Num > 1)
            {
              Part = 8;
            }
            else if (Num == 1)
            {
              Part = 7;
            }
            else
            {
              Num = static_cast<int>(SecondsBetween(ANow, AThen));
              if (Num > 1)
              {
                Part = 6;
              }
              else if (Num == 1)
              {
                Part = 5;
              }
              else if (Num == 0)
              {
                Part = 1;
              }
              else
              {
                DebugFail();
                Part = -1;
              }
            }
          }
        }
      }
    }

    if (DebugAlwaysTrue(Part >= 0))
    {
      Result = FORMAT(LoadStrPart(TIME_RELATIVE, Part), (abs(Num)));
    }
    else
    {
      Result = FormatDateTime(L"ddddd", AThen);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ExtractFileBaseName(const UnicodeString & Path)
{
  return ChangeFileExt(ExtractFileName(Path), L"");
}
//---------------------------------------------------------------------------
TStringList * __fastcall TextToStringList(const UnicodeString & Text)
{
  std::unique_ptr<TStringList> List(new TStringList());
  List->Text = Text;
  return List.release();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall StringsToText(TStrings * Strings)
{
  UnicodeString Result;
  if (Strings->Count == 1)
  {
    Result = Strings->Strings[0];
  }
  else
  {
    Result = Strings->Text;
  }
  return Result;
}
//---------------------------------------------------------------------------
TStringList * __fastcall CommaTextToStringList(const UnicodeString & CommaText)
{
  std::unique_ptr<TStringList> List(new TStringList());
  List->CommaText = CommaText;
  return List.release();
}
//---------------------------------------------------------------------------
TStrings * __fastcall CloneStrings(TStrings * Strings)
{
  std::unique_ptr<TStringList> List(new TStringList());
  List->AddStrings(Strings);
  return List.release();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TrimVersion(UnicodeString Version)
{
  while ((Version.Pos(L".") != Version.LastDelimiter(L".")) &&
    (Version.SubString(Version.Length() - 1, 2) == L".0"))
  {
    Version.SetLength(Version.Length() - 2);
  }
  return Version;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FormatVersion(int MajorVersion, int MinorVersion, int Release)
{
  return
    TrimVersion(FORMAT(L"%d.%d.%d",
      (MajorVersion, MinorVersion, Release)));
}
//---------------------------------------------------------------------------
TFormatSettings __fastcall GetEngFormatSettings()
{
  return TFormatSettings::Create((TLocaleID)1033);
}
//---------------------------------------------------------------------------
int __fastcall ParseShortEngMonthName(const UnicodeString & MonthStr)
{
  TFormatSettings FormatSettings = GetEngFormatSettings();
  return IndexStr(MonthStr, FormatSettings.ShortMonthNames, FormatSettings.ShortMonthNames.Size()) + 1;
}
//---------------------------------------------------------------------------
TStringList * __fastcall CreateSortedStringList(bool CaseSensitive, System::Types::TDuplicates Duplicates)
{
  TStringList * Result = new TStringList();
  Result->CaseSensitive = CaseSensitive;
  Result->Sorted = true;
  Result->Duplicates = Duplicates;
  return Result;
}
//---------------------------------------------------------------------------
bool SameIdent(const UnicodeString & Ident1, const UnicodeString & Ident2)
{
  const UnicodeString Dash(L"-");
  return SameText(ReplaceStr(Ident1, Dash, EmptyStr), ReplaceStr(Ident2, Dash, EmptyStr));
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FindIdent(const UnicodeString & Ident, TStrings * Idents)
{
  for (int Index = 0; Index < Idents->Count; Index++)
  {
    if (SameIdent(Ident, Idents->Strings[Index]))
    {
      return Idents->Strings[Index];
    }
  }
  return Ident;
}
//---------------------------------------------------------------------------
UnicodeString GetTlsErrorStr(unsigned long Err)
{
  char Buffer[512];
  ERR_error_string_n(Err, Buffer, sizeof(Buffer));
  UnicodeString S = UnicodeString(UTF8String(Buffer));
  for (int I = 0; I < 4; I++)
  {
    CutToChar(S, L':', false);
  }
  UnicodeString ErrStr = IntToHex(static_cast<unsigned int>(Err));
  return FORMAT(L"OpenSSL %s: %s", (ErrStr, S.TrimRight()));
}
//---------------------------------------------------------------------------
UnicodeString GetTlsErrorStrs()
{
  UnicodeString Result;
  int Error;
  const char * Data;
  while ((Error = ERR_get_error_all(NULL, NULL, NULL, &Data, NULL)) != 0)
  {
    UnicodeString S = GetTlsErrorStr(Error);
    if ((Data != NULL) && (strlen(Data) > 0))
    {
      UnicodeString DataStr = UnicodeString(UTF8String(Data)).TrimRight();
      S += FORMAT(L" (%s)", (DataStr));
    }
    AddToList(Result, S, L"\n");
  }
  return Result;
}
//---------------------------------------------------------------------------
static FILE * __fastcall OpenCertificate(const UnicodeString & Path)
{
  FILE * Result = _wfopen(ApiPath(Path).c_str(), L"rb");
  if (Result == NULL)
  {
    int Error = errno;
    throw EOSExtException(MainInstructions(FMTLOAD(CERTIFICATE_OPEN_ERROR, (Path))), Error);
  }

  return Result;
}
//---------------------------------------------------------------------------
struct TPemPasswordCallbackData
{
  UnicodeString * Passphrase;
};
//---------------------------------------------------------------------------
static int PemPasswordCallback(char * Buf, int Size, int /*RWFlag*/, void * UserData)
{
  TPemPasswordCallbackData & Data = *reinterpret_cast<TPemPasswordCallbackData *>(UserData);
  UTF8String UtfPassphrase = UTF8String(*Data.Passphrase);
  strncpy(Buf, UtfPassphrase.c_str(), Size);
  Shred(UtfPassphrase);
  Buf[Size - 1] = '\0';
  return strlen(Buf);
}
//---------------------------------------------------------------------------
static bool __fastcall IsTlsPassphraseError(unsigned long Error, bool HasPassphrase)
{
  int ErrorLib = ERR_GET_LIB(Error);
  int ErrorReason = ERR_GET_REASON(Error);

  bool Result =
    ((ErrorLib == ERR_LIB_PKCS12) &&
     (ErrorReason == PKCS12_R_MAC_VERIFY_FAILURE)) ||
    ((ErrorLib == ERR_LIB_PEM) &&
     (ErrorReason == PEM_R_BAD_PASSWORD_READ)) ||
    (HasPassphrase && (ERR_LIB_EVP == ERR_LIB_EVP) &&
     ((ErrorReason == PEM_R_BAD_DECRYPT) || (ErrorReason == PEM_R_BAD_BASE64_DECODE)));

  return Result;
}
//---------------------------------------------------------------------------
static void __fastcall ThrowTlsCertificateErrorIgnorePassphraseErrors(const UnicodeString & Path, bool HasPassphrase)
{
  unsigned long Error = ERR_get_error();
  if (!IsTlsPassphraseError(Error, HasPassphrase))
  {
    throw ExtException(MainInstructions(FMTLOAD(CERTIFICATE_READ_ERROR, (Path))), GetTlsErrorStr(Error));
  }
}
//---------------------------------------------------------------------------
void __fastcall ParseCertificate(const UnicodeString & Path,
  const UnicodeString & Passphrase, X509 *& Certificate, EVP_PKEY *& PrivateKey,
  bool & WrongPassphrase)
{
  Certificate = NULL;
  PrivateKey = NULL;
  WrongPassphrase = false;
  bool Discard = false;
  try
  {
    try
    {
      bool HasPassphrase = !Passphrase.IsEmpty();

      FILE * File;

      // Inspired by neon's ne_ssl_clicert_read
      File = OpenCertificate(Path);
      // openssl pkcs12 -inkey cert.pem -in cert.crt -export -out cert.pfx
      // Binary file
      PKCS12 * Pkcs12 = d2i_PKCS12_fp(File, NULL);
      fclose(File);

      if (Pkcs12 != NULL)
      {
        UTF8String PassphraseUtf(Passphrase);

        bool Result =
          (PKCS12_parse(Pkcs12, PassphraseUtf.c_str(), &PrivateKey, &Certificate, NULL) == 1);
        PKCS12_free(Pkcs12);

        if (!Result)
        {
          ThrowTlsCertificateErrorIgnorePassphraseErrors(Path, HasPassphrase);
          WrongPassphrase = true;
        }
        if (!WrongPassphrase && (PrivateKey == NULL))
        {
          throw Exception(FMTLOAD(NO_PRIVATE_KEY, (Path)));
        }
      }
      else
      {
        ERR_clear_error();

        TPemPasswordCallbackData CallbackUserData;
        // PemPasswordCallback never writes to the .Passphrase
        CallbackUserData.Passphrase = const_cast<UnicodeString *>(&Passphrase);

        File = OpenCertificate(Path);
        // Encrypted:
        // openssl req -x509 -newkey rsa:2048 -keyout cert.pem -out cert.crt
        // -----BEGIN ENCRYPTED PRIVATE KEY-----
        // ...
        // -----END ENCRYPTED PRIVATE KEY-----

        // Not encrypted (add -nodes):
        // -----BEGIN PRIVATE KEY-----
        // ...
        // -----END PRIVATE KEY-----
        // Or (openssl genrsa -out client.key 1024   # used for certificate signing request)
        // -----BEGIN RSA PRIVATE KEY-----
        // ...
        // -----END RSA PRIVATE KEY-----
        PrivateKey = PEM_read_PrivateKey(File, NULL, PemPasswordCallback, &CallbackUserData);
        fclose(File);

        if (PrivateKey == NULL)
        {
          ThrowTlsCertificateErrorIgnorePassphraseErrors(Path, HasPassphrase);
          WrongPassphrase = true;
        }

        File = OpenCertificate(Path);
        // The file can contain both private and public key
        // (basically cert.pem and cert.crt appended one to each other)
        // -----BEGIN ENCRYPTED PRIVATE KEY-----
        // ...
        // -----END ENCRYPTED PRIVATE KEY-----
        // -----BEGIN CERTIFICATE-----
        // ...
        // -----END CERTIFICATE-----
        Certificate = PEM_read_X509(File, NULL, PemPasswordCallback, &CallbackUserData);
        fclose(File);

        if (Certificate == NULL)
        {
          unsigned long Error = ERR_get_error();
          // unlikely
          if (IsTlsPassphraseError(Error, HasPassphrase))
          {
            WrongPassphrase = true;
          }
          else
          {
            UnicodeString CertificatePath = ChangeFileExt(Path, L".cer");
            if (!FileExists(CertificatePath))
            {
              CertificatePath = ChangeFileExt(Path, L".crt");
            }

            if (!FileExists(CertificatePath))
            {
              throw Exception(MainInstructions(FMTLOAD(CERTIFICATE_PUBLIC_KEY_NOT_FOUND, (Path))));
            }
            else
            {
              File = OpenCertificate(CertificatePath);
              // -----BEGIN CERTIFICATE-----
              // ...
              // -----END CERTIFICATE-----
              Certificate = PEM_read_X509(File, NULL, PemPasswordCallback, &CallbackUserData);
              fclose(File);

              if (Certificate == NULL)
              {
                unsigned long Base64Error = ERR_get_error();

                File = OpenCertificate(CertificatePath);
                // Binary DER-encoded certificate
                // (as above, with BEGIN/END removed, and decoded from Base64 to binary)
                // openssl x509 -in cert.crt -out client.der.crt -outform DER
                Certificate = d2i_X509_fp(File, NULL);
                fclose(File);

                if (Certificate == NULL)
                {
                  unsigned long DERError = ERR_get_error();

                  UnicodeString Message = MainInstructions(FMTLOAD(CERTIFICATE_READ_ERROR, (CertificatePath)));
                  UnicodeString MoreMessages =
                    FORMAT(L"Base64: %s\nDER: %s", (GetTlsErrorStr(Base64Error), GetTlsErrorStr(DERError)));
                  throw ExtException(Message, MoreMessages);
                }
              }
            }
          }
        }
      }
    }
    catch (...)
    {
      Discard = true;
      throw;
    }
  }
  __finally
  {
    if (Discard || WrongPassphrase)
    {
      if (PrivateKey != NULL)
      {
        EVP_PKEY_free(PrivateKey);
        PrivateKey = NULL;
      }
      if (Certificate != NULL)
      {
        X509_free(Certificate);
        Certificate = NULL;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall CheckCertificate(const UnicodeString & Path)
{
  X509 * Certificate;
  EVP_PKEY * PrivateKey;
  bool WrongPassphrase;

  ParseCertificate(Path, L"", Certificate, PrivateKey, WrongPassphrase);

  if (PrivateKey != NULL)
  {
    EVP_PKEY_free(PrivateKey);
  }
  if (Certificate != NULL)
  {
    X509_free(Certificate);
  }
}
//---------------------------------------------------------------------------
const UnicodeString HttpProtocol(L"http");
const UnicodeString HttpsProtocol(L"https");
const UnicodeString ProtocolSeparator(L"://");
//---------------------------------------------------------------------------
bool __fastcall IsHttpUrl(const UnicodeString & S)
{
  return StartsText(HttpProtocol + ProtocolSeparator, S);
}
//---------------------------------------------------------------------------
bool __fastcall IsHttpOrHttpsUrl(const UnicodeString & S)
{
  return
    IsHttpUrl(S) ||
    StartsText(HttpsProtocol + ProtocolSeparator, S);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ChangeUrlProtocol(const UnicodeString & S, const UnicodeString & Protocol)
{
  int P = S.Pos(ProtocolSeparator);
  DebugAssert(P > 0);
  return Protocol + ProtocolSeparator + RightStr(S, S.Length() - P - ProtocolSeparator.Length() + 1);
}
//---------------------------------------------------------------------------
const UnicodeString RtfPara(TraceInitStr(L"\\par\n"));
const UnicodeString AssemblyNamespace(TraceInitStr(L"WinSCP"));
const UnicodeString TransferOptionsClassName(TraceInitStr(L"TransferOptions"));
const UnicodeString SessionClassName(TraceInitStr(L"Session"));
const UnicodeString RtfHyperlinkField(TraceInitStr(L"HYPERLINK"));
const UnicodeString RtfHyperlinkFieldPrefix(TraceInitStr(RtfHyperlinkField + L" \""));
const UnicodeString RtfHyperlinkFieldSuffix(TraceInitStr(L"\" "));
//---------------------------------------------------------------------
UnicodeString __fastcall RtfColor(int Index)
{
  return FORMAT(L"\\cf%d", (Index));
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfText(const UnicodeString & Text, bool Rtf)
{
  UnicodeString Result = Text;
  if (Rtf)
  {
    int Index = 1;
    while (Index <= Result.Length())
    {
      UnicodeString Replacement;
      wchar_t Ch = Result[Index];
      if ((Ch == L'\\') || (Ch == L'{') || (Ch == L'}'))
      {
        Replacement = FORMAT(L"\\%s", (Ch));
      }
      else if (Ch >= 0x0080)
      {
        Replacement = FORMAT(L"\\u%d?", (int(Ch)));
      }

      if (!Replacement.IsEmpty())
      {
        Result.Delete(Index, 1);
        Result.Insert(Replacement, Index);
        Index += Replacement.Length();
      }
      else
      {
        Index++;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfColorText(int Color, const UnicodeString & Text)
{
  return RtfColor(Color) + L" " + RtfText(Text) + RtfColor(0) + L" ";
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfColorItalicText(int Color, const UnicodeString & Text)
{
  return RtfColor(Color) + L"\\i " + RtfText(Text) + L"\\i0" + RtfColor(0) + L" ";
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfOverrideColorText(const UnicodeString & Text)
{
  return RtfColorText(1, Text);
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfKeyword(const UnicodeString & Text)
{
  return RtfColorText(5, Text);
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfParameter(const UnicodeString & Text)
{
  return RtfColorText(6, Text);
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfString(const UnicodeString & Text)
{
  return RtfColorText(4, Text);
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfLink(const UnicodeString & Link, const UnicodeString & RtfText)
{
  return
    L"{\\field{\\*\\fldinst{" + RtfHyperlinkFieldPrefix + Link + RtfHyperlinkFieldSuffix + L"}}{\\fldrslt{" +
    RtfText + L"}}}";
}
//---------------------------------------------------------------------
UnicodeString __fastcall ScriptCommandLink(const UnicodeString & Command)
{
  return L"scriptcommand_" + Command;
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfSwitch(
  const UnicodeString & Switch, const UnicodeString & Link, bool Rtf)
{
  UnicodeString Result = FORMAT(L"-%s", (Switch));
  if (Rtf)
  {
    Result = RtfLink(Link + L"#" + Switch.LowerCase(), RtfParameter(Result));
  }
  return L" " + Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfSwitchValue(
  const UnicodeString & Name, const UnicodeString & Link, const UnicodeString & Value, bool Rtf)
{
  return RtfSwitch(Name, Link, Rtf) + L"=" + Value;
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfSwitch(
  const UnicodeString & Name, const UnicodeString & Link, const UnicodeString & Value, bool Rtf)
{
  return RtfSwitchValue(Name, Link, RtfText(FORMAT("\"%s\"", (EscapeParam(Value))), Rtf), Rtf);
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfSwitch(
  const UnicodeString & Name, const UnicodeString & Link, int Value, bool Rtf)
{
  return RtfSwitchValue(Name, Link, RtfText(IntToStr(Value), Rtf), Rtf);
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfRemoveHyperlinks(UnicodeString Text)
{
  // Remove all tags HYPERLINK "https://www.example.com".
  // See also RtfEscapeParam
  int Index = 1;
  int P;
  while ((P = PosEx(RtfHyperlinkFieldPrefix, Text, Index)) > 0)
  {
    int Index2 = P + RtfHyperlinkFieldPrefix.Length();
    int P2 = PosEx(RtfHyperlinkFieldSuffix, Text, Index2);
    if (P2 > 0)
    {
      Text.Delete(P, P2 - P + RtfHyperlinkFieldSuffix.Length());
    }
    else
    {
      Index = Index2;
    }
  }
  return Text;
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfEscapeParam(UnicodeString Param, bool PowerShellEscape)
{
  const UnicodeString Quote(L"\"");
  UnicodeString Escape(Quote);
  if (PowerShellEscape)
  {
    Escape = "`" + Escape + "`";
  }
  // Equivalent of EscapeParam, except that it does not double quotes in HYPERLINK.
  // See also RtfRemoveHyperlinks.
  int Index = 1;
  while (true)
  {
    int P1 = PosEx(Quote, Param, Index);
    if (P1 == 0)
    {
      // no more quotes
      break;
    }
    else
    {
      int P2 = PosEx(RtfHyperlinkFieldPrefix, Param, Index);
      int P3 CLANG_INITIALIZE(0);
      if ((P2 > 0) && (P2 < P1) && ((P3 = PosEx(RtfHyperlinkFieldSuffix, Param, P2)) > 0))
      {
        // skip HYPERLINK
        Index = P3 + RtfHyperlinkFieldSuffix.Length();
      }
      else
      {
        Param.Insert(Escape, P1);
        Index = P1 + (Escape.Length() + Quote.Length());
      }
    }
  }

  return Param;
}
//---------------------------------------------------------------------
static UnicodeString __fastcall RtfCodeComment(const UnicodeString & Text)
{
  return RtfColorItalicText(2, Text);
}
//---------------------------------------------------------------------
UnicodeString __fastcall AssemblyCommentLine(TAssemblyLanguage Language, const UnicodeString & Text)
{
  UnicodeString Prefix;
  switch (Language)
  {
    case alCSharp:
      Prefix = L"//";
      break;

    case alVBNET:
      Prefix = L"'";
      break;

    case alPowerShell:
      Prefix = L"#";
      break;
  }

  return RtfCodeComment(Prefix + L" " + Text) + RtfPara;
}
//---------------------------------------------------------------------
UnicodeString __fastcall AssemblyString(TAssemblyLanguage Language, UnicodeString S)
{
  switch (Language)
  {
    case alCSharp:
      if (S.Pos(L"\\") > 0)
      {
        S = FORMAT(L"@\"%s\"", (ReplaceStr(S, L"\"", L"\"\"")));
      }
      else
      {
        S = FORMAT(L"\"%s\"", (ReplaceStr(S, L"\"", L"\\\"")));
      }
      break;

    case alVBNET:
      S = FORMAT(L"\"%s\"", (ReplaceStr(S, L"\"", L"\"\"")));
      break;

    case alPowerShell:
      S = FORMAT(L"\"%s\"", (ReplaceStr(ReplaceStr(ReplaceStr(S, L"`", L"``"), L"$", L"`$"), L"\"", L"`\"")));
      break;

    default:
      DebugFail();
      break;
  }

  return RtfString(S);
}
//---------------------------------------------------------------------
static UnicodeString __fastcall RtfClass(const UnicodeString & Text)
{
  return RtfColorText(3, Text);
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfLibraryClass(const UnicodeString & ClassName)
{
  return RtfLink(L"library_" + ClassName.LowerCase(), RtfClass(ClassName));
}
//---------------------------------------------------------------------
UnicodeString __fastcall RtfLibraryMethod(const UnicodeString & ClassName, const UnicodeString & MethodName, bool InPage)
{
  return RtfLink(L"library_" + ClassName.LowerCase() + (InPage ? L"#" : L"_") + MethodName.LowerCase(), RtfOverrideColorText(MethodName));
}
//---------------------------------------------------------------------
static UnicodeString __fastcall RtfLibraryProperty(const UnicodeString & ClassName, const UnicodeString & PropertyName)
{
  return RtfLink(L"library_" + ClassName.LowerCase() + L"#" + PropertyName.LowerCase(), RtfOverrideColorText(PropertyName));
}
//---------------------------------------------------------------------
UnicodeString __fastcall AssemblyVariableName(TAssemblyLanguage Language, const UnicodeString & ClassName)
{
  UnicodeString Result = ClassName.SubString(1, 1).LowerCase() + ClassName.SubString(2, ClassName.Length() - 1);
  if (Language == alPowerShell)
  {
    Result = L"$" + Result;
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall AssemblyStatementSeparator(TAssemblyLanguage Language)
{
  UnicodeString Result;
  switch (Language)
  {
    case alCSharp:
      Result = L";";
      break;

    case alVBNET:
    case alPowerShell:
      // noop
      break;
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall AssemblyPropertyRaw(
  TAssemblyLanguage Language, const UnicodeString & ClassName, const UnicodeString & Name,
  const UnicodeString & Value, bool Inline)
{
  UnicodeString Result;
  UnicodeString RtfPropertyAndValue = RtfLibraryProperty(ClassName, Name) + L" = " + Value;
  UnicodeString Indetation = (Inline ? L"" : L"    ");
  UnicodeString SpaceOrPara = (Inline ? UnicodeString(L" ") : RtfPara);
  switch (Language)
  {
    case alCSharp:
      Result = Indetation + RtfPropertyAndValue + (Inline ? L"" : L",") + SpaceOrPara;
      break;

    case alVBNET:
      Result = Indetation + L"." + RtfPropertyAndValue + SpaceOrPara;
      break;

    case alPowerShell:
      Result = Indetation + RtfPropertyAndValue + SpaceOrPara;
      break;
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall AssemblyProperty(
  TAssemblyLanguage Language, const UnicodeString & ClassName, const UnicodeString & Name,
  const UnicodeString & Type, const UnicodeString & Member, bool Inline)
{
  UnicodeString PropertyValue;

  switch (Language)
  {
    case alCSharp:
    case alVBNET:
      PropertyValue = RtfClass(Type) + RtfText(L"." + Member);
      break;

    case alPowerShell:
      PropertyValue = RtfText(L"[" + AssemblyNamespace + L".") + RtfClass(Type) + RtfText(L"]::" + Member);
      break;
  }

  return AssemblyPropertyRaw(Language, ClassName, Name, PropertyValue, Inline);
}
//---------------------------------------------------------------------
UnicodeString __fastcall AssemblyProperty(
  TAssemblyLanguage Language, const UnicodeString & ClassName,
  const UnicodeString & Name, const UnicodeString & Value, bool Inline)
{
  return AssemblyPropertyRaw(Language, ClassName, Name, AssemblyString(Language, Value), Inline);
}
//---------------------------------------------------------------------
UnicodeString __fastcall AssemblyProperty(
  TAssemblyLanguage Language, const UnicodeString & ClassName,
  const UnicodeString & Name, int Value, bool Inline)
{
  return AssemblyPropertyRaw(Language, ClassName, Name, IntToStr(Value), Inline);
}
//---------------------------------------------------------------------
UnicodeString __fastcall AssemblyBoolean(TAssemblyLanguage Language, bool Value)
{
  UnicodeString Result;

  switch (Language)
  {
    case alCSharp:
      Result = (Value ? L"true" : L"false");
      break;

    case alVBNET:
      Result = (Value ? L"True" : L"False");
      break;

    case alPowerShell:
      Result = (Value ? L"$True" : L"$False");
      break;
  }

  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall AssemblyProperty(
  TAssemblyLanguage Language, const UnicodeString & ClassName, const UnicodeString & Name, bool Value, bool Inline)
{
  UnicodeString PropertyValue = AssemblyBoolean(Language, Value);

  return AssemblyPropertyRaw(Language, ClassName, Name, PropertyValue, Inline);
}
//---------------------------------------------------------------------
UnicodeString __fastcall AssemblyNewClassInstance(TAssemblyLanguage Language, const UnicodeString & ClassName, bool Inline)
{
  UnicodeString VariableName = AssemblyVariableName(Language, ClassName);
  UnicodeString RtfClass = RtfLibraryClass(ClassName);

  UnicodeString Result;
  switch (Language)
  {
    case alCSharp:
      if (!Inline)
      {
        Result += RtfClass + RtfText(L" " + VariableName  + L" = ");
      }
      Result += RtfKeyword(L"new") + RtfText(L" ") + RtfClass;
      break;

    case alVBNET:
      if (!Inline)
      {
        Result += RtfText(VariableName + L" ") + RtfKeyword(L"As") + RtfText(L" ");
      }
      Result += RtfKeyword(L"New") + RtfText(" ") + RtfClass;
      break;

    case alPowerShell:
      if (!Inline)
      {
        Result += RtfText(VariableName + L" = ");
      }
      Result += RtfKeyword(L"New-Object") + RtfText(L" " + AssemblyNamespace + L".") + RtfClass;
      break;
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall AssemblyVariableDeclaration(TAssemblyLanguage Language)
{
  UnicodeString Result;
  switch (Language)
  {
    case alVBNET:
      Result = RtfKeyword(L"Dim") + RtfText(L" ");
      break;
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall AssemblyNewClassInstanceStart(
  TAssemblyLanguage Language, const UnicodeString & ClassName, bool Inline)
{
  UnicodeString SpaceOrPara = (Inline ? UnicodeString(L" ") : RtfPara);

  UnicodeString Result;
  if (!Inline)
  {
    Result += AssemblyVariableDeclaration(Language);
  }
  Result += AssemblyNewClassInstance(Language, ClassName, Inline);

  switch (Language)
  {
    case alCSharp:
      Result += SpaceOrPara + RtfText(L"{") + SpaceOrPara;
      break;

    case alVBNET:
      // Historically we use Dim .. With instead of object initializer.
      // But for inline use, we have to use object initialize.
      // We should consistently always use object initializers.
      // Unfortunately VB.NET object initializer (contrary to C#) does not allow trailing comma.
      Result += SpaceOrPara + RtfKeyword(L"With");
      if (Inline)
      {
        Result += RtfText(L" { ");
      }
      else
      {
        Result += RtfText(L" " + AssemblyVariableName(Language, ClassName)) + RtfPara;
      }
      break;

    case alPowerShell:
      Result += RtfText(" -Property @{") + SpaceOrPara;
      break;
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall AssemblyNewClassInstanceEnd(TAssemblyLanguage Language, bool Inline)
{
  UnicodeString InlineEnd = RtfText(L"}");

  UnicodeString Result;
  switch (Language)
  {
    case alCSharp:
      if (Inline)
      {
        Result = InlineEnd;
      }
      else
      {
        Result = RtfText(L"};") + RtfPara;
      }
      break;

    case alVBNET:
      if (Inline)
      {
        Result = InlineEnd;
      }
      else
      {
        Result = RtfKeyword(L"End With") + RtfPara;
      }
      break;

    case alPowerShell:
      if (Inline)
      {
        Result = InlineEnd;
      }
      else
      {
        Result = RtfText(L"}") + RtfPara;
      }
      break;
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall AssemblyAddRawSettings(
  TAssemblyLanguage Language, TStrings * RawSettings, const UnicodeString & ClassName,
  const UnicodeString & MethodName)
{
  UnicodeString Result;
  for (int Index = 0; Index < RawSettings->Count; Index++)
  {
    UnicodeString Name = RawSettings->Names[Index];
    UnicodeString Value = RawSettings->ValueFromIndex[Index];
    UnicodeString AddRawSettingsMethod =
      RtfLibraryMethod(ClassName, MethodName, false) +
      FORMAT(L"(%s, %s)", (AssemblyString(Language, Name), AssemblyString(Language, Value)));
    UnicodeString VariableName = AssemblyVariableName(Language, ClassName);
    Result += RtfText(VariableName + L".") + AddRawSettingsMethod + AssemblyStatementSeparator(Language) + RtfPara;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall LoadScriptFromFile(UnicodeString FileName, TStrings * Lines, bool FallbackToAnsi)
{
  std::unique_ptr<TFileStream> Stream(new TFileStream(ApiPath(FileName), fmOpenRead | fmShareDenyWrite));

  // Simple stream reading, to make it work with named pipes too, not only with physical files
  TBytes Buffer;
  Buffer.Length = 10*1024;
  int Read;
  int Offset = 0;
  do
  {
    Read = Stream->Read(Buffer, Offset, Buffer.Length - Offset);
    Offset += Read;
    if (Offset > Buffer.Length / 2)
    {
      Buffer.Length = Buffer.Length * 2;
    }
  }
  while (Read > 0);
  Buffer.Length = Offset;

  TEncoding * Encoding = NULL;
  int PreambleSize = TEncoding::GetBufferEncoding(Buffer, Encoding, TEncoding::UTF8);
  UnicodeString S;
  try
  {
    S = Encoding->GetString(Buffer, PreambleSize, Buffer.Length - PreambleSize);
  }
  catch (EEncodingError & E)
  {
    if (FallbackToAnsi)
    {
      S = TEncoding::ANSI->GetString(Buffer);
    }
    else
    {
      throw ExtException(LoadStr(TEXT_FILE_ENCODING), &E);
    }
  }

  Lines->Text = S;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall StripEllipsis(const UnicodeString & S)
{
  UnicodeString Result = S;
  if (Result.SubString(Result.Length() - Ellipsis.Length() + 1, Ellipsis.Length()) == Ellipsis)
  {
    Result.SetLength(Result.Length() - Ellipsis.Length());
    Result = Result.TrimRight();
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall GetFileMimeType(const UnicodeString & FileName)
{
  wchar_t * MimeOut = NULL;
  UnicodeString Result;
  if (FindMimeFromData(NULL, FileName.c_str(), NULL, 0, NULL, FMFD_URLASFILENAME, &MimeOut, 0) == S_OK)
  {
    Result = MimeOut;
    CoTaskMemFree(MimeOut);
  }
  return Result;
}
//---------------------------------------------------------------------------
TStrings * TlsCipherList()
{
  std::unique_ptr<TStrings> Result(new TStringList());
  // Exact method that neon uses. FTP uses TLS_method() (FTP needs server method too). But they have the same ciphers.
  const SSL_METHOD * Method = TLS_client_method();
  SSL_CTX * Ctx = SSL_CTX_new(Method);
  SSL * Ssl = SSL_new(Ctx);

  int Index = 0;
  const char * CipherName;
  do
  {
    CipherName = SSL_get_cipher_list(Ssl, Index);
    Index++;
    if (CipherName != NULL)
    {
      Result->Add(UnicodeString(CipherName));
    }
  }
  while (CipherName != NULL);

  return Result.release();
}
//---------------------------------------------------------------------------
void SetStringValueEvenIfEmpty(TStrings * Strings, const UnicodeString & Name, const UnicodeString & Value)
{
  if (Value.IsEmpty())
  {
    int Index = Strings->IndexOfName(Name);
    if (Index < 0)
    {
      Index = Strings->Add(L"");
    }
    UnicodeString Line = Name + Strings->NameValueSeparator;
    Strings->Strings[Index] = Line;
  }
  else
  {
    Strings->Values[Name] = Value;
  }
}
//---------------------------------------------------------------------------
DWORD __fastcall GetParentProcessId(HANDLE Snapshot, DWORD ProcessId)
{
  DWORD Result = 0;

  PROCESSENTRY32 ProcessEntry;
  memset(&ProcessEntry, sizeof(ProcessEntry), 0);
  ProcessEntry.dwSize = sizeof(PROCESSENTRY32);

  if (Process32First(Snapshot, &ProcessEntry))
  {
    do
    {
      if (ProcessEntry.th32ProcessID == ProcessId)
      {
        Result = ProcessEntry.th32ParentProcessID;
      }
    } while (Process32Next(Snapshot, &ProcessEntry));
  }
  return Result;
}
//---------------------------------------------------------------------------
static UnicodeString GetProcessName(DWORD ProcessId)
{
  UnicodeString Result;
  HANDLE Process = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, ProcessId);
   // is common, when the parent process is installer, so we ignore it
  if (Process)
  {
    Result.SetLength(MAX_PATH);
    DWORD Len = GetModuleFileNameEx(Process, NULL, Result.c_str(), Result.Length());
    Result.SetLength(Len);
    // is common too, for some reason
    if (!Result.IsEmpty())
    {
      Result = ExtractProgramName(FormatCommand(Result, UnicodeString()));
    }
    CloseHandle(Process);
  }
  return Result;
}
//---------------------------------------------------------------------------
static UnicodeString ParentProcessName;
//---------------------------------------------------------------------------
UnicodeString __fastcall GetAncestorProcessName(int Levels)
{
  UnicodeString Result;
  bool Parent = (Levels == 1);
  if (Parent && !ParentProcessName.IsEmpty())
  {
    Result = ParentProcessName;
  }
  else
  {
    try
    {
      HANDLE Snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

      DWORD ProcessId = GetCurrentProcessId();

      typedef std::vector<DWORD> TProcesses;
      TProcesses Processes;
      // Either more to go (>0) or collecting all levels (-1 from GetAncestorProcessNames)
      while ((Levels != 0) &&
             (Levels > -20) && // prevent infinite loops
             (ProcessId != 0))
      {
        const UnicodeString Sep(L", ");
        ProcessId = GetParentProcessId(Snapshot, ProcessId);
        // When ancestor process is terminated and another process reuses its ID, we may get a cycle.
        TProcesses::const_iterator I = std::find(Processes.begin(), Processes.end(), ProcessId);
        if (I != Processes.end())
        {
          int Index = I - Processes.begin();
          AddToList(Result, FORMAT(L"cycle-%d", (Index)), Sep);
          ProcessId = 0;
        }
        else
        {
          Processes.push_back(ProcessId);

          if ((Levels < 0) && (ProcessId != 0))
          {
            UnicodeString Name = GetProcessName(ProcessId);
            if (Name.IsEmpty())
            {
              Name = L"...";
              ProcessId = 0;
            }
            AddToList(Result, Name, Sep);
          }
          Levels--;
        }
      }

      if (Levels >= 0)
      {
        if (ProcessId == 0)
        {
          Result = L"err-notfound";
        }
        else
        {
          Result = GetProcessName(ProcessId);
        }
      }
      else if (Result.IsEmpty())
      {
        Result = L"n/a";
      }

      CloseHandle(Snapshot);
    }
    catch (...)
    {
      Result = L"err-except";
    }

    if (Parent)
    {
      ParentProcessName = Result;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
static UnicodeString AncestorProcessNames;
//---------------------------------------------------------------------------
UnicodeString GetAncestorProcessNames()
{
  if (AncestorProcessNames.IsEmpty())
  {
    AncestorProcessNames = GetAncestorProcessName(-1);
  }
  return AncestorProcessNames;
}
//---------------------------------------------------------------------------
NORETURN void NotImplemented()
{
  DebugFail();
  throw Exception(L"Not implemented");
}
//---------------------------------------------------------------------------
NORETURN void NotSupported()
{
  throw Exception(MainInstructions(LoadStr(NOTSUPPORTED)));
}
//---------------------------------------------------------------------------
UnicodeString GetDividerLine()
{
  return UnicodeString::StringOfChar(L'-', 27);
}
//---------------------------------------------------------------------------
static UnicodeString CutFeature(UnicodeString & Buf)
{
  UnicodeString Result;
  if (Buf.SubString(1, 1) == L"\"")
  {
    Buf.Delete(1, 1);
    int P = Buf.Pos(L"\",");
    if (P == 0)
    {
      Result = Buf;
      Buf = UnicodeString();
      // there should be the ending quote, but if not, just do nothing
      if (Result.SubString(Result.Length(), 1) == L"\"")
      {
        Result.SetLength(Result.Length() - 1);
      }
    }
    else
    {
      Result = Buf.SubString(1, P - 1);
      Buf.Delete(1, P + 1);
    }
    Buf = Buf.TrimLeft();
  }
  else
  {
    Result = CutToChar(Buf, L',', true);
  }
  return Result;
}
//---------------------------------------------------------------------------
TStrings * ProcessFeatures(TStrings * Features, const UnicodeString & AFeaturesOverride)
{
  std::unique_ptr<TStrings> Result(new TStringList());
  UnicodeString FeaturesOverride = AFeaturesOverride;
  if (FeaturesOverride.SubString(1, 1) == L"*")
  {
    FeaturesOverride.Delete(1, 1);
    while (!FeaturesOverride.IsEmpty())
    {
      UnicodeString Feature = CutFeature(FeaturesOverride);
      Result->Add(Feature);
    }
  }
  else
  {
    std::unique_ptr<TStrings> DeleteFeatures(CreateSortedStringList());
    std::unique_ptr<TStrings> AddFeatures(new TStringList());
    while (!FeaturesOverride.IsEmpty())
    {
      UnicodeString Feature = CutFeature(FeaturesOverride);
      if (Feature.SubString(1, 1) == L"-")
      {
        Feature.Delete(1, 1);
        DeleteFeatures->Add(Feature.LowerCase());
      }
      else
      {
        if (Feature.SubString(1, 1) == L"+")
        {
          Feature.Delete(1, 1);
        }
        AddFeatures->Add(Feature);
      }
    }

    for (int Index = 0; Index < Features->Count; Index++)
    {
      UnicodeString Feature = Features->Strings[Index];
      if (DeleteFeatures->IndexOf(Feature) < 0)
      {
        Result->Add(Feature);
      }
    }

    Result->AddStrings(AddFeatures.get());
  }
  return Result.release();
}
//---------------------------------------------------------------------
