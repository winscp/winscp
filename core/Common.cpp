//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "Exceptions.h"
#include "TextsCore.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
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
AnsiString GetTemporaryPath()
{
  AnsiString Path;
  Path.SetLength(255);
  GetTempPath(Path.Length(), Path.c_str());
  PackStr(Path);
  if (!Path.IsPathDelimiter(Path.Length()))
  {
    Path += '\\';
  }
  return Path;
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