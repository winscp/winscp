//---------------------------------------------------------------------------
#define NO_WIN32_LEAN_AND_MEAN
#include <vcl.h>
#pragma hdrstop

#include <shlobj.h>
#include <Common.h>

#include "GUITools.h"
#include "GUIConfiguration.h"
#include <TextsCore.h>
#include <CoreMain.h>
#include <SessionData.h>
#include <WinInterface.h>
#include <TbxUtils.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
extern const UnicodeString PageantTool = L"pageant.exe";
extern const UnicodeString PuttygenTool = L"puttygen.exe";
//---------------------------------------------------------------------------
bool __fastcall FindFile(UnicodeString & Path)
{
  bool Result = FileExists(Path);
  if (!Result)
  {
    int Len = GetEnvironmentVariable(L"PATH", NULL, 0);
    if (Len > 0)
    {
      UnicodeString Paths;
      Paths.SetLength(Len - 1);
      GetEnvironmentVariable(L"PATH", Paths.c_str(), Len);

      UnicodeString NewPath = FileSearch(ExtractFileName(Path), Paths);
      Result = !NewPath.IsEmpty();
      if (Result)
      {
        Path = NewPath;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall FileExistsEx(UnicodeString Path)
{
  return FindFile(Path);
}
//---------------------------------------------------------------------------
void __fastcall OpenSessionInPutty(const UnicodeString PuttyPath,
  TSessionData * SessionData, UnicodeString Password)
{
  UnicodeString Program, AParams, Dir;
  SplitCommand(PuttyPath, Program, AParams, Dir);
  Program = ExpandEnvironmentVariables(Program);
  if (FindFile(Program))
  {

    AParams = ExpandEnvironmentVariables(AParams);
    TCustomCommandData Data(SessionData, Password);
    TRemoteCustomCommand RemoteCustomCommand(Data, SessionData->RemoteDirectory);
    TWinInteractiveCustomCommand InteractiveCustomCommand(
      &RemoteCustomCommand, L"PuTTY");

    UnicodeString Params =
      RemoteCustomCommand.Complete(InteractiveCustomCommand.Complete(AParams, false), true);

    if (!RemoteCustomCommand.IsSiteCommand(AParams))
    {
      UnicodeString SessionName;
      TRegistryStorage * Storage = NULL;
      TSessionData * ExportData = NULL;
      TRegistryStorage * SourceStorage = NULL;
      try
      {
        Storage = new TRegistryStorage(Configuration->PuttySessionsKey);
        Storage->AccessMode = smReadWrite;
        // make it compatible with putty
        Storage->MungeStringValues = false;
        Storage->ForceAnsi = true;
        if (Storage->OpenRootKey(true))
        {
          if (Storage->KeyExists(SessionData->StorageKey))
          {
            SessionName = SessionData->SessionName;
          }
          else
          {
            SourceStorage = new TRegistryStorage(Configuration->PuttySessionsKey);
            SourceStorage->MungeStringValues = false;
            SourceStorage->ForceAnsi = true;
            if (SourceStorage->OpenSubKey(StoredSessions->DefaultSettings->Name, false) &&
                Storage->OpenSubKey(GUIConfiguration->PuttySession, true))
            {
              Storage->Copy(SourceStorage);
              Storage->CloseSubKey();
            }

            ExportData = new TSessionData(L"");
            ExportData->Assign(SessionData);
            ExportData->Modified = true;
            ExportData->Name = GUIConfiguration->PuttySession;
            ExportData->Password = L"";

            if (SessionData->FSProtocol == fsFTP)
            {
              if (GUIConfiguration->TelnetForFtpInPutty)
              {
                ExportData->PuttyProtocol = PuttyTelnetProtocol;
                ExportData->PortNumber = TelnetPortNumber;
                // PuTTY  does not allow -pw for telnet
                Password = L"";
              }
              else
              {
                ExportData->PuttyProtocol = PuttySshProtocol;
                ExportData->PortNumber = SshPortNumber;
              }
            }

            ExportData->Save(Storage, true);
            SessionName = GUIConfiguration->PuttySession;
          }
        }
      }
      __finally
      {
        delete Storage;
        delete ExportData;
        delete SourceStorage;
      }

      AddToList(Params, FORMAT(L"-load %s", (EscapePuttyCommandParam(SessionName))), L" ");
    }

    if (!Password.IsEmpty() && !RemoteCustomCommand.IsPasswordCommand(AParams))
    {
      AddToList(Params, FORMAT(L"-pw %s", (EscapePuttyCommandParam(Password))), L" ");
    }

    if (!ExecuteShell(Program, Params))
    {
      throw Exception(FMTLOAD(EXECUTE_APP_ERROR, (Program)));
    }
  }
  else
  {
    throw Exception(FMTLOAD(FILE_NOT_FOUND, (Program)));
  }
}
//---------------------------------------------------------------------------
bool __fastcall FindTool(const UnicodeString & Name, UnicodeString & Path)
{
  UnicodeString AppPath = IncludeTrailingBackslash(ExtractFilePath(Application->ExeName));
  Path = AppPath + Name;
  bool Result = true;
  if (!FileExists(Path))
  {
    Path = AppPath + L"PuTTY\\" + Name;
    if (!FileExists(Path))
    {
      Path = Name;
      if (!FindFile(Path))
      {
        Result = false;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall ExecuteShell(const UnicodeString Path, const UnicodeString Params)
{
  return ((int)ShellExecute(NULL, L"open", (wchar_t*)Path.data(),
    (wchar_t*)Params.data(), NULL, SW_SHOWNORMAL) > 32);
}
//---------------------------------------------------------------------------
bool __fastcall ExecuteShell(const UnicodeString Path, const UnicodeString Params,
  HANDLE & Handle)
{
  bool Result;

  TShellExecuteInfoW ExecuteInfo;
  memset(&ExecuteInfo, 0, sizeof(ExecuteInfo));
  ExecuteInfo.cbSize = sizeof(ExecuteInfo);
  ExecuteInfo.fMask = SEE_MASK_NOCLOSEPROCESS;
  ExecuteInfo.hwnd = Application->Handle;
  ExecuteInfo.lpFile = (wchar_t*)Path.data();
  ExecuteInfo.lpParameters = (wchar_t*)Params.data();
  ExecuteInfo.nShow = SW_SHOW;

  Result = (ShellExecuteEx(&ExecuteInfo) != 0);
  if (Result)
  {
    Handle = ExecuteInfo.hProcess;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall ExecuteShellAndWait(HWND Handle, const UnicodeString Path,
  const UnicodeString Params, TProcessMessagesEvent ProcessMessages)
{
  bool Result;

  TShellExecuteInfoW ExecuteInfo;
  memset(&ExecuteInfo, 0, sizeof(ExecuteInfo));
  ExecuteInfo.cbSize = sizeof(ExecuteInfo);
  ExecuteInfo.fMask = SEE_MASK_NOCLOSEPROCESS;
  ExecuteInfo.hwnd = Handle;
  ExecuteInfo.lpFile = (wchar_t*)Path.data();
  ExecuteInfo.lpParameters = (wchar_t*)Params.data();
  ExecuteInfo.nShow = SW_SHOW;

  Result = (ShellExecuteEx(&ExecuteInfo) != 0);
  if (Result)
  {
    if (ProcessMessages != NULL)
    {
      unsigned long WaitResult;
      do
      {
        WaitResult = WaitForSingleObject(ExecuteInfo.hProcess, 200);
        if (WaitResult == WAIT_FAILED)
        {
          throw Exception(LoadStr(DOCUMENT_WAIT_ERROR));
        }
        ProcessMessages();
      }
      while (WaitResult == WAIT_TIMEOUT);
    }
    else
    {
      WaitForSingleObject(ExecuteInfo.hProcess, INFINITE);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall ExecuteShellAndWait(HWND Handle, const UnicodeString Command,
  TProcessMessagesEvent ProcessMessages)
{
  UnicodeString Program, Params, Dir;
  SplitCommand(Command, Program, Params, Dir);
  return ExecuteShellAndWait(Handle, Program, Params, ProcessMessages);
}
//---------------------------------------------------------------------------
bool __fastcall SpecialFolderLocation(int PathID, UnicodeString & Path)
{
  LPITEMIDLIST Pidl;
  wchar_t Buf[256];
  if (SHGetSpecialFolderLocation(NULL, PathID, &Pidl) == NO_ERROR &&
      SHGetPathFromIDList(Pidl, Buf))
  {
    Path = UnicodeString(Buf);
    return true;
  }
  return false;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ItemsFormatString(const UnicodeString SingleItemFormat,
  const UnicodeString MultiItemsFormat, int Count, const UnicodeString FirstItem)
{
  UnicodeString Result;
  if (Count == 1)
  {
    Result = FORMAT(SingleItemFormat, (FirstItem));
  }
  else
  {
    Result = FORMAT(MultiItemsFormat, (Count));
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ItemsFormatString(const UnicodeString SingleItemFormat,
  const UnicodeString MultiItemsFormat, TStrings * Items)
{
  return ItemsFormatString(SingleItemFormat, MultiItemsFormat,
    Items->Count, (Items->Count > 0 ? Items->Strings[0] : UnicodeString()));
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FileNameFormatString(const UnicodeString SingleFileFormat,
  const UnicodeString MultiFilesFormat, TStrings * Files, bool Remote)
{
  assert(Files != NULL);
  UnicodeString Item;
  if (Files->Count > 0)
  {
    Item = Remote ? UnixExtractFileName(Files->Strings[0]) :
      ExtractFileName(Files->Strings[0]);
  }
  return ItemsFormatString(SingleFileFormat, MultiFilesFormat,
    Files->Count, Item);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall UniqTempDir(const UnicodeString BaseDir, const UnicodeString Identity,
  bool Mask)
{
  UnicodeString TempDir;
  do
  {
    TempDir = BaseDir.IsEmpty() ? SystemTemporaryDirectory() : BaseDir;
    TempDir = IncludeTrailingBackslash(TempDir) + Identity;
    if (Mask)
    {
      TempDir += L"?????";
    }
    else
    {
      TempDir += IncludeTrailingBackslash(FormatDateTime(L"nnzzz", Now()));
    };
  }
  while (!Mask && DirectoryExists(TempDir));

  return TempDir;
}
//---------------------------------------------------------------------------
bool __fastcall DeleteDirectory(const UnicodeString DirName)
{
  TSearchRec sr;
  bool retval = true;
  if (FindFirst(DirName + L"\\*", faAnyFile, sr) == 0) // VCL Function
  {
    if (FLAGSET(sr.Attr, faDirectory))
    {
      if (sr.Name != L"." && sr.Name != L"..")
        retval = DeleteDirectory(DirName + L"\\" + sr.Name);
    }
    else
    {
      retval = DeleteFile(DirName + L"\\" + sr.Name);
    }

    if (retval)
    {
      while (FindNextChecked(sr) == 0)
      { // VCL Function
        if (FLAGSET(sr.Attr, faDirectory))
        {
          if (sr.Name != L"." && sr.Name != L"..")
            retval = DeleteDirectory(DirName + L"\\" + sr.Name);
        }
        else
        {
          retval = DeleteFile(DirName + L"\\" + sr.Name);
        }

        if (!retval) break;
      }
    }
  }
  FindClose(sr);
  if (retval) retval = RemoveDir(DirName); // VCL function
  return retval;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FormatDateTimeSpan(const UnicodeString TimeFormat, TDateTime DateTime)
{
  UnicodeString Result;
  if (int(DateTime) > 0)
  {
    Result = IntToStr(int(DateTime)) + L", ";
  }
  // days are decremented, because when there are to many of them,
  // "integer overflow" error occurs
  Result += FormatDateTime(TimeFormat, DateTime - int(DateTime));
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall AddSessionColorImage(
  TCustomImageList * ImageList, TColor Color, bool Disconnected)
{

  if (Disconnected)
  {
    Color = GetShadowColor(Color);
  }

  int R, G, B;
  GetRGB(Color, R, G, B);
  TColor TransparentColor =
    (TColor)
    ((static_cast<int>(static_cast<unsigned char>(~B)) << 16) +
     (static_cast<int>(static_cast<unsigned char>(~G)) << 8) +
     static_cast<int>(static_cast<unsigned char>(~R)));

  std::auto_ptr<TBitmap> Bitmap(new TBitmap());
  Bitmap->SetSize(ImageList->Width, ImageList->Height);
  Bitmap->Canvas->Brush->Color = TransparentColor;
  Bitmap->Canvas->Brush->Style = bsSolid;
  TRect Rect(0, 0, ImageList->Width, ImageList->Height);
  Bitmap->Canvas->FillRect(Rect);

  const int Padding = 2;
  TRect RoundRect(Padding, Padding + 1, ImageList->Width - Padding - 1, ImageList->Height - Padding);

  Bitmap->Canvas->Pen->Color = Color;
  Bitmap->Canvas->Pen->Style = psSolid;
  Bitmap->Canvas->Brush->Color = Color;
  Bitmap->Canvas->RoundRect(RoundRect, 4, 4);

  ImageList->AddMasked(Bitmap.get(), TransparentColor);
}
//---------------------------------------------------------------------------
TLocalCustomCommand::TLocalCustomCommand()
{
}
//---------------------------------------------------------------------------
TLocalCustomCommand::TLocalCustomCommand(const TCustomCommandData & Data,
    const UnicodeString & Path) :
  TFileCustomCommand(Data, Path)
{
}
//---------------------------------------------------------------------------
TLocalCustomCommand::TLocalCustomCommand(const TCustomCommandData & Data,
  const UnicodeString & Path, const UnicodeString & FileName,
  const UnicodeString & LocalFileName, const UnicodeString & FileList) :
  TFileCustomCommand(Data, Path, FileName, FileList)
{
  FLocalFileName = LocalFileName;
}
//---------------------------------------------------------------------------
int __fastcall TLocalCustomCommand::PatternLen(const UnicodeString & Command, int Index)
{
  int Len;
  if (Command[Index + 1] == L'^')
  {
    Len = 3;
  }
  else
  {
    Len = TFileCustomCommand::PatternLen(Command, Index);
  }
  return Len;
}
//---------------------------------------------------------------------------
bool __fastcall TLocalCustomCommand::PatternReplacement(
  const UnicodeString & Pattern, UnicodeString & Replacement, bool & Delimit)
{
  bool Result;
  if (Pattern == L"!^!")
  {
    Replacement = FLocalFileName;
    Result = true;
  }
  else
  {
    Result = TFileCustomCommand::PatternReplacement(Pattern, Replacement, Delimit);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TLocalCustomCommand::DelimitReplacement(
  UnicodeString & /*Replacement*/, wchar_t /*Quote*/)
{
  // never delimit local commands
}
//---------------------------------------------------------------------------
bool __fastcall TLocalCustomCommand::HasLocalFileName(const UnicodeString & Command)
{
  return FindPattern(Command, L'^');
}
//---------------------------------------------------------------------------
bool __fastcall TLocalCustomCommand::IsFileCommand(const UnicodeString & Command)
{
  return TFileCustomCommand::IsFileCommand(Command) || HasLocalFileName(Command);
}
