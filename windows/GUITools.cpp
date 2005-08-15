//---------------------------------------------------------------------------
#define NO_WIN32_LEAN_AND_MEAN
#include <vcl.h>
#pragma hdrstop

#include <Consts.hpp>
#include <shlobj.h>
#include <Common.h>

#include "GUITools.h"
#include "GUIConfiguration.h"
#include <TextsWin.h>
#include <TextsCore.h>
#include <ScpMain.h>
#include <SessionData.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
bool __fastcall FindFile(AnsiString & Path)
{
  bool Result = FileExists(Path);
  if (!Result)
  {
    int Len = GetEnvironmentVariable("PATH", NULL, 0);
    if (Len > 0)
    {
      AnsiString Paths;
      Paths.SetLength(Len - 1);
      GetEnvironmentVariable("PATH", Paths.c_str(), Len);

      AnsiString NewPath = FileSearch(ExtractFileName(Path), Paths);
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
bool __fastcall FileExistsEx(AnsiString Path)
{
  return FindFile(Path);
}
//---------------------------------------------------------------------------
void __fastcall OpenSessionInPutty(TSessionData * SessionData, 
  const AnsiString Password)
{
  AnsiString Program, Params, Dir;
  SplitCommand(GUIConfiguration->PuttyPath, Program, Params, Dir);
  if (FindFile(Program))
  {
    AnsiString SessionName;
    THierarchicalStorage * Storage = NULL;
    TSessionData * ExportData = NULL;
    try
    {
      Storage = new TRegistryStorage(Configuration->PuttySessionsKey);
      Storage->AccessMode = smReadWrite;
      if (Storage->OpenRootKey(true))
      {
        if (Storage->KeyExists(SessionData->StorageKey))
        {
          SessionName = SessionData->SessionName;
        }
        else
        {
          ExportData = new TSessionData("");
          ExportData->Assign(SessionData);
          ExportData->Modified = true;
          ExportData->Name = GUIConfiguration->PuttySession;
          ExportData->Password = "";
          ExportData->Save(Storage, true);
          SessionName = GUIConfiguration->PuttySession;
        }
      }
    }
    __finally
    {
      delete Storage;
      delete ExportData;
    }

    if (!Params.IsEmpty())
    {
      Params += " ";
    }
    if (!Password.IsEmpty())
    {
      Params += FORMAT("-pw \"%s\" ", (Password)); 
    }
    Params += FORMAT("-load \"%s\"", (SessionName)); 
    
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
bool __fastcall ExecuteShell(const AnsiString Path, const AnsiString Params)
{
  return ((int)ShellExecute(NULL, "open", (char*)Path.data(),
    (char*)Params.data(), NULL, SW_SHOWNORMAL) > 32);
}
//---------------------------------------------------------------------------
bool __fastcall ExecuteShell(const AnsiString Path, const AnsiString Params,
  HANDLE & Handle)
{
  bool Result;

  TShellExecuteInfo ExecuteInfo;
  memset(&ExecuteInfo, 0, sizeof(ExecuteInfo));
  ExecuteInfo.cbSize = sizeof(ExecuteInfo);
  ExecuteInfo.fMask = SEE_MASK_NOCLOSEPROCESS;
  ExecuteInfo.hwnd = Application->Handle;
  ExecuteInfo.lpFile = (char*)Path.data();
  ExecuteInfo.lpParameters = (char*)Params.data();
  ExecuteInfo.nShow = SW_SHOW;

  Result = (ShellExecuteEx(&ExecuteInfo) != 0);
  if (Result)
  {
    Handle = ExecuteInfo.hProcess;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall ExecuteShellAndWait(HWND Handle, const AnsiString Path, 
  const AnsiString Params, TProcessMessagesEvent ProcessMessages)
{
  bool Result;

  TShellExecuteInfo ExecuteInfo;
  memset(&ExecuteInfo, 0, sizeof(ExecuteInfo));
  ExecuteInfo.cbSize = sizeof(ExecuteInfo);
  ExecuteInfo.fMask = SEE_MASK_NOCLOSEPROCESS;
  ExecuteInfo.hwnd = Handle;
  ExecuteInfo.lpFile = (char*)Path.data();
  ExecuteInfo.lpParameters = (char*)Params.data();
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
bool __fastcall ExecuteShellAndWait(HWND Handle, const AnsiString Command, 
  TProcessMessagesEvent ProcessMessages)
{
  AnsiString Program, Params, Dir;
  SplitCommand(Command, Program, Params, Dir);
  return ExecuteShellAndWait(Handle, Program, Params, ProcessMessages);
} 
//---------------------------------------------------------------------------
bool __fastcall SpecialFolderLocation(int PathID, AnsiString & Path)
{
  LPITEMIDLIST Pidl;
  char Buf[256];
  if (SHGetSpecialFolderLocation(NULL, PathID, &Pidl) == NO_ERROR &&
      SHGetPathFromIDList(Pidl, Buf))
  {
    Path = AnsiString(Buf);
    return true;
  }
  return false;
}
//---------------------------------------------------------------------------
AnsiString __fastcall ItemsFormatString(const AnsiString SingleItemFormat,
  const AnsiString MultiItemsFormat, int Count, const AnsiString FirstItem)
{
  AnsiString Result;
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
AnsiString __fastcall ItemsFormatString(const AnsiString SingleItemFormat,
  const AnsiString MultiItemsFormat, TStrings * Items)
{
  return ItemsFormatString(SingleItemFormat, MultiItemsFormat,
    Items->Count, (Items->Count > 0 ? Items->Strings[0] : AnsiString()));
}
//---------------------------------------------------------------------------
AnsiString __fastcall FileNameFormatString(const AnsiString SingleFileFormat,
  const AnsiString MultiFilesFormat, TStrings * Files, bool Remote)
{
  assert(Files != NULL);
  AnsiString Item;
  if (Files->Count > 0)
  {
    Item = Remote ? UnixExtractFileName(Files->Strings[0]) :
      ExtractFileName(Files->Strings[0]);
  }
  return ItemsFormatString(SingleFileFormat, MultiFilesFormat,
    Files->Count, Item);
}
//---------------------------------------------------------------------
AnsiString __fastcall FormatBytes(__int64 Bytes, bool UseOrders)
{
  AnsiString Result;

  if (!UseOrders || (Bytes < __int64(100*1024)))
  {
    Result = FormatFloat("#,##0 \"B\"", Bytes);
  }
  else if (Bytes < __int64(100*1024*1024))
  {
    Result = FormatFloat("#,##0 \"KB\"", Bytes / 1024);
  }
  else
  {
    Result = FormatFloat("#,##0 \"MB\"", Bytes / (1024*1024));
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall CopyToClipboard(AnsiString Text)
{
  HANDLE Data;
  void * DataPtr;

  if (OpenClipboard(0))
  {
    try
    {
      Data = GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, Text.Length() + 1);
      try
      {
        DataPtr = GlobalLock(Data);
        try
        {
          memcpy(DataPtr, Text.c_str(), Text.Length() + 1);
          EmptyClipboard();
          SetClipboardData(CF_TEXT, Data);
        }
        __finally
        {
          GlobalUnlock(Data);
        }
      }
      catch(...)
      {
        GlobalFree(Data);
        throw;
      }
    }
    __finally
    {
      CloseClipboard();
    }
  }
  else
  {
    throw Exception(Consts_SCannotOpenClipboard);
  }
}
//---------------------------------------------------------------------------
void __fastcall CopyToClipboard(TStrings * Strings)
{
  if (Strings->Count > 0)
  {
    if (Strings->Count == 1)
    {
      CopyToClipboard(Strings->Strings[0]);
    }
    else
    {
      CopyToClipboard(Strings->Text);
    }
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall UniqTempDir(const AnsiString BaseDir, const AnsiString Identity,
  bool Mask)
{
  AnsiString TempDir;
  do
  {
    TempDir = BaseDir.IsEmpty() ? SystemTemporaryDirectory() : BaseDir;
    TempDir = IncludeTrailingBackslash(TempDir) + Identity;
    if (Mask)
    {
      TempDir += "?????";
    }
    else
    {
      TempDir += IncludeTrailingBackslash(FormatDateTime("nnzzz", Now()));
    };
  }
  while (!Mask && DirectoryExists(TempDir));

  return TempDir;
}
//---------------------------------------------------------------------------
bool __fastcall DeleteDirectory(const AnsiString DirName)
{
  TSearchRec sr;
  bool retval = true;
  if (FindFirst(DirName + "\\*", faAnyFile, sr) == 0) // VCL Function
  {
    if (FLAGSET(sr.Attr, faDirectory))
    {
      if (sr.Name != "." && sr.Name != "..")
        retval = DeleteDirectory(DirName + "\\" + sr.Name);
    }
    else
    {
      retval = DeleteFile(DirName + "\\" + sr.Name);
    }

    if (retval)
    {
      while (FindNext(sr) == 0)
      { // VCL Function
        if (FLAGSET(sr.Attr, faDirectory))
        {
          if (sr.Name != "." && sr.Name != "..")
            retval = DeleteDirectory(DirName + "\\" + sr.Name);
        }
        else
        {
          retval = DeleteFile(DirName + "\\" + sr.Name);
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
AnsiString __fastcall TranslateExceptionMessage(const Exception * E)
{
  if (dynamic_cast<const EAccessViolation*>(E) != NULL)
  {
    return LoadStr(ACCESS_VIOLATION_ERROR);
  }
  else
  {
    return E->Message;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall FormatDateTimeSpan(const AnsiString TimeFormat, TDateTime DateTime)
{
  AnsiString Result;
  if (int(DateTime) > 0)
  {
    Result = IntToStr(int(DateTime)) + ", ";
  }
  // days are decremented, because when there is to many of them,
  // "integer overflow" error occures
  Result += FormatDateTime(TimeFormat, DateTime - int(DateTime));
  return Result;
}
//---------------------------------------------------------------------------
TLocalCustomCommand::TLocalCustomCommand()
{
}
//---------------------------------------------------------------------------
TLocalCustomCommand::TLocalCustomCommand(const AnsiString & FileName,
  const AnsiString & LocalFileName, const AnsiString & FileList) :
  TFileCustomCommand(FileName, FileList)
{
  FLocalFileName = LocalFileName;
}
//---------------------------------------------------------------------------
int __fastcall TLocalCustomCommand::PatternLen(int Index, char PatternCmd)
{
  int Len;
  if (PatternCmd == '^')
  {
    Len = 3;
  }
  else
  {
    Len = TFileCustomCommand::PatternLen(Index, PatternCmd);
  }
  return Len;
}
//---------------------------------------------------------------------------
bool __fastcall TLocalCustomCommand::PatternReplacement(int Index,
  const AnsiString & Pattern, AnsiString & Replacement, bool & Delimit)
{
  bool Result;
  if (Pattern == "!^!")
  {
    Replacement = FLocalFileName;
    Result = true;
  }
  else
  {
    Result = TFileCustomCommand::PatternReplacement(Index, Pattern, Replacement, Delimit);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TLocalCustomCommand::DelimitReplacement(
  AnsiString & /*Replacement*/, char /*Quote*/)
{
  // never delimit local commands
}
//---------------------------------------------------------------------------
bool __fastcall TLocalCustomCommand::HasLocalFileName(const AnsiString & Command)
{
  return FindPattern(Command, '^');
}
//---------------------------------------------------------------------------
bool __fastcall TLocalCustomCommand::IsFileCommand(const AnsiString & Command)
{
  return TFileCustomCommand::IsFileCommand(Command) || HasLocalFileName(Command);
}

