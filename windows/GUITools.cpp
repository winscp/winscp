//---------------------------------------------------------------------------
#define NO_WIN32_LEAN_AND_MEAN
#include <vcl.h>
#pragma hdrstop

#include <shlobj.h>
#include <Common.h>

#include "GUITools.h"
#include "GUIConfiguration.h"
#include <TextsWin.h>
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
void __fastcall OpenSessionInPutty(TSessionData * SessionData)
{
  AnsiString PuttyPath = GUIConfiguration->PuttyPath;
  if (FindFile(PuttyPath))
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

    if (!ExecuteShell(PuttyPath, FORMAT("-load \"%s\"", (SessionName))))
    {
      throw Exception(FMTLOAD(EXECUTE_APP_ERROR, (PuttyPath)));
    }
  }
  else
  {
    throw Exception(FMTLOAD(FILE_NOT_FOUND, (GUIConfiguration->PuttyPath)));
  }
}
//---------------------------------------------------------------------------
bool __fastcall ExecuteShell(const AnsiString Path, const AnsiString Params)
{
  return ((int)ShellExecute(NULL, "open", (char*)Path.data(),
    (char*)Params.data(), NULL, SW_SHOWNORMAL) > 32);
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
//---------------------------------------------------------------------------
