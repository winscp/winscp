//---------------------------------------------------------------------------
#define NO_WIN32_LEAN_AND_MEAN
#include <vcl.h>
#pragma hdrstop

#include <shlobj.h>

#include <Common.h>
#include <TextsWin.h>

#include "Tools.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TFontStyles __fastcall IntToFontStyles(int value)
{
  TFontStyles Result;
  for (int i = fsBold; i <= fsStrikeOut; i++)
  {
    if (value & 1)
    {
      Result << (TFontStyle)i;
    }
    value >>= 1;
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall FontStylesToInt(const TFontStyles value)
{
  int Result = 0;
  for (int i = fsStrikeOut; i >= fsBold; i--)
  {
    Result <<= 1;
    if (value.Contains((TFontStyle)i))
    {
      Result |= 1;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall CenterFormOn(TForm * Form, TControl * CenterOn)
{
  TPoint ScreenPoint = CenterOn->ClientToScreen(TPoint(0, 0));
  Form->Left = ScreenPoint.x + (CenterOn->Width / 2) - (Form->Width / 2);
  Form->Top = ScreenPoint.y + (CenterOn->Height / 2) - (Form->Height / 2);
}
//---------------------------------------------------------------------------
AnsiString __fastcall GetCoolbarLayoutStr(TCoolBar *CoolBar)
{
  assert(CoolBar);
  AnsiString Result;
  for (int Index = CoolBar->Bands->Count - 1; Index >= 0; Index--)
  {
    TCoolBand *Band = CoolBar->Bands->Items[Index];
    if (!Result.IsEmpty()) Result += ";";
    Result += FORMAT("%d,%d,%d,%d,%d", (Band->ID, (int)Band->Visible,
      (int)Band->Break, Band->Width, Band->Index));
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall LoadCoolbarLayoutStr(TCoolBar * CoolBar, AnsiString LayoutStr)
{
  assert(CoolBar);
  while (!LayoutStr.IsEmpty())
  {
    AnsiString BarStr = CutToChar(LayoutStr, ';', true);
    TCoolBand * Band = (TCoolBand*)(CoolBar->Bands->FindItemID(StrToInt(CutToChar(BarStr, ',', true))));
    if (Band)
    {
      Band->Visible = (bool)StrToInt(CutToChar(BarStr, ',', true));
      Band->Break = (bool)StrToInt(CutToChar(BarStr, ',', true));
      Band->Width = StrToInt(CutToChar(BarStr, ',', true));
      Band->Index = StrToInt(BarStr);
    }
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall GetListViewStr(TListView * ListView)
{
  AnsiString Result;
  for (int Index = 0; Index < ListView->Columns->Count; Index++)
  {
    if (!Result.IsEmpty())
    {
      Result += ",";
    }
    Result += IntToStr(ListView->Column[Index]->Width);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall LoadListViewStr(TListView * ListView, AnsiString LayoutStr)
{
  int Index = 0;
  while (!LayoutStr.IsEmpty() && (Index < ListView->Columns->Count))
  {
    ListView->Column[Index]->Width = StrToIntDef(
      CutToChar(LayoutStr, ',', true), ListView->Column[Index]->Width);
    Index++;  
  }
}
//---------------------------------------------------------------------------
void __fastcall SetCoolBandsMinWidth(TCoolBar * CoolBar)
{
  assert(CoolBar);
  for (int Index = 0; Index < CoolBar->Bands->Count; Index++)
  {
    TCoolBand * Band = CoolBar->Bands->Items[Index];
    assert(Band->Control);
    if (!Band->Control->Tag)
    {
      Band->MinWidth = Band->Control->Width;
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall ExecuteShellAndWait(const AnsiString Path, const AnsiString Params)
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
    unsigned long WaitResult;
    do
    {
      WaitResult = WaitForSingleObject(ExecuteInfo.hProcess, 200);
      if (WaitResult == WAIT_FAILED)
      {
        throw Exception(LoadStr(DOCUMENT_WAIT_ERROR));
      }
      Application->ProcessMessages();
    }
    while (WaitResult == WAIT_TIMEOUT);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall ExecuteShellAndWait(const AnsiString Command)
{
  AnsiString Program, Params, Dir;
  SplitCommand(Command, Program, Params, Dir);
  return ExecuteShellAndWait(Program, Params);
}
//---------------------------------------------------------------------------
void __fastcall CreateDesktopShortCut(const AnsiString &Name,
  const AnsiString &File, const AnsiString & Params, const AnsiString & Description,
  int SpecialFolder)
{
  IShellLink* pLink;
  IPersistFile* pPersistFile;
  LPMALLOC      ShellMalloc;
  LPITEMIDLIST  DesktopPidl;
  char DesktopDir[MAX_PATH];

  if (SpecialFolder < 0)
  {
    SpecialFolder = CSIDL_DESKTOPDIRECTORY;
  }

  try
  {
    if (FAILED(SHGetMalloc(&ShellMalloc))) throw Exception("");

    if (FAILED(SHGetSpecialFolderLocation(NULL, SpecialFolder, &DesktopPidl)))
    {
      throw Exception("");
    }

    if (!SHGetPathFromIDList(DesktopPidl, DesktopDir))
    {
      ShellMalloc->Free(DesktopPidl);
      ShellMalloc->Release();
      throw Exception("");
    }

    ShellMalloc->Free(DesktopPidl);
    ShellMalloc->Release();

    if (SUCCEEDED(CoInitialize(NULL)))
    {
      if(SUCCEEDED(CoCreateInstance(CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER,
          IID_IShellLink, (void **) &pLink)))
      {
        try
        {
          pLink->SetPath(File.c_str());
          pLink->SetDescription(Description.c_str());
          pLink->SetArguments(Params.c_str());
          pLink->SetShowCmd(SW_SHOW);

          if (SUCCEEDED(pLink->QueryInterface(IID_IPersistFile, (void **)&pPersistFile)))
          {
            try
            {
              WideString strShortCutLocation(DesktopDir);
              // Name can contain even path (e.g. to create quick launch icon)
              strShortCutLocation += AnsiString("\\") + Name + ".lnk";
              if (!SUCCEEDED(pPersistFile->Save(strShortCutLocation.c_bstr(), TRUE)))
              {
                throw Exception("");
              }
            }
            __finally
            {
              pPersistFile->Release();
            }
          }
        }
        __finally
        {
          pLink->Release();
        }
      }
      CoUninitialize();
    }
  }
  catch(...)
  {
    throw Exception(CREATE_SHORTCUT_ERROR);
  }
}
//---------------------------------------------------------------------------
void __fastcall ValidateMaskEdit(TComboBox * Edit)
{
  assert(Edit != NULL);
  TFileMasks Masks = Edit->Text;
  int Start, Length;
  if (!Masks.IsValid(Start, Length))
  {
    SimpleErrorDialog(FMTLOAD(MASK_ERROR, (Masks.Masks.SubString(Start + 1, Length))));
    Edit->SetFocus();
    Edit->SelStart = Start;
    Edit->SelLength = Length;
    Abort();
  }
}
//---------------------------------------------------------------------------
void __fastcall OpenBrowser(AnsiString URL)
{
  ShellExecute(Application->Handle, "open", URL.c_str(), NULL, NULL, SW_SHOWNORMAL);
}
