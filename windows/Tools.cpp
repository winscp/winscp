//---------------------------------------------------------------------------
#define NO_WIN32_LEAN_AND_MEAN
#include <vcl.h>
#pragma hdrstop

#include <shlobj.h>
#include <stdio.h>

#include <Common.h>
#include <TextsWin.h>

#include "GUITools.h"
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
void __fastcall RestoreForm(AnsiString Data, TForm * Form)
{
  assert(Form);
  if (!Data.IsEmpty())
  {
    TRect Bounds = Form->BoundsRect;
    int Left = StrToIntDef(::CutToChar(Data, ';', true), Bounds.Left);
    int Top = StrToIntDef(::CutToChar(Data, ';', true), Bounds.Top);
    bool Center = (Left == -1) && (Top == -1);
    if (!Center)
    {
      Bounds.Left = Left;
      Bounds.Top = Top;
    }
    else
    {
      Bounds.Left = 0;
      Bounds.Top = 0;
    }
    Bounds.Right = StrToIntDef(::CutToChar(Data, ';', true), Bounds.Right);
    Bounds.Bottom = StrToIntDef(::CutToChar(Data, ';', true), Bounds.Bottom);
    TWindowState State = (TWindowState)StrToIntDef(::CutToChar(Data, ';', true), (int)wsNormal);
    Form->WindowState = State;
    if (State == wsNormal)
    {
      if (Bounds.Width() > Screen->Width) Bounds.Right -= (Bounds.Width() - Screen->Width);
      if (Bounds.Height() > Screen->Height) Bounds.Bottom -= (Bounds.Height() - Screen->Height);
      #define POS_RANGE(x, prop) (x < 0) || (x > Screen->prop)
      if (Center)
      {
        Form->Position = poMainFormCenter;
        Form->Width = Bounds.Width();
        Form->Height = Bounds.Height();
      }
      else if (POS_RANGE(Bounds.Left, Width - 20) ||
               POS_RANGE(Bounds.Top, Height - 40))
      {
        Form->Position = poDefaultPosOnly;
        Form->Width = Bounds.Width();
        Form->Height = Bounds.Height();
      }
      else
      {
        Form->Position = poDesigned;
        Form->BoundsRect = Bounds;
      }
      #undef POS_RANGE
    }
  }
  else if (Form->Position == poDesigned)
  {
    Form->Position = poDefaultPosOnly;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall StoreForm(TCustomForm * Form)
{
  assert(Form);
  return FORMAT("%d;%d;%d;%d;%d", ((int)Form->BoundsRect.Left, (int)Form->BoundsRect.Top,
    (int)Form->BoundsRect.Right, (int)Form->BoundsRect.Bottom,
    // we do not want WinSCP to start minimized next time (we cannot handle that anyway).
    // note that WindowState is wsNormal when window in minimized for some reason.
    // actually it is wsMinimized only when minimized by MSVDM
    (int)(Form->WindowState == wsMinimized ? wsNormal : Form->WindowState)));
}
//---------------------------------------------------------------------------
bool __fastcall ExecuteShellAndWait(const AnsiString Path, const AnsiString Params)
{
  return ExecuteShellAndWait(Application->Handle, Path, Params,
    &Application->ProcessMessages);
}
//---------------------------------------------------------------------------
bool __fastcall ExecuteShellAndWait(const AnsiString Command)
{
  return ExecuteShellAndWait(Application->Handle, Command,
    &Application->ProcessMessages);
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
void __fastcall ValidateMaskEdit(TEdit * Edit)
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
void __fastcall ExitActiveControl(TForm * Form)
{
  if (Form->ActiveControl != NULL)
  {
    TNotifyEvent OnExit = ((TEdit*)Form->ActiveControl)->OnExit;
    if (OnExit != NULL)
    {
      OnExit(Form->ActiveControl);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall OpenBrowser(AnsiString URL)
{
  ShellExecute(Application->Handle, "open", URL.c_str(), NULL, NULL, SW_SHOWNORMAL);
}
//---------------------------------------------------------------------------
bool __fastcall IsFormatInClipboard(unsigned int Format)
{
  bool Result = OpenClipboard(0);
  if (Result)
  {
    Result = IsClipboardFormatAvailable(Format);
    CloseClipboard();
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TextFromClipboard(AnsiString & Text)
{
  bool Result = OpenClipboard(0);
  if (Result)
  {
    HANDLE Handle = NULL;
    try
    {
      Handle = GetClipboardData(CF_TEXT);
      Result = (Handle != NULL);
      if (Result)
      {
        Text = static_cast<const char*>(GlobalLock(Handle));
      }
    }
    __finally
    {
      if (Handle != NULL)
      {
        GlobalUnlock(Handle);
      }
      CloseClipboard();
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
static bool __fastcall GetResource(
  const AnsiString ResName, void *& Content, unsigned long & Size)
{
  HRSRC Resource;
  Resource = FindResourceEx(NULL, RT_RCDATA, ResName.c_str(),
    MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL));
  bool Result = (Resource != NULL);
  if (Result)
  {
    Size = SizeofResource(NULL, Resource);
    if (!Size)
    {
      throw Exception(FORMAT("Cannot get size of resource %s", (ResName)));
    }

    Content = LoadResource(NULL, Resource);
    if (!Content)
    {
      throw Exception(FORMAT("Cannot read resource %s", (ResName)));
    }

    Content = LockResource(Content);
    if (!Content)
    {
      throw Exception(FORMAT("Cannot lock resource %s", (ResName)));
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall DumpResourceToFile(const AnsiString ResName,
  const AnsiString FileName)
{
  void * Content;
  unsigned long Size;
  bool Result = GetResource(ResName, Content, Size);

  if (Result)
  {
    FILE * f = fopen(FileName.c_str(), "wb");
    if (!f)
    {
      throw Exception(FORMAT("Cannot create file %s", (FileName)));
    }
    if (fwrite(Content, 1, Size, f) != Size)
    {
      throw Exception(FORMAT("Cannot write to file %s", (FileName)));
    }
    fclose(f);
  }

  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall ReadResource(const AnsiString ResName)
{
  void * Content;
  unsigned long Size;
  AnsiString Result;

  if (GetResource(ResName, Content, Size))
  {
    Result = AnsiString(static_cast<char*>(Content), Size);
  }

  return Result;
}
//---------------------------------------------------------------------------
template <class T>
void __fastcall BrowseForExecutableT(T * Control, AnsiString Title,
  AnsiString Filter, bool FileNameCommand)
{
  AnsiString Executable, Program, Params, Dir;
  Executable = Control->Text;
  if (FileNameCommand)
  {
    ReformatFileNameCommand(Executable);
  }
  SplitCommand(Executable, Program, Params, Dir);

  TOpenDialog * FileDialog = new TOpenDialog(Application);
  try
  {
    AnsiString ExpandedProgram = ExpandEnvironmentVariables(Program);
    FileDialog->FileName = ExpandedProgram;
    FileDialog->Filter = Filter;
    FileDialog->Title = Title;

    if (FileDialog->Execute())
    {
      TNotifyEvent PrevOnChange = Control->OnChange;
      Control->OnChange = NULL;
      try
      {
        // preserve unexpanded file, if the destination has not changed actually
        if (!CompareFileName(ExpandedProgram, FileDialog->FileName))
        {
          Program = FileDialog->FileName;
        }
        Control->Text = FormatCommand(Program, Params);
      }
      __finally
      {
        Control->OnChange = PrevOnChange;
      }

      if (Control->OnExit != NULL)
      {
        Control->OnExit(Control);
      }
    }
  }
  __finally
  {
    delete FileDialog;
  }
}
//---------------------------------------------------------------------------
void __fastcall BrowseForExecutable(TEdit * Control, AnsiString Title,
  AnsiString Filter, bool FileNameCommand)
{
  BrowseForExecutableT(Control, Title, Filter, FileNameCommand);
}
//---------------------------------------------------------------------------
void __fastcall BrowseForExecutable(TComboBox * Control, AnsiString Title,
  AnsiString Filter, bool FileNameCommand)
{
  BrowseForExecutableT(Control, Title, Filter, FileNameCommand);
}
//---------------------------------------------------------------------------
bool __fastcall IsWin64()
{
  static int Result = -1;
  if (Result < 0)
  {
    typedef BOOL WINAPI (*IsWow64ProcessType)(HANDLE Process, PBOOL Wow64Process);

    Result = 0;

    HMODULE Kernel = GetModuleHandle(kernel32);
    if (Kernel != NULL)
    {
      IsWow64ProcessType IsWow64Process =
        (IsWow64ProcessType)GetProcAddress(Kernel, "IsWow64Process");
      if (IsWow64Process != NULL)
      {
        BOOL Wow64Process = FALSE;
        if (IsWow64Process(GetCurrentProcess(), &Wow64Process))
        {
          if (Wow64Process)
          {
            Result = 1;
          }
        }
      }
    }
  }

  return (Result > 0);
}
