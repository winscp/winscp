//---------------------------------------------------------------------------
#define NO_WIN32_LEAN_AND_MEAN
#include <vcl.h>
#pragma hdrstop

#include <Consts.hpp>
#include <shlobj.h>
#include <stdio.h>
#define INITGUID
#include <propkey.h>

#include <Common.h>
#include <TextsWin.h>
#include <Exceptions.h>

#include "GUITools.h"
#include "VCLCommon.h"
#include "Setup.h"
#include "Tools.h"
#include <WinHelpViewer.hpp>
#include <PasTools.hpp>
#include <System.Win.ComObj.hpp>
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
UnicodeString __fastcall GetListViewStr(TListView * ListView)
{
  UnicodeString Result;
  for (int Index = 0; Index < ListView->Columns->Count; Index++)
  {
    if (!Result.IsEmpty())
    {
      Result += L",";
    }
    Result += IntToStr(ListView->Column[Index]->Width);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall LoadListViewStr(TListView * ListView, UnicodeString LayoutStr)
{
  int Index = 0;
  while (!LayoutStr.IsEmpty() && (Index < ListView->Columns->Count))
  {
    ListView->Column[Index]->Width = StrToIntDef(
      ::CutToChar(LayoutStr, L',', true), ListView->Column[Index]->Width);
    Index++;
  }
}
//---------------------------------------------------------------------------
void __fastcall RestoreForm(UnicodeString Data, TForm * Form)
{
  assert(Form);
  if (!Data.IsEmpty())
  {
    Forms::TMonitor * Monitor = FormMonitor(Form);

    TRect Bounds = Form->BoundsRect;
    int Left = StrToIntDef(::CutToChar(Data, L';', true), Bounds.Left);
    int Top = StrToIntDef(::CutToChar(Data, L';', true), Bounds.Top);
    bool DefaultPos = (Left == -1) && (Top == -1);
    if (!DefaultPos)
    {
      Bounds.Left = Left;
      Bounds.Top = Top;
    }
    else
    {
      Bounds.Left = 0;
      Bounds.Top = 0;
    }
    Bounds.Right = StrToIntDef(::CutToChar(Data, L';', true), Bounds.Right);
    Bounds.Bottom = StrToIntDef(::CutToChar(Data, L';', true), Bounds.Bottom);
    TWindowState State = (TWindowState)StrToIntDef(::CutToChar(Data, L';', true), (int)wsNormal);
    Form->WindowState = State;
    if (State == wsNormal)
    {
      // move to the target monitor
      OffsetRect(Bounds, Monitor->Left, Monitor->Top);

      // reduce window size to that of monitor size
      // (this does not cut window into monitor!)
      if (Bounds.Width() > Monitor->WorkareaRect.Width())
      {
        Bounds.Right -= (Bounds.Width() - Monitor->WorkareaRect.Width());
      }
      if (Bounds.Height() > Monitor->WorkareaRect.Height())
      {
        Bounds.Bottom -= (Bounds.Height() - Monitor->WorkareaRect.Height());
      }

      if (DefaultPos ||
          ((Bounds.Left < Monitor->Left) ||
           (Bounds.Left > Monitor->Left + Monitor->WorkareaRect.Width() - 20) ||
           (Bounds.Top < Monitor->Top) ||
           (Bounds.Top > Monitor->Top + Monitor->WorkareaRect.Height() - 20)))
      {
        if (Monitor->Primary)
        {
          if ((Application->MainForm == NULL) || (Application->MainForm == Form))
          {
            Form->Position = poDefaultPosOnly;
          }
          else
          {
            Form->Position = poOwnerFormCenter;
          }
          Form->Width = Bounds.Width();
          Form->Height = Bounds.Height();
        }
        else
        {
          // when positioning on non-primary monitor, we need
          // to handle that ourselves, so place window to center
          Form->SetBounds(Monitor->Left + ((Monitor->Width - Bounds.Width()) / 2),
            Monitor->Top + ((Monitor->Height - Bounds.Height()) / 2),
            Bounds.Width(), Bounds.Height());
          Form->Position = poDesigned;
        }
      }
      else
      {
        Form->Position = poDesigned;
        Form->BoundsRect = Bounds;
      }
    }
    else if (State == wsMaximized)
    {
      Form->Position = poDesigned;

      Bounds = Form->BoundsRect;
      OffsetRect(Bounds, Monitor->Left, Monitor->Top);
      Form->BoundsRect = Bounds;
    }
  }
  else if (Form->Position == poDesigned)
  {
    Form->Position = poDefaultPosOnly;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall StoreForm(TCustomForm * Form)
{
  assert(Form);
  TRect Bounds = Form->BoundsRect;
  OffsetRect(Bounds, -Form->Monitor->Left, -Form->Monitor->Top);
  return FORMAT(L"%d;%d;%d;%d;%d", ((int)Bounds.Left, (int)Bounds.Top,
    (int)Bounds.Right, (int)Bounds.Bottom,
    // we do not want WinSCP to start minimized next time (we cannot handle that anyway).
    // note that WindowState is wsNormal when window in minimized for some reason.
    // actually it is wsMinimized only when minimized by MSVDM
    (int)(Form->WindowState == wsMinimized ? wsNormal : Form->WindowState)));
}
//---------------------------------------------------------------------------
void __fastcall RestoreFormSize(UnicodeString Data, TForm * Form)
{
  int Width = StrToIntDef(::CutToChar(Data, L',', true), Form->Width);
  int Height = StrToIntDef(::CutToChar(Data, L',', true), Form->Height);
  ResizeForm(Form, Width, Height);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall StoreFormSize(TForm * Form)
{
  return FORMAT(L"%d,%d", (Form->Width, Form->Height));
}
//---------------------------------------------------------------------------
bool __fastcall ExecuteShellAndWait(const UnicodeString Path, const UnicodeString Params)
{
  return ExecuteShellAndWait(Application->Handle, Path, Params,
    &Application->ProcessMessages);
}
//---------------------------------------------------------------------------
bool __fastcall ExecuteShellAndWait(const UnicodeString Command)
{
  return ExecuteShellAndWait(Application->Handle, Command,
    &Application->ProcessMessages);
}
//---------------------------------------------------------------------------
IShellLink * __fastcall CreateDesktopShortCut(const UnicodeString & Name,
  const UnicodeString &File, const UnicodeString & Params, const UnicodeString & Description,
  int SpecialFolder, bool Return)
{
  IShellLink* pLink = NULL;

  if (SpecialFolder < 0)
  {
    SpecialFolder = CSIDL_DESKTOPDIRECTORY;
  }

  try
  {
    if (SUCCEEDED(CoCreateInstance(CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER,
         IID_IShellLink, (void **) &pLink)))
    {
      try
      {
        pLink->SetPath(File.c_str());
        pLink->SetDescription(Description.c_str());
        pLink->SetArguments(Params.c_str());
        pLink->SetShowCmd(SW_SHOW);
        // Explicitly setting icon file,
        // without this icons are not shown at least in Windows 7 jumplist
        pLink->SetIconLocation(File.c_str(), 0);

        IPersistFile* pPersistFile;
        if (!Return &&
            SUCCEEDED(pLink->QueryInterface(IID_IPersistFile, (void **)&pPersistFile)))
        {
          try
          {
            LPMALLOC      ShellMalloc;
            LPITEMIDLIST  DesktopPidl;
            wchar_t DesktopDir[MAX_PATH];

            OleCheck(SHGetMalloc(&ShellMalloc));

            try
            {
              OleCheck(SHGetSpecialFolderLocation(NULL, SpecialFolder, &DesktopPidl));

              OleCheck(SHGetPathFromIDList(DesktopPidl, DesktopDir));
            }
            __finally
            {
              ShellMalloc->Free(DesktopPidl);
              ShellMalloc->Release();
            }

            WideString strShortCutLocation(DesktopDir);
            // Name can contain even path (e.g. to create quick launch icon)
            strShortCutLocation += UnicodeString(L"\\") + Name + L".lnk";
            OleCheck(pPersistFile->Save(strShortCutLocation.c_bstr(), TRUE));
          }
          __finally
          {
            pPersistFile->Release();
          }
        }

        // this is necessary for Windows 7 taskbar jump list links
        IPropertyStore * PropertyStore;
        if (SUCCEEDED(pLink->QueryInterface(IID_IPropertyStore, (void**)&PropertyStore)))
        {
          PROPVARIANT Prop;
          Prop.vt = VT_LPWSTR;
          Prop.pwszVal = Name.c_str();
          PropertyStore->SetValue(PKEY_Title, Prop);
          PropertyStore->Commit();
          PropertyStore->Release();
        }
      }
      catch(...)
      {
        pLink->Release();
        throw;
      }

      if (!Return)
      {
        pLink->Release();
        pLink = NULL;
      }
    }
  }
  catch(Exception & E)
  {
    throw ExtException(&E, LoadStr(CREATE_SHORTCUT_ERROR));
  }

  return pLink;
}
//---------------------------------------------------------------------------
IShellLink * __fastcall CreateDesktopSessionShortCut(TSessionData * Session,
  const UnicodeString & Name, const UnicodeString & AdditionalParams,
  int SpecialFolder, bool Return)
{
  return
    CreateDesktopShortCut(ValidLocalFileName(Name), Application->ExeName,
      FORMAT(L"\"%s\"%s%s", (Session->SessionName, (AdditionalParams.IsEmpty() ? L"" : L" "), AdditionalParams)),
      FMTLOAD(SHORTCUT_INFO_TIP, (Session->SessionName, Session->InfoTip)),
      SpecialFolder, Return);
}
//---------------------------------------------------------------------------
template<class TEditControl>
void __fastcall ValidateMaskEditT(const UnicodeString & Mask, TEditControl * Edit, int ForceDirectoryMasks)
{
  assert(Edit != NULL);
  TFileMasks Masks(ForceDirectoryMasks);
  try
  {
    Masks = Mask;
  }
  catch(EFileMasksException & E)
  {
    ShowExtendedException(&E);
    Edit->SetFocus();
    // This does not work for TEdit and TMemo (descendants of TCustomEdit) anymore,
    // as it re-selects whole text on exception in TCustomEdit.CMExit
    Edit->SelStart = E.ErrorStart - 1;
    Edit->SelLength = E.ErrorLen;
    Abort();
  }
}
//---------------------------------------------------------------------------
void __fastcall ValidateMaskEdit(TComboBox * Edit)
{
  ValidateMaskEditT(Edit->Text, Edit, -1);
}
//---------------------------------------------------------------------------
void __fastcall ValidateMaskEdit(TEdit * Edit)
{
  ValidateMaskEditT(Edit->Text, Edit, -1);
}
//---------------------------------------------------------------------------
void __fastcall ValidateMaskEdit(TMemo * Edit, bool Directory)
{
  UnicodeString Mask = TFileMasks::ComposeMaskStr(GetUnwrappedMemoLines(Edit), Directory);
  ValidateMaskEditT(Mask, Edit, Directory ? 1 : 0);
}
//---------------------------------------------------------------------------
TStrings * __fastcall GetUnwrappedMemoLines(TMemo * Memo)
{
  TStrings * Result = new TStringList();
  // This removes soft linebreakes when text in memo wraps
  // (Memo->Lines includes soft linebreaks, while Memo->Text does not)
  Result->Text = Memo->Text;
  return Result;
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
void __fastcall OpenBrowser(UnicodeString URL)
{
  UnicodeString HomePageUrl = LoadStr(HOMEPAGE_URL);
  if (SameText(URL.SubString(1, HomePageUrl.Length()), HomePageUrl))
  {
    URL = CampaignUrl(URL);
  }
  ShellExecute(Application->Handle, L"open", URL.c_str(), NULL, NULL, SW_SHOWNORMAL);
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
HANDLE __fastcall OpenTextFromClipboard(const wchar_t *& Text)
{
  HANDLE Result = NULL;
  if (OpenClipboard(0))
  {
    // Check also for CF_TEXT?
    Result = GetClipboardData(CF_UNICODETEXT);
    if (Result != NULL)
    {
      Text = static_cast<const wchar_t*>(GlobalLock(Result));
    }
    else
    {
      CloseClipboard();
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall CloseTextFromClipboard(HANDLE Handle)
{
  if (Handle != NULL)
  {
    GlobalUnlock(Handle);
  }
  CloseClipboard();
}
//---------------------------------------------------------------------------
bool __fastcall TextFromClipboard(UnicodeString & Text)
{
  const wchar_t * AText = NULL;
  HANDLE Handle = OpenTextFromClipboard(AText);
  bool Result = (Handle != NULL);
  if (Result)
  {
    Text = AText;
    CloseTextFromClipboard(Handle);
  }
  return Result;
}
//---------------------------------------------------------------------------
static bool __fastcall GetResource(
  const UnicodeString ResName, void *& Content, unsigned long & Size)
{
  HRSRC Resource = FindResourceEx(HInstance, RT_RCDATA, ResName.c_str(),
    MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL));
  bool Result = (Resource != NULL);
  if (Result)
  {
    Size = SizeofResource(HInstance, Resource);
    if (!Size)
    {
      throw Exception(FORMAT(L"Cannot get size of resource %s", (ResName)));
    }

    Content = LoadResource(HInstance, Resource);
    if (!Content)
    {
      throw Exception(FORMAT(L"Cannot read resource %s", (ResName)));
    }

    Content = LockResource(Content);
    if (!Content)
    {
      throw Exception(FORMAT(L"Cannot lock resource %s", (ResName)));
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall DumpResourceToFile(const UnicodeString ResName,
  const UnicodeString FileName)
{
  void * Content;
  unsigned long Size;
  bool Result = GetResource(ResName, Content, Size);

  if (Result)
  {
    FILE * f = _wfopen(FileName.c_str(), L"wb");
    if (!f)
    {
      throw Exception(FORMAT(L"Cannot create file %s", (FileName)));
    }
    if (fwrite(Content, 1, Size, f) != Size)
    {
      throw Exception(FORMAT(L"Cannot write to file %s", (FileName)));
    }
    fclose(f);
  }

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ReadResource(const UnicodeString ResName)
{
  void * Content;
  unsigned long Size;
  UnicodeString Result;

  if (GetResource(ResName, Content, Size))
  {
    Result = UnicodeString(UTF8String(static_cast<char*>(Content), Size));
  }

  return Result;
}
//---------------------------------------------------------------------------
template <class T>
void __fastcall BrowseForExecutableT(T * Control, UnicodeString Title,
  UnicodeString Filter, bool FileNameCommand, bool Escape)
{
  UnicodeString Executable, Program, Params, Dir;
  Executable = Control->Text;
  if (FileNameCommand)
  {
    ReformatFileNameCommand(Executable);
  }
  SplitCommand(Executable, Program, Params, Dir);

  TOpenDialog * FileDialog = new TOpenDialog(Application);
  try
  {
    if (Escape)
    {
      Program = StringReplace(Program, L"\\\\", L"\\", TReplaceFlags() << rfReplaceAll);
    }
    UnicodeString ExpandedProgram = ExpandEnvironmentVariables(Program);
    FileDialog->FileName = ExpandedProgram;
    UnicodeString InitialDir = ExtractFilePath(ExpandedProgram);
    if (!InitialDir.IsEmpty())
    {
      FileDialog->InitialDir = InitialDir;
    }
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
          if (Escape)
          {
            Program = StringReplace(Program, L"\\", L"\\\\", TReplaceFlags() << rfReplaceAll);
          }
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
void __fastcall BrowseForExecutable(TEdit * Control, UnicodeString Title,
  UnicodeString Filter, bool FileNameCommand, bool Escape)
{
  BrowseForExecutableT(Control, Title, Filter, FileNameCommand, Escape);
}
//---------------------------------------------------------------------------
void __fastcall BrowseForExecutable(TComboBox * Control, UnicodeString Title,
  UnicodeString Filter, bool FileNameCommand, bool Escape)
{
  BrowseForExecutableT(Control, Title, Filter, FileNameCommand, Escape);
}
//---------------------------------------------------------------------------
bool __fastcall FontDialog(TFont * Font)
{
  bool Result;
  TFontDialog * Dialog = new TFontDialog(Application);
  try
  {
    Dialog->Device = fdScreen;
    Dialog->Options = TFontDialogOptions() << fdForceFontExist;
    Dialog->Font = Font;
    Result = Dialog->Execute();
    if (Result)
    {
      Font->Assign(Dialog->Font);
    }
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall SaveDialog(UnicodeString Title, UnicodeString Filter,
  UnicodeString DefaultExt, UnicodeString & FileName)
{
  bool Result;
  #if 0
  TFileSaveDialog * Dialog = new TFileSaveDialog(Application);
  try
  {
    Dialog->Title = Title;
    FilterToFileTypes(Filter, Dialog->FileTypes);
    Dialog->DefaultExtension = DefaultExt;
    Dialog->FileName = FileName;
    UnicodeString DefaultFolder = ExtractFilePath(FileName);
    if (!DefaultFolder.IsEmpty())
    {
      Dialog->DefaultFolder = DefaultFolder;
    }
    Dialog->Options = Dialog->Options << fdoOverWritePrompt << fdoForceFileSystem <<
      fdoPathMustExist << fdoNoReadOnlyReturn;
    Result = Dialog->Execute();
    if (Result)
    {
      FileName = Dialog->FileName;
    }
  }
  __finally
  {
    delete Dialog;
  }
  #else
  TSaveDialog * Dialog = new TSaveDialog(Application);
  try
  {
    Dialog->Title = Title;
    Dialog->Filter = Filter;
    Dialog->DefaultExt = DefaultExt;
    Dialog->FileName = FileName;
    UnicodeString InitialDir = ExtractFilePath(FileName);
    if (!InitialDir.IsEmpty())
    {
      Dialog->InitialDir = InitialDir;
    }
    Dialog->Options = Dialog->Options << ofOverwritePrompt << ofPathMustExist <<
      ofNoReadOnlyReturn;
    Result = Dialog->Execute();
    if (Result)
    {
      FileName = Dialog->FileName;
    }
  }
  __finally
  {
    delete Dialog;
  }
  #endif
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall CopyToClipboard(UnicodeString Text)
{
  HANDLE Data;
  void * DataPtr;

  if (OpenClipboard(0))
  {
    try
    {
      size_t Size = (Text.Length() + 1) * sizeof(wchar_t);
      Data = GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, Size);
      try
      {
        DataPtr = GlobalLock(Data);
        try
        {
          memcpy(DataPtr, Text.c_str(), Size);
          EmptyClipboard();
          SetClipboardData(CF_UNICODETEXT, Data);
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
    throw Exception(Vcl_Consts_SCannotOpenClipboard);
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
//---------------------------------------------------------------------------
void __fastcall ShutDownWindows()
{
  HANDLE Token;
  TOKEN_PRIVILEGES Priv;

  // Get a token for this process.
  Win32Check(OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &Token));

  // Get the LUID for the shutdown privilege.
  Win32Check(LookupPrivilegeValue(NULL, SE_SHUTDOWN_NAME, &Priv.Privileges[0].Luid));

  Priv.PrivilegeCount = 1;  // one privilege to set
  Priv.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;

  // Get the shutdown privilege for this process.
  Win32Check(AdjustTokenPrivileges(Token, FALSE, &Priv, 0, (PTOKEN_PRIVILEGES)NULL, 0));

  // Shut down the system and force all applications to close.
  Win32Check(ExitWindowsEx(EWX_SHUTDOWN | EWX_POWEROFF,
    SHTDN_REASON_MAJOR_OTHER | SHTDN_REASON_MINOR_OTHER | SHTDN_REASON_FLAG_PLANNED));
}
//---------------------------------------------------------------------------
void __fastcall EditSelectBaseName(HWND Edit)
{
  UnicodeString Text;
  Text.SetLength(GetWindowTextLength(Edit) + 1);
  GetWindowText(Edit, Text.c_str(), Text.Length());

  int P = Text.LastDelimiter(L".");
  if (P > 0)
  {
    // SendMessage does not work, if edit control is not fully
    // initialized yet
    PostMessage(Edit, EM_SETSEL, 0, P - 1);
  }
}
//---------------------------------------------------------------------------
// Code from http://gentoo.osuosl.org/distfiles/cl331.zip/io/

// The autoproxy functions were only documented in WinHTTP 5.1, so we have to
//   provide the necessary defines and structures ourselves
#ifndef WINHTTP_ACCESS_TYPE_DEFAULT_PROXY

#define HINTERNET HANDLE

typedef struct
{
  WORD dwAccessType;
  LPWSTR lpszProxy;
  LPWSTR lpszProxyBypass;
} WINHTTP_PROXY_INFO;

typedef struct {
  BOOL fAutoDetect;
  LPWSTR lpszAutoConfigUrl;
  LPWSTR lpszProxy;
  LPWSTR lpszProxyBypass;
} WINHTTP_CURRENT_USER_IE_PROXY_CONFIG;

#endif /* WinHTTP 5.1 defines and structures */

typedef BOOL (*WINHTTPGETDEFAULTPROXYCONFIGURATION)(WINHTTP_PROXY_INFO * pProxyInfo);
typedef BOOL (*WINHTTPGETIEPROXYCONFIGFORCURRENTUSER)(
  WINHTTP_CURRENT_USER_IE_PROXY_CONFIG * pProxyConfig);
//---------------------------------------------------------------------------
bool __fastcall AutodetectProxyUrl(UnicodeString & Proxy)
{
  static HMODULE WinHTTP = NULL;
  static WINHTTPGETDEFAULTPROXYCONFIGURATION WinHttpGetDefaultProxyConfiguration = NULL;
  static WINHTTPGETIEPROXYCONFIGFORCURRENTUSER WinHttpGetIEProxyConfigForCurrentUser = NULL;

  bool Result = true;

  /* Under Win2K SP3, XP and 2003 (or at least Windows versions with
     WinHTTP 5.1 installed in some way, it officially shipped with the
     versions mentioned earlier) we can use WinHTTP AutoProxy support,
     which implements the Web Proxy Auto-Discovery (WPAD) protocol from
     an internet draft that expired in May 2001.  Under older versions of
     Windows we have to use the WinINet InternetGetProxyInfo, however this
     consists of a ghastly set of kludges that were never meant to be
     exposed to the outside world (they were only crowbarred out of MS
     as part of the DoJ consent decree), and user experience with them is
     that they don't really work except in the one special way in which
     MS-internal code calls them.  Since we don't know what this is, we
     use the WinHTTP functions instead */
  if (WinHTTP == NULL)
  {
    if ((WinHTTP = LoadLibrary(L"WinHTTP.dll")) == NULL)
    {
      Result = false;
    }
    else
    {
      WinHttpGetDefaultProxyConfiguration = (WINHTTPGETDEFAULTPROXYCONFIGURATION)
        GetProcAddress(WinHTTP, "WinHttpGetDefaultProxyConfiguration");
      WinHttpGetIEProxyConfigForCurrentUser = (WINHTTPGETIEPROXYCONFIGFORCURRENTUSER)
        GetProcAddress(WinHTTP, "WinHttpGetIEProxyConfigForCurrentUser");
      if ((WinHttpGetDefaultProxyConfiguration == NULL) ||
          (WinHttpGetIEProxyConfigForCurrentUser == NULL))
      {
        FreeLibrary(WinHTTP);
        Result = false;
      }
    }
  }

  if (Result)
  {
    Result = false;

    /* Forst we try for proxy info direct from the registry if
       it's available. */
    if (!Result)
    {
      WINHTTP_PROXY_INFO ProxyInfo;
      memset(&ProxyInfo, 0, sizeof(ProxyInfo));
      if ((WinHttpGetDefaultProxyConfiguration != NULL) &&
          WinHttpGetDefaultProxyConfiguration(&ProxyInfo))
      {
        if (ProxyInfo.lpszProxy != NULL)
        {
          Proxy = ProxyInfo.lpszProxy;
          GlobalFree(ProxyInfo.lpszProxy);
          Result = true;
        }
        if (ProxyInfo.lpszProxyBypass != NULL)
        {
          GlobalFree(ProxyInfo.lpszProxyBypass);
        }
      }
    }

    /* The next fallback is to get the proxy info from MSIE.  This is also
       usually much quicker than WinHttpGetProxyForUrl(), although sometimes
       it seems to fall back to that, based on the longish delay involved.
       Another issue with this is that it won't work in a service process
       that isn't impersonating an interactive user (since there isn't a
       current user), but in that case we just fall back to
       WinHttpGetProxyForUrl() */
    if (!Result)
    {
      WINHTTP_CURRENT_USER_IE_PROXY_CONFIG IEProxyInfo;
      memset(&IEProxyInfo, 0, sizeof(IEProxyInfo));
      if ((WinHttpGetIEProxyConfigForCurrentUser != NULL) &&
          WinHttpGetIEProxyConfigForCurrentUser(&IEProxyInfo))
      {
        if (IEProxyInfo.lpszProxy != NULL)
        {
          Proxy = IEProxyInfo.lpszProxy;
          GlobalFree(IEProxyInfo.lpszProxy);
          Result = true;
        }
        if (IEProxyInfo.lpszAutoConfigUrl != NULL)
        {
          GlobalFree(IEProxyInfo.lpszAutoConfigUrl);
        }
        if (IEProxyInfo.lpszProxyBypass != NULL)
        {
          GlobalFree(IEProxyInfo.lpszProxyBypass);
        }
      }
    }

    // We can also use WinHttpGetProxyForUrl, but it is lengthy
    // See the source address of the code for example
  }
  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TWinHelpTester : public TInterfacedObject, public IWinHelpTester
{
public:
  virtual bool __fastcall CanShowALink(const UnicodeString ALink, const UnicodeString FileName);
  virtual bool __fastcall CanShowTopic(const UnicodeString Topic, const UnicodeString FileName);
  virtual bool __fastcall CanShowContext(const int Context, const UnicodeString FileName);
  virtual TStringList * __fastcall GetHelpStrings(const UnicodeString ALink);
  virtual UnicodeString __fastcall GetHelpPath();
  virtual UnicodeString __fastcall GetDefaultHelpFile();

  IUNKNOWN
};
//---------------------------------------------------------------------------
class TCustomHelpSelector : public TInterfacedObject, public IHelpSelector
{
public:
  __fastcall TCustomHelpSelector(const UnicodeString & Name);

  virtual int __fastcall SelectKeyword(TStrings * Keywords);
  virtual int __fastcall TableOfContents(TStrings * Contents);

  IUNKNOWN

private:
  UnicodeString FName;
};
//---------------------------------------------------------------------------
void __fastcall AssignHelpSelector(IHelpSelector * HelpSelector)
{
  _di_IHelpSystem HelpSystem;
  if (GetHelpSystem(HelpSystem))
  {
    HelpSystem->AssignHelpSelector(HelpSelector);
  }
}
//---------------------------------------------------------------------------
void __fastcall InitializeCustomHelp(ICustomHelpViewer * HelpViewer)
{
  _di_IHelpManager HelpManager;
  RegisterViewer(HelpViewer, HelpManager);

  // Register dummy tester that disables win help
  WinHelpTester = new TWinHelpTester();

  AssignHelpSelector(new TCustomHelpSelector(HelpViewer->GetViewerName()));
}
//---------------------------------------------------------------------------
void __fastcall FinalizeCustomHelp()
{
  AssignHelpSelector(NULL);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
bool __fastcall TWinHelpTester::CanShowALink(const UnicodeString ALink,
  const UnicodeString FileName)
{
  return !Application->HelpFile.IsEmpty();
}
//---------------------------------------------------------------------------
bool __fastcall TWinHelpTester::CanShowTopic(const UnicodeString Topic,
  const UnicodeString FileName)
{
  assert(false);
  return !Application->HelpFile.IsEmpty();
}
//---------------------------------------------------------------------------
bool __fastcall TWinHelpTester::CanShowContext(const int /*Context*/,
  const UnicodeString FileName)
{
  assert(false);
  return !Application->HelpFile.IsEmpty();
}
//---------------------------------------------------------------------------
TStringList * __fastcall TWinHelpTester::GetHelpStrings(const UnicodeString ALink)
{
  assert(false);
  TStringList * Result = new TStringList();
  Result->Add(ViewerName + L": " + ALink);
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWinHelpTester::GetHelpPath()
{
  // never called on windows anyway
  return ExtractFilePath(Application->HelpFile);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TWinHelpTester::GetDefaultHelpFile()
{
  return Application->HelpFile;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TCustomHelpSelector::TCustomHelpSelector(const UnicodeString & Name) :
  FName(Name)
{
}
//---------------------------------------------------------------------------
int __fastcall TCustomHelpSelector::SelectKeyword(TStrings * /*Keywords*/)
{
  FAIL;
  return 0;
}
//---------------------------------------------------------------------------
int __fastcall TCustomHelpSelector::TableOfContents(TStrings * Contents)
{
  return Contents->IndexOf(FName);
}
