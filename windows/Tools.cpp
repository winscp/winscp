//---------------------------------------------------------------------------
#define NO_WIN32_LEAN_AND_MEAN
#include <vcl.h>
#pragma hdrstop

#include <Consts.hpp>
#include <shlobj.h>
#include <stdio.h>

#include <Common.h>
#include <TextsWin.h>

#include "GUITools.h"
#include "VCLCommon.h"
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
    TMonitor * Monitor = FormMonitor(Form);

    TRect Bounds = Form->BoundsRect;
    int Left = StrToIntDef(::CutToChar(Data, ';', true), Bounds.Left);
    int Top = StrToIntDef(::CutToChar(Data, ';', true), Bounds.Top);
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
    Bounds.Right = StrToIntDef(::CutToChar(Data, ';', true), Bounds.Right);
    Bounds.Bottom = StrToIntDef(::CutToChar(Data, ';', true), Bounds.Bottom);
    TWindowState State = (TWindowState)StrToIntDef(::CutToChar(Data, ';', true), (int)wsNormal);
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
            Form->Position = poMainFormCenter;
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
AnsiString __fastcall StoreForm(TCustomForm * Form)
{
  assert(Form);
  TRect Bounds = Form->BoundsRect;
  OffsetRect(Bounds, -Form->Monitor->Left, -Form->Monitor->Top);
  return FORMAT("%d;%d;%d;%d;%d", ((int)Bounds.Left, (int)Bounds.Top,
    (int)Bounds.Right, (int)Bounds.Bottom,
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
HANDLE __fastcall OpenTextFromClipboard(const char *& Text)
{
  HANDLE Result = NULL;
  if (OpenClipboard(0))
  {
    Result = GetClipboardData(CF_TEXT);
    if (Result != NULL)
    {
      Text = static_cast<const char*>(GlobalLock(Result));
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
bool __fastcall TextFromClipboard(AnsiString & Text)
{
  const char * AText = NULL;
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
  const AnsiString ResName, void *& Content, unsigned long & Size)
{
  HRSRC Resource = FindResourceEx(HInstance, RT_RCDATA, ResName.c_str(),
    MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL));
  bool Result = (Resource != NULL);
  if (Result)
  {
    Size = SizeofResource(HInstance, Resource);
    if (!Size)
    {
      throw Exception(FORMAT("Cannot get size of resource %s", (ResName)));
    }

    Content = LoadResource(HInstance, Resource);
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
class TChildCommonDialog : public T
{
public:
  __fastcall TChildCommonDialog(TComponent * AOwner) :
    T(AOwner)
  {
  }

protected:
  // all common-dialog structures we use (save, open, font) start like this:
  typedef struct
  {
      DWORD        lStructSize;
      HWND         hwndOwner;
  } COMMONDLG;

  virtual BOOL __fastcall TaskModalDialog(void * DialogFunc, void * DialogData)
  {
    COMMONDLG * CommonDlg = static_cast<COMMONDLG *>(DialogData);
    HWND Parent = GetCorrectFormParent();
    if (Parent != NULL)
    {
      CommonDlg->hwndOwner = Parent;
    }

    return T::TaskModalDialog(DialogFunc, DialogData);
  }
};
//---------------------------------------------------------------------------
// without this intermediate class, failure occurs during construction in release version
#define CHILDCOMMONDIALOG(DIALOG) \
  class TChild ## DIALOG ## Dialog : public TChildCommonDialog<T ## DIALOG ## Dialog> \
  { \
    public: \
      __fastcall TChild ## DIALOG ## Dialog(TComponent * AOwner) : \
        TChildCommonDialog<T ## DIALOG ## Dialog>(AOwner) \
      { \
      } \
  }
//---------------------------------------------------------------------------
CHILDCOMMONDIALOG(Open);
CHILDCOMMONDIALOG(Save);
CHILDCOMMONDIALOG(Font);
//---------------------------------------------------------------------------
TOpenDialog * __fastcall CreateOpenDialog(TComponent * AOwner)
{
  return new TChildOpenDialog(AOwner);
}
//---------------------------------------------------------------------------
template <class T>
void __fastcall BrowseForExecutableT(T * Control, AnsiString Title,
  AnsiString Filter, bool FileNameCommand, bool Escape)
{
  AnsiString Executable, Program, Params, Dir;
  Executable = Control->Text;
  if (FileNameCommand)
  {
    ReformatFileNameCommand(Executable);
  }
  SplitCommand(Executable, Program, Params, Dir);

  TOpenDialog * FileDialog = CreateOpenDialog(Application);
  try
  {
    if (Escape)
    {
      Program = StringReplace(Program, "\\\\", "\\", TReplaceFlags() << rfReplaceAll);
    }
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
          if (Escape)
          {
            Program = StringReplace(Program, "\\", "\\\\", TReplaceFlags() << rfReplaceAll);
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
void __fastcall BrowseForExecutable(TEdit * Control, AnsiString Title,
  AnsiString Filter, bool FileNameCommand, bool Escape)
{
  BrowseForExecutableT(Control, Title, Filter, FileNameCommand, Escape);
}
//---------------------------------------------------------------------------
void __fastcall BrowseForExecutable(TComboBox * Control, AnsiString Title,
  AnsiString Filter, bool FileNameCommand, bool Escape)
{
  BrowseForExecutableT(Control, Title, Filter, FileNameCommand, Escape);
}
//---------------------------------------------------------------------------
bool __fastcall FontDialog(TFont * Font)
{
  bool Result;
  TFontDialog * Dialog = new TChildFontDialog(Application);
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
bool __fastcall SaveDialog(AnsiString Title, AnsiString Filter,
  AnsiString DefaultExt, AnsiString & FileName)
{
  bool Result;
  TSaveDialog * Dialog = new TChildSaveDialog(Application);
  try
  {
    Dialog->Title = Title;
    Dialog->Filter = Filter;
    Dialog->DefaultExt = DefaultExt;
    Dialog->FileName = FileName;
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
bool __fastcall AutodetectProxyUrl(AnsiString & Proxy)
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
    if ((WinHTTP = LoadLibrary("WinHTTP.dll")) == NULL)
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
  virtual __fastcall ~TWinHelpTester();
  virtual bool __fastcall CanShowALink(const AnsiString ALink, const AnsiString FileName);
  virtual bool __fastcall CanShowTopic(const AnsiString Topic, const AnsiString FileName);
  virtual bool __fastcall CanShowContext(const int Context, const AnsiString FileName);
  virtual TStringList * __fastcall GetHelpStrings(const AnsiString ALink);
  virtual AnsiString __fastcall GetHelpPath();
  virtual AnsiString __fastcall GetDefaultHelpFile();

  IUNKNOWN
};
//---------------------------------------------------------------------------
ICustomHelpViewer * CustomHelpViewer = NULL;
_di_IHelpManager * PHelpManager = NULL;
IUnknown * HelpManagerUnknown = NULL;
TObjectList * ViewerList = NULL;
ICustomHelpViewer * WinHelpViewer = NULL;
//---------------------------------------------------------------------------
static int __fastcall ViewerID(int Index)
{
  // 8 is offset from THelpViewerNode to THelpViewerNode::FViewer
  return
    *reinterpret_cast<int *>(reinterpret_cast<char *>(ViewerList->Items[Index])
    + 8);
}
//---------------------------------------------------------------------------
static void __fastcall InternalShutdown(int Index)
{
  assert(PHelpManager != NULL);
  if (PHelpManager != NULL)
  {
    // override conflict of two Release() methods by getting
    // IHelpManager explicitly using operator *
    (**PHelpManager).Release(ViewerID(Index));
    // registration to help manager increases refcount by 2, but deregistration
    // by one only
    CustomHelpViewer->Release();
  }
}
//---------------------------------------------------------------------------
void __fastcall InitializeCustomHelp(ICustomHelpViewer * HelpViewer)
{
  assert(PHelpManager == NULL);

  CustomHelpViewer = HelpViewer;

  // workaround
  // _di_IHelpManager cannot be instantiated due to either bug in compiler or
  // VCL code
  PHelpManager = (_di_IHelpManager*)malloc(sizeof(_di_IHelpManager));
  // our own reference
  CustomHelpViewer->AddRef();
  RegisterViewer(CustomHelpViewer, *PHelpManager);
  HelpManagerUnknown = dynamic_cast<IUnknown *>(&**PHelpManager);

  // 40 is offset from IHelpManager to THelpManager
  // 16 is offset from THelpManager to THelpManager::FViewerList
  ViewerList =
    *reinterpret_cast<TObjectList **>(reinterpret_cast<char *>(&**PHelpManager)
    - 40 + 16);
  assert(ViewerList->Count == 2);

  // gross hack
  // Due to major bugs in VCL help system, unregister winhelp at all.
  // To do this we must call RegisterViewer first to get HelpManager.
  // Due to another bug, viewers must be unregistred in order reversed to
  // registration order, so we must unregister out help viewer first
  // and register it again at the end
  InternalShutdown(1);
  assert(ViewerList->Count == 1);
  int WinHelpViewerID = ViewerID(0);
  // 4 is offset from THelpViewerNode to THelpViewerNode::FViewer
  WinHelpViewer =
    *reinterpret_cast<_di_ICustomHelpViewer *>(reinterpret_cast<char *>(ViewerList->Items[0])
    + 4);
  // our reference
  // we cannot release win help viewer completelly here as finalization code of
  // WinHelpViewer expect it to exist
  WinHelpViewer->AddRef();
  (**PHelpManager).Release(WinHelpViewerID);
  // remove forgoten 3 references of manager
  WinHelpViewer->Release();
  WinHelpViewer->Release();
  WinHelpViewer->Release();
  assert(ViewerList->Count == 0);
  // this clears reference to manager in TWinHelpViewer::FHelpManager,
  // preventing call to TWinHelpViewer::InternalShutdown from finalization code
  // of WinHelpViewer
  WinHelpViewer->ShutDown();

  RegisterViewer(CustomHelpViewer, *PHelpManager);
  // we've got second reference to the same pointer here, release it
  HelpManagerUnknown->Release();
  assert(ViewerList->Count == 1);

  // now when winhelp is not registered, the tester is not used anyway
  // for any real work, but we use it as a hook to be called after
  // finalization of WinHelpViewer (see its finalization section)
  WinHelpTester = new TWinHelpTester();
}
//---------------------------------------------------------------------------
void __fastcall FinalizeCustomHelp()
{
  if (CustomHelpViewer != NULL)
  {
    // unregister ourselves to release both references
    InternalShutdown(0);
    // our own reference
    CustomHelpViewer->Release();
  }

  if (PHelpManager != NULL)
  {
    assert(ViewerList->Count == 0);

    // our reference
    HelpManagerUnknown->Release();
    free(PHelpManager);
    PHelpManager = NULL;
    HelpManagerUnknown = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall CleanUpWinHelp()
{
  // WinHelpViewer finalization code should have been called by now already,
  // so we can safely remove the last reference to destroy it
  assert(WinHelpViewer != NULL);
  WinHelpViewer->Release();
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TWinHelpTester::~TWinHelpTester()
{
  CleanUpWinHelp();
}
//---------------------------------------------------------------------------
bool __fastcall TWinHelpTester::CanShowALink(const AnsiString ALink,
  const AnsiString FileName)
{
  assert(false);
  return !Application->HelpFile.IsEmpty();
}
//---------------------------------------------------------------------------
bool __fastcall TWinHelpTester::CanShowTopic(const AnsiString Topic,
  const AnsiString FileName)
{
  assert(false);
  return !Application->HelpFile.IsEmpty();
}
//---------------------------------------------------------------------------
bool __fastcall TWinHelpTester::CanShowContext(const int /*Context*/,
  const AnsiString FileName)
{
  assert(false);
  return !Application->HelpFile.IsEmpty();
}
//---------------------------------------------------------------------------
TStringList * __fastcall TWinHelpTester::GetHelpStrings(const AnsiString ALink)
{
  assert(false);
  TStringList * Result = new TStringList();
  Result->Add(ViewerName + ": " + ALink);
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TWinHelpTester::GetHelpPath()
{
  assert(false);
  // never called on windows anyway
  return ExtractFilePath(Application->HelpFile);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TWinHelpTester::GetDefaultHelpFile()
{
  assert(false);
  return Application->HelpFile;
}
