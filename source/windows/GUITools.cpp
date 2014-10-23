//---------------------------------------------------------------------------
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
#include <Math.hpp>
#include <WebBrowserEx.hpp>
#include <Tools.h>
#include "PngImageList.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
extern const UnicodeString PageantTool = L"pageant.exe";
extern const UnicodeString PuttygenTool = L"puttygen.exe";
//---------------------------------------------------------------------------
bool __fastcall FindFile(UnicodeString & Path)
{
  bool Result = FileExists(ApiPath(Path));
  if (!Result)
  {
    UnicodeString Paths = GetEnvironmentVariable(L"PATH");
    if (!Paths.IsEmpty())
    {
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
void __fastcall OpenSessionInPutty(const UnicodeString PuttyPath,
  TSessionData * SessionData)
{
  UnicodeString Program, AParams, Dir;
  SplitCommand(PuttyPath, Program, AParams, Dir);
  Program = ExpandEnvironmentVariables(Program);
  if (FindFile(Program))
  {

    AParams = ExpandEnvironmentVariables(AParams);
    UnicodeString Password = GUIConfiguration->PuttyPassword ? SessionData->Password : UnicodeString();
    TCustomCommandData Data(SessionData, SessionData->UserName, Password);
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
  if (!FileExists(ApiPath(Path)))
  {
    Path = AppPath + L"PuTTY\\" + Name;
    if (!FileExists(ApiPath(Path)))
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
  UnicodeString Result;
  SpecialFolderLocation(CSIDL_PERSONAL, Result);

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
  UnicodeString Result;
  SpecialFolderLocation(CSIDL_DESKTOPDIRECTORY, Result);

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
    }
  }
  while (!Mask && DirectoryExists(ApiPath(TempDir)));

  return TempDir;
}
//---------------------------------------------------------------------------
bool __fastcall DeleteDirectory(const UnicodeString DirName)
{
  TSearchRecChecked sr;
  bool retval = true;
  if (FindFirstUnchecked(DirName + L"\\*", faAnyFile, sr) == 0) // VCL Function
  {
    if (FLAGSET(sr.Attr, faDirectory))
    {
      if (sr.Name != L"." && sr.Name != L"..")
        retval = DeleteDirectory(DirName + L"\\" + sr.Name);
    }
    else
    {
      retval = DeleteFile(ApiPath(DirName + L"\\" + sr.Name));
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
          retval = DeleteFile(ApiPath(DirName + L"\\" + sr.Name));
        }

        if (!retval) break;
      }
    }
  }
  FindClose(sr);
  if (retval) retval = RemoveDir(ApiPath(DirName)); // VCL function
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
  TCustomImageList * ImageList, TColor Color, int MaskIndex)
{

  // This overly complex drawing is here to support color button on SiteAdvanced
  // dialog. There we use plain TImageList, instead of TPngImageList,
  // TButton does not work with transparent images
  // (not even TBitmap with Transparent = true)
  std::unique_ptr<TBitmap> MaskBitmap(new TBitmap());
  ImageList->GetBitmap(MaskIndex, MaskBitmap.get());

  std::unique_ptr<TPngImage> MaskImage(new TPngImage());
  MaskImage->Assign(MaskBitmap.get());

  std::unique_ptr<TPngImage> ColorImage(new TPngImage(COLOR_RGB, 16, ImageList->Width, ImageList->Height));

  TColor MaskTransparentColor = MaskImage->Pixels[0][0];
  TColor TransparentColor = MaskTransparentColor;
  // Expecting that the color to be replaced is in the centre of the image (HACK)
  TColor MaskColor = MaskImage->Pixels[ImageList->Width / 2][ImageList->Height / 2];

  for (int Y = 0; Y < ImageList->Height; Y++)
  {
    for (int X = 0; X < ImageList->Width; X++)
    {
      TColor SourceColor = MaskImage->Pixels[X][Y];
      TColor DestColor;
      // this branch is pointless as long as MaskTransparentColor and
      // TransparentColor are the same
      if (SourceColor == MaskTransparentColor)
      {
        DestColor = TransparentColor;
      }
      else if (SourceColor == MaskColor)
      {
        DestColor = Color;
      }
      else
      {
        DestColor = SourceColor;
      }
      ColorImage->Pixels[X][Y] = DestColor;
    }
  }

  std::unique_ptr<TBitmap> Bitmap(new TBitmap());
  Bitmap->SetSize(ImageList->Width, ImageList->Height);
  ColorImage->AssignTo(Bitmap.get());

  ImageList->AddMasked(Bitmap.get(), TransparentColor);
}
//---------------------------------------------------------------------------
void __fastcall SetSubmenu(TTBXCustomItem * Item)
{
  class TTBXPublicItem : public TTBXCustomItem
  {
  public:
    __property ItemStyle;
  };
  TTBXPublicItem * PublicItem = reinterpret_cast<TTBXPublicItem *>(Item);
  assert(PublicItem != NULL);
  // See TTBItemViewer.IsPtInButtonPart (called from TTBItemViewer.MouseDown)
  PublicItem->ItemStyle = PublicItem->ItemStyle << tbisSubmenu;
}
//---------------------------------------------------------------------------
bool __fastcall IsEligibleForApplyingTabs(
  UnicodeString Line, int & TabPos, UnicodeString & Start, UnicodeString & Remaining)
{
  bool Result = false;
  TabPos = Line.Pos(L"\t");
  if (TabPos > 0)
  {
    Remaining = Line.SubString(TabPos + 1, Line.Length() - TabPos);
    // WORKAROUND
    // Some translations still use obsolete hack of consecutive tabs to aling the contents.
    // Delete these, so that the following check does not fail on this
    while (Remaining.SubString(1, 1) == L"\t")
    {
      Remaining.Delete(1, 1);
    }

    // We do not have, not support, mutiple tabs on a single line
    if (ALWAYS_TRUE(Remaining.Pos(L"\t") == 0))
    {
      Start = Line.SubString(1, TabPos - 1);
      // WORKAROUND
      // Previously we padded the string before tab with spaces,
      // to aling the contents across multiple lines
      Start = Start.TrimRight();
      // at least two normal spaces for separation
      Start += L"  ";
      Result = true;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
static int __fastcall CalculateWidthByLength(UnicodeString Text, void * /*Arg*/)
{
  return Text.Length();
}
//---------------------------------------------------------------------------
void __fastcall ApplyTabs(
  UnicodeString & Text, wchar_t Padding,
  TCalculateWidth CalculateWidth, void * CalculateWidthArg)
{
  if (CalculateWidth == NULL)
  {
    assert(CalculateWidthArg == NULL);
    CalculateWidth = CalculateWidthByLength;
  }

  std::unique_ptr<TStringList> Lines(TextToStringList(Text));

  int MaxWidth = -1;
  for (int Index = 0; Index < Lines->Count; Index++)
  {
    UnicodeString Line = Lines->Strings[Index];
    int TabPos;
    UnicodeString Start;
    UnicodeString Remaining;
    if (IsEligibleForApplyingTabs(Line, TabPos, Start, Remaining))
    {
      int Width = CalculateWidth(Start, CalculateWidthArg);
      MaxWidth = Max(MaxWidth, Width);
    }
  }

  // Optimization and also to prevent potential regression for texts without tabs
  if (MaxWidth >= 0)
  {
    for (int Index = 0; Index < Lines->Count; Index++)
    {
      UnicodeString Line = Lines->Strings[Index];
      int TabPos;
      UnicodeString Start;
      UnicodeString Remaining;
      if (IsEligibleForApplyingTabs(Line, TabPos, Start, Remaining))
      {
        int Width;
        while ((Width = CalculateWidth(Start, CalculateWidthArg)) < MaxWidth)
        {
          int Wider = CalculateWidth(Start + Padding, CalculateWidthArg);
          // If padded string is wider than max width by more pixels
          // than non-padded string is shorter than max width
          if ((Wider > MaxWidth) && ((Wider - MaxWidth) > (MaxWidth - Width)))
          {
            break;
          }
          Start += Padding;
        }
        Lines->Strings[Index] = Start + Remaining;
      }
    }

    Text = Lines->Text;
    // remove trailing newline
    Text = Text.TrimRight();
  }
}
//---------------------------------------------------------------------------
class TBrowserViewer : public TWebBrowserEx
{
public:
  __fastcall virtual TBrowserViewer(TComponent* AOwner);

  void __fastcall AddLinkHandler(
    const UnicodeString & Url, TNotifyEvent Handler);

  TControl * LoadingPanel;

protected:
  DYNAMIC void __fastcall DoContextPopup(const TPoint & MousePos, bool & Handled);
  void __fastcall DocumentComplete(
    TObject * Sender, const _di_IDispatch Disp, const OleVariant & URL);
  void __fastcall BeforeNavigate2(
    TObject * Sender, const _di_IDispatch Disp, const OleVariant & URL,
    const OleVariant & Flags, const OleVariant & TargetFrameName,
    const OleVariant & PostData, const OleVariant & Headers, WordBool & Cancel);

  bool FComplete;
  std::map<UnicodeString, TNotifyEvent> FHandlers;
};
//---------------------------------------------------------------------------
__fastcall TBrowserViewer::TBrowserViewer(TComponent* AOwner) :
  TWebBrowserEx(AOwner)
{
  FComplete = false;

  OnDocumentComplete = DocumentComplete;
  OnBeforeNavigate2 = BeforeNavigate2;
  LoadingPanel = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TBrowserViewer::AddLinkHandler(
  const UnicodeString & Url, TNotifyEvent Handler)
{
  FHandlers.insert(std::make_pair(Url, Handler));
}
//---------------------------------------------------------------------------
void __fastcall TBrowserViewer::DoContextPopup(const TPoint & MousePos, bool & Handled)
{
  // suppress built-in context menu
  Handled = true;
  TWebBrowserEx::DoContextPopup(MousePos, Handled);
}
//---------------------------------------------------------------------------
void __fastcall TBrowserViewer::DocumentComplete(
    TObject * /*Sender*/, const _di_IDispatch /*Disp*/, const OleVariant & /*URL*/)
{
  SetBrowserDesignModeOff(this);

  FComplete = true;

  if (LoadingPanel != NULL)
  {
    LoadingPanel->Visible = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TBrowserViewer::BeforeNavigate2(
  TObject * /*Sender*/, const _di_IDispatch /*Disp*/, const OleVariant & AURL,
  const OleVariant & /*Flags*/, const OleVariant & /*TargetFrameName*/,
  const OleVariant & /*PostData*/, const OleVariant & /*Headers*/, WordBool & Cancel)
{
  // If OnDocumentComplete was not called yet, is has to be our initial message URL,
  // opened using TWebBrowserEx::Navigate(), allow it.
  // Otherwise it's user navigating, block that and open link
  // in an external browser, possibly adding campaign parameters on the way.
  if (FComplete)
  {
    Cancel = 1;

    UnicodeString URL = AURL;
    if (FHandlers.count(URL) > 0)
    {
      FHandlers[URL](this);
    }
    else
    {
      OpenBrowser(URL);
    }
  }
}
//---------------------------------------------------------------------------
TWebBrowserEx * __fastcall CreateBrowserViewer(TPanel * Parent, const UnicodeString & LoadingLabel)
{
  TBrowserViewer * Result = new TBrowserViewer(Parent);
  // TWebBrowserEx has its own unrelated Name and Parent properties
  static_cast<TWinControl *>(Result)->Name = L"BrowserViewer";
  static_cast<TWinControl *>(Result)->Parent = Parent;
  Result->Align = alClient;
  Result->ControlBorder = cbNone;

  TPanel * LoadingPanel = new TPanel(Parent);
  LoadingPanel->Parent = Parent;
  LoadingPanel->BevelOuter = bvNone;
  LoadingPanel->BevelInner = bvNone; // default
  LoadingPanel->Align = alClient;
  LoadingPanel->Caption = LoadingLabel;
  Result->LoadingPanel = LoadingPanel;

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall SetBrowserDesignModeOff(TWebBrowserEx * WebBrowser)
{
  if (ALWAYS_TRUE(WebBrowser->Document2 != NULL))
  {
    WebBrowser->Document2->designMode = L"Off";
  }
}
//---------------------------------------------------------------------------
void __fastcall AddBrowserLinkHandler(TWebBrowserEx * WebBrowser,
  const UnicodeString & Url, TNotifyEvent Handler)
{
  TBrowserViewer * BrowserViewer = dynamic_cast<TBrowserViewer *>(WebBrowser);
  if (ALWAYS_TRUE(BrowserViewer != NULL))
  {
    BrowserViewer->AddLinkHandler(Url, Handler);
  }
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
  if ((Index < Command.Length()) && (Command[Index + 1] == L'^'))
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
