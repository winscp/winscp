//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include "Editor.h"
#include "TextsWin.h"
#include "Tools.h"
#include <CoreMain.h>
#include "VCLCommon.h"
#include "WinConfiguration.h"
#include "HelpWin.h"
#include <BaseUtils.hpp>
#include <GUITools.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TB2Dock"
#pragma link "TBX"
#pragma link "TB2Item"
#pragma link "TB2Toolbar"
#pragma link "TBXStatusBars"
#pragma link "PngImageList"
#pragma link "TBXExtItems"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
TForm * __fastcall ShowEditorForm(const UnicodeString FileName, TCustomForm * ParentForm,
  TNotifyEvent OnFileChanged, TNotifyEvent OnFileReload, TFileClosedEvent OnClose,
  const UnicodeString Caption, bool StandaloneEditor, TColor Color)
{
  TEditorForm * Dialog = new TEditorForm(Application);
  try
  {
    Dialog->FileName = FileName;
    Dialog->ParentForm = ParentForm;
    UnicodeString ACaption = Caption.IsEmpty() ? FileName : Caption;
    Dialog->Caption = ACaption + L" - " + LoadStr(EDITOR_CAPTION) + L" - " + AppName;
    Dialog->OnFileChanged = OnFileChanged;
    Dialog->OnFileReload = OnFileReload;
    Dialog->StandaloneEditor = StandaloneEditor;
    Dialog->BackgroundColor = Color;
    // load before showing, so when loading failes,
    // we do not show an empty editor
    Dialog->LoadFile();
    Dialog->Show();
    // make sure editor closing is announced only if it was ever successfully shown
    // (at least current implementation of the events cannot handle that)
    Dialog->OnWindowClose = OnClose;
  }
  catch(...)
  {
    delete Dialog;
    throw;
  }

  return Dialog;
}
//---------------------------------------------------------------------------
void __fastcall ReconfigureEditorForm(TForm * Form)
{
  TEditorForm * Editor = dynamic_cast<TEditorForm *>(Form);
  assert(Editor != NULL);
  Editor->ApplyConfiguration();
}
//---------------------------------------------------------------------------
void __fastcall EditorFormFileUploadComplete(TForm * Form)
{
  NOT_NULL(dynamic_cast<TEditorForm *>(Form))->FileUploadComplete();
}
//---------------------------------------------------------------------------
class TPreambleFilteringFileStream : public TFileStream
{
public:
  __fastcall TPreambleFilteringFileStream(const UnicodeString AFileName, System::Word Mode,
    TEncoding * Encoding, bool AllowPreamble);
  virtual int __fastcall Write(const void * Buffer, int Count);
  virtual int __fastcall Write(const System::DynamicArray<System::Byte> Buffer, int Offset, int Count);
private:
  TBytes FPreamble;
  bool FDisallowPreamble;
};
//---------------------------------------------------------------------------
__fastcall TPreambleFilteringFileStream::TPreambleFilteringFileStream(
    const UnicodeString AFileName, System::Word Mode,
    TEncoding * Encoding, bool AllowPreamble) :
  TFileStream(AFileName, Mode)
{
  FDisallowPreamble = (Encoding != NULL) && !AllowPreamble;
  if (FDisallowPreamble)
  {
    FPreamble = Encoding->GetPreamble();
  }
}
//---------------------------------------------------------------------------
int __fastcall TPreambleFilteringFileStream::Write(const void * Buffer, int Count)
{
  bool IsDisallowedPreamble = false;
  if (FDisallowPreamble && (Count > 0) && (FPreamble.Length == Count))
  {
    int Index = 0;
    IsDisallowedPreamble = true;
    const unsigned char * ByteBuffer = reinterpret_cast<const unsigned char *>(Buffer);
    while (IsDisallowedPreamble && (Index < Count))
    {
      IsDisallowedPreamble = (ByteBuffer[Index] == FPreamble[Index]);
      Index++;
    }
  }

  // only on the first write
  FDisallowPreamble = false;

  int Result;
  if (IsDisallowedPreamble)
  {
    Result = Count;
  }
  else
  {
    Result = TFileStream::Write(Buffer, Count);
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TPreambleFilteringFileStream::Write(
  const System::DynamicArray<System::Byte> /*Buffer*/, int /*Offset*/, int /*Count*/)
{
  FAIL;
  EXCEPTION;
}
//---------------------------------------------------------------------------
class TRichEdit20 : public TRichEdit
{
public:
  virtual __fastcall TRichEdit20(TComponent * AOwner);

  bool __fastcall LoadFromStream(TStream * Stream, TEncoding * Encoding, bool & EncodingError);

  void __fastcall SetFormat(UnicodeString FontName, int FontSize,
    TFontCharset FontCharset, TFontStyles FontStyles, unsigned int TabSize,
    bool AWordWrap);
  void __fastcall ResetFormat();
  int __fastcall FindText(const UnicodeString SearchStr, int StartPos, int Length,
    TSearchTypes Options, bool Down);
  void __fastcall Redo();

  __property bool SupportsUpSearch = { read = FVersion20 };
  __property bool CanRedo = { read = GetCanRedo };
  __property bool LoadedWithPreamble = { read = FLoadedWithPreamble };

protected:
  friend unsigned long __stdcall StreamLoad(DWORD_PTR Cookie, unsigned char * Buff, long Read, long * WasRead);

  virtual void __fastcall CreateParams(TCreateParams & Params);
  virtual void __fastcall DestroyWnd();
  void __fastcall Dispatch(void * Message);
  bool __fastcall GetCanRedo();
  void __fastcall SetTabSize(unsigned int TabSize);
  void __fastcall WMPaste();
  void __fastcall EMStreamIn(TMessage & Message);
  bool __stdcall StreamLoad(TRichEditStreamInfo * StreamInfo,
    unsigned char * Buff, long Read, long & WasRead);

private:
  HINSTANCE FLibrary;
  bool FVersion20;
  bool FWordWrap;
  unsigned int FTabSize;
  bool FInitialized;
  bool FStreamLoadEncodingError;
  bool FStreamLoadError;
  bool FLoadedWithPreamble;
};
//---------------------------------------------------------------------------
__fastcall TRichEdit20::TRichEdit20(TComponent * AOwner) :
  TRichEdit(AOwner),
  FLibrary(0),
  FVersion20(false),
  FTabSize(0),
  FWordWrap(true),
  FInitialized(false),
  FLoadedWithPreamble(false)
{
}
//---------------------------------------------------------------------------
void __fastcall TRichEdit20::SetFormat(UnicodeString FontName, int FontSize,
  TFontCharset FontCharset, TFontStyles FontStyles, unsigned int TabSize,
  bool AWordWrap)
{

  if (!FInitialized)
  {
    // for efficiency we should be creating handle here
    assert(!HandleAllocated());
  }

  // setting DefAttributes is noop if we do not have a handle
  // (btw code below would create one anyway)
  HandleNeeded();

  LockWindowUpdate(Handle);

  // setting DefAttributes may take quite time, even if the font attributes
  // do not change, so avoid that if not necessary
  if (!FInitialized ||
      (Font->Name != FontName) ||
      (Font->Size != FontSize) ||
      (Font->Charset != FontCharset) ||
      (Font->Style != FontStyles))
  {
    Font->Name = FontName;
    Font->Size = FontSize;
    Font->Charset = FontCharset;
    Font->Style = FontStyles;
    DefAttributes->Assign(Font);
  }

  if (!FInitialized ||
      (FTabSize != TabSize))
  {
    SetTabSize(TabSize);
    FTabSize = TabSize;
  }

  if (!FInitialized ||
      (FWordWrap != AWordWrap))
  {
    assert(HandleAllocated());
    // Undocumented usage of EM_SETTARGETDEVICE.
    // But note that it is used by MFC in CRichEditView::WrapChanged()
    SendMessage(Handle, EM_SETTARGETDEVICE, 0, (AWordWrap ? 0 : 1));
    FWordWrap = AWordWrap;
  }

  LockWindowUpdate(NULL);

  FInitialized = true;

}
//---------------------------------------------------------------------------
void __fastcall TRichEdit20::ResetFormat()
{
  // tabs are paragraph attributes, which default values cannot be set,
  // so we need to reapply them after loading file
  SetTabSize(FTabSize);
}
//---------------------------------------------------------------------------
int __fastcall TRichEdit20::FindText(const UnicodeString SearchStr, int StartPos,
  int /*Length*/, TSearchTypes Options, bool Down)
{
  int Result;
  if (FVersion20)
  {
    ::FINDTEXTEX Find;
    memset(&Find, 0, sizeof(Find));
    Find.chrg.cpMin = StartPos;
    Find.chrg.cpMax = -1;
    Find.lpstrText = UnicodeString(SearchStr).c_str();

    unsigned int Flags =
      FLAGMASK(Options.Contains(stWholeWord), FR_WHOLEWORD) |
      FLAGMASK(Options.Contains(stMatchCase), FR_MATCHCASE) |
      FLAGMASK(Down, FR_DOWN);
    Result = SendMessage(Handle, EM_FINDTEXTEX, Flags, (LPARAM)&Find);
  }
  else
  {
    assert(Down);
    Result = TRichEdit::FindText(SearchStr, StartPos, Text.Length(), Options);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TRichEdit20::Redo()
{
  assert(FVersion20);
  SendMessage(Handle, EM_REDO, 0, 0);
}
//---------------------------------------------------------------------------
void __fastcall TRichEdit20::CreateParams(TCreateParams & Params)
{
  const wchar_t RichEditModuleName[] = L"RICHED20.DLL";
  long int OldError;

  OldError = SetErrorMode(SEM_NOOPENFILEERRORBOX);
  FLibrary = LoadLibrary(RichEditModuleName);
  SetErrorMode(OldError);

  FVersion20 = (FLibrary != 0);
  if (!FVersion20)
  {
    // fallback to richedit 1.0
    TRichEdit::CreateParams(Params);
  }
  else
  {
    TCustomMemo::CreateParams(Params);
    CreateSubClass(Params, RICHEDIT_CLASS);
    Params.Style = Params.Style |
      (HideScrollBars ? 0 : ES_DISABLENOSCROLL) |
      (HideSelection ? 0 : ES_NOHIDESEL);
    Params.WindowClass.style = Params.WindowClass.style &
      ~(CS_HREDRAW | CS_VREDRAW);
  }
}
//---------------------------------------------------------------------------
void __fastcall TRichEdit20::DestroyWnd()
{
  TRichEdit::DestroyWnd();

  if (FLibrary != 0)
  {
    FreeLibrary(FLibrary);
  }
}
//---------------------------------------------------------------------------
void __fastcall TRichEdit20::WMPaste()
{
  // override default pasting to prevent inserting formatted text (RTF).
  const wchar_t * Text = NULL;
  HANDLE Handle = OpenTextFromClipboard(Text);
  if (Handle != NULL)
  {
    try
    {
      // replacement for EM_PASTESPECIAL,
      // which ignores trailing line end for some reason
      Perform(EM_REPLACESEL, true, reinterpret_cast<int>(Text));
    }
    __finally
    {
      CloseTextFromClipboard(Handle);
    }
  }
}
//---------------------------------------------------------------------------
// Copy from Vcl.ComCtrls.pas
static int __fastcall AdjustLineBreaks(unsigned char * Dest, const TBytes & Source, int Start, int Len)
{
  unsigned char * P = Dest;
  int I = Start; // Position in Source

  while (I < (Len - 1))
  {
    if ((Source[I] == 10) && (Source[I + 1] == 0))
    {
      // Convert #10 to #13#10
      *P = 13;
      P++;
      *P = 0;
      P++;
      *P = 10;
      P++;
      *P = 0;
      P++;
    }
    else
    {
      *P = Source[I];
      P++;
      *P = Source[I + 1];
      P++;
      if ((Source[I] == 13) && (Source[I + 1] == 0))
      {
        // Convert #13 to #13#10
        *P = 10;
        P++;
        *P = 0;
        P++;
        // Skip #10 if preceeded by #13
        if ((I < (Len - 3)) &&
            (Source[I + 2] == 10) && (Source[I + 3] == 0))
        {
          I += 2;
        }
      }
    }

    I += 2;
  }
  if (I == Len - 1)
  {
    *P = Source[I];
    P++;
  }
  return (P - Dest);
}
//---------------------------------------------------------------------------
struct TStreamLoadInfo
{
  TRichEditStreamInfo * StreamInfo;
  TRichEdit20 * RichEdit;
};
//---------------------------------------------------------------------------
// Copy from Vcl.ComCtrls.pas,
// WORKAROUND for bug in BCB XE2-XE6 VCL
// Fixes conversion from UTF-8, when read buffer ends in the middle of UTF-8 char
static unsigned long __stdcall StreamLoad(DWORD_PTR Cookie, unsigned char * Buff, long Read, long * WasRead)
{
  TStreamLoadInfo * LoadInfo = reinterpret_cast<TStreamLoadInfo *>(Cookie);
  unsigned long Result =
    LoadInfo->RichEdit->StreamLoad(LoadInfo->StreamInfo, Buff, Read, *WasRead) ? 0 : 1;
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TRichEdit20::EMStreamIn(TMessage & Message)
{
  TEditStream * EditStream = reinterpret_cast<TEditStream *>(Message.LParam);
  EditStream->pfnCallback = &::StreamLoad;
  TStreamLoadInfo LoadInfo;
  LoadInfo.StreamInfo = reinterpret_cast<TRichEditStreamInfo *>(EditStream->dwCookie);
  LoadInfo.RichEdit = this;
  EditStream->dwCookie = reinterpret_cast<DWORD_PTR>(&LoadInfo);
  TRichEdit::Dispatch(&Message);
}
//---------------------------------------------------------------------------
void __fastcall TRichEdit20::Dispatch(void * Message)
{
  TMessage * M = static_cast<TMessage *>(Message);
  switch (M->Msg)
  {
    case WM_PASTE:
      WMPaste();
      break;

    case EM_STREAMIN:
      EMStreamIn(*M);
      break;

    default:
      TRichEdit::Dispatch(Message);
      break;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TRichEdit20::GetCanRedo()
{
  return FVersion20 && (SendMessage(Handle, EM_CANREDO, 0, 0) != 0);
}
//---------------------------------------------------------------------------
void __fastcall TRichEdit20::SetTabSize(unsigned int TabSize)
{
  assert(TabSize > 0);

  HDC DC = GetDC(Handle);
  SaveDC(DC);
  SetMapMode(DC, MM_TEXT);
  SelectObject(DC, Font->Handle);

  int LogPixelsX = GetDeviceCaps(DC, LOGPIXELSX);

  SIZE Size;
  GetTextExtentPoint(DC, UnicodeString::StringOfChar(L'X', TabSize).c_str(),
    TabSize, &Size);

  RestoreDC(DC, -1);
  ReleaseDC(Handle, DC);

  unsigned int TabTwips = MulDiv(Size.cx, 1440, LogPixelsX);

  // save selection
  CHARRANGE CharRange;
  SendMessage(Handle, EM_EXGETSEL, 0, (LPARAM)&CharRange);

  CHARRANGE CharRangeAll;
  CharRangeAll.cpMin = 0;
  CharRangeAll.cpMax = -1;
  SendMessage(Handle, EM_EXSETSEL, 0, (LPARAM)&CharRangeAll);

  PARAFORMAT2 ParaFormat;
  ParaFormat.cbSize = sizeof(ParaFormat);
  ParaFormat.dwMask = PFM_TABSTOPS;
  ParaFormat.cTabCount = MAX_TAB_STOPS;

  for (int i = 0; i < ParaFormat.cTabCount; i++)
  {
    ParaFormat.rgxTabs[i] = (i + 1) * TabTwips;
  }

  SendMessage(Handle, EM_SETPARAFORMAT, 0, (LPARAM)&ParaFormat);

  // restore selection
  SendMessage(Handle, EM_EXSETSEL, 0, (LPARAM)&CharRange);
}
//---------------------------------------------------------------------------
bool __stdcall TRichEdit20::StreamLoad(
  TRichEditStreamInfo * StreamInfo, unsigned char * Buff, long Read, long & WasRead)
{
  WasRead = 0;

  bool Result;
  try
  {
    if (StreamInfo->Converter != NULL)
    {
      TBytes Buffer;
      Buffer.Length = Read + 1;
      Read = Read / 2;
      if ((Read % 2) > 0)
      {
        Read--;
      }

      WasRead = StreamInfo->Converter->ConvertReadStream(StreamInfo->Stream, Buffer, Read);

      if (WasRead > 0)
      {
        Buffer[WasRead] = 0;
        if (Buffer[WasRead - 1] == 13)
        {
          Buffer[WasRead - 1] = 0;
          WasRead--;
        }

        int StartIndex = 0;
        // Convert from desired Encoding to Unicode
        if (StreamInfo->PlainText)
        {
          if (StreamInfo->Encoding == NULL)
          {
            Buffer = TEncoding::Convert(TEncoding::Default, TEncoding::Unicode, Buffer, 0, WasRead);
            WasRead = Buffer.Length;
          }
          else
          {
            if (!TEncoding::Unicode->Equals(StreamInfo->Encoding))
            {
              int MaxTries = StreamInfo->Encoding->GetMaxByteCount(1);
              while ((WasRead > 0) &&
                     (StreamInfo->Encoding->GetCharCount(Buffer, 0, WasRead)  == 0))
              {
                WasRead--;
                StreamInfo->Stream->Seek(-1, soFromCurrent);
                MaxTries--;
                if ((MaxTries == 0) || (WasRead == 0))
                {
                  FStreamLoadEncodingError = true;
                  Abort();
                }
              }
              Buffer = TEncoding::Convert(StreamInfo->Encoding, TEncoding::Unicode, Buffer, 0, WasRead);
              WasRead = Buffer.Length;
            }
            // If Unicode preamble is present, set StartIndex to skip over it
            TBytes Preamble = TEncoding::Unicode->GetPreamble();
            if (ALWAYS_TRUE(Preamble.Length == 2) &&
                (WasRead >= 2) && (Buffer[0] == Preamble[0]) && (Buffer[1] == Preamble[1]))
            {
              StartIndex = 2;
              // beware that this is also called from CreateWnd with some
              // dummy contents that always have BOM, so FLoadedWithPreamble
              // is true, unless overriden by LoadFromStream
              FLoadedWithPreamble = true;
            }
          }
        }

        WasRead = AdjustLineBreaks(Buff, Buffer, StartIndex, WasRead);
      }
    }

    Result = true;
  }
  catch (EEncodingError & E)
  {
    FStreamLoadError = true;
    FStreamLoadEncodingError = true;
    Result = false;
  }
  catch (Exception & E)
  {
    FStreamLoadError = true;
    Result = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TRichEdit20::LoadFromStream(TStream * Stream, TEncoding * Encoding, bool & EncodingError)
{
  FStreamLoadEncodingError = false;
  FStreamLoadError = false;
  FLoadedWithPreamble = false;
  Lines->LoadFromStream(Stream, Encoding);
  if (FStreamLoadError)
  {
    EncodingError = FStreamLoadEncodingError;
  }
  return !FStreamLoadError;
}
//---------------------------------------------------------------------------
class TFindDialogEx : public TFindDialog
{
public:
  __fastcall virtual TFindDialogEx(TComponent * AOwner) : TFindDialog(AOwner)
  {
    FHelpMsg = RegisterWindowMessage(HELPMSGSTRING);
  }

protected:
  unsigned int FHelpMsg;

  virtual bool __fastcall MessageHook(TMessage & Msg)
  {
    bool Result = false;
    if (Msg.Msg == FHelpMsg)
    {
      Application->HelpKeyword(HELP_EDITOR_FIND);
      Result = true;
    }

    if (!Result)
    {
      Result = TFindDialog::MessageHook(Msg);
    }

    return Result;
  }
};
//---------------------------------------------------------------------------
class TReplaceDialogEx : public TReplaceDialog
{
public:
  __fastcall virtual TReplaceDialogEx(TComponent * AOwner) : TReplaceDialog(AOwner)
  {
    FHelpMsg = RegisterWindowMessage(HELPMSGSTRING);
  }

protected:
  unsigned int FHelpMsg;

  virtual bool __fastcall MessageHook(TMessage & Msg)
  {
    bool Result = false;
    if (Msg.Msg == FHelpMsg)
    {
      Application->HelpKeyword(HELP_EDITOR_REPLACE);
      Result = true;
    }

    if (!Result)
    {
      Result = TReplaceDialog::MessageHook(Msg);
    }

    return Result;
  }
};
//---------------------------------------------------------------------------
unsigned int TEditorForm::FInstances = 0;
//---------------------------------------------------------------------------
__fastcall TEditorForm::TEditorForm(TComponent* Owner)
  : TForm(Owner)
{
  EditorMemo = new TRichEdit20(this);
  EditorMemo->Parent = this;
  EditorMemo->Align = alClient;
  EditorMemo->HideSelection = false;
  EditorMemo->PlainText = true;
  EditorMemo->PopupMenu = EditorPopup;
  EditorMemo->ScrollBars = ssBoth;
  EditorMemo->WantTabs = true;
  EditorMemo->OnChange = EditorMemoChange;
  EditorMemo->OnKeyUp = EditorMemoKeyUp;
  EditorMemo->OnMouseUp = EditorMemoMouseUp;

  FParentForm = NULL;
  FCaretPos = TPoint(-1, -1);
  FLastFindDialog = NULL;
  FCloseAnnounced = false;
  FShowStatusBarHint = false;
  ApplyConfiguration();
  FFindDialog = new TFindDialogEx(this);
  FFindDialog->OnFind = FindDialogFind;
  FReplaceDialog = new TReplaceDialogEx(this);
  FReplaceDialog->OnFind = FindDialogFind;
  FReplaceDialog->OnReplace = FindDialogFind;
  FEncoding = NULL;
  FSaving = false;
  FStandaloneEditor = false;
  FClosePending = false;
  SetSubmenu(ColorItem);

  InitCodePage();

  UseSystemSettings(this);
  UseDesktopFont(StatusBar);
  SetFormIcons(this, L"Z_ICON_EDITOR_BIG", L"Z_ICON_EDITOR_SMALL");
}
//---------------------------------------------------------------------------
__fastcall TEditorForm::~TEditorForm()
{
  assert(FInstances > 0);
  FInstances--;
  if (FInstance == 0)
  {
    UnicodeString WindowParams = StoreForm(this);
    // this is particularly to prevent saving the form state
    // for the first time, keeping default positioning by a system
    if (!FWindowParams.IsEmpty() && (FWindowParams != WindowParams))
    {
      TEditorConfiguration EditorConfiguration = WinConfiguration->Editor;
      EditorConfiguration.WindowParams = StoreForm(this);
      WinConfiguration->Editor = EditorConfiguration;
    }
  }

  // see FormClose for explanation
  if (!FCloseAnnounced)
  {
    BackupSave();
    DoWindowClose(true);
  }

  if (Application->OnHint == ApplicationHint)
  {
    Application->OnHint = NULL;
  }

  if (FStandaloneEditor)
  {
    Application->Terminate();
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TEditorForm::GetCodePageName(TEncoding * Encoding)
{
  if (Encoding == TEncoding::UTF8)
  {
    return LoadStr(UTF8_NAME);
  }
  else
  {
    return DefaultEncodingName();
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::InitCodePage()
{
  DefaultEncodingAction->Caption = DefaultEncodingName();
  DefaultEncodingAction->Hint = FORMAT(DefaultEncodingAction->Hint, (DefaultEncodingName()));
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::SetFileName(const UnicodeString value)
{
  if (value != FFileName)
  {
    FFileName = value;
    if (Visible)
    {
      LoadFile();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::SetParentForm(TCustomForm * value)
{
  if (FParentForm != value)
  {
    FParentForm = value;
    if (value)
    {
      Width = value->BoundsRect.Width();
      Height = value->BoundsRect.Height();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::EditorActionsUpdate(TBasicAction *Action,
      bool &Handled)
{
  Handled = true;
  if (Action == SaveAction)
  {
    SaveAction->Enabled = EditorMemo->Modified;
  }
  else if (Action == FindNextAction)
  {
    FindNextAction->Enabled =
      FLastFindDialog != NULL || !FFindDialog->FindText.IsEmpty();
  }
  else if (Action == EditRedo)
  {
    EditRedo->Enabled = EditorMemo->CanRedo;
  }
  else if (Action == PreferencesAction ||
    Action == FindAction || Action == ReplaceAction || Action == GoToLineAction ||
    Action == HelpAction || Action == ReloadAction || Action == ColorAction)
  {
    ((TAction *)Action)->Enabled = true;
  }
  else if (Action == DefaultEncodingAction)
  {
    DefaultEncodingAction->Enabled = true;
    DefaultEncodingAction->Checked = (FEncoding == TEncoding::Default);
  }
  else if (Action == UTF8EncodingAction)
  {
    UTF8EncodingAction->Enabled = true;
    UTF8EncodingAction->Checked = (FEncoding == TEncoding::UTF8);
  }
  else
  {
    Handled = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::SaveToFile()
{
  std::unique_ptr<TStream> Stream(
    new TPreambleFilteringFileStream(
      ::ApiPath(FFileName), fmCreate, FEncoding, EditorMemo->LoadedWithPreamble));
  EditorMemo->Lines->SaveToStream(Stream.get(), FEncoding);
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::EditorActionsExecute(TBasicAction *Action,
      bool &Handled)
{
  Handled = true;
  if (Action == SaveAction)
  {
    assert(!FFileName.IsEmpty());
    SaveToFile();
    if (FOnFileChanged)
    {
      FOnFileChanged(this);
    }
    FSaving = true;
    EditorMemo->Modified = false;
    UpdateControls();
  }
  else if (Action == PreferencesAction)
  {
    DoPreferencesDialog(pmEditor);
  }
  else if (Action == ReloadAction)
  {
    Reload();
  }
  else if (Action == FindAction || Action == ReplaceAction)
  {
    StartFind(Action == FindAction);
  }
  else if (Action == FindNextAction)
  {
    if (!FLastFindDialog)
    {
      FLastFindDialog = FFindDialog;
    }
    Find();
  }
  else if (Action == GoToLineAction)
  {
    GoToLine();
  }
  else if (Action == EditRedo)
  {
    EditorMemo->Redo();
  }
  else if (Action == HelpAction)
  {
    FormHelp(this);
  }
  else if (Action == DefaultEncodingAction)
  {
    ChangeEncoding(TEncoding::Default);
  }
  else if (Action == UTF8EncodingAction)
  {
    ChangeEncoding(TEncoding::UTF8);
  }
  else if (Action == ColorAction)
  {
    if (ALWAYS_TRUE(Action->ActionComponent != NULL))
    {
      ::CreateSessionColorMenu(Action->ActionComponent, BackgroundColor,
        SetBackgroundColor);
    }
  }
  else
  {
    Handled = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::BackupSave()
{
  if (EditorMemo->Modified)
  {
    int Uniq = 0;
    while (true)
    {
      UnicodeString FileName = FFileName + L".bak" + (Uniq == 0 ? UnicodeString() : IntToStr(Uniq));
      UnicodeString ApiFileName = ::ApiPath(FileName);
      if (!FileExists(ApiFileName))
      {
        EditorMemo->Lines->SaveToFile(ApiFileName, FEncoding);
        break;
      }
      Uniq++;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::ChangeEncoding(TEncoding * Encoding)
{
  if (FEncoding != Encoding)
  {
    if (!EditorMemo->Modified ||
        (MessageDialog(MainInstructions(LoadStr(EDITOR_MODIFIED_ENCODING)), qtConfirmation,
          qaOK | qaCancel) != qaCancel))
    {
      TEncoding * PrevEncoding = FEncoding;
      try
      {
        FEncoding = Encoding;
        LoadFile();
      }
      catch(...)
      {
        // try to restore
        FEncoding = PrevEncoding;
        LoadFile();
        throw;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::FormCloseQuery(TObject * /*Sender*/,
      bool &CanClose)
{
  if (EditorMemo->Modified)
  {
    SetFocus();
    UnicodeString Message = MainInstructions(LoadStr(SAVE_CHANGES));
    unsigned int Answer = MessageDialog(Message, qtConfirmation,
      qaYes | qaNo | qaCancel);
    CanClose = (Answer != qaCancel);
    if (Answer == qaYes)
    {
      SaveAction->Execute();
      if (FStandaloneEditor)
      {
        CanClose = false;
        FClosePending = true;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::ApplyConfiguration()
{
  bool PrevModified = EditorMemo->Modified;
  assert(Configuration);
  EditorMemo->SetFormat(WinConfiguration->Editor.FontName,
    WinConfiguration->Editor.FontSize, (TFontCharset)WinConfiguration->Editor.FontCharset,
    IntToFontStyles(WinConfiguration->Editor.FontStyle), WinConfiguration->Editor.TabSize,
    WinConfiguration->Editor.WordWrap);
  EditorMemo->Modified = PrevModified;
  EditorMemo->ClearUndo();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::FileUploadComplete()
{
  assert(FSaving);
  FSaving = false;
  UpdateControls();
  if (FClosePending && ALWAYS_TRUE(FStandaloneEditor))
  {
    Close();
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::UpdateControls()
{
  if (FShowStatusBarHint)
  {
    StatusBar->SimplePanel = true;
    StatusBar->SimpleText = FStatusBarHint;
    FCaretPos = TPoint(-1, -1);
  }
  else
  {
    TPoint ACaretPos = EditorMemo->CaretPos;

    if (ACaretPos.x != FCaretPos.x || ACaretPos.y != FCaretPos.y)
    {
      FCaretPos = ACaretPos;
      int Count = EditorMemo->Lines->Count;
      StatusBar->Panels->Items[0]->Caption = FMTLOAD(EDITOR_LINE_STATUS,
        ((int)FCaretPos.y+1, Count));
      int Column = 0;
      UnicodeString Character;
      if (FCaretPos.y >= 0 && FCaretPos.y < EditorMemo->Lines->Count)
      {
        UnicodeString Line = EditorMemo->Lines->Strings[FCaretPos.y];
        int TabSize = WinConfiguration->Editor.TabSize;
        for (int Index = 1; Index <= FCaretPos.x + 1; Index++)
        {
          if ((Index - 1 >= 1) && (Index - 1 <= Line.Length()) && (Line[Index - 1] == L'\t') &&
              (TabSize > 0)) // sanity check
          {
            Column = (((Column / TabSize) + 1) * TabSize) + 1;
          }
          else
          {
            Column++;
          }
        }

        if (FCaretPos.x+1 <= Line.Length())
        {
          int Code;
          wchar_t Ch = Line[FCaretPos.x + 1];
          if (FEncoding == TEncoding::Default)
          {
            char Buf[10];
            BOOL UsedDefaultChar = FALSE;
            int Conversion =
              WideCharToMultiByte(CP_ACP, WC_NO_BEST_FIT_CHARS, &Ch, 1,
                Buf, sizeof(Buf), NULL, &UsedDefaultChar);
            // actually with multibyte encoding it may be > 1,
            if ((Conversion == 1) && !UsedDefaultChar)
            {
              Code = static_cast<int>(static_cast<unsigned char>(Buf[0]));
            }
            else
            {
              Code = -1;
            }
          }
          else
          {
            Code = static_cast<int>(Ch);
          }

          if (Code >= 0)
          {
            Character = FMTLOAD(EDITOR_CHARACTER_STATUS2, (Code, Code));
          }
        }
      }
      StatusBar->Panels->Items[1]->Caption =
        (Column > 0) ? FMTLOAD(EDITOR_COLUMN_STATUS, (Column)) : UnicodeString();
      StatusBar->Panels->Items[2]->Caption = Character;
    }
    StatusBar->Panels->Items[3]->Caption = FMTLOAD(EDITOR_ENCODING_STATUS, (FEncodingName));
    StatusBar->Panels->Items[4]->Caption =
      (FSaving ? LoadStr(EDITOR_SAVING) :
        (EditorMemo->Modified ? LoadStr(EDITOR_MODIFIED) : UnicodeString(L"")));
    StatusBar->SimplePanel = false;
  }

  EditorActions->UpdateAction(SaveAction);
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::EditorMemoMouseUp(TObject * /*Sender*/,
      TMouseButton /*Button*/, TShiftState /*Shift*/, int /*X*/, int /*Y*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::EditorMemoKeyUp(TObject * /*Sender*/,
    WORD & /*Key*/, TShiftState /*Shift*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::EditorMemoChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::FindDialogFind(TObject * /*Sender*/)
{
  Find();
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::Find()
{
  int NewPos;
  int Replacements = 0;

  do
  {
    assert(FLastFindDialog);

    TSearchTypes SearchTypes;

    // length condition is there to improve performance when large
    // block is selected in editor
    if (FLastFindDialog == FReplaceDialog &&
        (FReplaceDialog->Options.Contains(frReplace) ||
         FReplaceDialog->Options.Contains(frReplaceAll)) &&
        FReplaceDialog->FindText.Length() == EditorMemo->SelLength &&
        AnsiSameText(FReplaceDialog->FindText, EditorMemo->SelText))
    {
      EditorMemo->SelText = FReplaceDialog->ReplaceText;
      Replacements++;
    }

    TEditorConfiguration EditorConfiguration = WinConfiguration->Editor;
    EditorConfiguration.FindText = FLastFindDialog->FindText;
    EditorConfiguration.ReplaceText = FReplaceDialog->ReplaceText;
    EditorConfiguration.FindMatchCase = FLastFindDialog->Options.Contains(frMatchCase);
    EditorConfiguration.FindWholeWord = FLastFindDialog->Options.Contains(frWholeWord);
    EditorConfiguration.FindDown = FLastFindDialog->Options.Contains(frDown);
    WinConfiguration->Editor = EditorConfiguration;

    if (EditorConfiguration.FindMatchCase)
    {
      SearchTypes << stMatchCase;
    }
    if (EditorConfiguration.FindWholeWord)
    {
      SearchTypes << stWholeWord;
    }

    NewPos = EditorMemo->FindText(EditorConfiguration.FindText,
      EditorMemo->SelLength ? EditorMemo->SelStart+1 : EditorMemo->SelStart,
      EditorMemo->Text.Length(), SearchTypes, EditorConfiguration.FindDown);

    if (NewPos >= 0)
    {
      EditorMemo->SelStart = NewPos;
      EditorMemo->SelLength = EditorConfiguration.FindText.Length();
    }

    if (FLastFindDialog->Handle)
    {
      PositionFindDialog(true);
    }

    if (NewPos < 0)
    {
      if ((Replacements == 0) || FReplaceDialog->Options.Contains(frReplaceAll))
      {
        // now Screen->ActiveForm can be NULL when other form was meanwhile
        // activated and then focus was returned back to "find" dialog
        // (non VCL form)
        if (Screen->ActiveForm != this)
        {
          SetFocus();
          FLastFindDialog->Execute();
        }

        if (Replacements == 0)
        {
          UnicodeString Message = MainInstructions(FMTLOAD(EDITOR_FIND_END, (EditorConfiguration.FindText)));
          MessageDialog(Message, qtInformation, qaOK, HELP_NONE);
        }
        else if (FReplaceDialog->Options.Contains(frReplaceAll))
        {
          UnicodeString Message = MainInstructions(FMTLOAD(EDITOR_REPLACE_END, (Replacements)));
          MessageDialog(Message, qtInformation, qaOK, HELP_NONE);
        }
      }
    }
  }
  while (NewPos >= 0 && FLastFindDialog == FReplaceDialog &&
         FReplaceDialog->Options.Contains(frReplaceAll));
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::FormShow(TObject * /*Sender*/)
{

  CutFormToDesktop(this);

  assert(FWindowParams.IsEmpty());
  if (FWindowParams.IsEmpty())
  {
    FWindowParams = StoreForm(this);
  }
}
//---------------------------------------------------------------------------
// copy from ComCtrls.pas
bool __fastcall TEditorForm::ContainsPreamble(TStream * Stream, const TBytes & Signature)
{
  bool Result;
  TBytes Buffer;
  int LBufLen;
  int LSignatureLen = Signature.Length;
  __int64 LPosition = Stream->Position;
  try
  {
    Buffer.Length = LSignatureLen;
    LBufLen = Stream->Read(&Buffer[0], LSignatureLen);
  }
  __finally
  {
    Stream->Position = LPosition;
  }

  if (LBufLen == LSignatureLen)
  {
    Result = true;
    for (int I = 1; I <= LSignatureLen; I++)
    {
      if (Buffer[I - 1] != Signature[I - 1])
      {
        Result = false;
        break;
      }
    }
  }
  else
  {
    Result = false;
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::LoadFromFile(bool PrimaryEncoding)
{
  TStream * Stream = new TFileStream(::ApiPath(FFileName), fmOpenRead | fmShareDenyWrite);
  try
  {
    bool CanTrySecondary;
    if (FEncoding == NULL)
    {
      int Encoding;
      if (ContainsPreamble(Stream, TEncoding::UTF8->GetPreamble()))
      {
        Encoding = CP_UTF8;
        CanTrySecondary = false;
      }
      else
      {
        Encoding = WinConfiguration->Editor.Encoding;
        CanTrySecondary = true;
      }

      switch (Encoding)
      {
        case CP_UTF8:
          FEncoding = PrimaryEncoding ? TEncoding::UTF8 : TEncoding::Default;
          break;

        default:
          CanTrySecondary = false;
          FAIL;
          // fallthru

        case CP_ACP:
          FEncoding = PrimaryEncoding ? TEncoding::Default : TEncoding::UTF8;
          break;
      }
    }
    else
    {
      CanTrySecondary = false;
    }

    FEncodingName = GetCodePageName(FEncoding);

    bool EncodingError;
    if (!EditorMemo->LoadFromStream(Stream, FEncoding, EncodingError))
    {
      if (EncodingError)
      {
        UnicodeString Message = FMTLOAD(EDITOR_ENCODING_ERROR, (FFileName, FEncodingName));

        if (CanTrySecondary)
        {
          TEncoding * EncodingBackup = FEncoding;
          UnicodeString EncodingNameBackup = FEncodingName;

          try
          {
            FEncoding = NULL;
            LoadFromFile(false);
            TEditorConfiguration EditorConfiguration = WinConfiguration->Editor;
            if (EditorConfiguration.WarnOnEncodingFallback)
            {
              // by now the FEncodingName is the secondary encoding
              Message = Message + L" " + FMTLOAD(EDITOR_ENCODING_REVERTED, (FEncodingName));
              TMessageParams Params(mpNeverAskAgainCheck);
              unsigned int Answer =
                MessageDialog(MainInstructions(Message), qtInformation, qaOK, HELP_NONE, &Params);
              if (Answer == qaNeverAskAgain)
              {
                EditorConfiguration.WarnOnEncodingFallback = false;
                WinConfiguration->Editor = EditorConfiguration;
              }
            }
          }
          catch(...)
          {
            // restored values are never used anyway, as this can get here only
            // when opening editor and this is fatal error preventing the editor from opening
            FEncoding = EncodingBackup;
            FEncodingName = EncodingNameBackup;
            throw Exception(Message);
          }
        }
        else
        {
          throw Exception(Message);
        }
      }
      else
      {
        throw Exception(MainInstructions(FMTLOAD(EDITOR_LOAD_ERROR, (FFileName))));
      }
    }
  }
  __finally
  {
    delete Stream;
  }

  SendMessage(EditorMemo->Handle, EM_EXLIMITTEXT, 0, 0x7FFFFFF0);
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::CheckFileSize()
{
  TEditorConfiguration EditorConfiguration = WinConfiguration->Editor;

  if (EditorConfiguration.WarnOrLargeFileSize)
  {
    TWin32FileAttributeData FileAttributeData;
    if (GetFileAttributesEx(::ApiPath(FFileName).c_str(), GetFileExInfoStandard, &FileAttributeData))
    {
      const __int64 MaxSize = 100 * 1024 * 1024;
      __int64 Size =
        (static_cast<__int64>(FileAttributeData.nFileSizeHigh) << 32) +
        FileAttributeData.nFileSizeLow;
      if (Size > MaxSize)
      {
        TMessageParams Params(mpNeverAskAgainCheck);
        unsigned int Answer =
          MoreMessageDialog(
            FMTLOAD(INTERNAL_EDITOR_LARGE_FILE, (FormatBytes(Size))), NULL,
            qtConfirmation, qaOK | qaCancel, HELP_NONE, &Params);
        switch (Answer)
        {
          case qaOK:
            // noop;
            break;

          case qaCancel:
            Abort();
            break;

          case qaNeverAskAgain:
            EditorConfiguration.WarnOrLargeFileSize = false;
            WinConfiguration->Editor = EditorConfiguration;
            break;

          default:
            FAIL;
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::LoadFile()
{
  CheckFileSize();
  HandleNeeded();
  LoadFromFile(true);
  EditorMemo->ResetFormat();
  EditorMemo->Modified = false;
  FCaretPos.x = -1;
  // this is important particularly after reload
  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TEditorForm::CursorInUpperPart()
{
  HFONT OldFont;
  HDC DC;
  TTextMetric TM;
  TRect Rect;

  DC = GetDC(EditorMemo->Handle);
  OldFont = (HFONT)SelectObject(DC, EditorMemo->Font->Handle);

  try
  {
    GetTextMetrics(DC, &TM);

    EditorMemo->Perform(EM_GETRECT, 0, ((int)&Rect));
  }
  __finally
  {
    SelectObject(DC, OldFont);
    ReleaseDC(EditorMemo->Handle, DC);
  }

  int VisibleLines = (Rect.Bottom - Rect.Top) / (TM.tmHeight + TM.tmExternalLeading);
  int FirstLine = SendMessage(EditorMemo->Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
  TPoint CaretPos = EditorMemo->CaretPos;

  return (CaretPos.y - FirstLine) < VisibleLines / 2;
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::PositionFindDialog(bool VerticalOnly)
{
  assert(FLastFindDialog);
  if (!VerticalOnly)
  {
    FLastFindDialog->Left = Left + EditorMemo->Left + EditorMemo->Width / 2 - ScaleByTextHeight(this, 100);
  }
  FLastFindDialog->Top = Top + EditorMemo->Top + (EditorMemo->Height / 4) +
    (CursorInUpperPart() ? (EditorMemo->Height / 2) : 0) - ScaleByTextHeight(this, 40);
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::StartFind(bool Find)
{
  UnicodeString Text = EditorMemo->SelText;
  TFindOptions Options;
  Options << frShowHelp;
  if (Text.IsEmpty())
  {
    Text = WinConfiguration->Editor.FindText;
  }
  TFindDialog * Dialog = Find ? FFindDialog : FReplaceDialog;
  if (FLastFindDialog && Dialog != FLastFindDialog && FLastFindDialog->Handle)
  {
    FLastFindDialog->CloseDialog();
  }
  FLastFindDialog = Dialog;
  if (!Text.IsEmpty())
  {
    FLastFindDialog->FindText = Text;
  }
  FReplaceDialog->ReplaceText = WinConfiguration->Editor.ReplaceText;
  if (WinConfiguration->Editor.FindMatchCase)
  {
    Options << frMatchCase;
  }
  if (WinConfiguration->Editor.FindWholeWord)
  {
    Options << frWholeWord;
  }
  if (EditorMemo->SupportsUpSearch)
  {
    if (WinConfiguration->Editor.FindDown)
    {
      Options << frDown;
    }
  }
  else
  {
    Options << frHideUpDown; // not implemented
    Options << frDown;
  }
  FLastFindDialog->Options = Options;
  if (!FLastFindDialog->Handle)
  {
    PositionFindDialog(false);
  }
  FLastFindDialog->Execute();
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::GoToLine()
{
  UnicodeString Str;
  if (InputDialog(LoadStr(EDITOR_GO_TO_LINE), LoadStr(EDITOR_LINE_NUMBER), Str))
  {
    int Line = StrToIntDef(Str, -1);
    if (Line <= 0 || Line > EditorMemo->Lines->Count)
    {
      throw Exception(MainInstructions(LoadStr(EDITOR_INVALID_LINE)));
    }
    else
    {
      EditorMemo->CaretPos = TPoint(0, Line-1);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::FormClose(TObject * /*Sender*/,
  TCloseAction & Action)
{
  // Preferably announce closure here as this is called from within TForm::Close(),
  // so the annoucement will be synchronous (and editor manager thus
  // will consider the form to be really closed and will not block
  // application closure).
  // However FormClose is not called when form is closed due to
  // application exit, so there is last resort call from destructor.
  DoWindowClose(false);
  FCloseAnnounced = true;
  Action = caFree;
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::DoWindowClose(bool Forced)
{
  if (FOnWindowClose != NULL)
  {
    try
    {
      FOnWindowClose(this, Forced);
    }
    catch(Exception & E)
    {
      ShowExtendedException(&E);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::CreateParams(TCreateParams & Params)
{
  // this is called for the first time from parent's constructor.
  // FFormRestored is set to false implicitly
  if (!FFormRestored)
  {
    FInstance = FInstances;
    FInstances++;

    FFormRestored = true;
    UnicodeString WindowParams = WinConfiguration->Editor.WindowParams;

    if ((FInstance == 0) && !WindowParams.IsEmpty())
    {
      RestoreForm(WindowParams, this);
    }
  }

  TForm::CreateParams(Params);
  Params.WndParent = GetDesktopWindow();
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::Reload()
{
  if (!EditorMemo->Modified ||
      (MessageDialog(MainInstructions(LoadStr(EDITOR_MODIFIED_RELOAD)), qtConfirmation,
        qaOK | qaCancel) != qaCancel))
  {
    if (FOnFileReload)
    {
      FOnFileReload(this);
    }
    LoadFile();
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::ApplicationHint(TObject * /*Sender*/)
{
  assert(Application);
  UnicodeString AHint = GetLongHint(Application->Hint);
  FShowStatusBarHint = Active && !AHint.IsEmpty();
  if (FShowStatusBarHint)
  {
    FStatusBarHint = AHint;
  }
  else
  {
    FStatusBarHint = L"";
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::FormActivate(TObject * /*Sender*/)
{
  Application->OnHint = ApplicationHint;
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::FormKeyDown(TObject * /*Sender*/, WORD & Key, TShiftState Shift)
{
  if (((Key == VK_ESCAPE) || (Key == VK_F10)) && Shift.Empty())
  {
    Key = 0;
    Close();
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::SetBackgroundColor(TColor Color)
{
  if (BackgroundColor != Color)
  {
    FBackgroundColor = Color;
    TColor ItemColor = (Color != 0 ? Color : Vcl::Graphics::clNone);
    ColorItem->Color = ItemColor;
    TColor EditorColor = (Color != 0 ? Color : clWindow);
    if (EditorMemo->Color != EditorColor)
    {
      EditorMemo->Color = EditorColor;
      // does not seem to have any effect (nor is needed), but just in case
      ForceColorChange(EditorMemo);
    }
  }
}
//---------------------------------------------------------------------------
