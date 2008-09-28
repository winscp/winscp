//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include "Editor.h"
#include "WinInterface.h"
#include "TextsWin.h"
#include "Tools.h"
#include <CoreMain.h>
#include "VCLCommon.h"
#include "WinConfiguration.h"
#include "HelpWin.h"
#include <CommDlg.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TB2Dock"
#pragma link "TBX"
#pragma link "TB2Item"
#pragma link "TB2Toolbar"
#pragma link "TBXStatusBars"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
TForm * __fastcall ShowEditorForm(const AnsiString FileName, TCustomForm * ParentForm,
  TNotifyEvent OnFileChanged, TNotifyEvent OnFileReload, TNotifyEvent OnClose,
  const AnsiString Caption)
{
  TEditorForm * Dialog = new TEditorForm(Application);
  try
  {
    Dialog->FileName = FileName;
    Dialog->ParentForm = ParentForm;
    Dialog->Caption = Caption.IsEmpty() ? FileName : Caption;
    Dialog->OnFileChanged = OnFileChanged;
    Dialog->OnFileReload = OnFileReload;
    Dialog->OnWindowClose = OnClose;
    Dialog->Show();
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
class TRichEdit20 : public TRichEdit
{
public:
  virtual __fastcall TRichEdit20(TComponent * AOwner);

  void __fastcall SetFormat(AnsiString FontName, int FontHeight,
    TFontCharset FontCharset, TFontStyles FontStyles, unsigned int TabSize,
    bool AWordWrap);
  void __fastcall ResetFormat();
  int __fastcall FindText(const AnsiString SearchStr, int StartPos, int Length,
    TSearchTypes Options, bool Down);
  void __fastcall Redo();

  __property bool SupportsUpSearch = { read = FVersion20 };
  __property bool CanRedo = { read = GetCanRedo };

protected:
  virtual void __fastcall CreateParams(TCreateParams & Params);
  virtual void __fastcall DestroyWnd();
  void __fastcall Dispatch(void * Message);
  bool __fastcall GetCanRedo();
  void __fastcall SetTabSize(unsigned int TabSize);
  void __fastcall WMPaste();

private:
  HINSTANCE FLibrary;
  bool FVersion20;
  bool FWordWrap;
  unsigned int FTabSize;
  bool FInitialized;
};
//---------------------------------------------------------------------------
__fastcall TRichEdit20::TRichEdit20(TComponent * AOwner) :
  TRichEdit(AOwner),
  FLibrary(0),
  FVersion20(false),
  FTabSize(0),
  FWordWrap(true),
  FInitialized(false)
{
}
//---------------------------------------------------------------------------
void __fastcall TRichEdit20::SetFormat(AnsiString FontName, int FontHeight,
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
      (Font->Height != FontHeight) ||
      (Font->Charset != FontCharset) ||
      (Font->Style != FontStyles))
  {
    Font->Name = FontName;
    Font->Height = FontHeight;
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
int __fastcall TRichEdit20::FindText(const AnsiString SearchStr, int StartPos,
  int /*Length*/, TSearchTypes Options, bool Down)
{
  int Result;
  if (FVersion20)
  {
    ::FINDTEXTEX Find;
    memset(&Find, 0, sizeof(Find));
    Find.chrg.cpMin = StartPos;
    Find.chrg.cpMax = -1;
    Find.lpstrText = SearchStr.c_str();

    unsigned int Flags =
      FLAGMASK(Options.Contains(stWholeWord), FT_WHOLEWORD) |
      FLAGMASK(Options.Contains(stMatchCase), FT_MATCHCASE) |
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
  const char RichEditModuleName[] = "RICHED20.DLL";
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
  const char * Text = NULL;
  HANDLE Handle = OpenTextFromClipboard(Text);
  if (Handle != NULL)
  {
    try
    {
      // replacement for EM_PASTESPECIAL,
      // which ignores trailing line end for some rea
      SetSelTextBuf(const_cast<char *>(Text));
    }
    __finally
    {
      CloseTextFromClipboard(Handle);
    }
  }
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
  GetTextExtentPoint(DC, AnsiString::StringOfChar('X', TabSize).c_str(),
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
  UseSystemSettings(this);
}
//---------------------------------------------------------------------------
__fastcall TEditorForm::~TEditorForm()
{
  // see FormClose for explanation
  if (!FCloseAnnounced)
  {
    DoWindowClose();
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::SetFileName(const AnsiString value)
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
  else if (Action == PreferencesAction || Action == CloseAction ||
    Action == FindAction || Action == ReplaceAction || Action == GoToLineAction ||
    Action == HelpAction || Action == ReloadAction)
  {
    ((TAction *)Action)->Enabled = true;
  }
  else
  {
    Handled = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::EditorActionsExecute(TBasicAction *Action,
      bool &Handled)
{
  Handled = true;
  if (Action == SaveAction)
  {
    assert(!FFileName.IsEmpty());
    EditorMemo->Lines->SaveToFile(FFileName);
    if (FOnFileChanged)
    {
      FOnFileChanged(this);
    }
    EditorMemo->Modified = false;
    UpdateControls();
  }
  else if (Action == PreferencesAction)
  {
    DoPreferencesDialog(pmEditor);
  }
  else if (Action == CloseAction)
  {
    Close();
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
  else
  {
    Handled = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::FormCloseQuery(TObject * /*Sender*/,
      bool &CanClose)
{
  if (EditorMemo->Modified)
  {
    SetFocus();
    int Answer = MessageDialog(LoadStr(SAVE_CHANGES), qtConfirmation,
      qaYes | qaNo | qaCancel);
    CanClose = (Answer != qaCancel);
    if (Answer == qaYes)
    {
      SaveAction->Execute();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::ApplyConfiguration()
{
  bool PrevModified = EditorMemo->Modified;
  assert(Configuration);
  EditorMemo->SetFormat(WinConfiguration->Editor.FontName,
    WinConfiguration->Editor.FontHeight, (TFontCharset)WinConfiguration->Editor.FontCharset,
    IntToFontStyles(WinConfiguration->Editor.FontStyle), WinConfiguration->Editor.TabSize,
    WinConfiguration->Editor.WordWrap);
  EditorMemo->Modified = PrevModified;
  EditorMemo->ClearUndo();
  UpdateControls();
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
      StatusBar->Panels->Items[1]->Caption = FMTLOAD(EDITOR_COLUMN_STATUS,
        ((int)FCaretPos.x+1));
      AnsiString Character;
      if (FCaretPos.y >= 0 && FCaretPos.y < EditorMemo->Lines->Count)
      {
        AnsiString Line = EditorMemo->Lines->Strings[FCaretPos.y];
        if (FCaretPos.x+1 <= Line.Length())
        {
          Character = FMTLOAD(EDITOR_CHARACTER_STATUS2,
            (int((unsigned char)Line[FCaretPos.x+1]), int((unsigned char)Line[FCaretPos.x+1])));
        }
      }
      StatusBar->Panels->Items[2]->Caption = Character;
    }
    StatusBar->Panels->Items[3]->Caption =
      (EditorMemo->Modified ? LoadStr(EDITOR_MODIFIED) : AnsiString(""));
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
          MessageDialog(FMTLOAD(EDITOR_FIND_END, (EditorConfiguration.FindText)), qtInformation, qaOK, HELP_NONE);
        }
        else if (FReplaceDialog->Options.Contains(frReplaceAll))
        {
          MessageDialog(FMTLOAD(EDITOR_REPLACE_END, (Replacements)), qtInformation, qaOK, HELP_NONE);
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
  LoadFile();

  CutFormToDesktop(this);
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::LoadFile()
{
  EditorMemo->Lines->LoadFromFile(FFileName);
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
  void *DC;
  TTextMetric TM;
  TRect Rect;

  DC = GetDC(EditorMemo->Handle);
  OldFont = SelectObject(DC, EditorMemo->Font->Handle);

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
    FLastFindDialog->Left = Left + EditorMemo->Left + EditorMemo->Width / 2 - 100;
  }
  FLastFindDialog->Top = Top + EditorMemo->Top + (EditorMemo->Height / 4) +
    (CursorInUpperPart() ? (EditorMemo->Height / 2) : 0) - 40;
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::StartFind(bool Find)
{
  AnsiString Text = EditorMemo->SelText;
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
  AnsiString Str;
  if (InputDialog(LoadStr(EDITOR_GO_TO_LINE), LoadStr(EDITOR_LINE_NUMBER), Str))
  {
    int Line = StrToIntDef(Str, -1);
    if (Line <= 0 || Line > EditorMemo->Lines->Count)
    {
      throw Exception(LoadStr(EDITOR_INVALID_LINE));
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
  DoWindowClose();
  FCloseAnnounced = true;
  Action = caFree;
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::DoWindowClose()
{
  if (FOnWindowClose != NULL)
  {
    try
    {
      FOnWindowClose(this);
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
  TForm::CreateParams(Params);
  Params.WndParent = GetDesktopWindow();
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::Reload()
{
  if (!EditorMemo->Modified ||
      (MessageDialog(LoadStr(EDITOR_MODIFIED_RELOAD), qtConfirmation,
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
  AnsiString AHint = GetLongHint(Application->Hint);
  FShowStatusBarHint = Active && !AHint.IsEmpty();
  if (FShowStatusBarHint)
  {
    FStatusBarHint = AHint != "E" ? AHint : AnsiString("");
  }
  else
  {
    FStatusBarHint = "";
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::FormActivate(TObject * /*Sender*/)
{
  Application->OnHint = ApplicationHint;
}
//---------------------------------------------------------------------------
