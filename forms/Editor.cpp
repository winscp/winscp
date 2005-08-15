//---------------------------------------------------------------------------
#include <vcl.h>        
#pragma hdrstop

#include <Common.h>
#include "Editor.h"
#include "WinInterface.h"
#include "TextsWin.h"
#include "Tools.h"
#include <ScpMain.h>
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
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
TForm * __fastcall ShowEditorForm(const AnsiString FileName, TCustomForm * ParentForm,
  TNotifyEvent OnFileChanged, TNotifyEvent OnClose, const AnsiString Caption,
  bool ShowWindowButton)
{
  TEditorForm * Dialog = new TEditorForm(Application);
  try
  {
    Dialog->ShowWindowButton = ShowWindowButton;
    Dialog->FileName = FileName;
    Dialog->ParentForm = ParentForm;
    Dialog->Caption = Caption.IsEmpty() ? FileName : Caption;
    Dialog->OnFileChanged = OnFileChanged;
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
  FParentForm = NULL;
  FCaretPos.x = -1;
  FCaretPos.y = -1;
  FLastFindDialog = NULL;
  FShowWindowButton = false;
  FCloseAnnounced = false;
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
      BoundsRect = value->BoundsRect;
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
  else if (Action == PreferencesAction || Action == CloseAction ||
    Action == FindAction || Action == ReplaceAction || Action == GoToLineAction ||
    Action == HelpAction)
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
  else if (Action == EditPaste)
  {
    // original source: http://home.att.net/~robertdunn/FAQs/Faqs.html
    // tell the Rich Edit control to insert unformatted text (CF_TEXT)
    REPASTESPECIAL RepasteSpecial = { 0, 0 };
    SendMessage(EditorMemo->Handle, EM_PASTESPECIAL, CF_TEXT,
      (LPARAM)&RepasteSpecial);
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
  EditorMemo->Font->Name = WinConfiguration->Editor.FontName;
  EditorMemo->Font->Height = WinConfiguration->Editor.FontHeight;
  EditorMemo->Font->Charset = (TFontCharset)WinConfiguration->Editor.FontCharset;
  EditorMemo->Font->Style = IntToFontStyles(WinConfiguration->Editor.FontStyle);
  EditorMemo->DefAttributes->Assign(EditorMemo->Font);
  if (EditorMemo->WordWrap != WinConfiguration->Editor.WordWrap)
  {
    if (Visible)
    {
      TStrings * Content = new TStringList();
      try
      {
        EditorMemo->WordWrap = False;
        Content->Assign(EditorMemo->Lines);
        EditorMemo->WordWrap = WinConfiguration->Editor.WordWrap;
        EditorMemo->Lines = Content;
        EditorMemo->CaretPos = TPoint(0, 0);
      }
      __finally
      {
        delete Content;
      }
    }
    else
    {
      EditorMemo->WordWrap = WinConfiguration->Editor.WordWrap;
    }
  }
  EditorMemo->Modified = PrevModified;
  EditorMemo->ClearUndo();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::UpdateControls()
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
        Character = FMTLOAD(EDITOR_CHARACTER_STATUS,
          (int((unsigned char)Line[FCaretPos.x+1]), int((unsigned char)Line[FCaretPos.x+1])));
      }
    }
    StatusBar->Panels->Items[2]->Caption = Character;
  }
  StatusBar->Panels->Items[3]->Caption =
    (EditorMemo->Modified ? LoadStr(EDITOR_MODIFIED) : AnsiString(""));
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
      EditorMemo->Text.Length(), SearchTypes);

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
  EditorMemo->Modified = false;
  FCaretPos.x = -1;
  ApplyConfiguration();
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
  Options << frHideUpDown; // not implemented
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
void __fastcall TEditorForm::SetShowWindowButton(bool value)
{
  if (value != ShowWindowButton)
  {
    FShowWindowButton = value;
    RecreateWnd();
  }
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::CreateParams(TCreateParams & Params)
{
  TForm::CreateParams(Params);
  if (ShowWindowButton)
  {
    Params.WndParent = GetDesktopWindow();
  }
}
//---------------------------------------------------------------------------

