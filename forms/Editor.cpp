//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <assert.h>

#include "Editor.h"
#include "WinInterface.h"
#include "TextsWin.h"
#include "Tools.h"
#include <Common.h>
#include <ScpMain.h>
#include "VCLCommon.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
void __fastcall DoEditorForm(const AnsiString FileName, TCustomForm * ParentForm,
  TNotifyEvent OnFileChanged, const AnsiString Caption)
{
  TEditorForm * Dialog = new TEditorForm(Application);
  try
  {
    Dialog->FileName = FileName;
    Dialog->ParentForm = ParentForm;
    Dialog->Caption = Caption.IsEmpty() ? FileName : Caption;
    Dialog->OnFileChanged = OnFileChanged;
    Dialog->Execute();
  }
  __finally
  {
    delete Dialog;
  }
}
//---------------------------------------------------------------------------
__fastcall TEditorForm::TEditorForm(TComponent* Owner)
  : TForm(Owner)
{
  FParentForm = NULL;
  FCaretPos.x = -1;
  FCaretPos.y = -1;
  FLastFindDialog = NULL;
  ApplyConfiguration();
  FindDialog->FindText = Configuration->Editor.FindText;
  TFindOptions Options = FindDialog->Options;
  if (Configuration->Editor.FindMatchCase)
  {
    Options << frMatchCase;
  }
  if (Configuration->Editor.FindWholeWord)
  {
    Options << frWholeWord;
  }
  FindDialog->Options = Options;
  ReplaceDialog->FindText = FindDialog->FindText;
  ReplaceDialog->Options = FindDialog->Options;
  ReplaceDialog->ReplaceText = Configuration->Editor.ReplaceText;
  UseSystemFont(this);
}
//---------------------------------------------------------------------------
__fastcall TEditorForm::~TEditorForm()
{
  if (FLastFindDialog)
  {
    Configuration->Editor.FindText = FLastFindDialog->FindText;
    Configuration->Editor.ReplaceText = ReplaceDialog->ReplaceText;
    Configuration->Editor.FindMatchCase = FLastFindDialog->Options.Contains(frMatchCase);
    Configuration->Editor.FindWholeWord = FLastFindDialog->Options.Contains(frWholeWord);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TEditorForm::Execute()
{
  ShowModal();
  return true;
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
      FLastFindDialog != NULL || !FindDialog->FindText.IsEmpty();
  }
  else if (Action == PreferencesAction || Action == CloseAction ||
    Action == FindAction || Action == ReplaceAction || Action == GoToLineAction)
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
    if (DoPreferencesDialog(pmEditor))
    {
      ApplyConfiguration();
    }
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
      FLastFindDialog = FindDialog;
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
  EditorMemo->Font->Name = Configuration->Editor.FontName;
  EditorMemo->Font->Height = Configuration->Editor.FontHeight;
  EditorMemo->Font->Charset = (TFontCharset)Configuration->Editor.FontCharset;
  EditorMemo->Font->Style = IntToFontStyles(Configuration->Editor.FontStyle);
  EditorMemo->DefAttributes->Assign(EditorMemo->Font);
  if (EditorMemo->WordWrap != Configuration->Editor.WordWrap)
  {
    if (Visible)
    {
      TStrings * Content = new TStringList();
      try
      {
        Content->Assign(EditorMemo->Lines);
        EditorMemo->WordWrap = Configuration->Editor.WordWrap;
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
      EditorMemo->WordWrap = Configuration->Editor.WordWrap;
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
    StatusBar->Panels->Items[0]->Text = FMTLOAD(EDITOR_LINE_STATUS,
      ((int)FCaretPos.y+1, Count));
    StatusBar->Panels->Items[1]->Text = FMTLOAD(EDITOR_COLUMN_STATUS,
      ((int)FCaretPos.x+1));
    if (FCaretPos.y >= 0 && FCaretPos.y < EditorMemo->Lines->Count)
    {
      AnsiString Line = EditorMemo->Lines->Strings[FCaretPos.y];
      if (FCaretPos.x+1 <= Line.Length())
      {
        StatusBar->Panels->Items[2]->Text = FMTLOAD(EDITOR_CHARACTER_STATUS,
          ((int)Line[FCaretPos.x+1], (int)Line[FCaretPos.x+1]));
      }
      else
      {
        StatusBar->Panels->Items[2]->Text = "";
      }
    }
  }
  StatusBar->Panels->Items[3]->Text =
    (EditorMemo->Modified ? LoadStr(EDITOR_MODIFIED) : AnsiString(""));
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
    if (FLastFindDialog == ReplaceDialog &&
        (ReplaceDialog->Options.Contains(frReplace) ||
         ReplaceDialog->Options.Contains(frReplaceAll)) &&
        ReplaceDialog->FindText.Length() == EditorMemo->SelLength &&
        AnsiSameText(ReplaceDialog->FindText, EditorMemo->SelText))
    {
      EditorMemo->SelText = ReplaceDialog->ReplaceText;
      Replacements++;
    }

    if (FLastFindDialog->Options.Contains(frMatchCase))
    {
      SearchTypes << stMatchCase;
    }
    if (FLastFindDialog->Options.Contains(frWholeWord))
    {
      SearchTypes << stWholeWord;
    }

    NewPos = EditorMemo->FindText(FLastFindDialog->FindText,
      EditorMemo->SelLength ? EditorMemo->SelStart+1 : EditorMemo->SelStart,
      EditorMemo->Text.Length(), SearchTypes);

    if (NewPos >= 0)
    {
      EditorMemo->SelStart = NewPos;
      EditorMemo->SelLength = FLastFindDialog->FindText.Length();
    }

    if (FLastFindDialog->Handle)
    {
      PositionFindDialog(true);
    }

    if (NewPos < 0)
    {
      if (!Replacements)
      {
        MessageDialog(FMTLOAD(EDITOR_FIND_END, (FLastFindDialog->FindText)), qtInformation, qaOK, 0);
      }
      else
      {
        MessageDialog(FMTLOAD(EDITOR_REPLACE_END, (Replacements)), qtInformation, qaOK, 0);
      }
    }
  }
  while (NewPos >= 0 && FLastFindDialog == ReplaceDialog &&
         ReplaceDialog->Options.Contains(frReplaceAll));
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::FormShow(TObject * /*Sender*/)
{
  LoadFile();
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
    FLastFindDialog->Left = EditorMemo->Left + EditorMemo->Width / 2 - 100;
  }
  FLastFindDialog->Top = EditorMemo->Top + (EditorMemo->Height / 4) +
    (CursorInUpperPart() ? (EditorMemo->Height / 2) : 0) - 30;
}
//---------------------------------------------------------------------------
void __fastcall TEditorForm::StartFind(bool Find)
{
  AnsiString Text = EditorMemo->SelText;
  TFindOptions Options;
  if (FLastFindDialog)
  {
    Options = FLastFindDialog->Options;
    if (Text.IsEmpty())
    {
      Text = FLastFindDialog->FindText;
    }
  }
  TFindDialog * Dialog = Find ? FindDialog : ReplaceDialog;
  if (FLastFindDialog && Dialog != FLastFindDialog && FLastFindDialog->Handle)
  {
    FLastFindDialog->CloseDialog();
  }
  FLastFindDialog = Dialog;
  if (!Text.IsEmpty())
  {
    FLastFindDialog->FindText = Text;
  }
  if (!Options.Empty())
  {
    FLastFindDialog->Options = Options;
  }
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



