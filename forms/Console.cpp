//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <TextsWin.h>
#include <Interface.h>
#include <CoreMain.h>
#include <VCLCommon.h>
#include <CustomWinConfiguration.h>

#include "Console.h"
#include <Tools.h>
//---------------------------------------------------------------------
#pragma link "HistoryComboBox"
#pragma link "PathLabel"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------
void __fastcall DoConsoleDialog(TTerminal * Terminal, const UnicodeString Command,
  const TStrings * Log)
{
  TConsoleDialog * Dialog = new TConsoleDialog(Application);
  try
  {
    Dialog->Terminal = Terminal;
    Dialog->Execute(Command, Log);
  }
  __finally
  {
    delete Dialog;
  }
}
//---------------------------------------------------------------------
__fastcall TConsoleDialog::TConsoleDialog(TComponent* AOwner)
    : TForm(AOwner)
{
  FTerminal = NULL;
  FClearExceptionOnFail = false;
  FOldChangeDirectory = NULL;
  FPrevTerminalClose = NULL;;
  FLastTerminal = NULL;
  FAnyCommandExecuted = false;
  OutputMemo->Color = clBlack;
  OutputMemo->Font->Color = (TColor)0x00BBBBBB; //clGray;
  FixComboBoxResizeBug(CommandEdit);
  UseSystemSettings(this);
  try
  {
    OutputMemo->Font->Name = L"Courier New";
  }
  catch(...)
  {
  }
}
//---------------------------------------------------------------------
__fastcall TConsoleDialog::~TConsoleDialog()
{
  Terminal = NULL;
}
//---------------------------------------------------------------------
void __fastcall TConsoleDialog::SetTerminal(TTerminal * value)
{
  if (FTerminal != value)
  {
    if (FTerminal)
    {
      if (FClearExceptionOnFail)
      {
        FTerminal->ExceptionOnFail = false;
        FClearExceptionOnFail = false;
      }
      assert(FTerminal->OnClose == TerminalClose);
      FTerminal->OnClose = FPrevTerminalClose;
      assert(FTerminal->OnChangeDirectory == DoChangeDirectory);
      FTerminal->OnChangeDirectory = FOldChangeDirectory;
      FOldChangeDirectory = NULL;
      if (FAnyCommandExecuted)
      {
        FAnyCommandExecuted = false;
        if (FTerminal->Active)
        {
          // directory would be read from EndTransaction anyway,
          // but with reload only flag set, what prevents
          // recording path in history, what we want if the path was
          // changed by "cd" command in console
          FTerminal->ReadDirectory(false);
        }
      }
      FTerminal->EndTransaction();
    }
    FTerminal = value;
    if (FTerminal)
    {
      OutputMemo->Clear();
      FOldChangeDirectory = FTerminal->OnChangeDirectory;
      FTerminal->OnChangeDirectory = DoChangeDirectory;
      // avoid reloading directory after each change of current directory from console
      FTerminal->BeginTransaction();
      FLastTerminal = FTerminal;
      FPrevTerminalClose = FTerminal->OnClose;
      // used instead of previous TTerminalManager::OnChangeTerminal
      FTerminal->OnClose = TerminalClose;
    }
    UpdateControls();
  }
}
//---------------------------------------------------------------------
void __fastcall TConsoleDialog::DoChangeDirectory(TObject * Sender)
{
  if (FOldChangeDirectory) FOldChangeDirectory(Sender);
  UpdateControls();
}
//---------------------------------------------------------------------
void __fastcall TConsoleDialog::UpdateControls()
{
  DirectoryLabel->Caption = (FTerminal ? FTerminal->CurrentDirectory : UnicodeString());
  EnableControl(ExecuteButton,
    (FTerminal != NULL) ? FTerminal->AllowedAnyCommand(CommandEdit->Text) : false);
}
//---------------------------------------------------------------------
bool __fastcall TConsoleDialog::Execute(const UnicodeString Command,
  const TStrings * Log)
{
  try
  {
    CommandEdit->Items = CustomWinConfiguration->History[L"Commands"];

    if (Log != NULL)
    {
      OutputMemo->Lines->BeginUpdate();
      try
      {
        TStrings * ALog = const_cast<TStrings *>(Log);
        for (int i = 0; i < ALog->Count; i++)
        {
          AddLine(ALog->Strings[i], false);
        }
      }
      __finally
      {
        OutputMemo->Lines->EndUpdate();
      }
    }

    if (!Command.IsEmpty())
    {
      CommandEdit->Text = Command;
      DoExecuteCommand();
    }
    UpdateControls();
    ShowModal();

    TConsoleWinConfiguration ConsoleWin = CustomWinConfiguration->ConsoleWin;
    if ((FAutoBounds.Width() != Width) ||
        (FAutoBounds.Height() != Height))
    {
      ConsoleWin.WindowSize = StoreFormSize(this);
    }
    CustomWinConfiguration->ConsoleWin = ConsoleWin;
  }
  __finally
  {
    if (FTerminal)
    {
      CommandEdit->SaveToHistory();
      CustomWinConfiguration->History[L"Commands"] = CommandEdit->Items;
    }
  }
  return true;
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::TerminalClose(TObject * Sender)
{
  Close();
  Terminal = NULL;
  if (FPrevTerminalClose)
  {
    FPrevTerminalClose(Sender);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::ExecuteButtonClick(TObject * /*Sender*/)
{
  ExecuteCommand();
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::DoExecuteCommand()
{
  CommandEdit->SelectAll();
  FTerminal->ExceptionOnFail = true;
  FClearExceptionOnFail = true;
  try
  {
    UnicodeString Command = CommandEdit->Text;
    OutputMemo->Lines->Add(FORMAT(L"%s$ %s", (FTerminal->CurrentDirectory, Command)));
    FAnyCommandExecuted = true;
    Configuration->Usage->Inc(L"RemoteCommandExecutions");
    FTerminal->AnyCommand(Command, AddLine);
  }
  __finally
  {
    if (FTerminal)
    {
      FTerminal->ExceptionOnFail = false;
      assert(FClearExceptionOnFail);
      FClearExceptionOnFail = false;
      if (FTerminal->Active)
      {
        FTerminal->ReadCurrentDirectory();
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::ExecuteCommand()
{
  try
  {
    DoExecuteCommand();
  }
  catch(Exception & E)
  {
    assert(FLastTerminal != NULL);
    FLastTerminal->ShowExtendedException(&E);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::CommandEditChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::AddLine(const UnicodeString & Line, bool /*StdError*/)
{
  OutputMemo->Lines->Add(Line);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::CreateParams(TCreateParams & Params)
{
  TForm::CreateParams(Params);
  // we no longer exclude WS_SYSMENU, was there any reason for that, apart from
  // hidding the window icon?
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::DoAdjustWindow()
{
  HGDIOBJ OldFont;
  HDC DC;
  TTextMetric TM;
  TRect Rect;

  DC = GetDC(OutputMemo->Handle);
  OldFont = SelectObject(DC, OutputMemo->Font->Handle);

  try
  {
    GetTextMetrics(DC, &TM);

    OutputMemo->Perform(EM_GETRECT, 0, ((int)&Rect));
  }
  __finally
  {
    SelectObject(DC, OldFont);
    ReleaseDC(OutputMemo->Handle, DC);
  }

  int Rows = OutputMemo->Lines->Count;
  int Columns = 0;
  for (int Index = 0; Index < Rows; Index++)
  {
    int Len = OutputMemo->Lines->Strings[Index].Length();
    if (Columns < Len)
    {
      Columns = Len;
    }
  }

  // 10 is surplus to cover any borders, etc.
  int RequiredWidth = (TM.tmAveCharWidth * Columns) + 10;
  // thre is always one line more
  int RequiredHeight = (TM.tmHeight + TM.tmExternalLeading) * (Rows + 1) + 10;

  int CurrentWidth = (Rect.Right - Rect.Left);
  int CurrentHeight = (Rect.Bottom - Rect.Top);

  ResizeForm(this,
    Width + (RequiredWidth - CurrentWidth),
    Height + (RequiredHeight - CurrentHeight));
  FAutoBounds = BoundsRect;
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::ActionListExecute(TBasicAction * Action,
  bool & Handled)
{
  if (Action == AdjustWindow)
  {
    DoAdjustWindow();
    Handled = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::ActionListUpdate(TBasicAction * Action,
  bool & Handled)
{
  if (Action == AdjustWindow)
  {
    Handled = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::FormShow(TObject * /*Sender*/)
{
  UpdateFormPosition(this, poOwnerFormCenter);
  RestoreFormSize(CustomWinConfiguration->ConsoleWin.WindowSize, this);
  FAutoBounds = BoundsRect;
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::OutputMemoContextPopup(TObject * Sender,
  TPoint & MousePos, bool & Handled)
{
  MenuPopup(Sender, MousePos, Handled);
}
//---------------------------------------------------------------------------
