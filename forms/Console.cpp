//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "Console.h"

#include <TextsWin.h>
#include <Interface.h>
#include <ScpFileSystem.h>
#include <ScpMain.h>

#include <VCLCommon.h>
#include <CustomWinConfiguration.h>
//---------------------------------------------------------------------
#pragma link "HistoryComboBox"
#pragma link "PathLabel"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
void __fastcall DoConsoleDialog(TTerminal * Terminal, const AnsiString Command,
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
  FOldLogAddLine = NULL;
  FOldChangeDirectory = NULL;
  FLastTerminal = NULL;
  FAddOutput = false;
  OutputMemo->Color = clBlack;
  OutputMemo->Font->Color = (TColor)0x00BBBBBB; //clGray;
  UseSystemSettings(this);
  try
  {
    OutputMemo->Font->Name = "Courier New";
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
      assert(FTerminal->OnChangeDirectory == DoChangeDirectory);
      FTerminal->OnChangeDirectory = FOldChangeDirectory;
      assert(FTerminal->Log->OnAddLine == DoLogAddLine);
      FTerminal->Log->OnAddLine = FOldLogAddLine;
      FOldChangeDirectory = NULL;
      FOldLogAddLine = NULL;
      FTerminal->EndTransaction();
    }
    FTerminal = value;
    if (FTerminal)
    {
      OutputMemo->Clear();
      FOldChangeDirectory = FTerminal->OnChangeDirectory;
      FTerminal->OnChangeDirectory = DoChangeDirectory;
      FOldLogAddLine = FTerminal->Log->OnAddLine;
      FTerminal->Log->OnAddLine = DoLogAddLine;
      // avoid reloading directory after each change of current directory from console
      FTerminal->BeginTransaction();
      FLastTerminal = FTerminal;
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
  DirectoryLabel->Caption = (FTerminal ? FTerminal->CurrentDirectory : AnsiString());
  EnableControl(ExecuteButton,
    (FTerminal != NULL) ? FTerminal->AllowedAnyCommand(CommandEdit->Text) : false);
}
//---------------------------------------------------------------------
bool __fastcall TConsoleDialog::Execute(const AnsiString Command,
  const TStrings * Log)
{
  FPrevTerminalClose = NULL;;
  if (FTerminal)
  {
    FPrevTerminalClose = FTerminal->OnClose;
    // used instead of previous TTerminalManager::OnChangeTerminal
    FTerminal->OnClose = TerminalClose;
  }
  
  try
  {
    TStrings * CommandsHistory = CustomWinConfiguration->History["Commands"];
    if ((CommandsHistory != NULL) && (CommandsHistory->Count > 0))
    {
      CommandEdit->Items = CommandsHistory;
    }
    else
    {
      CommandEdit->Items->Clear();
    }

    if (Log != NULL)
    {
      OutputMemo->Lines->BeginUpdate();
      try
      {
        TStrings * ALog = const_cast<TStrings *>(Log);
        for (int i = 0; i < ALog->Count; i++)
        {
          AddLine(ALog->Strings[i]);
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
    ShowModal();
  }
  __finally
  {
    if (FTerminal)
    {
      assert(FTerminal->OnClose == TerminalClose);
      FTerminal->OnClose = FPrevTerminalClose;
      CustomWinConfiguration->History["Commands"] = CommandEdit->Items;
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
  try
  {
    AnsiString Command = CommandEdit->Text;
    OutputMemo->Lines->Add(FORMAT("$ %s", ((Command))));
    FAddOutput = true;
    FTerminal->AnyCommand(Command);
  }
  __finally
  {
    FAddOutput = false;
    if (FTerminal)
    {
      FTerminal->ExceptionOnFail = false;
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
    FLastTerminal->DoShowExtendedException(&E);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::CommandEditChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::DoLogAddLine(TObject* /*Sender*/,
  const AnsiString AddedLine)
{
  if (FAddOutput)
  {
    AddLine(AddedLine);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::AddLine(AnsiString Line)
{
  if (!Line.IsEmpty() && (Line[1] == '<' || Line[1] == '!'))
  {
    int ReturnCode;
    Line.Delete(1, 2);
    if (!TSCPFileSystem::RemoveLastLine(Line, ReturnCode) ||
        !Line.IsEmpty())
    {
      OutputMemo->Lines->Add(Line);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::CreateParams(TCreateParams & Params)
{
  TForm::CreateParams(Params);
  Params.Style = Params.Style & ~WS_SYSMENU;
}
//---------------------------------------------------------------------------

