//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Console.h"

#include <Common.h>
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
void __fastcall DoConsoleDialog(TTerminal * Terminal, const AnsiString Command)
{
  TConsoleDialog * Dialog = new TConsoleDialog(Application);
  try
  {
    Dialog->Terminal = Terminal;
    Dialog->Execute(Command);
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
  EnableControl(ExecuteButton, !CommandEdit->Text.IsEmpty());
}
//---------------------------------------------------------------------
bool __fastcall TConsoleDialog::Execute(const AnsiString Command)
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
    ShowExtendedException(&E, this);
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
  if (FAddOutput &&
      !AddedLine.IsEmpty() && (AddedLine[1] == '<' || AddedLine[1] == '!'))
  {
    int ReturnCode;
    AnsiString Line = AddedLine;
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

