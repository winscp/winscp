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
#include "WinConfiguration.h"
#include "TerminalManager.h"
//---------------------------------------------------------------------
#pragma link "HistoryComboBox"
#pragma link "PathLabel"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
void __fastcall DoConsoleDialog()
{
  TConsoleDialog * Dialog = new TConsoleDialog(Application);
  try
  {
    Dialog->Execute();
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
bool __fastcall TConsoleDialog::Execute()
{
  TTerminalManager * Manager = TTerminalManager::Instance();
  TNotifyEvent POnChangeTerminal = Manager->OnChangeTerminal;
  Manager->OnChangeTerminal = TerminalManagerChangeTerminal;
  try
  {
    Terminal = TTerminalManager::Instance()->ActiveTerminal;
    if (FTerminal)
    {
      if (WinConfiguration->CommandsHistory->Count)
      {
        CommandEdit->Items = WinConfiguration->CommandsHistory;
      }
      else
      {
        CommandEdit->Items->Clear();
      }
    }
    ShowModal();
  }
  __finally
  {
    assert(Manager->OnChangeTerminal == TerminalManagerChangeTerminal);
    Manager->OnChangeTerminal = POnChangeTerminal;
    if (FTerminal)
    {
      WinConfiguration->CommandsHistory = CommandEdit->Items;
    }
  }
  return true;
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::TerminalManagerChangeTerminal(TObject * /*Sender*/)
{
  TTerminal * NewTerminal = TTerminalManager::Instance()->ActiveTerminal;
  if (!NewTerminal || NewTerminal->IsCapable[fcAnyCommand])
  {
    Terminal = TTerminalManager::Instance()->ActiveTerminal;
  }
  else
  {
    Terminal = NULL;
    Close();
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::ExecuteButtonClick(TObject * /*Sender*/)
{
  ExecuteCommand();
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::ExecuteCommand()
{
  if (!FTerminal) return;
  CommandEdit->SelectAll();
  try
  {
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
      FTerminal->ExceptionOnFail = false;
      if (FTerminal->Active) FTerminal->ReadCurrentDirectory();
    }
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
