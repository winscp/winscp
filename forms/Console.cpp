//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Console.h"

#include <Common.h>
#include <TextsWin.h>
#include <Interface.h>
#include <ScpFileSystem.h>

#include <VCLCommon.h>
//---------------------------------------------------------------------
#pragma link "HistoryComboBox"
#pragma link "PathLabel"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
void __fastcall DoConsoleDialog(TTerminal * Terminal)
{
  TConsoleDialog * Dialog = new TConsoleDialog(Application);
  try {
    Dialog->Terminal = Terminal;
    Dialog->Execute();
  } __finally {
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
  UseSystemFont(this);
  try {
    OutputMemo->Font->Name = "Courier New";
  } catch(...) { }
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
      FTerminal->OnChangeDirectory = FOldChangeDirectory;
      FTerminal->Log->OnAddLine = FOldLogAddLine;
      FOldChangeDirectory = NULL;
      FOldLogAddLine = NULL;
    }
    FTerminal = value;
    if (FTerminal)
    {
      FOldChangeDirectory = FTerminal->OnChangeDirectory;
      FTerminal->OnChangeDirectory = DoChangeDirectory;
      FOldLogAddLine = FTerminal->Log->OnAddLine;
      FTerminal->Log->OnAddLine = DoLogAddLine;
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
Boolean __fastcall TConsoleDialog::Execute()
{
  try {
    // avoid reloading directory after each change of current directory from console
    if (FTerminal)
    {
      FTerminal->BeginTransaction();
      if (FTerminal->Configuration->CommandsHistory->Count)
        CommandEdit->Items = FTerminal->Configuration->CommandsHistory;
      else
        CommandEdit->Items->Clear();
    }
    ShowModal();
  } __finally {
    if (FTerminal)
    {
      FTerminal->EndTransaction();
      FTerminal->Configuration->CommandsHistory = CommandEdit->Items;
    }
  }
  return true;
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
  try {
    FTerminal->ExceptionOnFail = true;
    try {
      AnsiString Command = CommandEdit->Text;
      OutputMemo->Lines->Add(FORMAT("$ %s", ((Command))));
      FAddOutput = true;
      FTerminal->AnyCommand(Command);
    } __finally {
      FAddOutput = false;
      FTerminal->ExceptionOnFail = false;
      if (FTerminal->Active) FTerminal->ReadCurrentDirectory();
    }
  } catch(Exception & E) {
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
