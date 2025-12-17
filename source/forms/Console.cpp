//---------------------------------------------------------------------
#include <FormsPCH.h>
#pragma hdrstop

#include "Console.h"
//---------------------------------------------------------------------
#pragma link "HistoryComboBox"
#pragma link "PathLabel"
#pragma link "PngImageList"
#pragma resource "*.dfm"
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
  FDirectoryChanged = false;
  FExecuting = false;
  OutputMemo->Color = clBlack;
  OutputMemo->Font->Color = static_cast<TColor>(0x00BBBBBB);
  FixComboBoxResizeBug(CommandEdit);
  UseSystemSettings(this);
  SelectScaledImageList(Images);
  LoadDialogImage(Image, L"Open console window");
  OutputMemo->Font->Name = CustomWinConfiguration->DefaultFixedWidthFontName;
  OutputMemo->Font->Size = CustomWinConfiguration->DefaultFixedWidthFontSize;
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
      DebugAssert(FTerminal->OnClose == TerminalClose);
      FTerminal->OnClose = FPrevTerminalClose;
      DebugAssert(FTerminal->OnChangeDirectory == DoChangeDirectory);
      FTerminal->OnChangeDirectory = FOldChangeDirectory;
      FOldChangeDirectory = NULL;
      if (FDirectoryChanged)
      {
        FDirectoryChanged = false;
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
      // If ever these events are used elsewhere, use TMulticastEvent,
      // to avoid problems when the connnection is lost while the dialog is opened.
      DebugAssert(FTerminal->OnChangeDirectory == NULL);
      DebugAssert(FTerminal->OnClose == NULL);
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
  // Disabling buttons while executing to prevent recursive execution,
  // now that message queue is processed while command is executing.
  EnableControl(ExecuteButton,
    ((FTerminal != NULL) ? FTerminal->AllowedAnyCommand(CommandEdit->Text) : false) &&
    !FExecuting);
  EnableControl(CancelBtn, !FExecuting);
}
//---------------------------------------------------------------------
bool __fastcall TConsoleDialog::Execute(const UnicodeString Command,
  const TStrings * Log)
{
  try
  {
    if (Log != NULL)
    {
      OutputMemo->Lines->BeginUpdate();
      try
      {
        TStrings * ALog = const_cast<TStrings *>(Log);
        for (int i = 0; i < ALog->Count; i++)
        {
          AddLine(ALog->Strings[i], cotOutput);
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
    }
  }
  return true;
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::TerminalClose(TObject * Sender)
{
  // Not deassociating terminal here, leaving it to the destructor.
  // Because at this point, we could be in the midddle of a reconnect and the event handlers can be set to TTerminalThread.
  Close();
  if (FPrevTerminalClose)
  {
    FPrevTerminalClose(Sender);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::ExecuteButtonClick(TObject * /*Sender*/)
{
  // When pressing "Enter" key, focus is not lst and
  // the command is not saved (as oppisute to clicking the button by mouse)
  CommandEdit->SaveToHistory();
  ExecuteCommand();
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::DoExecuteCommand()
{
  CommandEdit->SelectAll();
  FTerminal->ExceptionOnFail = true;
  FClearExceptionOnFail = true;
  UnicodeString CurrentDirectory = FTerminal->CurrentDirectory;
  try
  {
    UnicodeString Command = CommandEdit->Text;
    OutputMemo->Lines->Add(FORMAT(L"%s$ %s", (CurrentDirectory, Command)));
    Configuration->Usage->Inc(L"RemoteCommandExecutions");
    TAutoFlag ExecutingFlag(FExecuting);
    UpdateControls();
    // give a chance for disabled buttons to redraw
    Application->ProcessMessages();
    FTerminal->AnyCommand(Command, AddLine);
  }
  __finally
  {
    if (FTerminal)
    {
      FTerminal->ExceptionOnFail = false;
      DebugAssert(FClearExceptionOnFail);
      FClearExceptionOnFail = false;
      if (FTerminal->Active)
      {
        FTerminal->ReadCurrentDirectory();
      }
    }
    UpdateControls();
  }

  if (CurrentDirectory != FTerminal->CurrentDirectory)
  {
    FDirectoryChanged = true;
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
    DebugAssert(FLastTerminal != NULL);
    // Should use the command session, if there's one, not to close the main session
    FLastTerminal->ShowExtendedException(&E);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::CommandEditChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::AddLine(const UnicodeString & Line, TCaptureOutputType OutputType)
{
  if ((OutputType == cotOutput) || (OutputType == cotError))
  {
    OutputMemo->Lines->Add(Line);
  }
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

    OutputMemo->Perform(EM_GETRECT, 0, reinterpret_cast<int>(&Rect));
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
  int RequiredWidth = (TM.tmAveCharWidth * Columns) + ScaleByTextHeight(this, 10);
  // there is always one line more
  int RequiredHeight = (TM.tmHeight + TM.tmExternalLeading) * (Rows + 1) + ScaleByTextHeight(this, 10);

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
void __fastcall TConsoleDialog::FormCloseQuery(TObject * /*Sender*/, bool & CanClose)
{
  // Probably not necessary as this is called from top-level dialog loop,
  // where we do not get until ExecuteButtonClick exists.
  CanClose = !FExecuting;
}
//---------------------------------------------------------------------------
void __fastcall TConsoleDialog::Dispatch(void * Message)
{
  TMessage * M = reinterpret_cast<TMessage*>(Message);
  if (M->Msg == WM_SYSCOMMAND)
  {
    if (!HandleMinimizeSysCommand(*M))
    {
      TForm::Dispatch(Message);
    }
  }
  else
  {
    TForm::Dispatch(Message);
  }
}
//---------------------------------------------------------------------------
