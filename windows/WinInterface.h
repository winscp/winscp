//---------------------------------------------------------------------------
#ifndef WinInterfaceH
#define WinInterfaceH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Interface.h>
#include <GUIConfiguration.h>
#include <SynchronizeController.h>

class TStoredSessionList;
class TConfiguration;
class TTerminal;
class TSecureShell;

const int mpNeverAskAgainCheck =   0x01;
const int mpAllowContinueOnError = 0x02;

struct TMessageParams
{
  TMessageParams(unsigned int AParams = 0);
  TMessageParams(const TQueryParams * AParams);

  const TQueryButtonAlias * Aliases;
  unsigned int AliasesCount;
  unsigned int Params;
  unsigned int Timer;
  TQueryParamsTimerEvent TimerEvent;
  AnsiString TimerMessage;
  unsigned int TimerAnswers;

private:
  inline void Reset();
};

class TCustomScpExplorerForm;
TCustomScpExplorerForm * __fastcall CreateScpExplorer();

void __fastcall ConfigureInterface();

void __fastcall DoProductLicence();

extern const AnsiString AppName;
extern const AnsiString AppNameVersion;

void __fastcall FlashOnBackground();

void __fastcall ShowExtendedExceptionEx(TSecureShell * SecureShell, Exception * E);

// windows\WinHelp.cpp
void __fastcall InitializeWebHelp();
void __fastcall FinalizeWebHelp();

// windows\WinInterface.cpp
int __fastcall MessageDialog(const AnsiString Msg, TQueryType Type,
  int Answers, int HelpCtx = 0, const TMessageParams * Params = NULL);
int __fastcall MessageDialog(int Ident, TQueryType Type,
  int Answers, int HelpCtx = 0, const TMessageParams * Params = NULL);
int __fastcall SimpleErrorDialog(const AnsiString Msg);

int __fastcall MoreMessageDialog(const AnsiString Message,
  TStrings * MoreMessages, TQueryType Type, int Answers,
    int HelpCtx, const TMessageParams * Params = NULL);

int __fastcall ExceptionMessageDialog(Exception * E, TQueryType Type,
  const AnsiString MessageFormat = "", int Answers = qaOK, int HelpCtx = 0,
  const TMessageParams * Params = NULL);
int __fastcall FatalExceptionMessageDialog(Exception * E, TQueryType Type,
  const AnsiString MessageFormat = "", int Answers = qaOK, int HelpCtx = 0,
  const TMessageParams * Params = NULL);

// windows\WinMain.cpp
class TProgramParams;
int __fastcall Execute(TProgramParams * Params);

// forms\InputDlg.cpp
bool __fastcall InputDialog(const AnsiString ACaption,
  const AnsiString APrompt, AnsiString & Value, TStrings * History = NULL);

// forms\About.cpp
void __fastcall DoAboutDialog(TConfiguration *Configuration);

// forms\Cleanup.cpp
bool __fastcall DoCleanupDialog(TStoredSessionList *SessionList,
    TConfiguration *Configuration);

// forms\Console.cpp
void __fastcall DoConsoleDialog(TTerminal * Terminal,
    const AnsiString Command = "", const TStrings * Log = NULL);

// forms\Copy.cpp
const coDragDropTemp        = 0x01;
const coDisableQueue        = 0x02;
const coDisableTransferMode = 0x04;
const coDisableDirectory    = 0x08; // not used anymore
const coDisableNewerOnly    = 0x10;
bool __fastcall DoCopyDialog(bool ToRemote,
  bool Move, TStrings * FileList, AnsiString & TargetDirectory,
  TGUICopyParamType * Params, int Options);

// forms\CopyParams.cpp
enum TParamsForDirection { pdBoth, pdToRemote, pdToLocal, pdAll };

// forms\ImportSessions.cpp
bool __fastcall DoImportSessionsDialog(TStoredSessionList *SessionList);

// forms\Licence.cpp
enum TLicence { lcNoLicence = -1, lcWinScp, lcPutty, lcRX };
void __fastcall DoLicenceDialog(TLicence Licence);
void __fastcall DoLicenceDialog(const AnsiString LicenceText);

// forms\Login.cpp
const loLocalDirectory = 0x01;
const loLanguage       = 0x02;
const loTools          = 0x04;
const loLogWindow      = 0x08;
const loAbout          = 0x10;

const loPreferences    = 0x20;

const loNone           = 0x00;
const loAddSession     = (loLocalDirectory | loLogWindow);
const loStartup        = (loLocalDirectory | loLanguage | loTools |
  loLogWindow | loPreferences | loAbout);
bool __fastcall DoLoginDialog(TStoredSessionList * SessionList,
  TSessionData * Data, int Options);

// forms\OpenDirectory.cpp
enum TOpenDirectoryMode { odBrowse, odAddBookmark };
bool __fastcall DoOpenDirectoryDialog(TOpenDirectoryMode Mode, TOperationSide Side,
  AnsiString & Directory, TStrings * Directories, TTerminal * Terminal,
  bool AllowSwitch);

// forms\LocatinoProfiles.cpp
bool __fastcall LocationProfilesDialog(TOpenDirectoryMode Mode,
  TOperationSide Side, AnsiString & LocalDirectory, AnsiString & RemoteDirectory,
  TStrings * LocalDirectories, TStrings * RemoteDirectories, TTerminal * Terminal);

// forms\Preferences.cpp
enum TPreferencesMode { pmDefault, pmLogin, pmEditor, pmCustomCommands,
    pmQueue, pmTransfer };
typedef void __fastcall (__closure *TGetDefaultLogFileName)
  (System::TObject* Sender, AnsiString &DefaultLogFileName);
bool __fastcall DoPreferencesDialog(TPreferencesMode APreferencesMode);

// forms\CustomCommand.cpp
class TCustomCommands;
bool __fastcall DoCustomCommandDialog(AnsiString & Description,
  AnsiString & Command, int & Params, const TCustomCommands * CustomCommands,
  bool Edit);

// forms\Password.cpp
bool __fastcall DoPasswordDialog(const AnsiString Caption, TPromptKind Kind,
  AnsiString & Password);

// forms\Properties.cpp
class TRemoteProperties;
const cpMode =  0x01;
const cpOwner = 0x02;
const cpGroup = 0x04;
bool __fastcall DoPropertiesDialog(TStrings * FileList,
    const AnsiString Directory, TStrings * GroupList, TStrings * UserList,
    TRemoteProperties * Properties, int AllowedChanges,
    TTerminal * Terminal);

// forms\ComboInput.cpp
bool __fastcall DoComboInputDialog(
  const AnsiString Caption, const AnsiString Prompt, AnsiString & Text,
  TStrings * Items, TCloseQueryEvent OnCloseQuery, bool AllowEmpty,
  const AnsiString HelpKeyword = "");
AnsiString __fastcall DoSaveSessionDialog(
  TStoredSessionList * SessionList, const AnsiString DefaultName);
bool __fastcall DoRemoteTransferDialog(TStrings * FileList, AnsiString & Target,
  AnsiString & FileMask, bool Move);

// forms\SelectMask.cpp
#ifdef CustomDirViewHPP
bool __fastcall DoSelectMaskDialog(TCustomDirView * Parent, bool Select,
    TFileFilter * Filter, TConfiguration * Configuration);
#endif

const spDelete = 0x01;
const spNoConfirmation = 0x02;
const spExistingOnly = 0x04;
const spPreviewChanges = 0x40;

// forms\Synchronize.cpp
bool __fastcall DoSynchronizeDialog(TSynchronizeParamType & Params,
  TSynchronizeStartStopEvent OnStartStop, bool & SaveSettings);

// forms\FullSynchronize.cpp
enum TSynchronizeMode { smRemote, smLocal, smBoth };
bool __fastcall DoFullSynchronizeDialog(TSynchronizeMode & Mode, int & Params,
  AnsiString & LocalDirectory, AnsiString & RemoteDirectory, bool & SaveSettings);

TForm * __fastcall ShowEditorForm(const AnsiString FileName, TCustomForm * ParentForm,
  TNotifyEvent OnFileChanged, TNotifyEvent OnClose, const AnsiString Caption = "",
  bool ShowWindowButton = false);

bool __fastcall DoSymlinkDialog(AnsiString & FileName, AnsiString & PointTo,
  TOperationSide Side, bool & SymbolicLink, bool Edit, bool AllowSymbolic);

// forms\FileSystemInfo.cpp
void __fastcall DoFileSystemInfoDialog(TTerminal * Terminal);

// forms\MessageDlg.cpp
TForm * __fastcall CreateMoreMessageDialog(const AnsiString & Msg,
  TStrings * MoreMessages, TMsgDlgType DlgType, TMsgDlgButtons Buttons,
  TQueryButtonAlias * Aliases = NULL, unsigned int AliasesCount = 0);

// windows\\Console.cpp
int __fastcall Console(TProgramParams * Params, bool Help);

//---------------------------------------------------------------------------
class TWinInteractiveCustomCommand : public TInteractiveCustomCommand
{
public:
  TWinInteractiveCustomCommand(TCustomCommand * ChildCustomCommand,
    const AnsiString CustomCommandName);

protected:
  virtual void __fastcall Prompt(int Index, const AnsiString & Prompt,
    AnsiString & Value);

private:
  AnsiString FCustomCommandName;
};
//---------------------------------------------------------------------------
#endif // WinInterfaceH
