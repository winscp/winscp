//---------------------------------------------------------------------------
#ifndef WinInterfaceH
#define WinInterfaceH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Interface.h>

class TStoredSessionList;
class TConfiguration;
class TTerminal;

const int mpNeverAskAgainCheck =   0x01;
const int mpAllowContinueOnError = 0x02;

// windows\WinInterface.cpp
int __fastcall MessageDialog(const AnsiString Msg, TQueryType Type,
  int Answers, int HelpCtx = 0, int Params = 0);
int __fastcall MessageDialog(int Ident, TQueryType Type,
  int Answers, int HelpCtx = 0, int Params = 0);
int __fastcall SimpleErrorDialog(const AnsiString Msg);

int __fastcall MoreMessageDialog(const AnsiString Message,
  TStrings * MoreMessages, TQueryType Type, int Answers,
  int HelpCtx, int Params = 0);

int __fastcall FatalExceptionMessageDialog(Exception * E,
  TQueryType Type, int HelpCtx = 0);
int __fastcall ExceptionMessageDialog(Exception * E,
  TQueryType Type, int Answers, int HelpCtx = 0);

// windows\WinMain.cpp
class TProgramParams;
void __fastcall Execute(TProgramParams * Params);
//void __fastcall ReconnectTerminal();

// forms\InputDlg.cpp
TPoint __fastcall GetAveCharSize(TCanvas* Canvas);
bool __fastcall InputDialog(const AnsiString ACaption,
  const AnsiString APrompt, AnsiString &Value);

// forms\About.cpp
void __fastcall DoAboutDialog(TConfiguration *Configuration);

// forms\Cleanup.cpp
bool __fastcall DoCleanupDialog(TStoredSessionList *SessionList,
    TConfiguration *Configuration);

// forms\Console.cpp
void __fastcall DoConsoleDialog();

// forms\Copy.cpp
#ifdef UnixDirViewH
bool __fastcall DoCopyDialog(TTransferDirection Direction,
  TTransferType Type, bool DragDrop, TStrings * FileList,
  bool AllowTransferMode, AnsiString & TargetDirectory,
  TCopyParamType * Params);
#endif

// forms\CopyParams.cpp
enum TParamsForDirection { pdBoth, pdToRemote, pdToLocal, pdAll };

// forms\ImportSessions.cpp
bool __fastcall DoImportSessionsDialog(TStoredSessionList *SessionList);

// forms\Licence.cpp
enum TLicence { lcNoLicence = -1, lcWinScp, lcPutty, lcRX };
void __fastcall DoLicenceDialog(TLicence Licence);
void __fastcall DoLicenceDialog(const AnsiString LicenceText);

// forms\Login.cpp
bool __fastcall DoLoginDialog(TStoredSessionList * SessionList, TSessionData * Data);

// forms\OpenDirectory.cpp
enum TOpenDirectoryMode { odBrowse, odAddBookmark };
bool __fastcall OpenDirectoryDialog(TOpenDirectoryMode Mode, TOperationSide Side,
  AnsiString & Directory, TStrings * directories, TTerminal * Terminal);

// forms\LocatinoProfiles.cpp
bool __fastcall LocationProfilesDialog(TOpenDirectoryMode Mode,
  TOperationSide Side, AnsiString & LocalDirectory, AnsiString & RemoteDirectory,
  TStrings * RemoteDirectories, TTerminal * Terminal);

// forms\Preferences.cpp
enum TPreferencesMode { pmDefault, pmLogin, pmEditor, pmCustomCommands };
typedef void __fastcall (__closure *TGetDefaultLogFileName)
  (System::TObject* Sender, AnsiString &DefaultLogFileName);
bool __fastcall DoPreferencesDialog(TPreferencesMode APreferencesMode);

// forms\Password.cpp
bool __fastcall DoPasswordDialog(
    const AnsiString Caption, AnsiString &Password);

// forms\Properties.cpp
class TRemoteProperties;
const cpMode =  0x01;
const cpOwner = 0x02;
const cpGroup = 0x04;
bool __fastcall DoPropertiesDialog(TStrings * FileList,
    const AnsiString Directory, TStrings * GroupList,
    TRemoteProperties * Properties, int AllowedChanges);

// forms\ComboInput.cpp
bool __fastcall DoComboInputDialog(
  const AnsiString Caption, const AnsiString Prompt, AnsiString & Text,
  TStrings * Items, TCloseQueryEvent OnCloseQuery, bool AllowEmpty);
AnsiString __fastcall DoSaveSessionDialog(
  TStoredSessionList * SessionList, const AnsiString DefaultName);

// forms\SelectMask.cpp
#ifdef CustomDirViewHPP
bool __fastcall DoSelectMaskDialog(TCustomDirView * Parent, bool Select,
    TFileFilter * Filter, TConfiguration * Configuration);
#endif

// forms\Synchronize.cpp
class TSynchronizeParamType {
public:
  TCopyParamType CopyParams;
  bool AllowTransferMode;
  AnsiString LocalDirectory;
  AnsiString RemoteDirectory;

  TSynchronizeParamType __fastcall operator =(TSynchronizeParamType rhp);
  void __fastcall Assign(TSynchronizeParamType Source);
};
enum TSynchronizationStatus { ssStopped, ssWaiting, ssSynchronize, ssSynchronizing };
typedef void __fastcall (__closure * TSynchronizeStartStopEvent)
  (System::TObject * Sender, bool Start, TSynchronizeParamType Params);
void __fastcall DoSynchronizeDialog(TSynchronizeParamType Params,
    TSynchronizeStartStopEvent OnStartStop);

void __fastcall DoEditorForm(const AnsiString FileName, TCustomForm * ParentForm,
  TNotifyEvent OnFileChanged, const AnsiString Caption = "");

bool __fastcall DoSymlinkDialog(AnsiString & FileName, AnsiString & PointTo,
  TOperationSide Side, bool & SymbolicLink, bool Edit, bool AllowSymbolic);

// windows\WinMain.cpp
void __fastcall CheckForUpdates();

//---------------------------------------------------------------------------
#endif // WinInterfaceH
