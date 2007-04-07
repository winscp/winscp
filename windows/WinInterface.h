//---------------------------------------------------------------------------
#ifndef WinInterfaceH
#define WinInterfaceH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Buttons.hpp>
#include <Interface.h>
#include <GUIConfiguration.h>
#include <SynchronizeController.h>

#ifdef LOCALINTERFACE
#include <LocalInterface.h>
#endif

class TStoredSessionList;
class TConfiguration;
class TTerminal;

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
  unsigned int Timeout;
  unsigned int TimeoutAnswer;
  AnsiString NewerAskAgainTitle;
  int NewerAskAgainAnswer;

private:
  inline void Reset();
};

class TCustomScpExplorerForm;
TCustomScpExplorerForm * __fastcall CreateScpExplorer();

void __fastcall ConfigureInterface();

void __fastcall DoProductLicence();

extern const AnsiString AppName;
extern const AnsiString AppNameVersion;

void __fastcall SetOnForeground(bool OnForeground);
void __fastcall FlashOnBackground();

void __fastcall ShowExtendedExceptionEx(TTerminal * Terminal, Exception * E);
void __fastcall FormHelp(TForm * Form);

AnsiString __fastcall GetToolbarsLayoutStr(const TComponent * OwnerComponent);
void __fastcall LoadToolbarsLayoutStr(const TComponent * OwnerComponent, AnsiString LayoutStr);

namespace Tb2item { class TTBCustomItem; }
void __fastcall AddMenuSeparator(Tb2item::TTBCustomItem * Menu);

// windows\WinHelp.cpp
void __fastcall InitializeWinHelp();
void __fastcall FinalizeWinHelp();

// windows\WinInterface.cpp
int __fastcall MessageDialog(const AnsiString Msg, TQueryType Type,
  int Answers, AnsiString HelpKeyword = HELP_NONE, const TMessageParams * Params = NULL);
int __fastcall MessageDialog(int Ident, TQueryType Type,
  int Answers, AnsiString HelpKeyword = HELP_NONE, const TMessageParams * Params = NULL);
int __fastcall SimpleErrorDialog(const AnsiString Msg, const AnsiString MoreMessages = "");

int __fastcall MoreMessageDialog(const AnsiString Message,
  TStrings * MoreMessages, TQueryType Type, int Answers,
    AnsiString HelpKeyword, const TMessageParams * Params = NULL);

int __fastcall ExceptionMessageDialog(Exception * E, TQueryType Type,
  const AnsiString MessageFormat = "", int Answers = qaOK,
  AnsiString HelpKeyword = HELP_NONE, const TMessageParams * Params = NULL);
int __fastcall FatalExceptionMessageDialog(Exception * E, TQueryType Type,
  const AnsiString MessageFormat = "", int Answers = qaOK,
  AnsiString HelpKeyword = HELP_NONE, const TMessageParams * Params = NULL);

// windows\WinMain.cpp
int __fastcall Execute();

// forms\InputDlg.cpp
bool __fastcall InputDialog(const AnsiString ACaption,
  const AnsiString APrompt, AnsiString & Value, AnsiString HelpKeyword = HELP_NONE,
  TStrings * History = NULL, bool PathInput = false);

// forms\About.cpp
struct TRegistration
{
  bool Registered;
  AnsiString Subject;
  int Licences;
  AnsiString ProductId;
  bool NeverExpires;
  TDateTime Expiration;
  bool EduLicense;
};
void __fastcall DoAboutDialog(TConfiguration * Configuration,
  bool AllowLicence, TRegistration * Registration);
void __fastcall DoAboutDialog(TConfiguration *Configuration);

// forms\Cleanup.cpp
bool __fastcall DoCleanupDialog(TStoredSessionList *SessionList,
    TConfiguration *Configuration);

// forms\Console.cpp
void __fastcall DoConsoleDialog(TTerminal * Terminal,
    const AnsiString Command = "", const TStrings * Log = NULL);

// forms\Copy.cpp
const coTemp                = 0x01;
const coDisableQueue        = 0x02;
const coDisableDirectory    = 0x08; // not used anymore
const coDisableNewerOnly    = 0x10;
const coDoNotShowAgain      = 0x20;
const coDisableSaveSettings = 0x40; // not used anymore
const coDoNotUsePresets     = 0x80;
const coAllowRemoteTransfer = 0x100;
const cooDoNotShowAgain     = 0x01;
const cooRemoteTransfer     = 0x02;
const cooSaveSettings       = 0x04;
bool __fastcall DoCopyDialog(bool ToRemote,
  bool Move, TStrings * FileList, AnsiString & TargetDirectory,
  TGUICopyParamType * Params, int Options, int CopyParamAttrs,
  int * OutputOptions);

// forms\CopyParams.cpp
enum TParamsForDirection { pdBoth, pdToRemote, pdToLocal, pdAll };

// forms\CreateDirectory.cpp
bool __fastcall DoCreateDirectoryDialog(AnsiString & Directory,
  TRemoteProperties * Properties, bool & SaveSettings);

// forms\ImportSessions.cpp
bool __fastcall DoImportSessionsDialog(TStoredSessionList *SessionList);

// forms\Licence.cpp
enum TLicence { lcNoLicence = -1, lcWinScp, lcPutty };
void __fastcall DoLicenceDialog(TLicence Licence);
void __fastcall DoLicenceDialog(const AnsiString LicenceText);

// forms\Login.cpp
// these flags are used in navigation tree of login dialog, change with care
const loLocalDirectory = 0x01;
const loLanguage       = 0x02;
const loTools          = 0x04;
const loLogWindow      = 0x08;
const loAbout          = 0x10;

const loPreferences    = 0x20;
const loExternalProtocols = 0x40;
const loColor          = 0x80;

const loNone           = 0x00;
const loAddSession     = (loLocalDirectory | loLogWindow | loColor);
const loStartup        = (loLocalDirectory | loLanguage | loTools |
  loLogWindow | loPreferences | loAbout | loColor);
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
    pmQueue, pmTransfer, pmLogging, pmUpdates, pmPresets, pmEditors };
typedef void __fastcall (__closure *TGetDefaultLogFileName)
  (System::TObject* Sender, AnsiString &DefaultLogFileName);
class TCopyParamRuleData;
struct TPreferencesDialogData
{
  TCopyParamRuleData * CopyParamRuleData;
};
bool __fastcall DoPreferencesDialog(TPreferencesMode APreferencesMode,
  TPreferencesDialogData * DialogData = NULL);

// forms\CustomCommand.cpp
class TCustomCommands;
enum TCustomCommandsMode { ccmAdd, ccmEdit, ccmAdHoc };
const ccoDisableRemote = 0x01;
typedef void __fastcall (__closure *TCustomCommandValidate)
  (const AnsiString & Command, int Params);
bool __fastcall DoCustomCommandDialog(AnsiString & Description,
  AnsiString & Command, int & Params, const TCustomCommands * CustomCommands,
  TCustomCommandsMode Mode, int Options, TCustomCommandValidate OnValidate);

// forms\CopyParamPreset.cpp
class TCopyParamList;
enum TCopyParamPresetMode { cpmAdd, cpmEdit, cpmDuplicate };
bool __fastcall DoCopyParamPresetDialog(TCopyParamList * CopyParamList,
  int & Index, TCopyParamPresetMode Mode, TCopyParamRuleData * CurrentRuleData);

// forms\CopyParamCsutom.cpp
bool __fastcall DoCopyParamCustomDialog(TCopyParamType & CopyParam,
  int CopyParamAttrs);

// forms\Properties.cpp
class TRemoteProperties;
struct TCalculateSizeStats;
const cpMode =  0x01;
const cpOwner = 0x02;
const cpGroup = 0x04;
typedef void __fastcall (__closure *TCalculateSizeEvent)
  (TStrings * FileList, __int64 & Size, TCalculateSizeStats & Stats,
   bool & Close);
typedef void __fastcall (__closure *TCalculatedChecksumCallbackEvent)(
  const AnsiString & FileName, const AnsiString & Alg, const AnsiString & Hash);
typedef void __fastcall (__closure *TCalculateChecksumEvent)
  (const AnsiString & Alg, TStrings * FileList,
   TCalculatedChecksumCallbackEvent OnCalculatedChecksum, bool & Close);
bool __fastcall DoPropertiesDialog(TStrings * FileList,
    const AnsiString Directory, TStrings * GroupList, TStrings * UserList,
    TRemoteProperties * Properties, int AllowedChanges,
    TCalculateSizeEvent OnCalculateSize,
    TCalculateChecksumEvent OnCalculateChecksum);

// forms\ComboInput.cpp
typedef void __fastcall (*TInputValidateEvent)
  (TObject * Sender, const AnsiString & Text);
bool __fastcall DoComboInputDialog(
  const AnsiString Caption, const AnsiString Prompt, AnsiString & Text,
  TStrings * Items, TInputValidateEvent OnInputValidate, bool AllowEmpty,
  const AnsiString HelpKeyword = HELP_NONE);
AnsiString __fastcall DoSaveSessionDialog(const AnsiString DefaultName,
  TSessionData * OriginalSession);
void __fastcall SessionNameValidate(const AnsiString & Text,
  TSessionData * RenamingSession = NULL);
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
const spPreviewChanges = 0x40; // not used by core
const spTimestamp = 0x100;
const spNotByTime = 0x200;
const spBySize = 0x400;
const spSelectedOnly = 0x800;
const spMirror = 0x1000;

// forms\Synchronize.cpp
const soDoNotUsePresets =  0x01;
const soNoMinimize =       0x02;
const soAllowSelectedOnly = 0x04;
typedef void __fastcall (__closure *TGetSynchronizeOptionsEvent)
  (int Params, TSynchronizeOptions & Options);
bool __fastcall DoSynchronizeDialog(TSynchronizeParamType & Params,
  const TCopyParamType * CopyParams, TSynchronizeStartStopEvent OnStartStop,
  bool & SaveSettings, int Options, int CopyParamAttrs,
  TGetSynchronizeOptionsEvent OnGetOptions, bool Start);

// forms\FullSynchronize.cpp
struct TUsableCopyParamAttrs;
enum TSynchronizeMode { smRemote, smLocal, smBoth };
const fsoDisableTimestamp = 0x01;
const fsoDoNotUsePresets =  0x02;
const fsoAllowSelectedOnly = 0x04;
bool __fastcall DoFullSynchronizeDialog(TSynchronizeMode & Mode, int & Params,
  AnsiString & LocalDirectory, AnsiString & RemoteDirectory,
  TCopyParamType * CopyParams, bool & SaveSettings, bool & SaveMode,
  int Options, const TUsableCopyParamAttrs & CopyParamAttrs);

// forms\SynchronizeChecklist.cpp
class TSynchronizeChecklist;
bool __fastcall DoSynchronizeChecklistDialog(TSynchronizeChecklist * Checklist,
  TSynchronizeMode Mode, int Params,
  const AnsiString LocalDirectory, const AnsiString RemoteDirectory);

// forms\Editor.cpp
TForm * __fastcall ShowEditorForm(const AnsiString FileName, TCustomForm * ParentForm,
  TNotifyEvent OnFileChanged, TNotifyEvent OnFileReload, TNotifyEvent OnClose,
  const AnsiString Caption = "", bool ShowWindowButton = false);
void __fastcall ReconfigureEditorForm(TForm * Form);

bool __fastcall DoSymlinkDialog(AnsiString & FileName, AnsiString & PointTo,
  TOperationSide Side, bool & SymbolicLink, bool Edit, bool AllowSymbolic);

// forms\FileSystemInfo.cpp
struct TSpaceAvailable;
struct TFileSystemInfo;
struct TSessionInfo;
typedef void __fastcall (__closure *TGetSpaceAvailable)
  (const AnsiString Path, TSpaceAvailable & ASpaceAvailable, bool & Close);
void __fastcall DoFileSystemInfoDialog(
  const TSessionInfo & SessionInfo, const TFileSystemInfo & FileSystemInfo,
  AnsiString SpaceAvailablePath, TGetSpaceAvailable OnGetSpaceAvailable);

// forms\MessageDlg.cpp
TForm * __fastcall CreateMoreMessageDialog(const AnsiString & Msg,
  TStrings * MoreMessages, TMsgDlgType DlgType, TMsgDlgButtons Buttons,
  TQueryButtonAlias * Aliases = NULL, unsigned int AliasesCount = 0,
  TMsgDlgBtn TimeoutResult = mbHelp, TButton ** TimeoutButton = NULL);

// windows\Console.cpp
int __fastcall Console(bool Help);

// windows\EditorPreferences.cpp
enum TEditorPreferencesMode { epmAdd, epmEdit };
class TEditorPreferences;
bool __fastcall DoEditorPreferencesDialog(TEditorPreferences * Editor,
  TEditorPreferencesMode Mode);

const int cplNone =             0x00;
const int cplCustomize =        0x01;
const int cplCustomizeDefault = 0x02;
void __fastcall CopyParamListPopup(TPoint P, TPopupMenu * Menu,
  const TCopyParamType & Param, AnsiString Preset, TNotifyEvent OnClick,
  int Options);
bool __fastcall CopyParamListPopupClick(TObject * Sender,
  TCopyParamType & Param, AnsiString & Preset, int CopyParamAttrs);

void __fastcall MenuPopup(TPopupMenu * Menu, TPoint Point, TComponent * PopupComponent);
void __fastcall MenuPopup(TPopupMenu * Menu, TButtonControl * Button);
void __fastcall MenuPopup(TObject * Sender, const TPoint & MousePos, bool & Handled);

void __fastcall UpgradeSpeedButton(TSpeedButton * Button);

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
struct TNotifyIconData5;
//---------------------------------------------------------------------------
class TTrayIcon
{
public:
  __fastcall TTrayIcon(unsigned int Id);
  __fastcall ~TTrayIcon();

  static bool __fastcall SupportsBalloons();

  void __fastcall PopupBalloon(const AnsiString & Title, const AnsiString & Str,
    TQueryType QueryType, unsigned int Timeout);
  void __fastcall CancelBalloon();

  __property bool Visible = { read = FVisible, write = SetVisible };
  __property TNotifyEvent OnClick = { read = FOnClick, write = FOnClick };
  __property AnsiString Hint = { read = GetHint, write = SetHint };

protected:
  void __fastcall Update();
  bool __fastcall Notify(unsigned int Message);

private:
  bool FVisible;
  TNotifyIconData5 * FTrayIcon;
  TNotifyEvent FOnClick;

  void __fastcall WndProc(TMessage & Message);
  void __fastcall SetVisible(bool value);
  AnsiString __fastcall GetHint();
  void __fastcall SetHint(AnsiString value);
};
//---------------------------------------------------------------------------
#endif // WinInterfaceH
