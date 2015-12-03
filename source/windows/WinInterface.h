//---------------------------------------------------------------------------
#ifndef WinInterfaceH
#define WinInterfaceH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Buttons.hpp>
#include <Interface.h>
#include <WinConfiguration.h>
#include <SynchronizeController.h>

#ifdef LOCALINTERFACE
#include <LocalInterface.h>
#endif

#define SITE_ICON 1
#define SITE_FOLDER_ICON 2
#define WORKSPACE_ICON 3

class TStoredSessionList;
class TConfiguration;
class TTerminal;

const int mpNeverAskAgainCheck =   0x01;
const int mpAllowContinueOnError = 0x02;

#define UPLOAD_IF_ANY_SWITCH L"UploadIfAny"
#define UPLOAD_SWITCH L"Upload"
#define JUMPLIST_SWITCH L"JumpList"
#define DESKTOP_SWITCH L"Desktop"
#define SEND_TO_HOOK_SWITCH L"SendToHook"
#define UNSAFE_SWITCH L"Unsafe"
#define NEWINSTANCE_SWICH L"NewInstance"
#define KEYGEN_SWITCH L"KeyGen"
#define KEYGEN_OUTPUT_SWITCH L"Output"
#define KEYGEN_COMMENT_SWITCH L"Comment"
#define KEYGEN_CHANGE_PASSPHRASE_SWITCH L"ChangePassphrase"

struct TMessageParams
{
  TMessageParams(unsigned int AParams = 0);
  TMessageParams(const TQueryParams * AParams);

  const TQueryButtonAlias * Aliases;
  unsigned int AliasesCount;
  unsigned int Params;
  unsigned int Timer;
  TQueryParamsTimerEvent TimerEvent;
  UnicodeString TimerMessage;
  unsigned int TimerAnswers;
  TQueryType TimerQueryType;
  unsigned int Timeout;
  unsigned int TimeoutAnswer;
  UnicodeString NeverAskAgainTitle;
  unsigned int NeverAskAgainAnswer;
  bool NeverAskAgainCheckedInitially;
  bool AllowHelp;
  UnicodeString ImageName;
  UnicodeString MoreMessagesUrl;
  TSize MoreMessagesSize;
  UnicodeString CustomCaption;

private:
  inline void Reset();
};

class TCustomScpExplorerForm;
TCustomScpExplorerForm * __fastcall CreateScpExplorer();

void __fastcall ConfigureInterface();

void __fastcall DoProductLicense();

extern const UnicodeString AppName;

void __fastcall SetOnForeground(bool OnForeground);
void __fastcall FlashOnBackground();

void __fastcall ShowExtendedExceptionEx(TTerminal * Terminal, Exception * E);
void __fastcall FormHelp(TCustomForm * Form);
void __fastcall SearchHelp(const UnicodeString & Message);
void __fastcall MessageWithNoHelp(const UnicodeString & Message);

class TProgramParams;
bool __fastcall CheckSafe(TProgramParams * Params);
bool __fastcall CheckXmlLogParam(TProgramParams * Params);

UnicodeString __fastcall GetToolbarsLayoutStr(TComponent * OwnerComponent);
void __fastcall LoadToolbarsLayoutStr(TComponent * OwnerComponent, UnicodeString LayoutStr);

namespace Tb2item { class TTBCustomItem; }
void __fastcall AddMenuSeparator(Tb2item::TTBCustomItem * Menu);
void __fastcall AddMenuLabel(Tb2item::TTBCustomItem * Menu, const UnicodeString & Label);

// windows\WinHelp.cpp
void __fastcall InitializeWinHelp();
void __fastcall FinalizeWinHelp();

// windows\WinInterface.cpp
unsigned int __fastcall MessageDialog(const UnicodeString Msg, TQueryType Type,
  unsigned int Answers, UnicodeString HelpKeyword = HELP_NONE, const TMessageParams * Params = NULL);
unsigned int __fastcall MessageDialog(int Ident, TQueryType Type,
  unsigned int Answers, UnicodeString HelpKeyword = HELP_NONE, const TMessageParams * Params = NULL);
unsigned int __fastcall SimpleErrorDialog(const UnicodeString Msg, const UnicodeString MoreMessages = L"");

unsigned int __fastcall MoreMessageDialog(const UnicodeString Message,
  TStrings * MoreMessages, TQueryType Type, unsigned int Answers,
    UnicodeString HelpKeyword, const TMessageParams * Params = NULL);

unsigned int __fastcall ExceptionMessageDialog(Exception * E, TQueryType Type,
  const UnicodeString MessageFormat = L"", unsigned int Answers = qaOK,
  UnicodeString HelpKeyword = HELP_NONE, const TMessageParams * Params = NULL);
unsigned int __fastcall FatalExceptionMessageDialog(Exception * E, TQueryType Type,
  int SessionReopenTimeout, const UnicodeString MessageFormat = L"", unsigned int Answers = qaOK,
  UnicodeString HelpKeyword = HELP_NONE, const TMessageParams * Params = NULL);

// forms\Custom.cpp
TSessionData * __fastcall DoSaveSession(TSessionData * SessionData,
  TSessionData * OriginalSession, bool ForceDialog,
  TStrings * AdditionalFolders);
void __fastcall SessionNameValidate(const UnicodeString & Text,
  const UnicodeString & OriginalName);
bool __fastcall DoSaveWorkspaceDialog(UnicodeString & WorkspaceName,
  bool * SavePasswords, bool NotRecommendedSavingPasswords,
  bool & CreateShortcut, bool & EnableAutoSave);
class TShortCuts;
bool __fastcall DoShortCutDialog(TShortCut & ShortCut,
  const TShortCuts & ShortCuts, UnicodeString HelpKeyword);

// windows\UserInterface.cpp
bool __fastcall DoMasterPasswordDialog();
bool __fastcall DoChangeMasterPasswordDialog(UnicodeString & NewPassword);

// windows\WinMain.cpp
int __fastcall Execute();
void __fastcall GetLoginData(UnicodeString SessionName, TOptions * Options,
  TObjectList * DataList, UnicodeString & DownloadFile, bool NeedSession);

// forms\InputDlg.cpp
struct TInputDialogData
{
  TCustomEdit * Edit;
};
typedef void __fastcall (__closure *TInputDialogInitialize)
  (TObject * Sender, TInputDialogData * Data);
bool __fastcall InputDialog(const UnicodeString ACaption,
  const UnicodeString APrompt, UnicodeString & Value, UnicodeString HelpKeyword = HELP_NONE,
  TStrings * History = NULL, bool PathInput = false,
  TInputDialogInitialize OnInitialize = NULL, bool Echo = true);

// forms\About.cpp
struct TRegistration
{
  bool Registered;
  UnicodeString Subject;
  int Licenses;
  UnicodeString ProductId;
  bool NeverExpires;
  TDateTime Expiration;
  bool EduLicense;
  TNotifyEvent OnRegistrationLink;
};
void __fastcall DoAboutDialog(TConfiguration * Configuration,
  bool AllowLicense, TRegistration * Registration);
void __fastcall DoAboutDialog(TConfiguration *Configuration);

// forms\Cleanup.cpp
bool __fastcall DoCleanupDialog(TStoredSessionList *SessionList,
    TConfiguration *Configuration);

// forms\Console.cpp
void __fastcall DoConsoleDialog(TTerminal * Terminal,
    const UnicodeString Command = L"", const TStrings * Log = NULL);

// forms\Copy.cpp
const coTemp                = 0x001;
const coDisableQueue        = 0x002;
const coDisableDirectory    = 0x008; // not used anymore
const coDoNotShowAgain      = 0x020;
const coDisableSaveSettings = 0x040; // not used anymore
const coDoNotUsePresets     = 0x080;
const coAllowRemoteTransfer = 0x100;
const coNoQueue             = 0x200;
const coNoQueueIndividually = 0x400;
const coShortCutHint        = 0x800;
const cooDoNotShowAgain     = 0x01;
const cooRemoteTransfer     = 0x02;
const cooSaveSettings       = 0x04;
bool __fastcall DoCopyDialog(bool ToRemote,
  bool Move, TStrings * FileList, UnicodeString & TargetDirectory,
  TGUICopyParamType * Params, int Options, int CopyParamAttrs,
  int * OutputOptions);

// forms\CreateDirectory.cpp
bool __fastcall DoCreateDirectoryDialog(UnicodeString & Directory,
  TRemoteProperties * Properties, int AllowedChanges, bool & SaveSettings);

// forms\ImportSessions.cpp
bool __fastcall DoImportSessionsDialog(TList * Imported);

// forms\License.cpp
enum TLicense { lcNoLicense = -1, lcWinScp, lcExpat };
void __fastcall DoLicenseDialog(TLicense License);

// forms\Login.cpp
// these flags are used in navigation tree of login dialog, change with care
const loLocalDirectory = 0x01;
const loExternalTools  = 0x04;

const loColor          = 0x80;

const loNone           = 0x00;
const loAddSession     = (loLocalDirectory | loColor);
const loStartup        = (loLocalDirectory | loExternalTools | loColor);
bool __fastcall DoLoginDialog(TStoredSessionList * SessionList,
  TList * DataList, int Options);

  // forms\SiteAdvanced.cpp
bool __fastcall DoSiteAdvancedDialog(TSessionData * SessionData, int Options);

// forms\OpenDirectory.cpp
enum TOpenDirectoryMode { odBrowse, odAddBookmark };
bool __fastcall DoOpenDirectoryDialog(TOpenDirectoryMode Mode, TOperationSide Side,
  UnicodeString & Directory, TStrings * Directories, TTerminal * Terminal,
  bool AllowSwitch);

// forms\LocatinoProfiles.cpp
bool __fastcall LocationProfilesDialog(TOpenDirectoryMode Mode,
  TOperationSide Side, UnicodeString & LocalDirectory, UnicodeString & RemoteDirectory,
  TStrings * LocalDirectories, TStrings * RemoteDirectories, TTerminal * Terminal);

// forms\Preferences.cpp
enum TPreferencesMode { pmDefault, pmEditor, pmCustomCommands,
    pmQueue, pmLogging, pmUpdates, pmPresets, pmEditors, pmCommander,
    pmEditorInternal };
class TCopyParamRuleData;
struct TPreferencesDialogData
{
  TCopyParamRuleData * CopyParamRuleData;
};
bool __fastcall DoPreferencesDialog(TPreferencesMode APreferencesMode,
  TPreferencesDialogData * DialogData = NULL);

// forms\CustomCommand.cpp
class TCustomCommandList;
class TCustomCommandType;
class TShortCuts;
enum TCustomCommandsMode { ccmAdd, ccmEdit, ccmAdHoc };
const ccoDisableRemote = 0x01;
typedef void __fastcall (__closure *TCustomCommandValidate)
  (const TCustomCommandType & Command);
bool __fastcall DoCustomCommandDialog(TCustomCommandType & Command,
  const TCustomCommandList * CustomCommandList,
  TCustomCommandsMode Mode, int Options, TCustomCommandValidate OnValidate,
  const TShortCuts * ShortCuts);

// forms\CopyParamPreset.cpp
class TCopyParamList;
enum TCopyParamPresetMode { cpmAdd, cpmAddCurrent, cpmEdit, cpmDuplicate };
bool __fastcall DoCopyParamPresetDialog(TCopyParamList * CopyParamList,
  int & Index, TCopyParamPresetMode Mode, TCopyParamRuleData * CurrentRuleData,
  const TCopyParamType & DefaultCopyParams);

// forms\CopyParamCsutom.cpp
bool __fastcall DoCopyParamCustomDialog(TCopyParamType & CopyParam,
  int CopyParamAttrs);

// forms\Properties.cpp
class TRemoteProperties;
class TRemoteTokenList;
struct TCalculateSizeStats;
const cpMode =  0x01;
const cpOwner = 0x02;
const cpGroup = 0x04;
typedef void __fastcall (__closure *TCalculateSizeEvent)
  (TStrings * FileList, __int64 & Size, TCalculateSizeStats & Stats,
   bool & Close);
typedef void __fastcall (__closure *TCalculatedChecksumCallbackEvent)(
  const UnicodeString & FileName, const UnicodeString & Alg, const UnicodeString & Hash);
typedef void __fastcall (__closure *TCalculateChecksumEvent)
  (const UnicodeString & Alg, TStrings * FileList,
   TCalculatedChecksumCallbackEvent OnCalculatedChecksum, bool & Close);
bool __fastcall DoPropertiesDialog(TStrings * FileList,
    const UnicodeString Directory, const TRemoteTokenList * GroupList,
    const TRemoteTokenList * UserList, TStrings * ChecksumAlgs,
    TRemoteProperties * Properties,
    int AllowedChanges, bool UserGroupByID, TCalculateSizeEvent OnCalculateSize,
    TCalculateChecksumEvent OnCalculateChecksum);

bool __fastcall DoRemoteMoveDialog(bool Multi, UnicodeString & Target, UnicodeString & FileMask);
enum TDirectRemoteCopy { drcDisallow, drcAllow, drcConfirmCommandSession };
bool __fastcall DoRemoteCopyDialog(TStrings * Sessions, TStrings * Directories,
  TDirectRemoteCopy AllowDirectCopy, bool Multi, void *& Session,
  UnicodeString & Target, UnicodeString & FileMask, bool & DirectCopy);

// forms\SelectMask.cpp
#ifdef CustomdirviewHPP
bool __fastcall DoSelectMaskDialog(TCustomDirView * Parent, bool Select,
    TFileFilter * Filter, TConfiguration * Configuration);
bool __fastcall DoFilterMaskDialog(TCustomDirView * Parent,
  TFileFilter * Filter);
#endif

// forms\EditMask.cpp
bool __fastcall DoEditMaskDialog(TFileMasks & Mask);

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
typedef void __fastcall (__closure *TFeedSynchronizeError)
  (const UnicodeString & Message, TStrings * MoreMessages, TQueryType Type,
   const UnicodeString & HelpKeyword);
bool __fastcall DoSynchronizeDialog(TSynchronizeParamType & Params,
  const TCopyParamType * CopyParams, TSynchronizeStartStopEvent OnStartStop,
  bool & SaveSettings, int Options, int CopyParamAttrs,
  TGetSynchronizeOptionsEvent OnGetOptions,
  TFeedSynchronizeError & OnFeedSynchronizeError,
  bool Start);

// forms\FullSynchronize.cpp
struct TUsableCopyParamAttrs;
enum TSynchronizeMode { smRemote, smLocal, smBoth };
const fsoDisableTimestamp = 0x01;
const fsoDoNotUsePresets =  0x02;
const fsoAllowSelectedOnly = 0x04;
bool __fastcall DoFullSynchronizeDialog(TSynchronizeMode & Mode, int & Params,
  UnicodeString & LocalDirectory, UnicodeString & RemoteDirectory,
  TCopyParamType * CopyParams, bool & SaveSettings, bool & SaveMode,
  int Options, const TUsableCopyParamAttrs & CopyParamAttrs);

// forms\SynchronizeChecklist.cpp
class TSynchronizeChecklist;
typedef void __fastcall (__closure *TCustomCommandMenuEvent)
  (TAction * Action, TStrings * LocalFileList, TStrings * RemoteFileList);
bool __fastcall DoSynchronizeChecklistDialog(TSynchronizeChecklist * Checklist,
  TSynchronizeMode Mode, int Params,
  const UnicodeString LocalDirectory, const UnicodeString RemoteDirectory,
  TCustomCommandMenuEvent OnCustomCommandMenu);

// forms\Editor.cpp
typedef void __fastcall (__closure *TFileClosedEvent)
  (TObject * Sender, bool Forced);
typedef void __fastcall (__closure *TAnyModifiedEvent)
  (TObject * Sender, bool & Modified);
TForm * __fastcall ShowEditorForm(const UnicodeString FileName, TCustomForm * ParentForm,
  TNotifyEvent OnFileChanged, TNotifyEvent OnFileReload, TFileClosedEvent OnClose,
  TNotifyEvent OnSaveAll, TAnyModifiedEvent OnAnyModified,
  const UnicodeString Caption, bool StandaloneEditor, TColor Color);
void __fastcall ReconfigureEditorForm(TForm * Form);
void __fastcall EditorFormFileUploadComplete(TForm * Form);
void __fastcall EditorFormFileSave(TForm * Form);
bool __fastcall IsEditorFormModified(TForm * Form);

bool __fastcall DoSymlinkDialog(UnicodeString & FileName, UnicodeString & PointTo,
  TOperationSide Side, bool & SymbolicLink, bool Edit, bool AllowSymbolic);

// forms\FileSystemInfo.cpp
struct TSpaceAvailable;
struct TFileSystemInfo;
struct TSessionInfo;
typedef void __fastcall (__closure *TGetSpaceAvailable)
  (const UnicodeString Path, TSpaceAvailable & ASpaceAvailable, bool & Close);
void __fastcall DoFileSystemInfoDialog(
  const TSessionInfo & SessionInfo, const TFileSystemInfo & FileSystemInfo,
  UnicodeString SpaceAvailablePath, TGetSpaceAvailable OnGetSpaceAvailable);

// forms\MessageDlg.cpp
void __fastcall AnswerNameAndCaption(
  unsigned int Answer, UnicodeString & Name, UnicodeString & Caption);
TForm * __fastcall CreateMoreMessageDialog(const UnicodeString & Msg,
  TStrings * MoreMessages, TMsgDlgType DlgType, unsigned int Answers,
  const TQueryButtonAlias * Aliases, unsigned int AliasesCount,
  unsigned int TimeoutAnswer, TButton ** TimeoutButton,
  const UnicodeString & ImageName, const UnicodeString & NeverAskAgainCaption,
  const UnicodeString & MoreMessagesUrl, TSize MoreMessagesSize,
  const UnicodeString & CustomCaption);
TForm * __fastcall CreateMoreMessageDialogEx(const UnicodeString Message, TStrings * MoreMessages,
  TQueryType Type, unsigned int Answers, UnicodeString HelpKeyword, const TMessageParams * Params);
unsigned int __fastcall ExecuteMessageDialog(TForm * Dialog, unsigned int Answers, const TMessageParams * Params);
void __fastcall InsertPanelToMessageDialog(TCustomForm * Form, TPanel * Panel);
void __fastcall NavigateMessageDialogToUrl(TCustomForm * Form, const UnicodeString & Url);

// windows\Console.cpp
enum TConsoleMode { cmNone, cmScripting, cmHelp, cmBatchSettings, cmKeyGen };
int __fastcall Console(TConsoleMode Mode);

// forms\EditorPreferences.cpp
enum TEditorPreferencesMode { epmAdd, epmEdit, epmAdHoc };
class TEditorData;
bool __fastcall DoEditorPreferencesDialog(TEditorData * Editor,
  bool & Remember, TEditorPreferencesMode Mode, bool MayRemote);

// forms\Find.cpp
typedef void __fastcall (__closure *TFindEvent)
  (UnicodeString Directory, const TFileMasks & FileMask,
   TFileFoundEvent OnFileFound, TFindingFileEvent OnFindingFile);
bool __fastcall DoFileFindDialog(UnicodeString Directory,
  TFindEvent OnFind, UnicodeString & Path);

// forms\GenerateUrl.cpp
void __fastcall DoGenerateUrlDialog(TSessionData * Data, TStrings * Paths);

void __fastcall CopyParamListButton(TButton * Button);
const int cplNone =             0x00;
const int cplCustomize =        0x01;
const int cplCustomizeDefault = 0x02;
const int cplSaveSettings =     0x04;
void __fastcall CopyParamListPopup(TRect R, TPopupMenu * Menu,
  const TCopyParamType & Param, UnicodeString Preset, TNotifyEvent OnClick,
  int Options, int CopyParamAttrs, bool SaveSettings = false);
bool __fastcall CopyParamListPopupClick(TObject * Sender,
  TCopyParamType & Param, UnicodeString & Preset, int CopyParamAttrs,
  bool * SaveSettings = NULL);

void __fastcall MenuPopup(TPopupMenu * Menu, TRect Rect, TComponent * PopupComponent);
void __fastcall MenuPopup(TPopupMenu * Menu, TButton * Button);
void __fastcall MenuPopup(TObject * Sender, const TPoint & MousePos, bool & Handled);
void __fastcall MenuButton(TButton * Button);
TComponent * __fastcall GetPopupComponent(TObject * Sender);
TRect __fastcall CalculatePopupRect(TButton * Button);
TRect __fastcall CalculatePopupRect(TControl * Control, TPoint MousePos);

typedef void __fastcall (__closure *TColorChangeEvent)
  (TColor Color);
TPopupMenu * __fastcall CreateSessionColorPopupMenu(TColor Color,
  TColorChangeEvent OnColorChange);
void __fastcall CreateSessionColorMenu(TComponent * AOwner, TColor Color,
  TColorChangeEvent OnColorChange);
void __fastcall CreateEditorBackgroundColorMenu(TComponent * AOwner, TColor Color,
  TColorChangeEvent OnColorChange);
TPopupMenu * __fastcall CreateColorPopupMenu(TColor Color,
  TColorChangeEvent OnColorChange);

void __fastcall FixButtonImage(TButton * Button);
void __fastcall CenterButtonImage(TButton * Button);

void __fastcall UpgradeSpeedButton(TSpeedButton * Button);

int __fastcall AdjustLocaleFlag(const UnicodeString & S, TLocaleFlagOverride LocaleFlagOverride, bool Recommended, int On, int Off);

void __fastcall SetGlobalMinimizeHandler(TCustomForm * Form, TNotifyEvent OnMinimize);
void __fastcall ClearGlobalMinimizeHandler(TNotifyEvent OnMinimize);
void __fastcall CallGlobalMinimizeHandler(TObject * Sender);
bool __fastcall IsApplicationMinimized();
void __fastcall ApplicationMinimize();
void __fastcall ApplicationRestore();
bool __fastcall HandleMinimizeSysCommand(TMessage & Message);

void __fastcall WinInitialize();
void __fastcall WinFinalize();

void __fastcall ShowNotification(TTerminal * Terminal, const UnicodeString & Str,
  TQueryType Type);

void __fastcall InitializeShortCutCombo(TComboBox * ComboBox,
  const TShortCuts & ShortCuts);
void __fastcall SetShortCutCombo(TComboBox * ComboBox, TShortCut Value);
TShortCut __fastcall GetShortCutCombo(TComboBox * ComboBox);
bool __fastcall IsCustomShortCut(TShortCut ShortCut);

#ifdef _DEBUG
void __fastcall ForceTracing();
#endif
//---------------------------------------------------------------------------
#define HIDDEN_WINDOW_NAME L"WinSCPHiddenWindow2"
//---------------------------------------------------------------------------
struct TCopyDataMessage
{
  enum { CommandCanCommandLine, CommandCommandLine };
  static const unsigned int Version1 = 1;

  unsigned int Version;
  unsigned int Command;

  union
  {
    wchar_t CommandLine[10240];
  };
};
//---------------------------------------------------------------------------
class TWinInteractiveCustomCommand : public TInteractiveCustomCommand
{
public:
  TWinInteractiveCustomCommand(TCustomCommand * ChildCustomCommand,
    const UnicodeString CustomCommandName);

protected:
  virtual void __fastcall Prompt(const UnicodeString & Prompt,
    UnicodeString & Value);
  virtual void __fastcall Execute(const UnicodeString & Command,
    UnicodeString & Value);

private:
  UnicodeString FCustomCommandName;
};
//---------------------------------------------------------------------------
class TTrayIcon
{
public:
  __fastcall TTrayIcon(unsigned int Id);
  __fastcall ~TTrayIcon();

  void __fastcall PopupBalloon(UnicodeString Title, const UnicodeString & Str,
    TQueryType QueryType, unsigned int Timeout, TNotifyEvent OnBalloonClick,
    TObject * BalloonUserData);
  void __fastcall CancelBalloon();

  __property bool Visible = { read = FVisible, write = SetVisible };
  __property TNotifyEvent OnClick = { read = FOnClick, write = FOnClick };
  __property UnicodeString Hint = { read = GetHint, write = SetHint };

protected:
  void __fastcall Update();
  bool __fastcall Notify(unsigned int Message);

private:
  bool FVisible;
  NOTIFYICONDATA * FTrayIcon;
  TNotifyEvent FOnClick;
  TNotifyEvent FOnBalloonClick;
  TObject * FBalloonUserData;
  UINT FTaskbarCreatedMsg;

  void __fastcall WndProc(TMessage & Message);
  void __fastcall SetVisible(bool value);
  UnicodeString __fastcall GetHint();
  void __fastcall SetHint(UnicodeString value);
  void __fastcall BalloonCancelled();
};
//---------------------------------------------------------------------------
#endif // WinInterfaceH
