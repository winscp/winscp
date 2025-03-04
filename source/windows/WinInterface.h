//---------------------------------------------------------------------------
#ifndef WinInterfaceH
#define WinInterfaceH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Buttons.hpp>
#include <Interface.h>
#include <WinConfiguration.h>
#include <Terminal.h>
#include <SynchronizeController.h>
#include <Script.h>
#include "HistoryComboBox.hpp"

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
#define SYNCHRONIZE_SWITCH L"Synchronize"
#define KEEP_UP_TO_DATE_SWITCH L"KeepUpToDate"
#define JUMPLIST_SWITCH L"JumpList"
#define DESKTOP_SWITCH L"Desktop"
#define SEND_TO_HOOK_SWITCH L"SendToHook"
#define UNSAFE_SWITCH L"Unsafe"
#define DEFAULTS_SWITCH L"Defaults"
#define NEWINSTANCE_SWICH L"NewInstance"
#define KEYGEN_SWITCH L"KeyGen"
#define KEYGEN_OUTPUT_SWITCH L"Output"
#define KEYGEN_COMMENT_SWITCH L"Comment"
#define KEYGEN_CHANGE_PASSPHRASE_SWITCH L"ChangePassphrase"
#define KEYGEN_CERTIFICATE_SWITCH L"Certificate"
#define LOG_SWITCH L"Log"
#define LOGSIZE_SWITCH L"LogSize"
#define LOGSIZE_SEPARATOR L"*"
#define INI_SWITCH L"Ini"
#define RAW_CONFIG_SWITCH L"RawConfig"
#define FINGERPRINTSCAN_SWITCH L"FingerprintScan"
#define DUMPCALLSTACK_SWITCH L"DumpCallstack"
#define INFO_SWITCH L"Info"
#define COMREGISTRATION_SWITCH L"ComRegistration"
#define BROWSE_SWITCH L"Browse"
#define NOINTERACTIVEINPUT_SWITCH L"NoInteractiveInput"
#define STDOUT_SWITCH L"StdOut"
#define STDIN_SWITCH L"StdIn"
#define STDINOUT_BINARY_VALUE L"binary"
#define STDINOUT_CHUNKED_VALUE L"chunked"
#define COPYID_SWITCH L"CopyId"
#define IDENTITY_SWITCH L"Identity"

#define DUMPCALLSTACK_EVENT L"WinSCPCallstack%d"

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
  unsigned int TimeoutResponse;
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

UnicodeString GetThemeName(bool Dark);
void __fastcall ConfigureInterface();

void __fastcall DoProductLicense();

extern const UnicodeString AppName;

void __fastcall SetOnForeground(bool OnForeground);
void __fastcall FlashOnBackground();

void __fastcall TerminateApplication();
void __fastcall ShowExtendedExceptionEx(TTerminal * Terminal, Exception * E);
void __fastcall FormHelp(TCustomForm * Form);
void __fastcall SearchHelp(const UnicodeString & Message);
void __fastcall MessageWithNoHelp(const UnicodeString & Message);

class TProgramParams;
bool __fastcall CheckSafe(TProgramParams * Params);
void __fastcall CheckLogParam(TProgramParams * Params);
bool __fastcall CheckXmlLogParam(TProgramParams * Params);

UnicodeString __fastcall GetToolbarKey(const UnicodeString & ToolbarName);
UnicodeString __fastcall GetToolbarsLayoutStr(TControl * OwnerControl);
void __fastcall LoadToolbarsLayoutStr(TControl * OwnerControl, UnicodeString LayoutStr);

namespace Tb2item { class TTBCustomItem; }
namespace Tbx { class TTBXSeparatorItem; }
Tbx::TTBXSeparatorItem * __fastcall AddMenuSeparator(Tb2item::TTBCustomItem * Menu);
void __fastcall AddMenuLabel(Tb2item::TTBCustomItem * Menu, const UnicodeString & Label);
void __fastcall ClickToolbarItem(Tb2item::TTBCustomItem * Item, bool PositionCursor);

void InitiateDialogTimeout(TForm * Dialog, unsigned int Timeout, TButton * Button, unsigned int Answer = 0);

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
unsigned int __fastcall FatalExceptionMessageDialog(
  Exception * E, TQueryType Type, const UnicodeString & MessageFormat, unsigned int Answers,
  const UnicodeString & HelpKeyword, const TMessageParams * Params);

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
bool __fastcall DoCustomCommandOptionsDialog(
  const TCustomCommandType * Command, TStrings * CustomCommandOptions, TShortCut * ShortCut, unsigned int Flags,
  TCustomCommand * CustomCommandForOptions, const UnicodeString & Site, const TShortCuts * ShortCuts);
void __fastcall DoUsageStatisticsDialog();
void __fastcall DoSiteRawDialog(TSessionData * Data);
bool DoSshHostCADialog(bool Add, TSshHostCA & SshHostCA);
bool DoTagDialog(bool Add, TStrings * Tags, UnicodeString & Key, UnicodeString & Value);

// windows\UserInterface.cpp
bool __fastcall DoMasterPasswordDialog();
bool __fastcall DoChangeMasterPasswordDialog(UnicodeString & NewPassword);

// windows\WinMain.cpp
int __fastcall Execute();
void __fastcall GetLoginData(UnicodeString SessionName, TOptions * Options,
  TObjectList * DataList, UnicodeString & DownloadFile, bool NeedSession, TForm * LinkedForm, int Flags = 0);
int GetCommandLineParseUrlFlags(TProgramParams * Params);

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
  TInputDialogInitialize OnInitialize = NULL, bool Echo = true, int Width = 305);

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
bool __fastcall DoCleanupDialog();
void __fastcall DoCleanupDialogIfAnyDataAndWanted();

// forms\Console.cpp
void __fastcall DoConsoleDialog(TTerminal * Terminal,
    const UnicodeString Command = L"", const TStrings * Log = NULL);

// forms\Copy.cpp
const coTemp                = 0x001;
const coDisableQueue        = 0x002;
const coDoNotShowAgain      = 0x020;
const coAllowRemoteTransfer = 0x100;
const coShortCutHint        = 0x800;
const coAllFiles            = 0x1000;
const coBrowse              = 0x2000;
const cooDoNotShowAgain     = 0x01;
const cooRemoteTransfer     = 0x02;
const cooSaveSettings       = 0x04;
const cooBrowse             = 0x08;
bool __fastcall DoCopyDialog(
  bool ToRemote, bool Move, TStrings * FileList, UnicodeString & TargetDirectory,
  TGUICopyParamType * Params, int Options, int CopyParamAttrs,
  TSessionData * SessionData, int * OutputOptions, int AutoSubmit);
bool CopyDialogValidateLocalDirectory(const UnicodeString & Directory, THistoryComboBox * DirectoryEdit);
bool CopyDialogValidateFileMask(
  const UnicodeString & FileMask, THistoryComboBox * DirectoryEdit, bool MultipleFiles, bool RemotePaths);

// forms\CopyLocal.cpp
const cloShortCutHint = 0x01;
const cloMultipleFiles = 0x02;
const clooDoNotShowAgain = 0x01;
bool DoCopyLocalDialog(bool Move, int Options, UnicodeString & TargetDirectory, UnicodeString & FileMask, int & OutputOptions);

// forms\CreateDirectory.cpp
bool __fastcall DoCreateDirectoryDialog(UnicodeString & Directory,
  TRemoteProperties * Properties, int AllowedChanges, bool & SaveSettings);

// forms\ImportSessions.cpp
bool __fastcall DoImportSessionsDialog(TList * Imported);

// forms\License.cpp
enum TLicense { lcNoLicense = -1, lcWinScp, lcExpat };
void __fastcall DoLicenseDialog(TLicense License);

bool __fastcall DoLoginDialog(TList * DataList, TForm * LinkedForm);

  // forms\SiteAdvanced.cpp
bool __fastcall DoSiteAdvancedDialog(TSessionData * SessionData);

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
    pmEditorInternal, pmFileColors };
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
const ccoDisableRemoteFiles = 0x02;
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
const cpAcl =   0x08;
const poUserGroupByID = 0x01;
const poTags =          0x02;
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
    int AllowedChanges, int Options, TCalculateSizeEvent OnCalculateSize,
    TCalculateChecksumEvent OnCalculateChecksum);

typedef bool (__closure * TDirectoryExistsEvent)(void * Session, const UnicodeString & Directory);
bool __fastcall DoRemoteMoveDialog(
  bool Multi, UnicodeString & Target, UnicodeString & FileMask, TDirectoryExistsEvent OnDirectoryExists);
enum TDirectRemoteCopy { drcDisallow, drcAllow, drcConfirmCommandSession, drcConfirmCommandSessionDirs };
bool __fastcall DoRemoteCopyDialog(
  TStrings * Sessions, TStrings * Directories,
  TDirectRemoteCopy AllowDirectCopy, bool Multi, void *& Session, UnicodeString & Target, UnicodeString & FileMask,
  bool & DirectCopy, void * CurrentSession, TDirectoryExistsEvent OnDirectoryExists,
  bool TargetConfirmed);

// forms\SelectMask.cpp
bool __fastcall DoSelectMaskDialog(TControl * Parent, bool Select, TFileFilter & Filter);
bool __fastcall DoFilterMaskDialog(TControl * Parent, UnicodeString & Mask);
bool __fastcall DoFileColorDialog(TFileColorData & FileColorData);

// forms\EditMask.cpp
bool __fastcall DoEditMaskDialog(TFileMasks & Mask);

// forms\Synchronize.cpp
const soDoNotUsePresets =  0x01;
const soNoMinimize =       0x02;
const soAllowSelectedOnly = 0x04;
typedef void __fastcall (__closure *TGetSynchronizeOptionsEvent)
  (int Params, TSynchronizeOptions & Options);
typedef void __fastcall (__closure *TSynchronizeSessionLog)
  (const UnicodeString & Message);
typedef void __fastcall (__closure *TFeedSynchronizeError)
  (const UnicodeString & Message, TStrings * MoreMessages, TQueryType Type,
   const UnicodeString & HelpKeyword);
typedef void __fastcall (__closure *TSynchronizeInNewWindow)
  (const TSynchronizeParamType & Params, const TCopyParamType * CopyParams);
bool __fastcall DoSynchronizeDialog(TSynchronizeParamType & Params,
  const TCopyParamType * CopyParams, TSynchronizeStartStopEvent OnStartStop,
  bool & SaveSettings, int Options, int CopyParamAttrs,
  TGetSynchronizeOptionsEvent OnGetOptions,
  TSynchronizeSessionLog OnSynchronizeSessionLog,
  TFeedSynchronizeError & OnFeedSynchronizeError,
  TNotifyEvent & OnSynchronizeAbort,
  TSynchronizeInNewWindow OnSynchronizeInNewWindow,
  int AutoSubmit);

// forms\FullSynchronize.cpp
struct TUsableCopyParamAttrs;
enum TSynchronizeMode { smRemote, smLocal, smBoth };
const fsoDisableTimestamp = 0x01;
const fsoDoNotUsePresets =  0x02;
const fsoAllowSelectedOnly = 0x04;
const fsoDisableByChecksum = 0x08;
typedef void __fastcall (__closure *TFullSynchronizeInNewWindow)
  (TSynchronizeMode Mode, int Params, const UnicodeString & LocalDirectory, const UnicodeString & RemoteDirectory,
   const TCopyParamType * CopyParams);
bool __fastcall DoFullSynchronizeDialog(TSynchronizeMode & Mode, int & Params,
  UnicodeString & LocalDirectory, UnicodeString & RemoteDirectory,
  TCopyParamType * CopyParams, bool & SaveSettings, bool & SaveMode,
  int Options, const TUsableCopyParamAttrs & CopyParamAttrs,
  TFullSynchronizeInNewWindow OnFullSynchronizeInNewWindow, int AutoSubmit);

// forms\SynchronizeChecklist.cpp
class TSynchronizeChecklist;
typedef void __fastcall (__closure *TCustomCommandMenuEvent)
  (TAction * Action, TStrings * LocalFileList, TStrings * RemoteFileList);
typedef void __fastcall (__closure *TFullSynchronizeEvent)(
  void * Token, TProcessedSynchronizationChecklistItem OnProcessedItem,
  TUpdatedSynchronizationChecklistItems OnUpdatedSynchronizationChecklistItems);
typedef void (__closure *TQueueSynchronizeEvent)(void * Token);
typedef void __fastcall (__closure *TSynchronizeChecklistCalculateSize)
  (TSynchronizeChecklist * Checklist, const TSynchronizeChecklist::TItemList & Items, void * Token);
typedef void __fastcall (__closure *TSynchronizeMoveEvent)(
  TOperationSide Side, TStrings * FileList, const UnicodeString & NewFileName, bool TargetIsDirectory, void * Token);
typedef void __fastcall (__closure *TSynchronizeBrowseEvent)(
  TOperationSide Side, TSynchronizeChecklist::TAction Action, const TSynchronizeChecklist::TItem * Item);
bool __fastcall DoSynchronizeChecklistDialog(TSynchronizeChecklist * Checklist,
  TSynchronizeMode Mode, int Params,
  const UnicodeString LocalDirectory, const UnicodeString RemoteDirectory,
  TCustomCommandMenuEvent OnCustomCommandMenu, TFullSynchronizeEvent OnSynchronize,
  TQueueSynchronizeEvent OnQueueSynchronize,
  TSynchronizeChecklistCalculateSize OnSynchronizeChecklistCalculateSize, TSynchronizeMoveEvent OnSynchronizeMove,
  TSynchronizeBrowseEvent OnSynchronizeBrowse, void * Token);

// forms\Editor.cpp
typedef void __fastcall (__closure *TFileClosedEvent)
  (TObject * Sender, bool Forced);
typedef void __fastcall (__closure *TAnyModifiedEvent)
  (TObject * Sender, bool & Modified);
TForm * __fastcall ShowEditorForm(const UnicodeString FileName, TForm * ParentForm,
  TNotifyEvent OnFileChanged, TNotifyEvent OnFileReload, TFileClosedEvent OnClose,
  TNotifyEvent OnSaveAll, TAnyModifiedEvent OnAnyModified,
  const UnicodeString Caption, bool StandaloneEditor, TColor Color, int InternalEditorEncodingOverride,
  bool NewFile);
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
int __fastcall GetMessageDialogContentWidth(TCustomForm * Form);
void __fastcall NavigateMessageDialogToUrl(TCustomForm * Form, const UnicodeString & Url);
extern const UnicodeString MessagePanelName;
extern const UnicodeString MainMessageLabelName;
extern const UnicodeString MessageLabelName;
extern const UnicodeString YesButtonName;
extern const UnicodeString OKButtonName;

// windows\Console.cpp
enum TConsoleMode
{
  cmNone, cmScripting, cmHelp, cmBatchSettings, cmKeyGen, cmFingerprintScan, cmDumpCallstack, cmInfo, cmComRegistration,
  cmCopyId,
};
int __fastcall Console(TConsoleMode Mode);

// forms\EditorPreferences.cpp
enum TEditorPreferencesMode { epmAdd, epmEdit, epmAdHoc };
class TEditorData;
bool __fastcall DoEditorPreferencesDialog(TEditorData * Editor,
  bool & Remember, TEditorPreferencesMode Mode, bool MayRemote);

// forms\Find.cpp
typedef void __fastcall (__closure *TFindEvent)
  (TTerminal * Terminal, UnicodeString Directory, const TFileMasks & FileMask,
   TFileFoundEvent OnFileFound, TFindingFileEvent OnFindingFile);
typedef void __fastcall (__closure *TFocusFileEvent)
  (TTerminal * Terminal, const UnicodeString & Path);
typedef void __fastcall (__closure *TFileOperationFinishedEvent)
  (TOperationSide Side, const UnicodeString & FileName, bool Success, bool NotCancelled);
typedef void __fastcall (__closure *TFileListOperationEvent)
  (TTerminal * Terminal, TStrings * FileList, TFileOperationFinishedEvent OnFileOperationFinished);
void __fastcall ShowFileFindDialog(
  TTerminal * Terminal, UnicodeString Directory, TFindEvent OnFind, TFocusFileEvent OnFocusFile,
  TFileListOperationEvent OnDeleteFiles, TFileListOperationEvent OnDownloadFiles,
  TFileListOperationEvent OnEditFiles);
void __fastcall HideFileFindDialog();

// forms\GenerateUrl.cpp
void __fastcall DoGenerateUrlDialog(TSessionData * Data, TStrings * Paths);
enum TFilesSelected { fsList, fsAll };
void __fastcall DoGenerateTransferCodeDialog(
  bool ToRemote, bool Move, int CopyParamAttrs, TSessionData * Data, TFilesSelected FilesSelected,
  TStrings * FileList, const UnicodeString & Path, const TCopyParamType & CopyParam);

const int cplNone =             0x00;
const int cplCustomizeDefault = 0x02;
const int cplSaveSettings =     0x04;
const int cplGenerateCode =     0x08;
void __fastcall CopyParamListPopup(TRect R, TPopupMenu * Menu,
  const TCopyParamType & Param, UnicodeString Preset, TNotifyEvent OnClick,
  int Options, int CopyParamAttrs, bool SaveSettings = false);
int __fastcall CopyParamListPopupClick(TObject * Sender,
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
TColor __fastcall RestoreColor(const UnicodeString & CStr);
UnicodeString __fastcall StoreColor(TColor Color);

void __fastcall FixButtonImage(TButton * Button);
void __fastcall CenterButtonImage(TButton * Button);

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
TShortCut __fastcall NormalizeCustomShortCut(TShortCut ShortCut);

UnicodeString DumpCallstackEventName(int ProcessId);
UnicodeString DumpCallstackFileName(int ProcessId);

void CheckConfigurationForceSave();
void InterfaceStarted();
void InterfaceStartDontMeasure();
void AddStartupSequence(const UnicodeString & Tag);
//---------------------------------------------------------------------------
#define HIDDEN_WINDOW_NAME L"WinSCPHiddenWindow3"
//---------------------------------------------------------------------------
struct TCopyDataMessage
{
  enum { CommandCanCommandLine, CommandCommandLine, MainWindowCheck, RefreshPanel };
  static const unsigned int Version1 = 1;

  unsigned int Version;
  unsigned int Command;

  union
  {
    wchar_t CommandLine[10240];

    struct
    {
      wchar_t Session[1024];
      wchar_t Path[1024];
    } Refresh;
  };

  TCopyDataMessage()
  {
    Version = TCopyDataMessage::Version1;
    Command = static_cast<unsigned int>(-1);
  }
};
//---------------------------------------------------------------------------
class TWinInteractiveCustomCommand : public TInteractiveCustomCommand
{
public:
  TWinInteractiveCustomCommand(
    TCustomCommand * ChildCustomCommand, const UnicodeString CustomCommandName, const UnicodeString HelpKeyword);

protected:
  virtual void __fastcall Prompt(int Index, const UnicodeString & Prompt,
    UnicodeString & Value);
  virtual void __fastcall Execute(const UnicodeString & Command,
    UnicodeString & Value);
  virtual void __fastcall PatternHint(int Index, const UnicodeString & Pattern);

private:
  UnicodeString FCustomCommandName;
  std::map<int, size_t> FIndexes;
  TUnicodeStringVector FPrompts;
  TUnicodeStringVector FDefaults;
  TUnicodeStringVector FValues;
  UnicodeString FHelpKeyword;
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
enum TConsoleFlag
{
  cfLimitedOutput,
  cfLiveOutput,
  cfNoInteractiveInput,
  cfInteractive,
  cfCommandLineOnly,
  cfWantsProgress,
  cfStdOut,
  cfStdIn
};
//---------------------------------------------------------------------------
class TConsole
{
public:
  virtual __fastcall ~TConsole() {};
  virtual void __fastcall Print(UnicodeString Str, bool FromBeginning = false, bool Error = false) = 0;
  void __fastcall PrintLine(const UnicodeString & Str = UnicodeString(), bool Error = false);
  virtual bool __fastcall Input(UnicodeString & Str, bool Echo, unsigned int Timer) = 0;
  virtual int __fastcall Choice(
    UnicodeString Options, int Cancel, int Break, int Continue, int Timeouted, bool Timeouting, unsigned int Timer,
    UnicodeString Message) = 0;
  virtual bool __fastcall HasFlag(TConsoleFlag Flag) const = 0;
  virtual bool __fastcall PendingAbort() = 0;
  virtual void __fastcall SetTitle(UnicodeString Title) = 0;
  virtual void __fastcall WaitBeforeExit() = 0;
  virtual void __fastcall Progress(TScriptProgress & Progress) = 0;
  virtual void __fastcall TransferOut(const unsigned char * Data, size_t Len) = 0;
  virtual size_t __fastcall TransferIn(unsigned char * Data, size_t Len) = 0;
  virtual UnicodeString __fastcall FinalLogMessage() = 0;
};
//---------------------------------------------------------------------------
int __fastcall HandleException(TConsole * Console, Exception & E);
//---------------------------------------------------------------------------
enum { RESULT_SUCCESS = 0, RESULT_ANY_ERROR = 1 };
//---------------------------------------------------------------------------
#endif // WinInterfaceH
