//---------------------------------------------------------------------------
#ifndef InterfaceH
#define InterfaceH
//---------------------------------------------------------------------------
#include "Configuration.h"
#include "SessionData.h"
#include <typeinfo>
#define HELP_NONE ""
#define SCRIPT_SWITCH "script"
#define COMMAND_SWITCH L"Command"
#define SESSIONNAME_SWICH L"SessionName"
#define NEWPASSWORD_SWITCH L"newpassword"
#define INI_NUL L"nul"
#define PRESERVETIME_SWITCH L"preservetime"
#define PRESERVETIMEDIRS_SWITCH_VALUE L"all"
#define NOPRESERVETIME_SWITCH L"nopreservetime"
#define PERMISSIONS_SWITCH L"permissions"
#define NOPERMISSIONS_SWITCH L"nopermissions"
#define SPEED_SWITCH L"speed"
#define TRANSFER_SWITCH L"transfer"
#define FILEMASK_SWITCH L"filemask"
#define RESUMESUPPORT_SWITCH L"resumesupport"
#define NEWERONLY_SWICH L"neweronly"
#define NONEWERONLY_SWICH L"noneweronly"
#define DELETE_SWITCH L"delete"
#define REFRESH_SWITCH L"refresh"
#define RAWTRANSFERSETTINGS_SWITCH L"rawtransfersettings"
extern const wchar_t * TransferModeNames[];
extern const int TransferModeNamesCount;
extern const wchar_t * ToggleNames[];
enum TToggle { ToggleOff, ToggleOn };
//---------------------------------------------------------------------------
TConfiguration * __fastcall CreateConfiguration();
class TOptions;
TOptions * __fastcall GetGlobalOptions();

void __fastcall ShowExtendedException(Exception * E);
bool __fastcall AppendExceptionStackTraceAndForget(TStrings *& MoreMessages);
void __fastcall IgnoreException(const std::type_info & ExceptionType);
UnicodeString __fastcall GetExceptionDebugInfo();

UnicodeString __fastcall GetCompanyRegistryKey();
UnicodeString __fastcall GetRegistryKey();
void * __fastcall BusyStart();
void __fastcall BusyEnd(void * Token);
const unsigned int GUIUpdateInterval = 100;
void __fastcall SetNoGUI();
bool __fastcall ProcessGUI(bool Force = false);
UnicodeString __fastcall AppNameString();
UnicodeString __fastcall SshVersionString();
void __fastcall CopyToClipboard(UnicodeString Text);
int __fastcall StartThread(void * SecurityAttributes, unsigned StackSize,
  TThreadFunc ThreadFunc, void * Parameter, unsigned CreationFlags,
  TThreadID & ThreadId);
bool __fastcall TextFromClipboard(UnicodeString & Text, bool Trim);

// Order of the values also define order of the buttons/answers on the prompts
// MessageDlg relies on these to be <= 0x0000FFFF
const unsigned int qaYes =      0x00000001;
// MessageDlg relies that answer do not conflict with mrCancel (=0x2)
const unsigned int qaNo =       0x00000004;
const unsigned int qaOK =       0x00000008;
const unsigned int qaCancel =   0x00000010;
const unsigned int qaYesToAll = 0x00000020;
const unsigned int qaNoToAll =  0x00000040;
const unsigned int qaAbort =    0x00000080;
const unsigned int qaRetry =    0x00000100;
const unsigned int qaIgnore =   0x00000200;
const unsigned int qaSkip =     0x00000400;
const unsigned int qaAll =      0x00000800;
const unsigned int qaHelp =     0x00001000;
const unsigned int qaReport =   0x00002000;

const unsigned int qaFirst = qaYes;
const unsigned int qaLast = qaReport;

const unsigned int qaNeverAskAgain = 0x00010000;

const int qpFatalAbort =           0x01;
const int qpNeverAskAgainCheck =   0x02;
const int qpAllowContinueOnError = 0x04;
const int qpIgnoreAbort =          0x08;
const int qpWaitInBatch =          0x10;

typedef void __fastcall (__closure *TButtonSubmitEvent)(TObject * Sender, unsigned int & Answer);

struct TQueryButtonAlias
{
  TQueryButtonAlias();

  unsigned int Button;
  UnicodeString Alias;
  TButtonSubmitEvent OnSubmit;
  int GroupWith;
  TShiftState GrouppedShiftState;
  bool ElevationRequired;
  bool MenuButton;
  UnicodeString ActionAlias;

  static TQueryButtonAlias CreateYesToAllGrouppedWithYes();
  static TQueryButtonAlias CreateNoToAllGrouppedWithNo();
  static TQueryButtonAlias CreateAllAsYesToNewerGrouppedWithYes();
  static TQueryButtonAlias CreateIgnoreAsRenameGrouppedWithNo();
};

typedef void __fastcall (__closure *TQueryParamsTimerEvent)(unsigned int & Result);
enum TQueryType { qtConfirmation, qtWarning, qtError, qtInformation };

struct TQueryParams
{
  TQueryParams(unsigned int AParams = 0, UnicodeString AHelpKeyword = HELP_NONE);
  TQueryParams(const TQueryParams & Source);

  void Assign(const TQueryParams & Source);

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
  unsigned int NoBatchAnswers;
  UnicodeString HelpKeyword;
};

enum TPromptKind
{
  pkPrompt,
  pkFileName,
  pkUserName,
  pkPassphrase,
  pkTIS,
  pkCryptoCard,
  pkKeybInteractive,
  pkPassword,
  pkNewPassword
};

enum TPromptUserParam { pupEcho = 0x01, pupRemember = 0x02 };

bool __fastcall IsAuthenticationPrompt(TPromptKind Kind);
bool __fastcall IsPasswordOrPassphrasePrompt(TPromptKind Kind, TStrings * Prompts);
bool __fastcall IsPasswordPrompt(TPromptKind Kind, TStrings * Prompts);
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TFileFoundEvent)
  (TTerminal * Terminal, const UnicodeString FileName, const TRemoteFile * File,
   bool & Cancel);
typedef void __fastcall (__closure *TFindingFileEvent)
  (TTerminal * Terminal, const UnicodeString Directory, bool & Cancel);
//---------------------------------------------------------------------------
class TOperationVisualizer
{
public:
  __fastcall TOperationVisualizer(bool UseBusyCursor = true);
  __fastcall ~TOperationVisualizer();

private:
  bool FUseBusyCursor;
  void * FToken;
};
//---------------------------------------------------------------------------
class TInstantOperationVisualizer : public TOperationVisualizer
{
public:
  __fastcall TInstantOperationVisualizer();
  __fastcall ~TInstantOperationVisualizer();

private:
  TDateTime FStart;
};
//---------------------------------------------------------------------------
struct TClipboardHandler
{
  UnicodeString Text;

  void __fastcall Copy(TObject * /*Sender*/, unsigned int & /*Answer*/)
  {
    TInstantOperationVisualizer Visualizer;
    CopyToClipboard(Text);
  }
};
//---------------------------------------------------------------------------
#endif
