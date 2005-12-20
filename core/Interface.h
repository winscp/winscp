//---------------------------------------------------------------------------
#ifndef InterfaceH
#define InterfaceH
//---------------------------------------------------------------------------
#include "Configuration.h"
#include "SessionData.h"
#define HELP_NONE ""
//---------------------------------------------------------------------------
TConfiguration * __fastcall CreateConfiguration();

void __fastcall ShowExtendedException(Exception * E);

AnsiString __fastcall GetRegistryKey();
void __fastcall Busy(bool Start);
AnsiString __fastcall SshVersionString();

const unsigned int qaYes =      0x00000001;
const unsigned int qaNo =       0x00000002;
const unsigned int qaOK =       0x00000004;
const unsigned int qaCancel =   0x00000008;
const unsigned int qaAbort =    0x00000010;
const unsigned int qaRetry =    0x00000020;
const unsigned int qaIgnore =   0x00000040;
const unsigned int qaAll =      0x00000080;
const unsigned int qaNoToAll =  0x00000100;
const unsigned int qaYesToAll = 0x00000200;
const unsigned int qaHelp =     0x00000400;
const unsigned int qaSkip =     0x00000800;

const unsigned int qaNeverAskAgain = 0x00010000;

const int qpFatalAbort =           0x01;
const int qpNeverAskAgainCheck =   0x02;
const int qpAllowContinueOnError = 0x04;

struct TQueryButtonAlias
{
  unsigned int Button;
  AnsiString Alias;
};

typedef void __fastcall (__closure *TQueryParamsTimerEvent)(unsigned int & Result);

struct TQueryParams
{
  TQueryParams(unsigned int AParams = 0, AnsiString AHelpKeyword = HELP_NONE);

  const TQueryButtonAlias * Aliases;
  unsigned int AliasesCount;
  unsigned int Params;
  unsigned int Timer;
  TQueryParamsTimerEvent TimerEvent;
  AnsiString TimerMessage;
  unsigned int TimerAnswers;
  unsigned int Timeout;
  unsigned int TimeoutAnswer;
  AnsiString HelpKeyword;
};

enum TQueryType { qtConfirmation, qtWarning, qtError, qtInformation };
enum TPromptKind { pkPassword, pkPassphrase, pkServerPrompt };
//---------------------------------------------------------------------------
#endif
