//---------------------------------------------------------------------------
#ifndef InterfaceH
#define InterfaceH
//---------------------------------------------------------------------------
#include "Configuration.h"
#include "SessionData.h"
//---------------------------------------------------------------------------
TConfiguration * __fastcall CreateConfiguration();

void __fastcall ShowExtendedException(Exception * E, TObject * Sender = NULL);
void __fastcall HandleExtendedException(Exception * E, TObject * Sender = NULL);

enum TPasswordKind { pkPassword, pkPassphrase, pkServerPrompt };
int __fastcall GetSessionPassword(AnsiString Prompt, TPasswordKind Kind,
    AnsiString & Password);
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
const unsigned int qaPrev =     0x00001000;
const unsigned int qaNext =     0x00002000;
// reserved for "More" button in VCL interface
const unsigned int qaAppend =   0x00004000;
const unsigned int qaCustom =   0x00008000;

const unsigned int qaNeverAskAgain = 0x00010000;

const int qpFatalAbort =           0x01;
const int qpNeverAskAgainCheck =   0x02;
const int qpAllowContinueOnError = 0x04;

enum TQueryType { qtConfirmation, qtWarning, qtError, qtInformation };
//---------------------------------------------------------------------------
#endif
