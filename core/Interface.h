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

int __fastcall GetSessionPassword(AnsiString Prompt, AnsiString & Password);
AnsiString __fastcall GetRegistryKey();
void __fastcall Busy(bool Start);
AnsiString __fastcall SshVersionString();

const int qaYes =      0x0001;
const int qaNo =       0x0002;
const int qaOK =       0x0004;
const int qaCancel =   0x0008;
const int qaAbort =    0x0010;
const int qaRetry =    0x0020;
const int qaIgnore =   0x0040;
const int qaAll =      0x0080;
const int qaNoToAll =  0x0100;
const int qaYesToAll = 0x0200;
const int qaHelp =     0x0400;
const int qaSkip =     0x0800;
const int qaPrev =     0x1000;
const int qaNext =     0x2000;
const int qaCustom =   0x4000; // reserved for "More" button in VCL interface

const int qaNeverAskAgain = 0x8000;

const int qpFatalAbort =           0x01;
const int qpNeverAskAgainCheck =   0x02;
const int qpAllowContinueOnError = 0x04;

enum TQueryType { qtConfirmation, qtWarning, qtError, qtInformation };
//---------------------------------------------------------------------------
#endif
