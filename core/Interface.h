//---------------------------------------------------------------------------
#ifndef InterfaceH
#define InterfaceH
//---------------------------------------------------------------------------
//!!!#include <Classes.hpp>
#include "Configuration.h"
#include "SessionData.h"
//---------------------------------------------------------------------------
void __fastcall ShowExtendedException(Exception* E, TObject* Sender = NULL);
void __fastcall HandleExtendedException(Exception* E, TObject* Sender = NULL);

Integer GetSessionPassword(AnsiString Prompt, AnsiString &Password);
AnsiString __fastcall GetRegistryKey();

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
const int qaResume =   0x1000;
const int qaCustom =   0x2000; // reserved for "More" button in VCL interface

const int qaNeverAskAgain = 0x8000;

const int qpFatalAbort =         0x01;
const int qpNeverAskAgainCheck = 0x02;

enum TQueryType { qtConfirmation, qtWarning, qtError, qtInformation };

/*#define mb_YesNoCancel (TMsgDlgButtons() << mbYes << mbNo << mbCancel)
#define mb_OKCancel (TMsgDlgButtons() << mbOK << mbCancel)
#define mb_YesNo (TMsgDlgButtons() << mbYes << mbNo)
#define mb_RetryAbort (TMsgDlgButtons() << mbRetry << mbAbort)
#define mb_OKAbort (TMsgDlgButtons() << mbOK << mbAbort)*/
//#define mbSkip mbIgnore
//#define mbResume mbHelp
//#define mrNeverAskAgain 100
//---------------------------------------------------------------------------
#endif
