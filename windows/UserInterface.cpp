//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ScpCommander.h"
#include "ScpExplorer.h"
#include <About.h>

#include <ScpMain.h>
#include <Common.h>
#include "WinConfiguration.h"
#include "TerminalManager.h"
#include "TextsWin.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
const AnsiString AppName = "WinSCP";
const AnsiString AppNameVersion = "WinSCP3";
//---------------------------------------------------------------------------
TConfiguration * __fastcall CreateConfiguration()
{
  return new TWinConfiguration();
}
//---------------------------------------------------------------------------
TCustomScpExplorerForm * __fastcall CreateScpExplorer()
{
  TCustomScpExplorerForm * ScpExplorer;
  if (WinConfiguration->Interface == ifExplorer)
    Application->CreateForm(__classid(TScpExplorerForm), &ScpExplorer);
  else
    Application->CreateForm(__classid(TScpCommanderForm), &ScpExplorer);
  ScpExplorer->Icon->Assign(Application->Icon);
  return ScpExplorer;
}
//---------------------------------------------------------------------------
AnsiString __fastcall SshVersionString()
{
  return FORMAT("WinSCP-release-%s", (Configuration->Version));
}
//---------------------------------------------------------------------------
AnsiString __fastcall GetRegistryKey()
{
  return "Software\\Martin Prikryl\\WinSCP 2";
}
//---------------------------------------------------------------------------
void __fastcall FlashOnBackground()
{
  assert(Application);
  if (GetForegroundWindow() != GetActiveWindow())
  {
    FlashWindow(Application->Handle, true);
  }
}
//---------------------------------------------------------------------------
void __fastcall ShowExtendedException(Exception * E, TObject * Sender)
{
  ShowExtendedExceptionEx(E, Sender, false);
}
//---------------------------------------------------------------------------
void __fastcall ShowExtendedExceptionEx(Exception * E, TObject * Sender,
  bool NoReconnect)
{
  if (!E->Message.IsEmpty())
  {
    if (E->InheritsFrom(__classid(Exception)))
    {
      if (!E->InheritsFrom(__classid(EAbort)))
      {
        TQueryType Type;
        bool CloseOnCompletion = E->InheritsFrom(__classid(ESshTerminate));
        Type = CloseOnCompletion ? qtInformation : qtError;

        if (E->InheritsFrom(__classid(EFatal)) && !NoReconnect)
        {
          TTerminalManager * Manager = TTerminalManager::Instance();

          int Result;
          if (CloseOnCompletion)
          {
            if (WinConfiguration->ConfirmExitOnCompletion)
            {
              Result = FatalExceptionMessageDialog(E, Type,
                Manager->Count > 1 ?
                  FMTLOAD(DISCONNECT_ON_COMPLETION, (Manager->Count - 1)) :
                  LoadStr(EXIT_ON_COMPLETION),
                qaYes | qaNo, 0, mpNeverAskAgainCheck);

              if (Result == qaNeverAskAgain)
              {
                Result = qaYes;
                WinConfiguration->ConfirmExitOnCompletion = false;
              }
            }
            else
            {
              Result = qaYes;
            }
          }
          else
          {
            Result = FatalExceptionMessageDialog(E, Type);
          }

          if (Result == qaYes)
          {
            Application->Terminate();
          }
          else if (Result == qaRetry)
          {
            Manager->ReconnectActiveTerminal();
          }
          else
          {
            Manager->FreeActiveTerminal();
          }
        }
        else
        {
          ExceptionMessageDialog(E, Type, qaOK);
        }
      }
    }
    else
    {
      FlashOnBackground();
      ShowException(ExceptObject(), ExceptAddr());
    }
  }
  HandleExtendedException(E, Sender);
}
//---------------------------------------------------------------------------
void __fastcall HandleExtendedException(Exception * E, TObject* /*Sender*/)
{
  if (TTerminalManager::Instance(false) &&
      TTerminalManager::Instance()->ActiveTerminal)
  {
    TTerminalManager::Instance()->ActiveTerminal->Log->AddException(E);
  }
}
//---------------------------------------------------------------------------
void __fastcall ConfigureInterface()
{
  AnsiString S;
  S = LoadStr(MIDDLE_EAST);
  if (!S.IsEmpty())
  {
    SysLocale.MiddleEast = static_cast<bool>(StrToInt(S));
  }
  else
  {
    SysLocale.MiddleEast = false;
  }
  S = LoadStr(BIDI_MODE);
  if (!S.IsEmpty())
  {
    Application->BiDiMode = static_cast<TBiDiMode>(StrToInt(bdRightToLeft));
  }
  else
  {
    Application->BiDiMode = bdLeftToRight;
  }
}
//---------------------------------------------------------------------------
void __fastcall DoAboutDialog(TConfiguration *Configuration)
{
  TAboutDialog *AboutDialog = NULL;
  try
  {
    AboutDialog = new TAboutDialog(Application);
    AboutDialog->Configuration = Configuration;
    AboutDialog->ShowModal();
  }
  __finally
  {
    delete AboutDialog;
  }
}
//---------------------------------------------------------------------
void __fastcall DoProductLicence()
{
  DoLicenceDialog(lcWinScp);
}
//---------------------------------------------------------------------
