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
#include "TBXThemes.hpp"
#include "TBXOfficeXPTheme.hpp"
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
void __fastcall ShowExtendedException(Exception * E)
{
  ShowExtendedExceptionEx(NULL, E);
}
//---------------------------------------------------------------------------
void __fastcall ShowExtendedExceptionEx(TSecureShell * SecureShell,
  Exception * E)
{
  if (!E->Message.IsEmpty())
  {
    if (E->InheritsFrom(__classid(Exception)))
    {
      if (!E->InheritsFrom(__classid(EAbort)))
      {
        TTerminalManager * Manager = TTerminalManager::Instance(false);

        TQueryType Type;
        bool CloseOnCompletion = (dynamic_cast<ESshTerminate*>(E) != NULL);
        Type = CloseOnCompletion ? qtInformation : qtError;

        if (E->InheritsFrom(__classid(EFatal)) && (SecureShell != NULL) &&
            (Manager != NULL) && (Manager->ActiveTerminal == SecureShell))
        {
          int Result;
          if (CloseOnCompletion)
          {
            if (WinConfiguration->ConfirmExitOnCompletion)
            {
              TMessageParams Params(mpNeverAskAgainCheck);
              Result = FatalExceptionMessageDialog(E, Type,
                Manager->Count > 1 ?
                  FMTLOAD(DISCONNECT_ON_COMPLETION, (Manager->Count - 1)) :
                  LoadStr(EXIT_ON_COMPLETION),
                qaYes | qaNo, HELP_NONE, &Params);

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
          if (CloseOnCompletion)
          {
            if (WinConfiguration->ConfirmExitOnCompletion)
            {
              TMessageParams Params(mpNeverAskAgainCheck);
              if (ExceptionMessageDialog(E, Type, "", qaOK, HELP_NONE, &Params) ==
                    qaNeverAskAgain)
              {
                WinConfiguration->ConfirmExitOnCompletion = false;
              }
            }
          }
          else
          {
            ExceptionMessageDialog(E, Type);
          }
        }
      }
    }
    else
    {
      FlashOnBackground();
      ShowException(ExceptObject(), ExceptAddr());
    }
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
  SetTBXSysParam(TSP_XPVISUALSTYLE, XPVS_AUTOMATIC);
  // Can be called during configuration creation.
  // Skip now, will be called again later.
  if (Configuration != NULL)
  {
    TBXSetTheme(WinConfiguration->Theme);
  }
}
//---------------------------------------------------------------------------
// dummy function to force linking of TBXOfficeXPTheme.pas
TTBXOfficeXPTheme * CreateTheme()
{
  return new TTBXOfficeXPTheme("OfficeXP");
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
void __fastcall FormHelp(TForm * Form, TControl * Control)
{
  AnsiString Keyword;
  if ((Control != NULL) && !Control->HelpKeyword.IsEmpty())
  {
    Keyword = Control->HelpKeyword;
  }
  else
  {
    Keyword = Form->HelpKeyword;
  }
  Application->HelpKeyword(Keyword);
}
//---------------------------------------------------------------------
