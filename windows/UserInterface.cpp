//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ScpCommander.h"
#include "ScpExplorer.h"

#include <ScpMain.h>
#include <Common.h>
#include "WinConfiguration.h"
#include "TerminalManager.h"
#include "TextsWin.h"
#include "TBXThemes.hpp"
#include "TBXOfficeXPTheme.hpp"
#include "TBXOffice2003Theme.hpp"
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
static bool ForcedOnForeground = false;
void __fastcall SetOnForeground(bool OnForeground)
{
  ForcedOnForeground = OnForeground;
}
//---------------------------------------------------------------------------
void __fastcall FlashOnBackground()
{
  assert(Application);
  if (!ForcedOnForeground && !ForegroundTask())
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
          if (CloseOnCompletion)
          {
            Manager->DisconnectActiveTerminal();
          }

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
void __fastcall CreateThemes()
{
  new TTBXOfficeXPTheme("OfficeXP");
  new TTBXOffice2003Theme("Office2003");
}
//---------------------------------------------------------------------------
void __fastcall DoAboutDialog(TConfiguration *Configuration)
{
  DoAboutDialog(Configuration, true, NULL);
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
static inline void __fastcall GetToolbarKey(const AnsiString & ToolbarName,
  const AnsiString & Value, AnsiString & ToolbarKey)
{
  int ToolbarNameLen;
  if ((ToolbarName.Length() > 7) &&
      (ToolbarName.SubString(ToolbarName.Length() - 7 + 1, 7) == "Toolbar"))
  {
    ToolbarNameLen = ToolbarName.Length() - 7;
  }
  else
  {
    ToolbarNameLen = ToolbarName.Length();
  }
  ToolbarKey = ToolbarName.SubString(1, ToolbarNameLen) + "_" + Value;
}
//---------------------------------------------------------------------------
static int __fastcall ToolbarReadInt(const AnsiString ToolbarName,
  const AnsiString Value, const int Default, const void * ExtraData)
{
  int Result;
  if (Value == "Rev")
  {
    Result = 2000;
  }
  else
  {
    TStrings * Storage = static_cast<TStrings *>(const_cast<void*>(ExtraData));
    AnsiString ToolbarKey;
    GetToolbarKey(ToolbarName, Value, ToolbarKey);
    if (Storage->IndexOfName(ToolbarKey) >= 0)
    {
      Result = StrToIntDef(Storage->Values[ToolbarKey], Default);
    }
    else
    {
      Result = Default;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
static AnsiString __fastcall ToolbarReadString(const AnsiString ToolbarName,
  const AnsiString Value, const AnsiString Default, const void * ExtraData)
{
  AnsiString Result;
  TStrings * Storage = static_cast<TStrings *>(const_cast<void*>(ExtraData));
  AnsiString ToolbarKey;
  GetToolbarKey(ToolbarName, Value, ToolbarKey);
  if (Storage->IndexOfName(ToolbarKey) >= 0)
  {
    Result = Storage->Values[ToolbarKey];
  }
  else
  {
    Result = Default;
  }
  return Result;
}
//---------------------------------------------------------------------------
static void __fastcall ToolbarWriteInt(const AnsiString ToolbarName,
  const AnsiString Value, const int Data, const void * ExtraData)
{
  if (Value != "Rev")
  {
    TStrings * Storage = static_cast<TStrings *>(const_cast<void*>(ExtraData));
    AnsiString ToolbarKey;
    GetToolbarKey(ToolbarName, Value, ToolbarKey);
    assert(Storage->IndexOfName(ToolbarKey) < 0);
    Storage->Values[ToolbarKey] = IntToStr(Data);
  }
}
//---------------------------------------------------------------------------
static void __fastcall ToolbarWriteString(const AnsiString ToolbarName,
  const AnsiString Value, const AnsiString Data, const void * ExtraData)
{
  TStrings * Storage = static_cast<TStrings *>(const_cast<void*>(ExtraData));
  AnsiString ToolbarKey;
  GetToolbarKey(ToolbarName, Value, ToolbarKey);
  assert(Storage->IndexOfName(ToolbarKey) < 0);
  Storage->Values[ToolbarKey] = Data;
}
//---------------------------------------------------------------------------
AnsiString __fastcall GetToolbarsLayoutStr(const TComponent * OwnerComponent)
{
  AnsiString Result;
  TStrings * Storage = new TStringList();
  try
  {
    TBCustomSavePositions(OwnerComponent, ToolbarWriteInt, ToolbarWriteString,
      Storage);
    Result = Storage->CommaText;
  }
  __finally
  {
    delete Storage;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall LoadToolbarsLayoutStr(const TComponent * OwnerComponent, AnsiString LayoutStr)
{
  TStrings * Storage = new TStringList();
  try
  {
    Storage->CommaText = LayoutStr;
    TBCustomLoadPositions(OwnerComponent, ToolbarReadInt, ToolbarReadString,
      Storage);
  }
  __finally
  {
    delete Storage;
  }
}
//---------------------------------------------------------------------------
void __fastcall AddMenuSeparator(TTBCustomItem * Menu)
{
  TTBXSeparatorItem * Item = new TTBXSeparatorItem(Menu);
  Item->Caption = "-";
  Item->Hint = "E";
  Menu->Add(Item);
}
