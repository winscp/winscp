//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ScpCommander.h"
#include "ScpExplorer.h"

#include <CoreMain.h>
#include <Common.h>
#include <Exceptions.h>
#include "ProgParams.h"
#include "VCLCommon.h"
#include "WinConfiguration.h"
#include "TerminalManager.h"
#include "TextsWin.h"
#include "TBXThemes.hpp"
#include "TBXOfficeXPTheme.hpp"
#include "TBXOffice2003Theme.hpp"
#include "ProgParams.h"
#include "Tools.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
const AnsiString AppName = "WinSCP";
const AnsiString AppNameVersion = "WinSCP";
//---------------------------------------------------------------------------
TConfiguration * __fastcall CreateConfiguration()
{
  TConfiguration * Configuration = new TWinConfiguration();

  TProgramParams * Params = TProgramParams::Instance();
  AnsiString IniFileName = Params->SwitchValue("ini");
  if (!IniFileName.IsEmpty())
  {
    IniFileName = ExpandFileName(ExpandEnvironmentVariables(IniFileName));
    Configuration->IniFileStorageName = IniFileName;
  }

  return Configuration;
}
//---------------------------------------------------------------------------
TCustomScpExplorerForm * __fastcall CreateScpExplorer()
{
  TCustomScpExplorerForm * ScpExplorer;
  if (WinConfiguration->Interface == ifExplorer)
  {
    ScpExplorer = SafeFormCreate<TScpExplorerForm>();
  }
  else
  {
    ScpExplorer = SafeFormCreate<TScpCommanderForm>();
  }
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
void __fastcall LocalSystemSettings(TCustomForm * /*Control*/)
{
  // noop
}
//---------------------------------------------------------------------------
void __fastcall ShowExtendedException(Exception * E)
{
  ShowExtendedExceptionEx(NULL, E);
}
//---------------------------------------------------------------------------
void __fastcall ShowExtendedExceptionEx(TTerminal * Terminal,
  Exception * E)
{
  AnsiString Message; // not used
  if (ExceptionMessage(E, Message))
  {
    TTerminalManager * Manager = TTerminalManager::Instance(false);

    TQueryType Type;
    ESshTerminate * Terminate = dynamic_cast<ESshTerminate*>(E);
    bool CloseOnCompletion = (Terminate != NULL);
    Type = CloseOnCompletion ? qtInformation : qtError;

    if (E->InheritsFrom(__classid(EFatal)) && (Terminal != NULL) &&
        (Manager != NULL) && (Manager->ActiveTerminal == Terminal))
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
        assert(Terminate != NULL);
        assert(Terminate->Operation != odoIdle);
        Application->Terminate();

        switch (Terminate->Operation)
        {
          case odoDisconnect:
            break;

          case odoShutDown:
            ShutDownWindows();
            break;

          default:
            assert(false);
        }
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
//---------------------------------------------------------------------------
void __fastcall ShowNotification(TTerminal * Terminal, const AnsiString & Str,
  TQueryType Type)
{
  TTerminalManager * Manager = TTerminalManager::Instance(false);
  assert(Manager != NULL);

  Manager->ScpExplorer->PopupTrayBalloon(Terminal, Str, Type);
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
void __fastcall DoProductLicense()
{
  DoLicenseDialog(lcWinScp);
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
//---------------------------------------------------------------------------
void __fastcall MenuPopup(TPopupMenu * AMenu, TPoint Point,
  TComponent * PopupComponent)
{
  static TComponent * LastPopupComponent = NULL;
  static TDateTime LastCloseUp;

  // pressing the same button within 200ms after closing its popup menu
  // does nothing.
  // it is to immitate close-by-click behaviour. note that menu closes itself
  // before onclick handler of button occurs
  if ((PopupComponent == LastPopupComponent) &&
      (Now() - LastCloseUp < TDateTime(0, 0, 0, 200)))
  {
    LastPopupComponent = NULL;
  }
  else
  {
    TTBXPopupMenu * Menu = dynamic_cast<TTBXPopupMenu *>(AMenu);
    if (Menu == NULL)
    {
      Menu = CreateTBXPopupMenu(AMenu->Owner);
      Menu->OnPopup = AMenu->OnPopup;
      Menu->Items->SubMenuImages = AMenu->Images;

      for (int Index = 0; Index < AMenu->Items->Count; Index++)
      {
        TMenuItem * AItem = AMenu->Items->Items[Index];
        TTBCustomItem * Item;

        // recurse not implemented yet
        assert(AItem->Count == 0);

        // see TB2DsgnConverter.pas DoConvert
        if (AItem->Caption == "-")
        {
          Item = new TTBXSeparatorItem(Menu);
        }
        else
        {
          Item = new TTBXItem(Menu);
          Item->Action = AItem->Action;
          Item->AutoCheck = AItem->AutoCheck;
          Item->Caption = AItem->Caption;
          Item->Checked = AItem->Checked;
          if (AItem->Default)
          {
            Item->Options = Item->Options << tboDefault;
          }
          Item->Enabled = AItem->Enabled;
          Item->GroupIndex = AItem->GroupIndex;
          Item->HelpContext = AItem->HelpContext;
          Item->ImageIndex = AItem->ImageIndex;
          Item->RadioItem = AItem->RadioItem;
          Item->ShortCut = AItem->ShortCut;
          Item->SubMenuImages = AItem->SubMenuImages;
          Item->OnClick = AItem->OnClick;
        }
        Item->Hint = AItem->Hint;
        Item->Tag = AItem->Tag;
        Item->Visible = AItem->Visible;

        Menu->Items->Add(Item);
      }
    }

    Menu->PopupComponent = PopupComponent;
    Menu->Popup(Point.x, Point.y);

    LastPopupComponent = PopupComponent;
    LastCloseUp = Now();
  }
}
//---------------------------------------------------------------------------
void __fastcall UpgradeSpeedButton(TSpeedButton * /*Button*/)
{
  // no-op yet
}
//---------------------------------------------------------------------------
struct TThreadParam
{
  TThreadFunc ThreadFunc;
  void * Parameter;
};
//---------------------------------------------------------------------------
static int __fastcall ThreadProc(void * AParam)
{
  TThreadParam * Param = reinterpret_cast<TThreadParam *>(AParam);
  unsigned int Result = Param->ThreadFunc(Param->Parameter);
  delete Param;
  EndThread(Result);
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall StartThread(void * SecurityAttributes, unsigned StackSize,
  TThreadFunc ThreadFunc, void * Parameter, unsigned CreationFlags,
  unsigned & ThreadId)
{
  TThreadParam * Param = new TThreadParam;
  Param->ThreadFunc = ThreadFunc;
  Param->Parameter = Parameter;
  return BeginThread(SecurityAttributes, StackSize, ThreadProc, Param,
    CreationFlags, ThreadId);
}
