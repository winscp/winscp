//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ScpCommander.h"
#include "ScpExplorer.h"

#include <CoreMain.h>
#include <Common.h>
#include <Exceptions.h>
#include <Cryptography.h>
#include "ProgParams.h"
#include "VCLCommon.h"
#include "WinConfiguration.h"
#include "TerminalManager.h"
#include "TextsWin.h"
#include "TBXThemes.hpp"
#include "TBXOfficeXPTheme.hpp"
#include "TBXOffice2003Theme.hpp"
#include "PasswordEdit.hpp"
#include "ProgParams.h"
#include "Tools.h"
#include "Custom.h"
#include "HelpWin.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
const UnicodeString AppName = L"WinSCP";
//---------------------------------------------------------------------------
TConfiguration * __fastcall CreateConfiguration()
{
  TConfiguration * Configuration = new TWinConfiguration();

  TProgramParams * Params = TProgramParams::Instance();
  UnicodeString IniFileName = Params->SwitchValue(L"ini");
  if (!IniFileName.IsEmpty())
  {
    if (AnsiSameText(IniFileName, L"nul"))
    {
      Configuration->SetNulStorage();
    }
    else
    {
      IniFileName = ExpandFileName(ExpandEnvironmentVariables(IniFileName));
      Configuration->IniFileStorageName = IniFileName;
    }
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
  return ScpExplorer;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall SshVersionString()
{
  return FORMAT(L"WinSCP-release-%s", (Configuration->Version));
}
//---------------------------------------------------------------------------
UnicodeString __fastcall AppNameString()
{
  return L"WinSCP";
}
//---------------------------------------------------------------------------
UnicodeString __fastcall GetRegistryKey()
{
  return L"Software\\Martin Prikryl\\WinSCP 2";
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
    FlashWindow(Application->MainFormHandle, true);
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
  UnicodeString Message; // not used
  bool Show = ExceptionMessage(E, Message);

  TTerminalManager * Manager = TTerminalManager::Instance(false);

  TQueryType Type;
  ESshTerminate * Terminate = dynamic_cast<ESshTerminate*>(E);
  bool CloseOnCompletion = (Terminate != NULL);
  Type = CloseOnCompletion ? qtInformation : qtError;
  bool ConfirmExitOnCompletion =
    CloseOnCompletion &&
    (Terminate->Operation == odoDisconnect) &&
    WinConfiguration->ConfirmExitOnCompletion;

  if (E->InheritsFrom(__classid(EFatal)) && (Terminal != NULL) &&
      (Manager != NULL) && (Manager->ActiveTerminal == Terminal))
  {
    if (CloseOnCompletion)
    {
      Manager->DisconnectActiveTerminal();
    }

    int SessionReopenTimeout = 0;
    TManagedTerminal * ManagedTerminal = dynamic_cast<TManagedTerminal *>(Terminal);
    if ((ManagedTerminal != NULL) &&
        ((Configuration->SessionReopenTimeout == 0) ||
         ((double)ManagedTerminal->ReopenStart == 0) ||
         (int(double(Now() - ManagedTerminal->ReopenStart) * MSecsPerDay) < Configuration->SessionReopenTimeout)))
    {
      SessionReopenTimeout = GUIConfiguration->SessionReopenAutoIdle;
    }

    unsigned int Result;
    if (CloseOnCompletion)
    {
      assert(Show);
      if (ConfirmExitOnCompletion)
      {
        TMessageParams Params(mpNeverAskAgainCheck);
        Result = FatalExceptionMessageDialog(E, Type, 0,
          (Manager->Count > 1) ?
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
      if (Show)
      {
        Result = FatalExceptionMessageDialog(E, Type, SessionReopenTimeout);
      }
      else
      {
        Result = qaOK;
      }
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
    // this should not happen as we never use Terminal->CloseOnCompletion
    // on inactive terminal
    if (CloseOnCompletion)
    {
      assert(Show);
      if (ConfirmExitOnCompletion)
      {
        TMessageParams Params(mpNeverAskAgainCheck);
        if (ExceptionMessageDialog(E, Type, L"", qaOK, HELP_NONE, &Params) ==
              qaNeverAskAgain)
        {
          WinConfiguration->ConfirmExitOnCompletion = false;
        }
      }
    }
    else
    {
      if (Show)
      {
        ExceptionMessageDialog(E, Type);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall ShowNotification(TTerminal * Terminal, const UnicodeString & Str,
  TQueryType Type)
{
  TTerminalManager * Manager = TTerminalManager::Instance(false);
  assert(Manager != NULL);

  Manager->ScpExplorer->PopupTrayBalloon(Terminal, Str, Type);
}
//---------------------------------------------------------------------------
void __fastcall ConfigureInterface()
{
  UnicodeString S;
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
  new TTBXOfficeXPTheme(L"OfficeXP");
  new TTBXOffice2003Theme(L"Office2003");
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
static inline void __fastcall GetToolbarKey(const UnicodeString & ToolbarName,
  const UnicodeString & Value, UnicodeString & ToolbarKey)
{
  int ToolbarNameLen;
  if ((ToolbarName.Length() > 7) &&
      (ToolbarName.SubString(ToolbarName.Length() - 7 + 1, 7) == L"Toolbar"))
  {
    ToolbarNameLen = ToolbarName.Length() - 7;
  }
  else
  {
    ToolbarNameLen = ToolbarName.Length();
  }
  ToolbarKey = ToolbarName.SubString(1, ToolbarNameLen) + L"_" + Value;
}
//---------------------------------------------------------------------------
static int __fastcall ToolbarReadInt(const UnicodeString ToolbarName,
  const UnicodeString Value, const int Default, const void * ExtraData)
{
  int Result;
  if (Value == L"Rev")
  {
    Result = 2000;
  }
  else
  {
    TStrings * Storage = static_cast<TStrings *>(const_cast<void*>(ExtraData));
    UnicodeString ToolbarKey;
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
static UnicodeString __fastcall ToolbarReadString(const UnicodeString ToolbarName,
  const UnicodeString Value, const UnicodeString Default, const void * ExtraData)
{
  UnicodeString Result;
  TStrings * Storage = static_cast<TStrings *>(const_cast<void*>(ExtraData));
  UnicodeString ToolbarKey;
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
static void __fastcall ToolbarWriteInt(const UnicodeString ToolbarName,
  const UnicodeString Value, const int Data, const void * ExtraData)
{
  if (Value != L"Rev")
  {
    TStrings * Storage = static_cast<TStrings *>(const_cast<void*>(ExtraData));
    UnicodeString ToolbarKey;
    GetToolbarKey(ToolbarName, Value, ToolbarKey);
    assert(Storage->IndexOfName(ToolbarKey) < 0);
    Storage->Values[ToolbarKey] = IntToStr(Data);
  }
}
//---------------------------------------------------------------------------
static void __fastcall ToolbarWriteString(const UnicodeString ToolbarName,
  const UnicodeString Value, const UnicodeString Data, const void * ExtraData)
{
  TStrings * Storage = static_cast<TStrings *>(const_cast<void*>(ExtraData));
  UnicodeString ToolbarKey;
  GetToolbarKey(ToolbarName, Value, ToolbarKey);
  assert(Storage->IndexOfName(ToolbarKey) < 0);
  Storage->Values[ToolbarKey] = Data;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall GetToolbarsLayoutStr(const TComponent * OwnerComponent)
{
  UnicodeString Result;
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
void __fastcall LoadToolbarsLayoutStr(const TComponent * OwnerComponent, UnicodeString LayoutStr)
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
  Menu->Add(Item);
}
//---------------------------------------------------------------------------
static TComponent * LastPopupComponent = NULL;
static TPoint LastPopupPoint(-1, -1);
static TDateTime LastCloseUp;
//---------------------------------------------------------------------------
void __fastcall MenuPopup(TPopupMenu * AMenu, TPoint Point,
  TComponent * PopupComponent)
{
  // Pressing the same button within 200ms after closing its popup menu
  // does nothing.
  // It is to immitate close-by-click behaviour. Note that menu closes itself
  // before onclick handler of button occurs.
  // To support content menu popups, we have to check for the popup location too,
  // to allow poping menu on different location (such as different node of TTreeView),
  // even if there's another popup opened already (so that the time interval
  // below does not elapse).
  TDateTime N = Now();
  TDateTime Diff = N - LastCloseUp;
  if ((PopupComponent == LastPopupComponent) &&
      (Point == LastPopupPoint) &&
      (Diff < TDateTime(0, 0, 0, 200)))
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

        if (!AItem->Enabled && !AItem->Visible && (AItem->Action == NULL) && (AItem->OnClick == NULL))
        {
          TTBXLabelItem * LabelItem = new TTBXLabelItem(Menu);
          // TTBXLabelItem has it's own Caption
          LabelItem->Caption = AItem->Caption;
          LabelItem->SectionHeader = true;
          Item = LabelItem;
        }
        else
        {
          // see TB2DsgnConverter.pas DoConvert
          if (AItem->Caption == L"-")
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
        }

        Menu->Items->Add(Item);
      }
    }

    Menu->PopupComponent = PopupComponent;
    Menu->Popup(Point.x, Point.y);

    LastPopupComponent = PopupComponent;
    LastPopupPoint = Point;
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
  TThreadID & ThreadId)
{
  TThreadParam * Param = new TThreadParam;
  Param->ThreadFunc = ThreadFunc;
  Param->Parameter = Parameter;
  return BeginThread(SecurityAttributes, StackSize, ThreadProc, Param,
    CreationFlags, ThreadId);
}
//---------------------------------------------------------------------------
static TShortCut FirstCtrlNumberShortCut = ShortCut(L'0', TShiftState() << ssCtrl);
static TShortCut LastCtrlNumberShortCut = ShortCut(L'9', TShiftState() << ssCtrl);
static TShortCut FirstShiftCtrlAltLetterShortCut = ShortCut(L'A', TShiftState() << ssShift << ssCtrl << ssAlt);
static TShortCut LastShiftCtrlAltLetterShortCut = ShortCut(L'Z', TShiftState() << ssShift << ssCtrl << ssAlt);
//---------------------------------------------------------------------------
void __fastcall InitializeShortCutCombo(TComboBox * ComboBox,
  const TShortCuts & ShortCuts)
{
  ComboBox->Items->BeginUpdate();
  try
  {
    ComboBox->Items->Clear();

    ComboBox->Items->AddObject(LoadStr(SHORTCUT_NONE), reinterpret_cast<TObject* >(0));

    for (TShortCut AShortCut = FirstCtrlNumberShortCut; AShortCut <= LastCtrlNumberShortCut; AShortCut++)
    {
      if (!ShortCuts.Has(AShortCut))
      {
        ComboBox->Items->AddObject(ShortCutToText(AShortCut), reinterpret_cast<TObject* >(AShortCut));
      }
    }

    for (TShortCut AShortCut = FirstShiftCtrlAltLetterShortCut; AShortCut <= LastShiftCtrlAltLetterShortCut; AShortCut++)
    {
      if (!ShortCuts.Has(AShortCut))
      {
        ComboBox->Items->AddObject(ShortCutToText(AShortCut), reinterpret_cast<TObject* >(AShortCut));
      }
    }
  }
  __finally
  {
    ComboBox->Items->EndUpdate();
  }

  ComboBox->Style = csDropDownList;
  ComboBox->DropDownCount = 16;
}
//---------------------------------------------------------------------------
void __fastcall SetShortCutCombo(TComboBox * ComboBox, TShortCut Value)
{
  for (int Index = ComboBox->Items->Count - 1; Index >= 0; Index--)
  {
    TShortCut AShortCut = TShortCut(ComboBox->Items->Objects[Index]);
    if (AShortCut == Value)
    {
      ComboBox->ItemIndex = Index;
      break;
    }
    else if (AShortCut < Value)
    {
      assert(Value != 0);
      ComboBox->Items->InsertObject(Index + 1, ShortCutToText(Value),
        reinterpret_cast<TObject* >(Value));
      ComboBox->ItemIndex = Index + 1;
      break;
    }
    assert(Index > 0);
  }
}
//---------------------------------------------------------------------------
TShortCut __fastcall GetShortCutCombo(TComboBox * ComboBox)
{
  return TShortCut(ComboBox->Items->Objects[ComboBox->ItemIndex]);
}
//---------------------------------------------------------------------------
bool __fastcall IsCustomShortCut(TShortCut ShortCut)
{
  return
    ((FirstCtrlNumberShortCut <= ShortCut) && (ShortCut <= LastCtrlNumberShortCut)) ||
    ((FirstShiftCtrlAltLetterShortCut <= ShortCut) && (ShortCut <= LastShiftCtrlAltLetterShortCut));
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TMasterPasswordDialog : public TCustomDialog
{
public:
  __fastcall TMasterPasswordDialog(bool Current);

  bool __fastcall Execute(UnicodeString & CurrentPassword, UnicodeString & NewPassword);

protected:
  virtual void __fastcall DoValidate();
  virtual void __fastcall DoChange(bool & CanSubmit);

private:
  TPasswordEdit * CurrentEdit;
  TPasswordEdit * NewEdit;
  TPasswordEdit * ConfirmEdit;
};
//---------------------------------------------------------------------------
__fastcall TMasterPasswordDialog::TMasterPasswordDialog(bool Current) :
  TCustomDialog(Current ? HELP_MASTER_PASSWORD_CURRENT : HELP_MASTER_PASSWORD_CHANGE)
{
  Caption = LoadStr(MASTER_PASSWORD_CAPTION);

  CurrentEdit = new TPasswordEdit(this);
  AddEdit(CurrentEdit, CreateLabel(LoadStr(MASTER_PASSWORD_CURRENT)));
  EnableControl(CurrentEdit, Current || WinConfiguration->UseMasterPassword);
  CurrentEdit->MaxLength = PasswordMaxLength();

  if (!Current)
  {
    NewEdit = new TPasswordEdit(this);
    AddEdit(NewEdit, CreateLabel(LoadStr(MASTER_PASSWORD_NEW)));
    NewEdit->MaxLength = CurrentEdit->MaxLength;

    if (!WinConfiguration->UseMasterPassword)
    {
      ActiveControl = NewEdit;
    }

    ConfirmEdit = new TPasswordEdit(this);
    AddEdit(ConfirmEdit, CreateLabel(LoadStr(MASTER_PASSWORD_CONFIRM)));
    ConfirmEdit->MaxLength = CurrentEdit->MaxLength;
  }
  else
  {
    NewEdit = NULL;
    ConfirmEdit = NULL;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TMasterPasswordDialog::Execute(
  UnicodeString & CurrentPassword, UnicodeString & NewPassword)
{
  bool Result = TCustomDialog::Execute();
  if (Result)
  {
    if (CurrentEdit->Enabled)
    {
      CurrentPassword = CurrentEdit->Text;
    }
    if (NewEdit != NULL)
    {
      NewPassword = NewEdit->Text;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TMasterPasswordDialog::DoChange(bool & CanSubmit)
{
  CanSubmit =
    (!WinConfiguration->UseMasterPassword || (IsValidPassword(CurrentEdit->Text) >= 0)) &&
    ((NewEdit == NULL) || (IsValidPassword(NewEdit->Text) >= 0)) &&
    ((ConfirmEdit == NULL) || (IsValidPassword(ConfirmEdit->Text) >= 0));
  TCustomDialog::DoChange(CanSubmit);
}
//---------------------------------------------------------------------------
void __fastcall TMasterPasswordDialog::DoValidate()
{
  TCustomDialog::DoValidate();

  if (WinConfiguration->UseMasterPassword &&
      !WinConfiguration->ValidateMasterPassword(CurrentEdit->Text))
  {
    CurrentEdit->SetFocus();
    CurrentEdit->SelectAll();
    throw Exception(LoadStr(MASTER_PASSWORD_INCORRECT));
  }

  if (NewEdit != NULL)
  {
    if (NewEdit->Text != ConfirmEdit->Text)
    {
      ConfirmEdit->SetFocus();
      ConfirmEdit->SelectAll();
      throw Exception(LoadStr(MASTER_PASSWORD_DIFFERENT));
    }

    int Valid = IsValidPassword(NewEdit->Text);
    if (Valid <= 0)
    {
      assert(Valid == 0);
      if (MessageDialog(LoadStr(MASTER_PASSWORD_SIMPLE), qtWarning,
            qaOK | qaCancel, HELP_MASTER_PASSWORD_SIMPLE) == qaCancel)
      {
        NewEdit->SetFocus();
        NewEdit->SelectAll();
        Abort();
      }
    }
  }
}
//---------------------------------------------------------------------------
static bool __fastcall DoMasterPasswordDialog(bool Current,
  UnicodeString & NewPassword)
{
  bool Result;
  TMasterPasswordDialog * Dialog = new TMasterPasswordDialog(Current);
  try
  {
    UnicodeString CurrentPassword;
    Result = Dialog->Execute(CurrentPassword, NewPassword);
    if (Result)
    {
      if ((Current || WinConfiguration->UseMasterPassword) &&
          ALWAYS_TRUE(!CurrentPassword.IsEmpty()))
      {
        WinConfiguration->SetMasterPassword(CurrentPassword);
      }
    }
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall DoMasterPasswordDialog()
{
  UnicodeString NewPassword;
  bool Result = DoMasterPasswordDialog(true, NewPassword);
  assert(NewPassword.IsEmpty());
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall DoChangeMasterPasswordDialog(UnicodeString & NewPassword)
{
  bool Result = DoMasterPasswordDialog(false, NewPassword);
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall MessageWithNoHelp(const UnicodeString & Message)
{
  TMessageParams Params;
  Params.AllowHelp = false; // to avoid recursion
  if (MessageDialog(LoadStr(HELP_SEND_MESSAGE), qtConfirmation,
        qaOK | qaCancel, HELP_NONE, &Params) == qaOK)
  {
    SearchHelp(Message);
  }
}
