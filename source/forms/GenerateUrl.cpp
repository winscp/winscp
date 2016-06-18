//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "VCLCommon.h"
#include "GenerateUrl.h"
#include "CoreMain.h"
#include "WinConfiguration.h"
#include <StrUtils.hpp>
#include <Tools.h>
#include <PuttyTools.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
void __fastcall DoGenerateUrlDialog(TSessionData * Data, TStrings * Paths)
{
  std::unique_ptr<TGenerateUrlDialog> Dialog(new TGenerateUrlDialog(GetFormOwner(), Data, Paths));
  Dialog->Execute();
}
//---------------------------------------------------------------------------
__fastcall TGenerateUrlDialog::TGenerateUrlDialog(
  TComponent * Owner, TSessionData * Data, TStrings * Paths)
  : TForm(Owner)
{
  UseSystemSettings(this);
  FData = Data;
  FPaths = Paths;
  FChanging = false;
  ReadOnlyControl(UrlMemo);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TGenerateUrlDialog::GenerateUrl(UnicodeString Path)
{
  UnicodeString Url =
    FData->GenerateSessionUrl(
      FLAGMASK(WinSCPSpecificCheck->Checked, sufSpecific) |
      FLAGMASK(UserNameCheck->Enabled && UserNameCheck->Checked, sufUserName) |
      FLAGMASK(PasswordCheck->Enabled && PasswordCheck->Checked, sufPassword) |
      FLAGMASK(HostKeyCheck->Enabled && HostKeyCheck->Checked, sufHostKey));

  if ((RemoteDirectoryCheck->Enabled && RemoteDirectoryCheck->Checked) ||
      (FPaths != NULL))
  {
    if (StartsStr(L"/", Path));
    {
      Path.Delete(1, 1);
    }

    Url += EncodeUrlPath(Path);
  }

  if (SaveExtensionCheck->Enabled && SaveExtensionCheck->Checked)
  {
    Url += UnicodeString(UrlParamSeparator) + UrlSaveParamName;
  }

  return Url;
}
//---------------------------------------------------------------------------
void __fastcall TGenerateUrlDialog::UpdateControls()
{
  if (!FChanging)
  {
    EnableControl(UserNameCheck, !FData->UserNameExpanded.IsEmpty());
    bool UserNameIncluded = UserNameCheck->Enabled && UserNameCheck->Checked;
    EnableControl(PasswordCheck, UserNameIncluded && FData->HasPassword());
    EnableControl(HostKeyCheck, UserNameIncluded && !FData->HostKey.IsEmpty());
    EnableControl(RemoteDirectoryCheck, !FData->RemoteDirectory.IsEmpty() && (FPaths == NULL));
    EnableControl(SaveExtensionCheck, (FPaths == NULL));

    UnicodeString Urls;

    if (FPaths == NULL)
    {
      UnicodeString Path = FData->RemoteDirectory;
      if (!Path.IsEmpty() && !EndsStr(L"/", Path))
      {
        Path += L"/";
      }
      Urls = GenerateUrl(Path);
    }
    else
    {
      for (int Index = 0; Index < FPaths->Count; Index++)
      {
        Urls += GenerateUrl(FPaths->Strings[Index]) + L"\n";
      }
    }

    UrlMemo->Lines->Text = Urls;
  }
}
//---------------------------------------------------------------------------
void __fastcall TGenerateUrlDialog::Execute()
{
  int Components = WinConfiguration->GenerateUrlComponents;
  if (Components < 0)
  {
    Components = UserNameCheck->Tag | RemoteDirectoryCheck->Tag;
  }

  {
    TAutoFlag ChangingFlag(FChanging);

    for (int Index = 0; Index < OptionsGroup->ControlCount; Index++)
    {
      TCheckBox * CheckBox = dynamic_cast<TCheckBox *>(OptionsGroup->Controls[Index]);

      if (ALWAYS_TRUE((CheckBox != NULL) && (CheckBox->Tag != 0)))
      {
        CheckBox->Checked = FLAGSET(Components, CheckBox->Tag);
      }
    }
  }

  UpdateControls();

  ShowModal();

  Components = 0;
  for (int Index = 0; Index < OptionsGroup->ControlCount; Index++)
  {
    TCheckBox * CheckBox = dynamic_cast<TCheckBox *>(OptionsGroup->Controls[Index]);

    if (ALWAYS_TRUE((CheckBox != NULL) && (CheckBox->Tag != 0)) &&
        CheckBox->Checked)
    {
      Components |= CheckBox->Tag;
    }
  }
  WinConfiguration->GenerateUrlComponents = Components;
}
//---------------------------------------------------------------------------
void __fastcall TGenerateUrlDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TGenerateUrlDialog::ClipboardButtonClick(TObject * /*Sender*/)
{
  TInstantOperationVisualizer Visualizer;
  CopyToClipboard(UrlMemo->Lines);
}
//---------------------------------------------------------------------------
void __fastcall TGenerateUrlDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
