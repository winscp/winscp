//---------------------------------------------------------------------------
#include <FormsPCH.h>
#pragma hdrstop

#include "CopyParamPreset.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "CopyParams"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool __fastcall DoCopyParamPresetDialog(TCopyParamList * CopyParamList,
  int & Index, TCopyParamPresetMode Mode, TCopyParamRuleData * CurrentRuleData,
  const TCopyParamType & DefaultCopyParams)
{
  bool Result;
  TCopyParamPresetDialog * Dialog = new TCopyParamPresetDialog(GetFormOwner(), Mode, CurrentRuleData);
  try
  {
    Result = Dialog->Execute(CopyParamList, Index, DefaultCopyParams);
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall TCopyParamPresetDialog::TCopyParamPresetDialog(TComponent * Owner,
  TCopyParamPresetMode Mode, TCopyParamRuleData * CurrentRuleData)
  : TForm(Owner)
{
  SetCorrectFormParent(this);
  UseSystemSettings(this);
  FMode = Mode;
  FCurrentRuleData = CurrentRuleData;
  Caption = LoadStr(Mode == cpmEdit ? COPY_PARAM_EDIT : COPY_PARAM_ADD);
  HintLabel(RuleMaskHintText,
    FORMAT(L"%s\n \n%s",(LoadStr(MASK_HINT2), LoadStr(COMBINING_MASKS_HINT))));
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamPresetDialog::UpdateControls()
{
  EnableControl(OkButton, !DescriptionEdit->Text.IsEmpty());
  EnableControl(RuleGroup, HasRuleCheck->Checked);
  CurrentRuleButton->Visible =
    (FCurrentRuleData != NULL) &&
    // current rule data are loaded implicitly
    (FMode != cpmAddCurrent);
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamPresetDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamPresetDialog::Execute(TCopyParamList * CopyParamList,
  int & Index, const TCopyParamType & DefaultCopyParams)
{
  FCopyParamList = CopyParamList;
  if ((FMode == cpmEdit) || (FMode == cpmDuplicate))
  {
    const TCopyParamRule * Rule;
    if (Index >= 0)
    {
      CopyParamsFrame->Params = *CopyParamList->CopyParams[Index];
      Rule = CopyParamList->Rules[Index];
    }
    else
    {
      CopyParamsFrame->Params = DefaultCopyParams;
      Rule = NULL;
    }

    if (FMode == cpmEdit)
    {
      DescriptionEdit->Text = CopyParamList->Names[Index];
      FIndex = Index;
    }
    else
    {
      DescriptionEdit->Text = L"";
      FIndex = -1; // never used
      Index = FCopyParamList->Count;
    }
    HasRuleCheck->Checked = (Rule != NULL);

    if (Rule != NULL)
    {
      SetRuleData(Rule->Data);
    }
  }
  else
  {
    DescriptionEdit->Text = L"";
    CopyParamsFrame->Params = DefaultCopyParams;
    if (FMode == cpmAddCurrent)
    {
      SetRuleData(*FCurrentRuleData);
      HasRuleCheck->Checked = true;
    }
    else
    {
      HasRuleCheck->Checked = false;
    }
    FIndex = -1; // never used
    if (Index < 0)
    {
      Index = FCopyParamList->Count;
    }
  }

  CopyParamsFrame->BeforeExecute();
  bool Result = (ShowModal() == DefaultResult(this));

  if (Result)
  {
    CopyParamsFrame->AfterExecute();

    UnicodeString Name;
    TCopyParamType * CopyParam = NULL;
    TCopyParamRule * Rule = NULL;
    try
    {
      Name = DescriptionEdit->Text;
      CopyParam = new TCopyParamType(CopyParamsFrame->Params);
      Rule = GetRule();
    }
    catch(...)
    {
      delete CopyParam;
      delete Rule;
      throw;
    }

    if (FMode == cpmEdit)
    {
      FCopyParamList->Change(Index, Name, CopyParam, Rule);
    }
    else
    {
      FCopyParamList->Insert(Index, Name, CopyParam, Rule);
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamPresetDialog::SetRuleData(const TCopyParamRuleData & Data)
{
  HostNameEdit->Text = Data.HostName;
  UserNameEdit->Text = Data.UserName;
  RemoteDirectoryEdit->Text = Data.RemoteDirectory;
  LocalDirectoryEdit->Text = Data.LocalDirectory;
}
//---------------------------------------------------------------------------
TCopyParamRule * __fastcall TCopyParamPresetDialog::GetRule()
{
  TCopyParamRule * Rule = NULL;
  if (HasRuleCheck->Checked)
  {
    TCopyParamRuleData Data;
    // Last resort check, in case the mask escapes validation in OnExit by IsCancelButtonBeingClicked
    ValidateMask(HostNameEdit->Text);
    Data.HostName = HostNameEdit->Text;
    ValidateMask(UserNameEdit->Text);
    Data.UserName = UserNameEdit->Text;
    ValidateMask(RemoteDirectoryEdit->Text);
    Data.RemoteDirectory = RemoteDirectoryEdit->Text;
    ValidateMask(LocalDirectoryEdit->Text);
    Data.LocalDirectory = LocalDirectoryEdit->Text;
    Rule = new TCopyParamRule(Data);
  }
  return Rule;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamPresetDialog::FormShow(TObject * /*Sender*/)
{
  InstallPathWordBreakProc(HostNameEdit);
  InstallPathWordBreakProc(UserNameEdit);
  InstallPathWordBreakProc(RemoteDirectoryEdit);
  InstallPathWordBreakProc(LocalDirectoryEdit);

  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamPresetDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult == DefaultResult(this))
  {
    UnicodeString Description = DescriptionEdit->Text;
    TCopyParamList::ValidateName(Description);

    TCopyParamRule * Rule = GetRule();
    if (Rule != NULL)
    {
      try
      {
        if (Rule->IsEmpty)
        {
          throw Exception(LoadStr(COPY_PARAM_NO_RULE));
        }
      }
      __finally
      {
        delete Rule;
      }
    }

    int Index = FCopyParamList->IndexOfName(Description);
    if (((FMode == cpmEdit) && (Index >= 0) && (Index != FIndex)) ||
        (((FMode == cpmAdd) || (FMode == cpmAddCurrent) || (FMode == cpmDuplicate)) && (Index >= 0)))
    {
      DescriptionEdit->SetFocus();
      throw Exception(FMTLOAD(COPY_PARAM_DUPLICATE, (Description)));
    }

    ExitActiveControl(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamPresetDialog::CurrentRuleButtonClick(
  TObject * /*Sender*/)
{
  DebugAssert(FCurrentRuleData != NULL);
  SetRuleData(*FCurrentRuleData);
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamPresetDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamPresetDialog::MaskEditExit(TObject * Sender)
{
  ValidateMaskEdit(dynamic_cast<TEdit*>(Sender));
}
//---------------------------------------------------------------------------
