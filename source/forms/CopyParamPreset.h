//---------------------------------------------------------------------------
#ifndef CopyParamPresetH
#define CopyParamPresetH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "CopyParams.h"
//---------------------------------------------------------------------------
class TCopyParamPresetDialog : public TForm
{
__published:
  TButton *OkButton;
  TButton *CancelButton;
  TLabel *Label1;
  TEdit *DescriptionEdit;
  TCopyParamsFrame *CopyParamsFrame;
  TGroupBox *RuleGroup;
  TLabel *Label2;
  TEdit *HostNameEdit;
  TCheckBox *HasRuleCheck;
  TLabel *Label3;
  TEdit *UserNameEdit;
  TLabel *Label4;
  TEdit *RemoteDirectoryEdit;
  TLabel *Label5;
  TEdit *LocalDirectoryEdit;
  TButton *CurrentRuleButton;
  TButton *HelpButton;
  TStaticText *RuleMaskHintText;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall CurrentRuleButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall MaskEditExit(TObject *Sender);

private:
  TCopyParamPresetMode FMode;
  TCopyParamList * FCopyParamList;
  int FIndex;
  TCopyParamRuleData * FCurrentRuleData;

protected:
  void __fastcall UpdateControls();
  TCopyParamRule * __fastcall GetRule();
  void __fastcall SetRuleData(const TCopyParamRuleData & Data);

  INTERFACE_HOOK

public:
  __fastcall TCopyParamPresetDialog(TComponent * Owner,
    TCopyParamPresetMode Mode, TCopyParamRuleData * CurrentRuleData);

  bool __fastcall Execute(TCopyParamList * CopyParamList, int & Index,
    const TCopyParamType & DefaultCopyParams);
};
//---------------------------------------------------------------------------
#endif
