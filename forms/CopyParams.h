//---------------------------------------------------------------------------
#ifndef CopyParamsH
#define CopyParamsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <HistoryComboBox.hpp>
#include <XPGroupBox.hpp>

#include <WinInterface.h>

#include "Rights.h"
//---------------------------------------------------------------------------
class TCopyParamsFrame : public TFrame
{
__published:
  TXPGroupBox *TransferModeGroup;
  TLabel *AsciiFileMaskLabel;
  TRadioButton *TMTextButton;
  TRadioButton *TMBinaryButton;
  TRadioButton *TMAutomaticButton;
  THistoryComboBox *AsciiFileMaskCombo;
  TXPGroupBox *RemotePropertiesGroup;
  TRightsFrame *RightsFrame;
  TCheckBox *RemotePreserveTimeCheck;
  TXPGroupBox *LocalPropertiesGroup;
  TCheckBox *PreserveReadOnlyCheck;
  TCheckBox *LocalPreserveTimeCheck;
  TCheckBox *PreserveRightsCheck;
  TXPGroupBox *ChangeCaseGroup;
  TRadioButton *CCNoChangeButton;
  TRadioButton *CCUpperCaseButton;
  TRadioButton *CCLowerCaseButton;
  TRadioButton *CCFirstUpperCaseButton;
  TXPGroupBox *CommonPropertiesGroup;
  TCheckBox *CommonPreserveTimestampCheck;
  void __fastcall ControlChange(TObject *Sender);
private:
  TParamsForDirection FDirection;
  AnsiString FOrigMasks;
  Boolean FAllowTransferMode;
  TCopyParamType * FParams;
  Boolean __fastcall GetAllowTransferMode();
  AnsiString __fastcall GetAsciiFileMask();
  void __fastcall SetParams(TCopyParamType value);
  TCopyParamType __fastcall GetParams();
  void __fastcall SetAllowTransferMode(Boolean value);
  void __fastcall SetDirection(TParamsForDirection value);
  TCheckBox * __fastcall GetPreserveTimeCheck();
protected:
  void __fastcall UpdateControls();
  virtual void __fastcall SetEnabled(Boolean Value);
public:
  __fastcall ~TCopyParamsFrame();
  void __fastcall BeforeExecute();
  void __fastcall AfterExecute();
  void __fastcall SelectMask(Integer Start, Integer Length);
  __fastcall TCopyParamsFrame(TComponent* Owner);
  __property Boolean AllowTransferMode = { read = GetAllowTransferMode, write = SetAllowTransferMode };
  __property AnsiString AsciiFileMask = { read = GetAsciiFileMask };
  __property TParamsForDirection Direction = { read = FDirection, write = SetDirection };
  __property TCopyParamType Params = { read = GetParams, write = SetParams };
  __property TCheckBox * PreserveTimeCheck = { read = GetPreserveTimeCheck };
};
//---------------------------------------------------------------------------
#endif
