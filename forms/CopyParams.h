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
#include <XPThemes.hpp>

#include <WinInterface.h>

#include "Rights.h"
//---------------------------------------------------------------------------
const int cfAllowTransferMode = 0x01;
const int cfAllowExcludeMask =  0x02;
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
  TCheckBox *ReplaceInvalidCharsCheck;
  TCheckBox *CommonCalculateSizeCheck;
  TXPGroupBox *FilterGroup;
  TLabel *Label1;
  THistoryComboBox *ExcludeFileMaskCombo;
  TRadioButton *CCLowerCaseShortButton;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall ValidateMaskComboExit(TObject *Sender);
private:
  TParamsForDirection FDirection;
  AnsiString FOrigMasks;
  TCopyParamType * FParams;
  int FOptions;
  void __fastcall SetParams(TCopyParamType value);
  TCopyParamType __fastcall GetParams();
  void __fastcall SetDirection(TParamsForDirection value);
  TCheckBox * __fastcall GetPreserveTimeCheck();
  void __fastcall SetOptions(int value);
protected:
  void __fastcall UpdateControls();
  virtual void __fastcall SetEnabled(Boolean Value);

  __property TCheckBox * PreserveTimeCheck = { read = GetPreserveTimeCheck };
public:
  __fastcall TCopyParamsFrame(TComponent* Owner);
  __fastcall ~TCopyParamsFrame();

  void __fastcall BeforeExecute();
  void __fastcall AfterExecute();
  void __fastcall Validate();

  __property int Options = { read = FOptions, write = SetOptions }; 
  __property TParamsForDirection Direction = { read = FDirection, write = SetDirection };
  __property TCopyParamType Params = { read = GetParams, write = SetParams };
};
//---------------------------------------------------------------------------
#endif
