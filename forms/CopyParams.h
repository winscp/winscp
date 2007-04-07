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

#include <WinInterface.h>

#include "RightsExt.h"
#include "ComboEdit.hpp"
#include <Mask.hpp>
//---------------------------------------------------------------------------
class TCopyParamsFrame : public TFrame
{
__published:
  TGroupBox *TransferModeGroup;
  TLabel *AsciiFileMaskLabel;
  TRadioButton *TMTextButton;
  TRadioButton *TMBinaryButton;
  TRadioButton *TMAutomaticButton;
  THistoryComboBox *AsciiFileMaskCombo;
  TGroupBox *RemotePropertiesGroup;
  TCheckBox *RemotePreserveTimeCheck;
  TGroupBox *LocalPropertiesGroup;
  TCheckBox *PreserveReadOnlyCheck;
  TCheckBox *LocalPreserveTimeCheck;
  TCheckBox *PreserveRightsCheck;
  TGroupBox *ChangeCaseGroup;
  TRadioButton *CCNoChangeButton;
  TRadioButton *CCUpperCaseButton;
  TRadioButton *CCLowerCaseButton;
  TRadioButton *CCFirstUpperCaseButton;
  TGroupBox *CommonPropertiesGroup;
  TCheckBox *CommonPreserveTimestampCheck;
  TCheckBox *ReplaceInvalidCharsCheck;
  TCheckBox *CommonCalculateSizeCheck;
  TGroupBox *OtherGroup;
  TLabel *ExclusionFileMaskLabel;
  THistoryComboBox *ExcludeFileMaskCombo;
  TRadioButton *CCLowerCaseShortButton;
  TCheckBox *ClearArchiveCheck;
  TComboBox *NegativeExcludeCombo;
  TStaticText *ExcludeFileMaskHintText;
  TComboEdit *RightsEdit;
  TCheckBox *IgnorePermErrorsCheck;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall ValidateMaskComboExit(TObject *Sender);
  void __fastcall RightsEditButtonClick(TObject *Sender);
  void __fastcall RightsEditExit(TObject *Sender);
  void __fastcall RightsEditContextPopup(TObject *Sender, TPoint &MousePos,
          bool &Handled);
private:
  TParamsForDirection FDirection;
  AnsiString FOrigMasks;
  TCopyParamType * FParams;
  int FCopyParamAttrs;
  TRightsExtFrame * FRightsFrame;
  void __fastcall SetParams(TCopyParamType value);
  TCopyParamType __fastcall GetParams();
  void __fastcall SetDirection(TParamsForDirection value);
  TCheckBox * __fastcall GetPreserveTimeCheck();
  void __fastcall SetCopyParamAttrs(int value);
  void __fastcall RightsFrameChange(TObject * Sender);
protected:
  void __fastcall UpdateControls();
  virtual void __fastcall SetEnabled(Boolean Value);
  void __fastcall UpdateRightsByStr();

  __property TCheckBox * PreserveTimeCheck = { read = GetPreserveTimeCheck };
public:
  __fastcall TCopyParamsFrame(TComponent* Owner);
  __fastcall ~TCopyParamsFrame();

  void __fastcall BeforeExecute();
  void __fastcall AfterExecute();

  __property int CopyParamAttrs = { read = FCopyParamAttrs, write = SetCopyParamAttrs };
  __property TParamsForDirection Direction = { read = FDirection, write = SetDirection };
  __property TCopyParamType Params = { read = GetParams, write = SetParams };
};
//---------------------------------------------------------------------------
#endif
