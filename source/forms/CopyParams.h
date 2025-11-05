//---------------------------------------------------------------------------
#ifndef CopyParamsH
#define CopyParamsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <HistoryComboBox.h>

#include <WinInterface.h>

#include "Rights.h"
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
  TGroupBox *LocalPropertiesGroup;
  TCheckBox *PreserveReadOnlyCheck;
  TCheckBox *PreserveRightsCheck;
  TGroupBox *ChangeCaseGroup;
  TRadioButton *CCNoChangeButton;
  TRadioButton *CCUpperCaseButton;
  TRadioButton *CCLowerCaseButton;
  TGroupBox *CommonPropertiesGroup;
  TCheckBox *PreserveTimeCheck;
  TCheckBox *ReplaceInvalidCharsCheck;
  TCheckBox *CommonCalculateSizeCheck;
  TGroupBox *OtherGroup;
  TLabel *IncludeFileMaskLabel;
  THistoryComboBox *IncludeFileMaskCombo;
  TRadioButton *CCLowerCaseShortButton;
  TComboEdit *RightsEdit;
  TCheckBox *IgnorePermErrorsCheck;
  TCheckBox *ClearArchiveCheck;
  TStaticText *IncludeFileMaskHintText;
  TLabel *SpeedLabel3;
  THistoryComboBox *SpeedCombo;
  TButton *IncludeFileMaskButton;
  TCheckBox *NewerOnlyCheck;
  TCheckBox *RemoveCtrlZAndBOMCheck;
  TCheckBox *PreserveTimeDirsCheck;
  TCheckBox *EncryptNewFilesCheck;
  TCheckBox *ExcludeHiddenFilesCheck;
  TCheckBox *ExcludeEmptyDirectoriesCheck;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall ValidateMaskComboExit(TObject *Sender);
  void __fastcall RightsEditButtonClick(TObject *Sender);
  void __fastcall RightsEditExit(TObject *Sender);
  void __fastcall RightsEditContextPopup(TObject *Sender, TPoint &MousePos,
          bool &Handled);
  void __fastcall SpeedComboExit(TObject *Sender);
  void __fastcall IncludeFileMaskButtonClick(TObject *Sender);
private:
  UnicodeString FOrigMasks;
  TCopyParamType * FParams;
  int FCopyParamAttrs;
  TRightsFrame * FRightsFrame;
  bool FRemoveCtrlZ;
  void __fastcall SetParams(TCopyParamType value);
  TCopyParamType __fastcall GetParams();
  void __fastcall SetCopyParamAttrs(int value);
  void __fastcall RightsFrameChange(TObject * Sender);
protected:
  void __fastcall UpdateControls();
  virtual void __fastcall SetEnabled(Boolean Value);
  void __fastcall UpdateRightsByStr();
  virtual void __fastcall CreateWnd();

  INTERFACE_HOOK_CUSTOM(TFrame)

public:
  __fastcall TCopyParamsFrame(TComponent* Owner);
  __fastcall ~TCopyParamsFrame();

  void __fastcall BeforeExecute();
  void __fastcall AfterExecute();

  __property int CopyParamAttrs = { read = FCopyParamAttrs, write = SetCopyParamAttrs };
  __property TCopyParamType Params = { read = GetParams, write = SetParams };
};
//---------------------------------------------------------------------------
#endif
