//---------------------------------------------------------------------------
#ifndef RightsExtH
#define RightsExtH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "Rights.h"
#include <ActnList.hpp>
#include <Buttons.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "GrayedCheckBox.hpp"
//---------------------------------------------------------------------------
class TRightsExtFrame : public TRightsFrame
{
__published:
  TLabel *OctalLabel;
  TEdit *OctalEdit;
  TGrayedCheckBox *SetUidCheck;
  TGrayedCheckBox *SetGIDCheck;
  TGrayedCheckBox *StickyBitCheck;
  void __fastcall OctalEditChange(TObject *Sender);
  void __fastcall OctalEditExit(TObject *Sender);
  
public:
  __fastcall TRightsExtFrame(TComponent* Owner);

protected:
  virtual void __fastcall UpdateControls();
  virtual void __fastcall ForceUpdate();

private:
  void __fastcall UpdateByOctal();
};
//---------------------------------------------------------------------------
#endif
