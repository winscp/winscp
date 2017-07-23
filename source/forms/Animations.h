//---------------------------------------------------------------------------
#ifndef AnimationsH
#define AnimationsH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include "PngImageList.hpp"
#include <Vcl.Controls.hpp>
#include <Vcl.ImgList.hpp>
//---------------------------------------------------------------------------
class TAnimationsModule : public TDataModule
{
__published:
  TPngImageList *AnimationImages;

public:
  __fastcall TAnimationsModule(TComponent * Owner);
  virtual __fastcall ~TAnimationsModule();
};
//---------------------------------------------------------------------------
#endif
