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
  TPngImageList *AnimationImages120;
  TPngImageList *AnimationImages144;
  TPngImageList *AnimationImages192;

public:
  __fastcall TAnimationsModule(TComponent * Owner);
};
//---------------------------------------------------------------------------
#endif
