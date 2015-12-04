//---------------------------------------------------------------------------
#ifndef Animations144H
#define Animations144H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include "PngImageList.hpp"
#include <Vcl.Controls.hpp>
#include <Vcl.ImgList.hpp>
//---------------------------------------------------------------------------
class TAnimations144Module : public TDataModule
{
__published:
  TPngImageList *AnimationImages;

public:
  __fastcall TAnimations144Module(TComponent * Owner);
};
//---------------------------------------------------------------------------
#endif
