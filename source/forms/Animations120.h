//---------------------------------------------------------------------------
#ifndef Animations120H
#define Animations120H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include "PngImageList.hpp"
#include <Vcl.Controls.hpp>
#include <Vcl.ImgList.hpp>
#include <System.ImageList.hpp>
//---------------------------------------------------------------------------
class TAnimations120Module : public TDataModule
{
__published:
  TPngImageList *AnimationImages;
  TImageList *ButtonImages;
  TPngImageList *DialogImages;

public:
  __fastcall TAnimations120Module(TComponent * Owner);
};
//---------------------------------------------------------------------------
#endif
