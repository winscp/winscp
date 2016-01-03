//---------------------------------------------------------------------------
#ifndef Animations96H
#define Animations96H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include "PngImageList.hpp"
#include <Vcl.Controls.hpp>
#include <Vcl.ImgList.hpp>
//---------------------------------------------------------------------------
class TAnimations96Module : public TDataModule
{
__published:
  TPngImageList *AnimationImages;

public:
  __fastcall TAnimations96Module(TComponent * Owner);
  virtual __fastcall ~TAnimations96Module();
};
//---------------------------------------------------------------------------
#endif
