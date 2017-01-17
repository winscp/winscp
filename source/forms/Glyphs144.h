//---------------------------------------------------------------------------
#ifndef Glyphs144H
#define Glyphs144H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include "PngImageList.hpp"
#include <Vcl.Controls.hpp>
#include <Vcl.ImgList.hpp>
//---------------------------------------------------------------------------
class TGlyphs144Module : public TDataModule
{
__published:
  TPngImageList *ExplorerImages;
  TPngImageList *SessionImages;
  TPngImageList *QueueImages;
  TImageList *ButtonImages;
  TPngImageList *DialogImages;

public:
  __fastcall TGlyphs144Module(TComponent * Owner);
};
//---------------------------------------------------------------------------
#endif
