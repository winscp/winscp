//---------------------------------------------------------------------------
#ifndef Glyphs192H
#define Glyphs192H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include "PngImageList.hpp"
#include <Vcl.Controls.hpp>
#include <Vcl.ImgList.hpp>
#include <System.ImageList.hpp>
//---------------------------------------------------------------------------
class TGlyphs192Module : public TDataModule
{
__published:
  TPngImageList *ExplorerImages;
  TPngImageList *SessionImages;
  TPngImageList *QueueImages;

public:
  __fastcall TGlyphs192Module(TComponent * Owner);
};
//---------------------------------------------------------------------------
#endif
