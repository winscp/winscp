//---------------------------------------------------------------------------
#ifndef Glyphs120H
#define Glyphs120H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include "PngImageList.hpp"
#include <Vcl.Controls.hpp>
#include <Vcl.ImgList.hpp>
#include <System.ImageList.hpp>
//---------------------------------------------------------------------------
class TGlyphs120Module : public TDataModule
{
__published:
  TPngImageList *ExplorerImages;
  TPngImageList *SessionImages;
  TPngImageList *QueueImages;

public:
  __fastcall TGlyphs120Module(TComponent * Owner);
};
//---------------------------------------------------------------------------
#endif
