//---------------------------------------------------------------------------
#ifndef GlyphsH
#define GlyphsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <ImgList.hpp>
#include "PngImageList.hpp"
//---------------------------------------------------------------------------
class TGlyphsModule : public TDataModule
{
__published:
  TPngImageList *ExplorerImages;
  TPngImageList *SessionImages;
  TPngImageList *QueueImages;
  TPngImageList *LogImages;
  TImageList *ArrowImages;

public:
  __fastcall TGlyphsModule(TComponent * Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TGlyphsModule * GlyphsModule;
//---------------------------------------------------------------------------
#endif
