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
  TImageList *ButtonImages;
  TPngImageList *ExplorerImages120;
  TPngImageList *SessionImages120;
  TPngImageList *QueueImages120;
  TImageList *ButtonImages120;
  TPngImageList *LogImages120;
  TPngImageList *DialogImages;
  TPngImageList *DialogImages120;
  TPngImageList *ExplorerImages144;
  TPngImageList *ExplorerImages192;
  TPngImageList *SessionImages144;
  TPngImageList *SessionImages192;
  TPngImageList *QueueImages144;
  TPngImageList *QueueImages192;
  TImageList *ButtonImages144;
  TImageList *ButtonImages192;
  TPngImageList *LogImages144;
  TPngImageList *LogImages192;
  TPngImageList *DialogImages144;
  TPngImageList *DialogImages192;
  TPngImageList *AnimationImages;

public:
  __fastcall TGlyphsModule(TComponent * Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TGlyphsModule * GlyphsModule;
const int SiteImageIndex = 103;
const int SiteColorMaskImageIndex = 104;
//---------------------------------------------------------------------------
#endif
