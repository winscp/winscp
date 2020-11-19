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

public:
  __fastcall TGlyphsModule(TComponent * Owner);

  void __fastcall SetPixelsPerInch(int PixelsPerInch);

private:
  int FPixelsPerInch;

  __fastcall TGlyphsModule();
};
//---------------------------------------------------------------------------
extern PACKAGE TGlyphsModule * GlyphsModule;
const int SiteImageIndex = 103;
const int SiteColorMaskImageIndex = 104;
const int LocalBrowserImageIndex = 29;
//---------------------------------------------------------------------------
#endif
