//---------------------------------------------------------------------------
#ifndef GlyphsH
#define GlyphsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <ImgList.hpp>
#include "PngImageList.hpp"
#include <System.ImageList.hpp>
//---------------------------------------------------------------------------
class TGlyphsModule : public TDataModule
{
__published:
  TPngImageList *ExplorerImages;
  TPngImageList *SessionImages;
  TPngImageList *QueueImages;

public:
  __fastcall TGlyphsModule(TComponent * Owner);

  bool IsLargerToolbarPossible(int Larger);

  __property int PixelsPerInch = { read = FBasePixelsPerInch, write = SetPixelsPerInch };
  __property int LargerToolbar = { read = FLargerToolbar, write = SetLargerToolbar };

private:
  int FLargerToolbar;
  int FBasePixelsPerInch;
  int FPixelsPerInch;

  __fastcall TGlyphsModule();
  void UpdatePixelsPerInch();
  void SetPixelsPerInch(int PixelsPerInch);
  void SetLargerToolbar(int LargerToolbar);
};
//---------------------------------------------------------------------------
extern PACKAGE TGlyphsModule * GlyphsModule;
const int SiteImageIndex = 103;
const int SiteColorMaskImageIndex = 104;
const int LocalBrowserImageIndex = 114;
//---------------------------------------------------------------------------
#endif
