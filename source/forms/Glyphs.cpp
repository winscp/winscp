//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Global.h"
#include "Glyphs.h"
#include "Glyphs120.h"
#include "Glyphs144.h"
#include "Glyphs192.h"
#include "GUITools.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "PngImageList"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
TGlyphsModule * GlyphsModule;
//---------------------------------------------------------------------------
__fastcall TGlyphsModule::TGlyphsModule(TComponent* Owner)
  : TDataModule(Owner)
{
  int PixelsPerInch = Screen->PixelsPerInch;
  TDataModule * ScaledModule;
  if (PixelsPerInch >= 192)
  {
    ScaledModule = new TGlyphs192Module(Application);
  }
  else if (PixelsPerInch >= 144)
  {
    ScaledModule = new TGlyphs144Module(Application);
  }
  else if (PixelsPerInch >= 120)
  {
    ScaledModule = new TGlyphs120Module(Application);
  }
  else
  {
    // Do not have a separate 96 DPI module, as this module needs to
    // have the images loaded as they are used on design time.
    // Performace impact of loading 96 DPI images when they are not needed is not that big.
    ScaledModule = NULL;
  }

  if (ScaledModule != NULL)
  {
    CopyDataModule(this, ScaledModule);
  }
}
//---------------------------------------------------------------------------
