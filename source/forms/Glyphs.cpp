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
    // Performance impact of loading 96 DPI images when they are not needed is not that big.
    ScaledModule = NULL;
  }

  if (ScaledModule != NULL)
  {
    try
    {
      CopyDataModule(this, ScaledModule);

      // Not all these are accessed by field name, but we copy all for consistency
      ExplorerImages = DebugNotNull(dynamic_cast<TPngImageList *>(FindComponent(ExplorerImages->Name)));
      SessionImages = DebugNotNull(dynamic_cast<TPngImageList *>(FindComponent(SessionImages->Name)));
      QueueImages = DebugNotNull(dynamic_cast<TPngImageList *>(FindComponent(QueueImages->Name)));
      LogImages = DebugNotNull(dynamic_cast<TPngImageList *>(FindComponent(LogImages->Name)));
      ButtonImages = DebugNotNull(dynamic_cast<TImageList *>(FindComponent(ButtonImages->Name)));
      DialogImages = DebugNotNull(dynamic_cast<TPngImageList *>(FindComponent(DialogImages->Name)));
    }
    __finally
    {
      delete ScaledModule;
    }
  }
}
//---------------------------------------------------------------------------
