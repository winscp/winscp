//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Glyphs.h"
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
  SelectScaledImageList(ExplorerImages);
  SelectScaledImageList(SessionImages);
  SelectScaledImageList(QueueImages);
  SelectScaledImageList(ButtonImages);
  SelectScaledImageList(LogImages);
  SelectScaledImageList(DialogImages);
}
//---------------------------------------------------------------------------
