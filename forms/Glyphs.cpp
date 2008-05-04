//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Glyphs.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
TGlyphsModule * GlyphsModule;
//---------------------------------------------------------------------------
__fastcall TGlyphsModule::TGlyphsModule(TComponent* Owner)
  : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
