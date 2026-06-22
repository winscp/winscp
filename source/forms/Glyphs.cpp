//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Global.h"
#include "Glyphs.h"
#include "Glyphs120.h"
#include "Glyphs144.h"
#include "Glyphs192.h"
#include "Common.h"
#include "GUITools.h"
#include "GUIConfiguration.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "PngImageList"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
TGlyphsModule * GlyphsModule;
//---------------------------------------------------------------------------
__fastcall TGlyphsModule::TGlyphsModule(TComponent* Owner)
  : TDataModule(Owner)
{
  FPixelsPerInch = USER_DEFAULT_SCREEN_DPI;
  FLargerToolbar = 0;
  SetPixelsPerInch(Screen->PixelsPerInch);
}
//---------------------------------------------------------------------------
// Constructor without scaling
__fastcall TGlyphsModule::TGlyphsModule()
  : TDataModule(Application)
{
}
//---------------------------------------------------------------------------
void TGlyphsModule::SetPixelsPerInch(int PixelsPerInch)
{
  int BasePixelsPerInch = NormalizePixelsPerInch(PixelsPerInch);
  if (FBasePixelsPerInch != BasePixelsPerInch)
  {
    FBasePixelsPerInch = BasePixelsPerInch;
    UpdatePixelsPerInch();
  }
}
//---------------------------------------------------------------------------
void TGlyphsModule::SetLargerToolbar(int LargerToolbar)
{
  if (FLargerToolbar != LargerToolbar)
  {
    FLargerToolbar = LargerToolbar;
    UpdatePixelsPerInch();
  }
}
//---------------------------------------------------------------------------
bool TGlyphsModule::IsLargerToolbarPossible(int Larger)
{
  int Prev = LargerPixelsPerInch(FBasePixelsPerInch, Larger - 1);
  return (LargerPixelsPerInch(Prev, 1) > Prev);
}
//---------------------------------------------------------------------------
void TGlyphsModule::UpdatePixelsPerInch()
{
  HANDLE ResourceModule = GUIConfiguration->ChangeToDefaultResourceModule();
  try
  {
    int PixelsPerInch = LargerPixelsPerInch(FBasePixelsPerInch, FLargerToolbar);
    if (FPixelsPerInch != PixelsPerInch)
    {
      std::unique_ptr<TDataModule> ScaledModule;
      if (PixelsPerInch >= 192)
      {
        ScaledModule.reset(new TGlyphs192Module(Application));
      }
      else if (PixelsPerInch >= 144)
      {
        ScaledModule.reset(new TGlyphs144Module(Application));
      }
      else if (PixelsPerInch >= 120)
      {
        ScaledModule.reset(new TGlyphs120Module(Application));
      }
      else
      {
        // Do not have a separate 96 DPI module, as this module needs to
        // have the images loaded as they are used on design time.
        // Performance impact of loading 96 DPI images when they are not needed is not that big.
        ScaledModule.reset(new TGlyphsModule());
      }

      if (ScaledModule.get() != NULL)
      {
        for (int Index = 0; Index < ComponentCount; Index++)
        {
          TComponent * TargetComponent = Components[Index];
          TComponent * SourceComponent = ScaledModule->FindComponent(TargetComponent->Name);
          if (DebugAlwaysTrue(SourceComponent != NULL))
          {
            TargetComponent->Assign(SourceComponent);
          }
        }
      }

      FPixelsPerInch = PixelsPerInch;
    }
  }
  __finally
  {
    GUIConfiguration->ChangeResourceModule(ResourceModule);
  }
}
