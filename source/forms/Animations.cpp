//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Global.h"
#include "Animations.h"
#include "Animations96.h"
#include "Animations120.h"
#include "Animations144.h"
#include "Animations192.h"
#include "GUITools.h"
#include "GUIConfiguration.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "PngImageList"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
static TAnimationsModule * AnimationsModule = NULL;
//---------------------------------------------------------------------------
TAnimationsModule * __fastcall GetAnimationsModule()
{
  if (AnimationsModule == NULL)
  {
    HANDLE ResourceModule = GUIConfiguration->ChangeToDefaultResourceModule();
    try
    {
      AnimationsModule = new TAnimationsModule(Application);
    }
    __finally
    {
      GUIConfiguration->ChangeResourceModule(ResourceModule);
    }
  }
  return AnimationsModule;
}
//---------------------------------------------------------------------------
__fastcall TAnimationsModule::TAnimationsModule(TComponent * Owner)
  : TDataModule(Owner)
{
  int PixelsPerInch = Screen->PixelsPerInch;
  TDataModule * ScaledModule;
  if (PixelsPerInch >= 192)
  {
    ScaledModule = new TAnimations192Module(Application);
  }
  else if (PixelsPerInch >= 144)
  {
    ScaledModule = new TAnimations144Module(Application);
  }
  else if (PixelsPerInch >= 120)
  {
    ScaledModule = new TAnimations120Module(Application);
  }
  else
  {
    ScaledModule = new TAnimations96Module(Application);
  }

  // Not really necessary as we never acccess AnimationImages by name
  CopyDataModule(this, ScaledModule);

  AnimationImages = DebugNotNull(dynamic_cast<TPngImageList *>(FindComponent(AnimationImages->Name)));
}
//---------------------------------------------------------------------------
