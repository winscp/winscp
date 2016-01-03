//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
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
void __fastcall ReleaseAnimationsModule()
{
  if (AnimationsModule != NULL)
  {
    SAFE_DESTROY(AnimationsModule);
  }
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

  try
  {
    // Not really necessary as we never acccess AnimationImages by name
    CopyDataModule(this, ScaledModule);
  }
  __finally
  {
    delete ScaledModule;
  }

  AnimationImages = DebugNotNull(dynamic_cast<TPngImageList *>(FindComponent(AnimationImages->Name)));
}
//---------------------------------------------------------------------------
__fastcall TAnimationsModule::~TAnimationsModule()
{
}
//---------------------------------------------------------------------------
