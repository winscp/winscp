//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Global.h"
#include "Animations.h"
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
  SelectScaledImageList(AnimationImages);
}
//---------------------------------------------------------------------------
