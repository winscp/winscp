//---------------------------------------------------------------------------
#include <basepch.h>
#pragma hdrstop
#pragma package(smart_init)
#include "Common.h"
#include "HistoryComboBox.h"
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
namespace My
{
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmissing-prototypes"
  void __fastcall PACKAGE Register()
  {
    TComponentClass classes[] = {__classid(THistoryComboBox)};
    RegisterComponents(L"Martin", classes, LENOF(classes) - 1);
  }
#pragma clang diagnostic pop
}
//---------------------------------------------------------------------------
#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  return 1;
}
//---------------------------------------------------------------------------
