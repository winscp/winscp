//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include "ProgParams.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TProgramParams * TProgramParams::SInstance = NULL;
//---------------------------------------------------------------------------
TProgramParams * __fastcall TProgramParams::Instance()
{
  assert(SInstance != NULL);
  return SInstance;
}
//---------------------------------------------------------------------------
TProgramParams::TProgramParams()
{
  assert(SInstance == NULL);
  SInstance = this;

  for (int i = 1; i <= ::ParamCount(); i++)
  {
    Add(ParamStr(i));
  }
}
//---------------------------------------------------------------------------
TProgramParams::~TProgramParams()
{
  assert(SInstance == this);
  SInstance = NULL;
}
