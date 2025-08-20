//---------------------------------------------------------------------------
#include <WinPCH.h>
#pragma hdrstop

#include "ProgParams.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
static std::unique_ptr<TProgramParams> ProgramParamsOwner;
//---------------------------------------------------------------------------
TProgramParams * __fastcall TProgramParams::Instance()
{
  if (ProgramParamsOwner.get() == NULL)
  {
    ProgramParamsOwner.reset(new TProgramParams());
  }
  return ProgramParamsOwner.get();
}
//---------------------------------------------------------------------------
__fastcall TProgramParams::TProgramParams()
{
  Init(CmdLine);
}
//---------------------------------------------------------------------------
__fastcall TProgramParams::TProgramParams(const UnicodeString & CmdLine)
{
  Init(CmdLine);
}
//---------------------------------------------------------------------------
void __fastcall TProgramParams::Init(const UnicodeString & CmdLine)
{
  UnicodeString CommandLine = CmdLine;

  UnicodeString Param;
  CutToken(CommandLine, Param);
  Parse(CommandLine);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TProgramParams::FormatSwitch(const UnicodeString & Switch)
{
  return FORMAT(L"/%s", (Switch));
}
