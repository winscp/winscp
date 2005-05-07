// $jrsoftware: tb2k/Packages/tb2kdsgn_cb6.cpp,v 1.2 2002/11/14 18:07:19 jr Exp $
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEFORMNS("..\Source\TB2DsgnConverter.pas", Tb2dsgnconverter, TBConverterForm);
USEFORMNS("..\Source\TB2DsgnItemEditor.pas", Tb2dsgnitemeditor, TBItemEditForm);
USEFORMNS("..\Source\TB2DsgnConvertOptions.pas", Tb2dsgnconvertoptions, TBConvertOptionsForm);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
    return 1;
}
//---------------------------------------------------------------------------

