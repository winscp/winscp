//---------------------------------------------------------------------------
#ifndef LicenseH
#define LicenseH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Mask.hpp>

#include "WinInterface.h"
//---------------------------------------------------------------------------
class TLicenseDialog : public TForm
{
__published:
  TButton * CloseButton;
  TMemo * LicenseMemo;

public:
  __fastcall TLicenseDialog(TComponent * Owner, TLicense License);
};
//---------------------------------------------------------------------------
#endif
