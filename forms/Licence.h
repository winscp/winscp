//---------------------------------------------------------------------------
#ifndef LicenceH
#define LicenceH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Mask.hpp>

#include "WinInterface.h"
//---------------------------------------------------------------------------
class TLicenceDialog : public TForm
{
__published:
  TButton *CloseButton;
  TMemo *LicenceMemo;
public:
  __fastcall TLicenceDialog(TComponent* Owner);
  __property TLicence Licence  = { read=FLicence, write=SetLicence };
  __property AnsiString LicenceText = { read = GetLicenceText, write = SetLicenceText };
private:
  TLicence FLicence;
  AnsiString __fastcall GetLicenceText();
  void __fastcall SetLicence(TLicence value);
  void __fastcall SetLicenceText(AnsiString value);
};
//---------------------------------------------------------------------------
#endif
