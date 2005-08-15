//---------------------------------------------------------------------------
#ifndef BannerH
#define BannerH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Mask.hpp>

#include "WinInterface.h"
//---------------------------------------------------------------------------
class TBannerDialog : public TForm
{
__published:
  TButton *CloseButton;
  TMemo *BannerMemo;
  TCheckBox *NeverShowAgainCheck;
  TButton *HelpButton;
  void __fastcall HelpButtonClick(TObject *Sender);

public:
  __fastcall TBannerDialog(TComponent * Owner, AnsiString SessionName,
    const AnsiString & Banner);
  bool __fastcall Execute(bool & NeverShowAgain);
};
//---------------------------------------------------------------------------
#endif
