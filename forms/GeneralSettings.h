//---------------------------------------------------------------------------
#ifndef GeneralSettingsH
#define GeneralSettingsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Graphics.hpp>
#include <XPGroupBox.hpp>
//---------------------------------------------------------------------------
class TGeneralSettingsFrame : public TFrame
{
__published:
  TXPGroupBox *InterfaceGroup;
  TLabel *CommanderDescriptionLabel;
  TLabel *ExplorerDescriptionLabel;
  TImage *CommanderInterfacePicture;
  TImage *ExplorerInterfacePicture;
  TRadioButton *CommanderInterfaceButton;
  TRadioButton *ExplorerInterfaceButton;
  void __fastcall CommanderClick(TObject *Sender);
  void __fastcall ExplorerClick(TObject *Sender);
public:
  void __fastcall LoadConfiguration();
  void __fastcall SaveConfiguration();
  __fastcall TGeneralSettingsFrame(TComponent* Owner);
};
//---------------------------------------------------------------------------
#endif
