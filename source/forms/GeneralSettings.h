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
#include <Vcl.Imaging.pngimage.hpp>
//---------------------------------------------------------------------------
class TGeneralSettingsFrame : public TFrame
{
__published:
  TGroupBox *InterfaceGroup;
  TLabel *CommanderDescriptionLabel2;
  TLabel *ExplorerDescriptionLabel;
  TImage *CommanderInterfacePicture;
  TImage *ExplorerInterfacePicture;
  TRadioButton *CommanderInterfaceButton2;
  TRadioButton *ExplorerInterfaceButton2;
  void __fastcall CommanderClick(TObject *Sender);
  void __fastcall ExplorerClick(TObject *Sender);
public:
  void __fastcall LoadConfiguration();
  void __fastcall SaveConfiguration();
  __fastcall TGeneralSettingsFrame(TComponent* Owner);
};
//---------------------------------------------------------------------------
#endif
