//----------------------------------------------------------------------------
#ifndef FileSystemInfoH
#define FileSystemInfoH
//----------------------------------------------------------------------------
#include <vcl\System.hpp>
#include <vcl\Windows.hpp>
#include <vcl\SysUtils.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Graphics.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Buttons.hpp>
#include <vcl\ExtCtrls.hpp>
#include <XPThemes.hpp>
//----------------------------------------------------------------------------
class TTerminal;
//----------------------------------------------------------------------------
class TFileSystemInfoDialog : public TForm
{
__published:
  TButton *CloseButton;
  TXPGroupBox *ServerGroup;
  TLabel *Label1;
  TLabel *Label2;
  TEdit *SshVersionEdit;
  TEdit *CipherEdit;
  TLabel *Label3;
  TEdit *CompressionEdit;
  TXPGroupBox *ProtocolGroup;
  TLabel *Label4;
  TLabel *Label5;
  TLabel *Label6;
  TEdit *ModeChangingEdit;
  TEdit *OwnerGroupChangingEdit;
  TEdit *AnyCommandEdit;
  TLabel *Label7;
  TEdit *FSProtocolEdit;
  TLabel *Label8;
  TEdit *SymbolicHardLinkEdit;
  TLabel *Label9;
  TEdit *NativeTextModeEdit;
  TLabel *Label10;
  TEdit *UserGroupListingEdit;
  TMemo *InfoMemo;
  TEdit *SshImplementationEdit;
  TLabel *Label11;
  TLabel *Label12;
  TEdit *RemoteCopyEdit;
public:
	virtual __fastcall TFileSystemInfoDialog(TComponent* AOwner);

  __property TTerminal * Terminal = { read=FTerminal, write=SetTerminal };

private:
  TTerminal * FTerminal;

  void __fastcall SetTerminal(TTerminal * value);
  void __fastcall UpdateControls();
  AnsiString __fastcall CapabilityStr(TFSCapability Capability);
  AnsiString __fastcall CapabilityStr(TFSCapability Capability1,
    TFSCapability Capability2);
};
//----------------------------------------------------------------------------
#endif
