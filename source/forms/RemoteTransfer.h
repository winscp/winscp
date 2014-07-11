//---------------------------------------------------------------------------
#ifndef RemoteTransferH
#define RemoteTransferH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "HistoryComboBox.hpp"
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Imaging.pngimage.hpp>
//---------------------------------------------------------------------------
class TRemoteTransferDialog : public TForm
{
__published:
  TGroupBox *SymlinkGroup;
  TLabel *SessionLabel;
  TLabel *Label3;
  TComboBox *SessionCombo;
  THistoryComboBox *DirectoryEdit;
  TButton *OkButton;
  TButton *CancelButton;
  TButton *HelpButton;
  TCheckBox *NotDirectCopyCheck;
  TImage *Image;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall SessionComboChange(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall NotDirectCopyCheckClick(TObject *Sender);

public:
  __fastcall TRemoteTransferDialog(TComponent * Owner);

  void __fastcall Init(TStrings * Sessions, TStrings * Directories,
    TDirectRemoteCopy AllowDirectCopy);
  bool __fastcall Execute(void *& Session, UnicodeString & Target,
    UnicodeString & FileMask, bool & DirectCopy);

protected:
  void __fastcall UpdateControls();

private:
  TStrings * FDirectories;
  int FCurrentSession;
  bool FDirectCopy;
  TDirectRemoteCopy FAllowDirectCopy;
};
//---------------------------------------------------------------------------
extern PACKAGE TRemoteTransferDialog * RemoteTransferDialog;
//---------------------------------------------------------------------------
#endif
