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
//---------------------------------------------------------------------------
class TRemoteTransferDialog : public TForm
{
__published:
  TGroupBox *Group;
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

  void __fastcall Init(
    bool Multi, TStrings * Sessions, TStrings * Directories, TDirectRemoteCopy AllowDirectCopy,
    void * CurrentSession, TDirectoryExistsEvent OnDirectoryExists, bool TargetConfirmed);
  bool __fastcall Execute(void *& Session, UnicodeString & Target,
    UnicodeString & FileMask, bool & DirectCopy);

protected:
  void __fastcall UpdateControls();
  UnicodeString GetTarget();
  UnicodeString GetFileMask();
  bool __fastcall IsCurrentSessionSelected();
  void * GetSelectedSession();
  void __fastcall UpdateNotDirectCopyCheck();

private:
  TStrings * FDirectories;
  void * FCurrentSession;
  bool FDirectCopy;
  bool FMulti;
  TDirectRemoteCopy FAllowDirectCopy;
  TDirectoryExistsEvent FOnDirectoryExists;
  bool FTargetConfirmed;
  UnicodeString FOriginalTarget;

  INTERFACE_HOOK
};
//---------------------------------------------------------------------------
#endif
