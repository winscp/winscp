//----------------------------------------------------------------------------
#ifndef CreateDirectoryH
#define CreateDirectoryH
//----------------------------------------------------------------------------
#include "Rights.h"
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
//----------------------------------------------------------------------------
#include <Bookmarks.h>
#include <GUITools.h>
//----------------------------------------------------------------------------
class TCreateDirectoryDialog : public TForm
{
__published:
  TButton *OKBtn;
  TButton *CancelBtn;
  TEdit *DirectoryEdit;
  TLabel *EditLabel;
  TButton *HelpButton;
  TPanel *MorePanel;
  TGroupBox *AttributesGroup;
  TRightsFrame *RightsFrame;
  TCheckBox *SetRightsCheck;
  TCheckBox *SaveSettingsCheck;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall DirectoryEditChange(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);

public:
  __fastcall TCreateDirectoryDialog(TComponent* AOwner, int AllowedChanges, bool Remote);
  virtual __fastcall ~TCreateDirectoryDialog();

  bool __fastcall Execute(UnicodeString & Directory, TRemoteProperties * Properties,
    bool & SaveSettings);

protected:
  void __fastcall UpdateControls();

  INTERFACE_HOOK

private:
  int FAllowedChanges;
};
//----------------------------------------------------------------------------
#endif
