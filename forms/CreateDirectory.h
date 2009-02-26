//----------------------------------------------------------------------------
#ifndef CreateDirectoryH
#define CreateDirectoryH
//----------------------------------------------------------------------------
#include <vcl\System.hpp>
#include <vcl\Windows.hpp>
#include <vcl\SysUtils.hpp>
#include <vcl\Classes.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Buttons.hpp>
#include <ExtCtrls.hpp>

#include <Bookmarks.h>
#include "RightsExt.h"
#include "Rights.h"
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
  TRightsExtFrame *RightsFrame;
  TCheckBox *SetRightsCheck;
  TCheckBox *SaveSettingsCheck;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall DirectoryEditChange(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);

public:
  __fastcall TCreateDirectoryDialog(TComponent* AOwner);
  virtual __fastcall ~TCreateDirectoryDialog();

  bool __fastcall Execute(AnsiString & Directory, TRemoteProperties * Properties,
    bool & SaveSettings);

protected:
  void __fastcall UpdateControls();
};
//----------------------------------------------------------------------------
#endif
