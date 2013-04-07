//---------------------------------------------------------------------------
#ifndef EditorPreferencesH
#define EditorPreferencesH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "HistoryComboBox.hpp"
//---------------------------------------------------------------------------
class TEditorPreferencesDialog : public TForm
{
__published:
  TGroupBox *ExternalEditorGroup;
  TCheckBox *ExternalEditorTextCheck;
  TCheckBox *SDIExternalEditorCheck;
  TGroupBox *EditorGroup2;
  TRadioButton *EditorInternalButton;
  TRadioButton *EditorExternalButton;
  THistoryComboBox *ExternalEditorEdit;
  TButton *ExternalEditorBrowseButton;
  TGroupBox *MaskGroup;
  TLabel *MaskLabel;
  THistoryComboBox *MaskEdit;
  TButton *OkButton;
  TButton *CancelButton;
  TButton *HelpButton;
  TCheckBox *RememberCheck;
  TRadioButton *EditorOpenButton;
  void __fastcall ExternalEditorEditExit(TObject *Sender);
  void __fastcall ExternalEditorBrowseButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall MaskEditExit(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);

public:
  virtual __fastcall TEditorPreferencesDialog(TComponent * Owner);

  void __fastcall Init(TEditorPreferencesMode Mode, bool MayRemote);
  bool __fastcall Execute(TEditorData * Editor, bool & Remember);

private:
  void __fastcall UpdateControls();

  bool FMayRemote;
};
//---------------------------------------------------------------------------
#endif
