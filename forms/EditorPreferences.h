//---------------------------------------------------------------------------
#ifndef EditorPreferencesH
#define EditorPreferencesH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "HistoryComboBox.hpp"
#include "XPThemes.hpp"
//---------------------------------------------------------------------------
class TEditorPreferencesDialog : public TForm
{
__published:
  TXPGroupBox *ExternalEditorGroup;
  TCheckBox *ExternalEditorTextCheck;
  TCheckBox *MDIExternalEditorCheck;
  TXPGroupBox *EditorGroup;
  TRadioButton *EditorInternalButton;
  TRadioButton *EditorExternalButton;
  THistoryComboBox *ExternalEditorEdit;
  TButton *ExternalEditorBrowseButton;
  TXPGroupBox *MaskGroup;
  TLabel *Label1;
  THistoryComboBox *MaskEdit;
  TButton *OkButton;
  TButton *CancelButton;
  TButton *HelpButton;
  void __fastcall ExternalEditorEditChange(TObject *Sender);
  void __fastcall ExternalEditorEditExit(TObject *Sender);
  void __fastcall ExternalEditorBrowseButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall MaskEditExit(TObject *Sender);

public:
  __fastcall TEditorPreferencesDialog(TComponent * Owner,
    TEditorPreferencesMode Mode);

  bool __fastcall Execute(TEditorPreferences * Editor);

private:
  TEditorPreferencesMode FMode;
  bool FAfterFilenameEditDialog;
  
  void __fastcall UpdateControls();
};
//---------------------------------------------------------------------------
#endif
