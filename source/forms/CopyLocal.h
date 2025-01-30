//---------------------------------------------------------------------------
#ifndef CopyLocalH
#define CopyLocalH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "HistoryComboBox.hpp"
#include <Vcl.ExtCtrls.hpp>
#include <GUITools.h>
//---------------------------------------------------------------------------
class TCopyLocalDialog : public TForm
{
__published:
  TImage *Image;
  TLabel *DirectoryLabel;
  THistoryComboBox *DirectoryEdit;
  TButton *OkButton;
  TButton *CancelButton;
  TButton *LocalDirectoryBrowseButton;
  TButton *HelpButton;
  TCheckBox *NeverShowAgainCheck;
  TPanel *ShortCutHintPanel;
  TLabel *ShortCutHintLabel;
  void __fastcall ShortCutHintLabelClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall DirectoryEditExit(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall LocalDirectoryBrowseButtonClick(TObject *Sender);
  void __fastcall FormAfterMonitorDpiChanged(TObject *Sender, int OldDPI, int NewDPI);

private:
  int FOptions;

  void UpdateControls();
  void ValidateDirectoryEdit();
  UnicodeString GetDirectory();
  UnicodeString GetFileMask();
  void SetDirectoryAndFileMask(const UnicodeString & Directory, const UnicodeString & FileMask);

  INTERFACE_HOOK;

public:
  TCopyLocalDialog(TComponent * Owner, bool Move, int Options);

  bool Execute(UnicodeString & TargetDirectory, UnicodeString & FileMask, int & OutputOptions);
};
//---------------------------------------------------------------------------
#endif
