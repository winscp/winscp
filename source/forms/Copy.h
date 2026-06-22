//---------------------------------------------------------------------------
#ifndef CopyH
#define CopyH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Mask.hpp>
#include <ExtCtrls.hpp>
#include <HistoryComboBox.hpp>
#include <Vcl.Imaging.pngimage.hpp>

#include "Rights.h"
#include "CopyParams.h"
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TCopyDialog : public TForm
{
__published:
  TLabel *DirectoryLabel;
  THistoryComboBox *LocalDirectoryEdit;
  THistoryComboBox *RemoteDirectoryEdit;
  TButton *OkButton;
  TButton *CancelButton;
  TButton *LocalDirectoryBrowseButton;
  TCheckBox *QueueCheck2;
  TButton *HelpButton;
  TCheckBox *NeverShowAgainCheck;
  TButton *TransferSettingsButton;
  TGroupBox *CopyParamGroup;
  TLabel *CopyParamLabel;
  TImage *Image;
  TPanel *ShortCutHintPanel;
  TLabel *ShortCutHintLabel;
  TPopupMenu *OkMenu;
  TMenuItem *DownloadItem;
  TMenuItem *BrowseItem;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall LocalDirectoryBrowseButtonClick(TObject *Sender);
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall TransferSettingsButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall CopyParamGroupClick(TObject *Sender);
  void __fastcall CopyParamGroupContextPopup(TObject *Sender,
          TPoint &MousePos, bool &Handled);
  void __fastcall TransferSettingsButtonDropDownClick(TObject *Sender);
  void __fastcall NeverShowAgainCheckClick(TObject *Sender);
  void __fastcall ShortCutHintLabelClick(TObject *Sender);
  void __fastcall LocalDirectoryEditExit(TObject *Sender);
  void __fastcall DownloadItemClick(TObject *Sender);
  void __fastcall BrowseItemClick(TObject *Sender);
  void __fastcall OkButtonDropDownClick(TObject *Sender);
  void __fastcall FormAfterMonitorDpiChanged(TObject *Sender, int OldDPI, int NewDPI);
private:
  bool FDefaultToRemote;
  bool FToRemote;
  TStrings * FFileList;
  bool FMove;
  int FOptions;
  int FOutputOptions;
  TGUICopyParamType FParams;
  TPopupMenu * FPresetsMenu;
  UnicodeString FPreset;
  TCopyParamType FCopyParams;
  int FCopyParamAttrs;
  TSessionData * FSessionData;
  bool FSaveSettings;
  bool FBrowse;
  UnicodeString __fastcall GetDirectory();
  THistoryComboBox * __fastcall GetDirectoryEdit();
  void __fastcall SetParams(const TGUICopyParamType & value);
  TGUICopyParamType __fastcall GetParams();
  void __fastcall SetDirectory(UnicodeString value);
  UnicodeString __fastcall GetFileMask();
  void __fastcall SetOutputOptions(int value);
  int __fastcall GetOutputOptions();
  void __fastcall CopyParamClick(TObject * Sender);
  void __fastcall GenerateCode();
protected:
  void __fastcall UpdateControls();
  void __fastcall AdjustControls();
  void __fastcall AdjustTransferControls();
  bool __fastcall RemotePaths();
  void __fastcall CopyParamListPopup(TRect R, int AdditionalOptions);
  int __fastcall ActualCopyParamAttrs();

  INTERFACE_HOOK;

public:
  __fastcall TCopyDialog(
    TComponent* Owner, bool ToRemote, bool Move, TStrings * FileList, int Options,
    int CopyParamAttrs, TSessionData * SessionData);
  virtual __fastcall ~TCopyDialog();
  bool __fastcall Execute();

  __property UnicodeString Directory = { read = GetDirectory, write = SetDirectory };
  __property THistoryComboBox * DirectoryEdit = { read = GetDirectoryEdit };
  __property TGUICopyParamType Params = { read = GetParams, write = SetParams };
  __property int OutputOptions = { read = GetOutputOptions, write = SetOutputOptions };
};
//---------------------------------------------------------------------------
#endif
