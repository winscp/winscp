//---------------------------------------------------------------------------
#ifndef CopyH
#define CopyH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Mask.hpp>
#include <MoreButton.hpp>
#include <ExtCtrls.hpp>
#include <HistoryComboBox.hpp>

#include "Rights.h"
#include "CopyParams.h"
//---------------------------------------------------------------------------
class TCopyDialog : public TForm
{
__published:
  TLabel *DirectoryLabel;
  THistoryComboBox *LocalDirectoryEdit;
  THistoryComboBox *RemoteDirectoryEdit;
  TMoreButton *MoreButton;
  TButton *CopyButton;
  TButton *CancelButton;
  TPanel *MorePanel;
  TCheckBox *SaveSettingsCheck;
  TCopyParamsFrame *CopyParamsFrame;
  TButton *LocalDirectoryBrowseButton;
  TCheckBox *QueueCheck;
  TCheckBox *QueueNoConfirmationCheck;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall LocalDirectoryBrowseButtonClick(TObject *Sender);
  void __fastcall DirectoryEditKeyDown(TObject *Sender, WORD &Key,
    TShiftState Shift);
  void __fastcall ControlChange(TObject *Sender);
private:
  bool FToRemote;
  TStrings * FFileList;
  bool FMove;
  int FOptions;
  TGUICopyParamType FParams;
  AnsiString __fastcall GetDirectory();
  void __fastcall SetToRemote(bool value);
  THistoryComboBox * __fastcall GetDirectoryEdit();
  void __fastcall SetParams(const TGUICopyParamType & value);
  TGUICopyParamType __fastcall GetParams();
  void __fastcall SetDirectory(AnsiString value);
  void __fastcall SetFileList(TStrings * value);
  void __fastcall SetMove(bool value);
  AnsiString __fastcall GetFileMask();
  void __fastcall SetOptions(int value);
protected:
  void __fastcall UpdateControls();
  void __fastcall AdjustControls();
public:
  __fastcall TCopyDialog(TComponent* Owner);
  bool __fastcall Execute();

  __property bool ToRemote = { read = FToRemote, write = SetToRemote };
  __property AnsiString Directory = { read = GetDirectory, write = SetDirectory };
  __property THistoryComboBox * DirectoryEdit = { read = GetDirectoryEdit };
  __property TStrings * FileList = { read = FFileList, write = SetFileList };
  __property TGUICopyParamType Params = { read = GetParams, write = SetParams };
  __property bool Move = { read = FMove, write = SetMove };
  __property int Options = { read = FOptions, write = SetOptions };
};
//---------------------------------------------------------------------------
#endif
