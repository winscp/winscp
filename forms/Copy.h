//---------------------------------------------------------------------------
#ifndef CopyH
#define CopyH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComboEdit.hpp>
#include <Mask.hpp>
#include <MoreButton.hpp>
#include <ExtCtrls.hpp>
#include <UnixDirView.h>

#include "Rights.h"
#include "CopyParams.h"
//---------------------------------------------------------------------------
class TCopyDialog : public TForm
{
__published:
  TLabel *DirectoryLabel;
  TDirectoryEdit *LocalDirectoryEdit;
  TEdit *RemoteDirectoryEdit;
  TMoreButton *MoreButton;
  TButton *CopyButton;
  TButton *CancelButton;
  TPanel *MorePanel;
  TCheckBox *SaveSettingsCheck;
  TCopyParamsFrame *CopyParamsFrame;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
private:
  TTransferDirection FDirection;
  Boolean FDragDrop;
  TStrings * FFileList;
  TTransferType FTransferType;
  Boolean __fastcall GetAllowTransferMode();
  AnsiString __fastcall GetDirectory();
  void __fastcall SetDirection(TTransferDirection value);
  TCustomEdit * __fastcall GetDirectoryEdit();
  void __fastcall SetParams(TCopyParamType value);
  TCopyParamType __fastcall GetParams();
  void __fastcall SetAllowTransferMode(Boolean value);
  void __fastcall SetDirectory(AnsiString value);
  void __fastcall SetDragDrop(Boolean value);
  void __fastcall SetFileList(TStrings * value);
  void __fastcall SetTransferType(TTransferType value);
public:
  Boolean __fastcall Execute();
  __fastcall TCopyDialog(TComponent* Owner);
  __property Boolean AllowTransferMode = { read = GetAllowTransferMode, write = SetAllowTransferMode };
  __property TTransferDirection Direction = { read = FDirection, write = SetDirection };
  __property AnsiString Directory = { read = GetDirectory, write = SetDirectory };
  __property TCustomEdit * DirectoryEdit = { read = GetDirectoryEdit };
  __property Boolean DragDrop = { read = FDragDrop, write = SetDragDrop };
  __property TStrings * FileList = { read = FFileList, write = SetFileList };
  __property TCopyParamType Params = { read = GetParams, write = SetParams };
  __property TTransferType TransferType = { read = FTransferType, write = SetTransferType };
protected:
  void __fastcall UpdateControls();
};
//---------------------------------------------------------------------------
#endif
