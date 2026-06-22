//----------------------------------------------------------------------------
#ifndef ProgressH
#define ProgressH
//----------------------------------------------------------------------------
#include "PathLabel.hpp"
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
//----------------------------------------------------------------------------
#include <FileOperationProgress.h>
#include <Vcl.Menus.hpp>
#include "PngImageList.hpp"
#include <Vcl.Imaging.pngimage.hpp>
#include <Vcl.ImgList.hpp>
//----------------------------------------------------------------------------
#include <GUITools.h>
#include "TB2Dock.hpp"
#include "TB2Item.hpp"
#include "TB2Toolbar.hpp"
#include "TBX.hpp"
#include "TB2ExtItems.hpp"
#include "TBXExtItems.hpp"
#include <Vcl.AppEvnts.hpp>
#include <System.ImageList.hpp>
#include <list>
//----------------------------------------------------------------------------
class TProgressForm : public TForm
{
__published:
  TPanel *MainPanel;
  TLabel *PathLabel;
  TPathLabel *FileLabel;
  TLabel *TargetLabel;
  TPathLabel *TargetPathLabel;
  TProgressBar *OperationProgress;
  TPanel *TransferPanel;
  TLabel *Label3;
  TLabel *TimeElapsedLabel;
  TLabel *StartTimeLabelLabel;
  TLabel *StartTimeLabel;
  TLabel *Label4;
  TLabel *BytesTransferredLabel;
  TLabel *Label12;
  TLabel *CPSLabel;
  TProgressBar *FileProgress;
  TTimer *UpdateTimer;
  TLabel *TimeLeftLabelLabel;
  TLabel *TimeLeftLabel;
  TPaintBox *AnimationPaintBox;
  TPngImageList *ImageList;
  TPanel *ToolbarPanel;
  TTBXDock *Dock;
  TTBXToolbar *Toolbar;
  TTBXItem *CancelItem;
  TTBXItem *MinimizeItem;
  TTBXItem *MoveToQueueItem;
  TTBXSubmenuItem *CycleOnceDoneItem;
  TTBXItem *IdleOnceDoneItem;
  TTBXItem *DisconnectOnceDoneItem;
  TTBXItem *SuspendOnceDoneItem;
  TTBXItem *ShutDownOnceDoneItem;
  TTBXComboBoxItem *SpeedComboBoxItem;
  TPanel *ComponentsPanel;
  TPngImageList *ImageList120;
  TPngImageList *ImageList144;
  TPngImageList *ImageList192;
  TApplicationEvents *ApplicationEvents;
  TTBXItem *SkipItem;
  void __fastcall UpdateTimerTimer(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormHide(TObject *Sender);
  void __fastcall CancelItemClick(TObject *Sender);
  void __fastcall MinimizeItemClick(TObject *Sender);
  void __fastcall MoveToQueueItemClick(TObject *Sender);
  void __fastcall OnceDoneItemClick(TObject *Sender);
  void __fastcall CycleOnceDoneItemClick(TObject *Sender);
  void __fastcall SpeedComboBoxItemAcceptText(TObject *Sender, UnicodeString &NewText,
          bool &Accept);
  void __fastcall SpeedComboBoxItemItemClick(TObject *Sender);
  void __fastcall SpeedComboBoxItemAdjustImageIndex(TTBXComboBoxItem *Sender, const UnicodeString AText,
          int AIndex, int &ImageIndex);
  void __fastcall ApplicationModalBegin(TObject * Sender);
  void __fastcall SpeedComboBoxItemClick(TObject *Sender);
  void __fastcall SkipItemClick(TObject *Sender);

private:
  TCancelStatus FCancel;
  bool FPendingSkip;
  bool FMoveToQueue;
  TFileOperationProgressType FData;
  bool FDataGot;
  bool FDataReceived;
  TFileOperation FLastOperation;
  TOperationSide FLastSide;
  bool FLastTotalSizeSet;
  bool FMinimizedByMe;
  int FUpdateCounter;
  bool FAsciiTransferChanged;
  bool FResumeStatusChanged;
  void * FShowAsModalStorage;
  bool FDeleteLocalToRecycleBin;
  bool FDeleteRemoteToRecycleBin;
  bool FReadOnly;
  unsigned long FCPSLimit;
  TDateTime FStarted;
  int FSinceLastUpdate;
  bool FModalBeginHooked;
  int FModalLevel;
  TFrameAnimation FFrameAnimation;
  typedef BiDiMap<TOnceDoneOperation, TTBCustomItem *> TOnceDoneItems;
  TOnceDoneItems FOnceDoneItems;
  bool FAllowSkip;
  TSynchronizeProgress * FSynchronizeProgress;
  UnicodeString FProgressStr;
  int FWheelDelta;

  void __fastcall SetOnceDoneOperation(TOnceDoneOperation value);
  TTBCustomItem * __fastcall CurrentOnceDoneItem();
  TOnceDoneOperation __fastcall GetOnceDoneOperation();
  void __fastcall SetOnceDoneItem(TTBCustomItem * Item);
  void __fastcall SetAllowMinimize(bool value);
  bool __fastcall GetAllowMinimize();
  void __fastcall SetReadOnly(bool value);
  void __fastcall GlobalMinimize(TObject * Sender);
  UnicodeString __fastcall ItemSpeed(const UnicodeString & Text, TTBXComboBoxItem * Item);
  void __fastcall CMDialogKey(TCMDialogKey & Message);

protected:
  void __fastcall CancelOperation();
  void __fastcall UpdateControls();
  void __fastcall ResetOnceDoneOperation();
  bool __fastcall ReceiveData(bool Force, int ModalLevelOffset);
  void __fastcall Minimize(TObject * Sender);
  virtual void __fastcall Dispatch(void * Message);
  void __fastcall SetCancelLower(TCancelStatus ACancel);
  DYNAMIC void __fastcall MouseWheelHandler(TMessage & Message);

  INTERFACE_HOOK;

public:
  static UnicodeString __fastcall ProgressStr(
    const TSynchronizeProgress * SynchronizeProgress, const TFileOperationProgressType * ProgressData);
  static void SizeToolbar(TForm * Form, TTBXToolbar * Toolbar, TTBXDock * Dock, TPanel * ToolbarPanel);

  virtual __fastcall TProgressForm(
    TComponent * AOwner, bool AllowMoveToQueue, bool AllowSkip, TSynchronizeProgress * SynchronizeProgress);
  virtual __fastcall ~TProgressForm();
  void __fastcall SetProgressData(TFileOperationProgressType & AData);
  void __fastcall ClearCancel();
  UnicodeString __fastcall ProgressStr();
  __property TCancelStatus Cancel = { read = FCancel };
  __property bool MoveToQueue = { read = FMoveToQueue };
  __property TOnceDoneOperation OnceDoneOperation = { read=GetOnceDoneOperation, write=SetOnceDoneOperation };
  __property bool AllowMinimize = { read=GetAllowMinimize, write=SetAllowMinimize };
  __property bool DeleteLocalToRecycleBin = { read=FDeleteLocalToRecycleBin, write=FDeleteLocalToRecycleBin };
  __property bool DeleteRemoteToRecycleBin = { read=FDeleteRemoteToRecycleBin, write=FDeleteRemoteToRecycleBin };
  __property bool ReadOnly = { read=FReadOnly, write=SetReadOnly };
  __property TSynchronizeProgress * SynchronizeProgress = { read = FSynchronizeProgress };
};
//----------------------------------------------------------------------------
#endif
