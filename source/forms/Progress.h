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
  TProgressBar *TopProgress;
  TPanel *TransferPanel;
  TLabel *Label3;
  TLabel *TimeElapsedLabel;
  TLabel *StartTimeLabelLabel;
  TLabel *StartTimeLabel;
  TLabel *Label4;
  TLabel *BytesTransferedLabel;
  TLabel *Label12;
  TLabel *CPSLabel;
  TProgressBar *BottomProgress;
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
  void __fastcall FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
  void __fastcall ApplicationModalBegin(TObject * Sender);

private:
  TCancelStatus FCancel;
  bool FMoveToQueue;
  TFileOperationProgressType FData;
  bool FDataGot;
  bool FDataReceived;
  TFileOperation FLastOperation;
  bool FLastTotalSizeSet;
  bool FMinimizedByMe;
  int FUpdateCounter;
  bool FAsciiTransferChanged;
  bool FResumeStatusChanged;
  void * FShowAsModalStorage;
  bool FDeleteToRecycleBin;
  bool FReadOnly;
  unsigned long FCPSLimit;
  TProgressBar * FOperationProgress;
  TProgressBar * FFileProgress;
  TDateTime FStarted;
  int FSinceLastUpdate;
  bool FModalBeginHooked;
  int FModalLevel;
  TFrameAnimation FFrameAnimation;
  typedef BiDiMap<TOnceDoneOperation, TTBCustomItem *> TOnceDoneItems;
  TOnceDoneItems FOnceDoneItems;

  void __fastcall SetOnceDoneOperation(TOnceDoneOperation value);
  TTBCustomItem * __fastcall CurrentOnceDoneItem();
  TOnceDoneOperation __fastcall GetOnceDoneOperation();
  void __fastcall SetOnceDoneItem(TTBCustomItem * Item);
  void __fastcall SetAllowMinimize(bool value);
  bool __fastcall GetAllowMinimize();
  void __fastcall SetReadOnly(bool value);
  void __fastcall GlobalMinimize(TObject * Sender);
  UnicodeString __fastcall ItemSpeed(const UnicodeString & Text, TTBXComboBoxItem * Item);
  bool __fastcall ApplicationHook(TMessage & Message);

protected:
  void __fastcall CancelOperation();
  void __fastcall UpdateControls();
  void __fastcall ResetOnceDoneOperation();
  bool __fastcall ReceiveData(bool Force, int ModalLevelOffset);
  void __fastcall Minimize(TObject * Sender);
  virtual void __fastcall Dispatch(void * Message);

  static bool __fastcall IsIndeterminateOperation(TFileOperation Operation);

public:
  static UnicodeString __fastcall ProgressStr(TFileOperationProgressType * ProgressData);

  virtual __fastcall TProgressForm(TComponent * AOwner, bool AllowMoveToQueue);
  virtual __fastcall ~TProgressForm();
  void __fastcall SetProgressData(TFileOperationProgressType & AData);
  __property TCancelStatus Cancel = { read = FCancel };
  __property bool MoveToQueue = { read = FMoveToQueue };
  __property TOnceDoneOperation OnceDoneOperation = { read=GetOnceDoneOperation, write=SetOnceDoneOperation };
  __property bool AllowMinimize = { read=GetAllowMinimize, write=SetAllowMinimize };
  __property bool DeleteToRecycleBin = { read=FDeleteToRecycleBin, write=FDeleteToRecycleBin };
  __property bool ReadOnly = { read=FReadOnly, write=SetReadOnly };
};
//----------------------------------------------------------------------------
#endif
