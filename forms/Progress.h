//----------------------------------------------------------------------------
#ifndef ProgressH
#define ProgressH
//----------------------------------------------------------------------------
#include <vcl\System.hpp>
#include <vcl\Windows.hpp>
#include <vcl\SysUtils.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Graphics.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Buttons.hpp>
#include <vcl\ExtCtrls.hpp>
#include <ComCtrls.hpp>
#include <PathLabel.hpp>

#include <FileOperationProgress.h>
//----------------------------------------------------------------------------
class TProgressForm : public TForm
{
__published:
  TAnimate *Animate;
  TButton *CancelButton;
  TButton *MinimizeButton;
  TPanel *MainPanel;
  TLabel *Label1;
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
  TLabel *BytesTransferedLabel;
  TLabel *Label7;
  TLabel *CPSLabel;
  TProgressBar *FileProgress;
  TTimer *UpdateTimer;
  TCheckBox *DisconnectWhenCompleteCheck;
  TLabel *Label10;
  TLabel *TransferModeLabel;
  TLabel *Label11;
  TLabel *ResumeLabel;
  TPanel *SpeedPanel;
  TLabel *SpeedLabel;
  TTrackBar *SpeedBar;
  TLabel *SpeedLowLabel;
  TLabel *SpeedHighLabel;
  TLabel *TimeLeftLabelLabel;
  TLabel *TimeLeftLabel;
  void __fastcall UpdateTimerTimer(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormHide(TObject *Sender);
  void __fastcall CancelButtonClick(TObject *Sender);
  void __fastcall MinimizeButtonClick(TObject *Sender);
private:
  TCancelStatus FCancel;
  TFileOperationProgressType FData;
  bool FDataReceived;
  TFileOperation FLastOperation;
  bool FLastTotalSizeSet;
  bool FMinimizedByMe;
  int FUpdateCounter;
  bool FAsciiTransferChanged;
  bool FResumeStatusChanged;
  void * FShowAsModalStorage;
  TDateTime FLastUpdate;
  bool FDeleteToRecycleBin;
  bool FReadOnly;

  void __fastcall SetDisconnectWhenComplete(bool value);
  bool __fastcall GetDisconnectWhenComplete();
  void __fastcall SetAllowMinimize(bool value);
  bool __fastcall GetAllowMinimize();
  void __fastcall SetReadOnly(bool value);

protected:
  void __fastcall CancelOperation();
  void __fastcall MinimizeApp();
  void __fastcall UpdateControls();

public:
  static AnsiString __fastcall OperationName(TFileOperation Operation);

  virtual __fastcall ~TProgressForm();
  void __fastcall SetProgressData(const TFileOperationProgressType & AData);
  virtual __fastcall TProgressForm(TComponent * AOwner);
  __property TCancelStatus Cancel = { read = FCancel };
  __property bool DisconnectWhenComplete = { read=GetDisconnectWhenComplete, write=SetDisconnectWhenComplete };
  __property bool AllowMinimize = { read=GetAllowMinimize, write=SetAllowMinimize };
  __property bool DeleteToRecycleBin = { read=FDeleteToRecycleBin, write=FDeleteToRecycleBin };
  __property bool ReadOnly = { read=FReadOnly, write=SetReadOnly };
};
//----------------------------------------------------------------------------
#endif
