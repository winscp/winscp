//---------------------------------------------------------------------------
#ifndef SynchronizeProgressH
#define SynchronizeProgressH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "PathLabel.hpp"
#include <ExtCtrls.hpp>
#include "PngImageList.hpp"
#include "TB2Dock.hpp"
#include "TB2Item.hpp"
#include "TB2Toolbar.hpp"
#include "TBX.hpp"
#include <Vcl.ImgList.hpp>
#include <GUITools.h>
#include <Vcl.ComCtrls.hpp>
#include <System.ImageList.hpp>
//---------------------------------------------------------------------------
class TSynchronizeProgressForm : public TForm
{
__published:
  TLabel *Label1;
  TLabel *Label2;
  TPathLabel *RemoteDirectoryLabel;
  TPathLabel *LocalDirectoryLabel;
  TLabel *StartTimeLabel;
  TLabel *StartTimeLabelLabel;
  TLabel *Label3;
  TLabel *TimeElapsedLabel;
  TTimer *UpdateTimer;
  TPngImageList *ImageList;
  TPanel *ToolbarPanel;
  TTBXDock *Dock;
  TTBXToolbar *Toolbar;
  TTBXItem *CancelItem;
  TTBXItem *MinimizeItem;
  TPaintBox *AnimationPaintBox;
  TPanel *ComponentsPanel;
  TPngImageList *ImageList120;
  TPngImageList *ImageList144;
  TPngImageList *ImageList192;
  TProgressBar *OperationProgress;
  TLabel *TimeLeftLabelLabel;
  TLabel *TimeLeftLabel;
  void __fastcall UpdateTimerTimer(TObject *Sender);
  void __fastcall MinimizeItemClick(TObject *Sender);
  void __fastcall CancelItemClick(TObject *Sender);

public:
  __fastcall TSynchronizeProgressForm(TComponent * Owner, bool AllowMinimize, int Files);
  virtual __fastcall ~TSynchronizeProgressForm();

  void __fastcall Start();
  int __fastcall SetData(
    const UnicodeString & LocalDirectory, const UnicodeString & RemoteDirectory, int Progress, bool & Continue);

  __property bool Started = { read = FStarted };

protected:
  virtual void __fastcall Dispatch(void * Message);
  int __fastcall CalculateProgress();

private:
  TDateTime FStartTime;
  bool FStarted;
  bool FCanceled;
  void * FShowAsModalStorage;
  bool FMinimizedByMe;
  TFrameAnimation FFrameAnimation;

  void __fastcall UpdateControls();
  void __fastcall GlobalMinimize(TObject * Sender);
  void __fastcall CancelOperation();
  void __fastcall CMDialogKey(TCMDialogKey & Message);

  INTERFACE_HOOK;
};
//---------------------------------------------------------------------------
#endif
