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
//---------------------------------------------------------------------------
class TSynchronizeProgressForm : public TForm
{
__published:
  TLabel *Label1;
  TLabel *Label2;
  TPathLabel *RemoteDirectoryLabel;
  TPathLabel *LocalDirectoryLabel;
  TLabel *StartTimeLabel;
  TLabel *Label4;
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
  void __fastcall UpdateTimerTimer(TObject *Sender);
  void __fastcall MinimizeItemClick(TObject *Sender);
  void __fastcall CancelItemClick(TObject *Sender);

public:
  __fastcall TSynchronizeProgressForm(TComponent * Owner, bool AllowMinimize);
  virtual __fastcall ~TSynchronizeProgressForm();

  void __fastcall Start();
  void __fastcall SetData(const UnicodeString LocalDirectory,
    const UnicodeString RemoteDirectory, bool & Continue);

  __property bool Started = { read = FStarted };

protected:
  virtual void __fastcall Dispatch(void * Message);

private:
  TDateTime FStartTime;
  TDateTime FElapsed;
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
