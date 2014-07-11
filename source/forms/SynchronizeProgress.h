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
  TButton *CancelButton;
  TTimer *UpdateTimer;
  TButton *MinimizeButton;
  void __fastcall CancelButtonClick(TObject *Sender);
  void __fastcall UpdateTimerTimer(TObject *Sender);
  void __fastcall MinimizeButtonClick(TObject *Sender);

public:
  __fastcall TSynchronizeProgressForm(TComponent * Owner, bool AllowMinimize,
    bool CompareOnly);
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
  bool FCompareOnly;

  void __fastcall UpdateControls();
  void __fastcall GlobalMinimize(TObject * Sender);
  void __fastcall CancelOperation();
};
//---------------------------------------------------------------------------
#endif
