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
  __fastcall TSynchronizeProgressForm(TComponent* Owner);
  virtual __fastcall ~TSynchronizeProgressForm();

  void __fastcall Start();
  void __fastcall Stop();
  void __fastcall SetData(const AnsiString LocalDirectory,
    const AnsiString RemoteDirectory, bool & Continue);

  __property bool Started = { read = FStarted };

private:
  TDateTime FStartTime;
  TDateTime FElapsed;
  bool FStarted;
  bool FCanceled;
  void * FShowAsModalStorage;
  bool FMinimizedByMe;

  void __fastcall UpdateControls();
};
//---------------------------------------------------------------------------
#endif
