//----------------------------------------------------------------------------
#ifndef SynchronizeH
#define SynchronizeH
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
#include <MoreButton.hpp>

#include <WinInterface.h>

#include "CopyParams.h"
//----------------------------------------------------------------------------
class TSynchronizeDialog : public TForm
{
__published:
  TButton *StartButton;
  TButton *StopButton;
  TButton *CloseButton;
  TLabel *StatusLabel;
  TPanel *MorePanel;
  TCopyParamsFrame *CopyParamsFrame;
  TCheckBox *SaveSettingsCheck;
  TMoreButton *MoreButton;
  TButton *MinimizeButton;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall StartButtonClick(TObject *Sender);
  void __fastcall StopButtonClick(TObject *Sender);
  void __fastcall MinimizeButtonClick(TObject *Sender);
private:
  TSynchronizeStartStopEvent FOnStartStop;
  Boolean FSynchronizing;
  TSynchronizeParamType FParams;
  Boolean FWasExpanded;
  Boolean FMinimizedByMe;
  void __fastcall SetParams(TSynchronizeParamType value);
  TSynchronizeParamType __fastcall GetParams();
protected:
  void __fastcall UpdateControls();
  void __fastcall Validate();
  virtual void __fastcall DoStartStop(Boolean Start, TSynchronizeParamType Params);
  void __fastcall MinimizeApp();
public:
	virtual __fastcall TSynchronizeDialog(TComponent* AOwner);
  bool __fastcall Execute();
  void __fastcall Stop();
  __property TSynchronizeParamType Params = { read = GetParams, write = SetParams };
  __property TSynchronizeStartStopEvent OnStartStop  = { read=FOnStartStop, write=FOnStartStop };
};
//----------------------------------------------------------------------------
#endif
