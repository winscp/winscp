//---------------------------------------------------------------------------
#ifndef SynchronizeH
#define SynchronizeH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <HistoryComboBox.h>
#include "GrayedCheckBox.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Vcl.Menus.hpp>
//---------------------------------------------------------------------------
#include <WinInterface.h>
#include <GUITools.h>
//---------------------------------------------------------------------------
struct TLogItemData;
//---------------------------------------------------------------------------
class TSynchronizeDialog : public TForm
{
__published:
  TGroupBox *DirectoriesGroup;
  TButton *StopButton;
  TButton *CancelButton;
  TLabel *LocalDirectoryLabel;
  TLabel *RemoteDirectoryLabel;
  THistoryComboBox *RemoteDirectoryEdit;
  THistoryComboBox *LocalDirectoryEdit;
  TGroupBox *OptionsGroup;
  TCheckBox *SynchronizeDeleteCheck;
  TButton *LocalDirectoryBrowseButton;
  TCheckBox *SaveSettingsCheck;
  TCheckBox *SynchronizeExistingOnlyCheck;
  TButton *StartButton;
  TButton *MinimizeButton;
  TButton *TransferSettingsButton;
  TCheckBox *SynchronizeRecursiveCheck;
  TGrayedCheckBox *SynchronizeSynchronizeCheck;
  TGroupBox *CopyParamGroup;
  TLabel *CopyParamLabel;
  TButton *HelpButton;
  TCheckBox *SynchronizeSelectedOnlyCheck;
  TPanel *LogPanel;
  TListView *LogView;
  TImage *Image;
  TCheckBox *ContinueOnErrorCheck;
  TPopupMenu *MinimizeMenu;
  TMenuItem *Minimize1;
  TMenuItem *MinimizetoTray1;
  TPopupMenu *StartMenu;
  TMenuItem *Start1;
  TMenuItem *StartInNewWindowItem;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall LocalDirectoryBrowseButtonClick(TObject *Sender);
  void __fastcall TransferSettingsButtonClick(TObject *Sender);
  void __fastcall StartButtonClick(TObject *Sender);
  void __fastcall StopButtonClick(TObject *Sender);
  void __fastcall MinimizeButtonClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall CopyParamGroupContextPopup(TObject *Sender,
          TPoint &MousePos, bool &Handled);
  void __fastcall CopyParamGroupClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall LogViewKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
  void __fastcall FormKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
  void __fastcall TransferSettingsButtonDropDownClick(TObject *Sender);
  void __fastcall LogViewCustomDrawItem(TCustomListView *Sender, TListItem *Item,
          TCustomDrawState State, bool &DefaultDraw);
  void __fastcall LogViewDeletion(TObject *Sender, TListItem *Item);
  void __fastcall LogViewDblClick(TObject *Sender);
  void __fastcall Minimize1Click(TObject *Sender);
  void __fastcall MinimizetoTray1Click(TObject *Sender);
  void __fastcall MinimizeButtonDropDownClick(TObject *Sender);
  void __fastcall StartInNewWindowItemClick(TObject *Sender);
  void __fastcall StartButtonDropDownClick(TObject *Sender);

private:
  TSynchronizeParamType FParams;
  TSynchronizeStartStopEvent FOnStartStop;
  TGetSynchronizeOptionsEvent FOnGetOptions;
  TSynchronizeSessionLog FOnSynchronizeSessionLog;
  int FOptions;
  int FCopyParamAttrs;
  bool FSynchronizing;
  bool FMinimizedByMe;
  bool FAbort;
  bool FClose;
  bool FStartImmediately;
  TCopyParamType FCopyParams;
  TPopupMenu * FPresetsMenu;
  UnicodeString FPreset;
  TSynchronizeOptions * FSynchronizeOptions;
  TFeedSynchronizeError * FOnFeedSynchronizeError;
  TSynchronizeInNewWindow FOnSynchronizeInNewWindow;
  static const int MaxLogItems;

  void __fastcall SetParams(const TSynchronizeParamType& value);
  TSynchronizeParamType __fastcall GetParams();
  void __fastcall SetSaveSettings(bool value);
  bool __fastcall GetSaveSettings();
  void __fastcall SetCopyParams(const TCopyParamType & value);
  TCopyParamType __fastcall GetCopyParams();
  void __fastcall SetOptions(int value);

protected:
  void __fastcall DoStartStop(bool Start, bool Synchronize);
  void __fastcall DoAbort(TObject * Sender, bool Close);
  void __fastcall DoLogInternal(TSynchronizeLogEntry Entry, const UnicodeString & Message,
    TStrings * MoreMessages, TQueryType Type, const UnicodeString & HelpKeyword);
  void __fastcall DoLog(TSynchronizeController * Controller, TSynchronizeLogEntry Entry,
    const UnicodeString Message);
  void __fastcall OnlyStop();
  void __fastcall Stop();
  virtual void __fastcall Dispatch(void * Message);
  void __fastcall CopyParamClick(TObject * Sender);
  void __fastcall ClearLog();
  void __fastcall CopyLog();
  int __fastcall ActualCopyParamAttrs();
  void __fastcall GlobalMinimize(TObject * Sender);
  void __fastcall CopyParamListPopup(TRect R, int AdditionalOptions);
  void __fastcall FeedSynchronizeError(
    const UnicodeString & Message, TStrings * MoreMessages, TQueryType Type,
    const UnicodeString & HelpKeyword);
  void __fastcall SynchronizeAbort(TObject *);
  TLogItemData * __fastcall GetLogItemData(TListItem * Item);
  void __fastcall Minimize(TObject * Sender);
  void __fastcall Start();
  void __fastcall StartInNewWindow();
  void __fastcall SaveHistory();
  void __fastcall UpdateControls();
  bool __fastcall AllowStartInNewWindow();
  bool __fastcall CanStartInNewWindow();
  void Abort(bool Close);

  INTERFACE_HOOK

public:
  __fastcall TSynchronizeDialog(TComponent * Owner);
  void __fastcall Init(TSynchronizeStartStopEvent OnStartStop,
    TGetSynchronizeOptionsEvent OnGetOptions,
    TSynchronizeSessionLog OnSynchronizeSessionLog,
    TFeedSynchronizeError & OnFeedSynchronizeError,
    TNotifyEvent & OnSynchronizeAbort,
    TSynchronizeInNewWindow OnSynchronizeInNewWindow,
    int AutoSubmit);
  virtual __fastcall ~TSynchronizeDialog();

  bool __fastcall Execute();

  __property TSynchronizeParamType Params = { read = GetParams, write = SetParams };
  __property bool SaveSettings = { read = GetSaveSettings, write = SetSaveSettings };
  __property int Options = { read = FOptions, write = SetOptions };
  __property int CopyParamAttrs = { read = FCopyParamAttrs, write = FCopyParamAttrs };
  __property TCopyParamType CopyParams = { read = GetCopyParams, write = SetCopyParams };
};
//---------------------------------------------------------------------------
#endif
