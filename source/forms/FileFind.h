//---------------------------------------------------------------------------
#ifndef FileFindH
#define FileFindH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <HistoryComboBox.hpp>

#include <WinInterface.h>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include "IEListView.hpp"
#include "NortonLikeListView.hpp"
//---------------------------------------------------------------------------
class TFileFindDialog : public TForm
{
__published:
  TGroupBox *FilterGroup;
  TButton *CancelButton;
  TLabel *MaskLabel;
  TLabel *RemoteDirectoryLabel;
  THistoryComboBox *RemoteDirectoryEdit;
  THistoryComboBox *MaskEdit;
  TButton *StartStopButton;
  TButton *HelpButton;
  TIEListView *FileView;
  TStatusBar *StatusBar;
  TButton *FocusButton;
  TButton *MinimizeButton;
  TStaticText *MaskHintText;
  TButton *MaskButton;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall StartStopButtonClick(TObject *Sender);
  void __fastcall StopButtonClick(TObject *Sender);
  void __fastcall MinimizeButtonClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall FormKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
  void __fastcall MaskEditExit(TObject *Sender);
  void __fastcall FileViewDblClick(TObject *Sender);
  void __fastcall FocusButtonClick(TObject *Sender);
  void __fastcall FileViewSelectItem(TObject *Sender, TListItem *Item,
          bool Selected);
  void __fastcall MaskButtonClick(TObject *Sender);

public:
  __fastcall TFileFindDialog(TComponent * Owner, TFindEvent OnFind);
  virtual __fastcall ~TFileFindDialog();

  bool __fastcall Execute(UnicodeString Directory, UnicodeString & Path);

protected:
  void __fastcall Clear();
  void __fastcall Start();
  void __fastcall Stop();
  bool __fastcall StopIfFinding();
  void __fastcall GlobalMinimize(TObject * Sender);
  void __fastcall UpdateControls();
  bool __fastcall IsFinding();

private:
  enum { ffInit, ffFinding, ffAborting, ffAborted, ffDone } FState;
  bool FMinimizedByMe;
  UnicodeString FFindingInDirectory;
  UnicodeString FDirectory;
  TFindEvent FOnFind;
  TImageList * FSystemImageList;

  void __fastcall FileFound(TTerminal * Terminal,
    const UnicodeString FileName, const TRemoteFile * File, bool & Cancel);
  void __fastcall FindingFile(TTerminal * Terminal, const UnicodeString Directory,
    bool & Cancel);
};
//---------------------------------------------------------------------------
#endif
