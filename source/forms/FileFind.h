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
#include <GUITools.h>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include "IEListView.hpp"
#include "NortonLikeListView.hpp"
#include <Vcl.Imaging.pngimage.hpp>
#include <Vcl.ImgList.hpp>
#include "PngImageList.hpp"
//---------------------------------------------------------------------------
class TFileFindDialog : public TForm
{
__published:
  TGroupBox *FilterGroup;
  TLabel *MaskLabel;
  TLabel *RemoteDirectoryLabel;
  THistoryComboBox *RemoteDirectoryEdit;
  THistoryComboBox *MaskEdit;
  TButton *StartStopButton;
  TButton *HelpButton;
  TIEListView *FileView;
  TStatusBar *StatusBar;
  TButton *FocusButton;
  TStaticText *MaskHintText;
  TButton *MaskButton;
  TPaintBox *AnimationPaintBox;
  TButton *CopyButton;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall StartStopButtonClick(TObject *Sender);
  void __fastcall StopButtonClick(TObject *Sender);
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
  void __fastcall CopyButtonClick(TObject *Sender);
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);

public:
  __fastcall TFileFindDialog(TComponent * Owner);
  virtual __fastcall ~TFileFindDialog();

  void __fastcall Init(
    TTerminal * Terminal, UnicodeString Directory, TFindEvent OnFind, TFocusFileEvent OnFocusFile);

protected:
  void __fastcall Clear();
  void __fastcall Start();
  void __fastcall Stop();
  bool __fastcall StopIfFinding();
  void __fastcall GlobalMinimize(TObject * Sender);
  void __fastcall UpdateControls();
  bool __fastcall IsFinding();

  virtual void __fastcall CreateParams(TCreateParams & Params);
  virtual void __fastcall Dispatch(void * Message);

private:
  enum { ffInit, ffFinding, ffAborting, ffAborted, ffDone } FState;
  bool FMinimizedByMe;
  TTerminal * FTerminal;
  UnicodeString FTerminalName;
  UnicodeString FFindingInDirectory;
  UnicodeString FDirectory;
  UnicodeString FWindowParams;
  TFindEvent FOnFind;
  TFocusFileEvent FOnFocusFile;
  TImageList * FSystemImageList;
  TFrameAnimation FFrameAnimation;
  UnicodeString FFocusPath;

  void __fastcall FileFound(TTerminal * Terminal,
    const UnicodeString FileName, const TRemoteFile * File, bool & Cancel);
  void __fastcall FindingFile(TTerminal * Terminal, const UnicodeString Directory,
    bool & Cancel);
  void __fastcall CopyToClipboard();
  void __fastcall FocusFile();
  void __fastcall DoFocusFile(const UnicodeString & Path);
  void __fastcall CMDialogKey(TWMKeyDown & Message);
};
//---------------------------------------------------------------------------
#endif
