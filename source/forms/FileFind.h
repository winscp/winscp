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
#include <System.Actions.hpp>
#include <Vcl.ActnList.hpp>
#include <Vcl.Menus.hpp>
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
  TButton *DeleteButton;
  TPopupMenu *FileViewPopupMenu;
  TMenuItem *N2;
  TMenuItem *SelectAllItem;
  TActionList *ActionList;
  TAction *SelectAllAction;
  TAction *DeleteAction;
  TAction *FocusAction;
  TAction *CopyAction;
  TMenuItem *Focus1;
  TMenuItem *Delete1;
  TMenuItem *N1;
  TMenuItem *N3;
  TMenuItem *CopyResults1;
  TButton *DownloadButton;
  TAction *DownloadAction;
  TMenuItem *Download1;
  TAction *EditAction;
  TButton *EditButton;
  TMenuItem *Edit1;
  TPanel *FileViewPanel;
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
  void __fastcall FileViewSelectItem(TObject *Sender, TListItem *Item,
          bool Selected);
  void __fastcall MaskButtonClick(TObject *Sender);
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
  void __fastcall FileViewContextPopup(TObject *Sender, TPoint &MousePos, bool &Handled);
  void __fastcall DeleteActionExecute(TObject *Sender);
  void __fastcall CopyActionExecute(TObject *Sender);
  void __fastcall FocusActionExecute(TObject *Sender);
  void __fastcall SelectAllActionExecute(TObject *Sender);
  void __fastcall DownloadActionExecute(TObject *Sender);
  void __fastcall EditActionExecute(TObject *Sender);
  void __fastcall FileViewCompare(TObject *Sender, TListItem *Item1, TListItem *Item2, int Data, int &Compare);
  void __fastcall FormAfterMonitorDpiChanged(TObject *Sender, int OldDPI, int NewDPI);

public:
  __fastcall TFileFindDialog(TComponent * Owner);
  virtual __fastcall ~TFileFindDialog();

  void __fastcall Init(
    TTerminal * Terminal, UnicodeString Directory, TFindEvent OnFind, TFocusFileEvent OnFocusFile,
    TFileListOperationEvent OnDeleteFiles, TFileListOperationEvent OnDownloadFiles,
    TFileListOperationEvent OnEditFiles);

protected:
  void __fastcall Clear();
  void __fastcall Start();
  void __fastcall Stop();
  bool __fastcall StopIfFinding();
  void __fastcall GlobalMinimize(TObject * Sender);
  void __fastcall UpdateControls();
  bool __fastcall IsFinding();
  void __fastcall UpdateImages();

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
  TFileListOperationEvent FOnDeleteFiles;
  TFileListOperationEvent FOnDownloadFiles;
  TFileListOperationEvent FOnEditFiles;
  TFrameAnimation FFrameAnimation;
  UnicodeString FFocusPath;
  typedef std::map<UnicodeString, TListItem *> TFileItemMap;
  TFileItemMap FFileItemMap;
  bool FClosePending;

  void __fastcall FileFound(TTerminal * Terminal,
    const UnicodeString FileName, const TRemoteFile * File, bool & Cancel);
  void __fastcall FindingFile(TTerminal * Terminal, const UnicodeString Directory,
    bool & Cancel);
  void __fastcall CopyToClipboard();
  void __fastcall FocusFile();
  void __fastcall DoFocusFile(const UnicodeString & Path);
  void __fastcall CMDialogKey(TWMKeyDown & Message);
  void __fastcall ClearItem(TListItem * Item);
  void __fastcall FileDeleteFinished(TOperationSide Side, const UnicodeString & FileName, bool Success, bool NotCancelled);
  void __fastcall FileDownloadFinished(TOperationSide Side, const UnicodeString & FileName, bool Success, bool NotCancelled);
  TListItem * __fastcall FileOperationFinished(const UnicodeString & FileName);
  void __fastcall FileListOperation(TFileListOperationEvent Operation, TFileOperationFinishedEvent OnFileOperationFinished);
  TIEListViewColProperties * GetColProperties();
  int FilesCompare(const TRemoteFile * File1, const TRemoteFile * File2);

  INTERFACE_HOOK;
};
//---------------------------------------------------------------------------
#endif
