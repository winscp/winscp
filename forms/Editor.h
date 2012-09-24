//---------------------------------------------------------------------------
#ifndef EditorH
#define EditorH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ToolWin.hpp>
#include <ActnList.hpp>
#include <ImgList.hpp>
#include <ExtCtrls.hpp>
#include <StdActns.hpp>
#include <Dialogs.hpp>
#include <Menus.hpp>
#include "TB2Dock.hpp"
#include "TBX.hpp"
#include "TB2Item.hpp"
#include "TB2Toolbar.hpp"
#include "TBXStatusBars.hpp"
#include "WinConfiguration.h"
#include "WinInterface.h"
//---------------------------------------------------------------------------
class TRichEdit20;
//---------------------------------------------------------------------------
class TEditorForm : public TForm
{
__published:
  TActionList *EditorActions;
  TImageList *EditorImages;
  TAction *SaveAction;
  TTBXDock *TopDock;
  TTBXToolbar *Toolbar;
  TTBXStatusBar *StatusBar;
  TEditCut *EditCut;
  TEditCopy *EditCopy;
  TEditPaste *EditPaste;
  TEditSelectAll *EditSelectAll;
  TEditUndo *EditUndo;
  TEditDelete *EditDelete;
  TAction *PreferencesAction;
  TAction *CloseAction;
  TAction *FindAction;
  TAction *ReplaceAction;
  TAction *FindNextAction;
  TAction *GoToLineAction;
  TTBXPopupMenu *EditorPopup;
  TTBXItem *Undo1;
  TTBXSeparatorItem *N1;
  TTBXItem *Cut1;
  TTBXItem *Copy1;
  TTBXItem *Paste1;
  TTBXItem *Delete1;
  TTBXSeparatorItem *N2;
  TTBXItem *SelectAll1;
  TTBXSeparatorItem *N3;
  TTBXItem *Find1;
  TTBXItem *Replace1;
  TTBXItem *Findnext1;
  TTBXItem *Gotolinenumber1;
  TAction *HelpAction;
  TTBXSeparatorItem *TBXSeparatorItem6;
  TTBXItem *TBXItem15;
  TAction *ReloadAction;
  TAction *EditRedo;
  TTBXItem *TBXItem18;
  TTBXSubmenuItem *Encoding;
  TTBXItem *DefaultEncoding;
  TTBXItem *UTF8Encoding;
  TAction *DefaultEncodingAction;
  TAction *UTF8EncodingAction;
  void __fastcall EditorActionsUpdate(TBasicAction *Action, bool &Handled);
  void __fastcall EditorActionsExecute(TBasicAction *Action,
          bool &Handled);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall EditorMemoMouseUp(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
  void __fastcall EditorMemoKeyUp(TObject *Sender, WORD &Key,
          TShiftState Shift);
  void __fastcall EditorMemoChange(TObject *Sender);
  void __fastcall FindDialogFind(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
  void __fastcall FormActivate(TObject *Sender);
private:
  UnicodeString FFileName;
  TNotifyEvent FOnFileChanged;
  TNotifyEvent FOnFileReload;
  TFileClosedEvent FOnWindowClose;
  TCustomForm * FParentForm;
  TFindDialog * FLastFindDialog;
  TPoint FCaretPos;
  TFindDialog * FFindDialog;
  TReplaceDialog * FReplaceDialog;
  bool FCloseAnnounced;
  TRichEdit20 * EditorMemo;
  bool FShowStatusBarHint;
  UnicodeString FStatusBarHint;
  bool FFormRestored;
  UnicodeString FWindowParams;
  unsigned int FInstance;
  TEncoding * FEncoding;
  UnicodeString FEncodingName;

  static unsigned int FInstances;
  void __fastcall SetFileName(const UnicodeString value);
  void __fastcall SetParentForm(TCustomForm * value);
  void __fastcall ApplicationHint(TObject * Sender);
public:
  __fastcall TEditorForm(TComponent* Owner);
  virtual __fastcall ~TEditorForm();
  void __fastcall ApplyConfiguration();
  void __fastcall LoadFile();
  __property UnicodeString FileName = { read = FFileName, write = SetFileName };
  __property TNotifyEvent OnFileChanged = { read = FOnFileChanged, write = FOnFileChanged };
  __property TNotifyEvent OnFileReload = { read = FOnFileReload, write = FOnFileReload };
  __property TFileClosedEvent OnWindowClose = { read = FOnWindowClose, write = FOnWindowClose };
  __property TCustomForm * ParentForm = { read = FParentForm, write = SetParentForm };
protected:
  bool __fastcall CursorInUpperPart();
  void __fastcall Find();
  void __fastcall GoToLine();
  void __fastcall PositionFindDialog(bool VerticalOnly);
  void __fastcall StartFind(bool Find);
  void __fastcall UpdateControls();
  void __fastcall DoWindowClose(bool Forced);
  void __fastcall Reload();
  virtual void __fastcall CreateParams(TCreateParams & Params);
  void __fastcall LoadFromFile();
  bool __fastcall ContainsPreamble(TStream * Stream, const TBytes & Signature);
  void __fastcall ChangeEncoding(TEncoding * Encoding);
  void __fastcall InitCodePage();
  UnicodeString __fastcall GetCodePageName(TEncoding * Encoding);
  void __fastcall BackupSave();
};
//---------------------------------------------------------------------------
#endif
