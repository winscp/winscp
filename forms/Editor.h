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
//---------------------------------------------------------------------------
class TEditorForm : public TForm
{
__published:
  TActionList *EditorActions;
  TImageList *EditorImages;
  TAction *SaveAction;
  TCoolBar *TopCoolBar;
  TToolBar *ToolBar;
  TToolButton *ToolButton1;
  TRichEdit *EditorMemo;
  TStatusBar *StatusBar;
  TToolButton *ToolButton2;
  TToolButton *ToolButton3;
  TToolButton *ToolButton4;
  TToolButton *ToolButton5;
  TToolButton *ToolButton6;
  TToolButton *ToolButton7;
  TToolButton *ToolButton8;
  TEditCut *EditCut;
  TEditCopy *EditCopy;
  TEditPaste *EditPaste;
  TEditSelectAll *EditSelectAll;
  TEditUndo *EditUndo;
  TEditDelete *EditDelete;
  TToolButton *ToolButton9;
  TToolButton *ToolButton10;
  TAction *PreferencesAction;
  TToolButton *ToolButton11;
  TAction *CloseAction;
  TToolButton *ToolButton12;
  TAction *FindAction;
  TAction *ReplaceAction;
  TAction *FindNextAction;
  TAction *GoToLineAction;
  TToolButton *ToolButton13;
  TToolButton *ToolButton14;
  TToolButton *ToolButton15;
  TToolButton *ToolButton16;
  TToolButton *ToolButton17;
  TFindDialog *FindDialog;
  TReplaceDialog *ReplaceDialog;
  TPopupMenu *EditorPopup;
  TMenuItem *Undo1;
  TMenuItem *N1;
  TMenuItem *Cut1;
  TMenuItem *Copy1;
  TMenuItem *Paste1;
  TMenuItem *Delete1;
  TMenuItem *N2;
  TMenuItem *SelectAll1;
  TMenuItem *N3;
  TMenuItem *Find1;
  TMenuItem *Replace1;
  TMenuItem *Findnext1;
  TMenuItem *Gotolinenumber1;
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
private:
  AnsiString FFileName;
  TNotifyEvent FOnFileChanged;
  TCustomForm * FParentForm;
  TFindDialog * FLastFindDialog;
  TPoint FCaretPos;
  void __fastcall SetFileName(const AnsiString value);
  void __fastcall SetParentForm(TCustomForm * value);
public:
  __fastcall TEditorForm(TComponent* Owner);
  virtual __fastcall ~TEditorForm();
  bool __fastcall Execute();
  __property AnsiString FileName = { read = FFileName, write = SetFileName };
  __property TNotifyEvent OnFileChanged = { read = FOnFileChanged, write = FOnFileChanged };
  __property TCustomForm * ParentForm = { read = FParentForm, write = SetParentForm };
protected:
  void __fastcall ApplyConfiguration();
  bool __fastcall CursorInUpperPart();
  void __fastcall Find();
  void __fastcall GoToLine();
  void __fastcall LoadFile();
  void __fastcall PositionFindDialog(bool VerticalOnly);
  void __fastcall StartFind(bool Find);
  void __fastcall UpdateControls();
};
//---------------------------------------------------------------------------
#endif
