//---------------------------------------------------------------------------
#ifndef LogMemoH
#define LogMemoH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <StdCtrls.hpp>
#include <ComCtrls.hpp>

#define WM_WINSCP_USER   (WM_USER + 0x2000)
#define WM_LOG_UPDATE    (WM_WINSCP_USER + 6)

#ifndef DESIGN_ONLY
#include <SessionInfo.h>
#else
enum TLogLineType {llOutput, llInput, llStdError, llMessage, llException};
#endif
typedef Set<TLogLineType, llOutput, llException> TLogLineTypes;
//---------------------------------------------------------------------------
#define DEFAULT_LOGMEMO_FONT "Courier New"
#define DEFAULT_LOGMEMO_SHOWTYPES (TLogLineTypes() << llOutput << llInput << \
  llStdError << llMessage << llException)
class PACKAGE TLogMemo : public TCustomRichEdit
{
private:
#ifndef DESIGN_ONLY
  TSessionLog * FSessionLog;
#endif
  TLogLineTypes FShowTypes;
  TList *FIndexes;
  bool FUpdating;
  bool FWantScrollToEnd;
  bool FNeedsRepaint;
  unsigned int FLastUpdate;

  void __fastcall CMShowingChanged(TMessage & Message);
  void CMVisibleChanged(TMessage & Message);
  void WMLogUpdate(TMessage & Message);
  int __fastcall GetIndexes(int Index);
  int __fastcall GetLinesVisible();
  bool __fastcall IsFontStored();
#ifndef DESIGN_ONLY
  void __fastcall SetSessionLog(TSessionLog * value);
#endif
  void __fastcall SetShowTypes(TLogLineTypes value);
  bool __fastcall StoreShowTypes();
  void __fastcall WMPaint(TWMPaint & Message);
  MESSAGE void __fastcall WMSetFocus(TWMSetFocus & Message);
protected:
  DYNAMIC void __fastcall Change();
  DYNAMIC void __fastcall KeyDown(Word & Key, TShiftState Shift);
  DYNAMIC void __fastcall MouseDown(TMouseButton Button, TShiftState Shift, int X, int Y);
  void __fastcall ReloadFromLog();
  void __fastcall ScrollToEnd();
  void __fastcall SessionLogChange(TObject * Sender);
  virtual void __fastcall SetParent(TWinControl * AParent);
  void __fastcall UpdateFromLog();
  void __fastcall WMKeyDown(TWMKeyDown & Message);
  __property int Indexes[Integer Index] = { read = GetIndexes };

  #pragma warn -inl
  virtual void __fastcall Dispatch(void *Message) {
    if (Parent)
    switch (((PMessage)Message)->Msg) {
    VCL_MESSAGE_HANDLER(WM_SETFOCUS, TWMSetFocus, WMSetFocus)
    VCL_MESSAGE_HANDLER(CM_VISIBLECHANGED, TMessage, CMVisibleChanged)
    VCL_MESSAGE_HANDLER(CM_SHOWINGCHANGED, TMessage, CMShowingChanged)
    VCL_MESSAGE_HANDLER(WM_KEYDOWN, TWMKeyDown, WMKeyDown)
    VCL_MESSAGE_HANDLER(WM_PAINT, TWMPaint, WMPaint)
    VCL_MESSAGE_HANDLER(WM_LOG_UPDATE, TMessage, WMLogUpdate)
  END_MESSAGE_MAP(TCustomRichEdit)
  #pragma warn +inl
public:
  virtual __fastcall ~TLogMemo();
  __fastcall TLogMemo(TComponent* Owner);
  __property int LinesVisible = { read = GetLinesVisible };
#ifndef DESIGN_ONLY
  __property TSessionLog * SessionLog = { read = FSessionLog, write = SetSessionLog };
#endif
  __property Lines;
__published:
  __property TLogLineTypes ShowTypes = { read = FShowTypes, write = SetShowTypes,
    stored = StoreShowTypes };

  __property Align;
  __property Alignment;
  __property Anchors;
  __property BiDiMode;
  __property BorderStyle;
  __property BorderWidth;
  __property Color;
  __property Constraints;
  __property Ctl3D;
  __property DragCursor;
  __property DragKind;
  __property DragMode;
  __property Enabled;
  __property Font = { stored = IsFontStored };
  __property HideSelection;
  __property HideScrollBars;
  __property ImeMode;
  __property ImeName;
  __property ParentBiDiMode;
  __property ParentColor = {default = False };
  __property ParentCtl3D;
  __property ParentFont = { default = False };
  __property ParentShowHint;
  __property PopupMenu;
  __property ReadOnly = { default = True };
  __property ScrollBars = { default = ssBoth };
  __property ShowHint;
  __property TabOrder;
  __property TabStop;
  __property Visible;
  __property WantReturns = { default = False };
  __property WantTabs;
  __property WordWrap = { default = False };
  __property OnChange;
  __property OnContextPopup;
  __property OnDragDrop;
  __property OnDragOver;
  __property OnEndDock;
  __property OnEndDrag;
  __property OnEnter;
  __property OnExit;
  __property OnKeyDown;
  __property OnKeyPress;
  __property OnKeyUp;
  __property OnMouseDown;
  __property OnMouseMove;
  __property OnMouseUp;
  __property OnMouseWheel;
  __property OnMouseWheelDown;
  __property OnMouseWheelUp;
  __property OnProtectChange;
  __property OnResizeRequest;
  __property OnSaveClipboard;
  __property OnSelectionChange;
  __property OnStartDock;
  __property OnStartDrag;
};
//---------------------------------------------------------------------------
#endif
