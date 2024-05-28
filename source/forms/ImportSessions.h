//----------------------------------------------------------------------------
#ifndef ImportSessionsH
#define ImportSessionsH
//----------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------
#include <SessionData.h>
#include <GUITools.h>
//---------------------------------------------------------------------
class TImportSessionsDialog : public TForm
{
__published:
  TButton *OKButton;
  TButton *CancelButton;
  TListView *SessionListView2;
  TLabel *Label;
  TButton *CheckAllButton;
  TButton *HelpButton;
  TComboBox *SourceComboBox;
  TPanel *ErrorPanel;
  TLabel *ErrorLabel;
  TButton *PasteButton;
  TButton *BrowseButton;
  void __fastcall SessionListView2InfoTip(TObject *Sender,
    TListItem *Item, UnicodeString &InfoTip);
  void __fastcall SessionListView2MouseDown(TObject *Sender,
    TMouseButton Button, TShiftState Shift, int X, int Y);
  void __fastcall SessionListView2KeyUp(TObject *Sender, WORD &Key,
    TShiftState Shift);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall CheckAllButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall SourceComboBoxSelect(TObject *Sender);
  void __fastcall PasteButtonClick(TObject *Sender);
  void __fastcall BrowseButtonClick(TObject *Sender);

private:
  TList * FSessionListsList;
  TStrings * FErrors;
  std::unique_ptr<TStoredSessionList> FPastedKnownHosts;
  std::unique_ptr<TStoredSessionList> FIniImportSessionList;
  UnicodeString FIniFileName;
  void __fastcall UpdateControls();
  void __fastcall LoadSessions();
  void __fastcall ClearSelections();
  void __fastcall SaveSelection();
  TStoredSessionList * __fastcall GetSessionList(int Index);
  TSessionData * GetSessionData(TListItem * Item);
  bool ConvertKeyFile(UnicodeString & KeyFile, TStrings * ConvertedKeyFiles, TStrings * NotConvertedKeyFiles);
  virtual void __fastcall CreateHandle();
  virtual void __fastcall DestroyHandle();
  virtual void __fastcall Dispatch(void * Message);

  INTERFACE_HOOK;

public:
  virtual __fastcall TImportSessionsDialog(TComponent * AOwner);
  void __fastcall Init(TList * SessionListsList, TStrings * Errors);
  TStoredSessionList * SelectSessionsForImport(UnicodeString & Error);
  bool __fastcall Execute();
  __property UnicodeString IniFileName = { read = FIniFileName };
};
//----------------------------------------------------------------------------
#endif
