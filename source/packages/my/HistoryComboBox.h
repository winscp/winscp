//---------------------------------------------------------------------------
#ifndef HistoryComboBoxH
#define HistoryComboBoxH
//---------------------------------------------------------------------------
#include <Vcl.StdCtrls.hpp>
//---------------------------------------------------------------------------
class TUIStateAwareComboBox : public TComboBox
{
private:
  bool FDarkMode;
  void SetDarkMode(bool Value);

protected:
  virtual void __fastcall ComboWndProc(TMessage & Message, HWND ComboWnd, void * ComboProc);
  virtual void __fastcall CreateWnd();

public:
  __fastcall TUIStateAwareComboBox(TComponent * AOwner);

  __property bool DarkMode = { read = FDarkMode, write = SetDarkMode };
};
//---------------------------------------------------------------------------
enum THistorySaveOnEnum { soExit, soDropDown };
typedef Set<THistorySaveOnEnum, soExit, soDropDown> THistorySaveOn;
//---------------------------------------------------------------------------
class THistoryComboBox;
typedef void __fastcall (__closure *THistoryComboBoxGetData)(THistoryComboBox * Sender, TObject * &Data);
typedef void __fastcall (__closure *THistoryComboBoxSetData)(THistoryComboBox * Sender, TObject * Data);
typedef void __fastcall (__closure *THistoryComboHistoryEvent)(THistoryComboBox * Sender);
//---------------------------------------------------------------------------
#define DefaultHistorySaveOn ((1 << soExit) + (1 << soDropDown))
const int DefaultMaxHistorySize = 30;
const int DefaultHistoryDropDownCount = 16;
const int DefaultHistoryAutoComplete = false;
//---------------------------------------------------------------------------
class THistoryComboBox : public TUIStateAwareComboBox
{
private:
  THistorySaveOn FSaveOn;
  int FMaxHistorySize;
  THistoryComboBoxGetData FOnGetData;
  THistoryComboBoxSetData FOnSetData;
  UnicodeString FHistoryKey;

  void __fastcall SetMaxHistorySize(int AMaxHistorySize);
  void __fastcall SetHistoryKey(UnicodeString value);
  int GetMaxItemWidth();
  void LoadHistory();
  void AddToHistory();

protected:
  DYNAMIC void __fastcall DoExit();
  DYNAMIC void __fastcall DropDown();
  DYNAMIC void __fastcall KeyDown(Word & Key, TShiftState Shift);
  DYNAMIC void __fastcall Change();
  virtual void __fastcall CreateWnd();

public:
  __fastcall THistoryComboBox(TComponent * AOwner);
  void SaveToHistory();

  static THistoryComboHistoryEvent OnLoadHistory;
  static THistoryComboHistoryEvent OnSaveHistory;

__published:
  __property THistorySaveOn SaveOn = { read = FSaveOn, write = FSaveOn, default = 3 };
  // There's a redundant limit in TCustomWinConfiguration::SetHistory
  __property int MaxHistorySize = { read = FMaxHistorySize, write = SetMaxHistorySize, default = 30 };
  __property THistoryComboBoxGetData OnGetData = { read = FOnGetData, write = FOnGetData };
  __property THistoryComboBoxSetData OnSetData = { read = FOnSetData, write = FOnSetData };
  __property UnicodeString HistoryKey = { read = FHistoryKey, write = SetHistoryKey };
  __property DropDownCount = { default = DefaultHistoryDropDownCount };
  __property AutoComplete = { default = DefaultHistoryAutoComplete };
};
//---------------------------------------------------------------------------
void SaveToHistory(TStrings * Strings, const UnicodeString & T, TObject * Data = nullptr, int MaxHistorySize = DefaultMaxHistorySize);
//---------------------------------------------------------------------------
#endif  // HistoryComboBoxH
