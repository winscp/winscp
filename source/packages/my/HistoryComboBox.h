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
//---------------------------------------------------------------------------
#define DefaultHistorySaveOn ((1 << soExit) + (1 << soDropDown))
const int DefaultMaxHistorySize = 30;
//---------------------------------------------------------------------------
class THistoryComboBox : public TUIStateAwareComboBox
{
private:
  THistorySaveOn FSaveOn;
  int FMaxHistorySize;
  THistoryComboBoxGetData FOnGetData;
  THistoryComboBoxSetData FOnSetData;
  void __fastcall SetMaxHistorySize(int AMaxHistorySize);
  int GetMaxItemWidth();

protected:
  DYNAMIC void __fastcall DoExit();
  DYNAMIC void __fastcall DropDown();
  DYNAMIC void __fastcall KeyDown(Word & Key, TShiftState Shift);
  DYNAMIC void __fastcall Change();

public:
  __fastcall THistoryComboBox(TComponent * AOwner);
  void SaveToHistory();

__published:
  __property THistorySaveOn SaveOn = { read = FSaveOn, write = FSaveOn, default = 3 };
  __property int MaxHistorySize = { read = FMaxHistorySize, write = SetMaxHistorySize, default = 30 };
  __property THistoryComboBoxGetData OnGetData = { read = FOnGetData, write = FOnGetData };
  __property THistoryComboBoxSetData OnSetData = { read = FOnSetData, write = FOnSetData };
};
//---------------------------------------------------------------------------
void SaveToHistory(TStrings * Strings, const UnicodeString & T, TObject * Data = nullptr, int MaxHistorySize = DefaultMaxHistorySize);
//---------------------------------------------------------------------------
#endif  // HistoryComboBoxH
