//---------------------------------------------------------------------------
#ifndef OptionH
#define OptionH

#include <vector>
//---------------------------------------------------------------------------
enum TOptionType { otParam, otSwitch };
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TLogOptionEvent)(const UnicodeString & LogStr);
//---------------------------------------------------------------------------
class TOptions
{
public:
  __fastcall TOptions();
  __fastcall TOptions(const TOptions & Source);

  void __fastcall Add(UnicodeString Option);
  void __fastcall Parse(const UnicodeString & CmdLine);

  bool __fastcall FindSwitch(const UnicodeString Switch);
  bool __fastcall FindSwitch(const UnicodeString Switch, UnicodeString & Value);
  bool __fastcall FindSwitch(const UnicodeString Switch, UnicodeString & Value, bool & ValueSet);
  bool __fastcall FindSwitch(const UnicodeString Switch, int & ParamsStart,
    int & ParamsCount);
  bool __fastcall FindSwitch(const UnicodeString Switch, TStrings * Params,
    int ParamsMax = -1);
  bool __fastcall FindSwitchCaseSensitive(const UnicodeString Switch);
  bool __fastcall FindSwitchCaseSensitive(const UnicodeString Switch, TStrings * Params,
    int ParamsMax = -1);
  void __fastcall ParamsProcessed(int Position, int Count);
  UnicodeString __fastcall SwitchValue(const UnicodeString Switch, const UnicodeString Default = L"");
  bool __fastcall SwitchValue(const UnicodeString Switch, bool Default);
  bool __fastcall SwitchValue(const UnicodeString Switch, bool Default, bool DefaultOnNonExistence);
  bool __fastcall UnusedSwitch(UnicodeString & Switch);
  bool __fastcall WasSwitchAdded(UnicodeString & Switch, UnicodeString & Value, wchar_t & SwitchMark);

  void __fastcall LogOptions(TLogOptionEvent OnEnumOption);

  __property int ParamCount = { read = FParamCount };
  __property UnicodeString Param[int Index] = { read = GetParam };
  __property bool Empty = { read = GetEmpty };

protected:
  UnicodeString FSwitchMarks;
  UnicodeString FSwitchValueDelimiters;

  bool __fastcall FindSwitch(const UnicodeString Switch,
    UnicodeString & Value, int & ParamsStart, int & ParamsCount, bool CaseSensitive, bool & ValueSet);
  bool __fastcall DoFindSwitch(const UnicodeString Switch, TStrings * Params,
    int ParamsMax, bool CaseInsensitive);

private:
  struct TOption
  {
    TOptionType Type;
    UnicodeString Name;
    UnicodeString Value;
    bool ValueSet;
    bool Used;
    wchar_t SwitchMark;
  };

  typedef std::vector<TOption> TOptionsVector;
  TOptionsVector FOptions;
  TOptionsVector FOriginalOptions;
  bool FNoMoreSwitches;
  int FParamCount;

  UnicodeString __fastcall GetParam(int Index);
  bool __fastcall GetEmpty();
};
//---------------------------------------------------------------------------
#endif
