//---------------------------------------------------------------------------
#ifndef OptionH
#define OptionH

#include <vector>
//---------------------------------------------------------------------------
enum TOptionType { otParam, otSwitch };
//---------------------------------------------------------------------------
class TOptions
{
public:
  __fastcall TOptions();

  bool __fastcall FindSwitch(const AnsiString Switch);
  bool __fastcall FindSwitch(const AnsiString Switch, AnsiString & Value);
  bool __fastcall FindSwitch(const AnsiString Switch, int & ParamsStart,
    int & ParamsCount);
  bool __fastcall FindSwitch(const AnsiString Switch, TStrings * Params,
    int ParamsMax = -1);
  void __fastcall ParamsProcessed(int Position, int Count);
  AnsiString __fastcall SwitchValue(const AnsiString Switch, const AnsiString Default = "");
  bool __fastcall UnusedSwitch(AnsiString & Switch);

  __property int ParamCount = { read = FParamCount };
  __property AnsiString Param[int Index] = { read = GetParam };
  __property bool Empty = { read = GetEmpty };

protected:
  AnsiString FSwitchMarks;
  AnsiString FSwitchValueDelimiters;

  void __fastcall Add(AnsiString Option);

  bool __fastcall FindSwitch(const AnsiString Switch,
    AnsiString & Value, int & ParamsStart, int & ParamsCount);

private:
  struct TOption
  {
    TOptionType Type;
    AnsiString Name;
    AnsiString Value;
    bool Used;
  };

  std::vector<TOption> FOptions;
  bool FNoMoreSwitches;
  int FParamCount;

  AnsiString __fastcall GetParam(int Index);
  bool __fastcall GetEmpty();
};
//---------------------------------------------------------------------------
#endif
