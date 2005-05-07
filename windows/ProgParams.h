//---------------------------------------------------------------------------
#ifndef ProgParamsH
#define ProgParamsH
//---------------------------------------------------------------------------
enum TParamType {ptParam, ptSwitch};
//---------------------------------------------------------------------------
class TProgramParams 
{
public:
  TProgramParams();
  ~TProgramParams();
  bool __fastcall FindSwitch(const AnsiString Switch);
  bool __fastcall FindSwitch(const AnsiString Switch, AnsiString & Value);
  bool __fastcall FindSwitch(const AnsiString Switch, int & ParamsStart,
    int & ParamsCount);
  bool __fastcall FindSwitch(const AnsiString Switch, TStrings * Params,
    int ParamsMax = -1);
  void __fastcall ParamsProcessed(int Position, int Count);
  AnsiString __fastcall SwitchValue(const AnsiString Switch, const AnsiString Default = "");
  __property AnsiString SwitchValueDelimiters  = { read=FSwitchValueDelimiters, write=FSwitchValueDelimiters };
  __property bool IgnoreCase  = { read=FIgnoreCase, write=FIgnoreCase };
  __property Integer ParamCount  = { read=GetParamTypeCount, index=ptParam };
  __property Integer SwitchCount  = { read=GetParamTypeCount, index=ptSwitch };
  __property Integer Count  = { read=GetCount };
  __property AnsiString Param[Integer Index]  = { read=GetParam };
  __property AnsiString SwitchMarks  = { read=FSwitchMarks, write=FSwitchMarks };
protected:
  TParamType __fastcall ParamType(Integer Index, AnsiString & Value);
  bool __fastcall FindSwitch(const AnsiString Switch,
    AnsiString & Value, int & ParamsStart, int & ParamsCount);
private:
  AnsiString FSwitchMarks;
  AnsiString FSwitchValueDelimiters;
  TStrings * FParameters; 

  bool FIgnoreCase;
  Integer FParamCount[2];
  Integer __fastcall GetParamTypeCount(int Type);
  Integer __fastcall GetCount();
  AnsiString __fastcall GetParam(Integer Index);
  void __fastcall ResetParamCount();
};
//---------------------------------------------------------------------------
#endif
