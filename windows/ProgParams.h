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
  bool __fastcall FindSwitch(const AnsiString Switch);
  bool __fastcall FindSwitch(const AnsiString Switch, AnsiString & Value);
  bool __fastcall FindSwitch(const AnsiString Switch, AnsiString & Value, int & Position);
  AnsiString __fastcall SwitchValue(const AnsiString Switch, const AnsiString Default = "");
  __property AnsiString SwitchValueDelimiters  = { read=FSwitchValueDelimiters, write=FSwitchValueDelimiters };
  __property bool IgnoreCase  = { read=FIgnoreCase, write=FIgnoreCase };
  __property Integer ParamCount  = { read=GetParamCount };
  __property Integer Count  = { read=GetCount };
  __property AnsiString Param[Integer Index]  = { read=GetParam };
  __property AnsiString SwitchMarks  = { read=FSwitchMarks, write=FSwitchMarks };
protected:
  TParamType __fastcall ParamType(Integer Index, AnsiString & Value);
private:
  AnsiString FSwitchMarks;
  AnsiString FSwitchValueDelimiters;

  bool FIgnoreCase;
  Integer FParamCount;
  Integer __fastcall GetParamCount();
  Integer __fastcall GetCount();
  AnsiString __fastcall GetParam(Integer Index);
};
//---------------------------------------------------------------------------
#endif
