//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ProgParams.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TProgramParams::TProgramParams()
{
  FSwitchMarks = "-/";
  FSwitchValueDelimiters = ":=";
  FIgnoreCase = true;
  FParamCount = -1;
}
//---------------------------------------------------------------------------
Integer __fastcall TProgramParams::GetCount()
{
  return ::ParamCount();
}
//---------------------------------------------------------------------------
TParamType __fastcall TProgramParams::ParamType(Integer Index, AnsiString & Value)
{
  if (Index < 1 || Index > Count) throw Exception("");
  Value = ParamStr(Index);
  if (!Value.IsEmpty() && (FSwitchMarks.Pos(Value[1]) > 0))
  {
    Value.Delete(1, 1);
    return ptSwitch;
  }
  else return ptParam;
}
//---------------------------------------------------------------------------
Integer __fastcall TProgramParams::GetParamCount()
{
  if (FParamCount < 0)
  {
    FParamCount = 0;
    AnsiString S;
    for (Integer Index = 1; Index <= Count; Index++)
      if (ParamType(Index, S) == ptParam) FParamCount++;
  }
  return FParamCount;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TProgramParams::GetParam(Integer Index)
{
  if (Index < 0 || Index > ParamCount) throw Exception("");
  if (Index == 0) return ParamStr(0);
    else
  {
    AnsiString S;
    Integer P = 1;
    while (Index > 0)
    {
      if (ParamType(P, S) == ptParam) Index--;
      P++;
    }
    return S;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TProgramParams::FindSwitch(const AnsiString Switch,
  AnsiString & Value, int & Position)
{
  Position = 0;
  for (Integer Index = 1; Index <= Count; Index++)
  {
    AnsiString S;
    if (ParamType(Index, S) == ptParam) Position++;
      else
    if (ParamType(Index, S) == ptSwitch)
    {
      bool HasValue = false;
      AnsiString Sw, V;
      Integer Pos = 1;

      while (Pos <= S.Length() && !HasValue)
      {
        HasValue = S.IsDelimiter(FSwitchValueDelimiters, Pos);
        if (!HasValue) Pos++;
      }
      if (HasValue)
      {
        Sw = S.SubString(1, Pos-1);
        V = S.SubString(Pos+1, S.Length()-Pos);
      }
      else
      {
        Sw = S;
        V = "";
      };
      if ((IgnoreCase && (AnsiCompareText(Switch, Sw) == 0)) ||
          (!IgnoreCase && (AnsiCompareStr(Switch, Sw) == 0)))
      {
        Value = V;
        return true;
      }
    }
  }
  Position = 0;
  return false;
}
//---------------------------------------------------------------------------
bool __fastcall TProgramParams::FindSwitch(const AnsiString Switch, AnsiString & Value)
{
  int Position;
  return FindSwitch(Switch, Value, Position);
}
//---------------------------------------------------------------------------
bool __fastcall TProgramParams::FindSwitch(const AnsiString Switch)
{
  AnsiString Value;
  int Position;
  return FindSwitch(Switch, Value, Position);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TProgramParams::SwitchValue(const AnsiString Switch, const AnsiString Default)
{
  AnsiString Value;
  FindSwitch(Switch, Value);
  if (Value.IsEmpty()) Value = Default;
  return Value;
}
