//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ProgParams.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TProgramParams::TProgramParams()
{
  FParameters = new TStringList();
  for (int i = 0; i <= ::ParamCount(); i++)
  {
    FParameters->Add(ParamStr(i));
  }

  FSwitchMarks = "-/";
  FSwitchValueDelimiters = ":=";
  FIgnoreCase = true;
  for (int i = 0; i < (sizeof(FParamCount) / sizeof(*FParamCount)); i++)
  {
    FParamCount[i] = -1;
  }
}
//---------------------------------------------------------------------------
TProgramParams::~TProgramParams()
{
  delete FParameters;
}
//---------------------------------------------------------------------------
Integer __fastcall TProgramParams::GetCount()
{
  return FParameters->Count - 1;
}
//---------------------------------------------------------------------------
TParamType __fastcall TProgramParams::ParamType(Integer Index, AnsiString & Value)
{
  if (Index < 1 || Index > Count) throw Exception("");
  TParamType Result;
  Value = FParameters->Strings[Index];
  if ((Value.Length() >= 2) && (FSwitchMarks.Pos(Value[1]) > 0))
  {
    Result = ptSwitch;
    for (int i = 2; i <= Value.Length(); i++)
    {
      if ((i > 2) && (Value.IsDelimiter(FSwitchValueDelimiters, i)))
      {
        break;
      }
      else if ((Value[i] != '?') && ((UpCase(Value[i]) < 'A') || (UpCase(Value[i]) > 'Z')))
      {
        Result = ptParam;
        break;
      }
    }

    if (Result == ptSwitch)
    {
      Value.Delete(1, 1);
    }
  }
  else
  {
    Result = ptParam;
  }

  return Result;
}
//---------------------------------------------------------------------------
Integer __fastcall TProgramParams::GetParamTypeCount(int Type)
{
  if (FParamCount[Type] < 0)
  {
    FParamCount[Type] = 0;
    AnsiString S;
    for (Integer Index = 1; Index <= Count; Index++)
    {
      if (ParamType(Index, S) == Type)
      {
        FParamCount[Type]++;
      }
    }
  }
  return FParamCount[Type];
}
//---------------------------------------------------------------------------
AnsiString __fastcall TProgramParams::GetParam(Integer Index)
{
  if (Index < 0 || Index > ParamCount) throw Exception("");
  if (Index == 0) return FParameters->Strings[0];
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
//---------------------------------------------------------------------------
void __fastcall TProgramParams::ParamsProcessed(int Position, int Count)
{
  int Index = 1;
  int APosition = 0;
  while (Index <= Count)
  {
    AnsiString S;
    if (ParamType(Index, S) == ptParam)
    {
      APosition++;

      if ((APosition >= Position) && (APosition < Position + Count))
      {
        FParameters->Delete(Index);
        Index--;
      }
    }
    Index++;
  }
}

