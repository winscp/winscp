//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include "ProgParams.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TProgramParams * TProgramParams::SInstance = NULL;
//---------------------------------------------------------------------------
TProgramParams * __fastcall TProgramParams::Instance()
{
  assert(SInstance != NULL);
  return SInstance;
}
//---------------------------------------------------------------------------
TProgramParams::TProgramParams()
{
  assert(SInstance == NULL);
  SInstance = this;

  FParameters = new TStringList();
  for (int i = 0; i <= ::ParamCount(); i++)
  {
    FParameters->Add(ParamStr(i));
  }

  FSwitchMarks = "-/";
  FSwitchValueDelimiters = ":=";
  FIgnoreCase = true;
  ResetParamCount();
}
//---------------------------------------------------------------------------
TProgramParams::~TProgramParams()
{
  delete FParameters;
  assert(SInstance == this);
  SInstance = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TProgramParams::ResetParamCount()
{
  for (int i = 0; i < (sizeof(FParamCount) / sizeof(*FParamCount)); i++)
  {
    FParamCount[i] = -1;
  }
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
  AnsiString & Value, int & ParamsStart, int & ParamsCount)
{
  ParamsStart = 0;
  int Index = 1;
  bool Found = false;
  while ((Index <= Count) && !Found)
  {
    AnsiString S;
    if (ParamType(Index, S) == ptParam) ParamsStart++;
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
        Found = true;
      }
    }
    Index++;
  }

  ParamsCount = 0;
  if (Found)
  {
    AnsiString S;
    ParamsStart++;
    while ((Index + ParamsCount <= Count) &&
           (ParamType(Index + ParamsCount, S) == ptParam))
    {
      ParamsCount++;
    }
  }
  else
  {
    ParamsStart = 0;
  }

  return Found;
}
//---------------------------------------------------------------------------
bool __fastcall TProgramParams::FindSwitch(const AnsiString Switch, AnsiString & Value)
{
  int ParamsStart;
  int ParamsCount;
  return FindSwitch(Switch, Value, ParamsStart, ParamsCount);
}
//---------------------------------------------------------------------------
bool __fastcall TProgramParams::FindSwitch(const AnsiString Switch)
{
  AnsiString Value;
  int ParamsStart;
  int ParamsCount;
  return FindSwitch(Switch, Value, ParamsStart, ParamsCount);
}
//---------------------------------------------------------------------------
bool __fastcall TProgramParams::FindSwitch(const AnsiString Switch,
  TStrings * Params, int ParamsMax)
{
  AnsiString Value;
  int ParamsStart;
  int ParamsCount;
  bool Result = FindSwitch(Switch, Value, ParamsStart, ParamsCount);
  if (Result)
  {
    if ((ParamsMax >= 0) && (ParamsCount > ParamsMax))
    {
      ParamsCount = ParamsMax;
    }

    int Index = 0;
    while (Index < ParamsCount)
    {
      Params->Add(Param[ParamsStart + Index]);
      Index++;
    }
    ParamsProcessed(ParamsStart, ParamsCount);
  }
  return Result;
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
void __fastcall TProgramParams::ParamsProcessed(int ParamsStart, int ParamsCount)
{
  int Index = 1;
  int Position = 0;
  while (Index <= this->Count)
  {
    AnsiString S;
    if (ParamType(Index, S) == ptParam)
    {
      Position++;

      if ((Position >= ParamsStart) && (Position < ParamsStart + ParamsCount))
      {
        FParameters->Delete(Index);
        Index--;
      }
    }
    Index++;
  }
  ResetParamCount();
}
