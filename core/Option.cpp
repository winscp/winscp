//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include "Option.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall TOptions::TOptions()
{
  FSwitchMarks = "-/";
  FSwitchValueDelimiters = ":=";
  FNoMoreSwitches = false;
  FParamCount = 0;
}
//---------------------------------------------------------------------------
void __fastcall TOptions::Add(AnsiString Value)
{
  if (!FNoMoreSwitches &&
      (Value.Length() == 2) &&
      (Value[1] == Value[2]) &&
      (FSwitchMarks.Pos(Value[1]) > 0))
  {
    FNoMoreSwitches = true;
  }
  else if (!FNoMoreSwitches &&
    (Value.Length() >= 2) &&
    (FSwitchMarks.Pos(Value[1]) > 0))
  {
    int Index = 2;
    while (Index <= Value.Length())
    {
      if (Value.IsDelimiter(FSwitchValueDelimiters, Index))
      {
        break;
      }
      ++Index;
    }

    TOption Option;
    Option.Type = otSwitch;
    Option.Name = Value.SubString(2, Index - 2);
    Option.Value = Value.SubString(Index + 1, Value.Length());
    Option.Used = false;
    FOptions.push_back(Option);
  }
  else
  {
    TOption Option;
    Option.Type = otParam;
    Option.Value = Value;
    Option.Used = false;
    FOptions.push_back(Option);
    ++FParamCount;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TOptions::GetParam(int Index)
{
  assert((Index >= 1) && (Index <= FParamCount));

  AnsiString Result;
  size_t I = 0;
  while ((I < FOptions.size()) && (Index > 0))
  {
    if (FOptions[I].Type == otParam)
    {
      --Index;
      if (Index == 0)
      {
        Result = FOptions[I].Value;
        FOptions[I].Used = true;
      }
    }
    ++I;
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TOptions::GetEmpty()
{
  return FOptions.empty();
}
//---------------------------------------------------------------------------
bool __fastcall TOptions::FindSwitch(const AnsiString Switch,
  AnsiString & Value, int & ParamsStart, int & ParamsCount)
{
  ParamsStart = 0;
  int Index = 0;
  bool Found = false;
  while ((Index < int(FOptions.size())) && !Found)
  {
    AnsiString S;
    if (FOptions[Index].Type == otParam)
    {
      ParamsStart++;
    }
    else if (FOptions[Index].Type == otSwitch)
    {
      if (AnsiSameText(FOptions[Index].Name, Switch))
      {
        Found = true;
        Value = FOptions[Index].Value;
        FOptions[Index].Used = true;
      }
    }
    Index++;
  }

  ParamsCount = 0;
  if (Found)
  {
    ParamsStart++;
    while ((Index + ParamsCount < int(FOptions.size())) &&
           (FOptions[Index + ParamsCount].Type == otParam))
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
bool __fastcall TOptions::FindSwitch(const AnsiString Switch, AnsiString & Value)
{
  int ParamsStart;
  int ParamsCount;
  return FindSwitch(Switch, Value, ParamsStart, ParamsCount);
}
//---------------------------------------------------------------------------
bool __fastcall TOptions::FindSwitch(const AnsiString Switch)
{
  AnsiString Value;
  int ParamsStart;
  int ParamsCount;
  return FindSwitch(Switch, Value, ParamsStart, ParamsCount);
}
//---------------------------------------------------------------------------
bool __fastcall TOptions::FindSwitch(const AnsiString Switch,
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
AnsiString __fastcall TOptions::SwitchValue(const AnsiString Switch,
  const AnsiString Default)
{
  AnsiString Value;
  FindSwitch(Switch, Value);
  if (Value.IsEmpty())
  {
    Value = Default;
  }
  return Value;
}
//---------------------------------------------------------------------------
bool __fastcall TOptions::UnusedSwitch(AnsiString & Switch)
{
  bool Result = false;
  size_t Index = 0;
  while (!Result && (Index < FOptions.size()))
  {
    if ((FOptions[Index].Type == otSwitch) &&
        !FOptions[Index].Used)
    {
      Switch = FOptions[Index].Name;
      Result = true;
    }
    ++Index;
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TOptions::ParamsProcessed(int ParamsStart, int ParamsCount)
{
  if (ParamsCount > 0)
  {
    assert((ParamsStart >= 0) && ((ParamsStart - ParamsCount + 1) <= FParamCount));

    size_t Index = 0;
    while ((Index < FOptions.size()) && (ParamsStart > 0))
    {
      AnsiString S;
      if (FOptions[Index].Type == otParam)
      {
        --ParamsStart;

        if (ParamsStart == 0)
        {
          while (ParamsCount > 0)
          {
            assert(Index < FOptions.size());
            assert(FOptions[Index].Type == otParam);
            FOptions.erase(FOptions.begin() + Index);
            --FParamCount;
            --ParamsCount;
          }
        }
      }
      Index++;
    }
  }
}
