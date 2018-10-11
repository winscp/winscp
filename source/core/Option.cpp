//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include "Option.h"
#include "TextsCore.h"
#include "System.StrUtils.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
const wchar_t ArrayValueDelimiter = L'[';
const wchar_t ArrayValueEnd = L']';
//---------------------------------------------------------------------------
__fastcall TOptions::TOptions()
{
  FSwitchMarks = L"/-";
  FSwitchValueDelimiters = UnicodeString(L"=:") + ArrayValueDelimiter;
  FNoMoreSwitches = false;
  FParamCount = 0;
}
//---------------------------------------------------------------------------
void __fastcall TOptions::Add(UnicodeString Value)
{
  if (!FNoMoreSwitches &&
      (Value.Length() == 2) &&
      (Value[1] == Value[2]) &&
      (FSwitchMarks.Pos(Value[1]) > 0))
  {
    FNoMoreSwitches = true;
  }
  else
  {
    bool Switch = false;
    int Index = 0; // shut up
    wchar_t SwitchMark = L'\0';
    wchar_t ValueDelimiter = L'\0';
    if (!FNoMoreSwitches &&
        (Value.Length() >= 2) &&
        (FSwitchMarks.Pos(Value[1]) > 0))
    {
      Index = 2;
      Switch = true;
      SwitchMark = Value[1];
      while (Switch && (Index <= Value.Length()))
      {
        if (Value.IsDelimiter(FSwitchValueDelimiters, Index))
        {
          ValueDelimiter = Value[Index];
          break;
        }
        // this is to treat /home/martin as parameter, not as switch
        else if ((Value[Index] != L'?') && !IsLetter(Value[Index]))
        {
          Switch = false;
          break;
        }
        ++Index;
      }
    }

    TOption Option;

    if (Switch)
    {
      Option.Type = otSwitch;
      Option.Name = Value.SubString(2, Index - 2);
      Option.Value = Value.SubString(Index + 1, Value.Length());
      if ((ValueDelimiter == ArrayValueDelimiter) && EndsStr(ArrayValueEnd, Option.Value))
      {
        Option.Value.SetLength(Option.Value.Length() - 1);
      }
      Option.ValueSet = (Index <= Value.Length());
    }
    else
    {
      Option.Type = otParam;
      Option.Value = Value;
      Option.ValueSet = false; // unused
      ++FParamCount;
    }

    Option.Used = false;
    Option.SwitchMark = SwitchMark;

    FOptions.push_back(Option);
  }

  FOriginalOptions = FOptions;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TOptions::GetParam(int Index)
{
  DebugAssert((Index >= 1) && (Index <= FParamCount));

  UnicodeString Result;
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
bool __fastcall TOptions::FindSwitch(const UnicodeString Switch,
  UnicodeString & Value, int & ParamsStart, int & ParamsCount, bool CaseSensitive, bool & ValueSet)
{
  ParamsStart = 0;
  ValueSet = false;
  int Index = 0;
  bool Found = false;
  while ((Index < int(FOptions.size())) && !Found)
  {
    if (FOptions[Index].Type == otParam)
    {
      ParamsStart++;
    }
    else if (FOptions[Index].Type == otSwitch)
    {
      if ((!CaseSensitive && SameText(FOptions[Index].Name, Switch)) ||
          (CaseSensitive && SameStr(FOptions[Index].Name, Switch)))
      {
        Found = true;
        Value = FOptions[Index].Value;
        ValueSet = FOptions[Index].ValueSet;
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
bool __fastcall TOptions::FindSwitch(const UnicodeString Switch, UnicodeString & Value)
{
  bool ValueSet;
  return FindSwitch(Switch, Value, ValueSet);
}
//---------------------------------------------------------------------------
bool __fastcall TOptions::FindSwitch(const UnicodeString Switch, UnicodeString & Value, bool & ValueSet)
{
  int ParamsStart;
  int ParamsCount;
  return FindSwitch(Switch, Value, ParamsStart, ParamsCount, false, ValueSet);
}
//---------------------------------------------------------------------------
bool __fastcall TOptions::FindSwitch(const UnicodeString Switch)
{
  UnicodeString Value;
  int ParamsStart;
  int ParamsCount;
  bool ValueSet;
  return FindSwitch(Switch, Value, ParamsStart, ParamsCount, false, ValueSet);
}
//---------------------------------------------------------------------------
bool __fastcall TOptions::FindSwitchCaseSensitive(const UnicodeString Switch)
{
  UnicodeString Value;
  int ParamsStart;
  int ParamsCount;
  bool ValueSet;
  return FindSwitch(Switch, Value, ParamsStart, ParamsCount, true, ValueSet);
}
//---------------------------------------------------------------------------
bool __fastcall TOptions::FindSwitch(const UnicodeString Switch,
  TStrings * Params, int ParamsMax)
{
  return DoFindSwitch(Switch, Params, ParamsMax, false);
}
//---------------------------------------------------------------------------
bool __fastcall TOptions::FindSwitchCaseSensitive(const UnicodeString Switch,
  TStrings * Params, int ParamsMax)
{
  return DoFindSwitch(Switch, Params, ParamsMax, true);
}
//---------------------------------------------------------------------------
bool __fastcall TOptions::DoFindSwitch(const UnicodeString Switch,
  TStrings * Params, int ParamsMax, bool CaseSensitive)
{
  UnicodeString Value;
  int ParamsStart;
  int ParamsCount;
  bool ValueSet;
  bool Result = FindSwitch(Switch, Value, ParamsStart, ParamsCount, CaseSensitive, ValueSet);
  if (Result)
  {
    int AParamsCount;
    if (TryStrToInt(Value, AParamsCount) && (AParamsCount < ParamsCount))
    {
      ParamsCount = AParamsCount;
    }
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
UnicodeString __fastcall TOptions::SwitchValue(const UnicodeString Switch,
  const UnicodeString Default)
{
  UnicodeString Value;
  FindSwitch(Switch, Value);
  if (Value.IsEmpty())
  {
    Value = Default;
  }
  return Value;
}
//---------------------------------------------------------------------------
bool __fastcall TOptions::SwitchValue(const UnicodeString Switch, bool Default, bool DefaultOnNonExistence)
{
  bool Result;
  int IntValue;
  UnicodeString Value;
  if (!FindSwitch(Switch, Value))
  {
    Result = DefaultOnNonExistence;
  }
  else if (Value.IsEmpty())
  {
    Result = Default;
  }
  else if (SameText(Value, "on"))
  {
    Result = true;
  }
  else if (SameText(Value, "off"))
  {
    Result = false;
  }
  else if (TryStrToInt(Value, IntValue))
  {
    Result = (IntValue != 0);
  }
  else
  {
    throw Exception(FMTLOAD(URL_OPTION_BOOL_VALUE_ERROR, (Value)));
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TOptions::SwitchValue(const UnicodeString Switch, bool Default)
{
  return SwitchValue(Switch, Default, Default);
}
//---------------------------------------------------------------------------
bool __fastcall TOptions::UnusedSwitch(UnicodeString & Switch)
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
bool __fastcall TOptions::WasSwitchAdded(UnicodeString & Switch, wchar_t & SwitchMark)
{
  bool Result =
    DebugAlwaysTrue(FOptions.size() > 0) &&
    (FOptions.back().Type == otSwitch);
  if (Result)
  {
    TOption & Option = FOptions.back();
    Switch = Option.Name;
    SwitchMark = Option.SwitchMark;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TOptions::ParamsProcessed(int ParamsStart, int ParamsCount)
{
  if (ParamsCount > 0)
  {
    DebugAssert((ParamsStart >= 0) && ((ParamsStart - ParamsCount + 1) <= FParamCount));

    size_t Index = 0;
    while ((Index < FOptions.size()) && (ParamsStart > 0))
    {
      if (FOptions[Index].Type == otParam)
      {
        --ParamsStart;

        if (ParamsStart == 0)
        {
          while (ParamsCount > 0)
          {
            DebugAssert(Index < FOptions.size());
            DebugAssert(FOptions[Index].Type == otParam);
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
//---------------------------------------------------------------------------
void __fastcall TOptions::LogOptions(TLogOptionEvent OnLogOption)
{
  for (size_t Index = 0; Index < FOriginalOptions.size(); Index++)
  {
    const TOption & Option = FOriginalOptions[Index];
    UnicodeString LogStr;
    switch (Option.Type)
    {
      case otParam:
        LogStr = FORMAT(L"Parameter: %s", (Option.Value));
        DebugAssert(Option.Name.IsEmpty());
        break;

      case otSwitch:
        LogStr =
          FORMAT(L"Switch:    %s%s%s%s",
            (FSwitchMarks[1], Option.Name, (Option.Value.IsEmpty() ? UnicodeString() : FSwitchValueDelimiters.SubString(1, 1)), Option.Value));
        break;

      default:
        DebugFail();
        break;
    }
    OnLogOption(LogStr);
  }
}
