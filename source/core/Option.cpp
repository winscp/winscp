//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include "Option.h"
#include "TextsCore.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall TOptions::TOptions()
{
  FSwitchMarks = L"/-";
  FSwitchValueDelimiters = L"=:";
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
    if (!FNoMoreSwitches &&
        (Value.Length() >= 2) &&
        (FSwitchMarks.Pos(Value[1]) > 0))
    {
      Index = 2;
      Switch = true;
      while (Switch && (Index <= Value.Length()))
      {
        if (Value.IsDelimiter(FSwitchValueDelimiters, Index))
        {
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

    if (Switch)
    {
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

  FOriginalOptions = FOptions;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TOptions::GetParam(int Index)
{
  assert((Index >= 1) && (Index <= FParamCount));

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
  UnicodeString & Value, int & ParamsStart, int & ParamsCount)
{
  ParamsStart = 0;
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
bool __fastcall TOptions::FindSwitch(const UnicodeString Switch, UnicodeString & Value)
{
  int ParamsStart;
  int ParamsCount;
  return FindSwitch(Switch, Value, ParamsStart, ParamsCount);
}
//---------------------------------------------------------------------------
bool __fastcall TOptions::FindSwitch(const UnicodeString Switch)
{
  UnicodeString Value;
  int ParamsStart;
  int ParamsCount;
  return FindSwitch(Switch, Value, ParamsStart, ParamsCount);
}
//---------------------------------------------------------------------------
bool __fastcall TOptions::FindSwitch(const UnicodeString Switch,
  TStrings * Params, int ParamsMax)
{
  UnicodeString Value;
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
bool __fastcall TOptions::WasSwitchAdded(UnicodeString & Switch)
{
  bool Result =
    ALWAYS_TRUE(FOptions.size() > 0) &&
    (FOptions.back().Type == otSwitch);
  if (Result)
  {
    Switch = FOptions.back().Name;
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
        assert(Option.Name.IsEmpty());
        break;

      case otSwitch:
        LogStr =
          FORMAT(L"Switch:    %s%s%s%s",
            (FSwitchMarks[1], Option.Name, (Option.Value.IsEmpty() ? UnicodeString() : FSwitchValueDelimiters.SubString(1, 1)), Option.Value));
        break;

      default:
        FAIL;
        break;
    }
    OnLogOption(LogStr);
  }
}
