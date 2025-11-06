//---------------------------------------------------------------------------
#pragma warn -pch // WORKAROUND (see My.cpp)
#include <vcl.h>
#pragma hdrstop

#include "HistoryComboBox.h"
#include "PasTools.hpp"
#include "Common.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
void SaveToHistory(TStrings * Strings, const UnicodeString & T, TObject * Data, int MaxHistorySize)
{
  if (!T.IsEmpty())
  {
    while (Strings->IndexOf(T) >= 0)
    {
      Strings->Delete(Strings->IndexOf(T));
    }
    Strings->InsertObject(0, T, Data);
  }

  while (Strings->Count > MaxHistorySize)
  {
    Strings->Delete(Strings->Count - 1);
  }
}
//---------------------------------------------------------------------------
// TUIStateAwareComboBox
//---------------------------------------------------------------------------
TUIStateAwareComboBox::TUIStateAwareComboBox(TComponent * AOwner) :
  TComboBox(AOwner)
{
}
//---------------------------------------------------------------------------
void __fastcall TUIStateAwareComboBox::ComboWndProc(TMessage & Message, HWND ComboWnd, TWindowProcPtr ComboProc)
{
  TComboBox::ComboWndProc(Message, ComboWnd, ComboProc);

  if (Message.Msg == WM_SYSKEYDOWN)
  {
    UpdateUIState(reinterpret_cast<TWMKey &>(Message).CharCode);
  }
}
//---------------------------------------------------------------------------
void __fastcall TUIStateAwareComboBox::CreateWnd()
{
  TComboBox::CreateWnd();

  if (DarkMode)
  {
    SetDarkModeTheme(this, L"CFD");
  }
}
//---------------------------------------------------------------------------
void TUIStateAwareComboBox::SetDarkMode(bool Value)
{
  if (DarkMode != Value)
  {
    FDarkMode = Value;
    RecreateWnd();
  }
}
//---------------------------------------------------------------------------
// THistoryComboBox
//---------------------------------------------------------------------------
__fastcall THistoryComboBox::THistoryComboBox(TComponent * AOwner) :
  TUIStateAwareComboBox(AOwner)
{
  FSaveOn = THistorySaveOn(DefaultHistorySaveOn);
  FMaxHistorySize = DefaultMaxHistorySize;
  FOnGetData = nullptr;
  FOnSetData = nullptr;
}
//---------------------------------------------------------------------------
void __fastcall THistoryComboBox::KeyDown(unsigned short & Key, TShiftState Shift)
{
  if (((Key == VK_DOWN) || (Key == VK_UP)) &&
       !Shift.Contains(ssAlt) && SaveOn.Contains(soDropDown))
  {
    if (Items->IndexOf(Text) < 0)
    {
      SaveToHistory();
    }
  }
  if (DroppedDown && (Key == VK_DELETE) && Shift.Contains(ssCtrl) && !SaveOn.Empty())
  {
    Items->Clear();
    Key = 0;
  }
  TUIStateAwareComboBox::KeyDown(Key, Shift);
}
//---------------------------------------------------------------------------
void __fastcall THistoryComboBox::SetMaxHistorySize(int AMaxHistorySize)
{
  FMaxHistorySize = AMaxHistorySize;
  while (Items->Count > FMaxHistorySize)
  {
    Items->Delete(Items->Count - 1);
  }
}
//---------------------------------------------------------------------------
void __fastcall THistoryComboBox::DoExit()
{
  TUIStateAwareComboBox::DoExit();
  if (SaveOn.Contains(soExit))
  {
    SaveToHistory();
  }
}
//---------------------------------------------------------------------------
void __fastcall THistoryComboBox::DropDown()
{
  TUIStateAwareComboBox::DropDown();

  if (SaveOn.Contains(soDropDown))
  {
    SaveToHistory();
  }

  int ItemWidth = GetMaxItemWidth() + ScaleByPixelsPerInch(8, this);
  if (Items->Count > DropDownCount)
  {
    ItemWidth += GetSystemMetricsForControl(this, SM_CXVSCROLL);
  }
  Perform(CB_SETDROPPEDWIDTH, static_cast<unsigned int>(ItemWidth), 0);
}
//---------------------------------------------------------------------------
void __fastcall THistoryComboBox::Change()
{
  TUIStateAwareComboBox::Change();
  if (OnSetData != nullptr)
  {
    // note that ItemIndex is not reliable
    int Index = Items->IndexOf( Text );
    if (Index >= 0)
    {
      OnSetData(this, Items->Objects[Index]);
    }
  }
}
//---------------------------------------------------------------------------
void THistoryComboBox::SaveToHistory()
{
  if (!Text.IsEmpty())
  {
    TObject * Data = nullptr;
    if (OnGetData != nullptr)
    {
      OnGetData(this, Data);
    }
    ::SaveToHistory(Items, Text, Data, MaxHistorySize);
    ItemIndex = 0;
  }
}
//---------------------------------------------------------------------------
int THistoryComboBox::GetMaxItemWidth()
{
  int Result = 0;
  HDC DC = GetDC(0);
  try
  {
    HGDIOBJ SaveFont = SelectObject(DC, Font->Handle);
    for (int Index = 0; Index < Items->Count; Index++)
    {
      TSize Size;
      GetTextExtentPoint32(DC, Items->Strings[Index].c_str(), Items->Strings[Index].Length(), &Size);
      if (Size.cx > Result)
      {
        Result = Size.cx;
      }
    }
    SelectObject(DC, SaveFont);
  }
  __finally
  {
    ReleaseDC(0, DC);
  }
  return Result;
}
