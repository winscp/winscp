//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "LogMemo.h"
#include <StrUtils.hpp>

#pragma package(smart_init)
//---------------------------------------------------------------------------
#ifndef DESIGN_ONLY
const TColor LogLineColors[] =
  {clGreen, clRed, clMaroon, clBlue, clGray};
#endif
//---------------------------------------------------------------------------
// ValidCtrCheck is used to assure that the components created do not have
// any pure virtual functions.
static inline void ValidCtrCheck(TLogMemo *)
{
  new TLogMemo(NULL);
}
//---------------------------------------------------------------------------
namespace Logmemo
{
  void __fastcall PACKAGE Register()
  {
    TComponentClass classes[1] = {__classid(TLogMemo)};
    RegisterComponents(L"Scp", classes, 0);
  }
}
//---------------------------------------------------------------------------
__fastcall TLogMemo::TLogMemo(TComponent* Owner)
        : TCustomRichEdit(Owner)
{
  FIndexes = new TList();
  FWantScrollToEnd = false;
  FUpdating = false;
  FReloading = false;
  FNeedsRepaint = false;
  FLastUpdate = 0;

  FShowTypes = DEFAULT_LOGMEMO_SHOWTYPES;
  ReadOnly = true;
  Font->Name = DEFAULT_LOGMEMO_FONT;
  WantReturns = false;
  WordWrap = false;
  ScrollBars = ssBoth;
  FThread = GetCurrentThreadId();
}
//---------------------------------------------------------------------------
__fastcall TLogMemo::~TLogMemo()
{
#ifndef DESIGN_ONLY
  // deassociate us from session log change handler
  SessionLog = NULL;
#endif
  delete FIndexes;
}
//---------------------------------------------------------------------------
void __fastcall TLogMemo::WMSetFocus(TWMSetFocus & Message)
{
  try
  {
    TCustomRichEdit::Dispatch(&Message);
  }
  __finally
  {
    HideCaret(Handle);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TLogMemo::IsFontStored()
{
  return
    (Font->Name != DEFAULT_LOGMEMO_FONT) ||
    (Font->Charset != DEFAULT_CHARSET) ||
    (Font->Color != clWindowText) ||
    (Font->Height != -11) ||
    (Font->Pitch != TFontPitch::fpDefault) ||
    (Font->Size != 8) ||
    (Font->Style != TFontStyles());
}
//---------------------------------------------------------------------------
#ifndef DESIGN_ONLY
void __fastcall TLogMemo::SetSessionLog(TSessionLog * value)
{
  if (FSessionLog != value)
  {
    if (SessionLog && (SessionLog->OnChange == SessionLogChange))
    {
      SessionLog->OnChange = NULL;
    }
    FSessionLog = value;
    if (SessionLog)
    {
      SessionLog->OnChange = SessionLogChange;
    }
    ReloadFromLog();
  }
}
#endif
//---------------------------------------------------------------------------
void __fastcall TLogMemo::SessionLogChange(TObject * Sender)
{
  USEDPARAM(Sender);
#ifndef DESIGN_ONLY
  DebugAssert(Sender && (Sender == (TObject*)SessionLog));
#endif
  if (HandleAllocated())
  {
    unsigned int Ticks = GetTickCount();
    if (((FLastUpdate == 0) || (Ticks < FLastUpdate) || (Ticks - FLastUpdate > 200)) &&
        (FThread == GetCurrentThreadId()))
    {
      // forced update
      if (!FReloading)
      {
        UpdateFromLog();
      }
    }
    else
    {
      // update later, once idle
      PostMessage(Handle, WM_LOG_UPDATE, 0, 0);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLogMemo::UpdateFromLog()
{
#ifndef DESIGN_ONLY
  if (SessionLog && Parent && !Application->Terminated)
  {
    DebugAssert(FIndexes->Count == Lines->Count);
    FUpdating = true;
    bool Updated = false;
    SessionLog->Lock();
    try
    {
      ScrollToEnd();
      if (Lines->Count && (Indexes[0] < SessionLog->TopIndex))
      {
        try
        {
          SendMessage(Handle, WM_SETREDRAW, false, 0);
          FNeedsRepaint = true;
          while (Lines->Count && (Indexes[0] < SessionLog->TopIndex))
          {
            FIndexes->Delete(0);
            if (Parent)
            {
              Lines->Delete(0);
              Updated = true;
            }
          }
        }
        __finally
        {
          SendMessage(Handle, WM_SETREDRAW, true, 0);
        }
      }

      if (SessionLog->Count)
      {
        int LastIndex;
        if (Lines->Count)
        {
          LastIndex = Indexes[Lines->Count-1] + 1;
        }
        else
        {
          LastIndex = SessionLog->TopIndex;
        }

        while (Parent && LastIndex <= SessionLog->BottomIndex)
        {
          if (Parent && ShowTypes.Contains(SessionLog->Type[LastIndex]))
          {
            SelLength = 0;
            SelStart = Lines->Text.Length();
            if (Parent) SelAttributes->Color = LogLineColors[SessionLog->Type[LastIndex]];
            FIndexes->Add((void*) LastIndex);
            try
            {
              // this usually fails when log window is closed while
              // new line is being added (control has no parent)
              if (Parent)
              {
                if (SessionLog->Line[LastIndex].Pos(L"\r"))
                {
                  Lines->Add(ReplaceStr(SessionLog->Line[LastIndex], L"\r", L""));
                }
                else
                {
                  Lines->Add(SessionLog->Line[LastIndex]);
                }
                Updated = true;
              }
            }
            catch(...)
            {
              if (Lines->Count < FIndexes->Count)
              {
                FIndexes->Delete(FIndexes->Count - 1);
              }
              DebugAssert(FIndexes->Count == Lines->Count);
              // LastIndex is strangely reset to 0 when exception is caught
              LastIndex = Indexes[Lines->Count-1];
            }
          }
          LastIndex++;
        }
      }

      if (Parent)
      {
        ScrollToEnd();
        if (FNeedsRepaint)
        {
          FNeedsRepaint = false;
          Invalidate();
        }
      }

      DebugAssert(!Parent || FIndexes->Count == Lines->Count);

      FLastUpdate = GetTickCount();
    }
    __finally
    {
      SessionLog->Unlock();
      FUpdating = false;
    }

    if (Updated)
    {
      Change();
    }
  }
#endif
}
//---------------------------------------------------------------------------
void __fastcall TLogMemo::ReloadFromLog()
{
  if (Parent && !FReloading)
  {
    TAutoFlag ReloadingFlag(FReloading);

    Lines->BeginUpdate();
    try
    {
      Clear();
      FIndexes->Clear();
      UpdateFromLog();
    }
    __finally
    {
      Lines->EndUpdate();
    }
  }
}
//---------------------------------------------------------------------------
int __fastcall TLogMemo::GetIndexes(int Index)
{
  DebugAssert((Index >= 0) && (Index < Lines->Count) && (FIndexes->Count == Lines->Count));
  return ((int)FIndexes->Items[Index]);
}
//---------------------------------------------------------------------------
void __fastcall TLogMemo::SetShowTypes(TLogLineTypes value)
{
  if (ShowTypes != value)
  {
    FShowTypes = value;
    ReloadFromLog();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TLogMemo::StoreShowTypes()
{
  return (ShowTypes != DEFAULT_LOGMEMO_SHOWTYPES);
}
//---------------------------------------------------------------------------
void __fastcall TLogMemo::ScrollToEnd()
{
  TCharRange Selection;
  Selection.cpMin = Lines->Text.Length();
  Selection.cpMax = Selection.cpMin;
  Perform(EM_EXSETSEL, 0, ((long)&Selection));
  Perform(EM_SCROLLCARET, 0, 0);
}
//---------------------------------------------------------------------------
void TLogMemo::WMLogUpdate(TMessage & /*Message*/)
{
  if (!FReloading)
  {
    UpdateFromLog();
  }
}
//---------------------------------------------------------------------------
void TLogMemo::CMVisibleChanged(TMessage & Message)
{
  try
  {
    TCustomRichEdit::Dispatch(&Message);
  }
  __finally
  {
    ScrollToEnd();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLogMemo::MouseDown(TMouseButton Button, TShiftState Shift, int X, int Y)
{
  try
  {
    TCustomRichEdit::MouseDown(Button, Shift, X, Y);
  }
  __finally
  {
    HideCaret(Handle);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLogMemo::CMShowingChanged(TMessage & Message)
{
  bool VShowing = Showing;
  try
  {
    TCustomRichEdit::Dispatch(&Message);
  }
  __finally
  {
    if (VShowing)
    {
      FWantScrollToEnd = true;
    }
    HideCaret(Handle);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLogMemo::KeyDown(Word & Key, TShiftState Shift)
{
  if ((Key == VK_UP) || (Key == VK_DOWN))
  {
    SendMessage(Handle, EM_LINESCROLL, 0, ( Key == VK_UP ? -1 : 1));
    Key = 0;
  }
  else
  {
    TCustomRichEdit::KeyDown(Key, Shift);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLogMemo::WMKeyDown(TWMKeyDown & Message)
{
  try
  {
    TCustomRichEdit::Dispatch(&Message);
  }
  __finally
  {
    HideCaret(Handle);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLogMemo::WMPaint(TWMPaint & Message)
{
  try
  {
    TCustomRichEdit::Dispatch(&Message);
  }
  __finally
  {
    if (FWantScrollToEnd)
    {
      FWantScrollToEnd = false;
      SelLength = 0;
      SelStart = Lines->Text.Length();
      SendMessage(Handle, EM_LINESCROLL, 0, Lines->Count);
    }
    HideCaret(Handle);
  }
}
//---------------------------------------------------------------------------
int __fastcall TLogMemo::GetLinesVisible()
{
  HFONT OldFont;
  HDC DC;
  TTextMetricW TM;
  TRect Rect;

  DC = GetDC((HWND)Handle);
  OldFont = (HFONT)SelectObject(DC, (HWND)Font->Handle);

  try
  {
    GetTextMetrics(DC, &TM);

    Perform(EM_GETRECT, 0, ((int)&Rect));
  }
  __finally
  {
    SelectObject(DC, OldFont);
    ReleaseDC(Handle, DC);
  }

  return (Rect.Bottom - Rect.Top) / (TM.tmHeight + TM.tmExternalLeading);
}
//---------------------------------------------------------------------------
void __fastcall TLogMemo::SetParent(TWinControl * AParent)
{
  TCustomRichEdit::SetParent(AParent);
  if (AParent) UpdateFromLog();
}
//---------------------------------------------------------------------------
void __fastcall TLogMemo::Change()
{
  if (Parent && Visible && !Application->Terminated && !FUpdating)
  {
    TCustomRichEdit::Change();
  }
}
