//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "LogMemo.h"

#include <Common.h>

#pragma package(smart_init)
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
    RegisterComponents("Scp", classes, 0);
  }
}
//---------------------------------------------------------------------------
__fastcall TLogMemo::TLogMemo(TComponent* Owner)
        : TCustomRichEdit(Owner)
{
  FIndexes = new TList();
  FWantScrollToEnd = false;
  FUpdating = false;
  FNeedsRepaint = false;

  FShowTypes = DEFAULT_LOGMEMO_SHOWTYPES;
  ReadOnly = true;
  Font->Name = DEFAULT_LOGMEMO_FONT;
  WantReturns = false;
  WordWrap = false;
  ScrollBars = ssBoth;
}
//---------------------------------------------------------------------------
__fastcall TLogMemo::~TLogMemo()
{
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
    (Font->Pitch != fpDefault) ||
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
#ifndef DESIGN_ONLY
  USEDPARAM(Sender);
  assert(Sender && (Sender == SessionLog));
#endif
  UpdateFromLog();
}
//---------------------------------------------------------------------------
void __fastcall TLogMemo::UpdateFromLog()
{
#ifndef DESIGN_ONLY
  if (SessionLog && Parent && !Application->Terminated)
  {
    assert(FIndexes->Count == Lines->Count);
    FUpdating = true;
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
            if (Parent) Lines->Delete(0);
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
            if (Parent) SelAttributes->Color = SessionLog->Color[LastIndex];
            FIndexes->Add((void*) LastIndex);
            try
            {
              // this usually fails when log window is closed while
              // new line is being added (control has no parent)
              if (Parent)
              {
                if (SessionLog->Line[LastIndex].Pos("\r"))
                {
                  Lines->Add(StringReplace(SessionLog->Line[LastIndex], "\r", "",
                    TReplaceFlags() << rfReplaceAll));
                }
                else
                {
                  Lines->Add(SessionLog->Line[LastIndex]);
                }
              }
            }
            catch(...)
            {
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

      assert(!Parent || FIndexes->Count == Lines->Count);
    }
    __finally
    {
      FUpdating = false;
    }
  }
#endif
}
//---------------------------------------------------------------------------
void __fastcall TLogMemo::ReloadFromLog()
{
  if (Parent)
  {
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
  assert((Index >= 0) && (Index < Lines->Count) && (FIndexes->Count == Lines->Count));
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
      //SendMessage(Handle, EM_LINESCROLL, 0, -LinesVisible+1);
    }
    HideCaret(Handle);
  }
}
//---------------------------------------------------------------------------
int __fastcall TLogMemo::GetLinesVisible()
{
  HFONT OldFont;
  void *DC;
  TTextMetric TM;
  TRect Rect;

  DC = GetDC(Handle);
  OldFont = SelectObject(DC, Font->Handle);

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
void __fastcall TLogMemo::InitiateAction()
{
  TCustomRichEdit::InitiateAction();
  /*if (FNeedsRepaint)
  {
    FNeedsRepaint = false;
    Refresh();
  } */
}
//---------------------------------------------------------------------------
void __fastcall TLogMemo::Change()
{
  if (Parent && Visible && !Application->Terminated)
  {
    TCustomRichEdit::Change();
  }
}
