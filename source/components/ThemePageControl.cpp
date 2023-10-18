//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <vsstyle.h>
#include <memory>
#include <PasTools.hpp>
#include <TBXOfficeXPTheme.hpp>
#include <TBX.hpp>
#include <StrUtils.hpp>
#include <CustomWinConfiguration.h>
#include "ThemePageControl.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
// Based on
// https://www.codeproject.com/Articles/6355/XP-Themes-Tab-Control-in-any-orientation
//---------------------------------------------------------------------------
#define IDS_UTIL_TAB            L"TAB"
//---------------------------------------------------------------------------
static inline void ValidCtrCheck(TThemePageControl *)
{
  new TThemePageControl(NULL);
}
//---------------------------------------------------------------------------
namespace Themepagecontrol
{
  void __fastcall PACKAGE Register()
  {
    TComponentClass classes[2] = {__classid(TThemePageControl), __classid(TThemeTabSheet)};
    RegisterComponents(L"Scp", classes, 1);
  }
}
//----------------------------------------------------------------------------------------------------------
__fastcall TThemeTabSheet::TThemeTabSheet(TComponent * Owner) :
  TTabSheet(Owner)
{
  FShadowed = false;
  FButton = ttbNone;
  FCaptionTruncation = tttNone;
}
//----------------------------------------------------------------------------------------------------------
TThemePageControl * TThemeTabSheet::GetParentPageControl()
{
  return DebugNotNull(dynamic_cast<TThemePageControl *>(Parent));
}
//----------------------------------------------------------------------------------------------------------
void __fastcall TThemeTabSheet::Invalidate()
{
  TThemePageControl * ThemePageControl = GetParentPageControl();
  if (DebugAlwaysTrue(ThemePageControl != NULL))
  {
    ThemePageControl->InvalidateTab(TabIndex);
  }
  else
  {
    Parent->Invalidate();
  }
}
//----------------------------------------------------------------------------------------------------------
void __fastcall TThemeTabSheet::SetShadowed(bool Value)
{
  if (Shadowed != Value)
  {
    FShadowed = Value;
    Invalidate();
  }
}
//----------------------------------------------------------------------------------------------------------
void __fastcall TThemeTabSheet::SetButton(TThemeTabSheetButtons Value)
{
  if (Button != Value)
  {
    FButton = Value;
    Invalidate();
  }
}
//----------------------------------------------------------------------------------------------------------
void TThemeTabSheet::SetBaseCaption(const UnicodeString & value)
{
  if (FBaseCaption != value)
  {
    FBaseCaption = value;
    UpdateCaption();
  }
}
//----------------------------------------------------------------------------------------------------------
UnicodeString TThemeTabSheet::TruncatedCaption()
{
  UnicodeString Result = FBaseCaption;
  TThemePageControl * ParentPageControl = GetParentPageControl();
  if (ParentPageControl->FSessionTabShrink > 0)
  {
    if (FCaptionTruncation == tttNone)
    {
      // noop
    }
    else if (FCaptionTruncation == tttEllipsis)
    {
      if (ParentPageControl->FSessionTabShrink == 1)
      {
        Result = Result.SubString(1, 1);
      }
      else if (ParentPageControl->FSessionTabShrink < Result.Length())
      {
        Result = Result.SubString(1, ParentPageControl->FSessionTabShrink - 1) + Ellipsis;
      }
    }
    else if (DebugAlwaysTrue(FCaptionTruncation == tttNoText))
    {
      Result = EmptyStr;
    }
  }
  return Result;
}
//----------------------------------------------------------------------------------------------------------
void TThemeTabSheet::UpdateCaption()
{
  UnicodeString ACaption = TruncatedCaption();

  TThemePageControl * ParentPageControl = GetParentPageControl();

  if (UseThemes() && (Button != ttbNone))
  {
    int OrigWidth = ParentPageControl->Canvas->TextWidth(ACaption);
    int TabButtonWidth = ParentPageControl->TabButtonSize();
    while (ParentPageControl->Canvas->TextWidth(ACaption) < OrigWidth + TabButtonWidth)
    {
      ACaption += L" ";
    }
  }
  Caption = ACaption;
  ParentPageControl->TabChanged(TabIndex);
}
//----------------------------------------------------------------------------------------------------------
UnicodeString TThemeTabSheet::GetBaseCaption()
{
  DebugAssert(StartsStr(FBaseCaption, Caption));
  DebugAssert(RightStr(Caption, Caption.Length() - FBaseCaption.Length()).Trim().Length() == 0);
  return FBaseCaption;
}
//----------------------------------------------------------------------------------------------------------
void TThemeTabSheet::SetCaptionTruncation(TThemeTabCaptionTruncation Value)
{
  if (FCaptionTruncation != Value)
  {
    FCaptionTruncation = Value;
    UpdateCaption();
  }
}
//----------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------
__fastcall TThemePageControl::TThemePageControl(TComponent * Owner) :
  TPageControl(Owner)
{
  FOldTabIndex = -1;
  FHotTabButton = -1;
  FClickedButton = -1;
  FSessionTabShrink = 0;
  FOnTabButtonClick = NULL;
  FOnTabHint = NULL;
  FActiveTabTheme = NULL;
}
//----------------------------------------------------------------------------------------------------------
int __fastcall TThemePageControl::GetTabsHeight()
{
  // Calculated height includes tab/contents separator line on Windows 7/8,
  // but not on Windows XP

  TRect Rect = GetClientRect();
  ::SendMessage(Handle, TCM_ADJUSTRECT, FALSE, (LPARAM)&Rect);
  int Result = Rect.Top - 1;

  // Two different ways to calculate the same, not sure which one is more reliable,
  // so we want to know in case they differ.
  if (DebugAlwaysTrue(PageCount >= 0))
  {
    TRect Rect = TabRect(0);
    int Result2 = Rect.Bottom + 1;
    // On Windows 10 with 200% scaling, the first is 40, the second is 42.
    // With 250% scaling it's 50 vs 53.
    // Using the larger.
    if (Result2 > Result)
    {
      DebugAssert(IsWin10());
      Result = Result2;
    }
  }

  return Result;
}
//----------------------------------------------------------------------------------------------------------
void __fastcall TThemePageControl::PaintWindow(HDC DC)
{
  // Themes not enabled, give up
  if (!UseThemes())
  {
    TPageControl::PaintWindow(DC);
    return;
  }

  HTHEME Theme = OpenThemeData(NULL, IDS_UTIL_TAB);

  // TODO use GetClipBox

  TRect PageRect = GetClientRect();

  // 1st paint the tab body
  TRect ClientRect = PageRect;
  ::SendMessage(Handle, TCM_ADJUSTRECT, FALSE, (LPARAM)&PageRect);

  ClientRect.Top = PageRect.Top - 2;
  DrawThemeBackground(Theme, DC, TABP_PANE, 0, &ClientRect, NULL);

  // 2nd paint the inactive tabs

  int SelectedIndex = TabIndex; // optimization

  for (int Tab = 0; Tab < PageCount; Tab++)
  {
    if (Tab != SelectedIndex)
    {
      DrawThemesXpTab(DC, Theme, Tab);
    }
  }

  if (SelectedIndex >= 0)
  {
    DrawThemesXpTab(DC, Theme, TabIndex);
  }

  CloseThemeData(Theme);
}
//----------------------------------------------------------------------------------------------------------
TThemeTabSheetButtons __fastcall TThemePageControl::GetTabButton(int Index)
{
  TThemeTabSheet * ThemeTabSheet = dynamic_cast<TThemeTabSheet *>(Pages[Index]);
  return (UseThemes() && (ThemeTabSheet != NULL)) ? ThemeTabSheet->Button : ttbNone;
}
//----------------------------------------------------------------------------------------------------------
void __fastcall TThemePageControl::DrawThemesXpTab(HDC DC, HTHEME Theme, int Tab)
{
  TThemeTabSheet * ThemeTabSheet = dynamic_cast<TThemeTabSheet *>(Pages[Tab]);
  bool Shadowed = (ThemeTabSheet != NULL) ? ThemeTabSheet->Shadowed : false;
  TRect Rect = TabRect(Tab);
  ItemTabRect(Tab, Rect);
  int State;
  if (Tab != TabIndex)
  {
    TPoint Point = ScreenToClient(Mouse->CursorPos);
    int HotIndex = IndexOfTabAt(Point.X, Point.Y);
    State = (Tab == HotIndex ? TIS_HOT : (Shadowed ? TIS_DISABLED : TIS_NORMAL));
  }
  else
  {
    State = TIS_SELECTED;
  }
  DrawThemesXpTabItem(DC, Theme, Tab, Rect, State, Shadowed);
}
//----------------------------------------------------------------------------------------------------------
static TTBXItemInfo GetItemInfo(int State)
{
  TTBXItemInfo ItemInfo;
  memset(&ItemInfo, 0, sizeof(ItemInfo));
  ItemInfo.Enabled = true;
  ItemInfo.ViewType =
    VT_TOOLBAR | TVT_EMBEDDED |
    FLAGMASK(State == TIS_SELECTED, ISF_SELECTED);
  return ItemInfo;
}
//----------------------------------------------------------------------------------------------------------
void __fastcall TThemePageControl::DrawThemesXpTabItem(
  HDC DC, HTHEME Theme, int Item, const TRect & Rect, int State, bool Shadowed)
{
  TRect PaintRect = Rect;
  bool Selected = (State == TIS_SELECTED);
  if (Selected)
  {
    PaintRect.Bottom++;
  }

  if (Selected && (ActiveTabTheme != NULL))
  {
    std::unique_ptr<TCanvas> CanvasMem(new TCanvas());
    CanvasMem->Handle = DC;
    ActiveTabTheme->PaintFrame(CanvasMem.get(), PaintRect, GetItemInfo(State));
  }
  else
  {
    int PartID = (Item == 0) ? TABP_TABITEMLEFTEDGE : TABP_TABITEM;
    DrawThemeBackground(Theme, DC, PartID, State, &PaintRect, NULL);
  }

  if (Item >= 0)
  {
    DrawTabItem(DC, Item, Rect, State, Shadowed);
  }
}
//----------------------------------------------------------------------------------------------------------
void __fastcall TThemePageControl::ItemTabRect(int Item, TRect & Rect)
{
  if (Item == TabIndex)
  {
    // Countered in TabButtonRect
    Rect.Inflate(2, 2);
    Rect.Bottom--;
  }
}
//----------------------------------------------------------------------------------------------------------
void __fastcall TThemePageControl::ItemContentsRect(int Item, TRect & Rect)
{
  bool Selected = (Item == TabIndex);

  Rect.Left += 6;
  Rect.Top += 2;

  if (Selected)
  {
    Rect.Bottom -= 2;
    Rect.Top += 1;
  }
  else
  {
    Rect.Bottom += 2;
    Rect.Top += 3;
  }
}
//----------------------------------------------------------------------------------------------------------
bool __fastcall TThemePageControl::HasItemImage(int Item)
{
  return (Images != NULL) && (Pages[Item]->ImageIndex >= 0);
}
//----------------------------------------------------------------------------------------------------------
void __fastcall TThemePageControl::ItemTextRect(int Item, TRect & Rect)
{
  if (HasItemImage(Item))
  {
    Rect.Left += Images->Width + 3;
  }
  else
  {
    Rect.Left -= 2;
  }
  Rect.Right -= 3;
  OffsetRect(&Rect, 0, ((Item == TabIndex) ? 0 : -2));
}
//----------------------------------------------------------------------------------------------------------
void TThemePageControl::DrawCross(HDC DC, int Width, COLORREF Color, const TRect & Rect)
{
  HPEN Pen = CreatePen(PS_SOLID, Width, Color);
  HPEN OldPen = static_cast<HPEN>(SelectObject(DC, Pen));
  // To-and-back - to make both ends look the same
  MoveToEx(DC, Rect.Left, Rect.Bottom - 1, NULL);
  LineTo(DC, Rect.Right - 1, Rect.Top);
  LineTo(DC, Rect.Left, Rect.Bottom - 1);
  MoveToEx(DC, Rect.Left, Rect.Top, NULL);
  LineTo(DC, Rect.Right - 1, Rect.Bottom - 1);
  LineTo(DC, Rect.Left, Rect.Top);
  SelectObject(DC, OldPen);
  DeleteObject(Pen);
}
//----------------------------------------------------------------------------------------------------------
void TThemePageControl::DrawDropDown(HDC DC, int Radius, int X, int Y, COLORREF Color, int Grow)
{
  // Optimized for even-sized Rect (100% scaling), may need adjustments for even-sized to correctly center
  TPoint Points[] = {
    Point(X - Radius - 1 - Grow, Y), Point(X + Radius + Grow, Y),
    Point(X, Y + Radius + Grow), Point(X - 1, Y + Radius + Grow)
  };
  HBRUSH Brush = CreateSolidBrush(Color);
  HPEN Pen = CreatePen(PS_SOLID, 1, Color);
  HGDIOBJ OldBrush = SelectObject(DC, Brush);
  HGDIOBJ OldPen = SelectObject(DC, Pen);
  Polygon(DC, Points, LENOF(Points));
  SelectObject(DC, OldPen);
  SelectObject(DC, OldBrush);
}
//----------------------------------------------------------------------------------------------------------
// draw tab item context: possible icon and text
void __fastcall TThemePageControl::DrawTabItem(HDC DC, int Item, TRect Rect, int State, bool Shadowed)
{
  ItemContentsRect(Item, Rect);

  UnicodeString Text = Pages[Item]->Caption;
  bool Selected = (State == TIS_SELECTED);

  if (HasItemImage(Item))
  {
    int Left;
    if (!Text.IsEmpty())
    {
      Left = Rect.Left + (Selected ? 2 : 0);
    }
    else
    {
      Left = (Rect.Right - Images->Width - Rect.Left) / 2;
    }
    int Y = ((Rect.Top + Rect.Bottom - Images->Height) / 2) - 1 + (Selected ? 0 : -2);
    std::unique_ptr<TCanvas> Canvas(new TCanvas());
    Canvas->Handle = DC;
    Images->Draw(Canvas.get(), Left, Y, Pages[Item]->ImageIndex, !Shadowed);
  }

  int TextHeight = 20;
  int OldMode = SetBkMode(DC, TRANSPARENT);
  if (!Text.IsEmpty())
  {
    ItemTextRect(Item, Rect);
    if (Selected && (ActiveTabTheme != NULL))
    {
      SetTextColor(DC, ActiveTabTheme->GetItemTextColor(GetItemInfo(State)));
    }
    HFONT OldFont = (HFONT)SelectObject(DC, Font->Handle);
    wchar_t * Buf = new wchar_t[Text.Length() + 1 + 4];
    wcscpy(Buf, Text.c_str());
    TRect TextRect(0, 0, Rect.Right - Rect.Left, TextHeight);
    // Truncates too long texts with ellipsis
    ::DrawText(DC, Buf, -1, &TextRect, DT_CALCRECT | DT_SINGLELINE | DT_MODIFYSTRING | DT_END_ELLIPSIS);

    DrawText(DC, Buf, -1, &Rect, DT_NOPREFIX | DT_CENTER);
    delete[] Buf;

    TThemeTabSheetButtons Button = GetTabButton(Item);
    if (Button != ttbNone)
    {
      Rect = TabButtonRect(Item);

      TTBXItemInfo ButtonItemInfo = GetItemInfo(State);

      if (IsHotButton(Item))
      {
        ButtonItemInfo.HoverKind = hkMouseHover;

        // Untimatelly, merge both branches to use PaintFrame (just with a different theme) (and drop GetSelectedBodyColor)
        if (Selected && (ActiveTabTheme != NULL))
        {
          std::unique_ptr<TCanvas> CanvasMem(new TCanvas());
          CanvasMem->Handle = DC;
          CurrentTheme->PaintFrame(CanvasMem.get(), Rect, ButtonItemInfo);
        }
        else
        {
          HBRUSH Brush = CreateSolidBrush(GetSelectedBodyColor());
          FillRect(DC, &Rect, Brush);
          DeleteObject(Brush);

          HPEN Pen = CreatePen(PS_SOLID, 1, ColorToRGB(clHighlight));
          HPEN OldPen = static_cast<HPEN>(SelectObject(DC, Pen));
          Rectangle(DC, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
          SelectObject(DC, OldPen);
          DeleteObject(Pen);
        }
      }

      COLORREF BackColor = GetPixel(DC, Rect.Left + (Rect.Width() / 2), Rect.Top + (Rect.Height() / 2));
      COLORREF ShapeColor;
      if (Selected && (ActiveTabTheme != NULL))
      {
        ShapeColor = ColorToRGB(ActiveTabTheme->GetItemTextColor(ButtonItemInfo));
      }
      else
      {
        ShapeColor = ColorToRGB(Font->Color);
      }
      #define BlendValue(FN) (((4 * static_cast<int>(FN(BackColor))) + static_cast<int>(FN(ShapeColor))) / 5)
      COLORREF BlendColor = RGB(BlendValue(GetRValue), BlendValue(GetGValue), BlendValue(GetBValue));
      #undef BlendValue

      if (Button == ttbClose)
      {
        int CrossPadding = GetCrossPadding();

        TRect CrossRect(Rect);
        CrossRect.Inflate(-CrossPadding, -CrossPadding);

        int CrossWidth = ScaleByTextHeight(this, 1);
        DrawCross(DC, CrossWidth + 1, BlendColor, CrossRect);
        DrawCross(DC, CrossWidth, ShapeColor, CrossRect);
      }
      else if (DebugAlwaysTrue(Button == ttbDropDown))
      {
        // See TTBXOfficeXPTheme.PaintDropDownArrow
        int Radius = ScaleByTextHeight(this, 2);
        int X = ((Rect.Left + Rect.Right)) / 2;
        int Y = ((Rect.Top + Rect.Bottom) / 2) - (Radius * 2 / 3);
        DrawDropDown(DC, Radius, X, Y, BlendColor, 1);
        DrawDropDown(DC, Radius, X, Y, ShapeColor, 0);
      }
    }

    SelectObject(DC, OldFont);
  }

  SetBkMode(DC, OldMode);
}
//----------------------------------------------------------------------------------------------------------
int __fastcall TThemePageControl::TabButtonSize()
{
  return ScaleByTextHeight(this, 16);
}
//----------------------------------------------------------------------------------------------------------
int __fastcall TThemePageControl::GetCrossPadding()
{
  return ScaleByTextHeight(this, 4);
}
//----------------------------------------------------------------------------------------------------------
TRect __fastcall TThemePageControl::TabButtonRect(int Index)
{
  TRect Rect = TabRect(Index);
  ItemTabRect(Index, Rect);
  ItemContentsRect(Index, Rect);
  ItemTextRect(Index, Rect);

  int ATabButtonSize = TabButtonSize();
  int CrossPadding = GetCrossPadding();

  TEXTMETRIC TextMetric;
  Canvas->Font = Font;
  GetTextMetrics(Canvas->Handle, &TextMetric);

  Rect.Top += TextMetric.tmAscent - ATabButtonSize + CrossPadding;
  Rect.Left = Rect.Right - ATabButtonSize - ScaleByTextHeight(this, 1);
  if (Index == TabIndex)
  {
    // To counter Inflate(2, 2) in ItemTabRect
    Rect.Left -= 2;
  }
  Rect.Right = Rect.Left + ATabButtonSize;
  Rect.Bottom = Rect.Top + ATabButtonSize;
  return Rect;
}
//----------------------------------------------------------------------------------------------------------
bool TThemePageControl::IsHotButton(int Index)
{
  // This was an attempt to allow tracking close buttons, even while drop down button menu is popped,
  // but MouseMove does not trigger then.
  return (Index == FClickedButton) || (Index == FHotTabButton);
}
//----------------------------------------------------------------------------------------------------------
void TThemePageControl::TabChanged(int Index)
{
  // When the "clicked" tab changes, it's probably not anymore the tab that was actually clicked.
  // For example, when the last tab is closed, it's replaced with either local-local tab (without the X button),
  // or removed altogether. The Login dialog pops up and when new session is opened, its tab's X button is rendered clicked,
  // until connection openning finishes (and WMLButtonDown finishes).
  if (Index == FClickedButton)
  {
    UpdateHotButton(FClickedButton, -1);
  }
}
//----------------------------------------------------------------------------------------------------------
void TThemePageControl::UpdateHotButton(int & Ref, int Index)
{
  if (Ref != Index)
  {
    bool WasHot = (Index >= 0) && IsHotButton(Index);
    int Prev = Ref;
    Ref = Index;
    if ((Prev >= 0) && !IsHotButton(Prev))
    {
      InvalidateTab(Prev);
    }
    if ((Index >= 0) && !WasHot)
    {
      InvalidateTab(Index);
    }
  }
}
//----------------------------------------------------------------------------------------------------------
void __fastcall TThemePageControl::MouseMove(TShiftState Shift, int X, int Y)
{
  TPageControl::MouseMove(Shift, X, Y);
  UpdateHotButton(FHotTabButton, IndexOfTabButtonAt(X, Y));
}
//----------------------------------------------------------------------------------------------------------
int __fastcall TThemePageControl::IndexOfTabButtonAt(int X, int Y)
{
  int Result = IndexOfTabAt(X, Y);
  if ((Result < 0) ||
      !GetTabButton(Result) ||
      !TabButtonRect(Result).Contains(TPoint(X, Y)))
  {
    Result = -1;
  }
  return Result;
}
//----------------------------------------------------------------------------------------------------------
bool __fastcall TThemePageControl::CanChange()
{
  FOldTabIndex = ActivePageIndex;

  return TPageControl::CanChange();
}
//----------------------------------------------------------------------------------------------------------
void __fastcall TThemePageControl::InvalidateTab(int Index)
{
  if (HandleAllocated())
  {
    TRect Rect = TabRect(Index);
    if (Index == TabIndex)
    {
      Rect.Inflate(2, 2);
    }
    // Original code was invalidating range against parent window
    // (recalculating coordinates first)
    InvalidateRect(Handle, &Rect, true);
  }
}
//----------------------------------------------------------------------------------------------------------
void __fastcall TThemePageControl::Change()
{
  // note that TabIndex yields correct value already here,
  // while ActivePageIndex is not updated yet
  if ((FOldTabIndex >= 0) && (FOldTabIndex != TabIndex) && UseThemes())
  {
    InvalidateTab(FOldTabIndex);
  }

  TPageControl::Change();
}
//---------------------------------------------------------------------------
void __fastcall TThemePageControl::WMLButtonDown(TWMLButtonDown & Message)
{
  int Index = IndexOfTabButtonAt(Message.XPos, Message.YPos);
  if (Index >= 0)
  {
    Message.Result = 1;
    if (FOnTabButtonClick != NULL)
    {
      UpdateHotButton(FClickedButton, Index);
      try
      {
        FOnTabButtonClick(this, Index);
      }
      __finally
      {
        UpdateHotButton(FClickedButton, -1);
      }
    }
  }
  else
  {
    TPageControl::Dispatch(&Message);
  }
}
//---------------------------------------------------------------------------
void TThemePageControl::CMHintShow(TCMHintShow & HintShow)
{
  TPageControl::Dispatch(&HintShow);
  if (OnTabHint != NULL)
  {
    int Tab = IndexOfTabAt(HintShow.HintInfo->CursorPos.x, HintShow.HintInfo->CursorPos.y);
    OnTabHint(this, Tab, HintShow.HintInfo->HintStr);
    HintShow.HintInfo->CursorRect = TabRect(Tab);
  }
}
//---------------------------------------------------------------------------
void __fastcall TThemePageControl::Dispatch(void * Message)
{
  TMessage * M = reinterpret_cast<TMessage*>(Message);
  if (M->Msg == CM_MOUSELEAVE)
  {
    UpdateHotButton(FHotTabButton, -1);
    TPageControl::Dispatch(Message);
  }
  else if (M->Msg == WM_LBUTTONDOWN)
  {
    WMLButtonDown(*reinterpret_cast<TWMLButtonDown *>(M));
  }
  else if (M->Msg == WM_WANTS_SCREEN_TIPS)
  {
    M->Result = 1;
  }
  else if (M->Msg == CM_HINTSHOW)
  {
    CMHintShow(*reinterpret_cast<TCMHintShow *>(M));
  }
  else
  {
    TPageControl::Dispatch(Message);
  }
}
//----------------------------------------------------------------------------------------------------------
TThemeTabSheet * TThemePageControl::GetPage(int Index)
{
  return DebugNotNull(dynamic_cast<TThemeTabSheet *>(TPageControl::Pages[Index]));
}
//----------------------------------------------------------------------------------------------------------
TThemeTabSheet * TThemePageControl::GetActivePage()
{
  TTabSheet * TabSheet = TPageControl::ActivePage;
  TThemeTabSheet * Result = NULL;
  if (TabSheet != NULL)
  {
    Result = DebugNotNull(dynamic_cast<TThemeTabSheet *>(TabSheet));
  }
  return Result;
}
//----------------------------------------------------------------------------------------------------------
int TThemePageControl::TotalTabsWidth()
{
  TRect FirstTabRect = TabRect(0);
  TRect LastTabRect = TabRect(PageCount - 1);
  return -FirstTabRect.Left + LastTabRect.Right;
}
//----------------------------------------------------------------------------------------------------------
void TThemePageControl::UpdateTabsCaptionTruncation()
{
  DisableAlign();
  Tabs->BeginUpdate();
  try
  {
    FSessionTabShrink = 0;
    for (int Index = 0; Index < PageCount; Index++)
    {
      Pages[Index]->UpdateCaption();
    }

    int TabsWidth = TotalTabsWidth();
    int MaxWidth = ClientWidth - ScaleByTextHeight(this, 8); // arbitrary margin to avoid left/right buttons flicker
    if (TabsWidth > MaxWidth)
    {
      int NeedWidth = (TabsWidth - MaxWidth);
      int MaxLen = 0;
      int CaptionsWidth = 0;
      for (int Index = 0; Index < PageCount; Index++)
      {
        UnicodeString TabCaption = Pages[Index]->BaseCaption;
        MaxLen = std::max(MaxLen, TabCaption.Length());
        CaptionsWidth += Canvas->TextWidth(TabCaption);
      }

      bool Repeat;
      do
      {
        int NewShrink;
        if (FSessionTabShrink == 0)
        {
          NewShrink = MaxLen; // remove only new tab caption
        }
        else
        {
          NewShrink = FSessionTabShrink - 1;
        }

        if (NewShrink < 1)
        {
          Repeat = false;
        }
        else
        {
          FSessionTabShrink = NewShrink;
          int NewCaptionsWidth = 0;
          for (int Index = 0; Index < PageCount; Index++)
          {
            UnicodeString TabCaption = Pages[Index]->TruncatedCaption();
            NewCaptionsWidth += Canvas->TextWidth(TabCaption);
          }
          int GainedWidth = (CaptionsWidth - NewCaptionsWidth);
          Repeat = (GainedWidth < NeedWidth);
        }
      }
      while (Repeat);

      for (int Index = 0; Index < PageCount; Index++)
      {
        Pages[Index]->UpdateCaption();
      }
    }
  }
  __finally
  {
    Tabs->BeginUpdate();
    EnableAlign();
  }
}
//----------------------------------------------------------------------------------------------------------
void TThemePageControl::SetActiveTabTheme(TTBXTheme * value)
{
  if (FActiveTabTheme != value)
  {
    FActiveTabTheme = value;
    if (ActivePage != NULL)
    {
      ActivePage->Invalidate();
    }
  }
}
//----------------------------------------------------------------------------------------------------------
#ifdef _DEBUG
void __fastcall TThemePageControl::RequestAlign()
{
  TPageControl::RequestAlign();
}
#endif
//----------------------------------------------------------------------------------------------------------
