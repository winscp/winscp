//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <vsstyle.h>
#include <memory>
#include "ThemePageControl.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
// Based on
// http://www.codeproject.com/script/Articles/ViewDownloads.aspx?aid=6355
//---------------------------------------------------------------------------
//#define USE_DEFAULT_XP_TOPTAB     // XP top tab is drawn only for test purpose. To use default, uncoment this line
//---------------------------------------------------------------------------
// constant string definitions here (or you can put it into resource string table)
#define IDS_UTIL_TAB            L"TAB"
#define IDS_UTIL_UXTHEME        L"UxTheme.dll"
#define IDS_UTIL_THEMEACT       "IsThemeActive"
#define IDS_UTIL_THEMEOPN       "OpenThemeData"
#define IDS_UTIL_THEMEBCKG      "DrawThemeBackground"
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
    TComponentClass classes[1] = {__classid(TThemePageControl)};
    RegisterComponents(L"Scp", classes, 0);
  }
}
//----------------------------------------------------------------------------------------------------------
__fastcall TThemeTabSheet::TThemeTabSheet(TComponent * Owner) :
  TTabSheet(Owner)
{
  FShadowed = false;
}
//----------------------------------------------------------------------------------------------------------
void __fastcall TThemeTabSheet::SetShadowed(bool Value)
{
  if (Shadowed != Value)
  {
    FShadowed = Value;

    TThemePageControl * ThemePageControl = dynamic_cast<TThemePageControl *>(Parent);
    if (ALWAYS_TRUE(ThemePageControl != NULL))
    {
      ThemePageControl->InvalidateTab(TabIndex);
    }
    else
    {
      Parent->Invalidate();
    }
  }
}
//----------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------
__fastcall TThemePageControl::TThemePageControl(TComponent * Owner) :
  TPageControl(Owner)
{
  FOldTabIndex = -1;
}
//----------------------------------------------------------------------------------------------------------
int __fastcall TThemePageControl::GetTabsHeight()
{
  // Calculated height includes tab/contents separator line on Windows 7/8,
  // but not on Windows XP

  TRect Rect = GetClientRect();
  ::SendMessage(Handle, TCM_ADJUSTRECT, FALSE, (LPARAM)&Rect);
  int Result = Rect.Top - 1;

  // two different ways to calculate the same, not sure which one is more reliable,
  // so we want to know in case they differ
  if (ALWAYS_TRUE(PageCount >= 0))
  {
    TRect Rect = TabRect(0);
    int Result2 = Rect.Bottom + 1;
    if (ALWAYS_FALSE(Result != Result2))
    {
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

  // TODO use GetClipBox

  TRect PageRect = GetClientRect();

  // 1st paint the tab body
  TRect ClientRect = PageRect;
  ::SendMessage(Handle, TCM_ADJUSTRECT, FALSE, (LPARAM)&PageRect);

  ClientRect.Top = PageRect.Top - 2;
  DrawThemesXpTabItem(DC, -1, ClientRect, true, 0);

  // 2nd paint the inactive tabs

  TPoint Point = ScreenToClient(Mouse->CursorPos);
  int HotIndex = IndexOfTabAt(Point.X, Point.Y);
  int SelectedIndex = TabIndex;

  for (int Tab = 0; Tab < PageCount; Tab++)
  {
    if (Tab != SelectedIndex)
    {
      TThemeTabSheet * ThemeTabSheet = dynamic_cast<TThemeTabSheet *>(Pages[Tab]);
      bool Shadowed = (ThemeTabSheet != NULL) ? ThemeTabSheet->Shadowed : false;
      TRect Rect = TabRect(Tab);
      int State = (Tab == HotIndex ? TIS_HOT : (Shadowed ? TIS_DISABLED : TIS_NORMAL));
      DrawThemesXpTabItem(DC, Tab, Rect, false, State);
    }
  }

  if (SelectedIndex >= 0)
  {
    // 3rd paint the active selected tab
    TRect Rect = TabRect(SelectedIndex);
    Rect.Inflate(2, 2);
    Rect.Bottom--;
    DrawThemesXpTabItem(DC, SelectedIndex, Rect, false, TIS_SELECTED);
  }
}
//----------------------------------------------------------------------------------------------------------
// This function draws Themes Tab control parts: a) Tab-Body and b) Tab-tabs
void __fastcall TThemePageControl::DrawThemesXpTabItem(HDC DC, int Item,
  const TRect & Rect, bool Body, int State)
{
  TSize Size = Rect.Size;

  // Draw background
  HDC DCMem = CreateCompatibleDC(DC);
  HBITMAP BitmapMem = CreateCompatibleBitmap(DC, Size.Width, Size.Height);
  HBITMAP BitmapOld = (HBITMAP)SelectObject(DCMem, BitmapMem);

  TRect RectMem(0, 0, Size.Width, Size.Height);
  if (!Body && (State == TIS_SELECTED))
  {
    RectMem.Bottom++;
  }

  if (Body)
  {
    DrawThemesPart(DCMem, TABP_PANE, State, IDS_UTIL_TAB, &RectMem);
  }
  else
  {
    DrawThemesPart(DCMem, TABP_TABITEM, State, IDS_UTIL_TAB, &RectMem);
  }

  // Init some extra parameters
  BITMAPINFO BitmapInfo;
  // Fill local pixel arrays
  ZeroMemory(&BitmapInfo, sizeof(BITMAPINFO));
  BITMAPINFOHEADER & BitmapInfoHeader = BitmapInfo.bmiHeader;
  BitmapInfoHeader.biSize = sizeof(BITMAPINFOHEADER);
  BitmapInfoHeader.biCompression = BI_RGB;
  BitmapInfoHeader.biPlanes = 1;
  // force as RGB: 3 bytes,24 bits -> good for rotating bitmap in any resolution
  BitmapInfoHeader.biBitCount = 24;
  BitmapInfoHeader.biWidth = Size.Width;
  BitmapInfoHeader.biHeight = Size.Height;

  if (!Body && (Item >= 0))
  {
    if ((State == TIS_SELECTED))
    {
      RectMem.Bottom--;
    }

    DrawTabItem(DCMem, Item, RectMem, (State == TIS_SELECTED), (State == TIS_DISABLED));
  }

  // Blit image to the screen
  BitBlt(DC, Rect.Left, Rect.Top, Size.Width, Size.Height, DCMem, 0, 0, SRCCOPY);
  SelectObject(DCMem, BitmapOld);
  DeleteObject(BitmapMem);
  DeleteDC(DCMem);
}
//----------------------------------------------------------------------------------------------------------
// draw tab item context: possible icon and text
void __fastcall TThemePageControl::DrawTabItem(HDC DC, int Item, TRect Rect,
  bool Selected, bool Shadowed)
{
  if (Selected)
  {
    Rect.Bottom -= 1;
  }
  else
  {
    Rect.Bottom += 2;
  }

  Rect.Left += 6;
  Rect.Top += 2 + (Selected ? 1 : 3);

  UnicodeString Text = Pages[Item]->Caption;

  if ((Images != NULL) && (Pages[Item]->ImageIndex >= 0))
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
    Rect.Left += Images->Width + 3;
  }
  else
  {
    Rect.Left -= 2;
  }

  int OldMode = SetBkMode(DC, TRANSPARENT);
  if (!Text.IsEmpty())
  {
    HFONT OldFont = (HFONT)SelectObject(DC, Font->Handle);
    Rect.Right -= 3;
    wchar_t * Buf = new wchar_t[Text.Length() + 1 + 4];
    wcscpy(Buf, Text.c_str());
    TRect TextRect(0, 0, Rect.Right - Rect.Left, 20);
    ::DrawText(DC, Buf, -1, &TextRect, DT_CALCRECT | DT_SINGLELINE | DT_MODIFYSTRING | DT_END_ELLIPSIS);

    OffsetRect(&Rect, 0, (Selected ? 0 : -2));
    DrawText(DC, Buf, -1, &Rect, DT_NOPREFIX | DT_CENTER);
    delete[] Buf;
    SelectObject(DC, OldFont);
  }

  SetBkMode(DC, OldMode);
}
//----------------------------------------------------------------------------------------------------------
void __fastcall TThemePageControl::DrawThemesPart(HDC DC, int PartId,
  int StateId, LPCWSTR PartNameID, LPRECT Rect)
{
  HTHEME Theme = OpenThemeData(NULL, PartNameID);
  if (Theme != 0)
  {
    DrawThemeBackground(Theme, DC, PartId, StateId, Rect, NULL);
    CloseThemeData(Theme);
  }
}
//==========================================================================================================
// these two messages are necessary only to properly redraw deselected tab background, because
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
//----------------------------------------------------------------------------------------------------------
