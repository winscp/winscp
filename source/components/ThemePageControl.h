//---------------------------------------------------------------------------
#ifndef ThemePageControlH
#define ThemePageControlH
//---------------------------------------------------------------------------
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
enum TThemeTabSheetButtons { ttbNone, ttbClose, ttbDropDown };
//---------------------------------------------------------------------------
class TThemeTabSheet : public TTabSheet
{
public:
  __fastcall TThemeTabSheet(TComponent * Owner);

  __property bool Shadowed = { read = FShadowed, write = SetShadowed };
  __property TThemeTabSheetButtons Button = { read = FButton, write = SetButton };

private:
  void __fastcall SetShadowed(bool Value);
  void __fastcall SetButton(TThemeTabSheetButtons Value);
  void __fastcall Invalidate();

  bool FShadowed;
  TThemeTabSheetButtons FButton;
};
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TPageControlTabButtonClick)(TPageControl * Sender, int Index);
//---------------------------------------------------------------------------
class TThemePageControl : public TPageControl
{
friend class TThemeTabSheet;

__published:
  __property TPageControlTabButtonClick OnTabButtonClick = { read = FOnTabButtonClick, write = FOnTabButtonClick };

public:
  __fastcall TThemePageControl(TComponent * Owner);

  __property TThemeTabSheet * Pages[int Index] = { read = GetPage };
  __property TThemeTabSheet * ActivePage = { read = GetActivePage };

  int __fastcall GetTabsHeight();
  UnicodeString __fastcall FormatCaptionWithTabButton(const UnicodeString & Caption);
  TRect __fastcall TabButtonRect(int Index);

protected:
  virtual void __fastcall PaintWindow(HDC DC);
  DYNAMIC bool __fastcall CanChange();
  DYNAMIC void __fastcall Change();
  DYNAMIC void __fastcall MouseMove(TShiftState Shift, int X, int Y);
  virtual void __fastcall Dispatch(void * Message);
  #ifdef _DEBUG
  virtual void __fastcall RequestAlign();
  #endif

private:
  void __fastcall DrawThemesXpTab(HDC DC, int Tab);
  void __fastcall DrawThemesXpTabItem(HDC DC, int Item, const TRect & Rect, bool Body, int State, bool Selected);
  void __fastcall DrawTabItem(HDC DC, int Item, TRect TabRect, TRect Rect, bool Selected, bool Shadowed);
  void __fastcall DrawThemesPart(HDC DC, int PartId, int StateId, LPCWSTR PartNameID, LPRECT Rect);
  void __fastcall InvalidateTab(int Index);
  int __fastcall TabButtonSize();
  int __fastcall GetCrossPadding();
  int __fastcall IndexOfTabButtonAt(int X, int Y);
  void __fastcall ItemContentsRect(int Item, TRect & Rect);
  bool __fastcall HasItemImage(int Item);
  void __fastcall ItemTextRect(int Item, TRect & Rect);
  void __fastcall ItemTabRect(int Item, TRect & Rect);
  TThemeTabSheetButtons __fastcall GetTabButton(int Index);
  void UpdateHotButton(int & Ref, int Index);
  void DrawCross(HDC DC, int Width, COLORREF Color, const TRect & Rect);
  void DrawDropDown(HDC DC, int Radius, int X, int Y, COLORREF Color, int Grow);
  void __fastcall WMLButtonDown(TWMLButtonDown & Message);
  bool IsHotButton(int Index);
  TThemeTabSheet * GetPage(int Index);
  TThemeTabSheet * GetActivePage();

  int FOldTabIndex;
  int FHotTabButton;
  int FClickedButton;
  TPageControlTabButtonClick FOnTabButtonClick;
};
//---------------------------------------------------------------------------
#endif
