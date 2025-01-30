//---------------------------------------------------------------------------
#ifndef ThemePageControlH
#define ThemePageControlH
//---------------------------------------------------------------------------
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
enum TThemeTabSheetButtons { ttbNone, ttbClose, ttbDropDown };
enum TThemeTabCaptionTruncation { tttNone, tttEllipsis, tttNoText };
class TThemePageControl;
//---------------------------------------------------------------------------
class TThemeTabSheet : public TTabSheet
{
friend class TThemePageControl;

public:
  __fastcall TThemeTabSheet(TComponent * Owner);

  __property UnicodeString BaseCaption = { read = GetBaseCaption, write = SetBaseCaption };
  __property bool Shadowed = { read = FShadowed, write = SetShadowed };
  __property TThemeTabSheetButtons Button = { read = FButton, write = SetButton };
  __property TThemeTabCaptionTruncation CaptionTruncation = { read = FCaptionTruncation, write = SetCaptionTruncation };

private:
  void __fastcall SetShadowed(bool Value);
  void __fastcall SetButton(TThemeTabSheetButtons Value);
  void __fastcall Invalidate();
  void SetBaseCaption(const UnicodeString & value);
  UnicodeString GetBaseCaption();
  TThemePageControl * GetParentPageControl();
  void SetCaptionTruncation(TThemeTabCaptionTruncation Value);
  void UpdateCaption();
  UnicodeString TruncatedCaption();

  bool FShadowed;
  TThemeTabSheetButtons FButton;
  UnicodeString FBaseCaption;
  TThemeTabCaptionTruncation FCaptionTruncation;
};
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TPageControlTabButtonClick)(TPageControl * Sender, int Index);
typedef void __fastcall (__closure *TPageControlTabHint)(TPageControl * Sender, int Index, UnicodeString & Hint);
namespace Tbxthemes
{
  class TTBXTheme;
}
//---------------------------------------------------------------------------
class TThemePageControl : public TPageControl
{
friend class TThemeTabSheet;

__published:
  __property TPageControlTabButtonClick OnTabButtonClick = { read = FOnTabButtonClick, write = FOnTabButtonClick };
  __property TPageControlTabHint OnTabHint = { read = FOnTabHint, write = FOnTabHint };

public:
  __fastcall TThemePageControl(TComponent * Owner);

  __property TThemeTabSheet * Pages[int Index] = { read = GetPage };
  __property TThemeTabSheet * ActivePage = { read = GetActivePage };
  __property TTBXTheme * TabTheme = { read = FTabTheme, write = SetTabTheme };
  __property TTBXTheme * ActiveTabTheme = { read = FActiveTabTheme, write = SetActiveTabTheme };

  int __fastcall GetTabsHeight();
  TRect __fastcall TabButtonRect(int Index);
  int TotalTabsWidth();
  void UpdateTabsCaptionTruncation();

protected:
  virtual void __fastcall PaintWindow(HDC DC);
  DYNAMIC bool __fastcall CanChange();
  DYNAMIC void __fastcall Change();
  DYNAMIC void __fastcall MouseMove(TShiftState Shift, int X, int Y);
  virtual void __fastcall Dispatch(void * Message);
  void TabChanged(int Index);
  #ifdef _DEBUG
  virtual void __fastcall RequestAlign();
  #endif

private:
  void __fastcall DrawThemesXpTab(HDC DC, HTHEME Theme, int Tab);
  void __fastcall DrawThemesXpTabItem(
    HDC DC, HTHEME Theme, int Item, const TRect & Rect, int State, bool Selected, TTBXTheme * ATabTheme);
  void __fastcall DrawTabItem(HDC DC, int Item, TRect Rect, int State, bool Shadowed, TTBXTheme * ATabTheme);
  void __fastcall InvalidateTab(int Index);
  int __fastcall TabButtonSize();
  int __fastcall GetCrossPadding();
  int __fastcall IndexOfTabButtonAt(int X, int Y);
  void __fastcall ItemContentsRect(int Item, TRect & Rect);
  bool __fastcall HasItemImage(int Item);
  void __fastcall ItemTabRect(int Item, TRect & Rect);
  TThemeTabSheetButtons __fastcall GetTabButton(int Index);
  void UpdateHotButton(int & Ref, int Index);
  void DrawCross(HDC DC, int Width, COLORREF Color, const TRect & Rect);
  void DrawDropDown(HDC DC, int Radius, int X, int Y, COLORREF Color, int Grow);
  void __fastcall WMLButtonDown(TWMLButtonDown & Message);
  bool IsHotButton(int Index);
  TThemeTabSheet * GetPage(int Index);
  TThemeTabSheet * GetActivePage();
  void CMHintShow(TCMHintShow & Message);
  void SetTabTheme(TTBXTheme * value);
  void SetActiveTabTheme(TTBXTheme * value);

  int FOldTabIndex;
  int FHotTabButton;
  int FClickedButton;
  TPageControlTabButtonClick FOnTabButtonClick;
  TPageControlTabHint FOnTabHint;
  int FSessionTabShrink;
  TTBXTheme * FTabTheme;
  TTBXTheme * FActiveTabTheme;
  int FTextHeight;
};
//---------------------------------------------------------------------------
#endif
