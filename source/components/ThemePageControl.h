//---------------------------------------------------------------------------
#ifndef ThemePageControlH
#define ThemePageControlH
//---------------------------------------------------------------------------
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
class TThemeTabSheet : public TTabSheet
{
public:
  __fastcall TThemeTabSheet(TComponent * Owner);

  __property bool Shadowed = { read = FShadowed, write = SetShadowed };
  __property bool ShowCloseButton = { read = FShowCloseButton, write = SetShowCloseButton };

private:
  void __fastcall SetShadowed(bool Value);
  void __fastcall SetShowCloseButton(bool Value);
  void __fastcall Invalidate();

  bool FShadowed;
  bool FShowCloseButton;
};
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TPageControlCloseButtonClick)(TPageControl * Sender, int Index);
//---------------------------------------------------------------------------
class TThemePageControl : public TPageControl
{
friend class TThemeTabSheet;

__published:
  __property TPageControlCloseButtonClick OnCloseButtonClick = { read = FOnCloseButtonClick, write = FOnCloseButtonClick };

public:
  __fastcall TThemePageControl(TComponent * Owner);

  int __fastcall GetTabsHeight();
  UnicodeString __fastcall FormatCaptionWithCloseButton(const UnicodeString & Caption);

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
  int __fastcall CloseButtonSize();
  int __fastcall GetCrossPadding();
  TRect __fastcall CloseButtonRect(int Index);
  int __fastcall IndexOfCloseButtonAt(int X, int Y);
  void __fastcall ItemContentsRect(int Item, TRect & Rect);
  bool __fastcall HasItemImage(int Item);
  void __fastcall ItemTextRect(int Item, TRect & Rect);
  void __fastcall ItemTabRect(int Item, TRect & Rect);
  bool __fastcall HasTabCloseButton(int Index);
  void __fastcall SetHotCloseButton(int Index);
  void __fastcall DrawCross(HDC DC, int Width, COLORREF Color, const TRect & Rect);
  void __fastcall WMLButtonDown(TWMLButtonDown & Message);

  int FOldTabIndex;
  int FHotCloseButton;
  TPageControlCloseButtonClick FOnCloseButtonClick;
};
//---------------------------------------------------------------------------
#endif
