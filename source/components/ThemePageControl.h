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

private:
  void __fastcall SetShadowed(bool Value);

  bool FShadowed;
};
//---------------------------------------------------------------------------
class TThemePageControl : public TPageControl
{
friend class TThemeTabSheet;
public:
  __fastcall TThemePageControl(TComponent * Owner);

  int __fastcall GetTabsHeight();

protected:
  virtual void __fastcall PaintWindow(HDC DC);
  DYNAMIC bool __fastcall CanChange();
  DYNAMIC void __fastcall Change();
  #ifdef _DEBUG
  virtual void __fastcall RequestAlign();
  #endif

private:
  void __fastcall DrawThemesXpTabItem(HDC DC, int Item, const TRect & Rect, bool Body, int State);
  void __fastcall DrawTabItem(HDC DC, int Item, TRect Rect, bool Selected, bool Shadowed);
  void __fastcall DrawThemesPart(HDC DC, int PartId, int StateId, LPCWSTR PartNameID, LPRECT Rect);
  void __fastcall InvalidateTab(int Index);

  int FOldTabIndex;
};
//---------------------------------------------------------------------------
#endif
