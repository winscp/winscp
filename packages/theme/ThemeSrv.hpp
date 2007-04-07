// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ThemeSrv.pas' rev: 6.00

#ifndef ThemeSrvHPP
#define ThemeSrvHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <TmSchema.hpp>	// Pascal unit
#include <UxTheme.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Themesrv
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TThemedElement { teButton, teClock, teComboBox, teEdit, teExplorerBar, teHeader, teListView, teMenu, tePage, teProgress, teRebar, teScrollBar, teSpin, teStartPanel, teStatus, teTab, teTaskBand, teTaskBar, teToolBar, teToolTip, teTrackBar, teTrayNotify, teTreeview, teWindow };
#pragma option pop

#pragma option push -b-
enum TThemedButton { tbButtonDontCare, tbButtonRoot, tbPushButtonNormal, tbPushButtonHot, tbPushButtonPressed, tbPushButtonDisabled, tbPushButtonDefaulted, tbRadioButtonUncheckedNormal, tbRadioButtonUncheckedHot, tbRadioButtonUncheckedPressed, tbRadioButtonUncheckedDisabled, tbRadioButtonCheckedNormal, tbRadioButtonCheckedHot, tbRadioButtonCheckedPressed, tbRadioButtonCheckedDisabled, tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedHot, tbCheckBoxUncheckedPressed, tbCheckBoxUncheckedDisabled, tbCheckBoxCheckedNormal, tbCheckBoxCheckedHot, tbCheckBoxCheckedPressed, tbCheckBoxCheckedDisabled, tbCheckBoxMixedNormal, tbCheckBoxMixedHot, tbCheckBoxMixedPressed, tbCheckBoxMixedDisabled, tbGroupBoxNormal, tbGroupBoxDisabled, tbUserButton };
#pragma option pop

#pragma option push -b-
enum TThemedClock { tcClockDontCare, tcClockRoot, tcTimeNormal };
#pragma option pop

#pragma option push -b-
enum TThemedComboBox { tcComboBoxDontCare, tcComboBoxRoot, tcDropDownButtonNormal, tcDropDownButtonHot, tcDropDownButtonPressed, tcDropDownButtonDisabled };
#pragma option pop

#pragma option push -b-
enum TThemedEdit { teEditDontCare, teEditRoot, teEditTextNormal, teEditTextHot, teEditTextSelected, teEditTextDisabled, teEditTextFocused, teEditTextReadOnly, teEditTextAssist, teEditCaret };
#pragma option pop

#pragma option push -b-
enum TThemedExplorerBar { tebExplorerBarDontCare, tebExplorerBarRoot, tebHeaderBackgroundNormal, tebHeaderBackgroundHot, tebHeaderBackgroundPressed, tebHeaderCloseNormal, tebHeaderCloseHot, tebHeaderClosePressed, tebHeaderPinNormal, tebHeaderPinHot, tebHeaderPinPressed, tebHeaderPinSelectedNormal, tebHeaderPinSelectedHot, tebHeaderPinSelectedPressed, tebIEBarMenuNormal, tebIEBarMenuHot, tebIEBarMenuPressed, tebNormalGroupBackground, tebNormalGroupCollapseNormal, tebNormalGroupCollapseHot, tebNormalGroupCollapsePressed, tebNormalGroupExpandNormal, tebNormalGroupExpandHot, tebNormalGroupExpandPressed, tebNormalGroupHead, tebSpecialGroupBackground, tebSpecialGroupCollapseSpecial, tebSpecialGroupCollapseHot, tebSpecialGroupCollapsePressed, tebSpecialGroupExpandSpecial, tebSpecialGroupExpandHot, tebSpecialGroupExpandPressed, tebSpecialGroupHead };
#pragma option pop

#pragma option push -b-
enum TThemedHeader { thHeaderDontCare, thHeaderRoot, thHeaderItemNormal, thHeaderItemHot, thHeaderItemPressed, thHeaderItemLeftNormal, thHeaderItemLeftHot, thHeaderItemLeftPressed, thHeaderItemRightNormal, thHeaderItemRightHot, thHeaderItemRightPressed, thHeaderSortArrowSortedUp, thHeaderSortArrowSortedDown };
#pragma option pop

#pragma option push -b-
enum TThemedListview { tlListviewDontCare, tlListviewRoot, tlListItemNormal, tlListItemHot, tlListItemSelected, tlListItemDisabled, tlListItemSelectedNotFocus, tlListGroup, tlListDetail, tlListSortDetail, tlEmptyText };
#pragma option pop

#pragma option push -b-
enum TThemedMenu { tmMenuDontCare, tmMenuRoot, tmMenuItemNormal, tmMenuItemSelected, tmMenuItemDemoted, tmMenuDropDown, tmMenuBarItem, tmMenuBarDropDown, tmChevron, tmSeparator };
#pragma option pop

#pragma option push -b-
enum TThemedPage { tpPageDontCare, tpPageRoot, tpUpNormal, tpUpHot, tpUpPressed, tpUpDisabled, tpDownNormal, tpDownHot, tpDownPressed, tpDownDisabled, tpUpHorzNormal, tpUpHorzHot, tpUpHorzPressed, tpUpHorzDisabled, tpDownHorzNormal, tpDownHorzHot, tpDownHorzPressed, tpDownHorzDisabled };
#pragma option pop

#pragma option push -b-
enum TThemedProgress { tpProgressDontCare, tpProgressRoot, tpBar, tpBarVert, tpChunk, tpChunkVert };
#pragma option pop

#pragma option push -b-
enum TThemedRebar { trRebarDontCare, trRebarRoot, trGripper, trGripperVert, trBandNormal, trBandHot, trBandPressed, trBandDisabled, trBandChecked, trBandHotChecked, trChevronNormal, trChevronHot, trChevronPressed, trChevronDisabled, trChevronVertNormal, trChevronVertHot, trChevronVertPressed, trChevronVertDisabled };
#pragma option pop

#pragma option push -b-
enum TThemedScrollBar { tsScrollBarDontCare, tsScrollBarRoot, tsArrowBtnUpNormal, tsArrowBtnUpHot, tsArrowBtnUpPressed, tsArrowBtnUpDisabled, tsArrowBtnDownNormal, tsArrowBtnDownHot, tsArrowBtnDownPressed, tsArrowBtnDownDisabled, tsArrowBtnLeftNormal, tsArrowBtnLeftHot, tsArrowBtnLeftPressed, tsArrowBtnLeftDisabled, tsArrowBtnRightNormal, tsArrowBtnRightHot, tsArrowBtnRightPressed, tsArrowBtnRightDisabled, tsThumbBtnHorzNormal, tsThumbBtnHorzHot, tsThumbBtnHorzPressed, tsThumbBtnHorzDisabled, tsThumbBtnVertNormal, tsThumbBtnVertHot, tsThumbBtnVertPressed, tsThumbBtnVertDisabled, tsLowerTrackHorzNormal, tsLowerTrackHorzHot, tsLowerTrackHorzPressed, tsLowerTrackHorzDisabled, tsUpperTrackHorzNormal, tsUpperTrackHorzHot, tsUpperTrackHorzPressed, tsUpperTrackHorzDisabled, tsLowerTrackVertNormal, tsLowerTrackVertHot, tsLowerTrackVertPressed, tsLowerTrackVertDisabled, tsUpperTrackVertNormal, tsUpperTrackVertHot, tsUpperTrackVertPressed, tsUpperTrackVertDisabled, tsGripperHorzNormal, tsGripperHorzHot
	, tsGripperHorzPressed, tsGripperHorzDisabled, tsGripperVertNormal, tsGripperVertHot, tsGripperVertPressed, tsGripperVertDisabled, tsSizeBoxRightAlign, tsSizeBoxLeftAlign };
#pragma option pop

#pragma option push -b-
enum TThemedSpin { tsSpinDontCare, tsSpinRoot, tsUpNormal, tsUpHot, tsUpPressed, tsUpDisabled, tsDownNormal, tsDownHot, tsDownPressed, tsDownDisabled, tsUpHorzNormal, tsUpHorzHot, tsUpHorzPressed, tsUpHorzDisabled, tsDownHorzNormal, tsDownHorzHot, tsDownHorzPressed, tsDownHorzDisabled };
#pragma option pop

#pragma option push -b-
enum TThemedStartPanel { tspStartPanelDontCare, tspStartPanelRoot, tspUserPane, tspMorePrograms, tspMoreProgramsArrowNormal, tspMoreProgramsArrowHot, tspMoreProgramsArrowPressed, tspProgList, tspProgListSeparator, tspPlacesList, tspPlacesListSeparator, tspLogOff, tspLogOffButtonsNormal, tspLogOffButtonsHot, tspLogOffButtonsPressed, tspUserPicture, tspPreview };
#pragma option pop

#pragma option push -b-
enum TThemedStatus { tsStatusDontCare, tsStatusRoot, tsPane, tsGripperPane, tsGripper };
#pragma option pop

#pragma option push -b-
enum TThemedTab { ttTabDontCare, ttTabRoot, ttTabItemNormal, ttTabItemHot, ttTabItemSelected, ttTabItemDisabled, ttTabItemFocused, ttTabItemLeftEdgeNormal, ttTabItemLeftEdgeHot, ttTabItemLeftEdgeSelected, ttTabItemLeftEdgeDisabled, ttTabItemLeftEdgeFocused, ttTabItemRightEdgeNormal, ttTabItemRightEdgeHot, ttTabItemRightEdgeSelected, ttTabItemRightEdgeDisabled, ttTabItemRightEdgeFocused, ttTabItemBothEdgeNormal, ttTabItemBothEdgeHot, ttTabItemBothEdgeSelected, ttTabItemBothEdgeDisabled, ttTabItemBothEdgeFocused, ttTopTabItemNormal, ttTopTabItemHot, ttTopTabItemSelected, ttTopTabItemDisabled, ttTopTabItemFocused, ttTopTabItemLeftEdgeNormal, ttTopTabItemLeftEdgeHot, ttTopTabItemLeftEdgeSelected, ttTopTabItemLeftEdgeDisabled, ttTopTabItemLeftEdgeFocused, ttTopTabItemRightEdgeNormal, ttTopTabItemRightEdgeHot, ttTopTabItemRightEdgeSelected, ttTopTabItemRightEdgeDisabled, ttTopTabItemRightEdgeFocused, ttTopTabItemBothEdgeNormal, ttTopTabItemBothEdgeHot, ttTopTabItemBothEdgeSelected, ttTopTabItemBothEdgeDisabled
	, ttTopTabItemBothEdgeFocused, ttPane, ttBody };
#pragma option pop

#pragma option push -b-
enum TThemedTaskBand { ttbTaskBandDontCare, ttbTaskBandRoot, ttbGroupCount, ttbFlashButton, ttpFlashButtonGroupMenu };
#pragma option pop

#pragma option push -b-
enum TThemedTaskBar { ttTaskBarDontCare, ttTaskBarRoot, ttbTimeNormal };
#pragma option pop

#pragma option push -b-
enum TThemedToolBar { ttbToolBarDontCare, ttbToolBarRoot, ttbButtonNormal, ttbButtonHot, ttbButtonPressed, ttbButtonDisabled, ttbButtonChecked, ttbButtonCheckedHot, ttbDropDownButtonNormal, ttbDropDownButtonHot, ttbDropDownButtonPressed, ttbDropDownButtonDisabled, ttbDropDownButtonChecked, ttbDropDownButtonCheckedHot, ttbSplitButtonNormal, ttbSplitButtonHot, ttbSplitButtonPressed, ttbSplitButtonDisabled, ttbSplitButtonChecked, ttbSplitButtonCheckedHot, ttbSplitButtonDropDownNormal, ttbSplitButtonDropDownHot, ttbSplitButtonDropDownPressed, ttbSplitButtonDropDownDisabled, ttbSplitButtonDropDownChecked, ttbSplitButtonDropDownCheckedHot, ttbSeparatorNormal, ttbSeparatorHot, ttbSeparatorPressed, ttbSeparatorDisabled, ttbSeparatorChecked, ttbSeparatorCheckedHot, ttbSeparatorVertNormal, ttbSeparatorVertHot, ttbSeparatorVertPressed, ttbSeparatorVertDisabled, ttbSeparatorVertChecked, ttbSeparatorVertCheckedHot };
#pragma option pop

#pragma option push -b-
enum TThemedToolTip { tttToolTipDontCare, tttToolTipRoot, tttStandardNormal, tttStandardLink, tttStandardTitleNormal, tttStandardTitleLink, tttBaloonNormal, tttBaloonLink, tttBaloonTitleNormal, tttBaloonTitleLink, tttCloseNormal, tttCloseHot, tttClosePressed };
#pragma option pop

#pragma option push -b-
enum TThemedTrackBar { ttbTrackBarDontCare, ttbTrackBarRoot, ttbTrack, ttbTrackVert, ttbThumbNormal, ttbThumbHot, ttbThumbPressed, ttbThumbFocused, ttbThumbDisabled, ttbThumbBottomNormal, ttbThumbBottomHot, ttbThumbBottomPressed, ttbThumbBottomFocused, ttbThumbBottomDisabled, ttbThumbTopNormal, ttbThumbTopHot, ttbThumbTopPressed, ttbThumbTopFocused, ttbThumbTopDisabled, ttbThumbVertNormal, ttbThumbVertHot, ttbThumbVertPressed, ttbThumbVertFocused, ttbThumbVertDisabled, ttbThumbLeftNormal, ttbThumbLeftHot, ttbThumbLeftPressed, ttbThumbLeftFocused, ttbThumbLeftDisabled, ttbThumbRightNormal, ttbThumbRightHot, ttbThumbRightPressed, ttbThumbRightFocused, ttbThumbRightDisabled, ttbThumbTics, ttbThumbTicsVert };
#pragma option pop

#pragma option push -b-
enum TThemedTrayNotify { ttnTrayNotifyDontCare, ttnTrayNotifyRoot, ttnBackground, ttnAnimBackground };
#pragma option pop

#pragma option push -b-
enum TThemedTreeview { ttTreeviewDontCare, ttTreeviewRoot, ttItemNormal, ttItemHot, ttItemSelected, ttItemDisabled, ttItemSelectedNotFocus, ttGlyphClosed, ttGlyphOpened, ttBranch };
#pragma option pop

#pragma option push -b-
enum TThemedWindow { twWindowDontCare, twWindowRoot, twCaptionActive, twCaptionInactive, twCaptionDisabled, twSmallCaptionActive, twSmallCaptionInactive, twSmallCaptionDisabled, twMinCaptionActive, twMinCaptionInactive, twMinCaptionDisabled, twSmallMinCaptionActive, twSmallMinCaptionInactive, twSmallMinCaptionDisabled, twMaxCaptionActive, twMaxCaptionInactive, twMaxCaptionDisabled, twSmallMaxCaptionActive, twSmallMaxCaptionInactive, twSmallMaxCaptionDisabled, twFrameLeftActive, twFrameLeftInactive, twFrameRightActive, twFrameRightInactive, twFrameBottomActive, twFrameBottomInactive, twSmallFrameLeftActive, twSmallFrameLeftInactive, twSmallFrameRightActive, twSmallFrameRightInactive, twSmallFrameBottomActive, twSmallFrameBottomInactive, twSysButtonNormal, twSysButtonHot, twSysButtonPushed, twSysButtonDisabled, twSysButtonInactive, twMDISysButtonNormal, twMDISysButtonHot, twMDISysButtonPushed, twMDISysButtonDisabled, twMDISysButtonInactive, twMinButtonNormal, twMinButtonHot, twMinButtonPushed
	, twMinButtonDisabled, twMinButtonInactive, twMDIMinButtonNormal, twMDIMinButtonHot, twMDIMinButtonPushed, twMDIMinButtonDisabled, twMDIMinButtonInactive, twMaxButtonNormal, twMaxButtonHot, twMaxButtonPushed, twMaxButtonDisabled, twMaxButtonInactive, twCloseButtonNormal, twCloseButtonHot, twCloseButtonPushed, twCloseButtonDisabled, twCloseButtonInactive, twSmallCloseButtonNormal, twSmallCloseButtonHot, twSmallCloseButtonPushed, twSmallCloseButtonDisabled, twSmallCloseButtonInactive, twMDICloseButtonNormal, twMDICloseButtonHot, twMDICloseButtonPushed, twMDICloseButtonDisabled, twMDICloseButtonInactive, twRestoreButtonNormal, twRestoreButtonHot, twRestoreButtonPushed, twRestoreButtonDisabled, twRestoreButtonInactive, twMDIRestoreButtonNormal, twMDIRestoreButtonHot, twMDIRestoreButtonPushed, twMDIRestoreButtonDisabled, twMDIRestoreButtonInactive, twHelpButtonNormal, twHelpButtonHot, twHelpButtonPushed, twHelpButtonDisabled, twHelpButtonInactive, twMDIHelpButtonNormal, twMDIHelpButtonHot, twMDIHelpButtonPushed
	, twMDIHelpButtonDisabled, twMDIHelpButtonInactive, twHorzScrollNormal, twHorzScrollHot, twHorzScrollPushed, twHorzScrollDisabled, twHorzThumbNormal, twHorzThumbHot, twHorzThumbPushed, twHorzThumbDisabled, twVertScrollNormal, twVertScrollHot, twVertScrollPushed, twVertScrollDisabled, twVertThumbNormal, twVertThumbHot, twVertThumbPushed, twVertThumbDisabled, twDialog, twCaptionSizingTemplate, twSmallCaptionSizingTemplate, twFrameLeftSizingTemplate, twSmallFrameLeftSizingTemplate, twFrameRightSizingTemplate, twSmallFrameRightSizingTemplate, twFrameBottomSizingTemplate, twSmallFrameBottomSizingTemplate };
#pragma option pop

typedef unsigned TThemeData[24];

struct TThemedElementDetails;
typedef TThemedElementDetails *PThemedElementDetails;

#pragma pack(push, 4)
struct TThemedElementDetails
{
	TThemedElement Element;
	int Part;
	int State;
} ;
#pragma pack(pop)

class DELPHICLASS TThemeServices;
class PASCALIMPLEMENTATION TThemeServices : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	bool FThemesAvailable;
	bool FUseThemes;
	bool FControlsEnabled;
	HWND FWindowHandle;
	void *FDefWindowProc;
	void *FObjectInstance;
	unsigned FThemeData[24];
	Classes::TNotifyEvent FOnThemeChange;
	unsigned __fastcall GetTheme(TThemedElement Element);
	bool __fastcall GetThemesEnabled(void);
	void __fastcall WindowProc(Messages::TMessage &Message);
	
protected:
	virtual void __fastcall DoOnThemeChange(void);
	void __fastcall UnloadThemeData(void);
	
public:
	__fastcall TThemeServices(HWND ClientWindow);
	__fastcall virtual ~TThemeServices(void);
	TThemedElementDetails __fastcall GetElementDetails(TThemedButton Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedClock Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedComboBox Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedEdit Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedExplorerBar Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedHeader Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedListview Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedMenu Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedPage Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedProgress Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedRebar Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedScrollBar Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedSpin Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedStartPanel Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedStatus Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedTab Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedTaskBand Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedTaskBar Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedToolBar Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedToolTip Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedTrackBar Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedTrayNotify Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedTreeview Detail)/* overload */;
	TThemedElementDetails __fastcall GetElementDetails(TThemedWindow Detail)/* overload */;
	unsigned __fastcall ColorToRGB(Graphics::TColor Color, PThemedElementDetails Details = (void *)(0x0));
	Types::TRect __fastcall ContentRect(HDC DC, const TThemedElementDetails &Details, const Types::TRect &BoundingRect);
	void __fastcall DrawEdge(HDC DC, const TThemedElementDetails &Details, const Types::TRect &R, unsigned Edge, unsigned Flags, Types::PRect ContentRect = (void *)(0x0));
	void __fastcall DrawElement(HDC DC, const TThemedElementDetails &Details, const Types::TRect &R, Types::PRect ClipRect = (void *)(0x0));
	void __fastcall DrawIcon(HDC DC, const TThemedElementDetails &Details, const Types::TRect &R, unsigned himl, int Index);
	void __fastcall DrawParentBackground(HWND Window, HDC Target, PThemedElementDetails Details, bool OnlyIfTransparent, Types::PRect Bounds = (void *)(0x0));
	void __fastcall DrawText(HDC DC, const TThemedElementDetails &Details, const WideString S, const Types::TRect &R, unsigned Flags, unsigned Flags2);
	bool __fastcall HasTransparentParts(const TThemedElementDetails &Details);
	void __fastcall PaintBorder(Controls::TWinControl* Control, bool EraseLRCorner);
	void __fastcall UpdateThemes(void);
	Graphics::TColor __fastcall GetColor(TThemedElement Element, int PartId, int StateId, int PropId);
	__property HWND ClientWindow = {read=FWindowHandle, nodefault};
	__property unsigned Theme[TThemedElement Element] = {read=GetTheme};
	__property bool ThemesAvailable = {read=FThemesAvailable, nodefault};
	__property bool ThemesEnabled = {read=GetThemesEnabled, nodefault};
	__property Classes::TNotifyEvent OnThemeChange = {read=FOnThemeChange, write=FOnThemeChange};
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TThemeServices* __fastcall ThemeServices(void);

}	/* namespace Themesrv */
using namespace Themesrv;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ThemeSrv
