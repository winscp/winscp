// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ThemeMgr.pas' rev: 6.00

#ifndef ThemeMgrHPP
#define ThemeMgrHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ThemeSrv.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Thememgr
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TThemeOption { toAllowNonClientArea, toAllowControls, toAllowWebContent, toSubclassAnimate, toSubclassButtons, toSubclassCheckListbox, toSubclassDBLookup, toSubclassFrame, toSubclassGroupBox, toSubclassListView, toSubclassPanel, toSubclassTabSheet, toSubclassSpeedButtons, toSubclassSplitter, toSubclassStatusBar, toSubclassTrackBar, toSubclassWinControl, toResetMouseCapture, toSetTransparency, toAlternateTabSheetDraw };
#pragma option pop

typedef Set<TThemeOption, toAllowNonClientArea, toAlternateTabSheetDraw>  TThemeOptions;

#pragma pack(push, 1)
struct TWMPrint
{
	unsigned Msg;
	HDC DC;
	unsigned Flags;
	int Result;
} ;
#pragma pack(pop)

typedef TWMPrint  TWMPrintClient;

class DELPHICLASS TThemeManager;
typedef void __fastcall (__closure *TAllowSubclassingEvent)(TThemeManager* Sender, Controls::TControl* Control, bool &Allow);

typedef void __fastcall (__closure *TControlMessageEvent)(TThemeManager* Sender, Controls::TControl* Control, Messages::TMessage &Message, bool &Handled);

typedef TControlMessageEvent *PControlMessageEvent;

class DELPHICLASS TWindowProcList;
class PASCALIMPLEMENTATION TWindowProcList : public Classes::TList 
{
	typedef Classes::TList inherited;
	
private:
	bool FDirty;
	Controls::TControl* FLastControl;
	int FLastIndex;
	TThemeManager* FOwner;
	Classes::TWndMethod FNewWindowProc;
	TMetaClass*FControlClass;
	
public:
	__fastcall TWindowProcList(TThemeManager* Owner, Classes::TWndMethod WindowProc, TMetaClass* ControlClass);
	__fastcall virtual ~TWindowProcList(void);
	HIDESBASE int __fastcall Add(Controls::TControl* Control);
	virtual void __fastcall Clear(void);
	void __fastcall DispatchMessage(Controls::TControl* Control, Messages::TMessage &Message);
	bool __fastcall Find(Controls::TControl* Control, /* out */ int &Index);
	HIDESBASE void __fastcall Remove(Controls::TControl* Control);
};


class PASCALIMPLEMENTATION TThemeManager : public Classes::TComponent 
{
	typedef Classes::TComponent inherited;
	
private:
	TThemeOptions FOptions;
	TWindowProcList* FPanelList;
	TWindowProcList* FFrameList;
	TWindowProcList* FListViewList;
	TWindowProcList* FTabSheetList;
	TWindowProcList* FWinControlList;
	TWindowProcList* FGroupBoxList;
	TWindowProcList* FButtonControlList;
	TWindowProcList* FSpeedButtonList;
	TWindowProcList* FSplitterList;
	TWindowProcList* FTrackBarList;
	TWindowProcList* FAnimateList;
	TWindowProcList* FStatusBarList;
	TWindowProcList* FCheckListBoxList;
	TWindowProcList* FFormList;
	Classes::TList* FListeners;
	Classes::TList* FPendingFormsList;
	Classes::TList* FPendingRecreationList;
	bool FSubclassingDisabled;
	bool FHookWasInstalled;
	Classes::TNotifyEvent FOnThemeChange;
	TControlMessageEvent FOnControlMessage;
	TAllowSubclassingEvent FOnAllowSubclassing;
	void __fastcall AnimateWindowProc(Controls::TControl* Control, Messages::TMessage &Message);
	void __fastcall ButtonControlWindowProc(Controls::TControl* Control, Messages::TMessage &Message);
	void __fastcall CheckListBoxWindowProc(Controls::TControl* Control, Messages::TMessage &Message);
	void __fastcall FormWindowProc(Controls::TControl* Control, Messages::TMessage &Message);
	void __fastcall FrameWindowProc(Controls::TControl* Control, Messages::TMessage &Message);
	bool __fastcall GetIsMainManager(void);
	void __fastcall GroupBoxWindowProc(Controls::TControl* Control, Messages::TMessage &Message);
	void __fastcall ListviewWindowProc(Controls::TControl* Control, Messages::TMessage &Message);
	bool __fastcall MainWindowHook(Messages::TMessage &Message);
	void __fastcall PanelWindowProc(Controls::TControl* Control, Messages::TMessage &Message);
	void __fastcall SetThemeOptions(const TThemeOptions Value);
	void __fastcall SpeedButtonWindowProc(Controls::TControl* Control, Messages::TMessage &Message);
	void __fastcall SplitterWindowProc(Controls::TControl* Control, Messages::TMessage &Message);
	void __fastcall StatusBarWindowProc(Controls::TControl* Control, Messages::TMessage &Message);
	void __fastcall TabSheetWindowProc(Controls::TControl* Control, Messages::TMessage &Message);
	void __fastcall TrackBarWindowProc(Controls::TControl* Control, Messages::TMessage &Message);
	void __fastcall WinControlWindowProc(Controls::TControl* Control, Messages::TMessage &Message);
	void __fastcall PreAnimateWindowProc(Messages::TMessage &Message);
	void __fastcall PreButtonControlWindowProc(Messages::TMessage &Message);
	void __fastcall PreCheckListBoxWindowProc(Messages::TMessage &Message);
	void __fastcall PreFormWindowProc(Messages::TMessage &Message);
	void __fastcall PreFrameWindowProc(Messages::TMessage &Message);
	void __fastcall PreGroupBoxWindowProc(Messages::TMessage &Message);
	void __fastcall PreListviewWindowProc(Messages::TMessage &Message);
	void __fastcall PrePanelWindowProc(Messages::TMessage &Message);
	void __fastcall PreSpeedButtonWindowProc(Messages::TMessage &Message);
	void __fastcall PreSplitterWindowProc(Messages::TMessage &Message);
	void __fastcall PreStatusBarWindowProc(Messages::TMessage &Message);
	void __fastcall PreTabSheetWindowProc(Messages::TMessage &Message);
	void __fastcall PreTrackBarWindowProc(Messages::TMessage &Message);
	void __fastcall PreWinControlWindowProc(Messages::TMessage &Message);
	bool __fastcall GetThemesEnabled(void);
	
protected:
	virtual void __fastcall AddRecreationCandidate(Controls::TControl* Control);
	void __fastcall BroadcastThemeChange(void);
	/*         class method */ static TThemeManager* __fastcall CurrentThemeManager(TMetaClass* vmt);
	virtual bool __fastcall DoAllowSubclassing(Controls::TControl* Control);
	virtual bool __fastcall DoControlMessage(Controls::TControl* Control, Messages::TMessage &Message);
	virtual void __fastcall DoOnThemeChange(void);
	void __fastcall DrawBitBtn(Buttons::TBitBtn* Control, tagDRAWITEMSTRUCT &DrawItemStruct);
	void __fastcall DrawButton(Controls::TControl* Control, Themesrv::TThemedButton Button, HDC DC, const Types::TRect &R, bool Focused);
	bool __fastcall FindListener(TControlMessageEvent AControlMessage, int &Index);
	void __fastcall FixControls(Forms::TCustomForm* Form = (Forms::TCustomForm*)(0x0));
	virtual void __fastcall ForceAsMainManager(void);
	virtual void __fastcall HandleControlChange(Controls::TControl* Control, bool Inserting);
	bool __fastcall IsRecreationCandidate(Controls::TControl* Control);
	virtual void __fastcall Loaded(void);
	virtual bool __fastcall NeedsBorderPaint(Controls::TControl* Control);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	void __fastcall RemoveChildSubclassing(Controls::TWinControl* Control);
	void __fastcall RemoveRecreationCandidate(Controls::TControl* Control);
	void __fastcall UpdateThemes(void);
	void __fastcall UpdateUIState(Controls::TControl* Control, Word CharCode);
	
public:
	__fastcall virtual TThemeManager(Classes::TComponent* AOwner);
	__fastcall virtual ~TThemeManager(void);
	Graphics::TColor __fastcall GetColor(Themesrv::TThemedElement Element, int PartId, int StateId, int PropId);
	__property bool ThemesEnabled = {read=GetThemesEnabled, nodefault};
	void __fastcall ClearLists(void);
	void __fastcall CollectForms(Forms::TCustomForm* Form = (Forms::TCustomForm*)(0x0));
	void __fastcall CollectControls(Controls::TWinControl* Parent);
	void __fastcall PerformEraseBackground(Controls::TControl* Control, HDC DC);
	void __fastcall RegisterListener(TControlMessageEvent AControlMessage);
	void __fastcall UnregisterListener(TControlMessageEvent AControlMessage);
	__property bool IsMainManager = {read=GetIsMainManager, nodefault};
	
__published:
	__property TThemeOptions Options = {read=FOptions, write=SetThemeOptions, default=524279};
	__property TAllowSubclassingEvent OnAllowSubclassing = {read=FOnAllowSubclassing, write=FOnAllowSubclassing};
	__property TControlMessageEvent OnControlMessage = {read=FOnControlMessage, write=FOnControlMessage};
	__property Classes::TNotifyEvent OnThemeChange = {read=FOnThemeChange, write=FOnThemeChange};
};


//-- var, const, procedure ---------------------------------------------------
#define TMVersion "1.10.1"
static const Word CM_DENYSUBCLASSING = 0xb7d0;
static const Word SPI_GETFOCUSBORDERWIDTH = 0x200e;
static const Word SPI_SETFOCUSBORDERWIDTH = 0x200f;
static const Word SPI_GETFOCUSBORDERHEIGHT = 0x2010;
static const Word SPI_SETFOCUSBORDERHEIGHT = 0x2011;
#define DefaultThemeOptions (System::Set<TThemeOption, toAllowNonClientArea, toAlternateTabSheetDraw> () << TThemeOption(0) << TThemeOption(1) << TThemeOption(2) << TThemeOption(4) << TThemeOption(5) << TThemeOption(6) << TThemeOption(7) << TThemeOption(8) << TThemeOption(9) << TThemeOption(10) << TThemeOption(11) << TThemeOption(12) << TThemeOption(13) << TThemeOption(14) << TThemeOption(15) << TThemeOption(16) << TThemeOption(17) << TThemeOption(18) )
extern PACKAGE bool IsWindowsXP;

}	/* namespace Thememgr */
using namespace Thememgr;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ThemeMgr
