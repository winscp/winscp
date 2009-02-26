// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'PathLabel.pas' rev: 6.00

#ifndef PathLabelHPP
#define PathLabelHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Menus.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Pathlabel
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TCustomPathLabel;
typedef void __fastcall (__closure *TPathLabelGetStatusEvent)(TCustomPathLabel* Sender, bool &Active);

typedef void __fastcall (__closure *TPathLabelPathClickEvent)(TCustomPathLabel* Sender, AnsiString Path);

class PASCALIMPLEMENTATION TCustomPathLabel : public Stdctrls::TCustomLabel 
{
	typedef Stdctrls::TCustomLabel inherited;
	
private:
	Graphics::TColor FColors[6];
	int FIndentHorizontal;
	int FIndentVertical;
	bool FUnixPath;
	TPathLabelGetStatusEvent FOnGetStatus;
	TPathLabelPathClickEvent FOnPathClick;
	AnsiString FDisplayPath;
	AnsiString FDisplayHotTrack;
	AnsiString FDisplayMask;
	bool FHotTrack;
	bool FMouseInView;
	bool FIsActive;
	AnsiString FMask;
	HIDESBASE MESSAGE void __fastcall CMHintShow(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Messages::TMessage &Message);
	Graphics::TColor __fastcall GetColors(int Index);
	void __fastcall SetColors(int Index, Graphics::TColor Value);
	void __fastcall SetIndentHorizontal(int AIndent);
	void __fastcall SetIndentVertical(int AIndent);
	void __fastcall SetUnixPath(bool AUnixPath);
	void __fastcall SetMask(AnsiString Value);
	
protected:
	DYNAMIC void __fastcall AdjustBounds(void);
	DYNAMIC void __fastcall Click(void);
	DYNAMIC void __fastcall DoDrawText(Types::TRect &Rect, int Flags);
	virtual void __fastcall Notification(Classes::TComponent* AComponent, Classes::TOperation Operation);
	virtual void __fastcall Paint(void);
	bool __fastcall IsActive(void);
	AnsiString __fastcall HotTrackPath(AnsiString Path);
	DYNAMIC void __fastcall MouseMove(Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall DoPathClick(AnsiString Path);
	
public:
	__fastcall virtual TCustomPathLabel(Classes::TComponent* AnOwner);
	void __fastcall UpdateStatus(void);
	__property Graphics::TColor ActiveColor = {read=GetColors, write=SetColors, index=1, default=-2147483646};
	__property Graphics::TColor ActiveTextColor = {read=GetColors, write=SetColors, index=3, default=-2147483639};
	__property Graphics::TColor ActiveHotTrackColor = {read=GetColors, write=SetColors, index=5, default=-2147483621};
	__property bool UnixPath = {read=FUnixPath, write=SetUnixPath, default=0};
	__property int IndentHorizontal = {read=FIndentHorizontal, write=SetIndentHorizontal, default=5};
	__property int IndentVertical = {read=FIndentVertical, write=SetIndentVertical, default=1};
	__property Graphics::TColor InactiveColor = {read=GetColors, write=SetColors, index=0, default=-2147483645};
	__property Graphics::TColor InactiveTextColor = {read=GetColors, write=SetColors, index=2, default=-2147483629};
	__property Graphics::TColor InactiveHotTrackColor = {read=GetColors, write=SetColors, index=4, default=-2147483620};
	__property TPathLabelGetStatusEvent OnGetStatus = {read=FOnGetStatus, write=FOnGetStatus};
	__property TPathLabelPathClickEvent OnPathClick = {read=FOnPathClick, write=FOnPathClick};
	__property bool HotTrack = {read=FHotTrack, write=FHotTrack, default=0};
	__property AnsiString Mask = {read=FMask, write=SetMask};
	__property FocusControl ;
	__property Caption ;
	__property Hint  = {stored=false};
	__property Align  = {default=1};
public:
	#pragma option push -w-inl
	/* TGraphicControl.Destroy */ inline __fastcall virtual ~TCustomPathLabel(void) { }
	#pragma option pop
	
};


class DELPHICLASS TPathLabel;
class PASCALIMPLEMENTATION TPathLabel : public TCustomPathLabel 
{
	typedef TCustomPathLabel inherited;
	
__published:
	__property ActiveColor  = {index=1, default=-2147483646};
	__property ActiveTextColor  = {index=3, default=-2147483639};
	__property ActiveHotTrackColor  = {index=5, default=-2147483621};
	__property UnixPath  = {default=0};
	__property IndentHorizontal  = {default=5};
	__property IndentVertical  = {default=1};
	__property InactiveColor  = {index=0, default=-2147483645};
	__property InactiveTextColor  = {index=2, default=-2147483629};
	__property InactiveHotTrackColor  = {index=4, default=-2147483620};
	__property HotTrack  = {default=0};
	__property OnGetStatus ;
	__property OnPathClick ;
	__property Align  = {default=1};
	__property Alignment  = {default=0};
	__property Anchors  = {default=3};
	__property AutoSize  = {default=1};
	__property BiDiMode ;
	__property Constraints ;
	__property DragCursor  = {default=-12};
	__property DragKind  = {default=0};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property Font ;
	__property ParentBiDiMode  = {default=1};
	__property ParentFont  = {default=1};
	__property PopupMenu ;
	__property Transparent  = {default=0};
	__property Visible  = {default=1};
	__property OnClick ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEndDock ;
	__property OnEndDrag ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnStartDock ;
	__property OnStartDrag ;
public:
	#pragma option push -w-inl
	/* TCustomPathLabel.Create */ inline __fastcall virtual TPathLabel(Classes::TComponent* AnOwner) : TCustomPathLabel(AnOwner) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TGraphicControl.Destroy */ inline __fastcall virtual ~TPathLabel(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Pathlabel */
using namespace Pathlabel;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// PathLabel
