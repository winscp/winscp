// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UpDownEdit.pas' rev: 6.00

#ifndef UpDownEditHPP
#define UpDownEditHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysUtils.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Updownedit
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TValueType { vtInt, vtFloat, vtHex };
#pragma option pop

class DELPHICLASS TUpDownEdit;
class PASCALIMPLEMENTATION TUpDownEdit : public Stdctrls::TCustomEdit 
{
	typedef Stdctrls::TCustomEdit inherited;
	
private:
	Classes::TAlignment FAlignment;
	Extended FMinValue;
	Extended FMaxValue;
	Extended FIncrement;
	Byte FDecimal;
	bool FChanging;
	bool FEditorEnabled;
	TValueType FValueType;
	bool FArrowKeys;
	Classes::TNotifyEvent FOnTopClick;
	Classes::TNotifyEvent FOnBottomClick;
	Comctrls::TCustomUpDown* FUpDown;
	void __fastcall UpDownClick(System::TObject* Sender, Comctrls::TUDBtnType Button);
	int __fastcall GetMinHeight(void);
	void __fastcall GetTextHeight(int &SysHeight, int &Height);
	Extended __fastcall GetValue(void);
	Extended __fastcall CheckValue(Extended NewValue);
	int __fastcall GetAsInteger(void);
	bool __fastcall IsIncrementStored(void);
	bool __fastcall IsMaxStored(void);
	bool __fastcall IsMinStored(void);
	bool __fastcall IsValueStored(void);
	void __fastcall SetArrowKeys(bool Value);
	void __fastcall SetAsInteger(int NewValue);
	void __fastcall SetValue(Extended NewValue);
	void __fastcall SetValueType(TValueType NewType);
	void __fastcall SetDecimal(Byte NewValue);
	int __fastcall GetButtonWidth(void);
	void __fastcall RecreateButton(void);
	void __fastcall ResizeButton(void);
	void __fastcall SetEditRect(void);
	void __fastcall SetAlignment(Classes::TAlignment Value);
	HIDESBASE MESSAGE void __fastcall WMSize(Messages::TWMSize &Message);
	HIDESBASE MESSAGE void __fastcall CMEnter(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMExit(Messages::TWMNoParams &Message);
	MESSAGE void __fastcall WMPaste(Messages::TWMNoParams &Message);
	MESSAGE void __fastcall WMCut(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall CMCtl3DChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMBiDiModeChanged(Messages::TMessage &Message);
	
protected:
	DYNAMIC void __fastcall Change(void);
	virtual bool __fastcall IsValidChar(char Key);
	virtual void __fastcall UpClick(System::TObject* Sender);
	virtual void __fastcall DownClick(System::TObject* Sender);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(char &Key);
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd(void);
	
public:
	__fastcall virtual TUpDownEdit(Classes::TComponent* AOwner);
	__fastcall virtual ~TUpDownEdit(void);
	__property int AsInteger = {read=GetAsInteger, write=SetAsInteger, default=0};
	__property Text ;
	
__published:
	__property Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
	__property bool ArrowKeys = {read=FArrowKeys, write=SetArrowKeys, default=1};
	__property Byte Decimal = {read=FDecimal, write=SetDecimal, default=2};
	__property bool EditorEnabled = {read=FEditorEnabled, write=FEditorEnabled, default=1};
	__property Extended Increment = {read=FIncrement, write=FIncrement, stored=IsIncrementStored};
	__property Extended MaxValue = {read=FMaxValue, write=FMaxValue, stored=IsMaxStored};
	__property Extended MinValue = {read=FMinValue, write=FMinValue, stored=IsMinStored};
	__property TValueType ValueType = {read=FValueType, write=SetValueType, default=0};
	__property Extended Value = {read=GetValue, write=SetValue, stored=IsValueStored};
	__property AutoSelect  = {default=1};
	__property AutoSize  = {default=1};
	__property BorderStyle  = {default=1};
	__property Color  = {default=-2147483643};
	__property Ctl3D ;
	__property DragCursor  = {default=-12};
	__property DragMode  = {default=0};
	__property Enabled  = {default=1};
	__property Font ;
	__property Anchors  = {default=3};
	__property BiDiMode ;
	__property Constraints ;
	__property DragKind  = {default=0};
	__property ParentBiDiMode  = {default=1};
	__property ImeMode  = {default=3};
	__property ImeName ;
	__property MaxLength  = {default=0};
	__property ParentColor  = {default=0};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ReadOnly  = {default=0};
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property Visible  = {default=1};
	__property Classes::TNotifyEvent OnBottomClick = {read=FOnBottomClick, write=FOnBottomClick};
	__property Classes::TNotifyEvent OnTopClick = {read=FOnTopClick, write=FOnTopClick};
	__property OnChange ;
	__property OnClick ;
	__property OnDblClick ;
	__property OnDragDrop ;
	__property OnDragOver ;
	__property OnEndDrag ;
	__property OnEnter ;
	__property OnExit ;
	__property OnKeyDown ;
	__property OnKeyPress ;
	__property OnKeyUp ;
	__property OnMouseDown ;
	__property OnMouseMove ;
	__property OnMouseUp ;
	__property OnStartDrag ;
	__property OnContextPopup ;
	__property OnMouseWheelDown ;
	__property OnMouseWheelUp ;
	__property OnEndDock ;
	__property OnStartDock ;
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TUpDownEdit(HWND ParentWindow) : Stdctrls::TCustomEdit(ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall Register(void);

}	/* namespace Updownedit */
using namespace Updownedit;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UpDownEdit
