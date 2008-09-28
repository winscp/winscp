// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ComboEdit.pas' rev: 6.00

#ifndef ComboEditHPP
#define ComboEditHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <MaskUtils.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <FileCtrl.hpp>	// Pascal unit
#include <Mask.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Comboedit
{
//-- type declarations -------------------------------------------------------
typedef AnsiString TFileExt;

class DELPHICLASS TCustomComboEdit;
class PASCALIMPLEMENTATION TCustomComboEdit : public Mask::TCustomMaskEdit 
{
	typedef Mask::TCustomMaskEdit inherited;
	
private:
	Stdctrls::TButton* FButton;
	Controls::TWinControl* FBtnControl;
	Classes::TNotifyEvent FOnButtonClick;
	Classes::TShortCut FClickKey;
	bool FReadOnly;
	bool FDirectInput;
	bool FAlwaysEnable;
	Classes::TAlignment FAlignment;
	void __fastcall SetEditRect(void);
	void __fastcall UpdateBtnBounds(void);
	void __fastcall EditButtonClick(System::TObject* Sender);
	int __fastcall GetMinHeight(void);
	int __fastcall GetTextHeight(void);
	void __fastcall SetShowCaret(void);
	int __fastcall GetButtonWidth(void);
	void __fastcall SetButtonWidth(int Value);
	AnsiString __fastcall GetButtonHint();
	void __fastcall SetButtonHint(const AnsiString Value);
	void __fastcall SetDirectInput(bool Value);
	HIDESBASE void __fastcall SetReadOnly(bool Value);
	void __fastcall SetAlignment(Classes::TAlignment Value);
	bool __fastcall BtnWidthStored(void);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMEnter(Messages::TMessage &Message);
	MESSAGE void __fastcall CNCtlColor(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMSize(Messages::TWMSize &Message);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMPaste(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall WMCut(Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall CMCtl3DChanged(Messages::TMessage &Message);
	
protected:
	virtual void __fastcall CreateParams(Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd(void);
	virtual bool __fastcall EditCanModify(void);
	virtual bool __fastcall GetReadOnly(void);
	DYNAMIC void __fastcall KeyDown(Word &Key, Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(char &Key);
	DYNAMIC void __fastcall ButtonClick(void);
	__property Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
	__property bool AlwaysEnable = {read=FAlwaysEnable, write=FAlwaysEnable, default=0};
	__property Stdctrls::TButton* Button = {read=FButton};
	__property Classes::TShortCut ClickKey = {read=FClickKey, write=FClickKey, default=32808};
	__property int ButtonWidth = {read=GetButtonWidth, write=SetButtonWidth, stored=BtnWidthStored, nodefault};
	__property AnsiString ButtonHint = {read=GetButtonHint, write=SetButtonHint};
	__property bool DirectInput = {read=FDirectInput, write=SetDirectInput, default=1};
	__property bool ReadOnly = {read=GetReadOnly, write=SetReadOnly, default=0};
	__property Classes::TNotifyEvent OnButtonClick = {read=FOnButtonClick, write=FOnButtonClick};
	
public:
	__fastcall virtual TCustomComboEdit(Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomComboEdit(void);
	void __fastcall DoClick(void);
	HIDESBASE void __fastcall SelectAll(void);
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCustomComboEdit(HWND ParentWindow) : Mask::TCustomMaskEdit(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TComboEdit;
class PASCALIMPLEMENTATION TComboEdit : public TCustomComboEdit 
{
	typedef TCustomComboEdit inherited;
	
__published:
	__property AutoSelect  = {default=1};
	__property ButtonHint ;
	__property BorderStyle  = {default=1};
	__property CharCase  = {default=0};
	__property ClickKey  = {default=32808};
	__property Color  = {default=-2147483643};
	__property Ctl3D ;
	__property DirectInput  = {default=1};
	__property DragCursor  = {default=-12};
	__property DragMode  = {default=0};
	__property EditMask ;
	__property Enabled  = {default=1};
	__property Font ;
	__property ButtonWidth ;
	__property HideSelection  = {default=1};
	__property Anchors  = {default=3};
	__property BiDiMode ;
	__property Constraints ;
	__property DragKind  = {default=0};
	__property ParentBiDiMode  = {default=1};
	__property ImeMode  = {default=3};
	__property ImeName ;
	__property ParentColor  = {default=0};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ReadOnly  = {default=0};
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property Text ;
	__property Visible  = {default=1};
	__property OnButtonClick ;
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
	__property OnEndDock ;
	__property OnStartDock ;
public:
	#pragma option push -w-inl
	/* TCustomComboEdit.Create */ inline __fastcall virtual TComboEdit(Classes::TComponent* AOwner) : TCustomComboEdit(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomComboEdit.Destroy */ inline __fastcall virtual ~TComboEdit(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TComboEdit(HWND ParentWindow) : TCustomComboEdit(ParentWindow) { }
	#pragma option pop
	
};


typedef void __fastcall (__closure *TExecOpenDialogEvent)(System::TObject* Sender, AnsiString &Name, bool &Action);

class DELPHICLASS TFileDirEdit;
class PASCALIMPLEMENTATION TFileDirEdit : public TCustomComboEdit 
{
	typedef TCustomComboEdit inherited;
	
private:
	unsigned FErrMode;
	bool FAcceptFiles;
	Classes::TNotifyEvent FOnDropFiles;
	TExecOpenDialogEvent FOnBeforeDialog;
	TExecOpenDialogEvent FOnAfterDialog;
	void __fastcall SetDragAccept(bool Value);
	void __fastcall SetAcceptFiles(bool Value);
	MESSAGE void __fastcall WMDropFiles(Messages::TWMDropFiles &Msg);
	
protected:
	bool FMultipleDirs;
	virtual void __fastcall CreateHandle(void);
	virtual void __fastcall DestroyWindowHandle(void);
	virtual AnsiString __fastcall GetLongName(void) = 0 ;
	virtual AnsiString __fastcall GetShortName(void) = 0 ;
	DYNAMIC void __fastcall DoAfterDialog(AnsiString &FileName, bool &Action);
	DYNAMIC void __fastcall DoBeforeDialog(AnsiString &FileName, bool &Action);
	virtual void __fastcall ReceptFileDir(const AnsiString AFileName) = 0 ;
	virtual void __fastcall ClearFileList(void);
	void __fastcall DisableSysErrors(void);
	void __fastcall EnableSysErrors(void);
	__property MaxLength  = {default=0};
	
public:
	__fastcall virtual TFileDirEdit(Classes::TComponent* AOwner);
	__property AnsiString LongName = {read=GetLongName};
	__property AnsiString ShortName = {read=GetShortName};
	
__published:
	__property bool AcceptFiles = {read=FAcceptFiles, write=SetAcceptFiles, default=0};
	__property TExecOpenDialogEvent OnBeforeDialog = {read=FOnBeforeDialog, write=FOnBeforeDialog};
	__property TExecOpenDialogEvent OnAfterDialog = {read=FOnAfterDialog, write=FOnAfterDialog};
	__property Classes::TNotifyEvent OnDropFiles = {read=FOnDropFiles, write=FOnDropFiles};
	__property OnButtonClick ;
public:
	#pragma option push -w-inl
	/* TCustomComboEdit.Destroy */ inline __fastcall virtual ~TFileDirEdit(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TFileDirEdit(HWND ParentWindow) : TCustomComboEdit(ParentWindow) { }
	#pragma option pop
	
};


#pragma option push -b-
enum TFileDialogKind { dkOpen, dkSave, dkOpenPicture, dkSavePicture };
#pragma option pop

typedef void __fastcall (__closure *TCreateEditDialogEvent)(System::TObject* Sender, TFileDialogKind DialogKind, Dialogs::TOpenDialog* &Dialog);

class DELPHICLASS TFilenameEdit;
class PASCALIMPLEMENTATION TFilenameEdit : public TFileDirEdit 
{
	typedef TFileDirEdit inherited;
	
private:
	Dialogs::TOpenDialog* FDialog;
	TFileDialogKind FDialogKind;
	TCreateEditDialogEvent FOnCreateEditDialog;
	void __fastcall CreateEditDialog(void);
	AnsiString __fastcall GetFileName();
	AnsiString __fastcall GetDefaultExt();
	Dialogs::TFileEditStyle __fastcall GetFileEditStyle(void);
	AnsiString __fastcall GetFilter();
	int __fastcall GetFilterIndex(void);
	AnsiString __fastcall GetInitialDir();
	Classes::TStrings* __fastcall GetHistoryList(void);
	Dialogs::TOpenOptions __fastcall GetOptions(void);
	AnsiString __fastcall GetDialogTitle();
	Classes::TStrings* __fastcall GetDialogFiles(void);
	void __fastcall SetDialogKind(TFileDialogKind Value);
	void __fastcall SetFileName(const AnsiString Value);
	void __fastcall SetDefaultExt(AnsiString Value);
	void __fastcall SetFileEditStyle(Dialogs::TFileEditStyle Value);
	void __fastcall SetFilter(const AnsiString Value);
	void __fastcall SetFilterIndex(int Value);
	void __fastcall SetInitialDir(const AnsiString Value);
	void __fastcall SetHistoryList(Classes::TStrings* Value);
	void __fastcall SetOptions(Dialogs::TOpenOptions Value);
	void __fastcall SetDialogTitle(const AnsiString Value);
	void __fastcall SetOnCreateEditDialog(TCreateEditDialogEvent Value);
	bool __fastcall IsCustomTitle(void);
	bool __fastcall IsCustomFilter(void);
	
protected:
	DYNAMIC void __fastcall ButtonClick(void);
	virtual void __fastcall ReceptFileDir(const AnsiString AFileName);
	virtual void __fastcall ClearFileList(void);
	virtual AnsiString __fastcall GetLongName();
	virtual AnsiString __fastcall GetShortName();
	
public:
	__fastcall virtual TFilenameEdit(Classes::TComponent* AOwner);
	__property Dialogs::TOpenDialog* Dialog = {read=FDialog};
	__property Classes::TStrings* DialogFiles = {read=GetDialogFiles};
	
__published:
	__property TFileDialogKind DialogKind = {read=FDialogKind, write=SetDialogKind, default=0};
	__property AnsiString DefaultExt = {read=GetDefaultExt, write=SetDefaultExt};
	__property Dialogs::TFileEditStyle FileEditStyle = {read=GetFileEditStyle, write=SetFileEditStyle, default=0};
	__property AnsiString FileName = {read=GetFileName, write=SetFileName, stored=false};
	__property AnsiString Filter = {read=GetFilter, write=SetFilter, stored=IsCustomFilter};
	__property int FilterIndex = {read=GetFilterIndex, write=SetFilterIndex, default=1};
	__property AnsiString InitialDir = {read=GetInitialDir, write=SetInitialDir};
	__property Classes::TStrings* HistoryList = {read=GetHistoryList, write=SetHistoryList};
	__property Dialogs::TOpenOptions DialogOptions = {read=GetOptions, write=SetOptions, default=4};
	__property AnsiString DialogTitle = {read=GetDialogTitle, write=SetDialogTitle, stored=IsCustomTitle};
	__property TCreateEditDialogEvent OnCreateEditDialog = {read=FOnCreateEditDialog, write=SetOnCreateEditDialog};
	__property AutoSelect  = {default=1};
	__property ButtonHint ;
	__property BorderStyle  = {default=1};
	__property CharCase  = {default=0};
	__property ClickKey  = {default=32808};
	__property Color  = {default=-2147483643};
	__property Ctl3D ;
	__property DirectInput  = {default=1};
	__property DragCursor  = {default=-12};
	__property DragMode  = {default=0};
	__property EditMask ;
	__property Enabled  = {default=1};
	__property Font ;
	__property ButtonWidth ;
	__property HideSelection  = {default=1};
	__property Anchors  = {default=3};
	__property BiDiMode ;
	__property Constraints ;
	__property DragKind  = {default=0};
	__property ParentBiDiMode  = {default=1};
	__property ImeMode  = {default=3};
	__property ImeName ;
	__property ParentColor  = {default=0};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ReadOnly  = {default=0};
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property Text ;
	__property Visible  = {default=1};
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
	__property OnEndDock ;
	__property OnStartDock ;
public:
	#pragma option push -w-inl
	/* TCustomComboEdit.Destroy */ inline __fastcall virtual ~TFilenameEdit(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TFilenameEdit(HWND ParentWindow) : TFileDirEdit(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS TDirectoryEdit;
class PASCALIMPLEMENTATION TDirectoryEdit : public TFileDirEdit 
{
	typedef TFileDirEdit inherited;
	
private:
	AnsiString FInitialDir;
	AnsiString FDialogText;
	
protected:
	DYNAMIC void __fastcall ButtonClick(void);
	virtual void __fastcall ReceptFileDir(const AnsiString AFileName);
	virtual AnsiString __fastcall GetLongName();
	virtual AnsiString __fastcall GetShortName();
	
public:
	__fastcall virtual TDirectoryEdit(Classes::TComponent* AOwner);
	
__published:
	__property AnsiString DialogText = {read=FDialogText, write=FDialogText};
	__property AnsiString InitialDir = {read=FInitialDir, write=FInitialDir};
	__property bool MultipleDirs = {read=FMultipleDirs, write=FMultipleDirs, default=0};
	__property AutoSelect  = {default=1};
	__property ButtonHint ;
	__property BorderStyle  = {default=1};
	__property CharCase  = {default=0};
	__property ClickKey  = {default=32808};
	__property Color  = {default=-2147483643};
	__property Ctl3D ;
	__property DirectInput  = {default=1};
	__property DragCursor  = {default=-12};
	__property DragMode  = {default=0};
	__property EditMask ;
	__property Enabled  = {default=1};
	__property Font ;
	__property ButtonWidth ;
	__property HideSelection  = {default=1};
	__property Anchors  = {default=3};
	__property BiDiMode ;
	__property Constraints ;
	__property DragKind  = {default=0};
	__property ParentBiDiMode  = {default=1};
	__property ImeMode  = {default=3};
	__property ImeName ;
	__property ParentColor  = {default=0};
	__property ParentCtl3D  = {default=1};
	__property ParentFont  = {default=1};
	__property ParentShowHint  = {default=1};
	__property PopupMenu ;
	__property ReadOnly  = {default=0};
	__property ShowHint ;
	__property TabOrder  = {default=-1};
	__property TabStop  = {default=1};
	__property Text ;
	__property Visible  = {default=1};
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
	__property OnEndDock ;
	__property OnStartDock ;
public:
	#pragma option push -w-inl
	/* TCustomComboEdit.Destroy */ inline __fastcall virtual ~TDirectoryEdit(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TDirectoryEdit(HWND ParentWindow) : TFileDirEdit(ParentWindow) { }
	#pragma option pop
	
};


class DELPHICLASS EComboEditError;
class PASCALIMPLEMENTATION EComboEditError : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EComboEditError(const AnsiString Msg) : Sysutils::Exception(Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EComboEditError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EComboEditError(int Ident)/* overload */ : Sysutils::Exception(Ident) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EComboEditError(int Ident, const System::TVarRec * Args, const int Args_Size)/* overload */ : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EComboEditError(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EComboEditError(const AnsiString Msg, const System::TVarRec * Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EComboEditError(int Ident, int AHelpContext)/* overload */ : Sysutils::Exception(Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EComboEditError(System::PResStringRec ResStringRec, const System::TVarRec * Args, const int Args_Size, int AHelpContext)/* overload */ : Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EComboEditError(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
static const Word scAltDown = 0x8028;
static const Word scCtrlEnter = 0x400d;
static const Shortint DefEditBtnWidth = 0x19;
extern PACKAGE System::ResourceString _SBrowse;
#define Comboedit_SBrowse System::LoadResourceString(&Comboedit::_SBrowse)
extern PACKAGE System::ResourceString _SDefaultFilter;
#define Comboedit_SDefaultFilter System::LoadResourceString(&Comboedit::_SDefaultFilter)
extern PACKAGE System::ResourceString _SInvalidFileName;
#define Comboedit_SInvalidFileName System::LoadResourceString(&Comboedit::_SInvalidFileName)
static const Shortint MaxFileLength = 0x3;
extern PACKAGE void __fastcall Register(void);

}	/* namespace Comboedit */
using namespace Comboedit;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ComboEdit
