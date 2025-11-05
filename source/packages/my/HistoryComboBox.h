// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'HistoryComboBox.pas' rev: 35.00 (Windows)

#ifndef HistorycomboboxHPP
#define HistorycomboboxHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.StdCtrls.hpp>

//-- user supplied -----------------------------------------------------------

namespace Historycombobox
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TUIStateAwareComboBox;
class DELPHICLASS THistoryComboBox;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TUIStateAwareComboBox : public Vcl::Stdctrls::TComboBox
{
  typedef Vcl::Stdctrls::TComboBox inherited;

private:
  bool FDarkMode;
  void __fastcall SetDarkMode(bool Value);

protected:
  virtual void __fastcall ComboWndProc(Winapi::Messages::TMessage &Message, HWND ComboWnd, void * ComboProc);
  virtual void __fastcall CreateWnd();

public:
  __property bool DarkMode = {read=FDarkMode, write=SetDarkMode, default=0};
public:
  /* TCustomComboBox.Create */ inline __fastcall virtual TUIStateAwareComboBox(System::Classes::TComponent* AOwner) : Vcl::Stdctrls::TComboBox(AOwner) { }
  /* TCustomComboBox.Destroy */ inline __fastcall virtual ~TUIStateAwareComboBox() { }

public:
  /* TWinControl.CreateParented */ inline __fastcall TUIStateAwareComboBox(HWND ParentWindow) : Vcl::Stdctrls::TComboBox(ParentWindow) { }

};


enum DECLSPEC_DENUM Historycombobox__2 : unsigned char { soExit, soDropDown };

typedef System::Set<Historycombobox__2, Historycombobox__2::soExit, Historycombobox__2::soDropDown> THistorySaveOn;

typedef void __fastcall (__closure *THistoryComboBoxGetData)(THistoryComboBox* Sender, void * &Data);

typedef void __fastcall (__closure *THistoryComboBoxSetData)(THistoryComboBox* Sender, void * Data);

class PASCALIMPLEMENTATION THistoryComboBox : public TUIStateAwareComboBox
{
  typedef TUIStateAwareComboBox inherited;

private:
  THistorySaveOn FSaveOn;
  int FMaxHistorySize;
  THistoryComboBoxGetData FOnGetData;
  THistoryComboBoxSetData FOnSetData;
  void __fastcall SetMaxHistorySize(int AMaxHistorySize);
  int __fastcall GetMaxItemWidth();

protected:
  DYNAMIC void __fastcall DoExit();
  DYNAMIC void __fastcall DropDown();
  DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
  DYNAMIC void __fastcall Change();

public:
  __fastcall virtual THistoryComboBox(System::Classes::TComponent* AOwner);
  virtual void __fastcall SaveToHistory();

__published:
  __property THistorySaveOn SaveOn = {read=FSaveOn, write=FSaveOn, default=3};
  __property int MaxHistorySize = {read=FMaxHistorySize, write=SetMaxHistorySize, default=30};
  __property THistoryComboBoxGetData OnGetData = {read=FOnGetData, write=FOnGetData};
  __property THistoryComboBoxSetData OnSetData = {read=FOnSetData, write=FOnSetData};
public:
  /* TCustomComboBox.Destroy */ inline __fastcall virtual ~THistoryComboBox() { }

public:
  /* TWinControl.CreateParented */ inline __fastcall THistoryComboBox(HWND ParentWindow) : TUIStateAwareComboBox(ParentWindow) { }

};


//-- var, const, procedure ---------------------------------------------------
#define DefaultHistorySaveOn (System::Set<Historycombobox__2, Historycombobox__2::soExit, Historycombobox__2::soDropDown>() << Historycombobox__2::soExit << Historycombobox__2::soDropDown )
static const System::Int8 DefaultMaxHistorySize = System::Int8(0x1e);
extern DELPHI_PACKAGE void __fastcall Register();
extern DELPHI_PACKAGE void __fastcall SaveToHistory(System::Classes::TStrings* Strings, System::UnicodeString T, void * Data = (void *)(0x0), int MaxHistorySize = 0x1e);
}  /* namespace Historycombobox */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_HISTORYCOMBOBOX)
using namespace Historycombobox;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif  // HistorycomboboxHPP
