//---------------------------------------------------------------------------
#ifndef VCLCommonH
#define VCLCommonH
//---------------------------------------------------------------------------
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
void __fastcall AdjustListColumnsWidth(TListView* ListView);
void __fastcall EnableControl(TControl* Control, bool Enable);
void __fastcall UseSystemSettings(TCustomForm * Control, void ** Settings = NULL);
void __fastcall RevokeSystemSettings(TCustomForm * Control, void * Settings);
void __fastcall LinkLabel(TLabel * Label);
//---------------------------------------------------------------------------
#endif  // VCLCommonH
