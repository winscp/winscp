#ifndef VCLCommonH
#define VCLCommonH

#include <ComCtrls.hpp>	

void __fastcall AdjustListColumnsWidth(TListView* ListView);
void __fastcall EnableControl(TControl* Control, bool Enable);
void __fastcall UseSystemFont(TCustomForm * Control);
void __fastcall LinkLabel(TLabel * Label);

#endif	// VCLCommonH
