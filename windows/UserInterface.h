//---------------------------------------------------------------------------
#ifndef UserInterfaceH
#define UserInterfaceH
//---------------------------------------------------------------------------
#include "CustomScpExplorer.h"
//---------------------------------------------------------------------------
TCustomScpExplorerForm * __fastcall CreateScpExplorer();
void __fastcall ConfigureInterface();
//---------------------------------------------------------------------------
void __fastcall DoProductLicence();
//---------------------------------------------------------------------------
extern const AnsiString AppName;
extern const AnsiString HomepageUrl;
//---------------------------------------------------------------------------
#endif // UserInterfaceH
