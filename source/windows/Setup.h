//---------------------------------------------------------------------------
#ifndef SetupH
#define SetupH
//---------------------------------------------------------------------------
#include <Interface.h>
//---------------------------------------------------------------------------
void __fastcall SetupInitialize();
void __fastcall AddSearchPath(const UnicodeString Path);
void __fastcall RemoveSearchPath(const UnicodeString Path);
void __fastcall GetUpdatesMessage(UnicodeString & Message, bool & New, TQueryType & Type, bool Force);
void __fastcall CheckForUpdates(bool CachedResults);
UnicodeString __fastcall GetUsageData();
void __fastcall RegisterAsUrlHandler();
void __fastcall TemporaryDirectoryCleanup();
void __fastcall StartUpdateThread(TThreadMethod OnUpdatesChecked);
void __fastcall StopUpdateThread();
UnicodeString __fastcall CampaignUrl(UnicodeString URL);
void __fastcall UpdateJumpList(TStrings * SessionNames, TStrings * WorkspaceNames);
//---------------------------------------------------------------------------
#endif
