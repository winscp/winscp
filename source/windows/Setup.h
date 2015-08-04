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
bool __fastcall CheckForUpdates(bool CachedResults);
UnicodeString __fastcall GetUsageData();
void __fastcall RegisterForDefaultProtocols();
void __fastcall UnregisterForProtocols();
void __fastcall LaunchAdvancedAssociationUI();
void __fastcall TemporaryDirectoryCleanup();
void __fastcall StartUpdateThread(TThreadMethod OnUpdatesChecked);
void __fastcall StopUpdateThread();
UnicodeString __fastcall CampaignUrl(UnicodeString URL);
void __fastcall UpdateJumpList(TStrings * SessionNames, TStrings * WorkspaceNames);
bool __fastcall AnyOtherInstanceOfSelf();
bool __fastcall IsInstalled();
UnicodeString __fastcall ProgramUrl(UnicodeString URL);
//---------------------------------------------------------------------------
#endif
