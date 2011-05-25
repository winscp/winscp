//---------------------------------------------------------------------------
#ifndef SetupH
#define SetupH
//---------------------------------------------------------------------------
#include <Interface.h>
//---------------------------------------------------------------------------
void __fastcall AddSearchPath(const AnsiString Path);
void __fastcall RemoveSearchPath(const AnsiString Path);
void __fastcall GetUpdatesMessage(AnsiString & Message, bool & New, TQueryType & Type, bool Force);
void __fastcall CheckForUpdates(bool CachedResults);
void __fastcall RegisterAsUrlHandler();
void __fastcall TemporaryDirectoryCleanup();
void __fastcall StartUpdateThread(TThreadMethod OnUpdatesChecked);
void __fastcall StopUpdateThread();
AnsiString __fastcall CampaignUrl(AnsiString URL);
//---------------------------------------------------------------------------
#endif
