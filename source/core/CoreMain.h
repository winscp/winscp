//---------------------------------------------------------------------------
#ifndef CoreMainH
#define CoreMainH
//---------------------------------------------------------------------------
#include "Common.h"
#include "SessionInfo.h"
//---------------------------------------------------------------------------
extern TConfiguration *Configuration;
extern TStoredSessionList *StoredSessions;
extern bool AnySession;
extern TApplicationLog * ApplicationLog;
#define APPLOG_INTERNAL(S) if (ApplicationLog->Logging) ApplicationLog->Log(S)
#define AppLog(S) APPLOG_INTERNAL(S)
#define AppLogFmt(S, F) AppLog(FORMAT(S, F))
//---------------------------------------------------------------------------
void CoreInitialize();
void CoreFinalize();
void CoreSetResourceModule(void * ResourceHandle);
void CoreMaintenanceTask();
void CoreUpdateFinalStaticUsage();
//---------------------------------------------------------------------------
UnicodeString __fastcall NeonVersion();
UnicodeString __fastcall ExpatVersion();
//---------------------------------------------------------------------------
#endif
