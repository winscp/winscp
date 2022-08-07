//---------------------------------------------------------------------------
#ifndef CoreMainH
#define CoreMainH
//---------------------------------------------------------------------------
#include "Common.h"
//---------------------------------------------------------------------------
class TConfiguration;
extern TConfiguration *Configuration;
class TStoredSessionList;
extern TStoredSessionList *StoredSessions;
extern bool AnySession;
class TApplicationLog;
extern TApplicationLog * ApplicationLog;
#define AppLog(S) ApplicationLog->Log(S)
#define AppLogFmt(S, F) ApplicationLog->Log(Format(S, ARRAYOFCONST(F)))
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
