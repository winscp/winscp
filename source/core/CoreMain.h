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
