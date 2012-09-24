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
//---------------------------------------------------------------------------
void CoreInitialize();
void CoreFinalize();
void CoreSetResourceModule(void * ResourceHandle);
void CoreMaintenanceTask();
//---------------------------------------------------------------------------
#endif
