//---------------------------------------------------------------------------
#ifndef ScpMainH
#define ScpMainH
//---------------------------------------------------------------------------
class TConfiguration;
extern TConfiguration *Configuration;
class TStoredSessionList;
extern TStoredSessionList *StoredSessions;
//---------------------------------------------------------------------------
void Initialize(const AnsiString IniFileName);
void Finalize();
//---------------------------------------------------------------------------
#endif

