//---------------------------------------------------------------------------
#ifndef ScpMainH
#define ScpMainH
//---------------------------------------------------------------------------
#include "Common.h"
//---------------------------------------------------------------------------
class TConfiguration;
extern TConfiguration *Configuration;
class TStoredSessionList;
extern TStoredSessionList *StoredSessions;
//---------------------------------------------------------------------------
void Initialize(const AnsiString IniFileName);
void Finalize();
//---------------------------------------------------------------------------
class TCoreGuard : public TGuard
{
public:
  TCoreGuard();
};
//---------------------------------------------------------------------------
#endif

