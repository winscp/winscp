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
enum TKeyType { ktUnopenable, ktUnknown, ktSSH1, ktSSH2, ktOpenSSH, ktSSHCom };
TKeyType KeyType(AnsiString FileName);
AnsiString KeyTypeName(TKeyType KeyType);
//---------------------------------------------------------------------------
class TCoreGuard : public TGuard
{
public:
  TCoreGuard();
};
//---------------------------------------------------------------------------
#endif

