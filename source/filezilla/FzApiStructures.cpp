//---------------------------------------------------------------------------
#include "FileZillaPCH.h"
#include "FzApiStructures.h"
//---------------------------------------------------------------------------
t_server::t_server()
{
  port = 0;
  nServerType = 0;
  nPasv = 0;
  nTimeZoneOffset = 0;
  nUTF8 = 0;
  iForcePasvIp = -1;
  iUseMlsd = -1;
  Certificate = NULL;
  PrivateKey = NULL;
}
