//---------------------------------------------------------------------------
#ifndef FzApiStructuresH
#define FzApiStructuresH
//---------------------------------------------------------------------------
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wold-style-cast"
#include <openssl/pkcs12.h>
#pragma clang diagnostic pop
#include <FileBuffer.h>
//---------------------------------------------------------------------------
class t_server
{
public:
  t_server();
  ~t_server() {}
  CString host;
  int port;
  CString user, pass, account;
  CString path;
  int nServerType;
  int nPasv;
  int nTimeZoneOffset;
  int nUTF8;
  int iForcePasvIp;
  int iUseMlsd;
  X509 * Certificate;
  EVP_PKEY * PrivateKey;
};
//---------------------------------------------------------------------------
#include "ServerPath.h"
//---------------------------------------------------------------------------
typedef struct
{
    CString localfile;
    CString remotefile;
    CServerPath remotepath;
    BOOL get;
    __int64 size;
    t_server server;
    int nType;
    int nUserData;
    TTransferOutEvent OnTransferOut;
    TTransferInEvent OnTransferIn;
} t_transferfile;
//---------------------------------------------------------------------------
#endif // FzApiStructuresH
