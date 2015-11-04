//---------------------------------------------------------------------------
#ifndef StructuresH
#define StructuresH
//---------------------------------------------------------------------------
class CServerPath;
//---------------------------------------------------------------------------
#include "FileZillaApi.h"
//---------------------------------------------------------------------------
class t_directory
{
public:
  t_directory();
  ~t_directory();
  CServerPath path;
  int num;
  class t_direntry
  {
  public:
    t_direntry();
    bool bUnsure; // Set by CFtpControlSocket::FileTransfer when uploads fail after sending STOR/APPE
    CString name;
    CString permissionstr;
    CString humanpermstr; // RFC format
    CString ownergroup;
    __int64 size;
    bool dir;
    bool bLink;
    class t_date
    {
    public:
      t_date();
      int year,month,day,hour,minute,second;
      bool hastime;
      bool hasseconds;
      bool hasdate;
      bool utc;
    } date;
    CString linkTarget;
  } * direntry;
  t_server server;
  t_directory & operator=(const t_directory & a);
};
//---------------------------------------------------------------------------
#endif // StructuresH
