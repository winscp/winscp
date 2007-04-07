//---------------------------------------------------------------------------
#ifndef StdAfxH
#define StdAfxH
//---------------------------------------------------------------------------
#define _int64 __int64
#define CStringA CString
//---------------------------------------------------------------------------
#define MPEXT
#define MPEXT_NO_ZLIB
#define MPEXT_NO_GSS
#define MPEXT_NO_SSL
#define MPEXT_NO_SFTP
#define MPEXT_NO_IDENT
#define MPEXT_NO_CACHE
#define MPEXT_NO_SPEED_LIM
#define _AFX_NOFORCE_LIBS
//---------------------------------------------------------------------------
#define GetOption(OPTION) GetInstanceOption(this->m_pApiLogParent, OPTION)
#define GetOptionVal(OPTION) GetInstanceOptionVal(this->m_pApiLogParent, OPTION)
//---------------------------------------------------------------------------
#include <afx.h>
#include "wtypes.h"
#include <afxmt.h>

//STL includes
#include <list>
#include <map>
#include <vector>
#include <deque>
#include <set>
#include <algorithm>
//---------------------------------------------------------------------------
class CFileFix;
#define CFile CFileFix
//---------------------------------------------------------------------------
#include "MFC64bitFix.h"
#include <ApiLog.h>
#include <FileZillaApi.h>
#include <FileZillaOpt.h>
#include <Options.h>
#include <Crypt.h>
#include <TextsFileZilla.h>
#include <structures.h>
//---------------------------------------------------------------------------
#include <oleauto.h>
#include <afxdisp.h>
#include <afxconv.h>
//---------------------------------------------------------------------------
#define _strlwr strlwr
#define USEDPARAM(p) ((p) == (p))
//---------------------------------------------------------------------------
const int FILEEXISTS_ASK = -1;
const int FILEEXISTS_OVERWRITE = 0;
const int FILEEXISTS_OVERWRITEIFNEWER = 1;
const int FILEEXISTS_RESUME = 2;
const int FILEEXISTS_RENAME = 3;
const int FILEEXISTS_SKIP = 4;
const int FILEEXISTS_RESUME_ASKONFAIL = 5; // Used by queue for automatic resuming. If APPE failes, ask what to do instead.
//---------------------------------------------------------------------------
class t_ffam_statusmessage
{
public:
  CString status;
  int type;
  BOOL post;
};
//---------------------------------------------------------------------------
typedef struct
{
  __int64 bytes;
#ifdef MPEXT
  __int64 transfersize;
#endif
  int percent;
  int timeelapsed;
  int timeleft;
  int transferrate;
  BOOL bFileTransfer;
} t_ffam_transferstatus;
//---------------------------------------------------------------------------
#undef CFile
//---------------------------------------------------------------------------
// MFC allocates CObject (ancestor of CFile) with new, but deallocates with free,
// what codeguard dislikes, this is fix, not sure if it is necessary for
// release version, but probably causes no harm
class CFileFix: public CFile
{
public:
  void PASCAL operator delete(void* p)
  {
    delete p;
  }
};
//---------------------------------------------------------------------------
#define CFile CFileFix
//---------------------------------------------------------------------------
#endif