//---------------------------------------------------------------------------
#ifndef StdAfxH
#define StdAfxH
//---------------------------------------------------------------------------
#define _int64 __int64
//---------------------------------------------------------------------------
#define MPEXT_NO_GSS
#define _AFX_ENABLE_INLINES
#define _AFX_NOFORCE_LIBS
//---------------------------------------------------------------------------
#include <afx.h>
#include "wtypes.h"

// STL includes
#include <list>
#include <map>
#include <vector>
#include <deque>
#include <set>
#include <algorithm>
//---------------------------------------------------------------------------
#pragma hdrstop
//---------------------------------------------------------------------------
#include <Global.h>
// these create conflict with afxwin.h
#undef BEGIN_MESSAGE_MAP
#undef END_MESSAGE_MAP
//---------------------------------------------------------------------------
#include "MFC64bitFix.h"
#include <ApiLog.h>
#include <TextsFileZilla.h>
//---------------------------------------------------------------------------
#include <oleauto.h>
#include <afxconv.h>
//---------------------------------------------------------------------------
#define _strlwr strlwr
//---------------------------------------------------------------------------
const int FILEEXISTS_OVERWRITE = 0;
const int FILEEXISTS_RESUME = 1;
const int FILEEXISTS_RENAME = 2;
const int FILEEXISTS_SKIP = 3;
const int FILEEXISTS_COMPLETE = 4;
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
  __int64 transfersize;
  BOOL bFileTransfer;
} t_ffam_transferstatus;
//---------------------------------------------------------------------------
#include <FileZillaApi.h>
#include <FileZillaOpt.h>
//---------------------------------------------------------------------------
#endif
