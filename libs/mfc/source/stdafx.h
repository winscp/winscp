// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// STDAFX.H is the header that includes the standard includes that are used
//  for most of the project.  These are compiled into a pre-compiled header

// turn off warnings for /W4 (just for MFC implementation)
#ifndef ALL_WARNINGS
#pragma warning(disable: 4073)  // disable warning about using init_seg
#endif

// MFC inline constructors (including compiler generated) can get deep
#pragma inline_depth(16)

#ifdef _AFX_DEVBUILD
	#define AFX_IMPL_DATA AFX_DATA_EXPORT
#else
	#define AFX_IMPL_DATA
#endif

// override default values for data import/export when building MFC DLLs
#ifdef _AFX_CORE_IMPL
	#define AFX_CORE_DATA   AFX_IMPL_DATA
	#define AFX_CORE_DATADEF
#endif

#ifdef _AFX_OLE_IMPL
	#define AFX_OLE_DATA    AFX_IMPL_DATA
	#define AFX_OLE_DATADEF
#endif

#ifdef _AFX_DB_IMPL
	#define AFX_DB_DATA     AFX_IMPL_DATA
	#define AFX_DB_DATADEF
#endif

#ifdef _AFX_NET_IMPL
	#define AFX_NET_DATA    AFX_IMPL_DATA
	#define AFX_NET_DATADEF
#endif

#define _AFX_NOFORCE_LIBS
#define _AFX_FULLTYPEINFO
#define VC_EXTRALEAN
#define NO_ANSIUNI_ONLY
#define _MFC_OVERRIDES_NEW

#define AFX_COMDAT __declspec(selectany)

// core headers
#include "afx.h"
#include "afxplex_.h"
#include "afxcoll.h"

// public headers
#include "afxwin.h"
#include "afxdlgs.h"
#include "afxext.h"

#ifndef _AFX_NO_OLEDB_SUPPORT
	#include "atlbase.h"
#endif

#ifndef _AFX_NO_OLE_SUPPORT
	#ifndef _OLE2_H_
		#include <ole2.h>
	#endif

#include <winspool.h>

// include OLE dialog/helper APIs
#ifndef _OLEDLG_H_
	#include <oledlg.h>
#endif

#include <winreg.h>
	#include "afxcom_.h"
#include "afxole.h"
#include "afxdtctl.h"

#include "afxdocob.h"

#ifndef _AFX_NO_DAO_SUPPORT
	#include "afxdao.h"
#endif

	#include "afxodlgs.h"
#endif

#ifndef _AFX_NO_OCX_SUPPORT
	#include "afxctl.h"
#endif
#ifndef _AFX_NO_DB_SUPPORT
	#include "afxdb.h"
#endif
#ifndef _AFX_NO_SYNC_SUPPORT
	#include "afxmt.h"
#endif
#ifndef _AFX_NO_INET_SUPPORT
	#include "afxinet.h"
#endif

// private headers as well
#include "afxpriv.h"
#include "afximpl.h"
#include "winhand_.h"
#ifndef _AFX_NO_OLE_SUPPORT
	#include "oleimpl2.h"
#endif
#ifndef _AFX_NO_OCX_SUPPORT
	#include "ctlimpl.h"
#endif
#ifndef _AFX_NO_DB_SUPPORT
	#include "dbimpl.h"
#endif
#ifndef _AFX_NO_DAO_SUPPORT
	#include "daoimpl.h"
#endif
#ifndef _AFX_NO_SOCKET_SUPPORT
	#ifndef _WINSOCKAPI_
		#include <winsock.h>
	#endif
	#include "sockimpl.h"
	#include "afxsock.h"
#endif
#ifndef _AFX_NO_AFXCMN_SUPPORT
	#include "afxcmn.h"
	#include "afxcview.h"
#endif
#ifndef _AFX_NO_RICHEDIT_SUPPORT
	#include "afxrich.h"
#endif
#ifndef _AFX_NO_DHTML_SUPPORT
	#include "afxhtml.h"
#endif

#include <winreg.h>
#include <winnls.h>
#include <stddef.h>
#include <limits.h>
#include <malloc.h>
#include <new.h>
#ifndef _AFX_OLD_EXCEPTIONS
#include <eh.h>     // for set_terminate
#endif

#undef AfxWndProc

// implementation uses _AFX_PACKING as well
#ifdef _AFX_PACKING
#ifndef ALL_WARNINGS
#pragma warning(disable: 4103)
#endif
#ifndef __BORLANDC__
// In Borland C++ we set the packing to 4 in the BCC32.CFG file
// This is because the inclusion of the following pragma line disables our
// Pre-Compiled-Headers
#pragma pack(_AFX_PACKING)
#endif // __BORLANDC__
#endif

// special exception handling just for MFC library implementation
#ifndef _AFX_OLD_EXCEPTIONS

// MFC does not rely on auto-delete semantics of the TRY..CATCH macros,
//  therefore those macros are mapped to something closer to the native
//  C++ exception handling mechanism when building MFC itself.

#undef TRY
#define TRY { try {

#undef CATCH
#define CATCH(class, e) } catch (class* e) \
	{ ASSERT(e->IsKindOf(RUNTIME_CLASS(class))); UNUSED(e);

#undef AND_CATCH
#define AND_CATCH(class, e) } catch (class* e) \
	{ ASSERT(e->IsKindOf(RUNTIME_CLASS(class))); UNUSED(e);

#undef CATCH_ALL
#define CATCH_ALL(e) } catch (CException* e) \
	{ { ASSERT(e->IsKindOf(RUNTIME_CLASS(CException))); UNUSED(e);

#undef AND_CATCH_ALL
#define AND_CATCH_ALL(e) } catch (CException* e) \
	{ { ASSERT(e->IsKindOf(RUNTIME_CLASS(CException))); UNUSED(e);

#undef END_TRY
#define END_TRY } catch (CException* e) \
	{ ASSERT(e->IsKindOf(RUNTIME_CLASS(CException))); e->Delete(); } }

#undef THROW_LAST
#define THROW_LAST() throw

// Because of the above definitions of TRY...CATCH it is necessary to
//  explicitly delete exception objects at the catch site.

#define DELETE_EXCEPTION(e) do { e->Delete(); } while (0)
#define NO_CPP_EXCEPTION(expr)

#else   //!_AFX_OLD_EXCEPTIONS

// In this case, the TRY..CATCH macros provide auto-delete semantics, so
//  it is not necessary to explicitly delete exception objects at the catch site.

#define DELETE_EXCEPTION(e)
#define NO_CPP_EXCEPTION(expr) expr

#endif  //_AFX_OLD_EXCEPTIONS

/////////////////////////////////////////////////////////////////////////////
