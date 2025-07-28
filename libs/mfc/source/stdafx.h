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

// MFC inline constructors (including compiler generated) can get deep
#pragma inline_depth(16)

#define AFX_COMDAT __declspec(selectany)

// core headers
#include "afx.h"
#include "afxplex_.h"

// public headers
#include "afxres.h"
inline HINSTANCE AFXAPI AfxGetResourceHandle()
	{ ASSERT(afxCurrentResourceHandle != NULL);
		return afxCurrentResourceHandle; }

int AFXAPI AfxLoadString(UINT nIDS, LPTSTR lpszBuf, UINT nMaxBuf = 256);
#define _countof(array) (sizeof(array)/sizeof(array[0]))
BOOL AFXAPI AfxFullPath(LPTSTR lpszPathOut, LPCTSTR lpszFileIn);
void AFXAPI AfxGetRoot(LPCTSTR lpszPath, CString& strRoot);

#include <stddef.h>
#include <limits.h>
#include <malloc.h>
#include <new.h>

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

#define min std::min
#define max std::max

/////////////////////////////////////////////////////////////////////////////
