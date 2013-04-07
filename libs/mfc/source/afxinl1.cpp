// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#include "stdafx.h"

#ifndef _AFX_ENABLE_INLINES

#ifdef AFX_DBG1_SEG
#pragma code_seg(AFX_DBG1_SEG)
#endif

static const char _szAfxInl[] = "afx.inl";
#undef THIS_FILE
#define THIS_FILE _szAfxInl
#define _AFX_INLINE
#include "afx.inl"

static const char _szAfxCollInl[] = "afxcoll.inl";
#undef THIS_FILE
#define THIS_FILE _szAfxCollInl
#define _AFXCOLL_INLINE
#include "afxcoll.inl"

// the main 'afxwin.inl' is in a separate file

static const char _szAfxDlgsInl[] = "afxdlgs.inl";
#undef THIS_FILE
#define THIS_FILE _szAfxDlgsInl
#define _AFXDLGS_INLINE
#include "afxdlgs.inl"

// expand inlines
static const char _szAfxExtInl[] = "afxext.inl";
#undef THIS_FILE
#define THIS_FILE _szAfxExtInl
#define _AFXEXT_INLINE
#include "afxext.inl"

#endif //!_AFX_ENABLE_INLINES
