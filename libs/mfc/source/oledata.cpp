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

#ifdef CoRegisterClassObject
#undef CoRegisterClassObject
#endif

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

#pragma comment(lib, "ole32.lib")
#pragma comment(lib, "oleaut32.lib")
#pragma comment(lib, "oledlg.lib")
#pragma comment(lib, "urlmon.lib")
#pragma comment(lib, "shell32.lib")
#pragma comment(lib, "comctl32.lib")
#pragma comment(lib, "advapi32.lib")

/////////////////////////////////////////////////////////////////////////////
// _AFX_OLE_STATE implementation

_AFX_OLE_STATE::_AFX_OLE_STATE()
{
	// Note: it is only necessary to intialize non-zero data.
}

_AFX_OLE_STATE::~_AFX_OLE_STATE()
{
}

#pragma warning(disable: 4074)
#pragma init_seg(lib)

PROCESS_LOCAL(_AFX_OLE_STATE, _afxOleState)

/////////////////////////////////////////////////////////////////////////////
