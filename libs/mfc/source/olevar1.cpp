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

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// new COleVariant code (uses new features of OLEAUT32.DLL)

COleVariant::COleVariant(LPCITEMIDLIST pidl)
{
	AfxVariantInit(this);

	if (pidl != NULL)
	{
		// walk through entries in the list and accumulate their size

		UINT cbTotal = 0;
		SAFEARRAY *psa = NULL;
		LPCITEMIDLIST pidlWalker = pidl;

		while (pidlWalker->mkid.cb)
		{
			cbTotal += pidlWalker->mkid.cb;
			pidlWalker = (LPCITEMIDLIST)
				(((LPBYTE)pidlWalker) + pidlWalker->mkid.cb);
		}

		// add the base structure size
		cbTotal += sizeof(ITEMIDLIST);

		// get a safe array for them
		psa = SafeArrayCreateVector(VT_UI1, 0, cbTotal);

		// copy it and set members
		if (psa != NULL)
		{
			memcpy(psa->pvData, (LPBYTE) pidl, cbTotal);
			vt = VT_ARRAY | VT_UI1;
			parray = psa;
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
