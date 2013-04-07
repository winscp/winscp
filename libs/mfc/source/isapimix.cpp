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
#include <afxisapi.h>

CHttpServerContext& CHttpServerContext::operator<<(const CLongBinary& blob)
{
	ISAPIASSERT(m_pStream != NULL);
	if (m_pStream != NULL) *m_pStream << blob;
		return *this;
}

CHttpServerContext& CHttpServerContext::operator<<(const CByteArray& array)
{
	ISAPIASSERT(m_pStream != NULL);
	if (m_pStream != NULL) *m_pStream << array;
		return *this;
}

CHtmlStream& CHtmlStream::operator<<(const CByteArray& array)
{
	int nSize = array.GetSize();
	if (nSize > 0)
	{
		const BYTE* pStart = array.GetData();
		if (pStart != NULL)
			Write(pStart, nSize);
	}

	return *this;
}

CHtmlStream& CHtmlStream::operator<<(const CLongBinary& blob)
{
	if (blob.m_dwDataLength > 0 && blob.m_hData != NULL)
	{
		LPVOID lpData = GlobalLock(blob.m_hData);
		if (lpData != NULL)
		{
			Write(lpData, blob.m_dwDataLength);
			GlobalUnlock(blob.m_hData);
		}
	}

	return *this;
}
