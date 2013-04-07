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

#ifdef AFXCTL_PROP_SEG
#pragma code_seg(AFXCTL_PROP_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

#ifdef _DEBUG
#define ASSERT_BUFFER_VALID(p, cb, bWrite) \
	ASSERT(AfxIsValidAddress(p, cb, bWrite))
#else
#define ASSERT_BUFFER_VALID(p, cb, bWrite)
#endif

// Old class ID for picture type
AFX_STATIC_DATA const CLSID _afx_CLSID_StdPicture2_V1 =
	{ 0xfb8f0824,0x0164,0x101b, { 0x84,0xed,0x08,0x00,0x2b,0x2e,0xc7,0x13 } };

/////////////////////////////////////////////////////////////////////////////
// _AfxGetArchiveStream

LPSTREAM AFXAPI _AfxGetArchiveStream(CArchive& ar, CArchiveStream& stm)
{
	// Obtain direct access to the archive's LPSTREAM.
	ar.Flush();
	CFile* pFile = ar.GetFile();
	ASSERT(pFile != NULL);
	LPSTREAM pstm;
	if (pFile->IsKindOf(RUNTIME_CLASS(COleStreamFile)))
	{
		pstm = ((COleStreamFile*)pFile)->m_lpStream;
		ASSERT(pstm != NULL);
	}
	else
	{
		ASSERT(stm.m_pArchive == NULL || stm.m_pArchive == &ar);
		stm.m_pArchive = &ar;
		pstm = &stm;
	}
	return pstm;
}

/////////////////////////////////////////////////////////////////////////////
// _AfxInitBlob

BOOL AFXAPI _AfxInitBlob(HGLOBAL* phDst, void* pvSrc)
{
	BOOL bResult = FALSE;
	ULONG cb;
	if ((cb = *(long*)pvSrc) > 0)
	{
		ASSERT_BUFFER_VALID(pvSrc, sizeof(cb) + cb, TRUE);
		*phDst = GlobalAlloc(GMEM_MOVEABLE, sizeof(cb) + cb);
		if (*phDst != NULL)
		{
			void* pvDst = GlobalLock(*phDst);
			ASSERT(pvDst != NULL);
			memcpy(pvDst, pvSrc, sizeof(cb) + cb);
			bResult = TRUE;
			GlobalUnlock(*phDst);
		}
	}
	return bResult;
}

/////////////////////////////////////////////////////////////////////////////
// _AfxCopyBlob

BOOL AFXAPI _AfxCopyBlob(HGLOBAL* phDst, HGLOBAL hSrc)
{
	BOOL bResult = FALSE;
	void* pvSrc = GlobalLock(hSrc);
	if (pvSrc != NULL)
	{
		bResult = _AfxInitBlob(phDst, pvSrc);
		GlobalUnlock(hSrc);
	}
	return bResult;
}

/////////////////////////////////////////////////////////////////////////////
// _AfxCopyPropValue

BOOL AFXAPI _AfxCopyPropValue(VARTYPE vtProp, void* pvDest, const void * pvSrc)
{
	ASSERT(AfxIsValidAddress(pvDest, 1));

	if (pvSrc != NULL)
	{
		ASSERT(AfxIsValidAddress(pvSrc, 1, FALSE));

		switch (vtProp)
		{
		case VT_UI1:
			*(BYTE*)pvDest = *(BYTE*)pvSrc;
			break;
		case VT_I2:
			*(short*)pvDest = *(short*)pvSrc;
			break;
		case VT_I4:
			*(long*)pvDest = *(long*)pvSrc;
			break;
		case VT_BOOL:
			*(BOOL*)pvDest = *(BOOL*)pvSrc;
			break;
		case VT_BSTR:
			*(CString*)pvDest = *(CString*)pvSrc;
			break;
		case VT_LPSTR:
			*(CString*)pvDest = (LPCTSTR)pvSrc;
			break;
		case VT_CY:
			*(CY*)pvDest = *(CY*)pvSrc;
			break;
		case VT_R4:
			*(_AFX_FLOAT*)pvDest = *(_AFX_FLOAT*)pvSrc;
			break;
		case VT_R8:
			*(_AFX_DOUBLE*)pvDest = *(_AFX_DOUBLE*)pvSrc;
			break;
		default:
			return FALSE;
		}
	}
	return pvSrc != NULL;
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::ExchangeExtent

BOOL COleControl::ExchangeExtent(CPropExchange* pPX)
{
	// Save extent
	SIZEL szl;
	szl.cx = m_cxExtent;
	szl.cy = m_cyExtent;

	if (PX_Long(pPX, _T("_ExtentX"), szl.cx) &&
		PX_Long(pPX, _T("_ExtentY"), szl.cy))
	{
		if ((pPX->IsLoading()) &&
			((m_cxExtent != szl.cx) || (m_cyExtent != szl.cy)))
		{
			m_xOleObject.SetExtent(DVASPECT_CONTENT, &szl);
		}
		return TRUE;
	}
	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// Serialization for version number

DWORD COleControl::SerializeVersion(CArchive& ar, DWORD dwVersionDefault,
	BOOL bConvert)
{
	DWORD dwVersion;

	if (ar.IsLoading())
	{
		ar >> m_dwVersionLoaded;
		dwVersion = m_dwVersionLoaded;
	}
	else
	{
		dwVersion = bConvert ? dwVersionDefault : m_dwVersionLoaded;
		ar << dwVersion;
	}

	return dwVersion;
}

/////////////////////////////////////////////////////////////////////////////
// Initialization for version number

void COleControl::ResetVersion(DWORD dwVersionDefault)
{
	m_dwVersionLoaded = dwVersionDefault;
}

/////////////////////////////////////////////////////////////////////////////
// Serialization for extent

void COleControl::SerializeExtent(CArchive& ar)
{
	if (ar.IsLoading())
	{
		SIZEL szl;
		ar >> szl.cx;
		ar >> szl.cy;
		if ((m_cxExtent != szl.cx) || (m_cyExtent != szl.cy))
			m_xOleObject.SetExtent(DVASPECT_CONTENT, &szl);
	}
	else
	{
		ar << m_cxExtent;
		ar << m_cyExtent;
	}
}

/////////////////////////////////////////////////////////////////////////////
// CPropExchange member functions

BOOL CPropExchange::ExchangeVersion(DWORD& dwVersionLoaded,
	DWORD dwVersionDefault, BOOL bConvert)
{
	BOOL bResult;
	if (m_bLoading)
	{
		bResult = PX_ULong(this, _T("_Version"), m_dwVersion,
			dwVersionDefault);
		dwVersionLoaded = m_dwVersion;
	}
	else
	{
		m_dwVersion = bConvert ? dwVersionDefault : dwVersionLoaded;
		bResult = PX_ULong(this, _T("_Version"), m_dwVersion);
	}
	return bResult;
}

/////////////////////////////////////////////////////////////////////////////
// CArchivePropExchange member functions

CArchivePropExchange::CArchivePropExchange(CArchive& ar) :
	m_ar(ar)
{
	ASSERT_POINTER(&ar, CArchive);
	m_bLoading = m_ar.IsLoading();
}

BOOL CArchivePropExchange::ExchangeProp(LPCTSTR pszPropName, VARTYPE vtProp,
		void* pvProp, const void* pvDefault)
{
	UNUSED(pszPropName);     // unused in release builds
	UNUSED(pvDefault);       // unused in release builds

	ASSERT(AfxIsValidString(pszPropName));
	ASSERT(AfxIsValidAddress(pvProp, 1, FALSE));
	ASSERT((pvDefault == NULL) || AfxIsValidAddress(pvDefault, 1, FALSE));

	if (m_bLoading)
	{
		switch (vtProp)
		{
		case VT_UI1:
			m_ar >> *(BYTE*)pvProp;
			break;
		case VT_I2:
			m_ar >> *(WORD*)pvProp;
			break;
		case VT_I4:
			m_ar >> *(long*)pvProp;
			break;
		case VT_BOOL:
			*(BOOL*)pvProp = 0;
			m_ar >> *(BYTE*)pvProp;
			break;
		case VT_LPSTR:
		case VT_BSTR:
			m_ar >> *(CString*)pvProp;
			break;
		case VT_CY:
			m_ar >> ((CY*)pvProp)->Lo;
			m_ar >> ((CY*)pvProp)->Hi;
			break;
		case VT_R4:
			m_ar >> *(float*)pvProp;
			break;
		case VT_R8:
			m_ar >> *(double*)pvProp;
			break;
		}
	}
	else
	{
		switch (vtProp)
		{
		case VT_UI1:
			m_ar << *(BYTE*)pvProp;
			break;
		case VT_I2:
			m_ar << *(WORD*)pvProp;
			break;
		case VT_I4:
			m_ar << *(long*)pvProp;
			break;
		case VT_BOOL:
			m_ar << *(BYTE*)pvProp;
			break;
		case VT_LPSTR:
		case VT_BSTR:
			m_ar << *(CString*)pvProp;
			break;
		case VT_CY:
			m_ar << ((CY*)pvProp)->Lo;
			m_ar << ((CY*)pvProp)->Hi;
			break;
		case VT_R4:
			m_ar << *(float*)pvProp;
			break;
		case VT_R8:
			m_ar << *(double*)pvProp;
			break;
		}
	}

	return TRUE;
}

BOOL CArchivePropExchange::ExchangeBlobProp(LPCTSTR pszPropName,
	HGLOBAL* phBlob, HGLOBAL /*hBlobDefault*/)
{
	UNUSED(pszPropName);     // unused in release builds

	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(phBlob, HGLOBAL);

	DWORD cb;

	if (m_bLoading)
	{
		// free previous memory block
		if (*phBlob != NULL)
		{
			GlobalFree(*phBlob);
			*phBlob = NULL;
		}

		// read new size
		m_ar >> cb;

		// allocate and fill new memory block
		*phBlob = GlobalAlloc(GMEM_MOVEABLE, sizeof(cb)+cb);
		if (*phBlob != NULL)
		{
			void* pvBlob = GlobalLock(*phBlob);
			ASSERT(pvBlob != NULL);
			*(long*)pvBlob = cb;
			if (m_ar.Read((BYTE*)pvBlob+sizeof(cb), cb) != cb)
				AfxThrowArchiveException(CArchiveException::endOfFile);
			GlobalUnlock(*phBlob);
		}
	}
	else
	{
		if (*phBlob != NULL)
		{
			void* pvBlob = GlobalLock(*phBlob);
			ASSERT(pvBlob != NULL);
			cb = *(long*)pvBlob;
			ASSERT_BUFFER_VALID(pvBlob, sizeof(cb)+cb, FALSE);
			m_ar.Write(pvBlob, sizeof(cb)+cb);
			GlobalUnlock(*phBlob);
		}
		else
			m_ar << (DWORD)0;
	}
	return TRUE;
}

BOOL AFXAPI _AfxPeekAtClassIDInStream(LPSTREAM pstm, LPCLSID lpClassID)
{
	// Read the class ID, then restore the seek pointer.
	LARGE_INTEGER li;
	li.LowPart = (DWORD)(-(long)sizeof(CLSID));
	li.HighPart = -1;

	return (SUCCEEDED(ReadClassStm(pstm, lpClassID)) &&
		SUCCEEDED(pstm->Seek(li, STREAM_SEEK_CUR, NULL)));
}

BOOL AFXAPI _AfxIsSameUnknownObject(REFIID iid, LPUNKNOWN pUnk1, LPUNKNOWN pUnk2)
{
	if (pUnk1 == pUnk2)
		return TRUE;

	if (pUnk1 == NULL || pUnk2 == NULL)
		return FALSE;

	LPUNKNOWN pI1 = NULL;
	LPUNKNOWN pI2 = NULL;
	BOOL bResult = FALSE;
	if (SUCCEEDED(pUnk1->QueryInterface(iid, (void**)&pI1)))
	{
		ASSERT_POINTER(pI1, IUnknown);
		if (SUCCEEDED(pUnk2->QueryInterface(iid, (void**)&pI2)))
		{
			ASSERT_POINTER(pI2, IUnknown);
			bResult = (pI1 == pI2);
			pI2->Release();
		}
		pI1->Release();
	}
	return bResult;
}

BOOL CArchivePropExchange::ExchangePersistentProp(LPCTSTR pszPropName,
		LPUNKNOWN* ppUnk, REFIID iid, LPUNKNOWN pUnkDefault)
{
	UNUSED(pszPropName);     // unused in release builds

	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(ppUnk, LPUNKNOWN);
	ASSERT_NULL_OR_POINTER(pUnkDefault, IUnknown);

	BOOL bResult = FALSE;
	CArchiveStream stm(&m_ar);

	if (m_bLoading)
	{
		RELEASE(*ppUnk);
		*ppUnk = NULL;

		BYTE bFlag;
		m_ar >> bFlag;
		if (bFlag != 0xFF)
		{
			// read the CLSID
			CLSID clsid;
			m_ar >> clsid.Data1;
			m_ar >> clsid.Data2;
			m_ar >> clsid.Data3;
			m_ar.Read(&clsid.Data4[0], sizeof clsid.Data4);

			// check for GUID_NULL first and skip if found
			if (IsEqualCLSID(clsid, GUID_NULL))
				bResult = TRUE;
			else
			{
				// otherwise will need a stream
				LPSTREAM pstm = _AfxGetArchiveStream(m_ar, stm);
				if (IsEqualCLSID(clsid, CLSID_StdPicture) ||
					IsEqualCLSID(clsid, _afx_CLSID_StdPicture2_V1))
				{
					// special case for pictures
					bResult = SUCCEEDED(::OleLoadPicture(pstm, 0, FALSE, iid,
						(void**)ppUnk));
				}
				else
				{
					// otherwise, seek back to the CLSID
					LARGE_INTEGER li;
					li.LowPart = (DWORD)(-(long)sizeof(CLSID));
					li.HighPart = -1;
					VERIFY(SUCCEEDED(pstm->Seek(li, STREAM_SEEK_CUR, NULL)));

					// and load the object normally

					CLSID clsid;
					if (SUCCEEDED(::ReadClassStm(pstm, &clsid)) &&
							(SUCCEEDED(::CoCreateInstance(clsid, NULL,
								CLSCTX_SERVER | CLSCTX_REMOTE_SERVER,
								iid, (void**)ppUnk)) ||
							SUCCEEDED(::CoCreateInstance(clsid, NULL,
								CLSCTX_SERVER & ~CLSCTX_REMOTE_SERVER,
								iid, (void**)ppUnk))))
					{
						LPPERSISTSTREAM pps = NULL;
						if (SUCCEEDED((*ppUnk)->QueryInterface(
								IID_IPersistStream, (void**)&pps)) ||
							SUCCEEDED((*ppUnk)->QueryInterface(
								IID_IPersistStreamInit, (void**)&pps)))
						{
							ASSERT_POINTER(pps, IPersistStream);
							bResult = SUCCEEDED(pps->Load(pstm));
							pps->Release();
						}

						if (!bResult)
						{
							(*ppUnk)->Release();
							*ppUnk = NULL;
						}
					}
				}
			}
		}
		else
		{
			// Use default value.
			bResult = pUnkDefault == NULL ||
				SUCCEEDED(pUnkDefault->QueryInterface(iid, (LPVOID*)ppUnk));
		}
	}
	else
	{
		ASSERT_NULL_OR_POINTER(*ppUnk, IUnknown);

		// Check if *ppUnk and pUnkDefault are the same thing.  If so, don't
		// bother saving the object; just write a special flag instead.

		if (_AfxIsSameUnknownObject(iid, *ppUnk, pUnkDefault))
		{
			m_ar << (BYTE)0xFF;
			bResult = TRUE;
		}
		else
		{
			m_ar << (BYTE)0x00;
			if (*ppUnk != NULL)
			{
				LPPERSISTSTREAM pps = NULL;
				if (SUCCEEDED((*ppUnk)->QueryInterface(
						IID_IPersistStream, (void**)&pps)) ||
					SUCCEEDED((*ppUnk)->QueryInterface(
						IID_IPersistStreamInit, (void**)&pps)))
				{
					ASSERT_POINTER(pps, IPersistStream);
					LPSTREAM pstm = _AfxGetArchiveStream(m_ar, stm);
					bResult = SUCCEEDED(::OleSaveToStream(pps, pstm));
					pps->Release();
				}
			}
			else
			{
				// If no object, write null class ID.
				m_ar.Write(&GUID_NULL, sizeof(GUID));
			}
		}
	}

	// throw exception in case of unthrown errors
	if (!bResult)
		AfxThrowArchiveException(CArchiveException::generic);

	return TRUE;
}

BOOL CArchivePropExchange::ExchangeFontProp(LPCTSTR pszPropName,
	CFontHolder& font, const FONTDESC* pFontDesc, LPFONTDISP pFontDispAmbient)
{
	UNUSED(pszPropName);     // unused in release builds

	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&font, CFontHolder);
	ASSERT_NULL_OR_POINTER(pFontDesc, FONTDESC);
	ASSERT_NULL_OR_POINTER(pFontDispAmbient, IFontDisp);

	BOOL bResult = FALSE;
	CArchiveStream stm(&m_ar);
	LPFONT pFont;

	if (m_bLoading)
	{
		BYTE bFlag;
		m_ar >> bFlag;
		if (bFlag != 0xFF)
		{
			LPSTREAM pstm = _AfxGetArchiveStream(m_ar, stm);
			pFont = _AfxCreateFontFromStream(pstm);
			if (pFont != NULL)
			{
				font.SetFont(pFont);
				bResult = TRUE;
			}
		}
		if (!bResult)
		{
			font.InitializeFont(pFontDesc, pFontDispAmbient);
			bResult = TRUE;
		}
	}
	else
	{
		pFont = font.m_pFont;
		if (pFont != NULL)
		{
			// If same as ambient font (or error), write 0xFF for the flag
			if (!_AfxIsSameFont(font, pFontDesc, pFontDispAmbient))
			{
				LPPERSISTSTREAM pps = NULL;
				if (SUCCEEDED(pFont->QueryInterface(IID_IPersistStream,
					(LPVOID*)&pps)))
				{
					ASSERT_POINTER(pps, IPersistStream);
					m_ar << (BYTE)0x00;
					LPSTREAM pstm = _AfxGetArchiveStream(m_ar, stm);
					bResult = SUCCEEDED(::OleSaveToStream(pps, pstm));
					pps->Release();
					if (!bResult)
						AfxThrowArchiveException(CArchiveException::generic);
				}
			}
		}
		if (!bResult)
		{
			m_ar << (BYTE)0xFF;
			bResult = TRUE;
		}
	}
	return bResult;
}

/////////////////////////////////////////////////////////////////////////////
// CResetPropExchange member functions

CResetPropExchange::CResetPropExchange()
{
	m_bLoading = TRUE;
}

BOOL CResetPropExchange::ExchangeProp(LPCTSTR pszPropName, VARTYPE vtProp,
		void* pvProp, const void* pvDefault)
{
	UNUSED(pszPropName);     // unused in release builds

	ASSERT(AfxIsValidString(pszPropName));
	ASSERT(AfxIsValidAddress(pvProp, 1, FALSE));
	ASSERT((pvDefault == NULL) || AfxIsValidAddress(pvDefault, 1, FALSE));

	return _AfxCopyPropValue(vtProp, pvProp, pvDefault);
}

BOOL CResetPropExchange::ExchangeBlobProp(LPCTSTR pszPropName,
	HGLOBAL* phBlob, HGLOBAL hBlobDefault)
{
	UNUSED(pszPropName);     // unused in release builds

	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(phBlob, HGLOBAL);

	// free the previous memory block
	if (*phBlob != NULL)
	{
		GlobalFree(*phBlob);
		*phBlob = NULL;
	}

	// copy the memory block
	BOOL bResult = TRUE;
	if (hBlobDefault != NULL)
		bResult = _AfxCopyBlob(phBlob, hBlobDefault);
	return bResult;
}

BOOL CResetPropExchange::ExchangePersistentProp(LPCTSTR pszPropName,
		LPUNKNOWN* ppUnk, REFIID iid, LPUNKNOWN pUnkDefault)
{
	UNUSED(pszPropName);     // unused in release builds

	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(ppUnk, LPUNKNOWN);
	ASSERT_NULL_OR_POINTER(pUnkDefault, IUnknown);

	RELEASE(*ppUnk);

	BOOL bResult = TRUE;
	if (pUnkDefault != NULL)
	{
		bResult = SUCCEEDED(pUnkDefault->QueryInterface(iid,
			(LPVOID*)ppUnk));
	}
	return bResult;
}

BOOL CResetPropExchange::ExchangeFontProp(
	LPCTSTR pszPropName, CFontHolder& font, const FONTDESC* pFontDesc,
	LPFONTDISP pFontDispAmbient)
{
	UNUSED(pszPropName);     // unused in release builds

	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&font, CFontHolder);
	ASSERT_NULL_OR_POINTER(pFontDesc, FONTDESC);
	ASSERT_NULL_OR_POINTER(pFontDispAmbient, IFontDisp);

	font.InitializeFont(pFontDesc, pFontDispAmbient);
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CAsyncPropExchange member functions

CAsyncPropExchange::CAsyncPropExchange(DWORD dwVersion)
{
	m_bAsync = TRUE;
	m_bLoading = TRUE;
	m_dwVersion = dwVersion;
}

BOOL CAsyncPropExchange::ExchangeVersion(DWORD& dwVersionLoaded,
	DWORD dwVersionDefault, BOOL bConvert)
{
	if (m_bLoading)
	{
		dwVersionLoaded = m_dwVersion;
		return TRUE;
	}
	else
	{
		return m_dwVersion == (bConvert ? dwVersionDefault : dwVersionLoaded);
	}
}

BOOL CAsyncPropExchange::ExchangeProp(LPCTSTR pszPropName, VARTYPE vtProp,
	void* pvProp, const void* pvDefault)
{
	// Should never be called
	UNUSED_ALWAYS(pszPropName);
	UNUSED_ALWAYS(vtProp);
	UNUSED_ALWAYS(pvProp);
	UNUSED_ALWAYS(pvDefault);
	ASSERT(FALSE);
	return FALSE;
}

BOOL CAsyncPropExchange::ExchangeBlobProp(LPCTSTR pszPropName, HGLOBAL* phBlob,
	HGLOBAL hBlobDefault)
{
	// Should never be called
	UNUSED_ALWAYS(pszPropName);
	UNUSED_ALWAYS(phBlob);
	UNUSED_ALWAYS(hBlobDefault);
	ASSERT(FALSE);
	return FALSE;
}

BOOL CAsyncPropExchange::ExchangeFontProp(LPCTSTR pszPropName,
	CFontHolder& font, const FONTDESC* pFontDesc, LPFONTDISP pFontDispAmbient)
{
	// Should never be called
	UNUSED_ALWAYS(pszPropName);
	UNUSED_ALWAYS(font);
	UNUSED_ALWAYS(pFontDesc);
	UNUSED_ALWAYS(pFontDispAmbient);
	ASSERT(FALSE);
	return FALSE;
}

BOOL CAsyncPropExchange::ExchangePersistentProp(LPCTSTR pszPropName,
	LPUNKNOWN* ppUnk, REFIID iid, LPUNKNOWN pUnkDefault)
{
	// Should never be called
	UNUSED_ALWAYS(pszPropName);
	UNUSED_ALWAYS(ppUnk);
	UNUSED_ALWAYS(iid);
	UNUSED_ALWAYS(pUnkDefault);
	ASSERT(FALSE);
	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// PX_ functions

BOOL AFX_CDECL PX_Short(CPropExchange* pPX, LPCTSTR pszPropName, short& sValue)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&sValue, short);

	if (pPX->IsAsynchronous())
		return TRUE;
	return  pPX->ExchangeProp(pszPropName, VT_I2, &sValue);
}

BOOL AFX_CDECL PX_Short(CPropExchange* pPX, LPCTSTR pszPropName, short& sValue,
	short sDefault)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&sValue, short);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_I2, &sValue, &sDefault);
}

BOOL AFX_CDECL PX_UShort(CPropExchange* pPX, LPCTSTR pszPropName, USHORT& usValue)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&usValue, USHORT);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_I2, &usValue);
}

BOOL AFX_CDECL PX_UShort(CPropExchange* pPX, LPCTSTR pszPropName, USHORT& usValue,
	USHORT usDefault)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&usValue, USHORT);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_I2, &usValue, &usDefault);
}

BOOL AFX_CDECL PX_Long(CPropExchange* pPX, LPCTSTR pszPropName, long& lValue)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&lValue, long);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_I4, &lValue);
}

BOOL AFX_CDECL PX_Long(CPropExchange* pPX, LPCTSTR pszPropName, long& lValue,
	long lDefault)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&lValue, long);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_I4, &lValue, &lDefault);
}

BOOL AFX_CDECL PX_ULong(CPropExchange* pPX, LPCTSTR pszPropName, ULONG& ulValue)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&ulValue, ULONG);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_I4, &ulValue);
}

BOOL AFX_CDECL PX_ULong(CPropExchange* pPX, LPCTSTR pszPropName, ULONG& ulValue,
	ULONG ulDefault)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&ulValue, ULONG);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_I4, &ulValue, &ulDefault);
}

BOOL AFX_CDECL PX_Color(CPropExchange* pPX, LPCTSTR pszPropName, OLE_COLOR& clrValue)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&clrValue, OLE_COLOR);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_I4, &clrValue);
}

BOOL AFX_CDECL PX_Color(CPropExchange* pPX, LPCTSTR pszPropName, OLE_COLOR& clrValue,
	OLE_COLOR clrDefault)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&clrValue, OLE_COLOR);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_I4, &clrValue, &clrDefault);
}

BOOL AFX_CDECL PX_Bool(CPropExchange* pPX, LPCTSTR pszPropName, BOOL& bValue)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&bValue, BOOL);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_BOOL, &bValue);
}

BOOL AFX_CDECL PX_Bool(CPropExchange* pPX, LPCTSTR pszPropName, BOOL& bValue,
	BOOL bDefault)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&bValue, BOOL);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_BOOL, &bValue, &bDefault);
}

BOOL AFX_CDECL PX_String(CPropExchange* pPX, LPCTSTR pszPropName, CString& strValue)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&strValue, CString);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_BSTR, &strValue);
}

BOOL AFX_CDECL PX_String(CPropExchange* pPX, LPCTSTR pszPropName, CString& strValue,
	const CString& strDefault)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&strValue, CString);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_BSTR, &strValue, &strDefault);
}

BOOL AFX_CDECL PX_String(CPropExchange* pPX, LPCTSTR pszPropName, CString& strValue,
	LPCTSTR lpszDefault)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&strValue, CString);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_LPSTR, &strValue, lpszDefault);
}

BOOL AFX_CDECL PX_Currency(CPropExchange* pPX, LPCTSTR pszPropName, CY& cyValue)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&cyValue, CY);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_CY, &cyValue);
}

BOOL AFX_CDECL PX_Currency(CPropExchange* pPX, LPCTSTR pszPropName, CY& cyValue,
	CY cyDefault)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&cyValue, CY);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_CY, &cyValue, &cyDefault);
}

BOOL AFX_CDECL PX_Float(CPropExchange* pPX, LPCTSTR pszPropName, float& floatValue)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&floatValue, float);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_R4, &floatValue);
}

BOOL AFX_CDECL PX_Float(CPropExchange* pPX, LPCTSTR pszPropName, float& floatValue,
	float floatDefault)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&floatValue, float);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_R4, &floatValue, &floatDefault);
}

BOOL AFX_CDECL PX_Double(CPropExchange* pPX, LPCTSTR pszPropName, double& doubleValue)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&doubleValue, double);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_R8, &doubleValue);
}

BOOL AFX_CDECL PX_Double(CPropExchange* pPX, LPCTSTR pszPropName, double& doubleValue,
	double doubleDefault)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&doubleValue, double);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeProp(pszPropName, VT_R8, &doubleValue, &doubleDefault);
}

BOOL AFX_CDECL PX_Blob(CPropExchange* pPX, LPCTSTR pszPropName, HGLOBAL& hBlob,
	HGLOBAL hBlobDefault)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&hBlob, HGLOBAL);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeBlobProp(pszPropName, &hBlob, hBlobDefault);
}

BOOL AFX_CDECL PX_Font(CPropExchange* pPX, LPCTSTR pszPropName, CFontHolder& font,
		const FONTDESC* pFontDesc, LPFONTDISP pFontDispAmbient)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&font, CFontHolder);
	ASSERT_NULL_OR_POINTER(pFontDesc, FONTDESC);
	ASSERT_NULL_OR_POINTER(pFontDispAmbient, IFontDisp);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangeFontProp(pszPropName, font, pFontDesc,
		pFontDispAmbient);
}

BOOL AFX_CDECL PX_IUnknown(CPropExchange* pPX, LPCTSTR pszPropName, LPUNKNOWN& pUnk,
	REFIID iid, LPUNKNOWN pUnkDefault)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&pUnk, LPUNKNOWN);
	ASSERT_NULL_OR_POINTER(pUnk, IUnknown);
	ASSERT_NULL_OR_POINTER(pUnkDefault, IUnknown);

	if (pPX->IsAsynchronous())
		return TRUE;
	return pPX->ExchangePersistentProp(pszPropName, &pUnk, iid, pUnkDefault);
}

BOOL AFX_CDECL PX_Picture(CPropExchange* pPX, LPCTSTR pszPropName, CPictureHolder& pict)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&pict, CPictureHolder);

	if (pPX->IsAsynchronous())
		return TRUE;
	LPUNKNOWN& pUnk = (LPUNKNOWN&)pict.m_pPict;
	return PX_IUnknown(pPX, pszPropName, pUnk, IID_IPicture);
}

BOOL AFX_CDECL PX_Picture(CPropExchange* pPX, LPCTSTR pszPropName, CPictureHolder& pict,
	CPictureHolder& pictDefault)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&pict, CPictureHolder);

	if (pPX->IsAsynchronous())
		return TRUE;
	LPUNKNOWN& pUnk = (LPUNKNOWN&)pict.m_pPict;
	return PX_IUnknown(pPX, pszPropName, pUnk, IID_IPicture,
		pictDefault.m_pPict);
}

BOOL AFX_CDECL PX_VBXFontConvert(CPropExchange* pPX, CFontHolder& font)
{
	USES_CONVERSION;

	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT_POINTER(&font, CFontHolder);

	if (pPX->IsAsynchronous())
		return TRUE;
	if (font.m_pFont != NULL)
	{
		CString strName;
		if (PX_String(pPX, _T("FontName"), strName))
		{
			BSTR bstrName;
			bstrName = SysAllocString(T2COLE(strName));
			font.m_pFont->put_Name(bstrName);
			SysFreeString(bstrName);
		}

		CY cySize;
		if (PX_Currency(pPX, _T("FontSize"), cySize))
			font.m_pFont->put_Size(cySize);

		BOOL bFlag;
		if (PX_Bool(pPX, _T("FontBold"), bFlag))
			font.m_pFont->put_Bold(bFlag);
		if (PX_Bool(pPX, _T("FontItalic"), bFlag))
			font.m_pFont->put_Italic(bFlag);
		if (PX_Bool(pPX, _T("FontUnderline"), bFlag))
			font.m_pFont->put_Underline(bFlag);
		if (PX_Bool(pPX, _T("FontStrikethru"), bFlag))
			font.m_pFont->put_Strikethrough(bFlag);
	}
	return TRUE;
}

AFX_STATIC BOOL AFXAPI _Afx_PX_DataPath(CPropExchange* pPX, LPCTSTR pszPropName,
	CDataPathProperty& dataPathProp, VARTYPE vtDefault, const void* pvDefault)
{
	ASSERT_POINTER(pPX, CPropExchange);
	ASSERT(AfxIsValidString(pszPropName));

	const BOOL bIsLoading = pPX->IsLoading();

	COleControl* pControl = dataPathProp.GetControl();
	if (!pControl)
		TRACE0("No control in PX_DataPath!");

	BOOL bHasClientSite = pControl ? NULL != pControl->GetClientSite() : FALSE;

	if (!pPX->IsAsynchronous())
	{
		CString strPath;
		if (!bIsLoading)
			strPath = dataPathProp.GetPath();
		if (((pvDefault != NULL) && !pPX->ExchangeProp(pszPropName, vtDefault, &strPath, pvDefault)) ||
			((pvDefault == NULL) && !pPX->ExchangeProp(pszPropName, VT_BSTR, &strPath)))
			return FALSE;
		if (bIsLoading)
			dataPathProp.SetPath(strPath);
		if (pControl && !bHasClientSite)
			pControl->RequestAsynchronousExchange(pPX->GetVersion());
	}

	if (pPX->IsAsynchronous() || bHasClientSite)
	{
#ifdef _DEBUG
		{
			if (!bHasClientSite)
				TRACE0("No client site in PX_DataPath!");
		}
#endif // _DEBUG

		if (bIsLoading)
			return dataPathProp.Open();
	}

	return TRUE;
}

BOOL AFX_CDECL PX_DataPath(CPropExchange* pPX, LPCTSTR pszPropName,
	CDataPathProperty& dataPathProp, LPCTSTR pszDefault)
{
	return _Afx_PX_DataPath(pPX, pszPropName, dataPathProp, VT_LPSTR, pszDefault);
}

BOOL AFX_CDECL PX_DataPath(CPropExchange* pPX, LPCTSTR pszPropName,
	CDataPathProperty& dataPathProp, const CString& strDefault)
{
	return _Afx_PX_DataPath(pPX, pszPropName, dataPathProp, VT_BSTR, &strDefault);
}

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif
