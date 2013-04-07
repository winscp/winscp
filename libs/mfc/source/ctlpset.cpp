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

#define OSTYPE 2    // Win32

AFX_STATIC_DATA LARGE_INTEGER _afxLargeZero = { 0,0 };

// Old class IDs for font and picture types
AFX_STATIC_DATA const CLSID _afx_CLSID_StdFont_V1 =
	{ 0xfb8f0823,0x0164,0x101b, { 0x84,0xed,0x08,0x00,0x2b,0x2e,0xc7,0x13 } };
AFX_STATIC_DATA const CLSID _afx_CLSID_StdPicture_V1 =
	{ 0xfb8f0824,0x0164,0x101b, { 0x84,0xed,0x08,0x00,0x2b,0x2e,0xc7,0x13 } };

LPSTREAM AFXAPI _AfxCreateMemoryStream()
{
	LPSTREAM lpStream = NULL;

	// Create a stream object on a memory block.
	HGLOBAL hGlobal = GlobalAlloc(GMEM_MOVEABLE|GMEM_SHARE, 0);
	if (hGlobal != NULL)
	{
		if (FAILED(CreateStreamOnHGlobal(hGlobal, TRUE, &lpStream)))
		{
			TRACE0("CreateStreamOnHGlobal failed.\n");
			GlobalFree(hGlobal);
			return NULL;
		}

		ASSERT_POINTER(lpStream, IStream);
	}
	else
	{
		TRACE0("Failed to allocate memory for stream.\n");
		return NULL;
	}

	return lpStream;
}

BOOL COleControl::GetPropsetData(LPFORMATETC lpFormatEtc,
		LPSTGMEDIUM lpStgMedium, REFCLSID fmtid)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));
	ASSERT(AfxIsValidAddress(lpStgMedium, sizeof(STGMEDIUM)));

	BOOL bGetDataHere = (lpStgMedium->tymed != TYMED_NULL);

	// Allow IStream or IStorage as the storage medium.

	if (!(lpFormatEtc->tymed & (TYMED_ISTREAM|TYMED_ISTORAGE)))
	{
		TRACE0("Propset only supported for stream or storage.\n");
		return FALSE;
	}

	LPSTORAGE lpStorage = NULL;
	LPSTREAM lpStream = NULL;

	if (lpFormatEtc->tymed & TYMED_ISTORAGE)
	{
		// Caller wants propset data in a storage object.

		if (bGetDataHere)
		{
			// Use the caller-supplied storage object.
			lpStorage = lpStgMedium->pstg;
		}
		else
		{
			// Create a storage object on a memory ILockBytes implementation.
			LPLOCKBYTES lpLockBytes = NULL;

			if (FAILED(CreateILockBytesOnHGlobal(NULL, TRUE, &lpLockBytes)))
			{
				TRACE0("CreateILockBytesOnHGlobal failed.\n");
				return FALSE;
			}

			ASSERT_POINTER(lpLockBytes, ILockBytes);

			if (FAILED(StgCreateDocfileOnILockBytes(lpLockBytes,
					STGM_SHARE_EXCLUSIVE|STGM_CREATE|STGM_READWRITE, 0,
					&lpStorage)))
			{
				TRACE0("StgCreateDocfileOnILockBytes failed.\n");
				lpLockBytes->Release();
				return FALSE;
			}

			// Docfile now has reference to ILockBytes, so release ours.
			lpLockBytes->Release();
		}

		ASSERT_POINTER(lpStorage, IStorage);

		// Create a stream within the storage.
		if (FAILED(lpStorage->CreateStream(OLESTR("Contents"),
				STGM_SHARE_EXCLUSIVE|STGM_CREATE|STGM_READWRITE, 0, 0,
				&lpStream)))
		{
			TRACE0("IStorage::CreateStream failed.\n");
			if (!bGetDataHere)
				lpStorage->Release();
			return FALSE;
		}
	}
	else
	{
		// Caller wants propset data in a stream object.

		if (bGetDataHere)
		{
			// Use the caller-supplied stream object
			lpStream = lpStgMedium->pstm;
		}
		else
		{
			lpStream = _AfxCreateMemoryStream();
			if (lpStream == NULL)
				return FALSE;
		}
	}

	ASSERT_POINTER(lpStream, IStream);

	// Create the property set.

	CLSID clsid;
	GetClassID(&clsid);
	CPropertySet pset(clsid);
	pset.SetOSVersion(MAKELONG(LOWORD(GetVersion()), OSTYPE));
	CPropertySection* ppsec = pset.AddSection(fmtid);
	if (ppsec == NULL)
	{
		TRACE0("CPropertySet::AddSection failed.\n");
		lpStream->Release();
		lpStorage->Release();
		return FALSE;
	}

	// Set the name, based on the ambient display name (from the container).
	ppsec->SetSectionName(AmbientDisplayName());

	CPropsetPropExchange propx(*ppsec, lpStorage, FALSE);

	BOOL bPropExchange = FALSE;
	TRY
	{
		DoPropExchange(&propx);
		bPropExchange = TRUE;
	}
	END_TRY

	if (!bPropExchange)
	{
		TRACE0("DoPropExchange failed.\n");
		lpStream->Release();
		lpStorage->Release();
		return FALSE;
	}

	// Store the property set in the stream.

	if (FAILED(pset.WriteToStream(lpStream)))
	{
		TRACE0("CPropertySet::WriteToStream failed.\n");
		lpStream->Release();
		lpStorage->Release();
		return FALSE;
	}

	// Return the property set in the requested medium.

	if (lpFormatEtc->tymed & TYMED_ISTORAGE)
	{
		// Return as a storage object.

		ASSERT_POINTER(lpStorage, IStorage);
		lpStream->Release();
		lpStgMedium->pstg = lpStorage;
		lpStgMedium->tymed = TYMED_ISTORAGE;
		lpStgMedium->pUnkForRelease = NULL;
	}
	else
	{
		// Return as a stream.

		ASSERT_POINTER(lpStream, IStream);
		lpStgMedium->pstm = lpStream;
		lpStgMedium->tymed = TYMED_ISTREAM;
		lpStgMedium->pUnkForRelease = NULL;
	}

	return TRUE;
}

BOOL COleControl::SetPropsetData(LPFORMATETC lpFormatEtc,
		LPSTGMEDIUM lpStgMedium, REFCLSID fmtid)
{
	UNUSED(lpFormatEtc); // unused in release builds

	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));
	ASSERT(AfxIsValidAddress(lpStgMedium, sizeof(STGMEDIUM)));

	// Get the stream that contains the property set.

	LPSTORAGE lpStorage = NULL;
	LPSTREAM lpStream = NULL;

	switch (lpStgMedium->tymed)
	{
	case TYMED_ISTORAGE:
		{
			lpStorage = lpStgMedium->pstg;
			ASSERT_POINTER(lpStorage, IStorage);
			if (FAILED(lpStorage->OpenStream(OLESTR("Contents"), 0,
					STGM_SHARE_EXCLUSIVE|STGM_READ, 0, &lpStream)))
			{
				TRACE0("Failed to open content stream.\n");
				return FALSE;
			}
		}
		break;

	case TYMED_ISTREAM:
		lpStorage = NULL;
		lpStream = lpStgMedium->pstm;
		break;

	default:
		TRACE0("Propset only supported for stream or storage.\n");
		return FALSE;
	}

	ASSERT_POINTER(lpStream, IStream);

	// Read the property set from the stream.

	CPropertySet pset;
	if (!pset.ReadFromStream(lpStream))
	{
		TRACE0("CPropertySet::ReadFromStream failed.\n");
		return FALSE;
	}

	CPropertySection* ppsec = pset.GetSection(fmtid);
	if (ppsec == NULL)
	{
		TRACE0("CLSID_PersistPropset section not found in property set.\n");
		return FALSE;
	}

	// Detect whether we're converting a VBX
	m_bConvertVBX = (BYTE)IsEqualGUID(fmtid, CLSID_ConvertVBX);

	// Parse the property set.

	CPropsetPropExchange propx(*ppsec, lpStorage, TRUE);

	BOOL bPropExchange = FALSE;
	TRY
	{
		DoPropExchange(&propx);
		bPropExchange = TRUE;
	}
	END_TRY

	// Properties have probably changed
	BoundPropertyChanged(DISPID_UNKNOWN);
	InvalidateControl();

	m_bConvertVBX = FALSE;

	// Clear the modified flag.
	m_bModified = FALSE;

	// Unless IOleObject::SetClientSite is called after this, we can
	// count on ambient properties being available while loading.
	m_bCountOnAmbients = TRUE;

	// Properties have been initialized
	m_bInitialized = TRUE;

	// Cleanup.
	if (lpStorage != NULL)      // If we called OpenStream(), release now.
		lpStream->Release();

	BoundPropertyChanged(DISPID_UNKNOWN);
	return bPropExchange;
}

CPropsetPropExchange::CPropsetPropExchange(CPropertySection& psec,
		LPSTORAGE lpStorage, BOOL bLoading) :
	m_psec(psec),
	m_lpStorage(lpStorage),
	m_dwPropID(255)
{
	ASSERT_POINTER(&psec, CPropertySection);
	ASSERT_NULL_OR_POINTER(lpStorage, IStorage);

	m_bLoading = bLoading;
}

AFX_STATIC size_t AFXAPI _AfxGetSizeOfVarType(VARTYPE vt)
{
	switch (vt)
	{
	case VT_I2:
	case VT_BOOL:
		return 2;

	case VT_I4:
	case VT_R4:
		return 4;

	case VT_R8:
		return 8;

	case VT_CY:
		return sizeof(CURRENCY);

	case VT_BSTR:
		return sizeof(BSTR);
	}

	return 0;
}

BOOL AFXAPI _AfxCoerceNumber(void* pvDst, VARTYPE vtDst, void* pvSrc, VARTYPE vtSrc)
{
	// Check size of source.
	size_t cbSrc = _AfxGetSizeOfVarType(vtSrc);
	if (cbSrc == 0)
		return FALSE;

	// If source and destination are same type, just copy.
	if (vtSrc == vtDst)
	{
		memcpy(pvDst, pvSrc, cbSrc);
		return TRUE;
	}

	// Check size of destination.
	size_t cbDst = _AfxGetSizeOfVarType(vtDst);

	if (cbDst == 0)
		return FALSE;

	// Initialize variant for coercion.
	VARIANTARG var;
	V_VT(&var) = vtSrc;
	memcpy((void*)&V_NONE(&var), pvSrc, cbSrc);

	// Do the coercion.
	if (FAILED(VariantChangeType(&var, &var, 0, vtDst)))
		return FALSE;

	// Copy result to destination.
	memcpy(pvDst, (void*)&V_NONE(&var), cbDst);
	return TRUE;
}

BOOL AFXAPI _AfxIsSamePropValue(VARTYPE vtProp, const void* pv1, const void* pv2)
{
	if (pv1 == pv2)
		return TRUE;

	if ((pv1 == NULL) || (pv2 == NULL))
		return FALSE;

	BOOL bSame = FALSE;

	switch (vtProp)
	{
	case VT_BSTR:
		bSame = ((CString*)pv1)->Compare(*(CString*)pv2) == 0;
		break;
	case VT_LPSTR:
		bSame = ((CString*)pv1)->Compare((LPCTSTR)pv2) == 0;
		break;

	case VT_BOOL:
	case VT_I2:
	case VT_I4:
	case VT_CY:
	case VT_R4:
	case VT_R8:
		bSame = memcmp(pv1, pv2, _AfxGetSizeOfVarType(vtProp)) == 0;
		break;
	}

	return bSame;
}

BOOL CPropsetPropExchange::ExchangeProp(LPCTSTR pszPropName, VARTYPE vtProp,
	void* pvProp, const void* pvDefault)
{
	USES_CONVERSION;

	ASSERT(AfxIsValidString(pszPropName));
	ASSERT(AfxIsValidAddress(pvProp, 1, FALSE));
	ASSERT((pvDefault == NULL) || AfxIsValidAddress(pvDefault, 1, FALSE));

	BOOL bSuccess = FALSE;

	if (m_bLoading)
	{
		DWORD dwPropID;
		LPVOID pvData;
		CProperty* pprop;

		if (m_psec.GetID(pszPropName, &dwPropID) &&
			((pprop = m_psec.GetProperty(dwPropID)) != NULL) &&
			((pvData = pprop->Get()) != NULL))
		{
			VARTYPE vtData = (VARTYPE)pprop->GetType();

			CString strTmp;

#ifdef _UNICODE
			// Unicode is "native" format
			if ((vtData == VT_BSTR) || (vtData == VT_LPWSTR))
#else
			// ANSI is "native" format
			if ((vtData == VT_BSTR) || (vtData == VT_LPSTR))
#endif
			{
				strTmp = (LPCTSTR)pvData;
			}

#ifdef _UNICODE
			else if (vtData == VT_LPSTR)
			{
				// Convert from ANSI to Unicode
				strTmp = (LPCSTR)pvData;
			}
#else
			else if (vtData == VT_LPWSTR)
			{
				// Convert from Unicode to ANSI
				strTmp = (LPCWSTR)pvData;
			}
#endif

			switch (vtProp)
			{
			case VT_LPSTR:
			case VT_BSTR:
				bSuccess = _AfxCopyPropValue(VT_BSTR, pvProp, &strTmp);
				break;

			case VT_BOOL:
				{
					short sProp;
					BSTR bstrTmp = NULL;

					if ((vtData == VT_BSTR) || (vtData == VT_LPSTR) ||
						(vtData == VT_LPWSTR))
					{
						bstrTmp = SysAllocString(T2COLE(strTmp));
						pvData = &bstrTmp;
						vtData = VT_BSTR;
					}

					bSuccess = _AfxCoerceNumber(&sProp, VT_BOOL, pvData,
						vtData);

					if (bstrTmp != NULL)
						SysFreeString(bstrTmp);

					if (bSuccess)
					{
						ASSERT((sProp == -1) || (sProp == 0));
						*(BOOL*)pvProp = !!sProp;
					}
				}
				break;

			case VT_I2:
			case VT_I4:
			case VT_CY:
			case VT_R4:
			case VT_R8:
				bSuccess = _AfxCoerceNumber(pvProp, vtProp, pvData, vtData);
				break;
			}
		}
		else
		{
			bSuccess = _AfxCopyPropValue(vtProp, pvProp, pvDefault);
		}
	}
	else
	{
		if (!_AfxIsSamePropValue(vtProp, pvProp, pvDefault))
		{
			++m_dwPropID;

			LPVOID pvData = NULL;
			BOOL bData;

			switch (vtProp)
			{
			case VT_LPSTR:
			case VT_BSTR:
				pvData = (LPVOID)(LPCTSTR)*(CString*)pvProp;
				break;

			case VT_BOOL:
				// Convert boolean value to -1 or 0.
				bData = (*(BOOL*)pvProp) ? -1 : 0;
				pvData = &bData;
				break;

			case VT_I2:
			case VT_I4:
			case VT_CY:
			case VT_R4:
			case VT_R8:
				pvData = pvProp;
				break;
			}

			bSuccess = m_psec.SetName(m_dwPropID, pszPropName) &&
				m_psec.Set(m_dwPropID, pvData, vtProp);
		}
		else
		{
			bSuccess = TRUE;
		}
	}

	return bSuccess;
}

BOOL CPropsetPropExchange::ExchangeBlobProp(LPCTSTR pszPropName,
	HGLOBAL* phBlob, HGLOBAL hBlobDefault)
{
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(phBlob, HGLOBAL);

	BOOL bSuccess = FALSE;
	ULONG cb = 0;
	void* pvBlob = NULL;

	if (m_bLoading)
	{
		if (*phBlob != NULL)
		{
			GlobalFree(*phBlob);
			*phBlob = NULL;
		}

		DWORD dwPropID;
		LPVOID pvData;

		if (m_psec.GetID(pszPropName, &dwPropID) &&
			((pvData = m_psec.Get(dwPropID)) != NULL))
		{
			// Copy count and blob data

			cb = *(ULONG*)pvData;

			if (cb > 0)
			{
				bSuccess = _AfxInitBlob(phBlob, pvData);
			}
			else
			{
				bSuccess = (cb == 0);
			}
		}

		if (!bSuccess)
		{
			// Failed.  Use default values.

			if (hBlobDefault != NULL)
				_AfxCopyBlob(phBlob, hBlobDefault);

			bSuccess = TRUE;
		}
	}
	else
	{
		++m_dwPropID;

		pvBlob = NULL;
		if (*phBlob != NULL)
			pvBlob = GlobalLock(*phBlob);

		ULONG lZero = 0;
		void* pvBlobSave = (pvBlob != NULL) ? pvBlob : &lZero;

		bSuccess = m_psec.SetName(m_dwPropID, pszPropName) &&
			m_psec.Set(m_dwPropID, pvBlobSave, VT_BLOB);

		if ((*phBlob != NULL) && (pvBlob != NULL))
			GlobalUnlock(*phBlob);
	}

	return bSuccess;
}

BOOL AFXAPI _AfxSaveStreamDataAsBlobProp(LPSTREAM pstm, CPropertySection& psec,
		DWORD dwPropID, DWORD dwType)
{
	BOOL bSuccess = FALSE;
	ULARGE_INTEGER uliStart;
	ULARGE_INTEGER uliEnd;

	// Note:  Stream length must fit in a DWORD.

	if (SUCCEEDED(pstm->Seek(_afxLargeZero, STREAM_SEEK_CUR, &uliStart)) &&
		SUCCEEDED(pstm->Seek(_afxLargeZero, STREAM_SEEK_END, &uliEnd)) &&
		SUCCEEDED(pstm->Seek(*(LARGE_INTEGER*)&uliStart, STREAM_SEEK_SET,
			NULL)))
	{
		DWORD cb = uliEnd.LowPart - uliStart.LowPart;
		HGLOBAL hGlobal = GlobalAlloc(GMEM_MOVEABLE|GMEM_SHARE,
			cb + (DWORD)sizeof(cb));

		if (hGlobal != NULL)
		{
			LPBYTE pbData = (LPBYTE)GlobalLock(hGlobal);
			if (pbData != NULL)
			{
				*(DWORD*)pbData = cb;
				if (SUCCEEDED(pstm->Read(pbData + (DWORD)sizeof(DWORD), cb,
					NULL)))
				{
					bSuccess = psec.Set(dwPropID, pbData, dwType);
				}
				GlobalUnlock(hGlobal);
			}
			GlobalFree(hGlobal);
		}
	}

	return bSuccess;
}

BOOL AFXAPI _AfxInitStreamDataFromBlobProp(LPSTREAM pstm, CProperty* pprop)
{
	BOOL bSuccess = FALSE;
	ULONG cb;
	BYTE* pbData = (BYTE*)(pprop->Get(&cb));

	if (pbData != NULL)
	{
		// Put the data into the stream, then seek back to start of data.

		LARGE_INTEGER liOffset;
		liOffset.LowPart = -(LONG)cb;
		liOffset.HighPart = -1;
		if (SUCCEEDED(pstm->Write(pbData + sizeof(ULONG), cb, NULL)) &&
			SUCCEEDED(pstm->Seek(liOffset, STREAM_SEEK_CUR, NULL)))
		{
			bSuccess = TRUE;
		}
	}

	return bSuccess;
}

AFX_STATIC_DATA const FONTDESC _afxFontDescHelv =
	{ sizeof(FONTDESC), OLESTR("Helv"), FONTSIZE(12), FW_NORMAL,
	  DEFAULT_CHARSET, FALSE, FALSE, FALSE };

LPFONT AFXAPI _AfxCreateFontFromStream(LPSTREAM pstm)
{
	BOOL bSuccess = FALSE;
	LPFONT pFont = NULL;
	LPPERSISTSTREAM pPersStm = NULL;
	CLSID clsid;

	if (SUCCEEDED(pstm->Read(&clsid, sizeof(CLSID), NULL)))
	{
		HRESULT hr;

		if (IsEqualCLSID(clsid, CLSID_StdFont) ||
			IsEqualCLSID(clsid, _afx_CLSID_StdFont_V1))
		{
			// We know this kind of font; create it using the API.
			hr = ::OleCreateFontIndirect((LPFONTDESC)&_afxFontDescHelv, IID_IFont,
					(LPVOID*)&pFont);
		}
		else
		{
			// Some other implementation of IFont.
			hr = CoCreateInstance(clsid, NULL, CLSCTX_INPROC_SERVER, IID_IFont,
					(LPVOID*)&pFont);
		}

		if (SUCCEEDED(hr))
		{
			// Successfully created font, now get its IPersistStream interface.

			ASSERT_POINTER(pFont, IFont);

			if (SUCCEEDED(pFont->QueryInterface(IID_IPersistStream,
					(LPVOID*)&pPersStm)))
			{
				ASSERT_POINTER(pPersStm, IPersistStream);
			}
		}

		if (pPersStm != NULL)
		{
			// Load the font.

			ASSERT_POINTER(pFont, IFont);
			bSuccess = SUCCEEDED(pPersStm->Load(pstm));
			pPersStm->Release();
		}
	}

	// If we failed for any reason, clean up the font.
	if (!bSuccess && pFont != NULL)
	{
		pFont->Release();
		pFont = NULL;
	}

	return pFont;
}

BOOL AFXAPI _AfxLoadObjectFromStreamedPropset(LPUNKNOWN lpUnknown, LPSTREAM lpStream)
{
	ASSERT_POINTER(lpUnknown, IUnknown);
	ASSERT_POINTER(lpStream, IStream);

	BOOL bSuccess = FALSE;
	LPDATAOBJECT pDataObj = NULL;

	if (SUCCEEDED(lpUnknown->QueryInterface(IID_IDataObject,
			(LPVOID*)&pDataObj)))
	{
		ASSERT_POINTER(pDataObj, IDataObject);

		// Set the persistent propset format on the object.

		FORMATETC formatEtc;
		STGMEDIUM stgMedium;
		formatEtc.cfFormat = _AfxGetClipboardFormatPersistPropset();
		formatEtc.ptd = NULL;
		formatEtc.dwAspect = DVASPECT_CONTENT;
		formatEtc.lindex = -1;
		formatEtc.tymed = TYMED_ISTREAM;

		stgMedium.tymed = TYMED_ISTREAM;
		stgMedium.pstm = lpStream;
		stgMedium.pUnkForRelease = NULL;

		bSuccess = SUCCEEDED(pDataObj->SetData(&formatEtc, &stgMedium, FALSE));

		pDataObj->Release();
	}

	return bSuccess;
}

BOOL AFXAPI _AfxGetClassIDFromStreamedPropset(LPCLSID lpClsid, LPSTREAM lpStream)
{
	BOOL bSuccess = FALSE;
	ULARGE_INTEGER uliSave;
	LARGE_INTEGER liClsidOffset;
	LISet32(liClsidOffset, 8);

	if (SUCCEEDED(lpStream->Seek(_afxLargeZero, STREAM_SEEK_CUR, &uliSave)))
	{
		if (SUCCEEDED(lpStream->Seek(liClsidOffset, STREAM_SEEK_CUR, NULL)) &&
			SUCCEEDED(lpStream->Read(lpClsid, sizeof(CLSID), NULL)))
		{
			bSuccess = TRUE;
		}

		lpStream->Seek(*(LARGE_INTEGER*)&uliSave, STREAM_SEEK_SET, NULL);
	}

	return bSuccess;
}

LPUNKNOWN AFXAPI _AfxCreateObjectFromStreamedPropset(LPSTREAM lpStream, REFGUID iid)
{
	LPUNKNOWN pUnk = NULL;
	CLSID clsid;

	if (_AfxGetClassIDFromStreamedPropset(&clsid, lpStream))
	{
		// Special case: we know how to create font objects
		if (IsEqualCLSID(clsid, CLSID_StdFont) ||
			IsEqualCLSID(clsid, _afx_CLSID_StdFont_V1))
		{
			if (FAILED(::OleCreateFontIndirect((LPFONTDESC)&_afxFontDescHelv, iid,
					(LPVOID*)&pUnk)))
			{
				pUnk = NULL;
			}
		}
		// Special case: we know how to create picture objects
		else if (IsEqualCLSID(clsid, CLSID_StdPicture) ||
			IsEqualCLSID(clsid, _afx_CLSID_StdPicture_V1))
		{
			if (FAILED(::OleCreatePictureIndirect(NULL, iid, FALSE,
					(LPVOID*)&pUnk)))
			{
				pUnk = NULL;
			}
		}
		// General case: create the object
		else if (FAILED(CoCreateInstance(clsid, NULL,
					CLSCTX_INPROC_SERVER, iid, (LPVOID*)&pUnk)))
		{
			pUnk = NULL;
		}

		if (pUnk != NULL)
		{
			if (!_AfxLoadObjectFromStreamedPropset(pUnk, lpStream))
			{
				RELEASE(pUnk);
				pUnk = NULL;
			}
		}
	}

	return pUnk;
}

LPSTREAM AFXAPI _AfxLoadStreamFromPropset(CPropertySection& psec, LPCTSTR pszPropName,
	DWORD& vtType)
{
	ASSERT(AfxIsValidString(pszPropName));

	vtType = VT_EMPTY;

	DWORD dwPropID;
	CProperty* pprop = NULL;
	LPSTREAM pstm = NULL;

	if (psec.GetID(pszPropName, &dwPropID) &&
		((pprop = psec.GetProperty(dwPropID)) != NULL))
	{
		vtType = pprop->GetType();

		if ((vtType == VT_BLOB) || (vtType == VT_BLOB_PROPSET))
		{
			pstm = _AfxCreateMemoryStream();

			if (pstm != NULL)
			{
				if (!_AfxInitStreamDataFromBlobProp(pstm, pprop))
				{
					pstm->Release();
					pstm = NULL;
				}
			}
		}
	}

	return pstm;
}

BOOL AFXAPI _AfxSaveObjectInPropset(LPUNKNOWN pUnk, CPropertySection& psec,
	DWORD dwPropID)
{
	if (pUnk == NULL)
		return FALSE;

	ASSERT_POINTER(pUnk, IUnknown);

	BOOL bSuccess = FALSE;
	LPDATAOBJECT pDataObj;

	if (SUCCEEDED(pUnk->QueryInterface(IID_IDataObject,
			(LPVOID*)&pDataObj)))
	{
		// Get the persistent propset format from object.

		FORMATETC formatEtc;
		STGMEDIUM stgMedium;
		formatEtc.cfFormat = _AfxGetClipboardFormatPersistPropset();
		formatEtc.ptd = NULL;
		formatEtc.dwAspect = DVASPECT_CONTENT;
		formatEtc.lindex = -1;
		formatEtc.tymed = TYMED_ISTREAM;

		stgMedium.tymed = TYMED_NULL;
		stgMedium.pUnkForRelease = NULL;

		if (SUCCEEDED(pDataObj->GetData(&formatEtc, &stgMedium)))
		{
			if (stgMedium.tymed == TYMED_ISTREAM)
			{
				LPSTREAM pstm = stgMedium.pstm;

				// Seek to start of stream.
				if (SUCCEEDED(pstm->Seek(_afxLargeZero, STREAM_SEEK_SET, NULL)))
				{
					// Create a "blobbed" propset from the stream
					bSuccess = _AfxSaveStreamDataAsBlobProp(stgMedium.pstm,
						psec, dwPropID, VT_BLOB_PROPSET);
				}
			}

			// Cleanup
			ReleaseStgMedium(&stgMedium);
		}

		pDataObj->Release();
	}

	LPPERSISTSTREAM pPersStm = NULL;

	if ((!bSuccess) &&
		SUCCEEDED(pUnk->QueryInterface(IID_IPersistStream,
			(LPVOID*)&pPersStm)))
	{
		// Get the object to save itself into a stream, then store that
		// streamed data as a blob.

		ASSERT_POINTER(pPersStm, IPersistStream);
		LPSTREAM pstm = _AfxCreateMemoryStream();
		if (pstm != NULL)
		{
			if (SUCCEEDED(::OleSaveToStream(pPersStm, pstm)) &&
				SUCCEEDED(pstm->Seek(_afxLargeZero, STREAM_SEEK_SET, NULL)))
			{
				bSuccess = _AfxSaveStreamDataAsBlobProp(pstm, psec,
					dwPropID, VT_BLOB);
			}

			pstm->Release();
		}

		pPersStm->Release();
	}

	return bSuccess;
}

BOOL CPropsetPropExchange::ExchangePersistentProp(LPCTSTR pszPropName,
		LPUNKNOWN* ppUnk, REFIID iid, LPUNKNOWN pUnkDefault)
{
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(ppUnk, LPUNKNOWN);
	ASSERT_NULL_OR_POINTER(pUnkDefault, IUnknown);

	BOOL bSuccess = FALSE;

	if (m_bLoading)
	{
		RELEASE(*ppUnk);
		*ppUnk = NULL;

		DWORD vtType;
		LPSTREAM pstm = _AfxLoadStreamFromPropset(m_psec, pszPropName, vtType);

		if (pstm != NULL)
		{
			CLSID clsid;

			switch(vtType)
			{
			case VT_BLOB:
				if (_AfxPeekAtClassIDInStream(pstm, &clsid))
				{
					if (IsEqualCLSID(clsid, CLSID_StdPicture) ||
						IsEqualCLSID(clsid, _afx_CLSID_StdPicture_V1))
					{
						// Special case: load the picture directly.
						bSuccess = SUCCEEDED(::ReadClassStm(pstm, &clsid)) &&
							SUCCEEDED(::OleLoadPicture(pstm, 0, FALSE, iid,
							(LPVOID*)ppUnk));
					}
					else
					{
						// Load the object.
						bSuccess = SUCCEEDED(::OleLoadFromStream(pstm, iid,
							(LPVOID*)ppUnk));
					}
				}
				break;

			case VT_BLOB_PROPSET:
				*ppUnk = _AfxCreateObjectFromStreamedPropset(pstm, iid);
				break;

			default:
				break;
			}

			pstm->Release();
		}

		if (!bSuccess && (pUnkDefault != NULL))
		{
			bSuccess = SUCCEEDED(pUnkDefault->QueryInterface(iid,
				(LPVOID*)ppUnk));
		}
	}
	else
	{
		if ((*ppUnk == NULL) ||
			_AfxIsSameUnknownObject(iid, *ppUnk, pUnkDefault))
		{
			bSuccess = TRUE;
		}
		else
		{
			++m_dwPropID;

			bSuccess = m_psec.SetName(m_dwPropID, pszPropName) &&
				_AfxSaveObjectInPropset(*ppUnk, m_psec, m_dwPropID);
		}
	}

	return bSuccess;
}

BOOL CPropsetPropExchange::ExchangeFontProp(LPCTSTR pszPropName,
		CFontHolder& font, const FONTDESC* pFontDesc,
		LPFONTDISP pFontDispAmbient)
{
	ASSERT(AfxIsValidString(pszPropName));
	ASSERT_POINTER(&font, CFontHolder);
	ASSERT_NULL_OR_POINTER(pFontDesc, FONTDESC);
	ASSERT_NULL_OR_POINTER(pFontDispAmbient, IFontDisp);

	BOOL bSuccess = FALSE;

	if (m_bLoading)
	{
		DWORD vtType;
		LPSTREAM pstm = _AfxLoadStreamFromPropset(m_psec, pszPropName, vtType);

		if (pstm != NULL)
		{
			LPFONT pFont;

			switch(vtType)
			{
			case VT_BLOB:
				pFont = _AfxCreateFontFromStream(pstm);
				break;

			case VT_BLOB_PROPSET:
				pFont = (LPFONT)_AfxCreateObjectFromStreamedPropset(pstm,
					IID_IFont);
				break;

			default:
				pFont = NULL;
			}

			if (pFont != NULL)
			{
				font.SetFont(pFont);
				bSuccess = TRUE;
			}

			pstm->Release();
		}

		if (!bSuccess)
		{
			// Initialize font to its default state
			font.InitializeFont(pFontDesc, pFontDispAmbient);
		}
	}
	else
	{
		if ((font.m_pFont == NULL) ||
			_AfxIsSameFont(font, pFontDesc, pFontDispAmbient))
		{
			bSuccess = TRUE;
		}
		else
		{
			++m_dwPropID;

			bSuccess = m_psec.SetName(m_dwPropID, pszPropName) &&
				_AfxSaveObjectInPropset(font.m_pFont, m_psec, m_dwPropID);
		}
	}

	return bSuccess;
}

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif
