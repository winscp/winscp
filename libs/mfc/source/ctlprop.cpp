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

#ifndef _DEBUG
#pragma intrinsic(memset)
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// Stock property mask

#define STOCKPROP_BACKCOLOR     0x00000001
#define STOCKPROP_CAPTION       0x00000002
#define STOCKPROP_FONT          0x00000004
#define STOCKPROP_FORECOLOR     0x00000008
#define STOCKPROP_TEXT          0x00000010
#define STOCKPROP_BORDERSTYLE   0x00000020
#define STOCKPROP_ENABLED       0x00000040
#define STOCKPROP_APPEARANCE    0x00000080

AFX_STATIC_DATA const DWORD _afxStockProps[] =
{
	STOCKPROP_BACKCOLOR,    // -501
	0,                      // -502
	0,                      // -503
	STOCKPROP_BORDERSTYLE,  // -504
	0,                      // -505
	0,                      // -506
	0,                      // -507
	0,                      // -508
	0,                      // -509
	0,                      // -510
	0,                      // -511
	STOCKPROP_FONT,         // -512
	STOCKPROP_FORECOLOR,    // -513
	STOCKPROP_ENABLED,      // -514
	0,                      // -515
	0,                      // -516
	STOCKPROP_TEXT,         // -517
	STOCKPROP_CAPTION,      // -518
	0,                      // -519
	STOCKPROP_APPEARANCE,   // -520
};

void COleControl::InitStockPropMask()
{
	const AFX_DISPMAP* pDispMap = GetDispatchMap();
	const AFX_DISPMAP_ENTRY* pEntry;
	ASSERT(pDispMap != NULL);

	// If stock property mask is already initialized, we're outta here.
	if (*pDispMap->lpStockPropMask != (DWORD)-1)
		return;

	AfxLockGlobals(CRIT_STOCKMASK);

	if (*pDispMap->lpStockPropMask == (DWORD)-1)
	{
		const AFX_DISPMAP* pDispMapTop = pDispMap;
		DWORD dwStockPropMask = 0;

		while (pDispMap != NULL)
		{
			pEntry = pDispMap->lpEntries;
			while (pEntry->nPropOffset != -1)
			{
				int nIndex = DISPID_BACKCOLOR - pEntry->lDispID;
				DWORD dwFlag;
				if (nIndex >= 0 && nIndex < _countof(_afxStockProps) &&
					(dwFlag = _afxStockProps[nIndex]) != 0)
				{
					dwStockPropMask |= dwFlag;
				}

				++pEntry;
			}
			// check base class
#ifdef _AFXDLL
			pDispMap = (*pDispMap->pfnGetBaseMap)();
#else
			pDispMap = pDispMap->pBaseMap;
#endif
		}

		*pDispMapTop->lpStockPropMask = dwStockPropMask;
	}

	AfxUnlockGlobals(CRIT_STOCKMASK);
}

AFX_STATIC void AFXAPI _AfxToggleBorderStyle(CWnd* pWnd)
{
	if (pWnd->m_hWnd != NULL)
	{
		// toggle border style and force redraw of border
		::SetWindowLong(pWnd->m_hWnd, GWL_STYLE, pWnd->GetStyle() ^ WS_BORDER);
		::SetWindowPos(pWnd->m_hWnd, NULL, 0, 0, 0, 0,
			SWP_DRAWFRAME | SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);
	}
}

AFX_STATIC void AFXAPI _AfxToggleAppearance(CWnd* pWnd)
{
	if (pWnd->m_hWnd != NULL)
	{
		// toggle border style and force redraw of border
		::SetWindowLong(pWnd->m_hWnd, GWL_EXSTYLE, pWnd->GetExStyle() ^
			WS_EX_CLIENTEDGE);
		::SetWindowPos(pWnd->m_hWnd, NULL, 0, 0, 0, 0,
			SWP_DRAWFRAME | SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);
	}
}

/////////////////////////////////////////////////////////////////////////////
// Property exchange for stock properties

void COleControl::ExchangeStockProps(CPropExchange* pPX)
{
	BOOL bLoading = pPX->IsLoading();
	DWORD dwStockPropMask = GetStockPropMask();
	DWORD dwPersistMask = dwStockPropMask;

	PX_ULong(pPX, _T("_StockProps"), dwPersistMask);

	if (dwStockPropMask & (STOCKPROP_CAPTION | STOCKPROP_TEXT))
	{
		CString strText;

		if (dwPersistMask & (STOCKPROP_CAPTION | STOCKPROP_TEXT))
		{
			if (!bLoading)
				strText = InternalGetText();
			if (dwStockPropMask & STOCKPROP_CAPTION)
				PX_String(pPX, _T("Caption"), strText, _T(""));
			if (dwStockPropMask & STOCKPROP_TEXT)
				PX_String(pPX, _T("Text"), strText, _T(""));
		}
		if (bLoading)
		{
			TRY
				SetText(strText);
			END_TRY
		}
	}

	if (dwStockPropMask & STOCKPROP_FORECOLOR)
	{
		if (dwPersistMask & STOCKPROP_FORECOLOR)
			PX_Color(pPX, _T("ForeColor"), m_clrForeColor, AmbientForeColor());
		else if (bLoading)
			m_clrForeColor = AmbientForeColor();
	}

	if (dwStockPropMask & STOCKPROP_BACKCOLOR)
	{
		if (dwPersistMask & STOCKPROP_BACKCOLOR)
			PX_Color(pPX, _T("BackColor"), m_clrBackColor, AmbientBackColor());
		else if (bLoading)
			m_clrBackColor = AmbientBackColor();
	}

	if (dwStockPropMask & STOCKPROP_FONT)
	{
		LPFONTDISP pFontDispAmbient = AmbientFont();
		BOOL bChanged = TRUE;

		if (dwPersistMask & STOCKPROP_FONT)
			bChanged = PX_Font(pPX, _T("Font"), m_font, NULL, pFontDispAmbient);
		else if (bLoading)
			m_font.InitializeFont(NULL, pFontDispAmbient);

		if (bLoading && bChanged)
			OnFontChanged();

		RELEASE(pFontDispAmbient);
	}

	if (dwStockPropMask & STOCKPROP_BORDERSTYLE)
	{
		short sBorderStyle = m_sBorderStyle;

		if (dwPersistMask & STOCKPROP_BORDERSTYLE)
			PX_Short(pPX, _T("BorderStyle"), m_sBorderStyle, 0);
		else if (bLoading)
			m_sBorderStyle = 0;

		if (sBorderStyle != m_sBorderStyle)
			_AfxToggleBorderStyle(this);
	}

	if (dwStockPropMask & STOCKPROP_ENABLED)
	{
		BOOL bEnabled = m_bEnabled;

		if (dwPersistMask & STOCKPROP_ENABLED)
			PX_Bool(pPX, _T("Enabled"), m_bEnabled, TRUE);
		else if (bLoading)
			m_bEnabled = TRUE;

		if ((bEnabled != m_bEnabled) && (m_hWnd != NULL))
			::EnableWindow(m_hWnd, m_bEnabled);
	}

	if (dwStockPropMask & STOCKPROP_APPEARANCE)
	{
		short sAppearance = m_sAppearance;

		if (dwPersistMask & STOCKPROP_APPEARANCE)
			PX_Short(pPX, _T("Appearance"), m_sAppearance, 0);
		else if (bLoading)
			m_sAppearance = AmbientAppearance();

		if (sAppearance != m_sAppearance)
			_AfxToggleAppearance(this);
	}
}

/////////////////////////////////////////////////////////////////////////////
// Serialization for stock properties

void COleControl::SerializeStockProps(CArchive& ar)
{
	BOOL bLoading = ar.IsLoading();
	DWORD dwStockPropMask = GetStockPropMask();
	DWORD dwPersistMask = dwStockPropMask;

	if (bLoading)
	{
		ar >> dwPersistMask;

		if (dwStockPropMask & (STOCKPROP_CAPTION | STOCKPROP_TEXT))
		{
			if (dwPersistMask & (STOCKPROP_CAPTION | STOCKPROP_TEXT))
			{
				CString strText;
				ar >> strText;
				TRY
					SetText(strText);
				END_TRY
			}
			else
			{
				TRY
					SetText(_T(""));
				END_TRY
			}
		}

		if (dwStockPropMask & STOCKPROP_FORECOLOR)
		{
			if (dwPersistMask & STOCKPROP_FORECOLOR)
				ar >> m_clrForeColor;
			else
				m_clrForeColor = AmbientForeColor();
		}

		if (dwStockPropMask & STOCKPROP_BACKCOLOR)
		{
			if (dwPersistMask & STOCKPROP_BACKCOLOR)
				ar >> m_clrBackColor;
			else
				m_clrBackColor = AmbientBackColor();
		}

		if (dwStockPropMask & STOCKPROP_FONT)
		{
			BOOL bRead = FALSE;

			if (dwPersistMask & STOCKPROP_FONT)
			{
				BYTE bFlag;
				ar >> bFlag;
				if (bFlag != 0xFF)
				{
					CArchiveStream stm(&ar);
					LPSTREAM pstm = _AfxGetArchiveStream(ar, stm);
					LPFONT pFont = _AfxCreateFontFromStream(pstm);
					if (pFont != NULL)
					{
						m_font.SetFont(pFont);
						bRead = TRUE;
					}
				}
			}

			if (! bRead)
			{
				LPFONTDISP pFontDispAmbient = AmbientFont();
				m_font.InitializeFont(NULL, pFontDispAmbient);
				RELEASE(pFontDispAmbient);
			}

			OnFontChanged();
		}

		if (dwStockPropMask & STOCKPROP_BORDERSTYLE)
		{
			if (dwPersistMask & STOCKPROP_BORDERSTYLE)
			{
				short sBorderStyle = m_sBorderStyle;
				ar >> m_sBorderStyle;
				if (sBorderStyle != m_sBorderStyle)
					_AfxToggleBorderStyle(this);
			}
			else
			{
				m_sBorderStyle = 0;
			}
		}

		if (dwStockPropMask & STOCKPROP_ENABLED)
		{
			if (dwPersistMask & STOCKPROP_ENABLED)
			{
				BOOL bEnabled = m_bEnabled;
				ar >> m_bEnabled;
				if ((bEnabled != m_bEnabled) && (m_hWnd != NULL))
					::EnableWindow(m_hWnd, m_bEnabled);
			}
			else
			{
				m_bEnabled = TRUE;
			}
		}

		if (dwStockPropMask & STOCKPROP_APPEARANCE)
		{
			if (dwPersistMask & STOCKPROP_APPEARANCE)
			{
				short sAppearance = m_sAppearance;
				ar >> m_sAppearance;

				if (sAppearance != m_sAppearance)
					_AfxToggleAppearance(this);
			}
			else
			{
				m_sAppearance = AmbientAppearance();
			}
		}
	}
	else
	{
		ar << dwPersistMask;

		if (dwStockPropMask & (STOCKPROP_CAPTION | STOCKPROP_TEXT))
			ar << m_strText;

		if (dwStockPropMask & STOCKPROP_FORECOLOR)
			ar << m_clrForeColor;

		if (dwStockPropMask & STOCKPROP_BACKCOLOR)
			ar << m_clrBackColor;

		if (dwStockPropMask & STOCKPROP_FONT)
		{
			BOOL bWrite = FALSE;
			LPFONT pFont = m_font.m_pFont;
			if (pFont != NULL)
			{
				// If same as ambient font (or error), write 0xFF for the flag
				LPFONTDISP pFontDispAmbient = AmbientFont();
				if (!_AfxIsSameFont(m_font, NULL, pFontDispAmbient))
				{
					LPPERSISTSTREAM pps = NULL;
					if (SUCCEEDED(pFont->QueryInterface(IID_IPersistStream,
						(LPVOID*)&pps)))
					{
						ASSERT_POINTER(pps, IPersistStream);
						ar << (BYTE)0x00;
						CArchiveStream stm(&ar);
						LPSTREAM pstm = _AfxGetArchiveStream(ar, stm);
						bWrite = SUCCEEDED(::OleSaveToStream(pps, pstm));
						pps->Release();
						if (!bWrite)
							AfxThrowArchiveException(CArchiveException::generic);
					}
				}
				RELEASE(pFontDispAmbient);
			}
			if (! bWrite)
			{
				ar << (BYTE)0xFF;
			}
		}

		if (dwStockPropMask & STOCKPROP_BORDERSTYLE)
			ar << m_sBorderStyle;

		if (dwStockPropMask & STOCKPROP_ENABLED)
			ar << m_bEnabled;

		if (dwStockPropMask & STOCKPROP_APPEARANCE)
			ar << m_sAppearance;
	}
}

/////////////////////////////////////////////////////////////////////////////
// Initialization for stock properties

void COleControl::ResetStockProps()
{
	DWORD dwStockPropMask = GetStockPropMask();

	if (dwStockPropMask & (STOCKPROP_CAPTION | STOCKPROP_TEXT))
	{
		TRY
			SetText(_T(""));
		END_TRY
	}

	if (dwStockPropMask & STOCKPROP_FORECOLOR)
		m_clrForeColor = AmbientForeColor();

	if (dwStockPropMask & STOCKPROP_BACKCOLOR)
		m_clrBackColor = AmbientBackColor();

	if (dwStockPropMask & STOCKPROP_FONT)
	{
		LPFONTDISP pFontDispAmbient = AmbientFont();
		m_font.InitializeFont(NULL, pFontDispAmbient);
		RELEASE(pFontDispAmbient);
		OnFontChanged();
	}

	if (dwStockPropMask & STOCKPROP_BORDERSTYLE)
		m_sBorderStyle = 0;

	if (dwStockPropMask & STOCKPROP_ENABLED)
		m_bEnabled = TRUE;

	if (dwStockPropMask & STOCKPROP_APPEARANCE)
		m_sAppearance = AmbientAppearance();
}

/////////////////////////////////////////////////////////////////////////////
// Appearance property

short COleControl::GetAppearance()
{
	return m_sAppearance;
}

void COleControl::SetAppearance(short sAppearance)
{
	if (sAppearance != 0 && sAppearance != 1)
		ThrowError(CTL_E_INVALIDPROPERTYVALUE, AFX_IDP_E_INVALIDPROPERTYVALUE);

	// Is the property changing?
	if (m_sAppearance == sAppearance)
		return;

	if (!BoundPropertyRequestEdit(DISPID_APPEARANCE))
		SetNotPermitted();

	ASSERT((m_hWnd == NULL) ||
		((GetExStyle() & WS_EX_CLIENTEDGE) == (DWORD)(m_sAppearance ?
			WS_EX_CLIENTEDGE : 0)));

	m_sAppearance = sAppearance;
	m_bModified = TRUE;

	_AfxToggleAppearance(this);
	OnAppearanceChanged();

	BoundPropertyChanged(DISPID_APPEARANCE);
}

void COleControl::OnAppearanceChanged()
{
	// Can be overridden by subclass

	InvalidateControl();
}

/////////////////////////////////////////////////////////////////////////////
// BackColor property

OLE_COLOR COleControl::GetBackColor()
{
	return m_clrBackColor;
}

void COleControl::SetBackColor(OLE_COLOR clrBackColor)
{
	// Is the property changing?
	if (m_clrBackColor == clrBackColor)
		return;

	if (FAILED(::OleTranslateColor(clrBackColor, NULL, NULL)))
		ThrowError(CTL_E_INVALIDPROPERTYVALUE, AFX_IDP_E_INVALIDPROPERTYVALUE);

	if (!BoundPropertyRequestEdit(DISPID_BACKCOLOR))
		SetNotPermitted();

	m_clrBackColor = clrBackColor;
	m_bModified = TRUE;
	OnBackColorChanged();

	BoundPropertyChanged(DISPID_BACKCOLOR);
}

void COleControl::OnBackColorChanged()
{
	// Can be overridden by subclass
	InvalidateControl();
}

/////////////////////////////////////////////////////////////////////////////
// BorderStyle property

short COleControl::GetBorderStyle()
{
	return m_sBorderStyle;
}

void COleControl::SetBorderStyle(short sBorderStyle)
{
	if (sBorderStyle != 0 && sBorderStyle != 1)
		ThrowError(CTL_E_INVALIDPROPERTYVALUE, AFX_IDP_E_INVALIDPROPERTYVALUE);

	// Is the property changing?
	if (m_sBorderStyle == sBorderStyle)
		return;

	if (!BoundPropertyRequestEdit(DISPID_BORDERSTYLE))
		SetNotPermitted();

	ASSERT((m_hWnd == NULL) ||
		((GetStyle() & WS_BORDER) == (DWORD)(m_sBorderStyle ? WS_BORDER : 0)));

	m_sBorderStyle = sBorderStyle;
	m_bModified = TRUE;

	_AfxToggleBorderStyle(this);
	OnBorderStyleChanged();

	BoundPropertyChanged(DISPID_BORDERSTYLE);
}

void COleControl::OnBorderStyleChanged()
{
	// Can be overridden by subclass

	InvalidateControl();
}

/////////////////////////////////////////////////////////////////////////////
// Text and Caption properties

const CString& COleControl::InternalGetText()
{
	// Some subclassed controls (such as edit controls and comboboxes) change
	// the window text without sending WM_SETTEXT. Therefore, we need to
	// ensure that m_strText is up-to-date.

	if (m_hWnd != NULL && IsSubclassedControl())
	{
		// Usually, the window text will be shorter than 32 characters.
		// When it is, we can be more efficient.
		const int _cchUsual = 32;

		if (DefWindowProc(WM_GETTEXT, (WPARAM)_cchUsual,
			(LPARAM)m_strText.GetBufferSetLength(_cchUsual)) >= _cchUsual - 1)
		{
			// Text was too long: allocate a bigger buffer.

			int nLen = DefWindowProc(WM_GETTEXTLENGTH, (WPARAM)0, (LPARAM)0) + 1;
			DefWindowProc(WM_GETTEXT, (WPARAM)nLen,
				(LPARAM)m_strText.GetBufferSetLength(nLen));
		}
		m_strText.ReleaseBuffer();
	}

	return m_strText;
}

BSTR COleControl::GetText()
{
	return ((CString&)InternalGetText()).AllocSysString();
}

void COleControl::SetText(LPCTSTR pszText)
{
	LRESULT lResult;
	if (m_hWnd != NULL)
		lResult = SendMessage(WM_SETTEXT, 0, (LPARAM)pszText);
	else
		lResult = OnSetText(0, (LPARAM)pszText);

	if (lResult == -1)
		SetNotPermitted();
}

LRESULT COleControl::OnSetText(WPARAM wParam, LPARAM lParam)
{
	ASSERT(lParam == 0 || AfxIsValidString((LPCTSTR)lParam));

	CString str = InternalGetText();

	// Is the property changing?
	if ((lParam == 0 && str.IsEmpty()) ||
		(lParam != 0 && str == (LPCTSTR)lParam))
		return 0;

	DWORD dwStockPropMask = GetStockPropMask();

	if (dwStockPropMask & STOCKPROP_CAPTION)
		if (!BoundPropertyRequestEdit(DISPID_CAPTION))
			return -1;

	if (dwStockPropMask & STOCKPROP_TEXT)
		if (!BoundPropertyRequestEdit(DISPID_TEXT))
			return -1;

	LRESULT lResult = 0;
	m_strText = (LPCTSTR)lParam;
	m_bModified = TRUE;

	if (m_hWnd != NULL)
		lResult = DefWindowProc(WM_SETTEXT, wParam, lParam);

	OnTextChanged();

	if (dwStockPropMask & STOCKPROP_CAPTION)
		BoundPropertyChanged(DISPID_CAPTION);

	if (dwStockPropMask & STOCKPROP_TEXT)
		BoundPropertyChanged(DISPID_TEXT);

	return lResult;
}

void COleControl::OnTextChanged()
{
	// Can be overridden by subclass
	InvalidateControl();
}

/////////////////////////////////////////////////////////////////////////////
// Enabled property

BOOL COleControl::GetEnabled()
{
	return m_bEnabled;
}

void COleControl::SetEnabled(BOOL bEnabled)
{
	// Is the property changing?
	if (m_bEnabled == bEnabled)
		return;

	if (!BoundPropertyRequestEdit(DISPID_ENABLED))
		SetNotPermitted();

	m_bEnabled = bEnabled;
	m_bModified = TRUE;
	if (m_hWnd != NULL)
		::EnableWindow(m_hWnd, m_bEnabled);

	// If the control is UI Active and the Enabled property changed to FALSE,
	// then UI Deactivate the control.

	if (m_bUIActive && !bEnabled)
		m_xOleInPlaceObject.UIDeactivate();

	OnEnabledChanged();

	BoundPropertyChanged(DISPID_ENABLED);
}

void COleControl::OnEnabledChanged()
{
	// Can be overridden by subclass
	InvalidateControl();
}

/////////////////////////////////////////////////////////////////////////////
// Font property

CFontHolder& COleControl::InternalGetFont()
{
	return m_font;
}

LPFONTDISP COleControl::GetFont()
{
	return m_font.GetFontDispatch();
}

void COleControl::SetFont(LPFONTDISP pFontDisp)
{
	ASSERT((pFontDisp == NULL) ||
		   AfxIsValidAddress(pFontDisp, sizeof(IDispatch), FALSE));

	m_font.InitializeFont(NULL, pFontDisp);
	m_bModified = TRUE;
	OnFontChanged();

	BoundPropertyChanged(DISPID_FONT);
}

void COleControl::OnFontChanged()
{
	// Can be overridden by subclass

	// Send WM_SETFONT to control's window
	if ((m_hWnd != NULL) &&
		(GetStockPropMask() & STOCKPROP_FONT) &&
		IsSubclassedControl())
	{
		HFONT hFontPrev = (HFONT)SendMessage(WM_GETFONT, 0, 0);

		CFontHolder& font = InternalGetFont();

		if (font.m_pFont != NULL)
		{
			HFONT hFont = font.GetFontHandle();
			font.m_pFont->AddRefHfont(hFont);
			SendMessage(WM_SETFONT, (WPARAM)hFont, 0);

			if (m_hFontPrev != NULL)
			{
				ASSERT(hFontPrev == m_hFontPrev);
				font.m_pFont->ReleaseHfont(hFontPrev);
			}
			m_hFontPrev = hFont;
		}
		else
		{
			SendMessage(WM_SETFONT, NULL, 0);
			m_hFontPrev = NULL;
		}
	}

	// Invalidate the control
	InvalidateControl();
}

/////////////////////////////////////////////////////////////////////////////
// ForeColor property

OLE_COLOR COleControl::GetForeColor()
{
	return m_clrForeColor;
}

void COleControl::SetForeColor(OLE_COLOR clrForeColor)
{
	// Is the property changing?
	if (m_clrForeColor == clrForeColor)
		return;

	if (FAILED(::OleTranslateColor(clrForeColor, NULL, NULL)))
		ThrowError(CTL_E_INVALIDPROPERTYVALUE, AFX_IDP_E_INVALIDPROPERTYVALUE);

	if (!BoundPropertyRequestEdit(DISPID_FORECOLOR))
		SetNotPermitted();

	m_clrForeColor = clrForeColor;
	m_bModified = TRUE;
	OnForeColorChanged();

	BoundPropertyChanged(DISPID_FORECOLOR);
}

void COleControl::OnForeColorChanged()
{
	// Can be overridden by subclass
	InvalidateControl();
}

/////////////////////////////////////////////////////////////////////////////
// hWnd property

OLE_HANDLE COleControl::GetHwnd()
{
#ifdef _AFXDLL
	return (OLE_HANDLE)((m_bInPlaceActive || m_bOpen) ? m_hWnd : NULL);
#else
	return (OLE_HANDLE)(m_bInPlaceActive ? m_hWnd : NULL);
#endif
}

/////////////////////////////////////////////////////////////////////////////
// ReadyState property

long COleControl::GetReadyState()
{
	return m_lReadyState;
}

////////////////////////////////////////////////////////////////////////////
// COleControl::XFontNotification

STDMETHODIMP_(ULONG) COleControl::XFontNotification::AddRef()
{
	return 1;
}

STDMETHODIMP_(ULONG) COleControl::XFontNotification::Release()
{
	return 0;
}

STDMETHODIMP COleControl::XFontNotification::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	ASSERT(ppvObj != NULL);

	if (IsEqualIID(iid, IID_IUnknown) ||
		IsEqualIID(iid, IID_IPropertyNotifySink))
	{
		*ppvObj = this;
		return S_OK;
	}

	return E_NOINTERFACE;
}

STDMETHODIMP COleControl::XFontNotification::OnChanged(DISPID)
{
	METHOD_PROLOGUE_EX(COleControl, FontNotification)
	pThis->OnFontChanged();
	pThis->m_bModified = TRUE;
	pThis->BoundPropertyChanged(DISPID_FONT);
	return S_OK;
}

STDMETHODIMP COleControl::XFontNotification::OnRequestEdit(DISPID)
{
	return S_OK;
}

CFont* COleControl::SelectStockFont(CDC* pDC)
{
	return SelectFontObject(pDC, m_font);
}

CFont* COleControl::SelectFontObject(CDC* pDC, CFontHolder& fontHolder)
{
	return fontHolder.Select(pDC, m_rcBounds.Height(), m_cyExtent);
}

void COleControl::GetStockTextMetrics(LPTEXTMETRIC lptm)
{
	m_font.QueryTextMetrics(lptm);
}

void COleControl::GetFontTextMetrics(LPTEXTMETRIC lptm, CFontHolder& fontHolder)
{
	fontHolder.QueryTextMetrics(lptm);
}

/////////////////////////////////////////////////////////////////////////////
// Ambient property access

BOOL AFXAPI _GetI4Property(LPDISPATCH pDispatch, DISPID dwDispID, DWORD* pdwResult)
{
	if (pDispatch == NULL)
		return FALSE;

	DISPPARAMS dispparams;
	memset(&dispparams, 0, sizeof dispparams);

	VARIANT vaResult;
	AfxVariantInit(&vaResult);

	EXCEPINFO excepInfo;
	memset(&excepInfo, 0, sizeof excepInfo);

	UINT nArgErr = (UINT)-1;  // initialize to invalid arg

	HRESULT hr = pDispatch->Invoke(dwDispID, IID_NULL, 0, DISPATCH_PROPERTYGET,
		&dispparams, &vaResult, &excepInfo, &nArgErr);

	if (SUCCEEDED(hr))
	{
		if ((V_VT(&vaResult) == VT_I4) ||
			SUCCEEDED(VariantChangeType(&vaResult, &vaResult, 0, VT_I4)))
		{
			*pdwResult = V_I4(&vaResult);
			return TRUE;
		}
	}

	VariantClear(&vaResult);

	if (excepInfo.bstrSource != NULL)
		SysFreeString(excepInfo.bstrSource);
	if (excepInfo.bstrDescription != NULL)
		SysFreeString(excepInfo.bstrDescription);
	if (excepInfo.bstrHelpFile != NULL)
		SysFreeString(excepInfo.bstrHelpFile);

	return FALSE;
}

COleDispatchDriver* COleControl::GetAmbientDispatchDriver()
{
	if (m_ambientDispDriver.m_lpDispatch == NULL)
	{
		// Initialize pointer to ambient property dispinterface.
		IDispatch* pDispatch = NULL;

		if (m_pClientSite != NULL &&
			SUCCEEDED(m_pClientSite->QueryInterface(IID_IDispatch,
				reinterpret_cast<void**>(&pDispatch))))
		{
			ASSERT(pDispatch != NULL);
			m_ambientDispDriver.AttachDispatch(pDispatch);
		}
	}

	return &m_ambientDispDriver;
}

AFX_STATIC_DATA const DWORD _afxAmbientFlags[] =
{
	QACONTAINER_MESSAGEREFLECT,     // -706
	0,                              // -707
	0,                              // -708
	QACONTAINER_USERMODE,           // -709
	QACONTAINER_UIDEAD,             // -710
	QACONTAINER_SHOWGRABHANDLES,    // -711
	QACONTAINER_SHOWHATCHING,       // -712
	QACONTAINER_DISPLAYASDEFAULT,   // -713
	QACONTAINER_SUPPORTSMNEMONICS,  // -714
	QACONTAINER_AUTOCLIP,           // -715
};

BOOL COleControl::GetAmbientProperty(DISPID dwDispID, VARTYPE vtProp, void* pvProp)
{
	// First, check whether ambient property can be obtained from cache.

	_AFXCTL_AMBIENT_CACHE* pAmbientCache = _afxAmbientCache;
	if (pAmbientCache->m_bValid)
	{
		switch (vtProp)
		{
		// Fetch boolean by selecting appropriate flag in cache.
		case VT_BOOL:
			{
				int nIndex = DISPID_AMBIENT_MESSAGEREFLECT - dwDispID;
				DWORD dwFlag;
				if (nIndex >= 0 && nIndex < _countof(_afxAmbientFlags) &&
					(dwFlag = _afxAmbientFlags[nIndex]) != 0)
				{
					*(BOOL*)pvProp =
						((pAmbientCache->m_dwAmbientFlags & dwFlag) != 0);
					return TRUE;
				}
			}
			break;

		// Fetch color, appearance, or font from corresponding cache entry.
		case VT_I4:
			switch (dwDispID)
			{
			case DISPID_AMBIENT_FORECOLOR:
				*(DWORD*)pvProp = pAmbientCache->m_colorFore;
				return TRUE;

			case DISPID_AMBIENT_BACKCOLOR:
				*(DWORD*)pvProp = pAmbientCache->m_colorBack;
				return TRUE;

			case DISPID_AMBIENT_APPEARANCE:
				*(DWORD*)pvProp = pAmbientCache->m_dwAppearance;
				return TRUE;
			}
			break;

		case VT_I2:
			if (dwDispID == DISPID_AMBIENT_APPEARANCE)
			{
				*(short*)pvProp = (short)pAmbientCache->m_dwAppearance;
				return TRUE;
			}
			break;

		case VT_DISPATCH:
			if ((dwDispID == DISPID_AMBIENT_FONT) &&
				(pAmbientCache->m_pFont != NULL) &&
				SUCCEEDED(pAmbientCache->m_pFont->QueryInterface(IID_IFontDisp,
					reinterpret_cast<void**>(pvProp))))
			{
				return TRUE;
			}
			break;
		}
	}

	// If there's no ambient dispatch interface available, then fail.

	COleDispatchDriver* pDispDriver = GetAmbientDispatchDriver();
	if (pDispDriver->m_lpDispatch == NULL)
		return FALSE;

	// If requested property is of type VT_I4, use optimized function.

	if (vtProp == VT_I4)
		return _GetI4Property(pDispDriver->m_lpDispatch, dwDispID,
			(DWORD*)pvProp);

	// If none of the above apply, just use the dispatch driver.

	BOOL bSuccess = FALSE;
	TRY
	{
		pDispDriver->GetProperty(dwDispID, vtProp, pvProp);
		bSuccess = TRUE;
	}
	END_TRY

	return bSuccess;
}

short COleControl::AmbientAppearance()
{
	DWORD dwAppearance;
	if (!GetAmbientProperty(DISPID_AMBIENT_APPEARANCE, VT_I4, &dwAppearance))
		dwAppearance = 0;
	return (short)dwAppearance;
}

OLE_COLOR COleControl::AmbientBackColor()
{
	OLE_COLOR clrBackColor;
	if (!GetAmbientProperty(DISPID_AMBIENT_BACKCOLOR, VT_I4, &clrBackColor))
		clrBackColor = GetSysColor(COLOR_WINDOW);
	return clrBackColor;
}

CString COleControl::AmbientDisplayName()
{
	CString strDisplayName;
	GetAmbientProperty(DISPID_AMBIENT_DISPLAYNAME, VT_BSTR, &strDisplayName);
	return strDisplayName;
}

LPFONTDISP COleControl::AmbientFont()
{
	// Note: Caller MUST Release the font!
	LPFONTDISP pDisp;
	if (!GetAmbientProperty(DISPID_AMBIENT_FONT, VT_DISPATCH, &pDisp))
		pDisp = NULL;
	return pDisp;
}

OLE_COLOR COleControl::AmbientForeColor()
{
	OLE_COLOR clrForeColor;
	if (!GetAmbientProperty(DISPID_AMBIENT_FORECOLOR, VT_I4, &clrForeColor))
		clrForeColor = GetSysColor(COLOR_WINDOWTEXT);
	return clrForeColor;
}

LCID COleControl::AmbientLocaleID()
{
	LCID lcid;
	if (!GetAmbientProperty(DISPID_AMBIENT_LOCALEID, VT_I4, &lcid))
		lcid = 0;
	return lcid;
}

CString COleControl::AmbientScaleUnits()
{
	CString strScaleUnits;
	GetAmbientProperty(DISPID_AMBIENT_SCALEUNITS, VT_BSTR, &strScaleUnits);
	return strScaleUnits;
}

short COleControl::AmbientTextAlign()
{
	short iTextAlign;
	if (!GetAmbientProperty(DISPID_AMBIENT_TEXTALIGN, VT_I2, &iTextAlign))
		iTextAlign = 0;
	return iTextAlign;
}

BOOL COleControl::AmbientUserMode()
{
	BOOL bUserMode;
	if (!GetAmbientProperty(DISPID_AMBIENT_USERMODE, VT_BOOL, &bUserMode))
		bUserMode = TRUE;
	return bUserMode;
}

BOOL COleControl::AmbientUIDead()
{
	BOOL bUIDead;
	if (!GetAmbientProperty(DISPID_AMBIENT_UIDEAD, VT_BOOL, &bUIDead))
		bUIDead = FALSE;
	return bUIDead;
}

BOOL COleControl::AmbientShowGrabHandles()
{
	BOOL bShowGrab;
	if (!GetAmbientProperty(DISPID_AMBIENT_SHOWGRABHANDLES, VT_BOOL, &bShowGrab))
		bShowGrab = TRUE;
	return bShowGrab;
}

BOOL COleControl::AmbientShowHatching()
{
	BOOL bShowHatch;
	if (!GetAmbientProperty(DISPID_AMBIENT_SHOWHATCHING, VT_BOOL, &bShowHatch))
		bShowHatch = TRUE;
	return bShowHatch;
}

/////////////////////////////////////////////////////////////////////////////
// Calls to IPropertyNotifySink

void COleControl::BoundPropertyChanged(DISPID dispid)
{
	POSITION pos = m_xPropConnPt.GetStartPosition();
	LPPROPERTYNOTIFYSINK pPropNotifySink;

	while (pos != NULL)
	{
		pPropNotifySink =
			(LPPROPERTYNOTIFYSINK)m_xPropConnPt.GetNextConnection(pos);
		ASSERT(pPropNotifySink != NULL);
		pPropNotifySink->OnChanged(dispid);
	}
}

BOOL COleControl::BoundPropertyRequestEdit(DISPID dispid)
{
	POSITION pos = m_xPropConnPt.GetStartPosition();
	LPPROPERTYNOTIFYSINK pPropNotifySink;

	while (pos != NULL)
	{
		pPropNotifySink =
			(LPPROPERTYNOTIFYSINK)m_xPropConnPt.GetNextConnection(pos);
		ASSERT(pPropNotifySink != NULL);
		if (pPropNotifySink->OnRequestEdit(dispid) != S_OK)
			return FALSE;
	}

	// All of the sinks said yes, so it's ok.
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// Function to call when BoundPropertyRequestEdit fails

void COleControl::SetNotPermitted()
{
	ThrowError(CTL_E_SETNOTPERMITTED, AFX_IDP_E_SETNOTPERMITTED);
}

/////////////////////////////////////////////////////////////////////////////
// Placeholder functions for read-only or write-only properties

void COleControl::SetNotSupported()
{
	ThrowError(CTL_E_SETNOTSUPPORTED, AFX_IDP_E_SETNOTSUPPORTED);
}

void COleControl::GetNotSupported()
{
	ThrowError(CTL_E_GETNOTSUPPORTED, AFX_IDP_E_GETNOTSUPPORTED);
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::XPerPropertyBrowsing

STDMETHODIMP_(ULONG) COleControl::XPerPropertyBrowsing::AddRef()
{
	METHOD_PROLOGUE_EX_(COleControl, PerPropertyBrowsing)
	return (ULONG)pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleControl::XPerPropertyBrowsing::Release()
{
	METHOD_PROLOGUE_EX_(COleControl, PerPropertyBrowsing)
	return (ULONG)pThis->ExternalRelease();
}

STDMETHODIMP COleControl::XPerPropertyBrowsing::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControl, PerPropertyBrowsing)
	return (HRESULT)pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleControl::XPerPropertyBrowsing::GetDisplayString(
	DISPID dispid, BSTR* lpbstr)
{
	METHOD_PROLOGUE_EX(COleControl, PerPropertyBrowsing)

	ASSERT_NULL_OR_POINTER(lpbstr, BSTR);

	CString strValue;
	BOOL bSuccess = pThis->OnGetDisplayString(dispid, strValue);

	if (lpbstr != NULL)
		*lpbstr = (bSuccess ? strValue.AllocSysString() : NULL);

	return bSuccess ? S_OK : S_FALSE;
}

BOOL COleControl::OnGetDisplayString(DISPID dispid, CString& strValue)
{
	TRY
	{
		switch (dispid)
		{
		case DISPID_FONT:
			return m_font.GetDisplayString(strValue);

		case DISPID_BORDERSTYLE:
			return strValue.LoadString(m_sBorderStyle == 0 ?
				AFX_IDS_BORDERSTYLE_0 : AFX_IDS_BORDERSTYLE_1);
		}
	}
	END_TRY

	return FALSE;
}

STDMETHODIMP COleControl::XPerPropertyBrowsing::MapPropertyToPage(
	DISPID dispid, LPCLSID lpclsid)
{
	METHOD_PROLOGUE_EX(COleControl, PerPropertyBrowsing)

	ASSERT_NULL_OR_POINTER(lpclsid, CLSID);

	CLSID clsid = GUID_NULL;
	BOOL bPageOptional = FALSE;
	BOOL bSuccess = pThis->OnMapPropertyToPage(dispid, &clsid, &bPageOptional);

	if (lpclsid != NULL)
		*lpclsid = (bSuccess ? clsid : GUID_NULL);

	return bSuccess ? (bPageOptional ? S_OK : S_FALSE) :
		PERPROP_E_NOPAGEAVAILABLE;
}

BOOL COleControl::OnMapPropertyToPage(DISPID dispid, LPCLSID lpclsid,
	BOOL* pbPageOptional)
{
	switch (dispid)
	{
	case DISPID_FONT:
		*lpclsid = CLSID_CFontPropPage;
		*pbPageOptional = TRUE;
		return TRUE;

	case DISPID_BACKCOLOR:
	case DISPID_FORECOLOR:
		*lpclsid = CLSID_CColorPropPage;
		*pbPageOptional = TRUE;
		return TRUE;
	}

	return FALSE;
}

inline LPOLESTR AFXAPI _AfxCopyString(LPCTSTR psz)
{
	if (psz == NULL)
		return NULL;

	int cch = lstrlen(psz) + 1;
	LPOLESTR pszCopy = NULL;

	if ((pszCopy = (LPOLESTR)CoTaskMemAlloc(cch * sizeof(OLECHAR))) != NULL)
	{
#ifdef _UNICODE
		wcscpy(pszCopy, psz);
#elif !defined(OLE2ANSI)
		MultiByteToWideChar(CP_ACP, 0, psz, -1, pszCopy, cch);
#else
		lstrcpy(pszCopy, psz);
#endif
	}

	return pszCopy;
}

STDMETHODIMP COleControl::XPerPropertyBrowsing::GetPredefinedStrings(
	DISPID dispid, CALPOLESTR* lpcaStringsOut, CADWORD* lpcaCookiesOut)
{
	METHOD_PROLOGUE_EX(COleControl, PerPropertyBrowsing)

	if ((lpcaStringsOut == NULL) || (lpcaCookiesOut == NULL))
		return E_POINTER;

	ASSERT_POINTER(lpcaStringsOut, CALPOLESTR);
	ASSERT_POINTER(lpcaCookiesOut, CADWORD);

	CStringArray stringArray;
	CDWordArray cookieArray;

	BOOL bSuccess = pThis->OnGetPredefinedStrings(dispid, &stringArray,
		&cookieArray);

	if (bSuccess)
	{
		// Allocate and fill arrays to return.

		ASSERT(stringArray.GetSize() == cookieArray.GetSize());

		int iElem = 0;
		LPOLESTR lpszCopy;
		ULONG cElems = stringArray.GetSize();

		lpcaStringsOut->pElems = (LPOLESTR*)CoTaskMemAlloc(
			sizeof(LPOLESTR) * cElems);

		if (lpcaStringsOut->pElems == NULL)
			return E_OUTOFMEMORY;

		lpcaCookiesOut->pElems = (DWORD*)CoTaskMemAlloc(
			sizeof(DWORD*) * cElems);

		if (lpcaCookiesOut->pElems == NULL)
		{
			CoTaskMemFree(lpcaStringsOut->pElems);
			return E_OUTOFMEMORY;
		}

		lpcaStringsOut->cElems = cElems;
		lpcaCookiesOut->cElems = cElems;

		for (iElem = 0; iElem < (int)cElems; iElem++)
		{
			lpszCopy = _AfxCopyString(stringArray.GetAt(iElem));

			if (lpszCopy == NULL)
			{
				// cleanup everything allocated so far...
				while (--iElem >= 0)
					CoTaskMemFree(lpcaStringsOut->pElems[iElem]);

				CoTaskMemFree(lpcaCookiesOut->pElems);
				CoTaskMemFree(lpcaStringsOut->pElems);

				return E_OUTOFMEMORY;
			}

			lpcaStringsOut->pElems[iElem] = lpszCopy;
			lpcaCookiesOut->pElems[iElem] = cookieArray.GetAt(iElem);
		}
	}

	return bSuccess ? S_OK : S_FALSE;
}

BOOL COleControl::OnGetPredefinedStrings(DISPID dispid,
	CStringArray* pStringArray, CDWordArray* pCookieArray)
{
	BOOL bResult = FALSE;

	switch (dispid)
	{
	case DISPID_BORDERSTYLE:
		TRY
		{
			CString str;
			str.LoadString(AFX_IDS_BORDERSTYLE_0);
			pStringArray->Add(str);
			pCookieArray->Add(0);
			str.LoadString(AFX_IDS_BORDERSTYLE_1);
			pStringArray->Add(str);
			pCookieArray->Add(1);
			bResult = TRUE;
		}
		CATCH (CException, e)
		{
			pStringArray->RemoveAll();
			pCookieArray->RemoveAll();
			bResult = FALSE;
		}
		END_CATCH
		break;
	}

	return bResult;
}

STDMETHODIMP COleControl::XPerPropertyBrowsing::GetPredefinedValue(
	DISPID dispid, DWORD dwCookie, VARIANT* lpvarOut)
{
	METHOD_PROLOGUE_EX(COleControl, PerPropertyBrowsing)

	ASSERT_POINTER(lpvarOut, VARIANT);

	return pThis->OnGetPredefinedValue(dispid, dwCookie, lpvarOut) ?
		S_OK : E_FAIL;
}

BOOL COleControl::OnGetPredefinedValue(DISPID dispid, DWORD dwCookie,
	VARIANT* lpvarOut)
{
	switch (dispid)
	{
	case DISPID_BORDERSTYLE:
		if ((dwCookie == 0) || (dwCookie == 1))
		{
			VariantClear(lpvarOut);
			V_VT(lpvarOut) = VT_I4;
			V_I4(lpvarOut) = dwCookie;
			return TRUE;
		}
		break;
	}

	return FALSE;
}

void COleControl::Load(LPCTSTR strNewPath, CDataPathProperty& prop)
{
	prop.SetControl(this);
	prop.Open(strNewPath);
}

/////////////////////////////////////////////////////////////////////////////
// CDataPathProperty implementation

BOOL CDataPathProperty::Open(CFileException* pError)
{
	return CAsyncMonikerFile::Open(m_strPath, m_pControl ? m_pControl->GetClientSite() : NULL, pError);
}

BOOL CDataPathProperty::Open(LPCTSTR lpszPath, CFileException* pError)
{
	SetPath(lpszPath);
	return Open(pError);
}

BOOL CDataPathProperty::Open(COleControl* pControl, CFileException* pError)
{
	SetControl(pControl);
	return Open(pError);
}

BOOL CDataPathProperty::Open(LPCTSTR lpszPath, COleControl* pControl, CFileException* pError)
{
	SetControl(pControl);
	SetPath(lpszPath);
	return Open(pError);
}

void CDataPathProperty::ResetData()
{
}

#ifdef _DEBUG
void CDataPathProperty::AssertValid() const
{
	CAsyncMonikerFile::AssertValid();
}

void CDataPathProperty::Dump(CDumpContext& dc) const
{
	CAsyncMonikerFile::Dump(dc);

	dc << "\nm_pControl = " << m_pControl;
	dc << "\nm_strPath = \"" << m_strPath;
	dc << "\"\n";
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CCachedDataPathProperty implementation

AFX_STATIC inline DWORD _AfxTransferFileContent(CFile* pFrom, CFile* pTo)
{
	BYTE buff[1024];
	DWORD dwRead = 0;
	DWORD dwActual;
	do
	{
		dwActual = pFrom->Read(buff, 1024);
		pTo->Write(buff, dwActual);

		dwRead += dwActual;
	}
	while (dwActual > 0);
	return dwRead;
}

void CCachedDataPathProperty::OnDataAvailable(DWORD dwSize, DWORD bscfFlag)
{
	UNUSED_ALWAYS(bscfFlag);
	UNUSED_ALWAYS(dwSize);
	DWORD dwPos = m_Cache.GetPosition();
	TRY
	{
		// Cache the data in our mem file.
		m_Cache.SeekToEnd();
		_AfxTransferFileContent(this, &m_Cache);
	}
	CATCH_ALL(e)
	{
		m_Cache.Seek(dwPos, CFile::begin);
		THROW_LAST();
	}
	END_CATCH_ALL
	m_Cache.Seek(dwPos, CFile::begin);
}

void CCachedDataPathProperty::Close()
{
	m_Cache.SetLength(0);
	CDataPathProperty::Close();
}

void CCachedDataPathProperty::ResetData()
{
	m_Cache.SetLength(0);
}
#ifdef _DEBUG
void CCachedDataPathProperty::AssertValid() const
{
	CDataPathProperty::AssertValid();
	m_Cache.AssertValid();
}

void CCachedDataPathProperty::Dump(CDumpContext& dc) const
{
	CDataPathProperty::Dump(dc);

	m_Cache.Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CDataPathProperty, CAsyncMonikerFile)
IMPLEMENT_DYNAMIC(CCachedDataPathProperty, CDataPathProperty)
