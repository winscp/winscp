// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Inlines for AFXOLE.H

/////////////////////////////////////////////////////////////////////////////
// General OLE inlines (CDocItem, COleDocument)

#ifdef _AFXOLE_INLINE

// CDocItem
_AFXOLE_INLINE CDocument* CDocItem::GetDocument() const
	{ return m_pDocument; }

// COleDocument
_AFXOLE_INLINE void COleDocument::EnableCompoundFile(BOOL bEnable)
	{ m_bCompoundFile = bEnable; }

// COleMessageFilter
_AFXOLE_INLINE void COleMessageFilter::SetBusyReply(SERVERCALL nBusyReply)
	{ ASSERT_VALID(this); m_nBusyReply = nBusyReply; }
_AFXOLE_INLINE void COleMessageFilter::SetRetryReply(DWORD nRetryReply)
	{ ASSERT_VALID(this); m_nRetryReply = nRetryReply; }
_AFXOLE_INLINE void COleMessageFilter::SetMessagePendingDelay(DWORD nTimeout)
	{ ASSERT_VALID(this); m_nTimeout = nTimeout; }
_AFXOLE_INLINE void COleMessageFilter::EnableBusyDialog(BOOL bEnable)
	{ ASSERT_VALID(this); m_bEnableBusy = bEnable; }
_AFXOLE_INLINE void COleMessageFilter::EnableNotRespondingDialog(BOOL bEnable)
	{ ASSERT_VALID(this); m_bEnableNotResponding = bEnable; }

#endif //_AFXOLE_INLINE

/////////////////////////////////////////////////////////////////////////////
// OLE Moniker inlines

#ifdef _AFXOLEMONIKER_INLINE

// CMonikerFile
_AFXOLEMONIKER_INLINE CMonikerFile::CMonikerFile() { }
_AFXOLEMONIKER_INLINE IMoniker* CMonikerFile::GetMoniker() const
	{ ASSERT_VALID(this); return m_Moniker; }

// CAsyncMonikerFile
_AFXOLEMONIKER_INLINE IBinding* CAsyncMonikerFile::GetBinding() const
	{ ASSERT_VALID(this); return m_Binding; }
_AFXOLEMONIKER_INLINE void CAsyncMonikerFile::SetBinding(IBinding* pBinding)
	{ ASSERT_VALID(this); m_Binding=pBinding; }
_AFXOLEMONIKER_INLINE void CAsyncMonikerFile::SetFormatEtc(FORMATETC* pFormatEtc)
	{ ASSERT_VALID(this); m_pFormatEtc=pFormatEtc; }
_AFXOLEMONIKER_INLINE FORMATETC* CAsyncMonikerFile::GetFormatEtc() const
	{ ASSERT_VALID(this); return m_pFormatEtc; }

#endif //_AFXOLEMONIKER_INLINE

/////////////////////////////////////////////////////////////////////////////
// OLE automation inlines

#ifdef _AFXDISP_INLINE

// COleException
_AFXDISP_INLINE COleException::COleException()
	{ m_sc = S_OK; }
_AFXDISP_INLINE COleException::~COleException()
	{ }

// CCmdTarget
_AFXDISP_INLINE DWORD CCmdTarget::InternalAddRef()
	{ ASSERT(GetInterfaceMap() != NULL); return InterlockedIncrement(&m_dwRef); }

// CObjectFactory
_AFXDISP_INLINE BOOL COleObjectFactory::IsRegistered() const
	{ ASSERT_VALID(this); return m_dwRegister != 0; }
_AFXDISP_INLINE REFCLSID COleObjectFactory::GetClassID() const
	{ ASSERT_VALID(this); return m_clsid; }

// COleDispatchDriver
_AFXDISP_INLINE COleDispatchDriver::~COleDispatchDriver()
	{ ReleaseDispatch(); }
_AFXDISP_INLINE COleDispatchDriver::operator LPDISPATCH()
	{ return m_lpDispatch; }

// COleVariant
_AFXDISP_INLINE COleVariant::COleVariant()
	{ AfxVariantInit(this); }
_AFXDISP_INLINE COleVariant::~COleVariant()
	{ VERIFY(::VariantClear(this) == NOERROR); }
_AFXDISP_INLINE void COleVariant::Clear()
	{ VERIFY(::VariantClear(this) == NOERROR); }
_AFXDISP_INLINE COleVariant::COleVariant(LPCTSTR lpszSrc)
	{ vt = VT_EMPTY; *this = lpszSrc; }
_AFXDISP_INLINE COleVariant::COleVariant(CString& strSrc)
	{ vt = VT_EMPTY; *this = strSrc; }
_AFXDISP_INLINE COleVariant::COleVariant(BYTE nSrc)
	{ vt = VT_UI1; bVal = nSrc; }
_AFXDISP_INLINE COleVariant::COleVariant(const COleCurrency& curSrc)
	{ vt = VT_CY; cyVal = curSrc.m_cur; }
_AFXDISP_INLINE COleVariant::COleVariant(float fltSrc)
	{ vt = VT_R4; fltVal = fltSrc; }
_AFXDISP_INLINE COleVariant::COleVariant(double dblSrc)
	{ vt = VT_R8; dblVal = dblSrc; }
_AFXDISP_INLINE COleVariant::COleVariant(const COleDateTime& dateSrc)
	{ vt = VT_DATE; date = dateSrc.m_dt; }
_AFXDISP_INLINE COleVariant::COleVariant(const CByteArray& arrSrc)
	{ vt = VT_EMPTY; *this = arrSrc; }
_AFXDISP_INLINE COleVariant::COleVariant(const CLongBinary& lbSrc)
	{ vt = VT_EMPTY; *this = lbSrc; }
_AFXDISP_INLINE BOOL COleVariant::operator==(LPCVARIANT pSrc) const
	{ return *this == *pSrc; }
_AFXDISP_INLINE COleVariant::operator LPVARIANT()
	{ return this; }
_AFXDISP_INLINE COleVariant::operator LPCVARIANT() const
	{ return this; }

// COleCurrency
_AFXDISP_INLINE COleCurrency::COleCurrency()
	{ m_cur.Hi = 0; m_cur.Lo = 0; SetStatus(valid); }
_AFXDISP_INLINE COleCurrency::COleCurrency(CURRENCY cySrc)
	{ m_cur = cySrc; SetStatus(valid); }
_AFXDISP_INLINE COleCurrency::COleCurrency(const COleCurrency& curSrc)
	{ m_cur = curSrc.m_cur; m_status = curSrc.m_status; }
_AFXDISP_INLINE COleCurrency::COleCurrency(const VARIANT& varSrc)
	{ *this = varSrc; }
_AFXDISP_INLINE COleCurrency::CurrencyStatus COleCurrency::GetStatus() const
	{ return m_status; }
_AFXDISP_INLINE void COleCurrency::SetStatus(CurrencyStatus status)
	{ m_status = status; }
_AFXDISP_INLINE const COleCurrency& COleCurrency::operator+=(const COleCurrency& cur)
	{ *this = *this + cur; return *this; }
_AFXDISP_INLINE const COleCurrency& COleCurrency::operator-=(const COleCurrency& cur)
	{ *this = *this - cur; return *this; }
_AFXDISP_INLINE const COleCurrency& COleCurrency::operator*=(long nOperand)
	{ *this = *this * nOperand; return *this; }
_AFXDISP_INLINE const COleCurrency& COleCurrency::operator/=(long nOperand)
	{ *this = *this / nOperand; return *this; }
_AFXDISP_INLINE BOOL COleCurrency::operator==(const COleCurrency& cur) const
	{ return(m_status == cur.m_status && m_cur.Hi == cur.m_cur.Hi &&
		m_cur.Lo == cur.m_cur.Lo); }
_AFXDISP_INLINE BOOL COleCurrency::operator!=(const COleCurrency& cur) const
	{ return(m_status != cur.m_status || m_cur.Hi != cur.m_cur.Hi ||
		m_cur.Lo != cur.m_cur.Lo); }
_AFXDISP_INLINE COleCurrency::operator CURRENCY() const
	{ return m_cur; }

// COleDateTime
_AFXDISP_INLINE COleDateTime::COleDateTime()
	{ m_dt = 0; SetStatus(valid); }
_AFXDISP_INLINE COleDateTime::COleDateTime(const COleDateTime& dateSrc)
	{ m_dt = dateSrc.m_dt; m_status = dateSrc.m_status; }
_AFXDISP_INLINE COleDateTime::COleDateTime(const VARIANT& varSrc)
	{ *this = varSrc; }
_AFXDISP_INLINE COleDateTime::COleDateTime(DATE dtSrc)
	{ m_dt = dtSrc; SetStatus(valid); }
_AFXDISP_INLINE COleDateTime::COleDateTime(time_t timeSrc)
	{ *this = timeSrc; }
_AFXDISP_INLINE COleDateTime::COleDateTime(const SYSTEMTIME& systimeSrc)
	{ *this = systimeSrc; }
_AFXDISP_INLINE COleDateTime::COleDateTime(const FILETIME& filetimeSrc)
	{ *this = filetimeSrc; }
_AFXDISP_INLINE COleDateTime::COleDateTime(int nYear, int nMonth, int nDay,
	int nHour, int nMin, int nSec)
	{ SetDateTime(nYear, nMonth, nDay, nHour, nMin, nSec); }
_AFXDISP_INLINE COleDateTime::COleDateTime(WORD wDosDate, WORD wDosTime)
	{ m_status = DosDateTimeToVariantTime(wDosDate, wDosTime, &m_dt) ?
		valid : invalid; }
_AFXDISP_INLINE const COleDateTime& COleDateTime::operator=(const COleDateTime& dateSrc)
	{ m_dt = dateSrc.m_dt; m_status = dateSrc.m_status; return *this; }
_AFXDISP_INLINE COleDateTime::DateTimeStatus COleDateTime::GetStatus() const
	{ return m_status; }
_AFXDISP_INLINE void COleDateTime::SetStatus(DateTimeStatus status)
	{ m_status = status; }
_AFXDISP_INLINE BOOL COleDateTime::operator==(const COleDateTime& date) const
	{ return (m_status == date.m_status && m_dt == date.m_dt); }
_AFXDISP_INLINE BOOL COleDateTime::operator!=(const COleDateTime& date) const
	{ return (m_status != date.m_status || m_dt != date.m_dt); }
_AFXDISP_INLINE const COleDateTime& COleDateTime::operator+=(
	const COleDateTimeSpan dateSpan)
	{ *this = *this + dateSpan; return *this; }
_AFXDISP_INLINE const COleDateTime& COleDateTime::operator-=(
	const COleDateTimeSpan dateSpan)
	{ *this = *this - dateSpan; return *this; }
_AFXDISP_INLINE COleDateTime::operator DATE() const
	{ return m_dt; }
_AFXDISP_INLINE int COleDateTime::SetDate(int nYear, int nMonth, int nDay)
	{ return SetDateTime(nYear, nMonth, nDay, 0, 0, 0); }
_AFXDISP_INLINE int COleDateTime::SetTime(int nHour, int nMin, int nSec)
	// Set date to zero date - 12/30/1899
	{ return SetDateTime(1899, 12, 30, nHour, nMin, nSec); }

// COleDateTimeSpan
_AFXDISP_INLINE COleDateTimeSpan::COleDateTimeSpan()
	{ m_span = 0; SetStatus(valid); }
_AFXDISP_INLINE COleDateTimeSpan::COleDateTimeSpan(double dblSpanSrc)
	{ m_span = dblSpanSrc; SetStatus(valid); }
_AFXDISP_INLINE COleDateTimeSpan::COleDateTimeSpan(
	const COleDateTimeSpan& dateSpanSrc)
	{ m_span = dateSpanSrc.m_span; m_status = dateSpanSrc.m_status; }
_AFXDISP_INLINE COleDateTimeSpan::COleDateTimeSpan(
	long lDays, int nHours, int nMins, int nSecs)
	{ SetDateTimeSpan(lDays, nHours, nMins, nSecs); }
_AFXDISP_INLINE COleDateTimeSpan::DateTimeSpanStatus COleDateTimeSpan::GetStatus() const
	{ return m_status; }
_AFXDISP_INLINE void COleDateTimeSpan::SetStatus(DateTimeSpanStatus status)
	{ m_status = status; }
_AFXDISP_INLINE double COleDateTimeSpan::GetTotalDays() const
	{ ASSERT(GetStatus() == valid); return m_span; }
_AFXDISP_INLINE double COleDateTimeSpan::GetTotalHours() const
	{ ASSERT(GetStatus() == valid);
		long lReturns = (long)(m_span * 24 + AFX_OLE_DATETIME_HALFSECOND);
		return lReturns;
	}
_AFXDISP_INLINE double COleDateTimeSpan::GetTotalMinutes() const
	{ ASSERT(GetStatus() == valid);
		long lReturns = (long)(m_span * 24 * 60 + AFX_OLE_DATETIME_HALFSECOND);
		return lReturns;
	}
_AFXDISP_INLINE double COleDateTimeSpan::GetTotalSeconds() const
	{ ASSERT(GetStatus() == valid);
		long lReturns = (long)(m_span * 24 * 60 * 60 + AFX_OLE_DATETIME_HALFSECOND);
		return lReturns;
	}

_AFXDISP_INLINE long COleDateTimeSpan::GetDays() const
	{ ASSERT(GetStatus() == valid); return (long)m_span; }
_AFXDISP_INLINE BOOL COleDateTimeSpan::operator==(
	const COleDateTimeSpan& dateSpan) const
	{ return (m_status == dateSpan.m_status &&
		m_span == dateSpan.m_span); }
_AFXDISP_INLINE BOOL COleDateTimeSpan::operator!=(
	const COleDateTimeSpan& dateSpan) const
	{ return (m_status != dateSpan.m_status ||
		m_span != dateSpan.m_span); }
_AFXDISP_INLINE BOOL COleDateTimeSpan::operator<(
	const COleDateTimeSpan& dateSpan) const
	{ ASSERT(GetStatus() == valid);
		ASSERT(dateSpan.GetStatus() == valid);
		return m_span < dateSpan.m_span; }
_AFXDISP_INLINE BOOL COleDateTimeSpan::operator>(
	const COleDateTimeSpan& dateSpan) const
	{ ASSERT(GetStatus() == valid);
		ASSERT(dateSpan.GetStatus() == valid);
		return m_span > dateSpan.m_span; }
_AFXDISP_INLINE BOOL COleDateTimeSpan::operator<=(
	const COleDateTimeSpan& dateSpan) const
	{ ASSERT(GetStatus() == valid);
		ASSERT(dateSpan.GetStatus() == valid);
		return m_span <= dateSpan.m_span; }
_AFXDISP_INLINE BOOL COleDateTimeSpan::operator>=(
	const COleDateTimeSpan& dateSpan) const
	{ ASSERT(GetStatus() == valid);
		ASSERT(dateSpan.GetStatus() == valid);
		return m_span >= dateSpan.m_span; }
_AFXDISP_INLINE const COleDateTimeSpan& COleDateTimeSpan::operator+=(
	const COleDateTimeSpan dateSpan)
	{ *this = *this + dateSpan; return *this; }
_AFXDISP_INLINE const COleDateTimeSpan& COleDateTimeSpan::operator-=(
	const COleDateTimeSpan dateSpan)
	{ *this = *this - dateSpan; return *this; }
_AFXDISP_INLINE COleDateTimeSpan COleDateTimeSpan::operator-() const
	{ return -this->m_span; }
_AFXDISP_INLINE COleDateTimeSpan::operator double() const
	{ return m_span; }

// COleSafeArray
_AFXDISP_INLINE COleSafeArray::COleSafeArray()
	{ AfxSafeArrayInit(this);
		vt = VT_EMPTY; }
_AFXDISP_INLINE COleSafeArray::~COleSafeArray()
	{ Clear(); }
_AFXDISP_INLINE void COleSafeArray::Clear()
	{ VERIFY(::VariantClear(this) == NOERROR); }
_AFXDISP_INLINE COleSafeArray::operator LPVARIANT()
	{ return this; }
_AFXDISP_INLINE COleSafeArray::operator LPCVARIANT() const
	{ return this; }
_AFXDISP_INLINE DWORD COleSafeArray::GetDim()
	{ return ::SafeArrayGetDim(parray); }
_AFXDISP_INLINE DWORD COleSafeArray::GetElemSize()
	{ return ::SafeArrayGetElemsize(parray); }

#endif //_AFXDISP_INLINE

/////////////////////////////////////////////////////////////////////////////
// OLE Container inlines

#ifdef _AFXOLECLI_INLINE

// COleClientItem
_AFXOLECLI_INLINE SCODE COleClientItem::GetLastStatus() const
	{ ASSERT_VALID(this); return m_scLast; }
_AFXOLECLI_INLINE COleDocument* COleClientItem::GetDocument() const
	{ ASSERT_VALID(this); return (COleDocument*)m_pDocument; }
_AFXOLECLI_INLINE OLE_OBJTYPE COleClientItem::GetType() const
	{ ASSERT_VALID(this); return m_nItemType; }
_AFXOLECLI_INLINE DVASPECT COleClientItem::GetDrawAspect() const
	{ ASSERT_VALID(this); return m_nDrawAspect; }
_AFXOLECLI_INLINE BOOL COleClientItem::IsRunning() const
	{ ASSERT_VALID(this);
		ASSERT(m_lpObject != NULL);
		return ::OleIsRunning(m_lpObject); }
_AFXOLECLI_INLINE UINT COleClientItem::GetItemState() const
	{ ASSERT_VALID(this); return m_nItemState; }
_AFXOLECLI_INLINE BOOL COleClientItem::IsInPlaceActive() const
	{ ASSERT_VALID(this);
		return m_nItemState == activeState || m_nItemState == activeUIState; }
_AFXOLECLI_INLINE BOOL COleClientItem::IsOpen() const
	{ ASSERT_VALID(this); return m_nItemState == openState; }
_AFXOLECLI_INLINE BOOL COleClientItem::IsLinkUpToDate() const
	{ ASSERT_VALID(this);
		ASSERT(m_lpObject != NULL);
		// TRUE if result is S_OK (aka S_TRUE)
		return m_lpObject->IsUpToDate() == NOERROR; }
_AFXOLECLI_INLINE CView* COleClientItem::GetActiveView() const
	{ return m_pView; }

// COleLinkingDoc
_AFXOLECLI_INLINE void COleLinkingDoc::BeginDeferErrors()
	{ ASSERT(m_pLastException == NULL); ++m_bDeferErrors; }

#endif //_AFXOLECLI_INLINE

#ifdef _AFXOLEDOBJ_INLINE

// COleDataObject
_AFXOLEDOBJ_INLINE COleDataObject::~COleDataObject()
	{ Release(); }

#endif //_AFXOLECTL_INLINE

/////////////////////////////////////////////////////////////////////////////
// OLE dialog inlines

#ifdef _AFXODLGS_INLINE

_AFXODLGS_INLINE UINT COleDialog::GetLastError() const
	{ return m_nLastError; }
_AFXODLGS_INLINE CString COleInsertDialog::GetPathName() const
	{ ASSERT_VALID(this);
		ASSERT(GetSelectionType() != createNewItem); return m_szFileName; }
_AFXODLGS_INLINE REFCLSID COleInsertDialog::GetClassID() const
	{ ASSERT_VALID(this); return m_io.clsid; }
_AFXODLGS_INLINE HGLOBAL COleInsertDialog::GetIconicMetafile() const
	{ ASSERT_VALID(this); return m_io.hMetaPict; }
_AFXODLGS_INLINE DVASPECT COleInsertDialog::GetDrawAspect() const
	{ ASSERT_VALID(this); return m_io.dwFlags & IOF_CHECKDISPLAYASICON ?
		DVASPECT_ICON : DVASPECT_CONTENT; }
_AFXODLGS_INLINE HGLOBAL COleConvertDialog::GetIconicMetafile() const
	{ ASSERT_VALID(this); return m_cv.hMetaPict; }
_AFXODLGS_INLINE DVASPECT COleConvertDialog::GetDrawAspect() const
	{ ASSERT_VALID(this); return (DVASPECT)m_cv.dvAspect; }
_AFXODLGS_INLINE REFCLSID COleConvertDialog::GetClassID() const
	{ ASSERT_VALID(this); return m_cv.clsidNew; }
_AFXODLGS_INLINE HGLOBAL COleChangeIconDialog::GetIconicMetafile() const
	{ ASSERT_VALID(this); return m_ci.hMetaPict; }
_AFXODLGS_INLINE int COlePasteSpecialDialog::GetPasteIndex() const
	{ ASSERT_VALID(this); return m_ps.nSelectedIndex; }
_AFXODLGS_INLINE DVASPECT COlePasteSpecialDialog::GetDrawAspect() const
	{ ASSERT_VALID(this); return m_ps.dwFlags & PSF_CHECKDISPLAYASICON ?
		DVASPECT_ICON : DVASPECT_CONTENT; }
_AFXODLGS_INLINE HGLOBAL COlePasteSpecialDialog::GetIconicMetafile() const
	{ ASSERT_VALID(this); return m_ps.hMetaPict; }
_AFXODLGS_INLINE UINT COleBusyDialog::GetSelectionType() const
	{ ASSERT_VALID(this); return m_selection; }

_AFXODLGS_INLINE BOOL COleChangeSourceDialog::IsValidSource()
	{ return m_cs.dwFlags & CSF_VALIDSOURCE; }
_AFXODLGS_INLINE CString COleChangeSourceDialog::GetDisplayName()
	{ return m_cs.lpszDisplayName; }
_AFXODLGS_INLINE CString COleChangeSourceDialog::GetFileName()
	{ return CString(m_cs.lpszDisplayName, m_cs.nFileLength); }
_AFXODLGS_INLINE CString COleChangeSourceDialog::GetItemName()
	{ return m_cs.lpszDisplayName+m_cs.nFileLength; }
_AFXODLGS_INLINE CString COleChangeSourceDialog::GetFromPrefix()
	{ return m_cs.lpszFrom; }
_AFXODLGS_INLINE CString COleChangeSourceDialog::GetToPrefix()
	{ return m_cs.lpszTo; }

#endif //_AFXODLGS_INLINE

/////////////////////////////////////////////////////////////////////////////
// OLE Server inlines

#ifdef _AFXOLESVR_INLINE

// COleServerItem
_AFXOLESVR_INLINE COleServerDoc* COleServerItem::GetDocument() const
	{ ASSERT_VALID(this); return (COleServerDoc*)m_pDocument; }
_AFXOLESVR_INLINE void COleServerItem::NotifyChanged(DVASPECT nDrawAspect)
	{ ASSERT_VALID(this); NotifyClient(OLE_CHANGED, nDrawAspect); }
_AFXOLESVR_INLINE const CString& COleServerItem::GetItemName() const
	{ ASSERT_VALID(this); return m_strItemName; }
_AFXOLESVR_INLINE void COleServerItem::SetItemName(LPCTSTR lpszItemName)
{
	ASSERT_VALID(this);
	ASSERT(lpszItemName != NULL);
	ASSERT(AfxIsValidString(lpszItemName));
	m_strItemName = lpszItemName;
}
_AFXOLESVR_INLINE BOOL COleServerItem::IsLinkedItem() const
	{ ASSERT_VALID(this); return GetDocument()->m_pEmbeddedItem != this; }
_AFXOLESVR_INLINE COleDataSource* COleServerItem::GetDataSource()
	{ ASSERT_VALID(this); return &m_dataSource; }

// COleServerDoc
_AFXOLESVR_INLINE void COleServerDoc::NotifyChanged()
	{ ASSERT_VALID(this); NotifyAllItems(OLE_CHANGED, DVASPECT_CONTENT); }
_AFXOLESVR_INLINE void COleServerDoc::NotifyClosed()
	{ ASSERT_VALID(this); NotifyAllItems(OLE_CLOSED, 0); }
_AFXOLESVR_INLINE void COleServerDoc::NotifySaved()
	{ ASSERT_VALID(this); NotifyAllItems(OLE_SAVED, 0); }
_AFXOLESVR_INLINE BOOL COleServerDoc::IsEmbedded() const
	{ ASSERT_VALID(this); return m_bEmbedded; }
_AFXOLESVR_INLINE BOOL COleServerDoc::IsDocObject() const
	{ ASSERT_VALID(this); return m_pDocObjectServer != NULL; }
_AFXOLESVR_INLINE BOOL COleServerDoc::IsInPlaceActive() const
	{ ASSERT_VALID(this); return m_pInPlaceFrame != NULL; }
#endif //_AFXOLESVR_INLINE

/////////////////////////////////////////////////////////////////////////////
