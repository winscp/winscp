// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Inlines for AFXCTL.H

#ifdef _AFXCTL_INLINE

/////////////////////////////////////////////////////////////////////////////

// COleControl inlines
_AFXCTL_INLINE BOOL COleControl::IsOptimizedDraw()
	{ return m_bOptimizedDraw; }
_AFXCTL_INLINE BOOL COleControl::IsConvertingVBX()
	{ return m_bConvertVBX; }
_AFXCTL_INLINE void COleControl::FireKeyDown(USHORT* pnChar, short nShiftState)
	{ FireEvent(DISPID_KEYDOWN, EVENT_PARAM(VTS_PI2 VTS_I2), pnChar,
		nShiftState); }
_AFXCTL_INLINE void COleControl::FireKeyUp(USHORT* pnChar, short nShiftState)
	{ FireEvent(DISPID_KEYUP, EVENT_PARAM(VTS_PI2 VTS_I2), pnChar,
		nShiftState); }
_AFXCTL_INLINE void COleControl::FireKeyPress(USHORT* pnChar)
	{ FireEvent(DISPID_KEYPRESS, EVENT_PARAM(VTS_PI2), pnChar); }
_AFXCTL_INLINE void COleControl::FireMouseDown(short nButton,
		short nShiftState, OLE_XPOS_PIXELS x, OLE_YPOS_PIXELS y)
	{ FireEvent(DISPID_MOUSEDOWN,
		EVENT_PARAM(VTS_I2 VTS_I2 VTS_XPOS_PIXELS VTS_YPOS_PIXELS),
		nButton, nShiftState, x, y); }
_AFXCTL_INLINE void COleControl::FireMouseUp(short nButton,
		short nShiftState, OLE_XPOS_PIXELS x, OLE_YPOS_PIXELS y)
	{ FireEvent(DISPID_MOUSEUP,
		EVENT_PARAM(VTS_I2 VTS_I2 VTS_XPOS_PIXELS VTS_YPOS_PIXELS),
		nButton, nShiftState, x, y); }
_AFXCTL_INLINE void COleControl::FireMouseMove(short nButton,
		short nShiftState, OLE_XPOS_PIXELS x, OLE_YPOS_PIXELS y)
	{ FireEvent(DISPID_MOUSEMOVE,
		EVENT_PARAM(VTS_I2 VTS_I2 VTS_XPOS_PIXELS VTS_YPOS_PIXELS),
		nButton, nShiftState, x, y); }
_AFXCTL_INLINE void COleControl::FireClick()
	{ FireEvent(DISPID_CLICK, EVENT_PARAM(VTS_NONE)); }
_AFXCTL_INLINE void COleControl::FireDblClick()
	{ FireEvent(DISPID_DBLCLICK, EVENT_PARAM(VTS_NONE)); }
_AFXCTL_INLINE void COleControl::FireReadyStateChange()
	{ FireEvent(DISPID_READYSTATECHANGE, EVENT_PARAM(VTS_I4), m_lReadyState); }
_AFXCTL_INLINE void COleControl::InternalSetReadyState(long lNewReadyState)
	{ ASSERT((lNewReadyState >=0) && (lNewReadyState <= READYSTATE_COMPLETE));
	  if (m_lReadyState != lNewReadyState)
		{m_lReadyState = lNewReadyState; FireReadyStateChange(); } }
_AFXCTL_INLINE BOOL COleControl::ExchangeVersion(
	CPropExchange* pPX, DWORD dwVersionDefault, BOOL bConvert)
	{ return pPX->ExchangeVersion(m_dwVersionLoaded, dwVersionDefault, bConvert); }
_AFXCTL_INLINE DWORD COleControl::GetStockEventMask() const
	{ return *GetEventMap()->lpStockEventMask; }
_AFXCTL_INLINE DWORD COleControl::GetStockPropMask() const
	{ return *GetDispatchMap()->lpStockPropMask; }
_AFXCTL_INLINE void COleControl::RequestAsynchronousExchange(DWORD dwVersion)
	{ m_bDataPathPropertiesLoaded = FALSE; m_dwDataPathVersionToReport = dwVersion; }

// CPropExchange inlines
_AFXCTL_INLINE CPropExchange::CPropExchange() : m_dwVersion(0), m_bAsync(FALSE)
	{ }
_AFXCTL_INLINE BOOL CPropExchange::IsLoading()
	{ return m_bLoading; }
_AFXCTL_INLINE DWORD CPropExchange::GetVersion()
	{ return m_dwVersion; }
_AFXCTL_INLINE BOOL CPropExchange::IsAsynchronous()
	{ return m_bAsync; }

// CDataPathProperty inlines
_AFXCTL_INLINE CDataPathProperty::CDataPathProperty(COleControl* pControl)
	: m_pControl(pControl) {}
_AFXCTL_INLINE CDataPathProperty::CDataPathProperty(LPCTSTR lpszPath, COleControl* pControl)
	: m_pControl(pControl), m_strPath(lpszPath) {}
_AFXCTL_INLINE void CDataPathProperty::SetPath(LPCTSTR lpszPath)
	{ ASSERT_VALID(this); m_strPath = lpszPath; }
_AFXCTL_INLINE CString CDataPathProperty::GetPath() const
	{ ASSERT_VALID(this); return m_strPath; }
_AFXCTL_INLINE COleControl* CDataPathProperty::GetControl()
	{ ASSERT_VALID(this); return m_pControl; }
_AFXCTL_INLINE void CDataPathProperty::SetControl(COleControl* pControl)
	{ ASSERT_VALID(this); m_pControl=pControl; }

// CCachedDataPathProperty inlines
_AFXCTL_INLINE CCachedDataPathProperty::CCachedDataPathProperty(COleControl* pControl)
	: CDataPathProperty(pControl) {}
_AFXCTL_INLINE CCachedDataPathProperty::CCachedDataPathProperty(LPCTSTR lpszPath, COleControl* pControl)
	: CDataPathProperty(lpszPath, pControl) {}

// inline DDP_ routines
_AFXCTL_INLINE void AFXAPI DDP_LBString(CDataExchange* pDX, int id,
		CString& member, LPCTSTR pszPropName)
	{ DDP_Text(pDX, id, member, pszPropName); }
_AFXCTL_INLINE void AFXAPI DDP_LBStringExact(CDataExchange* pDX, int id,
		CString& member, LPCTSTR pszPropName)
	{ DDP_Text(pDX, id, member, pszPropName); }
_AFXCTL_INLINE void AFXAPI DDP_LBIndex(CDataExchange* pDX, int id,
		int& member, LPCTSTR pszPropName)
	{ DDP_Text(pDX, id, member, pszPropName); }
_AFXCTL_INLINE void AFXAPI DDP_CBString(CDataExchange* pDX, int id,
		CString& member, LPCTSTR pszPropName)
	{ DDP_Text(pDX, id, member, pszPropName); }
_AFXCTL_INLINE void AFXAPI DDP_CBStringExact(CDataExchange* pDX, int id,
		CString& member, LPCTSTR pszPropName)
	{ DDP_Text(pDX, id, member, pszPropName); }
_AFXCTL_INLINE void AFXAPI DDP_CBIndex(CDataExchange* pDX, int id,
		int& member, LPCTSTR pszPropName)
	{ DDP_Text(pDX, id, member, pszPropName); }

#endif //_AFXCTL_INLINE
