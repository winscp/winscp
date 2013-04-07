// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#ifndef _AFX_NO_OCC_SUPPORT

#include <oledb.h>
#include "olebind.h"
#include "ocdbid.h"
#include "ocdb.h"

// MFC doesn't use the OLEDB 1.5 features of ATL and it causes
// compile problems, so just make ATL think it's not version 1.5
#if (OLEDBVER >= 0x0150)
#undef OLEDBVER
#define OLEDBVER 0x0100
#endif

#define AtlTrace AfxTrace
#include "atldbcli.h"

class CDataSourceControl;
class CDataBoundProperty;

// CCmdTarget
	class COleControlContainer;
	class COleControlSite;

class COccManager;
struct _AFX_OCC_DIALOG_INFO;

#define DISPID_DATASOURCE   0x80010001
#define DISPID_DATAFIELD    0x80010002

interface AFX_NOVTABLE IDataSourceListener : public IUnknown
{
public:
	virtual HRESULT STDMETHODCALLTYPE OnDataMemberChanged(BSTR bstrDM) = 0;
	virtual HRESULT STDMETHODCALLTYPE OnDataMemberAdded(BSTR bstrDM) = 0;
	virtual HRESULT STDMETHODCALLTYPE OnDataMemberRemoved(BSTR bstrDM) = 0;
};

interface AFX_NOVTABLE IDataSource : public IUnknown
{
public:
	virtual HRESULT STDMETHODCALLTYPE GetDataMember(BSTR bstrDM, const GUID __RPC_FAR* riid, IUnknown __RPC_FAR* __RPC_FAR* ppunk) = 0;
	virtual HRESULT STDMETHODCALLTYPE GetDataMemberName(long lIndex, BSTR __RPC_FAR *pbstrDM) = 0;
	virtual HRESULT STDMETHODCALLTYPE GetDataMemberCount(long __RPC_FAR *plCount) = 0;
	virtual HRESULT STDMETHODCALLTYPE AddDataSourceListener(IDataSourceListener __RPC_FAR *pDSL) = 0;
	virtual HRESULT STDMETHODCALLTYPE RemoveDataSourceListener(IDataSourceListener __RPC_FAR *pDSL) = 0;
};

/////////////////////////////////////////////////////////////////////////////
// OLE Databinding support class for data sources

interface IRowPosition;

class CDataSourceControl
{
private:
	CDataSourceControl() {};
public:
	struct METAROWTYPE
	{
		DBCOLUMNID idColumnID;
		DWORD dwColumnID;
		LPSTR lpstrName;
		DWORD dwName;
		CPtrList* m_pClientList;
	};

	CDataSourceControl(COleControlSite *pClientSite);
	~CDataSourceControl();
	HRESULT Initialize();
	virtual IUnknown* GetCursor();
	HRESULT GetMetaData();
	virtual void BindProp(COleControlSite* pClientSite, BOOL bBind = TRUE);
	virtual void BindProp(CDataBoundProperty* pProperty, BOOL bBind = TRUE);
	virtual void BindColumns();
	BOOL CopyColumnID(DBCOLUMNID* pcidDst, DBCOLUMNID const *pcidSrc);
	HRESULT GetBoundClientRow();
	virtual HRESULT UpdateControls();
	virtual HRESULT UpdateCursor();
	COleVariant ToVariant(int nCol);

	COleControlSite *m_pClientSite;  // Back ptr to containing site
	ICursorMove* m_pCursorMove;
	ICursorUpdateARow* m_pCursorUpdateARow;
	int m_nColumns;
	METAROWTYPE* m_pMetaRowData;
	CPtrList m_CursorBoundProps;
	void* m_pVarData;
	int m_nBindings;
	DBCOLUMNBINDING *m_pColumnBindings;
	VARIANT* m_pValues;
	BOOL m_bUpdateInProgress;

// OLE/DB stuff
	IDataSource*            m_pDataSource;
	IRowPosition*           m_pRowPosition;
	ATL::CRowset*           m_pRowset;
	ATL::CDynamicAccessor*  m_pDynamicAccessor;

	DWORD m_dwRowsetNotify; // IRowsetNotify cookie
};

/////////////////////////////////////////////////////////////////////////////
// OLE Databinding support class for bound controls

class CDataBoundProperty
{
protected:
	CDataBoundProperty() {};
public:
	CDataBoundProperty(CDataBoundProperty* pLast, DISPID dispid, WORD ctlid);
	~CDataBoundProperty() {};
	void SetClientSite(COleControlSite *pClientSite);
	void SetDSCSite(COleControlSite *pDSCSite);
	void RemoveSource();
	void Notify();
	IUnknown* GetCursor();
	CDataBoundProperty* GetNext();

	COleControlSite *m_pClientSite;  // Back ptr to containing site
	WORD m_ctlid;
	DISPID m_dispid;
	COleControlSite *m_pDSCSite;
	BOOL m_bOwnXferOut;
	BOOL m_bIsDirty;
	CDataBoundProperty* m_pNext;
};

/////////////////////////////////////////////////////////////////////////////
// Control containment helper functions

DLGTEMPLATE* _AfxSplitDialogTemplate(const DLGTEMPLATE* pTemplate,
	CMapWordToPtr* pOleItemMap);

void _AfxZOrderOleControls(CWnd* pWnd, CMapWordToPtr* pOleItemMap);

/////////////////////////////////////////////////////////////////////////////
// COleControlContainer - implementation class

class COleControlContainer : public CCmdTarget
{
public:
// Constructors/destructors
	COleControlContainer(CWnd*  pWnd);
	virtual ~COleControlContainer();

// Operations
	BOOL CreateControl(CWnd* pWndCtrl, REFCLSID clsid,
		LPCTSTR lpszWindowName, DWORD dwStyle, const RECT& rect, UINT nID,
		CFile* pPersist=NULL, BOOL bStorage=FALSE, BSTR bstrLicKey=NULL,
		COleControlSite** ppNewSite=NULL);
   // Overload to allow creation of default-sized controls
	BOOL CreateControl(CWnd* pWndCtrl, REFCLSID clsid,
		LPCTSTR lpszWindowName, DWORD dwStyle, const POINT* ppt,
	  const SIZE* psize, UINT nID, CFile* pPersist=NULL, BOOL bStorage=FALSE,
	  BSTR bstrLicKey=NULL, COleControlSite** ppNewSite=NULL);
	virtual COleControlSite* FindItem(UINT nID) const;
	virtual BOOL GetAmbientProp(COleControlSite* pSite, DISPID dispid,
		VARIANT* pvarResult);
	void CreateOleFont(CFont* pFont);
	void FreezeAllEvents(BOOL bFreeze);
	virtual void ScrollChildren(int dx, int dy);
	virtual void OnUIActivate(COleControlSite* pSite);
	virtual void OnUIDeactivate(COleControlSite* pSite);

	virtual void CheckDlgButton(int nIDButton, UINT nCheck);
	virtual void CheckRadioButton(int nIDFirstButton, int nIDLastButton,
		int nIDCheckButton);
	virtual CWnd* GetDlgItem(int nID) const;
	virtual void GetDlgItem(int nID, HWND* phWnd) const;
	virtual UINT GetDlgItemInt(int nID, BOOL* lpTrans, BOOL bSigned) const;
	virtual int GetDlgItemText(int nID, LPTSTR lpStr, int nMaxCount) const;
	virtual LRESULT SendDlgItemMessage(int nID, UINT message, WPARAM wParam,
		LPARAM lParam);
	virtual void SetDlgItemInt(int nID, UINT nValue, BOOL bSigned);
	virtual void SetDlgItemText(int nID, LPCTSTR lpszString);
	virtual UINT IsDlgButtonChecked(int nIDButton) const;
#ifndef _AFXDLL
	virtual void AttachControlSite(CWnd* pWnd);
#else
	void AttachControlSite(CWnd* pWnd);
#endif

// Attributes
	CWnd* m_pWnd;
	CMapPtrToPtr m_siteMap;
	COLORREF m_crBack;
	COLORREF m_crFore;
	LPFONTDISP m_pOleFont;
	COleControlSite* m_pSiteUIActive;

public:
	// Interface maps
	BEGIN_INTERFACE_PART(OleIPFrame, IOleInPlaceFrame)
		INIT_INTERFACE_PART(COleControlContainer, OleIPFrame)
		STDMETHOD(GetWindow)(HWND*);
		STDMETHOD(ContextSensitiveHelp)(BOOL);
		STDMETHOD(GetBorder)(LPRECT);
		STDMETHOD(RequestBorderSpace)(LPCBORDERWIDTHS);
		STDMETHOD(SetBorderSpace)(LPCBORDERWIDTHS);
		STDMETHOD(SetActiveObject)(LPOLEINPLACEACTIVEOBJECT, LPCOLESTR);
		STDMETHOD(InsertMenus)(HMENU, LPOLEMENUGROUPWIDTHS);
		STDMETHOD(SetMenu)(HMENU, HOLEMENU, HWND);
		STDMETHOD(RemoveMenus)(HMENU);
		STDMETHOD(SetStatusText)(LPCOLESTR);
		STDMETHOD(EnableModeless)(BOOL);
		STDMETHOD(TranslateAccelerator)(LPMSG, WORD);
	END_INTERFACE_PART(OleIPFrame)

	BEGIN_INTERFACE_PART(OleContainer, IOleContainer)
		INIT_INTERFACE_PART(COleControlContainer, OleContainer)
		STDMETHOD(ParseDisplayName)(LPBINDCTX, LPOLESTR, ULONG*, LPMONIKER*);
		STDMETHOD(EnumObjects)(DWORD, LPENUMUNKNOWN*);
		STDMETHOD(LockContainer)(BOOL);
	END_INTERFACE_PART(OleContainer)

	DECLARE_INTERFACE_MAP()
	DECLARE_DISPATCH_MAP()
};


/////////////////////////////////////////////////////////////////////////////
// COleControlSite - implementation class

#define VT_MFCFORCEPUTREF   0x8000  // force DISPATCH_PROPERTYPUTREF

class COleControlSite : public CCmdTarget
{
public:
// Constructors/destructors
	COleControlSite(COleControlContainer* pCtrlCont);
	~COleControlSite();

// Operations
	HRESULT CreateControl(CWnd* pWndCtrl, REFCLSID clsid,
		LPCTSTR lpszWindowName, DWORD dwStyle, const RECT& rect, UINT nID,
		CFile* pPersist=NULL, BOOL bStorage=FALSE, BSTR bstrLicKey=NULL);
   // Overload to allow creation of default-sized controls
	HRESULT CreateControl(CWnd* pWndCtrl, REFCLSID clsid,
		LPCTSTR lpszWindowName, DWORD dwStyle, const POINT* ppt,
	  const SIZE* psize, UINT nID, CFile* pPersist=NULL, BOOL bStorage=FALSE,
	  BSTR bstrLicKey=NULL);
	virtual BOOL DestroyControl();
	UINT GetID();
	BOOL GetEventIID(IID* piid);
	virtual HRESULT DoVerb(LONG nVerb, LPMSG lpMsg = NULL);
	BOOL IsDefaultButton();
	DWORD GetDefBtnCode();
	void SetDefaultButton(BOOL bDefault);
	void GetControlInfo();
	BOOL IsMatchingMnemonic(LPMSG lpMsg);
	void SendMnemonic(LPMSG lpMsg);
	void FreezeEvents(BOOL bFreeze);

	virtual void InvokeHelperV(DISPID dwDispID, WORD wFlags, VARTYPE vtRet,
		void* pvRet, const BYTE* pbParamInfo, va_list argList);
	virtual void SetPropertyV(DISPID dwDispID, VARTYPE vtProp, va_list argList);
	virtual void AFX_CDECL InvokeHelper(DISPID dwDispID, WORD wFlags, VARTYPE vtRet,
		void* pvRet, const BYTE* pbParamInfo, ...);
	virtual void GetProperty(DISPID dwDispID, VARTYPE vtProp, void* pvProp) const;
	virtual void AFX_CDECL SetProperty(DISPID dwDispID, VARTYPE vtProp, ...);
	virtual BOOL AFX_CDECL SafeSetProperty(DISPID dwDispID, VARTYPE vtProp, ...);

	virtual DWORD GetStyle() const;
	virtual DWORD GetExStyle() const;
	virtual BOOL ModifyStyle(DWORD dwRemove, DWORD dwAdd, UINT nFlags);
	virtual BOOL ModifyStyleEx(DWORD dwRemove, DWORD dwAdd, UINT nFlags);
	virtual void SetWindowText(LPCTSTR lpszString);
	virtual void GetWindowText(CString& str) const;
	virtual int GetWindowText(LPTSTR lpszStringBuf, int nMaxCount) const;
	virtual int GetWindowTextLength() const;
	virtual int GetDlgCtrlID() const;
	virtual int SetDlgCtrlID(int nID);
	virtual void MoveWindow(int x, int y, int nWidth, int nHeight,
		BOOL bRepaint);
	virtual BOOL SetWindowPos(const CWnd* pWndInsertAfter, int x, int y,
		int cx, int cy, UINT nFlags);
	virtual BOOL ShowWindow(int nCmdShow);
	virtual BOOL IsWindowEnabled() const;
	virtual BOOL EnableWindow(BOOL bEnable);
	virtual CWnd* SetFocus();
	virtual void EnableDSC();
	virtual void BindDefaultProperty(DISPID dwDispID, VARTYPE vtProp, LPCTSTR szFieldName, CWnd* pDSCWnd);
	virtual void BindProperty(DISPID dwDispId, CWnd* pWndDSC);

// Overridables
	virtual BOOL QuickActivate();

// Attributes
	COleControlContainer* m_pCtrlCont;
	HWND m_hWnd;
	CWnd* m_pWndCtrl;
	UINT m_nID;
	CRect m_rect;
	IID m_iidEvents;
	LPOLEOBJECT m_pObject;
	LPOLEINPLACEOBJECT m_pInPlaceObject;
	LPOLEINPLACEACTIVEOBJECT m_pActiveObject;
	COleDispatchDriver m_dispDriver;
	DWORD m_dwEventSink;
	DWORD m_dwPropNotifySink;
	DWORD m_dwStyleMask;
	DWORD m_dwStyle;
	DWORD m_dwMiscStatus;
	CONTROLINFO m_ctlInfo;

	// Databound control stuff
	DWORD m_dwNotifyDBEvents; // INotifyDBEvents sink cookie
	CDataSourceControl* m_pDataSourceControl;
	CDataBoundProperty* m_pBindings;
	union {
		COleControlSite *m_pDSCSite;
		WORD m_ctlidRowSource;
	};
	DISPID m_defdispid;
	UINT m_dwType;
	CString m_strDataField;
	BOOL m_bIgnoreNotify;
	BOOL m_bIsDirty;
	VARIANT m_varResult;

protected:
// Implementation
	BOOL SetExtent();
	HRESULT CreateOrLoad(REFCLSID clsid, CFile* pPersist, BOOL bStorage,
		BSTR bstrLicKey);
	DWORD ConnectSink(REFIID iid, LPUNKNOWN punkSink);
	void DisconnectSink(REFIID iid, DWORD dwCookie);
	void AttachWindow();
	void DetachWindow();
	BOOL OnEvent(AFX_EVENT* pEvent);
	HRESULT GetCursor(DISPID dispid, LPUNKNOWN* ppcursorOut, LPVOID *ppcidOut);

public:
// Interface maps
	BEGIN_INTERFACE_PART(OleClientSite, IOleClientSite)
		INIT_INTERFACE_PART(COleControlSite, OleClientSite)
		STDMETHOD(SaveObject)();
		STDMETHOD(GetMoniker)(DWORD, DWORD, LPMONIKER*);
		STDMETHOD(GetContainer)(LPOLECONTAINER*);
		STDMETHOD(ShowObject)();
		STDMETHOD(OnShowWindow)(BOOL);
		STDMETHOD(RequestNewObjectLayout)();
	END_INTERFACE_PART(OleClientSite)

	BEGIN_INTERFACE_PART(OleIPSite, IOleInPlaceSite)
		INIT_INTERFACE_PART(COleControlSite, OleIPSite)
		STDMETHOD(GetWindow)(HWND*);
		STDMETHOD(ContextSensitiveHelp)(BOOL);
		STDMETHOD(CanInPlaceActivate)();
		STDMETHOD(OnInPlaceActivate)();
		STDMETHOD(OnUIActivate)();
		STDMETHOD(GetWindowContext)(LPOLEINPLACEFRAME*,
			LPOLEINPLACEUIWINDOW*, LPRECT, LPRECT, LPOLEINPLACEFRAMEINFO);
		STDMETHOD(Scroll)(SIZE);
		STDMETHOD(OnUIDeactivate)(BOOL);
		STDMETHOD(OnInPlaceDeactivate)();
		STDMETHOD(DiscardUndoState)();
		STDMETHOD(DeactivateAndUndo)();
		STDMETHOD(OnPosRectChange)(LPCRECT);
	END_INTERFACE_PART(OleIPSite)

	BEGIN_INTERFACE_PART(OleControlSite, IOleControlSite)
		INIT_INTERFACE_PART(COleControlSite, OleControlSite)
		STDMETHOD(OnControlInfoChanged)();
		STDMETHOD(LockInPlaceActive)(BOOL fLock);
		STDMETHOD(GetExtendedControl)(LPDISPATCH* ppDisp);
		STDMETHOD(TransformCoords)(POINTL* lpptlHimetric,
			POINTF* lpptfContainer, DWORD flags);
		STDMETHOD(TranslateAccelerator)(LPMSG lpMsg, DWORD grfModifiers);
		STDMETHOD(OnFocus)(BOOL fGotFocus);
		STDMETHOD(ShowPropertyFrame)();
	END_INTERFACE_PART(OleControlSite)

	BEGIN_INTERFACE_PART(AmbientProps, IDispatch)
		INIT_INTERFACE_PART(COleControlSite, AmbientProps)
		STDMETHOD(GetTypeInfoCount)(unsigned int*);
		STDMETHOD(GetTypeInfo)(unsigned int, LCID, ITypeInfo**);
		STDMETHOD(GetIDsOfNames)(REFIID, LPOLESTR*, unsigned int, LCID, DISPID*);
		STDMETHOD(Invoke)(DISPID, REFIID, LCID, unsigned short, DISPPARAMS*,
						  VARIANT*, EXCEPINFO*, unsigned int*);
	END_INTERFACE_PART(AmbientProps)

	BEGIN_INTERFACE_PART(PropertyNotifySink, IPropertyNotifySink)
		INIT_INTERFACE_PART(COleControlSite, PropertyNotifySink)
		STDMETHOD(OnChanged)(DISPID dispid);
		STDMETHOD(OnRequestEdit)(DISPID dispid);
	END_INTERFACE_PART(PropertyNotifySink)

	BEGIN_INTERFACE_PART(EventSink, IDispatch)
		INIT_INTERFACE_PART(COleControlSite, EventSink)
		STDMETHOD(GetTypeInfoCount)(unsigned int*);
		STDMETHOD(GetTypeInfo)(unsigned int, LCID, ITypeInfo**);
		STDMETHOD(GetIDsOfNames)(REFIID, LPOLESTR*, unsigned int, LCID, DISPID*);
		STDMETHOD(Invoke)(DISPID, REFIID, LCID, unsigned short, DISPPARAMS*,
						  VARIANT*, EXCEPINFO*, unsigned int*);
	END_INTERFACE_PART(EventSink)

	BEGIN_INTERFACE_PART(BoundObjectSite, IBoundObjectSite)
		STDMETHOD(GetCursor)(DISPID dispid, LPLPCURSOR ppcursorOut, LPVOID *ppcidOut);
	END_INTERFACE_PART(BoundObjectSite)

	BEGIN_INTERFACE_PART(NotifyDBEvents, INotifyDBEvents)
		STDMETHOD(OKToDo)(DWORD dwEventWhat, ULONG cReasons, DBNOTIFYREASON rgReasons[]);
		STDMETHOD(Cancelled)(DWORD dwEventWhat, ULONG cReasons, DBNOTIFYREASON rgReasons[]);
		STDMETHOD(SyncBefore)(DWORD dwEventWhat, ULONG cReasons, DBNOTIFYREASON rgReasons[]);
		STDMETHOD(AboutToDo)(DWORD dwEventWhat, ULONG cReasons, DBNOTIFYREASON rgReasons[]);
		STDMETHOD(FailedToDo)(DWORD dwEventWhat, ULONG cReasons, DBNOTIFYREASON rgReasons[]);
		STDMETHOD(SyncAfter)(DWORD dwEventWhat, ULONG cReasons, DBNOTIFYREASON rgReasons[]);
		STDMETHOD(DidEvent)(DWORD dwEventWhat, ULONG cReasons, DBNOTIFYREASON rgReasons[]);

		// Not part of i/f - just a helper
		HRESULT FireEvent(DWORD dwEventWhat, ULONG cReasons,
			DBNOTIFYREASON rgReasons[], DSCSTATE nState);
	END_INTERFACE_PART(NotifyDBEvents)

	BEGIN_INTERFACE_PART(RowsetNotify, IRowsetNotify)
		STDMETHOD(OnFieldChange)(IRowset* pRowset, HROW hRow, ULONG cColumns, ULONG rgColumns[], DBREASON eReason, DBEVENTPHASE ePhase, BOOL fCantDeny);
		STDMETHOD(OnRowChange)(IRowset* pRowset, ULONG cRows, const HROW rghRows[], DBREASON eReason, DBEVENTPHASE ePhase, BOOL fCantDeny);
		STDMETHOD(OnRowsetChange)(IRowset* pRowset, DBREASON eReason, DBEVENTPHASE ePhase, BOOL fCantDeny);
	END_INTERFACE_PART(RowsetNotify)

	DECLARE_INTERFACE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// OLE control container manager

class COccManager : public CNoTrackObject
{
// Operations
public:
	// Event handling
	virtual BOOL OnEvent(CCmdTarget* pCmdTarget, UINT idCtrl, AFX_EVENT* pEvent,
		AFX_CMDHANDLERINFO* pHandlerInfo);

	// Internal object creation
	virtual COleControlContainer* CreateContainer(CWnd* pWnd);
	virtual COleControlSite* CreateSite(COleControlContainer* pCtrlCont);

	// Dialog creation
	virtual const DLGTEMPLATE* PreCreateDialog(_AFX_OCC_DIALOG_INFO* pOccDialogInfo,
		const DLGTEMPLATE* pOrigTemplate);
	virtual void PostCreateDialog(_AFX_OCC_DIALOG_INFO* pOccDialogInfo);
	virtual DLGTEMPLATE* SplitDialogTemplate(const DLGTEMPLATE* pTemplate,
		DLGITEMTEMPLATE** ppOleDlgItems);
	virtual BOOL CreateDlgControls(CWnd* pWndParent, LPCTSTR lpszResourceName,
		_AFX_OCC_DIALOG_INFO* pOccDialogInfo);
	virtual BOOL CreateDlgControls(CWnd* pWndParent, void* lpResource,
		_AFX_OCC_DIALOG_INFO* pOccDialogInfo);

	// Dialog manager
	virtual BOOL IsDialogMessage(CWnd* pWndDlg, LPMSG lpMsg);
	static BOOL AFX_CDECL IsLabelControl(CWnd* pWnd);
	static BOOL AFX_CDECL IsMatchingMnemonic(CWnd* pWnd, LPMSG lpMsg);
	static void AFX_CDECL SetDefaultButton(CWnd* pWnd, BOOL bDefault);
	static DWORD AFX_CDECL GetDefBtnCode(CWnd* pWnd);

// Implementation
protected:
	// Dialog creation
	HWND CreateDlgControl(CWnd* pWndParent, HWND hwAfter, BOOL bDialogEx,
		LPDLGITEMTEMPLATE pDlgItem, WORD nMsg, BYTE* lpData, DWORD cb);

	// Databinding
	void BindControls(CWnd* pWndParent);

	// Dialog manager
	static void AFX_CDECL UIActivateControl(CWnd* pWndNewFocus);
	static void AFX_CDECL UIDeactivateIfNecessary(CWnd* pWndOldFocus, CWnd* pWndNewFocus);
};

struct _AFX_OCC_DIALOG_INFO
{
	DLGTEMPLATE* m_pNewTemplate;
	DLGITEMTEMPLATE** m_ppOleDlgItems;
};

#endif // !_AFX_NO_OCC_SUPPORT
