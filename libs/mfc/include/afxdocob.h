// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1996-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#ifndef __AFXDOCOB_H__
#define __AFXDOCOB_H__

//WINBUG: these error codes are needed by MFC's IPrint implementation
// but aren't available in the SDK headers.  Someday, these #define's
// can be removed.

#ifndef PRINT_E_CANCELLED
#define PRINT_E_CANCELLED 0x80040160L
#endif
#ifndef PRINT_E_NOSUCHPAGE
#define PRINT_E_NOSUCHPAGE 0x80040161L
#endif

#ifdef _AFX_NO_OLE_SUPPORT
	#error OLE classes not supported in this library variant.
#endif

#ifndef __AFXOLE_H__
	#include <afxole.h>
#endif

#ifndef __docobj_h__
	#include <docobj.h>     // defines Document Object interfaces
#endif

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, off)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, on)
#endif

#ifdef _AFX_PACKING
#pragma pack(push, _AFX_PACKING)
#endif

// AFXDLL support
#undef AFX_DATA
#define AFX_DATA AFX_OLE_DATA


/////////////////////////////////////////////////////////////////////////////
// AFXDOCOB.H - MFC OLE Document Object support

//CCmdUI
class COleCmdUI;

//CCmdTarg
class CDocObjectServer;

//COleIPFrameWnd
class COleDocIPFrameWnd;


/////////////////////////////////////////////////////////////////////////////
// COleCmdUI

class COleCmdUI : public CCmdUI
{
public:
	COleCmdUI(OLECMD* rgCmds, ULONG cCmds, const GUID* m_pGroup);
	virtual void Enable(BOOL bOn);
	virtual void SetCheck(int nCheck);
	virtual void SetText(LPCTSTR lpszText);
	virtual BOOL DoUpdate(CCmdTarget* pTarget, BOOL bDisableIfNoHandler);
	DWORD m_nCmdTextFlag;

protected:
	OLECMD* m_rgCmds;
	const GUID* m_pguidCmdGroup;
public:
	CString m_strText;

	friend class CCmdTarget;
};


#define ON_OLECMD(pguid, olecmdid, id) \
	{ pguid, (ULONG)olecmdid, (UINT)id },

#define ON_OLECMD_OPEN() \
	ON_OLECMD(NULL, OLECMDID_OPEN, ID_FILE_OPEN)

#define ON_OLECMD_NEW() \
	ON_OLECMD(NULL, OLECMDID_NEW, ID_FILE_NEW)

#define ON_OLECMD_SAVE() \
	ON_OLECMD(NULL, OLECMDID_SAVE, ID_FILE_SAVE)

#define ON_OLECMD_SAVE_AS() \
	ON_OLECMD(NULL, OLECMDID_SAVEAS, ID_FILE_SAVE_AS)

#define ON_OLECMD_SAVE_COPY_AS() \
	ON_OLECMD(NULL, OLECMDID_SAVECOPYAS, ID_FILE_SAVE_COPY_AS)

#define ON_OLECMD_PRINT() \
	ON_OLECMD(NULL, OLECMDID_PRINT, ID_FILE_PRINT)

#define ON_OLECMD_PRINTPREVIEW() \
	ON_OLECMD(NULL, OLECMDID_PRINTPREVIEW, ID_FILE_PRINT_PREVIEW)

#define ON_OLECMD_PAGESETUP() \
	ON_OLECMD(NULL, OLECMDID_PAGESETUP, ID_FILE_PAGE_SETUP)

#define ON_OLECMD_CUT() \
	ON_OLECMD(NULL, OLECMDID_CUT, ID_EDIT_CUT)

#define ON_OLECMD_COPY() \
	ON_OLECMD(NULL, OLECMDID_COPY, ID_EDIT_COPY)

#define ON_OLECMD_PASTE() \
	ON_OLECMD(NULL, OLECMDID_PASTE, ID_EDIT_PASTE)

#define ON_OLECMD_PASTESPECIAL() \
	ON_OLECMD(NULL, OLECMDID_PASTESPECIAL, ID_EDIT_PASTE_SPECIAL)

#define ON_OLECMD_UNDO() \
	ON_OLECMD(NULL, OLECMDID_UNDO, ID_EDIT_UNDO)

#define ON_OLECMD_REDO() \
	ON_OLECMD(NULL, OLECMDID_REDO, ID_EDIT_REDO)

#define ON_OLECMD_SELECTALL() \
	ON_OLECMD(NULL, OLECMDID_SELECTALL, ID_EDIT_SELECT_ALL)

#define ON_OLECMD_CLEARSELECTION() \
	ON_OLECMD(NULL, OLECMDID_CLEARSELECTION, ID_EDIT_CLEAR)


/////////////////////////////////////////////////////////////////////////////
// CDocObjectServer class

class CDocObjectServer : public CCmdTarget
{
	DECLARE_DYNAMIC(CDocObjectServer)

// Constructors
public:
	CDocObjectServer(COleServerDoc* pOwner,
			LPOLEDOCUMENTSITE pDocSite = NULL);

// Attributes
public:

// Operations
public:
   void ActivateDocObject();

// Overridables
protected:
   // Document Overridables

   // View Overridables
   virtual void OnApplyViewState(CArchive& ar);
   virtual void OnSaveViewState(CArchive& ar);
   virtual HRESULT OnActivateView();

// Implementation
public:
	virtual ~CDocObjectServer();
	void ReleaseDocSite();
	void SetDocSite(LPOLEDOCUMENTSITE pNewSite);
	COleDocIPFrameWnd* GetControllingFrame() const;
protected:
	STDMETHODIMP OnExecOleCmd(const GUID* pguidCmdGroup,
		DWORD nCmdID, DWORD nCmdExecOpt, VARIANTARG* pvarargIn,
		VARIANTARG* pvarargOut);
	BOOL DoPreparePrinting(CView* pView, CPrintInfo* printInfo);
	void DoPrepareDC(CView* pView, CDC* pdcPrint, CPrintInfo* pprintInfo);
	void DoPrint(CView* pView, CDC* pdcPrint, CPrintInfo* pprintInfo);
	void DoBeginPrinting(CView* pView, CDC* pDC, CPrintInfo* pprintInfo);
	void DoEndPrinting(CView* pView, CDC* pDC, CPrintInfo* pprintInfo);

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

   // Overrides
protected:
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDocObjectServer)
	public:
	virtual void OnCloseDocument();
	//}}AFX_VIRTUAL

	// Implementation Data
protected:
	// Document Data
	LPOLEDOCUMENTSITE m_pDocSite;
	COleServerDoc* m_pOwner;

	// Print Data
	LONG m_nFirstPage;
	LPCONTINUECALLBACK m_pContinueCallback;

	// View Data
	LPOLEINPLACESITE  m_pViewSite;

   // Implementation Helpers
protected:
	void OnSetItemRects(LPRECT lprcPosRect, LPRECT lprcClipRect);
//  LPUNKNOWN GetInterfaceHook(const void* iid);

	// Generated message map functions
protected:
	//{{AFX_MSG(CDocObjectServer)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

// Interface Maps
public:
	BEGIN_INTERFACE_PART(OleObject, IOleObject)
		INIT_INTERFACE_PART(CDocObjServerDoc, DocOleObject)
		STDMETHOD(SetClientSite)(LPOLECLIENTSITE);
		STDMETHOD(GetClientSite)(LPOLECLIENTSITE*);
		STDMETHOD(SetHostNames)(LPCOLESTR, LPCOLESTR);
		STDMETHOD(Close)(DWORD);
		STDMETHOD(SetMoniker)(DWORD, LPMONIKER);
		STDMETHOD(GetMoniker)(DWORD, DWORD, LPMONIKER*);
		STDMETHOD(InitFromData)(LPDATAOBJECT, BOOL, DWORD);
		STDMETHOD(GetClipboardData)(DWORD, LPDATAOBJECT*);
		STDMETHOD(DoVerb)(LONG, LPMSG, LPOLECLIENTSITE, LONG, HWND, LPCRECT);
		STDMETHOD(EnumVerbs)(IEnumOLEVERB**);
		STDMETHOD(Update)();
		STDMETHOD(IsUpToDate)();
		STDMETHOD(GetUserClassID)(CLSID*);
		STDMETHOD(GetUserType)(DWORD, LPOLESTR*);
		STDMETHOD(SetExtent)(DWORD, LPSIZEL);
		STDMETHOD(GetExtent)(DWORD, LPSIZEL);
		STDMETHOD(Advise)(LPADVISESINK, LPDWORD);
		STDMETHOD(Unadvise)(DWORD);
		STDMETHOD(EnumAdvise)(LPENUMSTATDATA*);
		STDMETHOD(GetMiscStatus)(DWORD, LPDWORD);
		STDMETHOD(SetColorScheme)(LPLOGPALETTE);
	END_INTERFACE_PART(OleObject)

	BEGIN_INTERFACE_PART(OleDocument, IOleDocument)
		INIT_INTERFACE_PART(CDocObjectServer, OleDocument)
		STDMETHOD(CreateView)(LPOLEINPLACESITE, LPSTREAM, DWORD, LPOLEDOCUMENTVIEW*);
		STDMETHOD(GetDocMiscStatus)(LPDWORD);
		STDMETHOD(EnumViews)(LPENUMOLEDOCUMENTVIEWS*, LPOLEDOCUMENTVIEW*);
	END_INTERFACE_PART(OleDocument)

	BEGIN_INTERFACE_PART(OleDocumentView, IOleDocumentView)
		INIT_INTERFACE_PART(CDocObjectServer, OleDocumentView)
		STDMETHOD(SetInPlaceSite)(LPOLEINPLACESITE);
		STDMETHOD(GetInPlaceSite)(LPOLEINPLACESITE*);
		STDMETHOD(GetDocument)(LPUNKNOWN*);
		STDMETHOD(SetRect)(LPRECT);
		STDMETHOD(GetRect)(LPRECT);
		STDMETHOD(SetRectComplex)(LPRECT, LPRECT, LPRECT, LPRECT);
		STDMETHOD(Show)(BOOL);
		STDMETHOD(UIActivate)(BOOL);
		STDMETHOD(Open)();
		STDMETHOD(CloseView)(DWORD);
		STDMETHOD(SaveViewState)(LPSTREAM);
		STDMETHOD(ApplyViewState)(LPSTREAM);
		STDMETHOD(Clone)(LPOLEINPLACESITE, LPOLEDOCUMENTVIEW*);
	END_INTERFACE_PART(OleDocumentView)

	BEGIN_INTERFACE_PART(OleCommandTarget, IOleCommandTarget)
		INIT_INTERFACE_PART(CDocObjectServer, OleCommandTarget)
		STDMETHOD(QueryStatus)(const GUID*, ULONG, OLECMD[], OLECMDTEXT*);
		STDMETHOD(Exec)(const GUID*, DWORD, DWORD, VARIANTARG*, VARIANTARG*);
	END_INTERFACE_PART(OleCommandTarget)

	BEGIN_INTERFACE_PART(Print, IPrint)
		INIT_INTERFACE_PART(CDocObjectServer, Print)
		STDMETHOD(SetInitialPageNum)(LONG);
		STDMETHOD(GetPageInfo)(LPLONG, LPLONG);
		STDMETHOD(Print)(DWORD, DVTARGETDEVICE**, PAGESET**, LPSTGMEDIUM,
				 LPCONTINUECALLBACK, LONG, LPLONG, LPLONG);
	END_INTERFACE_PART(Print)

	DECLARE_INTERFACE_MAP()
};


/////////////////////////////////////////////////////////////////////////////
// COleDocIPFrameWnd class

class COleDocIPFrameWnd : public COleIPFrameWnd
{
	DECLARE_DYNCREATE(COleDocIPFrameWnd)

// Constructors
public:
	COleDocIPFrameWnd();

// Attributes
public:

// Operations
public:

// Overridables
protected:

// Implementation
public:
	virtual ~COleDocIPFrameWnd();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(COleDocIPFrameWnd)
	//}}AFX_VIRTUAL
protected:
	virtual void OnRequestPositionChange(LPCRECT lpRect);
	virtual void RecalcLayout(BOOL bNotify = TRUE);

	// Menu Merging support
	HMENU m_hMenuHelpPopup;
	virtual BOOL BuildSharedMenu();
	virtual void DestroySharedMenu();

	// Generated message map functions
	//{{AFX_MSG(COleDocIPFrameWnd)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


class CDocObjectServerItem : public COleServerItem
{
	DECLARE_DYNAMIC(CDocObjectServerItem)

// Constructors
protected:
	CDocObjectServerItem(COleServerDoc* pServerDoc, BOOL bAutoDelete);

// Attributes
public:
	COleServerDoc* GetDocument() const
		{ return (COleServerDoc*) COleServerItem::GetDocument(); }

// Overridables
public:

// Implementation
public:
	virtual ~CDocObjectServerItem();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

   // Overrides
protected:
   virtual void OnDoVerb(LONG iVerb);
   virtual void OnHide();
   virtual void OnOpen();
   virtual void OnShow();
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDocObjectServerItem)
	//}}AFX_VIRTUAL

};

/////////////////////////////////////////////////////////////////////////////

#ifdef _AFX_PACKING
#pragma pack(pop)
#endif

#undef AFX_DATA
#define AFX_DATA

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, on)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, off)
#endif

#endif //__AFXDOCOB_H__

/////////////////////////////////////////////////////////////////////////////
