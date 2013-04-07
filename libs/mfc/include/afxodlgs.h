// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#ifndef __AFXODLGS_H__
#define __AFXODLGS_H__

#ifdef _AFX_NO_OLE_SUPPORT
	#error OLE classes not supported in this library variant.
#endif

#ifndef __AFXOLE_H__
	#include <afxole.h>
#endif

#ifndef __AFXDLGS_H__
	#include <afxdlgs.h>
#endif

// include OLE dialog/helper APIs
#ifndef _OLEDLG_H_
	#include <oledlg.h>
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

/////////////////////////////////////////////////////////////////////////////
// AFXODLGS.H - MFC OLE dialogs

// Classes declared in this file

//CCommonDialog
	class COleDialog;                   // base class for OLE dialog wrappers
		class COleInsertDialog;         // insert object dialog
		class COleConvertDialog;        // convert dialog
		class COleChangeIconDialog;     // change icon dialog
		class COlePasteSpecialDialog;   // paste special dialog
		class COleLinksDialog;          // edit links dialog
			class COleUpdateDialog;     // update links/embeddings dialog
		class COleBusyDialog;           // used for
		class COlePropertiesDialog;
		class COleChangeSourceDialog;

/////////////////////////////////////////////////////////////////////////////

// AFXDLL support
#undef AFX_DATA
#define AFX_DATA AFX_OLE_DATA

/////////////////////////////////////////////////////////////////////////////
// COleUILinkInfo -- used internally to implement
// IOleUILinkInfo and IOleUILinkContainer
// used by COleLinksDialog and COleChangeSourceDialog

class COleUILinkInfo : public IOleUILinkInfo
{
public:
	COleUILinkInfo(COleDocument* pDocument);

// Implementation
	COleDocument* m_pDocument;          // document being manipulated
	COleClientItem* m_pSelectedItem;    // primary selected item in m_pDocument
	POSITION m_pos;                     // used during link enumeration
	BOOL m_bUpdateLinks;                // update links?
	BOOL m_bUpdateEmbeddings;           // update embeddings?

	STDMETHOD_(ULONG, AddRef)();
	STDMETHOD_(ULONG, Release)();
	STDMETHOD(QueryInterface)(REFIID, LPVOID*);

	// IOleUILinkContainer
	STDMETHOD_(DWORD,GetNextLink)(DWORD);
	STDMETHOD(SetLinkUpdateOptions)(DWORD, DWORD);
	STDMETHOD(GetLinkUpdateOptions)(DWORD, LPDWORD);
	STDMETHOD(SetLinkSource)(DWORD, LPTSTR, ULONG, ULONG*, BOOL);
	STDMETHOD(GetLinkSource)(DWORD, LPTSTR*, ULONG*, LPTSTR*, LPTSTR*, BOOL*,
		BOOL*);
	STDMETHOD(OpenLinkSource)(DWORD);
	STDMETHOD(UpdateLink)(DWORD, BOOL, BOOL);
	STDMETHOD(CancelLink)(DWORD);
	// IOleUILinkInfo
	STDMETHOD(GetLastUpdate)(DWORD dwLink, FILETIME* lpLastUpdate);
};

/////////////////////////////////////////////////////////////////////////////
// Wrappers for OLE UI dialogs

#ifdef _AFXDLL
class COleDialog : public CCommonDialog
#else
class AFX_NOVTABLE COleDialog : public CCommonDialog
#endif
{
	DECLARE_DYNAMIC(COleDialog)

// Attributes
public:
	UINT GetLastError() const;

// Implementation
public:
	int MapResult(UINT nResult);
	COleDialog(CWnd* pParentWnd);
#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	UINT m_nLastError;

protected:
	friend UINT CALLBACK _AfxOleHookProc(HWND, UINT, WPARAM, LPARAM);
};

/////////////////////////////////////////////////////////////////////////////
// COleInsertDialog

class COleInsertDialog : public COleDialog
{
	DECLARE_DYNAMIC(COleInsertDialog)

// Attributes
public:
	OLEUIINSERTOBJECT m_io; // structure for OleUIInsertObject

// Constructors
	COleInsertDialog(DWORD dwFlags = IOF_SELECTCREATENEW,
		CWnd* pParentWnd = NULL);

// Operations
	virtual int DoModal();
	int DoModal(DWORD dwFlags);
	BOOL CreateItem(COleClientItem* pItem);
		// call after DoModal to create item based on dialog data

// Attributes (after DoModal returns IDOK)
	enum Selection { createNewItem, insertFromFile, linkToFile };
	UINT GetSelectionType() const;
		// return type of selection made

	CString GetPathName() const;  // return full path name
	REFCLSID GetClassID() const;    // get class ID of new item

	DVASPECT GetDrawAspect() const;
		// DVASPECT_CONTENT or DVASPECT_ICON
	HGLOBAL GetIconicMetafile() const;
		// returns HGLOBAL to METAFILEPICT struct with iconic data

// Implementation
public:
	virtual ~COleInsertDialog();
#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif

	enum FilterFlags {
		DocObjectsOnly = 1,
		ControlsOnly = 2,
	};

protected:
	TCHAR m_szFileName[_MAX_PATH];
		// contains full path name after return

	void AddClassIDToList(LPCLSID& lpList, int& nListCount,
		int& nBufferLen, LPCLSID pNewID);
};

/////////////////////////////////////////////////////////////////////////////
// COleConvertDialog

class COleConvertDialog : public COleDialog
{
	DECLARE_DYNAMIC(COleConvertDialog)

// Attributes
public:
	OLEUICONVERT m_cv;  // structure for OleUIConvert

// Constructors
	COleConvertDialog(COleClientItem* pItem,
		DWORD dwFlags = CF_SELECTCONVERTTO, CLSID* pClassID = NULL,
		CWnd* pParentWnd = NULL);

// Operations
	virtual int DoModal();
		// just display the dialog and collect convert info
	BOOL DoConvert(COleClientItem* pItem);
		// do the conversion on pItem (after DoModal == IDOK)

// Attributes (after DoModal returns IDOK)
	enum Selection { noConversion, convertItem, activateAs };
	UINT GetSelectionType() const;

	HGLOBAL GetIconicMetafile() const;  // will return NULL if same as before
	REFCLSID GetClassID() const;    // get class ID to convert or activate as
	DVASPECT GetDrawAspect() const; // get new draw aspect

// Implementation
public:
	virtual ~COleConvertDialog();
#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif
};

/////////////////////////////////////////////////////////////////////////////
// COleChangeIconDialog

class COleChangeIconDialog : public COleDialog
{
	DECLARE_DYNAMIC(COleChangeIconDialog)

// Attributes
public:
	OLEUICHANGEICON m_ci;   // structure for OleUIChangeIcon

// Constructors
	COleChangeIconDialog(COleClientItem* pItem,
		DWORD dwFlags = CIF_SELECTCURRENT,
		CWnd* pParentWnd = NULL);

// Operations
	virtual int DoModal();
	BOOL DoChangeIcon(COleClientItem* pItem);

// Attributes
	HGLOBAL GetIconicMetafile() const;

// Implementation
public:
	virtual ~COleChangeIconDialog();
#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif
};

/////////////////////////////////////////////////////////////////////////////
// COlePasteSpecialDialog

class COlePasteSpecialDialog : public COleDialog
{
	DECLARE_DYNAMIC(COlePasteSpecialDialog)

// Attributes
public:
	OLEUIPASTESPECIAL m_ps; // structure for OleUIPasteSpecial

// Constructors
	COlePasteSpecialDialog(DWORD dwFlags = PSF_SELECTPASTE,
		COleDataObject* pDataObject = NULL, CWnd *pParentWnd = NULL);

// Operations
	OLEUIPASTEFLAG AddLinkEntry(UINT cf);
	void AddFormat(const FORMATETC& formatEtc, LPTSTR lpszFormat,
		LPTSTR lpszResult, DWORD flags);
	void AddFormat(UINT cf, DWORD tymed, UINT nFormatID, BOOL bEnableIcon,
		BOOL bLink);
	void AddStandardFormats(BOOL bEnableLink = TRUE);

	virtual int DoModal();
	BOOL CreateItem(COleClientItem *pNewItem);
		// creates a standard OLE item from selection data

// Attributes (after DoModal returns IDOK)
	int GetPasteIndex() const;      // resulting index to use for paste

	enum Selection { pasteLink = 1, pasteNormal = 2, pasteStatic = 3, pasteOther = 4};
	UINT GetSelectionType() const;
		// get selection type (pasteLink, pasteNormal, pasteStatic)

	DVASPECT GetDrawAspect() const;
		// DVASPECT_CONTENT or DVASPECT_ICON
	HGLOBAL GetIconicMetafile() const;
		// returns HGLOBAL to METAFILEPICT struct with iconic data

// Implementation
public:
	virtual ~COlePasteSpecialDialog();
#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
	virtual void AssertValid() const;
#endif
	unsigned int m_arrLinkTypes[8];
		// size limit imposed by MFCUIx32.DLL library
};

/////////////////////////////////////////////////////////////////////////////
// COleLinksDialog

class COleLinksDialog : public COleDialog
{
	DECLARE_DYNAMIC(COleLinksDialog)

// Attributes
public:
	OLEUIEDITLINKS m_el;    // structure for OleUIEditLinks

// Constructors
	COleLinksDialog(COleDocument* pDoc, CView* pView, DWORD dwFlags = 0,
		CWnd* pParentWnd = NULL);

// Operations
	virtual int DoModal();  // display the dialog and edit links

// Implementation
public:
	virtual ~COleLinksDialog();
#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
	virtual void AssertValid() const;
#endif

public:
	COleUILinkInfo m_xLinkInfo; // implements IOleUILinkContainer
};

/////////////////////////////////////////////////////////////////////////////
// COleUpdateDialog

class COleUpdateDialog : public COleLinksDialog
{
	DECLARE_DYNAMIC(COleUpdateDialog)

// Constructors
public:
	COleUpdateDialog(COleDocument* pDoc,
		BOOL bUpdateLinks = TRUE, BOOL bUpdateEmbeddings = FALSE,
		CWnd* pParentWnd = NULL);

// Operations
	virtual int DoModal();

// Implementation
public:
	virtual ~COleUpdateDialog();
#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	CString m_strCaption;   // caption for the dialog
};

/////////////////////////////////////////////////////////////////////////////
// COleBusyDialog - useful in managing concurrency

class COleBusyDialog : public COleDialog
{
	DECLARE_DYNAMIC(COleBusyDialog)

// Attributes
public:
	OLEUIBUSY m_bz;

// Constructors
	COleBusyDialog(HTASK htaskBusy, BOOL bNotResponding = FALSE,
		DWORD dwFlags = 0, CWnd* pParentWnd = NULL);

// Operations
	virtual int DoModal();

	enum Selection { switchTo = 1, retry = 2, callUnblocked = 3 };
	UINT GetSelectionType() const;

// Implementation
public:
	~COleBusyDialog();
#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	Selection m_selection;  // selection after DoModal returns IDOK
};

/////////////////////////////////////////////////////////////////////////////
// COleEditProperties

class COlePropertiesDialog : public COleDialog
{
	DECLARE_DYNAMIC(COlePropertiesDialog)

// Attributes
public:
	OLEUIOBJECTPROPS m_op;      // structure for OleUIObjectProperties
	OLEUIGNRLPROPS m_gp;        // specific to "General" page
	OLEUIVIEWPROPS m_vp;        // specific to "View" page
	OLEUILINKPROPS m_lp;        // specific to "Link" page
	PROPSHEETHEADER m_psh;      // PROPSHEETHEADER for customization

// Constructors
public:
	COlePropertiesDialog(COleClientItem* pItem,
		UINT nScaleMin = 10, UINT nScaleMax = 500, CWnd* pParentWnd = NULL);

// Operations
	virtual int DoModal();

// Overridables
	virtual BOOL OnApplyScale(
		COleClientItem* pItem, int nCurrentScale, BOOL bRelativeToOrig);

// Implementation
public:
#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
	virtual void AssertValid() const;
#endif
	virtual BOOL OnInitDialog();

	BEGIN_INTERFACE_PART(OleUIObjInfo, IOleUIObjInfo)
		INIT_INTERFACE_PART(COlePropertiesDialog, OleUIObjInfo)
		STDMETHOD(GetObjectInfo) (THIS_ DWORD dwObject,
			DWORD* lpdwObjSize, LPTSTR* lplpszLabel,
			LPTSTR* lplpszType, LPTSTR* lplpszShortType,
			LPTSTR* lplpszLocation);
		STDMETHOD(GetConvertInfo) (THIS_ DWORD dwObject,
			CLSID* lpClassID, WORD* lpwFormat,
			CLSID* lpConvertDefaultClassID,
			LPCLSID* lplpClsidExclude, UINT* lpcClsidExclude);
		STDMETHOD(ConvertObject) (THIS_ DWORD dwObject, REFCLSID clsidNew);
		STDMETHOD(GetViewInfo) (THIS_ DWORD dwObject,
			HGLOBAL* phMetaPict, DWORD* pdvAspect, int* pnCurrentScale);
		STDMETHOD(SetViewInfo) (THIS_ DWORD dwObject,
			HGLOBAL hMetaPict, DWORD dvAspect,
			int nCurrentScale, BOOL bRelativeToOrig);
	END_INTERFACE_PART(OleUIObjInfo)
	COleUILinkInfo m_xLinkInfo; // implements IOleUILinkContainer
};

/////////////////////////////////////////////////////////////////////////////
// COleChangeSourceDialog

class COleChangeSourceDialog : public COleDialog
{
	DECLARE_DYNAMIC(COleChangeSourceDialog)

// Attributes
public:
	OLEUICHANGESOURCE m_cs; // structure for OleUIChangeSource

// Constructors
public:
	COleChangeSourceDialog(COleClientItem* pItem, CWnd* pParentWnd = NULL);

// Operations
	virtual int DoModal();

// Attributes (after DoModal returns IDOK)
	BOOL IsValidSource();
	CString GetDisplayName();
	CString GetFileName();
	CString GetItemName();
	CString GetFromPrefix();
	CString GetToPrefix();

// Implementation
public:
	COleUILinkInfo m_xLinkInfo;

	virtual ~COleChangeSourceDialog();
#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
	virtual void AssertValid() const;
#endif
	virtual void PreInitDialog();
};

/////////////////////////////////////////////////////////////////////////////
// Inline function declarations

#ifdef _AFX_PACKING
#pragma pack(pop)
#endif

#ifdef _AFX_ENABLE_INLINES
#define _AFXODLGS_INLINE AFX_INLINE
#include <afxole.inl>
#undef _AFXODLGS_INLINE
#endif

#undef AFX_DATA
#define AFX_DATA

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, on)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, off)
#endif

#endif //__AFXODLGS_H__

/////////////////////////////////////////////////////////////////////////////
