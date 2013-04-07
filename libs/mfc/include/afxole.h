// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#ifndef __AFXOLE_H__
#define __AFXOLE_H__

#ifdef _AFX_NO_OLE_SUPPORT
	#error OLE classes not supported in this library variant.
#endif

#ifndef __AFXEXT_H__
	#include <afxext.h>
#endif

#ifndef __AFXDISP_H__
	#include <afxdisp.h>
#endif

// include OLE Compound Document headers
#ifndef _OLE2_H_
	#include <ole2.h>
#endif

// ActiveX Document support
#ifndef __docobj_h__
	#include <docobj.h>
#endif

// URL Monikers support
#ifndef __urlmon_h__
	#include <urlmon.h>
#endif

#ifndef __AFXCOM_H__
#include <afxcom_.h>
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


#ifndef _AFX_NOFORCE_LIBS

#pragma comment(lib, "urlmon.lib")

#endif // !_AFX_NOFORCE_LIBS

/////////////////////////////////////////////////////////////////////////////
// AFXOLE.H - MFC OLE support

// Classes declared in this file

//CDocument
	class COleDocument;             // OLE container document
		class COleLinkingDoc;       // supports links to embeddings
			class COleServerDoc;    // OLE server document
	class CDocObjectServer;         // might be owned by a COleServerDoc

//CCmdTarget
	class CDocItem;                 // part of a document
		class COleClientItem;       // embedded ole object from outside
			class COleDocObjectItem;// ActiveX Document item
		class COleServerItem;       // ole object to export
	class COleDataSource;           // clipboard data source mechanism
	class COleDropSource;           // drag/drop source
	class COleDropTarget;           // drag/drop target
	class COleMessageFilter;        // concurrency management

//CFrameWnd
	class COleIPFrameWnd;           // frame window for in-place servers

//CControlBar
	class COleResizeBar;            // implements in-place resizing

//CFile
	class COleStreamFile;           // CFile wrapper for IStream interface
		class CMonikerFile;         // bound to via IMoniker
			class CAsyncMonikerFile;// asynchronous IMoniker

class COleDataObject;               // wrapper for IDataObject interface

/////////////////////////////////////////////////////////////////////////////

// AFXDLL support
#undef AFX_DATA
#define AFX_DATA AFX_OLE_DATA

/////////////////////////////////////////////////////////////////////////////
// backward compatibility

// COleClientDoc is now obsolete -- use COleDocument instead
#define COleClientDoc COleDocument

// COleServer has been replaced by the more general COleObjectFactory
#define COleServer  COleObjectFactory

/////////////////////////////////////////////////////////////////////////////
// Useful OLE specific types (some from OLE 1.0 headers)

// Codes for CallBack events
enum OLE_NOTIFICATION
{
	OLE_CHANGED,        // representation of a draw aspect has changed
	OLE_SAVED,          // the item has committed its storage
	OLE_CLOSED,         // the item has closed
	OLE_RENAMED,        // the item has changed its moniker
	OLE_CHANGED_STATE,  // the item state (open, active, etc.) has changed
	OLE_CHANGED_ASPECT, // the item draw aspect has changed
};

// Object types
enum OLE_OBJTYPE
{
	OT_UNKNOWN = 0,

	// These are OLE 1.0 types and OLE 2.0 types as returned from GetType().
	OT_LINK = 1,
	OT_EMBEDDED = 2,
	OT_STATIC = 3,

	// All OLE2 objects are written with this tag when serialized.  This
	//  differentiates them from OLE 1.0 objects written with MFC 2.0.
	//  This value will never be returned from GetType().
	OT_OLE2 = 256,
};

/////////////////////////////////////////////////////////////////////////////
// COleDataObject -- simple wrapper for IDataObject

class COleDataObject
{
// Constructors
public:
	COleDataObject();

// Operations
	void Attach(LPDATAOBJECT lpDataObject, BOOL bAutoRelease = TRUE);
	LPDATAOBJECT Detach();  // detach and get ownership of m_lpDataObject
	void Release(); // detach and Release ownership of m_lpDataObject
	BOOL AttachClipboard(); // attach to current clipboard object

// Attributes
	void BeginEnumFormats();
	BOOL GetNextFormat(LPFORMATETC lpFormatEtc);
	CFile* GetFileData(CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc = NULL);
	HGLOBAL GetGlobalData(CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc = NULL);
	BOOL GetData(CLIPFORMAT cfFormat, LPSTGMEDIUM lpStgMedium,
		LPFORMATETC lpFormatEtc = NULL);
	BOOL IsDataAvailable(CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc = NULL);

// Implementation
public:
	LPDATAOBJECT m_lpDataObject;
	LPENUMFORMATETC m_lpEnumerator;
	~COleDataObject();

	// advanced use and implementation
	LPDATAOBJECT GetIDataObject(BOOL bAddRef);
	void EnsureClipboardObject();
	BOOL m_bClipboard;      // TRUE if represents the Win32 clipboard

protected:
	BOOL m_bAutoRelease;    // TRUE if destructor should call Release

private:
	// Disable the copy constructor and assignment by default so you will get
	//   compiler errors instead of unexpected behaviour if you pass objects
	//   by value or assign objects.
	COleDataObject(const COleDataObject&);  // no implementation
	void operator=(const COleDataObject&);  // no implementation
};

/////////////////////////////////////////////////////////////////////////////
// COleDataSource -- wrapper for implementing IDataObject
//  (works similar to how data is provided on the clipboard)

struct AFX_DATACACHE_ENTRY;
class COleDropSource;

class COleDataSource : public CCmdTarget
{
// Constructors
public:
	COleDataSource();

// Operations
	void Empty();   // empty cache (similar to ::EmptyClipboard)

	// CacheData & DelayRenderData operations similar to ::SetClipboardData
	void CacheGlobalData(CLIPFORMAT cfFormat, HGLOBAL hGlobal,
		LPFORMATETC lpFormatEtc = NULL);    // for HGLOBAL based data
	void DelayRenderFileData(CLIPFORMAT cfFormat,
		LPFORMATETC lpFormatEtc = NULL);    // for CFile* based delayed render

	// Clipboard and Drag/Drop access
	DROPEFFECT DoDragDrop(
		DWORD dwEffects = DROPEFFECT_COPY|DROPEFFECT_MOVE|DROPEFFECT_LINK,
		LPCRECT lpRectStartDrag = NULL,
		COleDropSource* pDropSource = NULL);
	void SetClipboard();
	static void PASCAL FlushClipboard();
	static COleDataSource* PASCAL GetClipboardOwner();

	// Advanced: STGMEDIUM based cached data
	void CacheData(CLIPFORMAT cfFormat, LPSTGMEDIUM lpStgMedium,
		LPFORMATETC lpFormatEtc = NULL);    // for LPSTGMEDIUM based data
	// Advanced: STGMEDIUM or HGLOBAL based delayed render
	void DelayRenderData(CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc = NULL);

	// Advanced: support for SetData in COleServerItem
	//  (not generally useful for clipboard or drag/drop operations)
	void DelaySetData(CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc = NULL);

// Overidables
	virtual BOOL OnRenderGlobalData(LPFORMATETC lpFormatEtc, HGLOBAL* phGlobal);
	virtual BOOL OnRenderFileData(LPFORMATETC lpFormatEtc, CFile* pFile);
	virtual BOOL OnRenderData(LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium);
		// OnRenderFileData and OnRenderGlobalData are called by
		//  the default implementation of OnRenderData.

	virtual BOOL OnSetData(LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium,
		BOOL bRelease);
		// used only in COleServerItem implementation

// Implementation
public:
	virtual ~COleDataSource();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	AFX_DATACACHE_ENTRY* m_pDataCache;  // data cache itself
	UINT m_nMaxSize;    // current allocated size
	UINT m_nSize;       // current size of the cache
	UINT m_nGrowBy;     // number of cache elements to grow by for new allocs

	AFX_DATACACHE_ENTRY* Lookup(
		LPFORMATETC lpFormatEtc, DATADIR nDataDir) const;
	AFX_DATACACHE_ENTRY* GetCacheEntry(
		LPFORMATETC lpFormatEtc, DATADIR nDataDir);

// Interface Maps
public:
	BEGIN_INTERFACE_PART(DataObject, IDataObject)
		INIT_INTERFACE_PART(COleDataSource, DataObject)
		STDMETHOD(GetData)(LPFORMATETC, LPSTGMEDIUM);
		STDMETHOD(GetDataHere)(LPFORMATETC, LPSTGMEDIUM);
		STDMETHOD(QueryGetData)(LPFORMATETC);
		STDMETHOD(GetCanonicalFormatEtc)(LPFORMATETC, LPFORMATETC);
		STDMETHOD(SetData)(LPFORMATETC, LPSTGMEDIUM, BOOL);
		STDMETHOD(EnumFormatEtc)(DWORD, LPENUMFORMATETC*);
		STDMETHOD(DAdvise)(LPFORMATETC, DWORD, LPADVISESINK, LPDWORD);
		STDMETHOD(DUnadvise)(DWORD);
		STDMETHOD(EnumDAdvise)(LPENUMSTATDATA*);
	END_INTERFACE_PART(DataObject)

	DECLARE_INTERFACE_MAP()

	friend class COleServerItem;
};

//////////////////////////////////////////////////////////////////////////////
// DocItem support

#ifdef _AFXDLL
class CDocItem : public CCmdTarget
#else
class AFX_NOVTABLE CDocItem : public CCmdTarget
#endif
{
	DECLARE_SERIAL(CDocItem)

// Constructors
protected:      // abstract class
	CDocItem();

// Attributes
public:
	CDocument* GetDocument() const; // return container document

// Overridables
public:
	// Raw data access (native format)
	virtual BOOL IsBlank() const;

// Implementation
protected:
	COleDocument* m_pDocument;

public:
	virtual void Serialize(CArchive& ar);   // for Native data
	virtual ~CDocItem();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	friend class COleDocument;              // for access to back pointer
};

//////////////////////////////////////////////////////////////////////////////
// COleDocument - common OLE container behavior (enables server functionality)

class COleDocument : public CDocument
{
	DECLARE_DYNAMIC(COleDocument)

// Constructors
public:
	COleDocument();

// Attributes
	BOOL HasBlankItems() const; // check for BLANK items
	virtual COleClientItem* GetInPlaceActiveItem(CWnd* pWnd);
		// return in-place active item for this view or NULL if none

// Operations
	// iterating over existing items
	virtual POSITION GetStartPosition() const;
	virtual CDocItem* GetNextItem(POSITION& pos) const;

	// iterator helpers (helpers use virtual GetNextItem above)
	COleClientItem* GetNextClientItem(POSITION& pos) const;
	COleServerItem* GetNextServerItem(POSITION& pos) const;

	// adding new items - called from item constructors
	virtual void AddItem(CDocItem* pItem);
	virtual void RemoveItem(CDocItem* pItem);

	void EnableCompoundFile(BOOL bEnable = TRUE);
		// enable compound file support (only call during constructor)
	virtual void UpdateModifiedFlag();
		// scan for modified items -- mark document modified

	// printer-device caching/control
	BOOL ApplyPrintDevice(const DVTARGETDEVICE* ptd);
	BOOL ApplyPrintDevice(const PRINTDLG* ppd);
		// these apply the target device to all COleClientItem objects

// Overridables
	virtual COleClientItem* GetPrimarySelectedItem(CView* pView);
		// return primary selected item or NULL if none
	virtual void OnShowViews(BOOL bVisible);
		// called during app-idle when visibility of a document has changed

// Implementation
public:
	CObList m_docItemList;  // not owned items

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	CFrameWnd* GetFirstFrame();

	// document handling overrides
	virtual void SetPathName(LPCTSTR lpszPathName, BOOL bAddToMRU = TRUE);
	virtual ~COleDocument();
	virtual void DeleteContents(); // delete client items in list
	virtual void Serialize(CArchive& ar);   // serialize items to file
	virtual void PreCloseFrame(CFrameWnd* pFrame);
	virtual BOOL SaveModified();
	virtual void OnIdle();

	// compound file implementation
	virtual BOOL OnNewDocument();
	virtual BOOL OnOpenDocument(LPCTSTR lpszPathName);
	virtual BOOL OnSaveDocument(LPCTSTR lpszPathName);
	virtual void OnCloseDocument();
	void CommitItems(BOOL bSuccess);    // called during File.Save & File.Save As

	// minimal linking protocol
	virtual LPMONIKER GetMoniker(OLEGETMONIKER nAssign);
	virtual LPOLEITEMCONTAINER GetContainer();

protected:
	// document state implementation
	UINT m_dwNextItemNumber;// serial number for next item in this document
	BOOL m_bLastVisible;    // TRUE if one or more views was last visible

	// 'docfile' support
	BOOL m_bCompoundFile;   // TRUE if use compound files
	LPSTORAGE m_lpRootStg;  // root storage for the document
	BOOL m_bSameAsLoad;     // TRUE = file-save, FALSE = Save [Copy] As
	BOOL m_bRemember;       // if FALSE, indicates Save Copy As

	DVTARGETDEVICE* m_ptd;  // current document target device

	// implementation helpers
	virtual void LoadFromStorage();
	virtual void SaveToStorage(CObject* pObject = NULL);
	CDocItem* GetNextItemOfKind(POSITION& pos, CRuntimeClass* pClass) const;

	// command handling
public:
	virtual BOOL OnCmdMsg(UINT nID, int nCode, void* pExtra,
		AFX_CMDHANDLERINFO* pHandlerInfo);

protected:
	afx_msg void OnUpdatePasteMenu(CCmdUI* pCmdUI);
	afx_msg void OnUpdatePasteLinkMenu(CCmdUI* pCmdUI);
	afx_msg void OnUpdateEditLinksMenu(CCmdUI* pCmdUI);
	afx_msg void OnEditLinks();
	afx_msg void OnEditConvert();
	afx_msg void OnUpdateEditChangeIcon(CCmdUI* pCmdUI);
	afx_msg void OnEditChangeIcon();
	afx_msg void OnUpdateObjectVerbMenu(CCmdUI* pCmdUI);
	afx_msg void OnFileSendMail();

	friend class COleClientItem;
	friend class COleServerItem;
};

/////////////////////////////////////////////////////////////////////////////
// COleClientItem - Supports OLE2 non-inplace editing.
//      implements IOleClientSite, IAdviseSink, and IOleInPlaceSite

class COleFrameHook;    // forward reference (see ..\src\oleimpl2.h)

class COleClientItem : public CDocItem
{
	DECLARE_DYNAMIC(COleClientItem)

// Constructors
public:
	COleClientItem(COleDocument* pContainerDoc = NULL);

	// create from the clipboard
	BOOL CreateFromClipboard(OLERENDER render = OLERENDER_DRAW,
		CLIPFORMAT cfFormat = 0, LPFORMATETC lpFormatEtc = NULL);
	BOOL CreateLinkFromClipboard(OLERENDER render = OLERENDER_DRAW,
		CLIPFORMAT cfFormat = 0, LPFORMATETC lpFormatEtc = NULL);
	BOOL CreateStaticFromClipboard(OLERENDER render = OLERENDER_DRAW,
		CLIPFORMAT cfFormat = 0, LPFORMATETC lpFormatEtc = NULL);

	// create from a class ID (Insert New Object dialog)
	BOOL CreateNewItem(REFCLSID clsid, OLERENDER render = OLERENDER_DRAW,
		CLIPFORMAT cfFormat = 0, LPFORMATETC lpFormatEtc = NULL);

	// create from COleDataObject
	BOOL CreateFromData(COleDataObject* pDataObject,
		OLERENDER render = OLERENDER_DRAW,
		CLIPFORMAT cfFormat = 0, LPFORMATETC lpFormatEtc = NULL);
	BOOL CreateLinkFromData(COleDataObject* pDataObject,
		OLERENDER render = OLERENDER_DRAW,
		CLIPFORMAT cfFormat = 0, LPFORMATETC lpFormatEtc = NULL);
	BOOL CreateStaticFromData(COleDataObject* pDataObject,
		OLERENDER render = OLERENDER_DRAW,
		CLIPFORMAT cfFormat = 0, LPFORMATETC lpFormatEtc = NULL);

	// create from file (package support)
	BOOL CreateFromFile(LPCTSTR lpszFileName, REFCLSID clsid = CLSID_NULL,
		OLERENDER render = OLERENDER_DRAW,
		CLIPFORMAT cfFormat = 0, LPFORMATETC lpFormatEtc = NULL);
	BOOL CreateLinkFromFile(LPCTSTR lpszFileName,
		OLERENDER render = OLERENDER_DRAW,
		CLIPFORMAT cfFormat = 0, LPFORMATETC lpFormatEtc = NULL);

	// create a copy
	BOOL CreateCloneFrom(const COleClientItem* pSrcItem);

// General Attributes
public:
	HICON GetIconFromRegistry() const;
	static HICON GetIconFromRegistry(CLSID& clsid);
	SCODE GetLastStatus() const;
	OLE_OBJTYPE GetType() const; // OT_LINK, OT_EMBEDDED, OT_STATIC
	void GetClassID(CLSID* pClassID) const;
	void GetUserType(USERCLASSTYPE nUserClassType, CString& rString);
	BOOL GetExtent(LPSIZE lpSize, DVASPECT nDrawAspect = (DVASPECT)-1);
		// will return FALSE if item is BLANK
	BOOL GetCachedExtent(LPSIZE lpSize, DVASPECT nDrawAspect = (DVASPECT)-1);

	// getting/setting iconic cache
	HGLOBAL GetIconicMetafile();
	BOOL SetIconicMetafile(HGLOBAL hMetaPict);

	// setting/getting default display aspect
	DVASPECT GetDrawAspect() const;
	virtual void SetDrawAspect(DVASPECT nDrawAspect);

	// for printer presentation cache
	BOOL SetPrintDevice(const DVTARGETDEVICE* ptd);
	BOOL SetPrintDevice(const PRINTDLG* ppd);

	// Item state
	enum ItemState
		{ emptyState, loadedState, openState, activeState, activeUIState };
	UINT GetItemState() const;

	BOOL IsModified() const;
	BOOL IsRunning() const;
	BOOL IsInPlaceActive() const;
	BOOL IsOpen() const;
	CView* GetActiveView() const;

	// Data access
	void AttachDataObject(COleDataObject& rDataObject) const;

	// other rare access information
	COleDocument* GetDocument() const; // return container

	// helpers for checking clipboard data availability
	static BOOL PASCAL CanPaste();
	static BOOL PASCAL CanPasteLink();

	// helpers for checking COleDataObject, useful in drag drop
	static BOOL PASCAL CanCreateFromData(const COleDataObject* pDataObject);
	static BOOL PASCAL CanCreateLinkFromData(const COleDataObject* pDataObject);

// General Operations
	virtual void Release(OLECLOSE dwCloseOption = OLECLOSE_NOSAVE);
		// cleanup, detach (close if needed)
	void Close(OLECLOSE dwCloseOption = OLECLOSE_SAVEIFDIRTY);
		// close without releasing the item
	void Delete(BOOL bAutoDelete = TRUE);
		// logically delete from file -- not part of the document anymore
	void Run(); // insure item is in running state

	// Drawing
	BOOL Draw(CDC* pDC, LPCRECT lpBounds,
		DVASPECT nDrawAspect = (DVASPECT)-1);   // defaults to m_nDrawAspect

	// Activation
	virtual BOOL DoVerb(LONG nVerb, CView* pView, LPMSG lpMsg = NULL);
	void Activate(LONG nVerb, CView* pView, LPMSG lpMsg = NULL);

	// In-place Activation
	void Deactivate();          // completely deactivate
	void DeactivateUI();        // deactivate the user interface
	BOOL ReactivateAndUndo();   // reactivate then perform undo command
	BOOL SetItemRects(LPCRECT lpPosRect = NULL, LPCRECT lpClipRect = NULL);
	CWnd* GetInPlaceWindow();

	// Clipboard operations
	void CopyToClipboard(BOOL bIncludeLink = FALSE);
	DROPEFFECT DoDragDrop(LPCRECT lpItemRect, CPoint ptOffset,
		BOOL bIncludeLink = FALSE,
		DWORD dwEffects = DROPEFFECT_COPY|DROPEFFECT_MOVE,
		LPCRECT lpRectStartDrag = NULL);
	void GetClipboardData(COleDataSource* pDataSource,
		BOOL bIncludeLink = FALSE, LPPOINT lpOffset = NULL,
		LPSIZE lpSize = NULL);

	// called for creating a COleDataSource by CopyToClipboard and DoDragDrop
	virtual COleDataSource* OnGetClipboardData(BOOL bIncludeLink,
		LPPOINT lpOffset, LPSIZE lpSize);

	// Operations that apply to embedded items only
	void SetHostNames(LPCTSTR lpszHost, LPCTSTR lpszHostObj);
	void SetExtent(const CSize& size, DVASPECT nDrawAspect = DVASPECT_CONTENT);

	// Operations that apply to linked items only
	//  (link options are rarely changed, except through Links dialog)
	OLEUPDATE GetLinkUpdateOptions();
	void SetLinkUpdateOptions(OLEUPDATE dwUpdateOpt);

	// Link-source update status (also useful for embeddings that contain links)
	BOOL UpdateLink();  // make up-to-date
	BOOL IsLinkUpToDate() const;    // is link up-to-date

	// object conversion
	virtual BOOL ConvertTo(REFCLSID clsidNew);
	virtual BOOL ActivateAs(LPCTSTR lpszUserType, REFCLSID clsidOld, REFCLSID clsidNew);
	BOOL Reload();  // for lazy reload after ActivateAs

// Overridables (notifications of IAdviseSink, IOleClientSite and IOleInPlaceSite)
	// Callbacks/notifications from the server you must/should implement
	virtual void OnChange(OLE_NOTIFICATION nCode, DWORD dwParam);
		// implement OnChange to invalidate when item changes

protected:
	virtual void OnGetItemPosition(CRect& rPosition);
		// implement OnGetItemPosition if you support in-place activation

	// Common overrides for in-place activation
	virtual BOOL OnScrollBy(CSize sizeExtent);

	// Common overrides for applications supporting undo
	virtual void OnDiscardUndoState();
	virtual void OnDeactivateAndUndo();

public:
	virtual void OnDeactivateUI(BOOL bUndoable);

protected:
	// Common overrides for applications supporting links to embeddings
	virtual void OnShowItem();

	// Advanced overrides for in-place activation
	virtual void OnGetClipRect(CRect& rClipRect);
	virtual BOOL CanActivate();

public:
	virtual void OnActivate();
	virtual void OnActivateUI();
	virtual void OnDeactivate();

protected:
	virtual BOOL OnGetWindowContext(CFrameWnd** ppMainFrame,
		CFrameWnd** ppDocFrame, LPOLEINPLACEFRAMEINFO lpFrameInfo);
	virtual BOOL OnChangeItemPosition(const CRect& rectPos);
		// default calls SetItemRects and caches the pos rect

public:
	// Advanced overrides for menu/title handling (rarely overridden)
	virtual void OnInsertMenus(CMenu* pMenuShared,
		LPOLEMENUGROUPWIDTHS lpMenuWidths);
	virtual void OnSetMenu(CMenu* pMenuShared, HOLEMENU holemenu,
		HWND hwndActiveObject);
	virtual void OnRemoveMenus(CMenu* pMenuShared);
	virtual BOOL OnUpdateFrameTitle();

	// Advanced override for control bar handling
	virtual BOOL OnShowControlBars(CFrameWnd* pFrameWnd, BOOL bShow);

// Implementation
public:
	// data to support non-inplace activated items
	LPOLEOBJECT m_lpObject; // in case you want direct access to the OLE object
	LPVIEWOBJECT2 m_lpViewObject;// IViewObject for IOleObject above
	DWORD m_dwItemNumber;   // serial number for this item in this document
	DVASPECT m_nDrawAspect; // current default display aspect
	SCODE m_scLast;         // last error code encountered
	LPSTORAGE m_lpStorage;  // provides storage for m_lpObject
	LPLOCKBYTES m_lpLockBytes;  // part of implementation of m_lpStorage
	DWORD m_dwConnection;   // advise connection to the m_lpObject
	BYTE m_bLinkUnavail;    // TRUE if link is currently unavailable
	BYTE m_bMoniker;        // TRUE if moniker is assigned
	BYTE m_bLocked;         // TRUE if object has external lock
	BYTE m_bNeedCommit;     // TRUE if need CommitItem
	BYTE m_bClosing;        // TRUE if currently doing COleClientItem::Close
	BYTE m_bReserved[3];    // (reserved for future use)

	// for compound file support
	LPSTORAGE m_lpNewStorage;   // used during Save As situations

	// item state & item type
	ItemState m_nItemState; // item state (see ItemState enumeration)
	OLE_OBJTYPE m_nItemType;    // item type (depends on how created)

	// data valid when in-place activated
	CView* m_pView; // view when object is in-place activated
	DWORD m_dwContainerStyle;   // style of the container wnd before activation
	COleFrameHook* m_pInPlaceFrame;// frame window when in-place active
	COleFrameHook* m_pInPlaceDoc;   // doc window when in-place (may be NULL)
	HWND m_hWndServer;  // HWND of in-place server window

public:
	virtual ~COleClientItem();
	virtual void Serialize(CArchive& ar);
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

// Implementation
public:
	virtual BOOL ReportError(SCODE sc) const;
	virtual BOOL FreezeLink();  // converts to static: for edit links dialog

	DWORD GetNewItemNumber();   // generates new item number
	void GetItemName(LPTSTR lpszItemName) const; // gets readable item name

	void UpdateItemType();  // update m_nItemType

protected:
	// clipboard helpers
	void GetEmbeddedItemData(LPSTGMEDIUM lpStgMedium);
	void AddCachedData(COleDataSource* pDataSource);
	BOOL GetLinkSourceData(LPSTGMEDIUM lpStgMedium);
	void GetObjectDescriptorData(LPPOINT lpOffset, LPSIZE lpSize,
		LPSTGMEDIUM lpStgMedium);

	// interface helpers
	virtual LPOLECLIENTSITE GetClientSite();

	// helpers for printer-cached representation
	BOOL GetPrintDeviceInfo(LPOLECACHE* plpOleCache,
		DVTARGETDEVICE** pptd, DWORD* pdwConnection);

// Advanced Overridables for implementation
protected:
	virtual BOOL FinishCreate(SCODE sc);
	virtual void CheckGeneral(SCODE sc);

	virtual void OnDataChange(LPFORMATETC lpFormatEtc,
		LPSTGMEDIUM lpStgMedium);

public:
	// for storage hookability (override to use 'docfiles')
	virtual void GetItemStorage();  // allocate storage for new item
	virtual void ReadItem(CArchive& ar);    // read item from archive
	virtual void WriteItem(CArchive& ar);   // write item to archive
	virtual void CommitItem(BOOL bSuccess); // commit item's storage

	// compound & flat file implementations of above
	void GetItemStorageFlat();
	void ReadItemFlat(CArchive& ar);
	void WriteItemFlat(CArchive& ar);
	void GetItemStorageCompound();
	void ReadItemCompound(CArchive& ar);
	void WriteItemCompound(CArchive& ar);

// Interface Maps
public:
	BEGIN_INTERFACE_PART(OleClientSite, IOleClientSite)
		INIT_INTERFACE_PART(COleClientItem, OleClientSite)
		STDMETHOD(SaveObject)();
		STDMETHOD(GetMoniker)(DWORD, DWORD, LPMONIKER*);
		STDMETHOD(GetContainer)(LPOLECONTAINER*);
		STDMETHOD(ShowObject)();
		STDMETHOD(OnShowWindow)(BOOL);
		STDMETHOD(RequestNewObjectLayout)();
	END_INTERFACE_PART(OleClientSite)

	BEGIN_INTERFACE_PART(AdviseSink, IAdviseSink)
		INIT_INTERFACE_PART(COleClientItem, AdviseSink)
		STDMETHOD_(void,OnDataChange)(LPFORMATETC, LPSTGMEDIUM);
		STDMETHOD_(void,OnViewChange)(DWORD, LONG);
		STDMETHOD_(void,OnRename)(LPMONIKER);
		STDMETHOD_(void,OnSave)();
		STDMETHOD_(void,OnClose)();
	END_INTERFACE_PART(AdviseSink)

	BEGIN_INTERFACE_PART(OleIPSite, IOleInPlaceSite)
		INIT_INTERFACE_PART(COleClientItem, OleIPSite)
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

	DECLARE_INTERFACE_MAP()

// Friendship declarations (to avoid many public members)
	friend class COleUIWindow;
	friend class COleFrameWindow;
	friend class COleLinkingDoc;
};

class COleDocObjectItem : public COleClientItem
{
	friend class COleFrameHook;
	DECLARE_DYNAMIC(COleDocObjectItem)

// Constructors
public:
	COleDocObjectItem(COleDocument* pContainerDoc = NULL);

//Overridables
public:
	LPOLEDOCUMENTVIEW GetActiveView() const;
	virtual void Release(OLECLOSE dwCloseOption = OLECLOSE_NOSAVE);
	virtual void OnInsertMenus(CMenu* pMenuShared,
		LPOLEMENUGROUPWIDTHS lpMenuWidths);
	virtual void OnRemoveMenus(CMenu *pMenuShared);

// Operations
public:
	static BOOL OnPreparePrinting(CView* pCaller, CPrintInfo* pInfo,
		BOOL bPrintAll = TRUE);
	static void OnPrint(CView* pCaller, CPrintInfo* pInfo,
		BOOL bPrintAll = TRUE);
	BOOL GetPageCount(LPLONG pnFirstPage, LPLONG pcPages);
	HRESULT ExecCommand(DWORD nCmdID,
		DWORD nCmdExecOpt = OLECMDEXECOPT_DONTPROMPTUSER,
		const GUID* pguidCmdGroup = NULL);

// Implementation
public:
	virtual ~COleDocObjectItem();
	CMenu* m_pHelpPopupMenu;

protected:
	virtual void OnGetItemPosition(CRect& rPosition);
	virtual CMenu* GetHelpMenu(UINT& nPosition);
	void ActivateAndShow();
	LPOLEDOCUMENTVIEW m_pActiveView;
	LPPRINT m_pIPrint;
	BOOL SupportsIPrint();
	BOOL m_bInHelpMenu;

	BEGIN_INTERFACE_PART(OleDocumentSite, IOleDocumentSite)
		INIT_INTERFACE_PART(COleDocObjectItem, OleDocumentSite)
		STDMETHOD(ActivateMe)(LPOLEDOCUMENTVIEW pViewToActivate);
	END_INTERFACE_PART(OleDocumentSite)

	DECLARE_INTERFACE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// COleServerItem - IOleObject & IDataObject OLE component

#ifdef _AFXDLL
class COleServerItem : public CDocItem
#else
class AFX_NOVTABLE COleServerItem : public CDocItem
#endif
{
	DECLARE_DYNAMIC(COleServerItem)
protected:
	// NOTE: many members in this class are protected - since everything
	//   in this class is designed for implementing an OLE server.
	// Requests will come from OLE containers through non-C++ mechanisms,
	//   which will result in virtual functions in this class being called.

// Constructors
	COleServerItem(COleServerDoc* pServerDoc, BOOL bAutoDelete);
		// If your COleServerItem is an integral part of your data,
		//  bAutoDelete should be FALSE.  If your COleServerItem can be
		//  deleted when a link is released, it can be TRUE.

	COleDataSource* GetDataSource();
		// Use this data source to add conversion formats that your
		//  server should support.  Usually such formats are
		//  added in the item's constructor.

// Public Attributes
public:
	COleServerDoc* GetDocument() const; // return server document

	// naming (for links only)
	const CString& GetItemName() const; // get name of linked item
	void SetItemName(LPCTSTR lpszItemName);  // set name of linked item

	// link state
	BOOL IsConnected() const;   // returns TRUE if item has a client
	BOOL IsLinkedItem() const;  // returns TRUE if item is not embedded item

	// extents
	CSize m_sizeExtent;
		// HIMETRIC size -- the default implementation of OnSetExtent
		//  updates this member variable.  This member tells the server how
		//  much of the object is visible in the container document.

// Operations
public:
	void NotifyChanged(DVASPECT nDrawAspect = DVASPECT_CONTENT);
		// call this after you change item
	void CopyToClipboard(BOOL bIncludeLink = FALSE);
		// helper for implementing server 'copy to clipboard'
	DROPEFFECT DoDragDrop(LPCRECT lpRectItem, CPoint ptOffset,
		BOOL bIncludeLink = FALSE,
		DWORD dwEffects = DROPEFFECT_COPY|DROPEFFECT_MOVE,
		LPCRECT lpRectStartDrag = NULL);
	void GetClipboardData(COleDataSource* pDataSource,
		BOOL bIncludeLink = FALSE, LPPOINT lpOffset = NULL,
		LPSIZE lpSize = NULL);

// Overridables
	// overridables you must implement for yourself
	virtual BOOL OnDraw(CDC* pDC, CSize& rSize) = 0;
		// drawing for metafile format (return FALSE if not supported or error)
		//  (called for DVASPECT_CONTENT only)

	// overridables you may want to implement yourself
	virtual void OnUpdate(COleServerItem* pSender,
		LPARAM lHint, CObject* pHint, DVASPECT nDrawAspect);
		// the default implementation always calls NotifyChanged

	virtual BOOL OnDrawEx(CDC* pDC, DVASPECT nDrawAspect, CSize& rSize);
		// advanced drawing -- called for DVASPECT other than DVASPECT_CONTENT
	virtual BOOL OnSetExtent(DVASPECT nDrawAspect, const CSize& size);
	virtual BOOL OnGetExtent(DVASPECT nDrawAspect, CSize& rSize);
		// default implementation uses m_sizeExtent

	// overridables you do not have to implement
	virtual void OnDoVerb(LONG iVerb);
		// default routes to OnShow &/or OnOpen
	virtual BOOL OnSetColorScheme(const LOGPALETTE* lpLogPalette);
		// default does nothing
	virtual COleDataSource* OnGetClipboardData(BOOL bIncludeLink,
		LPPOINT lpOffset, LPSIZE lpSize);
		// called for access to clipboard data
	virtual BOOL OnQueryUpdateItems();
		// called to determine if there are any contained out-of-date links
	virtual void OnUpdateItems();
		// called to update any out-of-date links

protected:
	virtual void OnShow();
		// show item in the user interface (may edit in-place)
	virtual void OnOpen();
		// show item in the user interface (must open fully)
	virtual void OnHide();
		// hide document (and sometimes application)

	// very advanced overridables
public:
	virtual BOOL OnInitFromData(COleDataObject* pDataObject, BOOL bCreation);
		// initialize object from IDataObject

	// see COleDataSource for a description of these overridables
	virtual BOOL OnRenderGlobalData(LPFORMATETC lpFormatEtc, HGLOBAL* phGlobal);
	virtual BOOL OnRenderFileData(LPFORMATETC lpFormatEtc, CFile* pFile);
	virtual BOOL OnRenderData(LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium);
		// HGLOBAL version will be called first, then CFile* version

	virtual BOOL OnSetData(LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium,
		BOOL bRelease);
		// Rare -- only if you support SetData (programmatic paste)

	// advanced helpers for CopyToClipboard
	void GetEmbedSourceData(LPSTGMEDIUM lpStgMedium);
	void AddOtherClipboardData(COleDataSource* pDataSource);
	BOOL GetLinkSourceData(LPSTGMEDIUM lpStgMedium);
	void GetObjectDescriptorData(LPPOINT lpOffset, LPSIZE lpSize,
		LPSTGMEDIUM lpStgMedium);

// Implementation
public:
	BOOL m_bNeedUnlock;             // if TRUE need to pDoc->LockExternal(FALSE)
	BOOL m_bAutoDelete;             // if TRUE will OnRelease will 'delete this'

	// special version of OnFinalRelease to implement document locking
	virtual void OnFinalRelease();

protected:
	CString m_strItemName;          // simple item name

public:
	LPOLEADVISEHOLDER m_lpOleAdviseHolder;  // may be NULL
	LPDATAADVISEHOLDER m_lpDataAdviseHolder;    // may be NULL

	virtual ~COleServerItem();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	// implementation helpers
	void NotifyClient(OLE_NOTIFICATION wNotification, DWORD dwParam);
	LPDATAOBJECT GetDataObject();
	LPOLEOBJECT GetOleObject();
	LPMONIKER GetMoniker(OLEGETMONIKER nAssign);

protected:
	virtual BOOL GetMetafileData(LPFORMATETC lpFormatEtc, LPSTGMEDIUM pmedium);
		// calls OnDraw or OnDrawEx
	virtual void OnSaveEmbedding(LPSTORAGE lpStorage);
	virtual BOOL IsBlank() const;

	// CItemDataSource implements OnRender reflections to COleServerItem
	class CItemDataSource : public COleDataSource
	{
	protected:
		// the GetData and SetData interfaces forward to m_pItem
		virtual BOOL OnRenderGlobalData(LPFORMATETC lpFormatEtc, HGLOBAL* phGlobal);
		virtual BOOL OnRenderFileData(LPFORMATETC lpFormatEtc, CFile* pFile);
		virtual BOOL OnRenderData(LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium);
			// HGLOBAL version will be called first, then CFile* version

		virtual BOOL OnSetData(LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium,
			BOOL bRelease);
	};
	CItemDataSource m_dataSource;
		// data source used to implement IDataObject

// Interface Maps
//  (Note: these interface maps are used just for link implementation)
public:
	BEGIN_INTERFACE_PART(OleObject, IOleObject)
		INIT_INTERFACE_PART(COleServerItem, OleObject)
		STDMETHOD(SetClientSite)(LPOLECLIENTSITE);
		STDMETHOD(GetClientSite)(LPOLECLIENTSITE*);
		STDMETHOD(SetHostNames)(LPCOLESTR, LPCOLESTR);
		STDMETHOD(Close)(DWORD);
		STDMETHOD(SetMoniker)(DWORD, LPMONIKER);
		STDMETHOD(GetMoniker)(DWORD, DWORD, LPMONIKER*);
		STDMETHOD(InitFromData)(LPDATAOBJECT, BOOL, DWORD);
		STDMETHOD(GetClipboardData)(DWORD, LPDATAOBJECT*);
		STDMETHOD(DoVerb)(LONG, LPMSG, LPOLECLIENTSITE, LONG, HWND, LPCRECT);
		STDMETHOD(EnumVerbs)(LPENUMOLEVERB*);
		STDMETHOD(Update)();
		STDMETHOD(IsUpToDate)();
		STDMETHOD(GetUserClassID)(LPCLSID);
		STDMETHOD(GetUserType)(DWORD, LPOLESTR*);
		STDMETHOD(SetExtent)(DWORD, LPSIZEL);
		STDMETHOD(GetExtent)(DWORD, LPSIZEL);
		STDMETHOD(Advise)(LPADVISESINK, LPDWORD);
		STDMETHOD(Unadvise)(DWORD);
		STDMETHOD(EnumAdvise)(LPENUMSTATDATA*);
		STDMETHOD(GetMiscStatus)(DWORD, LPDWORD);
		STDMETHOD(SetColorScheme)(LPLOGPALETTE);
	END_INTERFACE_PART(OleObject)

	BEGIN_INTERFACE_PART(DataObject, IDataObject)
		INIT_INTERFACE_PART(COleServerItem, DataObject)
		STDMETHOD(GetData)(LPFORMATETC, LPSTGMEDIUM);
		STDMETHOD(GetDataHere)(LPFORMATETC, LPSTGMEDIUM);
		STDMETHOD(QueryGetData)(LPFORMATETC);
		STDMETHOD(GetCanonicalFormatEtc)(LPFORMATETC, LPFORMATETC);
		STDMETHOD(SetData)(LPFORMATETC, LPSTGMEDIUM, BOOL);
		STDMETHOD(EnumFormatEtc)(DWORD, LPENUMFORMATETC*);
		STDMETHOD(DAdvise)(LPFORMATETC, DWORD, LPADVISESINK, LPDWORD);
		STDMETHOD(DUnadvise)(DWORD);
		STDMETHOD(EnumDAdvise)(LPENUMSTATDATA*);
	END_INTERFACE_PART(DataObject)

	DECLARE_INTERFACE_MAP()

	friend class CItemDataSource;
	friend class COleServerDoc;
	friend class COleLinkingDoc;
};

/////////////////////////////////////////////////////////////////////////////
// COleLinkingDoc -
//  (enables linking to embeddings - beginnings of server fuctionality)

class COleLinkingDoc : public COleDocument
{
	DECLARE_DYNAMIC(COleLinkingDoc)

// Constructors
public:
	COleLinkingDoc();

// Operations
	BOOL Register(COleObjectFactory* pFactory, LPCTSTR lpszPathName);
		// notify the running object table and connect to pServer
	void Revoke();
		// revoke from running object table

// Overridables
protected:
	virtual COleServerItem* OnGetLinkedItem(LPCTSTR lpszItemName);
		// return item for the named linked item (for supporting links)
	virtual COleClientItem* OnFindEmbeddedItem(LPCTSTR lpszItemName);
		// return item for the named embedded item (for links to embeddings)

// Implementation
public:
	COleObjectFactory* m_pFactory;  // back-pointer to server

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	virtual ~COleLinkingDoc();

	// overrides for updating of monikers & running object table registration
	virtual BOOL OnNewDocument();
	virtual BOOL OnOpenDocument(LPCTSTR lpszPathName);
	virtual BOOL OnSaveDocument(LPCTSTR lpszPathName);
	virtual void OnCloseDocument();
	virtual LPOLEITEMCONTAINER GetContainer();
	virtual LPMONIKER GetMoniker(OLEGETMONIKER nAssign);

	// special handling of error messages during save/load
	virtual void ReportSaveLoadException(LPCTSTR lpszPathName,
		CException* e, BOOL bSaving, UINT nIDPDefault);
	void BeginDeferErrors();
	SCODE EndDeferErrors(SCODE sc);

protected:
	BOOL m_bDeferErrors;    // TRUE if in non-interactive OLE mode
	CException* m_pLastException;

	DWORD m_dwRegister;     // file moniker's registration in the ROT
	LPMONIKER m_lpMonikerROT; // file moniker that is registered
	CString m_strMoniker;   // filename used to create moniker
	BOOL m_bVisibleLock;    // TRUE if user is holding lock on document

	// implementation helpers
	virtual BOOL RegisterIfServerAttached(LPCTSTR lpszPathName, BOOL bMessage);
	void LockExternal(BOOL bLock, BOOL bRemoveRefs);
	void UpdateVisibleLock(BOOL bVisible, BOOL bRemoveRefs);
	virtual void OnShowViews(BOOL bVisible);

	virtual void SaveToStorage(CObject* pObject = NULL);

// Interface Maps
public:
	BEGIN_INTERFACE_PART(PersistFile, IPersistFile)
		INIT_INTERFACE_PART(COleLinkingDoc, PersistFile)
		STDMETHOD(GetClassID)(LPCLSID);
		STDMETHOD(IsDirty)();
		STDMETHOD(Load)(LPCOLESTR, DWORD);
		STDMETHOD(Save)(LPCOLESTR, BOOL);
		STDMETHOD(SaveCompleted)(LPCOLESTR);
		STDMETHOD(GetCurFile)(LPOLESTR*);
	END_INTERFACE_PART(PersistFile)

	BEGIN_INTERFACE_PART(OleItemContainer, IOleItemContainer)
		INIT_INTERFACE_PART(COleLinkingDoc, OleItemContainer)
		STDMETHOD(ParseDisplayName)(LPBC, LPOLESTR, ULONG*, LPMONIKER*);
		STDMETHOD(EnumObjects)(DWORD, LPENUMUNKNOWN*);
		STDMETHOD(LockContainer)(BOOL);
		STDMETHOD(GetObject)(LPOLESTR, DWORD, LPBINDCTX, REFIID, LPVOID*);
		STDMETHOD(GetObjectStorage)(LPOLESTR, LPBINDCTX, REFIID, LPVOID*);
		STDMETHOD(IsRunning)(LPOLESTR);
	END_INTERFACE_PART(OleItemContainer)

	DECLARE_INTERFACE_MAP()

	friend class COleClientItem;
	friend class COleClientItem::XOleClientSite;
	friend class COleServerItem::XOleObject;
};

//////////////////////////////////////////////////////////////////////////////
// COleServerDoc - registered server document containing COleServerItems

#ifdef _AFXDLL
class COleServerDoc : public COleLinkingDoc
#else
class AFX_NOVTABLE COleServerDoc : public COleLinkingDoc
#endif
{
	DECLARE_DYNAMIC(COleServerDoc)

// Constructors and Destructors
public:
	COleServerDoc();

// Attributes
	BOOL IsEmbedded() const;    // TRUE if document is an embedding
	BOOL IsDocObject() const;   // TRUE if document is a DocObject
	COleServerItem* GetEmbeddedItem();
		// return embedded item for document (will allocate if necessary)

	// attributes specific to in-place activation
	BOOL IsInPlaceActive() const;
	void GetItemPosition(LPRECT lpPosRect) const;
		// get current position rectangle of in-place edit
	void GetItemClipRect(LPRECT lpClipRect) const;
		// get current clipping rectangle of in-place edit
	BOOL GetZoomFactor(LPSIZE lpSizeNum = NULL, LPSIZE lpSizeDenom = NULL,
		LPCRECT lpPosRect = NULL) const;
		// returns the zoom factor in pixels

// Operations
	void NotifyChanged();
		// call this after you change some global attribute like
		//  document dimensions
	void UpdateAllItems(COleServerItem* pSender,
		LPARAM lHint = 0L, CObject* pHint = NULL,
		DVASPECT nDrawAspect = DVASPECT_CONTENT);

	// changes to the entire document (automatically notifies clients)
	void NotifyRename(LPCTSTR lpszNewName);
	void NotifySaved();
	void NotifyClosed();        // call this after you close document

	// specific operations for embedded documents
	void SaveEmbedding();       // call this to save embedded (before closing)

	// specific to in-place activation
	BOOL ActivateInPlace();
	void ActivateDocObject();
	void RequestPositionChange(LPCRECT lpPosRect);
	BOOL ScrollContainerBy(CSize sizeScroll);
	BOOL DeactivateAndUndo();
	BOOL DiscardUndoState();

public:
// Overridables for standard user interface (full server)
	virtual BOOL OnUpdateDocument(); // implementation of embedded update

protected:
// Overridables you must implement for yourself
	virtual COleServerItem* OnGetEmbeddedItem() = 0;
		// return item representing entire (embedded) document

// Overridables you do not have to implement
	virtual void OnClose(OLECLOSE dwCloseOption);
	virtual void OnSetHostNames(LPCTSTR lpszHost, LPCTSTR lpszHostObj);
	virtual HRESULT OnExecOleCmd(const GUID* pguidCmdGroup, DWORD nCmdID,
		DWORD nCmdExecOpt, VARIANTARG* pvarargIn, VARIANTARG* pvarargOut);
	virtual CDocObjectServer* GetDocObjectServer(LPOLEDOCUMENTSITE pDocSite);

// Advanced overridables
	LPUNKNOWN GetInterfaceHook(const void* piid);
	virtual void OnShowDocument(BOOL bShow);
		// show first frame for document or hide all frames for document

// Advanced overridables for in-place activation
public:
	virtual void OnDeactivate();
	virtual void OnDeactivateUI(BOOL bUndoable);

protected:
	virtual void OnSetItemRects(LPCRECT lpPosRect, LPCRECT lpClipRect);
	virtual BOOL OnReactivateAndUndo();

	virtual void OnFrameWindowActivate(BOOL bActivate);
	virtual void OnDocWindowActivate(BOOL bActivate);
	virtual void OnShowControlBars(CFrameWnd* pFrameWnd, BOOL bShow);
	virtual COleIPFrameWnd* CreateInPlaceFrame(CWnd* pParentWnd);
	virtual void DestroyInPlaceFrame(COleIPFrameWnd* pFrameWnd);
public:
	virtual void OnResizeBorder(LPCRECT lpRectBorder,
		LPOLEINPLACEUIWINDOW lpUIWindow, BOOL bFrame);

// Implementation
protected:
	LPOLECLIENTSITE m_lpClientSite;     // for embedded item
	CString m_strHostObj;               // name of document in container
	BOOL m_bCntrVisible;                // TRUE if OnShowWindow(TRUE) called
	BOOL m_bClosing;                    // TRUE if shutting down
	COleServerItem* m_pEmbeddedItem;    // pointer to embedded item for document

	COleIPFrameWnd* m_pInPlaceFrame;    // not NULL if in-place activated
	CWnd* m_pOrigParent;                // not NULL if existing view used
	DWORD m_dwOrigStyle;                // original style of in-place view
	DWORD m_dwOrigStyleEx;              // original extended style

	CDocObjectServer* m_pDocObjectServer;  // if DocObject, ptr to doc site

public:
	virtual ~COleServerDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	// overridables for implementation
	virtual BOOL OnSaveDocument(LPCTSTR lpszPathName);
	virtual void OnCloseDocument();
	virtual void DeleteContents(); // delete auto-delete server items
	virtual LPMONIKER GetMoniker(OLEGETMONIKER nAssign);
	virtual COleServerItem* OnGetLinkedItem(LPCTSTR lpszItemName);
		// return item for the named linked item (only if supporting links)
	virtual BOOL CanCloseFrame(CFrameWnd* pFrame);

protected:
	// overrides to handle server user-interface
	virtual BOOL SaveModified();        // return TRUE if ok to continue
	virtual HMENU GetDefaultMenu();     // return menu based on doc type
	virtual HACCEL GetDefaultAccelerator(); // return accel table based on doc type
	virtual BOOL GetFileTypeString(CString& rString);

	// IPersistStorage implementation
	virtual void OnNewEmbedding(LPSTORAGE lpStorage);
	virtual void OnOpenEmbedding(LPSTORAGE lpStorage);
	virtual void OnSaveEmbedding(LPSTORAGE lpStorage);

	// Implementation helpers
	void NotifyAllItems(OLE_NOTIFICATION wNotification, DWORD dwParam);
	BOOL SaveModifiedPrompt();
	void ConnectView(CWnd* pParentWnd, CView* pView);
	void UpdateUsingHostObj(UINT nIDS, CCmdUI* pCmdUI);

// Message Maps
	//{{AFX_MSG(COleServerDoc)
	afx_msg void OnFileUpdate();
	afx_msg void OnFileSaveCopyAs();
	afx_msg void OnUpdateFileUpdate(CCmdUI* pCmdUI);
	afx_msg void OnUpdateFileExit(CCmdUI* pCmdUI);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

// Interface Maps
public:
	BEGIN_INTERFACE_PART(PersistStorage, IPersistStorage)
		INIT_INTERFACE_PART(COleServerDoc, PersistStorage)
		STDMETHOD(GetClassID)(LPCLSID);
		STDMETHOD(IsDirty)();
		STDMETHOD(InitNew)(LPSTORAGE);
		STDMETHOD(Load)(LPSTORAGE);
		STDMETHOD(Save)(LPSTORAGE, BOOL);
		STDMETHOD(SaveCompleted)(LPSTORAGE);
		STDMETHOD(HandsOffStorage)();
	END_INTERFACE_PART(PersistStorage)

	BEGIN_INTERFACE_PART(OleObject, IOleObject)
		INIT_INTERFACE_PART(COleServerDoc, OleObject)
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

	BEGIN_INTERFACE_PART(DataObject, IDataObject)
		INIT_INTERFACE_PART(COleServerDoc, DataObject)
		STDMETHOD(GetData)(LPFORMATETC, LPSTGMEDIUM);
		STDMETHOD(GetDataHere)(LPFORMATETC, LPSTGMEDIUM);
		STDMETHOD(QueryGetData)(LPFORMATETC);
		STDMETHOD(GetCanonicalFormatEtc)(LPFORMATETC, LPFORMATETC);
		STDMETHOD(SetData)(LPFORMATETC, LPSTGMEDIUM, BOOL);
		STDMETHOD(EnumFormatEtc)(DWORD, LPENUMFORMATETC*);
		STDMETHOD(DAdvise)(LPFORMATETC, DWORD, LPADVISESINK, LPDWORD);
		STDMETHOD(DUnadvise)(DWORD);
		STDMETHOD(EnumDAdvise)(LPENUMSTATDATA*);
	END_INTERFACE_PART(DataObject)

	BEGIN_INTERFACE_PART(OleInPlaceObject, IOleInPlaceObject)
		INIT_INTERFACE_PART(COleServerDoc, OleInPlaceObject)
		STDMETHOD(GetWindow)(HWND*);
		STDMETHOD(ContextSensitiveHelp)(BOOL);
		STDMETHOD(InPlaceDeactivate)();
		STDMETHOD(UIDeactivate)();
		STDMETHOD(SetObjectRects)(LPCRECT, LPCRECT);
		STDMETHOD(ReactivateAndUndo)();
	END_INTERFACE_PART(OleInPlaceObject)

	BEGIN_INTERFACE_PART(OleInPlaceActiveObject, IOleInPlaceActiveObject)
		INIT_INTERFACE_PART(COleServerDoc, OleInPlaceActiveObject)
		STDMETHOD(GetWindow)(HWND*);
		STDMETHOD(ContextSensitiveHelp)(BOOL);
		STDMETHOD(TranslateAccelerator)(LPMSG);
		STDMETHOD(OnFrameWindowActivate)(BOOL);
		STDMETHOD(OnDocWindowActivate)(BOOL);
		STDMETHOD(ResizeBorder)(LPCRECT, LPOLEINPLACEUIWINDOW, BOOL);
		STDMETHOD(EnableModeless)(BOOL);
	END_INTERFACE_PART(OleInPlaceActiveObject)

	DECLARE_INTERFACE_MAP()

	friend class COleServer;
	friend class COleServerItem;
	friend class CDocObjectServer;
};

//////////////////////////////////////////////////////////////////////////////
// COleIPFrameWnd

class COleCntrFrameWnd;

class COleIPFrameWnd : public CFrameWnd
{
	DECLARE_DYNCREATE(COleIPFrameWnd)

// Constructors
public:
	COleIPFrameWnd();

// Overridables
public:
	virtual BOOL OnCreateControlBars(CWnd* pWndFrame, CWnd* pWndDoc);
		// create control bars on container windows (pWndDoc can be NULL)
	virtual BOOL OnCreateControlBars(CFrameWnd* pWndFrame, CFrameWnd* pWndDoc);
		// create control bars on container windows (pWndDoc can be NULL)

	virtual void RepositionFrame(LPCRECT lpPosRect, LPCRECT lpClipRect);
		// Advanced: reposition frame to wrap around new lpPosRect

// Implementation
public:
	BOOL m_bUIActive;   // TRUE if currently in uiacitve state

	virtual BOOL LoadFrame(UINT nIDResource,
		DWORD dwDefaultStyle = WS_CHILD|WS_BORDER|WS_CLIPSIBLINGS,
		CWnd* pParentWnd = NULL,
		CCreateContext* pContext = NULL);
	virtual void RecalcLayout(BOOL bNotify = TRUE);
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	virtual LRESULT OnSetMessageString(WPARAM wParam, LPARAM lParam);
	virtual ~COleIPFrameWnd();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	// in-place state
	OLEINPLACEFRAMEINFO m_frameInfo;
	LPOLEINPLACEFRAME m_lpFrame;
	LPOLEINPLACEUIWINDOW m_lpDocFrame;
	COleCntrFrameWnd* m_pMainFrame;
	COleCntrFrameWnd* m_pDocFrame;

	HMENU m_hSharedMenu;
	OLEMENUGROUPWIDTHS m_menuWidths;
	HOLEMENU m_hOleMenu;
	CRect m_rectPos;            // client area rect of the item
	CRect m_rectClip;           // area to which frame should be clipped
	BOOL m_bInsideRecalc;

	HMENU _m_Reserved;

	// Advanced: in-place activation virtual implementation
	virtual BOOL BuildSharedMenu();
	virtual void DestroySharedMenu();
	virtual HMENU GetInPlaceMenu();

	// Advanced: possible override to change in-place sizing behavior
	virtual void OnRequestPositionChange(LPCRECT lpRect);

protected:
	//{{AFX_MSG(COleIPFrameWnd)
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg LRESULT OnRecalcParent(WPARAM wParam, LPARAM lParam);
	afx_msg void OnIdleUpdateCmdUI();
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnDestroy();
	afx_msg LRESULT OnResizeChild(WPARAM wParam, LPARAM lParam);
	afx_msg void OnContextHelp();
	afx_msg void OnUpdateControlBarMenu(CCmdUI* pCmdUI);
	afx_msg BOOL OnBarCheck(UINT nID);
	afx_msg void OnWindowPosChanging(LPWINDOWPOS lpWndPos);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

	friend class COleServerDoc;
	friend class COleCntrFrameWnd;
	friend class CDocObjectServer;
};

/////////////////////////////////////////////////////////////////////////////
// COleResizeBar - supports in-place resizing in server applications

class COleResizeBar : public CControlBar
{
	DECLARE_DYNAMIC(COleResizeBar)

// Constructors
public:
	COleResizeBar();
	BOOL Create(CWnd* pParentWnd, DWORD dwStyle = WS_CHILD | WS_VISIBLE,
		UINT nID = AFX_IDW_RESIZE_BAR);

// Implementation
public:
	virtual ~COleResizeBar();
	virtual void OnUpdateCmdUI(CFrameWnd* pTarget, BOOL bDisableIfNoHndler);

protected:
	CRectTracker m_tracker;     // implemented with a tracker

protected:
	//{{AFX_MSG(COleResizeBar)
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnPaint();
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg BOOL OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message);
	afx_msg void OnLButtonDown(UINT, CPoint point);
	afx_msg LRESULT OnSizeParent(WPARAM wParam, LPARAM lParam);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// COleStreamFile - implementation of CFile which uses an IStream

class COleStreamFile : public CFile
{
	DECLARE_DYNAMIC(COleStreamFile)

// Constructors and Destructors
public:
	COleStreamFile(LPSTREAM lpStream = NULL);

// Operations
	// Note: OpenStream and CreateStream can accept eith STGM_ flags or
	//  CFile::OpenFlags bits since common values are guaranteed to have
	//  the same semantics.
	BOOL OpenStream(LPSTORAGE lpStorage, LPCTSTR lpszStreamName,
		DWORD nOpenFlags = modeReadWrite|shareExclusive,
		CFileException* pError = NULL);
	BOOL CreateStream(LPSTORAGE lpStorage, LPCTSTR lpszStreamName,
		DWORD nOpenFlags = modeReadWrite|shareExclusive|modeCreate,
		CFileException* pError = NULL);

	BOOL CreateMemoryStream(CFileException* pError = NULL);

	// attach & detach can be used when Open/Create functions aren't adequate
	void Attach(LPSTREAM lpStream);
	LPSTREAM Detach();

	IStream* GetStream() const;
	// Returns the current stream

// Implementation
public:
	LPSTREAM m_lpStream;
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	virtual ~COleStreamFile();

	// attributes for implementation
	BOOL GetStatus(CFileStatus& rStatus) const;
	virtual DWORD GetPosition() const;

	virtual const CString GetStorageName() const;

	// overrides for implementation
	virtual CFile* Duplicate() const;
	virtual LONG Seek(LONG lOff, UINT nFrom);
	virtual void SetLength(DWORD dwNewLen);
	virtual DWORD GetLength() const;
	virtual UINT Read(void* lpBuf, UINT nCount);
	virtual void Write(const void* lpBuf, UINT nCount);
	virtual void LockRange(DWORD dwPos, DWORD dwCount);
	virtual void UnlockRange(DWORD dwPos, DWORD dwCount);
	virtual void Abort();
	virtual void Flush();
	virtual void Close();

protected:
	CString m_strStorageName;
};

/////////////////////////////////////////////////////////////////////////////
// CMonikerFile - implementation of COleStreamFile that uses an IMoniker to
//                get the IStream

class CMonikerFile: public COleStreamFile
{
	DECLARE_DYNAMIC(CMonikerFile)

public:
	CMonikerFile();

	virtual BOOL Open(LPCTSTR lpszURL, CFileException* pError=NULL);
	// Uses synchronous URLMonikers to create a moniker.
	// Opens the URL specified.
	// If provided, pError will be set in case of error.
	// Return value: TRUE if successful, FALSE otherwise.

	virtual BOOL Open(IMoniker* pMoniker, CFileException* pError=NULL);
	// Binds to the provided moniker to obtain a stream.
	// If provided, pError will be set in case of error.
	// Return value: TRUE if successful, FALSE otherwise.

	virtual void Close();
	// Detaches the stream, Release()s it, and the moniker.  Close may be
	// called on unopened, or already closed streams.

	BOOL Detach(CFileException* pError = NULL);
	// Closes the stream.  If there is an error when closing, then the
	// error code will be placed in pError and the function will return FALSE.

	IMoniker* GetMoniker() const;
	// Returns the current moniker.  The moniker returned is not AddRef()'ed.

protected:
// Overidables
	IBindCtx* CreateBindContext(CFileException* pError);
	// A hook so users can provide a particular IBindCtx, potentially one
	// on which the user has registered one or more objects.

// Implementation
protected:
	virtual BOOL Open(LPCTSTR lpszUrl, IBindHost* pBindHost,
		IBindStatusCallback* pBSC, IBindCtx* pBindCtx, CFileException* pError);
	BOOL Attach(LPCTSTR lpszUrl, IBindHost* pBindHost,
		IBindStatusCallback* pBSC, IBindCtx* pBindCtx, CFileException* pError);
	virtual BOOL Open(IMoniker* pMoniker, IBindHost* pBindHost,
		IBindStatusCallback* pBSC, IBindCtx* pBindCtx, CFileException* pError);

	BOOL Attach(IMoniker* pMoniker, IBindHost* pBindHost,
		IBindStatusCallback* pBSC, IBindCtx* pBindCtx, CFileException* pError);

	virtual BOOL PostBindToStream(CFileException* pError);

	static IBindHost* CreateBindHost();
public:
	virtual ~CMonikerFile();
	// Closes the stream, and releases the moniker if needed.

	virtual void Flush();

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
	// Calls COleStreamFile::Dump(), and prints out moniker value.
#endif

protected:
	IPTR(IMoniker) m_Moniker;
	// The moniker provided or created to which this class is bound.

	CMonikerFile(const CMonikerFile&);
	// Prevents copying.
};

/////////////////////////////////////////////////////////////////////////////
// CAsyncMonikerFile - implementation of COleStreamFile that uses an
//                     asynchronous IMoniker to get the IStream

class _AfxBindStatusCallback; // Forward declaration

class CAsyncMonikerFile: public CMonikerFile
{
	DECLARE_DYNAMIC(CAsyncMonikerFile)

public:
	CAsyncMonikerFile();
	// Creates the IBindStatusCallback used internally to provide asynchronous
	// operation.

	//All Open overloads call one of these two.
	virtual BOOL Open(LPCTSTR lpszURL, IBindHost* pBindHost,
		CFileException* pError=NULL);
	virtual BOOL Open(IMoniker* pMoniker, IBindHost* pBindHost,
		CFileException* pError=NULL);

	//Open overloads that take monikers
	virtual BOOL Open(IMoniker* pMoniker, CFileException* pError=NULL);
	virtual BOOL Open(IMoniker* pMoniker, IServiceProvider* pServiceProvider,
		CFileException* pError=NULL);
	virtual BOOL Open(IMoniker* pMoniker, IUnknown* pUnknown,
		CFileException* pError=NULL);

	//Open overloads that take strings
	virtual BOOL Open(LPCTSTR lpszURL, CFileException* pError=NULL);
	virtual BOOL Open(LPCTSTR lpszURL, IServiceProvider* pServiceProvider,
		CFileException* pError=NULL);
	virtual BOOL Open(LPCTSTR lpszURL, IUnknown* pUnknown,
		CFileException* pError=NULL);

	virtual void Close();

	IBinding* GetBinding() const;
	// Returns the binding provided when the asychronous transfer begins.
	// With the IBinding*, the user may abort, or pause the transfer.
	// NULL may be returned if for any reason the transfer could not be
	// made asynchronous, or if the IBinding* has not yet been provided by
	// the system.

	FORMATETC* GetFormatEtc() const;
	// Returns the FORMATETC for the currently opened stream.  NULL will be
	// returned if this is called from outside the context of OnDataAvailable.
	// If you want to keep the FORMATETC beyond this call, make a copy of it.
	// The FORMATETC indicates the format of the data in the stream.

protected:
// Overidables
	virtual IUnknown* CreateBindStatusCallback(IUnknown* pUnkControlling);

	virtual DWORD GetBindInfo() const;
	// Returns the settings returned by IBindStatusCallback::GetBindInfo.
	// The default values returned should work for most cases and should not
	// be changed lightly.

	virtual LONG GetPriority() const;
	// Returns the priority at which the asynchronous transfer will take
	// place.  The value is one of the standard thread priority flags.
	// By default THREAD_PRIORITY_NORMAL is returned.

	virtual void OnDataAvailable(DWORD dwSize, DWORD bscfFlag);
	// Called when there is data available to be read.  dwSize indicates
	// the cumulative number of bytes which can be read.  The bscfFlag may be used
	// to identify first, last, and intermediate blocks of data.

	virtual void OnLowResource();
	// This is called when resources are low.

	virtual void OnStartBinding();
	// Called when the binding is starting up.

	virtual void OnProgress(ULONG ulProgress, ULONG ulProgressMax,
		ULONG ulStatusCode, LPCTSTR szStatusText);

	virtual void OnStopBinding(HRESULT hresult, LPCTSTR szError);
	// Called when the transfer is stopped.  This function releases the
	// IBinding and should nearly always be call when overidden.

// Implementation
public:
	virtual ~CAsyncMonikerFile();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
	// Calls CMonikerFile::Dump(), and prints out IBinding,
	// IBindStatusCallback, and m_pFormatEtc values.
#endif
	virtual UINT Read(void* lpBuf, UINT nCount);

protected:
	friend class _AfxBindStatusCallback;
	_AfxBindStatusCallback* m_pAfxBSCCurrent;
	BOOL m_bStopBindingReceived;
	void EndCallbacks();

	IPTR(IBinding) m_Binding;
	FORMATETC* m_pFormatEtc;

	void SetBinding(IBinding* pBinding);
	// Sets and AddRefs m_Binding

	void SetFormatEtc(FORMATETC* pFormatEtc);
	// Sets the FORMATETC for the current stream.

	virtual BOOL PostBindToStream(CFileException* pError);
};

/////////////////////////////////////////////////////////////////////////////
// COleDropSource (advanced drop source support)

class COleDropSource : public CCmdTarget
{
// Constructors
public:
	COleDropSource();

// Overridables
	virtual SCODE QueryContinueDrag(BOOL bEscapePressed, DWORD dwKeyState);
	virtual SCODE GiveFeedback(DROPEFFECT dropEffect);
	virtual BOOL OnBeginDrag(CWnd* pWnd);

// Implementation
public:
#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif

public:
	BEGIN_INTERFACE_PART(DropSource, IDropSource)
		INIT_INTERFACE_PART(COleDropSource, DropSource)
		STDMETHOD(QueryContinueDrag)(BOOL, DWORD);
		STDMETHOD(GiveFeedback)(DWORD);
	END_INTERFACE_PART(DropSource)

	DECLARE_INTERFACE_MAP()

	CRect m_rectStartDrag;  // when mouse leaves this rect, drag drop starts
	BOOL m_bDragStarted;    // has drag really started yet?
	DWORD m_dwButtonCancel; // which button will cancel (going down)
	DWORD m_dwButtonDrop;   // which button will confirm (going up)

	// metrics for drag start determination
	static AFX_DATA UINT nDragMinDist;  // min. amount mouse must move for drag
	static AFX_DATA UINT nDragDelay;    // delay before drag starts

	friend class COleDataSource;
};

/////////////////////////////////////////////////////////////////////////////
// COleDropTarget (advanced drop target support)

class COleDropTarget : public CCmdTarget
{
// Constructors
public:
	COleDropTarget();

// Operations
	BOOL Register(CWnd* pWnd);
	virtual void Revoke();  // virtual for implementation

// Overridables
	virtual DROPEFFECT OnDragEnter(CWnd* pWnd, COleDataObject* pDataObject,
		DWORD dwKeyState, CPoint point);
	virtual DROPEFFECT OnDragOver(CWnd* pWnd, COleDataObject* pDataObject,
		DWORD dwKeyState, CPoint point);
	virtual BOOL OnDrop(CWnd* pWnd, COleDataObject* pDataObject,
		DROPEFFECT dropEffect, CPoint point);
	virtual DROPEFFECT OnDropEx(CWnd* pWnd, COleDataObject* pDataObject,
		DROPEFFECT dropDefault, DROPEFFECT dropList, CPoint point);
	virtual void OnDragLeave(CWnd* pWnd);
	virtual DROPEFFECT OnDragScroll(CWnd* pWnd, DWORD dwKeyState,
		CPoint point);

// Implementation
public:
	virtual ~COleDropTarget();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	HWND m_hWnd;            // HWND this IDropTarget is attached to
	LPDATAOBJECT m_lpDataObject;    // != NULL between OnDragEnter, OnDragLeave
	UINT m_nTimerID;        // != MAKEWORD(-1, -1) when in scroll area
	DWORD m_dwLastTick;     // only valid when m_nTimerID valid
	UINT m_nScrollDelay;    // time to next scroll

	// metrics for drag-scrolling
	static AFX_DATA int nScrollInset;
	static AFX_DATA UINT nScrollDelay;
	static AFX_DATA UINT nScrollInterval;

	// implementation helpers
	void SetupTimer(CView* pView, UINT nTimerID);
	void CancelTimer(CWnd* pWnd);

// Interface Maps
public:
	BEGIN_INTERFACE_PART(DropTarget, IDropTarget)
		INIT_INTERFACE_PART(COleDropTarget, DropTarget)
		STDMETHOD(DragEnter)(LPDATAOBJECT, DWORD, POINTL, LPDWORD);
		STDMETHOD(DragOver)(DWORD, POINTL, LPDWORD);
		STDMETHOD(DragLeave)();
		STDMETHOD(Drop)(LPDATAOBJECT, DWORD, POINTL pt, LPDWORD);
	END_INTERFACE_PART(DropTarget)

	DECLARE_INTERFACE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// COleMessageFilter (implements IMessageFilter)

class COleMessageFilter : public CCmdTarget
{
// Constructors
public:
	COleMessageFilter();

// Operations
	BOOL Register();
	void Revoke();

	// for controlling the busy state of the server application (called app)
	virtual void BeginBusyState();
	virtual void EndBusyState();
	void SetBusyReply(SERVERCALL nBusyReply);

	// for controlling actions taken against rejected/retried calls
	void SetRetryReply(DWORD nRetryReply = 0);
		// only used when the "not responding" dialog is disabled
	void SetMessagePendingDelay(DWORD nTimeout = 5000);
		// used to determine amount of time before significant message
	void EnableBusyDialog(BOOL bEnableBusy = TRUE);
	void EnableNotRespondingDialog(BOOL bEnableNotResponding = TRUE);
		// used to enable/disable the two types of busy dialogs

// Overridables
	virtual BOOL OnMessagePending(const MSG* pMsg);
		// return TRUE to eat the message (usually only if processed)

// Implementation
public:
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	virtual ~COleMessageFilter();
	virtual BOOL IsSignificantMessage(MSG* pMsg);
		// determine if any significant messages are present in the queue
	virtual int OnBusyDialog(HTASK htaskBusy);
	virtual int OnNotRespondingDialog(HTASK htaskBusy);
		// these functions display the busy dialog

protected:
	BOOL m_bRegistered;
	LONG m_nBusyCount;  // for BeginBusyState & EndBusyState
	BOOL m_bEnableBusy;
	BOOL m_bEnableNotResponding;
	BOOL m_bUnblocking;
	DWORD m_nRetryReply;    // only used if m_bEnableNotResponding == FALSE
	DWORD m_nBusyReply;
	DWORD m_nTimeout;

// Interface Maps
public:
	BEGIN_INTERFACE_PART(MessageFilter, IMessageFilter)
		INIT_INTERFACE_PART(COleMessageFilter, MessageFilter)
		STDMETHOD_(DWORD, HandleInComingCall)(DWORD, HTASK, DWORD,
			LPINTERFACEINFO);
		STDMETHOD_(DWORD, RetryRejectedCall)(HTASK, DWORD, DWORD);
		STDMETHOD_(DWORD, MessagePending)(HTASK, DWORD, DWORD);
	END_INTERFACE_PART(MessageFilter)

	DECLARE_INTERFACE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// message map entries for OLE verbs

#define ON_STDOLEVERB(iVerb, memberFxn) \
	{ 0xC002, 0, (UINT)iVerb, (UINT)iVerb, (UINT)-1, \
		(AFX_PMSG)(BOOL (AFX_MSG_CALL CCmdTarget::*)(LPMSG, HWND, LPCRECT))&memberFxn },

#define ON_OLEVERB(idsVerbName, memberFxn) \
	{ 0xC002, 0, 1, 1, idsVerbName, \
		(AFX_PMSG)(BOOL (AFX_MSG_CALL CCmdTarget::*)(LPMSG, HWND, LPCRECT))&memberFxn },

/////////////////////////////////////////////////////////////////////////////
// global helpers and debugging

void AFXAPI AfxOleSetEditMenu(COleClientItem* pClient, CMenu* pMenu,
	UINT iMenuItem, UINT nIDVerbMin, UINT nIDVerbMax = 0, UINT nIDConvert = 0);

#ifdef _DEBUG
// Mapping SCODEs to readable text
LPCTSTR AFXAPI AfxGetFullScodeString(SCODE sc);
LPCTSTR AFXAPI AfxGetScodeString(SCODE sc);
LPCTSTR AFXAPI AfxGetScodeRangeString(SCODE sc);
LPCTSTR AFXAPI AfxGetSeverityString(SCODE sc);
LPCTSTR AFXAPI AfxGetFacilityString(SCODE sc);

// Mapping IIDs to readable text
LPCTSTR AFXAPI AfxGetIIDString(REFIID iid);
#endif

/////////////////////////////////////////////////////////////////////////////
// Inline function declarations

#ifdef _AFX_PACKING
#pragma pack(pop)
#endif

#ifdef _AFX_ENABLE_INLINES
#define _AFXOLE_INLINE AFX_INLINE
#define _AFXOLECLI_INLINE AFX_INLINE
#define _AFXOLESVR_INLINE AFX_INLINE
#define _AFXOLEDOBJ_INLINE AFX_INLINE
#define _AFXOLEMONIKER_INLINE AFX_INLINE
#include <afxole.inl>
#undef _AFXOLE_INLINE
#undef _AFXOLECLI_INLINE
#undef _AFXOLESVR_INLINE
#undef _AFXOLEDOBJ_INLINE
#undef _AFXOLEMONIKER_INLINE
#endif

#undef AFX_DATA
#define AFX_DATA

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, on)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, off)
#endif

#endif //__AFXOLE_H__

/////////////////////////////////////////////////////////////////////////////x
