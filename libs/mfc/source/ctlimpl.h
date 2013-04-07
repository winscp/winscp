// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#ifndef __CTLIMPL_H__
#define __CTLIMPL_H__

// MFC data definition for data exported from the runtime DLL

#undef AFX_DATA
#define AFX_DATA AFX_OLE_DATA

/////////////////////////////////////////////////////////////////////////////
// Codes for COleControl::SendAdvise
//......Code.........................Method called
#define OBJECTCODE_SAVED          0  //IOleAdviseHolder::SendOnSave
#define OBJECTCODE_CLOSED         1  //IOleAdviseHolder::SendOnClose
#define OBJECTCODE_RENAMED        2  //IOleAdviseHolder::SendOnRename
#define OBJECTCODE_SAVEOBJECT     3  //IOleClientSite::SaveObject
#define OBJECTCODE_DATACHANGED    4  //IDataAdviseHolder::SendOnDataChange
#define OBJECTCODE_SHOWWINDOW     5  //IOleClientSite::OnShowWindow(TRUE)
#define OBJECTCODE_HIDEWINDOW     6  //IOleClientSite::OnShowWindow(FALSE)
#define OBJECTCODE_SHOWOBJECT     7  //IOleClientSite::ShowObject
#define OBJECTCODE_VIEWCHANGED    8  //IOleAdviseHolder::SendOnViewChange


/////////////////////////////////////////////////////////////////////////////
// Typedefs

typedef LPVOID* LPLPVOID;

/////////////////////////////////////////////////////////////////////////////
// Functions

LPSTREAM AFXAPI _AfxGetArchiveStream(CArchive& ar, CArchiveStream& stm);
CLIPFORMAT AFXAPI _AfxGetClipboardFormatConvertVBX();
CLIPFORMAT AFXAPI _AfxGetClipboardFormatPersistPropset();
BOOL AFXAPI _AfxOleMatchPropsetClipFormat(CLIPFORMAT cfFormat, LPCLSID lpFmtID);
BOOL AFXAPI _AfxCopyPropValue(VARTYPE vtProp, void* pvDest, const void * pvSrc);
BOOL AFXAPI _AfxPeekAtClassIDInStream(LPSTREAM pstm, LPCLSID lpClassID);
BOOL AFXAPI _AfxIsSamePropValue(VARTYPE vtProp, const void* pv1, const void* pv2);
BOOL AFXAPI _AfxIsSameFont(CFontHolder& font, const FONTDESC* pFontDesc,
	LPFONTDISP pFontDispAmbient);
BOOL AFXAPI _AfxIsSameUnknownObject(REFIID iid, LPUNKNOWN pUnk1, LPUNKNOWN pUnk2);
BOOL AFXAPI _AfxInitBlob(HGLOBAL* phDst, void* pvSrc);
BOOL AFXAPI _AfxCopyBlob(HGLOBAL* phDst, HGLOBAL hSrc);
LPFONT AFXAPI _AfxCreateFontFromStream(LPSTREAM);
BOOL AFXAPI _AfxTreatAsClass(REFCLSID clsidOld, REFCLSID clsidNew);
void AFXAPI _AfxXformSizeInPixelsToHimetric(HDC, LPSIZEL, LPSIZEL);
void AFXAPI _AfxXformSizeInHimetricToPixels(HDC, LPSIZEL, LPSIZEL);
void AFXAPI _AfxDrawBorders(CDC* pDC, CRect& rc, BOOL bBorder, BOOL bClientEdge);

/////////////////////////////////////////////////////////////////////////////
// _AFXCTL_ADVISE_INFO - Information about an advise sink

struct _AFXCTL_ADVISE_INFO
{
	DWORD m_dwAspects;
	DWORD m_dwAdvf;
	LPADVISESINK m_pAdvSink;

	_AFXCTL_ADVISE_INFO() : m_dwAspects(0), m_dwAdvf(0), m_pAdvSink(NULL) {}
};

/////////////////////////////////////////////////////////////////////////////
// _AFXCTL_AMBIENT_CACHE - cache of common ambient property values

class _AFXCTL_AMBIENT_CACHE : public CNoTrackObject
{
// Constructor
public:
	_AFXCTL_AMBIENT_CACHE();

// Attributes
	BOOL m_bValid;
	DWORD m_dwAmbientFlags;
	OLE_COLOR m_colorFore;
	OLE_COLOR m_colorBack;
	IFont* m_pFont;
	void* m_pReserved;
	DWORD m_dwAppearance;

// Operations
	void Cache(QACONTAINER* pQAContainer);
};

EXTERN_THREAD_LOCAL(_AFXCTL_AMBIENT_CACHE, _afxAmbientCache)

/////////////////////////////////////////////////////////////////////////////
// CControlFrameWnd - used for a control's "open" (non-in-place) state.

class CControlFrameWnd : public CWnd
{
public:
	CControlFrameWnd(COleControl* pCtrl);
	virtual BOOL Create(LPCTSTR pszTitle);
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);

protected:
	virtual void PostNcDestroy();

	COleControl* m_pCtrl;

	//{{AFX_MSG(CControlFrameWnd)
	afx_msg void OnClose();
	afx_msg void OnActivate(UINT nState, CWnd* pWndOther, BOOL bMinimized);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////
// CReflectorWnd - reflects window messages to a subclassed control.

class CReflectorWnd : public CWnd
{
public:
	CReflectorWnd() : m_pCtrl(NULL) { }

	BOOL Create(const CRect& rect, HWND hWndParent);
	void SetControl(COleControl* pCtrl);

protected:
	virtual LRESULT WindowProc(UINT uMsg, WPARAM wParam, LPARAM lParam);
	virtual void PostNcDestroy();

	COleControl* m_pCtrl;
};


/////////////////////////////////////////////////////////////////////////////
// CParkingWnd - "parking space" for not-yet-activated subclassed controls

class CParkingWnd : public CWnd
{
public:
	CParkingWnd()
		{ AfxDeferRegisterClass(AFX_WNDOLECONTROL_REG);
		  CreateEx(WS_EX_NOPARENTNOTIFY|WS_EX_TOOLWINDOW,
			AFX_WNDOLECONTROL, NULL, WS_VISIBLE|WS_CHILD,
			-1000, -1000, 1, 1, ::GetDesktopWindow(), 0); }
	virtual LRESULT WindowProc(UINT uMsg, WPARAM wParam, LPARAM lParam);

protected:
	CMapPtrToPtr m_idMap;
};


/////////////////////////////////////////////////////////////////////////////
//  Property sets

typedef struct tagSECTIONHEADER
{
	DWORD       cbSection ;
	DWORD       cProperties ;  // Number of props.
} SECTIONHEADER, *LPSECTIONHEADER ;

typedef struct tagPROPERTYIDOFFSET
{
	DWORD       propertyID;
	DWORD       dwOffset;
} PROPERTYIDOFFSET, *LPPROPERTYIDOFFSET;

typedef struct tagPROPHEADER
{
	WORD        wByteOrder ;    // Always 0xFFFE
	WORD        wFormat ;       // Always 0
	DWORD       dwOSVer ;       // System version
	CLSID       clsID ;         // Application CLSID
	DWORD       cSections ;     // Number of sections (must be at least 1)
} PROPHEADER, *LPPROPHEADER ;

typedef struct tagFORMATIDOFFSET
{
	GUID        formatID;
	DWORD       dwOffset;
} FORMATIDOFFSET, *LPFORMATIDOFFSET;


/////////////////////////////////////////////////////////////////////////////
// CProperty

class CProperty
{
	friend class CPropertySet ;
	friend class CPropertySection ;

public:
// Construction
	CProperty( void ) ;
	CProperty( DWORD dwID, const LPVOID pValue, DWORD dwType ) ;

// Attributes
	BOOL    Set( DWORD dwID, const LPVOID pValue, DWORD dwType ) ;
	BOOL    Set( const LPVOID pValue, DWORD dwType ) ;
	BOOL    Set( const LPVOID pValue ) ;
	LPVOID  Get( DWORD* pcb ) ;     // Returns pointer to actual value
	LPVOID  Get( void ) ;           // Returns pointer to actual value
	DWORD   GetType( void ) ;       // Returns property type
	void    SetType( DWORD dwType ) ;
	DWORD   GetID( void ) ;
	void    SetID( DWORD dwPropID ) ;

	LPVOID  GetRawValue( void ) ;   // Returns pointer internal value (may
									// include size information)
// Operations
	BOOL    WriteToStream( IStream* pIStream ) ;
	BOOL    ReadFromStream( IStream* pIStream ) ;

private:
	DWORD       m_dwPropID ;
	DWORD       m_dwType ;
	LPVOID      m_pValue ;

	LPVOID  AllocValue(ULONG cb);
	void    FreeValue();

public:
	~CProperty() ;
} ;


/////////////////////////////////////////////////////////////////////////////
// CPropertySection

class CPropertySection
{
	friend class CPropertySet ;
	friend class CProperty ;

public:
// Construction
	CPropertySection( void ) ;
	CPropertySection( CLSID FormatID ) ;

// Attributes
	CLSID   GetFormatID( void ) ;
	void    SetFormatID( CLSID FormatID ) ;

	BOOL    Set( DWORD dwPropID, LPVOID pValue, DWORD dwType ) ;
	BOOL    Set( DWORD dwPropID, LPVOID pValue ) ;
	LPVOID  Get( DWORD dwPropID, DWORD* pcb ) ;
	LPVOID  Get( DWORD dwPropID ) ;
	void    Remove( DWORD dwPropID ) ;
	void    RemoveAll() ;

	CProperty* GetProperty( DWORD dwPropID ) ;
	void AddProperty( CProperty* pProp ) ;

	DWORD   GetSize( void ) ;
	DWORD   GetCount( void ) ;
	CPtrList* GetList( void ) ;

	BOOL    GetID( LPCTSTR pszName, DWORD* pdwPropID ) ;
	BOOL    SetName( DWORD dwPropID, LPCTSTR pszName ) ;

	BOOL    SetSectionName( LPCTSTR pszName );
	LPCTSTR GetSectionName( void );

// Operations
	BOOL    WriteToStream( IStream* pIStream ) ;
	BOOL    ReadFromStream( IStream* pIStream, LARGE_INTEGER liPropSet ) ;
	BOOL    WriteNameDictToStream( IStream* pIStream ) ;
	BOOL    ReadNameDictFromStream( IStream* pIStream ) ;

private:
// Implementation
	CLSID           m_FormatID ;
	SECTIONHEADER   m_SH ;
	// List of properties (CProperty)
	CPtrList         m_PropList ;
	// Dictionary of property names
	CMapStringToPtr m_NameDict ;
	CString         m_strSectionName;

public:
	~CPropertySection();
} ;


/////////////////////////////////////////////////////////////////////////////
// CPropertySet

class CPropertySet
{
	friend class CPropertySection ;
	friend class CProperty ;

public:
// Construction
	CPropertySet( void ) ;
	CPropertySet( CLSID clsID )  ;

// Attributes
	BOOL    Set( CLSID FormatID, DWORD dwPropID, LPVOID pValue, DWORD dwType ) ;
	BOOL    Set( CLSID FormatID, DWORD dwPropID, LPVOID pValue ) ;
	LPVOID  Get( CLSID FormatID, DWORD dwPropID, DWORD* pcb ) ;
	LPVOID  Get( CLSID FormatID, DWORD dwPropID ) ;
	void    Remove( CLSID FormatID, DWORD dwPropID ) ;
	void    Remove( CLSID FormatID ) ;
	void    RemoveAll( ) ;

	CProperty* GetProperty( CLSID FormatID, DWORD dwPropID ) ;
	void AddProperty( CLSID FormatID, CProperty* pProp ) ;
	CPropertySection* GetSection( CLSID FormatID ) ;
	CPropertySection* AddSection( CLSID FormatID ) ;
	void AddSection( CPropertySection* psect ) ;

	WORD    GetByteOrder( void ) ;
	WORD    GetFormatVersion( void ) ;
	void    SetFormatVersion( WORD wFmtVersion ) ;
	DWORD   GetOSVersion( void ) ;
	void    SetOSVersion( DWORD dwOSVer ) ;
	CLSID   GetClassID( void ) ;
	void    SetClassID( CLSID clsid ) ;
	DWORD   GetCount( void ) ;
	CPtrList* GetList( void ) ;

// Operations
	BOOL    WriteToStream( IStream* pIStream ) ;
	BOOL    ReadFromStream( IStream* pIStream ) ;

// Implementation
private:
	PROPHEADER      m_PH ;
	CPtrList         m_SectionList ;

public:
	~CPropertySet();
} ;


/////////////////////////////////////////////////////////////////////////////
// CArchivePropExchange - for persistence in an archive.

class CArchivePropExchange : public CPropExchange
{
// Constructors
public:
	CArchivePropExchange(CArchive& ar);

// Operations
	virtual BOOL ExchangeProp(LPCTSTR pszPropName, VARTYPE vtProp,
				void* pvProp, const void* pvDefault = NULL);
	virtual BOOL ExchangeBlobProp(LPCTSTR pszPropName, HGLOBAL* phBlob,
				HGLOBAL hBlobDefault = NULL);
	virtual BOOL ExchangeFontProp(LPCTSTR pszPropName, CFontHolder& font,
				const FONTDESC* pFontDesc, LPFONTDISP pFontDispAmbient);
	virtual BOOL ExchangePersistentProp(LPCTSTR pszPropName,
				LPUNKNOWN* ppUnk, REFIID iid, LPUNKNOWN pUnkDefault);

// Implementation
protected:
	CArchive& m_ar;
};


/////////////////////////////////////////////////////////////////////////////
// CResetPropExchange - for resetting property state to defaults.

class CResetPropExchange : public CPropExchange
{
// Constructors
public:
	CResetPropExchange(void);

// Operations
	virtual BOOL ExchangeProp(LPCTSTR pszPropName, VARTYPE vtProp,
				void* pvProp, const void* pvDefault = NULL);
	virtual BOOL ExchangeBlobProp(LPCTSTR pszPropName, HGLOBAL* phBlob,
				HGLOBAL hBlobDefault = NULL);
	virtual BOOL ExchangeFontProp(LPCTSTR pszPropName, CFontHolder& font,
				const FONTDESC* pFontDesc, LPFONTDISP pFontDispAmbient);
	virtual BOOL ExchangePersistentProp(LPCTSTR pszPropName,
				LPUNKNOWN* ppUnk, REFIID iid, LPUNKNOWN pUnkDefault);
};


/////////////////////////////////////////////////////////////////////////////
// CPropsetPropExchange - for persistence in a property set.

class CPropsetPropExchange : public CPropExchange
{
// Constructors
public:
	CPropsetPropExchange(CPropertySection& psec, LPSTORAGE lpStorage,
		BOOL bLoading);

// Operations
	virtual BOOL ExchangeProp(LPCTSTR pszPropName, VARTYPE vtProp,
				void* pvProp, const void* pvDefault = NULL);
	virtual BOOL ExchangeBlobProp(LPCTSTR pszPropName, HGLOBAL* phBlob,
				HGLOBAL hBlobDefault = NULL);
	virtual BOOL ExchangeFontProp(LPCTSTR pszPropName, CFontHolder& font,
				const FONTDESC* pFontDesc, LPFONTDISP pFontDispAmbient);
	virtual BOOL ExchangePersistentProp(LPCTSTR pszPropName,
				LPUNKNOWN* ppUnk, REFIID iid, LPUNKNOWN pUnkDefault);

// Implementation
	CPropertySection& m_psec;
	LPSTORAGE m_lpStorage;
	DWORD m_dwPropID;
};

/////////////////////////////////////////////////////////////////////////////
// CAsyncPropExchange - for launching asynchronous downloads set.

class CAsyncPropExchange : public CPropExchange
{
// Constructors
public:
	CAsyncPropExchange(DWORD dwVersion);

// Operations
public:
	virtual BOOL ExchangeVersion(DWORD& dwVersionLoaded,
		DWORD dwVersionDefault, BOOL bConvert);

	virtual BOOL ExchangeProp(LPCTSTR pszPropName, VARTYPE vtProp,
				void* pvProp, const void* pvDefault = NULL);
	virtual BOOL ExchangeBlobProp(LPCTSTR pszPropName, HGLOBAL* phBlob,
				HGLOBAL hBlobDefault = NULL);
	virtual BOOL ExchangeFontProp(LPCTSTR pszPropName, CFontHolder& font,
				const FONTDESC* pFontDesc,
				LPFONTDISP pFontDispAmbient);
	virtual BOOL ExchangePersistentProp(LPCTSTR pszPropName,
				LPUNKNOWN* ppUnk, REFIID iid, LPUNKNOWN pUnkDefault);
};


/////////////////////////////////////////////////////////////////////////////
// COleDispatchExceptionEx - dispatch exception that includes an SCODE

class COleDispatchExceptionEx : public COleDispatchException
{
public:
	COleDispatchExceptionEx(LPCTSTR lpszDescription, UINT nHelpID, SCODE sc);
};

/////////////////////////////////////////////////////////////////////////////
// Reset MFC data definitions

#undef AFX_DATA
#define AFX_DATA

#endif  //__CTLIMPL_H__
