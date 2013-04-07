// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#undef AFX_DATA
#define AFX_DATA AFX_CORE_DATA

/////////////////////////////////////////////////////////////////////////////
// Auxiliary System/Screen metrics

struct AUX_DATA
{
	// system metrics
	int cxVScroll, cyHScroll;
	int cxIcon, cyIcon;

	int cxBorder2, cyBorder2;

	// device metrics for screen
	int cxPixelsPerInch, cyPixelsPerInch;

	// convenient system color
	HBRUSH hbrWindowFrame;
	HBRUSH hbrBtnFace;

	// color values of system colors used for CToolBar
	COLORREF clrBtnFace, clrBtnShadow, clrBtnHilite;
	COLORREF clrBtnText, clrWindowFrame;

	// standard cursors
	HCURSOR hcurWait;
	HCURSOR hcurArrow;
	HCURSOR hcurHelp;       // cursor used in Shift+F1 help

	// special GDI objects allocated on demand
	HFONT   hStatusFont;
	HFONT   hToolTipsFont;
	HBITMAP hbmMenuDot;

	// other system information
	UINT    nWinVer;        // Major.Minor version numbers
	BOOL    bWin95;         // TRUE if Windows 95 (not NT)
	BOOL    bWin4;          // TRUE if Windows 4.0
	BOOL    bNotWin4;       // TRUE if not Windows 4.0
	BOOL    bSmCaption;     // TRUE if WS_EX_SMCAPTION is supported
	BOOL    bMarked4;       // TRUE if marked as 4.0

// Implementation
	AUX_DATA();
	~AUX_DATA();
	void UpdateSysColors();
	void UpdateSysMetrics();
};

extern AFX_DATA AUX_DATA afxData;

/////////////////////////////////////////////////////////////////////////////
// _AFX_CTL3D_STATE

#ifndef _AFX_NO_CTL3D_SUPPORT

#undef AFX_DATA
#define AFX_DATA

class _AFX_CTL3D_STATE : public CNoTrackObject
{
public:
	virtual ~_AFX_CTL3D_STATE();

	// setup during initialization
	BOOL m_bCtl3dInited;
	HINSTANCE m_hCtl3dLib;

	// CTL3D32 entry points
	BOOL (WINAPI* m_pfnRegister)(HINSTANCE);
	BOOL (WINAPI* m_pfnUnregister)(HINSTANCE);
	BOOL (WINAPI* m_pfnAutoSubclass)(HINSTANCE);
	BOOL (WINAPI* m_pfnUnAutoSubclass)();
	BOOL (WINAPI* m_pfnColorChange)();
	BOOL (WINAPI* m_pfnSubclassDlgEx)(HWND, DWORD);
	void (WINAPI* m_pfnWinIniChange)();
	BOOL (WINAPI* m_pfnSubclassCtl)(HWND);
	BOOL (WINAPI* m_pfnSubclassCtlEx)(HWND, int);
};

EXTERN_PROCESS_LOCAL(_AFX_CTL3D_STATE, _afxCtl3dState)

class _AFX_CTL3D_THREAD : public CNoTrackObject
{
public:
	virtual ~_AFX_CTL3D_THREAD();
};

EXTERN_THREAD_LOCAL(_AFX_CTL3D_THREAD, _afxCtl3dThread)

_AFX_CTL3D_STATE* AFXAPI AfxGetCtl3dState();

#endif //!_AFX_NO_CTL3D_SUPPORT

/////////////////////////////////////////////////////////////////////////////
// _AFX_EDIT_STATE

class _AFX_EDIT_STATE : public CNoTrackObject
{
public:
	_AFX_EDIT_STATE();
	virtual ~_AFX_EDIT_STATE();

	CFindReplaceDialog* pFindReplaceDlg; // find or replace dialog
	BOOL bFindOnly; // Is pFindReplace the find or replace?
	CString strFind;    // last find string
	CString strReplace; // last replace string
	BOOL bCase; // TRUE==case sensitive, FALSE==not
	int bNext;  // TRUE==search down, FALSE== search up
	BOOL bWord; // TRUE==match whole word, FALSE==not
};

#undef AFX_DATA
#define AFX_DATA AFX_CORE_DATA

class _AFX_RICHEDIT_STATE : public _AFX_EDIT_STATE
{
public:
	HINSTANCE m_hInstRichEdit;      // handle to RICHED32.DLL
	virtual ~_AFX_RICHEDIT_STATE();
};

EXTERN_PROCESS_LOCAL(_AFX_RICHEDIT_STATE, _afxRichEditState)

_AFX_RICHEDIT_STATE* AFX_CDECL AfxGetRichEditState();

#undef AFX_DATA
#define AFX_DATA

////////////////////////////////////////////////////////////////////////////
// other global state

class CPushRoutingFrame
{
protected:
	CFrameWnd* pOldRoutingFrame;
	_AFX_THREAD_STATE* pThreadState;

public:
	CPushRoutingFrame(CFrameWnd* pNewRoutingFrame)
	{
		pThreadState = AfxGetThreadState();
		pOldRoutingFrame = pThreadState->m_pRoutingFrame;
		pThreadState->m_pRoutingFrame = pNewRoutingFrame;
	}
	~CPushRoutingFrame()
	{ pThreadState->m_pRoutingFrame = pOldRoutingFrame; }
};

class CPushRoutingView
{
protected:
	CView* pOldRoutingView;
	_AFX_THREAD_STATE* pThreadState;

public:
	CPushRoutingView(CView* pNewRoutingView)
	{
		pThreadState = AfxGetThreadState();
		pOldRoutingView = pThreadState->m_pRoutingView;
		pThreadState->m_pRoutingView = pNewRoutingView;
	}
	~CPushRoutingView()
	{ pThreadState->m_pRoutingView = pOldRoutingView; }
};

// Note: afxData.cxBorder and afxData.cyBorder aren't used anymore
#define CX_BORDER   1
#define CY_BORDER   1

// states for Shift+F1 hep mode
#define HELP_INACTIVE   0   // not in Shift+F1 help mode (must be 0)
#define HELP_ACTIVE     1   // in Shift+F1 help mode (non-zero)
#define HELP_ENTERING   2   // entering Shift+F1 help mode (non-zero)

/////////////////////////////////////////////////////////////////////////////
// Window class names and other window creation support

// from wincore.cpp
extern const TCHAR _afxWnd[];           // simple child windows/controls
extern const TCHAR _afxWndControlBar[]; // controls with gray backgrounds
extern const TCHAR _afxWndMDIFrame[];
extern const TCHAR _afxWndFrameOrView[];
extern const TCHAR _afxWndOleControl[];

#define AFX_WND_REG                     0x00001
#define AFX_WNDCONTROLBAR_REG           0x00002
#define AFX_WNDMDIFRAME_REG             0x00004
#define AFX_WNDFRAMEORVIEW_REG          0x00008
#define AFX_WNDCOMMCTLS_REG             0x00010 // means all original Win95
#define AFX_WNDOLECONTROL_REG           0x00020
#define AFX_WNDCOMMCTL_UPDOWN_REG       0x00040 // these are original Win95
#define AFX_WNDCOMMCTL_TREEVIEW_REG     0x00080
#define AFX_WNDCOMMCTL_TAB_REG          0x00100
#define AFX_WNDCOMMCTL_PROGRESS_REG     0x00200
#define AFX_WNDCOMMCTL_LISTVIEW_REG     0x00400
#define AFX_WNDCOMMCTL_HOTKEY_REG       0x00800
#define AFX_WNDCOMMCTL_BAR_REG          0x01000
#define AFX_WNDCOMMCTL_ANIMATE_REG      0x02000
#define AFX_WNDCOMMCTL_INTERNET_REG     0x04000 // these are new in IE4
#define AFX_WNDCOMMCTL_COOL_REG         0x08000
#define AFX_WNDCOMMCTL_USEREX_REG       0x10000
#define AFX_WNDCOMMCTL_DATE_REG         0x20000

#define AFX_WIN95CTLS_MASK              0x03FC0 // UPDOWN -> ANIMATE
#define AFX_WNDCOMMCTLSALL_REG          0x3C010 // COMMCTLS|INTERNET|COOL|USEREX|DATE
#define AFX_WNDCOMMCTLSNEW_REG          0x3C000 // INTERNET|COOL|USEREX|DATE

#define AfxDeferRegisterClass(fClass) AfxEndDeferRegisterClass(fClass)

BOOL AFXAPI AfxEndDeferRegisterClass(LONG fToRegister);

// MFC has its own version of the TOOLINFO structure containing the
// the Win95 base version of the structure. Since MFC targets Win95 base,
// we need this structure so calls into that old library don't fail.

typedef struct tagAFX_OLDTOOLINFO {
	UINT cbSize;
	UINT uFlags;
	HWND hwnd;
	UINT uId;
	RECT rect;
	HINSTANCE hinst;
	LPTSTR lpszText;
} AFX_OLDTOOLINFO;

// special AFX window class name mangling

#ifndef _UNICODE
#define _UNICODE_SUFFIX
#else
#define _UNICODE_SUFFIX _T("u")
#endif

#ifndef _DEBUG
#define _DEBUG_SUFFIX
#else
#define _DEBUG_SUFFIX _T("d")
#endif

#ifdef _AFXDLL
#define _STATIC_SUFFIX
#else
#define _STATIC_SUFFIX _T("s")
#endif

#define AFX_WNDCLASS(s) \
	_T("Afx") _T(s) _T("42") _STATIC_SUFFIX _UNICODE_SUFFIX _DEBUG_SUFFIX

#define AFX_WND             AFX_WNDCLASS("Wnd")
#define AFX_WNDCONTROLBAR   AFX_WNDCLASS("ControlBar")
#define AFX_WNDMDIFRAME     AFX_WNDCLASS("MDIFrame")
#define AFX_WNDFRAMEORVIEW  AFX_WNDCLASS("FrameOrView")
#define AFX_WNDOLECONTROL   AFX_WNDCLASS("OleControl")

// dialog/commdlg hook procs
BOOL CALLBACK AfxDlgProc(HWND, UINT, WPARAM, LPARAM);
UINT CALLBACK _AfxCommDlgProc(HWND hWnd, UINT, WPARAM, LPARAM);

// support for standard dialogs
extern UINT _afxMsgSETRGB;
typedef UINT (CALLBACK* COMMDLGPROC)(HWND, UINT, UINT, LONG);

/////////////////////////////////////////////////////////////////////////////
// Extended dialog templates (new in Win95)

#pragma pack(push, 1)

typedef struct
{
	WORD dlgVer;
	WORD signature;
	DWORD helpID;
	DWORD exStyle;
	DWORD style;
	WORD cDlgItems;
	short x;
	short y;
	short cx;
	short cy;
} DLGTEMPLATEEX;

typedef struct
{
	DWORD helpID;
	DWORD exStyle;
	DWORD style;
	short x;
	short y;
	short cx;
	short cy;
	DWORD id;
} DLGITEMTEMPLATEEX;

#pragma pack(pop)

/////////////////////////////////////////////////////////////////////////////
// Special helpers

void AFXAPI AfxCancelModes(HWND hWndRcvr);
HWND AFXAPI AfxGetParentOwner(HWND hWnd);
BOOL AFXAPI AfxIsDescendant(HWND hWndParent, HWND hWndChild);
BOOL AFXAPI AfxHelpEnabled();  // determine if ID_HELP handler exists
void AFXAPI AfxDeleteObject(HGDIOBJ* pObject);
BOOL AFXAPI AfxCustomLogFont(UINT nIDS, LOGFONT* pLogFont);
BOOL AFXAPI AfxGetPropSheetFont(CString& strFace, WORD& wSize, BOOL bWizard);

BOOL AFXAPI _AfxIsComboBoxControl(HWND hWnd, UINT nStyle);
BOOL AFXAPI _AfxCheckCenterDialog(LPCTSTR lpszResource);
BOOL AFXAPI _AfxCompareClassName(HWND hWnd, LPCTSTR lpszClassName);
HWND AFXAPI _AfxChildWindowFromPoint(HWND, POINT);

// for determining version of COMCTL32.DLL
#define VERSION_WIN4    MAKELONG(0, 4)
#define VERSION_IE3     MAKELONG(70, 4)
#define VERSION_IE4     MAKELONG(71, 4)
#define VERSION_IE401   MAKELONG(72, 4)
extern int _afxComCtlVersion;
DWORD AFXAPI _AfxGetComCtlVersion();

#undef AFX_DATA
#define AFX_DATA AFX_CORE_DATA

// UNICODE/MBCS abstractions
#ifdef _MBCS
	extern AFX_DATA const BOOL _afxDBCS;
#else
	#define _afxDBCS FALSE
#endif

#undef AFX_DATA
#define AFX_DATA

// determine number of elements in an array (not bytes)
#define _countof(array) (sizeof(array)/sizeof(array[0]))

#ifndef _AFX_PORTABLE
int AFX_CDECL AfxCriticalNewHandler(size_t nSize);
#endif

void AFXAPI AfxGlobalFree(HGLOBAL hGlobal);

/////////////////////////////////////////////////////////////////////////////
// static exceptions

extern CNotSupportedException _simpleNotSupportedException;
extern CMemoryException _simpleMemoryException;
extern CUserException _simpleUserException;
extern CResourceException _simpleResourceException;

/////////////////////////////////////////////////////////////////////////////
// useful message ranges

#define WM_SYSKEYFIRST  WM_SYSKEYDOWN
#define WM_SYSKEYLAST   WM_SYSDEADCHAR

#define WM_NCMOUSEFIRST WM_NCMOUSEMOVE
#define WM_NCMOUSELAST  WM_NCMBUTTONDBLCLK


/////////////////////////////////////////////////////////////////////////////
// AFX_CRITICAL_SECTION

#undef AFX_DATA
#define AFX_DATA AFX_CORE_DATA

// these globals are protected by the same critical section
#define CRIT_DYNLINKLIST    0
#define CRIT_RUNTIMECLASSLIST   0
#define CRIT_OBJECTFACTORYLIST  0
#define CRIT_LOCKSHARED 0
// these globals are not protected by independent critical sections
#define CRIT_REGCLASSLIST   1
#define CRIT_WAITCURSOR     2
#define CRIT_DROPSOURCE     3
#define CRIT_DROPTARGET     4
#define CRIT_RECTTRACKER    5
#define CRIT_EDITVIEW       6
#define CRIT_WINMSGCACHE    7
#define CRIT_HALFTONEBRUSH  8
#define CRIT_SPLITTERWND    9
#define CRIT_MINIFRAMEWND   10
#define CRIT_CTLLOCKLIST    11
#define CRIT_DYNDLLLOAD     12
#define CRIT_TYPELIBCACHE   13
#define CRIT_STOCKMASK      14
#define CRIT_ODBC           15
#define CRIT_PROCESSLOCAL   16
#define CRIT_MAX    17  // Note: above plus one!

#ifdef _MT
void AFXAPI AfxLockGlobals(int nLockType);
void AFXAPI AfxUnlockGlobals(int nLockType);
BOOL AFXAPI AfxCriticalInit();
void AFXAPI AfxCriticalTerm();
#else
#define AfxLockGlobals(nLockType)
#define AfxUnlockGlobals(nLockType)
#define AfxCriticalInit() (TRUE)
#define AfxCriticalTerm()
#endif

/////////////////////////////////////////////////////////////////////////////
// Portability abstractions

#define _AfxSetDlgCtrlID(hWnd, nID)     SetWindowLong(hWnd, GWL_ID, nID)
#define _AfxGetDlgCtrlID(hWnd)          ((UINT)(WORD)::GetDlgCtrlID(hWnd))

// misc helpers
BOOL AFXAPI AfxFullPath(LPTSTR lpszPathOut, LPCTSTR lpszFileIn);
BOOL AFXAPI AfxComparePath(LPCTSTR lpszPath1, LPCTSTR lpszPath2);

UINT AFXAPI AfxGetFileTitle(LPCTSTR lpszPathName, LPTSTR lpszTitle, UINT nMax);
UINT AFXAPI AfxGetFileName(LPCTSTR lpszPathName, LPTSTR lpszTitle, UINT nMax);
void AFX_CDECL AfxTimeToFileTime(const CTime& time, LPFILETIME pFileTime);
void AFXAPI AfxGetRoot(LPCTSTR lpszPath, CString& strRoot);

#ifndef _AFX_NO_OLE_SUPPORT
class AFX_COM
{
public:
	HRESULT CreateInstance(REFCLSID rclsid, LPUNKNOWN pUnkOuter,
		REFIID riid, LPVOID* ppv);
	HRESULT GetClassObject(REFCLSID rclsid, REFIID riid, LPVOID* ppv);
};

CString AFXAPI AfxStringFromCLSID(REFCLSID rclsid);
BOOL AFXAPI AfxGetInProcServer(LPCTSTR lpszCLSID, CString& str);
BOOL AFXAPI AfxResolveShortcut(CWnd* pWnd, LPCTSTR pszShortcutFile,
	LPTSTR pszPath, int cchPath);
#endif // _AFX_NO_OLE_SUPPORT

#define NULL_TLS ((DWORD)-1)

/////////////////////////////////////////////////////////////////////////////
// Message map and message dispatch

const AFX_MSGMAP_ENTRY* AFXAPI
AfxFindMessageEntry(const AFX_MSGMAP_ENTRY* lpEntry,
	UINT nMsg, UINT nCode, UINT nID);

union MessageMapFunctions
{
	AFX_PMSG pfn;   // generic member function pointer

	// specific type safe variants for WM_COMMAND and WM_NOTIFY messages
	void (AFX_MSG_CALL CCmdTarget::*pfn_COMMAND)();
	BOOL (AFX_MSG_CALL CCmdTarget::*pfn_bCOMMAND)();
	void (AFX_MSG_CALL CCmdTarget::*pfn_COMMAND_RANGE)(UINT);
	BOOL (AFX_MSG_CALL CCmdTarget::*pfn_COMMAND_EX)(UINT);

	void (AFX_MSG_CALL CCmdTarget::*pfn_UPDATE_COMMAND_UI)(CCmdUI*);
	void (AFX_MSG_CALL CCmdTarget::*pfn_UPDATE_COMMAND_UI_RANGE)(CCmdUI*, UINT);
	void (AFX_MSG_CALL CCmdTarget::*pfn_OTHER)(void*);
	BOOL (AFX_MSG_CALL CCmdTarget::*pfn_OTHER_EX)(void*);

	void (AFX_MSG_CALL CCmdTarget::*pfn_NOTIFY)(NMHDR*, LRESULT*);
	BOOL (AFX_MSG_CALL CCmdTarget::*pfn_bNOTIFY)(NMHDR*, LRESULT*);
	void (AFX_MSG_CALL CCmdTarget::*pfn_NOTIFY_RANGE)(UINT, NMHDR*, LRESULT*);
	BOOL (AFX_MSG_CALL CCmdTarget::*pfn_NOTIFY_EX)(UINT, NMHDR*, LRESULT*);

	// type safe variant for thread messages

	void (AFX_MSG_CALL CWinThread::*pfn_THREAD)(WPARAM, LPARAM);

	// specific type safe variants for WM-style messages
	BOOL    (AFX_MSG_CALL CWnd::*pfn_bD)(CDC*);
	BOOL    (AFX_MSG_CALL CWnd::*pfn_bb)(BOOL);
	BOOL    (AFX_MSG_CALL CWnd::*pfn_bWww)(CWnd*, UINT, UINT);
	BOOL    (AFX_MSG_CALL CWnd::*pfn_bHELPINFO)(HELPINFO*);
	BOOL    (AFX_MSG_CALL CWnd::*pfn_bWCDS)(CWnd*, COPYDATASTRUCT*);
	HBRUSH  (AFX_MSG_CALL CWnd::*pfn_hDWw)(CDC*, CWnd*, UINT);
	HBRUSH  (AFX_MSG_CALL CWnd::*pfn_hDw)(CDC*, UINT);
	int     (AFX_MSG_CALL CWnd::*pfn_iwWw)(UINT, CWnd*, UINT);
	int     (AFX_MSG_CALL CWnd::*pfn_iww)(UINT, UINT);
	int     (AFX_MSG_CALL CWnd::*pfn_iWww)(CWnd*, UINT, UINT);
	int     (AFX_MSG_CALL CWnd::*pfn_is)(LPTSTR);
	LRESULT (AFX_MSG_CALL CWnd::*pfn_lwl)(WPARAM, LPARAM);
	LRESULT (AFX_MSG_CALL CWnd::*pfn_lwwM)(UINT, UINT, CMenu*);
	void    (AFX_MSG_CALL CWnd::*pfn_vv)(void);

	void    (AFX_MSG_CALL CWnd::*pfn_vw)(UINT);
	void    (AFX_MSG_CALL CWnd::*pfn_vww)(UINT, UINT);
	void    (AFX_MSG_CALL CWnd::*pfn_vvii)(int, int);
	void    (AFX_MSG_CALL CWnd::*pfn_vwww)(UINT, UINT, UINT);
	void    (AFX_MSG_CALL CWnd::*pfn_vwii)(UINT, int, int);
	void    (AFX_MSG_CALL CWnd::*pfn_vwl)(WPARAM, LPARAM);
	void    (AFX_MSG_CALL CWnd::*pfn_vbWW)(BOOL, CWnd*, CWnd*);
	void    (AFX_MSG_CALL CWnd::*pfn_vD)(CDC*);
	void    (AFX_MSG_CALL CWnd::*pfn_vM)(CMenu*);
	void    (AFX_MSG_CALL CWnd::*pfn_vMwb)(CMenu*, UINT, BOOL);

	void    (AFX_MSG_CALL CWnd::*pfn_vW)(CWnd*);
	void    (AFX_MSG_CALL CWnd::*pfn_vWww)(CWnd*, UINT, UINT);
	void    (AFX_MSG_CALL CWnd::*pfn_vWp)(CWnd*, CPoint);
	void    (AFX_MSG_CALL CWnd::*pfn_vWh)(CWnd*, HANDLE);
	void    (AFX_MSG_CALL CWnd::*pfn_vwW)(UINT, CWnd*);
	void    (AFX_MSG_CALL CWnd::*pfn_vwWb)(UINT, CWnd*, BOOL);
	void    (AFX_MSG_CALL CWnd::*pfn_vwwW)(UINT, UINT, CWnd*);
	void    (AFX_MSG_CALL CWnd::*pfn_vwwx)(UINT, UINT);
	void    (AFX_MSG_CALL CWnd::*pfn_vs)(LPTSTR);
	void    (AFX_MSG_CALL CWnd::*pfn_vOWNER)(int, LPTSTR);   // force return TRUE
	int     (AFX_MSG_CALL CWnd::*pfn_iis)(int, LPTSTR);
	UINT    (AFX_MSG_CALL CWnd::*pfn_wp)(CPoint);
	UINT    (AFX_MSG_CALL CWnd::*pfn_wv)(void);
	void    (AFX_MSG_CALL CWnd::*pfn_vPOS)(WINDOWPOS*);
	void    (AFX_MSG_CALL CWnd::*pfn_vCALC)(BOOL, NCCALCSIZE_PARAMS*);
	void    (AFX_MSG_CALL CWnd::*pfn_vwp)(UINT, CPoint);
	void    (AFX_MSG_CALL CWnd::*pfn_vwwh)(UINT, UINT, HANDLE);
	BOOL    (AFX_MSG_CALL CWnd::*pfn_bwsp)(UINT, short, CPoint);
	void    (AFX_MSG_CALL CWnd::*pfn_vws)(UINT, LPCTSTR);
};

CHandleMap* PASCAL afxMapHWND(BOOL bCreate = FALSE);
CHandleMap* PASCAL afxMapHIMAGELIST(BOOL bCreate = FALSE);
CHandleMap* PASCAL afxMapHDC(BOOL bCreate = FALSE);
CHandleMap* PASCAL afxMapHGDIOBJ(BOOL bCreate = FALSE);
CHandleMap* PASCAL afxMapHMENU(BOOL bCreate = FALSE);

/////////////////////////////////////////////////////////////////////////////
// Debugging/Tracing helpers

#ifdef _DEBUG
	void AFXAPI _AfxTraceMsg(LPCTSTR lpszPrefix, const MSG* pMsg);
	BOOL AFXAPI _AfxCheckDialogTemplate(LPCTSTR lpszResource,
		BOOL bInvisibleChild);
#endif

/////////////////////////////////////////////////////////////////////////////
// byte-swapping helpers

#ifdef _AFX_BYTESWAP

struct _AFXWORD
{
	BYTE WordBits[sizeof(WORD)];
};
struct _AFXDWORD
{
	BYTE DwordBits[sizeof(DWORD)];
};

struct _AFXFLOAT
{
	BYTE FloatBits[sizeof(float)];
};
struct _AFXDOUBLE
{
	BYTE DoubleBits[sizeof(double)];
};

inline void _AfxByteSwap(WORD w, BYTE* pb)
{
	_AFXWORD wAfx;
	*(WORD*)&wAfx = w;

	ASSERT(sizeof(WORD) == 2);

	*pb++ = wAfx.WordBits[1];
	*pb = wAfx.WordBits[0];
}

inline void _AfxByteSwap(DWORD dw, BYTE* pb)
{
	_AFXDWORD dwAfx;
	*(DWORD*)&dwAfx = dw;

	ASSERT(sizeof(DWORD) == 4);

	*pb++ = dwAfx.DwordBits[3];
	*pb++ = dwAfx.DwordBits[2];
	*pb++ = dwAfx.DwordBits[1];
	*pb = dwAfx.DwordBits[0];
}

inline void _AfxByteSwap(float f, BYTE* pb)
{
	_AFXFLOAT fAfx;
	*(float*)&fAfx = f;

	ASSERT(sizeof(float) == 4);

	*pb++ = fAfx.FloatBits[3];
	*pb++ = fAfx.FloatBits[2];
	*pb++ = fAfx.FloatBits[1];
	*pb = fAfx.FloatBits[0];
}

inline void _AfxByteSwap(double d, BYTE* pb)
{
	_AFXDOUBLE dAfx;
	*(double*)&dAfx = d;

	ASSERT(sizeof(double) == 8);

	*pb++ = dAfx.DoubleBits[7];
	*pb++ = dAfx.DoubleBits[6];
	*pb++ = dAfx.DoubleBits[5];
	*pb++ = dAfx.DoubleBits[4];
	*pb++ = dAfx.DoubleBits[3];
	*pb++ = dAfx.DoubleBits[2];
	*pb++ = dAfx.DoubleBits[1];
	*pb = dAfx.DoubleBits[0];
}
#endif //_AFX_BYTESWAP

#undef AFX_DATA
#define AFX_DATA

/////////////////////////////////////////////////////////////////////////////
