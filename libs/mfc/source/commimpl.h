// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1997 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.


/////////////////////////////////////////////////////////////////////////////
// AFX_COMCTL_CALL - used to dynamically load the COMCTL32 library

#ifdef _AFXDLL

#undef AFX_DATA
#define AFX_DATA AFX_CORE_DATA

struct AFX_COMMCTRL_CALL
{
// housekeeping and other
	void (STDAPICALLTYPE* pfnInitCommonControls)();
	int (STDAPICALLTYPE* pfnLBItemFromPt)(HWND hLB, POINT pt, BOOL bAutoScroll);

	HBITMAP (STDAPICALLTYPE* pfnCreateMappedBitmap)(HINSTANCE hInstance, int idBitmap,
								  UINT wFlags, LPCOLORMAP lpColorMap, int iNumMaps);
	BOOL (STDAPICALLTYPE* pfnMakeDragList)(HWND hLB);

// image lists
	BOOL (STDAPICALLTYPE* pfnImageList_SetOverlayImage)(HIMAGELIST himl, int iImage, int iOverlay);
	COLORREF (STDAPICALLTYPE* pfnImageList_GetBkColor)(HIMAGELIST himl);
	COLORREF (STDAPICALLTYPE* pfnImageList_SetBkColor)(HIMAGELIST himl, COLORREF clrBk);
	BOOL (STDAPICALLTYPE* pfnImageList_GetImageInfo)(HIMAGELIST himl, int i, IMAGEINFO FAR* pImageInfo);
	BOOL (STDAPICALLTYPE* pfnImageList_Draw)(HIMAGELIST himl, int i, HDC hdcDst, int x, int y, UINT fStyle);
	HICON (STDAPICALLTYPE* pfnImageList_GetIcon)(HIMAGELIST himl, int i, UINT flags);
	int (STDAPICALLTYPE* pfnImageList_ReplaceIcon)(HIMAGELIST himl, int i, HICON hicon);
	BOOL (STDAPICALLTYPE* pfnImageList_Replace)(HIMAGELIST himl, int i, HBITMAP hbmImage, HBITMAP hbmMask);
	BOOL (STDAPICALLTYPE* pfnImageList_Remove)(HIMAGELIST himl, int i);
	int (STDAPICALLTYPE* pfnImageList_AddMasked)(HIMAGELIST himl, HBITMAP hbmImage, COLORREF crMask);
	void (STDAPICALLTYPE* pfnImageList_EndDrag)();
	BOOL (STDAPICALLTYPE* pfnImageList_BeginDrag)(HIMAGELIST himlTrack, int iTrack, int dxHotspot, int dyHotspot);
	HIMAGELIST (STDAPICALLTYPE* pfnImageList_Merge)(HIMAGELIST himl1, int i1, HIMAGELIST himl2, int i2, int dx, int dy);
	HIMAGELIST (STDAPICALLTYPE* pfnImageList_Create)(int cx, int cy, UINT flags, int cInitial, int cGrow);
	BOOL (STDAPICALLTYPE* pfnImageList_Destroy)(HIMAGELIST himl);
	BOOL (STDAPICALLTYPE* pfnImageList_DragMove)(int x, int y);
	BOOL (STDAPICALLTYPE* pfnImageList_SetDragCursorImage)(HIMAGELIST himlDrag, int iDrag, int dxHotspot, int dyHotspot);
	BOOL (STDAPICALLTYPE* pfnImageList_DragShowNolock)(BOOL fShow);
	HIMAGELIST (STDAPICALLTYPE* pfnImageList_GetDragImage)(POINT FAR* ppt,POINT FAR* pptHotspot);
	BOOL (STDAPICALLTYPE* pfnImageList_DragEnter)(HWND hwndLock, int x, int y);
	BOOL (STDAPICALLTYPE* pfnImageList_DragLeave)(HWND hwndLock);
	int (STDAPICALLTYPE* pfnImageList_GetImageCount)(HIMAGELIST himl);
	int (STDAPICALLTYPE* pfnImageList_Add)(HIMAGELIST himl, HBITMAP hbmImage, HBITMAP hbmMask);

	HIMAGELIST (STDAPICALLTYPE* pfnImageList_LoadImage)(HINSTANCE hi, LPCTSTR lpbmp, int cx, int cGrow, COLORREF crMask, UINT uType, UINT uFlags);

#ifndef _AFX_NO_OLE_SUPPORT
	BOOL (STDAPICALLTYPE* pfnImageList_Write)(HIMAGELIST himl, LPSTREAM pstm);
	HIMAGELIST (STDAPICALLTYPE* pfnImageList_Read)(LPSTREAM pstm);
#endif

// property sheets
	BOOL (STDAPICALLTYPE* pfnDestroyPropertySheetPage)(HPROPSHEETPAGE);

	int  (STDAPICALLTYPE* pfnPropertySheet)(LPCPROPSHEETHEADER);
	HPROPSHEETPAGE (STDAPICALLTYPE* pfnCreatePropertySheetPage)(LPCPROPSHEETPAGE);
};

extern AFX_DATA AFX_COMMCTRL_CALL _afxCommCtrl;

/////////////////////////////////////////////////////////////////////////////
// AFX_SHELL_CALL - used to dynamically load SHELL32.DLL

#ifndef _MAC
struct AFX_SHELL_CALL
{
	DWORD (WINAPI* pfnSHGetFileInfo)(LPCTSTR pszPath, DWORD dwFileAttributes, SHFILEINFO FAR *psfi, UINT cbFileInfo, UINT uFlags);
	HICON (WINAPI* pfnExtractIcon)(HINSTANCE hInst, LPCTSTR lpszExeFileName, UINT nIconIndex);
	UINT (WINAPI* pfnDragQueryFile)(HDROP,UINT,LPTSTR,UINT);
	VOID (WINAPI* pfnDragAcceptFiles)(HWND,BOOL);
	VOID (WINAPI* pfnDragFinish)(HDROP);
};

/////////////////////////////////////////////////////////////////////////////
// AFX_WINSPOOL_CALL - used to dynamically load WINSPOOL.DLL

struct AFX_WINSPOOL_CALL
{
	BOOL (APIENTRY* pfnOpenPrinter)(LPTSTR, LPHANDLE, LPPRINTER_DEFAULTS);
	BOOL (APIENTRY* pfnClosePrinter)(HANDLE hPrinter);
	LONG (APIENTRY* pfnDocumentProperties)(HWND hWnd, HANDLE hPrinter,
		LPTSTR pDeviceName, PDEVMODE pDevModeOutput, PDEVMODE pDevModeInput,
		DWORD fMode);
};

/////////////////////////////////////////////////////////////////////////////
// AFX_COMDLG_CALL - used to dynamically load COMDLG32.DLL

struct AFX_COMDLG_CALL
{
	BOOL (APIENTRY* pfnChooseColor)(LPCHOOSECOLOR);
	DWORD (APIENTRY* pfnCommDlgExtendedError)(VOID);
	HWND (APIENTRY* pfnReplaceText)(LPFINDREPLACE);
	BOOL (APIENTRY* pfnGetSaveFileName)(LPOPENFILENAME);
	short (APIENTRY* pfnGetFileTitle)(LPCTSTR, LPTSTR, WORD);
	BOOL (APIENTRY* pfnPrintDlg)(LPPRINTDLG);
	BOOL (APIENTRY* pfnChooseFont)(LPCHOOSEFONT);
	HWND (APIENTRY* pfnFindText)(LPFINDREPLACE);
	BOOL (APIENTRY* pfnPageSetupDlg)(LPPAGESETUPDLG);
	BOOL (APIENTRY* pfnGetOpenFileName)(LPOPENFILENAME);
};

/////////////////////////////////////////////////////////////////////////////
// AFX_ADVAPI_CALL - used to dynamically load ADVAPI.DLL

struct AFX_ADVAPI_CALL
{
	LONG (APIENTRY* pfnRegCreateKeyEx)(HKEY hKey, LPCTSTR lpSubKey,
		DWORD Reserved, LPTSTR lpClass, DWORD dwOptions, REGSAM samDesired,
		LPSECURITY_ATTRIBUTES lpSecurityAttributes, PHKEY phkResult,
		LPDWORD lpdwDisposition);
	LONG (APIENTRY* pfnRegEnumKey)(HKEY hKey, DWORD dwIndex, LPTSTR lpName,
		DWORD cbName);
	LONG (APIENTRY* pfnRegDeleteKey)(HKEY hKey, LPCTSTR lpSubKey);
	LONG (APIENTRY* pfnRegDeleteValue)(HKEY hKey, LPCTSTR lpValueName);
	LONG (APIENTRY* pfnRegOpenKeyEx)(HKEY hKey, LPCTSTR lpSubKey,
		DWORD ulOptions, REGSAM samDesired, PHKEY phkResult);
	LONG (APIENTRY* pfnRegCloseKey)(HKEY hKey);
	LONG (APIENTRY* pfnRegSetValue)(HKEY hKey, LPCTSTR lpSubKey, DWORD dwType,
		LPCTSTR lpData, DWORD cbData);
	LONG (APIENTRY* pfnRegCreateKey)(HKEY hKey, LPCTSTR lpSubKey,
		PHKEY phkResult);
	LONG (APIENTRY* pfnRegSetValueEx)(HKEY hKey, LPCTSTR lpValueName,
		DWORD Reserved, DWORD dwType, CONST BYTE* lpData, DWORD cbData);
	LONG (APIENTRY* pfnRegQueryValue)(HKEY hKey, LPCTSTR lpSubKey,
		LPTSTR lpValue, PLONG lpcbValue);
	LONG (APIENTRY* pfnRegOpenKey)(HKEY hKey, LPCTSTR lpSubKey,
		PHKEY phkResult);
	LONG (APIENTRY* pfnRegQueryValueEx)(HKEY hKey, LPCTSTR lpValueName,
		LPDWORD lpReserved, LPDWORD lpType, LPBYTE lpData, LPDWORD lpcbData);
#ifndef _MAC
	BOOL (APIENTRY* pfnSetFileSecurity)(LPCTSTR lpszFile, SECURITY_INFORMATION si,
	   PSECURITY_DESCRIPTOR psd);
	BOOL (APIENTRY* pfnGetFileSecurity)(LPCTSTR lpFileName,
		SECURITY_INFORMATION RequestedInformation, PSECURITY_DESCRIPTOR pSecurityDescriptor,
		DWORD nLength, LPDWORD lpnLengthNeeded);
#endif
};

extern AFX_DATA AFX_COMDLG_CALL _afxComDlg;
extern AFX_DATA AFX_SHELL_CALL _afxShell;
extern AFX_DATA AFX_WINSPOOL_CALL _afxWinSpool;
extern AFX_DATA AFX_ADVAPI_CALL _afxAdvApi;

#endif //!_MAC

/////////////////////////////////////////////////////////////////////////////
// macros for AFX_COMCTL_CALL access

#ifdef InitCommonControls
#undef InitCommonControls
#endif
#define InitCommonControls _afxCommCtrl.pfnInitCommonControls

#ifdef LBItemFromPt
#undef LBItemFromPt
#endif
#define LBItemFromPt _afxCommCtrl.pfnLBItemFromPt

#ifdef CreateMappedBitmap
#undef CreateMappedBitmap
#endif
#define CreateMappedBitmap _afxCommCtrl.pfnCreateMappedBitmap

#ifdef MakeDragList
#undef MakeDragList
#endif
#define MakeDragList _afxCommCtrl.pfnMakeDragList

#ifdef ImageList_SetOverlayImage
#undef ImageList_SetOverlayImage
#endif
#define ImageList_SetOverlayImage _afxCommCtrl.pfnImageList_SetOverlayImage

#ifdef ImageList_GetBkColor
#undef ImageList_GetBkColor
#endif
#define ImageList_GetBkColor    _afxCommCtrl.pfnImageList_GetBkColor

#ifdef ImageList_SetBkColor
#undef ImageList_SetBkColor
#endif
#define ImageList_SetBkColor    _afxCommCtrl.pfnImageList_SetBkColor

#ifdef ImageList_GetImageInfo
#undef ImageList_GetImageInfo
#endif
#define ImageList_GetImageInfo  _afxCommCtrl.pfnImageList_GetImageInfo

#ifdef ImageList_Draw
#undef ImageList_Draw
#endif
#define ImageList_Draw  _afxCommCtrl.pfnImageList_Draw

#ifdef ImageList_GetIcon
#undef ImageList_GetIcon
#endif
#define ImageList_GetIcon   _afxCommCtrl.pfnImageList_GetIcon

#ifdef ImageList_ReplaceIcon
#undef ImageList_ReplaceIcon
#endif
#define ImageList_ReplaceIcon   _afxCommCtrl.pfnImageList_ReplaceIcon

#ifdef ImageList_Replace
#undef ImageList_Replace
#endif
#define ImageList_Replace   _afxCommCtrl.pfnImageList_Replace

#ifdef ImageList_Remove
#undef ImageList_Remove
#endif
#define ImageList_Remove    _afxCommCtrl.pfnImageList_Remove

#ifdef ImageList_AddMasked
#undef ImageList_AddMasked
#endif
#define ImageList_AddMasked _afxCommCtrl.pfnImageList_AddMasked

#ifdef ImageList_EndDrag
#undef ImageList_EndDrag
#endif
#define ImageList_EndDrag   _afxCommCtrl.pfnImageList_EndDrag

#ifdef ImageList_BeginDrag
#undef ImageList_BeginDrag
#endif
#define ImageList_BeginDrag _afxCommCtrl.pfnImageList_BeginDrag

#ifdef ImageList_LoadImage
#undef ImageList_LoadImage
#endif
#define ImageList_LoadImage _afxCommCtrl.pfnImageList_LoadImage

#ifndef _AFX_NO_OLE_SUPPORT

#ifdef ImageList_Write
#undef ImageList_Write
#endif
#define ImageList_Write _afxCommCtrl.pfnImageList_Write

#ifdef ImageList_Read
#undef ImageList_Read
#endif
#define ImageList_Read  _afxCommCtrl.pfnImageList_Read

#endif  // !_AFX_NO_OLE_SUPPORT

#ifdef ImageList_Merge
#undef ImageList_Merge
#endif
#define ImageList_Merge _afxCommCtrl.pfnImageList_Merge

#ifdef ImageList_Create
#undef ImageList_Create
#endif
#define ImageList_Create    _afxCommCtrl.pfnImageList_Create

#ifdef ImageList_Destroy
#undef ImageList_Destroy
#endif
#define ImageList_Destroy   _afxCommCtrl.pfnImageList_Destroy

#ifdef ImageList_DragMove
#undef ImageList_DragMove
#endif
#define ImageList_DragMove  _afxCommCtrl.pfnImageList_DragMove

#ifdef ImageList_SetDragCursorImage
#undef ImageList_SetDragCursorImage
#endif
#define ImageList_SetDragCursorImage    _afxCommCtrl.pfnImageList_SetDragCursorImage

#ifdef ImageList_DragShowNolock
#undef ImageList_DragShowNolock
#endif
#define ImageList_DragShowNolock    _afxCommCtrl.pfnImageList_DragShowNolock

#ifdef ImageList_GetDragImage
#undef ImageList_GetDragImage
#endif
#define ImageList_GetDragImage  _afxCommCtrl.pfnImageList_GetDragImage

#ifdef ImageList_DragEnter
#undef ImageList_DragEnter
#endif
#define ImageList_DragEnter _afxCommCtrl.pfnImageList_DragEnter

#ifdef ImageList_DragLeave
#undef ImageList_DragLeave
#endif
#define ImageList_DragLeave _afxCommCtrl.pfnImageList_DragLeave

#ifdef ImageList_GetImageCount
#undef ImageList_GetImageCount
#endif
#define ImageList_GetImageCount _afxCommCtrl.pfnImageList_GetImageCount

#ifdef ImageList_Add
#undef ImageList_Add
#endif
#define ImageList_Add   _afxCommCtrl.pfnImageList_Add

#ifdef DestroyPropertySheetPage
#undef DestroyPropertySheetPage
#endif
#define DestroyPropertySheetPage _afxCommCtrl.pfnDestroyPropertySheetPage

#ifdef PropertySheet
#undef PropertySheet
#endif
#define PropertySheet _afxCommCtrl.pfnPropertySheet

#ifdef CreatePropertySheetPage
#undef CreatePropertySheetPage
#endif
#define CreatePropertySheetPage _afxCommCtrl.pfnCreatePropertySheetPage

#ifndef _MAC
/////////////////////////////////////////////////////////////////////////////
// macros for AFX_COMDLG_CALL access

#ifdef ChooseColor
#undef ChooseColor
#endif
#define ChooseColor _afxComDlg.pfnChooseColor

#ifdef CommDlgExtendedError
#undef CommDlgExtendedError
#endif
#define CommDlgExtendedError _afxComDlg.pfnCommDlgExtendedError

#ifdef ReplaceText
#undef ReplaceText
#endif
#define ReplaceText _afxComDlg.pfnReplaceText

#ifdef GetSaveFileName
#undef GetSaveFileName
#endif
#define GetSaveFileName _afxComDlg.pfnGetSaveFileName

//#define GetFileTitle _afxComDlg.pfnGetFileTitle

#ifdef PrintDlg
#undef PrintDlg
#endif
#define PrintDlg _afxComDlg.pfnPrintDlg

#ifdef ChooseFont
#undef ChooseFont
#endif
#define ChooseFont _afxComDlg.pfnChooseFont

//#define FindText _afxComDlg.pfnFindText
inline HWND APIENTRY FindText(LPFINDREPLACE lp)
{
	return _afxComDlg.pfnFindText(lp);
}

#ifdef PageSetupDlg
#undef PageSetupDlg
#endif
#define PageSetupDlg _afxComDlg.pfnPageSetupDlg

#ifdef GetOpenFileName
#undef GetOpenFileName
#endif
#define GetOpenFileName _afxComDlg.pfnGetOpenFileName


/////////////////////////////////////////////////////////////////////////////
// macros for AFX_SHELL_CALL access

#ifdef SHGetFileInfo
#undef SHGetFileInfo
#endif
#define SHGetFileInfo _afxShell.pfnSHGetFileInfo

//#define ExtractIcon _afxShell.pfnExtractIcon

#ifdef DragQueryFile
#undef DragQueryFile
#endif
#define DragQueryFile _afxShell.pfnDragQueryFile

//#define DragAcceptFiles _afxShell.pfnDragAcceptFiles

#ifdef DragFinish
#undef DragFinish
#endif
#define DragFinish _afxShell.pfnDragFinish

/////////////////////////////////////////////////////////////////////////////
// macros for AFX_WINSPOOL_CALL access

#ifdef DocumentProperties
#undef DocumentProperties
#endif
#define DocumentProperties _afxWinSpool.pfnDocumentProperties

#ifdef OpenPrinter
#undef OpenPrinter
#endif
#define OpenPrinter _afxWinSpool.pfnOpenPrinter

#ifdef ClosePrinter
#undef ClosePrinter
#endif
#define ClosePrinter _afxWinSpool.pfnClosePrinter

/////////////////////////////////////////////////////////////////////////////
// macros for AFX_ADVAPI_CALL access

#ifdef RegCreateKeyEx
#undef RegCreateKeyEx
#endif
#define RegCreateKeyEx _afxAdvApi.pfnRegCreateKeyEx

#ifdef RegEnumKey
#undef RegEnumKey
#endif
#define RegEnumKey _afxAdvApi.pfnRegEnumKey

#ifdef RegDeleteKey
#undef RegDeleteKey
#endif
#define RegDeleteKey _afxAdvApi.pfnRegDeleteKey

#ifdef RegDeleteValue
#undef RegDeleteValue
#endif
#define RegDeleteValue _afxAdvApi.pfnRegDeleteValue

#ifdef RegOpenKeyEx
#undef RegOpenKeyEx
#endif
#define RegOpenKeyEx _afxAdvApi.pfnRegOpenKeyEx

#ifdef RegCloseKey
#undef RegCloseKey
#endif
#define RegCloseKey _afxAdvApi.pfnRegCloseKey

#ifdef RegSetValue
#undef RegSetValue
#endif
#define RegSetValue _afxAdvApi.pfnRegSetValue

#ifdef RegCreateKey
#undef RegCreateKey
#endif
#define RegCreateKey _afxAdvApi.pfnRegCreateKey

#ifdef RegSetValueEx
#undef RegSetValueEx
#endif
#define RegSetValueEx _afxAdvApi.pfnRegSetValueEx

#ifdef RegQueryValue
#undef RegQueryValue
#endif
#define RegQueryValue _afxAdvApi.pfnRegQueryValue

#ifdef RegOpenKey
#undef RegOpenKey
#endif
#define RegOpenKey _afxAdvApi.pfnRegOpenKey

#ifdef RegQueryValueEx
#undef RegQueryValueEx
#endif
#define RegQueryValueEx _afxAdvApi.pfnRegQueryValueEx

#ifdef SetFileSecurity
#undef SetFileSecurity
#endif
#define SetFileSecurity _afxAdvApi.pfnSetFileSecurity

#ifdef GetFileSecurity
#undef GetFileSecurity
#endif
#define GetFileSecurity _afxAdvApi.pfnGetFileSecurity

#define AfxDllExtractIcon _afxShell.pfnExtractIcon
#define AfxDllDragAcceptFiles _afxShell.pfnDragAcceptFiles
#define AfxDllGetFileTitle _afxComDlg.pfnGetFileTitle

#else // !_MAC

#define AfxDllExtractIcon ::ExtractIcon
#define AfxDllDragAcceptFiles ::DragAcceptFiles
#define AfxDllGetFileTitle ::GetFileTitle

#endif // !_MAC

/////////////////////////////////////////////////////////////////////////////
// _AFX_EXTDLL_STATE

#undef AFX_DATA
#define AFX_DATA

class _AFX_EXTDLL_STATE : public CNoTrackObject
{
public:
	_AFX_EXTDLL_STATE::~_AFX_EXTDLL_STATE();

	// Note: only necessary to initialize non-zero data
#ifdef _AFXDLL
	HINSTANCE m_hInstCommCtrl;
#ifndef _MAC
	HINSTANCE m_hInstComDlg;
	HINSTANCE m_hInstShell;
	HINSTANCE m_hInstWinSpool;
	HINSTANCE m_hInstAdvApi;
	HINSTANCE m_hInstInternet;
#endif
#endif
};

EXTERN_PROCESS_LOCAL(_AFX_EXTDLL_STATE, _afxExtDllState)

///////////////////////////////////////////////////////////////////////////////

#else // _AFXDLL

#define AfxDllExtractIcon ::ExtractIcon
#define AfxDllDragAcceptFiles ::DragAcceptFiles
#define AfxDllGetFileTitle ::GetFileTitle

#endif // _AFXDLL
