//---------------------------------------------------------------------------
#ifndef WinApiH
#define WinApiH
//---------------------------------------------------------------------------
#include <shlobj.h>
//---------------------------------------------------------------------------
EXTERN_C const GUID DECLSPEC_SELECTANY IID_ITaskbarList3
        = { 0xEA1AFB91, 0x9E28, 0x4B86, { 0x90, 0xE9, 0x9E, 0x9F, 0x8A, 0x5E, 0xEF, 0xAF } };
//---------------------------------------------------------------------------
typedef /* [v1_enum] */
enum TBPFLAG
{
  TBPF_NOPROGRESS = 0,
  TBPF_INDETERMINATE = 0x1,
  TBPF_NORMAL = 0x2,
  TBPF_ERROR = 0x4,
  TBPF_PAUSED = 0x8
} TBPFLAG;
//---------------------------------------------------------------------------
// Do not need this ATM
typedef void * LPTHUMBBUTTON;
//---------------------------------------------------------------------------
MIDL_INTERFACE("ea1afb91-9e28-4b86-90e9-9e9f8a5eefaf")
ITaskbarList3 : public ITaskbarList2
{
public:
    virtual HRESULT STDMETHODCALLTYPE SetProgressValue(
        /* [in] */ HWND hwnd,
        /* [in] */ ULONGLONG ullCompleted,
        /* [in] */ ULONGLONG ullTotal) = 0;

    virtual HRESULT STDMETHODCALLTYPE SetProgressState(
        /* [in] */ HWND hwnd,
        /* [in] */ TBPFLAG tbpFlags) = 0;

    virtual HRESULT STDMETHODCALLTYPE RegisterTab(
        /* [in] */ HWND hwndTab,
        /* [in] */ HWND hwndMDI) = 0;

    virtual HRESULT STDMETHODCALLTYPE UnregisterTab(
        /* [in] */ HWND hwndTab) = 0;

    virtual HRESULT STDMETHODCALLTYPE SetTabOrder(
        /* [in] */ HWND hwndTab,
        /* [in] */ HWND hwndInsertBefore) = 0;

    virtual HRESULT STDMETHODCALLTYPE SetTabActive(
        /* [in] */ HWND hwndTab,
        /* [in] */ HWND hwndMDI,
        /* [in] */ DWORD dwReserved) = 0;

    virtual HRESULT STDMETHODCALLTYPE ThumbBarAddButtons(
        /* [in] */ HWND hwnd,
        /* [in] */ UINT cButtons,
        /* [size_is][in] */ LPTHUMBBUTTON pButton) = 0;

    virtual HRESULT STDMETHODCALLTYPE ThumbBarUpdateButtons(
        /* [in] */ HWND hwnd,
        /* [in] */ UINT cButtons,
        /* [size_is][in] */ LPTHUMBBUTTON pButton) = 0;

    virtual HRESULT STDMETHODCALLTYPE ThumbBarSetImageList(
        /* [in] */ HWND hwnd,
        /* [in] */ ::HIMAGELIST himl) = 0;

    virtual HRESULT STDMETHODCALLTYPE SetOverlayIcon(
        /* [in] */ HWND hwnd,
        /* [in] */ HICON hIcon,
        /* [string][unique][in] */ LPCWSTR pszDescription) = 0;

    virtual HRESULT STDMETHODCALLTYPE SetThumbnailTooltip(
        /* [in] */ HWND hwnd,
        /* [string][unique][in] */ LPCWSTR pszTip) = 0;

    virtual HRESULT STDMETHODCALLTYPE SetThumbnailClip(
        /* [in] */ HWND hwnd,
        /* [in] */ RECT *prcClip) = 0;

};
//---------------------------------------------------------------------------
typedef struct tagCHANGEFILTERSTRUCT {
    DWORD cbSize;
    DWORD ExtStatus;
} CHANGEFILTERSTRUCT, *PCHANGEFILTERSTRUCT;

/*
 * Message filter action values (action parameter to ChangeWindowMessageFilterEx)
 */
#define MSGFLT_RESET                            (0)
#define MSGFLT_ALLOW                            (1)
#define MSGFLT_DISALLOW                         (2)

typedef BOOL WINAPI (* ChangeWindowMessageFilterExProc)(
    HWND hwnd, UINT message, DWORD action, PCHANGEFILTERSTRUCT pChangeFilterStruct);
//---------------------------------------------------------------------------
#endif  // WinApiH
