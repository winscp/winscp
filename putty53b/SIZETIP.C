#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <tchar.h>

#include "putty.h"
#include "winstuff.h"

static ATOM tip_class = 0;

static HFONT tip_font;
static COLORREF tip_bg;
static COLORREF tip_text;

static LRESULT CALLBACK SizeTipWndProc(HWND hWnd, UINT nMsg,
				       WPARAM wParam, LPARAM lParam)
{

    switch (nMsg) {
      case WM_ERASEBKGND:
	return TRUE;

      case WM_PAINT:
	{
	    HBRUSH hbr;
	    HGDIOBJ holdbr;
	    RECT cr;
	    int wtlen;
	    LPTSTR wt;
	    HDC hdc;

	    PAINTSTRUCT ps;
	    hdc = BeginPaint(hWnd, &ps);

	    SelectObject(hdc, tip_font);
	    SelectObject(hdc, GetStockObject(BLACK_PEN));

	    hbr = CreateSolidBrush(tip_bg);
	    holdbr = SelectObject(hdc, hbr);

	    GetClientRect(hWnd, &cr);
	    Rectangle(hdc, cr.left, cr.top, cr.right, cr.bottom);

	    wtlen = GetWindowTextLength(hWnd);
	    wt = (LPTSTR) smalloc((wtlen + 1) * sizeof(TCHAR));
	    GetWindowText(hWnd, wt, wtlen + 1);

	    SetTextColor(hdc, tip_text);
	    SetBkColor(hdc, tip_bg);

	    TextOut(hdc, cr.left + 3, cr.top + 3, wt, wtlen);

	    sfree(wt);

	    SelectObject(hdc, holdbr);
	    DeleteObject(hbr);

	    EndPaint(hWnd, &ps);
	}
	return 0;

      case WM_NCHITTEST:
	return HTTRANSPARENT;

      case WM_DESTROY:
	DeleteObject(tip_font);
	tip_font = NULL;
	break;

      case WM_SETTEXT:
	{
	    LPCTSTR str = (LPCTSTR) lParam;
	    SIZE sz;
	    HDC hdc = CreateCompatibleDC(NULL);

	    SelectObject(hdc, tip_font);
	    GetTextExtentPoint32(hdc, str, _tcslen(str), &sz);

	    SetWindowPos(hWnd, NULL, 0, 0, sz.cx + 6, sz.cy + 6,
			 SWP_NOZORDER | SWP_NOMOVE | SWP_NOACTIVATE);
	    InvalidateRect(hWnd, NULL, FALSE);

	    DeleteDC(hdc);
	}
	break;
    }

    return DefWindowProc(hWnd, nMsg, wParam, lParam);
}

static HWND tip_wnd = NULL;
static int tip_enabled = 0;

void UpdateSizeTip(HWND src, int cx, int cy)
{
    TCHAR str[32];

    if (!tip_enabled)
	return;

    if (!tip_wnd) {
	NONCLIENTMETRICS nci;

	/* First make sure the window class is registered */

	if (!tip_class) {
	    WNDCLASS wc;
	    wc.style = CS_HREDRAW | CS_VREDRAW;
	    wc.lpfnWndProc = SizeTipWndProc;
	    wc.cbClsExtra = 0;
	    wc.cbWndExtra = 0;
	    wc.hInstance = hinst;
	    wc.hIcon = NULL;
	    wc.hCursor = NULL;
	    wc.hbrBackground = NULL;
	    wc.lpszMenuName = NULL;
	    wc.lpszClassName = "SizeTipClass";

	    tip_class = RegisterClass(&wc);
	}
#if 0
	/* Default values based on Windows Standard color scheme */

	tip_font = GetStockObject(SYSTEM_FONT);
	tip_bg = RGB(255, 255, 225);
	tip_text = RGB(0, 0, 0);
#endif

	/* Prepare other GDI objects and drawing info */

	tip_bg = GetSysColor(COLOR_INFOBK);
	tip_text = GetSysColor(COLOR_INFOTEXT);

	memset(&nci, 0, sizeof(NONCLIENTMETRICS));
	nci.cbSize = sizeof(NONCLIENTMETRICS);
	SystemParametersInfo(SPI_GETNONCLIENTMETRICS,
			     sizeof(NONCLIENTMETRICS), &nci, 0);
	tip_font = CreateFontIndirect(&nci.lfStatusFont);
    }

    /* Generate the tip text */

    sprintf(str, "%dx%d", cx, cy);

    if (!tip_wnd) {
	HDC hdc;
	SIZE sz;
	RECT wr;
	int ix, iy;

	/* calculate the tip's size */

	hdc = CreateCompatibleDC(NULL);
	GetTextExtentPoint32(hdc, str, _tcslen(str), &sz);
	DeleteDC(hdc);

	GetWindowRect(src, &wr);

	ix = wr.left;
	if (ix < 16)
	    ix = 16;

	iy = wr.top - sz.cy;
	if (iy < 16)
	    iy = 16;

	/* Create the tip window */

	tip_wnd =
	    CreateWindowEx(WS_EX_TOOLWINDOW | WS_EX_TOPMOST,
			   MAKEINTRESOURCE(tip_class), str, WS_POPUP, ix,
			   iy, sz.cx, sz.cy, NULL, NULL, hinst, NULL);

	ShowWindow(tip_wnd, SW_SHOWNOACTIVATE);

    } else {

	/* Tip already exists, just set the text */

	SetWindowText(tip_wnd, str);
    }
}

void EnableSizeTip(int bEnable)
{
    if (tip_wnd && !bEnable) {
	DestroyWindow(tip_wnd);
	tip_wnd = NULL;
    }

    tip_enabled = bEnable;
}
