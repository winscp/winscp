#include <windows.h>
#include <commctrl.h>
#include <commdlg.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>

#include "ssh.h"
#include "putty.h"
#include "winstuff.h"
#include "win_res.h"
#include "storage.h"

#ifdef MSVC4
#define TVINSERTSTRUCT  TV_INSERTSTRUCT
#define TVITEM          TV_ITEM
#define ICON_BIG        1
#endif

static char **events = NULL;
static int nevents = 0, negsize = 0;

static int readytogo;
static int sesslist_has_focus;
static int requested_help;

static struct prefslist cipherlist;

#define PRINTER_DISABLED_STRING "None (printing disabled)"

void force_normal(HWND hwnd)
{
    static int recurse = 0;

    WINDOWPLACEMENT wp;

    if (recurse)
	return;
    recurse = 1;

    wp.length = sizeof(wp);
    if (GetWindowPlacement(hwnd, &wp) && wp.showCmd == SW_SHOWMAXIMIZED) {
	wp.showCmd = SW_SHOWNORMAL;
	SetWindowPlacement(hwnd, &wp);
    }
    recurse = 0;
}

static void MyGetDlgItemInt(HWND hwnd, int id, int *result)
{
    BOOL ok;
    int n;
    n = GetDlgItemInt(hwnd, id, &ok, FALSE);
    if (ok)
	*result = n;
}

static void MyGetDlgItemFlt(HWND hwnd, int id, int *result, int scale)
{
    char text[80];
    BOOL ok;
    ok = GetDlgItemText(hwnd, id, text, sizeof(text) - 1);
    if (ok && text[0])
	*result = (int) (scale * atof(text));
}

static void MySetDlgItemFlt(HWND hwnd, int id, double value)
{
    char text[80];
    sprintf(text, "%g", value);
    SetDlgItemText(hwnd, id, text);
}

static int CALLBACK LogProc(HWND hwnd, UINT msg,
			    WPARAM wParam, LPARAM lParam)
{
    int i;

    switch (msg) {
      case WM_INITDIALOG:
	{
	    static int tabs[4] = { 78, 108 };
	    SendDlgItemMessage(hwnd, IDN_LIST, LB_SETTABSTOPS, 2,
			       (LPARAM) tabs);
	}
	for (i = 0; i < nevents; i++)
	    SendDlgItemMessage(hwnd, IDN_LIST, LB_ADDSTRING,
			       0, (LPARAM) events[i]);
	return 1;
      case WM_COMMAND:
	switch (LOWORD(wParam)) {
	  case IDOK:
	  case IDCANCEL:
	    logbox = NULL;
	    SetActiveWindow(GetParent(hwnd));
	    DestroyWindow(hwnd);
	    return 0;
	  case IDN_COPY:
	    if (HIWORD(wParam) == BN_CLICKED ||
		HIWORD(wParam) == BN_DOUBLECLICKED) {
		int selcount;
		int *selitems;
		selcount = SendDlgItemMessage(hwnd, IDN_LIST,
					      LB_GETSELCOUNT, 0, 0);
		if (selcount == 0) {   /* don't even try to copy zero items */
		    MessageBeep(0);
		    break;
		}

		selitems = smalloc(selcount * sizeof(int));
		if (selitems) {
		    int count = SendDlgItemMessage(hwnd, IDN_LIST,
						   LB_GETSELITEMS,
						   selcount,
						   (LPARAM) selitems);
		    int i;
		    int size;
		    char *clipdata;
		    static unsigned char sel_nl[] = SEL_NL;

		    if (count == 0) {  /* can't copy zero stuff */
			MessageBeep(0);
			break;
		    }

		    size = 0;
		    for (i = 0; i < count; i++)
			size +=
			    strlen(events[selitems[i]]) + sizeof(sel_nl);

		    clipdata = smalloc(size);
		    if (clipdata) {
			char *p = clipdata;
			for (i = 0; i < count; i++) {
			    char *q = events[selitems[i]];
			    int qlen = strlen(q);
			    memcpy(p, q, qlen);
			    p += qlen;
			    memcpy(p, sel_nl, sizeof(sel_nl));
			    p += sizeof(sel_nl);
			}
			write_aclip(clipdata, size, TRUE);
			sfree(clipdata);
		    }
		    sfree(selitems);

		    for (i = 0; i < nevents; i++)
			SendDlgItemMessage(hwnd, IDN_LIST, LB_SETSEL,
					   FALSE, i);
		}
	    }
	    return 0;
	}
	return 0;
      case WM_CLOSE:
	logbox = NULL;
	SetActiveWindow(GetParent(hwnd));
	DestroyWindow(hwnd);
	return 0;
    }
    return 0;
}

static int CALLBACK LicenceProc(HWND hwnd, UINT msg,
				WPARAM wParam, LPARAM lParam)
{
    switch (msg) {
      case WM_INITDIALOG:
	return 1;
      case WM_COMMAND:
	switch (LOWORD(wParam)) {
	  case IDOK:
	    EndDialog(hwnd, 1);
	    return 0;
	}
	return 0;
      case WM_CLOSE:
	EndDialog(hwnd, 1);
	return 0;
    }
    return 0;
}

static int CALLBACK AboutProc(HWND hwnd, UINT msg,
			      WPARAM wParam, LPARAM lParam)
{
    switch (msg) {
      case WM_INITDIALOG:
	SetDlgItemText(hwnd, IDA_VERSION, ver);
	return 1;
      case WM_COMMAND:
	switch (LOWORD(wParam)) {
	  case IDOK:
	  case IDCANCEL:
	    EndDialog(hwnd, TRUE);
	    return 0;
	  case IDA_LICENCE:
	    EnableWindow(hwnd, 0);
	    DialogBox(hinst, MAKEINTRESOURCE(IDD_LICENCEBOX),
		      NULL, LicenceProc);
	    EnableWindow(hwnd, 1);
	    SetActiveWindow(hwnd);
	    return 0;

	  case IDA_WEB:
	    /* Load web browser */
	    ShellExecute(hwnd, "open",
			 "http://www.chiark.greenend.org.uk/~sgtatham/putty/",
			 0, 0, SW_SHOWDEFAULT);
	    return 0;
	}
	return 0;
      case WM_CLOSE:
	EndDialog(hwnd, TRUE);
	return 0;
    }
    return 0;
}

/*
 * Null dialog procedure.
 */
static int CALLBACK NullDlgProc(HWND hwnd, UINT msg,
				WPARAM wParam, LPARAM lParam)
{
    return 0;
}

static char savedsession[2048];

enum { IDCX_ABOUT =
	IDC_ABOUT, IDCX_TVSTATIC, IDCX_TREEVIEW, controlstartvalue,

    sessionpanelstart,
    IDC_TITLE_SESSION,
    IDC_BOX_SESSION1,
    IDC_BOX_SESSION2,
    IDC_BOX_SESSION3,
    IDC_HOSTSTATIC,
    IDC_HOST,
    IDC_PORTSTATIC,
    IDC_PORT,
    IDC_PROTSTATIC,
    IDC_PROTRAW,
    IDC_PROTTELNET,
    IDC_PROTRLOGIN,
    IDC_PROTSSH,
    IDC_SESSSTATIC,
    IDC_SESSEDIT,
    IDC_SESSLIST,
    IDC_SESSLOAD,
    IDC_SESSSAVE,
    IDC_SESSDEL,
    IDC_CLOSEEXIT,
    IDC_COEALWAYS,
    IDC_COENEVER,
    IDC_COENORMAL,
    sessionpanelend,

    loggingpanelstart,
    IDC_TITLE_LOGGING,
    IDC_BOX_LOGGING1,
    IDC_LSTATSTATIC,
    IDC_LSTATOFF,
    IDC_LSTATASCII,
    IDC_LSTATRAW,
    IDC_LSTATPACKET,
    IDC_LGFSTATIC,
    IDC_LGFEDIT,
    IDC_LGFBUTTON,
    IDC_LGFEXPLAIN,
    IDC_LSTATXIST,
    IDC_LSTATXOVR,
    IDC_LSTATXAPN,
    IDC_LSTATXASK,
    loggingpanelend,

    keyboardpanelstart,
    IDC_TITLE_KEYBOARD,
    IDC_BOX_KEYBOARD1,
    IDC_BOX_KEYBOARD2,
    IDC_BOX_KEYBOARD3,
    IDC_DELSTATIC,
    IDC_DEL008,
    IDC_DEL127,
    IDC_HOMESTATIC,
    IDC_HOMETILDE,
    IDC_HOMERXVT,
    IDC_FUNCSTATIC,
    IDC_FUNCTILDE,
    IDC_FUNCLINUX,
    IDC_FUNCXTERM,
    IDC_FUNCVT400,
    IDC_FUNCVT100P,
    IDC_FUNCSCO,
    IDC_KPSTATIC,
    IDC_KPNORMAL,
    IDC_KPAPPLIC,
    IDC_KPNH,
    IDC_CURSTATIC,
    IDC_CURNORMAL,
    IDC_CURAPPLIC,
    IDC_COMPOSEKEY,
    IDC_CTRLALTKEYS,
    keyboardpanelend,

    terminalpanelstart,
    IDC_TITLE_TERMINAL,
    IDC_BOX_TERMINAL1,
    IDC_BOX_TERMINAL2,
    IDC_BOX_TERMINAL3,
    IDC_WRAPMODE,
    IDC_DECOM,
    IDC_LFHASCR,
    IDC_BCE,
    IDC_BLINKTEXT,
    IDC_ANSWERBACK,
    IDC_ANSWEREDIT,
    IDC_ECHOSTATIC,
    IDC_ECHOBACKEND,
    IDC_ECHOYES,
    IDC_ECHONO,
    IDC_EDITSTATIC,
    IDC_EDITBACKEND,
    IDC_EDITYES,
    IDC_EDITNO,
    IDC_PRINTERSTATIC,
    IDC_PRINTER,
    terminalpanelend,

    featurespanelstart,
    IDC_TITLE_FEATURES,
    IDC_BOX_FEATURES1,
    IDC_NOAPPLICK,
    IDC_NOAPPLICC,
    IDC_NOMOUSEREP,
    IDC_NORESIZE,
    IDC_NOALTSCREEN,
    IDC_NOWINTITLE,
    IDC_NODBACKSPACE,
    IDC_NOCHARSET,
    featurespanelend,

    bellpanelstart,
    IDC_TITLE_BELL,
    IDC_BOX_BELL1,
    IDC_BOX_BELL2,
    IDC_BELLSTATIC,
    IDC_BELL_DISABLED,
    IDC_BELL_DEFAULT,
    IDC_BELL_WAVEFILE,
    IDC_BELL_VISUAL,
    IDC_BELL_WAVESTATIC,
    IDC_BELL_WAVEEDIT,
    IDC_BELL_WAVEBROWSE,
    IDC_B_IND_STATIC,
    IDC_B_IND_DISABLED,
    IDC_B_IND_FLASH,
    IDC_B_IND_STEADY,
    IDC_BELLOVL,
    IDC_BELLOVLNSTATIC,
    IDC_BELLOVLN,
    IDC_BELLOVLTSTATIC,
    IDC_BELLOVLT,
    IDC_BELLOVLEXPLAIN,
    IDC_BELLOVLSSTATIC,
    IDC_BELLOVLS,
    bellpanelend,

    windowpanelstart,
    IDC_TITLE_WINDOW,
    IDC_BOX_WINDOW1,
    IDC_BOX_WINDOW2,
    IDC_BOX_WINDOW3,
    IDC_ROWSSTATIC,
    IDC_ROWSEDIT,
    IDC_COLSSTATIC,
    IDC_COLSEDIT,
    IDC_RESIZESTATIC,
    IDC_RESIZETERM,
    IDC_RESIZEFONT,
    IDC_RESIZENONE,
    IDC_RESIZEEITHER,
    IDC_SCROLLBAR,
    IDC_SCROLLBARFULLSCREEN,
    IDC_SAVESTATIC,
    IDC_SAVEEDIT,
    IDC_SCROLLKEY,
    IDC_SCROLLDISP,
    windowpanelend,

    behaviourpanelstart,
    IDC_TITLE_BEHAVIOUR,
    IDC_BOX_BEHAVIOUR1,
    IDC_CLOSEWARN,
    IDC_ALTF4,
    IDC_ALTSPACE,
    IDC_ALTONLY,
    IDC_ALWAYSONTOP,
    IDC_FULLSCREENONALTENTER,
    behaviourpanelend,

    appearancepanelstart,
    IDC_TITLE_APPEARANCE,
    IDC_BOX_APPEARANCE1,
    IDC_BOX_APPEARANCE2,
    IDC_BOX_APPEARANCE3,
    IDC_BOX_APPEARANCE4,
    IDC_BOX_APPEARANCE5,
    IDC_CURSORSTATIC,
    IDC_CURBLOCK,
    IDC_CURUNDER,
    IDC_CURVERT,
    IDC_BLINKCUR,
    IDC_FONTSTATIC,
    IDC_CHOOSEFONT,
    IDC_WINTITLE,
    IDC_WINEDIT,
    IDC_WINNAME,
    IDC_HIDEMOUSE,
    IDC_SUNKENEDGE,
    IDC_WINBSTATIC,
    IDC_WINBEDIT,
    appearancepanelend,

    connectionpanelstart,
    IDC_TITLE_CONNECTION,
    IDC_BOX_CONNECTION1,
    IDC_BOX_CONNECTION2,
    IDC_BOX_CONNECTION3,
    IDC_TTSTATIC,
    IDC_TTEDIT,
    IDC_LOGSTATIC,
    IDC_LOGEDIT,
    IDC_PINGSTATIC,
    IDC_PINGEDIT,
    IDC_NODELAY,
    connectionpanelend,

    proxypanelstart,
    IDC_TITLE_PROXY,
    IDC_BOX_PROXY1,
    IDC_PROXYTYPESTATIC,
    IDC_PROXYTYPENONE,
    IDC_PROXYTYPEHTTP,
    IDC_PROXYTYPESOCKS,
    IDC_PROXYTYPETELNET,
    IDC_PROXYHOSTSTATIC,
    IDC_PROXYHOSTEDIT,
    IDC_PROXYPORTSTATIC,
    IDC_PROXYPORTEDIT,
    IDC_PROXYEXCLUDESTATIC,
    IDC_PROXYEXCLUDEEDIT,
    IDC_PROXYUSERSTATIC,
    IDC_PROXYUSEREDIT,
    IDC_PROXYPASSSTATIC,
    IDC_PROXYPASSEDIT,
    IDC_BOX_PROXY2,
    IDC_PROXYTELNETCMDSTATIC,
    IDC_PROXYTELNETCMDEDIT,
    IDC_PROXYSOCKSVERSTATIC,
    IDC_PROXYSOCKSVER5,
    IDC_PROXYSOCKSVER4,
    proxypanelend,

    telnetpanelstart,
    IDC_TITLE_TELNET,
    IDC_BOX_TELNET1,
    IDC_BOX_TELNET2,
    IDC_TSSTATIC,
    IDC_TSEDIT,
    IDC_ENVSTATIC,
    IDC_VARSTATIC,
    IDC_VAREDIT,
    IDC_VALSTATIC,
    IDC_VALEDIT,
    IDC_ENVLIST,
    IDC_ENVADD,
    IDC_ENVREMOVE,
    IDC_EMSTATIC,
    IDC_EMBSD,
    IDC_EMRFC,
    IDC_ACTSTATIC,
    IDC_TPASSIVE,
    IDC_TACTIVE,
    IDC_TELNETKEY,
    IDC_TELNETRET,
    telnetpanelend,

    rloginpanelstart,
    IDC_TITLE_RLOGIN,
    IDC_BOX_RLOGIN1,
    IDC_BOX_RLOGIN2,
    IDC_R_TSSTATIC,
    IDC_R_TSEDIT,
    IDC_RLLUSERSTATIC,
    IDC_RLLUSEREDIT,
    rloginpanelend,

    sshpanelstart,
    IDC_TITLE_SSH,
    IDC_BOX_SSH1,
    IDC_BOX_SSH2,
    IDC_BOX_SSH3,
    IDC_NOPTY,
    IDC_BOX_SSHCIPHER,
    IDC_CIPHERSTATIC2,
    IDC_CIPHERLIST,
    IDC_CIPHERUP,
    IDC_CIPHERDN,
    IDC_SSH2DES,
    IDC_SSHPROTSTATIC,
    IDC_SSHPROT1ONLY,
    IDC_SSHPROT1,
    IDC_SSHPROT2,
    IDC_SSHPROT2ONLY,
    IDC_CMDSTATIC,
    IDC_CMDEDIT,
    IDC_COMPRESS,
    sshpanelend,

    sshauthpanelstart,
    IDC_TITLE_SSHAUTH,
    IDC_BOX_SSHAUTH1,
    IDC_BOX_SSHAUTH2,
    IDC_PKSTATIC,
    IDC_PKEDIT,
    IDC_PKBUTTON,
    IDC_AGENTFWD,
    IDC_CHANGEUSER,
    IDC_AUTHTIS,
    IDC_AUTHKI,
    sshauthpanelend,

    sshbugspanelstart,
    IDC_TITLE_SSHBUGS,
    IDC_BOX_SSHBUGS1,
    IDC_BUGS_IGNORE1,
    IDC_BUGD_IGNORE1,
    IDC_BUGS_PLAINPW1,
    IDC_BUGD_PLAINPW1,
    IDC_BUGS_RSA1,
    IDC_BUGD_RSA1,
    IDC_BUGS_HMAC2,
    IDC_BUGD_HMAC2,
    IDC_BUGS_DERIVEKEY2,
    IDC_BUGD_DERIVEKEY2,
    IDC_BUGS_RSAPAD2,
    IDC_BUGD_RSAPAD2,
    IDC_BUGS_DHGEX2,
    IDC_BUGD_DHGEX2,
    sshbugspanelend,

    selectionpanelstart,
    IDC_TITLE_SELECTION,
    IDC_BOX_SELECTION1,
    IDC_BOX_SELECTION2,
    IDC_BOX_SELECTION3,
    IDC_MBSTATIC,
    IDC_MBWINDOWS,
    IDC_MBXTERM,
    IDC_MOUSEOVERRIDE,
    IDC_SELTYPESTATIC,
    IDC_SELTYPELEX,
    IDC_SELTYPERECT,
    IDC_CCSTATIC,
    IDC_CCLIST,
    IDC_CCSET,
    IDC_CCSTATIC2,
    IDC_CCEDIT,
    IDC_RAWCNP,
    IDC_RTFPASTE,
    selectionpanelend,

    colourspanelstart,
    IDC_TITLE_COLOURS,
    IDC_BOX_COLOURS1,
    IDC_BOX_COLOURS2,
    IDC_BOLDCOLOUR,
    IDC_PALETTE,
    IDC_COLOURSTATIC,
    IDC_COLOURLIST,
    IDC_RSTATIC,
    IDC_GSTATIC,
    IDC_BSTATIC,
    IDC_RVALUE,
    IDC_GVALUE,
    IDC_BVALUE,
    IDC_CHANGE,
    colourspanelend,

    translationpanelstart,
    IDC_TITLE_TRANSLATION,
    IDC_BOX_TRANSLATION1,
    IDC_BOX_TRANSLATION2,
    IDC_BOX_TRANSLATION3,
    IDC_CODEPAGESTATIC,
    IDC_CODEPAGE,
    IDC_CAPSLOCKCYR,
    IDC_VTSTATIC,
    IDC_VTXWINDOWS,
    IDC_VTOEMANSI,
    IDC_VTOEMONLY,
    IDC_VTPOORMAN,
    IDC_VTUNICODE,
    translationpanelend,

    tunnelspanelstart,
    IDC_TITLE_TUNNELS,
    IDC_BOX_TUNNELS1,
    IDC_BOX_TUNNELS2,
    IDC_X11_FORWARD,
    IDC_X11_DISPSTATIC,
    IDC_X11_DISPLAY,
    IDC_LPORT_ALL,
    IDC_RPORT_ALL,
    IDC_PFWDSTATIC,
    IDC_PFWDSTATIC2,
    IDC_PFWDREMOVE,
    IDC_PFWDLIST,
    IDC_PFWDADD,
    IDC_SPORTSTATIC,
    IDC_SPORTEDIT,
    IDC_DPORTSTATIC,
    IDC_DPORTEDIT,
    IDC_PFWDLOCAL,
    IDC_PFWDREMOTE,

    tunnelspanelend,

    controlendvalue
};

static const char *const colours[] = {
    "Default Foreground", "Default Bold Foreground",
    "Default Background", "Default Bold Background",
    "Cursor Text", "Cursor Colour",
    "ANSI Black", "ANSI Black Bold",
    "ANSI Red", "ANSI Red Bold",
    "ANSI Green", "ANSI Green Bold",
    "ANSI Yellow", "ANSI Yellow Bold",
    "ANSI Blue", "ANSI Blue Bold",
    "ANSI Magenta", "ANSI Magenta Bold",
    "ANSI Cyan", "ANSI Cyan Bold",
    "ANSI White", "ANSI White Bold"
};
static const int permcolour[] = {
    TRUE, FALSE, TRUE, FALSE, TRUE, TRUE,
    TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE,
    TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE
};

static void fmtfont(char *buf)
{
    sprintf(buf, "Font: %s, ", cfg.font);
    if (cfg.fontisbold)
	strcat(buf, "bold, ");
    if (cfg.fontheight == 0)
	strcat(buf, "default height");
    else
	sprintf(buf + strlen(buf), "%d-point",
		(cfg.fontheight < 0 ? -cfg.fontheight : cfg.fontheight));
}

char *help_context_cmd(int id)
{
    switch (id) {
      case IDC_HOSTSTATIC:
      case IDC_HOST:
      case IDC_PORTSTATIC:
      case IDC_PORT:
      case IDC_PROTSTATIC:
      case IDC_PROTRAW:
      case IDC_PROTTELNET:
      case IDC_PROTRLOGIN:
      case IDC_PROTSSH:
        return "JI(`',`session.hostname')";
      case IDC_SESSSTATIC:
      case IDC_SESSEDIT:
      case IDC_SESSLIST:
      case IDC_SESSLOAD:
      case IDC_SESSSAVE:
      case IDC_SESSDEL:
        return "JI(`',`session.saved')";
      case IDC_CLOSEEXIT:
      case IDC_COEALWAYS:
      case IDC_COENEVER:
      case IDC_COENORMAL:
        return "JI(`',`session.coe')";
      case IDC_LSTATSTATIC:
      case IDC_LSTATOFF:
      case IDC_LSTATASCII:
      case IDC_LSTATRAW:
      case IDC_LSTATPACKET:
        return "JI(`',`logging.main')";
      case IDC_LGFSTATIC:
      case IDC_LGFEDIT:
      case IDC_LGFBUTTON:
      case IDC_LGFEXPLAIN:
        return "JI(`',`logging.filename')";
      case IDC_LSTATXIST:
      case IDC_LSTATXOVR:
      case IDC_LSTATXAPN:
      case IDC_LSTATXASK:
        return "JI(`',`logging.exists')";

      case IDC_DELSTATIC:
      case IDC_DEL008:
      case IDC_DEL127:
        return "JI(`',`keyboard.backspace')";
      case IDC_HOMESTATIC:
      case IDC_HOMETILDE:
      case IDC_HOMERXVT:
        return "JI(`',`keyboard.homeend')";
      case IDC_FUNCSTATIC:
      case IDC_FUNCTILDE:
      case IDC_FUNCLINUX:
      case IDC_FUNCXTERM:
      case IDC_FUNCVT400:
      case IDC_FUNCVT100P:
      case IDC_FUNCSCO:
        return "JI(`',`keyboard.funkeys')";
      case IDC_KPSTATIC:
      case IDC_KPNORMAL:
      case IDC_KPAPPLIC:
        return "JI(`',`keyboard.appkeypad')";
      case IDC_CURSTATIC:
      case IDC_CURNORMAL:
      case IDC_CURAPPLIC:
        return "JI(`',`keyboard.appcursor')";
      case IDC_KPNH:
        return "JI(`',`keyboard.nethack')";
      case IDC_COMPOSEKEY:
        return "JI(`',`keyboard.compose')";
      case IDC_CTRLALTKEYS:
        return "JI(`',`keyboard.ctrlalt')";

      case IDC_NOAPPLICK:
      case IDC_NOAPPLICC:
        return "JI(`',`features.application')";
      case IDC_NOMOUSEREP:
        return "JI(`',`features.mouse')";
      case IDC_NORESIZE:
        return "JI(`',`features.resize')";
      case IDC_NOALTSCREEN:
        return "JI(`',`features.altscreen')";
      case IDC_NOWINTITLE:
        return "JI(`',`features.retitle')";
      case IDC_NODBACKSPACE:
        return "JI(`',`features.dbackspace')";
      case IDC_NOCHARSET:
        return "JI(`',`features.charset')";

      case IDC_WRAPMODE:
        return "JI(`',`terminal.autowrap')";
      case IDC_DECOM:
        return "JI(`',`terminal.decom')";
      case IDC_LFHASCR:
        return "JI(`',`terminal.lfhascr')";
      case IDC_BCE:
        return "JI(`',`terminal.bce')";
      case IDC_BLINKTEXT:
        return "JI(`',`terminal.blink')";
      case IDC_ANSWERBACK:
      case IDC_ANSWEREDIT:
        return "JI(`',`terminal.answerback')";
      case IDC_ECHOSTATIC:
      case IDC_ECHOBACKEND:
      case IDC_ECHOYES:
      case IDC_ECHONO:
        return "JI(`',`terminal.localecho')";
      case IDC_EDITSTATIC:
      case IDC_EDITBACKEND:
      case IDC_EDITYES:
      case IDC_EDITNO:
        return "JI(`',`terminal.localedit')";
      case IDC_PRINTERSTATIC:
      case IDC_PRINTER:
	return "JI(`',`terminal.printing')";

      case IDC_BELLSTATIC:
      case IDC_BELL_DISABLED:
      case IDC_BELL_DEFAULT:
      case IDC_BELL_WAVEFILE:
      case IDC_BELL_VISUAL:
      case IDC_BELL_WAVESTATIC:
      case IDC_BELL_WAVEEDIT:
      case IDC_BELL_WAVEBROWSE:
        return "JI(`',`bell.style')";
      case IDC_B_IND_STATIC:
      case IDC_B_IND_DISABLED:
      case IDC_B_IND_FLASH:
      case IDC_B_IND_STEADY:
        return "JI(`',`bell.taskbar')";
      case IDC_BELLOVL:
      case IDC_BELLOVLNSTATIC:
      case IDC_BELLOVLN:
      case IDC_BELLOVLTSTATIC:
      case IDC_BELLOVLT:
      case IDC_BELLOVLEXPLAIN:
      case IDC_BELLOVLSSTATIC:
      case IDC_BELLOVLS:
        return "JI(`',`bell.overload')";

      case IDC_ROWSSTATIC:
      case IDC_ROWSEDIT:
      case IDC_COLSSTATIC:
      case IDC_COLSEDIT:
        return "JI(`',`window.size')";
      case IDC_RESIZESTATIC:
      case IDC_RESIZETERM:
      case IDC_RESIZEFONT:
      case IDC_RESIZENONE:
      case IDC_RESIZEEITHER:
        return "JI(`',`window.resize')";
      case IDC_SCROLLBAR:
      case IDC_SCROLLBARFULLSCREEN:
      case IDC_SAVESTATIC:
      case IDC_SAVEEDIT:
      case IDC_SCROLLKEY:
      case IDC_SCROLLDISP:
        return "JI(`',`window.scrollback')";

      case IDC_CLOSEWARN:
        return "JI(`',`behaviour.closewarn')";
      case IDC_ALTF4:
        return "JI(`',`behaviour.altf4')";
      case IDC_ALTSPACE:
        return "JI(`',`behaviour.altspace')";
      case IDC_ALTONLY:
        return "JI(`',`behaviour.altonly')";
      case IDC_ALWAYSONTOP:
        return "JI(`',`behaviour.alwaysontop')";
      case IDC_FULLSCREENONALTENTER:
        return "JI(`',`behaviour.altenter')";

      case IDC_CURSORSTATIC:
      case IDC_CURBLOCK:
      case IDC_CURUNDER:
      case IDC_CURVERT:
      case IDC_BLINKCUR:
        return "JI(`',`appearance.cursor')";
      case IDC_FONTSTATIC:
      case IDC_CHOOSEFONT:
        return "JI(`',`appearance.font')";
      case IDC_WINTITLE:
      case IDC_WINEDIT:
      case IDC_WINNAME:
        return "JI(`',`appearance.title')";
      case IDC_HIDEMOUSE:
        return "JI(`',`appearance.hidemouse')";
      case IDC_SUNKENEDGE:
      case IDC_WINBSTATIC:
      case IDC_WINBEDIT:
        return "JI(`',`appearance.border')";

      case IDC_TTSTATIC:
      case IDC_TTEDIT:
        return "JI(`',`connection.termtype')";
      case IDC_LOGSTATIC:
      case IDC_LOGEDIT:
        return "JI(`',`connection.username')";
      case IDC_PINGSTATIC:
      case IDC_PINGEDIT:
        return "JI(`',`connection.keepalive')";
      case IDC_NODELAY:
        return "JI(`',`connection.nodelay')";

      case IDC_PROXYTYPESTATIC:
      case IDC_PROXYTYPENONE:
      case IDC_PROXYTYPEHTTP:
      case IDC_PROXYTYPESOCKS:
      case IDC_PROXYTYPETELNET:
        return "JI(`',`proxy.type')";
      case IDC_PROXYHOSTSTATIC:
      case IDC_PROXYHOSTEDIT:
      case IDC_PROXYPORTSTATIC:
      case IDC_PROXYPORTEDIT:
        return "JI(`',`proxy.main')";
      case IDC_PROXYEXCLUDESTATIC:
      case IDC_PROXYEXCLUDEEDIT:
        return "JI(`',`proxy.exclude')";
      case IDC_PROXYUSERSTATIC:
      case IDC_PROXYUSEREDIT:
      case IDC_PROXYPASSSTATIC:
      case IDC_PROXYPASSEDIT:
        return "JI(`',`proxy.auth')";
      case IDC_PROXYTELNETCMDSTATIC:
      case IDC_PROXYTELNETCMDEDIT:
        return "JI(`',`proxy.command')";
      case IDC_PROXYSOCKSVERSTATIC:
      case IDC_PROXYSOCKSVER5:
      case IDC_PROXYSOCKSVER4:
        return "JI(`',`proxy.socksver')";

      case IDC_TSSTATIC:
      case IDC_TSEDIT:
        return "JI(`',`telnet.termspeed')";
      case IDC_ENVSTATIC:
      case IDC_VARSTATIC:
      case IDC_VAREDIT:
      case IDC_VALSTATIC:
      case IDC_VALEDIT:
      case IDC_ENVLIST:
      case IDC_ENVADD:
      case IDC_ENVREMOVE:
        return "JI(`',`telnet.environ')";
      case IDC_EMSTATIC:
      case IDC_EMBSD:
      case IDC_EMRFC:
        return "JI(`',`telnet.oldenviron')";
      case IDC_ACTSTATIC:
      case IDC_TPASSIVE:
      case IDC_TACTIVE:
        return "JI(`',`telnet.passive')";
      case IDC_TELNETKEY:
        return "JI(`',`telnet.specialkeys')";
      case IDC_TELNETRET:
        return "JI(`',`telnet.newline')";

      case IDC_R_TSSTATIC:
      case IDC_R_TSEDIT:
        return "JI(`',`rlogin.termspeed')";
      case IDC_RLLUSERSTATIC:
      case IDC_RLLUSEREDIT:
        return "JI(`',`rlogin.localuser')";

      case IDC_NOPTY:
        return "JI(`',`ssh.nopty')";
      case IDC_CIPHERSTATIC2:
      case IDC_CIPHERLIST:
      case IDC_CIPHERUP:
      case IDC_CIPHERDN:
      case IDC_SSH2DES:
        return "JI(`',`ssh.ciphers')";
      case IDC_SSHPROTSTATIC:
      case IDC_SSHPROT1ONLY:
      case IDC_SSHPROT1:
      case IDC_SSHPROT2:
      case IDC_SSHPROT2ONLY:
        return "JI(`',`ssh.protocol')";
      case IDC_CMDSTATIC:
      case IDC_CMDEDIT:
        return "JI(`',`ssh.command')";
      case IDC_COMPRESS:
        return "JI(`',`ssh.compress')";

      case IDC_PKSTATIC:
      case IDC_PKEDIT:
      case IDC_PKBUTTON:
        return "JI(`',`ssh.auth.privkey')";
      case IDC_AGENTFWD:
        return "JI(`',`ssh.auth.agentfwd')";
      case IDC_CHANGEUSER:
        return "JI(`',`ssh.auth.changeuser')";
      case IDC_AUTHTIS:
        return "JI(`',`ssh.auth.tis')";
      case IDC_AUTHKI:
        return "JI(`',`ssh.auth.ki')";

      case IDC_MBSTATIC:
      case IDC_MBWINDOWS:
      case IDC_MBXTERM:
        return "JI(`',`selection.buttons')";
      case IDC_MOUSEOVERRIDE:
        return "JI(`',`selection.shiftdrag')";
      case IDC_SELTYPESTATIC:
      case IDC_SELTYPELEX:
      case IDC_SELTYPERECT:
        return "JI(`',`selection.rect')";
      case IDC_CCSTATIC:
      case IDC_CCLIST:
      case IDC_CCSET:
      case IDC_CCSTATIC2:
      case IDC_CCEDIT:
        return "JI(`',`selection.charclasses')";
      case IDC_RAWCNP:
        return "JI(`',`selection.linedraw')";
      case IDC_RTFPASTE:
        return "JI(`',`selection.rtf')";

      case IDC_BOLDCOLOUR:
        return "JI(`',`colours.bold')";
      case IDC_PALETTE:
        return "JI(`',`colours.logpal')";
      case IDC_COLOURSTATIC:
      case IDC_COLOURLIST:
      case IDC_RSTATIC:
      case IDC_GSTATIC:
      case IDC_BSTATIC:
      case IDC_RVALUE:
      case IDC_GVALUE:
      case IDC_BVALUE:
      case IDC_CHANGE:
        return "JI(`',`colours.config')";

      case IDC_CODEPAGESTATIC:
      case IDC_CODEPAGE:
        return "JI(`',`translation.codepage')";
      case IDC_CAPSLOCKCYR:
        return "JI(`',`translation.cyrillic')";
      case IDC_VTSTATIC:
      case IDC_VTXWINDOWS:
      case IDC_VTOEMANSI:
      case IDC_VTOEMONLY:
      case IDC_VTPOORMAN:
      case IDC_VTUNICODE:
        return "JI(`',`translation.linedraw')";

      case IDC_X11_FORWARD:
      case IDC_X11_DISPSTATIC:
      case IDC_X11_DISPLAY:
        return "JI(`',`ssh.tunnels.x11')";
      case IDC_PFWDSTATIC:
      case IDC_PFWDSTATIC2:
      case IDC_PFWDREMOVE:
      case IDC_PFWDLIST:
      case IDC_PFWDADD:
      case IDC_SPORTSTATIC:
      case IDC_SPORTEDIT:
      case IDC_DPORTSTATIC:
      case IDC_DPORTEDIT:
      case IDC_PFWDLOCAL:
      case IDC_PFWDREMOTE:
        return "JI(`',`ssh.tunnels.portfwd')";
      case IDC_LPORT_ALL:
      case IDC_RPORT_ALL:
        return "JI(`',`ssh.tunnels.portfwd.localhost')";

      case IDC_BUGS_IGNORE1:
      case IDC_BUGD_IGNORE1:
	return "JI(`',`ssh.bugs.ignore1')";
      case IDC_BUGS_PLAINPW1:
      case IDC_BUGD_PLAINPW1:
	return "JI(`',`ssh.bugs.plainpw1')";
      case IDC_BUGS_RSA1:
      case IDC_BUGD_RSA1:
	return "JI(`',`ssh.bugs.rsa1')";
      case IDC_BUGS_HMAC2:
      case IDC_BUGD_HMAC2:
	return "JI(`',`ssh.bugs.hmac2')";
      case IDC_BUGS_DERIVEKEY2:
      case IDC_BUGD_DERIVEKEY2:
	return "JI(`',`ssh.bugs.derivekey2')";
      case IDC_BUGS_RSAPAD2:
      case IDC_BUGD_RSAPAD2:
	return "JI(`',`ssh.bugs.rsapad2')";
      case IDC_BUGS_DHGEX2:
      case IDC_BUGD_DHGEX2:
	return "JI(`',`ssh.bugs.dhgex2')";

      default:
        return NULL;
    }
}

/* 2nd arg: NZ => don't redraw session list (use when loading
 * a new session) */
static void init_dlg_ctrls(HWND hwnd, int keepsess)
{
    int i;
    char fontstatic[256];

    SetDlgItemText(hwnd, IDC_HOST, cfg.host);
    SetDlgItemText(hwnd, IDC_SESSEDIT, savedsession);
    if (!keepsess) {
	int i, n;
	n = SendDlgItemMessage(hwnd, IDC_SESSLIST, LB_GETCOUNT, 0, 0);
	for (i = n; i-- > 0;)
	    SendDlgItemMessage(hwnd, IDC_SESSLIST, LB_DELETESTRING, i, 0);
	for (i = 0; i < nsessions; i++)
	    SendDlgItemMessage(hwnd, IDC_SESSLIST, LB_ADDSTRING,
			       0, (LPARAM) (sessions[i]));
    }
    SetDlgItemInt(hwnd, IDC_PORT, cfg.port, FALSE);
    CheckRadioButton(hwnd, IDC_PROTRAW, IDC_PROTSSH,
		     cfg.protocol == PROT_SSH ? IDC_PROTSSH :
		     cfg.protocol == PROT_TELNET ? IDC_PROTTELNET :
		     cfg.protocol ==
		     PROT_RLOGIN ? IDC_PROTRLOGIN : IDC_PROTRAW);
    SetDlgItemInt(hwnd, IDC_PINGEDIT, cfg.ping_interval, FALSE);
    CheckDlgButton(hwnd, IDC_NODELAY, cfg.tcp_nodelay);

    CheckRadioButton(hwnd, IDC_DEL008, IDC_DEL127,
		     cfg.bksp_is_delete ? IDC_DEL127 : IDC_DEL008);
    CheckRadioButton(hwnd, IDC_HOMETILDE, IDC_HOMERXVT,
		     cfg.rxvt_homeend ? IDC_HOMERXVT : IDC_HOMETILDE);
    CheckRadioButton(hwnd, IDC_FUNCTILDE, IDC_FUNCSCO,
		     cfg.funky_type == 0 ? IDC_FUNCTILDE :
		     cfg.funky_type == 1 ? IDC_FUNCLINUX :
		     cfg.funky_type == 2 ? IDC_FUNCXTERM :
		     cfg.funky_type == 3 ? IDC_FUNCVT400 :
		     cfg.funky_type == 4 ? IDC_FUNCVT100P :
		     cfg.funky_type == 5 ? IDC_FUNCSCO : IDC_FUNCTILDE);
    CheckDlgButton(hwnd, IDC_NOAPPLICC, cfg.no_applic_c);
    CheckDlgButton(hwnd, IDC_NOAPPLICK, cfg.no_applic_k);
    CheckDlgButton(hwnd, IDC_NOMOUSEREP, cfg.no_mouse_rep);
    CheckDlgButton(hwnd, IDC_NORESIZE, cfg.no_remote_resize);
    CheckDlgButton(hwnd, IDC_NOALTSCREEN, cfg.no_alt_screen);
    CheckDlgButton(hwnd, IDC_NOWINTITLE, cfg.no_remote_wintitle);
    CheckDlgButton(hwnd, IDC_NODBACKSPACE, cfg.no_dbackspace);
    CheckDlgButton(hwnd, IDC_NOCHARSET, cfg.no_remote_charset);
    CheckRadioButton(hwnd, IDC_CURNORMAL, IDC_CURAPPLIC,
		     cfg.app_cursor ? IDC_CURAPPLIC : IDC_CURNORMAL);
    CheckRadioButton(hwnd, IDC_KPNORMAL, IDC_KPNH,
		     cfg.nethack_keypad ? IDC_KPNH :
		     cfg.app_keypad ? IDC_KPAPPLIC : IDC_KPNORMAL);
    CheckDlgButton(hwnd, IDC_ALTF4, cfg.alt_f4);
    CheckDlgButton(hwnd, IDC_ALTSPACE, cfg.alt_space);
    CheckDlgButton(hwnd, IDC_ALTONLY, cfg.alt_only);
    CheckDlgButton(hwnd, IDC_COMPOSEKEY, cfg.compose_key);
    CheckDlgButton(hwnd, IDC_CTRLALTKEYS, cfg.ctrlaltkeys);
    CheckDlgButton(hwnd, IDC_TELNETKEY, cfg.telnet_keyboard);
    CheckDlgButton(hwnd, IDC_TELNETRET, cfg.telnet_newline);
    CheckRadioButton(hwnd, IDC_ECHOBACKEND, IDC_ECHONO,
		     cfg.localecho == LD_BACKEND ? IDC_ECHOBACKEND :
		     cfg.localecho == LD_YES ? IDC_ECHOYES : IDC_ECHONO);
    CheckRadioButton(hwnd, IDC_EDITBACKEND, IDC_EDITNO,
		     cfg.localedit == LD_BACKEND ? IDC_EDITBACKEND :
		     cfg.localedit == LD_YES ? IDC_EDITYES : IDC_EDITNO);
    SetDlgItemText(hwnd, IDC_ANSWEREDIT, cfg.answerback);
    CheckDlgButton(hwnd, IDC_ALWAYSONTOP, cfg.alwaysontop);
    CheckDlgButton(hwnd, IDC_FULLSCREENONALTENTER, cfg.fullscreenonaltenter);
    CheckDlgButton(hwnd, IDC_SCROLLKEY, cfg.scroll_on_key);
    CheckDlgButton(hwnd, IDC_SCROLLDISP, cfg.scroll_on_disp);

    CheckDlgButton(hwnd, IDC_WRAPMODE, cfg.wrap_mode);
    CheckDlgButton(hwnd, IDC_DECOM, cfg.dec_om);
    CheckDlgButton(hwnd, IDC_LFHASCR, cfg.lfhascr);
    SetDlgItemInt(hwnd, IDC_ROWSEDIT, cfg.height, FALSE);
    SetDlgItemInt(hwnd, IDC_COLSEDIT, cfg.width, FALSE);
    SetDlgItemInt(hwnd, IDC_SAVEEDIT, cfg.savelines, FALSE);
    fmtfont(fontstatic);
    SetDlgItemText(hwnd, IDC_FONTSTATIC, fontstatic);
    CheckRadioButton(hwnd, IDC_BELL_DISABLED, IDC_BELL_VISUAL,
		     cfg.beep == BELL_DISABLED ? IDC_BELL_DISABLED :
		     cfg.beep == BELL_DEFAULT ? IDC_BELL_DEFAULT :
		     cfg.beep == BELL_WAVEFILE ? IDC_BELL_WAVEFILE :
		     cfg.beep ==
		     BELL_VISUAL ? IDC_BELL_VISUAL : IDC_BELL_DEFAULT);
    CheckRadioButton(hwnd, IDC_B_IND_DISABLED, IDC_B_IND_STEADY,
		     cfg.beep_ind ==
		     B_IND_DISABLED ? IDC_B_IND_DISABLED : cfg.beep_ind ==
		     B_IND_FLASH ? IDC_B_IND_FLASH : cfg.beep_ind ==
		     B_IND_STEADY ? IDC_B_IND_STEADY : IDC_B_IND_DISABLED);
    SetDlgItemText(hwnd, IDC_BELL_WAVEEDIT, cfg.bell_wavefile);
    CheckDlgButton(hwnd, IDC_BELLOVL, cfg.bellovl);
    SetDlgItemInt(hwnd, IDC_BELLOVLN, cfg.bellovl_n, FALSE);
    MySetDlgItemFlt(hwnd, IDC_BELLOVLT, cfg.bellovl_t / 1000.0);
    MySetDlgItemFlt(hwnd, IDC_BELLOVLS, cfg.bellovl_s / 1000.0);

    CheckDlgButton(hwnd, IDC_BCE, cfg.bce);
    CheckDlgButton(hwnd, IDC_BLINKTEXT, cfg.blinktext);

    SetDlgItemText(hwnd, IDC_WINEDIT, cfg.wintitle);
    CheckDlgButton(hwnd, IDC_WINNAME, cfg.win_name_always);
    CheckDlgButton(hwnd, IDC_HIDEMOUSE, cfg.hide_mouseptr);
    CheckDlgButton(hwnd, IDC_SUNKENEDGE, cfg.sunken_edge);
    SetDlgItemInt(hwnd, IDC_WINBEDIT, cfg.window_border, FALSE);
    CheckRadioButton(hwnd, IDC_CURBLOCK, IDC_CURVERT,
		     cfg.cursor_type == 0 ? IDC_CURBLOCK :
		     cfg.cursor_type == 1 ? IDC_CURUNDER : IDC_CURVERT);
    CheckDlgButton(hwnd, IDC_BLINKCUR, cfg.blink_cur);
    CheckDlgButton(hwnd, IDC_SCROLLBAR, cfg.scrollbar);
    CheckDlgButton(hwnd, IDC_SCROLLBARFULLSCREEN, cfg.scrollbar_in_fullscreen);
    CheckRadioButton(hwnd, IDC_RESIZETERM, IDC_RESIZEEITHER,
		     cfg.resize_action == RESIZE_TERM ? IDC_RESIZETERM :
		     cfg.resize_action == RESIZE_FONT ? IDC_RESIZEFONT :
		     cfg.resize_action == RESIZE_EITHER ? IDC_RESIZEEITHER :
		     IDC_RESIZENONE);
    CheckRadioButton(hwnd, IDC_COEALWAYS, IDC_COENORMAL,
		     cfg.close_on_exit == COE_NORMAL ? IDC_COENORMAL :
		     cfg.close_on_exit ==
		     COE_NEVER ? IDC_COENEVER : IDC_COEALWAYS);
    CheckDlgButton(hwnd, IDC_CLOSEWARN, cfg.warn_on_close);

    SetDlgItemText(hwnd, IDC_TTEDIT, cfg.termtype);
    SetDlgItemText(hwnd, IDC_TSEDIT, cfg.termspeed);
    SetDlgItemText(hwnd, IDC_R_TSEDIT, cfg.termspeed);
    SetDlgItemText(hwnd, IDC_RLLUSEREDIT, cfg.localusername);
    SetDlgItemText(hwnd, IDC_LOGEDIT, cfg.username);
    SetDlgItemText(hwnd, IDC_LGFEDIT, cfg.logfilename);
    CheckRadioButton(hwnd, IDC_LSTATOFF, IDC_LSTATPACKET,
		     cfg.logtype == LGTYP_NONE ? IDC_LSTATOFF :
		     cfg.logtype == LGTYP_ASCII ? IDC_LSTATASCII :
		     cfg.logtype == LGTYP_DEBUG ? IDC_LSTATRAW :
		     IDC_LSTATPACKET);
    CheckRadioButton(hwnd, IDC_LSTATXOVR, IDC_LSTATXASK,
		     cfg.logxfovr == LGXF_OVR ? IDC_LSTATXOVR :
		     cfg.logxfovr == LGXF_ASK ? IDC_LSTATXASK :
		     IDC_LSTATXAPN);
    {
	char *p = cfg.environmt;
	SendDlgItemMessage(hwnd, IDC_ENVLIST, LB_RESETCONTENT, 0, 0);
	while (*p) {
	    SendDlgItemMessage(hwnd, IDC_ENVLIST, LB_ADDSTRING, 0,
			       (LPARAM) p);
	    p += strlen(p) + 1;
	}
	p = cfg.portfwd;
	while (*p) {
	    SendDlgItemMessage(hwnd, IDC_PFWDLIST, LB_ADDSTRING, 0,
			       (LPARAM) p);
	    p += strlen(p) + 1;
	}
    }
    CheckRadioButton(hwnd, IDC_EMBSD, IDC_EMRFC,
		     cfg.rfc_environ ? IDC_EMRFC : IDC_EMBSD);
    CheckRadioButton(hwnd, IDC_TPASSIVE, IDC_TACTIVE,
		     cfg.passive_telnet ? IDC_TPASSIVE : IDC_TACTIVE);

    SetDlgItemText(hwnd, IDC_TTEDIT, cfg.termtype);
    SetDlgItemText(hwnd, IDC_LOGEDIT, cfg.username);
    CheckDlgButton(hwnd, IDC_NOPTY, cfg.nopty);
    CheckDlgButton(hwnd, IDC_COMPRESS, cfg.compression);
    CheckDlgButton(hwnd, IDC_SSH2DES, cfg.ssh2_des_cbc);
    CheckDlgButton(hwnd, IDC_AGENTFWD, cfg.agentfwd);
    CheckDlgButton(hwnd, IDC_CHANGEUSER, cfg.change_username);
    CheckRadioButton(hwnd, IDC_SSHPROT1ONLY, IDC_SSHPROT2ONLY,
		     cfg.sshprot == 1 ? IDC_SSHPROT1 :
		     cfg.sshprot == 2 ? IDC_SSHPROT2 :
		     cfg.sshprot == 3 ? IDC_SSHPROT2ONLY : IDC_SSHPROT1ONLY);
    CheckDlgButton(hwnd, IDC_AUTHTIS, cfg.try_tis_auth);
    CheckDlgButton(hwnd, IDC_AUTHKI, cfg.try_ki_auth);
    SetDlgItemText(hwnd, IDC_PKEDIT, cfg.keyfile);
    SetDlgItemText(hwnd, IDC_CMDEDIT, cfg.remote_cmd);

    {
	int i;
	static const struct { char *s; int c; } ciphers[] = {
	    { "3DES",			CIPHER_3DES },
	    { "Blowfish",		CIPHER_BLOWFISH },
	    { "DES",			CIPHER_DES },
	    { "AES (SSH 2 only)",	CIPHER_AES },
	    { "-- warn below here --",	CIPHER_WARN }
	};

	/* Set up the "selected ciphers" box. */
	/* (cipherlist assumed to contain all ciphers) */
	SendDlgItemMessage(hwnd, IDC_CIPHERLIST, LB_RESETCONTENT, 0, 0);
	for (i = 0; i < CIPHER_MAX; i++) {
	    int c = cfg.ssh_cipherlist[i];
	    int j, pos;
	    char *cstr = NULL;
	    for (j = 0; j < (sizeof ciphers) / (sizeof ciphers[0]); j++) {
		if (ciphers[j].c == c) {
		    cstr = ciphers[j].s;
		    break;
		}
	    }
	    pos = SendDlgItemMessage(hwnd, IDC_CIPHERLIST, LB_ADDSTRING,
				     0, (LPARAM) cstr);
	    SendDlgItemMessage(hwnd, IDC_CIPHERLIST, LB_SETITEMDATA,
			       pos, (LPARAM) c);
	}

    }

    CheckRadioButton(hwnd, IDC_MBWINDOWS, IDC_MBXTERM,
		     cfg.mouse_is_xterm ? IDC_MBXTERM : IDC_MBWINDOWS);
    CheckRadioButton(hwnd, IDC_SELTYPELEX, IDC_SELTYPERECT,
		     cfg.rect_select == 0 ? IDC_SELTYPELEX : IDC_SELTYPERECT);
    CheckDlgButton(hwnd, IDC_MOUSEOVERRIDE, cfg.mouse_override);
    CheckDlgButton(hwnd, IDC_RAWCNP, cfg.rawcnp);
    CheckDlgButton(hwnd, IDC_RTFPASTE, cfg.rtf_paste);
    {
	static int tabs[4] = { 25, 61, 96, 128 };
	SendDlgItemMessage(hwnd, IDC_CCLIST, LB_SETTABSTOPS, 4,
			   (LPARAM) tabs);
    }
    for (i = 0; i < 128; i++) {
	char str[100];
	sprintf(str, "%d\t(0x%02X)\t%c\t%d", i, i,
		(i >= 0x21 && i != 0x7F) ? i : ' ', cfg.wordness[i]);
	SendDlgItemMessage(hwnd, IDC_CCLIST, LB_ADDSTRING, 0,
			   (LPARAM) str);
    }

    CheckDlgButton(hwnd, IDC_BOLDCOLOUR, cfg.bold_colour);
    CheckDlgButton(hwnd, IDC_PALETTE, cfg.try_palette);
    {
	int i, n;
	n = SendDlgItemMessage(hwnd, IDC_COLOURLIST, LB_GETCOUNT, 0, 0);
	for (i = n; i-- > 0;)
	    SendDlgItemMessage(hwnd, IDC_COLOURLIST,
			       LB_DELETESTRING, i, 0);
	for (i = 0; i < 22; i++)
	    if (cfg.bold_colour || permcolour[i])
		SendDlgItemMessage(hwnd, IDC_COLOURLIST, LB_ADDSTRING, 0,
				   (LPARAM) colours[i]);
    }
    SendDlgItemMessage(hwnd, IDC_COLOURLIST, LB_SETCURSEL, 0, 0);
    SetDlgItemInt(hwnd, IDC_RVALUE, cfg.colours[0][0], FALSE);
    SetDlgItemInt(hwnd, IDC_GVALUE, cfg.colours[0][1], FALSE);
    SetDlgItemInt(hwnd, IDC_BVALUE, cfg.colours[0][2], FALSE);

    {
	int i;
	char *cp;
	strcpy(cfg.line_codepage, cp_name(decode_codepage(cfg.line_codepage)));
	SendDlgItemMessage(hwnd, IDC_CODEPAGE, CB_RESETCONTENT, 0, 0);
	CheckDlgButton (hwnd, IDC_CAPSLOCKCYR, cfg.xlat_capslockcyr);
	for (i = 0; (cp = cp_enumerate(i)) != NULL; i++) {
	    SendDlgItemMessage(hwnd, IDC_CODEPAGE, CB_ADDSTRING,
			       0, (LPARAM) cp);
	}
	SetDlgItemText(hwnd, IDC_CODEPAGE, cfg.line_codepage);
    }

    {
	int i, nprinters;
	printer_enum *pe;
	pe = printer_start_enum(&nprinters);
	SendDlgItemMessage(hwnd, IDC_PRINTER, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage(hwnd, IDC_PRINTER, CB_ADDSTRING,
			   0, (LPARAM) PRINTER_DISABLED_STRING);
	for (i = 0; i < nprinters; i++) {
	    char *printer_name = printer_get_name(pe, i);
	    SendDlgItemMessage(hwnd, IDC_PRINTER, CB_ADDSTRING,
			       0, (LPARAM) printer_name);
	}
	printer_finish_enum(pe);
	SetDlgItemText(hwnd, IDC_PRINTER,
		       *cfg.printer ? cfg.printer : PRINTER_DISABLED_STRING);
    }

    CheckRadioButton(hwnd, IDC_VTXWINDOWS, IDC_VTUNICODE,
		     cfg.vtmode == VT_XWINDOWS ? IDC_VTXWINDOWS :
		     cfg.vtmode == VT_OEMANSI ? IDC_VTOEMANSI :
		     cfg.vtmode == VT_OEMONLY ? IDC_VTOEMONLY :
		     cfg.vtmode == VT_UNICODE ? IDC_VTUNICODE :
		     IDC_VTPOORMAN);

    CheckDlgButton(hwnd, IDC_X11_FORWARD, cfg.x11_forward);
    SetDlgItemText(hwnd, IDC_X11_DISPLAY, cfg.x11_display);

    CheckDlgButton(hwnd, IDC_LPORT_ALL, cfg.lport_acceptall);
    CheckDlgButton(hwnd, IDC_RPORT_ALL, cfg.rport_acceptall);
    CheckRadioButton(hwnd, IDC_PFWDLOCAL, IDC_PFWDREMOTE, IDC_PFWDLOCAL);

    /* proxy config */
    CheckRadioButton(hwnd, IDC_PROXYTYPENONE, IDC_PROXYTYPETELNET,
		     cfg.proxy_type == PROXY_HTTP ? IDC_PROXYTYPEHTTP :
		     cfg.proxy_type == PROXY_SOCKS ? IDC_PROXYTYPESOCKS :
		     cfg.proxy_type == PROXY_TELNET ? IDC_PROXYTYPETELNET : IDC_PROXYTYPENONE);
    SetDlgItemText(hwnd, IDC_PROXYHOSTEDIT, cfg.proxy_host);
    SetDlgItemInt(hwnd, IDC_PROXYPORTEDIT, cfg.proxy_port, FALSE);
    SetDlgItemText(hwnd, IDC_PROXYEXCLUDEEDIT, cfg.proxy_exclude_list);
    SetDlgItemText(hwnd, IDC_PROXYTELNETCMDEDIT, cfg.proxy_telnet_command);
    SetDlgItemText(hwnd, IDC_PROXYUSEREDIT, cfg.proxy_username);
    SetDlgItemText(hwnd, IDC_PROXYPASSEDIT, cfg.proxy_password);
    CheckRadioButton(hwnd, IDC_PROXYSOCKSVER5, IDC_PROXYSOCKSVER4,
		     cfg.proxy_socks_version == 4 ? IDC_PROXYSOCKSVER4 : IDC_PROXYSOCKSVER5);

    /* SSH bugs config */
    SendDlgItemMessage(hwnd, IDC_BUGD_IGNORE1, CB_RESETCONTENT, 0, 0);
    SendDlgItemMessage(hwnd, IDC_BUGD_IGNORE1, CB_ADDSTRING, 0, (LPARAM)"Auto");
    SendDlgItemMessage(hwnd, IDC_BUGD_IGNORE1, CB_ADDSTRING, 0, (LPARAM)"Off");
    SendDlgItemMessage(hwnd, IDC_BUGD_IGNORE1, CB_ADDSTRING, 0, (LPARAM)"On");
    SendDlgItemMessage(hwnd, IDC_BUGD_IGNORE1, CB_SETCURSEL,
		       cfg.sshbug_ignore1 == BUG_ON ? 2 :
		       cfg.sshbug_ignore1 == BUG_OFF ? 1 : 0, 0);
    SendDlgItemMessage(hwnd, IDC_BUGD_PLAINPW1, CB_RESETCONTENT, 0, 0);
    SendDlgItemMessage(hwnd, IDC_BUGD_PLAINPW1, CB_ADDSTRING, 0, (LPARAM)"Auto");
    SendDlgItemMessage(hwnd, IDC_BUGD_PLAINPW1, CB_ADDSTRING, 0, (LPARAM)"Off");
    SendDlgItemMessage(hwnd, IDC_BUGD_PLAINPW1, CB_ADDSTRING, 0, (LPARAM)"On");
    SendDlgItemMessage(hwnd, IDC_BUGD_PLAINPW1, CB_SETCURSEL,
		       cfg.sshbug_plainpw1 == BUG_ON ? 2 :
		       cfg.sshbug_plainpw1 == BUG_OFF ? 1 : 0, 0);
    SendDlgItemMessage(hwnd, IDC_BUGD_RSA1, CB_RESETCONTENT, 0, 0);
    SendDlgItemMessage(hwnd, IDC_BUGD_RSA1, CB_ADDSTRING, 0, (LPARAM)"Auto");
    SendDlgItemMessage(hwnd, IDC_BUGD_RSA1, CB_ADDSTRING, 0, (LPARAM)"Off");
    SendDlgItemMessage(hwnd, IDC_BUGD_RSA1, CB_ADDSTRING, 0, (LPARAM)"On");
    SendDlgItemMessage(hwnd, IDC_BUGD_RSA1, CB_SETCURSEL,
		       cfg.sshbug_rsa1 == BUG_ON ? 2 :
		       cfg.sshbug_rsa1 == BUG_OFF ? 1 : 0, 0);
    SendDlgItemMessage(hwnd, IDC_BUGD_HMAC2, CB_RESETCONTENT, 0, 0);
    SendDlgItemMessage(hwnd, IDC_BUGD_HMAC2, CB_ADDSTRING, 0, (LPARAM)"Auto");
    SendDlgItemMessage(hwnd, IDC_BUGD_HMAC2, CB_ADDSTRING, 0, (LPARAM)"Off");
    SendDlgItemMessage(hwnd, IDC_BUGD_HMAC2, CB_ADDSTRING, 0, (LPARAM)"On");
    SendDlgItemMessage(hwnd, IDC_BUGD_HMAC2, CB_SETCURSEL,
		       cfg.sshbug_hmac2 == BUG_ON ? 2 :
		       cfg.sshbug_hmac2 == BUG_OFF ? 1 : 0, 0);
    SendDlgItemMessage(hwnd, IDC_BUGD_DERIVEKEY2, CB_RESETCONTENT, 0, 0);
    SendDlgItemMessage(hwnd, IDC_BUGD_DERIVEKEY2, CB_ADDSTRING, 0, (LPARAM)"Auto");
    SendDlgItemMessage(hwnd, IDC_BUGD_DERIVEKEY2, CB_ADDSTRING, 0, (LPARAM)"Off");
    SendDlgItemMessage(hwnd, IDC_BUGD_DERIVEKEY2, CB_ADDSTRING, 0, (LPARAM)"On");
    SendDlgItemMessage(hwnd, IDC_BUGD_DERIVEKEY2, CB_SETCURSEL,
		       cfg.sshbug_derivekey2 == BUG_ON ? 2 :
		       cfg.sshbug_derivekey2 == BUG_OFF ? 1 : 0, 0);
    SendDlgItemMessage(hwnd, IDC_BUGD_RSAPAD2, CB_RESETCONTENT, 0, 0);
    SendDlgItemMessage(hwnd, IDC_BUGD_RSAPAD2, CB_ADDSTRING, 0, (LPARAM)"Auto");
    SendDlgItemMessage(hwnd, IDC_BUGD_RSAPAD2, CB_ADDSTRING, 0, (LPARAM)"Off");
    SendDlgItemMessage(hwnd, IDC_BUGD_RSAPAD2, CB_ADDSTRING, 0, (LPARAM)"On");
    SendDlgItemMessage(hwnd, IDC_BUGD_RSAPAD2, CB_SETCURSEL,
		       cfg.sshbug_rsapad2 == BUG_ON ? 2 :
		       cfg.sshbug_rsapad2 == BUG_OFF ? 1 : 0, 0);
    SendDlgItemMessage(hwnd, IDC_BUGD_DHGEX2, CB_RESETCONTENT, 0, 0);
    SendDlgItemMessage(hwnd, IDC_BUGD_DHGEX2, CB_ADDSTRING, 0, (LPARAM)"Auto");
    SendDlgItemMessage(hwnd, IDC_BUGD_DHGEX2, CB_ADDSTRING, 0, (LPARAM)"Off");
    SendDlgItemMessage(hwnd, IDC_BUGD_DHGEX2, CB_ADDSTRING, 0, (LPARAM)"On");
    SendDlgItemMessage(hwnd, IDC_BUGD_DHGEX2, CB_SETCURSEL,
		       cfg.sshbug_dhgex2 == BUG_ON ? 2 :
		       cfg.sshbug_dhgex2 == BUG_OFF ? 1 : 0, 0);
}

struct treeview_faff {
    HWND treeview;
    HTREEITEM lastat[4];
};

static HTREEITEM treeview_insert(struct treeview_faff *faff,
				 int level, char *text)
{
    TVINSERTSTRUCT ins;
    int i;
    HTREEITEM newitem;
    ins.hParent = (level > 0 ? faff->lastat[level - 1] : TVI_ROOT);
    ins.hInsertAfter = faff->lastat[level];
#if _WIN32_IE >= 0x0400 && defined NONAMELESSUNION
#define INSITEM DUMMYUNIONNAME.item
#else
#define INSITEM item
#endif
    ins.INSITEM.mask = TVIF_TEXT;
    ins.INSITEM.pszText = text;
    newitem = TreeView_InsertItem(faff->treeview, &ins);
    if (level > 0)
	TreeView_Expand(faff->treeview, faff->lastat[level - 1],
			TVE_EXPAND);
    faff->lastat[level] = newitem;
    for (i = level + 1; i < 4; i++)
	faff->lastat[i] = NULL;
    return newitem;
}

/*
 * Create the panelfuls of controls in the configuration box.
 */
static void create_controls(HWND hwnd, int dlgtype, int panel)
{
    if (panel == sessionpanelstart) {
	/* The Session panel. Accelerators used: [acgoh] nprtis elvd w */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	bartitle(&cp, "Basic options for your PuTTY session",
		 IDC_TITLE_SESSION);
	if (dlgtype == 0) {
	    beginbox(&cp, "Specify your connection by host name or IP address",
		     IDC_BOX_SESSION1);
	    multiedit(&cp,
		      "Host &Name (or IP address)",
		      IDC_HOSTSTATIC, IDC_HOST, 75,
		      "&Port", IDC_PORTSTATIC, IDC_PORT, 25, NULL);
	    if (backends[3].backend == NULL) {
		/* this is PuTTYtel, so only three protocols available */
		radioline(&cp, "Protocol:", IDC_PROTSTATIC, 3,
			  "&Raw", IDC_PROTRAW,
			  "&Telnet", IDC_PROTTELNET,
			  "Rlog&in", IDC_PROTRLOGIN, NULL);
	    } else {
		radioline(&cp, "Protocol:", IDC_PROTSTATIC, 4,
			  "&Raw", IDC_PROTRAW,
			  "&Telnet", IDC_PROTTELNET,
			  "Rlog&in", IDC_PROTRLOGIN,
#ifdef FWHACK
			  "&SSH/hack",
#else
			  "&SSH",
#endif
			  IDC_PROTSSH, NULL);
	    }
	    endbox(&cp);
	    beginbox(&cp, "Load, save or delete a stored session",
		     IDC_BOX_SESSION2);
	    sesssaver(&cp, "Sav&ed Sessions",
		      IDC_SESSSTATIC, IDC_SESSEDIT, IDC_SESSLIST,
		      "&Load", IDC_SESSLOAD,
		      "Sa&ve", IDC_SESSSAVE, "&Delete", IDC_SESSDEL, NULL);
	    endbox(&cp);
	}
	beginbox(&cp, NULL, IDC_BOX_SESSION3);
	radioline(&cp, "Close &window on exit:", IDC_CLOSEEXIT, 4,
		  "Always", IDC_COEALWAYS,
		  "Never", IDC_COENEVER,
		  "Only on clean exit", IDC_COENORMAL, NULL);
	endbox(&cp);
    }

    if (panel == loggingpanelstart) {
	/* The Logging panel. Accelerators used: [acgoh] tplsfwe */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	bartitle(&cp, "Options controlling session logging",
		 IDC_TITLE_LOGGING);
	beginbox(&cp, NULL, IDC_BOX_LOGGING1);
	radiobig(&cp,
		 "Session logging:", IDC_LSTATSTATIC,
		 "Logging &turned off completely", IDC_LSTATOFF,
		 "Log &printable output only", IDC_LSTATASCII,
		 "&Log all session output", IDC_LSTATRAW,
		 "Log &SSH packet data", IDC_LSTATPACKET,
		 NULL);
	editbutton(&cp, "Log &file name:",
		   IDC_LGFSTATIC, IDC_LGFEDIT, "Bro&wse...",
		   IDC_LGFBUTTON);
	statictext(&cp, "(Log file name can contain &&Y, &&M, &&D for date,"
		   " &&T for time, and &&H for host name)", 2, IDC_LGFEXPLAIN);
	radiobig(&cp,
		 "What to do if the log file already &exists:",
		 IDC_LSTATXIST, "Always overwrite it", IDC_LSTATXOVR,
		 "Always append to the end of it", IDC_LSTATXAPN,
		 "Ask the user every time", IDC_LSTATXASK, NULL);
	endbox(&cp);
    }

    if (panel == terminalpanelstart) {
	/* The Terminal panel. Accelerators used: [acgoh] wdren lts p */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	bartitle(&cp, "Options controlling the terminal emulation",
		 IDC_TITLE_TERMINAL);
	beginbox(&cp, "Set various terminal options", IDC_BOX_TERMINAL1);
	checkbox(&cp, "Auto &wrap mode initially on", IDC_WRAPMODE);
	checkbox(&cp, "&DEC Origin Mode initially on", IDC_DECOM);
	checkbox(&cp, "Implicit C&R in every LF", IDC_LFHASCR);
	checkbox(&cp, "Use background colour to &erase screen", IDC_BCE);
	checkbox(&cp, "Enable bli&nking text", IDC_BLINKTEXT);
	multiedit(&cp,
		  "An&swerback to ^E:", IDC_ANSWERBACK,
		  IDC_ANSWEREDIT, 100, NULL);
	endbox(&cp);

	beginbox(&cp, "Line discipline options", IDC_BOX_TERMINAL2);
	radioline(&cp, "&Local echo:", IDC_ECHOSTATIC, 3,
		  "Auto", IDC_ECHOBACKEND,
		  "Force on", IDC_ECHOYES, "Force off", IDC_ECHONO, NULL);
	radioline(&cp, "Local line edi&ting:", IDC_EDITSTATIC, 3,
		  "Auto", IDC_EDITBACKEND,
		  "Force on", IDC_EDITYES, "Force off", IDC_EDITNO, NULL);
	endbox(&cp);

	beginbox(&cp, "Remote-controlled printing", IDC_BOX_TERMINAL3);
	combobox(&cp, "&Printer to send ANSI printer output to:",
		 IDC_PRINTERSTATIC, IDC_PRINTER);
	endbox(&cp);
    }

    if (panel == featurespanelstart) {
	/* The Features panel. Accelerators used: [acgoh] ukswtbrx */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	bartitle(&cp, "Enabling and disabling advanced terminal features ",
		 IDC_TITLE_FEATURES);
	beginbox(&cp, NULL, IDC_BOX_FEATURES1);
	checkbox(&cp, "Disable application c&ursor keys mode", IDC_NOAPPLICC);
	checkbox(&cp, "Disable application &keypad mode", IDC_NOAPPLICK);
	checkbox(&cp, "Disable &xterm-style mouse reporting", IDC_NOMOUSEREP);
	checkbox(&cp, "Disable remote-controlled terminal re&sizing",
		 IDC_NORESIZE);
	checkbox(&cp, "Disable s&witching to alternate terminal screen",
		 IDC_NOALTSCREEN);
	checkbox(&cp, "Disable remote-controlled window &title changing",
		 IDC_NOWINTITLE);
	checkbox(&cp, "Disable destructive &backspace on server sending ^?",
		 IDC_NODBACKSPACE);
	checkbox(&cp, "Disable remote-controlled cha&racter set configuration",
		 IDC_NOCHARSET);
	endbox(&cp);
    }

    if (panel == bellpanelstart) {
	/* The Bell panel. Accelerators used: [acgoh] bdsm wit */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	bartitle(&cp, "Options controlling the terminal bell",
		 IDC_TITLE_BELL);
	beginbox(&cp, "Set the style of bell", IDC_BOX_BELL1);
	radiobig(&cp,
		 "Action to happen when a &bell occurs:", IDC_BELLSTATIC,
		 "None (bell disabled)", IDC_BELL_DISABLED,
		 "Play Windows Default Sound", IDC_BELL_DEFAULT,
		 "Play a custom sound file", IDC_BELL_WAVEFILE,
		 "Visual bell (flash window)", IDC_BELL_VISUAL, NULL);
	editbutton(&cp, "Custom sound file to play as a bell:",
		   IDC_BELL_WAVESTATIC, IDC_BELL_WAVEEDIT,
		   "Bro&wse...", IDC_BELL_WAVEBROWSE);
	radioline(&cp, "Taskbar/caption &indication on bell:",
		  IDC_B_IND_STATIC, 3, "Disabled", IDC_B_IND_DISABLED,
		  "Flashing", IDC_B_IND_FLASH, "Steady", IDC_B_IND_STEADY,
		  NULL);
	endbox(&cp);
	beginbox(&cp, "Control the bell overload behaviour",
		 IDC_BOX_BELL2);
	checkbox(&cp, "Bell is temporarily &disabled when over-used",
		 IDC_BELLOVL);
	staticedit(&cp, "Over-use means this &many bells...",
		   IDC_BELLOVLNSTATIC, IDC_BELLOVLN, 20);
	staticedit(&cp, "... in &this many seconds",
		   IDC_BELLOVLTSTATIC, IDC_BELLOVLT, 20);
	statictext(&cp,
		   "The bell is re-enabled after a few seconds of silence.",
		   1, IDC_BELLOVLEXPLAIN);
	staticedit(&cp, "Seconds of &silence required", IDC_BELLOVLSSTATIC,
		   IDC_BELLOVLS, 20);
	endbox(&cp);
    }

    if (panel == keyboardpanelstart) {
	/* The Keyboard panel. Accelerators used: [acgoh] bef rntd */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	bartitle(&cp, "Options controlling the effects of keys",
		 IDC_TITLE_KEYBOARD);
	beginbox(&cp, "Change the sequences sent by:", IDC_BOX_KEYBOARD1);
	radioline(&cp, "The &Backspace key", IDC_DELSTATIC, 2,
		  "Control-H", IDC_DEL008,
		  "Control-? (127)", IDC_DEL127, NULL);
	radioline(&cp, "The Home and &End keys", IDC_HOMESTATIC, 2,
		  "Standard", IDC_HOMETILDE, "rxvt", IDC_HOMERXVT, NULL);
	radioline(&cp, "The &Function keys and keypad", IDC_FUNCSTATIC, 3,
		  "ESC[n~", IDC_FUNCTILDE,
		  "Linux", IDC_FUNCLINUX,
		  "Xterm R6", IDC_FUNCXTERM,
		  "VT400", IDC_FUNCVT400,
		  "VT100+", IDC_FUNCVT100P, "SCO", IDC_FUNCSCO, NULL);
	endbox(&cp);
	beginbox(&cp, "Application keypad settings:", IDC_BOX_KEYBOARD2);
	radioline(&cp, "Initial state of cu&rsor keys:", IDC_CURSTATIC, 2,
		  "Normal", IDC_CURNORMAL,
		  "Application", IDC_CURAPPLIC, NULL);
	radioline(&cp, "Initial state of &numeric keypad:", IDC_KPSTATIC,
		  3, "Normal", IDC_KPNORMAL, "Application", IDC_KPAPPLIC,
		  "NetHack", IDC_KPNH, NULL);
	endbox(&cp);
	beginbox(&cp, "Enable extra keyboard features:",
		 IDC_BOX_KEYBOARD3);
	checkbox(&cp, "AltGr ac&ts as Compose key", IDC_COMPOSEKEY);
	checkbox(&cp, "Control-Alt is &different from AltGr",
		 IDC_CTRLALTKEYS);
	endbox(&cp);
    }

    if (panel == windowpanelstart) {
	/* The Window panel. Accelerators used: [acgoh] rmz sdikp */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	bartitle(&cp, "Options controlling PuTTY's window",
		 IDC_TITLE_WINDOW);
	beginbox(&cp, "Set the size of the window", IDC_BOX_WINDOW1);
	multiedit(&cp,
		  "&Rows", IDC_ROWSSTATIC, IDC_ROWSEDIT, 50,
		  "Colu&mns", IDC_COLSSTATIC, IDC_COLSEDIT, 50, NULL);
	radiobig(&cp, "When window is resi&zed:", IDC_RESIZESTATIC,
		 "Change the number of rows and columns", IDC_RESIZETERM,
		 "Change the size of the font", IDC_RESIZEFONT,
		 "Change font size only when maximised", IDC_RESIZEEITHER,
		 "Forbid resizing completely", IDC_RESIZENONE, NULL);
	endbox(&cp);
	beginbox(&cp, "Control the scrollback in the window",
		 IDC_BOX_WINDOW2);
	staticedit(&cp, "Lines of &scrollback",
		   IDC_SAVESTATIC, IDC_SAVEEDIT, 50);
	checkbox(&cp, "&Display scrollbar", IDC_SCROLLBAR);
	checkbox(&cp, "D&isplay scrollbar in full screen mode", IDC_SCROLLBARFULLSCREEN);
	checkbox(&cp, "Reset scrollback on &keypress", IDC_SCROLLKEY);
	checkbox(&cp, "Reset scrollback on dis&play activity",
		 IDC_SCROLLDISP);
	endbox(&cp);
    }

    if (panel == appearancepanelstart) {
	/* The Appearance panel. Accelerators used: [acgoh] luvb n ti p s */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	bartitle(&cp, "Configure the appearance of PuTTY's window",
		 IDC_TITLE_APPEARANCE);
	beginbox(&cp, "Adjust the use of the cursor", IDC_BOX_APPEARANCE1);
	radioline(&cp, "Cursor appearance:", IDC_CURSORSTATIC, 3,
		  "B&lock", IDC_CURBLOCK,
		  "&Underline", IDC_CURUNDER,
		  "&Vertical line", IDC_CURVERT, NULL);
	checkbox(&cp, "Cursor &blinks", IDC_BLINKCUR);
	endbox(&cp);
	beginbox(&cp, "Set the font used in the terminal window",
		 IDC_BOX_APPEARANCE2);
	staticbtn(&cp, "", IDC_FONTSTATIC, "Cha&nge...", IDC_CHOOSEFONT);
	endbox(&cp);
	beginbox(&cp, "Adjust the use of the window title",
		 IDC_BOX_APPEARANCE3);
	multiedit(&cp,
		  "Window &title:", IDC_WINTITLE, IDC_WINEDIT, 100, NULL);
	checkbox(&cp, "Avoid ever using &icon title", IDC_WINNAME);
	endbox(&cp);
	beginbox(&cp, "Adjust the use of the mouse pointer",
		 IDC_BOX_APPEARANCE4);
	checkbox(&cp, "Hide mouse &pointer when typing in window",
		 IDC_HIDEMOUSE);
	endbox(&cp);
	beginbox(&cp, "Adjust the window border", IDC_BOX_APPEARANCE5);
	checkbox(&cp, "&Sunken-edge border (slightly thicker)",
		 IDC_SUNKENEDGE);
	staticedit(&cp, "Gap between text and window edge",
		   IDC_WINBSTATIC, IDC_WINBEDIT, 20);
	endbox(&cp);
    }

    if (panel == behaviourpanelstart) {
	/* The Behaviour panel. Accelerators used: [acgoh] w4yltf */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	bartitle(&cp, "Configure the behaviour of PuTTY's window",
		 IDC_TITLE_WINDOW);
	beginbox(&cp, NULL, IDC_BOX_BEHAVIOUR1);
	checkbox(&cp, "&Warn before closing window", IDC_CLOSEWARN);
	checkbox(&cp, "Window closes on ALT-F&4", IDC_ALTF4);
	checkbox(&cp, "S&ystem menu appears on ALT-Space", IDC_ALTSPACE);
	checkbox(&cp, "System menu appears on A&LT alone", IDC_ALTONLY);
	checkbox(&cp, "Ensure window is always on &top", IDC_ALWAYSONTOP);
	checkbox(&cp, "&Full screen on Alt-Enter", IDC_FULLSCREENONALTENTER);
	endbox(&cp);
    }

    if (panel == translationpanelstart) {
	/* The Translation panel. Accelerators used: [acgoh] rxbepus */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	bartitle(&cp, "Options controlling character set translation",
		 IDC_TITLE_TRANSLATION);
	beginbox(&cp, "Character set translation on received data",
		 IDC_BOX_TRANSLATION1);
	combobox(&cp, "&Received data assumed to be in which character set:",
		 IDC_CODEPAGESTATIC, IDC_CODEPAGE);
	endbox(&cp);
        beginbox(&cp, "Enable character set translation on input data",
                 IDC_BOX_TRANSLATION2);
        checkbox(&cp, "Cap&s Lock acts as Cyrillic switch",
                 IDC_CAPSLOCKCYR);
        endbox(&cp);
	beginbox(&cp, "Adjust how PuTTY displays line drawing characters",
		 IDC_BOX_TRANSLATION3);
	radiobig(&cp,
		 "Handling of line drawing characters:", IDC_VTSTATIC,
		 "Font has &XWindows encoding", IDC_VTXWINDOWS,
		 "Use font in &both ANSI and OEM modes", IDC_VTOEMANSI,
		 "Use font in O&EM mode only", IDC_VTOEMONLY,
		 "&Poor man's line drawing (" "+" ", " "-" " and " "|" ")",
		 IDC_VTPOORMAN, "&Unicode mode", IDC_VTUNICODE, NULL);
	endbox(&cp);
    }

    if (panel == selectionpanelstart) {
	/* The Selection panel. Accelerators used: [acgoh] df wxp est nr */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	bartitle(&cp, "Options controlling copy and paste",
		 IDC_TITLE_SELECTION);
	beginbox(&cp, "Translation of pasted characters",
		 IDC_BOX_SELECTION1);
	checkbox(&cp,
		 "&Don't translate line drawing chars into +, - and |",
		 IDC_RAWCNP);
	checkbox(&cp,
		 "Paste to clipboard in RT&F as well as plain text",
		 IDC_RTFPASTE);
	endbox(&cp);
	beginbox(&cp, "Control which mouse button does which thing",
		 IDC_BOX_SELECTION2);
	radiobig(&cp, "Action of mouse buttons:", IDC_MBSTATIC,
		 "&Windows (Right pastes, Middle extends)", IDC_MBWINDOWS,
		 "&xterm (Right extends, Middle pastes)", IDC_MBXTERM,
		 NULL);
	checkbox(&cp,
		 "Shift overrides a&pplication's use of mouse",
		 IDC_MOUSEOVERRIDE);
        radioline(&cp,
                  "Default selection mode (Alt+drag does the other one):",
                  IDC_SELTYPESTATIC, 2,
		  "&Normal", IDC_SELTYPELEX,
		  "&Rectangular block", IDC_SELTYPERECT, NULL);
	endbox(&cp);
	beginbox(&cp, "Control the select-one-word-at-a-time mode",
		 IDC_BOX_SELECTION3);
	charclass(&cp, "Charact&er classes:", IDC_CCSTATIC, IDC_CCLIST,
		  "&Set", IDC_CCSET, IDC_CCEDIT,
		  "&to class", IDC_CCSTATIC2);
	endbox(&cp);
    }

    if (panel == colourspanelstart) {
	/* The Colours panel. Accelerators used: [acgoh] blum */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	bartitle(&cp, "Options controlling use of colours",
		 IDC_TITLE_COLOURS);
	beginbox(&cp, "General options for colour usage",
		 IDC_BOX_COLOURS1);
	checkbox(&cp, "&Bolded text is a different colour",
		 IDC_BOLDCOLOUR);
	checkbox(&cp, "Attempt to use &logical palettes", IDC_PALETTE);
	endbox(&cp);
	beginbox(&cp, "Adjust the precise colours PuTTY displays",
		 IDC_BOX_COLOURS2);
	colouredit(&cp, "Select a colo&ur and then click to modify it:",
		   IDC_COLOURSTATIC, IDC_COLOURLIST,
		   "&Modify...", IDC_CHANGE,
		   "Red:", IDC_RSTATIC, IDC_RVALUE,
		   "Green:", IDC_GSTATIC, IDC_GVALUE,
		   "Blue:", IDC_BSTATIC, IDC_BVALUE, NULL);
	endbox(&cp);
    }

    if (panel == connectionpanelstart) {
	/* The Connection panel. Accelerators used: [acgoh] tukn */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	bartitle(&cp, "Options controlling the connection",
		 IDC_TITLE_CONNECTION);
	if (dlgtype == 0) {
	    beginbox(&cp, "Data to send to the server",
		     IDC_BOX_CONNECTION1);
	    staticedit(&cp, "Terminal-&type string", IDC_TTSTATIC,
		       IDC_TTEDIT, 50);
	    staticedit(&cp, "Auto-login &username", IDC_LOGSTATIC,
		       IDC_LOGEDIT, 50);
	    endbox(&cp);
	} else {
	    beginbox(&cp, "Adjust telnet session.", IDC_BOX_CONNECTION1);
	    checkbox(&cp, "Keyboard sends telnet Backspace and Interrupt",
		     IDC_TELNETKEY);
	    checkbox(&cp, "Return key sends telnet New Line instead of ^M",
		     IDC_TELNETRET);
	    endbox(&cp);
	}
	beginbox(&cp, "Sending of null packets to keep session active",
		 IDC_BOX_CONNECTION2);
	staticedit(&cp, "Seconds between &keepalives (0 to turn off)",
		   IDC_PINGSTATIC, IDC_PINGEDIT, 20);
	endbox(&cp);
	if (dlgtype == 0) {
	    beginbox(&cp, "Low-level TCP connection options",
		     IDC_BOX_CONNECTION3);
	    checkbox(&cp, "Disable &Nagle's algorithm (TCP_NODELAY option)",
		     IDC_NODELAY);
	    endbox(&cp);
	}
    }

    if (panel == proxypanelstart) {
	/* The Proxy panel. Accelerators used: [acgoh] ntslypeuwmv */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	if (dlgtype == 0) {
	    bartitle(&cp, "Options controlling proxy usage",
		     IDC_TITLE_PROXY);
	    beginbox(&cp, "Proxy basics", IDC_BOX_PROXY1);
	    radioline(&cp, "Proxy type:", IDC_PROXYTYPESTATIC, 4,
		      "&None", IDC_PROXYTYPENONE,
		      "H&TTP", IDC_PROXYTYPEHTTP,
		      "&SOCKS", IDC_PROXYTYPESOCKS,
		      "Te&lnet", IDC_PROXYTYPETELNET, NULL);
	    multiedit(&cp,
		      "Prox&y Host", IDC_PROXYHOSTSTATIC, IDC_PROXYHOSTEDIT, 80,
		      "&Port", IDC_PROXYPORTSTATIC, IDC_PROXYPORTEDIT, 20, NULL);
	    multiedit(&cp,
		      "&Exclude Hosts/IPs", IDC_PROXYEXCLUDESTATIC,
		      IDC_PROXYEXCLUDEEDIT, 100, NULL);
	    staticedit(&cp, "&Username", IDC_PROXYUSERSTATIC,
		       IDC_PROXYUSEREDIT, 60);
	    staticpassedit(&cp, "Pass&word", IDC_PROXYPASSSTATIC,
			   IDC_PROXYPASSEDIT, 60);
	    endbox(&cp);
	    beginbox(&cp, "Misc. proxy settings", IDC_BOX_PROXY2);
	    multiedit(&cp,
		      "Telnet co&mmand", IDC_PROXYTELNETCMDSTATIC,
		      IDC_PROXYTELNETCMDEDIT, 100, NULL);
	    radioline(&cp, "SOCKS &Version", IDC_PROXYSOCKSVERSTATIC,
		      2, "Version 5", IDC_PROXYSOCKSVER5, "Version 4",
		      IDC_PROXYSOCKSVER4, NULL);
	    endbox(&cp);
	}
    }

    if (panel == telnetpanelstart) {
	/* The Telnet panel. Accelerators used: [acgoh] svldr bftk */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	if (dlgtype == 0) {
	    bartitle(&cp, "Options controlling Telnet connections",
		     IDC_TITLE_TELNET);
	    beginbox(&cp, "Data to send to the server", IDC_BOX_TELNET1);
	    staticedit(&cp, "Terminal-&speed string", IDC_TSSTATIC,
		       IDC_TSEDIT, 50);
	    envsetter(&cp, "Environment variables:", IDC_ENVSTATIC,
		      "&Variable", IDC_VARSTATIC, IDC_VAREDIT, "Va&lue",
		      IDC_VALSTATIC, IDC_VALEDIT, IDC_ENVLIST, "A&dd",
		      IDC_ENVADD, "&Remove", IDC_ENVREMOVE);
	    endbox(&cp);
	    beginbox(&cp, "Telnet protocol adjustments", IDC_BOX_TELNET2);
	    radioline(&cp, "Handling of OLD_ENVIRON ambiguity:",
		      IDC_EMSTATIC, 2, "&BSD (commonplace)", IDC_EMBSD,
		      "R&FC 1408 (unusual)", IDC_EMRFC, NULL);
	    radioline(&cp, "&Telnet negotiation mode:", IDC_ACTSTATIC, 2,
		      "Passive", IDC_TPASSIVE, "Active",
		      IDC_TACTIVE, NULL);
	    checkbox(&cp, "&Keyboard sends telnet Backspace and Interrupt",
		     IDC_TELNETKEY);
	    checkbox(&cp, "Return key sends telnet New Line instead of ^M",
		     IDC_TELNETRET);
	    endbox(&cp);
	}
    }

    if (panel == rloginpanelstart) {
	/* The Rlogin panel. Accelerators used: [acgoh] sl */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	if (dlgtype == 0) {
	    bartitle(&cp, "Options controlling Rlogin connections",
		     IDC_TITLE_RLOGIN);
	    beginbox(&cp, "Data to send to the server", IDC_BOX_RLOGIN1);
	    staticedit(&cp, "Terminal-&speed string", IDC_R_TSSTATIC,
		       IDC_R_TSEDIT, 50);
	    staticedit(&cp, "&Local username:", IDC_RLLUSERSTATIC,
		       IDC_RLLUSEREDIT, 50);
	    endbox(&cp);
	}
    }

    if (panel == sshpanelstart) {
	/* The SSH panel. Accelerators used: [acgoh] r pe12ni sd */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	if (dlgtype == 0) {
	    bartitle(&cp, "Options controlling SSH connections",
		     IDC_TITLE_SSH);
	    beginbox(&cp, "Data to send to the server", IDC_BOX_SSH1);
	    multiedit(&cp,
		      "&Remote command:", IDC_CMDSTATIC, IDC_CMDEDIT, 100,
		      NULL);
	    endbox(&cp);
	    beginbox(&cp, "Protocol options", IDC_BOX_SSH2);
	    checkbox(&cp, "Don't allocate a &pseudo-terminal", IDC_NOPTY);
	    checkbox(&cp, "Enable compr&ession", IDC_COMPRESS);
	    radioline(&cp, "Preferred SSH protocol version:",
		      IDC_SSHPROTSTATIC, 4,
		      "1 on&ly", IDC_SSHPROT1ONLY,
		      "&1", IDC_SSHPROT1, "&2", IDC_SSHPROT2,
		      "2 o&nly", IDC_SSHPROT2ONLY, NULL);
	    endbox(&cp);
	    beginbox(&cp, "Encryption options", IDC_BOX_SSH3);
	    prefslist(&cipherlist, &cp, "Encryption cipher &selection policy:",
		      IDC_CIPHERSTATIC2, IDC_CIPHERLIST, IDC_CIPHERUP,
		      IDC_CIPHERDN);
	    checkbox(&cp, "Enable non-standard use of single-&DES in SSH 2",
		     IDC_SSH2DES);
	    endbox(&cp);
	}
    }

    if (panel == sshauthpanelstart) {
	/* The SSH authentication panel. Accelerators used: [acgoh] m fkiuw */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	if (dlgtype == 0) {
	    bartitle(&cp, "Options controlling SSH authentication",
		     IDC_TITLE_SSHAUTH);
	    beginbox(&cp, "Authentication methods",
		     IDC_BOX_SSHAUTH1);
	    checkbox(&cp, "Atte&mpt TIS or CryptoCard authentication (SSH1)",
		     IDC_AUTHTIS);
	    checkbox(&cp, "Attempt \"keyboard-&interactive\" authentication"
		     " (SSH2)", IDC_AUTHKI);
	    endbox(&cp);
	    beginbox(&cp, "Authentication parameters",
		     IDC_BOX_SSHAUTH2);
	    checkbox(&cp, "Allow agent &forwarding", IDC_AGENTFWD);
	    checkbox(&cp, "Allow attempted changes of &username in SSH2",
		     IDC_CHANGEUSER);
	    editbutton(&cp, "Private &key file for authentication:",
		       IDC_PKSTATIC, IDC_PKEDIT, "Bro&wse...",
		       IDC_PKBUTTON);
	    endbox(&cp);
	}
    }

    if (panel == sshbugspanelstart) {
	/* The SSH bugs panel. Accelerators used: [acgoh] isrmep */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	if (dlgtype == 0) {
	    bartitle(&cp, "Workarounds for SSH server bugs",
		     IDC_TITLE_SSHBUGS);
	    beginbox(&cp, "Detection of known bugs in SSH servers",
		     IDC_BOX_SSHBUGS1);
	    staticddl(&cp, "Chokes on SSH1 &ignore messages",
		      IDC_BUGS_IGNORE1, IDC_BUGD_IGNORE1, 20);
	    staticddl(&cp, "Refuses all SSH1 pa&ssword camouflage",
		      IDC_BUGS_PLAINPW1, IDC_BUGD_PLAINPW1, 20);
	    staticddl(&cp, "Chokes on SSH1 &RSA authentication",
		      IDC_BUGS_RSA1, IDC_BUGD_RSA1, 20);
	    staticddl(&cp, "Miscomputes SSH2 H&MAC keys",
		      IDC_BUGS_HMAC2, IDC_BUGD_HMAC2, 20);
	    staticddl(&cp, "Miscomputes SSH2 &encryption keys",
		      IDC_BUGS_DERIVEKEY2, IDC_BUGD_DERIVEKEY2, 20);
	    staticddl(&cp, "Requires &padding on SSH2 RSA signatures",
		      IDC_BUGS_RSAPAD2, IDC_BUGD_RSAPAD2, 20);
	    staticddl(&cp, "Chokes on &Diffie-Hellman group exchange",
		      IDC_BUGS_DHGEX2, IDC_BUGD_DHGEX2, 20);
	    endbox(&cp);
	}
    }

    if (panel == tunnelspanelstart) {
	/* The Tunnels panel. Accelerators used: [acgoh] deilmrstxp */
	struct ctlpos cp;
	ctlposinit(&cp, hwnd, 80, 3, 13);
	if (dlgtype == 0) {
	    bartitle(&cp, "Options controlling SSH tunnelling",
		     IDC_TITLE_TUNNELS);
	    beginbox(&cp, "X11 forwarding", IDC_BOX_TUNNELS1);
	    checkbox(&cp, "&Enable X11 forwarding", IDC_X11_FORWARD);
	    multiedit(&cp, "&X display location", IDC_X11_DISPSTATIC,
		      IDC_X11_DISPLAY, 50, NULL);
	    endbox(&cp);
    	    beginbox(&cp, "Port forwarding", IDC_BOX_TUNNELS2);
	    checkbox(&cp, "Local ports accept connections from o&ther hosts",
		     IDC_LPORT_ALL);
	    checkbox(&cp, "Remote &ports do the same (SSH v2 only)",
		     IDC_RPORT_ALL);
	    staticbtn(&cp, "Forwarded ports:", IDC_PFWDSTATIC,
		      "&Remove", IDC_PFWDREMOVE);
	    fwdsetter(&cp, IDC_PFWDLIST,
		      "Add new forwarded port:", IDC_PFWDSTATIC2,
		      "&Source port", IDC_SPORTSTATIC, IDC_SPORTEDIT,
		      "Dest&ination", IDC_DPORTSTATIC, IDC_DPORTEDIT,
		      "A&dd", IDC_PFWDADD);
	    bareradioline(&cp, 2,
			  "&Local", IDC_PFWDLOCAL,
			  "Re&mote", IDC_PFWDREMOTE, NULL);
	    endbox(&cp);

	}
    }
}

/* 
 * Helper function to load the session selected in SESSLIST
 * if any, as this is done in more than one place in
 * GenericMainDlgProc(). 0 => failure.
 */
static int load_selected_session(HWND hwnd)
{
    int n = SendDlgItemMessage(hwnd, IDC_SESSLIST,
			       LB_GETCURSEL, 0, 0);
    int isdef;
    if (n == LB_ERR) {
	MessageBeep(0);
	return 0;
    }
    isdef = !strcmp(sessions[n], "Default Settings");
    load_settings(sessions[n], !isdef, &cfg);
    init_dlg_ctrls(hwnd, TRUE);
    if (!isdef)
	SetDlgItemText(hwnd, IDC_SESSEDIT, sessions[n]);
    else
	SetDlgItemText(hwnd, IDC_SESSEDIT, "");
    /* Restore the selection, which will have been clobbered by
     * SESSEDIT handling. */
    SendDlgItemMessage(hwnd, IDC_SESSLIST, LB_SETCURSEL, n, 0);
    return 1;
}

/*
 * This function is the configuration box.
 */
static int GenericMainDlgProc(HWND hwnd, UINT msg,
			      WPARAM wParam, LPARAM lParam, int dlgtype)
{
    HWND hw, treeview;
    struct treeview_faff tvfaff;
    HTREEITEM hsession;
    OPENFILENAME of;
    char filename[sizeof(cfg.keyfile)];
    CHOOSEFONT cf;
    LOGFONT lf;
    char fontstatic[256];
    char portname[32];
    struct servent *service;
    int i;
    static UINT draglistmsg = WM_NULL;

    switch (msg) {
      case WM_INITDIALOG:
	readytogo = 0;
	SetWindowLong(hwnd, GWL_USERDATA, 0);
        if (help_path)
            SetWindowLong(hwnd, GWL_EXSTYLE,
                          GetWindowLong(hwnd, GWL_EXSTYLE) | WS_EX_CONTEXTHELP);
        else {
            HWND item = GetDlgItem(hwnd, IDC_HELPBTN);
            if (item)
                DestroyWindow(item);
        }
        requested_help = FALSE;
	SendMessage(hwnd, WM_SETICON, (WPARAM) ICON_BIG,
		    (LPARAM) LoadIcon(hinst, MAKEINTRESOURCE(IDI_CFGICON)));
	/*
	 * Centre the window.
	 */
	{			       /* centre the window */
	    RECT rs, rd;

	    hw = GetDesktopWindow();
	    if (GetWindowRect(hw, &rs) && GetWindowRect(hwnd, &rd))
		MoveWindow(hwnd,
			   (rs.right + rs.left + rd.left - rd.right) / 2,
			   (rs.bottom + rs.top + rd.top - rd.bottom) / 2,
			   rd.right - rd.left, rd.bottom - rd.top, TRUE);
	}

	/*
	 * Create the tree view.
	 */
	{
	    RECT r;
	    WPARAM font;
	    HWND tvstatic;

	    r.left = 3;
	    r.right = r.left + 75;
	    r.top = 3;
	    r.bottom = r.top + 10;
	    MapDialogRect(hwnd, &r);
	    tvstatic = CreateWindowEx(0, "STATIC", "Cate&gory:",
				      WS_CHILD | WS_VISIBLE,
				      r.left, r.top,
				      r.right - r.left, r.bottom - r.top,
				      hwnd, (HMENU) IDCX_TVSTATIC, hinst,
				      NULL);
	    font = SendMessage(hwnd, WM_GETFONT, 0, 0);
	    SendMessage(tvstatic, WM_SETFONT, font, MAKELPARAM(TRUE, 0));

	    r.left = 3;
	    r.right = r.left + 75;
	    r.top = 13;
	    r.bottom = r.top + 219;
	    MapDialogRect(hwnd, &r);
	    treeview = CreateWindowEx(WS_EX_CLIENTEDGE, WC_TREEVIEW, "",
				      WS_CHILD | WS_VISIBLE |
				      WS_TABSTOP | TVS_HASLINES |
				      TVS_DISABLEDRAGDROP | TVS_HASBUTTONS
				      | TVS_LINESATROOT |
				      TVS_SHOWSELALWAYS, r.left, r.top,
				      r.right - r.left, r.bottom - r.top,
				      hwnd, (HMENU) IDCX_TREEVIEW, hinst,
				      NULL);
	    font = SendMessage(hwnd, WM_GETFONT, 0, 0);
	    SendMessage(treeview, WM_SETFONT, font, MAKELPARAM(TRUE, 0));
	    tvfaff.treeview = treeview;
	    memset(tvfaff.lastat, 0, sizeof(tvfaff.lastat));
	}

	/*
	 * Set up the tree view contents.
	 */
	hsession = treeview_insert(&tvfaff, 0, "Session");
	treeview_insert(&tvfaff, 1, "Logging");
	treeview_insert(&tvfaff, 0, "Terminal");
	treeview_insert(&tvfaff, 1, "Keyboard");
	treeview_insert(&tvfaff, 1, "Bell");
	treeview_insert(&tvfaff, 1, "Features");
	treeview_insert(&tvfaff, 0, "Window");
	treeview_insert(&tvfaff, 1, "Appearance");
	treeview_insert(&tvfaff, 1, "Behaviour");
	treeview_insert(&tvfaff, 1, "Translation");
	treeview_insert(&tvfaff, 1, "Selection");
	treeview_insert(&tvfaff, 1, "Colours");
	treeview_insert(&tvfaff, 0, "Connection");
	if (dlgtype == 0) {
	    treeview_insert(&tvfaff, 1, "Proxy");
	    treeview_insert(&tvfaff, 1, "Telnet");
	    treeview_insert(&tvfaff, 1, "Rlogin");
	    if (backends[3].backend != NULL) {
		treeview_insert(&tvfaff, 1, "SSH");
		/* XXX long name is ugly */
		/* XXX make it closed by default? */
		treeview_insert(&tvfaff, 2, "Auth");
		treeview_insert(&tvfaff, 2, "Tunnels");
		treeview_insert(&tvfaff, 2, "Bugs");
	    }
	}

	/*
	 * Put the treeview selection on to the Session panel. This
	 * should also cause creation of the relevant controls.
	 */
	TreeView_SelectItem(treeview, hsession);

	/*
	 * Set focus into the first available control.
	 */
	{
	    HWND ctl;
	    ctl = GetDlgItem(hwnd, IDC_HOST);
	    if (!ctl)
		ctl = GetDlgItem(hwnd, IDC_CLOSEEXIT);
	    SetFocus(ctl);
	}

	SetWindowLong(hwnd, GWL_USERDATA, 1);
	sesslist_has_focus = 0;
	return 0;
      case WM_LBUTTONUP:
	/*
	 * Button release should trigger WM_OK if there was a
	 * previous double click on the session list.
	 */
	ReleaseCapture();
	if (readytogo)
	    SendMessage(hwnd, WM_COMMAND, IDOK, 0);
	break;
      case WM_NOTIFY:
	if (LOWORD(wParam) == IDCX_TREEVIEW &&
	    ((LPNMHDR) lParam)->code == TVN_SELCHANGED) {
	    HTREEITEM i =
		TreeView_GetSelection(((LPNMHDR) lParam)->hwndFrom);
	    TVITEM item;
	    int j;
	    char buffer[64];
 
 	    SendMessage (hwnd, WM_SETREDRAW, FALSE, 0);
 
	    item.hItem = i;
	    item.pszText = buffer;
	    item.cchTextMax = sizeof(buffer);
	    item.mask = TVIF_TEXT;
	    TreeView_GetItem(((LPNMHDR) lParam)->hwndFrom, &item);
	    for (j = controlstartvalue; j < controlendvalue; j++) {
		HWND item = GetDlgItem(hwnd, j);
		if (item)
		    DestroyWindow(item);
	    }
	    if (!strcmp(buffer, "Session"))
		create_controls(hwnd, dlgtype, sessionpanelstart);
	    if (!strcmp(buffer, "Logging"))
		create_controls(hwnd, dlgtype, loggingpanelstart);
	    if (!strcmp(buffer, "Keyboard"))
		create_controls(hwnd, dlgtype, keyboardpanelstart);
	    if (!strcmp(buffer, "Terminal"))
		create_controls(hwnd, dlgtype, terminalpanelstart);
	    if (!strcmp(buffer, "Bell"))
		create_controls(hwnd, dlgtype, bellpanelstart);
	    if (!strcmp(buffer, "Features"))
		create_controls(hwnd, dlgtype, featurespanelstart);
	    if (!strcmp(buffer, "Window"))
		create_controls(hwnd, dlgtype, windowpanelstart);
	    if (!strcmp(buffer, "Appearance"))
		create_controls(hwnd, dlgtype, appearancepanelstart);
	    if (!strcmp(buffer, "Behaviour"))
		create_controls(hwnd, dlgtype, behaviourpanelstart);
	    if (!strcmp(buffer, "Tunnels"))
		create_controls(hwnd, dlgtype, tunnelspanelstart);
	    if (!strcmp(buffer, "Connection"))
		create_controls(hwnd, dlgtype, connectionpanelstart);
	    if (!strcmp(buffer, "Proxy"))
		create_controls(hwnd, dlgtype, proxypanelstart);
	    if (!strcmp(buffer, "Telnet"))
		create_controls(hwnd, dlgtype, telnetpanelstart);
	    if (!strcmp(buffer, "Rlogin"))
		create_controls(hwnd, dlgtype, rloginpanelstart);
	    if (!strcmp(buffer, "SSH"))
		create_controls(hwnd, dlgtype, sshpanelstart);
	    if (!strcmp(buffer, "Auth"))
		create_controls(hwnd, dlgtype, sshauthpanelstart);
	    if (!strcmp(buffer, "Bugs"))
		create_controls(hwnd, dlgtype, sshbugspanelstart);
	    if (!strcmp(buffer, "Selection"))
		create_controls(hwnd, dlgtype, selectionpanelstart);
	    if (!strcmp(buffer, "Colours"))
		create_controls(hwnd, dlgtype, colourspanelstart);
	    if (!strcmp(buffer, "Translation"))
		create_controls(hwnd, dlgtype, translationpanelstart);

	    init_dlg_ctrls(hwnd, FALSE);
 
	    SendMessage (hwnd, WM_SETREDRAW, TRUE, 0);
 	    InvalidateRect (hwnd, NULL, TRUE);

	    SetFocus(((LPNMHDR) lParam)->hwndFrom);	/* ensure focus stays */
	    return 0;
	}
	break;
      case WM_COMMAND:
	/*
	 * Only process WM_COMMAND once the dialog is fully formed.
	 */
	if (GetWindowLong(hwnd, GWL_USERDATA) == 1)
	    switch (LOWORD(wParam)) {
	      case IDOK:
		/* Behaviour of the "Open" button is different if the
		 * session list has focus, *unless* the user just
		 * double-clicked... */
		if (sesslist_has_focus && !readytogo) {
		    if (!load_selected_session(hwnd)) {
			MessageBeep(0);
			return 0;
		    }
		}
		/* If at this point we have a valid session, go! */
		if (*cfg.host) {
                    if (requested_help) {
                        WinHelp(hwnd, help_path, HELP_QUIT, 0);
                        requested_help = FALSE;
                    }
		    EndDialog(hwnd, 1);
                } else
		    MessageBeep(0);
		return 0;
	      case IDC_HELPBTN:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
                    if (help_path) {
                        WinHelp(hwnd, help_path,
                                help_has_contents ? HELP_FINDER : HELP_CONTENTS,
                                0);
                        requested_help = TRUE;
                    }
                }
                break;
	      case IDCANCEL:
                if (requested_help) {
                    WinHelp(hwnd, help_path, HELP_QUIT, 0);
                    requested_help = FALSE;
                }
		EndDialog(hwnd, 0);
		return 0;
	      case IDC_PROTTELNET:
	      case IDC_PROTRLOGIN:
	      case IDC_PROTSSH:
	      case IDC_PROTRAW:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    int i = IsDlgButtonChecked(hwnd, IDC_PROTSSH);
		    int j = IsDlgButtonChecked(hwnd, IDC_PROTTELNET);
		    int k = IsDlgButtonChecked(hwnd, IDC_PROTRLOGIN);
		    cfg.protocol =
			i ? PROT_SSH : j ? PROT_TELNET : k ? PROT_RLOGIN :
			PROT_RAW;
		    /*
		     * When switching using the arrow keys, we
		     * appear to get two of these messages, both
		     * mentioning the target button in
		     * LOWORD(wParam), but one of them called while
		     * the previous button is still checked. This
		     * causes an unnecessary reset of the port
		     * number field, which we fix by ensuring here
		     * that the button selected is indeed the one
		     * checked.
		     */
		    if (IsDlgButtonChecked(hwnd, LOWORD(wParam)) &&
			((cfg.protocol == PROT_SSH && cfg.port != 22)
			 || (cfg.protocol == PROT_TELNET && cfg.port != 23)
			 || (cfg.protocol == PROT_RLOGIN
			     && cfg.port != 513))) {
			cfg.port = i ? 22 : j ? 23 : 513;
			SetDlgItemInt(hwnd, IDC_PORT, cfg.port, FALSE);
		    }
		}
		break;
	      case IDC_HOST:
		if (HIWORD(wParam) == EN_CHANGE)
		    GetDlgItemText(hwnd, IDC_HOST, cfg.host,
				   sizeof(cfg.host) - 1);
		break;
	      case IDC_PORT:
		if (HIWORD(wParam) == EN_CHANGE) {
		    GetDlgItemText(hwnd, IDC_PORT, portname, 31);
		    if (isdigit(portname[0]))
			MyGetDlgItemInt(hwnd, IDC_PORT, &cfg.port);
		    else {
			service = getservbyname(portname, NULL);
			if (service)
			    cfg.port = ntohs(service->s_port);
			else
			    cfg.port = 0;
		    }
		}
		break;
	      case IDC_SESSEDIT:
		if (HIWORD(wParam) == EN_CHANGE) {
		    SendDlgItemMessage(hwnd, IDC_SESSLIST, LB_SETCURSEL,
				       (WPARAM) - 1, 0);
		    GetDlgItemText(hwnd, IDC_SESSEDIT,
				   savedsession, sizeof(savedsession) - 1);
		    savedsession[sizeof(savedsession) - 1] = '\0';
		}
		break;
	      case IDC_SESSSAVE:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    /*
		     * Save a session
		     */
		    char str[2048];
		    GetDlgItemText(hwnd, IDC_SESSEDIT, str,
				   sizeof(str) - 1);
		    if (!*str) {
			int n = SendDlgItemMessage(hwnd, IDC_SESSLIST,
						   LB_GETCURSEL, 0, 0);
			if (n == LB_ERR) {
			    MessageBeep(0);
			    break;
			}
			strcpy(str, sessions[n]);
		    }
		    save_settings(str, !!strcmp(str, "Default Settings"),
				  &cfg);
		    get_sesslist(FALSE);
		    get_sesslist(TRUE);
		    SendDlgItemMessage(hwnd, IDC_SESSLIST, WM_SETREDRAW,
				       FALSE, 0);
		    SendDlgItemMessage(hwnd, IDC_SESSLIST, LB_RESETCONTENT,
				       0, 0);
		    for (i = 0; i < nsessions; i++)
			SendDlgItemMessage(hwnd, IDC_SESSLIST,
					   LB_ADDSTRING, 0,
					   (LPARAM) (sessions[i]));
		    SendDlgItemMessage(hwnd, IDC_SESSLIST, LB_SETCURSEL,
				       (WPARAM) - 1, 0);
		    SendDlgItemMessage(hwnd, IDC_SESSLIST, WM_SETREDRAW,
				       TRUE, 0);
		    InvalidateRect(GetDlgItem(hwnd, IDC_SESSLIST), NULL,
				   TRUE);
		}
		break;
	      case IDC_SESSLIST:
	      case IDC_SESSLOAD:
		if (LOWORD(wParam) == IDC_SESSLIST) {
		    if (HIWORD(wParam) == LBN_SETFOCUS)
			sesslist_has_focus = 1;
		    else if (HIWORD(wParam) == LBN_KILLFOCUS)
			sesslist_has_focus = 0;
		}
		if (LOWORD(wParam) == IDC_SESSLOAD &&
		    HIWORD(wParam) != BN_CLICKED &&
		    HIWORD(wParam) != BN_DOUBLECLICKED) break;
		if (LOWORD(wParam) == IDC_SESSLIST &&
		    HIWORD(wParam) != LBN_DBLCLK) break;
		/* Load the session selected in SESSLIST. */
		if (load_selected_session(hwnd) &&
		    LOWORD(wParam) == IDC_SESSLIST) {
		    /*
		     * A double-click on a saved session should
		     * actually start the session, not just load it.
		     * Unless it's Default Settings or some other
		     * host-less set of saved settings.
		     */
		    if (*cfg.host) {
			readytogo = TRUE;
			SetCapture(hwnd);
		    }
		}
		break;
	      case IDC_SESSDEL:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    int n = SendDlgItemMessage(hwnd, IDC_SESSLIST,
					       LB_GETCURSEL, 0, 0);
		    if (n == LB_ERR || n == 0) {
			MessageBeep(0);
			break;
		    }
		    del_settings(sessions[n]);
		    get_sesslist(FALSE);
		    get_sesslist(TRUE);
		    SendDlgItemMessage(hwnd, IDC_SESSLIST, WM_SETREDRAW,
				       FALSE, 0);
		    SendDlgItemMessage(hwnd, IDC_SESSLIST, LB_RESETCONTENT,
				       0, 0);
		    for (i = 0; i < nsessions; i++)
			SendDlgItemMessage(hwnd, IDC_SESSLIST,
					   LB_ADDSTRING, 0,
					   (LPARAM) (sessions[i]));
		    SendDlgItemMessage(hwnd, IDC_SESSLIST, LB_SETCURSEL,
				       (WPARAM) - 1, 0);
		    SendDlgItemMessage(hwnd, IDC_SESSLIST, WM_SETREDRAW,
				       TRUE, 0);
		    InvalidateRect(GetDlgItem(hwnd, IDC_SESSLIST), NULL,
				   TRUE);
		}
	      case IDC_PINGEDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    MyGetDlgItemInt(hwnd, IDC_PINGEDIT,
				    &cfg.ping_interval);
		break;
	      case IDC_NODELAY:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.tcp_nodelay =
			IsDlgButtonChecked(hwnd, IDC_NODELAY);
		break;
	      case IDC_DEL008:
	      case IDC_DEL127:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.bksp_is_delete =
			IsDlgButtonChecked(hwnd, IDC_DEL127);
		break;
	      case IDC_HOMETILDE:
	      case IDC_HOMERXVT:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.rxvt_homeend =
			IsDlgButtonChecked(hwnd, IDC_HOMERXVT);
		break;
	      case IDC_FUNCTILDE:
	      case IDC_FUNCLINUX:
	      case IDC_FUNCXTERM:
	      case IDC_FUNCVT400:
	      case IDC_FUNCVT100P:
	      case IDC_FUNCSCO:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			switch (LOWORD(wParam)) {
		      case IDC_FUNCTILDE:
			cfg.funky_type = 0;
			break;
		      case IDC_FUNCLINUX:
			cfg.funky_type = 1;
			break;
		      case IDC_FUNCXTERM:
			cfg.funky_type = 2;
			break;
		      case IDC_FUNCVT400:
			cfg.funky_type = 3;
			break;
		      case IDC_FUNCVT100P:
			cfg.funky_type = 4;
			break;
		      case IDC_FUNCSCO:
			cfg.funky_type = 5;
			break;
		    }
		break;
	      case IDC_KPNORMAL:
	      case IDC_KPAPPLIC:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    cfg.app_keypad =
			IsDlgButtonChecked(hwnd, IDC_KPAPPLIC);
		    cfg.nethack_keypad = FALSE;
		}
		break;
	      case IDC_KPNH:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    cfg.app_keypad = FALSE;
		    cfg.nethack_keypad = TRUE;
		}
		break;
	      case IDC_CURNORMAL:
	      case IDC_CURAPPLIC:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.app_cursor =
			IsDlgButtonChecked(hwnd, IDC_CURAPPLIC);
		break;
	      case IDC_NOAPPLICC:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.no_applic_c =
			IsDlgButtonChecked(hwnd, IDC_NOAPPLICC);
		break;
	      case IDC_NOAPPLICK:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.no_applic_k =
			IsDlgButtonChecked(hwnd, IDC_NOAPPLICK);
		break;
	      case IDC_NOMOUSEREP:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.no_mouse_rep =
			IsDlgButtonChecked(hwnd, IDC_NOMOUSEREP);
		break;
	      case IDC_NORESIZE:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
		        cfg.no_remote_resize =
		        IsDlgButtonChecked(hwnd, IDC_NORESIZE);
		break;
	      case IDC_NOALTSCREEN:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
		        cfg.no_alt_screen =
		        IsDlgButtonChecked(hwnd, IDC_NOALTSCREEN);
		break;
	      case IDC_NOWINTITLE:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
		        cfg.no_remote_wintitle =
		        IsDlgButtonChecked(hwnd, IDC_NOWINTITLE);
		break;
	      case IDC_NODBACKSPACE:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
		        cfg.no_dbackspace =
		        IsDlgButtonChecked(hwnd, IDC_NODBACKSPACE);
		break;
	      case IDC_NOCHARSET:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
		        cfg.no_remote_charset =
		        IsDlgButtonChecked(hwnd, IDC_NOCHARSET);
		break;
	      case IDC_ALTF4:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.alt_f4 = IsDlgButtonChecked(hwnd, IDC_ALTF4);
		break;
	      case IDC_ALTSPACE:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.alt_space =
			IsDlgButtonChecked(hwnd, IDC_ALTSPACE);
		break;
	      case IDC_ALTONLY:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.alt_only =
			IsDlgButtonChecked(hwnd, IDC_ALTONLY);
		break;
	      case IDC_ECHOBACKEND:
	      case IDC_ECHOYES:
	      case IDC_ECHONO:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    if (LOWORD(wParam) == IDC_ECHOBACKEND)
			cfg.localecho = LD_BACKEND;
		    if (LOWORD(wParam) == IDC_ECHOYES)
			cfg.localecho = LD_YES;
		    if (LOWORD(wParam) == IDC_ECHONO)
			cfg.localecho = LD_NO;
		}
		break;
	      case IDC_EDITBACKEND:
	      case IDC_EDITYES:
	      case IDC_EDITNO:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    if (LOWORD(wParam) == IDC_EDITBACKEND)
			cfg.localedit = LD_BACKEND;
		    if (LOWORD(wParam) == IDC_EDITYES)
			cfg.localedit = LD_YES;
		    if (LOWORD(wParam) == IDC_EDITNO)
			cfg.localedit = LD_NO;
		}
		break;
	      case IDC_ANSWEREDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    GetDlgItemText(hwnd, IDC_ANSWEREDIT, cfg.answerback,
				   sizeof(cfg.answerback) - 1);
		break;
	      case IDC_ALWAYSONTOP:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.alwaysontop =
			IsDlgButtonChecked(hwnd, IDC_ALWAYSONTOP);
		break;
	      case IDC_FULLSCREENONALTENTER:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.fullscreenonaltenter =
			IsDlgButtonChecked(hwnd, IDC_FULLSCREENONALTENTER);
		break;
	      case IDC_SCROLLKEY:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.scroll_on_key =
			IsDlgButtonChecked(hwnd, IDC_SCROLLKEY);
		break;
	      case IDC_SCROLLDISP:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.scroll_on_disp =
			IsDlgButtonChecked(hwnd, IDC_SCROLLDISP);
		break;
	      case IDC_COMPOSEKEY:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.compose_key =
			IsDlgButtonChecked(hwnd, IDC_COMPOSEKEY);
		break;
	      case IDC_CTRLALTKEYS:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.ctrlaltkeys =
			IsDlgButtonChecked(hwnd, IDC_CTRLALTKEYS);
		break;
	      case IDC_TELNETKEY:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.telnet_keyboard =
			IsDlgButtonChecked(hwnd, IDC_TELNETKEY);
		break;
	      case IDC_TELNETRET:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.telnet_newline =
			IsDlgButtonChecked(hwnd, IDC_TELNETRET);
		break;
	      case IDC_WRAPMODE:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.wrap_mode =
			IsDlgButtonChecked(hwnd, IDC_WRAPMODE);
		break;
	      case IDC_DECOM:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.dec_om = IsDlgButtonChecked(hwnd, IDC_DECOM);
		break;
	      case IDC_LFHASCR:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.lfhascr =
			IsDlgButtonChecked(hwnd, IDC_LFHASCR);
		break;
	      case IDC_ROWSEDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    MyGetDlgItemInt(hwnd, IDC_ROWSEDIT, &cfg.height);
		break;
	      case IDC_COLSEDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    MyGetDlgItemInt(hwnd, IDC_COLSEDIT, &cfg.width);
		break;
	      case IDC_SAVEEDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    MyGetDlgItemInt(hwnd, IDC_SAVEEDIT, &cfg.savelines);
		break;
	      case IDC_CHOOSEFONT:
		{
		    HDC hdc = GetDC(0);
		    lf.lfHeight = -MulDiv(cfg.fontheight,
					  GetDeviceCaps(hdc, LOGPIXELSY),
					  72);
		    ReleaseDC(0, hdc);
		}
		lf.lfWidth = lf.lfEscapement = lf.lfOrientation = 0;
		lf.lfItalic = lf.lfUnderline = lf.lfStrikeOut = 0;
		lf.lfWeight = (cfg.fontisbold ? FW_BOLD : 0);
		lf.lfCharSet = cfg.fontcharset;
		lf.lfOutPrecision = OUT_DEFAULT_PRECIS;
		lf.lfClipPrecision = CLIP_DEFAULT_PRECIS;
		lf.lfQuality = DEFAULT_QUALITY;
		lf.lfPitchAndFamily = FIXED_PITCH | FF_DONTCARE;
		strncpy(lf.lfFaceName, cfg.font,
			sizeof(lf.lfFaceName) - 1);
		lf.lfFaceName[sizeof(lf.lfFaceName) - 1] = '\0';

		cf.lStructSize = sizeof(cf);
		cf.hwndOwner = hwnd;
		cf.lpLogFont = &lf;
		cf.Flags = CF_FIXEDPITCHONLY | CF_FORCEFONTEXIST |
		    CF_INITTOLOGFONTSTRUCT | CF_SCREENFONTS;

		if (ChooseFont(&cf)) {
		    strncpy(cfg.font, lf.lfFaceName, sizeof(cfg.font) - 1);
		    cfg.font[sizeof(cfg.font) - 1] = '\0';
		    cfg.fontisbold = (lf.lfWeight == FW_BOLD);
		    cfg.fontcharset = lf.lfCharSet;
		    cfg.fontheight = cf.iPointSize / 10;
		    fmtfont(fontstatic);
		    SetDlgItemText(hwnd, IDC_FONTSTATIC, fontstatic);
		}
		break;
	      case IDC_BELL_DISABLED:
	      case IDC_BELL_DEFAULT:
	      case IDC_BELL_WAVEFILE:
	      case IDC_BELL_VISUAL:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    if (LOWORD(wParam) == IDC_BELL_DISABLED)
			cfg.beep = BELL_DISABLED;
		    if (LOWORD(wParam) == IDC_BELL_DEFAULT)
			cfg.beep = BELL_DEFAULT;
		    if (LOWORD(wParam) == IDC_BELL_WAVEFILE)
			cfg.beep = BELL_WAVEFILE;
		    if (LOWORD(wParam) == IDC_BELL_VISUAL)
			cfg.beep = BELL_VISUAL;
		}
		break;
	      case IDC_B_IND_DISABLED:
	      case IDC_B_IND_FLASH:
	      case IDC_B_IND_STEADY:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    if (LOWORD(wParam) == IDC_B_IND_DISABLED)
			cfg.beep_ind = B_IND_DISABLED;
		    if (LOWORD(wParam) == IDC_B_IND_FLASH)
			cfg.beep_ind = B_IND_FLASH;
		    if (LOWORD(wParam) == IDC_B_IND_STEADY)
			cfg.beep_ind = B_IND_STEADY;
		}
		break;
	      case IDC_BELL_WAVEBROWSE:
		memset(&of, 0, sizeof(of));
#ifdef OPENFILENAME_SIZE_VERSION_400
		of.lStructSize = OPENFILENAME_SIZE_VERSION_400;
#else
		of.lStructSize = sizeof(of);
#endif
		of.hwndOwner = hwnd;
		of.lpstrFilter = "Wave Files\0*.WAV\0AllFiles\0*\0\0\0";
		of.lpstrCustomFilter = NULL;
		of.nFilterIndex = 1;
		of.lpstrFile = filename;
		strcpy(filename, cfg.bell_wavefile);
		of.nMaxFile = sizeof(filename);
		of.lpstrFileTitle = NULL;
		of.lpstrInitialDir = NULL;
		of.lpstrTitle = "Select Bell Sound File";
		of.Flags = 0;
		if (GetOpenFileName(&of)) {
		    strcpy(cfg.bell_wavefile, filename);
		    SetDlgItemText(hwnd, IDC_BELL_WAVEEDIT,
				   cfg.bell_wavefile);
		}
		break;
	      case IDC_BELL_WAVEEDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    GetDlgItemText(hwnd, IDC_BELL_WAVEEDIT,
				   cfg.bell_wavefile,
				   sizeof(cfg.bell_wavefile) - 1);
		break;
	      case IDC_BELLOVL:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.bellovl =
			IsDlgButtonChecked(hwnd, IDC_BELLOVL);
		break;
	      case IDC_BELLOVLN:
		if (HIWORD(wParam) == EN_CHANGE)
		    MyGetDlgItemInt(hwnd, IDC_BELLOVLN, &cfg.bellovl_n);
		break;
	      case IDC_BELLOVLT:
		if (HIWORD(wParam) == EN_CHANGE)
		    MyGetDlgItemFlt(hwnd, IDC_BELLOVLT, &cfg.bellovl_t,
				    1000);
		break;
	      case IDC_BELLOVLS:
		if (HIWORD(wParam) == EN_CHANGE)
		    MyGetDlgItemFlt(hwnd, IDC_BELLOVLS, &cfg.bellovl_s,
				    1000);
		break;
	      case IDC_BLINKTEXT:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.blinktext =
			IsDlgButtonChecked(hwnd, IDC_BLINKTEXT);
		break;
	      case IDC_BCE:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.bce = IsDlgButtonChecked(hwnd, IDC_BCE);
		break;
	      case IDC_WINNAME:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.win_name_always =
			IsDlgButtonChecked(hwnd, IDC_WINNAME);
		break;
	      case IDC_HIDEMOUSE:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.hide_mouseptr =
			IsDlgButtonChecked(hwnd, IDC_HIDEMOUSE);
		break;
	      case IDC_SUNKENEDGE:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.sunken_edge =
			IsDlgButtonChecked(hwnd, IDC_SUNKENEDGE);
		break;
	      case IDC_WINBEDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    MyGetDlgItemInt(hwnd, IDC_WINBEDIT,
				    &cfg.window_border);
		if (cfg.window_border > 32)
		    cfg.window_border = 32;
		break;
	      case IDC_CURBLOCK:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.cursor_type = 0;
		break;
	      case IDC_CURUNDER:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.cursor_type = 1;
		break;
	      case IDC_CURVERT:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.cursor_type = 2;
		break;
	      case IDC_BLINKCUR:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.blink_cur =
			IsDlgButtonChecked(hwnd, IDC_BLINKCUR);
		break;
	      case IDC_SCROLLBAR:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.scrollbar =
			IsDlgButtonChecked(hwnd, IDC_SCROLLBAR);
		break;
	      case IDC_SCROLLBARFULLSCREEN:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
		    cfg.scrollbar_in_fullscreen =
		    IsDlgButtonChecked(hwnd, IDC_SCROLLBARFULLSCREEN);
		break;
	      case IDC_RESIZETERM:
	      case IDC_RESIZEFONT:
	      case IDC_RESIZENONE:
	      case IDC_RESIZEEITHER:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    cfg.resize_action =
			IsDlgButtonChecked(hwnd,
					   IDC_RESIZETERM) ? RESIZE_TERM :
			IsDlgButtonChecked(hwnd,
					   IDC_RESIZEFONT) ? RESIZE_FONT :
			IsDlgButtonChecked(hwnd,
					   IDC_RESIZEEITHER) ? RESIZE_EITHER :
			RESIZE_DISABLED;
		}
		break;
	      case IDC_WINEDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    GetDlgItemText(hwnd, IDC_WINEDIT, cfg.wintitle,
				   sizeof(cfg.wintitle) - 1);
		break;
	      case IDC_COEALWAYS:
	      case IDC_COENEVER:
	      case IDC_COENORMAL:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    cfg.close_on_exit =
			IsDlgButtonChecked(hwnd,
					   IDC_COEALWAYS) ? COE_ALWAYS :
			IsDlgButtonChecked(hwnd,
					   IDC_COENEVER) ? COE_NEVER :
			COE_NORMAL;
		}
		break;
	      case IDC_CLOSEWARN:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.warn_on_close =
			IsDlgButtonChecked(hwnd, IDC_CLOSEWARN);
		break;
	      case IDC_TTEDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    GetDlgItemText(hwnd, IDC_TTEDIT, cfg.termtype,
				   sizeof(cfg.termtype) - 1);
		break;

		/* proxy config */
	      case IDC_PROXYHOSTEDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    GetDlgItemText(hwnd, IDC_PROXYHOSTEDIT, cfg.proxy_host, 
				   sizeof(cfg.proxy_host) - 1);
		break;
	      case IDC_PROXYPORTEDIT:
		if (HIWORD(wParam) == EN_CHANGE) {
		    GetDlgItemText(hwnd, IDC_PROXYPORTEDIT, portname, 31);
		    if (isdigit(portname[0]))
			MyGetDlgItemInt(hwnd, IDC_PROXYPORTEDIT, &cfg.proxy_port);
		    else {
			service = getservbyname(portname, NULL);
			if (service)
			    cfg.proxy_port = ntohs(service->s_port);
			else
			    cfg.proxy_port = 0;
		    }
		}
		break;
	      case IDC_PROXYEXCLUDEEDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    GetDlgItemText(hwnd, IDC_PROXYEXCLUDEEDIT,
				   cfg.proxy_exclude_list,
				   sizeof(cfg.proxy_exclude_list) - 1);
		break;
	      case IDC_PROXYUSEREDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    GetDlgItemText(hwnd, IDC_PROXYUSEREDIT,
				   cfg.proxy_username, 
				   sizeof(cfg.proxy_username) - 1);
		break;
	      case IDC_PROXYPASSEDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    GetDlgItemText(hwnd, IDC_PROXYPASSEDIT,
				   cfg.proxy_password, 
				   sizeof(cfg.proxy_password) - 1);
		break;
	      case IDC_PROXYTELNETCMDEDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    GetDlgItemText(hwnd, IDC_PROXYTELNETCMDEDIT,
				   cfg.proxy_telnet_command,
				   sizeof(cfg.proxy_telnet_command) - 1);
		break;
	      case IDC_PROXYSOCKSVER5:
	      case IDC_PROXYSOCKSVER4:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    cfg.proxy_socks_version =
			IsDlgButtonChecked(hwnd, IDC_PROXYSOCKSVER4) ? 4 : 5;
		}
		break;
	      case IDC_PROXYTYPENONE:
	      case IDC_PROXYTYPEHTTP:
	      case IDC_PROXYTYPESOCKS:
	      case IDC_PROXYTYPETELNET:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    cfg.proxy_type =
			IsDlgButtonChecked(hwnd, IDC_PROXYTYPEHTTP) ? PROXY_HTTP :
			IsDlgButtonChecked(hwnd, IDC_PROXYTYPESOCKS) ? PROXY_SOCKS :
			IsDlgButtonChecked(hwnd, IDC_PROXYTYPETELNET) ? PROXY_TELNET :
			PROXY_NONE;
		}
		break;

	      case IDC_LGFEDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    GetDlgItemText(hwnd, IDC_LGFEDIT, cfg.logfilename,
				   sizeof(cfg.logfilename) - 1);
		break;
	      case IDC_LGFBUTTON:
		memset(&of, 0, sizeof(of));
#ifdef OPENFILENAME_SIZE_VERSION_400
		of.lStructSize = OPENFILENAME_SIZE_VERSION_400;
#else
		of.lStructSize = sizeof(of);
#endif
		of.hwndOwner = hwnd;
		of.lpstrFilter = "All Files\0*\0\0\0";
		of.lpstrCustomFilter = NULL;
		of.nFilterIndex = 1;
		of.lpstrFile = filename;
		strcpy(filename, cfg.logfilename);
		of.nMaxFile = sizeof(filename);
		of.lpstrFileTitle = NULL;
		of.lpstrInitialDir = NULL;
		of.lpstrTitle = "Select session log file";
		of.Flags = 0;
		if (GetSaveFileName(&of)) {
		    strcpy(cfg.logfilename, filename);
		    SetDlgItemText(hwnd, IDC_LGFEDIT, cfg.logfilename);
		}
		break;
	      case IDC_LSTATOFF:
	      case IDC_LSTATASCII:
	      case IDC_LSTATRAW:
	      case IDC_LSTATPACKET:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    if (IsDlgButtonChecked(hwnd, IDC_LSTATOFF))
			cfg.logtype = LGTYP_NONE;
		    if (IsDlgButtonChecked(hwnd, IDC_LSTATASCII))
			cfg.logtype = LGTYP_ASCII;
		    if (IsDlgButtonChecked(hwnd, IDC_LSTATRAW))
			cfg.logtype = LGTYP_DEBUG;
		    if (IsDlgButtonChecked(hwnd, IDC_LSTATPACKET))
			cfg.logtype = LGTYP_PACKETS;
		}
		break;
	      case IDC_LSTATXASK:
	      case IDC_LSTATXAPN:
	      case IDC_LSTATXOVR:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    if (IsDlgButtonChecked(hwnd, IDC_LSTATXASK))
			cfg.logxfovr = LGXF_ASK;
		    if (IsDlgButtonChecked(hwnd, IDC_LSTATXAPN))
			cfg.logxfovr = LGXF_APN;
		    if (IsDlgButtonChecked(hwnd, IDC_LSTATXOVR))
			cfg.logxfovr = LGXF_OVR;
		}
		break;
	      case IDC_TSEDIT:
	      case IDC_R_TSEDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    GetDlgItemText(hwnd, LOWORD(wParam), cfg.termspeed,
				   sizeof(cfg.termspeed) - 1);
		break;
	      case IDC_LOGEDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    GetDlgItemText(hwnd, IDC_LOGEDIT, cfg.username,
				   sizeof(cfg.username) - 1);
		break;
	      case IDC_RLLUSEREDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    GetDlgItemText(hwnd, IDC_RLLUSEREDIT,
				   cfg.localusername,
				   sizeof(cfg.localusername) - 1);
		break;
	      case IDC_EMBSD:
	      case IDC_EMRFC:
		cfg.rfc_environ = IsDlgButtonChecked(hwnd, IDC_EMRFC);
		break;
	      case IDC_TPASSIVE:
	      case IDC_TACTIVE:
		cfg.passive_telnet =
		    IsDlgButtonChecked(hwnd, IDC_TPASSIVE);
		break;
	      case IDC_ENVADD:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    char str[sizeof(cfg.environmt)];
		    char *p;
		    GetDlgItemText(hwnd, IDC_VAREDIT, str,
				   sizeof(str) - 1);
		    if (!*str) {
			MessageBeep(0);
			break;
		    }
		    p = str + strlen(str);
		    *p++ = '\t';
		    GetDlgItemText(hwnd, IDC_VALEDIT, p,
				   sizeof(str) - 1 - (p - str));
		    if (!*p) {
			MessageBeep(0);
			break;
		    }
		    p = cfg.environmt;
		    while (*p) {
			while (*p)
			    p++;
			p++;
		    }
		    if ((p - cfg.environmt) + strlen(str) + 2 <
			sizeof(cfg.environmt)) {
			strcpy(p, str);
			p[strlen(str) + 1] = '\0';
			SendDlgItemMessage(hwnd, IDC_ENVLIST, LB_ADDSTRING,
					   0, (LPARAM) str);
			SetDlgItemText(hwnd, IDC_VAREDIT, "");
			SetDlgItemText(hwnd, IDC_VALEDIT, "");
		    } else {
			MessageBox(hwnd, "Environment too big",
				   "PuTTY Error", MB_OK | MB_ICONERROR);
		    }
		}
		break;
	      case IDC_ENVREMOVE:
		if (HIWORD(wParam) != BN_CLICKED &&
		    HIWORD(wParam) != BN_DOUBLECLICKED) break;
		i =
		    SendDlgItemMessage(hwnd, IDC_ENVLIST, LB_GETCURSEL, 0,
				       0);
		if (i == LB_ERR)
		    MessageBeep(0);
		else {
		    char *p, *q;

		    SendDlgItemMessage(hwnd, IDC_ENVLIST, LB_DELETESTRING,
				       i, 0);
		    p = cfg.environmt;
		    while (i > 0) {
			if (!*p)
			    goto disaster;
			while (*p)
			    p++;
			p++;
			i--;
		    }
		    q = p;
		    if (!*p)
			goto disaster;
		    while (*p)
			p++;
		    p++;
		    while (*p) {
			while (*p)
			    *q++ = *p++;
			*q++ = *p++;
		    }
		    *q = '\0';
		  disaster:;
		}
		break;
	      case IDC_NOPTY:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.nopty = IsDlgButtonChecked(hwnd, IDC_NOPTY);
		break;
	      case IDC_COMPRESS:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.compression =
			IsDlgButtonChecked(hwnd, IDC_COMPRESS);
		break;
	      case IDC_SSH2DES:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.ssh2_des_cbc =
			IsDlgButtonChecked(hwnd, IDC_SSH2DES);
		break;
	      case IDC_AGENTFWD:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.agentfwd =
			IsDlgButtonChecked(hwnd, IDC_AGENTFWD);
		break;
	      case IDC_CHANGEUSER:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.change_username =
			IsDlgButtonChecked(hwnd, IDC_CHANGEUSER);
		break;
	      case IDC_CIPHERLIST:
	      case IDC_CIPHERUP:
	      case IDC_CIPHERDN:
		handle_prefslist(&cipherlist,
				 cfg.ssh_cipherlist, CIPHER_MAX,
				 0, hwnd, wParam, lParam);
		break;
	      case IDC_SSHPROT1ONLY:
	      case IDC_SSHPROT1:
	      case IDC_SSHPROT2:
      	      case IDC_SSHPROT2ONLY:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    if (IsDlgButtonChecked(hwnd, IDC_SSHPROT1ONLY))
			cfg.sshprot = 0;
		    if (IsDlgButtonChecked(hwnd, IDC_SSHPROT1))
			cfg.sshprot = 1;
		    else if (IsDlgButtonChecked(hwnd, IDC_SSHPROT2))
			cfg.sshprot = 2;
		    else if (IsDlgButtonChecked(hwnd, IDC_SSHPROT2ONLY))
			cfg.sshprot = 3;
		}
		break;
	      case IDC_AUTHTIS:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.try_tis_auth =
			IsDlgButtonChecked(hwnd, IDC_AUTHTIS);
		break;
	      case IDC_AUTHKI:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.try_ki_auth =
			IsDlgButtonChecked(hwnd, IDC_AUTHKI);
		break;
	      case IDC_PKEDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    GetDlgItemText(hwnd, IDC_PKEDIT, cfg.keyfile,
				   sizeof(cfg.keyfile) - 1);
		break;
	      case IDC_CMDEDIT:
		if (HIWORD(wParam) == EN_CHANGE)
		    GetDlgItemText(hwnd, IDC_CMDEDIT, cfg.remote_cmd,
				   sizeof(cfg.remote_cmd) - 1);
		break;
	      case IDC_PKBUTTON:
		memset(&of, 0, sizeof(of));
#ifdef OPENFILENAME_SIZE_VERSION_400
		of.lStructSize = OPENFILENAME_SIZE_VERSION_400;
#else
		of.lStructSize = sizeof(of);
#endif
		of.hwndOwner = hwnd;
		of.lpstrFilter = "PuTTY Private Key Files\0*.PPK\0"
		    "AllFiles\0*\0\0\0";
		of.lpstrCustomFilter = NULL;
		of.nFilterIndex = 1;
		of.lpstrFile = filename;
		strcpy(filename, cfg.keyfile);
		of.nMaxFile = sizeof(filename);
		of.lpstrFileTitle = NULL;
		of.lpstrInitialDir = NULL;
		of.lpstrTitle = "Select Private Key File";
		of.Flags = 0;
		if (GetOpenFileName(&of)) {
		    strcpy(cfg.keyfile, filename);
		    SetDlgItemText(hwnd, IDC_PKEDIT, cfg.keyfile);
		}
		break;
	      case IDC_RAWCNP:
		cfg.rawcnp = IsDlgButtonChecked(hwnd, IDC_RAWCNP);
		break;
	      case IDC_RTFPASTE:
		cfg.rtf_paste = IsDlgButtonChecked(hwnd, IDC_RTFPASTE);
		break;
	      case IDC_MBWINDOWS:
	      case IDC_MBXTERM:
		cfg.mouse_is_xterm = IsDlgButtonChecked(hwnd, IDC_MBXTERM);
		break;
	      case IDC_SELTYPELEX:
	      case IDC_SELTYPERECT:
		cfg.rect_select = IsDlgButtonChecked(hwnd, IDC_SELTYPERECT);
		break;
	      case IDC_MOUSEOVERRIDE:
		cfg.mouse_override = IsDlgButtonChecked(hwnd, IDC_MOUSEOVERRIDE);
		break;
	      case IDC_CCSET:
		{
		    BOOL ok;
		    int i;
		    int n = GetDlgItemInt(hwnd, IDC_CCEDIT, &ok, FALSE);

		    if (!ok)
			MessageBeep(0);
		    else {
			for (i = 0; i < 128; i++)
			    if (SendDlgItemMessage
				(hwnd, IDC_CCLIST, LB_GETSEL, i, 0)) {
				char str[100];
				cfg.wordness[i] = n;
				SendDlgItemMessage(hwnd, IDC_CCLIST,
						   LB_DELETESTRING, i, 0);
				sprintf(str, "%d\t(0x%02X)\t%c\t%d", i, i,
					(i >= 0x21 && i != 0x7F) ? i : ' ',
					cfg.wordness[i]);
				SendDlgItemMessage(hwnd, IDC_CCLIST,
						   LB_INSERTSTRING, i,
						   (LPARAM) str);
			    }
		    }
		}
		break;
	      case IDC_BOLDCOLOUR:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    int n, i;
		    cfg.bold_colour =
			IsDlgButtonChecked(hwnd, IDC_BOLDCOLOUR);
		    SendDlgItemMessage(hwnd, IDC_COLOURLIST, WM_SETREDRAW,
				       FALSE, 0);
		    n =
			SendDlgItemMessage(hwnd, IDC_COLOURLIST,
					   LB_GETCOUNT, 0, 0);
		    if (n != 12 + 10 * cfg.bold_colour) {
			for (i = n; i-- > 0;)
			    SendDlgItemMessage(hwnd, IDC_COLOURLIST,
					       LB_DELETESTRING, i, 0);
			for (i = 0; i < 22; i++)
			    if (cfg.bold_colour || permcolour[i])
				SendDlgItemMessage(hwnd, IDC_COLOURLIST,
						   LB_ADDSTRING, 0,
						   (LPARAM) colours[i]);
		    }
		    SendDlgItemMessage(hwnd, IDC_COLOURLIST, WM_SETREDRAW,
				       TRUE, 0);
		    InvalidateRect(GetDlgItem(hwnd, IDC_COLOURLIST), NULL,
				   TRUE);
		}
		break;
	      case IDC_PALETTE:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
			cfg.try_palette =
			IsDlgButtonChecked(hwnd, IDC_PALETTE);
		break;
	      case IDC_COLOURLIST:
		if (HIWORD(wParam) == LBN_DBLCLK ||
		    HIWORD(wParam) == LBN_SELCHANGE) {
		    int i =
			SendDlgItemMessage(hwnd, IDC_COLOURLIST,
					   LB_GETCURSEL,
					   0, 0);
		    if (!cfg.bold_colour)
			i = (i < 3 ? i * 2 : i == 3 ? 5 : i * 2 - 2);
		    SetDlgItemInt(hwnd, IDC_RVALUE, cfg.colours[i][0],
				  FALSE);
		    SetDlgItemInt(hwnd, IDC_GVALUE, cfg.colours[i][1],
				  FALSE);
		    SetDlgItemInt(hwnd, IDC_BVALUE, cfg.colours[i][2],
				  FALSE);
		}
		break;
	      case IDC_CHANGE:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    static CHOOSECOLOR cc;
		    static DWORD custom[16] = { 0 };	/* zero initialisers */
		    int i =
			SendDlgItemMessage(hwnd, IDC_COLOURLIST,
					   LB_GETCURSEL,
					   0, 0);
		    if (!cfg.bold_colour)
			i = (i < 3 ? i * 2 : i == 3 ? 5 : i * 2 - 2);
		    cc.lStructSize = sizeof(cc);
		    cc.hwndOwner = hwnd;
		    cc.hInstance = (HWND) hinst;
		    cc.lpCustColors = custom;
		    cc.rgbResult =
			RGB(cfg.colours[i][0], cfg.colours[i][1],
			    cfg.colours[i][2]);
		    cc.Flags = CC_FULLOPEN | CC_RGBINIT;
		    if (ChooseColor(&cc)) {
			cfg.colours[i][0] =
			    (unsigned char) (cc.rgbResult & 0xFF);
			cfg.colours[i][1] =
			    (unsigned char) (cc.rgbResult >> 8) & 0xFF;
			cfg.colours[i][2] =
			    (unsigned char) (cc.rgbResult >> 16) & 0xFF;
			SetDlgItemInt(hwnd, IDC_RVALUE, cfg.colours[i][0],
				      FALSE);
			SetDlgItemInt(hwnd, IDC_GVALUE, cfg.colours[i][1],
				      FALSE);
			SetDlgItemInt(hwnd, IDC_BVALUE, cfg.colours[i][2],
				      FALSE);
		    }
		}
		break;
	      case IDC_CODEPAGE:
		if (HIWORD(wParam) == CBN_SELCHANGE) {
		    int index = SendDlgItemMessage(hwnd, IDC_CODEPAGE,
						   CB_GETCURSEL, 0, 0);
		    SendDlgItemMessage(hwnd, IDC_CODEPAGE, CB_GETLBTEXT,
				       index, (LPARAM)cfg.line_codepage);
		} else if (HIWORD(wParam) == CBN_EDITCHANGE) {
		    GetDlgItemText(hwnd, IDC_CODEPAGE, cfg.line_codepage,
				   sizeof(cfg.line_codepage) - 1);
		} else if (HIWORD(wParam) == CBN_KILLFOCUS) {
		    strcpy(cfg.line_codepage,
			   cp_name(decode_codepage(cfg.line_codepage)));
		    SetDlgItemText(hwnd, IDC_CODEPAGE, cfg.line_codepage);
		}
		break;
	      case IDC_PRINTER:
		if (HIWORD(wParam) == CBN_SELCHANGE) {
		    int index = SendDlgItemMessage(hwnd, IDC_PRINTER,
						   CB_GETCURSEL, 0, 0);
		    SendDlgItemMessage(hwnd, IDC_PRINTER, CB_GETLBTEXT,
				       index, (LPARAM)cfg.printer);
		} else if (HIWORD(wParam) == CBN_EDITCHANGE) {
		    GetDlgItemText(hwnd, IDC_PRINTER, cfg.printer,
				   sizeof(cfg.printer) - 1);
		}
		if (!strcmp(cfg.printer, PRINTER_DISABLED_STRING))
		    *cfg.printer = '\0';
		break;
	      case IDC_CAPSLOCKCYR:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    cfg.xlat_capslockcyr =
			IsDlgButtonChecked (hwnd, IDC_CAPSLOCKCYR);
		}
		break;
	      case IDC_VTXWINDOWS:
	      case IDC_VTOEMANSI:
	      case IDC_VTOEMONLY:
	      case IDC_VTPOORMAN:
	      case IDC_VTUNICODE:
		cfg.vtmode =
		    (IsDlgButtonChecked(hwnd, IDC_VTXWINDOWS) ? VT_XWINDOWS
		     : IsDlgButtonChecked(hwnd,
					  IDC_VTOEMANSI) ? VT_OEMANSI :
		     IsDlgButtonChecked(hwnd,
					IDC_VTOEMONLY) ? VT_OEMONLY :
		     IsDlgButtonChecked(hwnd,
					IDC_VTUNICODE) ? VT_UNICODE :
		     VT_POORMAN);
		break;
	      case IDC_X11_FORWARD:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
		    cfg.x11_forward =
		    IsDlgButtonChecked(hwnd, IDC_X11_FORWARD);
		break;
	      case IDC_LPORT_ALL:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
		    cfg.lport_acceptall =
		    IsDlgButtonChecked(hwnd, IDC_LPORT_ALL);
		break;
	      case IDC_RPORT_ALL:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED)
		    cfg.rport_acceptall =
		    IsDlgButtonChecked(hwnd, IDC_RPORT_ALL);
		break;
	      case IDC_X11_DISPLAY:
		if (HIWORD(wParam) == EN_CHANGE)
		    GetDlgItemText(hwnd, IDC_X11_DISPLAY, cfg.x11_display,
				   sizeof(cfg.x11_display) - 1);
		break;
	      case IDC_PFWDADD:
		if (HIWORD(wParam) == BN_CLICKED ||
		    HIWORD(wParam) == BN_DOUBLECLICKED) {
		    char str[sizeof(cfg.portfwd)];
		    char *p;
		    if (IsDlgButtonChecked(hwnd, IDC_PFWDLOCAL))
			str[0] = 'L';
		    else
			str[0] = 'R';
		    GetDlgItemText(hwnd, IDC_SPORTEDIT, str+1,
				   sizeof(str) - 2);
		    if (!str[1]) {
			MessageBox(hwnd,
				   "You need to specify a source port number",
				   "PuTTY Error", MB_OK | MB_ICONERROR);
			break;
		    }
		    p = str + strlen(str);
		    *p++ = '\t';
		    GetDlgItemText(hwnd, IDC_DPORTEDIT, p,
				   sizeof(str) - 1 - (p - str));
		    if (!*p || !strchr(p, ':')) {
			MessageBox(hwnd,
				   "You need to specify a destination address\n"
				   "in the form \"host.name:port\"",
				   "PuTTY Error", MB_OK | MB_ICONERROR);
			break;
		    }
		    p = cfg.portfwd;
		    while (*p) {
			while (*p)
			    p++;
			p++;
		    }
		    if ((p - cfg.portfwd) + strlen(str) + 2 <
			sizeof(cfg.portfwd)) {
			strcpy(p, str);
			p[strlen(str) + 1] = '\0';
			SendDlgItemMessage(hwnd, IDC_PFWDLIST, LB_ADDSTRING,
					   0, (LPARAM) str);
			SetDlgItemText(hwnd, IDC_SPORTEDIT, "");
			SetDlgItemText(hwnd, IDC_DPORTEDIT, "");
		    } else {
			MessageBox(hwnd, "Too many forwardings",
				   "PuTTY Error", MB_OK | MB_ICONERROR);
		    }
		}
		break;
	      case IDC_PFWDREMOVE:
		if (HIWORD(wParam) != BN_CLICKED &&
		    HIWORD(wParam) != BN_DOUBLECLICKED) break;
		i = SendDlgItemMessage(hwnd, IDC_PFWDLIST,
				       LB_GETCURSEL, 0, 0);
		if (i == LB_ERR)
		    MessageBeep(0);
		else {
		    char *p, *q;

		    SendDlgItemMessage(hwnd, IDC_PFWDLIST, LB_DELETESTRING,
				       i, 0);
		    p = cfg.portfwd;
		    while (i > 0) {
			if (!*p)
			    goto disaster2;
			while (*p)
			    p++;
			p++;
			i--;
		    }
		    q = p;
		    if (!*p)
			goto disaster2;
		    while (*p)
			p++;
		    p++;
		    while (*p) {
			while (*p)
			    *q++ = *p++;
			*q++ = *p++;
		    }
		    *q = '\0';
		  disaster2:;
		}
		break;
	      case IDC_BUGD_IGNORE1:
		if (HIWORD(wParam) == CBN_SELCHANGE) {
		    int index = SendDlgItemMessage(hwnd, IDC_BUGD_IGNORE1,
						   CB_GETCURSEL, 0, 0);
		    cfg.sshbug_ignore1 = (index == 0 ? BUG_AUTO :
					  index == 1 ? BUG_OFF : BUG_ON);
		}
		break;
	      case IDC_BUGD_PLAINPW1:
		if (HIWORD(wParam) == CBN_SELCHANGE) {
		    int index = SendDlgItemMessage(hwnd, IDC_BUGD_PLAINPW1,
						   CB_GETCURSEL, 0, 0);
		    cfg.sshbug_plainpw1 = (index == 0 ? BUG_AUTO :
					   index == 1 ? BUG_OFF : BUG_ON);
		}
		break;
	      case IDC_BUGD_RSA1:
		if (HIWORD(wParam) == CBN_SELCHANGE) {
		    int index = SendDlgItemMessage(hwnd, IDC_BUGD_RSA1,
						   CB_GETCURSEL, 0, 0);
		    cfg.sshbug_rsa1 = (index == 0 ? BUG_AUTO :
				       index == 1 ? BUG_OFF : BUG_ON);
		}
		break;
	      case IDC_BUGD_HMAC2:
		if (HIWORD(wParam) == CBN_SELCHANGE) {
		    int index = SendDlgItemMessage(hwnd, IDC_BUGD_HMAC2,
						   CB_GETCURSEL, 0, 0);
		    cfg.sshbug_hmac2 = (index == 0 ? BUG_AUTO :
					index == 1 ? BUG_OFF : BUG_ON);
		}
		break;
	      case IDC_BUGD_DERIVEKEY2:
		if (HIWORD(wParam) == CBN_SELCHANGE) {
		    int index = SendDlgItemMessage(hwnd, IDC_BUGD_DERIVEKEY2,
						   CB_GETCURSEL, 0, 0);
		    cfg.sshbug_derivekey2 = (index == 0 ? BUG_AUTO :
					     index == 1 ? BUG_OFF : BUG_ON);
		}
		break;
	      case IDC_BUGD_RSAPAD2:
		if (HIWORD(wParam) == CBN_SELCHANGE) {
		    int index = SendDlgItemMessage(hwnd, IDC_BUGD_RSAPAD2,
						   CB_GETCURSEL, 0, 0);
		    cfg.sshbug_rsapad2 = (index == 0 ? BUG_AUTO :
					  index == 1 ? BUG_OFF : BUG_ON);
		}
		break;
	      case IDC_BUGD_DHGEX2:
		if (HIWORD(wParam) == CBN_SELCHANGE) {
		    int index = SendDlgItemMessage(hwnd, IDC_BUGD_DHGEX2,
						   CB_GETCURSEL, 0, 0);
		    cfg.sshbug_dhgex2 = (index == 0 ? BUG_AUTO :
					 index == 1 ? BUG_OFF : BUG_ON);
		}
		break;
	    }
	return 0;
      case WM_HELP:
        if (help_path) {
            int id = ((LPHELPINFO)lParam)->iCtrlId;
            char *cmd = help_context_cmd(id);
            if (cmd) {
                WinHelp(hwnd, help_path, HELP_COMMAND, (DWORD)cmd);
                requested_help = TRUE;
            } else {
                MessageBeep(0);
            }
        }
        break;
      case WM_CLOSE:
        if (requested_help) {
            WinHelp(hwnd, help_path, HELP_QUIT, 0);
            requested_help = FALSE;
        }
	EndDialog(hwnd, 0);
	return 0;

	/* Grrr Explorer will maximize Dialogs! */
      case WM_SIZE:
	if (wParam == SIZE_MAXIMIZED)
	    force_normal(hwnd);
	return 0;

      default:
	/*
	 * Handle application-defined messages eg. DragListBox
	 */
	/* First find out what the number is (once). */
	if (draglistmsg == WM_NULL)
	    draglistmsg = RegisterWindowMessage (DRAGLISTMSGSTRING);

	if (msg == draglistmsg) {
	    /* Only process once dialog is fully formed. */
	    if (GetWindowLong(hwnd, GWL_USERDATA) == 1) switch (LOWORD(wParam)) {
	      case IDC_CIPHERLIST:
		return handle_prefslist(&cipherlist,
					cfg.ssh_cipherlist, CIPHER_MAX,
					1, hwnd, wParam, lParam);
	    }
	}
	return 0;

    }
    return 0;
}

static int CALLBACK MainDlgProc(HWND hwnd, UINT msg,
				WPARAM wParam, LPARAM lParam)
{
    if (msg == WM_COMMAND && LOWORD(wParam) == IDOK) {
    }
    if (msg == WM_COMMAND && LOWORD(wParam) == IDCX_ABOUT) {
	EnableWindow(hwnd, 0);
	DialogBox(hinst, MAKEINTRESOURCE(IDD_ABOUTBOX), hwnd, AboutProc);
	EnableWindow(hwnd, 1);
	SetActiveWindow(hwnd);
    }
    return GenericMainDlgProc(hwnd, msg, wParam, lParam, 0);
}

static int CALLBACK ReconfDlgProc(HWND hwnd, UINT msg,
				  WPARAM wParam, LPARAM lParam)
{
    return GenericMainDlgProc(hwnd, msg, wParam, lParam, 1);
}

void defuse_showwindow(void)
{
    /*
     * Work around the fact that the app's first call to ShowWindow
     * will ignore the default in favour of the shell-provided
     * setting.
     */
    {
	HWND hwnd;
	hwnd = CreateDialog(hinst, MAKEINTRESOURCE(IDD_ABOUTBOX),
			    NULL, NullDlgProc);
	ShowWindow(hwnd, SW_HIDE);
	SetActiveWindow(hwnd);
	DestroyWindow(hwnd);
    }
}

int do_config(void)
{
    int ret;

    get_sesslist(TRUE);
    savedsession[0] = '\0';
    ret =
	DialogBox(hinst, MAKEINTRESOURCE(IDD_MAINBOX), NULL, MainDlgProc);
    get_sesslist(FALSE);

    return ret;
}

int do_reconfig(HWND hwnd)
{
    Config backup_cfg;
    int ret;

    backup_cfg = cfg;		       /* structure copy */
    ret =
	DialogBox(hinst, MAKEINTRESOURCE(IDD_RECONF), hwnd, ReconfDlgProc);
    if (!ret)
	cfg = backup_cfg;	       /* structure copy */

    return ret;
}

void logevent(char *string)
{
    char timebuf[40];
    time_t t;

    log_eventlog(string);

    if (nevents >= negsize) {
	negsize += 64;
	events = srealloc(events, negsize * sizeof(*events));
    }

    time(&t);
    strftime(timebuf, sizeof(timebuf), "%Y-%m-%d %H:%M:%S\t",
	     localtime(&t));

    events[nevents] = smalloc(strlen(timebuf) + strlen(string) + 1);
    strcpy(events[nevents], timebuf);
    strcat(events[nevents], string);
    if (logbox) {
	int count;
	SendDlgItemMessage(logbox, IDN_LIST, LB_ADDSTRING,
			   0, (LPARAM) events[nevents]);
	count = SendDlgItemMessage(logbox, IDN_LIST, LB_GETCOUNT, 0, 0);
	SendDlgItemMessage(logbox, IDN_LIST, LB_SETTOPINDEX, count - 1, 0);
    }
    nevents++;
}

void showeventlog(HWND hwnd)
{
    if (!logbox) {
	logbox = CreateDialog(hinst, MAKEINTRESOURCE(IDD_LOGBOX),
			      hwnd, LogProc);
	ShowWindow(logbox, SW_SHOWNORMAL);
    }
    SetActiveWindow(logbox);
}

void showabout(HWND hwnd)
{
    DialogBox(hinst, MAKEINTRESOURCE(IDD_ABOUTBOX), hwnd, AboutProc);
}

void verify_ssh_host_key(char *host, int port, char *keytype,
			 char *keystr, char *fingerprint)
{
    int ret;

    static const char absentmsg[] =
	"The server's host key is not cached in the registry. You\n"
	"have no guarantee that the server is the computer you\n"
	"think it is.\n"
	"The server's key fingerprint is:\n"
	"%s\n"
	"If you trust this host, hit Yes to add the key to\n"
	"PuTTY's cache and carry on connecting.\n"
	"If you want to carry on connecting just once, without\n"
	"adding the key to the cache, hit No.\n"
	"If you do not trust this host, hit Cancel to abandon the\n"
	"connection.\n";

    static const char wrongmsg[] =
	"WARNING - POTENTIAL SECURITY BREACH!\n"
	"\n"
	"The server's host key does not match the one PuTTY has\n"
	"cached in the registry. This means that either the\n"
	"server administrator has changed the host key, or you\n"
	"have actually connected to another computer pretending\n"
	"to be the server.\n"
	"The new key fingerprint is:\n"
	"%s\n"
	"If you were expecting this change and trust the new key,\n"
	"hit Yes to update PuTTY's cache and continue connecting.\n"
	"If you want to carry on connecting but without updating\n"
	"the cache, hit No.\n"
	"If you want to abandon the connection completely, hit\n"
	"Cancel. Hitting Cancel is the ONLY guaranteed safe\n" "choice.\n";

    static const char mbtitle[] = "PuTTY Security Alert";

    char message[160 +
	/* sensible fingerprint max size */
	(sizeof(absentmsg) > sizeof(wrongmsg) ?
	 sizeof(absentmsg) : sizeof(wrongmsg))];

    /*
     * Verify the key against the registry.
     */
    ret = verify_host_key(host, port, keytype, keystr);

    if (ret == 0)		       /* success - key matched OK */
	return;
    if (ret == 2) {		       /* key was different */
	int mbret;
	sprintf(message, wrongmsg, fingerprint);
	mbret = MessageBox(NULL, message, mbtitle,
			   MB_ICONWARNING | MB_YESNOCANCEL);
	if (mbret == IDYES)
	    store_host_key(host, port, keytype, keystr);
	if (mbret == IDCANCEL)
	    cleanup_exit(0);
    }
    if (ret == 1) {		       /* key was absent */
	int mbret;
	sprintf(message, absentmsg, fingerprint);
	mbret = MessageBox(NULL, message, mbtitle,
			   MB_ICONWARNING | MB_YESNOCANCEL);
	if (mbret == IDYES)
	    store_host_key(host, port, keytype, keystr);
	if (mbret == IDCANCEL)
	    cleanup_exit(0);
    }
}

/*
 * Ask whether the selected cipher is acceptable (since it was
 * below the configured 'warn' threshold).
 * cs: 0 = both ways, 1 = client->server, 2 = server->client
 */
void askcipher(char *ciphername, int cs)
{
    static const char mbtitle[] = "PuTTY Security Alert";
    static const char msg[] =
	"The first %.35scipher supported by the server\n"
	"is %.64s, which is below the configured\n"
	"warning threshold.\n"
	"Do you want to continue with this connection?\n";
    /* guessed cipher name + type max length */
    char message[100 + sizeof(msg)];
    int mbret;

    sprintf(message, msg,
	    (cs == 0) ? "" :
	    (cs == 1) ? "client-to-server " :
		        "server-to-client ",
	    ciphername);
    mbret = MessageBox(NULL, message, mbtitle,
		       MB_ICONWARNING | MB_YESNO);
    if (mbret == IDYES)
	return;
    else
	cleanup_exit(0);
}

/*
 * Ask whether to wipe a session log file before writing to it.
 * Returns 2 for wipe, 1 for append, 0 for cancel (don't log).
 */
int askappend(char *filename)
{
    static const char mbtitle[] = "PuTTY Log to File";
    static const char msgtemplate[] =
	"The session log file \"%.*s\" already exists.\n"
	"You can overwrite it with a new session log,\n"
	"append your session log to the end of it,\n"
	"or disable session logging for this session.\n"
	"Hit Yes to wipe the file, No to append to it,\n"
	"or Cancel to disable logging.";
    char message[sizeof(msgtemplate) + FILENAME_MAX];
    int mbret;
    if (cfg.logxfovr != LGXF_ASK) {
	return ((cfg.logxfovr == LGXF_OVR) ? 2 : 1);
    }
    sprintf(message, msgtemplate, FILENAME_MAX, filename);

    mbret = MessageBox(NULL, message, mbtitle,
		       MB_ICONQUESTION | MB_YESNOCANCEL);
    if (mbret == IDYES)
	return 2;
    else if (mbret == IDNO)
	return 1;
    else
	return 0;
}

/*
 * Warn about the obsolescent key file format.
 */
void old_keyfile_warning(void)
{
    static const char mbtitle[] = "PuTTY Key File Warning";
    static const char message[] =
	"You are loading an SSH 2 private key which has an\n"
	"old version of the file format. This means your key\n"
	"file is not fully tamperproof. Future versions of\n"
	"PuTTY may stop supporting this private key format,\n"
	"so we recommend you convert your key to the new\n"
	"format.\n"
	"\n"
	"You can perform this conversion by loading the key\n"
	"into PuTTYgen and then saving it again.";

    MessageBox(NULL, message, mbtitle, MB_OK);
}
