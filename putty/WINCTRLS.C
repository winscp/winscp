/*
 * winctrls.c: routines to self-manage the controls in a dialog
 * box.
 */

/*
 * Possible TODO in new cross-platform config box stuff:
 *
 *  - When lining up two controls alongside each other, I wonder if
 *    we could conveniently arrange to centre them vertically?
 *    Particularly ugly in the current setup is the `Add new
 *    forwarded port:' static next to the rather taller `Remove'
 *    button.
 */

#include <assert.h>
#include <ctype.h>

#include "putty.h"
#include "misc.h"
#include "dialog.h"

#include <commctrl.h>

#define GAPBETWEEN 3
#define GAPWITHIN 1
#define GAPXBOX 7
#define GAPYBOX 4
#define DLGWIDTH 168
#define STATICHEIGHT 8
#define TITLEHEIGHT 12
#define CHECKBOXHEIGHT 8
#define RADIOHEIGHT 8
#define EDITHEIGHT 12
#define LISTHEIGHT 11
#define LISTINCREMENT 8
#define COMBOHEIGHT 12
#define PUSHBTNHEIGHT 14
#define PROGBARHEIGHT 14

void ctlposinit(struct ctlpos *cp, HWND hwnd,
		int leftborder, int rightborder, int topborder)
{
    RECT r, r2;
    cp->hwnd = hwnd;
    cp->font = SendMessage(hwnd, WM_GETFONT, 0, 0);
    cp->ypos = topborder;
    GetClientRect(hwnd, &r);
    r2.left = r2.top = 0;
    r2.right = 4;
    r2.bottom = 8;
    MapDialogRect(hwnd, &r2);
    cp->dlu4inpix = r2.right;
    cp->width = (r.right * 4) / (r2.right) - 2 * GAPBETWEEN;
    cp->xoff = leftborder;
    cp->width -= leftborder + rightborder;
}

HWND doctl(struct ctlpos *cp, RECT r,
	   char *wclass, int wstyle, int exstyle, char *wtext, int wid)
{
    HWND ctl;
    /*
     * Note nonstandard use of RECT. This is deliberate: by
     * transforming the width and height directly we arrange to
     * have all supposedly same-sized controls really same-sized.
     */

    r.left += cp->xoff;
    MapDialogRect(cp->hwnd, &r);

    /*
     * We can pass in cp->hwnd == NULL, to indicate a dry run
     * without creating any actual controls.
     */
    if (cp->hwnd) {
	ctl = CreateWindowEx(exstyle, wclass, wtext, wstyle,
			     r.left, r.top, r.right, r.bottom,
			     cp->hwnd, (HMENU) wid, hinst, NULL);
	SendMessage(ctl, WM_SETFONT, cp->font, MAKELPARAM(TRUE, 0));

	if (!strcmp(wclass, "LISTBOX")) {
	    /*
	     * Bizarre Windows bug: the list box calculates its
	     * number of lines based on the font it has at creation
	     * time, but sending it WM_SETFONT doesn't cause it to
	     * recalculate. So now, _after_ we've sent it
	     * WM_SETFONT, we explicitly resize it (to the same
	     * size it was already!) to force it to reconsider.
	     */
	    SetWindowPos(ctl, NULL, 0, 0, r.right, r.bottom,
			 SWP_NOACTIVATE | SWP_NOCOPYBITS |
			 SWP_NOMOVE | SWP_NOZORDER);
	}
    } else
	ctl = NULL;
    return ctl;
}

/*
 * A title bar across the top of a sub-dialog.
 */
void bartitle(struct ctlpos *cp, char *name, int id)
{
    RECT r;

    r.left = GAPBETWEEN;
    r.right = cp->width;
    r.top = cp->ypos;
    r.bottom = STATICHEIGHT;
    cp->ypos += r.bottom + GAPBETWEEN;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, name, id);
}

/*
 * Begin a grouping box, with or without a group title.
 */
void beginbox(struct ctlpos *cp, char *name, int idbox)
{
    cp->boxystart = cp->ypos;
    if (!name)
	cp->boxystart -= STATICHEIGHT / 2;
    if (name)
	cp->ypos += STATICHEIGHT;
    cp->ypos += GAPYBOX;
    cp->width -= 2 * GAPXBOX;
    cp->xoff += GAPXBOX;
    cp->boxid = idbox;
    cp->boxtext = name;
}

/*
 * End a grouping box.
 */
void endbox(struct ctlpos *cp)
{
    RECT r;
    cp->xoff -= GAPXBOX;
    cp->width += 2 * GAPXBOX;
    cp->ypos += GAPYBOX - GAPBETWEEN;
    r.left = GAPBETWEEN;
    r.right = cp->width;
    r.top = cp->boxystart;
    r.bottom = cp->ypos - cp->boxystart;
    doctl(cp, r, "BUTTON", BS_GROUPBOX | WS_CHILD | WS_VISIBLE, 0,
	  cp->boxtext ? cp->boxtext : "", cp->boxid);
    cp->ypos += GAPYBOX;
}

/*
 * Some edit boxes. Each one has a static above it. The percentages
 * of the horizontal space are provided.
 */
void multiedit(struct ctlpos *cp, int password, ...)
{
    RECT r;
    va_list ap;
    int percent, xpos;

    percent = xpos = 0;
    va_start(ap, password);
    while (1) {
	char *text;
	int staticid, editid, pcwidth;
	text = va_arg(ap, char *);
	if (!text)
	    break;
	staticid = va_arg(ap, int);
	editid = va_arg(ap, int);
	pcwidth = va_arg(ap, int);

	r.left = xpos + GAPBETWEEN;
	percent += pcwidth;
	xpos = (cp->width + GAPBETWEEN) * percent / 100;
	r.right = xpos - r.left;

	r.top = cp->ypos;
	r.bottom = STATICHEIGHT;
	doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, text, staticid);
	r.top = cp->ypos + 8 + GAPWITHIN;
	r.bottom = EDITHEIGHT;
	doctl(cp, r, "EDIT",
	      WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL |
	      (password ? ES_PASSWORD : 0),
	      WS_EX_CLIENTEDGE, "", editid);
    }
    va_end(ap);
    cp->ypos += STATICHEIGHT + GAPWITHIN + EDITHEIGHT + GAPBETWEEN;
}

/*
 * A static line, followed by a full-width combo box.
 */
void combobox(struct ctlpos *cp, char *text, int staticid, int listid)
{
    RECT r;

    r.left = GAPBETWEEN;
    r.right = cp->width;

    r.top = cp->ypos;
    r.bottom = STATICHEIGHT;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, text, staticid);
    r.top = cp->ypos + 8 + GAPWITHIN;
    r.bottom = COMBOHEIGHT * 10;
    doctl(cp, r, "COMBOBOX",
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL |
	  CBS_DROPDOWN | CBS_HASSTRINGS, WS_EX_CLIENTEDGE, "", listid);

    cp->ypos += STATICHEIGHT + GAPWITHIN + COMBOHEIGHT + GAPBETWEEN;
}

struct radio { char *text; int id; };

static void radioline_common(struct ctlpos *cp, char *text, int id,
			     int nacross, struct radio *buttons, int nbuttons)
{
    RECT r;
    int group;
    int i;
    int j;

    if (text) {
	r.left = GAPBETWEEN;
	r.top = cp->ypos;
	r.right = cp->width;
	r.bottom = STATICHEIGHT;
	cp->ypos += r.bottom + GAPWITHIN;
	doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, text, id);
    }

    group = WS_GROUP;
    i = 0;
    for (j = 0; j < nbuttons; j++) {
	char *btext = buttons[j].text;
	int bid = buttons[j].id;

	if (i == nacross) {
	    cp->ypos += r.bottom + (nacross > 1 ? GAPBETWEEN : GAPWITHIN);
	    i = 0;
	}
	r.left = GAPBETWEEN + i * (cp->width + GAPBETWEEN) / nacross;
	if (j < nbuttons-1)
	    r.right =
		(i + 1) * (cp->width + GAPBETWEEN) / nacross - r.left;
	else
	    r.right = cp->width - r.left;
	r.top = cp->ypos;
	r.bottom = RADIOHEIGHT;
	doctl(cp, r, "BUTTON",
	      BS_NOTIFY | BS_AUTORADIOBUTTON | WS_CHILD |
	      WS_VISIBLE | WS_TABSTOP | group, 0, btext, bid);
	group = 0;
	i++;
    }
    cp->ypos += r.bottom + GAPBETWEEN;
}

/*
 * A set of radio buttons on the same line, with a static above
 * them. `nacross' dictates how many parts the line is divided into
 * (you might want this not to equal the number of buttons if you
 * needed to line up some 2s and some 3s to look good in the same
 * panel).
 * 
 * There's a bit of a hack in here to ensure that if nacross
 * exceeds the actual number of buttons, the rightmost button
 * really does get all the space right to the edge of the line, so
 * you can do things like
 * 
 * (*) Button1  (*) Button2  (*) ButtonWithReallyLongTitle
 */
void radioline(struct ctlpos *cp, char *text, int id, int nacross, ...)
{
    va_list ap;
    struct radio *buttons;
    int i, nbuttons;

    va_start(ap, nacross);
    nbuttons = 0;
    while (1) {
	char *btext = va_arg(ap, char *);
	int bid;
	if (!btext)
	    break;
	bid = va_arg(ap, int);
	nbuttons++;
    }
    va_end(ap);
    buttons = snewn(nbuttons, struct radio);
    va_start(ap, nacross);
    for (i = 0; i < nbuttons; i++) {
	buttons[i].text = va_arg(ap, char *);
	buttons[i].id = va_arg(ap, int);
    }
    va_end(ap);
    radioline_common(cp, text, id, nacross, buttons, nbuttons);
    sfree(buttons);
}

/*
 * A set of radio buttons on the same line, without a static above
 * them. Otherwise just like radioline.
 */
void bareradioline(struct ctlpos *cp, int nacross, ...)
{
    va_list ap;
    struct radio *buttons;
    int i, nbuttons;

    va_start(ap, nacross);
    nbuttons = 0;
    while (1) {
	char *btext = va_arg(ap, char *);
	int bid;
	if (!btext)
	    break;
	bid = va_arg(ap, int);
    }
    va_end(ap);
    buttons = snewn(nbuttons, struct radio);
    va_start(ap, nacross);
    for (i = 0; i < nbuttons; i++) {
	buttons[i].text = va_arg(ap, char *);
	buttons[i].id = va_arg(ap, int);
    }
    va_end(ap);
    radioline_common(cp, NULL, 0, nacross, buttons, nbuttons);
    sfree(buttons);
}

/*
 * A set of radio buttons on multiple lines, with a static above
 * them.
 */
void radiobig(struct ctlpos *cp, char *text, int id, ...)
{
    va_list ap;
    struct radio *buttons;
    int i, nbuttons;

    va_start(ap, id);
    nbuttons = 0;
    while (1) {
	char *btext = va_arg(ap, char *);
	int bid;
	if (!btext)
	    break;
	bid = va_arg(ap, int);
    }
    va_end(ap);
    buttons = snewn(nbuttons, struct radio);
    va_start(ap, id);
    for (i = 0; i < nbuttons; i++) {
	buttons[i].text = va_arg(ap, char *);
	buttons[i].id = va_arg(ap, int);
    }
    va_end(ap);
    radioline_common(cp, text, id, 1, buttons, nbuttons);
    sfree(buttons);
}

/*
 * A single standalone checkbox.
 */
void checkbox(struct ctlpos *cp, char *text, int id)
{
    RECT r;

    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = CHECKBOXHEIGHT;
    cp->ypos += r.bottom + GAPBETWEEN;
    doctl(cp, r, "BUTTON",
	  BS_NOTIFY | BS_AUTOCHECKBOX | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 0,
	  text, id);
}

/*
 * Wrap a piece of text for a static text control. Returns the
 * wrapped text (a malloc'ed string containing \ns), and also
 * returns the number of lines required.
 */
char *staticwrap(struct ctlpos *cp, HWND hwnd, char *text, int *lines)
{
    HDC hdc = GetDC(hwnd);
    int lpx = GetDeviceCaps(hdc, LOGPIXELSX);
    int width, nlines, j;
    INT *pwidths, nfit;
    SIZE size;
    char *ret, *p, *q;
    RECT r;
    HFONT oldfont, newfont;

    ret = snewn(1+strlen(text), char);
    p = text;
    q = ret;
    pwidths = snewn(1+strlen(text), INT);

    /*
     * Work out the width the text will need to fit in, by doing
     * the same adjustment that the `statictext' function itself
     * will perform.
     */
    SetMapMode(hdc, MM_TEXT);	       /* ensure logical units == pixels */
    r.left = r.top = r.bottom = 0;
    r.right = cp->width;
    MapDialogRect(hwnd, &r);
    width = r.right;

    nlines = 1;

    /*
     * We must select the correct font into the HDC before calling
     * GetTextExtent*, or silly things will happen.
     */
    newfont = (HFONT)SendMessage(hwnd, WM_GETFONT, 0, 0);
    oldfont = SelectObject(hdc, newfont);

    while (*p) {
	if (!GetTextExtentExPoint(hdc, p, strlen(p), width,
				  &nfit, pwidths, &size) ||
	    (size_t)nfit >= strlen(p)) {
	    /*
	     * Either GetTextExtentExPoint returned failure, or the
	     * whole of the rest of the text fits on this line.
	     * Either way, we stop wrapping, copy the remainder of
	     * the input string unchanged to the output, and leave.
	     */
	    strcpy(q, p);
	    break;
	}

	/*
	 * Now we search backwards along the string from `nfit',
	 * looking for a space at which to break the line. If we
	 * don't find one at all, that's fine - we'll just break
	 * the line at `nfit'.
	 */
	for (j = nfit; j > 0; j--) {
	    if (isspace((unsigned char)p[j])) {
		nfit = j;
		break;
	    }
	}

	strncpy(q, p, nfit);
	q[nfit] = '\n';
	q += nfit+1;

	p += nfit;
	while (*p && isspace((unsigned char)*p))
	    p++;

	nlines++;
    }

    SelectObject(hdc, oldfont);
    ReleaseDC(cp->hwnd, hdc);

    if (lines) *lines = nlines;

    return ret;
}

/*
 * A single standalone static text control.
 */
void statictext(struct ctlpos *cp, char *text, int lines, int id)
{
    RECT r;

    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = STATICHEIGHT * lines;
    cp->ypos += r.bottom + GAPBETWEEN;
    doctl(cp, r, "STATIC",
	  WS_CHILD | WS_VISIBLE | SS_LEFTNOWORDWRAP,
	  0, text, id);
}

/*
 * An owner-drawn static text control for a panel title.
 */
void paneltitle(struct ctlpos *cp, int id)
{
    RECT r;

    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = TITLEHEIGHT;
    cp->ypos += r.bottom + GAPBETWEEN;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE | SS_OWNERDRAW,
	  0, NULL, id);
}

/*
 * A button on the right hand side, with a static to its left.
 */
void staticbtn(struct ctlpos *cp, char *stext, int sid,
	       char *btext, int bid)
{
    const int height = (PUSHBTNHEIGHT > STATICHEIGHT ?
			PUSHBTNHEIGHT : STATICHEIGHT);
    RECT r;
    int lwid, rwid, rpos;

    rpos = GAPBETWEEN + 3 * (cp->width + GAPBETWEEN) / 4;
    lwid = rpos - 2 * GAPBETWEEN;
    rwid = cp->width + GAPBETWEEN - rpos;

    r.left = GAPBETWEEN;
    r.top = cp->ypos + (height - STATICHEIGHT) / 2;
    r.right = lwid;
    r.bottom = STATICHEIGHT;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, stext, sid);

    r.left = rpos;
    r.top = cp->ypos + (height - PUSHBTNHEIGHT) / 2;
    r.right = rwid;
    r.bottom = PUSHBTNHEIGHT;
    doctl(cp, r, "BUTTON",
	  BS_NOTIFY | WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON,
	  0, btext, bid);

    cp->ypos += height + GAPBETWEEN;
}

/*
 * A simple push button.
 */
void button(struct ctlpos *cp, char *btext, int bid, int defbtn)
{
    RECT r;

    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = PUSHBTNHEIGHT;

    /* Q67655: the _dialog box_ must know which button is default
     * as well as the button itself knowing */
    if (defbtn && cp->hwnd)
	SendMessage(cp->hwnd, DM_SETDEFID, bid, 0);

    doctl(cp, r, "BUTTON",
	  BS_NOTIFY | WS_CHILD | WS_VISIBLE | WS_TABSTOP |
	  (defbtn ? BS_DEFPUSHBUTTON : 0) | BS_PUSHBUTTON,
	  0, btext, bid);

    cp->ypos += PUSHBTNHEIGHT + GAPBETWEEN;
}

/*
 * Like staticbtn, but two buttons.
 */
void static2btn(struct ctlpos *cp, char *stext, int sid,
		char *btext1, int bid1, char *btext2, int bid2)
{
    const int height = (PUSHBTNHEIGHT > STATICHEIGHT ?
			PUSHBTNHEIGHT : STATICHEIGHT);
    RECT r;
    int lwid, rwid1, rwid2, rpos1, rpos2;

    rpos1 = GAPBETWEEN + (cp->width + GAPBETWEEN) / 2;
    rpos2 = GAPBETWEEN + 3 * (cp->width + GAPBETWEEN) / 4;
    lwid = rpos1 - 2 * GAPBETWEEN;
    rwid1 = rpos2 - rpos1 - GAPBETWEEN;
    rwid2 = cp->width + GAPBETWEEN - rpos2;

    r.left = GAPBETWEEN;
    r.top = cp->ypos + (height - STATICHEIGHT) / 2;
    r.right = lwid;
    r.bottom = STATICHEIGHT;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, stext, sid);

    r.left = rpos1;
    r.top = cp->ypos + (height - PUSHBTNHEIGHT) / 2;
    r.right = rwid1;
    r.bottom = PUSHBTNHEIGHT;
    doctl(cp, r, "BUTTON",
	  BS_NOTIFY | WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON,
	  0, btext1, bid1);

    r.left = rpos2;
    r.top = cp->ypos + (height - PUSHBTNHEIGHT) / 2;
    r.right = rwid2;
    r.bottom = PUSHBTNHEIGHT;
    doctl(cp, r, "BUTTON",
	  BS_NOTIFY | WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON,
	  0, btext2, bid2);

    cp->ypos += height + GAPBETWEEN;
}

/*
 * An edit control on the right hand side, with a static to its left.
 */
static void staticedit_internal(struct ctlpos *cp, char *stext,
				int sid, int eid, int percentedit,
				int style)
{
    const int height = (EDITHEIGHT > STATICHEIGHT ?
			EDITHEIGHT : STATICHEIGHT);
    RECT r;
    int lwid, rwid, rpos;

    rpos =
	GAPBETWEEN + (100 - percentedit) * (cp->width + GAPBETWEEN) / 100;
    lwid = rpos - 2 * GAPBETWEEN;
    rwid = cp->width + GAPBETWEEN - rpos;

    r.left = GAPBETWEEN;
    r.top = cp->ypos + (height - STATICHEIGHT) / 2;
    r.right = lwid;
    r.bottom = STATICHEIGHT;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, stext, sid);

    r.left = rpos;
    r.top = cp->ypos + (height - EDITHEIGHT) / 2;
    r.right = rwid;
    r.bottom = EDITHEIGHT;
    doctl(cp, r, "EDIT",
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL | style,
	  WS_EX_CLIENTEDGE, "", eid);

    cp->ypos += height + GAPBETWEEN;
}

void staticedit(struct ctlpos *cp, char *stext,
		int sid, int eid, int percentedit)
{
    staticedit_internal(cp, stext, sid, eid, percentedit, 0);
}

void staticpassedit(struct ctlpos *cp, char *stext,
		    int sid, int eid, int percentedit)
{
    staticedit_internal(cp, stext, sid, eid, percentedit, ES_PASSWORD);
}

/*
 * A drop-down list box on the right hand side, with a static to
 * its left.
 */
void staticddl(struct ctlpos *cp, char *stext,
	       int sid, int lid, int percentlist)
{
    const int height = (COMBOHEIGHT > STATICHEIGHT ?
			COMBOHEIGHT : STATICHEIGHT);
    RECT r;
    int lwid, rwid, rpos;

    rpos =
	GAPBETWEEN + (100 - percentlist) * (cp->width + GAPBETWEEN) / 100;
    lwid = rpos - 2 * GAPBETWEEN;
    rwid = cp->width + GAPBETWEEN - rpos;

    r.left = GAPBETWEEN;
    r.top = cp->ypos + (height - STATICHEIGHT) / 2;
    r.right = lwid;
    r.bottom = STATICHEIGHT;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, stext, sid);

    r.left = rpos;
    r.top = cp->ypos + (height - EDITHEIGHT) / 2;
    r.right = rwid;
    r.bottom = COMBOHEIGHT*4;
    doctl(cp, r, "COMBOBOX",
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP |
	  CBS_DROPDOWNLIST | CBS_HASSTRINGS, WS_EX_CLIENTEDGE, "", lid);

    cp->ypos += height + GAPBETWEEN;
}

/*
 * A combo box on the right hand side, with a static to its left.
 */
void staticcombo(struct ctlpos *cp, char *stext,
		 int sid, int lid, int percentlist)
{
    const int height = (COMBOHEIGHT > STATICHEIGHT ?
			COMBOHEIGHT : STATICHEIGHT);
    RECT r;
    int lwid, rwid, rpos;

    rpos =
	GAPBETWEEN + (100 - percentlist) * (cp->width + GAPBETWEEN) / 100;
    lwid = rpos - 2 * GAPBETWEEN;
    rwid = cp->width + GAPBETWEEN - rpos;

    r.left = GAPBETWEEN;
    r.top = cp->ypos + (height - STATICHEIGHT) / 2;
    r.right = lwid;
    r.bottom = STATICHEIGHT;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, stext, sid);

    r.left = rpos;
    r.top = cp->ypos + (height - EDITHEIGHT) / 2;
    r.right = rwid;
    r.bottom = COMBOHEIGHT*10;
    doctl(cp, r, "COMBOBOX",
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL |
	  CBS_DROPDOWN | CBS_HASSTRINGS, WS_EX_CLIENTEDGE, "", lid);

    cp->ypos += height + GAPBETWEEN;
}

/*
 * A static, with a full-width drop-down list box below it.
 */
void staticddlbig(struct ctlpos *cp, char *stext,
		  int sid, int lid)
{
    RECT r;

    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = STATICHEIGHT;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, stext, sid);
    cp->ypos += STATICHEIGHT;

    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = COMBOHEIGHT*4;
    doctl(cp, r, "COMBOBOX",
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP |
	  CBS_DROPDOWNLIST | CBS_HASSTRINGS, WS_EX_CLIENTEDGE, "", lid);
    cp->ypos += COMBOHEIGHT + GAPBETWEEN;
}

/*
 * A big multiline edit control with a static labelling it.
 */
void bigeditctrl(struct ctlpos *cp, char *stext,
		 int sid, int eid, int lines)
{
    RECT r;

    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = STATICHEIGHT;
    cp->ypos += r.bottom + GAPWITHIN;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, stext, sid);

    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = EDITHEIGHT + (lines - 1) * STATICHEIGHT;
    cp->ypos += r.bottom + GAPBETWEEN;
    doctl(cp, r, "EDIT",
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL | ES_MULTILINE,
	  WS_EX_CLIENTEDGE, "", eid);
}

/*
 * A list box with a static labelling it.
 */
void listbox(struct ctlpos *cp, char *stext,
	     int sid, int lid, int lines, int multi)
{
    RECT r;

    if (stext != NULL) {
	r.left = GAPBETWEEN;
	r.top = cp->ypos;
	r.right = cp->width;
	r.bottom = STATICHEIGHT;
	cp->ypos += r.bottom + GAPWITHIN;
	doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, stext, sid);
    }

    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = LISTHEIGHT + (lines - 1) * LISTINCREMENT;
    cp->ypos += r.bottom + GAPBETWEEN;
    doctl(cp, r, "LISTBOX",
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL |
	  LBS_NOTIFY | LBS_HASSTRINGS | LBS_USETABSTOPS |
	  (multi ? LBS_MULTIPLESEL : 0),
	  WS_EX_CLIENTEDGE, "", lid);
}

/*
 * A tab-control substitute when a real tab control is unavailable.
 */
void ersatztab(struct ctlpos *cp, char *stext, int sid, int lid, int s2id)
{
    const int height = (COMBOHEIGHT > STATICHEIGHT ?
			COMBOHEIGHT : STATICHEIGHT);
    RECT r;
    int bigwid, lwid, rwid, rpos;
    static const int BIGGAP = 15;
    static const int MEDGAP = 3;

    bigwid = cp->width + 2 * GAPBETWEEN - 2 * BIGGAP;
    cp->ypos += MEDGAP;
    rpos = BIGGAP + (bigwid + BIGGAP) / 2;
    lwid = rpos - 2 * BIGGAP;
    rwid = bigwid + BIGGAP - rpos;

    r.left = BIGGAP;
    r.top = cp->ypos + (height - STATICHEIGHT) / 2;
    r.right = lwid;
    r.bottom = STATICHEIGHT;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, stext, sid);

    r.left = rpos;
    r.top = cp->ypos + (height - COMBOHEIGHT) / 2;
    r.right = rwid;
    r.bottom = COMBOHEIGHT * 10;
    doctl(cp, r, "COMBOBOX",
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP |
	  CBS_DROPDOWNLIST | CBS_HASSTRINGS, WS_EX_CLIENTEDGE, "", lid);

    cp->ypos += height + MEDGAP + GAPBETWEEN;

    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = 2;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE | SS_ETCHEDHORZ,
	  0, "", s2id);
}

/*
 * A static line, followed by an edit control on the left hand side
 * and a button on the right.
 */
void editbutton(struct ctlpos *cp, char *stext, int sid,
		int eid, char *btext, int bid)
{
    const int height = (EDITHEIGHT > PUSHBTNHEIGHT ?
			EDITHEIGHT : PUSHBTNHEIGHT);
    RECT r;
    int lwid, rwid, rpos;

    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = STATICHEIGHT;
    cp->ypos += r.bottom + GAPWITHIN;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, stext, sid);

    rpos = GAPBETWEEN + 3 * (cp->width + GAPBETWEEN) / 4;
    lwid = rpos - 2 * GAPBETWEEN;
    rwid = cp->width + GAPBETWEEN - rpos;

    r.left = GAPBETWEEN;
    r.top = cp->ypos + (height - EDITHEIGHT) / 2;
    r.right = lwid;
    r.bottom = EDITHEIGHT;
    doctl(cp, r, "EDIT",
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL,
	  WS_EX_CLIENTEDGE, "", eid);

    r.left = rpos;
    r.top = cp->ypos + (height - PUSHBTNHEIGHT) / 2;
    r.right = rwid;
    r.bottom = PUSHBTNHEIGHT;
    doctl(cp, r, "BUTTON",
	  BS_NOTIFY | WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON,
	  0, btext, bid);

    cp->ypos += height + GAPBETWEEN;
}

/*
 * A special control for manipulating an ordered preference list
 * (eg. for cipher selection).
 * XXX: this is a rough hack and could be improved.
 */
void prefslist(struct prefslist *hdl, struct ctlpos *cp, int lines,
	       char *stext, int sid, int listid, int upbid, int dnbid)
{
    const static int percents[] = { 5, 75, 20 };
    RECT r;
    int xpos, percent = 0, i;
    int listheight = LISTHEIGHT + (lines - 1) * LISTINCREMENT;
    const int BTNSHEIGHT = 2*PUSHBTNHEIGHT + GAPBETWEEN;
    int totalheight, buttonpos;

    /* Squirrel away IDs. */
    hdl->listid = listid;
    hdl->upbid  = upbid;
    hdl->dnbid  = dnbid;

    /* The static label. */
    if (stext != NULL) {
	r.left = GAPBETWEEN;
	r.top = cp->ypos;
	r.right = cp->width;
	r.bottom = STATICHEIGHT;
	cp->ypos += r.bottom + GAPWITHIN;
	doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, stext, sid);
    }

    if (listheight > BTNSHEIGHT) {
        totalheight = listheight;
	buttonpos = (listheight - BTNSHEIGHT) / 2;
    } else {
        totalheight = BTNSHEIGHT;
	buttonpos = 0;
    }

    for (i=0; i<3; i++) {
        int left, wid;
        xpos = (cp->width + GAPBETWEEN) * percent / 100;
        left = xpos + GAPBETWEEN;
        percent += percents[i];
        xpos = (cp->width + GAPBETWEEN) * percent / 100;
        wid = xpos - left;

        switch (i) {
          case 1:
            /* The drag list box. */
            r.left = left; r.right = wid;
            r.top = cp->ypos; r.bottom = listheight;
            {
                HWND ctl;
                ctl = doctl(cp, r, "LISTBOX",
                            WS_CHILD | WS_VISIBLE | WS_TABSTOP |
			    WS_VSCROLL | LBS_HASSTRINGS | LBS_USETABSTOPS,
                            WS_EX_CLIENTEDGE,
                            "", listid);
		MakeDragList(ctl);
            }
            break;

          case 2:
            /* The "Up" and "Down" buttons. */
	    /* XXX worry about accelerators if we have more than one
	     * prefslist on a panel */
            r.left = left; r.right = wid;
            r.top = cp->ypos + buttonpos; r.bottom = PUSHBTNHEIGHT;
            doctl(cp, r, "BUTTON",
                  BS_NOTIFY | WS_CHILD | WS_VISIBLE |
		  WS_TABSTOP | BS_PUSHBUTTON,
                  0, "&Up", upbid);

            r.left = left; r.right = wid;
            r.top = cp->ypos + buttonpos + PUSHBTNHEIGHT + GAPBETWEEN;
            r.bottom = PUSHBTNHEIGHT;
            doctl(cp, r, "BUTTON",
                  BS_NOTIFY | WS_CHILD | WS_VISIBLE |
		  WS_TABSTOP | BS_PUSHBUTTON,
                  0, "&Down", dnbid);

            break;

        }
    }

    cp->ypos += totalheight + GAPBETWEEN;

}

/*
 * Helper function for prefslist: move item in list box.
 */
static void pl_moveitem(HWND hwnd, int listid, int src, int dst)
{
    int tlen, val;
    char *txt;
    /* Get the item's data. */
    tlen = SendDlgItemMessage (hwnd, listid, LB_GETTEXTLEN, src, 0);
    txt = snewn(tlen+1, char);
    SendDlgItemMessage (hwnd, listid, LB_GETTEXT, src, (LPARAM) txt);
    val = SendDlgItemMessage (hwnd, listid, LB_GETITEMDATA, src, 0);
    /* Deselect old location. */
    SendDlgItemMessage (hwnd, listid, LB_SETSEL, FALSE, src);
    /* Delete it at the old location. */
    SendDlgItemMessage (hwnd, listid, LB_DELETESTRING, src, 0);
    /* Insert it at new location. */
    SendDlgItemMessage (hwnd, listid, LB_INSERTSTRING, dst,
			(LPARAM) txt);
    SendDlgItemMessage (hwnd, listid, LB_SETITEMDATA, dst,
			(LPARAM) val);
    /* Set selection. */
    SendDlgItemMessage (hwnd, listid, LB_SETCURSEL, dst, 0);
    sfree (txt);
}

int pl_itemfrompt(HWND hwnd, POINT cursor, BOOL scroll)
{
    int ret;
    POINT uppoint, downpoint;
    int updist, downdist, upitem, downitem, i;

    /*
     * Ghastly hackery to try to figure out not which
     * _item_, but which _gap between items_, the user
     * is pointing at. We do this by first working out
     * which list item is under the cursor, and then
     * working out how far the cursor would have to
     * move up or down before the answer was different.
     * Then we put the insertion point _above_ the
     * current item if the upper edge is closer than
     * the lower edge, or _below_ it if vice versa.
     */
    ret = LBItemFromPt(hwnd, cursor, scroll);
    if (ret == -1)
	return ret;
    ret = LBItemFromPt(hwnd, cursor, FALSE);
    updist = downdist = 0;
    for (i = 1; i < 4096 && (!updist || !downdist); i++) {
	uppoint = downpoint = cursor;
	uppoint.y -= i;
	downpoint.y += i;
	upitem = LBItemFromPt(hwnd, uppoint, FALSE);
	downitem = LBItemFromPt(hwnd, downpoint, FALSE);
	if (!updist && upitem != ret)
	    updist = i;
	if (!downdist && downitem != ret)
	    downdist = i;
    }
    if (downdist < updist)
	ret++;
    return ret;
}

/*
 * Handler for prefslist above.
 * 
 * Return value has bit 0 set if the dialog box procedure needs to
 * return TRUE from handling this message; it has bit 1 set if a
 * change may have been made in the contents of the list.
 */
int handle_prefslist(struct prefslist *hdl,
                     int *array, int maxmemb,
                     int is_dlmsg, HWND hwnd,
		     WPARAM wParam, LPARAM lParam)
{
    int i;
    int ret = 0;

    if (is_dlmsg) {

        if ((int)wParam == hdl->listid) {
            DRAGLISTINFO *dlm = (DRAGLISTINFO *)lParam;
            int dest = 0;	       /* initialise to placate gcc */
            switch (dlm->uNotification) {
              case DL_BEGINDRAG:
		hdl->dummyitem =
		    SendDlgItemMessage(hwnd, hdl->listid,
				       LB_ADDSTRING, 0, (LPARAM) "");

                hdl->srcitem = LBItemFromPt(dlm->hWnd, dlm->ptCursor, TRUE);
		hdl->dragging = 0;
		/* XXX hack Q183115 */
		SetWindowLong(hwnd, DWL_MSGRESULT, TRUE);
                ret |= 1; break;
              case DL_CANCELDRAG:
		DrawInsert(hwnd, dlm->hWnd, -1);     /* Clear arrow */
		SendDlgItemMessage(hwnd, hdl->listid,
				   LB_DELETESTRING, hdl->dummyitem, 0);
		hdl->dragging = 0;
                ret |= 1; break;
              case DL_DRAGGING:
		hdl->dragging = 1;
		dest = pl_itemfrompt(dlm->hWnd, dlm->ptCursor, TRUE);
		if (dest > hdl->dummyitem) dest = hdl->dummyitem;
		DrawInsert (hwnd, dlm->hWnd, dest);
		if (dest >= 0)
		    SetWindowLong(hwnd, DWL_MSGRESULT, DL_MOVECURSOR);
		else
		    SetWindowLong(hwnd, DWL_MSGRESULT, DL_STOPCURSOR);
                ret |= 1; break;
              case DL_DROPPED:
		if (hdl->dragging) {
		    dest = pl_itemfrompt(dlm->hWnd, dlm->ptCursor, TRUE);
		    if (dest > hdl->dummyitem) dest = hdl->dummyitem;
		    DrawInsert (hwnd, dlm->hWnd, -1);
		}
		SendDlgItemMessage(hwnd, hdl->listid,
				   LB_DELETESTRING, hdl->dummyitem, 0);
		if (hdl->dragging) {
		    hdl->dragging = 0;
		    if (dest >= 0) {
			/* Correct for "missing" item. */
			if (dest > hdl->srcitem) dest--;
			pl_moveitem(hwnd, hdl->listid, hdl->srcitem, dest);
		    }
		    ret |= 2;
		}
                ret |= 1; break;
            }
        }

    } else {

        if (((LOWORD(wParam) == hdl->upbid) ||
             (LOWORD(wParam) == hdl->dnbid)) &&
            ((HIWORD(wParam) == BN_CLICKED) ||
             (HIWORD(wParam) == BN_DOUBLECLICKED))) {
            /* Move an item up or down the list. */
            /* Get the current selection, if any. */
            int selection = SendDlgItemMessage (hwnd, hdl->listid, LB_GETCURSEL, 0, 0);
            if (selection == LB_ERR) {
                MessageBeep(0);
            } else {
                int nitems;
                /* Get the total number of items. */
                nitems = SendDlgItemMessage (hwnd, hdl->listid, LB_GETCOUNT, 0, 0);
                /* Should we do anything? */
		if (LOWORD(wParam) == hdl->upbid && (selection > 0))
		    pl_moveitem(hwnd, hdl->listid, selection, selection - 1);
		else if (LOWORD(wParam) == hdl->dnbid && (selection < nitems - 1))
		    pl_moveitem(hwnd, hdl->listid, selection, selection + 1);
		ret |= 2;
            }

        }

    }

    if (array) {
	/* Update array to match the list box. */
	for (i=0; i < maxmemb; i++)
	    array[i] = SendDlgItemMessage (hwnd, hdl->listid, LB_GETITEMDATA,
					   i, 0);
    }

    return ret;
}

/*
 * A progress bar (from Common Controls). We like our progress bars
 * to be smooth and unbroken, without those ugly divisions; some
 * older compilers may not support that, but that's life.
 */
void progressbar(struct ctlpos *cp, int id)
{
    RECT r;

    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = PROGBARHEIGHT;
    cp->ypos += r.bottom + GAPBETWEEN;

    doctl(cp, r, PROGRESS_CLASS, WS_CHILD | WS_VISIBLE
#ifdef PBS_SMOOTH
	  | PBS_SMOOTH
#endif
	  , WS_EX_CLIENTEDGE, "", id);
}

/* ----------------------------------------------------------------------
 * Platform-specific side of portable dialog-box mechanism.
 */

/*
 * This function takes a string, escapes all the ampersands, and
 * places a single (unescaped) ampersand in front of the first
 * occurrence of the given shortcut character (which may be
 * NO_SHORTCUT).
 * 
 * Return value is a malloc'ed copy of the processed version of the
 * string.
 */
static char *shortcut_escape(char *text, char shortcut)
{
    char *ret;
    char *p, *q;

    if (!text)
	return NULL;		       /* sfree won't choke on this */

    ret = snewn(2*strlen(text)+1, char);   /* size potentially doubles! */
    shortcut = tolower((unsigned char)shortcut);

    p = text;
    q = ret;
    while (*p) {
	if (shortcut != NO_SHORTCUT &&
	    tolower((unsigned char)*p) == shortcut) {
	    *q++ = '&';
	    shortcut = NO_SHORTCUT;    /* stop it happening twice */
	} else if (*p == '&') {
	    *q++ = '&';
	}
	*q++ = *p++;
    }
    *q = '\0';
    return ret;
}

void winctrl_add_shortcuts(struct dlgparam *dp, struct winctrl *c)
{
    int i;
    for (i = 0; i < lenof(c->shortcuts); i++)
	if (c->shortcuts[i] != NO_SHORTCUT) {
	    unsigned char s = tolower((unsigned char)c->shortcuts[i]);
	    assert(!dp->shortcuts[s]);
	    dp->shortcuts[s] = TRUE;
	}
}

void winctrl_rem_shortcuts(struct dlgparam *dp, struct winctrl *c)
{
    int i;
    for (i = 0; i < lenof(c->shortcuts); i++)
	if (c->shortcuts[i] != NO_SHORTCUT) {
	    unsigned char s = tolower((unsigned char)c->shortcuts[i]);
	    assert(dp->shortcuts[s]);
	    dp->shortcuts[s] = FALSE;
	}
}

static int winctrl_cmp_byctrl(void *av, void *bv)
{
    struct winctrl *a = (struct winctrl *)av;
    struct winctrl *b = (struct winctrl *)bv;
    if (a->ctrl < b->ctrl)
	return -1;
    else if (a->ctrl > b->ctrl)
	return +1;
    else
	return 0;
}
static int winctrl_cmp_byid(void *av, void *bv)
{
    struct winctrl *a = (struct winctrl *)av;
    struct winctrl *b = (struct winctrl *)bv;
    if (a->base_id < b->base_id)
	return -1;
    else if (a->base_id > b->base_id)
	return +1;
    else
	return 0;
}
static int winctrl_cmp_byctrl_find(void *av, void *bv)
{
    union control *a = (union control *)av;
    struct winctrl *b = (struct winctrl *)bv;
    if (a < b->ctrl)
	return -1;
    else if (a > b->ctrl)
	return +1;
    else
	return 0;
}
static int winctrl_cmp_byid_find(void *av, void *bv)
{
    int *a = (int *)av;
    struct winctrl *b = (struct winctrl *)bv;
    if (*a < b->base_id)
	return -1;
    else if (*a >= b->base_id + b->num_ids)
	return +1;
    else
	return 0;
}

void winctrl_init(struct winctrls *wc)
{
    wc->byctrl = newtree234(winctrl_cmp_byctrl);
    wc->byid = newtree234(winctrl_cmp_byid);
}
void winctrl_cleanup(struct winctrls *wc)
{
    struct winctrl *c;

    while ((c = index234(wc->byid, 0)) != NULL) {
	winctrl_remove(wc, c);
	sfree(c->data);
	sfree(c);
    }

    freetree234(wc->byctrl);
    freetree234(wc->byid);
    wc->byctrl = wc->byid = NULL;
}

void winctrl_add(struct winctrls *wc, struct winctrl *c)
{
    struct winctrl *ret;
    if (c->ctrl) {
	ret = add234(wc->byctrl, c);
	assert(ret == c);
    }
    ret = add234(wc->byid, c);
    assert(ret == c);
}

void winctrl_remove(struct winctrls *wc, struct winctrl *c)
{
    struct winctrl *ret;
    ret = del234(wc->byctrl, c);
    ret = del234(wc->byid, c);
    assert(ret == c);
}

struct winctrl *winctrl_findbyctrl(struct winctrls *wc, union control *ctrl)
{
    return find234(wc->byctrl, ctrl, winctrl_cmp_byctrl_find);
}

struct winctrl *winctrl_findbyid(struct winctrls *wc, int id)
{
    return find234(wc->byid, &id, winctrl_cmp_byid_find);
}

struct winctrl *winctrl_findbyindex(struct winctrls *wc, int index)
{
    return index234(wc->byid, index);
}

void winctrl_layout(struct dlgparam *dp, struct winctrls *wc,
		    struct ctlpos *cp, struct controlset *s, int *id)
{
    struct ctlpos columns[16];
    int ncols, colstart, colspan;

    struct ctlpos tabdelays[16];
    union control *tabdelayed[16];
    int ntabdelays;

    struct ctlpos pos;

    char shortcuts[MAX_SHORTCUTS_PER_CTRL];
    int nshortcuts;
    char *escaped;
    int i, actual_base_id, base_id, num_ids;
    void *data;

    base_id = *id;

    /* Start a containing box, if we have a boxname. */
    if (s->boxname && *s->boxname) {
	struct winctrl *c = snew(struct winctrl);
    	c->ctrl = NULL;
	c->base_id = base_id;
	c->num_ids = 1;
	c->data = NULL;
	memset(c->shortcuts, NO_SHORTCUT, lenof(c->shortcuts));
	winctrl_add(wc, c);
	beginbox(cp, s->boxtitle, base_id);
	base_id++;
    }

    /* Draw a title, if we have one. */
    if (!s->boxname && s->boxtitle) {
	struct winctrl *c = snew(struct winctrl);
    	c->ctrl = NULL;
	c->base_id = base_id;
	c->num_ids = 1;
	c->data = dupstr(s->boxtitle);
	memset(c->shortcuts, NO_SHORTCUT, lenof(c->shortcuts));
	winctrl_add(wc, c);
	paneltitle(cp, base_id);
	base_id++;
    }

    /* Initially we have just one column. */
    ncols = 1;
    columns[0] = *cp;		       /* structure copy */

    /* And initially, there are no pending tab-delayed controls. */
    ntabdelays = 0;

    /* Loop over each control in the controlset. */
    for (i = 0; i < s->ncontrols; i++) {
	union control *ctrl = s->ctrls[i];

	/*
	 * Generic processing that pertains to all control types.
	 * At the end of this if statement, we'll have produced
	 * `ctrl' (a pointer to the control we have to create, or
	 * think about creating, in this iteration of the loop),
	 * `pos' (a suitable ctlpos with which to position it), and
	 * `c' (a winctrl structure to receive details of the
	 * dialog IDs). Or we'll have done a `continue', if it was
	 * CTRL_COLUMNS and doesn't require any control creation at
	 * all.
	 */
	if (ctrl->generic.type == CTRL_COLUMNS) {
	    assert((ctrl->columns.ncols == 1) ^ (ncols == 1));

	    if (ncols == 1) {
		/*
		 * We're splitting into multiple columns.
		 */
		int lpercent, rpercent, lx, rx, i;

		ncols = ctrl->columns.ncols;
		assert(ncols <= lenof(columns));
		for (i = 1; i < ncols; i++)
		    columns[i] = columns[0];   /* structure copy */

		lpercent = 0;
		for (i = 0; i < ncols; i++) {
		    rpercent = lpercent + ctrl->columns.percentages[i];
		    lx = columns[i].xoff + lpercent *
			(columns[i].width + GAPBETWEEN) / 100;
		    rx = columns[i].xoff + rpercent *
			(columns[i].width + GAPBETWEEN) / 100;
		    columns[i].xoff = lx;
		    columns[i].width = rx - lx - GAPBETWEEN;
		    lpercent = rpercent;
		}
	    } else {
		/*
		 * We're recombining the various columns into one.
		 */
		int maxy = columns[0].ypos;
		int i;
		for (i = 1; i < ncols; i++)
		    if (maxy < columns[i].ypos)
			maxy = columns[i].ypos;
		ncols = 1;
		columns[0] = *cp;      /* structure copy */
		columns[0].ypos = maxy;
	    }

	    continue;
	} else if (ctrl->generic.type == CTRL_TABDELAY) {
	    int i;

	    assert(!ctrl->generic.tabdelay);
	    ctrl = ctrl->tabdelay.ctrl;

	    for (i = 0; i < ntabdelays; i++)
		if (tabdelayed[i] == ctrl)
		    break;
	    assert(i < ntabdelays);    /* we have to have found it */

	    pos = tabdelays[i];	       /* structure copy */

	    colstart = colspan = -1;   /* indicate this was tab-delayed */

	} else {
	    /*
	     * If it wasn't one of those, it's a genuine control;
	     * so we'll have to compute a position for it now, by
	     * checking its column span.
	     */
	    int col;

	    colstart = COLUMN_START(ctrl->generic.column);
	    colspan = COLUMN_SPAN(ctrl->generic.column);

	    pos = columns[colstart];   /* structure copy */
	    pos.width = columns[colstart+colspan-1].width +
		(columns[colstart+colspan-1].xoff - columns[colstart].xoff);

	    for (col = colstart; col < colstart+colspan; col++)
		if (pos.ypos < columns[col].ypos)
		    pos.ypos = columns[col].ypos;

	    /*
	     * If this control is to be tabdelayed, add it to the
	     * tabdelay list, and unset pos.hwnd to inhibit actual
	     * control creation.
	     */
	    if (ctrl->generic.tabdelay) {
		assert(ntabdelays < lenof(tabdelays));
		tabdelays[ntabdelays] = pos;   /* structure copy */
		tabdelayed[ntabdelays] = ctrl;
		ntabdelays++;
		pos.hwnd = NULL;
	    }
	}

	/* Most controls don't need anything in c->data. */
	data = NULL;

	/* And they all start off with no shortcuts registered. */
	memset(shortcuts, NO_SHORTCUT, lenof(shortcuts));
	nshortcuts = 0;

	/* Almost all controls start at base_id. */
	actual_base_id = base_id;

	/*
	 * Now we're ready to actually create the control, by
	 * switching on its type.
	 */
	switch (ctrl->generic.type) {
	  case CTRL_TEXT:
	    {
		char *wrapped, *escaped;
		int lines;
		num_ids = 1;
		wrapped = staticwrap(&pos, cp->hwnd,
				     ctrl->generic.label, &lines);
		escaped = shortcut_escape(wrapped, NO_SHORTCUT);
		statictext(&pos, escaped, lines, base_id);
		sfree(escaped);
		sfree(wrapped);
	    }
	    break;
	  case CTRL_EDITBOX:
	    num_ids = 2;	       /* static, edit */
	    escaped = shortcut_escape(ctrl->editbox.label,
				      ctrl->editbox.shortcut);
	    shortcuts[nshortcuts++] = ctrl->editbox.shortcut;
	    if (ctrl->editbox.percentwidth == 100) {
		if (ctrl->editbox.has_list)
		    combobox(&pos, escaped,
			     base_id, base_id+1);
		else
		    multiedit(&pos, ctrl->editbox.password, escaped,
			      base_id, base_id+1, 100, NULL);
	    } else {
		if (ctrl->editbox.has_list) {
		    staticcombo(&pos, escaped, base_id, base_id+1,
				ctrl->editbox.percentwidth);
		} else {
		    (ctrl->editbox.password ? staticpassedit : staticedit)
			(&pos, escaped, base_id, base_id+1,
			 ctrl->editbox.percentwidth);
		}
	    }
	    sfree(escaped);
	    break;
	  case CTRL_RADIO:
	    num_ids = ctrl->radio.nbuttons + 1;   /* label as well */
	    {
		struct radio *buttons;
		int i;

		escaped = shortcut_escape(ctrl->radio.label,
					  ctrl->radio.shortcut);
		shortcuts[nshortcuts++] = ctrl->radio.shortcut;

		buttons = snewn(ctrl->radio.nbuttons, struct radio);

		for (i = 0; i < ctrl->radio.nbuttons; i++) {
		    buttons[i].text =
			shortcut_escape(ctrl->radio.buttons[i],
					(char)(ctrl->radio.shortcuts ?
					       ctrl->radio.shortcuts[i] :
					       NO_SHORTCUT));
		    buttons[i].id = base_id + 1 + i;
		    if (ctrl->radio.shortcuts) {
			assert(nshortcuts < MAX_SHORTCUTS_PER_CTRL);
			shortcuts[nshortcuts++] = ctrl->radio.shortcuts[i];
		    }
		}

		radioline_common(&pos, escaped, base_id,
				 ctrl->radio.ncolumns,
				 buttons, ctrl->radio.nbuttons);

		for (i = 0; i < ctrl->radio.nbuttons; i++) {
		    sfree(buttons[i].text);
		}
		sfree(buttons);
		sfree(escaped);
	    }
	    break;
	  case CTRL_CHECKBOX:
	    num_ids = 1;
	    escaped = shortcut_escape(ctrl->checkbox.label,
				      ctrl->checkbox.shortcut);
	    shortcuts[nshortcuts++] = ctrl->checkbox.shortcut;
	    checkbox(&pos, escaped, base_id);
	    sfree(escaped);
	    break;
	  case CTRL_BUTTON:
	    escaped = shortcut_escape(ctrl->button.label,
				      ctrl->button.shortcut);
	    shortcuts[nshortcuts++] = ctrl->button.shortcut;
	    if (ctrl->button.iscancel)
		actual_base_id = IDCANCEL;
	    num_ids = 1;
	    button(&pos, escaped, actual_base_id, ctrl->button.isdefault);
	    sfree(escaped);
	    break;
	  case CTRL_LISTBOX:
	    num_ids = 2;
	    escaped = shortcut_escape(ctrl->listbox.label,
				      ctrl->listbox.shortcut);
	    shortcuts[nshortcuts++] = ctrl->listbox.shortcut;
	    if (ctrl->listbox.draglist) {
		data = snew(struct prefslist);
		num_ids = 4;
		prefslist(data, &pos, ctrl->listbox.height, escaped,
			  base_id, base_id+1, base_id+2, base_id+3);
		shortcuts[nshortcuts++] = 'u';   /* Up */
		shortcuts[nshortcuts++] = 'd';   /* Down */
	    } else if (ctrl->listbox.height == 0) {
		/* Drop-down list. */
		if (ctrl->listbox.percentwidth == 100) {
		    staticddlbig(&pos, escaped,
				 base_id, base_id+1);
		} else {
		    staticddl(&pos, escaped, base_id,
			      base_id+1, ctrl->listbox.percentwidth);
		}
	    } else {
		/* Ordinary list. */
		listbox(&pos, escaped, base_id, base_id+1,
			ctrl->listbox.height, ctrl->listbox.multisel);
	    }
	    if (ctrl->listbox.ncols) {
		/*
		 * This method of getting the box width is a bit of
		 * a hack; we'd do better to try to retrieve the
		 * actual width in dialog units from doctl() just
		 * before MapDialogRect. But that's going to be no
		 * fun, and this should be good enough accuracy.
		 */
		int width = cp->width * ctrl->listbox.percentwidth;
		int *tabarray;
		int i, percent;

		tabarray = snewn(ctrl->listbox.ncols-1, int);
		percent = 0;
		for (i = 0; i < ctrl->listbox.ncols-1; i++) {
		    percent += ctrl->listbox.percentages[i];
		    tabarray[i] = width * percent / 10000;
		}
		SendDlgItemMessage(cp->hwnd, base_id+1, LB_SETTABSTOPS,
				   ctrl->listbox.ncols-1, (LPARAM)tabarray);
		sfree(tabarray);
	    }
	    sfree(escaped);
	    break;
	  case CTRL_FILESELECT:
	    num_ids = 3;
	    escaped = shortcut_escape(ctrl->fileselect.label,
				      ctrl->fileselect.shortcut);
	    shortcuts[nshortcuts++] = ctrl->fileselect.shortcut;
	    editbutton(&pos, escaped, base_id, base_id+1,
		       "Bro&wse...", base_id+2);
	    shortcuts[nshortcuts++] = 'w';
	    sfree(escaped);
	    break;
	  case CTRL_FONTSELECT:
	    num_ids = 3;
	    escaped = shortcut_escape(ctrl->fontselect.label,
				      ctrl->fontselect.shortcut);
	    shortcuts[nshortcuts++] = ctrl->fontselect.shortcut;
	    statictext(&pos, escaped, 1, base_id);
	    staticbtn(&pos, "", base_id+1, "Change...", base_id+2);
	    sfree(escaped);
	    data = snew(FontSpec);
	    break;
	  default:
	    assert(!"Can't happen");
	    num_ids = 0;	       /* placate gcc */
	    break;
	}

	/*
	 * Create a `struct winctrl' for this control, and advance
	 * the dialog ID counter, if it's actually been created
	 * (and isn't tabdelayed).
	 */
	if (pos.hwnd) {
	    struct winctrl *c = snew(struct winctrl);

	    c->ctrl = ctrl;
	    c->base_id = actual_base_id;
	    c->num_ids = num_ids;
	    c->data = data;
	    memcpy(c->shortcuts, shortcuts, sizeof(shortcuts));
	    winctrl_add(wc, c);
	    winctrl_add_shortcuts(dp, c);
	    if (actual_base_id == base_id)
		base_id += num_ids;
	}

	if (colstart >= 0) {
	    /*
	     * Update the ypos in all columns crossed by this
	     * control.
	     */
	    int i;
	    for (i = colstart; i < colstart+colspan; i++)
		columns[i].ypos = pos.ypos;
	}
    }

    /*
     * We've now finished laying out the controls; so now update
     * the ctlpos and control ID that were passed in, terminate
     * any containing box, and return.
     */
    for (i = 0; i < ncols; i++)
	if (cp->ypos < columns[i].ypos)
	    cp->ypos = columns[i].ypos;
    *id = base_id;

    if (s->boxname && *s->boxname)
	endbox(cp);
}

static void winctrl_set_focus(union control *ctrl, struct dlgparam *dp,
			      int has_focus)
{
    if (has_focus) {
	if (dp->focused)
	    dp->lastfocused = dp->focused;
	dp->focused = ctrl;
    } else if (!has_focus && dp->focused == ctrl) {
	dp->lastfocused = dp->focused;
	dp->focused = NULL;
    }
}

union control *dlg_last_focused(union control *ctrl, void *dlg)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    return dp->focused == ctrl ? dp->lastfocused : dp->focused;
}

/*
 * The dialog-box procedure calls this function to handle Windows
 * messages on a control we manage.
 */
int winctrl_handle_command(struct dlgparam *dp, UINT msg,
			   WPARAM wParam, LPARAM lParam)
{
    struct winctrl *c;
    union control *ctrl;
    int i, id, ret;
    static UINT draglistmsg = WM_NULL;

    /*
     * Filter out pointless window messages. Our interest is in
     * WM_COMMAND and the drag list message, and nothing else.
     */
    if (draglistmsg == WM_NULL)
	draglistmsg = RegisterWindowMessage (DRAGLISTMSGSTRING);

    if (msg != draglistmsg && msg != WM_COMMAND && msg != WM_DRAWITEM)
	return 0;

    /*
     * Look up the control ID in our data.
     */
    c = NULL;
    for (i = 0; i < dp->nctrltrees; i++) {
	c = winctrl_findbyid(dp->controltrees[i], LOWORD(wParam));
	if (c)
	    break;
    }
    if (!c)
	return 0;		       /* we have nothing to do */

    if (msg == WM_DRAWITEM) {
	/*
	 * Owner-draw request for a panel title.
	 */
	LPDRAWITEMSTRUCT di = (LPDRAWITEMSTRUCT) lParam;
	HDC hdc = di->hDC;
	RECT r = di->rcItem;
	SIZE s;

	SetMapMode(hdc, MM_TEXT);      /* ensure logical units == pixels */

	GetTextExtentPoint32(hdc, (char *)c->data,
				 strlen((char *)c->data), &s);
	DrawEdge(hdc, &r, EDGE_ETCHED, BF_ADJUST | BF_RECT);
	TextOut(hdc,
		r.left + (r.right-r.left-s.cx)/2,
		r.top + (r.bottom-r.top-s.cy)/2,
		(char *)c->data, strlen((char *)c->data));

	return TRUE;
    }

    ctrl = c->ctrl;
    id = LOWORD(wParam) - c->base_id;

    if (!ctrl || !ctrl->generic.handler)
	return 0;		       /* nothing we can do here */

    /*
     * From here on we do not issue `return' statements until the
     * very end of the dialog box: any event handler is entitled to
     * ask for a colour selector, so we _must_ always allow control
     * to reach the end of this switch statement so that the
     * subsequent code can test dp->coloursel_wanted().
     */
    ret = 0;
    dp->coloursel_wanted = FALSE;

    /*
     * Now switch on the control type and the message.
     */
    switch (ctrl->generic.type) {
      case CTRL_EDITBOX:
	if (msg == WM_COMMAND && !ctrl->editbox.has_list &&
	    (HIWORD(wParam) == EN_SETFOCUS || HIWORD(wParam) == EN_KILLFOCUS))
	    winctrl_set_focus(ctrl, dp, HIWORD(wParam) == EN_SETFOCUS);
	if (msg == WM_COMMAND && ctrl->editbox.has_list &&
	    (HIWORD(wParam)==CBN_SETFOCUS || HIWORD(wParam)==CBN_KILLFOCUS))
	    winctrl_set_focus(ctrl, dp, HIWORD(wParam) == CBN_SETFOCUS);

	if (msg == WM_COMMAND && !ctrl->editbox.has_list &&
	    HIWORD(wParam) == EN_CHANGE)
	    ctrl->generic.handler(ctrl, dp, dp->data, EVENT_VALCHANGE);
	if (msg == WM_COMMAND &&
	    ctrl->editbox.has_list) {
	    if (HIWORD(wParam) == CBN_SELCHANGE) {
		int index, len;
		char *text;

		index = SendDlgItemMessage(dp->hwnd, c->base_id+1,
					   CB_GETCURSEL, 0, 0);
		len = SendDlgItemMessage(dp->hwnd, c->base_id+1,
					 CB_GETLBTEXTLEN, index, 0);
		text = snewn(len+1, char);
		SendDlgItemMessage(dp->hwnd, c->base_id+1, CB_GETLBTEXT,
				   index, (LPARAM)text);
		SetDlgItemText(dp->hwnd, c->base_id+1, text);
		sfree(text);
		ctrl->generic.handler(ctrl, dp, dp->data, EVENT_VALCHANGE);
	    } else if (HIWORD(wParam) == CBN_EDITCHANGE) {
		ctrl->generic.handler(ctrl, dp, dp->data, EVENT_VALCHANGE);
	    } else if (HIWORD(wParam) == CBN_KILLFOCUS) {
		ctrl->generic.handler(ctrl, dp, dp->data, EVENT_REFRESH);
	    }

	}
	break;
      case CTRL_RADIO:
	if (msg == WM_COMMAND &&
	    (HIWORD(wParam) == BN_SETFOCUS || HIWORD(wParam) == BN_KILLFOCUS))
	    winctrl_set_focus(ctrl, dp, HIWORD(wParam) == BN_SETFOCUS);
	/*
	 * We sometimes get spurious BN_CLICKED messages for the
	 * radio button that is just about to _lose_ selection, if
	 * we're switching using the arrow keys. Therefore we
	 * double-check that the button in wParam is actually
	 * checked before generating an event.
	 */
	if (msg == WM_COMMAND &&
	    (HIWORD(wParam) == BN_CLICKED ||
	     HIWORD(wParam) == BN_DOUBLECLICKED) &&
	    IsDlgButtonChecked(dp->hwnd, LOWORD(wParam))) {
	    ctrl->generic.handler(ctrl, dp, dp->data, EVENT_VALCHANGE);
	}
	break;
      case CTRL_CHECKBOX:
	if (msg == WM_COMMAND &&
	    (HIWORD(wParam) == BN_SETFOCUS || HIWORD(wParam) == BN_KILLFOCUS))
	    winctrl_set_focus(ctrl, dp, HIWORD(wParam) == BN_SETFOCUS);
	if (msg == WM_COMMAND &&
	    (HIWORD(wParam) == BN_CLICKED ||
	     HIWORD(wParam) == BN_DOUBLECLICKED)) {
	    ctrl->generic.handler(ctrl, dp, dp->data, EVENT_VALCHANGE);
	}
	break;
      case CTRL_BUTTON:
	if (msg == WM_COMMAND &&
	    (HIWORD(wParam) == BN_SETFOCUS || HIWORD(wParam) == BN_KILLFOCUS))
	    winctrl_set_focus(ctrl, dp, HIWORD(wParam) == BN_SETFOCUS);
	if (msg == WM_COMMAND &&
	    (HIWORD(wParam) == BN_CLICKED ||
	     HIWORD(wParam) == BN_DOUBLECLICKED)) {
	    ctrl->generic.handler(ctrl, dp, dp->data, EVENT_ACTION);
	}
	break;
      case CTRL_LISTBOX:
	if (msg == WM_COMMAND && ctrl->listbox.height != 0 &&
	    (HIWORD(wParam)==LBN_SETFOCUS || HIWORD(wParam)==LBN_KILLFOCUS))
	    winctrl_set_focus(ctrl, dp, HIWORD(wParam) == LBN_SETFOCUS);
	if (msg == WM_COMMAND && ctrl->listbox.height == 0 &&
	    (HIWORD(wParam)==CBN_SETFOCUS || HIWORD(wParam)==CBN_KILLFOCUS))
	    winctrl_set_focus(ctrl, dp, HIWORD(wParam) == CBN_SETFOCUS);
	if (msg == WM_COMMAND && id >= 2 &&
	    (HIWORD(wParam) == BN_SETFOCUS || HIWORD(wParam) == BN_KILLFOCUS))
	    winctrl_set_focus(ctrl, dp, HIWORD(wParam) == BN_SETFOCUS);
	if (ctrl->listbox.draglist) {
	    int pret;
	    pret = handle_prefslist(c->data, NULL, 0, (msg != WM_COMMAND),
				    dp->hwnd, wParam, lParam);
	    if (pret & 2)
		ctrl->generic.handler(ctrl, dp, dp->data, EVENT_VALCHANGE);
	    ret = pret & 1;
	} else {
	    if (msg == WM_COMMAND && HIWORD(wParam) == LBN_DBLCLK) {
		SetCapture(dp->hwnd);
		ctrl->generic.handler(ctrl, dp, dp->data, EVENT_ACTION);
	    } else if (msg == WM_COMMAND && HIWORD(wParam) == LBN_SELCHANGE) {
		ctrl->generic.handler(ctrl, dp, dp->data, EVENT_SELCHANGE);
	    }
	}
	break;
      case CTRL_FILESELECT:
	if (msg == WM_COMMAND && id == 1 &&
	    (HIWORD(wParam) == EN_SETFOCUS || HIWORD(wParam) == EN_KILLFOCUS))
	    winctrl_set_focus(ctrl, dp, HIWORD(wParam) == EN_SETFOCUS);
	if (msg == WM_COMMAND && id == 2 &&
	    (HIWORD(wParam) == BN_SETFOCUS || HIWORD(wParam) == BN_KILLFOCUS))
	    winctrl_set_focus(ctrl, dp, HIWORD(wParam) == BN_SETFOCUS);
	if (msg == WM_COMMAND && id == 1 && HIWORD(wParam) == EN_CHANGE)
	    ctrl->generic.handler(ctrl, dp, dp->data, EVENT_VALCHANGE);
	if (id == 2 &&
	    (msg == WM_COMMAND &&
	     (HIWORD(wParam) == BN_CLICKED ||
	      HIWORD(wParam) == BN_DOUBLECLICKED))) {
	    OPENFILENAME of;
	    char filename[FILENAME_MAX];
	    int ret;

	    memset(&of, 0, sizeof(of));
#ifdef OPENFILENAME_SIZE_VERSION_400
	    of.lStructSize = OPENFILENAME_SIZE_VERSION_400;
#else
	    of.lStructSize = sizeof(of);
#endif
	    of.hwndOwner = dp->hwnd;
	    if (ctrl->fileselect.filter)
		of.lpstrFilter = ctrl->fileselect.filter;
	    else
		of.lpstrFilter = "All Files (*.*)\0*\0\0\0";
	    of.lpstrCustomFilter = NULL;
	    of.nFilterIndex = 1;
	    of.lpstrFile = filename;
	    GetDlgItemText(dp->hwnd, c->base_id+1, filename, lenof(filename));
	    filename[lenof(filename)-1] = '\0';
	    of.nMaxFile = lenof(filename);
	    of.lpstrFileTitle = NULL;
	    of.lpstrInitialDir = NULL;
	    of.lpstrTitle = ctrl->fileselect.title;
	    of.Flags = 0;
	    if (ctrl->fileselect.for_writing)
		ret = GetSaveFileName(&of);
	    else
		ret = GetOpenFileName(&of);
	    if (ret) {
		SetDlgItemText(dp->hwnd, c->base_id + 1, filename);
		ctrl->generic.handler(ctrl, dp, dp->data, EVENT_VALCHANGE);
	    }
	}
	break;
      case CTRL_FONTSELECT:
	if (msg == WM_COMMAND && id == 2 &&
	    (HIWORD(wParam) == BN_SETFOCUS || HIWORD(wParam) == BN_KILLFOCUS))
	    winctrl_set_focus(ctrl, dp, HIWORD(wParam) == BN_SETFOCUS);
	if (id == 2 &&
	    (msg == WM_COMMAND &&
	     (HIWORD(wParam) == BN_CLICKED ||
	      HIWORD(wParam) == BN_DOUBLECLICKED))) {
	    CHOOSEFONT cf;
	    LOGFONT lf;
	    HDC hdc;
	    FontSpec fs = *(FontSpec *)c->data;
	    
	    hdc = GetDC(0);
	    lf.lfHeight = -MulDiv(fs.height,
				  GetDeviceCaps(hdc, LOGPIXELSY), 72);
	    ReleaseDC(0, hdc);
	    lf.lfWidth = lf.lfEscapement = lf.lfOrientation = 0;
	    lf.lfItalic = lf.lfUnderline = lf.lfStrikeOut = 0;
	    lf.lfWeight = (fs.isbold ? FW_BOLD : 0);
	    lf.lfCharSet = fs.charset;
	    lf.lfOutPrecision = OUT_DEFAULT_PRECIS;
	    lf.lfClipPrecision = CLIP_DEFAULT_PRECIS;
	    lf.lfQuality = DEFAULT_QUALITY;
	    lf.lfPitchAndFamily = FIXED_PITCH | FF_DONTCARE;
	    strncpy(lf.lfFaceName, fs.name,
		    sizeof(lf.lfFaceName) - 1);
	    lf.lfFaceName[sizeof(lf.lfFaceName) - 1] = '\0';

	    cf.lStructSize = sizeof(cf);
	    cf.hwndOwner = dp->hwnd;
	    cf.lpLogFont = &lf;
	    cf.Flags = CF_FIXEDPITCHONLY | CF_FORCEFONTEXIST |
		CF_INITTOLOGFONTSTRUCT | CF_SCREENFONTS;

	    if (ChooseFont(&cf)) {
		strncpy(fs.name, lf.lfFaceName,
			sizeof(fs.name) - 1);
		fs.name[sizeof(fs.name) - 1] = '\0';
		fs.isbold = (lf.lfWeight == FW_BOLD);
		fs.charset = lf.lfCharSet;
		fs.height = cf.iPointSize / 10;
		dlg_fontsel_set(ctrl, dp, fs);
		ctrl->generic.handler(ctrl, dp, dp->data, EVENT_VALCHANGE);
	    }
	}
	break;
    }

    /*
     * If the above event handler has asked for a colour selector,
     * now is the time to generate one.
     */
    if (dp->coloursel_wanted) {
	static CHOOSECOLOR cc;
	static DWORD custom[16] = { 0 };    /* zero initialisers */
	cc.lStructSize = sizeof(cc);
	cc.hwndOwner = dp->hwnd;
	cc.hInstance = (HWND) hinst;
	cc.lpCustColors = custom;
	cc.rgbResult = RGB(dp->coloursel_result.r,
			   dp->coloursel_result.g,
			   dp->coloursel_result.b);
	cc.Flags = CC_FULLOPEN | CC_RGBINIT;
	if (ChooseColor(&cc)) {
	    dp->coloursel_result.r =
		(unsigned char) (cc.rgbResult & 0xFF);
	    dp->coloursel_result.g =
		(unsigned char) (cc.rgbResult >> 8) & 0xFF;
	    dp->coloursel_result.b =
		(unsigned char) (cc.rgbResult >> 16) & 0xFF;
	    dp->coloursel_result.ok = TRUE;
	} else
	    dp->coloursel_result.ok = FALSE;
	ctrl->generic.handler(ctrl, dp, dp->data, EVENT_CALLBACK);
    }

    return ret;
}

/*
 * This function can be called to produce context help on a
 * control. Returns TRUE if it has actually launched WinHelp.
 */
int winctrl_context_help(struct dlgparam *dp, HWND hwnd, int id)
{
    int i;
    struct winctrl *c;
    char *cmd;

    /*
     * Look up the control ID in our data.
     */
    c = NULL;
    for (i = 0; i < dp->nctrltrees; i++) {
	c = winctrl_findbyid(dp->controltrees[i], id);
	if (c)
	    break;
    }
    if (!c)
	return 0;		       /* we have nothing to do */

    /*
     * This is the Windows front end, so we're allowed to assume
     * `helpctx.p' is a context string.
     */
    if (!c->ctrl || !c->ctrl->generic.helpctx.p)
	return 0;		       /* no help available for this ctrl */

    cmd = dupprintf("JI(`',`%s')", c->ctrl->generic.helpctx.p);
    WinHelp(hwnd, help_path, HELP_COMMAND, (DWORD)cmd);
    sfree(cmd);
    return 1;
}

/*
 * Now the various functions that the platform-independent
 * mechanism can call to access the dialog box entries.
 */

static struct winctrl *dlg_findbyctrl(struct dlgparam *dp, union control *ctrl)
{
    int i;

    for (i = 0; i < dp->nctrltrees; i++) {
	struct winctrl *c = winctrl_findbyctrl(dp->controltrees[i], ctrl);
	if (c)
	    return c;
    }
    return NULL;
}

void dlg_radiobutton_set(union control *ctrl, void *dlg, int whichbutton)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    assert(c && c->ctrl->generic.type == CTRL_RADIO);
    CheckRadioButton(dp->hwnd,
		     c->base_id + 1,
		     c->base_id + c->ctrl->radio.nbuttons,
		     c->base_id + 1 + whichbutton);
}

int dlg_radiobutton_get(union control *ctrl, void *dlg)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    int i;
    assert(c && c->ctrl->generic.type == CTRL_RADIO);
    for (i = 0; i < c->ctrl->radio.nbuttons; i++)
	if (IsDlgButtonChecked(dp->hwnd, c->base_id + 1 + i))
	    return i;
    assert(!"No radio button was checked?!");
    return 0;
}

void dlg_checkbox_set(union control *ctrl, void *dlg, int checked)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    assert(c && c->ctrl->generic.type == CTRL_CHECKBOX);
    CheckDlgButton(dp->hwnd, c->base_id, (checked != 0));
}

int dlg_checkbox_get(union control *ctrl, void *dlg)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    assert(c && c->ctrl->generic.type == CTRL_CHECKBOX);
    return 0 != IsDlgButtonChecked(dp->hwnd, c->base_id);
}

void dlg_editbox_set(union control *ctrl, void *dlg, char const *text)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    assert(c && c->ctrl->generic.type == CTRL_EDITBOX);
    SetDlgItemText(dp->hwnd, c->base_id+1, text);
}

void dlg_editbox_get(union control *ctrl, void *dlg, char *buffer, int length)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    assert(c && c->ctrl->generic.type == CTRL_EDITBOX);
    GetDlgItemText(dp->hwnd, c->base_id+1, buffer, length);
    buffer[length-1] = '\0';
}

/* The `listbox' functions can also apply to combo boxes. */
void dlg_listbox_clear(union control *ctrl, void *dlg)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    int msg;
    assert(c &&
	   (c->ctrl->generic.type == CTRL_LISTBOX ||
	    (c->ctrl->generic.type == CTRL_EDITBOX &&
	     c->ctrl->editbox.has_list)));
    msg = (c->ctrl->generic.type==CTRL_LISTBOX && c->ctrl->listbox.height!=0 ?
	   LB_RESETCONTENT : CB_RESETCONTENT);
    SendDlgItemMessage(dp->hwnd, c->base_id+1, msg, 0, 0);
}

void dlg_listbox_del(union control *ctrl, void *dlg, int index)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    int msg;
    assert(c &&
	   (c->ctrl->generic.type == CTRL_LISTBOX ||
	    (c->ctrl->generic.type == CTRL_EDITBOX &&
	     c->ctrl->editbox.has_list)));
    msg = (c->ctrl->generic.type==CTRL_LISTBOX && c->ctrl->listbox.height!=0 ?
	   LB_DELETESTRING : CB_DELETESTRING);
    SendDlgItemMessage(dp->hwnd, c->base_id+1, msg, index, 0);
}

void dlg_listbox_add(union control *ctrl, void *dlg, char const *text)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    int msg;
    assert(c &&
	   (c->ctrl->generic.type == CTRL_LISTBOX ||
	    (c->ctrl->generic.type == CTRL_EDITBOX &&
	     c->ctrl->editbox.has_list)));
    msg = (c->ctrl->generic.type==CTRL_LISTBOX && c->ctrl->listbox.height!=0 ?
	   LB_ADDSTRING : CB_ADDSTRING);
    SendDlgItemMessage(dp->hwnd, c->base_id+1, msg, 0, (LPARAM)text);
}

/*
 * Each listbox entry may have a numeric id associated with it.
 * Note that some front ends only permit a string to be stored at
 * each position, which means that _if_ you put two identical
 * strings in any listbox then you MUST not assign them different
 * IDs and expect to get meaningful results back.
 */
void dlg_listbox_addwithid(union control *ctrl, void *dlg,
			   char const *text, int id)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    int msg, msg2, index;
    assert(c &&
	   (c->ctrl->generic.type == CTRL_LISTBOX ||
	    (c->ctrl->generic.type == CTRL_EDITBOX &&
	     c->ctrl->editbox.has_list)));
    msg = (c->ctrl->generic.type==CTRL_LISTBOX && c->ctrl->listbox.height!=0 ?
	   LB_ADDSTRING : CB_ADDSTRING);
    msg2 = (c->ctrl->generic.type==CTRL_LISTBOX && c->ctrl->listbox.height!=0 ?
	   LB_SETITEMDATA : CB_SETITEMDATA);
    index = SendDlgItemMessage(dp->hwnd, c->base_id+1, msg, 0, (LPARAM)text);
    SendDlgItemMessage(dp->hwnd, c->base_id+1, msg2, index, (LPARAM)id);
}

int dlg_listbox_getid(union control *ctrl, void *dlg, int index)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    int msg;
    assert(c && c->ctrl->generic.type == CTRL_LISTBOX);
    msg = (c->ctrl->listbox.height != 0 ? LB_GETITEMDATA : CB_GETITEMDATA);
    return
	SendDlgItemMessage(dp->hwnd, c->base_id+1, msg, index, 0);
}

/* dlg_listbox_index returns <0 if no single element is selected. */
int dlg_listbox_index(union control *ctrl, void *dlg)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    int msg, ret;
    assert(c && c->ctrl->generic.type == CTRL_LISTBOX &&
	   !c->ctrl->listbox.multisel);
    msg = (c->ctrl->listbox.height != 0 ? LB_GETCURSEL : CB_GETCURSEL);
    ret = SendDlgItemMessage(dp->hwnd, c->base_id+1, msg, 0, 0);
    if (ret == LB_ERR)
	return -1;
    else
	return ret;
}

int dlg_listbox_issel(union control *ctrl, void *dlg, int index)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    assert(c && c->ctrl->generic.type == CTRL_LISTBOX &&
	   c->ctrl->listbox.multisel &&
	   c->ctrl->listbox.height != 0);
    return
	SendDlgItemMessage(dp->hwnd, c->base_id+1, LB_GETSEL, index, 0);
}

void dlg_listbox_select(union control *ctrl, void *dlg, int index)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    int msg;
    assert(c && c->ctrl->generic.type == CTRL_LISTBOX &&
	   !c->ctrl->listbox.multisel);
    msg = (c->ctrl->listbox.height != 0 ? LB_SETCURSEL : CB_SETCURSEL);
    SendDlgItemMessage(dp->hwnd, c->base_id+1, msg, index, 0);
}

void dlg_text_set(union control *ctrl, void *dlg, char const *text)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    assert(c && c->ctrl->generic.type == CTRL_TEXT);
    SetDlgItemText(dp->hwnd, c->base_id, text);
}

void dlg_filesel_set(union control *ctrl, void *dlg, Filename fn)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    assert(c && c->ctrl->generic.type == CTRL_FILESELECT);
    SetDlgItemText(dp->hwnd, c->base_id+1, fn.path);
}

void dlg_filesel_get(union control *ctrl, void *dlg, Filename *fn)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    assert(c && c->ctrl->generic.type == CTRL_FILESELECT);
    GetDlgItemText(dp->hwnd, c->base_id+1, fn->path, lenof(fn->path));
    fn->path[lenof(fn->path)-1] = '\0';
}

void dlg_fontsel_set(union control *ctrl, void *dlg, FontSpec fs)
{
    char *buf, *boldstr;
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    assert(c && c->ctrl->generic.type == CTRL_FONTSELECT);

    *(FontSpec *)c->data = fs;	       /* structure copy */

    boldstr = (fs.isbold ? "bold, " : "");
    if (fs.height == 0)
	buf = dupprintf("Font: %s, %sdefault height", fs.name, boldstr);
    else
	buf = dupprintf("Font: %s, %s%d-%s", fs.name, boldstr,
			(fs.height < 0 ? -fs.height : fs.height),
			(fs.height < 0 ? "pixel" : "point"));
    SetDlgItemText(dp->hwnd, c->base_id+1, buf);
    sfree(buf);
}

void dlg_fontsel_get(union control *ctrl, void *dlg, FontSpec *fs)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    assert(c && c->ctrl->generic.type == CTRL_FONTSELECT);
    *fs = *(FontSpec *)c->data;	       /* structure copy */
}

/*
 * Bracketing a large set of updates in these two functions will
 * cause the front end (if possible) to delay updating the screen
 * until it's all complete, thus avoiding flicker.
 */
void dlg_update_start(union control *ctrl, void *dlg)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    if (c && c->ctrl->generic.type == CTRL_LISTBOX) {
	SendDlgItemMessage(dp->hwnd, c->base_id+1, WM_SETREDRAW, FALSE, 0);
    }
}

void dlg_update_done(union control *ctrl, void *dlg)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    if (c && c->ctrl->generic.type == CTRL_LISTBOX) {
	HWND hw = GetDlgItem(dp->hwnd, c->base_id+1);
	SendMessage(hw, WM_SETREDRAW, TRUE, 0);
	InvalidateRect(hw, NULL, TRUE);
    }
}

void dlg_set_focus(union control *ctrl, void *dlg)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct winctrl *c = dlg_findbyctrl(dp, ctrl);
    int id;
    HWND ctl;
    switch (ctrl->generic.type) {
      case CTRL_EDITBOX: id = c->base_id + 1; break;
      case CTRL_RADIO:
	for (id = c->base_id + ctrl->radio.nbuttons; id > 1; id--)
	    if (IsDlgButtonChecked(dp->hwnd, id))
		break;
	/*
	 * In the theoretically-unlikely case that no button was
	 * selected, id should come out of this as 1, which is a
	 * reasonable enough choice.
	 */
	break;
      case CTRL_CHECKBOX: id = c->base_id; break;
      case CTRL_BUTTON: id = c->base_id; break;
      case CTRL_LISTBOX: id = c->base_id + 1; break;
      case CTRL_FILESELECT: id = c->base_id + 1; break;
      case CTRL_FONTSELECT: id = c->base_id + 2; break;
      default: id = c->base_id; break;
    }
    ctl = GetDlgItem(dp->hwnd, id);
    SetFocus(ctl);
}

/*
 * During event processing, you might well want to give an error
 * indication to the user. dlg_beep() is a quick and easy generic
 * error; dlg_error() puts up a message-box or equivalent.
 */
void dlg_beep(void *dlg)
{
    /* struct dlgparam *dp = (struct dlgparam *)dlg; */
    MessageBeep(0);
}

void dlg_error_msg(void *dlg, char *msg)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    MessageBox(dp->hwnd, msg,
	       dp->errtitle ? dp->errtitle : NULL,
	       MB_OK | MB_ICONERROR);
}

/*
 * This function signals to the front end that the dialog's
 * processing is completed, and passes an integer value (typically
 * a success status).
 */
void dlg_end(void *dlg, int value)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    dp->ended = TRUE;
    dp->endresult = value;
}

void dlg_refresh(union control *ctrl, void *dlg)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    int i, j;
    struct winctrl *c;

    if (!ctrl) {
	/*
	 * Send EVENT_REFRESH to absolutely everything.
	 */
	for (j = 0; j < dp->nctrltrees; j++) {
	    for (i = 0;
		 (c = winctrl_findbyindex(dp->controltrees[j], i)) != NULL;
		 i++) {
		if (c->ctrl && c->ctrl->generic.handler != NULL)
		    c->ctrl->generic.handler(c->ctrl, dp,
					     dp->data, EVENT_REFRESH);
	    }
	}
    } else {
	/*
	 * Send EVENT_REFRESH to a specific control.
	 */
	if (ctrl->generic.handler != NULL)
	    ctrl->generic.handler(ctrl, dp, dp->data, EVENT_REFRESH);
    }
}

void dlg_coloursel_start(union control *ctrl, void *dlg, int r, int g, int b)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    dp->coloursel_wanted = TRUE;
    dp->coloursel_result.r = r;
    dp->coloursel_result.g = g;
    dp->coloursel_result.b = b;
}

int dlg_coloursel_results(union control *ctrl, void *dlg,
			  int *r, int *g, int *b)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    if (dp->coloursel_result.ok) {
	*r = dp->coloursel_result.r;
	*g = dp->coloursel_result.g;
	*b = dp->coloursel_result.b;
	return 1;
    } else
	return 0;
}

struct perctrl_privdata {
    union control *ctrl;
    void *data;
    int needs_free;
};

static int perctrl_privdata_cmp(void *av, void *bv)
{
    struct perctrl_privdata *a = (struct perctrl_privdata *)av;
    struct perctrl_privdata *b = (struct perctrl_privdata *)bv;
    if (a->ctrl < b->ctrl)
	return -1;
    else if (a->ctrl > b->ctrl)
	return +1;
    return 0;
}

void dp_init(struct dlgparam *dp)
{
    dp->nctrltrees = 0;
    dp->data = NULL;
    dp->ended = FALSE;
    dp->focused = dp->lastfocused = NULL;
    memset(dp->shortcuts, 0, sizeof(dp->shortcuts));
    dp->hwnd = NULL;
    dp->wintitle = dp->errtitle = NULL;
    dp->privdata = newtree234(perctrl_privdata_cmp);
}

void dp_add_tree(struct dlgparam *dp, struct winctrls *wc)
{
    assert(dp->nctrltrees < lenof(dp->controltrees));
    dp->controltrees[dp->nctrltrees++] = wc;
}

void dp_cleanup(struct dlgparam *dp)
{
    struct perctrl_privdata *p;

    if (dp->privdata) {
	while ( (p = index234(dp->privdata, 0)) != NULL ) {
	    del234(dp->privdata, p);
	    if (p->needs_free)
		sfree(p->data);
	    sfree(p);
	}
	freetree234(dp->privdata);
	dp->privdata = NULL;
    }
    sfree(dp->wintitle);
    sfree(dp->errtitle);
}

void *dlg_get_privdata(union control *ctrl, void *dlg)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct perctrl_privdata tmp, *p;
    tmp.ctrl = ctrl;
    p = find234(dp->privdata, &tmp, NULL);
    if (p)
	return p->data;
    else
	return NULL;
}

void dlg_set_privdata(union control *ctrl, void *dlg, void *ptr)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct perctrl_privdata tmp, *p;
    tmp.ctrl = ctrl;
    p = find234(dp->privdata, &tmp, NULL);
    if (!p) {
	p = snew(struct perctrl_privdata);
	p->ctrl = ctrl;
	p->needs_free = FALSE;
	add234(dp->privdata, p);
    }
    p->data = ptr;
}

void *dlg_alloc_privdata(union control *ctrl, void *dlg, size_t size)
{
    struct dlgparam *dp = (struct dlgparam *)dlg;
    struct perctrl_privdata tmp, *p;
    tmp.ctrl = ctrl;
    p = find234(dp->privdata, &tmp, NULL);
    if (!p) {
	p = snew(struct perctrl_privdata);
	p->ctrl = ctrl;
	p->needs_free = FALSE;
	add234(dp->privdata, p);
    }
    assert(!p->needs_free);
    p->needs_free = TRUE;
    /*
     * This is an internal allocation routine, so it's allowed to
     * use smalloc directly.
     */
    p->data = smalloc(size);
    return p->data;
}
