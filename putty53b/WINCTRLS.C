/*
 * winctrls.c: routines to self-manage the controls in a dialog
 * box.
 */

#include <windows.h>
#include <commctrl.h>

#include "winstuff.h"
#include "puttymem.h"

#include "putty.h"

#define GAPBETWEEN 3
#define GAPWITHIN 1
#define GAPXBOX 7
#define GAPYBOX 4
#define DLGWIDTH 168
#define STATICHEIGHT 8
#define CHECKBOXHEIGHT 8
#define RADIOHEIGHT 8
#define EDITHEIGHT 12
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

    ctl = CreateWindowEx(exstyle, wclass, wtext, wstyle,
			 r.left, r.top, r.right, r.bottom,
			 cp->hwnd, (HMENU) wid, hinst, NULL);
    SendMessage(ctl, WM_SETFONT, cp->font, MAKELPARAM(TRUE, 0));
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
void multiedit(struct ctlpos *cp, ...)
{
    RECT r;
    va_list ap;
    int percent, xpos;

    percent = xpos = 0;
    va_start(ap, cp);
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
	      WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL,
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

static void radioline_common(struct ctlpos *cp, int nacross, va_list ap)
{
    RECT r;
    int group;
    int i;
    char *btext;

    group = WS_GROUP;
    i = 0;
    btext = va_arg(ap, char *);
    while (1) {
	char *nextbtext;
	int bid;
	if (!btext)
	    break;
	if (i == nacross) {
	    cp->ypos += r.bottom + GAPBETWEEN;
	    i = 0;
	}
	bid = va_arg(ap, int);
	nextbtext = va_arg(ap, char *);
	r.left = GAPBETWEEN + i * (cp->width + GAPBETWEEN) / nacross;
	if (nextbtext)
	    r.right =
		(i + 1) * (cp->width + GAPBETWEEN) / nacross - r.left;
	else
	    r.right = cp->width - r.left;
	r.top = cp->ypos;
	r.bottom = RADIOHEIGHT;
	doctl(cp, r, "BUTTON",
	      BS_AUTORADIOBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP |
	      group, 0, btext, bid);
	group = 0;
	i++;
	btext = nextbtext;
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
    RECT r;
    va_list ap;

    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = STATICHEIGHT;
    cp->ypos += r.bottom + GAPWITHIN;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, text, id);
    va_start(ap, nacross);
    radioline_common(cp, nacross, ap);
    va_end(ap);
}

/*
 * A set of radio buttons on the same line, without a static above
 * them. Otherwise just like radioline.
 */
void bareradioline(struct ctlpos *cp, int nacross, ...)
{
    va_list ap;

    va_start(ap, nacross);
    radioline_common(cp, nacross, ap);
    va_end(ap);
}

/*
 * A set of radio buttons on multiple lines, with a static above
 * them.
 */
void radiobig(struct ctlpos *cp, char *text, int id, ...)
{
    RECT r;
    va_list ap;
    int group;

    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = STATICHEIGHT;
    cp->ypos += r.bottom + GAPWITHIN;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, text, id);
    va_start(ap, id);
    group = WS_GROUP;
    while (1) {
	char *btext;
	int bid;
	btext = va_arg(ap, char *);
	if (!btext)
	    break;
	bid = va_arg(ap, int);
	r.left = GAPBETWEEN;
	r.top = cp->ypos;
	r.right = cp->width;
	r.bottom = STATICHEIGHT;
	cp->ypos += r.bottom + GAPWITHIN;
	doctl(cp, r, "BUTTON",
	      BS_AUTORADIOBUTTON | WS_CHILD | WS_VISIBLE | WS_TABSTOP |
	      group, 0, btext, bid);
	group = 0;
    }
    va_end(ap);
    cp->ypos += GAPBETWEEN - GAPWITHIN;
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
	  BS_AUTOCHECKBOX | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 0,
	  text, id);
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
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, text, id);
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
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON,
	  0, btext, bid);

    cp->ypos += height + GAPBETWEEN;
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
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON,
	  0, btext1, bid1);

    r.left = rpos2;
    r.top = cp->ypos + (height - PUSHBTNHEIGHT) / 2;
    r.right = rwid2;
    r.bottom = PUSHBTNHEIGHT;
    doctl(cp, r, "BUTTON",
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON,
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
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON,
	  0, btext, bid);

    cp->ypos += height + GAPBETWEEN;
}

/*
 * Special control which was hard to describe generically: the
 * session-saver assembly. A static; below that an edit box; below
 * that a list box. To the right of the list box, a column of
 * buttons.
 */
void sesssaver(struct ctlpos *cp, char *text,
	       int staticid, int editid, int listid, ...)
{
    RECT r;
    va_list ap;
    int lwid, rwid, rpos;
    int y;
    const int LISTDEFHEIGHT = 66;

    rpos = GAPBETWEEN + 3 * (cp->width + GAPBETWEEN) / 4;
    lwid = rpos - 2 * GAPBETWEEN;
    rwid = cp->width + GAPBETWEEN - rpos;

    /* The static control. */
    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = lwid;
    r.bottom = STATICHEIGHT;
    cp->ypos += r.bottom + GAPWITHIN;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, text, staticid);

    /* The edit control. */
    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = lwid;
    r.bottom = EDITHEIGHT;
    cp->ypos += r.bottom + GAPWITHIN;
    doctl(cp, r, "EDIT",
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL,
	  WS_EX_CLIENTEDGE, "", editid);

    /*
     * The buttons (we should hold off on the list box until we
     * know how big the buttons are).
     */
    va_start(ap, listid);
    y = cp->ypos;
    while (1) {
	char *btext = va_arg(ap, char *);
	int bid;
	if (!btext)
	    break;
	bid = va_arg(ap, int);
	r.left = rpos;
	r.top = y;
	r.right = rwid;
	r.bottom = PUSHBTNHEIGHT;
	y += r.bottom + GAPWITHIN;
	doctl(cp, r, "BUTTON",
	      WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON,
	      0, btext, bid);
    }

    /* Compute list box height. LISTDEFHEIGHT, or height of buttons. */
    y -= cp->ypos;
    y -= GAPWITHIN;
    if (y < LISTDEFHEIGHT)
	y = LISTDEFHEIGHT;
    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = lwid;
    r.bottom = y;
    cp->ypos += y + GAPBETWEEN;
    doctl(cp, r, "LISTBOX",
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL |
	  LBS_NOTIFY | LBS_HASSTRINGS, WS_EX_CLIENTEDGE, "", listid);
}

/*
 * Another special control: the environment-variable setter. A
 * static line first; then a pair of edit boxes with associated
 * statics, and two buttons; then a list box.
 */
void envsetter(struct ctlpos *cp, char *stext, int sid,
	       char *e1stext, int e1sid, int e1id,
	       char *e2stext, int e2sid, int e2id,
	       int listid, char *b1text, int b1id, char *b2text, int b2id)
{
    RECT r;
    const int height = (STATICHEIGHT > EDITHEIGHT
			&& STATICHEIGHT >
			PUSHBTNHEIGHT ? STATICHEIGHT : EDITHEIGHT >
			PUSHBTNHEIGHT ? EDITHEIGHT : PUSHBTNHEIGHT);
    const static int percents[] = { 20, 35, 10, 25 };
    int i, j, xpos, percent;
    const int LISTHEIGHT = 42;

    /* The static control. */
    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = STATICHEIGHT;
    cp->ypos += r.bottom + GAPWITHIN;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, stext, sid);

    /* The statics+edits+buttons. */
    for (j = 0; j < 2; j++) {
	percent = 10;
	for (i = 0; i < 4; i++) {
	    xpos = (cp->width + GAPBETWEEN) * percent / 100;
	    r.left = xpos + GAPBETWEEN;
	    percent += percents[i];
	    xpos = (cp->width + GAPBETWEEN) * percent / 100;
	    r.right = xpos - r.left;
	    r.top = cp->ypos;
	    r.bottom = (i == 0 ? STATICHEIGHT :
			i == 1 ? EDITHEIGHT : PUSHBTNHEIGHT);
	    r.top += (height - r.bottom) / 2;
	    if (i == 0) {
		doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0,
		      j == 0 ? e1stext : e2stext, j == 0 ? e1sid : e2sid);
	    } else if (i == 1) {
		doctl(cp, r, "EDIT",
		      WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL,
		      WS_EX_CLIENTEDGE, "", j == 0 ? e1id : e2id);
	    } else if (i == 3) {
		doctl(cp, r, "BUTTON",
		      WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON,
		      0, j == 0 ? b1text : b2text, j == 0 ? b1id : b2id);
	    }
	}
	cp->ypos += height + GAPWITHIN;
    }

    /* The list box. */
    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = LISTHEIGHT;
    cp->ypos += r.bottom + GAPBETWEEN;
    doctl(cp, r, "LISTBOX",
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL | LBS_HASSTRINGS
	  | LBS_USETABSTOPS, WS_EX_CLIENTEDGE, "", listid);
}

/*
 * Yet another special control: the character-class setter. A
 * static, then a list, then a line containing a
 * button-and-static-and-edit. 
 */
void charclass(struct ctlpos *cp, char *stext, int sid, int listid,
	       char *btext, int bid, int eid, char *s2text, int s2id)
{
    RECT r;
    const int height = (STATICHEIGHT > EDITHEIGHT
			&& STATICHEIGHT >
			PUSHBTNHEIGHT ? STATICHEIGHT : EDITHEIGHT >
			PUSHBTNHEIGHT ? EDITHEIGHT : PUSHBTNHEIGHT);
    const static int percents[] = { 30, 40, 30 };
    int i, xpos, percent;
    const int LISTHEIGHT = 52;

    /* The static control. */
    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = STATICHEIGHT;
    cp->ypos += r.bottom + GAPWITHIN;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, stext, sid);

    /* The list box. */
    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = LISTHEIGHT;
    cp->ypos += r.bottom + GAPWITHIN;
    doctl(cp, r, "LISTBOX",
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL | LBS_HASSTRINGS
	  | LBS_USETABSTOPS, WS_EX_CLIENTEDGE, "", listid);

    /* The button+static+edit. */
    percent = xpos = 0;
    for (i = 0; i < 3; i++) {
	r.left = xpos + GAPBETWEEN;
	percent += percents[i];
	xpos = (cp->width + GAPBETWEEN) * percent / 100;
	r.right = xpos - r.left;
	r.top = cp->ypos;
	r.bottom = (i == 0 ? PUSHBTNHEIGHT :
		    i == 1 ? STATICHEIGHT : EDITHEIGHT);
	r.top += (height - r.bottom) / 2;
	if (i == 0) {
	    doctl(cp, r, "BUTTON",
		  WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON,
		  0, btext, bid);
	} else if (i == 1) {
	    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE | SS_CENTER,
		  0, s2text, s2id);
	} else if (i == 2) {
	    doctl(cp, r, "EDIT",
		  WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL,
		  WS_EX_CLIENTEDGE, "", eid);
	}
    }
    cp->ypos += height + GAPBETWEEN;
}

/*
 * A special control (horrors!). The colour editor. A static line;
 * then on the left, a list box, and on the right, a sequence of
 * two-part statics followed by a button.
 */
void colouredit(struct ctlpos *cp, char *stext, int sid, int listid,
		char *btext, int bid, ...)
{
    RECT r;
    int y;
    va_list ap;
    int lwid, rwid, rpos;
    const int LISTHEIGHT = 66;

    /* The static control. */
    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = STATICHEIGHT;
    cp->ypos += r.bottom + GAPWITHIN;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, stext, sid);

    rpos = GAPBETWEEN + 2 * (cp->width + GAPBETWEEN) / 3;
    lwid = rpos - 2 * GAPBETWEEN;
    rwid = cp->width + GAPBETWEEN - rpos;

    /* The list box. */
    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = lwid;
    r.bottom = LISTHEIGHT;
    doctl(cp, r, "LISTBOX",
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL | LBS_HASSTRINGS
	  | LBS_USETABSTOPS | LBS_NOTIFY, WS_EX_CLIENTEDGE, "", listid);

    /* The statics. */
    y = cp->ypos;
    va_start(ap, bid);
    while (1) {
	char *ltext;
	int lid, rid;
	ltext = va_arg(ap, char *);
	if (!ltext)
	    break;
	lid = va_arg(ap, int);
	rid = va_arg(ap, int);
	r.top = y;
	r.bottom = STATICHEIGHT;
	y += r.bottom + GAPWITHIN;
	r.left = rpos;
	r.right = rwid / 2;
	doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, ltext, lid);
	r.left = rpos + r.right;
	r.right = rwid - r.right;
	doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE | SS_RIGHT, 0, "",
	      rid);
    }
    va_end(ap);

    /* The button. */
    r.top = y + 2 * GAPWITHIN;
    r.bottom = PUSHBTNHEIGHT;
    r.left = rpos;
    r.right = rwid;
    doctl(cp, r, "BUTTON",
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON,
	  0, btext, bid);

    cp->ypos += LISTHEIGHT + GAPBETWEEN;
}

/*
 * A special control for manipulating an ordered preference list
 * (eg. for cipher selection).
 * XXX: this is a rough hack and could be improved.
 */
void prefslist(struct prefslist *hdl, struct ctlpos *cp, char *stext,
               int sid, int listid, int upbid, int dnbid)
{
    const static int percents[] = { 5, 75, 20 };
    RECT r;
    int xpos, percent = 0, i;
    const int DEFLISTHEIGHT = 52;      /* XXX configurable? */
    const int BTNSHEIGHT = 2*PUSHBTNHEIGHT + GAPBETWEEN;
    int totalheight;

    /* Squirrel away IDs. */
    hdl->listid = listid;
    hdl->upbid  = upbid;
    hdl->dnbid  = dnbid;

    /* The static label. */
    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = STATICHEIGHT;
    cp->ypos += r.bottom + GAPWITHIN;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, stext, sid);

    /* XXX it'd be nice to centre the buttons wrt the listbox
     * but we'd have to find out how high the latter actually is. */
    if (DEFLISTHEIGHT > BTNSHEIGHT) {
        totalheight = DEFLISTHEIGHT;
    } else {
        totalheight = BTNSHEIGHT;
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
            r.top = cp->ypos; r.bottom = totalheight;
            {
                HWND ctl;
                ctl = doctl(cp, r, "LISTBOX",
                            WS_CHILD | WS_VISIBLE | WS_TABSTOP |
			    WS_VSCROLL | LBS_HASSTRINGS,
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
            r.top = cp->ypos; r.bottom = PUSHBTNHEIGHT;
            doctl(cp, r, "BUTTON",
                  WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON,
                  0, "&Up", upbid);

            r.left = left; r.right = wid;
            r.top = cp->ypos + PUSHBTNHEIGHT + GAPBETWEEN;
            r.bottom = PUSHBTNHEIGHT;
            doctl(cp, r, "BUTTON",
                  WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON,
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
    txt = smalloc(tlen+1);
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
 */
int handle_prefslist(struct prefslist *hdl,
                     int *array, int maxmemb,
                     int is_dlmsg, HWND hwnd,
		     WPARAM wParam, LPARAM lParam)
{
    int i;
    int ret;

    if (is_dlmsg) {

        if ((int)wParam == hdl->listid) {
            DRAGLISTINFO *dlm = (DRAGLISTINFO *)lParam;
            int dest;
            switch (dlm->uNotification) {
              case DL_BEGINDRAG:
		hdl->dummyitem =
		    SendDlgItemMessage(hwnd, hdl->listid,
				       LB_ADDSTRING, 0, (LPARAM) "");

                hdl->srcitem = LBItemFromPt(dlm->hWnd, dlm->ptCursor, TRUE);
		hdl->dragging = 0;
		/* XXX hack Q183115 */
		SetWindowLong(hwnd, DWL_MSGRESULT, TRUE);
                ret = 1; break;
              case DL_CANCELDRAG:
		DrawInsert(hwnd, dlm->hWnd, -1);     /* Clear arrow */
		SendDlgItemMessage(hwnd, hdl->listid,
				   LB_DELETESTRING, hdl->dummyitem, 0);
		hdl->dragging = 0;
                ret = 1; break;
              case DL_DRAGGING:
		hdl->dragging = 1;
		dest = pl_itemfrompt(dlm->hWnd, dlm->ptCursor, TRUE);
		if (dest > hdl->dummyitem) dest = hdl->dummyitem;
		DrawInsert (hwnd, dlm->hWnd, dest);
		if (dest >= 0)
		    SetWindowLong(hwnd, DWL_MSGRESULT, DL_MOVECURSOR);
		else
		    SetWindowLong(hwnd, DWL_MSGRESULT, DL_STOPCURSOR);
                ret = 1; break;
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
		}
                ret = 1; break;
            }
        }

    } else {

        ret = 0;
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
            }

        }

    }

    /* Update array to match the list box. */
    for (i=0; i < maxmemb; i++)
        array[i] = SendDlgItemMessage (hwnd, hdl->listid, LB_GETITEMDATA,
                                       i, 0);

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

/*
 * Another special control: the forwarding options setter. First a
 * list box; next a static header line, introducing a pair of edit
 * boxes with associated statics, another button, and a radio
 * button pair.
 */
void fwdsetter(struct ctlpos *cp, int listid, char *stext, int sid,
	       char *e1stext, int e1sid, int e1id,
	       char *e2stext, int e2sid, int e2id,
	       char *btext, int bid)
{
    RECT r;
    const int height = (STATICHEIGHT > EDITHEIGHT
			&& STATICHEIGHT >
			PUSHBTNHEIGHT ? STATICHEIGHT : EDITHEIGHT >
			PUSHBTNHEIGHT ? EDITHEIGHT : PUSHBTNHEIGHT);
    const static int percents[] = { 25, 35, 15, 25 };
    int i, j, xpos, percent;
    const int LISTHEIGHT = 42;

    /* The list box. */
    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = LISTHEIGHT;
    cp->ypos += r.bottom + GAPBETWEEN;
    doctl(cp, r, "LISTBOX",
	  WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL | LBS_HASSTRINGS
	  | LBS_USETABSTOPS, WS_EX_CLIENTEDGE, "", listid);

    /* The static control. */
    r.left = GAPBETWEEN;
    r.top = cp->ypos;
    r.right = cp->width;
    r.bottom = STATICHEIGHT;
    cp->ypos += r.bottom + GAPWITHIN;
    doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0, stext, sid);

    /* The statics+edits+buttons. */
    for (j = 0; j < 2; j++) {
	percent = 0;
	for (i = 0; i < (j ? 2 : 4); i++) {
	    xpos = (cp->width + GAPBETWEEN) * percent / 100;
	    r.left = xpos + GAPBETWEEN;
	    percent += percents[i];
	    if (j==1 && i==1) percent = 100;
	    xpos = (cp->width + GAPBETWEEN) * percent / 100;
	    r.right = xpos - r.left;
	    r.top = cp->ypos;
	    r.bottom = (i == 0 ? STATICHEIGHT :
			i == 1 ? EDITHEIGHT : PUSHBTNHEIGHT);
	    r.top += (height - r.bottom) / 2;
	    if (i == 0) {
		doctl(cp, r, "STATIC", WS_CHILD | WS_VISIBLE, 0,
		      j == 0 ? e1stext : e2stext, j == 0 ? e1sid : e2sid);
	    } else if (i == 1) {
		doctl(cp, r, "EDIT",
		      WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_AUTOHSCROLL,
		      WS_EX_CLIENTEDGE, "", j == 0 ? e1id : e2id);
	    } else if (i == 3) {
		doctl(cp, r, "BUTTON",
		      WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON,
		      0, btext, bid);
	    }
	}
	cp->ypos += height + GAPWITHIN;
    }
}
