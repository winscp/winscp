#include <windows.h>

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include <time.h>
#include <assert.h>
#include "putty.h"
#include "tree234.h"

#define VT52_PLUS

#define CL_ANSIMIN	0x0001	       /* Codes in all ANSI like terminals. */
#define CL_VT100	0x0002	       /* VT100 */
#define CL_VT100AVO	0x0004	       /* VT100 +AVO; 132x24 (not 132x14) & attrs */
#define CL_VT102	0x0008	       /* VT102 */
#define CL_VT220	0x0010	       /* VT220 */
#define CL_VT320	0x0020	       /* VT320 */
#define CL_VT420	0x0040	       /* VT420 */
#define CL_VT510	0x0080	       /* VT510, NB VT510 includes ANSI */
#define CL_VT340TEXT	0x0100	       /* VT340 extensions that appear in the VT420 */
#define CL_SCOANSI	0x1000	       /* SCOANSI not in ANSIMIN. */
#define CL_ANSI		0x2000	       /* ANSI ECMA-48 not in the VT100..VT420 */
#define CL_OTHER	0x4000	       /* Others, Xterm, linux, putty, dunno, etc */

#define TM_VT100	(CL_ANSIMIN|CL_VT100)
#define TM_VT100AVO	(TM_VT100|CL_VT100AVO)
#define TM_VT102	(TM_VT100AVO|CL_VT102)
#define TM_VT220	(TM_VT102|CL_VT220)
#define TM_VTXXX	(TM_VT220|CL_VT340TEXT|CL_VT510|CL_VT420|CL_VT320)
#define TM_SCOANSI	(CL_ANSIMIN|CL_SCOANSI)

#define TM_PUTTY	(0xFFFF)

#define compatibility(x) \
    if ( ((CL_##x)&compatibility_level) == 0 ) { 	\
       termstate=TOPLEVEL;				\
       break;						\
    }
#define compatibility2(x,y) \
    if ( ((CL_##x|CL_##y)&compatibility_level) == 0 ) { \
       termstate=TOPLEVEL;				\
       break;						\
    }

#define has_compat(x) ( ((CL_##x)&compatibility_level) != 0 )

static int compatibility_level = TM_PUTTY;

static tree234 *scrollback;	       /* lines scrolled off top of screen */
static tree234 *screen;		       /* lines on primary screen */
static tree234 *alt_screen;	       /* lines on alternate screen */
static int disptop;		       /* distance scrolled back (0 or -ve) */

static unsigned long *cpos;	       /* cursor position (convenience) */

static unsigned long *disptext;	       /* buffer of text on real screen */
static unsigned long *dispcurs;	       /* location of cursor on real screen */
static unsigned long curstype;	       /* type of cursor on real screen */

#define VBELL_TIMEOUT 100	       /* millisecond len of visual bell */

struct beeptime {
    struct beeptime *next;
    unsigned long ticks;
};
static struct beeptime *beephead, *beeptail;
int nbeeps;
int beep_overloaded;
long lastbeep;

#define TSIZE (sizeof(unsigned long))
#define fix_cpos do { cpos = lineptr(curs.y) + curs.x; } while(0)

static unsigned long curr_attr, save_attr;
static unsigned long erase_char = ERASE_CHAR;

typedef struct {
    int y, x;
} pos;
#define poslt(p1,p2) ( (p1).y < (p2).y || ( (p1).y == (p2).y && (p1).x < (p2).x ) )
#define posle(p1,p2) ( (p1).y < (p2).y || ( (p1).y == (p2).y && (p1).x <= (p2).x ) )
#define poseq(p1,p2) ( (p1).y == (p2).y && (p1).x == (p2).x )
#define posdiff(p1,p2) ( ((p1).y - (p2).y) * (cols+1) + (p1).x - (p2).x )
#define incpos(p) ( (p).x == cols ? ((p).x = 0, (p).y++, 1) : ((p).x++, 0) )
#define decpos(p) ( (p).x == 0 ? ((p).x = cols, (p).y--, 1) : ((p).x--, 0) )

/* Product-order comparisons for rectangular block selection. */
#define posPlt(p1,p2) ( (p1).y <= (p2).y && (p1).x < (p2).x )
#define posPle(p1,p2) ( (p1).y <= (p2).y && (p1).x <= (p2).x )

static bufchain inbuf;		       /* terminal input buffer */
static pos curs;		       /* cursor */
static pos savecurs;		       /* saved cursor position */
static int marg_t, marg_b;	       /* scroll margins */
static int dec_om;		       /* DEC origin mode flag */
static int wrap, wrapnext;	       /* wrap flags */
static int insert;		       /* insert-mode flag */
static int cset;		       /* 0 or 1: which char set */
static int save_cset, save_csattr;     /* saved with cursor position */
static int save_utf, save_wnext;       /* saved with cursor position */
static int rvideo;		       /* global reverse video flag */
static unsigned long rvbell_startpoint;/* for ESC[?5hESC[?5l vbell */
static int cursor_on;		       /* cursor enabled flag */
static int reset_132;		       /* Flag ESC c resets to 80 cols */
static int use_bce;		       /* Use Background coloured erase */
static int blinker;		       /* When blinking is the cursor on ? */
static int tblinker;		       /* When the blinking text is on */
static int blink_is_real;	       /* Actually blink blinking text */
static int term_echoing;	       /* Does terminal want local echo? */
static int term_editing;	       /* Does terminal want local edit? */
static int sco_acs, save_sco_acs;      /* CSI 10,11,12m -> OEM charset */
static int vt52_bold;		       /* Force bold on non-bold colours */
static int utf_state;		       /* Is there a pending UTF-8 character */
static int utf_char;		       /* and what is it so far. */
static int utf_size;		       /* The size of the UTF character. */
static int printing, only_printing;    /* Are we doing ANSI printing? */
static int print_state;		       /* state of print-end-sequence scan */
static bufchain printer_buf;	       /* buffered data for printer */
static printer_job *print_job;

static int xterm_mouse;		       /* send mouse messages to app */

static unsigned long cset_attr[2];

/*
 * Saved settings on the alternate screen.
 */
static int alt_x, alt_y, alt_om, alt_wrap, alt_wnext, alt_ins, alt_cset, alt_sco_acs, alt_utf;
static int alt_t, alt_b;
static int alt_which;

#define ARGS_MAX 32		       /* max # of esc sequence arguments */
#define ARG_DEFAULT 0		       /* if an arg isn't specified */
#define def(a,d) ( (a) == ARG_DEFAULT ? (d) : (a) )
static int esc_args[ARGS_MAX];
static int esc_nargs;
static int esc_query;
#define ANSI(x,y)	((x)+((y)<<8))
#define ANSI_QUE(x)	ANSI(x,TRUE)

#define OSC_STR_MAX 2048
static int osc_strlen;
static char osc_string[OSC_STR_MAX + 1];
static int osc_w;

static char id_string[1024] = "\033[?6c";

static unsigned char *tabs;

static enum {
    TOPLEVEL,
    SEEN_ESC,
    SEEN_CSI,
    SEEN_OSC,
    SEEN_OSC_W,

    DO_CTRLS,

    SEEN_OSC_P,
    OSC_STRING, OSC_MAYBE_ST,
    VT52_ESC,
    VT52_Y1,
    VT52_Y2,
    VT52_FG,
    VT52_BG
} termstate;

static enum {
    NO_SELECTION, ABOUT_TO, DRAGGING, SELECTED
} selstate;
static enum {
    LEXICOGRAPHIC, RECTANGULAR
} seltype;
static enum {
    SM_CHAR, SM_WORD, SM_LINE
} selmode;
static pos selstart, selend, selanchor;

static short wordness[256] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,	       /* 01 */
    0, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 1, 1, 1, 1, 1, 1,	       /* 23 */
    1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 1, 1, 1, 1, 2,	       /* 45 */
    1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 1, 1, 1, 1, 1,	       /* 67 */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,	       /* 89 */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,	       /* AB */
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1,
    2, 2, 2, 2, 2, 2, 2, 2,	       /* CD */
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1,
    2, 2, 2, 2, 2, 2, 2, 2,	       /* EF */
};

#define sel_nl_sz  (sizeof(sel_nl)/sizeof(wchar_t))
static wchar_t sel_nl[] = SEL_NL;
static wchar_t *paste_buffer = 0;
static int paste_len, paste_pos, paste_hold;

/*
 * Internal prototypes.
 */
static void do_paint(Context, int);
static void erase_lots(int, int, int);
static void swap_screen(int);
static void update_sbar(void);
static void deselect(void);
static void term_print_finish(void);

/*
 * Resize a line to make it `cols' columns wide.
 */
unsigned long *resizeline(unsigned long *line, int cols)
{
    int i, oldlen;
    unsigned long lineattrs;

    if (line[0] != (unsigned long)cols) {
	/*
	 * This line is the wrong length, which probably means it
	 * hasn't been accessed since a resize. Resize it now.
	 */
	oldlen = line[0];
	lineattrs = line[oldlen + 1];
	line = srealloc(line, TSIZE * (2 + cols));
	line[0] = cols;
	for (i = oldlen; i < cols; i++)
	    line[i + 1] = ERASE_CHAR;
	line[cols + 1] = lineattrs & LATTR_MODE;
    }

    return line;
}

/*
 * Retrieve a line of the screen or of the scrollback, according to
 * whether the y coordinate is non-negative or negative
 * (respectively).
 */
unsigned long *lineptr(int y, int lineno)
{
    unsigned long *line, *newline;
    tree234 *whichtree;
    int treeindex;

    if (y >= 0) {
	whichtree = screen;
	treeindex = y;
    } else {
	whichtree = scrollback;
	treeindex = y + count234(scrollback);
    }
    line = index234(whichtree, treeindex);

    /* We assume that we don't screw up and retrieve something out of range. */
    assert(line != NULL);

    newline = resizeline(line, cols);
    if (newline != line) {
	delpos234(whichtree, treeindex);
	addpos234(whichtree, newline, treeindex);
        line = newline;
    }

    return line + 1;
}

#define lineptr(x) lineptr(x,__LINE__)
/*
 * Set up power-on settings for the terminal.
 */
static void power_on(void)
{
    curs.x = curs.y = alt_x = alt_y = savecurs.x = savecurs.y = 0;
    alt_t = marg_t = 0;
    if (rows != -1)
	alt_b = marg_b = rows - 1;
    else
	alt_b = marg_b = 0;
    if (cols != -1) {
	int i;
	for (i = 0; i < cols; i++)
	    tabs[i] = (i % 8 == 0 ? TRUE : FALSE);
    }
    alt_om = dec_om = cfg.dec_om;
    alt_wnext = wrapnext = alt_ins = insert = FALSE;
    alt_wrap = wrap = cfg.wrap_mode;
    alt_cset = cset = 0;
    alt_utf = utf = 0;
    alt_sco_acs = sco_acs = 0;
    cset_attr[0] = cset_attr[1] = ATTR_ASCII;
    rvideo = 0;
    in_vbell = FALSE;
    cursor_on = 1;
    big_cursor = 0;
    save_attr = curr_attr = ATTR_DEFAULT;
    term_editing = term_echoing = FALSE;
    ldisc_send(NULL, 0, 0);	       /* cause ldisc to notice changes */
    app_cursor_keys = cfg.app_cursor;
    app_keypad_keys = cfg.app_keypad;
    use_bce = cfg.bce;
    blink_is_real = cfg.blinktext;
    erase_char = ERASE_CHAR;
    alt_which = 0;
    term_print_finish();
    {
	int i;
	for (i = 0; i < 256; i++)
	    wordness[i] = cfg.wordness[i];
    }
    if (screen) {
	swap_screen(1);
	erase_lots(FALSE, TRUE, TRUE);
	swap_screen(0);
	erase_lots(FALSE, TRUE, TRUE);
    }
}

/*
 * Force a screen update.
 */
void term_update(void)
{
    Context ctx;
    ctx = get_ctx();
    if (ctx) {
	int need_sbar_update = seen_disp_event;
	if ((seen_key_event && (cfg.scroll_on_key)) ||
	    (seen_disp_event && (cfg.scroll_on_disp))) {
	    disptop = 0;	       /* return to main screen */
	    seen_disp_event = seen_key_event = 0;
	    need_sbar_update = TRUE;
	}
	if (need_sbar_update)
	    update_sbar();
	do_paint(ctx, TRUE);
	sys_cursor(curs.x, curs.y - disptop);
	free_ctx(ctx);
    }
}

/*
 * Same as power_on(), but an external function.
 */
void term_pwron(void)
{
    power_on();
    fix_cpos;
    disptop = 0;
    deselect();
    term_update();
}

/*
 * When the user reconfigures us, we need to check the forbidden-
 * alternate-screen config option, disable raw mouse mode if the
 * user has disabled mouse reporting, and abandon a print job if
 * the user has disabled printing.
 */
void term_reconfig(void)
{
    if (cfg.no_alt_screen)
	swap_screen(0);
    if (cfg.no_mouse_rep) {
	xterm_mouse = 0;
	set_raw_mouse_mode(0);
    }
    if (cfg.no_remote_charset) {
	cset_attr[0] = cset_attr[1] = ATTR_ASCII;
	sco_acs = alt_sco_acs = 0;
	utf = 0;
    }
    if (!*cfg.printer) {
	term_print_finish();
    }
}

/*
 * Clear the scrollback.
 */
void term_clrsb(void)
{
    unsigned long *line;
    disptop = 0;
    while ((line = delpos234(scrollback, 0)) != NULL) {
	sfree(line);
    }
    update_sbar();
}

/*
 * Initialise the terminal.
 */
void term_init(void)
{
    screen = alt_screen = scrollback = NULL;
    disptop = 0;
    disptext = dispcurs = NULL;
    tabs = NULL;
    deselect();
    rows = cols = -1;
    power_on();
    beephead = beeptail = NULL;
    nbeeps = 0;
    lastbeep = FALSE;
    beep_overloaded = FALSE;
}

/*
 * Set up the terminal for a given size.
 */
void term_size(int newrows, int newcols, int newsavelines)
{
    tree234 *newalt;
    unsigned long *newdisp, *line;
    int i, j;
    int sblen;
    int save_alt_which = alt_which;

    if (newrows == rows && newcols == cols && newsavelines == savelines)
	return;			       /* nothing to do */

    deselect();
    swap_screen(0);

    alt_t = marg_t = 0;
    alt_b = marg_b = newrows - 1;

    if (rows == -1) {
	scrollback = newtree234(NULL);
	screen = newtree234(NULL);
	rows = 0;
    }

    /*
     * Resize the screen and scrollback. We only need to shift
     * lines around within our data structures, because lineptr()
     * will take care of resizing each individual line if
     * necessary. So:
     * 
     *  - If the new screen and the old screen differ in length, we
     *    must shunt some lines in from the scrollback or out to
     *    the scrollback.
     * 
     *  - If doing that fails to provide us with enough material to
     *    fill the new screen (i.e. the number of rows needed in
     *    the new screen exceeds the total number in the previous
     *    screen+scrollback), we must invent some blank lines to
     *    cover the gap.
     * 
     *  - Then, if the new scrollback length is less than the
     *    amount of scrollback we actually have, we must throw some
     *    away.
     */
    sblen = count234(scrollback);
    /* Do this loop to expand the screen if newrows > rows */
    for (i = rows; i < newrows; i++) {
	if (sblen > 0) {
	    line = delpos234(scrollback, --sblen);
	} else {
	    line = smalloc(TSIZE * (newcols + 2));
	    line[0] = newcols;
	    for (j = 0; j <= newcols; j++)
		line[j + 1] = ERASE_CHAR;
	}
	addpos234(screen, line, 0);
    }
    /* Do this loop to shrink the screen if newrows < rows */
    for (i = newrows; i < rows; i++) {
	line = delpos234(screen, 0);
	addpos234(scrollback, line, sblen++);
    }
    assert(count234(screen) == newrows);
    while (sblen > newsavelines) {
	line = delpos234(scrollback, 0);
	sfree(line);
	sblen--;
    }
    assert(count234(scrollback) <= newsavelines);
    disptop = 0;

    newdisp = smalloc(newrows * (newcols + 1) * TSIZE);
    for (i = 0; i < newrows * (newcols + 1); i++)
	newdisp[i] = ATTR_INVALID;
    sfree(disptext);
    disptext = newdisp;
    dispcurs = NULL;

    newalt = newtree234(NULL);
    for (i = 0; i < newrows; i++) {
	line = smalloc(TSIZE * (newcols + 2));
	line[0] = newcols;
	for (j = 0; j <= newcols; j++)
	    line[j + 1] = erase_char;
	addpos234(newalt, line, i);
    }
    if (alt_screen) {
	while (NULL != (line = delpos234(alt_screen, 0)))
	    sfree(line);
	freetree234(alt_screen);
    }
    alt_screen = newalt;

    tabs = srealloc(tabs, newcols * sizeof(*tabs));
    {
	int i;
	for (i = (cols > 0 ? cols : 0); i < newcols; i++)
	    tabs[i] = (i % 8 == 0 ? TRUE : FALSE);
    }

    if (rows > 0)
	curs.y += newrows - rows;
    if (curs.y < 0)
	curs.y = 0;
    if (curs.y >= newrows)
	curs.y = newrows - 1;
    if (curs.x >= newcols)
	curs.x = newcols - 1;
    alt_x = alt_y = 0;
    wrapnext = alt_wnext = FALSE;

    rows = newrows;
    cols = newcols;
    savelines = newsavelines;
    fix_cpos;

    swap_screen(save_alt_which);

    update_sbar();
    term_update();
    back->size();
}

/*
 * Swap screens.
 */
static void swap_screen(int which)
{
    int t;
    tree234 *ttr;

    if (which == alt_which)
	return;

    alt_which = which;

    ttr = alt_screen;
    alt_screen = screen;
    screen = ttr;
    t = curs.x;
    curs.x = alt_x;
    alt_x = t;
    t = curs.y;
    curs.y = alt_y;
    alt_y = t;
    t = marg_t;
    marg_t = alt_t;
    alt_t = t;
    t = marg_b;
    marg_b = alt_b;
    alt_b = t;
    t = dec_om;
    dec_om = alt_om;
    alt_om = t;
    t = wrap;
    wrap = alt_wrap;
    alt_wrap = t;
    t = wrapnext;
    wrapnext = alt_wnext;
    alt_wnext = t;
    t = insert;
    insert = alt_ins;
    alt_ins = t;
    t = cset;
    cset = alt_cset;
    alt_cset = t;
    t = utf;
    utf = alt_utf;
    alt_utf = t;
    t = sco_acs;
    sco_acs = alt_sco_acs;
    alt_sco_acs = t;

    fix_cpos;
}

/*
 * Update the scroll bar.
 */
static void update_sbar(void)
{
    int nscroll;

    nscroll = count234(scrollback);

    set_sbar(nscroll + rows, nscroll + disptop, rows);
}

/*
 * Check whether the region bounded by the two pointers intersects
 * the scroll region, and de-select the on-screen selection if so.
 */
static void check_selection(pos from, pos to)
{
    if (poslt(from, selend) && poslt(selstart, to))
	deselect();
}

/*
 * Scroll the screen. (`lines' is +ve for scrolling forward, -ve
 * for backward.) `sb' is TRUE if the scrolling is permitted to
 * affect the scrollback buffer.
 * 
 * NB this function invalidates all pointers into lines of the
 * screen data structures. In particular, you MUST call fix_cpos
 * after calling scroll() and before doing anything else that
 * uses the cpos shortcut pointer.
 */
static void scroll(int topline, int botline, int lines, int sb)
{
    unsigned long *line, *line2;
    int i, seltop;

    if (topline != 0 || alt_which != 0)
	sb = FALSE;

    if (lines < 0) {
	while (lines < 0) {
	    line = delpos234(screen, botline);
            line = resizeline(line, cols);
	    for (i = 0; i < cols; i++)
		line[i + 1] = erase_char;
	    line[cols + 1] = 0;
	    addpos234(screen, line, topline);

	    if (selstart.y >= topline && selstart.y <= botline) {
		selstart.y++;
		if (selstart.y > botline) {
		    selstart.y = botline;
		    selstart.x = 0;
		}
	    }
	    if (selend.y >= topline && selend.y <= botline) {
		selend.y++;
		if (selend.y > botline) {
		    selend.y = botline;
		    selend.x = 0;
		}
	    }

	    lines++;
	}
    } else {
	while (lines > 0) {
	    line = delpos234(screen, topline);
	    if (sb && savelines > 0) {
		int sblen = count234(scrollback);
		/*
		 * We must add this line to the scrollback. We'll
		 * remove a line from the top of the scrollback to
		 * replace it, or allocate a new one if the
		 * scrollback isn't full.
		 */
		if (sblen == savelines) {
		    sblen--, line2 = delpos234(scrollback, 0);
		} else {
		    line2 = smalloc(TSIZE * (cols + 2));
		    line2[0] = cols;
		}
		addpos234(scrollback, line, sblen);
		line = line2;

		/*
		 * If the user is currently looking at part of the
		 * scrollback, and they haven't enabled any options
		 * that are going to reset the scrollback as a
		 * result of this movement, then the chances are
		 * they'd like to keep looking at the same line. So
		 * we move their viewpoint at the same rate as the
		 * scroll, at least until their viewpoint hits the
		 * top end of the scrollback buffer, at which point
		 * we don't have the choice any more.
		 * 
		 * Thanks to Jan Holmen Holsten for the idea and
		 * initial implementation.
		 */
		if (disptop > -savelines && disptop < 0)
		    disptop--;
	    }
            line = resizeline(line, cols);
	    for (i = 0; i < cols; i++)
		line[i + 1] = erase_char;
	    line[cols + 1] = 0;
	    addpos234(screen, line, botline);

	    /*
	     * If the selection endpoints move into the scrollback,
	     * we keep them moving until they hit the top. However,
	     * of course, if the line _hasn't_ moved into the
	     * scrollback then we don't do this, and cut them off
	     * at the top of the scroll region.
	     * 
	     * This applies to selstart and selend (for an existing
	     * selection), and also selanchor (for one being
	     * selected as we speak).
	     */
	    seltop = sb ? -savelines : topline;

	    if (selstart.y >= seltop && selstart.y <= botline) {
		selstart.y--;
		if (selstart.y < seltop) {
		    selstart.y = seltop;
		    selstart.x = 0;
		}
	    }
	    if (selend.y >= seltop && selend.y <= botline) {
		selend.y--;
		if (selend.y < seltop) {
		    selend.y = seltop;
		    selend.x = 0;
		}
	    }
	    if (selanchor.y >= seltop && selanchor.y <= botline) {
		selanchor.y--;
		if (selanchor.y < seltop) {
		    selanchor.y = seltop;
		    selanchor.x = 0;
		}
	    }

	    lines--;
	}
    }
}

/*
 * Move the cursor to a given position, clipping at boundaries. We
 * may or may not want to clip at the scroll margin: marg_clip is 0
 * not to, 1 to disallow _passing_ the margins, and 2 to disallow
 * even _being_ outside the margins.
 */
static void move(int x, int y, int marg_clip)
{
    if (x < 0)
	x = 0;
    if (x >= cols)
	x = cols - 1;
    if (marg_clip) {
	if ((curs.y >= marg_t || marg_clip == 2) && y < marg_t)
	    y = marg_t;
	if ((curs.y <= marg_b || marg_clip == 2) && y > marg_b)
	    y = marg_b;
    }
    if (y < 0)
	y = 0;
    if (y >= rows)
	y = rows - 1;
    curs.x = x;
    curs.y = y;
    fix_cpos;
    wrapnext = FALSE;
}

/*
 * Save or restore the cursor and SGR mode.
 */
static void save_cursor(int save)
{
    if (save) {
	savecurs = curs;
	save_attr = curr_attr;
	save_cset = cset;
	save_utf = utf;
	save_wnext = wrapnext;
	save_csattr = cset_attr[cset];
	save_sco_acs = sco_acs;
    } else {
	curs = savecurs;
	/* Make sure the window hasn't shrunk since the save */
	if (curs.x >= cols)
	    curs.x = cols - 1;
	if (curs.y >= rows)
	    curs.y = rows - 1;

	curr_attr = save_attr;
	cset = save_cset;
	utf = save_utf;
	wrapnext = save_wnext;
	/*
	 * wrapnext might reset to False if the x position is no
	 * longer at the rightmost edge.
	 */
	if (wrapnext && curs.x < cols-1)
	    wrapnext = FALSE;
	cset_attr[cset] = save_csattr;
	sco_acs = save_sco_acs;
	fix_cpos;
	if (use_bce)
	    erase_char = (' ' | ATTR_ASCII |
		         (curr_attr & (ATTR_FGMASK | ATTR_BGMASK)));
    }
}

/*
 * Erase a large portion of the screen: the whole screen, or the
 * whole line, or parts thereof.
 */
static void erase_lots(int line_only, int from_begin, int to_end)
{
    pos start, end;
    int erase_lattr;
    unsigned long *ldata;

    if (line_only) {
	start.y = curs.y;
	start.x = 0;
	end.y = curs.y + 1;
	end.x = 0;
	erase_lattr = FALSE;
    } else {
	start.y = 0;
	start.x = 0;
	end.y = rows;
	end.x = 0;
	erase_lattr = TRUE;
    }
    if (!from_begin) {
	start = curs;
    }
    if (!to_end) {
	end = curs;
	incpos(end);
    }
    check_selection(start, end);

    /* Clear screen also forces a full window redraw, just in case. */
    if (start.y == 0 && start.x == 0 && end.y == rows)
	term_invalidate();

    ldata = lineptr(start.y);
    while (poslt(start, end)) {
	if (start.x == cols && !erase_lattr)
	    ldata[start.x] &= ~LATTR_WRAPPED;
	else
	    ldata[start.x] = erase_char;
	if (incpos(start) && start.y < rows)
	    ldata = lineptr(start.y);
    }
}

/*
 * Insert or delete characters within the current line. n is +ve if
 * insertion is desired, and -ve for deletion.
 */
static void insch(int n)
{
    int dir = (n < 0 ? -1 : +1);
    int m;
    pos cursplus;
    unsigned long *ldata;

    n = (n < 0 ? -n : n);
    if (n > cols - curs.x)
	n = cols - curs.x;
    m = cols - curs.x - n;
    cursplus.y = curs.y;
    cursplus.x = curs.x + n;
    check_selection(curs, cursplus);
    ldata = lineptr(curs.y);
    if (dir < 0) {
	memmove(ldata + curs.x, ldata + curs.x + n, m * TSIZE);
	while (n--)
	    ldata[curs.x + m++] = erase_char;
    } else {
	memmove(ldata + curs.x + n, ldata + curs.x, m * TSIZE);
	while (n--)
	    ldata[curs.x + n] = erase_char;
    }
}

/*
 * Toggle terminal mode `mode' to state `state'. (`query' indicates
 * whether the mode is a DEC private one or a normal one.)
 */
static void toggle_mode(int mode, int query, int state)
{
    unsigned long ticks;

    if (query)
	switch (mode) {
	  case 1:		       /* application cursor keys */
	    app_cursor_keys = state;
	    break;
	  case 2:		       /* VT52 mode */
	    vt52_mode = !state;
	    if (vt52_mode) {
		blink_is_real = FALSE;
		vt52_bold = FALSE;
	    } else {
		blink_is_real = cfg.blinktext;
	    }
	    break;
	  case 3:		       /* 80/132 columns */
	    deselect();
	    if (!cfg.no_remote_resize)
		request_resize(state ? 132 : 80, rows);
	    reset_132 = state;
	    break;
	  case 5:		       /* reverse video */
	    /*
	     * Toggle reverse video. If we receive an OFF within the
	     * visual bell timeout period after an ON, we trigger an
	     * effective visual bell, so that ESC[?5hESC[?5l will
	     * always be an actually _visible_ visual bell.
	     */
	    ticks = GetTickCount();
	    /* turn off a previous vbell to avoid inconsistencies */
	    if (ticks - vbell_startpoint >= VBELL_TIMEOUT)
		in_vbell = FALSE;
	    if (rvideo && !state &&    /* we're turning it off... */
		(ticks - rvbell_startpoint) < VBELL_TIMEOUT) {	/* ...soon */
		/* If there's no vbell timeout already, or this one lasts
		 * longer, replace vbell_timeout with ours. */
		if (!in_vbell ||
		    (rvbell_startpoint - vbell_startpoint < VBELL_TIMEOUT))
		    vbell_startpoint = rvbell_startpoint;
		in_vbell = TRUE;       /* we may clear rvideo but we set in_vbell */
	    } else if (!rvideo && state) {
		/* This is an ON, so we notice the time and save it. */
		rvbell_startpoint = ticks;
	    }
	    rvideo = state;
	    seen_disp_event = TRUE;
	    if (state)
		term_update();
	    break;
	  case 6:		       /* DEC origin mode */
	    dec_om = state;
	    break;
	  case 7:		       /* auto wrap */
	    wrap = state;
	    break;
	  case 8:		       /* auto key repeat */
	    repeat_off = !state;
	    break;
	  case 10:		       /* set local edit mode */
	    term_editing = state;
	    ldisc_send(NULL, 0, 0);    /* cause ldisc to notice changes */
	    break;
	  case 25:		       /* enable/disable cursor */
	    compatibility2(OTHER, VT220);
	    cursor_on = state;
	    seen_disp_event = TRUE;
	    break;
	  case 47:		       /* alternate screen */
	    compatibility(OTHER);
	    deselect();
	    swap_screen(cfg.no_alt_screen ? 0 : state);
	    disptop = 0;
	    break;
	  case 1000:		       /* xterm mouse 1 */
	    xterm_mouse = state ? 1 : 0;
	    set_raw_mouse_mode(state);
	    break;
	  case 1002:		       /* xterm mouse 2 */
	    xterm_mouse = state ? 2 : 0;
	    set_raw_mouse_mode(state);
	    break;
    } else
	switch (mode) {
	  case 4:		       /* set insert mode */
	    compatibility(VT102);
	    insert = state;
	    break;
	  case 12:		       /* set echo mode */
	    term_echoing = !state;
	    ldisc_send(NULL, 0, 0);    /* cause ldisc to notice changes */
	    break;
	  case 20:		       /* Return sends ... */
	    cr_lf_return = state;
	    break;
	  case 34:		       /* Make cursor BIG */
	    compatibility2(OTHER, VT220);
	    big_cursor = !state;
	}
}

/*
 * Process an OSC sequence: set window title or icon name.
 */
static void do_osc(void)
{
    if (osc_w) {
	while (osc_strlen--)
	    wordness[(unsigned char) osc_string[osc_strlen]] = esc_args[0];
    } else {
	osc_string[osc_strlen] = '\0';
	switch (esc_args[0]) {
	  case 0:
	  case 1:
	    if (!cfg.no_remote_wintitle)
		set_icon(osc_string);
	    if (esc_args[0] == 1)
		break;
	    /* fall through: parameter 0 means set both */
	  case 2:
	  case 21:
	    if (!cfg.no_remote_wintitle)
		set_title(osc_string);
	    break;
	}
    }
}

/*
 * ANSI printing routines.
 */
static void term_print_setup(void)
{
    bufchain_clear(&printer_buf);
    print_job = printer_start_job(cfg.printer);
}
static void term_print_flush(void)
{
    void *data;
    int len;
    int size;
    while ((size = bufchain_size(&printer_buf)) > 5) {
	bufchain_prefix(&printer_buf, &data, &len);
	if (len > size-5)
	    len = size-5;
	printer_job_data(print_job, data, len);
	bufchain_consume(&printer_buf, len);
    }
}
static void term_print_finish(void)
{
    void *data;
    int len, size;
    char c;

    term_print_flush();
    while ((size = bufchain_size(&printer_buf)) > 0) {
	bufchain_prefix(&printer_buf, &data, &len);
	c = *(char *)data;
	if (c == '\033' || c == '\233') {
	    bufchain_consume(&printer_buf, size);
	    break;
	} else {
	    printer_job_data(print_job, &c, 1);
	    bufchain_consume(&printer_buf, 1);
	}
    }
    printer_finish_job(print_job);
    print_job = NULL;
    printing = only_printing = FALSE;
}

/*
 * Remove everything currently in `inbuf' and stick it up on the
 * in-memory display. There's a big state machine in here to
 * process escape sequences...
 */
void term_out(void)
{
    int c, unget;
    unsigned char localbuf[256], *chars;
    int nchars = 0;

    unget = -1;

    while (nchars > 0 || bufchain_size(&inbuf) > 0) {
	if (unget == -1) {
	    if (nchars == 0) {
		void *ret;
		bufchain_prefix(&inbuf, &ret, &nchars);
		if (nchars > sizeof(localbuf))
		    nchars = sizeof(localbuf);
		memcpy(localbuf, ret, nchars);
		bufchain_consume(&inbuf, nchars);
		chars = localbuf;
		assert(chars != NULL);
	    }
	    c = *chars++;
	    nchars--;

	    /*
	     * Optionally log the session traffic to a file. Useful for
	     * debugging and possibly also useful for actual logging.
	     */
	    if (cfg.logtype == LGTYP_DEBUG)
		logtraffic((unsigned char) c, LGTYP_DEBUG);
	} else {
	    c = unget;
	    unget = -1;
	}

	/* Note only VT220+ are 8-bit VT102 is seven bit, it shouldn't even
	 * be able to display 8-bit characters, but I'll let that go 'cause
	 * of i18n.
	 */

	/*
	 * If we're printing, add the character to the printer
	 * buffer.
	 */
	if (printing) {
	    char cc = c;
	    bufchain_add(&printer_buf, &c, 1);

	    /*
	     * If we're in print-only mode, we use a much simpler
	     * state machine designed only to recognise the ESC[4i
	     * termination sequence.
	     */
	    if (only_printing) {
		if (c == '\033')
		    print_state = 1;
		else if (c == (unsigned char)'\233')
		    print_state = 2;
		else if (c == '[' && print_state == 1)
		    print_state = 2;
		else if (c == '4' && print_state == 2)
		    print_state = 3;
		else if (c == 'i' && print_state == 3)
		    print_state = 4;
		else
		    print_state = 0;
		if (print_state == 4) {
		    printing = only_printing = FALSE;
		    term_print_finish();
		}
		continue;
	    }
	}

	/* First see about all those translations. */
	if (termstate == TOPLEVEL) {
	    if (in_utf)
		switch (utf_state) {
		  case 0:
		    if (c < 0x80) {
			/* UTF-8 must be stateless so we ignore iso2022. */
			if (unitab_ctrl[c] != 0xFF) 
			     c = unitab_ctrl[c];
			else c = ((unsigned char)c) | ATTR_ASCII;
			break;
		    } else if ((c & 0xe0) == 0xc0) {
			utf_size = utf_state = 1;
			utf_char = (c & 0x1f);
		    } else if ((c & 0xf0) == 0xe0) {
			utf_size = utf_state = 2;
			utf_char = (c & 0x0f);
		    } else if ((c & 0xf8) == 0xf0) {
			utf_size = utf_state = 3;
			utf_char = (c & 0x07);
		    } else if ((c & 0xfc) == 0xf8) {
			utf_size = utf_state = 4;
			utf_char = (c & 0x03);
		    } else if ((c & 0xfe) == 0xfc) {
			utf_size = utf_state = 5;
			utf_char = (c & 0x01);
		    } else {
			c = UCSERR;
			break;
		    }
		    continue;
		  case 1:
		  case 2:
		  case 3:
		  case 4:
		  case 5:
		    if ((c & 0xC0) != 0x80) {
			unget = c;
			c = UCSERR;
			utf_state = 0;
			break;
		    }
		    utf_char = (utf_char << 6) | (c & 0x3f);
		    if (--utf_state)
			continue;

		    c = utf_char;

		    /* Is somebody trying to be evil! */
		    if (c < 0x80 ||
			(c < 0x800 && utf_size >= 2) ||
			(c < 0x10000 && utf_size >= 3) ||
			(c < 0x200000 && utf_size >= 4) ||
			(c < 0x4000000 && utf_size >= 5))
			c = UCSERR;

		    /* Unicode line separator and paragraph separator are CR-LF */
		    if (c == 0x2028 || c == 0x2029)
			c = 0x85;

		    /* High controls are probably a Baaad idea too. */
		    if (c < 0xA0)
			c = 0xFFFD;

		    /* The UTF-16 surrogates are not nice either. */
		    /*       The standard give the option of decoding these: 
		     *       I don't want to! */
		    if (c >= 0xD800 && c < 0xE000)
			c = UCSERR;

		    /* ISO 10646 characters now limited to UTF-16 range. */
		    if (c > 0x10FFFF)
			c = UCSERR;

		    /* This is currently a TagPhobic application.. */
		    if (c >= 0xE0000 && c <= 0xE007F)
			continue;

		    /* U+FEFF is best seen as a null. */
		    if (c == 0xFEFF)
			continue;
		    /* But U+FFFE is an error. */
		    if (c == 0xFFFE || c == 0xFFFF)
			c = UCSERR;

		    /* Oops this is a 16bit implementation */
		    if (c >= 0x10000)
			c = 0xFFFD;
		    break;
	    }
	    /* Are we in the nasty ACS mode? Note: no sco in utf mode. */
	    else if(sco_acs && 
		    (c!='\033' && c!='\n' && c!='\r' && c!='\b'))
	    {
	       if (sco_acs == 2) c ^= 0x80;
	       c |= ATTR_SCOACS;
	    } else {
		switch (cset_attr[cset]) {
		    /* 
		     * Linedraw characters are different from 'ESC ( B'
		     * only for a small range. For ones outside that
		     * range, make sure we use the same font as well as
		     * the same encoding.
		     */
		  case ATTR_LINEDRW:
		    if (unitab_ctrl[c] != 0xFF)
			c = unitab_ctrl[c];
		    else
			c = ((unsigned char) c) | ATTR_LINEDRW;
		    break;

		  case ATTR_GBCHR:
		    /* If UK-ASCII, make the '#' a LineDraw Pound */
		    if (c == '#') {
			c = '}' | ATTR_LINEDRW;
			break;
		    }
		  /*FALLTHROUGH*/ case ATTR_ASCII:
		    if (unitab_ctrl[c] != 0xFF)
			c = unitab_ctrl[c];
		    else
			c = ((unsigned char) c) | ATTR_ASCII;
		    break;
		case ATTR_SCOACS:
		    if (c>=' ') c = ((unsigned char)c) | ATTR_SCOACS;
		    break;
		}
	    }
	}

	/* How about C1 controls ? */
	if ((c & -32) == 0x80 && termstate < DO_CTRLS && !vt52_mode &&
	    has_compat(VT220)) {
	    termstate = SEEN_ESC;
	    esc_query = FALSE;
	    c = '@' + (c & 0x1F);
	}

	/* Or the GL control. */
	if (c == '\177' && termstate < DO_CTRLS && has_compat(OTHER)) {
	    if (curs.x && !wrapnext)
		curs.x--;
	    wrapnext = FALSE;
	    fix_cpos;
	    if (!cfg.no_dbackspace)    /* destructive bksp might be disabled */
		*cpos = (' ' | curr_attr | ATTR_ASCII);
	} else
	    /* Or normal C0 controls. */
	if ((c & -32) == 0 && termstate < DO_CTRLS) {
	    switch (c) {
	      case '\005':	       /* terminal type query */
		/* Strictly speaking this is VT100 but a VT100 defaults to
		 * no response. Other terminals respond at their option.
		 *
		 * Don't put a CR in the default string as this tends to
		 * upset some weird software.
		 *
		 * An xterm returns "xterm" (5 characters)
		 */
		compatibility(ANSIMIN);
		{
		    char abuf[256], *s, *d;
		    int state = 0;
		    for (s = cfg.answerback, d = abuf; *s; s++) {
			if (state) {
			    if (*s >= 'a' && *s <= 'z')
				*d++ = (*s - ('a' - 1));
			    else if ((*s >= '@' && *s <= '_') ||
				     *s == '?' || (*s & 0x80))
				*d++ = ('@' ^ *s);
			    else if (*s == '~')
				*d++ = '^';
			    state = 0;
			} else if (*s == '^') {
			    state = 1;
			} else
			    *d++ = *s;
		    }
		    lpage_send(CP_ACP, abuf, d - abuf, 0);
		}
		break;
	      case '\007':
		{
		    struct beeptime *newbeep;
		    unsigned long ticks;

		    ticks = GetTickCount();

		    if (!beep_overloaded) {
			newbeep = smalloc(sizeof(struct beeptime));
			newbeep->ticks = ticks;
			newbeep->next = NULL;
			if (!beephead)
			    beephead = newbeep;
			else
			    beeptail->next = newbeep;
			beeptail = newbeep;
			nbeeps++;
		    }

		    /*
		     * Throw out any beeps that happened more than
		     * t seconds ago.
		     */
		    while (beephead &&
			   beephead->ticks < ticks - cfg.bellovl_t) {
			struct beeptime *tmp = beephead;
			beephead = tmp->next;
			sfree(tmp);
			if (!beephead)
			    beeptail = NULL;
			nbeeps--;
		    }

		    if (cfg.bellovl && beep_overloaded &&
			ticks - lastbeep >= (unsigned)cfg.bellovl_s) {
			/*
			 * If we're currently overloaded and the
			 * last beep was more than s seconds ago,
			 * leave overload mode.
			 */
			beep_overloaded = FALSE;
		    } else if (cfg.bellovl && !beep_overloaded &&
			       nbeeps >= cfg.bellovl_n) {
			/*
			 * Now, if we have n or more beeps
			 * remaining in the queue, go into overload
			 * mode.
			 */
			beep_overloaded = TRUE;
		    }
		    lastbeep = ticks;

		    /*
		     * Perform an actual beep if we're not overloaded.
		     */
		    if (!cfg.bellovl || !beep_overloaded) {
			beep(cfg.beep);
			if (cfg.beep == BELL_VISUAL) {
			    in_vbell = TRUE;
			    vbell_startpoint = ticks;
			    term_update();
			}
		    }
		    disptop = 0;
		}
		break;
	      case '\b':
		if (curs.x == 0 && (curs.y == 0 || wrap == 0));
		else if (curs.x == 0 && curs.y > 0)
		    curs.x = cols - 1, curs.y--;
		else if (wrapnext)
		    wrapnext = FALSE;
		else
		    curs.x--;
		fix_cpos;
		seen_disp_event = TRUE;
		break;
	      case '\016':
		compatibility(VT100);
		cset = 1;
		break;
	      case '\017':
		compatibility(VT100);
		cset = 0;
		break;
	      case '\033':
		if (vt52_mode)
		    termstate = VT52_ESC;
		else {
		    compatibility(ANSIMIN);
		    termstate = SEEN_ESC;
		    esc_query = FALSE;
		}
		break;
	      case '\r':
		curs.x = 0;
		wrapnext = FALSE;
		fix_cpos;
		seen_disp_event = TRUE;
		paste_hold = 0;
		logtraffic((unsigned char) c, LGTYP_ASCII);
		break;
	      case '\014':
		if (has_compat(SCOANSI)) {
		    move(0, 0, 0);
		    erase_lots(FALSE, FALSE, TRUE);
		    disptop = 0;
		    wrapnext = FALSE;
		    seen_disp_event = 1;
		    break;
		}
	      case '\013':
		compatibility(VT100);
	      case '\n':
		if (curs.y == marg_b)
		    scroll(marg_t, marg_b, 1, TRUE);
		else if (curs.y < rows - 1)
		    curs.y++;
		if (cfg.lfhascr)
		    curs.x = 0;
		fix_cpos;
		wrapnext = FALSE;
		seen_disp_event = 1;
		paste_hold = 0;
		logtraffic((unsigned char) c, LGTYP_ASCII);
		break;
	      case '\t':
		{
		    pos old_curs = curs;
		    unsigned long *ldata = lineptr(curs.y);

		    do {
			curs.x++;
		    } while (curs.x < cols - 1 && !tabs[curs.x]);

		    if ((ldata[cols] & LATTR_MODE) != LATTR_NORM) {
			if (curs.x >= cols / 2)
			    curs.x = cols / 2 - 1;
		    } else {
			if (curs.x >= cols)
			    curs.x = cols - 1;
		    }

		    fix_cpos;
		    check_selection(old_curs, curs);
		}
		seen_disp_event = TRUE;
		break;
	    }
	} else
	    switch (termstate) {
	      case TOPLEVEL:
		/* Only graphic characters get this far, ctrls are stripped above */
		if (wrapnext && wrap) {
		    cpos[1] |= LATTR_WRAPPED;
		    if (curs.y == marg_b)
			scroll(marg_t, marg_b, 1, TRUE);
		    else if (curs.y < rows - 1)
			curs.y++;
		    curs.x = 0;
		    fix_cpos;
		    wrapnext = FALSE;
		}
		if (insert)
		    insch(1);
		if (selstate != NO_SELECTION) {
		    pos cursplus = curs;
		    incpos(cursplus);
		    check_selection(curs, cursplus);
		}
		if ((c & CSET_MASK) == ATTR_ASCII || (c & CSET_MASK) == 0)
		    logtraffic((unsigned char) c, LGTYP_ASCII);
		{
		    extern int wcwidth(wchar_t ucs);
		    int width = 0;
		    if (DIRECT_CHAR(c))
			width = 1;
		    if (!width)
			width = wcwidth((wchar_t) c);
		    switch (width) {
		      case 2:
			*cpos++ = c | curr_attr;
			if (++curs.x == cols) {
			    *cpos |= LATTR_WRAPPED;
			    if (curs.y == marg_b)
				scroll(marg_t, marg_b, 1, TRUE);
			    else if (curs.y < rows - 1)
				curs.y++;
			    curs.x = 0;
			    fix_cpos;
			}
			*cpos++ = UCSWIDE | curr_attr;
			break;
		      case 1:
			*cpos++ = c | curr_attr;
			break;
		      default:
			continue;
		    }
		}
		curs.x++;
		if (curs.x == cols) {
		    cpos--;
		    curs.x--;
		    wrapnext = TRUE;
		    if (wrap && vt52_mode) {
			cpos[1] |= LATTR_WRAPPED;
			if (curs.y == marg_b)
			    scroll(marg_t, marg_b, 1, TRUE);
			else if (curs.y < rows - 1)
			    curs.y++;
			curs.x = 0;
			fix_cpos;
			wrapnext = FALSE;
		    }
		}
		seen_disp_event = 1;
		break;

	      case OSC_MAYBE_ST:
		/*
		 * This state is virtually identical to SEEN_ESC, with the
		 * exception that we have an OSC sequence in the pipeline,
		 * and _if_ we see a backslash, we process it.
		 */
		if (c == '\\') {
		    do_osc();
		    termstate = TOPLEVEL;
		    break;
		}
		/* else fall through */
	      case SEEN_ESC:
		if (c >= ' ' && c <= '/') {
		    if (esc_query)
			esc_query = -1;
		    else
			esc_query = c;
		    break;
		}
		termstate = TOPLEVEL;
		switch (ANSI(c, esc_query)) {
		  case '[':	       /* enter CSI mode */
		    termstate = SEEN_CSI;
		    esc_nargs = 1;
		    esc_args[0] = ARG_DEFAULT;
		    esc_query = FALSE;
		    break;
		  case ']':	       /* xterm escape sequences */
		    /* Compatibility is nasty here, xterm, linux, decterm yuk! */
		    compatibility(OTHER);
		    termstate = SEEN_OSC;
		    esc_args[0] = 0;
		    break;
		  case '7':	       /* save cursor */
		    compatibility(VT100);
		    save_cursor(TRUE);
		    break;
		  case '8':	       /* restore cursor */
		    compatibility(VT100);
		    save_cursor(FALSE);
		    seen_disp_event = TRUE;
		    break;
		  case '=':
		    compatibility(VT100);
		    app_keypad_keys = TRUE;
		    break;
		  case '>':
		    compatibility(VT100);
		    app_keypad_keys = FALSE;
		    break;
		  case 'D':	       /* exactly equivalent to LF */
		    compatibility(VT100);
		    if (curs.y == marg_b)
			scroll(marg_t, marg_b, 1, TRUE);
		    else if (curs.y < rows - 1)
			curs.y++;
		    fix_cpos;
		    wrapnext = FALSE;
		    seen_disp_event = TRUE;
		    break;
		  case 'E':	       /* exactly equivalent to CR-LF */
		    compatibility(VT100);
		    curs.x = 0;
		    if (curs.y == marg_b)
			scroll(marg_t, marg_b, 1, TRUE);
		    else if (curs.y < rows - 1)
			curs.y++;
		    fix_cpos;
		    wrapnext = FALSE;
		    seen_disp_event = TRUE;
		    break;
		  case 'M':	       /* reverse index - backwards LF */
		    compatibility(VT100);
		    if (curs.y == marg_t)
			scroll(marg_t, marg_b, -1, TRUE);
		    else if (curs.y > 0)
			curs.y--;
		    fix_cpos;
		    wrapnext = FALSE;
		    seen_disp_event = TRUE;
		    break;
		  case 'Z':	       /* terminal type query */
		    compatibility(VT100);
		    ldisc_send(id_string, strlen(id_string), 0);
		    break;
		  case 'c':	       /* restore power-on settings */
		    compatibility(VT100);
		    power_on();
		    if (reset_132) {
			if (!cfg.no_remote_resize)
			    request_resize(80, rows);
			reset_132 = 0;
		    }
		    fix_cpos;
		    disptop = 0;
		    seen_disp_event = TRUE;
		    break;
		  case 'H':	       /* set a tab */
		    compatibility(VT100);
		    tabs[curs.x] = TRUE;
		    break;

		  case ANSI('8', '#'):	/* ESC # 8 fills screen with Es :-) */
		    compatibility(VT100);
		    {
			unsigned long *ldata;
			int i, j;
			pos scrtop, scrbot;

			for (i = 0; i < rows; i++) {
			    ldata = lineptr(i);
			    for (j = 0; j < cols; j++)
				ldata[j] = ATTR_DEFAULT | 'E';
			    ldata[cols] = 0;
			}
			disptop = 0;
			seen_disp_event = TRUE;
			scrtop.x = scrtop.y = 0;
			scrbot.x = 0;
			scrbot.y = rows;
			check_selection(scrtop, scrbot);
		    }
		    break;

		  case ANSI('3', '#'):
		  case ANSI('4', '#'):
		  case ANSI('5', '#'):
		  case ANSI('6', '#'):
		    compatibility(VT100);
		    {
			unsigned long nlattr;
			unsigned long *ldata;
			switch (ANSI(c, esc_query)) {
			  case ANSI('3', '#'):
			    nlattr = LATTR_TOP;
			    break;
			  case ANSI('4', '#'):
			    nlattr = LATTR_BOT;
			    break;
			  case ANSI('5', '#'):
			    nlattr = LATTR_NORM;
			    break;
			  default: /* spiritually case ANSI('6', '#'): */
			    nlattr = LATTR_WIDE;
			    break;
			}
			ldata = lineptr(curs.y);
			ldata[cols] &= ~LATTR_MODE;
			ldata[cols] |= nlattr;
		    }
		    break;

		  case ANSI('A', '('):
		    compatibility(VT100);
		    if (!cfg.no_remote_charset)
			cset_attr[0] = ATTR_GBCHR;
		    break;
		  case ANSI('B', '('):
		    compatibility(VT100);
		    if (!cfg.no_remote_charset)
			cset_attr[0] = ATTR_ASCII;
		    break;
		  case ANSI('0', '('):
		    compatibility(VT100);
		    if (!cfg.no_remote_charset)
			cset_attr[0] = ATTR_LINEDRW;
		    break;
		  case ANSI('U', '('): 
		    compatibility(OTHER);
		    if (!cfg.no_remote_charset)
			cset_attr[0] = ATTR_SCOACS; 
		    break;

		  case ANSI('A', ')'):
		    compatibility(VT100);
		    if (!cfg.no_remote_charset)
			cset_attr[1] = ATTR_GBCHR;
		    break;
		  case ANSI('B', ')'):
		    compatibility(VT100);
		    if (!cfg.no_remote_charset)
			cset_attr[1] = ATTR_ASCII;
		    break;
		  case ANSI('0', ')'):
		    compatibility(VT100);
		    if (!cfg.no_remote_charset)
			cset_attr[1] = ATTR_LINEDRW;
		    break;
		  case ANSI('U', ')'): 
		    compatibility(OTHER);
		    if (!cfg.no_remote_charset)
			cset_attr[1] = ATTR_SCOACS; 
		    break;

		  case ANSI('8', '%'):	/* Old Linux code */
		  case ANSI('G', '%'):
		    compatibility(OTHER);
		    if (!cfg.no_remote_charset)
			utf = 1;
		    break;
		  case ANSI('@', '%'):
		    compatibility(OTHER);
		    if (!cfg.no_remote_charset)
			utf = 0;
		    break;
		}
		break;
	      case SEEN_CSI:
		termstate = TOPLEVEL;  /* default */
		if (isdigit(c)) {
		    if (esc_nargs <= ARGS_MAX) {
			if (esc_args[esc_nargs - 1] == ARG_DEFAULT)
			    esc_args[esc_nargs - 1] = 0;
			esc_args[esc_nargs - 1] =
			    10 * esc_args[esc_nargs - 1] + c - '0';
		    }
		    termstate = SEEN_CSI;
		} else if (c == ';') {
		    if (++esc_nargs <= ARGS_MAX)
			esc_args[esc_nargs - 1] = ARG_DEFAULT;
		    termstate = SEEN_CSI;
		} else if (c < '@') {
		    if (esc_query)
			esc_query = -1;
		    else if (c == '?')
			esc_query = TRUE;
		    else
			esc_query = c;
		    termstate = SEEN_CSI;
		} else
		    switch (ANSI(c, esc_query)) {
		      case 'A':       /* move up N lines */
			move(curs.x, curs.y - def(esc_args[0], 1), 1);
			seen_disp_event = TRUE;
			break;
		      case 'e':       /* move down N lines */
			compatibility(ANSI);
			/* FALLTHROUGH */
		      case 'B':
			move(curs.x, curs.y + def(esc_args[0], 1), 1);
			seen_disp_event = TRUE;
			break;
		      case ANSI('c', '>'):	/* report xterm version */
			compatibility(OTHER);
			/* this reports xterm version 136 so that VIM can
			   use the drag messages from the mouse reporting */
			ldisc_send("\033[>0;136;0c", 11, 0);
			break;
		      case 'a':       /* move right N cols */
			compatibility(ANSI);
			/* FALLTHROUGH */
		      case 'C':
			move(curs.x + def(esc_args[0], 1), curs.y, 1);
			seen_disp_event = TRUE;
			break;
		      case 'D':       /* move left N cols */
			move(curs.x - def(esc_args[0], 1), curs.y, 1);
			seen_disp_event = TRUE;
			break;
		      case 'E':       /* move down N lines and CR */
			compatibility(ANSI);
			move(0, curs.y + def(esc_args[0], 1), 1);
			seen_disp_event = TRUE;
			break;
		      case 'F':       /* move up N lines and CR */
			compatibility(ANSI);
			move(0, curs.y - def(esc_args[0], 1), 1);
			seen_disp_event = TRUE;
			break;
		      case 'G':
		      case '`':       /* set horizontal posn */
			compatibility(ANSI);
			move(def(esc_args[0], 1) - 1, curs.y, 0);
			seen_disp_event = TRUE;
			break;
		      case 'd':       /* set vertical posn */
			compatibility(ANSI);
			move(curs.x,
			     (dec_om ? marg_t : 0) + def(esc_args[0],
							 1) - 1,
			     (dec_om ? 2 : 0));
			seen_disp_event = TRUE;
			break;
		      case 'H':
		      case 'f':       /* set horz and vert posns at once */
			if (esc_nargs < 2)
			    esc_args[1] = ARG_DEFAULT;
			move(def(esc_args[1], 1) - 1,
			     (dec_om ? marg_t : 0) + def(esc_args[0],
							 1) - 1,
			     (dec_om ? 2 : 0));
			seen_disp_event = TRUE;
			break;
		      case 'J':       /* erase screen or parts of it */
			{
			    unsigned int i = def(esc_args[0], 0) + 1;
			    if (i > 3)
				i = 0;
			    erase_lots(FALSE, !!(i & 2), !!(i & 1));
			}
			disptop = 0;
			seen_disp_event = TRUE;
			break;
		      case 'K':       /* erase line or parts of it */
			{
			    unsigned int i = def(esc_args[0], 0) + 1;
			    if (i > 3)
				i = 0;
			    erase_lots(TRUE, !!(i & 2), !!(i & 1));
			}
			seen_disp_event = TRUE;
			break;
		      case 'L':       /* insert lines */
			compatibility(VT102);
			if (curs.y <= marg_b)
			    scroll(curs.y, marg_b, -def(esc_args[0], 1),
				   FALSE);
			fix_cpos;
			seen_disp_event = TRUE;
			break;
		      case 'M':       /* delete lines */
			compatibility(VT102);
			if (curs.y <= marg_b)
			    scroll(curs.y, marg_b, def(esc_args[0], 1),
				   TRUE);
			fix_cpos;
			seen_disp_event = TRUE;
			break;
		      case '@':       /* insert chars */
			/* XXX VTTEST says this is vt220, vt510 manual says vt102 */
			compatibility(VT102);
			insch(def(esc_args[0], 1));
			seen_disp_event = TRUE;
			break;
		      case 'P':       /* delete chars */
			compatibility(VT102);
			insch(-def(esc_args[0], 1));
			seen_disp_event = TRUE;
			break;
		      case 'c':       /* terminal type query */
			compatibility(VT100);
			/* This is the response for a VT102 */
			ldisc_send(id_string, strlen(id_string), 0);
			break;
		      case 'n':       /* cursor position query */
			if (esc_args[0] == 6) {
			    char buf[32];
			    sprintf(buf, "\033[%d;%dR", curs.y + 1,
				    curs.x + 1);
			    ldisc_send(buf, strlen(buf), 0);
			} else if (esc_args[0] == 5) {
			    ldisc_send("\033[0n", 4, 0);
			}
			break;
		      case 'h':       /* toggle modes to high */
		      case ANSI_QUE('h'):
			compatibility(VT100);
			{
			    int i;
			    for (i = 0; i < esc_nargs; i++)
				toggle_mode(esc_args[i], esc_query, TRUE);
			}
			break;
		      case 'i':
		      case ANSI_QUE('i'):
			compatibility(VT100);
			{
			    int i;
			    if (esc_nargs != 1) break;
			    if (esc_args[0] == 5 && *cfg.printer) {
				printing = TRUE;
				only_printing = !esc_query;
				print_state = 0;
				term_print_setup();
			    } else if (esc_args[0] == 4 && printing) {
				printing = FALSE;
				only_printing = FALSE;
				term_print_finish();
			    }
			}
			break;			
		      case 'l':       /* toggle modes to low */
		      case ANSI_QUE('l'):
			compatibility(VT100);
			{
			    int i;
			    for (i = 0; i < esc_nargs; i++)
				toggle_mode(esc_args[i], esc_query, FALSE);
			}
			break;
		      case 'g':       /* clear tabs */
			compatibility(VT100);
			if (esc_nargs == 1) {
			    if (esc_args[0] == 0) {
				tabs[curs.x] = FALSE;
			    } else if (esc_args[0] == 3) {
				int i;
				for (i = 0; i < cols; i++)
				    tabs[i] = FALSE;
			    }
			}
			break;
		      case 'r':       /* set scroll margins */
			compatibility(VT100);
			if (esc_nargs <= 2) {
			    int top, bot;
			    top = def(esc_args[0], 1) - 1;
			    bot = (esc_nargs <= 1
				   || esc_args[1] ==
				   0 ? rows : def(esc_args[1], rows)) - 1;
			    if (bot >= rows)
				bot = rows - 1;
			    /* VTTEST Bug 9 - if region is less than 2 lines
			     * don't change region.
			     */
			    if (bot - top > 0) {
				marg_t = top;
				marg_b = bot;
				curs.x = 0;
				/*
				 * I used to think the cursor should be
				 * placed at the top of the newly marginned
				 * area. Apparently not: VMS TPU falls over
				 * if so.
				 *
				 * Well actually it should for Origin mode - RDB
				 */
				curs.y = (dec_om ? marg_t : 0);
				fix_cpos;
				seen_disp_event = TRUE;
			    }
			}
			break;
		      case 'm':       /* set graphics rendition */
			{
			    /* 
			     * A VT100 without the AVO only had one attribute, either
			     * underline or reverse video depending on the cursor type,
			     * this was selected by CSI 7m.
			     *
			     * case 2:
			     *  This is sometimes DIM, eg on the GIGI and Linux
			     * case 8:
			     *  This is sometimes INVIS various ANSI.
			     * case 21:
			     *  This like 22 disables BOLD, DIM and INVIS
			     *
			     * The ANSI colours appear on any terminal that has colour
			     * (obviously) but the interaction between sgr0 and the
			     * colours varies but is usually related to the background
			     * colour erase item.
			     * The interaction between colour attributes and the mono
			     * ones is also very implementation dependent.
			     *
			     * The 39 and 49 attributes are likely to be unimplemented.
			     */
			    int i;
			    for (i = 0; i < esc_nargs; i++) {
				switch (def(esc_args[i], 0)) {
				  case 0:	/* restore defaults */
				    curr_attr = ATTR_DEFAULT;
				    break;
				  case 1:	/* enable bold */
				    compatibility(VT100AVO);
				    curr_attr |= ATTR_BOLD;
				    break;
				  case 21:	/* (enable double underline) */
				    compatibility(OTHER);
				  case 4:	/* enable underline */
				    compatibility(VT100AVO);
				    curr_attr |= ATTR_UNDER;
				    break;
				  case 5:	/* enable blink */
				    compatibility(VT100AVO);
				    curr_attr |= ATTR_BLINK;
				    break;
				  case 7:	/* enable reverse video */
				    curr_attr |= ATTR_REVERSE;
				    break;
				  case 10:      /* SCO acs off */
				    compatibility(SCOANSI);
				    if (cfg.no_remote_charset) break;
				    sco_acs = 0; break;
				  case 11:      /* SCO acs on */
				    compatibility(SCOANSI);
				    if (cfg.no_remote_charset) break;
				    sco_acs = 1; break;
				  case 12:      /* SCO acs on flipped */
				    compatibility(SCOANSI);
				    if (cfg.no_remote_charset) break;
				    sco_acs = 2; break;
				  case 22:	/* disable bold */
				    compatibility2(OTHER, VT220);
				    curr_attr &= ~ATTR_BOLD;
				    break;
				  case 24:	/* disable underline */
				    compatibility2(OTHER, VT220);
				    curr_attr &= ~ATTR_UNDER;
				    break;
				  case 25:	/* disable blink */
				    compatibility2(OTHER, VT220);
				    curr_attr &= ~ATTR_BLINK;
				    break;
				  case 27:	/* disable reverse video */
				    compatibility2(OTHER, VT220);
				    curr_attr &= ~ATTR_REVERSE;
				    break;
				  case 30:
				  case 31:
				  case 32:
				  case 33:
				  case 34:
				  case 35:
				  case 36:
				  case 37:
				    /* foreground */
				    curr_attr &= ~ATTR_FGMASK;
				    curr_attr |=
					(esc_args[i] - 30) << ATTR_FGSHIFT;
				    break;
				  case 39:	/* default-foreground */
				    curr_attr &= ~ATTR_FGMASK;
				    curr_attr |= ATTR_DEFFG;
				    break;
				  case 40:
				  case 41:
				  case 42:
				  case 43:
				  case 44:
				  case 45:
				  case 46:
				  case 47:
				    /* background */
				    curr_attr &= ~ATTR_BGMASK;
				    curr_attr |=
					(esc_args[i] - 40) << ATTR_BGSHIFT;
				    break;
				  case 49:	/* default-background */
				    curr_attr &= ~ATTR_BGMASK;
				    curr_attr |= ATTR_DEFBG;
				    break;
				}
			    }
			    if (use_bce)
				erase_char = (' ' | ATTR_ASCII |
					     (curr_attr & 
					      (ATTR_FGMASK | ATTR_BGMASK)));
			}
			break;
		      case 's':       /* save cursor */
			save_cursor(TRUE);
			break;
		      case 'u':       /* restore cursor */
			save_cursor(FALSE);
			seen_disp_event = TRUE;
			break;
		      case 't':       /* set page size - ie window height */
			/*
			 * VT340/VT420 sequence DECSLPP, DEC only allows values
			 *  24/25/36/48/72/144 other emulators (eg dtterm) use
			 * illegal values (eg first arg 1..9) for window changing 
			 * and reports.
			 */
			if (esc_nargs <= 1
			    && (esc_args[0] < 1 || esc_args[0] >= 24)) {
			    compatibility(VT340TEXT);
			    if (!cfg.no_remote_resize)
				request_resize(cols, def(esc_args[0], 24));
			    deselect();
			} else if (esc_nargs >= 1 &&
				   esc_args[0] >= 1 &&
				   esc_args[0] < 24) {
			    compatibility(OTHER);

			    switch (esc_args[0]) {
				int x, y, len;
				char buf[80], *p;
			      case 1:
				set_iconic(FALSE);
				break;
			      case 2:
				set_iconic(TRUE);
				break;
			      case 3:
				if (esc_nargs >= 3) {
				    if (!cfg.no_remote_resize)
					move_window(def(esc_args[1], 0),
						    def(esc_args[2], 0));
				}
				break;
			      case 4:
				/* We should resize the window to a given
				 * size in pixels here, but currently our
				 * resizing code isn't healthy enough to
				 * manage it. */
				break;
			      case 5:
				set_zorder(TRUE);   /* move to top */
				break;
			      case 6:
				set_zorder(FALSE);  /* move to bottom */
				break;
			      case 7:
				refresh_window();
				break;
			      case 8:
				if (esc_nargs >= 3) {
				    if (!cfg.no_remote_resize)
					request_resize(def(esc_args[2], cfg.width),
						       def(esc_args[1], cfg.height));
				}
				break;
			      case 9:
				if (esc_nargs >= 2)
				    set_zoomed(esc_args[1] ? TRUE : FALSE);
				break;
			      case 11:
				ldisc_send(is_iconic() ? "\033[1t" : "\033[2t",
					   4, 0);
				break;
			      case 13:
				get_window_pos(&x, &y);
				len = sprintf(buf, "\033[3;%d;%dt", x, y);
				ldisc_send(buf, len, 0);
				break;
			      case 14:
				get_window_pixels(&x, &y);
				len = sprintf(buf, "\033[4;%d;%dt", x, y);
				ldisc_send(buf, len, 0);
				break;
			      case 18:
				len = sprintf(buf, "\033[8;%d;%dt",
					      rows, cols);
				ldisc_send(buf, len, 0);
				break;
			      case 19:
				/*
				 * Hmmm. Strictly speaking we
				 * should return `the size of the
				 * screen in characters', but
				 * that's not easy: (a) window
				 * furniture being what it is it's
				 * hard to compute, and (b) in
				 * resize-font mode maximising the
				 * window wouldn't change the
				 * number of characters. *shrug*. I
				 * think we'll ignore it for the
				 * moment and see if anyone
				 * complains, and then ask them
				 * what they would like it to do.
				 */
				break;
			      case 20:
				p = get_window_title(TRUE);
				len = strlen(p);
				ldisc_send("\033]L", 3, 0);
				ldisc_send(p, len, 0);
				ldisc_send("\033\\", 2, 0);
				break;
			      case 21:
				p = get_window_title(FALSE);
				len = strlen(p);
				ldisc_send("\033]l", 3, 0);
				ldisc_send(p, len, 0);
				ldisc_send("\033\\", 2, 0);
				break;
			    }
			}
			break;
		      case 'S':
			compatibility(SCOANSI);
			scroll(marg_t, marg_b, def(esc_args[0], 1), TRUE);
			fix_cpos;
			wrapnext = FALSE;
			seen_disp_event = TRUE;
			break;
		      case 'T':
			compatibility(SCOANSI);
			scroll(marg_t, marg_b, -def(esc_args[0], 1), TRUE);
			fix_cpos;
			wrapnext = FALSE;
			seen_disp_event = TRUE;
			break;
		      case ANSI('|', '*'):
			/* VT420 sequence DECSNLS
			 * Set number of lines on screen
			 * VT420 uses VGA like hardware and can support any size in
			 * reasonable range (24..49 AIUI) with no default specified.
			 */
			compatibility(VT420);
			if (esc_nargs == 1 && esc_args[0] > 0) {
			    if (!cfg.no_remote_resize)
				request_resize(cols, def(esc_args[0], cfg.height));
			    deselect();
			}
			break;
		      case ANSI('|', '$'):
			/* VT340/VT420 sequence DECSCPP
			 * Set number of columns per page
			 * Docs imply range is only 80 or 132, but I'll allow any.
			 */
			compatibility(VT340TEXT);
			if (esc_nargs <= 1) {
			    if (!cfg.no_remote_resize)
				request_resize(def(esc_args[0], cfg.width), rows);
			    deselect();
			}
			break;
		      case 'X':       /* write N spaces w/o moving cursor */
			/* XXX VTTEST says this is vt220, vt510 manual says vt100 */
			compatibility(ANSIMIN);
			{
			    int n = def(esc_args[0], 1);
			    pos cursplus;
			    unsigned long *p = cpos;
			    if (n > cols - curs.x)
				n = cols - curs.x;
			    cursplus = curs;
			    cursplus.x += n;
			    check_selection(curs, cursplus);
			    while (n--)
				*p++ = erase_char;
			    seen_disp_event = TRUE;
			}
			break;
		      case 'x':       /* report terminal characteristics */
			compatibility(VT100);
			{
			    char buf[32];
			    int i = def(esc_args[0], 0);
			    if (i == 0 || i == 1) {
				strcpy(buf, "\033[2;1;1;112;112;1;0x");
				buf[2] += i;
				ldisc_send(buf, 20, 0);
			    }
			}
			break;
		      case 'Z':		/* BackTab for xterm */
			compatibility(OTHER);
			{
			    int i = def(esc_args[0], 1);
			    pos old_curs = curs;

			    for(;i>0 && curs.x>0; i--) {
				do {
				    curs.x--;
				} while (curs.x >0 && !tabs[curs.x]);
			    }
			    fix_cpos;
			    check_selection(old_curs, curs);
			}
			break;
		      case ANSI('L', '='):
			compatibility(OTHER);
			use_bce = (esc_args[0] <= 0);
			erase_char = ERASE_CHAR;
			if (use_bce)
			    erase_char = (' ' | ATTR_ASCII |
					 (curr_attr & 
					  (ATTR_FGMASK | ATTR_BGMASK)));
			break;
		      case ANSI('E', '='):
			compatibility(OTHER);
			blink_is_real = (esc_args[0] >= 1);
			break;
		      case ANSI('p', '"'):
			/* Allow the host to make this emulator a 'perfect' VT102.
			 * This first appeared in the VT220, but we do need to get 
			 * back to PuTTY mode so I won't check it.
			 *
			 * The arg in 40..42,50 are a PuTTY extension.
			 * The 2nd arg, 8bit vs 7bit is not checked.
			 *
			 * Setting VT102 mode should also change the Fkeys to
			 * generate PF* codes as a real VT102 has no Fkeys.
			 * The VT220 does this, F11..F13 become ESC,BS,LF other Fkeys
			 * send nothing.
			 *
			 * Note ESC c will NOT change this!
			 */

			switch (esc_args[0]) {
			  case 61:
			    compatibility_level &= ~TM_VTXXX;
			    compatibility_level |= TM_VT102;
			    break;
			  case 62:
			    compatibility_level &= ~TM_VTXXX;
			    compatibility_level |= TM_VT220;
			    break;

			  default:
			    if (esc_args[0] > 60 && esc_args[0] < 70)
				compatibility_level |= TM_VTXXX;
			    break;

			  case 40:
			    compatibility_level &= TM_VTXXX;
			    break;
			  case 41:
			    compatibility_level = TM_PUTTY;
			    break;
			  case 42:
			    compatibility_level = TM_SCOANSI;
			    break;

			  case ARG_DEFAULT:
			    compatibility_level = TM_PUTTY;
			    break;
			  case 50:
			    break;
			}

			/* Change the response to CSI c */
			if (esc_args[0] == 50) {
			    int i;
			    char lbuf[64];
			    strcpy(id_string, "\033[?");
			    for (i = 1; i < esc_nargs; i++) {
				if (i != 1)
				    strcat(id_string, ";");
				sprintf(lbuf, "%d", esc_args[i]);
				strcat(id_string, lbuf);
			    }
			    strcat(id_string, "c");
			}
#if 0
			/* Is this a good idea ? 
			 * Well we should do a soft reset at this point ...
			 */
			if (!has_compat(VT420) && has_compat(VT100)) {
			    if (!cfg.no_remote_resize) {
				if (reset_132)
				    request_resize(132, 24);
				else
				    request_resize(80, 24);
			    }
			}
#endif
			break;
		    }
		break;
	      case SEEN_OSC:
		osc_w = FALSE;
		switch (c) {
		  case 'P':	       /* Linux palette sequence */
		    termstate = SEEN_OSC_P;
		    osc_strlen = 0;
		    break;
		  case 'R':	       /* Linux palette reset */
		    palette_reset();
		    term_invalidate();
		    termstate = TOPLEVEL;
		    break;
		  case 'W':	       /* word-set */
		    termstate = SEEN_OSC_W;
		    osc_w = TRUE;
		    break;
		  case '0':
		  case '1':
		  case '2':
		  case '3':
		  case '4':
		  case '5':
		  case '6':
		  case '7':
		  case '8':
		  case '9':
		    esc_args[0] = 10 * esc_args[0] + c - '0';
		    break;
		  case 'L':
		    /*
		     * Grotty hack to support xterm and DECterm title
		     * sequences concurrently.
		     */
		    if (esc_args[0] == 2) {
			esc_args[0] = 1;
			break;
		    }
		    /* else fall through */
		  default:
		    termstate = OSC_STRING;
		    osc_strlen = 0;
		}
		break;
	      case OSC_STRING:
		/*
		 * This OSC stuff is EVIL. It takes just one character to get into
		 * sysline mode and it's not initially obvious how to get out.
		 * So I've added CR and LF as string aborts.
		 * This shouldn't effect compatibility as I believe embedded 
		 * control characters are supposed to be interpreted (maybe?) 
		 * and they don't display anything useful anyway.
		 *
		 * -- RDB
		 */
		if (c == '\n' || c == '\r') {
		    termstate = TOPLEVEL;
		} else if (c == 0234 || c == '\007') {
		    /*
		     * These characters terminate the string; ST and BEL
		     * terminate the sequence and trigger instant
		     * processing of it, whereas ESC goes back to SEEN_ESC
		     * mode unless it is followed by \, in which case it is
		     * synonymous with ST in the first place.
		     */
		    do_osc();
		    termstate = TOPLEVEL;
		} else if (c == '\033')
		    termstate = OSC_MAYBE_ST;
		else if (osc_strlen < OSC_STR_MAX)
		    osc_string[osc_strlen++] = c;
		break;
	      case SEEN_OSC_P:
		{
		    int max = (osc_strlen == 0 ? 21 : 16);
		    int val;
		    if (c >= '0' && c <= '9')
			val = c - '0';
		    else if (c >= 'A' && c <= 'A' + max - 10)
			val = c - 'A' + 10;
		    else if (c >= 'a' && c <= 'a' + max - 10)
			val = c - 'a' + 10;
		    else {
			termstate = TOPLEVEL;
			break;
		    }
		    osc_string[osc_strlen++] = val;
		    if (osc_strlen >= 7) {
			palette_set(osc_string[0],
				    osc_string[1] * 16 + osc_string[2],
				    osc_string[3] * 16 + osc_string[4],
				    osc_string[5] * 16 + osc_string[6]);
			term_invalidate();
			termstate = TOPLEVEL;
		    }
		}
		break;
	      case SEEN_OSC_W:
		switch (c) {
		  case '0':
		  case '1':
		  case '2':
		  case '3':
		  case '4':
		  case '5':
		  case '6':
		  case '7':
		  case '8':
		  case '9':
		    esc_args[0] = 10 * esc_args[0] + c - '0';
		    break;
		  default:
		    termstate = OSC_STRING;
		    osc_strlen = 0;
		}
		break;
	      case VT52_ESC:
		termstate = TOPLEVEL;
		seen_disp_event = TRUE;
		switch (c) {
		  case 'A':
		    move(curs.x, curs.y - 1, 1);
		    break;
		  case 'B':
		    move(curs.x, curs.y + 1, 1);
		    break;
		  case 'C':
		    move(curs.x + 1, curs.y, 1);
		    break;
		  case 'D':
		    move(curs.x - 1, curs.y, 1);
		    break;
		    /*
		     * From the VT100 Manual
		     * NOTE: The special graphics characters in the VT100
		     *       are different from those in the VT52
		     *
		     * From VT102 manual:
		     *       137 _  Blank             - Same
		     *       140 `  Reserved          - Humm.
		     *       141 a  Solid rectangle   - Similar
		     *       142 b  1/                - Top half of fraction for the
		     *       143 c  3/                - subscript numbers below.
		     *       144 d  5/
		     *       145 e  7/
		     *       146 f  Degrees           - Same
		     *       147 g  Plus or minus     - Same
		     *       150 h  Right arrow
		     *       151 i  Ellipsis (dots)
		     *       152 j  Divide by
		     *       153 k  Down arrow
		     *       154 l  Bar at scan 0
		     *       155 m  Bar at scan 1
		     *       156 n  Bar at scan 2
		     *       157 o  Bar at scan 3     - Similar
		     *       160 p  Bar at scan 4     - Similar
		     *       161 q  Bar at scan 5     - Similar
		     *       162 r  Bar at scan 6     - Same
		     *       163 s  Bar at scan 7     - Similar
		     *       164 t  Subscript 0
		     *       165 u  Subscript 1
		     *       166 v  Subscript 2
		     *       167 w  Subscript 3
		     *       170 x  Subscript 4
		     *       171 y  Subscript 5
		     *       172 z  Subscript 6
		     *       173 {  Subscript 7
		     *       174 |  Subscript 8
		     *       175 }  Subscript 9
		     *       176 ~  Paragraph
		     *
		     */
		  case 'F':
		    cset_attr[cset = 0] = ATTR_LINEDRW;
		    break;
		  case 'G':
		    cset_attr[cset = 0] = ATTR_ASCII;
		    break;
		  case 'H':
		    move(0, 0, 0);
		    break;
		  case 'I':
		    if (curs.y == 0)
			scroll(0, rows - 1, -1, TRUE);
		    else if (curs.y > 0)
			curs.y--;
		    fix_cpos;
		    wrapnext = FALSE;
		    break;
		  case 'J':
		    erase_lots(FALSE, FALSE, TRUE);
		    disptop = 0;
		    break;
		  case 'K':
		    erase_lots(TRUE, FALSE, TRUE);
		    break;
#if 0
		  case 'V':
		    /* XXX Print cursor line */
		    break;
		  case 'W':
		    /* XXX Start controller mode */
		    break;
		  case 'X':
		    /* XXX Stop controller mode */
		    break;
#endif
		  case 'Y':
		    termstate = VT52_Y1;
		    break;
		  case 'Z':
		    ldisc_send("\033/Z", 3, 0);
		    break;
		  case '=':
		    app_keypad_keys = TRUE;
		    break;
		  case '>':
		    app_keypad_keys = FALSE;
		    break;
		  case '<':
		    /* XXX This should switch to VT100 mode not current or default
		     *     VT mode. But this will only have effect in a VT220+
		     *     emulation.
		     */
		    vt52_mode = FALSE;
		    blink_is_real = cfg.blinktext;
		    break;
#if 0
		  case '^':
		    /* XXX Enter auto print mode */
		    break;
		  case '_':
		    /* XXX Exit auto print mode */
		    break;
		  case ']':
		    /* XXX Print screen */
		    break;
#endif

#ifdef VT52_PLUS
		  case 'E':
		    /* compatibility(ATARI) */
		    move(0, 0, 0);
		    erase_lots(FALSE, FALSE, TRUE);
		    disptop = 0;
		    break;
		  case 'L':
		    /* compatibility(ATARI) */
		    if (curs.y <= marg_b)
			scroll(curs.y, marg_b, -1, FALSE);
		    break;
		  case 'M':
		    /* compatibility(ATARI) */
		    if (curs.y <= marg_b)
			scroll(curs.y, marg_b, 1, TRUE);
		    break;
		  case 'b':
		    /* compatibility(ATARI) */
		    termstate = VT52_FG;
		    break;
		  case 'c':
		    /* compatibility(ATARI) */
		    termstate = VT52_BG;
		    break;
		  case 'd':
		    /* compatibility(ATARI) */
		    erase_lots(FALSE, TRUE, FALSE);
		    disptop = 0;
		    break;
		  case 'e':
		    /* compatibility(ATARI) */
		    cursor_on = TRUE;
		    break;
		  case 'f':
		    /* compatibility(ATARI) */
		    cursor_on = FALSE;
		    break;
		    /* case 'j': Save cursor position - broken on ST */
		    /* case 'k': Restore cursor position */
		  case 'l':
		    /* compatibility(ATARI) */
		    erase_lots(TRUE, TRUE, TRUE);
		    curs.x = 0;
		    wrapnext = FALSE;
		    fix_cpos;
		    break;
		  case 'o':
		    /* compatibility(ATARI) */
		    erase_lots(TRUE, TRUE, FALSE);
		    break;
		  case 'p':
		    /* compatibility(ATARI) */
		    curr_attr |= ATTR_REVERSE;
		    break;
		  case 'q':
		    /* compatibility(ATARI) */
		    curr_attr &= ~ATTR_REVERSE;
		    break;
		  case 'v':	       /* wrap Autowrap on - Wyse style */
		    /* compatibility(ATARI) */
		    wrap = 1;
		    break;
		  case 'w':	       /* Autowrap off */
		    /* compatibility(ATARI) */
		    wrap = 0;
		    break;

		  case 'R':
		    /* compatibility(OTHER) */
		    vt52_bold = FALSE;
		    curr_attr = ATTR_DEFAULT;
		    if (use_bce)
			erase_char = (' ' | ATTR_ASCII |
				     (curr_attr & 
				      (ATTR_FGMASK | ATTR_BGMASK)));
		    break;
		  case 'S':
		    /* compatibility(VI50) */
		    curr_attr |= ATTR_UNDER;
		    break;
		  case 'W':
		    /* compatibility(VI50) */
		    curr_attr &= ~ATTR_UNDER;
		    break;
		  case 'U':
		    /* compatibility(VI50) */
		    vt52_bold = TRUE;
		    curr_attr |= ATTR_BOLD;
		    break;
		  case 'T':
		    /* compatibility(VI50) */
		    vt52_bold = FALSE;
		    curr_attr &= ~ATTR_BOLD;
		    break;
#endif
		}
		break;
	      case VT52_Y1:
		termstate = VT52_Y2;
		move(curs.x, c - ' ', 0);
		break;
	      case VT52_Y2:
		termstate = TOPLEVEL;
		move(c - ' ', curs.y, 0);
		break;

#ifdef VT52_PLUS
	      case VT52_FG:
		termstate = TOPLEVEL;
		curr_attr &= ~ATTR_FGMASK;
		curr_attr &= ~ATTR_BOLD;
		curr_attr |= (c & 0x7) << ATTR_FGSHIFT;
		if ((c & 0x8) || vt52_bold)
		    curr_attr |= ATTR_BOLD;

		if (use_bce)
		    erase_char = (' ' | ATTR_ASCII |
				 (curr_attr & (ATTR_FGMASK | ATTR_BGMASK)));
		break;
	      case VT52_BG:
		termstate = TOPLEVEL;
		curr_attr &= ~ATTR_BGMASK;
		curr_attr &= ~ATTR_BLINK;
		curr_attr |= (c & 0x7) << ATTR_BGSHIFT;

		/* Note: bold background */
		if (c & 0x8)
		    curr_attr |= ATTR_BLINK;

		if (use_bce)
		    erase_char = (' ' | ATTR_ASCII |
				 (curr_attr & (ATTR_FGMASK | ATTR_BGMASK)));
		break;
#endif
	      default: break;	       /* placate gcc warning about enum use */
	    }
	if (selstate != NO_SELECTION) {
	    pos cursplus = curs;
	    incpos(cursplus);
	    check_selection(curs, cursplus);
	}
    }

    term_print_flush();
}

#if 0
/*
 * Compare two lines to determine whether they are sufficiently
 * alike to scroll-optimise one to the other. Return the degree of
 * similarity.
 */
static int linecmp(unsigned long *a, unsigned long *b)
{
    int i, n;

    for (i = n = 0; i < cols; i++)
	n += (*a++ == *b++);
    return n;
}
#endif

/*
 * Given a context, update the window. Out of paranoia, we don't
 * allow WM_PAINT responses to do scrolling optimisations.
 */
static void do_paint(Context ctx, int may_optimise)
{
    int i, j, our_curs_y;
    unsigned long rv, cursor;
    pos scrpos;
    char ch[1024];
    long cursor_background = ERASE_CHAR;
    unsigned long ticks;

    /*
     * Check the visual bell state.
     */
    if (in_vbell) {
	ticks = GetTickCount();
	if (ticks - vbell_startpoint >= VBELL_TIMEOUT)
	    in_vbell = FALSE; 
   }

    rv = (!rvideo ^ !in_vbell ? ATTR_REVERSE : 0);

    /* Depends on:
     * screen array, disptop, scrtop,
     * selection, rv, 
     * cfg.blinkpc, blink_is_real, tblinker, 
     * curs.y, curs.x, blinker, cfg.blink_cur, cursor_on, has_focus, wrapnext
     */

    /* Has the cursor position or type changed ? */
    if (cursor_on) {
	if (has_focus) {
	    if (blinker || !cfg.blink_cur)
		cursor = TATTR_ACTCURS;
	    else
		cursor = 0;
	} else
	    cursor = TATTR_PASCURS;
	if (wrapnext)
	    cursor |= TATTR_RIGHTCURS;
    } else
	cursor = 0;
    our_curs_y = curs.y - disptop;

    if (dispcurs && (curstype != cursor ||
		     dispcurs !=
		     disptext + our_curs_y * (cols + 1) + curs.x)) {
	if (dispcurs > disptext && 
		(*dispcurs & (CHAR_MASK | CSET_MASK)) == UCSWIDE)
	    dispcurs[-1] |= ATTR_INVALID;
	if ( (dispcurs[1] & (CHAR_MASK | CSET_MASK)) == UCSWIDE)
	    dispcurs[1] |= ATTR_INVALID;
	*dispcurs |= ATTR_INVALID;
	curstype = 0;
    }
    dispcurs = NULL;

    /* The normal screen data */
    for (i = 0; i < rows; i++) {
	unsigned long *ldata;
	int lattr;
	int idx, dirty_line, dirty_run, selected;
	unsigned long attr = 0;
	int updated_line = 0;
	int start = 0;
	int ccount = 0;
	int last_run_dirty = 0;

	scrpos.y = i + disptop;
	ldata = lineptr(scrpos.y);
	lattr = (ldata[cols] & LATTR_MODE);

	idx = i * (cols + 1);
	dirty_run = dirty_line = (ldata[cols] != disptext[idx + cols]);
	disptext[idx + cols] = ldata[cols];

	for (j = 0; j < cols; j++, idx++) {
	    unsigned long tattr, tchar;
	    unsigned long *d = ldata + j;
	    int break_run;
	    scrpos.x = j;

	    tchar = (*d & (CHAR_MASK | CSET_MASK));
	    tattr = (*d & (ATTR_MASK ^ CSET_MASK));
	    switch (tchar & CSET_MASK) {
	      case ATTR_ASCII:
		tchar = unitab_line[tchar & 0xFF];
		break;
	      case ATTR_LINEDRW:
		tchar = unitab_xterm[tchar & 0xFF];
		break;
	      case ATTR_SCOACS:  
		tchar = unitab_scoacs[tchar&0xFF]; 
		break;
	    }
	    tattr |= (tchar & CSET_MASK);
	    tchar &= CHAR_MASK;
	    if ((d[1] & (CHAR_MASK | CSET_MASK)) == UCSWIDE)
		    tattr |= ATTR_WIDE;

	    /* Video reversing things */
	    if (seltype == LEXICOGRAPHIC)
		selected = posle(selstart, scrpos) && poslt(scrpos, selend);
	    else
		selected = posPle(selstart, scrpos) && posPlt(scrpos, selend);
	    tattr = (tattr ^ rv
		     ^ (selected ? ATTR_REVERSE : 0));

	    /* 'Real' blinking ? */
	    if (blink_is_real && (tattr & ATTR_BLINK)) {
		if (has_focus && tblinker) {
		    tchar = ' ';
		    tattr &= ~CSET_MASK;
		    tattr |= ATTR_ACP;
		}
		tattr &= ~ATTR_BLINK;
	    }

	    /*
	     * Check the font we'll _probably_ be using to see if 
	     * the character is wide when we don't want it to be.
	     */
	    if ((tchar | tattr) != (disptext[idx]& ~ATTR_NARROW)) {
		if ((tattr & ATTR_WIDE) == 0 && 
		    CharWidth(ctx, (tchar | tattr) & 0xFFFF) == 2)
		    tattr |= ATTR_NARROW;
	    } else if (disptext[idx]&ATTR_NARROW)
		tattr |= ATTR_NARROW;

	    /* Cursor here ? Save the 'background' */
	    if (i == our_curs_y && j == curs.x) {
		cursor_background = tattr | tchar;
		dispcurs = disptext + idx;
	    }

	    if ((disptext[idx] ^ tattr) & ATTR_WIDE)
		dirty_line = TRUE;

	    break_run = (tattr != attr || j - start >= sizeof(ch));

	    /* Special hack for VT100 Linedraw glyphs */
	    if ((attr & CSET_MASK) == 0x2300 && tchar >= 0xBA
		&& tchar <= 0xBD) break_run = TRUE;

	    if (!dbcs_screenfont && !dirty_line) {
		if ((tchar | tattr) == disptext[idx])
		    break_run = TRUE;
		else if (!dirty_run && ccount == 1)
		    break_run = TRUE;
	    }

	    if (break_run) {
		if ((dirty_run || last_run_dirty) && ccount > 0) {
		    do_text(ctx, start, i, ch, ccount, attr, lattr);
		    updated_line = 1;
		}
		start = j;
		ccount = 0;
		attr = tattr;
		if (dbcs_screenfont)
		    last_run_dirty = dirty_run;
		dirty_run = dirty_line;
	    }

	    if ((tchar | tattr) != disptext[idx])
		dirty_run = TRUE;
	    ch[ccount++] = (char) tchar;
	    disptext[idx] = tchar | tattr;

	    /* If it's a wide char step along to the next one. */
	    if (tattr & ATTR_WIDE) {
		if (++j < cols) {
		    idx++;
		    d++;
		    /* Cursor is here ? Ouch! */
		    if (i == our_curs_y && j == curs.x) {
			cursor_background = *d;
			dispcurs = disptext + idx;
		    }
		    if (disptext[idx] != *d)
			dirty_run = TRUE;
		    disptext[idx] = *d;
		}
	    }
	}
	if (dirty_run && ccount > 0) {
	    do_text(ctx, start, i, ch, ccount, attr, lattr);
	    updated_line = 1;
	}

	/* Cursor on this line ? (and changed) */
	if (i == our_curs_y && (curstype != cursor || updated_line)) {
	    ch[0] = (char) (cursor_background & CHAR_MASK);
	    attr = (cursor_background & ATTR_MASK) | cursor;
	    do_cursor(ctx, curs.x, i, ch, 1, attr, lattr);
	    curstype = cursor;
	}
    }
}

/*
 * Flick the switch that says if blinking things should be shown or hidden.
 */

void term_blink(int flg)
{
    static long last_blink = 0;
    static long last_tblink = 0;
    long now, blink_diff;

    now = GetTickCount();
    blink_diff = now - last_tblink;

    /* Make sure the text blinks no more than 2Hz */
    if (blink_diff < 0 || blink_diff > 450) {
	last_tblink = now;
	tblinker = !tblinker;
    }

    if (flg) {
	blinker = 1;
	last_blink = now;
	return;
    }

    blink_diff = now - last_blink;

    /* Make sure the cursor blinks no faster than GetCaretBlinkTime() */
    if (blink_diff >= 0 && blink_diff < (long) GetCaretBlinkTime())
	return;

    last_blink = now;
    blinker = !blinker;
}

/*
 * Invalidate the whole screen so it will be repainted in full.
 */
void term_invalidate(void)
{
    int i;

    for (i = 0; i < rows * (cols + 1); i++)
	disptext[i] = ATTR_INVALID;
}

/*
 * Paint the window in response to a WM_PAINT message.
 */
void term_paint(Context ctx, int left, int top, int right, int bottom)
{
    int i, j;
    if (left < 0) left = 0;
    if (top < 0) top = 0;
    if (right >= cols) right = cols-1;
    if (bottom >= rows) bottom = rows-1;

    for (i = top; i <= bottom && i < rows; i++) {
	if ((disptext[i * (cols + 1) + cols] & LATTR_MODE) == LATTR_NORM)
	    for (j = left; j <= right && j < cols; j++)
		disptext[i * (cols + 1) + j] = ATTR_INVALID;
	else
	    for (j = left / 2; j <= right / 2 + 1 && j < cols; j++)
		disptext[i * (cols + 1) + j] = ATTR_INVALID;
    }

    /* This should happen soon enough, also for some reason it sometimes 
     * fails to actually do anything when re-sizing ... painting the wrong
     * window perhaps ?
     */
    if (alt_pressed)
        do_paint (ctx, FALSE);
}

/*
 * Attempt to scroll the scrollback. The second parameter gives the
 * position we want to scroll to; the first is +1 to denote that
 * this position is relative to the beginning of the scrollback, -1
 * to denote it is relative to the end, and 0 to denote that it is
 * relative to the current position.
 */
void term_scroll(int rel, int where)
{
    int sbtop = -count234(scrollback);

    disptop = (rel < 0 ? 0 : rel > 0 ? sbtop : disptop) + where;
    if (disptop < sbtop)
	disptop = sbtop;
    if (disptop > 0)
	disptop = 0;
    update_sbar();
    term_update();
}

static void clipme(pos top, pos bottom, int rect)
{
    wchar_t *workbuf;
    wchar_t *wbptr;		       /* where next char goes within workbuf */
    int old_top_x;
    int wblen = 0;		       /* workbuf len */
    int buflen;			       /* amount of memory allocated to workbuf */

    buflen = 5120;		       /* Default size */
    workbuf = smalloc(buflen * sizeof(wchar_t));
    wbptr = workbuf;		       /* start filling here */
    old_top_x = top.x;		       /* needed for rect==1 */

    while (poslt(top, bottom)) {
	int nl = FALSE;
	unsigned long *ldata = lineptr(top.y);
	pos nlpos;

	/*
	 * nlpos will point at the maximum position on this line we
	 * should copy up to. So we start it at the end of the
	 * line...
	 */
	nlpos.y = top.y;
	nlpos.x = cols;

	/*
	 * ... move it backwards if there's unused space at the end
	 * of the line (and also set `nl' if this is the case,
	 * because in normal selection mode this means we need a
	 * newline at the end)...
	 */
	if (!(ldata[cols] & LATTR_WRAPPED)) {
	    while (((ldata[nlpos.x - 1] & 0xFF) == 0x20 ||
		    (DIRECT_CHAR(ldata[nlpos.x - 1]) &&
		     (ldata[nlpos.x - 1] & CHAR_MASK) == 0x20))
		   && poslt(top, nlpos))
		decpos(nlpos);
	    if (poslt(nlpos, bottom))
		nl = TRUE;
	}

	/*
	 * ... and then clip it to the terminal x coordinate if
	 * we're doing rectangular selection. (In this case we
	 * still did the above, so that copying e.g. the right-hand
	 * column from a table doesn't fill with spaces on the
	 * right.)
	 */
	if (rect) {
	    if (nlpos.x > bottom.x)
		nlpos.x = bottom.x;
	    nl = (top.y < bottom.y);
	}

	while (poslt(top, bottom) && poslt(top, nlpos)) {
#if 0
	    char cbuf[16], *p;
	    sprintf(cbuf, "<U+%04x>", (ldata[top.x] & 0xFFFF));
#else
	    wchar_t cbuf[16], *p;
	    int uc = (ldata[top.x] & 0xFFFF);
	    int set, c;

	    if (uc == UCSWIDE) {
		top.x++;
		continue;
	    }

	    switch (uc & CSET_MASK) {
	      case ATTR_LINEDRW:
		if (!cfg.rawcnp) {
		    uc = unitab_xterm[uc & 0xFF];
		    break;
		}
	      case ATTR_ASCII:
		uc = unitab_line[uc & 0xFF];
		break;
	      case ATTR_SCOACS:  
		uc = unitab_scoacs[uc&0xFF]; 
		break;
	    }
	    switch (uc & CSET_MASK) {
	      case ATTR_ACP:
		uc = unitab_font[uc & 0xFF];
		break;
	      case ATTR_OEMCP:
		uc = unitab_oemcp[uc & 0xFF];
		break;
	    }

	    set = (uc & CSET_MASK);
	    c = (uc & CHAR_MASK);
	    cbuf[0] = uc;
	    cbuf[1] = 0;

	    if (DIRECT_FONT(uc)) {
		if (c >= ' ' && c != 0x7F) {
		    unsigned char buf[4];
		    WCHAR wbuf[4];
		    int rv;
		    if (IsDBCSLeadByteEx(font_codepage, (BYTE) c)) {
			buf[0] = c;
			buf[1] = (unsigned char) ldata[top.x + 1];
			rv = MultiByteToWideChar(font_codepage,
						 0, buf, 2, wbuf, 4);
			top.x++;
		    } else {
			buf[0] = c;
			rv = MultiByteToWideChar(font_codepage,
						 0, buf, 1, wbuf, 4);
		    }

		    if (rv > 0) {
			memcpy(cbuf, wbuf, rv * sizeof(wchar_t));
			cbuf[rv] = 0;
		    }
		}
	    }
#endif

	    for (p = cbuf; *p; p++) {
		/* Enough overhead for trailing NL and nul */
		if (wblen >= buflen - 16) {
		    workbuf =
			srealloc(workbuf,
				 sizeof(wchar_t) * (buflen += 100));
		    wbptr = workbuf + wblen;
		}
		wblen++;
		*wbptr++ = *p;
	    }
	    top.x++;
	}
	if (nl) {
	    int i;
	    for (i = 0; i < sel_nl_sz; i++) {
		wblen++;
		*wbptr++ = sel_nl[i];
	    }
	}
	top.y++;
	top.x = rect ? old_top_x : 0;
    }
    wblen++;
    *wbptr++ = 0;
    write_clip(workbuf, wblen, FALSE); /* transfer to clipboard */
    if (buflen > 0)		       /* indicates we allocated this buffer */
	sfree(workbuf);
}

void term_copyall(void)
{
    pos top;
    top.y = -count234(scrollback);
    top.x = 0;
    clipme(top, curs, 0);
}

/*
 * The wordness array is mainly for deciding the disposition of the US-ASCII 
 * characters.
 */
static int wordtype(int uc)
{
    static struct {
	int start, end, ctype;
    } *wptr, ucs_words[] = {
	{
	128, 160, 0}, {
	161, 191, 1}, {
	215, 215, 1}, {
	247, 247, 1}, {
	0x037e, 0x037e, 1},	       /* Greek question mark */
	{
	0x0387, 0x0387, 1},	       /* Greek ano teleia */
	{
	0x055a, 0x055f, 1},	       /* Armenian punctuation */
	{
	0x0589, 0x0589, 1},	       /* Armenian full stop */
	{
	0x0700, 0x070d, 1},	       /* Syriac punctuation */
	{
	0x104a, 0x104f, 1},	       /* Myanmar punctuation */
	{
	0x10fb, 0x10fb, 1},	       /* Georgian punctuation */
	{
	0x1361, 0x1368, 1},	       /* Ethiopic punctuation */
	{
	0x166d, 0x166e, 1},	       /* Canadian Syl. punctuation */
	{
	0x17d4, 0x17dc, 1},	       /* Khmer punctuation */
	{
	0x1800, 0x180a, 1},	       /* Mongolian punctuation */
	{
	0x2000, 0x200a, 0},	       /* Various spaces */
	{
	0x2070, 0x207f, 2},	       /* superscript */
	{
	0x2080, 0x208f, 2},	       /* subscript */
	{
	0x200b, 0x27ff, 1},	       /* punctuation and symbols */
	{
	0x3000, 0x3000, 0},	       /* ideographic space */
	{
	0x3001, 0x3020, 1},	       /* ideographic punctuation */
	{
	0x303f, 0x309f, 3},	       /* Hiragana */
	{
	0x30a0, 0x30ff, 3},	       /* Katakana */
	{
	0x3300, 0x9fff, 3},	       /* CJK Ideographs */
	{
	0xac00, 0xd7a3, 3},	       /* Hangul Syllables */
	{
	0xf900, 0xfaff, 3},	       /* CJK Ideographs */
	{
	0xfe30, 0xfe6b, 1},	       /* punctuation forms */
	{
	0xff00, 0xff0f, 1},	       /* half/fullwidth ASCII */
	{
	0xff1a, 0xff20, 1},	       /* half/fullwidth ASCII */
	{
	0xff3b, 0xff40, 1},	       /* half/fullwidth ASCII */
	{
	0xff5b, 0xff64, 1},	       /* half/fullwidth ASCII */
	{
	0xfff0, 0xffff, 0},	       /* half/fullwidth ASCII */
	{
	0, 0, 0}
    };

    uc &= (CSET_MASK | CHAR_MASK);

    switch (uc & CSET_MASK) {
      case ATTR_LINEDRW:
	uc = unitab_xterm[uc & 0xFF];
	break;
      case ATTR_ASCII:
	uc = unitab_line[uc & 0xFF];
	break;
      case ATTR_SCOACS:  
	uc = unitab_scoacs[uc&0xFF]; 
	break;
    }
    switch (uc & CSET_MASK) {
      case ATTR_ACP:
	uc = unitab_font[uc & 0xFF];
	break;
      case ATTR_OEMCP:
	uc = unitab_oemcp[uc & 0xFF];
	break;
    }

    /* For DBCS font's I can't do anything usefull. Even this will sometimes
     * fail as there's such a thing as a double width space. :-(
     */
    if (dbcs_screenfont && font_codepage == line_codepage)
	return (uc != ' ');

    if (uc < 0x80)
	return wordness[uc];

    for (wptr = ucs_words; wptr->start; wptr++) {
	if (uc >= wptr->start && uc <= wptr->end)
	    return wptr->ctype;
    }

    return 2;
}

/*
 * Spread the selection outwards according to the selection mode.
 */
static pos sel_spread_half(pos p, int dir)
{
    unsigned long *ldata;
    short wvalue;
    int topy = -count234(scrollback);

    ldata = lineptr(p.y);

    switch (selmode) {
      case SM_CHAR:
	/*
	 * In this mode, every character is a separate unit, except
	 * for runs of spaces at the end of a non-wrapping line.
	 */
	if (!(ldata[cols] & LATTR_WRAPPED)) {
	    unsigned long *q = ldata + cols;
	    while (q > ldata && (q[-1] & CHAR_MASK) == 0x20)
		q--;
	    if (q == ldata + cols)
		q--;
	    if (p.x >= q - ldata)
		p.x = (dir == -1 ? q - ldata : cols - 1);
	}
	break;
      case SM_WORD:
	/*
	 * In this mode, the units are maximal runs of characters
	 * whose `wordness' has the same value.
	 */
	wvalue = wordtype(ldata[p.x]);
	if (dir == +1) {
	    while (1) {
		if (p.x < cols-1) {
		    if (wordtype(ldata[p.x + 1]) == wvalue)
			p.x++;
		    else
			break;
		} else {
		    if (ldata[cols] & LATTR_WRAPPED) {
			unsigned long *ldata2;
			ldata2 = lineptr(p.y+1);
			if (wordtype(ldata2[0]) == wvalue) {
			    p.x = 0;
			    p.y++;
			    ldata = ldata2;
			} else
			    break;
		    } else
			break;
		}
	    }
	} else {
	    while (1) {
		if (p.x > 0) {
		    if (wordtype(ldata[p.x - 1]) == wvalue)
			p.x--;
		    else
			break;
		} else {
		    unsigned long *ldata2;
		    if (p.y <= topy)
			break;
		    ldata2 = lineptr(p.y-1);
		    if ((ldata2[cols] & LATTR_WRAPPED) &&
			wordtype(ldata2[cols-1]) == wvalue) {
			p.x = cols-1;
			p.y--;
			ldata = ldata2;
		    } else
			break;
		}
	    }
	}
	break;
      case SM_LINE:
	/*
	 * In this mode, every line is a unit.
	 */
	p.x = (dir == -1 ? 0 : cols - 1);
	break;
    }
    return p;
}

static void sel_spread(void)
{
    if (seltype == LEXICOGRAPHIC) {
	selstart = sel_spread_half(selstart, -1);
	decpos(selend);
	selend = sel_spread_half(selend, +1);
	incpos(selend);
    }
}

void term_do_paste(void)
{
    wchar_t *data;
    int len;

    get_clip(&data, &len);
    if (data) {
        wchar_t *p, *q;

        if (paste_buffer)
            sfree(paste_buffer);
        paste_pos = paste_hold = paste_len = 0;
        paste_buffer = smalloc(len * sizeof(wchar_t));

        p = q = data;
        while (p < data + len) {
            while (p < data + len &&
                   !(p <= data + len - sel_nl_sz &&
                     !memcmp(p, sel_nl, sizeof(sel_nl))))
                p++;

            {
                int i;
                for (i = 0; i < p - q; i++) {
                    paste_buffer[paste_len++] = q[i];
                }
            }

            if (p <= data + len - sel_nl_sz &&
                !memcmp(p, sel_nl, sizeof(sel_nl))) {
                paste_buffer[paste_len++] = '\r';
                p += sel_nl_sz;
            }
            q = p;
        }

        /* Assume a small paste will be OK in one go. */
        if (paste_len < 256) {
            luni_send(paste_buffer, paste_len, 0);
            if (paste_buffer)
                sfree(paste_buffer);
            paste_buffer = 0;
            paste_pos = paste_hold = paste_len = 0;
        }
    }
    get_clip(NULL, NULL);
}

void term_mouse(Mouse_Button b, Mouse_Action a, int x, int y,
		int shift, int ctrl, int alt)
{
    pos selpoint;
    unsigned long *ldata;
    int raw_mouse = (xterm_mouse &&
		     !cfg.no_mouse_rep &&
		     !(cfg.mouse_override && shift));
    int default_seltype;

    if (y < 0) {
	y = 0;
	if (a == MA_DRAG && !raw_mouse)
	    term_scroll(0, -1);
    }
    if (y >= rows) {
	y = rows - 1;
	if (a == MA_DRAG && !raw_mouse)
	    term_scroll(0, +1);
    }
    if (x < 0) {
	if (y > 0) {
	    x = cols - 1;
	    y--;
	} else
	    x = 0;
    }
    if (x >= cols)
	x = cols - 1;

    selpoint.y = y + disptop;
    selpoint.x = x;
    ldata = lineptr(selpoint.y);
    if ((ldata[cols] & LATTR_MODE) != LATTR_NORM)
	selpoint.x /= 2;

    if (raw_mouse) {
	int encstate = 0, r, c;
	char abuf[16];
	static int is_down = 0;

	switch (b) {
	  case MBT_LEFT:
	    encstate = 0x20;	       /* left button down */
	    break;
	  case MBT_MIDDLE:
	    encstate = 0x21;
	    break;
	  case MBT_RIGHT:
	    encstate = 0x22;
	    break;
	  case MBT_WHEEL_UP:
	    encstate = 0x60;
	    break;
	  case MBT_WHEEL_DOWN:
	    encstate = 0x61;
	    break;
	  default: break;	       /* placate gcc warning about enum use */
	}
	switch (a) {
	  case MA_DRAG:
	    if (xterm_mouse == 1)
		return;
	    encstate += 0x20;
	    break;
	  case MA_RELEASE:
	    encstate = 0x23;
	    is_down = 0;
	    break;
	  case MA_CLICK:
	    if (is_down == b)
		return;
	    is_down = b;
	    break;
	  default: break;	       /* placate gcc warning about enum use */
	}
	if (shift)
	    encstate += 0x04;
	if (ctrl)
	    encstate += 0x10;
	r = y + 33;
	c = x + 33;

	sprintf(abuf, "\033[M%c%c%c", encstate, c, r);
	ldisc_send(abuf, 6, 0);
	return;
    }

    b = translate_button(b);

    /*
     * Set the selection type (rectangular or normal) at the start
     * of a selection attempt, from the state of Alt.
     */
    if (!alt ^ !cfg.rect_select)
	default_seltype = RECTANGULAR;
    else
	default_seltype = LEXICOGRAPHIC;
	
    if (selstate == NO_SELECTION) {
	seltype = default_seltype;
    }

    if (b == MBT_SELECT && a == MA_CLICK) {
	deselect();
	selstate = ABOUT_TO;
	seltype = default_seltype;
	selanchor = selpoint;
	selmode = SM_CHAR;
    } else if (b == MBT_SELECT && (a == MA_2CLK || a == MA_3CLK)) {
	deselect();
	selmode = (a == MA_2CLK ? SM_WORD : SM_LINE);
	selstate = DRAGGING;
	selstart = selanchor = selpoint;
	selend = selstart;
	incpos(selend);
	sel_spread();
    } else if ((b == MBT_SELECT && a == MA_DRAG) ||
	       (b == MBT_EXTEND && a != MA_RELEASE)) {
	if (selstate == ABOUT_TO && poseq(selanchor, selpoint))
	    return;
	if (b == MBT_EXTEND && a != MA_DRAG && selstate == SELECTED) {
	    if (seltype == LEXICOGRAPHIC) {
		/*
		 * For normal selection, we extend by moving
		 * whichever end of the current selection is closer
		 * to the mouse.
		 */
		if (posdiff(selpoint, selstart) <
		    posdiff(selend, selstart) / 2) {
		    selanchor = selend;
		    decpos(selanchor);
		} else {
		    selanchor = selstart;
		}
	    } else {
		/*
		 * For rectangular selection, we have a choice of
		 * _four_ places to put selanchor and selpoint: the
		 * four corners of the selection.
		 */
		if (2*selpoint.x < selstart.x + selend.x)
		    selanchor.x = selend.x-1;
		else
		    selanchor.x = selstart.x;

		if (2*selpoint.y < selstart.y + selend.y)
		    selanchor.y = selend.y;
		else
		    selanchor.y = selstart.y;
	    }
	    selstate = DRAGGING;
	}
	if (selstate != ABOUT_TO && selstate != DRAGGING)
	    selanchor = selpoint;
	selstate = DRAGGING;
	if (seltype == LEXICOGRAPHIC) {
	    /*
	     * For normal selection, we set (selstart,selend) to
	     * (selpoint,selanchor) in some order.
	     */
	    if (poslt(selpoint, selanchor)) {
		selstart = selpoint;
		selend = selanchor;
		incpos(selend);
	    } else {
		selstart = selanchor;
		selend = selpoint;
		incpos(selend);
	    }
	} else {
	    /*
	     * For rectangular selection, we may need to
	     * interchange x and y coordinates (if the user has
	     * dragged in the -x and +y directions, or vice versa).
	     */
	    selstart.x = min(selanchor.x, selpoint.x);
	    selend.x = 1+max(selanchor.x, selpoint.x);
	    selstart.y = min(selanchor.y, selpoint.y);
	    selend.y =   max(selanchor.y, selpoint.y);
	}
	sel_spread();
    } else if ((b == MBT_SELECT || b == MBT_EXTEND) && a == MA_RELEASE) {
	if (selstate == DRAGGING) {
	    /*
	     * We've completed a selection. We now transfer the
	     * data to the clipboard.
	     */
	    clipme(selstart, selend, (seltype == RECTANGULAR));
	    selstate = SELECTED;
	} else
	    selstate = NO_SELECTION;
    } else if (b == MBT_PASTE
	       && (a == MA_CLICK || a == MA_2CLK || a == MA_3CLK)) {
        term_do_paste();
    }

    term_update();
}

void term_nopaste()
{
    if (paste_len == 0)
	return;
    sfree(paste_buffer);
    paste_buffer = 0;
    paste_len = 0;
}

void term_paste()
{
    static long last_paste = 0;
    long now, paste_diff;

    if (paste_len == 0)
	return;

    /* Don't wait forever to paste */
    if (paste_hold) {
	now = GetTickCount();
	paste_diff = now - last_paste;
	if (paste_diff >= 0 && paste_diff < 450)
	    return;
    }
    paste_hold = 0;

    while (paste_pos < paste_len) {
	int n = 0;
	while (n + paste_pos < paste_len) {
	    if (paste_buffer[paste_pos + n++] == '\r')
		break;
	}
	luni_send(paste_buffer + paste_pos, n, 0);
	paste_pos += n;

	if (paste_pos < paste_len) {
	    paste_hold = 1;
	    return;
	}
    }
    sfree(paste_buffer);
    paste_buffer = 0;
    paste_len = 0;
}

static void deselect(void)
{
    selstate = NO_SELECTION;
    selstart.x = selstart.y = selend.x = selend.y = 0;
}

void term_deselect(void)
{
    deselect();
    term_update();
}

int term_ldisc(int option)
{
    if (option == LD_ECHO)
	return term_echoing;
    if (option == LD_EDIT)
	return term_editing;
    return FALSE;
}

/*
 * from_backend(), to get data from the backend for the terminal.
 */
int from_backend(int is_stderr, char *data, int len)
{
    assert(len > 0);

    bufchain_add(&inbuf, data, len);

    /*
     * term_out() always completely empties inbuf. Therefore,
     * there's no reason at all to return anything other than zero
     * from this function, because there _can't_ be a question of
     * the remote side needing to wait until term_out() has cleared
     * a backlog.
     *
     * This is a slightly suboptimal way to deal with SSH2 - in
     * principle, the window mechanism would allow us to continue
     * to accept data on forwarded ports and X connections even
     * while the terminal processing was going slowly - but we
     * can't do the 100% right thing without moving the terminal
     * processing into a separate thread, and that might hurt
     * portability. So we manage stdout buffering the old SSH1 way:
     * if the terminal processing goes slowly, the whole SSH
     * connection stops accepting data until it's ready.
     *
     * In practice, I can't imagine this causing serious trouble.
     */
    return 0;
}
