/*
 * ldisc.c: PuTTY line discipline. Sits between the input coming
 * from keypresses in the window, and the output channel leading to
 * the back end. Implements echo and/or local line editing,
 * depending on what's currently configured.
 */

#include <windows.h>
#include <stdio.h>
#include <ctype.h>

#include "putty.h"

#define ECHOING (cfg.localecho == LD_YES || \
                 (cfg.localecho == LD_BACKEND && \
                      (back->ldisc(LD_ECHO) || term_ldisc(LD_ECHO))))
#define EDITING (cfg.localedit == LD_YES || \
                 (cfg.localedit == LD_BACKEND && \
                      (back->ldisc(LD_EDIT) || term_ldisc(LD_EDIT))))

static void c_write(char *buf, int len)
{
    from_backend(0, buf, len);
}

static char *term_buf = NULL;
static int term_buflen = 0, term_bufsiz = 0, term_quotenext = 0;

static int plen(unsigned char c)
{
    if ((c >= 32 && c <= 126) || (c >= 160 && !in_utf))
	return 1;
    else if (c < 128)
	return 2;		       /* ^x for some x */
    else
	return 4;		       /* <XY> for hex XY */
}

static void pwrite(unsigned char c)
{
    if ((c >= 32 && c <= 126) || (c >= 160 && !in_utf)) {
	c_write(&c, 1);
    } else if (c < 128) {
	char cc[2];
	cc[1] = (c == 127 ? '?' : c + 0x40);
	cc[0] = '^';
	c_write(cc, 2);
    } else {
	char cc[5];
	sprintf(cc, "<%02X>", c);
	c_write(cc, 4);
    }
}

static void bsb(int n)
{
    while (n--)
	c_write("\010 \010", 3);
}

#define CTRL(x) (x^'@')
#define KCTRL(x) ((x^'@') | 0x100)

void ldisc_send(char *buf, int len, int interactive)
{
    int keyflag = 0;
    /*
     * Called with len=0 when the options change. We must inform
     * the front end in case it needs to know.
     */
    if (len == 0) {
	void ldisc_update(int echo, int edit);
	ldisc_update(ECHOING, EDITING);
    }
    /*
     * Less than zero means null terminated special string.
     */
    if (len < 0) {
	len = strlen(buf);
	keyflag = KCTRL('@');
    }
    /*
     * Either perform local editing, or just send characters.
     */
    if (EDITING) {
	while (len--) {
	    int c;
	    c = *buf++ + keyflag;
	    if (!interactive && c == '\r')
		c += KCTRL('@');
	    switch (term_quotenext ? ' ' : c) {
		/*
		 * ^h/^?: delete one char and output one BSB
		 * ^w: delete, and output BSBs, to return to last
		 * space/nonspace boundary
		 * ^u: delete, and output BSBs, to return to BOL
		 * ^c: Do a ^u then send a telnet IP
		 * ^z: Do a ^u then send a telnet SUSP
		 * ^\: Do a ^u then send a telnet ABORT
		 * ^r: echo "^R\n" and redraw line
		 * ^v: quote next char
		 * ^d: if at BOL, end of file and close connection,
		 * else send line and reset to BOL
		 * ^m: send line-plus-\r\n and reset to BOL
		 */
	      case KCTRL('H'):
	      case KCTRL('?'):	       /* backspace/delete */
		if (term_buflen > 0) {
		    if (ECHOING)
			bsb(plen(term_buf[term_buflen - 1]));
		    term_buflen--;
		}
		break;
	      case CTRL('W'):	       /* delete word */
		while (term_buflen > 0) {
		    if (ECHOING)
			bsb(plen(term_buf[term_buflen - 1]));
		    term_buflen--;
		    if (term_buflen > 0 &&
			isspace(term_buf[term_buflen - 1]) &&
			!isspace(term_buf[term_buflen]))
			break;
		}
		break;
	      case CTRL('U'):	       /* delete line */
	      case CTRL('C'):	       /* Send IP */
	      case CTRL('\\'):	       /* Quit */
	      case CTRL('Z'):	       /* Suspend */
		while (term_buflen > 0) {
		    if (ECHOING)
			bsb(plen(term_buf[term_buflen - 1]));
		    term_buflen--;
		}
		back->special(TS_EL);
                /*
                 * We don't send IP, SUSP or ABORT if the user has
                 * configured telnet specials off! This breaks
                 * talkers otherwise.
                 */
                if (!cfg.telnet_keyboard)
                    goto default_case;
		if (c == CTRL('C'))
		    back->special(TS_IP);
		if (c == CTRL('Z'))
		    back->special(TS_SUSP);
		if (c == CTRL('\\'))
		    back->special(TS_ABORT);
		break;
	      case CTRL('R'):	       /* redraw line */
		if (ECHOING) {
		    int i;
		    c_write("^R\r\n", 4);
		    for (i = 0; i < term_buflen; i++)
			pwrite(term_buf[i]);
		}
		break;
	      case CTRL('V'):	       /* quote next char */
		term_quotenext = TRUE;
		break;
	      case CTRL('D'):	       /* logout or send */
		if (term_buflen == 0) {
		    back->special(TS_EOF);
		} else {
		    back->send(term_buf, term_buflen);
		    term_buflen = 0;
		}
		break;
		/*
		 * This particularly hideous bit of code from RDB
		 * allows ordinary ^M^J to do the same thing as
		 * magic-^M when in Raw protocol. The line `case
		 * KCTRL('M'):' is _inside_ the if block. Thus:
		 * 
		 *  - receiving regular ^M goes straight to the
		 *    default clause and inserts as a literal ^M.
		 *  - receiving regular ^J _not_ directly after a
		 *    literal ^M (or not in Raw protocol) fails the
		 *    if condition, leaps to the bottom of the if,
		 *    and falls through into the default clause
		 *    again.
		 *  - receiving regular ^J just after a literal ^M
		 *    in Raw protocol passes the if condition,
		 *    deletes the literal ^M, and falls through
		 *    into the magic-^M code
		 *  - receiving a magic-^M empties the line buffer,
		 *    signals end-of-line in one of the various
		 *    entertaining ways, and _doesn't_ fall out of
		 *    the bottom of the if and through to the
		 *    default clause because of the break.
		 */
	      case CTRL('J'):
		if (cfg.protocol == PROT_RAW &&
		    term_buflen > 0 && term_buf[term_buflen - 1] == '\r') {
		    if (ECHOING)
			bsb(plen(term_buf[term_buflen - 1]));
		    term_buflen--;
		    /* FALLTHROUGH */
	      case KCTRL('M'):	       /* send with newline */
		    if (term_buflen > 0)
			back->send(term_buf, term_buflen);
		    if (cfg.protocol == PROT_RAW)
			back->send("\r\n", 2);
		    else if (cfg.protocol == PROT_TELNET && cfg.telnet_newline)
			back->special(TS_EOL);
		    else
			back->send("\r", 1);
		    if (ECHOING)
			c_write("\r\n", 2);
		    term_buflen = 0;
		    break;
		}
		/* FALLTHROUGH */
	      default:		       /* get to this label from ^V handler */
                default_case:
		if (term_buflen >= term_bufsiz) {
		    term_bufsiz = term_buflen + 256;
		    term_buf = saferealloc(term_buf, term_bufsiz);
		}
		term_buf[term_buflen++] = c;
		if (ECHOING)
		    pwrite((unsigned char) c);
		term_quotenext = FALSE;
		break;
	    }
	}
    } else {
	if (term_buflen != 0) {
	    back->send(term_buf, term_buflen);
	    while (term_buflen > 0) {
		bsb(plen(term_buf[term_buflen - 1]));
		term_buflen--;
	    }
	}
	if (len > 0) {
	    if (ECHOING)
		c_write(buf, len);
	    if (keyflag && cfg.protocol == PROT_TELNET && len == 1) {
		switch (buf[0]) {
		  case CTRL('M'):
		    if (cfg.protocol == PROT_TELNET && cfg.telnet_newline)
			back->special(TS_EOL);
		    else
			back->send("\r", 1);
		    break;
		  case CTRL('?'):
		  case CTRL('H'):
		    if (cfg.telnet_keyboard) {
			back->special(TS_EC);
			break;
		    }
		  case CTRL('C'):
		    if (cfg.telnet_keyboard) {
			back->special(TS_IP);
			break;
		    }
		  case CTRL('Z'):
		    if (cfg.telnet_keyboard) {
			back->special(TS_SUSP);
			break;
		    }

		  default:
		    back->send(buf, len);
		    break;
		}
	    } else
		back->send(buf, len);
	}
    }
}
