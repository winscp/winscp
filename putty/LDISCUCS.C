/*
 * ldisc.c: PuTTY line discipline. Sits between the input coming
 * from keypresses in the window, and the output channel leading to
 * the back end. Implements echo and/or local line editing,
 * depending on what's currently configured.
 */

#include <stdio.h>
#include <ctype.h>

#include "putty.h"
#include "terminal.h"
#include "ldisc.h"

void lpage_send(void *handle,
		int codepage, char *buf, int len, int interactive)
{
    Ldisc ldisc = (Ldisc)handle;
    wchar_t *widebuffer = 0;
    int widesize = 0;
    int wclen;

    if (codepage < 0) {
	ldisc_send(ldisc, buf, len, interactive);
	return;
    }

    widesize = len * 2;
    widebuffer = snewn(widesize, wchar_t);

    wclen = mb_to_wc(codepage, 0, buf, len, widebuffer, widesize);
    luni_send(ldisc, widebuffer, wclen, interactive);

    sfree(widebuffer);
}

void luni_send(void *handle, wchar_t * widebuf, int len, int interactive)
{
    Ldisc ldisc = (Ldisc)handle;
    int ratio = (in_utf(ldisc->term))?3:1;
    char *linebuffer;
    int linesize;
    int i;
    char *p;

    linesize = len * ratio * 2;
    linebuffer = snewn(linesize, char);

    if (in_utf(ldisc->term)) {
	/* UTF is a simple algorithm */
	for (p = linebuffer, i = 0; i < len; i++) {
	    wchar_t ch = widebuf[i];
	    /* We only deal with 16-bit wide chars */
	    if ((ch&0xF800) == 0xD800) ch = '.';

	    if (ch < 0x80) {
		*p++ = (char) (ch);
	    } else if (ch < 0x800) {
		*p++ = (0xC0 | (ch >> 6));
		*p++ = (0x80 | (ch & 0x3F));
	    } else {
		*p++ = (0xE0 | (ch >> 12));
		*p++ = (0x80 | ((ch >> 6) & 0x3F));
		*p++ = (0x80 | (ch & 0x3F));
	    }
	}
    } else {
	int rv;
	rv = wc_to_mb(ldisc->term->ucsdata->line_codepage, 0, widebuf, len,
		      linebuffer, linesize, NULL, NULL, ldisc->term->ucsdata);
	if (rv >= 0)
	    p = linebuffer + rv;
	else
	    p = linebuffer;
    }
    if (p > linebuffer)
	ldisc_send(ldisc, linebuffer, p - linebuffer, interactive);

    sfree(linebuffer);
}
