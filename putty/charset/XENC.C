/*
 * xenc.c - translate our internal character set codes to and from
 * X11 character encoding names.
 * 
 */

#include <ctype.h>
#include "charset.h"
#include "internal.h"

static const struct {
    const char *name;
    int charset;
} xencs[] = {
    /*
     * Officially registered encoding names. This list is derived
     * from the font encodings section of
     * 
     *   http://ftp.x.org/pub/DOCS/registry
     * 
     * Where multiple encoding names map to the same encoding id
     * (such as iso8859-15 and fcd8859-15), the first is considered
     * canonical and will be returned when translating the id to a
     * string.
     */
    { "iso8859-1", CS_ISO8859_1 },
    { "iso8859-2", CS_ISO8859_2 },
    { "iso8859-3", CS_ISO8859_3 },
    { "iso8859-4", CS_ISO8859_4 },
    { "iso8859-5", CS_ISO8859_5 },
    { "iso8859-6", CS_ISO8859_6 },
    { "iso8859-7", CS_ISO8859_7 },
    { "iso8859-8", CS_ISO8859_8 },
    { "iso8859-9", CS_ISO8859_9 },
    { "iso8859-10", CS_ISO8859_10 },
    { "iso8859-13", CS_ISO8859_13 },
    { "iso8859-14", CS_ISO8859_14 },
    { "iso8859-15", CS_ISO8859_15 },
    { "fcd8859-15", CS_ISO8859_15 },
    { "hp-roman8", CS_HP_ROMAN8 },
    { "koi8-r", CS_KOI8_R },
    /*
     * Unofficial encoding names found in the wild.
     */
    { "iso8859-16", CS_ISO8859_16 },
    { "koi8-u", CS_KOI8_U },
    { "ibm-cp437", CS_CP437 },
    { "ibm-cp850", CS_CP850 },
    { "microsoft-cp1250", CS_CP1250 },
    { "microsoft-cp1251", CS_CP1251 },
    { "microsoft-cp1252", CS_CP1252 },
    { "microsoft-cp1253", CS_CP1253 },
    { "microsoft-cp1254", CS_CP1254 },
    { "microsoft-cp1255", CS_CP1255 },
    { "microsoft-cp1256", CS_CP1256 },
    { "microsoft-cp1257", CS_CP1257 },
    { "microsoft-cp1258", CS_CP1258 },
    { "mac-roman", CS_MAC_ROMAN },
    { "viscii1.1-1", CS_VISCII },
    { "viscii1-1", CS_VISCII },
};

const char *charset_to_xenc(int charset)
{
    int i;

    for (i = 0; i < (int)lenof(xencs); i++)
	if (charset == xencs[i].charset)
	    return xencs[i].name;

    return NULL;		       /* not found */
}

int charset_from_xenc(const char *name)
{
    int i;

    for (i = 0; i < (int)lenof(xencs); i++) {
	const char *p, *q;
	p = name;
	q = xencs[i].name;
	while (*p || *q) {
	    if (tolower(*p) != tolower(*q))
		break;
	    p++; q++;
	}
	if (!*p && !*q)
	    return xencs[i].charset;
    }

    return CS_NONE;		       /* not found */
}
