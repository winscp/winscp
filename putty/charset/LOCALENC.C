/*
 * local.c - translate our internal character set codes to and from
 * our own set of plausibly legible character-set names. Also
 * provides a canonical name for each encoding (useful for software
 * announcing what character set it will be using), and a set of
 * enumeration functions which return a list of supported
 * encodings one by one.
 * 
 * charset_from_localenc will attempt all other text translations
 * as well as this table, to maximise the number of different ways
 * you can select a supported charset.
 */

#include <ctype.h>
#include "charset.h"
#include "internal.h"

static const struct {
    const char *name;
    int charset;
    int return_in_enum;   /* enumeration misses some charsets */
} localencs[] = {
    { "<UNKNOWN>", CS_NONE, 0 },
    { "ISO-8859-1", CS_ISO8859_1, 1 },
    { "ISO-8859-1 with X11 line drawing", CS_ISO8859_1_X11, 0 },
    { "ISO-8859-2", CS_ISO8859_2, 1 },
    { "ISO-8859-3", CS_ISO8859_3, 1 },
    { "ISO-8859-4", CS_ISO8859_4, 1 },
    { "ISO-8859-5", CS_ISO8859_5, 1 },
    { "ISO-8859-6", CS_ISO8859_6, 1 },
    { "ISO-8859-7", CS_ISO8859_7, 1 },
    { "ISO-8859-8", CS_ISO8859_8, 1 },
    { "ISO-8859-9", CS_ISO8859_9, 1 },
    { "ISO-8859-10", CS_ISO8859_10, 1 },
    { "ISO-8859-11", CS_ISO8859_11, 1 },
    { "ISO-8859-13", CS_ISO8859_13, 1 },
    { "ISO-8859-14", CS_ISO8859_14, 1 },
    { "ISO-8859-15", CS_ISO8859_15, 1 },
    { "ISO-8859-16", CS_ISO8859_16, 1 },
    { "CP437", CS_CP437, 1 },
    { "CP850", CS_CP850, 1 },
    { "CP1250", CS_CP1250, 1 },
    { "CP1251", CS_CP1251, 1 },
    { "CP1252", CS_CP1252, 1 },
    { "CP1253", CS_CP1253, 1 },
    { "CP1254", CS_CP1254, 1 },
    { "CP1255", CS_CP1255, 1 },
    { "CP1256", CS_CP1256, 1 },
    { "CP1257", CS_CP1257, 1 },
    { "CP1258", CS_CP1258, 1 },
    { "KOI8-R", CS_KOI8_R, 1 },
    { "KOI8-U", CS_KOI8_U, 1 },
    { "Mac Roman", CS_MAC_ROMAN, 1 },
    { "Mac Turkish", CS_MAC_TURKISH, 1 },
    { "Mac Croatian", CS_MAC_CROATIAN, 1 },
    { "Mac Iceland", CS_MAC_ICELAND, 1 },
    { "Mac Romanian", CS_MAC_ROMANIAN, 1 },
    { "Mac Greek", CS_MAC_GREEK, 1 },
    { "Mac Cyrillic", CS_MAC_CYRILLIC, 1 },
    { "Mac Thai", CS_MAC_THAI, 1 },
    { "Mac Centeuro", CS_MAC_CENTEURO, 1 },
    { "Mac Symbol", CS_MAC_SYMBOL, 1 },
    { "Mac Dingbats", CS_MAC_DINGBATS, 1 },
    { "Mac Roman (old)", CS_MAC_ROMAN_OLD, 0 },
    { "Mac Croatian (old)", CS_MAC_CROATIAN_OLD, 0 },
    { "Mac Iceland (old)", CS_MAC_ICELAND_OLD, 0 },
    { "Mac Romanian (old)", CS_MAC_ROMANIAN_OLD, 0 },
    { "Mac Greek (old)", CS_MAC_GREEK_OLD, 0 },
    { "Mac Cyrillic (old)", CS_MAC_CYRILLIC_OLD, 0 },
    { "Mac Ukraine", CS_MAC_UKRAINE, 1 },
    { "Mac VT100", CS_MAC_VT100, 1 },
    { "Mac VT100 (old)", CS_MAC_VT100_OLD, 0 },
    { "VISCII", CS_VISCII, 1 },
    { "HP ROMAN8", CS_HP_ROMAN8, 1 },
    { "DEC MCS", CS_DEC_MCS, 1 },
    { "UTF-8", CS_UTF8, 1 },
};

const char *charset_to_localenc(int charset)
{
    int i;

    for (i = 0; i < (int)lenof(localencs); i++)
	if (charset == localencs[i].charset)
	    return localencs[i].name;

    return NULL;		       /* not found */
}

int charset_from_localenc(const char *name)
{
    int i;

    if ( (i = charset_from_mimeenc(name)) != CS_NONE)
	return i;
    if ( (i = charset_from_xenc(name)) != CS_NONE)
	return i;

    for (i = 0; i < (int)lenof(localencs); i++) {
	const char *p, *q;
	p = name;
	q = localencs[i].name;
	while (*p || *q) {
	    if (tolower(*p) != tolower(*q))
		break;
	    p++; q++;
	}
	if (!*p && !*q)
	    return localencs[i].charset;
    }

    return CS_NONE;		       /* not found */
}

int charset_localenc_nth(int n)
{
    int i;

    for (i = 0; i < (int)lenof(localencs); i++)
	if (localencs[i].return_in_enum && !n--)
	    return localencs[i].charset;

    return CS_NONE;		       /* end of list */
}
