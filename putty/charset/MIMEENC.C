/*
 * mimeenc.c - translate our internal character set codes to and
 * from MIME standard character-set names.
 * 
 */

#include <ctype.h>
#include "charset.h"
#include "internal.h"

static const struct {
    const char *name;
    int charset;
} mimeencs[] = {
    /*
     * These names are taken from
     * 
     *   http://www.iana.org/assignments/character-sets
     * 
     * Where multiple encoding names map to the same encoding id
     * (such as the variety of aliases for ISO-8859-1), the first
     * is considered canonical and will be returned when
     * translating the id to a string.
     */
    { "ISO-8859-1", CS_ISO8859_1 },
    { "iso-ir-100", CS_ISO8859_1 },
    { "ISO_8859-1", CS_ISO8859_1 },
    { "ISO_8859-1:1987", CS_ISO8859_1 },
    { "latin1", CS_ISO8859_1 },
    { "l1", CS_ISO8859_1 },
    { "IBM819", CS_ISO8859_1 },
    { "CP819", CS_ISO8859_1 },
    { "csISOLatin1", CS_ISO8859_1 },

    { "ISO-8859-2", CS_ISO8859_2 },
    { "ISO_8859-2:1987", CS_ISO8859_2 },
    { "iso-ir-101", CS_ISO8859_2 },
    { "ISO_8859-2", CS_ISO8859_2 },
    { "latin2", CS_ISO8859_2 },
    { "l2", CS_ISO8859_2 },
    { "csISOLatin2", CS_ISO8859_2 },

    { "ISO-8859-3", CS_ISO8859_3 },
    { "ISO_8859-3:1988", CS_ISO8859_3 },
    { "iso-ir-109", CS_ISO8859_3 },
    { "ISO_8859-3", CS_ISO8859_3 },
    { "latin3", CS_ISO8859_3 },
    { "l3", CS_ISO8859_3 },
    { "csISOLatin3", CS_ISO8859_3 },

    { "ISO-8859-4", CS_ISO8859_4 },
    { "ISO_8859-4:1988", CS_ISO8859_4 },
    { "iso-ir-110", CS_ISO8859_4 },
    { "ISO_8859-4", CS_ISO8859_4 },
    { "latin4", CS_ISO8859_4 },
    { "l4", CS_ISO8859_4 },
    { "csISOLatin4", CS_ISO8859_4 },

    { "ISO-8859-5", CS_ISO8859_5 },
    { "ISO_8859-5:1988", CS_ISO8859_5 },
    { "iso-ir-144", CS_ISO8859_5 },
    { "ISO_8859-5", CS_ISO8859_5 },
    { "cyrillic", CS_ISO8859_5 },
    { "csISOLatinCyrillic", CS_ISO8859_5 },

    { "ISO-8859-6", CS_ISO8859_6 },
    { "ISO_8859-6:1987", CS_ISO8859_6 },
    { "iso-ir-127", CS_ISO8859_6 },
    { "ISO_8859-6", CS_ISO8859_6 },
    { "ECMA-114", CS_ISO8859_6 },
    { "ASMO-708", CS_ISO8859_6 },
    { "arabic", CS_ISO8859_6 },
    { "csISOLatinArabic", CS_ISO8859_6 },

    { "ISO-8859-7", CS_ISO8859_7 },
    { "ISO_8859-7:1987", CS_ISO8859_7 },
    { "iso-ir-126", CS_ISO8859_7 },
    { "ISO_8859-7", CS_ISO8859_7 },
    { "ELOT_928", CS_ISO8859_7 },
    { "ECMA-118", CS_ISO8859_7 },
    { "greek", CS_ISO8859_7 },
    { "greek8", CS_ISO8859_7 },
    { "csISOLatinGreek", CS_ISO8859_7 },

    { "ISO-8859-8", CS_ISO8859_8 },
    { "ISO_8859-8:1988", CS_ISO8859_8 },
    { "iso-ir-138", CS_ISO8859_8 },
    { "ISO_8859-8", CS_ISO8859_8 },
    { "hebrew", CS_ISO8859_8 },
    { "csISOLatinHebrew", CS_ISO8859_8 },

    { "ISO-8859-9", CS_ISO8859_9 },
    { "ISO_8859-9:1989", CS_ISO8859_9 },
    { "iso-ir-148", CS_ISO8859_9 },
    { "ISO_8859-9", CS_ISO8859_9 },
    { "latin5", CS_ISO8859_9 },
    { "l5", CS_ISO8859_9 },
    { "csISOLatin5", CS_ISO8859_9 },

    { "ISO-8859-10", CS_ISO8859_10 },
    { "iso-ir-157", CS_ISO8859_10 },
    { "l6", CS_ISO8859_10 },
    { "ISO_8859-10:1992", CS_ISO8859_10 },
    { "csISOLatin6", CS_ISO8859_10 },
    { "latin6", CS_ISO8859_10 },

    { "ISO-8859-13", CS_ISO8859_13 },

    { "ISO-8859-14", CS_ISO8859_14 },
    { "iso-ir-199", CS_ISO8859_14 },
    { "ISO_8859-14:1998", CS_ISO8859_14 },
    { "ISO_8859-14", CS_ISO8859_14 },
    { "latin8", CS_ISO8859_14 },
    { "iso-celtic", CS_ISO8859_14 },
    { "l8", CS_ISO8859_14 },

    { "ISO-8859-15", CS_ISO8859_15 },
    { "ISO_8859-15", CS_ISO8859_15 },
    { "Latin-9", CS_ISO8859_15 },

    { "ISO-8859-16", CS_ISO8859_16 },
    { "iso-ir-226", CS_ISO8859_16 },
    { "ISO_8859-16", CS_ISO8859_16 },
    { "ISO_8859-16:2001", CS_ISO8859_16 },
    { "latin10", CS_ISO8859_16 },
    { "l10", CS_ISO8859_16 },

    { "IBM437", CS_CP437 },
    { "cp437", CS_CP437 },
    { "437", CS_CP437 },
    { "csPC8CodePage437", CS_CP437 },

    { "IBM850", CS_CP850 },
    { "cp850", CS_CP850 },
    { "850", CS_CP850 },
    { "csPC850Multilingual", CS_CP850 },

    { "windows-1250", CS_CP1250 },

    { "windows-1251", CS_CP1251 },

    { "windows-1252", CS_CP1252 },

    { "windows-1253", CS_CP1253 },

    { "windows-1254", CS_CP1254 },

    { "windows-1255", CS_CP1255 },

    { "windows-1256", CS_CP1256 },

    { "windows-1257", CS_CP1257 },

    { "windows-1258", CS_CP1258 },

    { "KOI8-R", CS_KOI8_R },
    { "csKOI8R", CS_KOI8_R },

    { "KOI8-U", CS_KOI8_U },

    { "macintosh", CS_MAC_ROMAN_OLD },
    { "mac", CS_MAC_ROMAN_OLD },
    { "csMacintosh", CS_MAC_ROMAN_OLD },

    { "VISCII", CS_VISCII },
    { "csVISCII", CS_VISCII },

    { "hp-roman8", CS_HP_ROMAN8 },
    { "roman8", CS_HP_ROMAN8 },
    { "r8", CS_HP_ROMAN8 },
    { "csHPRoman8", CS_HP_ROMAN8 },

    { "DEC-MCS", CS_DEC_MCS },
    { "dec", CS_DEC_MCS },
    { "csDECMCS", CS_DEC_MCS },

    { "UTF-8", CS_UTF8 },
};

const char *charset_to_mimeenc(int charset)
{
    int i;

    for (i = 0; i < (int)lenof(mimeencs); i++)
	if (charset == mimeencs[i].charset)
	    return mimeencs[i].name;

    return NULL;		       /* not found */
}

int charset_from_mimeenc(const char *name)
{
    int i;

    for (i = 0; i < (int)lenof(mimeencs); i++) {
	const char *p, *q;
	p = name;
	q = mimeencs[i].name;
	while (*p || *q) {
	    if (tolower(*p) != tolower(*q))
		break;
	    p++; q++;
	}
	if (!*p && !*q)
	    return mimeencs[i].charset;
    }

    return CS_NONE;		       /* not found */
}
