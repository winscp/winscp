/* $Id$ */
/*
 * Copyright (c) 2003 Ben Harris
 * All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
/*
 * macenc.c -- Convert a Mac OS script/region/font combination to our
 * internal charset code.
 */

#include <string.h>

#include "charset.h"
#include "internal.h"

/*
 * These are defined by Mac OS's <Script.h>, but we'd like to be
 * independent of that.
 */

#define smRoman			0
#define smJapanese		1
#define smTradChinese		2
#define smKorean		3
#define smArabic		4
#define smHebrew		5
#define smCyrillic		7
#define smDevenagari		9
#define smGurmukhi		10
#define smGujurati		11
#define smThai			21
#define smSimpChinese		25
#define smTibetan		26
#define smEthiopic		28
#define smCentralEuroRoman	29

#define verGreece		20
#define verIceland		21
#define verTurkey		24
#define verYugoCroatian		25
#define verRomania		39
#define verFaroeIsl		47
#define verIran			48
#define verRussia		49
#define verSlovenian		66
#define verCroatia		68
#define verBulgaria		72
#define verScottishGaelic	75
#define verManxGaelic		76
#define verBreton		77
#define verNunavut		78
#define verWelsh		79
#define verIrishGaelicScript	81

static const struct {
    int script;
    int region;
    int sysvermin;
    char const *fontname;
    int charset;
} macencs[] = {
    { smRoman, -1,                   0x850, "VT100", CS_MAC_VT100 },
    { smRoman, -1,                   0,     "VT100", CS_MAC_VT100_OLD },
    /*
     * From here on, this table is largely derived from
     * <http://www.unicode.org/Public/MAPPINGS/VENDORS/APPLE/README.TXT>,
     * with _OLD version added based on the comments in individual
     * mapping files.
     */
    { smRoman, -1,                   0,     "Symbol", CS_MAC_SYMBOL },
    { smRoman, -1,                   0,     "Zapf Dingbats", CS_MAC_DINGBATS },
    { smRoman, verTurkey,            0,     NULL,    CS_MAC_TURKISH },
    { smRoman, verYugoCroatian,      0x850, NULL,    CS_MAC_CROATIAN },
    { smRoman, verYugoCroatian,      0,     NULL,    CS_MAC_CROATIAN_OLD },
    { smRoman, verSlovenian,         0x850, NULL,    CS_MAC_CROATIAN },
    { smRoman, verSlovenian,         0,     NULL,    CS_MAC_CROATIAN_OLD },
    { smRoman, verCroatia,           0x850, NULL,    CS_MAC_CROATIAN },
    { smRoman, verCroatia,           0,     NULL,    CS_MAC_CROATIAN_OLD },
    { smRoman, verIceland,           0x850, NULL,    CS_MAC_ICELAND },
    { smRoman, verIceland,           0,     NULL,    CS_MAC_ICELAND_OLD },
    { smRoman, verFaroeIsl,          0x850, NULL,    CS_MAC_ICELAND },
    { smRoman, verFaroeIsl,          0,     NULL,    CS_MAC_ICELAND_OLD },
    { smRoman, verRomania,           0x850, NULL,    CS_MAC_ROMANIAN },
    { smRoman, verRomania,           0,     NULL,    CS_MAC_ROMANIAN_OLD },
#if 0 /* No mapping table on ftp.unicode.org */
    { smRoman, verIreland,           0x850, NULL,    CS_MAC_CELTIC },
    { smRoman, verIreland,           0,     NULL,    CS_MAC_CELTIC_OLD },
    { smRoman, verScottishGaelic,    0x850, NULL,    CS_MAC_CELTIC },
    { smRoman, verScottishGaelic,    0,     NULL,    CS_MAC_CELTIC_OLD },
    { smRoman, verManxGaelic,        0x850, NULL,    CS_MAC_CELTIC },
    { smRoman, verManxGaelic,        0,     NULL,    CS_MAC_CELTIC_OLD },
    { smRoman, verBreton,            0x850, NULL,    CS_MAC_CELTIC },
    { smRoman, verBreton,            0,     NULL,    CS_MAC_CELTIC_OLD },
    { smRoman, verWelsh,             0x850, NULL,    CS_MAC_CELTIC },
    { smRoman, verWelsh,             0,     NULL,    CS_MAC_CELTIC_OLD },
    { smRoman, verIrishGaelicScript, 0x850, NULL,    CS_MAC_GAELIC },
    { smRoman, verIrishGaelicScript, 0,     NULL,    CS_MAC_GAELIC_OLD },
#endif
    { smRoman, verGreece,            0x922, NULL,    CS_MAC_GREEK },
    { smRoman, verGreece,            0,     NULL,    CS_MAC_GREEK_OLD },
    { smRoman, -1,                   0x850, NULL,    CS_MAC_ROMAN },
    { smRoman, -1,                   0,     NULL,    CS_MAC_ROMAN_OLD },
#if 0 /* Multi-byte encodings, not yet supported */
    { smJapanese,    -1,             0,     NULL,    CS_MAC_JAPANESE },
    { smTradChinese, -1,             0,     NULL,    CS_MAC_CHINTRAD },
    { smKorean,      -1,             0,     NULL,    CS_MAC_KOREAN },
#endif
#if 0 /* Bidirectional encodings, not yet supported */
    { smArabic, verIran,             0,     NULL,    CS_MAC_FARSI },
    { smArabic, -1,                  0,     NULL,    CS_MAC_ARABIC },
    { smHebrew, -1,                  0,     NULL,    CS_MAC_HEBREW },
#endif
    { smCyrillic, -1,                0x900, NULL,    CS_MAC_CYRILLIC },
    { smCyrillic, verRussia,         0,     NULL,    CS_MAC_CYRILLIC_OLD },
    { smCyrillic, verBulgaria,       0,     NULL,    CS_MAC_CYRILLIC_OLD },
    { smCyrillic, -1,                0,     NULL,    CS_MAC_UKRAINE },
#if 0 /* Complex Indic scripts, not yet supported */
    { smDevanagari, -1,              0,     NULL,    CS_MAC_DEVENAGA },
    { smGurmukhi, -1,                0,     NULL,    CS_MAC_GURMUKHI },
    { smGujurati, -1,                0,     NULL,    CS_MAC_GUJURATI },
#endif
    { smThai,  -1,                   0,     NULL,    CS_MAC_THAI },
#if 0 /* Multi-byte encoding, not yet supported */
    { smSimpChinese, -1,             0,     NULL,    CS_MAC_CHINSIMP },
#endif
#if 0 /* No mapping table on ftp.unicode.org */
    { smTibetan, -1,                 0,     NULL,    CS_MAC_TIBETAN },
    { smEthiopic, -1,                0,     NULL,    CS_MAC_ETHIOPIC },
    { smEthiopic, verNanavut,        0,     NULL,    CS_MAC_INUIT },
#endif
    { smCentralEuroRoman, -1,        0,     NULL,    CS_MAC_CENTEURO },
};

int charset_from_macenc(int script, int region, int sysvers,
			char const *fontname)
{
    int i;

    for (i = 0; i < (int)lenof(macencs); i++)
	if ((macencs[i].script == script) &&
	    (macencs[i].region < 0 || macencs[i].region == region) &&
	    (macencs[i].sysvermin <= sysvers) &&
	    (macencs[i].fontname == NULL ||
	     (fontname != NULL && strcmp(macencs[i].fontname, fontname) == 0)))
	    return macencs[i].charset;

    return CS_NONE;
}
