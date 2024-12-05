/*
 * Decode a single UTF-8 character.
 */

#include "putty.h"
#include "misc.h"

unsigned decode_utf8(BinarySource *src, DecodeUTF8Failure *err)
{
    /* Permit user to pass NULL as the err pointer */
    DecodeUTF8Failure dummy;
    if (!err) err = &dummy;

    /* If the source has no byte available, this will return 0, which
     * we'll return immediately and is a reasonable error return anyway */
    { // WINSCP
    unsigned char c = get_byte(src);

    /* One-byte cases. */
    if (c < 0x80) {
        *err = DUTF8_SUCCESS;
        return c;
    } else if (c < 0xC0) {
        *err = DUTF8_SPURIOUS_CONTINUATION;
        return 0xFFFD;
    }

    { // WINSCP
    unsigned long wc, min;
    size_t ncont;
    if (c < 0xE0) {
        wc = c & 0x1F; ncont = 1; min = 0x80;
    } else if (c < 0xF0) {
        wc = c & 0x0F; ncont = 2; min = 0x800;
    } else if (c < 0xF8) {
        wc = c & 0x07; ncont = 3; min = 0x10000;
    } else if (c < 0xFC) {
        wc = c & 0x03; ncont = 4; min = 0x200000;
    } else if (c < 0xFE) {
        wc = c & 0x01; ncont = 5; min = 0x4000000;
    } else {
        *err = DUTF8_ILLEGAL_BYTE; /* FE or FF */
        return 0xFFFD;
    }

    while (ncont-- > 0) {
        if (!get_avail(src)) {
            *err = DUTF8_E_OUT_OF_DATA;
            return 0xFFFD;
        }
        { // WINSCP
        unsigned char cont = get_byte(src);
        if (!(0x80 <= cont && cont < 0xC0)) {
            BinarySource_REWIND_TO(src, src->pos - 1);
            *err = DUTF8_TRUNCATED_SEQUENCE;
            return 0xFFFD;
        }

        wc = (wc << 6) | (cont & 0x3F);
        } // WINSCP
    }

    if (wc < min) {
        *err = DUTF8_OVERLONG_ENCODING;
        return 0xFFFD;
    }
    if (0xD800 <= wc && wc < 0xE000) {
        *err = DUTF8_ENCODED_SURROGATE;
        return 0xFFFD;
    }
    if (wc > 0x10FFFF) {
        *err = DUTF8_CODE_POINT_TOO_BIG;
        return 0xFFFD;                 /* outside Unicode range */
    }
    *err = DUTF8_SUCCESS;
    return wc;
    } // WINSCP
    } // WINSCP
}

const char *const decode_utf8_error_strings[DUTF8_N_FAILURE_CODES] = {
    #define MSG_ENTRY(sym, string) string,
    DECODE_UTF8_FAILURE_LIST(MSG_ENTRY)
    #undef MSG_ENTRY
};

#ifdef TEST

#include <stdio.h>

void out_of_memory(void)
{
    fprintf(stderr, "out of memory!\n");
    exit(2);
}

static const char *const decode_utf8_error_syms[DUTF8_N_FAILURE_CODES] = {
    #define SYM_ENTRY(sym, string) #sym,
    DECODE_UTF8_FAILURE_LIST(SYM_ENTRY)
    #undef SYM_ENTRY
};

bool dotest(const char *file, int line, const char *input, size_t ninput,
            const unsigned long *chars, size_t nchars)
{
    BinarySource src[1];
    BinarySource_BARE_INIT(src, input, ninput);
    size_t noutput = 0;

    printf("%s:%d: test start\n", file, line);

    while (get_avail(src)) {
        size_t before = src->pos;
        DecodeUTF8Failure err;
        unsigned long wc = decode_utf8(src, &err);

        printf("%s:%d in+%"SIZEu" out+%"SIZEu":", file, line, before, noutput);
        while (before < src->pos)
            printf(" %02x", (unsigned)(unsigned char)(input[before++]));
        printf(" -> U-%08lx %s\n", wc, decode_utf8_error_syms[err]);

        if (noutput >= nchars) {
            printf("%s:%d: FAIL: expected no further output\n", file, line);
            return false;
        }

        if (chars[noutput] != wc) {
            printf("%s:%d: FAIL: expected U-%08lx\n",
                   file, line, chars[noutput]);
            return false;
        }

        noutput++;

        DecodeUTF8Failure expected_err;
        if (wc == 0xFFFD) {
            /* In the 'chars' array, any occurrence of 0xFFFD is followed
             * by the expected error code */
            assert(noutput < nchars && "bad test data");
            expected_err = chars[noutput++];
        } else {
            /* Expect success status to go with any non-FFFD character */
            expected_err = DUTF8_SUCCESS;
        }
        if (err != expected_err) {
            printf("%s:%d: FAIL: expected %s\n", file, line,
                   decode_utf8_error_syms[expected_err]);
            return false;
        }
    }

    if (noutput < nchars) {
        printf("%s:%d: FAIL: expected further output\n", file, line);
        return false;
    }

    printf("%s:%d: pass\n", file, line);
    return true;
}

#define DOTEST(input, ...) do {                                         \
        static const unsigned long chars[] = { __VA_ARGS__ };           \
        ntest++;                                                        \
        if (dotest(__FILE__, __LINE__, input, sizeof(input)-1,          \
                   chars, lenof(chars)))                                \
            npass++;                                                    \
    } while (0)

int main(void)
{
    int ntest = 0, npass = 0;

    DOTEST("\xCE\xBA\xE1\xBD\xB9\xCF\x83\xCE\xBC\xCE\xB5",
           0x03BA, 0x1F79, 0x03C3, 0x03BC, 0x03B5);

    /* First sequence of each length */
    DOTEST("\x00", 0x0000);
    DOTEST("\xC2\x80", 0x0080);
    DOTEST("\xE0\xA0\x80", 0x0800);
    DOTEST("\xF0\x90\x80\x80", 0x00010000);
    DOTEST("\xF8\x88\x80\x80\x80",
           0xFFFD, DUTF8_CODE_POINT_TOO_BIG); /* would be 0x00200000 */
    DOTEST("\xFC\x84\x80\x80\x80\x80",
           0xFFFD, DUTF8_CODE_POINT_TOO_BIG); /* would be 0x04000000 */

    /* Last sequence of each length */
    DOTEST("\x7F", 0x007F);
    DOTEST("\xDF\xBF", 0x07FF);
    DOTEST("\xEF\xBF\xBF", 0xFFFF);
    DOTEST("\xF7\xBF\xBF\xBF",
           0xFFFD, DUTF8_CODE_POINT_TOO_BIG); /* would be 0x001FFFFF */
    DOTEST("\xFB\xBF\xBF\xBF\xBF",
           0xFFFD, DUTF8_CODE_POINT_TOO_BIG); /* would be 0x03FFFFFF */
    DOTEST("\xFD\xBF\xBF\xBF\xBF\xBF",
           0xFFFD, DUTF8_CODE_POINT_TOO_BIG); /* would be 0x7FFFFFFF */

    /* Endpoints of the surrogate range */
    DOTEST("\xED\x9F\xBF", 0xD7FF);
    DOTEST("\xED\xA0\x80", 0xFFFD, DUTF8_ENCODED_SURROGATE); /* 0xD800 */
    DOTEST("\xED\xBF\xBF", 0xFFFD, DUTF8_ENCODED_SURROGATE); /* 0xDFFF */
    DOTEST("\xEE\x80\x80", 0xE000);

    /* REPLACEMENT CHARACTER itself */
    DOTEST("\xEF\xBF\xBD", 0xFFFD, DUTF8_SUCCESS); /* FFFD but no error! */

    /* Endpoints of the legal Unicode range */
    DOTEST("\xF4\x8F\xBF\xBF", 0x0010FFFF);
    DOTEST("\xF4\x90\x80\x80", 0xFFFD,
           DUTF8_CODE_POINT_TOO_BIG); /* would be 0x00110000 */

    /* Spurious continuation bytes, each shown as a separate failure */
    DOTEST("\x80 \x81\x82 \xBD\xBE\xBF",
           0xFFFD, DUTF8_SPURIOUS_CONTINUATION,
           0x0020,
           0xFFFD, DUTF8_SPURIOUS_CONTINUATION,
           0xFFFD, DUTF8_SPURIOUS_CONTINUATION,
           0x0020,
           0xFFFD, DUTF8_SPURIOUS_CONTINUATION,
           0xFFFD, DUTF8_SPURIOUS_CONTINUATION,
           0xFFFD, DUTF8_SPURIOUS_CONTINUATION);

    /* Truncated sequences, each shown as just one failure. The last
     * one gets a different error code because the sequence is
     * interrupted by the end of the string instead of another
     * character, so that if the string were a prefix of a longer
     * chunk of data then that would not _necessarily_ indicate an
     * error */
    DOTEST("\xC2\xE0\xA0\xF0\x90\x80\xF8\x88\x80\x80\xFC\x84\x80\x80\x80",
           0xFFFD, DUTF8_TRUNCATED_SEQUENCE,
           0xFFFD, DUTF8_TRUNCATED_SEQUENCE,
           0xFFFD, DUTF8_TRUNCATED_SEQUENCE,
           0xFFFD, DUTF8_TRUNCATED_SEQUENCE,
           0xFFFD, DUTF8_E_OUT_OF_DATA);
    DOTEST("\xC2 \xE0\xA0 \xF0\x90\x80 \xF8\x88\x80\x80 \xFC\x84\x80\x80\x80",
           0xFFFD, DUTF8_TRUNCATED_SEQUENCE,
           0x0020,
           0xFFFD, DUTF8_TRUNCATED_SEQUENCE,
           0x0020,
           0xFFFD, DUTF8_TRUNCATED_SEQUENCE,
           0x0020,
           0xFFFD, DUTF8_TRUNCATED_SEQUENCE,
           0x0020,
           0xFFFD, DUTF8_E_OUT_OF_DATA);

    /* Illegal bytes */
    DOTEST("\xFE\xFF", 0xFFFD, DUTF8_ILLEGAL_BYTE, 0xFFFD, DUTF8_ILLEGAL_BYTE);

    /* Overlong sequences */
    DOTEST("\xC1\xBF", 0xFFFD, DUTF8_OVERLONG_ENCODING);
    DOTEST("\xE0\x9F\xBF", 0xFFFD, DUTF8_OVERLONG_ENCODING);
    DOTEST("\xF0\x8F\xBF\xBF", 0xFFFD, DUTF8_OVERLONG_ENCODING);
    DOTEST("\xF8\x87\xBF\xBF\xBF", 0xFFFD, DUTF8_OVERLONG_ENCODING);
    DOTEST("\xFC\x83\xBF\xBF\xBF\xBF", 0xFFFD, DUTF8_OVERLONG_ENCODING);

    DOTEST("\xC0\x80", 0xFFFD, DUTF8_OVERLONG_ENCODING);
    DOTEST("\xE0\x80\x80", 0xFFFD, DUTF8_OVERLONG_ENCODING);
    DOTEST("\xF0\x80\x80\x80", 0xFFFD, DUTF8_OVERLONG_ENCODING);
    DOTEST("\xF8\x80\x80\x80\x80", 0xFFFD, DUTF8_OVERLONG_ENCODING);
    DOTEST("\xFC\x80\x80\x80\x80\x80", 0xFFFD, DUTF8_OVERLONG_ENCODING);

    printf("%d tests %d passed", ntest, npass);
    if (npass < ntest) {
        printf(" %d FAILED\n", ntest-npass);
        return 1;
    } else {
        printf("\n");
        return 0;
    }
}
#endif
