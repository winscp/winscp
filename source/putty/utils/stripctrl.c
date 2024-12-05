/*
 * stripctrl.c: a facility for stripping control characters out of a
 * data stream (defined as any multibyte character in the system
 * locale which is neither printable nor \n), using the standard C
 * library multibyte character facilities.
 */

#include <assert.h>
#include <locale.h>
#include <string.h>
#include <wchar.h>
#include <wctype.h>

#include "putty.h"
#ifndef WINSCP
#include "terminal.h"
#endif
#include "misc.h"
#include "marshal.h"

#define SCC_BUFSIZE 64
#define LINE_LIMIT 77

typedef struct StripCtrlCharsImpl StripCtrlCharsImpl;
struct StripCtrlCharsImpl {
    mbstate_t mbs_in, mbs_out;

    bool permit_cr;
    wchar_t substitution;

    char buf[SCC_BUFSIZE];
    size_t buflen;

#ifndef WINSCP
    Terminal *term;
    bool last_term_utf;
    struct term_utf8_decode utf8;
    unsigned long (*translate)(Terminal *, term_utf8_decode *, unsigned char);
#endif

    bool line_limit;
    bool line_start;
    size_t line_chars_remaining;

    BinarySink *bs_out;

    StripCtrlChars public;
};

static void stripctrl_locale_BinarySink_write(
    BinarySink *bs, const void *vp, size_t len);
static void stripctrl_term_BinarySink_write(
    BinarySink *bs, const void *vp, size_t len);

static StripCtrlCharsImpl *stripctrl_new_common(
    BinarySink *bs_out, bool permit_cr, wchar_t substitution)
{
    StripCtrlCharsImpl *scc = snew(StripCtrlCharsImpl);
    memset(scc, 0, sizeof(StripCtrlCharsImpl)); /* zeroes mbstates */
    scc->bs_out = bs_out;
    scc->permit_cr = permit_cr;
    scc->substitution = substitution;
    return scc;
}

#ifndef WINSCP
StripCtrlChars *stripctrl_new(
    BinarySink *bs_out, bool permit_cr, wchar_t substitution)
{
    StripCtrlCharsImpl *scc = stripctrl_new_common(
        bs_out, permit_cr, substitution);
    BinarySink_INIT(&scc->public, stripctrl_locale_BinarySink_write);
    return &scc->public;
}

StripCtrlChars *stripctrl_new_term_fn(
    BinarySink *bs_out, bool permit_cr, wchar_t substitution,
    Terminal *term, unsigned long (*translate)(
        Terminal *, term_utf8_decode *, unsigned char))
{
    StripCtrlCharsImpl *scc = stripctrl_new_common(
        bs_out, permit_cr, substitution);
    scc->term = term;
    scc->translate = translate;
    BinarySink_INIT(&scc->public, stripctrl_term_BinarySink_write);
    return &scc->public;
}
#endif

void stripctrl_retarget(StripCtrlChars *sccpub, BinarySink *new_bs_out)
{
    StripCtrlCharsImpl *scc =
        container_of(sccpub, StripCtrlCharsImpl, public);
    scc->bs_out = new_bs_out;
    stripctrl_reset(sccpub);
}

void stripctrl_reset(StripCtrlChars *sccpub)
{
    StripCtrlCharsImpl *scc =
        container_of(sccpub, StripCtrlCharsImpl, public);

    /*
     * Clear all the fields that might have been in the middle of a
     * multibyte character or non-default shift state, so that we can
     * start converting a fresh piece of data to send to a channel
     * that hasn't seen the previous output.
     */
#ifndef WINSCP
    memset(&scc->utf8, 0, sizeof(scc->utf8));
#endif
    memset(&scc->mbs_in, 0, sizeof(scc->mbs_in));
    memset(&scc->mbs_out, 0, sizeof(scc->mbs_out));

    /*
     * Also, reset the line-limiting system to its starting state.
     */
    scc->line_start = true;
}

void stripctrl_free(StripCtrlChars *sccpub)
{
    StripCtrlCharsImpl *scc =
        container_of(sccpub, StripCtrlCharsImpl, public);
    smemclr(scc, sizeof(StripCtrlCharsImpl));
    sfree(scc);
}

void stripctrl_enable_line_limiting(StripCtrlChars *sccpub)
{
    StripCtrlCharsImpl *scc =
        container_of(sccpub, StripCtrlCharsImpl, public);
    scc->line_limit = true;
    scc->line_start = true;
}

#ifndef WINSCP
static inline bool stripctrl_ctrlchar_ok(StripCtrlCharsImpl *scc, wchar_t wc)
{
    return wc == L'\n' || (wc == L'\r' && scc->permit_cr);
}

static inline void stripctrl_check_line_limit(
    StripCtrlCharsImpl *scc, wchar_t wc, size_t width)
{
    if (!scc->line_limit)
        return;                        /* nothing to do */

    if (scc->line_start) {
        put_datapl(scc->bs_out, PTRLEN_LITERAL("| "));
        scc->line_start = false;
        scc->line_chars_remaining = LINE_LIMIT;
    }

    if (wc == '\n') {
        scc->line_start = true;
        return;
    }

    if (scc->line_chars_remaining < width) {
        put_datapl(scc->bs_out, PTRLEN_LITERAL("\r\n> "));
        scc->line_chars_remaining = LINE_LIMIT;
    }

    assert(width <= scc->line_chars_remaining);
    scc->line_chars_remaining -= width;
}

static inline void stripctrl_locale_put_wc(StripCtrlCharsImpl *scc, wchar_t wc)
{
    int width = mk_wcwidth(wc);
    if ((iswprint(wc) && width >= 0) || stripctrl_ctrlchar_ok(scc, wc)) {
        /* Printable character, or one we're going to let through anyway. */
        if (width < 0)
            width = 0;   /* sanitise for stripctrl_check_line_limit */
    } else if (scc->substitution) {
        wc = scc->substitution;
        width = mk_wcwidth(wc);
        assert(width >= 0);
    } else {
        /* No defined substitution, so don't write any output wchar_t. */
        return;
    }

    stripctrl_check_line_limit(scc, wc, width);

    char outbuf[MB_LEN_MAX];
    size_t produced = wcrtomb(outbuf, wc, &scc->mbs_out);
    if (produced > 0)
        put_data(scc->bs_out, outbuf, produced);
}

static inline void stripctrl_term_put_wc(
    StripCtrlCharsImpl *scc, unsigned long wc)
{
    ptrlen prefix = PTRLEN_LITERAL("");
    int width = term_char_width(scc->term, wc);

    if (!(wc & ~0x9F) || width < 0) {
        /* This is something the terminal interprets as a control
         * character. */
        if (!stripctrl_ctrlchar_ok(scc, wc)) {
            if (!scc->substitution) {
                return;
            } else {
                wc = scc->substitution;
                width = term_char_width(scc->term, wc);
                assert(width >= 0);
            }
        } else {
            if (width < 0)
                width = 0; /* sanitise for stripctrl_check_line_limit */
        }

        if (wc == '\012') {
            /* Precede \n with \r, because our terminal will not
             * generally be in the ONLCR mode where it assumes that
             * internally, and any \r on input has been stripped
             * out. */
            prefix = PTRLEN_LITERAL("\r");
        }
    }

    stripctrl_check_line_limit(scc, wc, width);

    if (prefix.len)
        put_datapl(scc->bs_out, prefix);

    /*
     * The Terminal implementation encodes 7-bit ASCII characters in
     * UTF-8 mode, and all printing characters in non-UTF-8 (i.e.
     * single-byte character set) mode, as values in the surrogate
     * range (a conveniently unused piece of space in this context)
     * whose low byte is the original 1-byte representation of the
     * character.
     */
    if ((wc - 0xD800) < (0xE000 - 0xD800))
        wc &= 0xFF;

    if (in_utf(scc->term)) {
        put_utf8_char(scc->bs_out, wc);
    } else {
        put_byte(scc->bs_out, wc);
    }
}

static inline size_t stripctrl_locale_try_consume(
    StripCtrlCharsImpl *scc, const char *p, size_t len)
{
    wchar_t wc;
    mbstate_t mbs_orig = scc->mbs_in;
    size_t consumed = mbrtowc(&wc, p, len, &scc->mbs_in);

    if (consumed == (size_t)-2) {
        /*
         * The buffer is too short to see the end of the multibyte
         * character that it appears to be starting with. We return 0
         * for 'no data consumed', restore the conversion state from
         * before consuming the partial character, and our caller will
         * come back when it has more data available.
         */
        scc->mbs_in = mbs_orig;
        return 0;
    }

    if (consumed == (size_t)-1) {
        /*
         * The buffer contains an illegal multibyte sequence. There's
         * no really good way to recover from this, so we'll just
         * reset our input state, consume a single byte without
         * emitting anything, and hope we can resynchronise to
         * _something_ sooner or later.
         */
        memset(&scc->mbs_in, 0, sizeof(scc->mbs_in));
        return 1;
    }

    if (consumed == 0) {
        /*
         * A zero wide character is encoded by the data, but mbrtowc
         * hasn't told us how many input bytes it takes. There isn't
         * really anything good we can do here, so we just advance by
         * one byte in the hope that that was the NUL.
         *
         * (If it wasn't - that is, if we're in a multibyte encoding
         * in which the terminator of a normal C string is encoded in
         * some way other than a single zero byte - then probably lots
         * of other things will have gone wrong before we get here!)
         */
        stripctrl_locale_put_wc(scc, L'\0');
        return 1;
    }

    /*
     * Otherwise, this is the easy case: consumed > 0, and we've eaten
     * a valid multibyte character.
     */
    stripctrl_locale_put_wc(scc, wc);
    return consumed;
}

static void stripctrl_locale_BinarySink_write(
    BinarySink *bs, const void *vp, size_t len)
{
    StripCtrlChars *sccpub = BinarySink_DOWNCAST(bs, StripCtrlChars);
    StripCtrlCharsImpl *scc =
        container_of(sccpub, StripCtrlCharsImpl, public);
    const char *p = (const char *)vp;

    char *previous_locale = dupstr(setlocale(LC_CTYPE, NULL));
    setlocale(LC_CTYPE, "");

    /*
     * Deal with any partial multibyte character buffered from last
     * time.
     */
    while (scc->buflen > 0) {
        size_t to_copy = SCC_BUFSIZE - scc->buflen;
        if (to_copy > len)
            to_copy = len;

        memcpy(scc->buf + scc->buflen, p, to_copy);
        { // WINSCP
        size_t consumed = stripctrl_locale_try_consume(
            scc, scc->buf, scc->buflen + to_copy);

        if (consumed >= scc->buflen) {
            /*
             * We've consumed a multibyte character that includes all
             * the data buffered from last time. So we can clear our
             * buffer and move on to processing the main input string
             * in situ, having first discarded whatever initial
             * segment of it completed our previous character.
             */
            size_t consumed_from_main_string = consumed - scc->buflen;
            assert(consumed_from_main_string <= len);
            p += consumed_from_main_string;
            len -= consumed_from_main_string;
            scc->buflen = 0;
            break;
        }

        if (consumed == 0) {
            /*
             * If we didn't manage to consume anything, i.e. the whole
             * buffer contains an incomplete sequence, it had better
             * be because our entire input string _this_ time plus
             * whatever leftover data we had from _last_ time still
             * comes to less than SCC_BUFSIZE. In other words, we've
             * already copied all the new data on to the end of our
             * buffer, and it still hasn't helped. So increment buflen
             * to reflect the new data, and return.
             */
            assert(to_copy == len);
            scc->buflen += to_copy;
            goto out;
        }

        /*
         * Otherwise, we've somehow consumed _less_ data than we had
         * buffered, and yet we weren't able to consume that data in
         * the last call to this function. That sounds impossible, but
         * I can think of one situation in which it could happen: if
         * we had an incomplete MB sequence last time, and now more
         * data has arrived, it turns out to be an _illegal_ one, so
         * we consume one byte in the hope of resynchronising.
         *
         * Anyway, in this case we move the buffer up and go back
         * round this initial loop.
         */
        scc->buflen -= consumed;
        memmove(scc->buf, scc->buf + consumed, scc->buflen);
        } // WINSCP
    }

    /*
     * Now charge along the main string.
     */
    while (len > 0) {
        size_t consumed = stripctrl_locale_try_consume(scc, p, len);
        if (consumed == 0)
            break;
        assert(consumed <= len);
        p += consumed;
        len -= consumed;
    }

    /*
     * Any data remaining should be copied into our buffer, to keep
     * for next time.
     */
    assert(len <= SCC_BUFSIZE);
    memcpy(scc->buf, p, len);
    scc->buflen = len;

  out:
    setlocale(LC_CTYPE, previous_locale);
    sfree(previous_locale);
}

static void stripctrl_term_BinarySink_write(
    BinarySink *bs, const void *vp, size_t len)
{
    StripCtrlChars *sccpub = BinarySink_DOWNCAST(bs, StripCtrlChars);
    StripCtrlCharsImpl *scc =
        container_of(sccpub, StripCtrlCharsImpl, public);

    bool utf = in_utf(scc->term);
    if (utf != scc->last_term_utf) {
        scc->last_term_utf = utf;
        scc->utf8.state = 0;
    }

    for (const unsigned char *p = (const unsigned char *)vp;
         len > 0; len--, p++) {
        unsigned long t = scc->translate(scc->term, &scc->utf8, *p);
        if (t == UCSTRUNCATED) {
            stripctrl_term_put_wc(scc, 0xFFFD);
            /* go round again */
            t = scc->translate(scc->term, &scc->utf8, *p);
        }
        if (t == UCSINCOMPLETE)
            continue;
        if (t == UCSINVALID)
            t = 0xFFFD;

        stripctrl_term_put_wc(scc, t);
    }
}

char *stripctrl_string_ptrlen(StripCtrlChars *sccpub, ptrlen str)
{
    strbuf *out = strbuf_new();
    stripctrl_retarget(sccpub, BinarySink_UPCAST(out));
    put_datapl(sccpub, str);
    stripctrl_retarget(sccpub, NULL);
    return strbuf_to_str(out);
}
#endif

#ifdef STRIPCTRL_TEST

/*
gcc -std=c99 -DSTRIPCTRL_TEST -o scctest stripctrl.c marshal.c utils.c memory.c wcwidth.c -I . -I unix -I charset
*/

void out_of_memory(void) { fprintf(stderr, "out of memory\n"); abort(); }

void stripctrl_write(BinarySink *bs, const void *vdata, size_t len)
{
    const uint8_t *p = vdata;
    printf("[");
    for (size_t i = 0; i < len; i++)
        printf("%*s%02x", i?1:0, "", (unsigned)p[i]);
    printf("]");
}

void stripctrl_test(StripCtrlChars *scc, ptrlen pl)
{
    stripctrl_write(NULL, pl.ptr, pl.len);
    printf(" -> ");
    put_datapl(scc, pl);
    printf("\n");
}

int main(void)
{
    struct foo { BinarySink_IMPLEMENTATION; } foo;
    BinarySink_INIT(&foo, stripctrl_write);
    StripCtrlChars *scc = stripctrl_new(BinarySink_UPCAST(&foo), false, '?');
    stripctrl_test(scc, PTRLEN_LITERAL("a\033[1mb"));
    stripctrl_test(scc, PTRLEN_LITERAL("a\xC2\x9B[1mb"));
    stripctrl_test(scc, PTRLEN_LITERAL("a\xC2\xC2[1mb"));
    stripctrl_test(scc, PTRLEN_LITERAL("\xC3"));
    stripctrl_test(scc, PTRLEN_LITERAL("\xA9"));
    stripctrl_test(scc, PTRLEN_LITERAL("\xE2\x80\x8F"));
    stripctrl_test(scc, PTRLEN_LITERAL("a\0b"));
    stripctrl_free(scc);
    return 0;
}

#endif /* STRIPCTRL_TEST */
