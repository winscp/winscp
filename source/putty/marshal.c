#include <assert.h>
#include <stddef.h>
#include <string.h>

#include "marshal.h"
#include "misc.h"
#include "int64.h"

void BinarySink_put_data(BinarySink *bs, const void *data, size_t len)
{
    bs->write(bs, data, len);
}

void BinarySink_put_padding(BinarySink *bs, size_t len, unsigned char padbyte)
{
    char buf[16];
    memset(buf, padbyte, sizeof(buf));
    while (len > 0) {
        size_t thislen = len < sizeof(buf) ? len : sizeof(buf);
        bs->write(bs, buf, thislen);
        len -= thislen;
    }
}

void BinarySink_put_byte(BinarySink *bs, unsigned char val)
{
    bs->write(bs, &val, 1);
}

void BinarySink_put_bool(BinarySink *bs, int val)
{
    unsigned char cval = val ? 1 : 0;
    bs->write(bs, &cval, 1);
}

void BinarySink_put_uint16(BinarySink *bs, unsigned long val)
{
    unsigned char data[2];
    PUT_16BIT_MSB_FIRST(data, val);
    bs->write(bs, data, sizeof(data));
}

void BinarySink_put_uint32(BinarySink *bs, unsigned long val)
{
    unsigned char data[4];
    PUT_32BIT_MSB_FIRST(data, val);
    bs->write(bs, data, sizeof(data));
}

void BinarySink_put_uint64(BinarySink *bs, uint64 val)
{
    BinarySink_put_uint32(bs, val.hi);
    BinarySink_put_uint32(bs, val.lo);
}

void BinarySink_put_string(BinarySink *bs, const void *data, size_t len)
{
    /* Check that the string length fits in a uint32, without doing a
     * potentially implementation-defined shift of more than 31 bits */
    assert((len >> 31) < 2);

    BinarySink_put_uint32(bs, len);
    bs->write(bs, data, len);
}

void BinarySink_put_stringpl(BinarySink *bs, ptrlen pl)
{
    BinarySink_put_string(bs, pl.ptr, pl.len);
}

void BinarySink_put_stringz(BinarySink *bs, const char *str)
{
    BinarySink_put_string(bs, str, strlen(str));
}

void BinarySink_put_stringsb(BinarySink *bs, struct strbuf *buf)
{
    BinarySink_put_string(bs, buf->s, buf->len);
    strbuf_free(buf);
}

void BinarySink_put_asciz(BinarySink *bs, const char *str)
{
    bs->write(bs, str, strlen(str) + 1);
}

int BinarySink_put_pstring(BinarySink *bs, const char *str)
{
    size_t len = strlen(str);
    if (len > 255)
        return FALSE; /* can't write a Pascal-style string this long */
    BinarySink_put_byte(bs, len);
    bs->write(bs, str, len);
    return TRUE;
}

/* ---------------------------------------------------------------------- */

static int BinarySource_data_avail(BinarySource *src, size_t wanted)
{
    if (src->err)
        return FALSE;

    if (wanted <= src->len - src->pos)
        return TRUE;

    src->err = BSE_OUT_OF_DATA;
    return FALSE;
}

#define avail(wanted) BinarySource_data_avail(src, wanted)
#define advance(dist) (src->pos += dist)
#define here ((const void *)((const unsigned char *)src->data + src->pos))
#define consume(dist)                           \
    ((const void *)((const unsigned char *)src->data + \
                    ((src->pos += dist) - dist)))

ptrlen BinarySource_get_data(BinarySource *src, size_t wanted)
{
    if (!avail(wanted))
        return make_ptrlen("", 0);

    return make_ptrlen(consume(wanted), wanted);
}

unsigned char BinarySource_get_byte(BinarySource *src)
{
    const unsigned char *ucp;

    if (!avail(1))
        return 0;

    ucp = consume(1);
    return *ucp;
}

int BinarySource_get_bool(BinarySource *src)
{
    const unsigned char *ucp;

    if (!avail(1))
        return 0;

    ucp = consume(1);
    return *ucp != 0;
}

unsigned BinarySource_get_uint16(BinarySource *src)
{
    const unsigned char *ucp;

    if (!avail(2))
        return 0;

    ucp = consume(2);
    return GET_16BIT_MSB_FIRST(ucp);
}

unsigned long BinarySource_get_uint32(BinarySource *src)
{
    const unsigned char *ucp;

    if (!avail(4))
        return 0;

    ucp = consume(4);
    return GET_32BIT_MSB_FIRST(ucp);
}

uint64 BinarySource_get_uint64(BinarySource *src)
{
    const unsigned char *ucp;
    uint64 toret;

    if (!avail(8)) {
        toret.hi = toret.lo = 0;
        return toret;
    }

    ucp = consume(8);
    toret.hi = GET_32BIT_MSB_FIRST(ucp);
    toret.lo = GET_32BIT_MSB_FIRST(ucp + 4);
    return toret;
}

ptrlen BinarySource_get_string(BinarySource *src)
{
    const unsigned char *ucp;
    size_t len;

    if (!avail(4))
        return make_ptrlen("", 0);

    ucp = consume(4);
    len = GET_32BIT_MSB_FIRST(ucp);

    if (!avail(len))
        return make_ptrlen("", 0);

    return make_ptrlen(consume(len), len);
}

const char *BinarySource_get_asciz(BinarySource *src)
{
    const char *start, *end;

    if (src->err)
        return "";

    start = here;
    end = memchr(start, '\0', src->len - src->pos);
    if (!end) {
        src->err = BSE_OUT_OF_DATA;
        return "";
    }

    advance(end + 1 - start);
    return start;
}

ptrlen BinarySource_get_pstring(BinarySource *src)
{
    const unsigned char *ucp;
    size_t len;

    if (!avail(1))
        return make_ptrlen("", 0);

    ucp = consume(1);
    len = *ucp;

    if (!avail(len))
        return make_ptrlen("", 0);

    return make_ptrlen(consume(len), len);
}
