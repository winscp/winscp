#include <assert.h>
#include <stddef.h>
#include <string.h>

#include "marshal.h"
#include "misc.h"

void BinarySink_put_data(BinarySink *bs, const void *data, size_t len)
{
    bs->write(bs, data, len);
}

void BinarySink_put_datapl(BinarySink *bs, ptrlen pl)
{
    BinarySink_put_data(bs, pl.ptr, pl.len);
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

void BinarySink_put_bool(BinarySink *bs, bool val)
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

void BinarySink_put_uint64(BinarySink *bs, uint64_t val)
{
    unsigned char data[8];
    PUT_64BIT_MSB_FIRST(data, val);
    bs->write(bs, data, sizeof(data));
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

bool BinarySink_put_pstring(BinarySink *bs, const char *str)
{
    size_t len = strlen(str);
    if (len > 255)
        return false; /* can't write a Pascal-style string this long */
    BinarySink_put_byte(bs, len);
    bs->write(bs, str, len);
    return true;
}

/* ---------------------------------------------------------------------- */

static bool BinarySource_data_avail(BinarySource *src, size_t wanted)
{
    if (src->err)
        return false;

    if (wanted <= src->len - src->pos)
        return true;

    src->err = BSE_OUT_OF_DATA;
    return false;
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

bool BinarySource_get_bool(BinarySource *src)
{
    const unsigned char *ucp;

    if (!avail(1))
        return false;

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

uint64_t BinarySource_get_uint64(BinarySource *src)
{
    const unsigned char *ucp;

    if (!avail(8))
        return 0;

    ucp = consume(8);
    return GET_64BIT_MSB_FIRST(ucp);
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

static void stdio_sink_write(BinarySink *bs, const void *data, size_t len)
{
    stdio_sink *sink = BinarySink_DOWNCAST(bs, stdio_sink);
    fwrite(data, 1, len, sink->fp);
}

void stdio_sink_init(stdio_sink *sink, FILE *fp)
{
    sink->fp = fp;
    BinarySink_INIT(sink, stdio_sink_write);
}

static void bufchain_sink_write(BinarySink *bs, const void *data, size_t len)
{
    bufchain_sink *sink = BinarySink_DOWNCAST(bs, bufchain_sink);
    bufchain_add(sink->ch, data, len);
}

void bufchain_sink_init(bufchain_sink *sink, bufchain *ch)
{
    sink->ch = ch;
    BinarySink_INIT(sink, bufchain_sink_write);
}
