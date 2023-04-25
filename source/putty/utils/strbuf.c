/*
 * Functions to work with strbufs.
 */

#include "defs.h"
#include "misc.h"
#include "utils/utils.h"

struct strbuf_impl {
    size_t size;
    struct strbuf visible;
    bool nm;          /* true if we insist on non-moving buffer resizes */
};

#define STRBUF_SET_UPTR(buf)                                    \
    ((buf)->visible.u = (unsigned char *)(buf)->visible.s)
#define STRBUF_SET_PTR(buf, ptr)                                \
    ((buf)->visible.s = (ptr), STRBUF_SET_UPTR(buf))

void *strbuf_append(strbuf *buf_o, size_t len)
{
    struct strbuf_impl *buf = container_of(buf_o, struct strbuf_impl, visible);
    char *toret;
    sgrowarray_general(
        buf->visible.s, buf->size, buf->visible.len + 1, len, buf->nm);
    STRBUF_SET_UPTR(buf);
    toret = buf->visible.s + buf->visible.len;
    buf->visible.len += len;
    buf->visible.s[buf->visible.len] = '\0';
    return toret;
}

void strbuf_shrink_to(strbuf *buf, size_t new_len)
{
    assert(new_len <= buf->len);
    buf->len = new_len;
    buf->s[buf->len] = '\0';
}

void strbuf_shrink_by(strbuf *buf, size_t amount_to_remove)
{
    assert(amount_to_remove <= buf->len);
    buf->len -= amount_to_remove;
    buf->s[buf->len] = '\0';
}

bool strbuf_chomp(strbuf *buf, char char_to_remove)
{
    if (buf->len > 0 && buf->s[buf->len-1] == char_to_remove) {
        strbuf_shrink_by(buf, 1);
        return true;
    }
    return false;
}

static void strbuf_BinarySink_write(
    BinarySink *bs, const void *data, size_t len)
{
    strbuf *buf_o = BinarySink_DOWNCAST(bs, strbuf);
    memcpy(strbuf_append(buf_o, len), data, len);
}

static void strbuf_BinarySink_writefmtv(
    BinarySink *bs, const char *fmt, va_list ap)
{
    strbuf *buf_o = BinarySink_DOWNCAST(bs, strbuf);
    struct strbuf_impl *buf = container_of(buf_o, struct strbuf_impl, visible);
    STRBUF_SET_PTR(buf, dupvprintf_inner(buf->visible.s, buf->visible.len,
                                         &buf->size, fmt, ap));
    buf->visible.len += strlen(buf->visible.s + buf->visible.len);
}

static strbuf *strbuf_new_general(bool nm)
{
    struct strbuf_impl *buf = snew(struct strbuf_impl);
    BinarySink_INIT(&buf->visible, strbuf_BinarySink_write);
    buf->visible.binarysink_->writefmtv = strbuf_BinarySink_writefmtv;
    buf->visible.len = 0;
    buf->size = 512;
    buf->nm = nm;
    STRBUF_SET_PTR(buf, snewn(buf->size, char));
    *buf->visible.s = '\0';
    return &buf->visible;
}
strbuf *strbuf_new(void) { return strbuf_new_general(false); }
strbuf *strbuf_new_nm(void) { return strbuf_new_general(true); }
void strbuf_free(strbuf *buf_o)
{
    struct strbuf_impl *buf = container_of(buf_o, struct strbuf_impl, visible);
    if (buf->visible.s) {
        smemclr(buf->visible.s, buf->size);
        sfree(buf->visible.s);
    }
    sfree(buf);
}
char *strbuf_to_str(strbuf *buf_o)
{
    struct strbuf_impl *buf = container_of(buf_o, struct strbuf_impl, visible);
    char *ret = buf->visible.s;
    sfree(buf);
    return ret;
}
strbuf *strbuf_new_for_agent_query(void)
{
    strbuf *buf = strbuf_new();
    strbuf_append(buf, 4);
    return buf;
}
void strbuf_finalise_agent_query(strbuf *buf_o)
{
    struct strbuf_impl *buf = container_of(buf_o, struct strbuf_impl, visible);
    assert(buf->visible.len >= 5);
    PUT_32BIT_MSB_FIRST(buf->visible.u, buf->visible.len - 4);
}
