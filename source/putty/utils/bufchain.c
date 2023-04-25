/*
 * Generic routines to deal with send buffers: a linked list of
 * smallish blocks, with the operations
 *
 *  - add an arbitrary amount of data to the end of the list
 *  - remove the first N bytes from the list
 *  - return a (pointer,length) pair giving some initial data in
 *    the list, suitable for passing to a send or write system
 *    call
 *  - retrieve a larger amount of initial data from the list
 *  - return the current size of the buffer chain in bytes
 */

#include "defs.h"
#include "misc.h"

#define BUFFER_MIN_GRANULE  512

struct bufchain_granule {
    struct bufchain_granule *next;
    char *bufpos, *bufend, *bufmax;
};

static void uninitialised_queue_idempotent_callback(IdempotentCallback *ic)
{
    unreachable("bufchain callback used while uninitialised");
}

void bufchain_init(bufchain *ch)
{
    ch->head = ch->tail = NULL;
    ch->buffersize = 0;
    ch->ic = NULL;
    ch->queue_idempotent_callback = uninitialised_queue_idempotent_callback;
}

void bufchain_clear(bufchain *ch)
{
    struct bufchain_granule *b;
    while (ch->head) {
        b = ch->head;
        ch->head = ch->head->next;
        smemclr(b, sizeof(*b));
        sfree(b);
    }
    ch->tail = NULL;
    ch->buffersize = 0;
}

size_t bufchain_size(bufchain *ch)
{
    return ch->buffersize;
}

void bufchain_set_callback_inner(
    bufchain *ch, IdempotentCallback *ic,
    void (*queue_idempotent_callback)(IdempotentCallback *ic))
{
    ch->queue_idempotent_callback = queue_idempotent_callback;
    ch->ic = ic;
}

void bufchain_add(bufchain *ch, const void *data, size_t len)
{
    const char *buf = (const char *)data;

    if (len == 0) return;

    ch->buffersize += len;

    while (len > 0) {
        if (ch->tail && ch->tail->bufend < ch->tail->bufmax) {
            size_t copylen = min(len, ch->tail->bufmax - ch->tail->bufend);
            memcpy(ch->tail->bufend, buf, copylen);
            buf += copylen;
            len -= copylen;
            ch->tail->bufend += copylen;
        }
        if (len > 0) {
            size_t grainlen =
                max(sizeof(struct bufchain_granule) + len, BUFFER_MIN_GRANULE);
            struct bufchain_granule *newbuf;
            newbuf = smalloc(grainlen);
            newbuf->bufpos = newbuf->bufend =
                (char *)newbuf + sizeof(struct bufchain_granule);
            newbuf->bufmax = (char *)newbuf + grainlen;
            newbuf->next = NULL;
            if (ch->tail)
                ch->tail->next = newbuf;
            else
                ch->head = newbuf;
            ch->tail = newbuf;
        }
    }

    if (ch->ic)
        ch->queue_idempotent_callback(ch->ic);
}

void bufchain_consume(bufchain *ch, size_t len)
{
    struct bufchain_granule *tmp;

    assert(ch->buffersize >= len);
    while (len > 0) {
        int remlen = len;
        assert(ch->head != NULL);
        if (remlen >= ch->head->bufend - ch->head->bufpos) {
            remlen = ch->head->bufend - ch->head->bufpos;
            tmp = ch->head;
            ch->head = tmp->next;
            if (!ch->head)
                ch->tail = NULL;
            smemclr(tmp, sizeof(*tmp));
            sfree(tmp);
        } else
            ch->head->bufpos += remlen;
        ch->buffersize -= remlen;
        len -= remlen;
    }
}

ptrlen bufchain_prefix(bufchain *ch)
{
    return make_ptrlen(ch->head->bufpos, ch->head->bufend - ch->head->bufpos);
}

void bufchain_fetch(bufchain *ch, void *data, size_t len)
{
    struct bufchain_granule *tmp;
    char *data_c = (char *)data;

    tmp = ch->head;

    assert(ch->buffersize >= len);
    while (len > 0) {
        int remlen = len;

        assert(tmp != NULL);
        if (remlen >= tmp->bufend - tmp->bufpos)
            remlen = tmp->bufend - tmp->bufpos;
        memcpy(data_c, tmp->bufpos, remlen);

        tmp = tmp->next;
        len -= remlen;
        data_c += remlen;
    }
}

void bufchain_fetch_consume(bufchain *ch, void *data, size_t len)
{
    bufchain_fetch(ch, data, len);
    bufchain_consume(ch, len);
}

bool bufchain_try_fetch(bufchain *ch, void *data, size_t len)
{
    if (ch->buffersize >= len) {
        bufchain_fetch(ch, data, len);
        return true;
    } else {
        return false;
    }
}

bool bufchain_try_consume(bufchain *ch, size_t len)
{
    if (ch->buffersize >= len) {
        bufchain_consume(ch, len);
        return true;
    } else {
        return false;
    }
}

bool bufchain_try_fetch_consume(bufchain *ch, void *data, size_t len)
{
    if (ch->buffersize >= len) {
        bufchain_fetch_consume(ch, data, len);
        return true;
    } else {
        return false;
    }
}

size_t bufchain_fetch_consume_up_to(bufchain *ch, void *data, size_t len)
{
    if (len > ch->buffersize)
        len = ch->buffersize;
    if (len)
        bufchain_fetch_consume(ch, data, len);
    return len;
}
