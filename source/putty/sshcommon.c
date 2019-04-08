/*
 * Supporting routines used in common by all the various components of
 * the SSH system.
 */

#include <assert.h>

#include "ssh.h"
#include "sshchan.h"

/* ----------------------------------------------------------------------
 * Implementation of PacketQueue.
 */

void pq_base_push(PacketQueueBase *pqb, PacketQueueNode *node)
{
    assert(!node->next);
    assert(!node->prev);
    node->next = &pqb->end;
    node->prev = pqb->end.prev;
    node->next->prev = node;
    node->prev->next = node;
}

void pq_base_push_front(PacketQueueBase *pqb, PacketQueueNode *node)
{
    assert(!node->next);
    assert(!node->prev);
    node->prev = &pqb->end;
    node->next = pqb->end.next;
    node->next->prev = node;
    node->prev->next = node;
}

static PktIn *pq_in_get(PacketQueueBase *pqb, int pop)
{
    PacketQueueNode *node = pqb->end.next;
    if (node == &pqb->end)
        return NULL;

    if (pop) {
        node->next->prev = node->prev;
        node->prev->next = node->next;
        node->prev = node->next = NULL;
    }

    return FROMFIELD(node, PktIn, qnode);
}

static PktOut *pq_out_get(PacketQueueBase *pqb, int pop)
{
    PacketQueueNode *node = pqb->end.next;
    if (node == &pqb->end)
        return NULL;

    if (pop) {
        node->next->prev = node->prev;
        node->prev->next = node->next;
        node->prev = node->next = NULL;
    }

    return FROMFIELD(node, PktOut, qnode);
}

void pq_in_init(PktInQueue *pq)
{
    pq->pqb.end.next = pq->pqb.end.prev = &pq->pqb.end;
    pq->get = pq_in_get;
}

void pq_out_init(PktOutQueue *pq)
{
    pq->pqb.end.next = pq->pqb.end.prev = &pq->pqb.end;
    pq->get = pq_out_get;
}

void pq_in_clear(PktInQueue *pq)
{
    PktIn *pkt;
    while ((pkt = pq_pop(pq)) != NULL)
        ssh_unref_packet(pkt);
}

void pq_out_clear(PktOutQueue *pq)
{
    PktOut *pkt;
    while ((pkt = pq_pop(pq)) != NULL)
        ssh_free_pktout(pkt);
}

int pq_base_empty_on_to_front_of(PacketQueueBase *src, PacketQueueBase *dest)
{
    struct PacketQueueNode *srcfirst, *srclast;

    if (src->end.next == &src->end)
        return FALSE;

    srcfirst = src->end.next;
    srclast = src->end.prev;
    srcfirst->prev = &dest->end;
    srclast->next = dest->end.next;
    srcfirst->prev->next = srcfirst;
    srclast->next->prev = srclast;
    src->end.next = src->end.prev = &src->end;

    return TRUE;
}

/* ----------------------------------------------------------------------
 * Low-level functions for the packet structures themselves.
 */

static void ssh_pkt_BinarySink_write(BinarySink *bs,
                                     const void *data, size_t len);
PktOut *ssh_new_packet(void)
{
    PktOut *pkt = snew(PktOut);

    BinarySink_INIT(pkt, ssh_pkt_BinarySink_write);
    pkt->data = NULL;
    pkt->length = 0;
    pkt->maxlen = 0;
    pkt->downstream_id = 0;
    pkt->additional_log_text = NULL;
    pkt->qnode.next = pkt->qnode.prev = NULL;

    return pkt;
}

static void ssh_pkt_ensure(PktOut *pkt, int length)
{
    if (pkt->maxlen < length) {
        pkt->maxlen = length + 256;
        pkt->data = sresize(pkt->data, pkt->maxlen, unsigned char);
    }
}
static void ssh_pkt_adddata(PktOut *pkt, const void *data, int len)
{
    pkt->length += len;
    ssh_pkt_ensure(pkt, pkt->length);
    memcpy(pkt->data + pkt->length - len, data, len);
}

static void ssh_pkt_BinarySink_write(BinarySink *bs,
                                     const void *data, size_t len)
{
    PktOut *pkt = BinarySink_DOWNCAST(bs, PktOut);
    ssh_pkt_adddata(pkt, data, len);
}

void ssh_unref_packet(PktIn *pkt)
{
    if (--pkt->refcount <= 0)
        sfree(pkt);
}

void ssh_free_pktout(PktOut *pkt)
{
    sfree(pkt->data);
    sfree(pkt);
}
