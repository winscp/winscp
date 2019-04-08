/*
 * Supporting routines used in common by all the various components of
 * the SSH system.
 */

#include <assert.h>
#include <stdlib.h>

#include "putty.h"
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

/*
 * Concatenate the contents of the two queues q1 and q2, and leave the
 * result in qdest. qdest must be either empty, or one of the input
 * queues.
 */
void pq_base_concatenate(PacketQueueBase *qdest,
                         PacketQueueBase *q1, PacketQueueBase *q2)
{
    struct PacketQueueNode *head1, *tail1, *head2, *tail2;

    /*
     * Extract the contents from both input queues, and empty them.
     */

    head1 = (q1->end.next == &q1->end ? NULL : q1->end.next);
    tail1 = (q1->end.prev == &q1->end ? NULL : q1->end.prev);
    head2 = (q2->end.next == &q2->end ? NULL : q2->end.next);
    tail2 = (q2->end.prev == &q2->end ? NULL : q2->end.prev);

    q1->end.next = q1->end.prev = &q1->end;
    q2->end.next = q2->end.prev = &q2->end;

    /*
     * Link the two lists together, handling the case where one or
     * both is empty.
     */

    if (tail1)
        tail1->next = head2;
    else
        head1 = head2;

    if (head2)
        head2->prev = tail1;
    else
        tail2 = tail1;

    /*
     * Check the destination queue is currently empty. (If it was one
     * of the input queues, then it will be, because we emptied both
     * of those just a moment ago.)
     */

    assert(qdest->end.next == &qdest->end);
    assert(qdest->end.prev == &qdest->end);

    /*
     * If our concatenated list has anything in it, then put it in
     * dest.
     */

    if (!head1) {
        assert(!tail2);
    } else {
        assert(tail2);
        qdest->end.next = head1;
        qdest->end.prev = tail2;
        head1->prev = &qdest->end;
        tail2->next = &qdest->end;
    }
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
/* ----------------------------------------------------------------------
 * Implement zombiechan_new() and its trivial vtable.
 */

static void zombiechan_free(Channel *chan);
static int zombiechan_send(Channel *chan, int is_stderr, const void *, int);
static void zombiechan_set_input_wanted(Channel *chan, int wanted);
static void zombiechan_do_nothing(Channel *chan);
static void zombiechan_open_failure(Channel *chan, const char *);
static int zombiechan_want_close(Channel *chan, int sent_eof, int rcvd_eof);
static char *zombiechan_log_close_msg(Channel *chan) { return NULL; }

static const struct ChannelVtable zombiechan_channelvt = {
    zombiechan_free,
    zombiechan_do_nothing,             /* open_confirmation */
    zombiechan_open_failure,
    zombiechan_send,
    zombiechan_do_nothing,             /* send_eof */
    zombiechan_set_input_wanted,
    zombiechan_log_close_msg,
    zombiechan_want_close,
};

Channel *zombiechan_new(void)
{
    Channel *chan = snew(Channel);
    chan->vt = &zombiechan_channelvt;
    chan->initial_fixed_window_size = 0;
    return chan;
}

static void zombiechan_free(Channel *chan)
{
    assert(chan->vt == &zombiechan_channelvt);
    sfree(chan);
}

static void zombiechan_do_nothing(Channel *chan)
{
    assert(chan->vt == &zombiechan_channelvt);
}

static void zombiechan_open_failure(Channel *chan, const char *errtext)
{
    assert(chan->vt == &zombiechan_channelvt);
}

static int zombiechan_send(Channel *chan, int is_stderr,
                           const void *data, int length)
{
    assert(chan->vt == &zombiechan_channelvt);
    return 0;
}

static void zombiechan_set_input_wanted(Channel *chan, int enable)
{
    assert(chan->vt == &zombiechan_channelvt);
}

static int zombiechan_want_close(Channel *chan, int sent_eof, int rcvd_eof)
{
    return TRUE;
}

/* ----------------------------------------------------------------------
 * Centralised standard methods for other channel implementations to
 * borrow.
 */

void chan_remotely_opened_confirmation(Channel *chan)
{
    assert(0 && "this channel type should never receive OPEN_CONFIRMATION");
}

void chan_remotely_opened_failure(Channel *chan, const char *errtext)
{
    assert(0 && "this channel type should never receive OPEN_FAILURE");
}

int chan_no_eager_close(Channel *chan, int sent_local_eof, int rcvd_remote_eof)
{
    return FALSE;     /* default: never proactively ask for a close */
}

/* ----------------------------------------------------------------------
 * Common routine to marshal tty modes into an SSH packet.
 */

void write_ttymodes_to_packet_from_conf(
    BinarySink *bs, Frontend *frontend, Conf *conf,
    int ssh_version, int ospeed, int ispeed)
{
    int i;

    /*
     * Codes for terminal modes.
     * Most of these are the same in SSH-1 and SSH-2.
     * This list is derived from RFC 4254 and
     * SSH-1 RFC-1.2.31.
     */
    static const struct ssh_ttymode {
        const char *mode;
        int opcode;
        enum { TTY_OP_CHAR, TTY_OP_BOOL } type;
    } ssh_ttymodes[] = {
        /* "V" prefix discarded for special characters relative to SSH specs */
        { "INTR",      1, TTY_OP_CHAR },
        { "QUIT",      2, TTY_OP_CHAR },
        { "ERASE",     3, TTY_OP_CHAR },
        { "KILL",      4, TTY_OP_CHAR },
        { "EOF",       5, TTY_OP_CHAR },
        { "EOL",       6, TTY_OP_CHAR },
        { "EOL2",      7, TTY_OP_CHAR },
        { "START",     8, TTY_OP_CHAR },
        { "STOP",      9, TTY_OP_CHAR },
        { "SUSP",     10, TTY_OP_CHAR },
        { "DSUSP",    11, TTY_OP_CHAR },
        { "REPRINT",  12, TTY_OP_CHAR },
        { "WERASE",   13, TTY_OP_CHAR },
        { "LNEXT",    14, TTY_OP_CHAR },
        { "FLUSH",    15, TTY_OP_CHAR },
        { "SWTCH",    16, TTY_OP_CHAR },
        { "STATUS",   17, TTY_OP_CHAR },
        { "DISCARD",  18, TTY_OP_CHAR },
        { "IGNPAR",   30, TTY_OP_BOOL },
        { "PARMRK",   31, TTY_OP_BOOL },
        { "INPCK",    32, TTY_OP_BOOL },
        { "ISTRIP",   33, TTY_OP_BOOL },
        { "INLCR",    34, TTY_OP_BOOL },
        { "IGNCR",    35, TTY_OP_BOOL },
        { "ICRNL",    36, TTY_OP_BOOL },
        { "IUCLC",    37, TTY_OP_BOOL },
        { "IXON",     38, TTY_OP_BOOL },
        { "IXANY",    39, TTY_OP_BOOL },
        { "IXOFF",    40, TTY_OP_BOOL },
        { "IMAXBEL",  41, TTY_OP_BOOL },
        { "IUTF8",    42, TTY_OP_BOOL },
        { "ISIG",     50, TTY_OP_BOOL },
        { "ICANON",   51, TTY_OP_BOOL },
        { "XCASE",    52, TTY_OP_BOOL },
        { "ECHO",     53, TTY_OP_BOOL },
        { "ECHOE",    54, TTY_OP_BOOL },
        { "ECHOK",    55, TTY_OP_BOOL },
        { "ECHONL",   56, TTY_OP_BOOL },
        { "NOFLSH",   57, TTY_OP_BOOL },
        { "TOSTOP",   58, TTY_OP_BOOL },
        { "IEXTEN",   59, TTY_OP_BOOL },
        { "ECHOCTL",  60, TTY_OP_BOOL },
        { "ECHOKE",   61, TTY_OP_BOOL },
        { "PENDIN",   62, TTY_OP_BOOL }, /* XXX is this a real mode? */
        { "OPOST",    70, TTY_OP_BOOL },
        { "OLCUC",    71, TTY_OP_BOOL },
        { "ONLCR",    72, TTY_OP_BOOL },
        { "OCRNL",    73, TTY_OP_BOOL },
        { "ONOCR",    74, TTY_OP_BOOL },
        { "ONLRET",   75, TTY_OP_BOOL },
        { "CS7",      90, TTY_OP_BOOL },
        { "CS8",      91, TTY_OP_BOOL },
        { "PARENB",   92, TTY_OP_BOOL },
        { "PARODD",   93, TTY_OP_BOOL }
    };

    /* Miscellaneous other tty-related constants. */
    enum {
        /* The opcodes for ISPEED/OSPEED differ between SSH-1 and SSH-2. */
        SSH1_TTY_OP_ISPEED = 192,
        SSH1_TTY_OP_OSPEED = 193,
        SSH2_TTY_OP_ISPEED = 128,
        SSH2_TTY_OP_OSPEED = 129,

        SSH_TTY_OP_END = 0
    };

    for (i = 0; i < lenof(ssh_ttymodes); i++) {
        const struct ssh_ttymode *mode = ssh_ttymodes + i;
        const char *sval = conf_get_str_str(conf, CONF_ttymodes, mode->mode);
        char *to_free = NULL;

        /* Every mode known to the current version of the code should be
         * mentioned; this was ensured when settings were loaded. */

        /*
         * sval[0] can be
         *  - 'V', indicating that an explicit value follows it;
         *  - 'A', indicating that we should pass the value through from
         *    the local environment via get_ttymode; or
         *  - 'N', indicating that we should explicitly not send this
         *    mode.
         */
        if (sval[0] == 'A') {
            sval = to_free = get_ttymode(frontend, mode->mode);
        } else if (sval[0] == 'V') {
            sval++;                    /* skip the 'V' */
        } else {
            /* else 'N', or something from the future we don't understand */
            continue;
        }

        if (sval) {
            /*
             * Parse the string representation of the tty mode
             * into the integer value it will take on the wire.
             */
            unsigned ival = 0;

            switch (mode->type) {
              case TTY_OP_CHAR:
                if (*sval) {
                    char *next = NULL;
                    /* We know ctrlparse won't write to the string, so
                     * casting away const is ugly but allowable. */
                    ival = ctrlparse((char *)sval, &next);
                    if (!next)
                        ival = sval[0];
                } else {
                    ival = 255; /* special value meaning "don't set" */
                }
                break;
              case TTY_OP_BOOL:
                if (stricmp(sval, "yes") == 0 ||
                    stricmp(sval, "on") == 0 ||
                    stricmp(sval, "true") == 0 ||
                    stricmp(sval, "+") == 0)
                    ival = 1;      /* true */
                else if (stricmp(sval, "no") == 0 ||
                         stricmp(sval, "off") == 0 ||
                         stricmp(sval, "false") == 0 ||
                         stricmp(sval, "-") == 0)
                    ival = 0;      /* false */
                else
                    ival = (atoi(sval) != 0);
                break;
              default:
                assert(0 && "Bad mode->type");
            }

            /*
             * And write it into the output packet. The parameter
             * value is formatted as a byte in SSH-1, but a uint32
             * in SSH-2.
             */
            put_byte(bs, mode->opcode);
            if (ssh_version == 1)
                put_byte(bs, ival);
            else
                put_uint32(bs, ival);
        }

        sfree(to_free);
    }

    /*
     * Finish off with the terminal speeds (which are formatted as
     * uint32 in both protocol versions) and the end marker.
     */
    put_byte(bs, ssh_version == 1 ? SSH1_TTY_OP_ISPEED : SSH2_TTY_OP_ISPEED);
    put_uint32(bs, ispeed);
    put_byte(bs, ssh_version == 1 ? SSH1_TTY_OP_OSPEED : SSH2_TTY_OP_OSPEED);
    put_uint32(bs, ospeed);
    put_byte(bs, SSH_TTY_OP_END);
}

/* ----------------------------------------------------------------------
 * Routine for allocating a new channel ID, given a means of finding
 * the index field in a given channel structure.
 */

unsigned alloc_channel_id_general(tree234 *channels, size_t localid_offset)
{
    const unsigned CHANNEL_NUMBER_OFFSET = 256;
    search234_state ss;

    /*
     * First-fit allocation of channel numbers: we always pick the
     * lowest unused one.
     *
     * Every channel before that, and no channel after it, has an ID
     * exactly equal to its tree index plus CHANNEL_NUMBER_OFFSET. So
     * we can use the search234 system to identify the length of that
     * initial sequence, in a single log-time pass down the channels
     * tree.
     */
    search234_start(&ss, channels);
    while (ss.element) {
        unsigned localid = *(unsigned *)((char *)ss.element + localid_offset);
        if (localid == ss.index + CHANNEL_NUMBER_OFFSET)
            search234_step(&ss, +1);
        else
            search234_step(&ss, -1);
    }

    /*
     * Now ss.index gives exactly the number of channels in that
     * initial sequence. So adding CHANNEL_NUMBER_OFFSET to it must
     * give precisely the lowest unused channel number.
     */
    return ss.index + CHANNEL_NUMBER_OFFSET;
}

/* ----------------------------------------------------------------------
 * Functions for handling the comma-separated strings used to store
 * lists of protocol identifiers in SSH-2.
 */

int first_in_commasep_string(char const *needle, char const *haystack,
                             int haylen)
{
    int needlen;
    if (!needle || !haystack)          /* protect against null pointers */
        return 0;
    needlen = strlen(needle);

    if (haylen >= needlen &&       /* haystack is long enough */
        !memcmp(needle, haystack, needlen) &&   /* initial match */
        (haylen == needlen || haystack[needlen] == ',')
        /* either , or EOS follows */
        )
        return 1;
    return 0;
}

int in_commasep_string(char const *needle, char const *haystack, int haylen)
{
    char *p;

    if (!needle || !haystack)          /* protect against null pointers */
        return FALSE;
    /*
     * Is it at the start of the string?
     */
    if (first_in_commasep_string(needle, haystack, haylen))
        return TRUE;
    /*
     * If not, search for the next comma and resume after that.
     * If no comma found, terminate.
     */
    p = memchr(haystack, ',', haylen);
    if (!p)
        return FALSE;
    /* + 1 to skip over comma */
    return in_commasep_string(needle, p + 1, haylen - (p + 1 - haystack));
}

void add_to_commasep(strbuf *buf, const char *data)
{
    if (buf->len > 0)
        put_byte(buf, ',');
    put_data(buf, data, strlen(data));
}

/* ----------------------------------------------------------------------
 * Functions for translating SSH packet type codes into their symbolic
 * string names.
 */

#define translate(x) if (type == x) return #x
#define translatek(x,ctx) if (type == x && (pkt_kctx == ctx)) return #x
#define translatea(x,ctx) if (type == x && (pkt_actx == ctx)) return #x
const char *ssh1_pkt_type(int type)
{
    translate(SSH1_MSG_DISCONNECT);
    translate(SSH1_SMSG_PUBLIC_KEY);
    translate(SSH1_CMSG_SESSION_KEY);
    translate(SSH1_CMSG_USER);
    translate(SSH1_CMSG_AUTH_RSA);
    translate(SSH1_SMSG_AUTH_RSA_CHALLENGE);
    translate(SSH1_CMSG_AUTH_RSA_RESPONSE);
    translate(SSH1_CMSG_AUTH_PASSWORD);
    translate(SSH1_CMSG_REQUEST_PTY);
    translate(SSH1_CMSG_WINDOW_SIZE);
    translate(SSH1_CMSG_EXEC_SHELL);
    translate(SSH1_CMSG_EXEC_CMD);
    translate(SSH1_SMSG_SUCCESS);
    translate(SSH1_SMSG_FAILURE);
    translate(SSH1_CMSG_STDIN_DATA);
    translate(SSH1_SMSG_STDOUT_DATA);
    translate(SSH1_SMSG_STDERR_DATA);
    translate(SSH1_CMSG_EOF);
    translate(SSH1_SMSG_EXIT_STATUS);
    translate(SSH1_MSG_CHANNEL_OPEN_CONFIRMATION);
    translate(SSH1_MSG_CHANNEL_OPEN_FAILURE);
    translate(SSH1_MSG_CHANNEL_DATA);
    translate(SSH1_MSG_CHANNEL_CLOSE);
    translate(SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION);
    translate(SSH1_SMSG_X11_OPEN);
    translate(SSH1_CMSG_PORT_FORWARD_REQUEST);
    translate(SSH1_MSG_PORT_OPEN);
    translate(SSH1_CMSG_AGENT_REQUEST_FORWARDING);
    translate(SSH1_SMSG_AGENT_OPEN);
    translate(SSH1_MSG_IGNORE);
    translate(SSH1_CMSG_EXIT_CONFIRMATION);
    translate(SSH1_CMSG_X11_REQUEST_FORWARDING);
    translate(SSH1_CMSG_AUTH_RHOSTS_RSA);
    translate(SSH1_MSG_DEBUG);
    translate(SSH1_CMSG_REQUEST_COMPRESSION);
    translate(SSH1_CMSG_AUTH_TIS);
    translate(SSH1_SMSG_AUTH_TIS_CHALLENGE);
    translate(SSH1_CMSG_AUTH_TIS_RESPONSE);
    translate(SSH1_CMSG_AUTH_CCARD);
    translate(SSH1_SMSG_AUTH_CCARD_CHALLENGE);
    translate(SSH1_CMSG_AUTH_CCARD_RESPONSE);
    return "unknown";
}
const char *ssh2_pkt_type(Pkt_KCtx pkt_kctx, Pkt_ACtx pkt_actx, int type)
{
    translatea(SSH2_MSG_USERAUTH_GSSAPI_RESPONSE,SSH2_PKTCTX_GSSAPI);
    translatea(SSH2_MSG_USERAUTH_GSSAPI_TOKEN,SSH2_PKTCTX_GSSAPI);
    translatea(SSH2_MSG_USERAUTH_GSSAPI_EXCHANGE_COMPLETE,SSH2_PKTCTX_GSSAPI);
    translatea(SSH2_MSG_USERAUTH_GSSAPI_ERROR,SSH2_PKTCTX_GSSAPI);
    translatea(SSH2_MSG_USERAUTH_GSSAPI_ERRTOK,SSH2_PKTCTX_GSSAPI);
    translatea(SSH2_MSG_USERAUTH_GSSAPI_MIC, SSH2_PKTCTX_GSSAPI);
    translate(SSH2_MSG_DISCONNECT);
    translate(SSH2_MSG_IGNORE);
    translate(SSH2_MSG_UNIMPLEMENTED);
    translate(SSH2_MSG_DEBUG);
    translate(SSH2_MSG_SERVICE_REQUEST);
    translate(SSH2_MSG_SERVICE_ACCEPT);
    translate(SSH2_MSG_KEXINIT);
    translate(SSH2_MSG_NEWKEYS);
    translatek(SSH2_MSG_KEXDH_INIT, SSH2_PKTCTX_DHGROUP);
    translatek(SSH2_MSG_KEXDH_REPLY, SSH2_PKTCTX_DHGROUP);
    translatek(SSH2_MSG_KEX_DH_GEX_REQUEST_OLD, SSH2_PKTCTX_DHGEX);
    translatek(SSH2_MSG_KEX_DH_GEX_REQUEST, SSH2_PKTCTX_DHGEX);
    translatek(SSH2_MSG_KEX_DH_GEX_GROUP, SSH2_PKTCTX_DHGEX);
    translatek(SSH2_MSG_KEX_DH_GEX_INIT, SSH2_PKTCTX_DHGEX);
    translatek(SSH2_MSG_KEX_DH_GEX_REPLY, SSH2_PKTCTX_DHGEX);
    translatek(SSH2_MSG_KEXRSA_PUBKEY, SSH2_PKTCTX_RSAKEX);
    translatek(SSH2_MSG_KEXRSA_SECRET, SSH2_PKTCTX_RSAKEX);
    translatek(SSH2_MSG_KEXRSA_DONE, SSH2_PKTCTX_RSAKEX);
    translatek(SSH2_MSG_KEX_ECDH_INIT, SSH2_PKTCTX_ECDHKEX);
    translatek(SSH2_MSG_KEX_ECDH_REPLY, SSH2_PKTCTX_ECDHKEX);
    translatek(SSH2_MSG_KEXGSS_INIT, SSH2_PKTCTX_GSSKEX);
    translatek(SSH2_MSG_KEXGSS_CONTINUE, SSH2_PKTCTX_GSSKEX);
    translatek(SSH2_MSG_KEXGSS_COMPLETE, SSH2_PKTCTX_GSSKEX);
    translatek(SSH2_MSG_KEXGSS_HOSTKEY, SSH2_PKTCTX_GSSKEX);
    translatek(SSH2_MSG_KEXGSS_ERROR, SSH2_PKTCTX_GSSKEX);
    translatek(SSH2_MSG_KEXGSS_GROUPREQ, SSH2_PKTCTX_GSSKEX);
    translatek(SSH2_MSG_KEXGSS_GROUP, SSH2_PKTCTX_GSSKEX);
    translate(SSH2_MSG_USERAUTH_REQUEST);
    translate(SSH2_MSG_USERAUTH_FAILURE);
    translate(SSH2_MSG_USERAUTH_SUCCESS);
    translate(SSH2_MSG_USERAUTH_BANNER);
    translatea(SSH2_MSG_USERAUTH_PK_OK, SSH2_PKTCTX_PUBLICKEY);
    translatea(SSH2_MSG_USERAUTH_PASSWD_CHANGEREQ, SSH2_PKTCTX_PASSWORD);
    translatea(SSH2_MSG_USERAUTH_INFO_REQUEST, SSH2_PKTCTX_KBDINTER);
    translatea(SSH2_MSG_USERAUTH_INFO_RESPONSE, SSH2_PKTCTX_KBDINTER);
    translate(SSH2_MSG_GLOBAL_REQUEST);
    translate(SSH2_MSG_REQUEST_SUCCESS);
    translate(SSH2_MSG_REQUEST_FAILURE);
    translate(SSH2_MSG_CHANNEL_OPEN);
    translate(SSH2_MSG_CHANNEL_OPEN_CONFIRMATION);
    translate(SSH2_MSG_CHANNEL_OPEN_FAILURE);
    translate(SSH2_MSG_CHANNEL_WINDOW_ADJUST);
    translate(SSH2_MSG_CHANNEL_DATA);
    translate(SSH2_MSG_CHANNEL_EXTENDED_DATA);
    translate(SSH2_MSG_CHANNEL_EOF);
    translate(SSH2_MSG_CHANNEL_CLOSE);
    translate(SSH2_MSG_CHANNEL_REQUEST);
    translate(SSH2_MSG_CHANNEL_SUCCESS);
    translate(SSH2_MSG_CHANNEL_FAILURE);
    return "unknown";
}
#undef translate
#undef translatec
