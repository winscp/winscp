/*
 * Support for SSH connection sharing, i.e. permitting one PuTTY to
 * open its own channels over the SSH session being run by another.
 */

/*
 * Discussion and technical documentation
 * ======================================
 *
 * The basic strategy for PuTTY's implementation of SSH connection
 * sharing is to have a single 'upstream' PuTTY process, which manages
 * the real SSH connection and all the cryptography, and then zero or
 * more 'downstream' PuTTYs, which never talk to the real host but
 * only talk to the upstream through local IPC (Unix-domain sockets or
 * Windows named pipes).
 *
 * The downstreams communicate with the upstream using a protocol
 * derived from SSH itself, which I'll document in detail below. In
 * brief, though: the downstream->upstream protocol uses a trivial
 * binary packet protocol (just length/type/data) to encapsulate
 * unencrypted SSH messages, and downstreams talk to the upstream more
 * or less as if it was an SSH server itself. (So downstreams can
 * themselves open multiple SSH channels, for example, by sending
 * multiple SSH2_MSG_CHANNEL_OPENs; they can send CHANNEL_REQUESTs of
 * their choice within each channel, and they handle their own
 * WINDOW_ADJUST messages.)
 *
 * The upstream would ideally handle these downstreams by just putting
 * their messages into the queue for proper SSH-2 encapsulation and
 * encryption and sending them straight on to the server. However,
 * that's not quite feasible as written, because client-side channel
 * IDs could easily conflict (between multiple downstreams, or between
 * a downstream and the upstream). To protect against that, the
 * upstream rewrites the client-side channel IDs in messages it passes
 * on to the server, so that it's performing what you might describe
 * as 'channel-number NAT'. Then the upstream remembers which of its
 * own channel IDs are channels it's managing itself, and which are
 * placeholders associated with a particular downstream, so that when
 * replies come in from the server they can be sent on to the relevant
 * downstream (after un-NATting the channel number, of course).
 *
 * Global requests from downstreams are only accepted if the upstream
 * knows what to do about them; currently the only such requests are
 * the ones having to do with remote-to-local port forwarding (in
 * which, again, the upstream remembers that some of the forwardings
 * it's asked the server to set up were on behalf of particular
 * downstreams, and sends the incoming CHANNEL_OPENs to those
 * downstreams when connections come in).
 *
 * Other fiddly pieces of this mechanism are X forwarding and
 * (OpenSSH-style) agent forwarding. Both of these have a fundamental
 * problem arising from the protocol design: that the CHANNEL_OPEN
 * from the server introducing a forwarded connection does not carry
 * any indication of which session channel gave rise to it; so if
 * session channels from multiple downstreams enable those forwarding
 * methods, it's hard for the upstream to know which downstream to
 * send the resulting connections back to.
 *
 * For X forwarding, we can work around this in a really painful way
 * by using the fake X11 authorisation data sent to the server as part
 * of the forwarding setup: upstream ensures that every X forwarding
 * request carries distinguishable fake auth data, and then when X
 * connections come in it waits to see the auth data in the X11 setup
 * message before it decides which downstream to pass the connection
 * on to.
 *
 * For agent forwarding, that workaround is unavailable. As a result,
 * this system (and, as far as I can think of, any other system too)
 * has the fundamental constraint that it can only forward one SSH
 * agent - it can't forward two agents to different session channels.
 * So downstreams can request agent forwarding if they like, but if
 * they do, they'll get whatever SSH agent is known to the upstream
 * (if any) forwarded to their sessions.
 *
 * Downstream-to-upstream protocol
 * -------------------------------
 *
 * Here I document in detail the protocol spoken between PuTTY
 * downstreams and upstreams over local IPC. The IPC mechanism can
 * vary between host platforms, but the protocol is the same.
 *
 * The protocol commences with a version exchange which is exactly
 * like the SSH-2 one, in that each side sends a single line of text
 * of the form
 *
 *   <protocol>-<version>-<softwareversion> [comments] \r\n
 *
 * The only difference is that in real SSH-2, <protocol> is the string
 * "SSH", whereas in this protocol the string is
 * "SSHCONNECTION@putty.projects.tartarus.org".
 *
 * (The SSH RFCs allow many protocol-level identifier namespaces to be
 * extended by implementors without central standardisation as long as
 * they suffix "@" and a domain name they control to their new ids.
 * RFC 4253 does not define this particular name to be changeable at
 * all, but I like to think this is obviously how it would have done
 * so if the working group had foreseen the need :-)
 *
 * Thereafter, all data exchanged consists of a sequence of binary
 * packets concatenated end-to-end, each of which is of the form
 *
 *     uint32     length of packet, N
 *     byte[N]    N bytes of packet data
 *
 * and, since these are SSH-2 messages, the first data byte is taken
 * to be the packet type code.
 *
 * These messages are interpreted as those of an SSH connection, after
 * userauth completes, and without any repeat key exchange.
 * Specifically, any message from the SSH Connection Protocol is
 * permitted, and also SSH_MSG_IGNORE, SSH_MSG_DEBUG,
 * SSH_MSG_DISCONNECT and SSH_MSG_UNIMPLEMENTED from the SSH Transport
 * Protocol.
 *
 * This protocol imposes a few additional requirements, over and above
 * those of the standard SSH Connection Protocol:
 *
 * Message sizes are not permitted to exceed 0x4010 (16400) bytes,
 * including their length header.
 *
 * When the server (i.e. really the PuTTY upstream) sends
 * SSH_MSG_CHANNEL_OPEN with channel type "x11", and the client
 * (downstream) responds with SSH_MSG_CHANNEL_OPEN_CONFIRMATION, that
 * confirmation message MUST include an initial window size of at
 * least 256. (Rationale: this is a bit of a fudge which makes it
 * easier, by eliminating the possibility of nasty edge cases, for an
 * upstream to arrange not to pass the CHANNEL_OPEN on to downstream
 * until after it's seen the X11 auth data to decide which downstream
 * it needs to go to.)
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>

#include "putty.h"
#include "tree234.h"
#include "ssh.h"

struct ssh_sharing_state {
    const struct plug_function_table *fn;
    /* the above variable absolutely *must* be the first in this structure */

    char *sockname;                  /* the socket name, kept for cleanup */
    Socket listensock;               /* the master listening Socket */
    tree234 *connections;            /* holds ssh_sharing_connstates */
    unsigned nextid;                 /* preferred id for next connstate */
    Ssh ssh;                         /* instance of the ssh backend */
    char *server_verstring;          /* server version string after "SSH-" */
};

struct share_globreq;

struct ssh_sharing_connstate {
    const struct plug_function_table *fn;
    /* the above variable absolutely *must* be the first in this structure */

    unsigned id;    /* used to identify this downstream in log messages */

    Socket sock;                     /* the Socket for this connection */
    struct ssh_sharing_state *parent;

    int crLine;                        /* coroutine state for share_receive */

    int sent_verstring, got_verstring, curr_packetlen;

    unsigned char recvbuf[0x4010];
    int recvlen;

    /*
     * Assorted state we have to remember about this downstream, so
     * that we can clean it up appropriately when the downstream goes
     * away.
     */

    /* Channels which don't have a downstream id, i.e. we've passed a
     * CHANNEL_OPEN down from the server but not had an
     * OPEN_CONFIRMATION or OPEN_FAILURE back. If downstream goes
     * away, we respond to all of these with OPEN_FAILURE. */
    tree234 *halfchannels;         /* stores 'struct share_halfchannel' */

    /* Channels which do have a downstream id. We need to index these
     * by both server id and upstream id, so we can find a channel
     * when handling either an upward or a downward message referring
     * to it. */
    tree234 *channels_by_us;       /* stores 'struct share_channel' */
    tree234 *channels_by_server;   /* stores 'struct share_channel' */

    /* Another class of channel which doesn't have a downstream id.
     * The difference between these and halfchannels is that xchannels
     * do have an *upstream* id, because upstream has already accepted
     * the channel request from the server. This arises in the case of
     * X forwarding, where we have to accept the request and read the
     * X authorisation data before we know whether the channel needs
     * to be forwarded to a downstream. */
    tree234 *xchannels_by_us;     /* stores 'struct share_xchannel' */
    tree234 *xchannels_by_server; /* stores 'struct share_xchannel' */

    /* Remote port forwarding requests in force. */
    tree234 *forwardings;          /* stores 'struct share_forwarding' */

    /* Global requests we've sent on to the server, pending replies. */
    struct share_globreq *globreq_head, *globreq_tail;
};

struct share_halfchannel {
    unsigned server_id;
};

/* States of a share_channel. */
enum {
    OPEN,
    SENT_CLOSE,
    RCVD_CLOSE,
    /* Downstream has sent CHANNEL_OPEN but server hasn't replied yet.
     * If downstream goes away when a channel is in this state, we
     * must wait for the server's response before starting to send
     * CLOSE. Channels in this state are also not held in
     * channels_by_server, because their server_id field is
     * meaningless. */
    UNACKNOWLEDGED
};

struct share_channel {
    unsigned downstream_id, upstream_id, server_id;
    int downstream_maxpkt;
    int state;
    /*
     * Some channels (specifically, channels on which downstream has
     * sent "x11-req") have the additional function of storing a set
     * of downstream X authorisation data and a handle to an upstream
     * fake set.
     */
    struct X11FakeAuth *x11_auth_upstream;
    int x11_auth_proto;
    char *x11_auth_data;
    int x11_auth_datalen;
    int x11_one_shot;
};

struct share_forwarding {
    char *host;
    int port;
    int active;             /* has the server sent REQUEST_SUCCESS? */
};

struct share_xchannel_message {
    struct share_xchannel_message *next;
    int type;
    unsigned char *data;
    int datalen;
};

struct share_xchannel {
    unsigned upstream_id, server_id;

    /*
     * xchannels come in two flavours: live and dead. Live ones are
     * waiting for an OPEN_CONFIRMATION or OPEN_FAILURE from
     * downstream; dead ones have had an OPEN_FAILURE, so they only
     * exist as a means of letting us conveniently respond to further
     * channel messages from the server until such time as the server
     * sends us CHANNEL_CLOSE.
     */
    int live;

    /*
     * When we receive OPEN_CONFIRMATION, we will need to send a
     * WINDOW_ADJUST to the server to synchronise the windows. For
     * this purpose we need to know what window we have so far offered
     * the server. We record this as exactly the value in the
     * OPEN_CONFIRMATION that upstream sent us, adjusted by the amount
     * by which the two X greetings differed in length.
     */
    int window;

    /*
     * Linked list of SSH messages from the server relating to this
     * channel, which we queue up until downstream sends us an
     * OPEN_CONFIRMATION and we can belatedly send them all on.
     */
    struct share_xchannel_message *msghead, *msgtail;
};

enum {
    GLOBREQ_TCPIP_FORWARD,
    GLOBREQ_CANCEL_TCPIP_FORWARD
};

struct share_globreq {
    struct share_globreq *next;
    int type;
    int want_reply;
    struct share_forwarding *fwd;
};

static int share_connstate_cmp(void *av, void *bv)
{
    const struct ssh_sharing_connstate *a =
        (const struct ssh_sharing_connstate *)av;
    const struct ssh_sharing_connstate *b =
        (const struct ssh_sharing_connstate *)bv;

    if (a->id < b->id)
        return -1;
    else if (a->id > b->id)
        return +1;
    else
        return 0;
}

static unsigned share_find_unused_id
(struct ssh_sharing_state *sharestate, unsigned first)
{
    int low_orig, low, mid, high, high_orig;
    struct ssh_sharing_connstate *cs;
    unsigned ret;

    /*
     * Find the lowest unused downstream ID greater or equal to
     * 'first'.
     *
     * Begin by seeing if 'first' itself is available. If it is, we'll
     * just return it; if it's already in the tree, we'll find the
     * tree index where it appears and use that for the next stage.
     */
    {
        struct ssh_sharing_connstate dummy;
        dummy.id = first;
        cs = findrelpos234(sharestate->connections, &dummy, NULL,
                           REL234_GE, &low_orig);
        if (!cs)
            return first;
    }

    /*
     * Now binary-search using the counted B-tree, to find the largest
     * ID which is in a contiguous sequence from the beginning of that
     * range.
     */
    low = low_orig;
    high = high_orig = count234(sharestate->connections);
    while (high - low > 1) {
	mid = (high + low) / 2;
	cs = index234(sharestate->connections, mid);
	if (cs->id == first + (mid - low_orig))
	    low = mid;		       /* this one is still in the sequence */
	else
	    high = mid;		       /* this one is past the end */
    }

    /*
     * Now low is the tree index of the largest ID in the initial
     * sequence. So the return value is one more than low's id, and we
     * know low's id is given by the formula in the binary search loop
     * above.
     *
     * (If an SSH connection went on for _enormously_ long, we might
     * reach a point where all ids from 'first' to UINT_MAX were in
     * use. In that situation the formula below would wrap round by
     * one and return zero, which is conveniently the right way to
     * signal 'no id available' from this function.)
     */
    ret = first + (low - low_orig) + 1;
    {
        struct ssh_sharing_connstate dummy;
        dummy.id = ret;
	assert(NULL == find234(sharestate->connections, &dummy, NULL));
    }
    return ret;
}

static int share_halfchannel_cmp(void *av, void *bv)
{
    const struct share_halfchannel *a = (const struct share_halfchannel *)av;
    const struct share_halfchannel *b = (const struct share_halfchannel *)bv;

    if (a->server_id < b->server_id)
        return -1;
    else if (a->server_id > b->server_id)
        return +1;
    else
        return 0;
}

static int share_channel_us_cmp(void *av, void *bv)
{
    const struct share_channel *a = (const struct share_channel *)av;
    const struct share_channel *b = (const struct share_channel *)bv;

    if (a->upstream_id < b->upstream_id)
        return -1;
    else if (a->upstream_id > b->upstream_id)
        return +1;
    else
        return 0;
}

static int share_channel_server_cmp(void *av, void *bv)
{
    const struct share_channel *a = (const struct share_channel *)av;
    const struct share_channel *b = (const struct share_channel *)bv;

    if (a->server_id < b->server_id)
        return -1;
    else if (a->server_id > b->server_id)
        return +1;
    else
        return 0;
}

static int share_xchannel_us_cmp(void *av, void *bv)
{
    const struct share_xchannel *a = (const struct share_xchannel *)av;
    const struct share_xchannel *b = (const struct share_xchannel *)bv;

    if (a->upstream_id < b->upstream_id)
        return -1;
    else if (a->upstream_id > b->upstream_id)
        return +1;
    else
        return 0;
}

static int share_xchannel_server_cmp(void *av, void *bv)
{
    const struct share_xchannel *a = (const struct share_xchannel *)av;
    const struct share_xchannel *b = (const struct share_xchannel *)bv;

    if (a->server_id < b->server_id)
        return -1;
    else if (a->server_id > b->server_id)
        return +1;
    else
        return 0;
}

static int share_forwarding_cmp(void *av, void *bv)
{
    const struct share_forwarding *a = (const struct share_forwarding *)av;
    const struct share_forwarding *b = (const struct share_forwarding *)bv;
    int i;

    if ((i = strcmp(a->host, b->host)) != 0)
        return i;
    else if (a->port < b->port)
        return -1;
    else if (a->port > b->port)
        return +1;
    else
        return 0;
}

static void share_xchannel_free(struct share_xchannel *xc)
{
    while (xc->msghead) {
        struct share_xchannel_message *tmp = xc->msghead;
        xc->msghead = tmp->next;
        sfree(tmp);
    }
    sfree(xc);
}

static void share_connstate_free(struct ssh_sharing_connstate *cs)
{
    struct share_halfchannel *hc;
    struct share_xchannel *xc;
    struct share_channel *chan;
    struct share_forwarding *fwd;

    while ((hc = (struct share_halfchannel *)
            delpos234(cs->halfchannels, 0)) != NULL)
        sfree(hc);
    freetree234(cs->halfchannels);

    /* All channels live in 'channels_by_us' but only some in
     * 'channels_by_server', so we use the former to find the list of
     * ones to free */
    freetree234(cs->channels_by_server);
    while ((chan = (struct share_channel *)
            delpos234(cs->channels_by_us, 0)) != NULL)
        sfree(chan);
    freetree234(cs->channels_by_us);

    /* But every xchannel is in both trees, so it doesn't matter which
     * we use to free them. */
    while ((xc = (struct share_xchannel *)
            delpos234(cs->xchannels_by_us, 0)) != NULL)
        share_xchannel_free(xc);
    freetree234(cs->xchannels_by_us);
    freetree234(cs->xchannels_by_server);

    while ((fwd = (struct share_forwarding *)
            delpos234(cs->forwardings, 0)) != NULL)
        sfree(fwd);
    freetree234(cs->forwardings);

    while (cs->globreq_head) {
        struct share_globreq *globreq = cs->globreq_head;
        cs->globreq_head = cs->globreq_head->next;
        sfree(globreq);
    }

    if (cs->sock)
        sk_close(cs->sock);

    sfree(cs);
}

void sharestate_free(void *v)
{
    struct ssh_sharing_state *sharestate = (struct ssh_sharing_state *)v;
    struct ssh_sharing_connstate *cs;

    platform_ssh_share_cleanup(sharestate->sockname);

    while ((cs = (struct ssh_sharing_connstate *)
            delpos234(sharestate->connections, 0)) != NULL) {
        share_connstate_free(cs);
    }
    freetree234(sharestate->connections);
    if (sharestate->listensock) {
        sk_close(sharestate->listensock);
        sharestate->listensock = NULL;
    }
    sfree(sharestate->server_verstring);
    sfree(sharestate->sockname);
    sfree(sharestate);
}

static struct share_halfchannel *share_add_halfchannel
    (struct ssh_sharing_connstate *cs, unsigned server_id)
{
    struct share_halfchannel *hc = snew(struct share_halfchannel);
    hc->server_id = server_id;
    if (add234(cs->halfchannels, hc) != hc) {
        /* Duplicate?! */
        sfree(hc);
        return NULL;
    } else {
        return hc;
    }
}

static struct share_halfchannel *share_find_halfchannel
    (struct ssh_sharing_connstate *cs, unsigned server_id)
{
    struct share_halfchannel dummyhc;
    dummyhc.server_id = server_id;
    return find234(cs->halfchannels, &dummyhc, NULL);
}

static void share_remove_halfchannel(struct ssh_sharing_connstate *cs,
                                     struct share_halfchannel *hc)
{
    del234(cs->halfchannels, hc);
    sfree(hc);
}

static struct share_channel *share_add_channel
    (struct ssh_sharing_connstate *cs, unsigned downstream_id,
     unsigned upstream_id, unsigned server_id, int state, int maxpkt)
{
    struct share_channel *chan = snew(struct share_channel);
    chan->downstream_id = downstream_id;
    chan->upstream_id = upstream_id;
    chan->server_id = server_id;
    chan->state = state;
    chan->downstream_maxpkt = maxpkt;
    chan->x11_auth_upstream = NULL;
    chan->x11_auth_data = NULL;
    chan->x11_auth_proto = -1;
    chan->x11_auth_datalen = 0;
    chan->x11_one_shot = 0;
    if (add234(cs->channels_by_us, chan) != chan) {
        sfree(chan);
        return NULL;
    }
    if (chan->state != UNACKNOWLEDGED) {
        if (add234(cs->channels_by_server, chan) != chan) {
            del234(cs->channels_by_us, chan);
            sfree(chan);            
            return NULL;
        }
    }
    return chan;
}

static void share_channel_set_server_id(struct ssh_sharing_connstate *cs,
                                        struct share_channel *chan,
                                        unsigned server_id, int newstate)
{
    chan->server_id = server_id;
    chan->state = newstate;
    assert(newstate != UNACKNOWLEDGED);
    add234(cs->channels_by_server, chan);
}

static struct share_channel *share_find_channel_by_upstream
    (struct ssh_sharing_connstate *cs, unsigned upstream_id)
{
    struct share_channel dummychan;
    dummychan.upstream_id = upstream_id;
    return find234(cs->channels_by_us, &dummychan, NULL);
}

static struct share_channel *share_find_channel_by_server
    (struct ssh_sharing_connstate *cs, unsigned server_id)
{
    struct share_channel dummychan;
    dummychan.server_id = server_id;
    return find234(cs->channels_by_server, &dummychan, NULL);
}

static void share_remove_channel(struct ssh_sharing_connstate *cs,
                                 struct share_channel *chan)
{
    del234(cs->channels_by_us, chan);
    del234(cs->channels_by_server, chan);
    if (chan->x11_auth_upstream)
        ssh_sharing_remove_x11_display(cs->parent->ssh,
                                       chan->x11_auth_upstream);
    sfree(chan->x11_auth_data);
    sfree(chan);
}

static struct share_xchannel *share_add_xchannel
    (struct ssh_sharing_connstate *cs,
     unsigned upstream_id, unsigned server_id)
{
    struct share_xchannel *xc = snew(struct share_xchannel);
    xc->upstream_id = upstream_id;
    xc->server_id = server_id;
    xc->live = TRUE;
    xc->msghead = xc->msgtail = NULL;
    if (add234(cs->xchannels_by_us, xc) != xc) {
        sfree(xc);
        return NULL;
    }
    if (add234(cs->xchannels_by_server, xc) != xc) {
        del234(cs->xchannels_by_us, xc);
        sfree(xc);
        return NULL;
    }
    return xc;
}

static struct share_xchannel *share_find_xchannel_by_upstream
    (struct ssh_sharing_connstate *cs, unsigned upstream_id)
{
    struct share_xchannel dummyxc;
    dummyxc.upstream_id = upstream_id;
    return find234(cs->xchannels_by_us, &dummyxc, NULL);
}

static struct share_xchannel *share_find_xchannel_by_server
    (struct ssh_sharing_connstate *cs, unsigned server_id)
{
    struct share_xchannel dummyxc;
    dummyxc.server_id = server_id;
    return find234(cs->xchannels_by_server, &dummyxc, NULL);
}

static void share_remove_xchannel(struct ssh_sharing_connstate *cs,
                                 struct share_xchannel *xc)
{
    del234(cs->xchannels_by_us, xc);
    del234(cs->xchannels_by_server, xc);
    share_xchannel_free(xc);
}

static struct share_forwarding *share_add_forwarding
    (struct ssh_sharing_connstate *cs,
     const char *host, int port)
{
    struct share_forwarding *fwd = snew(struct share_forwarding);
    fwd->host = dupstr(host);
    fwd->port = port;
    fwd->active = FALSE;
    if (add234(cs->forwardings, fwd) != fwd) {
        /* Duplicate?! */
        sfree(fwd);
        return NULL;
    }
    return fwd;
}

static struct share_forwarding *share_find_forwarding
    (struct ssh_sharing_connstate *cs, const char *host, int port)
{
    struct share_forwarding dummyfwd, *ret;
    dummyfwd.host = dupstr(host);
    dummyfwd.port = port;
    ret = find234(cs->forwardings, &dummyfwd, NULL);
    sfree(dummyfwd.host);
    return ret;
}

static void share_remove_forwarding(struct ssh_sharing_connstate *cs,
                                    struct share_forwarding *fwd)
{
    del234(cs->forwardings, fwd);
    sfree(fwd);
}

static void send_packet_to_downstream(struct ssh_sharing_connstate *cs,
                                      int type, const void *pkt, int pktlen,
                                      struct share_channel *chan)
{
    if (!cs->sock) /* throw away all packets destined for a dead downstream */
        return;

    if (type == SSH2_MSG_CHANNEL_DATA) {
        /*
         * Special case which we take care of at a low level, so as to
         * be sure to apply it in all cases. On rare occasions we
         * might find that we have a channel for which the
         * downstream's maximum packet size exceeds the max packet
         * size we presented to the server on its behalf. (This can
         * occur in X11 forwarding, where we have to send _our_
         * CHANNEL_OPEN_CONFIRMATION before we discover which if any
         * downstream the channel is destined for, so if that
         * downstream turns out to present a smaller max packet size
         * then we're in this situation.)
         *
         * If that happens, we just chop up the packet into pieces and
         * send them as separate CHANNEL_DATA packets.
         */
        const char *upkt = (const char *)pkt;
        char header[13]; /* 4 length + 1 type + 4 channel id + 4 string len */

        int len = toint(GET_32BIT(upkt + 4));
        upkt += 8;                /* skip channel id + length field */

        if (len < 0 || len > pktlen - 8)
            len = pktlen - 8;

        do {
            int this_len = (len > chan->downstream_maxpkt ?
                            chan->downstream_maxpkt : len);
            PUT_32BIT(header, this_len + 9);
            header[4] = type;
            PUT_32BIT(header + 5, chan->downstream_id);
            PUT_32BIT(header + 9, this_len);
            sk_write(cs->sock, header, 13);
            sk_write(cs->sock, upkt, this_len);
            len -= this_len;
            upkt += this_len;
        } while (len > 0);
    } else {
        /*
         * Just do the obvious thing.
         */
        char header[9];

        PUT_32BIT(header, pktlen + 1);
        header[4] = type;
        sk_write(cs->sock, header, 5);
        sk_write(cs->sock, pkt, pktlen);
    }
}

static void share_try_cleanup(struct ssh_sharing_connstate *cs)
{
    int i;
    struct share_halfchannel *hc;
    struct share_channel *chan;
    struct share_forwarding *fwd;

    /*
     * Any half-open channels, i.e. those for which we'd received
     * CHANNEL_OPEN from the server but not passed back a response
     * from downstream, should be responded to with OPEN_FAILURE.
     */
    while ((hc = (struct share_halfchannel *)
            index234(cs->halfchannels, 0)) != NULL) {
        static const char reason[] = "PuTTY downstream no longer available";
        static const char lang[] = "en";
        unsigned char packet[256];
        int pos = 0;

        PUT_32BIT(packet + pos, hc->server_id); pos += 4;
        PUT_32BIT(packet + pos, SSH2_OPEN_CONNECT_FAILED); pos += 4;
        PUT_32BIT(packet + pos, strlen(reason)); pos += 4;
        memcpy(packet + pos, reason, strlen(reason)); pos += strlen(reason);
        PUT_32BIT(packet + pos, strlen(lang)); pos += 4;
        memcpy(packet + pos, lang, strlen(lang)); pos += strlen(lang);
        ssh_send_packet_from_downstream(cs->parent->ssh, cs->id,
                                        SSH2_MSG_CHANNEL_OPEN_FAILURE,
                                        packet, pos, "cleanup after"
                                        " downstream went away");

        share_remove_halfchannel(cs, hc);
    }

    /*
     * Any actually open channels should have a CHANNEL_CLOSE sent for
     * them, unless we've already done so. We won't be able to
     * actually clean them up until CHANNEL_CLOSE comes back from the
     * server, though (unless the server happens to have sent a CLOSE
     * already).
     *
     * Another annoying exception is UNACKNOWLEDGED channels, i.e.
     * we've _sent_ a CHANNEL_OPEN to the server but not received an
     * OPEN_CONFIRMATION or OPEN_FAILURE. We must wait for a reply
     * before closing the channel, because until we see that reply we
     * won't have the server's channel id to put in the close message.
     */
    for (i = 0; (chan = (struct share_channel *)
                 index234(cs->channels_by_us, i)) != NULL; i++) {
        unsigned char packet[256];
        int pos = 0;

        if (chan->state != SENT_CLOSE && chan->state != UNACKNOWLEDGED) {
            PUT_32BIT(packet + pos, chan->server_id); pos += 4;
            ssh_send_packet_from_downstream(cs->parent->ssh, cs->id,
                                            SSH2_MSG_CHANNEL_CLOSE,
                                            packet, pos, "cleanup after"
                                            " downstream went away");
            if (chan->state != RCVD_CLOSE) {
                chan->state = SENT_CLOSE;
            } else {
                /* In this case, we _can_ clear up the channel now. */
                ssh_delete_sharing_channel(cs->parent->ssh, chan->upstream_id);
                share_remove_channel(cs, chan);
                i--;    /* don't accidentally skip one as a result */
            }
        }
    }

    /*
     * Any remote port forwardings we're managing on behalf of this
     * downstream should be cancelled. Again, we must defer those for
     * which we haven't yet seen REQUEST_SUCCESS/FAILURE.
     *
     * We take a fire-and-forget approach during cleanup, not
     * bothering to set want_reply.
     */
    for (i = 0; (fwd = (struct share_forwarding *)
                 index234(cs->forwardings, i)) != NULL; i++) {
        if (fwd->active) {
            static const char request[] = "cancel-tcpip-forward";
            char *packet = snewn(256 + strlen(fwd->host), char);
            int pos = 0;

            PUT_32BIT(packet + pos, strlen(request)); pos += 4;
            memcpy(packet + pos, request, strlen(request));
            pos += strlen(request);

            packet[pos++] = 0;         /* !want_reply */

            PUT_32BIT(packet + pos, strlen(fwd->host)); pos += 4;
            memcpy(packet + pos, fwd->host, strlen(fwd->host));
            pos += strlen(fwd->host);

            PUT_32BIT(packet + pos, fwd->port); pos += 4;

            ssh_send_packet_from_downstream(cs->parent->ssh, cs->id,
                                            SSH2_MSG_GLOBAL_REQUEST,
                                            packet, pos, "cleanup after"
                                            " downstream went away");
            sfree(packet);

            share_remove_forwarding(cs, fwd);
            i--;    /* don't accidentally skip one as a result */
        }
    }

    if (count234(cs->halfchannels) == 0 &&
        count234(cs->channels_by_us) == 0 &&
        count234(cs->forwardings) == 0) {
        /*
         * Now we're _really_ done, so we can get rid of cs completely.
         */
        del234(cs->parent->connections, cs);
        ssh_sharing_downstream_disconnected(cs->parent->ssh, cs->id);
        share_connstate_free(cs);
    }
}

static void share_begin_cleanup(struct ssh_sharing_connstate *cs)
{

    sk_close(cs->sock);
    cs->sock = NULL;

    share_try_cleanup(cs);
}

static void share_disconnect(struct ssh_sharing_connstate *cs,
                             const char *message)
{
    static const char lang[] = "en";
    int msglen = strlen(message);
    char *packet = snewn(msglen + 256, char);
    int pos = 0;

    PUT_32BIT(packet + pos, SSH2_DISCONNECT_PROTOCOL_ERROR); pos += 4;

    PUT_32BIT(packet + pos, msglen); pos += 4;
    memcpy(packet + pos, message, msglen);
    pos += msglen;

    PUT_32BIT(packet + pos, strlen(lang)); pos += 4;
    memcpy(packet + pos, lang, strlen(lang)); pos += strlen(lang);

    send_packet_to_downstream(cs, SSH2_MSG_DISCONNECT, packet, pos, NULL);

    share_begin_cleanup(cs);
}

static int share_closing(Plug plug, const char *error_msg, int error_code,
                         int calling_back)
{
    struct ssh_sharing_connstate *cs = (struct ssh_sharing_connstate *)plug;
    if (error_msg)
        ssh_sharing_logf(cs->parent->ssh, cs->id, "%s", error_msg);
    share_begin_cleanup(cs);
    return 1;
}

static int getstring_inner(const void *vdata, int datalen,
                           char **out, int *outlen)
{
    const unsigned char *data = (const unsigned char *)vdata;
    int len;

    if (datalen < 4)
        return FALSE;

    len = toint(GET_32BIT(data));
    if (len < 0 || len > datalen - 4)
        return FALSE;

    if (outlen)
        *outlen = len + 4;         /* total size including length field */
    if (out)
        *out = dupprintf("%.*s", len, (char *)data + 4);
    return TRUE;
}

static char *getstring(const void *data, int datalen)
{
    char *ret;
    if (getstring_inner(data, datalen, &ret, NULL))
        return ret;
    else
        return NULL;
}

static int getstring_size(const void *data, int datalen)
{
    int ret;
    if (getstring_inner(data, datalen, NULL, &ret))
        return ret;
    else
        return -1;
}

/*
 * Append a message to the end of an xchannel's queue, with the length
 * and type code filled in and the data block allocated but
 * uninitialised.
 */
struct share_xchannel_message *share_xchannel_add_message
(struct share_xchannel *xc, int type, int len)
{
    unsigned char *block;
    struct share_xchannel_message *msg;

    /*
     * Be a little tricksy here by allocating a single memory block
     * containing both the 'struct share_xchannel_message' and the
     * actual data. Simplifies freeing it later.
     */
    block = smalloc(sizeof(struct share_xchannel_message) + len);
    msg = (struct share_xchannel_message *)block;
    msg->data = block + sizeof(struct share_xchannel_message);
    msg->datalen = len;
    msg->type = type;

    /*
     * Queue it in the xchannel.
     */
    if (xc->msgtail)
        xc->msgtail->next = msg;
    else
        xc->msghead = msg;
    msg->next = NULL;
    xc->msgtail = msg;

    return msg;
}

void share_dead_xchannel_respond(struct ssh_sharing_connstate *cs,
                                 struct share_xchannel *xc)
{
    /*
     * Handle queued incoming messages from the server destined for an
     * xchannel which is dead (i.e. downstream sent OPEN_FAILURE).
     */
    int delete = FALSE;
    while (xc->msghead) {
        struct share_xchannel_message *msg = xc->msghead;
        xc->msghead = msg->next;

        if (msg->type == SSH2_MSG_CHANNEL_REQUEST && msg->datalen > 4) {
            /*
             * A CHANNEL_REQUEST is responded to by sending
             * CHANNEL_FAILURE, if it has want_reply set.
             */
            int wantreplypos = getstring_size(msg->data, msg->datalen);
            if (wantreplypos > 0 && wantreplypos < msg->datalen &&
                msg->data[wantreplypos] != 0) {
                unsigned char id[4];
                PUT_32BIT(id, xc->server_id);
                ssh_send_packet_from_downstream
                    (cs->parent->ssh, cs->id, SSH2_MSG_CHANNEL_FAILURE, id, 4,
                     "downstream refused X channel open");
            }
        } else if (msg->type == SSH2_MSG_CHANNEL_CLOSE) {
            /*
             * On CHANNEL_CLOSE we can discard the channel completely.
             */
            delete = TRUE;
        }

        sfree(msg);
    }
    xc->msgtail = NULL;
    if (delete) {
        ssh_delete_sharing_channel(cs->parent->ssh, xc->upstream_id);
        share_remove_xchannel(cs, xc);
    }
}

void share_xchannel_confirmation(struct ssh_sharing_connstate *cs,
                                 struct share_xchannel *xc,
                                 struct share_channel *chan,
                                 unsigned downstream_window)
{
    unsigned char window_adjust[8];

    /*
     * Send all the queued messages downstream.
     */
    while (xc->msghead) {
        struct share_xchannel_message *msg = xc->msghead;
        xc->msghead = msg->next;

        if (msg->datalen >= 4)
            PUT_32BIT(msg->data, chan->downstream_id);
        send_packet_to_downstream(cs, msg->type,
                                  msg->data, msg->datalen, chan);

        sfree(msg);
    }

    /*
     * Send a WINDOW_ADJUST back upstream, to synchronise the window
     * size downstream thinks it's presented with the one we've
     * actually presented.
     */
    PUT_32BIT(window_adjust, xc->server_id);
    PUT_32BIT(window_adjust + 4, downstream_window - xc->window);
    ssh_send_packet_from_downstream(cs->parent->ssh, cs->id,
                                    SSH2_MSG_CHANNEL_WINDOW_ADJUST,
                                    window_adjust, 8, "window adjustment after"
                                    " downstream accepted X channel");
}

void share_xchannel_failure(struct ssh_sharing_connstate *cs,
                            struct share_xchannel *xc)
{
    /*
     * If downstream refuses to open our X channel at all for some
     * reason, we must respond by sending an emergency CLOSE upstream.
     */
    unsigned char id[4];
    PUT_32BIT(id, xc->server_id);
    ssh_send_packet_from_downstream
        (cs->parent->ssh, cs->id, SSH2_MSG_CHANNEL_CLOSE, id, 4,
         "downstream refused X channel open");

    /*
     * Now mark the xchannel as dead, and respond to anything sent on
     * it until we see CLOSE for it in turn.
     */
    xc->live = FALSE;
    share_dead_xchannel_respond(cs, xc);
}

void share_setup_x11_channel(void *csv, void *chanv,
                             unsigned upstream_id, unsigned server_id,
                             unsigned server_currwin, unsigned server_maxpkt,
                             unsigned client_adjusted_window,
                             const char *peer_addr, int peer_port, int endian,
                             int protomajor, int protominor,
                             const void *initial_data, int initial_len)
{
    struct ssh_sharing_connstate *cs = (struct ssh_sharing_connstate *)csv;
    struct share_channel *chan = (struct share_channel *)chanv;
    struct share_xchannel *xc;
    struct share_xchannel_message *msg;
    void *greeting;
    int greeting_len;
    unsigned char *pkt;
    int pktlen;

    /*
     * Create an xchannel containing data we've already received from
     * the X client, and preload it with a CHANNEL_DATA message
     * containing our own made-up authorisation greeting and any
     * additional data sent from the server so far.
     */
    xc = share_add_xchannel(cs, upstream_id, server_id);
    greeting = x11_make_greeting(endian, protomajor, protominor,
                                 chan->x11_auth_proto,
                                 chan->x11_auth_data, chan->x11_auth_datalen,
                                 peer_addr, peer_port, &greeting_len);
    msg = share_xchannel_add_message(xc, SSH2_MSG_CHANNEL_DATA,
                                     8 + greeting_len + initial_len);
    /* leave the channel id field unfilled - we don't know the
     * downstream id yet, of course */
    PUT_32BIT(msg->data + 4, greeting_len + initial_len);
    memcpy(msg->data + 8, greeting, greeting_len);
    memcpy(msg->data + 8 + greeting_len, initial_data, initial_len);
    sfree(greeting);

    xc->window = client_adjusted_window + greeting_len;

    /*
     * Send on a CHANNEL_OPEN to downstream.
     */
    pktlen = 27 + strlen(peer_addr);
    pkt = snewn(pktlen, unsigned char);
    PUT_32BIT(pkt, 3);                 /* strlen("x11") */
    memcpy(pkt+4, "x11", 3);
    PUT_32BIT(pkt+7, server_id);
    PUT_32BIT(pkt+11, server_currwin);
    PUT_32BIT(pkt+15, server_maxpkt);
    PUT_32BIT(pkt+19, strlen(peer_addr));
    memcpy(pkt+23, peer_addr, strlen(peer_addr));
    PUT_32BIT(pkt+23+strlen(peer_addr), peer_port);
    send_packet_to_downstream(cs, SSH2_MSG_CHANNEL_OPEN, pkt, pktlen, NULL);
    sfree(pkt);

    /*
     * If this was a once-only X forwarding, clean it up now.
     */
    if (chan->x11_one_shot) {
        ssh_sharing_remove_x11_display(cs->parent->ssh,
                                       chan->x11_auth_upstream);
        chan->x11_auth_upstream = NULL;
        sfree(chan->x11_auth_data);
        chan->x11_auth_proto = -1;
        chan->x11_auth_datalen = 0;
        chan->x11_one_shot = 0;
    }
}

void share_got_pkt_from_server(void *csv, int type,
                               unsigned char *pkt, int pktlen)
{
    struct ssh_sharing_connstate *cs = (struct ssh_sharing_connstate *)csv;
    struct share_globreq *globreq;
    int id_pos;
    unsigned upstream_id, server_id;
    struct share_channel *chan;
    struct share_xchannel *xc;

    switch (type) {
      case SSH2_MSG_REQUEST_SUCCESS:
      case SSH2_MSG_REQUEST_FAILURE:
        globreq = cs->globreq_head;
        if (globreq->type == GLOBREQ_TCPIP_FORWARD) {
            if (type == SSH2_MSG_REQUEST_FAILURE) {
                share_remove_forwarding(cs, globreq->fwd);
            } else {
                globreq->fwd->active = TRUE;
            }
        } else if (globreq->type == GLOBREQ_CANCEL_TCPIP_FORWARD) {
            if (type == SSH2_MSG_REQUEST_SUCCESS) {
                share_remove_forwarding(cs, globreq->fwd);
            }
        }
        if (globreq->want_reply) {
            send_packet_to_downstream(cs, type, pkt, pktlen, NULL);
        }
        cs->globreq_head = globreq->next;
        sfree(globreq);
        if (cs->globreq_head == NULL)
            cs->globreq_tail = NULL;

        if (!cs->sock) {
            /* Retry cleaning up this connection, in case that reply
             * was the last thing we were waiting for. */
            share_try_cleanup(cs);
        }

        break;

      case SSH2_MSG_CHANNEL_OPEN:
        id_pos = getstring_size(pkt, pktlen);
        assert(id_pos >= 0);
        server_id = GET_32BIT(pkt + id_pos);
        share_add_halfchannel(cs, server_id);

        send_packet_to_downstream(cs, type, pkt, pktlen, NULL);
        break;

      case SSH2_MSG_CHANNEL_OPEN_CONFIRMATION:
      case SSH2_MSG_CHANNEL_OPEN_FAILURE:
      case SSH2_MSG_CHANNEL_CLOSE:
      case SSH2_MSG_CHANNEL_WINDOW_ADJUST:
      case SSH2_MSG_CHANNEL_DATA:
      case SSH2_MSG_CHANNEL_EXTENDED_DATA:
      case SSH2_MSG_CHANNEL_EOF:
      case SSH2_MSG_CHANNEL_REQUEST:
      case SSH2_MSG_CHANNEL_SUCCESS:
      case SSH2_MSG_CHANNEL_FAILURE:
        /*
         * All these messages have the recipient channel id as the
         * first uint32 field in the packet. Substitute the downstream
         * channel id for our one and pass the packet downstream.
         */
        assert(pktlen >= 4);
        upstream_id = GET_32BIT(pkt);
        if ((chan = share_find_channel_by_upstream(cs, upstream_id)) != NULL) {
            /*
             * The normal case: this id refers to an open channel.
             */
            PUT_32BIT(pkt, chan->downstream_id);
            send_packet_to_downstream(cs, type, pkt, pktlen, chan);

            /*
             * Update the channel state, for messages that need it.
             */
            if (type == SSH2_MSG_CHANNEL_OPEN_CONFIRMATION) {
                if (chan->state == UNACKNOWLEDGED && pktlen >= 8) {
                    share_channel_set_server_id(cs, chan, GET_32BIT(pkt+4),
                                                OPEN);
                    if (!cs->sock) {
                        /* Retry cleaning up this connection, so that we
                         * can send an immediate CLOSE on this channel for
                         * which we now know the server id. */
                        share_try_cleanup(cs);
                    }
                }
            } else if (type == SSH2_MSG_CHANNEL_OPEN_FAILURE) {
                ssh_delete_sharing_channel(cs->parent->ssh, chan->upstream_id);
                share_remove_channel(cs, chan);
            } else if (type == SSH2_MSG_CHANNEL_CLOSE) {
                if (chan->state == SENT_CLOSE) {
                    ssh_delete_sharing_channel(cs->parent->ssh,
                                               chan->upstream_id);
                    share_remove_channel(cs, chan);
                    if (!cs->sock) {
                        /* Retry cleaning up this connection, in case this
                         * channel closure was the last thing we were
                         * waiting for. */
                        share_try_cleanup(cs);
                    }
                } else {
                    chan->state = RCVD_CLOSE;
                }
            }
        } else if ((xc = share_find_xchannel_by_upstream(cs, upstream_id))
                   != NULL) {
            /*
             * The unusual case: this id refers to an xchannel. Add it
             * to the xchannel's queue.
             */
            struct share_xchannel_message *msg;

            msg = share_xchannel_add_message(xc, type, pktlen);
            memcpy(msg->data, pkt, pktlen);

            /* If the xchannel is dead, then also respond to it (which
             * may involve deleting the channel). */
            if (!xc->live)
                share_dead_xchannel_respond(cs, xc);
        }
        break;

      default:
        assert(!"This packet type should never have come from ssh.c");
        break;
    }
}

static void share_got_pkt_from_downstream(struct ssh_sharing_connstate *cs,
                                          int type,
                                          unsigned char *pkt, int pktlen)
{
    char *request_name;
    struct share_forwarding *fwd;
    int id_pos;
    unsigned old_id, new_id, server_id;
    struct share_globreq *globreq;
    struct share_channel *chan;
    struct share_halfchannel *hc;
    struct share_xchannel *xc;
    char *err = NULL;

    switch (type) {
      case SSH2_MSG_DISCONNECT:
        /*
         * This message stops here: if downstream is disconnecting
         * from us, that doesn't mean we want to disconnect from the
         * SSH server. Close the downstream connection and start
         * cleanup.
         */
        share_begin_cleanup(cs);
        break;

      case SSH2_MSG_GLOBAL_REQUEST:
        /*
         * The only global requests we understand are "tcpip-forward"
         * and "cancel-tcpip-forward". Since those require us to
         * maintain state, we must assume that other global requests
         * will probably require that too, and so we don't forward on
         * any request we don't understand.
         */
        request_name = getstring(pkt, pktlen);
        if (request_name == NULL) {
            err = dupprintf("Truncated GLOBAL_REQUEST packet");
            goto confused;
        }

        if (!strcmp(request_name, "tcpip-forward")) {
            int wantreplypos, orig_wantreply, port, ret;
            char *host;

            sfree(request_name);

            /*
             * Pick the packet apart to find the want_reply field and
             * the host/port we're going to ask to listen on.
             */
            wantreplypos = getstring_size(pkt, pktlen);
            if (wantreplypos < 0 || wantreplypos >= pktlen) {
                err = dupprintf("Truncated GLOBAL_REQUEST packet");
                goto confused;
            }
            orig_wantreply = pkt[wantreplypos];
            port = getstring_size(pkt + (wantreplypos + 1),
                                  pktlen - (wantreplypos + 1));
            port += (wantreplypos + 1);
            if (port < 0 || port > pktlen - 4) {
                err = dupprintf("Truncated GLOBAL_REQUEST packet");
                goto confused;
            }
            host = getstring(pkt + (wantreplypos + 1),
                             pktlen - (wantreplypos + 1));
            assert(host != NULL);
            port = GET_32BIT(pkt + port);

            /*
             * See if we can allocate space in ssh.c's tree of remote
             * port forwardings. If we can't, it's because another
             * client sharing this connection has already allocated
             * the identical port forwarding, so we take it on
             * ourselves to manufacture a failure packet and send it
             * back to downstream.
             */
            ret = ssh_alloc_sharing_rportfwd(cs->parent->ssh, host, port, cs);
            if (!ret) {
                if (orig_wantreply) {
                    send_packet_to_downstream(cs, SSH2_MSG_REQUEST_FAILURE,
                                              "", 0, NULL);
                }
            } else {
                /*
                 * We've managed to make space for this forwarding
                 * locally. Pass the request on to the SSH server, but
                 * set want_reply even if it wasn't originally set, so
                 * that we know whether this forwarding needs to be
                 * cleaned up if downstream goes away.
                 */
                int old_wantreply = pkt[wantreplypos];
                pkt[wantreplypos] = 1;
                ssh_send_packet_from_downstream
                    (cs->parent->ssh, cs->id, type, pkt, pktlen,
                     old_wantreply ? NULL : "upstream added want_reply flag");
                fwd = share_add_forwarding(cs, host, port);
                ssh_sharing_queue_global_request(cs->parent->ssh, cs);

                if (fwd) {
                    globreq = snew(struct share_globreq);
                    globreq->next = NULL;
                    if (cs->globreq_tail)
                        cs->globreq_tail->next = globreq;
                    else
                        cs->globreq_head = globreq;
                    globreq->fwd = fwd;
                    globreq->want_reply = orig_wantreply;
                    globreq->type = GLOBREQ_TCPIP_FORWARD;
                }
            }

            sfree(host);
        } else if (!strcmp(request_name, "cancel-tcpip-forward")) {
            int wantreplypos, orig_wantreply, port;
            char *host;
            struct share_forwarding *fwd;

            sfree(request_name);

            /*
             * Pick the packet apart to find the want_reply field and
             * the host/port we're going to ask to listen on.
             */
            wantreplypos = getstring_size(pkt, pktlen);
            if (wantreplypos < 0 || wantreplypos >= pktlen) {
                err = dupprintf("Truncated GLOBAL_REQUEST packet");
                goto confused;
            }
            orig_wantreply = pkt[wantreplypos];
            port = getstring_size(pkt + (wantreplypos + 1),
                                  pktlen - (wantreplypos + 1));
            port += (wantreplypos + 1);
            if (port < 0 || port > pktlen - 4) {
                err = dupprintf("Truncated GLOBAL_REQUEST packet");
                goto confused;
            }
            host = getstring(pkt + (wantreplypos + 1),
                             pktlen - (wantreplypos + 1));
            assert(host != NULL);
            port = GET_32BIT(pkt + port);

            /*
             * Look up the existing forwarding with these details.
             */
            fwd = share_find_forwarding(cs, host, port);
            if (!fwd) {
                if (orig_wantreply) {
                    send_packet_to_downstream(cs, SSH2_MSG_REQUEST_FAILURE,
                                              "", 0, NULL);
                }
            } else {
                /*
                 * Pass the cancel request on to the SSH server, but
                 * set want_reply even if it wasn't originally set, so
                 * that _we_ know whether the forwarding has been
                 * deleted even if downstream doesn't want to know.
                 */
                int old_wantreply = pkt[wantreplypos];
                pkt[wantreplypos] = 1;
                ssh_send_packet_from_downstream
                    (cs->parent->ssh, cs->id, type, pkt, pktlen,
                     old_wantreply ? NULL : "upstream added want_reply flag");
                ssh_sharing_queue_global_request(cs->parent->ssh, cs);
            }

            sfree(host);
        } else {
            /*
             * Request we don't understand. Manufacture a failure
             * message if an answer was required.
             */
            int wantreplypos;

            sfree(request_name);

            wantreplypos = getstring_size(pkt, pktlen);
            if (wantreplypos < 0 || wantreplypos >= pktlen) {
                err = dupprintf("Truncated GLOBAL_REQUEST packet");
                goto confused;
            }
            if (pkt[wantreplypos])
                send_packet_to_downstream(cs, SSH2_MSG_REQUEST_FAILURE,
                                          "", 0, NULL);
        }
        break;

      case SSH2_MSG_CHANNEL_OPEN:
        /* Sender channel id comes after the channel type string */
        id_pos = getstring_size(pkt, pktlen);
        if (id_pos < 0 || id_pos > pktlen - 12) {
            err = dupprintf("Truncated CHANNEL_OPEN packet");
            goto confused;
        }

        old_id = GET_32BIT(pkt + id_pos);
        new_id = ssh_alloc_sharing_channel(cs->parent->ssh, cs);
        share_add_channel(cs, old_id, new_id, 0, UNACKNOWLEDGED,
                          GET_32BIT(pkt + id_pos + 8));
        PUT_32BIT(pkt + id_pos, new_id);
        ssh_send_packet_from_downstream(cs->parent->ssh, cs->id,
                                        type, pkt, pktlen, NULL);
        break;

      case SSH2_MSG_CHANNEL_OPEN_CONFIRMATION:
        if (pktlen < 16) {
            err = dupprintf("Truncated CHANNEL_OPEN_CONFIRMATION packet");
            goto confused;
        }

        id_pos = 4;  /* sender channel id is 2nd uint32 field in packet */
        old_id = GET_32BIT(pkt + id_pos);

        server_id = GET_32BIT(pkt);
        /* This server id may refer to either a halfchannel or an xchannel. */
        hc = NULL, xc = NULL;          /* placate optimiser */
        if ((hc = share_find_halfchannel(cs, server_id)) != NULL) {
            new_id = ssh_alloc_sharing_channel(cs->parent->ssh, cs);
        } else if ((xc = share_find_xchannel_by_server(cs, server_id))
                   != NULL) {
            new_id = xc->upstream_id;
        } else {
            err = dupprintf("CHANNEL_OPEN_CONFIRMATION packet cited unknown channel %u", (unsigned)server_id);
            goto confused;
        }
            
        PUT_32BIT(pkt + id_pos, new_id);

        chan = share_add_channel(cs, old_id, new_id, server_id, OPEN,
                                 GET_32BIT(pkt + 12));

        if (hc) {
            ssh_send_packet_from_downstream(cs->parent->ssh, cs->id,
                                            type, pkt, pktlen, NULL);
            share_remove_halfchannel(cs, hc);
        } else if (xc) {
            unsigned downstream_window = GET_32BIT(pkt + 8);
            if (downstream_window < 256) {
                err = dupprintf("Initial window size for x11 channel must be at least 256 (got %u)", downstream_window);
                goto confused;
            }
            share_xchannel_confirmation(cs, xc, chan, downstream_window);
            share_remove_xchannel(cs, xc);
        }

        break;

      case SSH2_MSG_CHANNEL_OPEN_FAILURE:
        if (pktlen < 4) {
            err = dupprintf("Truncated CHANNEL_OPEN_FAILURE packet");
            goto confused;
        }

        server_id = GET_32BIT(pkt);
        /* This server id may refer to either a halfchannel or an xchannel. */
        if ((hc = share_find_halfchannel(cs, server_id)) != NULL) {
            ssh_send_packet_from_downstream(cs->parent->ssh, cs->id,
                                            type, pkt, pktlen, NULL);
            share_remove_halfchannel(cs, hc);
        } else if ((xc = share_find_xchannel_by_server(cs, server_id))
                   != NULL) {
            share_xchannel_failure(cs, xc);
        } else {
            err = dupprintf("CHANNEL_OPEN_FAILURE packet cited unknown channel %u", (unsigned)server_id);
            goto confused;
        }

        break;

      case SSH2_MSG_CHANNEL_WINDOW_ADJUST:
      case SSH2_MSG_CHANNEL_DATA:
      case SSH2_MSG_CHANNEL_EXTENDED_DATA:
      case SSH2_MSG_CHANNEL_EOF:
      case SSH2_MSG_CHANNEL_CLOSE:
      case SSH2_MSG_CHANNEL_REQUEST:
      case SSH2_MSG_CHANNEL_SUCCESS:
      case SSH2_MSG_CHANNEL_FAILURE:
      case SSH2_MSG_IGNORE:
      case SSH2_MSG_DEBUG:
        if (type == SSH2_MSG_CHANNEL_REQUEST &&
            (request_name = getstring(pkt + 4, pktlen - 4)) != NULL) {
            /*
             * Agent forwarding requests from downstream are treated
             * specially. Because OpenSSHD doesn't let us enable agent
             * forwarding independently per session channel, and in
             * particular because the OpenSSH-defined agent forwarding
             * protocol does not mark agent-channel requests with the
             * id of the session channel they originate from, the only
             * way we can implement agent forwarding in a
             * connection-shared PuTTY is to forward the _upstream_
             * agent. Hence, we unilaterally deny agent forwarding
             * requests from downstreams if we aren't prepared to
             * forward an agent ourselves.
             *
             * (If we are, then we dutifully pass agent forwarding
             * requests upstream. OpenSSHD has the curious behaviour
             * that all but the first such request will be rejected,
             * but all session channels opened after the first request
             * get agent forwarding enabled whether they ask for it or
             * not; but that's not our concern, since other SSH
             * servers supporting the same piece of protocol might in
             * principle at least manage to enable agent forwarding on
             * precisely the channels that requested it, even if the
             * subsequent CHANNEL_OPENs still can't be associated with
             * a parent session channel.)
             */
            if (!strcmp(request_name, "auth-agent-req@openssh.com") &&
                !ssh_agent_forwarding_permitted(cs->parent->ssh)) {
                unsigned server_id = GET_32BIT(pkt);
                unsigned char recipient_id[4];

                sfree(request_name);

                chan = share_find_channel_by_server(cs, server_id);
                if (chan) {
                    PUT_32BIT(recipient_id, chan->downstream_id);
                    send_packet_to_downstream(cs, SSH2_MSG_CHANNEL_FAILURE,
                                              recipient_id, 4, NULL);
                } else {
                    char *buf = dupprintf("Agent forwarding request for "
                                          "unrecognised channel %u", server_id);
                    share_disconnect(cs, buf);
                    sfree(buf);
                    return;
                }
                break;
            }

            /*
             * Another thing we treat specially is X11 forwarding
             * requests. For these, we have to make up another set of
             * X11 auth data, and enter it into our SSH connection's
             * list of possible X11 authorisation credentials so that
             * when we see an X11 channel open request we can know
             * whether it's one to handle locally or one to pass on to
             * a downstream, and if the latter, which one.
             */
            if (!strcmp(request_name, "x11-req")) {
                unsigned server_id = GET_32BIT(pkt);
                int want_reply, single_connection, screen;
                char *auth_proto_str, *auth_data;
                int auth_proto, protolen, datalen;
                int pos;

                sfree(request_name);

                chan = share_find_channel_by_server(cs, server_id);
                if (!chan) {
                    char *buf = dupprintf("X11 forwarding request for "
                                          "unrecognised channel %u", server_id);
                    share_disconnect(cs, buf);
                    sfree(buf);
                    return;
                }

                /*
                 * Pick apart the whole message to find the downstream
                 * auth details.
                 */
                /* we have already seen: 4 bytes channel id, 4+7 request name */
                if (pktlen < 17) {
                    err = dupprintf("Truncated CHANNEL_REQUEST(\"x11\") packet");
                    goto confused;
                }
                want_reply = pkt[15] != 0;
                single_connection = pkt[16] != 0;
                auth_proto_str = getstring(pkt+17, pktlen-17);
                auth_proto = x11_identify_auth_proto(auth_proto_str);
                sfree(auth_proto_str);
                pos = 17 + getstring_size(pkt+17, pktlen-17);
                auth_data = getstring(pkt+pos, pktlen-pos);
                pos += getstring_size(pkt+pos, pktlen-pos);

                if (pktlen < pos+4) {
                    err = dupprintf("Truncated CHANNEL_REQUEST(\"x11\") packet");
                    sfree(auth_data);
                    goto confused;
                }
                screen = GET_32BIT(pkt+pos);

                if (auth_proto < 0) {
                    /* Reject due to not understanding downstream's
                     * requested authorisation method. */
                    unsigned char recipient_id[4];
                    PUT_32BIT(recipient_id, chan->downstream_id);
                    send_packet_to_downstream(cs, SSH2_MSG_CHANNEL_FAILURE,
                                              recipient_id, 4, NULL);
                    sfree(auth_data);
                    break;
                }

                chan->x11_auth_proto = auth_proto;
                chan->x11_auth_data = x11_dehexify(auth_data,
                                                   &chan->x11_auth_datalen);
                sfree(auth_data);
                chan->x11_auth_upstream =
                    ssh_sharing_add_x11_display(cs->parent->ssh, auth_proto,
                                                cs, chan);
                chan->x11_one_shot = single_connection;

                /*
                 * Now construct a replacement X forwarding request,
                 * containing our own auth data, and send that to the
                 * server.
                 */
                protolen = strlen(chan->x11_auth_upstream->protoname);
                datalen = strlen(chan->x11_auth_upstream->datastring);
                pktlen = 29+protolen+datalen;
                pkt = snewn(pktlen, unsigned char);
                PUT_32BIT(pkt, server_id);
                PUT_32BIT(pkt+4, 7);   /* strlen("x11-req") */
                memcpy(pkt+8, "x11-req", 7);
                pkt[15] = want_reply;
                pkt[16] = single_connection;
                PUT_32BIT(pkt+17, protolen);
                memcpy(pkt+21, chan->x11_auth_upstream->protoname, protolen);
                PUT_32BIT(pkt+21+protolen, datalen);
                memcpy(pkt+25+protolen, chan->x11_auth_upstream->datastring,
                       datalen);
                PUT_32BIT(pkt+25+protolen+datalen, screen);
                ssh_send_packet_from_downstream(cs->parent->ssh, cs->id,
                                                SSH2_MSG_CHANNEL_REQUEST,
                                                pkt, pktlen, NULL);
                sfree(pkt);

                break;
            }

            sfree(request_name);
        }

        ssh_send_packet_from_downstream(cs->parent->ssh, cs->id,
                                        type, pkt, pktlen, NULL);
        if (type == SSH2_MSG_CHANNEL_CLOSE && pktlen >= 4) {
            server_id = GET_32BIT(pkt);
            chan = share_find_channel_by_server(cs, server_id);
            if (chan) {
                if (chan->state == RCVD_CLOSE) {
                    ssh_delete_sharing_channel(cs->parent->ssh,
                                               chan->upstream_id);
                    share_remove_channel(cs, chan);
                } else {
                    chan->state = SENT_CLOSE;
                }
            }
        }
        break;

      default:
        err = dupprintf("Unexpected packet type %d\n", type);
        goto confused;

        /*
         * Any other packet type is unexpected. In particular, we
         * never pass GLOBAL_REQUESTs downstream, so we never expect
         * to see SSH2_MSG_REQUEST_{SUCCESS,FAILURE}.
         */
      confused:
        assert(err != NULL);
        share_disconnect(cs, err);
        sfree(err);
        break;
    }
}

/*
 * Coroutine macros similar to, but simplified from, those in ssh.c.
 */
#define crBegin(v)	{ int *crLine = &v; switch(v) { case 0:;
#define crFinish(z)	} *crLine = 0; return (z); }
#define crGetChar(c) do                                         \
    {                                                           \
        while (len == 0) {                                      \
            *crLine =__LINE__; return 1; case __LINE__:;        \
        }                                                       \
        len--;                                                  \
        (c) = (unsigned char)*data++;                           \
    } while (0)

static int share_receive(Plug plug, int urgent, char *data, int len)
{
    struct ssh_sharing_connstate *cs = (struct ssh_sharing_connstate *)plug;
    static const char expected_verstring_prefix[] =
        "SSHCONNECTION@putty.projects.tartarus.org-2.0-";
    unsigned char c;

    crBegin(cs->crLine);

    /*
     * First read the version string from downstream.
     */
    cs->recvlen = 0;
    while (1) {
        crGetChar(c);
        if (c == '\012')
            break;
        if (cs->recvlen >= sizeof(cs->recvbuf)) {
            char *buf = dupprintf("Version string far too long\n");
            share_disconnect(cs, buf);
            sfree(buf);
            goto dead;
        }
        cs->recvbuf[cs->recvlen++] = c;
    }

    /*
     * Now parse the version string to make sure it's at least vaguely
     * sensible, and log it.
     */
    if (cs->recvlen < sizeof(expected_verstring_prefix)-1 ||
        memcmp(cs->recvbuf, expected_verstring_prefix,
               sizeof(expected_verstring_prefix) - 1)) {
        char *buf = dupprintf("Version string did not have expected prefix\n");
        share_disconnect(cs, buf);
        sfree(buf);
        goto dead;
    }
    if (cs->recvlen > 0 && cs->recvbuf[cs->recvlen-1] == '\015')
        cs->recvlen--;                 /* trim off \r before \n */
    ssh_sharing_logf(cs->parent->ssh, cs->id,
                     "Downstream version string: %.*s",
                     cs->recvlen, cs->recvbuf);

    /*
     * Loop round reading packets.
     */
    while (1) {
        cs->recvlen = 0;
        while (cs->recvlen < 4) {
            crGetChar(c);
            cs->recvbuf[cs->recvlen++] = c;
        }
        cs->curr_packetlen = toint(GET_32BIT(cs->recvbuf) + 4);
        if (cs->curr_packetlen < 5 ||
            cs->curr_packetlen > sizeof(cs->recvbuf)) {
            char *buf = dupprintf("Bad packet length %u\n",
                                  (unsigned)cs->curr_packetlen);
            share_disconnect(cs, buf);
            sfree(buf);
            goto dead;
        }
        while (cs->recvlen < cs->curr_packetlen) {
            crGetChar(c);
            cs->recvbuf[cs->recvlen++] = c;
        }

        share_got_pkt_from_downstream(cs, cs->recvbuf[4],
                                      cs->recvbuf + 5, cs->recvlen - 5);
    }

  dead:;
    crFinish(1);
}

static void share_sent(Plug plug, int bufsize)
{
    /* struct ssh_sharing_connstate *cs = (struct ssh_sharing_connstate *)plug; */

    /*
     * We do nothing here, because we expect that there won't be a
     * need to throttle and unthrottle the connection to a downstream.
     * It should automatically throttle itself: if the SSH server
     * sends huge amounts of data on all channels then it'll run out
     * of window until our downstream sends it back some
     * WINDOW_ADJUSTs.
     */
}

static int share_listen_closing(Plug plug, const char *error_msg,
                                int error_code, int calling_back)
{
    struct ssh_sharing_state *sharestate = (struct ssh_sharing_state *)plug;
    if (error_msg)
        ssh_sharing_logf(sharestate->ssh, 0,
                         "listening socket: %s", error_msg);
    sk_close(sharestate->listensock);
    sharestate->listensock = NULL;
    return 1;
}

static void share_send_verstring(struct ssh_sharing_connstate *cs)
{
    char *fullstring = dupcat("SSHCONNECTION@putty.projects.tartarus.org-2.0-",
                              cs->parent->server_verstring, "\015\012", NULL);
    sk_write(cs->sock, fullstring, strlen(fullstring));
    sfree(fullstring);

    cs->sent_verstring = TRUE;
}

int share_ndownstreams(void *state)
{
    struct ssh_sharing_state *sharestate = (struct ssh_sharing_state *)state;
    return count234(sharestate->connections);
}

void share_activate(void *state, const char *server_verstring)
{
    /*
     * Indication from ssh.c that we are now ready to begin serving
     * any downstreams that have already connected to us.
     */
    struct ssh_sharing_state *sharestate = (struct ssh_sharing_state *)state;
    struct ssh_sharing_connstate *cs;
    int i;

    /*
     * Trim the server's version string down to just the software
     * version component, removing "SSH-2.0-" or whatever at the
     * front.
     */
    for (i = 0; i < 2; i++) {
        server_verstring += strcspn(server_verstring, "-");
        if (*server_verstring)
            server_verstring++;
    }

    sharestate->server_verstring = dupstr(server_verstring);

    for (i = 0; (cs = (struct ssh_sharing_connstate *)
                 index234(sharestate->connections, i)) != NULL; i++) {
        assert(!cs->sent_verstring);
        share_send_verstring(cs);
    }
}

static int share_listen_accepting(Plug plug,
                                  accept_fn_t constructor, accept_ctx_t ctx)
{
    static const struct plug_function_table connection_fn_table = {
	NULL, /* no log function, because that's for outgoing connections */
	share_closing,
        share_receive,
        share_sent,
	NULL /* no accepting function, because we've already done it */
    };
    struct ssh_sharing_state *sharestate = (struct ssh_sharing_state *)plug;
    struct ssh_sharing_connstate *cs;
    const char *err;
    char *peerinfo;

    /*
     * A new downstream has connected to us.
     */
    cs = snew(struct ssh_sharing_connstate);
    cs->fn = &connection_fn_table;
    cs->parent = sharestate;

    if ((cs->id = share_find_unused_id(sharestate, sharestate->nextid)) == 0 &&
        (cs->id = share_find_unused_id(sharestate, 1)) == 0) {
        sfree(cs);
        return 1;
    }
    sharestate->nextid = cs->id + 1;
    if (sharestate->nextid == 0)
        sharestate->nextid++; /* only happens in VERY long-running upstreams */

    cs->sock = constructor(ctx, (Plug) cs);
    if ((err = sk_socket_error(cs->sock)) != NULL) {
        sfree(cs);
	return err != NULL;
    }

    sk_set_frozen(cs->sock, 0);

    add234(cs->parent->connections, cs);

    cs->sent_verstring = FALSE;
    if (sharestate->server_verstring)
        share_send_verstring(cs);

    cs->got_verstring = FALSE;
    cs->recvlen = 0;
    cs->crLine = 0;
    cs->halfchannels = newtree234(share_halfchannel_cmp);
    cs->channels_by_us = newtree234(share_channel_us_cmp);
    cs->channels_by_server = newtree234(share_channel_server_cmp);
    cs->xchannels_by_us = newtree234(share_xchannel_us_cmp);
    cs->xchannels_by_server = newtree234(share_xchannel_server_cmp);
    cs->forwardings = newtree234(share_forwarding_cmp);
    cs->globreq_head = cs->globreq_tail = NULL;

    peerinfo = sk_peer_info(cs->sock);
    ssh_sharing_downstream_connected(sharestate->ssh, cs->id, peerinfo);
    sfree(peerinfo);

    return 0;
}

/* Per-application overrides for what roles we can take (e.g. pscp
 * will never be an upstream) */
extern const int share_can_be_downstream;
extern const int share_can_be_upstream;

/*
 * Init function for connection sharing. We either open a listening
 * socket and become an upstream, or connect to an existing one and
 * become a downstream, or do neither. We are responsible for deciding
 * which of these to do (including checking the Conf to see if
 * connection sharing is even enabled in the first place). If we
 * become a downstream, we return the Socket with which we connected
 * to the upstream; otherwise (whether or not we have established an
 * upstream) we return NULL.
 */
Socket ssh_connection_sharing_init(const char *host, int port,
                                   Conf *conf, Ssh ssh, void **state)
{
    static const struct plug_function_table listen_fn_table = {
	NULL, /* no log function, because that's for outgoing connections */
	share_listen_closing,
        NULL, /* no receive function on a listening socket */
        NULL, /* no sent function on a listening socket */
	share_listen_accepting
    };

    int result, can_upstream, can_downstream;
    char *logtext, *ds_err, *us_err;
    char *sockname;
    Socket sock;
    struct ssh_sharing_state *sharestate;

    if (!conf_get_int(conf, CONF_ssh_connection_sharing))
        return NULL;                   /* do not share anything */
    can_upstream = share_can_be_upstream &&
        conf_get_int(conf, CONF_ssh_connection_sharing_upstream);
    can_downstream = share_can_be_downstream &&
        conf_get_int(conf, CONF_ssh_connection_sharing_downstream);
    if (!can_upstream && !can_downstream)
        return NULL;

    /*
     * Decide on the string used to identify the connection point
     * between upstream and downstream (be it a Windows named pipe or
     * a Unix-domain socket or whatever else).
     *
     * I wondered about making this a SHA hash of all sorts of pieces
     * of the PuTTY configuration - essentially everything PuTTY uses
     * to know where and how to make a connection, including all the
     * proxy details (or rather, all the _relevant_ ones - only
     * including settings that other settings didn't prevent from
     * having any effect), plus the username. However, I think it's
     * better to keep it really simple: the connection point
     * identifier is derived from the hostname and port used to index
     * the host-key cache (not necessarily where we _physically_
     * connected to, in cases involving proxies or CONF_loghost), plus
     * the username if one is specified.
     */
    {
        char *username = get_remote_username(conf);

        if (port == 22) {
            if (username)
                sockname = dupprintf("%s@%s", username, host);
            else
                sockname = dupprintf("%s", host);
        } else {
            if (username)
                sockname = dupprintf("%s@%s:%d", username, host, port);
            else
                sockname = dupprintf("%s:%d", host, port);
        }

        sfree(username);

        /*
         * The platform-specific code may transform this further in
         * order to conform to local namespace conventions (e.g. not
         * using slashes in filenames), but that's its job and not
         * ours.
         */
    }

    /*
     * Create a data structure for the listening plug if we turn out
     * to be an upstream.
     */
    sharestate = snew(struct ssh_sharing_state);
    sharestate->fn = &listen_fn_table;
    sharestate->listensock = NULL;

    /*
     * Now hand off to a per-platform routine that either connects to
     * an existing upstream (using 'ssh' as the plug), establishes our
     * own upstream (using 'sharestate' as the plug), or forks off a
     * separate upstream and then connects to that. It will return a
     * code telling us which kind of socket it put in 'sock'.
     */
    sock = NULL;
    logtext = ds_err = us_err = NULL;
    result = platform_ssh_share(sockname, conf, (Plug)ssh,
                                (Plug)sharestate, &sock, &logtext, &ds_err,
                                &us_err, can_upstream, can_downstream);
    ssh_connshare_log(ssh, result, logtext, ds_err, us_err);
    sfree(logtext);
    sfree(ds_err);
    sfree(us_err);
    switch (result) {
      case SHARE_NONE:
        /*
         * We aren't sharing our connection at all (e.g. something
         * went wrong setting the socket up). Free the upstream
         * structure and return NULL.
         */
        assert(sock == NULL);
        *state = NULL;
        sfree(sharestate);
        sfree(sockname);
        return NULL;

      case SHARE_DOWNSTREAM:
        /*
         * We are downstream, so free sharestate which it turns out we
         * don't need after all, and return the downstream socket as a
         * replacement for an ordinary SSH connection.
         */
        *state = NULL;
        sfree(sharestate);
        sfree(sockname);
        return sock;

      case SHARE_UPSTREAM:
        /*
         * We are upstream. Set up sharestate properly and pass a copy
         * to the caller; return NULL, to tell ssh.c that it has to
         * make an ordinary connection after all.
         */
        *state = sharestate;
        sharestate->listensock = sock;
        sharestate->connections = newtree234(share_connstate_cmp);
        sharestate->ssh = ssh;
        sharestate->server_verstring = NULL;
        sharestate->sockname = sockname;
        sharestate->nextid = 1;
        return NULL;
    }

    return NULL;
}
