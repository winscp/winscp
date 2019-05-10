/*
 * Code to handle the initial SSH version string exchange.
 */

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include "putty.h"
#include "ssh.h"
#include "sshbpp.h"
#include "sshcr.h"

#define PREFIX_MAXLEN 64

struct ssh_verstring_state {
    int crState;

    Conf *conf;
    ptrlen prefix_wanted;
    char *our_protoversion;
    struct ssh_version_receiver *receiver;

    bool send_early;

    bool found_prefix;
    int major_protoversion;
    int remote_bugs;
    char prefix[PREFIX_MAXLEN];
    char *impl_name;
    strbuf *vstring;
    char *protoversion;
    const char *softwareversion;

    char *our_vstring;
    int i;

    BinaryPacketProtocol bpp;
};

static void ssh_verstring_free(BinaryPacketProtocol *bpp);
static void ssh_verstring_handle_input(BinaryPacketProtocol *bpp);
static void ssh_verstring_handle_output(BinaryPacketProtocol *bpp);
static PktOut *ssh_verstring_new_pktout(int type);
static void ssh_verstring_queue_disconnect(BinaryPacketProtocol *bpp,
                                          const char *msg, int category);

static const struct BinaryPacketProtocolVtable ssh_verstring_vtable = {
    ssh_verstring_free,
    ssh_verstring_handle_input,
    ssh_verstring_handle_output,
    ssh_verstring_new_pktout,
    ssh_verstring_queue_disconnect,
};

static void ssh_detect_bugs(struct ssh_verstring_state *s);
static bool ssh_version_includes_v1(const char *ver);
static bool ssh_version_includes_v2(const char *ver);

BinaryPacketProtocol *ssh_verstring_new(
    Conf *conf, LogContext *logctx, bool bare_connection_mode,
    const char *protoversion, struct ssh_version_receiver *rcv,
    bool server_mode, const char *impl_name)
{
    struct ssh_verstring_state *s = snew(struct ssh_verstring_state);

    memset(s, 0, sizeof(struct ssh_verstring_state));

    if (!bare_connection_mode) {
        s->prefix_wanted = PTRLEN_LITERAL("SSH-");
    } else {
        /*
         * Ordinary SSH begins with the banner "SSH-x.y-...". Here,
         * we're going to be speaking just the ssh-connection
         * subprotocol, extracted and given a trivial binary packet
         * protocol, so we need a new banner.
         *
         * The new banner is like the ordinary SSH banner, but
         * replaces the prefix 'SSH-' at the start with a new name. In
         * proper SSH style (though of course this part of the proper
         * SSH protocol _isn't_ subject to this kind of
         * DNS-domain-based extension), we define the new name in our
         * extension space.
         */
        s->prefix_wanted = PTRLEN_LITERAL(
            "SSHCONNECTION@putty.projects.tartarus.org-");
    }
    assert(s->prefix_wanted.len <= PREFIX_MAXLEN);

    s->conf = conf_copy(conf);
    s->bpp.logctx = logctx;
    s->our_protoversion = dupstr(protoversion);
    s->receiver = rcv;
    s->impl_name = dupstr(impl_name);
    s->vstring = strbuf_new();

    /*
     * We send our version string early if we can. But if it includes
     * SSH-1, we can't, because we have to take the other end into
     * account too (see below).
     *
     * In server mode, we do send early.
     */
    s->send_early = server_mode || !ssh_version_includes_v1(protoversion);

    s->bpp.vt = &ssh_verstring_vtable;
    ssh_bpp_common_setup(&s->bpp);
    return &s->bpp;
}

void ssh_verstring_free(BinaryPacketProtocol *bpp)
{
    struct ssh_verstring_state *s =
        container_of(bpp, struct ssh_verstring_state, bpp);
    conf_free(s->conf);
    sfree(s->impl_name);
    strbuf_free(s->vstring);
    sfree(s->protoversion);
    sfree(s->our_vstring);
    sfree(s->our_protoversion);
    sfree(s);
}

static int ssh_versioncmp(const char *a, const char *b)
{
    char *ae, *be;
    unsigned long av, bv;

    av = strtoul(a, &ae, 10);
    bv = strtoul(b, &be, 10);
    if (av != bv)
        return (av < bv ? -1 : +1);
    if (*ae == '.')
        ae++;
    if (*be == '.')
        be++;
    av = strtoul(ae, &ae, 10);
    bv = strtoul(be, &be, 10);
    if (av != bv)
        return (av < bv ? -1 : +1);
    return 0;
}

static bool ssh_version_includes_v1(const char *ver)
{
    return ssh_versioncmp(ver, "2.0") < 0;
}

static bool ssh_version_includes_v2(const char *ver)
{
    return ssh_versioncmp(ver, "1.99") >= 0;
}

static void ssh_verstring_send(struct ssh_verstring_state *s)
{
    BinaryPacketProtocol *bpp = &s->bpp; /* for bpp_logevent */
    char *p;
    int sv_pos;

    /*
     * Construct our outgoing version string.
     */
    s->our_vstring = dupprintf(
        "%.*s%s-%s%s",
        (int)s->prefix_wanted.len, (const char *)s->prefix_wanted.ptr,
        s->our_protoversion, s->impl_name, sshver);
    sv_pos = s->prefix_wanted.len + strlen(s->our_protoversion) + 1;

    /* Convert minus signs and spaces in the software version string
     * into underscores. */
    for (p = s->our_vstring + sv_pos; *p; p++) {
        if (*p == '-' || *p == ' ')
            *p = '_';
    }

#ifdef FUZZING
    /*
     * Replace the first character of the string with an "I" if we're
     * compiling this code for fuzzing - i.e. the protocol prefix
     * becomes "ISH-" instead of "SSH-".
     *
     * This is irrelevant to any real client software (the only thing
     * reading the output of PuTTY built for fuzzing is the fuzzer,
     * which can adapt to whatever it sees anyway). But it's a safety
     * precaution making it difficult to accidentally run such a
     * version of PuTTY (which would be hugely insecure) against a
     * live peer implementation.
     *
     * (So the replacement prefix "ISH" notionally stands for
     * 'Insecure Shell', of course.)
     */
    s->our_vstring[0] = 'I';
#endif

    /*
     * Now send that version string, plus trailing \r\n or just \n
     * (the latter in SSH-1 mode).
     */
    bufchain_add(s->bpp.out_raw, s->our_vstring, strlen(s->our_vstring));
    if (ssh_version_includes_v2(s->our_protoversion))
        bufchain_add(s->bpp.out_raw, "\015", 1);
    bufchain_add(s->bpp.out_raw, "\012", 1);

    bpp_logevent("We claim version: %s", s->our_vstring);
}

#define BPP_WAITFOR(minlen) do                          \
    {                                                                   \
        bool success;                                                   \
        crMaybeWaitUntilV(                                              \
            (success = (bufchain_size(s->bpp.in_raw) >= (minlen))) ||   \
            s->bpp.input_eof);                                          \
        if (!success)                                                   \
            goto eof;                                                   \
    } while (0)

void ssh_verstring_handle_input(BinaryPacketProtocol *bpp)
{
    struct ssh_verstring_state *s =
        container_of(bpp, struct ssh_verstring_state, bpp);

    crBegin(s->crState);

    /*
     * If we're sending our version string up front before seeing the
     * other side's, then do it now.
     */
    if (s->send_early)
        ssh_verstring_send(s);

    /*
     * Search for a line beginning with the protocol name prefix in
     * the input.
     */
    s->i = 0;
    while (1) {
        /*
         * Every time round this loop, we're at the start of a new
         * line, so look for the prefix.
         */
        BPP_WAITFOR(s->prefix_wanted.len);
        bufchain_fetch(s->bpp.in_raw, s->prefix, s->prefix_wanted.len);
        if (!memcmp(s->prefix, s->prefix_wanted.ptr, s->prefix_wanted.len)) {
            bufchain_consume(s->bpp.in_raw, s->prefix_wanted.len);
            ssh_check_frozen(s->bpp.ssh);
            break;
        }

        /*
         * If we didn't find it, consume data until we see a newline.
         */
        while (1) {
            ptrlen data;
            char *nl;

            /* Wait to receive at least 1 byte, but then consume more
             * than that if it's there. */
            BPP_WAITFOR(1);
            data = bufchain_prefix(s->bpp.in_raw);
            if ((nl = memchr(data.ptr, '\012', data.len)) != NULL) {
                bufchain_consume(s->bpp.in_raw, nl - (char *)data.ptr + 1);
                ssh_check_frozen(s->bpp.ssh);
                break;
            } else {
                bufchain_consume(s->bpp.in_raw, data.len);
                ssh_check_frozen(s->bpp.ssh);
            }
        }
    }

    s->found_prefix = true;

    /*
     * Copy the greeting line so far into vstring.
     */
    put_data(s->vstring, s->prefix_wanted.ptr, s->prefix_wanted.len);

    /*
     * Now read the rest of the greeting line.
     */
    s->i = 0;
    do {
        ptrlen data;
        char *nl;

        BPP_WAITFOR(1);
        data = bufchain_prefix(s->bpp.in_raw);
        if ((nl = memchr(data.ptr, '\012', data.len)) != NULL) {
            data.len = nl - (char *)data.ptr + 1;
        }

        put_datapl(s->vstring, data);
        bufchain_consume(s->bpp.in_raw, data.len);
        ssh_check_frozen(s->bpp.ssh);

    } while (s->vstring->s[s->vstring->len-1] != '\012');

    /*
     * Trim \r and \n from the version string, and replace them with
     * a NUL terminator.
     */
    while (s->vstring->len > 0 &&
           (s->vstring->s[s->vstring->len-1] == '\r' ||
            s->vstring->s[s->vstring->len-1] == '\n'))
        s->vstring->len--;
    s->vstring->s[s->vstring->len] = '\0';

    bpp_logevent("Remote version: %s", s->vstring->s);

    /*
     * Pick out the protocol version and software version. The former
     * goes in a separately allocated string, so that s->vstring
     * remains intact for later use in key exchange; the latter is the
     * tail of s->vstring, so it doesn't need to be allocated.
     */
    {
        const char *pv_start = s->vstring->s + s->prefix_wanted.len;
        int pv_len = strcspn(pv_start, "-");
        s->protoversion = dupprintf("%.*s", pv_len, pv_start);
        s->softwareversion = pv_start + pv_len;
        if (*s->softwareversion) {
            assert(*s->softwareversion == '-');
            s->softwareversion++;
        }
    }

    ssh_detect_bugs(s);

    /*
     * Figure out what actual SSH protocol version we're speaking.
     */
    if (ssh_version_includes_v2(s->our_protoversion) &&
        ssh_version_includes_v2(s->protoversion)) {
        /*
         * We're doing SSH-2.
         */
        s->major_protoversion = 2;
    } else if (ssh_version_includes_v1(s->our_protoversion) &&
               ssh_version_includes_v1(s->protoversion)) {
        /*
         * We're doing SSH-1.
         */
        s->major_protoversion = 1;

        /*
         * There are multiple minor versions of SSH-1, and the
         * protocol does not specify that the minimum of client
         * and server versions is used. So we must adjust our
         * outgoing protocol version to be no higher than that of
         * the other side.
         */
        if (!s->send_early &&
            ssh_versioncmp(s->our_protoversion, s->protoversion) > 0) {
            sfree(s->our_protoversion);
            s->our_protoversion = dupstr(s->protoversion);
        }
    } else {
        /*
         * Unable to agree on a major protocol version at all.
         */
        if (!ssh_version_includes_v2(s->our_protoversion)) {
            ssh_sw_abort(s->bpp.ssh,
                         "SSH protocol version 1 required by our "
                         "configuration but not provided by remote");
        } else {
            ssh_sw_abort(s->bpp.ssh,
                         "SSH protocol version 2 required by our "
                         "configuration but remote only provides "
                         "(old, insecure) SSH-1");
        }
        crStopV;
    }

    bpp_logevent("Using SSH protocol version %d", s->major_protoversion);

    if (!s->send_early) {
        /*
         * If we didn't send our version string early, construct and
         * send it now, because now we know what it is.
         */
        ssh_verstring_send(s);
    }

    /*
     * And we're done. Notify our receiver that we now know our
     * protocol version. This will cause it to disconnect us from the
     * input stream and ultimately free us, because our job is now
     * done.
     */
    s->receiver->got_ssh_version(s->receiver, s->major_protoversion);
    return;

  eof:
    ssh_remote_error(s->bpp.ssh,
                     "Remote side unexpectedly closed network connection");
    return;  /* avoid touching s now it's been freed */

    crFinishV;
}

static PktOut *ssh_verstring_new_pktout(int type)
{
    unreachable("Should never try to send packets during SSH version "
                "string exchange");
}

static void ssh_verstring_handle_output(BinaryPacketProtocol *bpp)
{
    if (pq_peek(&bpp->out_pq)) {
        unreachable("Should never try to send packets during SSH version "
                    "string exchange");
    }
}

/*
 * Examine the remote side's version string, and compare it against a
 * list of known buggy implementations.
 */
static void ssh_detect_bugs(struct ssh_verstring_state *s)
{
    BinaryPacketProtocol *bpp = &s->bpp; /* for bpp_logevent */
    const char *imp = s->softwareversion;

    s->remote_bugs = 0;

    /*
     * General notes on server version strings:
     *  - Not all servers reporting "Cisco-1.25" have all the bugs listed
     *    here -- in particular, we've heard of one that's perfectly happy
     *    with SSH1_MSG_IGNOREs -- but this string never seems to change,
     *    so we can't distinguish them.
     */
    if (conf_get_int(s->conf, CONF_sshbug_ignore1) == FORCE_ON ||
        (conf_get_int(s->conf, CONF_sshbug_ignore1) == AUTO &&
         (!strcmp(imp, "1.2.18") || !strcmp(imp, "1.2.19") ||
          !strcmp(imp, "1.2.20") || !strcmp(imp, "1.2.21") ||
          !strcmp(imp, "1.2.22") || !strcmp(imp, "Cisco-1.25") ||
          !strcmp(imp, "OSU_1.4alpha3") || !strcmp(imp, "OSU_1.5alpha4")))) {
        /*
         * These versions don't support SSH1_MSG_IGNORE, so we have
         * to use a different defence against password length
         * sniffing.
         */
        s->remote_bugs |= BUG_CHOKES_ON_SSH1_IGNORE;
        bpp_logevent("We believe remote version has SSH-1 ignore bug");
    }

    if (conf_get_int(s->conf, CONF_sshbug_plainpw1) == FORCE_ON ||
        (conf_get_int(s->conf, CONF_sshbug_plainpw1) == AUTO &&
         (!strcmp(imp, "Cisco-1.25") || !strcmp(imp, "OSU_1.4alpha3")))) {
        /*
         * These versions need a plain password sent; they can't
         * handle having a null and a random length of data after
         * the password.
         */
        s->remote_bugs |= BUG_NEEDS_SSH1_PLAIN_PASSWORD;
        bpp_logevent("We believe remote version needs a "
                     "plain SSH-1 password");
    }

    if (conf_get_int(s->conf, CONF_sshbug_rsa1) == FORCE_ON ||
        (conf_get_int(s->conf, CONF_sshbug_rsa1) == AUTO &&
         (!strcmp(imp, "Cisco-1.25")))) {
        /*
         * These versions apparently have no clue whatever about
         * RSA authentication and will panic and die if they see
         * an AUTH_RSA message.
         */
        s->remote_bugs |= BUG_CHOKES_ON_RSA;
        bpp_logevent("We believe remote version can't handle SSH-1 "
                     "RSA authentication");
    }

    if (conf_get_int(s->conf, CONF_sshbug_hmac2) == FORCE_ON ||
        (conf_get_int(s->conf, CONF_sshbug_hmac2) == AUTO &&
         !wc_match("* VShell", imp) &&
         (wc_match("2.1.0*", imp) || wc_match("2.0.*", imp) ||
          wc_match("2.2.0*", imp) || wc_match("2.3.0*", imp) ||
          wc_match("2.1 *", imp)))) {
        /*
         * These versions have the HMAC bug.
         */
        s->remote_bugs |= BUG_SSH2_HMAC;
        bpp_logevent("We believe remote version has SSH-2 HMAC bug");
    }

    if (conf_get_int(s->conf, CONF_sshbug_derivekey2) == FORCE_ON ||
        (conf_get_int(s->conf, CONF_sshbug_derivekey2) == AUTO &&
         !wc_match("* VShell", imp) &&
         (wc_match("2.0.0*", imp) || wc_match("2.0.10*", imp) ))) {
        /*
         * These versions have the key-derivation bug (failing to
         * include the literal shared secret in the hashes that
         * generate the keys).
         */
        s->remote_bugs |= BUG_SSH2_DERIVEKEY;
        bpp_logevent("We believe remote version has SSH-2 "
                     "key-derivation bug");
    }

    if (conf_get_int(s->conf, CONF_sshbug_rsapad2) == FORCE_ON ||
        (conf_get_int(s->conf, CONF_sshbug_rsapad2) == AUTO &&
         (wc_match("OpenSSH_2.[5-9]*", imp) ||
          wc_match("OpenSSH_3.[0-2]*", imp) ||
          wc_match("mod_sftp/0.[0-8]*", imp) ||
          wc_match("mod_sftp/0.9.[0-8]", imp)))) {
        /*
         * These versions have the SSH-2 RSA padding bug.
         */
        s->remote_bugs |= BUG_SSH2_RSA_PADDING;
        bpp_logevent("We believe remote version has SSH-2 RSA padding bug");
    }

    if (conf_get_int(s->conf, CONF_sshbug_pksessid2) == FORCE_ON ||
        (conf_get_int(s->conf, CONF_sshbug_pksessid2) == AUTO &&
         wc_match("OpenSSH_2.[0-2]*", imp))) {
        /*
         * These versions have the SSH-2 session-ID bug in
         * public-key authentication.
         */
        s->remote_bugs |= BUG_SSH2_PK_SESSIONID;
        bpp_logevent("We believe remote version has SSH-2 "
                     "public-key-session-ID bug");
    }

    if (conf_get_int(s->conf, CONF_sshbug_rekey2) == FORCE_ON ||
        (conf_get_int(s->conf, CONF_sshbug_rekey2) == AUTO &&
         (wc_match("DigiSSH_2.0", imp) ||
          wc_match("OpenSSH_2.[0-4]*", imp) ||
          wc_match("OpenSSH_2.5.[0-3]*", imp) ||
          wc_match("Sun_SSH_1.0", imp) ||
          wc_match("Sun_SSH_1.0.1", imp) ||
          /* All versions <= 1.2.6 (they changed their format in 1.2.7) */
          wc_match("WeOnlyDo-*", imp)))) {
        /*
         * These versions have the SSH-2 rekey bug.
         */
        s->remote_bugs |= BUG_SSH2_REKEY;
        bpp_logevent("We believe remote version has SSH-2 rekey bug");
    }

    if (conf_get_int(s->conf, CONF_sshbug_maxpkt2) == FORCE_ON ||
        (conf_get_int(s->conf, CONF_sshbug_maxpkt2) == AUTO &&
         (wc_match("1.36_sshlib GlobalSCAPE", imp) ||
          wc_match("1.36 sshlib: GlobalScape", imp)))) {
        /*
         * This version ignores our makpkt and needs to be throttled.
         */
        s->remote_bugs |= BUG_SSH2_MAXPKT;
        bpp_logevent("We believe remote version ignores SSH-2 "
                     "maximum packet size");
    }

    if (conf_get_int(s->conf, CONF_sshbug_ignore2) == FORCE_ON) {
        /*
         * Servers that don't support SSH2_MSG_IGNORE. Currently,
         * none detected automatically.
         */
        s->remote_bugs |= BUG_CHOKES_ON_SSH2_IGNORE;
        bpp_logevent("We believe remote version has SSH-2 ignore bug");
    }

    if (conf_get_int(s->conf, CONF_sshbug_oldgex2) == FORCE_ON ||
        (conf_get_int(s->conf, CONF_sshbug_oldgex2) == AUTO &&
         (wc_match("OpenSSH_2.[235]*", imp)))) {
        /*
         * These versions only support the original (pre-RFC4419)
         * SSH-2 GEX request, and disconnect with a protocol error if
         * we use the newer version.
         */
        s->remote_bugs |= BUG_SSH2_OLDGEX;
        bpp_logevent("We believe remote version has outdated SSH-2 GEX");
    }

    if (conf_get_int(s->conf, CONF_sshbug_winadj) == FORCE_ON) {
        /*
         * Servers that don't support our winadj request for one
         * reason or another. Currently, none detected automatically.
         */
        s->remote_bugs |= BUG_CHOKES_ON_WINADJ;
        bpp_logevent("We believe remote version has winadj bug");
    }

    if (conf_get_int(s->conf, CONF_sshbug_chanreq) == FORCE_ON ||
        (conf_get_int(s->conf, CONF_sshbug_chanreq) == AUTO &&
         (wc_match("OpenSSH_[2-5].*", imp) ||
          wc_match("OpenSSH_6.[0-6]*", imp) ||
          wc_match("dropbear_0.[2-4][0-9]*", imp) ||
          wc_match("dropbear_0.5[01]*", imp)))) {
        /*
         * These versions have the SSH-2 channel request bug.
         * OpenSSH 6.7 and above do not:
         * https://bugzilla.mindrot.org/show_bug.cgi?id=1818
         * dropbear_0.52 and above do not:
         * https://secure.ucc.asn.au/hg/dropbear/rev/cd02449b709c
         */
        s->remote_bugs |= BUG_SENDS_LATE_REQUEST_REPLY;
        bpp_logevent("We believe remote version has SSH-2 "
                     "channel request bug");
    }
}

const char *ssh_verstring_get_remote(BinaryPacketProtocol *bpp)
{
    struct ssh_verstring_state *s =
        container_of(bpp, struct ssh_verstring_state, bpp);
    return s->vstring->s;
}

const char *ssh_verstring_get_local(BinaryPacketProtocol *bpp)
{
    struct ssh_verstring_state *s =
        container_of(bpp, struct ssh_verstring_state, bpp);
    return s->our_vstring;
}

int ssh_verstring_get_bugs(BinaryPacketProtocol *bpp)
{
    struct ssh_verstring_state *s =
        container_of(bpp, struct ssh_verstring_state, bpp);
    return s->remote_bugs;
}

static void ssh_verstring_queue_disconnect(BinaryPacketProtocol *bpp,
                                           const char *msg, int category)
{
    /* No way to send disconnect messages at this stage of the protocol! */
}
