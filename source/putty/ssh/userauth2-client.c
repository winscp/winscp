/*
 * Packet protocol layer for the client side of the SSH-2 userauth
 * protocol (RFC 4252).
 */

#include <assert.h>

#include "putty.h"
#include "ssh.h"
#include "bpp.h"
#include "ppl.h"
#include "sshcr.h"

#ifndef NO_GSSAPI
#include "gssc.h"
#include "gss.h"
#endif

#define BANNER_LIMIT 131072

typedef struct agent_key {
    strbuf *blob, *comment;
    ptrlen algorithm;
} agent_key;

struct ssh2_userauth_state {
    int crState;

    PacketProtocolLayer *transport_layer, *successor_layer;
    Filename *keyfile, *detached_cert_file;
    bool show_banner, tryagent, notrivialauth, change_username;
    char *hostname, *fullhostname;
    int port;
    char *default_username;
    bool try_ki_auth, try_gssapi_auth, try_gssapi_kex_auth, gssapi_fwd;
    char *loghost; // WINSCP
    bool change_password; // WINSCP

    ptrlen session_id;
    enum {
        AUTH_TYPE_NONE,
        AUTH_TYPE_PUBLICKEY,
        AUTH_TYPE_PUBLICKEY_OFFER_LOUD,
        AUTH_TYPE_PUBLICKEY_OFFER_QUIET,
        AUTH_TYPE_PASSWORD,
        AUTH_TYPE_GSSAPI,      /* always QUIET */
        AUTH_TYPE_KEYBOARD_INTERACTIVE,
        AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET
    } type;
    bool need_pw, can_pubkey, can_passwd, can_keyb_inter;
    SeatPromptResult spr;
    bool tried_pubkey_config, done_agent;
    struct ssh_connection_shared_gss_state *shgss;
#ifndef NO_GSSAPI
    bool can_gssapi;
    bool can_gssapi_keyex_auth;
    bool tried_gssapi;
    bool tried_gssapi_keyex_auth;
    time_t gss_cred_expiry;
    Ssh_gss_buf gss_buf;
    Ssh_gss_buf gss_rcvtok, gss_sndtok;
    Ssh_gss_stat gss_stat;
#endif
    bool suppress_wait_for_response_packet;
    strbuf *last_methods_string;
    bool kbd_inter_refused;
    prompts_t *cur_prompt;
    uint32_t num_prompts;
    const char *username;
    char *locally_allocated_username;
    char *password;
    bool got_username;
    strbuf *publickey_blob, *detached_cert_blob, *cert_pubkey_diagnosed;
    bool privatekey_available, privatekey_encrypted;
    char *publickey_algorithm;
    char *publickey_comment;
    void *agent_response_to_free;
    ptrlen agent_response;
    BinarySource asrc[1];          /* for reading SSH agent response */
    size_t agent_keys_len;
    agent_key *agent_keys;
    size_t agent_key_index, agent_key_limit;
    ptrlen agent_keyalg;
    unsigned signflags;
    int len;
    PktOut *pktout;
    bool is_trivial_auth;

    agent_pending_query *auth_agent_query;
    bufchain banner;
    bufchain_sink banner_bs;
    StripCtrlChars *banner_scc;
    bool banner_scc_initialised;

    char *authplugin_cmd;
    Socket *authplugin;
    uint32_t authplugin_version;
    Plug authplugin_plug;
    bufchain authplugin_bc;
    strbuf *authplugin_incoming_msg;
    size_t authplugin_backlog;
    bool authplugin_eof;
    bool authplugin_ki_active;

    StripCtrlChars *ki_scc;
    bool ki_scc_initialised;
    bool ki_printed_header;

    Seat *seat; // WINSCP

    PacketProtocolLayer ppl;
};

static void ssh2_userauth_free(PacketProtocolLayer *);
static void ssh2_userauth_process_queue(PacketProtocolLayer *);
static bool ssh2_userauth_get_specials(
    PacketProtocolLayer *ppl, add_special_fn_t add_special, void *ctx);
static void ssh2_userauth_special_cmd(PacketProtocolLayer *ppl,
                                      SessionSpecialCode code, int arg);
static void ssh2_userauth_reconfigure(PacketProtocolLayer *ppl, Conf *conf);

static void ssh2_userauth_agent_query(struct ssh2_userauth_state *, strbuf *);
static void ssh2_userauth_agent_callback(void *, void *, int);
static void ssh2_userauth_add_sigblob(
    struct ssh2_userauth_state *s, PktOut *pkt, ptrlen pkblob, ptrlen sigblob);
static void ssh2_userauth_add_alg_and_publickey(
    struct ssh2_userauth_state *s, PktOut *pkt, ptrlen alg, ptrlen pkblob);
static void ssh2_userauth_add_session_id(
    struct ssh2_userauth_state *s, strbuf *sigdata);
#ifndef NO_GSSAPI
static PktOut *ssh2_userauth_gss_packet(
    struct ssh2_userauth_state *s, const char *authtype);
#endif
static bool ssh2_userauth_ki_setup_prompts(
    struct ssh2_userauth_state *s, BinarySource *src, bool plugin);
static bool ssh2_userauth_ki_run_prompts(struct ssh2_userauth_state *s);
static void ssh2_userauth_ki_write_responses(
    struct ssh2_userauth_state *s, BinarySink *bs);
static void ssh2_userauth_final_output(PacketProtocolLayer *ppl);

static void ssh2_userauth_print_banner(struct ssh2_userauth_state *s);
static ptrlen workaround_rsa_sha2_cert_userauth(
    struct ssh2_userauth_state *s, ptrlen id);

static const PacketProtocolLayerVtable ssh2_userauth_vtable = {
    // WINSCP
    /*.free =*/ ssh2_userauth_free,
    /*.process_queue =*/ ssh2_userauth_process_queue,
    /*.get_specials =*/ ssh2_userauth_get_specials,
    /*.special_cmd =*/ ssh2_userauth_special_cmd,
    /*.reconfigure =*/ ssh2_userauth_reconfigure,
    /*.queued_data_size =*/ ssh_ppl_default_queued_data_size,
    /*.final_output =*/ ssh2_userauth_final_output,
    /*.name =*/ "ssh-userauth",
    NULL, // WINSCP
};

PacketProtocolLayer *ssh2_userauth_new(
    PacketProtocolLayer *successor_layer,
    const char *hostname, int port, const char *fullhostname,
    Filename *keyfile, Filename *detached_cert_file,
    bool show_banner, bool tryagent, bool notrivialauth,
    const char *default_username, bool change_username,
    bool try_ki_auth, bool try_gssapi_auth, bool try_gssapi_kex_auth,
    bool gssapi_fwd, struct ssh_connection_shared_gss_state *shgss,
    const char *authplugin_cmd,
    const char * loghost, bool change_password, Seat *seat) // WINSCP
{
    struct ssh2_userauth_state *s = snew(struct ssh2_userauth_state);
    memset(s, 0, sizeof(*s));
    s->seat = seat;
    s->ppl.vt = &ssh2_userauth_vtable;

    s->successor_layer = successor_layer;
    s->hostname = dupstr(hostname);
    s->port = port;
    s->fullhostname = dupstr(fullhostname);
    s->keyfile = filename_copy(keyfile);
    s->detached_cert_file = filename_copy(detached_cert_file);
    s->show_banner = show_banner;
    s->tryagent = tryagent;
    s->notrivialauth = notrivialauth;
    s->default_username = dupstr(default_username);
    s->change_username = change_username;
    s->try_ki_auth = try_ki_auth;
    s->try_gssapi_auth = try_gssapi_auth;
    s->try_gssapi_kex_auth = try_gssapi_kex_auth;
    s->gssapi_fwd = gssapi_fwd;
    s->shgss = shgss;
    s->last_methods_string = strbuf_new();
    s->is_trivial_auth = true;
    s->loghost = dupstr(loghost); // WINSCP
    s->change_password = change_password;
    bufchain_init(&s->banner);
    bufchain_sink_init(&s->banner_bs, &s->banner);
    s->authplugin_cmd = dupstr(authplugin_cmd);
    bufchain_init(&s->authplugin_bc);

    return &s->ppl;
}

void ssh2_userauth_set_transport_layer(PacketProtocolLayer *userauth,
                                       PacketProtocolLayer *transport)
{
    struct ssh2_userauth_state *s =
        container_of(userauth, struct ssh2_userauth_state, ppl);
    s->transport_layer = transport;
}

static void ssh2_userauth_free(PacketProtocolLayer *ppl)
{
    struct ssh2_userauth_state *s =
        container_of(ppl, struct ssh2_userauth_state, ppl);
    bufchain_clear(&s->banner);

    if (s->successor_layer)
        ssh_ppl_free(s->successor_layer);

    if (s->agent_keys) {
        size_t i; // WINSCP
        for (i = 0; i < s->agent_keys_len; i++) {
            strbuf_free(s->agent_keys[i].blob);
            strbuf_free(s->agent_keys[i].comment);
        }
        sfree(s->agent_keys);
    }
    sfree(s->agent_response_to_free);
    if (s->auth_agent_query)
        agent_cancel_query(s->auth_agent_query);
    filename_free(s->keyfile);
    filename_free(s->detached_cert_file);
    sfree(s->default_username);
    sfree(s->locally_allocated_username);
    sfree(s->hostname);
    sfree(s->fullhostname);
    if (s->cur_prompt)
        free_prompts(s->cur_prompt);
    sfree(s->publickey_comment);
    sfree(s->publickey_algorithm);
    if (s->publickey_blob)
        strbuf_free(s->publickey_blob);
    if (s->detached_cert_blob)
        strbuf_free(s->detached_cert_blob);
    if (s->cert_pubkey_diagnosed)
        strbuf_free(s->cert_pubkey_diagnosed);
    strbuf_free(s->last_methods_string);
    sfree(s->loghost);
    if (s->banner_scc)
        stripctrl_free(s->banner_scc);
    if (s->ki_scc)
        stripctrl_free(s->ki_scc);
    sfree(s->authplugin_cmd);
    if (s->authplugin)
        sk_close(s->authplugin);
    bufchain_clear(&s->authplugin_bc);
    if (s->authplugin_incoming_msg)
        strbuf_free(s->authplugin_incoming_msg);
    sfree(s);
}

static void ssh2_userauth_handle_banner_packet(struct ssh2_userauth_state *s,
                                               PktIn *pktin)
{
    if (!s->show_banner)
        return;

    { // WINSCP
    ptrlen string = get_string(pktin);
    if (string.len > BANNER_LIMIT - bufchain_size(&s->banner))
        string.len = BANNER_LIMIT - bufchain_size(&s->banner);

    if (!s->banner_scc_initialised) {
        s->banner_scc = seat_stripctrl_new(
            s->ppl.seat, BinarySink_UPCAST(&s->banner_bs), SIC_BANNER);
        if (s->banner_scc)
            stripctrl_enable_line_limiting(s->banner_scc);
        s->banner_scc_initialised = true;
    }

    if (s->banner_scc)
        put_datapl(s->banner_scc, string);
    else
        put_datapl(&s->banner_bs, string);
    } // WINSCP
}

static void ssh2_userauth_filter_queue(struct ssh2_userauth_state *s)
{
    PktIn *pktin;

    while ((pktin = pq_peek(s->ppl.in_pq)) != NULL) {
        switch (pktin->type) {
          case SSH2_MSG_USERAUTH_BANNER:
            ssh2_userauth_handle_banner_packet(s, pktin);
            pq_pop(s->ppl.in_pq);
            break;

          default:
            return;
        }
    }
}

static PktIn *ssh2_userauth_pop(struct ssh2_userauth_state *s)
{
    ssh2_userauth_filter_queue(s);
    return pq_pop(s->ppl.in_pq);
}

static bool ssh2_userauth_signflags(struct ssh2_userauth_state *s,
                                    unsigned *signflags, const char **algname)
{
    *signflags = 0;                    /* default */

    { // WINSCP
    const ssh_keyalg *alg = find_pubkey_alg(*algname);
    if (!alg)
        return false;          /* we don't know how to upgrade this */

    { // WINSCP
    unsigned supported_flags = ssh_keyalg_supported_flags(alg);

    if (s->ppl.bpp->ext_info_rsa_sha512_ok &&
        (supported_flags & SSH_AGENT_RSA_SHA2_512)) {
        *signflags = SSH_AGENT_RSA_SHA2_512;
    } else if (s->ppl.bpp->ext_info_rsa_sha256_ok &&
               (supported_flags & SSH_AGENT_RSA_SHA2_256)) {
        *signflags = SSH_AGENT_RSA_SHA2_256;
    } else {
        return false;
    }

    *algname = ssh_keyalg_alternate_ssh_id(alg, *signflags);
    return true;
    } // WINSCP
    } // WINSCP
}

static void authplugin_plug_log(Plug *plug, Socket *sock, PlugLogType type,
                                SockAddr *addr, int port,
                                const char *err_msg, int err_code)
{
    struct ssh2_userauth_state *s = container_of(
        plug, struct ssh2_userauth_state, authplugin_plug);
    PacketProtocolLayer *ppl = &s->ppl; /* for ppl_logevent */

    if (type == PLUGLOG_PROXY_MSG)
        ppl_logevent("%s", err_msg);
}

static void authplugin_plug_closing(
    Plug *plug, PlugCloseType type, const char *error_msg)
{
    struct ssh2_userauth_state *s = container_of(
        plug, struct ssh2_userauth_state, authplugin_plug);
    s->authplugin_eof = true;
    queue_idempotent_callback(&s->ppl.ic_process_queue);
}

static void authplugin_plug_receive(
    Plug *plug, int urgent, const char *data, size_t len)
{
    struct ssh2_userauth_state *s = container_of(
        plug, struct ssh2_userauth_state, authplugin_plug);
    bufchain_add(&s->authplugin_bc, data, len);
    queue_idempotent_callback(&s->ppl.ic_process_queue);
}

static void authplugin_plug_sent(Plug *plug, size_t bufsize)
{
    struct ssh2_userauth_state *s = container_of(
        plug, struct ssh2_userauth_state, authplugin_plug);
    s->authplugin_backlog = bufsize;
    queue_idempotent_callback(&s->ppl.ic_process_queue);
}

static const PlugVtable authplugin_plugvt = {
    /*.log =*/ authplugin_plug_log,
    /*.closing =*/ authplugin_plug_closing,
    /*.receive =*/ authplugin_plug_receive,
    /*.sent =*/ authplugin_plug_sent,
    NULL, // WINSCP
};

static strbuf *authplugin_newmsg(uint8_t type)
{
    strbuf *amsg = strbuf_new_nm();
    put_uint32(amsg, 0);               /* fill in later */
    put_byte(amsg, type);
    return amsg;
}

static void authplugin_send_free(struct ssh2_userauth_state *s, strbuf *amsg)
{
    PUT_32BIT_MSB_FIRST(amsg->u, amsg->len - 4);
    assert(s->authplugin);
    s->authplugin_backlog = sk_write(s->authplugin, amsg->u, amsg->len);
    strbuf_free(amsg);
}

static bool authplugin_expect_msg(struct ssh2_userauth_state *s,
                                  unsigned *type, BinarySource *src)
{
    if (s->authplugin_eof) {
        *type = PLUGIN_EOF;
        return true;
    }
    { // WINSCP
    uint8_t len[4];
    if (!bufchain_try_fetch(&s->authplugin_bc, len, 4))
        return false;
    { // WINSCP
    size_t size = GET_32BIT_MSB_FIRST(len);
    if (bufchain_size(&s->authplugin_bc) - 4 < size)
        return false;
    if (s->authplugin_incoming_msg) {
        strbuf_clear(s->authplugin_incoming_msg);
    } else {
        s->authplugin_incoming_msg = strbuf_new_nm();
    }
    bufchain_consume(&s->authplugin_bc, 4); /* eat length field */
    bufchain_fetch_consume(
        &s->authplugin_bc, strbuf_append(s->authplugin_incoming_msg, size),
        size);
    BinarySource_BARE_INIT_PL(
        src, ptrlen_from_strbuf(s->authplugin_incoming_msg));
    *type = get_byte(src);
    if (get_err(src))
        *type = PLUGIN_NOTYPE;
    return true;
    } // WINSCP
    } // WINSCP
}

static void authplugin_bad_packet(struct ssh2_userauth_state *s,
                                  unsigned type, const char *fmt, ...)
{
    strbuf *msg = strbuf_new();
    switch (type) {
      case PLUGIN_EOF:
        put_dataz(msg, "Unexpected end of file from auth helper plugin");
        break;
      case PLUGIN_NOTYPE:
        put_dataz(msg, "Received malformed packet from auth helper plugin "
                  "(too short to have a type code)");
        break;
      default:
        put_fmt(msg, "Received unknown message type %u "
                "from auth helper plugin", type);
        break;

      #define CASEDECL(name, value)                                     \
      case name:                                                        \
        put_fmt(msg, "Received unexpected %s message from auth helper " \
                "plugin", #name);                                       \
        break;
        AUTHPLUGIN_MSG_NAMES(CASEDECL);
      #undef CASEDECL
    }
    if (fmt) {
        put_dataz(msg, " (");
        { // WINSCP
        va_list ap;
        va_start(ap, fmt);
        put_fmt(msg, fmt, ap);
        va_end(ap);
        put_dataz(msg, ")");
        } // WINSCP
    }
    ssh_sw_abort(s->ppl.ssh, "%s", msg->s);
    strbuf_free(msg);
}

static void ssh2_userauth_process_queue(PacketProtocolLayer *ppl)
{
    struct ssh2_userauth_state *s =
        container_of(ppl, struct ssh2_userauth_state, ppl);
    PktIn *pktin;

    ssh2_userauth_filter_queue(s);     /* no matter why we were called */

    crBegin(s->crState);

#ifndef NO_GSSAPI
    s->tried_gssapi = false;
    s->tried_gssapi_keyex_auth = false;
#endif

    /*
     * Misc one-time setup for authentication.
     */
    s->publickey_blob = NULL;
    s->session_id = ssh2_transport_get_session_id(s->transport_layer);

    /*
     * Load the public half of any configured public key file for
     * later use.
     */
    if (!filename_is_null(s->keyfile)) {
        int keytype;
        ppl_logevent(WINSCP_BOM "Reading key file \"%s\"",
                     filename_to_str(s->keyfile));
        keytype = key_type(s->keyfile);
        if (keytype == SSH_KEYTYPE_SSH2 ||
            keytype == SSH_KEYTYPE_SSH2_PUBLIC_RFC4716 ||
            keytype == SSH_KEYTYPE_SSH2_PUBLIC_OPENSSH) {
            const char *error;
            s->publickey_blob = strbuf_new();
            if (ppk_loadpub_f(s->keyfile, &s->publickey_algorithm,
                              BinarySink_UPCAST(s->publickey_blob),
                              &s->publickey_comment, &error)) {
                s->privatekey_available = (keytype == SSH_KEYTYPE_SSH2);
                if (!s->privatekey_available)
                    ppl_logevent("Key file contains public key only");
                s->privatekey_encrypted = ppk_encrypted_f(s->keyfile, NULL);
            } else {
                ppl_logevent("Unable to load key (%s)", error);
                ppl_printf(WINSCP_BOM "Unable to load key file \"%s\" (%s)\r\n",
                           filename_to_str(s->keyfile), error);
                strbuf_free(s->publickey_blob);
                s->publickey_blob = NULL;
            }
        } else {
            ppl_logevent("Unable to use this key file (%s)",
                         key_type_to_str(keytype));
            ppl_printf(WINSCP_BOM "Unable to use key file \"%s\" (%s)\r\n",
                       filename_to_str(s->keyfile),
                       key_type_to_str(keytype));
            s->publickey_blob = NULL;
        }
    }

    /*
     * If the user provided a detached certificate file, load that.
     */
    if (!filename_is_null(s->detached_cert_file)) {
        char *cert_error = NULL;
        strbuf *cert_blob = strbuf_new();
        char *algname = NULL;
        char *comment = NULL;

        ppl_logevent(WINSCP_BOM "Reading certificate file \"%s\"",
                     filename_to_str(s->detached_cert_file));
        { // WINSCP
        int keytype = key_type(s->detached_cert_file);
        if (!(keytype == SSH_KEYTYPE_SSH2_PUBLIC_RFC4716 ||
              keytype == SSH_KEYTYPE_SSH2_PUBLIC_OPENSSH)) {
            cert_error = dupstr(key_type_to_str(keytype));
            goto cert_load_done;
        }

        { // WINSCP
        const char *error;
        bool success = ppk_loadpub_f(
            s->detached_cert_file, &algname,
            BinarySink_UPCAST(cert_blob), &comment, &error);

        if (!success) {
            cert_error = dupstr(error);
            goto cert_load_done;
        }

        { // WINSCP
        const ssh_keyalg *certalg = find_pubkey_alg(algname);
        if (!certalg) {
            cert_error = dupprintf(
                "unrecognised certificate type '%s'", algname);
            goto cert_load_done;
        }

        if (!certalg->is_certificate) {
            cert_error = dupprintf(
                "key type '%s' is not a certificate", certalg->ssh_id);
            goto cert_load_done;
        }

        /* OK, store the certificate blob to substitute for the
         * public blob in all publickey auth packets. */
        if (s->detached_cert_blob)
            strbuf_free(s->detached_cert_blob);
        s->detached_cert_blob = cert_blob;
        cert_blob = NULL;      /* prevent free */

      cert_load_done:
        if (cert_error) {
            ppl_logevent("Unable to use this certificate file (%s)",
                         cert_error);
            ppl_printf(
                WINSCP_BOM "Unable to use certificate file \"%s\" (%s)\r\n",
                filename_to_str(s->detached_cert_file), cert_error);
            sfree(cert_error);
        }

        if (cert_blob)
            strbuf_free(cert_blob);
        sfree(algname);
        sfree(comment);
        } // WINSCP
        } // WINSCP
        } // WINSCP
    }

    /*
     * Find out about any keys Pageant has (but if there's a public
     * key configured, filter out all others).
     */
    if (s->tryagent && agent_exists()) {
        ppl_logevent("Pageant is running. Requesting keys.");

        /* Request the keys held by the agent. */
        {
            strbuf *request = strbuf_new_for_agent_query();
            put_byte(request, SSH2_AGENTC_REQUEST_IDENTITIES);
            ssh2_userauth_agent_query(s, request);
            strbuf_free(request);
            crWaitUntilV(!s->auth_agent_query);
        }
        BinarySource_BARE_INIT_PL(s->asrc, s->agent_response);

        get_uint32(s->asrc); /* skip length field */
        if (get_byte(s->asrc) == SSH2_AGENT_IDENTITIES_ANSWER) {
            size_t nkeys = get_uint32(s->asrc);
            size_t origpos = s->asrc->pos;

            /*
             * Check that the agent response is well formed.
             */
            { // WINSCP
            size_t i; // WINSCP
            for (i = 0; i < nkeys; i++) {
                get_string(s->asrc);   /* blob */
                get_string(s->asrc);   /* comment */
                if (get_err(s->asrc)) {
                    ppl_logevent("Pageant's response was truncated");
                    goto done_agent_query;
                }
            }
            } // WINSCP

            /*
             * Copy the list of public-key blobs out of the Pageant
             * response.
             */
            BinarySource_REWIND_TO(s->asrc, origpos);
            s->agent_keys_len = nkeys;
            s->agent_keys = snewn(s->agent_keys_len, agent_key);
            { // WINSCP
            size_t i; // WINSCP
            for (i = 0; i < nkeys; i++) {
                s->agent_keys[i].blob = strbuf_dup(get_string(s->asrc));
                s->agent_keys[i].comment = strbuf_dup(get_string(s->asrc));

                { // WINSCP
                /* Also, extract the algorithm string from the start
                 * of the public-key blob. */
                s->agent_keys[i].algorithm = pubkey_blob_to_alg_name(
                    ptrlen_from_strbuf(s->agent_keys[i].blob));
                } // WINSCP
            }
            } // WINSCP

            ppl_logevent("Pageant has %"SIZEu" SSH-2 keys", nkeys);

            if (s->publickey_blob) {
                /*
                 * If we've been given a specific public key blob,
                 * filter the list of keys to try from the agent down
                 * to only that one, or none if it's not there.
                 */
                ptrlen our_blob = ptrlen_from_strbuf(s->publickey_blob);
                size_t i;

                for (i = 0; i < nkeys; i++) {
                    if (ptrlen_eq_ptrlen(our_blob, ptrlen_from_strbuf(
                                             s->agent_keys[i].blob)))
                        break;
                }

                if (i < nkeys) {
                    ppl_logevent("Pageant key #%"SIZEu" matches "
                                 "configured key file", i);
                    s->agent_key_index = i;
                    s->agent_key_limit = i+1;
                } else {
                    ppl_logevent("Configured key file not in Pageant");
                    s->agent_key_index = 0;
                    s->agent_key_limit = 0;
                }
            } else {
                /*
                 * Otherwise, try them all.
                 */
                s->agent_key_index = 0;
                s->agent_key_limit = nkeys;
            }
        } else {
            ppl_logevent("Failed to get reply from Pageant");
        }
      done_agent_query:;
    }

    s->got_username = false;

    if (*s->authplugin_cmd) {
        s->authplugin_plug.vt = &authplugin_plugvt;
        s->authplugin = platform_start_subprocess(
            s->authplugin_cmd, &s->authplugin_plug, "plugin");
        ppl_logevent("Started authentication plugin: %s", s->authplugin_cmd);
    }

    if (s->authplugin) {
        strbuf *amsg = authplugin_newmsg(PLUGIN_INIT);
        put_uint32(amsg, PLUGIN_PROTOCOL_MAX_VERSION);
        put_stringz(amsg, s->hostname);
        put_uint32(amsg, s->port);
        put_stringz(amsg, s->username ? s->username : "");
        authplugin_send_free(s, amsg);

        { // WINSCP
        BinarySource src[1];
        unsigned type;
        crMaybeWaitUntilV(authplugin_expect_msg(s, &type, src));
        switch (type) {
          case PLUGIN_INIT_RESPONSE: {
            s->authplugin_version = get_uint32(src);
            { // WINSCP
            ptrlen username = get_string(src);
            if (get_err(src)) {
                ssh_sw_abort(s->ppl.ssh, "Received malformed "
                             "PLUGIN_INIT_RESPONSE from auth helper plugin");
                return;
            }
            if (s->authplugin_version > PLUGIN_PROTOCOL_MAX_VERSION) {
                ssh_sw_abort(s->ppl.ssh, "Auth helper plugin announced "
                             "unsupported version number %"PRIu32,
                             s->authplugin_version);
                return;
            }
            if (username.len) {
                sfree(s->default_username);
                s->default_username = mkstr(username);
                ppl_logevent("Authentication plugin set username '%s'",
                             s->default_username);
            }
            } // WINSCP
            break;
          }
          case PLUGIN_INIT_FAILURE: {
            ptrlen message = get_string(src);
            if (get_err(src)) {
                ssh_sw_abort(s->ppl.ssh, "Received malformed "
                             "PLUGIN_INIT_FAILURE from auth helper plugin");
                return;
            }
            /* This is a controlled error, so we need not completely
             * abandon the connection. Instead, inform the user, and
             * proceed as if the plugin was not present */
            ppl_printf("Authentication plugin failed to initialise:\r\n");
            seat_set_trust_status(s->ppl.seat, false);
            ppl_printf("%.*s\r\n", PTRLEN_PRINTF(message));
            seat_set_trust_status(s->ppl.seat, true);
            sk_close(s->authplugin);
            s->authplugin = NULL;
            break;
          }
          default:
            authplugin_bad_packet(s, type, "expected PLUGIN_INIT_RESPONSE or "
                                  "PLUGIN_INIT_FAILURE");
            return;
        }
        } // WINSCP
    }

    /*
     * We repeat this whole loop, including the username prompt,
     * until we manage a successful authentication. If the user
     * types the wrong _password_, they can be sent back to the
     * beginning to try another username, if this is configured on.
     * (If they specify a username in the config, they are never
     * asked, even if they do give a wrong password.)
     *
     * I think this best serves the needs of
     *
     *  - the people who have no configuration, no keys, and just
     *    want to try repeated (username,password) pairs until they
     *    type both correctly
     *
     *  - people who have keys and configuration but occasionally
     *    need to fall back to passwords
     *
     *  - people with a key held in Pageant, who might not have
     *    logged in to a particular machine before; so they want to
     *    type a username, and then _either_ their key will be
     *    accepted, _or_ they will type a password. If they mistype
     *    the username they will want to be able to get back and
     *    retype it!
     */
    while (1) {
        /*
         * Get a username.
         */
        if (s->got_username && !s->change_username) {
            /*
             * We got a username last time round this loop, and
             * with change_username turned off we don't try to get
             * it again.
             */
        } else if ((s->username = s->default_username) == NULL) {
            s->cur_prompt = ssh_ppl_new_prompts(&s->ppl);
            s->cur_prompt->utf8 = true;
            s->cur_prompt->to_server = true;
            s->cur_prompt->from_server = false;
            s->cur_prompt->name = dupstr("SSH login name");
            add_prompt(s->cur_prompt, dupstr("login as: "), true);
            s->spr = seat_get_userpass_input(
                ppl_get_iseat(&s->ppl), s->cur_prompt);
            while (s->spr.kind == SPRK_INCOMPLETE) {
                crReturnV;
                s->spr = seat_get_userpass_input(
                    ppl_get_iseat(&s->ppl), s->cur_prompt);
            }
            if (spr_is_abort(s->spr)) {
                /*
                 * seat_get_userpass_input() failed to get a username.
                 * Terminate.
                 */
                free_prompts(s->cur_prompt);
                s->cur_prompt = NULL;
                ssh_spr_close(s->ppl.ssh, s->spr, "username prompt");
                return;
            }
            sfree(s->locally_allocated_username); /* for change_username */
            s->username = s->locally_allocated_username =
                prompt_get_result(s->cur_prompt->prompts[0]);
            free_prompts(s->cur_prompt);
            s->cur_prompt = NULL;
        } else {
            if (seat_verbose(s->ppl.seat) || seat_interactive(s->ppl.seat))
                ppl_printf(WINSCP_BOM "Using username \"%s\".\r\n", s->username);
        }
        s->got_username = true;

        /*
         * Send an authentication request using method "none": (a)
         * just in case it succeeds, and (b) so that we know what
         * authentication methods we can usefully try next.
         */
        s->ppl.bpp->pls->actx = SSH2_PKTCTX_NOAUTH;

        s->pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH2_MSG_USERAUTH_REQUEST);
        put_stringz(s->pktout, s->username);
        put_stringz(s->pktout, s->successor_layer->vt->name);
        put_stringz(s->pktout, "none");    /* method */
        pq_push(s->ppl.out_pq, s->pktout);
        s->type = AUTH_TYPE_NONE;

        s->tried_pubkey_config = false;
        s->kbd_inter_refused = false;
        s->done_agent = false;

        while (1) {
            /*
             * Wait for the result of the last authentication request,
             * unless the request terminated for some reason on our
             * own side.
             */
            if (s->suppress_wait_for_response_packet) {
                pktin = NULL;
                s->suppress_wait_for_response_packet = false;
            } else {
                crMaybeWaitUntilV((pktin = ssh2_userauth_pop(s)) != NULL);
            }

            /*
             * Now is a convenient point to spew any banner material
             * that we've accumulated. (This should ensure that when
             * we exit the auth loop, we haven't any left to deal
             * with.)
             *
             * Don't show the banner if we're operating in non-verbose
             * non-interactive mode. (It's probably a script, which
             * means nobody will read the banner _anyway_, and
             * moreover the printing of the banner will screw up
             * processing on the output of (say) plink.)
             *
             * The banner data has been sanitised already by this
             * point, but we still need to precede and follow it with
             * anti-spoofing header lines.
             */
            ssh2_userauth_print_banner(s);
                { // WINSCP
                } // WINSCP

            if (pktin && pktin->type == SSH2_MSG_USERAUTH_SUCCESS) {
                ppl_logevent("Access granted");
                goto userauth_success;
            }

            if (pktin && pktin->type != SSH2_MSG_USERAUTH_FAILURE &&
                s->type != AUTH_TYPE_GSSAPI) {
                ssh_proto_error(s->ppl.ssh, "Received unexpected packet "
                                "in response to authentication request, "
                                "type %d (%s)", pktin->type,
                                ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                              s->ppl.bpp->pls->actx,
                                              pktin->type));
                return;
            }

            /*
             * OK, we're now sitting on a USERAUTH_FAILURE message, so
             * we can look at the string in it and know what we can
             * helpfully try next.
             */
            if (pktin && pktin->type == SSH2_MSG_USERAUTH_FAILURE) {
                ptrlen methods = get_string(pktin);
                bool partial_success = get_bool(pktin);

                if (!partial_success) {
                    /*
                     * We have received an unequivocal Access
                     * Denied. This can translate to a variety of
                     * messages, or no message at all.
                     *
                     * For forms of authentication which are attempted
                     * implicitly, by which I mean without printing
                     * anything in the window indicating that we're
                     * trying them, we should never print 'Access
                     * denied'.
                     *
                     * If we do print a message saying that we're
                     * attempting some kind of authentication, it's OK
                     * to print a followup message saying it failed -
                     * but the message may sometimes be more specific
                     * than simply 'Access denied'.
                     *
                     * Additionally, if we'd just tried password
                     * authentication, we should break out of this
                     * whole loop so as to go back to the username
                     * prompt (iff we're configured to allow
                     * username change attempts).
                     */
                    if (s->type == AUTH_TYPE_NONE) {
                        /* do nothing */
                    } else if (s->type == AUTH_TYPE_PUBLICKEY_OFFER_LOUD ||
                               s->type == AUTH_TYPE_PUBLICKEY_OFFER_QUIET) {
                        if (s->type == AUTH_TYPE_PUBLICKEY_OFFER_LOUD)
                            ppl_printf("Server refused our key\r\n");
                        ppl_logevent("Server refused our key");
                    } else if (s->type == AUTH_TYPE_PUBLICKEY) {
                        /* This _shouldn't_ happen except by a
                         * protocol bug causing client and server to
                         * disagree on what is a correct signature. */
                        ppl_printf("Server refused public-key signature"
                                   " despite accepting key!\r\n");
                        ppl_logevent("Server refused public-key signature"
                                     " despite accepting key!");
                    } else if (s->type==AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET) {
                        /* quiet, so no ppl_printf */
                        ppl_logevent("Server refused keyboard-interactive "
                                     "authentication");
                    } else if (s->type==AUTH_TYPE_GSSAPI) {
                        /* always quiet, so no ppl_printf */
                        /* also, the code down in the GSSAPI block has
                         * already logged this in the Event Log */
                    } else if (s->type == AUTH_TYPE_KEYBOARD_INTERACTIVE) {
                        ppl_logevent("Keyboard-interactive authentication "
                                     "failed");
                        ppl_printf("Access denied\r\n");
                    } else {
                        assert(s->type == AUTH_TYPE_PASSWORD);
                        ppl_logevent("Password authentication failed");
                        ppl_printf("Access denied\r\n");

                        if (s->change_username) {
                            /* XXX perhaps we should allow
                             * keyboard-interactive to do this too? */
                            goto try_new_username;
                        }
                    }
                } else {
                    ppl_printf("Further authentication required\r\n");
                    ppl_logevent("Further authentication required");
                }

                /*
                 * Save the methods string for use in error messages.
                 */
                strbuf_clear(s->last_methods_string);
                put_datapl(s->last_methods_string, methods);
#ifdef WINSCP
                ppl_logevent("Server offered these authentication methods: %s", s->last_methods_string->s);
#endif

                /*
                 * Scan it for method identifiers we know about.
                 */
                { // WINSCP
                bool srv_pubkey = false, srv_passwd = false;
                bool srv_keyb_inter = false;
#ifndef NO_GSSAPI
                bool srv_gssapi = false, srv_gssapi_keyex_auth = false;
#endif

                ptrlen method; // WINSCP
                for (; get_commasep_word(&methods, &method) ;) {
                    if (ptrlen_eq_string(method, "publickey"))
                        srv_pubkey = true;
                    else if (ptrlen_eq_string(method, "password"))
                        srv_passwd = true;
                    else if (ptrlen_eq_string(method, "keyboard-interactive"))
                        srv_keyb_inter = true;
#ifndef NO_GSSAPI
                    else if (ptrlen_eq_string(method, "gssapi-with-mic"))
                        srv_gssapi = true;
                    else if (ptrlen_eq_string(method, "gssapi-keyex"))
                        srv_gssapi_keyex_auth = true;
#endif
                }

                /*
                 * And combine those flags with our own configuration
                 * and context to set the main can_foo variables.
                 */
                s->can_pubkey = srv_pubkey;
                s->can_passwd = srv_passwd;
                s->can_keyb_inter = s->try_ki_auth && srv_keyb_inter;
#ifndef NO_GSSAPI
                s->can_gssapi = s->try_gssapi_auth && srv_gssapi &&
                    s->shgss->libs->nlibraries > 0;
                s->can_gssapi_keyex_auth = s->try_gssapi_kex_auth &&
                    srv_gssapi_keyex_auth &&
                    s->shgss->libs->nlibraries > 0 && s->shgss->ctx;
#endif
                } // WINSCP
            }

            s->ppl.bpp->pls->actx = SSH2_PKTCTX_NOAUTH;

#ifndef NO_GSSAPI
            if (s->can_gssapi_keyex_auth && !s->tried_gssapi_keyex_auth) {

                /* gssapi-keyex authentication */

                s->type = AUTH_TYPE_GSSAPI;
                s->tried_gssapi_keyex_auth = true;
                s->ppl.bpp->pls->actx = SSH2_PKTCTX_GSSAPI;

                if (s->shgss->lib->gsslogmsg)
                    ppl_logevent("%s", s->shgss->lib->gsslogmsg);

                ppl_logevent("Trying gssapi-keyex...");
                s->pktout = ssh2_userauth_gss_packet(s, "gssapi-keyex");
                pq_push(s->ppl.out_pq, s->pktout);
                s->shgss->lib->release_cred(s->shgss->lib, &s->shgss->ctx);
                s->shgss->ctx = NULL;

                continue;
            } else
#endif /* NO_GSSAPI */

            if (s->can_pubkey && !s->done_agent &&
                s->agent_key_index < s->agent_key_limit) {

                /*
                 * Attempt public-key authentication using a key from Pageant.
                 */
                s->agent_keyalg = s->agent_keys[s->agent_key_index].algorithm;
                { // WINSCP
                char *alg_tmp = mkstr(s->agent_keyalg);
                const char *newalg = alg_tmp;
                if (ssh2_userauth_signflags(s, &s->signflags, &newalg))
                    s->agent_keyalg = ptrlen_from_asciz(newalg);
                sfree(alg_tmp);

                s->ppl.bpp->pls->actx = SSH2_PKTCTX_PUBLICKEY;

                ppl_logevent("Trying Pageant key #%"SIZEu, s->agent_key_index);

                /* See if server will accept it */
                s->pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH2_MSG_USERAUTH_REQUEST);
                put_stringz(s->pktout, s->username);
                put_stringz(s->pktout, s->successor_layer->vt->name);
                put_stringz(s->pktout, "publickey");
                                                    /* method */
                put_bool(s->pktout, false); /* no signature included */
                ssh2_userauth_add_alg_and_publickey(
                    s, s->pktout, s->agent_keyalg, ptrlen_from_strbuf(
                        s->agent_keys[s->agent_key_index].blob));
                pq_push(s->ppl.out_pq, s->pktout);
                s->type = AUTH_TYPE_PUBLICKEY_OFFER_QUIET;

                crMaybeWaitUntilV((pktin = ssh2_userauth_pop(s)) != NULL);
                if (pktin->type != SSH2_MSG_USERAUTH_PK_OK) {

                    /* Offer of key refused, presumably via
                     * USERAUTH_FAILURE. Requeue for the next iteration. */
                    pq_push_front(s->ppl.in_pq, pktin);

                } else {
                    strbuf *agentreq, *sigdata;
                    ptrlen comment = ptrlen_from_strbuf(
                        s->agent_keys[s->agent_key_index].comment);

                    if (seat_verbose(s->ppl.seat))
                        ppl_printf("Authenticating with public key "
                                   "\"%.*s\" from agent\r\n",
                                   PTRLEN_PRINTF(comment));

                    /*
                     * Server is willing to accept the key.
                     * Construct a SIGN_REQUEST.
                     */
                    s->pktout = ssh_bpp_new_pktout(
                        s->ppl.bpp, SSH2_MSG_USERAUTH_REQUEST);
                    put_stringz(s->pktout, s->username);
                    put_stringz(s->pktout, s->successor_layer->vt->name);
                    put_stringz(s->pktout, "publickey");
                                                        /* method */
                    put_bool(s->pktout, true);  /* signature included */
                    ssh2_userauth_add_alg_and_publickey(
                        s, s->pktout, s->agent_keyalg, ptrlen_from_strbuf(
                            s->agent_keys[s->agent_key_index].blob));

                    /* Ask agent for signature. */
                    agentreq = strbuf_new_for_agent_query();
                    put_byte(agentreq, SSH2_AGENTC_SIGN_REQUEST);
                    put_stringpl(agentreq, ptrlen_from_strbuf(
                                     s->agent_keys[s->agent_key_index].blob));
                    /* Now the data to be signed... */
                    sigdata = strbuf_new();
                    ssh2_userauth_add_session_id(s, sigdata);
                    put_data(sigdata, s->pktout->data + 5,
                             s->pktout->length - 5);
                    put_stringsb(agentreq, sigdata);
                    /* And finally the flags word. */
                    put_uint32(agentreq, s->signflags);
                    ssh2_userauth_agent_query(s, agentreq);
                    strbuf_free(agentreq);
                    crWaitUntilV(!s->auth_agent_query);

                    if (s->agent_response.ptr) {
                        ptrlen sigblob;
                        BinarySource src[1];
                        BinarySource_BARE_INIT(src, s->agent_response.ptr,
                                               s->agent_response.len);
                        get_uint32(src); /* skip length field */
                        if (get_byte(src) == SSH2_AGENT_SIGN_RESPONSE &&
                            (sigblob = get_string(src), !get_err(src))) {
                            ppl_logevent("Sending Pageant's response");
                            ssh2_userauth_add_sigblob(
                                s, s->pktout,
                                ptrlen_from_strbuf(
                                    s->agent_keys[s->agent_key_index].blob),
                                sigblob);
                            pq_push(s->ppl.out_pq, s->pktout);
                            s->type = AUTH_TYPE_PUBLICKEY;
                            s->is_trivial_auth = false;
                        } else {
                            ppl_logevent("Pageant refused signing request");
                            ppl_printf("Pageant failed to "
                                       "provide a signature\r\n");
                            s->suppress_wait_for_response_packet = true;
                            ssh_free_pktout(s->pktout);
                        }
                    } else {
                        ppl_logevent("Pageant failed to respond to "
                                     "signing request");
                        ppl_printf("Pageant failed to "
                                   "respond to signing request\r\n");
                        s->suppress_wait_for_response_packet = true;
                        ssh_free_pktout(s->pktout);
                    }
                }

                /* Do we have any keys left to try? */
                if (++s->agent_key_index >= s->agent_key_limit)
                    s->done_agent = true;
                } // WINSCP

            } else if (s->can_pubkey && s->publickey_blob &&
                       s->privatekey_available && !s->tried_pubkey_config) {

                ssh2_userkey *key;   /* not live over crReturn */
                char *passphrase;           /* not live over crReturn */

                s->ppl.bpp->pls->actx = SSH2_PKTCTX_PUBLICKEY;

                s->tried_pubkey_config = true;

                /*
                 * Try the public key supplied in the configuration.
                 *
                 * First, try to upgrade its algorithm.
                 */
                { // WINSCP
                const char *newalg = s->publickey_algorithm;
                if (ssh2_userauth_signflags(s, &s->signflags, &newalg)) {
                    sfree(s->publickey_algorithm);
                    s->publickey_algorithm = dupstr(newalg);
                }

                /*
                 * Offer the public blob to see if the server is willing to
                 * accept it.
                 */
                s->pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH2_MSG_USERAUTH_REQUEST);
                put_stringz(s->pktout, s->username);
                put_stringz(s->pktout, s->successor_layer->vt->name);
                put_stringz(s->pktout, "publickey");    /* method */
                put_bool(s->pktout, false);
                                                /* no signature included */
                ssh2_userauth_add_alg_and_publickey(
                    s, s->pktout, ptrlen_from_asciz(s->publickey_algorithm),
                    ptrlen_from_strbuf(s->publickey_blob));
                pq_push(s->ppl.out_pq, s->pktout);
                ppl_logevent("Offered public key");

                crMaybeWaitUntilV((pktin = ssh2_userauth_pop(s)) != NULL);
                if (pktin->type != SSH2_MSG_USERAUTH_PK_OK) {
                    /* Key refused. Give up. */
                    pq_push_front(s->ppl.in_pq, pktin);
                    s->type = AUTH_TYPE_PUBLICKEY_OFFER_LOUD;
                    continue; /* process this new message */
                }
                ppl_logevent("Offer of public key accepted");

                /*
                 * Actually attempt a serious authentication using
                 * the key.
                 */
                if (seat_verbose(s->ppl.seat))
                    ppl_printf("Authenticating with public key \"%s\"\r\n",
                               s->publickey_comment);

                key = NULL;
                while (!key) {
                    const char *error;  /* not live over crReturn */
                    if (s->privatekey_encrypted) {
                        /*
                         * Get a passphrase from the user.
                         */
                        s->cur_prompt = ssh_ppl_new_prompts(&s->ppl);
                        s->cur_prompt->to_server = false;
                        s->cur_prompt->from_server = false;
                        s->cur_prompt->name = dupstr("SSH key passphrase");
                        add_prompt(s->cur_prompt,
                                   dupprintf("Passphrase for key \"%s\": ",
                                             s->publickey_comment),
                                   false);
                        s->spr = seat_get_userpass_input(
                            ppl_get_iseat(&s->ppl), s->cur_prompt);
                        while (s->spr.kind == SPRK_INCOMPLETE) {
                            crReturnV;
                            s->spr = seat_get_userpass_input(
                                ppl_get_iseat(&s->ppl), s->cur_prompt);
                        }
                        if (spr_is_abort(s->spr)) {
                            /* Failed to get a passphrase. Terminate. */
                            free_prompts(s->cur_prompt);
                            s->cur_prompt = NULL;
                            ssh_bpp_queue_disconnect(
                                s->ppl.bpp, "Unable to authenticate",
                                SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER);
                            ssh_spr_close(s->ppl.ssh, s->spr,
                                          "passphrase prompt");
                            return;
                        }
                        passphrase =
                            prompt_get_result(s->cur_prompt->prompts[0]);
                        free_prompts(s->cur_prompt);
                        s->cur_prompt = NULL;
                    } else {
                        passphrase = NULL; /* no passphrase needed */
                    }

                    /*
                     * Try decrypting the key.
                     */
                    key = ppk_load_f(s->keyfile, passphrase, &error);
                    if (passphrase) {
                        /* burn the evidence */
                        smemclr(passphrase, strlen(passphrase));
                        sfree(passphrase);
                    }
                    if (key == SSH2_WRONG_PASSPHRASE || key == NULL) {
                        if (passphrase &&
                            (key == SSH2_WRONG_PASSPHRASE)) {
                            ppl_printf("Wrong passphrase\r\n");
                            key = NULL;
                            /* and loop again */
                        } else {
                            ppl_printf("Unable to load private key (%s)\r\n",
                                       error);
                            key = NULL;
                            s->suppress_wait_for_response_packet = true;
                            break; /* try something else */
                        }
                    } else {
                        /* FIXME: if we ever support variable signature
                         * flags, this is somewhere they'll need to be
                         * put */
                        char *invalid = ssh_key_invalid(key->key, 0);
                        if (invalid) {
                            ppl_printf("Cannot use this private key (%s)\r\n",
                                       invalid);
                            ssh_key_free(key->key);
                            sfree(key->comment);
                            sfree(key);
                            sfree(invalid);
                            key = NULL;
                            s->suppress_wait_for_response_packet = true;
                            break; /* try something else */
                        }
                    }
                }

                if (key) {
                    strbuf *pkblob, *sigdata, *sigblob;

                    /*
                     * We have loaded the private key and the server
                     * has announced that it's willing to accept it.
                     * Hallelujah. Generate a signature and send it.
                     */
                    s->pktout = ssh_bpp_new_pktout(
                        s->ppl.bpp, SSH2_MSG_USERAUTH_REQUEST);
                    put_stringz(s->pktout, s->username);
                    put_stringz(s->pktout, s->successor_layer->vt->name);
                    put_stringz(s->pktout, "publickey"); /* method */
                    put_bool(s->pktout, true); /* signature follows */
                    pkblob = strbuf_new();
                    ssh_key_public_blob(key->key, BinarySink_UPCAST(pkblob));
                    ssh2_userauth_add_alg_and_publickey(
                        s, s->pktout,
                        ptrlen_from_asciz(s->publickey_algorithm),
                        ptrlen_from_strbuf(pkblob));

                    /*
                     * The data to be signed is:
                     *
                     *   string  session-id
                     *
                     * followed by everything so far placed in the
                     * outgoing packet.
                     */
                    sigdata = strbuf_new();
                    ssh2_userauth_add_session_id(s, sigdata);
                    put_data(sigdata, s->pktout->data + 5,
                             s->pktout->length - 5);
                    sigblob = strbuf_new();
                    ssh_key_sign(key->key, ptrlen_from_strbuf(sigdata),
                                 s->signflags, BinarySink_UPCAST(sigblob));
                    strbuf_free(sigdata);
                    ssh2_userauth_add_sigblob(
                        s, s->pktout, ptrlen_from_strbuf(pkblob),
                        ptrlen_from_strbuf(sigblob));
                    strbuf_free(pkblob);
                    strbuf_free(sigblob);

                    pq_push(s->ppl.out_pq, s->pktout);
                    ppl_logevent("Sent public key signature");
                    s->type = AUTH_TYPE_PUBLICKEY;
                    ssh_key_free(key->key);
                    sfree(key->comment);
                    sfree(key);
                    s->is_trivial_auth = false;
                }
                } // WINSCP

#ifndef NO_GSSAPI
            } else if (s->can_gssapi && !s->tried_gssapi) {

                /* gssapi-with-mic authentication */

                ptrlen data;

                s->type = AUTH_TYPE_GSSAPI;
                s->tried_gssapi = true;
                s->ppl.bpp->pls->actx = SSH2_PKTCTX_GSSAPI;

                if (s->shgss->lib->gsslogmsg)
                    ppl_logevent("%s", s->shgss->lib->gsslogmsg);

                /* Sending USERAUTH_REQUEST with "gssapi-with-mic" method */
                ppl_logevent("Trying gssapi-with-mic...");
                s->pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH2_MSG_USERAUTH_REQUEST);
                put_stringz(s->pktout, s->username);
                put_stringz(s->pktout, s->successor_layer->vt->name);
                put_stringz(s->pktout, "gssapi-with-mic");
                ppl_logevent("Attempting GSSAPI authentication");

                /* add mechanism info */
                s->shgss->lib->indicate_mech(s->shgss->lib, &s->gss_buf);

                /* number of GSSAPI mechanisms */
                put_uint32(s->pktout, 1);

                /* length of OID + 2 */
                put_uint32(s->pktout, s->gss_buf.length + 2);
                put_byte(s->pktout, SSH2_GSS_OIDTYPE);

                /* length of OID */
                put_byte(s->pktout, s->gss_buf.length);

                put_data(s->pktout, s->gss_buf.value, s->gss_buf.length);
                pq_push(s->ppl.out_pq, s->pktout);
                crMaybeWaitUntilV((pktin = ssh2_userauth_pop(s)) != NULL);
                if (pktin->type != SSH2_MSG_USERAUTH_GSSAPI_RESPONSE) {
                    ppl_logevent("GSSAPI authentication request refused");
                    pq_push_front(s->ppl.in_pq, pktin);
                    continue;
                }

                /* check returned packet ... */

                data = get_string(pktin);
                s->gss_rcvtok.value = (char *)data.ptr;
                s->gss_rcvtok.length = data.len;
                if (s->gss_rcvtok.length != s->gss_buf.length + 2 ||
                    ((char *)s->gss_rcvtok.value)[0] != SSH2_GSS_OIDTYPE ||
                    ((char *)s->gss_rcvtok.value)[1] != s->gss_buf.length ||
                    memcmp((char *)s->gss_rcvtok.value + 2,
                           s->gss_buf.value,s->gss_buf.length) ) {
                    ppl_logevent("GSSAPI authentication - wrong response "
                                 "from server");
                    continue;
                }

                /* Import server name if not cached from KEX */
                if (s->shgss->srv_name == GSS_C_NO_NAME) {
                    // WINSCP
                    char * fullhostname = s->fullhostname;
                    if (s->loghost[0] != '\0')
                    {
                        fullhostname = s->loghost;
                    }
                    // /WINSCP
                    s->gss_stat = s->shgss->lib->import_name(
                        s->shgss->lib, fullhostname, &s->shgss->srv_name); // WINSCP
                    if (s->gss_stat != SSH_GSS_OK) {
                        if (s->gss_stat == SSH_GSS_BAD_HOST_NAME)
                            ppl_logevent("GSSAPI import name failed -"
                                         " Bad service name");
                        else
                            ppl_logevent("GSSAPI import name failed");
                        continue;
                    }
                }

                /* Allocate our gss_ctx */
                s->gss_stat = s->shgss->lib->acquire_cred(
                    s->shgss->lib, &s->shgss->ctx, NULL);
                if (s->gss_stat != SSH_GSS_OK) {
                    ppl_logevent("GSSAPI authentication failed to get "
                                 "credentials");
                    /* The failure was on our side, so the server
                     * won't be sending a response packet indicating
                     * failure. Avoid waiting for it next time round
                     * the loop. */
                    s->suppress_wait_for_response_packet = true;
                    continue;
                }

                /* initial tokens are empty */
                SSH_GSS_CLEAR_BUF(&s->gss_rcvtok);
                SSH_GSS_CLEAR_BUF(&s->gss_sndtok);

                /* now enter the loop */
                do {
                    /*
                     * When acquire_cred yields no useful expiration, go with
                     * the service ticket expiration.
                     */
                    s->gss_stat = s->shgss->lib->init_sec_context(
                        s->shgss->lib,
                        &s->shgss->ctx,
                        s->shgss->srv_name,
                        s->gssapi_fwd,
                        &s->gss_rcvtok,
                        &s->gss_sndtok,
                        NULL,
                        NULL);

                    if (s->gss_stat!=SSH_GSS_S_COMPLETE &&
                        s->gss_stat!=SSH_GSS_S_CONTINUE_NEEDED) {
                        ppl_logevent("GSSAPI authentication initialisation "
                                     "failed");

                        if (s->shgss->lib->display_status(
                                s->shgss->lib, s->shgss->ctx, &s->gss_buf)
                            == SSH_GSS_OK) {
                            ppl_logevent("%s", (char *)s->gss_buf.value);
                            sfree(s->gss_buf.value);
                        }

                        pq_push_front(s->ppl.in_pq, pktin);
                        break;
                    }
                    ppl_logevent("GSSAPI authentication initialised");

                    /*
                     * Client and server now exchange tokens until GSSAPI
                     * no longer says CONTINUE_NEEDED
                     */
                    if (s->gss_sndtok.length != 0) {
                        s->is_trivial_auth = false;
                        s->pktout =
                            ssh_bpp_new_pktout(
                                s->ppl.bpp, SSH2_MSG_USERAUTH_GSSAPI_TOKEN);
                        put_string(s->pktout,
                                   s->gss_sndtok.value, s->gss_sndtok.length);
                        pq_push(s->ppl.out_pq, s->pktout);
                        s->shgss->lib->free_tok(s->shgss->lib, &s->gss_sndtok);
                    }

                    if (s->gss_stat == SSH_GSS_S_CONTINUE_NEEDED) {
                        crMaybeWaitUntilV((pktin = ssh2_userauth_pop(s)) != NULL);

                        if (pktin->type == SSH2_MSG_USERAUTH_GSSAPI_ERRTOK) {
                            /*
                             * Per RFC 4462 section 3.9, this packet
                             * type MUST immediately precede an
                             * ordinary USERAUTH_FAILURE.
                             *
                             * We currently don't know how to do
                             * anything with the GSSAPI error token
                             * contained in this packet, so we ignore
                             * it and just wait for the following
                             * FAILURE.
                             */
                            crMaybeWaitUntilV(
                                (pktin = ssh2_userauth_pop(s)) != NULL);
                            if (pktin->type != SSH2_MSG_USERAUTH_FAILURE) {
                                ssh_proto_error(
                                    s->ppl.ssh, "Received unexpected packet "
                                    "after SSH_MSG_USERAUTH_GSSAPI_ERRTOK "
                                    "(expected SSH_MSG_USERAUTH_FAILURE): "
                                    "type %d (%s)", pktin->type,
                                    ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                                  s->ppl.bpp->pls->actx,
                                                  pktin->type));
                                return;
                            }
                        }

                        if (pktin->type == SSH2_MSG_USERAUTH_FAILURE) {
                            ppl_logevent("GSSAPI authentication failed");
                            s->gss_stat = SSH_GSS_FAILURE;
                            pq_push_front(s->ppl.in_pq, pktin);
                            break;
                        } else if (pktin->type !=
                                   SSH2_MSG_USERAUTH_GSSAPI_TOKEN) {
                            ppl_logevent("GSSAPI authentication -"
                                         " bad server response");
                            s->gss_stat = SSH_GSS_FAILURE;
                            break;
                        }
                        data = get_string(pktin);
                        s->gss_rcvtok.value = (char *)data.ptr;
                        s->gss_rcvtok.length = data.len;
                    }
                } while (s-> gss_stat == SSH_GSS_S_CONTINUE_NEEDED);

                if (s->gss_stat != SSH_GSS_OK) {
                    s->shgss->lib->release_cred(s->shgss->lib, &s->shgss->ctx);
                    continue;
                }
                ppl_logevent("GSSAPI authentication loop finished OK");

                /* Now send the MIC */

                s->pktout = ssh2_userauth_gss_packet(s, "gssapi-with-mic");
                pq_push(s->ppl.out_pq, s->pktout);

                s->shgss->lib->release_cred(s->shgss->lib, &s->shgss->ctx);
                continue;
#endif
            } else if (s->can_keyb_inter && !s->kbd_inter_refused) {

                /*
                 * Keyboard-interactive authentication.
                 */

                s->type = AUTH_TYPE_KEYBOARD_INTERACTIVE;

                s->ppl.bpp->pls->actx = SSH2_PKTCTX_KBDINTER;

                s->pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH2_MSG_USERAUTH_REQUEST);
                put_stringz(s->pktout, s->username);
                put_stringz(s->pktout, s->successor_layer->vt->name);
                put_stringz(s->pktout, "keyboard-interactive");
                                                        /* method */
                put_stringz(s->pktout, "");     /* lang */
                put_stringz(s->pktout, "");     /* submethods */
                pq_push(s->ppl.out_pq, s->pktout);

                ppl_logevent("Attempting keyboard-interactive authentication");

                if (s->authplugin) {
                    strbuf *amsg = authplugin_newmsg(PLUGIN_PROTOCOL);
                    put_stringz(amsg, "keyboard-interactive");
                    authplugin_send_free(s, amsg);

                    { // WINSCP
                    BinarySource src[1];
                    unsigned type;
                    crMaybeWaitUntilV(authplugin_expect_msg(s, &type, src));
                    switch (type) {
                      case PLUGIN_PROTOCOL_REJECT: {
                        ptrlen message = PTRLEN_LITERAL("");
                        if (s->authplugin_version >= 2) {
                            /* draft protocol didn't include a message here */
                            message = get_string(src);
                        }
                        if (get_err(src)) {
                            ssh_sw_abort(s->ppl.ssh, "Received malformed "
                                         "PLUGIN_PROTOCOL_REJECT from auth "
                                         "helper plugin");
                            return;
                        }
                        if (message.len) {
                            /* If the plugin sent a message about
                             * _why_ it didn't want to do k-i, pass
                             * that message on to the user. (It might
                             * say, for example, what went wrong when
                             * it tried to open its config file.) */
                            ppl_printf("Authentication plugin failed to set "
                                       "up keyboard-interactive "
                                       "authentication:\r\n");
                            seat_set_trust_status(s->ppl.seat, false);
                            ppl_printf("%.*s\r\n", PTRLEN_PRINTF(message));
                            seat_set_trust_status(s->ppl.seat, true);
                            ppl_logevent("Authentication plugin declined to "
                                         "help with keyboard-interactive: "
                                         "%.*s", PTRLEN_PRINTF(message));
                        } else {
                            ppl_logevent("Authentication plugin declined to "
                                         "help with keyboard-interactive");
                        }
                        s->authplugin_ki_active = false;
                        break;
                      }
                      case PLUGIN_PROTOCOL_ACCEPT:
                        s->authplugin_ki_active = true;
                        ppl_logevent("Authentication plugin agreed to help "
                                     "with keyboard-interactive");
                        break;
                      default:
                        authplugin_bad_packet(
                            s, type, "expected PLUGIN_PROTOCOL_ACCEPT or "
                            "PLUGIN_PROTOCOL_REJECT");
                        return;
                    }
                    } // WINSCP
                } else {
                    s->authplugin_ki_active = false;
                }

                if (!s->ki_scc_initialised) {
                    s->ki_scc = seat_stripctrl_new(
                        s->ppl.seat, NULL, SIC_KI_PROMPTS);
                    if (s->ki_scc)
                        stripctrl_enable_line_limiting(s->ki_scc);
                    s->ki_scc_initialised = true;
                }

                crMaybeWaitUntilV((pktin = ssh2_userauth_pop(s)) != NULL);
                if (pktin->type != SSH2_MSG_USERAUTH_INFO_REQUEST) {
                    /* Server is not willing to do keyboard-interactive
                     * at all (or, bizarrely but legally, accepts the
                     * user without actually issuing any prompts).
                     * Give up on it entirely. */
                    pq_push_front(s->ppl.in_pq, pktin);
                    s->type = AUTH_TYPE_KEYBOARD_INTERACTIVE_QUIET;
                    s->kbd_inter_refused = true; /* don't try it again */
                    continue;
                }

                s->ki_printed_header = false;

                /*
                 * Loop while we still have prompts to send to the user.
                 */
                if (!s->authplugin_ki_active) {
                    /*
                     * The simple case: INFO_REQUESTs are passed on to
                     * the user, and responses are sent straight back
                     * to the SSH server.
                     */
                    while (pktin->type == SSH2_MSG_USERAUTH_INFO_REQUEST) {
                        if (!ssh2_userauth_ki_setup_prompts(
                                s, BinarySource_UPCAST(pktin), false))
                            return;
                        crMaybeWaitUntilV(ssh2_userauth_ki_run_prompts(s));

                        if (spr_is_abort(s->spr)) {
                            /*
                             * Failed to get responses. Terminate.
                             */
                            free_prompts(s->cur_prompt);
                            s->cur_prompt = NULL;
                            ssh_bpp_queue_disconnect(
                                s->ppl.bpp, "Unable to authenticate",
                                SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER);
                            ssh_spr_close(s->ppl.ssh, s->spr, "keyboard-"
                                          "interactive authentication prompt");
                            return;
                        }

                        /*
                         * Send the response(s) to the server.
                         */
                        s->pktout = ssh_bpp_new_pktout(
                            s->ppl.bpp, SSH2_MSG_USERAUTH_INFO_RESPONSE);
                        ssh2_userauth_ki_write_responses(
                            s, BinarySink_UPCAST(s->pktout));
                        s->pktout->minlen = 256;
                        pq_push(s->ppl.out_pq, s->pktout);

                        /*
                         * Get the next packet in case it's another
                         * INFO_REQUEST.
                         */
                        crMaybeWaitUntilV(
                            (pktin = ssh2_userauth_pop(s)) != NULL);
                    }
                } else {
                    /*
                     * The case where a plugin is involved:
                     * INFO_REQUEST from the server is sent to the
                     * plugin, which sends responses that we hand back
                     * to the server. But in the meantime, the plugin
                     * might send USER_REQUEST for us to pass to the
                     * user, and then we send responses to that.
                     */
                    while (pktin->type == SSH2_MSG_USERAUTH_INFO_REQUEST) {
                        strbuf *amsg = authplugin_newmsg(
                            PLUGIN_KI_SERVER_REQUEST);
                        put_datapl(amsg, get_data(pktin, get_avail(pktin)));
                        authplugin_send_free(s, amsg);

                        { // WINSCP
                        BinarySource src[1];
                        unsigned type;
                        while (true) {
                            crMaybeWaitUntilV(authplugin_expect_msg(
                                                  s, &type, src));
                            if (type != PLUGIN_KI_USER_REQUEST)
                                break;

                            if (!ssh2_userauth_ki_setup_prompts(s, src, true))
                                return;
                            crMaybeWaitUntilV(ssh2_userauth_ki_run_prompts(s));

                            if (spr_is_abort(s->spr)) {
                                /*
                                 * Failed to get responses. Terminate.
                                 */
                                free_prompts(s->cur_prompt);
                                s->cur_prompt = NULL;
                                ssh_bpp_queue_disconnect(
                                    s->ppl.bpp, "Unable to authenticate",
                                    SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER);
                                ssh_spr_close(
                                    s->ppl.ssh, s->spr, "keyboard-"
                                    "interactive authentication prompt");
                                return;
                            }

                            /*
                             * Send the responses on to the plugin.
                             */
                            { // WINSCP
                            strbuf *amsg = authplugin_newmsg(
                                PLUGIN_KI_USER_RESPONSE);
                            ssh2_userauth_ki_write_responses(
                                s, BinarySink_UPCAST(amsg));
                            authplugin_send_free(s, amsg);
                            } // WINSCP
                        }

                        if (type != PLUGIN_KI_SERVER_RESPONSE) {
                            authplugin_bad_packet(
                                s, type, "expected PLUGIN_KI_SERVER_RESPONSE "
                                "or PLUGIN_PROTOCOL_USER_REQUEST");
                            return;
                        }

                        s->pktout = ssh_bpp_new_pktout(
                            s->ppl.bpp, SSH2_MSG_USERAUTH_INFO_RESPONSE);
                        put_datapl(s->pktout, get_data(src, get_avail(src)));
                        s->pktout->minlen = 256;
                        pq_push(s->ppl.out_pq, s->pktout);

                        /*
                         * Get the next packet in case it's another
                         * INFO_REQUEST.
                         */
                        crMaybeWaitUntilV(
                            (pktin = ssh2_userauth_pop(s)) != NULL);
                        } // WINSCP
                    }
                }

                /*
                 * Print our trailer line, if we printed a header.
                 */
                if (s->ki_printed_header) {
                    seat_set_trust_status(s->ppl.seat, true);
                    seat_antispoof_msg(
                        ppl_get_iseat(&s->ppl),
                        (s->authplugin_ki_active ?
                         "End of keyboard-interactive prompts from plugin" :
                         "End of keyboard-interactive prompts from server"));
                }

                /*
                 * We should have SUCCESS or FAILURE now.
                 */
                pq_push_front(s->ppl.in_pq, pktin);

                if (s->authplugin_ki_active) {
                    /*
                     * As our last communication with the plugin, tell
                     * it whether the k-i authentication succeeded.
                     */
                    int plugin_msg = -1;
                    if (pktin->type == SSH2_MSG_USERAUTH_SUCCESS) {
                        plugin_msg = PLUGIN_AUTH_SUCCESS;
                    } else if (pktin->type == SSH2_MSG_USERAUTH_FAILURE) {
                        /*
                         * Peek in the failure packet to see if it's a
                         * partial success.
                         */
                        BinarySource src[1];
                        BinarySource_BARE_INIT(
                            src, get_ptr(pktin), get_avail(pktin));
                        get_string(pktin); /* skip methods */
                        { // WINSCP
                        bool partial_success = get_bool(pktin);
                        if (!get_err(src)) {
                            plugin_msg = partial_success ?
                                PLUGIN_AUTH_SUCCESS : PLUGIN_AUTH_FAILURE;
                        }
                        } // WINSCP
                    }

                    if (plugin_msg >= 0) {
                        strbuf *amsg = authplugin_newmsg(plugin_msg);
                        authplugin_send_free(s, amsg);

                        /* Wait until we've actually sent it, in case
                         * we close the connection to the plugin
                         * before that outgoing message has left our
                         * own buffers */
                        crMaybeWaitUntilV(s->authplugin_backlog == 0);
                    }
                }
            } else if (s->can_passwd) {
                s->is_trivial_auth = false;
                { // WINSCP
                /*
                 * Plain old password authentication.
                 */
                bool changereq_first_time; /* not live over crReturn */

                s->ppl.bpp->pls->actx = SSH2_PKTCTX_PASSWORD;

                // WINSCP
                if (s->change_password)
                {
                    s->password = dupstr("");
                    s->type = AUTH_TYPE_PASSWORD;
                }
                else
                {
                // no indentation to ease merges
                // /WINSCP
                s->cur_prompt = ssh_ppl_new_prompts(&s->ppl);
                s->cur_prompt->utf8 = true;
                s->cur_prompt->to_server = true;
                s->cur_prompt->from_server = false;
                s->cur_prompt->name = dupstr("SSH password");
                add_prompt(s->cur_prompt, dupprintf("%s@%s's password: ",
                                                    s->username, s->hostname),
                           false);

                s->spr = seat_get_userpass_input(
                    ppl_get_iseat(&s->ppl), s->cur_prompt);
                while (s->spr.kind == SPRK_INCOMPLETE) {
                    crReturnV;
                    s->spr = seat_get_userpass_input(
                        ppl_get_iseat(&s->ppl), s->cur_prompt);
                }
                if (spr_is_abort(s->spr)) {
                    /*
                     * Failed to get responses. Terminate.
                     */
                    free_prompts(s->cur_prompt);
                    s->cur_prompt = NULL;
                    ssh_bpp_queue_disconnect(
                        s->ppl.bpp, "Unable to authenticate",
                        SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER);
                    ssh_spr_close(s->ppl.ssh, s->spr, "password prompt");
                    return;
                }
                /*
                 * Squirrel away the password. (We may need it later if
                 * asked to change it.)
                 */
                s->password = prompt_get_result(s->cur_prompt->prompts[0]);
                free_prompts(s->cur_prompt);
                s->cur_prompt = NULL;

                /*
                 * Send the password packet.
                 *
                 * We pad out the password packet to 256 bytes to make
                 * it harder for an attacker to find the length of the
                 * user's password.
                 *
                 * Anyone using a password longer than 256 bytes
                 * probably doesn't have much to worry about from
                 * people who find out how long their password is!
                 */
                s->pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH2_MSG_USERAUTH_REQUEST);
                put_stringz(s->pktout, s->username);
                put_stringz(s->pktout, s->successor_layer->vt->name);
                put_stringz(s->pktout, "password");
                put_bool(s->pktout, false);
                put_stringz(s->pktout, s->password);
                s->pktout->minlen = 256;
                pq_push(s->ppl.out_pq, s->pktout);
                ppl_logevent("Sent password");
                s->type = AUTH_TYPE_PASSWORD;

                /*
                 * Wait for next packet, in case it's a password change
                 * request.
                 */
                crMaybeWaitUntilV((pktin = ssh2_userauth_pop(s)) != NULL);
                } // WINSCP
                changereq_first_time = true;

                while ((pktin->type == SSH2_MSG_USERAUTH_PASSWD_CHANGEREQ) ||
                       s->change_password) { // WINSCP

                    /*
                     * We're being asked for a new password
                     * (perhaps not for the first time).
                     * Loop until the server accepts it.
                     */

                    bool got_new = false; /* not live over crReturn */
                    ptrlen prompt;  /* not live over crReturn */

                    if (!s->change_password) // WINSCP
                    {
                        const char *msg;
                        if (changereq_first_time)
                            msg = "Server requested password change";
                        else
                            msg = "Server rejected new password";
                        ppl_logevent("%s", msg);
                        ppl_printf("%s\r\n", msg);
                    }

                    s->change_password = false; // WINSCP

                    prompt = get_string(pktin);

                    s->cur_prompt = ssh_ppl_new_prompts(&s->ppl);
                    s->cur_prompt->utf8 = true;
                    s->cur_prompt->to_server = true;
                    s->cur_prompt->from_server = false;
                    s->cur_prompt->name = dupstr("New SSH password");
                    s->cur_prompt->instruction = mkstr(prompt);
                    s->cur_prompt->instr_reqd = true;
                    /*
                     * There's no explicit requirement in the protocol
                     * for the "old" passwords in the original and
                     * password-change messages to be the same, and
                     * apparently some Cisco kit supports password change
                     * by the user entering a blank password originally
                     * and the real password subsequently, so,
                     * reluctantly, we prompt for the old password again.
                     *
                     * (On the other hand, some servers don't even bother
                     * to check this field.)
                     */
                    add_prompt(s->cur_prompt,
                               dupstr("Current password (blank for previously entered password): "),
                               false);
                    add_prompt(s->cur_prompt, dupstr("Enter new password: "),
                               false);
                    add_prompt(s->cur_prompt, dupstr("Confirm new password: "),
                               false);

                    /*
                     * Loop until the user manages to enter the same
                     * password twice.
                     */
                    while (!got_new) {
                        s->spr = seat_get_userpass_input(
                            ppl_get_iseat(&s->ppl), s->cur_prompt);
                        while (s->spr.kind == SPRK_INCOMPLETE) {
                            crReturnV;
                            s->spr = seat_get_userpass_input(
                                ppl_get_iseat(&s->ppl), s->cur_prompt);
                        }
                        if (spr_is_abort(s->spr)) {
                            /*
                             * Failed to get responses. Terminate.
                             */
                            /* burn the evidence */
                            free_prompts(s->cur_prompt);
                            s->cur_prompt = NULL;
                            smemclr(s->password, strlen(s->password));
                            sfree(s->password);
                            ssh_bpp_queue_disconnect(
                                s->ppl.bpp, "Unable to authenticate",
                                SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER);
                            ssh_spr_close(s->ppl.ssh, s->spr,
                                          "password-change prompt");
                            return;
                        }

                        /*
                         * If the user specified a new original password
                         * (IYSWIM), overwrite any previously specified
                         * one.
                         * (A side effect is that the user doesn't have to
                         * re-enter it if they louse up the new password.)
                         */
                        if (s->cur_prompt->prompts[0]->result->s[0]) {
                            smemclr(s->password, strlen(s->password));
                                /* burn the evidence */
                            sfree(s->password);
                            s->password = prompt_get_result(
                                s->cur_prompt->prompts[0]);
                        }

                        /*
                         * Check the two new passwords match.
                         */
                        got_new = !strcmp(
                            prompt_get_result_ref(s->cur_prompt->prompts[1]),
                            prompt_get_result_ref(s->cur_prompt->prompts[2]));
                        if (!got_new)
                            /* They don't. Silly user. */
                            ppl_printf("Passwords do not match\r\n");

                    }

                    /*
                     * Send the new password (along with the old one).
                     * (see above for padding rationale)
                     */
                    s->pktout = ssh_bpp_new_pktout(
                        s->ppl.bpp, SSH2_MSG_USERAUTH_REQUEST);
                    put_stringz(s->pktout, s->username);
                    put_stringz(s->pktout, s->successor_layer->vt->name);
                    put_stringz(s->pktout, "password");
                    put_bool(s->pktout, true);
                    put_stringz(s->pktout, s->password);
                    put_stringz(s->pktout, prompt_get_result_ref(
                                    s->cur_prompt->prompts[1]));
                    free_prompts(s->cur_prompt);
                    s->cur_prompt = NULL;
                    s->pktout->minlen = 256;
                    pq_push(s->ppl.out_pq, s->pktout);
                    ppl_logevent("Sent new password");

                    /*
                     * Now see what the server has to say about it.
                     * (If it's CHANGEREQ again, it's not happy with the
                     * new password.)
                     */
                    crMaybeWaitUntilV((pktin = ssh2_userauth_pop(s)) != NULL);
                    changereq_first_time = false;

                }

                /*
                 * We need to reexamine the current pktin at the top
                 * of the loop. Either:
                 *  - we weren't asked to change password at all, in
                 *    which case it's a SUCCESS or FAILURE with the
                 *    usual meaning
                 *  - we sent a new password, and the server was
                 *    either OK with it (SUCCESS or FAILURE w/partial
                 *    success) or unhappy with the _old_ password
                 *    (FAILURE w/o partial success)
                 * In any of these cases, we go back to the top of
                 * the loop and start again.
                 */
                pq_push_front(s->ppl.in_pq, pktin);

                /*
                 * We don't need the old password any more, in any
                 * case. Burn the evidence.
                 */
                smemclr(s->password, strlen(s->password));
                sfree(s->password);
                } // WINSCP

            } else {
                ssh_bpp_queue_disconnect(
                    s->ppl.bpp,
                    "No supported authentication methods available",
                    SSH2_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE);
                ssh_sw_abort(s->ppl.ssh, "No supported authentication methods "
                             "available (server sent: %s)",
                             s->last_methods_string->s);
                return;
            }

        }
      try_new_username:;
    }

  userauth_success:
    if (s->notrivialauth && s->is_trivial_auth) {
        ssh_proto_error(s->ppl.ssh, "Authentication was trivial! "
                        "Abandoning session as specified in configuration.");
        return;
    }

    /*
     * We've just received USERAUTH_SUCCESS, and we haven't sent
     * any packets since. Signal the transport layer to consider
     * doing an immediate rekey, if it has any reason to want to.
     */
    ssh2_transport_notify_auth_done(s->transport_layer);

    /*
     * Finally, hand over to our successor layer, and return
     * immediately without reaching the crFinishV: ssh_ppl_replace
     * will have freed us, so crFinishV's zeroing-out of crState would
     * be a use-after-free bug.
     */
    {
        PacketProtocolLayer *successor = s->successor_layer;
        s->successor_layer = NULL;     /* avoid freeing it ourself */
        ssh_ppl_replace(&s->ppl, successor);
        return;   /* we've just freed s, so avoid even touching s->crState */
    }

    crFinishV;
}

static void ssh2_userauth_print_banner(struct ssh2_userauth_state *s)
{
    if (bufchain_size(&s->banner) &&
        (seat_verbose(s->ppl.seat) || seat_interactive(s->ppl.seat))) {
        if (s->banner_scc) {
            seat_antispoof_msg(
                ppl_get_iseat(&s->ppl),
                "Pre-authentication banner message from server:");
            seat_set_trust_status(s->ppl.seat, false);
        }

        { // WINSCP
        bool mid_line = false;
        while (bufchain_size(&s->banner) > 0) {
            // WINSCP: consume banner buffer before calling seat_banner_pl.
            // As we might get disconnected, while the banner is displaying,
            // and ssh_remote_error calls here via ssh2_userauth_final_output,
            // so we might get into loop
            ptrlen data = bufchain_prefix(&s->banner);
            char * buf = smalloc(data.len);
            bufchain_fetch(&s->banner, buf, data.len);
            mid_line =
                (((const char *)data.ptr)[data.len-1] != '\n');
            bufchain_consume(&s->banner, data.len);
            seat_banner_pl(ppl_get_iseat(&s->ppl), make_ptrlen(buf, data.len));
            sfree(buf);
        }
        bufchain_clear(&s->banner);

        if (mid_line)
            seat_banner_pl(ppl_get_iseat(&s->ppl),
                           PTRLEN_LITERAL("\r\n"));

        if (s->banner_scc) {
            seat_set_trust_status(s->ppl.seat, true);
            seat_antispoof_msg(ppl_get_iseat(&s->ppl),
                               "End of banner message from server");
        }
        } // WINSCP
    }
}

static bool ssh2_userauth_ki_setup_prompts(
    struct ssh2_userauth_state *s, BinarySource *src, bool plugin)
{
    ptrlen name, inst;
    strbuf *sb;

    /*
     * We've got a fresh USERAUTH_INFO_REQUEST. Get the preamble and
     * start building a prompt.
     */
    name = get_string(src);
    inst = get_string(src);
    get_string(src); /* skip language tag */
    s->cur_prompt = ssh_ppl_new_prompts(&s->ppl);
    s->cur_prompt->utf8 = true;
    s->cur_prompt->to_server = true;
    s->cur_prompt->from_server = true;

    /*
     * Get any prompt(s) from the packet.
     */
    s->num_prompts = get_uint32(src);
    { // WINSCP
    uint32_t i; // WINSCP
    for (i = 0; i < s->num_prompts; i++) {
        s->is_trivial_auth = false;
        { // WINSCP
        ptrlen prompt = get_string(src);
        bool echo = get_bool(src);

        if (get_err(src)) {
            ssh_proto_error(s->ppl.ssh, "%s sent truncated %s packet",
                            plugin ? "Plugin" : "Server",
                            plugin ? "PLUGIN_KI_USER_REQUEST" :
                            "SSH_MSG_USERAUTH_INFO_REQUEST");
            return false;
        }

        sb = strbuf_new();
        if (!prompt.len) {
            put_fmt(sb, "<%s failed to send prompt>: ",
                    plugin ? "plugin" : "server");
        } else if (s->ki_scc) {
            stripctrl_retarget(s->ki_scc, BinarySink_UPCAST(sb));
            put_datapl(s->ki_scc, prompt);
            stripctrl_retarget(s->ki_scc, NULL);
        } else {
            put_datapl(sb, prompt);
        }
        add_prompt(s->cur_prompt, strbuf_to_str(sb), echo);
        } // WINSCP
    }
    } // WINSCP

    /*
     * Make the header strings. This includes the 'name' (optional
     * dialog-box title) and 'instruction' from the server.
     *
     * First, display our disambiguating header line if this is the
     * first time round the loop - _unless_ the server has sent a
     * completely empty k-i packet with no prompts _or_ text, which
     * apparently some do. In that situation there's no need to alert
     * the user that the following text is server- supplied, because,
     * well, _what_ text?
     *
     * We also only do this if we got a stripctrl, because if we
     * didn't, that suggests this is all being done via dialog boxes
     * anyway.
     */
    if (!s->ki_printed_header && s->ki_scc &&
        (s->num_prompts || name.len || inst.len)) {
        seat_antispoof_msg(
            ppl_get_iseat(&s->ppl),
            (plugin ?
             "Keyboard-interactive authentication prompts from plugin:" :
             "Keyboard-interactive authentication prompts from server:"));
        s->ki_printed_header = true;
        seat_set_trust_status(s->ppl.seat, false);
    }

    sb = strbuf_new();
    if (name.len) {
        put_datapl(sb, PTRLEN_LITERAL("SSH server: ")); // WINSCP
        if (s->ki_scc) {
            stripctrl_retarget(s->ki_scc, BinarySink_UPCAST(sb));
            put_datapl(s->ki_scc, name);
            stripctrl_retarget(s->ki_scc, NULL);
        } else {
            put_datapl(sb, name);
        }
        s->cur_prompt->name_reqd = true;
    } else {
        if (plugin)
            put_datapl(sb, PTRLEN_LITERAL(
                           "Communication with authentication plugin"));
        else
            put_datapl(sb, PTRLEN_LITERAL("SSH server authentication"));
        s->cur_prompt->name_reqd = false;
    }
    s->cur_prompt->name = strbuf_to_str(sb);

    sb = strbuf_new();
    if (inst.len) {
        if (s->ki_scc) {
            stripctrl_retarget(s->ki_scc, BinarySink_UPCAST(sb));
            put_datapl(s->ki_scc, inst);
            stripctrl_retarget(s->ki_scc, NULL);
        } else {
            put_datapl(sb, inst);
        }
        s->cur_prompt->instr_reqd = true;
    } else {
        s->cur_prompt->instr_reqd = false;
    }
    if (sb->len)
        s->cur_prompt->instruction = strbuf_to_str(sb);
    else
        strbuf_free(sb);

    return true;
}

static bool ssh2_userauth_ki_run_prompts(struct ssh2_userauth_state *s)
{
    s->spr = seat_get_userpass_input(
        ppl_get_iseat(&s->ppl), s->cur_prompt);
    return s->spr.kind != SPRK_INCOMPLETE;
}

static void ssh2_userauth_ki_write_responses(
    struct ssh2_userauth_state *s, BinarySink *bs)
{
    put_uint32(bs, s->num_prompts);
    { // WINSCP
    uint32_t i;
    for (i = 0; i < s->num_prompts; i++)
        put_stringz(bs, prompt_get_result_ref(s->cur_prompt->prompts[i]));
    } // WINSCP

    /*
     * Free the prompts structure from this iteration. If there's
     * another, a new one will be allocated when we return to the top
     * of this while loop.
     */
    free_prompts(s->cur_prompt);
    s->cur_prompt = NULL;
}

static void ssh2_userauth_add_session_id(
    struct ssh2_userauth_state *s, strbuf *sigdata)
{
    if (s->ppl.remote_bugs & BUG_SSH2_PK_SESSIONID) {
        put_datapl(sigdata, s->session_id);
    } else {
        put_stringpl(sigdata, s->session_id);
    }
}

static void ssh2_userauth_agent_query(
    struct ssh2_userauth_state *s, strbuf *req)
{
    void *response;
    int response_len;

    sfree(s->agent_response_to_free);
    s->agent_response_to_free = NULL;

    s->auth_agent_query = agent_query(req, &response, &response_len,
                                      ssh2_userauth_agent_callback, s, get_seat_callback_set(s->seat)); // WINSCP
    if (!s->auth_agent_query)
        ssh2_userauth_agent_callback(s, response, response_len);
}

static void ssh2_userauth_agent_callback(void *uav, void *reply, int replylen)
{
    struct ssh2_userauth_state *s = (struct ssh2_userauth_state *)uav;

    s->auth_agent_query = NULL;
    s->agent_response_to_free = reply;
    s->agent_response = make_ptrlen(reply, replylen);

    queue_idempotent_callback(&s->ppl.ic_process_queue);
}

/*
 * Helper function to add the algorithm and public key strings to a
 * "publickey" auth packet. Deals with overriding both strings if the
 * user has provided a detached certificate which matches the public
 * key in question.
 */
static void ssh2_userauth_add_alg_and_publickey(
    struct ssh2_userauth_state *s, PktOut *pkt, ptrlen alg, ptrlen pkblob)
{
    PacketProtocolLayer *ppl = &s->ppl; /* for ppl_logevent */

    if (s->detached_cert_blob) {
        ptrlen detached_cert_pl = ptrlen_from_strbuf(s->detached_cert_blob);
        strbuf *certbase = NULL, *pkbase = NULL;
        bool done = false;
        const ssh_keyalg *pkalg = find_pubkey_alg_len(alg);
        ssh_key *certkey = NULL, *pk = NULL;
        strbuf *fail_reason = strbuf_new();
        bool verbose = true;

        /*
         * Whether or not we send the certificate, we're likely to
         * generate a log message about it. But we don't want to log
         * once for the offer and once for the real auth attempt, so
         * we de-duplicate by remembering the last public key this
         * function saw. */
        if (!s->cert_pubkey_diagnosed)
            s->cert_pubkey_diagnosed = strbuf_new();
        if (ptrlen_eq_ptrlen(ptrlen_from_strbuf(s->cert_pubkey_diagnosed),
                             pkblob)) {
            verbose = false;
        } else {
            /* Log this time, but arrange that we don't mention it next time */
            strbuf_clear(s->cert_pubkey_diagnosed);
            put_datapl(s->cert_pubkey_diagnosed, pkblob);
        }

        /*
         * Check that the public key we're replacing is compatible
         * with the certificate, in that they should have the same
         * base public key.
         */

        { // WINSCP
        const ssh_keyalg *certalg = pubkey_blob_to_alg(detached_cert_pl);
        assert(certalg); /* we checked this before setting s->detached_blob */
        assert(certalg->is_certificate); /* and this too */

        certkey = ssh_key_new_pub(certalg, detached_cert_pl);
        if (!certkey) {
            put_fmt(fail_reason, "certificate key file is invalid");
            goto no_match;
        }

        certbase = strbuf_new();
        ssh_key_public_blob(ssh_key_base_key(certkey),
                            BinarySink_UPCAST(certbase));
        if (ptrlen_eq_ptrlen(pkblob, ptrlen_from_strbuf(certbase)))
            goto match;                /* yes, a match! */

        /*
         * If we reach here, the certificate's base key was not
         * identical to the key we're given. But it might still be
         * identical to the _base_ key of the key we're given, if we
         * were using a differently certified version of the same key.
         * In that situation, the detached cert should still override.
         */
        if (!pkalg) {
            put_fmt(fail_reason, "unable to identify algorithm of base key");
            goto no_match;
        }

        pk = ssh_key_new_pub(pkalg, pkblob);
        if (!pk) {
            put_fmt(fail_reason, "base public key is invalid");
            goto no_match;
        }

        pkbase = strbuf_new();
        ssh_key_public_blob(ssh_key_base_key(pk), BinarySink_UPCAST(pkbase));
        if (ptrlen_eq_ptrlen(ptrlen_from_strbuf(pkbase),
                             ptrlen_from_strbuf(certbase)))
            goto match;                /* yes, a match on 2nd attempt! */

        /* Give up; we've tried to match these keys up and failed. */
        put_fmt(fail_reason, "base public key does not match certificate");
        goto no_match;

      match:
        /*
         * The two keys match, so insert the detached certificate into
         * the output packet in place of the public key we were given.
         *
         * However, we need to be a bit careful with the algorithm
         * name: we might need to upgrade it to one that matches the
         * original algorithm name. (If we were asked to add an
         * ssh-rsa key but were given algorithm name "rsa-sha2-512",
         * then instead of the certificate's algorithm name
         * ssh-rsa-cert-v01@... we need to write the corresponding
         * SHA-512 name rsa-sha2-512-cert-v01@... .)
         */
        if (verbose) {
            ppl_logevent(WINSCP_BOM "Sending public key with certificate from \"%s\"",
                         filename_to_str(s->detached_cert_file));
        }
        {
            /* Strip off any existing certificate-nature from pkalg,
             * for the case where we're replacing a cert embedded in
             * the key with the detached one. The second argument of
             * ssh_keyalg_related_alg is expected to be one of the
             * bare key algorithms, or nothing useful will happen. */
            const ssh_keyalg *pkalg_base =
                pkalg->base_alg ? pkalg->base_alg : pkalg;

            /* Construct an algorithm string that includes both the
             * signature subtype (e.g. rsa-sha2-512) and the
             * certificate-ness. Exception: in earlier versions of
             * OpenSSH we don't want to do that, and must send just
             * ssh-rsa-cert-... even when we're delivering a non-SHA-1
             * signature. */
            const ssh_keyalg *output_alg =
                ssh_keyalg_related_alg(certalg, pkalg_base);
            ptrlen output_id = ptrlen_from_asciz(output_alg->ssh_id);
            output_id = workaround_rsa_sha2_cert_userauth(s, output_id);

            put_stringpl(pkt, output_id);
        }
        put_stringpl(pkt, ptrlen_from_strbuf(s->detached_cert_blob));
        done = true;
        goto out;

      no_match:
        /* Log that we didn't send the certificate, if this public key
         * isn't the same one as last call to this function. (Need to
         * avoid verbosely logging once for the offer and once for the
         * real auth attempt.) */
	if (verbose) {
            ppl_logevent(WINSCP_BOM "Not substituting certificate \"%s\" for public "
                         "key: %s", filename_to_str(s->detached_cert_file),
                         fail_reason->s);
            if (s->publickey_blob) {
                /* If the user provided a specific key file to use (i.e.
                 * this wasn't just a key we picked opportunistically out
                 * of an agent), then they probably _care_ that we didn't
                 * send the certificate, so we should make a loud error
                 * message about it as well as just commenting in the
                 * Event Log. */
                ppl_printf(WINSCP_BOM "Unable to use certificate \"%s\" with public "
                           "key \"%s\": %s\r\n",
                           filename_to_str(s->detached_cert_file),
                           filename_to_str(s->keyfile),
                           fail_reason->s);
            }
        }

      out:
        /* Whether we did that or not, free our stuff. */
        if (certbase)
            strbuf_free(certbase);
        if (pkbase)
            strbuf_free(pkbase);
        if (certkey)
            ssh_key_free(certkey);
        if (pk)
            ssh_key_free(pk);
        strbuf_free(fail_reason);

        /* And if we did, don't fall through to the alternative below */
        if (done)
            return;
        } // WINSCP
    }

    /* In all other cases, basically just put in what we were given -
     * except for the same bug workaround as above. */
    alg = workaround_rsa_sha2_cert_userauth(s, alg);
    put_stringpl(pkt, alg);
    put_stringpl(pkt, pkblob);
}

static ptrlen workaround_rsa_sha2_cert_userauth(
    struct ssh2_userauth_state *s, ptrlen id)
{
    if (!(s->ppl.remote_bugs & BUG_RSA_SHA2_CERT_USERAUTH))
        return id;
    /*
     * No need to try to do this in a general way based on the
     * relations between ssh_keyalgs; we know there are a limited
     * number of affected versions of OpenSSH, so this doesn't have to
     * be futureproof against later additions to the family.
     */
    if (ptrlen_eq_string(id, "rsa-sha2-256-cert-v01@openssh.com") ||
        ptrlen_eq_string(id, "rsa-sha2-512-cert-v01@openssh.com"))
        return PTRLEN_LITERAL("ssh-rsa-cert-v01@openssh.com");
    return id;
}

/*
 * Helper function to add an SSH-2 signature blob to a packet. Expects
 * to be shown the public key blob as well as the signature blob.
 * Normally just appends the sig blob unmodified as a string, except
 * that it optionally breaks it open and fiddle with it to work around
 * BUG_SSH2_RSA_PADDING.
 */
static void ssh2_userauth_add_sigblob(
    struct ssh2_userauth_state *s, PktOut *pkt, ptrlen pkblob, ptrlen sigblob)
{
    BinarySource pk[1], sig[1];
    BinarySource_BARE_INIT_PL(pk, pkblob);
    BinarySource_BARE_INIT_PL(sig, sigblob);

    /* dmemdump(pkblob, pkblob_len); */
    /* dmemdump(sigblob, sigblob_len); */

    /*
     * See if this is in fact an ssh-rsa signature and a buggy
     * server; otherwise we can just do this the easy way.
     */
    if ((s->ppl.remote_bugs & BUG_SSH2_RSA_PADDING) &&
        ptrlen_eq_string(get_string(pk), "ssh-rsa") &&
        ptrlen_eq_string(get_string(sig), "ssh-rsa")) {
        ptrlen mod_mp, sig_mp;
        size_t sig_prefix_len;

        /*
         * Find the modulus and signature integers.
         */
        get_string(pk);                /* skip over exponent */
        mod_mp = get_string(pk);       /* remember modulus */
        sig_prefix_len = sig->pos;
        sig_mp = get_string(sig);
        if (get_err(pk) || get_err(sig))
            goto give_up;

        /*
         * Find the byte length of the modulus, not counting leading
         * zeroes.
         */
        while (mod_mp.len > 0 && *(const char *)mod_mp.ptr == 0) {
            mod_mp.len--;
            mod_mp.ptr = (const char *)mod_mp.ptr + 1;
        }

        /* debug("modulus length is %d\n", len); */
        /* debug("signature length is %d\n", siglen); */

        if (mod_mp.len > sig_mp.len) {
            strbuf *substr = strbuf_new();
            put_data(substr, sigblob.ptr, sig_prefix_len);
            put_uint32(substr, mod_mp.len);
            put_padding(substr, mod_mp.len - sig_mp.len, 0);
            put_datapl(substr, sig_mp);
            put_stringsb(pkt, substr);
            return;
        }

        /* Otherwise fall through and do it the easy way. We also come
         * here as a fallback if we discover above that the key blob
         * is misformatted in some way. */
      give_up:;
    }

    put_stringpl(pkt, sigblob);
}

#ifndef NO_GSSAPI
static PktOut *ssh2_userauth_gss_packet(
    struct ssh2_userauth_state *s, const char *authtype)
{
    strbuf *sb;
    PktOut *p;
    Ssh_gss_buf buf;
    Ssh_gss_buf mic;

    /*
     * The mic is computed over the session id + intended
     * USERAUTH_REQUEST packet.
     */
    sb = strbuf_new();
    put_stringpl(sb, s->session_id);
    put_byte(sb, SSH2_MSG_USERAUTH_REQUEST);
    put_stringz(sb, s->username);
    put_stringz(sb, s->successor_layer->vt->name);
    put_stringz(sb, authtype);

    /* Compute the mic */
    buf.value = sb->s;
    buf.length = sb->len;
    s->shgss->lib->get_mic(s->shgss->lib, s->shgss->ctx, &buf, &mic);
    strbuf_free(sb);

    /* Now we can build the real packet */
    if (strcmp(authtype, "gssapi-with-mic") == 0) {
        p = ssh_bpp_new_pktout(s->ppl.bpp, SSH2_MSG_USERAUTH_GSSAPI_MIC);
    } else {
        p = ssh_bpp_new_pktout(s->ppl.bpp, SSH2_MSG_USERAUTH_REQUEST);
        put_stringz(p, s->username);
        put_stringz(p, s->successor_layer->vt->name);
        put_stringz(p, authtype);
    }
    put_string(p, mic.value, mic.length);

    return p;
}
#endif

static bool ssh2_userauth_get_specials(
    PacketProtocolLayer *ppl, add_special_fn_t add_special, void *ctx)
{
    /* No specials provided by this layer. */
    return false;
}

static void ssh2_userauth_special_cmd(PacketProtocolLayer *ppl,
                                      SessionSpecialCode code, int arg)
{
    /* No specials provided by this layer. */
}

static void ssh2_userauth_reconfigure(PacketProtocolLayer *ppl, Conf *conf)
{
    struct ssh2_userauth_state *s =
        container_of(ppl, struct ssh2_userauth_state, ppl);
    ssh_ppl_reconfigure(s->successor_layer, conf);
}

static void ssh2_userauth_final_output(PacketProtocolLayer *ppl)
{
    struct ssh2_userauth_state *s =
        container_of(ppl, struct ssh2_userauth_state, ppl);

    /*
     * Check for any unconsumed banner packets that might have landed
     * in our queue just before the server closed the connection, and
     * add them to our banner buffer.
     */
    PktIn *pktin; // WINSCP
    for (pktin = pq_first(s->ppl.in_pq); pktin != NULL;
         pktin = pq_next(s->ppl.in_pq, pktin)) {
        if (pktin->type == SSH2_MSG_USERAUTH_BANNER)
            ssh2_userauth_handle_banner_packet(s, pktin);
    }

    /* And now make sure we've shown the banner, before exiting */
    ssh2_userauth_print_banner(s);
}
