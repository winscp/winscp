/*
 * Packet protocol layer for the client side of the SSH-2 userauth
 * protocol (RFC 4252).
 */

#include <assert.h>

#include "putty.h"
#include "ssh.h"
#include "sshbpp.h"
#include "sshppl.h"
#include "sshcr.h"

#ifndef NO_GSSAPI
#include "sshgssc.h"
#include "sshgss.h"
#endif

#define BANNER_LIMIT 131072

struct ssh2_userauth_state {
    int crState;

    PacketProtocolLayer *transport_layer, *successor_layer;
    Filename *keyfile;
    bool tryagent, change_username;
    char *hostname, *fullhostname;
    char *default_username;
    bool try_ki_auth, try_gssapi_auth, try_gssapi_kex_auth, gssapi_fwd;

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
    int userpass_ret;
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
    int num_prompts;
    char *username;
    char *password;
    bool got_username;
    strbuf *publickey_blob;
    bool privatekey_available, privatekey_encrypted;
    char *publickey_algorithm;
    char *publickey_comment;
    void *agent_response_to_free;
    ptrlen agent_response;
    BinarySource asrc[1];          /* for reading SSH agent response */
    size_t pkblob_pos_in_agent;
    int keyi, nkeys;
    ptrlen pk, alg, comment;
    int len;
    PktOut *pktout;
    bool want_user_input;

    agent_pending_query *auth_agent_query;
    bufchain banner;
    bufchain_sink banner_bs;
    StripCtrlChars *banner_scc;
    bool banner_scc_initialised;

    StripCtrlChars *ki_scc;
    bool ki_scc_initialised;
    bool ki_printed_header;

    PacketProtocolLayer ppl;
};

static void ssh2_userauth_free(PacketProtocolLayer *); 
static void ssh2_userauth_process_queue(PacketProtocolLayer *);
static bool ssh2_userauth_get_specials(
    PacketProtocolLayer *ppl, add_special_fn_t add_special, void *ctx);
static void ssh2_userauth_special_cmd(PacketProtocolLayer *ppl,
                                      SessionSpecialCode code, int arg);
static bool ssh2_userauth_want_user_input(PacketProtocolLayer *ppl);
static void ssh2_userauth_got_user_input(PacketProtocolLayer *ppl);
static void ssh2_userauth_reconfigure(PacketProtocolLayer *ppl, Conf *conf);

static void ssh2_userauth_agent_query(struct ssh2_userauth_state *, strbuf *);
static void ssh2_userauth_agent_callback(void *, void *, int);
static void ssh2_userauth_add_sigblob(
    struct ssh2_userauth_state *s, PktOut *pkt, ptrlen pkblob, ptrlen sigblob);
static void ssh2_userauth_add_session_id(
    struct ssh2_userauth_state *s, strbuf *sigdata);
#ifndef NO_GSSAPI
static PktOut *ssh2_userauth_gss_packet(
    struct ssh2_userauth_state *s, const char *authtype);
#endif
static void ssh2_userauth_antispoof_msg(
    struct ssh2_userauth_state *s, const char *msg);

static const struct PacketProtocolLayerVtable ssh2_userauth_vtable = {
    ssh2_userauth_free,
    ssh2_userauth_process_queue,
    ssh2_userauth_get_specials,
    ssh2_userauth_special_cmd,
    ssh2_userauth_want_user_input,
    ssh2_userauth_got_user_input,
    ssh2_userauth_reconfigure,
    "ssh-userauth",
};

PacketProtocolLayer *ssh2_userauth_new(
    PacketProtocolLayer *successor_layer,
    const char *hostname, const char *fullhostname,
    Filename *keyfile, bool tryagent,
    const char *default_username, bool change_username,
    bool try_ki_auth, bool try_gssapi_auth, bool try_gssapi_kex_auth,
    bool gssapi_fwd, struct ssh_connection_shared_gss_state *shgss)
{
    struct ssh2_userauth_state *s = snew(struct ssh2_userauth_state);
    memset(s, 0, sizeof(*s));
    s->ppl.vt = &ssh2_userauth_vtable;

    s->successor_layer = successor_layer;
    s->hostname = dupstr(hostname);
    s->fullhostname = dupstr(fullhostname);
    s->keyfile = filename_copy(keyfile);
    s->tryagent = tryagent;
    s->default_username = dupstr(default_username);
    s->change_username = change_username;
    s->try_ki_auth = try_ki_auth;
    s->try_gssapi_auth = try_gssapi_auth;
    s->try_gssapi_kex_auth = try_gssapi_kex_auth;
    s->gssapi_fwd = gssapi_fwd;
    s->shgss = shgss;
    s->last_methods_string = strbuf_new();
    bufchain_init(&s->banner);
    bufchain_sink_init(&s->banner_bs, &s->banner);

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

    sfree(s->agent_response_to_free);
    if (s->auth_agent_query)
        agent_cancel_query(s->auth_agent_query);
    filename_free(s->keyfile);
    sfree(s->default_username);
    sfree(s->hostname);
    sfree(s->fullhostname);
    strbuf_free(s->last_methods_string);
    if (s->banner_scc)
        stripctrl_free(s->banner_scc);
    if (s->ki_scc)
        stripctrl_free(s->ki_scc);
    sfree(s);
}

static void ssh2_userauth_filter_queue(struct ssh2_userauth_state *s)
{
    PktIn *pktin;
    ptrlen string;

    while ((pktin = pq_peek(s->ppl.in_pq)) != NULL) {
        switch (pktin->type) {
          case SSH2_MSG_USERAUTH_BANNER:
            string = get_string(pktin);
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
        ppl_logevent("Reading key file \"%s\"",
                     filename_to_str(s->keyfile));
        keytype = key_type(s->keyfile);
        if (keytype == SSH_KEYTYPE_SSH2 ||
            keytype == SSH_KEYTYPE_SSH2_PUBLIC_RFC4716 ||
            keytype == SSH_KEYTYPE_SSH2_PUBLIC_OPENSSH) {
            const char *error;
            s->publickey_blob = strbuf_new();
            if (ssh2_userkey_loadpub(s->keyfile,
                                     &s->publickey_algorithm,
                                     BinarySink_UPCAST(s->publickey_blob),
                                     &s->publickey_comment, &error)) {
                s->privatekey_available = (keytype == SSH_KEYTYPE_SSH2);
                if (!s->privatekey_available)
                    ppl_logevent("Key file contains public key only");
                s->privatekey_encrypted =
                    ssh2_userkey_encrypted(s->keyfile, NULL);
            } else {
                ppl_logevent("Unable to load key (%s)", error);
                ppl_printf("Unable to load key file \"%s\" (%s)\r\n",
                           filename_to_str(s->keyfile), error);
                strbuf_free(s->publickey_blob);
                s->publickey_blob = NULL;
            }
        } else {
            ppl_logevent("Unable to use this key file (%s)",
                         key_type_to_str(keytype));
            ppl_printf("Unable to use key file \"%s\" (%s)\r\n",
                       filename_to_str(s->keyfile),
                       key_type_to_str(keytype));
            s->publickey_blob = NULL;
        }
    }

    /*
     * Find out about any keys Pageant has (but if there's a public
     * key configured, filter out all others).
     */
    s->nkeys = 0;
    s->pkblob_pos_in_agent = 0;
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
            int keyi;

            s->nkeys = toint(get_uint32(s->asrc));

            /*
             * Vet the Pageant response to ensure that the key count
             * and blob lengths make sense.
             */
            if (s->nkeys < 0) {
                ppl_logevent("Pageant response contained a negative"
                             " key count %d", s->nkeys);
                s->nkeys = 0;
                goto done_agent_query;
            } else {
                ppl_logevent("Pageant has %d SSH-2 keys", s->nkeys);

                /* See if configured key is in agent. */
                for (keyi = 0; keyi < s->nkeys; keyi++) {
                    size_t pos = s->asrc->pos;
                    ptrlen blob = get_string(s->asrc);
                    get_string(s->asrc); /* skip comment */
                    if (get_err(s->asrc)) {
                        ppl_logevent("Pageant response was truncated");
                        s->nkeys = 0;
                        goto done_agent_query;
                    }

                    if (s->publickey_blob &&
                        blob.len == s->publickey_blob->len &&
                        !memcmp(blob.ptr, s->publickey_blob->s,
                                s->publickey_blob->len)) {
                        ppl_logevent("Pageant key #%d matches "
                                     "configured key file", keyi);
                        s->keyi = keyi;
                        s->pkblob_pos_in_agent = pos;
                        break;
                    }
                }
                if (s->publickey_blob && !s->pkblob_pos_in_agent) {
                    ppl_logevent("Configured key file not in Pageant");
                    s->nkeys = 0;
                }
            }
        } else {
            ppl_logevent("Failed to get reply from Pageant");
        }
      done_agent_query:;
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
    s->got_username = false;
    while (1) {
        /*
         * Get a username.
         */
        if (s->got_username && s->change_username) {
            /*
             * We got a username last time round this loop, and
             * with change_username turned off we don't try to get
             * it again.
             */
        } else if ((s->username = s->default_username) == NULL) {
            s->cur_prompt = new_prompts();
            s->cur_prompt->to_server = true;
            s->cur_prompt->from_server = false;
            s->cur_prompt->name = dupstr("SSH login name");
            add_prompt(s->cur_prompt, dupstr("login as: "), true); 
            s->userpass_ret = seat_get_userpass_input(
                s->ppl.seat, s->cur_prompt, NULL);
            while (1) {
                while (s->userpass_ret < 0 &&
                       bufchain_size(s->ppl.user_input) > 0)
                    s->userpass_ret = seat_get_userpass_input(
                        s->ppl.seat, s->cur_prompt, s->ppl.user_input);

                if (s->userpass_ret >= 0)
                    break;

                s->want_user_input = true;
                crReturnV;
                s->want_user_input = false;
            }
            if (!s->userpass_ret) {
                /*
                 * seat_get_userpass_input() failed to get a username.
                 * Terminate.
                 */
                free_prompts(s->cur_prompt);
                ssh_user_close(s->ppl.ssh, "No username provided");
                return;
            }
            s->username = dupstr(s->cur_prompt->prompts[0]->result);
            free_prompts(s->cur_prompt);
        } else {
            if ((flags & FLAG_VERBOSE) || (flags & FLAG_INTERACTIVE))
                ppl_printf("Using username \"%s\".\r\n", s->username);
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

        /* Reset agent request state. */
        s->done_agent = false;
        if (s->agent_response.ptr) {
            if (s->pkblob_pos_in_agent) {
                s->asrc->pos = s->pkblob_pos_in_agent;
            } else {
                s->asrc->pos = 9;      /* skip length + type + key count */
                s->keyi = 0;
            }
        }

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
            if (bufchain_size(&s->banner) &&
                (flags & (FLAG_VERBOSE | FLAG_INTERACTIVE))) {
                if (s->banner_scc) {
                    ssh2_userauth_antispoof_msg(
                        s, "Pre-authentication banner message from server:");
                    seat_set_trust_status(s->ppl.seat, false);
                }

                bool mid_line = false;
                while (bufchain_size(&s->banner) > 0) {
                    ptrlen data = bufchain_prefix(&s->banner);
                    seat_stderr_pl(s->ppl.seat, data);
                    bufchain_consume(&s->banner, data.len);
                    mid_line =
                        (((const char *)data.ptr)[data.len-1] != '\n');
                }
                bufchain_clear(&s->banner);

                if (mid_line)
                    seat_stderr_pl(s->ppl.seat, PTRLEN_LITERAL("\r\n"));

                if (s->banner_scc) {
                    seat_set_trust_status(s->ppl.seat, true);
                    ssh2_userauth_antispoof_msg(
                        s, "End of banner message from server");
                }
            }

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
                s->last_methods_string->len = 0;
                put_datapl(s->last_methods_string, methods);

                /*
                 * Scan it for method identifiers we know about.
                 */
                bool srv_pubkey = false, srv_passwd = false;
                bool srv_keyb_inter = false, srv_gssapi = false;
                bool srv_gssapi_keyex_auth = false;

                for (ptrlen method; get_commasep_word(&methods, &method) ;) {
                    if (ptrlen_eq_string(method, "publickey"))
                        srv_pubkey = true;
                    else if (ptrlen_eq_string(method, "password"))
                        srv_passwd = true;
                    else if (ptrlen_eq_string(method, "keyboard-interactive"))
                        srv_keyb_inter = true;
                    else if (ptrlen_eq_string(method, "gssapi-with-mic"))
                        srv_gssapi = true;
                    else if (ptrlen_eq_string(method, "gssapi-keyex"))
                        srv_gssapi_keyex_auth = true;
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

            if (s->can_pubkey && !s->done_agent && s->nkeys) {

                /*
                 * Attempt public-key authentication using a key from Pageant.
                 */

                s->ppl.bpp->pls->actx = SSH2_PKTCTX_PUBLICKEY;

                ppl_logevent("Trying Pageant key #%d", s->keyi);

                /* Unpack key from agent response */
                s->pk = get_string(s->asrc);
                s->comment = get_string(s->asrc);
                {
                    BinarySource src[1];
                    BinarySource_BARE_INIT_PL(src, s->pk);
                    s->alg = get_string(src);
                }

                /* See if server will accept it */
                s->pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH2_MSG_USERAUTH_REQUEST);
                put_stringz(s->pktout, s->username);
                put_stringz(s->pktout, s->successor_layer->vt->name);
                put_stringz(s->pktout, "publickey");
                                                    /* method */
                put_bool(s->pktout, false); /* no signature included */
                put_stringpl(s->pktout, s->alg);
                put_stringpl(s->pktout, s->pk);
                pq_push(s->ppl.out_pq, s->pktout);
                s->type = AUTH_TYPE_PUBLICKEY_OFFER_QUIET;

                crMaybeWaitUntilV((pktin = ssh2_userauth_pop(s)) != NULL);
                if (pktin->type != SSH2_MSG_USERAUTH_PK_OK) {

                    /* Offer of key refused, presumably via
                     * USERAUTH_FAILURE. Requeue for the next iteration. */
                    pq_push_front(s->ppl.in_pq, pktin);

                } else {
                    strbuf *agentreq, *sigdata;

                    if (flags & FLAG_VERBOSE)
                        ppl_printf("Authenticating with public key "
                                   "\"%.*s\" from agent\r\n",
                                   PTRLEN_PRINTF(s->comment));

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
                    put_stringpl(s->pktout, s->alg);
                    put_stringpl(s->pktout, s->pk);

                    /* Ask agent for signature. */
                    agentreq = strbuf_new_for_agent_query();
                    put_byte(agentreq, SSH2_AGENTC_SIGN_REQUEST);
                    put_stringpl(agentreq, s->pk);
                    /* Now the data to be signed... */
                    sigdata = strbuf_new();
                    ssh2_userauth_add_session_id(s, sigdata);
                    put_data(sigdata, s->pktout->data + 5,
                             s->pktout->length - 5);
                    put_stringsb(agentreq, sigdata);
                    /* And finally the (zero) flags word. */
                    put_uint32(agentreq, 0);
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
                            ssh2_userauth_add_sigblob(s, s->pktout,
                                                      s->pk, sigblob);
                            pq_push(s->ppl.out_pq, s->pktout);
                            s->type = AUTH_TYPE_PUBLICKEY;
                        } else {
                            ppl_logevent("Pageant refused signing request");
                            ppl_printf("Pageant failed to "
                                       "provide a signature\r\n");
                            s->suppress_wait_for_response_packet = true;
                        }
                    }
                }

                /* Do we have any keys left to try? */
                if (s->pkblob_pos_in_agent) {
                    s->done_agent = true;
                    s->tried_pubkey_config = true;
                } else {
                    s->keyi++;
                    if (s->keyi >= s->nkeys)
                        s->done_agent = true;
                }

            } else if (s->can_pubkey && s->publickey_blob &&
                       s->privatekey_available && !s->tried_pubkey_config) {

                ssh2_userkey *key;   /* not live over crReturn */
                char *passphrase;           /* not live over crReturn */

                s->ppl.bpp->pls->actx = SSH2_PKTCTX_PUBLICKEY;

                s->tried_pubkey_config = true;

                /*
                 * Try the public key supplied in the configuration.
                 *
                 * First, offer the public blob to see if the server is
                 * willing to accept it.
                 */
                s->pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH2_MSG_USERAUTH_REQUEST);
                put_stringz(s->pktout, s->username);
                put_stringz(s->pktout, s->successor_layer->vt->name);
                put_stringz(s->pktout, "publickey");    /* method */
                put_bool(s->pktout, false);
                                                /* no signature included */
                put_stringz(s->pktout, s->publickey_algorithm);
                put_string(s->pktout, s->publickey_blob->s,
                           s->publickey_blob->len);
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
                if (flags & FLAG_VERBOSE)
                    ppl_printf("Authenticating with public key \"%s\"\r\n",
                               s->publickey_comment);

                key = NULL;
                while (!key) {
                    const char *error;  /* not live over crReturn */
                    if (s->privatekey_encrypted) {
                        /*
                         * Get a passphrase from the user.
                         */
                        s->cur_prompt = new_prompts();
                        s->cur_prompt->to_server = false;
                        s->cur_prompt->from_server = false;
                        s->cur_prompt->name = dupstr("SSH key passphrase");
                        add_prompt(s->cur_prompt,
                                   dupprintf("Passphrase for key \"%s\": ",
                                             s->publickey_comment),
                                   false);
                        s->userpass_ret = seat_get_userpass_input(
                            s->ppl.seat, s->cur_prompt, NULL);
                        while (1) {
                            while (s->userpass_ret < 0 &&
                                   bufchain_size(s->ppl.user_input) > 0)
                                s->userpass_ret = seat_get_userpass_input(
                                    s->ppl.seat, s->cur_prompt,
                                    s->ppl.user_input);

                            if (s->userpass_ret >= 0)
                                break;

                            s->want_user_input = true;
                            crReturnV;
                            s->want_user_input = false;
                        }
                        if (!s->userpass_ret) {
                            /* Failed to get a passphrase. Terminate. */
                            free_prompts(s->cur_prompt);
                            ssh_bpp_queue_disconnect(
                                s->ppl.bpp, "Unable to authenticate",
                                SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER);
                            ssh_user_close(s->ppl.ssh, "User aborted at "
                                           "passphrase prompt");
                            return;
                        }
                        passphrase =
                            dupstr(s->cur_prompt->prompts[0]->result);
                        free_prompts(s->cur_prompt);
                    } else {
                        passphrase = NULL; /* no passphrase needed */
                    }

                    /*
                     * Try decrypting the key.
                     */
                    key = ssh2_load_userkey(s->keyfile, passphrase, &error);
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
                    put_stringz(s->pktout, ssh_key_ssh_id(key->key));
                    pkblob = strbuf_new();
                    ssh_key_public_blob(key->key, BinarySink_UPCAST(pkblob));
                    put_string(s->pktout, pkblob->s, pkblob->len);

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
                    ssh_key_sign(key->key, ptrlen_from_strbuf(sigdata), 0,
                                 BinarySink_UPCAST(sigblob));
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
                }

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
                    s->gss_stat = s->shgss->lib->import_name(
                        s->shgss->lib, s->fullhostname, &s->shgss->srv_name);
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
                    s->gss_stat = s->shgss->lib->init_sec_context
                        (s->shgss->lib,
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

                        if (s->shgss->lib->display_status(s->shgss->lib,
                                s->shgss->ctx, &s->gss_buf) == SSH_GSS_OK) {
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
                 * Loop while the server continues to send INFO_REQUESTs.
                 */
                while (pktin->type == SSH2_MSG_USERAUTH_INFO_REQUEST) {

                    ptrlen name, inst;
                    strbuf *sb;
                    int i;

                    /*
                     * We've got a fresh USERAUTH_INFO_REQUEST.
                     * Get the preamble and start building a prompt.
                     */
                    name = get_string(pktin);
                    inst = get_string(pktin);
                    get_string(pktin); /* skip language tag */
                    s->cur_prompt = new_prompts();
                    s->cur_prompt->to_server = true;
                    s->cur_prompt->from_server = true;

                    /*
                     * Get any prompt(s) from the packet.
                     */
                    s->num_prompts = get_uint32(pktin);
                    for (i = 0; i < s->num_prompts; i++) {
                        ptrlen prompt = get_string(pktin);
                        bool echo = get_bool(pktin);

                        sb = strbuf_new();
                        if (!prompt.len) {
                            put_datapl(sb, PTRLEN_LITERAL(
                                "<server failed to send prompt>: "));
                        } else if (s->ki_scc) {
                            stripctrl_retarget(
                                s->ki_scc, BinarySink_UPCAST(sb));
                            put_datapl(s->ki_scc, prompt);
                            stripctrl_retarget(s->ki_scc, NULL);
                        } else {
                            put_datapl(sb, prompt);
                        }
                        add_prompt(s->cur_prompt, strbuf_to_str(sb), echo);
                    }

                    /*
                     * Make the header strings. This includes the
                     * 'name' (optional dialog-box title) and
                     * 'instruction' from the server.
                     *
                     * First, display our disambiguating header line
                     * if this is the first time round the loop -
                     * _unless_ the server has sent a completely empty
                     * k-i packet with no prompts _or_ text, which
                     * apparently some do. In that situation there's
                     * no need to alert the user that the following
                     * text is server- supplied, because, well, _what_
                     * text?
                     *
                     * We also only do this if we got a stripctrl,
                     * because if we didn't, that suggests this is all
                     * being done via dialog boxes anyway.
                     */
                    if (!s->ki_printed_header && s->ki_scc &&
                        (s->num_prompts || name.len || inst.len)) {
                        ssh2_userauth_antispoof_msg(
                            s, "Keyboard-interactive authentication "
                            "prompts from server:");
                        s->ki_printed_header = true;
                        seat_set_trust_status(s->ppl.seat, false);
                    }

                    sb = strbuf_new();
                    if (name.len) {
                        stripctrl_retarget(s->ki_scc, BinarySink_UPCAST(sb));
                        put_datapl(s->ki_scc, name);
                        stripctrl_retarget(s->ki_scc, NULL);

                        s->cur_prompt->name_reqd = true;
                    } else {
                        put_datapl(sb, PTRLEN_LITERAL(
                            "SSH server authentication"));
                        s->cur_prompt->name_reqd = false;
                    }
                    s->cur_prompt->name = strbuf_to_str(sb);

                    sb = strbuf_new();
                    if (inst.len) {
                        stripctrl_retarget(s->ki_scc, BinarySink_UPCAST(sb));
                        put_datapl(s->ki_scc, inst);
                        stripctrl_retarget(s->ki_scc, NULL);

                        s->cur_prompt->instr_reqd = true;
                    } else {
                        s->cur_prompt->instr_reqd = false;
                    }

                    /*
                     * Our prompts_t is fully constructed now. Get the
                     * user's response(s).
                     */
                    s->userpass_ret = seat_get_userpass_input(
                        s->ppl.seat, s->cur_prompt, NULL);
                    while (1) {
                        while (s->userpass_ret < 0 &&
                               bufchain_size(s->ppl.user_input) > 0)
                            s->userpass_ret = seat_get_userpass_input(
                                s->ppl.seat, s->cur_prompt, s->ppl.user_input);

                        if (s->userpass_ret >= 0)
                            break;

                        s->want_user_input = true;
                        crReturnV;
                        s->want_user_input = false;
                    }
                    if (!s->userpass_ret) {
                        /*
                         * Failed to get responses. Terminate.
                         */
                        free_prompts(s->cur_prompt);
                        ssh_bpp_queue_disconnect(
                            s->ppl.bpp, "Unable to authenticate",
                            SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER);
                        ssh_user_close(s->ppl.ssh, "User aborted during "
                                       "keyboard-interactive authentication");
                        return;
                    }

                    /*
                     * Send the response(s) to the server.
                     */
                    s->pktout = ssh_bpp_new_pktout(
                        s->ppl.bpp, SSH2_MSG_USERAUTH_INFO_RESPONSE);
                    put_uint32(s->pktout, s->num_prompts);
                    for (i=0; i < s->num_prompts; i++) {
                        put_stringz(s->pktout,
                                    s->cur_prompt->prompts[i]->result);
                    }
                    s->pktout->minlen = 256;
                    pq_push(s->ppl.out_pq, s->pktout);

                    /*
                     * Free the prompts structure from this iteration.
                     * If there's another, a new one will be allocated
                     * when we return to the top of this while loop.
                     */
                    free_prompts(s->cur_prompt);

                    /*
                     * Get the next packet in case it's another
                     * INFO_REQUEST.
                     */
                    crMaybeWaitUntilV((pktin = ssh2_userauth_pop(s)) != NULL);

                }

                /*
                 * Print our trailer line, if we printed a header.
                 */
                if (s->ki_printed_header) {
                    seat_set_trust_status(s->ppl.seat, true);
                    ssh2_userauth_antispoof_msg(
                        s, "End of keyboard-interactive prompts from server");
                }

                /*
                 * We should have SUCCESS or FAILURE now.
                 */
                pq_push_front(s->ppl.in_pq, pktin);

            } else if (s->can_passwd) {

                /*
                 * Plain old password authentication.
                 */
                bool changereq_first_time; /* not live over crReturn */

                s->ppl.bpp->pls->actx = SSH2_PKTCTX_PASSWORD;

                s->cur_prompt = new_prompts();
                s->cur_prompt->to_server = true;
                s->cur_prompt->from_server = false;
                s->cur_prompt->name = dupstr("SSH password");
                add_prompt(s->cur_prompt, dupprintf("%s@%s's password: ",
                                                    s->username, s->hostname),
                           false);

                s->userpass_ret = seat_get_userpass_input(
                    s->ppl.seat, s->cur_prompt, NULL);
                while (1) {
                    while (s->userpass_ret < 0 &&
                           bufchain_size(s->ppl.user_input) > 0)
                        s->userpass_ret = seat_get_userpass_input(
                            s->ppl.seat, s->cur_prompt, s->ppl.user_input);

                    if (s->userpass_ret >= 0)
                        break;

                    s->want_user_input = true;
                    crReturnV;
                    s->want_user_input = false;
                }
                if (!s->userpass_ret) {
                    /*
                     * Failed to get responses. Terminate.
                     */
                    free_prompts(s->cur_prompt);
                    ssh_bpp_queue_disconnect(
                        s->ppl.bpp, "Unable to authenticate",
                        SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER);
                    ssh_user_close(s->ppl.ssh, "User aborted during password "
                                   "authentication");
                    return;
                }
                /*
                 * Squirrel away the password. (We may need it later if
                 * asked to change it.)
                 */
                s->password = dupstr(s->cur_prompt->prompts[0]->result);
                free_prompts(s->cur_prompt);

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
                changereq_first_time = true;

                while (pktin->type == SSH2_MSG_USERAUTH_PASSWD_CHANGEREQ) {

                    /* 
                     * We're being asked for a new password
                     * (perhaps not for the first time).
                     * Loop until the server accepts it.
                     */

                    bool got_new = false; /* not live over crReturn */
                    ptrlen prompt;  /* not live over crReturn */
                    
                    {
                        const char *msg;
                        if (changereq_first_time)
                            msg = "Server requested password change";
                        else
                            msg = "Server rejected new password";
                        ppl_logevent("%s", msg);
                        ppl_printf("%s\r\n", msg);
                    }

                    prompt = get_string(pktin);

                    s->cur_prompt = new_prompts();
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
                        s->userpass_ret = seat_get_userpass_input(
                            s->ppl.seat, s->cur_prompt, NULL);
                        while (1) {
                            while (s->userpass_ret < 0 &&
                                   bufchain_size(s->ppl.user_input) > 0)
                                s->userpass_ret = seat_get_userpass_input(
                                    s->ppl.seat, s->cur_prompt,
                                    s->ppl.user_input);

                            if (s->userpass_ret >= 0)
                                break;

                            s->want_user_input = true;
                            crReturnV;
                            s->want_user_input = false;
                        }
                        if (!s->userpass_ret) {
                            /*
                             * Failed to get responses. Terminate.
                             */
                            /* burn the evidence */
                            free_prompts(s->cur_prompt);
                            smemclr(s->password, strlen(s->password));
                            sfree(s->password);
                            ssh_bpp_queue_disconnect(
                                s->ppl.bpp, "Unable to authenticate",
                                SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER);
                            ssh_user_close(s->ppl.ssh, "User aborted during "
                                           "password changing");
                            return;
                        }

                        /*
                         * If the user specified a new original password
                         * (IYSWIM), overwrite any previously specified
                         * one.
                         * (A side effect is that the user doesn't have to
                         * re-enter it if they louse up the new password.)
                         */
                        if (s->cur_prompt->prompts[0]->result[0]) {
                            smemclr(s->password, strlen(s->password));
                                /* burn the evidence */
                            sfree(s->password);
                            s->password =
                                dupstr(s->cur_prompt->prompts[0]->result);
                        }

                        /*
                         * Check the two new passwords match.
                         */
                        got_new = (strcmp(s->cur_prompt->prompts[1]->result,
                                          s->cur_prompt->prompts[2]->result)
                                   == 0);
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
                    put_stringz(s->pktout,
                                       s->cur_prompt->prompts[1]->result);
                    free_prompts(s->cur_prompt);
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
                                      ssh2_userauth_agent_callback, s);
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

	if (mod_mp.len != sig_mp.len) {
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

static bool ssh2_userauth_want_user_input(PacketProtocolLayer *ppl)
{
    struct ssh2_userauth_state *s =
        container_of(ppl, struct ssh2_userauth_state, ppl);
    return s->want_user_input;
}

static void ssh2_userauth_got_user_input(PacketProtocolLayer *ppl)
{
    struct ssh2_userauth_state *s =
        container_of(ppl, struct ssh2_userauth_state, ppl);
    if (s->want_user_input)
        queue_idempotent_callback(&s->ppl.ic_process_queue);
}

static void ssh2_userauth_reconfigure(PacketProtocolLayer *ppl, Conf *conf)
{
    struct ssh2_userauth_state *s =
        container_of(ppl, struct ssh2_userauth_state, ppl);
    ssh_ppl_reconfigure(s->successor_layer, conf);
}

static void ssh2_userauth_antispoof_msg(
    struct ssh2_userauth_state *s, const char *msg)
{
    strbuf *sb = strbuf_new();
    if (seat_set_trust_status(s->ppl.seat, true)) {
        /*
         * If the seat can directly indicate that this message is
         * generated by the client, then we can just use the message
         * unmodified as an unspoofable header.
         */
        put_datapl(sb, ptrlen_from_asciz(msg));
    } else {
        /*
         * Otherwise, add enough padding around it that the server
         * wouldn't be able to mimic it within our line-length
         * constraint.
         */
        strbuf_catf(sb, "-- %s ", msg);
        while (sb->len < 78)
            put_byte(sb, '-');
    }
    put_datapl(sb, PTRLEN_LITERAL("\r\n"));
    seat_stderr_pl(s->ppl.seat, ptrlen_from_strbuf(sb));
    strbuf_free(sb);
}
