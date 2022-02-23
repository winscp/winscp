/*
 * SOCKS 5 proxy negotiation.
 */

#include "putty.h"
#include "network.h"
#include "proxy.h"
#include "socks.h"
#include "sshcr.h"

static inline const char *socks5_auth_name(unsigned char m)
{
    switch (m) {
      case SOCKS5_AUTH_NONE: return "none";
      case SOCKS5_AUTH_GSSAPI: return "GSSAPI";
      case SOCKS5_AUTH_PASSWORD: return "password";
      case SOCKS5_AUTH_CHAP: return "CHAP";
      default: return "unknown";
    }
}

static inline const char *socks5_response_text(unsigned char m)
{
    switch (m) {
      case SOCKS5_RESP_SUCCESS: return "success";
      case SOCKS5_RESP_FAILURE: return "unspecified failure";
      case SOCKS5_RESP_CONNECTION_NOT_ALLOWED_BY_RULESET:
        return "connection not allowed by ruleset";
      case SOCKS5_RESP_NETWORK_UNREACHABLE: return "network unreachable";
      case SOCKS5_RESP_HOST_UNREACHABLE: return "host unreachable";
      case SOCKS5_RESP_CONNECTION_REFUSED: return "connection refused";
      case SOCKS5_RESP_TTL_EXPIRED: return "TTL expired";
      case SOCKS5_RESP_COMMAND_NOT_SUPPORTED: return "command not supported";
      case SOCKS5_RESP_ADDRTYPE_NOT_SUPPORTED:
        return "address type not supported";
      default: return "unknown";
    }
}

typedef struct Socks5ProxyNegotiator {
    int crLine;
    strbuf *auth_methods_offered;
    unsigned char auth_method;
    unsigned n_chap_attrs;
    unsigned chap_attr, chap_attr_len;
    unsigned char chap_buf[256];
    strbuf *username, *password;
    prompts_t *prompts;
    int username_prompt_index, password_prompt_index;
    int response_addr_length;
    ProxyNegotiator pn;
} Socks5ProxyNegotiator;

static ProxyNegotiator *proxy_socks5_new(const ProxyNegotiatorVT *vt)
{
    Socks5ProxyNegotiator *s = snew(Socks5ProxyNegotiator);
    memset(s, 0, sizeof(*s));
    s->pn.vt = vt;
    s->auth_methods_offered = strbuf_new();
    s->username = strbuf_new();
    s->password = strbuf_new_nm();
    return &s->pn;
}

static void proxy_socks5_free(ProxyNegotiator *pn)
{
    Socks5ProxyNegotiator *s = container_of(pn, Socks5ProxyNegotiator, pn);
    strbuf_free(s->auth_methods_offered);
    strbuf_free(s->username);
    strbuf_free(s->password);
    if (s->prompts)
        free_prompts(s->prompts);
    smemclr(s, sizeof(*s));
    sfree(s);
}

static void proxy_socks5_process_queue(ProxyNegotiator *pn)
{
    Socks5ProxyNegotiator *s = container_of(pn, Socks5ProxyNegotiator, pn);

    crBegin(s->crLine);

    /*
     * SOCKS 5 initial client packet:
     *
     *   byte      version
     *   byte      number of available auth methods
     *   byte[]    that many bytes indicating auth types
     */

    put_byte(pn->output, SOCKS5_REQUEST_VERSION);

    strbuf_clear(s->auth_methods_offered);

    /*
     * We have two basic kinds of authentication to offer: none at
     * all, and password-based systems (whether the password is sent
     * in cleartext or proved via CHAP).
     *
     * We always offer 'none' as an option. We offer 'password' if we
     * either have a username and password already from the Conf, or
     * we have a Seat available to ask for them interactively. If
     * neither, we don't offer those options in the first place.
     */
    put_byte(s->auth_methods_offered, SOCKS5_AUTH_NONE);

    put_dataz(s->username, conf_get_str(pn->ps->conf, CONF_proxy_username));
    put_dataz(s->password, conf_get_str(pn->ps->conf, CONF_proxy_password));
    if (pn->itr || (s->username->len && s->password->len)) {
        if (socks5_chap_available)
            put_byte(s->auth_methods_offered, SOCKS5_AUTH_CHAP);

        put_byte(s->auth_methods_offered, SOCKS5_AUTH_PASSWORD);
    }

    put_byte(pn->output, s->auth_methods_offered->len);
    put_datapl(pn->output, ptrlen_from_strbuf(s->auth_methods_offered));

    crReturnV;

    /*
     * SOCKS 5 initial server packet:
     *
     *   byte      version
     *   byte      selected auth method, or SOCKS5_AUTH_REJECTED
     */
    {
        unsigned char data[2];
        crMaybeWaitUntilV(bufchain_try_fetch_consume(pn->input, data, 2));

        if (data[0] != SOCKS5_REPLY_VERSION) {
            pn->error = dupprintf("SOCKS proxy returned unexpected "
                                  "reply version %d (expected %d)",
                                  (int)data[0], SOCKS5_REPLY_VERSION);
            crStopV;
        }

        if (data[1] == SOCKS5_AUTH_REJECTED) {
            pn->error = dupstr("SOCKS server rejected every authentication "
                               "method we offered");
            crStopV;
        }

        {
            bool found = false;
            for (size_t i = 0; i < s->auth_methods_offered->len; i++)
                if (s->auth_methods_offered->u[i] == data[1]) {
                    found = true;
                    break;
                }

            if (!found) {
                pn->error = dupprintf("SOCKS server asked for auth method %d "
                                      "(%s), which we did not offer",
                                      (int)data[1], socks5_auth_name(data[1]));
                crStopV;
            }
        }

        s->auth_method = data[1];
    }

    /*
     * The 'none' auth option requires no further negotiation. If that
     * was the one we selected, go straight to making the connection.
     */
    if (s->auth_method == SOCKS5_AUTH_NONE)
        goto authenticated;

    /*
     * Otherwise, we're going to need a username and password, so this
     * is the moment to stop and ask for one if we don't already have
     * them.
     */
    if (pn->itr && (!s->username->len || !s->password->len)) {
        s->prompts = proxy_new_prompts(pn->ps);
        s->prompts->to_server = true;
        s->prompts->from_server = false;
        s->prompts->name = dupstr("SOCKS proxy authentication");
        if (!s->username->len) {
            s->username_prompt_index = s->prompts->n_prompts;
            add_prompt(s->prompts, dupstr("Proxy username: "), true);
        } else {
            s->username_prompt_index = -1;
        }
        if (!s->password->len) {
            s->password_prompt_index = s->prompts->n_prompts;
            add_prompt(s->prompts, dupstr("Proxy password: "), false);
        } else {
            s->password_prompt_index = -1;
        }

        while (true) {
            SeatPromptResult spr = seat_get_userpass_input(
                interactor_announce(pn->itr), s->prompts);
            if (spr.kind == SPRK_OK) {
                break;
            } else if (spr_is_abort(spr)) {
                proxy_spr_abort(pn, spr);
                crStopV;
            }
            crReturnV;
        }

        if (s->username_prompt_index != -1) {
            strbuf_clear(s->username);
            put_dataz(s->username,
                      prompt_get_result_ref(
                          s->prompts->prompts[s->username_prompt_index]));
        }

        if (s->password_prompt_index != -1) {
            strbuf_clear(s->password);
            put_dataz(s->password,
                      prompt_get_result_ref(
                          s->prompts->prompts[s->password_prompt_index]));
        }

        free_prompts(s->prompts);
        s->prompts = NULL;
    }

    /*
     * Now process the different auth methods that will use that
     * username and password. Note we can't do this using the natural
     * idiom of a switch statement, because there are crReturns inside
     * some cases.
     */
    if (s->auth_method == SOCKS5_AUTH_PASSWORD) {
        /*
         * SOCKS 5 password auth packet:
         *
         *   byte      version
         *   pstring   username
         *   pstring   password
         */
        put_byte(pn->output, SOCKS5_AUTH_PASSWORD_VERSION);
        if (!put_pstring(pn->output, s->username->s)) {
            pn->error = dupstr("SOCKS 5 authentication cannot support "
                               "usernames longer than 255 chars");
            crStopV;
        }
        if (!put_pstring(pn->output, s->password->s)) {
            pn->error = dupstr("SOCKS 5 authentication cannot support "
                               "passwords longer than 255 chars");
            crStopV;
        }

        /*
         * SOCKS 5 password reply packet:
         *
         *   byte      version
         *   byte      0 for success, >0 for failure
         */
        unsigned char data[2];
        crMaybeWaitUntilV(bufchain_try_fetch_consume(pn->input, data, 2));

        if (data[0] != SOCKS5_AUTH_PASSWORD_VERSION) {
            pn->error = dupprintf(
                "SOCKS 5 password reply had version number %d (expected "
                "%d)", (int)data[0], SOCKS5_AUTH_PASSWORD_VERSION);
            crStopV;
        }

        if (data[1] != 0) {
            pn->error = dupstr("SOCKS 5 server rejected our password");
            crStopV;
        }
    } else if (s->auth_method == SOCKS5_AUTH_CHAP) {
        assert(socks5_chap_available);

        /*
         * All CHAP packets, in both directions, have the same
         * overall format:
         *
         *   byte      version
         *   byte      number of attributes
         *
         * and then for each attribute:
         *
         *   byte      attribute type
         *   byte      length
         *   byte[]    that many bytes of payload
         *
         * In the initial outgoing packet we send two attributes:
         * the list of supported algorithm names, and the
         * username.
         *
         * (It's possible that we ought to delay sending the
         * username until the second packet, in case the proxy
         * sent back an attribute indicating which character set
         * it would like us to use.)
         */
        put_byte(pn->output, SOCKS5_AUTH_CHAP_VERSION);
        put_byte(pn->output, 2);   /* number of attributes */

        put_byte(pn->output, SOCKS5_AUTH_CHAP_ATTR_ALGLIST);
        put_byte(pn->output, 1);   /* string length */
        put_byte(pn->output, SOCKS5_AUTH_CHAP_ALG_HMACMD5);

        /* Second attribute: username */
        {
            put_byte(pn->output, SOCKS5_AUTH_CHAP_ATTR_USERNAME);
            if (!put_pstring(pn->output, s->username->s)) {
                pn->error = dupstr(
                    "SOCKS 5 CHAP authentication cannot support "
                    "usernames longer than 255 chars");
                crStopV;
            }
        }

        while (true) {
            /*
             * Process a CHAP response packet, which has the same
             * overall format as the outgoing packet shown above.
             */
            unsigned char data[2];
            crMaybeWaitUntilV(bufchain_try_fetch_consume(
                                  pn->input, data, 2));
            if (data[0] != SOCKS5_AUTH_CHAP_VERSION) {
                pn->error = dupprintf(
                    "SOCKS 5 CHAP reply had version number %d (expected "
                    "%d)", (int)data[0], SOCKS5_AUTH_CHAP_VERSION);
                crStopV;
            }

            s->n_chap_attrs = data[1];
            if (s->n_chap_attrs == 0) {
                /*
                 * If we receive a CHAP packet containing no
                 * attributes, then we have nothing we didn't have
                 * before, and can't make further progress.
                 */
                pn->error = dupprintf(
                    "SOCKS 5 CHAP reply sent no attributes");
                crStopV;
            }
            while (s->n_chap_attrs-- > 0) {
                unsigned char data[2];
                crMaybeWaitUntilV(bufchain_try_fetch_consume(
                                      pn->input, data, 2));
                s->chap_attr = data[0];
                s->chap_attr_len = data[1];
                crMaybeWaitUntilV(bufchain_try_fetch_consume(
                                      pn->input, s->chap_buf, s->chap_attr_len));

                if (s->chap_attr == SOCKS5_AUTH_CHAP_ATTR_STATUS) {
                    if (s->chap_attr_len == 1 && s->chap_buf[0] == 0) {
                        /* Status 0 means success: we are authenticated! */
                        goto authenticated;
                    } else {
                        pn->error = dupstr(
                            "SOCKS 5 CHAP authentication failed");
                        crStopV;
                    }
                } else if (s->chap_attr==SOCKS5_AUTH_CHAP_ATTR_CHALLENGE) {
                    /* The CHAP challenge string. Send the response */
                    strbuf *response = chap_response(
                        make_ptrlen(s->chap_buf, s->chap_attr_len),
                        ptrlen_from_strbuf(s->password));

                    put_byte(pn->output, SOCKS5_AUTH_CHAP_VERSION);
                    put_byte(pn->output, 1); /* number of attributes */
                    put_byte(pn->output, SOCKS5_AUTH_CHAP_ATTR_RESPONSE);
                    put_byte(pn->output, response->len);
                    put_datapl(pn->output, ptrlen_from_strbuf(response));

                    strbuf_free(response);
                } else {
                    /* ignore all other attributes */
                }
            }
        }
    } else {
        unreachable("bad auth method in SOCKS 5 negotiation");
    }

  authenticated:

    /*
     * SOCKS 5 connection command:
     *
     *   byte      version
     *   byte      command
     *   byte      reserved (send as zero)
     *   byte      address type
     *   byte[]    address, with variable size (see below)
     *   uint16    port
     */
    put_byte(pn->output, SOCKS5_REPLY_VERSION);
    put_byte(pn->output, SOCKS_CMD_CONNECT);
    put_byte(pn->output, 0);   /* reserved byte */

    switch (sk_addrtype(pn->ps->remote_addr)) {
      case ADDRTYPE_IPV4: {
        /* IPv4: address is 4 raw bytes */
        put_byte(pn->output, SOCKS5_ADDR_IPV4);
        char buf[4];
        sk_addrcopy(pn->ps->remote_addr, buf);
        put_data(pn->output, buf, sizeof(buf));
        break;
      }
      case ADDRTYPE_IPV6: {
        /* IPv6: address is 16 raw bytes */
        put_byte(pn->output, SOCKS5_ADDR_IPV6);
        char buf[16];
        sk_addrcopy(pn->ps->remote_addr, buf);
        put_data(pn->output, buf, sizeof(buf));
        break;
      }
      case ADDRTYPE_NAME: {
        /* Hostname: address is a pstring (Pascal-style string,
         * unterminated but with a one-byte prefix length) */
        put_byte(pn->output, SOCKS5_ADDR_HOSTNAME);
        char hostname[512];
        sk_getaddr(pn->ps->remote_addr, hostname, lenof(hostname));
        if (!put_pstring(pn->output, hostname)) {
            pn->error = dupstr(
                "SOCKS 5 cannot support host names longer than 255 chars");
            crStopV;
        }
        break;
      }
      default:
        unreachable("Unexpected addrtype in SOCKS 5 proxy");
    }

    put_uint16(pn->output, pn->ps->remote_port);
    crReturnV;

    /*
     * SOCKS 5 connection response:
     *
     *   byte      version
     *   byte      status
     *   byte      reserved
     *   byte      address type
     *   byte[]    address bound to (same formats as in connection request)
     *   uint16    port
     *
     * We read the first four bytes and then decide what to do next.
     */
    {
        unsigned char data[4];
        crMaybeWaitUntilV(bufchain_try_fetch_consume(pn->input, data, 4));

        if (data[0] != SOCKS5_REPLY_VERSION) {
            pn->error = dupprintf("SOCKS proxy returned unexpected "
                                  "reply version %d (expected %d)",
                                  (int)data[0], SOCKS5_REPLY_VERSION);
            crStopV;
        }

        if (data[1] != SOCKS5_RESP_SUCCESS) {
            pn->error = dupprintf("SOCKS proxy failed to connect, error %d "
                                  "(%s)", (int)data[1],
                                  socks5_response_text(data[1]));
            crStopV;
        }

        /*
         * Process each address type to find out the size of the rest
         * of the packet. Note we can't do this using the natural
         * idiom of a switch statement, because there are crReturns
         * inside some cases.
         */
        if (data[3] == SOCKS5_ADDR_IPV4) {
            s->response_addr_length = 4;
        } else if (data[3] == SOCKS5_ADDR_IPV6) {
            s->response_addr_length = 16;
        } else if (data[3] == SOCKS5_ADDR_HOSTNAME) {
            /* Read the hostname length byte to find out how much to read */
            unsigned char len; 
            crMaybeWaitUntilV(bufchain_try_fetch_consume(pn->input, &len, 1));
            s->response_addr_length = len;
            break;
        } else {
            pn->error = dupprintf("SOCKS proxy response included unknown "
                                  "address type %d", (int)data[3]);
            crStopV;
        }

        /* Read and ignore the address and port fields */
        crMaybeWaitUntilV(bufchain_try_consume(
                              pn->input, s->response_addr_length + 2));
    }

    /* And we're done! */
    pn->done = true;
    crFinishV;
}

const struct ProxyNegotiatorVT socks5_proxy_negotiator_vt = {
    .new = proxy_socks5_new,
    .free = proxy_socks5_free,
    .process_queue = proxy_socks5_process_queue,
    .type = "SOCKS 5",
};
