/*
 * HTTP CONNECT proxy negotiation.
 */

#include "putty.h"
#include "network.h"
#include "proxy.h"
#include "sshcr.h"

static bool read_line(bufchain *input, strbuf *output, bool is_header)
{
    char c;

    while (bufchain_try_fetch(input, &c, 1)) {
        if (is_header && output->len > 0 &&
            output->s[output->len - 1] == '\n') {
            /*
             * A newline terminates the header, provided we're sure it
             * is _not_ followed by a space or a tab.
             */
            if (c != ' ' && c != '\t')
                goto done;  /* we have a complete header line */
        } else {
            put_byte(output, c);
            bufchain_consume(input, 1);

            if (!is_header && output->len > 0 &&
                output->s[output->len - 1] == '\n') {
                /* If we're looking for just a line, not an HTTP
                 * header, then any newline terminates it. */
                goto done;
            }
        }
    }

    return false;

  done:
    strbuf_chomp(output, '\n');
    strbuf_chomp(output, '\r');
    return true;
}

/* Types of HTTP authentication, in preference order. */
typedef enum HttpAuthType {
    AUTH_ERROR, /* if an HttpAuthDetails was never satisfactorily filled in */
    AUTH_NONE,  /* if no auth header is seen, assume no auth required */
    AUTH_BASIC, /* username + password sent in clear (only keyless base64) */
    AUTH_DIGEST, /* cryptographic hash, most preferred if available */
} HttpAuthType;

typedef struct HttpAuthDetails {
    HttpAuthType auth_type;
    bool digest_nonce_was_stale;
    HttpDigestHash digest_hash;
    strbuf *realm, *nonce, *opaque, *error;
    bool got_opaque;
    bool hash_username;
} HttpAuthDetails;

typedef struct HttpProxyNegotiator {
    int crLine;
    strbuf *response, *header, *token;
    int http_status_pos;
    size_t header_pos;
    strbuf *username, *password;
    int http_status;
    bool connection_close;
    HttpAuthDetails *next_auth;
    bool try_auth_from_conf;
    strbuf *uri;
    uint32_t nonce_count;
    prompts_t *prompts;
    int username_prompt_index, password_prompt_index;
    size_t content_length, chunk_length;
    bool chunked_transfer;
    ProxyNegotiator pn;
} HttpProxyNegotiator;

static inline HttpAuthDetails *auth_error(HttpAuthDetails *d,
                                          const char *fmt, ...)
{
    d->auth_type = AUTH_ERROR;
    put_fmt(d->error, "Unable to parse auth header from HTTP proxy");
    if (fmt) {
        va_list ap;
        va_start(ap, fmt);
        put_datalit(d->error, ": ");
        put_fmtv(d->error, fmt, ap);
        va_end(ap);
    }
    return d;
}

static HttpAuthDetails *http_auth_details_new(void)
{
    HttpAuthDetails *d = snew(HttpAuthDetails);
    memset(d, 0, sizeof(*d));
    d->realm = strbuf_new();
    d->nonce = strbuf_new();
    d->opaque = strbuf_new();
    d->error = strbuf_new();
    return d;
}

static void http_auth_details_free(HttpAuthDetails *d)
{
    strbuf_free(d->realm);
    strbuf_free(d->nonce);
    strbuf_free(d->opaque);
    strbuf_free(d->error);
    sfree(d);
}

static ProxyNegotiator *proxy_http_new(const ProxyNegotiatorVT *vt)
{
    HttpProxyNegotiator *s = snew(HttpProxyNegotiator);
    memset(s, 0, sizeof(*s));
    s->pn.vt = vt;
    s->response = strbuf_new();
    s->header = strbuf_new();
    s->token = strbuf_new();
    s->username = strbuf_new();
    s->password = strbuf_new_nm();
    s->uri = strbuf_new();
    s->nonce_count = 0;
    /*
     * Always start with a CONNECT request containing no auth. If the
     * proxy rejects that, it will tell us what kind of auth it would
     * prefer.
     */
    s->next_auth = http_auth_details_new();
    s->next_auth->auth_type = AUTH_NONE;
    return &s->pn;
}

static void proxy_http_free(ProxyNegotiator *pn)
{
    HttpProxyNegotiator *s = container_of(pn, HttpProxyNegotiator, pn);
    strbuf_free(s->response);
    strbuf_free(s->header);
    strbuf_free(s->token);
    strbuf_free(s->username);
    strbuf_free(s->password);
    strbuf_free(s->uri);
    http_auth_details_free(s->next_auth);
    if (s->prompts)
        free_prompts(s->prompts);
    sfree(s);
}

#define HTTP_HEADER_LIST(X) \
    X(HDR_CONNECTION, "Connection") \
    X(HDR_CONTENT_LENGTH, "Content-Length") \
    X(HDR_TRANSFER_ENCODING, "Transfer-Encoding") \
    X(HDR_PROXY_AUTHENTICATE, "Proxy-Authenticate") \
    X(HDR_PROXY_CONNECTION, "Proxy-Connection") \
    /* end of list */

typedef enum HttpHeader {
    #define ENUM_DEF(id, string) id,
    HTTP_HEADER_LIST(ENUM_DEF)
    #undef ENUM_DEF
    HDR_UNKNOWN
} HttpHeader;

static inline bool is_whitespace(char c)
{
    return (c == ' ' || c == '\t' || c == '\n');
}

static inline bool is_separator(char c)
{
    return (c == '(' || c == ')' || c == '<' || c == '>' || c == '@' ||
            c == ',' || c == ';' || c == ':' || c == '\\' || c == '"' ||
            c == '/' || c == '[' || c == ']' || c == '?' || c == '=' ||
            c == '{' || c == '}');
}

#define HTTP_SEPARATORS

static bool get_end_of_header(HttpProxyNegotiator *s)
{
    size_t pos = s->header_pos;

    while (pos < s->header->len && is_whitespace(s->header->s[pos]))
        pos++;

    if (pos == s->header->len) {
        s->header_pos = pos;
        return true;
    }

    return false;
}

static bool get_token(HttpProxyNegotiator *s)
{
    size_t pos = s->header_pos;

    while (pos < s->header->len && is_whitespace(s->header->s[pos]))
        pos++;

    if (pos == s->header->len)
        return false;                  /* end of string */

    if (is_separator(s->header->s[pos]))
        return false;

    strbuf_clear(s->token);
    while (pos < s->header->len &&
           !is_whitespace(s->header->s[pos]) &&
           !is_separator(s->header->s[pos]))
        put_byte(s->token, s->header->s[pos++]);

    s->header_pos = pos;
    return true;
}

static bool get_separator(HttpProxyNegotiator *s, char sep)
{
    size_t pos = s->header_pos;

    while (pos < s->header->len && is_whitespace(s->header->s[pos]))
        pos++;

    if (pos == s->header->len)
        return false;                  /* end of string */

    if (s->header->s[pos] != sep)
        return false;

    s->header_pos = ++pos;
    return true;
}

static bool get_quoted_string(HttpProxyNegotiator *s)
{
    size_t pos = s->header_pos;

    while (pos < s->header->len && is_whitespace(s->header->s[pos]))
        pos++;

    if (pos == s->header->len)
        return false;                  /* end of string */

    if (s->header->s[pos] != '"')
        return false;
    pos++;

    strbuf_clear(s->token);
    while (pos < s->header->len && s->header->s[pos] != '"') {
        if (s->header->s[pos] == '\\') {
            /* Backslash makes the next char literal, even if it's " or \ */
            pos++;
            if (pos == s->header->len)
                return false;          /* unexpected end of string */
        }
        put_byte(s->token, s->header->s[pos++]);
    }

    if (pos == s->header->len)
        return false;                  /* no closing quote */
    pos++;

    s->header_pos = pos;
    return true;
}

static HttpAuthDetails *parse_http_auth_header(HttpProxyNegotiator *s)
{
    HttpAuthDetails *d = http_auth_details_new();

    /* Default hash for HTTP Digest is MD5, if none specified explicitly */
    d->digest_hash = HTTP_DIGEST_MD5;

    if (!get_token(s))
        return auth_error(d, "parse error");

    if (!stricmp(s->token->s, "Basic")) {
        /* For Basic authentication, we don't need anything else. The
         * realm string is not required for the protocol. */
        d->auth_type = AUTH_BASIC;
        return d;
    }

    if (!stricmp(s->token->s, "Digest")) {
        /* Parse all the additional parts of the Digest header. */
        if (!http_digest_available)
            return auth_error(d, "Digest authentication not supported");

        /* Parse the rest of the Digest header */
        while (true) {
            if (!get_token(s))
                return auth_error(d, "parse error in Digest header");

            if (!stricmp(s->token->s, "realm")) {
                if (!get_separator(s, '=') ||
                    !get_quoted_string(s))
                    return auth_error(d, "parse error in Digest realm field");
                put_datapl(d->realm, ptrlen_from_strbuf(s->token));
            } else if (!stricmp(s->token->s, "nonce")) {
                if (!get_separator(s, '=') ||
                    !get_quoted_string(s))
                    return auth_error(d, "parse error in Digest nonce field");
                put_datapl(d->nonce, ptrlen_from_strbuf(s->token));
            } else if (!stricmp(s->token->s, "opaque")) {
                if (!get_separator(s, '=') ||
                    !get_quoted_string(s))
                    return auth_error(d, "parse error in Digest opaque field");
                put_datapl(d->opaque,
                           ptrlen_from_strbuf(s->token));
                d->got_opaque = true;
            } else if (!stricmp(s->token->s, "stale")) {
                if (!get_separator(s, '=') ||
                    !get_token(s))
                    return auth_error(d, "parse error in Digest stale field");
                d->digest_nonce_was_stale = !stricmp(
                    s->token->s, "true");
            } else if (!stricmp(s->token->s, "userhash")) {
                if (!get_separator(s, '=') ||
                    !get_token(s))
                    return auth_error(d, "parse error in Digest userhash "
                                      "field");
                d->hash_username = !stricmp(s->token->s, "true");
            } else if (!stricmp(s->token->s, "algorithm")) {
                if (!get_separator(s, '=') ||
                    (!get_token(s) && !get_quoted_string(s)))
                    return auth_error(d, "parse error in Digest algorithm "
                                      "field");
                bool found = false;
                size_t i;

                for (i = 0; i < N_HTTP_DIGEST_HASHES; i++) {
                    if (!stricmp(s->token->s, httphashnames[i])) {
                        found = true;
                        break;
                    }
                }

                if (!found) {
                    /* We don't even recognise the name */
                    return auth_error(d, "Digest hash algorithm '%s' not "
                                      "recognised", s->token->s);
                }

                if (!httphashaccepted[i]) {
                    /* We do recognise the name but we
                     * don't like it (see comment in cproxy.h) */
                    return auth_error(d, "Digest hash algorithm '%s' not "
                                      "supported", s->token->s);
                }

                d->digest_hash = i;
            } else if (!stricmp(s->token->s, "qop")) {
                if (!get_separator(s, '=') ||
                    !get_quoted_string(s))
                    return auth_error(d, "parse error in Digest qop field");
                if (stricmp(s->token->s, "auth"))
                    return auth_error(d, "quality-of-protection type '%s' not "
                                      "supported", s->token->s);
            } else {
                /* Ignore any other auth-param */
                if (!get_separator(s, '=') ||
                    (!get_quoted_string(s) && !get_token(s)))
                    return auth_error(d, "parse error in Digest header");
            }

            if (get_end_of_header(s))
                break;
            if (!get_separator(s, ','))
                return auth_error(d, "parse error in Digest header");
        }
        d->auth_type = AUTH_DIGEST;
        return d;
    }

    return auth_error(d, "authentication type '%s' not supported",
                      s->token->s);
}

static void proxy_http_process_queue(ProxyNegotiator *pn)
{
    HttpProxyNegotiator *s = container_of(pn, HttpProxyNegotiator, pn);

    crBegin(s->crLine);

    /*
     * Initialise our username and password strbufs from the Conf.
     */
    put_dataz(s->username, conf_get_str(pn->ps->conf, CONF_proxy_username));
    put_dataz(s->password, conf_get_str(pn->ps->conf, CONF_proxy_password));
    if (s->username->len || s->password->len)
        s->try_auth_from_conf = true;

    /*
     * Set up the host:port string we're trying to connect to, also
     * used as the URI string in HTTP Digest auth.
     */
    {
        char dest[512];
        sk_getaddr(pn->ps->remote_addr, dest, lenof(dest));
        put_fmt(s->uri, "%s:%d", dest, pn->ps->remote_port);
    }

    while (true) {
        /*
         * Standard prefix for the HTTP CONNECT request.
         */
        put_fmt(pn->output,
                "CONNECT %s HTTP/1.1\r\n"
                "Host: %s\r\n", s->uri->s, s->uri->s);

        /*
         * Add an auth header, if we're planning to this time round.
         */
        if (s->next_auth->auth_type == AUTH_BASIC) {
            put_datalit(pn->output, "Proxy-Authorization: Basic ");

            strbuf *base64_input = strbuf_new_nm();
            put_datapl(base64_input, ptrlen_from_strbuf(s->username));
            put_byte(base64_input, ':');
            put_datapl(base64_input, ptrlen_from_strbuf(s->password));

            char base64_output[4];
            for (size_t i = 0, e = base64_input->len; i < e; i += 3) {
                base64_encode_atom(base64_input->u + i,
                                   e-i > 3 ? 3 : e-i, base64_output);
                put_data(pn->output, base64_output, 4);
            }
            strbuf_free(base64_input);
            smemclr(base64_output, sizeof(base64_output));
            put_datalit(pn->output, "\r\n");
        } else if (s->next_auth->auth_type == AUTH_DIGEST) {
            put_datalit(pn->output, "Proxy-Authorization: Digest ");

            /* If we have a fresh nonce, reset the
             * nonce count. Otherwise, keep incrementing it. */
            if (!ptrlen_eq_ptrlen(ptrlen_from_strbuf(s->token),
                                  ptrlen_from_strbuf(s->next_auth->nonce)))
                s->nonce_count = 0;

            http_digest_response(BinarySink_UPCAST(pn->output),
                                 ptrlen_from_strbuf(s->username),
                                 ptrlen_from_strbuf(s->password),
                                 ptrlen_from_strbuf(s->next_auth->realm),
                                 PTRLEN_LITERAL("CONNECT"),
                                 ptrlen_from_strbuf(s->uri),
                                 PTRLEN_LITERAL("auth"),
                                 ptrlen_from_strbuf(s->next_auth->nonce),
                                 (s->next_auth->got_opaque ?
                                  ptrlen_from_strbuf(s->next_auth->opaque) :
                                  make_ptrlen(NULL, 0)),
                                 ++s->nonce_count, s->next_auth->digest_hash,
                                 s->next_auth->hash_username);
            put_datalit(pn->output, "\r\n");
        }

        /*
         * Blank line to terminate the HTTP request.
         */
        put_datalit(pn->output, "\r\n");
        crReturnV;

        s->content_length = 0;
        s->chunked_transfer = false;
        s->connection_close = false;

        /*
         * Read and parse the HTTP status line, and check if it's a 2xx
         * for success.
         */
        strbuf_clear(s->response);
        crMaybeWaitUntilV(read_line(pn->input, s->response, false));
        {
            int maj_ver, min_ver, n_scanned;
            n_scanned = sscanf(
                s->response->s, "HTTP/%d.%d %n%d",
                &maj_ver, &min_ver, &s->http_status_pos, &s->http_status);

            if (n_scanned < 3) {
                pn->error = dupstr("HTTP response was absent or malformed");
                crStopV;
            }

            if (maj_ver < 1 || (maj_ver == 1 && min_ver < 1)) {
                /* Before HTTP/1.1, connections close by default */
                s->connection_close = true;
            }
        }

        if (s->http_status == 407) {
            /*
             * If this is going to be an auth request, we expect to
             * see at least one Proxy-Authorization header offering us
             * auth options. Start by preloading s->next_auth with a
             * fallback error message, which will be used if nothing
             * better is available.
             */
            http_auth_details_free(s->next_auth);
            s->next_auth = http_auth_details_new();
            auth_error(s->next_auth, "no Proxy-Authorization header seen in "
                       "HTTP 407 Proxy Authentication Required response");
        }

        /*
         * Read the HTTP response header section.
         */
        do {
            strbuf_clear(s->header);
            crMaybeWaitUntilV(read_line(pn->input, s->header, true));
            s->header_pos = 0;

            if (!get_token(s)) {
                /* Possibly we ought to panic if we see an HTTP header
                 * we can't make any sense of at all? But whatever,
                 * ignore it and hope the next one makes more sense */
                continue;
            }

            /* Parse the header name */
            HttpHeader hdr = HDR_UNKNOWN;
            {
                #define CHECK_HEADER(id, string) \
                    if (!stricmp(s->token->s, string)) hdr = id;
                HTTP_HEADER_LIST(CHECK_HEADER);
                #undef CHECK_HEADER
            }

            if (!get_separator(s, ':'))
                continue;

            if (hdr == HDR_CONTENT_LENGTH) {
                if (!get_token(s))
                    continue;
                s->content_length = strtoumax(s->token->s, NULL, 10);
            } else if (hdr == HDR_TRANSFER_ENCODING) {
                /*
                 * The Transfer-Encoding header value should be a
                 * comma-separated list of keywords including
                 * "chunked", "deflate" and "gzip". We parse it in the
                 * most superficial way, by just looking for "chunked"
                 * and ignoring everything else.
                 *
                 * It's OK to do that because we're not actually
                 * _using_ the error document - we only have to skip
                 * over it to find the end of the HTTP response. So we
                 * don't care if it's gzipped or not.
                 */
                while (get_token(s)) {
                    if (!stricmp(s->token->s, "chunked"))
                        s->chunked_transfer = true;
                }
            } else if (hdr == HDR_CONNECTION ||
                       hdr == HDR_PROXY_CONNECTION) {
                if (!get_token(s))
                    continue;
                if (!stricmp(s->token->s, "close"))
                    s->connection_close = true;
                else if (!stricmp(s->token->s, "keep-alive"))
                    s->connection_close = false;
            } else if (hdr == HDR_PROXY_AUTHENTICATE) {
                HttpAuthDetails *auth = parse_http_auth_header(s);

                /*
                 * See if we prefer this set of auth details to the
                 * previous one we had (either from a previous auth
                 * header, or the fallback when no auth header is
                 * provided at all).
                 */
                bool change;

                if (auth->auth_type != s->next_auth->auth_type) {
                    /* Use the preference order implied by the enum */
                    change = auth->auth_type > s->next_auth->auth_type;
                } else if (auth->auth_type == AUTH_DIGEST &&
                           auth->digest_hash != s->next_auth->digest_hash) {
                    /* Choose based on the hash functions */
                    change = auth->digest_hash > s->next_auth->digest_hash;
                } else {
                    /*
                     * If in doubt, go with the later one of the
                     * headers.
                     *
                     * The main reason for this is so that an error in
                     * interpreting an auth header will supersede the
                     * default error we preload saying 'no header
                     * found', because that would be a particularly
                     * bad error to report if there _was_ one.
                     *
                     * But we're in a tie-breaking situation by now,
                     * so there's no other reason to choose - we might
                     * as well apply the same policy everywhere else
                     * too.
                     */
                    change = true;
                }

                if (change) {
                    http_auth_details_free(s->next_auth);
                    s->next_auth = auth;
                } else {
                    http_auth_details_free(auth);
                }
            }
        } while (s->header->len > 0);

        /* Read and ignore the entire response document */
        if (!s->chunked_transfer) {
            /* Simple approach: read exactly Content-Length bytes */
            crMaybeWaitUntilV(bufchain_try_consume(
                                  pn->input, s->content_length));
        } else {
            /* Chunked transfer: read a sequence of
             * <hex length>\r\n<data>\r\n chunks, terminating in one with
             * zero length */
            do {
                /*
                 * Expect a chunk length
                 */
                s->chunk_length = 0;
                while (true) {
                    char c;
                    crMaybeWaitUntilV(bufchain_try_fetch_consume(
                                          pn->input, &c, 1));
                    if (c == '\r') {
                        continue;
                    } else if (c == '\n') {
                        break;
                    } else if ('0' <= c && c <= '9') {
                        s->chunk_length = s->chunk_length*16 + (c-'0');
                    } else if ('A' <= c && c <= 'F') {
                        s->chunk_length = s->chunk_length*16 + (c-'A'+10);
                    } else if ('a' <= c && c <= 'f') {
                        s->chunk_length = s->chunk_length*16 + (c-'a'+10);
                    } else {
                        pn->error = dupprintf(
                            "Received bad character 0x%02X in chunk length "
                            "during HTTP chunked transfer encoding",
                            (unsigned)(unsigned char)c);
                        crStopV;
                    }
                }

                /*
                 * Expect that many bytes of chunked data
                 */
                crMaybeWaitUntilV(bufchain_try_consume(
                                      pn->input, s->chunk_length));

                /* Now expect \r\n */
                {
                    char buf[2];
                    crMaybeWaitUntilV(bufchain_try_fetch_consume(
                                          pn->input, buf, 2));
                    if (memcmp(buf, "\r\n", 2)) {
                        pn->error = dupprintf(
                            "Missing CRLF after chunk "
                            "during HTTP chunked transfer encoding");
                        crStopV;
                    }
                }
            } while (s->chunk_length);
        }

        if (200 <= s->http_status && s->http_status < 300) {
            /* Any 2xx HTTP response means we're done */
            goto authenticated;
        } else if (s->http_status == 407) {
            /* 407 is Proxy Authentication Required, which we may be
             * able to do something about. */
            if (s->connection_close) {
                /* If we got 407 + connection closed, reconnect before
                 * sending our next request. */
                pn->reconnect = true;
            }

            /* If the best we can do is report some kind of error from
             * a Proxy-Auth header (or an error saying there wasn't
             * one at all), and no successful parsing of an auth
             * header superseded that, then just throw that error and
             * die. */
            if (s->next_auth->auth_type == AUTH_ERROR) {
                pn->error = dupstr(s->next_auth->error->s);
                crStopV;
            }

            /* If we have auth details from the Conf and haven't tried
             * them yet, that's our first step. */
            if (s->try_auth_from_conf) {
                s->try_auth_from_conf = false;
                continue;
            }

            /* If the server sent us stale="true" in a Digest auth
             * header, that means we _don't_ need to request a new
             * password yet; just try again with the existing details
             * and the fresh nonce it sent us. */
            if (s->next_auth->digest_nonce_was_stale)
                continue;

            /* Either we never had a password in the first place, or
             * the one we already presented was rejected. We can only
             * proceed from here if we have a way to ask the user
             * questions. */
            if (!pn->itr) {
                pn->error = dupprintf("HTTP proxy requested authentication "
                                      "which we do not have");
                crStopV;
            }

            /*
             * Send some prompts to the user. We'll assume the
             * password is always required (since it's just been
             * rejected, even if we did send one before), and we'll
             * prompt for the username only if we don't have one from
             * the Conf.
             */
            s->prompts = proxy_new_prompts(pn->ps);
            s->prompts->to_server = true;
            s->prompts->from_server = false;
            s->prompts->name = dupstr("HTTP proxy authentication");
            if (!s->username->len) {
                s->username_prompt_index = s->prompts->n_prompts;
                add_prompt(s->prompts, dupstr("Proxy username: "), true);
            } else {
                s->username_prompt_index = -1;
            }

            s->password_prompt_index = s->prompts->n_prompts;
            add_prompt(s->prompts, dupstr("Proxy password: "), false);

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

            strbuf_clear(s->password);
            put_dataz(s->password,
                      prompt_get_result_ref(
                          s->prompts->prompts[s->password_prompt_index]));

            free_prompts(s->prompts);
            s->prompts = NULL;
        } else {
            /* Any other HTTP response is treated as permanent failure */
            pn->error = dupprintf("HTTP response %s",
                                  s->response->s + s->http_status_pos);
            crStopV;
        }
    }

  authenticated:
    /*
     * Success! Hand over to the main connection.
     */
    pn->done = true;

    crFinishV;
}

const struct ProxyNegotiatorVT http_proxy_negotiator_vt = {
    .new = proxy_http_new,
    .free = proxy_http_free,
    .process_queue = proxy_http_process_queue,
    .type = "HTTP",
};
