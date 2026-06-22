/*
 * Routines to do cryptographic interaction with proxies in PuTTY.
 * This is in a separate module from proxy.c, so that it can be
 * conveniently removed in PuTTYtel by replacing this module with
 * the stub version nocproxy.c.
 */

#include <assert.h>
#include <ctype.h>
#include <string.h>

#include "putty.h"
#include "ssh.h" /* For MD5 support */
#include "network.h"
#include "proxy.h"
#include "marshal.h"

const bool socks5_chap_available = true;
const bool http_digest_available = true;

strbuf *chap_response(ptrlen challenge, ptrlen password)
{
    strbuf *sb = strbuf_new_nm();
    const ssh2_macalg *alg = &ssh_hmac_md5;
    enable_dit(); /* just in case main() forgot */
    mac_simple(alg, password, challenge, strbuf_append(sb, alg->len));
    return sb;
}

static void BinarySink_put_hex_data(BinarySink *bs, const void *vptr,
                                    size_t len)
{
    const unsigned char *p = (const unsigned char *)vptr;
    const char *hexdigits = "0123456789abcdef";
    while (len-- > 0) {
        unsigned c = *p++;
        put_byte(bs, hexdigits[0xF & (c >> 4)]);
        put_byte(bs, hexdigits[0xF & (c     )]);
    }
}

#define put_hex_data(bs, p, len) \
    BinarySink_put_hex_data(BinarySink_UPCAST(bs), p, len)

const char *const httphashnames[] = {
    #define DECL_ARRAY(id, str, alg, bits, accepted) str,
    HTTP_DIGEST_HASHES(DECL_ARRAY)
    #undef DECL_ARRAY
};

const bool httphashaccepted[] = {
    #define DECL_ARRAY(id, str, alg, bits, accepted) accepted,
    HTTP_DIGEST_HASHES(DECL_ARRAY)
    #undef DECL_ARRAY
};

static const ssh_hashalg *const httphashalgs[] = {
    #define DECL_ARRAY(id, str, alg, bits, accepted) alg,
    HTTP_DIGEST_HASHES(DECL_ARRAY)
    #undef DECL_ARRAY
};
static const size_t httphashlengths[] = {
    #define DECL_ARRAY(id, str, alg, bits, accepted) bits/8,
    HTTP_DIGEST_HASHES(DECL_ARRAY)
    #undef DECL_ARRAY
};

void http_digest_response(BinarySink *bs, ptrlen username, ptrlen password,
                          ptrlen realm, ptrlen method, ptrlen uri, ptrlen qop,
                          ptrlen nonce, ptrlen opaque, uint32_t nonce_count,
                          HttpDigestHash hash, bool hash_username)
{
    unsigned char a1hash[MAX_HASH_LEN];
    unsigned char a2hash[MAX_HASH_LEN];
    unsigned char rsphash[MAX_HASH_LEN];
    const ssh_hashalg *alg = httphashalgs[hash];
    size_t hashlen = httphashlengths[hash];

    enable_dit(); /* just in case main() forgot */

    { // WINSCP
    unsigned char ncbuf[4];
    PUT_32BIT_MSB_FIRST(ncbuf, nonce_count);

    { // WINSCP
    unsigned char client_nonce_raw[33];
    random_read(client_nonce_raw, lenof(client_nonce_raw));
    { // WINSCP
    char client_nonce_base64[lenof(client_nonce_raw) / 3 * 4];
    { // WINSCP
    unsigned i;
    for (i = 0; i < lenof(client_nonce_raw)/3; i++)
        base64_encode_atom(client_nonce_raw + 3*i, 3,
                           client_nonce_base64 + 4*i);

    /*
     * RFC 7616 section 3.4.2: the hash "A1" is a hash of
     * username:realm:password (in the absence of hash names like
     * "MD5-sess" which as far as I know don't sensibly apply to
     * proxies and HTTP CONNECT).
     */
    { // WINSCP
    ssh_hash *h = ssh_hash_new(alg);
    put_datapl(h, username);
    put_byte(h, ':');
    put_datapl(h, realm);
    put_byte(h, ':');
    put_datapl(h, password);
    ssh_hash_digest_nondestructive(h, a1hash);

    /*
     * RFC 7616 section 3.4.3: the hash "A2" is a hash of method:uri
     * (in the absence of more interesting quality-of-protection
     * schemes than plain "auth" - e.g. "auth-int" hashes the entire
     * document as well - which again I don't think make sense in the
     * context of proxies and CONNECT).
     */
    ssh_hash_reset(h);
    put_datapl(h, method);
    put_byte(h, ':');
    put_datapl(h, uri);
    ssh_hash_digest_nondestructive(h, a2hash);

    /*
     * RFC 7616 section 3.4.1: the overall output hash in the
     * "response" parameter of the authorization header is a hash of
     * A1:nonce:nonce-count:client-nonce:qop:A2, where A1 and A2 are
     * the hashes computed above.
     */
    ssh_hash_reset(h);
    put_hex_data(h, a1hash, hashlen);
    put_byte(h, ':');
    put_datapl(h, nonce);
    put_byte(h, ':');
    put_hex_data(h, ncbuf, 4);
    put_byte(h, ':');
    put_data(h, client_nonce_base64, lenof(client_nonce_base64));
    put_byte(h, ':');
    put_datapl(h, qop);
    put_byte(h, ':');
    put_hex_data(h, a2hash, hashlen);
    ssh_hash_final(h, rsphash);

    /*
     * Now construct the output header (everything after the initial
     * "Proxy-Authorization: Digest ") and write it to the provided
     * BinarySink.
     */
    put_datalit(bs, "username=\"");
    if (hash_username) {
        /*
         * RFC 7616 section 3.4.4: if we're hashing the username, we
         * actually hash username:realm (like a truncated version of
         * A1 above).
         */
        ssh_hash *h = ssh_hash_new(alg);
        put_datapl(h, username);
        put_byte(h, ':');
        put_datapl(h, realm);
        ssh_hash_final(h, a1hash);
        put_hex_data(bs, a1hash, hashlen);
    } else {
        put_datapl(bs, username);
    }
    put_datalit(bs, "\", realm=\"");
    put_datapl(bs, realm);
    put_datalit(bs, "\", uri=\"");
    put_datapl(bs, uri);
    put_datalit(bs, "\", algorithm=");
    put_dataz(bs, httphashnames[hash]);
    put_datalit(bs, ", nonce=\"");
    put_datapl(bs, nonce);
    put_datalit(bs, "\", nc=");
    put_hex_data(bs, ncbuf, 4);
    put_datalit(bs, ", cnonce=\"");
    put_data(bs, client_nonce_base64, lenof(client_nonce_base64));
    put_datalit(bs, "\", qop=");
    put_datapl(bs, qop);
    put_datalit(bs, ", response=\"");
    put_hex_data(bs, rsphash, hashlen);
    put_datalit(bs, "\"");

    if (opaque.ptr) {
        put_datalit(bs, ", opaque=\"");
        put_datapl(bs, opaque);
        put_datalit(bs, "\"");
    }

    if (hash_username) {
        put_datalit(bs, ", userhash=true");
    }

    smemclr(a1hash, lenof(a1hash));
    smemclr(a2hash, lenof(a2hash));
    smemclr(rsphash, lenof(rsphash));
    smemclr(client_nonce_raw, lenof(client_nonce_raw));
    smemclr(client_nonce_base64, lenof(client_nonce_base64));
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
}
