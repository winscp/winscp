/*
 * Header for the interaction between proxy.c and cproxy.c. Separated
 * from proxy.h proper so that testcrypt can include it conveniently.
 */

extern const bool socks5_chap_available;
strbuf *chap_response(ptrlen challenge, ptrlen password);
extern const bool http_digest_available;

/*
 * List macro for the various hash functions defined for HTTP Digest.
 *
 * Of these, MD5 is the original one; SHA-256 is unambiguous; but
 * SHA-512-256 seems to be controversial.
 *
 * RFC 7616 doesn't provide a normative reference, or any text
 * explaining what they mean by it. They apparently expect you to
 * already know. The problem with that is that there are two plausible
 * things they _might_ have meant:
 *
 *  1. Ordinary SHA-512, truncated to 256 bits by discarding the
 *     second half of the hash output, per FIPS 180-4 section 7 (which
 *     says that in general it's OK to truncate hash functions like
 *     that if you need to). FIPS 180-4 assigns no particular specific
 *     spelling to this kind of truncated hash.
 *
 *  2. The same except that the initial state of the SHA-512 algorithm
 *     is reset to a different 512-bit vector to ensure that it's a
 *     distinguishable hash function in its own right, per FIPS 180-4
 *     section 6.7 (which in turn refers to section 5.3.6.2 for the
 *     actual initial values). FIPS 180-4 spells this "SHA-512/256".
 *
 * The text of RFC 7616 is totally silent as to which of these they
 * meant. Their spelling is inconsistent: the protocol identifier is
 * "SHA-512-256", but in some places in the RFC they say
 * "SHA-512/256", matching FIPS's spelling for the hash in option 2
 * above. On the other hand, the example authentication exchange in
 * section 3.9.2 of the RFC contains hashes that are consistent with
 * option 1 above (a truncation of plain SHA-512).
 *
 * Erratum 4897, https://www.rfc-editor.org/errata/eid4897, points out
 * this ambiguity, and suggests correcting the example exchange to be
 * consistent with option 2. However, as of 2021-11-27, that erratum
 * is shown on the RFC Editor website in state "Reported", with no
 * response (positive _or_ negative) from the RFC authors or anyone
 * else. (And it was reported in 2016, so it's not as if they haven't
 * had time.)
 *
 * So, which hash should we implement? Perhaps there's a consensus
 * among existing implementations in the wild?
 *
 * I rigged up an HTTP server to present a SHA-512-256 Digest auth
 * request, and tried various HTTP clients against it. The only HTTP
 * client I found that accepts 'algorithm="SHA-512-256"' and sends
 * back an auth attempt quoting the same hash is curl - and curl,
 * bizarrely, seems to treat "SHA-512-256" as _neither_ of the above
 * options, but as simply an alias for SHA-256!
 *
 * Therefore, I think the only safe answer is to refuse to support
 * that hash at all: it's too confusing.
 *
 * However, I keep it in the list of hashes here, so that we can check
 * the test case from RFC 7616, because that test case is also the
 * only test of username hashing. So we reject it in proxy/http.c, but
 * accept it in the internal function http_digest_response(), and
 * treat it as option 1 (truncated SHA-512).
 *
 * Therefore, the parameters to each invocation of X in the following
 * list macro are:
 *
 *  - internal enum id for the hash
 *  - protocol identifier string
 *  - algorithm to use for computing it (as a const ssh_hashalg *)
 *  - length to truncate the output to
 *  - whether we accept it in http.c or not.
 *
 * Finally, the ordering of the accepted hashes is our preference
 * order among them if the server offers a choice.
 */
#define HTTP_DIGEST_HASHES(X)                                           \
    X(HTTP_DIGEST_MD5, "MD5", &ssh_md5, 128, true)                      \
    X(HTTP_DIGEST_SHA256, "SHA-256", &ssh_sha256, 256, true)            \
    X(HTTP_DIGEST_SHA512_256, "SHA-512-256", &ssh_sha512, 256, false)   \
    /* end of list */

typedef enum HttpDigestHash {
    #define DECL_ENUM(id, str, alg, bits, accepted) id,
    HTTP_DIGEST_HASHES(DECL_ENUM)
    #undef DECL_ENUM
    N_HTTP_DIGEST_HASHES
} HttpDigestHash;

extern const char *const httphashnames[];
extern const bool httphashaccepted[];

void http_digest_response(BinarySink *bs, ptrlen username, ptrlen password,
                          ptrlen realm, ptrlen method, ptrlen uri, ptrlen qop,
                          ptrlen nonce, ptrlen opaque, uint32_t nonce_count,
                          HttpDigestHash hash, bool hash_username);
