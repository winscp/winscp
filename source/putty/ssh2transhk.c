/*
 * Data structure managing host keys in sessions based on GSSAPI KEX.
 *
 * In a session we started with a GSSAPI key exchange, the concept of
 * 'host key' has completely different lifetime and security semantics
 * from the usual ones. Per RFC 4462 section 2.1, we assume that any
 * host key delivered to us in the course of a GSSAPI key exchange is
 * _solely_ there to use as a transient fallback within the same
 * session, if at the time of a subsequent rekey the GSS credentials
 * are temporarily invalid and so a non-GSS KEX method has to be used.
 *
 * In particular, in a GSS-based SSH deployment, host keys may not
 * even _be_ persistent identities for the server; it would be
 * legitimate for a server to generate a fresh one routinely if it
 * wanted to, like SSH-1 server keys.
 *
 * So, in this mode, we never touch the persistent host key cache at
 * all, either to check keys against it _or_ to store keys in it.
 * Instead, we maintain an in-memory cache of host keys that have been
 * mentioned in GSS key exchanges within this particular session, and
 * we permit precisely those host keys in non-GSS rekeys.
 */

#include <assert.h>

#include "putty.h"
#include "ssh.h"

struct ssh_transient_hostkey_cache {
    tree234 *cache;
};

struct ssh_transient_hostkey_cache_entry {
    const ssh_keyalg *alg;
    strbuf *pub_blob;
};

static int ssh_transient_hostkey_cache_cmp(void *av, void *bv)
{
    const struct ssh_transient_hostkey_cache_entry
        *a = (const struct ssh_transient_hostkey_cache_entry *)av,
        *b = (const struct ssh_transient_hostkey_cache_entry *)bv;
    return strcmp(a->alg->ssh_id, b->alg->ssh_id);
}

static int ssh_transient_hostkey_cache_find(void *av, void *bv)
{
    const ssh_keyalg *aalg = (const ssh_keyalg *)av;
    const struct ssh_transient_hostkey_cache_entry
        *b = (const struct ssh_transient_hostkey_cache_entry *)bv;
    return strcmp(aalg->ssh_id, b->alg->ssh_id);
}

ssh_transient_hostkey_cache *ssh_transient_hostkey_cache_new(void)
{
    ssh_transient_hostkey_cache *thc = snew(ssh_transient_hostkey_cache);
    thc->cache = newtree234(ssh_transient_hostkey_cache_cmp);
    return thc;
}

void ssh_transient_hostkey_cache_free(ssh_transient_hostkey_cache *thc)
{
    struct ssh_transient_hostkey_cache_entry *ent;
    while ((ent = delpos234(thc->cache, 0)) != NULL) {
        strbuf_free(ent->pub_blob);
        sfree(ent);
    }
    freetree234(thc->cache);
    sfree(thc);
}

void ssh_transient_hostkey_cache_add(
    ssh_transient_hostkey_cache *thc, ssh_key *key)
{
    struct ssh_transient_hostkey_cache_entry *ent, *retd;

    if ((ent = find234(thc->cache, (void *)ssh_key_alg(key),
                       ssh_transient_hostkey_cache_find)) != NULL) {
        strbuf_free(ent->pub_blob);
        sfree(ent);
    }

    ent = snew(struct ssh_transient_hostkey_cache_entry);
    ent->alg = ssh_key_alg(key);
    ent->pub_blob = strbuf_new();
    ssh_key_public_blob(key, BinarySink_UPCAST(ent->pub_blob));
    retd = add234(thc->cache, ent);
    assert(retd == ent);
}

bool ssh_transient_hostkey_cache_verify(
    ssh_transient_hostkey_cache *thc, ssh_key *key)
{
    struct ssh_transient_hostkey_cache_entry *ent;
    bool toret = false;

    if ((ent = find234(thc->cache, (void *)ssh_key_alg(key),
                       ssh_transient_hostkey_cache_find)) != NULL) {
        strbuf *this_blob = strbuf_new();
        ssh_key_public_blob(key, BinarySink_UPCAST(this_blob));

        if (this_blob->len == ent->pub_blob->len &&
            !memcmp(this_blob->s, ent->pub_blob->s,
                    this_blob->len))
            toret = true;

        strbuf_free(this_blob);
    }

    return toret;
}

bool ssh_transient_hostkey_cache_has(
    ssh_transient_hostkey_cache *thc, const ssh_keyalg *alg)
{
    struct ssh_transient_hostkey_cache_entry *ent =
        find234(thc->cache, (void *)alg,
                ssh_transient_hostkey_cache_find);
    return ent != NULL;
}

bool ssh_transient_hostkey_cache_non_empty(ssh_transient_hostkey_cache *thc)
{
    return count234(thc->cache) > 0;
}
