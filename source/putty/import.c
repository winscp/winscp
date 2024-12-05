/*
 * Code for PuTTY to import and export private key files in other
 * SSH clients' formats.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>

#include "putty.h"
#include "ssh.h"
#include "mpint.h"
#include "misc.h"

static bool openssh_pem_encrypted(BinarySource *src);
static bool openssh_new_encrypted(BinarySource *src);
static ssh2_userkey *openssh_pem_read(
    BinarySource *src, const char *passphrase, const char **errmsg_p);
static ssh2_userkey *openssh_new_read(
    BinarySource *src, const char *passphrase, const char **errmsg_p);
static bool openssh_auto_write(
    const Filename *file, ssh2_userkey *key, const char *passphrase);
static bool openssh_pem_write(
    const Filename *file, ssh2_userkey *key, const char *passphrase);
static bool openssh_new_write(
    const Filename *file, ssh2_userkey *key, const char *passphrase);

static bool sshcom_encrypted(BinarySource *src, char **comment);
static ssh2_userkey *sshcom_read(
    BinarySource *src, const char *passphrase, const char **errmsg_p);
static bool sshcom_write(
    const Filename *file, ssh2_userkey *key, const char *passphrase);

/*
 * Given a key type, determine whether we know how to import it.
 */
bool import_possible(int type)
{
    if (type == SSH_KEYTYPE_OPENSSH_PEM)
        return true;
    if (type == SSH_KEYTYPE_OPENSSH_NEW)
        return true;
    if (type == SSH_KEYTYPE_SSHCOM)
        return true;
    return false;
}

/*
 * Given a key type, determine what native key type
 * (SSH_KEYTYPE_SSH1 or SSH_KEYTYPE_SSH2) it will come out as once
 * we've imported it.
 */
int import_target_type(int type)
{
    /*
     * There are no known foreign SSH-1 key formats.
     */
    return SSH_KEYTYPE_SSH2;
}

static inline char *bsgetline(BinarySource *src)
{
    ptrlen line = get_chomped_line(src);
    if (get_err(src))
        return NULL;
    return mkstr(line);
}

/*
 * Determine whether a foreign key is encrypted.
 */
bool import_encrypted_s(const Filename *filename, BinarySource *src,
                        int type, char **comment)
{
    if (type == SSH_KEYTYPE_OPENSSH_PEM) {
        /* OpenSSH PEM format doesn't contain a key comment at all */
        *comment = dupstr(filename_to_str(filename));
        return openssh_pem_encrypted(src);
    } else if (type == SSH_KEYTYPE_OPENSSH_NEW) {
        /* OpenSSH new format does, but it's inside the encrypted
         * section for some reason */
        *comment = dupstr(filename_to_str(filename));
        return openssh_new_encrypted(src);
    } else if (type == SSH_KEYTYPE_SSHCOM) {
        return sshcom_encrypted(src, comment);
    }
    return false;
}

bool import_encrypted(const Filename *filename, int type, char **comment)
{
    LoadedFile *lf = lf_load_keyfile(filename, NULL);
    if (!lf)
        return false; /* couldn't even open the file */

    { // WINSCP
    bool toret = import_encrypted_s(filename, BinarySource_UPCAST(lf),
                                    type, comment);
    lf_free(lf);
    return toret;
    } // WINSCP
}

/*
 * Import an SSH-1 key.
 */
int import_ssh1_s(BinarySource *src, int type,
                  RSAKey *key, char *passphrase, const char **errmsg_p)
{
    return 0;
}

int import_ssh1(const Filename *filename, int type,
                RSAKey *key, char *passphrase, const char **errmsg_p)
{
    LoadedFile *lf = lf_load_keyfile(filename, errmsg_p);
    if (!lf)
        return false;

    { // WINSCP
    int toret = import_ssh1_s(BinarySource_UPCAST(lf),
                              type, key, passphrase, errmsg_p);
    lf_free(lf);
    return toret;
    } // WINSCP
}

/*
 * Import an SSH-2 key.
 */
ssh2_userkey *import_ssh2_s(BinarySource *src, int type,
                            char *passphrase, const char **errmsg_p)
{
    if (type == SSH_KEYTYPE_OPENSSH_PEM)
        return openssh_pem_read(src, passphrase, errmsg_p);
    else if (type == SSH_KEYTYPE_OPENSSH_NEW)
        return openssh_new_read(src, passphrase, errmsg_p);
    if (type == SSH_KEYTYPE_SSHCOM)
        return sshcom_read(src, passphrase, errmsg_p);
    return NULL;
}

ssh2_userkey *import_ssh2(const Filename *filename, int type,
                          char *passphrase, const char **errmsg_p)
{
    LoadedFile *lf = lf_load_keyfile(filename, errmsg_p);
    if (!lf)
        return false;

    { // WINSCP
    ssh2_userkey *toret = import_ssh2_s(BinarySource_UPCAST(lf),
                                        type, passphrase, errmsg_p);
    lf_free(lf);
    return toret;
    } // WINSCP
}

/*
 * Export an SSH-1 key.
 */
bool export_ssh1(const Filename *filename, int type, RSAKey *key,
                 char *passphrase)
{
    return false;
}

/*
 * Export an SSH-2 key.
 */
bool export_ssh2(const Filename *filename, int type,
                 ssh2_userkey *key, char *passphrase)
{
    if (type == SSH_KEYTYPE_OPENSSH_AUTO)
        return openssh_auto_write(filename, key, passphrase);
    if (type == SSH_KEYTYPE_OPENSSH_NEW)
        return openssh_new_write(filename, key, passphrase);
    if (type == SSH_KEYTYPE_SSHCOM)
        return sshcom_write(filename, key, passphrase);
    return false;
}

/* ----------------------------------------------------------------------
 * Helper routines. (The base64 ones are defined in sshpubk.c.)
 */

#define isbase64(c) (    ((c) >= 'A' && (c) <= 'Z') || \
                         ((c) >= 'a' && (c) <= 'z') || \
                         ((c) >= '0' && (c) <= '9') || \
                         (c) == '+' || (c) == '/' || (c) == '=' \
                         )

/*
 * Read an ASN.1/BER identifier and length pair.
 *
 * Flags are a combination of the #defines listed below.
 *
 * Returns -1 if unsuccessful; otherwise returns the number of
 * bytes used out of the source data.
 */

/* ASN.1 tag classes. */
#define ASN1_CLASS_UNIVERSAL        (0 << 6)
#define ASN1_CLASS_APPLICATION      (1 << 6)
#define ASN1_CLASS_CONTEXT_SPECIFIC (2 << 6)
#define ASN1_CLASS_PRIVATE          (3 << 6)
#define ASN1_CLASS_MASK             (3 << 6)

/* Primitive versus constructed bit. */
#define ASN1_CONSTRUCTED            (1 << 5)

/*
 * Write an ASN.1/BER identifier and length pair. Returns the
 * number of bytes consumed. Assumes dest contains enough space.
 * Will avoid writing anything if dest is NULL, but still return
 * amount of space required.
 */
static void BinarySink_put_ber_id_len(BinarySink *bs,
                                      int id, int length, int flags)
{
    if (id <= 30) {
        /*
         * Identifier is one byte.
         */
        put_byte(bs, id | flags);
    } else {
        int n;
        /*
         * Identifier is multiple bytes: the first byte is 11111
         * plus the flags, and subsequent bytes encode the value of
         * the identifier, 7 bits at a time, with the top bit of
         * each byte 1 except the last one which is 0.
         */
        put_byte(bs, 0x1F | flags);
        for (n = 1; (id >> (7*n)) > 0; n++)
            continue;                  /* count the bytes */
        while (n--)
            put_byte(bs, (n ? 0x80 : 0) | ((id >> (7*n)) & 0x7F));
    }

    if (length < 128) {
        /*
         * Length is one byte.
         */
        put_byte(bs, length);
    } else {
        int n;
        /*
         * Length is multiple bytes. The first is 0x80 plus the
         * number of subsequent bytes, and the subsequent bytes
         * encode the actual length.
         */
        for (n = 1; (length >> (8*n)) > 0; n++)
            continue;                  /* count the bytes */
        put_byte(bs, 0x80 | n);
        while (n--)
            put_byte(bs, (length >> (8*n)) & 0xFF);
    }
}

#define put_ber_id_len(bs, id, len, flags) \
    BinarySink_put_ber_id_len(BinarySink_UPCAST(bs), id, len, flags)

typedef struct ber_item {
    int id;
    int flags;
    ptrlen data;
} ber_item;

static ber_item BinarySource_get_ber(BinarySource *src)
{
    ber_item toret;
    unsigned char leadbyte, lenbyte;
    size_t length;

    leadbyte = get_byte(src);
    toret.flags = (leadbyte & 0xE0);
    if ((leadbyte & 0x1F) == 0x1F) {
        unsigned char idbyte;

        toret.id = 0;
        do {
            idbyte = get_byte(src);
            toret.id = (toret.id << 7) | (idbyte & 0x7F);
        } while (idbyte & 0x80);
    } else {
        toret.id = leadbyte & 0x1F;
    }

    lenbyte = get_byte(src);
    if (lenbyte & 0x80) {
        int nbytes = lenbyte & 0x7F;
        length = 0;
        while (nbytes-- > 0)
            length = (length << 8) | get_byte(src);
    } else {
        length = lenbyte;
    }

    toret.data = get_data(src, length);
    return toret;
}

#define get_ber(bs) BinarySource_get_ber(BinarySource_UPCAST(bs))

/* ----------------------------------------------------------------------
 * Code to read and write OpenSSH private keys, in the old-style PEM
 * format.
 */

typedef enum {
    OP_DSA, OP_RSA, OP_ECDSA
} openssh_pem_keytype;
typedef enum {
    OP_E_3DES, OP_E_AES
} openssh_pem_enc;

struct openssh_pem_key {
    openssh_pem_keytype keytype;
    bool encrypted;
    openssh_pem_enc encryption;
    char iv[32];
    strbuf *keyblob;
};

static void BinarySink_put_mp_ssh2_from_string(BinarySink *bs, ptrlen str)
{
    const unsigned char *bytes = (const unsigned char *)str.ptr;
    size_t nbytes = str.len;
    while (nbytes > 0 && bytes[0] == 0) {
        nbytes--;
        bytes++;
    }
    if (nbytes > 0 && bytes[0] & 0x80) {
        put_uint32(bs, nbytes + 1);
        put_byte(bs, 0);
    } else {
        put_uint32(bs, nbytes);
    }
    put_data(bs, bytes, nbytes);
}
#define put_mp_ssh2_from_string(bs, str) \
    BinarySink_put_mp_ssh2_from_string(BinarySink_UPCAST(bs), str)

static struct openssh_pem_key *load_openssh_pem_key(BinarySource *src,
                                                    const char **errmsg_p)
{
    struct openssh_pem_key *key;
    char *line = NULL;
    const char *errmsg;
    char *p;
    bool headers_done;
    char base64_bit[4];
    int base64_chars = 0;

    key = snew(struct openssh_pem_key);
    key->keyblob = strbuf_new_nm();

    if (!(line = bsgetline(src))) {
        errmsg = "unexpected end of file";
        goto error;
    }
    if (!strstartswith(line, "-----BEGIN ") ||
        !strendswith(line, "PRIVATE KEY-----")) {
        errmsg = "file does not begin with OpenSSH key header";
        goto error;
    }
    /*
     * Parse the BEGIN line. For old-format keys, this tells us the
     * type of the key; for new-format keys, all it tells us is the
     * format, and we'll find out the key type once we parse the
     * base64.
     */
    if (!strcmp(line, "-----BEGIN RSA PRIVATE KEY-----")) {
        key->keytype = OP_RSA;
    } else if (!strcmp(line, "-----BEGIN DSA PRIVATE KEY-----")) {
        key->keytype = OP_DSA;
    } else if (!strcmp(line, "-----BEGIN EC PRIVATE KEY-----")) {
        key->keytype = OP_ECDSA;
    } else if (!strcmp(line, "-----BEGIN OPENSSH PRIVATE KEY-----")) {
        errmsg = "this is a new-style OpenSSH key";
        goto error;
    } else {
        errmsg = "unrecognised key type";
        goto error;
    }
    smemclr(line, strlen(line));
    sfree(line);
    line = NULL;

    key->encrypted = false;
    memset(key->iv, 0, sizeof(key->iv));

    headers_done = false;
    while (1) {
        if (!(line = bsgetline(src))) {
            errmsg = "unexpected end of file";
            goto error;
        }
        if (strstartswith(line, "-----END ") &&
            strendswith(line, "PRIVATE KEY-----")) {
            sfree(line);
            line = NULL;
            break;                     /* done */
        }
        if ((p = strchr(line, ':')) != NULL) {
            if (headers_done) {
                errmsg = "header found in body of key data";
                goto error;
            }
            *p++ = '\0';
            while (*p && isspace((unsigned char)*p)) p++;
            if (!strcmp(line, "Proc-Type")) {
                if (p[0] != '4' || p[1] != ',') {
                    errmsg = "Proc-Type is not 4 (only 4 is supported)";
                    goto error;
                }
                p += 2;
                if (!strcmp(p, "ENCRYPTED"))
                    key->encrypted = true;
            } else if (!strcmp(line, "DEK-Info")) {
                int i, ivlen;

                if (!strncmp(p, "DES-EDE3-CBC,", 13)) {
                    key->encryption = OP_E_3DES;
                    ivlen = 8;
                } else if (!strncmp(p, "AES-128-CBC,", 12)) {
                    key->encryption = OP_E_AES;
                    ivlen = 16;
                } else {
                    errmsg = "unsupported cipher";
                    goto error;
                }
                p = strchr(p, ',') + 1;/* always non-NULL, by above checks */
                for (i = 0; i < ivlen; i++) {
                    unsigned j;
                    if (1 != sscanf(p, "%2x", &j)) {
                        errmsg = "expected more iv data in DEK-Info";
                        goto error;
                    }
                    key->iv[i] = j;
                    p += 2;
                }
                if (*p) {
                    errmsg = "more iv data than expected in DEK-Info";
                    goto error;
                }
            }
        } else {
            headers_done = true;

            p = line;
            while (isbase64(*p)) {
                base64_bit[base64_chars++] = *p;
                if (base64_chars == 4) {
                    unsigned char out[3];
                    int len;

                    base64_chars = 0;

                    len = base64_decode_atom(base64_bit, out);

                    if (len <= 0) {
                        errmsg = "invalid base64 encoding";
                        goto error;
                    }

                    put_data(key->keyblob, out, len);

                    smemclr(out, sizeof(out));
                }

                p++;
            }
        }
        smemclr(line, strlen(line));
        sfree(line);
        line = NULL;
    }

    if (!key->keyblob || key->keyblob->len == 0) {
        errmsg = "key body not present";
        goto error;
    }

    if (key->encrypted && key->keyblob->len % 8 != 0) {
        errmsg = "encrypted key blob is not a multiple of "
            "cipher block size";
        goto error;
    }

    smemclr(base64_bit, sizeof(base64_bit));
    if (errmsg_p) *errmsg_p = NULL;
    return key;

  error:
    if (line) {
        smemclr(line, strlen(line));
        sfree(line);
        line = NULL;
    }
    smemclr(base64_bit, sizeof(base64_bit));
    if (key) {
        if (key->keyblob)
            strbuf_free(key->keyblob);
        smemclr(key, sizeof(*key));
        sfree(key);
    }
    if (errmsg_p) *errmsg_p = errmsg;
    return NULL;
}

static bool openssh_pem_encrypted(BinarySource *src)
{
    struct openssh_pem_key *key = load_openssh_pem_key(src, NULL);
    bool ret;

    if (!key)
        return false;
    ret = key->encrypted;
    strbuf_free(key->keyblob);
    smemclr(key, sizeof(*key));
    sfree(key);
    return ret;
}

static void openssh_pem_derivekey(
    ptrlen passphrase, const void *iv, uint8_t *keybuf)
{
    /*
     * Derive the encryption key for a PEM key file from the
     * passphrase and iv/salt:
     *
     *  - let block A equal MD5(passphrase || iv)
     *  - let block B equal MD5(A || passphrase || iv)
     *  - block C would be MD5(B || passphrase || iv) and so on
     *  - encryption key is the first N bytes of A || B
     *
     * (Note that only 8 bytes of the iv are used for key
     * derivation, even when the key is encrypted with AES and
     * hence there are 16 bytes available.)
     */
    ssh_hash *h;

    h = ssh_hash_new(&ssh_md5);
    put_datapl(h, passphrase);
    put_data(h, iv, 8);
    ssh_hash_digest(h, keybuf);

    ssh_hash_reset(h);
    put_data(h, keybuf, 16);
    put_datapl(h, passphrase);
    put_data(h, iv, 8);
    ssh_hash_final(h, keybuf + 16);
}

static ssh2_userkey *openssh_pem_read(
    BinarySource *filesrc, const char *passphrase, const char **errmsg_p)
{
    struct openssh_pem_key *key = load_openssh_pem_key(filesrc, errmsg_p);
    ssh2_userkey *retkey;
    const ssh_keyalg *alg;
    BinarySource src[1];
    int i, num_integers;
    ssh2_userkey *retval = NULL;
    const char *errmsg;
    strbuf *blob = strbuf_new_nm();
    int privptr = 0, publen;

    if (!key) {
        strbuf_free(blob);
        return NULL;
    }

    if (key->encrypted) {
        unsigned char keybuf[32];
        openssh_pem_derivekey(ptrlen_from_asciz(passphrase), key->iv, keybuf);

        /*
         * Decrypt the key blob.
         */
        if (key->encryption == OP_E_3DES)
            des3_decrypt_pubkey_ossh(keybuf, key->iv,
                                     key->keyblob->u, key->keyblob->len);
        else {
            ssh_cipher *cipher = ssh_cipher_new(&ssh_aes128_cbc);
            ssh_cipher_setkey(cipher, keybuf);
            ssh_cipher_setiv(cipher, key->iv);
            ssh_cipher_decrypt(cipher, key->keyblob->u, key->keyblob->len);
            ssh_cipher_free(cipher);
        }

        smemclr(keybuf, sizeof(keybuf));
    }

    /*
     * Now we have a decrypted key blob, which contains an ASN.1
     * encoded private key. We must now untangle the ASN.1.
     *
     * We expect the whole key blob to be formatted as a SEQUENCE
     * (0x30 followed by a length code indicating that the rest of
     * the blob is part of the sequence). Within that SEQUENCE we
     * expect to see a bunch of INTEGERs. What those integers mean
     * depends on the key type:
     *
     *  - For RSA, we expect the integers to be 0, n, e, d, p, q,
     *    dmp1, dmq1, iqmp in that order. (The last three are d mod
     *    (p-1), d mod (q-1), inverse of q mod p respectively.)
     *
     *  - For DSA, we expect them to be 0, p, q, g, y, x in that
     *    order.
     *
     *  - In ECDSA the format is totally different: we see the
     *    SEQUENCE, but beneath is an INTEGER 1, OCTET STRING priv
     *    EXPLICIT [0] OID curve, EXPLICIT [1] BIT STRING pubPoint
     */

    BinarySource_BARE_INIT(src, key->keyblob->u, key->keyblob->len);

    {
        /* Expect the SEQUENCE header. Take its absence as a failure to
         * decrypt, if the key was encrypted. */
        ber_item seq = get_ber(src);
        if (get_err(src) || seq.id != 16) {
            errmsg = "ASN.1 decoding failure";
            retval = key->encrypted ? SSH2_WRONG_PASSPHRASE : NULL;
            goto error;
        }

        /* Reinitialise our BinarySource to parse just the inside of that
         * SEQUENCE. */
        BinarySource_BARE_INIT_PL(src, seq.data);
    }

    /* Expect a load of INTEGERs. */
    if (key->keytype == OP_RSA)
        num_integers = 9;
    else if (key->keytype == OP_DSA)
        num_integers = 6;
    else
        num_integers = 0;              /* placate compiler warnings */


    if (key->keytype == OP_ECDSA) {
        /* And now for something completely different */
        ber_item integer, privkey, sub0, sub1, oid, pubkey;
        const ssh_keyalg *alg;
        const struct ec_curve *curve;

        /* Parse the outer layer of things inside the containing SEQUENCE */
        integer = get_ber(src);
        privkey = get_ber(src);
        sub0 = get_ber(src);
        sub1 = get_ber(src);

        /* Now look inside sub0 for the curve OID */
        BinarySource_BARE_INIT_PL(src, sub0.data);
        oid = get_ber(src);

        /* And inside sub1 for the public-key BIT STRING */
        BinarySource_BARE_INIT_PL(src, sub1.data);
        pubkey = get_ber(src);

        if (get_err(src) ||
            integer.id != 2 ||
            integer.data.len != 1 ||
            ((const unsigned char *)integer.data.ptr)[0] != 1 ||
            privkey.id != 4 ||
            sub0.id != 0 ||
            sub1.id != 1 ||
            oid.id != 6 ||
            pubkey.id != 3) {

            errmsg = "ASN.1 decoding failure";
            retval = key->encrypted ? SSH2_WRONG_PASSPHRASE : NULL;
            goto error;
        }

        alg = ec_alg_by_oid(oid.data.len, oid.data.ptr, &curve);
        if (!alg) {
            errmsg = "Unsupported ECDSA curve.";
            retval = NULL;
            goto error;
        }
        if (pubkey.data.len != ((((curve->fieldBits + 7) / 8) * 2) + 2)) {
            errmsg = "ASN.1 decoding failure";
            retval = key->encrypted ? SSH2_WRONG_PASSPHRASE : NULL;
            goto error;
        }
        /* Skip 0x00 before point */
        pubkey.data.ptr = (const char *)pubkey.data.ptr + 1;
        pubkey.data.len -= 1;

        /* Construct the key */
        retkey = snew(ssh2_userkey);

        put_stringz(blob, alg->ssh_id);
        put_stringz(blob, curve->name);
        put_stringpl(blob, pubkey.data);
        publen = blob->len;
        put_mp_ssh2_from_string(blob, privkey.data);

        retkey->key = ssh_key_new_priv(
            alg, make_ptrlen(blob->u, publen),
            make_ptrlen(blob->u + publen, blob->len - publen));

        if (!retkey->key) {
            sfree(retkey);
            errmsg = "unable to create key data structure";
            goto error;
        }

    } else if (key->keytype == OP_RSA || key->keytype == OP_DSA) {

        put_stringz(blob, key->keytype == OP_DSA ? "ssh-dss" : "ssh-rsa");

        { // WINSCP
        ptrlen rsa_modulus = PTRLEN_LITERAL("");

        for (i = 0; i < num_integers; i++) {
            ber_item integer = get_ber(src);

            if (get_err(src) || integer.id != 2) {
                errmsg = "ASN.1 decoding failure";
                retval = key->encrypted ? SSH2_WRONG_PASSPHRASE : NULL;
                goto error;
            }

            if (i == 0) {
                /*
                 * The first integer should be zero always (I think
                 * this is some sort of version indication).
                 */
                if (integer.data.len != 1 ||
                    ((const unsigned char *)integer.data.ptr)[0] != 0) {
                    errmsg = "version number mismatch";
                    goto error;
                }
            } else if (key->keytype == OP_RSA) {
                /*
                 * Integers 1 and 2 go into the public blob but in the
                 * opposite order; integers 3, 4, 5 and 8 go into the
                 * private blob. The other two (6 and 7) are ignored.
                 */
                if (i == 1) {
                    /* Save the details for after we deal with number 2. */
                    rsa_modulus = integer.data;
                } else if (i != 6 && i != 7) {
                    put_mp_ssh2_from_string(blob, integer.data);
                    if (i == 2) {
                        put_mp_ssh2_from_string(blob, rsa_modulus);
                        privptr = blob->len;
                    }
                }
            } else if (key->keytype == OP_DSA) {
                /*
                 * Integers 1-4 go into the public blob; integer 5 goes
                 * into the private blob.
                 */
                put_mp_ssh2_from_string(blob, integer.data);
                if (i == 4)
                    privptr = blob->len;
            }
        }

        /*
         * Now put together the actual key. Simplest way to do this is
         * to assemble our own key blobs and feed them to the createkey
         * functions; this is a bit faffy but it does mean we get all
         * the sanity checks for free.
         */
        assert(privptr > 0);          /* should have bombed by now if not */
        retkey = snew(ssh2_userkey);
        alg = (key->keytype == OP_RSA ? &ssh_rsa : &ssh_dsa);
        retkey->key = ssh_key_new_priv(
            alg, make_ptrlen(blob->u, privptr),
            make_ptrlen(blob->u+privptr, blob->len-privptr));

        if (!retkey->key) {
            sfree(retkey);
            errmsg = "unable to create key data structure";
            goto error;
        }
        } // WINSCP

    } else {
        unreachable("Bad key type from load_openssh_pem_key");
        errmsg = "Bad key type from load_openssh_pem_key";
        goto error;
    }

    /*
     * The old key format doesn't include a comment in the private
     * key file.
     */
    retkey->comment = dupstr("imported-openssh-key");

    errmsg = NULL;                     /* no error */
    retval = retkey;

  error:
    strbuf_free(blob);
    strbuf_free(key->keyblob);
    smemclr(key, sizeof(*key));
    sfree(key);
    if (errmsg_p) *errmsg_p = errmsg;
    return retval;
}

static bool openssh_pem_write(
    const Filename *filename, ssh2_userkey *ukey, const char *passphrase)
{
    strbuf *pubblob, *privblob, *outblob;
    unsigned char *spareblob;
    int sparelen = 0;
    ptrlen numbers[9];
    int nnumbers, i;
    const char *header, *footer;
    char zero[1];
    unsigned char iv[8];
    bool ret = false;
    FILE *fp;
    BinarySource src[1];

    /* OpenSSH's private key files never contain a certificate, so
     * revert to the underlying base key if necessary */
    ssh_key *key = ssh_key_base_key(ukey->key);

    /*
     * Fetch the key blobs.
     */
    pubblob = strbuf_new();
    ssh_key_public_blob(key, BinarySink_UPCAST(pubblob));
    privblob = strbuf_new_nm();
    ssh_key_private_blob(key, BinarySink_UPCAST(privblob));
    spareblob = NULL;

    outblob = strbuf_new_nm();

    /*
     * Encode the OpenSSH key blob, and also decide on the header
     * line.
     */
    if (ssh_key_alg(key) == &ssh_rsa ||
        ssh_key_alg(key) == &ssh_dsa) {
        strbuf *seq;

        /*
         * The RSA and DSA handlers share some code because the two
         * key types have very similar ASN.1 representations, as a
         * plain SEQUENCE of big integers. So we set up a list of
         * bignums per key type and then construct the actual blob in
         * common code after that.
         */
        if (ssh_key_alg(key) == &ssh_rsa) {
            ptrlen n, e, d, p, q, iqmp, dmp1, dmq1;
            mp_int *bd, *bp, *bq, *bdmp1, *bdmq1;

            /*
             * These blobs were generated from inside PuTTY, so we needn't
             * treat them as untrusted.
             */
            BinarySource_BARE_INIT(src, pubblob->u, pubblob->len);
            get_string(src);           /* skip algorithm name */
            e = get_string(src);
            n = get_string(src);
            BinarySource_BARE_INIT(src, privblob->u, privblob->len);
            d = get_string(src);
            p = get_string(src);
            q = get_string(src);
            iqmp = get_string(src);

            assert(!get_err(src));     /* can't go wrong */

            /* We also need d mod (p-1) and d mod (q-1). */
            bd = mp_from_bytes_be(d);
            bp = mp_from_bytes_be(p);
            bq = mp_from_bytes_be(q);
            mp_sub_integer_into(bp, bp, 1);
            mp_sub_integer_into(bq, bq, 1);
            bdmp1 = mp_mod(bd, bp);
            bdmq1 = mp_mod(bd, bq);
            mp_free(bd);
            mp_free(bp);
            mp_free(bq);

            dmp1.len = (mp_get_nbits(bdmp1)+8)/8;
            dmq1.len = (mp_get_nbits(bdmq1)+8)/8;
            sparelen = dmp1.len + dmq1.len;
            spareblob = snewn(sparelen, unsigned char);
            dmp1.ptr = spareblob;
            dmq1.ptr = spareblob + dmp1.len;
            for (i = 0; i < dmp1.len; i++)
                spareblob[i] = mp_get_byte(bdmp1, dmp1.len-1 - i);
            for (i = 0; i < dmq1.len; i++)
                spareblob[i+dmp1.len] = mp_get_byte(bdmq1, dmq1.len-1 - i);
            mp_free(bdmp1);
            mp_free(bdmq1);

            numbers[0] = make_ptrlen(zero, 1); zero[0] = '\0';
            numbers[1] = n;
            numbers[2] = e;
            numbers[3] = d;
            numbers[4] = p;
            numbers[5] = q;
            numbers[6] = dmp1;
            numbers[7] = dmq1;
            numbers[8] = iqmp;

            nnumbers = 9;
            header = "-----BEGIN RSA PRIVATE KEY-----\n";
            footer = "-----END RSA PRIVATE KEY-----\n";
        } else {                       /* ssh-dss */
            ptrlen p, q, g, y, x;

            /*
             * These blobs were generated from inside PuTTY, so we needn't
             * treat them as untrusted.
             */
            BinarySource_BARE_INIT(src, pubblob->u, pubblob->len);
            get_string(src);           /* skip algorithm name */
            p = get_string(src);
            q = get_string(src);
            g = get_string(src);
            y = get_string(src);
            BinarySource_BARE_INIT(src, privblob->u, privblob->len);
            x = get_string(src);

            assert(!get_err(src));     /* can't go wrong */

            numbers[0].ptr = zero; numbers[0].len = 1; zero[0] = '\0';
            numbers[1] = p;
            numbers[2] = q;
            numbers[3] = g;
            numbers[4] = y;
            numbers[5] = x;

            nnumbers = 6;
            header = "-----BEGIN DSA PRIVATE KEY-----\n";
            footer = "-----END DSA PRIVATE KEY-----\n";
        }

        seq = strbuf_new_nm();
        for (i = 0; i < nnumbers; i++) {
            put_ber_id_len(seq, 2, numbers[i].len, 0);
            put_datapl(seq, numbers[i]);
        }
        put_ber_id_len(outblob, 16, seq->len, ASN1_CONSTRUCTED);
        put_data(outblob, seq->s, seq->len);
        strbuf_free(seq);
    } else if (ssh_key_alg(key) == &ssh_ecdsa_nistp256 ||
               ssh_key_alg(key) == &ssh_ecdsa_nistp384 ||
               ssh_key_alg(key) == &ssh_ecdsa_nistp521) {
        const unsigned char *oid;
        struct ecdsa_key *ec = container_of(key, struct ecdsa_key, sshk);
        int oidlen;
        int pointlen;
        strbuf *seq, *sub;

        /*
         * Structure of asn1:
         * SEQUENCE
         *   INTEGER 1
         *   OCTET STRING (private key)
         *   [0]
         *     OID (curve)
         *   [1]
         *     BIT STRING (0x00 public key point)
         */
        oid = ec_alg_oid(ssh_key_alg(key), &oidlen);
        pointlen = (ec->curve->fieldBits + 7) / 8 * 2;

        seq = strbuf_new_nm();

        /* INTEGER 1 */
        put_ber_id_len(seq, 2, 1, 0);
        put_byte(seq, 1);

        /* OCTET STRING private key */
        put_ber_id_len(seq, 4, privblob->len - 4, 0);
        put_data(seq, privblob->s + 4, privblob->len - 4);

        /* Subsidiary OID */
        sub = strbuf_new();
        put_ber_id_len(sub, 6, oidlen, 0);
        put_data(sub, oid, oidlen);

        /* Append the OID to the sequence */
        put_ber_id_len(seq, 0, sub->len,
                       ASN1_CLASS_CONTEXT_SPECIFIC | ASN1_CONSTRUCTED);
        put_data(seq, sub->s, sub->len);
        strbuf_free(sub);

        /* Subsidiary BIT STRING */
        sub = strbuf_new();
        put_ber_id_len(sub, 3, 2 + pointlen, 0);
        put_byte(sub, 0);
        put_data(sub, pubblob->s+39, 1 + pointlen);

        /* Append the BIT STRING to the sequence */
        put_ber_id_len(seq, 1, sub->len,
                       ASN1_CLASS_CONTEXT_SPECIFIC | ASN1_CONSTRUCTED);
        put_data(seq, sub->s, sub->len);
        strbuf_free(sub);

        /* Write the full sequence with header to the output blob. */
        put_ber_id_len(outblob, 16, seq->len, ASN1_CONSTRUCTED);
        put_data(outblob, seq->s, seq->len);
        strbuf_free(seq);

        header = "-----BEGIN EC PRIVATE KEY-----\n";
        footer = "-----END EC PRIVATE KEY-----\n";
    } else {
        unreachable("bad key alg in openssh_pem_write");
    }

    /*
     * Encrypt the key.
     *
     * For the moment, we still encrypt our OpenSSH keys using
     * old-style 3DES.
     */
    if (passphrase) {
        unsigned char keybuf[32];
        int origlen, outlen, pad;

        /*
         * Padding on OpenSSH keys is deterministic. The number of
         * padding bytes is always more than zero, and always at most
         * the cipher block length. The value of each padding byte is
         * equal to the number of padding bytes. So a plaintext that's
         * an exact multiple of the block size will be padded with 08
         * 08 08 08 08 08 08 08 (assuming a 64-bit block cipher); a
         * plaintext one byte less than a multiple of the block size
         * will be padded with just 01.
         *
         * This enables the OpenSSL key decryption function to strip
         * off the padding algorithmically and return the unpadded
         * plaintext to the next layer: it looks at the final byte, and
         * then expects to find that many bytes at the end of the data
         * with the same value. Those are all removed and the rest is
         * returned.
         */
        origlen = outblob->len;
        outlen = (origlen + 8) &~ 7;
        pad = outlen - origlen;
        put_padding(outblob, pad, pad);

        /*
         * Invent an iv, and derive the encryption key.
         */
        random_read(iv, 8);

        openssh_pem_derivekey(ptrlen_from_asciz(passphrase), iv, keybuf);

        /*
         * Now encrypt the key blob.
         */
        des3_encrypt_pubkey_ossh(keybuf, iv,
                                 outblob->u, outlen);

        smemclr(keybuf, sizeof(keybuf));
    }

    /*
     * And save it. We'll use Unix line endings just in case it's
     * subsequently transferred in binary mode.
     */
    fp = f_open(filename, "wb", true);      /* ensure Unix line endings */
    if (!fp)
        goto error;
    fputs(header, fp);
    if (passphrase) {
        fprintf(fp, "Proc-Type: 4,ENCRYPTED\nDEK-Info: DES-EDE3-CBC,");
        for (i = 0; i < 8; i++)
            fprintf(fp, "%02X", iv[i]);
        fprintf(fp, "\n\n");
    }
    base64_encode_fp(fp, ptrlen_from_strbuf(outblob), 64);
    fputs(footer, fp);
    fclose(fp);
    ret = true;

  error:
    if (outblob)
        strbuf_free(outblob);
    if (spareblob) {
        smemclr(spareblob, sparelen);
        sfree(spareblob);
    }
    if (privblob)
        strbuf_free(privblob);
    if (pubblob)
        strbuf_free(pubblob);
    return ret;
}

/* ----------------------------------------------------------------------
 * Code to read and write OpenSSH private keys in the new-style format.
 */

typedef enum {
    ON_E_NONE, ON_E_AES256CBC, ON_E_AES256CTR
} openssh_new_cipher;
typedef enum {
    ON_K_NONE, ON_K_BCRYPT
} openssh_new_kdf;

struct openssh_new_key {
    openssh_new_cipher cipher;
    openssh_new_kdf kdf;
    union {
        struct {
            int rounds;
            /* This points to a position within keyblob, not a
             * separately allocated thing */
            ptrlen salt;
        } bcrypt;
    } kdfopts;
    int nkeys, key_wanted;
    /* This too points to a position within keyblob */
    ptrlen private;

    strbuf *keyblob;
};

static struct openssh_new_key *load_openssh_new_key(BinarySource *filesrc,
                                                    const char **errmsg_p)
{
    struct openssh_new_key *key;
    char *line = NULL;
    const char *errmsg;
    char *p;
    char base64_bit[4];
    int base64_chars = 0;
    BinarySource src[1];
    ptrlen str;
    unsigned key_index;

    key = snew(struct openssh_new_key);
    key->keyblob = strbuf_new_nm();

    if (!(line = bsgetline(filesrc))) {
        errmsg = "unexpected end of file";
        goto error;
    }
    if (0 != strcmp(line, "-----BEGIN OPENSSH PRIVATE KEY-----")) {
        errmsg = "file does not begin with OpenSSH new-style key header";
        goto error;
    }
    smemclr(line, strlen(line));
    sfree(line);
    line = NULL;

    while (1) {
        if (!(line = bsgetline(filesrc))) {
            errmsg = "unexpected end of file";
            goto error;
        }
        if (0 == strcmp(line, "-----END OPENSSH PRIVATE KEY-----")) {
            sfree(line);
            line = NULL;
            break;                     /* done */
        }

        p = line;
        while (isbase64(*p)) {
            base64_bit[base64_chars++] = *p;
            if (base64_chars == 4) {
                unsigned char out[3];
                int len;

                base64_chars = 0;

                len = base64_decode_atom(base64_bit, out);

                if (len <= 0) {
                    errmsg = "invalid base64 encoding";
                    goto error;
                }

                put_data(key->keyblob, out, len);

                smemclr(out, sizeof(out));
            }

            p++;
        }
        smemclr(line, strlen(line));
        sfree(line);
        line = NULL;
    }

    if (key->keyblob->len == 0) {
        errmsg = "key body not present";
        goto error;
    }

    BinarySource_BARE_INIT_PL(src, ptrlen_from_strbuf(key->keyblob));

    if (strcmp(get_asciz(src), "openssh-key-v1") != 0) {
        errmsg = "new-style OpenSSH magic number missing\n";
        goto error;
    }

    /* Cipher name */
    str = get_string(src);
    if (ptrlen_eq_string(str, "none")) {
        key->cipher = ON_E_NONE;
    } else if (ptrlen_eq_string(str, "aes256-cbc")) {
        key->cipher = ON_E_AES256CBC;
    } else if (ptrlen_eq_string(str, "aes256-ctr")) {
        key->cipher = ON_E_AES256CTR;
    } else {
        errmsg = get_err(src) ? "no cipher name found" :
            "unrecognised cipher name\n";
        goto error;
    }

    /* Key derivation function name */
    str = get_string(src);
    if (ptrlen_eq_string(str, "none")) {
        key->kdf = ON_K_NONE;
    } else if (ptrlen_eq_string(str, "bcrypt")) {
        key->kdf = ON_K_BCRYPT;
    } else {
        errmsg = get_err(src) ? "no kdf name found" :
            "unrecognised kdf name\n";
        goto error;
    }

    /* KDF extra options */
    str = get_string(src);
    switch (key->kdf) {
      case ON_K_NONE:
        if (str.len != 0) {
            errmsg = "expected empty options string for 'none' kdf";
            goto error;
        }
        break;
      case ON_K_BCRYPT: {
        BinarySource opts[1];

        BinarySource_BARE_INIT_PL(opts, str);
        key->kdfopts.bcrypt.salt = get_string(opts);
        key->kdfopts.bcrypt.rounds = get_uint32(opts);

        if (get_err(opts)) {
            errmsg = "failed to parse bcrypt options string";
            goto error;
        }
        break;
      }
    }

    /*
     * At this point we expect a uint32 saying how many keys are
     * stored in this file. OpenSSH new-style key files can
     * contain more than one. Currently we don't have any user
     * interface to specify which one we're trying to extract, so
     * we just bomb out with an error if more than one is found in
     * the file. However, I've put in all the mechanism here to
     * extract the nth one for a given n, in case we later connect
     * up some UI to that mechanism. Just arrange that the
     * 'key_wanted' field is set to a value in the range [0,
     * nkeys) by some mechanism.
     */
    key->nkeys = toint(get_uint32(src));
    if (key->nkeys != 1) {
        errmsg = get_err(src) ? "no key count found" :
            "multiple keys in new-style OpenSSH key file not supported\n";
        goto error;
    }
    key->key_wanted = 0;

    /* Read and ignore a string per public key. */
    for (key_index = 0; key_index < key->nkeys; key_index++)
        str = get_string(src);

    /*
     * Now we expect a string containing the encrypted part of the
     * key file.
     */
    key->private = get_string(src);
    if (get_err(src)) {
        errmsg = "no private key container string found\n";
        goto error;
    }

    /*
     * And now we're done, until asked to actually decrypt.
     */

    smemclr(base64_bit, sizeof(base64_bit));
    if (errmsg_p) *errmsg_p = NULL;
    return key;

  error:
    if (line) {
        smemclr(line, strlen(line));
        sfree(line);
        line = NULL;
    }
    smemclr(base64_bit, sizeof(base64_bit));
    if (key) {
        strbuf_free(key->keyblob);
        smemclr(key, sizeof(*key));
        sfree(key);
    }
    if (errmsg_p) *errmsg_p = errmsg;
    return NULL;
}

static bool openssh_new_encrypted(BinarySource *src)
{
    struct openssh_new_key *key = load_openssh_new_key(src, NULL);
    bool ret;

    if (!key)
        return false;
    ret = (key->cipher != ON_E_NONE);
    strbuf_free(key->keyblob);
    smemclr(key, sizeof(*key));
    sfree(key);
    return ret;
}

static ssh2_userkey *openssh_new_read(
    BinarySource *filesrc, const char *passphrase, const char **errmsg_p)
{
    struct openssh_new_key *key = load_openssh_new_key(filesrc, errmsg_p);
    ssh2_userkey *retkey = NULL;
    ssh2_userkey *retval = NULL;
    const char *errmsg;
    unsigned checkint;
    BinarySource src[1];
    int key_index;
    const ssh_keyalg *alg = NULL;

    if (!key)
        return NULL;

    if (key->cipher != ON_E_NONE) {
        unsigned char keybuf[48];
        int keysize;

        /*
         * Construct the decryption key, and decrypt the string.
         */
        switch (key->cipher) {
          case ON_E_NONE:
            keysize = 0;
            break;
          case ON_E_AES256CBC:
          case ON_E_AES256CTR:
            keysize = 48;              /* 32 byte key + 16 byte IV */
            break;
          default:
            unreachable("Bad cipher enumeration value");
        }
        assert(keysize <= sizeof(keybuf));
        switch (key->kdf) {
          case ON_K_NONE:
            memset(keybuf, 0, keysize);
            break;
          case ON_K_BCRYPT:
            openssh_bcrypt(ptrlen_from_asciz(passphrase),
                           key->kdfopts.bcrypt.salt,
                           key->kdfopts.bcrypt.rounds,
                           keybuf, keysize);
            break;
          default:
            unreachable("Bad kdf enumeration value");
        }
        switch (key->cipher) {
          case ON_E_NONE:
            break;
          case ON_E_AES256CBC:
          case ON_E_AES256CTR:
            if (key->private.len % 16 != 0) {
                errmsg = "private key container length is not a"
                    " multiple of AES block size\n";
                goto error;
            }
            {
                ssh_cipher *cipher = ssh_cipher_new(
                    key->cipher == ON_E_AES256CBC ?
                    &ssh_aes256_cbc : &ssh_aes256_sdctr);
                ssh_cipher_setkey(cipher, keybuf);
                ssh_cipher_setiv(cipher, keybuf + 32);
                /* Decrypt the private section in place, casting away
                 * the const from key->private being a ptrlen */
                ssh_cipher_decrypt(cipher, (char *)key->private.ptr,
                                   key->private.len);
                ssh_cipher_free(cipher);
            }
            break;
          default:
            unreachable("Bad cipher enumeration value");
        }
    }

    /*
     * Now parse the entire encrypted section, and extract the key
     * identified by key_wanted.
     */
    BinarySource_BARE_INIT_PL(src, key->private);

    checkint = get_uint32(src);
    if (get_uint32(src) != checkint || get_err(src)) {
        errmsg = "decryption check failed";
        goto error;
    }

    retkey = snew(ssh2_userkey);
    retkey->key = NULL;
    retkey->comment = NULL;

    for (key_index = 0; key_index < key->nkeys; key_index++) {
        ptrlen comment;

        /*
         * Identify the key type.
         */
        alg = find_pubkey_alg_len(get_string(src));
        if (!alg) {
            errmsg = "private key type not recognised\n";
            goto error;
        }

        /*
         * Read the key. We have to do this even if it's not the one
         * we want, because it's the only way to find out how much
         * data to skip past to get to the next key in the file.
         */
        retkey->key = ssh_key_new_priv_openssh(alg, src);
        if (get_err(src)) {
            errmsg = "unable to read entire private key";
            goto error;
        }
        if (!retkey->key) {
            errmsg = "unable to create key data structure";
            goto error;
        }
        if (key_index != key->key_wanted) {
            /*
             * If this isn't the key we're looking for, throw it away.
             */
            ssh_key_free(retkey->key);
            retkey->key = NULL;
        }

        /*
         * Read the key comment.
         */
        comment = get_string(src);
        if (get_err(src)) {
            errmsg = "unable to read key comment";
            goto error;
        }
        if (key_index == key->key_wanted) {
            assert(retkey);
            retkey->comment = mkstr(comment);
        }
    }

    if (!retkey->key) {
        errmsg = "key index out of range";
        goto error;
    }

    /*
     * Now we expect nothing left but padding.
     */
    {
        unsigned char expected_pad_byte = 1;
        while (get_avail(src) > 0)
            if (get_byte(src) != expected_pad_byte++) {
                errmsg = "padding at end of private string did not match";
                goto error;
            }
    }

    errmsg = NULL;                     /* no error */
    retval = retkey;
    retkey = NULL;                     /* prevent the free */

  error:
    if (retkey) {
        sfree(retkey->comment);
        if (retkey->key)
            ssh_key_free(retkey->key);
        sfree(retkey);
    }
    strbuf_free(key->keyblob);
    smemclr(key, sizeof(*key));
    sfree(key);
    if (errmsg_p) *errmsg_p = errmsg;
    return retval;
}

static bool openssh_new_write(
    const Filename *filename, ssh2_userkey *ukey, const char *passphrase)
{
    strbuf *pubblob, *privblob, *cblob;
    int padvalue;
    unsigned checkint;
    bool ret = false;
    unsigned char bcrypt_salt[16];
    const int bcrypt_rounds = 16;
    FILE *fp;

    /* OpenSSH's private key files never contain a certificate, so
     * revert to the underlying base key if necessary */
    ssh_key *key = ssh_key_base_key(ukey->key);

    /*
     * Fetch the key blobs and find out the lengths of things.
     */
    pubblob = strbuf_new();
    ssh_key_public_blob(key, BinarySink_UPCAST(pubblob));
    privblob = strbuf_new_nm();
    ssh_key_openssh_blob(key, BinarySink_UPCAST(privblob));

    /*
     * Construct the cleartext version of the blob.
     */
    cblob = strbuf_new_nm();

    /* Magic number. */
    put_asciz(cblob, "openssh-key-v1");

    /* Cipher and kdf names, and kdf options. */
    if (!passphrase) {
        memset(bcrypt_salt, 0, sizeof(bcrypt_salt)); /* prevent warnings */
        put_stringz(cblob, "none");
        put_stringz(cblob, "none");
        put_stringz(cblob, "");
    } else {
        strbuf *substr;

        random_read(bcrypt_salt, sizeof(bcrypt_salt));
        put_stringz(cblob, "aes256-ctr");
        put_stringz(cblob, "bcrypt");
        substr = strbuf_new_nm();
        put_string(substr, bcrypt_salt, sizeof(bcrypt_salt));
        put_uint32(substr, bcrypt_rounds);
        put_stringsb(cblob, substr);
    }

    /* Number of keys. */
    put_uint32(cblob, 1);

    /* Public blob. */
    put_string(cblob, pubblob->s, pubblob->len);

    /* Private section. */
    {
        strbuf *cpblob = strbuf_new_nm();

        /* checkint. */
        uint8_t checkint_buf[4];
        random_read(checkint_buf, 4);
        checkint = GET_32BIT_MSB_FIRST(checkint_buf);
        put_uint32(cpblob, checkint);
        put_uint32(cpblob, checkint);

        /* Private key. The main private blob goes inline, with no string
         * wrapper. */
        put_stringz(cpblob, ssh_key_ssh_id(key));
        put_data(cpblob, privblob->s, privblob->len);

        /* Comment. */
        put_stringz(cpblob, ukey->comment);

        /* Pad out the encrypted section. */
        padvalue = 1;
        do {
            put_byte(cpblob, padvalue++);
        } while (cpblob->len & 15);

        if (passphrase) {
            /*
             * Encrypt the private section. We need 48 bytes of key
             * material: 32 bytes AES key + 16 bytes iv.
             */
            unsigned char keybuf[48];
            ssh_cipher *cipher;

            openssh_bcrypt(ptrlen_from_asciz(passphrase),
                           make_ptrlen(bcrypt_salt, sizeof(bcrypt_salt)),
                           bcrypt_rounds, keybuf, sizeof(keybuf));

            cipher = ssh_cipher_new(&ssh_aes256_sdctr);
            ssh_cipher_setkey(cipher, keybuf);
            ssh_cipher_setiv(cipher, keybuf + 32);
            ssh_cipher_encrypt(cipher, cpblob->u, cpblob->len);
            ssh_cipher_free(cipher);

            smemclr(keybuf, sizeof(keybuf));
        }

        put_stringsb(cblob, cpblob);
    }

    /*
     * And save it. We'll use Unix line endings just in case it's
     * subsequently transferred in binary mode.
     */
    fp = f_open(filename, "wb", true);      /* ensure Unix line endings */
    if (!fp)
        goto error;
    fputs("-----BEGIN OPENSSH PRIVATE KEY-----\n", fp);
    base64_encode_fp(fp, ptrlen_from_strbuf(cblob), 64);
    fputs("-----END OPENSSH PRIVATE KEY-----\n", fp);
    fclose(fp);
    ret = true;

  error:
    if (cblob)
        strbuf_free(cblob);
    if (privblob)
        strbuf_free(privblob);
    if (pubblob)
        strbuf_free(pubblob);
    return ret;
}

/* ----------------------------------------------------------------------
 * The switch function openssh_auto_write(), which chooses one of the
 * concrete OpenSSH output formats based on the key type.
 */
static bool openssh_auto_write(
    const Filename *filename, ssh2_userkey *key, const char *passphrase)
{
    /*
     * The old OpenSSH format supports a fixed list of key types. We
     * assume that anything not in that fixed list is newer, and hence
     * will use the new format.
     */
    const ssh_keyalg *alg = ssh_key_alg(ssh_key_base_key(key->key));
    if (alg == &ssh_dsa ||
        alg == &ssh_rsa ||
        alg == &ssh_ecdsa_nistp256 ||
        alg == &ssh_ecdsa_nistp384 ||
        alg == &ssh_ecdsa_nistp521)
        return openssh_pem_write(filename, key, passphrase);
    else
        return openssh_new_write(filename, key, passphrase);
}

/* ----------------------------------------------------------------------
 * Code to read ssh.com private keys.
 */

/*
 * The format of the base64 blob is largely SSH-2-packet-formatted,
 * except that mpints are a bit different: they're more like the
 * old SSH-1 mpint. You have a 32-bit bit count N, followed by
 * (N+7)/8 bytes of data.
 *
 * So. The blob contains:
 *
 *  - uint32 0x3f6ff9eb       (magic number)
 *  - uint32 size             (total blob size)
 *  - string key-type         (see below)
 *  - string cipher-type      (tells you if key is encrypted)
 *  - string encrypted-blob
 *
 * (The first size field includes the size field itself and the
 * magic number before it. All other size fields are ordinary SSH-2
 * strings, so the size field indicates how much data is to
 * _follow_.)
 *
 * The encrypted blob, once decrypted, contains a single string
 * which in turn contains the payload. (This allows padding to be
 * added after that string while still making it clear where the
 * real payload ends. Also it probably makes for a reasonable
 * decryption check.)
 *
 * The payload blob, for an RSA key, contains:
 *  - mpint e
 *  - mpint d
 *  - mpint n  (yes, the public and private stuff is intermixed)
 *  - mpint u  (presumably inverse of p mod q)
 *  - mpint p  (p is the smaller prime)
 *  - mpint q  (q is the larger)
 *
 * For a DSA key, the payload blob contains:
 *  - uint32 0
 *  - mpint p
 *  - mpint g
 *  - mpint q
 *  - mpint y
 *  - mpint x
 *
 * Alternatively, if the parameters are `predefined', that
 * (0,p,g,q) sequence can be replaced by a uint32 1 and a string
 * containing some predefined parameter specification. *shudder*,
 * but I doubt we'll encounter this in real life.
 *
 * The key type strings are ghastly. The RSA key I looked at had a
 * type string of
 *
 *   `if-modn{sign{rsa-pkcs1-sha1},encrypt{rsa-pkcs1v2-oaep}}'
 *
 * and the DSA key wasn't much better:
 *
 *   `dl-modp{sign{dsa-nist-sha1},dh{plain}}'
 *
 * It isn't clear that these will always be the same. I think it
 * might be wise just to look at the `if-modn{sign{rsa' and
 * `dl-modp{sign{dsa' prefixes.
 *
 * Finally, the encryption. The cipher-type string appears to be
 * either `none' or `3des-cbc'. Looks as if this is SSH-2-style
 * 3des-cbc (i.e. outer cbc rather than inner). The key is created
 * from the passphrase by means of yet another hashing faff:
 *
 *  - first 16 bytes are MD5(passphrase)
 *  - next 16 bytes are MD5(passphrase || first 16 bytes)
 *  - if there were more, they'd be MD5(passphrase || first 32),
 *    and so on.
 */

#define SSHCOM_MAGIC_NUMBER 0x3f6ff9eb

struct sshcom_key {
    char comment[256];                 /* allowing any length is overkill */
    strbuf *keyblob;
};

static struct sshcom_key *load_sshcom_key(BinarySource *src,
                                          const char **errmsg_p)
{
    struct sshcom_key *key;
    char *line = NULL;
    int hdrstart, len;
    const char *errmsg;
    char *p;
    bool headers_done;
    char base64_bit[4];
    int base64_chars = 0;

    key = snew(struct sshcom_key);
    key->comment[0] = '\0';
    key->keyblob = strbuf_new_nm();

    if (!(line = bsgetline(src))) {
        errmsg = "unexpected end of file";
        goto error;
    }
    if (0 != strcmp(line, "---- BEGIN SSH2 ENCRYPTED PRIVATE KEY ----")) {
        errmsg = "file does not begin with ssh.com key header";
        goto error;
    }
    smemclr(line, strlen(line));
    sfree(line);
    line = NULL;

    headers_done = false;
    while (1) {
        if (!(line = bsgetline(src))) {
            errmsg = "unexpected end of file";
            goto error;
        }
        if (!strcmp(line, "---- END SSH2 ENCRYPTED PRIVATE KEY ----")) {
            sfree(line);
            line = NULL;
            break;                     /* done */
        }
        if ((p = strchr(line, ':')) != NULL) {
            if (headers_done) {
                errmsg = "header found in body of key data";
                goto error;
            }
            *p++ = '\0';
            while (*p && isspace((unsigned char)*p)) p++;
            hdrstart = p - line;

            /*
             * Header lines can end in a trailing backslash for
             * continuation.
             */
            len = hdrstart + strlen(line+hdrstart);
            assert(!line[len]);
            while (line[len-1] == '\\') {
                char *line2;
                int line2len;

                line2 = bsgetline(src);
                if (!line2) {
                    errmsg = "unexpected end of file";
                    goto error;
                }

                line2len = strlen(line2);
                line = sresize(line, len + line2len + 1, char);
                strcpy(line + len - 1, line2);
                len += line2len - 1;
                assert(!line[len]);

                smemclr(line2, strlen(line2));
                sfree(line2);
                line2 = NULL;
            }
            p = line + hdrstart;
            if (!strcmp(line, "Comment")) {
                /* Strip quotes in comment if present. */
                if (p[0] == '"' && p[strlen(p)-1] == '"') {
                    p++;
                    p[strlen(p)-1] = '\0';
                }
                strncpy(key->comment, p, sizeof(key->comment));
                key->comment[sizeof(key->comment)-1] = '\0';
            }
        } else {
            headers_done = true;

            p = line;
            while (isbase64(*p)) {
                base64_bit[base64_chars++] = *p;
                if (base64_chars == 4) {
                    unsigned char out[3];

                    base64_chars = 0;

                    len = base64_decode_atom(base64_bit, out);

                    if (len <= 0) {
                        errmsg = "invalid base64 encoding";
                        goto error;
                    }

                    put_data(key->keyblob, out, len);
                }

                p++;
            }
        }
        smemclr(line, strlen(line));
        sfree(line);
        line = NULL;
    }

    if (key->keyblob->len == 0) {
        errmsg = "key body not present";
        goto error;
    }

    if (errmsg_p) *errmsg_p = NULL;
    return key;

  error:
    if (line) {
        smemclr(line, strlen(line));
        sfree(line);
        line = NULL;
    }
    if (key) {
        strbuf_free(key->keyblob);
        smemclr(key, sizeof(*key));
        sfree(key);
    }
    if (errmsg_p) *errmsg_p = errmsg;
    return NULL;
}

static bool sshcom_encrypted(BinarySource *filesrc, char **comment)
{
    struct sshcom_key *key = load_sshcom_key(filesrc, NULL);
    BinarySource src[1];
    ptrlen str;
    bool answer = false;

    *comment = NULL;
    if (!key)
        goto done;

    BinarySource_BARE_INIT_PL(src, ptrlen_from_strbuf(key->keyblob));

    if (get_uint32(src) != SSHCOM_MAGIC_NUMBER)
        goto done;                     /* key is invalid */
    get_uint32(src);                   /* skip length field */
    get_string(src);                   /* skip key type */
    str = get_string(src);             /* cipher type */
    if (get_err(src))
        goto done;                     /* key is invalid */
    if (!ptrlen_eq_string(str, "none"))
        answer = true;

  done:
    if (key) {
        *comment = dupstr(key->comment);
        strbuf_free(key->keyblob);
        smemclr(key, sizeof(*key));
        sfree(key);
    } else {
        *comment = dupstr("");
    }
    return answer;
}

static void BinarySink_put_mp_sshcom_from_string(BinarySink *bs, ptrlen str)
{
    const unsigned char *bytes = (const unsigned char *)str.ptr;
    size_t nbytes = str.len;
    int bits = nbytes * 8 - 1;

    while (bits > 0) {
        if (*bytes & (1 << (bits & 7)))
            break;
        if (!(bits-- & 7))
            bytes++, nbytes--;
    }

    put_uint32(bs, bits+1);
    put_data(bs, bytes, nbytes);
}

#define put_mp_sshcom_from_string(bs, str) \
    BinarySink_put_mp_sshcom_from_string(BinarySink_UPCAST(bs), str)

static ptrlen BinarySource_get_mp_sshcom_as_string(BinarySource *src)
{
    unsigned bits = get_uint32(src);
    return get_data(src, (bits + 7) / 8);
}

#define get_mp_sshcom_as_string(bs) \
    BinarySource_get_mp_sshcom_as_string(BinarySource_UPCAST(bs))

static void sshcom_derivekey(ptrlen passphrase, uint8_t *keybuf)
{
    /*
     * Derive the encryption key for an ssh.com key file from the
     * passphrase and iv/salt:
     *
     *  - let block A equal MD5(passphrase)
     *  - let block B equal MD5(passphrase || A)
     *  - block C would be MD5(passphrase || A || B) and so on
     *  - encryption key is the first N bytes of A || B
     */
    ssh_hash *h;

    h = ssh_hash_new(&ssh_md5);
    put_datapl(h, passphrase);
    ssh_hash_digest_nondestructive(h, keybuf);
    put_data(h, keybuf, 16);
    ssh_hash_final(h, keybuf + 16);
}

static ssh2_userkey *sshcom_read(
    BinarySource *filesrc, const char *passphrase, const char **errmsg_p)
{
    struct sshcom_key *key = load_sshcom_key(filesrc, errmsg_p);
    const char *errmsg;
    BinarySource src[1];
    ptrlen str, ciphertext;
    int publen;
    const char prefix_rsa[] = "if-modn{sign{rsa";
    const char prefix_dsa[] = "dl-modp{sign{dsa";
    enum { RSA, DSA } type;
    bool encrypted;
    ssh2_userkey *ret = NULL, *retkey;
    const ssh_keyalg *alg;
    strbuf *blob = NULL;

    if (!key)
        return NULL;

    BinarySource_BARE_INIT_PL(src, ptrlen_from_strbuf(key->keyblob));

    if (get_uint32(src) != SSHCOM_MAGIC_NUMBER) {
        errmsg = "key does not begin with magic number";
        goto error;
    }
    get_uint32(src);                   /* skip length field */

    /*
     * Determine the key type.
     */
    str = get_string(src);
    if (str.len > sizeof(prefix_rsa) - 1 &&
        !memcmp(str.ptr, prefix_rsa, sizeof(prefix_rsa) - 1)) {
        type = RSA;
    } else if (str.len > sizeof(prefix_dsa) - 1 &&
               !memcmp(str.ptr, prefix_dsa, sizeof(prefix_dsa) - 1)) {
        type = DSA;
    } else {
        errmsg = "key is of unknown type";
        goto error;
    }

    /*
     * Determine the cipher type.
     */
    str = get_string(src);
    if (ptrlen_eq_string(str, "none"))
        encrypted = false;
    else if (ptrlen_eq_string(str, "3des-cbc"))
        encrypted = true;
    else {
        errmsg = "key encryption is of unknown type";
        goto error;
    }

    /*
     * Get hold of the encrypted part of the key.
     */
    ciphertext = get_string(src);
    if (ciphertext.len == 0) {
        errmsg = "no key data found";
        goto error;
    }

    /*
     * Decrypt it if necessary.
     */
    if (encrypted) {
        /*
         * Derive encryption key from passphrase and iv/salt:
         *
         *  - let block A equal MD5(passphrase)
         *  - let block B equal MD5(passphrase || A)
         *  - block C would be MD5(passphrase || A || B) and so on
         *  - encryption key is the first N bytes of A || B
         */
        unsigned char keybuf[32], iv[8];

        if (ciphertext.len % 8 != 0) {
            errmsg = "encrypted part of key is not a multiple of cipher block"
                " size";
            goto error;
        }

        sshcom_derivekey(ptrlen_from_asciz(passphrase), keybuf);

        /*
         * Now decrypt the key blob in place (casting away const from
         * ciphertext being a ptrlen).
         */
        memset(iv, 0, sizeof(iv));
        des3_decrypt_pubkey_ossh(keybuf, iv,
                                 (char *)ciphertext.ptr, ciphertext.len);

        smemclr(keybuf, sizeof(keybuf));

        /*
         * Hereafter we return WRONG_PASSPHRASE for any parsing
         * error. (But only if we've just tried to decrypt it!
         * Returning WRONG_PASSPHRASE for an unencrypted key is
         * automatic doom.)
         */
        if (encrypted)
            ret = SSH2_WRONG_PASSPHRASE;
    }

    /*
     * Expect the ciphertext to be formatted as a containing string,
     * and reinitialise src to start parsing the inside of that string.
     */
    BinarySource_BARE_INIT_PL(src, ciphertext);
    str = get_string(src);
    if (get_err(src)) {
        errmsg = "containing string was ill-formed";
        goto error;
    }
    BinarySource_BARE_INIT_PL(src, str);

    /*
     * Now we break down into RSA versus DSA. In either case we'll
     * construct public and private blobs in our own format, and
     * end up feeding them to ssh_key_new_priv().
     */
    blob = strbuf_new_nm();
    if (type == RSA) {
        ptrlen n, e, d, u, p, q;

        e = get_mp_sshcom_as_string(src);
        d = get_mp_sshcom_as_string(src);
        n = get_mp_sshcom_as_string(src);
        u = get_mp_sshcom_as_string(src);
        p = get_mp_sshcom_as_string(src);
        q = get_mp_sshcom_as_string(src);
        if (get_err(src)) {
            errmsg = "key data did not contain six integers";
            goto error;
        }

        alg = &ssh_rsa;
        put_stringz(blob, "ssh-rsa");
        put_mp_ssh2_from_string(blob, e);
        put_mp_ssh2_from_string(blob, n);
        publen = blob->len;
        put_mp_ssh2_from_string(blob, d);
        put_mp_ssh2_from_string(blob, q);
        put_mp_ssh2_from_string(blob, p);
        put_mp_ssh2_from_string(blob, u);
    } else {
        ptrlen p, q, g, x, y;

        assert(type == DSA); /* the only other option from the if above */

        if (get_uint32(src) != 0) {
            errmsg = "predefined DSA parameters not supported";
            goto error;
        }
        p = get_mp_sshcom_as_string(src);
        g = get_mp_sshcom_as_string(src);
        q = get_mp_sshcom_as_string(src);
        y = get_mp_sshcom_as_string(src);
        x = get_mp_sshcom_as_string(src);
        if (get_err(src)) {
            errmsg = "key data did not contain five integers";
            goto error;
        }

        alg = &ssh_dsa;
        put_stringz(blob, "ssh-dss");
        put_mp_ssh2_from_string(blob, p);
        put_mp_ssh2_from_string(blob, q);
        put_mp_ssh2_from_string(blob, g);
        put_mp_ssh2_from_string(blob, y);
        publen = blob->len;
        put_mp_ssh2_from_string(blob, x);
    }

    retkey = snew(ssh2_userkey);
    retkey->key = ssh_key_new_priv(
        alg, make_ptrlen(blob->u, publen),
        make_ptrlen(blob->u + publen, blob->len - publen));
    if (!retkey->key) {
        sfree(retkey);
        errmsg = "unable to create key data structure";
        goto error;
    }
    retkey->comment = dupstr(key->comment);

    errmsg = NULL; /* no error */
    ret = retkey;

  error:
    if (blob) {
        strbuf_free(blob);
    }
    strbuf_free(key->keyblob);
    smemclr(key, sizeof(*key));
    sfree(key);
    if (errmsg_p) *errmsg_p = errmsg;
    return ret;
}

static bool sshcom_write(
    const Filename *filename, ssh2_userkey *key, const char *passphrase)
{
    strbuf *pubblob, *privblob, *outblob;
    ptrlen numbers[6];
    int nnumbers, lenpos, i;
    bool initial_zero;
    BinarySource src[1];
    const char *type;
    char *ciphertext;
    int cipherlen;
    bool ret = false;
    FILE *fp;

    /*
     * Fetch the key blobs.
     */
    pubblob = strbuf_new();
    ssh_key_public_blob(key->key, BinarySink_UPCAST(pubblob));
    privblob = strbuf_new_nm();
    ssh_key_private_blob(key->key, BinarySink_UPCAST(privblob));
    outblob = NULL;

    /*
     * Find the sequence of integers to be encoded into the OpenSSH
     * key blob, and also decide on the header line.
     */
    if (ssh_key_alg(key->key) == &ssh_rsa) {
        ptrlen n, e, d, p, q, iqmp;

        /*
         * These blobs were generated from inside PuTTY, so we needn't
         * treat them as untrusted.
         */
        BinarySource_BARE_INIT(src, pubblob->u, pubblob->len);
        get_string(src);               /* skip algorithm name */
        e = get_string(src);
        n = get_string(src);
        BinarySource_BARE_INIT(src, privblob->u, privblob->len);
        d = get_string(src);
        p = get_string(src);
        q = get_string(src);
        iqmp = get_string(src);

        assert(!get_err(src));         /* can't go wrong */

        numbers[0] = e;
        numbers[1] = d;
        numbers[2] = n;
        numbers[3] = iqmp;
        numbers[4] = q;
        numbers[5] = p;

        nnumbers = 6;
        initial_zero = false;
        type = "if-modn{sign{rsa-pkcs1-sha1},encrypt{rsa-pkcs1v2-oaep}}";
    } else if (ssh_key_alg(key->key) == &ssh_dsa) {
        ptrlen p, q, g, y, x;

        /*
         * These blobs were generated from inside PuTTY, so we needn't
         * treat them as untrusted.
         */
        BinarySource_BARE_INIT(src, pubblob->u, pubblob->len);
        get_string(src);               /* skip algorithm name */
        p = get_string(src);
        q = get_string(src);
        g = get_string(src);
        y = get_string(src);
        BinarySource_BARE_INIT(src, privblob->u, privblob->len);
        x = get_string(src);

        assert(!get_err(src));         /* can't go wrong */

        numbers[0] = p;
        numbers[1] = g;
        numbers[2] = q;
        numbers[3] = y;
        numbers[4] = x;

        nnumbers = 5;
        initial_zero = true;
        type = "dl-modp{sign{dsa-nist-sha1},dh{plain}}";
    } else {
        goto error;                    /* unsupported key type */
    }

    outblob = strbuf_new_nm();

    /*
     * Create the unencrypted key blob.
     */
    put_uint32(outblob, SSHCOM_MAGIC_NUMBER);
    put_uint32(outblob, 0);          /* length field, fill in later */
    put_stringz(outblob, type);
    put_stringz(outblob, passphrase ? "3des-cbc" : "none");
    lenpos = outblob->len;      /* remember this position */
    put_uint32(outblob, 0);            /* encrypted-blob size */
    put_uint32(outblob, 0);            /* encrypted-payload size */
    if (initial_zero)
        put_uint32(outblob, 0);
    for (i = 0; i < nnumbers; i++)
        put_mp_sshcom_from_string(outblob, numbers[i]);
    /* Now wrap up the encrypted payload. */
    PUT_32BIT_MSB_FIRST(outblob->s + lenpos + 4,
                        outblob->len - (lenpos + 8));
    /* Pad encrypted blob to a multiple of cipher block size. */
    if (passphrase) {
        int padding = -(ssize_t)(outblob->len - (lenpos+4)) & 7; // WINSCP
        uint8_t padding_buf[8];
        random_read(padding_buf, padding);
        put_data(outblob, padding_buf, padding);
    }
    ciphertext = outblob->s + lenpos + 4;
    cipherlen = outblob->len - (lenpos + 4);
    assert(!passphrase || cipherlen % 8 == 0);
    /* Wrap up the encrypted blob string. */
    PUT_32BIT_MSB_FIRST(outblob->s + lenpos, cipherlen);
    /* And finally fill in the total length field. */
    PUT_32BIT_MSB_FIRST(outblob->s + 4, outblob->len);

    /*
     * Encrypt the key.
     */
    if (passphrase) {
        unsigned char keybuf[32], iv[8];

        sshcom_derivekey(ptrlen_from_asciz(passphrase), keybuf);

        /*
         * Now decrypt the key blob.
         */
        memset(iv, 0, sizeof(iv));
        des3_encrypt_pubkey_ossh(keybuf, iv, ciphertext, cipherlen);

        smemclr(keybuf, sizeof(keybuf));
    }

    /*
     * And save it. We'll use Unix line endings just in case it's
     * subsequently transferred in binary mode.
     */
    fp = f_open(filename, "wb", true);      /* ensure Unix line endings */
    if (!fp)
        goto error;
    fputs("---- BEGIN SSH2 ENCRYPTED PRIVATE KEY ----\n", fp);
    fprintf(fp, "Comment: \"");
    /*
     * Comment header is broken with backslash-newline if it goes
     * over 70 chars. Although it's surrounded by quotes, it
     * _doesn't_ escape backslashes or quotes within the string.
     * Don't ask me, I didn't design it.
     */
    {
        int slen = 60;                 /* starts at 60 due to "Comment: " */
        char *c = key->comment;
        while ((int)strlen(c) > slen) {
            fprintf(fp, "%.*s\\\n", slen, c);
            c += slen;
            slen = 70;                 /* allow 70 chars on subsequent lines */
        }
        fprintf(fp, "%s\"\n", c);
    }
    base64_encode_fp(fp, ptrlen_from_strbuf(outblob), 70);
    fputs("---- END SSH2 ENCRYPTED PRIVATE KEY ----\n", fp);
    fclose(fp);
    ret = true;

  error:
    if (outblob)
        strbuf_free(outblob);
    if (privblob)
        strbuf_free(privblob);
    if (pubblob)
        strbuf_free(pubblob);
    return ret;
}
